unit UltraBuffers;

{$mode objfpc}{$H+}
{$modeswitch AdvancedRecords}

interface

const

    UltraBufferSize= 16384; // 16K buffers - keep this reasonable - default haproxy Load Balancer value:)

type

   TReadDataCallback=function(Instance: Pointer;const DataPtr: Pointer; MaxLen: Integer): Integer;


   PUBuffer=^TUltraBuffer;
   TUltraBuffer = record
                   private
                    type TBufData=Array[00..UltraBufferSize-1] of char;
                   private
                       FNext: PUBuffer; // next free buffer
                       FTail: Integer; // used part of the buffer
                       FCursor: Integer;
                       FData:  TBufData;
                       FOnRead: TMethod;
                       function InternalOnRead(const DataPtr: Pointer; MaxLen: Integer): Integer;
                   public
                    class procedure InitializeBuffers; static;// inits buffers free list - one per thread
                    class procedure FinalizeBuffers; static; // releases all alocated buffer in the thread
                    class function Alloc: PUBuffer;static;
                          procedure Release( DoReleaseMem: Boolean=false);
                          procedure Reset(ZeroLen: Boolean);
                          property  Len: Integer read FTail;
                          property Cursor: Integer read FCursor;
                          function DataPtr(Index: Integer): Pointer;
                          function Full: Boolean;
                          function NextChar(Normalize: Boolean=true ): Char;
                          procedure SetReadCallBack(Instance,Proc: Pointer);


    end;

implementation

// Buffers

threadvar  FreeBuffersList: PUBuffer;

class procedure TUltraBuffer.InitializeBuffers;static;
begin
 FreeBuffersList:=nil;
end;

class procedure TUltraBuffer.FinalizeBuffers;static;
var P: PUBuffer;
begin
 P:= FreeBuffersList;
 while P<>nil do
  with P^ do
    begin
     P:=P^.FNext;
     Release(true)
    end;
 FreeBuffersList:=nil;
end;

class function TUltraBuffer.Alloc: PUBuffer;static;
begin
 Result:=FreeBuffersList;
 if Result<>nil then FreeBuffersList:=Result^.FNext
                else GetMem(Result,SizeOf(TUltraBuffer));

 with Result^ do
  begin
    Reset(True);
    FNext:=nil;
    FOnRead.Code:=nil;
    FOnRead.Data:=nil;
  end
end;

procedure TUltraBuffer.Release(DoReleaseMem: Boolean=false); inline;
begin
  if @Self=nil then exit;
  if DoReleaseMem then Freemem(@Self)
                  else  begin
                    FNext:=FreeBuffersList;
                    FreeBuffersList:=@Self;
                  end;
end;

procedure TUltraBuffer.Reset( ZeroLen: Boolean);
begin
 if ZeroLen then FTail:=00;
 FCursor:=00;
end;

function TUltraBuffer.DataPtr(Index: Integer): Pointer;
begin
 if (Index>=0) and (Index<FTail) then Result:=@FData[Index]
                                 else Result:=nil;
end;

procedure TUltraBuffer.SetReadCallBack(Instance,Proc: Pointer);
begin
 FOnRead.Code:=Proc;
 FOnRead.Data:=Instance;
end;

function TUltraBuffer.Full:boolean;inline;
begin
  Result:=FTail>=UltraBufferSize;
end;

function TUltraBuffer.NextChar( Normalize: Boolean=True):char;
var r: Integer;
begin
 if FCursor>=FTail then
  begin
   if Full or (FOnRead.Code=nil)  then exit(#0);
   r:=InternalOnRead(@FData[FTail],UltraBufferSize-FTail);
   if r>0 then inc(FTail,r)
    else exit(#0);
  end;
  Result:=FData[FCursor];
  if Normalize and (Result in ['A'..'Z']) then Inc(Result,32);
  inc(FCursor);
end;

function TUltraBuffer.InternalOnRead(const DataPtr: Pointer; MaxLen: Integer): Integer;
begin
 if FOnRead.Code=nil then exit(-1);
 Result:=TReadDataCallback(FOnRead.Code)(FOnRead.Data,DataPtr,MaxLen);
end;

initialization

TUltraBuffer.InitializeBuffers; // for the main thread

finalization

TUltraBuffer.FinalizeBuffers; // for the main thread

end.

