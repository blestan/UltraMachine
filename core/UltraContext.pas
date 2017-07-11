unit UltraContext;

{$mode objfpc}{$H+}
{$modeswitch AdvancedRecords}

interface

uses

 UltraHttp,UltraSockets,UltraBuffers,xtypes,xon;

type


  PUltraContext=^TUltraContext;
  TUltraContext=record
             public
              MTDID: Cardinal; // Used by controller dispatch
             private
              FStartTime,
              FEndTime: QWord;
              FRunning: PBoolean;
              FHandled: Boolean;
              function  GetTerminate:Boolean;
              function ReadBuffer(const DataPtr: Pointer; MaxLen: Integer): Integer; // callback to read data in buffer when needed
              procedure SetHandled(AValue: Boolean);
             public
             Application: TObject;
             Socket: TAPISocket;
             Request: TUltraRequest;
             Response: TUltraResponse;
             Buffer: PUBuffer;

             procedure Prepare(ASocket: TSocket; RunningBool: PBoolean=nil);
             procedure Cleanup;

             procedure SendErrorResponse; // send default response in case of error, empty response, or unhandled

             property StartTime: QWord read FStartTime;
             property EndTime: QWord read FEndTime;

             property  Terminated: Boolean read GetTerminate;
             property Handled: Boolean read FHandled write SetHandled;
             procedure Terminate;

   end;



implementation

uses Sysutils;

procedure TUltraContext.Prepare(ASocket: TSocket; RunningBool: PBoolean=nil);
begin
 FStartTime:=GetTickCount64;
 FRunning:=RunningBool;
 Socket.Init(ASocket);

 Buffer:=TUltraBuffer.Alloc;
 Buffer^.SetReadCallBack(@Self,@Self.ReadBuffer);

 Request.Initialize;

 Response.StatusCode:=HTTP_ERROR_NONE;
 Response.Headers:=XVar.Null;

 Handled:=False;
end;

procedure TUltraContext.Cleanup;
begin
  Socket.Close;
  Response.Headers.Free;
  Request.Finalize;
  Buffer^.Release;
  Buffer:=nil;
  Application:=nil;
end;

procedure TUltraContext.SendErrorResponse;
begin
  with socket do
   begin
    Send(Format('HTTP/1.1 %d %s'#13#10,[Response.StatusCode,HTTPStatusPhrase(Response.StatusCode)]));
    Send('Connection: Closed'#13#10#13#10)
   end
end;

function TUltraContext.ReadBuffer(const DataPtr: Pointer; MaxLen: Integer): Integer;
begin
 Result:=Socket.RecvPacket(DataPtr,MaxLen,UltraTimeOut);
//  Result:=Socket.RecvPacket(DataPtr,4,UltraTimeOut); test -> bad network simulation
end;

procedure TUltraContext.Terminate;
begin
 if (FRunning<>nil) and (FRunning^) then FRunning^:=false;
end;

function TUltraContext.GetTerminate:boolean;
begin
 if FRunning<>nil then Result:=not FRunning^
                  else Result:=False;
end;

procedure TUltraContext.SetHandled(AValue: Boolean);
begin
  FHandled:=AValue;
  FEndTime:=GetTickCount64;
end;

end.

