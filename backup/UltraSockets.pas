unit UltraSockets;
{$modeswitch AdvancedRecords}
interface

uses  Sockets;


const

    UltraBufferSize= 16384; // 16K buffers - keep this reasonable - default haproxy Load Balancer value:)

    UltraTimeOut = 300;

    INVALID_SOCKET: TSocket = not(0);

type

  TSocket = Integer;


  PUBuffer=^TUBuffer;

  TApiSocket = record
  private
    FHandle : TSocket;
    RemoteSin : TInetSockAddr;
    function IsValid: Boolean;
  public
    procedure Init( ASocket: TSocket = not(0));
    procedure NewSocket;
    procedure Bind(pPort, BackLog : word);
    function Accept(Timeout : integer) : TSocket;
    procedure Connect(Host : string; pPort : word);
    procedure Purge;
    procedure Close;

    function RecvPacket( Buf: PUBuffer; Timeout : Integer ): Integer;
    function RecvString(Timeout : integer) : AnsiString;
    procedure SendString(const Data: AnsiString);
    function WaitingData : cardinal;
    function CanRead(Timeout: Integer): Boolean;
    function Error : integer;
    function GetHostAddress : string;
    property Socket: TSocket read FHandle;
    property Valid: Boolean read IsValid;
  end;


   TBufData=Array[00..UltraBufferSize-1] of byte;

   TUBuffer = record
                   private
                       FNext: PUBuffer; // next free buffer
                       FLen, // used part of the buffer
                       FCursor: Integer;
                       FData:  TBufData;
                   public
                    class function Alloc: PUBuffer;static;
                          procedure Release( DoReleaseMem: Boolean=false);
                          procedure Reset( ZeroLen: Boolean = True);
                          property  Len: Integer read FLen;
                          function DataPtr(Index: Integer): Pointer;
                          function Full: Boolean;


    end;



procedure InitBuffers;  // inits buffers free list - one per thread
procedure ReleaseBuffers; // releases all alocated buffer in the thread


implementation

uses
  SysUtils,
  {$IFDEF MSWINDOWS}
    WinSock2
  {$ELSE}
    {$DEFINE IOCtlSocket:=fpIOCtl}{$DEFINE FD_Zero:=fpFD_Zero}{$DEFINE FD_Set:=fpFD_Set}{$DEFINE Select:=fpSelect}
  {$ENDIF};


// Buffers

threadvar  FreeBuffersList: PUBuffer;
threadvar BuffersCount: Integer;

procedure InitBuffers;inline;
begin
 FreeBuffersList:=nil;
 BuffersCount:=00;
end;

procedure ReleaseBuffers;
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
 BuffersCount:=0;
end;

class function TUBuffer.Alloc: PUBuffer;static;
begin
 Result:=FreeBuffersList;
 if Result<>nil then FreeBuffersList:=Result^.FNext
                else GetMem(Result,SizeOf(TUBuffer));

 with Result^ do
  begin
    FNext:=nil;
    Reset
  end
end;

procedure TUBuffer.Release(DoReleaseMem: Boolean=false); inline;
begin
  if @Self=nil then exit;
  if DoReleaseMem then Freemem(@Self)
                  else  begin
                    FNext:=FreeBuffersList;
                    FreeBuffersList:=@Self;
                  end;
end;

procedure TUBuffer.Reset( ZeroLen: Boolean = True);
begin
 if ZeroLen then FLen:=00;
 FCursor:=00;
end;

function TUBuffer.DataPtr(Index: Integer): Pointer;
begin
 if (Index>=0) and (Index<FLen) then Result:=@FData[Index]
                                else Result:=nil;
end;

function TUBuffer.Full:boolean;inline;
begin
  Result:=UltraBufferSize-FCursor>0;
end;

// Sockets
procedure TApiSocket.Init( ASocket: TSocket = not(0));
begin
  {$IFNDEF MSWINDOWS}fpSetErrNo(0);{$ENDIF}
  FillChar(RemoteSin,SizeOf(RemoteSin),0);
  FHandle:=ASocket;
end;

procedure TApiSocket.NewSocket;
begin
  Init(fpSocket(AF_INET, SOCK_STREAM, 0))
end;

function TApiSocket.IsValid:boolean;inline;
begin
  Result:=FHandle<>INVALID_SOCKET;
end;

procedure TApiSocket.Bind(pPort, BackLog : word); begin
  with RemoteSin do begin
    Sin_Family := AF_INET;
    Sin_Addr.s_Addr := 0;
    Sin_Port   := htons(pPort);
  end;
  {$IFNDEF MSWINDOWS}
    fpSetSockOpt(Socket, SOL_SOCKET, SO_REUSEADDR, @RemoteSin, SizeOf(RemoteSin));
  {$ENDIF} // remedy socket port locking on Posix platforms
  fpBind(FHandle, @RemoteSin, sizeof(RemoteSin));
  fpListen(FHandle, BackLog);
end;

{
Attempts to establish a new TCP connection, used on the client side
@param Host IP address to the server machine
@param Port Port number to connect
@see Error
}
procedure TApiSocket.Connect(Host : string; pPort : word); begin
  with RemoteSin do begin
    Sin_Family := AF_INET;
    Sin_Addr   := StrToNetAddr(Host);
    Sin_Port   := htons(pPort);
  end;
  fpConnect(FHandle, @RemoteSin, sizeof(RemoteSin));
end;

// Returns the host IP address
function TApiSocket.GetHostAddress: string;
var
  Tam : integer;
  Addr: SockAddr;
begin
  Tam := sizeof(Addr);
  fpGetSockName(FHandle, @Addr, @Tam);
  Result := NetAddrToStr(Addr.Sin_Addr);
end;

procedure TApiSocket.Close;
begin
  fpshutdown(FHandle,0);
  CloseSocket(FHandle);
  FHandle:=INVALID_SOCKET;
end;

function TApiSocket.Accept(Timeout : integer) : integer;
var
 sz : integer;
begin
  if CanRead(Timeout) then begin
    sleep(0);
    sz := sizeof(RemoteSin);
    Result := fpAccept(FHandle, @RemoteSin, @sz)
  end
  else
    Result := 0;
end;

function TApiSocket.WaitingData : cardinal;
var
  L : Cardinal;
begin
  Sleep(1);
  if IOCtlSocket(FHandle, FIONREAD, @L) <> 0 then Result := 0
                                             else Result := L;
end;

{
Tests if data are available for reading within the timeout
@param Timeout Max time to wait until returns
@return True if data are available, false otherwise
}
function TApiSocket.CanRead(Timeout : Integer) : Boolean;
var
  FDS: TFDSet;
  TimeV: TTimeVal;
begin
  FD_Zero(FDS);
  FD_Set(FHandle, FDS);
  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
  Result := Select(FHandle + 1, @FDS, nil, nil, @TimeV) > 0;
end;

// Cleans the socket input stream
procedure TApiSocket.Purge;
var
	Tam : cardinal;
	Buffer : pointer;
begin
  Tam := WaitingData;
	if Tam = 0 then exit;
	getmem(Buffer, Tam);
	fpRecv(FHandle, Buffer, Tam, 0);
	freemem(Buffer, Tam);
end;


function TApiSocket.RecvPacket( Buf: PUBuffer; Timeout : integer): Integer;
begin
  Result:=UltraBuffSize-Buf^.FCursor; // calc the space left in buffer
  if Result=0 then exit; // buffer full .. cannot read  more data in it
  if Not CanRead(Timeout) then exit(-1); // socket not ready

  {$IFNDEF MSWINDOWS}fpSetErrNo(0);{$ENDIF}
  Result:=fpRecv(FHandle,@Buf^.FData[Buf^.FLen],Result,0); // try to fill the buffer
  Sleep(1); // required on some environments to prevent streaming truncation
  //Writeln(format('%d bytes received',[Result]));
  if Result<>-1 then
    begin
      Buf^.FCursor:=Buf^.Len; // now the cursor points the first byte of the new data
      Inc(Buf^.FLen,Result);
    end
end;

function TApiSocket.RecvString(Timeout : integer) : AnsiString;
var L,P: Integer;
    Buf: PUBuffer;
begin
  Result := '';
  Buf:=TUBuffer.Alloc;

  repeat
    L:=RecvPacket(Buf,Timeout);
    if (L=0) or (WaitingData=0) then // no more data or buffer full?
      begin
        P:=Length(Result);
        SetLength(Result,P+Buf^.Len);
        Move(Buf^.DataPtr(0)^,Result[P+1],Buf^.Len);
        if L=0 then Buf^.Reset;
        if WaitingData=0 then break;
        break;
      end
   until L=-1;
end;

{
Sends a string by the socket
@param Data String to send
@see Error
}
procedure TAPISocket.SendString(const Data : AnsiString);
begin
 fpSend(FHandle, @Data[1], length(Data), 0);
end;

{
Use Error method to test if others TBlockSocket methods succeded
@return 0 if success or socket error code otherwise
}
function TAPISocket.Error : integer; begin
  Result := SocketError
end;

initialization

InitBuffers; // for the main thread

finalization

ReleaseBuffers; // for the main thread

end.
