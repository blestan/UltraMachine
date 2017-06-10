unit UltraSockets;
{$mode objfpc}{$H+}
{$modeswitch AdvancedRecords}
interface

uses  Sockets,UltraBuffers;


const

    UltraTimeOut = 300;

    INVALID_SOCKET: TSocket = not(0);

type

  TSocket = Integer;

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
    procedure Shutdown(Mode: Integer);

    function RecvPacket(Buf: Pointer; MaxSize: Integer; Timeout : integer): Integer;
    procedure Send(const Data: AnsiString);
    function WaitingData : cardinal;
    function CanRead(Timeout: Integer): Boolean;
    function Error : integer;
    function GetHostAddress : string;
    property Socket: TSocket read FHandle;
    property Valid: Boolean read IsValid;
  end;


implementation

uses
  SysUtils,
  {$IFDEF MSWINDOWS}
    WinSock2
  {$ELSE}
    {$DEFINE IOCtlSocket:=fpIOCtl}{$DEFINE FD_Zero:=fpFD_Zero}{$DEFINE FD_Set:=fpFD_Set}{$DEFINE Select:=fpSelect}
  {$ENDIF};


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
  fpshutdown(FHandle,2);
  CloseSocket(FHandle);
  FHandle:=INVALID_SOCKET;
end;

procedure TApiSocket.Shutdown(Mode: Integer); inline;
begin
  fpshutdown(FHandle,Mode);
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
	Res,Tam : cardinal;
	Buffer : pointer;
begin
  Write('Begin Purging...');
  Tam := WaitingData;
  //if Tam = 0 then exit;
  getmem(Buffer, Tam);
  Res:=fpRecv(FHandle, Buffer, Tam, 0);
  Writeln(format('%d bytes purged',[res]));
  freemem(Buffer, Tam);
end;


function TApiSocket.RecvPacket(Buf: Pointer; MaxSize: Integer; Timeout : integer): Integer;
begin
  if Not CanRead(Timeout) then exit(-1); // socket not ready
  {$IFNDEF MSWINDOWS}fpSetErrNo(0);{$ENDIF}
  Result:=fpRecv(FHandle,Buf,MaxSize,0); // try to fill the buffer
  Sleep(1); // required on some environments to prevent streaming truncation
  //Writeln(format('%d bytes received',[Result]));
end;

{
Sends a string by the socket
@param Data String to send
@see Error
}
procedure TAPISocket.Send(const Data : AnsiString);
var i: Integer;
begin
 i:=fpSend(FHandle, @Data[1], length(Data), 0);
end;

{
Use Error method to test if others TBlockSocket methods succeded
@return 0 if success or socket error code otherwise
}
function TAPISocket.Error : integer; begin
  Result := SocketError
end;

end.
