unit contexts;

{$mode objfpc}{$H+}
{$modeswitch AdvancedRecords}

interface

uses

 UltraHttp,UltraSockets,xtypes,xon;

type

  PContext= ^TContext;
  TContext=record
            private
             FSocket: TAPISocket;

             FRequestMethod: HTTP_Method;
             FRequestURL: String;
             FRequestProtocol: HTTP_Protocol;
             FRequestHost: String;
             FRequestPort: Word;
             FRequestHeaders: XVar;

             FResponse: XVar;

             FBuffer: PUBuffer;
            public
             ResponseCode: Integer;
             procedure Init(ASocket: TSocket);
             procedure Cleanup;
             function ReadBuffer: Integer;
             procedure Send(const Data: AnsiString);
             procedure SendResponse; // send default response in case of error, empty response, or unhandled

             property Buffer: PUBuffer read FBuffer;

             property RequestMethod: HTTP_Method read FRequestMethod;
             property RequestURL: String read FRequestURL;
             property RequestProtocol: HTTP_Protocol read FRequestProtocol;
             property RequestHost: String read FRequestHost;
             property RequestPort: Word read FRequestPort;
             property RequestHeaders: XVar read FRequestHeaders;

             property Response: XVar read FResponse;

  end;

implementation

uses sysutils;

procedure TContext.init(ASocket: TSocket);
begin
 FRequestHeaders:=XVar.New(xtObject,XVar.Null);
 FResponse:=XVar.Null;
 FSocket.Init(ASocket);
 FBuffer:=TUBuffer.Alloc;

 FRequestMethod := mtUnknown;
 FRequestURL := '';
 FRequestProtocol := HTTP10;
 FRequestHost := '';
 FRequestPort := 0;
 ResponseCode:=0;
end;

procedure TContext.Cleanup;
begin
  FSocket.Close;
  FRequestHeaders.Free;
  FResponse.Free;
  FBuffer^.Release;
  FBuffer:=nil;
end;

function TContext.ReadBuffer: Integer; inline;
begin
  Result:=FSocket.RecvPacket(FBuffer,UltraTimeOut);
end;

procedure TContext.Send(const Data: AnsiString);inline;
begin
  FSocket.SendString(Data);
end;

procedure TContext.SendResponse;
begin
  Send(Format('HTTP/1.1 %d %s'#13#10,[ResponseCode,HTTPStatusPhrase(ResponseCode)]));
  Send('Connection: Closed'#1310#13#10);
end;

end.

