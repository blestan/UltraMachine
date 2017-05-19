unit UltraContext;

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
             FRequestPath: XVar;
             FRequestParams: XVar;
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

             function ParseHTTPRequest: Integer;

             property Buffer: PUBuffer read FBuffer;

             property RequestMethod: HTTP_Method read FRequestMethod;
             property RequestPath: XVar read FRequestPath;
             property RequestParams: XVar read FRequestParams;
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
 FSocket.Init(ASocket);
 FBuffer:=TUBuffer.Alloc;

 FRequestMethod := mtUnknown;
 FRequestPath:=XVar.Null;
 FRequestParams:=XVar.Null;
 FRequestProtocol := HTTP10;
 FRequestHost := '';
 FRequestPort := 0;
 FRequestHeaders:=XVar.Null;

 ResponseCode:=0;
 FResponse:=XVar.Null;
end;

procedure TContext.Cleanup;
begin
  FSocket.Close;
  FResponse.Free;
  FRequestHeaders.Free;
  FRequestParams.Free;
  FRequestPath.Free;
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
  Send('Connection: Closed'#13#10#13#10);
end;


function TContext.ParseHTTPRequest: Integer;
var P: TUltraParser;
    i: integer;
begin
  repeat
    P.Init(FBuffer^.DataPtr(0),FBuffer^.Len);
    FRequestMethod:=P.ParseMethod;
    if FRequestMethod=mtUnknown then ReadBuffer;
  until FRequestMethod<>mtUnknown;
  FRequestPath:=XVar.New(xtArray);

  P.ParseURL(FRequestPath);

  FRequestProtocol:=P.ParseProtocol;

  FRequestHeaders:=XVar.New(xtObject);

  P.ParseHeaders(FRequestHeaders);

  Result:=P.Error;

end;

end.

