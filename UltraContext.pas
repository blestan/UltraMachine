{

Context Singletons
One context per thread

}
unit UltraContext;


{$mode objfpc}{$H+}
{$modeswitch AdvancedRecords}

interface

uses

 UltraHttp,UltraSockets,UltraBuffers,xtypes,xon;

type



  TRequest = record
              Method: HTTP_Method;
              Protocol: HTTP_Protocol;
              Path: XVar;
              Params: XVar;
              Headers: XVar;
  end;

  TResponse=record
             Code: Integer;
             Headers: XVar;
  end;

  PUltraContext=^TUltraContext;
  TUltraContext=record
             private
              FStartTick: QWord;
              function ReadBuffer(const DataPtr: Pointer; MaxLen: Integer): Integer; // callback to read data in buffer when needed
             public
             Socket: TAPISocket;
             Request: TRequest;
             Response: TResponse;
             Buffer: PUBuffer;

             procedure Prepare(ASocket: TSocket);
             procedure Cleanup;

             procedure SendErrorResponse; // send default response in case of error, empty response, or unhandled

             property StartTick: QWord read FStartTick;
   end;



implementation

uses Sysutils;

procedure TUltraContext.Prepare(ASocket: TSocket);
begin
 FStartTick:=GetTickCount64;
 Socket.Init(ASocket);

 Buffer:=TUBuffer.Alloc;
 Buffer^.SetReadDataCallBack(@Self,@self.ReadBuffer);
 Request.Method := mtUnknown;
 Request.Path:=XVar.New(xtArray);
 Request.Params:=XVar.New(xtObject);
 Request.Headers:=XVar.New(xtObject);

 Request.Protocol := HTTPUnknown;

 Response.Code:=HTTP_ERROR_NONE;
 Response.Headers:=XVar.Null;
end;

procedure TUltraContext.Cleanup;
begin
  Socket.Close;
  Response.Headers.Free;
  Request.Headers.Free;
  Request.Params.Free;
  Request.Path.Free;
  Buffer^.Release;
  Buffer:=nil;
end;

procedure TUltraContext.SendErrorResponse;
begin
  with socket do
   begin
    Send(Format('HTTP/1.1 %d %s'#13#10,[Response.Code,HTTPStatusPhrase(Response.Code)]));
    Send('Connection: Closed'#13#10#13#10)
   end
end;

function TUltraContext.ReadBuffer(const DataPtr: Pointer; MaxLen: Integer): Integer;
begin
 Result:=Socket.RecvPacket(DataPtr,MaxLen,UltraTimeOut);
end;

end.

