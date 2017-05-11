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
               const MaxPathSegments=15;
                     MaxParamsSegments=32;
               type TSegment=record Ps,Ln: Word end;
                    TPath= array [00..MaxPathSegments] of TSegment;
                    TParams = array [00..MaxParamsSegments] of TSegment;
             private
             FSocket: TAPISocket;

             FRequestMethod: HTTP_Method;
             FRequestURL: String;
             FPath: TPath;
             FRequestProtocol: HTTP_Protocol;
             FRequestHost: String;
             FRequestPort: Word;
             FRequestHeaders: XVar;
             FParams: TParams;

             FResponse: XVar;

             FBuffer: PUBuffer;

             function GetPathSegment(Index: Word): String;
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
             property RequestURL: String read FRequestURL;
             property RequestProtocol: HTTP_Protocol read FRequestProtocol;
             property RequestHost: String read FRequestHost;
             property RequestPort: Word read FRequestPort;
             property RequestHeaders: XVar read FRequestHeaders;

             property Response: XVar read FResponse;
             property Path[Index: Word]: String read GetPathSegment;

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
 FillByte(FPath,SizeOf(FPath),0);
 FillByte(FParams,SizeOf(FParams),0);
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
  Send('Connection: Closed'#13#10#13#10);
end;

function TContext.GetPathSegment(Index: Word): String;
begin
  Result:='';
  if index>MaxPathSegments then exit;
  Result:=Copy(FRequestURL,FPath[Index].Ps,FPath[Index].Ln);
end;

function TContext.ParseHTTPRequest: Integer;
var StartPos,
         Pos,
          Sz,
         Len: Integer;
         Buf: PChar;

         k,v: string;

function Tokenize(Delimiter: Char): Integer;
begin

 // Skip CRLF if any
  if Buf[Pos]=#13 then
   begin
     Inc(Pos);
     if Pos>=Len then exit(HTTP_ERROR_PARTIAL);
     if Buf[Pos]<>#10 then exit(HTTP_ERROR_INVAL); // wrong char!
     inc(Pos);
     exit(HTTP_ERROR_NONE)
   end;

 // skip leading spaces if any
  while (Pos<Len) and (Buf[Pos]=#32) do Inc(Pos);
  if Pos>=Len then exit(HTTP_ERROR_PARTIAL); // end of string

  StartPos:=Pos;

  while (Pos<len) do
    if  (Buf[Pos] = #13) or (Buf[pos]=Delimiter) then exit(Pos-StartPos)
                                                 else inc(pos);
  Result:=HTTP_ERROR_PARTIAL; // end of string
end;

function ParseMethod: HTTP_Method;
var sz: Integer;
begin
 Result := mtUnknown;
 Sz:=Tokenize(#32);
 if sz<3 then exit;
 case Buf[StartPos] of
  'g','G':  if (sz=3) and // GET
              (buf[StartPos+1] in ['e','E']) and
              (buf[StartPos+2] in ['t','T']) then Result := mtGET;

  'p','P': Result := mtPOST;
 end;
 // writeln(format('METHOD: "%s" size %d -> %d',[Copy(Pchar(@Buf[startpos]),0,sz),sz,Result]));
end;

function TokenStr(UpperC: Boolean=true): String;
var i: Integer;
    c: Char;
begin
  if sz<=0 then exit('');
  SetLength(Result,Sz);
  for i:=1 to sz do
   begin
    c:= Buf[StartPos+i-1];
    if UpperC and (c in ['a'..'z']) then dec(c,32);
    Result[i]:=c
   end;
  //Move(Buf[StartPos],Result[1],Sz);
end;

function ParseHeaders( out Key: String; out Value: String):boolean;
begin
 sz:=Tokenize(':');
 if sz<1 then exit(false);
 Key:=TokenStr;
 inc(Pos);
 sz:=Tokenize(#13);
 if sz<0 then exit(false);
 Value:=TokenStr;
 inc(pos);
 if (pos>=Len) or (buf[Pos]<>#10) then exit(false);
 inc(pos);
 result:=true;
end;

procedure ParseURL;
var i,j,Seg: Integer;
begin
 if Buf[StartPos]<>'/' then exit; // not a valid start of url
 Seg:=0;
 FPath[0].Ps:=2;
 FPath[0].Ln:=0;
 i:=0;
 j:=StartPos+1;
 while j<Pos do
  begin
    if Buf[j]='/' then
     begin
       inc(j);
       FPath[Seg].Ln:=i;
       Inc(Seg);
       if Seg>MaxPathSegments then exit;
       FPath[Seg].Ps:=(j-startpos)+1;
       FPath[Seg].ln:=0;
       i:=0;
     end;
    inc(j);
    inc(i);
  end;

end;

begin
  Pos:=0;
  StartPos:=0;
  Len:=FBuffer^.Len;
  Buf:=FBuffer^.DataPtr(0);

  // Method
  FRequestMethod:=ParseMethod;
  if FRequestMethod=mtUnknown then exit (HTTP_BadRequest);

  //URL
  sz:=Tokenize(#32);
  if sz<0 then exit(-1);
  FRequestURL:=TokenStr;
  ParseUrl; // Extract Path Segments

  // HTTP Version
  sz:=Tokenize(#32);

  if sz<8 then exit(HTTP_ERROR_PARTIAL);
  if sz>8 then FRequestProtocol:=HTTPUnknown
   else if (Buf[StartPos+5]='1') and (Buf[StartPos+7]='0') then FRequestProtocol:=HTTP10
     else if (Buf[StartPos+5]='1') and (Buf[StartPos+7]='1') then FRequestProtocol:=HTTP11
      else if (Buf[StartPos+5]='2') and (Buf[StartPos+7]='0') then FRequestProtocol:=HTTP20
       else FRequestProtocol:=HTTPUnknown;

  if FRequestProtocol<>HTTP11 then exit(HTTP_VersionNotSupported);

  Tokenize(#32);// skip first line crlf


  // Headers
  while ParseHeaders(k,v) do RequestHeaders.Add(xtString,k).AsString:=v;


  // Payload



  if Pos<=Len then Result:=HTTP_ERROR_NONE
              else Result:=HTTP_ERROR_PARTIAL;
end;

end.

