unit UltraParser;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses UltraHTTP,UltraBuffers;


function ParseHTTPRequest(var Buffer: TUltraBuffer; var Request: TUltraRequest) : Integer;

implementation

uses xtypes, xon;

function ParseHTTPRequest(var Buffer: TUltraBuffer; var Request: TUltraRequest) : Integer;
Var Error : Integer;
        V : String;
function Next(Normalize: boolean=true): Char;
begin
 Result:=Buffer.NextChar(Normalize);
 if Result=#0 then Error:=HTTP_ERROR_PARTIAL;
end;

function ParseMethod:boolean;
begin
  Request.Method:=hmUnknown;
  Case next of // to do : add all methods

  'd': if (next='e') and (next='l') and (next='e') and (next='t') and (next='e') and (next=#32) then Request.Method:=hmDelete;
  'g': if (next='e') and (next='t') and (next=#32) then Request.Method:=hmGet;
  'p': case next of
          'a': if (next='t') and (next='c') and (next='h') and (next=#32) then Request.Method:=hmPATCH;
          'o': if (next='s') and (next='t') and (next=#32) then Request.Method:=hmPOST;
          'u': if (next='t') and (next=#32) then Request.Method:=hmPUT;

       end
 end;
  if Request.Method<>hmUnknown then exit(true);
  while not (next in [#0,#32]) do;  // skip unsupported method chars
  Error:=HTTP_MethodNotAllowed;
  Result:=false;
end;

function ParseURL:boolean;
var     i,s: Integer;
        c: char;
label done,err;
begin
  if Next<>'/' then goto err;
  i:=0;
  s:=Buffer.Cursor;
  c:=Next;
  while c<>#13 do
     case c of
       #0: goto err;
       #32,'/','?': begin
             if i>0 then Request.Path.Add(xtString).SetString(Buffer.DataPtr(S),i);
             case c of
              #32: goto done;
              '?': break; // params start found
             end;
             i:=0;
             s:=Buffer.Cursor;
             c:=Next;
            end;
        else
       begin
        inc(i);
        c:=Next;
       end
     end;
   i:=0;
   s:=Buffer.Cursor;
   c:=next; //parse params
   while c <>#13 do
    case c of
           #0: goto err;
      #32,'&': begin
                Request.Path.Add(xtString).SetString(Buffer.DataPtr(S),i);
                if c=#32 then goto done;
                i:=0;
                s:=Buffer.Cursor;
                c:=Next;
              end;
          else
             begin
               inc(i);
               c:=Next;
             end
    end;
done:
 exit(true);
err:
 Result:=False;
 Error:=HTTP_BadRequest;
end;

function ParseProtocol:boolean;
var major,minor: char;
label err;
begin
  Request.Protocol:=HTTPUnknown;
  if (next<>'h') or (next<>'t') or (next<>'t') or (next<>'p') or (next<>'/') then goto err;
  major:=next;
  if not (major in ['1'..'2'])then goto err;
  if next<>'.' then goto err;
  minor:=next;
  if not (major in ['0'..'1'])then goto err;
  case major of
    '1': case minor of
            '0': Request.Protocol:=HTTP10;
            '1': Request.Protocol:=HTTP11;
         end;
    '2': if minor='0' then Request.Protocol:=HTTP20;
  end;

  if Request.Protocol<>HTTPUnknown then exit(true);

err:
  Result:=false;
  Error:=HTTP_VersionNotSupported;
end;

function ParseHeaders:boolean;
var start,i:integer;
    c: char;
label err;
begin
  if (next<>#13) or (next<>#10) then goto err;
  c:=next;
  while c<>#13 do
  begin
   if c=#0 then goto err;
   start:=Buffer.Cursor-1;
   i:=1;

   repeat
     case next of
       #0: goto err;
      ':': break;
      else inc(i);
     end
   until false;

   XVar.New(xtString,Request.Headers).SetString(PChar(Buffer.DataPtr(start)),i);

   start:=Buffer.Cursor;

   repeat
     c:=next;
     if c=#0 then goto err;
     if c=#32 then inc(start);
   until c<>#32;

   i:=1;
   repeat
     c:=next;
     if c=#0 then exit;
     if c<>#13 then inc(i);
   until c=#13;

   XVar.New(xtString,Request.Headers).SetString(PChar(Buffer.DataPtr(start)),i);
   next;  //skip #10
   c:=next;
  end;

  if next=#10 then exit(true);

err:
  Error:=HTTP_BadRequest;
  Result:=false;
end;

begin

 Error:=HTTP_ERROR_NONE;

 Buffer.Reset(False);
 if ParseMethod and ParseURL and ParseProtocol and ParseHeaders
  then
    begin
      Result:=HTTP_ERROR_NONE;
      V:=Request.Path[0].asString;
      if (length(V)=2) and (V[1] in ['V','v']) and (V[2] in ['0'..'9'])
        then Request.ApiVersion:=Ord(V[2])-Ord('0')
        else Result:=HTTP_NotFound;
    end
  else  Result:=Error;

end;

end.

