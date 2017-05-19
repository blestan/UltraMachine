unit UltraHttp;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses xtypes,xon,UltraSockets;

type


    HTTP_Method = (        mtUnknown = 0,
                           mtOPTIONS,
                           mtGET,
                           mtHEAD,
                           mtPOST,
                           mtPUT,
                           mtDELETE,
                           mtPATCH,
                           mtTRACE,
                           mtCONNECT
    );

    HTTP_Protocol = ( HTTPUnknown = -1,
                      HTTP10,
                      HTTP11,
                      HTTP20
    );


    TStatuses = (
                    psReady,

                    psError,
                    psNeedData,

                    psInUrl,
                    psInParams,

                    psInHeaders,
                    psInBody,

                    psMethodDone,
                    psUrlDone,
                    psParamsDone,
                    psVersionDone,
                    psHeadersDone,
                    psBodyDone
                  );
    TParserStatus = set of TStatuses;

    TUltraParser=record
      private
         FStatus: TParserStatus;
         FError: Integer;
         FPos: Integer;
         FLen: Integer;
         FUrlPos: Integer;
         FParamsPos: Integer;
         FVerPos: Integer;
         FData: PChar;
         FURL: String;
         FContentLength: Integer;
         procedure Reset;
         function Next(Normalize: boolean=true): Char;
         function SkipBlank(Normalize: boolean=true): Char;
         function SkipCRLF(Normalize: boolean =true): Char;
     public
       function ParseMethod: HTTP_Method;
       function ParseURL( Segments: XVar):boolean;
       function ParseProtocol: HTTP_Protocol;
       function ParseHeaders( Headers: XVar):boolean;
       property Status: TParserStatus read FStatus;
       property Error: Integer read FError;
       procedure Init( Buffer: PChar; BuffLen: Integer);
    end;

  const


    HTTP_ERROR_NONE = 0;   // No error
    HTTP_ERROR_INVAL = -1; // Invalid character inside HTTP Header
    HTTP_ERROR_PARTIAL = -2;  // This is not a full HTTP Header, more characters expected
    HTTP_ERROR_TOOLONG = -3;

    //  HTTP Response codes

    HTTP_OK         = 200;
    HTTP_Accepted   = 202;

    HTTP_BadRequest = 400;
    HTTP_Unauthorized = 401;
    HTTP_NotFound   = 404;
    HTTP_MethodNotAllowed = 405;

    HTTP_VersionNotSupported = 505;



    function HTTPStatusPhrase( Status: Integer): String;

implementation

procedure TUltraParser.Init( Buffer: PChar; BuffLen: Integer);
begin
  Reset;
  FData:=Buffer;
  FLen:=BuffLen;
end;

Procedure TUltraParser.Reset;
begin
  FStatus:=[psReady];
  FError:=HTTP_ERROR_NONE;

  FData:=nil;
  FPos:=0;
  FLen:=0;


  FUrlPos:=0;
  FVerPos:=0;
  FParamsPos:=0;

  FContentLength:=0;
end;

function TUltraParser.Next(Normalize: boolean=true): Char;
begin
  if FPos<FLen then
   begin
    Result:=FData[FPos];
    if Normalize and (Result in ['A'..'Z']) then
      begin
       Inc(Result,32);
       FData[FPos]:=Result;
      end;
    Inc(FPos)
   end
    else
   Begin
     Result:=#0;
     Include(FStatus,psNeedData);
   end;
end;

function TUltraParser.SkipBlank(Normalize: boolean=true): Char;
var c: char;
begin
  c:=Next;
  while c in [#8,#32] do c:=Next(Normalize);
  Result:=c;
end;

function TUltraParser.SkipCRLF(Normalize: boolean=true): Char;
begin
  if (SkipBlank(Normalize)=#13) and (next=#10) then exit(next(Normalize))
                                              else Include(FStatus,psError);
  Result:=#0;
end;

function TUltraParser.ParseMethod:HTTP_Method;
begin
  FPos:=00; // method is located at the begining of the start line
  Result:=mtUnknown;
  Case next of

   // to do : add all methods

  'd': if (next='e') and (next='l') and (next='e') and (next='t') and (next='e') and (next=#32) then Result:=mtDelete;
  'g': if (next='e') and (next='t') and (next=#32) then Result:=mtGet;
  'p': case next of
          'a': if (next='t') and (next='c') and (next='h') and (next=#32) then Result:=mtPATCH;
          'o': if (next='s') and (next='t') and (next=#32) then Result:=mtPOST;
          'u': if (next='t') and (next=#32) then Result:=mtPUT;

       end
 end;
  if Result<>mtUnknown then Include(FStatus,psMethodDone)
      else include(FStatus,psError)
end;

function TUltraParser.ParseURL( Segments: XVar):boolean;
var     i,s: Integer;
        c: char;
begin
  if FUrlPos<>0 then FPos:=FUrlPos //this is not our first run
                else
                  begin
                     if SkipBlank<> '/' then
                       begin
                        Include(FStatus,psError);
                        exit
                        end;
                       FUrlPos:=FPos;
                  end;

  i:=0;
  s:=FPos;
  c:=Next;
  while not (c in [#13,#32]) do
   begin
     case c of
       #0: begin
            FUrlPos:=s;
            exit(false);
           end;
       '/': begin
             Segments.Add(xtString).SetString(@FData[S],i);
             i:=0;
             s:=FPos;
             c:=Next;
            end;
        else
       begin
        inc(i);
        c:=Next;
      end
     end
   end
end;

function TUltraParser.ParseProtocol: HTTP_Protocol;
var major,minor: char;
begin
  Result:=HTTPUnknown;
  if FVerPos<>0 then FPos:=FVerPos //this is not our first run
                else FVerPos:=FPos;

  if (SkipBlank<>'h') or (next<>'t') or (next<>'t') or (next<>'p') or (next<>'/') then exit;
  major:=next;
  if not (major in ['1'..'2'])then exit;
  if next<>'.' then exit;
  minor:=next;
  if not (major in ['0'..'1'])then exit;
  case major of
    '1': case minor of
            '0': Result:=HTTP10;
            '1': Result:=HTTP11;
         end;
    '2': if minor='0' then Result:=HTTP20;
  end;
  Include(FStatus,psVersionDone);
end;

function TUltraParser.ParseHeaders( Headers: XVar):boolean;
var start,i:integer;
begin
  Include(FStatus,psInHeaders);
  while not(SkipCRLF in [#0,#13]) do
  begin
   start:=FPos-1;
   i:=1;
   while next<>':' do inc(i);
   XVar.New(xtString,Headers).SetString(@FData[start],i);
   start:=FPos;
   i:=0;
   while next<>#13 do inc(i);
   XVar.New(xtString,Headers).SetString(@FData[start],i);
   Dec(FPos);
  end;
  if next=#13 then
   begin
    Exclude(FStatus,psInHeaders);
    Include(FStatus,psHeadersDone);
   end;
end;


const

HTTP_RESPONSE_200 : array[200..202] of string =
                     ('OK',
                      'Created',
                      'Accepted');

HTTP_ERROR_400 : array[400..405] of string =
                     ('Bad Request',
                      'Unauthorized',
                      'Payment Required',
                      'Forbidden',
                      'Not Found',
                      'Method Not Allowed');

HTTP_ERROR_500 : array[500..505] of string =
                     ('Internal Server Error',
                      'Not Implemented',
                      'Bad Gateway',
                      'Service Unavailable',
                      'Gateway Timeout',
                      'HTTP Version Not Supported');



 function HTTPStatusPhrase( Status: Integer): String;
 begin
   Result:='';
   Case Status of
    200..299: Result := HTTP_RESPONSE_200[Status];
    400..499: Result := HTTP_ERROR_400[Status];
    500..599: Result := HTTP_ERROR_500[Status];
   end;

 end;

end.

