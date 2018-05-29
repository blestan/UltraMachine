unit UltraHttp;

{$mode objfpc}{$H+}
{$modeswitch AdvancedRecords}


interface

uses xon;

type

    HTTP_Method = (        hmUnknown,
                           hmOPTIONS,
                           hmGET,
                           hmHEAD,
                           hmPOST,
                           hmPUT,
                           hmDELETE,
                           hmPATCH,
                           hmTRACE,
                           hmCONNECT
    );

    HTTP_Methods= set of HTTP_Method;

    HTTP_Protocol = ( HTTPUnknown = -1,
                      HTTP10,
                      HTTP11,
                      HTTP20
    );


// Http request and response objects

  TUltraRequest = record
             private
              FData:  XVar;
             public
              Method: HTTP_Method;
              Protocol: HTTP_Protocol;
              ApiVersion: Integer;
              Path: XVar;
              Params: XVar;
              Headers: XVar;
              procedure Initialize;
              procedure Finalize;
  end;

  TUltraResponse=record
              Protocol: HTTP_Protocol;
              StatusCode: Integer;
              StatusText: String;
              ApiVersion: Integer;
              Headers: XVar;
              procedure Initialize;
              procedure Finilize;
  end;


const

    HTTP_ERROR_NONE = 0;   // No error
    HTTP_ERROR_PARTIAL = -1;  // This is not a full HTTP Header, more characters expected


    //  HTTP Response codes

    HTTP_OK         = 200;
    HTTP_Accepted   = 202;

    HTTP_BadRequest = 400;
    HTTP_Unauthorized = 401;
    HTTP_NotFound   = 404;
    HTTP_MethodNotAllowed = 405;

    HTTP_NotImplemented = 501;
    HTTP_BadGateway=502;
    HTTP_VersionNotSupported = 505;

    HEADER_API_KEY = 'x-api-key';
    HEADER_APP_KEY = 'x-application-key';



    function HTTPStatusPhrase( Status: Integer): String;

implementation

uses xtypes;

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

 procedure TUltraRequest.Initialize;
begin
 Method := hmUnknown;
 Protocol := HTTPUnknown;
 FData:=XVar.NewList(8);
 Path:=XVar.NewArray(FData,8);
 Params:=XVar.NewList(FData);
 Headers:=XVar.NewList(FData);
end;

Procedure TUltraRequest.Finalize;
begin
 FData.Free;
 Path:=XVar.Null;
 Params:=XVar.Null;
 Headers:=Xvar.Null;
end;

procedure TUltraResponse.Initialize;
begin
  Headers:=XVar.NewList;
end;

procedure TUltraResponse.Finilize;
begin
  Headers.Free;
end;

end.

