unit UltraHttp;

{$mode objfpc}{$H+}

interface

uses xtypes,xon,UltraSockets;

  type

    HTTP_Method = (        mtUnknown = 0,
                           mtOPTIONS,
                           mtGET,
                           mHEAD,
                           mtPOST,
                           mtPUT,
                           mtDELETE,
                           mtTRACE,
                           mtCONNECT
    );

    HTTP_Protocol = ( HTTPUnknown = 0,
                      HTTP10,
                      HTTP11,
                      HTTP20
    );

  const


    HTTP_ERROR_NONE = 0;   // No error
    HTTP_ERROR_INVAL = -1; // Invalid character inside HTTP Header
    HTTP_ERROR_PARTIAL = -2;  // This is not a full HTTP Header, more characters expected

    HTTP_OK         = 200;
    HTTP_Accepted   = 202;

    HTTP_BadRequest = 400;
    HTTP_Unauthorized = 401;
    HTTP_NotFound   = 404;
    HTTP_MethodNotAllowed = 405;

    HTTP_VersionNotSupported = 505;



    function HTTPStatusPhrase( Status: Integer): String;

implementation

const

HTTP_ERROR_200 : array[200..202] of string =
                     ('OK',
                      'created',
                      'accepted');

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
    400..499: Result := HTTP_ERROR_400[Status];
    500..599: Result := HTTP_ERROR_500[Status];
   end;

 end;

end.

