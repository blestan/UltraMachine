unit UltraApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UltraHttp,UltraContext,xon;

type

   TUltraController=class;

   TUltraControllerClass = class of TUltraController;

   TUltraApp=class
         private
          type
              PControllerRec=^TControllerRec;
              TControllerRec = record
                   Next: PControllerRec;
                   Enabled: Boolean;
                   ControllerName: String;
                   SupportedMethods: HTTP_Methods;
                   ControllerClass: TUltraControllerClass;
             end;
         private
           FKey: String;
           FAppKeys: XVar;
           FControllers: PControllerRec;
           FUsageCounter: Cardinal;
         protected
           function FindController(AName: String; Method: HTTP_Method): TUltraControllerClass;
            procedure FreeControlers;
         public
           Constructor Create(const AppKey: String);
           destructor Destroy; override;
           class Function Version: Integer;virtual;
           procedure HandleRequest(var Context:TUltraContext);virtual;
           function RegisterController(AName: String; Methods: HTTP_Methods; AClass: TUltraControllerClass):boolean;
           //function UnregisterController(AName: String):boolean;
           property Key: String read FKey;
           property UsageCounter: Cardinal read FUsageCounter;
   end;

   // this a base controller class used to dispach http methods
   TUltraController=class
     public
       constructor Create;virtual;
       procedure DefaultHandler(var context); override;
   end;



implementation
uses xtypes;

constructor TUltraApp.Create(const AppKey: String);
begin
  FKey:= AppKey;
  FControllers:=nil;
  FUsageCounter:=00;
  FAppKeys:=XVar.New(xtList);
end;

destructor TUltraApp.Destroy;
begin
  FreeControlers;
  FAppKeys.Free;
  Inherited Destroy;
end;

procedure TUltraApp.FreeControlers;
var C,CC: PControllerRec;
begin
  C:=FControllers;
  FControllers:=nil;
  while C<>nil do
     begin
       C^.ControllerName:='';
       CC:=C;
       C:=C^.Next;
       FreeMem(CC);
     end;
end;

class function TUltraApp.Version: integer;
begin
  Result:=1;
end;

function TUltraApp.FindController(AName: String; Method: HTTP_Method): TUltraControllerClass;
var Ctrl: PControllerRec;
begin
 Result:=nil;
 Ctrl:=FControllers;
 while Ctrl<>nil do
   if (Ctrl^.ControllerName=AName) and
     ((Ctrl^.SupportedMethods=[]) or (Method in Ctrl^.SupportedMethods)) then exit(Ctrl^.ControllerClass)
     else Ctrl:=Ctrl^.Next;
end;

function TUltraApp.RegisterController(AName: String; Methods: HTTP_Methods; AClass: TUltraControllerClass):boolean;
var N: PControllerRec;
begin
 N:=GetMem(SizeOf(N^));
 FillChar(N^,SizeOf(TControllerRec),0);
 with N^ do
  begin
    Next:=FControllers;
    Enabled:=True;
    ControllerName:=AName;
    SupportedMethods:=Methods;
    ControllerClass:=AClass;
  end;
 FControllers:=N;
 Result:=true;
end;

procedure TUltraApp.HandleRequest(var Context:TUltraContext);
var C: TUltraControllerClass;
begin
  C:=FindController(Context.Request.Path[2].AsString,Context.Request.Method);
  if C<>nil then
   with C.Create do
     begin
       Inc(FUsageCounter);
       Context.MTDID:=Ord(Context.Request.Method);
       Context.Application:=Self;
       Dispatch(context);
       Free;
     end
    else Context.Response.StatusCode:=HTTP_NotFound;
end;

// Controller


Constructor TUltraController.Create;
begin
end;

procedure TUltraController.DefaultHandler(var context);
begin
 with TUltraContext(context) do  Response.StatusCode:=HTTP_NotImplemented;
end;

end.

