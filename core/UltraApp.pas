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
                   Path: String;
                   SupportedMethods: HTTP_Methods;
                   ControllerClass: TUltraControllerClass;
             end;
         private
           FParams: XVar;
           FVersion: XVar;
           FAppKeys: XVar;
           FUsageCounter: XVar;

           FControllers: PControllerRec;

           function GetVersion:Integer;
           function GetUsageCounter:Integer;
         protected
            function FindController(AName: String; Method: HTTP_Method): TUltraControllerClass;
            procedure FreeControlers;
         public
           Constructor Create(AVersion: Integer; const AppKey: String);
           destructor Destroy; override;
           procedure HandleRequest(var Context:TUltraContext);virtual;
           function RegisterController(APath: String; Methods: HTTP_Methods; AClass: TUltraControllerClass):boolean;
           function ValidateKey(const AKey: String):boolean;
           //function UnregisterController(AName: String):boolean;
           property UsageCounter: Integer read GetUsageCounter;
           property Version: Integer read GetVersion;
   end;

   // this a base controller class used to dispach http methods
   TUltraController=class
     public
       constructor Create;virtual;
       procedure DefaultHandler(var context); override;
   end;



implementation
uses xtypes;

constructor TUltraApp.Create(AVersion:Integer; const AppKey: String);
begin
  FControllers:=nil;

  FParams:=XVar.NewList(8);

  FVersion:=FParams.Add(xtInteger,'Version');
  FVersion.AsInteger:=AVersion;

  FUsageCounter:=FParams.Add(xtInteger,'Usage-Counter');

  FAppKeys:=FParams.AddArray('APP-Keys',8);
  if AppKey<>'' then FAppKeys.Add(xtString).SetString(AppKey);

end;

destructor TUltraApp.Destroy;
begin
  FParams.Free;
  FreeControlers;
  Inherited Destroy;
end;

procedure TUltraApp.FreeControlers;
var C,CC: PControllerRec;
begin
  C:=FControllers;
  FControllers:=nil;
  while C<>nil do
     begin
       C^.Path:='';
       CC:=C;
       C:=C^.Next;
       FreeMem(CC);
     end;
end;

function TUltraApp.GetVersion:Integer;inline;
begin
 Result:=FVersion.AsInteger;
end;

function TUltraApp.GetUsageCounter:Integer;inline;
begin
 Result:=FUsageCounter.AsInteger;
end;

function TUltraApp.FindController(AName: String; Method: HTTP_Method): TUltraControllerClass;
var Ctrl: PControllerRec;
begin
 Result:=nil;
 Ctrl:=FControllers;
 while Ctrl<>nil do
   if (Ctrl^.Path=AName) and
     ((Ctrl^.SupportedMethods=[]) or (Method in Ctrl^.SupportedMethods)) then exit(Ctrl^.ControllerClass)
     else Ctrl:=Ctrl^.Next;
end;

function TUltraApp.RegisterController(APath: String; Methods: HTTP_Methods; AClass: TUltraControllerClass):boolean;
var N: PControllerRec;
begin
 N:=GetMem(SizeOf(N^));
 FillChar(N^,SizeOf(TControllerRec),0);
 with N^ do
  begin
    Next:=FControllers;
    Path:=APath;
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
       FUsageCounter.Increment;
       Context.MTDID:=Ord(Context.Request.Method);
       Context.Application:=Self;
       Dispatch(context);
       Free;
     end
    else Context.Response.StatusCode:=HTTP_NotFound;
end;

function TUltraApp.ValidateKey(const AKey: String):Boolean;
var i,l: Integer;
begin
 l:=FAppKeys.Count;
 if l=0 then exit(true); // if no keys are provided then any key is valid
 if AKey='' then exit(false); // empty keys are not valid if keys are registered
 for i:=0 to l-1 do
   if (FAppKeys[i].AsString=AKey) then exit(true);
 Result:=False;
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

