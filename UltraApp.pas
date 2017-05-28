unit UltraApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UltraContext;

type

   TCustomUltraHandler=class;

   TUltraApp=class
         private
           FName: String;
           FKey: String;
         public
           Constructor Create(const AppName, AppKey: String);
           Destructor Destroy;override;
           function HandleRequest(var Context:TUltraContext): boolean ;virtual;
           property AppName: String read FName;
           property AppKey: String read FKey;
   end;

   TCustomUltraHandler=class
     Private
        FCOntext: PUltraContext;
     public
       constructor Create(var Context : TUltraContext);
       function  HandleRequest:boolean;virtual;
       property Context: PUltraContext read  FContext;
  end;



implementation

constructor TUltraApp.Create(const AppName,AppKey: String);
begin
  FName:=AppName;
  FKey:= AppKey;
end;

destructor TUltraApp.Destroy;
begin
end;

function TUltraApp.HandleRequest(var Context: TUltraContext): Boolean;
begin
  Result:=False
end;

Constructor TCustomUltraHandler.Create(var Context : TUltraContext);
begin
  FCOntext:=@Context;
end;

function TCustomUltraHandler.HandleRequest:boolean;
begin
  Result:=False;
end;

end.

