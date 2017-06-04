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
           class Function Version: Integer;virtual;
           procedure HandleRequest(var Context:TUltraContext);virtual;abstract;
           property Name: String read FName;
           property Key: String read FKey;
   end;

   TCustomUltraHandler=class
     Private
        FCOntext: PUltraContext;
     public
       constructor Create(var Context : TUltraContext);
       procedure  Execute;virtual;abstract;
       property Context: PUltraContext read  FContext;
  end;



implementation

constructor TUltraApp.Create(const AppName,AppKey: String);
begin
  FName:=AppName;
  FKey:= AppKey;
end;

class function TUltraApp.Version: integer;
begin
  Result:=1;
end;

Constructor TCustomUltraHandler.Create(var Context : TUltraContext);
begin
  FCOntext:=@Context;
end;


end.

