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
           procedure HandleRequest(var Context:TUltraContext);virtual;abstract;
           property AppName: String read FName;
           property AppKey: String read FKey;
   end;

   TCustomUltraHandler=class
     Private
        FCOntext: PUltraContext;
     public
       constructor Create(var Context : TUltraContext);
       function  HandleRequest:boolean;virtual;abstract;
       property Context: PUltraContext read  FContext;
  end;



implementation

constructor TUltraApp.Create(const AppName,AppKey: String);
begin
  FName:=AppName;
  FKey:= AppKey;
end;


Constructor TCustomUltraHandler.Create(var Context : TUltraContext);
begin
  FCOntext:=@Context;
end;


end.

