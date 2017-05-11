unit UltraApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UltraSockets, UltraContext;

type

   TBaseHandler=class;
   TUltraApp=class
         private
           FName: String;
         public
           Constructor Create(const AName: String);
           Destructor Destroy;override;
           function HandleRequest(var AContext: TContext): TBaseHandler;virtual;
           property AppName: String read FName;
   end;

   TBaseHandler=class
     Private
        FCOntext: PContext;
     public
       constructor Create(var Context : TContext);
       procedure HandleRequest;virtual;abstract;
       property Context: PContext read  FContext;
  end;



implementation

constructor TUltraApp.Create(const AName: String);
begin
  FName:=AName;
end;

destructor TUltraApp.Destroy;
begin
end;

function TUltraApp.HandleRequest(var AContext: TContext): TBaseHandler;
begin
  Result:=nil
end;

Constructor TBaseHandler.Create(var Context : TContext);
begin
  FCOntext:=@Context;
end;


end.

