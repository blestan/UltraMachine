unit UltraHandlers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UltraSockets, Contexts;

type


   TBaseHandler=class
     Private
        FCOntext: PContext;
     public
       constructor Create(var Context : TContext);
       procedure HandleRequest;virtual;abstract;
       property Context: PContext read  FContext;
  end;

  THandlerClass = class of TBaseHandler;


implementation

Constructor TBaseHandler.Create(var Context : TContext);
begin
  FCOntext:=@Context;
end;


end.

