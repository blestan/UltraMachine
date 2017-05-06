unit TestMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ActnList, Menus;

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionList1: TActionList;
    GroupBox1: TGroupBox;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    Memo3: TMemo;
    StartAction: TAction;
    StartBackendBTN: TButton;
    StopAction: TAction;
    StopBackendBTN: TButton;
    procedure StartActionExecute(Sender: TObject);
    procedure StartActionUpdate(Sender: TObject);
    procedure StopActionExecute(Sender: TObject);
    procedure StopActionUpdate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
uses UltraSockets,contexts,UltraHandlers,UltraBackend;

{$R *.lfm}

type MyModule=class (TBaseHandler)
                    public
                      procedure HandleRequest;override;
                    end;


var
    RespStr: String ='';

procedure MyModule.HandleRequest;
var i: integer;
begin
  Form1.Memo1.Lines.Clear;
  Form1.Memo1.Lines.Add(format('Processing request in thread %d -->"%s"',[GetCurrentThreadId,Copy(Pchar(Context^.Buffer^.DataPtr(0)),0,Context^.Buffer^.Len)]));
  with Context^.RequestHeaders do for i:=0 to Count-1 do
   Form1.Memo1.Lines.Add(format('Header: "%s:%s"',[Keys[i].AsString,Vars[i].AsString]));
  RespStr:=Form1.Memo3.Lines.Text;
  Context^.Send(RespStr+#13#10);
end;


function ResolveRoute ( var AContext : TContext ) : THandlerClass;
begin
  Result:=MyModule;
end;

{ TForm1 }
procedure TForm1.StartActionExecute(Sender: TObject);
begin
  UltraStart('');
  GroupBox1.Caption:=format('Backend running [%d]',[BackendThread]);
end;



procedure TForm1.StartActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:= not BackendRunning;
end;

procedure TForm1.StopActionExecute(Sender: TObject);
begin
  BackendStop;
  GroupBox1.Caption:=format('Backend Stopped',[BackendThread]);
end;

procedure TForm1.StopActionUpdate(Sender: TObject);
begin
  with (Sender as TAction) do
   begin
     Enabled:= BackendRunning;
     Caption:=format('Stop [%d]',[BackendThreadsCount]);
   end;
end;
initialization
 SetRouteResolver(@ResolveRoute);
end.

