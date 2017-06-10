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
    MainMenu2: TMainMenu;
    Memo1: TMemo;
    Memo3: TMemo;
    StartAction: TAction;
    StartBackendBTN: TButton;
    StopAction: TAction;
    StopBackendBTN: TButton;
    procedure Memo3Change(Sender: TObject);
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
uses UltraContext,UltraApp,UltraBackend;

{$R *.lfm}

type


     TMyApp=class (TUltraApp)
       public
         procedure HandleRequest(var AContext: TUltraContext);override;
     end;

     MyHandler=class (TCustomUltraHandler)
                    public
                      procedure  Execute;override;
                    end;


var


    MyApp: TMyApp;

Procedure TMyApp.HandleRequest(var AContext: TUltraContext);
begin
  with MyHandler.Create(AContext) do
   begin
     Execute;
     Free;
   end
end;

procedure MyHandler.Execute;
var i: integer;
    E: QWord;
    RespStr: String;
begin
  RespStr:=Form1.Memo3.Lines.Text;
  Sleep(5000);
  Context^.Socket.Send(RespStr+#13#10);
  Context^.Socket.Shutdown(2);
  Context^.Handled;
  E:=GetTickCount64-Context^.StartTime;
  Form1.Memo1.Lines.BeginUpdate;
  Form1.Memo1.Lines.Clear;
 Form1.Memo1.Lines.Add(format('Processed request in thread %d for %d ticks-->"%s"',[GetCurrentThreadId,E,Copy(Pchar(Context^.Buffer^.DataPtr(0)),0,Context^.Buffer^.Len)]));
 Form1.Memo1.Lines.Add(format('Method:%d',[Context^.Request.Method]));
  with Context^.Request.Path do for i:=0 to Count-1 do
   Form1.Memo1.Lines.Add(format('Path Segments: "%s"',[Vars[i].AsString]));
 Form1.Memo1.Lines.Add(format('Version:%d',[Context^.Request.Protocol]));
  with Context^.Request.Headers do for i:=0 to Count-1 do
   Form1.Memo1.Lines.Add(format('Header: "%s:%s"',[Keys[i].AsString,Vars[i].AsString]));
  Form1.Memo1.Lines.EndUpdate;
end;

{ TForm1 }
procedure TForm1.StartActionExecute(Sender: TObject);
begin
  MyApp:=TMyApp.Create('myapp','ABCDE123456');
  UltraAddApp(MyApp);
  UltraStart('');
  GroupBox1.Caption:=format('Backend running [%d]',[UltraThread]);
end;

procedure TForm1.Memo3Change(Sender: TObject);
begin

end;



procedure TForm1.StartActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:= not UltraRunning;
end;

procedure TForm1.StopActionExecute(Sender: TObject);
begin
  UltraStop;
  GroupBox1.Caption:=format('Backend Stopped',[UltraThread]);
end;

procedure TForm1.StopActionUpdate(Sender: TObject);
begin
  with (Sender as TAction) do
   begin
     Enabled:= UltraRunning;
     Caption:=format('Stop [%d]',[UltraThreadsCount]);
   end;
end;
end.

