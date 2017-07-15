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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
uses UltraSockets,UltraHttp,UltraContext,UltraApp,UltraBackend,xins,xon,xonjson;

{$R *.lfm}

type


     MyHandler=class (TUltraController)
                    public
                      procedure GET(var Context: TUltraContext);message ord(hmGET);
                    end;


var


Server: TUltraServer=nil;
MyApp: TUltraApp;


procedure MyHandler.GET(var Context: TUltraContext);
var RespStr: String;
          i: integer;
begin
  RespStr:=Form1.Memo3.Lines.Text;
  for i:=0 to Context.Request.Headers.Count-1 do Form1.Memo1.Lines.Add(Context.Request.Headers.Keys[i].AsString+':'+Context.Request.Headers[i].AsString);
  Context.Socket.Send(RespStr+#13#10);
  Context.Socket.Shutdown(2);
  Context.Handled:=true;
end;

{ TForm1 }
procedure TForm1.StartActionExecute(Sender: TObject);
var Conf: XVar;
begin
  Conf:=JSON2XON('{"net":{"port":9001}}');
  Server.Configure(Conf);
  Server.Start;
  GroupBox1.Caption:=format('Backend running [%d]',[Server.ThreadID]);
  Conf.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 if Server=nil  then
  begin
   Server:=TUltraServer.Create;
   MyApp:=TUltraApp.Create(1,'ABCDE123456');
   MyApp.RegisterController('login',[hmGET,hmPOST],MyHandler);
   Server.InstallApp('myapp',MyApp)
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Server<>nil then FreeAndNil(Server);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Caption:=IntToStr(SizeOf(XContainer));
end;


procedure TForm1.StartActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:= (Server=nil) or (not Server.Running);
end;

procedure TForm1.StopActionExecute(Sender: TObject);
begin
  Server.Stop;
  GroupBox1.Caption:=format('Backend Stopped',[Server.ThreadID]);
end;

procedure TForm1.StopActionUpdate(Sender: TObject);
begin
  with (Sender as TAction) do
   begin
     Enabled:= (Server<>nil) and Server.Running;
     if Server<>nil then Caption:=format('Stop [%d]',[Server.ThreadsCount]);
   end;
end;
end.

