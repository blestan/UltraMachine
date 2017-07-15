unit UltraBackend;
{$mode objfpc}{$H+}



interface

uses
  Sockets,  UltraWorkers, UltraSockets, UltraApp, xon;

type

  TUltraServer=class
    private
        FTID: TThreadID;
        FTCount: Cardinal;
        FMustStop: Boolean;

        FServerPort: Integer;

        FMaxThreads: Integer;
        FMaxPersistentThreads: Integer;

        FWorkers: PWorker;
        FCurrentWorker: Integer;

        FConfig: XVar; // configuration container

        // sortcuts to config sections
        FApplications: XVar;
        FAPIKeys: XVar;

        function DispatchRequest(NewSocket: TSocket):boolean;
      public
        Constructor Create;
        function Configure(const NewConfig: XVar):Boolean;
        destructor Destroy;override;

        procedure Start;
        procedure Stop;
        function Running:boolean;

        function FindApp(const AName: String; AVersion: Integer): TUltraApp;
        function InstallApp(const AName: String;AnApp: TUltraApp):boolean;

        property ThreadID: TThreadID read FTID;
        property ThreadsCount: Cardinal read FTCount;
        function IncThreads: Cardinal;
        function DecThreads: Cardinal;
  end;

var

 // Default Config Vars

    ListenPort: Word = 8080;

    MaxThreads: Integer = 8;

    MaxPersistentThreads: Integer = 4;




implementation


uses sysutils,xtypes,UltraHTTP;


 // Backend

 constructor TUltraServer.Create;
 begin
   FTID:=0;
   FMaxThreads:=MaxThreads;
   FMaxPersistentThreads:=MaxPersistentThreads;
   FMustStop:=False;
   FTCount:=0;
   FServerPort:=0;
   FConfig:=XVar.NewList(8);
   FApplications:=FConfig.AddList('Applications',8);
   FAPIKeys:=FConfig.AddList('API-Keys',8);
   FWorkers:=nil;
 end;

 function TUltraServer.Configure(const NewConfig: XVar):Boolean;
 begin
  Result:=False;
  if Running then exit; // cannot configure a running server

  // try to read configuration

  FServerPort:=NewConfig['net']['port'].AsInteger;
  if FServerPort=0 then FServerPort:=ListenPort;

  result:=true
 end;

 destructor TUltraServer.Destroy;
 begin
  Stop;
  FConfig.Free;
  inherited destroy;
 end;

 function TUltraServer.Running:boolean;
 begin
  Result:=FTID<>0;
 end;

function ServerLoop(Server: Pointer): Integer;
var ServerSock: TApiSocket;
    NewSocket: TSocket;
    var i: PtrUInt;
begin
 with TUltraServer(Server) do
  begin
  ServerSock.Init(fpSocket(AF_INET, SOCK_STREAM, 0));
  ServerSock.Bind(FServerPort, 100);
  if ServerSock.Error = 0 then
  while not FMustStop do
    begin
     NewSocket := ServerSock.Accept(1000);
     if (not FMustStop) and (NewSocket <> 0) then DispatchRequest(NewSocket);
    end;

 ServerSock.Close;

 if FWorkers<>nil then
  begin
   for i:=0 to FMaxThreads-1 do FWorkers[i].Stop;
   while ThreadsCount>0 do {Sleep(100)};
   FTID:=00;
   FreeMem(FWorkers);
   FWorkers:=nil;
  end;
 Result:=0;
 end
end;

 procedure TUltraServer.Start;
 var i,sz: Integer;
 begin
   if Running then exit;
   sz:=FMaxThreads*SizeOf(TWorker);
   GetMem(FWorkers,sz);
   FillByte(FWorkers^,Sz,0);
   for i:=0 to FMaxPersistentThreads-1 do
     with PWorker(FWorkers)[i] do
       begin
         Prepare(Self,True);
         Start;
       end;
   FCurrentWorker:=0;
   FMustStop:=False;
   BeginThread(@ServerLoop,Self,FTID);
 end;

 procedure TUltraServer.Stop;
 begin
  if Not Running then exit;
  FMustStop:=True;
  // WaitForThreadTerminate(FTID,1000);
  while Running do {sleep(100)};
 end;

 function TUltraServer.DispatchRequest(NewSocket: TSocket):boolean;
 var StartWorker: Integer;
     DummySock: TApiSocket;
 begin
   if not Running then exit(false);
   StartWorker:=FCurrentWorker;
    repeat
     with FWorkers[FCurrentWorker]  do
     begin
     if not Initialized then
      begin
        Prepare(Self,False);
        Start;
      end;
     if Enqueue(NewSocket) then
      begin
       //writeln(format('%d Running Threads',[FThreadCount,Id]));
       exit(true);
      end else inc(FCurrentWorker);
      //writeln('nope!');
     if FCurrentWorker>MaxThreads-1 then FCurrentWorker:=0
     end;
    until FCurrentWorker=StartWorker;
    DummySock.Init(NewSocket);
    if  DummySock.CanRead(UltraTimeOut) then DummySock.Purge;
    DummySock.Send(Format('HTTP/1.1 %d %s'#13#10,[HTTP_BadGateway,HTTPStatusPhrase(HTTP_BadGateway)]));
    DummySock.Send('Connection: Closed'#13#10#13#10);
    DummySock.Close;
    //Writeln(format('!!!!!!!!!!!!!!droping request on socket [%d]!',[NewSocket]));
   Result:=false;
 end;

 function TUltraServer.IncThreads: Cardinal; inline;
 begin
  Result:=InterLockedIncrement(FTCount);
 end;

  function TUltraServer.DecThreads: Cardinal; inline;
 begin
  Result:=InterLockedDecrement(FTCount);
 end;

function TUltraServer.FindApp(const AName: String; AVersion: Integer): TUltraApp;
begin
 Result:=(FApplications[AName].AsObject) as TUltraApp;
 if (Result<>nil) and (Result.Version=AVersion) then exit;
 Result:=nil
end;

function TUltraServer.InstallApp(const AName: String;AnApp: TUltraApp):boolean;
begin
 if FindApp(AName,AnApp.Version)<>nil then exit(false);
 FApplications.Add(xtNativeObject,AName).AsObject:=AnApp;
 Result:=True;
end;
end.

