unit UltraWorkers;

{$mode objfpc}{$H+}
{$modeswitch AdvancedRecords}

interface

uses UltraSockets,UltraBuffers,UltraContext;

type
  PWorker = ^TWorker;
  TWorker=record
             private
              FServer: Pointer; // a not null values indicates an initialized and assigned to server worker
              FId: TThreadID;
              FEvent: PRTLEvent;
              FKeepRunning: Boolean;
              FPersistent: Boolean;
              FNextSocket: TSocket;
              FReturnCode: Integer; //is this needed?
              procedure Cleanup;
              function GetRunning: Boolean;
              function GetWillStop: Boolean;
              function GetInitialized: boolean;
              procedure Dequeue;
             public
              procedure Prepare(AServer: Pointer; isPersistent:Boolean=false);
              procedure Start;
              function Stop:boolean;
              function Enqueue(ASocket: TSocket): boolean;
              property WillStop: Boolean read GetWillStop;
              property Running: Boolean read GetRunning;
              property Initialized: boolean read GetInitialized;
              property Id: TThreadID read FId;
   end;

implementation

uses SysUtils,UltraHTTP,UltraParser,UltraApp,UltraBackend;

// Worker Threads
 procedure TWorker.Prepare(AServer:Pointer; isPersistent:Boolean=false);
 begin
   if FServer<>nil then exit;
   FKeepRunning:=true;
   FId:=0;
   FServer:=AServer;
   FEvent:=RTLEventCreate;
   FPersistent:=isPersistent;
   FNextSocket:=INVALID_SOCKET;
   FReturnCode:=0;
 end;

 procedure TWorker.Cleanup;
 begin
   if FServer=nil then exit;
   FId:=0;
   FServer:=nil;
   FKeepRunning:=false;
   RTLeventdestroy(FEvent);
   FReturnCode:=0;
   FNextSocket:=INVALID_SOCKET;
 end;

 function TWorker.GetInitialized:boolean;inline;
 begin
   Result:= FServer<>nil;
 end;

 function TWorker.GetRunning:boolean;inline;
 begin
   Result:= Initialized and (FId<>0);
 end;

 function TWorker.GetWillStop:boolean;inline;
 begin
   Result:= (FServer=nil) or (not FKeepRunning);
 end;


 function TWorker.Stop:boolean;
 begin
    if FServer=nil then exit(false);
    FKeepRunning:=False;
    RTLeventSetEvent(FEvent); // wakeup if sleeping!
    Result:=True;
 end;

 function TWorker.Enqueue(ASocket: TSocket):boolean;
 begin
   // push only if new requests are accepted and no other is pending
   if WillStop  or
      (InterlockedCompareExchange(FNextSocket,ASocket,INVALID_SOCKET)<>INVALID_SOCKET) then exit(false);
   //Writeln(format('Post  socket %d! to thread %d',[ASocket,Id]));
   RTLeventSetEvent(FEvent); // wake up and go to work!
   Result:=True;
 end;

procedure TWorker.Dequeue;
var
    App: TUltraApp;
    Context: TUltraContext;
begin
 Context.Prepare(FNextSocket,@FKeepRunning);
 App:=nil;
 try
  Context.Response.StatusCode:=ParseHTTPRequest(Context.Buffer^,Context.Request);
  if Context.Response.StatusCode=HTTP_ERROR_NONE then
   begin
     App:=TUltraServer(FServer).FindApp(Context.Request.Path[1].AsString,Context.Request.APIVersion);
     if (App<>nil) and App.ValidateKey(Context.Request.Headers[HEADER_API_KEY].AsString)
       then App.HandleRequest(Context)
       else Context.Response.StatusCode:=HTTP_NotFound;
    end
 finally
  if not Context.Handled then Context.SendErrorResponse;
  Context.Cleanup;
  FNextSocket:=INVALID_SOCKET;
  end
end;

 function WorkerMainLoop(Instance: Pointer): Integer;
 begin
  Result:=0;
  with TWorker(Instance^) do try
   //Writeln(format('Starting thread %d',[FId]));
   TUltraBuffer.InitializeBuffers;
   TUltraServer(FServer).IncThreads;
   while FKeepRunning do
    begin
     //at this point we have notning to do and are going to sleep till next request comes... zzzz
     // if the event was set to mark a new request (NextSocket<>INVALID_SOCKET) the wait will return asap

     if FPersistent then RTLeventWaitFor(FEvent) // persistent thread sleeps forever
                    else RTLeventWaitFor(FEvent,1000); // other waits only 1 sec between requests

     // good morning! okay we got the event ... reset it for the future loops
     RTLeventResetEvent(FEvent);

     //  is new request pending?
     if FNextSocket<>INVALID_SOCKET then Dequeue  // process it
      else if not FPersistent then break; // nope... we just woke up by timeout or by gracefull stop
    end
    // here even persisten threads are exiting the loop

  finally
   //writeln(format('thread %d is stopping.',[FId]));
   //at this point we are exiting the threadfunc
   Result:=FReturnCode;
   TUltraServer(FServer).DecThreads;
   Cleanup;
   TUltraBuffer.FinalizeBuffers;
  end
 end;

 procedure TWorker.Start;
 begin
   if FServer<>nil then BeginThread(@WorkerMainLoop,@Self,FId);
 end;

end.

