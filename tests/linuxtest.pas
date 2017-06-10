program linuxtest;


uses cmem, cthreads, sysutils,xon, UltraBuffers,UltraContext,UltraApp,UltraBackend;


type


   TMyApp=class (TUltraApp)
     public
       procedure HandleRequest(var AContext: TUltraContext);override;
   end;
                          
   MyHandler=class (TCustomUltraHandler)
    public
     procedure  Execute;override;
    end;
                                                        
                                                                                             
   var MyApp: TMyApp;
   
   var  RespStr:string='HTTP/1.1 200 OK'#13#10#13#10;
                   
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
    begin
    
     Sleep(10000); 
     Context^.Socket.Send(RespStr);
     Context^.Socket.Shutdown(2);
     Context^.Handled;
{     E:=GetTickCount64-Context^.StartTime;
     writeln(format('Processed request in thread %d for %d ticks-->"%s"',[GetCurrentThreadId,E,Copy(Pchar(Context^.Buffer^.DataPtr(0)),0,Context^.Buffer^.Len)]));
     writeln(format('Method:%d',[Context^.Request.Method]));
     
     with Context^.Request.Path do for i:=0 to Count-1 do writeln(format('Path Segments: "%s"',[Vars[i].AsString]));
     writeln(format('Version:%d',[Context^.Request.Protocol]));
     with Context^.Request.Headers do for i:=0 to Count-1 do writeln(format('Header: "%s:%s"',[Keys[i].AsString,Vars[i].AsString]));
}
 end;                                   

begin
 writeln('starting...');
 MyApp:=TMyApp.Create('myapp','ABCDE123456');
 UltraAddApp(MyApp); 
 UltraStart('');
 readln;
 UltraStop;
end.
