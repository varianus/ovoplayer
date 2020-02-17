unit catch_signal;

{$mode objfpc}{$H+}

interface
uses
 Forms, Classes, SysUtils;

Procedure Init_SignalHandler;

implementation
uses BaseUnix;

Procedure DoShutDown(Sig : Longint; Info : PSigInfo; Context : PSigContext); cdecl;

begin
   Application.Terminate;
end;

Procedure Init_SignalHandler;

Var
   old,new : SigactionRec;

begin
   New.sa_handler:=@DoShutDown;
   fpSigaction(SIGQUIT,@New,@Old);
   fpSigaction(SIGTERM,@New,@Old);
   fpSigaction(SIGINT,@New,@Old);
end;

end.

