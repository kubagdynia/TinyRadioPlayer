program TinyRadioPlayer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  cmem, // the c memory manager is on some systems much faster for multi-threading
  {$ENDIF}{$ENDIF}
  {$IFDEF DEBUG}
  // You can not use the -gh switch with the cmem unit.
  // The -gh switch uses the heaptrc unit, which extends the heap manager.
  // Therefore the heaptrc unit must be used after the cmem unit.
  heaptrc,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainFormUnit, RadioPlayerThread, Consts, RadioPlayer, Helpers, RadioPlayerTypes
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Tiny Radio Player';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

