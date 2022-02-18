program guiexample;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  sysutils,
  Interfaces, // this includes the LCL widgetset
  Forms, guimain;

{$R *.res}

{$IFDEF DEBUG}
const cHeapTraceFile = 'heap.trc';
{$ENDIF}

begin
  {$IFDEF DEBUG}
  if FileExists(cHeapTraceFile) then
    DeleteFile(cHeapTraceFile);
  SetHeapTraceOutput(cHeapTraceFile);
  {$ENDIF}
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

