program FormatMyDrive;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainProgram, About, udev, form_info
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='fmtmydrv';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TaboutForm, aboutForm);
  Application.Run;
end.

