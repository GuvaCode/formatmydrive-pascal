unit About;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    btnClose: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    procedure btnCloseClick(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  aboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }



procedure TAboutForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.

