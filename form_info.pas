unit form_info;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Udev;

type

  { TFormInfo }

  TFormInfo = class(TForm)
    buttonSave: TButton;
    buttonOk: TButton;
    editPath: TEdit;
    editBus: TEdit;
    editVendor: TEdit;
    editProduct: TEdit;
    editIdProduct: TEdit;
    editManufacturer: TEdit;
    editSerial: TEdit;
    editVersion: TEdit;
    editPower: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    saveDlg: TSaveDialog;
    procedure buttonSaveClick(Sender: TObject);


  private
      device: PDeviceProperty;
      procedure WriteFile (namefile: string);
  public
      procedure FillInformation(dev : PDeviceProperty);
  end;

var
  FormInfo: TFormInfo;

implementation

{$R *.lfm}

{ TFormInfo }

procedure TFormInfo.buttonSaveClick(Sender: TObject);
const
   MSG_FORMAT_TITLE = 'Confirmación';
   MSG_FORMAT_BODY  = 'El archivo ya existe.' + #13#13 + '¿Decea sobrescribir el archivo?';
var
  msg : Integer;
begin
    if saveDlg.Execute then
    begin
      if FileExists (saveDlg.FileName) then
        begin
            msg := MessageDlg(MSG_FORMAT_TITLE, MSG_FORMAT_BODY, mtConfirmation, [mbYes, mbNo], 0);
            if msg = mrNo then
            begin
                 exit;
            end;
        end;
        WriteFile (saveDlg.FileName);
    end;
end;


{sobreescribe el archivo}
procedure TFormInfo.WriteFile (namefile: string);
var
  tfile : TextFile;

begin
    AssignFile (tfile, namefile);
    rewrite(tfile);

    with device^ do
       begin
          writeln (tfile, node_path);
          writeln (tfile, id_vendor);
          writeln (tfile, product);
          writeln (tfile, id_product);
          writeln (tfile, manufacturer);
          writeln (tfile, serial);
          writeln (tfile, version);
          writeln (tfile, max_power);
          writeln (tfile, bus);
       end;
    CloseFile (tfile);
end;



procedure TFormInfo.FillInformation(dev : PDeviceProperty);
begin
  with dev^ do
     begin
        editPath.Text := node_path;
        editVendor.Text := id_vendor;
        editProduct.Text := product;
        editIdProduct.Text := id_product;
        editManufacturer.Text := manufacturer;
        editSerial.Text := serial;
        editVersion.Text := version;
        editPower.Text := max_power;
        editBus.Text := bus;
     end;

  device := dev;
end;


end.

