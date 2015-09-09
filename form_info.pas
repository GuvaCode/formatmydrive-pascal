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
    procedure buttonOkClick(Sender: TObject);
    procedure buttonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);


  private
    { private declarations }
  public
        procedure FillInformation(dev : PDeviceProperty);
  end;

var
  FormInfo: TFormInfo;

implementation

{$R *.lfm}

{ TFormInfo }

procedure TFormInfo.buttonSaveClick(Sender: TObject);
begin

end;

procedure TFormInfo.FormCreate(Sender: TObject);
begin

end;

procedure TFormInfo.buttonOkClick(Sender: TObject);
begin

end;

procedure TFormInfo.FillInformation(dev : PDeviceProperty);
begin
  editPath.Text := dev^.node_path;
  editVendor.Text := dev^.id_vendor;
  editProduct.Text := dev^.product;
  editIdProduct.Text := dev^.id_product;
  editManufacturer.Text := dev^.manufacturer;
  editSerial.Text := dev^.serial;
  editVersion.Text := dev^.version;
  editPower.Text := dev^.max_power;
  editBus.Text := dev^.bus;
end;


end.

