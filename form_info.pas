unit form_info;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Udev,Contnrs;

type

  PHashListDevices = ^TFPHashList;
  PListBox         = ^TListBox;

  { TFormInfo }

  TFormInfo = class(TForm)
    btnPrev: TButton;
    btnNext: TButton;
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
    procedure btnNavClick(Sender: TObject);
    procedure buttonSaveClick(Sender: TObject);


  private
      m_device   : PDeviceProperty;
      m_hashList : PHashListDevices;
      m_listbox  : PListBox;

      procedure WriteFile (namefile: string);
      procedure ValidateNavButtons ();
  public
      procedure FillInformation(hashList : PHashListdevices; listBox : PListBox);
  end;


var
  FormInfo: TFormInfo;

implementation

{$R *.lfm}

{ TFormInfo }

procedure TFormInfo.buttonSaveClick(Sender: TObject);
const
   MsgTitle = 'Confirmación';
   MsgBody  = 'El archivo ya existe.' + #13#13 + '¿Desea sobrescribir el archivo?';
var
  msg : Integer;
begin
    if saveDlg.Execute then
    begin
      if FileExists (saveDlg.FileName) then
        begin
            msg := MessageDlg(MsgTitle, MsgBody, mtConfirmation, [mbYes, mbNo], 0);
            if msg = mrNo then
            begin
                 exit;
            end;
        end;
        WriteFile (saveDlg.FileName);
    end;
end;


procedure TFormInfo.btnNavClick(Sender: TObject);
var
  index : Word; //unsigned
begin

    index := m_listbox^.ItemIndex;

    if Sender = btnPrev then
        index := index - 1
    else
        index := index + 1;

    m_listbox^.ItemIndex := index;

    FillInformation (nil, nil);

end;


{sobreescribe el archivo}
procedure TFormInfo.WriteFile (namefile: string);
var
  tfile : TextFile;

begin
    AssignFile (tfile, namefile);
    rewrite(tfile);

    with m_device^ do
       begin
          writeln (tfile, 'Node       : ' + node_path);
          writeln (tfile, 'Vendor     : ' + id_vendor);
          writeln (tfile, 'Product    : ' + product);
          writeln (tfile, 'ID Product : ' + id_product);
          writeln (tfile, 'Manufacturer: ' + manufacturer);
          writeln (tfile, 'Serial     : ' + serial);
          writeln (tfile, 'Version    : ' + version);
          writeln (tfile, 'Power max. : ' + max_power);
          writeln (tfile, 'Bus        : ' + bus);
       end;
    CloseFile (tfile);
end;


procedure TFormINfo.ValidateNavButtons ();
begin
    if m_listbox^.Count = 1 then
    begin
       btnNext.Enabled := false;
       btnPrev.Enabled := false;
       exit;
    end;

     if m_listbox^.ItemIndex < m_listbox^.Count -1 then
        btnNext.Enabled := true
     else
        btnNext.Enabled := false;

     if (m_listbox^.ItemIndex > Int(0)) then
        btnPrev.Enabled := true
     else
        btnPrev.Enabled := false;
end;



procedure TFormInfo.FillInformation(hashList : PHashListdevices; listBox : PListBox);
var
  dev : PDeviceProperty;
begin
      if m_listbox = nil then m_listbox := listBox;
      if m_hashList = nil then m_hashList := hashList;

      ValidateNavButtons ();

  { supone que el indice del hash coincide al mostrado en el editbox }
  dev := m_hashList^.Find (m_hashList^.NameOfIndex (m_listbox^.ItemIndex));

  if dev = nil then exit;


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

  m_device := dev;
end;


end.

