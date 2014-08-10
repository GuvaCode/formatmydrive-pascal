{
   Copyright (c) 2014, Daniel T. Borelli

   This file is part of FormatMyDrive.

    FormatMyDrive is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    FormatMyDrive is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with FormatMyDrive.  If not, see <http://www.gnu.org/licenses/>.
}

unit MainProgram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Buttons, Menus, About, clocale, udev, LCLIntf,
  InterfaceBase, LCLType, CTypes;

  type


  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    btnStart: TButton;
    btnClose: TButton;
    cboxFormat: TComboBox;
    cboxPartition: TComboBox;
    chkBoxBoot: TCheckBox;
    chkBoxFastFormat: TCheckBox;
    chkGrpOptions: TCheckGroup;
    edtLabel: TEdit;
    Image1: TImage;
    lstBoxDevices: TListBox;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    menuAbout: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    menuQuit: TMenuItem;
    progressBar: TProgressBar;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    staticVersion: TStaticText;
    procedure btnCloseClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure menuAboutClick(Sender: TObject);
    procedure menuQuitClick(Sender: TObject);
    procedure askFormatContinue();
    procedure UdevEvent(AData: PtrInt; AFlags: dword);
    procedure AddItemToDevicesList;
    procedure RemoveItemFromDevicesList;

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;
  UdevEventEventHandler: PEventHandler;
  StrListDevices : TStringList;
  DeviceProperty : PDeviceProperty;


implementation

{$R *.lfm}

{ TMainForm }


procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  UnitializeUdev;
  Close;
end;

procedure TMainForm.btnStartClick(Sender: TObject);
begin
  askFormatContinue;
end;


{
Pointer function callback.
External Call.
}
procedure ListDevicesCallback(); cdecl;
begin
     DeviceProperty := GetDeviceProperty;
     MainForm.AddItemToDevicesList;
end;


procedure TMainForm.AddItemToDevicesList;
var
  attr : string;
begin
     attr := DeviceProperty^.manufacturer + ' ' + DeviceProperty^.product;
     StrListDevices.Add(attr);
     lstBoxDevices.Items.Clear;
     lstBoxDevices.Items.Assign( StrListDevices );
end;


procedure TMainForm.RemoveItemFromDevicesList;
var
  index_remove : Integer;
  attr : string;
begin
     attr := DeviceProperty^.manufacturer + ' ' + DeviceProperty^.product;
     index_remove := StrListDevices.IndexOf( attr );
     if index_remove <> -1 then
     begin
       StrListDevices.Delete(index_remove);
       lstBoxDevices.Items.Clear;
       lstBoxDevices.Items.Assign( StrListDevices);
     end;
end;


{
 Event Handle Callback
}
procedure TMainForm.UdevEvent(AData: PtrInt; AFlags: dword);
var
    ret : ctypes.cint32;
begin

  ret := ReceiveEvents;

  if (ret = 1) then { add }
     begin
       DeviceProperty := GetDeviceProperty;
       AddItemToDevicesList;
     end
  else if(ret = 2) then { remove }
     begin
       DeviceProperty := GetDeviceProperty;
       RemoveItemFromDevicesList;
     end
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  StrListDevices := TStringList.Create;
  InitializeUdev;
  ListDevices( @ListDevicesCallback );
  UdevEventEventHandler := AddEventHandler( GetMonitorEvent, 3, @UdevEvent,0 );
end;


procedure TMainForm.menuAboutClick(Sender: TObject);
begin
  aboutForm.ShowModal;
  end;


procedure TMainForm.menuQuitClick(Sender: TObject);
begin
  RemoveEventhandler(UdevEventEventHandler);
  StrListDevices.Free;
  Close;
end;


procedure TMainForm.askFormatContinue();
const
 MSG_FORMAT_TITLE = 'Confirmación';
 MSG_FORMAT_BODY  = '¿Desea comenzar con el formato?' + #13#13 + 'Perderá los datos del dispositivo.';
begin
 MessageDlg(MSG_FORMAT_TITLE, MSG_FORMAT_BODY, mtConfirmation, [mbYes, mbNo], 0) { = mrYes then }
end;


end.

