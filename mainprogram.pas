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

{$rangechecks on} {debug}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Buttons, Menus, About, clocale, udev, LCLIntf,
  InterfaceBase, LCLType, CTypes, Contnrs, form_info;

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
    menuVer: TMenuItem;
    menuVerInfo: TMenuItem;
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
    procedure lstBoxDevicesDblClick(Sender: TObject);
    procedure menuAboutClick(Sender: TObject);
    procedure menuVerClick(Sender: TObject);
    procedure menuQuitClick(Sender: TObject);
    procedure askFormatContinue();
    procedure menuVerInfoClick(Sender: TObject);
    procedure UdevEvent(AData: PtrInt; AFlags: dword);
    procedure AddItemToDevicesList;
    procedure RemoveItemFromDevicesList;
    procedure FreeAllHash(A : pointer; B : pointer);
    procedure FillListDevices(A : pointer; B : pointer);
    procedure NewListItem ();
    procedure Quit ();


  private
    { private declarations }
    m_device : PDeviceProperty;
    m_hash_devices : TFPHashList;
    UdevEventEventHandler: PEventHandler;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;


implementation

{$R *.lfm}

{ TMainForm }


procedure TMainForm.btnCloseClick (Sender: TObject);
begin
     Quit;
end;

procedure TMainForm.Quit;
begin
  RemoveEventhandler (UdevEventEventHandler);
  UnitializeUdev ();
  m_hash_devices.ForEachCall (@FreeAllHash, nil);
  m_hash_devices.Destroy ();
  Close;
end;

procedure TMainForm.btnStartClick (Sender: TObject);
begin
  askFormatContinue ();
end;

procedure TMainForm.FreeAllHash (A : pointer; B : pointer);
begin
   Dispose (PDeviceProperty (A));
end;

procedure TMainForm.FillListDevices (A : pointer; B : pointer);
begin
   m_device := PDeviceProperty (A);
   NewListItem ();
end;


{
Pointer function callback.
External Call.
}
procedure ListDevicesCallback (); cdecl;
begin
     MainForm.m_device := GetDeviceProperty ();
     MainForm.AddItemToDevicesList ();
end;


procedure TMainForm.NewListItem ();
var
   attr : string;
begin
     attr := m_device^.manufacturer + ' ' + m_device^.product;
     lstBoxDevices.Items.Add (attr);
     lstBoxDevices.ItemIndex := lstBoxDevices.Count - 1;
end;

procedure TMainForm.AddItemToDevicesList ();
var
   dev_copy : PDeviceProperty;
begin
   NewListItem ();
   New (dev_copy);
   Move (m_device^, dev_copy^, sizeof (TDeviceProperty));
   m_hash_devices.add (dev_copy^.node_path, dev_copy);
   if FormInfo.IsVisible () then FormInfo.FillInformation (@m_hash_devices, @lstBoxDevices);
end;


procedure TMainForm.RemoveItemFromDevicesList ();
var
   dev : PDeviceProperty;
begin
   dev := m_hash_devices.Find (m_device^.node_path);
   if dev <> nil then
   begin
     m_hash_devices.Delete (m_hash_devices.IndexOf (dev));
     Dispose (dev);
   end;

   lstBoxDevices.Items.Clear ();
   m_hash_devices.ForEachCall (@FillListDevices, nil);
   if FormInfo.IsVisible () then FormInfo.FillInformation (@m_hash_devices, @lstBoxDevices);
end;


{
 Event Handle Callback
}
procedure TMainForm.UdevEvent (AData: PtrInt; AFlags: dword);
const
   ADD = 1;      //ref. udev.c
   REMOVE = 2;   //ref. udev.c

var
   ret : ctypes.cint32;

begin
  ret := ReceiveEvents;

  if ret = ADD then
  begin
    m_device := GetDeviceProperty ();
    AddItemToDevicesList ();
  end
  else if ret = REMOVE then
  begin
    m_device := GetDeviceProperty ();
    RemoveItemFromDevicesList ();
  end
end;


procedure TMainForm.FormCreate (Sender: TObject);
begin
  FormInfo := TFormInfo.Create (Application);  // creado explícitamente.
  m_hash_devices := TFPHashList.Create ();
  InitializeUdev ();
  ListDevices (@ListDevicesCallback);
  UdevEventEventHandler := AddEventHandler (GetMonitorEvent, 3, @UdevEvent,0);
end;

procedure TMainForm.lstBoxDevicesDblClick(Sender: TObject);
begin
  FormInfo.FillInformation(@m_hash_devices, @lstBoxDevices);
  FormInfo.ShowModal;
end;


procedure TMainForm.menuAboutClick (Sender: TObject);
begin
  aboutForm.ShowModal ();
end;

procedure TMainForm.menuVerClick (Sender: TObject);
begin
  if lstBoxDevices.SelCount  > 0 then
     menuVerInfo.Enabled := True;
end;


procedure TMainForm.menuQuitClick (Sender: TObject);
begin
  Quit ();
end;


procedure TMainForm.askFormatContinue ();
const
 MSG_FORMAT_TITLE = 'Confirmación';
 MSG_FORMAT_BODY  = '¿Desea comenzar con el formato?' + #13#13 + 'Perderá los datos del dispositivo.';
begin
 MessageDlg (MSG_FORMAT_TITLE, MSG_FORMAT_BODY, mtConfirmation, [mbYes, mbNo], 0) { = mrYes then }
end;

procedure TMainForm.menuVerInfoClick (Sender: TObject);
begin
    lstBoxDevicesDblClick (nil);
end;


end.

