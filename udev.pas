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

{
 Representa la envoltura del archivo c_in/udev.c
}

unit udev;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, CTypes;

type
  {$IFDEF FPC}
  {$PACKRECORDS C}

  PDeviceProperty = ^TDeviceProperty;

  TDeviceProperty = record
  node_path : array[0..40] of Char;
  id_vendor : array[0..40] of Char;
  id_product : array[0..40] of Char;
  product : array[0..40] of Char;
  manufacturer : array[0..40] of Char;
  serial : array[0..40] of Char;
  end;
  {$ENDIF}

  type TListDevicesCallback = procedure; cdecl;

  procedure InitializeUdev; cdecl;
  procedure UnitializeUdev; cdecl;
  function GetMonitorEvent : ctypes.cint32; cdecl;
  function ReceiveEvents : ctypes.cint32; cdecl;
  procedure ListDevices(cb : TListDevicesCallback); cdecl;
  function GetDeviceAttribute(attr : PChar) : PChar; cdecl;
  function GetDeviceNodePath : PChar; cdecl;
  function GetDeviceProperty : PDeviceProperty; cdecl;

implementation

{$linklib c}
{$linklib udev}
{$link udev.o}

procedure InitializeUdev; cdecl; external;
procedure UnitializeUdev; cdecl; external;
procedure ListDevices( cb : TListDevicesCallback); cdecl; external;
function GetMonitorEvent : ctypes.cint32; cdecl; external;
function ReceiveEvents : ctypes.cint32; cdecl; external;
function GetDeviceAttribute(attr : PChar) : PChar; cdecl; external;
function GetDeviceNodePath : PChar; cdecl; external;
function GetDeviceProperty : PDeviceProperty; cdecl; external;
end.

