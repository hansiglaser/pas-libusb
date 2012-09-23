(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************)

(*

This program demonstrates the usage of the pas-libusb OOP wrapper. In the unit
MyDevice a class is derived from TUSBDeviceWithFirmware to implement a "driver"
for a device using the firmware as provided by the ezusb-firmware project at
https://github.com/hansiglaser/ezusb-firmware . It implements three features
 - identify an USB device as supported
 - find the firmware file
 - implements device functions

The firmware is searched in multiple paths:
  .
  ~/.testfirmware
  path of the executable
  $PATH
  /etc/testfirmware

To get this example working, you should download and compile the EZ-USB
firmware project noted above and put a symbol link to its firmware.ihx file
to the directory of this test program.

*)

Program TestFirmware;

{$mode objfpc}{$H+}

Uses Classes,SysUtils,MyDevice,LibUsbOop,LibUsbUtil;

Var Context   : TLibUsbContext;
    TheDevice : TMyDevice;

Begin
  // create context
  Context := TLibUsbContext.Create;
  try
    // connect to the device
    TheDevice := TMyDevice.Create(
      Context,
      TLibUsbDeviceMatchVidPid.Create(Context,MyDevice.USBVendEmpty,MyDevice.USBProdEmpty),
      TMyDevice.FindFirmware(MyDevice.FirmwareName,'testfirmware'),
      TLibUsbDeviceMatchVidPid.Create(Context,MyDevice.USBVendConf,MyDevice.USBProdConf));
  except
    on E : Exception do
      Begin
        WriteLn('Couldn''t connect to device: ',E.Message);
        Halt(1);
      End;
  End;
  WriteLn('Successfully connected to USB device ',IntToHex(MyDevice.USBVendConf,4),':',IntToHex(MyDevice.USBProdConf,4));
  WriteLn('Version:        ',TheDevice.GetVersion);
  WriteLn('Version String: ',TheDevice.GetVersionString);
  WriteLn('Status:         ',TheDevice.GetStatus);

  TheDevice.Free;
  Context.Free;
End.

