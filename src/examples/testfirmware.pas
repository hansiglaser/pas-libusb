(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   Test program demonstrating the implementation of a device driver.     *
 *                                                                         *
 *   This is free and unencumbered software released into the public       *
 *   domain.                                                               *
 *                                                                         *
 *   Anyone is free to copy, modify, publish, use, compile, sell, or       *
 *   distribute this software, either in source code form or as a compiled *
 *   binary, for any purpose, commercial or non-commercial, and by any     *
 *   means.                                                                *
 *                                                                         *
 *   In jurisdictions that recognize copyright laws, the author or authors *
 *   of this software dedicate any and all copyright interest in the       *
 *   software to the public domain. We make this dedication for the        *
 *   benefit of the public at large and to the detriment of our heirs and  *
 *   successors. We intend this dedication to be an overt act of           *
 *   relinquishment in perpetuity of all present and future rights to this *
 *   software under copyright law.                                         *
 *                                                                         *
 *   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *
 *   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF    *
 *   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND                 *
 *   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY      *
 *   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,  *
 *   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE     *
 *   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                *
 *                                                                         *
 ***************************************************************************)

(*

This program demonstrates the usage of the pas-libusb OOP wrapper. In the unit
MyDevice a class is derived from TLibUsbDeviceWithFirmware to implement a "driver"
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

