(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   Cypress/Anchor Chip EZ-USB AN2131 firmware download handler.          *
 *                                                                         *
 *   This Pascal unit is free software; you can redistribute it and/or     *
 *   modify it under the terms of a modified GNU Lesser General Public     *
 *   License (see the file COPYING.modifiedLGPL.txt).                      *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the          *
 *   GNU Lesser General Public License for more details.                   *
 *                                                                         *
 ***************************************************************************)

{$MODE OBJFPC}
Unit EZUSB;

Interface
Uses LibUsb,LibUsbOop,IntelHex;

Type

  { TLibUsbDeviceEZUSB }

  TLibUsbDeviceEZUSB = class(TLibUsbDevice)
  public
    Function ReadMem (Pos:LongInt;Out   Data;Length:LongInt):LongInt;
    Function WriteMem(Pos:LongInt;Var   Data;Length:LongInt):LongInt;
    Function ResetCPU(ResetBit:Byte) : LongInt;
    Function LoadMem(HexRecord:PIntelHexRecord):LongInt;
    Procedure DownloadFirmware(AFirmware:String;StartImmediately:Boolean=true);
  End;

Const ANCHOR_USB_CONFIG    = 1;       { bConfigurationValue }
      ANCHOR_LOAD_INTERNAL = $A0;     { Vendor specific request code for Anchor Upload/Download. This one is implemented in the core. }
      CPUCS_REG            = $7F92;   { EZ-USB Control and Status Register }
      CPUCS_8051RESET      = $01;     { 1: reset 8051, 0: run }

Implementation
Uses SysUtils;

{ TLibUsbDeviceEZUSB }

(**
 * Read "Length" bytes to "Data" at memory position "Pos"
 *
 * Errors: opening the file: -errno
 *         allocating memory: returns -ENOMEM
 *
 * returns: >0 .... count of bytes written
 *          <0 .... error number
 *)
Function TLibUsbDeviceEZUSB.ReadMem(Pos:LongInt;Out Data;Length:LongInt) : LongInt;
Begin
  Result := FControl.ControlMsg(
    { bmRequestType } LIBUSB_ENDPOINT_IN or LIBUSB_REQUEST_TYPE_VENDOR or LIBUSB_RECIPIENT_DEVICE,
    { bRequest      } ANCHOR_LOAD_INTERNAL,
    { wValue        } Pos,
    { wIndex        } 0,
    { Buf           } Data,
    { wLength       } Length,
    { Timeout       } 300);
End;

(**
 * Write "Length" bytes from "Data" at memory position "Pos"
 *
 * Errors: opening the file: -errno
 *         allocating memory: returns -ENOMEM
 *
 * returns: >0 .... count of bytes written
 *          <0 .... error number
 *)
Function TLibUsbDeviceEZUSB.WriteMem(Pos:LongInt;Var Data;Length:LongInt) : LongInt;
Begin
  Result := FControl.ControlMsg(
    { bmRequestType } LIBUSB_ENDPOINT_Out or LIBUSB_REQUEST_TYPE_VENDOR or LIBUSB_RECIPIENT_DEVICE,
    { bRequest      } ANCHOR_LOAD_INTERNAL,
    { wValue        } Pos,
    { wIndex        } 0,
    { Buf           } Data,
    { wLength       } Length,
    { Timeout       } 300);
End;

Function TLibUsbDeviceEZUSB.ResetCPU(ResetBit:Byte) : LongInt;
Begin
  Result := WriteMem(CPUCS_REG,ResetBit,1);
End;

Function TLibUsbDeviceEZUSB.LoadMem(HexRecord:PIntelHexRecord):LongInt;
Begin
  While (HexRecord <> Nil) and (HexRecord^.TheType = 0) do
    Begin
      Result := WriteMem(HexRecord^.Address,HexRecord^.Data,HexRecord^.Length);
      if Result < 0 then
        raise ELibUsb.CreateFmt(Result,'TLibUsbDeviceEZUSB.WriteMem failed (%d %04X %p %d)\n',[Result,HexRecord^.Address,@HexRecord^.Data,HexRecord^.Length]);
      HexRecord := HexRecord^.Next;
    End;
End;

Procedure TLibUsbDeviceEZUSB.DownloadFirmware(AFirmware:String;StartImmediately:Boolean);
Var Firmware : PIntelHexRecord;
Begin
  { read firmware: the only file format we currently support is Intel HEX }
  Firmware := ReadHexFile(AFirmware); { might raise an exception if the ihx file is currupt }
  if Firmware = Nil then
    raise Exception.Create('No firmware found');
  try
    if ResetCPU(CPUCS_8051RESET) < 0 then
      raise Exception.Create('Error resetting the device');
    LoadMem(Firmware); { might raise an exception on usb errors }
    if StartImmediately then
      if ResetCPU(0) < 0 then
        raise Exception.Create('Error releasing the device from reset');
  finally
    FreeIntelHex(Firmware);
  End;
End;

End.

