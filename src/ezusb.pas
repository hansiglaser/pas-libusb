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

{$MODE OBJFPC}
Unit EZUSB;

Interface
Uses LibUSB,USB,IntelHex;

Type

  { TUSBDeviceEZUSB }

  TUSBDeviceEZUSB = class(TUSBDevice)
  public
    Constructor Create(ADev:PUSBDevice);
    Constructor Create(AidVendor,AidProduct:Word);
    Function ReadMem (Pos:LongInt;Out   Data;Length:LongInt):LongInt;
    Function WriteMem(Pos:LongInt;Const Data;Length:LongInt):LongInt;
    Function ResetCPU(ResetBit:Byte) : LongInt;
    Function LoadMem(HexRecord:PIntelHexRecord):LongInt;
    Procedure DownloadFirmware(AFirmware:String;StartImmediately:Boolean=true);
  End;

Const ANCHOR_LOAD_INTERNAL = $A0;     { Vendor specific request code for Anchor Upload/Download. This one is implemented in the core. }
      CPUCS_REG            = $7F92;   { EZ-USB Control and Status Register }
      CPUCS_8051RESET      = $01;     { 1: reset 8051, 0: run }

Implementation
Uses SysUtils,Errors;

Const
  EZUSBUnconfiguredConfiguration = 1;

{ TUSBDeviceEZUSB }

Constructor TUSBDeviceEZUSB.Create(ADev:PUSBDevice);
Begin
  inherited Create(ADev,EZUSBUnconfiguredConfiguration);
End;

Constructor TUSBDeviceEZUSB.Create(AidVendor, AidProduct : Word);
Begin
  inherited Create(AidVendor,AidProduct,EZUSBUnconfiguredConfiguration);
End;

(**
 * Read "Length" bytes to "Data" at memory position "Pos"
 *
 * Errors: opening the file: -errno
 *         allocating memory: returns -ENOMEM
 *
 * returns: >0 .... count of bytes written
 *          <0 .... error number
 *)
Function TUSBDeviceEZUSB.ReadMem(Pos:LongInt;Out Data;Length:LongInt) : LongInt;
Begin
  Result := FControl.ControlMsg(USB_ENDPOINT_IN or USB_RECIP_DEVICE or USB_TYPE_VENDOR,ANCHOR_LOAD_INTERNAL,Pos,0,Data,Length,300);
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
Function TUSBDeviceEZUSB.WriteMem(Pos:LongInt;Const Data;Length:LongInt) : LongInt;
Begin
  Result := FControl.ControlMsg(USB_ENDPOINT_OUT or USB_RECIP_DEVICE or USB_TYPE_VENDOR,ANCHOR_LOAD_INTERNAL,Pos,0,Data,Length,300);
End;

Function TUSBDeviceEZUSB.ResetCPU(ResetBit:Byte) : LongInt;
Begin
  Result := WriteMem(CPUCS_REG,ResetBit,1);
End;

Function TUSBDeviceEZUSB.LoadMem(HexRecord:PIntelHexRecord):LongInt;
Begin
  While (HexRecord <> Nil) and (HexRecord^.TheType = 0) do
    Begin
      Result := WriteMem(HexRecord^.Address,HexRecord^.Data,HexRecord^.Length);
      if Result < 0 then
        raise EInOutError.CreateFmt('TUSBDeviceEZUSB.WriteMem failed (%d %04X %p %d) with error %d: %s\n',[Result,HexRecord^.Address,@HexRecord^.Data,HexRecord^.Length,usb_error_errno,StrError(usb_error_errno)]);
      HexRecord := HexRecord^.Next;
    End;
End;

Procedure TUSBDeviceEZUSB.DownloadFirmware(AFirmware:String;StartImmediately:Boolean);
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

