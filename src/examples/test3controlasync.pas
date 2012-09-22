(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   Test program demonstrating async I/O.                                 *
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
Program Test3ControlAsync;

{$mode objfpc}{$H+}

Uses SysUtils,LibUsb,LibUsbOop;

Const
  DevVID = $0547;
  DevPID = $2131;
//  DevPID = $CFAA;

Procedure PrintDeviceDescriptor(DevDescr:Plibusb_device_descriptor);
Begin
  With DevDescr^ do
    Begin
      WriteLn('Device Descriptor:');
      WriteLn('  bLength               ',bLength:3);
      WriteLn('  bDescriptorType       ',bDescriptorType:3);
      WriteLn('  bcdUSB              ',IntToHex(bcdUSB shr 8,1):2,'.',IntToHex(bcdUSB and $FF,2));
      WriteLn('  bDeviceClass          ',bDeviceClass:3);
      WriteLn('  bDeviceSubClass       ',bDeviceSubClass:3);
      WriteLn('  bDeviceProtocol       ',bDeviceProtocol:3);
      WriteLn('  bMaxPacketSize0       ',bMaxPacketSize0:3);
      WriteLn('  idVendor            $',IntToHex(idVendor, 4));
      WriteLn('  idProduct           $',IntToHex(idProduct,4));
      WriteLn('  bcdDevice           ',IntToHex(bcdDevice shr 8,1):2,'.',IntToHex(bcdDevice and $FF,2));
      WriteLn('  iManufacturer         ',iManufacturer:3);
      WriteLn('  iProduct              ',iProduct:3);
      WriteLn('  iSerialNumber         ',iSerialNumber:3);
      WriteLn('  bNumConfigurations    ',bNumConfigurations:3);
    End;
End;

Var Context   : TLibUsbContext;
    Device    : TLibUsbDevice;
    EP0       : TLibUsbDeviceControlEndpoint;
    CT        : TLibUsbControlTransfer;

Procedure Callback(Transfer:TLibUsbTransfer;Data:Pointer);
Var MyCT     : TLibUsbControlTransfer;
    DevDescr : Plibusb_device_descriptor;
Begin
  WriteLn('Finished Transfer, Data = ',PtrUInt(Data),', Status = ',Transfer.Status,', ActualLength = ',Transfer.ActualLength);
  if Transfer.Status <> LIBUSB_TRANSFER_COMPLETED then
    raise Exception.CreateFmt('Transfer didn''t complete: Status = %d',[Transfer.Status]);
  if Transfer.ActualLength <> LIBUSB_DT_DEVICE_SIZE then
    raise Exception.CreateFmt('Received %d bytes but expected %d',[Transfer.ActualLength,LIBUSB_DT_DEVICE_SIZE]);
  // type cast
  MyCT := Transfer as TLibUsbControlTransfer;
  // get received data
  DevDescr := Plibusb_device_descriptor(MyCT.Ptr);
  // pretty-print device descriptor
  PrintDeviceDescriptor(DevDescr);
End;

Begin
  try
    // create context
    Context := TLibUsbContext.Create;
    //Context.SetDebug(LIBUSB_LOG_LEVEL_DEBUG);
    try
      Device := TLibUsbDevice.Create(Context,DevVID,DevPID);
      try
        EP0 := TLibUsbDeviceControlEndpoint.Create(Device);
        CT := TLibUsbControlTransfer.Create(EP0,LIBUSB_DT_DEVICE_SIZE);
        // setup control transfer
        CT.bmRequestType := LIBUSB_ENDPOINT_IN or LIBUSB_REQUEST_TYPE_STANDARD or LIBUSB_RECIPIENT_DEVICE;
        CT.bRequest      := LIBUSB_REQUEST_GET_DESCRIPTOR;
        CT.wValue        := (LIBUSB_DT_DEVICE shl 8) or 0;
        CT.wIndex        := 0;
        // setup callback
        CT.ProcCallback  := @Callback;
        CT.ProcCbData    := Pointer(38);

        // submit control transfer
        WriteLn('Submitting control transfer');
        ELibUsb.Check(CT.Submit,'Submit');

        // handle events
        Context.HandleEvents(1,0);

        if assigned(CT.CbException) then
          Begin
            WriteLn(StdErr,'Exception caught in callback: ',CT.CbException.Message);
          End;

        WriteLn('Done.');
        CT.Free;
        EP0.Free;
      finally
        Device.Free;
      End;
    finally
      Context.Free;
    End;
  except
    on E : ELibUsb do
      WriteLn('USB Error: ',E.Message,': ',E.ErrorStr);
  End;
End.

