(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   Utilities for the object-oriented wrapper for libusb.                 *
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
Unit LibUsbUtil;

{$mode objfpc}{$H+}

Interface

Uses LibUsb,LibUsbOop;

Type

  { TLibUsbDeviceMatchVidPid }

  TLibUsbDeviceMatchVidPid = class(TLibUsbDeviceMatchClass)
  protected
    FContext : TLibUsbContext;
    FVid     : Word;
    FPid     : Word;
  public
    Function Match(Dev:Plibusb_device) : Boolean; override;
    Constructor Create(AContext:TLibUsbContext;AVid,APid:Word);
  End;

  { TLibUsbDeviceMatchVidPidSerial }

  TLibUsbDeviceMatchVidPidSerial = class(TLibUsbDeviceMatchVidPid)
  protected
    FSerial : String;
  public
    Function Match(Dev:Plibusb_device) : Boolean; override;
    Constructor Create(AContext:TLibUsbContext;AVid,APid:Word;ASerial:String);
  End;

  { TLibUsbInterfaceMatchVidPidSerial }

  { TLibUsbInterfaceMatchNumAlt }

  TLibUsbInterfaceMatchNumAlt = class(TLibUsbInterfaceMatchClass)
  protected
    FIntfNum    : Byte;
    FAltSetting : Byte;
  public
    Function Match(Intf:Plibusb_interface_descriptor) : Boolean; override;
    Constructor Create(AIntfNum,AAltSetting:Byte);
  End;

Function MatchEPBulkIn (EP:Plibusb_endpoint_descriptor;Data:Pointer) : Boolean;
Function MatchEPBulkOut(EP:Plibusb_endpoint_descriptor;Data:Pointer) : Boolean;
Function MatchEPIntrIn (EP:Plibusb_endpoint_descriptor;Data:Pointer) : Boolean;
Function MatchEPIntrOut(EP:Plibusb_endpoint_descriptor;Data:Pointer) : Boolean;

Type
  { TLibUsbDeviceWithFirmware }

  TLibUsbDeviceWithFirmware = class(TLibUsbDevice)
  protected
    Procedure Configure(ADev:Plibusb_device); virtual; abstract;
  public
    Constructor Create(AContext:TLibUsbContext;AMatchUnconfigured,AMatchConfigured:TLibUsbDeviceMatchClass); overload;
  End;

Implementation
Uses SysUtils;

{ TLibUsbDeviceMatchVidPid }

Function TLibUsbDeviceMatchVidPid.Match(Dev : Plibusb_device) : Boolean;
Var DeviceDescriptor : libusb_device_descriptor;
Begin
  DeviceDescriptor := FContext.GetDeviceDescriptor(dev);
  Result := ((DeviceDescriptor.idVendor = FVid) and (DeviceDescriptor.idProduct = FPid));
End;

Constructor TLibUsbDeviceMatchVidPid.Create(AContext : TLibUsbContext; AVid, APid : Word);
Begin
  inherited Create;
  FContext := AContext;
  FVid     := AVid;
  FPid     := APid;
End;

{ TLibUsbDeviceMatchVidPidSerial }

Function TLibUsbDeviceMatchVidPidSerial.Match(Dev : Plibusb_device) : Boolean;
Begin
  Result := inherited Match(Dev) and (FContext.GetSerialNumber(Dev) = FSerial);
End;

Constructor TLibUsbDeviceMatchVidPidSerial.Create(AContext : TLibUsbContext; AVid, APid : Word; ASerial : String);
Begin
  inherited Create(AContext,AVid,APid);
  FSerial := ASerial;
End;

{ TLibUsbInterfaceMatchNumAlt }

Function TLibUsbInterfaceMatchNumAlt.Match(Intf : Plibusb_interface_descriptor) : Boolean;
Begin
  Result := ((Intf^.bInterfaceNumber = FIntfNum) and (Intf^.bAlternateSetting = FAltSetting));
End;

Constructor TLibUsbInterfaceMatchNumAlt.Create(AIntfNum, AAltSetting : Byte);
Begin
  inherited Create;
  FIntfNum    := AIntfNum;
  FAltSetting := AAltSetting;
End;

Function MatchEPBulkIn (EP:Plibusb_endpoint_descriptor;Data:Pointer) : Boolean;
Begin
  Result := (EP^.bEndpointAddress and LIBUSB_ENDPOINT_DIR_MASK  = LIBUSB_ENDPOINT_IN) and
            (EP^.bmAttributes     and LIBUSB_TRANSFER_TYPE_MASK = LIBUSB_TRANSFER_TYPE_BULK);
End;

Function MatchEPBulkOut(EP:Plibusb_endpoint_descriptor;Data:Pointer) : Boolean;
Begin
  Result := (EP^.bEndpointAddress and LIBUSB_ENDPOINT_DIR_MASK  = LIBUSB_ENDPOINT_OUT) and
            (EP^.bmAttributes     and LIBUSB_TRANSFER_TYPE_MASK = LIBUSB_TRANSFER_TYPE_BULK);
End;

Function MatchEPIntrIn (EP:Plibusb_endpoint_descriptor;Data:Pointer) : Boolean;
Begin
  Result := (EP^.bEndpointAddress and LIBUSB_ENDPOINT_DIR_MASK  = LIBUSB_ENDPOINT_IN) and
            (EP^.bmAttributes     and LIBUSB_TRANSFER_TYPE_MASK = LIBUSB_TRANSFER_TYPE_INTERRUPT);
End;

Function MatchEPIntrOut(EP:Plibusb_endpoint_descriptor;Data:Pointer) : Boolean;
Begin
  Result := (EP^.bEndpointAddress and LIBUSB_ENDPOINT_DIR_MASK  = LIBUSB_ENDPOINT_OUT) and
            (EP^.bmAttributes     and LIBUSB_TRANSFER_TYPE_MASK = LIBUSB_TRANSFER_TYPE_INTERRUPT);
End;

{ TLibUsbDeviceWithFirmware }

Constructor TLibUsbDeviceWithFirmware.Create(AContext:TLibUsbContext;AMatchUnconfigured,AMatchConfigured:TLibUsbDeviceMatchClass);
Var Devs    : TLibUsbDeviceArray;
    Timeout : Integer;
Begin
  SetLength(Devs,0);
  // find configured devices, but don't complain if non were found
  Devs := AContext.FindDevices(AMatchConfigured,false,0);

  if (Length(Devs) = 0) and assigned(AMatchUnconfigured) then
    Begin
      // find unconfigured devices
      Devs := AContext.FindDevices(AMatchUnconfigured,false);

      if Length(Devs) > 1 then
        raise EUSBError.Create('Found too many unconfigured devices')
      else if Length(Devs) = 0 then
        Begin
          WriteLn(StdErr,'No unconfigured devices found.');  // TODO: this should only be a debug message
          Timeout := 300;
        End
      else
        Begin
          Configure(Devs[0]);
          // give it some time to ReNumerate, or at least to disconnect, otherwise
          // the following USBFindDevices would find the old device if both, the
          // unconfigured and the configured use the same IDs.
          Sleep(200);
          Timeout := 2000;
          SetLength(Devs,0);
        End;
    End
  else
    // don't even search for unconfigured devices
    Timeout := 300;

  // find configured devices
  if Length(Devs) = 0 then
    Devs := AContext.FindDevices(AMatchConfigured,false,Timeout);

  // before any exceptions, free matcher classes
  AMatchUnconfigured.Free;
  AMatchConfigured.Free;

  if Length(Devs) = 0 then
    raise EUSBError.Create('No configured devices found.')
  else if Length(Devs) > 1 then
    raise EUSBError.Create('Too many configured devices found.');

  inherited Create(AContext,Devs[0]);
End;

End.

