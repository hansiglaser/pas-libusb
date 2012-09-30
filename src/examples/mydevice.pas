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

Unit MyDevice;

{$mode objfpc}{$H+}

Interface

Uses
  Classes,SysUtils,BaseUnix,CTypes,LibUSB,USB,EZUSB;

Const
  USBVendEmpty  = $0547;   // Cypress
  USBProdEmpty  = $2131;   // AN2131 chip
  USBVendConf   = $fff0;
  USBProdConf   = $0002;
  FirmwareName  = 'firmware.ihx';

Const
  EP_IN    =  2 or USB_ENDPOINT_IN;
  EP_OUT   =  2 or USB_ENDPOINT_OUT;

Const
  CMD_GET_VERSION         = $80;
  CMD_GET_VERSION_STRING  = $81;
  CMD_GET_STATUS          = $82;
  // ... add further commands here as defined in the firmware's commands.h ...

Const
  ConfigUSBConfiguration = 1;
  ConfigUSBInterface     = 0;
  ConfigUSBAltInterface  = 1;

Type

  (** Protocol data types **)

  Int8   = cint8;
  Int16  = cint16;
  Int32  = cint32;
  UInt8  = cuint8;
  UInt16 = cuint16;
  UInt32 = cuint32;

  TGetVersion = packed record  // see commands.h
    Firmware : UInt16;
    // ... add further fields here as defined in the firmware's commands.h ...
  End;

  TGetStatus = packed record
    MyStatus  : UInt8;
    // ... add further fields here as defined in the firmware's commands.h ...
  End;

  TStatus = Byte;  // TODO: define type as you like

  { TMyDevice }

  TMyDevice = Class(TUSBDeviceWithFirmware)
  protected
    FUidVendorEmpty   : Word;   // unconfigured
    FUidProductEmpty  : Word;
    FUidVendorConfig  : Word;   // configured
    FUidProductConfig : Word;
    FFirmwareFile     : String;
    { USB variables }
    FInterface       : TUSBInterface;
    FEPIn            : TUSBBulkInEndpoint;
    FEPOut           : TUSBBulkOutEndpoint;
    Function  MatchUnconfigured(ADev:PUSBDevice) : Boolean; override;
    Function  MatchConfigured  (ADev:PUSBDevice) : Boolean; override;
    Procedure Configure(ADev:PUSBDevice); override;
  public
    { class methods }
    Constructor Create(AIsConfigured:Boolean;AidVendorEmpty,AidProductEmpty:Word;AFirmwareFile:String;AidVendorConfig:Word;AidProductConfig:Word);
    Destructor  Destroy; override;
    Class Function FindFirmware(AName, AProgram : String) : String;
  protected
    Function  SendCommand   (Cmd:Byte;Value:Word;Index:Word) : Integer;
    Function  SendCommandOut(Cmd:Byte;Value:Word;Index:Word;Const Buf;Length:Integer) : Integer;
    Function  SendCommandIn (Cmd:Byte;Value:Word;Index:Word;Out   Buf;Length:Integer) : Integer;
  public
    Procedure GetVersion(Out Firmware:UInt16);
    Function  GetVersion : String;
    Function  GetVersionString : String;
    Procedure GetStatus(Out Status : UInt8);
    Function  GetStatus : String;
    // ... add further device functions ...
  End;

Implementation

(**
 * Constructor
 *
 * @param AIsConfigured     if set to true, we only search for a configured device
 * @param AidVendorEmpty
 * @param AidProductEmpty   idVendor and idProduct of an EZ-USB device without firmware
 * @param AFirmwareFile     filename of the firmware Intel Hex file. e.g. as retured by FindFirmware()
 * @param AidVendorConfig
 * @param AidProductConfig  idVendor and idProduct of the configured device
 *)
Constructor TMyDevice.Create(AIsConfigured:Boolean;AidVendorEmpty,AidProductEmpty:Word;AFirmwareFile:String;AidVendorConfig:Word;AidProductConfig:Word);
Begin
  FUidVendorEmpty   := AidVendorEmpty;
  FUidProductEmpty  := AidProductEmpty;
  FUidVendorConfig  := AidVendorConfig;
  FUidProductConfig := AidProductConfig;
  FFirmwareFile     := AFirmwareFile;
  { uses MatchUnconfigured to find an unconfigured device, then Configure to
    do the configuration and finally MatchConfigured to find the configured
    device. }
  inherited Create(AIsConfigured,ConfigUSBConfiguration);

  // create handlers for the endpoints (and the interface they belong to)
  FInterface       := TUSBInterface.Create(Self,USBFindInterface(ConfigUSBInterface,ConfigUSBAltInterface,FDevice));
  FEPIn            := TUSBBulkInEndpoint. Create(FInterface,USBFindEndpoint(EP_IN,   FInterface.FInterface));
  FEPOut           := TUSBBulkOutEndpoint.Create(FInterface,USBFindEndpoint(EP_OUT,  FInterface.FInterface));
End;

(**
 * Destructor
 *)
Destructor  TMyDevice.Destroy;
Begin
  { If no configured devices were found in the constructor (or any other
    exception occured there), this destructor is automatically called. In this
    case we don't have the USB stuff setup (and nothing else), so we don't
    do the freeing and finalization stuff. }
  FInterface.Free;
  inherited Destroy;
End;

(**
 * Send a command to the device
 *
 * Commands are sent as control transfers.
 *
 * This function does not use the data phase.
 *)
Function TMyDevice.SendCommand(Cmd:Byte;Value:Word;Index:Word):Integer;
Begin
  Result := FControl.ControlMsg(
    USB_ENDPOINT_OUT or USB_TYPE_VENDOR or USB_RECIP_DEVICE { bmRequestType },
    Cmd      {bRequest},
    Value,   { wValue }
    Index,   { wIndex }
    100);
End;

(**
 * Send a command to the device
 *
 * Commands are sent as control transfers.
 *
 * This function uses an out data phase.
 *)
Function TMyDevice.SendCommandOut(Cmd:Byte;Value:Word;Index:Word;Const Buf;Length:Integer):Integer;
Begin
  Result := FControl.ControlMsg(
    USB_ENDPOINT_OUT or USB_TYPE_VENDOR or USB_RECIP_DEVICE { bmRequestType },
    Cmd      {bRequest},
    Value,   { wValue }
    Index,   { wIndex }
    Buf,     { data packet }
    Length,  { wLength }
    100);
End;

(**
 * Send a command to the device
 *
 * Commands are sent as control transfers.
 *
 * This function uses an in data phase.
 *)
Function TMyDevice.SendCommandIn(Cmd:Byte;Value:Word;Index:Word;Out Buf;Length:Integer):Integer;
Begin
  Result := FControl.ControlMsg(
    USB_ENDPOINT_IN or USB_TYPE_VENDOR or USB_RECIP_DEVICE { bmRequestType },
    Cmd      {bRequest},
    Value,   { wValue }
    Index,   { wIndex }
    Buf,     { data packet }
    Length,  { wLength }
    100);
End;

(*****************************************************************************)
(***  Device Functions  ******************************************************)
(*****************************************************************************)

(**
 * Query device version
 *
 * The firmware version is returned.
 *)
Procedure TMyDevice.GetVersion(Out Firmware:UInt16);
Var R       : LongInt;
    Version : TGetVersion;
Begin
  R := SendCommandIn(CMD_GET_VERSION,0,0,Version,SizeOf(Version));
  if R <> SizeOf(Version) then
    raise USBException('GetVersion SendCommand',R);
  Firmware := Version.Firmware;
End;

(**
 * Query device version and return a human readable string
 *)
Function TMyDevice.GetVersion : String;
Var Firmware : UInt16;
Begin
  GetVersion(Firmware);
  Result := 'Firmware version: ' + IntToStr(Firmware shr 8) + '.' + IntToStr(Firmware and $FF);
End;

(**
 * Query device version string
 *)
Function TMyDevice.GetVersionString : String;
Var R   : LongInt;
    Buf : Array[0..63] of Char;
Begin
  R := SendCommandIn(CMD_GET_VERSION_STRING,0,0,Buf,SizeOf(Buf));
  if R < 0 then
    raise USBException('GetVersionString SendCommand',R);
  SetLength(Result,R);
  Move(Buf,Result[1],R);
End;

(**
 * Query device status
 *)
Procedure TMyDevice.GetStatus(Out Status:UInt8);
Var R    : LongInt;
    Data : TGetStatus;
Begin
  R := SendCommandIn(CMD_GET_STATUS,0,0,Data,SizeOf(Data));
  if R <> SizeOf(Data) then
    raise USBException('GetStatus SendCommand',R);
  Status := Data.MyStatus;
End;

(**
 * Query device status and return a human readable string
 *)
Function TMyDevice.GetStatus : String;
Var MyStatus : UInt8;
Begin
  GetStatus(MyStatus);
  Result := 'MyStatus = ' + IntToStr(MyStatus);
End;

(*****************************************************************************)
(***  Internal Private Functions  ********************************************)
(*****************************************************************************)

(**
 * Called by ancestor class to determine wheter a given device is an
 * unconfigured device we want to use.
 *)
Function TMyDevice.MatchUnconfigured(ADev:PUSBDevice):Boolean;
Begin
  { We only use the _first_ controller from the list. This is                }
  { probably a problem when several are found to distinguish between them.   }
  { Since in this application we only have one, this problem is not further  }
  { investigated                                                             }

  Result := (ADev^.Descriptor.idVendor  = FUidVendorEmpty) and
            (ADev^.Descriptor.idProduct = FUidProductEmpty);
End;

(**
 * Called by ancestor class to determine wheter a given device is a configured
 * device we want to use.
 *)
Function TMyDevice.MatchConfigured(ADev:PUSBDevice):Boolean;
Begin
  Result := (ADev^.Descriptor.idVendor  = FUidVendorConfig) and
            (ADev^.Descriptor.idProduct = FUidProductConfig);
End;

(**
 * Download the firmware to the given device
 *)
Procedure TMyDevice.Configure(ADev:PUSBDevice);
Var EZUSB : TUSBDeviceEZUSB;
Begin
  WriteLn('Using Firmware file "'+FFirmwareFile+'" to configure devices.');
  // create a temporary TUSBDeviceEZUSB object to download the firmware
  EZUSB := TUSBDeviceEZUSB.Create(ADev);
  EZUSB.DownloadFirmware(FFirmwareFile);
  EZUSB.Free;
End;

(**
 * Replace all occurences of Src in St to Dst
 *
 * This is a Helper Function, it should be placed to a common "utils" unit.
 *)
Function StrReplace(St:String;Src,Dst:String):String;
Var L : LongInt;
Begin
  Result := '';
  repeat
    L := Pos(Src,St);
    if L = 0 then Exit(Result+St);
    Result := Result+Copy(St,1,L-1)+Dst;
    St := Copy(St,L+Length(Src),Length(St));
  Until False;
End;

(**
 * Find a file in a ':'-delimited list of paths
 *
 * This is a Helper Function, it should be placed to a common "utils" unit.
 *)
Function FindFileInPath(AName,APath:String) : String;
Begin
  APath := StrReplace(APath,'~',fpGetEnv('HOME'));
  { !!!TODO!!! $HOME points to "root"'s home when fork()ed before! Must find a function which reads /etc/passwd }
  APath := StrReplace(APath,'::',':');

  Result := FileSearch(AName,APath);
  if Result = '' then
    Exit;
  Result := ExpandFileName(Result);
End;

(**
 * Find the firmware at several directories
 *
 *)
class Function TMyDevice.FindFirmware(AName,AProgram:String):String;
Var Path : String;
Begin
  { search controller firmware file }
  Path := '.:~/.'+AProgram+':'
          +fpGetEnv(UpperCase(AProgram)+'FIRMWAREPATH')+':'
          +ExtractFilePath(ExpandFileName(ParamStr(0)))+':'
          +fpGetEnv('PATH')
          +':/etc/'+AProgram;
  ParamStr(0); { strange, this seems to be necessary to free memory previously reserved by ParamStr }
  Result := FindFileInPath(AName,Path);
  if Result = '' then
    raise Exception.Create('Couldn''t find firmware file "'+AName+'" in search path "'+Path+'". You might try to set the environment variable "'+UpperCase(AProgram)+'FIRMWAREPATH".');
End;

End.

