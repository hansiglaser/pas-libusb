(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   USB device driver skeleton.                                           *
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

Unit MyDevice;

{$mode objfpc}{$H+}

Interface

Uses
  Classes,SysUtils,CTypes,LibUsb,LibUsbOop,LibUsbUtil,EZUSB;

Const
  USBVendEmpty  = $0547;   // Cypress
  USBProdEmpty  = $2131;   // AN2131 chip
  USBVendConf   = $fff0;
  USBProdConf   = $0002;
  FirmwareName  = 'firmware.ihx';

Const
  EP_IN    =  2 or LIBUSB_ENDPOINT_IN;
  EP_OUT   =  2 or LIBUSB_ENDPOINT_OUT;

Const
  CMD_GET_VERSION         = $80;
  CMD_GET_VERSION_STRING  = $81;
  CMD_GET_STATUS          = $82;
  // ... add further commands here as defined in the firmware's commands.h ...

Const
  ConfigUSBConfiguration = 1;
  ConfigUSBInterface     = 0;
  ConfigUSBAltInterface  = 0;

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

  TMyDevice = Class(TLibUsbDeviceWithFirmware)
  protected
    FUidVendorEmpty   : Word;   // unconfigured
    FUidProductEmpty  : Word;
    FUidVendorConfig  : Word;   // configured
    FUidProductConfig : Word;
    FFirmwareFile     : String;
    { USB variables }
    FInterface       : TLibUsbInterface;
    FEPIn            : TLibUsbBulkInEndpoint;
    FEPOut           : TLibUsbBulkOutEndpoint;
    Procedure Configure(ADev:Plibusb_device); override;
  public
    { class methods }
    Constructor Create(AContext:TLibUsbContext;AMatchUnconfigured:TLibUsbDeviceMatchClass;AFirmwareFile:String;AMatchConfigured:TLibUsbDeviceMatchClass);
    Destructor  Destroy; override;
    Class Function FindFirmware(AName, AProgram : String) : String;
  protected
    Function  SendCommand   (Cmd:Byte;Value:Word;Index:Word) : Integer;
    Function  SendCommandOut(Cmd:Byte;Value:Word;Index:Word;Var   Buf;Length:Integer) : Integer;
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
 * @param AContext            libusb context
 * @param AMatchUnconfigured  device matcher class for the unconfigured device
 * @param AFirmwareFile       filename of the firmware Intel Hex file. e.g. as retured by FindFirmware()
 * @param AMatchConfigured    device matcher class for the configured device
 *)
Constructor TMyDevice.Create(AContext:TLibUsbContext;AMatchUnconfigured:TLibUsbDeviceMatchClass;AFirmwareFile:String;AMatchConfigured:TLibUsbDeviceMatchClass);
Begin
  FFirmwareFile     := AFirmwareFile;
  { uses AMatchUnconfigured to find an unconfigured device, then Configure to
    do the configuration and finally AMatchConfigured to find the configured
    device. }
  inherited Create(AContext,AMatchUnconfigured,AMatchConfigured);
  SetConfiguration(ConfigUSBConfiguration);

  // create handlers for the endpoints (and the interface they belong to)
  FInterface       := TLibUsbInterface.Create(Self,FindInterface(ConfigUSBInterface,ConfigUSBAltInterface));
  FEPIn            := TLibUsbBulkInEndpoint. Create(FInterface,FInterface.FindEndpoint(EP_IN));
  FEPOut           := TLibUsbBulkOutEndpoint.Create(FInterface,FInterface.FindEndpoint(EP_OUT));
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
    { bmRequestType } LIBUSB_ENDPOINT_OUT or LIBUSB_REQUEST_TYPE_VENDOR or LIBUSB_RECIPIENT_DEVICE,
    { bRequest      } Cmd,
    { wValue        } Value,
    { wIndex        } Index,
    { Timeout       } 100);
End;

(**
 * Send a command to the device
 *
 * Commands are sent as control transfers.
 *
 * This function uses an out data phase.
 *)
Function TMyDevice.SendCommandOut(Cmd:Byte;Value:Word;Index:Word;Var Buf;Length:Integer):Integer;
Begin
  Result := FControl.ControlMsg(
    { bmRequestType } LIBUSB_ENDPOINT_OUT or LIBUSB_REQUEST_TYPE_VENDOR or LIBUSB_RECIPIENT_DEVICE,
    { bRequest      } Cmd,
    { wValue        } Value,
    { wIndex        } Index,
    { Buf           } Buf,
    { wLength       } Length,
    { Timeout       } 100);
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
    { bmRequestType } LIBUSB_ENDPOINT_IN or LIBUSB_REQUEST_TYPE_VENDOR or LIBUSB_RECIPIENT_DEVICE,
    { bRequest      } Cmd,
    { wValue        } Value,
    { wIndex        } Index,
    { Buf           } Buf,
    { wLength       } Length,
    { Timeout       } 100);
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
    raise ELibUsb.Create(R,'GetVersion SendCommand');
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
    raise ELibUsb.Create(R,'GetVersionString SendCommand');
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
    raise ELibUsb.Create(R,'GetStatus SendCommand');
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
 * Download the firmware to the given device
 *)
Procedure TMyDevice.Configure(ADev:Plibusb_device);
Var EZUSB : TLibUsbDeviceEZUSB;
Begin
  WriteLn('Using Firmware file "'+FFirmwareFile+'" to configure devices.');
  // create a temporary TUSBDeviceEZUSB object to download the firmware
  EZUSB := TLibUsbDeviceEZUSB.Create(FContext,ADev);
  EZUSB.SetConfiguration(ANCHOR_USB_CONFIG);
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
  APath := StrReplace(APath,'~',GetEnvironmentVariable('HOME'));
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
          +GetEnvironmentVariable(UpperCase(AProgram)+'FIRMWAREPATH')+':'
          +ExtractFilePath(ExpandFileName(ParamStr(0)))+':'
          +GetEnvironmentVariable('PATH')
          +':/etc/'+AProgram;
  ParamStr(0); { strange, this seems to be necessary to free memory previously reserved by ParamStr }
  Result := FindFileInPath(AName,Path);
  if Result = '' then
    raise Exception.Create('Couldn''t find firmware file "'+AName+'" in search path "'+Path+'". You might try to set the environment variable "'+UpperCase(AProgram)+'FIRMWAREPATH".');
End;

End.

