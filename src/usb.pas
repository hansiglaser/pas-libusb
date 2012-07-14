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
TODO:
 - Control Transfer from Device, Interface and Class
*)
Unit USB;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Baseunix, LibUSB;

(*****************************************************************************)
(**  Procedural Interface  ***************************************************)
(*****************************************************************************)

Type
  USBDeviceArray = Array of PUSBDevice;
  TUSBDeviceMatchMethod    = Function(Dev:PUSBDevice) : Boolean of object;
  TUSBDeviceMatchFunc      = Function(Dev:PUSBDevice;Data:Pointer) : Boolean;
  TUSBInterfaceMatchMethod = Function(Intf:PUSBInterfaceDescriptor) : Boolean of object;
  TUSBInterfaceMatchFunc   = Function(Intf:PUSBInterfaceDescriptor;Data:Pointer) : Boolean;
  TUSBEndpointMatchMethod  = Function(EP:PUSBEndpointDescriptor) : Boolean of object;
  TUSBEndpointMatchFunc    = Function(EP:PUSBEndpointDescriptor;Data:Pointer) : Boolean;

Function USBFindDevices  (MatchFunc:TUSBDeviceMatchMethod;           Timeout:Integer=0) : USBDeviceArray;
Function USBFindDevices  (MatchFunc:TUSBDeviceMatchFunc;Data:Pointer;Timeout:Integer=0) : USBDeviceArray;
Function USBFindInterface(MatchFunc:TUSBInterfaceMatchMethod;           Dev:PUSBDevice):PUSBInterfaceDescriptor;
Function USBFindInterface(MatchFunc:TUSBInterfaceMatchFunc;Data:Pointer;Dev:PUSBDevice):PUSBInterfaceDescriptor;
Function USBFindInterface(AInterface,AAltSetting:Byte;                  Dev:PUSBDevice):PUSBInterfaceDescriptor;
Function USBFindInterface(                                              Dev:PUSBDevice):PUSBInterfaceDescriptor;
Function USBFindEndpoint (MatchFunc:TUSBEndpointMatchMethod;            Intf:PUSBInterfaceDescriptor):PUSBEndpointDescriptor;
Function USBFindEndpoint (MatchFunc:TUSBEndpointMatchFunc;Data:Pointer; Intf:PUSBInterfaceDescriptor):PUSBEndpointDescriptor;
Function USBFindEndpoint (bEndpointAddress:Byte;                        Intf:PUSBInterfaceDescriptor):PUSBEndpointDescriptor;

Type
  PUSBVendorProduct = ^TUSBVendorProduct;
  TUSBVendorProduct = record
    idVendor  : Word;
    idProduct : Word;
  End;

Function USBMatchDeviceVendorProduct(Dev:PUSBDevice;Data:Pointer) : Boolean;

Function USBGetString(Handle:PUSBDevHandle;Index:Integer) : String;
Function USB_Send(Hnd:PUSBDevHandle;EP:LongInt;Const Buf;Length,Timeout:LongInt):LongInt;
Function USB_Send(Hnd:PUSBDevHandle;EP:LongInt;St:String;Timeout:LongInt):LongInt;
Function USB_Recv(Hnd:PUSBDevHandle;EP:LongInt;Out Buf;Length:LongInt;Timeout:LongInt) : LongInt;

Function USBGetDriver(Handle:PUSBDevHandle;Intf:Integer) : String;

(*****************************************************************************)
(**  Object-Oriented Interface  **********************************************)
(*****************************************************************************)

Type
  TUSBDeviceControlEndpoint = class;
  TUSBInterruptInEndpoint   = class;

  { TUSBDevice }

  (**
   * Generic USB Device
   *
   * Derive from this class for your own device and offer a constructor which
   * better suits your device. It should search for the device, then use
   * this inherited constructor, and then instanciate one or more interfaces
   * and seek for their endpoints.
   *)
  TUSBDevice = class
  protected
    FidProduct : Word;
    FidVendor  : Word;
    FSerial    : String;
    FDevice    : PUSBDevice;
    FHandle    : PUSBDevHandle;
    FConfig    : Integer;
    FControl   : TUSBDeviceControlEndpoint;
    Procedure Open;
    Procedure Close;
    Function  MatchVendorProduct      (ADev:PUSBDevice) : Boolean;
    Function  MatchVendorProductSerial(ADev:PUSBDevice) : Boolean;
  public
    Constructor Create(ADev:PUSBDevice;AConfig:Integer=-1); virtual; overload;
    Constructor Create(AidVendor,AidProduct:Word;AConfig:Integer=-1); overload;
    Constructor Create(AidVendor,AidProduct:Word;ASerial:String;AConfig:Integer=-1); overload;
    Destructor  Destroy; override;
    Function  IsPresent : Boolean;
    Function  USBGetString(Index:Integer) : String;
    property Device : PUSBDevice read FDevice;
  End;

  { TUSBDeviceWithFirmware }

  TUSBDeviceWithFirmware = class(TUSBDevice)
  protected
    Function MatchUnconfigured(ADev:PUSBDevice) : Boolean; virtual; abstract;
    Function MatchConfigured  (ADev:PUSBDevice) : Boolean; virtual; abstract;
    Procedure Configure(ADev:PUSBDevice); virtual; abstract;
  public
    Constructor Create(AIsConfigured:Boolean;AConfig:Integer=-1); overload;
  End;

  { TUSBInterface }

  TUSBInterface = class
    FDevice    : TUSBDevice;
    FInterface : PUSBInterfaceDescriptor;
    Procedure Claim;
    Procedure Release;
  public
    Constructor Create(ADev:TUSBDevice;AIntf:PUSBInterfaceDescriptor=Nil); overload;
    Destructor  Destroy; override;
  End;

  { TUSBPseudoHIDInterface }

  PHIDReport = ^THIDReport;
  THIDReport = record
    ReportID : Byte;
    Data : Array[0..0] of Byte;
  End;

  TIntrReportFunc = Function(Report:PHIDReport) : Boolean of object;  // return true if report was consumed

  TUSBPseudoHIDInterface = class(TUSBInterface)
  private
    FIntrReports  : TThreadList;
    FOnIntrReport : TIntrReportFunc;
  protected
    FIntrEndpoint : TUSBInterruptInEndpoint;
    Function SetReport(ReportType, ReportID: Byte; const Buf; Length: LongInt): LongInt;
    Function GetReport(ReportType, ReportID: Byte; var Buf; Length: LongInt): LongInt;
  public
    Constructor Create(ADevice:TUSBDevice;AIntf:PUSBInterfaceDescriptor=Nil);
    Destructor  Destroy; override;
    Function SetOutputReport(ReportID:Byte;Const Buf;Length:LongInt) : LongInt;
    Function InterruptRead: Integer;
    Function HasReport(ReportID:Byte) : PHIDReport;
    Procedure EatReport(Report: PHIDReport);
    Function GetReport(ReportID:Byte) : PHIDReport;
    property OnIntrReport : TIntrReportFunc read FOnIntrReport write FOnIntrReport;
  End;

  { TUSBEndpoint }
  TUSBEndpoint = class
  protected
    FEndpoint  : PUSBEndpointDescriptor;
  public
    Constructor Create(AEndpoint:PUSBEndpointDescriptor); overload;
    Destructor  Destroy; override;
  End;

  { TUSBDeviceEndpoint }

  TUSBDeviceEndpoint = class(TUSBEndpoint)
  protected
    FDevice : TUSBDevice;
  public
    Constructor Create(ADevice:TUSBDevice;AEndpoint:PUSBEndpointDescriptor); overload;
  End;

  { TUSBInterfaceEndpoint }

  TUSBInterfaceEndpoint = class(TUSBEndpoint)
  protected
    FInterface : TUSBInterface;
  public
    Constructor Create(AIntf:TUSBInterface;AEndpoint:PUSBEndpointDescriptor); overload;
  End;

  { TUSBDeviceControlEndpoint }

  TUSBDeviceControlEndpoint = class(TUSBDeviceEndpoint)
    Constructor Create(ADevice:TUSBDevice); overload;
    Function ControlMsg(RequestType:Byte;Request:Byte;Value:Word;Index:Word;Const Buf;Length,Timeout:LongInt):LongInt;
    Function ControlMsg(RequestType:Byte;Request:Byte;Value:Word;Index:Word;Timeout:LongInt):LongInt;
    Function GetString(Index:Word):String;
    Function GetConfiguration : Integer;
  End;

  { TUSBBulkOutEndpoint }

  TUSBBulkOutEndpoint = class(TUSBInterfaceEndpoint)
    Function Send(Const Buf;Length,Timeout:LongInt):LongInt;
    Function Send(St:String;Timeout:LongInt):LongInt;
  End;

  { TUSBBulkInEndpoint }

  TUSBBulkInEndpoint = class(TUSBInterfaceEndpoint)
    Function Recv(Out Buf;Length:LongInt;Timeout:LongInt) : LongInt;
  End;

  { TUSBInterruptOutEndpoint }

  TUSBInterruptOutEndpoint = class(TUSBInterfaceEndpoint)
    Function Send(Const Buf;Length,Timeout:LongInt):LongInt;
    Function Send(St:String;Timeout:LongInt):LongInt;
  End;

  { TUSBInterruptInEndpoint }

  TUSBInterruptInEndpoint = class(TUSBInterfaceEndpoint)
    Function Recv(Out Buf;Length:LongInt;Timeout:LongInt) : LongInt;
  End;


Type EUSBError    = class(Exception);
     EUSBTimeout  = class(EUSBError);

Function USBException(Proc:String;Ret:LongInt) : Exception;

Implementation
Uses PasLibUsbUtils,Math;

(*****************************************************************************)
(**  Procedural Interface  ***************************************************)
(*****************************************************************************)

Type TUSBExceptionType = Class of Exception;

Function USBException(Proc:String;Ret:LongInt) : Exception;
Var Ex : TUSBExceptionType;
Begin
  Case -Ret of
    ESysETIMEDOUT : Ex := EUSBTimeout;
  else
    Ex := EUSBError;
  End;
  Result := Ex.CreateFmt('%s returns %d; %s',[Proc,Ret,usb_strerror]);
End;

(**
 * Find USB Devices according to MatchFunc
 *
 * MatchFunc  method to compare a given device with a criterion
 * Devs       linked list of devices which were found
 * Timeout    maximum time we retry to find the device, 0 means only a single try
 *
 * returns count of found devices, -1 on error
 *)
Function USBFindDevices(MatchFunc:TUSBDeviceMatchMethod;Timeout:Integer=0) : USBDeviceArray;
Var Busses : PUSBBus;
    Dev    : PUSBDevice;
    Start  : UInt64;
Begin
  SetLength(Result,0);
  Start := GetUSec;
  repeat
    if usb_find_busses  < 0 then Exit;
    if usb_find_devices < 0 then Exit;
    Busses := usb_get_busses;

    While Busses <> Nil do
      Begin
        Dev := Busses^.Devices;
        While Dev <> Nil do
          Begin
            if MatchFunc(Dev) then
              Begin
                SetLength(Result,Length(Result)+1);
                Result[Length(Result)-1] := Dev;
              End;
            Dev := Dev^.Next;
          End;
        Busses := Busses^.Next;
      End;
    if (Length(Result) = 0) and (Timeout > 0) then
      Begin
        Sleep(Min(100,(GetUSec-Start) div 1000));  // milliseconds
      End;
  Until (Length(Result) > 0) or (GetUSec-Start > Timeout*1000);
End;

(**
 * Helper class for function below
 *)
Type
  TDeviceFuncMatcher = class
    FFunc : TUSBDeviceMatchFunc;
    FData : Pointer;
    Constructor Create(AFunc:TUSBDeviceMatchFunc;AData:Pointer);
    Function    Match(Dev:PUSBDevice) : Boolean;
  End;
Constructor TDeviceFuncMatcher.Create(AFunc:TUSBDeviceMatchFunc;AData:Pointer);
Begin
  FFunc := AFunc;
  FData := AData;
End;
Function TDeviceFuncMatcher.Match(Dev:PUSBDevice):Boolean;
Begin
  Result := FFunc(Dev,FData);
End;

(**
 * Find USB Devices according to MatchFunc
 *
 * MatchFunc  function to compare a given device with a criterion
 * Data       Data for MatchFunc
 * Devs       linked list of devices which were found
 *
 * returns count of found devices, -1 on error
 *)
Function USBFindDevices(MatchFunc:TUSBDeviceMatchFunc;Data:Pointer;Timeout:Integer=0) : USBDeviceArray;
Var FuncMatcher : TDeviceFuncMatcher;
Begin
  FuncMatcher := TDeviceFuncMatcher.Create(MatchFunc,Data);
  Result := USBFindDevices(@FuncMatcher.Match,Timeout);
  FuncMatcher.Free;
End;

(**
 * Find USB Interface according to MatchFunc
 *
 * MatchFunc  method to compare a given interface with a criterion
 * Dev        device which is searched for interfaces
 *
 * returns an interface descritor or Nil if none was found
 *)
Function USBFindInterface(MatchFunc:TUSBInterfaceMatchMethod;Dev:PUSBDevice):PUSBInterfaceDescriptor;
Var I,J : Integer;
Begin
  Result := Nil;
  if Dev^.Config^.TheInterface = Nil then Exit;
  For I := 0 to Dev^.Config^.bNumInterfaces-1 do
    Begin
      if Dev^.Config^.TheInterface^[I].AltSetting = Nil then Continue;
      For J := 0 to Dev^.Config^.TheInterface^[I].num_altsetting-1 do
        if MatchFunc(@(Dev^.Config^.TheInterface^[I].AltSetting^[J])) then
          Exit(@(Dev^.Config^.TheInterface^[I].AltSetting^[J]));
    End;
End;

(**
 * Helper class for function below
 *)
Type
  TInterfaceFuncMatcher = class
    FFunc : TUSBInterfaceMatchFunc;
    FData : Pointer;
    Constructor Create(AFunc:TUSBInterfaceMatchFunc;AData:Pointer);
    Function    Match(Dev:PUSBInterfaceDescriptor) : Boolean;
  End;
Constructor TInterfaceFuncMatcher.Create(AFunc:TUSBInterfaceMatchFunc;AData:Pointer);
Begin
  FFunc := AFunc;
  FData := AData;
End;
Function TInterfaceFuncMatcher.Match(Dev:PUSBInterfaceDescriptor):Boolean;
Begin
  Result := FFunc(Dev,FData);
End;

(**
 * Find USB Interface according to MatchFunc
 *
 * MatchFunc  method to compare a given interface with a criterion
 * Data       Data for MatchFunc
 * Dev        device which is searched for interfaces
 *
 * returns an interface descritor or Nil if none was found
 *)
Function USBFindInterface(MatchFunc: TUSBInterfaceMatchFunc; Data:Pointer;Dev: PUSBDevice): PUSBInterfaceDescriptor;
Var FuncMatcher : TInterfaceFuncMatcher;
Begin
  FuncMatcher := TInterfaceFuncMatcher.Create(MatchFunc,Data);
  Result := USBFindInterface(@FuncMatcher.Match,Dev);
  FuncMatcher.Free;
End;

Function MatchTemplate(Intf:PUSBInterfaceDescriptor;Data:Pointer) : Boolean;
Begin
  Result := (Intf^.bInterfaceNumber  = PUSBInterfaceDescriptor(Data)^.bInterfaceNumber) and
            (Intf^.bAlternateSetting = PUSBInterfaceDescriptor(Data)^.bAlternateSetting);
End;

(**
 * Find USB Interface according to MatchFunc
 *
 * AInterface  desired bInterfaceNumber
 * AAltSetting desired bAlternateSetting
 * Dev         device which is searched for interfaces
 *
 * returns an interface descritor or Nil if none was found
 *)
Function USBFindInterface(AInterface,AAltSetting:Byte;Dev:PUSBDevice):PUSBInterfaceDescriptor;
Var Template : USBInterfaceDescriptor;
Begin
  Template.bInterfaceNumber  := AInterface;
  Template.bAlternateSetting := AAltSetting;
  Result := USBFindInterface(@MatchTemplate,@Template,Dev);
End;

(**
 * Return Solitary USB Interface
 *
 * Dev        device which is searched for interfaces
 *
 * returns an interface descritor or Nil if none or too many were found
 *)
Function USBFindInterface(Dev:PUSBDevice):PUSBInterfaceDescriptor;
Begin
  if Dev^.Config^.TheInterface = Nil then Exit;
  if Dev^.Config^.bNumInterfaces <> 1 then Exit;
  if Dev^.Config^.TheInterface^[0].AltSetting = Nil then Exit;
  if Dev^.Config^.TheInterface^[0].num_altsetting <> 1 then Exit;
  Result := @(Dev^.Config^.TheInterface^[0].AltSetting^[0]);
End;

(**
 * Find USB endpoint according to MatchFunc
 *
 * MatchFunc  method to compare a endpoint with a criterion
 * Dev        device which is searched for interfaces
 *
 * returns an endpoint descritor or Nil if none was found
 *)
Function USBFindEndpoint(MatchFunc: TUSBEndpointMatchMethod;Intf: PUSBInterfaceDescriptor): PUSBEndpointDescriptor;
Var I : Integer;
Begin
  For I := 0 to Intf^.bNumEndpoints-1 do
    if MatchFunc(@(Intf^.Endpoint^[I])) then
      Exit(@(Intf^.Endpoint^[I]));
  Result := Nil;
End;

(**
 * Helper class for function below
 *)
Type
  TEndpointFuncMatcher = class
    FFunc : TUSBEndpointMatchFunc;
    FData : Pointer;
    Constructor Create(AFunc:TUSBEndpointMatchFunc;AData:Pointer);
    Function    Match(Dev:PUSBEndpointDescriptor) : Boolean;
  End;
Constructor TEndpointFuncMatcher.Create(AFunc:TUSBEndpointMatchFunc;AData:Pointer);
Begin
  FFunc := AFunc;
  FData := AData;
End;
Function TEndpointFuncMatcher.Match(Dev:PUSBEndpointDescriptor):Boolean;
Begin
  Result := FFunc(Dev,FData);
End;

(**
 * Find USB Endpoint according to MatchFunc
 *
 * MatchFunc  method to compare a endpoint with a criterion
 * Data       Data for MatchFunc
 * Dev        device which is searched for interfaces
 *
 * returns an endpoint descritor or Nil if none was found
 *)
Function USBFindEndpoint(MatchFunc: TUSBEndpointMatchFunc; Data:Pointer;Intf: PUSBInterfaceDescriptor): PUSBEndpointDescriptor;
Var FuncMatcher : TEndpointFuncMatcher;
Begin
  FuncMatcher := TEndpointFuncMatcher.Create(MatchFunc,Data);
  Result := USBFindEndpoint(@FuncMatcher.Match,Intf);
  FuncMatcher.Free;
End;

Function MatchEndpointAddress(EP:PUSBEndpointDescriptor;Data:Pointer) : Boolean;
Begin
  Result := EP^.bEndpointAddress = Byte(PtrUInt(Data));
End;

(**
 * Find USB Endpoint with given endpoint address
 *
 * EndpointAddress endpoint address
 * Dev             device which is searched for interfaces
 *
 * returns an endpoint descritor or Nil if none was found
 *)
Function USBFindEndpoint(bEndpointAddress:Byte;Intf:PUSBInterfaceDescriptor):PUSBEndpointDescriptor;
Begin
  Result := USBFindEndpoint(@MatchEndpointAddress,Pointer(PtrUInt(bEndpointAddress)),Intf);
End;

Function USBMatchDeviceVendorProduct(Dev:PUSBDevice;Data:Pointer) : Boolean;
Var VP : PUSBVendorProduct;
Begin
  VP := PUSBVendorProduct(Data);
  Result := (Dev^.Descriptor.idVendor  = VP^.idVendor ) and
            (Dev^.Descriptor.idProduct = VP^.idProduct);
End;

Function USBGetString(Handle:PUSBDevHandle;Index:Integer) : String;
Begin
  SetLength(Result,64);
  SetLength(Result,usb_get_string_simple(Handle,Index,Result[1],Length(Result)));
End;

Function USB_Send(Hnd:PUSBDevHandle;EP:LongInt;Const Buf;Length,Timeout:LongInt):LongInt;
Var Loops  : Integer;
    Sent   : LongInt;
    ToSend : LongInt;
    Again  : Boolean;
    P      : Pointer;
Begin
{  WriteLn('USB_Send: EP = ',EP,' St = ',St,' Length = ',Length(St));}
  Sent := 0;
  ToSend := Length;
  Loops := 10;
  P     := @Buf;
  repeat
    usb_error_errno := 0;
    Result := usb_bulk_write(Hnd,EP,P,ToSend,Timeout); { returns number of bytes written on success or < 0 on error }
    Dec(Loops);
    Again := False;
    { check if an error occured }
    if Result < 0 then
      Begin
WriteLn('Problem with usb_bulk_write: Result = ',Result,', usb_error_errno = ',usb_error_errno,
                                                        ', usb_error_type = ',usb_error_type {,
                                                        ', usb_error_str = ',usb_error_str });
        if (Loops > 0) and (usb_error_errno = ESysEAGAIN) then
          Begin
            Again := True;
            fpSelect(0,Nil,Nil,Nil,100);
          End;
      End
    else
      Begin
        Inc(Sent,Result);
        Inc(P,Result);
        Dec(ToSend,Result);
        if Sent < Length then Again := True;
      End;
    { WriteLn('USB_Send: Hnd = ',Cardinal(Hnd),'  Loops = ',Loops,'  ErrNo = ',usb_error_errno,'  Result = ',Result,'  Length = ',Length,'  Timeout = ',Timeout,'  Sent = ',Sent,'  ToSend = ',ToSend,'  Again = ',Again); }
  until not Again;
  if Result < 0 then
    raise USBException('usb_bulk_write',Result);
End;

Function USB_Send(Hnd:PUSBDevHandle;EP:LongInt;St:String;Timeout:LongInt):LongInt;
Begin
  Result := USB_Send(Hnd,EP,St[1],Length(St),Timeout);
End;

Function USB_Recv(Hnd:PUSBDevHandle;EP:LongInt;Out Buf;Length:LongInt;Timeout:LongInt) : LongInt;
{Var I : LongInt;
    B : Array[0..63] of Char absolute Buf;}
Begin
  usb_error_errno := 0;
  Result := usb_bulk_read(Hnd,$80 or EP,@Buf,Length,Timeout);
  //if Result > 0 then HexDump(Buf,Result);
  //if Result < 0 then raise USBException('usb_bulk_read',Result);
  //if Result < 0 then WriteLn(Format('%s returns %d; %s',['usb_bulk_read',Result,usb_strerror]));
End;

Function USBGetDriver(Handle:PUSBDevHandle;Intf:Integer):String;
Var R : Integer;
Const MaxLen = 100;
Begin
  SetLength(Result,MaxLen);
  R := usb_get_driver_np(Handle,Intf,Result[1],MaxLen);  // puts a 0-terminated string to St[1], returns 0 on success
  if R = -ESysENODATA then
    Exit('');   // no data available, so no driver bound
  if R < 0 then
    raise USBException('usb_get_driver_np',R);
  SetLength(Result,strlen(@Result[1]));  // update length field of AnsiString
End;

(*****************************************************************************)
(**  Object-Oriented Interface  **********************************************)
(*****************************************************************************)

{ TUSBDevice }

Constructor TUSBDevice.Create(ADev:PUSBDevice;AConfig:Integer);
Begin
  FDevice := ADev;
  if AConfig >= 0 then
    FConfig := AConfig
  else
    Begin
      { use value of the single configuration }
      // TODO: shouldn't LibUSB offer multiple configurations?
      FConfig := FDevice^.config^.bConfigurationValue;
    End;
  { Open a connection to the USB device }
  Open;
End;

Constructor TUSBDevice.Create(AidVendor,AidProduct:Word;AConfig:Integer);
Var Devices : USBDeviceArray;
Begin
  FidVendor  := AidVendor;
  FidProduct := AidProduct;
  Devices := USBFindDevices(@MatchVendorProduct);
  if Devices = Nil then
    raise EUSBError.CreateFmt('Found no devices with %.04x:%.04x',[AidVendor,AidProduct]);
  if Length(Devices) <> 1 then
    raise EUSBError.CreateFmt('Found %d devices with %.04x:%.04x',[Length(Devices),AidVendor,AidProduct]);
  Create(Devices[0],AConfig);
End;

Constructor TUSBDevice.Create(AidVendor,AidProduct:Word;ASerial:String;AConfig:Integer);
Var Devices : USBDeviceArray;
Begin
  FidVendor  := AidVendor;
  FidProduct := AidProduct;
  FSerial    := ASerial;
  Devices := USBFindDevices(@MatchVendorProductSerial);
  if Length(Devices) <> 1 then
    raise EUSBError.CreateFmt('Found %d devices with %04x:%04x',[Length(Devices),AidVendor,AidProduct]);
  Create(Devices[0],AConfig);
End;

Destructor TUSBDevice.Destroy;
Begin
  Close;
  inherited Destroy;
End;

Function TUSBDevice.MatchVendorProduct(ADev:PUSBDevice):Boolean;
Begin
  Result := (ADev^.Descriptor.idVendor  = FidVendor ) and
            (ADev^.Descriptor.idProduct = FidProduct);
End;

Function TUSBDevice.MatchVendorProductSerial(ADev:PUSBDevice):Boolean;
Var Handle : PUSBDevHandle;
Begin
  Result := (ADev^.Descriptor.idVendor  = FidVendor ) and
            (ADev^.Descriptor.idProduct = FidProduct);
  if not Result then Exit;
  { check serial number }
  Handle := usb_open(FDevice);
  if Handle = Nil then Exit;  // couldn't open the device, so its not what we want to access
  Result := (USBGetString(ADev^.Descriptor.iSerialNumber) = FSerial);
  usb_close(Handle);
End;

Procedure TUSBDevice.Open;
Var R  : Integer;
    I  : Integer;
Begin
  if FHandle <> Nil then Exit;  // already open

  { open the device }
  usb_error_errno := 0;
  { WriteLn('Opening device ',IntToHex(FDevice^.Descriptor.idVendor,4),':',IntToHex(FDevice^.Descriptor.idProduct,4),'...'); }
  FHandle := usb_open(FDevice);
  { WriteLn('Result = ',Cardinal(Result),'  ErrNo = ',usb_error_errno,' Config = ',Config,' Interface = ',TheInterface,' AltInterface = ',AltInterface); }
  if FHandle = Nil then raise USBException('usb_open',PtrUInt(FHandle));

  { create control endpoint }
  FControl := TUSBDeviceControlEndpoint.Create(Self);

  FidProduct := FDevice^.Descriptor.idProduct;
  FidVendor  := FDevice^.Descriptor.idVendor;
  FSerial    := FControl.GetString(FDevice^.Descriptor.iSerialNumber);

  try
    { set configuration }
    if FControl.GetConfiguration = FConfig then
      Exit;  // desired configuration is already set
    usb_error_errno := 0;
    R := usb_set_configuration(FHandle,FConfig);
    { WriteLn('R = ',R,'  ErrNo = ',usb_error_errno); }
    if R >= 0 then Exit;
    if R <> -ESysEBUSY then
      raise USBException('usb_set_configuration',R);
    { device or resource busy, this is probably because one of the
      interfaces in this config is occupied by another driver, e.g. the
      kernel }
    WriteLn('Couldn''t set configuration, probably an interface is claimed by another driver:');
    For I := 0 to FDevice^.Config^.bNumInterfaces-1 do
      Begin
        WriteLn('  Driver of Interface ',FDevice^.Config^.TheInterface^[I].altsetting^[0].bInterfaceNumber,
                         ': ',USBGetDriver(FHandle,FDevice^.Config^.TheInterface^[I].altsetting^[0].bInterfaceNumber));
      End;
  except
    on E : Exception do
      Begin
        { close the USB handle }
        usb_close(FHandle);
        FHandle := Nil;  // don't close it twice
        raise; { reraise the exception }
      End;
  End;

End;

Procedure TUSBDevice.Close;
Begin
  if FHandle = Nil then Exit;
  { close the device }
  FControl.Free;
  usb_close(FHandle);
  FHandle := Nil;
End;

(**
 * Check if the device is still connected
 *)
Function TUSBDevice.IsPresent:Boolean;
Var Status : Word;
    R      : Integer;
Begin
  R := FControl.ControlMsg(USB_RECIP_DEVICE or USB_ENDPOINT_IN,USB_REQ_GET_STATUS,0,0,Status,2,40);
  Result := (R = 2);
End;

Function TUSBDevice.USBGetString(Index : Integer) : String;
Begin
  Result := USB.USBGetString(FHandle,Index);
End;

{ TUSBDeviceWithFirmware }

Constructor TUSBDeviceWithFirmware.Create(AIsConfigured:Boolean;AConfig:Integer);
Var Devs : USBDeviceArray;
    Timeout : Integer;
Begin
  if not AIsConfigured then
    Begin
      { find unconfigured devices }
      Devs := USBFindDevices(@MatchUnconfigured);

      if Length(Devs) > 1 then
        raise EUSBError.Create('Found too many unconfigured devices')
      else if Length(Devs) = 0 then
        Begin
          WriteLn('No unconfigured devices found.');  // TODO: this should only be a debug message
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
        End;
    End
  else
    // don't even search for unconfigured devices
    Timeout := 300;

  { find configured devices }
  Devs := USBFindDevices(@MatchConfigured,Timeout);
  if Length(Devs) = 0 then
    raise EUSBError.Create('No configured devices found.')
  else if Length(Devs) > 1 then
    raise EUSBError.Create('Too many configured devices found.');

  inherited Create(Devs[0],AConfig);
End;

{ TUSBInterface }

Constructor TUSBInterface.Create(ADev:TUSBDevice;AIntf:PUSBInterfaceDescriptor);
Begin
  inherited Create;
  FDevice    := ADev;
  FInterface := AIntf;

  if FInterface = Nil then
    FInterface := USBFindInterface(FDevice.FDevice);

  if FInterface = Nil then
    raise EUSBError.Create('No interface defined for this device.');

  Claim;
End;

Destructor TUSBInterface.Destroy;
Begin
  Release;
  Inherited Destroy;
End;

Procedure TUSBInterface.Claim;
Var R  : Integer;
    St : String;
Begin
  { claim interface }
  usb_error_errno := 0;
  R := usb_claim_interface(FDevice.FHandle,FInterface^.bInterfaceNumber);
  { WriteLn('R = ',R,'  ErrNo = ',usb_error_errno); }
  if R < 0 then
    Begin
      if R <> -ESysEBUSY then
        raise USBException('usb_claim_interface',R);
      { device or resource busy, this is probably because the interface is
        clamied by another driver, e.g. the kernel }
      St := USBGetDriver(FDevice.FHandle,FInterface^.bInterfaceNumber);
      if St = '' then
        raise EUSBError.CreateFmt('Couldn''t claim interface %d but no driver was reported.',[FInterface^.bInterfaceNumber]);
      WriteLn('Couldn''t claim interface because is claimed by another driver "',St,'", trying to detach.');
      { detach driver }
      R := usb_detach_kernel_driver_np(FDevice.FHandle,FInterface^.bInterfaceNumber);
      if R < 0 then
        raise USBException('usb_detach_kernel_driver_np',R);
      { claim again }
      R := usb_claim_interface(FDevice.FHandle,FInterface^.bInterfaceNumber);
      if R < 0 then
        raise USBException('2nd usb_claim_interface',R);
    End;

  { set alternate interface }
  usb_error_errno := 0;
  R := usb_set_altinterface(FDevice.FHandle,FInterface^.bAlternateSetting);
  { WriteLn('R = ',R,'  ErrNo = ',usb_error_errno); }
  if R < 0 then raise USBException('usb_set_altinterface',R);
End;

Procedure TUSBInterface.Release;
Var R : Integer;
Begin
  R := usb_release_interface(FDevice.FHandle,FInterface^.bInterfaceNumber);
  if R < 0 then raise USBException('usb_release_interface',R);
End;

{ TUSBPseudoHIDInterface }

Constructor TUSBPseudoHIDInterface.Create(ADevice:TUSBDevice;AIntf:PUSBInterfaceDescriptor);
Var E : Integer;
    EP   : USBEndpointDescriptor;
Begin
  inherited Create(ADevice,AIntf);

  { search Interrupt IN endpoint }
  For E := 0 to FInterface^.bNumEndpoints-1 do
    Begin
      EP := FInterface^.Endpoint^[E];
      if (EP.bmAttributes and USB_ENDPOINT_TYPE_MASK = USB_ENDPOINT_TYPE_INTERRUPT) and
         (EP.bEndpointAddress and USB_ENDPOINT_DIR_MASK <> 0) then
        Begin
          FIntrEndpoint := TUSBInterruptInEndpoint.Create(Self,@(FInterface^.Endpoint^[E]));
          break;
        End;
    End;

  if not assigned(FIntrEndpoint) then
    raise EUSBError.Create('ERROR: Couldn''t find interrupt endpoint');

  { create list for received reports }
  FIntrReports  := TThreadList.Create;
End;

Destructor TUSBPseudoHIDInterface.Destroy;
Var List : TList;
    I    : Integer;
Begin
  { free unused reports }
  List := FIntrReports.LockList;
    For I := 0 to List.Count-1 do
      FreeMem(PHIDReport(List[I]));
  FIntrReports.UnlockList;
  { free objects }
  FIntrReports.Free;
  FIntrEndpoint.Free;
  Inherited Destroy;
End;

(**
 * Set Report
 *
 * Sends Lenght+1 bytes as USB Control Message
 *)
Function TUSBPseudoHIDInterface.SetReport(ReportType, ReportID: Byte; const Buf; Length: LongInt): LongInt;
Var Data : PByteArray;
Begin
  GetMem(Data,Length+1);
  Data^[0] := ReportID;
  Move(Buf,Data^[1],Length);
  Result := FDevice.FControl.ControlMsg(
    USB_ENDPOINT_OUT or USB_TYPE_CLASS or USB_RECIP_INTERFACE { bmRequestType },
    USB_REQ_HID_SET_REPORT {bRequest},
    ReportType shl 8 or ReportID,   { wValue }
    0,   { wIndex }
    Data^,
    Length+1,
    100);
  FreeMem(Data);
End;

(**
 * Get Report
 *
 * Receives at most Lenght bytes as USB Control Message
 *)
Function TUSBPseudoHIDInterface.GetReport(ReportType, ReportID: Byte; var Buf; Length: LongInt): LongInt;
Begin
  Result := FDevice.FControl.ControlMsg(
    USB_ENDPOINT_IN or USB_TYPE_CLASS or USB_RECIP_INTERFACE { bmRequestType },
    USB_REQ_HID_GET_REPORT {bRequest},
    ReportType shl 8 or ReportID,   { wValue }
    0,   { wIndex }
    Buf,
    Length,
    100);
End;

Function TUSBPseudoHIDInterface.SetOutputReport(ReportID: Byte; const Buf; Length: LongInt): LongInt;
Begin
  Result := SetReport(HID_REPORT_TYPE_OUTPUT,ReportID,Buf,Length);
End;

Function TUSBPseudoHIDInterface.InterruptRead : Integer;
Const BufSize = 64;
Var Buf : Array[0..BufSize-1] of Byte;
    Report : PHIDReport;
Begin
  Result := FIntrEndpoint.Recv(Buf,BufSize,100);
  if Result <= 0 then
    Begin
//      WriteLn('usb_interrupt_read: ',Result,', usb_error_errno = ',usb_error_errno,', usb_error_str = ',usb_error_str);
      Exit;
    End;
//WriteLn('usb_interrupt_read: ',Result);
  GetMem(Report,Result);
  Move(Buf,Report^,Result);
  if (FOnIntrReport = Nil) or (FOnIntrReport(Report) = False) then  // we trust on logic optimization
    FIntrReports.Add(Report);  // add only if not yet consumed
End;

Function TUSBPseudoHIDInterface.HasReport(ReportID: Byte): PHIDReport;
Var List : TList;
    I    : Integer;
Begin
  Result := Nil;
  List := FIntrReports.LockList;
  try
    if List.Count <= 0 then
      Exit;
    For I := 0 to List.Count-1 do
      if PHIDReport(List[I])^.ReportID = ReportID then
        Exit(List[I]);
  finally
    FIntrReports.UnlockList;
  End;
End;

Procedure TUSBPseudoHIDInterface.EatReport(Report:PHIDReport);
Begin
  FIntrReports.Remove(Report);
End;

Function TUSBPseudoHIDInterface.GetReport(ReportID: Byte): PHIDReport;
Begin
  raise Exception.Create('Not implemented!');
End;

{ TUSBEndpoint }

Constructor TUSBEndpoint.Create(AEndpoint:PUSBEndpointDescriptor);
Begin
  inherited Create;
  FEndpoint  := AEndpoint;
  // TODO: in constructors of descendants check that endpoint type and direction match
End;

Destructor TUSBEndpoint.Destroy;
Begin
  Inherited Destroy;
End;

{ TUSBDeviceEndpoint }

Constructor TUSBDeviceEndpoint.Create(ADevice:TUSBDevice;AEndpoint:PUSBEndpointDescriptor);
Begin
  inherited Create(AEndpoint);
  FDevice := ADevice;
End;

{ TUSBInterfaceEndpoint }

Constructor TUSBInterfaceEndpoint.Create(AIntf:TUSBInterface;AEndpoint:PUSBEndpointDescriptor);
Begin
  inherited Create(AEndpoint);
  FInterface := AIntf;
End;

{ TUSBDeviceControlEndpoint }

Constructor TUSBDeviceControlEndpoint.Create(ADevice:TUSBDevice);
Begin
  inherited Create(ADevice,Nil);
End;

Function TUSBDeviceControlEndpoint.ControlMsg(RequestType:Byte;Request:Byte;Value:Word;Index:Word;Const Buf;Length,Timeout:LongInt):LongInt;
Begin
  Result := usb_control_msg(FDevice.FHandle,RequestType,Request,Value,Index,@Buf,Length,Timeout);
End;

Function TUSBDeviceControlEndpoint.ControlMsg(RequestType:Byte;Request:Byte;Value:Word;Index:Word;Timeout:LongInt):LongInt;
Begin
  Result := usb_control_msg(FDevice.FHandle,RequestType,Request,Value,Index,Nil,0,Timeout);
End;

Function TUSBDeviceControlEndpoint.GetString(Index:Word):String;
Begin
  SetLength(Result,64);
  SetLength(Result,usb_get_string_simple(FDevice.FHandle,Index,Result[1],Length(Result)));
End;

(**
 * Request the current device configuration
 *
 * Returns: A zero value means the device is not configured and a non-zero
 * value indicates the device is configured. It is the value of
 * bConfigurationValue of the active configuration descriptor.
 *)
Function TUSBDeviceControlEndpoint.GetConfiguration : Integer;
Var R : Integer;
    B : Byte;
Begin
  R := ControlMsg(USB_RECIP_DEVICE or USB_ENDPOINT_IN,USB_REQ_GET_CONFIGURATION,0,0,B,1,40);
  if R < 0 then Result := R
  else Result := B;
End;

{ TUSBBulkOutEndpoint }

Function TUSBBulkOutEndpoint.Send(Const Buf;Length,Timeout:LongInt):LongInt;
Begin
  Result := USB_Send(FInterface.FDevice.FHandle,FEndpoint^.bEndpointAddress,Buf,Length,Timeout);
End;

Function TUSBBulkOutEndpoint.Send(St:String;Timeout:LongInt):LongInt;
Begin
  Result := USB_Send(FInterface.FDevice.FHandle,FEndpoint^.bEndpointAddress,St,Timeout);
End;

{ TUSBBulkInEndpoint }

Function TUSBBulkInEndpoint.Recv(Out Buf;Length:LongInt;Timeout:LongInt):LongInt;
Begin
  Result := USB_Recv(FInterface.FDevice.fHandle,FEndpoint^.bEndpointAddress,Buf,Length,Timeout);
End;

{ TUSBInterruptOutEndpoint }

Function TUSBInterruptOutEndpoint.Send(Const Buf;Length,Timeout:LongInt):LongInt;
Begin
  Result := usb_interrupt_read(FInterface.FDevice.FHandle,FEndpoint^.bEndpointAddress,@Buf,Length,Timeout);
End;

Function TUSBInterruptOutEndpoint.Send(St:String;Timeout:LongInt):LongInt;
Begin
  Result := usb_interrupt_read(FInterface.FDevice.FHandle,FEndpoint^.bEndpointAddress,@St[1],Length(St),Timeout);
End;

{ TUSBInterruptInEndpoint }

Function TUSBInterruptInEndpoint.Recv(Out Buf;Length:LongInt;Timeout:LongInt):LongInt;
Begin
  Result := usb_interrupt_read(FInterface.FDevice.FHandle,FEndpoint^.bEndpointAddress,@Buf,Length,Timeout);
End;

(*****************************************************************************)
(**  Initialization  *********************************************************)
(*****************************************************************************)

Initialization
  { init libusb }
  usb_init; { no problems will occur }
  //  if Debug > 0 then usb_set_debug($FFFF);
End.

