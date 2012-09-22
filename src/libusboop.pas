(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   Object-oriented wrapper for libusb.                                   *
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
Unit LibUsbOop;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, LibUsb;

Type

  TDynByteArray = Array of Byte;
  TLibUsbDeviceArray       = Array of Plibusb_device;
  TLibUsbDeviceMatchMethod = Function(Dev:Plibusb_device) : Boolean of object;
  TLibUsbDeviceMatchFunc   = Function(Dev:Plibusb_device;Data:Pointer) : Boolean;
  (**
   * Device matcher class for TLibUsbContext.FindDevices
   *)
  TLibUsbDeviceMatchClass = class
    Function Match(Dev:Plibusb_device) : Boolean; virtual; abstract;
  End;

  { TLibUsbContext }

  TLibUsbContext = class
  private
    FContext : Plibusb_context;
  public
    Constructor Create;
    Destructor Destroy; override;
          Procedure SetDebug(Level:Integer);
    Class Function  GetVersion : Plibusb_version;
    Class Function  HasCapability(Capability : Cardinal) : Boolean;
          Function  ErrorName(ErrorCode : Integer) : PChar;
    // device list
          Function  GetDeviceList(Out List:PPlibusb_device) : cssize;
    Class Procedure FreeDeviceList(List:PPlibusb_device;UnrefDevices:Boolean=false);
          Function  FindDevices(MatchFunc :TLibUsbDeviceMatchMethod;                  Timeout:Integer=0) : TLibUsbDeviceArray;
          Function  FindDevices(MatchClass:TLibUsbDeviceMatchClass;CFree:Boolean=True;Timeout:Integer=0) : TLibUsbDeviceArray;
          Function  FindDevices(MatchFunc :TLibUsbDeviceMatchFunc;Data:Pointer;       Timeout:Integer=0) : TLibUsbDeviceArray;
    // device handling
    Class Function  RefDevice(dev:Plibusb_device):Plibusb_device;
    Class Procedure UnrefDevice(dev:Plibusb_device);
    Class Function  GetDeviceDescriptor(dev : Plibusb_device) : libusb_device_descriptor;
    Class Function  GetActiveConfigDescriptor(dev : Plibusb_device) : Plibusb_config_descriptor;
    Class Function  GetConfigDescriptor(dev : Plibusb_device; ConfigIndex : Byte) : Plibusb_config_descriptor;
    Class Function  GetConfigDescriptorByValue(dev : Plibusb_device; bConfigurationValue : Byte) : Plibusb_config_descriptor;
    Class Procedure FreeConfigDescriptor(config : Plibusb_config_descriptor);
    Class Function  GetBusNumber(dev : Plibusb_device) : Byte;
    Class Function  GetPortNumber(dev : Plibusb_device) : Byte;
    Class Function  GetParent(dev : Plibusb_device) : Plibusb_device;
          Function  GetPortPath(dev : Plibusb_device; path : PByteArray; path_len:Integer) : Byte;
          Function  GetPortPath(dev : Plibusb_device) : TDynByteArray;
    Class Function  GetDeviceAddress(dev : Plibusb_device) : Byte;
    Class Function  GetDeviceSpeed(dev : Plibusb_device) : Byte;
    Class Function  GetMaxPacketSize(dev : Plibusb_device; endpoint : Byte) : Integer;
    Class Function  GetMaxIsoPacketSize(dev : Plibusb_device; endpoint : Byte) : Integer;
    // locking
          Function  TryLockEvents : Integer;
          Procedure LockEvents;
          Procedure UnlockEvents;
          Function  EventHandlingOk : Integer;
          Function  EventHandlerActive : Integer;
          Procedure LockEventWaiters;
          Procedure UnlockEventWaiters;
          Function  WaitForEvent(tv:Ptimeval) : Integer;
          Function  WaitForEvent : Integer;
          Function  WaitForEvent(Sec,USec:Integer) : Integer;
    // event handling and timeout
          Function  HandleEvents : Integer;
          Function  HandleEvents(tv:Ptimeval) : Integer;
          Function  HandleEvents(Sec,USec:Integer) : Integer;
          Function  HandleEvents(completed:PInteger) : Integer;
          Function  HandleEvents(tv:Ptimeval;completed:PInteger) : Integer;
          Function  HandleEvents(Sec,USec:Integer;completed:PInteger) : Integer;
          Function  HandleEventsLocked( tv:Ptimeval) : Integer;
          Function  PollfdsHandleTimeouts : Integer;
          Function  GetNextTimeout( tv:Ptimeval) : Integer;
    // polling
          Function  GetPollFDs:PPlibusb_pollfd;     (* Const before type ignored *)
          Procedure SetPollFDNotifiers(added_cb:libusb_pollfd_added_cb; removed_cb:libusb_pollfd_removed_cb; user_data:pointer);
    // special functions
          Function  GetSerialNumber(dev : Plibusb_device) : String;
    property Context : Plibusb_context read FContext;
  End;

  TLibUsbDeviceControlEndpoint = class;  // forward declaration

  { TLibUsbDevice }

  TLibUsbDevice = class   { this corresponds to libusb_device_handle }
  private
    FContext : TLibUsbContext;
    FDevice  : Plibusb_device;
    FHandle  : Plibusb_device_handle;
    FControl : TLibUsbDeviceControlEndpoint;
  public
    Constructor Create(AContext:TLibUsbContext;ADevice:Plibusb_device);
    Constructor Create(AContext:TLibUsbContext;AVID,APID:Word);
    Destructor Destroy; override;
    Function  GetConfiguration : Integer;
    Procedure SetConfiguration(AConfiguration : Integer);
    Procedure Reset;
    Function IsPresent : Boolean;
    property Device  : Plibusb_device read FDevice;
    property Handle  : Plibusb_device_handle read FHandle;
    property Control : TLibUsbDeviceControlEndpoint read FControl;
  End;

  { TLibUsbInterface }

  TLibUsbInterface = class
  private
    FDevice     : TLibUsbDevice;
    FInterface  : Integer;
    FAltSetting : Integer;
    Procedure Claim;
    Procedure Release;
    Function  IsKernelDriverActive : Boolean;
    Procedure DetachKernelDriver;
    Procedure AttachKernelDriver;
  public
    Constructor Create(ADevice:TLibUsbDevice;AInterface:Integer);
    Destructor Destroy; override;
    Procedure SetAltSetting(AAltSetting:Integer);
  End;

  TLibUsbTransfer = class;

  { TLibUsbEndpoint }

  TLibUsbEndpoint = class
  protected
    FDevice   : TLibUsbDevice;
    FEndpoint : Byte;
  public
    Constructor Create(ADevice:TLibUsbDevice;AEndpoint:Byte);
    Destructor Destroy; override;
    Procedure ClearHalt;
  End;

  { TLibUsbDeviceEndpoint }

  TLibUsbDeviceEndpoint = class(TLibUsbEndpoint)
  public
    Constructor Create(ADevice:TLibUsbDevice;AEndpoint:Byte);
  End;

  { TLibUsbInterfaceEndpoint }

  TLibUsbInterfaceEndpoint = class(TLibUsbEndpoint)
  protected
    FInterface : TLibUsbInterface;
  public
    Constructor Create(AIntf:TLibUsbInterface;AEndpoint:Byte);
  End;

  { TLibUsbDeviceControlEndpoint }

  TLibUsbDeviceControlEndpoint = class(TLibUsbDeviceEndpoint)
    Constructor Create(ADevice:TLibUsbDevice); overload;
    Function  ControlMsg(RequestType:Byte;Request:Byte;Value:Word;Index:Word;Const Buf;Length,Timeout:LongInt):LongInt;
    Function  ControlMsg(RequestType:Byte;Request:Byte;Value:Word;Index:Word;Buf:TDynByteArray;Timeout:LongInt):LongInt;
    Function  ControlMsg(RequestType:Byte;Request:Byte;Value:Word;Index:Word;Length,Timeout:LongInt):TDynByteArray;
    Function  ControlMsg(RequestType:Byte;Request:Byte;Value:Word;Index:Word;Timeout:LongInt):LongInt;
    Function  GetDescriptor(DescType:Byte;DescIndex:Byte;Out Data;Length:Integer) : Integer;
    Function  GetString     (DescIndex:Byte;LangID:Word;Out Data;Length:Integer) : Integer;
    Function  GetStringAscii(DescIndex:Byte;Out Data;Length:Integer) : Integer;
    Function  GetString     (Index:Byte):String;
  End;

  { TLibUsbBulkOutEndpoint }

  TLibUsbBulkOutEndpoint = class(TLibUsbInterfaceEndpoint)
    Function Send(Const Buf;Length,Timeout:LongInt):LongInt;
    Function Send(Buf:TDynByteArray;Timeout:LongInt):LongInt;
  End;

  { TLibUsbBulkInEndpoint }

  TLibUsbBulkInEndpoint = class(TLibUsbInterfaceEndpoint)
    Function Recv(Out Buf;Length:LongInt;Timeout:LongInt) : LongInt;
    Function Recv(Length:LongInt;Timeout:LongInt):TDynByteArray;
  End;

  { TLibUsbInterruptOutEndpoint }

  TLibUsbInterruptOutEndpoint = class(TLibUsbInterfaceEndpoint)
    Function Send(Const Buf;Length,Timeout:LongInt):LongInt;
    Function Send(Buf:TDynByteArray;Timeout:LongInt):LongInt;
  End;

  { TLibUsbInterruptInEndpoint }

  TLibUsbInterruptInEndpoint = class(TLibUsbInterfaceEndpoint)
    Function Recv(Out Buf;Length:LongInt;Timeout:LongInt) : LongInt;
    Function Recv(Length:LongInt;Timeout:LongInt):TDynByteArray;
  End;

  { TLibUsbTransfer }

  TLibUsbTransferObjCallback  = Procedure(Transfer:TLibUsbTransfer) of object;
  TLibUsbTransferProcCallback = Procedure(Transfer:TLibUsbTransfer;Data:Pointer);
  TLibUsbTransfer = class    // don't use directly
  private
    FObjCallback  : TLibUsbTransferObjCallback;
    FProcCallback : TLibUsbTransferProcCallback;
    FProcCbData   : Pointer;
    FException    : Exception;
    Function  GetFlags : Byte;
    Procedure SetFlags(Const AFlags : Byte);
    Function  GetTimeout : Cardinal;
    Procedure SetTimeout(Const ATimeout : Cardinal);
    Function  GetStatus : Integer;
    Function  GetActualLength : Integer;
  protected
    FTransfer : Plibusb_transfer;
    FEndpoint : TLibUsbEndpoint;
    Constructor Create(AEndpoint:TLibUsbEndpoint;AIsoPackets:Integer);
    Procedure   Callback;
  public
    Destructor Destroy; override;
    Function Submit : Integer; virtual;
    Function Cancel : Integer;
    property Flags    : Byte     read GetFlags   write SetFlags;
    property Timeout  : Cardinal read GetTimeout write SetTimeout;
    property Status   : Integer  read GetStatus;
    property ActualLength : Integer read GetActualLength;
    property ObjCallback  : TLibUsbTransferObjCallback  read FObjCallback  write FObjCallback;
    property ProcCallback : TLibUsbTransferProcCallback read FProcCallback write FProcCallback;
    property ProcCbData   : Pointer                     read FProcCbData   write FProcCbData;
    property CbException  : Exception                   read FException;
  End;

  { TLibUsbControlTransfer }

  TLibUsbControlTransfer = class(TLibUsbTransfer)
  Private
    Function  GetRequestType : Byte;
    Procedure SetRequestType(Const ARequestType : Byte);
    Function  GetRequest : Byte;
    Procedure SetRequest(Const ARequest : Byte);
    Function  GetValue : Word;
    Procedure SetValue(Const AValue : Word);
    Function  GetIndex : Word;
    Procedure SetIndex(Const AIndex : Word);
    Function  GetLength : Word;
    Procedure SetLength(Const ALength : Word);
    Function  GetBuffer(Index : Word) : Byte;
    Procedure SetBuffer(Index : Word; Const AValue : Byte);
    Function  GetBufferPtr : Pointer;
  private
    FBuf : TDynByteArray;
  public
    Constructor Create(AEndpoint:TLibUsbDeviceControlEndpoint;ALength:Cardinal = 0);
    Destructor  Destroy; override;
    Procedure Setup(ARequestType:Byte;ARequest:Byte;AValue:Word;AIndex:Word;Const ABuf;ALength:Word;ATimeout:LongInt);
    Function Submit : Integer; override;
    property bmRequestType : Byte read GetRequestType write SetRequestType;
    property bRequest      : Byte read GetRequest     write SetRequest;
    property wValue        : Word read GetValue       write SetValue;
    property wIndex        : Word read GetIndex       write SetIndex;
    property wLength       : Word read GetLength      write SetLength;
    property Buffer[Index:Word] : Byte read GetBuffer write SetBuffer; default;
    property Ptr : Pointer read GetBufferPtr;
  End;

  { TLibUsbBufferTransfer }

  TLibUsbBufferTransfer = class(TLibUsbTransfer)    // don't use directly
  private
    Function GetBuffer : Pointer;
    Procedure SetBuffer(Const ABuffer : Pointer);
    Function GetLength : Cardinal;
    Procedure SetLength(Const ALength : Cardinal);
  public
    property Buffer : Pointer  read GetBuffer write SetBuffer;
    property Length : Cardinal read GetLength write SetLength;
  End;

  { TLibUsbBulkTransfer }

  TLibUsbBulkTransfer = class(TLibUsbBufferTransfer)
  public
    Constructor Create(AEndpoint:TLibUsbBulkInEndpoint);
    Constructor Create(AEndpoint:TLibUsbBulkOutEndpoint);
  End;

  { TLibUsbInterruptTransfer }

  TLibUsbInterruptTransfer = class(TLibUsbBufferTransfer)
  public
    Constructor Create(AEndpoint:TLibUsbInterruptInEndpoint);
    Constructor Create(AEndpoint:TLibUsbInterruptOutEndpoint);
  End;

  EUSBError = class(Exception);

  { ELibUsb }

  ELibUsb = class(EUSBError)
  private
    FError : Integer;
    Function GetErrorStr : PChar;
  public
    Constructor Create   (AError : Integer;Const Msg : String);
    Constructor CreateFmt(AError : Integer;Const Msg : String; Const Args : Array of Const);
    Class Function Check   (AResult:Integer;Const Msg : String) : Integer;
    Class Function CheckFmt(AResult:Integer;Const Msg : String; Const Args : Array of Const) : Integer;
    property Error : Integer read FError;
    property ErrorStr : PChar read GetErrorStr;
  End;

Implementation
Uses CTypes,PasLibUsbUtils,Math;

(**
 * Helper class to use TLibUsbDeviceMatchFunc instead of TLibUsbDeviceMatchMethod
 *)
Type
  TDeviceFuncMatcher = class(TLibUsbDeviceMatchClass)
    FFunc : TLibUsbDeviceMatchFunc;
    FData : Pointer;
    Constructor Create(AFunc:TLibUsbDeviceMatchFunc;AData:Pointer);
    Function    Match(Dev:Plibusb_device) : Boolean; override;
  End;
Constructor TDeviceFuncMatcher.Create(AFunc:TLibUsbDeviceMatchFunc;AData:Pointer);
Begin
  FFunc := AFunc;
  FData := AData;
End;
Function TDeviceFuncMatcher.Match(Dev:Plibusb_device):Boolean;
Begin
  Result := FFunc(Dev,FData);
End;

{ TLibUsbContext }

Constructor TLibUsbContext.Create;
Begin
  inherited Create;
  ELibUsb.Check(libusb_init(FContext),'Init');
End;

Destructor TLibUsbContext.Destroy;
Begin
  libusb_exit(FContext);
  Inherited Destroy;
End;

Procedure TLibUsbContext.SetDebug(Level : Integer);
Begin
  libusb_set_debug(FContext,Level);
End;

Class Function TLibUsbContext.GetVersion : Plibusb_version;
Begin
  Result := libusb_get_version;
End;

Class Function TLibUsbContext.HasCapability(Capability:Cardinal) : Boolean;
Begin
  Result := (libusb_has_capability(Capability) <> 0);
End;

Function TLibUsbContext.ErrorName(ErrorCode:Integer):PChar;
Begin
  Result := PChar(libusb_error_name(ErrorCode));
End;

Function TLibUsbContext.GetDeviceList(Out List : PPlibusb_device) : cssize;
Begin
  Result := libusb_get_device_list(FContext,List);
End;

Class Procedure TLibUsbContext.FreeDeviceList(List : PPlibusb_device; UnrefDevices : Boolean);
Begin
  libusb_free_device_list(List,Integer(UnrefDevices));
End;

(**
 * Find USB Devices according to MatchFunc
 *
 * @param MatchFunc  method to compare a given device with a criterion
 * @param Timeout    if > 0, the search will be retried Timeout milliseconds
 *
 * @returns dynamic array of found devices
 *)
Function TLibUsbContext.FindDevices(MatchFunc : TLibUsbDeviceMatchMethod;Timeout:Integer=0) : TLibUsbDeviceArray;
Var Start    : UInt64;
    DevList  : PPlibusb_device;
    DevCount : Integer;
    I        : Integer;
Begin
  SetLength(Result,0);
  Start := GetUSec;
  repeat
    DevCount := ELibUsb.Check(GetDeviceList(DevList),'GetDeviceList');
    For I := 0 to DevCount-1 do
      if MatchFunc(DevList[I]) then
        Begin
          SetLength(Result,Length(Result)+1);
          Result[Length(Result)-1] := DevList[I];
        End;
    FreeDeviceList(DevList);
    if (Length(Result) = 0) and (Timeout > 0) then
      Begin
        Sleep(Min(100,(GetUSec-Start) div 1000));  // milliseconds
      End;
  Until (Length(Result) > 0) or (GetUSec-Start > Timeout*1000);
End;

(**
 * Find USB Devices according to MatchFunc
 *
 * @param MatchClass  TLibUsbDeviceMatchClass descendent with an overridden
 *                    Match method to compare a given device with a criterion.
 * @param Free        if true (the default), MatchClass will be Free()d
 * @param Timeout     if > 0, the search will be retried Timeout milliseconds
 *
 * @returns dynamic array of found devices
 *)
Function TLibUsbContext.FindDevices(MatchClass : TLibUsbDeviceMatchClass;CFree:Boolean=True;Timeout:Integer=0) : TLibUsbDeviceArray;
Begin
  Result := FindDevices(@MatchClass.Match,Timeout);
  if CFree then
    MatchClass.Free;
End;

(**
 * Find USB Devices according to MatchFunc
 *
 * @param MatchFunc  function to compare a given device with a criterion
 * @param Data       user data pointer to be supplied to MatchFunc
 * @param Timeout    if > 0, the search will be retried Timeout milliseconds
 *
 * @returns dynamic array of found devices
 *)
Function TLibUsbContext.FindDevices(MatchFunc : TLibUsbDeviceMatchFunc; Data : Pointer;Timeout:Integer=0) : TLibUsbDeviceArray;
Begin
  Result := FindDevices(TDeviceFuncMatcher.Create(MatchFunc,Data),true,Timeout);
End;

Class Function TLibUsbContext.RefDevice(dev : Plibusb_device) : Plibusb_device;
Begin
  Result := libusb_ref_device(dev);
End;

Class Procedure TLibUsbContext.UnrefDevice(dev : Plibusb_device);
Begin
  libusb_unref_device(dev);
End;

Class Function TLibUsbContext.GetDeviceDescriptor(dev:Plibusb_device):libusb_device_descriptor;
Begin
  ELibUsb.Check(libusb_get_device_descriptor(dev,Result),'GetDeviceDescriptor');
End;

Class Function TLibUsbContext.GetActiveConfigDescriptor(dev:Plibusb_device):Plibusb_config_descriptor;
Begin
  ELibUsb.Check(libusb_get_active_config_descriptor(dev,Result),'GetActiveConfigDescriptor');
End;

Class Function TLibUsbContext.GetConfigDescriptor(dev:Plibusb_device; ConfigIndex:Byte):Plibusb_config_descriptor;
Begin
  ELibUsb.Check(libusb_get_config_descriptor(dev,ConfigIndex,Result),'GetConfigDescriptor');
End;

Class Function TLibUsbContext.GetConfigDescriptorByValue(dev:Plibusb_device; bConfigurationValue:Byte):Plibusb_config_descriptor;
Begin
  ELibUsb.Check(libusb_get_config_descriptor_by_value(dev,bConfigurationValue,Result),'GetConfigDescriptor by value');
End;

Class Procedure TLibUsbContext.FreeConfigDescriptor(config:Plibusb_config_descriptor);
Begin
  libusb_free_config_descriptor(config);
End;

Class Function TLibUsbContext.GetBusNumber(dev:Plibusb_device) : Byte;
Begin
  Result := libusb_get_bus_number(dev);
End;

Class Function  TLibUsbContext.GetPortNumber(dev:Plibusb_device) : Byte;
Begin
  Result := libusb_get_port_number(dev);
End;

(**
 * Must be called between GetDeviceList and FreeDeviceList
 *)
Class Function  TLibUsbContext.GetParent(dev:Plibusb_device) : Plibusb_device;
Begin
  Result := libusb_get_parent(dev);
End;

(**
 * PByteArray must offer at least 7 bytes (maximum depth of USB bus)
 *)
Function TLibUsbContext.GetPortPath(dev:Plibusb_device; path:PByteArray; path_len:Integer) : Byte;
Begin
  Result := ELibUsb.Check(libusb_get_port_path(FContext,dev,pcuint8(path),path_len),'GetPortPath');
End;

Function TLibUsbContext.GetPortPath(dev : Plibusb_device) : TDynByteArray;
Begin
  SetLength(Result,7);
  SetLength(Result,ELibUsb.Check(libusb_get_port_path(FContext,dev,@Result[1],Length(Result)),'GetPortPath'));
End;

Class Function TLibUsbContext.GetDeviceAddress(dev:Plibusb_device) : Byte;
Begin
  Result := libusb_get_device_address(dev);
End;

Class Function TLibUsbContext.GetDeviceSpeed(dev:Plibusb_device) : Byte;
Begin
  Result := libusb_get_device_speed(dev);
End;

Class Function TLibUsbContext.GetMaxPacketSize(dev:Plibusb_device; endpoint:Byte) : Integer;
Begin
  Result := ELibUsb.Check(libusb_get_max_packet_size(dev,endpoint),'GetMaxPacketSize');
End;

Class Function TLibUsbContext.GetMaxIsoPacketSize(dev:Plibusb_device; endpoint:Byte) : Integer;
Begin
  Result := ELibUsb.Check(libusb_get_max_iso_packet_size(dev,endpoint),'GetMaxIsoPacketSize');
End;

Function TLibUsbContext.TryLockEvents : Integer;
Begin
  Result := libusb_try_lock_events(FContext);
End;

Procedure TLibUsbContext.LockEvents;
Begin
  libusb_lock_events(FContext);
End;

Procedure TLibUsbContext.UnlockEvents;
Begin
  libusb_unlock_events(FContext);
End;

Function TLibUsbContext.EventHandlingOk : Integer;
Begin
  Result := libusb_event_handling_ok(FContext);
End;

Function TLibUsbContext.EventHandlerActive : Integer;
Begin
  Result := libusb_event_handler_active(FContext);
End;

Procedure TLibUsbContext.LockEventWaiters;
Begin
  libusb_lock_event_waiters(FContext);
End;

Procedure TLibUsbContext.UnlockEventWaiters;
Begin
  libusb_unlock_event_waiters(FContext);
End;

Function TLibUsbContext.WaitForEvent(tv : Ptimeval) : Integer;
Begin
  Result := libusb_wait_for_event(FContext,tv);
End;

Function TLibUsbContext.WaitForEvent : Integer;
Begin
  Result := libusb_wait_for_event(FContext,Nil);
End;

Function TLibUsbContext.WaitForEvent(Sec, USec : Integer) : Integer;
Var tv : timeval;
Begin
  tv.tv_sec  := Sec;
  tv.tv_usec := USec;
  Result := libusb_wait_for_event(FContext,@tv);
End;

Function TLibUsbContext.HandleEvents : Integer;
Begin
  Result := libusb_handle_events(FContext);
End;

Function TLibUsbContext.HandleEvents(tv : Ptimeval) : Integer;
Begin
  Result := libusb_handle_events_timeout(FContext,tv);
End;

Function TLibUsbContext.HandleEvents(Sec, USec : Integer) : Integer;
Var tv : timeval;
Begin
  tv.tv_sec  := Sec;
  tv.tv_usec := USec;
  Result := libusb_handle_events_timeout(FContext,@tv);
End;

Function TLibUsbContext.HandleEvents(completed : PInteger) : Integer;
Begin
  Result := libusb_handle_events_completed(FContext,completed);
End;

Function TLibUsbContext.HandleEvents(tv : Ptimeval;completed : PInteger) : Integer;
Begin
  Result := libusb_handle_events_timeout_completed(FContext,tv,completed);
End;

Function TLibUsbContext.HandleEvents(Sec, USec : Integer;completed : PInteger) : Integer;
Var tv : timeval;
Begin
  tv.tv_sec  := Sec;
  tv.tv_usec := USec;
  Result := libusb_handle_events_timeout_completed(FContext,@tv,completed);
End;

Function TLibUsbContext.HandleEventsLocked(tv : Ptimeval) : Integer;
Begin
  Result := libusb_handle_events_locked(FContext,tv);
End;

Function TLibUsbContext.PollfdsHandleTimeouts : Integer;
Begin
  Result := libusb_pollfds_handle_timeouts(FContext);
End;

Function TLibUsbContext.GetNextTimeout(tv : Ptimeval) : Integer;
Begin
  Result := libusb_get_next_timeout(FContext,tv);
End;

Function TLibUsbContext.GetPollFDs : PPlibusb_pollfd;
Begin
  Result := libusb_get_pollfds(FContext);
End;

Procedure TLibUsbContext.SetPollFDNotifiers(added_cb : libusb_pollfd_added_cb;removed_cb : libusb_pollfd_removed_cb; user_data : pointer);
Begin
  libusb_set_pollfd_notifiers(FContext,added_cb,removed_cb,user_data);
End;

Function TLibUsbContext.GetSerialNumber(dev : Plibusb_device) : String;
Var Handle : Plibusb_device_handle;
    DeviceDescriptor : libusb_device_descriptor;
    iSerialNumber : Byte;
Begin
  Result := '';
  DeviceDescriptor := GetDeviceDescriptor(dev);
  iSerialNumber := DeviceDescriptor.iSerialNumber;
  if iSerialNumber = 0 then
    Exit('');
  ELibUsb.Check(libusb_open(dev,Handle),'Open');
  SetLength(Result,256);
  try
    SetLength(Result,ELibUsb.Check(libusb_get_string_descriptor_ascii(Handle,iSerialNumber,@Result[1],Length(Result)),'GetString'));
  Finally
    libusb_close(Handle);
  End;
End;

{ TLibUsbDevice }

Constructor TLibUsbDevice.Create(AContext : TLibUsbContext; ADevice : Plibusb_device);
Begin
  inherited Create;
  FContext := AContext;
  FDevice  := ADevice;
  ELibUsb.Check(libusb_open(FDevice,FHandle),'Open');
  FControl := TLibUsbDeviceControlEndpoint.Create(Self);
End;

Constructor TLibUsbDevice.Create(AContext : TLibUsbContext; AVID, APID : Word);
Begin
  inherited Create;
  FContext := AContext;
  FHandle := libusb_open_device_with_vid_pid(FContext.Context,AVID,APID);
  if FHandle = Nil then
    raise ELibUsb.CreateFmt(Integer(LIBUSB_ERROR_NO_DEVICE),'Couldn''t find or open device %.4x:%.4x',[AVID,APID]);
  FDevice := libusb_get_device(FHandle);
  FControl := TLibUsbDeviceControlEndpoint.Create(Self);
End;

Destructor TLibUsbDevice.Destroy;
Begin
  FControl.Free;
  libusb_close(FHandle);
  inherited Destroy;
End;

(**
 * Request the current device configuration
 *
 * Returns: A zero value means the device is not configured and a non-zero
 * value indicates the device is configured. It is the value of
 * bConfigurationValue of the active configuration descriptor.
 *)
Function TLibUsbDevice.GetConfiguration : Integer;
Begin
  ELibUsb.Check(libusb_get_configuration(FHandle,Result),'GetConfiguration');
End;

Procedure TLibUsbDevice.SetConfiguration(AConfiguration:Integer);
Begin
  ELibUsb.Check(libusb_set_configuration(FHandle,AConfiguration),'SetConfiguration');
End;

Procedure TLibUsbDevice.Reset;
Begin
  ELibUsb.Check(libusb_reset_device(FHandle),'Reset');
End;

(**
 * Check if the device is still connected
 *)
Function TLibUsbDevice.IsPresent:Boolean;
Var Status : Word;
    R      : Integer;
Begin
  R := FControl.ControlMsg(
    { bmRequestType } LIBUSB_ENDPOINT_IN or LIBUSB_REQUEST_TYPE_STANDARD or LIBUSB_RECIPIENT_DEVICE,
    { bRequest      } LIBUSB_REQUEST_GET_STATUS,
    { wValue        } 0,
    { wIndex        } 0,
    { Buf           } Result,
    { wLength       } 2,
    { Timeout       } 40);
  Result := (R = 2);
End;

{ TLibUsbInterface }

Constructor TLibUsbInterface.Create(ADevice : TLibUsbDevice; AInterface : Integer);
Begin
  inherited Create;
  FDevice     := ADevice;
  FInterface  := AInterface;
  FAltSetting := -1;   // unset

  Claim;
End;

Destructor TLibUsbInterface.Destroy;
Begin
  Release;
  Inherited Destroy;
End;

Procedure TLibUsbInterface.SetAltSetting(AAltSetting : Integer);
Begin
  FAltSetting := AAltSetting;
  ELibUsb.Check(libusb_set_interface_alt_setting(FDevice.FHandle,FInterface,FAltSetting),'SetAltSetting');
End;

Procedure TLibUsbInterface.Claim;
Var Ret : Integer;
Begin
  { claim interface }
  Ret := libusb_claim_interface(FDevice.FHandle,FInterface);
  if Ret >= 0 then
    Exit;   // everything ok
  if Ret <> LIBUSB_ERROR_BUSY then
    ELibUsb.Create(Ret,'Claim');   // any other error except BUSY
  { device or resource busy, this is probably because the interface is
    clamied by another driver, e.g. the kernel }
  if not IsKernelDriverActive then
    ELibUsb.CreateFmt(Ret,'Couldn''t claim interface %d but there is no kernel driver.',[FInterface]);
  { detach driver }
  DetachKernelDriver;
  { claim again }
  ELibUsb.Check(libusb_claim_interface(FDevice.FHandle,FInterface),'2nd Claim');
End;

Procedure TLibUsbInterface.Release;
Begin
  ELibUsb.Check(libusb_release_interface(FDevice.FHandle,FInterface),'Release');
End;

Function TLibUsbInterface.IsKernelDriverActive : Boolean;
Begin
  Result := (ELibUsb.Check(libusb_kernel_driver_active(FDevice.FHandle,FInterface),'IsKernelDriverActive') > 0);
End;

Procedure TLibUsbInterface.DetachKernelDriver;
Begin
  ELibUsb.Check(libusb_detach_kernel_driver(FDevice.FHandle,FInterface),'DetachKernelDriver');
End;

Procedure TLibUsbInterface.AttachKernelDriver;
Begin
  ELibUsb.Check(libusb_attach_kernel_driver(FDevice.FHandle,FInterface),'AttachKernelDriver');
End;

{ TLibUsbEndpoint }

Constructor TLibUsbEndpoint.Create(ADevice : TLibUsbDevice; AEndpoint : Byte);
Begin
  inherited Create;
  FDevice   := ADevice;
  FEndpoint := AEndpoint;
End;

Destructor TLibUsbEndpoint.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TLibUsbEndpoint.ClearHalt;
Begin
  ELibUsb.Check(libusb_clear_halt(FDevice.FHandle,FEndpoint),'ClearHalt');
End;

{ TLibUsbDeviceEndpoint }

Constructor TLibUsbDeviceEndpoint.Create(ADevice:TLibUsbDevice;AEndpoint:Byte);
Begin
  inherited Create(ADevice,AEndpoint);
End;

{ TLibUsbInterfaceEndpoint }

Constructor TLibUsbInterfaceEndpoint.Create(AIntf:TLibUsbInterface;AEndpoint:Byte);
Begin
  inherited Create(AIntf.FDevice,AEndpoint);
  FInterface := AIntf;
End;

{ TLibUsbDeviceControlEndpoint }

Constructor TLibUsbDeviceControlEndpoint.Create(ADevice:TLibUsbDevice);
Begin
  inherited Create(ADevice,0);
End;

Function TLibUsbDeviceControlEndpoint.ControlMsg(RequestType:Byte;Request:Byte;Value:Word;Index:Word;Const Buf;Length,Timeout:LongInt):LongInt;
Begin
  Result := libusb_control_transfer(FDevice.Handle,RequestType,Request,Value,Index,@Buf,Length,Timeout);
End;

Function TLibUsbDeviceControlEndpoint.ControlMsg(RequestType:Byte;Request:Byte;Value:Word;Index:Word;Buf:TDynByteArray;Timeout:LongInt):LongInt;
Begin
  Result := libusb_control_transfer(FDevice.Handle,RequestType,Request,Value,Index,@(Buf[0]),Length(Buf),Timeout);
End;

Function TLibUsbDeviceControlEndpoint.ControlMsg(RequestType:Byte;Request:Byte;Value:Word;Index:Word;Length,Timeout:LongInt):TDynByteArray;
Begin
  // allocate buffer
  SetLength(Result,Length);
  // set buffer length to actual number of received bytes
  SetLength(Result,ELibUsb.Check(libusb_control_transfer(FDevice.Handle,RequestType,Request,Value,Index,@(Result[0]),Length,Timeout),'ControlMsg'));
End;

Function TLibUsbDeviceControlEndpoint.ControlMsg(RequestType:Byte;Request:Byte;Value:Word;Index:Word;Timeout:LongInt):LongInt;
Begin
  Result := libusb_control_transfer(FDevice.Handle,RequestType,Request,Value,Index,Nil,0,Timeout);
End;

Function TLibUsbDeviceControlEndpoint.GetDescriptor(DescType : Byte; DescIndex : Byte; Out Data; Length : Integer) : Integer;
Begin
  Result := libusb_get_descriptor(FDevice.Handle,DescType,DescIndex,@Data,Length);
End;

Function TLibUsbDeviceControlEndpoint.GetString(DescIndex : Byte; LangID : Word; Out Data; Length : Integer) : Integer;
Begin
  Result := libusb_get_string_descriptor(FDevice.Handle,DescIndex,LangID,@Data,Length);
End;

Function TLibUsbDeviceControlEndpoint.GetStringAscii(DescIndex : Byte; Out Data; Length : Integer) : Integer;
Begin
  Result := libusb_get_string_descriptor_ascii(FDevice.Handle,DescIndex,@Data,Length);
End;

Function TLibUsbDeviceControlEndpoint.GetString(Index : Byte) : String;
Begin
  if Index = 0 then
    Exit('');
  SetLength(Result,256);
  SetLength(Result,ELibUsb.Check(libusb_get_string_descriptor_ascii(FDevice.Handle,Index,@Result[1],Length(Result)),'GetString'));
End;

{ TLibUsbBulkOutEndpoint }

Function TLibUsbBulkOutEndpoint.Send(Const Buf;Length,Timeout:LongInt):LongInt;
Begin
  Result := libusb_bulk_transfer(FDevice.Handle,FEndpoint,@Buf,Length,Length,Timeout);
  if Result = 0 then Result := Length;
End;

Function TLibUsbBulkOutEndpoint.Send(Buf:TDynByteArray;Timeout:LongInt):LongInt;
Var Transferred : Integer;
Begin
  Result := libusb_bulk_transfer(FDevice.Handle,FEndpoint,@(Buf[0]),Length(Buf),Transferred,Timeout);
  if Result = 0 then Result := Transferred;
End;

{ TLibUsbBulkInEndpoint }

Function TLibUsbBulkInEndpoint.Recv(Out Buf;Length:LongInt;Timeout:LongInt):LongInt;
Begin
  Result := libusb_bulk_transfer(FDevice.Handle,FEndpoint,@Buf,Length,Length,Timeout);
  if Result = 0 then Result := Length;
End;

Function TLibUsbBulkInEndpoint.Recv(Length:LongInt;Timeout:LongInt):TDynByteArray;
Begin
  // allocate buffer
  SetLength(Result,Length);
  // set buffer length to actual number of received bytes
  SetLength(Result,ELibUsb.Check(libusb_bulk_transfer(FDevice.Handle,FEndpoint,@(Result[0]),Length,Length,Timeout),'Recv'));
End;

{ TLibUsbInterruptOutEndpoint }

Function TLibUsbInterruptOutEndpoint.Send(Const Buf;Length,Timeout:LongInt):LongInt;
Begin
  Result := libusb_interrupt_transfer(FDevice.Handle,FEndpoint,@Buf,Length,Length,Timeout);
  if Result = 0 then Result := Length;
End;

Function TLibUsbInterruptOutEndpoint.Send(Buf:TDynByteArray;Timeout:LongInt):LongInt;
Var Transferred : Integer;
Begin
  Result := libusb_interrupt_transfer(FDevice.Handle,FEndpoint,@(Buf[0]),Length(Buf),Transferred,Timeout);
  if Result = 0 then Result := Transferred;
End;

{ TLibUsbInterruptInEndpoint }

Function TLibUsbInterruptInEndpoint.Recv(Out Buf;Length:LongInt;Timeout:LongInt):LongInt;
Begin
  Result := libusb_interrupt_transfer(FDevice.Handle,FEndpoint,@Buf,Length,Length,Timeout);
  if Result = 0 then Result := Length;
End;

Function TLibUsbInterruptInEndpoint.Recv(Length:LongInt;Timeout:LongInt):TDynByteArray;
Begin
  // allocate buffer
  SetLength(Result,Length);
  // set buffer length to actual number of received bytes
  SetLength(Result,ELibUsb.Check(libusb_interrupt_transfer(FDevice.Handle,FEndpoint,@(Result[0]),Length,Length,Timeout),'Recv'));
End;

{ TLibUsbTransfer }

Procedure LibUsbTransferCallback(transfer:Plibusb_transfer); cdecl;
Var MyTransfer : TLibUsbTransfer;
Begin
  MyTransfer := TLibUsbTransfer(transfer^.user_data);
  if not assigned(MyTransfer) then Exit;
  MyTransfer.Callback;
End;

Constructor TLibUsbTransfer.Create(AEndpoint : TLibUsbEndpoint;AIsoPackets:Integer);
Begin
  inherited Create;
  FEndpoint := AEndpoint;
  FTransfer := libusb_alloc_transfer(AIsoPackets);
  FTransfer^.dev_handle := FEndpoint.FDevice.Handle;
  FTransfer^.endpoint   := FEndpoint.FEndpoint;
  FTransfer^.timeout    := 1000;   // set some default value
  FTransfer^.callback   := @LibUsbTransferCallback;
  FTransfer^.user_data  := Self;
End;

Procedure TLibUsbTransfer.Callback;
Begin
  try
    if assigned(FObjCallback) then
      FObjCallback(Self)
    else if assigned(FProcCallback) then
      FProcCallback(Self,FProcCbData);
  except
    on E : Exception do
      Begin
        // catch exception, otherwise program execution would hang
        WriteLn(StdErr,'Exception caught in callback: ',E.Message);
        DumpExceptionBackTrace(StdErr);
        // increment reference count of exception and store the object
        FException := Exception(AcquireExceptionObject);
      End;
  End;
End;

Destructor TLibUsbTransfer.Destroy;
Begin
  libusb_free_transfer(FTransfer);
  Inherited Destroy;
End;

Function TLibUsbTransfer.Submit : Integer;
Begin
  // clear exception
  FreeAndNil(FException);
  // submit the transfer
  Result := libusb_submit_transfer(FTransfer);
End;

Function TLibUsbTransfer.Cancel : Integer;
Begin
  Result := libusb_cancel_transfer(FTransfer);
End;

Function TLibUsbTransfer.GetFlags : Byte;
Begin
  Result := FTransfer^.flags;
End;

Procedure TLibUsbTransfer.SetFlags(Const AFlags : Byte);
Begin
  FTransfer^.flags := AFlags;
End;

Function TLibUsbTransfer.GetTimeout : Cardinal;
Begin
  Result := FTransfer^.timeout;
End;

Procedure TLibUsbTransfer.SetTimeout(Const ATimeout : Cardinal);
Begin
  FTransfer^.timeout := ATimeout;
End;

Function TLibUsbTransfer.GetStatus : Integer;
Begin
  Result := FTransfer^.status;
End;

Function TLibUsbTransfer.GetActualLength : Integer;
Begin
  Result := FTransfer^.actual_length;
End;

{ TLibUsbControlTransfer }

Constructor TLibUsbControlTransfer.Create(AEndpoint : TLibUsbDeviceControlEndpoint;ALength:Cardinal);
Begin
  inherited Create(AEndpoint,0);
  FTransfer^._type := LIBUSB_TRANSFER_TYPE_CONTROL;
  SetLength(ALength);
End;

Destructor TLibUsbControlTransfer.Destroy;
Begin
  // free buffer
  System.SetLength(FBuf,0);
  Inherited Destroy;
End;

Procedure TLibUsbControlTransfer.Setup(ARequestType : Byte; ARequest : Byte;AValue : Word; AIndex : Word; Const ABuf; ALength : Word; ATimeout : LongInt);
Begin
  // allocate buffer
  SetLength(ALength);
  With Plibusb_control_setup(@(FBuf[0]))^ do
    Begin
      bmRequestType := ARequestType;
      bRequest      := ARequest;
      wValue        := AValue;
      wIndex        := AIndex;
    End;
  // copy buffer
  Move(ABuf,FBuf[sizeof(libusb_control_setup)],ALength);
  Timeout := ATimeout;
End;

Function TLibUsbControlTransfer.Submit : Integer;
Begin
  // set direction according to bmRequestType
  FTransfer^.endpoint := FEndpoint.FEndpoint or (bmRequestType and $80);
  // dynamic array might change its location in memory, so put its current address to FTransfer
  FTransfer^.buffer := @(FBuf[0]);
  Result := Inherited Submit;
End;

Function TLibUsbControlTransfer.GetRequestType : Byte;
Begin
  Result := Plibusb_control_setup(@(FBuf[0]))^.bmRequestType;
End;

Procedure TLibUsbControlTransfer.SetRequestType(Const ARequestType : Byte);
Begin
  Plibusb_control_setup(@(FBuf[0]))^.bmRequestType := ARequestType;
End;

Function TLibUsbControlTransfer.GetRequest : Byte;
Begin
  Result := Plibusb_control_setup(@(FBuf[0]))^.bRequest;
End;

Procedure TLibUsbControlTransfer.SetRequest(Const ARequest : Byte);
Begin
  Plibusb_control_setup(@(FBuf[0]))^.bRequest := ARequest;
End;

Function TLibUsbControlTransfer.GetValue : Word;
Begin
  Result := Plibusb_control_setup(@(FBuf[0]))^.wValue;
End;

Procedure TLibUsbControlTransfer.SetValue(Const AValue : Word);
Begin
  Plibusb_control_setup(@(FBuf[0]))^.wValue := AValue;
End;

Function TLibUsbControlTransfer.GetIndex : Word;
Begin
  Result := Plibusb_control_setup(@(FBuf[0]))^.wIndex;
End;

Procedure TLibUsbControlTransfer.SetIndex(Const AIndex : Word);
Begin
  Plibusb_control_setup(@(FBuf[0]))^.wIndex := AIndex;
End;

Function TLibUsbControlTransfer.GetLength : Word;
Begin
  Result := Plibusb_control_setup(@(FBuf[0]))^.wLength;
End;

Procedure TLibUsbControlTransfer.SetLength(Const ALength : Word);
Begin
  // allocate setup packet + data
  FTransfer^.length := SizeOf(libusb_control_setup) + ALength;
  System.SetLength(FBuf,FTransfer^.length);
  Plibusb_control_setup(@(FBuf[0]))^.wLength := ALength;
End;

Function TLibUsbControlTransfer.GetBuffer(Index : Word) : Byte;
Begin
  if Index >= Plibusb_control_setup(@(FBuf[0]))^.wLength then
    raise ELibUsb.CreateFmt(LIBUSB_ERROR_OTHER,'Can''t access buffer at %d, out of range',[Index]);
  Result := FBuf[SizeOf(libusb_control_setup) + Index];
End;

Procedure TLibUsbControlTransfer.SetBuffer(Index : Word; Const AValue : Byte);
Begin
  if Index >= Plibusb_control_setup(@(FBuf[0]))^.wLength then
    raise ELibUsb.CreateFmt(LIBUSB_ERROR_OTHER,'Can''t access buffer at %d, out of range',[Index]);
  FBuf[SizeOf(libusb_control_setup) + Index] := AValue;
End;

Function TLibUsbControlTransfer.GetBufferPtr : Pointer;
Begin
  Result := @(FBuf[SizeOf(libusb_control_setup)]);
End;

{ TLibUsbBufferTransfer }

Function TLibUsbBufferTransfer.GetBuffer : Pointer;
Begin
  Result := FTransfer^.buffer;
End;

Procedure TLibUsbBufferTransfer.SetBuffer(Const ABuffer : Pointer);
Begin
  FTransfer^.buffer := ABuffer;
End;

Function TLibUsbBufferTransfer.GetLength : Cardinal;
Begin
  Result := FTransfer^.length;
End;

Procedure TLibUsbBufferTransfer.SetLength(Const ALength : Cardinal);
Begin
  FTransfer^.length := ALength;
End;

{ TLibUsbBulkTransfer }

Constructor TLibUsbBulkTransfer.Create(AEndpoint : TLibUsbBulkInEndpoint);
Begin
  inherited Create(AEndpoint,0);
  FTransfer^._type := LIBUSB_TRANSFER_TYPE_BULK;
End;

Constructor TLibUsbBulkTransfer.Create(AEndpoint : TLibUsbBulkOutEndpoint);
Begin
  inherited Create(AEndpoint,0);
  FTransfer^._type := LIBUSB_TRANSFER_TYPE_BULK;
End;

{ TLibUsbInterruptTransfer }

Constructor TLibUsbInterruptTransfer.Create(AEndpoint : TLibUsbInterruptInEndpoint);
Begin
  inherited Create(AEndpoint,0);
  FTransfer^._type := LIBUSB_TRANSFER_TYPE_INTERRUPT;
End;

Constructor TLibUsbInterruptTransfer.Create(AEndpoint : TLibUsbInterruptOutEndpoint);
Begin
  inherited Create(AEndpoint,0);
  FTransfer^._type := LIBUSB_TRANSFER_TYPE_INTERRUPT;
End;

{ ELibUsb }

Constructor ELibUsb.Create(AError : Integer; Const Msg : String);
Begin
  inherited Create(Msg);
  FError := AError;
End;

Constructor ELibUsb.CreateFmt(AError : Integer; Const Msg : String; Const Args : Array Of Const);
Begin
  inherited CreateFmt(Msg,Args);
  FError := AError;
End;

Class Function ELibUsb.Check(AResult : Integer; Const Msg : String) : Integer;
Begin
  if AResult < 0 then
    raise ELibUsb.Create(AResult,Msg);
  Result := AResult;
End;

Class Function ELibUsb.CheckFmt(AResult : Integer; Const Msg : String; Const Args : Array Of Const) : Integer;
Begin
  if AResult < 0 then
    raise ELibUsb.CreateFmt(AResult,Msg,Args);
  Result := AResult;
End;

Function ELibUsb.GetErrorStr : PChar;
Begin
  Result := PChar(libusb_error_name(FError));
End;

End.

