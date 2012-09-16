Unit LibUsbOop;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, LibUsb;

Type

  TDynByteArray = Array of Byte;

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
          Function  GetDeviceList(Out List:PPlibusb_device) : cssize;
    Class Procedure FreeDeviceList(List:PPlibusb_device;UnrefDevices:Boolean=false);
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
    property Context : Plibusb_context read FContext;
  End;

  { TLibUsbDevice }

  TLibUsbDevice = class   { this corresponds to libusb_device_handle }
  private
    FContext : TLibUsbContext;
    FDevice  : Plibusb_device;
    FHandle  : Plibusb_device_handle;
  public
    Constructor Create(AContext:TLibUsbContext;ADevice:Plibusb_device);
    Constructor Create(AContext:TLibUsbContext;AVID,APID:Word);
    Destructor Destroy; override;
    Function  GetConfiguration : Integer;
    Procedure SetConfiguration(AConfiguration : Integer);
    Procedure Reset;
    property Device : Plibusb_device read FDevice;
    property Handle : Plibusb_device_handle read FHandle;
  End;

  { TLibUsbInterface }

  TLibUsbInterface = class
  private
    FDevice     : TLibUsbDevice;
    FNum        : Integer;
    FAltSetting : Integer;
    Procedure Claim;
    Procedure Release;
    Function  IsKernelDriverActive : Boolean;
    Procedure DetachKernelDriver;
    Procedure AttachKernelDriver;
  public
    Constructor Create(ADevice:TLibUsbDevice;ANum:Integer);
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
    Function  ControlMsg(RequestType:Byte;Request:Byte;Value:Word;Index:Word;Timeout:LongInt):LongInt;
    Function  GetDescriptor(DescType:Byte;DescIndex:Byte;Out Data;Length:Integer) : Integer;
    Function  GetString     (DescIndex:Byte;LangID:Word;Out Data;Length:Integer) : Integer;
    Function  GetStringAscii(DescIndex:Byte;Out Data;Length:Integer) : Integer;
    Function  GetString     (Index:Byte):String;
  End;

  { TLibUsbBulkOutEndpoint }

  TLibUsbBulkOutEndpoint = class(TLibUsbInterfaceEndpoint)
    Function Send(Const Buf;Length,Timeout:LongInt):LongInt;
    Function Send(St:String;Timeout:LongInt):LongInt;
  End;

  { TLibUsbBulkInEndpoint }

  TLibUsbBulkInEndpoint = class(TLibUsbInterfaceEndpoint)
    Function Recv(Out Buf;Length:LongInt;Timeout:LongInt) : LongInt;
  End;

  { TLibUsbInterruptOutEndpoint }

  TLibUsbInterruptOutEndpoint = class(TLibUsbInterfaceEndpoint)
    Function Send(Const Buf;Length,Timeout:LongInt):LongInt;
    Function Send(St:String;Timeout:LongInt):LongInt;
  End;

  { TLibUsbInterruptInEndpoint }

  TLibUsbInterruptInEndpoint = class(TLibUsbInterfaceEndpoint)
    Function Recv(Out Buf;Length:LongInt;Timeout:LongInt) : LongInt;
  End;

  { TLibUsbTransfer }

  TLibUsbTransferCallback = Procedure(Transfer:TLibUsbTransfer) of object;
  TLibUsbTransfer = class    // don't use directly
  private
    FCallback : TLibUsbTransferCallback;
    Function  GetTimeout : Cardinal;
    Procedure SetTimeout(Const ATimeout : Cardinal);
  protected
    FTransfer : Plibusb_transfer;
    FEndpoint : TLibUsbEndpoint;
    Constructor Create(AEndpoint:TLibUsbEndpoint;AIsoPackets:Integer);
  public
    Destructor Destroy; override;
    Function Submit : Integer;
    Function Cancel : Integer;
    property Timeout  : Cardinal Read GetTimeout Write SetTimeout;
    property Callback : TLibUsbTransferCallback read FCallback write FCallback;
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

  { ELibUsb }

  ELibUsb = class(Exception)
  private
    FError : Integer;
  public
    Constructor Create   (AError : Integer;Const Msg : String);
    Constructor CreateFmt(AError : Integer;Const Msg : String; Const Args : Array of Const);
    Class Function Check   (AResult:Integer;Const Msg : String) : Integer;
    Class Function CheckFmt(AResult:Integer;Const Msg : String; Const Args : Array of Const) : Integer;
  End;

Implementation
Uses CTypes;

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

{ TLibUsbDevice }

Constructor TLibUsbDevice.Create(AContext : TLibUsbContext; ADevice : Plibusb_device);
Begin
  inherited Create;
  FContext := AContext;
  FDevice  := ADevice;
  ELibUsb.Check(libusb_open(FDevice,FHandle),'Open');
End;

Constructor TLibUsbDevice.Create(AContext : TLibUsbContext; AVID, APID : Word);
Begin
  inherited Create;
  FContext := AContext;
  FHandle := libusb_open_device_with_vid_pid(FContext.Context,AVID,APID);
  if FHandle = Nil then
    raise ELibUsb.CreateFmt(Integer(LIBUSB_ERROR_NO_DEVICE),'Couldn''t find or open device %.4x:%.4x',[AVID,APID]);
  FDevice := libusb_get_device(FHandle);
End;

Destructor TLibUsbDevice.Destroy;
Begin
  libusb_close(FHandle);
  inherited Destroy;
End;

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

{ TLibUsbInterface }

Constructor TLibUsbInterface.Create(ADevice : TLibUsbDevice; ANum : Integer);
Begin
  inherited Create;
  FDevice     := ADevice;
  FNum        := ANum;
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
  ELibUsb.Check(libusb_set_interface_alt_setting(FDevice.FHandle,FNum,FAltSetting),'SetAltSetting');
End;

Procedure TLibUsbInterface.Claim;
Var Ret : Integer;
Begin
  { claim interface }
  Ret := libusb_claim_interface(FDevice.FHandle,FNum);
  if Ret >= 0 then
    Exit;   // everything ok
  if Ret <> LIBUSB_ERROR_BUSY then
    ELibUsb.Create(Ret,'Claim');   // any other error except BUSY
  { device or resource busy, this is probably because the interface is
    clamied by another driver, e.g. the kernel }
  if not IsKernelDriverActive then
    ELibUsb.CreateFmt(Ret,'Couldn''t claim interface %d but there is no kernel driver.',[FNum]);
  { detach driver }
  DetachKernelDriver;
  { claim again }
  ELibUsb.Check(libusb_claim_interface(FDevice.FHandle,FNum),'2nd Claim');
End;

Procedure TLibUsbInterface.Release;
Begin
  ELibUsb.Check(libusb_release_interface(FDevice.FHandle,FNum),'Release');
End;

Function TLibUsbInterface.IsKernelDriverActive : Boolean;
Begin
  Result := (ELibUsb.Check(libusb_kernel_driver_active(FDevice.FHandle,FNum),'IsKernelDriverActive') > 0);
End;

Procedure TLibUsbInterface.DetachKernelDriver;
Begin
  ELibUsb.Check(libusb_detach_kernel_driver(FDevice.FHandle,FNum),'DetachKernelDriver');
End;

Procedure TLibUsbInterface.AttachKernelDriver;
Begin
  ELibUsb.Check(libusb_attach_kernel_driver(FDevice.FHandle,FNum),'AttachKernelDriver');
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

Function TLibUsbBulkOutEndpoint.Send(St:String;Timeout:LongInt):LongInt;
Var Transferred : Integer;
Begin
  Result := libusb_bulk_transfer(FDevice.Handle,FEndpoint,@St[1],Length(St),Transferred,Timeout);
  if Result = 0 then Result := Transferred;
End;

{ TLibUsbBulkInEndpoint }

Function TLibUsbBulkInEndpoint.Recv(Out Buf;Length:LongInt;Timeout:LongInt):LongInt;
Begin
  Result := libusb_bulk_transfer(FDevice.Handle,FEndpoint,@Buf,Length,Length,Timeout);
  if Result = 0 then Result := Length;
End;

{ TLibUsbInterruptOutEndpoint }

Function TLibUsbInterruptOutEndpoint.Send(Const Buf;Length,Timeout:LongInt):LongInt;
Begin
  Result := libusb_interrupt_transfer(FDevice.Handle,FEndpoint,@Buf,Length,Length,Timeout);
  if Result = 0 then Result := Length;
End;

Function TLibUsbInterruptOutEndpoint.Send(St:String;Timeout:LongInt):LongInt;
Var Transferred : Integer;
Begin
  Result := libusb_interrupt_transfer(FDevice.Handle,FEndpoint,@St[1],Length(St),Transferred,Timeout);
  if Result = 0 then Result := Transferred;
End;

{ TLibUsbInterruptInEndpoint }

Function TLibUsbInterruptInEndpoint.Recv(Out Buf;Length:LongInt;Timeout:LongInt):LongInt;
Begin
  Result := libusb_interrupt_transfer(FDevice.Handle,FEndpoint,@Buf,Length,Length,Timeout);
  if Result = 0 then Result := Length;
End;

{ TLibUsbTransfer }

Procedure LibUsbTransferCallback(transfer:Plibusb_transfer); cdecl;
Var MyTransfer : TLibUsbTransfer;
Begin
  MyTransfer := TLibUsbTransfer(transfer^.user_data);
  if not assigned(MyTransfer) then Exit;
  if not assigned(MyTransfer.FCallback) then Exit;
  MyTransfer.FCallback(MyTransfer);
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

Destructor TLibUsbTransfer.Destroy;
Begin
  libusb_free_transfer(FTransfer);
  Inherited Destroy;
End;

Function TLibUsbTransfer.Submit : Integer;
Begin
  Result := libusb_submit_transfer(FTransfer);
End;

Function TLibUsbTransfer.Cancel : Integer;
Begin
  Result := libusb_cancel_transfer(FTransfer);
End;

Function TLibUsbTransfer.GetTimeout : Cardinal;
Begin
  Result := FTransfer^.timeout;
End;

Procedure TLibUsbTransfer.SetTimeout(Const ATimeout : Cardinal);
Begin
  FTransfer^.timeout := ATimeout;
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
  System.SetLength(FBuf,SizeOf(libusb_control_setup) + ALength);
  Plibusb_control_setup(@(FBuf[0]))^.wLength := ALength;;
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

End.

