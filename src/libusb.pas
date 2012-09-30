unit LibUSB;

Interface

{$LINKLIB c}
{$LINKLIB usb}

{
  Automatically converted by H2Pas 0.99.15 from usb.h
  The following command line parameters were used:
    -d
    -l
    usb
    -o
    libusb.pp
    -p
    -v
    usb.h
}

{ global Linux constants }

Const PATH_MAX = 4096;

{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

{$PACKRECORDS C}

{
   USB spec information
  
   This is all stuff grabbed from various USB specs and is pretty much
   not subject to change
}
{
   Device and/or Interface Class codes
}
{ for DeviceClass  }

Const
   USB_CLASS_PER_INTERFACE = 0;
   USB_CLASS_AUDIO = 1;
   USB_CLASS_COMM = 2;
   USB_CLASS_HID = 3;
   USB_CLASS_PRINTER = 7;
   USB_CLASS_MASS_STORAGE = 8;
   USB_CLASS_HUB = 9;
   USB_CLASS_DATA = 10;
   USB_CLASS_VENDOR_SPEC = $ff;
{
   Descriptor types
}
   USB_DT_DEVICE = $01;
   USB_DT_CONFIG = $02;
   USB_DT_STRING = $03;
   USB_DT_INTERFACE = $04;
   USB_DT_ENDPOINT = $05;
   USB_DT_HID = $21;
   USB_DT_REPORT = $22;
   USB_DT_PHYSICAL = $23;
   USB_DT_HUB = $29;
{
   Descriptor sizes per descriptor type
}
   USB_DT_DEVICE_SIZE = 18;
   USB_DT_CONFIG_SIZE = 9;
   USB_DT_INTERFACE_SIZE = 9;
   USB_DT_ENDPOINT_SIZE = 7;
{ Audio extension  }
   USB_DT_ENDPOINT_AUDIO_SIZE = 9;
   USB_DT_HUB_NONVAR_SIZE = 7;
{ All standard descriptors have these 2 fields in common  }

Type
   PUSBDescriptorHeader = ^USBDescriptorHeader;
   USBDescriptorHeader = packed record
        bLength : Byte;
        bDescriptorType : Byte;
     End;

{ String descriptor  }
   PUSBStringDescriptor = ^USBStringDescriptor;
   USBStringDescriptor = packed record
        bLength : Byte;
        bDescriptorType : Byte;
        wData : Array[0..0] of WideChar;
     End;

{ HID descriptor  }
   Pusb_hid_descriptor = ^usb_hid_descriptor;
   usb_hid_descriptor = packed record
        bLength : Byte;
        bDescriptorType : Byte;
        bcdHID : Word;
        bCountryCode : Byte;
        bNumDescriptors : Byte;
        bReportDescriptorType : Byte;
        wDescriptorLength : Word;
     End;

{ Endpoint descriptor  }

Const
   USB_MAXENDPOINTS = 32;
{ Extra descriptors  }

Type
   PUSBEndpointDescriptor = ^USBEndpointDescriptor;
   USBEndpointDescriptor = record
        bLength : Byte;
        bDescriptorType : Byte;
        bEndpointAddress : Byte;
        bmAttributes : Byte;
        wMaxPacketSize : Word;
        bInterval : Byte;
        bRefresh : Byte;
        bSynchAddress : Byte;
        extra : Pbyte;
        extralen : Longint;
     End;
   PUSBEndpointDescriptorArray = ^USBEndpointDescriptorArray;
   USBEndpointDescriptorArray = Array[0..0] of USBEndpointDescriptor;

{ in bEndpointAddress  }

Const
   USB_ENDPOINT_ADDRESS_MASK = $0f;
   USB_ENDPOINT_DIR_MASK = $80;
{ in bmAttributes  }
   USB_ENDPOINT_TYPE_MASK = $03;
   USB_ENDPOINT_TYPE_CONTROL = 0;
   USB_ENDPOINT_TYPE_ISOCHRONOUS = 1;
   USB_ENDPOINT_TYPE_BULK = 2;
   USB_ENDPOINT_TYPE_INTERRUPT = 3;
{ Interface descriptor  }
   USB_MAXINTERFACES = 32;
{ Extra descriptors  }

Type
   PUSBInterfaceDescriptor = ^USBInterfaceDescriptor;
   USBInterfaceDescriptor = record
        bLength : Byte;
        bDescriptorType : Byte;
        bInterfaceNumber : Byte;
        bAlternateSetting : Byte;
        bNumEndpoints : Byte;
        bInterfaceClass : Byte;
        bInterfaceSubClass : Byte;
        bInterfaceProtocol : Byte;
        iInterface : Byte;
        endpoint : PUSBEndpointDescriptorArray;
        extra : Pbyte;
        extralen : Longint;
     End;
   PUSBInterfaceDescriptorArray = ^USBInterfaceDescriptorArray;
   USBInterfaceDescriptorArray = Array[0..0] of USBInterfaceDescriptor;


{ Hard limit  }

Const
   USB_MAXALTSETTING = 128;

Type
   PUSB_Interface = ^USB_Interface;
   USB_Interface = record
        altsetting : PUSBInterfaceDescriptorArray;
        num_altsetting : Longint;
     End;
   PUSB_InterfaceArray = ^USB_InterfaceArray;
   USB_InterfaceArray = Array[0..0] of USB_Interface;

{ Configuration descriptor information..  }

Const
   USB_MAXCONFIG = 8;
{ Extra descriptors  }

Type
   PUSBConfigDescriptor = ^USBConfigDescriptor;
   USBConfigDescriptor = record
        bLength : Byte;
        bDescriptorType : Byte;
        wTotalLength : Word;
        bNumInterfaces : Byte;
        bConfigurationValue : Byte;
        iConfiguration : Byte;
        bmAttributes : Byte;
        MaxPower : Byte;
        TheInterface : PUSB_InterfaceArray;
        extra : Pbyte;
        extralen : Longint;
     End;

{ Device descriptor  }
   PUSBDeviceDescriptor = ^USBDeviceDescriptor;
   USBDeviceDescriptor = record
        bLength : Byte;
        bDescriptorType : Byte;
        bcdUSB : Word;
        bDeviceClass : Byte;
        bDeviceSubClass : Byte;
        bDeviceProtocol : Byte;
        bMaxPacketSize0 : Byte;
        idVendor : Word;
        idProduct : Word;
        bcdDevice : Word;
        iManufacturer : Byte;
        iProduct : Byte;
        iSerialNumber : Byte;
        bNumConfigurations : Byte;
     End;

{
   Standard requests
}

Const
   USB_REQ_GET_STATUS = $00;
   USB_REQ_CLEAR_FEATURE = $01;
{ 0x02 is reserved  }
   USB_REQ_SET_FEATURE = $03;
{ 0x04 is reserved  }
   USB_REQ_SET_ADDRESS = $05;
   USB_REQ_GET_DESCRIPTOR = $06;
   USB_REQ_SET_DESCRIPTOR = $07;
   USB_REQ_GET_CONFIGURATION = $08;
   USB_REQ_SET_CONFIGURATION = $09;
   USB_REQ_GET_INTERFACE = $0A;
   USB_REQ_SET_INTERFACE = $0B;
   USB_REQ_SYNCH_FRAME = $0C;
   USB_TYPE_STANDARD = $00 shl 5;
   USB_TYPE_CLASS = $01 shl 5;
   USB_TYPE_VENDOR = $02 shl 5;
   USB_TYPE_RESERVED = $03 shl 5;
   USB_RECIP_DEVICE = $00;
   USB_RECIP_INTERFACE = $01;
   USB_RECIP_ENDPOINT = $02;
   USB_RECIP_OTHER = $03;

  USB_REQ_HID_GET_REPORT   = $01;
  USB_REQ_HID_GET_IDLE     = $02;
  USB_REQ_HID_GET_PROTOCOL = $03;
  USB_REQ_HID_SET_REPORT   = $09;
  USB_REQ_HID_SET_IDLE     = $0A;
  USB_REQ_HID_SET_PROTOCOL = $0B;

  HID_REPORT_TYPE_INPUT    = $01;
  HID_REPORT_TYPE_OUTPUT   = $02;
  HID_REPORT_TYPE_FEATURE  = $03;

{
   Various libusb API related stuff
  }
   USB_ENDPOINT_IN = $80;
   USB_ENDPOINT_OUT = $00;
{ Error codes  }
   USB_ERROR_BEGIN = 500000;

Var (* errno : LongInt; cvar; external; { from libc } *)
    usb_error_errno : LongInt; cvar; external; { from libusb }
    usb_error_type  : LongInt; cvar; external; { from libusb }
    usb_error_str   : PChar;   cvar; external; { from libusb }

Type PUSBBus = ^USBBus;

   PUSBDevice = ^USBDevice;
   USBDevice = record
        next : PUSBDevice;
        prev : PUSBDevice;
        filename : Array[0..(PATH_MAX + 1)-1] of Char;
        bus : PUSBBus;
        descriptor : USBDeviceDescriptor;
        config : PUSBConfigDescriptor;
        dev : pointer;
     End;

   { PUSBBus = ^USBBus;}
   USBBus = record
        next : PUSBBus;
        prev : PUSBBus;
        dirname : Array[0..(PATH_MAX + 1)-1] of Char;
        devices : PUSBDevice;
     End;
   PUSBDevHandle = ^TUSBDevHandle;
   TUSBDevHandle = record
     End;

{ Variables  }

Var USBBusses : PUSBBus; cvar; external;

{ Function prototypes  }

Function usb_open(dev:PUSBDevice):PUSBDevHandle; cdecl; external;

Function usb_close(dev:PUSBDevHandle):Longint; cdecl; external;

Function usb_get_string       (dev:PUSBDevHandle; Index:Longint; langid:Longint; Var Buf; buflen:Longint) : Longint; cdecl; external;
Function usb_get_string_simple(dev:PUSBDevHandle; index:Longint;                 Var Buf; buflen:Longint) : Longint; cdecl; external;


Function usb_bulk_write(dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint; cdecl; external;

Function usb_bulk_read(dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint; cdecl; external;

Function usb_interrupt_write(dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint; cdecl; external;

Function usb_interrupt_read(dev:PUSBDevHandle; ep:Longint; bytes:PChar; size:Longint; timeout:Longint):Longint; cdecl; external;

Function usb_control_msg(dev:PUSBDevHandle; requestType:Longint; request:Longint; value:Longint; index:Longint; 
           bytes:PChar; size:Longint; timeout:Longint):Longint; cdecl; external;

Function usb_set_configuration(dev:PUSBDevHandle; configuration:Longint):Longint; cdecl; external;

Function usb_claim_interface(dev:PUSBDevHandle; TheInterface:Longint):Longint; cdecl; external;

Function usb_release_interface(dev:PUSBDevHandle; TheInterface:Longint):Longint; cdecl; external;

Function usb_set_altinterface(dev:PUSBDevHandle; alternate:Longint):Longint; cdecl; external;

Function usb_resetep(dev:PUSBDevHandle; ep:dword):Longint; cdecl; external;

Function usb_clear_halt(dev:PUSBDevHandle; ep:dword):Longint; cdecl; external;

Function usb_reset(dev:PUSBDevHandle):Longint; cdecl; external;

Function usb_strerror:PChar; cdecl; external;

Procedure usb_init; cdecl; external;

Procedure usb_set_debug(level:Longint); cdecl; external;

Function usb_find_busses:Longint; cdecl; external;

Function usb_find_devices:Longint; cdecl; external;

Function usb_device(Dev:PUSBDevHandle):PUSBDevice; cdecl; external;

Function usb_get_busses : PUSBBus; cdecl; external;

Function usb_get_driver_np          (dev:PUSBDevHandle; TheInterface:Longint; Var name; namelen:LongInt) : Longint; cdecl; external;

Function usb_detach_kernel_driver_np(dev:PUSBDevHandle; TheInterface:Longint) : LongInt; cdecl; external;

Implementation

End.

