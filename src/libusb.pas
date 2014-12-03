(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   Pascal unit translated from the public libusbx header file.           *
 *                                                                         *
 *   This Pascal unit is free software; you can redistribute it and/or     *
 *   modify it under the terms of the GNU Lesser General Public            *
 *   License as published by the Free Software Foundation; either          *
 *   version 2.1 of the License, or (at your option) any later version.    *
 *                                                                         *
 *   Some C preprocessor macros were translated to Pascal functions.       *
 *   These are free software too; you can redistribute and/or modify       *
 *   them under the terms of a modified GNU Lesser General Public License  *
 *   (see the file COPYING.modifiedLGPL.txt).                              *
 *                                                                         *
 *   The original file header follows.                                     *
 *                                                                         *
 ***************************************************************************)
(*
 * Public libusbx header file
 * Copyright © 2007-2008 Daniel Drake <dsd@gentoo.org>
 * Copyright © 2001 Johannes Erdfelt <johannes@erdfelt.com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)

Unit LibUsb;

{$mode objfpc}
{$PACKRECORDS C}

{$macro on}
{$ifdef windows}
  {$define extdecl:=stdcall}
{$else}
  {$define extdecl:=cdecl}
{$endif}

Interface

Uses CTypes;

{$LINKLIB c}
{$LINKLIB usb-1.0}

(*
  Automatically converted by H2Pas 1.0.0 from libusb.h
  The following command line parameters were used:
    -C
    -c
    -d
    -p
    -pr
    -v
    libusb.h
  Then corrected and beautified by Johann Glaser <Johann.Glaser@gmx.at>.
*)

Type
  csize  = Cardinal;
  cssize = Longint;
  __time_t      = cuint64;
  __suseconds_t = cuint64;
  Ptimeval = ^timeval;
  timeval = record
    tv_sec  : __time_t;
    tv_usec : __suseconds_t;
  End;

(** \def LIBUSB_CALL
 * \ingroup misc
 * libusbx's Windows calling convention.
 *
 * Under Windows, the selection of available compilers and configurations
 * means that, unlike other platforms, there is not <em>one true calling
 * convention</em> (calling convention: the manner in which parameters are
 * passed to funcions in the generated assembly code).
 *
 * Matching the Windows API itself, libusbx uses the WINAPI convention (which
 * translates to the <tt>stdcall</tt> convention) and guarantees that the
 * library is compiled in this way. The public header file also includes
 * appropriate annotations so that your own software will use the right
 * convention, even if another convention is being used by default within
 * your codebase.
 *
 * The one consideration that you must apply in your software is to mark
 * all functions which you use as libusbx callbacks with this LIBUSB_CALL
 * annotation, so that they too get compiled for the correct calling
 * convention.
 *
 * On non-Windows operating systems, this macro is defined as nothing. This
 * means that you can apply it to your code without worrying about
 * cross-platform compatibility.
 *)
(* must be defined on both definition and declaration of libusbx
 * functions. You'd think that declaration would be enough, but cygwin will
 * complain about conflicting types unless both are marked this way.
 * The placement of this macro is important too; it must appear after the
 * return type, before the function name. See internal documentation for
 * API_EXPORTED.
 *)

(** \def libusb_cpu_to_le16
 * \ingroup misc
 * Convert a 16-bit value from host-endian to little-endian format. On
 * little endian systems, this function does nothing. On big endian systems,
 * the bytes are swapped.
 * \param x the host-endian value to convert
 * \returns the value in little-endian byte order
 *)
Function libusb_cpu_to_le16(x:cuint16) : cuint16;

(** \def libusb_le16_to_cpu
 * \ingroup misc
 * Convert a 16-bit value from little-endian to host-endian format. On
 * little endian systems, this function does nothing. On big endian systems,
 * the bytes are swapped.
 * \param x the little-endian value to convert
 * \returns the value in host-endian byte order
 *)
Function libusb_le16_to_cpu(x:cuint16) : cuint16;

(* standard USB stuff *)

(** \ingroup desc
 * Device and/or Interface Class codes *)

Const
  (** In the context of a \ref libusb_device_descriptor "device descriptor",
    * this bDeviceClass value indicates that each interface specifies its
    * own class information and all interfaces operate independently.
    *)
  LIBUSB_CLASS_PER_INTERFACE = 0;
  (** Audio class *)
  LIBUSB_CLASS_AUDIO = 1;
  (** Communications class *)
  LIBUSB_CLASS_COMM = 2;
  (** Human Interface Device class *)
  LIBUSB_CLASS_HID = 3;
  (** Physical *)
  LIBUSB_CLASS_PHYSICAL = 5;
  (** Printer class *)
  LIBUSB_CLASS_PRINTER = 7;
  (** Image class *)
  LIBUSB_CLASS_PTP = 6;        (* legacy name from libusb-0.1 usb.h *)
  LIBUSB_CLASS_IMAGE = 6;
  (** Mass storage class *)
  LIBUSB_CLASS_MASS_STORAGE = 8;
  (** Hub class *)
  LIBUSB_CLASS_HUB = 9;
  (** Data class *)
  LIBUSB_CLASS_DATA = 10;
  (** Smart Card *)
  LIBUSB_CLASS_SMART_CARD = $0b;
  (** Content Security *)
  LIBUSB_CLASS_CONTENT_SECURITY = $0d;
  (** Video *)
  LIBUSB_CLASS_VIDEO = $0e;
  (** Personal Healthcare *)
  LIBUSB_CLASS_PERSONAL_HEALTHCARE = $0f;
  (** Diagnostic Device *)
  LIBUSB_CLASS_DIAGNOSTIC_DEVICE = $dc;
  (** Wireless class *)
  LIBUSB_CLASS_WIRELESS = $e0;
  (** Application class *)
  LIBUSB_CLASS_APPLICATION = $fe;
  (** Class is vendor-specific *)
  LIBUSB_CLASS_VENDOR_SPEC = $ff;

  (** \ingroup desc
   * Descriptor types as defined by the USB specification. *)
  (** Device descriptor. See libusb_device_descriptor. *)
  LIBUSB_DT_DEVICE = $01;
  (** Configuration descriptor. See libusb_config_descriptor. *)
  LIBUSB_DT_CONFIG = $02;
  (** String descriptor *)
  LIBUSB_DT_STRING = $03;
  (** Interface descriptor. See libusb_interface_descriptor. *)
  LIBUSB_DT_INTERFACE = $04;
  (** Endpoint descriptor. See libusb_endpoint_descriptor. *)
  LIBUSB_DT_ENDPOINT = $05;
  (** HID descriptor *)
  LIBUSB_DT_HID = $21;
  (** HID report descriptor *)
  LIBUSB_DT_REPORT = $22;
  (** Physical descriptor *)
  LIBUSB_DT_PHYSICAL = $23;
  (** Hub descriptor *)
  LIBUSB_DT_HUB = $29;

(* Descriptor sizes per descriptor type *)

Const
  LIBUSB_DT_DEVICE_SIZE         = 18;
  LIBUSB_DT_CONFIG_SIZE         =  9;
  LIBUSB_DT_INTERFACE_SIZE      =  9;
  LIBUSB_DT_ENDPOINT_SIZE       =  7;
  LIBUSB_DT_ENDPOINT_AUDIO_SIZE =  9; (* Audio extension *)
  LIBUSB_DT_HUB_NONVAR_SIZE     =  7;

  LIBUSB_ENDPOINT_ADDRESS_MASK = $0f;   (* in bEndpointAddress *)
  LIBUSB_ENDPOINT_DIR_MASK     = $80;

  (** \ingroup desc
   * Endpoint direction. Values for bit 7 of the
   * \ref libusb_endpoint_descriptor::bEndpointAddress "endpoint address" scheme.
   *)
  (** In: device-to-host *)
  LIBUSB_ENDPOINT_IN  = $80;
  (** Out: host-to-device *)
  LIBUSB_ENDPOINT_OUT = $00;

  LIBUSB_TRANSFER_TYPE_MASK = $03;   (* in bmAttributes *)

  (** \ingroup desc
   * Endpoint transfer type. Values for bits 0:1 of the
   * \ref libusb_endpoint_descriptor::bmAttributes "endpoint attributes" field.
   *)
  (** Control endpoint *)
  LIBUSB_TRANSFER_TYPE_CONTROL = 0;
  (** Isochronous endpoint *)
  LIBUSB_TRANSFER_TYPE_ISOCHRONOUS = 1;
  (** Bulk endpoint *)
  LIBUSB_TRANSFER_TYPE_BULK = 2;
  (** Interrupt endpoint *)
  LIBUSB_TRANSFER_TYPE_INTERRUPT = 3;

  (** \ingroup misc
   * Standard requests; as defined in table 9-3 of the USB2 specifications *)

  (** Request status of the specific recipient *)
  LIBUSB_REQUEST_GET_STATUS = $00;
  (** Clear or disable a specific feature *)
  LIBUSB_REQUEST_CLEAR_FEATURE = $01;
  (* 0x02 is reserved *)
  (** Set or enable a specific feature *)
  LIBUSB_REQUEST_SET_FEATURE = $03;
  (* 0x04 is reserved *)
  (** Set device address for all future accesses *)
  LIBUSB_REQUEST_SET_ADDRESS = $05;
  (** Get the specified descriptor *)
  LIBUSB_REQUEST_GET_DESCRIPTOR = $06;
  (** Used to update existing descriptors or add new descriptors *)
  LIBUSB_REQUEST_SET_DESCRIPTOR = $07;
  (** Get the current device configuration value *)
  LIBUSB_REQUEST_GET_CONFIGURATION = $08;
  (** Set device configuration *)
  LIBUSB_REQUEST_SET_CONFIGURATION = $09;
  (** Return the selected alternate setting for the specified interface *)
  LIBUSB_REQUEST_GET_INTERFACE = $0A;
  (** Select an alternate interface for the specified interface *)
  LIBUSB_REQUEST_SET_INTERFACE = $0B;
  (** Set then report an endpoint's synchronization frame *)
  LIBUSB_REQUEST_SYNCH_FRAME = $0C;

  (** \ingroup misc
   * Request type bits of the
   * \ref libusb_control_setup::bmRequestType "bmRequestType" field in control
   * transfers. *)
  (** Standard *)
  LIBUSB_REQUEST_TYPE_STANDARD = $00 shl 5;
  (** Class *)
  LIBUSB_REQUEST_TYPE_CLASS = $01 shl 5;
  (** Vendor *)
  LIBUSB_REQUEST_TYPE_VENDOR = $02 shl 5;
  (** Reserved *)
  LIBUSB_REQUEST_TYPE_RESERVED = $03 shl 5;

  (** \ingroup misc
   * Recipient bits of the
   * \ref libusb_control_setup::bmRequestType "bmRequestType" field in control
   * transfers. Values 4 through 31 are reserved. *)
  (** Device *)
  LIBUSB_RECIPIENT_DEVICE = $00;
  (** Interface *)
  LIBUSB_RECIPIENT_INTERFACE = $01;
  (** Endpoint *)
  LIBUSB_RECIPIENT_ENDPOINT = $02;
  (** Other *)
  LIBUSB_RECIPIENT_OTHER = $03;

  LIBUSB_ISO_SYNC_TYPE_MASK = $0C;

  (** \ingroup desc
   * Synchronization type for isochronous endpoints. Values for bits 2:3 of the
   * \ref libusb_endpoint_descriptor::bmAttributes "bmAttributes" field in
   * libusb_endpoint_descriptor.
   *)
  (** No synchronization *)
  LIBUSB_ISO_SYNC_TYPE_NONE = 0;
  (** Asynchronous *)
  LIBUSB_ISO_SYNC_TYPE_ASYNC = 1;
  (** Adaptive *)
  LIBUSB_ISO_SYNC_TYPE_ADAPTIVE = 2;
  (** Synchronous *)
  LIBUSB_ISO_SYNC_TYPE_SYNC = 3;

  LIBUSB_ISO_USAGE_TYPE_MASK = $30;

  (** \ingroup desc
   * Usage type for isochronous endpoints. Values for bits 4:5 of the
   * \ref libusb_endpoint_descriptor::bmAttributes "bmAttributes" field in
   * libusb_endpoint_descriptor.
   *)
  (** Data endpoint *)
  LIBUSB_ISO_USAGE_TYPE_DATA = 0;
  (** Feedback endpoint *)
  LIBUSB_ISO_USAGE_TYPE_FEEDBACK = 1;
  (** Implicit feedback Data endpoint *)
  LIBUSB_ISO_USAGE_TYPE_IMPLICIT = 2;

Type
  (** \ingroup desc
   * A structure representing the standard USB device descriptor. This
   * descriptor is documented in section 9.6.1 of the USB 2.0 specification.
   * All multiple-byte fields are represented in host-endian format.
   *)
  Plibusb_device_descriptor = ^libusb_device_descriptor;
  libusb_device_descriptor = record
    (** Size of this descriptor (in bytes) *)
    bLength : cuint8;
    (** Descriptor type. Will have value
     * \ref libusb_descriptor_type::LIBUSB_DT_DEVICE LIBUSB_DT_DEVICE in this
     * context. *)
    bDescriptorType : cuint8;
    (** USB specification release number in binary-coded decimal. A value of
     * 0x0200 indicates USB 2.0, 0x0110 indicates USB 1.1, etc. *)
    bcdUSB : cuint16;
    (** USB-IF class code for the device. See \ref libusb_class_code. *)
    bDeviceClass : cuint8;
    (** USB-IF subclass code for the device, qualified by the bDeviceClass
     * value *)
    bDeviceSubClass : cuint8;
    (** USB-IF protocol code for the device, qualified by the bDeviceClass and
     * bDeviceSubClass values *)
    bDeviceProtocol : cuint8;
    (** Maximum packet size for endpoint 0 *)
    bMaxPacketSize0 : cuint8;
    (** USB-IF vendor ID *)
    idVendor : cuint16;
    (** USB-IF product ID *)
    idProduct : cuint16;
    (** Device release number in binary-coded decimal *)
    bcdDevice : cuint16;
    (** Index of string descriptor describing manufacturer *)
    iManufacturer : cuint8;
    (** Index of string descriptor describing product *)
    iProduct : cuint8;
    (** Index of string descriptor containing device serial number *)
    iSerialNumber : cuint8;
    (** Number of possible configurations *)
    bNumConfigurations : cuint8;
  end;

  (** \ingroup desc
   * A structure representing the standard USB endpoint descriptor. This
   * descriptor is documented in section 9.6.3 of the USB 2.0 specification.
   * All multiple-byte fields are represented in host-endian format.
   *)
  Plibusb_endpoint_descriptor = ^libusb_endpoint_descriptor;
  libusb_endpoint_descriptor = record
    (** Size of this descriptor (in bytes) *)
    bLength : cuint8;
    (** Descriptor type. Will have value
       * \ref libusb_descriptor_type::LIBUSB_DT_ENDPOINT LIBUSB_DT_ENDPOINT in
       * this context. *)
    bDescriptorType : cuint8;
    (** The address of the endpoint described by this descriptor. Bits 0:3 are
     * the endpoint number. Bits 4:6 are reserved. Bit 7 indicates direction,
     * see \ref libusb_endpoint_direction.
     *)
    bEndpointAddress : cuint8;
    (** Attributes which apply to the endpoint when it is configured using
     * the bConfigurationValue. Bits 0:1 determine the transfer type and
     * correspond to \ref libusb_transfer_type. Bits 2:3 are only used for
     * isochronous endpoints and correspond to \ref libusb_iso_sync_type.
     * Bits 4:5 are also only used for isochronous endpoints and correspond to
     * \ref libusb_iso_usage_type. Bits 6:7 are reserved.
     *)
    bmAttributes : cuint8;
    (** Maximum packet size this endpoint is capable of sending/receiving. *)
    wMaxPacketSize : cuint16;
    (** Interval for polling endpoint for data transfers. *)
    bInterval : cuint8;
    (** For audio devices only: the rate at which synchronization feedback
     * is provided. *)
    bRefresh : cuint8;
    (** For audio devices only: the address if the synch endpoint *)
    bSynchAddress : cuint8;
    (** Extra descriptors. If libusbx encounters unknown endpoint descriptors,
     * it will store them here, should you wish to parse them. *)
    extra : pcuchar; (* Const before type ignored *)
    (** Length of the extra descriptors, in bytes. *)
    extra_length : cint;
  end;
  Plibusb_endpoint_descriptor_array = ^libusb_endpoint_descriptor_array;
  libusb_endpoint_descriptor_array = Array[0..0] of libusb_endpoint_descriptor;

  (** \ingroup desc
   * A structure representing the standard USB interface descriptor. This
   * descriptor is documented in section 9.6.5 of the USB 2.0 specification.
   * All multiple-byte fields are represented in host-endian format.
   *)
  Plibusb_interface_descriptor = ^libusb_interface_descriptor;
  libusb_interface_descriptor = record
    (** Size of this descriptor (in bytes) *)
    bLength : cuint8;
    (** Descriptor type. Will have value
     * \ref libusb_descriptor_type::LIBUSB_DT_INTERFACE LIBUSB_DT_INTERFACE
     * in this context. *)
    bDescriptorType : cuint8;
    (** Number of this interface *)
    bInterfaceNumber : cuint8;
    (** Value used to select this alternate setting for this interface *)
    bAlternateSetting : cuint8;
    (** Number of endpoints used by this interface (excluding the control
     * endpoint). *)
    bNumEndpoints : cuint8;
    (** USB-IF class code for this interface. See \ref libusb_class_code. *)
    bInterfaceClass : cuint8;
    (** USB-IF subclass code for this interface, qualified by the
     * bInterfaceClass value *)
    bInterfaceSubClass : cuint8;
    (** USB-IF protocol code for this interface, qualified by the
     * bInterfaceClass and bInterfaceSubClass values *)
    bInterfaceProtocol : cuint8;
    (** Index of string descriptor describing this interface *)
    iInterface : cuint8;
    (** Array of endpoint descriptors. This length of this array is determined
     * by the bNumEndpoints field. *)
    endpoint : Plibusb_endpoint_descriptor_array;  (* Const before type ignored *)
    (** Extra descriptors. If libusbx encounters unknown interface descriptors,
     * it will store them here, should you wish to parse them. *)
    extra : pcuchar;    (* Const before type ignored *)
    (** Length of the extra descriptors, in bytes. *)
    extra_length : cint;
  end;
  Plibusb_interface_descriptor_array = ^libusb_interface_descriptor_array;
  libusb_interface_descriptor_array = Array[0..0] of libusb_interface_descriptor;

  (** \ingroup desc
   * A collection of alternate settings for a particular USB interface.
   *)
  Plibusb_interface = ^libusb_interface;
  libusb_interface = record
    (** Array of interface descriptors. The length of this array is determined
     * by the num_altsetting field. *)
    altsetting : Plibusb_interface_descriptor_array;    (* Const before type ignored *)
    (** The number of alternate settings that belong to this interface *)
    num_altsetting : cint;
  end;
  Plibusb_interface_array = ^libusb_interface_array;
  libusb_interface_array = Array[0..0] of libusb_interface;

  (** \ingroup desc
   * A structure representing the standard USB configuration descriptor. This
   * descriptor is documented in section 9.6.3 of the USB 2.0 specification.
   * All multiple-byte fields are represented in host-endian format.
   *)
  Plibusb_config_descriptor = ^libusb_config_descriptor;
  libusb_config_descriptor = record
    (** Size of this descriptor (in bytes) *)
    bLength : cuint8;
    (** Descriptor type. Will have value
     * \ref libusb_descriptor_type::LIBUSB_DT_CONFIG LIBUSB_DT_CONFIG
     * in this context. *)
    bDescriptorType : cuint8;
    (** Total length of data returned for this configuration *)
    wTotalLength : cuint16;
    (** Number of interfaces supported by this configuration *)
    bNumInterfaces : cuint8;
    (** Identifier value for this configuration *)
    bConfigurationValue : cuint8;
    (** Index of string descriptor describing this configuration *)
    iConfiguration : cuint8;
    (** Configuration characteristics *)
    bmAttributes : cuint8;
    (** Maximum power consumption of the USB device from this bus in this
     * configuration when the device is fully opreation. Expressed in units
     * of 2 mA. *)
    MaxPower : cuint8;
    (** Array of interfaces supported by this configuration. The length of
     * this array is determined by the bNumInterfaces field. *)
    _interface : Plibusb_interface_array;   (* Const before type ignored *)
    (** Extra descriptors. If libusbx encounters unknown configuration
     * descriptors, it will store them here, should you wish to parse them. *)
    extra : pcuchar;    (* Const before type ignored *)
    (** Length of the extra descriptors, in bytes. *)
    extra_length : cint;
  end;

  (** \ingroup asyncio
   * Setup packet for control transfers. *)
  Plibusb_control_setup = ^libusb_control_setup;
  libusb_control_setup = record
    (** Request type. Bits 0:4 determine recipient, see
     * \ref libusb_request_recipient. Bits 5:6 determine type, see
     * \ref libusb_request_type. Bit 7 determines data transfer direction, see
     * \ref libusb_endpoint_direction.
     *)
    bmRequestType : cuint8;
    (** Request. If the type bits of bmRequestType are equal to
     * \ref libusb_request_type::LIBUSB_REQUEST_TYPE_STANDARD
     * "LIBUSB_REQUEST_TYPE_STANDARD" then this field refers to
     * \ref libusb_standard_request. For other cases, use of this field is
     * application-specific. *)
    bRequest : cuint8;
    (** Value. Varies according to request *)
    wValue : cuint16;
    (** Index. Varies according to request, typically used to pass an index
     * or offset *)
    wIndex : cuint16;
    (** Number of bytes to transfer *)
    wLength : cuint16;
  end;

Const
  LIBUSB_CONTROL_SETUP_SIZE = sizeof(libusb_control_setup);

(* libusbx *)

Type
  Plibusb_context = ^libusb_context;
  libusb_context = record
    (*undefined structure*)
  end;

  PPlibusb_device = ^Plibusb_device;
  Plibusb_device = ^libusb_device;
  libusb_device = record
    (*undefined structure*)
  end;

  Plibusb_device_handle = ^libusb_device_handle;
  libusb_device_handle = record
    (*undefined structure*)
  end;

  (** \ingroup lib
   * Structure providing the version of the libusbx runtime
   *)
  Plibusb_version = ^libusb_version;
  libusb_version = record
    (** Library major version. *)
    major : cuint16;            (* Const before type ignored *)
    (** Library minor version. *)
    minor : cuint16;            (* Const before type ignored *)
    (** Library micro version. *)
    micro : cuint16;            (* Const before type ignored *)
    (** Library nano version. *)
    nano : cuint16;             (* Const before type ignored *)
    (** Library release candidate suffix string, e.g. "-rc4". *)
    rc : pcchar;                (* Const before type ignored *)
    (** For ABI compatibility only. *)
    describe : pcchar;          (* Const before type ignored *)
  end;

  (** \ingroup lib
   * Structure representing a libusbx session. The concept of individual libusbx
   * sessions allows for your program to use two libraries (or dynamically
   * load two modules) which both independently use libusb. This will prevent
   * interference between the individual libusbx users - for example
   * libusb_set_debug() will not affect the other user of the library, and
   * libusb_exit() will not destroy resources that the other user is still
   * using.
   *
   * Sessions are created by libusb_init() and destroyed through libusb_exit().
   * If your application is guaranteed to only ever include a single libusbx
   * user (i.e. you), you do not have to worry about contexts: pass NULL in
   * every function call where a context is required. The default context
   * will be used.
   *
   * For more information, see \ref contexts.
   *)
  (** \ingroup dev
   * Structure representing a USB device detected on the system. This is an
   * opaque type for which you are only ever provided with a pointer, usually
   * originating from libusb_get_device_list().
   *
   * Certain operations can be performed on a device, but in order to do any
   * I/O you will have to first obtain a device handle using libusb_open().
   *
   * Devices are reference counted with libusb_device_ref() and
   * libusb_device_unref(), and are freed when the reference count reaches 0.
   * New devices presented by libusb_get_device_list() have a reference count of
   * 1, and libusb_free_device_list() can optionally decrease the reference count
   * on all devices in the list. libusb_open() adds another reference which is
   * later destroyed by libusb_close().
   *)

  (** \ingroup dev
   * Structure representing a handle on a USB device. This is an opaque type for
   * which you are only ever provided with a pointer, usually originating from
   * libusb_open().
   *
   * A device handle is used to perform I/O and other operations. When finished
   * with a device handle, you should call libusb_close().
   *)

Const
  (** \ingroup dev
   * Speed codes. Indicates the speed at which the device is operating.
   *)
  (** The OS doesn't report or know the device speed. *)
  LIBUSB_SPEED_UNKNOWN = 0;
  (** The device is operating at low speed (1.5MBit/s). *)
  LIBUSB_SPEED_LOW = 1;
  (** The device is operating at full speed (12MBit/s). *)
  LIBUSB_SPEED_FULL = 2;
  (** The device is operating at high speed (480MBit/s). *)
  LIBUSB_SPEED_HIGH = 3;
  (** The device is operating at super speed (5000MBit/s). *)
  LIBUSB_SPEED_SUPER = 4;

  (** \ingroup misc
   * Error codes. Most libusbx functions return 0 on success or one of these
   * codes on failure.
   * You can call \ref libusb_error_name() to retrieve a string representation
   * of an error code.
   *)
  (** Success (no error) *)
  LIBUSB_SUCCESS = 0;
  (** Input/output error *)
  LIBUSB_ERROR_IO = -(1);
  (** Invalid parameter *)
  LIBUSB_ERROR_INVALID_PARAM = -(2);
  (** Access denied (insufficient permissions) *)
  LIBUSB_ERROR_ACCESS = -(3);
  (** No such device (it may have been disconnected) *)
  LIBUSB_ERROR_NO_DEVICE = -(4);
  (** Entity not found *)
  LIBUSB_ERROR_NOT_FOUND = -(5);
  (** Resource busy *)
  LIBUSB_ERROR_BUSY = -(6);
  (** Operation timed out *)
  LIBUSB_ERROR_TIMEOUT = -(7);
  (** Overflow *)
  LIBUSB_ERROR_OVERFLOW = -(8);
  (** Pipe error *)
  LIBUSB_ERROR_PIPE = -(9);
  (** System call interrupted (perhaps due to signal) *)
  LIBUSB_ERROR_INTERRUPTED = -(10);
  (** Insufficient memory *)
  LIBUSB_ERROR_NO_MEM = -(11);
  (** Operation not supported or unimplemented on this platform *)
  LIBUSB_ERROR_NOT_SUPPORTED = -(12);
  (* NB! Remember to update libusb_error_name()
     when adding new error codes here. *)
  (** Other error *)
  LIBUSB_ERROR_OTHER = -(99);

  (** \ingroup asyncio
   * Transfer status codes *)
  (** Transfer completed without error. Note that this does not indicate
   * that the entire amount of requested data was transferred. *)
  LIBUSB_TRANSFER_COMPLETED  = 0;
  (** Transfer failed *)
  LIBUSB_TRANSFER_ERROR      = 1;
  (** Transfer timed out *)
  LIBUSB_TRANSFER_TIMED_OUT  = 2;
  (** Transfer was cancelled *)
  LIBUSB_TRANSFER_CANCELLED  = 3;
  (** For bulk/interrupt endpoints: halt condition detected (endpoint
   * stalled). For control endpoints: control request not supported. *)
  LIBUSB_TRANSFER_STALL      = 4;
  (** Device was disconnected *)
  LIBUSB_TRANSFER_NO_DEVICE  = 5;
  (** Device sent more data than requested *)
  LIBUSB_TRANSFER_OVERFLOW   = 6;

  (** \ingroup asyncio
   * libusb_transfer.flags values *)
  (** Report short frames as errors *)
  LIBUSB_TRANSFER_SHORT_NOT_OK = 1 shl 0;
  (** Automatically free() transfer buffer during libusb_free_transfer() *)
  LIBUSB_TRANSFER_FREE_BUFFER = 1 shl 1;
  (** Automatically call libusb_free_transfer() after callback returns.
   * If this flag is set, it is illegal to call libusb_free_transfer()
   * from your transfer callback, as this will result in a double-free
   * when this flag is acted upon. *)
  LIBUSB_TRANSFER_FREE_TRANSFER = 1 shl 2;
  (** Terminate transfers that are a multiple of the endpoint's
   * wMaxPacketSize with an extra zero length packet. This is useful
   * when a device protocol mandates that each logical request is
   * terminated by an incomplete packet (i.e. the logical requests are
   * not separated by other means).
   *
   * This flag only affects host-to-device transfers to bulk and interrupt
   * endpoints. In other situations, it is ignored.
   *
   * This flag only affects transfers with a length that is a multiple of
   * the endpoint's wMaxPacketSize. On transfers of other lengths, this
   * flag has no effect. Therefore, if you are working with a device that
   * needs a ZLP whenever the end of the logical request falls on a packet
   * boundary, then it is sensible to set this flag on <em>every</em>
   * transfer (you do not have to worry about only setting it on transfers
   * that end on the boundary).
   *
   * This flag is currently only supported on Linux.
   * On other systems, libusb_submit_transfer() will return
   * LIBUSB_ERROR_NOT_SUPPORTED for every transfer where this flag is set.
   *
   * Available since libusb-1.0.9.
   *)
  LIBUSB_TRANSFER_ADD_ZERO_PACKET = 1 shl 3;

Type
  (** \ingroup asyncio
   * Isochronous packet descriptor. *)
  Plibusb_iso_packet_descriptor = ^libusb_iso_packet_descriptor;
  libusb_iso_packet_descriptor = record
    (** Length of data to request in this packet *)
    length : cuint;
    (** Amount of data that was actually transferred *)
    actual_length : cuint;
    (** Status code for this packet *)
    status : cint; // was: libusb_transfer_status
  end;

  Plibusb_transfer = ^libusb_transfer;

  (** \ingroup asyncio
   * Asynchronous transfer callback function type. When submitting asynchronous
   * transfers, you pass a pointer to a callback function of this type via the
   * \ref libusb_transfer::callback "callback" member of the libusb_transfer
   * structure. libusbx will call this function later, when the transfer has
   * completed or failed. See \ref asyncio for more information.
   * \param transfer The libusb_transfer struct the callback function is being
   * notified about.
   *)

  libusb_transfer_cb_fn = procedure (transfer:Plibusb_transfer);extdecl;

  (** \ingroup asyncio
   * The generic USB transfer structure. The user populates this structure and
   * then submits it in order to request a transfer. After the transfer has
   * completed, the library populates the transfer with the results and passes
   * it back to the user.
   *)
  libusb_transfer = record
    (** Handle of the device that this transfer will be submitted to *)
    dev_handle : Plibusb_device_handle;
    (** A bitwise OR combination of \ref libusb_transfer_flags. *)
    flags : cuint8;
    (** Address of the endpoint where this transfer will be sent. *)
    endpoint : cuchar;
    (** Type of the endpoint from \ref libusb_transfer_type *)
    _type : cuchar;
    (** Timeout for this transfer in millseconds. A value of 0 indicates no
     * timeout. *)
    timeout : cuint;
    (** The status of the transfer. Read-only, and only for use within
     * transfer callback function.
     *
     * If this is an isochronous transfer, this field may read COMPLETED even
     * if there were errors in the frames. Use the
     * \ref libusb_iso_packet_descriptor::status "status" field in each packet
     * to determine if errors occurred. *)
    status : cint; // was: libusb_transfer_status
    (** Length of the data buffer *)
    length : cint;
    (** Actual length of data that was transferred. Read-only, and only for
     * use within transfer callback function. Not valid for isochronous
     * endpoint transfers. *)
    actual_length : cint;
    (** Callback function. This will be invoked when the transfer completes,
     * fails, or is cancelled. *)
    callback : libusb_transfer_cb_fn;
    (** User context data to pass to the callback function. *)
    user_data : pointer;
    (** Data buffer *)
    buffer : pcuchar;
    (** Number of isochronous packets. Only used for I/O with isochronous
     * endpoints. *)
    num_iso_packets : cint;
    (** Isochronous packet descriptors, for isochronous transfers only. *)
    iso_packet_desc : array[0..0] of libusb_iso_packet_descriptor;
  end;

Const
  (** \ingroup misc
   * Capabilities supported by this instance of libusb. Test if the loaded
   * library supports a given capability by calling
   * \ref libusb_has_capability().
   *)
  (** The libusb_has_capability() API is available. *)
  LIBUSB_CAP_HAS_CAPABILITY = 0;

    (** \ingroup lib
     *  Log message levels.
     *  - LIBUSB_LOG_LEVEL_NONE (0)    : no messages ever printed by the library (default)
     *  - LIBUSB_LOG_LEVEL_ERROR (1)   : error messages are printed to stderr
     *  - LIBUSB_LOG_LEVEL_WARNING (2) : warning and error messages are printed to stderr
     *  - LIBUSB_LOG_LEVEL_INFO (3)    : informational messages are printed to stdout, warning
     *    and error messages are printed to stderr
     *  - LIBUSB_LOG_LEVEL_DEBUG (4)   : debug and informational messages are printed to stdout,
     *    warnings and errors to stderr
     *)
  LIBUSB_LOG_LEVEL_NONE    = 0;
  LIBUSB_LOG_LEVEL_ERROR   = 1;
  LIBUSB_LOG_LEVEL_WARNING = 2;
  LIBUSB_LOG_LEVEL_INFO    = 3;
  LIBUSB_LOG_LEVEL_DEBUG   = 4;


function libusb_init(out ctx:Plibusb_context):cint;extdecl;external;
procedure libusb_exit(ctx:Plibusb_context);extdecl;external;
procedure libusb_set_debug(ctx:Plibusb_context; level:cint);extdecl;external;
function libusb_get_version:Plibusb_version;extdecl;external; (* Const before type ignored *)
function libusb_has_capability(capability:cuint32):cint;extdecl;external; (* Const before type ignored *)
function libusb_error_name(errcode:cint):pcchar;extdecl;external;
function libusb_strerror(errcode:cint):pcchar;extdecl;external;

function libusb_get_device_list(ctx:Plibusb_context; out list:PPlibusb_device):cssize;extdecl;external;
procedure libusb_free_device_list(list:PPlibusb_device; unref_devices:cint);extdecl;external;
function libusb_ref_device(dev:Plibusb_device):Plibusb_device;extdecl;external;
procedure libusb_unref_device(dev:Plibusb_device);extdecl;external;

function libusb_get_configuration(dev:Plibusb_device_handle; Out config:cint):cint;extdecl;external;
function libusb_get_device_descriptor(dev:Plibusb_device; Out desc:libusb_device_descriptor):cint;extdecl;external;
function libusb_get_active_config_descriptor(dev:Plibusb_device; Out config:Plibusb_config_descriptor):cint;extdecl;external;
function libusb_get_config_descriptor(dev:Plibusb_device; config_index:cuint8; Out config:Plibusb_config_descriptor):cint;extdecl;external;
function libusb_get_config_descriptor_by_value(dev:Plibusb_device; bConfigurationValue:cuint8; Out config:Plibusb_config_descriptor):cint;extdecl;external;
procedure libusb_free_config_descriptor(config:Plibusb_config_descriptor);extdecl;external;
function libusb_get_bus_number(dev:Plibusb_device):cuint8;extdecl;external;
function libusb_get_port_number(dev:Plibusb_device):cuint8;extdecl;external;
function libusb_get_parent(dev:Plibusb_device):Plibusb_device;extdecl;external;
function libusb_get_port_path(ctx:Plibusb_context; dev:Plibusb_device; path:Pcuint8; path_length:cuint8):cint;extdecl;external;
function libusb_get_device_address(dev:Plibusb_device):cuint8;extdecl;external;
function libusb_get_device_speed(dev:Plibusb_device):cuint8;extdecl;external;
function libusb_get_max_packet_size(dev:Plibusb_device; endpoint:cuchar):cint;extdecl;external;
function libusb_get_max_iso_packet_size(dev:Plibusb_device; endpoint:cuchar):cint;extdecl;external;

function libusb_open(dev:Plibusb_device; out handle:Plibusb_device_handle):cint;extdecl;external;
procedure libusb_close(dev_handle:Plibusb_device_handle);extdecl;external;
function libusb_get_device(dev_handle:Plibusb_device_handle):Plibusb_device;extdecl;external;

function libusb_set_configuration(dev:Plibusb_device_handle; configuration:cint):cint;extdecl;external;
function libusb_claim_interface(dev:Plibusb_device_handle; interface_number:cint):cint;extdecl;external;
function libusb_release_interface(dev:Plibusb_device_handle; interface_number:cint):cint;extdecl;external;

function libusb_open_device_with_vid_pid(ctx:Plibusb_context; vendor_id:cuint16; product_id:cuint16):Plibusb_device_handle;extdecl;external;

function libusb_set_interface_alt_setting(dev:Plibusb_device_handle; interface_number:cint; alternate_setting:cint):cint;extdecl;external;
function libusb_clear_halt(dev:Plibusb_device_handle; endpoint:cuchar):cint;extdecl;external;
function libusb_reset_device(dev:Plibusb_device_handle):cint;extdecl;external;

function libusb_kernel_driver_active(dev:Plibusb_device_handle; interface_number:cint):cint;extdecl;external;
function libusb_detach_kernel_driver(dev:Plibusb_device_handle; interface_number:cint):cint;extdecl;external;
function libusb_attach_kernel_driver(dev:Plibusb_device_handle; interface_number:cint):cint;extdecl;external;

(* async I/O *)

(** \ingroup asyncio
 * Get the data section of a control transfer. This convenience function is here
 * to remind you that the data does not start until 8 bytes into the actual
 * buffer, as the setup packet comes first.
 *
 * Calling this function only makes sense from a transfer callback function,
 * or situations where you have already allocated a suitably sized buffer at
 * transfer->buffer.
 *
 * \param transfer a transfer
 * \returns pointer to the first byte of the data section
 *)
Function libusb_control_transfer_get_data(transfer:Plibusb_transfer):Pointer;

(** \ingroup asyncio
 * Get the control setup packet of a control transfer. This convenience
 * function is here to remind you that the control setup occupies the first
 * 8 bytes of the transfer data buffer.
 *
 * Calling this function only makes sense from a transfer callback function,
 * or situations where you have already allocated a suitably sized buffer at
 * transfer->buffer.
 *
 * \param transfer a transfer
 * \returns a casted pointer to the start of the transfer data buffer
 *)
Function libusb_control_transfer_get_setup(transfer:Plibusb_transfer):Plibusb_control_setup;

(** \ingroup asyncio
 * Helper function to populate the setup packet (first 8 bytes of the data
 * buffer) for a control transfer. The wIndex, wValue and wLength values should
 * be given in host-endian byte order.
 *
 * \param buffer buffer to output the setup packet into
 * \param bmRequestType see the
 * \ref libusb_control_setup::bmRequestType "bmRequestType" field of
 * \ref libusb_control_setup
 * \param bRequest see the
 * \ref libusb_control_setup::bRequest "bRequest" field of
 * \ref libusb_control_setup
 * \param wValue see the
 * \ref libusb_control_setup::wValue "wValue" field of
 * \ref libusb_control_setup
 * \param wIndex see the
 * \ref libusb_control_setup::wIndex "wIndex" field of
 * \ref libusb_control_setup
 * \param wLength see the
 * \ref libusb_control_setup::wLength "wLength" field of
 * \ref libusb_control_setup
 *)
Procedure libusb_fill_control_setup(Out buffer;bmRequestType:cuint8;bRequest:cuint8;wValue:cuint16;wIndex:cuint16;wLength:cuint16);

function libusb_alloc_transfer(iso_packets:cint):Plibusb_transfer;extdecl;external;
function libusb_submit_transfer(transfer:Plibusb_transfer):cint;extdecl;external;
function libusb_cancel_transfer(transfer:Plibusb_transfer):cint;extdecl;external;
procedure libusb_free_transfer(transfer:Plibusb_transfer);extdecl;external;

(** \ingroup asyncio
 * Helper function to populate the required \ref libusb_transfer fields
 * for a control transfer.
 *
 * If you pass a transfer buffer to this function, the first 8 bytes will
 * be interpreted as a control setup packet, and the wLength field will be
 * used to automatically populate the \ref libusb_transfer::length "length"
 * field of the transfer. Therefore the recommended approach is:
 * -# Allocate a suitably sized data buffer (including space for control setup)
 * -# Call libusb_fill_control_setup()
 * -# If this is a host-to-device transfer with a data stage, put the data
 *    in place after the setup packet
 * -# Call this function
 * -# Call libusb_submit_transfer()
 *
 * It is also legal to pass a NULL buffer to this function, in which case this
 * function will not attempt to populate the length field. Remember that you
 * must then populate the buffer and length fields later.
 *
 * \param transfer the transfer to populate
 * \param dev_handle handle of the device that will handle the transfer
 * \param buffer data buffer. If provided, this function will interpret the
 * first 8 bytes as a setup packet and infer the transfer length from that.
 * \param callback callback function to be invoked on transfer completion
 * \param user_data user data to pass to callback function
 * \param timeout timeout for the transfer in milliseconds
 *)
Procedure libusb_fill_control_transfer(
  transfer:Plibusb_transfer;
  dev_handle:Plibusb_device_handle;
  Out buffer;
  callback:libusb_transfer_cb_fn;
  user_data:Pointer;
  timeout:cuint);

(** \ingroup asyncio
 * Helper function to populate the required \ref libusb_transfer fields
 * for a bulk transfer.
 *
 * \param transfer the transfer to populate
 * \param dev_handle handle of the device that will handle the transfer
 * \param endpoint address of the endpoint where this transfer will be sent
 * \param buffer data buffer
 * \param length length of data buffer
 * \param callback callback function to be invoked on transfer completion
 * \param user_data user data to pass to callback function
 * \param timeout timeout for the transfer in milliseconds
 *)
Procedure libusb_fill_bulk_transfer(
  transfer:Plibusb_transfer;
  dev_handle:Plibusb_device_handle;
  endpoint:cuchar;
  Out buffer;
  length:cint;
  callback:libusb_transfer_cb_fn;
  user_data:Pointer;
  timeout:cuint);

(** \ingroup asyncio
 * Helper function to populate the required \ref libusb_transfer fields
 * for an interrupt transfer.
 *
 * \param transfer the transfer to populate
 * \param dev_handle handle of the device that will handle the transfer
 * \param endpoint address of the endpoint where this transfer will be sent
 * \param buffer data buffer
 * \param length length of data buffer
 * \param callback callback function to be invoked on transfer completion
 * \param user_data user data to pass to callback function
 * \param timeout timeout for the transfer in milliseconds
 *)
Procedure libusb_fill_interrupt_transfer(
  transfer:Plibusb_transfer;
  dev_handle:Plibusb_device_handle;
  endpoint:cuchar;
  Out buffer;
  length:cint;
  callback:libusb_transfer_cb_fn;
  user_data:Pointer;
  timeout:cuint);

(** \ingroup asyncio
 * Helper function to populate the required \ref libusb_transfer fields
 * for an isochronous transfer.
 *
 * \param transfer the transfer to populate
 * \param dev_handle handle of the device that will handle the transfer
 * \param endpoint address of the endpoint where this transfer will be sent
 * \param buffer data buffer
 * \param length length of data buffer
 * \param num_iso_packets the number of isochronous packets
 * \param callback callback function to be invoked on transfer completion
 * \param user_data user data to pass to callback function
 * \param timeout timeout for the transfer in milliseconds
 *)
Procedure libusb_fill_iso_transfer(
  transfer:Plibusb_transfer;
  dev_handle:Plibusb_device_handle;
  endpoint:cuchar;
  Out buffer;
  length:cint;
  num_iso_packets:cint;
  callback:libusb_transfer_cb_fn;
  user_data:Pointer;
  timeout:cuint);

(** \ingroup asyncio
 * Convenience function to set the length of all packets in an isochronous
 * transfer, based on the num_iso_packets field in the transfer structure.
 *
 * \param transfer a transfer
 * \param length the length to set in each isochronous packet descriptor
 * \see libusb_get_max_packet_size()
 *)
Procedure libusb_set_iso_packet_lengths(transfer:Plibusb_transfer;length:cuint);

(** \ingroup asyncio
 * Convenience function to locate the position of an isochronous packet
 * within the buffer of an isochronous transfer.
 *
 * This is a thorough function which loops through all preceding packets,
 * accumulating their lengths to find the position of the specified packet.
 * Typically you will assign equal lengths to each packet in the transfer,
 * and hence the above method is sub-optimal. You may wish to use
 * libusb_get_iso_packet_buffer_simple() instead.
 *
 * \param transfer a transfer
 * \param packet the packet to return the address of
 * \returns the base address of the packet buffer inside the transfer buffer,
 * or NULL if the packet does not exist.
 * \see libusb_get_iso_packet_buffer_simple()
 *)
Function libusb_get_iso_packet_buffer(transfer:Plibusb_transfer;packet:cuint):Pointer;

(** \ingroup asyncio
 * Convenience function to locate the position of an isochronous packet
 * within the buffer of an isochronous transfer, for transfers where each
 * packet is of identical size.
 *
 * This function relies on the assumption that every packet within the transfer
 * is of identical size to the first packet. Calculating the location of
 * the packet buffer is then just a simple calculation:
 * <tt>buffer + (packet_size * packet)</tt>
 *
 * Do not use this function on transfers other than those that have identical
 * packet lengths for each packet.
 *
 * \param transfer a transfer
 * \param packet the packet to return the address of
 * \returns the base address of the packet buffer inside the transfer buffer,
 * or NULL if the packet does not exist.
 * \see libusb_get_iso_packet_buffer()
 *)
Function libusb_get_iso_packet_buffer_simple(transfer:Plibusb_transfer;packet:cuint):Pointer;

(* sync I/O *)

function libusb_control_transfer  (dev_handle:Plibusb_device_handle; request_type:cuint8; bRequest:cuint8; wValue:cuint16; wIndex:cuint16; data:pcuchar; WLength:cuint16; timeout:cuint):cint;extdecl;external;
function libusb_bulk_transfer     (dev_handle:Plibusb_device_handle; endpoint:cuchar; data:pcuchar; length:cint; out actual_length:cint;timeout:cuint):cint;extdecl;external;
function libusb_interrupt_transfer(dev_handle:Plibusb_device_handle; endpoint:cuchar; data:pcuchar; length:cint; out actual_length:cint;timeout:cuint):cint;extdecl;external;

(** \ingroup desc
 * Retrieve a descriptor from the default control pipe.
 * This is a convenience function which formulates the appropriate control
 * message to retrieve the descriptor.
 *
 * \param dev a device handle
 * \param desc_type the descriptor type, see \ref libusb_descriptor_type
 * \param desc_index the index of the descriptor to retrieve
 * \param data output buffer for descriptor
 * \param length size of data buffer
 * \returns number of bytes returned in data, or LIBUSB_ERROR code on failure
 *)
Function libusb_get_descriptor(dev:Plibusb_device_handle;desc_type:cuint8;desc_index:cuint8;data:pcuchar;length:cint):cint;

(** \ingroup desc
 * Retrieve a descriptor from a device.
 * This is a convenience function which formulates the appropriate control
 * message to retrieve the descriptor. The string returned is Unicode, as
 * detailed in the USB specifications.
 *
 * \param dev a device handle
 * \param desc_index the index of the descriptor to retrieve
 * \param langid the language ID for the string descriptor
 * \param data output buffer for descriptor
 * \param length size of data buffer
 * \returns number of bytes returned in data, or LIBUSB_ERROR code on failure
 * \see libusb_get_string_descriptor_ascii()
 *)
Function libusb_get_string_descriptor(dev:Plibusb_device_handle;desc_index:cuint8;langid:cuint16;data:pcuchar;length:cint):cint;

Function libusb_get_string_descriptor_ascii(dev:Plibusb_device_handle;desc_index:cuint8;data:pcuchar;length:cint):cint;extdecl;external;

(* polling and timeouts *)

function libusb_try_lock_events(ctx:Plibusb_context):cint;extdecl;external;
procedure libusb_lock_events(ctx:Plibusb_context);extdecl;external;
procedure libusb_unlock_events(ctx:Plibusb_context);extdecl;external;
function libusb_event_handling_ok(ctx:Plibusb_context):cint;extdecl;external;
function libusb_event_handler_active(ctx:Plibusb_context):cint;extdecl;external;
procedure libusb_lock_event_waiters(ctx:Plibusb_context);extdecl;external;
procedure libusb_unlock_event_waiters(ctx:Plibusb_context);extdecl;external;
function libusb_wait_for_event(ctx:Plibusb_context; tv:Ptimeval):cint;extdecl;external;

function libusb_handle_events_timeout(ctx:Plibusb_context; tv:Ptimeval):cint;extdecl;external;
function libusb_handle_events_timeout_completed(ctx:Plibusb_context; tv:Ptimeval; completed:Pcint):cint;extdecl;external;
function libusb_handle_events(ctx:Plibusb_context):cint;extdecl;external;
function libusb_handle_events_completed(ctx:Plibusb_context; completed:Pcint):cint;extdecl;external;
function libusb_handle_events_locked(ctx:Plibusb_context; tv:Ptimeval):cint;extdecl;external;
function libusb_pollfds_handle_timeouts(ctx:Plibusb_context):cint;extdecl;external;
function libusb_get_next_timeout(ctx:Plibusb_context; tv:Ptimeval):cint;extdecl;external;

type
  PPlibusb_pollfd = ^Plibusb_pollfd;
  Plibusb_pollfd = ^libusb_pollfd;
  (** \ingroup poll
   * File descriptor for polling
   *)
  libusb_pollfd = record
    (** Numeric file descriptor *)
    fd : cint;
    (** Event flags to poll for from <poll.h>. POLLIN indicates that you
     * should monitor this file descriptor for becoming ready to read from,
     * and POLLOUT indicates that you should monitor this file descriptor for
     * nonblocking write readiness. *)
    events : cshort;
  end;

  (** \ingroup poll
   * Callback function, invoked when a new file descriptor should be added
   * to the set of file descriptors monitored for events.
   * \param fd the new file descriptor
   * \param events events to monitor for, see \ref libusb_pollfd for a
   * description
   * \param user_data User data pointer specified in
   * libusb_set_pollfd_notifiers() call
   * \see libusb_set_pollfd_notifiers()
   *)
  libusb_pollfd_added_cb = procedure (fd:cint; events:cshort; user_data:pointer);extdecl;

  (** \ingroup poll
   * Callback function, invoked when a file descriptor should be removed from
   * the set of file descriptors being monitored for events. After returning
   * from this callback, do not use that file descriptor again.
   * \param fd the file descriptor to stop monitoring
   * \param user_data User data pointer specified in
   * libusb_set_pollfd_notifiers() call
   * \see libusb_set_pollfd_notifiers()
   *)
  libusb_pollfd_removed_cb = procedure (fd:cint; user_data:pointer);extdecl;

function libusb_get_pollfds(ctx:Plibusb_context):PPlibusb_pollfd;extdecl;external;     (* Const before type ignored *)
procedure libusb_set_pollfd_notifiers(ctx:Plibusb_context; added_cb:libusb_pollfd_added_cb; removed_cb:libusb_pollfd_removed_cb; user_data:pointer);extdecl;external;

Implementation

Function libusb_cpu_to_le16(x:cuint16) : cuint16;
Begin
  Result := NtoLE(x);
End;

Function libusb_le16_to_cpu(x:cuint16) : cuint16;
Begin
  Result := LEtoN(x);
End;

Function libusb_control_transfer_get_data(transfer:Plibusb_transfer):Pointer;
Begin
  Result := Transfer^.Buffer + LIBUSB_CONTROL_SETUP_SIZE;
End;

Function libusb_control_transfer_get_setup(transfer:Plibusb_transfer):Plibusb_control_setup;
Begin
  Result := Plibusb_control_setup(transfer^.buffer);
End;

Procedure libusb_fill_control_setup(Out buffer;bmRequestType:cuint8;bRequest:cuint8;wValue:cuint16;wIndex:cuint16;wLength:cuint16);
Var setup : Plibusb_control_setup;
Begin
  setup := Plibusb_control_setup(@buffer);
  setup^.bmRequestType := bmRequestType;
  setup^.bRequest      := bRequest;
  setup^.wValue        := libusb_cpu_to_le16(wValue);
  setup^.wIndex        := libusb_cpu_to_le16(wIndex);
  setup^.wLength       := libusb_cpu_to_le16(wLength);
End;

Procedure libusb_fill_control_transfer(
  transfer:Plibusb_transfer;
  dev_handle:Plibusb_device_handle;
  Out buffer;
  callback:libusb_transfer_cb_fn;
  user_data:Pointer;
  timeout : cuint);
Var setup : Plibusb_control_setup;
Begin
  setup := Plibusb_control_setup(@buffer);
  transfer^.dev_handle := dev_handle;
  transfer^.endpoint   := 0;
  transfer^._type      := LIBUSB_TRANSFER_TYPE_CONTROL;
  transfer^.timeout    := timeout;
  transfer^.buffer     := @buffer;
  if setup <> Nil then
    transfer^.length   := LIBUSB_CONTROL_SETUP_SIZE + libusb_le16_to_cpu(setup^.wLength);
  transfer^.user_data  := user_data;
  transfer^.callback   := callback;
End;

Procedure libusb_fill_bulk_transfer(
  transfer:Plibusb_transfer;
  dev_handle:Plibusb_device_handle;
  endpoint:cuchar;
  Out buffer;
  length:cint;
  callback:libusb_transfer_cb_fn;
  user_data:Pointer;
  timeout:cuint);
Begin
  transfer^.dev_handle := dev_handle;
  transfer^.endpoint   := endpoint;
  transfer^._type      := LIBUSB_TRANSFER_TYPE_BULK;
  transfer^.timeout    := timeout;
  transfer^.buffer     := @buffer;
  transfer^.length     := length;
  transfer^.user_data  := user_data;
  transfer^.callback   := callback;
End;

Procedure libusb_fill_interrupt_transfer(
  transfer:Plibusb_transfer;
  dev_handle:Plibusb_device_handle;
  endpoint:cuchar;
  Out buffer;
  length:cint;
  callback:libusb_transfer_cb_fn;
  user_data:Pointer;
  timeout:cuint);
Begin
  transfer^.dev_handle := dev_handle;
  transfer^.endpoint   := endpoint;
  transfer^._type      := LIBUSB_TRANSFER_TYPE_INTERRUPT;
  transfer^.timeout    := timeout;
  transfer^.buffer     := @buffer;
  transfer^.length     := length;
  transfer^.user_data  := user_data;
  transfer^.callback   := callback;
End;

Procedure libusb_fill_iso_transfer(
  transfer:Plibusb_transfer;
  dev_handle:Plibusb_device_handle;
  endpoint:cuchar;
  Out buffer;
  length:cint;
  num_iso_packets:cint;
  callback:libusb_transfer_cb_fn;
  user_data:Pointer;
  timeout:cuint);
Begin
  transfer^.dev_handle      := dev_handle;
  transfer^.endpoint        := endpoint;
  transfer^._type           := LIBUSB_TRANSFER_TYPE_ISOCHRONOUS;
  transfer^.timeout         := timeout;
  transfer^.buffer          := @buffer;
  transfer^.length          := length;
  transfer^.num_iso_packets := num_iso_packets;
  transfer^.user_data       := user_data;
  transfer^.callback        := callback;
End;

Procedure libusb_set_iso_packet_lengths(transfer:Plibusb_transfer;length:cuint);
Var I : Integer;
Begin
  For I := 0 to transfer^.num_iso_packets-1 do
    transfer^.iso_packet_desc[I].length := length;
End;

Function libusb_get_iso_packet_buffer(transfer:Plibusb_transfer;packet:cuint):Pointer;
Var I : Integer;
    Offset : csize;
Begin
  (* oops..slight bug in the API. packet is an unsigned int, but we use
   * signed integers almost everywhere else. range-check and convert to
   * signed to avoid compiler warnings. FIXME for libusb-2. *)
  if packet > MaxInt then
    Exit(Nil);

  if packet >= transfer^.num_iso_packets then
    Exit(Nil);

  Offset := 0;
  For I := 0 to packet-1 do
    Offset := Offset + transfer^.iso_packet_desc[I].length;

  Result := transfer^.buffer + offset;
End;

Function libusb_get_iso_packet_buffer_simple(transfer:Plibusb_transfer;packet:cuint):Pointer;
Begin
  (* oops..slight bug in the API. packet is an unsigned int, but we use
   * signed integers almost everywhere else. range-check and convert to
   * signed to avoid compiler warnings. FIXME for libusb-2. *)
  if packet > MaxInt then
    Exit(Nil);

  if packet >= transfer^.num_iso_packets then
    Exit(Nil);

  Result := transfer^.buffer + (transfer^.iso_packet_desc[0].length * packet);
End;

Function libusb_get_descriptor(dev:Plibusb_device_handle;desc_type:cuint8;desc_index:cuint8;data:pcuchar;length:cint):cint;
Begin
  Result := libusb_control_transfer(dev, cuint8(LIBUSB_ENDPOINT_IN),
    cuint8(LIBUSB_REQUEST_GET_DESCRIPTOR), (desc_type shl 8) or desc_index, 0, data,
    cuint16(length), 1000);
End;

Function libusb_get_string_descriptor(dev:Plibusb_device_handle;desc_index:cuint8;langid:cuint16;data:pcuchar;length:cint):cint;
Begin
  Result := libusb_control_transfer(dev, cuint8(LIBUSB_ENDPOINT_IN),
    cuint8(LIBUSB_REQUEST_GET_DESCRIPTOR), cuint16(cuint8(LIBUSB_DT_STRING) shl 8) or desc_index,
    langid, data, cuint16(length), 1000);
End;

End.

