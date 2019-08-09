(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   Test program demonstrating usage of the libusb context.               *
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
Program Test1Library;

{$mode objfpc}{$H+}

Uses LibUsb,LibUsbOop,SysUtils;

Const SpeedName : Array[0..4] of String = (
       'Unknown',
       '1.5 Mbit/s (USB LowSpeed)',
       '12 Mbit/s (USB FullSpeed)',
       '480 Mbit/s (USB HighSpeed)',
       '5000 Mbit/s (USB SuperSpeed)');

Var Version  : Plibusb_version;
    Context  : TLibUsbContext;
    DevList  : PPlibusb_device;
    DevCount : Integer;
    Addr     : Byte;
    Bus      : Byte;
    Port     : Byte;
    PortPath : TDynByteArray;
    Speed    : Byte;
    DevDesc  : libusb_device_descriptor;
    I,J      : Integer;

Begin
  // get library version
  Version := TLibUsbContext.GetVersion;
  With Version^ do
    WriteLn('Using libusb(x) v',major,'.',minor,'.',micro,'.',nano);

  // create context
  Context := TLibUsbContext.Create;
  try
    DevCount := ELibUsb.Check(Context.GetDeviceList(DevList),'GetDeviceList');
    WriteLn('Found ',DevCount,' devices:');
    // list all devices
    For I := 0 to DevCount-1 do
      Begin
        Addr     := TLibUsbContext.GetDeviceAddress   (DevList[I]);
        Bus      := TLibUsbContext.GetBusNumber       (DevList[I]);
        Port     := TLibUsbContext.GetPortNumber      (DevList[I]);
        PortPath :=        Context.GetPortPath        (DevList[I]);
        Speed    := TLibUsbContext.GetDeviceSpeed     (DevList[I]);
        DevDesc  := TLibUsbContext.GetDeviceDescriptor(DevList[I]);
        Write('  Bus ',Bus:3,' Device ',Addr:3,': ID ',IntToHex(DevDesc.idVendor,4),':',IntToHex(DevDesc.idProduct,4));
        Write(',  port: ',Port:3);
        if Length(PortPath) > 0 then
          Begin
            Write(', port path from HCD: ',PortPath[0]);
            For J := 1 to Length(PortPath)-1 do
              Write('->',PortPath[J]);
          End;
        Write(', Speed: ');
        if Speed < High(SpeedName) then
          Write(SpeedName[Speed])
        else
          Write('unknown (',Speed,')');
        WriteLn;
      End;
    Context.FreeDeviceList(DevList);
  finally
    Context.Free;
  End;
End.

