Program Test1Library;

(*Nächste Schritte:
 - neues testprogramm test3controlasync, soll nur einen String holen oder so,
   dass die Aufmerksamkeit nicht vom wesentlichen abgelenkt wird*)

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
              Write('->',PortPath[I]);
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

