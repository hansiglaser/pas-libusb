Unit LibUsbUtil;

{$mode objfpc}{$H+}

Interface

Uses LibUsb,LibUsbOop;

Type

  { TLibUsbDeviceWithFirmware }

  TLibUsbDeviceWithFirmware = class(TLibUsbDevice)
  protected
    Procedure Configure(ADev:Plibusb_device); virtual; abstract;
  public
    Constructor Create(AContext : TLibUsbContext;MatchUnconfigured,MatchConfigured:TLibUsbDeviceMatchClass); overload;
  End;

Implementation
Uses SysUtils;

{ TLibUsbDeviceWithFirmware }

Constructor TLibUsbDeviceWithFirmware.Create(AContext : TLibUsbContext;MatchUnconfigured,MatchConfigured:TLibUsbDeviceMatchClass);
Var Devs    : TLibUsbDeviceArray;
    Timeout : Integer;
Begin
  SetLength(Devs,0);
  // find configured devices, but don't complain if non were found
  Devs := AContext.FindDevices(MatchConfigured,false,0);

  if Length(Devs) = 0 then
    Begin
      // find unconfigured devices
      Devs := AContext.FindDevices(MatchUnconfigured,false);

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

  { find configured devices }
  if Length(Devs) = 0 then
    Devs := AContext.FindDevices(MatchConfigured,true,Timeout);
  if Length(Devs) = 0 then
    raise EUSBError.Create('No configured devices found.')
  else if Length(Devs) > 1 then
    raise EUSBError.Create('Too many configured devices found.');

  inherited Create(AContext,Devs[0]);
End;

End.

