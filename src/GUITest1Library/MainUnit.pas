unit MainUnit;

{$mode objfpc}{$H+}

(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   Test program demonstrating usage of the libusb context.               *
 *                                                                         *
 *   Converted to a Lazaurs GUI program by BREAKOUTBOX 2020-12-23          *
 *                                                                         *
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

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LibUsb, {LibUsbUtil,} LibUsbOop;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

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

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Self.Close;
end;

procedure TForm1.Button2Click(Sender: TObject);
var s:string;
begin
  // ...
  Button2.Visible:= FALSE;
  Application.ProcessMessages;

  // get library version
  Version := TLibUsbContext.GetVersion;
  Memo1.Clear;
  Memo1.Append( 'Using libusb(x) v'
                +IntToStr( Version^.major) +'.'
                +IntToStr( Version^.minor) +'.'
                +IntToStr( Version^.micro) +'.'
                +IntToStr( Version^.nano) +#13#10);

  // create context
  Context := TLibUsbContext.Create;
  try
    DevCount := ELibUsb.Check( Context.GetDeviceList( DevList), 'GetDeviceList');
    Memo1.Append( 'Found ' +IntToStr( DevCount) +' devices:');

    // list all devices
    for I := 0 to DevCount-1 do
      begin
        Addr     := TLibUsbContext.GetDeviceAddress   ( DevList[I]);
        Bus      := TLibUsbContext.GetBusNumber       ( DevList[I]);
        Port     := TLibUsbContext.GetPortNumber      ( DevList[I]);
        PortPath :=        Context.GetPortPath        ( DevList[I]);
        Speed    := TLibUsbContext.GetDeviceSpeed     ( DevList[I]);
        DevDesc  := TLibUsbContext.GetDeviceDescriptor( DevList[I]);

        Memo1.Append( '');
        Memo1.Append(  ' Bus'       +#9#9#9 +IntToStr( Bus)  +#13#10
                      +' Device'    +#9#9#9 +IntToStr( Addr) +#13#10
                      +' idVendor'  +#9#9   +IntToHex( DevDesc.idVendor, 4)  +#13#10
                      +' idProduct' +#9#9   +IntToHex( DevDesc.idProduct, 4) +#13#10
                      +' port'      +#9#9#9 +IntToStr( Port));
        if Length( PortPath) > 0 then
          begin
            s:= ' port path from HCD: ' +#9 +IntToStr( PortPath[0]) +' ';
            for J := 1 to Length( PortPath) -1
              do s:= s +IntToStr( PortPath[J]);
            Memo1.Append( s);
          end;

        s:= ' Speed: ';
        if Speed < High( SpeedName)
          then Memo1.Append( s +#9#9#9 +SpeedName[Speed])
          else Memo1.Append( s +#9#9#9 +'unknown (' +IntToStr( Speed) +')');
      end;
    Context.FreeDeviceList( DevList);
  finally
    Context.Free;
  end;

  // ...
  Button2.Visible:= TRUE;
end;


end.

