{$MODE OBJFPC}
Unit PasLibUsbUtils;

Interface
Uses SysUtils,StrUtils,Classes,BaseUnix,Unix;

Const HexChars = '0123456789ABCDEF';

Function HexToInt(St:ShortString):Int64;
Function GetUSec : UInt64;

Implementation

Function HexToInt(St:ShortString):Int64;
Var I,J : LongInt;
Begin
  Result := 0;
  For I := 1 to Length(St) do
    Begin
      J := Pos(UpCase(St[I]),HexChars);
      if J = 0 then
        raise EConvertError.CreateFmt('Wrong hex char "%s" in hex number "%s" at position %d.',[St[I],St,I]);
      Result := Result shl 4 or (J-1);
    End;
End;

Function GetUSec : UInt64;
Var TZ : TimeVal;
Begin
  fpGetTimeOfDay(@TZ,Nil);
  Result := TZ.tv_usec + TZ.tv_sec*1000000;
End;

End.
