(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   Utility functions.                                                    *
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
