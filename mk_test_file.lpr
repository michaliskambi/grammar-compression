{ -*- compile-command: "make build-debug" -*- }

{ Generate on stdout $2 characters, taken from random pool of
  $1 distinct characters. }

uses SysUtils, CastleUtils, CastleParameters;

var
  CharsToUse: array [Byte] of char;
  I: Integer;
  CharsCount, Size: Integer;
  B: Byte;
begin
  Parameters.CheckHigh(2);
  CharsCount := Clamped(StrToInt(Parameters[1]), 1, 256);
  Size := StrToInt(Parameters[2]);

  { initialize CharsToUse[0..CharsCount - 1] to random (different) characters }
  for B := Low(B) to High(B) do
    CharsToUse[B] := Chr(B);
  for I := 0 to CharsCount - 1 do
    SwapValues(CharsToUse[I], CharsToUse[
      { Random from I ... 255 } I + Random(256 - I)]);

  for I := 0 to Size - 1 do
    Write(CharsToUse[Random(CharsCount)]);
end.