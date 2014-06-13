{
  Copyright 2006 Michalis Kamburelis.

  This file is part of "grammar_compression".

  "grammar_compression" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "grammar_compression" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "grammar_compression"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ }
unit BitStreams;

interface

uses Classes;

type
  { This class allows to write to Stream LongWord values that are packed
    on less than 32 bits. When starting writing, you specify
    MaxValue, the highest possible LongWord value that you will ever write.
    This class calculates how many bits are needed to express any value
    between 0 and MaxValue, and uses these many bits to write
    all the following values in WriteValue. }
  TStreamBitWriter = class
  private
    FMaxValue: LongWord;
    FStream: TStream;
    FBitsPerValue: Cardinal;

    { Actually only the lowest 8 bits of Buffer will be used
      always when calling any method. But inside implementation
      of WriteValue, it's useful to have Buffer as 64-bit
      (so it can always hold one additional LongWord value). }
    Buffer: QWord;
    BufferedBits: Cardinal;
  public
    constructor Create(AStream: TStream; const AMaxValue: LongWord);
    destructor Destroy; override;

    property MaxValue: LongWord read FMaxValue;
    property BitsPerValue: Cardinal read FBitsPerValue;
    property Stream: TStream read FStream;

    procedure WriteValue(const Value: LongWord);
  end;

  TStreamBitReader = class
  private
    FMaxValue: LongWord;
    FStream: TStream;
    FBitsPerValue: Cardinal;

    { Just like with TStreamBitWriter:

      Actually only the lowest 8 bits of Buffer will be used
      always when calling any method. But inside implementation
      of WriteValue, it's useful to have Buffer as 64-bit
      (so it can always hold one additional LongWord value). }
    Buffer: QWord;
    BufferedBits: Cardinal;
  public
    constructor Create(AStream: TStream; const AMaxValue: LongWord);
    destructor Destroy; override;

    property MaxValue: LongWord read FMaxValue;
    property BitsPerValue: Cardinal read FBitsPerValue;
    property Stream: TStream read FStream;

    function ReadValue: LongWord;
  end;

function BitsPerSymbolFromMaxValue(const MaxValue: Cardinal): Cardinal;

implementation

uses CastleUtils, CastleClassUtils;

function BitsPerSymbolFromMaxValue(const MaxValue: Cardinal): Cardinal;
begin
  { I want to be able to write numbers 0..MaxValue.

    Test: for
      MaxValue = 0, 1  = 2^1 - 1 -> 1
      MaxValue = 2, 3  = 2^2 - 1 -> 2
      MaxValue = 4...7 = 2^3 - 1 -> 3
    Yes, it's OK. }

  Result := Smallest2Exponent(MaxValue + 1);
end;

{ TStreamBitWriter ----------------------------------------------------------- }

constructor TStreamBitWriter.Create(AStream: TStream; const AMaxValue: LongWord);
begin
  inherited Create;
  FStream := AStream;
  FMaxValue := AMaxValue;
  FBitsPerValue := BitsPerSymbolFromMaxValue(MaxValue);
  BufferedBits := 0;
end;

destructor TStreamBitWriter.Destroy;
begin
  if Stream <> nil then
  begin
    { Flush buffer }
    while BufferedBits >= 8 do
    begin
      StreamWriteByte(Stream, Byte(Buffer and $FF));
      Buffer := Buffer shr 8;
      BufferedBits -= 8;
    end;
    if BufferedBits > 0 then
      { Flush last bits.
        There are > 0 and < 8 bits left to write,
        so some bits at the end are wasted and meaningless. }
      StreamWriteByte(Stream, Byte(Buffer and $FF));
  end;

  inherited;
end;

procedure TStreamBitWriter.WriteValue(const Value: LongWord);
begin
  Buffer := Buffer or (QWord(Value) shl BufferedBits);
  BufferedBits += BitsPerValue;
  while BufferedBits >= 8 do
  begin
    StreamWriteByte(Stream, Byte(Buffer and $FF));
    Buffer := Buffer shr 8;
    BufferedBits -= 8;
  end;
end;

{ TStreamBitReader ----------------------------------------------------------- }

constructor TStreamBitReader.Create(AStream: TStream; const AMaxValue: LongWord);
begin
  inherited Create;
  FStream := AStream;
  FMaxValue := AMaxValue;
  FBitsPerValue := BitsPerSymbolFromMaxValue(MaxValue);
end;

destructor TStreamBitReader.Destroy;
begin
  inherited;
end;

function TStreamBitReader.ReadValue: LongWord;
begin
  while BufferedBits < BitsPerValue do
  begin
    Buffer := Buffer or (QWord(StreamReadByte(Stream)) shl BufferedBits);
    BufferedBits += 8;
  end;

  Result := Buffer and ((1 shl BitsPerValue) - 1);

  Buffer := Buffer shr BitsPerValue;
  BufferedBits -= BitsPerValue;
end;

end.