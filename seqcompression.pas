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

{ Implementation of Sequitur and Sequential compression algorithms. }
unit SeqCompression;

interface

uses Classes, Grammar;

procedure CompressSequitur(StartProduction: TProduction; InputStream: TStream);

procedure CompressSequential(var StartProduction: TProduction;
  InputStream: TStream);

implementation

uses SysUtils, DigraphsListUnit, KambiClassUtils, KambiStringUtils;

var
  DigraphsList: TDigraphsList;
  CalculateCollectedValues: boolean;

{ $define DEBUG_SEQ_COMPRESSION}

function CorrectDigraph(Symbol: TSymbol): boolean; forward;

{ Substitute Symbol and Symbol.Next with new non-terminal with production
  NewProduction. Note that this assumes that digraph between
  Symbol and Symbol.Next is already removed from DigraphsList.

  Note that this removes Symbol and Symbol.Next, but it doesn't check
  whether some rule becomes underused (i.e. it's UsedCount = 1
  and should be expanded). You should do this yourself after executing this. }
procedure Substitute(Symbol: TSymbol; NewProduction: TProduction);
var
  NewSymbol: TSymbol;
  SymbolPrevious, SymbolNext, SymbolNextNext: TSymbol;
begin
  {$ifdef DEBUG_SEQ_COMPRESSION}
  Writeln('Substituting digraph ' + Symbol.Name + ' + ' +
    Symbol.Next.Name +' with ' + NewProduction.Name);
  {$endif}

  { Save some things, because we will destroy symbols soon. }
  SymbolPrevious := Symbol.Previous;
  SymbolNext := Symbol.Next;
  SymbolNextNext := Symbol.Next.Next;

  if not Symbol.IsFirst then
    DigraphsList.DeleteDigraph(SymbolPrevious);
  if not SymbolNext.IsLast then
    DigraphsList.DeleteDigraph(SymbolNext);

  FreeAndNil(Symbol);
  FreeAndNil(SymbolNext);
  SymbolPrevious.Next := SymbolNextNext;

  NewSymbol := TNonTerminal.Create(NewProduction);
  SymbolPrevious.InsertNext(NewSymbol);

  { Inserting NewSymbol creates 2 new digraphs.
    They should be added, and corrected.

    Note that if 1st CorrectDigraph(NewSymbol) call returns @true,
    then it means that it called Substitute on digraph
    starting on NewSymbol.Previous. Assertion is: Substitute creates
    border digraphs. So CorrectDigraph(NewSymbol) below already created
    digraph between SymbolNextNext and previous symbol (that can
    no longer be NewSymbol; in fact, SymbolNextNext is possibly freed
    and replaced by something else now ?). So we
    1. Don't have to call CorrectDigraph(SymbolNextNext)
    2. In fact, we can't call CorrectDigraph(SymbolNextNext)
  }
  if CorrectDigraph(NewSymbol) then
    if not (SymbolNextNext is TGuard) then
      CorrectDigraph(SymbolNextNext);
end;

{ If this is non-terminal and it's the only user of it's
  production, then expand it. Expand means that whole production
  is placed where the symbol was, digraphs are deleted and added
  as necessary. Assumes that productions are never empty. }
procedure ExpandIfUnderusedProduction(Symbol: TSymbol);
var
  P: TProduction;
  S1, S2: TSymbol;
begin
  if (Symbol is TNonTerminal) and
     (TNonTerminal(Symbol).Production.UsedCount = 1) then
  begin
    {$ifdef DEBUG_SEQ_COMPRESSION}
    Writeln('Expanding ', Symbol.Name);
    {$endif}

    P := TNonTerminal(Symbol).Production;
    S1 := P.FirstSymbol;
    S2 := P.LastSymbol;

    Symbol.Previous.Next := S1;
    S1.Previous := Symbol.Previous;
    Symbol.Next.Previous := S2;
    S2.Next := Symbol.Next;
    P.ReleaseSymbols;

    { remove symbol, with related digraphs. }
    if not Symbol.IsFirst then
      DigraphsList.DeleteDigraph(Symbol.Previous);
    if not Symbol.IsLast then
      DigraphsList.DeleteDigraph(Symbol);
    FreeAndNil(Symbol);

    Assert(S1.IsFirst xor S2.IsLast);
    if S1.IsFirst then
      CorrectDigraph(S2.Next) else
      CorrectDigraph(S1);
  end;
end;

{ Search for digraph starting at Symbol.Previous.
  Add this digraph to the list, or (if such digraph already
  exists) replace this by some production. Returns @true if simple
  digraph addition was enough (or symbol was first in it's production). }
function CorrectDigraph(Symbol: TSymbol): boolean;
var
  NewProduction: TProduction;
  FoundDigraph: TSymbol;
begin
  Result := true;
  if not Symbol.IsFirst then
  begin
    {$ifdef DEBUG_SEQ_COMPRESSION}
    Writeln('Correcting digraph ' + Symbol.Previous.Name + ' + ' +
      Symbol.Name);
    {$endif}

    FoundDigraph := DigraphsList.IndexOfDigraph(Symbol.Previous, Symbol);
    Result := FoundDigraph = nil;

    if Result then
    begin
      DigraphsList.AddDigraph(Symbol.Previous);
    end else
    begin
      if FoundDigraph.IsFirst and FoundDigraph.Next.IsLast then
      begin
        NewProduction := (FoundDigraph.Previous as TGuard).ParentProduction;
        Substitute(Symbol.Previous, NewProduction);
      end else
      begin
        NewProduction := TProduction.Create;
        NewProduction.LastSymbol.InsertNext(
          FoundDigraph.CreateCopyContents);
        NewProduction.LastSymbol.InsertNext(
          FoundDigraph.Next.CreateCopyContents);
        if CalculateCollectedValues then
          NewProduction.SimpleCalculateCollectedValue;

        { For safety Substitute requires that digraph
          between two removed symbols should be already removed.
          So we remove FoundDigraph first, and then add
          NewProduction's digraph.

          Note that we can safely add
          NewProduction's digraph, no need to check anything ---
          nothing else equal to FoundDigraph may exist now in DigraphsList.
          Even if FoundDigraph was part of the triple, now (i.e. after
          the substitutes) it will not be part of the triple, so no trouble. }
        DigraphsList.DeleteDigraph(FoundDigraph);
        DigraphsList.AddDigraph(NewProduction.FirstSymbol);

        { Note that at the end of first Substitute call,
          we call CorrectDigraph at the both ends of NewSymbol
          (from NewProduction). Is it harmless ?
          I.e., what if FoundDigraph was 2 symbols
          after Symbol.Previous, or vice versa. E.g. when we have "baba"
          we want to convert it to "AA" with "A -> ba".
          Yes, this is harmless. That's because in this case "A" is
          NewProduction, so it will not be merged with anything.

          In fact, in the 1st Substitute call below, we're sure that
          both CorrectDigraph calls will return true, because NewProduction
          is new digraph. In the 2nd Substitute call some calls
          to CorrectDigraph may return false (i.e. do more substitutions),
          to merge digraphs like "XA" or "AX",
          where X is non-terminal or terminal. But for sure X is not A,
          as such digraph can only occur once for now. Actually, "XA"
          case is not possible (if all Sequitur properties were satisfied),
          because this would mean that digraph "X" + Symbol.Previous was
          already duplicated. }
        Substitute(Symbol.Previous, NewProduction);
        Substitute(FoundDigraph, NewProduction);
      end;

      { Proof that second char cannot be underused: suppose it can.
        Name the underused non-terminal ,,A''. Name the fist symbol ,,x''.
        We would need exactly two sequences ,,x A'' at some point
        of our updating work (it can't be so at the beginning of work,
        since all Sequitur rules are guaranteed at that point,
        so ,,x A'' digraph is only once at that point). For example

          P2 -> ... x A ...
          P1 -> ... x A ...

        or

          P1 -> ... x A ... x A ...

        So there must be some non-terminal that gets expanded,
        in the middle of our ,,grammar correcting'' work, to ,,... x''.
        So

          P2 -> ... B A ...
          P1 -> ... x A ...
          B -> ... x

        or

          P1 -> ... x A ... B A ...
          B -> ... x

        But how to expand B ? Well, we must place B as a 1st char of some
        rule. So we must have P2 -> B A (exactly, without ,,...'' around). So

          P2 -> B A
          P1 -> ... x A ...
          B -> ... x

        So we must somehow force expansion of B. So B must be used elsewhere,
        exactly twice, as this is the only way to cause creating new
        rule P2 -> B A, thus underusing B.
        So ,,B A'' must occur twice. But this means
        that A occurs *more than twice* ! So A will not get underused.

        So ExpandIfUnderusedProduction(NewProduction.LastSymbol);
        is not needed. }
      ExpandIfUnderusedProduction(NewProduction.FirstSymbol);
    end;
  end;
end;

procedure CompressSequitur(StartProduction: TProduction; InputStream: TStream);
var
  C: char;
  NewSymbol: TSymbol;
begin
  CalculateCollectedValues := false;
  DigraphsList := TDigraphsList.Create;
  try
    while InputStream.Read(C, 1) <> 0 do
    begin
      NewSymbol := TTerminal.Create(C);
      StartProduction.LastSymbol.InsertNext(NewSymbol);
      CorrectDigraph(NewSymbol);
      { Tests: }
      { StartProduction.GraphSaveToFile(FNameAutoInc('test_%d.dot'), true); }
    end;
  finally FreeAndNil(DigraphsList) end;
end;

procedure CompressSequential(var StartProduction: TProduction;
  InputStream: TStream);

  procedure AddNewSymbol(NewSymbol: TSymbol; const NewSymbolValue: string);
  begin
    StartProduction.LastSymbol.InsertNext(NewSymbol);
    StartProduction.CollectedValue := StartProduction.CollectedValue +
      NewSymbolValue;
    CorrectDigraph(NewSymbol);
  end;

  procedure DuplicateStartProduction(NewSymbol: TSymbol;
    const NewSymbolValue: string);
  var
    NewStartProduction: TProduction;
  begin
    NewStartProduction := TProduction.Create;
    NewStartProduction.LastSymbol.InsertNext(
      TNonTerminal.Create(StartProduction));
    NewStartProduction.LastSymbol.InsertNext(NewSymbol);
    StartProduction := NewStartProduction;
    StartProduction.CollectedValue := NewSymbolValue + NewSymbolValue;
    { No need to call CorrectDigraph here }
  end;

var
  NewSymbol: TSymbol;
  Buffer: string;
  I: Integer;
  MaxPrefixLength, MaxPrefixIndex: Integer;
begin
  CalculateCollectedValues := true;

  Buffer := ReadGrowingStreamToString(InputStream);

  AllProductionsList := TProductionsList.Create;
  try
    AllProductionsList.Add(StartProduction);

    DigraphsList := TDigraphsList.Create;
    try
      while Buffer <> '' do
      begin
        { search longest prefix of Buffer within productions. }
        MaxPrefixLength := 0;
        for I := 0 to AllProductionsList.High do
        begin
          if IsPrefix(AllProductionsList[I].CollectedValue, Buffer, false) and
             (Length(AllProductionsList[I].CollectedValue) > MaxPrefixLength) then
          begin
            MaxPrefixIndex := I;
            MaxPrefixLength := Length(AllProductionsList[I].CollectedValue);
          end;
        end;

        { calculate NewSymbol, update MaxPrefixLength to 1 for terminal }
        if MaxPrefixLength <> 0 then
        begin
          NewSymbol := TNonTerminal.Create(AllProductionsList[MaxPrefixIndex]);

          { Handle special case when whole StartProduction was a prefix
           of Buffer. }
          if StartProduction = AllProductionsList[MaxPrefixIndex] then
            DuplicateStartProduction(NewSymbol, Copy(Buffer, 1, MaxPrefixLength)) else
            AddNewSymbol(NewSymbol, Copy(Buffer, 1, MaxPrefixLength));

          {$ifdef DEBUG_SEQ_COMPRESSION}
          Writeln(Format('Buffer "%s", found non-terminal %s (prefix "%s")',
            [ Buffer, NewSymbol.Name,
              AllProductionsList[MaxPrefixIndex].CollectedValue ]));
          {$endif}
        end else
        begin
          NewSymbol := TTerminal.Create(Buffer[1]);
          MaxPrefixLength := 1;
          AddNewSymbol(NewSymbol, Buffer[1]);
          {$ifdef DEBUG_SEQ_COMPRESSION}
          Writeln(Format('Buffer "%s", adding terminal %s',
            [ Buffer, NewSymbol.Name ]));
          {$endif}
        end;

        Delete(Buffer, 1, MaxPrefixLength);

        { Tests: }
        { StartProduction.GraphSaveToFile(FNameAutoInc('test_%d.dot'), true); }
      end;
    finally FreeAndNil(DigraphsList) end;
  finally FreeAndNil(AllProductionsList) end;
end;

end.