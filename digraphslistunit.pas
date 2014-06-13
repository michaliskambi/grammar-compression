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
unit DigraphsListUnit;

interface

uses SysUtils, KambiUtils, KambiClassUtils, Grammar;

{$define read_interface}

type
  TObjectsListItem_1 = TSymbol;
  {$I objectslist_1.inc}
  TSymbolsList = TObjectsList_1;

  { List of digraphs.

    Note that you @italic(have) to delete digraphs @italic(before) their meaning
    changes (e.g. before Symbol's Next property changes, or before
    you free symbol etc.).

    Note that it's allowed to have more than one digraph with the same
    value, i.e. where terminal's values or non-terminal's productions
    are equal. That's needed to handle triples, e.g. "aaa" ---
    in such case you can think that you have two "aa"
    digraphs inside this list. IndexOfDigraph is smart and will actually
    not indicate equality between two "aa" digraphs that are linked like this. }
  TDigraphsList = class
  private
    Items: TSymbolsList;
    { This is important only where Items[I] = nil }
    ItemsDeleted: TDynBooleanArray;

    { Find digraph starting with S1 (exactly S1, it compares references). }
    function IndexOfExactDigraph(S1: TSymbol): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    { Searches for digraph S1, S2 by comparing Values of terminals and Productions
      of non-terminals (so it doesn't exactly compare S1 and S2
      references).

      Returns nil if not found, or TSymbol reference if found
      (to the first item of digraph --- 2nd item is the Next one).

      Note that this omits finding triples, by comparing TSymbol references:
      it never returns symbol that is = S2, or symbol that has Next equal
      to S1. }
    function IndexOfDigraph(const S1, S2: TSymbol): TSymbol;

    { Find digraph starting with S1 (exactly S1, it compares references)
      and delete it.

      If digraph doesn't exist, this is ignored (no error is raised etc.)
      This behavior is actually used by Substitute implementation of
      Sequitur, where border digraphs need not exist ---
      see comments at the end of Substitute implementation,
      where we call CorrectDigraph). }
    procedure DeleteDigraph(S1: TSymbol);

    procedure AddDigraph(S: TSymbol);
  end;

{$undef read_interface}

implementation

{$define read_implementation}
{$I objectslist_1.inc}

const
  HashArraySize = 2265539;

function SymbolValue(S: TSymbol): TPointerUInt;
begin
  if S is TNonTerminal then
    Result := TPointerUInt(TNonTerminal(S).Production) else
    Result := Ord(TTerminal(S).Value);
end;

{$I norqcheckbegin.inc}

function Hash(One, Two: TSymbol): Cardinal;
begin
  Result := ((SymbolValue(One) shl 16) or SymbolValue(Two)) mod HashArraySize;
end;

function Hash2(One: TSymbol): Cardinal;
begin
  Result := SymbolValue(One);
  Result := 17 - (Result mod 17);
end;

{$I norqcheckend.inc}

{ TDigraphsList -------------------------------------------------------------- }

constructor TDigraphsList.Create;
begin
  inherited;

  Items := TSymbolsList.Create;
  Items.Count := HashArraySize;

  ItemsDeleted := TDynBooleanArray.Create;
  ItemsDeleted.Count := HashArraySize;
  ItemsDeleted.SetAll(false);
end;

destructor TDigraphsList.Destroy;
begin
  FreeAndNil(Items);
  FreeAndNil(ItemsDeleted);
  inherited;
end;

function TDigraphsList.IndexOfDigraph(const S1, S2: TSymbol): TSymbol;

  function SymbolsEqual(const S1, S2: TSymbol): boolean;
  begin
    Result :=
      ( (S1 is TTerminal) and (S2 is TTerminal) and
        (TTerminal(S1).Value = TTerminal(S2).Value) ) or
      ( (S1 is TNonTerminal) and (S2 is TNonTerminal) and
        (TNonTerminal(S1).Production = TNonTerminal(S2).Production) );
  end;

var
  I, Jump: Integer;
begin
  Jump := Hash2(S1);
  I := Hash(S1, S2);

  repeat
    Result := Items[I];
    if Result <> nil then
    begin
      if (S2 <> Result) and
         (S1 <> Result.Next) and
         SymbolsEqual(S1, Result) and
         SymbolsEqual(S2, Result.Next) then
        Exit;
    end else
    { If Result = nil then item is either deleted (so search further)
      or never-existent (so end of the search) }
    if not ItemsDeleted[I] then
      Break;

    I := (I + Jump) mod HashArraySize;
  until false;
end;

function TDigraphsList.IndexOfExactDigraph(S1: TSymbol): Integer;
var
  Jump: Integer;
  M: TSymbol;
begin
  Jump := Hash2(S1);
  Result := Hash(S1, S1.Next);

  repeat
    M := Items[Result];
    if M <> nil then
    begin
      if S1 = M then
        Exit;
    end else
    { If M = nil then item is either deleted (so search further)
      or never-existent (so end of the search) }
    if not ItemsDeleted[Result] then
      Break;

    Result := (Result + Jump) mod HashArraySize;
  until false;

  Result := -1;
end;

procedure TDigraphsList.DeleteDigraph(S1: TSymbol);
var
  DIndex: Integer;
begin
  DIndex := IndexOfExactDigraph(S1);
  if DIndex <> -1 then
  begin
    Items[DIndex] := nil;
    ItemsDeleted[DIndex] := true;
  end;
end;

procedure TDigraphsList.AddDigraph(S: TSymbol);
var
  I, Jump: Integer;
begin
  Jump := Hash2(S);
  I := Hash(S, S.Next);

  repeat
    if Items[I] = nil then
    begin
      Items[I] := S;
      Exit;
    end;

    I := (I + Jump) mod HashArraySize;
  until false;
end;

end.