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

{ @noAutoLinkHere }
unit Grammar;

interface

uses Classes, SysUtils, KambiUtils, KambiClassUtils, KambiStringUtils;

{$define read_interface}

type
  TSymbol = class;
  TProductionsList = class;

  { This is just a double-linked list of TSymbol instances.
    It's reference counted, so it can be used by more than one TNonTerminal. }
  TProduction = class
  private
    { This is reference-count, used by TNonTerminal. }
    FUsedCount: Cardinal;

    { This is a guarding symbol for our list (it's
      Next always points to our First, it's Previous always points to our Last).
      This makes implementation easier. }
    Guard: TSymbol;

    Collected: boolean;
    FCollectedIndex: Integer;
    FCollectedValue: string;

    procedure DoCollect(
      var FirstIndex: Cardinal;
      CollectValues: boolean;
      ProductionsList: TProductionsList;
      var UsedTerminals: TSetOfChars);

    procedure DoCollectValue;

    { This enumerates the nodes with Collected = @true, and sets recursively
      Collected = @false. This way it undoes the changes to Collected value
      done by DoCollect. This way you can freely call Collect at any time,
      on any production (starting or some children production) and all will
      be OK (i.e. all the productions will be enumerated each time,
      because Collect always starts with Collected = @false everywhere)
      and all will work efficiently (because DoCollect within single Collect
      call will not have to calculate the same production twice). }
    procedure UndoCollect;
  public
    constructor Create;
    destructor Destroy; override;

    { Note that these return special Guard symbol when production is empty. }
    function FirstSymbol: TSymbol;
    function LastSymbol: TSymbol;

    property UsedCount: Cardinal read FUsedCount;

    function Empty: boolean;

    property CollectedIndex: Integer read FCollectedIndex;

    { This is usually calculated by various CollectXxx routines and
      read-only from outside.

      But you can't modify it --- at your
      own responsibility. This may be useful if you simply extend this
      production by new symbol --- then simple update of CollectedValue
      may be faster than any ColllectXxx call. }
    property CollectedValue: string read FCollectedValue write FCollectedValue;

    { This is used to recursively calculate various properties
      of this production, and productions referenced by used non-terminals.

      Sets CollectedIndex of this production and then all referenced productions,
      recursively. Starts indexing using FirstIndex, and always after
      assigning some CollectedIndex increments FirstIndex, so
      FirstIndex should always indicate next free index.

      Each time when index is assigned it adds the item to
      ProductionsList, so ProductionsList contains all the indexed
      items, in the indexing order. Pass ProductionsList = nil if you
      don't need this.

      If CollectValues then also sets CollectedValue of this production
      and all children. CollectValues = @true may eat a lot of memory for large
      grammars, so be careful when you set this to @true.

      UsedTerminals is set to all terminals used. }
    procedure Collect(
      var FirstIndex: Cardinal;
      CollectValues: boolean;
      ProductionsList: TProductionsList;
      out UsedTerminals: TSetOfChars);

    { This is like simpler version of @link(Collect), when all you want
      is to set CollectedValue. This calculates CollectedValue of this
      production and all references productions, and then returns
      CollectedValue (that's comfortable sometimes). }
    function CollectValue: string;

    procedure DoCollectGrammarSize(
      var GrammarSize: Cardinal;
      var FirstIndexProductions: Cardinal;
      var UsedTerminals: TSetOfChars);

    function GraphLabel(LongNonTerminalNames: boolean): string;

    { Saves grammar starting from this production to a text stream,
      readable by graphviz. This uses DeleteIndexes and AddIndexes. }
    procedure GraphSaveToStream(Stream: TStream;
      LongNonTerminalNames: boolean);

    { Like GraphSaveToStream but takes FileName as a parameter. }
    procedure GraphSaveToFile(const FileName: string;
      LongNonTerminalNames: boolean);

    { Saves grammar starting from this production to a binary stream,
      trying to use as little bits as possible.
      In other words: do the compression. }
    procedure SaveToStream(Stream: TStream);

    { Loads grammar from a binary stream saved by SaveToStream.
      In other words: do 1st part the decompression
      (2nd part is to actually calculate the result, this
      is easily done by the Collect method). }
    constructor CreateFromStream(Stream: TStream);

    { Calculate various sizes and counts of this grammar. }
    procedure GrammarSizes(var GrammarSize, SymbolsCount,
      NonTerminalsCount, TerminalsCount, BitsPerSymbol: Cardinal);

    function Name: string;

    { Simply set this production to empty, forgetting about
      all symbols between FirstSymbol and LastSymbol.
      You're responsible to save somewhere FirstSymbol and/or LastSymbol
      references before calling this, to not leak memory. }
    procedure ReleaseSymbols;

    { Calculates CollectedValue by simply iteraring over items
      of this production, and assuming that all children productions
      already have correct CollectedValue. }
    procedure SimpleCalculateCollectedValue;
  end;

  TObjectsListItem_1 = TProduction;
  {$I objectslist_1.inc}
  TProductionsList = class(TObjectsList_1);

  { Terminal or non-terminal symbol of the grammar.

    This is an item of double-linked list TProduction.
    Note that one TProduction instance may be used many times (by many
    TNonTerminal symbols), however each TSymbol instance can be used in only one
    TProduction instance --- so that @link(Next) and @link(Previous)
    fields have sense. }
  TSymbol = class
  private
    FNext, FPrevious: TSymbol;
  public
    { @noAutoLinkHere }
    property Next: TSymbol read FNext write FNext;

    { @noAutoLinkHere }
    property Previous: TSymbol read FPrevious write FPrevious;

    { Insert NewNext as the next symbol after this symbol, within our
      parent production. IOW, @link(Next) becomes NewNext and all
      other previous/next links are set correctly too. }
    procedure InsertNext(NewNext: TSymbol);

    function GraphLabel(LongNonTerminalNames: boolean): string; virtual; abstract;
    function Name: string; virtual; abstract;

    function IsFirst: boolean;
    function IsLast: boolean;

    { This creates new TSymbol instance with contents (Value of terminal,
      production of non-terminal) copied. But links (Next, Previous)
      are not copied --- the intention is that you will want to insert
      new symbol to new production. }
    function CreateCopyContents: TSymbol; virtual; abstract;
  end;

  { This is non-terminal.

    Actually, this represents "an occurence of non-terminal
    within a rule" --- i.e. the same non-terminal may be actually represented
    by many different TNonTerminal instances, all sharing the same
    @link(Production). So (depending on what you want to get) actually
    the TProduction instance may be considered more accurate representation
    of non-terminal. }
  TNonTerminal = class(TSymbol)
  private
    FProduction: TProduction;
  public
    { @noAutoLinkHere }
    constructor Create(AProduction: TProduction);

    { @noAutoLinkHere }
    destructor Destroy; override;

    { Production, i.e. what does this non-terminal expand to.
      Always non-nil.

      This class takes care of operating Production.UsedCount
      --- it's incremented when assigning initial Production,
      and decremented (and Production is eventually freed)
      when this class is destroyed.

      @noAutoLinkHere }
    property Production: TProduction read FProduction;

    function GraphLabel(LongNonTerminalNames: boolean): string; override;
    function Name: string; override;

    function CreateCopyContents: TSymbol; override;
  end;

  TTerminal = class(TSymbol)
  private
    FValue: char;
  public
    { @noAutoLinkHere }
    constructor Create(AValue: char);

    { @noAutoLinkHere }
    property Value: char read FValue write FValue;

    function GraphLabel(LongNonTerminalNames: boolean): string; override;
    function Name: string; override;

    class function CharGraphLabel(const AValue: char): string;

    function CreateCopyContents: TSymbol; override;
  end;

  TGuard = class(TSymbol)
  private
    FParentProduction: TProduction;
  public
    { @noAutoLinkHere }
    constructor Create(AParentProduction: TProduction);

    { Note that only the TGuard has ParentProduction, not all TSymbol.

      Reason: if all symbols had ParentProduction,
      then we would have to change it when doing
      ExpandIfUnderusedProduction, and this would make this
      operation run in longer than O(1) time. }
    property ParentProduction: TProduction
      read FParentProduction;

    function GraphLabel(LongNonTerminalNames: boolean): string; override;
    function Name: string; override;
    function CreateCopyContents: TSymbol; override;
  end;

var
  { If non-nil, then all created productions will insert themselves
    to this and all destroyed will delete themselves from this.

    Note that this slightly slows down the work, and Sequitur doesn't need
    this (Sequential does). }
  AllProductionsList: TProductionsList;

{$undef read_interface}

implementation

uses BitStreams;

{$define read_implementation}
{$I objectslist_1.inc}

{ Convert unreadable characters in AValue and transform to valid
  quoted ID for graphviz. }
function StrToGraphId(const AValue: string): string;
begin
  Result := SReadableForm(AValue);
  Result := StringReplace(Result, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
  Result := '"' + Result + '"';
end;

type
  TTerminalIndexes = array [char] of Cardinal;

{ For all characters in Terminals set, it assigns index
  in TerminalIndexes[I]. Indexing starts from FirstIndex
  and after each use FirstIndex is incremented, so at the end
  FirstIndex points to next free index. }
procedure CollectTerminals(const Terminals: TSetOfChars;
  var FirstIndex: Cardinal;
  out TerminalIndexes: TTerminalIndexes);
var
  C: char;
begin
  for C := Low(C) to High(C) do
    if C in Terminals then
    begin
      TerminalIndexes[C] := FirstIndex;
      Inc(FirstIndex);
    end;
end;

{ TProduction ---------------------------------------------------------------- }

constructor TProduction.Create;
begin
  inherited;
  Guard := TGuard.Create(Self);
  Guard.Next := Guard;
  Guard.Previous := Guard;
  Collected := false;

  if AllProductionsList <> nil then
    AllProductionsList.Add(Self);
end;

destructor TProduction.Destroy;
var
  Tmp: TSymbol;
begin
  if AllProductionsList <> nil then
    AllProductionsList.Delete(Self);

  if Guard <> nil then
  begin
    while Guard.Next <> Guard do
    begin
      Tmp := Guard.Next;
      Guard.Next := Guard.Next.Next;
      FreeAndNil(Tmp);
    end;

    FreeAndNil(Guard);
  end;

  inherited;
end;

procedure TProduction.DoCollect(
  var FirstIndex: Cardinal;
  CollectValues: boolean;
  ProductionsList: TProductionsList;
  var UsedTerminals: TSetOfChars);
var
  S: TSymbol;
begin
  if not Collected then
  begin
    FCollectedIndex := FirstIndex;
    Inc(FirstIndex);
    if ProductionsList <> nil then
      ProductionsList.Add(Self);

    Collected := true;

    if CollectValues then
      FCollectedValue := '';

    S := FirstSymbol;
    while S <> Guard do
    begin
      if S is TNonTerminal then
      begin
        TNonTerminal(S).Production.DoCollect(FirstIndex, CollectValues,
          ProductionsList, UsedTerminals);
        if CollectValues then
          FCollectedValue += TNonTerminal(S).Production.CollectedValue;
      end else
      begin
        Assert(S is TTerminal);
        if CollectValues then
        begin
          FCollectedValue += TTerminal(S).Value;
        end;
        Include(UsedTerminals, TTerminal(S).Value);
      end;

      S := S.Next;
    end;
  end;
end;

procedure TProduction.SimpleCalculateCollectedValue;
var
  S: TSymbol;
begin
  FCollectedValue := '';

  S := FirstSymbol;
  while S <> Guard do
  begin
    if S is TNonTerminal then
    begin
      FCollectedValue += TNonTerminal(S).Production.CollectedValue;
    end else
    begin
      Assert(S is TTerminal);
      FCollectedValue += TTerminal(S).Value;
    end;

    S := S.Next;
  end;
end;

procedure TProduction.DoCollectGrammarSize(
  var GrammarSize: Cardinal;
  var FirstIndexProductions: Cardinal;
  var UsedTerminals: TSetOfChars);
var
  S: TSymbol;
begin
  if not Collected then
  begin
    FCollectedIndex := FirstIndexProductions;
    Inc(FirstIndexProductions);

    Collected := true;

    S := FirstSymbol;
    while S <> Guard do
    begin
      if S is TNonTerminal then
      begin
        TNonTerminal(S).Production.DoCollectGrammarSize(
          GrammarSize, FirstIndexProductions, UsedTerminals);
      end else
      begin
        Assert(S is TTerminal);
        Include(UsedTerminals, TTerminal(S).Value);
      end;

      Inc(GrammarSize);

      S := S.Next;
    end;
  end;
end;

procedure TProduction.DoCollectValue;
var
  S: TSymbol;
begin
  if not Collected then
  begin
    Collected := true;

    FCollectedValue := '';

    S := FirstSymbol;
    while S <> Guard do
    begin
      if S is TNonTerminal then
      begin
        TNonTerminal(S).Production.DoCollectValue;
        FCollectedValue += TNonTerminal(S).Production.CollectedValue;
      end else
      begin
        Assert(S is TTerminal);
        FCollectedValue += TTerminal(S).Value;
      end;

      S := S.Next;
    end;
  end;
end;

procedure TProduction.UndoCollect;
var
  S: TSymbol;
begin
  if Collected then
  begin
    Collected := false;
    S := FirstSymbol;
    while S <> Guard do
    begin
      if S is TNonTerminal then
        TNonTerminal(S).Production.UndoCollect;
      S := S.Next;
    end;
  end;
end;

procedure TProduction.Collect(
  var FirstIndex: Cardinal;
  CollectValues: boolean;
  ProductionsList: TProductionsList;
  out UsedTerminals: TSetOfChars);
begin
  UsedTerminals := [];
  DoCollect(FirstIndex, CollectValues, ProductionsList, UsedTerminals);
  UndoCollect;
end;

function TProduction.CollectValue: string;
begin
  DoCollectValue;
  UndoCollect;
  Result := CollectedValue;
end;

function TProduction.FirstSymbol: TSymbol;
begin
  Result := Guard.Next;
end;

function TProduction.LastSymbol: TSymbol;
begin
  Result := Guard.Previous;
end;

function TProduction.Empty: boolean;
begin
  Result := Guard.Next = Guard;
end;

procedure TProduction.ReleaseSymbols;
begin
  Guard.Next := Guard;
  Guard.Previous := Guard;
end;

function TProduction.GraphLabel(LongNonTerminalNames: boolean): string;
begin
  if CollectedIndex = 0 then
    Result := 'Start' else
    Result := 'P' + IntToStr(CollectedIndex);

  { Even when LongNonTerminalNames, I still have to keep
    CollectedIndex in the Result ---- in case the same CollectedValue
    may be represented by two different production. Sequitur guarantees
    that this will not occur --- but other algorithms may not ? }
  if LongNonTerminalNames then
    Result := StrToGraphId('(' + Result + ') ' + CollectedValue);
end;

procedure TProduction.GraphSaveToStream(Stream: TStream;
  LongNonTerminalNames: boolean);

  { Write edges from Prod. }
  procedure WriteEdges(Prod: TProduction);
  var
    S: TSymbol;
  begin
    S := Prod.FirstSymbol;
    while S <> Prod.Guard do
    begin
      WritelnStr(Stream, '  ' +
        Prod.GraphLabel(LongNonTerminalNames) + ' -> ' +
        S.GraphLabel(LongNonTerminalNames));
      S := S.Next;
    end;
  end;

var
  FirstIndex: Cardinal;
  ProductionsList: TProductionsList;
  UsedTerminals: TSetOfChars;
  I: Integer;
  C: char;
begin
  ProductionsList := TProductionsList.Create;
  try
    FirstIndex := 0;
    Collect(FirstIndex, LongNonTerminalNames, ProductionsList, UsedTerminals);

    WritelnStr(Stream, 'digraph Grammar {');
    for I := 0 to ProductionsList.Count - 1 do
      WriteEdges(ProductionsList[I]);
    WritelnStr(Stream, nl+
                       '  subgraph cluster_non_terminals {' +nl+
                       '    label="NonTerminals";');
    for I := 0 to ProductionsList.Count - 1 do
      WritelnStr(Stream, '    ' +
        ProductionsList[I].GraphLabel(LongNonTerminalNames) + ';');
    WritelnStr(Stream, '  }' +nl+
                       nl+
                       '  subgraph cluster_terminals {' +nl+
                       '    rank=same;' +nl+
                       '    label="Terminals";');
    for C := Low(C) to High(C) do
      if C in UsedTerminals then
        WritelnStr(Stream, '    ' + TTerminal.CharGraphLabel(C) + ';');
    WritelnStr(Stream, '  }' +nl+
                       '}');

  finally FreeAndNil(ProductionsList) end;
end;

procedure TProduction.GraphSaveToFile(const FileName: string;
  LongNonTerminalNames: boolean);
var
  S: TStream;
begin
  S := TFileStream.Create(FileName, fmCreate);
  try
    GraphSaveToStream(S, LongNonTerminalNames);
  finally FreeAndNil(S) end;
end;

procedure TProduction.GrammarSizes(var GrammarSize, SymbolsCount,
  NonTerminalsCount, TerminalsCount, BitsPerSymbol: Cardinal);
var
  UsedTerminals: TSetOfChars;
  C: char;
begin
  NonTerminalsCount := 0;
  GrammarSize := 0;
  UsedTerminals := [];
  DoCollectGrammarSize(GrammarSize, NonTerminalsCount, UsedTerminals);
  UndoCollect;

  TerminalsCount := 0;
  for C := Low(C) to High(C) do
    if C in UsedTerminals then
      Inc(TerminalsCount);

  SymbolsCount := TerminalsCount + NonTerminalsCount;

  BitsPerSymbol := BitsPerSymbolFromMaxValue(SymbolsCount - 1);
end;

procedure TProduction.SaveToStream(Stream: TStream);

{ $define DEBUG_BINARY_SAVE}

var
  FirstIndex: Cardinal;
  ProductionsList: TProductionsList;
  UsedTerminals: TSetOfChars;
  TerminalIndexes: TTerminalIndexes;
  StreamBitWriter: TStreamBitWriter;
  I: Integer;
  S: TSymbol;
begin
  ProductionsList := TProductionsList.Create;
  try
    FirstIndex := 0;
    Collect(FirstIndex, false, ProductionsList, UsedTerminals);
    Assert(CollectedIndex = 0);
    Assert(FirstIndex = Cardinal(ProductionsList.Count));
    CollectTerminals(UsedTerminals, FirstIndex, TerminalIndexes);

    { Now FirstIndex is the total count of symbols. }

    StreamWriteLongWord(Stream, ProductionsList.Count);
    {$ifdef DEBUG_BINARY_SAVE}
    Writeln('save ProductionsList.Count ', ProductionsList.Count);
    {$endif}
    Stream.WriteBuffer(UsedTerminals, SizeOf(UsedTerminals));

    { Decompressor will be able to figure out total number of symbols
      used (i.e. current FirstIndex value)
      by reading ProductionsList.Count and UsedTerminals. }

    StreamBitWriter := TStreamBitWriter.Create(Stream, FirstIndex - 1);
    try
      {$ifdef DEBUG_BINARY_SAVE}
      Writeln('save BitsPerValue will be ', StreamBitWriter.BitsPerValue);
      {$endif}
      for I := 0 to ProductionsList.High do
      begin
        S := ProductionsList[I].FirstSymbol;
        while S <> ProductionsList[I].Guard do
        begin
          if S is TNonTerminal then
          begin
            StreamBitWriter.WriteValue(
              TNonTerminal(S).Production.CollectedIndex);
            {$ifdef DEBUG_BINARY_SAVE}
            Writeln('save NonTerminal ',
              TNonTerminal(S).Production.CollectedIndex);
            {$endif}
          end else
          begin
            Assert(S is TTerminal);
            StreamBitWriter.WriteValue(TerminalIndexes[TTerminal(S).Value]);
            {$ifdef DEBUG_BINARY_SAVE}
            Writeln('save Terminal ', SReadableForm(TTerminal(S).Value),
              ' index ', TerminalIndexes[TTerminal(S).Value]);
            {$endif}
          end;
          S := S.Next;
        end;

        { I know that 0 is CollectedIndex of starting production.
          So I know that it cannot ever be written
          by 2 StreamBitWriter.WriteValue calls above for TNonTerminal
          and TTerminal (otherwise (if some production would lead
          back to starting production) we would have infinite
          expansion of the production, and this can't happen in our
          grammar). }
        StreamBitWriter.WriteValue(0);
        {$ifdef DEBUG_BINARY_SAVE}
        Writeln('save delimiter 0');
        {$endif}
      end;
    finally FreeAndNil(StreamBitWriter) end;
  finally FreeAndNil(ProductionsList) end;
end;

constructor TProduction.CreateFromStream(Stream: TStream);

{ $define DEBUG_BINARY_LOAD}

var
  FirstIndex: Cardinal;
  ProductionsList: TProductionsList;
  UsedTerminals: TSetOfChars;
  TerminalIndexes: TTerminalIndexes;
  { This is reverse of TerminalIndexes array.
    For given SymbolIndex - ProductionsList.Count, what is the resulting
    terminal ? }
  ReverseTerminalIndexes: array [Byte] of char;
  C: char;
  I: Integer;
  SymbolIndex: Cardinal;
  NewSymbol: TSymbol;
  StreamBitReader: TStreamBitReader;
begin
  Create;

  ProductionsList := TProductionsList.Create;
  try
    ProductionsList.Count := StreamReadLongWord(Stream);
    {$ifdef DEBUG_BINARY_LOAD}
    Writeln('load ProductionsList.Count ', ProductionsList.Count);
    {$endif}
    ProductionsList[0] := Self;
    for I := 1 to ProductionsList.High do
      ProductionsList[I] := TProduction.Create;

    Stream.ReadBuffer(UsedTerminals, SizeOf(UsedTerminals));

    FirstIndex := ProductionsList.Count;
    CollectTerminals(UsedTerminals, FirstIndex, TerminalIndexes);

    { Now FirstIndex stores the count of non-terminals + terminals }

    { Calculate ReverseTerminalIndexes }
    for C := Low(C) to High(C) do
      if C in UsedTerminals then
        ReverseTerminalIndexes[TerminalIndexes[C] -
          Cardinal(ProductionsList.Count)] := C;

    StreamBitReader := TStreamBitReader.Create(Stream, FirstIndex - 1);
    try
      {$ifdef DEBUG_BINARY_LOAD}
      Writeln('load BitsPerValue will be ', StreamBitReader.BitsPerValue);
      {$endif}

      for I := 0 to ProductionsList.High do
      begin
        repeat
          SymbolIndex := StreamBitReader.ReadValue;
          if SymbolIndex = 0 then
          begin
            {$ifdef DEBUG_BINARY_LOAD}
            Writeln('load delimiter 0');
            {$endif}
            Break;
          end;

          if SymbolIndex < Cardinal(ProductionsList.Count) then
          begin
            NewSymbol := TNonTerminal.Create(ProductionsList[SymbolIndex]);
            {$ifdef DEBUG_BINARY_LOAD}
            Writeln('load NonTerminal ', SymbolIndex);
            {$endif}
          end else
          begin
            NewSymbol := TTerminal.Create(ReverseTerminalIndexes[
              SymbolIndex - Cardinal(ProductionsList.Count)]);
            {$ifdef DEBUG_BINARY_LOAD}
            Writeln('load Terminal ', SReadableForm(TTerminal(NewSymbol).Value),
              ' index ', SymbolIndex);
            {$endif}
          end;

          ProductionsList[I].LastSymbol.InsertNext(NewSymbol);
        until false;
      end;
    finally FreeAndNil(StreamBitReader) end;
  finally FreeAndNil(ProductionsList) end;
end;

function TProduction.Name: string;
begin
  Result := 'Production $' + IntToHex(TPointerUInt(Pointer(Self)), 8);
end;

{ TSymbol -------------------------------------------------------------------- }

procedure TSymbol.InsertNext(NewNext: TSymbol);
begin
  NewNext.Previous := Self;
  NewNext.Next := Next;

  Next.Previous := NewNext;
  Next := NewNext;
end;

function TSymbol.IsFirst: boolean;
begin
  Result := Previous is TGuard;
end;

function TSymbol.IsLast: boolean;
begin
  Result := Next is TGuard;
end;

{ TNonTerminal --------------------------------------------------------------- }

constructor TNonTerminal.Create(AProduction: TProduction);
begin
  inherited Create;
  FProduction := AProduction;
  Inc(Production.FUsedCount);
end;

destructor TNonTerminal.Destroy;
begin
  if Production <> nil then
  begin
    Dec(Production.FUsedCount);
    if Production.FUsedCount = 0 then
      FreeAndNil(FProduction);
  end;

  inherited;
end;

function TNonTerminal.GraphLabel(LongNonTerminalNames: boolean): string;
begin
  Result := Production.GraphLabel(LongNonTerminalNames);
end;

function TNonTerminal.Name: string;
begin
  Result := Production.Name;
end;

function TNonTerminal.CreateCopyContents: TSymbol;
begin
  Result := TNonTerminal.Create(Production);
end;

{ TTerminal ------------------------------------------------------------------ }

constructor TTerminal.Create(AValue: char);
begin
  inherited Create;
  FValue := AValue;
end;

function TTerminal.GraphLabel(LongNonTerminalNames: boolean): string;
begin
  Result := CharGraphLabel(Value);
end;

class function TTerminal.CharGraphLabel(const AValue: char): string;
begin
  Result := StrToGraphId(AValue);
end;

function TTerminal.Name: string;
begin
  Result := CharGraphLabel(Value);
end;

function TTerminal.CreateCopyContents: TSymbol;
begin
  Result := TTerminal.Create(Value);
end;

{ TGuard --------------------------------------------------------------------- }

constructor TGuard.Create(AParentProduction: TProduction);
begin
  inherited Create;
  FParentProduction := AParentProduction;
end;

function TGuard.GraphLabel(LongNonTerminalNames: boolean): string;
begin
  Result := 'GUARD';
end;

function TGuard.Name: string;
begin
  Result := 'GUARD';
end;

function TGuard.CreateCopyContents: TSymbol;
begin
  raise EInternalError.Create('Don''t create copies of the TGuard symbol');
  Result := nil; { silence the warning }
end;

end.