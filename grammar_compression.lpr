{ -*- compile-command: "make build-debug" -*- }

{ If defined, the program tries harder to catch proper time:
  - doesn't do OutputGrammarSizes (as it may take a while)
  - it may run the whole process a couple of times, and then output
    time as an average --- see RepeatTimes const. This is good for
    testing on very short files. }
{ $define PRECISE_MEASURE_TIME}

program grammar_compression;

uses SysUtils, Classes, CastleUtils, CastleClassUtils,
  CastleParameters, CastleFilesUtils, CastleTimeUtils,
  Grammar, SeqCompression;

procedure CompressNone(StartProduction: TProduction; InputStream: TStream);
var
  C: char;
begin
  while InputStream.Read(C, 1) <> 0 do
    StartProduction.LastSymbol.InsertNext(TTerminal.Create(C));
end;

procedure OutputGrammarSizes(StartProduction: TProduction);
var
  GrammarSize, SymbolsCount,
    NonTerminalsCount, TerminalsCount, BitsPerSymbol: Cardinal;
begin
  StartProduction.GrammarSizes(GrammarSize, SymbolsCount,
    NonTerminalsCount, TerminalsCount, BitsPerSymbol);
  Writeln(ErrOutput, Format(
    'Grammar info: %d symbols used (%d non-terminals + %d terminals),' +nl+
    '  size (sum lengths of all productions): %d,' +nl+
    '  bits per symbol needed: %d',
    [ SymbolsCount, NonTerminalsCount, TerminalsCount,
      GrammarSize, BitsPerSymbol ]));
end;

type
  TAlgorithm = (caNone, caSequitur, caSequential, caRytter);

const
  AlgorithmNames: array [TAlgorithm] of string =
  ( 'none', 'sequitur', 'sequential', 'rytter' );

  DefaultSequentialWindowSize = 0;

var
  WasParam_DeCompress: boolean = false;
  Param_OutputGraph: string = '';
  Param_Algorithm: TAlgorithm = caSequitur;

const
  Options: array[0..3] of TOption =
  (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'g'; Long: 'output-graph'; Argument: oaRequired),
    (Short: 'd'; Long: 'decompress'; Argument: oaNone),
    (Short: 'a'; Long: 'algorithm'; Argument: oaRequired)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
var
  Alg: TAlgorithm;
begin
  case OptionNum of
    0: begin
         InfoWrite(
           'grammar_compression: Compress/decompress using one of grammar-based' +nl+
           'compression algorithms.' +nl+
           nl+
           'Syntax is:' +nl+
           '  grammar_compression [ options ... ] input_filename [output_filename]' +nl+
           nl+
           'input_filename and/or output_filename may be "-" to indicate stdin/stdout.' +nl+
           nl+
           'When no output_filename will be specified, then the resulting' +nl+
           'file will not be written at all. This is useful if you run the program' +nl+
           'only to generate grammar graph, see --output-graph option. Note that in' +nl+
           'this case other parameters (like --decompress, --algorithm) are still' +nl+
           'meaningfull: they specify whether you want to create the grammar' +nl+
           '(like when compressing, using specified algorithm) or just to read' +nl+
           'stored grammar (like when decompressing).' +nl+
           nl+
           'Available options:' +nl+
           '  -h / --help               Show this help.' +nl+
           '  -g / --output-graph graph_filename' +nl+
           '                            Write resulting grammar to "graph_filename".' +nl+
           '                            You can also use this option when decompressing' +nl+
           '                            --- then it will just write the grammar' +nl+
           '                            already recorded in input_file.' +nl+
           '  -d / --decompress         Decompress. By default we do compression.' +nl+
           '  -a / --algorithm none|sequitur|sequential|rytter' +nl+
           '                            Specify algorithm used for compression.' +nl+
           '                            Note that this is ignored when decompressing' +nl+
           '                            because the compressed output is the same' +nl+
           '                            for every algorithm.' +nl+
           '                            Default is to use "sequitur".' +nl+
           nl+
           'Upon completion, program will output some information on stderr' +nl+
           '(stderr, to not collide with stdout when it''s used for output_filename = "-").');
         ProgramBreak;
       end;
    1: Param_OutputGraph := Argument;
    2: WasParam_DeCompress := true;
    3: begin
         for Alg := Low(Alg) to High(Alg) do
           if SameText(AlgorithmNames[Alg], Argument) then
           begin
             Param_Algorithm := Alg;
             Exit;
           end;
         raise EInvalidParams.CreateFmt('Invalid --algorithm "%s"', [Argument]);
       end;
  end;
end;

var
  InputFileName, OutputFileName: string;
  IsOutputFileName: boolean;
  InputStream, OutputStream: TStream;
  InputStreamByteSize: Int64;
  OutputStreamByteSize: Int64 = -1;

procedure DoWork;
var
  StartProduction: TProduction;
begin
  try
    { Initialize InputStream }
    if InputFileName = '-' then
      InputStream := TBufferedReadStream.Create(StdInStream, false) else
      InputStream := TBufferedReadStream.Create(
        TFileStream.Create(InputFileName, fmOpenRead), true);
    try
      { Read StartProduction from InputStream }
      if WasParam_DeCompress then
      begin
        StartProduction := TProduction.CreateFromStream(InputStream);
      end else
      begin
        StartProduction := TProduction.Create;
        case Param_Algorithm of
          caNone: CompressNone(StartProduction, InputStream);
          caSequitur: CompressSequitur(StartProduction, InputStream);
          caSequential: CompressSequential(StartProduction, InputStream);
          else raise Exception.Create('Algorithm not implemented');
        end;
      end;

      InputStreamByteSize := InputStream.Size;
      if InputStreamByteSize = -1 then
        Writeln(ErrOutput, 'Input stream bytes: unknown (stdin)') else
        Writeln(ErrOutput, 'Input stream bytes: ', InputStreamByteSize);
    finally FreeAndNil(InputStream) end;

    { Output graph of StartProduction }
    if Param_OutputGraph <> '' then
      StartProduction.GraphSaveToFile(Param_OutputGraph, true);

    {$ifndef PRECISE_MEASURE_TIME}
    OutputGrammarSizes(StartProduction);
    {$endif}

    if IsOutputFileName then
    begin
      { Initialize OutputStream }
      if OutputFileName = '-' then
        OutputStream := StdOutStream else
        OutputStream := TFileStream.Create(OutputFileName, fmCreate);
      try
        { Save StartProduction to OutputStream }
        if WasParam_DeCompress then
          WriteStr(OutputStream, StartProduction.CollectValue) else
          StartProduction.SaveToStream(OutputStream);

        OutputStreamByteSize := OutputStream.Size;
        if OutputStreamByteSize = -1 then
          Writeln(ErrOutput, 'Output stream bytes: unknown (stdout)') else
          Writeln(ErrOutput, 'Output stream bytes: ', OutputStreamByteSize);
      finally
        if OutputStream <> StdOutStream then
          FreeAndNil(OutputStream);
      end;
    end;
  finally FreeAndNil(StartProduction) end;
end;

const
  RepeatTimes = {$ifdef PRECISE_MEASURE_TIME} 10 {$else} 1 {$endif};

var
  UncompressedStreamByteSize: Int64 = -1;
  CompressionRatio: Float;
  TotalTime: Double;
  I: Integer;
  BytesPerSecond: string;
begin
  { Parse parameters }
  Parameters.Parse(Options, @OptionProc, nil);
  case Parameters.High of
    0: raise EInvalidParams.Create('input_filename parameter missing');
    1: begin
         InputFileName := Parameters[1];
         IsOutputFileName := false;
       end;
    2: begin
         InputFileName := Parameters[1];
         OutputFileName := Parameters[2];
         IsOutputFileName := true;
       end;
    else raise EInvalidParams.Create('Too many parameters');
  end;

  { do the work, measuring time }
  ProcessTimerBegin;
  for I := 0 to RepeatTimes - 1 do DoWork;
  TotalTime := ProcessTimerEnd;

  UncompressedStreamByteSize := -1;

  if (InputStreamByteSize <> -1) and
     (OutputStreamByteSize <> -1) and
     { compressed version has always > 0 size (because of the header),
       so I check below to check whether uncompressed version has <> 0. }
     (InputStreamByteSize <> 0) and
     (OutputStreamByteSize <> 0) then
    if WasParam_DeCompress then
    begin
      UncompressedStreamByteSize := OutputStreamByteSize;
      CompressionRatio := InputStreamByteSize / OutputStreamByteSize;
      Writeln(ErrOutput, Format('Compression ratio (input / output): %f',
        [CompressionRatio]));
    end else
    begin
      UncompressedStreamByteSize := InputStreamByteSize;
      CompressionRatio := OutputStreamByteSize / InputStreamByteSize;
      Writeln(ErrOutput, Format('Compression ratio (output / input): %f',
        [CompressionRatio]));
    end;

  if UncompressedStreamByteSize <> -1 then
  begin
    if TotalTime < 0.01 then
      BytesPerSecond := '(unknown (time too short))' else
      BytesPerSecond := Format('%f', [
        UncompressedStreamByteSize * RepeatTimes / TotalTime]);
    Writeln(ErrOutput, Format('Time: %f, (de)compressed bytes per second: %s',
      [ TotalTime / RepeatTimes, BytesPerSecond ]));
  end;
end.