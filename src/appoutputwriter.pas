unit AppOutputWriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure WriteHelp(AppName: String);
procedure WriteNoArgsError;
procedure WriteProcessingFailedError(Msg: String);
procedure WriteSkipFileWarning(FileName: String; ErrorMsg: String);
procedure WriteExtractingMessage(InName: String; OutName: String);
procedure WriteSizeMismatchWarning(CalcSize: LongInt; RealSize: LongInt);

implementation

procedure WriteHelp(AppName: String);
begin
  writeln('Usage: ', AppName, ' [-f|-d] input');
  writeln(' --help              : Prints help');
  writeln('                       alias: -h');
  writeln('');
  writeln(' --dir=indir         : Converts files inside indir from *.ss to *.jpg');
  writeln('                       alias -d indir');
  writeln('');
  writeln(' --file=infile.ss    : Converts infile.ss to infile.jpg');
  writeln('                       alias -f infile.ss');
  writeln('');
  writeln(' --continue-on-error : Does not stop processing on error');
  writeln('                       when processing a directory');
  writeln('                       alias -c');
end;

procedure WriteNoArgsError;
begin
  WriteLn(StdErr, 'No arguments specified');
end;

procedure WriteProcessingFailedError(Msg: String);
begin
  WriteLn(StdErr, 'Processing failed with error: ', Msg);
end;

procedure WriteSkipFileWarning(FileName: String; ErrorMsg: String);
begin
  WriteLn(StdErr, 'Skipping file ', FileName, ' because ', ErrorMsg);
end;

procedure WriteExtractingMessage(InName: String; OutName: String);
begin
  WriteLn('Extracting <', InName, '> to <', OutName, '>');
end;

procedure WriteSizeMismatchWarning(CalcSize: LongInt; RealSize: LongInt);
begin
  WriteLn(
    StdErr
  , 'Warning: number of written bytes differs: expected='
  , CalcSize
  , ' actual='
  , RealSize
  );
end;

end.

