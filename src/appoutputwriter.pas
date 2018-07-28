{
  Copyright (C) 2018 Nikita B. Zuev

  This file is part of FFXVPhotoEx.

  FFXVPhotoEx is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  FFXVPhotoEx is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with FFXVPhotoEx.  If not, see <https://www.gnu.org/licenses/>.
}
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
  writeln('                       alias -h');
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

