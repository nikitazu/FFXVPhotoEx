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
program ffxvphotoex;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  FileUtil,
  LazFileUtils, // After FileUtil to shadow deprecated functions
  JpegSsExtractor,
  AppOutputWriter;

type

  { TFFXVPhotoEx }

  TFFXVPhotoEx = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure ProcessDirectory(const InName: String);
    procedure OnFileFound(FileIterator: TFileIterator);
    procedure ProcessFile(const InName: String);
    procedure ExtractImage(const InName: String);
    function JpegName(const InName: String): String;
  end;

{ TFFXVPhotoEx }

var
  ContinueOnError: Boolean = False;
  Extractor: TJpegSsExtractor;
const
  ecFail: LongInt = 1;

procedure TFFXVPhotoEx.DoRun;
var
  ErrorMsg: String;
  ProcessedOk: Boolean = False;
begin
  // Define parameters
  //

  ErrorMsg:=CheckOptions('hd:f:c', 'help dir: file: continue-on-error');
  if ErrorMsg<>'' then begin
    WriteLn(StdErr, ErrorMsg);
    WriteHelp;
    Terminate;
    ExitCode := ecFail;
    Exit;
  end;

  // Process parametsr
  //

  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  ContinueOnError := HasOption('c', 'continue-on-error');
  Extractor := TJpegSsExtractor.Create(Self);
  try
    if HasOption('d', 'dir') then begin
      ProcessDirectory(ExpandFileNameUTF8(GetOptionValue('d', 'dir')));
      ProcessedOk := True;
    end else if HasOption('f', 'file') then begin
      ProcessFile(ExpandFileNameUTF8(GetOptionValue('f', 'file')));
      ProcessedOk := True;
    end;
  except
    on E: Exception do begin
      WriteProcessingFailedError(E.Message);
      Terminate;
      ExitCode := ecFail;
      Exit;
    end;
  end;

  if not ProcessedOk then begin
    WriteNoArgsError;
    WriteHelp;
    ExitCode := ecFail;
  end;

  Terminate;
  Exit;
end;

constructor TFFXVPhotoEx.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TFFXVPhotoEx.Destroy;
begin
  inherited Destroy;
end;

procedure TFFXVPhotoEx.WriteHelp;
begin
  AppOutputWriter.WriteHelp(ExeName);
end;

procedure TFFXVPhotoEx.ProcessDirectory(const InName: String);
var
  FS: TFileSearcher;
begin
  if DirectoryExistsUTF8(ChompPathDelim(InName)) then begin
    try
      FS := TFileSearcher.Create;
      FS.OnFileFound := @OnFileFound;
      FS.Search(InName, '*.ss', True, False);
    finally
      FreeAndNil(FS);
    end;
  end else begin
    raise Exception.Create('Directory not found: ' + InName);
  end;
end;

procedure TFFXVPhotoEx.OnFileFound(FileIterator: TFileIterator);
begin
  try
    ProcessFile(FileIterator.FileName);
  except
    on E: Exception do begin
      if ContinueOnError then begin
        WriteSkipFileWarning(FileIterator.FileName, E.Message);
      end else begin
        FileIterator.Stop;
        raise Exception.Create(
          'Directory process terminated on file: '
          + FileIterator.FileName
          + ' because '
          + E.Message
        );
      end;
    end;
  end;
end;

procedure TFFXVPhotoEx.ProcessFile(const InName: String);
begin
  if (FileExistsUTF8(InName) and not DirectoryExistsUTF8(ChompPathDelim(InName))) then begin
    ExtractImage(InName);
  end else begin
    raise Exception.Create('File not found: ' + InName);
  end;
end;

procedure TFFXVPhotoEx.ExtractImage(const InName: String);
var
  InStream: TFileStream;
  OutStream: TFileStream;
  OutName: String;
  Result: TJpegExtractResult;
begin
  Result := Default(TJpegExtractResult);
  OutName := JpegName(InName);
  WriteExtractingMessage(InName, OutName);
  InStream := TFileStream.Create(InName, fmOpenRead);
  try
    OutStream := TFileStream.Create(OutName, fmCreate);
    try
      Result := Extractor.Extract(InStream, OutStream);
      with Result do begin
        if RealJpegSize <> CalculatedJpegSize then begin
          WriteSizeMismatchWarning(CalculatedJpegSize, RealJpegSize);
        end;
      end;
    finally
      FreeAndNil(OutStream);
      if not Result.IsSuccess then begin
        DeleteFile(OutName);
      end;
    end;
  finally
    FreeAndNil(InStream);
  end;
end;

function TFFXVPhotoEx.JpegName(const InName: String): String;
var
  NameBase: String;
begin
  NameBase := ExtractFileNameWithoutExt(InName);
  Result := NameBase + '.jpg';
end;

var
  Application: TFFXVPhotoEx;
begin
  Application:=TFFXVPhotoEx.Create(nil);
  Application.Title:='FFXV Photo Extractor';
  Application.Run;
  Application.Free;
end.

