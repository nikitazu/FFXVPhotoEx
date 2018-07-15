program ffxvphotoex;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  FileUtil;

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
  end;

{ TFFXVPhotoEx }

var
  ContinueOnError: Boolean = False;

procedure TFFXVPhotoEx.DoRun;
var
  ErrorMsg: String;
begin
  // Define parameters
  //

  ErrorMsg:=CheckOptions('hd:f:c', 'help dir: file: continue-on-error');
  if ErrorMsg<>'' then begin
    WriteLn(ErrorMsg);
    WriteHelp;
    Terminate;
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

  try
    if HasOption('d', 'dir') then begin
      ProcessDirectory(GetOptionValue('d', 'dir'));
      Terminate;
      Exit;
    end;

    if HasOption('f', 'file') then begin
      ProcessFile(GetOptionValue('f', 'file'));
      Terminate;
      Exit;
    end;
  except
    on E: Exception do begin
      WriteLn('Processing failed!');
      ShowException(E);
      Terminate;
      Exit;
    end;
  end;

  // No parameters found, print usage
  //

  WriteLn('No arguments specified');
  WriteHelp;
  Terminate;
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
  writeln('Usage: ', ExeName, ' [-f|-d] input');
  writeln(' --help              : Prints help');
  writeln('');
  writeln(' --dir=indir         : Converts files inside indir from *.ss to *.jpg');
  writeln('                       alias -d indir');
  writeln('                       alias: -h');
  writeln('');
  writeln(' --file=infile.ss    : Converts infile.ss to infile.jpg');
  writeln('                       alias -f infile.ss');
  writeln('');
  writeln(' --continue-on-error : Does not stop processing on error');
  writeln('                       when processing a directory');
  writeln('                       alias -c');
end;

procedure TFFXVPhotoEx.ProcessDirectory(const InName: String);
var
  FS: TFileSearcher;
begin
  if DirectoryExists(InName) then begin
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
        WriteLn('Skipping file ', FileIterator.FileName, ' because ', E.Message);
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
  if (FileExists(InName) and not DirectoryExists(InName)) then begin
    ExtractImage(InName);
  end else begin
    raise Exception.Create('File not found: ' + InName);
  end;
end;

procedure TFFXVPhotoEx.ExtractImage(const InName: String);
var
  InStream: TFileStream;
  InFileOpened: Boolean;
  InSize: LongInt;
  OutStream: TFileStream;
  OutFileOpened: Boolean;
  OutName: String;
  OutSize: LongInt;

  Buf: Array [1..4096] Of Byte;
  NumRead: Word = 0;
  NumReadTotal: LongInt = 0;
  NumWritten: Word = 0;
  NumWrittenTotal: LongInt = 0;

  const
    ImageStartOffset = 36;
    ImageEndOffset = 130;
begin
  OutName := InName + '.jpg';

  WriteLn('Extracting ', InName, ' to ', OutName);

  try
    InStream := TFileStream.Create(InName, fmOpenRead);
    InFileOpened := True;

    OutStream := TFileStream.Create(OutName, fmOpenWrite);
    OutFileOpened := True;

    InSize := InStream.Size;
    OutSize := InSize - ImageStartOffset - ImageEndOffset;

    if ((InSize = 0) or (OutSize <= 0)) then begin
      raise Exception.Create('Invalid file format');
    end;

    InStream.Seek(ImageStartOffset, soFromBeginning);
    repeat
      NumRead := InStream.Read(Buf, SizeOf(Buf));
      NumReadTotal += NumRead;
      if (NumReadTotal < OutSize) then begin
        NumWritten := OutStream.Write(Buf, NumRead);
        NumWrittenTotal += NumWritten;
      end;
    until (NumRead = 0) or (NumWritten <> NumRead) or (NumReadTotal >= OutSize);

  finally
    if InFileOpened then begin
      FreeAndNil(InStream);
    end;
    if OutFileOpened then begin
      FreeAndNil(OutStream);
    end;
    if OutFileOpened and (OutSize <= 0) then begin
      DeleteFile(OutName);
    end;
  end;

end;

var
  Application: TFFXVPhotoEx;
begin
  Application:=TFFXVPhotoEx.Create(nil);
  Application.Title:='FFXV Photo Extractor';
  Application.Run;
  Application.Free;
end.

