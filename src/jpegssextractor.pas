unit JpegSsExtractor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TJpegExtractResult = record
    InputSize: LongInt;
    CalculatedJpegSize: LongInt;
    RealJpegSize: LongInt;
  end;

  TJpegSsExtractor = class(TComponent)
    function Extract(
      const InStream: TFileStream;
      const OutStream: TFileStream
    ): TJpegExtractResult;
  end;

implementation

const
  ImageStartOffset = 36;
  ImageEndOffset = 130;

function TJpegSsExtractor.Extract(
  const InStream: TFileStream;
  const OutStream: TFileStream
): TJpegExtractResult;
begin
  Result.InputSize := InStream.Size;
  Result.CalculatedJpegSize := Result.InputSize - ImageStartOffset - ImageEndOffset;

  if ((Result.InputSize = 0) or (Result.CalculatedJpegSize <= 0)) then begin
    raise Exception.Create('Invalid input format');
  end;

  InStream.Seek(ImageStartOffset, soFromBeginning);
  OutStream.Position := 0;
  Result.RealJpegSize := OutStream.CopyFrom(InStream, Result.CalculatedJpegSize);
end;

end.

