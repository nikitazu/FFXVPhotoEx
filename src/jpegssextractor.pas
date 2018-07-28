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
    IsSuccess: Boolean;
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
  Result.IsSuccess := False;
  Result.InputSize := InStream.Size;
  Result.CalculatedJpegSize := Result.InputSize - ImageStartOffset - ImageEndOffset;

  if ((Result.InputSize = 0) or (Result.CalculatedJpegSize <= 0)) then begin
    raise Exception.Create('Invalid input format');
  end;

  InStream.Seek(ImageStartOffset, soFromBeginning);
  OutStream.Position := 0;
  Result.RealJpegSize := OutStream.CopyFrom(InStream, Result.CalculatedJpegSize);
  Result.IsSuccess := Result.RealJpegSize > 0;
end;

end.

