unit ExportImport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, jsonConf, fpjsonrtti, contnrs,
  RadioPlayerTypes;

type

  TExportImport = class (TObject)
  private
    function SerializeObjectToJSON(AObject: TObject): string;
    procedure SaveStringToFile(Str: string; FilePath: string);
    function GetFormatedJsonString(var exportImportDto: TExportImportDto): string;
  public
    constructor Create(); overload;
    destructor Destroy; override;

    procedure ExportToJsonFile(FilePath: string);
  end;


implementation

uses
  Repository;

{ TExportImport }

function TExportImport.SerializeObjectToJSON(AObject: TObject): string;
var
  jsoSerialize: TJSONStreamer;
begin
  jsoSerialize := TJSONStreamer.Create(nil);

  try
    Result := jsoSerialize.ObjectToJSONString(AObject);
  finally
    jsoSerialize.Free;
  end;

end;

procedure TExportImport.SaveStringToFile(Str: string; FilePath: string);
var
  f: TMemoryStream;
begin
  f := TMemoryStream.Create;
  try
    f.WriteBuffer(Pointer(Str)^, Length(Str));
    f.Position := 0;
    f.SaveToFile(FilePath);
  finally
   f.Free;
  end;
end;

function TExportImport.GetFormatedJsonString(var exportImportDto: TExportImportDto): string;
var
  strContent: string;
  jData: TJSONData;
begin
  strContent := SerializeObjectToJSON(exportImportDto);
  jData := GetJSON(strContent);
  try
    Result := jData.FormatJSON;
  finally
    jData.Free;
  end;
end;

constructor TExportImport.Create();
begin

end;

destructor TExportImport.Destroy;
begin
  inherited Destroy;
end;

procedure TExportImport.ExportToJsonFile(FilePath: string);
var
  err: ErrorId;

  jObject: TJSONObject;
  jArray: TJSONArray;
  content: string;

  exportImportDto: TExportImportDto;
  stationList: TObjectList;
  dictionaryList: TObjectList;
begin

  err := TRepository.GetAllStations(stationList);
  err := TRepository.GetAllDictionaries(dictionaryList);

  exportImportDto := TExportImportDto.Create;
  try
    // prepare data
    exportImportDto.A_Date := DateTimeToStr(Now);
    exportImportDto.B_Stations := stationList;
    exportImportDto.C_Dictionaries := dictionaryList;

    // and save
    content := GetFormatedJsonString(exportImportDto);
    SaveStringToFile(content, FilePath);
  finally
    FreeAndNil(exportImportDto);
  end;
end;

end.

