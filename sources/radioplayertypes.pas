unit RadioPlayerTypes;
{===============================================================================
File:                RadioPlayerTypes.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Types and classes used in the application

================================================================================}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{ - - - - - - - - - - - - - - - - - Enums - - - - - - - - - - - - - - - - - -  }
type
  TPlayerMessageType = (Connecting=0, Error=1, Progress=2, StreamName=3, Bitrate=4, StreamTitle=7, Channels=10, Freq=11, Other=25);

{ - - - - - - - - - - - - - - - - Error Type - - - - - - - - - - - - - - - - - }
type
  ErrorId = integer;

{ - - - - - - - - - - - - - - - - - - Forms - - - - - - - - - - - - - - - - - -}
type
  TOpenMode = (omNew, omEdit, omDelete, omNormal);

{ - - - - - - - - - - - - - - - - - Dictionary - - - - - - - - - - - - - - - - }
type
  TDictionaryKind = (dkGenre, dkRegion, dkCountry);

{ - - - - - - - - - - - - - - - - StationInfo - - - - - - - - - - - - - - - - - }
type
  TStationInfo = packed record
    Id                  : integer;
    Name                : string;
    StreamUrl           : string;
    Description         : string;
    WebpageUrl          : string;
    GenreCode           : string;
    CountryCode         : string;
  end;

{ - - - - - - - - - - - - - - - - DictionaryTable - - - - - - - - - - - - - - -}
type
  PDictionaryTable = ^TDictionaryTable;
  TDictionaryTable = record
    Id          : integer;
    Text        : string;
    Code        : string;
  end;

{ - - - - - - - - - - - - - - - TStationNodeData  - - - - - - - - - - - - - - - }
type
  TStationNodeData = class
  protected
    FID      : integer;
    FName    : string;
    FGenre   : string;
    FCountry : string;
  public
    constructor Create(const Id: integer;
      const Name, Genre, Country: string); overload;

    property ID       : integer  read FID       write FID;
    property Name     : string   read FName     write FName;
    property Genre    : string   read FGenre    write FGenre;
    property Country  : string   read FCountry  write FCountry;
  end;

  PStationNodeRec = ^TStationNodeRec;
  TStationNodeRec =
  record
     snd : TStationNodeData;
  end;

implementation

constructor TStationNodeData.Create(const Id: integer; const Name, Genre,
  Country: string);
begin
  inherited Create;
    FID       := Id;
    FName     := Name;
    FGenre    := Genre;
    FCountry  := Country;
end;

end.

