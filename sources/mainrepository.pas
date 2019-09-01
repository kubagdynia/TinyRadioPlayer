unit MainRepository;
{===============================================================================
File:                MainRepository.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Database management, the main database

================================================================================}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RadioPlayerTypes, BaseRepository;

type

  { TMainRepository }

  TMainRepository = class (TBaseRepository)
  private
    function CreateStations: ErrorId;
    function CreateDictionaries: ErrorId;
  protected
    function GetDBPath: string; override;
    function CreateDML: ErrorId; override;
  public
    constructor Create; override;

    function GetDBName: string; override;
  end;

implementation

uses
  Consts, ZDataset, TRPErrors, Helpers, dateutils;

// Constructor
constructor TMainRepository.Create;
begin
  inherited Create;
end;

// Function returns the path to the database
function TMainRepository.GetDBPath: string;
begin
  Result := DB_PATH;
end;

function TMainRepository.GetDBName: string;
begin
  Result := DB_MAIN_DB_FILE_NAME;
end;

function TMainRepository.CreateDML: ErrorId;
var
  err: ErrorId;
begin
  err := ERR_OK;

  try
    err := CreateDictionaries;
    err := CreateStations;
  except
    on E: Exception do
    begin
      LogException(EmptyStr, ClassName, 'CreateDML', E);
      err := ERR_DB_CREATE_DML_ERROR;
    end;
  end;

  Result := err;
end;

function TMainRepository.CreateStations: ErrorId;
var
  err: ErrorId;
  stationId: string;
begin
  err := ERR_OK;

  // Stations
  err := StationRepo.AddStation('.977 The 80''s Channel', 'http://www.977music.com/tunein/web/80s.asx',
    EMPTY_STR, 'http://www.977music.com/', 'ClassicHits', 'US', 'NorthAmerica', stationId);

  err := StationRepo.AddStation('.977 The Classic Rock Channel', 'http://www.977music.com/tunein/web/classicrock.asx',
    EMPTY_STR, 'http://www.977music.com/', 'CLASSICROCK', 'US', 'NorthAmerica', stationId);

  err := StationRepo.AddStation('Radio Kaszebe', 'http://stream3.nadaje.com:8048',
    EMPTY_STR, 'http://radiokaszebe.pl/', 'Pop', 'PL', 'Europe', stationId);

  err := StationRepo.AddStation('Radio Malbork', 'http://78.46.246.97:9022',
    EMPTY_STR, 'https://www.radiomalbork.fm/', 'Pop', 'PL', 'Europe', stationId);

  err := StationRepo.AddStation('Planeta RnB', 'http://plarnb-01.cdn.eurozet.pl:8216/',
    EMPTY_STR, 'https://www.planetafm.pl/', 'RnBSoul', 'PL', 'Europe', stationId);

  Result := err;
end;

function TMainRepository.CreateDictionaries: ErrorId;
var
  err: ErrorId;
  dictionaryId, dictionaryRegionId, dictionaryCountryId: integer;
  dictionaryRowId, parentDictionaryRowId: integer;
begin
  err := ERR_OK;

  // GENRE
  err := DictionaryRepo.AddDictionary('Genre', DICTIONARY_GENRE_CODE, dictionaryId);

  err := DictionaryRepo.AddDictionaryRow('Alternative Music', 'AlternativeMusic', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Blues', 'Blues', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Classic Hits', 'ClassicHits', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Classic Rock', 'ClassicRock', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Classical Music', 'ClassicalMusic', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Country Music', 'CountryMusic', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Dance Music', 'DanceMusic', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Easy Listening', 'EasyListening', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Electronic Music', 'ElectronicMusic', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('European Music (Folk / Pop)', 'EuropeanMusic', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Hip Hop / Rap', 'HipHopRap', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Indie Pop', 'IndiePop', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Inspirational (incl. Gospel)', 'Inspirational', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Asian Pop (J-Pop, K-pop)', 'AsianPop', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Jazz', 'Jazz', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Latin Music', 'LatinMusic', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('New Age', 'NewAge', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Opera', 'Opera', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Pop (Popular music)', 'Pop', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('RnB / Soul', 'RnBSoul', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Reggae', 'Reggae', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Rock', 'Rock', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Singer / Songwriter (inc. Folk)', 'SingerSongwriter', 1, dictionaryId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('World Music / Beats', 'WorldMusicBeats', 1, dictionaryId, dictionaryRowId);

  // REGION & COUNTRY
  err := DictionaryRepo.AddDictionary('Region', DICTIONARY_REGION_CODE, dictionaryRegionId);
  err := DictionaryRepo.AddDictionary('Country', DICTIONARY_COUNTRY_CODE, dictionaryCountryId, 'Region');

  // header - region AFRICA
  err := DictionaryRepo.AddDictionaryRow('Africa', 'Africa', 1, dictionaryRegionId, parentDictionaryRowId);
  // items - country
  err := DictionaryRepo.AddDictionaryRow('Algeria', 'DZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Angola', 'AO', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Benin', 'BJ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Botswana', 'BW', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Burkina Faso', 'BF', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Burundi', 'BI', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Cameroon', 'CM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Cape Verde', 'CV', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Central African Republic', 'CF', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Chad', 'TD', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Comoros', 'KM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('CONGO, Democratic Republic of', 'CD', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('CONGO, Republic of', 'CG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Cote d''Ivoire', 'CI', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Djibouti', 'DJ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Egypt', 'EG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Equatorial Guinea', 'GQ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Eritrea', 'ER', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Ethiopia', 'ET', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Gabon', 'GA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Gambia', 'GM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Ghana', 'GH', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Guinea', 'GN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Guinea-Bissau', 'GW', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Kenya', 'KE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Lesotho', 'LS', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Liberia', 'LR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Libya', 'LY', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Madagascar', 'MG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Malawi', 'MW', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Mali', 'ML', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Mauritania', 'MR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Mauritius', 'MU', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Morocco', 'MA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Mozambique', 'MZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Namibia', 'NA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Niger', 'NE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Nigeria', 'NG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Rwanda', 'RW', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Sao Tome and Principe', 'ST', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Senegal', 'SN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Seychelles', 'SC', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Sierra Leone', 'SL', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Somalia', 'SO', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('South Africa', 'ZA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Sudan', 'SD', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Swaziland', 'SZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Tanzania', 'TZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Togo', 'TG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Tunisia', 'TN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Uganda', 'UG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Western Sahara', 'EH', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Zambia', 'ZM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Zimbabwe', 'ZW', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);

  // header - region ANTARCTICA
  err := DictionaryRepo.AddDictionaryRow('Antarctica', 'Antarctica', 2, dictionaryRegionId, parentDictionaryRowId);

  // header - region ASIA
  err := DictionaryRepo.AddDictionaryRow('Asia', 'Asia', 3, dictionaryRegionId, parentDictionaryRowId);
  // items - country
  err := DictionaryRepo.AddDictionaryRow('Afghanistan', 'AF', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Armenia', 'AM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Azerbaijan', 'AZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Bahrain', 'BH', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Bangladesh', 'BD', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Bhutan', 'BT', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Brunei', 'BN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Burma (Myanmar)', 'MM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Cambodia', 'KH', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('China', 'CN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Georgia', 'GE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Hong Kong', 'HK', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('India', 'IN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Indonesia', 'ID', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Iran', 'IR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Iraq', 'IQ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Israel', 'IL', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Japan', 'JP', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Jordan', 'JO', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Kazakhstan', 'KZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Kuwait', 'KQ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Kyrgyzstan', 'KG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Laos', 'LA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Lebanon', 'LB', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Malaysia', 'MY', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Maldives', 'MV', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Mongolia', 'MN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Nepal', 'NP', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('North Korea', 'KP', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Oman', 'OM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Pakistan', 'PK', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Philippines', 'PH', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Qatar', 'QA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Russia', 'RU', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Saudi Arabia', 'SA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Singapore', 'SG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('South Korea', 'KR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Sri Lanka', 'LK', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Syria', 'SY', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Taiwan', 'TW', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Tajikistan', 'TJ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Thailand', 'TH', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Turkey', 'TR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Turkmenistan', 'TM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('United Arab Emirates', 'AE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Uzbekistan', 'UZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Vietnam', 'VN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Yemen', 'YE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);

  // header - region AUSTRALIA AND OCEANIA
  err := DictionaryRepo.AddDictionaryRow('Australia and Oceania', 'AustraliaAndOceania', 4, dictionaryRegionId, parentDictionaryRowId);
  // items - country
  err := DictionaryRepo.AddDictionaryRow('Australia', 'AU', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Fiji', 'FJ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Kiribati', 'KI', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Marshall Islands', 'MH', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Micronesia', 'FM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Nauru', 'NR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('New Zealand', 'NZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Palau', 'PW', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Papua New Guinea', 'PG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Samoa', 'WS', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Solomon Islands', 'SB', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Tonga', 'TO', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Tuvalu', 'TV', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Vanuatu', 'VU', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);

  // header - region EUROPE
  err := DictionaryRepo.AddDictionaryRow('Europe', 'Europe', 5, dictionaryRegionId, parentDictionaryRowId);
  // items - country
  err := DictionaryRepo.AddDictionaryRow('Albania', 'AL', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Andorra', 'AD', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Austria', 'AT', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Belarus', 'BY', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Belgium', 'BE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Bosnia and Herzegovina', 'BA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Bulgaria', 'BG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Croatia', 'HR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Cyprus', 'CY', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Czech Republic', 'CZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Denmark', 'DK', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Estonia', 'EE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Finland', 'FI', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('France', 'FR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Germany', 'DE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Greece', 'GR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Hungary', 'HU', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Iceland', 'IS', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Ireland', 'IE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Italy', 'IT', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Latvia', 'LV', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Liechtenstein', 'LI', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Lithuania', 'LT', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Luxembourg', 'LU', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Macedonia', 'MK', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Malta', 'MT', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Moldova', 'MD', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Monaco', 'MC', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Montenegro', 'ME', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Netherlands', 'NL', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Norway', 'NO', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Poland', 'PL', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Portugal', 'PT', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Romania', 'RO', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('San Marino', 'SM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Serbia', 'RS', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Slovakia', 'SK', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Slovenia', 'SI', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Spain', 'ES', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Sweden', 'SE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Switzerland', 'CH', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Turkey', 'TR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Ukraine', 'UA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('United Kingdom', 'GB', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Vatican', 'VA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);

  // header - region NORTH AMERICA
  err := DictionaryRepo.AddDictionaryRow('North America', 'NorthAmerica', 6, dictionaryRegionId, parentDictionaryRowId);
  // items - country
  err := DictionaryRepo.AddDictionaryRow('Antigua and Barbuda', 'AG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Bahamas', 'BS', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Barbados', 'BB', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Belize', 'BZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Canada', 'CA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Costa Rica', 'CR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Cuba', 'CU', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Dominica', 'DM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Dominican Republic', 'DO', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('El Salvador', 'SV', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Greenland', 'GL', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Grenada', 'GD', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Guatemala', 'GT', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Haiti', 'HT', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Honduras', 'HN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Jamaica', 'JM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Mexico', 'MX', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Nicaragua', 'NI', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Panama', 'PA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Saint Kitts and Nevis', 'KN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Saint Lucia', 'LC', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Saint Vincent and the Grenadines', 'VC', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Trinidad and Tobago', 'TT', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('United States', 'US', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);

  // header - region SOUTH AMERICA
  err := DictionaryRepo.AddDictionaryRow('South America', 'SouthAmerica', 7, dictionaryRegionId, parentDictionaryRowId);
  // items - country
  err := DictionaryRepo.AddDictionaryRow('Argentina', 'AR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Bolivia', 'BO', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Brazil', 'BR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Chile', 'CL', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Colombia', 'CO', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Ecuador', 'EC', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('FRENCH GUIANA', 'GF', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Guyana', 'GY', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Paraguay', 'PY', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Peru', 'PE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Suriname', 'SR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Uruguay', 'UY', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := DictionaryRepo.AddDictionaryRow('Venezuela', 'VE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);

  Result := err;
end;

end.

