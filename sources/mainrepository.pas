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
    err := CreateStations;
    err := CreateDictionaries;
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
  stationId: integer;
begin
  err := ERR_OK;

  // Stations
  err := StationRepo.AddStation('Radio Kaszebe', 'http://stream3.nadaje.com:8048',
    EMPTY_STR, 'http://radiokaszebe.pl/', 'Pop', 'PL', stationId);

  err := StationRepo.AddStation('Radio Malbork', 'http://78.46.246.97:9022',
    EMPTY_STR, 'https://www.radiomalbork.fm/', 'Pop', 'PL', stationId);

  err := StationRepo.AddStation('Planeta RnB', 'http://plarnb-01.cdn.eurozet.pl:8216/',
    EMPTY_STR, 'https://www.planetafm.pl/', 'RnBSoul', 'PL', stationId);

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
  err := AddDictionary('Genre', DICTIONARY_GENRE_CODE, dictionaryId);

  err := AddDictionaryRow('Alternative Music', 'AlternativeMusic', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('Blues', 'Blues', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('Classical Music', 'ClassicalMusic', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('Country Music', 'CountryMusic', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('Dance Music', 'DanceMusic', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('Easy Listening', 'EasyListening', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('Electronic Music', 'ElectronicMusic', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('European Music (Folk / Pop)', 'EuropeanMusic', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('Hip Hop / Rap', 'HipHopRap', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('Indie Pop', 'IndiePop', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('Inspirational (incl. Gospel)', 'Inspirational', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('Asian Pop (J-Pop, K-pop)', 'AsianPop', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('Jazz', 'Jazz', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('Latin Music', 'LatinMusic', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('New Age', 'NewAge', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('Opera', 'Opera', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('Pop (Popular music)', 'Pop', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('RnB / Soul', 'RnBSoul', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('Reggae', 'Reggae', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('Rock', 'Rock', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('Singer / Songwriter (inc. Folk)', 'SingerSongwriter', 1, dictionaryId, dictionaryRowId);
  err := AddDictionaryRow('World Music / Beats', 'WorldMusicBeats', 1, dictionaryId, dictionaryRowId);

  // REGION & COUNTRY
  err := AddDictionary('Region', DICTIONARY_REGION_CODE, dictionaryRegionId);
  err := AddDictionary('Country', DICTIONARY_COUNTRY_CODE, dictionaryCountryId);

  // header - region AFRICA
  err := AddDictionaryRow('Africa', 'Africa', 1, dictionaryRegionId, parentDictionaryRowId);
  // items - country
  err := AddDictionaryRow('Algeria', 'DZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Angola', 'AO', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Benin', 'BJ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Botswana', 'BW', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Burkina Faso', 'BF', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Burundi', 'BI', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Cameroon', 'CM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Cape Verde', 'CV', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Central African Republic', 'CF', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Chad', 'TD', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Comoros', 'KM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('CONGO, Democratic Republic of', 'CD', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('CONGO, Republic of', 'CG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Cote d''Ivoire', 'CI', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Djibouti', 'DJ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Egypt', 'EG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Equatorial Guinea', 'GQ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Eritrea', 'ER', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Ethiopia', 'ET', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Gabon', 'GA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Gambia', 'GM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Ghana', 'GH', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Guinea', 'GN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Guinea-Bissau', 'GW', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Kenya', 'KE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Lesotho', 'LS', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Liberia', 'LR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Libya', 'LY', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Madagascar', 'MG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Malawi', 'MW', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Mali', 'ML', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Mauritania', 'MR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Mauritius', 'MU', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Morocco', 'MA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Mozambique', 'MZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Namibia', 'NA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Niger', 'NE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Nigeria', 'NG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Rwanda', 'RW', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Sao Tome and Principe', 'ST', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Senegal', 'SN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Seychelles', 'SC', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Sierra Leone', 'SL', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Somalia', 'SO', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('South Africa', 'ZA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Sudan', 'SD', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Swaziland', 'SZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Tanzania', 'TZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Togo', 'TG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Tunisia', 'TN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Uganda', 'UG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Western Sahara', 'EH', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Zambia', 'ZM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Zimbabwe', 'ZW', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);

  // header - region ANTARCTICA
  err := AddDictionaryRow('Antarctica', 'Antarctica', 2, dictionaryRegionId, parentDictionaryRowId);

  // header - region ASIA
  err := AddDictionaryRow('Asia', 'Asia', 3, dictionaryRegionId, parentDictionaryRowId);
  // items - country
  err := AddDictionaryRow('Afghanistan', 'AF', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Armenia', 'AM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Azerbaijan', 'AZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Bahrain', 'BH', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Bangladesh', 'BD', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Bhutan', 'BT', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Brunei', 'BN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Burma (Myanmar)', 'MM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Cambodia', 'KH', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('China', 'CN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Georgia', 'GE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Hong Kong', 'HK', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('India', 'IN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Indonesia', 'ID', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Iran', 'IR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Iraq', 'IQ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Israel', 'IL', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Japan', 'JP', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Jordan', 'JO', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Kazakhstan', 'KZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Kuwait', 'KQ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Kyrgyzstan', 'KG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Laos', 'LA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Lebanon', 'LB', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Malaysia', 'MY', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Maldives', 'MV', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Mongolia', 'MN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Nepal', 'NP', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('North Korea', 'KP', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Oman', 'OM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Pakistan', 'PK', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Philippines', 'PH', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Qatar', 'QA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Russia', 'RU', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Saudi Arabia', 'SA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Singapore', 'SG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('South Korea', 'KR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Sri Lanka', 'LK', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Syria', 'SY', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Taiwan', 'TW', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Tajikistan', 'TJ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Thailand', 'TH', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Turkey', 'TR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Turkmenistan', 'TM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('United Arab Emirates', 'AE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Uzbekistan', 'UZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Vietnam', 'VN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Yemen', 'YE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);

  // header - region AUSTRALIA AND OCEANIA
  err := AddDictionaryRow('Australia and Oceania', 'AustraliaAndOceania', 4, dictionaryRegionId, parentDictionaryRowId);
  // items - country
  err := AddDictionaryRow('Australia', 'AU', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Fiji', 'FJ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Kiribati', 'KI', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Marshall Islands', 'MH', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Micronesia', 'FM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Nauru', 'NR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('New Zealand', 'NZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Palau', 'PW', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Papua New Guinea', 'PG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Samoa', 'WS', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Solomon Islands', 'SB', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Tonga', 'TO', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Tuvalu', 'TV', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Vanuatu', 'VU', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);

  // header - region EUROPE
  err := AddDictionaryRow('Europe', 'Europe', 5, dictionaryRegionId, parentDictionaryRowId);
  // items - country
  err := AddDictionaryRow('Albania', 'AL', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Andorra', 'AD', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Austria', 'AT', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Belarus', 'BY', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Belgium', 'BE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Bosnia and Herzegovina', 'BA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Bulgaria', 'BG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Croatia', 'HR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Cyprus', 'CY', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Czech Republic', 'CZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Denmark', 'DK', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Estonia', 'EE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Finland', 'FI', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('France', 'FR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Germany', 'DE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Greece', 'GR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Hungary', 'HU', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Iceland', 'IS', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Ireland', 'IE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Italy', 'IT', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Latvia', 'LV', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Liechtenstein', 'LI', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Lithuania', 'LT', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Luxembourg', 'LU', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Macedonia', 'MK', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Malta', 'MT', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Moldova', 'MD', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Monaco', 'MC', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Montenegro', 'ME', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Netherlands', 'NL', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Norway', 'NO', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Poland', 'PL', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Portugal', 'PT', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Romania', 'RO', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('San Marino', 'SM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Serbia', 'RS', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Slovakia', 'SK', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Slovenia', 'SI', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Spain', 'ES', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Sweden', 'SE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Switzerland', 'CH', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Turkey', 'TR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Ukraine', 'UA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('United Kingdom', 'GB', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Vatican', 'VA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);

  // header - region NORTH AMERICA
  err := AddDictionaryRow('North America', 'NorthAmerica', 6, dictionaryRegionId, parentDictionaryRowId);
  // items - country
  err := AddDictionaryRow('Antigua and Barbuda', 'AG', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Bahamas', 'BS', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Barbados', 'BB', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Belize', 'BZ', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Canada', 'CA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Costa Rica', 'CR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Cuba', 'CU', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Dominica', 'DM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Dominican Republic', 'DO', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('El Salvador', 'SV', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Greenland', 'GL', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Grenada', 'GD', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Guatemala', 'GT', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Haiti', 'HT', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Honduras', 'HN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Jamaica', 'JM', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Mexico', 'MX', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Nicaragua', 'NI', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Panama', 'PA', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Saint Kitts and Nevis', 'KN', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Saint Lucia', 'LC', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Saint Vincent and the Grenadines', 'VC', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Trinidad and Tobago', 'TT', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('United States', 'US', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);

  // header - region SOUTH AMERICA
  err := AddDictionaryRow('South America', 'SouthAmerica', 7, dictionaryRegionId, parentDictionaryRowId);
  // items - country
  err := AddDictionaryRow('Argentina', 'AR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Bolivia', 'BO', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Brazil', 'BR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Chile', 'CL', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Colombia', 'CO', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Ecuador', 'EC', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('FRENCH GUIANA', 'GF', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Guyana', 'GY', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Paraguay', 'PY', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Peru', 'PE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Suriname', 'SR', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Uruguay', 'UY', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);
  err := AddDictionaryRow('Venezuela', 'VE', 1, dictionaryCountryId, parentDictionaryRowId, dictionaryRowId);

  Result := err;
end;

end.

