unit CTRPCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  ICTRPControl = interface
    ['{4225E4DA-E7C9-45FD-BDC9-6215D8B31578}']
    function GetRttiPropertiesToIgnore: string;
    procedure SetRttiPropertiesToIgnore(const AValue: string);

    property RttiPropertiesToIgnore: string
      read GetRttiPropertiesToIgnore write SetRttiPropertiesToIgnore;
  end;

implementation

end.

