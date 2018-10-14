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

implementation

end.

