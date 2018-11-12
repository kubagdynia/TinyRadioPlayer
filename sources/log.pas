unit Log;
{===============================================================================
File:                Log.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Logging errors and warnings

================================================================================}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Consts;

type

  TLog = class sealed (TObject)
  private
    class procedure SaveToFile(const Message: string);
  public
    class procedure LogException(const Message: string;
      const NameOfTheClass: string = EMPTY_STR;
      const NameOfTheMethod: string = EMPTY_STR;
      const E: Exception = nil); // A static method
  end;

implementation

uses
  Helpers;

class procedure TLog.LogException(const Message: string; const NameOfTheClass: string;
  const NameOfTheMethod: string; const E: Exception);
var
  exceptionMessage: string;
begin
  // Get exception message
  if E <> nil then
    exceptionMessage := E.Message
  else
    exceptionMessage := EMPTY_STR;

  // Save a message
  SaveToFile(
    Format('%s %s.%s:%s error raised, with message: %s',
      [DateTimeToStr(Now), NameOfTheClass, NameOfTheMethod, NameOfTheClass,
       Message + IIF(exceptionMessage <> EMPTY_STR, #13#10 + exceptionMessage, EMPTY_STR)]
    )
  );
end;

// Saves the message to the disk
class procedure TLog.SaveToFile(const Message: string);
var
  TF: TextFile;
  Path: string;
begin
  // path to the log file
  Path :=  ConcatPaths([GetApplicationPath, LOG_NAME]);

  AssignFile(TF, Path);
  try
    if FileExists(Path) then
      Append(TF)
    else
      Rewrite(TF);

    Writeln(TF, Message);

    Flush(TF);

  finally
    CloseFile(TF);
  end;
end;

end.

