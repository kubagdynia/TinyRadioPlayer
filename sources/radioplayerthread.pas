unit RadioPlayerThread;
{===============================================================================
File:                RadioPlayerThread.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Thread that supports stream play and all related tasks

================================================================================}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LMessages, LCLIntf, Dialogs, LCLType, FileUtil, Consts,
  lazdynamic_bass;

const
  WM_INFO_UPDATE = WM_USER + 101;
  PLAY_PROGRESS = 25;

type
  // Types used by events to synchronize the data
  TStreamGetTagsEvent = procedure(ASender: TObject; AMsg: string; AMsgNumber: byte) of object;
  TStreamStatusEvent = procedure(ASender: TObject; AThreadIndex: integer) of object;

  // Custom thread class
  TRadioPlayerThread = class(TThread)
  private
    FActive: boolean;
    FThreadIndex: integer;
    FChan: HSTREAM;
    FVolume: integer;  // main volume
    FReq: DWord;
    FCritSection: TCriticalSection;
    FChannelStatus: DWord;         // BASS_ACTIVE_STOPPED, BAS_ACTIVE_PLAYING, BASS_ACTIVE_STALLED, BASS_ACTIVE_PAYSED
    FStreamUrlToPlay: string;

    FPlayerMessage: string;
    FPlayerMessageNumber: integer;

    // floating-point channel support? 0 = no, else yes
    FFloatable: DWord;

    // Custom events
    FOnStreamPlaying: TStreamStatusEvent;
    FOnStreamStopped: TNotifyEvent;
    FOnStreamPaused: TNotifyEvent;
    FOnStreamStalled: TNotifyEvent;
    FOnStreamGetTags: TStreamGetTagsEvent;

    procedure DoFadeIn(time: integer = 1000);
    procedure DoFadeOut(time: integer = 200);

    function OpenURL(url: PChar): Integer;
    procedure StreamStop;
    procedure MetaStream;
    procedure SendPlayerMessage(AMsg: string; AMsgNumber: byte);
    procedure CheckBufferProgress;
  protected
    procedure Execute; override;
    procedure SynchronizePlayerMessage;
    procedure SynchronizeOnStreamPlaying;
    procedure SynchronizeOnStreamStopped;
    procedure SynchronizeOnStreamPaused;
    procedure SynchronizeOnStreamStalled;
  public
    constructor Create(CreateSuspended : boolean; Floatable: DWord = 0);
    destructor Destroy; override;

    function ErrorGetCode: Integer;
    function ChannelGetLevel: DWORD;
    function StreamGetFilePosition(mode: DWORD): QWORD;
    function ChannelIsActiveAndPlaying: Boolean;
    function ChannelIsActiveAndPaused: Boolean;
    function ChannelIsActiveAndStopped: Boolean;
    function ChannelIsActiveAndStalled: Boolean;
    function Pause: Boolean;
    function Stop: Boolean;
    function ChangeVolume(Value: Integer): Boolean;
    function PlayURL(AStreamUrl: string;
      const AVolume: Integer; const AThreadIndex: Integer): Boolean;

    property OnStreamPlaying: TStreamStatusEvent read FOnStreamPlaying write FOnStreamPlaying;
    property OnStreamStopped: TNotifyEvent read FOnStreamStopped write FOnStreamStopped;
    property OnStreamPaused: TNotifyEvent read FOnStreamPaused write FOnStreamPaused;
    property OnStreamStalled: TNotifyEvent read FOnStreamStalled write FOnStreamStalled;
    property OnStreamGetTags: TStreamGetTagsEvent read FOnStreamGetTags write FOnStreamGetTags;
    property Active: boolean read FActive;

  end;

implementation

// MetaSync
procedure StatusProc(buffer: Pointer; len, user: DWORD); stdcall;
begin
  //if (buffer <> nil) and (len = 0) then
    //SendPlayerMessage(8, LPARAM(PChar(buffer)));}
end;

// MetaSync
procedure MetaSync(handle: HSYNC; channel, data, user: DWORD); stdcall;
begin
  //DoMeta(channel);
end;

// Fade in effect
procedure TRadioPlayerThread.DoFadeIn(time: integer);
begin
  // Sets the volume of the station and at the same time use the
  // fade-in effect, which gradually increase the volume

  // Set 0 at the beginning
  BASS_ChannelSetAttribute(FChan, BASS_ATTRIB_VOL, 0);

  // Slides a channel's attribute from its current value to a new value.
  BASS_ChannelSlideAttribute(FChan, BASS_ATTRIB_VOL, FVolume / 100, time);
end;

// Fade out effect
procedure TRadioPlayerThread.DoFadeOut(time: integer);
begin
  // If the radio is playing we do fade-out
  if BASS_ChannelIsActive(FChan) in [BASS_ACTIVE_PLAYING] then
  begin
    // Slides a channel's attribute from its current value to a new value.
    BASS_ChannelSlideAttribute(FChan, BASS_ATTRIB_VOL, 0, time);
    // Check if an attribute of stream is sliding
    while BASS_ChannelIsSliding(FChan, BASS_ATTRIB_VOL) do
      Sleep(1);
  end;
end;

function TRadioPlayerThread.OpenURL(url: PChar): Integer;
var
  icy: PAnsiChar;
  len, progress: DWORD;
  c, r: DWORD;
begin
  SendPlayerMessage('0', 2); // set progress bar to 0

  EnterCriticalSection(FCritSection);
  FReq := FReq + 1;
  r := FReq;
  LeaveCriticalSection(FCritSection);

  // If the radio is playing we do fade-out
  DoFadeOut;

  BASS_StreamFree(FChan); // close old stream

  progress := 0;
  // Send information that we try to connect
  SendPlayerMessage('', 0);

  // Creates a sample stream from an MP3, MP2, MP1, OGG, WAV, AIFF or plugin
  // supported file on the internet, optionally receiving the downloaded
  // data in a callback function.
  c := BASS_StreamCreateURL(url, 0, BASS_STREAM_STATUS or FFloatable,
    nil, nil);

  EnterCriticalSection(FCritSection);
  if (r <> FReq) then
  begin
    LeaveCriticalSection(FCritSection);
    if (c = 0) then
      BASS_StreamFree(c);
    Exit;
  end;
  FChan := c;
  LeaveCriticalSection(FCritSection);

  if (FChan = 0) then
  begin
    // Catch the error here inside the Thread
    // and send it to the WndProc
    SendPlayerMessage(IntToStr(Bass_ErrorGetCode()), 1); // Oops Error
  end
  else
  begin
    // Progress
    repeat
      if Terminated then
        Exit;

      // Retrieves the file position/status of a stream.
      len := BASS_StreamGetFilePosition(FChan, BASS_FILEPOS_END);
        if (len = DW_Error) then
        begin
          SendPlayerMessage(IntToStr(Bass_ErrorGetCode()), 1); // Oops Error
          Exit; // something's gone wrong! (eg. BASS_Free called)
        end;

        progress := (BASS_StreamGetFilePosition(FChan, BASS_FILEPOS_DOWNLOAD) -
          BASS_StreamGetFilePosition(FChan, BASS_FILEPOS_CURRENT)) * 100 div len;

        // percentage of buffer filled
        SendPlayerMessage(IntToStr(progress), 2); // show progess bar
    until
      progress > PLAY_PROGRESS;

    DoFadeIn;

    // get the broadcast name and bitrate
      icy := BASS_ChannelGetTags(FChan, BASS_TAG_ICY);

      if (icy = nil) then
        icy := BASS_ChannelGetTags(FChan, BASS_TAG_HTTP); // no ICY tags, try HTTP
      if (icy <> nil) then
        while (icy^ <> #0) do
        begin
          if (Copy(icy, 1, 9) = 'icy-name:') then
            SendPlayerMessage(Copy(icy, 10, MaxInt), 3)
          else if (Copy(icy, 1, 7) = 'icy-br:') then
            SendPlayerMessage(Copy(icy, 8, MaxInt), 4); // bitrate
          icy := icy + Length(icy) + 1;
        end;

      // get the stream title and set sync for subsequent titles
      MetaStream;

      // Sets up a synchronizer stream channel.
      BASS_ChannelSetSync(FChan, BASS_SYNC_META, 0, SYNCPROC(@MetaSync), nil);

      // play it!
      if BASS_ChannelPlay(FChan, FALSE) then
      begin
        SendPlayerMessage('', 25);
      end;

  end;

end;



constructor TRadioPlayerThread.Create(CreateSuspended: boolean;
  Floatable: DWord = 0);
begin
  FActive := false;
  FThreadIndex := EMPTY_INT;
  FChan := 0;
  FReq := 0;

  FStreamUrlToPlay := EMPTY_STR;

  FFloatable := Floatable;

  FChannelStatus := BASS_ACTIVE_STOPPED;

  {initialize critical section
   http://www.lazarus.freepascal.org/index.php/topic,15321.msg82202.html#msg82202
   unit       RTL(system)           windows	               libc	                  lclintf,lcltype
   parameter  TRTLCriticalSection   TRTLCriticalSection	       TRTLCriticalSection	  TCriticalSection
   function   InitCriticalSection   InitializeCriticalSection  InitializeCriticalSection  InitializeCriticalSection
   function   EnterCriticalSection  EnterCriticalSection       EnterCriticalSection	  EnterCriticalSection
   function   LeaveCriticalSection  LeaveCriticalSection       LeaveCriticalSection 	  LeaveCriticalSection
   function   DoneCriticalSection   DeleteCriticalSection      DeleteCriticalSection	  DeleteCriticalSection
  }
  InitializeCriticalSection(FCritSection);

  FreeOnTerminate := False;
  FOnStreamPaused := nil;
  FOnStreamPlaying := nil;
  FOnStreamStopped := nil;
  FOnStreamStalled := nil;

  inherited Create(CreateSuspended);
end;

destructor TRadioPlayerThread.Destroy;
begin
  StreamStop;

  // Delete critical section
  DeleteCriticalSection(FCritSection);

  inherited Destroy;
end;

// Retrieves the error code for the most recent BASS function
// call in the current thread.
function TRadioPlayerThread.ErrorGetCode: Integer;
begin
  Result := BASS_ErrorGetCode();
end;

// Retrieves the level (peak amplitude) of a stream
function TRadioPlayerThread.ChannelGetLevel: DWORD;
begin
  // The level of the left channel is returned in the low word (low 16-bits),
  // and the level of the right channel is returned in the high
  // word (high 16-bits). If the channel is mono, then the low word is
  // duplicated in the high word.
  // The level ranges linearly from 0 (silent) to 32768 (max).
  // 0 will be returned when a channel is stalled.

  Result := BASS_ChannelGetLevel(FChan);
end;

// Retrieves the file position/status of a stream.
function TRadioPlayerThread.StreamGetFilePosition(mode: DWORD): QWORD;
begin
  Result := BASS_StreamGetFilePosition(FChan, mode);
end;

// Check if channel is active and paused
function TRadioPlayerThread.ChannelIsActiveAndPlaying: Boolean;
begin
  // The channel is playing (or recording).
  Result := BASS_ChannelIsActive(FChan) = BASS_ACTIVE_PLAYING;
end;

// Check if channel is active and paused
function TRadioPlayerThread.ChannelIsActiveAndPaused: Boolean;
begin
  // The channel is paused.
  Result := BASS_ChannelIsActive(FChan) = BASS_ACTIVE_PAUSED;
end;

// Check if channel is active and stopped
function TRadioPlayerThread.ChannelIsActiveAndStopped: Boolean;
begin
  // The channel is not active, or handle is not a valid channel.
  Result := BASS_ChannelIsActive(FChan) = BASS_ACTIVE_PAUSED;
end;

// Check if channel is active and stalled
function TRadioPlayerThread.ChannelIsActiveAndStalled: Boolean;
begin
  // Playback of the stream has been stalled due to a lack of sample data.
  // The playback will automatically resume once there
  // is sufficient data to do so.
  Result := BASS_ChannelIsActive(FChan) = BASS_ACTIVE_PAUSED;
end;

// Pause and resume a stream
function TRadioPlayerThread.Pause: Boolean;
begin
  case BASS_ChannelIsActive(FChan) of
    BASS_ACTIVE_PAUSED  : begin
      DoFadeIn;
      Result := BASS_ChannelPlay(FChan, False);
    end;
    BASS_ACTIVE_PLAYING : begin
      DoFadeOut;
      Result := BASS_ChannelPause(FChan);
    end;
    else Result := True;
  end;
end;

// Stop a stream
function TRadioPlayerThread.Stop: Boolean;
begin
  Result := True;

  if BASS_ChannelIsActive(FChan) <> 0 then // in [BASS_ACTIVE_PLAYING, BASS_ACTIVE_PAUSED] then
  begin
    DoFadeOut;

    Result := BASS_ChannelStop(FChan);
    Result := BASS_StreamFree(FChan);
  end;

  FActive := false;
end;

// Volume level
function TRadioPlayerThread.ChangeVolume(Value: Integer): Boolean;
begin
  FVolume := Value;
  Result := BASS_ChannelSetAttribute(FChan, BASS_ATTRIB_VOL, FVolume / 100);
end;

function TRadioPlayerThread.PlayURL(AStreamUrl: string;
  const AVolume: Integer; const AThreadIndex: Integer): Boolean;
begin
  StreamStop;

  BASS_StreamFree(FChan);

  FVolume := AVolume;
  FThreadIndex := AThreadIndex;
  FActive := true;
  FChannelStatus := BASS_ACTIVE_STOPPED;
  // Copy these data to used it by execute method
  FStreamUrlToPlay := AStreamUrl;

end;

procedure TRadioPlayerThread.StreamStop;
begin
  // Stop a stream
  if BASS_ChannelIsActive(FChan) <> 0 then
  begin
    BASS_ChannelStop(FChan);
    BASS_StreamFree(FChan);
  end;
  FActive := false;
end;

// Update stream info from metadata
procedure TRadioPlayerThread.MetaStream;
var
  meta: PAnsiChar;
  p: Integer;
  metaChanInfo: BASS_CHANNELINFO;
begin
  if BASS_ChannelIsActive(FChan) <> BASS_ACTIVE_PLAYING then Exit;

  // Retrieves tags/headers from a channel.
  // Shoutcast metadata. A single string is returned, containing the current
  // stream title and url (usually omitted).
  // The format of the string is: StreamTitle='xxx';StreamUrl='xxx';
  meta := BASS_ChannelGetTags(FChan, BASS_TAG_META);

  if (meta <> nil) then
  begin
    p := Pos('StreamTitle=', AnsiString(meta));
    if (p = 0) then
      Exit;
    p := p + 13;
    SendPlayerMessage(Copy(meta, p, Pos(';', String(meta)) - p - 1), 7);
  end;

  // Retrieves information on a channel.
  BASS_ChannelGetInfo(FChan, metaChanInfo);

  { Mono / Stereo }
  if metaChanInfo.chans = 2 then
    SendPlayerMessage('stereo', 10)
  else
  if metaChanInfo.chans = 1 then
    SendPlayerMessage('mono', 10);

  { Freq }
  if metaChanInfo.freq <> 0 then
    SendPlayerMessage(IntToStr(metaChanInfo.freq), 11)
  else
    SendPlayerMessage(EMPTY_STR, 11);

  // Retrieves tags/headers from a channel.
  // ICY (Shoutcast) tags. A pointer to a series of null-terminated strings
  // is returned, the final string ending with a double null.
  meta := BASS_ChannelGetTags(FChan, BASS_TAG_ICY);

  // HTTP headers, only available when streaming from a HTTP server.
  // A pointer to a series of null-terminated strings is returned, the final
  // string ending with a double null.
  if (meta = nil) then
    meta := BASS_ChannelGetTags(FChan, BASS_TAG_HTTP); // no ICY tags, try HTTP

  if (meta <> nil) then
    while (meta^ <> #0) do
    begin
      if (Copy(meta, 1, 9) = 'icy-name:') then
        SendPlayerMessage(Copy(meta, 10, MaxInt), 3)
      else if (Copy(meta, 1, 7) = 'icy-br:') then
        SendPlayerMessage(Copy(meta, 8, MaxInt), 4);
      meta := meta + Length(meta) + 1;
    end;

end;

procedure TRadioPlayerThread.SendPlayerMessage(AMsg: string; AMsgNumber: byte);
begin
  FPlayerMessage := AMsg;
  FPlayerMessageNumber := AMsgNumber;

  // Thread should not interact with the visible components so we call method
  // through Synchronize to do it within the context of the main thread
  Synchronize(@SynchronizePlayerMessage);

  // 1 indicates that an error occurred, so switch active to false
  // to close a thread
  if AMsgNumber = 1 then
    FActive := false;
end;

//
procedure TRadioPlayerThread.SynchronizePlayerMessage;
begin
  // this method is executed by the mainthread and can therefore access all GUI elements.
  if Assigned(OnStreamGetTags) then
    OnStreamGetTags(Self, FPlayerMessage, FPlayerMessageNumber);
end;

procedure TRadioPlayerThread.SynchronizeOnStreamPlaying;
begin
  if Assigned(OnStreamPlaying) then
    OnStreamPlaying(Self, FThreadIndex);
end;

procedure TRadioPlayerThread.SynchronizeOnStreamPaused;
begin
  if Assigned(OnStreamPaused) then
    OnStreamPaused(Self);
end;

procedure TRadioPlayerThread.SynchronizeOnStreamStalled;
begin
  if Assigned(OnStreamStalled) then
    OnStreamStalled(Self);
end;

procedure TRadioPlayerThread.SynchronizeOnStreamStopped;
begin
  if Assigned(OnStreamStopped) then
    OnStreamStopped(Self);
end;

procedure TRadioPlayerThread.CheckBufferProgress;
var
  len, progress: DWORD;
begin
  if BASS_ChannelIsActive(FChan) <> BASS_ACTIVE_PLAYING then Exit;

  // BASS_FILEPOS_END - End of audio data. When streaming in blocks (the BASS_STREAM_BLOCK flag is
  // in effect), the download buffer length is given.

  // Retrieves the buffer length of a stream.
  len := BASS_StreamGetFilePosition(FChan, BASS_FILEPOS_END);

  // BASS_FILEPOS_DOWNLOAD - Download progress of an internet file stream or "buffered" user file stream.
  // BASS_FILEPOS_CURRENT - Position that is to be decoded for playback next.
  // This will be a bit ahead of the position actually being heard due to buffering.
  progress := (BASS_StreamGetFilePosition(FChan, BASS_FILEPOS_DOWNLOAD) -
    BASS_StreamGetFilePosition(FChan, BASS_FILEPOS_CURRENT)) * 100 div len;

  SendPlayerMessage(IntToStr(progress), 2); // Buffering progress
end;

procedure TRadioPlayerThread.Execute;
var
  channelStatus: DWORD;
  urlPlay: string;
begin
  while (not Terminated) do
  begin
    if (FStreamUrlToPlay <> EmptyStr) then
    begin
      urlPlay := FStreamUrlToPlay;
      FStreamUrlToPlay := EmptyStr;
      OpenURL(PChar(urlPlay));
    end;

    channelStatus := BASS_ChannelIsActive(FChan);
    if channelStatus <> FChannelStatus then
    begin
      FChannelStatus := channelStatus;
      // Launch the event
      case channelStatus of
        BASS_ACTIVE_STOPPED: Synchronize(@SynchronizeOnStreamStopped);
        BASS_ACTIVE_PLAYING: Synchronize(@SynchronizeOnStreamPlaying);
        BASS_ACTIVE_STALLED: Synchronize(@SynchronizeOnStreamStalled);
        BASS_ACTIVE_PAUSED: Synchronize(@SynchronizeOnStreamPaused);
      end;
    end;

    CheckBufferProgress;

    Sleep(100);

    if not FActive then StreamStop else MetaStream;
  end;

end;

end.

