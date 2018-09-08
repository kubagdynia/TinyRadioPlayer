unit RadioPlayer;
{===============================================================================
File:                RadioPlayer.pas

Application Name:    Tiny Radio Player

Created:             2018 Jakub Kurlowicz (jakubkurlowicz.pl)

Description:         Radio player

================================================================================}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  LCLIntf, Dialogs, LCLType, ExtCtrls, Consts, Helpers,
  RadioPlayerThread,
  {$IFDEF USE_DYNAMIC_BASS}lazdynamic_bass{$ELSE}BASS{$ENDIF};

type
  TRadioPlayerTagsEvent = procedure(AMsg: string; AMsgNbr: byte) of object;

  TRadioPlayer = Class(TObject)
    procedure FRadioPlayerThreadsOnStreamPlaying(ASender: TObject; AThreadIndex: integer);
    procedure FRadioPlayerThreadsStreamGetTags(ASender: TObject; AMsg: string; AMsgNbr: byte);
    procedure FThreadWatcherTimer(Sender: TObject);
  private
    FActiveRadioPlayerThread: integer;
    // For now using only one thread because of thread issue
    FRadioPlayerThreads: array [1..3] of TRadioPlayerThread;
    FFloatable: DWord;
    FThreadWatcher: TTimer;
    FOnRadioPlayerTags: TRadioPlayerTagsEvent;
    FOnRadioPlay: TNotifyEvent;
    procedure Error(msg: string);
    procedure RadioInit;
    function LoadBassPlugins: Boolean;
  public
    { Public declarations }

    constructor Create; overload;
    destructor Destroy; override;

    function PlayURL(const AStreamUrl: string;
      const AVolume: ShortInt): Boolean;

    function Stop(): Boolean;
    procedure Volume(Value: Integer);

    function ChannelIsActiveAndPlaying: Boolean;
    function ChannelGetLevel: DWORD;
    function NumberOfActiveThreads: integer;

    property OnRadioPlayerTags: TRadioPlayerTagsEvent
      read FOnRadioPlayerTags write FOnRadioPlayerTags;
    property OnRadioPlay: TNotifyEvent read FOnRadioPlay write FOnRadioPlay;
  end;

var
  Proxy: array [0..99] of char; // proxy server

implementation

{ TRadioPlayer }

procedure TRadioPlayer.FRadioPlayerThreadsOnStreamPlaying(ASender: TObject;
  AThreadIndex: integer);
var
  i: integer;
begin
  try
    FActiveRadioPlayerThread := AThreadIndex;

    if Assigned(OnRadioPlay) then
      OnRadioPlay(Self);

    for i := Low(FRadioPlayerThreads) to High(FRadioPlayerThreads) do
    begin
      if (i <> FActiveRadioPlayerThread) and (FRadioPlayerThreads[i] <> nil) then
        FRadioPlayerThreads[i].Stop;
    end;
  finally

  end;
end;

procedure TRadioPlayer.FRadioPlayerThreadsStreamGetTags(ASender: TObject;
  AMsg: string; AMsgNbr: byte);
begin
  if Assigned(OnRadioPlayerTags) then
    OnRadioPlayerTags(AMsg, AMsgNbr);
end;

procedure TRadioPlayer.FThreadWatcherTimer(Sender: TObject);
var
  i: integer;
begin
  for i := Low(FRadioPlayerThreads) to High(FRadioPlayerThreads) do
  begin
    if FRadioPlayerThreads[i] <> nil then
    begin
      if not FRadioPlayerThreads[i].Active then
      begin
        FRadioPlayerThreads[i].Terminate;
        FRadioPlayerThreads[i].WaitFor;
        FreeAndNil(FRadioPlayerThreads[i]);
      end;
    end;
  end;
end;

// Displays an error message
procedure TRadioPlayer.Error(msg: string);
var
  s: string;
begin
  s := msg + NEW_LINE + '(Error code: ' + IntToStr(BASS_ErrorGetCode()) + ')';
  Dialogs.MessageDlg('error', s, mtError, [mbOk],0);
end;

procedure TRadioPlayer.RadioInit;
begin
  {$IFDEF USE_DYNAMIC_BASS}
    {$IFDEF WIN32}
      Load_BASSDLL(GetApplicationPath + '/' + LIB_PATH + 'bass.dll');
    {$ELSE}
      Load_BASSDLL(GetApplicationPath + '/' + LIB_PATH + 'libbass.so');
    {$ENDIF}
  {$ENDIF}

  // check the correct BASS was loaded
  if (HIWORD(BASS_GetVersion()) <> BASSVERSION) then
  begin
    Dialogs.MessageDlg('error','An incorrect version of BASS.DLL was loaded', mtError, [mbOk],0);
    Halt;
  end;

  // enable playlist processing
  BASS_SetConfig(BASS_CONFIG_NET_PLAYLIST, 1);
  // minimize automatic pre-buffering, so we can do it (and display it) instead
  BASS_SetConfig(BASS_CONFIG_NET_PREBUF, 0);
  // setup proxy server location
  BASS_SetConfigPtr(BASS_CONFIG_NET_PROXY, @proxy[0]);
  // enable floating-point DSP
  BASS_SetConfig(BASS_CONFIG_FLOATDSP, 1);
  {$IFNDEF WIN32}
  // set device buffer to 50ms
  // http://www.un4seen.com/forum/?topic=15263.0;hl=pulseaudio
  BASS_SetConfig(BASS_CONFIG_DEV_BUFFER, 50);
  {$ENDIF}

  // Initialize audio - default device, 44100hz, stereo, 16 bits
  if not BASS_Init(-1, 44100, 0, MainWindowHandle, nil) then
    Error('Error initializing audio!');

  // check for floating-point capability
  FFloatable :=
    BASS_StreamCreate(44100, 2, BASS_SAMPLE_FLOAT, nil, nil);
  if (FFloatable > 0) then
  begin
    BASS_StreamFree(FFloatable);
    FFloatable := BASS_SAMPLE_FLOAT;
  end;

  LoadBassPlugins;
end;

function TRadioPlayer.LoadBassPlugins: Boolean;
var
  fs: TSearchRec;
  Plug: HPLUGIN;
  Info: ^Bass_PluginInfo;
  libFullPath, libPattern: string;
begin
  try
    libFullPath := GetApplicationPath + LIB_PATH;

    // Load all bass libraries
    {$IFDEF WIN32}
    libPattern := 'bass*.dll';
    {$ELSE}
      {$IFDEF UNIX}
      libPattern := 'libbass*.so';
      {$ELSE}

      {$ENDIF}
    {$ENDIF}

    if FindFirst(libFullPath + libPattern, faAnyFile, fs) <> 0 then Exit;
    try
      repeat
        Plug := BASS_PluginLoad (PAnsiChar(libFullPath + fs.Name),
          0 {$IFDEF UNICODE} or BASS_UNICODE {$ENDIF});
        if Plug <> 0 then
        begin
          // get plugin info to add e.g. to the file selector filter...
          Info := pointer(BASS_PluginGetInfo(Plug));
        end;

      until FindNext(fs) <> 0;
    finally
      SysUtils.FindClose(fs);
    end;

    Result := True;

  except
    Result := False;
  end;
end;

constructor TRadioPlayer.Create;
begin
  inherited Create;

  RadioInit;

  FActiveRadioPlayerThread := Low(FRadioPlayerThreads);

  FThreadWatcher := TTimer.Create(nil);
  FThreadWatcher.Interval := 2000;
  FThreadWatcher.OnTimer := @FThreadWatcherTimer;
  FThreadWatcher.Enabled := True;
end;

destructor TRadioPlayer.Destroy;
var
  i: integer;
begin
  FreeAndNil(FThreadWatcher);

  for i := Low(FRadioPlayerThreads) to High(FRadioPlayerThreads) do
  begin
    if FRadioPlayerThreads[i] <> nil then
    begin
      FRadioPlayerThreads[i].Terminate;
      FRadioPlayerThreads[i].WaitFor;
      FreeAndNil(FRadioPlayerThreads[i]);
    end;
  end;

  // Close BASS
  BASS_Free();

  {$IFDEF USE_DYNAMIC_BASS}
  // release the bass library
  Unload_BASSDLL();
  {$ENDIF}

  inherited Destroy;
end;

function TRadioPlayer.PlayURL(const AStreamUrl: string;
  const AVolume: ShortInt): Boolean;
var
  threadToPlayNextStream: Integer;
begin
  if Trim(AStreamUrl) = EMPTY_STR then
    Exit;

  threadToPlayNextStream := FActiveRadioPlayerThread;

  if ChannelIsActiveAndPlaying then
  begin
    // If the current radio player thread is active and playing then we should
    // switch to another thread
    Inc(threadToPlayNextStream);

    if threadToPlayNextStream > High(FRadioPlayerThreads) then
      threadToPlayNextStream := Low(FRadioPlayerThreads);
  end;

  if FRadioPlayerThreads[threadToPlayNextStream] = nil then
  begin
    FRadioPlayerThreads[threadToPlayNextStream] := TRadioPlayerThread.Create(True, FFloatable);
    FRadioPlayerThreads[threadToPlayNextStream].OnStreamPlaying := @FRadioPlayerThreadsOnStreamPlaying;
    FRadioPlayerThreads[threadToPlayNextStream].OnStreamGetTags := @FRadioPlayerThreadsStreamGetTags;
    FRadioPlayerThreads[threadToPlayNextStream].Start;
  end;

  Result := FRadioPlayerThreads[threadToPlayNextStream].PlayURL(
      AStreamUrl, AVolume, threadToPlayNextStream);

end;

function TRadioPlayer.Stop(): Boolean;
begin
  Result := False;

  if FRadioPlayerThreads[FActiveRadioPlayerThread] <> nil then
    Result := FRadioPlayerThreads[FActiveRadioPlayerThread].Stop;
end;

procedure TRadioPlayer.Volume(Value: Integer);
begin
  if FRadioPlayerThreads[FActiveRadioPlayerThread] <> nil then
    FRadioPlayerThreads[FActiveRadioPlayerThread].ChangeVolume(Value);
end;

function TRadioPlayer.ChannelIsActiveAndPlaying: Boolean;
begin
  Result := False;

  if FRadioPlayerThreads[FActiveRadioPlayerThread] <> nil then
    Result := FRadioPlayerThreads[FActiveRadioPlayerThread].ChannelIsActiveAndPlaying;
end;

function TRadioPlayer.ChannelGetLevel: DWORD;
begin
  Result := 0;

  if FRadioPlayerThreads[FActiveRadioPlayerThread] <> nil then
    Result := FRadioPlayerThreads[FActiveRadioPlayerThread].ChannelGetLevel;
end;

function TRadioPlayer.NumberOfActiveThreads: integer;
var
  i: integer;
begin
  Result := 0;

  for i := Low(FRadioPlayerThreads) to High(FRadioPlayerThreads) do
    if FRadioPlayerThreads[i] <> nil then
      Result := Result + 1;
end;

end.

