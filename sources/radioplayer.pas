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
  RadioPlayerThread, RadioPlayerTypes, VirtualTrees,
  { Bass } lazdynamic_bass, lazdynamic_bass_fx;

const
  MAX_PLAYER_THREADS = 3;

type
  TRadioPlayerTagsEvent = procedure(AMessage: string; APlayerMessageType: TPlayerMessageType) of object;

  { TRadioPlayer }

  TRadioPlayer = Class(TObject)
  private
    FActiveRadioPlayerThread: integer;
    FRadioPlayerThreads: array [1..MAX_PLAYER_THREADS] of TRadioPlayerThread;
    FFloatable: DWord;
    FThreadWatcher: TTimer;
    FOnRadioPlayerTags: TRadioPlayerTagsEvent;
    FOnRadioPlay: TNotifyEvent;
    FCurrentStationId: integer;

    FEqualizerConfig: TEqualizerConfig;

    procedure Error(msg: string);
    procedure RadioInit;
    function LoadBassPlugins: Boolean;
    procedure RadioPlayerThreadsOnStreamPlaying(ASender: TObject; AThreadIndex: integer);
    procedure RadioPlayerThreadsStreamGetTags(ASender: TObject; AMessage:
      string; APlayerMessageType: TPlayerMessageType);
    procedure ThreadWatcherTimer(Sender: TObject);
    procedure TerminateThread(ThreadIndex: Integer; TerminateIfNotActive: Boolean);
    procedure CreateAndLaunchNewThread(ThreadIndex: Integer);

    procedure LoadEqualizerConfig;
  public
    constructor Create; overload;
    destructor Destroy; override;

    procedure PlayURL(const AStreamUrl: string; const AVolume: ShortInt);
    procedure PlayStation(const StationId: integer; const Volume: ShortInt);
    function GetSelectedStationId(var VstStationsList: TVirtualStringTree): integer;

    function Stop(): Boolean;
    procedure Volume(Value: Integer);

    procedure EqualizerEnable;
    procedure EqualizerDisable;

    function ChannelIsActiveAndPlaying: Boolean;
    function ChannelGetLevel: DWORD;
    function NumberOfRunningThreads: integer;

    function NoCurrentStationLoaded: Boolean;

    property OnRadioPlayerTags: TRadioPlayerTagsEvent
      read FOnRadioPlayerTags write FOnRadioPlayerTags;
    property OnRadioPlay: TNotifyEvent read FOnRadioPlay write FOnRadioPlay;

    property CurrentStationId: integer read FCurrentStationId;
  end;

var
  Proxy: array [0..99] of char; // proxy server

implementation

uses
  Repository, TRPSettings;

// Constructor
constructor TRadioPlayer.Create;
begin
  inherited Create;

  FCurrentStationId := EMPTY_INT;

  LoadEqualizerConfig;

  RadioInit;

  FActiveRadioPlayerThread := Low(FRadioPlayerThreads);

  // Create timer to watch threads
  FThreadWatcher := TTimer.Create(nil);
  FThreadWatcher.Interval := 2000;
  FThreadWatcher.OnTimer := @ThreadWatcherTimer;
  FThreadWatcher.Enabled := True;
end;

// Destructor
destructor TRadioPlayer.Destroy;
var
  i: integer;
begin
  FreeAndNil(FThreadWatcher);

  for i := Low(FRadioPlayerThreads) to High(FRadioPlayerThreads) do
  begin
    TerminateThread(i, false)
  end;

  if FEqualizerConfig <> nil then
    FreeAndNil(FEqualizerConfig);

  // Close BASS
  BASS_Free();

  // release the bass library
  Unload_BASSDLL;
  Unload_BASSFXDLL;

  inherited Destroy;
end;

procedure TRadioPlayer.PlayURL(const AStreamUrl: string;
  const AVolume: ShortInt);
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

  CreateAndLaunchNewThread(threadToPlayNextStream);

  FRadioPlayerThreads[threadToPlayNextStream].PlayURL(AStreamUrl, AVolume, threadToPlayNextStream);
end;

procedure TRadioPlayer.PlayStation(const StationId: integer;
  const Volume: ShortInt);
var
  StationInfo: TStationInfo;
begin
  TRepository.LoadStation(StationInfo, StationId);
  FCurrentStationId := StationId;
  PlayURL(StationInfo.StreamUrl, Volume);
end;

function TRadioPlayer.GetSelectedStationId(var VstStationsList: TVirtualStringTree): integer;
var
  node: PVirtualNode;
  data: PStationNodeRec;
begin
  Result := EMPTY_INT;

  node := VstStationsList.GetFirstSelected;

  if node <> nil then
    data := VstStationsList.GetNodeData(node)
  else
    exit;

  Result := data^.snd.ID;
end;

// Stop the stream from the active thread
function TRadioPlayer.Stop(): Boolean;
begin
  Result := False;

  if FRadioPlayerThreads[FActiveRadioPlayerThread] <> nil then
  begin
    Result := FRadioPlayerThreads[FActiveRadioPlayerThread].Stop;
    FCurrentStationId := EMPTY_INT;
  end;
end;

// Change volume of the active thread
procedure TRadioPlayer.Volume(Value: Integer);
begin
  if FRadioPlayerThreads[FActiveRadioPlayerThread] <> nil then
    FRadioPlayerThreads[FActiveRadioPlayerThread].ChangeVolume(Value);
end;

procedure TRadioPlayer.EqualizerEnable;
begin
  if FRadioPlayerThreads[FActiveRadioPlayerThread] <> nil then
    FRadioPlayerThreads[FActiveRadioPlayerThread].EqualizerEnable;
end;

procedure TRadioPlayer.EqualizerDisable;
begin
  if FRadioPlayerThreads[FActiveRadioPlayerThread] <> nil then
    FRadioPlayerThreads[FActiveRadioPlayerThread].EqualizerDisable();
end;

// Check if channel from the active thread is active and playing
function TRadioPlayer.ChannelIsActiveAndPlaying: Boolean;
begin
  Result := False;

  if FRadioPlayerThreads[FActiveRadioPlayerThread] <> nil then
    Result := FRadioPlayerThreads[FActiveRadioPlayerThread].ChannelIsActiveAndPlaying;
end;

// Retrieves the level (peak amplitude) of the stream from the active thread
function TRadioPlayer.ChannelGetLevel: DWORD;
begin
  Result := 0;

  if FRadioPlayerThreads[FActiveRadioPlayerThread] <> nil then
    Result := FRadioPlayerThreads[FActiveRadioPlayerThread].ChannelGetLevel;
end;

// Returns the number of threads that have not been terminated
function TRadioPlayer.NumberOfRunningThreads: integer;
var
  i: integer;
begin
  Result := 0;

  for i := Low(FRadioPlayerThreads) to High(FRadioPlayerThreads) do
    if FRadioPlayerThreads[i] <> nil then
      Result := Result + 1;
end;

function TRadioPlayer.NoCurrentStationLoaded: Boolean;
begin
  Result := CurrentStationId = EMPTY_INT;
end;

// Displays an error message
procedure TRadioPlayer.Error(msg: string);
var
  s: string;
begin
  s := msg + NEW_LINE + '(Error code: ' + IntToStr(BASS_ErrorGetCode()) + ')';
  Dialogs.MessageDlg('error', s, mtError, [mbOk],0);
end;

// Init Bass library
procedure TRadioPlayer.RadioInit;
begin
  {$IFDEF WIN32}
  Load_BASSDLL(ConcatPaths([GetApplicationPath, LIB_PATH, 'bass.dll']));
  Load_BASSFXDLL(ConcatPaths([GetApplicationPath, LIB_PATH, 'bass_fx.dll']));
  {$ELSE}
    {$IFDEF UNIX}
    Load_BASSDLL(ConcatPaths([GetApplicationPath, LIB_PATH, 'libbass.so']));
    Load_BASSFXDLL(ConcatPaths([GetApplicationPath, LIB_PATH, 'libbass_fx.so']));
    {$ELSE}
    Load_BASSDLL(ConcatPaths([GetApplicationPath, LIB_PATH, 'libbass.dylib']));
    Load_BASSFXDLL(ConcatPaths([GetApplicationPath, LIB_PATH, 'libbass_fx.dylib']));
    {$ENDIF}
  {$ENDIF}

  // check the correct BASS was loaded
  if (HiWord(BASS_GetVersion()) <> BASSVERSION) then
  begin
    Dialogs.MessageDlg('error','An incorrect version of BASS was loaded', mtError, [mbOk],0);
    Halt;
  end;

  // check the correct BASS_FX was loaded
  if (HiWord(BASS_FX_GetVersion()) <> BASSVERSION) then
  begin
    Dialogs.MessageDlg('error','An incorrect version of BASS_FX was loaded', mtError, [mbOk],0);
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
  begin
    Error('Error initializing audio!');
  end;

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

// Load extensions libraries
function TRadioPlayer.LoadBassPlugins: Boolean;
var
  fs: TSearchRec;
  plug: HPLUGIN;
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
      libPattern := 'libbass*.dylib';
      {$ENDIF}
    {$ENDIF}

    if FindFirst(libFullPath + libPattern, faAnyFile, fs) <> 0 then Exit;
    try
      repeat
        plug := BASS_PluginLoad (PAnsiChar(libFullPath + fs.Name),
          0 {$IFDEF UNICODE} or BASS_UNICODE {$ENDIF});

        //if plug <> 0 then
        //begin
          // get plugin info to add e.g. to the file selector filter...
          //Info := pointer(BASS_PluginGetInfo(plug));
        //end;

      until FindNext(fs) <> 0;
    finally
      SysUtils.FindClose(fs);
    end;

    Result := True;

  except
    Result := False;
  end;
end;

// Stop all threads that are not active. It's launch when the active thread is going to start playing.
procedure TRadioPlayer.RadioPlayerThreadsOnStreamPlaying(ASender: TObject;
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

// It's launch when new tags have been retrieved from the stream 
procedure TRadioPlayer.RadioPlayerThreadsStreamGetTags(ASender: TObject;
  AMessage: string; APlayerMessageType: TPlayerMessageType);
begin
  if Assigned(OnRadioPlayerTags) then
    OnRadioPlayerTags(AMessage, APlayerMessageType);
end;

// Looking for not active threads and terminate them
procedure TRadioPlayer.ThreadWatcherTimer(Sender: TObject);
var
  i: integer;
begin
  for i := Low(FRadioPlayerThreads) to High(FRadioPlayerThreads) do
    TerminateThread(i, true);
end;

// Terminate the thread with the given index
procedure TRadioPlayer.TerminateThread(ThreadIndex: Integer; TerminateIfNotActive: Boolean);
begin
  if FRadioPlayerThreads[ThreadIndex] = nil then Exit;

  if (TerminateIfNotActive) and (FRadioPlayerThreads[ThreadIndex].Active) then Exit;

  FRadioPlayerThreads[ThreadIndex].Terminate;
  FRadioPlayerThreads[ThreadIndex].WaitFor;
  FreeAndNil(FRadioPlayerThreads[ThreadIndex]);
end;

// Create and launch the new thread with the given index
procedure TRadioPlayer.CreateAndLaunchNewThread(ThreadIndex: Integer);
begin
  if FRadioPlayerThreads[ThreadIndex] = nil then
  begin
    FRadioPlayerThreads[ThreadIndex] := TRadioPlayerThread.Create(True, FEqualizerConfig, FFloatable);
    FRadioPlayerThreads[ThreadIndex].OnStreamPlaying := @RadioPlayerThreadsOnStreamPlaying;
    FRadioPlayerThreads[ThreadIndex].OnStreamGetTags := @RadioPlayerThreadsStreamGetTags;
    FRadioPlayerThreads[ThreadIndex].Start;
  end;
end;

procedure TRadioPlayer.LoadEqualizerConfig;
begin
  if FEqualizerConfig <> nil then
    FreeAndNil(FEqualizerConfig);

  FEqualizerConfig := TEqualizerConfig.Create(
    TTRPSettings.GetValue('Equalizer.Config.Bandwidth', 2.5, true),
    TTRPSettings.GetValue('Equalizer.Config.Band1.Center', 125, true),
    TTRPSettings.GetValue('Equalizer.Config.Band2.Center', 250, true),
    TTRPSettings.GetValue('Equalizer.Config.Band3.Center', 500, true),
    TTRPSettings.GetValue('Equalizer.Config.Band4.Center', 1000, true),
    TTRPSettings.GetValue('Equalizer.Config.Band5.Center', 2000, true),
    TTRPSettings.GetValue('Equalizer.Config.Band6.Center', 4000, true),
    TTRPSettings.GetValue('Equalizer.Config.Band7.Center', 8000, true),
    TTRPSettings.GetValue('Equalizer.Config.Band8.Center', 16000, true));
end;

end.

