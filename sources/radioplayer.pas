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
  { Bass } lazdynamic_bass, lazdynamic_bass_fx,
  fgl;

const
  MAX_PLAYER_THREADS = 3;

type
  TEqualizerPresets = specialize TFPGMap<string, TEqualizerPreset>;

type
  TCompressorPresets = specialize TFPGMap<string, TCompressorPreset>;

type
  TDampPresets = specialize TFPGMap<string, TDampPreset>;

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

    // Equalizer
    FEqualizerConfig: TEqualizerConfig;
    FEqualizerPresets: TEqualizerPresets;
    FCompressorPresets: TCompressorPresets;
    FDampPresets: TDampPresets;

    procedure Error(msg: string);
    procedure RadioInit;
    function LoadBassPlugins: Boolean;
    procedure RadioPlayerThreadsOnStreamPlaying(ASender: TObject; AThreadIndex: integer);
    procedure RadioPlayerThreadsStreamGetTags(ASender: TObject; AMessage:
      string; APlayerMessageType: TPlayerMessageType);
    procedure ThreadWatcherTimer(Sender: TObject);
    procedure TerminateThread(ThreadIndex: Integer; TerminateIfNotActive: Boolean);
    procedure CreateAndLaunchNewThread(ThreadIndex: Integer);

    // Equalizer
    procedure LoadEqualizerConfigAndPresets;
    procedure LoadPreset(APresetName: string; DefaultValues: array of integer);

    // Compressor
    procedure LoadCompressorConfigAndPressets;
    procedure LoadCompressorPreset(APresetName: string;
      AGain, AThreshold, ARatio, AAttack, ARelease: single);

    // DAMP
    procedure LoadDampConfigAndPressets;
    procedure LoadDampPreset(APresetName: string;
      ATarget, AQuiet, ARate, AGain, ADelay: single);
  public
    constructor Create; overload;
    destructor Destroy; override;

    procedure PlayURL(const AStreamUrl: string; const AVolume: ShortInt);
    procedure PlayStation(const StationId: integer; const Volume: ShortInt);
    function GetSelectedStationId(var VstStationsList: TVirtualStringTree): integer;

    function Stop(): Boolean;
    procedure Volume(Value: Integer);

    // Equalizer
    procedure EqualizerEnable;
    procedure EqualizerDisable;
    procedure UpdateEqualizerPreset(const APresetName: string;
      const ABandNumber: ShortInt; const AValue: integer);

    // Compressor
    procedure CompressorEnable;
    procedure CompressorDisable;
    procedure UpdateCompressorPreset(APresetName: string);

    // DAMP
    procedure DampEnable;
    procedure DampDisable;
    procedure UpdateDampPreset(APresetName: string);

    procedure UpdateSettings;

    function ChannelIsActiveAndPlaying: Boolean;
    function ChannelGetLevel: DWORD;
    function NumberOfRunningThreads: integer;

    function NoCurrentStationLoaded: Boolean;

    property OnRadioPlayerTags: TRadioPlayerTagsEvent
      read FOnRadioPlayerTags write FOnRadioPlayerTags;
    property OnRadioPlay: TNotifyEvent read FOnRadioPlay write FOnRadioPlay;

    property CurrentStationId: integer read FCurrentStationId;

    property EqualizerPresets: TEqualizerPresets read FEqualizerPresets;
    property EqualizerConfig: TEqualizerConfig read FEqualizerConfig;
    property CompressorPresets: TCompressorPresets read FCompressorPresets;
    property DampPresets: TDampPresets read FDampPresets;
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

  LoadEqualizerConfigAndPresets;
  LoadCompressorConfigAndPressets;
  LoadDampConfigAndPressets;

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
  presetsCount: integer;
begin
  UpdateSettings;

  FreeAndNil(FThreadWatcher);

  for i := Low(FRadioPlayerThreads) to High(FRadioPlayerThreads) do
  begin
    TerminateThread(i, false)
  end;

  if Assigned(FEqualizerConfig) then
    FreeAndNil(FEqualizerConfig);

  if Assigned(FEqualizerPresets) then
  begin
    presetsCount := FEqualizerPresets.Count - 1;
    for i := 0 to presetsCount do
      FEqualizerPresets.Data[i].Free;

    FreeAndNil(FEqualizerPresets);
  end;

  if Assigned(FCompressorPresets) then
  begin
    presetsCount := FCompressorPresets.Count - 1;
    for i := 0 to presetsCount do
      FCompressorPresets.Data[i].Free;

    FreeAndNil(FCompressorPresets);
  end;

  if Assigned(FDampPresets) then
  begin
    presetsCount := FDampPresets.Count - 1;
    for i := 0 to presetsCount do
      FDampPresets.Data[i].Free;

    FreeAndNil(FDampPresets);
  end;

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

  FRadioPlayerThreads[threadToPlayNextStream].PlayURL(AStreamUrl, AVolume,
    threadToPlayNextStream,
    FEqualizerPresets[FEqualizerConfig.DefaultPreset],
    FCompressorPresets[FEqualizerConfig.DefaultCompressorPreset]);
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

procedure TRadioPlayer.UpdateSettings;
var
  i: integer;
  presetsCount: integer;
begin
  // Update status
  TTRPSettings.SetGroupValue('Enabled', 'Equalizer.Config', FEqualizerConfig.Enabled);
  TTRPSettings.SetGroupValue('CompressorEnabled', 'Equalizer.Config', FEqualizerConfig.CompressorEnabled);
  TTRPSettings.SetGroupValue('DAmpEnabled', 'Equalizer.Config', FEqualizerConfig.DampEnabled);

  // Update default preset
  TTRPSettings.SetGroupValue('DefaultPreset', 'Equalizer.Config', FEqualizerConfig.DefaultPreset);
  TTRPSettings.SetGroupValue('DefaultCompressorPreset', 'Equalizer.Config', FEqualizerConfig.DefaultCompressorPreset);
  TTRPSettings.SetGroupValue('DefaultDAmpPreset', 'Equalizer.Config', FEqualizerConfig.DefaultDampPreset);

  // Update presets
  presetsCount := FEqualizerPresets.Count - 1;
  for i := 0 to presetsCount do
  begin
    TTRPSettings.SetGroupValue(FEqualizerPresets.Data[i].Name + '.Band1.Gain', 'Equalizer.Presets', FEqualizerPresets.Data[i].Band1Gain);
    TTRPSettings.SetGroupValue(FEqualizerPresets.Data[i].Name + '.Band2.Gain', 'Equalizer.Presets', FEqualizerPresets.Data[i].Band2Gain);
    TTRPSettings.SetGroupValue(FEqualizerPresets.Data[i].Name + '.Band3.Gain', 'Equalizer.Presets', FEqualizerPresets.Data[i].Band3Gain);
    TTRPSettings.SetGroupValue(FEqualizerPresets.Data[i].Name + '.Band4.Gain', 'Equalizer.Presets', FEqualizerPresets.Data[i].Band4Gain);
    TTRPSettings.SetGroupValue(FEqualizerPresets.Data[i].Name + '.Band5.Gain', 'Equalizer.Presets', FEqualizerPresets.Data[i].Band5Gain);
    TTRPSettings.SetGroupValue(FEqualizerPresets.Data[i].Name + '.Band6.Gain', 'Equalizer.Presets', FEqualizerPresets.Data[i].Band6Gain);
    TTRPSettings.SetGroupValue(FEqualizerPresets.Data[i].Name + '.Band7.Gain', 'Equalizer.Presets', FEqualizerPresets.Data[i].Band7Gain);
    TTRPSettings.SetGroupValue(FEqualizerPresets.Data[i].Name + '.Band8.Gain', 'Equalizer.Presets', FEqualizerPresets.Data[i].Band8Gain);
  end;
end;

{$REGION 'Equalizer'}

procedure TRadioPlayer.EqualizerEnable;
begin
  FEqualizerConfig.Enabled := true;

  if FRadioPlayerThreads[FActiveRadioPlayerThread] <> nil then
    FRadioPlayerThreads[FActiveRadioPlayerThread].EqualizerEnable(true);
end;

procedure TRadioPlayer.EqualizerDisable;
begin
  FEqualizerConfig.Enabled := false;

  if FRadioPlayerThreads[FActiveRadioPlayerThread] <> nil then
    FRadioPlayerThreads[FActiveRadioPlayerThread].EqualizerDisable();
end;

procedure TRadioPlayer.UpdateEqualizerPreset(const APresetName: string;
  const ABandNumber: ShortInt; const AValue: integer);
begin
  if (Trim(APresetName) = EMPTY_STR) or (ABandNumber < 1) or (ABandNumber > 8) then Exit;

  case ABandNumber of
    1: EqualizerPresets[APresetName].Band1Gain := AValue;
    2: EqualizerPresets[APresetName].Band2Gain := AValue;
    3: EqualizerPresets[APresetName].Band3Gain := AValue;
    4: EqualizerPresets[APresetName].Band4Gain := AValue;
    5: EqualizerPresets[APresetName].Band5Gain := AValue;
    6: EqualizerPresets[APresetName].Band6Gain := AValue;
    7: EqualizerPresets[APresetName].Band7Gain := AValue;
    8: EqualizerPresets[APresetName].Band8Gain := AValue;
  end;

  if FRadioPlayerThreads[FActiveRadioPlayerThread] <> nil then
    FRadioPlayerThreads[FActiveRadioPlayerThread].UpdateEQ(ABandNumber, AValue);
end;

procedure TRadioPlayer.LoadEqualizerConfigAndPresets;
begin
  if not Assigned(FEqualizerConfig) then
    FEqualizerConfig := TEqualizerConfig.Create;

  with FEqualizerConfig do
  begin
    Enabled := TTRPSettings.GetGroupValue('Enabled', 'Equalizer.Config', false, true);
    Bandwidth := TTRPSettings.GetGroupValue('Bandwidth', 'Equalizer.Config', 2.5, true);
    Band1Center := TTRPSettings.GetGroupValue('Band1.Center', 'Equalizer.Config', 125, true);
    Band2Center := TTRPSettings.GetGroupValue('Band2.Center', 'Equalizer.Config', 250, true);
    Band3Center := TTRPSettings.GetGroupValue('Band3.Center', 'Equalizer.Config', 500, true);
    Band4Center := TTRPSettings.GetGroupValue('Band4.Center', 'Equalizer.Config', 1000, true);
    Band5Center := TTRPSettings.GetGroupValue('Band5.Center', 'Equalizer.Config', 2000, true);
    Band6Center := TTRPSettings.GetGroupValue('Band6.Center', 'Equalizer.Config', 4000, true);
    Band7Center := TTRPSettings.GetGroupValue('Band7.Center', 'Equalizer.Config', 8000, true);
    Band8Center := TTRPSettings.GetGroupValue('Band8.Center', 'Equalizer.Config', 16000, true);
    DefaultPreset := TTRPSettings.GetGroupValue('DefaultPreset', 'Equalizer.Config', 'Default', true);
    CompressorEnabled := TTRPSettings.GetGroupValue('CompressorEnabled', 'Equalizer.Config', false, true);
    DefaultCompressorPreset := TTRPSettings.GetGroupValue('DefaultCompressorPreset', 'Equalizer.Config', 'Default', true);
    DampEnabled := TTRPSettings.GetGroupValue('DAmpEnabled', 'Equalizer.Config', false, true);
    DefaultDampPreset := TTRPSettings.GetGroupValue('DefaultDAmpPreset', 'Equalizer.Config', 'Default', true);
  end;

  if not Assigned(FEqualizerPresets) then
    FEqualizerPresets := TEqualizerPresets.Create;

  LoadPreset('Default', [ 0,  0,  0,  0,  0,  0,  0,  0]);
  LoadPreset('Custom1', [ 0,  0,  0,  0,  0,  0,  0,  0]);
  LoadPreset('Custom2', [-1,  0,  0,  0,  0,  0,  3,  5]);
  LoadPreset('Blues',   [ 2,  1,  0,  0,  0,  0, -1, -3]);
  LoadPreset('Classic', [ 6,  3,  0,  0,  0,  0,  2,  2]);
  LoadPreset('Country', [ 0,  2,  2,  0,  0,  0,  3,  3]);
  LoadPreset('Dance',   [ 5,  1, -1, -1,  0,  0,  4,  4]);
  LoadPreset('Jazz',    [ 0,  3,  3,  3,  0,  2,  4,  4]);
  LoadPreset('Metal',   [ 0,  0,  0,  0,  3,  0,  3,  1]);
  LoadPreset('NewAge',  [ 3,  0,  0,  0,  0,  0,  1,  1]);
  LoadPreset('Oldies',  [ 2,  1,  0,  0,  0,  0, -2, -5]);
  LoadPreset('Techno',  [ 4, -1, -1, -1,  0,  0,  5,  5]);
  LoadPreset('Rock',    [ 2,  3, -1, -1,  0,  0,  4,  4]);

end;

procedure TRadioPlayer.LoadPreset(APresetName: string; DefaultValues: array of integer);
var
  preset: TEqualizerPreset;
begin
  preset := TEqualizerPreset.Create;

  with preset do
  begin
    Name := APresetName;
    Band1Gain := TTRPSettings.GetGroupValue(APresetName + '.Band1.Gain', 'Equalizer.Presets', DefaultValues[0], true);
    Band2Gain := TTRPSettings.GetGroupValue(APresetName + '.Band2.Gain', 'Equalizer.Presets', DefaultValues[1], true);
    Band3Gain := TTRPSettings.GetGroupValue(APresetName + '.Band3.Gain', 'Equalizer.Presets', DefaultValues[2], true);
    Band4Gain := TTRPSettings.GetGroupValue(APresetName + '.Band4.Gain', 'Equalizer.Presets', DefaultValues[3], true);
    Band5Gain := TTRPSettings.GetGroupValue(APresetName + '.Band5.Gain', 'Equalizer.Presets', DefaultValues[4], true);
    Band6Gain := TTRPSettings.GetGroupValue(APresetName + '.Band6.Gain', 'Equalizer.Presets', DefaultValues[5], true);
    Band7Gain := TTRPSettings.GetGroupValue(APresetName + '.Band7.Gain', 'Equalizer.Presets', DefaultValues[6], true);
    Band8Gain := TTRPSettings.GetGroupValue(APresetName + '.Band8.Gain', 'Equalizer.Presets', DefaultValues[7], true);
  end;

  FEqualizerPresets.Add(APresetName, preset);
end;

{$ENDREGION}

{$REGION 'Compressor'}

procedure TRadioPlayer.CompressorEnable;
begin
  FEqualizerConfig.CompressorEnabled := true;

  if FRadioPlayerThreads[FActiveRadioPlayerThread] <> nil then
    FRadioPlayerThreads[FActiveRadioPlayerThread].CompressorEnable(true);
end;

procedure TRadioPlayer.CompressorDisable;
begin
  FEqualizerConfig.CompressorEnabled := false;

  if FRadioPlayerThreads[FActiveRadioPlayerThread] <> nil then
    FRadioPlayerThreads[FActiveRadioPlayerThread].CompressorDisable();
end;

procedure TRadioPlayer.UpdateCompressorPreset(APresetName: string);
begin
  EqualizerConfig.DefaultCompressorPreset := APresetName;

  if FRadioPlayerThreads[FActiveRadioPlayerThread] <> nil then
    FRadioPlayerThreads[FActiveRadioPlayerThread].UpdateCompressorPreset(
      FCompressorPresets[FEqualizerConfig.DefaultCompressorPreset]);
end;

procedure TRadioPlayer.LoadCompressorConfigAndPressets;
begin
  if not Assigned(FCompressorPresets) then
    FCompressorPresets := TCompressorPresets.Create;

  LoadCompressorPreset('Default', 5.0, -15.0, 3.0, 20.0, 200.0);
  LoadCompressorPreset('Soft', 3.7, -15.0, 2.0, 24.0, 800.0);
  LoadCompressorPreset('Medium', 7.5, -20.0, 4.0, 16.0, 500.0);
  LoadCompressorPreset('Hard', 10.0, -23.0, 8.0, 12.0, 400.0);
end;

procedure TRadioPlayer.LoadCompressorPreset(APresetName: string;
  AGain, AThreshold, ARatio, AAttack, ARelease: single);
var
  preset: TCompressorPreset;
begin
  preset := TCompressorPreset.Create;

  with preset do
  begin
    Name      := APresetName;
    Gain      := TTRPSettings.GetGroupValue(APresetName + '.Gain',      'Equalizer.Compressor.Presets', AGain,      true);
    Threshold := TTRPSettings.GetGroupValue(APresetName + '.Threshold', 'Equalizer.Compressor.Presets', AThreshold, true);
    Ratio     := TTRPSettings.GetGroupValue(APresetName + '.Ratio',     'Equalizer.Compressor.Presets', ARatio,     true);
    Attack    := TTRPSettings.GetGroupValue(APresetName + '.Attack',    'Equalizer.Compressor.Presets', AAttack,    true);
    Release   := TTRPSettings.GetGroupValue(APresetName + '.Release',   'Equalizer.Compressor.Presets', ARelease,   true);
  end;

  FCompressorPresets.Add(APresetName, preset);
end;

{$ENDREGION}

{$REGION 'DAMP'}

procedure TRadioPlayer.DampEnable;
begin
  FEqualizerConfig.DampEnabled := true;

  if FRadioPlayerThreads[FActiveRadioPlayerThread] <> nil then
    FRadioPlayerThreads[FActiveRadioPlayerThread].DampEnable(true);
end;

procedure TRadioPlayer.DampDisable;
begin
  FEqualizerConfig.DampEnabled := false;

  if FRadioPlayerThreads[FActiveRadioPlayerThread] <> nil then
    FRadioPlayerThreads[FActiveRadioPlayerThread].DampDisable();
end;

procedure TRadioPlayer.UpdateDampPreset(APresetName: string);
begin
  EqualizerConfig.DefaultDampPreset := APresetName;

  if FRadioPlayerThreads[FActiveRadioPlayerThread] <> nil then
    FRadioPlayerThreads[FActiveRadioPlayerThread].UpdateDampPreset(
      FDampPresets[FEqualizerConfig.DefaultDampPreset]);
end;

procedure TRadioPlayer.LoadDampConfigAndPressets;
begin
  if not Assigned(FDampPresets) then
    FDampPresets := TDampPresets.Create;

  LoadDampPreset('Soft', 0.92, 0.02, 0.01, 1.0, 0.5);
  LoadDampPreset('Medium', 0.94, 0.03, 0.01, 1.0, 0.35);
  LoadDampPreset('Hard', 0.98, 0.04, 0.02, 2.0, 0.2);
end;

procedure TRadioPlayer.LoadDampPreset(APresetName: string; ATarget, AQuiet,
  ARate, AGain, ADelay: single);
var
  preset: TDampPreset;
begin
  preset := TDampPreset.Create;

  with preset do
  begin
    Name   := APresetName;
    Target := TTRPSettings.GetGroupValue(APresetName + '.Target', 'Equalizer.DAmp.Presets', ATarget, true);
    Quiet  := TTRPSettings.GetGroupValue(APresetName + '.Quiet',  'Equalizer.DAmp.Presets', AQuiet,  true);
    Rate   := TTRPSettings.GetGroupValue(APresetName + '.Rate',   'Equalizer.DAmp.Presets', ARate,   true);
    Gain   := TTRPSettings.GetGroupValue(APresetName + '.Gain',   'Equalizer.DAmp.Presets', AGain,   true);
    Delay  := TTRPSettings.GetGroupValue(APresetName + '.Delay',  'Equalizer.DAmp.Presets', ADelay,  true);
  end;

  FDampPresets.Add(APresetName, preset);
end;

{$ENDREGION}

end.

