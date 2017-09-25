unit Utimelin;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
utimelin: Time line form. Displays (read-only) time line of year with frost dates
and 'normal' planting/harvest dates for cultivar selected in list box. 'Normal' dates
are based on parameters for the plant and the frost dates for the current climate.
User can use this window to decide when to plant various plants. Reaches right
into domain.garden.weather to use weather info; reaches into domain.templateManager
to get information on list of cultivars. This is ok because nothing is changed here.
No interactions between this window and anything else. Was thrown together quickly.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Buttons,
  ugsform;

type
  TtimelineForm = class(GsForm)
    timelinePanel: TPanel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    Bevel10: TBevel;
    Bevel11: TBevel;
    Bevel12: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    cultivarListBox: TListBox;
    Label13: TLabel;
    Close: TButton;
    helpButton: TBitBtn;
    Panel1: TPanel;
    frostNamesLabel: TLabel;
    Label16: TLabel;
    lastSpringFrostLabel: TLabel;
    firstFallFrostLabel: TLabel;
    plantingDateLabel: TLabel;
    germinationDateLabel: TLabel;
    harvestDateLabel: TLabel;
    germinationToHarvestPanel: TPanel;
    plantingToGerminationPanel: TPanel;
    extraPlantingToGerminationPanel: TPanel;
    extraGerminationToHarvestPanel: TPanel;
    seedlingOptimalGrowthDateLabel: TLabel;
    weatherNameLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure cultivarListBoxClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  udomain, uestruct, ueplant, ueweath;

{$R *.DFM}

const
  kEndGap = 10;

procedure TtimelineForm.FormCreate(Sender: TObject);
  var
    i: longint;
    cultivar: GsPlant;
  begin
  { this will make list box create horizontal scroll bar }
  sendMessage(cultivarListBox.handle, LB_SETHORIZONTALEXTENT, 500, longint(0));
  if (domain <> nil) and (domain.templateManager <> nil)
    and (domain.templateManager.cultivarList <> nil) then
    begin
    if domain.templateManager.cultivarList.count > 0 then
    for i := 0 to domain.templateManager.cultivarList.count - 1 do
      begin
      cultivar := GsPlant(domain.templateManager.cultivarList.items[i]);
      if cultivar = nil then continue;
      cultivarListBox.items.addObject(cultivar.getName, cultivar);
      end;
    end
  else
    raise Exception.create('Problem opening timeline window');
  if (domain <> nil) and (domain.garden <> nil)
    and (domain.garden.weather <> nil) then
    begin
    lastSpringFrostLabel.left := kEndGap + domain.garden.weather.stationVars.julianDayOfLastSpringFrost;
    firstFallFrostLabel.left := kEndGap + domain.garden.weather.stationVars.julianDayOfFirstFallFrost;
    if lastSpringFrostLabel.left < kEndGap then
      begin
      lastSpringFrostLabel.visible := false;
      frostNamesLabel.caption := 'LSF = (no spring frost in this climate), FFF = first fall frost';
      end;
    if firstFallFrostLabel.left < kEndGap then
      begin
      firstFallFrostLabel.visible := false;
      frostNamesLabel.caption := 'LSF = last spring frost, FFF = (no fall frost in this climate)';
      end;
    if (lastSpringFrostLabel.left < kEndGap) and (firstFallFrostLabel.left < kEndGap) then
      frostNamesLabel.caption := '(no spring or fall frosts in this climate)';
    weatherNameLabel.caption := domain.garden.weather.getName;
    end
  else
    raise Exception.create('Problem opening timeline window');
  plantingToGerminationPanel.visible := false;
  germinationToHarvestPanel.visible := false;
  extraPlantingToGerminationPanel.visible := false;
  extraGerminationToHarvestPanel.visible := false;
  plantingDateLabel.visible := false;
  germinationDateLabel.visible := false;
  harvestDateLabel.visible := false;
  seedlingOptimalGrowthDateLabel.visible := false;
  end;

procedure TtimelineForm.cultivarListBoxClick(Sender: TObject);
  var
    useSpring: boolean;
    dayOfLastSpringFrost, dayOfFirstFallFrost, distanceFromSpringFrost, distanceFromFallFrost,
      startDay, dayGerminationIsComplete, dayOfMaturity, julianDayToday, dayOfSeedlingAutomaticGrowth: smallint;
    cultivar: GsPlant;
  begin
  if cultivarListBox.itemIndex < 0 then exit;
  cultivar := GsPlant(cultivarListBox.items.objects[cultivarListBox.itemIndex]);
  if cultivar = nil then exit;
  if (domain <> nil) and (domain.garden <> nil) then
    julianDayToday := domain.garden.julianDayToday
  else
    julianDayToday := 0;
  dayOfLastSpringFrost := lastSpringFrostLabel.left - kEndGap;
  dayOfFirstFallFrost := firstFallFrostLabel.left - kEndGap;
  case cultivar.params.plantingSpringFallOrBoth of
    kCultivarIsPlantedInSpring: useSpring := true;
    kCultivarIsPlantedInFall: useSpring := false;
    kCultivarCanBePlantedSpringOrFall:
      begin
      if dayOfLastSpringFrost < 0 then
        distanceFromSpringFrost := 365 {to make use fall}
      else
        distanceFromSpringFrost := julianDayToday - dayOfLastSpringFrost;
      if dayOfFirstFallFrost < 0 then
        distanceFromFallFrost := 365
      else
        distanceFromFallFrost := dayOfFirstFallFrost - julianDayToday;
      useSpring := (distanceFromSpringFrost < distanceFromFallFrost);
      end;
    else
      raise Exception.create('Problem: bad planting index');
    end;
  if useSpring then
    begin
    if dayOfLastSpringFrost >= 0 then
      startDay := dayOfLastSpringFrost + cultivar.params.plantingDaysAfterLastSpringFrost_days
    else
      startDay := julianDayToday;
    end
  else
    begin
    if dayOfFirstFallFrost >= 0 then
      startDay := dayOfFirstFallFrost - cultivar.params.maturityDaysBeforeFirstFallFrost_days
          - cultivar.params.plantingDaysFromGerminatedSeedToSeasonMaturity_days
          - cultivar.params.plantingDaysFromSeedToGerminatedSeed_days
    else
      startDay := julianDayToday;
    end;
  if startDay < 0 then startDay := startDay + 365;
  startDay := startDay mod 364;
  dayGerminationIsComplete := startDay + cultivar.params.plantingDaysFromSeedToGerminatedSeed_days;
  if dayGerminationIsComplete < 0 then dayGerminationIsComplete := dayGerminationIsComplete + 365;
  dayGerminationIsComplete := dayGerminationIsComplete mod 364;
  dayOfMaturity := startDay + cultivar.params.plantingDaysFromSeedToGerminatedSeed_days
      + cultivar.params.plantingDaysFromGerminatedSeedToSeasonMaturity_days;
  if dayOfMaturity < 0 then dayOfMaturity := dayOfMaturity + 365;
  dayOfMaturity := dayOfMaturity mod 364;
  dayOfSeedlingAutomaticGrowth := startDay + cultivar.params.daysToGrowOptimallyAfterPlanting_days;
  if dayOfSeedlingAutomaticGrowth < 0 then dayOfSeedlingAutomaticGrowth := dayOfMaturity + 365;
  dayOfSeedlingAutomaticGrowth := dayOfSeedlingAutomaticGrowth mod 364;
  { place panels }
  with plantingToGerminationPanel do
    setBounds(kEndGap + startDay, top, abs(dayGerminationIsComplete - startDay), height);
  if plantingToGerminationPanel.left + plantingToGerminationPanel.width > kEndGap + 364 then
    begin
    with extraPlantingToGerminationPanel do
      setBounds(kEndGap, plantingToGerminationPanel.top, dayGerminationIsComplete, height);
    plantingToGerminationPanel.width := kEndGap + 364 - plantingToGerminationPanel.left;
    extraPlantingToGerminationPanel.visible := true;
    end
  else
    extraPlantingToGerminationPanel.visible := false;
  with germinationToHarvestPanel do
    setBounds(kEndGap + dayGerminationIsComplete, top, abs(dayOfMaturity - dayGerminationIsComplete), height);
  if germinationToHarvestPanel.left + germinationToHarvestPanel.width > kEndGap + 364 then
    begin
    with extraGerminationToHarvestPanel do
      setBounds(kEndGap, germinationToHarvestPanel.top, dayOfMaturity, height);
    germinationToHarvestPanel.width := kEndGap + 364 - germinationToHarvestPanel.left;
    extraGerminationToHarvestPanel.visible := true;
    end
  else
    extraGerminationToHarvestPanel.visible := false;
  plantingDateLabel.left := kEndGap + startDay - plantingDateLabel.width div 2;
  germinationDateLabel.left := kEndGap + dayGerminationIsComplete - germinationDateLabel.width div 2;
  harvestDateLabel.left := kEndGap + dayOfMaturity - harvestDateLabel.width div 2;
  plantingToGerminationPanel.visible := true;
  germinationToHarvestPanel.visible := true;
  plantingDateLabel.visible := true;
  germinationDateLabel.visible := true;
  harvestDateLabel.visible := true;
  if dayOfSeedlingAutomaticGrowth <> startDay then
    begin
    seedlingOptimalGrowthDateLabel.visible := true;
    seedlingOptimalGrowthDateLabel.left := kEndGap + dayOfSeedlingAutomaticGrowth;
    end
  else
    seedlingOptimalGrowthDateLabel.visible := false;
  end;

procedure TtimelineForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Time_line_window')
  end;

end.
