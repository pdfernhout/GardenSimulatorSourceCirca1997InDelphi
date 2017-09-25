unit Usimop;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
usimop: Simulation options form. Many options, but most are fairly simple 3-state
check boxes. Loading and saving are directly from garden (domain.garden) using simple
transfer functions for all elements. There are some interactions between some options;
for example, if runoff is turned off, water erosion from runoff is automatically
disabled. And the user can get to the templates window from here to add another
climate or soil type from another template file, and this form must update its
list correctly. Otherwise the window is straightforward though large. }

interface

uses
  WinProcs, WinTypes, SysUtils, Messages, Classes, Graphics, Controls, 
  Forms, Dialogs, StdCtrls, utempl, ueweath, uesoil, Buttons, ExtCtrls, ugsform;

type
  TSimulationOptionsForm = class(GsForm)
    weatherPanel: TGroupBox;
    plantsPanel: TGroupBox;
    OptimalRadiation: TCheckBox;
    OptimalWaterUptake: TCheckBox;
    OptimalTemperature: TCheckBox;
    OptimalNitrogenUptake: TCheckBox;
    OptimalPhosphorusUptake: TCheckBox;
    OptimalSoilStrengthForRootGrowth: TCheckBox;
    IgnoreLeafSenescence: TCheckBox;
    IgnoreAluminumToxicity: TCheckBox;
    IgnoreAerationStress: TCheckBox;
    IgnorePhotoperiod: TCheckBox;
    Cancel: TButton;
    OK: TButton;
    ignoreGerminationRequirements: TCheckBox;
    helpButton: TBitBtn;
    EverythingGrows: TButton;
    Beginner: TButton;
    Advanced: TButton;
    Expert: TButton;
    soilPatchGeneralPanel: TGroupBox;
    Label2: TLabel;
    ClimateList: TComboBox;
    LoadNewClimate: TButton;
    Label3: TLabel;
    weatherTempOption: TComboBox;
    Label4: TLabel;
    weatherPrecipOption: TComboBox;
    Label5: TLabel;
    weatherRadiationOption: TComboBox;
    Label6: TLabel;
    weatherRelHumOption: TComboBox;
    Label7: TLabel;
    weatherWindSpeedOption: TComboBox;
    Label8: TLabel;
    weatherWindDirectionOption: TComboBox;
    updateSoilCoverIndex: TCheckBox;
    updateAlbedo: TCheckBox;
    updateSoilTemperature: TCheckBox;
    updateWaterTableDepth: TCheckBox;
    Label10: TLabel;
    allowSnowMelt: TCheckBox;
    allowPrecipitationToReachPatch: TCheckBox;
    allowPercolation: TCheckBox;
    allowLateralFlow: TCheckBox;
    allowSoilEvaporation: TCheckBox;
    allowPlantTranspiration: TCheckBox;
    Label11: TLabel;
    allowRunoff: TCheckBox;
    allowRunoffFromIrrigation: TCheckBox;
    allowWaterErosionFromRunoff: TCheckBox;
    allowWaterErosionFromRunoffFromIrrigation: TCheckBox;
    allowWindErosion: TCheckBox;
    allowRidgeSettlingFromRainfall: TCheckBox;
    soilPatchNitrogenPanel: TGroupBox;
    Label12: TLabel;
    allowNitrateToEnterSoilInRainfall: TCheckBox;
    allowNitrateToEnterSoilInIrrigationWater: TCheckBox;
    Label13: TLabel;
    allowActiveAndStableHumusNFlow: TCheckBox;
    allowNitrification: TCheckBox;
    allowVolatilization: TCheckBox;
    allowActiveHumusNMineralization: TCheckBox;
    allowFreshNMineralization: TCheckBox;
    allowDenitrification: TCheckBox;
    Label14: TLabel;
    allowNUptakeByPlants: TCheckBox;
    allowNitrateLossInRunoff: TCheckBox;
    allowNitrateToMoveToTopSoilLayerByEvaporation: TCheckBox;
    allowNitrateLossInPercolation: TCheckBox;
    allowNitrateLossInLateralFlow: TCheckBox;
    allowErodedSoilToTakeOrganicNActiveHumus: TCheckBox;
    allowErodedSoilToTakeOrganicNStableHumus: TCheckBox;
    allowErodedSoilToTakeNitrate: TCheckBox;
    allowErodedSoilToTakeAmmonia: TCheckBox;
    allowErodedSoilToTakeOrganicNFresh: TCheckBox;
    soilTypeList: TComboBox;
    LoadNewSoilType: TButton;
    FindSoilType: TButton;
    Label9: TLabel;
    soilPatchPhosphorusPanel: TGroupBox;
    Label15: TLabel;
    allowLabileAndMineralPFlow: TCheckBox;
    allowActiveAndStableMineralPFlow: TCheckBox;
    allowActiveHumusPMineralization: TCheckBox;
    allowFreshPMineralization: TCheckBox;
    allowPUptakeByPlants: TCheckBox;
    allowErodedSoilToTakeOrganicPHumus: TCheckBox;
    allowErodedSoilToTakeLabileP: TCheckBox;
    allowErodedSoilToTakeMineralPActive: TCheckBox;
    allowErodedSoilToTakeMineralPStable: TCheckBox;
    allowErodedSoilToTakeOrganicPFresh: TCheckBox;
    Label16: TLabel;
    allowStandingDeadToDecayToResidue: TCheckBox;
    allowErodedSoilToTakeOrganicMatter: TCheckBox;
    allowErodedSoilToTakeFlatCropResidue: TCheckBox;
    Label17: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label34: TLabel;
    allowLabilePLossInRunoff: TCheckBox;
    allowLabilePLossInPercolation: TCheckBox;
    allowSoilSettlingFromRain: TCheckBox;
    Panel1: TPanel;
    Label19: TLabel;
    Label21: TLabel;
    Label20: TLabel;
    Label23: TLabel;
    Label22: TLabel;
    Label18: TLabel;
    Panel2: TPanel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    keepCurrentWeather: TRadioButton;
    chooseNewClimateForWeather: TRadioButton;
    soilPatchAutoPanel: TGroupBox;
    Label36: TLabel;
    AutoIrrigation: TCheckBox;
    AutoFertilization: TCheckBox;
    AutoPHControl: TCheckBox;
    AutoHarvest: TCheckBox;
    AutoMaintainFurrowDikes: TCheckBox;
    AutoMaintainDrainageSystem: TCheckBox;
    tabsPanel: TPanel;
    showWeather: TButton;
    showSoilPatchGeneral: TButton;
    showSoilPatchAuto: TButton;
    showNitrogen: TButton;
    showPhosphorus: TButton;
    showPlants: TButton;
    noteLabel: TLabel;
    allOn: TButton;
    allOff: TButton;
    allNotOverride: TButton;
    ignoreReproductiveDecay: TCheckBox;
    ignoreLifeCycleLimits: TCheckBox;
    Panel3: TPanel;
    Label1: TLabel;
    Label32: TLabel;
    allowFlatCropResidueToDecay: TCheckBox;
    procedure FormActivate(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure EverythingGrowsClick(Sender: TObject);
    procedure BeginnerClick(Sender: TObject);
    procedure AdvancedClick(Sender: TObject);
    procedure ExpertClick(Sender: TObject);
    procedure LoadNewClimateClick(Sender: TObject);
    procedure LoadNewSoilTypeClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure showWeatherClick(Sender: TObject);
    procedure showSoilPatchGeneralClick(Sender: TObject);
    procedure showNitrogenClick(Sender: TObject);
    procedure showPhosphorusClick(Sender: TObject);
    procedure showPlantsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure allowPercolationClick(Sender: TObject);
    procedure allowLateralFlowClick(Sender: TObject);
    procedure allowSoilEvaporationClick(Sender: TObject);
    procedure allowRunoffClick(Sender: TObject);
    procedure allowRunoffFromIrrigationClick(Sender: TObject);
    procedure allowWaterErosionFromRunoffClick(Sender: TObject);
    procedure allowWaterErosionFromRunoffFromIrrigationClick(
      Sender: TObject);
    procedure allowWindErosionClick(Sender: TObject);
    procedure keepCurrentWeatherClick(Sender: TObject);
    procedure chooseNewClimateForWeatherClick(Sender: TObject);
    procedure allOnClick(Sender: TObject);
    procedure allOffClick(Sender: TObject);
    procedure showSoilPatchAutoClick(Sender: TObject);
    procedure allNotOverrideClick(Sender: TObject);
    procedure allowActiveHumusNMineralizationClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    transferDirection: integer;
    panelShowing: smallint;
		procedure transferFields;
    procedure transfer3StateCheckBox(checkBox: TCheckBox; var overriding: boolean;
      var overrideValue: boolean);
    procedure transferSoil3StateCheckBox(checkBox: TCheckBox; index: smallint);
    procedure transferPlant3StateCheckBox(checkBox: TCheckBox; index: smallint);
    procedure transferComboBox(comboBox: TComboBox; var value: smallint);
		procedure updateClimate;
		procedure updateSoilType;
    procedure NutrientsInRunoffDependOnRunoff;
    procedure NutrientsInSedimentDependOnErosion;
    procedure SetAllCheckboxesInPanel(panel: TGroupBox; check: boolean);
    procedure grayAllCheckboxesInPanel(panel: TGroupBox);
    procedure showOnePanel(panelIndex: smallint);
    function panelForIndex(panelIndex: smallint): TGroupBox;
    procedure setAllCheckboxesAndComboBoxesInPanel(panel: TGroupBox; check: boolean);
    procedure allOnOrOff(allOn: boolean);
  end;

implementation

uses udomain, uegarden, uestruct, ubrowser, ugraph;

{$R *.DFM}

const
  kLoadIntoCheckBoxes = 1;
  kLoadFromCheckBoxes = 2;

  kWeatherPanel = 1;
  kSoilPatchAutoPanel = 2;
  kSoilPatchGeneralPanel = 3;
  kSoilPatchNitrogenPanel = 4;
  kSoilPatchPhosphorusPanel = 5;
  kPlantsPanel = 6;
  kLastPanel = 6;

procedure TSimulationOptionsForm.FormCreate(Sender: TObject);
  begin
  { this is here because the form is bigger during design so you can see all the panels,
    but it should not be that large at run time }
  self.width := 600;
  self.height := 400;
  end;

procedure TSimulationOptionsForm.FormActivate(Sender: TObject);
	begin
  self.showOnePanel(kWeatherPanel);
  keepCurrentWeather.checked := true;
  if (Domain <> nil) and (Domain.garden <> nil) and (Domain.garden.weather <> nil) then
    keepCurrentWeather.caption := 'Keep current weather (' + Domain.garden.weather.getName + ')';
  transferDirection := kLoadIntoCheckBoxes;
	self.transferFields;
  self.updateClimate;
  self.updateSoilType;
	end;

procedure TSimulationOptionsForm.transferFields;
  begin
  if domain.garden = nil then raise Exception.create('Problem: nil garden');
  if domain.garden.weather = nil then raise Exception.create('Problem: nil weather');
  with domain.garden do
  begin
  { weather - cannot change overrides (always override) }
  transferComboBox(weatherTempOption, weatherOptions[kWeatherOptionsTemperature]);
  transferComboBox(weatherPrecipOption, weatherOptions[kWeatherOptionsRainfall]);
  transferComboBox(weatherRadiationOption, weatherOptions[kWeatherOptionsRadiation]);
  transferComboBox(weatherRelHumOption, weatherOptions[kWeatherOptionsRelHum]);
  transferComboBox(weatherWindSpeedOption, weatherOptions[kWeatherOptionsWindSpeed]);
  transferComboBox(weatherWindDirectionOption, weatherOptions[kWeatherOptionsWindDirection]);
  { soil auto operations }
  transferSoil3StateCheckBox(autoIrrigation, kSoilOptionsAutoIrrigation);
  transferSoil3StateCheckBox(autoFertilization, kSoilOptionsAutoFertilization);
  transferSoil3StateCheckBox(autoPHControl, kSoilOptionsAutoPHControl);
  transferSoil3StateCheckBox(autoHarvest, kSoilOptionsAutoHarvest);
  transferSoil3StateCheckBox(autoMaintainFurrowDikes, kSoilOptionsAutoMaintainFurrowDikes);
  transferSoil3StateCheckBox(autoMaintainDrainageSystem, kSoilOptionsAutoMaintainDrainageSystem);
  { soil general }
  transferSoil3StateCheckBox(updateSoilCoverIndex, kSoilOptionsUpdateSoilCoverIndex);
  transferSoil3StateCheckBox(updateAlbedo, kSoilOptionsUpdateAlbedo);
  transferSoil3StateCheckBox(updateSoilTemperature, kSoilOptionsUpdateSoilTemperature);
  transferSoil3StateCheckBox(updateWaterTableDepth, kSoilOptionsUpdateWaterTableDepth);
  transferSoil3StateCheckBox(allowWindErosion, kSoilOptionsAllowWindErosion);
  transferSoil3StateCheckBox(allowSnowMelt, kSoilOptionsAllowSnowMelt);
  transferSoil3StateCheckBox(allowPrecipitationToReachPatch, kSoilOptionsAllowPrecipitationToReachPatch);
  transferSoil3StateCheckBox(allowRunoff, kSoilOptionsAllowRunoff);
  transferSoil3StateCheckBox(allowWaterErosionFromRunoff, kSoilOptionsAllowWaterErosionFromRunoff);
  transferSoil3StateCheckBox(allowRidgeSettlingFromRainfall, kSoilOptionsAllowRidgeSettlingFromRainfall);
  transferSoil3StateCheckBox(allowNitrateToEnterSoilInRainfall, kSoilOptionsAllowNitrateToEnterSoilInRainfall);
  transferSoil3StateCheckBox(allowNitrateToEnterSoilInIrrigationWater, kSoilOptionsAllowNitrateToEnterSoilInIrrigationWater);
  transferSoil3StateCheckBox(allowRunoffFromIrrigation, kSoilOptionsAllowRunoffFromIrrigation);
  transferSoil3StateCheckBox(allowWaterErosionFromRunoffFromIrrigation, kSoilOptionsAllowWaterErosionFromRunoffFromIrrigation);
  transferSoil3StateCheckBox(allowPercolation, kSoilOptionsAllowPercolation);
  transferSoil3StateCheckBox(allowLateralFlow, kSoilOptionsAllowLateralFlow);
  transferSoil3StateCheckBox(allowSoilEvaporation, kSoilOptionsAllowSoilEvaporation);
  transferSoil3StateCheckBox(allowPlantTranspiration, kSoilOptionsAllowPlantTranspiration);
  {transferSoil3StateCheckBox(allowRebuildingDikes, kSoilOptionsAllowRebuildingDikes);} {not using}
  transferSoil3StateCheckBox(allowActiveAndStableHumusNFlow, kSoilOptionsAllowActiveAndStableHumusNFlow);
  transferSoil3StateCheckBox(allowLabileAndMineralPFlow, kSoilOptionsAllowLabileAndMineralPFlow);
  transferSoil3StateCheckBox(allowActiveAndStableMineralPFlow, kSoilOptionsAllowActiveAndStableMineralPFlow);
  transferSoil3StateCheckBox(allowNitrateLossInRunoff, kSoilOptionsAllowNitrateLossInRunoff);
  transferSoil3StateCheckBox(allowLabilePLossInRunoff, kSoilOptionsAllowLabilePLossInRunoff);
  transferSoil3StateCheckBox(allowNitrateLossInLateralFlow, kSoilOptionsAllowNitrateLossInLateralFlow);
  transferSoil3StateCheckBox(allowNitrateLossInPercolation, kSoilOptionsAllowNitrateLossInPercolation);
  transferSoil3StateCheckBox(allowLabilePLossInPercolation, kSoilOptionsAllowLabilePLossInPercolation);
  transferSoil3StateCheckBox(allowNitrification, kSoilOptionsAllowNitrification);
  transferSoil3StateCheckBox(allowVolatilization, kSoilOptionsAllowVolatilization);
  transferSoil3StateCheckBox(allowActiveHumusNMineralization, kSoilOptionsAllowActiveHumusNMineralization);
  transferSoil3StateCheckBox(allowActiveHumusPMineralization, kSoilOptionsAllowActiveHumusPMineralization);
  transferSoil3StateCheckBox(allowFreshNMineralization, kSoilOptionsAllowFreshNMineralization);
  transferSoil3StateCheckBox(allowFreshPMineralization, kSoilOptionsAllowFreshPMineralization);
  transferSoil3StateCheckBox(allowStandingDeadToDecayToResidue, kSoilOptionsAllowStandingDeadToDecayToResidue);
  transferSoil3StateCheckBox(allowFlatCropResidueToDecay, kSoilOptionsAllowFlatCropResidueToDecay);
  transferSoil3StateCheckBox(allowDenitrification, kSoilOptionsAllowDenitrification);
  transferSoil3StateCheckBox(allowNUptakeByPlants, kSoilOptionsAllowNUptakeByPlants);
  transferSoil3StateCheckBox(allowPUptakeByPlants, kSoilOptionsAllowPUptakeByPlants);
  transferSoil3StateCheckBox(allowErodedSoilToTakeOrganicNActiveHumus, kSoilOptionsAllowErodedSoilToTakeOrganicNActiveHumus);
  transferSoil3StateCheckBox(allowErodedSoilToTakeOrganicNStableHumus, kSoilOptionsAllowErodedSoilToTakeOrganicNStableHumus);
  transferSoil3StateCheckBox(allowErodedSoilToTakeNitrate, kSoilOptionsAllowErodedSoilToTakeNitrate);
  transferSoil3StateCheckBox(allowErodedSoilToTakeAmmonia, kSoilOptionsAllowErodedSoilToTakeAmmonia);
  transferSoil3StateCheckBox(allowErodedSoilToTakeOrganicNFresh, kSoilOptionsAllowErodedSoilToTakeOrganicNFresh);
  transferSoil3StateCheckBox(allowErodedSoilToTakeOrganicPHumus, kSoilOptionsAllowErodedSoilToTakeOrganicPHumus);
  transferSoil3StateCheckBox(allowErodedSoilToTakeLabileP, kSoilOptionsAllowErodedSoilToTakeLabileP);
  transferSoil3StateCheckBox(allowErodedSoilToTakeMineralPActive, kSoilOptionsAllowErodedSoilToTakeMineralPActive);
  transferSoil3StateCheckBox(allowErodedSoilToTakeMineralPStable, kSoilOptionsAllowErodedSoilToTakeMineralPStable);
  transferSoil3StateCheckBox(allowErodedSoilToTakeOrganicPFresh, kSoilOptionsAllowErodedSoilToTakeOrganicPFresh);
  transferSoil3StateCheckBox(allowErodedSoilToTakeOrganicMatter, kSoilOptionsAllowErodedSoilToTakeOrganicMatter);
  transferSoil3StateCheckBox(allowErodedSoilToTakeFlatCropResidue, kSoilOptionsAllowErodedSoilToTakeFlatCropResidue);
  transferSoil3StateCheckBox(allowSoilSettlingFromRain, kSoilOptionsAllowSoilSettlingFromRain);
  transferSoil3StateCheckBox(allowNitrateToMoveToTopSoilLayerByEvaporation,
      kSoilOptionsAllowNitrateToMoveToTopSoilLayerByEvaporation);
  { plant }
  transferPlant3StateCheckBox(optimalRadiation, kPlantOptionsOptimalRadiation);
  transferPlant3StateCheckBox(optimalWaterUptake, kPlantOptionsOptimalWaterUptake);
  transferPlant3StateCheckBox(optimalTemperature, kPlantOptionsOptimalTemperature);
  transferPlant3StateCheckBox(optimalNitrogenUptake, kPlantOptionsOptimalNitrogenUptake);
  transferPlant3StateCheckBox(optimalPhosphorusUptake, kPlantOptionsOptimalPhosphorusUptake);
  transferPlant3StateCheckBox(optimalSoilStrengthForRootGrowth, kPlantOptionsOptimalSoilStrengthForRootGrowth);
  transferPlant3StateCheckBox(ignoreGerminationRequirements, kPlantOptionsIgnoreGerminationRequirements);
  transferPlant3StateCheckBox(ignoreLeafSenescence, kPlantOptionsIgnoreLeafSenescence);
  transferPlant3StateCheckBox(ignoreAluminumToxicity, kPlantOptionsIgnoreAluminumToxicity);
  transferPlant3StateCheckBox(ignoreAerationStress, kPlantOptionsIgnoreAerationStress);
  transferPlant3StateCheckBox(ignorePhotoperiod, kPlantOptionsIgnorePhotoperiod);
  transferPlant3StateCheckBox(ignoreReproductiveDecay, kPlantOptionsIgnoreReproductiveDecay);
  transferPlant3StateCheckBox(ignoreLifeCycleLimits, kPlantOptionsIgnoreLifeCycleLimits);
  end;
  end;

procedure TSimulationOptionsForm.transferSoil3StateCheckBox(checkBox: TCheckBox; index: smallint);
  begin
  { assumes domain and garden pointers are correct }
  with domain.garden do
    self.transfer3StateCheckBox(checkBox, soilOverrides[index], soilOptions[index]);
  end;

procedure TSimulationOptionsForm.transferPlant3StateCheckBox(checkBox: TCheckBox; index: smallint);
  begin
  { assumes domain and garden pointers are correct }
  with domain.garden do
    self.transfer3StateCheckBox(checkBox, plantOverrides[index], plantOptions[index]);
  end;

procedure TSimulationOptionsForm.transfer3StateCheckBox(checkBox: TCheckBox; var overriding: boolean;
    var overrideValue: boolean);
  begin
  if transferDirection = kLoadIntoCheckBoxes then
    begin
    if not overriding then
      checkBox.state := cbGrayed
    else
      checkBox.checked := overrideValue;
    end
  else if transferDirection = kLoadFromCheckBoxes then
    begin
    overriding := (checkBox.state <> cbGrayed);
    if overriding then
      overrideValue := checkBox.checked;
    end
  else
  	raise exception.create('TSimulationOptionsForm: invalid transfer option');
  end;

procedure TSimulationOptionsForm.transferComboBox(comboBox: TComboBox; var value: smallint);
  begin
  if transferDirection = kLoadIntoCheckBoxes then
    comboBox.itemIndex := value
  else if transferDirection = kLoadFromCheckBoxes then
    value := comboBox.itemIndex
  else
  	raise exception.create('TSimulationOptionsForm: invalid transfer option');
  end;

procedure TSimulationOptionsForm.OKClick(Sender: TObject);
  var
    newClimate: GsWeather;
    newSoilType: GsSoilPatch;
	begin
  if chooseNewClimateForWeather.checked then
    begin
    if (climateList.items.count > 0) and (climateList.itemIndex >= 0) then
      newClimate := GsWeather(climateList.items.objects[climateList.itemIndex])
    else
      newClimate := nil;
    if (newClimate <> nil) then
      begin
      {need to change climate - loses any changes}
      if messageDlg('Changing the weather to use the climate' + chr(13)
          + '     ' + newClimate.getName + chr(13)
          + 'will lose all changes you have made to the current weather.' + chr(13) + chr(13) 
          + 'Proceed?',
          mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
        begin
        modalResult := 0;
        exit;
        end;
      Domain.garden.weather.copyFromClimate(newClimate);
      Domain.templateManager.defaultClimateIndex := Domain.templateManager.climateList.indexOf(newClimate);
      { update browser and grapher if climate changed }
      BrowserForm.updateForNewDomain; 
      GraphForm.loggedVars.invalidate;
		  end;
    end;
  transferDirection := kLoadFromCheckBoxes;
	self.transferFields;
  if (soilTypeList.items.count > 0) and (soilTypeList.itemIndex >= 0) then
    newSoilType := GsSoilPatch(soilTypeList.items.objects[soilTypeList.itemIndex])
  else
    newSoilType := nil;
  Domain.templateManager.defaultSoilTypeIndex := Domain.templateManager.soilTypeList.indexOf(newSoilType);
  end;

procedure TSimulationOptionsForm.EverythingGrowsClick(Sender: TObject);
  begin
  { all options on }
  SetAllCheckboxesInPanel(plantsPanel, true);
  SetAllCheckboxesInPanel(soilPatchAutoPanel, true);
  end;

procedure TSimulationOptionsForm.BeginnerClick(Sender: TObject);
  begin
	{ all auto on }
  SetAllCheckboxesInPanel(soilPatchAutoPanel, true);
  { plant - all on but radiation, water, temperature, germination, photoperiod }
  SetAllCheckboxesInPanel(plantsPanel, true);
  optimalRadiation.checked := false;
  optimalWaterUptake.checked := false;
  optimalTemperature.checked := false;
  ignoreGerminationRequirements.checked := false;
  ignorePhotoperiod.checked := false;
  end;

procedure TSimulationOptionsForm.AdvancedClick(Sender: TObject);
  begin
	{ all auto off }
  SetAllCheckboxesInPanel(soilPatchAutoPanel, false);
  { plant - all off but P, soil strength, leaf senescence, al toxicity }
  SetAllCheckboxesInPanel(plantsPanel, false);
  optimalPhosphorusUptake.checked := true;
  optimalSoilStrengthForRootGrowth.checked := true;
  ignoreLeafSenescence.checked := true;
  ignoreAluminumToxicity.checked := true;
  ignoreAerationStress.checked := true;
  end;

procedure TSimulationOptionsForm.ExpertClick(Sender: TObject);
  begin
  { all options off }
  SetAllCheckboxesInPanel(plantsPanel, false);
  SetAllCheckboxesInPanel(soilPatchAutoPanel, false);
  end;

procedure TSimulationOptionsForm.LoadNewClimateClick(Sender: TObject);
  var
    templatesForm: TTemplatesForm;
    response: integer;
    oldFileName: string;
  begin
  oldFileName := Domain.fileOptions.templatesFileName;
  templatesForm := TTemplatesForm.create(self);
  if templatesForm = nil then
    raise Exception.create('Could not create templates window');
  try
    response := templatesForm.showModal;
    if response = mrOK then
      begin
      self.updateClimate;
      self.updateSoilType;
      end
    else
      Domain.fileOptions.templatesFileName := oldFileName;
  finally
    templatesForm.free;
    templatesForm := nil;
  end;
  end;

procedure TSimulationOptionsForm.LoadNewSoilTypeClick(Sender: TObject);
  begin
  self.loadNewClimateClick(sender);
  end;

procedure TSimulationOptionsForm.updateClimate;
  var
    i: longint;
    aClimate: GsWeather;
  begin
  climateList.items.clear;
  if Domain.templateManager.climateList.count > 0 then
    begin
    for i := 0 to Domain.templateManager.climateList.count - 1 do
    	begin
    	aClimate := GsWeather(Domain.templateManager.climateList.items[i]);
    	climateList.items.addObject(aClimate.getName, aClimate);
    	end;
    climateList.itemIndex := climateList.items.indexOfObject(Domain.templateManager.defaultClimate);
    end;
  end;

procedure TSimulationOptionsForm.updateSoilType;
  var
    i: longint;
    aSoilType: GsSoilPatch;
  begin
  soilTypeList.items.clear;
  if Domain.templateManager.soilTypeList.count > 0 then
    begin
    for i := 0 to Domain.templateManager.soilTypeList.count - 1 do
    	begin
    	aSoilType := GsSoilPatch(Domain.templateManager.soilTypeList.items[i]);
    	soilTypeList.items.addObject(aSoilType.getName, aSoilType);
    	end;
    soilTypeList.itemIndex := soilTypeList.items.indexOfObject(Domain.templateManager.defaultSoilType);
    end;
  end;

procedure TSimulationOptionsForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Simulation_options')
  end;

const
  kBetweenGap = 4;

procedure TSimulationOptionsForm.FormResize(Sender: TObject);
  var newWidth, newHeight: integer;
  begin
  newWidth := self.clientWidth - OK.width - kBetweenGap * 3;
  newHeight := self.clientHeight - tabsPanel.height - kBetweenGap * 3;
  OK.left := self.clientWidth - OK.width - kBetweenGap;
  Cancel.left := OK.left;
  EverythingGrows.left := OK.left;
  Beginner.left := OK.left;
  Advanced.left := OK.left;
  Expert.left := OK.left;
  allOn.left := OK.left;
  allOff.left := OK.left;
  helpButton.left := OK.left;
  with tabsPanel do setBounds(4, self.clientHeight - height - 4, width, height);
  with weatherPanel do setBounds(kBetweenGap, kBetweenGap, newWidth, newHeight);
  with soilPatchGeneralPanel do setBounds(kBetweenGap, kBetweenGap, newWidth, newHeight);
  with soilPatchAutoPanel do setBounds(kBetweenGap, kBetweenGap, newWidth, newHeight);
  with soilPatchNitrogenPanel do setBounds(kBetweenGap, kBetweenGap, newWidth, newHeight);
  with soilPatchPhosphorusPanel do setBounds(kBetweenGap, kBetweenGap, newWidth, newHeight);
  with plantsPanel do setBounds(kBetweenGap, kBetweenGap, newWidth, newHeight);
  end;

procedure TSimulationOptionsForm.showWeatherClick(Sender: TObject);
  begin
  self.showOnePanel(kWeatherPanel);
  end;

procedure TSimulationOptionsForm.showSoilPatchGeneralClick(Sender: TObject);
  begin
  self.showOnePanel(kSoilPatchGeneralPanel);
  if soilTypeList.items.count <= 0 then
    showMessage('No soil type templates are available.' + chr(13)
        + 'You cannot choose a default soil type until you ' + chr(13)
        + 'create or load more soil type templates.');
  end;

procedure TSimulationOptionsForm.showSoilPatchAutoClick(Sender: TObject);
  begin
  self.showOnePanel(kSoilPatchAutoPanel);
  end;

procedure TSimulationOptionsForm.showNitrogenClick(Sender: TObject);
  begin
  self.showOnePanel(kSoilPatchNitrogenPanel);
  end;

procedure TSimulationOptionsForm.showPhosphorusClick(Sender: TObject);
  begin
  self.showOnePanel(kSoilPatchPhosphorusPanel);
  end;

procedure TSimulationOptionsForm.showPlantsClick(Sender: TObject);
  begin
  self.showOnePanel(kPlantsPanel);
  end;

procedure TSimulationOptionsForm.showOnePanel(panelIndex: smallint);
  begin
  if (panelIndex < 0) or (panelIndex > kLastPanel) then
    raise Exception.create('TSimulationOptionsForm.showOnePanel: bad panel index');
  weatherPanel.visible := (panelIndex = kWeatherPanel);
  soilPatchGeneralPanel.visible := (panelIndex = kSoilPatchGeneralPanel);
  soilPatchAutoPanel.visible := (panelIndex = kSoilPatchAutoPanel);
  soilPatchNitrogenPanel.visible := (panelIndex = kSoilPatchNitrogenPanel);
  soilPatchPhosphorusPanel.visible := (panelIndex = kSoilPatchPhosphorusPanel);
  plantsPanel.visible := (panelIndex = kPlantsPanel);
  if panelForIndex(panelIndex) <> nil then
    panelForIndex(panelIndex).bringToFront;
  panelShowing := panelIndex;
  case panelIndex of
    kWeatherPanel: noteLabel.caption := '(Weather options cannot be grayed.)';
    kSoilPatchAutoPanel, kSoilPatchGeneralPanel,
      kSoilPatchNitrogenPanel, kSoilPatchPhosphorusPanel:
      noteLabel.caption := '(If an option is grayed, settings in each soil patch will be used.)';
    kPlantsPanel: noteLabel.caption := '(If an option is grayed, settings in each plant will be used.)';
    end;
  end;

function TSimulationOptionsForm.panelForIndex(panelIndex: smallint): TGroupBox;
  begin
  result := nil;
  case panelIndex of
    kWeatherPanel: result := weatherPanel;
    kSoilPatchAutoPanel: result := soilPatchAutoPanel;
    kSoilPatchGeneralPanel: result := soilPatchGeneralPanel;
    kSoilPatchNitrogenPanel: result := soilPatchNitrogenPanel;
    kSoilPatchPhosphorusPanel: result := soilPatchPhosphorusPanel;
    kPlantsPanel: result := plantsPanel;
    else
      raise Exception.create('TSimulationOptionsForm.panelForIndex: bad panel index');
    end;
  end;

procedure TSimulationOptionsForm.allowPercolationClick(Sender: TObject);
  begin
  allowNitrateLossInPercolation.enabled := allowPercolation.checked;
  allowLabilePLossInPercolation.enabled := allowPercolation.checked;
  end;

procedure TSimulationOptionsForm.allowLateralFlowClick(Sender: TObject);
  begin
  allowNitrateLossInLateralFlow.enabled := allowLateralFlow.checked;
  end;

procedure TSimulationOptionsForm.allowSoilEvaporationClick(
  Sender: TObject);
  begin
  allowNitrateToMoveToTopSoilLayerByEvaporation.enabled := allowSoilEvaporation.checked;
  end;

procedure TSimulationOptionsForm.NutrientsInRunoffDependOnRunoff;
  var enable: boolean;
  begin
  enable := allowRunoff.checked or allowRunoffFromIrrigation.checked;
  allowNitrateLossInRunoff.enabled := enable;
  allowLabilePLossInRunoff.enabled := enable;
  end;

procedure TSimulationOptionsForm.allowRunoffClick(Sender: TObject);
  begin
  allowWaterErosionFromRunoff.enabled := allowRunoff.checked;
  NutrientsInRunoffDependOnRunoff;
  end;

procedure TSimulationOptionsForm.allowRunoffFromIrrigationClick(
  Sender: TObject);
  begin
  allowWaterErosionFromRunoffFromIrrigation.enabled := allowRunoffFromIrrigation.checked;
  NutrientsInRunoffDependOnRunoff;
  end;

procedure TSimulationOptionsForm.NutrientsInSedimentDependOnErosion;
  var enable: boolean;
  begin
  enable := allowWaterErosionFromRunoff.checked or allowWaterErosionFromRunoffFromIrrigation.checked
      or allowWindErosion.checked;
  allowErodedSoilToTakeOrganicNActiveHumus.enabled := enable;
  allowErodedSoilToTakeOrganicNStableHumus.enabled := enable;
  allowErodedSoilToTakeNitrate.enabled := enable;
  allowErodedSoilToTakeAmmonia.enabled := enable;
  allowErodedSoilToTakeOrganicNFresh.enabled := enable;
  allowErodedSoilToTakeOrganicPHumus.enabled := enable;
  allowErodedSoilToTakeLabileP.enabled := enable;
  allowErodedSoilToTakeMineralPActive.enabled := enable;
  allowErodedSoilToTakeMineralPStable.enabled := enable;
  allowErodedSoilToTakeOrganicPFresh.enabled := enable;
  allowErodedSoilToTakeOrganicMatter.enabled := enable;
  allowErodedSoilToTakeFlatCropResidue.enabled := enable;
  end;

procedure TSimulationOptionsForm.allowWaterErosionFromRunoffClick(
  Sender: TObject);
  begin
  NutrientsInSedimentDependOnErosion;
  end;

procedure TSimulationOptionsForm.allowWaterErosionFromRunoffFromIrrigationClick(
  Sender: TObject);
  begin
  NutrientsInSedimentDependOnErosion;
  end;

procedure TSimulationOptionsForm.allowWindErosionClick(Sender: TObject);
  begin
  NutrientsInSedimentDependOnErosion;
  end;

procedure TSimulationOptionsForm.allowActiveHumusNMineralizationClick(Sender: TObject);
  begin
  allowActiveHumusPMineralization.enabled := allowActiveHumusNMineralization.checked;
  end;

procedure TSimulationOptionsForm.SetAllCheckboxesInPanel(panel: TGroupBox; check: boolean);
  var i: smallint;
  begin
  for i := 0 to panel.controlCount - 1 do
    if panel.controls[i] is TCheckBox then
        TCheckBox(panel.controls[i]).checked := check;
  end;

procedure TSimulationOptionsForm.grayAllCheckboxesInPanel(panel: TGroupBox);
  var i: smallint;
  begin
  for i := 0 to panel.controlCount - 1 do
    if panel.controls[i] is TCheckBox then
        TCheckBox(panel.controls[i]).state := cbGrayed;
  end;

procedure TSimulationOptionsForm.setAllCheckboxesAndComboBoxesInPanel(panel: TGroupBox; check: boolean);
  var
    i, setItemIndex: smallint;
  begin
  { only used for weather }
  SetAllCheckboxesInPanel(panel, check);
  if check then
    setItemIndex := kWeatherOptionNormal
  else
    setItemIndex := kWeatherOptionDisabled; 
  for i := 0 to panel.controlCount - 1 do
    if (panel.controls[i] is TComboBox) and (panel.controls[i].name <> 'ClimateList') then
      TComboBox(panel.controls[i]).itemIndex := setItemIndex;
  end;

procedure TSimulationOptionsForm.allOnOrOff(allOn: boolean);
  begin
  case panelShowing of
    kWeatherPanel: setAllCheckboxesAndComboBoxesInPanel(weatherPanel, allOn);
    kSoilPatchAutoPanel: SetAllCheckboxesInPanel(soilPatchAutoPanel, allOn);
    kSoilPatchGeneralPanel: SetAllCheckboxesInPanel(soilPatchGeneralPanel, allOn);
    kSoilPatchNitrogenPanel: SetAllCheckboxesInPanel(soilPatchNitrogenPanel, allOn);
    kSoilPatchPhosphorusPanel: SetAllCheckboxesInPanel(soilPatchPhosphorusPanel, allOn);
    kPlantsPanel: SetAllCheckboxesInPanel(plantsPanel, allOn);
    end;
  end;

procedure TSimulationOptionsForm.allOnClick(Sender: TObject);
  begin
  self.allOnOrOff(true);
  end;

procedure TSimulationOptionsForm.allOffClick(Sender: TObject);
  begin
  self.allOnOrOff(false);
  end;

procedure TSimulationOptionsForm.allNotOverrideClick(Sender: TObject);
  begin
  if panelShowing <> kWeatherPanel then
    grayAllCheckboxesInPanel(self.panelForIndex(panelShowing));
  end;

procedure TSimulationOptionsForm.keepCurrentWeatherClick(Sender: TObject);
  begin
  climateList.enabled := chooseNewClimateForWeather.checked and (climateList.items.count > 0);
  end;

procedure TSimulationOptionsForm.chooseNewClimateForWeatherClick(
  Sender: TObject);
  begin
  if climateList.items.count <= 0 then
    begin
    showMessage('No climate templates are available.' + chr(13)
      + 'You cannot choose another climate until you' + chr(13)
      + 'create or load more climate templates.');
    keepCurrentWeather.checked := true;
    end;
  climateList.enabled := chooseNewClimateForWeather.checked and (climateList.items.count > 0);
  end;

end.
