unit Upatarea;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
upatarea: Soil patch area form. Also contains some other stuff about the soil patch.
We need this information right away when the soil patch is created, so it pops up
right when the user is done sizing the soil patch after creation. The form can also
appear if the user selects the tool action 'change soil patch' (usually in the shovel).
If the soil patch has just been created, the area and soil type are editable; if the soil
patch was created previously (and 'change soil patch' is being done) the area
and soil type are not editable. Actually it would not be that big a deal to change
the soil patch area now, because we divorced it from the watershed area (it used to
be the same thing). But we have not looked into changing it. Because this form is
needed often and we don't want it to fail, it is created automatically at startup
like the non-modal windows (garden, browser, graph, harvest). The other modal
windows that have this distinction are the wait form (for progress messages),
the about box form, the numerical exceptions form (udebug), and the exit form. }

interface

uses
  WinProcs, WinTypes, SysUtils, Messages, Classes, Graphics, Controls, 
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, uesoil, usliders, uestruct, ugsform;

type
  TSoilPatchAreaForm = class(GsForm)
    OK: TButton;
    cancel: TButton;
    areaPanel: TPanel;
    PatchArea: TEdit;
    StaticText1: TLabel;
    shadePanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    anglePanel: TPanel;
    Label8: TLabel;
    top: TRadioButton;
    side: TRadioButton;
    custom: TRadioButton;
    customAngle: TEdit;
    Label7: TLabel;
    scalePanel: TPanel;
    Label4: TLabel;
    scale: TEdit;
    Label9: TLabel;
    baseSoilTypePanel: TPanel;
    Label5: TLabel;
    baseSoilType: TComboBox;
    helpButton: TBitBtn;
    SlopePanel: TPanel;
    Label11: TLabel;
    orientation: TComboBox;
    Label6: TLabel;
    slope: TEdit;
    Label10: TLabel;
    patchAreaUnits: TEdit;
    procedure orientationChange(Sender: TObject);
    procedure shadeLevelChange(Sender: TObject);
    procedure topClick(Sender: TObject);
    procedure sideClick(Sender: TObject);
    procedure customClick(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure cancelClick(Sender: TObject);
    procedure patchAreaUnitsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure helpButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure patchAreaUnitsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    soilPatchInfo: SoilPatchBasicInfoStructure;
    currentAreaUnit: smallint;
    shadeLevelSlider: KfSlider;
    procedure initializeWithSoilPatchBasicInfo(someInfo: SoilPatchBasicInfoStructure;
        isNewPatch: boolean; soilTypeNameIfNotNew: string);
    procedure fillBasicInfoStructure(var theInfo: SoilPatchBasicInfoStructure);
    function selectedSoilType: GsSoilPatch;
    function toModelAreaUnit(value: single): single;
    function toCurrentAreaUnit(value: single): single;
    function getValueFromEditText(name: string; editText: TEdit; var value: single; lowerBound: single;
      upperBound: single; isArea: boolean): boolean;
  end;

var soilPatchAreaForm: TSoilPatchAreaForm;

implementation

{$R *.DFM}

uses uunits, udomain, usupport;

const
  kAreaUnitSet = kArea;
  kAreaUnitModel = kAreaHectares;
  kAreaBoundLower_ha = 0.0000001; {0.01 m2 = 10x10 cm}
  kAreaBoundUpper_ha = 100.0;
  kScaleBoundLower = 0.001; { arbitrary - CFK FIX get better }
  kScaleBoundUpper = 100.0; { arbitrary - CFK FIX get better }

procedure TSoilPatchAreaForm.FormCreate(Sender: TObject);
	begin
  shadeLevelSlider := KfSlider.create(self);
  with shadeLevelSlider do
    begin
    parent := shadePanel;
    minValue := 0;
    maxValue := 100;
    currentValue := 100;
    left := 128;
    top := 5;
    useDefaultSizeAndDraggerSize;
    width := 93;
    FOnMouseMove := self.shadeLevelChange;
    FOnMouseUp := self.shadeLevelChange;
    FOnKeyDown := self.shadeLevelChange;
    tabStop := true;
    end;
	end;

procedure TSoilPatchAreaForm.initializeWithSoilPatchBasicInfo(someInfo: SoilPatchBasicInfoStructure;
    isNewPatch: boolean; soilTypeNameIfNotNew: string);
  var
    direction: smallint;
    i: longint;
    aSoilType: GsSoilPatch;
  begin
  { copy info }
  soilPatchInfo := someInfo;
  if isNewPatch then
    cancel.visible := false;
  { fill soil type combo box  }
  baseSoilType.items.clear;
  if Domain.templateManager.soilTypeList.count > 0 then
    for i := 0 to Domain.templateManager.soilTypeList.count - 1 do
    	begin
    	aSoilType := GsSoilPatch(Domain.templateManager.soilTypeList.items[i]);
    	baseSoilType.items.addObject(aSoilType.getName, aSoilType);
    	end
  else
    raise Exception.create('No soil types to copy from');
  { can only choose soil type if new }
  baseSoilType.enabled := isNewPatch;
  { if new patch, select default; otherwise, select using name stored in patch }
  if isNewPatch then
    baseSoilType.itemIndex := baseSoilType.items.indexOfObject(Domain.templateManager.defaultSoilType)
  else
    baseSoilType.itemIndex := baseSoilType.items.indexOf(soilTypeNameIfNotNew);
  { set up unit }
  if Domain.menuOptions.showMetricUnits then
    currentAreaUnit := kAreaSquareMeters
  else
    currentAreaUnit := kAreaSquareFeet;
  patchAreaUnits.text := unitStringForEnum(kAreaUnitSet, currentAreaUnit);
  patchArea.text := digitValueString(toCurrentAreaUnit(soilPatchInfo.area_ha));
  { can only set area if new }
  patchArea.enabled := isNewPatch;
  slope.text := digitValueString(soilPatchInfo.slope_mPm * 100.0);
  { there are 16 direction divisions }
  direction := round(soilPatchInfo.orientationFromNorth_deg / (360.0 / 16.0));
  if (direction >= 0) and (direction <= orientation.items.count - 1) then
    orientation.itemIndex := direction
  else
    raise Exception.create('Orientation direction out of bounds');
  shadeLevelSlider.currentValue := round(soilPatchInfo.shade_pct);
  if soilPatchInfo.viewingAngle_deg = 90 {top} then
    begin
    top.checked := true;
    customAngle.text := '';
    customAngle.enabled := false;
    end
  else if soilPatchInfo.viewingAngle_deg = 0 {side} then
    begin
    side.checked := true;
    customAngle.text := '';
    customAngle.enabled := false;
    end
  else
    begin
    custom.checked := true;
    customAngle.text := digitValueString(soilPatchInfo.viewingAngle_deg);
    customAngle.enabled := true;
    end;
  scale.text := digitValueString(soilPatchInfo.scale);
  end;

procedure TSoilPatchAreaForm.fillBasicInfoStructure(var theInfo: SoilPatchBasicInfoStructure);
  begin
  theInfo := soilPatchInfo;
  end;

procedure TSoilPatchAreaForm.patchAreaUnitsKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
  begin
  if key = VK_DOWN then
    self.patchAreaUnitsMouseUp(sender, mbLeft, [ssShift], 0, 0)
  else if key = VK_UP then
    self.patchAreaUnitsMouseUp(sender, mbLeft, [], 0, 0)
  end;

procedure TSoilPatchAreaForm.patchAreaUnitsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  if ssShift in shift then
    currentAreaUnit := GetPreviousUnitEnumInunitSet(kAreaUnitSet, currentAreaUnit)
  else
    currentAreaUnit := GetNextUnitEnumInunitSet(kAreaUnitSet, currentAreaUnit);
  patchAreaUnits.text := unitStringForEnum(kAreaUnitSet, currentAreaUnit);
  patchArea.text := digitValueString(toCurrentAreaUnit(soilPatchInfo.area_ha));
  end;

function TSoilPatchAreaForm.toModelAreaUnit(value: single): single;
  begin
  result := Convert(kAreaUnitSet, currentAreaUnit, kAreaUnitModel, value);
  end;

function TSoilPatchAreaForm.toCurrentAreaUnit(value: single): single;
  begin
  result := Convert(kAreaUnitSet, kAreaUnitModel, currentAreaUnit, value);
  end;

function TSoilPatchAreaForm.selectedSoilType: GsSoilPatch;
  begin
  if baseSoilType.items.count = 0 then
    result := nil
  else if baseSoilType.itemIndex <> -1 then
    result := GsSoilPatch(baseSoilType.items.objects[baseSoilType.itemIndex])
  else
    result := nil;
  end;

function TSoilPatchAreaForm.getValueFromEditText(name: string; editText: TEdit; var value: single; lowerBound: single;
    upperBound: single; isArea: boolean): boolean;
  var tryValue: single;
  begin
  tryValue := 0.0;
  result := true;
  try
    tryValue := strToFloat(editText.text);
    if (tryValue < lowerBound) or (tryValue > upperBound) then
      result := false;
  except
    result := false;
  end;
  if result then
    value := tryValue
  else
    begin
    if isArea then
      begin
      lowerBound := toCurrentAreaUnit(lowerBound);
      upperBound := toCurrentAreaUnit(upperBound);
      end;
    showMessage('The value entered for ' + name + ' (' + editText.text + ') ' + chr(13)
      + 'is out of range (' + digitValueString(lowerBound) + ' to ' + digitValueString(upperBound) + ') ' + chr(13)
      + 'or not a valid number.');
    end;
  end;

procedure TSoilPatchAreaForm.orientationChange(Sender: TObject);
  begin
  if orientation.itemIndex <> -1 then
    soilPatchInfo.orientationFromNorth_deg := orientation.itemIndex * (360.0 / 16.0);
  end;

procedure TSoilPatchAreaForm.shadeLevelChange(Sender: TObject);
  begin
  soilPatchInfo.shade_pct := shadeLevelSlider.currentValue;
  shadeLevelSlider.invalidate;
  end;

procedure TSoilPatchAreaForm.topClick(Sender: TObject);
  begin
  soilPatchInfo.viewingAngle_deg := 90;
  customAngle.enabled := false;
  end;

procedure TSoilPatchAreaForm.sideClick(Sender: TObject);
  begin
  soilPatchInfo.viewingAngle_deg := 0;
  customAngle.enabled := false;
  end;

procedure TSoilPatchAreaForm.customClick(Sender: TObject);
  begin
  customAngle.enabled := true;
  customAngle.text := digitValueString(soilPatchInfo.viewingAngle_deg);
  end;

procedure TSoilPatchAreaForm.OKClick(Sender: TObject);
  var
    tryValue: single;
    okToLeave: boolean;
  begin
  okToLeave := true;
  if self.getValueFromEditText('area', patchArea, tryValue, kAreaBoundLower_ha, kAreaBoundUpper_ha, true) then
    soilPatchInfo.area_ha := toModelAreaUnit(tryValue)
  else
    begin
    patchArea.text := digitValueString(toCurrentAreaUnit(soilPatchInfo.area_ha));
    okToLeave := false;
    end;
  if self.getValueFromEditText('slope', slope, tryValue, 0, 100, false) then
    soilPatchInfo.slope_mPm := tryValue / 100.0
  else
    begin
    slope.text := digitValueString(soilPatchInfo.slope_mPm * 100.0);
    okToLeave := false;
    end;
  if custom.checked then
    if self.getValueFromEditText('viewing angle', customAngle, tryValue, 0, 360, false) then
      soilPatchInfo.viewingAngle_deg := tryValue
    else
      begin
      customAngle.text := digitValueString(soilPatchInfo.viewingAngle_deg);
      okToLeave := false;
      end;
  if self.getValueFromEditText('drawing scale', scale, tryValue, kScaleBoundLower, kScaleBoundUpper, false) then
    soilPatchInfo.scale := tryValue
  else
    begin
    scale.text := digitValueString(soilPatchInfo.scale);
    okToLeave := false;
    end;
  if okToLeave then modalResult := mrOK;
  end;

procedure TSoilPatchAreaForm.cancelClick(Sender: TObject);
  begin
  modalResult := mrCancel;
  end;

procedure TSoilPatchAreaForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Soil_patch_basic_parameters_window')
  end;

procedure TSoilPatchAreaForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  begin
  { move shade slider up, or start at beginning if at end }
  if (ssAlt in shift) and (chr(key) in ['S', 's']) then
    begin
    shadeLevelSlider.currentValue := shadeLevelSlider.currentValue + 10;
    if shadeLevelSlider.currentValue > 100 then shadeLevelSlider.currentValue := 0;
    self.shadeLevelChange(self);
    end;
  end;

end.
