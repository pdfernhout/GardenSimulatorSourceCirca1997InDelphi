unit Uboundch;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uboundch: Bounds change window for aspects in browser components. Fairly stupid form;
caller must initialize with pointers to aspect and groupItem, then accept or reject
changes to bounds based on whether OK was clicked. Note that changes to things in the
aspect and groupItem that can be changed in this dialog (only the unit) carry back
to the browser component. This means if I open the bounds change window and the
aspect is in meters, and I change the unit to feet in the window, it will be in feet
when I get back to the browser. Called by ubrowrl, ubrowrla, ubrowscv. }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, uaspects, ugroups, Buttons, ugsform;

type
  TBoundsChangeForm = class(GsForm)
    okButton: TButton;
    cancelButton: TButton;
    lowerBoundPanel: TPanel;
    Label1: TLabel;
    lowerBoundEdit: TEdit;
    lowerBoundAbsoluteLabel: TLabel;
    upperBoundPanel: TPanel;
    Label2: TLabel;
    upperBoundEdit: TEdit;
    upperBoundAbsoluteLabel: TLabel;
    helpButton: TBitBtn;
    lowerBoundUnit: TEdit;
    upperBoundUnit: TEdit;
    aspectNameLabel: TEdit;
    procedure okButtonClick(Sender: TObject);
    procedure cancelButtonClick(Sender: TObject);
    procedure useDefaultRangeButtonClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
    procedure lowerBoundUnitMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure upperBoundUnitMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lowerBoundUnitKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure upperBoundUnitKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    aspect: GsAspect;
    groupItem: GsGroupItem;
    softMin: single;  { all in model unit }
    softMax: single;
    hardMin: single;
    hardMax: single;
    currentUnit: integer;
    function toModelUnit(value: single): single;
    function toCurrentUnit(value: single): single;
    procedure initialize(anAspect: GsAspect; aGroupItem: GsGroupItem);
    procedure updateDisplay;
    procedure changeUnit(shift: boolean);
    procedure getSoftBoundsFromGroupItem;
    procedure getHardBoundsFromAspect;
  end;

implementation

uses uunits, usupport, udomain, ueutils;

{$R *.DFM}

const
  kDefaultHardMin: single = 0.0;
  kDefaultHardMax: single = 1000.0;

{ user should call this after creating form }
procedure TBoundsChangeForm.initialize(anAspect: GsAspect; aGroupItem: GsGroupItem);
  begin
  aspect := anAspect;
  groupItem := aGroupItem;
  self.getSoftBoundsFromGroupItem;
  self.getHardBoundsFromAspect;
  currentUnit := groupItem.currentUnit;
  if currentUnit = 0 then
    begin
    if GsDomain.default.menuOptions.showMetricUnits then
      currentUnit := aspect.unitDefaultMetric(groupItem.derivedIndex)
    else
      currentUnit := aspect.unitDefaultEnglish(groupItem.derivedIndex);
    groupItem.setUnit(currentUnit);
    end;
  self.updateDisplay;
  end;

procedure TBoundsChangeForm.getSoftBoundsFromGroupItem;
  begin
  softMin := groupItem.boundSoftLower;
  softMax := groupItem.boundSoftUpper;
  end;

procedure TBoundsChangeForm.getHardBoundsFromAspect;
  begin
  hardMin := aspect.boundHardLower(groupItem.derivedIndex);
  hardMax := aspect.boundHardUpper(groupItem.derivedIndex);
  end;

procedure TBoundsChangeForm.updateDisplay;
  var
    unitString: string[60];
  begin
  aspectNameLabel.text := removeUnitSuffix(groupItem.displayName);
  lowerBoundEdit.text := digitValueString(toCurrentUnit(softMin));
  upperBoundEdit.text := digitValueString(toCurrentUnit(softMax));
  unitString := unitStringForEnum(aspect.unitSet(groupItem.derivedIndex), currentUnit);
  lowerBoundUnit.text := unitString;
  upperBoundUnit.text := unitString;
  lowerBoundAbsoluteLabel.caption := '(Absolute lower bound: '
    + digitValueString(toCurrentUnit(hardMin)) + ' ' + unitString + '.)';
  upperBoundAbsoluteLabel.caption := '(Absolute upper bound: '
    + digitValueString(toCurrentUnit(hardMax)) + ' ' + unitString + '.)';
  end;

procedure TBoundsChangeForm.lowerBoundUnitMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  self.changeUnit(ssShift in shift);
  end;

procedure TBoundsChangeForm.upperBoundUnitMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  self.changeUnit(ssShift in shift);
  end;

procedure TBoundsChangeForm.lowerBoundUnitKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
  begin
  { up/down to cycle units }
  if (key = VK_DOWN) then
    self.changeUnit(false)
  else if (key = VK_UP) then
    self.changeUnit(true);
  end;

procedure TBoundsChangeForm.upperBoundUnitKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
  begin
  if (key = VK_DOWN) then
    self.changeUnit(false)
  else if (key = VK_UP) then
    self.changeUnit(true);
  end;

procedure TBoundsChangeForm.changeUnit(shift: boolean);
  begin
  if shift then
    currentUnit := GetPreviousUnitEnumInUnitSet(aspect.unitSet(groupItem.derivedIndex), currentUnit)
  else
    currentUnit := GetNextUnitEnumInUnitSet(aspect.unitSet(groupItem.derivedIndex), currentUnit);
  self.updateDisplay;
  end;

procedure TBoundsChangeForm.useDefaultRangeButtonClick(Sender: TObject);
  begin
  self.getSoftBoundsFromGroupItem;
  self.updateDisplay;
  end;

procedure TBoundsChangeForm.okButtonClick(Sender: TObject);
  begin
  groupsFileMightHaveChanged := true;
  self.modalResult := mrOK;
  end;

procedure TBoundsChangeForm.cancelButtonClick(Sender: TObject);
  begin
  self.modalResult := mrCancel;
  end;

function TBoundsChangeForm.toModelUnit(value: single): single;
  begin
  result := Convert(aspect.unitSet(groupItem.derivedIndex), currentUnit, aspect.unitModel(groupItem.derivedIndex), value);
  end;

function TBoundsChangeForm.toCurrentUnit(value: single): single;
  begin
  result := Convert(aspect.unitSet(groupItem.derivedIndex), aspect.unitModel(groupItem.derivedIndex), currentUnit, value);
  end;

procedure TBoundsChangeForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Bounds_change_window')
  end;

end.

