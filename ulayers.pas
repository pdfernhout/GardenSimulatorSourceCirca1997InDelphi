unit Ulayers;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ulayers: Layer options form. Lets user choose options for displaying an aspect having
to do with soil layers. Note this is for soil layer aspects only, not for aspects
having to do with other arrays such as weather months. This is called by the group
editor and by the graph editor. Choices are to show data from only one layer, to
select data from any number of layers, or to show summary information for all or some
layers (min, max, mean, sum). This form is mainly stupid, but has a few checks.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Buttons, StdCtrls, ExtCtrls, ugroups, ugsform;

const
  kLayersFormIsBeingCalledFromGraphForm = true;
  kLayersFormIsNotBeingCalledFromGraphForm = false;

type
  TlayerOptionsForm = class(GsForm)
    OK: TButton;
    Cancel: TButton;
    help: TBitBtn;
    arrayShowTypeRadioGroup: TRadioGroup;
    layersList: TListBox;
    selectAllIndexes: TButton;
    Label1: TLabel;
    aspectName: TEdit;
    procedure selectAllIndexesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure initialize(aName: string; anArrayShowType: smallint; anArraySelected: selectionArray;
        isBeingCalledFromGraphForm: boolean);
    procedure getResult(var anArrayShowType: smallint; var anArraySelected: selectionArray);
    procedure fillItems(isBeingCalledFromGraphForm: boolean);
  end;

implementation

{$R *.DFM}

procedure TlayerOptionsForm.initialize(aName: string; anArrayShowType: smallint; anArraySelected: selectionArray;
    isBeingCalledFromGraphForm: boolean);
  var i: smallint;
  begin
  aspectName.text := aName;
  self.fillItems(isBeingCalledFromGraphForm);
  { don't allow graphed aspect to have all values }
  if isBeingCalledFromGraphForm and (anArrayShowType = kArrayShowAllValues) then
    anArrayShowType := kArrayShowMeanWeightedBySoilLayerThickness;
  if (anArrayShowType >= 0) and (anArrayShowType <= arrayShowTypeRadioGroup.items.count - 1) then
    arrayShowTypeRadioGroup.itemIndex := anArrayShowType
  else
    arrayShowTypeRadioGroup.itemIndex := 0;
  for i := 0 to 9 do layersList.selected[i] := anArraySelected[i];
  end;

procedure TLayerOptionsForm.fillItems(isBeingCalledFromGraphForm: boolean);
  var lastIndex, i: smallint;
  begin
  arrayShowTypeRadioGroup.items.clear;
  if isBeingCalledFromGraphForm then
    lastIndex := kArrayShowSum
  else
    lastIndex := kArrayShowAllValues;
  for i := 0 to lastIndex do
    arrayShowTypeRadioGroup.items.add(GsGroupItem.arrayShowTypeLongName(i));
  end;

procedure TLayerOptionsForm.getResult(var anArrayShowType: smallint; var anArraySelected: selectionArray);
  var i: smallint;
  begin
  if arrayShowTypeRadioGroup.itemIndex >= 0 then
    anArrayShowType := arrayShowTypeRadioGroup.itemIndex;
  for i := 0 to 9 do anArraySelected[i] := layersList.selected[i];
  end;

procedure TlayerOptionsForm.selectAllIndexesClick(Sender: TObject);
  var i: integer;
  begin
  if layersList.items.count > 0 then
    for i := 0 to layersList.items.count - 1 do layersList.selected[i] := true;
  layersList.invalidate;
  end;

end.
