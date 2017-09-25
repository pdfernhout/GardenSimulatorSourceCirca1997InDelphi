unit Uderopt;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uderopt: Derivation options form. Aspects can be derived from simulation variables in
a few ways.
* kDeriveTypeUndefined is no derivation (just uses the simulation variable);
* kDeriveTypeDepth divides the liquid depth (mm) by the soil layer depth to get the
  relative depth fraction of the soil layer (m/m);
* kDeriveTypeConcentration divides an amount in the soil (t/ha or kg/ha) by the weight
  of the soil layer to get an amount per unit soil weight (g/t);
* kDeriveTypeArea divides an amount in the soil (t/ha or kg/ha) by the soil area to
  get an absolute amount (t or kg);
* kDeriveTypeConcentrationFromPercent is just for the bag, and turns a percentage
  concentration by weight into a concentration by weight (g/t) by simple conversion.
This dialog allows the user to choose between whatever derivation options are available for any
aspect. It is accessed from the group editor, browser or graph window. (In the browser and
graph window you ctrl-click on the unit.) The dialog is pretty stupid and lets the caller
fill it and get the results. }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, uaspects, ugsform;

type
  TAspectDerivationForm = class(GsForm)
    OK: TButton;
    Cancel: TButton;
    help: TBitBtn;
    methodChoice: TRadioGroup;
    methodDescription: TMemo;
    Label2: TLabel;
    procedure methodChoiceClick(Sender: TObject);
    procedure helpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    descriptions: array[0..kMaxNumDerivations] of string;
  procedure initialize(name0, desc0, name1, desc1, name2, desc2, name3, desc3: string; deriveMethod: smallint);
  function deriveMethod: smallint;
  end;

implementation

{$R *.DFM}

procedure TAspectDerivationForm.initialize(
    name0, desc0, name1, desc1, name2, desc2, name3, desc3: string; deriveMethod: smallint);
  begin
  methodChoice.items.clear;
  if name0 = '' then
    raise Exception.create('TAspectDerivationForm.initialize: aspect must have at least one derivation name');
  methodChoice.items.add(name0);
  if name1 <> '' then methodChoice.items.add(name1);
  if name2 <> '' then methodChoice.items.add(name2);
  if name3 <> '' then methodChoice.items.add(name3);
  descriptions[0] := desc0;
  descriptions[1] := desc1;
  descriptions[2] := desc2;
  descriptions[3] := desc3;
  methodChoice.itemIndex := deriveMethod;
  end;

function TAspectDerivationForm.deriveMethod: smallint;
  begin
  result := methodChoice.itemIndex;
  end;

procedure TAspectDerivationForm.methodChoiceClick(Sender: TObject);
  begin
  methodDescription.lines.clear;
  methodDescription.lines.add(descriptions[methodChoice.itemIndex]);
  end;

procedure TAspectDerivationForm.helpClick(Sender: TObject);
  begin
  application.helpJump('windows_Derivation_options_window')
  end;

end.
