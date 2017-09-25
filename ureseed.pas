unit Ureseed;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
ureseed: Reseeding options form. User can choose when to reseed all the plants
in the garden (choose from radio buttons), or reseed now (click check box).
Sets things directly in domain if OK pressed, but the calling form (ugsim)
does updating and immediate reseeding based on the result of the form.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ugsform;

type
  TReseedingOptionsForm = class(GsForm)
    OK: TButton;
    Cancel: TButton;
    ReseedNow: TCheckBox;
    frequencyGroupBox: TGroupBox;
    DontReseedUnlessRequested: TRadioButton;
    ReseedAllPlantsOnPlantingDate: TRadioButton;
    ReseedAllPlantsOnTheFirstOf: TRadioButton;
    ReseedingDate: TComboBox;
    helpButton: TBitBtn;
    keepPlantsDormantUntilNextReseeding: TCheckBox;
    procedure FormActivate(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
    procedure DontReseedUnlessRequestedClick(Sender: TObject);
    procedure ReseedAllPlantsOnPlantingDateClick(Sender: TObject);
    procedure ReseedAllPlantsOnTheFirstOfClick(Sender: TObject);
    procedure ReseedNowClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure updateEnabling;
  end;

implementation

uses udomain;

{$R *.DFM}

procedure TReseedingOptionsForm.FormActivate(Sender: TObject);
	begin
  with Domain.menuOptions do
    begin
  	ReseedAllPlantsOnTheFirstOf.checked := (reseedingOption = kReseedingOnMonth);
  	ReseedAllPlantsOnPlantingDate.checked := (reseedingOption = kReseedingWhenPlanted);
  	DontReseedUnlessRequested.checked := (reseedingOption = kReseedingOptionNever);
    self.updateEnabling;
    ReseedingDate.itemIndex := reseedingMonth;
    end;
	end;

procedure TReseedingOptionsForm.OKClick(Sender: TObject);
	begin
  with Domain.menuOptions do
    begin
  	if DontReseedUnlessRequested.checked then
      reseedingOption := kReseedingOptionNever
    else if ReseedAllPlantsOnPlantingDate.checked then
      reseedingOption := kReseedingWhenPlanted
    else if ReseedAllPlantsOnTheFirstOf.checked then
      begin
      reseedingOption := kReseedingOnMonth;
      reseedingMonth := ReseedingDate.itemIndex;
      end
    else
      raise Exception.create('Unexpected alternative');
    end;
	end;

procedure TReseedingOptionsForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Reseeding_options')
  end;

procedure TReseedingOptionsForm.updateEnabling;
  begin
  reseedingDate.enabled := reseedAllPlantsOnTheFirstOf.checked;
  keepPlantsDormantUntilNextReseeding.enabled := ((reseedNow.checked)
      and (reseedAllPlantsOnPlantingDate.checked or reseedAllPlantsOnTheFirstOf.checked));
  end;

procedure TReseedingOptionsForm.DontReseedUnlessRequestedClick(Sender: TObject);
  begin
  self.updateEnabling;
  end;

procedure TReseedingOptionsForm.ReseedAllPlantsOnPlantingDateClick(
  Sender: TObject);
  begin
  self.updateEnabling;
  end;

procedure TReseedingOptionsForm.ReseedAllPlantsOnTheFirstOfClick(
  Sender: TObject);
  begin
  self.updateEnabling;
  end;

procedure TReseedingOptionsForm.ReseedNowClick(Sender: TObject);
  begin
  self.updateEnabling;
  end;

end.
