unit Uexit;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uexit: Exit form. Appears when user leaves the program. Suggests files the user might
want to save (from garden, groups, templates, tools files). User can check boxes for
which files to save. Choices are to save checked files and exit, don't save any files
and exit, or go back to the program. No code here; initialized and run by the
garden window (ugsim). Notice this is not created when needed, in case of a memory
problem. }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ugsform;

type
  TExitForm = class(GsForm)
    Save: TButton;
    DontSave: TButton;
    Cancel: TButton;
    helpButton: TBitBtn;
    exitLabel: TLabel;
    trademarkLabel: TLabel;
    savingLabel: TLabel;
    saveGardenFile: TCheckBox;
    saveGroupsFile: TCheckBox;
    saveTemplatesFile: TCheckBox;
    saveToolsFile: TCheckBox;
    procedure SaveClick(Sender: TObject);
    procedure DontSaveClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var exitForm: TExitForm;

implementation

{$R *.DFM}

procedure TExitForm.SaveClick(Sender: TObject);
  begin
  modalResult := mrYes;
  end;

procedure TExitForm.DontSaveClick(Sender: TObject);
  begin
  modalResult := mrNo;
  end;

procedure TExitForm.CancelClick(Sender: TObject);
  begin
  modalResult := mrCancel;
  end;

procedure TExitForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Exit_window');
  end;

end.
