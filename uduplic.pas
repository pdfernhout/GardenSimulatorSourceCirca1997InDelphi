unit Uduplic;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uduplic: Duplicate object form. Used when importing templates from a tab-delimited file.
Since objects in the file are identified only by their names, the user must resolve
problems with duplicate names. In the templates tab-delimited file are: 3d objects (tdos),
icons, harvest items, cultivars, climates, soil types, and bags. Any of these things
can duplicate an existing object of that type. The dialog asks the user whether to
overwrite the existing object, stop importing the new object, or rename the new object.
The form is absolutely stupid; it must be filled and checked by the caller. It is called
only by a method in the text filer (ufilertx) which is used by the templates manager (utempl).}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Buttons, ugsform;

type
  TduplicateObjectForm = class(GsForm)
    duplicateTypeLabel: TLabel;
    optionsForThisObject: TRadioGroup;
    optionsForAllObjects: TRadioGroup;
    OK: TButton;
    stopImport: TButton;
    objectName: TEdit;
    helpButton: TBitBtn;
    procedure OKClick(Sender: TObject);
    procedure stopImportClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TduplicateObjectForm.OKClick(Sender: TObject);
  begin
  modalResult := mrOK;
  end;

procedure TduplicateObjectForm.stopImportClick(Sender: TObject);
  begin
  modalResult := mrCancel;
  end;

procedure TduplicateObjectForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Duplicate_object_name_window');
  end;

end.
