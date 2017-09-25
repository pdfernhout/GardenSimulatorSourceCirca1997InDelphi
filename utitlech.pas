unit Utitlech;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
utitlech: Graph title form. User can change graph title, axis titles, and choose whether
to auto-title axes (from aspects graphed) and whether to autoscale or input scale for
each axis. Called only from graph window. Graph window calls initializeForAxis for
each axis, then gets the data back after OK is clicked. Nothing has to be copied
as the graph window directly inspects the values of the interface elements. Some
complications arise from interdependence of axis title edit boxes and 'auto-title'
check boxes. If 'auto-title' is checked, the appropriate edit box becomes read-only.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, ugrafcom, ulogvar, ugsform;

type
  TGraphTitleForm = class(GsForm)
    OK: TButton;
    cancel: TButton;
    leftAxisGroupBox: TGroupBox;
    Label2: TLabel;
    leftAxisTitleEdit: TEdit;
    Label4: TLabel;
    leftAxisMinimum: TEdit;
    Label5: TLabel;
    leftAxisMaximum: TEdit;
    leftAxisAutoScale: TCheckBox;
    rightAxisGroupBox: TGroupBox;
    Label3: TLabel;
    rightAxisTitleEdit: TEdit;
    Label6: TLabel;
    rightAxisMinimum: TEdit;
    Label7: TLabel;
    rightAxisMaximum: TEdit;
    rightAxisAutoScale: TCheckBox;
    help: TBitBtn;
    leftAxisShowZero: TCheckBox;
    rightAxisShowZero: TCheckBox;
    Graphgroupbox: TGroupBox;
    Label1: TLabel;
    graphTitleEdit: TEdit;
    leftAxisAutoTitle: TCheckBox;
    rightAxisAutoTitle: TCheckBox;
    procedure OKClick(Sender: TObject);
    procedure cancelClick(Sender: TObject);
    procedure leftAxisAutoScaleClick(Sender: TObject);
    procedure rightAxisAutoScaleClick(Sender: TObject);
    procedure leftAxisMinimumChange(Sender: TObject);
    procedure leftAxisMaximumChange(Sender: TObject);
    procedure rightAxisMinimumChange(Sender: TObject);
    procedure rightAxisMaximumChange(Sender: TObject);
    procedure helpClick(Sender: TObject);
    procedure leftAxisAutoTitleClick(Sender: TObject);
    procedure rightAxisAutoTitleClick(Sender: TObject);
    procedure leftAxisTitleEditChange(Sender: TObject);
    procedure rightAxisTitleEditChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    leftAxisAutoMin, leftAxisAutoMax, rightAxisAutoMin, rightAxisAutoMax: single;
    leftAxisUserMin, leftAxisUserMax, rightAxisUserMin, rightAxisUserMax: single;
    leftAxisGeneratedTitle, rightAxisGeneratedTitle: string;
    leftAxisUserTitle, rightAxisUserTitle: string;
    procedure initializeForAxis(whichAxis: smallint; axis: GsGraphScale; start, stop: single; generatedTitle: string);
    procedure setAxisTitleReadOnly(axis: smallint; readOnly: boolean);
  end;

implementation

{$R *.DFM}

uses usupport;

var internalChange, internalCheck: boolean;

procedure TGraphTitleForm.OKClick(Sender: TObject);
  begin
  modalResult := mrOK;
  end;

procedure TGraphTitleForm.cancelClick(Sender: TObject);
  begin
  modalResult := mrCancel;
  end;

procedure TGraphTitleForm.initializeForAxis(whichAxis: smallint; axis: GsGraphScale; start, stop: single;
    generatedTitle: string);
  begin
  internalChange := true;
  internalCheck := true;
  if whichAxis = kLeftAxis then with axis do
    begin
    leftAxisAutoTitle.checked := autoTitle;
    leftAxisTitleEdit.text := getTitle;
    if autoTitle then
      begin
      leftAxisGeneratedTitle := getTitle;
      leftAxisUserTitle := '';
      end
    else
      begin
      leftAxisGeneratedTitle := generatedTitle;
      leftAxisUserTitle := getTitle;
      end;
    self.setAxisTitleReadOnly(kLeftAxis, leftAxisAutoTitle.checked);
    leftAxisShowZero.checked := showZeroLine;
    leftAxisAutoScale.checked := autoScale;
    leftAxisAutoMin := start;
    leftAxisAutoMax := stop;
    leftAxisUserMin := leftAxisAutoMin;
    leftAxisUserMax := leftAxisAutoMax;
    leftAxisMinimum.text := digitValueString(yStart);
    leftAxisMaximum.text := digitValueString(yStart + yRange);
    end
  else if whichAxis = kRightAxis then with axis do
    begin
    rightAxisAutoTitle.checked := autoTitle;
    rightAxisTitleEdit.text := getTitle;
    if autoTitle then
      begin
      rightAxisGeneratedTitle := getTitle;
      rightAxisUserTitle := '';
      end
    else
      begin
      rightAxisGeneratedTitle := generatedTitle;
      rightAxisUserTitle := getTitle;
      end;
    self.setAxisTitleReadOnly(kRightAxis, rightAxisAutoTitle.checked);
    rightAxisShowZero.checked := showZeroLine;
    rightAxisAutoScale.checked := autoScale;
    rightAxisAutoMin := start;
    rightAxisAutoMax := stop;
    rightAxisUserMin := rightAxisAutoMin;
    rightAxisUserMax := rightAxisAutoMax;
    rightAxisMinimum.text := digitValueString(yStart);
    rightAxisMaximum.text := digitValueString(yStart + yRange);
    end;
  internalChange := false;
  internalCheck := false;
  end;

procedure TGraphTitleForm.setAxisTitleReadOnly(axis: smallint; readOnly: boolean);
  begin
  if axis = kLeftAxis then
    begin
    leftAxisTitleEdit.readOnly := readOnly;
    if readOnly then leftAxisTitleEdit.color := clBtnFace else leftAxisTitleEdit.color := clWindow;
    leftAxisTitleEdit.invalidate;
    end
  else if axis = kRightAxis then
    begin
    rightAxisTitleEdit.readOnly := readOnly;
    if readOnly then rightAxisTitleEdit.color := clBtnFace else rightAxisTitleEdit.color := clWindow;
    rightAxisTitleEdit.invalidate;
    end;
  end;

procedure TGraphTitleForm.leftAxisAutoScaleClick(Sender: TObject);
  begin
  if internalCheck then exit;
  if leftAxisAutoScale.checked then
    begin
    internalChange := true;
    leftAxisMinimum.text := digitValueString(leftAxisAutoMin);
    leftAxisMaximum.text := digitValueString(leftAxisAutoMax);
    internalChange := false;
    end
  else
    begin
    leftAxisMinimum.text := digitValueString(leftAxisUserMin);
    leftAxisMaximum.text := digitValueString(leftAxisUserMax);
    end;
  end;

procedure TGraphTitleForm.rightAxisAutoScaleClick(Sender: TObject);
  begin
  if internalCheck then exit;
  if rightAxisAutoScale.checked then
    begin
    internalChange := true;
    rightAxisMinimum.text := digitValueString(rightAxisAutoMin);
    rightAxisMaximum.text := digitValueString(rightAxisAutoMax);
    internalChange := false;
    end
  else
    begin
    rightAxisMinimum.text := digitValueString(rightAxisUserMin);
    rightAxisMaximum.text := digitValueString(rightAxisUserMax);
    end;
  end;

procedure TGraphTitleForm.leftAxisMinimumChange(Sender: TObject);
  var saved: single;
  begin
  if internalChange then exit;
  saved := leftAxisUserMin;
  try
    leftAxisUserMin := strToFloat(leftAxisMinimum.text);
    leftAxisAutoScale.checked := false;
  except
    leftAxisUserMin := saved;
  end;
  end;

procedure TGraphTitleForm.leftAxisMaximumChange(Sender: TObject);
  var saved: single;
  begin
  if internalChange then exit;
  saved := leftAxisUserMax;
  try
    leftAxisUserMax := strToFloat(leftAxisMaximum.text);
    leftAxisAutoScale.checked := false;
  except
    leftAxisUserMax := saved;
  end;
  end;

procedure TGraphTitleForm.rightAxisMinimumChange(Sender: TObject);
  var saved: single;
  begin
  if internalChange then exit;
  saved := rightAxisUserMin;
  try
    rightAxisUserMin := strToFloat(rightAxisMinimum.text);
    rightAxisAutoScale.checked := false;
  except
    rightAxisUserMin := saved;
  end;
  end;

procedure TGraphTitleForm.rightAxisMaximumChange(Sender: TObject);
  var saved: single;
  begin
  if internalChange then exit;
  saved := rightAxisUserMax;
  try
    rightAxisUserMax := strToFloat(rightAxisMaximum.text);
    rightAxisAutoScale.checked := false;
  except
    rightAxisUserMax := saved;
  end;
  end;

procedure TGraphTitleForm.helpClick(Sender: TObject);
  begin
  application.helpJump('windows_Graph_titles_and_scales_window')
  end;

procedure TGraphTitleForm.leftAxisAutoTitleClick(Sender: TObject);
  begin
  if internalChange then exit;
  internalChange := true;
  if leftAxisAutoTitle.checked then
    { reset to auto title }
    leftAxisTitleEdit.text := leftAxisGeneratedTitle
  else
    { reset to user title }
    leftAxisTitleEdit.text := leftAxisUserTitle;
  internalChange := false;
  self.setAxisTitleReadOnly(kLeftAxis, leftAxisAutoTitle.checked);
  end;

procedure TGraphTitleForm.rightAxisAutoTitleClick(Sender: TObject);
  begin
  if internalChange then exit;
  internalChange := true;
  if rightAxisAutoTitle.checked then
    { reset to auto title }
    rightAxisTitleEdit.text := rightAxisGeneratedTitle
  else
    { reset to user title }
    rightAxisTitleEdit.text := rightAxisUserTitle;
  internalChange := false;
  self.setAxisTitleReadOnly(kRightAxis, rightAxisAutoTitle.checked);
  end;

procedure TGraphTitleForm.leftAxisTitleEditChange(Sender: TObject);
  begin
  if internalChange then exit;
  leftAxisUserTitle := leftAxisTitleEdit.text;
  end;

procedure TGraphTitleForm.rightAxisTitleEditChange(Sender: TObject);
  begin
  if internalChange then exit;
  rightAxisUserTitle := rightAxisTitleEdit.text;
  end;

end.
