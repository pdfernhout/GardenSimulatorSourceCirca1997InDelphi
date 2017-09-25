unit Udebug;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
udebug: The numerical exceptions window. All calls to the global procedure debugPrint
cause this window to add a text line to its list box. If the 'appear' check box is
checked, the window comes to the front. If the 'log to file' check box is checked,
the line is added to a text file. The 'break' check box only appears if the GS_DEBUG
conditional define is set (see ueutils). If the 'stop simulation' check box is checked,
the simulation stops running at the end of the day. The messages that appear are not
that complex; they are mostly in calls to the errorMessage function (ueutils) which
just calls debugPrint. All the eq and ep level functions in the EPIC part of the code have
error messages in their exception code, and they usually pass on the primary error
string as well.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ugsform, ExtCtrls;

type
  TDebugForm = class(GsForm)
    DebugList: TListBox;
    Close: TButton;
    saveListAs: TButton;
    clearList: TButton;
    helpButton: TBitBtn;
    optionsPanel: TPanel;
    exceptionLabel: TLabel;
    showOnExceptionCheckBox: TCheckBox;
    logToFile: TCheckBox;
    stopSimulationOnException: TCheckBox;
    breakOnException: TCheckBox;
    procedure CloseClick(Sender: TObject);
    procedure saveListAsClick(Sender: TObject);
    procedure clearListClick(Sender: TObject);
    procedure helpButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure showOnExceptionCheckBoxClick(Sender: TObject);
    procedure logToFileClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure stopSimulationOnExceptionClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    outputFile: TextFile;
    logging: boolean;
    procedure print(aString: string);
    procedure printNested(level: longint; aString: string);
    procedure clear;
		procedure startLogging(fileName: string);
		procedure stopLogging;
    procedure saveExceptionListToFile(fileName: string);
    procedure respondToSimulationRunningStatus(isRunning: boolean);
  end;

procedure DebugPrint(aMessage: string);

var
  DebugForm: TDebugForm;

implementation

{$R *.DFM}

uses udomain, usupport;

procedure DebugPrint(aMessage: string);
	begin
  DebugForm.print(aMessage);
  end;

procedure TDebugForm.FormCreate(Sender: TObject);
  begin
  {$IFDEF GS_DEBUG}
    breakOnException.visible := true;
  {$ELSE}
    breakOnException.visible := false;
  {$ENDIF}
  showOnExceptionCheckBox.checked := Domain.menuOptions.showNumericalExceptionsWindowOnException;
  stopSimulationOnException.checked := Domain.menuOptions.stopSimulationAtEndOfDayOnException;
  if breakOnException.visible then breakOnException.checked := true;
  { this will make the list box create a horizontal scroll bar }
  sendMessage(debugList.handle, LB_SETHORIZONTALEXTENT, 1000, longint(0));
  end;

procedure TDebugForm.FormDestroy(Sender: TObject);
  begin
  self.stopLogging;
  end;

procedure TDebugForm.respondToSimulationRunningStatus(isRunning: boolean);
  begin
  {everything can be done}
  end;

procedure TDebugForm.startLogging(fileName: string);
  var dateString: string;
	begin
  if logging then exit;
	assignFile(outputFile, fileName);
	if not fileExists(fileName) then
    rewrite(outputFile)
  else
    begin
    append(outputFile);
    dateString := formatDateTime('m/d/yyyy, h:m am/pm', now);
    writeln(outputFile, '---- log start (' + dateString + ') ----');
    end;
  logging := true;
  end;

procedure TDebugForm.stopLogging;
	begin
  if not logging then exit;
  writeln(outputFile, '---- log stop ----');
  closeFile(outputFile);
  logging := false;
	end;

procedure TDebugForm.saveExceptionListToFile(fileName: string);
  var i: longint;
  begin
	assignFile(outputFile, fileName);
	rewrite(outputFile);
  try
  if debugList.items.count > 0 then
    for i := 0 to debugList.items.count - 1 do
      writeln(outputFile, debugList.items[i]);
  finally
  closeFile(outputFile);
  end;
  end;

procedure TDebugForm.printNested(level: longint; aString: string);
  var
  i: longint;
{$IFDEF WINDOWS}
  prefix: string;
{$ELSE}
  prefix: ansistring;
{$ENDIF}
	begin
  if level > 40 then level := 40;
  prefix := '';
  if level > 0 then
  	for i := 1 to level do
    	AppendStr(prefix, '..');
  print(prefix + aString);
  end;

procedure TDebugForm.print(aString: string);
  var response: word;
	begin
  if logging then
  	begin
  	writeln(outputFile, aString);
    flush(outputFile);
    end;
  if showOnExceptionCheckBox.checked then
    begin
    self.visible := true;
    self.bringToFront;
    end;
  if debugList.items.count > 1000 then debugList.items.clear;
  try
    DebugList.items.add(aString);
  except
    on EOutOfResources do
      debugList.items.clear;
  end;
  DebugList.itemIndex := DebugList.items.count - 1;
  if breakOnException.visible and breakOnException.checked then
    begin
    response := messageDlg('Error! ' + chr(13) + aString + chr(13) + 'Stop? (Press F8 to see place in code.)',
        mtWarning, mbYesNoCancel, 0);
    if response = mrYes then
      debugBreak
    else if response = mrCancel then
      breakOnException.checked := false;
    end;
  debugList.invalidate;
  debugList.refresh;
  end;

procedure TDebugForm.clear;
	begin
  self.visible := false;
  debugList.items.clear;
  end;

procedure TDebugForm.CloseClick(Sender: TObject);
  begin
  self.visible := false;
  end;

procedure TDebugForm.saveListAsClick(Sender: TObject);
  var
    fileInfo: SaveFileNamesStructure;
	begin
  if debugList.items.count <= 0 then exit;
  if not GsFile_GetFileSaveInfo(kFileTypeExceptionList, kAskForFileName, 'gwi.nex', fileInfo) then exit;
  try
    self.saveExceptionListToFile(fileInfo.tempFile);
    fileInfo.writingWasSuccessful := true;
  finally
    GsFile_CleanUpAfterFileSave(fileInfo);
  end;
	end;

procedure TDebugForm.clearListClick(Sender: TObject);
  begin
  if debugList.items.count <= 0 then exit;
  debugList.items.clear;
  end;

procedure TDebugForm.helpButtonClick(Sender: TObject);
  begin
  application.helpJump('windows_Numerical_exceptions_window')
  end;

const kBetweenGap = 4;

procedure TDebugForm.FormResize(Sender: TObject);
  begin
  close.left := self.clientWidth - close.width - kBetweenGap;
  saveListAs.left := close.left;
  clearList.left := close.left;
  helpButton.left := close.left;
  with debugList do setBounds(kBetweenGap, kBetweenGap,
    close.left - kBetweenGap * 2, self.clientHeight - optionsPanel.height - kBetweenGap * 3);
  with optionsPanel do setBounds(kBetweenGap, self.clientHeight - height - kBetweenGap,
    close.left - kBetweenGap * 2, height);
  end;

procedure TDebugForm.showOnExceptionCheckBoxClick(Sender: TObject);
  begin
  Domain.menuOptions.showNumericalExceptionsWindowOnException := showOnExceptionCheckBox.checked;
  end;

procedure TDebugForm.logToFileClick(Sender: TObject);
  begin
  if not logging then
    startLogging(Domain.exeDirectory + 'errors.nex')
  else
    stopLogging;
  logging := logToFile.checked;
  end;

procedure TDebugForm.stopSimulationOnExceptionClick(Sender: TObject);
  begin
  Domain.menuOptions.stopSimulationAtEndOfDayOnException := stopSimulationOnException.checked;
  end;

end.
