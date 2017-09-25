unit Uhints;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uhints: The hint manager. Makes the long hints and aspect hints show instead of
normal short hints. Hints come from two files that are read in at startup, usually
hints.tab and asphints.tab (though the names can be changed in the ini file). Almost
all of the forms in the project descend from GsForm (in ugsform) which has a constructor that
sets all the ShowHint properties of its objects to true. This is done by
subclassing instead of setting all the ShowHint properties by hand because a) it was
quicker, and b) it can be changed back easily simply by commenting out the line that
sets all the ShowHints to true. In the main form file (ugsim) the hint catching
function calls the functions here to display the proper hint for the focused control
or aspect. The hint manager also reads the button hints from the file and sets them up into
a list of GsHint objects. Idenfication of components is by form name and component name
so two forms can have buttons with identical names (like helpButton). Aspect hints
are loaded by the aspect manager (uaspects) called by the domain at startup.}

interface

uses Forms, Classes, WinTypes, ucollect;

type
  GsHint = class
    public
    formClassName: PChar;
    componentName: PChar;
    shortHint: PChar;
    longHint: PChar;
    constructor create;
    destructor destroy; override;
    end;

  GsHintManager = class
    public
    hints: TListCollection;
    constructor create;
    destructor destroy; override;
    function hintForComponentName(var HintInfo: THintInfo; wantLongHint, showAspectHint: boolean): string;
    procedure cleanUpHint(var HintInfo: THintInfo; var hintStr: string; isAspectHint: boolean);
    procedure readHintsFromFile(aFileName: string);
    end;

procedure makeAllFormComponentsHaveHints(aForm: TForm);

implementation

uses SysUtils, Controls, StdCtrls, ExtCtrls, Buttons, Menus, Graphics, ufilertx, usupport,
  ubrowser, ubrowcom, ugrped, ugraph, usliders, uharvpnl;

{ GsHint }
constructor GsHint.create;
  begin
  formClassName := nil;
  componentName := nil;
  shortHint := nil;
  longHint := nil;
  end;

destructor GsHint.destroy;
  begin
  if formClassName <> nil then StrDispose(formClassName); formClassName := nil;
  if componentName <> nil then StrDispose(componentName); componentName := nil;
  if shortHint <> nil then StrDispose(shortHint); shortHint := nil;
  if longHint <> nil then StrDispose(longHint); longHint := nil;
  inherited destroy;
  end;

{ GsHintManager }
constructor GsHintManager.create;
  begin
  hints := nil;  {only create collection when read hints}
  end;

destructor GsHintManager.destroy;
  begin
  hints.free;
  hints := nil;
  end;

function GsHintManager.hintForComponentName(var HintInfo: THintInfo; wantLongHint, showAspectHint: boolean): string;
  var
    i, itemAtPos: longint;
    itemRectAtPos: TRect;
    hint: GsHint;
    component, owner: TComponent;
    hintFormClassName, hintComponentName: string;
    browserComponent: KfBrowserComponent;
  begin
  result := '';
  if Application.terminated then exit;
  if hints = nil then exit;
  owner := nil;
  component := TComponent(hintInfo.hintControl);
  if component = nil then
    raise Exception.create('GsHintManager.hintForComponentName: nil component');
  owner := component.owner;
  if owner = nil then
    raise Exception.create('GsHintManager.hintForComponentName: component has nil owner');
  { if harvest panel in the browser or garden window, show hint for harvested item }
  if (component is GsHarvestItemPanel) then
    begin
    result := GsHarvestItemPanel(component).hintForHarvestItemAtCursor(hintInfo, wantLongHint);
    self.cleanUpHint(hintInfo, result, false);
    exit;
    end;
  { if browser component in the browser (or slider on it), show hint for aspect pointed to by the browser component }
  browserComponent := nil;
  if (owner is TBrowserForm) and (component is KfBrowserComponent) then
    browserComponent := component as KfBrowserComponent
  else if (owner is KfBrowserComponent) and (component is KfSlider) then
    browserComponent := KfSlider(component).parent as KfBrowserComponent;
  if browserComponent <> nil then
    begin
    if (browserComponent.aspect <> nil) and (browserComponent.aspect.hint <> nil) then
      begin
      if not showAspectHint then exit;
      result := strPas(browserComponent.aspect.hint);
      self.cleanUpHint(hintInfo, result, true);
      end;
    exit;
    end;
  { if list box in the group editor, show hint for aspect mouse is over in the list }
  if (owner is TGroupEditorForm) and (component is TListBox) then
    begin
    if not showAspectHint then exit;
    result := TGroupEditorForm(owner).hintForAspectInListBox(component, hintInfo.cursorPos);
    self.cleanUpHint(hintInfo, result, true);
    { since this is for a list box, change hint if cursor moves out of the current item's rectangle }
    itemAtPos := (component as TListBox).itemAtPos(hintInfo.cursorPos, true); {true = must be on an item}
    if (itemAtPos >= 0) then
      begin
      itemRectAtPos := (component as TListBox).itemRect(itemAtPos);
      hintInfo.cursorRect := itemRectAtPos;
      end;
    exit;
    end;
  { if list box in the graph, show hint for aspect mouse is over in the list }
  if (owner is TGraphForm) and (component is TListBox) then
    begin
    if not showAspectHint then exit;
    result := TGraphForm(owner).hintForAspectInListBox(component, hintInfo.cursorPos);
    self.cleanUpHint(hintInfo, result, true);
    { since this is for a list box, change hint if cursor moves out of the current item's rectangle }
    itemAtPos := (component as TListBox).itemAtPos(hintInfo.cursorPos, true); {true = must be on an item}
    if (itemAtPos >= 0) then
      begin
      itemRectAtPos := (component as TListBox).itemRect(itemAtPos);
      hintInfo.cursorRect := itemRectAtPos;
      end;
    exit;
    end;
  { otherwise look up non-aspect hint from hint list }
  if hints.count > 0 then
    for i := 0 to hints.count - 1 do
      begin
      hint := GsHint(hints.items[i]);
      hintFormClassName := strPas(hint.formClassName);
      hintComponentName := strPas(hint.componentName);
      { check both form class and component name, so only components in one form need be uniquely named }
      if (upperCase(hintFormClassName) = upperCase(owner.className)) and
          (upperCase(hintComponentName) = upperCase(component.name)) then
        begin
        if wantLongHint then
          result := strPas(hint.longHint)
        else
          result := strPas(hint.shortHint);
        self.cleanUpHint(hintInfo, result, false);
        break;
        end;
      end;
  end;

procedure GsHintManager.cleanUpHint(var HintInfo: THintInfo; var hintStr: string; isAspectHint: boolean);
  begin
  if hintStr = '(none)' then hintStr := '';
  if isAspectHint then
    hintInfo.hintColor := clAqua;
  end;

procedure GsHintManager.readHintsFromFile(aFileName: string);
  var
    textFiler: GsTextFiler;
    hint: GsHint;
    ignore, temp: string;
    noHint: PChar;
  begin
  hints.free;
  hints := TListCollection.create;
  textFiler := GsTextFiler.createWithFileNameForReading(aFileName);
  try
  textFiler.skipRestOfLine; {skip labels line}
  while not textFiler.atEndOfFile do
    begin
    hint := GsHint.create;
    textFiler.streamPChar(hint.formClassName, '');
    textFiler.streamString(ignore, ''); {componentType not needed}
    textFiler.streamPChar(hint.componentName, '');
    textFiler.streamPChar(hint.shortHint, '');
    textFiler.streamPChar(hint.longHint, '');
    { don't add lines with no hints }
    noHint := '(none)';
    if (hint.shortHint = noHint) and (hint.longHint = noHint) then
      begin
      hint.free;
      hint := nil;
      end
    else
      hints.add(hint);
    end;
  finally
    textFiler.free;
  end;
  end;

{ from utab2asp.pas - these are the component types that have hints
type
componentConstants = (TButton, TBitBtn, TSpeedButton,
  TCheckBox, TRadioButton, TRadioGroup,
  TListBox, TComboBox,
  TEdit, TMemo,
  TPanel, THeader, TGroupBox, TScrollBox,
  TImage, TShape, TPaintBox);
  }

procedure makeAllFormComponentsHaveHints(aForm: TForm);
  var
    i: longint;
    component: TComponent;
  begin
  if aForm.componentCount > 0 then
    for i := 0 to aForm.componentCount - 1 do
      begin
      component := aForm.components[i];
      if (component is TButton) or (component is TBitBtn) or (component is TSpeedButton) or
        (component is TCheckBox) or (component is TRadioButton) or (component is TRadioGroup) or
        (component is TListBox) or (component is TComboBox) or
        (component is TEdit) or (component is TMemo) or
        (component is TPanel) or (component is THeader) or (component is TGroupBox) or (component is TScrollBox) or
        (component is TImage) or (component is TShape) or (component is TPaintBox) then
        TControl(component).showHint := true;
      end;
  end;

end.
 