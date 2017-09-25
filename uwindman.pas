unit Uwindman;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uwindman: Window manager. This class creates the forms that are to be created when
the domain is streamed in (if they are not created already, that is, if this is
the first garden opened in the session); and it manages the streaming of data
about the garden window, browser, graph window, and harvest window during file
streaming. Information saved is mainly positional (place on screen, size,
position of splitter, etc) and what is selected in the forms (the object
in the browser, the current tool, etc). Note that since the forms are not
streamable objects, the window manager acts as a proxy to the filer (ufiler)
to stream the information for them.}

interface

uses Forms, Classes, ufiler, uclasses;

type

GsWindowManager = class(GsStreamableObject)
  public
  procedure streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord); override;
  procedure classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord); override;
  end;

implementation

uses ugsim, ugraph, ubrowser, uharvest;

procedure GsWindowManager.classAndVersionInformation(var cvir: GsClassAndVersionInformationRecord);
  begin
  cvir.classNumber := KGsWindowManager;
  cvir.versionNumber := 0;
  cvir.additionNumber := 0;
  end;

procedure GsWindowManager.streamDataWithFiler(filer: GsFiler; const cvir: GsClassAndVersionInformationRecord);
  begin
  inherited streamDataWithFiler(filer, cvir);
  { garden - must create garden form before other forms, or it will not be the main form }
  if (filer.isReading) and (GardenForm = nil) then
    Application.CreateForm(TGardenForm, GardenForm);
  if GardenForm <> nil then
    GardenForm.streamInfoWithFiler(filer, cvir);
  { graph }
  if (filer.isReading) and (GraphForm = nil) then
    Application.CreateForm(TGraphForm, GraphForm);
  if GraphForm <> nil then
    GraphForm.streamInfoWithFiler(filer, cvir);
  { browser }
  if (filer.isReading) and (BrowserForm = nil) then
    Application.CreateForm(TBrowserForm, BrowserForm);
  if BrowserForm <> nil then
    BrowserForm.streamInfoWithFiler(filer, cvir);
  { harvest }
  if (filer.isReading) and (HarvestForm = nil) then
    Application.CreateForm(THarvestForm, HarvestForm);
  if HarvestForm <> nil then
    HarvestForm.streamInfoWithFiler(filer, cvir);
  end;

end.
