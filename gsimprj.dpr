program Gsimprj;

uses
  Forms,
  Usplash in 'USPLASH.PAS' {splashForm},
  Ugsim in 'UGSIM.PAS' {GardenForm},
  Ubrowser in 'UBROWSER.PAS' {BrowserForm},
  Udomain in 'UDOMAIN.PAS',
  Utools in 'UTOOLS.PAS',
  Ucollect in 'UCOLLECT.PAS',
  Uprofile in 'UPROFILE.PAS',
  Ufiler in 'UFILER.PAS',
  Ugraph in 'UGRAPH.PAS' {GraphForm},
  Ugrped in 'UGRPED.PAS' {GroupEditorForm},
  Upatarea in 'UPATAREA.PAS' {SoilPatchArea},
  Usimop in 'USIMOP.PAS' {SimulationOptionsForm},
  Utempl in 'UTEMPL.PAS' {TemplatesForm},
  Uexcept in 'UEXCEPT.PAS',
  Umconsts in 'UMCONSTS.PAS',
  Ucommand in 'UCOMMAND.PAS',
  Ugscom in 'UGSCOM.PAS',
  Uaspects in 'UASPECTS.PAS',
  Ubrowcom in 'UBROWCOM.PAS',
  Ugroups in 'UGROUPS.PAS',
  ueg in 'UEG.PAS',
  Uegarden in 'UEGARDEN.PAS',
  ueorgmat in 'UEORGMAT.PAS',
  Uep in 'UEP.PAS',
  Ueplant in 'UEPLANT.PAS',
  Ueq in 'UEQ.PAS',
  Ueqh in 'UEQH.PAS',
  Uesoil in 'UESOIL.PAS',
  Ueutils in 'UEUTILS.PAS',
  Ueweath in 'UEWEATH.PAS',
  Umodel in 'UMODEL.PAS',
  Udplant in 'UDPLANT.PAS',
  uunits in 'UUNITS.PAS',
  Uturt3d in 'UTURT3D.PAS',
  Usupport in 'USUPPORT.PAS',
  ucursor in 'UCURSOR.PAS',
  Udebug in 'UDEBUG.PAS' {DebugForm},
  Ubrowrl in 'UBROWRL.PAS',
  Ubrowrla in 'UBROWRLA.PAS',
  Ubrowint in 'UBROWINT.PAS',
  Ubrowboo in 'UBROWBOO.PAS',
  Ubrowscv in 'UBROWSCV.PAS',
  Ugrafcom in 'UGRAFCOM.PAS',
  Ubrowtri in 'UBROWTRI.PAS',
  Utooledt in 'UTOOLEDT.PAS' {toolEditorForm},
  Uplotopt in 'UPLOTOPT.PAS' {PlotOptionsForm},
  Ubrowcol in 'UBROWCOL.PAS',
  Ubrowtdo in 'UBROWTDO.PAS',
  Ubrowlis in 'UBROWLIS.PAS',
  Udsoil in 'UDSOIL.PAS',
  Usound in 'USOUND.PAS',
  Uboundch in 'UBOUNDCH.PAS' {BoundsChangeForm},
  Udweath in 'UDWEATH.PAS',
  Unotes in 'UNOTES.PAS' {NotesForm},
  Uebag in 'UEBAG.PAS',
  Utempman in 'UTEMPMAN.PAS',
  Uicon in 'UICON.PAS',
  Urandom in 'URANDOM.PAS',
  Udate in 'UDATE.PAS',
  Udppart in 'UDPPART.PAS',
  Udpfruit in 'UDPFRUIT.PAS',
  Udpinflo in 'UDPINFLO.PAS',
  Udpinter in 'UDPINTER.PAS',
  Udpmeris in 'UDPMERIS.PAS',
  Udpleaf in 'UDPLEAF.PAS',
  Uharvest in 'UHARVEST.PAS' {HarvestForm},
  Ulogvar in 'ULOGVAR.PAS',
  Utitlech in 'UTITLECH.PAS' {GraphTitleForm},
  Udisplay in 'UDISPLAY.PAS' {DisplayOptionsForm},
  Ureseed in 'URESEED.PAS' {ReseedingOptionsForm},
  Uharved in 'UHARVED.PAS' {HarvestItemTemplateEditorForm},
  Uharvpnl in 'UHARVPNL.PAS',
  uharvprt in 'UHARVPRT.PAS',
  Uhvrpted in 'UHVRPTED.PAS' {HarvestReportEditorForm},
  Ubrowhar in 'UBROWHAR.PAS',
  Uclasses in 'UCLASSES.PAS',
  Uiconch in 'UICONCH.PAS' {IconChooserForm},
  Usstream in 'USSTREAM.PAS',
  GRAPHICS,
  Ubackdrp in 'UBACKDRP.PAS' {BackdropChooserForm},
  Umovtemp in 'UMOVTEMP.PAS' {TemplateMover},
  Utlpmed in 'UTLPMED.PAS' {ToolParamListEditorForm},
  Ubitmap in 'UBITMAP.PAS',
  Usplant in 'USPLANT.PAS',
  Uesoilop in 'UESOILOP.PAS',
  Utdoch in 'UTDOCH.PAS' {ThreeDObjectChooserForm},
  uasp_dpl in 'UASP_DPL.PAS',
  uasp_pla in 'UASP_PLA.PAS',
  Uasp_s1 in 'UASP_S1.PAS',
  uasp_wea in 'UASP_WEA.PAS',
  uasp_s2 in 'UASP_S2.PAS',
  Ugetepic in 'UGETEPIC.PAS' {EpicImportForm},
  Udefault in 'UDEFAULT.PAS',
  Usliders in 'USLIDERS.PAS',
  Uabout in 'UABOUT.PAS' {AboutBoxForm},
  Uestruct in 'UESTRUCT.PAS',
  Uexit in 'UEXIT.PAS' {ExitForm},
  Uwait in 'UWAIT.PAS' {WaitForm},
  Ulayers in 'ULAYERS.PAS' {layerOptionsForm},
  Uwindman in 'UWINDMAN.PAS',
  Ufilertx in 'UFILERTX.PAS',
  Uduplic in 'UDUPLIC.PAS' {duplicateObjectForm},
  Uhardcd in 'UHARDCD.PAS',
  Umusic in 'UMUSIC.PAS' {musicForm},
  utr55 in 'UTR55.PAS',
  uasp_s3 in 'UASP_S3.PAS',
  Uderopt in 'UDEROPT.PAS' {AspectDerivationForm},
  Ugsform in 'UGSFORM.PAS',
  Uhints in 'UHINTS.PAS',
  Utdoed in 'UTDOED.PAS' {tdoEditorForm},
  Utstream in 'UTSTREAM.PAS',
  Utimelin in 'UTIMELIN.PAS' {timelineForm};

{$R *.RES}

  var
    successfulCreation: boolean;

  begin
  splashForm := nil;
  musicForm := nil;
  GardenForm := nil;
  BrowserForm := nil;
  GraphForm := nil;
  HarvestForm := nil;
  Domain := nil;

  splashForm := TSplashForm.Create(Application);
  splashForm.show;
  splashForm.update;
  splashForm.bringToFront;  
  musicForm := TMusicForm.create(Application);
  musicForm.hide;

  successfulCreation := GsDomain.createDefault;
  splashForm.hide;
  splashForm.free;
  splashForm := nil;  

  if successfulCreation then
    begin
  	Application.Title := 'Garden with Insight (TM)';
    Application.HelpFile := 'gwi.hlp';
    if GardenForm = nil then Application.CreateForm(TGardenForm, GardenForm);
  if BrowserForm = nil then Application.CreateForm(TBrowserForm, BrowserForm);
    if GraphForm = nil then Application.CreateForm(TGraphForm, GraphForm);
    if HarvestForm = nil then Application.CreateForm(THarvestForm, HarvestForm);
    { do not stream - modal but special use }
    Application.CreateForm(TsoilPatchAreaForm, soilPatchAreaForm);
    Application.CreateForm(TWaitForm, WaitForm);
    Application.CreateForm(TAboutBoxForm, AboutBoxForm);
    Application.CreateForm(TDebugForm, DebugForm);
    Application.CreateForm(TExitForm, ExitForm);
    globalFirstUpdateForNewDomain := true;
    GardenForm.updateForNewDomain;
    globalFirstUpdateForNewDomain := false;
    end;

  if successfulCreation then
  	Application.run
  else
    Application.terminate;

  musicForm.free;
  musicForm := nil;

  GsDomain.destroyDefault;
  KfTurtle.defaultFree;
  end.
