unit Uabout;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uabout: About box form (modal). Displays a picture and a few text labels with an OK button. }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls;

type
  TAboutBoxForm = class(TForm)
    Panel1: TPanel;
    Label7: TLabel;
    copyright: TLabel;
    versionText: TLabel;
    Label8: TLabel;
    Panel2: TPanel;
    Image5: TImage;
    Label2: TLabel;
    okButton: TButton;
    label9: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBoxForm: TAboutBoxForm;

implementation

uses usplash;

{$R *.DFM}

procedure TAboutBoxForm.FormCreate(Sender: TObject);
  begin
  versionText.caption := 'Version ' + GardenWithInsightVersionString;
  end;

end.

