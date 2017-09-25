unit Usplash;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
usplash: Splash screen form. Does nothing but appear. While domain is loading, it
calls loadNextAnimationPicture as each stage in loading files is completed, and the
splash screen changes the animation picture. The pictures come from nine TImages
that are hidden from view. }

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, MPlayer;

type
  TsplashForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    copyright: TLabel;
    loadingText: TLabel;
    versionText: TLabel;
    animationImage1: TImage;
    animationImage2: TImage;
    animationImage3: TImage;
    animationImage4: TImage;
    animationImage5: TImage;
    animationImage6: TImage;
    animationImage7: TImage;
    animationImage8: TImage;
    mainPicture: TImage;
    animationImage9: TImage;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    currentAnimationPictureIndex: smallint;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure loadNextAnimationPicture;
  end;

var
	splashForm: TSplashForm;
  GardenWithInsightVersionString: string;

procedure showLoadingString(aString: string);

implementation

{$R *.DFM}

uses umusic;

const kNumAnimationPictures = 9;

procedure TSplashForm.CreateParams(var Params: TCreateParams);
  begin
  inherited CreateParams(Params);
  with Params do
    Style :=  Style or WS_BORDER or WS_EX_DLGMODALFRAME;
  end;

procedure showLoadingString(aString: string);
  begin
  if splashForm = nil then exit;
  splashForm.loadingText.caption := aString;
  splashForm.update;
  end;

procedure TsplashForm.FormCreate(Sender: TObject);
	begin
{$IFDEF WINDOWS}
  GardenWithInsightVersionString := '1.0 for Windows 3.1 (16-bit)';
{$ELSE}
  GardenWithInsightVersionString := '1.0 for Windows 95 (32-bit)';
{$ENDIF}
  versionText.caption := 'Version ' + GardenWithInsightVersionString;
  currentAnimationPictureIndex := 0;
  self.loadNextAnimationPicture;
	end;

procedure TsplashForm.loadNextAnimationPicture;
  begin
  if currentAnimationPictureIndex < kNumAnimationPictures then
    begin
    inc(currentAnimationPictureIndex);
    case currentAnimationPictureIndex of
      1: mainPicture.picture.bitmap := animationImage1.picture.bitmap;
      2: mainPicture.picture.bitmap := animationImage2.picture.bitmap;
      3: mainPicture.picture.bitmap := animationImage3.picture.bitmap;
      4: mainPicture.picture.bitmap := animationImage4.picture.bitmap;
      5: mainPicture.picture.bitmap := animationImage5.picture.bitmap;
      6: mainPicture.picture.bitmap := animationImage6.picture.bitmap;
      7: mainPicture.picture.bitmap := animationImage7.picture.bitmap;
      8: mainPicture.picture.bitmap := animationImage8.picture.bitmap;
      9: mainPicture.picture.bitmap := animationImage9.picture.bitmap;
      end;
    application.processMessages;
    end;
  end;

end.
