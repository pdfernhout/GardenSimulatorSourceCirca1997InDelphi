unit Umusic;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
umusic: This form does nothing but play the startup music. Don't even remember why
we thought it was necessary to have a separate form. Something to do with starting
the music right away, or making the music play better during startup...}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, MPlayer;

type
  TmusicForm = class(TForm)
    musicPlayer: TMediaPlayer;
  private
    { Private declarations }
  public
    { Public declarations }
  procedure playMusic(aFileName: string);
  procedure stopMusic;
  end;

var
  musicForm: TmusicForm;

implementation

{$R *.DFM}

procedure TmusicForm.playMusic(aFileName: string);
  begin
  try
  if fileExists(aFileName) then
    with MusicPlayer do
      begin
      fileName := aFileName;
      open;
      play;
      end;
  except
    { do nothing };
  end;
  end;

procedure TMusicForm.stopMusic;
  begin
  musicPlayer.stop;
  end;

end.
