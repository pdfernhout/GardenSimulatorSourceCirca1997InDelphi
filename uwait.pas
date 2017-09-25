unit Uwait;
{Copyright (c) 1997 Paul D. Fernhout and Cynthia F. Kurtz All rights reserved
http://www.gardenwithinsight.com. See the license file for details on redistribution.
=====================================================================================
uwait: A little window that just shows a short message telling the user something
is going on and they should wait. Right now this window is only used if files are
saved after the user exits the program (if they click 'Save and exit'). The form
is created at startup to make sure it is available at exit.}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls;

type
  TWaitForm = class(TForm)
    messageLabel: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure startWaitMessage(messageString: string);
procedure stopWaitMessage;

var waitForm: TWaitForm;

implementation

{$R *.DFM}

procedure startWaitMessage(messageString: string);
  var newWidth: longint;
  begin
  waitForm.messageLabel.caption := messageString;
  newWidth := waitForm.messageLabel.width + 4 * 2;
  if waitForm.width < newWidth then waitForm.width := newWidth;
  waitForm.messageLabel.invalidate;
  waitForm.show;
  application.processMessages;
  end;

procedure stopWaitMessage;
  begin
  waitForm.hide;
  end;


end.
