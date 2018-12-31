unit notification;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  
  { TNoticeFrm }

  TNoticeFrm = class(TForm)
    OkNoticeBtn: TButton;
    NotifyLabel: TLabel;
    NoticeStaticText: TStaticText;
    procedure OkNoticeBtnClick(Sender: TObject);
  private

  public

  end;

var
  NoticeFrm: TNoticeFrm;

implementation

{$R *.lfm}

{ TNoticeFrm }

procedure TNoticeFrm.OkNoticeBtnClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

end.

