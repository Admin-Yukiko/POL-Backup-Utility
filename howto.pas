unit HowTo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  
  { TDocForm }

  TDocForm = class(TForm)
    Memo1: TMemo;
  private

  public

  end;

var
  DocForm: TDocForm;

implementation

{$R *.lfm}

end.

