///////////////////////////////////////////////////////////////////////////////////
//
//    POL Backupper is Copyright 2018 by Dan Gardner.
//    Contact information: hopelives@outlook.com
//
//
//     This file is part of POL Backupper.
//
//    POL Backupper is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    POL Backupper is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with POL Backupper.  If not, see <https://www.gnu.org/licenses/>.
///////////////////////////////////////////////////////////////////////////////////

unit splash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type
  
  { TSplashScrn }

  TSplashScrn = class(TForm)
    Image1: TImage;
  private

  public

  end;

var
  SplashScrn: TSplashScrn;

implementation

{$R *.lfm}

end.

