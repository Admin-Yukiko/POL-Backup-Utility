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

program pol_backupper;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, POL_backup, splash, laz_synapse, HowTo, notification
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TPOLBackupper, POLBackupper);
  Application.CreateForm(TSplashScrn, SplashScrn);
  Application.CreateForm(TDocForm, DocForm);
  Application.CreateForm(TNoticeFrm, NoticeFrm);
  Application.Run;
end.

