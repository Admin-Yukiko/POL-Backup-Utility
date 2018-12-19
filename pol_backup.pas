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

unit POL_backup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ComCtrls, ExtCtrls, ShellAPI, StrUtils, LazFileUtils, Zipper, splash;

type

  { TPOLBackupper }

  TPOLBackupper = class(TForm)
    StartPOLBtn: TButton;
    BackupFileNameEdit: TEdit;
    DaysCheckGroup: TCheckGroup;
    Label2: TLabel;
    StatusBar1: TStatusBar;
    ToglAllDaysCheckBox: TCheckBox;
    POLDirBrowseBtn: TButton;
    POLDirEdit: TEdit;
    Label1: TLabel;
    POLDirLabel: TLabel;
    BkpLocLabel: TLabel;
    StopBtn: TButton;
    StartBtn: TButton;
    SourceDirBtn: TButton;
    DestDirBtn: TButton;
    SourceDirEdit: TEdit;
    DestDirEdit: TEdit;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    TimeLabel: TLabel;
    MondayCheckBox: TCheckBox;
    TimeEdit1: TTimeEdit;
    TuesdayCheckBox: TCheckBox;
    WednesdayCheckBox: TCheckBox;
    ThursdayCheckBox: TCheckBox;
    FridayCheckBox: TCheckBox;
    SaturdayCheckBox: TCheckBox;
    SundayCheckBox: TCheckBox;
    procedure DestDirBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure SourceDirBtnClick(Sender: TObject);
    procedure StartPOLBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure ToglAllDaysCheckBoxChange(Sender: TObject);
    procedure POLDirBrowseBtnClick(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);

  private

  public

  end;
Type
  DayNums = Array[0..6] of Integer;


const
	DayNames : array [0..6] of String  = ('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday');

var
  POLBackupper: TPOLBackupper;
  TimerRunning: Boolean = False;

implementation

/////////////////////////////////////////////////
//
// Looks for the presence of transitional
// *.ndt files in POL data folder. These are
// files created by POL during a world save.
// Once the world save is complete POL renames
// the *.ndt files to *.txt.
//
/////////////////////////////////////////////////
function NDTFilesPresent(SourceFolder: String) : Boolean;

Var
  NDTsPresent: Boolean = False;
	FoundFiles: TStringList;
Begin
	FoundFiles := FindAllFiles(SourceFolder, '*.ndt', True);
  
  // If count is greater than 0 then there are *.ndt files in the POL /data directory.
  If(FoundFiles.Count > 0) Then NDTsPresent := True
  Else NDTsPresent := False;
  Result := NDTsPresent;
end;

///////////////////////////////////////////////////////////////
//
// This procedure does the zip compression of the directory
// and directories under the target directory and places it
// in the back-up directory specified. It is called by the
// IsAScheduledDay function.
//
///////////////////////////////////////////////////////////////
procedure ZipTheDir();

var
  AZipper: TZipper;
  szPathEntry:String;
  i:Integer;
  ZEntries : TZipFileEntries;
  TheFileList:TStringList;
  RelativeDirectory:String;
  result: Boolean = False;
	SourceDir: String;
  DestinationFile: String;

begin
  SourceDir := POLBackupper.SourceDirEdit.Text;
  DestinationFile := POLBackupper.DestDirEdit.Text + '/' + POLBackupper.BackupFileNameEdit.Text + '_' + FormatDateTime('yyyy-mm-dd',Now);
  AZipper := TZipper.Create;
  try
    try
      AZipper.Filename := DestinationFile + '.zip';
      RelativeDirectory:=SourceDir;
      AZipper.Clear;
      ZEntries := TZipFileEntries.Create(TZipFileEntry);
        // Construct the path to the directory BELOW RelativeDirectory
        // If user specifies 'C:\MyFolder\Subfolder' it returns 'C:\MyFolder\'
        // If user specifies 'C:\MyFolder' it returns 'C:\'
        // If user specifies 'C:\' it returns 'C:\'
        i:=RPos(PathDelim,ChompPathDelim(RelativeDirectory));
        szPathEntry:=LeftStr(RelativeDirectory,i);

        // Use the FileUtils.FindAllFiles function to get everything (files and folders) recursively
        TheFileList:=TstringList.Create;
        try
          FindAllFiles(TheFileList, RelativeDirectory);
          for i:=0 to TheFileList.Count -1 do
          begin
            // Make sure the RelativeDirectory files are not in the root of the ZipFile
            ZEntries.AddFileEntry(TheFileList[i],CreateRelativePath(TheFileList[i],szPathEntry));
          end;
        finally
          TheFileList.Free;
        end;

      if (ZEntries.Count > 0) then
        AZipper.ZipFiles(ZEntries);
      except
        On E: EZipError do
          E.CreateFmt('Zipfile could not be created%sReason: %s', [LineEnding, E.Message])
      end;
    result := True;
  finally
    FreeAndNil(ZEntries);
    AZipper.Free;
  end;
end;

///////////////////////////////////////////////////////////////
//
// This function is called by WaitForTime and returns if the
// checkbox for a day is not checked. If it is checked then
// it waits for the proper time. It then calls the
// NDTFilesPresent function until there are no longer any
// *.ndt files present in the directory to be backed-up.
// Then it calls ZipTheDir.
//
///////////////////////////////////////////////////////////////
Function IsAScheduledDay(DayCheck: Boolean): Boolean;

Var
  CurrentTime: Integer;
  BackupTime: Integer;
Begin
  // If the checkbox for the current day is not checked then exit back to WaitForTime
  If(Not DayCheck) Then Exit;
  
  // Get the current time in hours and minutes.
  CurrentTime := Numb2Dec(FormatDateTime('hhnn',Now), 10);
  
  // Get the backup time from TimeEdit1, remove the semicolon, and convert it to a decimal integer.
  BackupTime := Numb2Dec(ReplaceStr(POLBackupper.TimeEdit1.Text, ':', ''), 10);
  
  // If it is already past the backup time then return to WaitForTime.
  // This is necessary if the user starts the backup timer on a day she has selected
  // but later than the time scheduled backup time.
  If(CurrentTime > BackupTime) Then Exit;
  
  // Now we wait while the current time is not equal to the backup time.
  While(Numb2Dec(FormatDateTime('hhnn',Now), 10) <> BackupTime) Do
  Begin
    If(Not TimerRunning) Then Exit;
    Application.ProcessMessages;
    Sleep(100);
  end;
  
  // Ok. The current time equals the backup time.
  // Check for *.ndt files. When they are gone exit the loop.
  While(NDTFilesPresent(POLBackupper.SourceDirEdit.Text + '/')) Do
    Begin
      Sleep(50);
      Application.ProcessMessages;
    end;
	
  //Do the backup.
  ZipTheDir();

end;

///////////////////////////////////////////////////////////////
// procedure WaitForTime(BackupTime: String);
// BackupTime is the time set by the user to backup the POL
// directory.
//
// TimerRunning is a boolean that is set to true by the Start Timer
// button just prior to this procedure being called. The Stop Timer
// button sets it to false.
//
// This is truly the core function, along with its support
// procedures and functions, that does the bulk of the work.
//
///////////////////////////////////////////////////////////////

procedure WaitForTime();

Var
  // String for holding the current day.
  CurrentDay: String;

Begin
  While(TimerRunning) Do
    Begin
		  CurrentDay := FormatDateTime('dddd',Now);

      // Goes through the days looking for the next one that has its checkbox checked.
		  Case(CurrentDay) of
  	    'Sunday':			IsAScheduledDay(POLBackupper.SundayCheckBox.Checked);
  	    'Monday':			IsAScheduledDay(POLBackupper.MondayCheckBox.Checked);
  	    'Tuesday':		IsAScheduledDay(POLBackupper.TuesdayCheckBox.Checked);
  	    'Wednesday':	IsAScheduledDay(POLBackupper.WednesdayCheckBox.Checked);
  	    'Thursday':		IsAScheduledDay(POLBackupper.ThursdayCheckBox.Checked);
  	    'Friday':			IsAScheduledDay(POLBackupper.FridayCheckBox.Checked);
  	    'Saturday':		IsAScheduledDay(POLBackupper.SaturdayCheckBox.Checked);
      end;
      
      If(Not TimerRunning) Then Exit;
      
      // Here we wait for the current day to change to the next day.
      Repeat
        Sleep(100);
        If(Not TimerRunning) Then Exit;
        Application.ProcessMessages;
      until (FormatDateTime('dddd',Now) <> CurrentDay);
			Sleep(100);
    end; // While...
end;

{$R *.lfm}

{ TPOLBackupper }

///////////////////////////////////////////////////////////////
//
// Bulk enable or disable checkboxes and certain other things
// during the time the scheduler is active.
// EnableThem - True to Enable, False to disable.
//
///////////////////////////////////////////////////////////////
procedure EnableDaysCheckBoxes(EnableThem: Boolean);

Begin
  POLBackupper.SundayCheckBox.Enabled:=EnableThem;
  POLBackupper.MondayCheckBox.Enabled:=EnableThem;
  POLBackupper.TuesdayCheckBox.Enabled:=EnableThem;
  POLBackupper.WednesdayCheckBox.Enabled:=EnableThem;
  POLBackupper.ThursdayCheckBox.Enabled:=EnableThem;
  POLBackupper.FridayCheckBox.Enabled:=EnableThem;
  POLBackupper.SaturdayCheckBox.Enabled:=EnableThem;
	POLBackupper.ToglAllDaysCheckBox.Enabled:=EnableThem;
  POLBackupper.SourceDirBtn.Enabled:=EnableThem;
  POLBackupper.DestDirBtn.Enabled:=EnableThem;
  POLBackupper.StartBtn.Enabled:=EnableThem;
  POLBackupper.TimeEdit1.Enabled:=EnableThem;
  POLBackupper.BackupFileNameEdit.Enabled:=EnableThem;

end;

///////////////////////////////////////////////////////////////
//
// The procedure for the POL directory browse butto.
// Used to select a POL server location should the user want
// to run POL from this program.
//
// It ensures the user has selected a valid POL directory.
//
///////////////////////////////////////////////////////////////
procedure TPOLBackupper.POLDirBrowseBtnClick(Sender: TObject);

Var
  JunkString: String;

begin
  SelectDirectoryDialog1.Title:='Choose your POL directory';
  if SelectDirectoryDialog1.Execute then
    JunkString := SelectDirectoryDialog1.FileName;
    If(FileExists(JunkString + '/pol.exe')) Then
      POLDirEdit.Text := JunkString
    Else ShowMessage('You must select a valid POL directory. Either you accidentally chose the wrong directory or you might ' + Chr(13) + Chr(10) +
                      'be missing one of three *.exe files that the Configurator uses to validate your POL location. Paths are relative to' + Chr(13) + Chr(10) +
                      'your POL installation. These are /pol.exe, /uoconvert.exe, and /poltool.exe. ');


end;

///////////////////////////////////////////////////////////////
//
// The procedure for the Start Backup button.
//
///////////////////////////////////////////////////////////////
procedure TPOLBackupper.StartBtnClick(Sender: TObject);

Var
  JunkPosition: Integer;								// A temporary variable.

begin
    If(SourceDirEdit.Text = '') Then
    Begin
      ShowMessage('Please select the directory you wish to back-up.');
      Exit;
    end;
    If(DestDirEdit.Text = '') Then
    Begin
      ShowMessage('Please select a back-up location.');
      Exit;
    end;
	// Set TimerRunning to True.
  TimerRunning := True;
  // Displays the text in the status bar.
  StatusBar1.SimpleText:='    Status of the Backup Scheduler:    Running.';
  // Disables various things whilst the backup timer is running.
  SourceDirBtn.Enabled:=False;
  DestDirBtn.Enabled:=False;
  StartBtn.Enabled:=False;
  TimeEdit1.Enabled:=False;
  BackupFileNameEdit.Enabled:=False;
  EnableDaysCheckBoxes(False);
  // The WaitForTime function is called.
  WaitForTime();
end;

///////////////////////////////////////////////////////////////
//
// Procedure for the browse button to choose the directory
// to backup.
//
///////////////////////////////////////////////////////////////
procedure TPOLBackupper.SourceDirBtnClick(Sender: TObject);

Var
	JunkString: String;

begin
  SelectDirectoryDialog1.Title:='Choose the directory you want to back-up:';
  if SelectDirectoryDialog1.Execute then SourceDirEdit.Text:=SelectDirectoryDialog1.FileName;
end;

///////////////////////////////////////////////////////////////
//
// The procedure that will start the POL server, provided the
// user has selected a valid POL directory.
//
///////////////////////////////////////////////////////////////
procedure TPOLBackupper.StartPOLBtnClick(Sender: TObject);
begin
  If(POLDirEdit.Text = '') Then
  Begin
    ShowMessage('Please select a valid POL directory.');
  end
  Else ShellExecute(0,nil, PChar('cmd'),PChar('/c ' + 'cd /D ' + POLDirEdit.Text + ' && ' + 'pol.exe'),nil,1);
end;

///////////////////////////////////////////////////////////////
//
// Procedure to stop the backup timer.
//
///////////////////////////////////////////////////////////////
procedure TPOLBackupper.StopBtnClick(Sender: TObject);
begin
  TimerRunning := False;
  StatusBar1.SimpleText:='    Status of the Backup Scheduler:    Stopped.';
  SourceDirBtn.Enabled:=True;
  DestDirBtn.Enabled:=True;
  StartBtn.Enabled:=True;
  TimeEdit1.Enabled:=True;
  BackupFileNameEdit.Enabled:=True;
  EnableDaysCheckBoxes(True);

end;

///////////////////////////////////////////////////////////////
//
// Procedure to toggle all day's checkboxes.
//
///////////////////////////////////////////////////////////////
procedure TPOLBackupper.ToglAllDaysCheckBoxChange(Sender: TObject);
begin
  If(ToglAllDaysCheckBox.Checked) Then
  Begin
    SundayCheckBox.Checked:=True;
    MondayCheckBox.Checked:=True;
    TuesdayCheckBox.Checked:=True;
    WednesdayCheckBox.Checked:=True;
    ThursdayCheckBox.Checked:=True;
    FridayCheckBox.Checked:=True;
    SaturdayCheckBox.Checked:=True;
  end
  Else
  Begin
    SundayCheckBox.Checked:=False;
    MondayCheckBox.Checked:=False;
    TuesdayCheckBox.Checked:=False;
    WednesdayCheckBox.Checked:=False;
    ThursdayCheckBox.Checked:=False;
    FridayCheckBox.Checked:=False;
    SaturdayCheckBox.Checked:=False;
  end;
end;

///////////////////////////////////////////////////////////////
//
// Procedure for the browse button to choose a backup
// location.
//
///////////////////////////////////////////////////////////////
procedure TPOLBackupper.DestDirBtnClick(Sender: TObject);
Var
	JunkString: String;

begin
  SelectDirectoryDialog1.Title:='Choose the destination directory for the back-up:';
  if SelectDirectoryDialog1.Execute then DestDirEdit.Text:=SelectDirectoryDialog1.FileName;
end;

///////////////////////////////////////////////////////////////
//
// The procedure called when the program is closed.
//
///////////////////////////////////////////////////////////////
procedure TPOLBackupper.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Halt;
end;

///////////////////////////////////////////////////////////////
//
// This is called when the program is executed.
// It displays the splash screen.
//
///////////////////////////////////////////////////////////////
procedure TPOLBackupper.FormShow(Sender: TObject);
begin
  Sleep(100);
  SplashScrn.Show;
  SplashScrn.Update;
  Application.ProcessMessages;
  Sleep(5000);
	SplashScrn.Close;
end;

end.

