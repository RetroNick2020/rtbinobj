unit rtbinobjform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LazFileUtils, objlib,hunklib,bsavelib,cofflib;

Const
  ProgramName = 'RtBinObj v1.7 By RetroNick - Released December 21 - 2023';

type

  { TForm1 }

  TForm1 = class(TForm)
    FarCallCheckBox: TCheckBox;
    EditPublicSizeName: TEdit;
    EditSegmentName: TEdit;
    EditClassName: TEdit;
    OpenDialog: TOpenDialog;
    PublicSizeLabel: TLabel;
    ObjModeRadioGroup: TRadioGroup;
    AmigaMemRadioGroup: TRadioGroup;
    SegmentNameLabel: TLabel;
    SaveAsButton: TButton;
    EditFileName: TEdit;
    InfoLabel: TLabel;
    EditPublicName: TEdit;
    InFileButton: TButton;
    PublicLabel: TLabel;
    SaveDialog: TSaveDialog;
    ClassNameLabel: TLabel;
    procedure AmigaMemRadioGroupClick(Sender: TObject);
    procedure FarCallCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure InFileButtonClick(Sender: TObject);
    procedure ObjModeRadioGroupClick(Sender: TObject);
    procedure SaveAsButtonClick(Sender: TObject);
  private
    MemLoad     : Longword;

    function ValidFields : boolean;
    procedure SetPublicNames;
    procedure CreateTPOBJFile;

    procedure CreateTMTOBJFile;

    procedure CreateTCOBJFile;
    procedure CreateOBJFile;
    procedure CreateOWDOS32OBJFile;
    procedure CreateOWDos16OBJFile;
    procedure CreateAmigaHunkFile;
    procedure CreateBSaveFile;
    procedure CreateCOFFFile;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.SetPublicNames;
var
  sname : string;
begin
  if ObjModeRadioGroup.ItemIndex = 0 then
    begin
       sname:=UpperCase(ExtractFileName(ExtractFileNameWithoutExt(OpenDialog.FileName)));
       EditPublicName.Text:=sname;
       EditPublicSizeName.Text:=sname+'SIZE';
    end
    else
    begin
      sname:=LowerCase(ExtractFileName(ExtractFileNameWithoutExt(OpenDialog.FileName)));
      EditPublicName.Text:='_'+sname;
      EditPublicSizeName.Text:='_'+sname+'size';
    end;
end;

procedure TForm1.InFileButtonClick(Sender: TObject);
begin
  // OpenDialog.Filter := 'Windows BMP|*.bmp|PNG|*.png|PC Paintbrush |*.pcx|DP-Amiga IFF LBM|*.lbm|DP-Amiga IFF BBM Brush|*.bbm|GIF|*.gif|RM RAW Files|*.raw|All Files|*.*';
  if OpenDialog.Execute then
  begin
    InfoLabel.Caption:='';
    EditFileName.Text:=OpenDialog.FileName;
    SetPublicNames;
  end;
end;

procedure TForm1.FarCallCheckBoxChange(Sender: TObject);
begin
  if FarCallCheckBox.Checked  then
  begin
    EditSegmentName.Text:='';
    EditSegmentName.Enabled:=false;
    EditClassName.Text:='';
    EditClassName.Enabled:=false;
  end
  else
  begin
    EditSegmentName.Enabled:=true;
    EditClassName.Enabled:=true;
  end;
end;

procedure TForm1.AmigaMemRadioGroupClick(Sender: TObject);
begin
  case AmigaMemRadioGroup.ItemIndex of 0:begin
                                          MemLoad:=ANY_MEM;
                                          EditSegmentName.Text:='ANYMEM';
                                         end;
                                       1:begin
                                          MemLoad:=CHIP_MEM;
                                          EditSegmentName.Text:='CHIPMEM';
                                         end;
                                       2:begin
                                          MemLoad:=FAST_MEM;
                                          EditSegmentName.Text:='FASTMEM';
                                         end;
 end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption:=ProgramName;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  if High(Filenames) > 0 then
  begin
    ShowMessage('Drag only one file!');
    exit;
  end;

  if FileExists(FileNames[0]) then
  begin
    OpenDialog.FileName:=FileNames[0];
    EditFileName.Caption:=FileNames[0];
    SetPublicNames;
  end
  else
  begin
    ShowMessage('Invalid File/Directory!');
    exit;
  end;
end;

procedure TForm1.ObjModeRadioGroupClick(Sender: TObject);
begin
  if ObjModeRadioGroup.ItemIndex = 0 then
  begin
    EditPublicName.Enabled:=true;
    EditPublicSizeName.Enabled:=true;
    EditSegmentName.Enabled:=false;
    EditClassName.Enabled:=false;
    SegmentNameLabel.Caption:='Segment Name';

    AmigaMemRadioGroup.Enabled:=false;
    FarCallCheckbox.Enabled:=false;
    FarCallCheckbox.checked:=false;
  end
  else if ObjModeRadioGroup.ItemIndex = 1 then
  begin
    EditPublicName.Enabled:=true;
    EditPublicSizeName.Enabled:=true;
    EditSegmentName.Enabled:=true;
    EditClassName.Enabled:=true;
    SegmentNameLabel.Caption:='Segment Name';

    AmigaMemRadioGroup.Enabled:=false;
    FarCallCheckbox.Enabled:=true;
    FarCallCheckbox.Checked:=false;
  end
  else if ObjModeRadioGroup.ItemIndex = 2 then
  begin
    EditPublicName.Enabled:=true;
    EditPublicSizeName.Enabled:=true;
    EditSegmentName.Enabled:=true;
    EditClassName.Enabled:=true;
    SegmentNameLabel.Caption:='Segment Name';

    AmigaMemRadioGroup.Enabled:=false;
    FarCallCheckbox.Enabled:=true;
    FarCallCheckbox.Checked:=false;
  end
  else if ObjModeRadioGroup.ItemIndex = 3 then
  begin
    EditPublicName.Enabled:=true;
    EditPublicSizeName.Enabled:=true;
    EditSegmentName.Enabled:=true;
    EditClassName.Enabled:=true;
    SegmentNameLabel.Caption:='Segment Name';

    AmigaMemRadioGroup.Enabled:=false;
    FarCallCheckbox.Enabled:=false;
    FarCallCheckbox.Checked:=false;
  end
  else if ObjModeRadioGroup.ItemIndex = 4 then
  begin
    EditPublicName.Enabled:=true;
    EditPublicSizeName.Enabled:=true;
    EditSegmentName.Enabled:=true;
    EditClassName.Enabled:=false;

    SegmentNameLabel.Caption:='      Hunk Name';

    AmigaMemRadioGroup.Enabled:=true;
    FarCallCheckbox.Enabled:=false;
    FarCallCheckbox.Checked:=false;
  end
  else if ObjModeRadioGroup.ItemIndex = 5 then
  begin
    EditPublicName.Enabled:=false;
    EditPublicSizeName.Enabled:=false;
    EditSegmentName.Enabled:=false;
    EditClassName.Enabled:=false;
    SegmentNameLabel.Caption:='Segment Name';

    AmigaMemRadioGroup.Enabled:=false;
    FarCallCheckbox.Enabled:=false;
    FarCallCheckbox.Checked:=false;
  end
  else if ObjModeRadioGroup.ItemIndex = 6 then
    begin
      EditPublicName.Enabled:=true;
      EditPublicSizeName.Enabled:=true;
      EditSegmentName.Enabled:=false;
      EditClassName.Enabled:=false;
      SegmentNameLabel.Caption:='Segment Name';

      AmigaMemRadioGroup.Enabled:=false;
      FarCallCheckbox.Enabled:=false;
      FarCallCheckbox.Checked:=false;
    end
  else if ObjModeRadioGroup.ItemIndex = 7 then
     begin
       EditPublicName.Enabled:=true;
       EditPublicSizeName.Enabled:=false;
       EditSegmentName.Enabled:=false;
       EditClassName.Enabled:=false;
       SegmentNameLabel.Caption:='Segment Name';

       AmigaMemRadioGroup.Enabled:=false;
       FarCallCheckbox.Enabled:=false;
       FarCallCheckbox.Checked:=false;
     end;
end;

procedure TForm1.SaveAsButtonClick(Sender: TObject);
begin
  if ValidFields then CreateObjFile;
end;

function TForm1.ValidFields: boolean;
begin
    result:=false;
    if EditFileName.Text = '' then
    begin
      ShowMessage('No File Selected');
    end
    else if (EditPublicName.Text = '') and (ObjModeRadioGroup.ItemIndex <> 5) then
    begin
      ShowMessage('Public Name cannot be empty');
    end
    else if (EditSegmentName.Text = '') and (ObjModeRadioGroup.ItemIndex = 4) then
    begin
      ShowMessage('Hunk Name cannot be empty');
    end
    else
    begin
      result:=true;
    end;
end;

procedure TForm1.CreateTPOBJFile;
var
  error : word;
begin
  if EditPublicSizeName.Text<>'' then
     error:=CreateTPObj(OpenDialog.Filename,SaveDialog.FileName,EditPublicName.Text,EditPublicSizeName.Text)
   else
     error:=CreateTPObj(OpenDialog.Filename,SaveDialog.FileName,EditPublicName.Text);

  if error=0 then
  begin
    InfoLabel.Caption:='New Obj successfully created and saved!';
  end
  else
  begin
    InfoLabel.Caption:='Ouch it looks like we had booboo #'+IntToStr(error);
  end;
end;

procedure TForm1.CreateTMTOBJFile;
var
  error : word;
begin
  InfoLabel.Caption:='We are In correct area';
  if EditPublicSizeName.Text<>'' then
     error:=CreateTMTObj(OpenDialog.Filename,SaveDialog.FileName,EditPublicName.Text,EditPublicSizeName.Text)
   else
     error:=CreateTMTObj(OpenDialog.Filename,SaveDialog.FileName,EditPublicName.Text);

  if error=0 then
  begin
    InfoLabel.Caption:='New Obj successfully created and saved!';
  end
  else
  begin
    InfoLabel.Caption:='Ouch it looks like we had booboo #'+IntToStr(error);
  end;
end;

procedure TForm1.CreateTCOBJFile;
var
  error : word;
begin
  if EditPublicSizeName.Text<>'' then
     error:=CreateTCObj(OpenDialog.Filename,SaveDialog.FileName,EditPublicName.Text,EditPublicSizeName.Text,EditSegmentName.Text,EditClassName.Text,FarCallCheckBox.Checked)
   else
     error:=CreateTCObj(OpenDialog.Filename,SaveDialog.FileName,EditPublicName.Text,EditSegmentName.Text,EditClassName.Text,FarCallCheckBox.Checked);


  if error=0 then
  begin
    InfoLabel.Caption:='New Obj successfully created and saved!';
  end
  else
  begin
    InfoLabel.Caption:='Ouch it looks like we had booboo #'+IntToStr(error);
  end;
end;


procedure TForm1.CreateOWDOS32OBJFile;
var
  error : word;
begin
  if EditPublicSizeName.Text<>'' then
     error:=CreateOWObj(OpenDialog.Filename,SaveDialog.FileName,EditPublicName.Text,EditPublicSizeName.Text,EditSegmentName.Text,EditClassName.Text,FarCallCheckBox.Checked)
   else
     error:=CreateOWObj(OpenDialog.Filename,SaveDialog.FileName,EditPublicName.Text,EditSegmentName.Text,EditClassName.Text,FarCallCheckBox.Checked);

  if error=0 then
  begin
    InfoLabel.Caption:='New Obj successfully created and saved!';
  end
  else
  begin
    InfoLabel.Caption:='Ouch it looks like we had booboo #'+IntToStr(error);
  end;
end;

procedure TForm1.CreateOWDos16OBJFile;
var
  error : word;
begin
  //not a bug we are using the same format as Turbo C for the 16 bit - it works
  if EditPublicSizeName.Text<>'' then
     error:=CreateTCObj(OpenDialog.Filename,SaveDialog.FileName,EditPublicName.Text,EditPublicSizeName.Text,EditSegmentName.Text,EditClassName.Text,FarCallCheckBox.Checked)
   else
     error:=CreateTCObj(OpenDialog.Filename,SaveDialog.FileName,EditPublicName.Text,EditSegmentName.Text,EditClassName.Text,FarCallCheckBox.Checked);

  if error=0 then
  begin
    InfoLabel.Caption:='New Obj successfully created and saved!';
  end
  else
  begin
    InfoLabel.Caption:='Ouch it looks like we had booboo #'+IntToStr(error);
  end;
end;

procedure TForm1.CreateAmigaHunkFile;
var
  IncludeFileSize : boolean;
  error: word;
begin
  if EditPublicSizeName.Text<>'' then IncludeFileSize:=true else IncludeFileSize:=false;
  error:=CreateHunkObj(OpenDialog.Filename,SaveDialog.FileName,EditPublicName.Text,EditPublicSizeName.Text,EditSegmentName.Text,IncludeFileSize,MemLoad);
  if error=0 then
  begin
    InfoLabel.Caption:='New Hunk successfully created and saved!';
  end
  else
  begin
    InfoLabel.Caption:='Ouch it looks like we had booboo #'+IntToStr(error);
  end;
end;

procedure TForm1.CreateCOFFFile;
var
  IncludeFileSize : boolean;
  error: word;
begin
  if EditPublicSizeName.Text<>'' then IncludeFileSize:=true else IncludeFileSize:=false;
  error:=CreateCOFF(OpenDialog.Filename,SaveDialog.FileName,EditPublicName.Text,EditPublicSizeName.Text,IncludeFileSize);
  if error=0 then
  begin
    InfoLabel.Caption:='New COFF successfully created and saved!';
  end
  else
  begin
    InfoLabel.Caption:='Ouch it looks like we had booboo #'+IntToStr(error);
  end;
end;

procedure TForm1.CreateBSaveFile;
var
  error: word;
begin
  if NOT ValidBSaveSource(OpenDialog.Filename)  then
  begin
    ShowMessage('Source File too big!');
    exit;
  end;
  error:=CreateBSaveObj(OpenDialog.Filename,SaveDialog.FileName);
  if error=0 then
  begin
    InfoLabel.Caption:='New BSave Object successfully created and saved!';
  end
  else
  begin
    InfoLabel.Caption:='Ouch it looks like we had booboo #'+IntToStr(error);
  end;
end;


procedure TForm1.CreateOBJFile;
begin
  if SaveDialog.Execute = false then exit;
  case ObjModeRadioGroup.ItemIndex of 0:CreateTPObjFile;
                                      1:CreateTCObjFile;
                                      2:CreateOWDOS16OBJFile;
                                      3:CreateOWDOS32OBJFile;
                                      4:CreateAmigaHunkFile;
                                      5:CreateBSaveFile;
                                      6:CreateCOFFFile;
                                      7:CreateTMTObjFile;

  end;
end;

end.

