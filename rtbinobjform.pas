unit rtbinobjform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LazFileUtils, objlib;

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
    SegmentNameLabel: TLabel;
    SaveAsButton: TButton;
    EditFileName: TEdit;
    InfoLabel: TLabel;
    EditPublicName: TEdit;
    InFileButton: TButton;
    PublicLabel: TLabel;
    SaveDialog: TSaveDialog;
    ClassNameLabel: TLabel;
    procedure FarCallCheckBoxChange(Sender: TObject);
    procedure InFileButtonClick(Sender: TObject);
    procedure ObjModeRadioGroupClick(Sender: TObject);
    procedure SaveAsButtonClick(Sender: TObject);
  private
    function ValidFields : boolean;
    procedure CreateTPOBJFile;
    procedure CreateTCOBJFile;
    procedure CreateOBJFile;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.InFileButtonClick(Sender: TObject);
var
  sname : string;
begin
  // OpenDialog.Filter := 'Windows BMP|*.bmp|PNG|*.png|PC Paintbrush |*.pcx|DP-Amiga IFF LBM|*.lbm|DP-Amiga IFF BBM Brush|*.bbm|GIF|*.gif|RM RAW Files|*.raw|All Files|*.*';
  if OpenDialog.Execute then
  begin
    InfoLabel.Caption:='';
    EditFileName.Text:=OpenDialog.FileName;
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

procedure TForm1.ObjModeRadioGroupClick(Sender: TObject);
begin
  if ObjModeRadioGroup.ItemIndex = 0 then
  begin
    EditSegmentName.Enabled:=false;
    EditClassName.Enabled:=false;
    FarCallCheckbox.Enabled:=false;
  end
  else if ObjModeRadioGroup.ItemIndex = 1 then
  begin
    EditSegmentName.Enabled:=true;
    EditClassName.Enabled:=true;
    FarCallCheckbox.Enabled:=true;
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
    else if EditPublicName.Text = ''  then
    begin
      ShowMessage('Public Name cannot be empty');
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

procedure TForm1.CreateOBJFile;
begin
  if SaveDialog.Execute = false then exit;
  if ObjModeRadioGroup.ItemIndex = 0 then CreateTPObjFile else CreateTCObjFile;
end;

end.

