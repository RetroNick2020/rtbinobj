unit rtbinobjform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,LazFileUtils,objlib;

type

  { TForm1 }

  TForm1 = class(TForm)
    OpenDialog: TOpenDialog;
    SaveAsButton: TButton;
    EditFileName: TEdit;
    InfoLabel: TLabel;
    EditPublicName: TEdit;
    InFileButton: TButton;
    PublicLabel: TLabel;
    SaveDialog: TSaveDialog;
    procedure InFileButtonClick(Sender: TObject);
    procedure SaveAsButtonClick(Sender: TObject);
  private
    function ValidFields : boolean;
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
    sname:=UpperCase(ExtractFileName(ExtractFileNameWithoutExt(OpenDialog.FileName)));
    EditPublicName.Text:=sname;
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

procedure TForm1.CreateOBJFile;
var
  error : word;
begin
  if SaveDialog.Execute = false then exit;
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

end.

