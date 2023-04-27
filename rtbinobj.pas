// Turbo Pascal BINOBJ clone for Windows\Linux - output is exactly the same

Program RtBinToObj;
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  objlib;

Const
  ProgramName = 'RtBinObj v1.3 - Released April 26 - 2023 By RetroNick';

  CompTP = 0;
  CompTC = 1;
  CompOW16 = 2;
  CompOW32 = 3;

type
  { RtBinObj }

  TRTBinObj = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;


function GetCompModeName(Compiler : integer) : string;
begin
 result:='';
 case Compiler of CompTP:result:='Turbo Pascal BINOBJ Mode';
                  CompTC:result:='Turbo C BGIOBJ Mode';
                  CompOW16:result:='Open Watcom DOS 16bit Mode';
                  CompOW32:result:='Open Watcom DOS 32bit Mode';
  end;
end;


procedure TRTBinObj.DoRun;
var
  infile,outfile : string;
  publicName     : String;
  publicsizename : string;
  segmentname    : string;
  clname         : string;
  useFswitch     : boolean;
  CompilerMode   : integer;
  error          : word;
begin
  CaseSensitiveOptions:=false;

  // parse parameters
  if HasOption('h', 'help') or (ParamCount < 3) then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  error:=0;
  useFSwitch:=false;
  CompilerMode:=CompTP;
  infile:=paramstr(1);
  outfile:=paramstr(2);
  publicname:=paramstr(3);
  publicsizename:='';
  segmentname:='';
  clname:='';


  Case UpperCase(GetOptionValue('o')) of 'TC':CompilerMode:=CompTC;
                                         'OW16':CompilerMode:=CompOW16;
                                         'OW32':CompilerMode:=CompOW32;
  end;

  if HasOption('f','usefswitch') and (CompilerMode in [CompTC,CompOW16,CompOW32]) then usefswitch:=true;

  if GetOptionValue('ps') <> '' then
  begin
     publicsizename:=GetOptionValue('ps');
  end;

  if (GetOptionValue('sn')<>'') and (CompilerMode in [CompTC,CompOW16,CompOW32]) then
  begin
     segmentname:=GetOptionValue('sn');
  end;

  if (GetOptionValue('cn')<>'') and (CompilerMode in [CompTC,CompOW16,CompOW32]) then
  begin
     clname:=GetOptionValue('cn');
  end;

  if CompilerMode = CompTP then
  begin
    if publicsizename<>'' then
    begin
      error:=CreateTPObj(infile,outfile,publicname,publicsizename);
    end
    else
    begin
      error:=CreateTPObj(infile,outfile,publicname);
    end;
  end
  else if CompilerMode in [CompTC,CompOW16] then
  begin
    if publicsizename<>'' then
    begin
       error:=CreateTCObj(infile,outfile,publicname,publicsizename,segmentname,clname,UseFswitch);
    end
    else
    begin
       error:=CreateTCObj(infile,outfile,publicname,segmentname,clname,UseFswitch);
    end;
  end
  else if CompilerMode = CompOW32 then
  begin
    if publicsizename<>'' then
    begin
       error:=CreateOWObj(infile,outfile,publicname,publicsizename,segmentname,clname,UseFswitch);
    end
    else
    begin
        error:=CreateOWObj(infile,outfile,publicname,segmentname,clname,UseFswitch);
    end;
  end;

  if error = 0 then writeln('Converted Successfully using ',GetCompModeName(CompilerMode)) else writeln('Looks like we have an error# ',error);

  // stop program loop
  Terminate;
end;

constructor TRTBinObj.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TRTBinObj.Destroy;
begin
  inherited Destroy;
end;

procedure TRTBinObj.WriteHelp;
begin
  { add your help code here }
  writeln(programname);
  writeln('Usage: RtBinObj infile outfile public_name');
  writeln('  Optional -PS  public size name');
  writeln('           -O   OBJ Mode {TP,TC,OW16,OW32');
  writeln('           -SN  segment name');
  writeln('           -CN  class name');
  writeln('           -F   use far call');
  writeln;
  writeln('eg. RtBinObj image.xgf image.obj  image -PS imagesize');
  writeln('eg. RtBinObj image.xgf image.obj  _image -PS _imagesize -O TC'); //turbo c BGIBIN mode
  writeln('eg. RtBinObj image.xgf image.obj  _image -PS _imagesize -F -O TC');
  writeln('eg. RtBinObj image.xgf image.obj  _image -PS _imagesize -SN segname -CN classname -O TC');
end;

var
  Application: TRTBinObj;
begin
  Application:=TRTBinObj.Create(nil);
  Application.Title:='TRtBinObj';
  Application.Run;
  Application.Free;
end.





