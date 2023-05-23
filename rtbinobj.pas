// Turbo Pascal BINOBJ clone for Windows\Linux - output is exactly the same

Program RtBinToObj;
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  objlib,hunklib,bsavelib,cofflib;

Const
  ProgramName = 'RtBinObj v1.6 - Released May 22 - 2023 By RetroNick';

  CompTP = 0;
  CompTC = 1;
  CompOW16 = 2;
  CompOW32 = 3;
  CompAmigaHunk = 4;
  CompBSAVE = 5;
  CompCOFF  = 6;
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
                  CompAmigaHunk:result:='Amiga Hunk Mode';
                  CompBSAVE:result:='QuickBasic\GWBASIC BSAVE Mode';
                  CompCOFF:result:='COFF 32bit Mode';

  end;
end;


procedure TRTBinObj.DoRun;
var
  infile,outfile : string;
  publicName     : String;
  publicsizename : string;
  segmentname    : string;
  clname         : string;
  HunkName       : string;
  useFswitch     : boolean;
  CompilerMode   : integer;
  MemLoad        : longword;
  error          : word;
begin
  MemLoad:=ANY_MEM; //amiga option
  CaseSensitiveOptions:=false;
  HunkName:='ANY_MEM';

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
                                         'HUNK':CompilerMode:=CompAmigaHunk;
                                         'BSAVE':CompilerMode:=CompBSAVE;
                                         'COFF':CompilerMode:=CompCOFF;

  end;

  if HasOption('f','usefswitch') and (CompilerMode in [CompTC,CompOW16]) then usefswitch:=true;

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


  if (GetOptionValue('ml')<>'') and (CompilerMode = CompAmigaHunk) then
  begin
    if UpperCase(GetOptionValue('ml')) = 'CHIP' then
    begin
      MemLoad:=CHIP_MEM;
      HunkName:='CHIP_MEM';
    end;
    if UpperCase(GetOptionValue('ml')) = 'FAST' then
    begin
      MemLoad:=FAST_MEM;
      HunkName:='FAST_MEM';
    end;
  end;

  if (GetOptionValue('hn')<>'') and (CompilerMode = CompAmigaHunk ) then
  begin
     hunkname:=GetOptionValue('hn');
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
  end
  else if CompilerMode = CompAmigaHunk then
  begin
    if publicsizename<>'' then
    begin
       error:=CreateHunkObj(infile,outfile,publicname,publicsizename,hunkname,True,MemLoad);
    end
    else
    begin
       error:=CreateHunkObj(infile,outfile,publicname,publicsizename,hunkname,False,MemLoad);
    end;
  end
  else if CompilerMode = CompBSAVE then
  begin
    if NOT ValidBSaveSource(infile)  then
    begin
      writeln('Source File too big!');
      Terminate;
    end
    else
    begin
     error:=CreateBSaveObj(infile,outfile);
    end;
  end
  else if CompilerMode = CompCOFF then
  begin
    if publicsizename<>'' then
    begin
       error:=CreateCOFF(infile,outfile,publicname,publicsizename,True);
    end
    else
    begin
       error:=CreateCOFF(infile,outfile,publicname,publicsizename,FALSE);
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
  writeln('           -O   OBJ Mode {TP,TC,OW16,OW32,HUNK,BSAVE,COFF}');
  writeln('           -SN  segment name');
  writeln('           -CN  class name');
  writeln('           -HN  hunk name (Amiga 68k)');
  writeln('           -F   use far call');
  writeln('           -ML  Memory Load {ANY,CHIP,FAST');

  writeln;
  writeln('eg. RtBinObj image.xgf image.obj  image -PS imagesize');
  writeln('eg. RtBinObj image.xgf image.obj  _image -PS _imagesize -O TC'); //turbo c BGIBIN mode
  writeln('eg. RtBinObj image.xgf image.obj  _image -PS _imagesize -F -O TC');
  writeln('eg. RtBinObj image.xgf image.obj  _image -PS _imagesize -SN segname -CN classname -O TC');
  writeln('eg. RtBinObj image.xgf image.obj  _image -PS _imagesize -HN hunkname -O HUNK');  //Amiga
  writeln('eg. RtBinObj image.xgf image.bsv -O BSAVE');                                    //QuickBasic/GWBASIC

end;

var
  Application: TRTBinObj;
begin
  Application:=TRTBinObj.Create(nil);
  Application.Title:='TRtBinObj';
  Application.Run;
  Application.Free;
end.





