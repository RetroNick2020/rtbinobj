// Turbo Pascal BINOBJ clone for Windows\Linux - output is exactly the same

Program RtBinToObj;
  uses ObjLib;

Const
 ProgramName = 'RtBinObj v1.2 - Released April 24 - 2023 By RetroNick';

procedure Usage;
begin
  writeln(programname);
  writeln('Usage: RtBinObj <source[.BIN]> <destination[.OBJ] <public name> <public size name>');
  writeln('       <public size name> is optional');
  writeln;
  writeln('eg. RtBinObj image.xgf image.obj myimagename');
  writeln('    RtBinObj image.xgf image.obj myimagename myimagesize');

end;

procedure convert;
var
 error : word;
begin
  writeln(programName);
  case ParamCount of 3:error:=CreateTPObj(ParamStr(1),ParamStr(2),ParamStr(3));
                     4:error:=CreateTPObj(ParamStr(1),ParamStr(2),ParamStr(3),ParamStr(4));
  end;
  if error = 0 then writeln('Converted Successfully') else writeln('Looks like we have an error# ',error);
end;

begin
  if (ParamCount=3) or (ParamCount=4) then convert else Usage;
end.

