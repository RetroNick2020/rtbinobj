// Turbo Pascal

Program RtBinToObj;
  uses ObjLib;

Const
 ProgramName = 'RtBinObj - Released April 22 - 2023 By RetroNick';

procedure Usage;
begin
  writeln(programname);
  writeln('Usage: RtBinObj <source[.BIN]> <destination[.OBJ] <public name>');
end;

procedure convert;
var
 error : word;
begin
  writeln(programName);
  error:=CreateTPObj(ParamStr(1),ParamStr(2),ParamStr(3));
  if error = 0 then writeln('Converted Successfully') else writeln('Looks like we have an error# ',error);
end;

begin
  if ParamCount=3 then convert else Usage;
end.

