Unit BINOBJu;

{

This unit allows TP programs to emit .OBJ-files containing data
in the same format as the BINOBJ.EXE utility does

Author: Per B. Larsen ; CIS:75470,1320

Extensive information on the internals of .OBJ-files can be found in
any Programmers Reference Manual to MS-DOS

The unit works, but it could need better I/O-error checking and handling.

Feel free to modify and use.

}

{==========================================================================}
Interface
{$I-}

Procedure BINOBJ(Var Data;DataLen:Word;FileName:String;PublicName:String);

{==========================================================================}
Implementation

Procedure BINOBJ(Var Data;DataLen:Word;FileName:String;PublicName:String);
Const
  TMOD:ARRAY[1..7] OF Byte =  ($80,$04,$00,$02,$3A,$3A,$06);
  LNAM:ARRAY[1..11] OF Byte = ($96,$08,$00,$00,$04,$43,$4F,$44,$45,$00,$43);
  SEGD:ARRAY[1..10] OF Byte = ($98,$07,$00,$28,$00,$00,$02,$01,$01,$00);
  {                                            length-             Chk}
  PUBD:ARRAY[1..5] OF Byte = ($90,$00,$00,$00,$01);     {+NAME+}
  PUBT:ARRAY[1..3] OF Byte = ($00,$00,$00);             {+CHECKSUM}
  LEDA:Byte=$A0;                                        {+RECLEN(Data+4)+}
  LEDD:ARRAY[1..3] OF Byte = ($01,$00,$00);             {+Data+CHECKSUM}
  MODE:ARRAY[1..5] OF Byte = ($8A,$02,$00,$00,$74);
Var
  UD:FILE;
  I,REST,BLOCKS,DAOF,BYTES,RBYTES,FS:Word;
  BUFFER:ARRAY[1..2048] OF Byte;
  DATABUF:ARRAY[1..63,1..1024] OF Byte absolute Data;

Procedure CHECK(Var A;L:Word);
Var
  I,C:Word;
  AR:ARRAY[1..2048] OF Byte absolute A;
BEGIN
  C:=0;
  For I := 1 to PRED(L) do
    C := (C AND $FF)+AR[I];
  C := ($FF-(C AND $FF)+1) AND $FF;
  AR[L] := C;
END;

BEGIN
  For I:=1 to length(PublicName) do
    PublicName[I] := upcase(PublicName[I]);
  FS:=DataLen;
  For I:=1 to length(FileName) do
    FileName[I]:=upcase(FileName[I]);
  IF pos('.',FileName)=0 THEN
    FileName:=FileName+'.OBJ';
  Assign(UD,FileName);
  ReWrite(UD,1);
  IF ioresult<>0 THEN
    BEGIN
      WriteLn('Cannot create .OBJ-file');
      Halt;
    END;
  Write('Writing ',FileName);
  BLockWrite(UD,TMOD,sizeof(TMOD),BYTES);
  BLockWrite(UD,LNAM,sizeof(LNAM),BYTES);
  SEGD[5]:=lo(FS);
  SEGD[6]:=hi(FS);
  CHECK(SEGD,sizeof(SEGD));
  BLockWrite(UD,SEGD,sizeof(SEGD),BYTES);
  FS:=4+length(PublicName)+sizeof(PUBT);
  PUBD[2]:=lo(FS);
  PUBD[3]:=hi(FS);
  Move(PUBD,BUFFER,sizeof(PUBD));
  Move(PublicName,BUFFER[sizeof(PUBD)+1],length(PublicName)+1);
  Move(PUBT,BUFFER[sizeof(PUBD)+length(PublicName)+2],3);
  CHECK(BUFFER,sizeof(PUBD)+length(PublicName)+5);
  DAOF:=0;
  BLockWrite(UD,BUFFER,sizeof(PUBD)+length(PublicName)+5,BYTES);
  BLOCKS := DataLen DIV 1024;
  REST := DataLen MOD 1024;
  IF REST<>0 THEN
    Inc(BLOCKS)
  ELSE
    REST:=1024;
  IF BLOCKS>63 THEN
    BEGIN
      WriteLn('Data buffer too large for BINOBJ');
      Halt;
    END;
  For I := 1 to BLOCKS do
    BEGIN
      IF I=BLOCKS THEN
        RBYTES:=REST
      ELSE
        RBYTES:=1024;
      BUFFER[1]:=LEDA;
      LEDD[2]:=lo(DAOF);
      LEDD[3]:=hi(DAOF);
      Move(LEDD,BUFFER[4],sizeof(LEDD));
      Move(DATABUF[I,1],BUFFER[7],1024);
      FS:=RBYTES+4;
      BUFFER[2]:=lo(FS);
      BUFFER[3]:=hi(FS);
      CHECK(BUFFER,RBYTES+7);
      BLockWrite(UD,BUFFER,RBYTES+7);
      Inc(DAOF,RBYTES);
      Write(DAOF:6,#8#8#8#8#8#8);
    END;
  WriteLn;
  BLockWrite(UD,MODE,sizeof(MODE),BYTES);
  Close(UD);
END;

END.
