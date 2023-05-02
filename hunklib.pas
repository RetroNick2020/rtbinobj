unit hunklib;
{$PACKRECORDS 1}

Interface

Const
 HUNK_UNIT    = $03E7;  //lower word
 HUNK_NAME    = $03E8;
 HUNK_DATA    = $03EA;
 HUNK_EXT     = $03EF;
 HUNK_END     = $03F2;

 CHIP_MEM = $40000000;
 FAST_MEM = $80000000;
 ANY_MEM  = $00000000;

procedure WriteHunk(var F : File; hunk : longword);
Procedure WriteHunkUnit(var F : File);
Procedure WriteHunkName(var F : File; name : string);
Function WriteHunkData(var F : File;MemLoad : longword;Filename : string;IncludeSize : Boolean) : longword;
Procedure WriteHunkExtSize(var F : File;SymbolName : string; filesize : longword);
Procedure WriteHunkExt(var F : File;SymbolName : string);
Procedure WriteHunkEnd(var F : File);
Function CreateHunkObj(infile,outfile,DataName,DataSizeName,Hunkname : string;IncludeFileSize : Boolean;MemLoad  : longword) : word;


Implementation

Function LongToLE(myLongWord : LongWord) : LongWord;
var
  Temp : array[1..4] of Byte;
begin
  Move(myLongWord,Temp,sizeof(Temp));
  LongToLE := (Temp[1] shl 24) + (Temp[2] shl 16) + (Temp[3] shl 8) + Temp[4];
end;

Function WordToLE(myWord : Word) : Word; // if its little endian to will become big endian vice versa
begin
  WordToLE:=LO(myWord) SHL 8 + HI(myWord);
end;

procedure WriteHunk(var F : File; hunk : longword);
begin
  Blockwrite(F,LongToLE(hunk),sizeof(hunk));
end;

Procedure WriteHunkUnit(var F : File);
begin
  WriteHunk(F,HUNK_UNIT);
  WriteHunk(F,$00000000);
end;

Procedure WriteHunkName(var F : File; name : string);
var
 qname   : string;
 qLen,c  : longword;
 SBuf    : array[1..20] of CHAR;
begin
 qname:=Copy(name,1,20);
 qLen:=(Length(qname)+3) Div 4;

 WriteHunk(F,HUNK_NAME);
 WriteHunk(F,qLen);

 FillChar(Sbuf,sizeof(SBuf),0);
 for c:=1 to  Length(qname) do
 begin
   SBuf[c]:=qname[c];
 end;
 Blockwrite(F,SBuf,qLen*4);
end;

Function WriteHunkData(var F : File;MemLoad : longword;Filename : string;IncludeSize : Boolean) : longword;
var
 DataFile   : File;
 size,qsize : longword;
 DataPtr    : Pointer;
begin
 WriteHunk(F,HUNK_DATA+MemLoad);
 Assign(DataFile,Filename);
 Reset(DataFile,1);
 size:=FileSize(DataFile);
 qsize:=((size+3) div 4)*4;

 if IncludeSize then WriteHunk(F,qsize div 4+1) else WriteHunk(F,qsize div 4);

 GetMem(DataPtr,qsize);
 FillChar(DataPtr^,qsize,0);
 If DataPtr<>NIL then
 begin
    Blockread(DataFile,DataPtr^,size);
    Blockwrite(F,DataPtr^,qsize);
    FreeMem(DataPtr,qsize);
 end;
 close(DataFile);
 if IncludeSize then WriteHunk(F,size);
 WriteHunkData:=qsize;
end;

Procedure WriteHunkExt(var F : File;SymbolName : string);
const
 Symbol : longword = $01000000;
var
 qLen,c : longword;
 qSymbolName : string;
 SBuf    : array[1..20] of CHAR;

begin
 qSymbolName:=Copy(SymbolName,1,20);
 qLen:=(Length(qSymbolName)+3) Div 4;

 WriteHunk(F,HUNK_EXT);
 WriteHunk(F,Symbol+qLen);

// qfilesize:=((filesize+3) div 4)*4;
 FillChar(Sbuf,sizeof(SBuf),0);
 for c:=1 to  Length(qSymbolName) do
 begin
   SBuf[c]:=qSymbolName[c];
 end;
 Blockwrite(F,SBuf,qLen*4);
 WriteHunk(F,$00000000);
end;

Procedure WriteHunkExtSize(var F : File;SymbolName : string; filesize : longword);
const
 Symbol : longword = $01000000;
var
 qLen,qfilesize,c : longword;
 qSymbolName : string;
 SBuf    : array[1..20] of CHAR;

begin
 qSymbolName:=Copy(SymbolName,1,20);
 qLen:=(Length(qSymbolName)+3) Div 4;

// WriteHunk(HUNK_EXT);  //not needed if proceded by previous HUNK_EXT definition in WriteHunkExt
 WriteHunk(F,Symbol+qLen);

 qfilesize:=((filesize+3) div 4)*4;
 FillChar(Sbuf,sizeof(SBuf),0);
 for c:=1 to  Length(qSymbolName) do
 begin
   SBuf[c]:=qSymbolName[c];
 end;
 Blockwrite(F,SBuf,qLen*4);
 WriteHunk(F,qfilesize);
end;

Procedure WriteHunkEnd(var F : File);
begin
 WriteHunk(F,$00000000);
 WriteHunk(F,HUNK_END);
end;


Function CreateHunkObj(infile,outfile,DataName,DataSizeName,Hunkname : string;IncludeFileSize : Boolean;MemLoad  : longword) : word;
var
 outF : File;
 size : longword;
begin
{$I-}
Assign(outF,outFile);
Rewrite(outF,1);
WriteHunkUnit(outF);
WriteHunkName(outF,HunkName);
size:=WriteHunkData(outF,MemLoad,inFile,IncludeFileSize);
WriteHunkExt(outF,DataName);
if IncludeFileSize then
begin
  WriteHunkExtSize(outF,DataSizeName,size);
end;
WriteHunkEnd(outF);
close(outF);
result:=IORESULT;
{$I+}
end;

begin
end.
