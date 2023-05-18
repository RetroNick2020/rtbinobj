unit cofflib;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils,DateUtils;

Function CreateCOFF(inFile,OutFile,PublicName,publicsizename : String; UseFileSizeSymbol : Boolean) : word;

implementation

type
  TCOFFHeader = packed record
    Machine: Word;
    NumberOfSections: Word;
    TimeDateStamp: LongWord;
    PointerToSymbolTable: LongWord;
    NumberOfSymbols: LongWord;
    SizeOfOptionalHeader: Word;
    Characteristics: Word;
  end;

  TCOFFSectionHeader = packed record
    Name: array[0..7] of Char;
    VirtualSize: LongWord;
    VirtualAddress: LongWord;
    SizeOfRawData: LongWord;
    PointerToRawData: LongWord;
    PointerToRelocations: LongWord;
    PointerToLinenumbers: LongWord;
    NumberOfRelocations: Word;
    NumberOfLinenumbers: Word;
    Characteristics: LongWord;
  end;

  TCOFFSymbolTable = packed record
                     PointerToName       : LongWord;  //0 by default
                     Offset              : LongWord; //first entry starts at 4 offset; there is longword size of string table
                     Value               : LongWord; //for name_size put value in this and SectionNumber = $FFFF
                     SectionNumber       : Word;  // should be 1 for .data
                     SType               : Word; // should be 0 for name
                     StorageClass        : Byte; // 2
                     AuxSymbols          : Byte; // 0
  end;

  TCOFFStringTable = packed record
                       Length : LongWord; // includes this field (4 bytes) as part of string table length  = all strings added up + 4 bytes
                       Names  : array[0..255] of char;  // just storing 2 strings - going to limit names to 20 chars each
  end;

  TCOFFSymbolTableArray = array[1..2] of TCOFFSymbolTable;
  var
  coff_header: TCOFFHeader;
  section_headers: TCOFFSectionHeader;
  section_data: TMemoryStream;
  section_data_size: LongWord;
  out_file: TFileStream;
  string_symbol_table : TCOFFSymbolTableArray;
  string_table : TCOFFStringTable;

procedure SetHeader(var H : TCOFFHeader;fsize : LongWord;UseFileSizeSymbol : boolean);
begin
  // Set the COFF header fields
  H.Machine := $14C;
  H.NumberOfSections := 1;
  H.TimeDateStamp := DateTimeToUnix(Now);
  H.PointerToSymbolTable := SizeOf(TCOFFHeader) + SizeOf(TCOFFSectionHeader)+((4+fsize+15) div 16)*16; // rounded to LongWords
  if UseFileSizeSymbol then H.NumberOfSymbols := 2 else H.NumberOfSymbols := 1;  //name and name_size
  H.SizeOfOptionalHeader := 0;
  H.Characteristics := $105;
end;

procedure setsectionheader(var SH : TCOFFSectionHeader; fsize : LongWord);
begin
  // Set the section header fields
  SH.Name := '.data'#0#0#0;  //pad with nulls
  SH.VirtualSize := 0;
  SH.VirtualAddress := 0;
  SH.SizeOfRawData := ((4+fsize+15) div 16)*16 ; // ((4 + size +15 ) div 16) *16
  SH.PointerToRawData := SizeOf(TCOFFHeader) + SizeOf(TCOFFSectionHeader); //just after the headers
  SH.PointerToRelocations := 0;
  SH.PointerToLinenumbers := 0;
  SH.NumberOfRelocations := 0;
  SH.NumberOfLinenumbers := 0;
  SH.Characteristics := $40;
end;

procedure SetStringValues(var STA : TCOFFSymbolTableArray;stable : word;offset,value : longword;sectionnumber,stype :word; StorageClass,AuxSymbols : byte);
begin
  STA[stable].PointerToName:=0;
  STA[stable].Offset:=offset;
  STA[stable].value:=value;
  STA[stable].sectionnumber:=sectionnumber;
  STA[stable].stype:=stype;
  STA[stable].storageclass:=storageclass;
  STA[stable].auxsymbols:=auxsymbols;
end;

procedure SetStringData(var STA  : TCOFFSymbolTableArray;
                        var STRT : TCOFFStringTable;
                        sname,name_size : shortstring; fsize : longword; UseFileSizeSymbol : boolean);
begin
  FillChar(STRT.names,sizeof(STRT.names),0); //clear the string tables with nulls

  SetStringValues(STA,1,4,4,1,0,2,0);
  SetStringValues(STA,2,4+length(sname)+1,fsize,$FFFF,0,2,0);
  if UseFileSizeSymbol then
    STRT.Length:=4+length(sname)+1+length(name_size)+1
  else
    STRT.Length:=4+length(sname)+1;

  Move(sname[1],STRT.names[0],length(sname));
  if UseFileSizeSymbol then  Move(name_size[1],STRT.names[length(sname)+1],length(name_size));
end;

procedure WriteHeader(var F : File;var H : TCOFFHeader);
begin
  Blockwrite(F,H, SizeOf(H));
end;

procedure WriteSectionHeader(var F : File;var SH : TCOFFSectionHeader);
begin
  Blockwrite(F,SH, SizeOf(SH));
end;

procedure WriteSectionData(var F : File;var Data; DataSize,rawsize : LongWord);      //rawsize is the longword alligned size
var
 startpad : longword =0;
 zeropadlength : longword;
 padbytes  : array[1..32] of byte;
begin
 Blockwrite(F,startpad,sizeof(startpad));
 Blockwrite(F,Data, DataSize);
 zeropadlength:=rawsize-DataSize-sizeof(startpad);
 FillChar(padbytes,sizeof(padbytes),0);
 Blockwrite(F,padbytes,zeropadlength);
end;

procedure WriteSymbolTable(var F:File;var STA : TCOFFSymbolTableArray;UseFileSizeSymbol : Boolean);
begin
  if UseFileSizeSymbol then
     Blockwrite(F,STA,sizeof(STA))
  else
     Blockwrite(F,STA[1],sizeof(STA[1]));
end;

procedure WriteStringTable(var F:File;var STRT : TCOFFStringTable);
begin
  Blockwrite(F,STRT,STRT.length);
end;

Function CreateCOFF(inFile,OutFile,PublicName,publicsizename : String; UseFileSizeSymbol : Boolean) : word;
var
 inF,OutF : File;
 error    : word;
 InFileSize : Longword;
 H  : TCOFFHeader;
 SH : TCOFFSectionHeader;
 STA  : TCOFFSymbolTableArray;
 STRT : TCOFFStringTable;
 Data : Pointer;
begin
{$I-}
  Assign(inF,inFile);
  Reset(inF,1);
  InFileSize:=FileSize(inF);
  GetMem(Data,InFileSize);
  if Data<>NIL then
  begin
    Blockread(inF,Data^,InFileSize);

    SetHeader(H,InFileSize,UseFileSizeSymbol);
    SetSectionHeader(SH,InFileSize);
    SetStringData(STA,STRT,PublicName,PublicSizeName,InFileSize,UseFileSizeSymbol);

    Assign(outF,OutFile);
    Rewrite(outF,1);

    WriteHeader(outF,H);
    WriteSectionHeader(outF,SH);
    WriteSectionData(outF,Data^,InFileSize,SH.SizeOfRawData);
    WriteSymbolTable(outF,STA,UseFileSizeSymbol);
    WriteStringTable(outF,STRT);

    close(outF);
    FreeMem(Data,InFileSize);
  end;
  close(inF);
{$I+}
  result:=IORESULT;
end;


begin

end.

