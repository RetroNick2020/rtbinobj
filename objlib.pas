unit objlib;
{PACKED RECORDS 1}
{$H-}
interface
type
 TPubDefStrRec = Packed Record
                   StringLength : byte;
                   PubLicName   : String;
                   PublicOffset : word;
                   TypeIndex    : byte;
                 end;

procedure Write_THeadr(var F : File;name : string);
procedure Write_LNames(var F : File;names : string);
procedure Write_SegDef(var F : File; flags : byte; Segmentlength: word;
                           SegNameIndex: byte;
                           ClassNameIndex: byte;
                           OverlayNameIndex: byte);

procedure Write_PubDef(var F : File; BaseGroupIndex: byte; BaseSegmentIndex: byte; PublicName: string;
                           PublicOffset: word; TypeIndex: byte);
procedure Write_GRPDEF(var F : File; GroupNameIndex: byte; SegmentDefinition: byte);

procedure Write_SegmentData(var F : File; SegmentIndex: byte; EnumeratedDataOffset: word; dataBytes: PByte; dataLength: word);

function GetFileSize(filename : string) : longword;
Procedure Write_FileContents(var F : File;filename : string;segIdx: byte; dataOffset: word);
procedure Write_ModEnd(var F : File);

procedure Write_PubDefRecords(var F : File; BaseGroupIndex: byte; BaseSegmentIndex: byte;var rdata;rcount : word);
Function CreateTPObj(infile,outfile,publicname : string) : word;
Function CreateTPObj(infile,outfile,publicname,publicsizename : string) : word;

implementation

type
 IdLengthRec = packed record
                 id     : byte;
                 length : word;
               end;

 TSegDefRec = Packed Record
                SegmentAttributes: byte;
                SegmentLength    : word;
                SegmentNameIndex : byte;
                ClassNameIndex   : byte;
                OverlayNameIndex : byte;
              end;


Function GetCheckSum(id : byte;Var data ;Length:Word) : word;   //works
Var
 I,CheckSum:Word;
 DataA :array[1..2048] OF Byte absolute Data;
BEGIN
 CheckSum:=0;
 CheckSum := (CheckSum AND $FF)+id;
 CheckSum := (CheckSum AND $FF)+HI(Length);
 CheckSum := (CheckSum AND $FF)+LO(Length);

 For I := 1 to Length do
 begin
   CheckSum := (CheckSum AND $FF)+DataA[I];
 end;
 CheckSum := ($FF-(CheckSum AND $FF)+1) AND $FF;

 result:=CheckSum-1;
END;


procedure Write_Id_Length_Data(var F : FILE; id : byte ; var data; len : word);
var
 IdLength : IdLengthRec;
 checksum : byte;
begin
  IdLength.id:=id;
  IdLength.Length:=len+1; // +1 for checksum byte
  Blockwrite(F,IdLength,sizeof(idLength));
  Blockwrite(F,data,len);
  checksum:=GetCheckSum(id,data,len);

  Blockwrite(F,checksum,1); // write checksum
end;


//80H THEADR - Translator Header Record
procedure Write_THeadr(var F : File;name : string);
begin
  Write_Id_Length_Data(F,$80,name,length(name)+1);
end;

function CreateStringRec(names : string; sep : char;var dataptr;maxcount : word) : word;
var
  data   : array[0..255] of  TPubDefStrRec absolute dataptr;
  slen   : integer;
  scount : integer;
begin
  // create pattern of -  string length/string pairs
  // eg CODE  = 4CODE,_TEXT = 5_TEXT
  // 0 length string is valid = 0  (just 0 - no name followed)

  scount:=0;
  While (Length(names) > 0) and (scount<maxcount) do
  begin
    slen:=Pos(sep,names);
    if slen=0 then //no # - just one name
    begin
      slen:=length(names);
      inc(scount);
      data[scount].StringLength:=slen;
      data[scount].PubLicName:=names;
      data[scount].PublicOffset:=0;
      data[scount].TypeIndex:=0;
      Delete(names,1,slen);
    end
    else
    begin
      if slen=1 then //0 byte name
      begin
        inc(scount);
        data[scount].StringLength:=slen;
        data[scount].PubLicName:='';
        data[scount].PublicOffset:=0;
        data[scount].TypeIndex:=0;
        Delete(names,1,1); // delete #
      end
      else  //slen-1 size name
      begin
        inc(scount);
        data[scount].StringLength:=slen-1;
        data[scount].PubLicName:=Copy(names,1,slen-1);
        data[scount].PublicOffset:=0;
        data[scount].TypeIndex:=0;
        Delete(names,1,slen); //delete name + #
      end;
    end;
  end;

  result:=scount;
end;



function CreateStringSet(names : string; sep : char) : string;
var
  data   : array[0..255] of Byte;
  slen   : integer;
  scount : integer;
begin
  // create pattern of -  string length/string pairs
  // eg CODE  = 4CODE,_TEXT = 5_TEXT
  // 0 length string is valid = 0  (just 0 - no name followed)

  scount:=0;
  While Length(names) > 0 do
  begin
    slen:=Pos(sep,names);
    if slen=0 then //no # - just one name
    begin
      slen:=length(names);
      data[scount]:=slen; //set name length in data
      inc(scount);
      Move(names[1],data[scount],slen);
      inc(scount,slen);
      Delete(names,1,slen);
    end
    else
    begin
      if slen=1 then //0 byte name
      begin
       data[scount]:=0;
       Delete(names,1,1); // delete #
       inc(scount);
      end
      else  //slen-1 size name
      begin
        data[scount]:=slen-1; //set name length in data
        inc(scount);
        move(names[1],data[scount],slen-1); //copy name only
        inc(scount,slen-1);
        Delete(names,1,slen); //delete name + #
      end;
    end;
  end;
  Move(data,result[1],scount);
  result[0]:=char(scount);
end;


//96H LNAMES - List of Names Record
procedure Write_LNames(var F : File;names : string);
var
  data   : array[0..255] of Byte;
  slen   : integer;
  scount : integer;
begin
  // create pattern of -  string length/string pairs
  // eg CODE  = 4CODE,_TEXT = 5_TEXT
  // 0 length string is valid = 0  (just 0 - no name followed)
  FillChar(data,sizeof(data),0);
  scount:=0;
  While Length(names) > 0 do
  begin
    slen:=Pos('#',names);
    if slen=0 then //no # - just one name
    begin
      slen:=length(names);
      data[scount]:=slen; //set name length in data
      inc(scount);
      Move(names[1],data[scount],slen);
      inc(scount,slen);
      Delete(names,1,slen);
    end
    else
    begin
      if slen=1 then //0 byte name
      begin
       data[scount]:=0;
       Delete(names,1,1); // delete #
       inc(scount);
      end
      else  //slen-1 size name
      begin
        data[scount]:=slen-1; //set name length in data
        inc(scount);
        move(names[1],data[scount],slen-1); //copy name only
        inc(scount,slen-1);
        Delete(names,1,slen); //delete name + #
      end;
    end;
  end;
  Write_Id_Length_Data(F,$96,data,scount);
end;

//98H SEGDEF - Segment Definition Record
procedure Write_SegDef(var F : File;
                           flags: byte;
                           Segmentlength: word;
                           SegNameIndex: byte;
                           ClassNameIndex: byte;
                           OverlayNameIndex: byte);
var
  data : TSegDefRec;
begin
  data.segmentattributes := flags;
  data.segmentlength := SegmentLength;
  data.SegmentNameIndex := SegNameIndex;
  data.ClassNameIndex := classNameIndex;
  data.OverlayNameIndex := OverlayNameIndex;

  Write_Id_Length_Data(F, $98,data,sizeof(data));
end;

// 9AH GRPDEF-Group Definition Record
procedure Write_GRPDEF(var F : File; GroupNameIndex: byte; SegmentDefinition: byte);
var
  data: array[0..3] of byte;
begin
  data[0] := GroupNameIndex;
  data[1] := $FF;
  data[2] := SegmentDefinition;
  Write_Id_Length_Data(F,$9A,data,sizeof(data));
end;


// write mulitple public names/public offset supplied in records
// 90H PUBDEF-Public Names Definition Record
procedure Write_PubDefRecords(var F : File; BaseGroupIndex: byte; BaseSegmentIndex: byte;var rdata;rcount : word);
var
  nameLen: word;
  data   : ^Byte;
  rdataptr : array[1..256] of TPubDefStrRec absolute rdata;
  i : word;
  strmemreq : word;
  totalmemreq : word;
  bpos  : word;
begin
  strmemreq:=0;
  for i:=1 to rcount do
  begin
    inc(strmemreq,rdataptr[i].stringlength);
  end;
  totalmemreq:=2+(rcount*4+strmemreq);// 2 byte is size of basegroupindex(1) + basesegmentindex(1), 4 byte is string lengh(1), public offset(2), and type index(1)

  GetMem(data, totalmemreq);
  data^ := BaseGroupIndex;
  (data + 1)^ := BaseSegmentIndex;

  bpos:=2;
  for i:=1 to rcount do
  begin
    namelen:=rdataptr[i].stringlength;
    (data + bpos)^ := namelen;
    inc(bpos);
    Move(rdataptr[i].PublicName[1], (data + bpos)^, nameLen);
    inc(bpos,namelen);
    (data + bpos)^ := rdataptr[i].PublicOffset and $FF;
    inc(bpos);
    (data + bpos)^ := rdataptr[i].PublicOffset shr 8;
    inc(bpos);
    (data + bpos)^ := rdataptr[i].TypeIndex;
    inc(bpos);
  end;
  Write_Id_Length_Data(F, $90,data^,totalmemreq);
  FreeMem(data);
end;

// 90H PUBDEF-Public Names Definition Record
procedure Write_PubDef(var F : File; BaseGroupIndex: byte; BaseSegmentIndex: byte; PublicName: string;
                           PublicOffset: word; TypeIndex: byte);
var
  data : TPubDefStrRec;
begin
  data.publicname:=publicname;
  data.stringlength:=length(publicname);
  data.publicoffset:=publicoffset;
  data.typeindex:=typeindex;
  Write_PubDefRecords(F,BasegroupIndex,BaseSegmentIndex,data,1);
end;

// 8AH MODEND-Module End Record
procedure Write_ModEnd(var F : File);
var
 Data : Byte = 0;
begin
  Write_Id_Length_Data(F, $8A,Data,1);
end;

// A0H LEDATA-Logical Enumerated Data Record
procedure Write_LeData(var F : File;SegmentIndex : Byte;EnumeratedDataOffset : word;var DataBytes  ; datalength : word);
var
  recordPtr: PByte;
begin
  GetMem(recordPtr, dataLength + 3);
  recordPtr^ := SegmentIndex;
  (recordPtr + 1)^ := EnumeratedDataOffset and $FF;
  (recordPtr + 2)^ := EnumeratedDataOffset shr 8;
  Move(pbyte(databytes)^, (recordPtr + 3)^, dataLength);

  Write_Id_Length_Data(F, $A0,recordPtr^,datalength+3);
  FreeMem(recordPtr, dataLength + 3);
end;

procedure Write_SegmentData(var F : File; SegmentIndex: byte; EnumeratedDataOffset: word; dataBytes : PByte; dataLength: word);
begin
  while (dataLength > 1024) do
  begin
    Write_LeData(F, SegmentIndex, EnumerateddataOffset, databytes, 1024);
    EnumerateDdataOffset := EnumeratedDataOffset + 1024;
    Inc(dataBytes, 1024);
    dataLength := datalength - 1024;
  end;
  Write_LeData(F, SegmentIndex, EnumeratedDataOffset, dataBytes, dataLength);
end;

function GetFileSize(filename : string) : longword;
var
 F : File;
begin
 Assign(F,filename);
{$I-}
 Reset(F,1);
 result:=WORD(FileSize(F));
 close(F);
{$I+}
end;

Procedure Write_FileContents(var F : File;filename : string;segIdx: byte; dataOffset: word);
var
 size : longword;
 FC   : File;
 data : pointer;
begin
 Assign(FC,filename);
 Reset(FC,1);
 size:=WORD(FileSize(FC));
 GetMem(data,size);
 if data<>NIL then
 begin
   Blockread(FC,data^,size);
   Write_SegmentData(F,SegIdx,dataOffset,data,size);
   FreeMem(data,size);
 end;
 close(FC);
end;

Procedure Write_FileContentsAndSize(var F : File;filename : string;segIdx: byte; dataOffset: word);
var
 size : word;
 FC   : File;
 data : PByte;
begin
 Assign(FC,filename);
 Reset(FC,1);
 size:=WORD(FileSize(FC));
 GetMem(data,size+2);
 if data<>NIL then
 begin
   Blockread(FC,data^,size);
   (data + size)^:=LO(size);
   (data + size+1)^:=HI(size);

   Write_SegmentData(F,SegIdx,dataOffset,data,size+2);
   FreeMem(data,size+2);
 end;
 close(FC);
end;



// create Turbo Pascal Compatile BINOBJ output exactly to the byte level
Function CreateTPObj(infile,outfile,publicname : string) : word;
var
 size : word;
 F    : File;
begin
 size:=WORD(GetFileSize(infile));
{$I-}
 assign(F,outfile);
 rewrite(F,1);
 Write_THeadr(F,#$3a#$3a);
 Write_LNames(F,'#CODE##');
 Write_SegDef(F,$28,size,2,1,1);
 Write_PubDef(F,0,1,publicname,0,0);
 Write_FileContents(F,infile,1,0);
 Write_ModEnd(F);
 close(F);
{$I+}
 result:=IORESULT;
end;

procedure ChangePubDefStr(index : byte;var data; PublicName : string; PublicOffset : word; typeindex : byte);
var
 DataA : array[1..255] of TPubDefStrRec absolute data;
begin
  dataA[index].StringLength:=length(publicname);
  dataA[index].PubLicName:=publicname;
  dataA[index].PublicOffset:=publicoffset;
  dataA[index].TypeIndex:=typeindex;
end;

Function CreateTPObj(infile,outfile,publicname,publicsizename : string) : word;
var
 size : word;
 F    : File;
 data : array[1..2] of TPubDefStrRec;
begin
 size:=WORD(GetFileSize(infile));
{$I-}
 assign(F,outfile);
 rewrite(F,1);
 Write_THeadr(F,#$3a#$3a);
 Write_LNames(F,'#CODE##');
 Write_SegDef(F,$28,size+2,2,1,1);  //+2 is the addtional bytes we will need to include the size information

 ChangePubDefStr(1,data,publicname,0,0);
 ChangePubDefStr(2,data,publicsizename,size,0);
 Write_PubDefRecords(F,0,1,data,2);

 Write_FileContentsAndSize(F,infile,1,0);
 Write_ModEnd(F);
 close(F);
{$I+}
 result:=IORESULT;
end;

begin
end.
