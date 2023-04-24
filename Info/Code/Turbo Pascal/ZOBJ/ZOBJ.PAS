program ZObj;
{ Zobj version 1.00.
  by David Neal Dubois.
  Please see ZObj.DOC. }

uses
  CRT;
var
  MaxLEDATA : word;
var
  F : file;
  G : file;
  X, Y : integer;

type
  BuffType = array [ 0 .. 65534 ] of byte;
var
  FBuff, GBuff : ^ BuffType;
  FSize, GSize : word;
  FPos, GPos   : word;

  Buffer : array [ 1 .. 60000 ] of byte;
  BufferLength : word;
  RecordHeader : record
                   RecordType   : byte;
                   RecordLength : word;
                 end absolute Buffer;

  SegmentOffset : word;

const
  THEADR = $80;
  MODEND = $8A;
  PUBDEF = $90;
  LNAMES = $96;
  SEGDEF = $98;
  FIXUPP = $9C;
  LEDATA = $A0;

  procedure ResetBuffer;
  begin
    BufferLength := 3;
  end;

  procedure BufferizeByte ( B : byte );
  begin
    inc ( BufferLength );
    Buffer [ BufferLength ] := B;
  end;

  procedure BufferizeWord ( W : word );
  begin
    BufferizeByte ( lo ( W ) );
    BufferizeByte ( hi ( W ) );
  end;

  procedure BufferizeString ( S : string );
  var
    I : integer;
  begin
    BufferizeByte ( length ( S ) );
    for I := 1 to length ( S ) do
      BufferizeByte ( ord ( S [ I ] ) );
  end;

  procedure WriteObjectRecord ( RecordType : byte );
  var
    CheckSum     : byte;
    RecordLength : word;
    I            : word;
  begin
    RecordHeader . RecordType := RecordType;
    RecordLength := BufferLength - 2;
    RecordHeader . RecordLength := RecordLength;
    CheckSum :=   ( RecordType
                + lo ( RecordLength )
                + hi ( RecordLength ) ) mod 256;
    for I := 4 to BufferLength do
      CheckSum := ( CheckSum + Buffer [ I ] ) mod 256;
    Buffer [ BufferLength + 1 ] := ( 256 - CheckSum ) mod 256;

    move ( Buffer, GBuff ^ [ GPos ], BufferLength + 1 );
    inc ( GPos, BufferLength + 1 );

    gotoxy ( X, Y + 1 ); write ( GPos : 5 );
  end;

  procedure MoveData ( MaxBytes : word );
  var
    ToMove : word;
  begin
    ToMove := FSize - Fpos;
    if MaxBytes < ToMove then
      ToMove := MaxBytes;

    move ( FBuff ^ [ FPos ], Buffer [ BufferLength + 1 ], ToMove );
    inc ( FPos, ToMove );

    inc ( BufferLength, ToMove );
    inc ( SegmentOffset, ToMove );
    gotoxy ( X, Y ); write ( FPos : 5 );
  end;

var
  I : integer;
begin
  val ( paramstr ( 4 ), MaxLEData, I );
  if I <> 0 then
    MaxLEData := 1024;

  writeln ( 'ZObj version 1.00  -  1989.02.06 - David Neal Dubois - Zelkop Software' );
  writeln;
  if paramcount < 3 then
    begin
      writeln ( 'Converts BINary to OBJect format, with address function.' );
      writeln ( '*** Please, see file ZObj.DOC ***' );
      halt;
    end;

  new ( FBuff );
  new ( GBuff );

  writeln ( ' Reading file: "', paramstr ( 1 ), '"' );
  writeln ( ' Writing file: "', paramstr ( 2 ), '"' );
  writeln ( 'Function name: "', paramstr ( 3 ), '"' );
  writeln;
  writeln ( '   Bytes read:     0' );
  write   ( 'Bytes written:     0' );
  X := WhereX - 5; Y := WhereY - 1;

  assign ( F, paramstr ( 1 ) );
  reset ( F, 1 );
  FSize := filesize ( F );
  blockread ( F, FBuff ^, FSize );
  close ( F );
  FPos := 0;

  GPos := 0;

  { Write Header record }

  ResetBuffer;
  BufferizeString ( '' ); { No T-module name (to save space) }
  WriteObjectRecord ( THEADR );

  { Write List of Names record }

  ResetBuffer;
  BufferizeString ( '' );
  BufferizeString ( 'CODE' );
  BufferizeString ( '' );
  WriteObjectRecord ( LNAMES );

  { Write Segment Definition record }

  ResetBuffer;
  BufferizeByte ( $28 ); { ACBP = Relocatable, Byte, Public }

  BufferizeWord ( Fsize + 6 ); { Segment length is data + 6 for code }
  BufferizeByte ( $02 ); { segment name index }
  BufferizeByte ( $01 ); { class name index }
  BufferizeByte ( $01 ); { overlay name index }
  WriteObjectRecord ( SEGDEF );

  { Write Public Names Definition record }

  ResetBuffer;
  BufferizeByte ( $00 ); { segment 0 ? }
  BufferizeByte ( $01 ); { frame   1 ? }
  BufferizeString ( paramstr ( 3 ) ); { The name of the function }
  BufferizeWord ( $0000 ); { offset }
  BufferizeByte ( $00 );   { type, none ? }
  WriteObjectRecord ( PUBDEF );

  { Now the data itself }

  SegmentOffset := 0;

  ResetBuffer;

  BufferizeByte ( $01 ); { segment index }
  BufferizeWord ( SegmentOffset );

  BufferizeByte ( $8C ); { MOV DX, CS }
  BufferizeByte ( $CA );
  BufferizeByte ( $B8 ); { MOV AX, offset data }
  BufferizeByte ( $06 );
  BufferizeByte ( $00 );
  BufferizeByte ( $CB ); { *** RETF ***, change this if you want near. }

  SegmentOffset := 6;
  MoveData ( MaxLEDATA - 6 );

  WriteObjectRecord ( LEDATA );

  { Write Fix-up record }

  ResetBuffer;
  BufferizeWord ( $03C4 ); { locat }
  BufferizeByte ( $54 ); { fixdat }
  BufferizeByte ( $01 ); { frame reference }
  WriteObjectRecord ( FIXUPP );

  { Write rest of data }

  while FPos < FSize do
    begin
      ResetBuffer;
      BufferizeByte ( $01 );
      BufferizeWord ( SegmentOffset );
      MoveData ( MaxLEDATA );
      WriteObjectRecord ( LEDATA );
    end;

  { Write Module End record }

  ResetBuffer;
  BufferizeByte ( 1 ); { Is not main program. Does not contain start address }
  WriteObjectRecord ( MODEND );

  assign ( G, paramstr ( 2 ) );
  rewrite ( G, 1 );
  blockwrite ( G, GBuff ^, GPos );
  close ( G );

  writeln;
end.