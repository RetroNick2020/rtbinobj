/*
This material has been placed in the public domain and as such there are no
limitations on its use.  I do ask however that an acknowledgement is made if
this helps you out and the header remains intact in the source (yes I've got
an ego too folks).  As is usually the case, no warranties explicitly or
implied is made with this stuff.

This program takes any binary file and converts it to an .OBJ file which can
be linked with your application.  Two symbols are defined at the start and end
of the data through which you can address and size the information.

If you want more information regarding the operation, see the acompanying .TXT
file.  Apologies for the terseness of the comments, but the object record
structure and contents are well documented.

A successful compilation & link can be achieved using MSC 7 with:

CL /AL /Za /W4 /WX bintoobj.c

Lee Brown      Compuserve: 100325,3304
*/

#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <io.h>
#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>


/* Readability */

typedef unsigned int    uint;
typedef unsigned char   ubyte;
typedef unsigned long   ulong;

#define WriteRecord( fh, ObjRec )   _write( (fh), (char *)(ObjRec), \
                                            3 + *(uint *)((ObjRec)+1) )


/* Manifests */

#define SEGDEF_ALIGN_REL_B_LSEG  1
#define SEGDEF_COMBINE_PUBLIC    2
#define SEGMENT_PREFIX           "BINARY"
#define SEGMENT_POSTFIX_SIZE     2              /* No. of digits to follow */
#define SEGMENT_POSTFIX_SIZE_S   "2"            /* No. of digits to follow */

#define LLNAME_NULL              1
#define LLNAME_FAR_DATA          2
#define LLNAME_BINARY            3

#define EXIT_OK       0
#define EXIT_WARNING 10
#define EXIT_ERROR   20


/* a safer way to allocate memory */

void *SafeMAlloc( uint length ) {
   void *ret;

   if ( (ret = malloc( length )) != NULL )
      return ret;
   fprintf( stderr, "memory allocation failure.\n" );
   exit( EXIT_ERROR );
}


/* Calculate and store the checksum for a record */

ubyte CheckSum( ubyte *ObjRec ) {
   int CS = 0,
       i;
   for ( i = *(uint *)(ObjRec+1)+3; i>1; i-- )
      CS += *ObjRec++;
   return (*ObjRec = (ubyte)(-CS) );
}

/* write a THEADR record */

void writeTHEADR( int out, char *Name ) {
   ubyte Buf[32];
   int NameLen = strlen( Name );

   Buf[0] = 0x80;                               /* Identifier */
   *(uint *)(Buf+1) = NameLen + 2;              /* Record length */
   Buf[3] = (ubyte)NameLen;                     /* name length */
   strncpy( (char *)(Buf+4), Name, NameLen );   /* name */
   CheckSum( Buf );                             /* store checksum */
   WriteRecord( out, Buf );                     /* and write record */
}

/* write a SEGDEF record.  The Segname, Classname and Overlaynames are actually
   indexes into a table referenced by the LNAMES record */

void writeSEGDEF( int out, uint Align, uint Combine, ulong Length,
                  uint Segname, uint Classname, uint Overlayname ) {
   ubyte Buf[32];

   Buf[0] = 0x98;                               /* identifier */
   *(uint *)(Buf+1) = 7;                        /* Record length */
   Buf[3] = (ubyte)( (Align << 5) | (Combine << 2) |  /* Alignment, combine */
                     (Length == 65536L ? 2L : 0L ) ); /* and length bit 16 */
   *(uint *)(Buf+4) = (uint)(Length & 0x0000ffffL);   /* length */
   Buf[6] = (ubyte)Segname;                     /* Segment name index */
   Buf[7] = (ubyte)Classname;                   /* Classname index */
   Buf[8] = (ubyte)Overlayname;                 /* Overlayname index */
   CheckSum( Buf );
   WriteRecord( out, Buf );
}


/* write an LEDATA record.  The SegmentIndex is an index into the LNAME list
*/

void writeLEDATA( int out, ubyte *Buffer, uint Bytes, uint SegmentIndex,
      uint Offset ) {
   ubyte *Buf = SafeMAlloc( 1040 );

   Buf[0] = 0xa0;                               /* identifier */
   *(uint *)(Buf + 1) = Bytes + 4;              /* record length */
   Buf[3] = (ubyte)SegmentIndex;                /* segment name index */
   *(uint *)(Buf+4) = Offset;                   /* Offset into segment */
   memcpy( Buf+6, Buffer, Bytes );              /* actual data */
   CheckSum( Buf );
   WriteRecord( out, Buf );
   free( Buf );
}


/* write a MODEND record */

void writeMODEND( int out ) {
   ubyte Buf[32];

   Buf[0] = 0x8a;                               /* identifier */
   *(uint *)(Buf+1) = 2;                        /* record length */
   Buf[3] = 0;
   CheckSum( Buf );
   WriteRecord( out, Buf );
}


/* create and write the LNAMES record */

void genLNAME( char **buffer, char *name ) {
   int namelen = strlen( name );
   *(ubyte *)(*buffer) = (ubyte)namelen;
   memcpy( *buffer + 1, name, namelen );
   *buffer += namelen + 1;
}
void generateLNAMES( int out, ulong Filelength ) {
   ubyte *Buf, *Bufptr;
   uint Segments = (uint)(( Filelength - 1) / 65536L + 1L);
   uint Namesize = strlen( SEGMENT_PREFIX ) + SEGMENT_POSTFIX_SIZE;
   uint i;

   Buf = SafeMAlloc( 1024 );
   Bufptr = Buf+3;
   Buf[0] = 0x96;
   genLNAME( (char **)&Bufptr, "" );
   genLNAME( (char **)&Bufptr, "FAR_DATA" );
   for ( i = 0; i < Segments ; i++ ) {
      sprintf( (char *)(Bufptr+1), "%s%0" SEGMENT_POSTFIX_SIZE_S "d", 
            SEGMENT_PREFIX, i );
      genLNAME( (char **)&Bufptr, (char *)(Bufptr+1) );
   }
   *(uint *)(Buf+1) = Bufptr - Buf + 1 - 3;
   CheckSum( Buf );
   WriteRecord( out, Buf );
   free( Buf );
}


/* write a PUBDEF record */

void writePUBDEF( int out, uint SegmentIndex, uint Offset, char *Name ) {
   ubyte Buf[64];
   int Namelen = strlen( Name );

   Buf[0] = 0x90;
   *(uint *)(Buf+1) = Namelen + 7;
   Buf[3] = 0;                                  /* group index */
   Buf[4] = (ubyte)SegmentIndex;                /* segment index */
   Buf[5] = (ubyte)Namelen;
   strncpy( (char *)(Buf+6), Name, Namelen );
   *(uint *)(Buf+6+Namelen) = Offset;
   Buf[8+Namelen] = 0;
   CheckSum( Buf );
   WriteRecord( out, Buf );
}


void help( void ) {
   printf( "usage:\nBinToObj <infile> <outfile>\n" );
   exit( EXIT_ERROR );
}

/* Once the command line has been parsed and the relevant file opened, the
   real thing starts.  Two files are created, one the final output file, the
   other a temporary file.

   The LNAMES are constructed on-the-fly (ok, so it's not necessary for this,
   but it seemed like a good idea at the time...)

*/
int BinToObj( char **argv ) {
   int in, out;
   uint BytesRead, Offset, Segment;
   ulong BytesLeft, i, TotalBytes;
   char *Buffer,
         NameBuf[32],
        *TempFile = "$$$TMP$.$$$";

   if ( (in = _open( *++argv, _O_BINARY | _O_RDONLY )) == -1 ) {
      printf( "Can't open '%s'\n", *argv );
      return EXIT_ERROR;
   }
   if ( (out = _open( *++argv, _O_BINARY | _O_WRONLY | _O_TRUNC | _O_CREAT,
                      _S_IREAD | _S_IWRITE )) == -1 ) {
      printf( "Can't create '%s'\n", *argv );
      return EXIT_ERROR;
   }
   writeTHEADR( out, *argv );
   TotalBytes = BytesLeft = _filelength( in );
   generateLNAMES( out, TotalBytes );
   Buffer = SafeMAlloc( 1024 );
   for ( Segment = 0; BytesLeft; Segment++ ) {
      sprintf( NameBuf, "BINARY%02d", Segment );
      writeSEGDEF( out, SEGDEF_ALIGN_REL_B_LSEG, SEGDEF_COMBINE_PUBLIC, 
                   __min( BytesLeft, 65536 ), Segment + LLNAME_BINARY,
                   LLNAME_FAR_DATA, LLNAME_NULL );
      if ( Segment == 0 )
         writePUBDEF( out, Segment+1, 0, "_RawExeStart" );
      if ( BytesLeft < 65536 )
         writePUBDEF( out, Segment+1, (uint)BytesLeft, "_RawExeEnd" );
      for ( Offset = 0, i = __min( BytesLeft, 65536 ); i; i -= BytesRead,
                                                        Offset += 1024 ) {
         BytesRead = _read( in, Buffer, (uint)__min( 1024, BytesLeft ) );
         writeLEDATA( out, (ubyte *)Buffer, BytesRead, Segment+1, Offset );
         BytesLeft -= BytesRead;
      }
   }
   writeMODEND( out );
   _close( out );
   _close( in );
   return 0;
}

int main( int argc, char **argv ) {
   if ( argc != 3 )
      help();

   return BinToObj( argv );
}

