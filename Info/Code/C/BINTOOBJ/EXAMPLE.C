#include "BinToObj.h"

#include <fcntl.h>
#include <io.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

typedef unsigned long ulong;
typedef unsigned int uint;

#define BLOCKSIZE 16


/* Huge version of function similar to fwrite as fwrite doesn't wrap 
   segments it's particularly useless. */

ulong hfwrite( char __huge *buffer, ulong Size, FILE *stream ) {
   ulong Return = 0;
   size_t ToRead,
          Read;

   while ( Size )
      if ( (Read = fwrite( buffer, (ToRead = (size_t)__min(INT_MAX, Size)),
            1, stream )) == 1 ) {
         buffer += ToRead;
         Size -= ToRead;
         Return += ToRead;
      } else
         return Read;
   return Return;
}
int main( void ) {
   ulong Size = &RawExeEnd - &RawExeStart;

   printf( "Simple example using BinToObj: (Size %ld)\n"
           "------------------------------\n"
           "\n", Size );

   /* we need to set the mode to _O_BINARY to prevent translation of CR/LF 
      pairs while will exist in the original .TXT file into CR/CR/LF (as the
      standard _O_TEXT conversion emits CR/LF for each CR it encounters.
      So first we flush the buffer to emit everything, then set the new mode.
   */
   fflush( stdout );
   _setmode( _fileno( stdout ), _O_BINARY );
   hfwrite( &RawExeStart, Size, stdout );
   return 0;
}

