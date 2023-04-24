/*
 * ckmakeobj: A tool for generating DOS .OBJ files from Keen data files.
 *
 * Copyright (c) 2014, David Gow <david@davidgow.net>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
 * IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

// Should be compatible with the OMF Relocatable Object Module Format v1.1
// You can find 'OMF-1.1.pdf' around with the specification.

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifndef __BORLANDC__
#include <stdint.h>
#else
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef signed short int16_t;

#define strcasecmp(x,y) stricmp(x,y)
#endif

unsigned OBJ_ComputeChecksum(uint8_t id, uint8_t *input, size_t len)
{
	int i;
	unsigned total_len = len + 3; // id byte and length itself
	unsigned checksum = id + (total_len >> 8) + (total_len & 0xFF);
	for (i = 0; i < len; ++i)
	{
		checksum += input[i];
	}
	return (checksum^0xff)+1;
}

#define OBJ_THEADR_ID 0x80
#define OBJ_COMENT_ID 0x88
#define OBJ_MODEND_ID 0x8A
#define OBJ_PUBDEF_ID 0x90
#define OBJ_LNAMES_ID 0x96
#define OBJ_SEGDEF_ID 0x98
#define OBJ_GRPDEF_ID 0x9A
#define OBJ_LEDATA_ID 0xA0

void OBJ_WriteResource(FILE *outfile, uint8_t id, int16_t len, uint8_t *data)
{
	len++;
	fputc(id, outfile);
	fwrite(&len,2,1,outfile);
	fwrite(data,len - 1,1,outfile);
	fputc(OBJ_ComputeChecksum(id, data, len - 1), outfile);
}

void OBJ_WriteTHEADR(FILE *outfile, char *filename)
{
	uint8_t *theadr = malloc(strlen(filename) + 1);
	memcpy(theadr+1, filename, strlen(filename));
	theadr[0] = strlen(filename);
	OBJ_WriteResource(outfile, OBJ_THEADR_ID, strlen(filename)+1, theadr);
}

void OBJ_WriteCOMENT(FILE *outfile)
{
	uint8_t comment[] = { 0x00, 0x00, 'C', 'K', 'M', 'a', 'k', 'e', 'O', 'B', 'J', ' ',
		'b', 'y', ' ', 'D', 'a', 'v', 'i', 'd', ' ', 'G', 'o', 'w', ' ',
		'h', 't', 't', 'p', ':', '/', '/', 'd', 'a', 'v', 'i', 'd', 'g',
		'o', 'w', '.', 'n', 'e', 't', '/' };
		
	OBJ_WriteResource(outfile, OBJ_COMENT_ID, sizeof(comment), comment);
}

void OBJ_WriteLNAMES(FILE *outfile, char **names)
{
	int i;
	int16_t numNames = 0;
	int16_t segLen = 0;
	uint8_t *seg, *segoff;
	for (i = 0; names[i]; i++)
	{
		numNames++;
		segLen += strlen(names[i]) + 1;
	}
	seg = malloc(segLen);
	segoff = seg;
	for (i = 0; i < numNames; ++i)
	{
		*segoff = strlen(names[i]);
		segoff++;
		memcpy(segoff, names[i], strlen(names[i]));
		segoff+=strlen(names[i]);
	}
	OBJ_WriteResource(outfile, OBJ_LNAMES_ID, segLen, seg);
}

void OBJ_WriteSEGDEF(FILE *outfile, uint8_t flags, uint16_t len, uint8_t segNameIdx, uint8_t classNameIdx, uint8_t overlayNameIdx)
{
	uint8_t data[6];
	data[0] = flags;
	data[1] = len & 0xFF;
	data[2] = len >> 8;
	data[3] = segNameIdx;
	data[4] = classNameIdx;
	data[5] = overlayNameIdx;
	OBJ_WriteResource(outfile, OBJ_SEGDEF_ID, 7, data);
}

void OBJ_WriteGRPDEF(FILE *outfile, uint8_t groupNameIdx, uint8_t segDefIdx)
{
	uint8_t data[4];
	data[0] = groupNameIdx;
	data[1] = 0xFF;
	data[2] = segDefIdx;
	OBJ_WriteResource(outfile, OBJ_GRPDEF_ID, 3, data);
}

void OBJ_WritePUBDEF(FILE *outFile, uint8_t groupIdx, uint8_t segIdx, char *pubName, uint16_t pubOffset, uint8_t typeIdx)
{
	uint8_t nameLen = strlen(pubName);
	uint8_t *data = malloc(5 + nameLen);
	data[0] = groupIdx;
	data[1] = segIdx;
	data[2] = nameLen;
	memcpy(data+3, pubName, nameLen);
	data[3 + nameLen] = pubOffset & 0xFF;
	data[4 + nameLen] = pubOffset >> 8;
	data[5 + nameLen] = typeIdx;
	OBJ_WriteResource(outFile, OBJ_PUBDEF_ID, 5+nameLen, data);
}

void OBJ_WriteLEDATA(FILE *outFile, uint8_t segIdx, uint16_t dataOffset, uint8_t *data, size_t dataLen)
{
	uint8_t *record = malloc(dataLen + 3);
	record[0] = segIdx;
	record[1] = dataOffset & 0xFF;
	record[2] = dataOffset >> 8;
	memcpy(&record[3], data, dataLen);
	OBJ_WriteResource(outFile, OBJ_LEDATA_ID, 3 + dataLen, record);
}

void OBJ_WriteSegmentData(FILE *outFile, uint8_t segIdx, uint16_t dataOffset, uint8_t *data, size_t dataLen)
{
	while (dataLen > 1024)
	{
		OBJ_WriteLEDATA(outFile, segIdx, dataOffset, data, 1024);
		dataOffset += 1024;
		data += 1024;
		dataLen -= 1024;
	}
	OBJ_WriteLEDATA(outFile, segIdx, dataOffset, data, dataLen);
}

void OBJ_WriteMODEND(FILE *outFile, uint8_t modType)
{
	if (modType & (1 << 6))
	{
		fprintf(stderr, "Warning: MODEND module type byte requires Start Address field, which we do not support.\n");
	}
	
	OBJ_WriteResource(outFile, OBJ_MODEND_ID, 1, &modType);
}

typedef enum OBJ_SEGTYPE
{
	SEGTYPE_DATA,
	SEGTYPE_CODE,
	SEGTYPE_FARDATA
} OBJ_SEGTYPE;

int MakeOBJ(char *filename,char *destfilename,char *public,OBJ_SEGTYPE segtype,char *farname)
{
	char *block;
	FILE *handle;
	// All of the names in the .OBJ
	char *names[] =
	{
		"DGROUP",
		"_DATA",
		"DATA",
		"",
		"_TEXT",
		"CODE",
		"FAR_DATA",
		0,
		0
	};
	int16_t fsize;
	names[7] = farname;

	if ((handle=fopen(filename,"rb"))==NULL)
	{
		printf("Error: Couldn't open input file \"%s\"\n", filename);
		return -1;
	}

	fseek(handle, 0, SEEK_END);
	fsize=ftell(handle);
	fclose(handle);
	if (fsize>0x10000L)
	{
		printf("File exceeded 1 segment (64k). Cannot create obj\n");
		return -2;
	}

	block = malloc(fsize);
	handle = fopen(filename, "rb");
	fread(block, fsize, 1, handle);
	fclose(handle);

	handle = fopen(destfilename, "wb");

	// Write the header and comment records.
	OBJ_WriteTHEADR(handle, filename);
	OBJ_WriteCOMENT(handle);

	OBJ_WriteLNAMES(handle, names);
	switch(segtype)
	{
		case SEGTYPE_DATA:
			OBJ_WriteSEGDEF(handle, 0x48, fsize, 2, 3, 4);
			OBJ_WriteGRPDEF(handle, 1, 1);
			break;
		case SEGTYPE_CODE:
			OBJ_WriteSEGDEF(handle, 0x48, fsize, 5, 6, 4);
			OBJ_WriteGRPDEF(handle, 1, 1);
			break;
		case SEGTYPE_FARDATA:
			OBJ_WriteSEGDEF(handle, 0x60, fsize, 8, 7, 4);
			break;
	}
	OBJ_WritePUBDEF(handle, 1, 1, public, 0, 0);
	OBJ_WriteSegmentData(handle, 1, 0, block, fsize);
	OBJ_WriteMODEND(handle, 0);

	free(block);
	fclose(handle);
	return 0;
}

void PrintUsage()
{
	printf("CKMakeOBJ: A tool for generating DOS .OBJ files from Keen data files\n");
	printf("(C) 2014 David Gow <david@davidgow.net>\n\n");
	printf("Usage: ./ckmakeobj infile outfile symbol segtype [segname]\n");
	printf("\tinfile - the input datafile (must be <64k)\n");
	printf("\toutfile - the output .OBJ file to create\n");
	printf("\tsymbol - the symbol name to export the data as\n");
	printf("\tsegtype - the type of segment to create (DATA, CODE, FARDATA)\n");
	printf("\t[segname] - optional name for a FARDATA segment\n");
	printf("\n\n");
	printf("For Keen 4, these should be:\n");
	printf("\t./ckmakeobj MAPHEAD.CK4 CK4MHEAD.OBJ _maphead FARDATA MapHeader\n");
	printf("\t./ckmakeobj EGAHEAD.CK4 CK4EHEAD.OBJ _EGAhead FARDATA EGA_grafixheader\n");
	printf("\t./ckmakeobj EGADICT.CK4 CK4EDICT.OBJ _EGAdict DATA\n");
	printf("\t./ckmakeobj AUDIOHHD.CK4 CK4AHEAD.OBJ _audiohed FARDATA _AudioHeader\n");
	printf("\t./ckmakeobj AUDIODCT.CK4 CK4ADICT.OBJ _audiodict DATA\n");
	printf("\nHave fun!\n");
}

int main(int argc, char **argv)
{
	OBJ_SEGTYPE segType;
#ifdef KEEN4
	MakeOBJ("MAPHEAD.CK4", "CK4MHEAD.OBJ", "_maphead", SEGTYPE_FARDATA, "MapHeader");
	MakeOBJ("EGAHEAD.CK4", "CK4EHEAD.OBJ", "_EGAhead", SEGTYPE_FARDATA, "EGA_grafixheader");
	MakeOBJ("EGADICT.CK4", "CK4EDICT.OBJ", "_EGAdict", SEGTYPE_DATA, 0);
	MakeOBJ("AUDIOHHD.CK4", "CK4AHEAD.OBJ", "_audiohed", SEGTYPE_FARDATA, "_AudioHeader");
	MakeOBJ("AUDIODCT.CK4", "CK4ADICT.OBJ", "_audiodict", SEGTYPE_DATA, 0);
	return 0;
#else
	if (argc < 5)
	{
		PrintUsage();
		exit(-1);
	}

	if (!strcasecmp(argv[4], "DATA"))
		segType = SEGTYPE_DATA;
	else if (!strcasecmp(argv[4], "CODE"))
		segType = SEGTYPE_CODE;
	else if (!strcasecmp(argv[4], "FARDATA"))
		segType = SEGTYPE_FARDATA;
	else
	{
		PrintUsage();
		exit(-1);
	}

	return MakeOBJ(argv[1], argv[2], argv[3], segType, (argc > 5) ? argv[5] : NULL);

#endif
}
