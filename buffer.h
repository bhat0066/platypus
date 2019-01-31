/*********************************************************************************************
*	File name: buffer.h
*	Compiler: MS Visual Studio Enterprise 2015
*	Author: Jinesh Bhatt, 040845930
*	Course: CST8152 - Compilers, Lab Section: 11
*	Assignment: 1
*	Date: 9/29/2017
*	Professor: Sv. Ranev
*	Purpose: Preprocessor directives, type declarations and prototypes necessary for buffer 
*			 implementation as required for the assignment.
*	Function List:
*					Buffer *b_allocate(),
*					pBuffer b_addc(),
*					int b_clear(),
*					void b_free(),
*					int b_isfull(),
*					short b_limit(),
*					short b_capacity(),
*					short b_mark(),
*					int b_mode(),
*					size_t b_incfactor(),
*					int b_load(),
*					int b_isempty(),
*					int b_eob(),
*					char b_getc(),
*					int b_print(),
*					Buffer *b_compact(),
*					char b_rflag(),
*					short b_retract(),
*					short b_reset(),
*					short b_getcoffset(),
*					int b_rewind(),
*					char *b_location()				
*
**********************************************************************************************/
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
/* You may add your own constant definitions here */
#define RT_FAIL1 -1				/* fail return value */
#define RT_FAIL2 -2				/* fail return value */
#define LOAD_FAIL -2			/* load fail error */
#define SET_R_FLAG 1			/* realloc flag set value */
#define FIXED_MODE 0			/* fixed-size mode*/
#define ADD_MODE 1				/* additive self incrementing mode*/
#define MULTI_MODE -1			/* multiplicative self incrementing mode*/
#define MIN_FACTOR 1			/* mode a & m min factor*/
#define MAX_M_FACTOR 100		/* mode m max factor*/
#define MAX_A_FACTOR 255		/* mode a max factor*/
#define R_FLAG 0				/* initial r flag value */
#define RT_FAIL3 256			/* fail retun value of size_t b_incfactor()*/
#define SUCCESS 0				/* success return value*/
#define NEWSHORT (SHRT_MAX-1)	/* short max - 1 value*/

/* user data type declarations */
typedef struct BufferDescriptor {
	char *cb_head;   /* pointer to the beginning of character array (character buffer) */
	short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
	short addc_offset;  /* the offset (in chars) to the add-character location */
	short getc_offset;  /* the offset (in chars) to the get-character location */
	short markc_offset; /* the offset (in chars) to the mark location */
	char  inc_factor; /* character array increment factor */
	char  r_flag;     /* reallocation flag */
	char  mode;       /* operational mode indicator*/
	int   eob;       /* end-of-buffer flag */
} Buffer, *pBuffer;


/* function declarations */
Buffer *b_allocate(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_clear(Buffer * const pBD);
void b_free(Buffer * const pBD);
int b_isfull(Buffer * const pBD);
short b_limit(Buffer * const pBD);
short b_capacity(Buffer * const pBD);
short b_mark(Buffer * const pBD, short mark);
int b_mode(Buffer * const pBD);
size_t b_incfactor(Buffer * const pBD);
int b_load(FILE * const fi, Buffer * const pBD);
int b_isempty(Buffer * const pBD);
int b_eob(Buffer * const pBD);
char b_getc(Buffer * const pBD);
int b_print(Buffer * const pBD);
Buffer *b_compact(Buffer * const pBD, char symbol);
char b_rflag(Buffer * const pBD);
short b_retract(Buffer * const pBD);
short b_reset(Buffer * const pBD);
short b_getcoffset(Buffer * const pBD);
int b_rewind(Buffer * const pBD);
char *b_location(Buffer * const pBD, short loc_offset);

#endif

