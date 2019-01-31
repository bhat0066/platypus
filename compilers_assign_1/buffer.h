/*
* File name: buffer.h
* Compiler: MS Visual Studio Enterprise 2015 Update 3
* Author: Silviu Riley, 040836045
* Course: CST8152 - Compilers, Lab Section: 13
* Assignment: 1
* Date: 2017-10-13
* Professor: Sv. Ranev
* Purpose: Preprocessor directives, type declarations, and prototypes necessary for Buffer implementation
* Function list: b_allocate(), b_addc(), b_clear(), b_free(), b_isfull(), b_limit(), b_capacity(), b_mark(),
                 b_mode(), b_incfactor(), b_load(), b_isempty(), b_eob(), b_getc(), b_print(), b_compact(),
                 b_rflag(),  b_retract(),  b_reset(),  b_getcoffset(),  b_rewind(),  b_location()
* Constant list: FALSE, TRUE, SUCCESS, RT_FAIL1, RT_FAIL2, LOAD_FAIL, SET_R_FLAG, CAPACITY_MIN, CAPACITY_MAX,
                 MODE_M, MODE_M_TXT, MODE_F, MODE_F_TXT, MODE_A, MODE_A_TXT, INCREMENT_F, INCREMENT_A_MIN,
                 INCREMENT_A_MAX, INCREMENT_M_MIN, INCREMENT_M_MAX, INCREMENT_FAIL
* Macro list: 
*/

#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001)*/ /* to enforce C89 type comments - to make // comments a warning */
/*#pragma warning(error:4001)*/ /* to enforce C89 type comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* dynamic memory allocation */
#include <limits.h> /* implementation-defined data type ranges and limits */

/* macro definitions */


/* constant definitions */
#define FALSE 0
#define TRUE 1
#define SUCCESS 0               /* successfully completed function */
#define RT_FAIL1 -1             /* fail return value */
#define RT_FAIL2 -2             /* fail return value */
#define LOAD_FAIL -2            /* load fail error */
#define SET_R_FLAG 1            /* realloc flag set value */
#define CAPACITY_MIN 0          /* minimum initial capacity */
#define CAPACITY_MAX (SHRT_MAX - 1)   /* maximum initial capacity */
#define MODE_M -1               /* multiplicative self-incrementing buffer mode */
#define MODE_M_TXT 'm'          /* multiplicative mode text representation */
#define MODE_F 0                /* fixed-size buffer mode */
#define MODE_F_TXT 'f'          /* fixed mode text representation */
#define MODE_A 1                /* additive self-incrementing buffer mode */
#define MODE_A_TXT 'a'          /* additive mode text representation */
#define INCREMENT_F 0           /* increment factor for fixed buffer mode */
#define INCREMENT_A_MIN 1       /* minimum increment in additive buffer mode */
#define INCREMENT_A_MAX 255     /* maximum increment in additive buffer mode */
#define INCREMENT_M_MIN 1       /* minimum increment in multiplicative buffer mode */
#define INCREMENT_M_MAX 100     /* maximum increment percent in multiplicative buffer mode */
#define INCREMENT_FAIL 256      /* maximum increment if failure condition happens */

/* user data type declarations */
typedef struct BufferDescriptor {
    char    *cb_head;       /* pointer to the beginning of character array (character buffer) */
    short   capacity;       /* current dynamic memory size (in bytes) allocated to character buffer */
    short   addc_offset;    /* the offset (in chars) to the add-character location */
    short   getc_offset;    /* the offset (in chars) to the get-character location */
    short   markc_offset;   /* the offset (in chars) to the mark location */
    char    inc_factor;     /* character array increment factor */
    char    r_flag;         /* reallocation flag */
    char    mode;           /* operational mode indicator */
    int     eob;            /* end-of-buffer flag */
} Buffer, *pBuffer;

/* function declarations */
Buffer *b_allocate(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_clear(Buffer *const pBD);
void b_free(Buffer *const pBD);
int b_isfull(Buffer *const pBD);
short b_limit(Buffer *const pBD);
short b_capacity(Buffer *const pBD);
short b_mark(Buffer *const pBD, short mark);
int b_mode(Buffer *const pBD);
size_t b_incfactor(Buffer *const pBD);
int b_load(FILE *const fi, Buffer *const pBD);
int b_isempty(Buffer *const pBD);
int b_eob(Buffer *const pBD);
char b_getc(Buffer *const pBD);
int b_print(Buffer *const pBD);
Buffer *b_compact(Buffer *const pBD, char symbol);
char b_rflag(Buffer *const pBD);
short b_retract(Buffer *const pBD);
short b_reset(Buffer *const pBD);
short b_getcoffset(Buffer *const pBD);
int b_rewind(Buffer *const pBD);
char *b_location(Buffer *const pBD, short loc_offset);

#endif

