/*********************************************************************************************
*	File name: table.h
*	Compiler: MS Visual Studio Enterprise 2015
*	Author: Jinesh Bhatt, 040845930
*	Course: CST8152 - Compilers, Lab Section: 11
*	Assignment: 2
*	Date: 12/7/2017
*	Professor: Sv. Ranev
*	Purpose: Transition Table and function declarations necessary for the scanner implementation
*			 as required for CST8152 - Assignment #2.
*	Function List:
*					Token aa_func02();
*					Token aa_func03();
*					Token aa_func05();
*					Token aa_func08();
*					Token aa_func10();
*					Token aa_func12();
*
**********************************************************************************************/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
*    '\0' or only one of the folowing constants: 255, 0xFF , EOF
*/

#define SEOF_255 255
#define SEOF_0 '\0'

/*  Single-lexeme tokens processed separately one by one
*  in the token-driven part of the scanner
*  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' ,
*       space
*  !!comment , ',' , '"' , ';' , '-' , '+' , '*' , '/', '#' ,
*  .AND., .OR. , SEOF, 'wrong symbol',
*/

/*REPLACE *ESN* WITH YOUR ERROR STATE NUMBER */
#define ER  13	/* Error state */
#define ES  12	/* Error state */
#define IS -1    /* Inavalid state */

/* State transition table definition */

/*REPLACE *CN* WITH YOUR COLUMN NUMBER*/

#define TABLE_COLUMNS 8

/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/*				[a-w||y||z]||[G-Z]	x	[A-F]	0	[1-9]	.	 $	  other	*/
	/* State 0 */{			1,			1,	  1,	6,	  4,	IS,  IS,  IS },
	/* State 1 */{			1,			1,    1,	1,    1,	2,   3,   2 },
	/* State 2 */{			IS,			IS,   IS,	IS,   IS,	IS,  IS,  IS },
	/* State 3 */{			IS,			IS,   IS,	IS,   IS,	IS,  IS,  IS },
	/* State 4 */{			ES,			ES,   ES,	4,    4,	7,   5,   5 },
	/* State 5 */{			IS,			IS,   IS,	IS,   IS,	IS,  IS,  IS },
	/* State 6 */{			ES,			9,    ES,   5,    ES,	7,   ES,  5 },
	/* State 7 */{			8,			8,    8,	7,    7,	8,   8,   8 },
	/* State 8 */{			IS,			IS,   IS,	IS,   IS,	IS,  IS,  IS },
	/* State 9 */{			ER,			ER,   10,	10,   10,	ER,  ER,  ER },
	/* State 10 */{			ES,			ES,   10,	10,   10,	ES,  ES,  11 },
	/* State 11 */{			IS,			IS,   IS,	IS,   IS,	IS,  IS,  IS },
	/* State 12 */{			IS,			IS,   IS,	IS,   IS,	IS,  IS,  IS },
	/* State 13 */{			IS,			IS,   IS,	IS,   IS,	IS,  IS,  IS }
};

/* Accepting state table definition */
/*REPLACE *N1*, *N2*, and *N3* WITH YOUR NUMBERS*/
#define ASWR     0  /* accepting state with retract */
#define ASNR     1  /* accepting state with no retract */
#define NOAS     2  /* not accepting state */

int as_table[] = {
	/* State 0 */	NOAS,
	/* State 1 */	NOAS,
	/* State 2 */	ASWR,
	/* State 3 */	ASNR,
	/* State 4 */	NOAS,
	/* State 5 */	ASWR,
	/* State 6 */	NOAS,
	/* State 7 */	NOAS,
	/* State 8 */	ASWR,
	/* State 9 */	NOAS,
	/* State 10 */	NOAS,
	/* State 11 */	ASWR,
	/* State 12 */	ASNR,
	/* State 13 */	ASWR
};

/* Accepting action function declarations */

/*FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
ONE FUNCTION PROTOTYPE. THEY ALL RETURN Token AND TAKE
ONE ARGUMENT: A string REPRESENTING A TOKEN LEXEME. */

Token aa_func02(char *lexeme);
Token aa_func03(char *lexeme);
Token aa_func05(char *lexeme);
Token aa_func08(char *lexeme);
Token aa_func10(char *lexeme);
Token aa_func12(char *lexeme);

/* defining a new type: pointer to function (of one char * argument)
returning Token
*/

typedef Token(*PTR_AAF)(char *lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
* Token (*aa_table[])(char lexeme[]) = {
*/

PTR_AAF aa_table[] = {
	/*HERE YOU MUST PROVIDE AN INITIALIZATION FOR AN ARRAY OF POINTERS
	TO ACCEPTING FUNCTIONS. THE ARRAY HAS THE SAME SIZE AS as_table[ ].
	YOU MUST INITIALIZE THE ARRAY ELEMENTS WITH THE CORRESPONDING
	ACCEPTING FUNCTIONS (FOR THE STATES MARKED AS ACCEPTING IN as_table[]).
	THE REST OF THE ELEMENTS MUST BE SET TO NULL.*/

	/* State 0 */ NULL,
	/* State 1 */ NULL,
	/* State 2 */ aa_func02,
	/* State 3 */ aa_func03,
	/* State 4 */ NULL,
	/* State 5 */ aa_func05,
	/* State 6 */ NULL,
	/* State 7 */ NULL,
	/* State 8 */ aa_func08,
	/* State 9 */ NULL,
	/* State 10 */ NULL,
	/* State 11 */ aa_func10,
	/* State 12 */ aa_func12,
	/* State 13 */ aa_func12
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  10

char * kw_table[] =
{
	"ELSE",
	"FALSE",
	"IF",
	"PLATYPUS",
	"READ",
	"REPEAT",
	"THEN",
	"TRUE",
	"WHILE",
	"WRITE"
};

#endif
