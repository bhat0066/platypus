 /*********************************************************************************************
 *	File name: scanner.c
 *	Compiler: MS Visual Studio Enterprise 2015
 *	Author: Jinesh Bhatt, 040845930
 *	Course: CST8152 - Compilers, Lab Section: 11
 *	Assignment: 2
 *	Date: 12/7/2017
 *	Professor: Sv. Ranev
 *	Purpose: SCANNER.C: Functions implementing a Lexical Analyzer (Scanner)
 *			as required for CST8152, Assignment #2
 *			scanner_init() must be called before using the scanner.
 *	Function List:
 *					int scanner_init(),
 *					Token malar_next_token(),
 *					int get_next_state(),
 *					int char_class(),
 *					Token aa_func02(),
 *					Token aa_func03(),
 *					Token aa_func05(),
 *					Token aa_func08(),
 *					Token aa_func10(),
 *					Token aa_func12(),
 *					long atolh(),
 *					int iskeyword()
 **********************************************************************************************/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */ 
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */
static long atolh(char * lexeme); /* converts hexadecimal string to decimal value */

/*********************************************************************************************
*	Purpose: Function intializes the scanner.
*	Author: Svillen Ranev
*	History/Version: 1.0
**********************************************************************************************/
int scanner_init(Buffer * sc_buf) {
  	if(b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(sc_buf);
	b_clear(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*********************************************************************************************
*	Purpose: function malar_next_token() performs the token 
*			 recognition. It “reads” the lexeme from the input stream one character at a time
*			 and returns a token structure any time it finds a token pattern. 
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: b_getcoffset(), b_getc(), strcp(), ...,etc
*	Parameters: a pointer to buffer
*	Return value: Returns a token of the newly created item.
*	Algorithm:
*				- a very complex alorithm which looks at each char and proccess it
*					to a token.
**********************************************************************************************/
Token malar_next_token(Buffer * sc_buf)
{
	Token t; /* token to return after recognition */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/
	int accept = NOAS; /* type of state - initially not accepting */
	
	 /* DECLARE YOUR LOCAL VARIABLES HERE IF NEEDED */
	short tempSize;
	 /* endless loop broken by token returns it will generate a warning */
	while (1) {

		/* GET THE NEXT SYMBOL FROM THE INPUT BUFFER */
		c = b_getc(sc_buf);

		/* special cases or token driven processing */
		switch (c) {
			
			case SEOF_255:
			case SEOF_0:
				t.code = SEOF_T;			/* EOF cases */
				return t; 
			case ' ':
				continue;					/* Ignore white space */ 
			case '\t':
				continue;					/* Ignore tabs */
			case '\n':
				line++;
				continue;					/* Ignore new line, increment line count */
			case ';':
				t.code = EOS_T;
				return t;					/* End of statement */
			case ',':
				t.code = COM_T;
				return t;					/* Comma */
			case '{':
				t.code = LBR_T;
				return t;					/* Left brace */
			case '}':
				t.code = RBR_T;
				return t;					/* Right brace */
			case '(':
				t.code = LPR_T;
				return t;					/* Left parenthesis */
			case ')':
				t.code = RPR_T;
				return t;					/* Right parenthesis */
			case '#':
				t.code = SCC_OP_T;
				return t;					/* strong concatination token */
			case '+':
				t.code = ART_OP_T;
				t.attribute.arr_op = PLUS;
				return t;					/* Addition operator */
			case '-':
				t.code = ART_OP_T;
				t.attribute.arr_op = MINUS;
				return t;					/* Substraction operator */
			case '*':
				t.code = ART_OP_T;
				t.attribute.arr_op = MULT;
				return t;					/* Multiplication operator */
			case '/':
				t.code = ART_OP_T;
				t.attribute.arr_op = DIV;
				return t;					/* Devision operator */
			case '>':
				t.code = REL_OP_T;
				t.attribute.rel_op = GT;
				return t;					/* Greater-than relational operator */
			case '<':
				c = b_getc(sc_buf);
				if (c == '>') {
					t.code = REL_OP_T;
					t.attribute.rel_op = NE; /* not equal operator */
				}
				else {
					b_retract(sc_buf);
					t.code = REL_OP_T;
					t.attribute.rel_op = LT; /* less-than relational operator */
				}
				return t;
			case '=':
				c = b_getc(sc_buf);
				if (c == '=') {
					t.code = REL_OP_T;
					t.attribute.rel_op = EQ; /* not equal operator */
				}
				else {
					b_retract(sc_buf);
					t.code = ASS_OP_T;
				}
				return t;
			case '.':

				/*mark the startof the location*/
				b_mark(sc_buf, b_getcoffset(sc_buf)); /* safe gaurd check here??*/

				/* get the next char*/
				c = b_getc(sc_buf);

				/* lets look for AND, set the code and attribute and return it*/
				if (c == 'A') {
					if (b_getc(sc_buf) == 'N') {
						if (b_getc(sc_buf) == 'D') {
							if (b_getc(sc_buf) == '.') {
								t.code = LOG_OP_T;
								t.attribute.log_op = AND; /* and operator */
								return t;
							}
						}
					}
				}

				/* lets look for OR, set the code and attribute and return it*/
				if (c == 'O') {
					if (b_getc(sc_buf) == 'R') {
						if (b_getc(sc_buf) == '.') {
							t.code = LOG_OP_T;
							t.attribute.log_op = OR; /* OR operator */
							return t;
						}
					}
				}

				
				/*didn't find either; so set the error token, reset and return it*/
				t.code = ERR_T;
				b_reset(sc_buf);
				t.attribute.err_lex[0] = '.';
				t.attribute.err_lex[1] = '\0';
				return t;

			case '!':
				/*mark the startof the location*/
				b_mark(sc_buf, b_getcoffset(sc_buf)); /* safe gaurd check here??*/

				 /* get the next char*/
				c = b_getc(sc_buf);

				/* lets look for next '!'; to make sure its a comment*/
				if (c == '!') {
					
					/* go through the meat of the comment till we hit 
						a new line, end of file char*/
					while (c != '\n' && c != SEOF_0 && c != SEOF_255) {
						c = b_getc(sc_buf);
					}

					/* retract the buffer and continue*/
					b_retract(sc_buf);
					continue;
				}

				/* next '!' comment symbol wasn't found, so create error token*/
				else {
					/* retact it to include the previous incorrect char*/
					b_retract(sc_buf);

					/* get that previous incorrect char*/
					c = b_getc(sc_buf);
				
					/* set the code and '!c\o' int to err_lex array 
						as per assignment specs*/
					t.code = ERR_T;
					t.attribute.err_lex[0] = '!';
					t.attribute.err_lex[1] = c;
					t.attribute.err_lex[2] = '\0';

					/*  go through the meat of the comment till we hit 
					a new line, end of file char*/
					while (c != '\n' && c != SEOF_0 && c != SEOF_255) {
						c = b_getc(sc_buf);

						/* for a new line, increment and return the token*/
						if (c == '\n') {
							line++;
							return t;
						}
					}
				}

			case '"':
				
				/*mark the startof the location & set the size*/
				lexstart = b_mark(sc_buf, b_getcoffset(sc_buf));
				tempSize = b_limit(str_LTBL);

				/* empty string; if the next char is '"'
					set the code, attribute, offset, add '\0'
					and return it*/
				if (b_getc(sc_buf) == '"') {
					t.code = STR_T;
					t.attribute.str_offset = b_limit(str_LTBL);
					b_addc(str_LTBL, '\0');
					return t;
				}

				/* try processing the rest of the string*/
				else {

					/* might create a warning; going to infinte loop*/
					for (;;) {
						/* grab the next char*/
						c = b_getc(sc_buf);

						/* next char is '"'*/
						if (c == '"') {\
							/* reset it and grab the char before*/
							b_reset(sc_buf);
							c = b_getc(sc_buf);

							/* might create a warning; going to infite loop*/
							for(;;) {
								
								/* grab the next char, copy it and check for closing '"'
								   when found break out */
								if (c != '"') {
									b_addc(str_LTBL, c);
									c = b_getc(sc_buf);
								}
								else
									break;
							}

							/* set the code, attribute, offset, add '\0'
								and return it*/
							t.code = STR_T;
							t.attribute.str_offset = tempSize;
							b_addc(str_LTBL, '\0');
							return t;
						}

						/* next char is end of file char*/
						if (c == SEOF_0 || c == SEOF_255) {
							
							/* set the code to error, set the lexend, 
								reset and retract the buff*/
							t.code = ERR_T;
							lexend = b_getcoffset(sc_buf)-1;
							b_reset(sc_buf);
							b_retract(sc_buf);

							/* check if the string is greater then err_len(20)*/
							if ((lexend - lexstart) > ERR_LEN) {
								
								/* copy only first 17 char t err_lex array*/
								for (int i = 0; i < (ERR_LEN - 3); i++)
									t.attribute.err_lex[i] = b_getc(sc_buf);

								/* for last index's 17, 18, 19, 20 add "...\0"*/
								t.attribute.err_lex[(ERR_LEN - 1)] = '.';
								t.attribute.err_lex[(ERR_LEN - 2)] = '.';
								t.attribute.err_lex[(ERR_LEN - 3)] = '.';
								t.attribute.err_lex[ERR_LEN] = '\0';

								/* go the end of the string by incrementing the
									getcoffset to lexend*/
								for(int i = b_getcoffset(sc_buf); i <= lexend; i++)
									c = b_getc(sc_buf);

								/* retract the buffer and return the token*/
								b_retract(sc_buf);
								return t;
							}

							/* else the string is okay, copy and return the token*/
							else {
								/* copy the string to err_lex arrray*/
								for (int i = 0; i < (lexend - lexstart); i++)
									t.attribute.err_lex[i] = b_getc(sc_buf);

								/* add char '\0' at the end and return it*/
								t.attribute.err_lex[ERR_LEN] = '\0';
								return t;
							}
						}

						/* for every new line increment the counter*/
						if (c == '\n')
							line++;
					}
				}
			/* go to final state machine*/
			default:
				break;
			}
		
		/* for alphabets and digits*/
		if (isalpha(c) || isdigit(c)) {

			/* using b_mark set the begining of the lexeme and save it in lexstart*/
			lexstart = b_mark(sc_buf, b_getcoffset(sc_buf)-1); 
			
			/* Final State machine algorithm:
				FSM0. Begin with state = 0 and the input character c
				FSM1. Get the next state from the transition table calling
						state = get_next_state(state, c, &accept);
				FSM2. Get the next character
				FSM3. If the state is not accepting (accept == NOAS), go to step FSM1
					  If the step is accepting, token is found, leave the machine and
				      call an accepting function. */
			
			/* get the state*/
			state = get_next_state(state, c, &accept);
			
			/*  while state is not accepting (NOAS), go to step FSM1*/
			while (accept == NOAS) {
				c = b_getc(sc_buf);
				state = get_next_state(state, c, &accept);
			}
			
			/*  if state is accepting, retract the machine*/
			if (accept == ASWR) 
				b_retract(sc_buf);

			/* set lexend to offset*/
			lexend = b_getcoffset(sc_buf);

			/* allocate a temporary lexeme buffer*/
			lex_buf = b_allocate((((short) lexend-lexstart) + 1), 0, 'f');
			
			/* if it was null create and return a error token*/
			if(lex_buf==NULL) {
				t.code = ERR_T;
				strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
				scerrnum = 1;
				return t;
			}
			
			/* using reset retract the getc_offset*/
			b_reset(sc_buf);

			/* using b_getc() copy the lexeme from lexstart to lexend
				from the input stream (b_getc()) to lex_buf by using b_addc() */
			for (int i = lexstart; i < lexend; i++)
				b_addc(lex_buf, b_getc(sc_buf));

			/* add a '\0' using b_addc()*/
			b_addc(lex_buf, '\0');

			/* using the accepting function the array index as state 
			   and location is used to create a token for VID(S/A), FPL, HIL, IL and ERR*/
			t = aa_table[state](b_location(lex_buf, 0));
			
			/* frre the lex_buf & return the token*/
			b_free(lex_buf);
			return t;
		}

		/* for illegal chars*/
		else{
			/* create a error token and return it */
			t.code = ERR_T;
			t.attribute.err_lex[0] = c;
			t.attribute.err_lex[1] = '\0';
			return t;
		}
                                       
   }//end while(1)
}


/*********************************************************************************************
*	Purpose: Function gets the next state
*	Author: Svillen Ranev
*	History/Version: 1.0
**********************************************************************************************/
int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
printf("Input symbol: %c Row: %d Column: %d Next: %d \n",c,state,col,next);
#endif
	/*
	The assert(int test) macro can be used to add run-time diagnostic to programs
	and to "defend" from producing unexpected results.
	assert() is a macro that expands to an if statement;
	if test evaluates to false (zero) , assert aborts the program
	(by calling abort()) and sends the following message on stderr:

	Assertion failed: test, file filename, line linenum

	The filename and linenum listed in the message are the source file name
	and line number where the assert macro appears.
	If you place the #define NDEBUG directive ("no debugging")
	in the source code before the #include <assert.h> directive,
	the effect is to comment out the assert statement.
	*/
    assert(next != IS);

	/*
	The other way to include diagnostics in a program is to use
	conditional preprocessing as shown bellow. It allows the programmer
	to send more details describing the run-time problem. 
	Once the program is tested thoroughly #define DEBUG is commented out
	or #undef DEBUF is used - see the top of the file.
	*/ 
#ifdef DEBUG
	if(next == IS){
	  printf("Scanner Error: Illegal state:\n");
	  printf("Input symbol: %c Row: %d Column: %d\n",c,state,col);
	  exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/*********************************************************************************************
*	Purpose: Function return the column number in the transition table st_table for the input
*			 character c.
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: isalpha()
*	Parameters: char c
*	Return value: Returns a value dependent on the type of char which correspond to column.
**********************************************************************************************/
int char_class(char c)
{
		int val;	/*temp return value holder*/
		
		/* look for all the alphabets here*/
		if (isalpha(c)) {
			/* for hex x set 1*/
			if (c == 'x')
				val = 1;
			/* for hex A - F set 2*/
			else if ((c >= 'A') && (c <= 'F'))
				val = 2;
			/*rest of alphabet chars set 0*/
			else
				val = 0;
		}
		/* everything else that is not alphabet*/
		else {
			if (c == '0')
				val = 3;
			/* from 1-9 set 4*/
			else if (c >= '1' && c <= '9')
				val = 4;
			else if (c == '.')
				val = 5;
			else if (c == '$')
				val = 6;
			/* all the other character symbols set 7*/
			else
				val = 7;
		}

		return val;
}

/*********************************************************************************************
*	Purpose: Accepting function for the arithmentic variable identifier AND keywords 
*			(VID - AVID/KW).
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: iskeyword(), strlen(), strcpy()
*	Parameters: char lexeme []
*	Return value: returns a newly created error or VID - AVID/KW token
*	Algorithm:
*				- check if the lexeme is keyword
*				- if yes, sets the token with the corrresponding attribute
*					for the keyword and code.
*				- else sets a AVID token and check if it is longer then vid_len
*					if so it copies 7 chars and add '\0' at the end regardless
*				- returns the token
**********************************************************************************************/
Token aa_func02(char lexeme[]){
	int keywordIndex; /* temp keyword index holder*/
	Token myToken;	  /* temp token*/

	/* check and set the index of the keyword*/
	keywordIndex = iskeyword(lexeme);	

	/* check if the lexeme is keyword sets the token 
	with the corrresponding attribute for the keyword and code.*/
	if (keywordIndex != RT_FAIL1) {
		myToken.code = KW_T;
		myToken.attribute.kwt_idx = keywordIndex;	
	}
	else {
		/*else sets a AVID token */
		myToken.code = AVID_T;

		/* checks if lexeme is longer then vid_len
			if yes it copies 8 chars 
			else all of the chars*/
		if (strlen(lexeme) > VID_LEN) {
			for (int i = 0; i < VID_LEN; i++)
				myToken.attribute.vid_lex[i] = lexeme[i];
		}
		else 
			strcpy(myToken.attribute.vid_lex, lexeme);
		
		/* regardless add '\0' at the end */
		myToken.attribute.vid_lex[VID_LEN] = '\0'; 
	}
	return myToken;
}

/*********************************************************************************************
*	Purpose: Accepting function for the string variable identifier AND keywords
*			(VID - SVID).
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: strlen(), strcpy()
*	Parameters: char lexeme []
*	Return value: returns a newly created error or VID - SVID token
*	Algorithm:
*				- sets a SVID token
*				- if the lexme is longer than vid_len, then first 7 char are stored with '$'
*				- else copy it all.
*				- add '\0' at the end regardless
*				- returns the token
**********************************************************************************************/
Token aa_func03(char lexeme[]){
	Token myToken;	/* temp token*/

	/* sets a SVID token*/
	myToken.code = SVID_T;

	/*if the lexme is longer than vid_len*/ 
	if (strlen(lexeme) > VID_LEN) {

		/*then first 7 char are stored into the array*/
		for (int i = 0; i < (VID_LEN-1); i++)
			myToken.attribute.vid_lex[i] = lexeme[i];

		/* at the end add '$'*/
		myToken.attribute.vid_lex[VID_LEN-1] = '$';
	}
	
	/*else copy all the chars into the array*/
	else
		strcpy(myToken.attribute.vid_lex, lexeme);
	
	/* regardless add '\0' at the end */
	myToken.attribute.vid_lex[VID_LEN] = '\0';
	
	return myToken;
}

/*********************************************************************************************
*	Purpose: Accepting function for the floating-point literal (FPL).
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: atof(), aa_table[]()
*	Parameters: char lexeme []
*	Return value: returns a newly created error or FPL token
*	Algorithm:
*				- if the lexme converted to float isn't correct range and not 0.0
*					call error printing function
*				- else set the attribute and code.
*				- returns the token
**********************************************************************************************/
Token aa_func08(char lexeme[]){
	Token myToken;		/* temp token*/
	double tempValue;	/* temp double value storage*/

	/* convert the lexeme to float using atof and hold it in tempValue*/
	tempValue = atof(lexeme);

	/* type cast the double to float and check if its out side the bounds and 0.0*/
	if (((float)tempValue > FLT_MAX) || ((float)tempValue < FLT_MIN) && (tempValue != 0.0)) 
		return aa_table[ES](lexeme); /* for error call the error printing and checking function*/
	
    /* else set the token code and attribute value*/
	else {
		myToken.code = FPL_T;
		myToken.attribute.flt_value = (float)tempValue;
	}	

	return myToken;
}

/*********************************************************************************************
*	Purpose: Accepting function for the integer literal(IL) - decimal constant (DIL).
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: atoi(), aa_table[]()
*	Parameters: char lexeme []
*	Return value: returns a newly created error or DIL token
*	Algorithm:
*				- if the lexme converted to int isn't correct range and length
*					call error printing function
*				- else set the attribute and code.
*				- returns the token
**********************************************************************************************/
Token aa_func05(char lexeme[]){

	/*THE FUNCTION MUST CONVERT THE LEXEME REPRESENTING A DECIMAL CONSTANT
	TO A DECIMAL INTEGER VALUE, WHICH IS THE ATTRIBUTE FOR THE TOKEN.
	THE VALUE MUST BE IN THE SAME RANGE AS the value of 2-byte integer in C.
	IN CASE OF ERROR (OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
	THE ERROR TOKEN ATTRIBUTE IS  lexeme. IF THE ERROR lexeme IS LONGER
	than ERR_LEN characters, ONLY THE FIRST ERR_LEN-3 characters ARE
	STORED IN err_lex. THEN THREE DOTS ... ARE ADDED TO THE END OF THE
	err_lex C-type string. 
	BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE
	  return t;*/

	Token myToken;	/* temp token*/
	int tempValue;  /* temp int value storage*/

	/* convert the lexeme to int using atoi and hold in tempValue*/
	tempValue = atoi(lexeme);

	/* type cast the int to short and check if its out side the bounds of short 
	   or lenght of lexeme is greater then integer length*/
	if ((((short)tempValue > SHRT_MAX) || ((short)tempValue < 0) ) || (strlen(lexeme) > INL_LEN)) 
		return aa_table[ES](lexeme); /* for error call the error printing and checking function*/

	/* else set the token code and attribute value*/
	else {
		myToken.code = INL_T;
		myToken.attribute.int_value = (short) tempValue;
	}

	return myToken;
}

/*********************************************************************************************
*	Purpose: Accepting function for the integer literal(IL) - hexadecimal constant (HIL).
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: atoi(), aa_table[]()
*	Parameters: char lexeme []
*	Return value: returns a newly created error or HIL token
*	Algorithm:
*				- if the lexme converted to int isn't in correct range and length
*					call error printing function
*				- else set the attribute and code.
*				- returns the token
**********************************************************************************************/
Token aa_func10(char lexeme[]){

	Token myToken;	/* temp token*/
	long tempValue;	/* temp long value storage*/

	/* convert the lexeme using altoh and hold in tempValue*/
	tempValue = atolh(lexeme);

	/* type cast the long to short and check if its out side the bounds of short
	and lenght of lexeme is greater then integer length*/
	if ((((short)tempValue > SHRT_MAX) || ((short)tempValue < 0)) && (strlen(lexeme) > INL_LEN))
		return aa_table[ES](lexeme);	/* for error call the error printing and checking function*/

	/* else set the token code and attribute value*/
	else {
		myToken.code = INL_T;
		myToken.attribute.int_value = (short) tempValue;
	}

	return myToken;
}

/*********************************************************************************************
*	Purpose: Accepting function for error token.
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: strlen(), strcpy()
*	Parameters: char lexeme []
*	Return value: returns a newly created error token
*	Algorithm:
*				- set error token code
*				- if the lexme is longer than err_len chars then only first 17 are copied
*					followed by "..." in to err_lex array
*				- else copy all the chars to err_lex array
*				- to the err_lex array add '\0' at the end
*				- returns the token
**********************************************************************************************/
Token aa_func12(char lexeme[]){
	Token myToken;	/* temp token*/

	/* set code to error token*/
	myToken.code = ERR_T;

	/* if the lexme is longer than err_len chars(20) 
		then only first 17 are copied in to err_lex array*/
	if (strlen(lexeme) > ERR_LEN) {
		for (int i = 0; i < (ERR_LEN - 3); i++)
			myToken.attribute.err_lex[i] = lexeme[i];

		/* add "..." in to err_lex array @ location 19, 18, 17*/
		myToken.attribute.err_lex[ERR_LEN - 1] = '.';
		myToken.attribute.err_lex[ERR_LEN - 2] = '.';
		myToken.attribute.err_lex[ERR_LEN - 3] = '.';
	}

	/* else copy all the chars to err_lex array*/
	else
		strcpy(myToken.attribute.err_lex, lexeme);

	/* add char '\0' at the end of err_lex array*/
	myToken.attribute.err_lex[ERR_LEN] = '\0';
	return myToken;
}

/*********************************************************************************************
*	Purpose: Function acts as conversiton function for hex
*	Author: Jinesh Bhatt
*	History/Version: 1.1
*	Called functions: strtol()
*	Parameters: char* lexeme 
*	Return value: returns a integer value
**********************************************************************************************/
long atolh(char * lexeme){

	/*THE FUNCTION CONVERTS AN ASCII STRING
	REPRESENTING AN HEXADECIMAL INTEGER CONSTANT TO INTEGER VALUE*/
	return strtol(lexeme, NULL, 16);
}

/*********************************************************************************************
*	Purpose: Function searched keyword table for keywords and return the index if
*			 a keyword is found.
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: strcmp()
*	Parameters: char lexeme []
*	Return value: returns a index or -1
*	Algorithm:
*				- loop through table looking for keyword
*					if found return its index
*				- else return -1 or RT_FAIL1
**********************************************************************************************/
int iskeyword(char * kw_lexeme){
	
	/* loop through table looking for keyword
		if found return its index*/
	for (int i = 0; i < KWT_SIZE; i++) {
		if (strcmp(kw_table[i], kw_lexeme) == 0) {
			return i;
		}
	}

	/* since no keyword is found return -1 or RT_FAIL1*/
	return RT_FAIL1;
}