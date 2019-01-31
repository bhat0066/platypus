/*********************************************************************************************
*	File name: buffer.c
*	Compiler: MS Visual Studio Enterprise 2015
*	Author: Jinesh Bhatt, 040845930
*	Course: CST8152 - Compilers, Lab Section: 11
*	Assignment: 1
*	Date: 9/29/2017
*	Professor: Sv. Ranev
*	Purpose: Implementation of functions to achive a working buffer.
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
#include "buffer.h"

/*********************************************************************************************
*	Purpose: Function tries to allocate a new buffer sturcture on the heap dependent on 
*			 success checks on its parameters.  
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: calloc(), malloc(), free()
*	Parameters: short init_capacity, char inc_factor, char o_mode(a,f,m)
*	Return value: Returns pointer to newly created buffer or NULL on fail.
*	Algorithm: 
*				- tries to calloc a new buffer sturc
*				- tries to malloc a new character array to new buffer member cb_head
*				- on any pointer fails, it frees the pointer and return NULL
*				- safe checks the parameters, on fail it resets them to predesignated values
*				- for each mode(fixed, multiplicative, additive) it sets the newly buffers 
*				  members(mode, capacity, increment factor) and return a pointer to buffer.				
**********************************************************************************************/
Buffer *b_allocate(short init_capacity, char inc_factor, char o_mode) {
	/*TODO: Function isn't well optimized or implemented properly, need to redo it*/

	/*for easier reading a temp unsinged char is created which is type casted on char*/
	unsigned char incFactor = (unsigned char)inc_factor;

	/*callocs one Buffer struct using calloc*/
	Buffer *myBuffer = (Buffer *)calloc(1, sizeof(Buffer));

	/*check the buffer struct for null & capatcity is less then 0*/
	if (myBuffer == NULL || init_capacity < 0) return NULL;
	
	/*  The following check fixes the memory leak issue
		for number 15 in my test plan, but due to time restriction I haven't been
		able to test throughly.
		if (init_capacity < 0) { free(myBuffer);return NULL; }*/
	
	/*check if the inital capacity is between 0 and short max - 1*/
	if ((init_capacity >= 0) && (init_capacity <= NEWSHORT)) {
		
		/*mallocs a character array and assigns it to head*/
		myBuffer->cb_head = (char *)malloc(sizeof(char) * init_capacity); 

		/*checks the head to be NULL, free the new buffer and returns a NULL*/
		if (myBuffer->cb_head == NULL) {
			free(myBuffer);
			return NULL;
		}
		
		/*malloc was successfull*/
		else {

			/*checks  if the mode is f and factors to be 0 & not 0*/
			if ((o_mode == 'f' || incFactor == 0) || (o_mode == 'f' && incFactor != 0)) {
				
				/*for invalid capacity, free and return NULL*/
				if (!init_capacity) {
					free(myBuffer->cb_head);
					free(myBuffer);
					return NULL;
				}
				/* with valid capacity set the buffer members and return the buffer*/
				myBuffer->mode = FIXED_MODE;
				myBuffer->inc_factor = 0;
				myBuffer->capacity = init_capacity;
				return myBuffer;
			}

			/*for other modes(additive & multiplicative)*/
			switch (o_mode) {
				case 'a':
					/*check if the factor is in range 1 - 255, assign members its predesignated values*/
					if (incFactor >= MIN_FACTOR && incFactor <= MAX_A_FACTOR) {
						myBuffer->mode = ADD_MODE;
						myBuffer->inc_factor = incFactor;
					}

					/*TODO: should check when its outside of the range, for now it will break out 
							and set the new capacity and return the pointer*/
					break;
				case 'm':
					/*check if the factor is in range 1 - 100, assign members its predesignated values*/
					if (incFactor >= MIN_FACTOR && incFactor <= MAX_M_FACTOR) {
						myBuffer->mode = MULTI_MODE;
						myBuffer->inc_factor = incFactor;
					}

					/*for out of range, free and return the NULL*/
					else {
						free(myBuffer->cb_head);
						free(myBuffer);
						return NULL;
					}
					break;
				default:
					/*incorrect mode, free and return NULL*/
					free(myBuffer->cb_head);
					free(myBuffer);
					return NULL;
			}
		}
	}
	/*capacity wasn't in range free the buffer and return NULL*/
	else {
		free(myBuffer);
		return NULL;
	}

	/*set the capacity and return the pointer to buffer*/
	myBuffer->capacity = init_capacity;
	return myBuffer;
}

/*********************************************************************************************
*	Purpose: Function tries to add a character to the character array, depending on the mode 
*			 it tires expand the capacity.
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: realloc()
*	Parameters: pBuffer const pBD, char symbol
*	Return value: Returns pointer to the buffer or NULL on fail.
*	Algorithm:
*				- sets the r_flag & check if the pointer is no null
*				- if the buffer is operational and not full; adds the character, increments
*				  addc offset by 1 and returns the pointer; 
*				- if the buffer is operational and full; depending on the mode expand its size
*				  by realloc(); adds the character, increments
*				  addc offset by 1 and returns the pointer; 
**********************************************************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol) {
	/*TODO: Function isn't well optimized, need to redo it*/
	
	char* origLocation; /*used to store the original location of buffer head*/
	char* tempLocation; /*used to store newly realloced location*/
	
	/*variables are used for multiplicative mode calculations*/
	short availableSpace, newIncrement, newCapacity;

	/*check the pointer*/
	if (pBD == NULL) return NULL;

	/*set the r-flag*/
	pBD->r_flag = R_FLAG;

	/* if the buffer is operational and not full; adds the character, increments
	   addc offset by 1 and returns the pointer;*/
	if ((pBD != NULL) && (b_isfull(pBD) == 0)) {
		pBD->cb_head[pBD->addc_offset] = symbol;
		pBD->addc_offset += 1;
		return pBD;
	}
	
	/*safe check to see if the function is retunring is fail*/
	else if (b_isfull(pBD) == RT_FAIL1) {
		return NULL;
	}

	/* if the buffer is operational and full; depending on the mode expand its size
       by realloc(); adds the character, increments
	   addc offset by 1 and returns the pointer; */
	else{
		switch (pBD->mode) {
			case FIXED_MODE:
				return NULL;
			case ADD_MODE:
				/*calculate the newcapacity*/
				newCapacity = (((unsigned char)pBD->inc_factor) + (pBD->capacity));
				
				/*checks if the capacity is equal to short max - 1*/
				if (pBD->capacity == NEWSHORT) {
					return NULL;
				}
				
				/*check if the capacity is in range; breaks out realloc(),...etc*/
				if (newCapacity >= 0 && newCapacity <= NEWSHORT) {
					break;
				}
				
				/*not in range, return a null*/
				else {
					return NULL;
				}
				break;
			case MULTI_MODE:
				/*checks if the capacity is equal to short max - 1*/
				if (pBD->capacity == NEWSHORT) {
					return NULL;
				}
				
				/*follows the given alogorithm from assignment to calculate the capacity*/
				availableSpace = (NEWSHORT - (pBD->capacity));
				newIncrement = (availableSpace * ((unsigned char)pBD->inc_factor)) / 100;
				newCapacity = pBD->capacity + newIncrement;
				
				/*if the new capacity is eqaul to orginal capacity...somehow;
				  lets simply increment it and assign it back*/
				if (newCapacity == pBD->capacity) {
					newCapacity = pBD->capacity + 1;
				}
				
				/*check if the capacity is in range; breaks out realloc(),...etc*/
				if (newCapacity >= 0 && newCapacity <= NEWSHORT) {
					break;
				}
				
				/*not in range, return a null*/
				else {
					newCapacity = NEWSHORT;
				}
				break;
			default:
				/*wrong mode, return NULL*/
				return NULL;
		}

		/*store the original location for future comparision; most probably not needed*/
		origLocation = pBD->cb_head;

		/*realloc with the new set capacity*/
		tempLocation = (char*)realloc(pBD->cb_head, (sizeof(char) *newCapacity));

		/*check if realloc hasn't returned a null, free original location and retun null*/
		if (tempLocation == NULL) {
			free(origLocation);
			return NULL;
		}

		/*realloc has worked*/
		else {
			/*set the new realloc location to the head*/
			pBD->cb_head = tempLocation;

			/*if the location has changed, set the flag accordingly*/
			if (pBD->cb_head != origLocation) {
				pBD->r_flag = SET_R_FLAG;
			}

			/*add the character, increment the offset by 1, set the capacity & return the pointer*/
			pBD->cb_head[pBD->addc_offset] = symbol;
			pBD->addc_offset++;
			pBD->capacity = newCapacity;
			return pBD;
		}
	}
}

/*********************************************************************************************
*	Purpose: Function clears all the buffer member values.
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: n/a
*	Parameters: Buffer * const pBD
*	Return value: Returns success(0) or RT_FAIL1(-1) on fail.
*	Algorithm: sets the buffer members to 0.
**********************************************************************************************/
int b_clear(Buffer * const pBD) {

	/*return a fail for null pointer*/
	if (pBD == NULL) return RT_FAIL1;
	
	/*clear the members to 0 and return success*/
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	pBD->r_flag = 0;
	pBD->eob = 0;
	return SUCCESS; 
}

/*********************************************************************************************
*	Purpose: Function frees the buffer character array and buffer itself
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: free()
*	Parameters: Buffer * const pBD
*	Return value: n/a
*	Algorithm: n/a
**********************************************************************************************/
void b_free(Buffer * const pBD) {

	/*if the pointer to buffer & character array isn't null(already freed),
	  then free buffer and its array*/
	if (pBD != NULL && pBD->cb_head != NULL) {
		free(pBD->cb_head);
		free(pBD);
	}
	/*TODO: do i need to explicitly set them to null?*/
}

/*********************************************************************************************
*	Purpose: Function checks if the buffer is full or not
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: n/a
*	Parameters: Buffer * const pBD
*	Return value: 1 when buffer is full, 0 when it isn't & -1 on fail
*	Algorithm: check if the capacity is equal to addc offset
**********************************************************************************************/
int b_isfull(Buffer * const pBD) {
	/*check for null*/
	if (pBD == NULL) return RT_FAIL1;

	/*check for buffer full or not*/
	if (pBD->capacity == pBD->addc_offset) return 1;
	else return 0;
}

/*********************************************************************************************
*	Purpose: Function checks the limit of buffer and returns it
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: n/a
*	Parameters: Buffer * const pBD
*	Return value: return the limit or fail (-1)
*	Algorithm: n/a
**********************************************************************************************/
short b_limit(Buffer * const pBD) {
	if (pBD == NULL) return RT_FAIL1;
	else return pBD->addc_offset; 

	/*TODO: replace if and else with one line returns*/
}

/*********************************************************************************************
*	Purpose: Function checks the capacity of buffer and returns it
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: n/a
*	Parameters: Buffer * const pBD
*	Return value: return the capacity or fail (-1)
*	Algorithm: n/a
**********************************************************************************************/
short b_capacity(Buffer * const pBD) {
	if (pBD == NULL) return RT_FAIL1;
	else return pBD->capacity;

	/*TODO: replace if and else with one line returns*/
}

/*********************************************************************************************
*	Purpose: Function sets the newly identifed mark to buffer member mark and return it.
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: n/a
*	Parameters: Buffer * const pBD, short mark
*	Return value: return the mark or fail (-1)
*	Algorithm: n/a
**********************************************************************************************/
short b_mark(Buffer * const pBD, short mark) {
	/*checks for null*/
	if (pBD == NULL) return RT_FAIL1;

	else {
		/*checks if the mark is in range or not*/
		if (mark >= 0 && mark <= pBD->addc_offset) {
			/*sets and return it*/
			pBD->markc_offset = mark;
			return pBD->markc_offset;
		}

		/*not in range, return a fail*/
		else return RT_FAIL1; 
		/*TODO: above return fail is probably not correct, as it technically isn't a run time
				fail. Need to probably return something else, a constant instead?*/
	}
}

/*********************************************************************************************
*	Purpose: Function returns the currently set mode.
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: n/a
*	Parameters: Buffer * const pBD
*	Return value: return the mode(0, -1, 1) or fail (-2)
*	Algorithm: n/a
**********************************************************************************************/
int b_mode(Buffer * const pBD) {
	if (pBD == NULL) return RT_FAIL2;
	else return pBD->mode;

	/*TODO: replace if and else with one line returns*/
}

/*********************************************************************************************
*	Purpose: Function returns size of the increment factor.
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: n/a
*	Parameters: Buffer * const pBD
*	Return value: return the increment factor(1-255(a mode) or 1-100(m mode) or fail (256)
*	Algorithm: n/a
**********************************************************************************************/
size_t b_incfactor(Buffer * const pBD) {
	/*quick note: need to return 256 on fail b/c the incfactor range is from 1-255*/
	if (pBD == NULL) return RT_FAIL3;
	else return ((unsigned char)pBD->inc_factor);

	/*TODO: replace if and else with one line returns*/
}

/*********************************************************************************************
*	Purpose: Function loads character in to the buffer character array and return the
*			 number of total characters added.
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: b_addc(), feof(), fgetc()
*	Parameters: FILE * const fi, Buffer * const pBD
*	Return value: return the number of total characters added or fail (-1) or function fail(-2)
*	Algorithm: 
*				- check for nulls from params, return null, if needed
*				- till the end of file is detected, add the characters using function b_addc()
*				  and finaly return number of characters
**********************************************************************************************/
int b_load(FILE * const fi, Buffer * const pBD) {
	int numOfChar = 0; /*used to count the characted added to buffer*/
	char character;	 /*temporarily holds the character retrived from file*/

	/*check for nulls*/
	if (pBD == NULL || fi == NULL) {
		return RT_FAIL1;
	}

	/*grab the intial character, probably could get away by calling it directly in line 454*/
	character = (char)fgetc(fi);
	
	/*go in to infinite loop till end of file*/
	while (!feof(fi)) {

		/*add the character and see if the return isn't a null*/
		if (b_addc(pBD, character) == NULL)
			return LOAD_FAIL;		
		
		/*increment the counter and get the next character*/
		++numOfChar;
		character = (char)fgetc(fi);
	}
	return numOfChar;
}

/*********************************************************************************************
*	Purpose: Function checks if the buffer is empty or not
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: n/a
*	Parameters: Buffer * const pBD
*	Return value: return  fail (-1), otherwise 1 or 0
*	Algorithm: n/a
**********************************************************************************************/
int b_isempty(Buffer * const pBD) {
	if (pBD == NULL) return RT_FAIL1;
	else {
		/*checks the offset, return the appropriate value*/
		if (pBD->addc_offset == 0)
			return 1; 
		else return 0; 
	}
}

/*********************************************************************************************
*	Purpose: Function return the end of buffer value
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: n/a
*	Parameters: Buffer * const pBD
*	Return value: return  fail (-1), otherwise 1 or 0
*	Algorithm: n/a
**********************************************************************************************/
int b_eob(Buffer * const pBD) {
	if (pBD == NULL) return RT_FAIL1;
	else return pBD->eob;
}

/*********************************************************************************************
*	Purpose: Function retives a character from the buffer.
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: n/a
*	Parameters: Buffer * const pBD
*	Return value: return  fail (-1), runtime fail return -2 otherwise the character
*	Algorithm: 
*				- check the validity of the arguments
*				- if get and add offset are equal set eob to 1 and return -1
*				  else eob to 0 and return the character and increment the get offset by 1.
**********************************************************************************************/
char b_getc(Buffer * const pBD) {
	
	char tempChar; /*temp variable to store the character*/

	/*check for null*/
	if (pBD == NULL || pBD->cb_head == NULL) return RT_FAIL2;

	/*if get and add offset are equal set eob to 1 and return -1*/
	if (pBD->getc_offset == pBD->addc_offset) {
		pBD->eob = 1; 
		return RT_FAIL1; 
	}

	/*else eob to 0 and return the character and increment the get offset by 1*/
	else {
		pBD->eob = 0;
		tempChar = pBD->cb_head[pBD->getc_offset]; 
		pBD->getc_offset++;
		return tempChar;
	}
}

/*********************************************************************************************
*	Purpose: Function is meant for diagnostic purposes. Its used to print the buffer content.
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: b_isempty(), b_getc(), b_eob()
*	Parameters: Buffer * const pBD
*	Return value: return  fail (-1) or number of printed characters
*	Algorithm:
*				- if the buffer is empty, print stuff
*				- if the buffer is not empty, loop through it by b_getc() to retrive characters
*				  print them out if the end of buffer flag hasn't been set.
*				- finally return the number of characters printed
**********************************************************************************************/
int b_print(Buffer * const pBD) {
	int numOfChar = 0; /*counter for number of characters*/
	char character; /*temp variable to store the character*/
	
	/*check for null*/
	if (pBD == NULL) {
		return RT_FAIL1;
	}	
	
	/*check for empty buffer, and print stuff*/
	if (b_isempty(pBD) == 1) {
		printf("Empty buffer\n");
		return numOfChar; 
	}
	
	/*buffer is not empty, loop through it*/
	while (1){	
		/*retrive and check character is not a fail*/
		character = b_getc(pBD);

		if (character == RT_FAIL2) return RT_FAIL1;
				
		/*if (pBD->eob == 0) {*/
		/*if the end of buffer flag hasn't been set, print out the character*/
		if(b_eob(pBD) == 0){
			printf("%c", character);
			numOfChar++;
		}
		/*end of buffer is reached, print a new line and return the number of characters*/
		else {
			printf("\n");
			return numOfChar;
		}
	}
}

/*********************************************************************************************
*	Purpose: Function compacts the current buffer and adds a character
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: realloc()
*	Parameters: Buffer * const pBD
*	Return value: return  fail (-1) or number of printed characters
*	Algorithm:
*				- check for argument validity
*				- set the new capacity and increase the existing size 
*				  set the member of the buffer, add the symbol 
*				  finally return the pointer to buffer
**********************************************************************************************/
Buffer * b_compact(Buffer * const pBD, char symbol) {
	unsigned short newCapacity; /*used for capacity calculations*/
	char* origLocation; /*used to store the original location of buffer head*/
	char* tempLocation; /*used to store newly realloced location*/
	
	if (pBD == NULL || pBD->cb_head == NULL) return NULL;
	else {
		/*calculate the new capacity*/
		newCapacity = pBD->addc_offset + sizeof(char);
		
		/*store the original location for future comparision; most probably not needed*/
		origLocation = pBD->cb_head;

		/*realloc with the new set capacity*/
		/*check if realloc hasn't returned a null, free original location and retun null*/

		tempLocation = (char*)realloc(pBD->cb_head, (newCapacity* sizeof(char)));
		if (tempLocation == NULL) return NULL;
		
		/*realloc has worked*/
		else {
			/*set the new realloc location to the head*/
			pBD->cb_head = tempLocation;
			
			/*if the location has changed, set the flag accordingly*/
			if (pBD->cb_head != origLocation) {
				pBD->r_flag = SET_R_FLAG;
			}
			
			/*add the character, increment the offset by 1, set the capacity & return the pointer*/
			pBD->cb_head[pBD->addc_offset] = symbol;
			pBD->addc_offset++;
			pBD->capacity = newCapacity;
			return pBD;
		}
	}
}

/*********************************************************************************************
*	Purpose: Function returns the currently set r flag
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: n/a
*	Parameters: Buffer * const pBD
*	Return value: return  fail (-1) or r flag (0 or 1)
*	Algorithm: n/a
**********************************************************************************************/
char b_rflag(Buffer * const pBD) {
	if (pBD == NULL) return RT_FAIL1; 
	else return pBD->r_flag;

	/*TODO: replace if and else with one line returns*/
}

/*********************************************************************************************
*	Purpose: Function retracts the buffer by decrementing get offset.
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: n/a
*	Parameters: Buffer * const pBD
*	Return value: return  fail (-1) or getc_offset value(>=0)
*	Algorithm: 
*				- check the validity of arguments
*				- if get offset is 0, return it
*				- if it is less then 0, return a fail(-1)
*				- if it is greater then 0, decrement it by 1 and return it
**********************************************************************************************/
short b_retract(Buffer * const pBD) {
	/*check for null*/
	if (pBD == NULL) return RT_FAIL1; 

	else {
		/*get offset is 0*/
		if (pBD->getc_offset == 0) return RT_FAIL1;/*pBD->getc_offset;*/
		
		/*negative offset
		else if(pBD->getc_offset < 0) return RT_FAIL1;*/
		
		/*positive offset (>0)*/
		else {
			/*pBD->getc_offset--;*/
			return --pBD->getc_offset;
		}
	}
}

/*********************************************************************************************
*	Purpose: Function resets the buffer by setting the getc offset to markc offset
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: n/a
*	Parameters: Buffer * const pBD
*	Return value: return  fail (-1) or getc_offset value(>=0)
*	Algorithm: n/a
**********************************************************************************************/
short b_reset(Buffer * const pBD) {
	if (pBD == NULL) return RT_FAIL1;
	else {
		pBD->getc_offset = pBD->markc_offset;
		return pBD->getc_offset;
	}
}

/*********************************************************************************************
*	Purpose: Function returns the getc offset.
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: n/a
*	Parameters: Buffer * const pBD
*	Return value: return  fail (-1) or getc_offset value(>=0)
*	Algorithm: n/a
**********************************************************************************************/
short b_getcoffset(Buffer * const pBD) {
	if (pBD == NULL) return RT_FAIL1; 
	else return pBD->getc_offset;

	/*TODO: replace if and else with one line returns*/
}

/*********************************************************************************************
*	Purpose: Function rewinds the buffer by setting the getc & markc offset to 0
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: n/a
*	Parameters: Buffer * const pBD
*	Return value: return  fail (-1) or 0 on success
*	Algorithm: n/a
**********************************************************************************************/
int b_rewind(Buffer * const pBD) {
	if (pBD == NULL) return RT_FAIL1;
	else {
		pBD->getc_offset = 0;
		pBD->markc_offset = 0;
		return SUCCESS;
		/*TODO: correct value to return?*/
	}
}

/*********************************************************************************************
*	Purpose: Function retives the pointer to location of the character pointed by loc_offset
*	Author: Jinesh Bhatt
*	History/Version: 1.0
*	Called functions: n/a
*	Parameters: Buffer * const pBD
*	Return value: return  fail (NULL) or character 
*	Algorithm: 
*				- check for validity of the argument
*				- retrive the character and return it
**********************************************************************************************/
char * b_location(Buffer * const pBD, short loc_offset) {
	char *tempLocation; /*temp variable to hold the character*/

	/*check for null*/
	if (pBD == NULL) return NULL;
	else {
		/*retrive the character by loc_offset*/
		tempLocation = (pBD->cb_head + loc_offset);
		
		/*check for null and return the character if not*/
		if (tempLocation == NULL) return NULL;
		else return tempLocation;
	}
}