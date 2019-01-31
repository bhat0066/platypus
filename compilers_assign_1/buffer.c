/*
* File name: buffer.c
* Compiler: MS Visual Studio Enterprise 2015 Update 3
* Author: Silviu Riley, 040836045
* Course: CST8152 - Compilers, Lab Section: 13
* Assignment: 1
* Date: 2017-10-13
* Professor: Sv. Ranev
* Purpose: Functions that implement a buffer
*/

#include "buffer.h"

/*
* Purpose: Creates memory for buffer descriptor and character buffer
* Author: Silviu Riley
* Version: 10
* Called functions: calloc(), sizeof(), malloc(), free()
* Parameters: short(0 to SHRT_MAX - 1), char(0, 1 to 100, or 1 to 255), char(a, f, m)
* Return value: NULL or Buffer *
* Algorithm: Allocates heap memory, checks operational mode, and sets descriptor members to initial values
*/
Buffer *b_allocate(short init_capacity, char inc_factor, char o_mode) {
    Buffer *tempDescriptor;
    unsigned char increment = (unsigned char)inc_factor;
    
    /* check capacity and fail if not within range */
    if (init_capacity < CAPACITY_MIN || init_capacity > CAPACITY_MAX) {
        return NULL;
    }

    /* space for the buffer descriptor */
    tempDescriptor = calloc(1, sizeof(Buffer));
    
    /* memory allocated successfully? */
    if (tempDescriptor == NULL) {
        return NULL;
    }

    /* space for character buffer with user-supplied capacity */
    tempDescriptor->cb_head = malloc(init_capacity);
    
    /* memory allocated successfully? */
    if (tempDescriptor->cb_head == NULL) {
        /* prevent memory leak of buffer descriptor */
        free(tempDescriptor);

        return NULL;
    }

    /* determine buffer mode */
    if ((o_mode == MODE_F_TXT || increment == 0) &&
        init_capacity > 0
        ) {
        /* fixed-size buffer with no increment factor */
        tempDescriptor->mode = MODE_F;
        tempDescriptor->inc_factor = INCREMENT_F;
    }
    else if (o_mode == MODE_A_TXT &&
        increment >= INCREMENT_A_MIN &&
        increment <= INCREMENT_A_MAX
        ) {
        /* additive self-incrementing buffer */
        tempDescriptor->mode = MODE_A;
        tempDescriptor->inc_factor = inc_factor;
    }
    else if (o_mode == MODE_M_TXT &&
        increment >= INCREMENT_M_MIN &&
        increment <= INCREMENT_M_MAX
        ) {
        /* multiplicative self-incrementing buffer */
        tempDescriptor->mode = MODE_M;
        tempDescriptor->inc_factor = inc_factor;
    }
    /* improper mode parameter */
    else {
        /* prevent memory leak of buffer and buffer descriptor */
        free(tempDescriptor->cb_head);
        free(tempDescriptor);

        return NULL;
    }

    /* keep initial capacity for self-incrementing modes */
    tempDescriptor->capacity = init_capacity;

    /* initialize other descriptor members */
    tempDescriptor->addc_offset = 0;
    tempDescriptor->getc_offset = 0;
    tempDescriptor->markc_offset = 0;
    tempDescriptor->r_flag = FALSE;
    tempDescriptor->eob = FALSE;

    /* validated buffer is returned */
    return tempDescriptor;
}

/*
* Purpose: Add character to the character buffer
* Author: Silviu Riley
* Version: 10
* Called functions: realloc()
* Parameters: pBuffer(non-NULL), char(any ASCII)
* Return value: NULL or pBuffer
* Algorithm: Checks for space, expands capacity if needed, and add character
*/
pBuffer b_addc(pBuffer const pBD, char symbol) {
    char *tempBuffer;
    unsigned short interimCapacity = 0, interimIncrement = 0;
    unsigned short factor = (unsigned char)pBD->inc_factor;
    
    /* might have to reallocate so reset the flag */
    pBD->r_flag = FALSE;

    /* have to increment buffer? */
    if (pBD->addc_offset == pBD->capacity) {
        /* fixed mode */
        if (pBD->mode == MODE_F) {
            /* can't increment in fixed mode */
            return NULL;
        }

        /* is the buffer already at capacity? */
        if (pBD->capacity == CAPACITY_MAX) {
            return NULL;
        }

        /* additive mode */
        if (pBD->mode == MODE_A) {
            /* calculate the new capacity */
            interimCapacity = pBD->capacity + factor;

            /* additive mode can't overflow */
            if (interimCapacity > CAPACITY_MAX) {
                return NULL;
            }
        }
        /* multiplicative mode */
        else if (pBD->mode == MODE_M) {
            /* calculate new increment based a percent of available space */
            interimIncrement = (CAPACITY_MAX - pBD->capacity) * factor / 100;

            /* prevent increment from leading to zero capacity when casted */
            if (interimIncrement == 0) {
                /* still space left but less than can be stored in short
                so setting to max capacity so next time char buffer 
                memory space expands */
                interimCapacity = CAPACITY_MAX;
            }
            else {
                /* can successfully add increment and capacity will not 
                cause issues when stored in short */
                interimCapacity = pBD->capacity + interimIncrement;
            }
            
            /* check for overflow or if interim capacity actually been changed? */
            if (interimCapacity > CAPACITY_MAX || interimCapacity == pBD->capacity) {
                interimCapacity = CAPACITY_MAX;
            }
        }

        /* extend the size of the buffer */
        tempBuffer = realloc(pBD->cb_head, (short)interimCapacity);

        /* memory allocated successfully? */
        if (tempBuffer == NULL) {
            return NULL;
        }

        /* buffer reallocated in memory? */
        if (tempBuffer != pBD->cb_head) {
            pBD->r_flag = SET_R_FLAG;
        }

        /* store successfully incremented character buffer */
        pBD->cb_head = tempBuffer;
        /* store new capacity for next expansion */
        pBD->capacity = (short)interimCapacity;
    }

    /* there's enough space to store the character */
    pBD->cb_head[pBD->addc_offset] = symbol;
    /* account for newly added character */
    ++pBD->addc_offset;

    /* successfully added character to buffer */
    return pBD;
}

/*
* Purpose: Reset descriptor to appear as if character buffer is empty
* Author: Silviu Riley
* Version: 10
* Called functions: none
* Parameters: Buffer *
* Return value: int(RT_FAIL1 or SUCCESS)
* Algorithm: Reset all offsets and flags
*/
int b_clear(Buffer *const pBD) {
    /* is provided pointer initialized? */
    if (pBD == NULL) {
        return RT_FAIL1;
    }

    /* reset offsets and flags to appear as if character buffer is empty */
    pBD->addc_offset = 0;
    pBD->getc_offset = 0;
    pBD->markc_offset = 0;
    pBD->r_flag = FALSE;
    pBD->eob = FALSE;

    /* successfully made character buffer appear empty */
    return SUCCESS;
}

/*
* Purpose: Free memory associated with character buffer and/or buffer descriptor
* Author: Silviu Riley
* Version: 10
* Called functions: free()
* Parameters: Buffer *
* Return value: void
* Algorithm: Free character buffer first, then descriptor
*/
void b_free(Buffer *const pBD) {
    pBuffer descriptorToFree = pBD;

    /* is provided pointer initialized? */
    if (descriptorToFree != NULL) {
        /* free character buffer in descriptor before descriptor itself */
        free(descriptorToFree->cb_head);
    }
    
    /* free descriptor itself now that buffer is free */
    free(descriptorToFree);
}

/*
* Purpose: Find out if buffer is full
* Author: Silviu Riley
* Version: 10
* Called functions: none
* Parameters: Buffer *
* Return value: int(RT_FAIL1, TRUE, or FALSE)
* Algorithm:
*/
int b_isfull(Buffer *const pBD) {
    /* is provided pointer initialized? */
    if (pBD == NULL) {
        return RT_FAIL1;
    }

    /* character adding offset indicates whether buffer is full */
    if (pBD->addc_offset == pBD->capacity) {
        return TRUE;
    }

    /* can still add characters to buffer so not full */
    return FALSE;
}

/*
* Purpose: Find out the space used so far in the buffer
* Author: Silviu Riley
* Version: 10
* Called functions: none
* Parameters: Buffer *
* Return value: short(RT_FAIL1, or 0 to CAPACITY_MAX)
* Algorithm:
*/
short b_limit(Buffer *const pBD) {
    /* is provided pointer initialized? */
    if (pBD == NULL) {
        return RT_FAIL1;
    }

    /* amount of space in chars that is being used by the character buffer 
    since addc_offset is measured in chars, it indicates the space used */
    return pBD->addc_offset;
}

/*
* Purpose: Find out the capacity of the buffer
* Author: Silviu Riley
* Version: 10
* Called functions: none
* Parameters: Buffer *
* Return value: short(RT_FAIL1, or CAPACITY_MIN to CAPACITY_MAX)
* Algorithm:
*/
short b_capacity(Buffer *const pBD) {
    /* is provided pointer initialized? */
    if (pBD == NULL) {
        return RT_FAIL1;
    }

    /* provides the capacity of the buffer */
    return pBD->capacity;
}

/*
* Purpose: Set a mark offset in the buffer descriptor
* Author: Silviu Riley
* Version: 10
* Called functions: none
* Parameters: Buffer *, short(0 to addc_offset)
* Return value: short(RT_FAIL1, or 0 to addc_offset)
* Algorithm: Validate range of mark and set mark offset
*/
short b_mark(Buffer *const pBD, short mark) {
    /* is provided pointer initialized? */
    if (pBD == NULL) {
        return RT_FAIL1;
    }

    /* check if mark is within bounds */
    if (mark < 0 || mark > pBD->addc_offset) {
        /* cannot mark before beginning of character buffer
           or past the newest character */
        return RT_FAIL1;
    }

    /* set the validated mark in the buffer description */
    pBD->markc_offset = mark;

    /* provide the mark offset */
    return pBD->markc_offset;
}

/*
* Purpose: Find out the mode of the buffer
* Author: Silviu Riley
* Version: 10
* Called functions: none
* Parameters: Buffer *
* Return value: int(RT_FAIL2, MODE_M, MODE_F, or MODE_A)
* Algorithm:
*/
int b_mode(Buffer *const pBD) {
    /* is provided pointer initialized? */
    if (pBD == NULL) {
        return RT_FAIL2;
    }

    /* provide the operational mode of the buffer */
    return pBD->mode;
}

/*
* Purpose: Find the increment factor of the buffer
* Author: Silviu Riley
* Version: 10
* Called functions: none
* Parameters: Buffer *
* Return value: size_t(INCREMENT_FAIL, 0, 1 to 100, or 1 to 255)
* Algorithm:
*/
size_t b_incfactor(Buffer *const pBD) {
    /* is provided pointer initialized? */
    if (pBD == NULL) {
        return INCREMENT_FAIL;
    }

    /* provide the increment factor of the buffer */
    return (unsigned char)pBD->inc_factor;
}

/*
* Purpose: Load a file into the buffer
* Author: Silviu Riley
* Version: 10
* Called functions: fgetc(), b_addc()
* Parameters: File *, Buffer *
* Return value: int(RT_FAIL1, or a count)
* Algorithm: Get a character, check if it's EOF, if not add to buffer, increment count, repeat
*/
int b_load(FILE *const fi, Buffer *const pBD) {
    char currentCharacter;
    int countAdded = 0;

    /* are provided pointers initialized? */
    if (fi == NULL || pBD == NULL) {
        return RT_FAIL1;
    }

    /* read at least one character before checking for end-of-file */
    currentCharacter = (char)fgetc(fi);

    /* continue until the end-of-file */
    while (!feof(fi)) {
        /* add character to buffer
        and check if it was added successfully? */
        if (b_addc(pBD, currentCharacter) == NULL) {
            return LOAD_FAIL;
        }

        /* add was successful, keep a count */
        ++countAdded;

        /* read another character */
        currentCharacter = (char)fgetc(fi);
    }

    /* load complete, returning number of characters added */
    return countAdded;
}

/*
* Purpose: Find out if buffer is empty
* Author: Silviu Riley
* Version: 10
* Called functions: none
* Parameters: Buffer *
* Return value: int(RT_FAIL1, TRUE, FALSE)
* Algorithm:
*/
int b_isempty(Buffer *const pBD) {
    /* is provided pointer initialized? */
    if (pBD == NULL) {
        return RT_FAIL1;
    }

    /* character buffer is empty if add offset is at the start */
    if (pBD->addc_offset == 0) {
        return TRUE;
    }
    
    /* otherwise not empty */
    return FALSE;
}

/*
* Purpose: Find out if reached end of buffer
* Author: Silviu Riley
* Version: 10
* Called functions: none
* Parameters: Buffer *
* Return value: int(RT_FAIL1, or eob flag)
* Algorithm:
*/
int b_eob(Buffer *const pBD) {
    /* is provided pointer initialized? */
    if (pBD == NULL) {
        return RT_FAIL1;
    }

    /* provide the end-of-buffer flag */
    return pBD->eob;
}

/*
* Purpose: Read a character from the buffer
* Author: Silviu Riley
* Version: 10
* Called functions: none
* Parameters: Buffer *
* Return value: char(RT_FAIL2, RT_FAIL1, or any ASCII)
* Algorithm: Check if end of buffer, set eob flag, get character from buffer, increment count, return character
*/
char b_getc(Buffer *const pBD) {
    char retrievedCharacter;

    /* is provided pointer initialized? */
    if (pBD == NULL) {
        return RT_FAIL2;
    }

    /* at end of buffer? */
    if (pBD->getc_offset == pBD->addc_offset) {
        pBD->eob = TRUE;

        return RT_FAIL1;
    }

    /* not at end so ensure flag set appropriately */
    pBD->eob = FALSE;

    /* get the next character in buffer */
    retrievedCharacter = pBD->cb_head[pBD->getc_offset];

    /* move the offset to not re-read the same character */
    ++pBD->getc_offset;

    /* provide the retrieved character */
    return retrievedCharacter;
}

/*
* Purpose: Print contents of buffer
* Author: Silviu Riley
* Version: 10
* Called functions: b_getc(), b_eob(), printf()
* Parameters: Buffer *
* Return value: int(RT_FAIL1, SUCCESS, or a count)
* Algorithm: Check if buffer is empty, get a character, check if end of buffer, print character, increment count, loop
*/
int b_print(Buffer *const pBD) {
    char printCharacter;
    int countPrinted = 0;

    /* is provided pointer initialized? */
    if (pBD == NULL) {
        return RT_FAIL1;
    }

    /* empty buffer? */
    if (pBD->addc_offset == 0) {
        printf("Empty buffer\n");

        return SUCCESS;
    }

    /* get the character to print */
    printCharacter = b_getc(pBD);

    /* print the buffer checking for hitting the end */
    while(!b_eob(pBD)) {
        /* print the error-checked character */
        printf("%c",printCharacter);

        /* keep a count */
        ++countPrinted;

        /* get another character to print */
        printCharacter = b_getc(pBD);
    }

    /* new line now that buffer is printed */
    printf("\n");

    /* print complete, returning number of characters printed */
    return countPrinted;
}

/*
* Purpose: Reduce size of buffer to add offset, or expand by one to add end of buffer
* Author: Silviu Riley
* Version: 10
* Called functions: realloc()
* Parameters: Buffer *, char(any ASCII)
* Return value: NULL or Buffer *
* Algorithm: Check if can be expanded, realloc to one past add offset, store symbol
*/
Buffer *b_compact(Buffer *const pBD, char symbol) {
    char *tempBuffer;

    /* is provided pointer initialized? */
    if (pBD == NULL) {
        return NULL;
    }

    /* can buffer be expanded? */
    /* using short max to account for reserved space for EOF */
    if (pBD->addc_offset >= SHRT_MAX) {
        return NULL;
    }

    /* might have to reallocate so reset the flag */
    pBD->r_flag = FALSE;

    /* modify the size of the buffer */
    tempBuffer = realloc(pBD->cb_head, (pBD->addc_offset + 1));

    /* memory allocated successfully? */
    if (tempBuffer == NULL) {
        return NULL;
    }

    /* buffer reallocated in memory? */
    if (tempBuffer != pBD->cb_head) {
        pBD->r_flag = SET_R_FLAG;
    }

    /* add character to buffer now that space has been created */
    tempBuffer[pBD->addc_offset] = symbol;

    /* account for newly added character */
    ++pBD->addc_offset;

    /* store successfully compacted/expanded character buffer */
    pBD->cb_head = tempBuffer;

    /* store new capacity for next expansion */
    pBD->capacity = pBD->addc_offset;

    /* successfully added character to buffer */
    return pBD;
}

/*
* Purpose: Find out condition of the reallocation flag
* Author: Silviu Riley
* Version: 10
* Called functions: none
* Parameters: Buffer *
* Return value: char(RT_FAIL1 or reallocation flag)
* Algorithm:
*/
char b_rflag(Buffer *const pBD) {
    /* is provided pointer initialized? */
    if (pBD == NULL) {
        return RT_FAIL1;
    }

    /* provide the reallocation flag of the buffer */
    return pBD->r_flag;
}

/*
* Purpose: Allow reading from end of word/token
* Author: Silviu Riley
* Version: 10
* Called functions: none
* Parameters: Buffer *
* Return value: short(RT_FAIL1, or get offset)
* Algorithm: Check if can retract, decrement get offset
*/
short b_retract(Buffer *const pBD) {
    /* is provided pointer initialized? */
    if (pBD == NULL) {
        return RT_FAIL1;
    }

    /* can't retract into negative number (if get offset is at start) */
    if (pBD->getc_offset == 0) {
        return RT_FAIL1;
    }

    /* decrement get offset to allow reading a new token */
    --pBD->getc_offset;

    /* provide the new get offset */
    return pBD->getc_offset;
}

/*
* Purpose: Move get offset to match mark offset
* Author: Silviu Riley
* Version: 10
* Called functions: none
* Parameters: Buffer *
* Return value: short(RT_FAIL1, or get offset)
* Algorithm:
*/
short b_reset(Buffer *const pBD) {
    /* is provided pointer initialized? */
    if (pBD == NULL) {
        return RT_FAIL1;
    }

    /* reset get offset to mark offset */
    pBD->getc_offset = pBD->markc_offset;

    /* provide the new get offset */
    return pBD->getc_offset;
}

/*
* Purpose: Find out the get offset of the buffer
* Author: Silviu Riley
* Version: 10
* Called functions: none
* Parameters: Buffer *
* Return value: short(RT_FAIL1, or get offset)
* Algorithm:
*/
short b_getcoffset(Buffer *const pBD) {
    /* is provided pointer initialized? */
    if (pBD == NULL) {
        return RT_FAIL1;
    }

    /* provide the get offset */
    return pBD->getc_offset;
}

/*
* Purpose: Allow buffer to be read from start again
* Author: Silviu Riley
* Version: 10
* Called functions: none
* Parameters: Buffer *
* Return value: int(RT_FAIL1, or SUCCESS)
* Algorithm:
*/
int b_rewind(Buffer *const pBD) {
    /* is provided pointer initialized? */
    if (pBD == NULL) {
        return RT_FAIL1;
    }

    /* allow buffer to be re-read */
    pBD->getc_offset = 0;
    pBD->markc_offset = 0;

    return SUCCESS;
}

/*
* Purpose: Create a pointer to location in buffer
* Author: Silviu Riley
* Version: 10
* Called functions: none
* Parameters: Buffer *
* Return value: char *
* Algorithm:
*/
char *b_location(Buffer *const pBD, short loc_offset) {
    /* is provided pointer initialized? */
    if (pBD == NULL) {
        return NULL;
    }

    /* is offset within range? */
    if (loc_offset < 0 || loc_offset > pBD->addc_offset) {
        return NULL;
    }

    /* provide pointer to validated offset */
    return &pBD->cb_head[loc_offset];
}

