#ifndef PARSER_H_
#define PARSER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>
#include <stdlib.h>

#include "buffer.h"
#include "token.h"

/*Static Global Variables*/
static Token lookahead;
static Buffer * sc_buf;

/*Global Variables*/
int synerrno;

/*Constants used for the index position in kw_table array*/

#define ELSE 0
#define FALSE_PSR 1
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define TRUE_PSR 7
#define WHILE 8
#define WRITE 9
#define NO_ATTR 10

/*Constant Definitions*/
#define TABLE_SIZE 10

/*Extern variables (defined in other files)*/
extern int line;
extern Buffer * str_LTBL;
extern char * kw_table[TABLE_SIZE];
extern Token malar_next_token(Buffer * sc_buf);

/* Function prototypes (parser related) */
void parser(Buffer *);
void match(int, int);
void syn_eh(int);
void syn_printe(void);
void gen_incode(char *);

/* Function prototypes (production related) */
void program(void);
void opt_statements(void);
void statements(void);
void statements_p(void);
void statement(void);
void assignment_statement(void);
void assignment_expression(void);
void selection_statement(void);
void iteration_statement(void);
void pre_condition(void);
void input_statement(void);
void variable_list(void);
void variable_list_p(void);
void variable_identifier(void);
void output_statement(void);
void output_list(void);
void opt_variable_list(void);
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_p(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_p(void);
void primary_arithmetic_expression(void);
void string_expression(void);
void string_expression_p(void);
void primary_string_expression(void);
void conditional_expression(void);
void logical_OR_expression(void);
void logical_OR_expression_p(void);
void logical_AND_expression(void);
void logical_AND_expression_p(void);
void relational_expression(void);
void relational_operator(void);
void primary_a_relational_expression(void);
void primary_s_relational_expression(void);

#endif

