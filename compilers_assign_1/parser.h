/*
* File name: parser.h
* Compiler: MS Visual Studio Enterprise 2015 Update 3
* Authors:
Silviu Riley, 040836045 (initial code provided by Svillen Ranev)
Jinesh Bhatt, 040845930
* Course: CST8152 - Compilers, Lab Section: 13
* Assignment: 3
* Date: 2018-01-11
* Professor: Sv. Ranev
* Purpose: Preprocessor directives, type declarations, and prototypes necessary for a parser for the Platypus compiler
*/

#ifndef PARSER_H_
#define PARSER_H_

#include <stdio.h>
#include <stdlib.h>
#include "buffer.h"
#include "token.h"

/* token code */
#define NO_ATTR -1

/* keyword array index for use in match attribute */
#define ELSE 0
#define FALSE_PRSR 1
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define TRUE_PRSR 7
#define WHILE 8
#define WRITE 9

/* function prototypes */
extern Token malar_next_token(Buffer * sc_buf);
void parser(Buffer* in_buf);
void match(int pr_token_code, int pr_token_attribute);
void syn_printe();
void syn_eh(int pr_token_code);
void gen_incode(char* inText);

/* global variables */
Token lookahead;
Buffer* sc_buf;
int synerrno = 0;
extern int line;
extern Buffer* str_LTBL;
extern char* kw_table[];

/* production function prototypes */
void program(void);
void opt_statements(void);
void statements(void);
void statements_prime(void);
void statement(void);
void assignment_statement(void);
void assignment_expression(void);
void selection_statement(void);
void iteration_statement(void);
void pre_condition(void);
void input_statement(void);
void variable_list(void);
void variable_list_prime(void);
void variable_identifier(void);
void opt_variable_list(void);
void output_statement(void);
void output_list(void);
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_prime(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_prime(void);
void primary_arithmetic_expression(void);
void string_expression(void);
void string_expression_prime(void);
void primary_string_expression(void);
void conditional_expression(void);
void logical_OR_expression(void);
void logical_OR_expression_prime(void);
void logical_AND_expression(void);
void logical_AND_expression_prime(void);
void relational_operator(void);
void relational_expression(void);
void primary_a_relational_expression(void);
void primary_s_relational_expression(void);

#endif

