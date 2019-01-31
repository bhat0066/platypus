/*
* File name: parser.c
* Compiler: MS Visual Studio Enterprise 2015 Update 3
* Authors:
Silviu Riley, 040836045 (initial code provided by Svillen Ranev)
Jinesh Bhatt, 040845930
* Course: CST8152 - Compilers, Lab Section: 13
* Assignment: 3
* Date: 2018-01-11
* Professor: Sv. Ranev
* Purpose: Functions that implement a parser for the Platypus compiler 
*/

#include "parser.h"

void parser(Buffer* in_buf) {
    sc_buf = in_buf;
    lookahead = malar_next_token(sc_buf);

    program();
    match(SEOF_T, NO_ATTR);
    
    gen_incode("PLATY: Source file parsed");
}

void match(int pr_token_code, int pr_token_attribute) {
    /* reached end of source file? */
    if (lookahead.code == SEOF_T) {
        return;
    }

    /* does code match? */
    if (lookahead.code != pr_token_code) {
        /* handle this as panic recovery */
        syn_eh(pr_token_code);
        return;
    }

    /* code that requires attribute check? */
    if (pr_token_code == KW_T ||
        pr_token_code == LOG_OP_T ||
        pr_token_code == ART_OP_T ||
        pr_token_code == REL_OP_T) {
        /* check attribute */
        if (lookahead.attribute.get_int != pr_token_attribute) {
            /* handle this as panic recovery */
            syn_eh(pr_token_code);
            return;
        }
    }

    /* token matches, can get next token */
    lookahead = malar_next_token(sc_buf);

    /* was there an error? */
    if (lookahead.code == ERR_T) {
        /* print info about the error */
        syn_printe();

        /* track error count */
        ++synerrno;

        /* processed error, get next token */
        lookahead = malar_next_token(sc_buf);
    }

    return;
}

void syn_eh(int sync_token_code) {
    /* print info about the error */
    syn_printe();

    /* track error count */
    ++synerrno;

    /* match didn't advance to next token so do that */
    lookahead = malar_next_token(sc_buf);

    /* find the sync token in the buffer but check not to reach end of source file and overrun buffer */
    while (lookahead.code != SEOF_T) {
        if (lookahead.code == sync_token_code) {
            /* found sync point, advance to next token */
            lookahead = malar_next_token(sc_buf);

            return;
        }
    }

    /* end of source file found, was that the sync token? */
    if (SEOF_T == sync_token_code) {
        /* nothing left to do */
        return;
    }

    /* reached end of source file but wasn't looking for that so exit the entire program */
    exit(synerrno);
}

void gen_incode(char* inText) {
    /* print message */
    printf("%s\n", inText);
}

/* error printing function for Assignment 3 (Parser), F17 */
void syn_printe() {
    Token t = lookahead;

    printf("PLATY: Syntax error:  Line:%3d\n", line);
    printf("*****  Token code:%3d Attribute: ", t.code);
    switch (t.code) {
    case  ERR_T: /* ERR_T     0   Error token */
        printf("%s\n", t.attribute.err_lex);
        break;
    case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
        printf("NA\n");
        break;
    case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
    case  SVID_T:/* SVID_T    3  String Variable identifier token */
        printf("%s\n", t.attribute.vid_lex);
        break;
    case  FPL_T: /* FPL_T     4  Floating point literal token */
        printf("%5.1f\n", t.attribute.flt_value);
        break;
    case INL_T: /* INL_T      5   Integer literal token */
        printf("%d\n", t.attribute.get_int);
        break;
    case STR_T:/* STR_T     6   String literal token */
        printf("%s\n", b_location(str_LTBL, t.attribute.str_offset));
        break;

    case SCC_OP_T: /* 7   String concatenation operator token */
        printf("NA\n");
        break;

    case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
        printf("NA\n");
        break;
    case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
        printf("%d\n", t.attribute.get_int);
        break;
    case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
        printf("%d\n", t.attribute.get_int);
        break;
    case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
        printf("%d\n", t.attribute.get_int);
        break;

    case  LPR_T: /*LPR_T    12  Left parenthesis token */
        printf("NA\n");
        break;
    case  RPR_T: /*RPR_T    13  Right parenthesis token */
        printf("NA\n");
        break;
    case LBR_T: /*    14   Left brace token */
        printf("NA\n");
        break;
    case RBR_T: /*    15  Right brace token */
        printf("NA\n");
        break;

    case KW_T: /*     16   Keyword token */
        printf("%s\n", kw_table[t.attribute.get_int]);
        break;

    case COM_T: /* 17   Comma token */
        printf("NA\n");
        break;
    case EOS_T: /*    18  End of statement *(semi - colon) */
        printf("NA\n");
        break;
    default:
        printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
    }/*end switch*/
}/* end syn_printe()*/

/*********************
  Production Functions
**********************/

/*
<program> ->
PLATYPUS { <opt_statements> }
FIRST(program) = {KW_T(PLATYPUS)}
*/
void program(void) {
    match(KW_T, PLATYPUS);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);

    gen_incode("PLATY: Program parsed");
}

/*
<opt_statements> ->
<statements> | e
FIRST(opt_statements) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), e}
*/
void opt_statements(void) {
    switch (lookahead.code) {
    case AVID_T:
    case SVID_T:
        statements();
        break;
    case KW_T:
        /* check for PLATYPUS, ELSE, THEN, REPEAT */
        if (lookahead.attribute.get_int != PLATYPUS &&
            lookahead.attribute.get_int != ELSE &&
            lookahead.attribute.get_int != THEN &&
            lookahead.attribute.get_int != REPEAT) {

            statements();
            break;
        }
    default:
        /* empty */

        gen_incode("PLATY: Opt_statements parsed");
    }
}

/*
<statements> ->
<statement> <statements'>
FIRST(statements) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE)}
*/
void statements(void) {
    statement();
    statements_prime();
}

/*
<statements'> ->
<statement> <statements'> | e
FIRST(statements') = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), e}
*/
void statements_prime(void) {
    switch (lookahead.code) {
    case AVID_T:
    case SVID_T:
        statement();
        statements_prime();
        break;
    case KW_T:
        /* check for PLATYPUS, ELSE, THEN, REPEAT */
        if (lookahead.attribute.get_int != PLATYPUS &&
            lookahead.attribute.get_int != ELSE &&
            lookahead.attribute.get_int != THEN &&
            lookahead.attribute.get_int != REPEAT) {

            statement();
            statements_prime();
            break;
        }
    default:
        /* empty */
        ;
    }
}

/*
<statement> ->
<assignment statement> | <selection statement> | <iteration statement> | <input statement> | <output statement>
FIRST(statement) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE)}
*/
void statement(void) {
    switch (lookahead.code) {
    case AVID_T:
    case SVID_T:
        assignment_statement();
        break;
    case KW_T:
        if (lookahead.attribute.get_int == IF) {
            selection_statement();
            break;
        }
        
        if (lookahead.attribute.get_int == WHILE) {
            iteration_statement();
            break;
        }
        
        if (lookahead.attribute.get_int == READ) {
            input_statement();
            break;
        }
        
        if (lookahead.attribute.get_int == WRITE) {
            output_statement();
            break;
        }
    default:
        /* error */
        syn_printe();
    }
}

/*
<assignment statement> ->
<assignment expression> ;
FIRST(assignment statement) = {AVID_T, SVID_T}
*/
void assignment_statement(void) {
    assignment_expression();
    match(EOS_T, NO_ATTR);

    gen_incode("PLATY: Assignment statement parsed");
}

/*
<assignment expression> ->
AVID = <arithmetic expression> | SVID = <string expression>
FIRST(assignment expression) = {AVID_T, SVID_T}
*/
void assignment_expression(void) {
    switch (lookahead.code) {
    case AVID_T:
        match(AVID_T, NO_ATTR);
        match(ASS_OP_T, NO_ATTR);
        arithmetic_expression();

        gen_incode("PLATY: Assignment expression (arithmetic) parsed");
        break;
    case SVID_T:
        match(SVID_T, NO_ATTR);
        match(ASS_OP_T, NO_ATTR);
        string_expression();

        gen_incode("PLATY: Assignment expression (string) parsed");
        break;
    default:
        /* error */
        syn_printe();
    }
}

/*
<selection statement> ->
IF TRUE ( <conditional expression> ) THEN { <opt_statements> }
ELSE { <opt_statements> } ;
FIRST(selection statement) = {KW_T(IF)}
*/
void selection_statement() {
    match(KW_T, IF);
    match(KW_T, TRUE_PRSR);
    match(LPR_T, NO_ATTR);
    conditional_expression();
    match(RPR_T, NO_ATTR);
    match(KW_T, THEN);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);
    match(KW_T, ELSE);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);

    gen_incode("PLATY: Selection statement parsed");
}

/*
<iteration statement> ->
WHILE <pre-condition> ( <conditional expression> )
REPEAT { <statements> } ;
FIRST(iteration statement) = {KW_T(WHILE)}
*/
void iteration_statement(void) {
    match(KW_T, WHILE);
    pre_condition();
    match(LPR_T, NO_ATTR);
    conditional_expression();
    match(RPR_T, NO_ATTR);
    match(KW_T, REPEAT);
    match(LBR_T, NO_ATTR);
    statements();
    match(RBR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);

    gen_incode("PLATY: Iteration statement parsed");
}

/*
<pre-condition> ->
TRUE | FALSE
FIRST(pre-condition) = {KW_T(TRUE), KW_T(FALSE)}
*/
void pre_condition(void) {
    if (lookahead.attribute.get_int == TRUE_PRSR ||
        lookahead.attribute.get_int == FALSE_PRSR) {

        match(KW_T, lookahead.attribute.get_int);
        return;
    }

    /* error */
    syn_printe();
}

/*
<input statement> ->
READ ( <variable list> ) ;
FIRST(input statement) = {KW_T(READ)}
*/
void input_statement(void) {
    match(KW_T, READ);
    match(LPR_T, NO_ATTR);
    variable_list();
    match(RPR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);

    gen_incode("PLATY: Input statement parsed");
}

/*
<variable list> ->
<variable identifier> <variable list'>
FIRST(variable list) = {AVID_T, SVID_T}
*/
void variable_list(void) {
    variable_identifier();
    variable_list_prime();

    gen_incode("PLATY: Variable list parsed");
}

/*
<variable list'> ->
, <variable identifier> <variable list'> | e
FIRST(variable list) = {COM_T, e}
*/
void variable_list_prime(void) {
    if (lookahead.code == COM_T) {
        match(COM_T, NO_ATTR);
        variable_identifier();
        variable_list_prime();
    }

    /* empty */
}

/*
<variable identifier> ->
AVID_T | SVID_T
FIRST(variable identifier) = {AVID_T, SVID_T}
*/
void variable_identifier(void) {
    if (lookahead.code == AVID_T) {
        match(AVID_T, NO_ATTR);
        return;
    }
    
    if (lookahead.code == SVID_T) {
        match(SVID_T, NO_ATTR);
        return;
    }

    /* error */
    syn_printe();
}

/*
<opt_variable list> ->
<variable list> | e
FIRST(opt_variable list) = {AVID_T, SVID_T, e}
*/
void opt_variable_list(void) {
    if (lookahead.code == AVID_T ||
        lookahead.code == SVID_T) {

        variable_list();
    }

    /* empty */
}

/*
<output statement> ->
    WRITE ( <output list> ) ;
FIRST(output statement) = {KW_T(WRITE)}
*/
void output_statement(void) {
    match(KW_T, WRITE);
    match(LPR_T, NO_ATTR);
    output_list();
    match(RPR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);

    gen_incode("PLATY: Output statement parsed");
}

/*
<output list> ->
<opt_variable list> | STR_T
FIRST(output list) = {AVID_T, SVID_T, STR_T, e}
*/
void output_list(void) {
    if (lookahead.code == AVID_T ||
        lookahead.code == SVID_T) {

        opt_variable_list();
        return;
    }

    if (lookahead.code == STR_T) {
        match(STR_T, NO_ATTR);

        gen_incode("PLATY: Output list (string literal) parsed");
        return;
    }

    /* empty */
    gen_incode("PLATY: Output list (empty) parsed");
}

/*
<arithmetic expression> ->
<unary arithmetic expression> | <additive arithmetic expression>
FIRST(arithmetic expression) = {ART_OP_T(MINUS), ART_OP_T(PLUS), AVID_T, FPL_T, INL_T, LPR_T}
*/
void arithmetic_expression(void) {
    if (lookahead.code == ART_OP_T &&
        (lookahead.attribute.get_int == MINUS || lookahead.attribute.get_int == PLUS)) {

        unary_arithmetic_expression();

        gen_incode("PLATY: Arithmetic expression parsed");
        return;
    }

    if (lookahead.code == AVID_T ||
        lookahead.code == FPL_T ||
        lookahead.code == INL_T ||
        lookahead.code == LPR_T) {

        additive_arithmetic_expression();

        gen_incode("PLATY: Arithmetic expression parsed");
        return;
    }

    /* error */
    syn_printe();
}

/*
<unary arithmetic expression> ->
- <primary arithmetic expression> | + <primary arithmetic expression>
FIRST(unary arithmetic expression) = {ART_OPT_T(MINUS), ART_OPT_T(PLUS)}
*/
void unary_arithmetic_expression(void) {
    if (lookahead.code == ART_OP_T &&
        (lookahead.attribute.get_int == MINUS || lookahead.attribute.get_int == PLUS)) {

        match(ART_OP_T, lookahead.attribute.get_int);
        primary_arithmetic_expression();

        gen_incode("PLATY: Unary arithmetic expression parsed");
        return;
    }

    /* error */
    syn_printe();
}

/*
<additive arithmetic expression> ->
<multiplicative arithmetic expression> <additive arithmetic expression'>
FIRST(additive arithmetic expression) = {AVID_T, FPL_T, INL_T, (}
*/
void additive_arithmetic_expression(void) {
    multiplicative_arithmetic_expression();
    additive_arithmetic_expression_prime();
}

/*
<additive arithmetic expression'> ->
+ <multiplicative arithmetic expression> <additive arithmetic expression'> | - <multiplicative arithmetic expression> <additive arithmetic expression'> | e
FIRST(additive arithmetic expression') = {ART_OPT_T(MINUS), ART_OPT_T(PLUS), e}
*/
void additive_arithmetic_expression_prime(void) {
    if (lookahead.code == ART_OP_T) {
        switch (lookahead.attribute.get_int) {
        case PLUS:
            match(ART_OP_T, PLUS);
            multiplicative_arithmetic_expression();
            additive_arithmetic_expression_prime();

            break;
        case MINUS:
            match(ART_OP_T, MINUS);
            multiplicative_arithmetic_expression();
            additive_arithmetic_expression_prime();

            break;
        }
    }

    /* empty */
    gen_incode("PLATY: Additive arithmetic expression parsed");
}

/*
<multiplicative arithmetic expression> ->
<primary arithmetic expression> <multiplicative arithmetic expression'>
FIRST(multiplicative arithmetic expression) = {AVID_T, FPL_T, INL_T, RPR_T}
*/
void multiplicative_arithmetic_expression(void) {
    primary_arithmetic_expression();
    multiplicative_arithmetic_expression_prime();
}

/*
<multiplicative arithmetic expression'> ->
* <primary arithmetic expression> <multiplicative arithmetic expression'> | / <primary arithmetic expression> <multiplicative arithmetic expression'> | e
FIRST(multiplicative arithmetic expression') = {ART_OPT_T(MULT), ART_OPT_T(DIV), e}
*/
void multiplicative_arithmetic_expression_prime(void) {
    if (lookahead.code == ART_OP_T) {
        switch (lookahead.attribute.get_int) {
        case MULT:
            match(ART_OP_T, MULT);
            primary_arithmetic_expression();
            multiplicative_arithmetic_expression_prime();
            
            break;
        case DIV:
            match(ART_OP_T, DIV);
            primary_arithmetic_expression();
            multiplicative_arithmetic_expression_prime();
            
            break;
        }
    }

    /* empty */
    gen_incode("PLATY: Multiplicative arithmetic expression parsed");
}

/*
<primary arithmetic expression> ->
AVID_T | FPL_T | INL_T | ( <arithmetic expression> )
FIRST(primary arithmetic expression) = {AVID_T, FPL_T, INL_T, RPR_T}
*/
void primary_arithmetic_expression(void) {
    switch (lookahead.code) {
    case AVID_T:
        match(AVID_T, NO_ATTR);

        break;
    case FPL_T:
        match(FPL_T, NO_ATTR);

        break;
    case INL_T:
        match(INL_T, NO_ATTR);

        break;
    case LPR_T:
        match(LPR_T, NO_ATTR);
        arithmetic_expression();
        match(RPR_T, NO_ATTR);

        break;
    }

    gen_incode("PLATY: Primary arithmetic expression parsed");
}

/*
<string expression> ->
<primary string expression> <string expression'>
FIRST(string expression) = {SVID_T, STR_T}
*/
void string_expression(void) {
    primary_string_expression();
    string_expression_prime();

    gen_incode("PLATY: String expression parsed");
}

/*
<string expression'> ->
# <primary string expression> <string expression'> | e
FIRST(string expression') = {SCC_OP_T, e}
*/
void string_expression_prime(void) {
    if (lookahead.code == SCC_OP_T) {
        match(SCC_OP_T, NO_ATTR);
        primary_string_expression();
        string_expression_prime();
    }

    /* empty */
}

/*
<primary string expression> ->
SVID_T | STR_T
FIRST(primary string expression) = {SVID_T, STR_T}
*/
void primary_string_expression(void) {
    if (lookahead.code == SVID_T) {
        match(SVID_T, NO_ATTR);

        gen_incode("PLATY: Primary string expression parsed");
        return;
    }

    if (lookahead.code == STR_T) {
        match(STR_T, NO_ATTR);

        gen_incode("PLATY: Primary string expression parsed");
        return;
    }
    
    /* error */
    syn_printe();
}

/*
<conditional expression> ->
<logical OR  expression>
FIRST(conditional expression) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void conditional_expression(void) {
    logical_OR_expression();

    gen_incode("PLATY: Conditional expression parsed");
}

/*
<logical OR expression> ->
<logical AND expression> <logical OR expression'>
FIRST(logical OR expression) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void logical_OR_expression(void) {
    logical_AND_expression();
    logical_OR_expression_prime();
}

/*
<logical OR expression'> ->
.OR. <logical AND expression> <logical OR expression'> | e
FIRST(logical OR expression') = {LOG_OP_T(OR), e}
*/
void logical_OR_expression_prime(void) {
    if (lookahead.code == LOG_OP_T &&
        lookahead.attribute.get_int == OR) {

        match(LOG_OP_T, OR);
        logical_AND_expression();
        logical_OR_expression_prime();

        gen_incode("PLATY: Logical OR expression parsed");
    }
}

/*
<logical AND expression> ->
<relational expression> <logical AND expression'>
FIRST(logical AND expression) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void logical_AND_expression(void) {
    relational_expression();
    logical_AND_expression_prime();
}

/*
<logical AND expression'> ->
.AND. <relational expression> <logical AND expression'> | e
FIRST(logical AND expression') = {LOG_OP_T(AND), e}
*/
void logical_AND_expression_prime(void) {
    if (lookahead.code == LOG_OP_T &&
        lookahead.attribute.get_int == AND) {

        match(LOG_OP_T, AND);
        relational_expression();
        logical_AND_expression_prime();

        gen_incode("PLATY: Logical AND expression parsed");
    }
}

/*
<relational operator> ->
> | < | == | <>
FIRST(relational operator) = {REL_OP_T(GT), REL_OP_T(LT), REL_OP_T(EQ), REL_OP_T(NE)}
*/
void relational_operator(void) {
    switch (lookahead.attribute.get_int) {
    case EQ:
    case NE:
    case GT:
    case LT:
        match(REL_OP_T, lookahead.attribute.get_int);

        break;
    default:
        /* error */
        syn_printe();
    }
}

/*
<relational expression> ->
<primary a_relational expression> <relational operator> <primary a_relational expression> | <primary s_relational expression> <relational operator> <primary s_relational expression>
FIRST(relational expression) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void relational_expression(void) {
    switch (lookahead.code) {
    case AVID_T:
    case FPL_T:
    case INL_T:
        primary_a_relational_expression();
        relational_operator();
        primary_a_relational_expression();

        gen_incode("PLATY: Relational expression parsed");

        break;
    case SVID_T:
    case STR_T:
        primary_s_relational_expression();
        relational_operator();
        primary_s_relational_expression();

        gen_incode("PLATY: Relational expression parsed");

        break;
    default:
        syn_printe();
    }
}

/*
<primary a_relational expression> ->
AVID_T | FPL_T | INL_T
FIRST(primary a_relational expression) = {AVID_T, FPL_T, INL_T}
*/
void primary_a_relational_expression(void) {
    switch (lookahead.code) {
    case AVID_T:
    case FPL_T:
    case INL_T:
        match(lookahead.code, NO_ATTR);

        gen_incode("PLATY: Primary a_relational expression parsed");

        break;
    default:
        /* error */
        syn_printe();
    }
}

/*
<primary s_relational expression> ->
<primary string expression>
FIRST(primary s_relational expression) = {SVID_T, STR_T}
*/
void primary_s_relational_expression(void) {
    switch (lookahead.code) {
    case SVID_T:
    case STR_T:
        primary_string_expression();

        gen_incode("PLATY: Primary s_relational expression parsed");

        break;
    default:
        /* error */
        syn_printe();
    }
}
