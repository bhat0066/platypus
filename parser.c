#include "parser.h"

/**/
void parser(Buffer* in_buf) {
	sc_buf = in_buf;
	lookahead = malar_next_token(sc_buf);
	program(); 
	match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/**/
void match(int pr_token_code, int pr_token_attribute) {
	
	if (lookahead.code == pr_token_code) {
		switch (pr_token_code) {
		case SEOF_T: return;

		case KW_T:
		case LOG_OP_T:
		case ART_OP_T:
		case REL_OP_T:
			if (lookahead.attribute.get_int != pr_token_attribute)
				syn_eh(pr_token_code);
			else
				lookahead = malar_next_token(sc_buf);
			break;
		default:
			lookahead = malar_next_token(sc_buf);
			break;
		}

		if (lookahead.code == ERR_T) {
			syn_printe();
			lookahead = malar_next_token(sc_buf);
			synerrno++;
			return;
		}
	}else {
		syn_eh(pr_token_code);
		return;
	}
}

/**/
void syn_eh(int sync_token_code) {
	syn_printe();
	synerrno++;
	
	if (sync_token_code == SEOF_T) {
		return;
	}

	while (1) {
		lookahead = malar_next_token(sc_buf);
		if (lookahead.code == sync_token_code) {
			lookahead = malar_next_token(sc_buf);
			return;
		}

		else if (lookahead.code == SEOF_T)
			exit(synerrno);
	}
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

/**/
void gen_incode(char* string) {
	printf("%s\n", string);
}

/**/
void program(void) {
	match(KW_T, PLATYPUS); match(LBR_T, NO_ATTR); 
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*	
	<opt_statements> -> <statements> | e 
	FIRST(<opt_statements>) = {AVID_T, SVID_T KW_T( IF, WHILE, READ, WRITE) and e}
	Author: Sv. Ranev
*/
void opt_statements() {
	/* FIRST set: {AVID_T,SVID_T,KW_T(but not … see above),e} */
	switch (lookahead.code) {
		case AVID_T:
		case SVID_T: statements(); break;
		case KW_T:
			/* check for PLATYPUS, ELSE, THEN, REPEAT here and in
			statements_p()*/
			if (lookahead.attribute.get_int != PLATYPUS
				&& lookahead.attribute.get_int != ELSE
				&& lookahead.attribute.get_int != THEN
				&& lookahead.attribute.get_int != REPEAT) {
				statements();
				break;
			}
		default: /*empty string – optional statements*/;
		gen_incode("PLATY: Opt_statements parsed");
	}
}

/*
	<statements> -> <statement><statements'>
	FIRST(<statements>) = {AVID_T, SVID_T, KW_T(IF, WHILE, READ, WRITE)}
*/
void statements(void) {
	statement();
	statements_p();
}

/*
    <statements’> -> <statement> <statements’> | ϵ
    FIRST{<statements’>} = {AVID, SVID, IF, WHILE, READ, WRITE, ϵ}
 */
void statements_p(void) {
    switch(lookahead.code)
    {
        case AVID_T:
        case SVID_T: statement(); statements_p(); break;
        case KW_T:
            if (lookahead.attribute.get_int != PLATYPUS
                && lookahead.attribute.get_int != ELSE
                && lookahead.attribute.get_int != THEN
                && lookahead.attribute.get_int != REPEAT) {
				statement();
                statements_p(); 
                break;
            }
		default:
			;
    }
}

/*
    <statement> ->
    <assignment statement>
    |<selection statement>
    |<iteration statement>
    |<input statement>
    | <output statement>
 
    FIRST(<statement>) = {AVID, SVID, IF, USING, INPUT, OUTPUT}
 */
void statement(void) {
    switch(lookahead.code) {
        case AVID_T:
        case SVID_T: assignment_statement(); break;
        case KW_T:
            if(lookahead.attribute.get_int == IF) {
                selection_statement();
                break;
            }
            if(lookahead.attribute.get_int == WHILE) {
                iteration_statement();
                break;
            }
            if(lookahead.attribute.get_int == READ) {
                input_statement();
                break;
            }
            if(lookahead.attribute.get_int == WRITE) {
                output_statement();
                break;
            }
            /*else {
                syn_printe();
                break;
            }*/
        default:
            syn_printe();
    } 
}

/*
	<assignment statement> ->
	<assignment expression>

	FIRST(<assignment statement>) = {AVID, SVID}
*/
void assignment_statement(void) {
    assignment_expression();
    match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
           
}

/*
	<assignment expression> ->
	AVID  = <arithmetic expression>
	|SVID = <string expression>

	FIRST(<assignment expression>) = {AVID, SVID}
*/
void assignment_expression(void) {
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, EQ);
		match(ASS_OP_T, EQ);
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	default:
		syn_printe();
	}
}

/*
	<selection statement> ->
	IF (<conditional expression>) THEN <opt_statements>
	ELSE {<opt_statements>};

	FIRST(<selection statement>) = {IF}
*/
void selection_statement(void) {
	match(KW_T, IF);
	match(KW_T, TRUE_PSR);
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(KW_T,ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");
}

/*
	<iteration statement>
	USING (<assignment expression>, <conditional expression>, <assignment expression>)
	REPEAT {
	<opt_statements>
	};

	FIRST(<iteration statement>) = {USING}
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

/**/
void pre_condition(void) {
	if (lookahead.attribute.get_int == TRUE_PSR || lookahead.attribute.get_int == FALSE_PSR) {
		match(KW_T, lookahead.attribute.get_int);
		return;
	}
	syn_printe();
}

/*
	<input statement>
	INPUT (<variable list>);

	FIRST(<input statement>) = {INPUT}
*/
void input_statement(void) {
   match(KW_T, READ);
   match(LPR_T,NO_ATTR);
   variable_list();
   match(RPR_T,NO_ATTR); 
   match(EOS_T,NO_ATTR);
   gen_incode("PLATY: Input statement parsed");
}

/*
	<variable list> -> <variable identifier> , <variable list’>
	FIRST(<variable list>) = {AVID_T, SVID_T}
*/
void variable_list(void) {
	variable_identifier();
	variable_list_p();
	gen_incode("PLATY: Variable list parsed");
}

/*
	<variable list’> -> , <variable identifier> <variable list’> | ε

	FIRST(<variable list’>) = {,  ϵ}
*/
void variable_list_p(void) {
	switch (lookahead.code) {
	case COM_T:
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_p();
	}
}

/*
	<variable identifier> -> AVID_T | SVID_T

	FIRST(<variable identifier>) = {AVID_T, SVID_T}
*/
void variable_identifier(void) {
	switch(lookahead.code) {
		case AVID_T:
			match(AVID_T, NO_ATTR);
			break;
		case SVID_T:
			match(SVID_T, NO_ATTR);
			break;
		default:
			syn_printe();
			break;
	}
}

/*
	<output statement> -> OUTPUT (<output list>);

	FIRST(<output statement>) = {OUTPUT}
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
	if (lookahead.code == AVID_T || lookahead.code == SVID_T)
		opt_variable_list();
	else {
		switch (lookahead.code) {
			case STR_T:
				match(STR_T, NO_ATTR);
				gen_incode("PLATY: Output list (string literal) parsed");
				break;
			default:
				gen_incode("PLATY: Output list (empty) parsed");
				break;
		}
	}
}

/*
	<output list> -> <variable list> | STR_T| ε

	FIRST(<output list>) = {AVID_T, SVID_T STR_T, ϵ}
*/
void opt_variable_list(void) {
	if (lookahead.code == AVID_T || lookahead.code == SVID_T)
		variable_list();
}

/*
	<arithmetic expression> ->
	<unary arithmetic expression>
	| <additive arithmetic expression>

	FIRST{<arithmetic expression>) = {-, +, AVID_T, FPL_T, INL_T, (}
*/
void arithmetic_expression(void) {
	if (lookahead.code == ART_OP_T) {
		switch (lookahead.attribute.arr_op) {
		case MINUS:
			unary_arithmetic_expression();
			gen_incode("PLATY: Arithmetic expression parsed");
			break;

		case PLUS:
			unary_arithmetic_expression();
			gen_incode("PLATY: Arithmetic expression parsed");
			break;
		}
	}
	else if (lookahead.code == AVID_T ||
		lookahead.code == FPL_T ||
		lookahead.code == INL_T ||
		lookahead.code == LPR_T) {

		additive_arithmetic_expression();
		gen_incode("PLATY: Arithmetic expression parsed");
	}
	else		
		syn_printe();	 
}

/*
	<unary arithmetic expression> ->
	- <primary arithmetic expression>
	| + <primary arithmetic expression>

	FIRST(<unary arithmetic expression>) = {-, +}

*/
void unary_arithmetic_expression(void) {
	switch (lookahead.attribute.arr_op){
		case MINUS:
			match(ART_OP_T, MINUS);
			break;
		case PLUS:
			match(ART_OP_T, PLUS);
			break;
		default:
			syn_printe();
			return;
	}
	primary_arithmetic_expression();
	gen_incode("PLATY: Unary arithmetic expression parsed");
}

/*
	<additive arithmetic expression> -> 
	<multiplicative arithmetic expression> <additive arithmetic expression’>

	FIRST(<additive arithmetic expression>) = { AVID_T, FPL_T, INL_T, ( }
*/
void additive_arithmetic_expression(void) {
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_p();
}

/*
	<additive arithmetic expression’> ->
	+ <multiplicative arithmetic expression> <additive arithmetic expression’>
	| - <multiplicative arithmetic expression> <additive arithmetic expression’>
	| ε

	FIRST(<additive arithmetic expression’>) = {+, -, ϵ}
*/
void additive_arithmetic_expression_p(void) {
	if (lookahead.code == ART_OP_T) {
		switch (lookahead.attribute.arr_op) {
			case PLUS:
				match(ART_OP_T, PLUS);
				multiplicative_arithmetic_expression();
				additive_arithmetic_expression_p();
				gen_incode("PLATY: Additive arithmetic expression parsed");
				break;
			case MINUS:
				match(ART_OP_T, MINUS);
				multiplicative_arithmetic_expression();
				additive_arithmetic_expression_p();
				gen_incode("PLATY: Additive arithmetic expression parsed");
				break;
		}
	}
}

/*
	<multiplicative arithmetic expression> ->
	<primary arithmetic expression> <multiplicative arithmetic expression’>

	FIRST(<multiplicative arithmetic expression> = { AVID_T, FPL_T, INL_T, (}
*/
void multiplicative_arithmetic_expression(void) {
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_p();
}

/*
	<multiplicative arithmetic expression’> ->
	* <primary arithmetic expression> <multiplicative arithmetic expression’
	| /  <primary arithmetic expression> <multiplicative arithmetic expression’>
	| ε

	FIRST(<multiplicative arithmetic expression’> = {*, /, ϵ}
*/
void multiplicative_arithmetic_expression_p(void) {
	if(lookahead.code == ART_OP_T) {
		switch(lookahead.attribute.arr_op) {
			case MULT:
				match(ART_OP_T, MULT);
				primary_arithmetic_expression();
				multiplicative_arithmetic_expression_p();
				gen_incode("PLATY: Multiplicative arithmetic expression parsed");
				break;

			case DIV:
				match(ART_OP_T, DIV);
				primary_arithmetic_expression();
				multiplicative_arithmetic_expression_p();
				gen_incode("PLATY: Multiplicative arithmetic expression parsed");
				break;
		}
	}
}

/*
	<primary arithmetic expression> ->
	AVID_T
	| FPL_T
	| INL_T
	| (<arithmetic expression>)

	FIRST(<primary arithmetic expression>) = {AVID_T, FPL_T, INL_T, (}
*/
void primary_arithmetic_expression(void) {
	switch(lookahead.code) {
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
	<string expression> -> <primary string expression> <string expression’>

	FIRST(<string expression>) = {SVID_T, STR_T}
*/
void string_expression(void) {
	primary_string_expression();
	string_expression_p();
	gen_incode("PLATY: String expression parsed");
}

/*
	<string expression’> -> # <primary string expression> <string expression’> | ε

	FIRST(<string expression’>) = { <<, ϵ}
*/
void string_expression_p(void) {
	switch(lookahead.code) {
		case SCC_OP_T:
			match(SCC_OP_T, NO_ATTR);
			primary_string_expression();
			string_expression_p();
			break;
	}
}

/*
	<primary string expression> -> SVID_T | STR_T

	FIRST(<primary string expression>) = {SVID_T, STR_T}
*/
void primary_string_expression(void) {
	switch(lookahead.code) {
		case SVID_T:
			match(SVID_T, NO_ATTR);
			gen_incode("PLATY: Primary string expression parsed");
			break;

		case STR_T:
			match(STR_T, NO_ATTR);
			gen_incode("PLATY: Primary string expression parsed");
			break;

		default: 
			syn_printe();
			break;

	}
}

/*
	<conditional expression> -> <logical OR expression>

	FIRST(<conditional expression>} ={ AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void conditional_expression(void) {
	logical_OR_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

/*
	<logical OR expression> ->
	<logical AND expression> <logical OR expression’>

	FIRST(<logical .OR. expression> = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void logical_OR_expression(void) {
	logical_AND_expression();
	logical_OR_expression_p();
}

/*
	<logical OR expression’> ->
	.OR. <logical AND expression> <logical OR expression’> | ε

	FIRST(<logical .OR. expression’> = {.OR., ϵ}
*/
void logical_OR_expression_p(void) {
	if(lookahead.code == LOG_OP_T) {
		switch(lookahead.attribute.log_op) {
			case OR:
				match(LOG_OP_T, OR);
				logical_AND_expression();
				logical_OR_expression_p();
				gen_incode("PLATY: Logical OR expression parsed");
		}
	}
}

/*
	<logical AND expression> -> <relational expression> <logical AND expression’>

	FIRST(<logical AND expression>) = { AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void logical_AND_expression(void) {
	relational_expression();
	logical_AND_expression_p();

}

/*
	<logical AND expression’> -> .AND. <relational expression> <logical AND expression’> | ε

	FIRST(<logical AND expression’> = {.AND., ϵ}
*/
void logical_AND_expression_p(void) {
	if(lookahead.code == LOG_OP_T) {
		switch(lookahead.attribute.log_op) {
			case AND:
				match(LOG_OP_T, AND);
				relational_expression();
				logical_AND_expression_p();
				gen_incode("PLATY: Logical AND expression parsed");
		}
	}
}

/*
		<relational expression> -> 
	<primary a_relational expression> <relational operator> <primary a_relational expression>
	| <primary s_relational expression> <relational operator> <primary s_relational expression>

	FIRST(<relational expression>) {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void relational_expression(void) {
	switch(lookahead.code) {
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
	<relational operator> ->
	== | <> | < | >

	FIRST(<relational operators>) = {==, <>, <, >}
*/
void relational_operator(void) {
	switch (lookahead.attribute.rel_op) {
		case EQ:
		case NE:
		case GT:
		case LT:
			match(REL_OP_T, lookahead.attribute.rel_op);
			break;

		default:
			syn_printe();
			break;
		}
}

/*
		<primary a_relational expression> -> 
			AVID_T
			| FPL_T 
			| INL_T

	FIRST(<primary a_relational expression>) = {AVID_T, FPL_T, INL_T}
*/
void primary_a_relational_expression(void) {
	switch(lookahead.code) {
		case AVID_T:
		case FPL_T:
		case INL_T:
			match(lookahead.code, NO_ATTR);
			gen_incode("PLATY: Primary a_relational expression parsed");
			break;

		default:
			syn_printe();
			break;
	}
}

/*
	<primary s_relational expression> ->
	<primary string expression>

	FIRST(<primary s_relational expression>) = {SVID_T, STR_T}
*/
void primary_s_relational_expression(void) {
	switch (lookahead.code) {
		case SVID_T:
		case STR_T:
			primary_string_expression();
			gen_incode("PLATY: Primary s_relational expression parsed");
			break;

		default:
			syn_printe();
			break;
	}
}
