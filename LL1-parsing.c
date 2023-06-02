/* LL1-parsing.c */

#include "compiler.h"

/* global variables */
int                                          token; /* look-ahead token buffer */
int                                     type_token; /* type of look-ahead token */
int                               depth_whileState; /* block depth of while statement */
char               declaring_proc_name[MAXSTRSIZE]; /* The name of procedure of subprogram declaration */
char label_stack[MAX_WHILE_DEPTH][JUMP_LABEL_SIZE]; /* stack of end label of while statement */

/* flag */
int           isLocal; /* 1(LOCAL) = local variable, 0(GLOBAL) = global variable */
int       isParameter; /* 1:formal parameter, 0:else(variable) */
int      isGR1stuffed; /* 1:GR1 is full, 0:GR1 is empty */
int       isMinusTerm; /* 1:need LoaD value of formal parameters */
static int     isOnlyRightValue; /* 1:expressions and constants expect variables */
static int    isMultipleOperand; /* 1:second and subsequent factors */
static int isExpressionInFactor; /* 1:in "(" expression ")" */

int   isCallState; /* 1:in callStatement() */
int isAssignState; /* 1:in Left Value of assignmentStatement() */
int  isInputState; /* 1:in inputStatement() */
static int isOutputState; /* 1:in outputStatement() */

struct ID       *variable_id; /* variable ID pointer */
struct ID      *procedure_id; /* procedure ID pointer */
struct ID       *Left_var_id; /* left value pointer at assignment statement */

/* static function */
static int                 block();
static int   variableDeclaration();
static int         variableNames();
static int                  type(); /* type_token = type() */
static int             arrayType();
static int subprogramDeclaration();
static int      formalParameters();
static int     compoundStatement();
static int             statement();
static int    conditionStatement();
static int    iterationStatement();
static int         callStatement();
static int   assignmentStatement();
static int              variable(); /* type_token = variable() */
static int            expression(); /* type_token = expression() */
static int      simpleExpression(); /* type_token = simpleExpression() */
static int                  term(); /* type_token = term() */
static int                factor(); /* type_token = factor() */
static int        inputStatement();
static int       outputStatement();
static int          outputFormat();

/*-----function called from the main program------*/
int parse_program() {

    /* Initialize */
    isLocal              = GLOBAL;
    depth_whileState     = 0;
    isParameter          = 0;
    isOnlyRightValue     = 0;
    isGR1stuffed         = 0;
    isMinusTerm          = 0;
    isMultipleOperand    = 0;
    isExpressionInFactor = 0;

    isInputState  = 0;
    isOutputState = 0;
    isCallState   = 0;
    isAssignState = 0;

    variable_id  = NULL;
    procedure_id = NULL;
    Left_var_id  = NULL;

    init_alltab();
    init_label_list();

    /* Start　parsing */
    IS_TOKEN(TPROGRAM)
    NEXT_TOKEN

    IS_SPECIFIC_TOKEN(TNAME, "Program name is not found.")

    /* START */
    IS_ERROR(write_start(string_attr))
    /* set 0 to GR0 */
    IS_ERROR(write_command_Reg_Adr(LAD, GR0, 0))

    NEXT_TOKEN

    IS_TOKEN(TSEMI)
    NEXT_TOKEN

    BLOCK

    IS_SPECIFIC_TOKEN(TDOT, "Period '.' is not found at the end of program.")
    NEXT_TOKEN

    /* finish run main program */
    IS_ERROR(write_command(RET, ""))

    /* write DC labels below RET */
    IS_ERROR(write_DC_labels())

    /* write library */
    IS_ERROR(write_library())

    /* end program */
    IS_ERROR(write_command(END, ""))

    return NORMAL;
}

/*-----static function-----*/

static int block() {
    char block_label[JUMP_LABEL_SIZE];

    NEW_JUMP_LABEL(block_label)

    /* call main program */
    IS_ERROR(write_command(CALL, block_label))
    /* output all of buffer */
    IS_ERROR(write_command(CALL, FLUSH))
    /* supervisor call(halt program) */
    IS_ERROR(write_command_Adr(SVC, 0))

    while (token == TVAR || token == TPROCEDURE) {

        if (token == TVAR) {
            VARIABLE_DECLARAION
            release_passlist();
        }
        else {
            SUBPROGRAM_DECLARATION
        }
    }

    IS_ERROR(write_Label(block_label))
    COMPOUND_STATEMENT

    return NORMAL;
}

static int variableDeclaration() {
    IS_TOKEN(TVAR) /* No error occurs at this point because of an element of FIRST set. */
    NEXT_TOKEN

    VARIABLE_NAMES

    IS_TOKEN(TCOLON)
    NEXT_TOKEN

    TYPES /* type_token = type() */

    /* variable ID registration */
    /* in add_variables_idtab(), A DC instruction is written that defines the area for the variable */
    IS_ERROR(add_variables_idtab(isLocal, type_token))

    IS_TOKEN(TSEMI)
    NEXT_TOKEN

    while (token == TNAME) {
        VARIABLE_NAMES

        IS_TOKEN(TCOLON)
        NEXT_TOKEN

        TYPES
        /* variable ID registration */
        IS_ERROR(add_variables_idtab(isLocal, type_token))

        IS_TOKEN(TSEMI)
        NEXT_TOKEN
    }

    return NORMAL;
}

static int variableNames() {
    IS_SPECIFIC_TOKEN(TNAME, "Variable name is not found.")
    /* make a temporary list of names */
    IS_ERROR(store_varname(string_attr))
    NEXT_TOKEN

    while (token == TCOMMA) {
        NEXT_TOKEN

        IS_SPECIFIC_TOKEN(TNAME, "Variable name is not found.")
        IS_ERROR(store_varname(string_attr))
        NEXT_TOKEN
    }

    return NORMAL;
}

static int type() {
    int t;

    switch (token) {
        case TINTEGER:
            t = TPINT;
            NEXT_TOKEN
            break;
        case TBOOLEAN:
            t = TPBOOL;
            NEXT_TOKEN
            break;
        case    TCHAR:
            t = TPCHAR;
            NEXT_TOKEN
            break;
        case   TARRAY:
            ARRAY_TYPE
            t = type_token;
            break;
        default:
            return(parse_error("Variable type is undefined."));
    }

    return t;
}

static int arrayType() {
    IS_TOKEN(TARRAY) /* No error occurs at this point because of an element of FIRST set. */
    NEXT_TOKEN

    IS_TOKEN(TLSQPAREN)
    NEXT_TOKEN

    IS_SPECIFIC_TOKEN(TNUMBER, "Array suffix is not found.")
    if(num_attr < 1) {
        return(parse_error("Array size must be 1 or more."));
    }
    NEXT_TOKEN

    IS_TOKEN(TRSQPAREN)
    NEXT_TOKEN

    IS_TOKEN(TOF)
    NEXT_TOKEN

    switch (token) {
        case TINTEGER:
            type_token =  TPARRAYINT;
            break;
        case TBOOLEAN:
            type_token = TPARRAYBOOL;
            break;
        case    TCHAR:
            type_token = TPARRAYCHAR;
            break;
        default:
            return(parse_error("Array type is undefined."));
    }
    NEXT_TOKEN

    return NORMAL;
}

static int subprogramDeclaration() {
    char proc_label[MAX_LABEL_SIZE];

    IS_TOKEN(TPROCEDURE) /* No error occurs at this point because of an element of FIRST set. */
    NEXT_TOKEN

    IS_SPECIFIC_TOKEN(TNAME, "Procedure name is not found.")
    strcpy(declaring_proc_name, string_attr);

    /* procedure ID registration */
    IS_ERROR(add_idtab(GLOBAL, TPPROC, declaring_proc_name))

    procedure_id = search_idtab(GLOBAL, declaring_proc_name);
    NEXT_TOKEN

    /* flag set */
    isLocal = LOCAL;

    if(token == TLPAREN) {
        FORMAL_PARAMETERS
    }

    IS_TOKEN(TSEMI)
    NEXT_TOKEN

    if(token == TVAR) {
        VARIABLE_DECLARAION
    }

    IS_ERROR(make_var_label_byID(procedure_id, proc_label))

    /* subroutine label */
    IS_ERROR(write_Label(proc_label))

    if(procedure_id->itp->paratp != NULL) {
        /* save the program counter to the register */
        IS_ERROR(write_command(POP, GR2))
        /* pass arguments */
        IS_ERROR(write_pass_Adr_to_formalPara(declaring_proc_name))
        /* return the program counter to the stack */
        IS_ERROR(write_command_Adr_Index(PUSH, 0, GR2))
    }

    isGR1stuffed = 0; /* flag reset */

    COMPOUND_STATEMENT

    IS_ERROR(write_command(RET, ""))

    IS_TOKEN(TSEMI)
    NEXT_TOKEN

    /* flag reset */
    isLocal = GLOBAL;
    memset(declaring_proc_name, '\0', sizeof(declaring_proc_name));

    return NORMAL;
}

static int formalParameters() {
    isParameter = 1; /* flag set */

    IS_TOKEN(TLPAREN)  /* No error occurs at this point because of an element of FIRST set. */
    NEXT_TOKEN

    VARIABLE_NAMES

    IS_TOKEN(TCOLON)
    NEXT_TOKEN

    TYPES /* type_token = type() */

    IS_STANDARD_TYPE(type_token, "Formal parameters' types must be standard type.")

    /* make a temporary list of types */
    IS_ERROR(store_formalParalist(type_token))
    /* variable ID registration */
    IS_ERROR(add_variables_idtab(isLocal, type_token))

    while (token == TSEMI) {
        NEXT_TOKEN

        VARIABLE_NAMES

        IS_TOKEN(TCOLON)
        NEXT_TOKEN

        TYPES

        IS_STANDARD_TYPE(type_token, "Formal parameters' types must be standard type.")

        IS_ERROR(store_formalParalist(type_token))
        IS_ERROR(add_variables_idtab(isLocal, type_token))
    }

    /* formal parameters' types registration */
    IS_ERROR(add_formalPara_typetab(declaring_proc_name))

    IS_TOKEN(TRPAREN)
    NEXT_TOKEN

    isParameter = 0; /* flag reset */
    return NORMAL;
}

static int compoundStatement() {
    IS_TOKEN(TBEGIN)
    NEXT_TOKEN

    STATEMENT

    while (token == TSEMI) {
        NEXT_TOKEN
        STATEMENT
    }

    IS_TOKEN(TEND)
    NEXT_TOKEN

    return NORMAL;
}

static int statement() {

    switch (token) {
        case TNAME:
            ASSIGNMENT_STATEMENT
            break;
        case TIF:
            CONDITION_STATEMENT
            break;
        case TWHILE:
            ITERATION_STATEMENT
            break;
        case TBREAK:
            if(depth_whileState <= 0) {
                return(parse_error("Keyword 'break' is not located in the iteration statement."));
            }

            char break_label[JUMP_LABEL_SIZE];
            IS_ERROR(while_stack_pop(break_label))

            /* jump to end label of the innermost while statement */
            IS_ERROR(write_command(JUMP, break_label))

            NEXT_TOKEN
            break;
        case TCALL:
            CALL_STATEMENT
            break;
        case TRETURN:
            IS_ERROR(write_command(RET, ""))

            NEXT_TOKEN
            break;
        case   TREAD:
        case TREADLN:
            INPUT_STATEMENT
            break;
        case   TWRITE:
        case TWRITELN:
            OUTPUT_STATEMENT
            break;
        case TBEGIN:
            COMPOUND_STATEMENT
            break;
        default: /* empty statement */
            break;
    }

    return NORMAL;
}

static int conditionStatement() {
    char false_label[JUMP_LABEL_SIZE];
    char   end_label[JUMP_LABEL_SIZE];

    NEW_JUMP_LABEL(false_label)
    NEW_JUMP_LABEL(end_label) /* the label number is skipped if "else" does not exist */

    IS_TOKEN(TIF) /* No error occurs at this point because of an element of FIRST set. */
    NEXT_TOKEN

    EXPRESSION /* type_token = expression() */

    IS_TYPE_MATCH(type_token, TPBOOL, "The type of conditional expression of if statement must be boolean.")

    if(isGR1stuffed == 0) {
        IS_ERROR(write_command(POP, GR1))
    }

    /* compare the result of the conditional expression with GR0 (0) */
    IS_ERROR(write_command_Reg_Reg(CPA, GR1, GR0))
    /* jump to end label if result is false(0) */
    IS_ERROR(write_command(JZE, false_label))

    IS_TOKEN(TTHEN)
    NEXT_TOKEN

    isGR1stuffed = 0; /* flag reset */

    /* statement at true */
    STATEMENT

    if(token != TELSE) {
        /* end label if "else" does not exist */
        IS_ERROR(write_Label(false_label))
        return NORMAL;
    }

    /* NEW_JUMP_LABEL(end_label) */
    /* jump to end label */
    IS_ERROR(write_command(JUMP, end_label))

    /* label at false */
    IS_ERROR(write_Label(false_label))

    NEXT_TOKEN

    isGR1stuffed = 0; /* flag reset */

    /* statement at false (else statement) */
    STATEMENT

    /* end label */
    IS_ERROR(write_Label(end_label))

    return NORMAL;
}

static int iterationStatement() {
    char       end_label[JUMP_LABEL_SIZE];
    char loop_head_label[JUMP_LABEL_SIZE];

    depth_whileState++;

    IS_TOKEN(TWHILE) /* No error occurs at this point because of an element of FIRST set. */
    NEXT_TOKEN

    NEW_JUMP_LABEL(loop_head_label)
    NEW_JUMP_LABEL(end_label)

    /* push the end label of a while statement to the stack of labels */
    IS_ERROR(while_stack_push(end_label))

    /* loop head label */
    IS_ERROR(write_Label(loop_head_label))

    EXPRESSION /* type_token = expression() */

    IS_TYPE_MATCH(type_token, TPBOOL, "The type of conditional expression of while statement must be boolean.")

    if(isGR1stuffed == 0) {
        IS_ERROR(write_command(POP, GR1))
    }

    /* compare the result of the conditional expression with GR0 (0) */
    IS_ERROR(write_command_Reg_Reg(CPA, GR1, GR0))
    /* jump to end label if result is false(0) */
    IS_ERROR(write_command(JZE, end_label))

    IS_TOKEN(TDO)
    NEXT_TOKEN

    isGR1stuffed = 0; /* flag reset */

    STATEMENT

    /* jump to loop head label */
    IS_ERROR(write_command(JUMP, loop_head_label))

    /* while end label */
    IS_ERROR(write_Label(end_label))

    depth_whileState--;
    return NORMAL;
}

static int callStatement() {
    char proc_label[MAX_LABEL_SIZE];

    isCallState = 1; /* flag set */

    IS_TOKEN(TCALL) /* No error occurs at this point because of an element of FIRST set. */
    NEXT_TOKEN

    IS_SPECIFIC_TOKEN(TNAME, "Procedure name to call is not found.")

    procedure_id = search_idtab(GLOBAL, string_attr);

    /* check if ID is declared  */
    IS_ERROR(check_proc_id(procedure_id))

    IS_ERROR(make_var_label_byID(procedure_id, proc_label))

    NEXT_TOKEN

    if(token != TLPAREN) {
        /* argument check(procedure with no arguments) */
        IS_ERROR(verify_paralist(procedure_id))

        /* call procedure */
        IS_ERROR(write_command(CALL, proc_label))

        /* flag reset */
        isGR1stuffed = 0;
        isCallState = 0;
        isOnlyRightValue = 0;
        return NORMAL;
    }
    NEXT_TOKEN

    isOnlyRightValue = 0;
    /* expression of argument */
    EXPRESSION

    /* make a temporary list of types */
    IS_ERROR(store_paralist(type_token))

    if(isOnlyRightValue == 1) { /* expression has only right value */
        IS_ERROR(write_pass_reference())
        isGR1stuffed = 0;
    }


    while (token == TCOMMA) {
        NEXT_TOKEN

        isOnlyRightValue = 0;
        EXPRESSION

        IS_ERROR(store_paralist(type_token))

        if(isOnlyRightValue == 1) { /* expression has only right value */
            IS_ERROR(write_pass_reference())
            isGR1stuffed = 0;
        }

    }
    /* argument check(procedure with arguments) */
    IS_ERROR(verify_paralist(procedure_id))

    IS_TOKEN(TRPAREN)
    NEXT_TOKEN

    /* call procedure */
    IS_ERROR(write_command(CALL, proc_label))

    /* flag reset */
    isGR1stuffed = 0;
    isCallState = 0;
    isOnlyRightValue = 0;
    return NORMAL;
}

static int assignmentStatement() {
    int var_type;
    char var_label[MAX_LABEL_SIZE];

    isAssignState = 1; /* flag set */
    VARIABLE /* type_token = variable() */
    isAssignState = 0; /* flag reset */

    var_type = type_token;

    if(Left_var_id->ispara == 1 || Left_var_id->itp->t_type == TPARRAY) {
        IS_ERROR(write_command_Adr_Index(PUSH, 0, GR1))
        isGR1stuffed = 0;
    }

    IS_TOKEN(TASSIGN)
    NEXT_TOKEN

    /* expression to assign */
    EXPRESSION /* type_token = expression() */

    IS_TYPE_MATCH(var_type, type_token, "Different types variable and the expression to be assigned.")

    if(isGR1stuffed == 0) {
        IS_ERROR(write_command(POP, GR1))
    }

    if(Left_var_id->ispara == 1 || Left_var_id->itp->t_type == TPARRAY) {
        IS_ERROR(write_command(POP, GR2))
        IS_ERROR(write_command_Reg_Adr_Index(ST, GR1, 0, GR2))
    }
    else {
        IS_ERROR(make_var_label_byID(Left_var_id, var_label))
        IS_ERROR(write_command_Reg_Reg(ST, GR1, var_label))
    }

    isGR1stuffed = 0; /* flag reset */
    return NORMAL;
}

static int variable() {
    int          var_type;
    int        array_size;
    int isAssgin_left = 0;

    IS_SPECIFIC_TOKEN(TNAME, "Variable name is not found.")

    /* check if ID is declared */
    variable_id = get_var_id(isLocal, string_attr);

    if(isAssignState == 1) {
        /* memorize the ID of the variable  to be assigned */
        Left_var_id = variable_id;
    }

    IS_ERROR((var_type = get_vartype(variable_id)))

    IS_ERROR(write_var_reference_byID(variable_id))

    NEXT_TOKEN

    if(token != TLSQPAREN) {
        /* if return value is TPARRAY, error will occur at subsequent standard type check(IS_STANDARD_TYPE) */
        return var_type;
    }

    IS_TYPE_MATCH(var_type, TPARRAY, "The type of the variable with expression must be array.")
    /* push address of array */
    IS_ERROR(write_command_Adr_Index(PUSH, 0, GR1))
    isGR1stuffed = 0;

    array_size = variable_id->itp->array_size;
      var_type = variable_id->itp->etp->t_type;

    NEXT_TOKEN
    if(isAssignState == 1) {
        isAssignState = 0;
        isAssgin_left = 1;
    }
    EXPRESSION /* type_token = expression() */
    if(isAssgin_left == 1) {
        isAssignState = 1;
    }

    IS_TYPE_MATCH(type_token, TPINT, "The type of array index must be integer.")

    if(isGR1stuffed == 0) {
        IS_ERROR(write_command(POP, GR1))
    }

    /* array suffix are checked at run time */

    char inRange_label[JUMP_LABEL_SIZE];
    NEW_JUMP_LABEL(inRange_label)

    /* error if the suffix is negative */
    IS_ERROR(write_command(JMI, EROV))

    IS_ERROR(write_command_Reg_Adr(LAD, GR2, array_size))
    IS_ERROR(write_command_Reg_Reg(SUBA, GR2, GR1))

    IS_ERROR(write_command(JPL, inRange_label))
    /* error if the suffix is out of range */
    IS_ERROR(write_command(JUMP, EROV))

    IS_ERROR(write_Label(inRange_label))
    IS_ERROR(write_command_Reg_Reg(LD, GR2, GR1))   /* load value of suffix */
    IS_ERROR(write_command(POP, GR1))               /* pop address of array top */
    IS_ERROR(write_command_Reg_Reg(ADDA, GR1, GR2)) /* address of (array top + suffix) */
    IS_ERROR(write_command(JOV, EOVF))

    if(isAssignState == 0 && isCallState == 0) {
        IS_ERROR(write_command_Reg_Adr_Index(LD, GR1, 0, GR1)) /* load value */
        isOnlyRightValue = 1;
    }

    IS_TOKEN(TRSQPAREN)
    NEXT_TOKEN

    isGR1stuffed = 1;  /* flag set */
    return var_type;
}

static int expression() {
    int exp_type, simple_type;
    int exp_operand;

    /* push the previous expression when there are two or more operators */
    if(isMultipleOperand == 1 && isGR1stuffed == 1) {
        IS_ERROR(write_command_Adr_Index(PUSH, 0, GR1))
        isGR1stuffed = 0;
    }

    SIMPLE_EXPRESSION /* type_token = simpleExpression() */
    exp_type    = type_token;
    simple_type = type_token;

    while (token == TEQUAL || token == TNOTEQ || token ==    TLE ||
           token ==  TLEEQ || token ==    TGR || token ==  TGREQ) {
        exp_operand = token;

        if(isCallState == 1) {
            isOnlyRightValue = 1; /* flag set */
        }

        NEXT_TOKEN

        /* push the previous expression when there are two or more operators */
        if(isMultipleOperand == 1 && isGR1stuffed == 1) {
            IS_ERROR(write_command_Adr_Index(PUSH, 0, GR1))
            isGR1stuffed = 0;
        }

        isMultipleOperand = 1; /* flag set */
        SIMPLE_EXPRESSION
        isMultipleOperand = 0; /* flag reset */

        IS_TYPE_MATCH(simple_type, type_token, "Types of operands of relational operator must be the same standard type.")
        exp_type = TPBOOL;

        if(isGR1stuffed == 0) {
            IS_ERROR(write_command(POP, GR1))
        }

        IS_ERROR(write_command(POP, GR2))
        IS_ERROR(write_command_Reg_Reg(CPA, GR2, GR1))

        char  expression_label_true[JUMP_LABEL_SIZE];
        char expression_label_false[JUMP_LABEL_SIZE];
        NEW_JUMP_LABEL(expression_label_true)
        NEW_JUMP_LABEL(expression_label_false)

        switch (exp_operand) {
            case TEQUAL:
                IS_ERROR(write_command(JZE, expression_label_true))
                IS_ERROR(write_command_Reg_Reg(LD, GR1, GR0)) /* false is op1 != op2 */
                IS_ERROR(write_command(JUMP, expression_label_false))

                IS_ERROR(write_Label(expression_label_true))
                IS_ERROR(write_command_Reg_Adr(LAD, GR1, 1)) /* true is op1 == op2 */
                break;
            case TNOTEQ:
                IS_ERROR(write_command(JZE, expression_label_true))
                IS_ERROR(write_command_Reg_Adr(LAD, GR1, 1)) /* false is op1 == op2 */
                IS_ERROR(write_command(JUMP, expression_label_false))

                IS_ERROR(write_Label(expression_label_true))
                IS_ERROR(write_command_Reg_Reg(LD, GR1, GR0)) /* true is op1 != op2 */
                break;
            case TLE:
                IS_ERROR(write_command(JMI, expression_label_true))
                IS_ERROR(write_command_Reg_Reg(LD, GR1, GR0)) /* false is op1 >= op2 */
                IS_ERROR(write_command(JUMP, expression_label_false))

                IS_ERROR(write_Label(expression_label_true))
                IS_ERROR(write_command_Reg_Adr(LAD, GR1, 1)) /* true is op1 < op2 */
                break;
            case TLEEQ:
                IS_ERROR(write_command(JPL, expression_label_true))
                IS_ERROR(write_command_Reg_Adr(LAD, GR1, 1)) /* false is op1 > op2 */
                IS_ERROR(write_command(JUMP, expression_label_false))

                IS_ERROR(write_Label(expression_label_true))
                IS_ERROR(write_command_Reg_Reg(LD, GR1, GR0)) /* true is op1 <= op2 */
                break;
            case TGR:
                IS_ERROR(write_command(JPL, expression_label_true))
                IS_ERROR(write_command_Reg_Reg(LD, GR1, GR0)) /* false is op1 <= op2 */
                IS_ERROR(write_command(JUMP, expression_label_false))

                IS_ERROR(write_Label(expression_label_true))
                IS_ERROR(write_command_Reg_Adr(LAD, GR1, 1)) /* true is op1 > op2 */
                break;
            default: /* TGREQ */
                IS_ERROR(write_command(JMI, expression_label_true))
                IS_ERROR(write_command_Reg_Adr(LAD, GR1, 1)) /* false is op1 < op2 */
                IS_ERROR(write_command(JUMP, expression_label_false))

                IS_ERROR(write_Label(expression_label_true))
                IS_ERROR(write_command_Reg_Reg(LD, GR1, GR0)) /* true is op1 >= op2 */
                break;
        }

        IS_ERROR(write_Label(expression_label_false))

        /* push an intermediate expression if there is an operator behind */
        if(token ==    TOR || token ==   TDIV || token ==   TAND ||
          (token >=  TPLUS && token <=  TGREQ) ) {
            IS_ERROR(write_command_Adr_Index(PUSH, 0, GR1))
            isGR1stuffed = 0;
        }
        else {
            isGR1stuffed = 1;
        }
    }

    return exp_type;
}

static int simpleExpression() {
    int simple_type = NORMAL;
    int simple_operand;

    if(token == TPLUS || token == TMINUS) {
        simple_operand = token;
        simple_type = TPINT;
        NEXT_TOKEN

        if(simple_operand == TMINUS) {
            isMinusTerm = 1;
        }
    }

    /* push the previous expression when there are two or more operators */
    if(isMultipleOperand == 1 && isGR1stuffed == 1) {
        IS_ERROR(write_command_Adr_Index(PUSH, 0, GR1))
        isGR1stuffed = 0;
    }

    TERM /* type_token = term() */
    isMinusTerm = 0;

    if(simple_type == TPINT) {
        IS_TYPE_MATCH(type_token, TPINT, "The type of \"+\" or \"-\" operands must be integer.")
    }

    simple_type = type_token;

    if(simple_operand == TMINUS) {

        if(isGR1stuffed == 0) {
            IS_ERROR(write_command(POP, GR1))
        }

        /* multiply the value of the first term by -1 */
        IS_ERROR(write_command_Reg_Adr(LAD, GR2, -1))
        IS_ERROR(write_command_Reg_Reg(MULA, GR1, GR2))
        IS_ERROR(write_command(JOV, EOVF))
        isGR1stuffed = 1; /* flag set */
        isOnlyRightValue = 1;
    }
    isMinusTerm = 0;

    while (token == TPLUS || token == TMINUS || token == TOR) {
        simple_operand = token;

        if(isCallState == 1) {
            isOnlyRightValue = 1; /* flag set */
        }

        switch (token) {
            case  TPLUS:
            case TMINUS:
                IS_TYPE_MATCH(type_token, TPINT, "Types of operands of additive operator are different.")
                simple_type = TPINT;
                break;
            default: /* TOR */
                IS_TYPE_MATCH(type_token, TPBOOL, "Types of operands of additive operator are different.")
                simple_type = TPBOOL;
                break;
        }
        NEXT_TOKEN

        /* push the previous expression when there are two or more operators */
        if(isMultipleOperand == 1 && isGR1stuffed == 1) {
            IS_ERROR(write_command_Adr_Index(PUSH, 0, GR1))
            isGR1stuffed = 0;
        }

        isMultipleOperand = 1; /* flag set */
        TERM
        isMultipleOperand = 0; /* flag reset */

        IS_TYPE_MATCH(simple_type, type_token, "Types of operands of additive operator are different.")

        if(isGR1stuffed == 0) {
            IS_ERROR(write_command(POP, GR1))
        }

        IS_ERROR(write_command(POP, GR2))

        switch (simple_operand) {
            case TPLUS:
                IS_ERROR(write_command_Reg_Reg(ADDA, GR1, GR2))
                IS_ERROR(write_command(JOV, EOVF))
                break;
            case  TMINUS:
                IS_ERROR(write_command_Reg_Reg(SUBA, GR2, GR1))
                IS_ERROR(write_command(JOV, EOVF))
                IS_ERROR(write_command_Reg_Reg(LD, GR1, GR2))
                break;
            default: /* TOR */
                IS_ERROR(write_command_Reg_Reg(OR, GR1, GR2))
                break;
        }

        /* push an intermediate expression if there is an operator behind */
        if(token ==    TOR || token ==   TDIV || token ==   TAND ||
           (token >=  TPLUS && token <=  TGREQ) ) {
            IS_ERROR(write_command_Adr_Index(PUSH, 0, GR1))
            isGR1stuffed = 0;
        }
        else {
            isGR1stuffed = 1;
        }
    }

    return simple_type;
}

static int term() {
    int term_type;
    int term_operand;

    /* push the previous expression when there are two or more operators */
    if(isMultipleOperand == 1 && isGR1stuffed == 1) {
        IS_ERROR(write_command_Adr_Index(PUSH, 0, GR1))
        isGR1stuffed = 0;
    }

    FACTOR /* type_token = factor() */
    term_type = type_token;

    while (token == TSTAR || token == TDIV || token == TAND) {
        term_operand = token;
        if(isCallState == 1) {
            isOnlyRightValue = 1; /* flag set */
        }

        switch (token) {
            case TSTAR:
            case  TDIV:
                IS_TYPE_MATCH(type_token, TPINT, "Types of operands of multiplicative operator are different.")
                term_type = TPINT;
                break;
            default: /* TAND */
                IS_TYPE_MATCH(type_token, TPBOOL, "Types of operands of multiplicative operator are different.")
                term_type = TPBOOL;
                break;
        }
        NEXT_TOKEN

        /* push the previous expression when there are two or more operators */
        if(isMultipleOperand == 1 && isGR1stuffed == 1) {
            IS_ERROR(write_command_Adr_Index(PUSH, 0, GR1))
            isGR1stuffed = 0;
        }

        isMultipleOperand = 1; /* flag set */
        FACTOR
        isMultipleOperand = 0; /* flag reset */

        IS_TYPE_MATCH(term_type, type_token, "Types of operands of multiplicative operator are different.")

        if(isGR1stuffed == 0) {
            IS_ERROR(write_command(POP, GR1))
        }

        IS_ERROR(write_command(POP, GR2))

        switch (term_operand) {
            case TSTAR:
                IS_ERROR(write_command_Reg_Reg(MULA, GR1, GR2))
                IS_ERROR(write_command(JOV, EOVF))
                break;
            case  TDIV:
                IS_ERROR(write_command_Reg_Reg(DIVA, GR2, GR1))
                IS_ERROR(write_command(JOV, E0DIV))
                IS_ERROR(write_command_Reg_Reg(LD, GR1, GR2))
                break;
            default: /* TAND */
                IS_ERROR(write_command_Reg_Reg(AND, GR1, GR2))
                break;
        }

        /* push an intermediate expression if there is an operator behind */
        if(token ==    TOR || token ==   TDIV || token ==   TAND ||
           (token >=  TPLUS && token <=  TGREQ) ) {
            IS_ERROR(write_command_Adr_Index(PUSH, 0, GR1))
            isGR1stuffed = 0;
        }
        else {
            isGR1stuffed = 1;
        }
    }

    return term_type;
}

static int factor() {
    int fact_type;

    switch (token) {
        case TNAME:
            if(isGR1stuffed == 1) {
                IS_ERROR(write_command_Adr_Index(PUSH, 0, GR1))
                isGR1stuffed = 0;
            }

            VARIABLE /* type_token = variable() */
            fact_type = type_token;

            /* when the variable is a formal argument or LAD is executed before */
            if(
                ( (token != TCOMMA && token != TRPAREN && token != TTHEN) && (variable_id->ispara == 1 || isCallState == 1)  ) ||
                ( (token == TCOMMA || token == TRPAREN)                   &&
                    (   ( (variable_id->ispara == 1 || isCallState == 1) && isMultipleOperand == 1 ) ||
                        ( variable_id->ispara  == 1 && (isExpressionInFactor == 1 || isMinusTerm == 1) )   )                 ) ||
                ( isOutputState == 1                                      && variable_id->ispara  == 1 )
              )
            {
                IS_ERROR(write_command_Reg_Adr_Index(LD, GR1, 0, GR1))
            }

            if(isOutputState == 0 && isMultipleOperand == 0) {
                IS_ERROR(write_command_Adr_Index(PUSH, 0, GR1))
                isGR1stuffed = 0;
            }
            else {
                isGR1stuffed = 1;
            }

            break;

        case  TNUMBER:
            if(isGR1stuffed == 1) {
                IS_ERROR(write_command_Adr_Index(PUSH, 0, GR1))
                isGR1stuffed = 0;
            }

            IS_ERROR(write_command_Reg_Adr(LAD, GR1, num_attr))

            /* flag set */
            isOnlyRightValue = 1;
            isGR1stuffed = 1;
            NEXT_TOKEN
            fact_type = TPINT;
            break;

        case   TFALSE:
        case    TTRUE:
            if(isGR1stuffed == 1) {
                IS_ERROR(write_command_Adr_Index(PUSH, 0, GR1))
                isGR1stuffed = 0;
            }

            if(token == TTRUE) {
                IS_ERROR(write_command_Reg_Adr(LAD, GR1, 1))
            }
            else {
                IS_ERROR(write_command_Reg_Reg(LD, GR1, GR0))
            }

            /* flag set */
            isOnlyRightValue = 1;
            isGR1stuffed = 1;
            NEXT_TOKEN
            fact_type = TPBOOL;
            break;

        case  TSTRING:
            if(isGR1stuffed == 1) {
                IS_ERROR(write_command_Adr_Index(PUSH, 0, GR1))
                isGR1stuffed = 0;
            }

            /* if string is "''", it is assumed '(one single quotation) */
            if(strcmp("''", string_attr) == 0) {
                IS_ERROR(write_command_Reg_Adr(LAD, GR1, (int)'\''))
            }
            else {
                IS_TYPE_MATCH((int)strlen(string_attr), 1, "size of constant in expression must be 1.")

                IS_ERROR(write_command_Reg_Adr(LAD, GR1, (int) *string_attr))
            }

            /* flag set */
            isOnlyRightValue = 1;
            isGR1stuffed = 1;
            NEXT_TOKEN
            fact_type = TPCHAR;
            break;

        case TLPAREN:

            NEXT_TOKEN

            isExpressionInFactor = 1;
            EXPRESSION /* type_token = expression() */
            isExpressionInFactor = 0;

            fact_type = type_token;

            /* flag set */
            isOnlyRightValue = 1;
            IS_TOKEN(TRPAREN)
            NEXT_TOKEN

            /* push an intermediate expression if there is an operator behind */
            if(isGR1stuffed == 1 &&
               ( token ==    TOR || token ==   TDIV || token ==   TAND ||
                 (token >=  TPLUS && token <=  TGREQ) )
                    ) {
                IS_ERROR(write_command_Adr_Index(PUSH, 0, GR1))
                isGR1stuffed = 0;
            }
            else {
                isGR1stuffed = 1;
            }

            break;

        case TNOT:
            NEXT_TOKEN

            FACTOR /* type_token = factor() */

            IS_TYPE_MATCH(type_token, TPBOOL, "The type of \"not\" operand must be boolean.")

            if(isGR1stuffed == 0) {
                IS_ERROR(write_command(POP, GR1))
            }

            IS_ERROR(write_command_Reg_Adr(LAD, GR2, 1))
            /* bit inversion by XORing with a logical value (true:1 or false:0) and 1 */
            IS_ERROR(write_command_Reg_Reg(XOR, GR1, GR2))

            /* flag set */
            isOnlyRightValue = 1;
            isGR1stuffed = 1;
            fact_type = TPBOOL;
            break;

        case TINTEGER:
        case TBOOLEAN:
        case    TCHAR:

            switch (token) {
                case TINTEGER:
                    fact_type = TPINT;
                    break;
                case TBOOLEAN:
                    fact_type = TPBOOL;
                    break;
                default: /* TCHAR */
                    fact_type = TPCHAR;
                    break;
            }
            NEXT_TOKEN

            IS_TOKEN(TLPAREN)
            NEXT_TOKEN

            EXPRESSION /* type_token = expression() */

            IS_STANDARD_TYPE(type_token, "The type of expression to be cast must be standard type.")

            switch (fact_type) {
                case TPINT:
                    /* do not anything */
                    break;

                case TPBOOL:

                    if(type_token == TPINT || type_token == TPCHAR) {
                        char zero_label[JUMP_LABEL_SIZE];
                        NEW_JUMP_LABEL(zero_label)

                        if(isGR1stuffed == 0) {
                            IS_ERROR(write_command(POP, GR1))
                        }

                        /* compare the value of the expression with GR0 (0) */
                        IS_ERROR(write_command_Reg_Reg(CPA, GR1, GR0))
                        IS_ERROR(write_command(JZE, zero_label))
                        /* if the result is not 0, load ture (= 1) */
                        IS_ERROR(write_command_Reg_Adr(LAD, GR1, 1))
                        IS_ERROR(write_Label(zero_label))
                    }

                    break;

                default: /* TPCHAR */

                    if(type_token == TPINT) {

                        if(isGR1stuffed == 0) {
                            IS_ERROR(write_command(POP, GR1))
                        }

                        /* extract the lower 7 bits */
                        /* 127(10) = 007F(16) = 0000_0000_0111_1111(2) */
                        IS_ERROR(write_command_Reg_Adr(LAD, GR2, 127))
                        IS_ERROR(write_command_Reg_Reg(AND, GR1, GR2))
                    }

                    break;
            }

            isGR1stuffed = 1;
            IS_TOKEN(TRPAREN)
            NEXT_TOKEN
            break;

        default:
            return(parse_error("Factor is not found."));
    }

    return fact_type;
}

static int inputStatement() {
    int read_token;
    isInputState = 1; /* flag set */

    if(token != TREAD && token != TREADLN) {
        /* No error occurs at this point because of an element of FIRST set. */
        return (parse_error("Keyword 'read' or 'readln' is not found."));
    }
    read_token = token;
    NEXT_TOKEN

    if(token != TLPAREN) {

        if (read_token == TREADLN) {
            IS_ERROR(write_command(CALL, READLINE))
        }

        isInputState = 0; /* flag reset */
        return NORMAL;
    }
    NEXT_TOKEN

    VARIABLE /* type_token = variable() */

    IS_TYPES_MATCH(type_token, TPINT, TPCHAR, "The type of variable in isInputState statement must be integer or char.")

    if(type_token == TPINT) {
        IS_ERROR(write_command(CALL, READINT))
    }
    else {
        IS_ERROR(write_command(CALL, READCHAR))
    }

    isGR1stuffed = 0; /* flag reset */

    while (token == TCOMMA) {
        NEXT_TOKEN

        VARIABLE

        IS_TYPES_MATCH(type_token, TPINT, TPCHAR, "The type of variable in isInputState statement must be integer or char.")

        if(type_token == TPINT) {
            IS_ERROR(write_command(CALL, READINT))
        }
        else {
            IS_ERROR(write_command(CALL, READCHAR))
        }
    }

    IS_TOKEN(TRPAREN)
    NEXT_TOKEN

    if (read_token == TREADLN) {
        IS_ERROR(write_command(CALL, READLINE))
    }

    isGR1stuffed = 0; /* flag reset */
    isInputState = 0;
    return NORMAL;
}

static int outputStatement() {
    int write_token;

    if(token != TWRITE && token != TWRITELN) {
        /* No error occurs at this point because of an element of FIRST set. */
        return(parse_error("Keyword 'write' or 'writeln' is not found."));
    }
    write_token = token;
    NEXT_TOKEN

    if(token != TLPAREN) {
        if (write_token == TWRITELN) {
            IS_ERROR(write_command(CALL, WRITELINE))
        }

        return NORMAL;
    }
    NEXT_TOKEN

    OUTPUT_FORMAT

    while (token == TCOMMA) {
        NEXT_TOKEN

        OUTPUT_FORMAT
    }

    IS_TOKEN(TRPAREN)
    NEXT_TOKEN

    if (write_token == TWRITELN) {
        IS_ERROR(write_command(CALL, WRITELINE))
    }

    return NORMAL;
}

static int outputFormat() {

    if(token == TSTRING) {

        if(strlen(string_attr) != 1) {
            char string_label[JUMP_LABEL_SIZE];
            NEW_JUMP_LABEL(string_label)

            IS_ERROR(write_command_Reg_Reg(LAD, GR1, string_label))
            IS_ERROR(write_command_Reg_Reg(LD, GR2, GR0))
            IS_ERROR(write_command(CALL, WRITESTR))

            IS_ERROR(store_label_list(string_label, string_attr))

            NEXT_TOKEN
            isGR1stuffed = 0; /* flag reset */
            return NORMAL;
        }
    }

    isOutputState = 1; /* flag set */
    EXPRESSION /* type_token = expression() */
    isOutputState = 0; /* flag reset */
    IS_STANDARD_TYPE(type_token, "The type of expression in isOutputState format must be standard type.")

    if(token != TCOLON) {
        IS_ERROR(write_command_Reg_Reg(LD, GR2, GR0))

        switch (type_token) {
            case TPINT:
                IS_ERROR(write_command(CALL, WRITEINT))
                break;
            case TPBOOL:
                IS_ERROR(write_command(CALL, WRITEBOOL))
                break;
            default:
                IS_ERROR(write_command(CALL, WRITECHAR))
                break;
        }

        isGR1stuffed = 0; /* flag reset */
        return NORMAL;
    }
    NEXT_TOKEN

    IS_SPECIFIC_TOKEN(TNUMBER, "The number of isOutputState format is not found.")

    IS_ERROR(write_command_Reg_Adr(LAD, GR2, num_attr))

    NEXT_TOKEN

    switch (type_token) {
        case TPINT:
            IS_ERROR(write_command(CALL, WRITEINT))
            break;
        case TPBOOL:
            IS_ERROR(write_command(CALL, WRITEBOOL))
            break;
        default:
            IS_ERROR(write_command(CALL, WRITECHAR))
            break;
    }
    isGR1stuffed = 0; /* flag reset */
    return NORMAL;
}