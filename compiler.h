#ifndef LANGUAGE_PROCESS_4_COMPILER_H
#define LANGUAGE_PROCESS_4_COMPILER_H

/* compiler.h  */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* ---------------------------------------------------------- */

/* scan.c */
#include <stdint.h>

#define MAXSTRSIZE 1024

/* Token */
#define	TNAME		1	/* Name : Alphabet { Alphabet | Digit } */
#define	TPROGRAM	2	/* program : Keyword */
#define	TVAR		3	/* var : Keyword */
#define	TARRAY		4	/* array : Keyword */
#define	TOF	     	5	/* of : Keyword */
#define	TBEGIN		6	/* begin : Keyword */
#define	TEND		7  	/* end : Keyword */
#define	TIF	    	8  	/* if : Keyword */
#define	TTHEN		9	/* then : Keyword */
#define	TELSE		10	/* else : Keyword */
#define	TPROCEDURE	11	/* procedure : Keyword */
#define	TRETURN		12	/* return : Keyword */
#define	TCALL		13	/* call : Keyword */
#define	TWHILE		14	/* while : Keyword */
#define	TDO		    15 	/* do : Keyword */
#define	TNOT		16	/* not : Keyword */
#define	TOR	    	17	/* or : Keyword */
#define	TDIV		18 	/* div : Keyword */
#define	TAND		19 	/* and : Keyword */
#define	TCHAR		20	/* char : Keyword */
#define	TINTEGER	21	/* integer : Keyword */
#define	TBOOLEAN	22 	/* boolean : Keyword */
#define	TREADLN		23	/* readln : Keyword */
#define	TWRITELN	24	/* writeln : Keyword */
#define	TTRUE		25	/* true : Keyword */
#define	TFALSE		26	/* false : Keyword */
#define	TNUMBER		27	/* unsigned integer */
#define	TSTRING		28	/* String */
#define	TPLUS		29	/* + : symbol */
#define	TMINUS		30 	/* - : symbol */
#define	TSTAR		31 	/* * : symbol */
#define	TEQUAL		32 	/* = : symbol */
#define	TNOTEQ		33 	/* <> : symbol */
#define	TLE	    	34 	/* < : symbol */
#define	TLEEQ		35 	/* <= : symbol */
#define	TGR		    36	/* > : symbol */
#define	TGREQ		37	/* >= : symbol */
#define	TLPAREN		38 	/* ( : symbol */
#define	TRPAREN		39 	/* ) : symbol */
#define	TLSQPAREN	40	/* [ : symbol */
#define	TRSQPAREN	41 	/* ] : symbol */
#define	TASSIGN		42	/* := : symbol */
#define	TDOT		43 	/* . : symbol */
#define	TCOMMA		44	/* , : symbol */
#define	TCOLON		45	/* : : symbol */
#define	TSEMI		46	/* ; : symbol */
#define	TREAD		47	/* read : Keyword */
#define	TWRITE		48	/* write : Keyword */
#define	TBREAK		49	/* break : Keyword */

#define NUMOFTOKEN	49
/* ---------------------------------------------------------- */

/* compiler.c */
#define KEYWORDSIZE	28

extern struct KEY {
    char*  keyword;
    int   keytoken;
} key[KEYWORDSIZE];

extern char *tokenstr[NUMOFTOKEN+1];

extern void scan_error(char *mes);
extern int  parse_error(char * mes);
extern int  parse_token_error(int code);
/* ---------------------------------------------------------- */

/* scan.c */
#define NOTGRAPHICCHAR (-2)
#define NOTGRAPHICMESSAGE  "not a graphic character(unauthorized control code or double-byte character)."
#define OVERSTRSIZEMESSAGE "a length of token is too long."

extern int  init_scan(char *filename);
extern int  scan(void);
extern int  get_linenum(void);
extern void end_scan(void);

extern int  num_attr;
extern char string_attr[MAXSTRSIZE];
/* ---------------------------------------------------------- */

/* id-table.c */

struct TYPE {
    int           t_type; /* TPINT TPCHAR TPBOOL TPARRAY TPARRAYINT TPARRAYCHAR TPARRAYBOOL TPPROC */
    int       array_size; /* size of array, if TPARRAY */
    struct TYPE     *etp; /* pointer to element type if TPARRAY */
    struct TYPE  *paratp; /* pointer to parameter's type list if t_type is TPPROC */
};

struct ID {
    char          *name;
    char     *proc_name; /* procedure name within this name is defined */ /* NULL if global name */
    /* int     def_linenum; */
    int          ispara; /* 1:formal parameter, 0:else(variable) */
    struct TYPE    *itp;
    struct LINE  *irefp;
    struct ID    *nextp;
};

extern struct VARNAME {
    char              *name;
    struct VARNAME *nextvnp;
} *temp_passlist;

/* init & release */
extern void init_alltab();
extern void release_alltab();
extern void release_passlist();

/* make a temporary list of  variable names (:names of the same type) */
extern int store_varname(char *var_name);
/* make a temporary list of 'type'(integer, integer, ...) */
extern int store_paralist(int type);
/* make a temporary list of 'type' to use for registering formal parameters */
extern int store_formalParalist(int type);

/* check duplicate definition and register 'idname' with 'type' in the table specified by 'scope' */
extern int add_idtab(int scope, int type, char *idname);
/* register variables in the temporary list of variable names with 'type' */
extern int add_variables_idtab(int scope, int type);
/* register formal parameter types in the procedure 'proc_name' */
extern int add_formalPara_typetab(char *proc_name);

/* check if 'proc_name' is declared and register the reference line */
extern int check_proc_id(struct ID *proc_id);
/* return the pointer of referenced variable ID */
extern struct ID *get_var_id(int scope, char *var_name);
/* return the type of variable */
extern int get_vartype(struct ID *var_id);
/* collate the list of formal parameters and actual arguments */
extern int verify_paralist(struct ID *proc_id);

#define IS_STRUCT_MALLOC_ERROR(p, STRUCT) if (((p) = (struct STRUCT *)malloc(sizeof(struct STRUCT))) == NULL) {printf("\nERROR:can not malloc\n"); return (ERROR);}
#define IS_STRING_MALLOC_ERROR(p, string) if (((p) = (char *)malloc(strlen(string)+1)) == NULL)               {printf("\nERROR:can not malloc\n"); return (ERROR);}

/* search the name pointed by "idname" from the table specified by 'scope' */
extern struct ID *search_idtab(int scope, char *idname);

/* ---------------------------------------------------------- */

/* CASL2-printer.c */

#define COMMAND_PLACE   10
#define OPERAND_PLACE    8
#define JUMP_LABEL_SIZE  6 /* L0001 ~ L9999 */
#define MAX_LABEL_SIZE 128

extern int labelnum;

/* define string using to write CASL2 program */
#define GR0 "gr0"
#define GR1 "gr1"
#define GR2 "gr2"
#define GR3 "gr3"
#define GR4 "gr4"
#define GR5 "gr5"
#define GR6 "gr6"
#define GR7 "gr7"

#define LD    "LD      " /* LoaD                   */
#define ST    "ST      " /* STore                  */
#define LAD   "LAD     " /* Load ADress            */
#define ADDA  "ADDA    " /* ADD Arithmetic         */
#define ADDL  "ADDL    " /* ADD Logical            */
#define SUBA  "SUBA    " /* SUBtract Arithmetic    */
#define SUBL  "SUBL    " /* SUBtract Logical       */
#define AND   "AND     " /* AND                    */
#define OR    "OR      " /* OR                     */
#define XOR   "XOR     " /* eXclusive OR           */
#define CPA   "CPA     " /* ComPare Arithmetic     */
#define CPL   "CPL     " /* Compare Logical        */
#define SLA   "SLA     " /* Shift Left Arithmetic  */
#define SRA   "SRA     " /* Shift Right Arithmetic */
#define SLL   "SLL     " /* Shift Left Logical     */
#define SRL   "SRL     " /* Shift Right Logical    */
#define JPL   "JPL     " /* Jump on PLus           */
#define JMI   "JMI     " /* Jump on Minus          */
#define JNZ   "JNZ     " /* Jump on Non Zero       */
#define JZE   "JZE     " /* Jump on ZEro           */
#define JOV   "JOV     " /* Jump on OVerflow       */
#define JUMP  "JUMP    " /* unconditional JUMP     */
#define PUSH  "PUSH    " /* PUSH                   */
#define POP   "POP     " /* POP                    */
#define CALL  "CALL    " /* CALL subroutine        */
#define RET   "RET     " /* RETurn from subroutine */
#define SVC   "SVC     " /* SuperVisor Call        */
#define NOP   "NOP     " /* No Operation           */

#define MULA  "MULA    " /* MULtiply Arithmetic    */
#define MULL  "MULL    " /* MULtiply Logical       */
#define DIVA  "DIVA    " /* DIVide Arithmetic      */
#define DIVL  "DIVL    " /* DIVide Logical         */

#define START "START   "
#define END   "END     "
#define DS    "DS      "
#define DC    "DC      "
#define IN    "IN      "
#define OUT   "OUT     "
#define RPUSH "RPUSH   "
#define RPOP  "RPOP    "

#define EOVF      "EOVF"
#define E0DIV     "E0DIV"
#define EROV      "EROV"
#define FLUSH     "FLUSH"
#define WRITECHAR "WRITECHAR"
#define WRITESTR  "WRITESTR"
#define WRITEINT  "WRITEINT"
#define WRITEBOOL "WRITEBOOL"
#define WRITELINE "WRITELINE"
#define READCHAR  "READCHAR"
#define READINT   "READINT"
#define READLINE  "READLINE"

#define NEW_JUMP_LABEL(label) if(sprintf(label, "L%04d", labelnum++) == EOF ) { return(parse_error("Failed to make label."));}

extern int  init_write(char *filename);
extern void end_write(void);

/* push & pop the stack of end label of while statement */
extern int while_stack_push(char *label);
extern int while_stack_pop(char *label);

extern void init_label_list();
/* make a temporary list of DC label */
extern int store_label_list(char *label, char *string);
/* write DC instructions for strings and areas for passing by reference. */
extern int write_DC_labels(void);

/* make variable label by ID information and set to pointer *label */
extern int make_var_label_byID(struct ID *id, char *label);

/* write command to secure the area of an argument */
extern int write_pass_reference(void);
/* pass addresses of arguments to formal parameters */
extern int write_pass_Adr_to_formalPara(char *proc_name);

/* write command to declare variable */
extern int write_var_declaration_byID(struct ID *var_id);
/* write command to refer variable */
extern int write_var_reference_byID(struct ID *var_id);

/* write START label of CASL2 program */
extern int write_start(char *program_name);
/* write library using in CASL2 program */
extern int write_library(void);

/* interface function */
extern int write_Label(char *label);
extern int write_command(char *commamd, char *operand);
extern int write_command_Adr(char *command, int adr);
extern int write_command_Reg_Reg(char *command, char *gr1, char *gr2);
extern int write_command_Reg_Adr(char *command, char *gr, int adr);
extern int write_command_Adr_Index(char *command, int adr, char *index);
extern int write_command_Reg_Adr_Index(char *command, char *gr, int adr, char *index);

/* ---------------------------------------------------------- */

/* LL1-parsing.c */

/* maximum depth of while statements */
/* an error will occur if the number of repetitions exceeds this number. */
#define MAX_WHILE_DEPTH 20

#define NORMAL  0
#define ERROR   1

#define TPINT       2
#define TPCHAR      3
#define TPBOOL      4
#define TPARRAY     5
#define TPARRAYINT  6
#define TPARRAYCHAR 7
#define TPARRAYBOOL 8
#define TPPROC      9

#define GLOBAL 0
#define LOCAL  1

/* read the next token */
#define NEXT_TOKEN token = scan();
/* token check */
#define IS_TOKEN(code)               if(token != (code)) {return(parse_token_error(code));}
/* specific token check */
#define IS_SPECIFIC_TOKEN(code, str) if(token != (code)) {return(parse_error(str));}

/* standard type check */
#define IS_STANDARD_TYPE(t, str)             if((t) != TPINT && (t) != TPBOOL && (t) != TPCHAR) {return(parse_error(str));}
/* type match check */
#define IS_TYPE_MATCH(t, type1, str)         if((t) != (type1))                                 {return(parse_error(str));}
#define IS_TYPES_MATCH(t, type1, type2, str) if((t) != (type1) && (t) != (type2))               {return(parse_error(str));}

/* non-terminal symbol check */
#define BLOCK                  if(block() == ERROR)                           {return (ERROR);}
#define VARIABLE_DECLARAION    if(variableDeclaration() == ERROR)             {return (ERROR);}
#define VARIABLE_NAMES         if(variableNames() == ERROR)                   {return (ERROR);}
#define TYPES                  if((type_token = type()) == ERROR)             {return (ERROR);}
#define ARRAY_TYPE             if(arrayType() == ERROR)                       {return (ERROR);}
#define SUBPROGRAM_DECLARATION if(subprogramDeclaration() == ERROR)           {return (ERROR);}
#define FORMAL_PARAMETERS      if(formalParameters() == ERROR)                {return (ERROR);}
#define COMPOUND_STATEMENT     if(compoundStatement() == ERROR)               {return (ERROR);}
#define STATEMENT              if(statement() == ERROR)                       {return (ERROR);}
#define CONDITION_STATEMENT    if(conditionStatement() == ERROR)              {return (ERROR);}
#define ITERATION_STATEMENT    if(iterationStatement() == ERROR)              {return (ERROR);}
#define CALL_STATEMENT         if(callStatement() == ERROR)                   {return (ERROR);}
#define ASSIGNMENT_STATEMENT   if(assignmentStatement() == ERROR)             {return (ERROR);}
#define VARIABLE               if((type_token = variable()) == ERROR)         {return (ERROR);}
#define EXPRESSION             if((type_token = expression()) == ERROR)       {return (ERROR);}
#define SIMPLE_EXPRESSION      if((type_token = simpleExpression()) == ERROR) {return (ERROR);}
#define TERM                   if((type_token = term()) == ERROR)             {return (ERROR);}
#define FACTOR                 if((type_token = factor()) == ERROR)           {return (ERROR);}
#define INPUT_STATEMENT        if(inputStatement() == ERROR)                  {return (ERROR);}
#define OUTPUT_STATEMENT       if(outputStatement() == ERROR)                 {return (ERROR);}
#define OUTPUT_FORMAT          if(outputFormat() == ERROR)                    {return (ERROR);}

#define IS_ERROR(func)         if((func) == ERROR)                            {return (ERROR);}

/* variables */
extern int                                          token; /* look-ahead token buffer */
extern int                                     type_token; /* type of look-ahead token */
extern int                               depth_whileState; /* block depth of while statement */
extern char               declaring_proc_name[MAXSTRSIZE]; /* The name of procedure of subprogram declaration */
extern char label_stack[MAX_WHILE_DEPTH][JUMP_LABEL_SIZE]; /* stack of end label of while statement */

extern struct ID                             *variable_id; /* variable ID pointer */

/* flag */
extern int      isLocal; /* 1(LOCAL) = local variable, 0(GLOBAL) = global variable */
extern int  isParameter; /* 1:formal parameter, 0:else(variable) */
extern int isGR1stuffed; /* 1:GR1 is full, 0:GR1 is empty */

extern int   isCallState; /* 1:in callStatement() */
extern int isAssignState; /* 1:in Left Value of assignmentStatement() */
extern int  isInputState; /* 1:in inputStatement() */

extern int parse_program();

#endif /* LANGUAGE_PROCESS_4_COMPILER_H */
