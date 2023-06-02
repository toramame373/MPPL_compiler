/* CASL2-printer.c */

#include "compiler.h"

struct LABEL {
    char         *label; /* number label (L0001 ~ L9999) */
    char      *constant; /* constants defined by DC instructions */
    struct LABEL *nextp;
} *label_list;

/* global variables */
int  labelnum;

static FILE   *filep;

/* static function */
static int write_Blank(void);
static int write_Blanks(int num);
static int write_LineCode(void);

/* make a label within MAX_LABEL_SIZE */
static int make_var_label(char *name, char *proc_name, char *label);
/* secure an area for string */
static int write_str_declaration(char *label, char *str);
/* secure an area for passing by reference */
static int write_R_value_declaration(char *label);

/*-----function called from the main program------*/

/* initialize */
int init_write(char *filename) {
    /* Initialize */
    labelnum = 1;
    char name[strlen(filename)+1];

    strcpy(name, filename);
    strtok(name, ".");
    strcat(name, ".csl");

    if((filep = fopen(name, "w")) == NULL) {
        printf("File %s can not open.\n", name);
        return (ERROR);
    }

    return NORMAL;
}

/* termination */
void end_write(void) {
    fclose(filep);
}

/* ----------------------------------------------------- */

/* push & pop the stack of end label of while statement */
int while_stack_push(char *label) {
    if(depth_whileState >= MAX_WHILE_DEPTH) {
        return parse_error("Stack overflow.");
    }

    strcpy(label_stack[depth_whileState - 1], label);

    return NORMAL;
}

int while_stack_pop(char *label) {
    if(depth_whileState <= 0) {
        return parse_error("Stack underflow.");
    }

    strcpy(label, label_stack[depth_whileState - 1]);

    return NORMAL;
}

/* ----------------------------------------------------- */

void init_label_list() {
    label_list = NULL;
}

void release_label_list() {
    struct LABEL *p, *q;

    for(p = label_list; p != NULL; p = q) {
        free(p->label);
        free(p->constant);
        q = p->nextp;
        free(p);
    }
    init_label_list();
}

/* make a temporary list of DC label */
int store_label_list(char *label, char *string) {
    struct LABEL *p, *q;
    char *lab;
    char *str = NULL;

    IS_STRING_MALLOC_ERROR(lab, label)
    strcpy(lab, label);

    if(string != NULL) {
        IS_STRING_MALLOC_ERROR(str, string)
        strcpy(str, string);
    }

    IS_STRUCT_MALLOC_ERROR(p, LABEL)
    p->label  = lab;
    p->constant = str;
    p->nextp  = NULL;

    if ((q = label_list) == NULL) {
        label_list = p; /* put the first data in the list */
        return NORMAL;
    }

    while (q->nextp != NULL) {
        q = q->nextp;
    }
    q->nextp = p; /* connect new member to last */

    return NORMAL;
}

/* register variables in the temporary list of variable names */
int write_DC_labels(void) {
    struct LABEL *p;

    for(p = label_list; p != NULL; p = p->nextp) {

        if(p->constant != NULL) {
            IS_ERROR(write_str_declaration(p->label, p->constant))
        }
        else {
            IS_ERROR(write_R_value_declaration(p->label))
        }

    }

    /* reset list */
    release_label_list();
    return NORMAL;
}

/* ----------------------------------------------------- */

/* make variable label by ID information and set to pointer *label */
int make_var_label_byID(struct ID *id, char *label) {

    if(id->proc_name != NULL) {
        IS_ERROR(make_var_label(id->name, id->proc_name, label))
        return NORMAL;
    }
    else {
        IS_ERROR(make_var_label(id->name, NULL, label))
        return NORMAL;
    }
}

/* ----------------------------------------------------- */

/* write command to secure the area of an argument */
int write_pass_reference(void) {
    char r_value_label[JUMP_LABEL_SIZE];
    NEW_JUMP_LABEL(r_value_label)

    IS_ERROR(store_label_list(r_value_label, NULL))
    IS_ERROR(write_command_Reg_Reg(LAD, GR2, r_value_label))
    IS_ERROR(write_command_Reg_Adr_Index(ST, GR1, 0, GR2))
    IS_ERROR(write_command_Adr_Index(PUSH, 0, GR2))

    return NORMAL;
}

/* pass addresses of arguments to formal parameters */
int write_pass_Adr_to_formalPara(char *proc_name) {
    struct VARNAME *p;
    struct ID *q;

    for(p = temp_passlist; p != NULL; p = p->nextvnp) {

        if((q = search_idtab(LOCAL, p->name)) != NULL && q->ispara == 1) {

            char var_label[MAX_LABEL_SIZE];
            IS_ERROR(make_var_label(p->name, proc_name, var_label))

            IS_ERROR(write_command(POP, GR1))
            IS_ERROR(write_command_Reg_Reg(ST, GR1, var_label))
        }
    }

    release_passlist();
    return NORMAL;
}

/* write command to declare variable */
int write_var_declaration_byID(struct ID *var_id) {
    int strsize, blanksize;
    char var_label[MAX_LABEL_SIZE];

    if(var_id->itp->t_type != TPPROC) {
        IS_ERROR(make_var_label_byID(var_id, var_label))
        if(fprintf(filep, "%s", var_label) < 0) {
            printf("Failed to write the file.\n");
            return (ERROR);
        }

        if((strsize = (int)strlen(var_label)) < 10) {
            blanksize = COMMAND_PLACE;
        }
        else if(strsize < 18) {
            blanksize = COMMAND_PLACE  + OPERAND_PLACE;
        }
        else if(strsize < 26) {
            blanksize = COMMAND_PLACE  + OPERAND_PLACE + OPERAND_PLACE;
        }
        else {
            blanksize = 0; /* sets only 1 blank if the label length is 26 or more */
        }

        if(blanksize != 0) {
            IS_ERROR(write_Blanks(blanksize - strsize))
        }
        else {
            IS_ERROR(write_Blank())
        }

        if(var_id->itp->t_type == TPARRAY) {

            if(fprintf(filep, "%s%d", DS, var_id->itp->array_size) < 0) {
                printf("Failed to write the file.\n");
                return (ERROR);
            }

        }
        else {
            if(fprintf(filep, "%s%d", DC, 0) < 0) {
                printf("Failed to write the file.\n");
                return (ERROR);
            }
        }
        IS_ERROR(write_LineCode())
    }

    return NORMAL;
}

/* write command to refer variable */
int write_var_reference_byID(struct ID *var_id) {
    char var_label[MAX_LABEL_SIZE];
    IS_ERROR(make_var_label_byID(var_id, var_label))

        if(var_id->itp->t_type == TPARRAY ||
           ( var_id->ispara == 0 && (isCallState == 1 || isInputState == 1) )
          ) {
            IS_ERROR(write_command_Reg_Reg(LAD, GR1, var_label))
            isGR1stuffed = 1;
        }
        else {

            if(isAssignState == 0 || var_id->ispara == 1) {
                IS_ERROR(write_command_Reg_Reg(LD, GR1, var_label))
                isGR1stuffed = 1;
            }
        }

    return NORMAL;
}

/* ----------------------------------------------------- */

/* interface function */
int write_Label(char *label) {
    if(fprintf(filep, "%s", label) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    IS_ERROR(write_LineCode())
    return NORMAL;
}

int write_start(char *program_name) {
    if(fprintf(filep, "$$%s", program_name) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    IS_ERROR(write_Blanks((int)( (COMMAND_PLACE + OPERAND_PLACE) - (strlen(program_name)+2) )))

    if(fprintf(filep, START) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    IS_ERROR(write_LineCode())
    return NORMAL;
}

int write_command(char *commamd, char *operand) {
    IS_ERROR(write_Blanks(COMMAND_PLACE))

    if(fprintf(filep, "%s%s", commamd, operand) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    IS_ERROR(write_LineCode())
    return NORMAL;
}

int write_command_Reg_Reg(char *command, char *gr1, char *gr2) {
    IS_ERROR(write_Blanks(COMMAND_PLACE))

    if(fprintf(filep, "%s%s,", command, gr1) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    IS_ERROR(write_Blanks(5))

    if(fprintf(filep, "%s", gr2) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    IS_ERROR(write_LineCode())
    return NORMAL;
}

int write_command_Reg_Adr(char *command, char *gr, int adr) {
    IS_ERROR(write_Blanks(COMMAND_PLACE))

    if(fprintf(filep, "%s%s,", command, gr) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    IS_ERROR(write_Blanks(5))

    if(fprintf(filep, "%d", adr) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    IS_ERROR(write_LineCode())
    return NORMAL;
}

int write_command_Adr(char *command, int adr) {
    IS_ERROR(write_Blanks(COMMAND_PLACE))

    if(fprintf(filep, "%s%d", command, adr) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    IS_ERROR(write_LineCode())
    return NORMAL;
}

int write_command_Adr_Index(char *command, int adr, char *index) {
    IS_ERROR(write_Blanks(COMMAND_PLACE))

    if(fprintf(filep, "%s%d, %s", command, adr, index) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    IS_ERROR(write_LineCode())
    return NORMAL;
}

int write_command_Reg_Adr_Index(char *command, char *gr, int adr, char *index) {
    IS_ERROR(write_Blanks(COMMAND_PLACE))

    if(fprintf(filep, "%s%s,", command, gr) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    IS_ERROR(write_Blanks(5))

    if(fprintf(filep, "%d, ", adr) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    if(fprintf(filep, "%s", index) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    IS_ERROR(write_LineCode())
    return NORMAL;
}

/* ----------------------------------------------------- */

/* write library using in CASL2 program */
int write_library(void) {

    if(fprintf(filep, ";\n"
                      ";----------------------------Below is library----------------------------\n"
                      ";\n"
                      ) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    if(fprintf(filep, "EOVF ; オーバーフローエラー\n"
                      "  CALL  WRITELINE\n"
                      "  LAD   gr1, EOVF1\n"
                      "  LD    gr2, gr0\n"
                      "  CALL  WRITESTR\n"
                      "  CALL  WRITELINE\n"
                      "  SVC   1  ;  overflow error stop\n"
                      "EOVF1    DC  '***** Run-Time Error : Overflow *****'\n"
                      "E0DIV ;０除算エラー\n"
                      "  JNZ   EOVF\n"
                      "  CALL  WRITELINE\n"
                      "  LAD   gr1, E0DIV1\n"
                      "  LD    gr2, gr0\n"
                      "  CALL  WRITESTR\n"
                      "  CALL  WRITELINE\n"
                      "  SVC   2  ;  0-divide error stop\n"
                      "E0DIV1    DC  '***** Run-Time Error : Zero-Divide *****'\n"
                      "EROV ; 配列の範囲外エラー\n"
                      "  CALL  WRITELINE\n"
                      "  LAD   gr1, EROV1\n"
                      "  LD    gr2, gr0\n"
                      "  CALL  WRITESTR\n"
                      "  CALL  WRITELINE\n"
                      "  SVC   3  ;  range-over error stop\n"
                      "EROV1     DC  '***** Run-Time Error : Range-Over in Array Index *****'\n"
                      ) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    if(fprintf(filep, "WRITECHAR\n"
                      "; gr1の値（文字）をgr2のけた数で出力する．\n"
                      "; gr2が0なら必要最小限の桁数で出力する\n"
                      "  RPUSH\n"
                      "  LD  gr6, SPACE\n"
                      "  LD  gr7, OBUFSIZE\n"
                      "WC1\n"
                      "  SUBA  gr2, ONE        ; while(--c > 0) {\n"
                      "  JZE   WC2\n"
                      "  JMI   WC2\n"
                      "  ST    gr6, OBUF, gr7  ;  *p++ = ' ';\n"
                      "  CALL  BOVFCHECK\n"
                      "  JUMP  WC1             ; }\n"
                      "WC2\n"
                      "  ST    gr1, OBUF,gr7   ; *p++ = gr1;\n"
                      "  CALL  BOVFCHECK\n"
                      "  ST    gr7, OBUFSIZE\n"
                      "  RPOP\n"
                      "  RET\n"
                      ) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    if(fprintf(filep, "WRITESTR\n"
                      "; gr1が指す文字列をgr2のけた数で出力する．\n"
                      "; gr2が0なら必要最小限の桁数で出力する\n"
                      "  RPUSH\n"
                      "  LD  gr6, gr1    ; p = gr1;\n"
                      "WS1\n"
                      "  LD    gr4, 0,gr6   ; while(*p != '¥0') {\n"
                      "  JZE   WS2\n"
                      "  ADDA  gr6, ONE     ;  p++;\n"
                      "  SUBA  gr2, ONE     ;  c--;\n"
                      "  JUMP  WS1          ; }\n"
                      "WS2\n"
                      "  LD  gr7, OBUFSIZE  ; q = OBUFSIZE;\n"
                      "  LD  gr5, SPACE\n"
                      "WS3\n"
                      "  SUBA  gr2, ONE       ; while(--c >= 0) {\n"
                      "  JMI   WS4\n"
                      "  ST    gr5, OBUF,gr7  ;  *q++ = ' ';\n"
                      "  CALL  BOVFCHECK\n"
                      "  JUMP  WS3  ; }\n"
                      "WS4\n"
                      "  LD    gr4, 0,gr1     ; while(*gr1 != '¥0') {\n"
                      "  JZE   WS5\n"
                      "  ST    gr4, OBUF,gr7  ;  *q++ = *gr1++;\n"
                      "  ADDA  gr1, ONE\n"
                      "  CALL  BOVFCHECK\n"
                      "  JUMP  WS4            ; }\n"
                      "WS5\n"
                      "  ST  gr7, OBUFSIZE  ; OBUFSIZE = q;\n"
                      "  RPOP\n"
                      "  RET\n"
                      "BOVFCHECK\n"
                      "    ADDA  gr7, ONE\n"
                      "    CPA   gr7, BOVFLEVEL\n"
                      "    JMI   BOVF1\n"
                      "    CALL  WRITELINE\n"
                      "    LD    gr7, OBUFSIZE\n"
                      "BOVF1\n"
                      "    RET\n"
                      "BOVFLEVEL  DC 256\n"
                      ) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    if(fprintf(filep, "WRITEINT\n"
                      "; gr1の値（整数）をgr2のけた数で出力する．\n"
                      "; gr2が0なら必要最小限の桁数で出力する\n"
                      "  RPUSH\n"
                      "  LD    gr7, gr0  ; flag = 0;\n"
                      "  CPA   gr1, gr0  ; if(gr1>=0) goto WI1;\n"
                      "  JPL   WI1\n"
                      "  JZE   WI1\n"
                      "  LD    gr4, gr0  ; gr1= - gr1;\n"
                      "  SUBA  gr4, gr1\n"
                      "  CPA   gr4, gr1\n"
                      "  JZE   WI6\n"
                      "  LD    gr1, gr4\n"
                      "  LD    gr7, ONE  ; flag = 1;\n"
                      "WI1\n"
                      "  LD    gr6, SIX         ; p = INTBUF+6;\n"
                      "  ST    gr0, INTBUF, gr6  ; *p = '¥0';\n"
                      "  SUBA  gr6, ONE         ; p--;\n"
                      "  CPA   gr1, gr0         ; if(gr1 == 0)\n"
                      "  JNZ   WI2\n"
                      "  LD    gr4, ZERO        ;  *p = '0';\n"
                      "  ST    gr4, INTBUF, gr6\n"
                      "  JUMP  WI5              ; }\n"
                      "WI2      ; else {\n"
                      "  CPA   gr1, gr0          ;  while(gr1 != 0) {\n"
                      "  JZE   WI3\n"
                      "  LD    gr5, gr1          ;   gr5 = gr1 - (gr1 / 10) * 10;\n"
                      "  DIVA  gr1, TEN          ;   gr1 /= 10;\n"
                      "  LD    gr4, gr1\n"
                      "  MULA  gr4, TEN\n"
                      "  SUBA  gr5, gr4\n"
                      "  ADDA  gr5, ZERO         ;   gr5 += '0';\n"
                      "  ST    gr5, INTBUF, gr6  ;   *p = gr5;\n"
                      "  SUBA  gr6, ONE          ;   p--;\n"
                      "  JUMP  WI2               ;  }\n"
                      ) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    if(fprintf(filep, "WI3\n"
                      "  CPA   gr7, gr0          ;  if(flag != 0) {\n"
                      "  JZE   WI4\n"
                      "  LD    gr4, MINUS        ;   *p = '-';\n"
                      "  ST    gr4, INTBUF, gr6\n"
                      "  JUMP  WI5               ;  }\n"
                      "WI4\n"
                      "  ADDA  gr6, ONE          ;  else p++;\n"
                      "                          ; }\n"
                      "WI5\n"
                      "  LAD   gr1, INTBUF,gr6   ; gr1 = p;\n"
                      "  CALL  WRITESTR          ; WRITESTR();\n"
                      "  RPOP\n"
                      "  RET\n"
                      "WI6\n"
                      "  LAD   gr1, MMINT\n"
                      "  CALL  WRITESTR           ; WRITESTR();\n"
                      "  RPOP\n"
                      "  RET\n"
                      "MMINT    DC  '-32768'\n"
                      ) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    if(fprintf(filep, "WRITEBOOL\n"
                      "; gr1の値（真理値）が0なら'FALSE'を\n"
                      "; 0以外なら'TRUE'をgr2のけた数で出力する．\n"
                      "; gr2が0なら必要最小限の桁数で出力する\n"
                      "  RPUSH\n"
                      "  CPA   gr1, gr0     ; if(gr1 != 0)\n"
                      "  JZE   WB1\n"
                      "  LAD   gr1, WBTRUE  ;  gr1 = \"TRUE\";\n"
                      "  JUMP  WB2\n"
                      "WB1      ; else\n"
                      "  LAD   gr1, WBFALSE ;  gr1 = \"FALSE\";\n"
                      "WB2\n"
                      "  CALL  WRITESTR     ; WRITESTR();\n"
                      "  RPOP\n"
                      "  RET\n"
                      "WBTRUE     DC  'TRUE'\n"
                      "WBFALSE    DC  'FALSE'\n"
                      ) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    if(fprintf(filep, "WRITELINE\n"
                      "; 改行を出力する\n"
                      "  RPUSH\n"
                      "  LD    gr7, OBUFSIZE\n"
                      "  LD    gr6, NEWLINE\n"
                      "  ST    gr6, OBUF,gr7\n"
                      "  ADDA  gr7, ONE\n"
                      "  ST    gr7, OBUFSIZE\n"
                      "  OUT   OBUF, OBUFSIZE\n"
                      "  ST    gr0, OBUFSIZE\n"
                      "  RPOP\n"
                      "  RET\n"
                      ) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    if(fprintf(filep, "FLUSH\n"
                      "  RPUSH\n"
                      "  LD gr7, OBUFSIZE\n"
                      "  JZE FL1\n"
                      "  CALL WRITELINE\n"
                      "FL1\n"
                      "  RPOP\n"
                      "  RET\n"
                      ) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    if(fprintf(filep, "READCHAR\n"
                      "; gr1が指す番地に文字一つを読み込む\n"
                      "  RPUSH\n"
                      "  LD    gr5, RPBBUF  ; if(RPBBUF != '¥0') {\n"
                      "  JZE   RC0\n"
                      "  ST    gr5, 0,gr1   ;  *gr1 = RPBBUF;\n"
                      "  ST    gr0, RPBBUF  ;  RPBBUF = '¥0'\n"
                      "  JUMP  RC3          ;  return; }\n"
                      "RC0\n"
                      "  LD   gr7, INP        ; inp = INP;\n"
                      "  LD   gr6, IBUFSIZE   ; if(IBUFSIZE == 0) {\n"
                      "  JNZ  RC1\n"
                      "  IN   IBUF, IBUFSIZE  ;  IN();\n"
                      "  LD   gr7, gr0        ;  inp = 0;\n"
                      "                       ; }\n"
                      "RC1\n"
                      "  CPA   gr7, IBUFSIZE  ; if(inp == IBUFSIZE) {\n"
                      "  JNZ   RC2\n"
                      "  LD    gr5, NEWLINE   ;  *gr1 = '¥n';\n"
                      "  ST    gr5, 0,gr1\n"
                      "  ST    gr0, IBUFSIZE  ;  IBUFSIZE = INP = 0;\n"
                      "  ST    gr0, INP\n"
                      "  JUMP  RC3            ; }\n"
                      "RC2      ; else {\n"
                      "  LD    gr5, IBUF,gr7  ;  *gr1 = *inp++;\n"
                      "  ADDA  gr7, ONE\n"
                      "  ST    gr5, 0,gr1\n"
                      "  ST    gr7, INP       ;  INP = inp;\n"
                      "RC3      ; }\n"
                      "  RPOP\n"
                      "  RET\n"
                      ) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    if(fprintf(filep, "READINT\n"
                      "; gr1が指す番地に整数値一つを読み込む\n"
                      "  RPUSH\n"
                      "RI1      ; do {\n"
                      "  CALL  READCHAR    ;  ch = READCHAR();\n"
                      "  LD    gr7, 0,gr1\n"
                      "  CPA   gr7, SPACE  ; } while(ch == ' ' || ch == '¥t' || ch == '¥n');\n"
                      "  JZE   RI1\n"
                      "  CPA   gr7, TAB\n"
                      "  JZE   RI1\n"
                      "  CPA   gr7, NEWLINE\n"
                      "  JZE   RI1\n"
                      "  LD    gr5, ONE    ; flag = 1\n"
                      "  CPA   gr7, MINUS  ; if(ch == '-') {\n"
                      "  JNZ   RI4\n"
                      "  LD    gr5, gr0    ;  flag = 0;\n"
                      "  CALL  READCHAR    ;  ch = READCHAR();\n"
                      "  LD    gr7, 0,gr1\n"
                      "RI4      ; }\n"
                      "  LD  gr6, gr0      ; v = 0;\n"
                      ) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    if(fprintf(filep, "RI2\n"
                      "  CPA   gr7, ZERO    ; while('0' <= ch && ch <= '9') {\n"
                      "  JMI   RI3\n"
                      "  CPA   gr7, NINE\n"
                      "  JPL   RI3\n"
                      "  MULA  gr6, TEN     ;  v = v*10+ch-'0';\n"
                      "  ADDA  gr6, gr7\n"
                      "  SUBA  gr6, ZERO\n"
                      "  CALL  READCHAR     ;  ch = READSCHAR();\n"
                      "  LD    gr7, 0,gr1\n"
                      "  JUMP  RI2          ; }\n"
                      "RI3\n"
                      "  ST    gr7, RPBBUF  ; ReadPushBack();\n"
                      "  ST    gr6, 0,gr1   ; *gr1 = v;\n"
                      "  CPA   gr5, gr0     ; if(flag == 0) {\n"
                      "  JNZ   RI5\n"
                      "  SUBA  gr5, gr6     ;  *gr1 = -v;\n"
                      "  ST    gr5, 0,gr1\n"
                      "RI5      ; }\n"
                      "  RPOP\n"
                      "  RET\n"
                      ) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    if(fprintf(filep, "READLINE\n"
                      "; 入力を改行コードまで（改行コードも含む）読み飛ばす\n"
                      "  ST  gr0, IBUFSIZE\n"
                      "  ST  gr0, INP\n"
                      "  ST  gr0, RPBBUF\n"
                      "  RET\n"
                      ) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    if(fprintf(filep, "ONE        DC  1\n"
                      "SIX        DC  6\n"
                      "TEN        DC  10\n"
                      "SPACE      DC  #0020  ; ' '\n"
                      "MINUS      DC  #002D  ; '-'\n"
                      "TAB        DC  #0009  ; '¥t'\n"
                      "ZERO       DC  #0030  ; '0'\n"
                      "NINE       DC  #0039  ; '9'\n"
                      "NEWLINE    DC  #000A  ; '¥n'\n"
                      "INTBUF     DS  8\n"
                      "OBUFSIZE   DC  0\n"
                      "IBUFSIZE   DC  0\n"
                      "INP        DC  0\n"
                      "OBUF       DS  257\n"
                      "IBUF       DS  257\n"
                      "RPBBUF     DC  0\n"
                      ) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    return NORMAL;
}

/*-----static function-----*/
static int write_Blank(void) {
    if(fprintf(filep, " ") < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }
    return NORMAL;
}

static int write_Blanks(int num) {
    int i;

    for(i = 0; i < num; i++) {
        if(write_Blank() < 0) {
            printf("Failed to write the file.\n");
            return (ERROR);
        }
    }
    return NORMAL;
}

static int write_LineCode(void) {
    if(fprintf(filep, "\n") < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }
    return NORMAL;
}

static int make_var_label(char *name, char *proc_name, char *label) {
    if(proc_name != NULL) {
        if((strlen(name)+strlen(proc_name)+2) > (MAX_LABEL_SIZE-1)) {
            return parse_error("Label length limit exceeded.");
        }

        if(sprintf(label, "$%s%%%s", name, proc_name) == EOF) {
            return parse_error("Failed to make variable label.");
        }
    }
    else {
        if((strlen(name)+1) > (MAX_LABEL_SIZE-1)) {
            return parse_error("Label length limit exceeded.");
        }

        if(sprintf(label, "$%s", name) == EOF) {
            return parse_error("Failed to make variable label.");
        }
    }
    return NORMAL;
}

/* secure an area for string */
static int write_str_declaration(char *label, char *str) {
    char write_string[strlen(str)+2];
    sprintf(write_string, "'%s'", str);

    if(fprintf(filep, "%s", label) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }
    IS_ERROR(write_Blanks(COMMAND_PLACE - (int)strlen(label)))

    if(fprintf(filep, "%s%s", DC, write_string) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    IS_ERROR(write_LineCode())
    return NORMAL;
}

/* secure an area for passing by reference */
static int write_R_value_declaration(char *label) {

    if(fprintf(filep, "%s", label) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }
    IS_ERROR(write_Blanks(COMMAND_PLACE - (int)strlen(label)))

    if(fprintf(filep, "%s%d", DC, 0) < 0) {
        printf("Failed to write the file.\n");
        return (ERROR);
    }

    IS_ERROR(write_LineCode())
    return NORMAL;
}