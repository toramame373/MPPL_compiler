/* compiler.c(main) */

#include "compiler.h"

/* keyword list */
struct KEY key[KEYWORDSIZE] = {
        {"and", 	    TAND	},
        {"array",	    TARRAY	},
        {"begin",	    TBEGIN	},
        {"boolean",	TBOOLEAN},
        {"break",	    TBREAK  },
        {"call",	    TCALL	},
        {"char",	    TCHAR	},
        {"div",		TDIV	},
        {"do",		    TDO 	},
        {"else",	    TELSE	},
        {"end",		TEND	},
        {"false",	    TFALSE	},
        {"if",		    TIF	    },
        {"integer",	TINTEGER},
        {"not",		TNOT	},
        {"of",		    TOF	    },
        {"or",		    TOR 	},
        {"procedure",  TPROCEDURE},
        {"program",	TPROGRAM},
        {"read",	    TREAD	},
        {"readln",  	TREADLN },
        {"return", 	TRETURN },
        {"then",	    TTHEN	},
        {"true",	    TTRUE	},
        {"var",		TVAR	},
        {"while",	    TWHILE	},
        {"write",	    TWRITE  },
        {"writeln", 	TWRITELN}
};

/* Token counter */
/* int numtoken[NUMOFTOKEN+1]; */

/* constant of each token */
char *tokenstr[NUMOFTOKEN+1] = {
        "",
        "NAME", "program", "var", "array", "of", "begin", "end", "if", "then",
        "else", "procedure", "return", "call", "while", "do", "not", "or",
        "div", "and", "char", "integer", "boolean", "readln", "writeln", "true",
        "false", "NUMBER", "STRING", "+", "-", "*", "=", "<>", "<", "<=", ">",
        ">=", "(", ")", "[", "]", ":=", ".", ",", ":", ";", "read","write", "break"
};

int main(int nc, char *np[]) {
    if(nc < 2) {
        printf("File name id not given.\n");
        return 0;
    }
    if(init_scan(np[1]) < 0) {
        printf("File %s can not open.\n", np[1]);
        return 0;
    }

    IS_ERROR(init_write(np[1]))

    /* scan the first token */
    token = scan();

    /* LL1-parsing & pretty-printer */
    if(parse_program() == ERROR) {
        printf("Number of line : %d\n", get_linenum());
    }
    else {
        end_scan();
        printf("Writing the csl file was finished successfully.\n");

    }

    end_write();
    release_alltab();

    return 0;
}

void scan_error(char *mes) {
    printf("\nERROR: %s\n", mes);
    /*end_scan();*/
}

int parse_error(char *mes) {
    printf("\nERROR: %s\n", mes);
    end_scan();
    return(ERROR);
}

int parse_token_error(int code) {
    printf("\nERROR: Keyword '%s' is not found.\n", tokenstr[code]);
    end_scan();
    return(ERROR);
}