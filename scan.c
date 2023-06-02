/* scan.c */

#include "compiler.h"

/* global variables */
FILE                     *fp;
int                     cbuf; /* look-ahead character buffer */
int                 num_attr; /* constant attribute */
char string_attr[MAXSTRSIZE]; /* constant attribute */

int                 line_num; /* line number where the token returned by scan() most recently */
static int        line_count; /* line number of the entire program */
static int    line_code_flag; /* flag = 1 / 2 : previous character is line code(1 = '\r' / 2 = '\n') */

/* static function */
static int isKeywords(char *str);
static int isSymbols(int word);
static int isGraphicChar(int word);
static void check_linenum(int word);
/* read the next character and check it is not unauthorized control code or EOF. */
static int nextChar(void);


/*-----function called from the main program------*/

/* initialize */
int init_scan(char *filename) {
    line_num       = 0;
    line_count     = 0;
    line_code_flag = 0;

    if((fp = fopen(filename, "r")) == NULL) {
        return -1;
    }

    return 0;
}

/* scan one token */
int scan(void) {
    int string_num;

    /* at the first time scan() called */
    if(!line_count) {
        line_count++;

        /* read the first character from a file */
        nextChar();
    }

    /* main loop */
    while (1) {
        /* value reset */
        string_num = 0;
        memset(string_attr, '\0', sizeof(string_attr));

        /* determine number of program line at the moment the first character of token read */
        line_num = line_count;

        /* end the loop(return -1) if cbuf is unauthorized control code or EOF.*/
        switch (isGraphicChar(cbuf)) {
            case EOF:
                /* printf("catch EOF.\n"); */
                return -1;
            case NOTGRAPHICCHAR:
                scan_error(NOTGRAPHICMESSAGE);
                return -1;
        }

        /* separator */
        if(cbuf == '\t' || cbuf == '\r' || cbuf == '\n' || cbuf == ' ' ) {
            /* skip */
            nextChar();
        }

            /* comment */
        else if(cbuf == '{') {
            do {
                switch (nextChar()) {
                    case EOF:
                        scan_error("unterminated { comment.");
                        return -1;
                    case NOTGRAPHICCHAR:
                        scan_error(NOTGRAPHICMESSAGE);
                        return -1;
                }
            } while (cbuf != '}');

            nextChar();
        }
        else if(cbuf == '/') {
            switch (nextChar()) {
                case EOF:
                    scan_error("symbol '/' does not exist.");
                    return -1;
                case NOTGRAPHICCHAR:
                    scan_error(NOTGRAPHICMESSAGE);
                    return -1;
            }

            if(cbuf == '*') {

                while (1) {
                    switch (nextChar()) {
                        case EOF:
                            scan_error("unterminated /* comment.");
                            return -1;
                        case NOTGRAPHICCHAR:
                            scan_error(NOTGRAPHICMESSAGE);
                            return -1;
                    }

                    if(cbuf == '*') {
                        switch (nextChar()) {
                            case EOF:
                                scan_error("unterminated /* comment.");
                                return -1;
                            case NOTGRAPHICCHAR:
                                scan_error(NOTGRAPHICMESSAGE);
                                return -1;
                        }

                        if(cbuf == '/') {
                            break;
                        }
                    }
                }

                nextChar();
            }
            else {
                scan_error("symbol '/' does not exist.");
                return -1;
            }
        }

            /* name or keyword */
        else if((cbuf >= 'A' && cbuf <= 'Z')
                || (cbuf >= 'a' && cbuf <= 'z')) {
            /* continue reading while cbuf is alphanumeric character. */
            while (1) {
                if((cbuf >= 'A' && cbuf <= 'Z')
                   || (cbuf >= 'a' && cbuf <= 'z')
                   || (cbuf >= '0' && cbuf <= '9')) {

                    if(string_num >= MAXSTRSIZE) {
                        scan_error(OVERSTRSIZEMESSAGE);
                        return -1;
                    }
                    else {
                        string_attr[string_num++] = (char)cbuf;
                    }

                    if(nextChar() == NOTGRAPHICCHAR) {
                        scan_error(NOTGRAPHICMESSAGE);
                        return -1;
                    }
                }
                else {
                    break;
                }
            }

            /* return the token code of NAME or keywords */
            return isKeywords(string_attr);
        }
            /* unsigned integer */
        else if((cbuf >= '0' && cbuf <= '9')) {
            /* continue reading while cbuf is number. */
            while (1) {
                if((cbuf >= '0' && cbuf <= '9')) {

                    if(string_num >= MAXSTRSIZE) {
                        scan_error(OVERSTRSIZEMESSAGE);
                        return -1;
                    }
                    else {
                        string_attr[string_num++] = (char)cbuf;
                    }

                    if(nextChar() == NOTGRAPHICCHAR) {
                        scan_error(NOTGRAPHICMESSAGE);
                        return -1;
                    }
                }
                else {
                    break;
                }
            }

            char *e = NULL;
            long r = strtol(string_attr, &e, 10);

            if(*e != '\0') {
                /* this statement will not be executed for normal isInputState */
                scan_error("token is not an integer.");
                return -1;
            }

            if(r > INT16_MAX || r < 0) {
                scan_error("out of range of unsigned integers.");
                return -1;
            }
            num_attr = (int)r;
            return TNUMBER;
        }
            /* constant */
        else if(cbuf == '\'') {
            while (1) {
                switch (nextChar()) {
                    case EOF:
                        scan_error("unterminated '' constant.");
                        return -1;
                    case NOTGRAPHICCHAR:
                        scan_error(NOTGRAPHICMESSAGE);
                        return -1;
                }

                if(cbuf == '\'') {

                    if(nextChar() == NOTGRAPHICCHAR) {
                        scan_error(NOTGRAPHICMESSAGE);
                        return -1;
                    }

                    if(cbuf == '\'') {

                        if(string_num >= MAXSTRSIZE) {
                            scan_error(OVERSTRSIZEMESSAGE);
                            return -1;
                        }
                        else {
                            string_attr[string_num++] = '\'';
                            string_attr[string_num++] = '\'';
                        }
                    }
                    else {
                        break;
                    }
                }
                else if(cbuf == '\r' || cbuf == '\n') {
                    /* do not include line codes in the constant */
                }
                else {
                    if(string_num >= MAXSTRSIZE) {
                        scan_error(OVERSTRSIZEMESSAGE);
                        return -1;
                    }
                    else {
                        string_attr[string_num++] = (char)cbuf;
                    }
                }
            }
            return TSTRING;
        }
            /* symbol */
        else {
            int temp_token = isSymbols(cbuf);

            /* return -1 if scanning of all tokens fails */
            /* do not read the next character if the next character is not part of the symbol */
            switch (temp_token) {
                case 0:
                    scan_error("not token or separator.");
                    return -1;
                case NOTGRAPHICCHAR:
                    scan_error(NOTGRAPHICMESSAGE);
                    return -1;
                case TLE:
                case TGR:
                case TCOLON:
                    break;
                default:
                    nextChar();
                    break;
            }
            return temp_token;
        }
    }
}

int get_linenum(void) {
    return line_num;
}

/* termination */
void end_scan(void) {
    fclose(fp);
}

/*-----static function-----*/

/* check if the constant buffer are keywords */
static int isKeywords(char *str) {
    static int i;

    for(i = 0; i < KEYWORDSIZE; i++) {
        if(!(strcmp(key[i].keyword, str))) {
            return key[i].keytoken;
        }
    }
    return TNAME;
}

/* check if the word is symbols */
static int isSymbols(int word) {
    switch (word) {
        case '+':
            return TPLUS;
        case '-':
            return TMINUS;
        case '*':
            return TSTAR;
        case '=':
            return TEQUAL;
        case '<':
            if(nextChar() == NOTGRAPHICCHAR) {
                return NOTGRAPHICCHAR;
            }

            switch (cbuf) {
                case '>':
                    return TNOTEQ;
                case '=':
                    return TLEEQ;
                default:
                    return TLE;
            }
        case '>':
            if(nextChar() == NOTGRAPHICCHAR) {
                return NOTGRAPHICCHAR;
            }

            if(cbuf == '=') {
                return TGREQ;
            } else {
                return TGR;
            }
        case '(':
            return TLPAREN;
        case ')':
            return TRPAREN;
        case '[':
            return TLSQPAREN;
        case ']':
            return TRSQPAREN;
        case ':':
            if(nextChar() == NOTGRAPHICCHAR) {
                return NOTGRAPHICCHAR;
            }

            if(cbuf == '=') {
                return TASSIGN;
            } else {
                return TCOLON;
            }
        case '.':
            return TDOT;
        case ',':
            return TCOMMA;
        case ';':
            return TSEMI;
        default:
            return 0;
    }
}

/* check if the word is a graphic character.*/
static int isGraphicChar(int word) {
    if(word == EOF) {
        return EOF;
    }
    else if(word != '\t' && word != '\r' && word != '\n' && (word < ' ' || word > '~')) {
        return NOTGRAPHICCHAR;
    }
    else {
        return 0;
    }
}

/* check if the word is a line code.*/
static void check_linenum(int word) {
    if(word == '\r') {

        if(line_code_flag == 2) { /* if previous char is '\n' */
            line_code_flag = 0;   /* flag reset and do not count number of line. */
        }
        else {
            line_code_flag = 1;
            line_count++;
        }
    }
    else if (word == '\n') {

        if(line_code_flag == 1) { /* if previous char is '\r' */
            line_code_flag = 0;
        }
        else {
            line_code_flag = 2;
            line_count++;
        }
    }
    else {
        line_code_flag = 0;
    }
}

/* read the next character and check whether it is unauthorized control code or EOF. */
static int nextChar(void) {
    cbuf = fgetc(fp);
    check_linenum(cbuf);
    return isGraphicChar(cbuf);
}