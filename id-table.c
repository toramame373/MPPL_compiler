/* id-table.c */

#include "compiler.h"

struct TYPE *temp_paralist;                    /* temporary list of type to use for registering formal parameters and collation of actual arguments */
struct ID *global_idroot, *local_idroot;       /* Pointers to root of global & local symbol tables */
struct VARNAME *temp_varname, *temp_passlist;  /* List of temporary variable names (:names of the same type) */

/* ----------------------------------------------------- */
/* static function */

/* init & release */
static void init_global_idtab();
static void  init_local_idtab();
static void   init_varnametab();
static void     init_paralist();
static void     init_passlist();

static void release_formalPara_typetab(struct TYPE *p);
static void release_global_idtab();
static void  release_local_idtab();
static void   release_varnametab();
static void     release_paralist();

/* create the member "idname" in the table specified by 'scope' */
static int create_id(int scope, int type, char *idname);

/* ---------------------------------------------------------- */
/* extern function */

void init_alltab() {
    init_global_idtab();
    init_local_idtab();
    init_varnametab();
    init_paralist();
    init_passlist();
}

void release_alltab() {
    release_global_idtab();
    release_local_idtab();
    release_varnametab();
    release_paralist();
    release_passlist();
}

void release_passlist() {
    struct VARNAME *p, *q;

    for(p = temp_passlist; p != NULL; p = q) {
        free(p->name);
        q = p->nextvnp;
        free(p);
    }
    init_passlist();
}
/* ---------------------------------------------------------- */

/* search the name pointed by "idname" from the table specified by 'scope' */
struct ID *search_idtab(int scope, char *idname) {
    struct ID *p;

    if(scope == GLOBAL) {
        p = global_idroot;
    }
    else {
        p = local_idroot;
    }

    for(; p != NULL; p = p->nextp) {
        if(strcmp(idname, p->name) == 0) {
            if(scope == GLOBAL) {
                /* for global variables, It's assumed that ID is found if only name matches. */
                return(p);
            }
            else {
                /* for local variables, It's assumed that ID is found if the procedure name also matches. */
                if(strcmp(declaring_proc_name, p->proc_name) == 0) {
                    return (p);
                }
            }
        }
    }
    return(NULL);
}
/* ---------------------------------------------------------- */


int store_passlist() {
    struct VARNAME *p;

    for(p = temp_varname; p != NULL; p = p->nextvnp) {
        struct VARNAME *q;
        char *cp;

        IS_STRING_MALLOC_ERROR(cp, p->name)
        strcpy(cp, p->name);

        IS_STRUCT_MALLOC_ERROR(q, VARNAME)
        q->name       = cp;
        q->nextvnp    = temp_passlist; /* connect new member to first */
        temp_passlist = q;
    }

    return NORMAL;
}

/* make a temporary list of variable names(:names of the same type) */
int store_varname(char *var_name) {
    struct VARNAME *p, *q;
    char *cp;

    IS_STRING_MALLOC_ERROR(cp, var_name)
    strcpy(cp, var_name);

    IS_STRUCT_MALLOC_ERROR(p, VARNAME)
    p->name      = cp;
    p->nextvnp   = NULL;

    if ((q = temp_varname) == NULL) {
        temp_varname = p; /* put the first data in the list */
        return NORMAL;
    }

    while (q->nextvnp != NULL) {
        q = q->nextvnp;
    }
    q->nextvnp = p; /* connect new member to last */

    return NORMAL;
}

/* make a temporary list of 'type'(integer, integer, ...) */
int store_paralist(int type) {
    struct TYPE *p, *tp;

    IS_STRUCT_MALLOC_ERROR(tp, TYPE)
    tp->t_type = type;
    tp->paratp = NULL;

    if ((p = temp_paralist) == NULL) {
        temp_paralist = tp; /* put the first data in the list */
        return NORMAL;
    }

    while (p->paratp != NULL) {
        p = p->paratp;
    }
    p->paratp = tp; /* connect new member to last */

    return NORMAL;
}

/* make a temporary list of 'type' to use for registering formal parameters */
int store_formalParalist(int type) {
    struct VARNAME *q;

    /* add as many types to the list as elements of the list made by store_varname() */
    for(q = temp_varname; q != NULL; q = q->nextvnp) {
        IS_ERROR(store_paralist(type))
    }

    return NORMAL;
}
/* ---------------------------------------------------------- */

/* check duplicate definition and register 'idname' in the table specified by 'scope' */
int add_idtab(int scope, int type, char *idname) {

    /* check for the same name in the same scope */
    /* for local variables, It's assumed that ID is found if the procedure name also matches. */
    if(search_idtab(scope, idname) != NULL) {
        return parse_error("Cannot declare the same name with the same scope.");
    }

    IS_ERROR(create_id(scope, type, idname))

    return NORMAL;
}

/* register variables in the temporary list of variable names */
int add_variables_idtab(int scope, int type) {
    struct VARNAME *p;

    for(p = temp_varname; p != NULL; p = p->nextvnp) {
        IS_ERROR(add_idtab(scope, type, p->name))

        IS_ERROR(write_var_declaration_byID(variable_id))
    }

    /* make a list for passing by reference  */
    store_passlist();

    /* reset the temporary name list */
    release_varnametab();
    return NORMAL;
}

/* register formal parameter types in the procedure 'proc_name' */
int add_formalPara_typetab(char *proc_name) {
    struct ID *p;
    struct TYPE *q, *tp;

    if((p = search_idtab(GLOBAL, proc_name)) != NULL && p->itp->t_type == TPPROC) {
        q = p->itp;

        for (tp = temp_paralist; tp != NULL; tp = tp->paratp) {
            struct TYPE *ptp;
            IS_STRUCT_MALLOC_ERROR(ptp, TYPE)
            ptp->t_type = tp->t_type;
            ptp->paratp = NULL;

            q->paratp = ptp;
            q = q->paratp;
        }
    }

    /* reset the temporary type list */
    release_paralist();
    return NORMAL;
}
/* ---------------------------------------------------------- */

/* check if 'proc_name' is declared */
int check_proc_id(struct ID *proc_id) {
    struct ID *q;

    if(proc_id->itp->t_type == TPPROC) {

        /* check recursive calls */
        if(strcmp(proc_id->name, declaring_proc_name) == 0) {
            return parse_error("The procedure was called recursively.");
        }

        /* check local variable wtih the same name with 'proc_name' */
        if((q = search_idtab(LOCAL, proc_id->name)) != NULL &&
           strcmp(q->name, proc_id->name) == 0 &&
           strcmp(q->proc_name, declaring_proc_name) == 0) {
            return parse_error("A local variable was called.");
        }

        return NORMAL;
    }

    return parse_error("An undeclared procedure was called.");
}

/* return the pointer of referenced variable ID */
struct ID *get_var_id(int scope, char *var_name) {
    struct ID *p;

    if((p = search_idtab(scope, var_name)) != NULL) {
        return p;
    }
    else {
        if(scope == LOCAL) {

            /* look in the global table if ID is not found in the local table */
            if((p = search_idtab(GLOBAL, var_name)) != NULL) {
                return p;
            }
        }

        return (NULL);
    }
}

/* return the type of variable */
int get_vartype(struct ID *var_id) {

    if(var_id != NULL) {
        return var_id->itp->t_type;
    }

    return parse_error("An undefined variable was referenced.");
}

/* ---------------------------------------------------------- */

/* collate the list of formal parameters and actual arguments */
int verify_paralist(struct ID *proc_id) {
    struct TYPE *p, *q;
    q = proc_id->itp->paratp;

    for(p = temp_paralist; p != NULL; p = p->paratp) {
        if(q == NULL) {
            break;
        }

        if(p->t_type != q->t_type) {
            return parse_error("Expression and formal argument types are different.");
        }
        q = q->paratp;
    }

    /* the number of arguments matches if both lists are at the end */
    if(q == NULL && p == NULL) {
        release_paralist();
        return NORMAL;
    }
    else {
        return parse_error("Different number of expressions and formal parameters.");
    }
}

/* ---------------------------------------------------------- */

/* static function */

/* Initialise the table */
static void init_global_idtab() {
    global_idroot = NULL;
}

static void init_local_idtab() {
    local_idroot = NULL;
}

static void init_varnametab() {
    temp_varname = NULL;
}

static void init_paralist() {
    temp_paralist = NULL;
}

static void init_passlist() {
    temp_passlist = NULL;
}

/* Release tha data structure */
static void release_formalPara_typetab(struct TYPE *p) {
    struct TYPE *q;

    for(; p != NULL; p = q) {
        q = p->paratp;
        free(p);
    }
}

static void release_global_idtab() {
    struct ID *p, *q;

    for(p = global_idroot; p != NULL; p = q) {
        free(p->name);
        free(p->proc_name);
        free(p->itp->etp);
        release_formalPara_typetab(p->itp->paratp);
        free(p->itp);
        free(p->irefp);
        q = p->nextp;
        free(p);
    }
    init_global_idtab();
}

static void release_local_idtab() {
    struct ID *p, *q;

    for(p = local_idroot; p != NULL; p = q) {
        free(p->name);
        free(p->proc_name);
        free(p->itp->etp);
        release_formalPara_typetab(p->itp->paratp);
        free(p->itp);
        free(p->irefp);
        q = p->nextp;
        free(p);
    }
    init_local_idtab();
}

static void release_varnametab() {
    struct VARNAME *p, *q;

    for(p = temp_varname; p != NULL; p = q) {
        free(p->name);
        q = p->nextvnp;
        free(p);
    }
    init_varnametab();
}

static void release_paralist() {
    struct TYPE *p, *q;

    for(p = temp_paralist; p != NULL; p = q) {
        q = p->paratp;
        free(p);
    }
    init_paralist();
}

/* ---------------------------------------------------------- */

/* create the member "idname" in the table specified by 'scope' */
static int create_id(int scope, int type, char *idname) {
    struct ID   *q, *p;
    struct TYPE *tp, *atp;
    struct LINE *refp = NULL;
    char *cp, *pcp;

    IS_STRING_MALLOC_ERROR(cp, idname)
    strcpy(cp, idname);

    /* set common parameters */
    IS_STRUCT_MALLOC_ERROR(q, ID)
    IS_STRUCT_MALLOC_ERROR(tp, TYPE)
    q->name        = cp;
    /* q->def_linenum = get_linenum(); */
    q->ispara      = isParameter;
    q->itp         = tp;
    q->irefp       = refp;
    tp->t_type     = type;
    tp->paratp     = NULL;

    variable_id    = q;

    if(type == TPARRAYINT || type == TPARRAYBOOL || type == TPARRAYCHAR) {
        tp->t_type = TPARRAY;

        /* set array parameters */
        IS_STRUCT_MALLOC_ERROR(atp, TYPE)
        switch (type) {
            case TPARRAYINT:
                atp->t_type = TPINT;
                break;
            case TPARRAYBOOL:
                atp->t_type = TPBOOL;
                break;
            default:
                atp->t_type = TPCHAR;
                break;
        }
        tp->array_size = num_attr;
        tp->etp        = atp;
    }
    else {
        tp->etp = NULL;
    }

    if(scope == GLOBAL) {
        q->proc_name  = NULL;

        if((p = global_idroot) == NULL) {
            global_idroot = q; /* add global symbol tables */
            return NORMAL;
        }
    }
    else {
        IS_STRING_MALLOC_ERROR(pcp, declaring_proc_name)
        strcpy(pcp, declaring_proc_name);
        q->proc_name = pcp;

        if((p = local_idroot) == NULL) {
            local_idroot = q; /* add local symbol tables */
            return NORMAL;
        }
    }

    while (p->nextp != NULL) {
        p = p->nextp;
    }
    p->nextp = q; /* connect new member to last */

    return NORMAL;
}
/* ---------------------------------------------------------- */