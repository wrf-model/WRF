%{
/* 
 * Copyright (C) 2000-2006 Erik Edelmann <Erik.Edelmann@iki.fi>
 *
 *     This program is free software;  you  can  redistribute  it
 *     and/or modify it under the terms of the GNU General Public
 *     License version 2 as published  by  the  Free  Software  
 *     Foundation.
 *
 *     This program is distributed in the hope that  it  will  be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS  FOR  A  PARTICULAR
 *     PURPOSE.   See  the  GNU  General  Public License for more
 *     details.
 *
 *     You should have received a copy of the GNU General  Public
 *     License along with this program; if not, write to the Free
 *     Software Foundation, Inc., 59  Temple  Place,  Suite  330,
 *     Boston, MA  02111-1307  USA
 */

#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_ALLOCA_H
#  include <alloca.h>
#endif

#include "finddep.h"
#include "utils.h"
#include "errormesg.h"
#include "global.h"
#include "macro.h"
#include "modfile_name.h"

static char *sourcefile;
static char *curr_file;
static Dependency *dep;  /* Dependencies of the file */
static List *modules;    /* Modules defined in the file */
static List *macrolist;
static char *filestack[INCLUDE_RECURSION_LIMIT+1];
static int filestack_i;
static int pp_ignore;    /* In 'false'-branch of a pre-processor 'if' */
static bool skip_to_end[20];
static int skip_i;
static bool in_interface = false;

int yyerror(const char *s);

static int modcmp(const void *m1, const void *m2);
SourceFmt get_format(const char *filename);
void handle_include(const char *incfile);

Macro *defmac;

/* Defined in lexer.l */
int yylex();
bool lex_include_file(const char *incfile);
void lex_set_format(SourceFmt fmt);

%}

%union {
    char *string;
    int number;
}

%token EOSTMT ASSIGNMENT_OP GARBAGE
%token CPP_INCLUDE F90PPR_INCLUDE COCO_INCLUDE 
%token F90PPR_DEFINE CPP_DEFINE F90PPR_UNDEF CPP_UNDEF 
%token CPP_IFDEF CPP_IFNDEF CPP_IF CPP_ELSE CPP_ELIF CPP_ENDIF
%token F90PPR_IFDEF F90PPR_IFNDEF F90PPR_IF F90PPR_ELSE F90PPR_ELIF F90PPR_ENDIF
%token <string> CPP_TOENDL
%token <number> UNTERMINATED_STRING
%token <string> STRING WORD 

%%

code: /* empty */
    | code stmt
    ;

stmt: keyword_stmt
    | assignment_stmt
    ;

assignment_stmt: WORD ASSIGNMENT_OP other EOSTMT    /* Ignore */

keyword_stmt: WORD EOSTMT {
        if (strcasecmp($1, "interface") == 0)
            in_interface = true;
        free($1);
    }
    | WORD WORD other EOSTMT { 
        if (strcasecmp($1, "use") == 0) {
            if (!pp_ignore) {
                DEBUG_PRINT("Use '%s'\n", $2);
                if (!list_find(options.ignore_mods, $2,COMP_FUN(&strcasecmp))) {
                    if (!list_find(dep->modules, $2, COMP_FUN(&strcasecmp)))
                        dep->modules = list_prepend(dep->modules, $2);
                }
            }
        } else if (strcasecmp($1, "module") == 0) {
            if (!pp_ignore && !in_interface) {
                if (!list_find(options.ignore_mods, $2,COMP_FUN(&strcasecmp))) {
                    Module *mod;

                    mod = module_new();
                    mod->sourcefile = xstrdup(sourcefile);
                    mod->modulename = $2;
                    mod->modfile_name = modfile_name($2, mod->sourcefile);

                    if (list_find(modules, mod, &modcmp))
                        warning("Several modules named '%s'", $2);
                    else
                        modules = list_prepend(modules, mod);

                    if (!list_find(dep->targets, mod->modfile_name,
                                   COMP_FUN(&strcasecmp)))
                    dep->targets=list_prepend(dep->targets, mod->modfile_name);
                }
            }
        } else if (strcasecmp($1, "interface") == 0) {
            in_interface = true;
            free($2);
        } else if (strcasecmp($2, "interface") == 0 
                   && strcasecmp($1, "end") == 0) {
            in_interface = false;
            free($2);
        }

        free($1);
    }
    | WORD STRING other EOSTMT { 
        if (strcasecmp($1, "include") == 0) {
            handle_include($2);
        }
        free($1);
        free($2);
    }
    | include STRING other EOSTMT { 
        handle_include($2);
        free($2);
    }
    | CPP_INCLUDE GARBAGE other EOSTMT  /* Ignore #include <whatever.h> */
    | define WORD other EOSTMT{ 
        if (!pp_ignore) {
            DEBUG_PRINT("%s defined\n", $2);
            defmac = macro_new();
            macro_setname(defmac, $2);
            if (!list_find(macrolist, defmac, &macrocmp))
                macrolist = list_prepend(macrolist, defmac);
        }
    }
    | undef WORD other EOSTMT {
        if (!pp_ignore) {
            Macro *mac;
            List *l;
            
            mac = macro_new();
            macro_setname(mac, $2);

            l = list_find(macrolist, mac, &macrocmp);
            if (l) {
                macrolist = list_remove(macrolist, l);
                macro_free(l->data);
                list_free(l);
            }
            macro_free(mac);
        }
    }
    | ifdef WORD other EOSTMT {
        Macro *mac;

        mac = macro_new();
        macro_setname(mac, $2);

        skip_i++;
        if (pp_ignore)
            pp_ignore++;
        else if (!list_find(macrolist, mac, &macrocmp))
            pp_ignore = 1;
        else
            skip_to_end[skip_i] = true;

        macro_free(mac);
    }
    | ifndef WORD other EOSTMT {
        Macro *mac;

        mac = macro_new();
        macro_setname(mac, $2);

        skip_i++;
        if (pp_ignore)
            pp_ignore++;
        if (list_find(macrolist, mac, &macrocmp))
            pp_ignore = 1;
        else
            skip_to_end[skip_i] = true;

        macro_free(mac);
    }
    | if other EOSTMT { 
        /* #if:s can't be completely ignored, since #else:s, #elif:s and
         * #endif:s aren't.  An #if branch is allways taken, and so are any
         * following #else or #elif:s (ie. no 'skip_to_end'). */
        skip_i++;
        if (pp_ignore)  pp_ignore++;
        skip_to_end[skip_i] = false;
    }
    | elif other EOSTMT {
        /* Allways taken unless an #ifdef or #ifndef-branch has been taken
         * allready. */
        if (skip_to_end[skip_i] && pp_ignore == 0)  pp_ignore = 1;
    }
    | else other EOSTMT {
        if (pp_ignore == 1 && skip_to_end[skip_i] == false)  
            pp_ignore = 0;
        else if (pp_ignore == 0)
            pp_ignore = 1;
    }
    | endif other EOSTMT {
        skip_to_end[skip_i] = false;
        if (skip_i > 0)  skip_i--;
        if (pp_ignore) pp_ignore--;
    }
    | WORD GARBAGE other EOSTMT             /* Ignore */
    | GARBAGE other EOSTMT
    | EOSTMT
    | error { yyerrok; }
    ;

include: CPP_INCLUDE
    | F90PPR_INCLUDE
    | COCO_INCLUDE
    ;

define: CPP_DEFINE
    | F90PPR_DEFINE
    ;

undef: CPP_UNDEF
    | F90PPR_UNDEF
    ;

ifdef: CPP_IFDEF
    | F90PPR_IFDEF
    ;

ifndef: CPP_IFNDEF
    | F90PPR_IFNDEF
    ;

if: CPP_IF
    | F90PPR_IF
    ;

elif: CPP_ELIF
    | F90PPR_ELIF
    ;

else: CPP_ELSE
    | F90PPR_ELSE
    ;

endif: CPP_ENDIF
    | F90PPR_ENDIF
    ;

other: /* empty */
    | other misc_code               
    ;
    
misc_code: WORD                     { free ($1); }
    | STRING                        { free ($1); }
    | GARBAGE
    | ASSIGNMENT_OP
    | UNTERMINATED_STRING           { 
        if (options.warn_confused) 
            warning ("Unterminated string in file '%s' on line %i", 
                     curr_file, $1);
    }
    ;

%% 

int yyerror(const char *s)
{
    extern int yylineno;

    if (options.warn_confused)
        warning("Line %i in file '%s' confuses me\n", yylineno, curr_file);

    return 0;
}


/* Return false for failure reading file, else return true.  */

bool find_dep(char *file, Dependency *d, List **mods, const List *predef_macro)
{
    extern FILE *yyin;
    extern int yylineno;
    Macro *mac;
    const List *h;

    /* Initialize */
    sourcefile = file;
    curr_file = file;
    dep = d;
    modules = *mods;
    yylineno = 1;
    filestack_i = 0;
    pp_ignore = 0;

    yyin = open_src_file(file, options.src_path);
    if (yyin == NULL) {
        warning("Skipping file '%s': %s", file, strerror(errno));
        return false;
    }

    /* Check source format */
    if (options.src_fmt == SUFFIX)
        lex_set_format(get_format(file));
    else
        lex_set_format(options.src_fmt);

    /* Initialize a list of macros, and fill it with macrodefinitions from -D
     * flags at the command line */
    macrolist = NULL;
    for (h = predef_macro; h; h = h->next) {
        mac = macro_new();
        macro_copy(mac, (Macro *)h->data);
        if (!list_find(macrolist, mac, &macrocmp))
            macrolist = list_prepend(macrolist, mac);
    }

    yyparse();
    *mods = modules;

    /* Delete macrolist */
    for (h = macrolist; h; h = h->next)
        macro_free((Macro *)h->data);
    list_free(macrolist);

    fclose(yyin);

    return true;
}
            

int modstrcmp(const void *s, const void *m)
{
    return strcasecmp(((Module *)m)->modulename, (char *)s);
}



static int modcmp (const void *m1, const void *m2)
{
    return strcasecmp(((Module *)m1)->modulename, ((Module *)m2)->modulename);
}



static char *fixed_suffixes[] = {".f", ".F", ".for", ".FOR", ".ftn", ".FTN"};

static char *free_suffixes[]  = {".f90", ".F90", ".f95", ".F95"};
                                  
static const int fixsuffn = sizeof(fixed_suffixes)/sizeof(void *);
static const int freesuffn = sizeof(free_suffixes)/sizeof(void *);


SourceFmt get_format(const char *filename)
{
    const char *p;
    int i;

    /* Search for the end */
    for (p = filename; *p; p++);

    /* Search backwards for the last '.' */
    for (; *p != '.' && p != filename; p--);

    /* Check for any of the free suffixes */
    for (i = 0; i < freesuffn; i++) 
        if (strcmp(p, free_suffixes[i]) == 0)  return FREE;

    /* Check for any of the fixed suffixes */
    for (i = 0; i < fixsuffn; i++)
        if (strcmp(p, fixed_suffixes[i]) == 0)  return FIXED;

    return UNKNOWN;
}


void pop_filestack()
{
    curr_file = filestack[--filestack_i];
}


void handle_include(const char *incfile)
{
    if (!pp_ignore) {
        filestack[filestack_i++] = curr_file;
        curr_file = remove_citation(incfile);
        if (lex_include_file(curr_file))  {
            DEBUG_PRINT("including file '%s'\n", curr_file);
            if (!list_find(dep->includes, curr_file, COMP_FUN(&strcasecmp)))
                dep->includes = list_prepend(dep->includes, curr_file);
        } else {
            pop_filestack();
        }
    }
}


Module *module_new()
{
    Module *m;

    m = (Module *) xmalloc(sizeof(Module));
    m->modulename = m->modfile_name = m->sourcefile = NULL;
    return m;
}


Dependency *dependency_new()
{
    Dependency *d;

    d = (Dependency *) xmalloc(sizeof(Dependency));
    d->sourcefile = NULL;
    d->targets = d->modules = d->includes = NULL;
    return d;
}
