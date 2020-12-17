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


#include <string.h>
#include <errno.h>
#include <assert.h>
#include "global.h"
#include "errormesg.h"
#include "find_dep.h"
#include "utils.h"
#include "xmalloc.h"

void yy_push_state(int new_state);
void yy_pop_state();
void pop_filestack();


static YY_BUFFER_STATE include_stack[INCLUDE_RECURSION_LIMIT];
static int line_num_stack[INCLUDE_RECURSION_LIMIT];
static int incl_buff = 0;

static char string_buf[MAX_STRING_LEN];
static char *string_buf_ptr;

static int old_startcond;

static int last_returned;

#define RETURN(n) last_returned = n; return n;


%}

%option yylineno

%s free_fmt fixed_fmt
%x str_sq str_dq

%%


\"              { string_buf_ptr = string_buf; old_startcond = YY_START; 
                  BEGIN(str_dq); }
'               { string_buf_ptr = string_buf; old_startcond = YY_START; 
                  BEGIN(str_sq); }

<str_dq>\" |
<str_sq>'       { 
                    BEGIN(old_startcond);
                    *string_buf_ptr = '\0';
                    yylval.string = xstrdup(string_buf);
                    DEBUG_PRINT("string: '%s'\n", yylval.string);
                    RETURN(STRING);
                }

<str_dq,str_sq>&[ \t]*\n |
<str_dq,str_sq>&[ \t]*\n[ \t]*&  /* Ignore (continued strings, free fmt) */

<str_dq,str_sq>\n[ ]{5}[^ \t\n] {
                    if (old_startcond == fixed_fmt) 
                        ; /* Ignore (cont. strings, fixed fmt) */
                    else
                        unput(yytext[strlen(yytext)-1]);
                }

<str_dq,str_sq>\n { 
                    unput ('\n');
                    BEGIN(old_startcond);
                    yylval.number = yylineno;
                    RETURN(UNTERMINATED_STRING);
                }

<str_sq,str_dq>. { *string_buf_ptr++ = yytext[0]; }




!.*             { RETURN(EOSTMT); } /* Treat comments like */
<fixed_fmt>^[cC*dD].*\n { RETURN(EOSTMT); } /* empty lines */

#[ \t]*include  { RETURN(CPP_INCLUDE); }
\$[ \t]*include { RETURN(F90PPR_INCLUDE); }
\?\?[ \t]*include { RETURN(COCO_INCLUDE); }

#[ \t]*define   { RETURN(CPP_DEFINE); }
\$[ \t]*DEFINE   { RETURN(F90PPR_DEFINE); }

#[ \t]*undef    { RETURN(CPP_UNDEF); }
\$[ \t]*UNDEF   { RETURN(F90PPR_UNDEF); }

#[ \t]*ifdef    { RETURN(CPP_IFDEF); }
#[ \t]*ifndef   { RETURN(CPP_IFNDEF); }
#[ \t]*if[ \t].*      { RETURN(CPP_IF); }
#[ \t]*elif[ \t].*     { RETURN(CPP_ELIF); }
#[ \t]*else     { RETURN(CPP_ELSE); }
#[ \t]*endif    { RETURN(CPP_ENDIF); }

$[ \t]*ifdef    { RETURN(F90PPR_IFDEF); }
$[ \t]*ifndef   { RETURN(F90PPR_IFNDEF); }
$[ \t]*if       { RETURN(F90PPR_IF); }
$[ \t]*elif     { RETURN(F90PPR_ELIF); }
$[ \t]*else     { RETURN(F90PPR_ELSE); }
$[ \t]*endif    { RETURN(F90PPR_ENDIF); }

 /* Line continuations, possible involving comments.  */
&([ \t\n]*|!.*)*
&([ \t\n]*|!.*)*& 

<fixed_fmt>\n[ ]{5}[^ ]  { RETURN(GARBAGE); }

=|=>             { RETURN(ASSIGNMENT_OP); }

[a-zA-Z_][a-zA-Z_0-9]* { ;yylval.string = xstrdup(yytext); RETURN(WORD); }

[^ \t\n\r;,!'"a-zA-Z=&]+ { RETURN(GARBAGE); }

;|\n             { RETURN(EOSTMT); }

[ \t\r,]         /* Ignore */
\\[ \t]*\n       /* Ignore line-endings preceeded by \ */

.               { RETURN(*yytext); }


<<EOF>>         { 
                    DEBUG_PRINT("EOF reached %i\n", incl_buff);
                    incl_buff--;
                    if (incl_buff < 0) {
                        if (last_returned == EOSTMT) {
                            incl_buff = 0;
                            yyterminate();
                        } else {
                            /* "EOF without \n first" cases. */
                            RETURN(EOSTMT);
                        }
                    } else {
			fclose(yyin);
                        yy_delete_buffer(YY_CURRENT_BUFFER);
                        yy_switch_to_buffer (include_stack[incl_buff]);
                        yylineno = line_num_stack[incl_buff];
                        pop_filestack();
                    }
                }

%%


/* "Include" file 'incfile' here. Return false for failure, true for success. */

bool lex_include_file(const char *incfile)
{
    FILE *bakup;

    DEBUG_PRINT("'%s'\n", incfile);

    if (incl_buff >= INCLUDE_RECURSION_LIMIT) {
        warning("Recursion limit reached in file '%s'", incfile);
        return false;
    }

    bakup = yyin;
    yyin = open_src_file(incfile, options.src_path);
    if (yyin == NULL) {
        if (errno == ENOENT) {
            if (options.warn_missing)
                warning("Include file '%s' not found", incfile);
        } else
            warning("Skipping include file '%s': %s", incfile, strerror(errno));

        yyin = bakup;
        return false;
    }

    include_stack[incl_buff] = YY_CURRENT_BUFFER;
    yy_switch_to_buffer(yy_create_buffer (yyin, YY_BUF_SIZE));

    line_num_stack[incl_buff++] = yylineno;
    yylineno = 1;

    return true;
}


void lex_set_format(SourceFmt fmt)
{
    switch (fmt) {
        case UNKNOWN:
        case FREE: 
            BEGIN(free_fmt); break;
        case FIXED: 
            BEGIN(fixed_fmt); break;
        default: 
            assert(0);
    }
}


int yywrap()
{
    return 1;
}
