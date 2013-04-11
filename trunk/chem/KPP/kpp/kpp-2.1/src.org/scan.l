/******************************************************************************

  KPP - The Kinetic PreProcessor
        Builds simulation code for chemical kinetic systems

  Copyright (C) 1995-1996 Valeriu Damian and Adrian Sandu
  Copyright (C) 1997-2005 Adrian Sandu

  KPP is free software; you can redistribute it and/or modify it under the
  terms of the GNU General Public License as published by the Free Software
  Foundation (http://www.gnu.org/copyleft/gpl.html); either version 2 of the
  License, or (at your option) any later version.

  KPP is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along
  with this program; if not, consult http://www.gnu.org/copyleft/gpl.html or
  write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA  02111-1307,  USA.

  Adrian Sandu
  Computer Science Department
  Virginia Polytechnic Institute and State University
  Blacksburg, VA 24060
  E-mail: sandu@cs.vt.edu

******************************************************************************/



%s CMD_STATE INC_STATE MOD_STATE INT_STATE 
%s PRM_STATE DSP_STATE SSP_STATE INI_STATE EQN_STATE EQNTAG_STATE 
%s RATE_STATE LMP_STATE CR_IGNORE SC_IGNORE ATM_STATE LKT_STATE INL_STATE 
%s MNI_STATE TPT_STATE USE_STATE
%s COMMENT COMMENT2 EQN_ID
%x INL_CODE

%{
  #include "gdata.h" 
  #include "scan.h" 
  #include "y.tab.h"

  void Include ( char * filename );
  int EndInclude();

  int crt_line_no = 1;
  char *crt_filename;

  #define MAX_INCLUDE 10
  
  YY_BUFFER_STATE yy_buffers[ MAX_INCLUDE ];
  int yy_line_no[ MAX_INCLUDE ];
  char *yy_filename[ MAX_INCLUDE ];
  int yy_buf_level = 0;
  
  char crtToken[100];
  char nextToken[100];
  int crtTokType;
  int nextTokType;
  int crtLine;
  char crtFile[100];
  char crt_rate[100];

  int oldnErr = 0;

  int idx;
  int oldstate;
  extern int yyerrflag;

#define INITIAL 0
#define CMD_STATE 1
#define INC_STATE 2
#define MOD_STATE 3
#define INT_STATE 4
#define PRM_STATE 5
#define DSP_STATE 6
#define SSP_STATE 7
#define INI_STATE 8
#define EQN_STATE 9
#define EQNTAG_STATE 10
#define RATE_STATE 11
#define LMP_STATE 12
#define CR_IGNORE 13
#define SC_IGNORE 14
#define ATM_STATE 15
#define LKT_STATE 16
#define INL_STATE 17
#define MNI_STATE 18
#define TPT_STATE 19
#define USE_STATE 20
#define COMMENT 21
#define COMMENT2 22
#define EQN_ID 23
#define INL_CODE 24

  typedef struct {
                   char *name;
                   int next;
                   int cmd; 
                 } KEYWORD;

  KEYWORD keywords[] = { { "INCLUDE",    INC_STATE, 0 },
                         { "MODEL",      MOD_STATE, 0 },
                         { "INTEGRATOR", INT_STATE, 0 },
                         { "JACOBIAN",   PRM_STATE, JACOBIAN }, 
                         { "HESSIAN",    PRM_STATE, HESSIAN }, 
                         { "STOICMAT",   PRM_STATE, STOICMAT }, 
                         { "STOCHASTIC", PRM_STATE, STOCHASTIC }, 
                         { "DOUBLE",     PRM_STATE, DOUBLE }, 
                         { "REORDER",    PRM_STATE, REORDER }, 
                         { "MEX",        PRM_STATE, MEX }, 
                         { "DUMMYINDEX", PRM_STATE, DUMMYINDEX}, 
                         { "EQNTAGS",    PRM_STATE, EQNTAGS}, 
                         { "FUNCTION",   PRM_STATE, FUNCTION }, 
                         { "ATOMS",      ATM_STATE, ATOMDECL },   
                         { "CHECK",      ATM_STATE, CHECK },
                         { "CHECKALL",   INITIAL,   CHECKALL },
                         { "DEFVAR",     DSP_STATE, DEFVAR }, 
                         { "DEFRAD",     DSP_STATE, DEFRAD }, 
                         { "DEFFIX",     DSP_STATE, DEFFIX }, 
                         { "SETVAR",     SSP_STATE, SETVAR }, 
                         { "SETRAD",     SSP_STATE, SETRAD }, 
                         { "SETFIX",     SSP_STATE, SETFIX }, 
                         { "INITVALUES", INI_STATE, INITVALUES }, 
                         { "EQUATIONS",  EQN_STATE, EQUATIONS }, 
                         { "LUMP",       LMP_STATE, LUMP }, 
                         { "LOOKAT", 	 LKT_STATE, LOOKAT },
                         { "LOOKATALL",  INITIAL,   LOOKATALL },
                         { "TRANSPORT",  TPT_STATE, TRANSPORT },
                         { "TRANSPORTALL", INITIAL,   TRANSPORTALL },
                         { "INITIALIZE", PRM_STATE, INITIALIZE },
                         { "XGRID",  	 PRM_STATE, XGRID },
                         { "YGRID",  	 PRM_STATE, YGRID },
                         { "ZGRID",  	 PRM_STATE, ZGRID },
                         { "MONITOR", 	 MNI_STATE, MONITOR },
                         { "WRITE_ATM",  INITIAL,   WRITE_ATM },
                         { "WRITE_SPC",  INITIAL,   WRITE_SPC },
                         { "WRITE_MAT",  INITIAL,   WRITE_MAT },
                         { "WRITE_OPT",  INITIAL,   WRITE_OPT },
                         { "USE",        PRM_STATE, USE },
                         { "LANGUAGE",   PRM_STATE, LANGUAGE },
                         { "INLINE",     INL_STATE, INLINE },
                         { "ENDINLINE",  INITIAL,   ENDINLINE },
                         { "INTFILE",    PRM_STATE, INTFILE },
                         { "DRIVER",     PRM_STATE, DRIVER },
                         { "RUN",        PRM_STATE, RUN },
                         { "USES",       USE_STATE, USES },
                         { "SPARSEDATA", PRM_STATE, SPARSEDATA },
                         { 0, 0, 0 } 
                       };

  int CheckKeyword( char *cmd );

#define RETURN( x ) \
        if(1) { \
          if ( yyerrflag == 0) { \
            strcpy( crtToken, nextToken ); \
            crtTokType = nextTokType; \
            crtLine = crt_line_no; \
            strcpy( crtFile, crt_filename ); \
          } \
          strcpy( nextToken, yytext); \
          nextTokType = x; \
          return (x); \
        }
%}


BT    	[ \t]
SPACE 	[ \t]
CR      [\n] 
TAG     [a-zA-Z_0-9]+
STRING 	[^ \t\n{}#;]+

LIT   	[a-zA-Z_]
CIF   	[0-9]

IDSPC   {LIT}[a-zA-Z_0-9]*

NR  	{CIF}*
NRS 	[+-]?{CIF}+
REAL  	{NRS}?"."?{NR}
UREAL  	{NR}?"."?{NR}
FLOAT   {REAL}([eE]{NRS})?
UFLOAT  {UREAL}([eE]{NRS})?

%%
{SPACE}+                {
                        }  
#		        { BEGIN CMD_STATE; 
                        }
\{                      { oldstate = (yy_start - 1) / 2;
                          BEGIN COMMENT;
			}
\/\/                    { oldstate = (yy_start - 1) / 2;
                          BEGIN COMMENT2;
                        }
<COMMENT>[^\}\n]*       {
                        } 
<COMMENT>\}             { BEGIN oldstate;
                        }    
<COMMENT2>[^\n]*        {
                        } 
<COMMENT2>{CR}          { crt_line_no++; 
			  BEGIN oldstate;
                        }    
{CR}			{ crt_line_no++; 
                        }
<CMD_STATE>{STRING}     { idx = CheckKeyword( yytext );
                          if ( idx < 0 ) { 
                            BEGIN CR_IGNORE;
                            break; 
                          } 
                          BEGIN keywords[idx].next;
                          if ( keywords[idx].cmd ) {
                            crt_section = keywords[idx].cmd;
                            RETURN( keywords[idx].cmd );
                          }
                        } 
<INC_STATE>{STRING}     { Include( IncName(yytext) );
                          BEGIN CR_IGNORE;
                        } 
<MOD_STATE>{STRING}     { Include( ModelName(yytext) );
                          BEGIN CR_IGNORE;
                        } 
<INT_STATE>{STRING}     { Include( IntegName(yytext) );
                          BEGIN CR_IGNORE;
                        } 
<PRM_STATE>{STRING}     { strcpy( yylval.str, yytext );
                          BEGIN CR_IGNORE;                         
                          RETURN( PARAMETER ); 
                        }
<CR_IGNORE>{STRING}     { ScanError("Extra parameter on command line '%s'", yytext);
                        }
<ATM_STATE>{IDSPC}      { strcpy( yylval.str, yytext );
                          RETURN( ATOMID );
                        }
<ATM_STATE>;            { RETURN( yytext[0] );
                        } 
<DSP_STATE>{IDSPC}      { strcpy( yylval.str, yytext );
                          RETURN( SPCSPC );
                        }
<DSP_STATE>{NR}         { strcpy( yylval.str, yytext );
                          RETURN( SPCNR );
                        }
<DSP_STATE>[=]          { RETURN( SPCEQUAL );
                        } 
<DSP_STATE>[+]          { RETURN( SPCPLUS );
                        } 
<DSP_STATE>;            { RETURN( yytext[0] );
                        } 
<DSP_STATE>[^;#]        { ScanError("Invalid character '%c' in species definition", yytext[0] );
                        }
<SSP_STATE>{IDSPC}      { strcpy( yylval.str, yytext );
                          RETURN( SSPID );
                        }
<SSP_STATE>;            { RETURN( yytext[0] );
                        } 
<INI_STATE>{IDSPC}      { strcpy( yylval.str, yytext );
                          RETURN( INISPC ); 
                        } 
<INI_STATE>[=]          { RETURN( INIEQUAL );
                        } 
<INI_STATE>;            { RETURN( yytext[0] );
                        } 
<INI_STATE>{FLOAT}      { strcpy( yylval.str, yytext );
                          RETURN( INIVALUE ); 
                        } 
<INI_STATE>[^=;#]       { ScanError("Invalid character '%c' in initial values", yytext[0] );
                        }
<EQN_STATE>{IDSPC}      { strcpy( yylval.str, yytext );
                          RETURN( EQNSPC );
                        } 
<EQN_STATE>[=]          { RETURN( EQNEQUAL );
                        } 
<EQN_STATE>{UFLOAT}     { strcpy( yylval.str, yytext );
                          RETURN( EQNCOEF ); 
                        } 
<EQN_STATE>[:]          { BEGIN RATE_STATE;
                          *crt_rate = 0;
                          RETURN( EQNCOLON );
                        } 
<EQN_STATE>[+-]         { strcpy( yylval.str, yytext );
                          RETURN( EQNSIGN );
                        } 
<EQN_STATE>[<]          { BEGIN EQNTAG_STATE;
                          RETURN( EQNLESS );
			}
<EQNTAG_STATE>{TAG}     { strcpy( yylval.str, yytext );
                          RETURN( EQNTAG );
                        } 
<EQNTAG_STATE>[>]       { BEGIN EQN_STATE;
                          RETURN( EQNGREATER );
                        }    
<RATE_STATE>{STRING}    { strcpy( yylval.str, yytext );
                          RETURN( RATE );
                        } 
<RATE_STATE>;           { BEGIN EQN_STATE;
                          RETURN( yytext[0] );
                        } 
<LMP_STATE>{IDSPC}      { strcpy( yylval.str, yytext );
                          RETURN( LMPSPC );
                        }
<LMP_STATE>[+]          { RETURN( LMPPLUS );
                        } 
<LMP_STATE>[:]          { RETURN( LMPCOLON );
                        } 
<LMP_STATE>;            { RETURN( yytext[0] );
                        } 
<LMP_STATE>[^;#]        { ScanError("Invalid character '%c' in species definition", yytext[0] );
                        }
<LKT_STATE>{IDSPC}      { strcpy( yylval.str, yytext );
                          RETURN( LKTID );
                        }
<LKT_STATE>;            { RETURN( yytext[0] );
                        } 
<TPT_STATE>{IDSPC}      { strcpy( yylval.str, yytext );
                          RETURN( TPTID );
                        }
<TPT_STATE>;            { RETURN( yytext[0] );
                        } 
<USE_STATE>{STRING}     { strcpy( yylval.str, yytext );
                          RETURN( USEID );
                        }
<USE_STATE>;            { RETURN( yytext[0] );
                        } 
<MNI_STATE>{IDSPC}      { strcpy( yylval.str, yytext );
                          RETURN( MNIID );
                        }
<MNI_STATE>;            { RETURN( yytext[0] );
                        } 
<INL_STATE>{STRING}	{ strcpy( yylval.str, yytext );
                          BEGIN INL_CODE;
			  RETURN( INLCTX );
			}
<INL_CODE>#[^ \t\n]*    { if ( EqNoCase( yytext+1, "ENDINLINE" ) ){
                            BEGIN INITIAL;
                            RETURN( ENDINLINE );
			  }
			  else {
                            strcpy( yylval.str, yytext );
                            RETURN( INCODE );
			  }
			}
<INL_CODE>\n		{ crt_line_no++;
			  strcpy( yylval.str,yytext );
			  RETURN( INCODE );
			}
<INL_CODE>[^#\n]*	{ strcpy( yylval.str,yytext );
		          RETURN( INCODE );
			}
<<EOF>>			{ if ( ! EndInclude() ) {
                            RETURN( INITIAL );
                          }
                        }  
%%

void Include ( char * name )
{
FILE *f;
YY_BUFFER_STATE newb;

  if ( yy_buf_level == MAX_INCLUDE ) {
    printf("\nInclude nested too deep. Include %s ignored", name);
    return;
  }

  yy_buffers[ yy_buf_level ]  = YY_CURRENT_BUFFER;
  yy_line_no[ yy_buf_level ]  = crt_line_no;
  yy_filename[ yy_buf_level ] = crt_filename;
  yy_buf_level++;
  
  crt_line_no = 1;  

  crt_filename = malloc( 1 + strlen( name ) );
  strcpy( crt_filename, name ); 

 
  f = fopen( name, "r" );
  if( f == 0 )
    FatalError(3,"%s: Can't read file", name );

  newb = yy_create_buffer(f, YY_BUF_SIZE);
  yy_switch_to_buffer( newb );
}

int EndInclude()
{
YY_BUFFER_STATE oldb;
char * oldn;

  if ( yy_buf_level > 0 ) {
    oldb = YY_CURRENT_BUFFER;
    oldn = crt_filename; 
    yy_buf_level--;
    yy_switch_to_buffer( yy_buffers[yy_buf_level] );
    crt_line_no = yy_line_no[ yy_buf_level ];
    crt_filename = yy_filename[ yy_buf_level ]; 
    yy_delete_buffer( oldb ); 
    free( oldn );
    return 1;
  } 
  return 0;
}

int EqNoCase( char *s1, char *s2 )
{
  while( *s1 ) {
    if ( toupper(*s1++) != toupper(*s2++) ) return 0;
  }
  return *s1 == *s2;
}

int CheckKeyword( char *cmd )
{
int i;

  i = 0;
  while( 1 ) {
    if( keywords[i].name == 0 ) {
      ScanError( "'%s': Unknown command (ignored)", cmd);
      return -1;
    }
    if( EqNoCase( cmd, keywords[i].name ) ) {
      return i;
    }
    i++;
  }
}

