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



%{
  #include <stdio.h>
  #include <stdlib.h>
  #include <malloc.h>
  #include <string.h>
  #include <unistd.h>
  #include "scan.h"

  #define __YYSCLASS

  #define YYDEBUG 1
  extern char yytext[];
  extern FILE * yyin;
  
  int nError   = 0;
  int nWarning = 0;

  int crt_section;
  int eqState;
  int isPhoto = 0;

  char crt_term[ 30 ];
  char crt_coef[ 30 ];

  char * InlineBuf;
  int InlineLen;

  void SemicolonError();
  extern int yyerrflag;

%}

%union{
  char str[80];
};

%token JACOBIAN DOUBLE FUNCTION DEFVAR DEFRAD DEFFIX SETVAR SETRAD SETFIX 
%token HESSIAN STOICMAT STOCHASTIC
%token INITVALUES EQUATIONS LUMP INIEQUAL EQNEQUAL EQNCOLON 
%token LMPCOLON LMPPLUS SPCPLUS SPCEQUAL ATOMDECL CHECK CHECKALL REORDER
%token MEX DUMMYINDEX EQNTAGS
%token LOOKAT LOOKATALL TRANSPORT TRANSPORTALL MONITOR USES SPARSEDATA 
%token WRFCONFORM
%token WRITE_ATM WRITE_SPC WRITE_MAT WRITE_OPT INITIALIZE XGRID YGRID ZGRID
%token USE LANGUAGE INTFILE DRIVER RUN INLINE ENDINLINE
%token      PARAMETER SPCSPC INISPC INIVALUE EQNSPC EQNSIGN EQNCOEF
%type <str> PARAMETER SPCSPC INISPC INIVALUE EQNSPC EQNSIGN EQNCOEF
%token      RATE LMPSPC SPCNR ATOMID LKTID MNIID INLCTX INCODE SSPID 
%type <str> RATE LMPSPC SPCNR ATOMID LKTID MNIID INLCTX INCODE SSPID
%token      EQNLESS EQNTAG EQNGREATER
%type <str> EQNLESS EQNTAG EQNGREATER
%token      TPTID USEID
%type <str> TPTID USEID
%type <str> rate eqntag

%%

program		: section
		| section program
		;
section	        : JACOBIAN PARAMETER
		  { CmdJacobian( $2 );
                  }
                | HESSIAN PARAMETER
		  { CmdHessian( $2 );
                  }
                | STOICMAT PARAMETER
		  { CmdStoicmat( $2 );
                  }
                | DOUBLE PARAMETER
		  { CmdDouble( $2 );
                  }
                | REORDER PARAMETER
		  { CmdReorder( $2 );
                  }
                | MEX PARAMETER
		  { CmdMex( $2 );
                  }
                | DUMMYINDEX PARAMETER
		  { CmdDummyindex( $2 );
                  }
                | EQNTAGS PARAMETER
		  { CmdEqntags( $2 );
                  }
                | FUNCTION PARAMETER
		  { CmdFunction( $2 );
                  }
                | STOCHASTIC PARAMETER
		  { CmdStochastic( $2 );
                  }
                | ATOMDECL atomlist
                  {}  
                | CHECK atomlist
                  {}
                | DEFVAR species
                  {}  
                | DEFRAD species
                  {}  
                | DEFFIX species
                  {}  
                | SETVAR setspclist
                  {}  
                | SETRAD setspclist
                  {}  
                | SETFIX setspclist
                  {}  
                | INITVALUES initvalues
                  {}
                | EQUATIONS equations
                  {}
                | LUMP lumps  
                  {}
                | LOOKAT lookatlist  
                  {}
                | MONITOR monitorlist  
                  {}
                | TRANSPORT translist  
                  {}
                | CHECKALL
                  { CheckAll(); }
                | LOOKATALL
                  { LookAtAll(); }
                | TRANSPORTALL
                  { TransportAll(); }
                | WRITE_ATM
                  { WriteAtoms(); }
                | WRITE_SPC
                  { WriteSpecies(); }
                | WRITE_MAT
                  { WriteMatrices(); }
                | WRITE_OPT
                  { WriteOptions(); }
                | USE PARAMETER
		  { CmdUse( $2 ); }
                | LANGUAGE PARAMETER
		  { CmdLanguage( $2 ); }
                | INITIALIZE PARAMETER
                  { DefineInitializeNbr( $2 ); }
                | XGRID PARAMETER
                  { DefineXGrid( $2 ); }
                | YGRID PARAMETER
                  { DefineYGrid( $2 ); }
                | ZGRID PARAMETER
                  { DefineZGrid( $2 ); }
		| INLINE INLCTX inlinecode ENDINLINE
		  { 
		    AddInlineCode( $2, InlineBuf );
                    free( InlineBuf );
		  }
		| INLINE error
		  { ParserErrorMessage(); }
                | INTFILE PARAMETER
		  { CmdIntegrator( $2 ); }
                | DRIVER PARAMETER
		  { CmdDriver( $2 ); }
                | RUN PARAMETER
		  { CmdRun( $2 ); }
                | USES uselist  
                  {}
                | SPARSEDATA PARAMETER
		  { SparseData( $2 );
                  }
                ;  
semicolon       : semicolon ';'
                  { ScanWarning("Unnecessary ';'");
                  }
                | ';'
                ;
atomlist	: atomlist atomdef semicolon
                | atomdef semicolon
                | error semicolon
                  { ParserErrorMessage(); }
                ;
atomdef		: ATOMID
                  { switch( crt_section ) {
                      case ATOMDECL: DeclareAtom( $1 ); break;
                      case CHECK:    SetAtomType( $1, DO_CHECK ); break;
                    }
                  }
                ;     
lookatlist	: lookatlist lookatspc semicolon
                | lookatspc semicolon
                | error semicolon
                  { ParserErrorMessage(); }
                ;
lookatspc	: LKTID
                  { AddLookAt( $1 );
                  }
                ;     
monitorlist	: monitorlist monitorspc semicolon
                | monitorspc semicolon
                | error semicolon
                  { ParserErrorMessage(); }
                ;
monitorspc	: MNIID
                  { AddMonitor( $1 );
                  }
                ;     
translist	: translist transspc semicolon
                | transspc semicolon
                | error semicolon
                  { ParserErrorMessage(); }
                ;
transspc	: TPTID
                  { AddTransport( $1 );
                  }
                ;     
uselist		: uselist usefile semicolon
                | usefile semicolon
                | error semicolon
                  { ParserErrorMessage(); }
                ;
usefile		: USEID
                  { AddUseFile( $1 );
                  }
                ;     
setspclist	: setspclist setspcspc semicolon
                | setspcspc semicolon
                | error semicolon
                  { ParserErrorMessage(); }
                ;
setspcspc	: SSPID
                  { switch( crt_section ) {
                      case SETVAR: SetSpcType( VAR_SPC, $1 ); break;
                      case SETRAD: SetSpcType( RAD_SPC, $1 ); break;
                      case SETFIX: SetSpcType( FIX_SPC, $1 ); break;
                    }
                  }
                ;     
species         : species spc semicolon
                | spc semicolon 
                | error semicolon
                  { ParserErrorMessage(); }
                ;
spc             : spcname
                | spcdef
                ;
spcname         : SPCSPC SPCEQUAL atoms
                  { switch( crt_section ) {
                      case DEFVAR: DeclareSpecies( VAR_SPC, $1 ); break;
                      case DEFRAD: DeclareSpecies( RAD_SPC, $1 ); break;
                      case DEFFIX: DeclareSpecies( FIX_SPC, $1 ); break;
                    } 
                  }
                ;
spcdef          : SPCSPC
                  { switch( crt_section ) {
                      case DEFVAR: DeclareSpecies( VAR_SPC, $1 ); break;
                      case DEFRAD: DeclareSpecies( RAD_SPC, $1 ); break;
                      case DEFFIX: DeclareSpecies( FIX_SPC, $1 ); break;
                    } 
                  }
                ;
atoms           : atoms SPCPLUS atom 
                | atom
                ; 
atom            : SPCNR SPCSPC
                  { AddAtom( $2, $1 );
                  }
                | SPCSPC
                  { AddAtom( $1, "1" );
                  }
                ;   
initvalues      : initvalues assignment semicolon
                | assignment semicolon
                | error semicolon
                  { ParserErrorMessage(); }
                ;
assignment      : INISPC INIEQUAL INIVALUE
                  { AssignInitialValue( $1, $3 ); }
                ;
equations       : equations equation semicolon
                | equation semicolon
                | error semicolon
                  { ParserErrorMessage();
                    eqState = LHS; 
                  }
                ;
equation        : eqntag lefths righths rate
                  { eqState = LHS;
                    StoreEquationRate( $4, $1 ); 
                    CheckEquation();
                  }
                | lefths righths rate
                  { eqState = LHS;
                    StoreEquationRate( $3, "          " ); 
                    CheckEquation();
                  }
rate            : RATE rate
                  { strcpy( $$, $1 );
                    strcat( $$, $2 ); 
                  } 
                | RATE
                  { strcpy( $$, $1 );
                  } 
                ;                  
eqntag           : EQNLESS EQNTAG EQNGREATER
                  { strcpy( $$, $2 );
                  } 
                ;                  
lefths          : expresion EQNEQUAL
                  { eqState = RHS; }
                ;   
righths         : expresion EQNCOLON
                  { eqState = RAT; }
                ;
expresion       : expresion EQNSIGN term
                  { ProcessTerm( eqState, $2, crt_coef, crt_term ); 
                  }
                | EQNSIGN term 
                  { ProcessTerm( eqState, $1, crt_coef, crt_term );
                  }
                | term
                  { ProcessTerm( eqState, "+", crt_coef, crt_term );
                  }
                ;
term            : EQNCOEF EQNSPC
                  { strcpy( crt_term, $2 );
                    strcpy( crt_coef, $1 );  
                  }
                | EQNSPC
                  { strcpy( crt_term, $1 );         
                    strcpy( crt_coef, "1" ); 
                  }
                ;
lumps           : lumps lump semicolon
                | lump semicolon 
                | error semicolon
                  { ParserErrorMessage(); }
                ;
lump            : LMPSPC LMPPLUS lump
                  { AddLumpSpecies( $1 );
                  }
                | LMPSPC LMPCOLON LMPSPC
                  {
                    AddLumpSpecies( $1 );
                    CheckLump( $3 );  
                  }
inlinecode      : inlinecode INCODE
		  {
		    InlineBuf = AppendString( InlineBuf, $2, &InlineLen, MAX_INLINE );
		  }
                | INCODE 
		  {
		    InlineBuf = malloc( MAX_INLINE ); 
                    InlineLen = MAX_INLINE;
		    strcpy( InlineBuf, $1);
		  }
                ;  
%%

void yyerror( char * str )
{
}

void ParserErrorMessage()
{
  yyerrok;
/*
  Message("[%d,%s] -> [%d,%s]", crtTokType, crtToken, nextTokType, nextToken );  
*/
  if( crtToken[0] == ';' ) {
    ParserError("Misplaced ';'");
    return;
  }
  switch( crtTokType ) {
    case ATOMID:
      ParserError("Missing ';' after '%s'", crtToken );
      break; 

    case SPCSPC: 
      ParserError("Missing ';' or '+' after '%s'", crtToken );
      break; 
    case SPCNR:
      ParserError("Missing species after '%s'", crtToken );
      break; 
    case SPCPLUS:
      ParserError("Missing atom after '%s'", crtToken );
      break; 
    case SPCEQUAL:
      ParserError("Invalid '=' after '%s'", crtToken );
      break; 

    case INISPC: 
      ParserError("Missing '=' after '%s'", crtToken );
      break; 
    case INIEQUAL: 
      ParserError("Missing value after '%s'", crtToken );
      break; 
    case INIVALUE: 
      ParserError("Missing ';' after '%s'", crtToken );
      break; 

    case EQNSPC: 
      ParserError("Missing '+' or '=' after '%s'", crtToken );
      break; 
    case EQNEQUAL: 
      ParserError("Invalid right hand side of equation");
      break; 
    case EQNCOLON: 
      ParserError("Missing rate after '%s'", crtToken );
      break; 
    case EQNSIGN: 
      ParserError("Missing coeficient after '%s'", crtToken );
      break; 
    case EQNCOEF: 
      ParserError("Missing species after '%s'", crtToken );
      break; 
    case RATE: 
      ParserError("Missing ';' after '%s'", crtToken );
      break; 

    case LMPSPC: 
      ParserError("Missing '+' or ':' or ';' after '%s'", crtToken );
      break; 
    case LMPPLUS: 
      ParserError("Missing species after '%s'", crtToken );
      break; 
    case LMPCOLON: 
      ParserError("Missing species after '%s'", crtToken );
      break; 
    case INLINE:
      ParserError("Missing inline option after '%s'", crtToken );
      break;

    default:
      ParserError("Syntax error after '%s'", crtToken ); 
  }
}


int Parser( char * filename )
{
extern int yydebug;
FILE *f;

  crt_filename = filename;

  f = fopen( crt_filename, "r" );
  if( f == 0 ) {
    FatalError(7,"%s: File not found", crt_filename);
  } 
  
  yyin = f;
  nError   = 0;
  nWarning = 0;
  yydebug = 0;

  yyparse();

  fclose( f );

  return nError;
}          

