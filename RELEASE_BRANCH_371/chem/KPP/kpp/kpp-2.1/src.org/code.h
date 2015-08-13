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


#ifndef _CODE_H_
#define _CODE_H_

#include <stdlib.h>
#include "gdef.h"

#define MAX_DEPTH 	 10
#define MAX_SUBST 	 20
#define SUBST	  	100
#define MAX_VAR 	150
#define MAX_OUTBUF   200000
#define MAX_COLS          8
#define MAX_LINES        20

#define WriteAll bprintf

enum types  { NONE, ADD, SUB, MUL, DIV, POW, CONST, ELM, VELM, MELM, EELM, FNC };
extern int PRI[];

enum signs { O_PAREN = 20, C_PAREN };
enum base_types { VOID, INT, REAL, DOUBLE, STRING, DOUBLESTRING };
/*  mz_rs_20050117+ */
extern FILE * initFile;
/*  mz_rs_20050117- */
extern FILE * driverFile;
extern FILE * functionFile;
extern FILE * global_dataFile;
extern FILE * hessianFile;
extern FILE * integratorFile;
extern FILE * jacobianFile;
extern FILE * linalgFile;
extern FILE * mapFile;
extern FILE * makeFile;
extern FILE * monitorFile;
extern FILE * mex_funFile;
extern FILE * mex_jacFile;
extern FILE * mex_hessFile;
extern FILE * param_headerFile;
extern FILE * rateFile;
extern FILE * sparse_dataFile;
extern FILE * sparse_jacFile;
extern FILE * sparse_hessFile;
extern FILE * sparse_stoicmFile;
extern FILE * stoichiomFile;
extern FILE * stochasticFile;
extern FILE * utilFile;

extern FILE * currentFile;

extern int ident;
extern int real;
extern char * CommonName;

void OpenFile( FILE **fpp, char *name, char * ext, char * identity );
FILE * UseFile( FILE *fp );
 
typedef struct {
		 char *name;
		 int type;
		 int baseType;
                 int maxi;
                 int maxj;
                 int value;
                 char *comment;
	       } VARIABLE;

extern VARIABLE* varTable[];

extern char *outBuf;
extern char *outBuffer;

void AllowBreak();
void bprintf( char *fmt, ... );
void FlushBuf();
void FlushThisBuf( char * buf );
void NewLines( int n );
void C_Inline( char *fmt, ... );
void F77_Inline( char *fmt, ... );
void IncludeFile( char * fname );
void IncludeCode( char *fmt, ... );
void MapFunctionComment( int f, int *vars );
      
int DefineVariable( char * name, int t, int bt, int maxi, int maxj, char * comment );
void FreeVariable( int n );

#define DefConst( name, bt, cmt ) DefineVariable( name, CONST, bt, 0, 0, cmt )
#define DefElm( name, bt, cmt ) DefineVariable( name, ELM, bt, 0, 0, cmt )
#define DefvElm( name, bt, n, cmt ) DefineVariable( name, VELM, bt, n, 0, cmt )
#define DefmElm( name, bt, m, n, cmt ) DefineVariable( name, MELM, bt, m, n, cmt )
#define DefeElm( name, cmt ) DefineVariable( name, EELM, 0, 0, 0, cmt )
#define DefFnc( name, n, cmt ) DefineVariable( name, FNC, 0, n, 0, cmt )
  
typedef struct {
		 int var;
		 union {
		   char * expr;
		   float cnst;
		   struct {
		     int i;
		     int j;
		   } idx;
		 } val;
	       } ELEMENT;

typedef struct node {
		      struct node * left;
		      struct node * right;
		      int type;
		      int sign;
		      ELEMENT *elm;
		    } NODE;

extern char *F77_types[];
extern char *F90_types[];
extern char *C_types[];
extern char *MATLAB_types[];

NODE * Elm( int v, ... );
#define Const( x ) Elm( 0, (double)x )
#define Expr( x ) Elm( 1, x )

void FreeNode( NODE * n );

NODE * Add( NODE *n1, NODE *n2 );
NODE * Sub( NODE *n1, NODE *n2 );
NODE * Mul( NODE *n1, NODE *n2 );
NODE * Div( NODE *n1, NODE *n2 );
NODE * Pow( NODE *n1, NODE *n2 );

void Assign( NODE *lval, NODE *rval );
void MkSubst( NODE *n1, NODE *n2 );
void RmSubst( NODE *n );
void CommentFncBegin( int f, int *vars );
void CommentFunctionBegin( int f, ... );
void CommentFunctionEnd( int f );

void Use_C();
void Use_F();
void Use_F90();
void Use_MATLAB();

extern void (*WriteElm)( NODE *n );
extern void (*WriteSymbol)( int op );
extern void (*WriteAssign)( char* ls, char* rs );
extern void (*WriteComment)( char *fmt, ...  );
extern void (*Declare)( int v );
extern void (*ExternDeclare)( int v );
extern void (*GlobalDeclare)( int v );
extern void (*InitDeclare)( int v, int n, void * values );
extern void (*DeclareConstant)( int v, char *val );
extern void (*FunctionStart)( int f, int *vars );
extern void (*FunctionPrototipe)( int f, ... );
extern void (*FunctionBegin)( int f, ... );
extern void (*FunctionEnd)( int f );

void WriteDelim();

#endif
