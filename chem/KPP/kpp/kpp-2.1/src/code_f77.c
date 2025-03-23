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


#include "gdata.h"
#include "code.h"
#include <string.h>
#include <stdio.h>

#define MAX_LINE 120

char *F77_types[] = { "",                 /* VOID */ 
                    "INTEGER",          /* INT */
                    "REAL",             /* FLOAT */
                    "REAL*8",           /* DOUBLE */
                    "CHARACTER*12",     /* STRING */
                    "CHARACTER*100"     /* DOUBLESTRING */
                  };

/*************************************************************************************************/
void F77_WriteElm( NODE * n )
{
ELEMENT *elm;
char * name;
char maxi[20];
char maxj[20];

  elm = n->elm;
  name = varTable[ elm->var ]->name;

  switch( n->type ) {
    case CONST: bprintf("%g", elm->val.cnst);
		break;
    case ELM:   bprintf("%s", name);
		break;
    case VELM:  if( elm->val.idx.i >= 0 ) sprintf( maxi, "%d", elm->val.idx.i+1 );
                  else sprintf( maxi, "%s", varTable[ -elm->val.idx.i ]->name );
                bprintf("%s(%s)", name, maxi );
		break;
    case MELM:  if( elm->val.idx.i >= 0 ) sprintf( maxi, "%d", elm->val.idx.i+1 );
                  else sprintf( maxi, "%s", varTable[ -elm->val.idx.i ]->name );
                if( elm->val.idx.j >= 0 ) sprintf( maxj, "%d", elm->val.idx.j+1 );
                  else sprintf( maxj, "%s", varTable[ -elm->val.idx.j ]->name );
                bprintf("%s(%s,%s)", name, maxi, maxj );
		break;
    case EELM:  bprintf("(%s)", elm->val.expr );
		break;
  }
}

/*************************************************************************************************/
void F77_WriteSymbol( int op )
{
  switch( op ) {
    case ADD:   bprintf("+"); 
                AllowBreak();
    		break;
    case SUB:   bprintf("-"); 
                AllowBreak();
    		break;
    case MUL:   bprintf("*"); 
                AllowBreak();
    		break;
    case DIV:   bprintf("/"); 
                AllowBreak();
    		break;
    case POW:   bprintf("power");
                break;		
    case O_PAREN: bprintf("(");
                AllowBreak();
                break;            
    case C_PAREN: bprintf(")");
                break;            
    case NONE:
                break;            
  }
}

/*************************************************************************************************/
void F77_WriteAssign( char *ls, char *rs )
{
int start;
int linelg;
int i,j;
char c;
int first;
int crtident;
int number_of_lines = 1, MAX_NO_OF_LINES = 36;
int ifound, jfound;
    
/*  Operator Mapping: 0xaa = '*' | 0xab = '+' | 0xac = ',' 
                      0xad = '-' | 0xae ='.' | 0xaf = '/' */		      
char op_mult=0xaa, op_plus=0xab, op_minus=0xad, op_dot=0xae, op_div=0xaf;		      
  
  crtident = 6 + ident * 2;
  bprintf("%*s%s = ", crtident, "", ls);
  start = strlen( ls ) + 2;
  linelg = 70 - crtident - start - 1;

  first = 1;
  while( strlen(rs) > linelg ) {
    ifound = 0; jfound = 0;
    if ( number_of_lines >= MAX_NO_OF_LINES ) {/* if a new line needs to be started */
     for( j=linelg; j>5; j-- ) /* split row here if +, -, or comma */
       if ( ( rs[j] == op_plus )||( rs[j] == op_minus )||( rs[j]==',' ) ) { 
        jfound = 1; i=j; break;
	}
    }
    if ( ( number_of_lines < MAX_NO_OF_LINES )||( !jfound ) ) {
     for( i=linelg; i>10; i-- ) /* split row here if operator or comma */
       if ( ( rs[i] & 0x80 )||( rs[i]==',' ) ) {
        ifound = 1; break;
	}
     if( i <= 10 ) {
      printf("\n Warning: possible error in continuation lines for %s = ...",ls);
      i = linelg;
     }
    } 
    while ( rs[i-1] & 0x80 ) i--; /* put all operators on the next row */
    while ( rs[i] == ',' ) i++; /* put commas on the current row */
    
    /*for( i=linelg; i>10; i-- )
      if( ( rs[i] & 0x80 )||( rs[i]==',' ) )
        break;
    if( i < 10 ) {
      printf("\nPossible error when cutting lines");
      i = linelg;
    } */
	
    c = rs[i]; 
    rs[i] = 0;
    
    if ( first ) { /* first line in a split row */
      bprintf("%s", rs );
      linelg++;
      first = 0;
    } else {/* continuation line in a split row - but not last line*/
      bprintf("\n     &%*s%s", start, "", rs );
      if ( jfound ) {
         bprintf("\n%*s%s = %s", crtident, "", ls, ls);
	 number_of_lines = 1;
	 }
    }  
    rs[i] = c;
    rs += i;
    number_of_lines++;
  }

  if ( number_of_lines > MAX_NO_OF_LINES )
     printf("\n Warning: many continuation lines (%d) for %s = ...",number_of_lines,ls);
  
  if ( first ) bprintf("%s\n", rs ); /* non-split row */
          else bprintf("\n     &%*s%s\n", start, "", rs ); /* last line in a split row */

  FlushBuf();
}


/*************************************************************************************************/
void F77_WriteComment( char *fmt, ... )
{
Va_list args;
char buf[ MAX_LINE ];

  Va_start( args, fmt );
  vsprintf( buf, fmt, args );
  va_end( args );
  bprintf( "C %-65s\n", buf );

  FlushBuf();
}

/*************************************************************************************************/
char * F77_Decl( int v )
{
static char buf[120];
VARIABLE *var;
char *baseType;
char maxi[20];
char maxj[20];

  var = varTable[ v ];
  baseType = F77_types[ var->baseType ];
  
  *buf = 0;

  switch( var->type ) {
    case ELM:   sprintf( buf, "%s %s",
                        baseType, var->name );
		break;
    case VELM:  
                if( var->maxi > 0 ) sprintf( maxi, "%d", var->maxi );
                if( var->maxi == 0 ) sprintf( maxi, "%d", 1 );
                /*  else sprintf( maxi, "%s", varTable[ -var->maxi ]->name);  */
                if ( var->maxi < 0 ) {
		    if (varTable[ -var->maxi ]->value < 0) 
		      sprintf( maxi, "%s", varTable[ -var->maxi ]->name );
		    else  
		      sprintf( maxi, "%d", (varTable[-var->maxi]->value)==0?
		           1:varTable[-var->maxi]->value );
		  }  
                /* if( (var->maxi == 0) || 
                    ((var->maxi < 0) && (varTable[ -var->maxi ]->maxi == 0)) )
                  strcat( maxi, "+1"); */
                sprintf( buf, "%s %s(%s)",
                        baseType, var->name, maxi );
		break;
    case MELM:  if( var->maxi > 0 ) sprintf( maxi, "%d", var->maxi );
               else { 
		 if (varTable[ -var->maxi ]->value < 0)
		    sprintf( maxi, "%s", varTable[ -var->maxi ]->name );
		 else  
		    sprintf( maxi, "%d", (varTable[-var->maxi]->value)==0?
		           1:varTable[-var->maxi]->value );
	       }  
                /* if( (var->maxi == 0) || 
                    ((var->maxi < 0) && (varTable[ -var->maxi ]->maxi == 0)) )
                  strcat( maxi, "+1"); */
                if( var->maxj > 0 ) sprintf( maxj, "%d", var->maxj );
                else {
		  if (varTable[ -var->maxj ]->value < 0)
		     sprintf( maxj, "%s", varTable[ -var->maxj ]->name );
		  else  
		     sprintf( maxj, "%d", (varTable[-var->maxj]->value)==0?
		           1:varTable[-var->maxj]->value );
		  }  
                /*if( (var->maxj == 0) || 
                    ((var->maxj < 0 ) && (varTable[ -var->maxj ]->maxi == 0)) )
                  strcat( maxj, "+1");*/
                sprintf( buf, "%s %s(%s,%s)",
                         baseType, var->name, maxi, maxj );  
		break;
    default:
                printf( "Can not declare type %d\n", var->type );
                break;
  }
  return buf;
}

/*************************************************************************************************/
void F77_Declare( int v )
{
  if( varTable[ v ]->comment ) {
    F77_WriteComment( "%s - %s", 
                    varTable[ v ]->name, varTable[ v ]->comment );
  }
  bprintf("      %s\n", F77_Decl(v) );

  FlushBuf();
}

/*************************************************************************************************/
void F77_ExternDeclare( int v )
{
  F77_Declare( v );
  bprintf("      COMMON /%s/ %s\n", CommonName, varTable[ v ]->name );
}

/*************************************************************************************************/
void F77_GlobalDeclare( int v )
{
}

/*************************************************************************************************/
void F77_DeclareConstant( int v, char *val ) 
{
VARIABLE *var;
int ival;
char dummy_val[100];           /* used just to avoid strange behaviour of
                                  sscanf when compiled with gcc */
                                  
  strcpy(dummy_val,val);val = dummy_val;

  var = varTable[ v ];
  
  if( sscanf(val, "%d", &ival) == 1 )
    if( ival == 0 ) var->maxi = 0;
               else var->maxi = 1;
  else
    var->maxi = -1;       
  
  if( var->comment ) 
    F77_WriteComment( "%s - %s", var->name, var->comment );

  switch( var->type ) {
    case CONST: bprintf("      %s %s\n", 
                       F77_types[ var->baseType ], var->name );
                bprintf("      PARAMETER ( %s = %s )\n", 
                       var->name, val);
                break;       
    default:
                printf( "Invalid constant %d", var->type );
                break;
  }

  FlushBuf();
}

/*************************************************************************************************/
void WriteVecData( VARIABLE * var, int min, int max, int split )
{
char buf[80];
char *p;

  if( split )
    sprintf( buf, "%6sDATA( %s(i), i = %d, %d ) /\n%5s*", 
                " ", var->name, min, max, " " );
  else
    sprintf( buf, "%6sDATA %s /\n%5s*",
                    " ", var->name, " " );
                                      
  FlushThisBuf( buf );
  bprintf( " / \n\n" );
  FlushBuf();        
}

/*************************************************************************************************/
void F77_DeclareData( int v, int * values, int n )
{
int i, j;
int nlines, min, max;
int split;
VARIABLE *var;
int * ival;
double * dval;
char **cval;
int maxCols = MAX_COLS;
char dsbuf[55];

  var = varTable[ v ];
  ival = (int*) values;
  dval = (double*) values;
  cval = (char**) values;
    
  nlines = 1;
  min = max = 1;
  split = 0;

  switch( var->type ) {
    case VELM: if( n <= 0 ) break;
    	       for( i = 0; i < n; i++ ) {
                 switch( var->baseType ) {
                   case INT: bprintf( "%3d",  ival[i] ); maxCols=12; break;
                   case DOUBLE: 
                   case REAL:bprintf( "%5lg", dval[i] ); maxCols=8; break;
                   case STRING:bprintf( "'%s'", cval[i] ); maxCols=5; break;
                   case DOUBLESTRING:
		        strncpy( dsbuf, cval[i], 54 ); dsbuf[54]='\0';
		        bprintf( "'%48s'", dsbuf ); maxCols=1; break;
                 }
                 if( ( (i+1) % 12 == 0 ) && ( nlines > MAX_LINES ) ) {
                     split = 1; nlines = 1;
                     WriteVecData( var, min, max, split );  
                     min = max + 1;
                 } 
                 else { 
                   if( i < n-1 ) bprintf( "," );
                   if( (i+1) % maxCols == 0 ) { 
                     bprintf( "\n%5s*", " " );
                     nlines++;                 
                   }  
                 }  
                 max ++;
               }
               WriteVecData( var, min, max-1, split );
               break;
                                                                 
    case ELM:  bprintf( "%6sDATA %s / ", " ", var->name );
               switch( var->baseType ) {
                 case INT: bprintf( "%d",  *ival ); break;
                 case DOUBLE: 
                 case REAL:bprintf( "%lg", *dval ); break;
                 case STRING:bprintf( "'%s'", *cval ); break;
                 case DOUBLESTRING:		        
		        strncpy( dsbuf, *cval, 54 ); dsbuf[54]='\0';
		        bprintf( "'%s'", dsbuf ); maxCols=1; break;
                        /* bprintf( "'%50s'", *cval ); break; */
               }
               bprintf( " / \n" );
               FlushBuf();
               break;
    default:
               printf( "\n Function not defined !\n" );
               break;
  }
}

/*************************************************************************************************/
void F77_InitDeclare( int v, int n, void * values )
{
int i;
VARIABLE * var;

  var = varTable[ v ];
  var->maxi = max( n, 1 );

  NewLines(1);  
  F77_DeclareData( v, values, n );
}

/*************************************************************************************************/
void F77_FunctionStart( int f, int *vars )
{
int i;
int v;
char * name;
int narg;

  name = varTable[ f ]->name;
  narg = varTable[ f ]->maxi;

  bprintf("      SUBROUTINE %s ( ", name );
  for( i = 0; i < narg-1; i++ ) {
    v = vars[ i ];
    bprintf("%s, ", varTable[ v ]->name );
  }
  if( narg >= 1 ) {
    v = vars[ narg-1 ];
    bprintf("%s ", varTable[ v ]->name );
  }
  bprintf(")\n");

  FlushBuf();
}                  

/*************************************************************************************************/
void F77_FunctionPrototipe( int f, ... )
{
char * name;
int narg;

  name = varTable[ f ]->name;
  narg = varTable[ f ]->maxi;

  bprintf("      EXTERNAL %s\n", name );

  FlushBuf();
}

/*************************************************************************************************/
void F77_FunctionBegin( int f, ... )
{
Va_list args;
int i;
int v;
int vars[20];
char * name;
int narg;
FILE *oldf;

  name = varTable[ f ]->name;
  narg = varTable[ f ]->maxi;
    
  Va_start( args, f );
  for( i = 0; i < narg; i++ ) 
    vars[ i ] = va_arg( args, int );
  va_end( args );
    
  CommentFncBegin( f, vars );
  F77_FunctionStart( f, vars );
  NewLines(1);
  bprintf("      IMPLICIT NONE\n" );
  bprintf("      INCLUDE '%s_Parameters.h'\n\n", rootFileName );

  FlushBuf();

  for( i = 0; i < narg; i++ ) 
    F77_Declare( vars[ i ] );

  bprintf("\n");
  FlushBuf();

  MapFunctionComment( f, vars );
}

/*************************************************************************************************/
void F77_FunctionEnd( int f )
{
  bprintf("      RETURN\n");
  bprintf("      END\n\n");

  FlushBuf();

  CommentFunctionEnd( f );
}

/*************************************************************************************************/
void F77_Inline( char *fmt, ... )
{
Va_list args;
char buf[ 1000 ];

  if( useLang != F77_LANG ) return;
  
  Va_start( args, fmt );
  vsprintf( buf, fmt, args );
  va_end( args );
  bprintf( "%s\n", buf );
  
  FlushBuf();
}

/*************************************************************************************************/
void Use_F( char *rootFileName )
{ 
  WriteElm 	    = F77_WriteElm;
  WriteSymbol 	    = F77_WriteSymbol;  
  WriteAssign 	    = F77_WriteAssign;
  WriteComment 	    = F77_WriteComment;
  DeclareConstant   = F77_DeclareConstant;
  Declare           = F77_Declare;
  ExternDeclare     = F77_ExternDeclare;
  GlobalDeclare     = F77_GlobalDeclare;
  InitDeclare       = F77_InitDeclare;

  FunctionStart     = F77_FunctionStart;
  FunctionPrototipe = F77_FunctionPrototipe;
  FunctionBegin     = F77_FunctionBegin;
  FunctionEnd       = F77_FunctionEnd;

  OpenFile( &param_headerFile,   rootFileName, "_Parameters.h", "Parameter Header File" );
  OpenFile( &initFile, rootFileName, "_Initialize.f", "Initialization File" );
  OpenFile( &driverFile, rootFileName, "_Main.f", "Main Program File" );
  OpenFile( &integratorFile, rootFileName, "_Integrator.f", 
                   "Numerical Integrator (Time-Stepping) File" );
  OpenFile( &linalgFile, rootFileName, "_LinearAlgebra.f", 
                   "Linear Algebra Data and Routines File" );
  OpenFile( &functionFile, rootFileName, "_Function.f", 
                   "The ODE Function of Chemical Model File" );
  OpenFile( &jacobianFile, rootFileName, "_Jacobian.f", 
                   "The ODE Jacobian of Chemical Model File" );
  OpenFile( &rateFile, rootFileName, "_Rates.f", 
                   "The Reaction Rates File" );
  if ( useStochastic )
    OpenFile( &stochasticFile, rootFileName, "_Stochastic.f", 
                   "The Stochastic Chemical Model File" );
  if ( useStoicmat ) {
     OpenFile( &stoichiomFile, rootFileName, "_Stoichiom.f", 
                   "The Stoichiometric Chemical Model File" );
     OpenFile( &sparse_stoicmFile, rootFileName, "_StoichiomSP.f", 
                   "Sparse Stoichiometric Data Structures File" );
  }		   
  OpenFile( &utilFile, rootFileName, "_Util.f", 
                   "Auxiliary Routines File" );
  OpenFile( &sparse_dataFile, rootFileName, "_Sparse.h", "Sparse Data Header File" );
  OpenFile( &global_dataFile, rootFileName, "_Global.h", "Global Data Header File" );
  if ( useJacSparse ) {
     OpenFile( &sparse_jacFile, rootFileName, "_JacobianSP.f",
         "Sparse Jacobian Data Structures File" );  
  }
  if ( useHessian ) {
     OpenFile( &hessianFile, rootFileName, "_Hessian.f", "Hessian File" );
     OpenFile( &sparse_hessFile, rootFileName, "_HessianSP.f",
         "Sparse Hessian Data Structures File" );
  }     
  OpenFile( &mapFile, rootFileName, ".map", 
                   "Map File with Human-Readable Information" );
  OpenFile( &monitorFile, rootFileName, "_Monitor.f", 
                   "Initialization of Utility Data Structures" );
} 
