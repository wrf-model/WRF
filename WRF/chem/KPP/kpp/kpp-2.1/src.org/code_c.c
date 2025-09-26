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

#define MAX_LINE  120
#define LINE_LENGTH 70

int fncPrototipe = 0;

char *C_types[] = { "void",     /* VOID */ 
                    "int",    /* INT */
                    "float",  /* FLOAT */
                    "double", /* DOUBLE */
                    "char *", /* STRING */
                    "char *"  /* DOUBLESTRING */
                  }; 

void C_WriteElm( NODE * n )
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
    case VELM:  if( elm->val.idx.i >= 0 ) sprintf( maxi, "%d", elm->val.idx.i );
                  else sprintf( maxi, "%s", varTable[ -elm->val.idx.i ]->name );
                bprintf("%s[%s]", name, maxi );
		break;
    case MELM:  if( elm->val.idx.i >= 0 ) sprintf( maxi, "%d", elm->val.idx.i );
                  else sprintf( maxi, "%s", varTable[ -elm->val.idx.i ]->name );
                if( elm->val.idx.j >= 0 ) sprintf( maxj, "%d", elm->val.idx.j );
                  else sprintf( maxj, "%s", varTable[ -elm->val.idx.j ]->name );
                bprintf("%s[%s][%s]", name, maxi, maxj );
		break;
    case EELM:  bprintf("(%s)", elm->val.expr );
		break;
  }
}

void C_WriteSymbol( int op )
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

void C_WriteAssign( char *ls, char *rs )
{
int start;
int crtident;
int linelg;
int i,j;
char c;
int first;
int number_of_lines = 1, MAX_NO_OF_LINES = 99;
int ifound, jfound;

/*  Operator Mapping: 0xaa = '*' | 0xab = '+' | 0xac = ',' 
                      0xad = '-' | 0xae ='.' | 0xaf = '/' */		      
char op_mult=0xaa, op_plus=0xab, op_minus=0xad, op_dot=0xae, op_div=0xaf;		      

  crtident = 2 + ident * 2;
  bprintf("%*s%s = ", crtident, "", ls);
  start = strlen( ls ) + crtident + 2;
  linelg = LINE_LENGTH - start;

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
      if( ( rs[i] & 0x80 ) || ( rs[i] == ',' ) ) 
        break;
    if( i < 10 ) {
      printf("\nPossible error when cutting lines");
      i = linelg;
    }*/
    
    c = rs[i]; 
    rs[i] = 0;
    if ( first ) { 
      bprintf("%s", rs );
      linelg++;
      first = 0;
    } else {
      bprintf("\n%*s%s", start, "", rs );      
      if ( jfound ) {
         bprintf(";\n%*s%s = %s", crtident, "", ls, ls);
	 number_of_lines = 1;
	 }
    }  
    rs[i] = c;
    rs += i;
    number_of_lines++;
  }

  if ( number_of_lines > MAX_NO_OF_LINES )
     printf("\n Warning: many continuation lines (%d) for %s = ...",number_of_lines,ls);
  
  if ( first ) bprintf("%s;\n", rs );
          else bprintf("\n%*s%s;\n", start, "", rs );

  FlushBuf();
}

void C_WriteComment( char *fmt, ... )
{
Va_list args;
char buf[ MAX_LINE ];

  Va_start( args, fmt );
  vsprintf( buf, fmt, args );
  va_end( args );

  bprintf( "/* %-*s */\n", LINE_LENGTH - 6, buf );

  FlushBuf();
}


char * C_Decl( int v )
{ 
static char buf[120];
VARIABLE *var;
char *baseType;
char maxi[20];
char maxj[20];

  var = varTable[ v ];
  baseType = C_types[ var->baseType ];
  
  *buf = 0;
  
  switch( var->type ) {
    case ELM:   
              sprintf( buf, "%s %s", baseType, var->name );
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
                /*if( (var->maxi == 0) || 
                    ((var->maxi < 0) && (varTable[ -var->maxi ]->maxi == 0)) )
                  sprintf( maxi, "%s+1", maxi );*/
              if( fncPrototipe ) 
                  sprintf( buf, "%s %s[]", baseType, var->name );
              else  
                  sprintf( buf, "%s %s[%s]", baseType, var->name, maxi );
              break;
    case MELM:  
              if( var->maxi > 0 ) sprintf( maxi, "%d", var->maxi );
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
              if( fncPrototipe ) 
                  sprintf( buf, "%s %s[][]", baseType, var->name );
              else  
                  sprintf( buf, "%s %s[%s][%s]",
                        baseType, var->name, maxi, maxj );  
		break;
    default:
                Message( "Can not declare type %d", var->type );
                Message( "v = %d", v );
                break;
  }
  return buf;
}

void C_Declare( int v )
{
  bprintf("%-40s", strcat( C_Decl(v), ";" ) ); 
  if( varTable[ v ]->comment )
    bprintf(" /* %s */\n", varTable[ v ]->comment );
  else 
    bprintf("\n");   

  FlushBuf();
}

void C_ExternDeclare( int v )
{
  bprintf("extern %-40s", strcat( C_Decl(v), ";" ) ); 
  if( varTable[ v ]->comment )
    bprintf(" /* %s */\n", varTable[ v ]->comment );
  else 
    bprintf("\n");   

  FlushBuf();
}

void C_GlobalDeclare( int v )
{
  C_Declare( v );
}

void C_InitDeclare( int v, int n, void * values )
{
int i;
VARIABLE *var;
int * ival;
double * dval;
char ** cval;
int maxCols = MAX_COLS;

  var = varTable[ v ];
  ival = (int*) values;
  dval = (double*) values;
  cval = (char**) values;
  
  if( var->comment )
      bprintf(" /* %s */\n\n", var->comment );
        
  switch( var->type ) {
    case VELM: bprintf( "  %s  %s[] = {\n%5s", C_types[var->baseType], var->name, " " );
               for( i = 0; i < n; i++ ) {
                 switch( var->baseType ) {
                   case INT: bprintf( "%3d",  ival[i] ); maxCols=12; break;
                   case DOUBLE: 
                   case REAL:bprintf( "%5lg", dval[i] ); maxCols=8; break;
                   case STRING:bprintf( "\"%s\"", cval[i] ); maxCols=8; break;
                   case DOUBLESTRING:bprintf( "\"%s\"", cval[i] ); maxCols=1; break;
                 }
                 if( i < n-1 ) bprintf( "," );
                 if( (i+1) % maxCols == 0 ) bprintf( "\n%5s", " " );
               }  
               if( n == 0 ) bprintf( "0" );
               bprintf( " }; \n\n" );
               break;
               
    case ELM:  bprintf( "  %s %s = ", C_types[var->baseType], var->name );
               switch( var->baseType ) {
                 case INT: bprintf( "%d",  *ival ); break;
                 case DOUBLE: 
                 case REAL:bprintf( "%lg", *dval ); break;
                 case STRING:bprintf( "\"%s\"", *cval ); break;
                 case DOUBLESTRING:bprintf( "\"%s\"", *cval ); break;
               }
               bprintf( ";\n\n" );
               break;
               
    default:   printf( "\n Function not defined !\n" );
               break;
  }

  FlushBuf();
}

void C_DeclareConstant( int v, char *val ) 
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

  switch( var->type ) {
    case CONST: bprintf("#define %-20s %-10s ", var->name, val );
                break;       
    default:
                printf( "Invalid constant", var->type );
                break;
  }
  if( varTable[ v ]->comment )
    bprintf(" /* %s */\n", varTable[ v ]->comment );
  else 
    bprintf("\n");   

  FlushBuf();
}

void C_FunctionStart( int f, int *vars )
{
int i;
int v;
char * name;
int narg;

  name = varTable[ f ]->name;
  narg = varTable[ f ]->maxi;

  fncPrototipe = 1;

  bprintf("void %s( \n", name );
  for( i = 0; i < narg-1; i++ ) {
    v = vars[ i ];
    bprintf("  %-38s", strcat( C_Decl(v), "," ) ); 
    if( varTable[ v ]->comment )
      bprintf(" /* %s */\n", varTable[ v ]->comment );
    else 
      bprintf("\n");   
  }
  if( narg >= 1 ) {
    v = vars[ i ];
    bprintf("  %-38s", C_Decl(v) );
    if( varTable[ v ]->comment )
      bprintf(" /* %s */\n", varTable[ v ]->comment );
    else 
      bprintf("\n");   
  }
  bprintf(")");

  fncPrototipe = 0;
  
  FlushBuf();
}

void C_FunctionPrototipe( int f, ... )
{
Va_list args;
int i;
int vars[20];
char * name;
int narg;

  name = varTable[ f ]->name;
  narg = varTable[ f ]->maxi;

  Va_start( args, f );  
  for( i = 0; i < narg; i++ ) 
    vars[i] = va_arg( args, int );
  va_end( args );
  C_FunctionStart( f, vars );
  bprintf(";\n");

  FlushBuf();
}

void C_FunctionBegin( int f, ... )
{
Va_list args;
int i;
int vars[20];
char * name;
int narg;

  name = varTable[ f ]->name;
  narg = varTable[ f ]->maxi;

  Va_start( args, f );  
  for( i = 0; i < narg; i++ ) 
    vars[i] = va_arg( args, int );
  va_end( args );
  
  CommentFncBegin( f, vars );
  C_FunctionStart( f, vars );
  bprintf("\n");
  bprintf("{\n");
  
  FlushBuf();

  MapFunctionComment( f, vars );
}

void C_FunctionEnd( int f )
{
  bprintf("}\n\n");

  FlushBuf();

  CommentFunctionEnd( f );
}

void C_Inline( char *fmt, ... )
{
Va_list args;
char buf[ 1000 ];

  if( useLang != C_LANG ) return;
  
  Va_start( args, fmt );
  vsprintf( buf, fmt, args );
  va_end( args );
  bprintf( "%s\n",buf );
  
  FlushBuf();
}

void Use_C()
{ 
  WriteElm 	    = C_WriteElm;
  WriteSymbol 	    = C_WriteSymbol;  
  WriteAssign 	    = C_WriteAssign;
  WriteComment 	    = C_WriteComment;
  DeclareConstant   = C_DeclareConstant;
  Declare           = C_Declare;
  ExternDeclare     = C_ExternDeclare;
  GlobalDeclare     = C_GlobalDeclare;
  InitDeclare       = C_InitDeclare;
  
  FunctionStart     = C_FunctionStart;
  FunctionPrototipe = C_FunctionPrototipe;
  FunctionBegin     = C_FunctionBegin;
  FunctionEnd       = C_FunctionEnd;

  OpenFile( &param_headerFile,   rootFileName, "_Parameters.h", "Parameter Header File" );
  OpenFile( &initFile, rootFileName, "_Initialize.c", "Initialization File" );
  OpenFile( &driverFile, rootFileName, "_Main.c", "Main Program File" );
  OpenFile( &integratorFile, rootFileName, "_Integrator.c", 
                   "Numerical Integrator (Time-Stepping) File" );
  OpenFile( &linalgFile, rootFileName, "_LinearAlgebra.c", 
                   "Linear Algebra Data and Routines File" );
  OpenFile( &functionFile, rootFileName, "_Function.c", 
                   "The ODE Function of Chemical Model File" );
  OpenFile( &jacobianFile, rootFileName, "_Jacobian.c", 
                   "The ODE Jacobian of Chemical Model File" );
  OpenFile( &rateFile, rootFileName, "_Rates.c", 
                   "The Reaction Rates File" );
  if ( useStochastic )
    OpenFile( &stochasticFile, rootFileName, "_Stochastic.c", 
                   "The Stochastic Chemical Model File" );
  if ( useStoicmat ) {
    OpenFile( &stoichiomFile, rootFileName, "_Stoichiom.c", 
                   "The Stoichiometric Chemical Model File" );
    OpenFile( &sparse_stoicmFile, rootFileName, "_StoichiomSP.c", 
                   "Sparse Stoichiometric Data Structures File" );
  }		   
  OpenFile( &utilFile, rootFileName, "_Util.c", 
                   "Auxiliary Routines File" );
  OpenFile( &sparse_dataFile, rootFileName, "_Sparse.h", "Sparse Data Header File" );
  OpenFile( &global_dataFile, rootFileName, "_Global.h", "Global Data Header File" );
  if ( useJacSparse ) {
     OpenFile( &sparse_jacFile, rootFileName, "_JacobianSP.c",
        "Sparse Jacobian Data Structures File" );  
  }
  if ( useHessian ) {
     OpenFile( &hessianFile, rootFileName, "_Hessian.c", "Hessian File" );
     OpenFile( &sparse_hessFile, rootFileName, "_HessianSP.c",
         "Sparse Hessian Data Structures File" );
  }   
  OpenFile( &mapFile, rootFileName, ".map", 
                   "Map File with Human-Readable Information" );
  OpenFile( &monitorFile, rootFileName, "_Monitor.c", 
                   "Utility Data Initialization" );
} 
