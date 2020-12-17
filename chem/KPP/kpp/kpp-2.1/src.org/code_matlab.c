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

#define MAX_LINE 120

char *MATLAB_types[] = { "",                /* VOID */ 
                    "INTEGER",            /* INT */
                    "REAL",               /* FLOAT */
                    /*"REAL(dp)", */            /* DOUBLE */
                    "DOUBLE PRECISION",   /* DOUBLE */
                    "CHARACTER(LEN=12)",  /* STRING */
                    "CHARACTER(LEN=100)"  /* DOUBLESTRING */
                  };

/*************************************************************************************************/
void MATLAB_WriteElm( NODE * n )
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
void MATLAB_WriteSymbol( int op )
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
    case POW:   bprintf("^");
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
void MATLAB_WriteAssign( char *ls, char *rs )
{
int start;
int linelg;
int i, j;
int ifound, jfound;
char c;
int first;
int crtident;

/* Max no of continuation lines in F95 standard is 39 */
int number_of_lines = 1, MAX_NO_OF_LINES = 36;

/*  Operator Mapping: 0xaa = '*' | 0xab = '+' | 0xac = ',' 
                      0xad = '-' | 0xae ='.' | 0xaf = '/' */		      
char op_mult=0xaa, op_plus=0xab, op_minus=0xad, op_dot=0xae, op_div=0xaf;		      
  
  crtident = 3 + ident * 2;
  bprintf("%*s%s = ", crtident, "", ls);
  start = strlen( ls ) + 2;
  linelg = 70 - crtident - start - 1;

  first = 1;
  while( strlen(rs) > linelg ) {
    ifound = 0; jfound = 0;
    if ( number_of_lines >= MAX_NO_OF_LINES ) {
     /* If a new line needs to be started. 
          Note: the approach below will create erroneous code if the +/- is within a subexpression, e.g. for
          A*(B+C) one cannot start a new continuation line by splitting at the + sign */
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
    
    c = rs[i]; 
    rs[i] = 0;
    
    if ( first ) { /* first line in a split row */
      bprintf("%s", rs ); 
      linelg++;
      first = 0;
    } else {/* continuation line in a split row - but not last line*/
      bprintf(" ...\n     %*s%s", start, "", rs ); 			
      if ( jfound ) {
         bprintf(" ;\n%*s%s = %s", crtident, "", ls, ls);
	 number_of_lines = 1;
	 }
    }  
    rs[i] = c;
    rs += i;  /* jump to the first not-yet-written character */
    number_of_lines++;
  }
  
  if ( number_of_lines > MAX_NO_OF_LINES ) {
     printf("\n Warning: %d continuation lines for %s = ...",number_of_lines,ls);
     }

  if ( first ) bprintf("%s ;\n", rs );  /* non-split row */
          else bprintf(" ...\n     %*s%s;\n", start, "", rs ); /* last line in a split row */


  FlushBuf();
}


/*************************************************************************************************/
void MATLAB_WriteComment( char *fmt, ... )
{
Va_list args;
char buf[ MAX_LINE ];

  va_start( args, fmt );
  vsprintf( buf, fmt, args );
  va_end( args );
  
  fprintf( currentFile, "%c ", '%' );
  bprintf( "%-65s\n", buf );

  FlushBuf();
}

/*************************************************************************************************/
char * MATLAB_Decl( int v )
{
static char buf[120];
VARIABLE *var;
char *baseType;
char maxi[20];
char maxj[20];

  buf[0] = 0; return buf; /* Nothing to declare in matlab */
  var = varTable[ v ];
  baseType = MATLAB_types[ var->baseType ];
  
  *buf = 0;

  switch( var->type ) {
    case ELM:   sprintf( buf, "%s :: %s",
                        baseType, var->name );
		break;
    case VELM:  
                if( var->maxi > 0 ) sprintf( maxi, "%d", var->maxi );
                  else sprintf( maxi, "%s", varTable[ -var->maxi ]->name );  
                if( (var->maxi == 0) || 
                    ((var->maxi < 0) && (varTable[ -var->maxi ]->maxi == 0)) )
                  strcat( maxi, "+1");
                sprintf( buf, "%s, DIMENSION(%s) :: %s",	
                        baseType, maxi, var->name );
 		break;
    case MELM:  if( var->maxi > 0 ) sprintf( maxi, "%d", var->maxi );
                  else sprintf( maxi, "%s", varTable[ -var->maxi ]->name );  
                if( (var->maxi == 0) || 
                    ((var->maxi < 0) && (varTable[ -var->maxi ]->maxi == 0)) )
                  strcat( maxi, "+1");
                if( var->maxj > 0 ) sprintf( maxj, "%d", var->maxj );
                  else sprintf( maxj, "%s", varTable[ -var->maxj ]->name );  
                if( (var->maxj == 0) || 
                    ((var->maxj < 0 ) && (varTable[ -var->maxj ]->maxi == 0)) )
                  strcat( maxj, "+1");
                sprintf( buf, "%s, DIMENSION(%s,%s) :: %s",			
                         baseType, maxi, maxj,var->name );  
		break;
    default:
                printf( "Can not declare type %d\n", var->type );
                break;
  }
  return buf;
}

/*************************************************************************************************/
char * MATLAB_DeclareData( int v, void * values, int n)
{
int i, j;
int nlines, nmax;
int split;
static char buf[120];
VARIABLE *var;
int * ival;
double * dval;
char ** cval;
char *baseType;
char maxi[20];
char maxj[20];
int maxCols = MAX_COLS;
char dsbuf[55];
 
 int i_from, i_to;
 int isplit;
 int splitsize;
 int maxi_mod;
 int maxi_div;
 
 char mynumber[30];

  var = varTable[ v ];
  ival = (int*) values;
  dval = (double *) values;
  cval = (char **) values;

  nlines = 1;
  nmax = 1;
  split = 0;
  var -> maxi = max( n, 1 );

  baseType = MATLAB_types[ var->baseType ];
  
  *buf = 0;

  switch( var->type ) {				/* changed here */
    case ELM:   
	   /* bprintf( "  %s :: %s = ", baseType, var->name );
		switch ( var->baseType ) {
		  case INT: bprintf( "%d", *ival ); break;
		  case DOUBLE: bprintf( "%f", *dval); break;
		  case REAL: bprintf( "%lg", *dval ); break;
		  case STRING: bprintf( "'%3s'", *cval ); break;
		} */
		break;
     case VELM:
      splitsize = 36; /*elements*/
      maxi_mod = var->maxi % splitsize;
      maxi_div = var->maxi / splitsize;

      if( var->maxi > 0 ) sprintf( maxi, "%d", var->maxi );
                  else sprintf( maxi, "%s", varTable[ -var->maxi ]->name );  
                if( (var->maxi == 0) || 
                    ((var->maxi < 0) && (varTable[ -var->maxi ]->maxi == 0)) )
                  strcat( maxi, "+1");
                if( var->maxj > 0 ) sprintf( maxj, "%d", var->maxj );
                  else sprintf( maxj, "%s", varTable[ -var->maxj ]->name );  
                if( (var->maxj == 0) || 
                    ((var->maxj < 0 ) && (varTable[ -var->maxj ]->maxi == 0)) )
                  strcat( maxj, "+1");        
		  /* now list values */
        /* if ( (var->baseType==STRING)||(var->baseType==DOUBLESTRING) ) {
           bprintf( "%s(1:%s,:) = [ ... \n", var->name, maxi) ;	
	} else {
           bprintf( "%s(1:%s) = [ ... \n", var->name, maxi) ;
	}*/
        if ( (var->baseType==STRING)||(var->baseType==DOUBLESTRING) ) {
           bprintf( "%s = [ ... \n", var->name, maxi) ;	
	} else {
           bprintf( "%s = [ ... \n", var->name, maxi) ;
	}
        
        /* if the array is defined in one piece, then the for loop will
           go from 0 to n. Otherwise, there will be partial arrays from
           i_from to i_to which are of size splitsize except for the
           last one which is usually smaller and contains the rest */
        for ( i=0; i < n; i++ ) {
          switch( var -> baseType ) {
          case INT:
            bprintf( "%4d", ival[i] ); maxCols =12; break;
          case DOUBLE:
 	    sprintf(mynumber, "%12.6e",dval[i]);
            bprintf( "  %s", mynumber ); maxCols = 5; break;
          case REAL:
            bprintf( "%12.6e", dval[i] ); maxCols = 5; break;
          case STRING:
            bprintf( "'%12s'", cval[i] ); maxCols = 3; break;
          case DOUBLESTRING:
	    strncpy( dsbuf, cval[i], 54 ); dsbuf[54]='\0';
	    bprintf( "'%48s'", dsbuf ); maxCols=1; break;
          }
          if( i < n-1 ) {
            bprintf( ";" );
            if( (i+1) % maxCols == 0 ) {
              bprintf( " ... \n" );
              nlines++;
            }
          }
         }
        bprintf( " ];\n" );
        break;
				
    case MELM:  if( var->maxi > 0 ) sprintf( maxi, "%d", var->maxi );
                  else sprintf( maxi, "%s", varTable[ -var->maxi ]->name );  
                if( (var->maxi == 0) || 
                    ((var->maxi < 0) && (varTable[ -var->maxi ]->maxi == 0)) )
                  strcat( maxi, "+1");
                if( var->maxj > 0 ) sprintf( maxj, "%d", var->maxj );
                  else sprintf( maxj, "%s", varTable[ -var->maxj ]->name );  
                if( (var->maxj == 0) || 
                    ((var->maxj < 0 ) && (varTable[ -var->maxj ]->maxi == 0)) )
                  strcat( maxj, "+1");
                sprintf( buf, "%s, DIMENSION(%s,%s) :: %s\n",	/* changed here */		
                         baseType, maxi, maxj,var->name );  
		break;
    default:
                printf( "Can not declare type %d", var->type );
                break;
  }
  return buf;
}

/*************************************************************************************************/
void MATLAB_Declare( int v )
{
  if( varTable[ v ]->comment ) {
    MATLAB_WriteComment( "%s - %s", 
                    varTable[ v ]->name, varTable[ v ]->comment );
  }
  FlushBuf();
  bprintf("  %s\n", MATLAB_Decl(v) );

  FlushBuf();
}

/*************************************************************************************************/
void MATLAB_ExternDeclare( int v )
{
  if( varTable[ v ]->comment ) {
    MATLAB_WriteComment( "%s - %s", 
                    varTable[ v ]->name, varTable[ v ]->comment );
  }
  FlushBuf();
  bprintf(" global %s;\n", varTable[ v ]->name );
  FlushBuf();
}


/*************************************************************************************************/
void MATLAB_GlobalDeclare( int v )
{
}


/*************************************************************************************************/
void MATLAB_DeclareConstant( int v, char *val ) 
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
    MATLAB_WriteComment( "%s - %s", var->name, var->comment );

  switch( var->type ) {
    case CONST: bprintf(" global %s;",var->name, val );
	      bprintf(" %s = %s; \n", var->name, val );
                break;       
    default:
                printf( "Invalid constant %d", var->type );
                break;
  }

  FlushBuf();
}


/*************************************************************************************************/
void MATLAB_WriteVecData( VARIABLE * var, int min, int max, int split )	
{
char buf[80];
char *p;

  if( split )
    sprintf( buf, "%6sdata( %s(i), i = %d, %d ) / &\n%5s", 		
                " ", var->name, min, max, " " );
  else
    sprintf( buf, "%6sdata %s / &\n%5s",
                    " ", var->name, " " );
                                      
  FlushThisBuf( buf );
  bprintf( " / \n\n" );
  FlushBuf();        
}

/*************************************************************************************************/
void MATLAB_DeclareDataOld( int v, int * values, int n )
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
                     MATLAB_WriteVecData( var, min, max, split );  
                     min = max + 1;
                 } 
                 else { 
                   if( i < n-1 ) bprintf( "," );
                   if( (i+1) % maxCols == 0 ) { 
                     bprintf( "\n%5s", " " );
                     nlines++;                 
                   }  
                 }  
                 max ++;
               }
               MATLAB_WriteVecData( var, min, max-1, split );
               break;
                                                                 
    case ELM:  bprintf( "%6sdata %s / ", " ", var->name );
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
void MATLAB_InitDeclare( int v, int n, void * values )
{
int i;
VARIABLE * var;

  var = varTable[ v ];
  var->maxi = max( n, 1 );

  NewLines(1);  
  MATLAB_DeclareData( v, values, n );
}

/*************************************************************************************************/
void MATLAB_FunctionStart( int f, int *vars )
{
int i;
int v;
char * name;
int narg;

  name = varTable[ f ]->name;
  narg = varTable[ f ]->maxi;

  bprintf("function  " );
  if( narg >= 1 ) {
    v = vars[ narg-1 ];
    bprintf("[ %s ] = ", varTable[ v ]->name );
  }
  bprintf(" %s_%s ( ", rootFileName, name );
  for( i = 0; i < narg-1; i++ ) {
    v = vars[ i ];
    bprintf("%s ", varTable[ v ]->name );
    if (i<narg-2) bprintf(", ");
  }
  bprintf(")\n");

  FlushBuf();
}                  

/*************************************************************************************************/
void MATLAB_FunctionPrototipe( int f, ... )
{
char * name;
int narg;

  name = varTable[ f ]->name;
  narg = varTable[ f ]->maxi;

  bprintf("      EXTERNAL %s\n", name );

  FlushBuf();
}

/*************************************************************************************************/
void MATLAB_FunctionBegin( int f, ... )
{
Va_list args;
int i;
int v;
int vars[20];
char * name;
int narg;
FILE *oldf;
char buf[200], bufname[200];
time_t t;

  name = varTable[ f ]->name;
  narg = varTable[ f ]->maxi;

 /*Adi - each Matlab functin requires a separate file*/
  sprintf( buf, "%s_%s.m", rootFileName, varTable[ f ]->name );  
  mex_funFile = fopen(buf, "w");
  if( mex_funFile == 0 ) {
    FatalError(3,"%s: Can't create file", buf );
  } 
  UseFile( mex_funFile );
 /*Adi*/
  
    
  Va_start( args, f );
  for( i = 0; i < narg; i++ ) 
    vars[ i ] = va_arg( args, int );
  va_end( args );
    
  CommentFncBegin( f, vars );
  
  WriteDelim();
  WriteComment("");
  WriteComment("Generated by KPP - symbolic chemistry Kinetics PreProcessor" );
  WriteComment("    KPP is developed at CGRER labs University of Iowa by" );
  WriteComment("    Valeriu Damian & Adrian Sandu" );
  WriteComment("");
  WriteComment("%-20s : %s", "File", buf  );
  strcpy( buf, (char*)ctime( &t ) ); 
  buf[ (int)strlen(buf) - 1 ] = 0;
  WriteComment("%-20s : %s", "Time", buf );
  WriteComment("%-20s : %s", "Working directory", getcwd(buf, 200) );
  WriteComment("%-20s : %s", "Equation file", eqFileName );
  WriteComment("%-20s : %s", "Output root filename", rootFileName );
  WriteComment("");
  WriteDelim();
  NewLines(1);
  
  MATLAB_FunctionStart( f, vars );
  NewLines(1);

  FlushBuf();

  MapFunctionComment( f, vars );
}

/*************************************************************************************************/
void MATLAB_FunctionEnd( int f )
{
  bprintf("      \nreturn\n\n");

  FlushBuf();

  CommentFunctionEnd( f );
  
 /*Adi*/
  fclose(mex_funFile);
  

}

/*************************************************************************************************/
void MATLAB_Inline( char *fmt, ... )
{
Va_list args;
char buf[ 1000 ];

  if( useLang != MATLAB_LANG ) return;
  
  Va_start( args, fmt );
  vsprintf( buf, fmt, args );
  va_end( args );
  bprintf( "%s\n", buf );
  
  FlushBuf();
}

/*************************************************************************************************/
void Use_MATLAB()
{ 
  WriteElm 	    = MATLAB_WriteElm;
  WriteSymbol 	    = MATLAB_WriteSymbol;  
  WriteAssign 	    = MATLAB_WriteAssign;
  WriteComment 	    = MATLAB_WriteComment;
  DeclareConstant   = MATLAB_DeclareConstant;
  Declare           = MATLAB_Declare;
  ExternDeclare     = MATLAB_ExternDeclare;
  GlobalDeclare     = MATLAB_GlobalDeclare;
  InitDeclare       = MATLAB_InitDeclare;

  FunctionStart     = MATLAB_FunctionStart;
  FunctionPrototipe = MATLAB_FunctionPrototipe;
  FunctionBegin     = MATLAB_FunctionBegin;
  FunctionEnd       = MATLAB_FunctionEnd;

  OpenFile( &param_headerFile,   rootFileName, "_Parameters.m","Parameter Definition File" );
  OpenFile( &driverFile, rootFileName, "_Main.m", "Main Program File" );
  OpenFile( &rateFile, rootFileName, "_Rates.m", 
                   "The Reaction Rates File" );
  if ( useStoicmat ) {
     OpenFile( &stoichiomFile, rootFileName, "_Stoichiom.m", 
                   "The Stoichiometric Chemical Model File" );
     OpenFile( &sparse_stoicmFile, rootFileName, "_StoichiomSP.m", 
                   "Sparse Stoichiometric Data Structures File" );
  }		   
  OpenFile( &utilFile, rootFileName, "_Util.m", 
                   "Auxiliary Routines File" );
  OpenFile( &sparse_dataFile, rootFileName, "_Sparse.m",
                       "Sparse Data Definition File" );
  OpenFile( &global_dataFile, rootFileName, "_Global_defs.m", "Global Data Definition File" );
  if ( useJacSparse ) {
     OpenFile( &sparse_jacFile, rootFileName, "_JacobianSP.m",
         "Sparse Jacobian Data Structures File" );  
  }
  if ( useHessian ) {
     OpenFile( &sparse_hessFile, rootFileName, "_HessianSP.m",
         "Sparse Hessian Data Structures File" );
  }     
  OpenFile( &mapFile, rootFileName, ".map", 
                   "Map File with Human-Readable Information" );
  OpenFile( &monitorFile, rootFileName, "_Monitor.m", 
                   "Utility Data Definition File" );
} 
