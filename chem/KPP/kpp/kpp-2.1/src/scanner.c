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
#include "scan.h"
#include "y.tab.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

int AtomNr    = 0;
int SpeciesNr = 0;
int EqnNr     = 0;
int SpcNr     = 0;
int VarNr     = 0;
int VarActiveNr  = 0;
int FixNr     = 0;
int VarStartNr   = 0;
int FixStartNr   = 0;


int initNr = -1;
int xNr = 0;
int yNr = 0;
int zNr = 0;

int falseSpcNr = 0;

ATOM_DEF AtomTable[ MAX_ATNR ];
SPECIES_DEF SpeciesTable[ MAX_SPECIES ];
CODE ReverseCode[ MAX_SPECIES ];
CODE Code[ MAX_SPECIES ];
KREACT kr[ MAX_EQN ];

float** Stoich_Left;
float** Stoich;
float** Stoich_Right;
int Reactive[ MAX_SPECIES ];

INLINE_KEY InlineKeys[] = { { F77_GLOBAL,   APPEND,  "F77_GLOBAL" },
                            { F77_INIT,   APPEND,  "F77_INIT" },
                            { F77_DATA,   APPEND,  "F77_DATA" },
                            { F77_UTIL,   APPEND,  "F77_UTIL" }, 
                            { F77_RATES, APPEND,  "F77_RATES" }, 
                            { F77_RCONST, APPEND,  "F77_RCONST" }, 
			    { F90_GLOBAL,   APPEND,  "F90_GLOBAL" },
                            { F90_INIT,   APPEND,  "F90_INIT" },
                            { F90_DATA,   APPEND,  "F90_DATA" },
                            { F90_UTIL,   APPEND,  "F90_UTIL" },
                            { F90_RATES, APPEND,  "F90_RATES" }, 
                            { F90_RCONST, APPEND,  "F90_RCONST" }, 
                            { C_GLOBAL,     APPEND,  "C_GLOBAL" },
                            { C_INIT,     APPEND,  "C_INIT" },
                            { C_DATA,     APPEND,  "C_DATA" },
                            { C_UTIL,     APPEND,  "C_UTIL" },
                            { C_RATES,   APPEND,  "C_RATES" }, 
                            { C_RCONST,   APPEND,  "C_RCONST" }, 
                            { MATLAB_GLOBAL,     APPEND,  "MATLAB_GLOBAL" },
                            { MATLAB_INIT,     APPEND,  "MATLAB_INIT" },
                            { MATLAB_DATA,     APPEND,  "MATLAB_DATA" },
                            { MATLAB_UTIL,     APPEND,  "MATLAB_UTIL" },
                            { MATLAB_RATES,   APPEND,  "MATLAB_RATES" }, 
                            { MATLAB_RCONST,   APPEND,  "MATLAB_RCONST" } 
		 	  };

int useAggregate   = 1;
int useJacobian    = JAC_LU_ROW;
int useJacSparse   = 1;
int useHessian     = 1;
int useStoicmat    = 1;
int useDouble      = 1;
int useReorder     = 1;
int useMex         = 1;
int useDummyindex  = 0;
int useEqntags     = 0;
int useLang        = F77_LANG;
int useStochastic  = 0;
int useWRFConform  = 0;


char integrator[ MAX_PATH ] = "none";
char driver[ MAX_PATH ] = "none";
char runArgs[  MAX_PATH ] = "";

/*  mz_rs_20050701+ */
/* char varDefault[ MAX_IVAL ] = "1.E-8"; */
/* char fixDefault[ MAX_IVAL ] = "1.E-8"; */
/* double cfactor = 1.09E+10; */
char varDefault[ MAX_IVAL ] = "0.";
char fixDefault[ MAX_IVAL ] = "0.";
double cfactor = 1.;
/*  mz_rs_20050701- */

ATOM crtAtoms[ MAX_ATOMS ];
int crtAtomNr = 0;

char *fileList[ MAX_FILES ];
int fileNr = 0;

double Abs( double x ) 
{
  return x > 0 ? x : -x;
}

void DefineInitializeNbr( char *cmd )
{
int n;

  n = sscanf( cmd, "%d", &initNr);
  if( n != 1 )
    ScanError("Bad number of species to initialize <%s>", cmd);
}

void DefineXGrid( char *cmd )
{
int n;

  xNr = 1;
  n = sscanf( cmd, "%d", &xNr);
  if( n != 1 )
    ScanError("Bad X grid number <%s>", cmd);
}

void DefineYGrid( char *cmd )
{
int n;

  yNr = 1;
  n = sscanf( cmd, "%d", &yNr);
  if( n != 1 )
    ScanError("Bad Y grid number <%s>", cmd);
}

void DefineZGrid( char *cmd )
{
int n;

  zNr = 1;
  n = sscanf( cmd, "%d", &zNr);
  if( n != 1 )
    ScanError("Bad Z grid number <%s>", cmd);
}

void CmdFunction( char *cmd )
{
  if( EqNoCase( cmd, "AGGREGATE" ) ) {
    useAggregate = 1;
    return;
  }
  if( EqNoCase( cmd, "SPLIT" ) ) {
    useAggregate = 0;
    return;
  }
  ScanError("'%s': Unknown parameter for #FUNCTION [AGGREGATE|SPLIT]", cmd );
}

void CmdJacobian( char *cmd )
{
  if( EqNoCase( cmd, "OFF" ) ) {
    useJacobian = JAC_OFF;
    useJacSparse = 0;
    return;
  }
  if( EqNoCase( cmd, "FULL" ) ) {
    useJacobian = JAC_FULL;
    useJacSparse = 0;
    return;
  }
  if( EqNoCase( cmd, "SPARSE_LU_ROW" ) ) {
    useJacobian = JAC_LU_ROW;
    useJacSparse = 1;
    return;
  }
  if( EqNoCase( cmd, "SPARSE_ROW" ) ) {
    useJacobian = JAC_ROW;
    useJacSparse = 1;
    return;
  }
  ScanError("'%s': Unknown parameter for #JACOBIAN [OFF|FULL|SPARSE_LU_ROW|SPARSE_ROW]", cmd );
}

void SparseData( char *cmd ) {
  ScanError("Deprecated use of #SPARSEDATA %s: see #JACOBIAN for equivalent functionality", cmd );
}

void CmdHessian( char *cmd )
{
  if( EqNoCase( cmd, "OFF" ) ) {
    useHessian = 0;
    return;
  }
  if( EqNoCase( cmd, "ON" ) ) {
    useHessian = 1;
    return;
  }
  ScanError("'%s': Unknown parameter for #HESSIAN [ON|OFF]", cmd );
}

void CmdStoicmat( char *cmd )
{
  if( EqNoCase( cmd, "OFF" ) ) {
    useStoicmat = 0;
    return;
  }
  if( EqNoCase( cmd, "ON" ) ) {
    useStoicmat = 1;
    return;
  }
  ScanError("'%s': Unknown parameter for #STOICMAT [ON|OFF]", cmd );
}

void CmdDouble( char *cmd )
{
  if( EqNoCase( cmd, "OFF" ) ) {
    useDouble = 0;
    return;
  }
  if( EqNoCase( cmd, "ON" ) ) {
    useDouble = 1;
    return;
  }
  ScanError("'%s': Unknown parameter for #DOUBLE [ON|OFF]", cmd );
}

void CmdReorder( char *cmd )
{
  if( EqNoCase( cmd, "OFF" ) ) {
    useReorder = 0;
    return;
  }
  if( EqNoCase( cmd, "ON" ) ) {
    useReorder = 1;
    return;
  }
  ScanError("'%s': Unknown parameter for #REORDER [ON|OFF]", cmd );
}

void CmdMex( char *cmd )
{
  if( EqNoCase( cmd, "OFF" ) ) {
    useMex = 0;
    return;
  }
  if( EqNoCase( cmd, "ON" ) ) {
    useMex = 1;
    return;
  }
  ScanError("'%s': Unknown parameter for #MEX [ON|OFF]", cmd );
}

void CmdDummyindex( char *cmd )
{
  if( EqNoCase( cmd, "OFF" ) ) {
    useDummyindex = 0;
    return;
  }
  if( EqNoCase( cmd, "ON" ) ) {
    useDummyindex = 1;
    return;
  }
  ScanError("'%s': Unknown parameter for #DUMMYINDEX [ON|OFF]", cmd );
}

void CmdEqntags( char *cmd )
{
  if( EqNoCase( cmd, "OFF" ) ) {
    useEqntags = 0;
    return;
  }
  if( EqNoCase( cmd, "ON" ) ) {
    useEqntags = 1;
    return;
  }
  ScanError("'%s': Unknown parameter for #EQNTAGS [ON|OFF]", cmd );
}

void CmdUse( char *cmd )
{
  ScanError("Deprecated command '#USE %s';\nReplace with '#LANGUAGE %s'.",cmd,cmd );
}


void CmdLanguage( char *cmd )
{
  if( EqNoCase( cmd, "FORTRAN77" ) ) {
    useLang = F77_LANG;
    return;
  }
  if( EqNoCase( cmd, "FORTRAN" ) ) {
    ScanWarning("Fortran version not specified in '#LANGUAGE %s'. Will use Fortran 77.", cmd);
    useLang = F77_LANG;
    return;
  }
  if( EqNoCase( cmd, "FORTRAN90" ) ) {
    useLang = F90_LANG;
    return;
  }
  if( EqNoCase( cmd, "MATLAB" ) ) {
    useLang = MATLAB_LANG;
    return;
  }
  if( EqNoCase( cmd, "C" ) ) {
    useLang = C_LANG;
    return;
  }
  ScanError("'%s': Unknown parameter for #LANGUAGE [Fortran77|Fortran90|C|Matlab]", cmd );
}

void CmdStochastic( char *cmd )
{
  if( EqNoCase( cmd, "ON" ) ) {
    useStochastic = 1;
    return;
  }
  if( EqNoCase( cmd, "OFF" ) ) {
    useStochastic = 0;
    return;
  }
  ScanError("'%s': Unknown parameter for #STOCHASTIC [OFF|ON]", cmd );
}

void CmdIntegrator( char *cmd )
{
  strcpy( integrator, cmd );
}

void CmdDriver( char *cmd )
{
  strcpy( driver, cmd );
}

void CmdRun( char *cmd )
{
  strcpy( runArgs, cmd );
}

int FindAtom( char *atname )
{
int i;
  
  for( i=0; i<AtomNr; i++ )
    if( EqNoCase( AtomTable[ i ].name, atname ) ) {
      return i;   
    }
  return -1;      
}

void DeclareAtom( char *atname )
{
int code;

  code = FindAtom( atname );
  if ( code >= 0 ) {
    ScanError("Multiple declaration for atom %s.", atname );
    return;
  }
  if( AtomNr >= MAX_ATNR ) {
    Error("Too many atoms");
    return;
  }
  
  strcpy( AtomTable[ AtomNr ].name, atname );
  AtomTable[ AtomNr ].check = NO_CHECK;
  AtomTable[ AtomNr ].masscheck = 0;
  AtomNr++;
}

void SetAtomType( char *atname, int type )
{
int code;

  code = FindAtom( atname );
  if ( code < 0 ) {
    ScanError("Undefined atom %s.", atname );
    return;
  }
  AtomTable[ code ].check = type;
}

void CheckAll()
{
int i;
  
  for( i=0; i<AtomNr; i++ ) {
    if( AtomTable[ i ].check != CANCEL_CHECK )
      AtomTable[ i ].check = DO_CHECK;
  }   
  SetAtomType( "IGNORE", NO_CHECK );
}

void AddAtom( char *atname, char *nr )
{
int code;

  code = FindAtom( atname );
  if ( code < 0 ) {
    ScanError("Undefined atom %s.", atname );
    return;
  }
  crtAtoms[ crtAtomNr ].code = (unsigned char)code;
  crtAtoms[ crtAtomNr ].nr = (unsigned char)atoi(nr);
  crtAtomNr++;
}

int FindSpecies( char *spname )
{
int i;
  
  for( i=0; i<SpeciesNr; i++ )
    if( EqNoCase( SpeciesTable[ i ].name, spname ) ) {
      return i;   
    }
  for( i=0; i<2; i++ )
    if( EqNoCase( SpeciesTable[ MAX_SPECIES -1 - i ].name, spname ) ) {
      return MAX_SPECIES -1 - i;   
    }
  return -1;      
}

void StoreSpecies( int index, int type, char *spname )
{
int i;

  strcpy( SpeciesTable[ index ].name, spname );
  SpeciesTable[ index ].type = type; 
  *SpeciesTable[ index ].ival = '\0'; 
  SpeciesTable[ index ].lookat = 0;
  SpeciesTable[ index ].moni = 0;
  SpeciesTable[ index ].trans = 0;
  if( (SpeciesTable[ index ].nratoms == 0) || ( crtAtomNr > 0 ) ) {
    SpeciesTable[ index ].nratoms = crtAtomNr;
    for( i = 0; i < crtAtomNr; i++ )
      SpeciesTable[ index ].atoms[i] = crtAtoms[i];
  }
  crtAtomNr = 0;
}

void DeclareSpecies( int type, char *spname )
{
int code;

  code = FindSpecies( spname );
  if ( code >= 0 ) {
    ScanError("Multiple declaration for species %s.", spname );
    return;
  }
  if( SpeciesNr >= MAX_SPECIES ) {
    Error("Too many species");
    return;
  }
  StoreSpecies( SpeciesNr, type, spname );
  SpeciesNr++;
}

void SetSpcType( int type, char *spname )
{
int code;
int i;

  if( EqNoCase( spname, "VAR_SPEC" ) ) {
    for( i = 0; i < SpeciesNr; i++ ) 
      if( SpeciesTable[i].type == VAR_SPC ) 
        SpeciesTable[i].type = type;
    return;
  }
  if( EqNoCase( spname, "FIX_SPEC" ) ) {
    for( i = 0; i < SpeciesNr; i++ ) 
      if( SpeciesTable[i].type == FIX_SPC ) 
        SpeciesTable[i].type = type;
    return;
  }
  if( EqNoCase( spname, "ALL_SPEC" ) ) {
    for( i = 0; i < SpeciesNr; i++ )
        SpeciesTable[i].type = type;
    return;
  }

  code = FindSpecies( spname );
  if ( code < 0 ) {
    ScanError("Undefined species %s.", spname );
    return;
  }
  SpeciesTable[ code ].type = type; 
}

void AssignInitialValue( char *spname , char *spval )
{
int code;  
double cf;
  
  if( EqNoCase( spname, "CFACTOR" ) ) {
    code = sscanf( spval, "%lg", &cf );
    if( code != 1 ) {
      ScanWarning("Invalid CFACTOR value: %s", spval);
      return;
    }
    cfactor = cf;
    return;
  }
  
  if( EqNoCase( spname, "VAR_SPEC" ) ) {
    strcpy( varDefault, spval );
    return;
  }
  
  
  if( EqNoCase( spname, "FIX_SPEC" ) ) {
    strcpy( fixDefault, spval );
    return;
  }
  
  if( EqNoCase( spname, "ALL_SPEC" ) ) {
    strcpy( varDefault, spval );
    strcpy( fixDefault, spval );
    return;
  }
  
  code = FindSpecies( spname );
  if ( code < 0 ) {
    ScanError("Undefined species %s.", spname );
    return; 
  }     
  strcpy( SpeciesTable[ code ].ival, spval );  
}

void StoreEquationRate( char *rate, char *label )
{
double f;
char buf[ MAX_K ];
int n;
KREACT *kreact;

  kreact = &kr[ EqnNr ]; 
  strcpy( kreact->label, label );
  if( isPhoto ) {
    kreact->type = PHOTO;
    strcpy( kreact->val.st, rate );
    isPhoto = 0;
    return;
  }
  n = sscanf( rate, "%lf%s", &f, buf );
  if ( n == 1 ) {
    kreact->type = NUMBER;
    kreact->val.f = f;
    return;
  }
  kreact->type = EXPRESION;
  strcpy( kreact->val.st, rate );
  return;
}

void CheckEquation()
{
int i,j;
int equal, index;
double r1, r2;
float atcnt[ MAX_ATNR ];
int spc;
SPECIES_DEF *sp;
char errmsg[80];
int err;

  if( EqnNr >= MAX_EQN ) {
    Error("Too many equations");
    return;
  }
  
  for( i = 0; i < AtomNr; i++ )
    atcnt[i] = 0;
    
  for( spc = 0; spc < SpcNr; spc++ ) {
    sp = &SpeciesTable[ Code[spc] ];
    if( Stoich_Left[spc][EqnNr] != 0 ) {
      for( i = 0; i < sp->nratoms; i++ )  
        atcnt[ sp->atoms[i].code ] += Stoich_Left[spc][EqnNr] * sp->atoms[i].nr;
    }
    if( Stoich_Right[spc][EqnNr] != 0 ) {
      for( i = 0; i < sp->nratoms; i++ ) 
        atcnt[ sp->atoms[i].code ] -= Stoich_Right[spc][EqnNr] * sp->atoms[i].nr; 
    }
  } 
  
  *errmsg = 0;
  err = 0;
  
  for( i = 0; i < AtomNr; i++ ) {
    if ( Abs( atcnt[i] ) > 1e-5 ) {
      if ( AtomTable[i].check == CANCEL_CHECK ) {
        err = 0;
        break;
      }
      if ( AtomTable[i].check == NO_CHECK ) {
        continue;
      }
      if ( AtomTable[i].check == DO_CHECK ) {
        err = 1;
        sprintf(errmsg, "%s %s", errmsg, AtomTable[i].name );
        continue;
      }
    }
  } 
   
  if ( err ) 
    ScanWarning( "(eqn %d) Atom balance mismatch for:%s.", EqnNr+1, errmsg );    
        
  for( j = 0; j < SpcNr; j++ )
    if( Stoich_Left[j][EqnNr] != 0 )
      { index = j; break; }
  for( i = 0; i < EqnNr; i++ ) {
    equal = 1;
    r1 = Stoich_Left[index][EqnNr];
    r2 = Stoich_Left[index][i];
    for( j = 0; j < SpcNr; j++ ) {
      if( r1 * Stoich_Left[j][i] != r2 * Stoich_Left[j][EqnNr] )
	{ equal = 0; break; }
      if( r1 * Stoich_Right[j][i] != r2 * Stoich_Right[j][EqnNr] )
	{ equal = 0; break; }
    }
    if ( equal ) {
      if( r1 == r2 )
        ScanError( "Duplicate equation: "
        	   " (eqn<%d> = eqn<%d> )", i+1, EqnNr+1 );
      else
	ScanError( "Linearly dependent equations: "
		   "( %.0f eqn<%d> = %.0f eqn<%d> )",
		   r1, i+1, r2, EqnNr+1 );
      break;
    }
  }
  EqnNr++;
}

void ProcessTerm( int side, char *sign, char *coef, char *spname  )
{
int code;  
CODE crtSpec;
double val;
char buf[40];


  code = FindSpecies( spname );
  if ( code < 0 ) {
    ScanError("Undefined species %s.", spname );
    return;
  }
  
  crtSpec = ReverseCode[ code ];

  if(EqNoCase(spname,"HV")) isPhoto = 1;

  if ( crtSpec == NO_CODE ) {
    if( MAX_SPECIES - code <= 2 ) falseSpcNr++;
    crtSpec = SpcNr++;
    Code[ crtSpec ] = code;
    ReverseCode[ code ] = crtSpec;
  }
  
  strcpy( buf, sign ); 
  strcat( buf, coef ); 
  sscanf( buf, "%lf", &val );

  switch( side ) {
    case LHS: Stoich_Left[ crtSpec ][ EqnNr ] += val;
	      Stoich[ crtSpec ][ EqnNr ] -= val;
	      Reactive[ crtSpec ] = 1;
	      break;
    case RHS: Stoich_Right[ crtSpec ][ EqnNr ] += val;
	      Stoich[ crtSpec ][ EqnNr ] += val;
	      break;
  }
}
           
void AddLumpSpecies( char *spname )
{
int code;  
  
  code = FindSpecies( spname );
  if ( code < 0 ) {
    ScanError("Undefined species %s.", spname );
    return;
  }

  /* ... */                

}

void CheckLump( char *spname )
{
int code;  
  
  code = FindSpecies( spname );
  if ( code < 0 ) {
    ScanError("Undefined species %s.", spname );
    return;
  }

  /* ... */                

}

void AddLookAt( char *spname )
{
int code;  
  
  code = FindSpecies( spname );
  if ( code < 0 ) {
    ScanError("Undefined species %s.", spname );
    return;
  }

  SpeciesTable[ code ].lookat = 1;   
}

void LookAtAll()
{
int i;

  for( i=0; i<SpeciesNr; i++ )
    SpeciesTable[ i ].lookat = 1;  
}

void AddMonitor( char *spname )
{
int code;  
  
  code = FindSpecies( spname );
  if ( code >= 0 ) {
    SpeciesTable[ code ].moni = 1;
    return;
  } 
  
  code = FindAtom( spname );
  if ( code >= 0 ) {
    AtomTable[ code ].masscheck = 1;
    return;
  }
    
  ScanError("Undefined species or atom %s.", spname );
}

void AddTransport( char *spname )
{
int code;  
  
  code = FindSpecies( spname );
  if ( code < 0 ) {
    ScanError("Undefined species %s.", spname );
    return;
  }

  SpeciesTable[ code ].trans = 1;   
}

void TransportAll()
{
int i;

  for( i=0; i<SpeciesNr; i++ )
    SpeciesTable[ i ].trans = 1;  
}

void AddUseFile( char *fname )
{
  fileList[fileNr] = (char*)malloc(strlen(fname)+1);
  strcpy(fileList[fileNr], fname);
  fileNr++;
}

char * AppendString( char * s1, char * s2, int * maxlen, int addlen ) 
{
char * tmp;

  *maxlen += addlen;

  if( !s1 ) {
    s1 = (char*)malloc( *maxlen );
    *s1 = 0;
  }    
  
  if( strlen( s1 ) + strlen( s2 ) >= *maxlen ) {
    s1 = (char*)realloc( (void*)s1, *maxlen );
  }
  strcat( s1, s2 ); 
  return s1;
}

char * ReplaceString( char * s1, char * s2, int * maxlen, int addlen ) 
{
char * tmp;

  if( s1 ) free(s1);

  *maxlen = strlen( s2 );
  s1 = (char*)malloc( 1+*maxlen );
  strcpy( s1, s2 );

  return s1;
}

void AddInlineCode( char * ctx, char * s )
{
ICODE * c;
int i, key, type;
int totallength; /* mz_rs_20050607 */
  
  c = NULL;
  
  for( i = 0; i < INLINE_OPT; i++ ) 
    if( EqNoCase( ctx, InlineKeys[i].kname ) ) {
      key = InlineKeys[i].key;
      c = &InlineCode[key];
      type = InlineKeys[i].type;
      break;
    }
  if( !c ) {
    printf( "\n'%s': Unknown inline option (ignored)", ctx );
    return;
  }

  /*  mz_rs_20050607+ */
  if (c->code) 
    totallength = strlen( c->code )+strlen( s );
  else
    totallength = strlen( s );
  if (totallength>MAX_INLINE)
    ScanError("\nInline code for %s is too long (%d>%d).\nIncrease MAX_INLINE in scan.h and recompile kpp!", 
              ctx, totallength, MAX_INLINE);    
  /*  mz_rs_20050607- */

  switch( type ) {
    case APPEND:  c->code = AppendString( c->code, s, &c->maxlen, MAX_INLINE );
                  break;
    case REPLACE: c->code = ReplaceString( c->code, s, &c->maxlen, MAX_INLINE );
                  break;
  }
}

int ParseEquationFile( char * filename )
{
int i,j;
int code;

  for( i = 0; i < MAX_SPECIES; i++ ) {
    ReverseCode[i] = NO_CODE;
    Reactive[i] = 0;
  }
  for( i = 0; i < MAX_SPECIES; i++ ) {
    for( j = 0; j < MAX_EQN; j++ ) {
      Stoich_Left[i][j] = 0;
      Stoich[i][j] = 0;
      Stoich_Right[i][j] = 0;
    }
  }
  for( i = 0; i < MAX_SPECIES; i++ ) {
    SpeciesTable[ i ].nratoms = 0;
  }
  
  for( i = 0; i < INLINE_OPT; i++ ) {
    InlineCode[i].code = NULL;
    InlineCode[i].maxlen = 0;
  }  
  
  EqnNr = 0;
  SpcNr = 0;
  
  DeclareAtom( "CANCEL" );   
  SetAtomType( "CANCEL", CANCEL_CHECK );
  DeclareAtom( "IGNORE" );   
  SetAtomType( "IGNORE", NO_CHECK );
  DeclareSpecies( DUMMY_SPC, "???" );
  StoreSpecies( MAX_SPECIES-1, DUMMY_SPC, "HV" );
  AddAtom( "CANCEL", "1" );
  StoreSpecies( MAX_SPECIES-2, DUMMY_SPC, "PROD" );

  code = Parser( filename );

  return code;
}

void WRFConform()
{
  useWRFConform = 1;
printf("\nKPP was told to generate WRF conform code");
}
