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

#define KPP_VERSION "2.1"

#ifndef _GDATA_H_
#define _GDATA_H_

#include <stdio.h>

#define MAX_EQN         1200 /* mz_rs_20050130 */
#define MAX_SPECIES     500 /* mz_rs_20050130 */
#define MAX_SPNAME       30
#define MAX_IVAL         40
/* MAX_EQNTAG = max length of equation ID in eqn file */
#define MAX_EQNTAG       32
/* MAX_K = max length of rate expression in eqn file */
#define MAX_K           400
#define MAX_ATOMS	 10
#define MAX_ATNAME	 10
#define MAX_ATNR	250 
/* made bigger -- JM */
#define MAX_PATH        1024
#define MAX_FILES	 20
#define MAX_EQNLEN      500

#define NO_CODE 	-1
#define max( x, y ) (x) > (y) ? (x) : (y)
#define min( x, y ) (x) < (y) ? (x) : (y)

#define IncName(x)   FileName((x),"MODELS","models","")
#define ModelName(x) FileName((x),"MODELS","models",".def")
#define IntegName(x) FileName((x),"INTEG","int",".def")

enum krtypes { NUMBER, EXPRESION, PHOTO };
enum table_modes { F_TEXT, FC_TEXT, C_TEXT, S_TEXT }; 
enum lang { NO_LANG, C_LANG, F77_LANG, F90_LANG, MATLAB_LANG };
enum inl_code { F77_GLOBAL,    F77_INIT,    F77_DATA,    F77_UTIL,    F77_RATES,    F77_RCONST,
	      F90_GLOBAL,    F90_INIT,    F90_DATA,    F90_UTIL,    F90_RATES,    F90_RCONST,
              C_GLOBAL,      C_INIT,      C_DATA,      C_UTIL,      C_RATES,      C_RCONST,
              MATLAB_GLOBAL, MATLAB_INIT, MATLAB_DATA, MATLAB_UTIL, MATLAB_RATES, MATLAB_RCONST,
	      INLINE_OPT
	      };

enum jacobian_format { JAC_OFF, JAC_FULL, JAC_LU_ROW, JAC_ROW };	      

               	      
typedef short int CODE;
typedef float EQ_VECT[ MAX_EQN ];

typedef struct {
                 char name[ MAX_ATNAME ];
                 char check;
                 char masscheck;
               } ATOM_DEF;

typedef struct {
                 unsigned char code;
                 unsigned char nr; 
               } ATOM;

typedef struct {
		 char type;
		 char lookat;
		 char moni;
		 char trans;
                 short int nratoms;
		 char name[ MAX_SPNAME ];
                 char ival[ MAX_IVAL ];
                 ATOM atoms[ MAX_ATOMS ]; 
	       } SPECIES_DEF;

typedef struct {
		 char type;
		 union {
		   char st[ MAX_K ];
		   float f;
		 } val;
                 char label[ MAX_EQNTAG ];
	       } KREACT;

typedef struct {
		 char * code;
		 int maxlen;
	       } ICODE;


extern int SpeciesNr;
extern int EqnNr;
extern int SpcNr;
extern int AtomNr;
extern int VarNr;
extern int VarActiveNr;
extern int FixNr;
extern int VarStartNr;
extern int FixStartNr;
extern int Hess_NZ;
extern int LU_Jac_NZ;
extern int Jac_NZ;

extern int generateSD;

extern int initNr;
extern int xNr;
extern int yNr;
extern int zNr;

extern int falseSpcNr;

extern int useAggregate;
extern int useJacobian;
extern int useJacSparse;
extern int useHessian;
extern int useStoicmat;
extern int useDouble;
extern int useReorder;
extern int useMex;
extern int useDummyindex;
extern int useEqntags;
extern int useLang;
extern int useStochastic;
extern int useWRFConform; 

extern char Home[ MAX_PATH ];
extern char integrator[ MAX_PATH ];
extern char driver[ MAX_PATH ];
extern char runArgs[  MAX_PATH ];

extern char *eqFileName;
extern char *rootFileName;

extern ATOM_DEF AtomTable[ MAX_ATNR ];
extern SPECIES_DEF SpeciesTable[ MAX_SPECIES ];
extern KREACT 	kr	 [ MAX_EQN ];
extern CODE 	ReverseCode[ MAX_SPECIES ];
extern CODE 	Code	 [ MAX_SPECIES ];
extern float** 	Stoich_Left;
extern float** 	Stoich;
extern float**  Stoich_Right;
extern int 	Reactive [ MAX_SPECIES ];

extern int **structB;
extern int **structJ;
extern int **LUstructJ;

extern ICODE InlineCode[ INLINE_OPT ];

extern char *fileList[ MAX_FILES ];
extern int fileNr;

extern char varDefault[ MAX_IVAL ];
extern char radDefault[ MAX_IVAL ];
extern char fixDefault[ MAX_IVAL ];
extern double cfactor;

void CmdFunction( char *cmd );
void CmdJacobian( char *cmd );
void CmdHessian( char *cmd );
void CmdDouble( char *cmd );
void CmdReorder( char *cmd );
void CmdMex( char *cmd );
void CmdDummyindex( char *cmd );
void CmdEqntags( char *cmd );
void CmdUse( char *cmd );
void CmdLanguage( char *cmd );
void CmdIntegrator( char *cmd );
void CmdDriver( char *cmd );
void CmdRun( char *cmd );
void CmdStochastic( char *cmd );

void Generate();

char * FileName( char *name, char* env, char *dir, char *ext );

int*  AllocIntegerVector( int n, char* message );
int** AllocIntegerMatrix( int m, int n, char* message );
void FreeIntegerMatrix ( int** mat, int m, int n );
int Index( int i );

#endif

