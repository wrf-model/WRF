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



#ifndef _SCAN_H_
#define _SCAN_H_

#include <stdio.h>
#include "gdef.h"

/*  mz_rs_20050518+ value increased */
#define MAX_INLINE 10000
/* #define MAX_INLINE 4000 */
/*  mz_rs_20050518- */
          
enum eq_state { LHS, RHS, RAT };
enum sptypes { DUMMY_SPC, VAR_SPC, RAD_SPC, FIX_SPC };
enum atomcheck { NO_CHECK, DO_CHECK, CANCEL_CHECK };
enum codetype { APPEND, REPLACE };

typedef struct {
                 int key;
                 int type;
                 char * kname;
               } INLINE_KEY;

extern int eqState;
extern int isPhoto;
extern int crt_line_no;
extern char *crt_filename;
extern int crtLine;
extern char crtFile[];
extern char crtToken[];
extern char nextToken[];
extern int crtTokType;
extern int nextTokType;
extern int nError;
extern int nWarning;
extern int crt_section;

int Parser( char * filename );  
void ScanError( char *fmt, ...  );
void ParserError( char *fmt, ...  );
void ScanWarning( char *fmt, ...  );
void ParserWarning( char *fmt, ...  );
void Error( char *fmt, ...  );
void Warning( char *fmt, ...  );
void Message( char *fmt, ...  );
void FatalError( int status, char *fmt, ... );

void DeclareAtom( char *atname );
void SetAtomType( char *atname, int type );
void AddAtom( char *atname, char *nr );
void DeclareSpecies( int type, char* spname );
void SetSpcType( int type, char *spname );
void AssignInitialValue( char *spname , char *spval ); 
void StoreEquationRate( char *rate, char *label );
void CheckEquation(); 
void ProcessTerm( int side, char *sign, char *coef, char *spname  );
void AddLumpSpecies( char *spname );
void CheckLump( char *spname );
void AddLookAt( char *spname );
void AddMonitor( char *spname );
void AddTransport( char *spname );

void WriteAtoms();
void WriteSpecies();
void WriteMatrices();
void WriteOptions();

char * AppendString( char * s1, char * s2, int * len, int addlen );
void AddInlineCode( char * context, char * code );

#endif
