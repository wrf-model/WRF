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

void WriteAtoms()
{
int i;

  printf("\nATM -----------------------------------------------" );

  for( i = 0; i < SpeciesNr; i++ ) {
    switch( AtomTable[i].check ) {
      case NO_CHECK:
        printf( "\n(%3d) %6s, NO -- ------ ", i, AtomTable[i].name );
        break;
      case DO_CHECK:
        printf( "\n(%3d) %6s, -- DO ------", i, AtomTable[i].name );
        break;
      case CANCEL_CHECK:
        printf( "\n(%3d) %6s, -- -- CANCEL", i, AtomTable[i].name );
        break;
      default:
        printf( "\n(%3d) %6s, -- -- ------ UNKNOWN [%d]", i, 
                AtomTable[i].name, AtomTable[i].check );
        break;
    }
  }  
}

void WriteSpecies()
{
int i;
int j;
char *type;
char *lookat;

  printf("\nSPC -----------------------------------------------" );
  
  for( i = 0; i < SpeciesNr; i++ ) {

    switch( SpeciesTable[i].type ) {
      case VAR_SPC: type = "V - -"; break;
      case RAD_SPC:  type = "- R -"; break;
      case FIX_SPC:    type = "- - F"; break;
      default:       type = "? ? ?"; break;
    }

    switch( SpeciesTable[i].lookat ) {
      case 0:	   lookat = "----"; break; 
      case 1:	   lookat = "LOOK"; break; 
      default:     lookat = "????"; break;
    }

    printf( "\n(%3d) %-10s, type %s,%s {", 
             i, SpeciesTable[i].name, type, lookat );
    for( j = 0; j < SpeciesTable[i].nratoms; j++ )
      printf( " %d%s", SpeciesTable[i].atoms[j].nr, 
                       AtomTable[ SpeciesTable[i].atoms[j].code ].name );
    printf("}");
  }  
}

void WriteMatrices()
{
int i, j;

  printf("\nMAT ------------------ cc -------------------------" );
  for( i = 0; i < SpcNr; i++ ) {
    printf("\n %-6s (%d)[%d]  ", SpeciesTable[ Code[i] ].name,
				 SpeciesTable[ Code[i] ].type, Code[i] );
    for( j = 0; j < EqnNr; j++ ) {
      printf( "%5.1f  ", Stoich_Left[i][j] );
    }
  }

  printf("\nMAT ------------------ cd -------------------------" );
  for( i = 0; i < SpcNr; i++ ) {
    printf("\n %-6s (%d)[%d]  ", SpeciesTable[ Code[i] ].name,
				 SpeciesTable[ Code[i] ].type, Code[i] );
    for( j = 0; j < EqnNr; j++ ) {
      printf( "%5.1f  ", Stoich_Right[i][j] );
    }
  }

  printf("\nMAT ------------------ cf -------------------------" );
  for( i = 0; i < SpcNr; i++ ) {
    printf("\n %-6s (%d)[%d] <r%d> ", SpeciesTable[ Code[i] ].name,
            SpeciesTable[ Code[i] ].type, Code[i], Reactive[i] );
    for( j = 0; j < EqnNr; j++ ) {
      printf( "%5.1f  ", Stoich[i][j] );
    }
  }
}

void WriteOptions()
{
  printf("\n### Options -------------------------------------------\n");
  if( useAggregate )     printf("FUNCTION - AGGREGATE\n");
		    else printf("FUNCTION - SPLIT\n");
  switch ( useJacobian ) {
     case JAC_OFF: printf("JACOBIAN - OFF\n"); break;
     case JAC_FULL: printf("JACOBIAN - FULL\n"); break;
     case JAC_LU_ROW: printf("JACOBIAN - SPARSE W/ ACCOUNT FOR LU DECOMPOSITION FILL-IN\n"); break;
     case JAC_ROW: printf("JACOBIAN - SPARSE\n"); break;
  }		    
  if( useDouble )       printf("DOUBLE   - ON\n");
		    else printf("DOUBLE   - OFF\n");
  if( useReorder )      printf("REORDER  - ON\n");
		    else printf("REORDER  - OFF\n");
  if( useMex     )      printf("MEX      - ON\n");
		    else printf("MEX      - OFF\n");
  if( useDummyindex)    printf("DUMMYINDEX - ON\n");
		    else printf("DUMMYINDEX - OFF\n");
  if( useEqntags)        printf("EQNTAGS - ON\n");
		    else printf("EQNTAGS - OFF\n");
}

