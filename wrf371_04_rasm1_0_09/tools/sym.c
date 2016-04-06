/***********************************************************************
     
                              COPYRIGHT
     
     The following is a notice of limited availability of the code and 
     Government license and disclaimer which must be included in the 
     prologue of the code and in all source listings of the code.
     
     Copyright notice
       (c) 1977  University of Chicago
     
     Permission is hereby granted to use, reproduce, prepare 
     derivative works, and to redistribute to others at no charge.  If 
     you distribute a copy or copies of the Software, or you modify a 
     copy or copies of the Software or any portion of it, thus forming 
     a work based on the Software and make and/or distribute copies of 
     such work, you must meet the following conditions:
     
          a) If you make a copy of the Software (modified or verbatim) 
             it must include the copyright notice and Government       
             license and disclaimer.
     
          b) You must cause the modified Software to carry prominent   
             notices stating that you changed specified portions of    
             the Software.
     
     This software was authored by:
     
     Argonne National Laboratory
     J. Michalakes: (630) 252-6646; email: michalak@mcs.anl.gov
     Mathematics and Computer Science Division
     Argonne National Laboratory, Argonne, IL  60439
     
     ARGONNE NATIONAL LABORATORY (ANL), WITH FACILITIES IN THE STATES 
     OF ILLINOIS AND IDAHO, IS OWNED BY THE UNITED STATES GOVERNMENT, 
     AND OPERATED BY THE UNIVERSITY OF CHICAGO UNDER PROVISION OF A 
     CONTRACT WITH THE DEPARTMENT OF ENERGY.
     
                      GOVERNMENT LICENSE AND DISCLAIMER
     
     This computer code material was prepared, in part, as an account 
     of work sponsored by an agency of the United States Government.
     The Government is granted for itself and others acting on its 
     behalf a paid-up, nonexclusive, irrevocable worldwide license in 
     this data to reproduce, prepare derivative works, distribute 
     copies to the public, perform publicly and display publicly, and 
     to permit others to do so.  NEITHER THE UNITED STATES GOVERNMENT 
     NOR ANY AGENCY THEREOF, NOR THE UNIVERSITY OF CHICAGO, NOR ANY OF 
     THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR 
     ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, 
     COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, 
     PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
     NOT INFRINGE PRIVATELY OWNED RIGHTS.

***************************************************************************/
/* sym.c

    Implementation dependent routines for using symtab_gen.c
    in N32 .

*/

#include <stdio.h>
#include "sym.h"

extern sym_nodeptr symget() ;

static char ** symtab ;  /* 2-19-90 */

int
sym_init() /* 2-19-90, initialize symbol table package */
{
    create_ht( &symtab ) ;
    if (symtab == NULL) 
    {
	fprintf(stderr,"init_sym(): could not create hash table") ;
	exit(1) ;
    }
    return(0) ;
}

sym_nodeptr
sym_add( name )
char * name ;
{
    sym_nodeptr new_sym_node(); 
    char **node_name() ;
    sym_nodeptr *node_next() ;
    return( symget( name, new_sym_node, node_name, node_next, symtab, 1 ) ) ;
}

sym_nodeptr
sym_get( name )
char * name ;
{
    sym_nodeptr new_sym_node(); 
    char **node_name() ;
    sym_nodeptr *node_next() ;
    return( symget( name, new_sym_node, node_name, node_next, symtab, 0 ) ) ;
}

sym_nodeptr
new_sym_node()
{
    void * malloc() ;
    sym_nodeptr p ;
    p = (sym_nodeptr) malloc( sizeof( struct sym_node ) ) ;
    p->name = NULL ;
    p->next = NULL ;

    return( p ) ;
}

char **
node_name(p)
sym_nodeptr p ;
{
    char ** x ;
    x = &(p->name) ;
    return( x ) ;
}

sym_nodeptr *
node_next(p)
sym_nodeptr p ;
{
    sym_nodeptr *x ;
    x = &(p->next) ;
    return( x ) ;
}

int
show_entry(x)
sym_nodeptr x ;
{
  int i ;
  if ( x == NULL ) return(0) ;
  printf("Symbol table entry:\n") ;
  printf("lexeme %s\n", x->name ) ;
  printf("   dim %s\n", (x->dim==1?"M":(x->dim==2?"N":"O")) ) ;
  printf(" ndims %d\n", x->ndims ) ;
  for ( i = 0 ; i < x->ndims && i < 7 ; i++ )
    printf(" dim %d -> %s\n",i,(x->dims[i]==1?"M":(x->dims[i]==2?"N":"O")) ) ;
  return(0) ;
}

/* MEMORY LEAK !!!! -- this just abandons the old table and leaves on the heap. */
/* The registry mechanism is not a long-running program and is not apt to 
   run into memory problems.  Might want to fix this anyway, though, someday. */
int
sym_forget() 
{
  create_ht( &symtab ) ;
  if (symtab == NULL)
  {
      fprintf(stderr,"init_sym(): could not create hash table") ;
      exit(1) ;
  }
  return(0) ;
}

