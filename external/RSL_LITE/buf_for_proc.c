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

#ifndef MS_SUA
# include <stdio.h>
#endif
#include <stdlib.h>
#include "rsl_lite.h"
#ifndef STUBMPI
#  include "mpi.h"
#endif

typedef struct bufdesc {
  char * buf ;
  int size ;
} bufdesc_t ;

/* buftab[RSL_SENDBUF] is send buffer descriptor,
   buftab[RSL_RECVBUF] is recv buffer descriptor. */
static bufdesc_t buftab[2][RSL_MAXPROC] ;
static int first = 1 ;

/* 
   buffer_for_proc

   returns a pointer to a buffer already allocated for processor P if
   it is big enough; otherwise, it frees the existing buffer, if there
   is one and then allocates a new one that is big enough.  If RSL_FREEBUF
   is called for a P, the two buffers (send and recv) are truncated and
   freed and NULL is returned.

   You are guaranteed to get back the same buffer as the previous call
   for a given P, as long as the size is less than the size passed to
   the previous call.  Thus, you can use this routine to manage the
   pointers to the buffers for P and avoid having to set up arrays
   of pointers in the routines that use these buffers.

*/

char *
buffer_for_proc( P, size, code )
  int P ;   /* processor number */
  int size,		/* requested size */
      code ;		/* RSL_SENDBUF, RSL_RECVBUF, or RSL_FREEBUF */
{
  int p ;
  int i, j ;
  char mess[1024] ;
  char * ret ;

  ret = NULL ;
  if ( first )
  {
    for ( p = 0 ; p < RSL_MAXPROC ; p++ )
    {
      buftab[0][p].buf  = NULL ;    
      buftab[1][p].buf  = NULL ;    
      buftab[0][p].size = 0 ;    
      buftab[1][p].size = 0 ;    
    }
    first = 0 ;
  }
  if ( P < 0 || P >= RSL_MAXPROC )
  {
    sprintf(mess,"Bad P argument to buffer_for_proc.  P = %d. Has RSL_MESH been called?\n",P) ;
    RSL_TEST_ERR( 1, mess ) ;
  }
  if ( code == RSL_FREEBUF )
  {
/* fprintf(stderr,"buffer_for_proc freeing buffer %d\n",P) ; */
    if ( buftab[0][P].buf != NULL ) RSL_FREE( buftab[0][P].buf ) ;
    if ( buftab[1][P].buf != NULL ) RSL_FREE( buftab[1][P].buf ) ;
    buftab[0][P].buf  = NULL ;    
    buftab[1][P].buf  = NULL ;    
    buftab[0][P].size = 0 ;    
    buftab[1][P].size = 0 ;    
/* show_tot_size() ; */
  }
  else if ( code == RSL_SENDBUF || code == RSL_RECVBUF )
  {
    if ( buftab[code][P].size < size )
    {
#if 0
fprintf(stderr,"buffer_for_proc %s %d : was %d, increasing to %d\n",
         (code == RSL_SENDBUF)?"RSL_SENDBUF":"RSL_RECVBUF",
	 P,buftab[code][P].size, size+512) ;
#endif
      if ( buftab[code][P].buf != NULL ) RSL_FREE( buftab[code][P].buf ) ;
      buftab[code][P].buf = RSL_MALLOC(char,size+512) ;
      buftab[code][P].size = size+512 ;
/* show_tot_size() ; */
    }
    ret = buftab[code][P].buf ;
  }
  return(ret) ;
}

show_tot_size()
{
  int P ;
  int acc ;
  acc = 0 ;
  for ( P = 0 ; P < RSL_MAXPROC ; P++ )
  {
    acc += buftab[0][P].size ;
    acc += buftab[1][P].size ;
  }
#ifndef MS_SUA
  fprintf(stderr,"Total bytes allocated for buffers: %d\n", acc ) ;
#endif
}

int
buffer_size_for_proc( P, code )
  int P ;
  int code ;
{
  return( buftab[code][P].size ) ;
}
