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

/* added 9/20/94 */

/*
 *  rsl_mon_bcast
 *
 *  broadcasts a buffer from the monitor to all other nodes 
 *
 *  Right now this is a dumb algorithm for portability.  Could
 *  map this down to bcasts in specific underlying message packages
 *  at some point.
 */

#include <stdio.h>
#include <stdlib.h>
#include "rsl.h"

/*@
  RSL_MON_BCAST -- Broadcast a buffer from monitor to all other procs.

  Notes:
  On return, the contents of Arg1 on the monitor processor (usually
  processor zero) will be contents of Arg1 on all processors.
  The integer Arg2 is the number of bytes of Arg1 to be broadcast.
  This routine may be called before RSL_MESH has been called.

  This routine broadcasts the entire buffer, which is considered to be
  replicated, and therefore undecomposed, data.
  RSL also permits decomposed array data to be aggragated and distributed
  from the monitor processor using internal writes and reads (see RSL_WRITE
  and RSL_READ).

  Despite the unfortunate similarity in names, this routine should not be confused with RSL_BCAST, the scatter operation
  for nest forcing.  

  Example:

$ Need example

BREAKTHEEXAMPLECODE

  See also:
  RSL_INITIALIZE, RSL_IAMMONITOR, RSL_READ, RSL_WRITE
@*/

RSL_MON_BCAST ( buf, nbytes0 )
  void *
    buf ;       /*  (IO) Buffer to be broadcast. */
  int_p 
    nbytes0 ;   /*  (I) Length of buffer in bytes. */
{
  int nbytes ;
  int retval ;
  int mtype, mdest, mfrom, mlen ;
  int P ;

  nbytes = *nbytes0 ;

  RSL_TEST_ERR( buf == NULL, "NULL pointer" ) ;
  RSL_TEST_ERR( nbytes < 0 , "Invalid (negative) number of bytes" ) ;

#if 0
/* Replaced this with an MPI_Bcast (below), 3/3/00 */
  RSL_C_IAMMONITOR ( &retval ) ;

  if ( retval == 1 )                    /* monitor code */
  {
    for ( P = 0 ; P < rsl_nproc_all ; P++ )     /* 95/02/22 */
    {
      if ( rsl_c_comp2phys_proc(P) != rsl_myproc )  /* not me */
      {
               mdest = rsl_c_comp2phys_proc (P) ;
        mlen  = nbytes ;
        mtype = MTYPE_FROMTO( MSG_MON_BCAST, rsl_myproc, mdest ) ;
               RSL_SEND( buf, mlen, mtype, mdest ) ;
      }
    }
  }
  else                                  /* other nodes */
  {
    mfrom = RSL_C_MONITOR_PROC () ;
    mlen  = nbytes ;
    mtype  = MTYPE_FROMTO( MSG_MON_BCAST, mfrom, rsl_myproc ) ;
    RSL_RECV( buf, mlen, mtype ) ;
  }
#else
#  ifndef STUBS
  MPI_Bcast( buf, nbytes, MPI_BYTE, 0, rsl_mpi_communicator ) ;
#  endif
#endif

}
