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

#include <stdio.h>
#include <stdlib.h>
#include "rsl.h"

/* for reading in replicated data */

int
handle_special2( req )
  rsl_read_req_t * req ;
{
  int mlen, mtag, mdest ;
  rsl_read_resp_t resp ;
  char *rbuf ;
  int nbytes ;
  int P ;

#ifdef T3D
fprintf(stderr,"handle_special2 called - disabled on T3D.  Stopping\n") ;
exit(3) ;
#endif

  /* is this a new request?  if so, service it.  otherwise, it has already
  being handled -- discard the message */

  if ( req->sequence <= io_seq_monitor ) return(0) ;
  if ( req->sequence > io_seq_monitor+1 )
  {
    sprintf(mess,"handle_read_request: sequence number race %d > %d",
	   req->sequence, io_seq_monitor+1 ) ;
    RSL_TEST_ERR(1,mess) ;
  }
  /* req->sequence equals io_seq_monitor+1 */
  io_seq_monitor++ ;

  nbytes = req->speciala ;
  rbuf = RSL_MALLOC( char, nbytes ) ;

  FORT_CHARACTERREAD ( &(req->unit), rbuf, &nbytes ) ;

  resp.sequence = req->sequence ;

  for ( P = 0 ; P < rsl_nproc ; P++ )
  {
    mdest = rsl_c_comp2phys_proc(P) ;
    mtag = MTYPE_FROMTO( MSG_SPECIAL2_RESPONSE, rsl_myproc, mdest ) ;
    mlen = sizeof( resp ) ;
    RSL_SEND( &resp, mlen, mtag, mdest ) ;
    mlen = nbytes ;
    RSL_SEND( rbuf, mlen, mtag, mdest ) ;
  }

  RSL_FREE( rbuf ) ;
}

