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
#include "rsl_comm.h"

char buf1[4096] ;
char buf2[4096] ;
char buf3[4096] ;
char buf4[4096] ;
char buf5[4096] ;

main()
{
  int rc, bc ;
  int source ;
  int tag, tag1, tag2, tag3, tag4, tag5 ;
  int handle1 ;
  int nbuf[10] ;
  int que[5] ;
  int retval ;
  int i, ngot ;

  RSL_INITIALIZE () ;

  tag1 = 1001 ;
  tag2 = 1002 ;
  tag3 = 1003 ;
  tag4 = 1004 ;
  tag5 = 1005 ;

  if ( rsl_myproc != 0 )
  {
/* Post several receives */
    RSL_RECVBEGIN ( buf1, 2000, tag1 ) ; 
    RSL_RECVBEGIN ( buf2, 2000, tag2 ) ; 
    RSL_RECVBEGIN ( buf3, 2000, tag3 ) ; 
    RSL_RECVBEGIN ( buf4, 2000, tag4 ) ; 
    RSL_RECVBEGIN ( buf5, 2000, tag5 ) ; 

    que[0] = 0 ;
    que[1] = 0 ;
    que[2] = 0 ;
    que[3] = 0 ;
    que[4] = 0 ;

/* Probe for receive */
    i = 0 ;
    ngot = 0 ;
    while ( ngot < 5 ) 
    {
      if ( i >= 5 ) i = 0 ; 
      tag = 1001 + i ;
      if ( que[i] != RSL_INVALID )
      {
      RSL_PROBE ( tag, &retval ) ;
      if ( retval )
      {
        RSL_RECVEND ( tag ) ;
        que[i] = RSL_INVALID ;
        ngot++ ;
      }
      }
      i++ ;
    }
  }
  else
  {
    RSL_SEND ( buf1, 2000, tag1, 1 ) ;
    RSL_SEND ( buf2, 2000, tag2, 1 ) ;
    RSL_SEND ( buf3, 2000, tag3, 1 ) ;
    RSL_SEND ( buf4, 2000, tag4, 1 ) ;
    RSL_SEND ( buf5, 2000, tag5, 1 ) ;
  }
}


