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

FILE * inp = stdin ;
char inline[256] ;

main( argc, argv )
  int argc;
  char * argv[] ;
{
  int n, done ;

  if ( argc == 2 )
  {
    inp = NULL ;
  }

  done = 0 ;
  while ( ! done )
  {
  if ( inp == NULL )
  {
    n = atoi( argv[1] ) ;
    done = 1 ;
  }
  else
  {
    if ( fgets( inline, 80, stdin ) == NULL ) 
      break ;
    n = atoi( inline ) ;
  }

  printf("From  is %d\n", MTYPE_FROM(n) ) ;
  printf("Tag   is %d\n", MTYPE_TAG(n) ) ;

  switch ( MTYPE_TAG(n) )
  {
case MSG_STENCOM	: printf("msg_stencom\n") ; break ;
case MSG_READ_RESPONSE	: printf("msg_read_response\n") ; break ;
case MSG_WRITE_RESPONSE	: printf("msg_write_response\n") ; break ;
case MSG_WRITE_COMPUTE_RESPONSE	: printf("msg_write_compute_response\n") ; break ;
case MSG_SPECIAL1_RESPONSE	: printf("msg_special1_response\n") ; break ;
case MSG_SPECIAL2_RESPONSE	: printf("msg_special2_response\n") ; break ;
case MSG_FROM_PARENT	: printf("msg_from_parent\n") ; break ;
case MSG_BCAST_SETUP	: printf("msg_bcast_setup\n") ; break ;
case MSG_MERGE_SETUP	: printf("msg_merge_setup\n") ; break ;
case MSG_TO_PARENT	: printf("msg_to_parent\n") ; break ;
  default : break ;
  }
  }
  exit(0) ;
}
