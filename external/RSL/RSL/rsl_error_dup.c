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

/* redirect standard error from process into a file */
/* also redirect standard output to a file */

#include <stdio.h>
#ifdef SEQUENT
#include <sys/file.h>
#else
#include <fcntl.h>
#endif

#include "rsl.h"

#define STANDARD_ERROR 2

#define STANDARD_OUTPUT 1

/*@
  RSL_ERROR_DUP --- Redirect standard out and error on each proc.

  Notes:
  This routine redirects the standard and error outputs to a 
  unique pair of files for each processor.  The file names generated
  are rsl.out.dddd and rsl.error.dddd, where dddd is the 4-digit
  zero-padded processor number.  RSL\_INITIALIZE must be called before
  this routine.

  See also:
  RSL\_INITIALIZE, RSL\_MESH
@*/

RSL_ERROR_DUP () 
{
    int newfd ;
    char filename[256] ;

    int *me ;

    me = &rsl_myproc ;

/* redirect standard out*/
    sprintf(filename,"rsl.out.%04d",*me) ;
    if ((newfd = open( filename, O_CREAT | O_WRONLY, 0666 )) < 0 )
    {
	perror("error_dup: cannot open rsl.out.nnnn") ;
	fprintf(stderr,"...sending output to standard output and continuing.\n") ;
	return ;
    }
    if( dup2( newfd, STANDARD_OUTPUT ) < 0 )
    {
	perror("error_dup: dup2 fails to change output descriptor") ;
	fprintf(stderr,"...sending output to standard output and continuing.\n") ;
	close(newfd) ;
	return ;
    }

/* redirect standard error */
    sprintf(filename,"rsl.error.%04d",*me) ;
    if ((newfd = open( filename, O_CREAT | O_WRONLY, 0666 )) < 0 )
    {
	perror("error_dup: cannot open rsl.error.log") ;
	fprintf(stderr,"...sending error to standard error and continuing.\n") ;
	return ;
    }
    if( dup2( newfd, STANDARD_ERROR ) < 0 )
    {
	perror("error_dup: dup2 fails to change error descriptor") ;
	fprintf(stderr,"...sending error to standard error and continuing.\n") ;
	close(newfd) ;
	return ;
    }

}

RSL_ERROR_DUP1 ( int *me )
{
    int newfd ;
    char filename[256] ;

/* redirect standard out*/
    sprintf(filename,"rsl.out.%04d",*me) ;
    if ((newfd = open( filename, O_CREAT | O_WRONLY, 0666 )) < 0 )
    {
        perror("error_dup: cannot open rsl.out.nnnn") ;
        fprintf(stderr,"...sending output to standard output and continuing.\n") ;
        return ;
    }
    if( dup2( newfd, STANDARD_OUTPUT ) < 0 )
    {
        perror("error_dup: dup2 fails to change output descriptor") ;
        fprintf(stderr,"...sending output to standard output and continuing.\n") ;
        close(newfd) ;
        return ;
    }

/* redirect standard error */
    sprintf(filename,"rsl.error.%04d",*me) ;
    if ((newfd = open( filename, O_CREAT | O_WRONLY, 0666 )) < 0 )
    {
        perror("error_dup: cannot open rsl.error.log") ;
        fprintf(stderr,"...sending error to standard error and continuing.\n") ;
        return ;
    }
    if( dup2( newfd, STANDARD_ERROR ) < 0 )
    {
        perror("error_dup: dup2 fails to change error descriptor") ;
        fprintf(stderr,"...sending error to standard error and continuing.\n") ;
        close(newfd) ;
        return ;
    }

}


