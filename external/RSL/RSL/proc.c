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

#ifdef FORTRANMANUAL
/*@
  RSL_IAMMONITOR -- Informational routine: am I the monitor?

  Notes:
  This function returns the value .TRUE. on the monitor processor
  (usually node zero), and .FALSE. otherwise.  The monitor node is the
  I/O node and also the root node for broadcasts (using RSL_MON_BCAST)
  and internal reads and writes (RSL_READ, RSL_WRITE).  RSL_IAMMONITOR
  may be called before RSL_MESH has been called.

  Example:
$  IF ( RSL_IAMMONITOR() ) THEN
$    CALL RSL_UOPEN( INPUT_UNIT, FNAME, "OLD" )
$  ENDIF
BREAKTHEEXAMPLECODE
  This shows RSL_IAMMONITOR being used in conjunction with
  the RSL unformatted fortran open routine to do a named
  upon on the monitor processor.

  See also:
  RSL_C_IAMMONITOR, RSL_MESH, RSL_MON_BCAST, RSL_READ, RSL_WRITE
  
@*/
int
RSL_IAMMONITOR ( retval )
  int_p retval ;   /* Return value */
{}
#endif

/*@
  RSL_C_IAMMONITOR -- Informational routine: am I the monitor?

  Notes:
  The integer Arg1 is set to 1 on the monitor processor.  The monitor
  (usually node zero).  The monitor node is the I/O node and also the
  root node for broadcasts
  (using RSL_MON_BCAST) and internal reads and writes (RSL_READ, RSL_WRITE).
  RSL_C_IAMMONITOR may be called before RSL_MESH has been called.

  See also:
  RSL_IAMMONITOR, RSL_MESH, RSL_MON_BCAST, RSL_READ, RSL_WRITE
  
@*/

int
RSL_C_IAMMONITOR ( retval )
  int_p retval ;  /* Return value. */
{
#if ( HOST_NODE == 0 )
#  if ( MON_LOW == 0 )
					/* hostless */
  if ( rsl_myproc == rsl_nproc-1 ) 
    *retval = 1 ;
  else
    *retval = 0 ;
#  else
					/* have a host node */
  if ( rsl_myproc == 0 ) 
    *retval = 1 ;
  else
    *retval = 0 ;
#  endif
#else
#  if ( MON_LOW == 0 )
  if ( rsl_myproc == rsl_nproc )
    *retval = 1 ;
  else
    *retval = 0 ;
#  else
  if ( rsl_myproc == 0 )
    *retval = 1 ;
  else
    *retval = 0 ;
#  endif
#endif
  return(0) ;
}

int
RSL_C_IAMCOMPUTE ( retval )
  int_p retval ;
{
#if ( HOST_NODE == 0 )
#  if ( MON_LOW == 0 )
  if ( rsl_myproc < rsl_nproc-1 ) 
    *retval = 1 ;
  else
    *retval = 0 ;
#  else
  if ( rsl_myproc > 0 )
    *retval = 1 ;
  else
    *retval = 0 ;
#  endif
#else
#  if ( MON_LOW == 0 )
  if ( rsl_myproc < rsl_nproc )
    *retval = 1 ;
  else
    *retval = 0 ;
#  else
  if ( rsl_myproc > 0 )
    *retval = 1 ;
  else
    *retval = 0 ;
#  endif
#endif
  return(0) ;
}

RSL_C_MONITOR_PROC ()
{
#if ( HOST_NODE == 0 )
#  if ( MON_LOW == 0 )
  return(rsl_nproc-1) ; 
#  else
  return(0) ; 
#  endif
#else
#  if ( MON_LOW == 0 )
  return(rsl_nproc) ; 
#  else
  return(0) ; 
#  endif
#endif
}

/* this maps compute processor id (0 based) to physical
   processor id */
int
rsl_c_comp2phys_proc ( P ) 
  int P ;
{
#if ( HOST_NODE == 0 )
#  if ( MON_LOW == 0 )
  return(P) ;
#  else
  return(P) ;
#  endif
#else
#  if ( MON_LOW == 0 )
  return(P) ;     /* monitor is high */
#  else
  return(P+1) ;     /* monitor is low */
#  endif
#endif
}


RSL_COMP2PHYS_C ( P, retval )
  int_p P, retval ;
{
  *retval = rsl_c_comp2phys_proc( *P ) ;
  return ;
}


/* this maps physical processor id to compute proc id
   inverse of rsl_c_comp2phys_proc */

int 
rsl_c_phys2comp_proc( P )
{
#if ( HOST_NODE == 0 )
#  if ( MON_LOW == 0 )
  return(P) ;
#  else
  return(P) ;
#  endif
#else
#  if ( MON_LOW == 0 )
  return(P) ;		/* monitor is high */
#  else
  return(P-1) ;		/* monitor is low */
#  endif
#endif
}

RSL_PHYS2COMP_C ( P, retval )
  int_p P, retval ;
{
  *retval = rsl_c_phys2comp_proc( *P ) ;
  return ;
}


