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

static rsl_list_t *rp[RSL_MAXDOMAINS] ;

/*@
  RSL_COMPUTE_ISLAB - apply a routine to each islab locally stored points

  Synopsis:
  subroutine RSL_COMPUTE_ISLAB ( d, f )
  integer d
  external f

  Input parameters:
. d - domain descriptor
. f - subroutine to be applied

  Notes:
  This is one of the principal computational routines of RSL.  It applies
  a function that represents some piece of work -- compute a time step
  -- to be done on a horizontal grid point.  RSL_COMPUTE_ISLAB will
  call the subroutine for each continguous run of points local to the
  processor up the minor dimension of the domain.

  The subroutine, f, provided should be slab-callable
  and have six
  integer dummy arguments which are shown in the example below.

  Example:

$  -- prototypical function --
$  subroutine F( inest, irun, i1, j1, ig1, jg1 )
$  integer inest    ! nest level (1 is top)
$  integer irun     ! number of points in the run
$  integer i, j     ! index of starting point in local memory
$  integer ig, jg   ! index of starting point in global domain

$  j = j1
$  ig = ig1-1
$  do i = i1, i1+irun-1
$    ig = ig+1
$    -- computation --
$  enddo

$  return
$  end

$  -- top level routine --

$  external f, m
$  logical  m
$    --
$  call rsl_compute_islab ( d, f )
$    --

BREAKTHEEXAMPLECODE

@*/

/*@
  RSL_INIT_NEXTISLAB - Initialize a traversal over local slabs

  RSL_C_NEXTISLAB - Get indices and run length of next slab in traversal

  Notes:
  These routines are called from within RSL_COMPUTE_ISLAB and are not
  typically used by the application programmer.

@*/
int RSL_INIT_NEXTISLAB ( d_p )
  int_p d_p ;
{
  rsl_index_t d ;

  d = *d_p ;
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "rsl_init_nextislab: bad domain descriptor") ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
     "rsl_init_nextislab: invalid domain descriptor") ;

  rp[d] = domain_info[d].iruns ;
  return(0) ;
}

int RSL_C_NEXTISLAB ( d_p, irun_p, min_p, maj_p, min_g_p, maj_g_p, retval_p )
  int_p d_p, irun_p, min_p, maj_p, min_g_p, maj_g_p, retval_p ;
{
  rsl_index_t d ;
  rsl_runrec_t *rrec ;

  d = *d_p ;
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "rsl_init_nextcell: bad domain") ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
     "rsl_init_nextcell: invalid domain") ;
  if ( domain_info[d].decomposed != 1 )
  {
    default_decomposition( d_p,
                           &(domain_info[*d_p].loc_m),
                           &(domain_info[*d_p].loc_n) ) ;
  }

  if ( rp[d] == NULL )
  {
    *retval_p = 0 ;		/* no more */
  }
  else
  {
    rrec = (rsl_runrec_t *)(rp[d]->data) ;
    *irun_p = rrec->runlength ;
    *min_g_p = rrec->ig + 1;
    *maj_g_p = rrec->jg + 1;
    *min_p = *min_g_p - domain_info[d].ilocaloffset ;
    *maj_p = *maj_g_p - domain_info[d].jlocaloffset ;
    rp[d] = rp[d]->next ;
    *retval_p = 1 ;
  }
  return(0) ;
}

