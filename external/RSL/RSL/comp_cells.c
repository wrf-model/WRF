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

static rsl_list_t *lp[RSL_MAXDOMAINS] ;


/*@
  RSL_COMPUTE_CELLS - apply a subroutine to all points of a domain

  Synopsis:
  RSL_COMPUTE_CELLS ( d, f )
    INTEGER     d        ! (I) RSL domain descriptor
    EXTERNAL    f        ! (I) Subroutine

  Notes:
  This routine is supported but considered obsolete.
  RSL now provides more integrated means for iterating over decomposed
  domain dimensions --- see LoopMacros.

  RSL_COMPUTE_CELLS calls
  a subroutine for each grid point of the domain.
  By default, the
  cells on each processor are traversed in an M-minor, N-major ordering.
  This ordering can be changed
  using RSL_ORDER.  Use RSL_COMPUTE_MASK to iterate over a subset of a domain.
  The subroutine F takes five 
  integer arguments (see example) that provide local and global indices of
  a point and nest level.  Other model data must be provided to the subroutine
  through common or through a USE statement (Fortran90).

  Example:

$  subroutine F( inest, i, j, ig, jg )
$  integer inest    ! nest level (1 is top)
$  integer i, j     ! index of point in local memory
$  integer ig, jg   ! index of point in global domain
$    -- computation for point --
$  return
$  end

$  external f
$    --
$  call rsl_compute_cells ( d, f )
$    --
$  stop
$  end

BREAKTHEEXAMPLECODE

  The subroutine F is 
  called for a point of the domain d if M evaluates true.

  See also:
  LoopMacros, RSL_ORDER, RSL_COMPUTE_MASK

@*/

/*@
  RSL_COMPUTE_MASK - apply a subroutine to selected points in a domain

  Synopsis:
  RSL_COMPUTE_MASK ( d, f, m )
    INTEGER    d      ! (I) RSL domain descriptor
    EXTERNAL   f      ! (I) Subroutine
    EXTERNAL   m      ! (I) Mask function
    LOGICAL    m

  Notes:
  This routine is supported but considered obsolete.
  RSL now provides more integrated means for iterating over decomposed
  domain dimensions --- see LoopMacros.

  RSL_COMPUTE_MASK calls
  a subroutine for grid points of the domain based on evaluation of a
  mask function.  See also RSL_COMPUTE_CELLS.

  Example:
$  subroutine F( inest, i, j, ig, jg )
$  integer inest    ! nest level (1 is top)
$  integer i, j     ! index of point in local memory
$  integer ig, jg   ! index of point in global domain
$    -- computation for point --
$  return
$  end
$
$  logical function M ( inest, i, j, ig, jg )
$  M = < .true. if included in computation >
$  return
$  end
$
$  external f, m
$  logical  m
$    --
$  call rsl_compute_mask ( d, f, m )
$    --
BREAKTHEEXAMPLECODE

  The subroutine F is 
  called for a point of the domain d if M evaluates true.

  See also:
  LoopMacros, RSL_ORDER, RSL_COMPUTE_MASK

@*/

int RSL_INIT_NEXTCELL ( d_p )
  int_p d_p ;
{
  rsl_index_t d ;

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
  lp[d] = domain_info[d].pts ;
  return(0) ;
}

int RSL_INIT_GHOST ( d_p )
  int_p d_p ;
{
  rsl_index_t d ;

  d = *d_p ;
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "rsl_init_nextcell: invalid domain") ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
     "rsl_init_nextcell: invalid domain") ;
  lp[d] = domain_info[d].ghost_pts ;
  return(0) ;
}

int RSL_C_NEXTCELL ( d_p, min_p, maj_p, min_g_p, maj_g_p, retval_p )
  int_p d_p, min_p, maj_p, min_g_p, maj_g_p, retval_p ;
{
  rsl_index_t d ;
  rsl_point_t *pt ;

  d = *d_p ;
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "rsl_init_nextcell: invalid domain") ;

  if ( lp[d] == NULL )
  {
    *retval_p = 0 ;		/* no more */
  }
  else
  {
    pt = (rsl_point_t *) lp[d]->data ;
    *min_g_p = ID_IDEX( pt->id ) + 1;
    *maj_g_p = ID_JDEX( pt->id ) + 1;
    *min_p = *min_g_p - domain_info[d].ilocaloffset ;
    *maj_p = *maj_g_p - domain_info[d].jlocaloffset ;
#if 0
fprintf(stderr,"%d comp_cells point -> %d %d %d %d; jlocaloffset %d\n",
       rsl_myproc, *min_p, *maj_p, *min_g_p, *maj_g_p,
       domain_info[d].jlocaloffset) ;
#endif
    lp[d] = lp[d]->next ;
    *retval_p = 1 ;
  }
  return(0) ;
}
