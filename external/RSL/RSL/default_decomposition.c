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

static struct default_decomp {
   int (*default_decomp_fcn)() ;
} default_decomp[RSL_MAXDOMAINS] ;
static int *def_decomp_info[RSL_MAXDOMAINS] ;

/* this is called from various parts of the code to decompose a
   domain if it has not been decomposed already */
default_decomposition( d_p, mloc_p, nloc_p )
  int_p d_p, mloc_p, nloc_p ;
{ 
  int px, py ;
  int (*f)() ;
  int retval ;
  int zloc_p,
      mloc_mz_p, nloc_mz_p, zloc_mz_p,
      mloc_nz_p, nloc_nz_p, zloc_nz_p ;

  py = rsl_nproc_m ;
  px = rsl_nproc_n ;

  f = default_decomp[*d_p].default_decomp_fcn ;

  retval = RSL_FDECOMPOSE (
          d_p,
	  f,
	  &py,
	  &px,
	  def_decomp_info[*d_p],
	  mloc_p, nloc_p, &zloc_p,
	  &mloc_mz_p, &nloc_mz_p, &zloc_mz_p,
	  &mloc_nz_p, &nloc_nz_p, &zloc_nz_p ) ;

/* these have been added for the parallel matrix transpose, 20010223.
   default_decomp is called many places in RSL, and rather than thread
   all this through, I have opted to just store the additional info
   from teh decomposition algorithma and store it in the domain info */
  domain_info[*d_p].loc_m = *mloc_p ;
  domain_info[*d_p].loc_n = *nloc_p ;
  domain_info[*d_p].loc_z = zloc_p ;
  domain_info[*d_p].loc_mz_m = mloc_mz_p ;
  domain_info[*d_p].loc_mz_n = nloc_mz_p ;
  domain_info[*d_p].loc_mz_z = zloc_mz_p ;
  domain_info[*d_p].loc_nz_m = mloc_nz_p ;
  domain_info[*d_p].loc_nz_n = nloc_nz_p ;
  domain_info[*d_p].loc_nz_z = zloc_nz_p ;

  return(retval) ;
}

/*@
  SET_DEF_DECOMP_FCN --- Replace the default mapping routine.

  This provides a mechanism to replace the mapping function currently
  in effect with a different routine, specified by the functional
  pointer Arg1.  This is the routine that will be called by RSL
  whenever a new domain is created.  The user program can provide
  additional information to the mapping function using 
  SET_DEF_DECOMP_INFO.

  Mapping function:
  The user-supplied function for decomposing the domain should have
  the following form.

  Verbatim:
$ INTEGER FUNCTION MAPPING ( in, out, info, m, n, py, px )
BREAKTHEEXAMPLECODE

  In and out are m by n integer arrays.  Each element of the in array is
  set to RSL_VALID for valid points in the domain or RSL_INVALID if the
  point is not considered to be part of the domain and therefore not
  to be allocated to a processor (as might be the case if extra memory
  is allocated but not used).  The function generates an out array
  such that every point that was RSL_VALID in the in array is given
  a processor number between 0 (zero) and py times px minus 1.  Integers
  m and n are the in and out array dimensions; integer py is the number
  of processors decomposing m; integer px is the number of processors
  decomposing n.

  See also:
  SET_DEF_DECOMP_INFO

@*/
SET_DEF_DECOMP_FCN ( f )
  int 
    (*f)() ;        /* (I) Function to use as default decomposition. */
{
  int i ;

  for ( i = 0 ; i < RSL_MAXDOMAINS ; i++ )
    default_decomp[i].default_decomp_fcn = f ;
}

SET_DEF_DECOMP_FCN1 ( d_p, f )
  int_p d_p ;
  int
    (*f)() ;        /* (I) Function to use as default decomposition. */
{
  int i ;
  int d ;

  d = *d_p ;
  RSL_TEST_ERR(d < 0 || d >= RSL_MAXDOMAINS,
          "rsl_compile_stencil: bad domain descriptor" ) ;
  default_decomp[*d_p].default_decomp_fcn = f ;
}


/*@
  SET_DEF_DECOMP_INFO --- Provide data to RSL to pass to the mapping routine.

  Notes:
  When RSL calls the default mapping function, the data provided as Arg2
  will be passed as the third argument to the function.  This provides
  mechanism for moving data such as timing information from the user program
  to the mapping routine for use in calculating the domain decomposition.

  See also:
  SET_DEF_DECOMP_FCN


@*/
SET_DEF_DECOMP_INFO ( d_p, inf )
  int_p 
      d_p ;    /* (I) RSL domain descriptor */
  void *
      inf ;    /* (I) Pointer to memory that will be passed as-is to the decomposition routine */
{
  def_decomp_info[*d_p] = (int_p) inf ;
}


