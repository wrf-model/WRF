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

#define SOUTH RSL_MLOW
#define NORTH RSL_MHIGH
#define WEST  RSL_NLOW
#define EAST  RSL_NHIGH

static int debuggal_mine = 0 ;

/*@
  RSL_MOTHER_DOMAIN - Define the topmost domain.

  Notes:
  This routine defines the principal, top-level domain of the model.

  As input, the routine takes Arg2, a specifier for the largest stencil that
  will be used.  It may be one of
  
  Verbatim:
  
$    RSL_4PT,
$    RSL_8PT,
$    RSL_12PT,
$    RSL_24PT, or
$    RSL_168PT.

BREAKTHEEXAMPLECODE
  It also takes Arg3 and Arg4, the
  logical (undecomposed) dimensions of the domain, m and n.

  Output is Arg1, a domain descriptor, a
  handle to RSL's definition of the domain, and Arg5 and Arg6,
  minimum local sizes
  for array dimensions on the local processor.  These values can be used
  to allocate local arrays in models that use dynamic memory allocation.
  Models that allocate memory
  statically may use these values as a check to be sure that enough
  memory has been allocated.

  RSL must have been initialized and a processor mesh specified
  before a domain is defined.

  Example:

$  INCLUDE 'rsl.inc'
$  INTEGER D
$  INTEGER MLEN, NLEN
$  --
$  CALL RSL_INITIALIZE
$  CALL RSL_MESH(2, 2)
$  CALL RSL_MOTHER_DOMAIN(D,RSL_8PT,25,30,MLEN,NLEN)

BREAKTHEEXAMPLECODE
  This declares a 25 by 30 element domain.
  The
  integer d contains a domain descriptor, which will
  be used in all subsequent operations involving the domain.

  See also:
  RSL_INITIALIZE, RSL_SPAWN_REGULAR_NEST

@*/
int
RSL_MOTHER_DOMAIN ( domain_p, maskid_p, mlen_p, nlen_p, mloc_p, nloc_p )
  int_p
    domain_p        /* (O)  Domain id. */
   ,maskid_p        /* (I)  Id of maximum stencil for this domain. */
   ,mlen_p          /* (I)  Number of cells in m dimension of domain. */
   ,nlen_p          /* (I)  Number of cells in n dimension of domain. */
   ,mloc_p          /* (O)  Required size of local memory in m. */
   ,nloc_p          /* (O)  Required size of local memory in n. */
    ;
{
  rsl_index_t i, j, k ;
  int  d ;
  rsl_dimlen_t nmax ;
  rsl_dimlen_t mmax ;
  rsl_dimlen_t zmax ;
  int mmin, nmin, mtrim, ntrim ;

  mmax = *mlen_p ;
  nmax = *nlen_p ;
  zmax = 1 ;  /* trivial */
  GET_NEXT_DOMAIN_DESCRIPTOR ( &d ) ; ;
  *domain_p = d ;

  RSL_TEST_ERR( domain_info[d].valid == RSL_VALID,
     "rsl_mother_domain() called more than once" ) ;

  domain_info[d].nest_level = 0 ;
  domain_info[d].maskid     = *maskid_p ;
  domain_info[d].trim_m     = 0 ;
  domain_info[d].trim_n     = 0 ;

  rsl_c_initialize_domain( d, nmax, mmax, zmax, nmax, mmax, zmax ) ;

  mtrim = domain_info[d].trim_m ;
  ntrim = domain_info[d].trim_n ;

  for ( j = 0 ; j < nmax ; j++ ) for ( i = 0 ; i < mmax ; i++ )
    { /* all points valid on mother domain -- always rectangular */
      domain_info[d].domain[INDEX_2(j,i,mmax)].valid = RSL_VALID ;
      domain_info[d].domain[INDEX_2(j,i,mmax)].trimmed = 0 ; }

  for ( k = 0 ; k < zmax ; k++ ) for ( i = 0 ; i < mmax ; i++ )
    { /* all points valid on mother domain -- always rectangular */
      domain_info[d].domain_mz[INDEX_2(k,i,mmax)].valid = RSL_VALID ; }

  for ( k = 0 ; k < zmax ; k++ ) for ( j = 0 ; j < nmax ; j++ )
    { /* all points valid on mother domain -- always rectangular */
      domain_info[d].domain_nz[INDEX_2(k,j,nmax)].valid = RSL_VALID ; }

  trim_domain( domain_info[d].domain, mmax, nmax, 0, 0 ) ;

  work_out_bdy( domain_info[d].domain, mmax, nmax ) ;

  {
/* moved here, 4/21/95 --- see comment in spawn nest below */
    int dd ;
    dd = d ;
    default_decomposition( &dd,
                           mloc_p,
                           nloc_p ) ;
  }

  rsl_ndomains = 1 ;
  return(0) ;
}

/* (20010222) */
RSL_MOTHER_DOMAIN3D( domain_p, maskid_p, mlen_p, nlen_p, zlen_p, 
                                         mloc_p, nloc_p, zloc_p,
                                         mloc_mz_p, nloc_mz_p , zloc_mz_p,
                                         mloc_nz_p, nloc_nz_p , zloc_nz_p )
  int_p
    domain_p        /* (O)  Domain id. */
   ,maskid_p        /* (I)  Id of maximum stencil for this domain. */
   ,mlen_p          /* (I)  Number of cells in m dimension of domain. */
   ,nlen_p          /* (I)  Number of cells in n dimension of domain. */
   ,zlen_p          /* (I)  Number of cells in z dimension of domain. */
   ,mloc_p          /* (O)  Required size of local memory in m. (MN decomp) */
   ,nloc_p          /* (O)  Required size of local memory in n.      "      */
   ,zloc_p          /* (O)  Required size of local memory in z.      "      */
   ,mloc_mz_p       /* (O)  Required size of local memory in m. (MZ decomp) */
   ,nloc_mz_p       /* (O)  Required size of local memory in n.      "      */
   ,zloc_mz_p       /* (O)  Required size of local memory in z.      "      */
   ,mloc_nz_p       /* (O)  Required size of local memory in m. (NZ decomp) */
   ,nloc_nz_p       /* (O)  Required size of local memory in n.      "      */
   ,zloc_nz_p       /* (O)  Required size of local memory in z.      "      */
    ;
{

  rsl_index_t i, j, k ;
  int  d ;
  rsl_dimlen_t nmax ;
  rsl_dimlen_t mmax ;
  rsl_dimlen_t zmax ;
  int mmin, nmin, mtrim, ntrim ;

  mmax = *mlen_p ;
  nmax = *nlen_p ;
  zmax = *zlen_p ;
  GET_NEXT_DOMAIN_DESCRIPTOR ( &d ) ;
  *domain_p = d ;

  RSL_TEST_ERR( domain_info[d].valid == RSL_VALID,
     "rsl_mother_domain() called more than once" ) ;

  if ( domain_info[d].domain_mz != NULL ) { RSL_FREE( domain_info[d].domain_mz ) ; }
  if ( domain_info[d].domain_nz != NULL ) { RSL_FREE( domain_info[d].domain_nz ) ; }


  domain_info[d].nest_level = 0 ;
  domain_info[d].maskid     = *maskid_p ;
  domain_info[d].trim_m     = 0 ;
  domain_info[d].trim_n     = 0 ;

  rsl_c_initialize_domain( d, nmax, mmax, zmax, nmax, mmax, zmax ) ;

  mtrim = domain_info[d].trim_m ;
  ntrim = domain_info[d].trim_n ;

  for ( j = 0 ; j < nmax ; j++ ) for ( i = 0 ; i < mmax ; i++ )
    { /* all points valid on mother domain -- always rectangular */
      domain_info[d].domain[INDEX_2(j,i,mmax)].valid = RSL_VALID ;
      domain_info[d].domain[INDEX_2(j,i,mmax)].trimmed = 0 ; }

  for ( k = 0 ; k < zmax ; k++ ) for ( i = 0 ; i < mmax ; i++ )
    { /* all points valid on mother domain -- always rectangular */
      domain_info[d].domain_mz[INDEX_2(k,i,mmax)].valid = RSL_VALID ; }

  for ( k = 0 ; k < zmax ; k++ ) for ( j = 0 ; j < nmax ; j++ )
    { /* all points valid on mother domain -- always rectangular */
      domain_info[d].domain_nz[INDEX_2(k,j,nmax)].valid = RSL_VALID ; }

  trim_domain( domain_info[d].domain, mmax, nmax, 0, 0 ) ;

  work_out_bdy( domain_info[d].domain, mmax, nmax ) ;

  {
/* moved here, 4/21/95 --- see comment in spawn nest below */
    int dd ;
    dd = d ;
    default_decomposition( &dd,
                           mloc_p,
                           nloc_p ) ;
  }

/* these are set by the revised default_decomposition routine (20010223) */

  *zloc_p    = domain_info[d].loc_z ;
  *mloc_mz_p = domain_info[d].loc_mz_m ;
  *nloc_mz_p = domain_info[d].loc_mz_n ;
  *zloc_mz_p = domain_info[d].loc_mz_z ;
  *mloc_nz_p = domain_info[d].loc_nz_m ;
  *nloc_nz_p = domain_info[d].loc_nz_n ;
  *zloc_nz_p = domain_info[d].loc_nz_z ;

  rsl_ndomains = 1 ;

}

/*
   rsl_c_initialize_domain

   Initializes a domain descriptor.

   For a mother domain the actual and effective sizes will be the same.

   For a nest, they may differ.   This is because the nest must always
   be a multiple of IRAX wide, but may not compute over all of that.

*/
int
rsl_c_initialize_domain ( d, nmax, mmax, zmax, neff, meff, zeff )
  rsl_index_t d ;
  rsl_dimlen_t nmax, mmax, zmax ;   /* actual size definition */
  rsl_dimlen_t neff, meff, zeff ;   /* effective size definition (for computing) */
{
  rsl_index_t i, j, k ;
  rsl_point_t *p ;

#if 0
  fprintf(stderr,"1> rsl_c_initialize_domain d = %d\n",d ) ;
  fprintf(stderr,"                    nmax     = %d\n",nmax ) ;
  fprintf(stderr,"                    mmax     = %d\n",mmax ) ;
  fprintf(stderr,"                    neff     = %d\n",neff ) ;
  fprintf(stderr,"                    meff     = %d\n",meff ) ;
#endif

  domain_info[d].pts = NULL ;
  domain_info[d].ghost_pts = NULL ;
  domain_info[d].iruns = NULL ;
  domain_info[d].len_n = nmax ;
  domain_info[d].len_m = mmax ;
  domain_info[d].len_z = zmax ;
  domain_info[d].eff_n = neff ;
  domain_info[d].eff_m = meff ;
  domain_info[d].eff_z = zeff ;
  domain_info[d].coord_n = RSL_INVALID ;
  domain_info[d].coord_m = RSL_INVALID ;

#if 0
  fprintf(stderr,"2> rsl_c_initialize_domain d = %d\n",d ) ;
  fprintf(stderr,"                    nmax     = %d\n",nmax ) ;
  fprintf(stderr,"                    mmax     = %d\n",mmax ) ;
  fprintf(stderr,"                    zmax     = %d\n",zmax ) ;
  fprintf(stderr,"                    neff     = %d\n",neff ) ;
  fprintf(stderr,"                    meff     = %d\n",meff ) ;
#endif

  if ( domain_info[d].domain != NULL )    { RSL_FREE( domain_info[d].domain ) ; }
  if ( domain_info[d].domain_mz != NULL ) { RSL_FREE( domain_info[d].domain_mz ) ; }
  if ( domain_info[d].domain_nz != NULL ) { RSL_FREE( domain_info[d].domain_nz ) ; }

  domain_info[d].domain    = RSL_MALLOC(rsl_point_t,(nmax*mmax)) ;
  domain_info[d].domain_nz = RSL_MALLOC(rsl_point_t,(nmax*zmax)) ;
  domain_info[d].domain_mz = RSL_MALLOC(rsl_point_t,(mmax*zmax)) ;

  domain_info[d].valid = RSL_VALID ;
  domain_info[d].decomposed = 0 ;
  domain_info[d].trim_m = 0 ;
  domain_info[d].trim_n = 0 ;

  for ( j = 0 ; j < nmax ; j++ )
  {
    for ( i = 0 ; i < mmax ; i++ )
    {
  
      p = &(domain_info[d].domain[ INDEX_2(j,i,mmax) ]) ;
      p->dbdy = RSL_INVALID ;
      p->P           = RSL_INVALID ;
      p->valid       = RSL_INVALID ;
      p->trimmed     = RSL_INVALID ;
      p->id          = POINTID( d, j, i ) ;
      p->info_1      = RSL_INVALID ;
      p->info_2      = RSL_INVALID ;
      p->mother_id   = RSL_INVALID ;
      p->which_kid_am_i_m   = RSL_INVALID ;
      p->which_kid_am_i_n   = RSL_INVALID ;
      if ( p->children_p != NULL ) 
      {
        RSL_FREE( p->children_p ) ;
      }
      p->dbdy = RSL_INVALID ;
      p->bdy_cclockwise = RSL_INVALID ;
      p->bdy_clockwise = RSL_INVALID ;
      p->dbdy_x = RSL_INVALID ;
      p->bdy_x_cclockwise = RSL_INVALID ;
      p->bdy_x_clockwise = RSL_INVALID ;
    }
  }


  for ( i = 0 ; i < RSL_MAXDESCRIPTORS ; i++ )
  {
    domain_info[d].stenlist[i] = RSL_INVALID ;
  }
  for ( i = 0 ; i < RSL_MAXDOMAINS ; i++ )
  {
    domain_info[d].child_bcast_compiled[i] = 0 ;
    domain_info[d].child_merge_compiled[i] = 0 ;
  }
  domain_info[d].parent_bcast_compiled = 0 ;
  domain_info[d].parent_merge_compiled = 0 ;
  domain_info[d].stencurs = 0 ;
  domain_info[d].periodcurs = 0 ;
  domain_info[d].xposecurs = 0 ;

  return(0);
}

/*@
  RSL_SPAWN_REGULAR_NEST - Spawn a domain using coarse domain dimensions. 

  Notes:
  This routine spawns a rectangular nest in an existing domain whose
  descriptor is provided as Arg2.  The newly defined domain descriptor
  is returned as Arg1.

  The argument Arg3 specifies the maximum stencil that will be used on
  the domain and may be one of

  Verbatim:
$    RSL_4PT,
$    RSL_8PT,
$    RSL_12PT,
$    RSL_24PT, or
$    RSL_168PT.

BREAKTHEEXAMPLECODE

  These are defined in the file rsl.inc.
  For efficiency, choose the smallest.  Arg4 and Arg5 are the coordinates
  of the south-west corner of the nest in the parent domain.  The
  arguments Arg6 and Arg7 are the dimensions of the parent domain area
  under which the nest is being defined.  In other words, the nest
  dimensions are specified in parent domain units
  (RSL_SPAWN_REGULAR_NEST1 allows specification of the nest dimensions
  in nest units).  The Arg8 and Arg9 are the nesting ratios in M and N
  respectively; these are the number of nest cells per parent cell in
  each dimension (e.g. 3, as in MM5.) The Arg10 and Arg11 arguments are
  trim arguments for adjusting a logical (undecomposed) dimension of
  the nest from other than a multiple of the number of parent domain
  cells in the dimension.

  On return, Arg12 and Arg13 are set to the minimum local dimensions that
  will be required for model arrays on the processor.  This also includes
  additional memory for ghost points around the local processor partition
  (the amount allocated will depend on the stencil specified by Arg3).
  The Arg14 and Arg15 arguments will be set to the effective undecomposed
  dimensions, after trimming.

  Example:

$   IPOS = 15 ; JPOS = 20     ! position of nest sw corner
$   IDIM = 7  ; JDIM = 10     ! dimensions of nested region in parent
$   BTRIM = 2                 ! trimming for MM5 nesting w/ Arakawa B-grid
$   IRAX = 3                  ! nesting ratio
$   CALL RSL_SPAWN_REGULAR_NEST( NEST%RSL_ID, PARENT%RSL_ID, RSL_24PT,
$  +                             IPOS, JPOS, IDIM, JDIM,
$  +                             IRAX, IRAX,
$  +                             BTRIM, BTRIM,
$  +                             MLOC, NLOC, M, N )
$ C
$ C Use size info returned by RSL to allocate 2-d and 3-d domain data
$ C structures for nest.
$ C
$   NLEV = 25                 ! number vert. levels
$   ALLOCATE( NEST\%UA(MLOC,NLOC,NLEV) )   ! ew wind
$   ALLOCATE( NEST\%VA(MLOC,NLOC,NLEV) )   ! ns wind
$   ALLOCATE( NEST\%TA(MLOC,NLOC,NLEV) )   ! ns wind
$   ALLOCATE( NEST\%PS(MLOC,NLOC) )        ! surface pres
$   ...

BREAKTHEEXAMPLECODE

  A rectangular nest is spawned within a 7-cell by 10-cell region of a
  parent domain.  The southwest corner of the nest is positioned under
  parent cell (15,20).  A 24-point stencil is the largest that will be used
  on the nest.  There are 3 nest cells per parent cell in both
  horizontal dimensions; thus the total nest size is 21 by 30 cells.
  However, we wish to follow a rule (used in MM5) that nest dimensions
  must always be one greater than a multiple of the nesting ratio to
  accounts for the staggering of dot and cross grids on the parent and
  nest.  Therefore, we trim two points from the northern and eastern
  edges of the nest; the result is effective nest dimensions of 19 by
  28, the values returned in m and n respectively.  In the example,
  return values mloc and nloc are used in subsequent Fortran90 allocate
  statements to size the local state arrays for the nest.

  See also:
  RSL_INITIALIZE, RSL_MOTHER_DOMAIN, RSL_SPAWN_REGULAR_NEST1

@*/

RSL_SPAWN_REGULAR_NEST ( n_p, d_p, maskid_p, sw_min_p,  sw_maj_p, dim_min_p, dim_maj_p, irax_m_p,  irax_n_p, mtrim_p, ntrim_p, mloc_p, nloc_p, mlen_p, nlen_p )
  int_p 
    n_p             /* (O) Handle to nested domain. */
   ,d_p             /* (I) Handle to parent domain. */
   ,maskid_p        /* (I) Id of maximum stencil for this domain. */
   ,sw_min_p        /* (I) M coord in mother domain of sw of nest.*/
   ,sw_maj_p        /* (I) N coord in mother domain of sw of nest.*/
   ,dim_min_p       /* (I) M dimension of nest.  */
   ,dim_maj_p       /* (I) N dimension of nest.  */
   ,mtrim_p         /* (I) M trim. */
   ,ntrim_p         /* (I) N trip. */
   ,irax_m_p        /* (I) Nesting ratio in M dimension. */
   ,irax_n_p ;      /* (I) Nesting ration in n dimension. */
  int_p
    mloc_p          /* (O) Minimum local array size in M. */
   ,nloc_p ;        /* (O) Minimum local array size in N. */
  int_p
    mlen_p          /* (O) Undecomposed size of M. */
   ,nlen_p ;        /* (O) Undecomposed size of N. */
{
rsl_c_spawn_regnest( n_p, d_p, maskid_p, sw_min_p, sw_maj_p,
                     dim_min_p, dim_maj_p,
                     irax_m_p, irax_n_p,
		     mtrim_p, ntrim_p,
                     mloc_p, nloc_p,
                     mlen_p, nlen_p, 1 ) ;
}

/*@
  RSL_SPAWN_REGULAR_NEST1 - Spawn a domain using nested domain dimensions. 

  Notes:
  This routine spawns a rectangular nest in an existing domain whose
  descriptor is provided as Arg2.  The newly defined domain descriptor
  is returned as Arg1.

  The argument Arg3 specifies the maximum stencil that will be used on
  the domain and may be one of

  Verbatim:
$    RSL_4PT,
$    RSL_8PT,
$    RSL_12PT,
$    RSL_24PT, or
$    RSL_168PT.

BREAKTHEEXAMPLECODE

  These are defined in the file rsl.inc.
  For efficiency, choose the smallest.
  Arg4 and Arg5 are the coordinates of the south-west corner of the nest in
  the parent domain.
  The arguments Arg6 and Arg7 are the dimensions of the nest.
  (RSL_SPAWN_REGULAR_NEST allows
  specification of the nest dimensions in terms of the parent grid).
  The Arg8 and Arg9 are the nesting ratios in M and N respectively; these
  are the number of nest cells per parent cell in each dimension (e.g. 3, as
  in MM5.)

  On return, Arg10 and Arg11 are set to the minimum local dimensions that
  will be required for model arrays on the processor.  This also includes
  additional memory for ghost points around the local processor partition
  (the amount allocated will depend on the stencil specified by Arg3).
  The Arg12 and Arg13 arguments will be set to the effective undecomposed
  dimensions.

  Example:

$   IPOS = 15 ; JPOS = 20     ! position of nest sw corner
$   IDIM = 19 ; JDIM = 28     ! dimensions of nest
$   IRAX = 3                  ! nesting ratio
$   CALL RSL_SPAWN_REGULAR_NEST1( NEST%RSL_ID, PARENT%RSL_ID, RSL_24PT,
$  +                              IPOS, JPOS, IDIM, JDIM,
$  +                              IRAX, IRAX,
$  +                              MLOC, NLOC, M, N )
$ C
$ C Use size info returned by RSL to allocate 2-d and 3-d domain data
$ C structures for nest.
$ C
$   NLEV = 25                 ! number vert. levels
$   ALLOCATE( NEST\%UA(MLOC,NLOC,NLEV) )   ! ew wind
$   ALLOCATE( NEST\%VA(MLOC,NLOC,NLEV) )   ! ns wind
$   ALLOCATE( NEST\%TA(MLOC,NLOC,NLEV) )   ! ns wind
$   ALLOCATE( NEST\%PS(MLOC,NLOC) )        ! surface pres
$   ...

BREAKTHEEXAMPLECODE

  A rectangular 19 by 28 cell nest is spawned an located with its
  southwest corner at point (15,20) in the parent domain.  A 24-point
  stencil is the largest that will be used on the nest.  There are 3
  nest cells per parent cell in both horizontal dimensions.  In the
  example, return values mloc and nloc are used in subsequent Fortran90
  allocate statements to size the local state arrays for the nest.

  See also:
  RSL_INITIALIZE, RSL_MOTHER_DOMAIN, RSL_SPAWN_REGULAR_NEST

@*/
RSL_SPAWN_REGULAR_NEST1 ( n_p, d_p, maskid_p,
                          sw_min_p,  sw_maj_p,
                          dim_min_p, dim_maj_p,
                          irax_m_p,  irax_n_p,
                          mloc_p, nloc_p, mlen_p, nlen_p ) 
  int_p 
    n_p,            /* (O) Handle to nested domain. */
    d_p,            /* (I) Handle to parent domain. */
    maskid_p,       /* (I) Id of maximum stencil for this domain. */
    sw_min_p,       /* (I) M coord in mother domain of sw of nest. */
    sw_maj_p,       /* (I) N coord in mother domain of sw of nest. */
    dim_min_p,      /* (I) M dimension of nest. */
    dim_maj_p,      /* (I) N dimension of nest. */
    irax_m_p,       /* (I) Nesting ratio in M dimension.*/
    irax_n_p ;      /* (I) Nesting ratio in N dimension.*/
  int_p
    mloc_p,         /* (O) Minimum local array size in M. */
    nloc_p ;        /* (O) Minimum local array size in N. */
  int_p
    mlen_p,         /* (O) Undecomposed size of M. */
    nlen_p ;        /* (O) Undecomposed size of N. */
{
int dummy ;
rsl_c_spawn_regnest( n_p, d_p, maskid_p, sw_min_p, sw_maj_p,
                     dim_min_p, dim_maj_p,
                     irax_m_p, irax_n_p,
		     &dummy, &dummy,	/* not needed when using n.d. coords */
                     mloc_p, nloc_p,  
                     mlen_p, nlen_p, 0 ) ;
}

rsl_c_spawn_regnest( n_p, d_p, maskid_p,
                     sw_min_p, sw_maj_p,
                     dim_min_p, dim_maj_p,
                     irax_m_p, irax_n_p,
		     mtrim_p, ntrim_p,
                     mloc_p, nloc_p, 
                     mlen_p, nlen_p, sw )
  int_p n_p,            /* handle to nested domain */
        d_p,            /* handle to mother domain */
        maskid_p,       /* Input.   Id of maximum stencil for this domain. */
        sw_min_p,       /* coordinates in mother domain of sw corner of nest */
        sw_maj_p,       /* <MM: sw_min is i (n/s), sw_maj is j (e/w) */
        dim_min_p,
        dim_maj_p,
	mtrim_p, ntrim_p,
        irax_m_p,       /* nesting ratio in m dimension */
        irax_n_p ;      /* nesting ration in n dimension */
  int   sw ;            /* switch: if 1, dims are specified in c.d. coords
                           if 0, dims are specified in n.d. coords. */
  int_p mloc_p, nloc_p ;/* local memory needs */
  int_p mlen_p, nlen_p ;/* resulting effective global dimensions of nest */
{
  int c ;
  rsl_index_t i, j, in, jn ;
  rsl_index_t d ;               /* number of mother domain */
  rsl_index_t nest ;            /* number of nest domain */
  rsl_index_t im,ime ;  /* coordinate of first/last in mother domain */
  rsl_index_t jm,jme ;  /* coordinate of first/last in mother domain */
  rsl_dimlen_t len_n ;  /* mother domain length of nested dimension */
  rsl_dimlen_t len_m ;  /* mother domain length of nested dimension */
  rsl_dimlen_t nlen_n ; /* number of cells in nested dimension */
  rsl_dimlen_t nlen_m ; /* number of cells in nested dimension */
  rsl_dimlen_t elen_n ; /* effective number of cells in nested dimension */
  rsl_dimlen_t elen_m ; /* effective number of cells in nested dimension */
  int irax_m, irax_n ;
  int mtrim, ntrim ;

  int xlist[4], ylist[4] ;      /* point lists */
  int nlistpoints ;

  d = *d_p ;
  im = *sw_min_p - 1 ;  /* note conversion to C coords (0..n-1) */
  jm = *sw_maj_p - 1 ;
  irax_m = *irax_m_p ;
  irax_n = *irax_n_p ;
  mtrim = *mtrim_p ;	/* this is only used if sw == 1 */
  ntrim = *ntrim_p ;	/* this is only used if sw == 1 */

  if ( sw == 0 )        /* dims are specified in n.d. coords. */
  {
    elen_m = *dim_min_p ;
    elen_n = *dim_maj_p ;
    len_m = (elen_m+irax_m-1) / irax_m ;
    len_n = (elen_n+irax_n-1) / irax_n ;
    nlen_m = len_m*irax_m ;
    nlen_n = len_n*irax_n ;
    ime = im + len_m - 1 ;
    jme = jm + len_n - 1 ;

    mtrim = ((len_m*irax_m-elen_m)>0)?(len_m*irax_m-elen_m):0 ;
    ntrim = ((len_n*irax_n-elen_n)>0)?(len_n*irax_n-elen_n):0 ;

  }
  else          /* default: dims are specified in c.d. coords */
  {
    len_m = *dim_min_p ;
    len_n = *dim_maj_p ;
    nlen_m = len_m * irax_m ;
    nlen_n = len_n * irax_n ;
    elen_m = nlen_m ;
    elen_n = nlen_n ;
    ime = im + len_m - 1 ;
    jme = jm + len_n - 1 ;
  }

  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS, "invalid domain index") ;
  RSL_TEST_ERR( im < 0 || im >= domain_info[d].len_m || 
                jm < 0 || jm >= domain_info[d].len_n || 
                ime < 0 || ime>= domain_info[d].len_m || 
                jme< 0 || jme>= domain_info[d].len_n , 
                     "nested domain won't fit in mother domain" ) ;

  /* set the four corners of the nest in c.d. coordinates */
  xlist[0] = jm   ; ylist[0] = im  ;
  xlist[1] = jm   ; ylist[1] = ime ;
  xlist[2] = jme  ; ylist[2] = ime ;
  xlist[3] = jme  ; ylist[3] = im  ;
  nlistpoints = 4 ;

  rsl_spawn_nest ( n_p, d_p, maskid_p, xlist, ylist, &nlistpoints,
                   &mtrim, &ntrim,
                   irax_m_p, irax_n_p,
                   mloc_p, nloc_p, mlen_p, nlen_p ) ;

  domain_info[*n_p].nestshape = RSL_REGULAR_NEST ;
  domain_info[*n_p].eff_m = elen_m ;
  domain_info[*n_p].eff_n = elen_n ;
  domain_info[*n_p].trim_m = mtrim ;
  domain_info[*n_p].trim_n = ntrim ;

}

/*@
  RSL_SPAWN_IRREG_NEST - Spawn a non-rectangular nest.

  Notes:
  This routine spawns an irregularly shaped nest in an existing domain whose
  descriptor is provided as Arg2.  The newly defined domain descriptor
  is returned as Arg1.

  The argument Arg3 specifies the maximum stencil that will be used on
  the domain and may be one of

  Verbatim:
$    RSL_4PT,
$    RSL_8PT,
$    RSL_12PT,
$    RSL_24PT, or
$    RSL_168PT.

BREAKTHEEXAMPLECODE

  These are defined in the file rsl.inc.
  that will be used on the domain.  For efficiency, choose the smallest.

  The shape of the irregular nest is specified by listing the nodes (corners)
  of the polygon.  The corner vertices are specified by their parent domain
  coordinates.  The M coordinates of the vertices are given by the array Arg4.
  the N coordinates are given by Arg5.  Arg6 gives the number of vertices.

  The Arg7 and Arg8 are the nesting ratios in M and N respectively; these
  are the number of nest cells per parent cell in each dimension (e.g. 3
  in MM5.)
  The Arg9 and Arg10 arguments are trim arguments for adjusting a
  logical (undecomposed) dimension of the nest from other than a multiple
  of the number of parent domain cells in the dimension.

  On return, Arg11 and Arg12 are set to the minimum local dimensions that
  will be required for model arrays on the processor.  This is always
  rectangular, even though the local processor partition may not be.
  This also includes
  additional memory for ghost points around the local processor partition
  (the amount allocated will depend on the stencil specified by Arg3).
  The Arg13 and Arg14 arguments will be set to the effective undecomposed
  dimensions of the rectangle enclosing the irregularly shaped nest.

  Example:

$ C
$ C Irregular nest outline specified by listing the vertices
$ C of the polygonal domain.  These are parent domain coordinates.
$ C
$   ILIST( 1) = 15 ; JLIST( 1) =        18
$   ILIST( 2) = 10 ; JLIST( 2) =        20
$   ILIST( 3) = 10 ; JLIST( 3) =        27
$   ILIST( 4) = 15 ; JLIST( 4) =        27
$   ILIST( 5) = 20 ; JLIST( 5) =        32
$   ILIST( 6) = 20 ; JLIST( 6) =        54
$   ILIST( 7) = 39 ; JLIST( 7) =        54
$   ILIST( 8) = 39 ; JLIST( 8) =        31
$   ILIST( 9) = 26 ; JLIST( 9) =        18
$   NVX = 9                   ! Number of vertices in polygon.
$   BTRIM = 2                 ! trimming for MM5 nesting w/ Arakawa B-grid
$   IRAX = 3                  ! nesting ratio
$ CALL RSL_SPAWN_IRREG_NEST(
$ +                NEST%RSL_ID, PARENT%RSL_ID,
$ +                RSL_24PT,
$ +                ILIST, JLIST, NVX,
$ +                IRAX, IRAX,
$ +                BTRIM, BTRIM,
$ +                MLOC, NLOC,
$ +                M,    N      )

BREAKTHEEXAMPLECODE

  A polygonal region of nesting is specified in the parent domain
  by listing the coordinates of the vertices.  As with the other
  nest spawning routines in RSL, MLOC and
  NLOC are set to the minimum sizes of data arrays to hold the local
  processor's allocation of the domain.  M and N are the global
  dimensions (in nest coordiates) of the bounding box for the
  irregularly shaped nest.


  See also:
  RSL_INITIALIZE, RSL_SPAWN_REGULAR_NEST

@*/
RSL_SPAWN_IRREG_NEST ( n_p, d_p, maskid_p,
                       ypoints0, xpoints0, npoints_p,
                       irax_m_p, irax_n_p,
                       mtrim_p, ntrim_p,
                       mloc_p, nloc_p,
		       mlen_p, nlen_p )
  int_p
    n_p             /* (O) Handle to nested domain. */
   ,d_p             /* (I) Handle to parent domain. */
   ,maskid_p ;      /* (I) Id of maximum stencil for this domain. */
  int
    xpoints0[]       /* (I) List of M coordinates of nest shape vertices. */
   ,ypoints0[] ;     /* (I) List of N coordinates of nest shape vertices. */
  int_p
    npoints_p       /* (I) Number of vertices in nest shape polygon. */
   ,mtrim_p         /* (I) Number of points to trim from M dimension. */
   ,ntrim_p         /* (I) Number of points to trim from N dimension. */
   ,irax_m_p        /* (I) Nesting ratio in M dimension. */
   ,irax_n_p        /* (I) Nesting ratio in N dimension. */
   ,mloc_p          /* (O) Minimum local array size in M. */
   ,nloc_p ;        /* (O) Minimum local array size in N. */
  int_p
    mlen_p          /* (O) Undecomposed size in M of enclosing rectangle. */
   ,nlen_p ;        /* (O) Undecomposed size in N of enclosing rectangle. */
{
  int c ;
  rsl_index_t i, j, in, jn ;
  rsl_index_t d ;               /* number of mother domain */
  rsl_index_t nest ;            /* number of nest domain */
  int irax_m, irax_n ;
  int xpoints[1024], ypoints[1024] ;

  d = *d_p ;
  irax_m = *irax_m_p ;
  irax_n = *irax_n_p ;

  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS, "invalid domain index") ;
  RSL_TEST_ERR( *npoints_p >= 1024,
    "internal error; see routine and recompile with larger arrays") ;

  /* convert fortran point specs to C (zero based) */

  for ( i = 0 ; i < *npoints_p ; i++ )
  {
     xpoints[i] = xpoints0[i] - 1 ;
     ypoints[i] = ypoints0[i] - 1 ;
  }

  rsl_spawn_nest ( n_p, d_p, maskid_p, xpoints, ypoints, npoints_p,
                   mtrim_p, ntrim_p,
                   irax_m_p, irax_n_p, mloc_p, nloc_p, mlen_p, nlen_p ) ;

  domain_info[*n_p].nestshape = RSL_RAGGED_NEST ;
  domain_info[*n_p].trim_m = *mtrim_p ;
  domain_info[*n_p].trim_n = *ntrim_p ;

}

rsl_spawn_nest ( nest_p, par_p, maskid_p,
                 xpoints, ypoints, npoints_p,
                 mtrim_p, ntrim_p,
                 irax_m_p, irax_n_p,
                 mloc_p, nloc_p, mlen_p, nlen_p )
  int_p 
    nest_p,
    par_p,
    maskid_p,
    xpoints,
    ypoints,       /* these are arrays */
    npoints_p,
    mtrim_p,
    ntrim_p,       /* trim amounts */
    irax_m_p,
    irax_n_p ;
  int_p
    mloc_p,
    nloc_p ;        /* output: local memory needs */
  int_p
    mlen_p,
    nlen_p ;
{
  int par ;                     /* parent domain descriptor */
  int nest ;                    /* nested domain descriptor */
  int nmax_par, mmax_par ;      /* parent global dimensions */
  int neff_par, meff_par ;      /* parent effective global dimensions */
  int nmax_nes, mmax_nes ;      /* nest global dimensions */
  int neff_nes, meff_nes ;      /* nest effective global dimensions */
  int irax_m, irax_n ;          /* nesting ratio, m by n */
  int npts ;
  int i, j, m, n ;
  int in, jn ;  /* nested domain cursors */
  int which_kid_am_i_m, which_kid_am_i_n ;
  int mid ;     /* mother point id */
  int nid ;     /* nest point id */
  int cm, cn ;  /* cursors for m, n dimension in children of cd point */
  int im,ime ;  /* coordinate of first/last in mother domain */
  int jm,jme ;  /* coordinate of first/last in mother domain */
  int * work ;                  /* work array */
  int mtrim, ntrim ;
  int dmlow, dmhigh, dnlow, dnhigh, mmin, nmin ;
  rsl_point_t *ppt, *p, *pdom, *ndom ; /* working point and domain pointers */

  par = *par_p ;
  npts = *npoints_p ;
  irax_m = *irax_m_p ;
  irax_n = *irax_n_p ;
  mtrim = *mtrim_p ;
  ntrim = *ntrim_p ;

  RSL_TEST_ERR( par < 0 || par >= RSL_MAXDOMAINS,
             "spawn_nest: bad parent domain index") ;
  RSL_TEST_ERR( !domain_info[par].valid == RSL_VALID,
                "spawn_nest: invalid parent domain") ;

/* test for the existence of other, previously defined active nests at
   this level. */
  if ( old_offsets == 1 )
  {
    int i ;
    for ( i = 1 ; i < RSL_MAXDOMAINS ; i++ )
    {
      RSL_TEST_ERR( 
      (domain_info[i].valid == RSL_VALID && domain_info[i].parent == par ),
      "Already a nest at this level; disallowed with old offsetting scheme.") ;
    }
  }

  nmax_par = domain_info[par].len_n ;
  mmax_par = domain_info[par].len_m ;
  neff_par = domain_info[par].eff_n ;
  meff_par = domain_info[par].eff_m ;

  work = RSL_MALLOC( int,  nmax_par*mmax_par ) ;
  for ( i = 0 ; i < nmax_par*mmax_par ; i++ )
    work[i] = 0 ;

  /* draw the polygon */
  i = 0 ;
  if ( xpoints[i] < 0 || xpoints[i] >= neff_par ||
       ypoints[i] < 0 || ypoints[i] >= meff_par )
  {
    sprintf(mess,
      "spawn_nest: point outside effective boundary of parent x=%d, y=%d\n",
      xpoints[i], ypoints[i] ) ;
    RSL_TEST_ERR(1,mess) ;
  }
  for ( i = 1 ; i < npts ; i++ )
  {
    if ( xpoints[i] < 0 || xpoints[i] >= neff_par ||
         ypoints[i] < 0 || ypoints[i] >= meff_par )
    {
      sprintf(mess,
        "spawn_nest: point outside effective boundary of parent x=%d, y=%d\n",
        xpoints[i], ypoints[i] ) ;
      RSL_TEST_ERR(1,mess) ;
    }
    mark_line( work, mmax_par, nmax_par,
               xpoints[i-1], ypoints[i-1], 
               xpoints[i],   ypoints[i], 1 ) ;
  }
  /* close polygon if not closed already
     (if it is closed, this won't do anything) */
  mark_line( work, mmax_par, nmax_par,
             xpoints[0], ypoints[0], 
             xpoints[npts-1], ypoints[npts-1], 1 ) ;

  fill_region( work, mmax_par, nmax_par, 0, 2 ) ;

  m = mmax_par ;
  n = nmax_par ;

  for ( j = 0 ; j < n ; j++ )
    for ( i = 0 ; i < m ; i++ )
    {
      if ( work[ INDEX_2( j, i, m ) ] == 2 )
        work[ INDEX_2( j, i, m ) ] = RSL_INVALID ;
      else
        work[ INDEX_2( j, i, m ) ] = RSL_VALID ;
    }

  /* at this point, the region of the c.d. that will have a nest has
     been drawn.  Figure out how big a domain we need to hold this shape. */

  /* work out min and max indices in i and j */
  ime = -99999999 ;
  jme = -99999999 ;
  im  =  99999999 ;
  jm  =  99999999 ;
  for ( j = 0 ; j < n ; j++ )
    for ( i = 0 ; i < m ; i++ )
      if ( work[ INDEX_2( j, i, m ) ] == RSL_VALID )
      {
        if ( i > ime ) ime = i ;
        if ( i < im ) im = i ;
        if ( j > jme ) jme = j ;
        if ( j < jm ) jm = j ;
      }

/* the nested dimensions */
  nmax_nes = ( jme - jm + 1 ) * irax_n ;
  mmax_nes = ( ime - im + 1 ) * irax_m ;
  *nlen_p = nmax_nes - ntrim ;
  *mlen_p = mmax_nes - mtrim ;

/* create the nest */
  GET_NEXT_DOMAIN_DESCRIPTOR ( &nest ) ;
  *nest_p = nest ;
  domain_info[nest].nest_level = domain_info[par].nest_level + 1 ;
  rsl_ndomains++ ;
  rsl_c_initialize_domain( nest,
      nmax_nes, mmax_nes, 1, *nlen_p, *mlen_p, 1 ) ;
  domain_info[nest].parent = par ;
  domain_info[nest].maskid = *maskid_p ;

/* the coordinate in the c.d. -- note, the coordinate point does not
   have to be in the nest itself. */

  domain_info[nest].coord_m = im ;
  domain_info[nest].coord_n = jm ;
  domain_info[nest].irax_m = irax_m ;
  domain_info[nest].irax_n = irax_n ;

  pdom = domain_info[par].domain ;
  ndom = domain_info[nest].domain ;

/* link up child pointers */
  for ( j = jm ; j <= jme ; j++ )
  {
    for ( i = im ; i <= ime ; i++ )
    {
      if ( work[ INDEX_2( j, i, mmax_par ) ] == RSL_VALID )
      {
        ppt = &(pdom[ INDEX_2( j, i, mmax_par ) ]) ;
        if ( ppt->children_p != NULL )
        {
          RSL_FREE(  ppt->children_p ) ;
        }
        ppt->children_p = RSL_MALLOC( rsl_child_info_t, 1 ) ;
        mid = POINTID(par,j,i) ;
        for ( cn = 0 ; cn < irax_n ; cn++ )
        {
          for ( cm = 0 ; cm < irax_m ; cm++ )
          {
            jn = (j-jm)*irax_n+cn ;
            in = (i-im)*irax_m+cm ;
            nid = POINTID( nest, jn, in ) ;
            /* connect child pointers in mother domain */
            ppt->children_p->child[ INDEX_2(cn,cm,irax_m) ] = nid ;
            /* connect mother pointers in child domain */
            ndom[INDEX_2(jn,in,mmax_nes)].mother_id = mid ;
            ndom[INDEX_2(jn,in,mmax_nes)].which_kid_am_i_m = cm ;
            ndom[INDEX_2(jn,in,mmax_nes)].which_kid_am_i_n = cn ;
            /* mark the nested point as valid */
            ndom[INDEX_2(jn,in,mmax_nes)].valid = RSL_VALID ;
          }
        }
      }
    }
  }

  trim_domain( ndom, mmax_nes, nmax_nes, mtrim, ntrim ) ;

  debuggal_mine = 1 ;

  work_out_bdy( ndom, mmax_nes, nmax_nes ) ;

  if ( domain_info[par].decomposed == 0 )
  {
    int dum1, dum2 ;
    default_decomposition( &par, &dum1, &dum2 ) ;
    domain_info[par].decomposed = 1 ;
  }
  if ( domain_info[nest].decomposed == 0 )
  {
    default_decomposition( &nest, mloc_p, nloc_p ) ;
    domain_info[nest].decomposed = 1 ;
  }

  RSL_FREE( work ) ;
}


/* 

The effective size of a nest for purposes of computation on the
nest is not necessarily it's actual size.  The actual size is
constrained to be some multiple of the nesting ratio in each 
dimension, whereas the effective size may be less (it is, in
fact, 2 less in MM5).  An earlier version of R90 attempted to
express this by simply invalidating the extra cells in the
trim_domain routine after the nested domain had been defined.
However, this caused problems for the broadcast/merges because
although the extra cells do not participate in computation, they
may receive data from a parent -- this happens in the case of
dot-variables in a staggered B-grid (U and V in MM5).  The last
row and column of the dot variables end up being passed down from
the parent into the last+1 row and column of the nest (because
of staggering).  If those cells are invalidated they will not
participate in the bcast merge and this data is lost to the
nest.  By trimming, but not invalidating the extra cells, they
are allowed to participate in the bcast merge, but the section
of R90 code that works out what cells are valid for computation
(this is in rsl_new_decomp.c) can still avoid including them.
jm 9/13/95.

*/

trim_domain( dom, mmax, nmax, mtrim, ntrim )
  rsl_point_t *dom ;
  int mmax, nmax, mtrim, ntrim ;
{
  int i, j, jj, ii, cnt ;
  rsl_point_t *p, *pdiag, *pn, *pne, *pe ;

  for ( j = nmax-1 ; j >= 0 ; j-- )
  {
    for ( i = mmax-1 ; i >= 0 ; i-- )
    {
      p = &(dom[INDEX_2(j,i,mmax)]) ;
      p->trimmed = 0 ;
    }
  }

  /* sweep down from high m and knock out first mtrim we hit */

  for ( j = nmax-1 ; j >= 0 ; j-- )
  {
    cnt = 0 ;
    for ( i = mmax-1 ; i >= 0 ; i-- )
    {
      p = &(dom[INDEX_2(j,i,mmax)]) ;
      if ( p->valid == RSL_VALID )
      {
	if ( cnt < mtrim )
	{
	  /* p->valid = RSL_INVALID ; */
	  p->trimmed = 1 ;
	  /* 1/22/96 -- handle inside northeast corners on irreg grids. jm */
	  jj = j-cnt-1 ;
	  if ( jj >= 0 )
	  {
	    pdiag = &(dom[INDEX_2(jj,i,mmax)]) ;
	    if ( pdiag->valid == RSL_VALID )
	    {
	      pdiag->trimmed = 1 ;
	    }
	  }
	  /* 1/22/96 -- end */
	}
	cnt++ ;
      }
      else
	cnt = 0 ;
    }
  }
  /* now do the same for ntrim */
  for ( i = mmax-1 ; i >= 0 ; i-- )
  {
    cnt = 0 ;
    for ( j = nmax-1 ; j >= 0 ; j-- )
    {
      p = &(dom[INDEX_2(j,i,mmax)]) ;
      if ( p->valid == RSL_VALID )
      {
        if ( cnt < ntrim )
        {
          /* p->valid = RSL_INVALID ; */
	  p->trimmed = 1 ;
          /* 1/22/96 -- handle inside northeast corners on irreg grids. jm */
          ii = i-cnt-1 ;
          if ( ii >= 0 )
          {
            pdiag = &(dom[INDEX_2(j,ii,mmax)]) ;
            if ( pdiag->valid == RSL_VALID )
            {
              pdiag->trimmed = 1 ;
            }
          }
          /* 1/22/96 -- end */
        }
        cnt++ ;
      }
      else
        cnt = 0 ;
    }
  }

/* set up cross grid points */
  for ( j = 0 ; j < nmax ; j++ )
  {
    for ( i = mmax-1 ; i >= 0 ; i-- )
    {
      p = &(dom[INDEX_2(j,i,mmax)]) ;
      p->cross = 0 ;
      if ( p->valid == RSL_VALID && ! p->trimmed )
      {
        if ( (i+1 < mmax) && (j+1 < nmax) )
        {
          if (    ((pn  =  &(dom[INDEX_2(j  ,i+1,mmax)])) != NULL )
               && ((pne =  &(dom[INDEX_2(j+1,i+1,mmax)])) != NULL )
               && ((pe  =  &(dom[INDEX_2(j+1,i  ,mmax)])) != NULL ) )
          {
            if ( ( pn->valid == RSL_VALID  &&  ! pn->trimmed  ) &&
                 ( pne->valid == RSL_VALID &&  ! pne->trimmed ) &&
                 ( pe->valid == RSL_VALID  &&  ! pe->trimmed  )  )
            {
              p->cross = 1 ;
            }
          }
        }
      }
    }
  }
}

#define PTEST(P) (!(P.valid==RSL_VALID && P.trimmed == 0)) 
#define QTEST(P) (!(P.valid==RSL_VALID && P.trimmed == 0 && P.cross == 1 )) 


work_out_bdy( dom, mmax, nmax )
  rsl_point_t *dom ;
  int mmax, nmax ;
{
  int i, j, idx, in ;
  int dmlow, dmhigh, dnlow, dnhigh ;
  int d00, d0n, dm0, dmn ;
  int min, closest ;
  rsl_point_t *p ;
  int *wrk_bdy1, *wrk_dbdy1 ;
  int *wrk_bdy2, *wrk_dbdy2 ;

/* this first section works out the distance and direction to the "closest"
   boundary. */

  wrk_bdy1 = RSL_MALLOC(int, mmax*nmax) ;
  wrk_dbdy1 = RSL_MALLOC(int, mmax*nmax) ;
  wrk_bdy2 = RSL_MALLOC(int, mmax*nmax) ;
  wrk_dbdy2 = RSL_MALLOC(int, mmax*nmax) ;

/* DOT GRID */
#pragma csd parallel for private(j, i, idx, p)
  for ( j = 0 ; j < nmax ; j++ )
#pragma _CRI concurrent
    for ( i = 0 ; i < mmax ; i++ )
    {
      idx = INDEX_2(j,i,mmax) ;
      p = &(dom[ idx ]) ;
      if ( p->valid == RSL_VALID && p->trimmed == 0 )
      {
	wrk_bdy1[ idx ] = 0 ; wrk_dbdy1[ idx ] = 0 ;
	wrk_bdy2[ idx ] = 0 ; wrk_dbdy2[ idx ] = 0 ;
      }
      else
      {
	wrk_bdy1[ idx ] = -1 ; wrk_dbdy1[ idx ] = -1 ;
	wrk_bdy2[ idx ] = -1 ; wrk_dbdy2[ idx ] = -1 ;
      }

    }

  fill_boundary( wrk_bdy1, wrk_dbdy1, mmax, nmax,	/* counter clockwise */
		 1, -1, NORTH, SOUTH, EAST, WEST ) ;
  fill_boundary( wrk_bdy2, wrk_dbdy2, mmax, nmax,	/* clockwise */
		 0, -1, NORTH, SOUTH, EAST, WEST ) ;
#pragma csd parallel for private(j, i, idx, p)
  for ( j = 0 ; j < nmax ; j++ )
#pragma _CRI concurrent
    for ( i = 0 ; i < mmax ; i++ )
    {
      idx = INDEX_2(j,i,mmax) ;
      p = &(dom[ idx ]) ;
      if ( p->valid == RSL_VALID && p->trimmed == 0 )
      {
	p->dbdy = wrk_dbdy1[ idx ] ;
	p->bdy_cclockwise = wrk_bdy1[ idx ] ;
	p->bdy_clockwise = wrk_bdy2[ idx ] ;
      }
      else
      {
	p->dbdy = RSL_INVALID ;
	p->bdy_cclockwise = RSL_INVALID ;
	p->bdy_clockwise = RSL_INVALID ;
      }
    }

/* CROSS GRID */
#pragma csd parallel for private(j, i, idx, p)
  for ( j = 0 ; j < nmax ; j++ )
#pragma _CRI concurrent
    for ( i = 0 ; i < mmax ; i++ )
    {
      idx = INDEX_2(j,i,mmax) ;
      p = &(dom[ idx ]) ;
      if ( p->valid == RSL_VALID && p->trimmed == 0 && p->cross == 1 )
      {
	wrk_bdy1[ idx ] = 0 ; wrk_dbdy1[ idx ] = 0 ;
	wrk_bdy2[ idx ] = 0 ; wrk_dbdy2[ idx ] = 0 ;
      }
      else
      {
	wrk_bdy1[ idx ] = -1 ; wrk_dbdy1[ idx ] = -1 ;
	wrk_bdy2[ idx ] = -1 ; wrk_dbdy2[ idx ] = -1 ;
      }

    }

  fill_boundary( wrk_bdy1, wrk_dbdy1, mmax, nmax,	/* counter clockwise */
		 1, -1, NORTH, SOUTH, EAST, WEST ) ;
  fill_boundary( wrk_bdy2, wrk_dbdy2, mmax, nmax,	/* clockwise */
		 0, -1, NORTH, SOUTH, EAST, WEST ) ;

#pragma csd parallel for private(j, i, idx, p)
  for ( j = 0 ; j < nmax ; j++ )
#pragma _CRI concurrent
    for ( i = 0 ; i < mmax ; i++ )
    {
      idx = INDEX_2(j,i,mmax) ;
      p = &(dom[ idx ]) ;
      if ( p->valid == RSL_VALID && p->trimmed == 0 && p->cross == 1 )
      {
	p->dbdy_x = wrk_dbdy1[ idx ] ;
	p->bdy_x_cclockwise = wrk_bdy1[ idx ] ;
	p->bdy_x_clockwise = wrk_bdy2[ idx ] ;
      }
      else
      {
	p->dbdy_x = RSL_INVALID ;
	p->bdy_x_cclockwise = RSL_INVALID ;
	p->bdy_x_clockwise = RSL_INVALID ;
      }
    }

  RSL_FREE( wrk_bdy1 ) ;
  RSL_FREE( wrk_dbdy1 ) ;
  RSL_FREE( wrk_bdy2 ) ;
  RSL_FREE( wrk_dbdy2 ) ;

/* this second section works out the distances to each of the four boundaries
   but does not take corners into account */

#ifdef NO_RAGGED

/* DOT GRID */
  for ( j = 0 ; j < nmax ; j++ )
  {
    for ( i = 0 ; i < mmax ; i++ )
    {
      p = &(dom[INDEX_2(j,i,mmax)]) ;
      if ( p->valid == RSL_VALID && p->trimmed == 0 )
      {
        p->dist_mlow  = (i)+1 ;
        p->dist_mhigh = (mmax-i) ;
        p->dist_nlow  = (j)+1 ;
        p->dist_nhigh = (nmax-j) ;
      }
    }
  }
/* CROSS GRID */
  for ( j = 0 ; j < nmax ; j++ )
  {
    for ( i = 0 ; i < mmax ; i++ )
    {
      p = &(dom[INDEX_2(j,i,mmax)]) ;
      if ( p->valid == RSL_VALID && p->trimmed == 0 && p->cross == 1 )
      {
        p->dist_mlow_x  = (i)+1  ;              /* make 1 based for fortran */
        p->dist_mhigh_x = (mmax-i) ;
        p->dist_nlow_x  = (j)+1  ;
        p->dist_nhigh_x = (nmax-j) ;
      }
    }
  }

#else

/* DOT GRID */
  /* work out boundary info -- this is a little tricky (and time consuming)
     because we have to allow for irregularly shaped nests */
#pragma csd parallel for private(j, i, p, dmlow, dmhigh, dnlow, dnhigh)
  for ( j = 0 ; j < nmax ; j++ )
  {
    for ( i = 0 ; i < mmax ; i++ )
    {
      p = &(dom[INDEX_2(j,i,mmax)]) ;
      if ( p->valid == RSL_VALID && p->trimmed == 0 )
      {
        /* zip down til we find the mlow boundary */
#pragma _CRI concurrent
        for ( dmlow = 1 ; i-dmlow >= 0 ; dmlow++ )
        {
          if ( PTEST(dom[INDEX_2(j,i-dmlow,mmax)])) break ;
        }
        dmlow-- ;
        /* zip up til we find the mhigh boundary */
#pragma _CRI concurrent
        for ( dmhigh = 1 ; i+dmhigh < mmax ; dmhigh++ )
        {
          if ( PTEST(dom[INDEX_2(j,i+dmhigh,mmax)])) break ;
        }
        dmhigh-- ;
        /* zip west til we find the nlow boundary */
#pragma _CRI concurrent
        for ( dnlow = 1 ; j-dnlow >= 0 ; dnlow++ )
        {
          if ( PTEST(dom[INDEX_2(j-dnlow,i,mmax)])) break ;
        }
        dnlow-- ;
        /* zip east til we find the nhigh boundary */
#pragma _CRI concurrent
        for ( dnhigh = 1 ; j+dnhigh < nmax ; dnhigh++ )
        {
          if ( PTEST(dom[INDEX_2(j+dnhigh,i,mmax)])) break ;
        }
        dnhigh-- ;

        p->dist_mlow  = dmlow+1  ;		/* make 1 based for fortran */
        p->dist_mhigh = dmhigh+1 ;
        p->dist_nlow  = dnlow+1  ;
        p->dist_nhigh = dnhigh+1 ;

      }
    }
  }

/* CROSS GRID */
  /* work out boundary info -- this is a little tricky (and time consuming)
     because we have to allow for irregularly shaped nests */
#pragma csd parallel for private(j, i, p, dmlow, dmhigh, dnlow, dnhigh)
  for ( j = 0 ; j < nmax-1 ; j++ )
  {
    for ( i = 0 ; i < mmax-1 ; i++ )
    {
      p = &(dom[INDEX_2(j,i,mmax)]) ;
      if ( p->valid == RSL_VALID && p->trimmed == 0 && p->cross == 1 )
      {
        /* zip down til we find the mlow boundary */
#pragma _CRI concurrent
        for ( dmlow = 1 ; i-dmlow >= 0 ; dmlow++ )
        {
          if ( QTEST(dom[INDEX_2(j,i-dmlow,mmax)])) break ;
        }
        dmlow-- ;
        /* zip up til we find the mhigh boundary */
#pragma _CRI concurrent
        for ( dmhigh = 1 ; i+dmhigh < mmax ; dmhigh++ )
        {
          if ( QTEST(dom[INDEX_2(j,i+dmhigh,mmax)])) break ;
        }
        dmhigh-- ;
        /* zip west til we find the nlow boundary */
#pragma _CRI concurrent
        for ( dnlow = 1 ; j-dnlow >= 0 ; dnlow++ )
        {
          if ( QTEST(dom[INDEX_2(j-dnlow,i,mmax)])) break ;
        }
        dnlow-- ;
        /* zip east til we find the nhigh boundary */
#pragma _CRI concurrent
        for ( dnhigh = 1 ; j+dnhigh < nmax ; dnhigh++ )
        {
          if ( QTEST(dom[INDEX_2(j+dnhigh,i,mmax)])) break ;
        }
        dnhigh-- ;

        p->dist_mlow_x  = dmlow+1  ;		/* make 1 based for fortran */
        p->dist_mhigh_x = dmhigh+1 ;
        p->dist_nlow_x  = dnlow+1  ;
        p->dist_nhigh_x = dnhigh+1 ;

      }
    }
  }
#endif
}

#if COMMENTED_OUT
RSL_DEACTIVATE_DOMAIN ( d_p )
  int_p d_p ;
{
  rsl_domain_info_t * ninfo ;
  int d, P ;
  int destroy_runrec() ;
  int i ;

  d = *d_p ;

  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS, "invalid domain index") ;

  domain_info[d].valid = RSL_INVALID ;
  domain_info[d].decomposed = 0 ;

  if ( domain_info[d].domain != NULL ) RSL_FREE( domain_info[d].domain ) ;

  destroy_list( &(domain_info[d].pts), NULL ) ;
  destroy_list( &(domain_info[d].ghost_pts), NULL ) ;
  destroy_list( &(domain_info[d].iruns), destroy_runrec ) ;

  if ( domain_info[d].js != NULL ) RSL_FREE( domain_info[d].js ) ;
  if ( domain_info[d].is != NULL ) RSL_FREE( domain_info[d].is ) ;
  if ( domain_info[d].ie != NULL ) RSL_FREE( domain_info[d].ie ) ;

  for ( P = 0 ; P < rsl_nproc_all ; P++ )
  {
    ninfo->bcast_recv_Pnpts[P] = 0 ;
    ninfo->bcast_recv_Plist[P] = RSL_INVALID ;
    ninfo->bcast_recv_Ptags[P] = RSL_INVALID ;
    ninfo->Nbcast_recv_Plist = 0 ;
  }

/* get rid of stencils on this domain */
  for ( i = 0 ; i < domain_info[d].stencurs ; i++ )
  {
    if ( domain_info[d].stenlist[i] > 0 && 
	 domain_info[d].stenlist[i] < RSL_MAXDESCRIPTORS )
    {
      destroy_stencil_on_domain( d,
				 sh_descriptors[domain_info[d].stenlist[i]] ) ;
    }
  }
  domain_info[d].stencurs = 0 ;

}
#endif


/* this returns the first unused domain descriptor; it checks
   for unusededness by seeing if the domain pointed to is valid 
   or not.  If it's not, it's unused.  That's the only checking
   that this thing does. */
/* this can also be used to find out what the next descriptor
   will be -- so it shouldn't, by itself, change the state of 
   the domain descriptor as a result of being called */

GET_NEXT_DOMAIN_DESCRIPTOR ( d_p )
  int_p d_p ;
{
  int d ;

  for ( d = 0 ; d < RSL_MAXDOMAINS ; d++ )
  {
    if ( domain_info[d].valid == RSL_INVALID )
    {
      break ;
    }
  }
  RSL_TEST_ERR( d == RSL_MAXDOMAINS, "Out of domains." ) ;
  *d_p = d ;
}


#if 0
clear()
{
char clear[] = "[H[2J" ;
printf(clear) ;
}


if (debuggal_mine) {
  int i, j, x ;
  rsl_point_t *p ;
  printf("------ in %d ------ bc: %d %d\n", in, breadcrumb_i, breadcrumb_j ) ;
  for ( i = mmax-1 ; i >= 0 ; i-- )
  {
    for ( j = 0 ; j < nmax ; j++ )
    {
      p = &(dom[INDEX_2(j,i,mmax)]) ;
      if ( p->dbdy==-1 ) x = 0 ;
      else x = p->dbdy % 10 ;
      printf("%1d",x ) ;
    }
    printf("\n") ;
  }
/*
  sleep(1) ;
  clear() ;
*/
}
#endif
