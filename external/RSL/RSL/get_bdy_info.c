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

static int tiebreaker = DIAG_WINS ;
/*@
  RSL_BDY_TIEBRK -- Break ties in determining cells' boundary affiliations.

  Notes:
  This routine effects the information provided by the set of RSL_GET_BDY routines.

  Corner points that are equidistant to both a boundary in the M dimension and
  a boundary in the N dimension are, by default considered neither; they are
  considered "diagonal." RSL_BDY_TIEBRK changes the behavior to the one specified
  by the argument Arg1, which may be M_WINS, N_WINS, or DIAG_WINS (defined in the file
  rsl.inc).

  See also:

  RSL_GET_BDY_LPT, RSL_GET_BDY_LARRAY, RSL_GET_BDY_GPT,

  RSL_GET_BDY_GARRAY

@*/
RSL_BDY_TIEBRK ( brk_p )
  int_p brk_p ;      /* (I) M_WINS, N_WINS, or DIAG_WINS */
{
  if      ( *brk_p == M_WINS )
    tiebreaker = M_WINS ;
  else if ( *brk_p == N_WINS ) 
    tiebreaker = N_WINS ;
  else 
    tiebreaker = DIAG_WINS ;
}


/*@
  RSL_GET_BDY_LPT  -- Get boundary information for a locally indexed point.

  Notes:
  This returns boundary information for a locally specified point in the integer
  array Arg2. The information is useful with irregularly shaped nests
  (see RSL_SPAWN_IRREG_NEST), in which boundary proximity is not easily determined
  from loop indices.

  The variations of this routine are RSL_GET_BDY_LPT, RSL_GET_BDY_LARRAY,
  RSL_GET_BDY_GPT, and RSL_GET_BDY_GARRAY.

  The argument Arg3 should be either DOT_BDY_INFO_LEN or 
  CROSS_BDY_INFO_LEN (defined in the file rsl.inc).  With DOT_BDY_INFO_LEN, the
  routine will return
  boundary information for a domain containing all points of the grid (i.e., the 
  dot-boundaries of an Arakawa-B grid).
  With CROSS_BDY_INFO_LEN, the routine will return the dot boundary information plus
  the cross boundary information (i.e., the uppermost boundaries in M and N are at
  M-1 and N-1).

  The array Arg2 may be indexed to obtain the following information.

  Verbatim:
$      symbolic name        description
$      (rsl.inc)          
$      --------------------------------------------
$
$ Normal grid boundary information (dot, in Arakawa B)
$
$      RSL_MLOW             Distance to MLOW (south) boundary
$      RSL_MHIGH            Distance to MHIGH (north) boundary
$      RSL_NLOW             Distance to NLOW (west) boundary 
$      RSL_NHIGH            Distance to NHIGH (east) boundary
$      RSL_DBDY             Distance to closest boundary
$      RSL_CLOSEST          Closest boundary
$
$ Cross grid boundary information (Arakawa B)
$
$      RSL_MLOW_X           Distance to MLOW (south) cross boundary
$      RSL_MHIGH_X          Distance to MHIGH (north) cross boundary
$      RSL_NLOW_X           Distance to NLOW (west) cross boundary 
$      RSL_NHIGH_X          Distance to NHIGH (east) cross boundary
$      RSL_DBDY_X           Distance to closest cross boundary
$      RSL_CLOSEST_X        Closest cross boundary

BREAKTHEEXAMPLECODE

  Example:
$      INTEGER LBDYINFO(CROSS_BDY_INFO_LEN)
$      ...
$      RSL_DO_N(J,1,JMAX)
$        RSL_DO_M(I,1,IMAX)
$          CALL RSL_GET_BDY_LPT( DID, GBDYINFO, CROSS_BDY_INFO_LEN, I, J )
$          IF ( LBDYINFO(RSL_DBDY) .EQ. 1 ) THEN
$            ...CODE TO EXECUTE ONLY ON OUTERMOST BOUNDARY CELLS...
$          ENDIF
$        RSL_ENDDO
$      RSL_ENDDO
$      
BREAKTHEEXAMPLECODE

  In this example, the boundary information in LBDYINFO is used to determine
  the first boundary row or column during iteration over a domain.


  See also:

  RSL_GET_BDY_LPT, RSL_GET_BDY_LARRAY, RSL_GET_BDY_GPT,

  RSL_GET_BDY_GARRAY


@*/
   
RSL_GET_BDY_LPT ( d_p, bdyinf, n_p, i_p, j_p )
  int_p 
    d_p ;            /* (I) RSL domain descriptor */
  int 
    bdyinf[] ;       /* (O) Boundary information (see table) */
  int_p 
    n_p              /* (I) DOT_BDY_INFO_LEN or CROSS_BDY_INFO_LEN */
   ,i_p              /* (I) M coordinate of point */
   ,j_p ;            /* (I) N coordinate of point */
{
  int d ;
  int i, j, nrun, ig, jg ;
  int mloc, nloc, len, mtn ;
  int mlen, idif, jdif ;
  int *p ;
  rsl_domain_info_t *dinfo ;
  rsl_point_t *ddom, *pt ;

  d = *d_p ;

  if ( *n_p < DOT_BDY_INFO_LEN )
  {
    sprintf(mess, "argument for length of bdyinfo too small, %d < %d",
	    *n_p, DOT_BDY_INFO_LEN ) ;
    RSL_TEST_ERR( 1, mess ) ;
  }
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "rsl_compute: bad domain") ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
     "rsl_init_nextcell: invalid domain") ;
  if ( domain_info[d].decomposed != 1 )
  {
    default_decomposition( d_p,
                           &(domain_info[*d_p].loc_m),
                           &(domain_info[*d_p].loc_n) ) ;
  }

  dinfo = &(domain_info[*d_p]) ;
  ddom = dinfo->domain ;

  mlen = dinfo->len_m ;
  idif = dinfo->idif ;
  jdif = dinfo->jdif ;

  for ( p = bdyinf, i = 0 ; i < *n_p ; i++, p++ )
  {
    *p = RSL_INVALID ;
  }
  i = *i_p-1 ;
  j = *j_p-1 ;
  jg = j - jdif ;
  ig = i - idif ;

  pt = &(ddom[INDEX_2(jg,ig,mlen)]) ;

  bdyinf[ RSL_MLOW      -1 ] = pt->dist_mlow ;
  bdyinf[ RSL_MHIGH     -1 ] = pt->dist_mhigh ;
  bdyinf[ RSL_NLOW      -1 ] = pt->dist_nlow ;
  bdyinf[ RSL_NHIGH     -1 ] = pt->dist_nhigh ;
  bdyinf[ RSL_DBDY      -1 ] = pt->dbdy ;
  bdyinf[ RSL_CLOSEST   -1 ] = set_bdy_direction( pt->bdy_cclockwise,
		   				  pt->bdy_clockwise ) ;

  if ( *n_p > DOT_BDY_INFO_LEN && *n_p <= CROSS_BDY_INFO_LEN ) 
  {
    bdyinf[ RSL_MLOW_X    -1 ] = pt->dist_mlow_x ;
    bdyinf[ RSL_MHIGH_X   -1 ] = pt->dist_mhigh_x ;
    bdyinf[ RSL_NLOW_X    -1 ] = pt->dist_nlow_x ;
    bdyinf[ RSL_NHIGH_X   -1 ] = pt->dist_nhigh_x ;
    bdyinf[ RSL_DBDY_X    -1 ] = pt->dbdy_x ;
    bdyinf[ RSL_CLOSEST_X -1 ] = set_bdy_direction( pt->bdy_x_cclockwise,
		   				    pt->bdy_x_clockwise ) ;
  }

}


/*@
  RSL_GET_BDY_LARRAY  -- Get local array containing boundary information.

  Notes:
  This fills in the entries of Arg2, a 3-dimensional array of boundary information for
  a domain.  The first two dimensions are the locally sized M and N dimensions
  of the domain.  The array is indexed by the local domain indices and by the
  third (most major) index that specifies the
  information about the point (see table).

  The variations of this routine are RSL_GET_BDY_LPT, RSL_GET_BDY_LARRAY,
  RSL_GET_BDY_GPT, and RSL_GET_BDY_GARRAY.
  
  The first two dimensions of Arg2 must be declared MLOC and NLOC, the local array sizes returned
  by the RSL domain definition routines RSL_MOTHER and the several nest spawning routines.
  The third
  dimension of Arg2 should be declared using one either CROSS_BDY_INFO_LEN or
  DOT_BDY_INFO_LEN (if in doubt use CROSS_BDY_INFO_LEN, since it is larger).
  These are defined in the file ``rsl.inc." Also use this constant as
  Arg3 in the call to RSL_GET_BDY_LARRAY.  With DOT_BDY_INFO_LEN, the
  routine will return
  boundary information for a domain containing all points of the grid (i.e., the 
  dot-boundaries of an Arakawa-B grid).
  With CROSS_BDY_INFO_LEN, the routine will return the dot boundary information plus
  the cross boundary information (i.e., the uppermost boundaries in M and N are at
  M-1 and N-1).

  Improper declaration and allocation
  of memory for
  Arg2 properly or an incorrect value for Arg3 may result in memory
  being overwritten with unpredictable results.

  The array Arg2 may be indexed to obtain the following information.

  Verbatim:
$      symbolic name        description
$      (rsl.inc)          
$      --------------------------------------------
$
$      RSL_MLOW             Distance to MLOW (south) boundary
$      RSL_MHIGH            Distance to MHIGH (north) boundary
$      RSL_NLOW             Distance to NLOW (west) boundary
$      RSL_NHIGH            Distance to NHIGH (east) boundary
$      RSL_DBDY             Distance to closest boundary
$      RSL_CLOSEST          Closest boundary
$
$ Cross grid boundary information (Arakawa B)
$
$      RSL_MLOW_X           Distance to MLOW (south) cross boundary
$      RSL_MHIGH_X          Distance to MHIGH (north) cross boundary
$      RSL_NLOW_X           Distance to NLOW (west) cross boundary
$      RSL_NHIGH_X          Distance to NHIGH (east) cross boundary
$      RSL_DBDY_X           Distance to closest cross boundary
$      RSL_CLOSEST_X        Closest cross boundary

BREAKTHEEXAMPLECODE



  Example:
$      INTEGER, ALLOCATABLE :: LBDYINFO(:,:,:)
$      ...
$      ALLOCATE( LBDYINFO( MLOC, NLOC, CROSS_BDY_INFO_LEN ) )
$      ...
$      CALL RSL_GET_BDY_LARRAY( DID, LBDYINFO, CROSS_BDY_INFO_LEN )
$      RSL_DO_N(J,1,JMAX)
$        RSL_DO_M(I,1,IMAX)
$          IF ( LBDYINFO(I,J,RSL_DBDY) .EQ. 1 ) THEN
$            ...CODE TO EXECUTE ONLY ON OUTERMOST BOUNDARY CELLS...
$          ENDIF
$        RSL_ENDDO
$      RSL_ENDDO
$      
BREAKTHEEXAMPLECODE


  See also:

  RSL_GET_BDY_LPT, RSL_GET_BDY_LARRAY, RSL_GET_BDY_GPT,

  RSL_GET_BDY_GARRAY


@*/

RSL_GET_BDY_LARRAY ( d_p, bdyinf, n_p )
  int_p d_p ;       /* (I) RSL domain descriptor */
  int_p bdyinf ;    /* (O) A 3-dimensional array (see discussion) */
  int_p n_p ;       /* (I) DOT_BDY_INFO_LEN or CROSS_BDY_INFO_LEN */
{
  int d ;
  int cross ;
  int i, j, nrun, ig, jg ;
  int mloc, nloc, len, mtn ;
  int mlen, idif, jdif ;
  int *p ;
  rsl_domain_info_t *dinfo ;
  rsl_point_t *ddom, *pt ;

  d = *d_p ;

  if ( *n_p < DOT_BDY_INFO_LEN )
  {
    sprintf(mess, "argument for length of bdyinfo too small, %d < %d",
            *n_p, DOT_BDY_INFO_LEN ) ;
    RSL_TEST_ERR( 1, mess ) ;
  }
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "rsl_compute: bad domain") ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
     "rsl_init_nextcell: invalid domain") ;
  if ( domain_info[d].decomposed != 1 )
  {
    default_decomposition( d_p,
                           &(domain_info[*d_p].loc_m),
                           &(domain_info[*d_p].loc_n) ) ;
  }

  dinfo = &(domain_info[*d_p]) ;
  ddom = dinfo->domain ;

  mlen = dinfo->len_m ;
  mloc = dinfo->loc_m ;
  nloc = dinfo->loc_n ;
  mtn = mloc*nloc ;
  idif = dinfo->idif ;
  jdif = dinfo->jdif ;

  for ( p = bdyinf, i = 0 ; i < *n_p*mtn ; i++, p++ )
  {
    *p = RSL_INVALID ;
  }
  for ( nrun = 0 ; nrun < domain_info[d].nrun[0] ; nrun++ )
  {
    j = dinfo->js[0][nrun]-1 ;
    jg = j - jdif ;
    for ( i = dinfo->is[0][nrun]-1 ; i <= dinfo->ie[0][nrun]-1 ; i++ )
    {
      ig = i - idif ;
      pt = &(ddom[INDEX_2(jg,ig,mlen)]) ;
      cross = pt->cross ;
      p = &(bdyinf[INDEX_3(0,j,nloc,i,mloc)]) ;

      *p = pt->dist_mlow    ; p+=mtn ;
      *p = pt->dist_mhigh   ; p+=mtn ;
      *p = pt->dist_nlow    ; p+=mtn ;
      *p = pt->dist_nhigh   ; p+=mtn ;
      *p = pt->dbdy         ; p+=mtn ;
      *p = set_bdy_direction( pt->bdy_cclockwise,
			      pt->bdy_clockwise ) ;
			      p+=mtn ;

      if ( *n_p > DOT_BDY_INFO_LEN && *n_p <= CROSS_BDY_INFO_LEN )
      {
	/* note that the increment of p is outside the if  and
	   happens unconditionally */
        if ( cross ) *p = pt->dist_mlow_x    ; p+=mtn ;
        if ( cross ) *p = pt->dist_mhigh_x   ; p+=mtn ;
        if ( cross ) *p = pt->dist_nlow_x    ; p+=mtn ;
        if ( cross ) *p = pt->dist_nhigh_x   ; p+=mtn ;
        if ( cross ) *p = pt->dbdy_x         ; p+=mtn ;
        if ( cross ) *p = set_bdy_direction( pt->bdy_x_cclockwise,
		           	             pt->bdy_x_clockwise ) ;
			                       p+=mtn ;
      }
    }
  }
}

RSL_GET_BDY_LARRAY2 ( d_p, bdyinf, n_p, mloc_p, nloc_p )
  int_p d_p ;       /* (I) RSL domain descriptor */
  int_p bdyinf ;    /* (O) A 3-dimensional array (see discussion) */
  int_p n_p ;       /* (I) DOT_BDY_INFO_LEN or CROSS_BDY_INFO_LEN */
  int_p mloc_p, nloc_p ; /* (I) m and n dimensions of bdyinf */
{
  int d ;
  int cross ;
  int i, j, nrun, ig, jg ;
  int mloc, nloc, len, mtn ;
  int mlen, idif, jdif ;
  int *p ;
  rsl_domain_info_t *dinfo ;
  rsl_point_t *ddom, *pt ;

  d = *d_p ;

  if ( *n_p < DOT_BDY_INFO_LEN )
  {
    sprintf(mess, "argument for length of bdyinfo too small, %d < %d",
            *n_p, DOT_BDY_INFO_LEN ) ;
    RSL_TEST_ERR( 1, mess ) ;
  }
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "rsl_compute: bad domain") ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
     "rsl_init_nextcell: invalid domain") ;
  if ( domain_info[d].decomposed != 1 )
  {
    default_decomposition( d_p,
                           &(domain_info[*d_p].loc_m),
                           &(domain_info[*d_p].loc_n) ) ;
  }

  dinfo = &(domain_info[*d_p]) ;
  ddom = dinfo->domain ;

  mlen = dinfo->len_m ;
  mloc = *mloc_p ;
  nloc = *nloc_p ;
  mtn = mloc*nloc ;
  idif = dinfo->idif ;
  jdif = dinfo->jdif ;

  for ( p = bdyinf, i = 0 ; i < *n_p*mtn ; i++, p++ )
  {
    *p = RSL_INVALID ;
  }
  for ( nrun = 0 ; nrun < domain_info[d].nrun[0] ; nrun++ )
  {
    j = dinfo->js[0][nrun]-1 ;
    jg = j - jdif ;
    for ( i = dinfo->is[0][nrun]-1 ; i <= dinfo->ie[0][nrun]-1 ; i++ )
    {
      ig = i - idif ;
      pt = &(ddom[INDEX_2(jg,ig,mlen)]) ;
      cross = pt->cross ;
      p = &(bdyinf[INDEX_3(0,j,nloc,i,mloc)]) ;

      *p = pt->dist_mlow    ; p+=mtn ;
      *p = pt->dist_mhigh   ; p+=mtn ;
      *p = pt->dist_nlow    ; p+=mtn ;
      *p = pt->dist_nhigh   ; p+=mtn ;
      *p = pt->dbdy         ; p+=mtn ;
      *p = set_bdy_direction( pt->bdy_cclockwise,
			      pt->bdy_clockwise ) ;
			      p+=mtn ;

      if ( *n_p > DOT_BDY_INFO_LEN && *n_p <= CROSS_BDY_INFO_LEN )
      {
	/* note that the increment of p is outside the if  and
	   happens unconditionally */
        if ( cross ) *p = pt->dist_mlow_x    ; p+=mtn ;
        if ( cross ) *p = pt->dist_mhigh_x   ; p+=mtn ;
        if ( cross ) *p = pt->dist_nlow_x    ; p+=mtn ;
        if ( cross ) *p = pt->dist_nhigh_x   ; p+=mtn ;
        if ( cross ) *p = pt->dbdy_x         ; p+=mtn ;
        if ( cross ) *p = set_bdy_direction( pt->bdy_x_cclockwise,
		           	             pt->bdy_x_clockwise ) ;
			                       p+=mtn ;
      }
    }
  }
}


/* These are like the other routines but return the global (undecomposed)
   boundary data for a domain.  Bdyinf would be indexed by ig and jg,
   rather than i and j in the calling program.  */

/*@
  RSL_GET_BDY_GPT  -- Get boundary information for a globally indexed point.

  Notes:
  This returns boundary information for a globally specified point in the integer
  array Arg2. The information is useful with irregularly shaped nests
  (see RSL_SPAWN_IRREG_NEST), in which boundary proximity is not easily determined
  from loop indices.

  The variations of this routine are RSL_GET_BDY_LPT, RSL_GET_BDY_LARRAY,
  RSL_GET_BDY_GPT, and RSL_GET_BDY_GARRAY.

  The argument Arg3 should be either DOT_BDY_INFO_LEN or 
  CROSS_BDY_INFO_LEN (defined in the file rsl.inc).  With DOT_BDY_INFO_LEN, the
  routine will return
  boundary information for a domain containing all points of the grid (i.e., the 
  dot-boundaries of an Arakawa-B grid).
  With CROSS_BDY_INFO_LEN, the routine will return the dot boundary information plus
  the cross boundary information (i.e., the uppermost boundaries in M and N are at
  M-1 and N-1).

  The array Arg2 may be indexed to obtain the following information.

  Verbatim:
$      symbolic name        description
$      (rsl.inc)          
$      --------------------------------------------
$
$      RSL_MLOW             Distance to MLOW (south) boundary
$      RSL_MHIGH            Distance to MHIGH (north) boundary
$      RSL_NLOW             Distance to NLOW (west) boundary
$      RSL_NHIGH            Distance to NHIGH (east) boundary
$      RSL_DBDY             Distance to closest boundary
$      RSL_CLOSEST          Closest boundary
$
$ Cross grid boundary information (Arakawa B)
$
$      RSL_MLOW_X           Distance to MLOW (south) cross boundary
$      RSL_MHIGH_X          Distance to MHIGH (north) cross boundary
$      RSL_NLOW_X           Distance to NLOW (west) cross boundary
$      RSL_NHIGH_X          Distance to NHIGH (east) cross boundary
$      RSL_DBDY_X           Distance to closest cross boundary
$      RSL_CLOSEST_X        Closest cross boundary

BREAKTHEEXAMPLECODE

  Example:
$      INTEGER GBDYINFO(CROSS_BDY_INFO_LEN)
$      ...
$      RSL_DO_N(J,1,JMAX)
$        RSL_DO_M(I,1,IMAX)
$          CALL RSL_GET_BDY_GPT( DID, GBDYINFO, CROSS_BDY_INFO_LEN, IG, JG )
$          IF ( GBDYINFO(RSL_DBDY) .EQ. 1 ) THEN
$            ...CODE TO EXECUTE ONLY ON OUTERMOST BOUNDARY CELLS...
$          ENDIF
$        RSL_ENDDO
$      RSL_ENDDO
$      
BREAKTHEEXAMPLECODE

  In this example, the boundary information in GBDYINFO is used to determine
  the first boundary row or column during iteration over a domain.  The reserved
  global index variables IG and JG are set by the RSL_DO macros.

  See also:

  RSL_GET_BDY_LPT, RSL_GET_BDY_LARRAY, RSL_GET_BDY_GPT,

  RSL_GET_BDY_GARRAY

@*/


RSL_GET_BDY_GPT ( d_p, bdyinf, n_p, ig_p, jg_p )
  int_p 
    d_p ;            /* (I) RSL domain descriptor */
  int 
    bdyinf[] ;       /* (O) Boundary information (see table). */
  int_p 
    n_p              /* (I) DOT_BDY_INFO_LEN or CROSS_BDY_INFO_LEN */
   ,ig_p             /* (I) Global index into M dimension */
   ,jg_p ;           /* (I) Global index into N dimension */
{
  int d ;
  int i, j ;
  int mlen, nlen ;
  int *p ;
  rsl_domain_info_t *dinfo ;
  rsl_point_t *ddom, *pt ;

  d = *d_p ;

  if ( *n_p < DOT_BDY_INFO_LEN )
  {
    sprintf(mess, "argument for length of bdyinfo too small, %d < %d",
            *n_p, DOT_BDY_INFO_LEN ) ;
    RSL_TEST_ERR( 1, mess ) ;
  }
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "rsl_compute: bad domain") ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
     "rsl_init_nextcell: invalid domain") ;

  dinfo = &(domain_info[*d_p]) ;
  ddom = dinfo->domain ;
  mlen = dinfo->len_m ;
  nlen = dinfo->len_n ;

  for ( p = bdyinf, i = 0 ; i < *n_p ; i++, p++ )
  {
    *p = RSL_INVALID ;
  }

  j = *jg_p-1 ;
  i = *ig_p-1 ;

  pt = &(ddom[INDEX_2(j,i,mlen)]) ;

  bdyinf[ RSL_MLOW      -1 ] = pt->dist_mlow ;
  bdyinf[ RSL_MHIGH     -1 ] = pt->dist_mhigh ;
  bdyinf[ RSL_NLOW      -1 ] = pt->dist_nlow ;
  bdyinf[ RSL_NHIGH     -1 ] = pt->dist_nhigh ;
  bdyinf[ RSL_DBDY      -1 ] = pt->dbdy ;
  bdyinf[ RSL_CLOSEST   -1 ] = set_bdy_direction( pt->bdy_cclockwise,
                                                  pt->bdy_clockwise ) ;

  if ( *n_p > DOT_BDY_INFO_LEN && *n_p <= CROSS_BDY_INFO_LEN )
  {
    bdyinf[ RSL_MLOW_X    -1 ] = pt->dist_mlow_x ;
    bdyinf[ RSL_MHIGH_X   -1 ] = pt->dist_mhigh_x ;
    bdyinf[ RSL_NLOW_X    -1 ] = pt->dist_nlow_x ;
    bdyinf[ RSL_NHIGH_X   -1 ] = pt->dist_nhigh_x ;
    bdyinf[ RSL_DBDY_X    -1 ] = pt->dbdy_x ;
    bdyinf[ RSL_CLOSEST_X -1 ] = set_bdy_direction( pt->bdy_x_cclockwise,
                                                    pt->bdy_x_clockwise ) ;
  }

}

/*@
  RSL_GET_BDY_GARRAY  -- Get global array containing boundary information.

  Notes:
  This fills in the entries of Arg2, a 3-dimensional array of boundary information for
  a domain.  The first two dimensions are the globally sized M and N dimensions
  of the domain.  The array is indexed by the global domain indices and by the
  third (most major) index that specifies the
  information about the point (see table).

  The variations of this routine are RSL_GET_BDY_LPT, RSL_GET_BDY_LARRAY,
  RSL_GET_BDY_GPT, and RSL_GET_BDY_GARRAY.
  
  The first two dimensions of Arg2 must be declared M and N, the global array sizes returned
  by the RSL domain definition routines RSL_MOTHER and the several nest spawning routines.
  The third
  dimension of Arg2 should be declared using one either CROSS_BDY_INFO_LEN or
  DOT_BDY_INFO_LEN (if in doubt use CROSS_BDY_INFO_LEN, since it is larger).
  These are defined in the file ``rsl.inc." Also use this constant as
  Arg3 in the call to RSL_GET_BDY_LARRAY.  With DOT_BDY_INFO_LEN, the
  routine will return
  boundary information for a domain containing all points of the grid (i.e., the 
  dot-boundaries of an Arakawa-B grid).
  With CROSS_BDY_INFO_LEN, the routine will return the dot boundary information plus
  the cross boundary information (i.e., the uppermost boundaries in M and N are at
  M-1 and N-1).

  Improper declaration and allocation
  of memory for
  Arg2 properly or an incorrect value for Arg3 may result in memory
  being overwritten with unpredictable results.

  The array Arg2 may be indexed to obtain the following information.

  Verbatim:
$      symbolic name        description
$      (rsl.inc)          
$      --------------------------------------------
$
$      RSL_MLOW             Distance to MLOW (south) boundary
$      RSL_MHIGH            Distance to MHIGH (north) boundary
$      RSL_NLOW             Distance to NLOW (west) boundary
$      RSL_NHIGH            Distance to NHIGH (east) boundary
$      RSL_DBDY             Distance to closest boundary
$      RSL_CLOSEST          Closest boundary
$
$ Cross grid boundary information (Arakawa B)
$
$      RSL_MLOW_X           Distance to MLOW (south) cross boundary
$      RSL_MHIGH_X          Distance to MHIGH (north) cross boundary
$      RSL_NLOW_X           Distance to NLOW (west) cross boundary
$      RSL_NHIGH_X          Distance to NHIGH (east) cross boundary
$      RSL_DBDY_X           Distance to closest cross boundary
$      RSL_CLOSEST_X        Closest cross boundary


BREAKTHEEXAMPLECODE

  Example:
$      INTEGER, ALLOCATABLE :: GBDYINFO(:,:,:)
$      ...
$      ALLOCATE( GBDYINFO( M, N, CROSS_BDY_INFO_LEN ) )
$      ...
$      CALL RSL_GET_BDY_LARRAY( DID, GBDYINFO, CROSS_BDY_INFO_LEN )
$      ...
$      RSL_DO_N(J,1,JMAX)
$        RSL_DO_M(I,1,IMAX)
$          IF ( GBDYINFO(IG,JG,RSL_DBDY) .EQ. 1 ) THEN
$            ...CODE TO EXECUTE ONLY ON OUTERMOST BOUNDARY CELLS...
$          ENDIF
$        RSL_ENDDO
$      RSL_ENDDO
$      
BREAKTHEEXAMPLECODE

  In this example, the boundary information array GBDYINFO is used to determine
  the first boundary row or column during iteration over a domain.  The reserved
  global index variables IG and JG are set by the RSL_DO macros.


  See also:

  RSL_GET_BDY_LPT, RSL_GET_BDY_LARRAY, RSL_GET_BDY_GPT,

  RSL_GET_BDY_GARRAY


@*/

RSL_GET_BDY_GARRAY ( d_p, bdyinf, n_p )
  int_p d_p ;         /* (I) RSL domain descriptor */
  int_p bdyinf ;      /* (O) A 3-dimensional array (see discussion) */
  int_p n_p ;         /* (I) DOT_BDY_INFO_LEN or CROSS_BDY_INFO_LEN */
{
  int d ;
  int i, j ;
  int mtn ;
  int mlen, nlen ;
  int cross ;
  int *p ;
  rsl_domain_info_t *dinfo ;
  rsl_point_t *ddom, *pt ;

  d = *d_p ;

  if ( *n_p < DOT_BDY_INFO_LEN )
  {
    sprintf(mess, "argument for length of bdyinfo too small, %d < %d",
            *n_p, DOT_BDY_INFO_LEN ) ;
    RSL_TEST_ERR( 1, mess ) ;
  }
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "rsl_compute: bad domain") ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
     "rsl_init_nextcell: invalid domain") ;

  dinfo = &(domain_info[*d_p]) ;
  ddom = dinfo->domain ;
  mlen = dinfo->len_m ;
  nlen = dinfo->len_n ;
  mtn = mlen*nlen ;

  for ( p = bdyinf, i = 0 ; i < 2*mtn ; i++, p++ )
  {
    *p = RSL_INVALID ;
  }
  for ( j = 0 ; j < nlen ; j++ )
  {
    for ( i = 0 ; i < mlen ; i++ )
    {
      pt = &(ddom[INDEX_2(j,i,mlen)]) ;
      cross = pt->cross ;
      p = &(bdyinf[INDEX_3(0,j,nlen,i,mlen)]) ;

      *p = pt->dist_mlow    ; p+=mtn ;
      *p = pt->dist_mhigh   ; p+=mtn ;
      *p = pt->dist_nlow    ; p+=mtn ;
      *p = pt->dist_nhigh   ; p+=mtn ;
      *p = pt->dbdy         ; p+=mtn ;
      *p = set_bdy_direction( pt->bdy_cclockwise,
                              pt->bdy_clockwise ) ;
                              p+=mtn ;

      if ( *n_p > DOT_BDY_INFO_LEN && *n_p <= CROSS_BDY_INFO_LEN )
      {
        /* note that the increment of p is outside the if  and
           happens unconditionally */
        if ( cross ) *p = pt->dist_mlow_x    ; p+=mtn ;
        if ( cross ) *p = pt->dist_mhigh_x   ; p+=mtn ;
        if ( cross ) *p = pt->dist_nlow_x    ; p+=mtn ;
        if ( cross ) *p = pt->dist_nhigh_x   ; p+=mtn ;
        if ( cross ) *p = pt->dbdy_x         ; p+=mtn ;
        if ( cross ) *p = set_bdy_direction( pt->bdy_x_cclockwise,
                                             pt->bdy_x_clockwise ) ;
                                               p+=mtn ;
      }
    }
  }
}


/*****************************************/
/* bits in closest are mapped as follows */
/*                                       */
/*       5  6  7                         */
/*       3     4                         */
/*       0  1  2                         */
/*                                       */
/*****************************************/
set_bdy_direction( cclock, clock )
  int cclock,	/* dir to closest boundary from counter-clockwise traversal */
      clock ;   /* dir to closest boundary from clockwise traversal */
{
  /* if they agree, fine.  Just return */
  if ( cclock == clock ) return(cclock) ;

  /* otherwise, resolve the disagrement using one of several strategies */

  if      ( tiebreaker == M_WINS )
  {
    if      ( cclock == RSL_MLOW || cclock == RSL_MHIGH )
      return( cclock ) ;
    else if ( clock == RSL_MLOW || clock == RSL_MHIGH )
      return( clock ) ;
    else
      return( RSL_INVALID ) ;
  }
  else if ( tiebreaker == N_WINS ) 
  {
    if      ( cclock == RSL_NLOW || cclock == RSL_NHIGH )
      return( cclock ) ;
    else if ( clock == RSL_NLOW || clock == RSL_NHIGH )
      return( clock ) ;
    else
      return( RSL_INVALID ) ;
  }
  else if ( tiebreaker == DIAG_WINS )
  {
    if      (( cclock == RSL_MLOW &&  clock == RSL_NLOW ) ||
	     (  clock == RSL_MLOW && cclock == RSL_NLOW ))
      return( RSL_00 ) ;
    else if (( cclock == RSL_MHIGH &&  clock == RSL_NLOW )||
	     (  clock == RSL_MHIGH && cclock == RSL_NLOW ))
      return( RSL_M0 ) ;
    else if (( cclock == RSL_MLOW &&  clock == RSL_NHIGH ) ||
	     (  clock == RSL_MLOW && cclock == RSL_NHIGH ))
      return( RSL_0N ) ;
    else if (( cclock == RSL_MHIGH &&  clock == RSL_NHIGH ) ||
	     (  clock == RSL_MHIGH && cclock == RSL_NHIGH ))
      return( RSL_MN ) ;
    else
      return( RSL_INVALID ) ;
  }
  else
    RSL_TEST_ERR(1,"no such strategy") ;
}

