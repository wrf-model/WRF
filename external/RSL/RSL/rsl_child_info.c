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

/* given a coarse domain point and nest index within it, return
   its nest coords */
RSL_CHILD_INFO ( d_p, n_p, ig_p, jg_p, cm_p, cn_p, ni_p, nj_p, nig_p, njg_p )
  int_p d_p, n_p, ig_p, jg_p, cm_p, cn_p ;      /* input */ 
  int_p ni_p, nj_p, nig_p, njg_p ; 				/* output */
{
  int d, nst ;
  int kidid ;
  rsl_domain_info_t *ninfo, *dinfo ;
  rsl_point_t *ndomain, *ddomain ;
  int ig, jg, cn, cm, mlen, nlen, irax_n, irax_m ;

  d = *d_p ;
  nst = *n_p ;
  RSL_TEST_ERR( d < 0 || d > RSL_MAXDOMAINS,
    "rsl_child_info: bad parent domain descriptor" ) ;
  RSL_TEST_ERR( nst < 0 || nst > RSL_MAXDOMAINS,
    "rsl_child_info: bad nested domain descriptor" ) ;
  RSL_TEST_ERR( d == nst,
    "rsl_child_info: domain cannot broadcast to itself" ) ;
  RSL_TEST_ERR( domain_info[nst].parent != d ,
    "rsl_child_info: the nest is not a child of the parent" ) ;

  dinfo = &( domain_info[d]) ;
  ninfo = &( domain_info[nst]) ;
  RSL_TEST_ERR( dinfo->valid != RSL_VALID,
    "rsl_child_info: invalid parent domain" ) ;
  RSL_TEST_ERR( ninfo->valid != RSL_VALID,
    "rsl_child_info: invalid nested domain" ) ;
  ddomain = dinfo->domain ;
  ndomain = ninfo->domain ;

  ig = *ig_p ;
  jg = *jg_p ;
  cn = *cn_p ;
  cm = *cm_p ;
  mlen = dinfo->len_m ;
  nlen = dinfo->len_n ;
  irax_n = dinfo->irax_n ;
  irax_m = dinfo->irax_m ;

  *nig_p = RSL_INVALID ;
  *njg_p = RSL_INVALID ;
  *ni_p = RSL_INVALID ;
  *nj_p = RSL_INVALID ;

  if ( ddomain[INDEX_2(jg,ig,mlen)].children_p != NULL )
  {
    kidid = ddomain[INDEX_2(jg,ig,mlen)].children_p->
		    child[INDEX_2(cn,cm,irax_m)] ;
    *nig_p = ID_IDEX(kidid) ;
    *njg_p = ID_JDEX(kidid) ;
    *ni_p = *nig_p + ninfo->idif ;
    *nj_p = *njg_p + ninfo->jdif ;
  }
  return ;
}

/* given a nested domain pt, return its coarse domain coords and
   position within the cd cell */
RSL_CHILD_INFO1 ( d_p, n_p, nig_p, njg_p, cm_p, cn_p, i_p, j_p, ig_p, jg_p )
  int_p d_p, n_p, nig_p, njg_p ; /* input */
  int_p cm_p, cn_p, i_p, j_p, ig_p, jg_p ; /* output */
{
  int d, nst ;
  int mid ;
  rsl_domain_info_t *ninfo, *dinfo ;
  rsl_point_t *ndomain, *ddomain ;
  int nig, njg, cn, cm, mlen, nlen, irax_n, irax_m ;

  d = *d_p ;
  nst = *n_p ;
  RSL_TEST_ERR( d < 0 || d > RSL_MAXDOMAINS,
    "rsl_child_info1: bad parent domain descriptor" ) ;
  RSL_TEST_ERR( nst < 0 || nst > RSL_MAXDOMAINS,
    "rsl_child_info1: bad nested domain descriptor" ) ;
  RSL_TEST_ERR( d == nst,
    "rsl_child_info1: domain cannot broadcast to itself" ) ;
  RSL_TEST_ERR( domain_info[nst].parent != d ,
    "rsl_child_info1: the nest is not a child of the parent" ) ;

  dinfo = &( domain_info[d]) ;
  ninfo = &( domain_info[nst]) ;
  RSL_TEST_ERR( dinfo->valid != RSL_VALID,
    "rsl_child_info1: invalid parent domain" ) ;
  RSL_TEST_ERR( ninfo->valid != RSL_VALID,
    "rsl_child_info1: invalid nested domain" ) ;
  ddomain = dinfo->domain ;
  ndomain = ninfo->domain ;

  nig = *nig_p ;
  njg = *njg_p ;
  mlen = dinfo->len_m ;
  nlen = dinfo->len_n ;
  irax_n = dinfo->irax_n ;
  irax_m = dinfo->irax_m ;

  mid = ndomain[INDEX_2(njg,nig,mlen)].mother_id ;
  *ig_p = ID_IDEX(mid) ;
  *jg_p = ID_JDEX(mid) ;
  *i_p = *ig_p + dinfo->idif ;
  *j_p = *jg_p + dinfo->jdif ;
  *cn_p = ndomain[INDEX_2(njg,nig,mlen)].which_kid_am_i_n ;
  *cm_p = ndomain[INDEX_2(njg,nig,mlen)].which_kid_am_i_m ;

  return ;
}

