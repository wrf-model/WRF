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

#define DEFINE_GLOBAL

#include <stdio.h>
#include <stdlib.h>
#include "rsl.h"

rsl_list_t *point_move_receives[ RSL_MAXPROC ] ;
rsl_list_t *point_move_sends[ RSL_MAXPROC ] ;

/*@
  RSL_FDECOMPOSE Decompose domain using user-supplied function.

  Notes:
  This routine decomposes the a domain using a function provided
  by the user.  The prototypical mapping function is described below.

  Mapping function:
  The user-supplied function for decomposing the domain should have
  the following form.

  Verbatim:
$  INTEGER FUNCTION MAPPING ( in, out, info_p, m, n, py, px )
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

@*/

RSL_FDECOMPOSE (d_p,fcn,py_p,px_p,info_p,mloc_p,nloc_p,zloc_p,
                                         mloc_mz_p,nloc_mz_p,zloc_mz_p,
                                         mloc_nz_p,nloc_nz_p,zloc_nz_p )
  int_p
    d_p  ;      /* (I) domain descriptor */
  int 
   (*fcn)() ;   /* (I) decomposition function */
  int_p
    py_p        /* (I) number of processors in y dimension */
   ,px_p        /* (I) number of processors in x dimension */
   ,info_p	     /* (I) extra argument, passed as-is to fcn */
   ,mloc_p      /* (O) minimum m size of local array on this processor */
   ,nloc_p      /* (O) minimum n size of local array on this processor */
   ,zloc_p      /* (O) minimum n size of local array on this processor */
   ,mloc_mz_p   /* (O) minimum m size of local array on this processor */
   ,nloc_mz_p   /* (O) minimum n size of local array on this processor */
   ,zloc_mz_p   /* (O) minimum n size of local array on this processor */
   ,mloc_nz_p   /* (O) minimum m size of local array on this processor */
   ,nloc_nz_p   /* (O) minimum n size of local array on this processor */
   ,zloc_nz_p   /* (O) minimum n size of local array on this processor */
   ;
{

  int d, nest, state, mlen, nlen, zlen, i, j, k, m, n, z, px, py, px1, py1 ;
  int result ;
  int mom ;
  int kn, km, kp, np ;
  int was_decomposed ;
  rsl_domain_info_t *dinfo, *ninfo ;
  rsl_point_t *domain, *ndomain, *p ;
  rsl_child_info_t *moms_kids, *my_kids ;
  int *wrk1, *wrk2 ;
  rsl_list_t *lp ;
  int P, my_P ;
  int l, h, lz, hiz ;
  int retval ;

  retval = 0 ;
  d = *d_p ;
  px = *px_p ;
  py = *py_p ;

  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS, 
                "rsl_fdecompose: bad domain descriptor" ) ;
  dinfo = &( domain_info[d] ) ;
  RSL_TEST_ERR( dinfo->valid != RSL_VALID,
                "rsl_fdecompose: invalid domain descriptor" ) ;
  mlen = dinfo->len_m ;
  nlen = dinfo->len_n ;
  zlen = dinfo->len_z ;

/* set the ones that aren't decomposed */
  *zloc_p = zlen ;
  *nloc_mz_p = nlen + 2 * rsl_padarea ;
  *mloc_nz_p = mlen + 2 * rsl_padarea ;

  if ( zlen > 1 ) {
/* figure out MZ decomp: added 20010222 -- for 3d decomposition for xposes */
    wrk1 = RSL_MALLOC( int, mlen * zlen ) ;
    wrk2 = RSL_MALLOC( int, mlen * zlen ) ;
    domain = dinfo->domain_mz ;

  /* load up input work array with valid points */
    for ( k = 0 ; k < zlen ; k++ )
      for ( i = 0 ; i < mlen ; i++ )
      {
        if ( domain[ INDEX_2(k,i,mlen) ].valid == RSL_VALID )
          wrk1[INDEX_2(k,i,mlen)] = RSL_VALID ;
        else
          wrk1[INDEX_2(k,i,mlen)] = RSL_INVALID ;
      }

    m = mlen ; z = zlen ; px1 = px ; py1 = py ;  /* protect variables */
    result = rsl_default_decomp( wrk1, wrk2, info_p, &m, &z, &px1, &py1 ) ;

    if ( ! result )
    {
      /* set the processor numbers of the valid points */
      l = -1 ; h = -1 ; lz = -1 ; hiz = -1 ;
      for ( k = 0 ; k < zlen ; k++ ) {
        for ( i = 0 ; i < mlen ; i++ ) {
          p = &(domain[ INDEX_2(k,i,mlen) ]) ;
	  p->valid = RSL_VALID ;
          p->P = wrk2[INDEX_2(k,i,mlen)] ;
          if ( rsl_c_comp2phys_proc(p->P) == rsl_myproc ) {
	    if ( l == -1 ) l = i ;
	    h = i ;
	    if ( lz == -1 ) lz = k ;
	    hiz = k ;
          }
        }
      }
      *mloc_mz_p = h-l+1 + 2 * rsl_padarea ;
      *zloc_mz_p = hiz-lz+1 ;
    }

    RSL_FREE( wrk1 ) ;
    RSL_FREE( wrk2 ) ;

/* figure out NZ decomp: added 20010222 -- for 3d decomposition for xposes */
    wrk1 = RSL_MALLOC( int, nlen * zlen ) ;
    wrk2 = RSL_MALLOC( int, nlen * zlen ) ;
    domain = dinfo->domain_nz ;

  /* load up input work array with valid points */
    for ( k = 0 ; k < zlen ; k++ )
      for ( j = 0 ; j < nlen ; j++ )
      {
        if ( domain[ INDEX_2(k,j,nlen) ].valid == RSL_VALID )
          wrk1[INDEX_2(k,j,nlen)] = RSL_VALID ;
        else
          wrk1[INDEX_2(k,j,nlen)] = RSL_INVALID ;
      }

    n = nlen ; z = zlen ; px1 = px ; py1 = py ;  /* protect variables */
    result = rsl_default_decomp( wrk1, wrk2, info_p, &n, &z, &px1, &py1 ) ;

    if ( ! result )
    {
      /* set the processor numbers of the valid points */
      l = -1 ; h = -1 ; lz = -1 ; hiz = -1 ;
      for ( k = 0 ; k < zlen ; k++ ) {
        for ( j = 0 ; j < nlen ; j++ ) {
          p = &(domain[ INDEX_2(k,j,nlen) ]) ;
	  p->valid = RSL_VALID ;
          p->P = wrk2[INDEX_2(k,j,nlen)] ;
          if ( rsl_c_comp2phys_proc(p->P) == rsl_myproc ) {
	    if ( l == -1 ) l = j ;
	    h = j ;
	    if ( lz == -1 ) lz = k ;
	    hiz = k ;
          }
        }
      }
      *nloc_nz_p = h-l+1 + 2 * rsl_padarea ;
      *zloc_nz_p = hiz-lz+1 ;
    }

    RSL_FREE( wrk1 ) ;
    RSL_FREE( wrk2 ) ;

  }
/* end of changes for xposes -- 20010222 */

  wrk1 = RSL_MALLOC( int, mlen * nlen ) ;
  wrk2 = RSL_MALLOC( int, mlen * nlen ) ;
  domain = dinfo->domain ;

  /* load up input work array with valid points */
  for ( j = 0 ; j < nlen ; j++ )
    for ( i = 0 ; i < mlen ; i++ )
    {
      if ( domain[ INDEX_2(j,i,mlen) ].valid == RSL_VALID )
        wrk1[INDEX_2(j,i,mlen)] = RSL_VALID ;
      else
        wrk1[INDEX_2(j,i,mlen)] = RSL_INVALID ;
    }

  if ( fcn != NULL )
  {
    m = mlen ; n = nlen ; px1 = px ; py1 = py ;  /* protect variables */
    result = (*fcn)( wrk1, wrk2, info_p, &m, &n, &py1, &px1 ) ;
  }
  else
  {
    /* call internal decomposition routine */
    m = mlen ; n = nlen ; px1 = px ; py1 = py ;  /* protect variables */
    result = rsl_default_decomp( wrk1, wrk2, info_p, &m, &n, &py1, &px1 ) ;
  }


  if ( result )
  {
    if ( dinfo->decomposed )
    {
      RSL_TEST_WRN( 1, "RSL_FDECOMPOSE: Decomposition not changed." ) ;
      return(1) ;
    }
    else
    {
      RSL_TEST_WRN( 1, "RSL_FDECOMPOSE: method failure -- not decomposed" ) ;
      return(1) ;
    }
  }

  if ( dinfo->decomposed )
  {
    /* make a note that there was a previous decomposition and
       store it in wrk1 for use in figuring out what needs to 
       be moved to get from the old decomp to the new one */
    was_decomposed = 1 ;
    for ( j = 0 ; j < nlen ; j++ )
    {
      for ( i = 0 ; i < mlen ; i++ )
      {
	wrk1[INDEX_2(j,i,mlen)] = domain[ INDEX_2(j,i,mlen) ].P ;
      }
    }
  }

  destroy_decomposition( d_p ) ;                /* no backing out now */

#ifdef ENABLE_READ_DECOMP
result = read_domain_decomp ( d, wrk2,  m, n ) ;  /* 20030505 */
#endif

  for ( P = 0 ; P < RSL_MAXPROC ; P++ )
  {
    destroy_list( &(point_move_receives[P]), NULL ) ;
    destroy_list( &(point_move_sends[P]), NULL ) ;
  }

  /* set the processor numbers of the valid points */
  for ( j = 0 ; j < nlen ; j++ )
  {
    for ( i = 0 ; i < mlen ; i++ )
    {
      if ( wrk2[INDEX_2(j,i,mlen)] != RSL_INVALID )
      {
        p = &(domain[ INDEX_2(j,i,mlen) ]) ;
/* added 5/16/95 */
/* If a previous decomposition exists, build a list of point moves to each processor */
	if ( was_decomposed )
	{
	  /* figure out the sends */
	  /* note: wrk1 contains the old decomposition; wrk2 the new one */
	  if (( rsl_c_comp2phys_proc(wrk1[INDEX_2(j,i,mlen)]) == rsl_myproc))
	  {
	    lp = RSL_MALLOC( rsl_list_t, 1 ) ;
	    lp->info1 = i ;
	    lp->info2 = j ;
	    lp->next = point_move_sends[wrk2[INDEX_2(j,i,mlen)]] ;
            point_move_sends[wrk2[INDEX_2(j,i,mlen)]] = lp ;
	  }
	  else
	  if (( rsl_c_comp2phys_proc(wrk2[INDEX_2(j,i,mlen)] ) == rsl_myproc )) 
	  {
	    int idex ;

	    idex = wrk1[INDEX_2(j,i,mlen)] ;
	    lp = RSL_MALLOC( rsl_list_t, 1 ) ;
	    lp->info1 = i ;
	    lp->info2 = j ;
	    lp->next = point_move_receives[idex] ;
            point_move_receives[idex] = lp ;
	  }
	}

	p->valid = RSL_VALID ;
        p->P = wrk2[INDEX_2(j,i,mlen)] ;
        /* if this is a child of point in the next domain up, let the
	   mother know where the kid is going */
	if (( mom = p->mother_id ) != RSL_INVALID )
	{
	  p->mother_P =
	    domain_info[ID_DOMAIN(mom)].
	      domain[INDEX_2(ID_JDEX(mom),
			     ID_IDEX(mom),
			     domain_info[ID_DOMAIN(mom)].len_m )].P ;
	  moms_kids =
	    domain_info[ID_DOMAIN(mom)].
	      domain[INDEX_2(ID_JDEX(mom),
			     ID_IDEX(mom),
			     domain_info[ID_DOMAIN(mom)].len_m )].children_p ;
          /* search for me in mom's list -- mom! remember your own kid? */
	  for ( kn = 0 ; kn < dinfo->irax_n ; kn++ )
	  {
	    for ( km = 0 ; km < dinfo->irax_m ; km++ )
	    {
	      if ( moms_kids->child[INDEX_2(kn,km,dinfo->irax_m)] ==
		   POINTID( d, j, i ))
	      {
		/* she remembered!
		   here's my processsor, mom. don't be a stranger, OK?  */
		moms_kids->P[ INDEX_2(kn,km,dinfo->irax_m) ] = 
		   domain[ INDEX_2(j,i,mlen) ].P ;
		break ;
	      }
	    }
	  }
	}
#if 0
	/* added 4/21/95 -- there should never be a point without an
	   associated mother id at this point in the program */
	else 
	{
	  /* note, the mother domain would not have a parent */
	  if ( d != 0 )
	  {
	    sprintf(mess,"Point %d %d on domain %d has no mother.\n",
		  i,j,d ) ;
	    RSL_TEST_ERR(1,mess)
	  }
	}
#endif

        /* this bit informs the children associated with this point
           that the processor allocation for this point has set */
	my_P = domain[ INDEX_2(j,i,mlen) ].P ;
	if (( my_kids = p->children_p ) != NULL )
	{
	  /* the the nest id and the nest info for my chidren */
	  nest = ID_DOMAIN(my_kids->child[0]) ;
	  ninfo = &(domain_info[nest]) ;
	  ndomain = ninfo->domain ;
	  /* */
	  for ( kp = 0 ; kp < ninfo->irax_n * ninfo->irax_m ; kp++ )
	  {
	    np = my_kids->child[kp] ;
	    ndomain[INDEX_2(ID_JDEX(np),ID_IDEX(np),
		    domain_info[ID_DOMAIN(np)].len_m )].mother_P = my_P ;
	  }
	}
      }
    }
  }

  /* note that this call will set mloc_p and nloc_p, which can then
     be used by the calling program to allocate buffers of the
     appropriate size, if desired.  NOTE ALSO, THAT THE EXTRA MEMORY
     FOR THE PADS IS INCLUDED. */
  dinfo->decomposed = 1 ;
  rsl_new_decomposition( d_p, mloc_p, nloc_p ) ;

  /* TODO code to move state vector stuff around needs to be added */

  RSL_FREE( wrk1 ) ;
  RSL_FREE( wrk2 ) ;
  return(retval) ;
}

destroy_decomposition( d_p )
  int_p d_p ;
{
  int d, nest ;
  int mlen, nlen, i, j, mom, np ;
  int kn, km, kp ;
  rsl_domain_info_t *dinfo, *ninfo ;
  rsl_point_t *domain, *ndomain, *p ;
  rsl_child_info_t *moms_kids ;
  rsl_child_info_t *my_kids ;
  int destroy_runrec() ;

  d = *d_p ;

  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
                "rsl_fdecompose: bad domain descriptor" ) ;
  dinfo = &( domain_info[d] ) ;
  domain = dinfo->domain ;
  RSL_TEST_ERR( dinfo->valid != RSL_VALID,
                "rsl_fdecompose: invalid domain descriptor" ) ;

  dinfo->decomposed = 0 ;

  /* uncompile stencils associated with domain */
  /* note: we aren't getting rid of them, just uncompiling them */
  for ( i = 0 ; i < dinfo->stencurs ; i++ )
  {
    if ( dinfo->stenlist[i] != RSL_INVALID )
      uncompile_stencil_on_domain( d, sh_descriptors[dinfo->stenlist[i]] ) ;
  }

  /* TODO uncompile bcast/merges associated with domain */

  mlen = dinfo->len_m ; nlen = dinfo->len_n ;

  for ( j = 0 ; j < nlen ; j++ )
  {
    for ( i = 0 ; i < mlen ; i++ )
    {
      p = &(domain[ INDEX_2(j,i,mlen) ]) ;
      p->P = RSL_INVALID ;
      if ( p->valid )
      {
	/* this bit informs the mother domain point associated with this point
	   that the processor allocation for this point has been invalidated */
	if (( mom = p->mother_id ) != RSL_INVALID )
	{
          moms_kids =
            domain_info[ID_DOMAIN(mom)].
              domain[INDEX_2(ID_JDEX(mom),
                             ID_IDEX(mom),
                             domain_info[ID_DOMAIN(mom)].len_m )].children_p ;
          for ( kn = 0 ; kn < dinfo->irax_n ; kn++ )
          {
            for ( km = 0 ; km < dinfo->irax_m ; km++ )
            {
              if ( moms_kids->child[INDEX_2(kn,km,dinfo->irax_m)] ==
                   POINTID( d, j, i ))
              {
                /*  I've lost my processor, ma.  Goodbye. */
                moms_kids->P[ INDEX_2(kn,km,dinfo->irax_m) ] = RSL_INVALID ;
                break ;
              }
            }
          }
        }
	/* this bit informs the children associated with this point
	   that the processor allocation for this point has been invalidated */
	if (( my_kids = p->children_p ) != NULL )
	{
	  /* the the nest id and the nest info for my chidren */
	  nest = ID_DOMAIN(my_kids->child[0]) ;
	  ninfo = &(domain_info[nest]) ;
	  ndomain = ninfo->domain ;
	  /* */
	  for ( kp = 0 ; kp < ninfo->irax_n * ninfo->irax_m ; kp++ )
	  {
	    np = my_kids->child[kp] ;
	    ndomain[INDEX_2(ID_JDEX(np),ID_IDEX(np),
		    domain_info[ID_DOMAIN(np)].len_m )].mother_P = RSL_INVALID ;
	  }
	}
      }
    }
  }

  for ( i = 0 ; i < RSL_MAXDOMAINS ; i++ )
  {
    domain_info[d].child_bcast_compiled[i] = 0 ;
    domain_info[d].child_merge_compiled[i] = 0 ;
  }
  domain_info[d].parent_bcast_compiled = 0 ;
  domain_info[d].parent_merge_compiled = 0 ;

/* store for move */
  domain_info[d].old_ilocaloffset = domain_info[d].ilocaloffset ;
  domain_info[d].old_jlocaloffset = domain_info[d].jlocaloffset ;
  domain_info[d].ilocaloffset = RSL_INVALID ;
  domain_info[d].jlocaloffset = RSL_INVALID ;
  domain_info[d].loc_m = RSL_INVALID ;
  domain_info[d].loc_n = RSL_INVALID ;
  destroy_list( &(domain_info[d].pts), NULL ) ;
  destroy_list( &(domain_info[d].ghost_pts), NULL ) ;
  destroy_list( &(domain_info[d].iruns), destroy_runrec ) ;
  { int p ;
    for ( p = 0 ; p < MAX_KINDPAD ; p++ )
    {
      if ( domain_info[d].js[p] != NULL ) RSL_FREE( domain_info[d].js[p] ) ;
      if ( domain_info[d].is[p] != NULL ) RSL_FREE( domain_info[d].is[p] ) ;
      if ( domain_info[d].ie[p] != NULL ) RSL_FREE( domain_info[d].ie[p] ) ;
      if ( domain_info[d].ie[p] != NULL ) RSL_FREE( domain_info[d].jg2n[p] ) ;
      domain_info[d].js[p] = NULL ;
      domain_info[d].is[p] = NULL ;
      domain_info[d].ie[p] = NULL ;
      domain_info[d].jg2n[p] = NULL ;
      if ( domain_info[d].js[p] != NULL ) RSL_FREE( domain_info[d].is2[p] ) ;
      if ( domain_info[d].is[p] != NULL ) RSL_FREE( domain_info[d].js2[p] ) ;
      if ( domain_info[d].ie[p] != NULL ) RSL_FREE( domain_info[d].je2[p] ) ;
      if ( domain_info[d].ie[p] != NULL ) RSL_FREE( domain_info[d].ig2n[p] ) ;
      domain_info[d].is2[p] = NULL ;
      domain_info[d].js2[p] = NULL ;
      domain_info[d].je2[p] = NULL ;
      domain_info[d].ig2n[p] = NULL ;
    }
  }
}

