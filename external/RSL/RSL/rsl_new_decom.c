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

/* routine and local data used by rsl_new_decomposition */
static int count_neighbors_count ;
static rsl_processor_t count_neighbors_myproc ;
count_neighbors( d, m, n, hm, hn, pt, ipt )
  rsl_index_t d ;
  rsl_index_t m, n ;        /* this point */
  rsl_index_t hm, hn ;      /* home point (whose stencil I'm on)  */
  rsl_index_t pt ;              /* point in stencil */
  rsl_index_t ipt ;             /* inverse point in stencil */
{
  int mlen ;
  rsl_point_t *domain ;
  rsl_domain_info_t *dinfo ;

  dinfo = &(domain_info[d]) ;
  domain = dinfo->domain ;
  mlen = dinfo->len_m ;

  if ( (domain[INDEX_2(n,m,mlen)].info_1 == 0)
        && (domain[INDEX_2(n,m,mlen)].P != RSL_INVALID) /* 970216 */
        && (domain[INDEX_2(n,m,mlen)].P != count_neighbors_myproc ) )
  {
    domain[INDEX_2(n,m,mlen)].info_1 = 1 ;    /* mark as counted */
    count_neighbors_count++ ;           /* increment counter */
  }
}


/* note that this has been changed to an internal routine that
   is callable for a single domain at a time */
int
rsl_new_decomposition( d_p, mloc_p, nloc_p )
  int_p d_p, 
        mloc_p, nloc_p ; /* output: minimum sizes to hold local partition */
{
  int (*pt)(), (*f)() ;
  int d, m, n ;
  int mlen, nlen, zlen, meff, neff ;
  int P ;
  int i, j, k, size ;
  int firsti, firstj, firstk ;
  int mtype, mdest, retval ;
  int maskid ;
  int no_points ;
  rsl_list_t *lp, *tlp[RSL_MAXDOMAINS] ;
  rsl_domain_info_t *dinfo ;
  rsl_point_t *domain ;

  /* added 3/23/95 */
  int nrun, eff_n, dex ;
  rsl_list_t *rp ;

  extern rsl_4pt(), rsl_8pt(), rsl_12pt(), rsl_24pt(), rsl_48pt(),
         rsl_80pt(), rsl_120pt(), rsl_168pt(), count_neighbors() ;
#if ( ALLOW_RSL_168PT == 1 )
  extern rsl_168pt() ;
#endif

  P = rsl_c_phys2comp_proc(rsl_myproc) ;
  d = *d_p ;

  dinfo = &(domain_info[d]) ;

/* 20010228 */

/* work out the i, j, and k offsets for the transpose arrays */
/* MZ */
  domain = dinfo->domain_mz ;
  mlen = dinfo->len_m ;
  nlen = dinfo->len_n ;
  zlen = dinfo->len_z ;
  firsti = RSL_INVALID ;
  firstk = RSL_INVALID ;
  for ( k = 0 ; k < zlen ; k++ )
  {
    for ( i = 0 ; i < mlen ; i++ )
    {
      if ( rsl_c_comp2phys_proc ( domain[INDEX_2(k,i,mlen)].P ) == rsl_myproc )
      {
	if ( firsti == RSL_INVALID ) firsti = i ;
	if ( firstk == RSL_INVALID ) firstk = k ;
      }
    }
  }
  dinfo->ilocaloffset_mz = firsti - rsl_padarea ;
  dinfo->klocaloffset_mz = firstk ;
  dinfo->jlocaloffset_mz = -rsl_padarea ;

/* NZ */
  domain = dinfo->domain_nz ;
  mlen = dinfo->len_m ;
  nlen = dinfo->len_n ;
  zlen = dinfo->len_z ;
  firstj = RSL_INVALID ;
  firstk = RSL_INVALID ;
  for ( k = 0 ; k < zlen ; k++ )
  {
    for ( j = 0 ; j < nlen ; j++ )
    {
      if ( rsl_c_comp2phys_proc ( domain[INDEX_2(k,j,nlen)].P ) == rsl_myproc )
      {
	if ( firstj == RSL_INVALID ) firstj = j ;
	if ( firstk == RSL_INVALID ) firstk = k ;
      }
    }
  }
  dinfo->jlocaloffset_nz = firstj - rsl_padarea ;
  dinfo->klocaloffset_nz = firstk ;
  dinfo->ilocaloffset_nz = -rsl_padarea ;

/* end 20010228 */

  domain = dinfo->domain ;
  mlen = dinfo->len_m ;
  nlen = dinfo->len_n ;

  for ( n = 0 ; n < nlen ; n++ )
    for ( m = 0 ; m < mlen ; m++ )
    {
      domain[INDEX_2(n,m,mlen)].info_1 = 0 ;    /* mark all untouched */
    }

  maskid = dinfo->maskid ;
  switch( maskid )
  {

  case RSL_4PT   : pt = rsl_4pt  ; break ;
  case RSL_8PT   : pt = rsl_8pt  ; break ;
  case RSL_12PT  : pt = rsl_12pt ; break ;
  case RSL_24PT  : pt = rsl_24pt ; break ;
  case RSL_48PT  : pt = rsl_48pt ; break ;
  case RSL_80PT  : pt = rsl_80pt ; break ;
  case RSL_120PT  : pt = rsl_120pt ; break ;
#if ( ALLOW_RSL_168PT == 1 )
  case RSL_168PT  : pt = rsl_168pt ; break ;
#endif
  default: 
     RSL_TEST_ERR(1,"rsl_new_decomposition(): bad mask spec") ; break ;

  }
  mlen = domain_info[d].len_m ;  /* actual dimensions */
  nlen = domain_info[d].len_n ;
  meff = domain_info[d].eff_m ;  /* effective dimensions */
  neff = domain_info[d].eff_n ;
  firsti = RSL_INVALID ;
  firstj = RSL_INVALID ;
  for ( n = 0 ; n < nlen ; n++ )
  {
    for ( m = 0 ; m < mlen ; m++ )
    {
      if ( domain[INDEX_2(n,m,mlen)].P == P ) 
      {
        /* this is assigned to the processor I'm looking at (me).
           count it and it's untouched off proc neighbors */
        /* set firsti and firstj for the computation of 
           ioffset and joffset, later.  Note, what we're really
           doing is finding the minimum index in each dimension. */
        if ( domain[INDEX_2(n,m,mlen)].info_1 == 0 )
        {
          if ( firsti == RSL_INVALID ) 
          {
            firsti = m ;
          }
          else
          {
            if ( m < firsti ) firsti = m ;
          }
          if ( firstj == RSL_INVALID ) 
          {
            firstj = n ;
          }
          else
          {
            if ( n < firstj ) firstj = n ;
          }
          domain[INDEX_2(n,m,mlen)].info_1 = 1 ;

          count_neighbors_myproc = P ;
          count_neighbors_count = 0 ;
          /* pt is the stencil function set above in this routine,
             count_neighbors is defined in this file */
          (*pt)( d, m, mlen, n, nlen, count_neighbors ) ;
        }
      }
    }
  }

  /* work out the sizes of the local memory requirements for the domain */
  /* for now, this information is just passed back to the caller; at some
     point we could optionally allocate the domains, here or elsewhere
     in RSL */

  no_points=0;
  if ( firsti == RSL_INVALID || firstj == RSL_INVALID )
  {
    no_points=1 ;
    /* it so happens that this processor doesn't have any points */
    *mloc_p = 0 ;
    *nloc_p = 0 ;
#if 0
/* removed this return so that the js (and so forth) fields in the domain_info
   structure get memory allocated  -- 981227 JM */
    return ;                    /* RETURN */
#endif
  }
  else
  {
    int mlow, nlow, mhigh, nhigh ;
    mlow =   99999999 ;
    nlow =   99999999 ;
    mhigh = -99999999 ;
    nhigh = -99999999 ;

    for ( n = 0 ; n < nlen ; n++ )
    {
      for ( m = 0 ; m < mlen ; m++ )
      {
        if ( domain[INDEX_2(n,m,mlen)].P == P )
        {
          if ( m < mlow  ) mlow  = m ;
          if ( m > mhigh ) mhigh = m ;
          if ( n < nlow  ) nlow  = n ;
          if ( n > nhigh ) nhigh = n ;
        }
      }
    }
#ifndef vpp
    *mloc_p = mhigh - mlow + 1 + 2 * rsl_padarea ;
#else
    *mloc_p = mhigh - mlow + 1 ;
#endif
    *nloc_p = nhigh - nlow + 1 + 2 * rsl_padarea ;
    dinfo->loc_m = *mloc_p ;
    dinfo->loc_n = *nloc_p ;
  }


#ifndef vpp
  dinfo->ilocaloffset = firsti-rsl_padarea ;
#else
  dinfo->ilocaloffset = firsti ;
#endif

  if ( old_offsets == 0 )
  {
    dinfo->jlocaloffset = firstj-rsl_padarea ;
  }
  else
  {
    if ( dinfo->nest_level == 0 )
    {
      dinfo->jlocaloffset = firstj-rsl_padarea ;
    }
    else
    {
      int prevsize, i, pid /*parent id*/, did /*domain id*/;
      prevsize = 0 ;
      did = d ;
      for ( i = 0 ; i < dinfo->nest_level ; i++ )
      {
        pid = domain_info[did].parent ;
        /*
        prevsize +=  2*rsl_padarea + domain_info[pid].loc_n ;
        */
        prevsize +=  domain_info[pid].loc_n ;
        did = pid ;
      }
      RSL_TEST_ERR( (did != 0) , "internal error" ) ;
      dinfo->jlocaloffset = firstj - rsl_padarea - prevsize ;
    }
  }

/* went back to use of actual dimensions -- will test for effective
   dimensions using whether the point was trimmed */
  for ( n = 0 ; n < nlen ; n++ )
  {
    for ( m = 0 ; m < mlen ; m++ )
    {
      if ( domain[INDEX_2(n,m,mlen)].trimmed == 0 )
      {
        if ( rsl_c_comp2phys_proc (domain[INDEX_2(n,m,mlen)].P) == rsl_myproc )
        {
          /* a local point -- add to list of local points */
          lp = RSL_MALLOC( rsl_list_t, 1 ) ;
          lp->data = &(domain[INDEX_2(n,m,mlen)]) ;
          lp->next = NULL ;
          /* add to end of list of local points */
          if ( domain_info[d].pts == NULL )
          {
             domain_info[d].pts = lp ;
             tlp[d] = lp ;
          }
          else
          {
             tlp[d]->next = lp ;
             tlp[d] = lp ;
          }
        }
        /* this test is necessary to avoid counting everyone in the
           domain -- we only want those who have been marked as neighbors
           by the count_neighbors function */
        else if ( domain[INDEX_2(n,m,mlen)].info_1 == 1 )
        {
          lp = RSL_MALLOC( rsl_list_t, 1 ) ;
          lp->data = &(domain[INDEX_2(n,m,mlen)]) ;
          lp->next = domain_info[d].ghost_pts ;
          domain_info[d].ghost_pts = lp ;
        }
      }
    }
  }

  /* 1/9/95.  compute runs through partition for rsl_compute_islab */

  {
    int retval, i, j, ig, jg, prev_ig, prev_jg ;
    int start_i, start_j, start_ig, start_jg, runlength ;
    rsl_list_t *rp, *trp[RSL_MAXDOMAINS] ;
    rsl_runrec_t *rr ;


    RSL_INIT_NEXTCELL ( &d ) ;
    RSL_C_NEXTCELL ( &d, &i, &j, &ig, &jg, &retval ) ;
    i-- ; j-- ; ig-- ; jg-- ;         /* base 0 for C */
    start_i = i ;
    start_j = j ;
    start_ig = ig ;
    start_jg = jg ;
    runlength = 1 ;
    prev_ig = ig-1 ;
    prev_jg = jg ;
    while ( retval != 0 )
    {
      if ( ! ( ig == prev_ig+1 && jg == prev_jg ) )
      {
        rr = RSL_MALLOC( rsl_runrec_t, 1 ) ;
        rr->i  = start_i ;
        rr->j  = start_j ;
        rr->ig = start_ig ;
        rr->jg = start_jg ;
        rr->runlength = runlength-1 ;
        rp = RSL_MALLOC( rsl_list_t, 1 ) ;
        rp->data = rr ;
        rp->next = NULL ;
        /* add to list of local points */
        if ( domain_info[d].iruns == NULL )
        {
           domain_info[d].iruns = rp ;
           trp[d] = rp ;
        }
        else
        {
           trp[d]->next = rp ;
           trp[d] = rp ;
        }
        runlength = 1 ;
        start_i = i ;
        start_j = j ;
        start_ig = ig ;
        start_jg = jg ;
      }
      prev_ig = ig ;
      prev_jg = jg ;
      RSL_C_NEXTCELL ( &d, &i, &j, &ig, &jg, &retval ) ;
      if ( retval == 1 ) runlength++ ;
      i-- ; j-- ; ig-- ; jg-- ;       /* base 0 for C */
    }
    /* handle last one */
    if ( runlength != 0 )
    {
        rr = RSL_MALLOC( rsl_runrec_t, 1 ) ;
        rr->i  = start_i ;
        rr->j  = start_j ;
        rr->ig = start_ig ;
        rr->jg = start_jg ;
        rr->runlength = runlength ;
        rp = RSL_MALLOC( rsl_list_t, 1 ) ;
        rp->data = rr ;
        rp->next = NULL ;
        /* add to list of local points */
        if ( domain_info[d].iruns == NULL )
        {
           domain_info[d].iruns = rp ;
           trp[d] = rp ;
        }
        else
        {
           trp[d]->next = rp ;
           trp[d] = rp ;
        }
    }
  }

  /* 3/22/95.  compute 2d runs through partition for rsl_compute */
  { 
    int p ;
    int i, j, ig, jg, prev_ig, prev_jg ;
    int start_i, start_j, start_ig, start_jg, runlength ;
    int nrun ;
    int m, mlen ;
    int n, nlen ;
    int ilocaloffset, jlocaloffset ;
    int first ;
    int square ;

    for ( p =  0 ; p <= MAX_KINDPAD ; p++ )
    {
      mlen = domain_info[d].len_m ;
      nlen = domain_info[d].len_n ;
      for ( n = 0 ; n < nlen ; n++ )
        for ( m = 0 ; m < mlen ; m++ )
        {
          domain[INDEX_2(n,m,mlen)].info_1 = 0 ;    /* mark all untouched */
        }

      if ( p > 0 )
      {
        for ( n = 0 ; n < nlen ; n++ )
        {
          for ( m = 0 ; m < mlen ; m++ )
          {
            if ( domain[INDEX_2(n,m,mlen)].P == P ) 
            {
              /* this is assigned to the processor I'm looking at (me).
                 count it and it's untouched off proc neighbors */
                count_neighbors_myproc = P ;
                count_neighbors_count = 0 ;
              /* pt is the stencil function set above in this routine,
                 count_neighbors is defined in this file */
              switch ( p )
              {
              case 1 :
                rsl_8pt( d, m, mlen, n, nlen, count_neighbors ) ;   
                break ;
              case 2 :
#ifdef INLINE_COUNTS
                square = 2 ;
                /* note fall through */
#else
                rsl_24pt( d, m, mlen, n, nlen, count_neighbors ) ;  
                break ;
#endif
	      case 3 :
#ifdef INLINE_COUNTS
                if ( sw_allow_dynpad )
                {
                  square = 3 ;
                  /* note fall through */
                }
                else
                {
                  rsl_2ptm( d, m, mlen, n, nlen, count_neighbors ) ; /* 1 extra ns */
                  break ;
                }
#else
		if ( sw_allow_dynpad )
		{
                  rsl_48pt( d, m, mlen, n, nlen, count_neighbors ) ;
		}
		else
		{
                  rsl_2ptm( d, m, mlen, n, nlen, count_neighbors ) ; /* 1 extra ns */
		}
                break ;
#endif
	      case 4 :
#ifdef INLINE_COUNTS
		if ( sw_allow_dynpad )
		{
                  square = 4 ;
                  /* note fall through */
		}
		else
		{
                  rsl_4ptm( d, m, mlen, n, nlen, count_neighbors ) ; /* 2 extra ns */
                  break ;
		}
#else
		if ( sw_allow_dynpad )
		{
                  rsl_80pt( d, m, mlen, n, nlen, count_neighbors ) ;
		}
		else
		{
                  rsl_4ptm( d, m, mlen, n, nlen, count_neighbors ) ; /* 2 extra ns */
		}
                break ;
#endif
	      case 5 :
#ifdef INLINE_COUNTS
                /* note fall through */
                square = 5 ;
#else
                rsl_120pt( d, m, mlen, n, nlen, count_neighbors ) ;
                break ;
#endif
	      case 6 :
#ifdef INLINE_COUNTS
                square = 6 ;
{
/* this code comes from pt.c, rsl_168pt */
  rsl_index_t min = m ;
  rsl_dimlen_t minlen = mlen ;
  rsl_index_t maj = n ;
  rsl_dimlen_t majlen = nlen ;

  rsl_index_t i, j ;

  for ( i = -square ; i <= square ; i++ )
  {
    for ( j = -square ; j <= square ; j++ )
    {
      if ( min+i >= 0 && min+i < minlen &&
             maj+j >= 0 && maj+j < majlen )
      {

# ifdef INLINE_COUNTS
/* this code is count_neighbors, above, in this file */
{
  rsl_index_t m=min+i, n=maj+j ;        /* this point */
  rsl_index_t hm=min, hn=maj ;      /* home point (whose stencil I'm on)  */

  if ( (domain[INDEX_2(n,m,mlen)].info_1 == 0)
        && (domain[INDEX_2(n,m,mlen)].P != RSL_INVALID) /* 970216 */
        && (domain[INDEX_2(n,m,mlen)].P != count_neighbors_myproc ) )
  {
    domain[INDEX_2(n,m,mlen)].info_1 = 1 ;    /* mark as counted */
    count_neighbors_count++ ;           /* increment counter */
  }
}
# else
        (*f)(d,min+i,maj+j,min,maj,pts168[k],ipts168[k]) ;
# endif
      }
    }
  }
}

#else
                rsl_168pt( d, m, mlen, n, nlen, count_neighbors ) ; 
#endif
                break ;
	      /* special cases */
              case 7 :
                rsl_2ptm( d, m, mlen, n, nlen, count_neighbors ) ; /* 1 extra ns */
                break ;
              case 8 :
                rsl_4ptm( d, m, mlen, n, nlen, count_neighbors ) ; /* 2 extra ns */
                break ;
              default :
                sprintf(mess,"internal error p=%d\n",p) ;
                RSL_TEST_ERR(1,mess) ; 
              }
            }
          }
        }
      }
      /* at this point all the cells in my partition are marked */

      /* set up j-major iteration */
      if ( domain_info[d].js[p] != NULL ) RSL_FREE( domain_info[d].js[p] ) ;
      if ( domain_info[d].is[p] != NULL ) RSL_FREE( domain_info[d].is[p] ) ;
      if ( domain_info[d].ie[p] != NULL ) RSL_FREE( domain_info[d].ie[p] ) ;
      if ( domain_info[d].jg2n[p] != NULL ) RSL_FREE( domain_info[d].jg2n[p] ) ;
      domain_info[d].js[p] = RSL_MALLOC(int,2*nlen+10) ;
      domain_info[d].is[p] = RSL_MALLOC(int,2*nlen+10) ;
      domain_info[d].ie[p] = RSL_MALLOC(int,2*nlen+10) ;
      domain_info[d].jg2n[p] = RSL_MALLOC(int,2*nlen+10) ;
      ilocaloffset = domain_info[d].ilocaloffset ;
      jlocaloffset = domain_info[d].jlocaloffset ;
      for ( nrun = 0 ; nrun < 2*nlen+10 ; nrun++ )
      {
	domain_info[d].is[p][nrun] = 999999 ;
	domain_info[d].ie[p][nrun] = -999999 ;
      }
      nrun = 0 ;
      if ( ! no_points )
      {
      first = 1 ;
      for ( n = 0 ; n < nlen ; n++ )
      {
        for ( m = 0 ; m < mlen ; m++ )
        {
          if ( domain[INDEX_2(n,m,mlen)].trimmed == 0 )
          {
            if (domain[INDEX_2(n,m,mlen)].P == P
              || ( domain[INDEX_2(n,m,mlen)].info_1 == 1 ))
            {
              ig = ID_IDEX(domain[INDEX_2(n,m,mlen)].id) ;
              jg = ID_JDEX(domain[INDEX_2(n,m,mlen)].id) ;
              i = ig - ilocaloffset ;
              j = jg - jlocaloffset ;
              if ( first )
              {
                first = 0 ;
                start_i = i ;
                start_j = j ;
                start_ig = ig ;
                start_jg = jg ;
                runlength = 1 ;
                prev_ig = ig-1 ;
                prev_jg = jg ;
                domain_info[d].idif = i - ig ;
                domain_info[d].jdif = j - jg ;
              }
              else
              {
                runlength++ ;
              }
              if ( ! ( ig == prev_ig+1 && jg == prev_jg ) )
              {
                domain_info[d].js[p][nrun] = start_j + 1 ;
                domain_info[d].is[p][nrun] = start_i + 1 ;
                domain_info[d].ie[p][nrun] = start_i + runlength-1 ;
		domain_info[d].jg2n[p][jg] = nrun + 1  ;
                runlength = 1 ;
                start_i = i ;
                start_j = j ;
                start_ig = ig ;
                start_jg = jg ;
                nrun++ ;
              }
              prev_ig = ig ;
              prev_jg = jg ;
            }
          }
        }
      }
      /* handle last one -- note assumption on setting of jg for
	 the index into jg2n field */
      if ( runlength != 0 )
      {
        domain_info[d].js[p][nrun] = start_j + 1 ;
        domain_info[d].is[p][nrun] = start_i + 1 ;
        domain_info[d].ie[p][nrun] = start_i + runlength ;
	domain_info[d].jg2n[p][jg] = nrun + 1  ;
        nrun++ ; 
      }
      }
      domain_info[d].nrun[p] = nrun ;

      /* set up i-major iteration */
      if ( domain_info[d].is2[p] != NULL ) RSL_FREE( domain_info[d].is2[p] ) ;
      if ( domain_info[d].js2[p] != NULL ) RSL_FREE( domain_info[d].js2[p] ) ;
      if ( domain_info[d].je2[p] != NULL ) RSL_FREE( domain_info[d].je2[p] ) ;
      if ( domain_info[d].ig2n[p] != NULL ) RSL_FREE( domain_info[d].ig2n[p] ) ;
      domain_info[d].is2[p] = RSL_MALLOC(int,2*nlen+10) ;
      domain_info[d].js2[p] = RSL_MALLOC(int,2*nlen+10) ;
      domain_info[d].je2[p] = RSL_MALLOC(int,2*nlen+10) ;
      domain_info[d].ig2n[p] = RSL_MALLOC(int,2*nlen+10) ;
      ilocaloffset = domain_info[d].ilocaloffset ;
      jlocaloffset = domain_info[d].jlocaloffset ;
      for ( nrun = 0 ; nrun < 2*nlen+10 ; nrun++ )
      {
	domain_info[d].js2[p][nrun] = 999999 ;
	domain_info[d].je2[p][nrun] = -999999 ;
      }
      nrun = 0 ;
      if ( ! no_points )
      {
      first = 1 ;
      for ( m = 0 ; m < mlen ; m++ )
      {
        for ( n = 0 ; n < nlen ; n++ )
        {
          if ( domain[INDEX_2(n,m,mlen)].trimmed == 0 )
          {
            if (domain[INDEX_2(n,m,mlen)].P == P
              || ( domain[INDEX_2(n,m,mlen)].info_1 == 1 ))
            {
              ig = ID_IDEX(domain[INDEX_2(n,m,mlen)].id) ;
              jg = ID_JDEX(domain[INDEX_2(n,m,mlen)].id) ;
              i = ig - ilocaloffset ;
              j = jg - jlocaloffset ;
              if ( first )
              {
                first = 0 ;
                start_i = i ;
                start_j = j ;
                start_ig = ig ;
                start_jg = jg ;
                runlength = 1 ;
                prev_jg = jg-1 ;
                prev_ig = ig ;
              }
              else
              {
                runlength++ ;
              }
              if ( ! ( jg == prev_jg+1 && ig == prev_ig ) )
              {
                domain_info[d].is2[p][nrun] = start_i + 1 ;
                domain_info[d].js2[p][nrun] = start_j + 1 ;
                domain_info[d].je2[p][nrun] = start_j + runlength-1 ;
		domain_info[d].ig2n[p][jg] = nrun + 1  ;
                runlength = 1 ;
                start_i = i ;
                start_j = j ;
                start_ig = ig ;
                start_jg = jg ;
                nrun++ ;
              }
              prev_ig = ig ;
              prev_jg = jg ;
            }
          }
        }
      }
      /* handle last one -- note assumption on setting of jg for
	 the index into jg2n field */
      if ( runlength != 0 )
      {
        domain_info[d].is2[p][nrun] = start_i + 1 ;
        domain_info[d].js2[p][nrun] = start_j + 1 ;
        domain_info[d].je2[p][nrun] = start_j + runlength ;
	domain_info[d].ig2n[p][jg] = nrun + 1  ;
        nrun++ ; 
      }
      }
      domain_info[d].nruni[p] = nrun ;
    }
  }
  return(0) ;
}

RSL_ALLOW_DYNPAD ()
{
  sw_allow_dynpad = 1 ;
}
