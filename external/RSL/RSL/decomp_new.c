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

RSL_FDECOMPOSE (
   d_p,         /* domain descriptor */
   fcn,         /* decomposition function */
   py_p,        /* number of processors in y dimension */
   px_p,        /* number of processors in x dimension */
   state_p,     /* state vector -- can be RSL_INVALID */
   info_p,	/* extra argument for passing down weight info */
   mloc_p,      /* output: minimum m size of local array on this proc */
   nloc_p,      /* output: minimum n size of local array on this proc */
              )
  int_p d_p, state_p, mloc_p, nloc_p, info_p ;
  int (*fcn)() ;  /* decomposition function */
{

  int d, state, mlen, nlen, i, j, m, n, px, py, px1, py1 ;
  rsl_domain_info_t *dinfo ;
  rsl_point_t *domain, *p ;
  rsl_child_info_t *moms_kids ;

  d = *d_p ;
  px = *px_p ;
  py = *py_p ;
  state = *state_p ;

  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS, 
                "rsl_fdecompose: bad domain descriptor" ) ;
  dinfo = &( domain_info[d] ) ;
  domain = dinfo->domain ;
  RSL_TEST_ERR( dinfo->valid != RSL_VALID,
                "rsl_fdecompose: invalid domain descriptor" ) ;
  mlen = dinfo->len_m ;
  nlen = dinfo->len_n ;

  wrk1 = RSL_MALLOC( int, mlen * nlen ) ;
  wrk2 = RSL_MALLOC( int, mlen * nlen ) ;

  /* load up input work array with valid points */
  for ( j = 0 ; j < nlen ; j++ )
    for ( i = 0 ; i < mlen ; i++ )
    {
      if ( domain[ INDEX_2(j,i,mlen) ]->valid )
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
      RSL_TEST_WRN( 1, "RSL_FDECMPOSE: no change in decomposition" ) ;
    else
      RSL_TEST_WRN( 1, "RSL_FDECMPOSE: method failure -- not decomposed" ) ;
    return(1) ;
  }

  destroy_decomposition( d_p ) ;                /* no backing out now */

  /* set the processor numbers of the valid points */
  for ( j = 0 ; j < nlen ; j++ )
  {
    for ( i = 0 ; i < mlen ; i++ )
    {
      p = domain[ INDEX_2(j,i,mlen) ] ;
      if ( p->valid )
      {
	RSL_TEST_ERR( wrk2[INDEX_2(j,i,mlen)] == RSL_INVALID,
  "RSL_FDECMPOSE: method failed to assign all valid points to a processor") ;
        p->P = wrk2[INDEX_2(j,i,mlen)] ;

        /* if this is a child of point in the next domain up, let the
	   mother know where the kid is going */
	if (( mom = p->mother_id ) != RSL_INVALID )
	{
	  p->mother_P =
	    domain_info[ID_DOMAIN(mom)]->
	      domain[INDEX_2(ID_JDEX(mom),
			     ID_IDEX(mom),
			     domain_info[ID_DOMAIN(mom)].len_m )].P ;
	  moms_kids =
	    domain_info[ID_DOMAIN(mom)]->
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
      }
    }
  }

  /* note that this call will set mloc_p and nloc_p, which can then
     be used by the calling program to allocate buffers of the
     appropriate size, if desired.  NOTE ALSO, THAT THE EXTRA MEMORY
     FOR THE PADS IS INCLUDED. */
  rsl_new_decomposition( d_p, mloc_p, nloc_p ) ;

  /* TODO code to move state vector stuff around needs to be added */

  RSL_FREE( wrk1 ) ;
  RSL_FREE( wrk2 ) ;
  dinfo->decomposed = 1 ;
  return(0) ;
}
