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
/*
   Given a work array with the in-valid entries marked with invalid,
   Traverse the boundaries and figure the distances using the snake
   and breadcrumb method: start at an outer point, leave a breadcrumb
   and snake around either clockwise or counterclockwise (value of rot)
   feeling the left or right edge respectively, until you get back to the
   breadcrumb, then jump in and start the next boundary.  Each valid
   cell is marked with an integer signifying its distance from the boundary
   (1 is the outermost valid row or column).

   The points are also marked with the code for which boundary they are
   closest to.  How corners are marked will depend on which direction
   the rotation occured.  Therefore, one may wish to compute two
   traversals, and then difference them.  Any points that come up
   different can be resolved by the differencing algorithm with an
   east-west wins, north-south wins, or diagonal-separate policy.

   the array bdy countains the invalid markings
   the array dbdy, may be uninitialized
   m is assumed to be minor and the north-south dimension.
   n is assumed to be major and the east-west dimension.
   the integer boundary tags are passed in.

   returns zero for success, non-zero for failure 

*/

#include "rsl.h"

#ifndef INDEX_2
# define INDEX_2(A,B,NB)       ( (B) + (A)*(NB) )
#endif
#ifndef INDEX_3
# define INDEX_3(A,B,NB,C,NC)  INDEX_2((A),INDEX_2((B),(C),(NC)),(NB)*(NC))
#endif
#ifndef RSL_MALLOC
# define RSL_MALLOC(T,N)  (T *)rsl_malloc(__FILE__,__LINE__,(sizeof(T))*(N))
#endif
#ifndef RSL_TEST_ERR
# define RSL_TEST_ERR(T,M) {if(T){fprintf(stderr,"error (\"%s\":%d) %s\n",__FILE__,__LINE__,M);RSL_FATAL(5);}}
#endif
#ifndef RSL_FATAL
# define RSL_FATAL(X) exit(X)
#endif

static int bc_i, bc_j, dir ;    /* breadcrumb indices */
static int in ;


/* fortran wrapper */
#ifdef NOUNDERSCORE
fill_bdy( bdy, dbdy, mmax, nmax, rot, invalid, ntag, stag, etag, wtag )
#else
# ifdef F2CSTYLE
fill_bdy__( bdy, dbdy, mmax, nmax, rot, invalid, ntag, stag, etag, wtag )
# else
fill_bdy_( bdy, dbdy, mmax, nmax, rot, invalid, ntag, stag, etag, wtag )
# endif
#endif
  int * bdy ;     /* boundary associations, on entry contains invalid entries */
  int * dbdy ;    /* distances to boundaries, may be unitialized */
  int * mmax,     /* minor n/s dimension of arrays */
      * nmax ;    /* major e/w dimension of arrays */
  int * rot ;     /* rotation 1=counterclockwise, 0=clockwise */
  int * invalid ; /* integer value of an invalid point */
  int * ntag ;    /* tag for north boundary assn */
  int * stag ;    /* tag for south boundary assn */
  int * etag ;    /* tag for east  boundary assn */
  int * wtag ;    /* tag for west  boundary assn */
{
 fill_boundary( bdy, dbdy, *mmax, *nmax, *rot, *invalid,
                *ntag, *stag, *etag, *wtag ) ;
}

fill_boundary( bdy, dbdy, mmax, nmax, rot, invalid, ntag, stag, etag, wtag )
  int * bdy ;   /* boundary associations, on entry contains invalid entries */
  int * dbdy ;  /* distances to boundaries, may be unitialized */
  int mmax,     /* minor n/s dimension of arrays */
      nmax ;    /* major e/w dimension of arrays */
  int rot ;     /* rotation 1=counterclockwise, 0=clockwise */
  int invalid ; /* integer value of an invalid point */
  int ntag ;    /* tag for north boundary assn */
  int stag ;    /* tag for south boundary assn */
  int etag ;    /* tag for east  boundary assn */
  int wtag ;    /* tag for west  boundary assn */
{
  int i, j, retval ;

  if ( bdy == NULL && dbdy == NULL ) return(1) ;

  retval = 0 ; 

  /* initialized distances array -- nextcell relies on this */
  for ( i = 0 ; i < mmax*nmax ; i++ ) dbdy[i] = -1 ;

  in = 0 ;
  while ( ! place_to_start( bdy, dbdy, mmax, nmax, rot,
                            &i, &j, 
                            invalid, ntag, stag, etag, wtag,
                            &dir ) )
  {
    bc_i = i ;
    bc_j = j ;
    /* recursive */
    in++ ;
    retval = fill_boundary_1( bdy, dbdy, mmax, nmax, rot, i, j,
                              invalid, ntag, stag, etag, wtag ) ;
  }
  return( retval ) ;
}

fill_boundary_1( bdy, dbdy, mmax, nmax, rot, i, j,
                 invalid, ntag, stag, etag, wtag )
  int * bdy ;   /* boundary associations, on entry contains invalid entries */
  int * dbdy ;  /* distances to boundaries, initialized to -1 */
  int mmax,     /* minor n/s dimension of arrays */
      nmax ;    /* major e/w dimension of arrays */
  int rot ;     /* rotation 1=counterclockwise, 0=clockwise */
  int i, j ;    /* current point */
  int invalid ; /* integer value of an invalid point */
  int ntag ;    /* tag for north boundary assn */
  int stag ;    /* tag for south boundary assn */
  int etag ;    /* tag for east  boundary assn */
  int wtag ;    /* tag for west  boundary assn */

{
  int idx ;     /* index of point */
  int fbc ;     /* found bread crumb */
  int i2, j2 ;
  int retval ;

  idx = INDEX_2(j,i,mmax) ;

  dbdy[ idx ] = in ;
  bdy[ idx ] = bdy_from_dir( dir, rot, ntag, stag, etag, wtag ) ;


#if 0
if ( in >= 15 ) {
  int i, j, idx ;
  char x ;
  printf("------ in %d ------ bc: %d %d\n", in, bc_i, bc_j ) ;
  for ( i = mmax-1 ; i >= 0 ; i-- )
  {
    for ( j = 0 ; j < nmax ; j++ )
    {
      idx = INDEX_2(j,i,mmax) ;
      if ( dbdy[idx]==-1 ) x = '.' ;
      else x = (dbdy[idx] % 10) + '0' ;
      printf("%1c",x ) ;
    }
    printf("\n") ;
  }
/*  sleep(1) ; */
/*  clear() ; */
}
#endif

  while ( nextcell( bdy, dbdy, mmax, nmax, rot, i, j, in, &i2, &j2, &fbc,
                    invalid, ntag, stag, etag, wtag ) )
  {
    if ( fbc )
    {
      bc_i = i2 ;
      bc_j = j2 ;
      /* recurse */
      in++ ;
      retval = fill_boundary_1( bdy, dbdy, mmax, nmax, rot, i2, j2,
                                invalid, ntag, stag, etag, wtag ) ;
      break ;
    }
    else
    {
      idx = INDEX_2(j2,i2,mmax) ;
      dbdy[ idx ] = in ;
      bdy[ idx ] = bdy_from_dir( dir, rot, ntag, stag, etag, wtag ) ;
    }
    i = i2 ;
    j = j2 ;
  }
#if 0
if ( in >= 15 ) {
  char x ;
  int i, j,  idx ;
  printf("------ in %d ------ bc: %d %d\n", in, bc_i, bc_j ) ;
  for ( i = mmax-1 ; i >= 0 ; i-- )
  {
    for ( j = 0 ; j < nmax ; j++ )
    {
      idx = INDEX_2(j,i,mmax) ;
      if ( dbdy[idx]==-1 ) x = '.' ;
      else x = (dbdy[idx] % 10) + '0' ;
      printf("%1c",x ) ;
    }
    printf("\n") ;
  }
/*  sleep(1) ; */
/*  clear() ; */
}
#endif
  return(retval) ;
}

nextcell( bdy, dbdy, mmax, nmax, rot, i, j, in, i2, j2, fbc,
          invalid, ntag, stag, etag, wtag )
  int * bdy ;   /* boundary associations, on entry contains invalid entries */
  int * dbdy ;  /* distances to boundaries, initialized to -1 */
  int mmax,     /* minor n/s dimension of arrays */
      nmax ;    /* major e/w dimension of arrays */
  int rot ;     /* rotation 1=counterclockwise, 0=clockwise */
  int i, j ;    /* current point */
  int in ;      /* how far into domain on this instantiation of routine */
  int *i2,*j2 ;	/* new point */
  int *fbc ;    /* found bread crumb */
  int invalid ; /* integer value of an invalid point */
  int ntag ;    /* tag for north boundary assn */
  int stag ;    /* tag for south boundary assn */
  int etag ;    /* tag for east  boundary assn */
  int wtag ;    /* tag for west  boundary assn */
{
  int cnt, try, fail ;
  int ii, jj, idx ;

  *fbc = 0 ;
  for ( cnt = 0 ; cnt < 4 ; cnt++ )
  {
    if ( rot ) 		/* counter clockwise */
    {
      if      ( dir == ntag )
      {
      /* going north; try to go east */
        ii = i  ; jj = j+1 ; try = etag ; fail = wtag ;
      }
      else if ( dir == etag )
      {
      /* going east; try to go south */
        ii = i-1 ; jj = j   ; try = stag ; fail = ntag ;
      }
      else if ( dir == stag )
      {
      /* going south; try to go west */
        ii = i   ; jj = j-1 ; try = wtag ; fail = etag ;
      }
      else if ( dir == wtag )
      {
      /* going west; try to go north */
        ii = i+1 ; jj = j   ; try = ntag ; fail = stag ;
      }
      else
      {
        RSL_TEST_ERR(1,"method failure") ;
      }
    }
    else   		/* clockwise */
    {
      if      ( dir == ntag )
      {
      /* going north; try to go west */
        ii = i   ; jj = j-1 ; try = wtag ; fail = etag ;
      }
      else if ( dir == etag )
      {
      /* going east; try to go north */
        ii = i+1 ; jj = j   ; try = ntag ; fail = stag ;
      }
      else if ( dir == stag )
      {
      /* going south; try to go east */
        ii = i   ; jj = j+1 ; try = etag ; fail = wtag ;
      }
      else if ( dir == wtag )
      {
      /* going west; try to go south */
        ii = i-1 ; jj = j   ; try = stag ; fail = ntag ;
      }
      else
      {
        RSL_TEST_ERR(1,"method failure") ;
      }
    }
    if ( ii >= 0 && ii < mmax && jj >= 0 && jj < nmax )
    {
      if ( ii == bc_i && jj == bc_j )
      {
        *fbc = 1 ;
      }
      idx = INDEX_2(jj,ii,mmax) ;
      dir = try ;
      *i2 = ii ; *j2 = jj ;
      if ( bdy[idx] != invalid &&
          (dbdy[idx] == -1 || (dbdy[idx] == in && ! *fbc )))
                  /* this business (prev line) allows backtracking */
      {
        return(1) ;
      }
    }
    dir = fail ;
  }
  return(0) ;
}

/***********************
  Table for bdy_from_dir
                             boundary associating for rotation
      going             rot=1 (counterclock)    rot=0 (clockwise)
      ---------------------------------------------------
      north             east                    west
      east              south                   north
      south             west                    east
      west              north                   south


************************/
bdy_from_dir( dir, rot, ntag, stag, etag, wtag )
  int dir ;                     /* current direction */
  int rot ;                     /* rotation (see table) */
  int ntag, stag, etag, wtag ;  /* direction tags */
{
  int bdy ;

  if      ( dir == ntag )
    bdy = (rot?etag:wtag) ;  /* if north, then east  bdy */
  else if ( dir == etag )
    bdy = (rot?stag:ntag) ;  /* if east , then south bdy */
  else if ( dir == stag )
    bdy = (rot?wtag:etag) ;  /* if south, then west  bdy */
  else if ( dir == wtag )
    bdy = (rot?ntag:stag) ;  /* if west , then north bdy */
  else
    RSL_TEST_ERR(1,"bad tag") ;

  return( bdy ) ;
}

/* returns 0 on success (found a place to start, otherwise non-zero */
place_to_start( bdy, dbdy, mmax, nmax, rot,
                i2, j2,
                invalid, ntag, stag, etag, wtag,
                dir )
  int * bdy ;   /* boundary associations, on entry contains invalid entries */
  int * dbdy ;  /* distances to boundaries, initialized to -1 */
  int mmax,     /* minor n/s dimension of arrays */
      nmax ;    /* major e/w dimension of arrays */
  int rot ;     /* rotation 1=counterclockwise, 0=clockwise */
  int *i2, *j2 ; /* current point  (output) */
  int invalid ; /* integer value of an invalid point */
  int ntag ;    /* tag for north boundary assn */
  int stag ;    /* tag for south boundary assn */
  int etag ;    /* tag for east  boundary assn */
  int wtag ;    /* tag for west  boundary assn */
  int *dir ;	/* initial direction (output) */
{
  int i, j, idx ;
  int i0, j0, it, jt ;
  int cnt ;

  /* find first valid point */
  for ( j = 0 ; j < nmax ; j++ )
  {
    for ( i = 0 ; i < mmax ; i++ )
    {
      idx = INDEX_2(j,i,mmax) ;
      if ( bdy[ idx ] != invalid && dbdy[idx] == -1 )
      {
	*i2 = i ;
	*j2 = j ;
	goto out1 ;
      }
    }
  }

out1:

  /* set an initial direction -- we will chose the first valid direction
     that does not have a valid direction to its right (rot=1, cntrclkwise)
     or left (rot=1, clkwise) */


  for ( cnt = 0 ; cnt < 4 ; cnt++ ) 
  {
    switch(cnt)
    {
    case 0 :
      /* how about north ? */
      *dir = ntag ;
      i0 = i+1 ; j0 = j ;
      it = i ; jt = (rot?j+1:j-1) ;
      break ;
    case 1 :
      /* how about south ? */
      *dir = stag ;
      i0 = i-1 ; j0 = j ;
      it = i ; jt = (rot?j-1:j+1) ;
      break ;
    case 2 :
      /* how about east ? */
      *dir = etag ;
      i0 = i ; j0 = j+1 ;
      it = (rot?i-1:i+1) ; jt = j ;
      break ;
    case 3 :
      /* how about west ? */
      *dir = wtag ;
      i0 = i ; j0 = j-1 ;
      it = (rot?i+1:i-1) ; jt = j ;
      break ;
    }

    /* there are RETURN statements in these conditionals */
    /* see if i0, j0 is valid, then see if it,jt is invalid */
    if ( i0 >= 0 && i0 < mmax && j0 >= 0 && j0 < nmax )
    {
      idx = INDEX_2(j0,i0,mmax) ;
      if ( bdy[ idx ] != invalid && dbdy[idx] == -1 ) /*and unvisited*/
      {
	/* if this point is invalid or out of bounds, we've got the
	   correct choice for i0, j0 and direction */
        if ( ! ( it >= 0 && it < mmax && jt >= 0 && jt < nmax ) )
	{
	  return(0) ;
	}
	idx = INDEX_2(jt,it,mmax) ;
        if ( bdy[ idx ] == invalid || dbdy[idx] != -1 ) /* or visited */
	{
	  return(0) ;
	}
      }
    }
#if 0
    switch(cnt)
    {
    case 0 :
      /* how about north ? */
      *dir = ntag ;
      i0 = i+1 ; j0 = j ;
      it = i0 ; jt = (rot?j+1:j-1) ;
      break ;
    case 1 :
      /* how about south ? */
      *dir = stag ;
      i0 = i-1 ; j0 = j ;
      it = i0 ; jt = (rot?j-1:j+1) ;
      break ;
    case 2 :
      /* how about east ? */
      *dir = etag ;
      i0 = i ; j0 = j+1 ;
      it = (rot?i-1:i+1) ; jt = j0 ;
      break ;
    case 3 :
      /* how about west ? */
      *dir = wtag ;
      i0 = i ; j0 = j-1 ;
      it = (rot?i+1:i-1) ; jt = j0 ;
      break ;
    }

    /* there are RETURN statements in these conditionals */
    /* see if i0, j0 is valid, then see if it,jt is invalid */
    if ( i0 >= 0 && i0 < mmax && j0 >= 0 && j0 < nmax )
    {
      idx = INDEX_2(j0,i0,mmax) ;
      if ( bdy[ idx ] != invalid && dbdy[idx] == -1 ) /*and unvisited*/
      {
	/* if this point is invalid or out of bounds, we've got the
	   correct choice for i0, j0 and direction */
        if ( ! ( it >= 0 && it < mmax && jt >= 0 && jt < nmax ) )
	{
	  return(0) ;
	}
	idx = INDEX_2(jt,it,mmax) ;
        if ( bdy[ idx ] == invalid || dbdy[idx] != -1 ) /* or visited */
	{
	  return(0) ;
	}
      }
    }
#endif
  }
  return(1) ;
}

char cl[] = "[H[2J" ;

clear()
{
printf(cl) ;
}

