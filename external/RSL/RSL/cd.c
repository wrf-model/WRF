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
#include <math.h>

#ifdef STANDALONE
  typedef int * int_p ;
# define RSL_INVALID -1
# define RSL_VALID 1
# define INDEX_2(A,B,NB)       ( (B) + (A)*(NB) )
# define INDEX_3(A,B,NB,C,NC)  INDEX_2( (A), INDEX_2( (B), (C), (NC) ), (NB)*(NC))
# define RSL_MALLOC(T,N)  (T *)malloc((sizeof(T))*(N))
# define RSL_MALLOC(T,N)  (T *)malloc((sizeof(T))*(N))
# define RSL_FREE(P)  free(P)
# define BOUNDARY_SAFE boundary_safe_

main()
{
  int i,m,n,py,px ;
  int bwdth ;
  int wk1[4000], wk2[4000] ;
  for ( i = 0 ; i < 4000 ; i++ ) wk1[i] = RSL_VALID ;
  printf("m n py px bwdth\n") ; scanf("%d %d %d %d %d",&m,&n,&py,&px,&bwdth) ;
  rsl_default_decomp( wk1, wk2, NULL, &m,&n,&py,&px ) ;
  if ( bwdth > 0 ) BOUNDARY_SAFE ( wk2, &bwdth, &m, &n, &m, &n ) ;
  print_region( wk2, m,n) ; 
}
#else
#  include "rsl.h"
#endif

/*#define INDEX_2(A,B,NB)       ( (B) + (A)*(NB) ) */

mark_line( wrk, m, n, x1, y1, x2, y2, val )
  int wrk[] ;
  int m, n, x1, y1, x2, y2, val ;
{
  int x, y, i, j, k ;
  int yz, yz2, dex ;

  double SLOPE, X, Y, DX, DY, dY, X1, X2, Y1, Y2 ;

  X1 = x1 ;
  X2 = x2 ;
  DX = X2 - X1 ;
  Y1 = y1 ;
  Y2 = y2 ;
  DY = Y2 - Y1 ;
  if ( DX == 0.0 )
  {
    if ( y2 >= y1 )
      for ( i = y1 ; i <= y2 ; i++ )
        wrk[ INDEX_2( x1, i, m )] = val ;
    else
      for ( i = y2 ; i <= y1 ; i++ )
        wrk[ INDEX_2( x1, i, m )] = val ;
    return ;
  }
  else
  {
    SLOPE = DY/DX ;
    dY = SLOPE * .5 ;
  }

  if ( x2 >= X1 )
  {
    Y = y1 + .5 ;
    for ( X = x1+.5 ; X < x2+.5 ; X = X+.5 )
    {
      j = X + .25 ;
      if ( dY >= 0.0 )
        for ( i = Y ; i <= (k=Y+dY) ; i++ )     /* k business converts to int */
          wrk[ INDEX_2( j, i, m )] = val ;
      else
        for ( i = Y ; i >= (k=Y+dY) ; i-- )
          wrk[ INDEX_2( j, i, m )] = val ;
      Y = Y + dY ;
    }
    wrk[ INDEX_2( x2, y2, m )] = val ;
  }
  else
  {
    Y = y2 + .5 ;
    for ( X = x2+.5 ; X < x1+.5 ; X = X+.5 )
    {
      j = X + .25 ;
      if ( dY >= 0.0 )
        for ( i = Y ; i <= (k=Y+dY) ; i++ )     /* k business converts to int */
          wrk[ INDEX_2( j, i, m )] = val ;
      else
        for ( i = Y ; i >= (k=Y+dY) ; i-- )
          wrk[ INDEX_2( j, i, m )] = val ;
      Y = Y + dY ;
    }
    wrk[ INDEX_2( x1, y1, m )] = val ;
  }
}

fill_region( wrk, m, n, v, v2 )
  int wrk[], m, n, v, v2 ;
{
  int x, y ;
  
  for ( y = 0 ; y < m ; y++ )
  {
    flood( 0, y, v, v2, wrk, m, n ) ;
    flood( n-1, y, v, v2, wrk, m, n ) ;
  }
  for ( x = 0 ; x < n ; x++ )
  {
    flood( x, 0, v, v2, wrk, m, n ) ;
    flood( x, m-1, v, v2, wrk, m, n ) ;
  }
}

flood( x, y, v, v2, wrk, m, n )
  int x, y, v, v2, wrk[], m, n ;
{
  if ( x < 0 || x >= n || y < 0 || y >= m )
    return ;
  if ( wrk[INDEX_2(x,y,m)] == v )
  {
    wrk[INDEX_2(x,y,m)] = v2 ;
    flood( x+1, y  , v, v2, wrk, m, n ) ;
    flood( x-1, y  , v, v2, wrk, m, n ) ;
    flood( x  , y+1, v, v2, wrk, m, n ) ;
    flood( x  , y-1, v, v2, wrk, m, n ) ;
  }
}

decomp_region_2( wrk, m, n, py, px )
  int wrk[], m, n, py, px ;
{
  int *wk ;
  int x, y, ncells, nprocs, n_p, n_py, n_px, i, pid, p ;

  wk = RSL_MALLOC( int, m*n ) ;

  nprocs = px * py ;

  ncells = 0 ;
  for ( x = 0 ; x < m*n ; x++ ) 
  {
    wk[x] = -1 ;
    if ( wrk[x] != -1 ) ncells++ ;
  }

  n_p = ncells / nprocs ;
#if 0
printf("ncells %d\n",ncells) ;
printf("nprocs %d\n",nprocs) ;
printf("n_p    %d\n",n_p) ;
#endif

  /* divide over py in m dimension first */
  pid = -1 ;
  i = 0 ;
  n_py = ncells / py ;
  for ( y = 0 ; y < m ; y++ )
  {
    for ( x = 0 ; x < n ; x++ )
    {
      if ( wrk[INDEX_2(x,y,m)] != RSL_INVALID )
      {
        if ( i % n_py == 0 ) pid++ ;
        i++ ;
        if ( pid > py-1 ) pid = py-1 ;
        wk[INDEX_2(x,y,m)] = pid ;
      }
    }
  }

  /* now divide over px in n dimension */
  n_px = n_py / px ;
  for ( p = 0 ; p < py ; p++ )
  {
    pid = -1 ;
    i = 0 ;
    for ( x = 0 ; x < n ; x++ )
    {
      for ( y = 0 ; y < m ; y++ )
      {
        if ( wk[INDEX_2(x,y,m)] == p )
        {
          if ( i % n_px == 0 ) pid++ ;
          i++ ;
          if ( pid > px-1 ) pid = px-1 ;
          wk[INDEX_2(x,y,m)] = pid*10000 + p ;
        }
      }
    }
  }
  
  for ( x = 0 ; x < n ; x++ )
    for ( y = 0 ; y < m ; y++ )
  {
    if (( p = wk[ INDEX_2( x, y, m ) ] ) != RSL_INVALID )
    {
      n_py = p % 10000 ;
      n_px = p / 10000 ;
      wrk[INDEX_2( x, y, m )] = n_py*px + n_px ;
    }
    else
    {
      wrk[INDEX_2( x, y, m )] = wk[INDEX_2( x, y, m )] ;
    }
  }

  RSL_FREE(wk) ;
}

print_region( wrk, m, n )
  int wrk[], m, n ;
{
  int i, j ;
  for ( i = m-1 ; i >= 0 ; i-- )
  {
    for ( j = 0 ; j < n ; j++ )
    {
      if ( wrk[ INDEX_2( j, i, m ) ] == RSL_INVALID )
        printf("  ." ) ;
      else 
	printf("%3d", wrk[ INDEX_2( j, i, m ) ] ) ;
    }
    printf("\n") ;
  }
}

#ifndef vpp
rsl_default_decomp( w1, w2, info_p, m_p, n_p, py_p, px_p )
  int_p w1, w2, info_p, m_p, n_p, py_p, px_p ;
{
  int i ;
  int bwdth ;

  bwdth = HARD_CODED_BOUNDARY_WIDTH_FIX_ME_PLEASE ;

  for ( i = 0 ; i < *n_p * *m_p ; i++ )
    w2[i] = w1[i] ;
  if ( regular_decomp )
  {
    patchmap( w2, *m_p, *n_p, *py_p, *px_p ) ;
  }
  else
  {
    decomp_region_2( w2, *m_p, *n_p, *py_p, *px_p ) ;
  }
  return(0) ;
}
#else
rsl_default_decomp( w1, w2, info_p, m_p, n_p, py_p, px_p )
  int_p w1, w2, info_p, m_p, n_p, py_p, px_p ;
{
  int i ;

  for ( i = 0 ; i < *n_p * *m_p ; i++ )
    w2[i] = w1[i] ;
  patchmap( w2, *m_p, *n_p, *py_p, *px_p ) ;
  return(0) ;
}
#endif

