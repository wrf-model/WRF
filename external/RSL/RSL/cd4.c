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
#include "rsl.h"


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

/* decompose over a 1-d vector of processors in y */
decomp_region_1y( wrk, m, n, py )
  int wrk[], m, n, py ;
{
  int *wk ;
  int nprocs, i, x, y, ncells, pid, n_py ;

  wk = RSL_MALLOC( int, m*n ) ;
  for ( x = 0 ; x < m*n ; x++ ) 
  {
    wk[x] = -1 ;
    if ( wrk[x] != 0 ) ncells++ ;
  }
  /* divide over py in m dimension first */
  pid = -1 ;
  i = 0 ;
  n_py = ncells / py ;
  for ( y = 0 ; y < m ; y++ )
  {
    for ( x = 0 ; x < n ; x++ )
    {
      if ( wrk[INDEX_2(x,y,m)] != 0 )   /* only do cells in partition */
      {
        if ( i % n_py == 0 ) pid++ ;
        i++ ;
        if ( pid > py-1 ) pid = py-1 ;
        wk[INDEX_2(x,y,m)] = pid ;
      }
    }
  }

  for ( y = 0 ; y < m ; y++ )
  {
    for ( x = 0 ; x < n ; x++ )
    {
      wrk[INDEX_2( x, y, m )] = wk[INDEX_2( x, y, m )] ;
    }
  }

  RSL_FREE(wk) ;
}

/* decompose over a 1-d vector of processors in x */
decomp_region_1x( wrk, m, n, px )
  int wrk[], m, n, px ;
{
  int *wk ;
  int nprocs, i, x, y, ncells, pid, n_px ;

  wk = RSL_MALLOC( int, m*n ) ;
  for ( x = 0 ; x < m*n ; x++ ) 
  {
    wk[x] = -1 ;
    if ( wrk[x] != 0 ) ncells++ ;
  }
  /* divide over px in m dimension first */
  pid = -1 ;
  i = 0 ;
  n_px = ncells / px ;
  for ( x = 0 ; x < n ; x++ )
  {
    for ( y = 0 ; y < m ; y++ )
    {
      if ( wrk[INDEX_2(x,y,m)] != 0 )   /* only do cells in partition */
      {
        if ( i % n_px == 0 ) pid++ ;
        i++ ;
        if ( pid > px-1 ) pid = px-1 ;
        wk[INDEX_2(x,y,m)] = pid ;
      }
    }
  }

  for ( y = 0 ; y < m ; y++ )
  {
    for ( x = 0 ; x < n ; x++ )
    {
      wrk[INDEX_2( x, y, m )] = wk[INDEX_2( x, y, m )] ;
    }
  }

  RSL_FREE(wk) ;
}

decomp_region_2( wrk, m, n, py, px )
  int wrk[], m, n, py, px ;
{
  int *wk ;
  int x, y, ncells, nprocs, n_p, n_py, n_px, i, pid, p ;

  wk = RSL_MALLOC( int, m*n ) ;

  nprocs = px * py ;

  for ( x = 0 ; x < m*n ; x++ ) 
  {
    wk[x] = -1 ;
    if ( wrk[x] != 0 ) ncells++ ;
  }

  n_p = ncells / nprocs ;

  /* divide over py in m dimension first */
  pid = -1 ;
  i = 0 ;
  n_py = ncells / py ;
  for ( y = 0 ; y < m ; y++ )
  {
    for ( x = 0 ; x < n ; x++ )
    {
      if ( wrk[INDEX_2(x,y,m)] != 0 )   /* only do cells in partition */
      {
        if ( i % n_py == 0 ) pid++ ;
        i++ ;
        if ( pid > px-1 ) pid = px-1 ;
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
          if ( pid > py-1 ) pid = py-1 ;
          wk[INDEX_2(x,y,m)] = pid*10000 + p ;
        }
      }
    }
  }
  
  for ( x = 0 ; x < n ; x++ )
    for ( y = 0 ; y < m ; y++ )
  {
    if (( p = wk[ INDEX_2( x, y, m ) ] ) != -1 )
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

print_region( wrk, m, n )
  int wrk[], m, n ;
{
  int i, j ;
  for ( i = m-1 ; i >= 0 ; i-- )
  {
    for ( j = 0 ; j < n ; j++ )
    {
      if ( wrk[ INDEX_2( j, i, m ) ] == -1 )
        printf("  ." ) ;
      else 
	printf("%3d", wrk[ INDEX_2( j, i, m ) ] ) ;
    }
    printf("\n") ;
  }
}

/****************/

main()
{
  /*      n   m */
  int wrk[100 * 100] ;
  int i, j, m, n, x1, y1, x2, y2, py, px, opt ;

  for ( i = 0 ; i < 100*100 ; i++ ) wrk[i] = 0 ;

  scanf("%d %d", &m, &n) ;
  scanf("%d %d %d", &py, &px, &opt) ;
  scanf("%d %d", &x1, &y1) ;
  while ( scanf("%d %d", &x2, &y2) != EOF )
  {
    mark_line( wrk, m, n, x1, y1, x2, y2, 1 ) ;
    x1 = x2 ;
    y1 = y2 ;
  }

  print_region( wrk, m, n ) ;

  fill_region( wrk, m, n, 0, 2 ) ;
  for ( j = 0 ; j < n ; j++ )
    for ( i = 0 ; i < m ; i++ )
    {
      if ( wrk[ INDEX_2( j, i, m ) ] == 2 )
        wrk[ INDEX_2( j, i, m ) ] = 0 ;
      else
        wrk[ INDEX_2( j, i, m ) ] = 1 ;
    }

  print_region( wrk, m, n ) ;

  switch ( opt )
  {
    case 0 : decomp_region_1x( wrk, m, n, py*px ) ;
	     print_region( wrk, m, n ) ;
	     break ;
    case 1 : decomp_region_1y( wrk, m, n, py*px ) ;
	     print_region( wrk, m, n ) ;
	     break ;
    case 2 : decomp_region_2( wrk, m, n, py, px ) ;
	     print_region( wrk, m, n ) ;
	     break ; 
    default : break ;
  }

  printf("\n") ;


}


