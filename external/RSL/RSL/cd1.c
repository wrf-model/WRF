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


#define INDEX_2(A,B,NB)       ( (B) + (A)*(NB) )

mark_line( wrk, m, n, x1, y1, x2, y2 )
  int wrk[] ;
  int m, n, x1, y1, x2, y2 ;
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
        wrk[ INDEX_2( x1, i, m )] = 1 ;
    else
      for ( i = y2 ; i <= y1 ; i++ )
        wrk[ INDEX_2( x1, i, m )] = 1 ;
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
          wrk[ INDEX_2( j, i, m )] = 1 ;
      else
        for ( i = Y ; i >= (k=Y+dY) ; i-- )
          wrk[ INDEX_2( j, i, m )] = 1 ;
      Y = Y + dY ;
    }
    wrk[ INDEX_2( x2, y2, m )] = 1 ;
  }
  else
  {
    Y = y2 + .5 ;
    for ( X = x2+.5 ; X < x1+.5 ; X = X+.5 )
    {
      j = X + .25 ;
      if ( dY >= 0.0 )
        for ( i = Y ; i <= (k=Y+dY) ; i++ )     /* k business converts to int */
          wrk[ INDEX_2( j, i, m )] = 1 ;
      else
        for ( i = Y ; i >= (k=Y+dY) ; i-- )
          wrk[ INDEX_2( j, i, m )] = 1 ;
      Y = Y + dY ;
    }
    wrk[ INDEX_2( x1, y1, m )] = 1 ;
  }
}

fill_region( wrk, m, n )
  int wrk[], m, n ;
{
  int x, y ;
  
  for ( y = 0 ; y < m ; y++ )
  {
    flood( 0, y, 0, 2, wrk, m, n ) ;
    flood( n-1, y, 0, 2, wrk, m, n ) ;
  }
  for ( x = 0 ; x < n ; x++ )
  {
    flood( x, 0, 0, 2, wrk, m, n ) ;
    flood( x, m-1, 0, 2, wrk, m, n ) ;
  }

  for ( y = 0 ; y < n ; y++ )
  {
    for ( x = 0 ; x < m ; x++ )
    {
      if      ( wrk[ INDEX_2(x,y,m) ] == 0 )
        wrk[ INDEX_2(x,y,m) ] = 1 ;
      else if ( wrk[ INDEX_2(x,y,m) ] == 2 )
        wrk[ INDEX_2(x,y,m) ] = 0 ;
    }
  }
}

decomp_region_1( wrk, m, n, py, px )
  int wrk[], m, n, py, px ;
{
  int *wk ;
  int x, y, ncells, nprocs, n_p, n_py, n_px, i, pid, p ;

  wk = (int*)malloc(m*n*sizeof(int)) ;

  nprocs = px * py ;

  for ( x = 0 ; x < m*n ; x++ ) 
  {
    wk[x] = -1 ;
    if ( wrk[x] != 0 ) ncells++ ;
  }

  n_p = ncells / nprocs ;

  /* divide over py in m dimension first */
  pid = 0 ;
  i = 0 ;
  n_py = ncells / py ;
  for ( y = 0 ; y < m ; y++ )
  {
    for ( x = 0 ; x < n ; x++ )
    {
      if ( wrk[INDEX_2(x,y,m)] != 0 )   /* only do cells in partition */
      {
        i++ ;
        if ( i % n_py == 0 ) pid++ ;
        wk[INDEX_2(x,y,m)] = pid ;
      }
    }
  }

  /* now divide over px in n dimension */
  n_px = n_py / px ;
  for ( p = 0 ; p < py ; p++ )
  {
    pid = 0 ;
    i = 0 ;
    for ( x = 0 ; x < n ; x++ )
    {
      for ( y = 0 ; y < m ; y++ )
      {
        if ( wk[INDEX_2(x,y,m)] == p )
        {
          i++ ;
          if ( i % n_px == 0 ) pid++ ;
          if ( pid > px-1 ) pid = px-1 ;
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

  free(wk) ;
}

decomp_region_2( wrk, m, n, py, px )
  int wrk[], m, n, py, px ;
{
  int *wk ;
  int x, y, ncells, nprocs, n_p, n_py, n_px, i, pid, p ;


  printf("decomp_region_2( wrk, %d, %d, %d, %d )\n",m, n, py, px ) ;
  wk = (int*)malloc(m*n*sizeof(int)) ;

  nprocs = px * py ;

  for ( x = 0 ; x < m*n ; x++ ) 
  {
    wk[x] = -1 ;
    if ( wrk[x] != 0 ) ncells++ ;
  }

  n_p = ncells / nprocs ;

  dc2( 0, nprocs, wk, wrk, m, n, py, px, n_p ) ;
  
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

  free(wk) ;
}

dc2( p, nprocs, wk, wrk, m, n, py, px, n_p )
  int p, nprocs, wk[], wrk[], m, n, py, px, n_p ;
{
  int x, y, i, v, flg, reach, oldi ;

  if ( p >= nprocs ) return ;

  printf("dc2(%d, %d, wk, wk, %d, %d, %d, %d, %d)\n",p, nprocs,m, n, py, px, n_p );


  for ( x = 0 ; x < n ; x++ )
  {
    flg = 1 ;
    for ( y = 0 ; y < m && flg ; y++ )
    {
      if ( wrk[INDEX_2(x,y,m)] != 0 )
      {
        if ( wk[INDEX_2(x,y,m)] == -1 ) goto breakout ;
        if ( wk[INDEX_2(x,y,m)] != -1 ) flg = 0 ; 
      }
    }
  }

  for ( y = 0 ; y < m ; y++ )
  {
    flg = 1 ;
    for ( x = n-1 ; x >= 0 && flg ; x-- )
    {
      if ( wrk[INDEX_2(x,y,m)] != 0 )
      {
        if ( wk[INDEX_2(x,y,m)] == -1 ) goto breakout ;
        if ( wk[INDEX_2(x,y,m)] != -1 ) flg = 0 ; 
      }
    }
  }

  for ( x = n-1 ; x >=0 ; x-- )
  {
    flg = 1 ;
    for ( y = m-1 ; y >=0 && flg ; y-- )
    {
      if ( wrk[INDEX_2(x,y,m)] != 0 )
      {
        if ( wk[INDEX_2(x,y,m)] == -1 ) goto breakout ;
        if ( wk[INDEX_2(x,y,m)] != -1 ) flg = 0 ; 
      }
    }
  }

  for ( y = m-1 ; y >= 0 ; y-- )
  {
    flg = 1 ;
    for ( x = 0 ; x < n && flg ; x++ )
    {
      if ( wrk[INDEX_2(x,y,m)] != 0 )
      {
        if ( wk[INDEX_2(x,y,m)] == -1 ) goto breakout ;
        if ( wk[INDEX_2(x,y,m)] != -1 ) flg = 0 ; 
      }
    }
  }

  for ( x = 0 ; x < n ; x++ )
  {
    for ( y = 0 ; y < m ; y++ )
    {
      v = wk[INDEX_2(x,y,m)] ;
      if ( v == -1 && wrk[INDEX_2(x,y,m)] != 0 )
      {
        goto breakout ;
      }
    }
  }

breakout:
  if ( x == n ) return ;                /* done, none found */

  printf("dc2: p %d v %d x %d y %d  wrk %d\n", p,v,x,y,wrk[INDEX_2(x,y,m)]) ;
  
  reach = 0 ;
  i = 0 ;
  /* start acreting outward until we're stopped dead or we get enough */
  while ( (i < n_p) )
  {
    reach++ ;
    oldi = i ;
    acrete( wk, wrk, p, &i, n_p, reach, x, y, m, n ) ;
    if ( i == oldi ) break ;
  }

  printf("\n") ;
  print_region( wk, m, n ) ;

  dc2( p+1, nprocs, wk, wrk, m, n, py, px, n_p ) ;
}

#define T(X,Y) \
   {                                                                    \
     if( (X) >= 0 && (X) < n && (Y) >= 0 && (Y) < m && *i < n_p )       \
       if (wk[INDEX_2((X),(Y),m)]==-1&&wrk[INDEX_2((X),(Y),m)]!=0)      \
       {                                                                \
	   setone = 1 ;							\
           wk[INDEX_2((X),(Y),m)] = p ;                                 \
           *i = *i + 1 ;                                                \
       }                                                                \
   }

acrete( wk, wrk, p, i, n_p, reach, x, y, m, n )
  int wk[], wrk[], p, *i, n_p ;
{
  int setone ;

  if ( reach == 0 ) return ;
  if ( *i >= n_p ) return ;
  if ( !(x >= 0 && x < n && y >= 0 && y < m) ) return ;
  if ( wrk[INDEX_2(x,y,m)]==0 ) return ;
  if ( wk[INDEX_2(x,y,m)]!=-1 && wk[INDEX_2(x,y,m)]!= p) return ;

  /*
  if ( p == 5 && reach == 1 )
  printf("acrete(wk,wrk, p %d, *i %d, n_p %d, reach %d, x %d, y %d, m %d, n %d)\n",p, *i, n_p, reach, x, y, m, n ) ;
  */

  T( x   , y   ) ;
  T( x-1 , y+1 ) ;
  T( x   , y+1 ) ;
  T( x+1 , y+1 ) ;
  T( x-1 , y   ) ;
  T( x+1 , y   ) ;
  T( x-1 , y-1 ) ;
  T( x   , y-1 ) ;
  T( x+1 , y-1 ) ;

  acrete( wk, wrk, p, i, n_p, reach-1, x-1 , y-1 , m, n ) ;
  acrete( wk, wrk, p, i, n_p, reach-1, x-1 , y   , m, n ) ;
  acrete( wk, wrk, p, i, n_p, reach-1, x-1 , y+1 , m, n ) ;
  acrete( wk, wrk, p, i, n_p, reach-1, x   , y+1 , m, n ) ;
  acrete( wk, wrk, p, i, n_p, reach-1, x+1 , y+1 , m, n ) ;
  acrete( wk, wrk, p, i, n_p, reach-1, x+1 , y   , m, n ) ;
  acrete( wk, wrk, p, i, n_p, reach-1, x+1 , y-1 , m, n ) ;
  acrete( wk, wrk, p, i, n_p, reach-1, x   , y-1 , m, n ) ;

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
    mark_line( wrk, m, n, x1, y1, x2, y2 ) ;
    x1 = x2 ;
    y1 = y2 ;
  }

  /* print_region( wrk, m, n ) ; */

  fill_region( wrk, m, n ) ;

  /* print_region( wrk, m, n ) ; */

  switch ( opt )
  {
    case 1 : decomp_region_1( wrk, m, n, py, px ) ;
	     print_region( wrk, m, n ) ;
	     break ;
    case 2 : decomp_region_2( wrk, m, n, py, px ) ;
	     break ; 
    default : break ;
  }

  printf("\n") ;


}


