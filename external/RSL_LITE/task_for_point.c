#include <stdio.h>
#include "rsl_lite.h"

/*

      rem = mod(idim,ntasks_x)
      a = ( idim / ntasks_x + 1 ) * min( rem, mytask_x )
      b = ( idim / ntasks_x     ) * max( 0, mytask_x - rem )
      ips = max( 1, a + b + 1 )
      a = ( idim / ntasks_x + 1 ) * min( rem, mytask_x+1 )
      b = ( idim / ntasks_x     ) * max( 0, mytask_x+1 - rem )
      ipe = a + b

      rem = mod(jdim,ntasks_y)
      a = ( jdim / ntasks_y + 1 ) * min( rem, mytask_y )
      b = ( jdim / ntasks_y     ) * max( 0, mytask_y - rem )
      jps = max( 1, a + b + 1 )
      a = ( jdim / ntasks_y + 1 ) * min( rem, mytask_y+1 )
      b = ( jdim / ntasks_y     ) * max( 0, mytask_y+1 - rem )
      jpe = a + b

*/

/* for a given global i,j, return the process id */

TASK_FOR_POINT ( i_p , j_p , idim_p , jdim_p , npx_p , npy_p , Px_p, Py_p, P_p )
  int_p i_p , j_p , P_p , Px_p , Py_p , idim_p , jdim_p , npx_p , npy_p ;
{
  int i , j , idim, jdim, npx, npy ;  /* inputs */
  int P, Px, Py ;                     /* output */
  int rem, a, b ;
  i = *i_p ;
  j = *j_p ;
  npx = *npx_p ;
  npy = *npy_p ;
  idim = *idim_p ;
  jdim = *jdim_p ;

  rem = idim % npx ;
  a = rem * ( (idim / npx) + 1 ) ; 
  if ( i-1 < a ) {
    Px = (i-1) / ( (idim / npx) + 1 ) ;
  }
  else {
    Px = ( a / ( (idim / npx) + 1 ) ) + (i-a-1) / ( ( idim - a ) / ( npx - rem ) )     ;
  }

  rem = jdim % npy ;
  a = rem * ( (jdim / npy) + 1 ) ; 
  if ( j-1 < a ) {
    Py = (j-1) / ( (jdim / npy) + 1 ) ;
  }
  else {
    Py = ( a / ( (jdim / npy) + 1 ) ) + (j-a-1) / ( ( jdim - a ) / ( npy - rem ) )     ;
  }

  *Px_p = Px ;
  *Py_p = Py ;
  *P_p = Px + Py * npx ;
}

#if 0
main()
{
  int i , j , ids, ide, jds, jde, npx, npy ;  /* inputs */
  int Px, Py, P ;                             /* output */
  scanf("%d %d %d %d",&ide,&jde,&npx,&npy ) ;
  for ( j = 1 ; j <= jde ; j++ ) {
  printf("%3d. ",j) ;
  for ( i = 1 ; i <= ide ; i++ ) {
  ids = 1 ;
  TASK_FOR_POINT ( &i , &j ,
                   &ide, &jde , &npx , &npy ,
                   &Px, &Py, &P ) ;
  printf("%3d",P) ;
  }
  printf("\n") ;
  }
}
#endif
