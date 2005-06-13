#include <stdio.h>
#include "rsl_lite.h"

/* for a given global i,j, return the process id */

TASK_FOR_POINT ( i_p , j_p , ids_p, ide_p , jds_p, jde_p , npx_p , npy_p , Px_p, Py_p, P_p )
  int_p i_p , j_p , P_p , Px_p , Py_p , ids_p, ide_p , jds_p, jde_p , npx_p , npy_p ;
{
  int i , j , ids, ide, jds, jde, npx, npy ;  /* inputs */
  int P, Px, Py ;                             /* output */
  int idim, jdim ;
  int rem, a, b ;
  i = *i_p ;
  j = *j_p ;
  npx = *npx_p ;
  npy = *npy_p ;
  ids = *ids_p ; ide = *ide_p ;
  jds = *jds_p ; jde = *jde_p ;
  idim = ide - ids + 1 ;
  jdim = jde - jds + 1 ;

#if 0
fprintf(stderr,"tfp: i,j,npx,npy,ids,ide,jds,jde,idim,jdim\n%d %d %d %d %d %d %d %d %d %d\n",i,j,npx,npy,ids,ide,jds,jde,idim,jdim) ;
#endif

  i = i >= ids ? i : ids ; i = i <= ide ? i : ide ;
  rem = idim % npx ;
  a = rem * ( (idim / npx) + 1 ) ; 
  if ( i-ids < a ) {
    Px = (i-ids) / ( (idim / npx) + 1 ) ;
  }
  else {
    Px = ( a / ( (idim / npx) + 1 ) ) + (i-a-ids) / ( ( idim - a ) / ( npx - rem ) )     ;
  }

  j = j >= jds ? j : jds ; j = j <= jde ? j : jde ;
  rem = jdim % npy ;
  a = rem * ( (jdim / npy) + 1 ) ; 
  if ( j-jds < a ) {
    Py = (j-jds) / ( (jdim / npy) + 1 ) ;
  }
  else {
    Py = ( a / ( (jdim / npy) + 1 ) ) + (j-a-jds) / ( ( jdim - a ) / ( npy - rem ) )     ;
  }

  *Px_p = Px ;
  *Py_p = Py ;
  *P_p = Px + Py * npx ;
}

#if 0
main()
{
  int shw, i , j , ids, ide, jds, jde, npx, npy ;  /* inputs */
  int Px, Py, P ;                             /* output */
  printf("i, j, ids, ide, jds, jde, npx, npy\n") ;
  scanf("%d %d %d %d %d %d %d %d",&i, &j, &ids,&ide,&jds,&jde,&npx,&npy ) ;
#if 0
  for ( j = jds-shw ; j <= jde+shw ; j++ )
  {
  printf("%3d. ",j) ;
  for ( i = ids-shw ; i <= ide+shw ; i++ )
  {
#endif
  TASK_FOR_POINT ( &i , &j ,
                   &ids, &ide, &jds, &jde , &npx , &npy ,
                   &Px, &Py, &P ) ;
  printf("%3d %3d %3d\n",Px, Py, P) ;
#if 0
  }
  printf("\n") ;
  }
#endif
}
#endif
