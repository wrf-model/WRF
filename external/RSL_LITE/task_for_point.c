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
  i = *i_p - 1 ;
  j = *j_p - 1 ;
  npx = *npx_p ;
  npy = *npy_p ;
  ids = *ids_p - 1 ; ide = *ide_p - 1 ;
  jds = *jds_p - 1 ; jde = *jde_p - 1 ;
  idim = ide - ids + 1 ;
  jdim = jde - jds + 1 ;

#if 0
fprintf(stderr,"tfp: i,j,npx,npy,ids,ide,jds,jde,idim,jdim\n%d %d %d %d %d %d %d %d %d %d\n",i,j,npx,npy,ids,ide,jds,jde,idim,jdim) ;
#endif

  i = i >= ids ? i : ids ; i = i <= ide ? i : ide ;
  rem = idim % npx ;
  a = rem * ( (idim / npx) + 1 ) ; 
#if 0
fprintf(stderr,"idim = %d rem = %d, a %d, i %d , i-ids %d\n", idim , rem , a, i, i-ids) ;
#endif
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
  int ips[100], ipe[100] ;
  int jps[100], jpe[100] ;
  int shw, i , j , ids, ide, jds, jde, npx, npy ;  /* inputs */
  int Px, Py, P ;                             /* output */
  printf("i, j, ids, ide, jds, jde, npx, npy\n") ;
  scanf("%d %d %d %d %d %d %d %d",&i, &j, &ids,&ide,&jds,&jde,&npx,&npy ) ;
  shw =0 ;
  for ( i = 0 ; i < 100 ; i++ ) { ips[i] = 9999999 ; ipe[i] = -99999999 ; }
  for ( i = 0 ; i < 100 ; i++ ) { jps[i] = 9999999 ; jpe[i] = -99999999 ; }
#if 1
  for ( j = jds-shw ; j <= jde+shw ; j++ )
  {
  for ( i = ids-shw ; i <= ide+shw ; i++ )
  {
#endif
  TASK_FOR_POINT ( &i , &j ,
                   &ids, &ide, &jds, &jde , &npx , &npy ,
                   &Px, &Py, &P ) ;
  if ( i < ips[P] ) ips[P] = i ;
  if ( j < jps[P] ) jps[P] = j ;
  if ( i > ipe[P] ) ipe[P] = i ;
  if ( j > jpe[P] ) jpe[P] = j ;
/*  printf("%3d",P) ; */
#if 1
  }
/*  printf("\n") ; */
  }
for ( i = 0 ; i < 16 ; i++ ) {
  fprintf(stderr,"%3d. ips %d ipe %d jps %d jpe %d\n", i, ips[i], ipe[i], jps[i], jpe[i] ) ;
}
#endif
}
#endif
