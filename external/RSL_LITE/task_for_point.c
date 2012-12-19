#ifndef MS_SUA
# include <stdio.h>
#endif
#include "rsl_lite.h"

/* updated 20051021, new algorithm distributes the remainder, if any, at either ends of the dimension
   rather than the first remainder number of processors in the dimension. Idea is that the processes
   on the ends have less work because they're boundary processes.  New alg works like this:
                     a                         b
         + + + + + + o o o o o o o o o o o o o + + + + + +

   + represents a process with an extra point (npoints is n/p+1), o processors that don't (n/p)
   a and b are the starting process indices in the dimension of the new section of o or x.
   JM
*/

/* experimental for running some tasks on host and some on MIC 
   if minx = -99 then miny is the number of grid points I want in the Y dimension.
   Otherwise both are set to 1 and it works normally 20121018 JM */

static char tfpmess[1024] ;

TASK_FOR_POINT ( i_p , j_p , ids_p, ide_p , jds_p, jde_p , npx_p , npy_p , Px_p, Py_p , minx_p, miny_p, ierr_p )
  int_p i_p , j_p , Px_p , Py_p , ids_p, ide_p , jds_p, jde_p , npx_p , npy_p, minx_p, miny_p, ierr_p ;
{
  int i , j , ids, ide, jds, jde, npx, npy, minx, miny ;  /* inputs */
  int Px, Py ;                                            /* output */
  int idim, jdim ;
  int rem, a, b ;
  i = *i_p - 1 ;
  j = *j_p - 1 ;
  npx = *npx_p ;
  npy = *npy_p ;
#if 0
  minx = *minx_p ;
  miny = *miny_p ;
#else
  if ( *minx_p == -99 ) {
    minx = 1 ;
    miny = *miny_p ;
    npx = ( *npx_p * *npy_p ) / 2 ;     /* x dim gets half the tasks , only decompose Y by 2 */
    if ( npx * 2 != *npx_p * *npy_p ) {
      *ierr_p = 1 ;
      sprintf(tfpmess,"%d by %d decomp will not work for MIC/HOST splitting. Need even number of tasks\n") ;
    }
  } else {
  minx = 1 ;
  miny = 1 ;
  }
#endif
  ids = *ids_p - 1 ; ide = *ide_p - 1 ;
  jds = *jds_p - 1 ; jde = *jde_p - 1 ;
  idim = ide - ids + 1 ;
  jdim = jde - jds + 1 ;

  *ierr_p = 0 ;

  if ( *minx_p != -99 ) {
  /* begin: jm for Peter Johnsen -- noticed problem with polar filters in gwrf
     if the number of processors exceeds number of vertical levels */
  if ( npx > idim ) { npx = idim ; }
  if ( npy > jdim ) { npy = jdim ; }

  /* begin: wig; 10-Mar-2008
    Check that the number of processors is not so high that the halos begin to overlap.
    If they do, then reduce the number of processors allowed for that dimension.
  */
  tfpmess[0] = '\0' ;
  if ( idim / npx < minx ) {
    npx = idim/minx ;
    if (npx < 1) { npx = 1 ;}
    if (npx != *npx_p) {
      sprintf(tfpmess,"RSL_LITE: TASK_FOR_POINT LIMITING PROCESSOR COUNT IN X-DIRECTION TO %d %d\n", npx,*npx_p) ;
      *ierr_p = 1 ;
    }
  }
  if ( jdim / npy < miny ) {
    npy = jdim/miny ;
    if (npy < 1) { npy = 1 ;}
    if (npy != *npy_p) {
      sprintf(tfpmess,"RSL_LITE: TASK_FOR_POINT LIMITING PROCESSOR COUNT IN Y-DIRECTION TO %d %d\n", npy,*npy_p) ;
      *ierr_p = 1 ;
    }
  }
  /* end: wig */
  }

  i = i >= ids ? i : ids ; i = i <= ide ? i : ide ;
  rem = idim % npx ;
  a = ( rem / 2 ) * ( (idim / npx) + 1 ) ;
  b = a + ( npx - rem ) * ( idim / npx ) ;
  if ( i-ids < a ) {
    Px = (i-ids) / ( (idim / npx) + 1 ) ;
  }
  else if ( i-ids < b ) {
    Px = ( a / ( (idim / npx) + 1 ) ) + (i-a-ids) / ( ( b - a ) / ( npx - rem ) )     ;
  }
  else {
    Px = ( a / ( (idim / npx) + 1 ) ) + (b-a-ids) / ( ( b - a ) / ( npx - rem ) ) +
                                        (i-b-ids) / ( ( idim / npx ) + 1 )  ;
  }

  j = j >= jds ? j : jds ; j = j <= jde ? j : jde ;
  if ( *minx_p != -99 ) {
  rem = jdim % npy ;
  a = ( rem / 2 ) * ( (jdim / npy) + 1 ) ;
  b = a + ( npy - rem ) * ( jdim / npy ) ;
  if ( j-jds < a ) {
    Py = (j-jds) / ( (jdim / npy) + 1 ) ;
  }
  else if ( j-jds < b ) {
    Py = ( a / ( (jdim / npy) + 1 ) ) + (j-a-jds) / ( ( b - a ) / ( npy - rem ) )     ;
  }
  else {
    Py = ( a / ( (jdim / npy) + 1 ) ) + (b-a-jds) / ( ( b - a ) / ( npy - rem ) ) +
                                        (j-b-jds) / ( ( jdim / npy ) + 1 )  ;
  }
  } else {
    Py = 1 ;
    if ( j <= jde-miny ) Py = 0 ;
  }

  *Px_p = Px ;
  *Py_p = Py ;
}

TASK_FOR_POINT_MESSAGE()
{
  fprintf(stderr,"%s\n",tfpmess) ;
}

#if 0
main()
{
  int minx, miny, ierr ;
  int ips[100], ipe[100] ;
  int jps[100], jpe[100] ;
  int shw, i , j , ids, ide, jds, jde, npx, npy ;  /* inputs */
  int Px, Py, P ;                             /* output */
  printf("i, j, ids, ide, jds, jde, npx, npy\n") ;
  scanf("%d %d %d %d %d %d %d %d",&i, &j, &ids,&ide,&jds,&jde,&npx,&npy ) ;
  shw =0 ;
  minx = -99 ;
  miny = 180 ;
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
                   &Px, &Py, &minx, &miny, &ierr ) ;
//  printf("(%3d %3d)   ",Px,Py) ;
  printf("%d %3d\n  ",i, Px) ;
#if 1
  }
  printf("\n") ;
  }
/*  for ( i = 0 ; i < npx*npy ; i++ ) { */
/*    fprintf(stderr,"%3d. ips %d ipe %d (%d) jps %d jpe %d (%d)\n", i, ips[i], ipe[i], ipe[i]-ips[i]+1, jps[i], jpe[i], jpe[i]-jps[i]+1 ) ; */
/*  } */
#endif
}
#endif
