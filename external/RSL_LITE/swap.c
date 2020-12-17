#ifndef MS_SUA
# include <stdio.h>
#endif
#include <fcntl.h>

#define STANDARD_ERROR 2

#define STANDARD_OUTPUT 1

#ifndef STUBMPI
# include "mpi.h"
#endif
#include "rsl_lite.h"

#define  UP_EVEN(A)   ((A)+abs((A)%2))
#define  DOWN_EVEN(A) ((A) - abs((A)%2))
#define  UP_ODD(A)    ((A) + abs(((A)+1)%2))
#define  DOWN_ODD(A)  ((A) - abs(((A)+1)%2))
#define  MIN(A,B)     ((A)<(B)?(A):(B))
#define  MAX(A,B)     ((A)>(B)?(A):(B))

static int *y_curs = NULL ;
static int *x_curs = NULL ;
static int *x_peermask = NULL ;
static int *nbytes = NULL ; 
#ifndef STUBMPI
static MPI_Request *x_recv = NULL , *x_send = NULL ;
#endif

RSL_LITE_INIT_SWAP ( 
                int * Fcomm ,
                int * xy0 ,
                int * n3dR0, int *n2dR0, int * typesizeR0 , 
                int * n3dI0, int *n2dI0, int * typesizeI0 , 
                int * n3dD0, int *n2dD0, int * typesizeD0 , 
                int * n3dL0, int *n2dL0, int * typesizeL0 , 
                int * me0, int * np0 , int * np_x0 , int * np_y0 ,
                int * min_subdomain ,
                int * ids0 , int * ide0 , int * jds0 , int * jde0 , int * kds0 , int * kde0 ,
                int * ips0 , int * ipe0 , int * jps0 , int * jpe0 , int * kps0 , int * kpe0 )
{
#ifndef STUBMPI
  int n3dR, n2dR, typesizeR ;
  int n3dI, n2dI, typesizeI ;
  int n3dD, n2dD, typesizeD ;
  int n3dL, n2dL, typesizeL ;
  int xy ;
  int me, np, np_x, np_y ;
  int ids , ide , jds , jde , kds , kde ;
  int ips , ipe , jps , jpe , kps , kpe ;
  int ips_send , ipe_send ;
  int npts, i, ii, j, m, n, ps, pe, ops, ope ;
  int Px, Py, P, coords[2] ;
  int ips_swap, ipe_swap ;
  MPI_Comm *comm, dummy_comm ;
  int ierr ;

  comm = &dummy_comm ;
  *comm = MPI_Comm_f2c( *Fcomm ) ;

  xy = *xy0 ;
  n3dR = *n3dR0 ; n2dR = *n2dR0 ; typesizeR = *typesizeR0 ;
  n3dI = *n3dI0 ; n2dI = *n2dI0 ; typesizeI = *typesizeI0 ;
  n3dD = *n3dD0 ; n2dD = *n2dD0 ; typesizeD = *typesizeD0 ;
  n3dL = *n3dL0 ; n2dL = *n2dL0 ; typesizeL = *typesizeL0 ;
  me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;
  ids = *ids0-1 ; ide = *ide0-1 ; jds = *jds0-1 ; jde = *jde0-1 ; kds = *kds0-1 ; kde = *kde0-1 ;
  ips = *ips0-1 ; ipe = *ipe0-1 ; jps = *jps0-1 ; jpe = *jpe0-1 ; kps = *kps0-1 ; kpe = *kpe0-1 ;

  if ( nbytes == NULL ) nbytes = RSL_MALLOC ( int , np ) ;
  if ( x_curs == NULL ) x_curs = RSL_MALLOC ( int , np ) ;
  if ( x_peermask == NULL ) x_peermask = RSL_MALLOC ( int , np ) ;
  if ( x_recv == NULL ) x_recv = RSL_MALLOC ( MPI_Request , np ) ;
  if ( x_send == NULL ) x_send = RSL_MALLOC ( MPI_Request , np ) ;
  for ( i = 0 ; i < np ; i++ ) { nbytes[i] = 0 ; x_curs[i] = 0 ; x_peermask[i] = 0 ; }

  if ( xy == 1 ) {   /* xy = 1, swap in X, otherwise Y */
    n = (ide-ids+1)/4*2 ;
    m = n*2 ;
    ps = ips ;
    pe = ipe ;
    ops = jps ;
    ope = jpe ;
  } else {
    n = (jde-jds+1)/4*2 ;
    m = n*2 ;
    ps = jps ;
    pe = jpe ;
    ops = ips ;
    ope = ipe ;
  }

  for ( i = UP_ODD( ps ) ; i <= MIN(pe,m) ; i+=2 ) {
    ii = abs(i+n) % m ;
    if ( xy == 1 ) {
      TASK_FOR_POINT ( &ii , &jps , &ids, &ide , &jds, &jde , &np_x , &np_y , &Px, &Py, 
                       min_subdomain, min_subdomain, &ierr ) ;
      coords[1] = Px ; coords[0] = Py ;
      MPI_Cart_rank( *comm, coords, &P ) ;
    } else {
      TASK_FOR_POINT ( &ips , &ii , &ids, &ide , &jds, &jde , &np_x , &np_y , &Px, &Py, 
                       min_subdomain, min_subdomain, &ierr ) ;
      coords[1] = Px ; coords[0] = Py ;
      MPI_Cart_rank( *comm, coords, &P ) ;
    }
    nbytes[P] += typesizeR*(ope-ops+1)*(n3dR*(kpe-kps+1)+n2dR) +
                 typesizeI*(ope-ops+1)*(n3dI*(kpe-kps+1)+n2dI) +
                 typesizeD*(ope-ops+1)*(n3dD*(kpe-kps+1)+n2dD) +
                 typesizeL*(ope-ops+1)*(n3dL*(kpe-kps+1)+n2dL) ;
    x_peermask[P] = 1 ;
  }

  for ( P = 0 ; P < np ; P++ ) {
     if ( x_peermask[P] ) {
       buffer_for_proc ( P , nbytes[P], RSL_RECVBUF ) ;
       buffer_for_proc ( P , nbytes[P], RSL_SENDBUF ) ;
     }
  }
#endif
}

RSL_LITE_PACK_SWAP ( int * Fcomm , char * buf , int * odd0 , int * typesize0 , int * xy0 , int * pu0 , char * memord , int * xstag0 ,
           int *me0, int * np0 , int * np_x0 , int * np_y0 , 
           int * min_subdomain ,
           int * ids0 , int * ide0 , int * jds0 , int * jde0 , int * kds0 , int * kde0 ,
           int * ims0 , int * ime0 , int * jms0 , int * jme0 , int * kms0 , int * kme0 ,
           int * ips0 , int * ipe0 , int * jps0 , int * jpe0 , int * kps0 , int * kpe0 )
{
#ifndef STUBMPI
  int me, np, np_x, np_y ;
  int odd , typesize ;
  int ids , ide , jds , jde , kds , kde ;
  int ims , ime , jms , jme , kms , kme ;
  int ips , ipe , jps , jpe , kps , kpe ;
  int xstag ;  /* 0 not stag, 1 stag */
  int xy ;   /* y = 0 , x = 1 */
  int pu ;   /* pack = 0 , unpack = 1 */
  int i, ii, j, jj, m, n  ;
  int ps, pe, ops, ope ;
  register int k, t ;
#ifdef crayx1
  register int i2,i3,i4,i_offset;
#endif
  char *p ;
  int da_buf ;
  int Px, Py, P, coords[2] ;
  int ierr = 0 ;
  register int *pi, *qi ;
  float f ;
  MPI_Comm *comm, dummy_comm ;

  comm = &dummy_comm ;
  *comm = MPI_Comm_f2c( *Fcomm ) ;

  me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;
  xstag = *xstag0 ;
  odd = *odd0 ; typesize = *typesize0 ;
  ids = *ids0-1 ; ide = *ide0-1 ; jds = *jds0-1 ; jde = *jde0-1 ; kds = *kds0-1 ; kde = *kde0-1 ;
  ims = *ims0-1 ; ime = *ime0-1 ; jms = *jms0-1 ; jme = *jme0-1 ; kms = *kms0-1 ; kme = *kme0-1 ;
  ips = *ips0-1 ; ipe = *ipe0-1 ; jps = *jps0-1 ; jpe = *jpe0-1 ; kps = *kps0-1 ; kpe = *kpe0-1 ;
  xy = *xy0 ;
  pu = *pu0 ;

/* need to adapt for other memory orders */
#define RANGE(S1,E1,S2,E2,S3,E3,S4,E4) (((E1)-(S1)+1)*((E2)-(S2)+1)*(((E3)-(S3)+1)/2)*((E4)-(S4)+1))
#define IMAX(A) (((A)>ids)?(A):ids)
#define IMIN(A) (((A)<ide)?(A):ide)
#define JMAX(A) (((A)>jds)?(A):jds)
#define JMIN(A) (((A)<jde)?(A):jde)

  da_buf = ( pu == 0 ) ? RSL_SENDBUF : RSL_RECVBUF ;


  if ( xy == 1 ) {   /* xy = 1, swap in X, otherwise Y */
    n = (ide-ids+1)/4*2 ;
    m = n*2 ;
  } else {
    n = (jde-jds+1)/4*2 ;
    m = n*2 ;
  }

  if ( np_x > 1 && xy == 1 ) {

    for ( i = UP_ODD(ips) ; i <= MIN(ipe,m) ; i+=2 ) {
      ii = abs(i+n) % m ;
      TASK_FOR_POINT ( &ii , &jps , &ids, &ide , &jds, &jde , &np_x , &np_y , &Px, &Py, 
                       min_subdomain, min_subdomain, &ierr ) ;
      coords[1] = Px ; coords[0] = Py ;
      MPI_Cart_rank( *comm, coords, &P ) ;
      p = buffer_for_proc( P , 0 , da_buf ) ;
      if ( pu == 0 ) {
	if ( typesize == sizeof(int) ) {
          for ( j = JMAX(jps) ; j <= JMIN(jpe) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
	      pi = (int *)(p+x_curs[P]) ;
	      qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
	      *pi++ = *qi++ ;
	      x_curs[P] += typesize ;
	    }
	  }
	}
	else {
          for ( j = JMAX(jps) ; j <= JMIN(jpe) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              for ( t = 0 ; t < typesize ; t++ ) {
                *(p+x_curs[P]) = 
                               *(buf + t + typesize*(
                                      (i-ims) + (ime-ims+1)*(
                                      (k-kms) + (j-jms)*(kme-kms+1))) ) ;
                x_curs[P]++ ;
              }
            }
          }
	}
      } else {
	if ( typesize == sizeof(int) ) {
          for ( j = JMAX(jps) ; j <= JMIN(jpe) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
	      pi = (int *)(p+x_curs[P]) ;
	      qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
	      *qi++ = *pi++ ;
	      x_curs[P] += typesize ;
	    }
	  }
	}
	else {
          for ( j = JMAX(jps) ; j <= JMIN(jpe) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              for ( t = 0 ; t < typesize ; t++ ) {
                               *(buf + t + typesize*(
                                      (i-ims) + (ime-ims+1)*(
                                      (k-kms) + (j-jms)*(kme-kms+1))) ) =
                *(p+x_curs[P]) ;
                x_curs[P]++ ;
              }
            }
          }
        }
      }
    }
  } else if ( np_y > 1 && xy == 0 ) {
    for ( j = UP_ODD(jps) ; j <= MIN(jpe,m) ; j+=2 ) {
      jj = abs(j+n) % m ;
      TASK_FOR_POINT ( &ips , &jj , &ids, &ide , &jds, &jde , &np_x , &np_y , &Px, &Py, 
                       min_subdomain, min_subdomain, &ierr ) ;
      coords[1] = Px ; coords[0] = Py ;
      MPI_Cart_rank( *comm, coords, &P ) ;
      p = buffer_for_proc( P , 0 , da_buf ) ;
      if ( pu == 0 ) {
	if ( typesize == sizeof(int) ) {
          for ( i = IMAX(ips) ; i <= IMIN(ipe) ; i++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
	      pi = (int *)(p+x_curs[P]) ;
	      qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
	      *pi++ = *qi++ ;
	      x_curs[P] += typesize ;
	    }
	  }
	}
	else {
          for ( i = IMAX(ips) ; i <= IMIN(ipe) ; i++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              for ( t = 0 ; t < typesize ; t++ ) {
                *(p+x_curs[P]) = 
                               *(buf + t + typesize*(
                                      (i-ims) + (ime-ims+1)*(
                                      (k-kms) + (j-jms)*(kme-kms+1))) ) ;
                x_curs[P]++ ;
              }
            }
          }
	}
      } else {
	if ( typesize == sizeof(int) ) {
          for ( i = IMAX(ips) ; i <= IMIN(ipe) ; i++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
	      pi = (int *)(p+x_curs[P]) ;
	      qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
	      *qi++ = *pi++ ;
	      x_curs[P] += typesize ;
	    }
	  }
	}
	else {
          for ( i = IMAX(ips) ; i <= IMIN(ipe) ; i++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              for ( t = 0 ; t < typesize ; t++ ) {
                               *(buf + t + typesize*(
                                      (i-ims) + (ime-ims+1)*(
                                      (k-kms) + (j-jms)*(kme-kms+1))) ) =
                *(p+x_curs[P]) ;
                x_curs[P]++ ;
              }
            }
          }
        }
      }
    }
  }
#endif
}

RSL_LITE_SWAP ( int * Fcomm0, int *me0, int * np0 , int * np_x0 , int * np_y0 )
{
#ifndef STUBMPI
  int me, np, np_x, np_y ;
  int yp, ym, xp, xm, nb ;
  MPI_Status stat ;
  MPI_Comm comm, *comm0, dummy_comm ;
  int i, P ;

  comm0 = &dummy_comm ;
  *comm0 = MPI_Comm_f2c( *Fcomm0 ) ;
#if 1

  comm = *comm0 ; me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;

/* fprintf(stderr,"RSL_LITE_SWAP\n") ; */

  for ( P = 0 ; P < np ; P++ ) {
    if ( x_peermask[P] ) {
      nb = buffer_size_for_proc( P, RSL_RECVBUF ) ;
/* fprintf(stderr,"posting irecv from %d, nb = %d\n",P,nb) ; */
      MPI_Irecv ( buffer_for_proc( P, x_curs[P], RSL_RECVBUF ), nb, MPI_CHAR, P, me, comm, &(x_recv[P]) ) ;
/* fprintf(stderr,"sending to         %d, nb = %d\n",P,x_curs[P]) ; */
      MPI_Isend ( buffer_for_proc( P, 0,         RSL_SENDBUF ), x_curs[P], MPI_CHAR, P, P, comm, &(x_send[P]) ) ;
    }
  }
  for ( P = 0 ; P < np ; P++ ) {
    if ( x_peermask[P] ) {
      MPI_Wait( &x_recv[P], &stat ) ; 
      MPI_Wait( &x_send[P], &stat ) ; 
    }
  }
#else 
# ifndef MS_SUA
fprintf(stderr,"RSL_LITE_SWAP disabled\n") ;
# endif
#endif
  for ( i = 0 ; i < np ; i++ ) {  x_curs[i] = 0 ;  }
#endif
}

