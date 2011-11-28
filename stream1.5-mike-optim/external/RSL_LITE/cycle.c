#ifndef MS_SUA
# include <stdio.h>
#endif
#include <fcntl.h>

#define STANDARD_ERROR 2

#define STANDARD_OUTPUT 1

#ifndef STUBMPI
#  include "mpi.h"
#endif
#include "rsl_lite.h"

#define  UP_EVEN(A)   ((A)+abs((A)%2))
#define  DOWN_EVEN(A) ((A) - abs((A)%2))
#define  UP_ODD(A)    ((A) + abs(((A)+1)%2))
#define  DOWN_ODD(A)  ((A) - abs(((A)+1)%2))
#define  MIN(A,B)     ((A)<(B)?(A):(B))
#define  MAX(A,B)     ((A)>(B)?(A):(B))

static int *y_curs_src = NULL ;
static int *x_curs_src = NULL ;
static int *y_curs_dst = NULL ;
static int *x_curs_dst = NULL ;
static int *x_peermask_src = NULL ;
static int *x_peermask_dst = NULL ;
static int *nbytes_src = NULL ; 
static int *nbytes_dst = NULL ; 

#ifndef STUBMPI
static MPI_Request *x_recv = NULL ,  *x_send = NULL ;
#endif

RSL_LITE_INIT_CYCLE (  int * Fcomm ,
                int * xy0 , int * inout0 ,
                int * n3dR0, int *n2dR0, int * typesizeR0 , 
                int * n3dI0, int *n2dI0, int * typesizeI0 , 
                int * n3dD0, int *n2dD0, int * typesizeD0 , 
                int * n3dL0, int *n2dL0, int * typesizeL0 , 
                int * me0, int * np0 , int * np_x0 , int * np_y0 ,
                int * min_subdomain ,
                int * ids0 , int * ide0 , int * jds0 , int * jde0 , int * kds0 , int * kde0 ,
                int * ips0 , int * ipe0 , int * jps0 , int * jpe0 , int * kps0 , int * kpe0 )
{
  int n3dR, n2dR, typesizeR ;
  int n3dI, n2dI, typesizeI ;
  int n3dD, n2dD, typesizeD ;
  int n3dL, n2dL, typesizeL ;
  int xy, inout ;
  int me, np, np_x, np_y, np_dim ;
  int ids , ide , jds , jde , kds , kde ;
  int ips , ipe , jps , jpe , kps , kpe ;
  int ips_send , ipe_send ;
  int npts, i, ii, j, jj, m, n, ps, pe, ops, ope ;
  int Px, Py, P, Q, swap, coords[2] ;
#ifndef STUBMPI
  MPI_Comm *comm, dummy_comm ;
  int ierr ;

  comm = &dummy_comm ;
  *comm = MPI_Comm_f2c( *Fcomm ) ;

  xy = *xy0 ;
  inout = *inout0 ;     /* 1 is in (uncycled to cycled) 0 is out */
  n3dR = *n3dR0 ; n2dR = *n2dR0 ; typesizeR = *typesizeR0 ;
  n3dI = *n3dI0 ; n2dI = *n2dI0 ; typesizeI = *typesizeI0 ;
  n3dD = *n3dD0 ; n2dD = *n2dD0 ; typesizeD = *typesizeD0 ;
  n3dL = *n3dL0 ; n2dL = *n2dL0 ; typesizeL = *typesizeL0 ;
  me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;
  ids = *ids0-1 ; ide = *ide0-1 ; jds = *jds0-1 ; jde = *jde0-1 ; kds = *kds0-1 ; kde = *kde0-1 ;
  ips = *ips0-1 ; ipe = *ipe0-1 ; jps = *jps0-1 ; jpe = *jpe0-1 ; kps = *kps0-1 ; kpe = *kpe0-1 ;

  if ( nbytes_src == NULL ) nbytes_src = RSL_MALLOC ( int , np ) ;
  if ( nbytes_dst == NULL ) nbytes_dst = RSL_MALLOC ( int , np ) ;
  if ( x_curs_src == NULL ) x_curs_src = RSL_MALLOC ( int , np ) ;
  if ( x_curs_dst == NULL ) x_curs_dst = RSL_MALLOC ( int , np ) ;
  if ( x_peermask_src == NULL ) x_peermask_src = RSL_MALLOC ( int , np ) ;
  if ( x_peermask_dst == NULL ) x_peermask_dst = RSL_MALLOC ( int , np ) ;
  if ( x_recv == NULL ) x_recv = RSL_MALLOC ( MPI_Request , np ) ;
  if ( x_send == NULL ) x_send = RSL_MALLOC ( MPI_Request , np ) ;
  for ( i = 0 ; i < np ; i++ ) { nbytes_src[i] = 0 ; x_curs_src[i] = 0 ; x_peermask_src[i] = 0 ; }
  for ( i = 0 ; i < np ; i++ ) { nbytes_dst[i] = 0 ; x_curs_dst[i] = 0 ; x_peermask_dst[i] = 0 ; }

  if ( xy == 1 ) {   /* xy = 1, cycle in X, otherwise Y */
    np_dim = np_x ;
    ps = ips ;
    pe = ipe ;
    ops = jps ;
    ope = jpe ;
    m = (ide-ids+1)/np_dim ;
    n = (m*np_dim)/m ;
  } else {
    np_dim = np_y ;
    ps = jps ;
    pe = jpe ;
    ops = ips ;
    ope = ipe ;
    m = (jde-jds+1)/np_dim ;
    n = (m*np_dim)/m ;
  }

  for ( i = ps ; i <= MIN(pe,m*np_dim) ; i++ ) {
    ii = (i/n) + (i%n)*m ;
    jj = (i/m) + (i%m)*n ;
    if ( xy == 1 ) {
      TASK_FOR_POINT ( &ii , &jps , &ids, &ide , &jds, &jde , &np_x , &np_y , &Px, &Py, 
                       min_subdomain, min_subdomain, &ierr ) ;
      coords[1] = Px ; coords[0] = Py ;
      MPI_Cart_rank( *comm, coords, &P ) ;
      TASK_FOR_POINT ( &jj , &jps , &ids, &ide , &jds, &jde , &np_x , &np_y , &Px, &Py, 
                       min_subdomain, min_subdomain, &ierr ) ;
      coords[1] = Px ; coords[0] = Py ;
      MPI_Cart_rank( *comm, coords, &Q ) ;
    } else {
      TASK_FOR_POINT ( &ips , &ii , &ids, &ide , &jds, &jde , &np_x , &np_y , &Px, &Py, 
                       min_subdomain, min_subdomain, &ierr ) ;
      coords[1] = Px ; coords[0] = Py ;
      MPI_Cart_rank( *comm, coords, &P ) ;
      TASK_FOR_POINT ( &ips , &jj , &ids, &ide , &jds, &jde , &np_x , &np_y , &Px, &Py, 
                       min_subdomain, min_subdomain, &ierr ) ;
      coords[1] = Px ; coords[0] = Py ;
      MPI_Cart_rank( *comm, coords, &Q ) ;
    }
    if ( inout == 0 ) { swap = P ; P = Q ; Q = swap ; }

    nbytes_src[P] += typesizeR*(ope-ops+1)*(n3dR*(kpe-kps+1)+n2dR) +
                     typesizeI*(ope-ops+1)*(n3dI*(kpe-kps+1)+n2dI) +
                     typesizeD*(ope-ops+1)*(n3dD*(kpe-kps+1)+n2dD) +
                     typesizeL*(ope-ops+1)*(n3dL*(kpe-kps+1)+n2dL) ;

    nbytes_dst[Q] += typesizeR*(ope-ops+1)*(n3dR*(kpe-kps+1)+n2dR) +
                     typesizeI*(ope-ops+1)*(n3dI*(kpe-kps+1)+n2dI) +
                     typesizeD*(ope-ops+1)*(n3dD*(kpe-kps+1)+n2dD) +
                     typesizeL*(ope-ops+1)*(n3dL*(kpe-kps+1)+n2dL) ;
  }

  for ( P = 0 ; P < np ; P++ ) {
       buffer_for_proc ( P , nbytes_src[P], RSL_SENDBUF ) ;
       buffer_for_proc ( P , nbytes_dst[P], RSL_RECVBUF ) ;
  }
#endif
}

RSL_LITE_PACK_CYCLE ( int * Fcomm, char * buf , int * inout0 , int * typesize0 , int * xy0 , int * pu0 , char * memord , int * xstag0 ,
           int *me0, int * np0 , int * np_x0 , int * np_y0 , 
           int * min_subdomain,
           int * ids0 , int * ide0 , int * jds0 , int * jde0 , int * kds0 , int * kde0 ,
           int * ims0 , int * ime0 , int * jms0 , int * jme0 , int * kms0 , int * kme0 ,
           int * ips0 , int * ipe0 , int * jps0 , int * jpe0 , int * kps0 , int * kpe0 )
{
  int me, np, np_x, np_y, np_dim ;
  int inout , typesize ;
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
#ifndef STUBMPI
  MPI_Comm *comm, dummy_comm ;

  comm = &dummy_comm ;
  *comm = MPI_Comm_f2c( *Fcomm ) ;

  me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;
  xstag = *xstag0 ;
  inout = *inout0 ; typesize = *typesize0 ;
  ids = *ids0-1 ; ide = *ide0-1 ; jds = *jds0-1 ; jde = *jde0-1 ; kds = *kds0-1 ; kde = *kde0-1 ;
  ims = *ims0-1 ; ime = *ime0-1 ; jms = *jms0-1 ; jme = *jme0-1 ; kms = *kms0-1 ; kme = *kme0-1 ;
  ips = *ips0-1 ; ipe = *ipe0-1 ; jps = *jps0-1 ; jpe = *jpe0-1 ; kps = *kps0-1 ; kpe = *kpe0-1 ;
  xy = *xy0 ;
  pu = *pu0 ;

/* need to adapt for other memory orders */
#define IMAX(A) (((A)>ids)?(A):ids)
#define IMIN(A) (((A)<ide)?(A):ide)
#define JMAX(A) (((A)>jds)?(A):jds)
#define JMIN(A) (((A)<jde)?(A):jde)

  da_buf = ( pu == 0 ) ? RSL_SENDBUF : RSL_RECVBUF ;

  if ( xy == 1 ) {   /* xy = 1, cycle in X, otherwise Y */
    np_dim = np_x ;
    ps = ips ;
    pe = ipe ;
    m = (ide-ids+1)/np_dim ;
    n = (m*np_dim)/m ;
  } else {
    np_dim = np_y ;
    ps = jps ;
    pe = jpe ;
    m = (jde-jds+1)/np_dim ;
    n = (m*np_dim)/m ;
  }

  if ( np_x > 1 && xy == 1 ) {

    for ( i = ips ; i <= MIN(ipe,m*np_dim-1) ; i++ ) {
      if ( pu == 0 ) {
        ii = (inout)?(i/n)+(i%n)*m:(i/m)+(i%m)*n  ;
        TASK_FOR_POINT ( &ii , &jps , &ids, &ide , &jds, &jde , &np_x , &np_y , &Px, &Py, 
                       min_subdomain, min_subdomain, &ierr ) ;
        coords[1] = Px ; coords[0] = Py ;
        MPI_Cart_rank( *comm, coords, &P ) ;
        p = buffer_for_proc( P , 0 , da_buf ) ;
	if ( typesize == sizeof(int) ) {
          for ( j = jps ; j <= jpe ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
	      pi = (int *)(p+x_curs_src[P]) ;
	      qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
	      *pi++ = *qi++ ;
	      x_curs_src[P] += typesize ;
	    }
	  }
	}
	else {
          for ( j = jps ; j <= jpe ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              for ( t = 0 ; t < typesize ; t++ ) {
                *(p+x_curs_src[P]) = 
                               *(buf + t + typesize*(
                                      (i-ims) + (ime-ims+1)*(
                                      (k-kms) + (j-jms)*(kme-kms+1))) ) ;
                x_curs_src[P]++ ;
              }
            }
          }
	}
      } else {
        ii = (inout)?(i/m)+(i%m)*n:(i/n)+(i%n)*m  ;
        TASK_FOR_POINT ( &ii , &jps , &ids, &ide , &jds, &jde , &np_x , &np_y , &Px, &Py, 
                       min_subdomain, min_subdomain, &ierr ) ;
        coords[1] = Px ; coords[0] = Py ;
        MPI_Cart_rank( *comm, coords, &P ) ;
        p = buffer_for_proc( P , 0 , da_buf ) ;
	if ( typesize == sizeof(int) ) {
          for ( j = jps ; j <= jpe ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
	      pi = (int *)(p+x_curs_dst[P]) ;
	      qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
	      *qi++ = *pi++ ;
	      x_curs_dst[P] += typesize ;
	    }
	  }
	}
	else {
          for ( j = jps ; j <= jpe ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              for ( t = 0 ; t < typesize ; t++ ) {
                               *(buf + t + typesize*(
                                      (i-ims) + (ime-ims+1)*(
                                      (k-kms) + (j-jms)*(kme-kms+1))) ) =
                *(p+x_curs_dst[P]) ;
                x_curs_dst[P]++ ;
              }
            }
          }
        }
      }
    }
  } else if ( np_y > 1 && xy == 0 ) {
    for ( j = jps ; j <= MIN(jpe,m*np_dim-1) ; j++ ) {
      if ( pu == 0 ) {
        jj = (inout)?(j/n) + (j%n)*m:(j/m) + (j%m)*n ;
        TASK_FOR_POINT ( &ips , &jj , &ids, &ide , &jds, &jde , &np_x , &np_y , &Px, &Py, 
                       min_subdomain, min_subdomain, &ierr ) ;
        coords[1] = Px ; coords[0] = Py ;
        MPI_Cart_rank( *comm, coords, &P ) ;
        p = buffer_for_proc( P , 0 , da_buf ) ;
	if ( typesize == sizeof(int) ) {
          for ( i = ips ; i <= ipe ; i++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
	      pi = (int *)(p+x_curs_src[P]) ;
	      qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
	      *pi++ = *qi++ ;
	      x_curs_src[P] += typesize ;
	    }
	  }
	}
	else {
          for ( i = ips ; i <= ipe ; i++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              for ( t = 0 ; t < typesize ; t++ ) {
                *(p+x_curs_src[P]) = 
                               *(buf + t + typesize*(
                                      (i-ims) + (ime-ims+1)*(
                                      (k-kms) + (j-jms)*(kme-kms+1))) ) ;
                x_curs_src[P]++ ;
              }
            }
          }
	}
      } else {
        jj = (inout)?(j/m) + (j%m)*n:(j/n) + (j%n)*m ;
        TASK_FOR_POINT ( &ips , &jj , &ids, &ide , &jds, &jde , &np_x , &np_y , &Px, &Py, 
                       min_subdomain, min_subdomain, &ierr ) ;
        coords[1] = Px ; coords[0] = Py ;
        MPI_Cart_rank( *comm, coords, &P ) ;
        p = buffer_for_proc( P , 0 , da_buf ) ;
	if ( typesize == sizeof(int) ) {
          for ( i = ips ; i <= ipe ; i++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
	      pi = (int *)(p+x_curs_dst[P]) ;
	      qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
	      *qi++ = *pi++ ;
	      x_curs_dst[P] += typesize ;
	    }
	  }
	}
	else {
          for ( i = ips ; i <= ipe ; i++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              for ( t = 0 ; t < typesize ; t++ ) {
                               *(buf + t + typesize*(
                                      (i-ims) + (ime-ims+1)*(
                                      (k-kms) + (j-jms)*(kme-kms+1))) ) =
                *(p+x_curs_dst[P]) ;
                x_curs_dst[P]++ ;
              }
            }
          }
        }
      }
    }
  }
#endif
}

RSL_LITE_CYCLE ( int * Fcomm0, int *me0, int * np0 , int * np_x0 , int * np_y0 )
{
  int me, np, np_x, np_y ;
  int yp, ym, xp, xm, nb ;
#ifndef STUBMPI
  MPI_Status stat ;
  MPI_Comm comm, *comm0, dummy_comm ;
  int i, P ;

  comm0 = &dummy_comm ;
  *comm0 = MPI_Comm_f2c( *Fcomm0 ) ;

  comm = *comm0 ; me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;

  for ( P = 0 ; P < np ; P++ ) {
      nb = buffer_size_for_proc( P, RSL_RECVBUF ) ;
      MPI_Irecv ( buffer_for_proc( P, 0, RSL_RECVBUF ), nb, MPI_CHAR, P, me, comm, &(x_recv[P]) ) ;
      MPI_Isend ( buffer_for_proc( P, 0, RSL_SENDBUF ), x_curs_src[P], MPI_CHAR, P, P, comm, &(x_send[P]) ) ;
  }
  for ( P = 0 ; P < np ; P++ ) {
      MPI_Wait( &x_recv[P], &stat ) ; 
      MPI_Wait( &x_send[P], &stat ) ; 
  }
  for ( i = 0 ; i < np ; i++ ) {  x_curs_src[i] = 0 ; x_curs_dst[i] ; }
#endif
}

