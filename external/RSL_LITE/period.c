#include <stdio.h>
#include <fcntl.h>

#define STANDARD_ERROR 2

#define STANDARD_OUTPUT 1

#include "mpi.h"
#include "rsl_lite.h"

static int yp_curs, ym_curs, xp_curs, xm_curs ;

RSL_LITE_INIT_PERIOD ( 
                int * shw0,
                int * n3dR0, int *n2dR0, int * typesizeR0 , 
                int * n3dI0, int *n2dI0, int * typesizeI0 , 
                int * n3dD0, int *n2dD0, int * typesizeD0 , 
                int * n3dL0, int *n2dL0, int * typesizeL0 , 
                int * me0, int * np0 , int * np_x0 , int * np_y0 ,
                int * ips0 , int * ipe0 , int * jps0 , int * jpe0 , int * kps0 , int * kpe0 )
{
  int n3dR, n2dR, typesizeR ;
  int n3dI, n2dI, typesizeI ;
  int n3dD, n2dD, typesizeD ;
  int n3dL, n2dL, typesizeL ;
  int shw ;
  int me, np, np_x, np_y ;
  int ips , ipe , jps , jpe , kps , kpe ;
  int yp, ym, xp, xm ;
  int nbytes ;

  shw = *shw0 ;
  n3dR = *n3dR0 ; n2dR = *n2dR0 ; typesizeR = *typesizeR0 ;
  n3dI = *n3dI0 ; n2dI = *n2dI0 ; typesizeI = *typesizeI0 ;
  n3dD = *n3dD0 ; n2dD = *n2dD0 ; typesizeD = *typesizeD0 ;
  n3dL = *n3dL0 ; n2dL = *n2dL0 ; typesizeL = *typesizeL0 ;
  me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;
  ips = *ips0-1 ; ipe = *ipe0-1 ; jps = *jps0-1 ; jpe = *jpe0-1 ; kps = *kps0-1 ; kpe = *kpe0-1 ;

#if 0
  if ( np_y > 1 ) {
    nbytes = typesizeR*(ipe-ips+1+2*shw)*(shw+1)*(n3dR*(kpe-kps+1)+n2dR) +
             typesizeI*(ipe-ips+1+2*shw)*(shw+1)*(n3dI*(kpe-kps+1)+n2dI) +
             typesizeD*(ipe-ips+1+2*shw)*(shw+1)*(n3dD*(kpe-kps+1)+n2dD) +
             typesizeL*(ipe-ips+1+2*shw)*(shw+1)*(n3dL*(kpe-kps+1)+n2dL) ;
    yp = me + np_x ; ym = me - np_x ;
    if ( yp >= 0 && yp < np ) {
       buffer_for_proc ( yp , nbytes, RSL_RECVBUF ) ;
       buffer_for_proc ( yp , nbytes, RSL_SENDBUF ) ;
    }
    if ( ym >= 0 && ym < np ) {
       buffer_for_proc ( ym , nbytes, RSL_RECVBUF ) ;
       buffer_for_proc ( ym , nbytes, RSL_SENDBUF ) ;
    }
  }
#endif
  if ( np_x > 1 ) {
    nbytes = typesizeR*(jpe-jps+1+2*shw)*(shw+1)*(n3dR*(kpe-kps+1)+n2dR) +
             typesizeI*(jpe-jps+1+2*shw)*(shw+1)*(n3dI*(kpe-kps+1)+n2dI) +
             typesizeD*(jpe-jps+1+2*shw)*(shw+1)*(n3dD*(kpe-kps+1)+n2dD) +
             typesizeL*(jpe-jps+1+2*shw)*(shw+1)*(n3dL*(kpe-kps+1)+n2dL) ;

    if ( me % np_x == 0 ) {
       buffer_for_proc ( me + np_x - 1 , nbytes, RSL_RECVBUF ) ;
       buffer_for_proc ( me + np_x - 1 , nbytes, RSL_SENDBUF ) ;
    }
    if ( me % np_x == np_x - 1 ) {
       buffer_for_proc ( me - np_x + 1 , nbytes, RSL_RECVBUF ) ;
       buffer_for_proc ( me - np_x + 1 , nbytes, RSL_SENDBUF ) ;
    }
  }
  yp_curs = 0 ; ym_curs = 0 ; xp_curs = 0 ; xm_curs = 0 ;
}

RSL_LITE_PACK_PERIOD_X ( char * buf , int * shw0 , int * typesize0 , int * xy0 , int * pu0 , char * memord , int * xstag0 ,
           int *me0, int * np0 , int * np_x0 , int * np_y0 , 
           int * ids0 , int * ide0 , int * jds0 , int * jde0 , int * kds0 , int * kde0 ,
           int * ims0 , int * ime0 , int * jms0 , int * jme0 , int * kms0 , int * kme0 ,
           int * ips0 , int * ipe0 , int * jps0 , int * jpe0 , int * kps0 , int * kpe0 )
{
  int me, np, np_x, np_y ;
  int shw , typesize ;
  int ids , ide , jds , jde , kds , kde ;
  int ims , ime , jms , jme , kms , kme ;
  int ips , ipe , jps , jpe , kps , kpe ;
  int xstag ;  /* 0 not stag, 1 stag */
  int xy ;   /* y = 0 , x = 1 */
  int pu ;   /* pack = 0 , unpack = 1 */
  register int i, j, k, t ;
#ifdef crayx1
  register int i2,i3,i4,i_offset;
#endif
  char *p ;
  int da_buf ;
  int yp, ym, xp, xm ;
  int nbytes, ierr ;
  register int *pi, *qi ;
float f ;

  me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;
  xstag = *xstag0 ;
  shw = *shw0 ; typesize = *typesize0 ;
  ids = *ids0-1 ; ide = *ide0-1 ; jds = *jds0-1 ; jde = *jde0-1 ; kds = *kds0-1 ; kde = *kde0-1 ;
  ims = *ims0-1 ; ime = *ime0-1 ; jms = *jms0-1 ; jme = *jme0-1 ; kms = *kms0-1 ; kme = *kme0-1 ;
  ips = *ips0-1 ; ipe = *ipe0-1 ; jps = *jps0-1 ; jpe = *jpe0-1 ; kps = *kps0-1 ; kpe = *kpe0-1 ;
  xy = *xy0 ;
  pu = *pu0 ;

/* need to adapt for other memory orders */

#define RANGE(S1,E1,S2,E2,S3,E3,S4,E4) (((E1)-(S1)+1)*((E2)-(S2)+1)*((E3)-(S3)+1)*((E4)-(S4)+1))
#define IMAX(A) (((A)>ids)?(A):ids)
#define IMIN(A) (((A)<ide)?(A):ide)
#define JMAX(A) (((A)>jds)?(A):jds)
#define JMIN(A) (((A)<jde)?(A):jde)

  da_buf = ( pu == 0 ) ? RSL_SENDBUF : RSL_RECVBUF ;

  if ( np_x > 1 && xy == 1 ) {
    if ( me % np_x == np_x - 1 ) {

      p = buffer_for_proc( me - np_x + 1 , 0 , da_buf ) ;
      if ( pu == 0 ) {
        nbytes = buffer_size_for_proc( me - np_x + 1, da_buf ) ;

        if ( xp_curs + RANGE( JMAX(jps-shw), JMIN(jpe+shw), kps, kpe, ipe-shw, ipe-1, 1, typesize ) > nbytes ) {
	  fprintf(stderr,"memory overwrite in rsl_lite_pack_period_x, right hand X to %d, %d > %d\n",me - np_x + 1,
	      xp_curs + RANGE( JMAX(jps-shw), JMIN(jpe+shw), kps, kpe, ipe-shw, ipe-1, 1, typesize ), nbytes ) ;
	  MPI_Abort(MPI_COMM_WORLD, ierr) ;
        }
	if ( typesize == sizeof(int) ) {
          for ( j = JMAX(jps-shw) ; j <= JMIN(jpe+shw) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
	      pi = (int *)(p+xp_curs) ;
	      i = ipe-shw ;
	      qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
              for ( i = ipe-shw ; i <= ipe-1 ; i++ ) {
	        *pi++ = *qi++ ;
	        xp_curs += typesize ;
	      }
	    }
	  }
	}
	else {
          for ( j = JMAX(jps-shw) ; j <= JMIN(jpe+shw) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              for ( i = ipe-shw ; i <= ipe-1 ; i++ ) {
                for ( t = 0 ; t < typesize ; t++ ) {
                  *(p+xp_curs) = 
                                 *(buf + t + typesize*(
                                        (i-ims) + (ime-ims+1)*(
                                        (k-kms) + (j-jms)*(kme-kms+1))) ) ;
                  xp_curs++ ;
                }
              }
            }
          }
	}
      } else {
	if ( typesize == sizeof(int) ) {
          for ( j = JMAX(jps-shw) ; j <= JMIN(jpe+shw) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
	      pi = (int *)(p+xp_curs) ;
	      i = ipe ;
	      qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
              for ( i = ipe ; i <= ipe+shw-1+xstag ; i++ ) {
	        *qi++ = *pi++ ;
	        xp_curs += typesize ;
	      }
	    }
	  }
	}
	else {
          for ( j = JMAX(jps-shw) ; j <= JMIN(jpe+shw) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              for ( i = ipe ; i <= ipe+shw-1+xstag ; i++ ) {
                for ( t = 0 ; t < typesize ; t++ ) {
                                 *(buf + t + typesize*(
                                        (i-ims) + (ime-ims+1)*(
                                        (k-kms) + (j-jms)*(kme-kms+1))) ) =
                  *(p+xp_curs) ;
                  xp_curs++ ;
                }
              }
            }
          }
        }
      }
    }
    if ( me % np_x == 0 ) {
      p = buffer_for_proc( me + np_x - 1 , 0 , da_buf ) ;
      if ( pu == 0 ) {
        nbytes = buffer_size_for_proc( me + np_x - 1 , da_buf ) ;
        if ( xm_curs + RANGE( JMAX(jps-shw), JMIN(jpe+shw), kps, kpe, ips, ips+shw-1+xstag, 1, typesize ) > nbytes ) {
	  fprintf(stderr,"memory overwrite in rsl_lite_pack,  left hand X to %d , %d > %d\n",me + np_x - 1,
	      xm_curs + RANGE( JMAX(jps-shw), JMIN(jpe+shw), kps, kpe, ips, ips+shw-1+xstag, 1, typesize ), nbytes ) ;
	  MPI_Abort(MPI_COMM_WORLD, ierr) ;
        }
	if ( typesize == sizeof(int) ) {
          for ( j = JMAX(jps-shw) ; j <= JMIN(jpe+shw) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
	      pi = (int *)(p+xm_curs) ;
	      i = ips ;
	      qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
              for ( i = ips ; i <= ips+shw-1+xstag ; i++ ) {
	        *pi++ = *qi++ ;
	        xm_curs += typesize ;
	      }
	    }
	  }
	}
	else {
          for ( j = JMAX(jps-shw) ; j <= JMIN(jpe+shw) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              for ( i = ips ; i <= ips+shw-1+xstag ; i++ ) {
                for ( t = 0 ; t < typesize ; t++ ) {
                  *(p+xm_curs) = 
                                 *(buf + t + typesize*(
                                        (i-ims) + (ime-ims+1)*(
                                        (k-kms) + (j-jms)*(kme-kms+1))) ) ;
                  xm_curs++ ;
                }
              }
            }
          }
        }
      } else {
	if ( typesize == sizeof(int) ) {
          for ( j = JMAX(jps-shw) ; j <= JMIN(jpe+shw) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
	      pi = (int *)(p+xm_curs) ;
	      i = ips-shw ;
	      qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
              for ( i = ips-shw ; i < ips ; i++ ) {
	        *qi++ = *pi++ ;
	        xm_curs += typesize ;
	      }
	    }
	  }
	}
	else {
          for ( j = JMAX(jps-shw) ; j <= JMIN(jpe+shw) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              for ( i = ips-shw ; i < ips ; i++ ) {
                for ( t = 0 ; t < typesize ; t++ ) {
                                 *(buf + t + typesize*(
                                        (i-ims) + (ime-ims+1)*(
                                        (k-kms) + (j-jms)*(kme-kms+1))) ) =
                  *(p+xm_curs) ;
                  xm_curs++ ;
                }
              }
            }
          }
        }
      }
    }
  }
}

static MPI_Request yp_recv, ym_recv, yp_send, ym_send ;
static MPI_Request xp_recv, xm_recv, xp_send, xm_send ;

#if 0
RSL_LITE_EXCH_PERIOD_Y ( int * Fcomm0, int *me0, int * np0 , int * np_x0 , int * np_y0 )
{
  int me, np, np_x, np_y ;
  int yp, ym, xp, xm, ierr ;
  MPI_Status stat ;
  MPI_Comm comm, *comm0, dummy_comm ;

  comm0 = &dummy_comm ;
  *comm0 = MPI_Comm_f2c( *Fcomm0 ) ;
#if 1
  comm = *comm0 ; me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;
  if ( np_y > 1 ) {
    yp = me + np_x ; ym = me - np_x ;
    if ( yp >= 0 && yp < np ) {
      ierr=MPI_Irecv ( buffer_for_proc( yp, yp_curs, RSL_RECVBUF ), yp_curs, MPI_CHAR, yp, me, comm, &yp_recv ) ;
    }
    if ( ym >= 0 && ym < np ) {
      ierr=MPI_Irecv ( buffer_for_proc( ym, ym_curs, RSL_RECVBUF ), ym_curs, MPI_CHAR, ym, me, comm, &ym_recv ) ;
    }
    if ( yp >= 0 && yp < np ) {
      ierr=MPI_Isend ( buffer_for_proc( yp, 0,       RSL_SENDBUF ), yp_curs, MPI_CHAR, yp, yp, comm, &yp_send ) ;
    }
    if ( ym >= 0 && ym < np ) {
      ierr=MPI_Isend ( buffer_for_proc( ym, 0,       RSL_SENDBUF ), ym_curs, MPI_CHAR, ym, ym, comm, &ym_send ) ;
    }
    if ( yp >= 0 && yp < np ) MPI_Wait( &yp_recv, &stat ) ; 
    if ( ym >= 0 && ym < np ) MPI_Wait( &ym_recv, &stat ) ; 
    if ( yp >= 0 && yp < np ) MPI_Wait( &yp_send, &stat ) ; 
    if ( ym >= 0 && ym < np ) MPI_Wait( &ym_send, &stat ) ;
  }
  yp_curs = 0 ; ym_curs = 0 ; xp_curs = 0 ; xm_curs = 0 ;
#else 
fprintf(stderr,"RSL_LITE_EXCH_Y disabled\n") ;
#endif
}
#endif

RSL_LITE_EXCH_PERIOD_X ( int * Fcomm0, int *me0, int * np0 , int * np_x0 , int * np_y0 )
{
  int me, np, np_x, np_y ;
  int yp, ym, xp, xm, nbytes ;
  MPI_Status stat ;
  MPI_Comm comm, *comm0, dummy_comm ;

  comm0 = &dummy_comm ;
  *comm0 = MPI_Comm_f2c( *Fcomm0 ) ;
#if 1

/* if ( me % np_x == np_x - 1 ) { */

  comm = *comm0 ; me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;
  if ( np_x > 1 ) {
    if ( me % np_x == np_x - 1 ) {
      nbytes = buffer_size_for_proc( me - np_x + 1, RSL_RECVBUF ) ;
      MPI_Irecv ( buffer_for_proc( me - np_x + 1, xp_curs, RSL_RECVBUF ), nbytes, MPI_CHAR, me - np_x + 1, me, comm, &xp_recv ) ;
    }
    if ( me % np_x == 0 ) {
      nbytes = buffer_size_for_proc( me + np_x - 1, RSL_RECVBUF ) ;
      MPI_Irecv ( buffer_for_proc( me + np_x - 1, xm_curs, RSL_RECVBUF ), nbytes, MPI_CHAR, me + np_x - 1, me, comm, &xm_recv ) ;
    }
    if ( me % np_x == np_x - 1 ) {
      MPI_Isend ( buffer_for_proc( me - np_x + 1, 0,       RSL_SENDBUF ), xp_curs, MPI_CHAR, me - np_x + 1, me - np_x + 1, comm, &xp_send ) ;
    }
    if ( me % np_x == 0 ) {
      MPI_Isend ( buffer_for_proc( me + np_x - 1, 0,       RSL_SENDBUF ), xm_curs, MPI_CHAR, me + np_x - 1, me + np_x - 1, comm, &xm_send ) ;
    }
    if ( me % np_x == np_x - 1 ) MPI_Wait( &xp_recv, &stat ) ; 
    if ( me % np_x == 0        ) MPI_Wait( &xm_recv, &stat ) ; 
    if ( me % np_x == np_x - 1 ) MPI_Wait( &xp_send, &stat ) ; 
    if ( me % np_x == 0        ) MPI_Wait( &xm_send, &stat ) ;
  }
#else 
fprintf(stderr,"RSL_LITE_EXCH_X disabled\n") ;
#endif
  yp_curs = 0 ; ym_curs = 0 ; xp_curs = 0 ; xm_curs = 0 ;
}

