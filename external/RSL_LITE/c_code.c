#include <stdio.h>
#include <fcntl.h>

#define STANDARD_ERROR 2

#define STANDARD_OUTPUT 1

#include "mpi.h"
#include "rsl_lite.h"


RSL_LITE_ERROR_DUP1 ( int *me )
{
    int newfd ;
    char filename[256] ;

/* redirect standard out*/
    sprintf(filename,"rsl.out.%04d",*me) ;
    if ((newfd = open( filename, O_CREAT | O_WRONLY, 0666 )) < 0 )
    {
        perror("error_dup: cannot open rsl.out.nnnn") ;
        fprintf(stderr,"...sending output to standard output and continuing.\n") ;
        return ;
    }
    if( dup2( newfd, STANDARD_OUTPUT ) < 0 )
    {
        perror("error_dup: dup2 fails to change output descriptor") ;
        fprintf(stderr,"...sending output to standard output and continuing.\n") ;
        close(newfd) ;
        return ;
    }

/* redirect standard error */
    sprintf(filename,"rsl.error.%04d",*me) ;
    if ((newfd = open( filename, O_CREAT | O_WRONLY, 0666 )) < 0 )
    {
        perror("error_dup: cannot open rsl.error.log") ;
        fprintf(stderr,"...sending error to standard error and continuing.\n") ;
        return ;
    }
    if( dup2( newfd, STANDARD_ERROR ) < 0 )
    {
        perror("error_dup: dup2 fails to change error descriptor") ;
        fprintf(stderr,"...sending error to standard error and continuing.\n") ;
        close(newfd) ;
        return ;
    }

}

BYTE_BCAST ( char * buf, int * size, int * Fcomm )
{
    MPI_Comm *comm, dummy_comm ;

    comm = &dummy_comm ;
    *comm = MPI_Comm_f2c( *Fcomm ) ;
#ifdef crayx1
    if (*size % sizeof(int) == 0) {
       MPI_Bcast ( buf, *size/sizeof(int), MPI_INT, 0, *comm ) ;
    } else {
       MPI_Bcast ( buf, *size, MPI_BYTE, 0, *comm ) ;
    }
#else
    MPI_Bcast ( buf, *size, MPI_BYTE, 0, *comm ) ;
#endif
}

int yp_curs, ym_curs, xp_curs, xm_curs ;

RSL_LITE_INIT_EXCH ( 
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

#if 1

  if ( np_y > 1 ) {
    nbytes = typesizeR*(ipe-ips+1+2*shw)*shw*(n3dR*(kpe-kps+1)+n2dR) +
             typesizeI*(ipe-ips+1+2*shw)*shw*(n3dI*(kpe-kps+1)+n2dI) +
             typesizeD*(ipe-ips+1+2*shw)*shw*(n3dD*(kpe-kps+1)+n2dD) +
             typesizeL*(ipe-ips+1+2*shw)*shw*(n3dL*(kpe-kps+1)+n2dL) ;
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
  if ( np_x > 1 ) {
    nbytes = typesizeR*(jpe-jps+1+2*shw)*shw*(n3dR*(kpe-kps+1)+n2dR) +
             typesizeI*(jpe-jps+1+2*shw)*shw*(n3dI*(kpe-kps+1)+n2dI) +
             typesizeD*(jpe-jps+1+2*shw)*shw*(n3dD*(kpe-kps+1)+n2dD) +
             typesizeL*(jpe-jps+1+2*shw)*shw*(n3dL*(kpe-kps+1)+n2dL) ;
    xp = me + 1 ; xm = me - 1 ;
    if ( xp % np_x > me % np_x && xp < np ) {
       buffer_for_proc ( xp , nbytes, RSL_RECVBUF ) ;
       buffer_for_proc ( xp , nbytes, RSL_SENDBUF ) ;
    }
    if ( xm % np_x < me % np_x && xm >= 0 ) {
       buffer_for_proc ( xm , nbytes, RSL_RECVBUF ) ;
       buffer_for_proc ( xm , nbytes, RSL_SENDBUF ) ;
    }
  }
#endif
  yp_curs = 0 ; ym_curs = 0 ; xp_curs = 0 ; xm_curs = 0 ;
}

RSL_LITE_PACK ( char * buf , int * shw0 , int * typesize0 , int * xy0 , int * pu0 , char * memord ,
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

  me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;
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

  if ( np_y > 1 && xy == 0 ) {
    yp = me + np_x ; ym = me - np_x ;
    if ( yp >= 0 && yp < np ) {
      p = buffer_for_proc( yp , 0 , da_buf ) ;
      if ( pu == 0 ) {
        nbytes = buffer_size_for_proc( yp, da_buf ) ;
	if ( yp_curs + RANGE( jpe-shw+1, jpe, kps, kpe, ips-shw, ipe+shw, 1, typesize ) > nbytes ) {
	  fprintf(stderr,"memory overwrite in rsl_lite_pack, Y pack up, %d > %d\n",
	      yp_curs + RANGE( jpe-shw+1, jpe, kps, kpe, ips-shw, ipe+shw, 1, typesize ), nbytes ) ;
	  MPI_Abort(MPI_COMM_WORLD, ierr) ;
        }
	if ( typesize == sizeof(int) ) {
#ifdef crayx1
/* PETER -- CHECK THIS AND UPDATE TO USE IMAX AND IMIN MACROS */
        i4 = IMAX(ips-shw) ;
          i_offset = ipe+shw+1-i4;
          i3=0;
          for ( j = jpe-shw+1 ; j <= jpe ; j++ ) {
          pi = (int *)(p+yp_curs) ;
#pragma concurrent
#pragma preferstream
            for ( k = kps ; k <= kpe ; k++ ) {
              i3 = i_offset*k;
            qi = (int *)((buf + typesize*( (i4-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
#pragma concurrent
#pragma prefervector
              for ( i = i4, i2=0; i <= ipe+shw ; i++ ) {
                pi[i3++] = qi[i2++];
            }
          }
            yp_curs += (i_offset*(kpe+1))*typesize;
        }
#else
          for ( j = jpe-shw+1 ; j <= jpe ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              pi = (int *)(p+yp_curs) ;
              i = IMAX(ips-shw) ;
              qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
              for ( i = IMAX(ips-shw) ; i <= IMIN(ipe+shw) ; i++ ) {
                *pi++ = *qi++ ;
              }
              yp_curs += (i-(ips-shw))*typesize ;
            }
          }
#endif
	}
	else {
          for ( j = jpe-shw+1 ; j <= jpe ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              for ( i = IMAX(ips-shw) ; i <= IMIN(ipe+shw) ; i++ ) {
                for ( t = 0 ; t < typesize ; t++ ) {
                  *(p+yp_curs) = 
                                 *(buf + t + typesize*(
                                        (i-ims) + (ime-ims+1)*(
                                        (k-kms) + (j-jms)*(kme-kms+1))) ) ;
                  yp_curs++ ;
                }
              }
            }
          }
        }
      } else {
	if ( typesize == sizeof(int) ) {
#ifdef crayx1
/* PETER -- CHECK THIS AND UPDATE TO USE IMAX AND IMIN MACROS */
        i4 = IMAX(ips-shw) ;
          i_offset = ipe+shw+1-i4;
          for ( j = jpe+1 ; j <= jpe+shw ; j++ ) {
          pi = (int *)(p+yp_curs) ;
#pragma concurrent
#pragma perferstream
            for ( k = kps ; k <= kpe ; k++ ) {
              i3 = i_offset*k;
            qi = (int *)((buf + typesize*( (i4-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
#pragma concurrent
#pragma prefervector
              for ( i = i4, i2=0; i <= ipe+shw ; i++ ) {
                qi[i2++] = pi[i3++];
            }
          }
            yp_curs += (i_offset*(kpe+1))*typesize;
        }
#else
          for ( j = jpe+1 ; j <= jpe+shw ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              pi = (int *)(p+yp_curs) ;
              i = IMAX(ips-shw) ;
              qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
              for ( i = IMAX(ips-shw) ; i <= IMIN(ipe+shw) ; i++ ) {
                *qi++ = *pi++ ;
              }
              yp_curs += (i-(ips-shw))*typesize ;
            }
          }
#endif
	}
	else {
          for ( j = jpe+1 ; j <= jpe+shw ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              for ( i = IMAX(ips-shw) ; i <= IMIN(ipe+shw) ; i++ ) {
                for ( t = 0 ; t < typesize ; t++ ) {
                                 *(buf + t + typesize*(
                                        (i-ims) + (ime-ims+1)*(
                                        (k-kms) + (j-jms)*(kme-kms+1))) ) =
                  *(p+yp_curs) ;
                  yp_curs++ ;
                }
              }
            }
          }
	}
      }
    }
    if ( ym >= 0 && ym < np ) {
      p = buffer_for_proc( ym , 0 , da_buf ) ;
      if ( pu == 0 ) {
        nbytes = buffer_size_for_proc( ym, da_buf ) ;
	if ( ym_curs + RANGE( jps, jps+shw-1, kps, kpe, ips-shw, ipe+shw, 1, typesize ) > nbytes ) {
	  fprintf(stderr,"memory overwrite in rsl_lite_pack, Y pack dn, %d > %d\n",
	      ym_curs + RANGE( jps, jps+shw-1, kps, kpe, ips-shw, ipe+shw, 1, typesize ), nbytes ) ;
	  MPI_Abort(MPI_COMM_WORLD, ierr) ;
        }
	if ( typesize == sizeof(int) ) {
#ifdef crayx1
/* PETER -- CHECK THIS AND UPDATE TO USE IMAX AND IMIN MACROS */
        i4 = IMAX(ips-shw) ;
          i_offset = ipe+shw+1-i4;
          for ( j = jps ; j <= jps+shw-1 ; j++ ) {
          pi = (int *)(p+ym_curs) ;
#pragma concurrent
#pragma perferstream
            for ( k = kps ; k <= kpe ; k++ ) {
              i3 = i_offset*k;
            qi = (int *)((buf + typesize*( (i4-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
#pragma concurrent
#pragma perfervector
              for ( i = i4, i2=0 ; i <= ipe+shw ; i++) {
                pi[i3++] = qi[i2++];
            }
          }
            ym_curs += (i_offset*(kpe+1))*typesize;
        }
#else
          for ( j = jps ; j <= jps+shw-1 ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              pi = (int *)(p+ym_curs) ;
              i = IMAX(ips-shw) ;
              qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
              for ( i = IMAX(ips-shw) ; i <= IMIN(ipe+shw) ; i++ ) {
                *pi++ = *qi++ ;
              }
              ym_curs += (i-(ips-shw))*typesize ;
            }
          }
#endif
	}
	else {
          for ( j = jps ; j <= jps+shw-1 ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              for ( i = IMAX(ips-shw) ; i <= IMIN(ipe+shw) ; i++ ) {
                for ( t = 0 ; t < typesize ; t++ ) {
                  *(p+ym_curs) = 
                                 *(buf + t + typesize*(
                                        (i-ims) + (ime-ims+1)*(
                                        (k-kms) + (j-jms)*(kme-kms+1))) ) ;
                  ym_curs++ ;
                }
              }
            }
          }
	}
      } else {
	if ( typesize == sizeof(int) ) {
#ifdef crayx1
/* PETER -- CHECK THIS AND UPDATE TO USE IMAX AND IMIN MACROS */
        i4 = IMAX(ips-shw) ;
          i_offset = ipe+shw+1-i4;
          for ( j = jps-shw ; j <= jps-1 ; j++ ) {
          pi = (int *)(p+ym_curs) ;
#pragma concurrent
#pragma perferstream
            for ( k = kps ; k <= kpe ; k++ ) {
              i3 =i_offset*k;
            qi = (int *)((buf + typesize*( (i4-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
              i2=0;
#pragma concurrent
#pragma prefervector
              for ( i = i4; i <= ipe+shw ; i++ ) {
                qi[i2++] = pi[i3++];
            }
          }
          ym_curs += (i_offset*(kpe+1))*typesize;
        }
#else
         for ( j = jps-shw ; j <= jps-1 ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              pi = (int *)(p+ym_curs) ;
              i = IMAX(ips-shw) ;
              qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
              for ( i = IMAX(ips-shw) ; i <= IMIN(ipe+shw) ; i++ ) {
                *qi++ = *pi++ ;
              }
              ym_curs += (i-(ips-shw))*typesize ;
            }
          }
#endif
	}
	else {
          for ( j = jps-shw ; j <= jps-1 ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              for ( i = IMAX(ips-shw) ; i <= IMIN(ipe+shw) ; i++ ) {
                for ( t = 0 ; t < typesize ; t++ ) {
                                 *(buf + t + typesize*(
                                        (i-ims) + (ime-ims+1)*(
                                        (k-kms) + (j-jms)*(kme-kms+1))) ) =
                  *(p+ym_curs)  ;
                  ym_curs++ ;
                }
              }
            }
          }
        }
      }
    }
  }

  if ( np_x > 1 && xy == 1 ) {
    xp = me + 1 ; xm = me - 1 ;
    if ( xp % np_x > me % np_x && xp < np ) {
      p = buffer_for_proc( xp , 0 , da_buf ) ;
      if ( pu == 0 ) {
        nbytes = buffer_size_for_proc( xp, da_buf ) ;
        if ( xp_curs + RANGE( jps-shw, jpe+shw, kps, kpe, ipe-shw+1, ipe, 1, typesize ) > nbytes ) {
	  fprintf(stderr,"memory overwrite in rsl_lite_pack, X pack right, %d > %d\n",
	      xp_curs + RANGE( jps-shw, jpe+shw, kps, kpe, ipe-shw+1, ipe, 1, typesize ), nbytes ) ;
	  MPI_Abort(MPI_COMM_WORLD, ierr) ;
        }
	if ( typesize == sizeof(int) ) {
#ifdef crayx1
          pi = (int *)(p+xp_curs) ;
          i3=0;
#pragma concurrent
          for ( j = JMAX(jps-shw) ; j <= JMIN(jpe+shw) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              qi = (int *)((buf + typesize*( (ipe-shw+1-ims) + (ime-ims+1)*(
                                            (k-kms) + (j-jms)*(kme-kms+1))))) ;
              for ( i = ipe-shw+1, i2=0 ; i <= ipe ; i++ ) {
                pi[i3++] = qi[i2++];
              }
            }
          }
          xp_curs += i3*typesize;
#else
          for ( j = JMAX(jps-shw) ; j <= JMIN(jpe+shw) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
	      pi = (int *)(p+xp_curs) ;
	      i = ipe-shw+1 ;
	      qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
              for ( i = ipe-shw+1 ; i <= ipe ; i++ ) {
	        *pi++ = *qi++ ;
	      }
	      xp_curs += shw*typesize ;
	    }
	  }
#endif
	}
	else {
          for ( j = JMAX(jps-shw) ; j <= JMIN(jpe+shw) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              for ( i = ipe-shw+1 ; i <= ipe ; i++ ) {
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
#ifdef crayx1
          pi = (int *)(p+xp_curs) ;
          i3=0;
#pragma concurrent
          for ( j = JMAX(jps-shw) ; j <= JMIN(jpe+shw) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              qi = (int *)((buf + typesize*( (ipe-ims+1) + (ime-ims+1)*(
                                            (k-kms) + (j-jms)*(kme-kms+1))))) ;
              for ( i = ipe+1, i2=0 ; i <= ipe+shw ; i++ ) {
                qi[i2++] = pi[i3++];
              }
            }
          }
          xp_curs += i3*typesize;
#else
          for ( j = JMAX(jps-shw) ; j <= JMIN(jpe+shw) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
	      pi = (int *)(p+xp_curs) ;
	      i = ipe+1 ;
	      qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
              for ( i = ipe+1 ; i <= ipe+shw ; i++ ) {
	        *qi++ = *pi++ ;
	      }
	      xp_curs += shw*typesize ;
	    }
	  }
#endif
	}
	else {
          for ( j = JMAX(jps-shw) ; j <= JMIN(jpe+shw) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              for ( i = ipe+1 ; i <= ipe+shw ; i++ ) {
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
    if ( xm % np_x < me % np_x && xm >= 0 ) {
      p = buffer_for_proc( xm , 0 , da_buf ) ;
      if ( pu == 0 ) {
        nbytes = buffer_size_for_proc( xm, da_buf ) ;
        if ( xm_curs + RANGE( jps-shw, jpe+shw, kps, kpe, ips, ips+shw-1, 1, typesize ) > nbytes ) {
	  fprintf(stderr,"memory overwrite in rsl_lite_pack, X left , %d > %d\n",
	      xm_curs + RANGE( jps-shw, jpe+shw, kps, kpe, ips, ips+shw-1, 1, typesize ), nbytes ) ;
	  MPI_Abort(MPI_COMM_WORLD, ierr) ;
        }
	if ( typesize == sizeof(int) ) {
#ifdef crayx1
          pi = (int *)(p+xm_curs) ;
          i3=0;
#pragma concurrent
          for ( j = JMAX(jps-shw) ; j <= JMIN(jpe+shw) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              qi = (int *)((buf + typesize*( (ips-ims) + (ime-ims+1)*(
                                            (k-kms) + (j-jms)*(kme-kms+1))))) ;
              for ( i = ips, i2=0 ; i <= ips+shw-1 ; i++ ) {
                pi[i3++] = qi[i2++];
              }
            }
          }
          xm_curs += i3*typesize;
#else
          for ( j = JMAX(jps-shw) ; j <= JMIN(jpe+shw) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
	      pi = (int *)(p+xm_curs) ;
	      i = ips ;
	      qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
              for ( i = ips ; i <= ips+shw-1 ; i++ ) {
	        *pi++ = *qi++ ;
	      }
	      xm_curs += shw*typesize ;
	    }
	  }
#endif
	}
	else {
          for ( j = JMAX(jps-shw) ; j <= JMIN(jpe+shw) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              for ( i = ips ; i <= ips+shw-1 ; i++ ) {
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
#ifdef crayx1
          pi = (int *)(p+xm_curs) ;
          i3=0;
#pragma concurrent
          for ( j = JMAX(jps-shw) ; j <= JMIN(jpe+shw) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
              qi = (int *)((buf + typesize*( (ips-shw-ims) + (ime-ims+1)*(
                                            (k-kms) + (j-jms)*(kme-kms+1))))) ;
              for ( i = ips-shw, i2=0 ; i < ips ; i++ ) {
                qi[i2++] = pi[i3++];
              }
            }
          }
          xm_curs += i3*typesize;
#else
          for ( j = JMAX(jps-shw) ; j <= JMIN(jpe+shw) ; j++ ) {
            for ( k = kps ; k <= kpe ; k++ ) {
	      pi = (int *)(p+xm_curs) ;
	      i = ips-shw ;
	      qi = (int *)((buf + typesize*( (i-ims) + (ime-ims+1)*(
                                             (k-kms) + (j-jms)*(kme-kms+1))))) ;
              for ( i = ips-shw ; i < ips ; i++ ) {
	        *qi++ = *pi++ ;
	      }
	      xm_curs += shw*typesize ;
	    }
	  }
#endif
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

RSL_LITE_EXCH_Y ( int * Fcomm0, int *me0, int * np0 , int * np_x0 , int * np_y0 )
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

RSL_LITE_EXCH_X ( int * Fcomm0, int *me0, int * np0 , int * np_x0 , int * np_y0 )
{
  int me, np, np_x, np_y ;
  int yp, ym, xp, xm ;
  MPI_Status stat ;
  MPI_Comm comm, *comm0, dummy_comm ;

  comm0 = &dummy_comm ;
  *comm0 = MPI_Comm_f2c( *Fcomm0 ) ;
#if 1
  comm = *comm0 ; me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;
  if ( np_x > 1 ) {
    xp = me + 1 ; xm = me - 1 ;
    if ( xp % np_x > me % np_x && xp < np ) {
      MPI_Irecv ( buffer_for_proc( xp, xp_curs, RSL_RECVBUF ), xp_curs, MPI_CHAR, xp, me, comm, &xp_recv ) ;
    }
    if ( xm % np_x < me % np_x && xm >= 0 ) {
      MPI_Irecv ( buffer_for_proc( xm, xm_curs, RSL_RECVBUF ), xm_curs, MPI_CHAR, xm, me, comm, &xm_recv ) ;
    }
    if ( xp % np_x > me % np_x && xp < np ) {
      MPI_Isend ( buffer_for_proc( xp, 0,       RSL_SENDBUF ), xp_curs, MPI_CHAR, xp, xp, comm, &xp_send ) ;
    }
    if ( xm % np_x < me % np_x && xm >= 0 ) {
      MPI_Isend ( buffer_for_proc( xm, 0,       RSL_SENDBUF ), xm_curs, MPI_CHAR, xm, xm, comm, &xm_send ) ;
    }
    if ( xp % np_x > me % np_x && xp < np ) MPI_Wait( &xp_recv, &stat ) ; 
    if ( xm % np_x < me % np_x && xm >= 0 ) MPI_Wait( &xm_recv, &stat ) ; 
    if ( xp % np_x > me % np_x && xp < np ) MPI_Wait( &xp_send, &stat ) ; 
    if ( xm % np_x < me % np_x && xm >= 0 ) MPI_Wait( &xm_send, &stat ) ;
  }
#else 
fprintf(stderr,"RSL_LITE_EXCH_X disabled\n") ;
#endif
  yp_curs = 0 ; ym_curs = 0 ; xp_curs = 0 ; xm_curs = 0 ;
}

#include <sys/time.h>
RSL_INTERNAL_MILLICLOCK ()
{
    struct timeval tb ;
    struct timezone tzp ;
    int isec ;  /* seconds */
    int usec ;  /* microseconds */
    int msecs ;
    gettimeofday( &tb, &tzp ) ;
    isec = tb.tv_sec ;
    usec = tb.tv_usec ;
    msecs = 1000 * isec + usec / 1000 ;
    return(msecs) ;
}
RSL_INTERNAL_MICROCLOCK ()
{
    struct timeval tb ;
    struct timezone tzp ;
    int isec ;  /* seconds */
    int usec ;  /* microseconds */
    int msecs ;
    gettimeofday( &tb, &tzp ) ;
    isec = tb.tv_sec ;
    usec = tb.tv_usec ;
    msecs = 1000000 * isec + usec ;
    return(msecs) ;
}
