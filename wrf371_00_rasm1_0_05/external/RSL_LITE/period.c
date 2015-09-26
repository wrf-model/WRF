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

static int yp_curs, ym_curs, xp_curs, xm_curs ;

RSL_LITE_INIT_PERIOD ( 
                int * Fcomm0,
                int * shw0,
                int * n3dR0, int *n2dR0, int * typesizeR0 , 
                int * n3dI0, int *n2dI0, int * typesizeI0 , 
                int * n3dD0, int *n2dD0, int * typesizeD0 , 
                int * n3dL0, int *n2dL0, int * typesizeL0 , 
                int * me0, int * np0 , int * np_x0 , int * np_y0 ,
                int * ips0 , int * ipe0 , int * jps0 , int * jpe0 , int * kps0 , int * kpe0 )
{
#ifndef STUBMPI
  int n3dR, n2dR, typesizeR ;
  int n3dI, n2dI, typesizeI ;
  int n3dD, n2dD, typesizeD ;
  int n3dL, n2dL, typesizeL ;
  int shw ;
  int me, np, np_x, np_y ;
  int ips , ipe , jps , jpe , kps , kpe ;
  int yp, ym, xp, xm ;
  int nbytes ;
  int coords[2] ;
  MPI_Comm comm, *comm0, dummy_comm ;

  comm0 = &dummy_comm ;
  *comm0 = MPI_Comm_f2c( *Fcomm0 ) ;

  shw = *shw0 ;
  n3dR = *n3dR0 ; n2dR = *n2dR0 ; typesizeR = *typesizeR0 ;
  n3dI = *n3dI0 ; n2dI = *n2dI0 ; typesizeI = *typesizeI0 ;
  n3dD = *n3dD0 ; n2dD = *n2dD0 ; typesizeD = *typesizeD0 ;
  n3dL = *n3dL0 ; n2dL = *n2dL0 ; typesizeL = *typesizeL0 ;
  me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;
  ips = *ips0-1 ; ipe = *ipe0-1 ; jps = *jps0-1 ; jpe = *jpe0-1 ; kps = *kps0-1 ; kpe = *kpe0-1 ;

/*
 This assumes that the topoology associated with the communicator is periodic
 the period routines should be called with "local_communicator_periodic", which
 is set up in module_dm.F for RSL_LITE.  Registry generated code automatically
 does this (gen_comms.c for RSL_LITE).
*/
  if ( np_y > 1 ) {
    nbytes = typesizeR*(ipe-ips+1+2*shw)*(shw+1)*(n3dR*(kpe-kps+1)+n2dR) +
             typesizeI*(ipe-ips+1+2*shw)*(shw+1)*(n3dI*(kpe-kps+1)+n2dI) +
             typesizeD*(ipe-ips+1+2*shw)*(shw+1)*(n3dD*(kpe-kps+1)+n2dD) +
             typesizeL*(ipe-ips+1+2*shw)*(shw+1)*(n3dL*(kpe-kps+1)+n2dL) ;
    MPI_Comm_rank( *comm0, &me ) ;
    MPI_Cart_coords( *comm0, me, 2, coords ) ;
    MPI_Cart_shift( *comm0, 0, 1, &ym, &yp ) ;
    if ( yp != MPI_PROC_NULL && coords[0] == np_y - 1 ) {  /* process on top of mesh */
       buffer_for_proc ( yp , nbytes, RSL_RECVBUF ) ;
       buffer_for_proc ( yp , nbytes, RSL_SENDBUF ) ;
    }
    if ( ym != MPI_PROC_NULL && coords[0] == 0 ) {         /* process on bottom of mesh */
       buffer_for_proc ( ym , nbytes, RSL_RECVBUF ) ;
       buffer_for_proc ( ym , nbytes, RSL_SENDBUF ) ;
    }
  }
  if ( np_x > 1 ) {
    nbytes = typesizeR*(jpe-jps+1+2*shw)*(shw+1)*(n3dR*(kpe-kps+1)+n2dR) +
             typesizeI*(jpe-jps+1+2*shw)*(shw+1)*(n3dI*(kpe-kps+1)+n2dI) +
             typesizeD*(jpe-jps+1+2*shw)*(shw+1)*(n3dD*(kpe-kps+1)+n2dD) +
             typesizeL*(jpe-jps+1+2*shw)*(shw+1)*(n3dL*(kpe-kps+1)+n2dL) ;
    MPI_Comm_rank( *comm0, &me ) ;
    MPI_Cart_coords( *comm0, me, 2, coords ) ;
    MPI_Cart_shift( *comm0, 1, 1, &xm, &xp ) ;
    if ( xm != MPI_PROC_NULL && coords[1] == np_x - 1 ) { /* process on right hand side of mesh */
       buffer_for_proc ( xp , nbytes, RSL_RECVBUF ) ;
       buffer_for_proc ( xp , nbytes, RSL_SENDBUF ) ;
    }
    if ( xp != MPI_PROC_NULL && coords[1] == 0 ) {        /* process on left hand side of mesh */
       buffer_for_proc ( xm,  nbytes, RSL_RECVBUF ) ;
       buffer_for_proc ( xm , nbytes, RSL_SENDBUF ) ;
    }
  }
  yp_curs = 0 ; ym_curs = 0 ; xp_curs = 0 ; xm_curs = 0 ;
#endif
}


RSL_LITE_PACK_PERIOD ( int* Fcomm0, char * buf , int * shw0 , int * typesize0 , int * xy0 , int * pu0 , int * imemord , int * stag0 ,
           int *me0, int * np0 , int * np_x0 , int * np_y0 , 
           int * ids0 , int * ide0 , int * jds0 , int * jde0 , int * kds0 , int * kde0 ,
           int * ims0 , int * ime0 , int * jms0 , int * jme0 , int * kms0 , int * kme0 ,
           int * ips0 , int * ipe0 , int * jps0 , int * jpe0 , int * kps0 , int * kpe0 )
{
#ifndef STUBMPI
  int me, np, np_x, np_y ;
  int shw , typesize ;
  int ids , ide , jds , jde , kds , kde ;
  int ims , ime , jms , jme , kms , kme ;
  int ips , ipe , jps , jpe , kps , kpe ;
  int stag ;  /* 0 not stag, 1 stag */
  int xy ;   /* y = 0 , x = 1 */
  int pu ;   /* pack = 0 , unpack = 1 */
  register int i, j, k, t ;
#ifdef crayx1
  register int i2,i3,i4,i_offset;
#endif
  char *p ;
  int the_buf ;
  int yp, ym, xp, xm ;
  int nbytes, ierr ;
  register int *pi, *qi ;
  int coords[2] ;
  int js, je, ks, ke, is, ie, wcount ;
  MPI_Comm comm, *comm0, dummy_comm ;

  comm0 = &dummy_comm ;
  *comm0 = MPI_Comm_f2c( *Fcomm0 ) ;

  me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;
  stag = *stag0 ;
  shw = *shw0 ; typesize = *typesize0 ;
  ids = *ids0-1 ; ide = *ide0-1 ; jds = *jds0-1 ; jde = *jde0-1 ; kds = *kds0-1 ; kde = *kde0-1 ;
  ims = *ims0-1 ; ime = *ime0-1 ; jms = *jms0-1 ; jme = *jme0-1 ; kms = *kms0-1 ; kme = *kme0-1 ;
  ips = *ips0-1 ; ipe = *ipe0-1 ; jps = *jps0-1 ; jpe = *jpe0-1 ; kps = *kps0-1 ; kpe = *kpe0-1 ;
  xy = *xy0 ;
  pu = *pu0 ;

#define RANGE(S1,E1,S2,E2,S3,E3,S4,E4) (((E1)-(S1)+1)*((E2)-(S2)+1)*((E3)-(S3)+1)*((E4)-(S4)+1))
#if 0
#define IMAX(A) (((A)>ids)?(A):ids)
#define IMIN(A) (((A)<ide)?(A):ide)
#define JMAX(A) (((A)>jds)?(A):jds)
#define JMIN(A) (((A)<jde)?(A):jde)
#else
/* allow the extent in other dimension to go into boundary region (e.g. < ids or > ide) since
   this will handle corner points for doubly periodic updates (he wrote hopefully) */
#define IMAX(A) (A)
#define IMIN(A) (A)
#define JMAX(A) (A)
#define JMIN(A) (A)
#endif

  the_buf = ( pu == 0 ) ? RSL_SENDBUF : RSL_RECVBUF ;

  if ( np_x > 1 && xy == 1 ) {   /* exchange period in x dim */
    MPI_Comm_rank( *comm0, &me ) ;
    MPI_Cart_coords( *comm0, me, 2, coords ) ;
    MPI_Cart_shift( *comm0, 1, 1, &xm, &xp ) ;
    if ( coords[1] == np_x - 1 ) {                /* process on right hand edge of domain */
      p = buffer_for_proc( xp , 0 , the_buf ) ;
      if ( pu == 0 ) {
        js = JMAX(jps-shw) ; je = JMIN(jpe+shw) ;
        ks = kps           ; ke = kpe ;
        is = ipe-shw       ; ie = ipe-1         ;
        nbytes = buffer_size_for_proc( xp , the_buf ) ;
        if ( xp_curs + RANGE( JMAX(jps-shw), JMIN(jpe+shw), kps, kpe, ipe-shw, ipe-1, 1, typesize ) > nbytes ) {
#ifndef MS_SUA
	  fprintf(stderr,"memory overwrite in rsl_lite_pack_period_x, right hand X to %d, %d > %d\n",xp,
	      xp_curs + RANGE( JMAX(jps-shw), JMIN(jpe+shw), kps, kpe, ipe-shw, ipe-1, 1, typesize ), nbytes ) ;
#endif
	  MPI_Abort(MPI_COMM_WORLD, 98) ;
        }
        if ( typesize == 8 ) {
          F_PACK_LINT ( buf, p+xp_curs, imemord, &js, &je, &ks, &ke, &is, &ie,
                                        &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
          xp_curs += wcount*typesize ;
        } else
	if ( typesize == 4 ) {
          F_PACK_INT ( buf, p+xp_curs, imemord, &js, &je, &ks, &ke, &is, &ie,
                                       &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
          xp_curs += wcount*typesize ;
	}
	else {
#ifndef MS_SUA
          fprintf(stderr,"internal error: %s %d\n",__FILE__,__LINE__) ;
#endif
	}
      } else {
        js = JMAX(jps-shw) ; je = JMIN(jpe+shw) ;
        ks = kps           ; ke = kpe ;
        is = ipe           ; ie = ipe+shw-1+stag ;
        if ( typesize == 8 ) {
          F_UNPACK_LINT ( p+xp_curs, buf, imemord, &js, &je, &ks, &ke, &is, &ie,
                                          &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
          xp_curs += wcount*typesize ;
        } else
	if ( typesize == 4 ) {
          F_UNPACK_INT ( p+xp_curs, buf, imemord, &js, &je, &ks, &ke, &is, &ie,
                                         &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
          xp_curs += wcount*typesize ;
	}
	else {
#ifndef MS_SUA
          fprintf(stderr,"internal error: %s %d\n",__FILE__,__LINE__) ;
#endif
        }
      }
    }
    if ( coords[1] == 0 ) {         /* process on left hand edge of domain */
      p = buffer_for_proc( xm , 0 , the_buf ) ;
      if ( pu == 0 ) {
        js = JMAX(jps-shw) ; je = JMIN(jpe+shw) ;
        ks = kps           ; ke = kpe ;
        is = ips           ; ie = ips+shw-1+stag ;
        nbytes = buffer_size_for_proc( xm , the_buf ) ;
        if ( xm_curs + RANGE( JMAX(jps-shw), JMIN(jpe+shw), kps, kpe, ips, ips+shw-1+stag, 1, typesize ) > nbytes ) {
#ifndef MS_SUA
	  fprintf(stderr,"memory overwrite in rsl_lite_pack_period_x,  left hand X to %d , %d > %d\n",xm,
	      xm_curs + RANGE( JMAX(jps-shw), JMIN(jpe+shw), kps, kpe, ips, ips+shw-1+stag, 1, typesize ), nbytes ) ;
#endif
	  MPI_Abort(MPI_COMM_WORLD, 98) ;
        }
        if ( typesize == 8 ) {
          F_PACK_LINT ( buf, p+xm_curs, imemord, &js, &je, &ks, &ke, &is, &ie,
                                        &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
          xm_curs += wcount*typesize ;
        } else
	if ( typesize == 4 ) {
          F_PACK_INT ( buf, p+xm_curs, imemord, &js, &je, &ks, &ke, &is, &ie,
                                       &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
          xm_curs += wcount*typesize ;
	}
	else {
#ifndef MS_SUA
          fprintf(stderr,"internal error: %s %d\n",__FILE__,__LINE__) ;
#endif
        }
      } else {
        js = JMAX(jps-shw) ; je = JMIN(jpe+shw) ;
        ks = kps           ; ke = kpe ;
        is = ips-shw       ; ie = ips-1           ;
        if ( typesize == 8 ) {
          F_UNPACK_LINT ( p+xm_curs, buf, imemord, &js, &je, &ks, &ke, &is, &ie,
                                          &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
          xm_curs += wcount*typesize ;
        } else
	if ( typesize == 4 ) {
          F_UNPACK_INT ( p+xm_curs, buf, imemord, &js, &je, &ks, &ke, &is, &ie,
                                         &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
          xm_curs += wcount*typesize ;
	}
	else {
#ifndef MS_SUA
          fprintf(stderr,"internal error: %s %d\n",__FILE__,__LINE__) ;
#endif
        }
      }
    }
  }
  if ( np_y > 1 && xy == 0 ) {    /* exchange period in Y dim */
    MPI_Comm_rank( *comm0, &me ) ;
    MPI_Cart_coords( *comm0, me, 2, coords ) ;
    MPI_Cart_shift( *comm0, 0, 1, &ym, &yp ) ;
    if ( coords[0] == np_y - 1 ) {                /* process on top edge of domain */
      p = buffer_for_proc( yp , 0 , the_buf ) ;
      if ( pu == 0 ) {
        is = IMAX(ips-shw) ; ie = IMIN(ipe+shw) ;
        ks = kps           ; ke = kpe ;
        js = jpe-shw       ; je = jpe-1         ;
        nbytes = buffer_size_for_proc( yp , the_buf ) ;
        if ( yp_curs + RANGE( IMAX(ips-shw), IMIN(ipe+shw), kps, kpe, jpe-shw, jpe-1, 1, typesize ) > nbytes ) {
#ifndef MS_SUA
	  fprintf(stderr,"memory overwrite in rsl_lite_pack_period_y, right hand Y to %d, %d > %d\n",yp,
	      yp_curs + RANGE( IMAX(ips-shw), IMIN(ipe+shw), kps, kpe, jpe-shw, jpe-1, 1, typesize ), nbytes ) ;
#endif
	  MPI_Abort(MPI_COMM_WORLD, 98) ;
        }
        if ( typesize == 8 ) {
          F_PACK_LINT ( buf, p+yp_curs, imemord, &js, &je, &ks, &ke, &is, &ie,
                                        &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
          yp_curs += wcount*typesize ;
        } else
	if ( typesize == 4 ) {
          F_PACK_INT ( buf, p+yp_curs, imemord, &js, &je, &ks, &ke, &is, &ie,
                                       &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
          yp_curs += wcount*typesize ;
	}
	else {
#ifndef MS_SUA
          fprintf(stderr,"internal error: %s %d\n",__FILE__,__LINE__) ;
#endif
	}
      } else {
        is = IMAX(ips-shw) ; ie = IMIN(ipe+shw) ;
        ks = kps           ; ke = kpe ;
        js = jpe           ; je = jpe+shw-1+stag ;
        if ( typesize == 8 ) {
          F_UNPACK_LINT ( p+yp_curs, buf, imemord, &js, &je, &ks, &ke, &is, &ie,
                                          &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
          yp_curs += wcount*typesize ;
        } else
	if ( typesize == 4 ) {
          F_UNPACK_INT ( p+yp_curs, buf, imemord, &js, &je, &ks, &ke, &is, &ie,
                                         &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
          yp_curs += wcount*typesize ;
	}
	else {
#ifndef MS_SUA
          fprintf(stderr,"internal error: %s %d\n",__FILE__,__LINE__) ;
#endif
        }
      }
    }
    if ( coords[0] == 0 ) {         /* process on bottom edge of domain */
      p = buffer_for_proc( ym , 0 , the_buf ) ;
      if ( pu == 0 ) {
        is = IMAX(ips-shw) ; ie = IMIN(ipe+shw) ;
        ks = kps           ; ke = kpe ;
        js = jps           ; je = jps+shw-1+stag ;
        nbytes = buffer_size_for_proc( ym , the_buf ) ;
        if ( ym_curs + RANGE( IMAX(ips-shw), IMIN(ipe+shw), kps, kpe, jps, jps+shw-1+stag, 1, typesize ) > nbytes ) {
#ifndef MS_SUA
	  fprintf(stderr,"memory overwrite in rsl_lite_pack_period_y,  left hand Y to %d , %d > %d\n",xm,
	      ym_curs + RANGE( IMAX(ips-shw), IMIN(ipe+shw), kps, kpe, jps, jps+shw-1+stag, 1, typesize ), nbytes ) ;
#endif
	  MPI_Abort(MPI_COMM_WORLD, 98) ;
        }
        if ( typesize == 8 ) {
          F_PACK_LINT ( buf, p+ym_curs, imemord, &js, &je, &ks, &ke, &is, &ie,
                                        &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
          ym_curs += wcount*typesize ;
        } else
	if ( typesize == 4 ) {
          F_PACK_INT ( buf, p+ym_curs, imemord, &js, &je, &ks, &ke, &is, &ie,
                                       &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
          ym_curs += wcount*typesize ;
	}
	else {
#ifndef MS_SUA
          fprintf(stderr,"internal error: %s %d\n",__FILE__,__LINE__) ;
#endif
        }
      } else {
        is = IMAX(ips-shw) ; ie = IMIN(ipe+shw) ;
        ks = kps           ; ke = kpe ;
        js = jps-shw       ; je = jps-1           ;
        if ( typesize == 8 ) {
          F_UNPACK_LINT ( p+ym_curs, buf, imemord, &js, &je, &ks, &ke, &is, &ie,
                                          &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
          ym_curs += wcount*typesize ;
        } else
	if ( typesize == 4 ) {
          F_UNPACK_INT ( p+ym_curs, buf, imemord, &js, &je, &ks, &ke, &is, &ie,
                                         &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
          ym_curs += wcount*typesize ;
	}
	else {
#ifndef MS_SUA
          fprintf(stderr,"internal error: %s %d\n",__FILE__,__LINE__) ;
#endif
        }
      }
    }
  }
#endif
}

#ifndef STUBMPI
static MPI_Request yp_recv, ym_recv, yp_send, ym_send ;
static MPI_Request xp_recv, xm_recv, xp_send, xm_send ;
#endif

RSL_LITE_EXCH_PERIOD_X ( int * Fcomm0, int *me0, int * np0 , int * np_x0 , int * np_y0 )
{
#ifndef STUBMPI
  int me, np, np_x, np_y ;
  int yp, ym, xp, xm, nbytes ;
  MPI_Status stat ;
  MPI_Comm comm, *comm0, dummy_comm ;
  int coords[2] ;

  comm0 = &dummy_comm ;
  *comm0 = MPI_Comm_f2c( *Fcomm0 ) ;
#if 1
  comm = *comm0 ; me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;

  if ( np_x > 1 ) {
    MPI_Comm_rank( *comm0, &me ) ;
    MPI_Cart_coords( *comm0, me, 2, coords ) ;
    MPI_Cart_shift( *comm0, 1, 1, &xm, &xp ) ;
    if ( coords[1] == np_x - 1 ) {   /* proc on right hand side of domain */
      nbytes = buffer_size_for_proc( xp, RSL_RECVBUF ) ;
      MPI_Irecv ( buffer_for_proc( xp , xp_curs, RSL_RECVBUF ), nbytes, MPI_CHAR, xp, me, comm, &xp_recv ) ;
    }
    if ( coords[1] == 0 ) {          /* proc on left hand side of domain */
      nbytes = buffer_size_for_proc( xm, RSL_RECVBUF ) ;
      MPI_Irecv ( buffer_for_proc( xm, xm_curs, RSL_RECVBUF ), nbytes, MPI_CHAR, xm, me, comm, &xm_recv ) ;
    }
    if ( coords[1] == np_x - 1 ) {   /* proc on right hand side of domain */
      MPI_Isend ( buffer_for_proc( xp , 0,       RSL_SENDBUF ), xp_curs, MPI_CHAR, xp, xp, comm, &xp_send ) ;
    }
    if ( coords[1] == 0 ) {          /* proc on left hand side of domain */
      MPI_Isend ( buffer_for_proc( xm, 0,       RSL_SENDBUF ), xm_curs, MPI_CHAR, xm, xm, comm, &xm_send ) ;
    }
    if ( coords[1] == np_x - 1 ) MPI_Wait( &xp_recv, &stat ) ; 
    if ( coords[1] == 0        ) MPI_Wait( &xm_recv, &stat ) ; 
    if ( coords[1] == np_x - 1 ) MPI_Wait( &xp_send, &stat ) ; 
    if ( coords[1] == 0        ) MPI_Wait( &xm_send, &stat ) ;
  }
#else 
# ifndef MS_SUA
fprintf(stderr,"RSL_LITE_EXCH_PERIOD_X disabled\n") ;
# endif
#endif
  yp_curs = 0 ; ym_curs = 0 ; xp_curs = 0 ; xm_curs = 0 ;
#endif
}

RSL_LITE_EXCH_PERIOD_Y ( int * Fcomm0, int *me0, int * np0 , int * np_x0 , int * np_y0 )
{
#ifndef STUBMPI
  int me, np, np_x, np_y ;
  int yp, ym, xp, xm, nbytes ;
  MPI_Status stat ;
  MPI_Comm comm, *comm0, dummy_comm ;
  int coords[2] ;

  comm0 = &dummy_comm ;
  *comm0 = MPI_Comm_f2c( *Fcomm0 ) ;
#if 1
  comm = *comm0 ; me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;

  if ( np_y > 1 ) {
    MPI_Comm_rank( *comm0, &me ) ;
    MPI_Cart_coords( *comm0, me, 2, coords ) ;
    MPI_Cart_shift( *comm0, 0, 1, &ym, &yp ) ;
    if ( coords[0] == np_y - 1 ) {   /* proc on top of domain */
      nbytes = buffer_size_for_proc( yp, RSL_RECVBUF ) ;
      MPI_Irecv ( buffer_for_proc( yp , yp_curs, RSL_RECVBUF ), nbytes, MPI_CHAR, yp, me, comm, &yp_recv ) ;
    }
    if ( coords[0] == 0 ) {          /* proc on bottom of domain */
      nbytes = buffer_size_for_proc( ym, RSL_RECVBUF ) ;
      MPI_Irecv ( buffer_for_proc( ym, ym_curs, RSL_RECVBUF ), nbytes, MPI_CHAR, ym, me, comm, &ym_recv ) ;
    }
    if ( coords[0] == np_y - 1 ) {   /* proc on top of domain */
      MPI_Isend ( buffer_for_proc( yp , 0,       RSL_SENDBUF ), yp_curs, MPI_CHAR, yp, yp, comm, &yp_send ) ;
    }
    if ( coords[0] == 0 ) {          /* proc on bottom of domain */
      MPI_Isend ( buffer_for_proc( ym, 0,       RSL_SENDBUF ), ym_curs, MPI_CHAR, ym, ym, comm, &ym_send ) ;
    }
    if ( coords[0] == np_y - 1 ) MPI_Wait( &yp_recv, &stat ) ;
    if ( coords[0] == 0        ) MPI_Wait( &ym_recv, &stat ) ;
    if ( coords[0] == np_y - 1 ) MPI_Wait( &yp_send, &stat ) ;
    if ( coords[0] == 0        ) MPI_Wait( &ym_send, &stat ) ;
  }
#else
# ifndef MS_SUA
fprintf(stderr,"RSL_LITE_EXCH_PERIOD_Y disabled\n") ;
# endif
#endif
  yp_curs = 0 ; ym_curs = 0 ; xp_curs = 0 ; xm_curs = 0 ;
#endif
}

