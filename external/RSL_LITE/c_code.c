#ifndef MS_SUA_
# include <stdio.h>
#endif
#include <fcntl.h>
#ifndef O_CREAT
# define O_CREAT _O_CREAT
#endif
#ifndef O_WRONLY
# define O_WRONLY _O_WRONLY
#endif
#ifndef O_TRUNC
# define O_TRUNC _O_TRUNC
#endif

#ifdef _WIN32
#include <Winsock2.h>
#endif
#ifdef NCEP_DEBUG_MULTIDIR
// #  include <errno.h>
#endif

#define STANDARD_ERROR 2

#define STANDARD_OUTPUT 1

#ifndef STUBMPI
#  include "mpi.h"
#endif
#include "rsl_lite.h"

#define F_PACK

void RSL_LITE_ERROR_DUP1 ( int *me )
{
    int newfd,rc ;
    char filename[256] ;
    char dirname[256] ;
    char hostname[256] ;

/* redirect standard out and standard error based on compile options*/
                                                                                                                                              
#ifndef NCEP_DEBUG_MULTIDIR
    gethostname( hostname, 256 ) ;

/* redirect standard out*/
# ifndef RSL0_ONLY
    sprintf(filename,"rsl.out.%04d",*me) ;
# else
    if (*me == 0)
     {
     sprintf(filename,"rsl.out.%04d",*me) ;
     }
    else
     {
     sprintf(filename,"/dev/null") ;
     }
# endif
    if ((newfd = open( filename, O_CREAT | O_WRONLY | O_TRUNC, 0666 )) < 0 )
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
# if defined( _WIN32 ) 
    if ( *me != 0 ) {   /* stderr from task 0 should come to screen on windows because it is buffered if redirected */
#endif
# ifndef RSL0_ONLY
    sprintf(filename,"rsl.error.%04d",*me) ;
# else
    if (*me == 0)
     {
     sprintf(filename,"rsl.error.%04d",*me) ;
     }
    else
     {
     sprintf(filename,"/dev/null") ;
     }
# endif
    if ((newfd = open( filename, O_CREAT | O_WRONLY | O_TRUNC, 0666 )) < 0 )
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
    fprintf( stdout, "taskid: %d hostname: %s\n",*me,hostname) ;
    fprintf( stderr, "taskid: %d hostname: %s\n",*me,hostname) ;
# if defined( _WIN32 ) 
    }
# endif
#else
# ifndef NCEP_DEBUG_GLOBALSTDOUT

/*create TASKOUTPUT directory to contain separate task owned output directories*/
                                                                                                                                              
   /* let task 0 create the subdirectory path for the task directories */
                                                                                                                                              
    if (*me == 0)
    {
        sprintf(dirname, "%s","TASKOUTPUT");
        rc = mkdir(dirname, 0777);
        if ( rc != 0 && errno==EEXIST) rc=0;
    }
                                                                                                                                              
    /* If TASKOUTPUT directory is not created then return */
                                                                                                                                              
    MPI_Bcast(&rc, 1, MPI_INTEGER, 0, MPI_COMM_WORLD);
                                                                                                                                              
    if (rc != 0 ) {
       if (*me == 0 ) {
          perror("mkdir error");
          fprintf(stderr, "mkdir failed for directory %s on task %d. Sending error/output to stderr/stdout for all tasks and continuing.\n", dirname, *me);
          return;
       }
       else {
          return;
       }
    }
        
    /* TASKOUTPUT directory exists, continue with task specific directory */
                                                                                                                                              
    sprintf(dirname, "TASKOUTPUT/%04d", *me);
    rc=mkdir(dirname, 0777);
    if (  rc !=0 && errno!=EEXIST ) {
        perror("mkdir error");
        fprintf(stderr, "mkdir failed for directory %s on task %d. Sending error/output to stderr/stdout and continuing.\n", dirname, *me);
        return;
    }
                                                                                                                                              
   /* Each tasks creates/opens its own output and error files */
                                                                                                                                              
   sprintf(filename, "%s/%04d/rsl.out.%04d","TASKOUTPUT",*me,*me) ;
        
   if ((newfd = open( filename, O_CREAT | O_WRONLY | O_TRUNC, 0666 )) < 0 )
   {
        perror("error_dup: cannot open ./TASKOUTPUT/nnnn/rsl.out.nnnn") ;
        fprintf(stderr,"...sending output to standard output and continuing.\n")
 ;
        return ;
   }
   if( dup2( newfd, STANDARD_OUTPUT ) < 0 )
   {
        perror("error_dup: dup2 fails to change output descriptor") ;
        fprintf(stderr,"...sending output to standard output and continuing.\n");
        close(newfd) ;
        return ;
   }
        
   sprintf(filename, "%s/%04d/rsl.error.%04d","TASKOUTPUT",*me,*me) ;
   if ((newfd = open( filename, O_CREAT | O_WRONLY | O_TRUNC, 0666 )) < 0 )
   {
       perror("error_dup: cannot open ./TASKOUTPUT/nnnn/rsl.error.nnnn") ;
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
# else
/* Each task writes to global standard error and standard out */
     
   return;
     
# endif
#endif
}

#ifdef _WIN32
/* Windows doesn't have a gethostid function so add a stub.
   TODO: Create a version that will work on Windows. */
int
gethostid ()
{
        return 0;
}
#endif

RSL_LITE_GET_HOSTNAME ( char * hn, int * size, int *n, int *hostid ) 
{
   char temp[512] ;
   char *p, *q ; 
   int i, cs ;
   if ( gethostname(temp,512) ) return(1) ;
   cs = gethostid() ;
   for ( p = temp , q = hn , i = 0 ; *p && i < *size && i < 512 ; i++ , p++ , q++ ) { *q = *p ; }
   *n = i ;
   *hostid = cs ;
   return(0) ;
}

BYTE_BCAST ( char * buf, int * size, int * Fcomm )
{
#ifndef STUBMPI
    MPI_Comm *comm, dummy_comm ;

    comm = &dummy_comm ;
    *comm = MPI_Comm_f2c( *Fcomm ) ;
# ifdef crayx1
    if (*size % sizeof(int) == 0) {
       MPI_Bcast ( buf, *size/sizeof(int), MPI_INT, 0, *comm ) ;
    } else {
       MPI_Bcast ( buf, *size, MPI_BYTE, 0, *comm ) ;
    }
# else
    MPI_Bcast ( buf, *size, MPI_BYTE, 0, *comm ) ;
# endif
#endif
}

BYTE_BCAST_FROM_ROOT ( char * buf, int * size, int *root , int * Fcomm )
{
#ifndef STUBMPI
    MPI_Comm *comm, dummy_comm ;

    comm = &dummy_comm ;
    *comm = MPI_Comm_f2c( *Fcomm ) ;
# ifdef crayx1
    if (*size % sizeof(int) == 0) {
       MPI_Bcast ( buf, *size/sizeof(int), MPI_INT, *root, *comm ) ;
    } else {
       MPI_Bcast ( buf, *size, MPI_BYTE, *root, *comm ) ;
    }
# else
    MPI_Bcast ( buf, *size, MPI_BYTE, *root, *comm ) ;
# endif
#endif
}

static int yp_curs, ym_curs, xp_curs, xm_curs ;
static int yp_curs_recv, ym_curs_recv, xp_curs_recv, xm_curs_recv ;

RSL_LITE_INIT_EXCH ( 
                int * Fcomm0,
                int * shw0,  int * xy0 ,
                int *sendbegm0 , int * sendwm0 , int * sendbegp0 , int * sendwp0 ,
                int *recvbegm0 , int * recvwm0 , int * recvbegp0 , int * recvwp0 ,
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
  int sendbegm , sendwm, sendbegp , sendwp ;
  int recvbegm , recvwm, recvbegp , recvwp ;
  int me, np, np_x, np_y ;
  int ips , ipe , jps , jpe , kps , kpe ;
  int xy ;
  int yp, ym, xp, xm ;
  int nbytes ;
  int nbytes_x_recv = 0, nbytes_y_recv = 0 ;

#ifndef STUBMPI
  MPI_Comm comm, *comm0, dummy_comm ;

  comm0 = &dummy_comm ;
  *comm0 = MPI_Comm_f2c( *Fcomm0 ) ;

  shw = *shw0 ;          /* logical half-width of stencil */
  xy = *xy0 ;              /* 0 = y , 1 = x */
  sendbegm = *sendbegm0 ;  /* send index of sten copy (edge = 1), lower/left */
  sendwm   = *sendwm0   ;  /* send width of sten copy counting towards edge, lower/left */
  sendbegp = *sendbegp0 ;  /* send index of sten copy (edge = 1), upper/right */
  sendwp   = *sendwp0   ;  /* send width of sten copy counting towards edge, upper/right */
  recvbegm = *recvbegm0 ;  /* recv index of sten copy (edge = 1), lower/left */
  recvwm   = *recvwm0   ;  /* recv width of sten copy counting towards edge, lower/left */
  recvbegp = *recvbegp0 ;  /* recv index of sten copy (edge = 1), upper/right */
  recvwp   = *recvwp0   ;  /* recv width of sten copy counting towards edge, upper/right */
  n3dR = *n3dR0 ; n2dR = *n2dR0 ; typesizeR = *typesizeR0 ;
  n3dI = *n3dI0 ; n2dI = *n2dI0 ; typesizeI = *typesizeI0 ;
  n3dD = *n3dD0 ; n2dD = *n2dD0 ; typesizeD = *typesizeD0 ;
  n3dL = *n3dL0 ; n2dL = *n2dL0 ; typesizeL = *typesizeL0 ;
  me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;
  ips = *ips0-1 ; ipe = *ipe0-1 ; jps = *jps0-1 ; jpe = *jpe0-1 ; kps = *kps0-1 ; kpe = *kpe0-1 ;

  yp_curs_recv = 0 ; ym_curs_recv = 0 ; 
  xp_curs_recv = 0 ; xm_curs_recv = 0 ;

  if ( xy == 0 && np_y > 1 ) {
    nbytes = typesizeR*(ipe-ips+1+2*shw)*shw*(n3dR*(kpe-kps+1)+n2dR) +
             typesizeI*(ipe-ips+1+2*shw)*shw*(n3dI*(kpe-kps+1)+n2dI) +
             typesizeD*(ipe-ips+1+2*shw)*shw*(n3dD*(kpe-kps+1)+n2dD) +
             typesizeL*(ipe-ips+1+2*shw)*shw*(n3dL*(kpe-kps+1)+n2dL) ;
    nbytes_y_recv = 
             typesizeR*(ipe-ips+1+2*shw)*shw*(n3dR*(kpe-kps+1)+n2dR) +
             typesizeI*(ipe-ips+1+2*shw)*shw*(n3dI*(kpe-kps+1)+n2dI) +
             typesizeD*(ipe-ips+1+2*shw)*shw*(n3dD*(kpe-kps+1)+n2dD) +
             typesizeL*(ipe-ips+1+2*shw)*shw*(n3dL*(kpe-kps+1)+n2dL) ;
    MPI_Cart_shift ( *comm0, 0, 1, &ym, &yp ) ;
    if ( yp != MPI_PROC_NULL ) {
       buffer_for_proc ( yp , nbytes_y_recv, RSL_RECVBUF ) ;
       buffer_for_proc ( yp , nbytes, RSL_SENDBUF ) ;
    }
    if ( ym != MPI_PROC_NULL ) {
       buffer_for_proc ( ym , nbytes_y_recv, RSL_RECVBUF ) ;
       buffer_for_proc ( ym , nbytes, RSL_SENDBUF ) ;
    }
  }
  if ( xy == 1 && np_x > 1 ) {
    nbytes = typesizeR*(jpe-jps+1+2*shw)*shw*(n3dR*(kpe-kps+1)+n2dR) +
             typesizeI*(jpe-jps+1+2*shw)*shw*(n3dI*(kpe-kps+1)+n2dI) +
             typesizeD*(jpe-jps+1+2*shw)*shw*(n3dD*(kpe-kps+1)+n2dD) +
             typesizeL*(jpe-jps+1+2*shw)*shw*(n3dL*(kpe-kps+1)+n2dL) ;
    nbytes_x_recv = 
             typesizeR*(jpe-jps+1+2*shw)*shw*(n3dR*(kpe-kps+1)+n2dR) +
             typesizeI*(jpe-jps+1+2*shw)*shw*(n3dI*(kpe-kps+1)+n2dI) +
             typesizeD*(jpe-jps+1+2*shw)*shw*(n3dD*(kpe-kps+1)+n2dD) +
             typesizeL*(jpe-jps+1+2*shw)*shw*(n3dL*(kpe-kps+1)+n2dL) ;
    MPI_Cart_shift ( *comm0, 1, 1, &xm, &xp ) ;
    if ( xp != MPI_PROC_NULL ) {
       buffer_for_proc ( xp , nbytes_x_recv, RSL_RECVBUF ) ;
       buffer_for_proc ( xp , nbytes, RSL_SENDBUF ) ;
    }
    if ( xm != MPI_PROC_NULL ) {
       buffer_for_proc ( xm , nbytes_x_recv, RSL_RECVBUF ) ;
       buffer_for_proc ( xm , nbytes, RSL_SENDBUF ) ;
    }
  }
#endif
  yp_curs = 0 ; ym_curs = 0 ; xp_curs = 0 ; xm_curs = 0 ;
  yp_curs_recv = nbytes_y_recv ; ym_curs_recv = nbytes_y_recv ; 
  xp_curs_recv = nbytes_x_recv ; xm_curs_recv = nbytes_x_recv ;
}

RSL_LITE_PACK ( int * Fcomm0, char * buf , int * shw0 , 
           int * sendbegm0 , int * sendwm0 , int * sendbegp0 , int * sendwp0 ,
           int * recvbegm0 , int * recvwm0 , int * recvbegp0 , int * recvwp0 ,
           int * typesize0 , int * xy0 , int * pu0 , int * imemord , int * xstag0, /* not used */
           int *me0, int * np0 , int * np_x0 , int * np_y0 , 
           int * ids0 , int * ide0 , int * jds0 , int * jde0 , int * kds0 , int * kde0 ,
           int * ims0 , int * ime0 , int * jms0 , int * jme0 , int * kms0 , int * kme0 ,
           int * ips0 , int * ipe0 , int * jps0 , int * jpe0 , int * kps0 , int * kpe0 )
{
  int me, np, np_x, np_y ;
  int sendbegm , sendwm, sendbegp , sendwp ;
  int recvbegm , recvwm, recvbegp , recvwp ;
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

#ifndef STUBMPI
  MPI_Comm comm, *comm0, dummy_comm ;
  int js, je, ks, ke, is, ie, wcount ;

  comm0 = &dummy_comm ;
  *comm0 = MPI_Comm_f2c( *Fcomm0 ) ;

  shw = *shw0 ;          /* logical half-width of stencil */
  sendbegm = *sendbegm0 ;  /* send index of sten copy (edge = 1), lower/left */
  sendwm   = *sendwm0   ;  /* send width of sten copy counting towards edge, lower/left */
  sendbegp = *sendbegp0 ;  /* send index of sten copy (edge = 1), upper/right */
  sendwp   = *sendwp0   ;  /* send width of sten copy counting towards edge, upper/right */
  recvbegm = *recvbegm0 ;  /* recv index of sten copy (edge = 1), lower/left */
  recvwm   = *recvwm0   ;  /* recv width of sten copy counting towards edge, lower/left */
  recvbegp = *recvbegp0 ;  /* recv index of sten copy (edge = 1), upper/right */
  recvwp   = *recvwp0   ;  /* recv width of sten copy counting towards edge, upper/right */
  me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;
  typesize = *typesize0 ;
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

  if ( ips <= ipe && jps <= jpe ) {

  if ( np_y > 1 && xy == 0 ) {
    MPI_Cart_shift( *comm0 , 0, 1, &ym, &yp ) ;
    if ( yp != MPI_PROC_NULL && jpe <= jde  && jde != jpe ) {
      p = buffer_for_proc( yp , 0 , da_buf ) ;
      if ( pu == 0 ) {
        if ( sendwp > 0 ) {
          je = jpe - sendbegp + 1 ; js = je - sendwp + 1 ;
          ks = kps           ; ke = kpe ;
          is = IMAX(ips-shw) ; ie = IMIN(ipe+shw) ;
          nbytes = buffer_size_for_proc( yp, da_buf ) ;
	  if ( yp_curs + RANGE( js, je, kps, kpe, ips-shw, ipe+shw, 1, typesize ) > nbytes ) {
#ifndef MS_SUA
	    fprintf(stderr,"memory overwrite in rsl_lite_pack, Y pack up, %d > %d\n",
	        yp_curs + RANGE( js, je, kps, kpe, ips-shw, ipe+shw, 1, typesize ), nbytes ) ;
#endif
	    MPI_Abort(MPI_COMM_WORLD, 99) ;
          }
          if ( typesize == 8 ) {
            F_PACK_LINT ( buf, p+yp_curs, imemord, &js, &je, &ks, &ke, &is, &ie, 
                                                &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
            yp_curs += wcount*typesize ;
          }
	  else if ( typesize == 4 ) {
            F_PACK_INT ( buf, p+yp_curs, imemord, &js, &je, &ks, &ke, &is, &ie,
                                               &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
            yp_curs += wcount*typesize ;
	  }
	  else {
#ifndef MS_SUA
            fprintf(stderr,"internal error: %s %d\n",__FILE__,__LINE__) ;
#endif
          }
        }
      } else {
        if ( recvwp > 0 ) {
          js = jpe+recvbegp         ; je = js + recvwp - 1 ;
          ks = kps           ; ke = kpe ;
          is = IMAX(ips-shw) ; ie = IMIN(ipe+shw) ;
          if ( typesize == 8 ) {
            F_UNPACK_LINT ( p+yp_curs, buf, imemord, &js, &je, &ks, &ke, &is, &ie,
                                               &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
            yp_curs += wcount*typesize ;
          }
	  else if ( typesize == 4 ) {
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
    }
    if ( ym != MPI_PROC_NULL && jps >= jds  && jps != jds ) {
      p = buffer_for_proc( ym , 0 , da_buf ) ;
      if ( pu == 0 ) {
        if ( sendwm > 0 ) {
          js = jps+sendbegm-1 ; je = js + sendwm -1 ;
          ks = kps           ; ke = kpe ;
          is = IMAX(ips-shw) ; ie = IMIN(ipe+shw) ;
          nbytes = buffer_size_for_proc( ym, da_buf ) ;
	  if ( ym_curs + RANGE( js, je, kps, kpe, ips-shw, ipe+shw, 1, typesize ) > nbytes ) {
#ifndef  MS_SUA
	    fprintf(stderr,"memory overwrite in rsl_lite_pack, Y pack dn, %d > %d\n",
	        ym_curs + RANGE( js, je, kps, kpe, ips-shw, ipe+shw, 1, typesize ), nbytes ) ;
#endif
	    MPI_Abort(MPI_COMM_WORLD, 99) ;
          }
          if ( typesize == 8 ) {
            F_PACK_LINT ( buf, p+ym_curs, imemord, &js, &je, &ks, &ke, &is, &ie,
                                               &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
            ym_curs += wcount*typesize ;
          }
	  else if ( typesize == 4 ) {
            F_PACK_INT ( buf, p+ym_curs, imemord, &js, &je, &ks, &ke, &is, &ie,
                                               &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
            ym_curs += wcount*typesize ;
  	  }
	  else {
#ifndef MS_SUA
            fprintf(stderr,"internal error: %s %d\n",__FILE__,__LINE__) ;
#endif
	  }
	}
      } else {
        if ( recvwm > 0 ) {
          je = jps-recvbegm ; js = je - recvwm + 1 ;
          ks = kps           ; ke = kpe ;
          is = IMAX(ips-shw) ; ie = IMIN(ipe+shw) ;
          if ( typesize == 8 ) {
            F_UNPACK_LINT ( p+ym_curs, buf, imemord, &js, &je, &ks, &ke, &is, &ie,
                                                  &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
            ym_curs += wcount*typesize ;
          }
	  else if ( typesize == 4 ) {
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
  }

  if ( np_x > 1 && xy == 1 ) {
    MPI_Cart_shift( *comm0, 1, 1, &xm, &xp ) ;
    if ( xp != MPI_PROC_NULL  && ipe <= ide && ide != ipe ) {
      p = buffer_for_proc( xp , 0 , da_buf ) ;
      if ( pu == 0 ) {
        if ( sendwp > 0 ) {
          js = JMAX(jps-shw) ; je = JMIN(jpe+shw) ;
          ks = kps           ; ke = kpe ;
          ie = ipe - sendbegp + 1 ; is = ie - sendwp + 1 ;
          nbytes = buffer_size_for_proc( xp, da_buf ) ;
          if ( xp_curs + RANGE( js, je, kps, kpe, ipe-shw+1, ipe, 1, typesize ) > nbytes ) {
#ifndef MS_SUA
	    fprintf(stderr,"memory overwrite in rsl_lite_pack, X pack right, %d > %d\n",
	        xp_curs + RANGE( js, je, kps, kpe, ipe-shw+1, ipe, 1, typesize ), nbytes ) ;
#endif
	    MPI_Abort(MPI_COMM_WORLD, 99) ;
          }
          if ( typesize == 8 ) {
            F_PACK_LINT ( buf, p+xp_curs, imemord, &js, &je, &ks, &ke, &is, &ie,
                                                &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
            xp_curs += wcount*typesize ;
          }
	  else if ( typesize == 4 ) {
            F_PACK_INT ( buf, p+xp_curs, imemord, &js, &je, &ks, &ke, &is, &ie,
                                               &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
            xp_curs += wcount*typesize ;
	  }
	  else {
#ifndef MS_SUA
            fprintf(stderr,"A internal error: %s %d\n",__FILE__,__LINE__) ;
#endif
	  }
	}
      } else {
        if ( recvwp > 0 ) {
          js = JMAX(jps-shw) ; je = JMIN(jpe+shw) ;
          ks = kps           ; ke = kpe ;
          is = ipe+recvbegp  ; ie = is + recvwp - 1 ;
          if ( typesize == 8 ) {
            F_UNPACK_LINT ( p+xp_curs, buf, imemord, &js, &je, &ks, &ke, &is, &ie,
                                                  &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
            xp_curs += wcount*typesize ;
          }
	  else if ( typesize == 4 ) {
            F_UNPACK_INT ( p+xp_curs, buf, imemord, &js, &je, &ks, &ke, &is, &ie,
                                                 &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
            xp_curs += wcount*typesize ;
	  }
	  else {
#ifndef MS_SUA
            fprintf(stderr,"B internal error: %s %d\n",__FILE__,__LINE__) ;
            fprintf(stderr,"  stenbeg %d stenw  %d \n",is,ie) ;
            fprintf(stderr,"  is %d ie %d \n",is,ie) ;
#endif
          }
        }
      }
    }
    if ( xm != MPI_PROC_NULL  && ips >= ids && ids != ips ) {
      p = buffer_for_proc( xm , 0 , da_buf ) ;
      if ( pu == 0 ) {
        if ( sendwm > 0 ) {
          js = JMAX(jps-shw) ; je = JMIN(jpe+shw) ;
          ks = kps           ; ke = kpe ;
          is = ips+sendbegm-1 ; ie = is + sendwm-1 ;
          nbytes = buffer_size_for_proc( xm, da_buf ) ;
          if ( xm_curs + RANGE( js, je, kps, kpe, ips, ips+shw-1, 1, typesize ) > nbytes ) {
#ifndef MS_SUA
	    fprintf(stderr,"memory overwrite in rsl_lite_pack, X left , %d > %d\n",
	        xm_curs + RANGE( js, je, kps, kpe, ips, ips+shw-1, 1, typesize ), nbytes ) ;
#endif
	    MPI_Abort(MPI_COMM_WORLD, 99) ;
          }
          if ( typesize == 8 ) {
            F_PACK_LINT ( buf, p+xm_curs, imemord, &js, &je, &ks, &ke, &is, &ie,
                                                &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
            xm_curs += wcount*typesize ;
          }
	  else if ( typesize == 4 ) {
            F_PACK_INT ( buf, p+xm_curs, imemord, &js, &je, &ks, &ke, &is, &ie,
                                               &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
            xm_curs += wcount*typesize ;
	  }
	  else {
#ifndef MS_SUA
            fprintf(stderr,"internal error: %s %d\n",__FILE__,__LINE__) ;
#endif
          }
        }
      } else {
        if ( recvwm > 0 ) {
          js = JMAX(jps-shw) ; je = JMIN(jpe+shw) ;
          ks = kps           ; ke = kpe ;
          ie = ips-recvbegm ; is = ie - recvwm + 1 ;
          if ( typesize == 8 ) {
            F_UNPACK_LINT ( p+xm_curs, buf, imemord, &js, &je, &ks, &ke, &is, &ie,
                                                  &jms,&jme,&kms,&kme,&ims,&ime, &wcount ) ;
            xm_curs += wcount*typesize ;
          } 
          else if ( typesize == 4 ) {
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
  }
  }
#endif

}

#ifndef STUBMPI
static MPI_Request yp_recv, ym_recv, yp_send, ym_send ;
static MPI_Request xp_recv, xm_recv, xp_send, xm_send ;
#endif

RSL_LITE_EXCH_Y ( int * Fcomm0, int *me0, int * np0 , int * np_x0 , int * np_y0 ,
                  int * sendw_m, int * sendw_p, int * recvw_m , int * recvw_p )
{
  int me, np, np_x, np_y ;
  int yp, ym, xp, xm, ierr ;
#ifndef STUBMPI
  MPI_Status stat ;
  MPI_Comm comm, *comm0, dummy_comm ;

  comm0 = &dummy_comm ;
  *comm0 = MPI_Comm_f2c( *Fcomm0 ) ;
  comm = *comm0 ; me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;
  if ( np_y > 1 ) {
    MPI_Cart_shift( *comm0, 0, 1, &ym, &yp ) ;
    if ( yp != MPI_PROC_NULL && *recvw_p > 0 ) {
      ierr=MPI_Irecv ( buffer_for_proc( yp, yp_curs_recv, RSL_RECVBUF ), yp_curs_recv, MPI_CHAR, yp, me, comm, &yp_recv ) ;
    }
    if ( ym != MPI_PROC_NULL && *recvw_m > 0 ) {
      ierr=MPI_Irecv ( buffer_for_proc( ym, ym_curs_recv, RSL_RECVBUF ), ym_curs_recv, MPI_CHAR, ym, me, comm, &ym_recv ) ;
    }
    if ( yp != MPI_PROC_NULL && *sendw_p > 0 ) {
      ierr=MPI_Isend ( buffer_for_proc( yp, 0,       RSL_SENDBUF ), yp_curs, MPI_CHAR, yp, yp, comm, &yp_send ) ;
    }
    if ( ym != MPI_PROC_NULL && *sendw_m > 0 ) {
      ierr=MPI_Isend ( buffer_for_proc( ym, 0,       RSL_SENDBUF ), ym_curs, MPI_CHAR, ym, ym, comm, &ym_send ) ;
    }
    if ( yp != MPI_PROC_NULL && *recvw_p > 0 ) {  MPI_Wait( &yp_recv, &stat ) ;  }
    if ( ym != MPI_PROC_NULL && *recvw_m > 0 ) {  MPI_Wait( &ym_recv, &stat ) ;  }
    if ( yp != MPI_PROC_NULL && *sendw_p > 0 ) {  MPI_Wait( &yp_send, &stat ) ;  }
    if ( ym != MPI_PROC_NULL && *sendw_m > 0 ) {  MPI_Wait( &ym_send, &stat ) ;  }
  }
  yp_curs = 0 ; ym_curs = 0 ; xp_curs = 0 ; xm_curs = 0 ;
  yp_curs_recv = 0 ; ym_curs_recv = 0 ; 
  xp_curs_recv = 0 ; xm_curs_recv = 0 ;
#endif
}

RSL_LITE_EXCH_X ( int * Fcomm0, int *me0, int * np0 , int * np_x0 , int * np_y0 ,
                  int * sendw_m, int * sendw_p, int * recvw_m , int * recvw_p )
{
  int me, np, np_x, np_y ;
  int yp, ym, xp, xm ;
#ifndef STUBMPI
  MPI_Status stat ;
  MPI_Comm comm, *comm0, dummy_comm ;

  comm0 = &dummy_comm ;
  *comm0 = MPI_Comm_f2c( *Fcomm0 ) ;
  comm = *comm0 ; me = *me0 ; np = *np0 ; np_x = *np_x0 ; np_y = *np_y0 ;
  if ( np_x > 1 ) {
    MPI_Cart_shift( *comm0, 1, 1, &xm, &xp ) ;
    if ( xp != MPI_PROC_NULL && *recvw_p > 0 ) {
      MPI_Irecv ( buffer_for_proc( xp, xp_curs_recv, RSL_RECVBUF ), xp_curs_recv, MPI_CHAR, xp, me, comm, &xp_recv ) ;
    }
    if ( xm != MPI_PROC_NULL && *recvw_m > 0 ) {
      MPI_Irecv ( buffer_for_proc( xm, xm_curs_recv, RSL_RECVBUF ), xm_curs_recv, MPI_CHAR, xm, me, comm, &xm_recv ) ;
    }
    if ( xp != MPI_PROC_NULL && *sendw_p > 0 ) {
      MPI_Isend ( buffer_for_proc( xp, 0,       RSL_SENDBUF ), xp_curs, MPI_CHAR, xp, xp, comm, &xp_send ) ;
    }
    if ( xm != MPI_PROC_NULL && *sendw_m > 0 ) {
      MPI_Isend ( buffer_for_proc( xm, 0,       RSL_SENDBUF ), xm_curs, MPI_CHAR, xm, xm, comm, &xm_send ) ;
    }
    if ( xp != MPI_PROC_NULL && *recvw_p > 0 ) {  MPI_Wait( &xp_recv, &stat ) ;  }
    if ( xm != MPI_PROC_NULL && *recvw_m > 0 ) {  MPI_Wait( &xm_recv, &stat ) ;  }
    if ( xp != MPI_PROC_NULL && *sendw_p > 0 ) {  MPI_Wait( &xp_send, &stat ) ;  }
    if ( xm != MPI_PROC_NULL && *sendw_m > 0 ) {  MPI_Wait( &xm_send, &stat ) ;  }
  }
  yp_curs = 0 ; ym_curs = 0 ; xp_curs = 0 ; xm_curs = 0 ;
  yp_curs_recv = 0 ; ym_curs_recv = 0 ; 
  xp_curs_recv = 0 ; xm_curs_recv = 0 ;
#endif
}

#if !defined( MS_SUA)  && !defined(_WIN32)
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
#endif
