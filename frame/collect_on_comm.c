#ifndef MS_SUA
# include <stdio.h>
# include <stdlib.h>
#endif
#if defined( DM_PARALLEL ) && ! defined( STUBMPI )
# include <mpi.h>
#endif

#ifndef CRAY
# ifdef NOUNDERSCORE
#      define COLLECT_ON_COMM  collect_on_comm
#      define COLLECT_ON_COMM0 collect_on_comm0
#      define DIST_ON_COMM  dist_on_comm
#      define DIST_ON_COMM0 dist_on_comm0
#      define INT_PACK_DATA  int_pack_data
#      define INT_GET_TI_HEADER_C  int_get_ti_header_c
#      define INT_GEN_TI_HEADER_C  int_gen_ti_header_c
# else
#   ifdef F2CSTYLE
#      define COLLECT_ON_COMM  collect_on_comm__
#      define COLLECT_ON_COMM0 collect_on_comm0__
#      define DIST_ON_COMM  dist_on_comm__
#      define DIST_ON_COMM0 dist_on_comm0__
#      define INT_PACK_DATA  int_pack_data__
#      define INT_GET_TI_HEADER_C  int_get_ti_header_c__
#      define INT_GEN_TI_HEADER_C  int_gen_ti_header_c__
#   else
#      define COLLECT_ON_COMM  collect_on_comm_
#      define COLLECT_ON_COMM0 collect_on_comm0_
#      define DIST_ON_COMM  dist_on_comm_
#      define DIST_ON_COMM0 dist_on_comm0_
#      define INT_PACK_DATA  int_pack_data_
#      define INT_GET_TI_HEADER_C  int_get_ti_header_c_
#      define INT_GEN_TI_HEADER_C  int_gen_ti_header_c_
#   endif
# endif
#endif

  
int col_on_comm ( int *, int *, void *, int *, void *, int *, int);
int dst_on_comm ( int *, int *, void *, int *, void *, int *, int);

void 
COLLECT_ON_COMM ( int * comm, int * typesize ,
                  void * inbuf, int *ninbuf , void * outbuf, int * noutbuf )
{
  col_on_comm ( comm, typesize ,
                inbuf, ninbuf , outbuf, noutbuf, 1 ) ;
}

/* collect on node 0*/
void
COLLECT_ON_COMM0 ( int * comm, int * typesize ,
                 void * inbuf, int *ninbuf , void * outbuf, int * noutbuf )
{
  col_on_comm ( comm, typesize ,
                inbuf, ninbuf , outbuf, noutbuf, 0 ) ;
}

int
col_on_comm ( int * Fcomm, int * typesize ,
              void * inbuf, int *ninbuf , void * outbuf, int * noutbuf, int sw )
{
#if defined( DM_PARALLEL ) && ! defined(STUBMPI)
  int mytask, ntasks, p ;
  int *recvcounts ;
  int *displace ;
  int noutbuf_loc ;
  int root_task ;
  MPI_Comm *comm, dummy_comm ;
  int ierr ;

  comm = &dummy_comm ;
  *comm = MPI_Comm_f2c( *Fcomm ) ;
  MPI_Comm_size ( *comm, &ntasks ) ;
  MPI_Comm_rank ( *comm, &mytask ) ;
  recvcounts = (int *) malloc( ntasks * sizeof(int)) ;
  displace   = (int *) malloc( ntasks * sizeof(int)) ;
  root_task = ( sw == 0 ) ? 0 : ntasks-1 ;

  /* collect up recvcounts */
  ierr = MPI_Gather( ninbuf , 1 , MPI_INT , recvcounts , 1 , MPI_INT , root_task , *comm ) ;
#ifndef MS_SUA
  if ( ierr != 0 ) fprintf(stderr,"%s %d MPI_Gather returns %d\n",__FILE__,__LINE__,ierr ) ;
#endif

  if ( mytask == root_task ) {

    /* figure out displacements */
    for ( p = 1 , displace[0] = 0 , noutbuf_loc = recvcounts[0] ; p < ntasks ; p++ ) {
      displace[p] = displace[p-1]+recvcounts[p-1] ;
      noutbuf_loc = noutbuf_loc + recvcounts[p] ;
    }

    if ( noutbuf_loc > * noutbuf )
    {
#ifndef MS_SUA
      fprintf(stderr,"FATAL ERROR: collect_on_comm: noutbuf_loc (%d) > noutbuf (%d)\n",
		      noutbuf_loc , * noutbuf ) ; 
      fprintf(stderr,"WILL NOT perform the collection operation\n") ;
#endif
      MPI_Abort(MPI_COMM_WORLD,1) ;
    }

    /* multiply everything by the size of the type */
    for ( p = 0 ; p < ntasks ; p++ ) {
      displace[p] *= *typesize ;
      recvcounts[p] *= *typesize ;
    }
  }

  ierr = MPI_Gatherv( inbuf  , *ninbuf * *typesize  , MPI_CHAR ,
               outbuf , recvcounts , displace, MPI_CHAR ,
               root_task , *comm ) ;
#ifndef MS_SUA
  if ( ierr != 0 ) fprintf(stderr,"%s %d MPI_Gatherv returns %d\n",__FILE__,__LINE__,ierr ) ;
#endif

  free(recvcounts) ;
  free(displace) ;
#endif
  return(0) ;
}

int
dst_on_comm ( int * Fcomm, int * typesize ,
              void * inbuf, int *ninbuf , void * outbuf, int * noutbuf, int sw ) ;

void
DIST_ON_COMM ( int * comm, int * typesize ,
                 void * inbuf, int *ninbuf , void * outbuf, int * noutbuf )
{
  dst_on_comm ( comm, typesize ,
                inbuf, ninbuf , outbuf, noutbuf, 1 ) ;
}

void
DIST_ON_COMM0 ( int * comm, int * typesize ,
                 void * inbuf, int *ninbuf , void * outbuf, int * noutbuf )
{
  dst_on_comm ( comm, typesize ,
                inbuf, ninbuf , outbuf, noutbuf, 0 ) ;
}

int
dst_on_comm ( int * Fcomm, int * typesize ,
              void * inbuf, int *ninbuf , void * outbuf, int * noutbuf, int sw )
{
#if defined(DM_PARALLEL) && ! defined(STUBMPI)
  int mytask, ntasks, p ;
  int *sendcounts ;
  int *displace ;
  int noutbuf_loc ;
  int root_task ;
  MPI_Comm *comm, dummy_comm ;

  comm = &dummy_comm ;
  *comm = MPI_Comm_f2c( *Fcomm ) ;
  MPI_Comm_size ( *comm, &ntasks ) ;
  MPI_Comm_rank ( *comm, &mytask ) ;
  sendcounts = (int *) malloc( ntasks * sizeof(int)) ;
  displace   = (int *) malloc( ntasks * sizeof(int)) ;
  root_task = ( sw == 0 ) ? 0 : ntasks-1 ;

  /* collect up sendcounts */
  MPI_Gather( noutbuf , 1 , MPI_INT , sendcounts , 1 , MPI_INT , root_task , *comm ) ;

  if ( mytask == root_task ) {

    /* figure out displacements */
    for ( p = 1 , displace[0] = 0 , noutbuf_loc = sendcounts[0] ; p < ntasks ; p++ ) {
      displace[p] = displace[p-1]+sendcounts[p-1] ;
      noutbuf_loc = noutbuf_loc + sendcounts[p] ;
    }

    /* multiply everything by the size of the type */
    for ( p = 0 ; p < ntasks ; p++ ) {
      displace[p] *= *typesize ;
      sendcounts[p] *= *typesize ;
    }
  }

  MPI_Scatterv( inbuf   , sendcounts , displace, MPI_CHAR ,
                outbuf  , *noutbuf * *typesize  , MPI_CHAR ,
                root_task , *comm ) ;

  free(sendcounts) ;
  free(displace) ;
#endif
  return(0) ;
}

#ifndef _WIN32
#ifndef MACOS
#  include <malloc.h>
#  include <sys/resource.h>
#endif

#if 0
  int getrusage(
          int who,
          struct rusage *r_usage);
#endif

#if 0
extern int outy ;
extern int maxstug, nouty, maxouty ;
#endif

#if 0
#include <unistd.h>
#include <sys/times.h>
/*  used internally for chasing memory leaks on ibm  */
rlim_ ()
{
#ifndef MACOS

   struct rusage r_usage ;
   struct mallinfo minf ;
   struct tms  tm ;
   long tick, tock ;

   tick = sysconf( _SC_CLK_TCK ) ;
   times( &tm ) ;
   tock = (tm.tms_utime + tm.tms_stime)*tick ;

   getrusage ( RUSAGE_SELF, &r_usage ) ;
   if ( tock != 0 ) {
#ifndef _WIN32
     fprintf(stderr,"sm %ld d %ld s %ld maxrss %ld %d %d %ld\n",r_usage.ru_ixrss/tock,r_usage.ru_idrss/tock,r_usage.ru_isrss/tock, r_usage.ru_maxrss,tick,tock,r_usage.ru_ixrss) ;
#endif
   }
   minf = mallinfo() ;
#ifndef _WIN32
   fprintf(stderr,"a %ld usm %ld fsm %ld uord %ld ford %ld hblkhd %d\n",minf.arena,minf.usmblks,minf.fsmblks,minf.uordblks,minf.fordblks,minf.hblkhd) ;
#endif
# if 0
   fprintf(stderr," outy %d  nouty %d  maxstug %d maxouty %d \n", outy, nouty, maxstug, maxouty ) ;
# endif
#endif
}
#endif
#endif

