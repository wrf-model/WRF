#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include "compat.h"

/* this is not used in RSL but provided to programs that may want it (like WRF for NCEP) */

COLLECT_ON_COMM ( int * comm, int * typesize ,
                 void * inbuf, int *ninbuf , void * outbuf, int * noutbuf )
{
  int mytask, ntasks, p ;
  int *recvcounts ;
  int *displace ;
  int noutbuf_loc ;

  MPI_Comm_size ( *comm, &ntasks ) ;
  MPI_Comm_rank ( *comm, &mytask ) ;
  recvcounts = (int *) malloc( ntasks * sizeof(int)) ;
  displace   = (int *) malloc( ntasks * sizeof(int)) ;

  /* collect up recvcounts */
  MPI_Gather( ninbuf , 1 , MPI_INT , recvcounts , 1 , MPI_INT , ntasks-1 , *comm ) ;

  if ( mytask == ntasks-1 ) {

    /* figure out displacements */
    for ( p = 1 , displace[0] = 0 , noutbuf_loc = recvcounts[0] ; p < ntasks ; p++ ) {
      displace[p] = displace[p-1]+recvcounts[p-1] ;
      noutbuf_loc = noutbuf_loc + recvcounts[p] ;
    }

    if ( noutbuf_loc > * noutbuf )
    {
      fprintf(stderr,"FATAL ERROR: collect_on_comm: noutbuf_loc (%d) > noutbuf (%d)\n",
		      noutbuf_loc , * noutbuf ) ; 
      fprintf(stderr,"WILL NOT perform the collection operation\n") ;
      MPI_Abort(MPI_COMM_WORLD,1) ;
    }

    /* multiply everything by the size of the type */
    for ( p = 0 ; p < ntasks ; p++ ) {
      displace[p] *= *typesize ;
      recvcounts[p] *= *typesize ;
    }
  }

  MPI_Gatherv( inbuf  , *ninbuf * *typesize  , MPI_CHAR ,
               outbuf , recvcounts , displace, MPI_CHAR ,
               ntasks-1 , *comm ) ;

  free(recvcounts) ;
  free(displace) ;
  return(0) ;
}

/*    CALL int_pack_data ( hdrbuf , hdrbufsiz * inttypesize , int_local_output_buffer, int_local_output_cursor ) */

INT_PACK_DATA ( unsigned char *buf , int *ninbytes , unsigned char *obuf, int *cursor )
{
  int i, lcurs ;
  lcurs = *cursor - 1 ;
  for ( i = 0 ; i < *ninbytes ; i++ )
  {
    obuf[lcurs++] = buf[i] ;
  }
  *cursor = lcurs+1 ;
}


