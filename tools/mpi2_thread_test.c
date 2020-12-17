#include <stdio.h>
#include <mpi.h>
main( int argc, char ** argv ) 
{
   int y, retval ;
   MPI_Comm x ;
   y = 1 ;

   MPI_Init_thread( &argc, &argv, MPI_THREAD_FUNNELED, &retval  ) ;
}


