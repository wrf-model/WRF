#include <stdio.h>
#include <mpi.h>
main()
{
   int y ;
   MPI_Comm x ;
   y = 1 ;
#if 1
   x = MPI_Comm_f2c( y ) ;
   y = MPI_Comm_c2f( x ) ;
#endif
   fprintf(stderr,"y %d \n",y) ;
}
