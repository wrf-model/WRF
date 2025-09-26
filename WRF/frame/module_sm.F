!WRF:PACKAGE:OPENMP
!

MODULE module_sm

#ifdef _OPENMP

   !  There are a few functions in the OpenMP library,
   !  and to use them easily, we need to define the 
   !  return types of these functions.

   INTEGER , EXTERNAL :: omp_get_num_threads , &
                         omp_get_max_threads , &
                         omp_get_thread_num  , &
                         omp_get_num_procs

   LOGICAL , EXTERNAL :: omp_in_parallel        
#endif

CONTAINS

   SUBROUTINE omp_info

#ifdef _OPENMP

      IMPLICIT NONE

      PRINT '(/A,/,A,/,A,I2/)','omp_get_num_threads:', &
                              'Number of threads currently in the team executing', &
                              'the parallel region = ',omp_get_num_threads()

      PRINT '(A,/,A,/,A,I2/)', 'omp_get_max_threads:', &
                              'Maximum value that can be returned by the',&
                              'omp_get_num_threads function = ',omp_get_max_threads()

      PRINT '(A,/,A,/,A,I2/)', 'omp_get_thread_num:', &
                              'Returns the thread number, within the team, between', &
                              '0 and omp_get_num_threads-1, inclusive = ',omp_get_thread_num()

      PRINT '(A,/,A,/,A,I2/)', 'omp_get_num_procs:', &
                              'Returns the number of processors that are available', &
                              'to the program = ',omp_get_num_procs()

      PRINT '(A,/,A,/,A,L7/)','omp_in_parallel:', &
                              'Returns .TRUE. if called with the dynamic extent of a region', &
                              'executing in parallel, and otherwise .FALSE. = ',omp_in_parallel()

#endif

   END SUBROUTINE omp_info

   SUBROUTINE init_module_sm
   END SUBROUTINE init_module_sm

END MODULE module_sm
