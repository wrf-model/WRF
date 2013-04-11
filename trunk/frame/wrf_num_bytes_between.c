#include <stdio.h>

#ifndef CRAY
# ifdef NOUNDERSCORE
#  define WRF_NUM_BYTES_BETWEEN wrf_num_bytes_between
#  define GET_INITIAL_DATA_VALUE get_initial_data_value
#  define WHAT_IS_A_NAN  what_is_a_nan
#  define WRF_MEM_COPY  wrf_mem_copy
# else
#   ifdef F2CSTYLE
#  define WRF_NUM_BYTES_BETWEEN wrf_num_bytes_between__
#  define GET_INITIAL_DATA_VALUE get_initial_data_value__
#  define WHAT_IS_A_NAN  what_is_a_nan__
#  define WRF_MEM_COPY  wrf_mem_copy__
#   else
#  define WRF_NUM_BYTES_BETWEEN wrf_num_bytes_between_
#  define GET_INITIAL_DATA_VALUE get_initial_data_value_
#  define WHAT_IS_A_NAN  what_is_a_nan_
#  define WRF_MEM_COPY  wrf_mem_copy_
#   endif
# endif
#endif

void
WRF_NUM_BYTES_BETWEEN ( a , b , n )
  char * a ;
  char * b ;
  int * n ;
{
  *n = a - b ;
  if ( *n < 0 ) *n = -(*n) ;
}

/*#define NAN_VALUE */
#ifdef NAN_VALUE
void
GET_INITIAL_DATA_VALUE ( n )
  int * n ;
{
  *n = 0xffc00000 ;
}
#else
void
GET_INITIAL_DATA_VALUE ( n )
  float * n ;
{
  *n = 0. ;
}
#endif

void
WHAT_IS_A_NAN ( n )
  int * n ;
{
  *n = 0xffc00000 ;
#if 0
*n = 0. ;
fprintf(stderr,"WHAT_IS_NAN disabled\n") ;
#endif
}

/* SUBROUTINE wrf_mem_copy( a, b, n )
   INTEGER*1, INTENT (INOUT) :: a(*), b(*)
   INTEGER,   INTENT (IN)    :: n
   INTEGER                   :: i
   DO i = 1, n
     b(i) = a(i)
    ENDDO
    RETURN
    END SUBROUTINE wrf_mem_copy */

void
WRF_MEM_COPY ( a , b, n )
  char * a ; 
  char * b ; 
  int * n ;
{
  int i ;
  for ( i = 0 ; i < *n ; i++ )
  {
    *b++ = *a++ ;
  }
}

