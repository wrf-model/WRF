#ifndef MS_SUA
# include <stdio.h>
# include <stdlib.h>
#endif

/*
   bitwise OR operator called from Fortran, not
   assuming that the C return will be validly
   interpreted in Fortran
*/

#ifndef CRAY
# ifdef NOUNDERSCORE
#      define BITWISE_OR  bitwise_or
# else
#   ifdef F2CSTYLE
#      define BITWISE_OR  bitwise_or__
#   else
#      define BITWISE_OR  bitwise_or_
#   endif
# endif
#endif

void BITWISE_OR ( int * a , int * b , int * c ) 
{
   *c = *a | *b;
}
