#include <stdio.h>
#ifdef UNDERSCORE 
#  define foo foo_
#endif
void foo( void )
{
  printf( "Hello World!\n" );
}
