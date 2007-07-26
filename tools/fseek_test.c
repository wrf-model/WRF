#define _FILE_OFFSET_BITS 64
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
main()
{
  FILE *fp ;
  long long y ;
  int retval ;
  int result1 ;
#ifdef TEST_FSEEKO
  off_t x ;
  off_t result2 ;
#endif
#ifdef TEST_FSEEKO64
  long long x ;
  int result2 ;
#endif
  fp = NULL ;
  fp = fopen( "Makefile" , "r" ) ;
#ifdef TEST_FSEEKO
  x = 0xffffffff ;
  result1 = (sizeof(x) == 8) ;
  result2 = fseeko( fp, x, SEEK_SET ) ;
#endif
#ifdef TEST_FSEEKO64
  x = 0xffffffffL ;
  result1 = (sizeof(x) == 8) ;
  result2 = fseeko64( fp, x, SEEK_SET ) ;
#endif
  if ( result2 ) perror("error") ;
  fprintf(stdout,"pointer is 8 bytes: %s\n",result1?"true":"false") ;
  fprintf(stdout,"seek returns correctly: %s\n",!result2?"true":"false") ;
  if ( result1 && !result2 ) { 
    fprintf(stdout,"status: OK\n") ; 
    retval = 0 ;
  } else { 
    fprintf(stdout,"status: BUMMER\n") ;
    retval = 1 ;
  }
  fclose(fp) ;
  exit(retval) ;
}

