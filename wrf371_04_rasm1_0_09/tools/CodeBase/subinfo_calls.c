#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define COMPARE(A,B) ( ! strncmp ( A , B , strlen( B ) ) )
#define COMPARE2(A,B) ( ! strcmp ( A , B ) )
#define INLINELEN (4*8192)
#define VARLEN 128
#define MAXARGS (4*8192)
#define MAXDEPTH 50

#define DIR "tools/code_dbase"

char levels[MAXDEPTH][VARLEN] ;

main( int argc, char *argv[] )
{
  int indent ;
  int i ;
  char *rout ;
  indent = atoi( argv[1] ) ;
  rout = argv[2] ; 
  for ( i = 0 ; i < MAXDEPTH ; i++ ) strcpy( levels[i] , "" ) ;
  subinfo_calls ( rout, indent ) ;
  
}

subinfo_calls ( char *rout, int indent ) 
{
  FILE *CALLERS ;
  FILE *CALLER ;
  char inln[INLINELEN], inln2[INLINELEN] ;
  int i ;
  char fname[VARLEN], fname2[VARLEN], sf[VARLEN] ;
  char u0[VARLEN] , u1[VARLEN] , u2[VARLEN] ;
  char v0[VARLEN] , v1[VARLEN] , v2[VARLEN] ;
  char u0_upper[VARLEN] ;

  if ( COMPARE( rout, "add_msg_" ) ||
       COMPARE( rout, "wrf_error" ) ||
       COMPARE( rout, "wrf_debug" ) ||
       COMPARE( rout, "wrf_message" ) ||
       COMPARE( rout, "stencil_" ) ||
       COMPARE( rout, "start_timing" ) ||
       COMPARE( rout, "end_timing" ) ) exit(0) ;

  sprintf(fname, "%s/calls", DIR ) ;
  if (( CALLERS = fopen( fname, "r" )) == NULL ) { fprintf(stderr,"subinfo_calls: cannot open %s\n",fname) ; exit(1) ; }

  while ( fgets( inln, INLINELEN, CALLERS ) != NULL ) {
    get_token_n ( inln, " ", 0, u0 ) ; remove_nl(u0) ;
    get_token_n ( inln, " ", 1, u1 ) ; remove_nl(u1) ;
    get_token_n ( inln, " ", 2, u2 ) ; remove_nl(u2) ;
    if ( COMPARE2( rout, u2 ) && ! COMPARE( u2 , u0 ) ) {
      sprintf( fname2 , "%s/%s", DIR, u0 ) ;
      if (( CALLER = fopen( fname2, "r" )) == NULL ) { fprintf(stderr,"subinfo_calls: cannot open %s\n",fname2 ) ; exit(2) ; }
      while ( fgets( inln2 , INLINELEN, CALLER ) != NULL ) {
        get_token_n ( inln2, " ", 0, v0 ) ; remove_nl ( v0 ) ;
        get_token_n ( inln2, " ", 1, v1 ) ; remove_nl ( v1 ) ;
        if ( COMPARE2( v0, "sourcefile" ) ) { strcpy ( sf , v1 ) ; break ; }
      }
      fclose(CALLER) ;
      for ( i = 0 ; i < indent * 3 ; i++ ) {
        printf( "&nbsp " ) ;
      }
      switch_little_big_f( sf ) ;
#if 0
      printf("<a href=\"%s.html\">%s</a> (<a href=\"../../%s\">%s</a>)<br>\n", u0, u0, sf, sf )  ;
#else
      strcpy( u0_upper, u0 ) ; upper_case_str( u0_upper ) ;
      printf("<a href=\"%s.html\">%s</a> (<a href=\"../../../wrfbrowser/html_code/%s.html#%s\">%s</a>)<br>\n", u0, u0, sf, u0_upper,sf )  ;
#endif
      /* RECURSION */
      if ( ! COMPARE2( u0 , levels[ indent+1 ] ) ) {
        strcpy( levels[ indent+1 ], u0 ) ;
        subinfo_calls ( u0 , indent + 1 ) ;
      }
    }
  }
  fclose( CALLERS ) ;
}


