#include <stdio.h>
#include <string.h>

#define INLINELEN 8092

char inln[INLINELEN] ;

int protex_state = 0 ;

main()
{
  while( fgets( inln, INLINELEN, stdin ) != NULL )
  {
    remove_nl( inln ) ;
    if ( contains_str ( inln , "<DESCRIPTION>" ) ) {
       protex_state = 1 ;
       printf("%s\n",inln) ;
       continue ;
    }
    if ( contains_str ( inln , "</DESCRIPTION>" ) ) {
       protex_state = 0 ;
       printf("%s\n",inln) ;
       continue ;
    }
    if ( ! contains_str( inln , "SCALAR DEREFS" ) && 
         ! ( contains_str( inln , "STARTOFREGISTRYGENERATEDINCLUDE" ) && contains_str( inln , "i1_decl.inc" ) ) &&
         ! protex_state ) {
      remove_comments ( inln ) ;
    }
    if ( ! protex_state ) { 
      lower_case_str ( inln ) ;
      remove_chars ( inln, ";", '\n' ) ;
    }
    if ( empty ( inln ) ) continue ;
    if ( remove_ampersands ( inln ) )
      printf("%s",inln) ;
    else
      printf("%s\n",inln) ;
  }
}

