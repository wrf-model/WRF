#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LINELEN 8192
#define STRINGLEN 1024
int drop_comment( char *);
int change_to_lower( char *, int );

int
main( int argc , char *argv[] )
{
  FILE *fp ;
  char linei[LINELEN] ;
  char lineo[8*LINELEN] ;
  char wrf_error_fatal_str[STRINGLEN] ;
  char surface_driver_str[STRINGLEN] ;
  char radiation_driver_str[STRINGLEN] ;
  char cumulus_driver_str[STRINGLEN] ;
  char pbl_driver_str[STRINGLEN] ;
  char *p, *q, *r ;
  char firstp ;
  int state, ns, ns2 ;
  int inparen ;
  int inacall ;
  int col ;

  if ( argc != 2 ) {
     fprintf(stderr,"usage: %s file\n",argv[0]) ;
  }
  if ((fp = fopen(argv[1],"r"))==NULL) {
    fprintf(stderr,"cannot open %s for reading\n",argv[1]) ;
    exit(2) ;
  }
  while ( fgets( linei, LINELEN, fp ) != NULL ) {
    lineo[0] = '\0' ;
    if ( linei[0] != '#' ) drop_comment( linei ) ;
    inacall = 0 ;
    for ( p = linei, q = lineo , firstp = *p ; *p ; p++ ) {
      if ( !inacall && (*(p+0) == 'c' || *(p+0) == 'C' ) && 
           (*(p+1) == 'a' || *(p+1) == 'A' ) &&
           (*(p+2) == 'l' || *(p+2) == 'L' ) &&
           (*(p+3) == 'l' || *(p+3) == 'L' ) && firstp != '#' )
      {
        inacall = 1 ;
        strncpy(q,p,4) ; q+=4 ;
        ns = 1 ; while (  *(p+3+ns) && *(p+3+ns) != '\n' &&
                         (*(p+3+ns) == ' ' || 
                          *(p+3+ns) == '\t' )) { *q++ = *(p+3+ns) ; ns++ ; }

        strncpy(wrf_error_fatal_str,  p+3+ns,15+1) ; change_to_lower(wrf_error_fatal_str,15+1 ) ; /* 15, but add one to check for '3' */
        strncpy(surface_driver_str,   p+3+ns,14)   ; change_to_lower(surface_driver_str,14) ;
        strncpy(radiation_driver_str, p+3+ns,16)   ; change_to_lower(radiation_driver_str,16) ;
        strncpy(cumulus_driver_str,   p+3+ns,14)   ; change_to_lower(cumulus_driver_str,14) ;
        strncpy(pbl_driver_str,       p+3+ns,10)   ; change_to_lower(pbl_driver_str,10) ;

        if ( !strncmp( wrf_error_fatal_str, "wrf_error_fatal", 15 ) && wrf_error_fatal_str[15] != '3' )
        {
          ns2 = 1 ; while ( *(p+3+ns+14+ns2) && *(p+3+ns+14+ns2) != '\n' &&
                           (*(p+3+ns+14+ns2) == ' ' ) ) ns2++ ;
          if ( *(p+3+ns+14+ns2) == '(' ) {
             *q='\0';
             printf("%s",lineo) ;
             printf("wrf_error_fatal3(__FILE__,__LINE__,&\n") ;
             ns2 = 1 ; while ( *(p+3+ns+14+ns2) && *(p+3+ns+14+ns2) != '\n' &&
                              (*(p+3+ns+14+ns2) == ' ' || 
                               *(p+3+ns+14+ns2) == '(' ||
                               *(p+3+ns+14+ns2) == '\t' ||
                               *(p+3+ns+14+ns2) == '&' )) ns2++ ;
             if( *(p+3+ns+14+ns2) != '\n') printf("%s",(p+3+ns+14+ns2)) ;
             goto next_line ;
          } else {
             printf("%s",linei) ;
             goto next_line ;
          }
        } else if ( !strncmp ( surface_driver_str,   "surface_driver", 14 )  ||
                    !strncmp ( radiation_driver_str, "radiation_driver", 16) ||
                    !strncmp ( cumulus_driver_str,   "cumulus_driver", 14)   ||
                    !strncmp ( pbl_driver_str,       "pbl_driver", 10)
                  ) {
          strcpy(lineo,p+3+ns) ;
          inparen = 1 ;
          while ( fgets( linei, LINELEN, fp ) != NULL ) {
            for ( q = linei ; *q ; q++ ) {
              if (*q=='!') { *q = '\n' ; *(q+1) = '\0' ; break ; }
            }
            for ( q = linei ; *q ; q++ ) {
              if      ( *q == '(' ) inparen++ ;
              else if ( *q == ')' ) inparen-- ;
            }
            strcat(lineo,linei) ;
            if ( inparen == 0 ) {
              break ;
            }
          }
          for(q=lineo,r=lineo;*q;q++) {
            if (*q == '#' && *(q-1) == '\n') { /* CPP def. copy as is*/
              *r++ = '&' ;
              *r++ = '\n' ;
              for (; *q; q++) {
                 *r++ = *q; 
                 if ( *q == '\n' ) break ;
              }
            }
            if ( *q == ' ' || *q == '\n' || *q == '&' ) continue ;
            *r++ = *q ;
          }
          *r = '\0' ;
          printf("CALL ") ;
          for(q=lineo,col=130-5;*q;q++) {
            putchar(*q) ;
            if ( *q == '\n' ) { if (*(q+1) != '#') { putchar('&') ; } ; col = 131 ; }
            col-- ;
            if ( col <= 0 ) {
              col = 130 ;
              if (*q!=')' || *(q+1) ) {  putchar('&') ; putchar('\n') ; putchar('&') ; }
            }
          }
          putchar('\n') ;
          goto next_line ;
        } else {
          p += 3+ns ;
          *q++ = *p ;
        }
      } else {
        *q++ = *p ;
      }
    }
    *q='\0';
    printf("%s",lineo) ;
next_line:
    state = 0 ;
  }
  fclose(fp) ;
  exit(0) ;
}

int
drop_comment( char * linei )
{
  char *p, *q ;
  char inquote = '\0' ;

  for ( p = linei ; *p ; p++ )
  {
    if ( *p == '\'' ) { if ( inquote == *p ) { inquote = '\0' ; } else { inquote = *p ; } }
    if ( *p == '"' )  { if ( inquote == *p ) { inquote = '\0' ; } else { inquote = *p ; } }
    if ( !inquote && *p == '!' ) { 
       /* let us make sure this is not an OMP directive shall we? */
       for ( q = p ; *q ; q++ ) {
         if ((*q == '$') && 
            (*(q+1) == 'o' || *(q+1) == 'O') &&
            (*(q+2) == 'm' || *(q+2) == 'M') &&
            (*(q+3) == 'p' || *(q+3) == 'P') )  return(0) ;
       /* nor an intel compiler directive, what? */
         if ((*(q+3) == '$') &&
            (*(q)   == 'd' || *(q)   == 'D') &&
            (*(q+1) == 'e' || *(q+1) == 'E') &&
            (*(q+2) == 'c' || *(q+2) == 'C') )  return(0) ;
       /* nor an intel compiler directive, just so. quite. */
         if ((*(q+3) == '$') &&
            (*(q)   == 'd' || *(q)   == 'D') &&
            (*(q+1) == 'i' || *(q+1) == 'I') &&
            (*(q+2) == 'r' || *(q+2) == 'R') )  return(0) ;
       /* nor a pgi accelerator directive */
         if ((*q == '$') &&
            (*(q+1) == 'a' || *(q+1) == 'A') &&
            (*(q+2) == 'c' || *(q+2) == 'C') &&
            (*(q+3) == 'c' || *(q+3) == 'C') )  return(0) ;
       /* nor a fujitsu compiler directive */
         if ((*(q+1) == 'o' || *(q+1) == 'O') &&
            (*(q+2) == 'c' || *(q+2) == 'C') &&
            (*(q+3) == 'l' || *(q+3) == 'L') )  return(0) ;
       }
       *p = '\n' ; *(p+1) = '\0' ; return(0) ; 
    }
  }
  return 0; /* SamT: bug fix: return a value */
}

int 
change_to_lower( char * s , int n ) 
{
  int i ;
  for ( i = 0 ; i < n ; i++ )
  {
    if ( s[i] >= 'A' && s[i] <= 'Z' ) s[i] = s[i] - 'A' + 'a' ;
  }
  return 0; /* SamT: bug fix: return a value */
}

