#include <stdio.h>
#include <string.h>

#define INLINELEN (4*8192)

/* if sf is a string that ends in .f, end in .F, or vice verrsa */

int switch_little_big_f ( char s[] ) 
{
  int n ;
  n = strlen(s) ;
  if      ( !strcmp( &(s[n-2]) , ".f" ) ) { s[n-1] = 'F' ; }
  else if ( !strcmp( &(s[n-2]) , ".F" ) ) { s[n-1] = 'f' ; }
  return(0) ;
}

int contains_str( char *s1, char *s2 )
{
  int i ;
  char *p, *q, *r ;
  if ( s2 == NULL || s1 == NULL ) return ( 0 ) ;
  if ( *s2 == '\0' || *s1 == '\0' ) return ( 0 ) ;
  p = s1 ;
  while ( *p ) {
    if ((r = (char *)index( p , *s2 )) == NULL ) { return( 0 ) ; }
    for ( q = s2 ; *q && *r == *q ; r++ , q++ )  ;
    if ( *q == '\0' ) return (1) ;
    p++ ;
  }
  return( 0 ) ;
}

int
find_str( char *s1, char *s2, char **strp )
{
  int i ;
  char *p, *q, *r ;
  if ( s2 == NULL || s1 == NULL ) return ( 0 ) ;
  if ( *s2 == '\0' || *s1 == '\0' ) return ( 0 ) ;
  p = s1 ;
  while ( *p ) {
    *strp = NULL ;
    if ((r = (char *)index( p , *s2 )) == NULL ) { return( 0 ) ; }
    *strp = r ;
    for ( q = s2 ; *q && *r == *q ; r++ , q++ )  ;
    if ( *q == '\0' ) return (1) ;
    p++ ;
  }
  return( 0 ) ;
}


int contains_tok( char *s1, char *s2, char *delims )
{
  char *p ;
  char tempstr[INLINELEN] ;
  int i ;

  strcpy( tempstr , s1 ) ;
  p = strtok ( tempstr, delims ) ;
  i = 0 ;
  while ( p != NULL )
  {
    if ( !strcmp ( p , s2 ) ) {  return(i) ;}
    i++ ;
    p = strtok( NULL, delims ) ;
  }
  return(0) ;
}

int
get_token_n ( char *s1, char *delims , int n, char* retval )
{
  char *p ;
  int i ;
  static char tempstr[INLINELEN] ;
  strcpy( tempstr , s1 ) ;
  p = strtok ( tempstr, delims ) ;
  i = 0 ;
  while ( p != NULL )
  {
    if ( i == n ) { strcpy( retval, p ) ; return( 1 ) ; }
    p = strtok( NULL, delims ) ;
    i++ ;
  }
  return( 0 ) ;
}

int
get_arg_n ( char *s1, int n, char *retval )
{
  char *p, *q ;
  int i, arg, inquote = -1, inparen = -1 ;
  static char tempstr[INLINELEN] ;
  char quotes[INLINELEN] ;
  char parens[INLINELEN] ;
  strcpy( tempstr , s1 ) ;
  strcpy( retval, "" ) ;

  if ( (p = index(tempstr, '(')) == NULL ) return(0) ;
  p++ ;
  arg = 0 ;
  while ( *p && arg < n+1 ) {
    q = p ;
    inquote = -1 ;
    for ( ; *p ; p++ )
    {
      if ( *p == '\'' || *p == '"' ) {
        if ( inquote >= 0 ) {
          if ( quotes[inquote] == *p ) {
            inquote-- ;
          } else {
            inquote++ ;
            quotes[inquote] = *p ;
          }
        } else {
          inquote++ ;
          quotes[inquote] = *p ;
        }
      }
      if ( inquote < 0 ) {
        if ( *p == '(' ) inparen++ ;
        if ( *p == ')' ) {
          if  ( inparen >= 0 ) { inparen-- ; }
          else                 { *p = '\0' ; arg++ ; break ;}
        }
      }
      if ( inquote < 0 && inparen < 0 ) {
        if ( *p == ',' ) { arg++ ; *p = '\0' ; p++ ; break ; }
      }
    }
  }
  if ( arg == n+1 ) {
    for ( ; *q ; q++ ) { if ( *q != ' ' && *q != '\t' ) break ; }
    strcpy( retval, q ) ;
    return (1) ;
  } else {
    strcpy( retval, "" ) ;
    return(0) ;
  }
}

int
empty ( char *s )
{
  char *p ;
  for ( p = s ; *p ; p++ )
  {
    if ( ! ( *p == ' ' || *p == '\t' || *p == '\n' ) ) return( 0 ) ;
  }
  return( 1 ) ;
}

remove_nl ( char *s )
{
  char *p ;
  if (( p = index( s , '\n' )) != NULL ) *p = '\0' ; 
}


remove_comments ( char *s )
{
  int inquote = -1 ;
  char quotes[INLINELEN] ;
  char *p ;
  for ( p = s ; *p ; p++ )
  {
    if ( *p == '\'' || *p == '"' ) { 
      if ( inquote >= 0 ) { 
        if ( quotes[inquote] == *p ) {
          inquote-- ;
        } else {
          inquote++ ; 
          quotes[inquote] = *p ;
        }
      } else {
        inquote++ ; 
        quotes[inquote] = *p ;
      }
    }
    if ( inquote < 0 ) {
      if ( *p == '!' ) { *p = '\0' ; break ; }
    }
  }
}

int
remove_chars ( char *s, char *r, char replace )
{
  int inquote = -1 ;
  int retval = 0 ;
  char quotes[INLINELEN] ;
  char *p, *q ;
  for ( p = s ; *p ; p++ )
  {
    if ( *p == '\'' || *p == '"' ) {
      if ( inquote >= 0 ) {
        if ( quotes[inquote] == *p ) {
          inquote-- ;
        } else {
          inquote++ ;
          quotes[inquote] = *p ;
        }
      } else {
        inquote++ ;
        quotes[inquote] = *p ;
      }
    }
    if ( inquote < 0 ) {
      for ( q = r ; *q ; q++ ) {
        if ( *p == *q ) { *p = replace ; retval = 1 ; }
      }
    }
  }
  return(retval) ;
}

int
remove_whitespace ( char *s )
{
  char *p, *q ;
  for ( p = s, q = s ; *p ; p++ )
  {
    if ( ! (*p == ' ' || *p == '\t') ) {
      *q++ = *p ;
    }
  }
  *q = '\0' ;
  return(0) ;
}


int
iswhite( char *s )
{
  char *p ; 
  for ( p = s ; *p ; p++ ) if ( *p != ' ' && *p != '\t' ) return(0) ;
  return(1) ;
}

int
remove_ampersands ( char *s )
{
  char * p, * q ; 
  int retval ;
  if (( p = rindex ( s, '&' )) != NULL )
  {
    if ( iswhite( p+1 ) ) retval = 1 ;
    else                retval = 0 ;
  }
  else
  {
    retval = 0 ;
  }
  remove_chars ( s , "&", ' ' ) ;
  return(retval) ;
}

lower_case_str ( char *s ) 
{
  char * p ;
  for ( p = s ; *p ; p++ )
  {
    if ( *p >= 'A' && *p <= 'Z' ) *p = *p - 'A' + 'a' ;
  }
}

upper_case_str ( char *s ) 
{
  char * p ;
  for ( p = s ; *p ; p++ )
  {
    if ( *p >= 'a' && *p <= 'z' ) *p = *p - 'a' + 'A' ;
  }
}
