#include <stdio.h>
#include "registry.h"
#include "protos.h"
#include "ctype.h"


/* work sort of like strtok but mind quote chars */
static char * tokpos = NULL ;
char *
my_strtok( char * s1 )
{
  char *p, *retval ;
  int state ;
  state = 0 ;
  retval = NULL ;
  if ( s1 == NULL && tokpos == NULL ) return( NULL ) ;
  if ( s1 != NULL ) tokpos = s1 ;
  for ( p = tokpos ; *p ; p++ )
  {
    if ( state == 0 && (*p == ' ' || *p == '\t') ) continue ;
    if ( state == 0 && !(*p == ' ' || *p == '\t') ) { state = 1 ; retval = p ; } ;
    if      ( state == 1 && (*p == '"') ) { state = 2 ; }
    else if ( state == 2 && (*p == '"') ) { state = 1 ; }
    if ( state == 1 && (*p == ' ' || *p == '\t') ) { *p = '\0' ; p++ ; break ; }
  }
  tokpos = p ;
  return( retval ) ;
}


/* posix like rentrant strtok; not quote safe, and not quite strtok -- new version; skips multi delims  */
char *
strtok_rentr( char * s1 , char * s2, char ** tokpos )
{
  char *p, *q, *retval ;
  int match ;
  retval = NULL ;
  if ( s1 == NULL && s2 == NULL ) return( NULL ) ;
  if ( s1 != NULL ) { *tokpos = s1 ; }
  if ( **tokpos ) retval = *tokpos ;
  for ( p = *tokpos ; *p ; p++ )
  {
    for ( q = s2 ; *q ; q++ )
    {
      if ( *p == *q ) { *p = '\0' ; p++ ; goto foundit  ; }
    }
  }
foundit:
/* skip over multi-delims */
  for ( ; *p ; p++ )
  {
    match = 0 ;
    for ( q = s2 ; *q ; q++ )
    {
      if ( *p == *q ) { *p = '\0' ; match++  ; }
    }
    if ( match == 0 ) { break ; }
  }
  *tokpos = p ;
  return( retval ) ;
}

#if 0
/* posix like rentrant strtok; not quote safe, and not quite strtok -- won't skip over multiple delims  */
char *
strtok_rentr( char * s1 , char * s2, char ** tokpos )
{
  char *p, *q, *retval ;
  retval = NULL ;
  if ( s1 == NULL && s2 == NULL ) return( NULL ) ;
  if ( s1 != NULL ) { *tokpos = s1 ; }
  if ( **tokpos ) retval = *tokpos ;
  for ( p = *tokpos ; *p ; p++ )
  {
    for ( q = s2 ; *q ; q++ )
    {
      if ( *p == *q ) { *p = '\0' ; p++ ; goto foundit  ; }
    }
  }
foundit:
  *tokpos = p ;
  return( retval ) ;
}
#endif

int
make_lower( char * s1 )
{
  char * p ;
  int state ;
  state = 0 ;
  for ( p = s1 ; *p ; p++ )
  {
    if      ( state == 0 && *p == '"' ) state = 1 ;
    else if ( state == 1 && *p == '"' ) state = 0 ;
    if ( state == 0 )
    {
      *p = tolower(*p) ;
    }
  }
  return(0) ;
}
