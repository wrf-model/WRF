#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#define rindex(X,Y) strrchr(X,Y)
#define index(X,Y) strchr(X,Y)
#else
# include <strings.h>
#endif

#include "protos.h"
#include "registry.h"
#include "data.h"

int
gen_module_state_description ( char * dirname )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char * fn = "module_state_description.F" ;

  strcpy( fname, fn ) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_module_state_description1 ( fp , &Domain ) ;
  close_the_file( fp ) ;
  return(0) ;
}

int
gen_module_state_description1 ( FILE * fp , node_t * node )
{
  node_t * p, * q ; 
  char * x ;

  if ( node == NULL ) return(1) ;

  fprintf(fp,"MODULE module_state_description\n") ;

  fprintf(fp,"  ! package constants\n") ;
  for ( p = Packages ; p != NULL ; p = p->next )
  {
    x=index(p->pkg_assoc,'=') ; x+=2 ;
    fprintf(fp,"  INTEGER, PARAMETER :: %s = %s\n",p->name,x) ;
  }
  fprintf(fp,"  ! 4D array constants\n") ;
  for ( p = FourD ; p != NULL ; p=p->next4d )
  {
    int c1 ;
    for( q = p->members, c1=0 ; q != NULL ; q=q->next, c1++ )
    {
      if ( strcmp(q->name,"-" ) ) 
      {
        fprintf(fp,"  INTEGER, PARAMETER :: PARAM_%s = %d\n",q->name,c1) ;
        fprintf(fp,"  INTEGER            ::     P_%s = 1\n",q->name) ;
        fprintf(fp,"  LOGICAL            ::     F_%s = .FALSE.\n",q->name) ;
      }
    }
    fprintf(fp,"  INTEGER, PARAMETER :: PARAM_NUM_%s = %d\n",p->name,c1) ;
    fprintf(fp,"  INTEGER            ::       NUM_%s = 1\n",p->name) ;
  }
  fprintf(fp,"  INTEGER, PARAMETER :: %-30s = %d\n", "P_XSB",1 ) ;
  fprintf(fp,"  INTEGER, PARAMETER :: %-30s = %d\n", "P_XEB",2 ) ;
  fprintf(fp,"  INTEGER, PARAMETER :: %-30s = %d\n", "P_YSB",3 ) ;
  fprintf(fp,"  INTEGER, PARAMETER :: %-30s = %d\n", "P_YEB",4 ) ;

  fprintf(fp,"  INTEGER, PARAMETER :: NUM_TIME_LEVELS = %d\n", max_time_level ) ;
  fprintf(fp,"  INTEGER , PARAMETER :: PARAM_FIRST_SCALAR = 2\n" ) ;

  fprintf(fp,"CONTAINS\n" ) ;
  fprintf(fp,"SUBROUTINE init_module_state_description\n" ) ;
  fprintf(fp,"END SUBROUTINE init_module_state_description\n" ) ;
  fprintf(fp,"END MODULE module_state_description\n") ;

  return(0) ;
}

