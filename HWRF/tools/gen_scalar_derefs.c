#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
# include <strings.h>
#endif

#include "protos.h"
#include "registry.h"
#include "data.h"

#define DUMMY 1
#define ACTUAL 2 

int
gen_scalar_derefs ( char * dirname )
{
  scalar_derefs ( dirname  ) ; 
  return(0) ;
}

#define DIR_COPY_OUT 1
#define DIR_COPY_IN  2

int
scalar_derefs ( char * dirname )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char * fn = "scalar_derefs.inc" ;
  char * p ;
  int linelen ;
  /* Had to increase size for SOA from 64*4096 to 64*7000, Manish Shrivastava 2010 */
  char outstr[64*7000] ;

  if ( dirname == NULL ) return(1) ;
  if ( strlen(dirname) > 0 ) 
   { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       
   { sprintf(fname,"%s",fn) ; }

  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  fprintf(fp,"! BEGIN SCALAR DEREFS\n") ;
  linelen = 0 ;
  if ( sw_limit_args ) {
    fprintf(fp,"#undef CPY\n") ;
    fprintf(fp,"#undef CPYC\n") ;
    fprintf(fp,"#ifdef COPY_OUT\n") ;
    scalar_derefs1 ( fp , &Domain, DIR_COPY_OUT ) ;
    fprintf(fp,"#else\n") ;
    scalar_derefs1 ( fp , &Domain, DIR_COPY_IN ) ;
    fprintf(fp,"#endif\n") ;
  }
  fprintf(fp,"! END SCALAR DEREFS\n") ;
  close_the_file( fp ) ;
  return(0) ;
}

int
scalar_derefs1 ( FILE * fp , node_t * node, int direction )
{
  node_t * p ;
  int tag ;
  char fname[NAMELEN] ;

  if ( node == NULL ) return(1) ;
  for ( p = node->fields ; p != NULL ; p = p->next )
  {
    if ( p->node_kind & I1 ) continue ;              /* short circuit any field that is not state */
                                                     /* short circuit DERIVED types */
    if ( p->type->type_type == DERIVED ) continue ;
                                                     /* short circuit non-scalars */
    if ( p->ndims > 0 ) continue ; 

    if (                 (
          (p->node_kind & FIELD )
                                                   /* it is not a derived type -ajb */
       || (p->node_kind & FIELD && (p->type->type_type != DERIVED) )
                         )
       )
    {
      for ( tag = 1 ; tag <= p->ntl ; tag++ )
      {
        strcpy(fname,field_name(t4,p,(p->ntl>1)?tag:0)) ;
        /* generate deref */
        if ( direction == DIR_COPY_OUT ) {
          fprintf(fp, " grid%%%s    = %s\n",fname,fname ) ;
        } else {
          fprintf(fp, " %s = grid%%%s\n",fname,fname ) ;
        }
      }
    }
  }
  return(0) ;
}

