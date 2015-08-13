#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
# include <strings.h>
#endif

#include "protos.h"
#include "registry.h"
#include "data.h"

int
gen_model_data_ord ( char * dirname )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char * fn = "model_data_order.inc" ;
  int i ;

  if ( dirname == NULL ) return(1) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  fprintf(fp,"INTEGER , PARAMETER :: model_data_order   = DATA_ORDER_") ;
  for ( i = 0 ; i < 3 ; i++ )
  {
    switch ( model_order[i] )
    {
    case ( COORD_X ) : fprintf(fp,"X") ; break ;
    case ( COORD_Y ) : fprintf(fp,"Y") ; break ;
    case ( COORD_Z ) : fprintf(fp,"Z") ; break ;
    default : fprintf(stderr,"Model data order ambiguous. Is there a dimspec for all three coordinate axes?\n") ;  break ;
    }
  }
  fprintf(fp,"\n") ;
  close_the_file( fp ) ;
  return(0) ;
}

