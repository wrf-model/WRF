#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "registry.h"
#include "protos.h"
#include "protos_kpp.h"
#include "data.h"
#include "kpp_data.h"


knode_t * 
new_knode ( int * kind )
{ knode_t *p ; p = (knode_t *)malloc(sizeof(knode_t)) ; bzero(p,sizeof(knode_t)); return (p) ; }

int
add_knode_to_end ( knode_t * knode , knode_t ** list )
{
  knode_t * p ;
  if ( *list == NULL ) 
    { *list = knode ; }
  else
  {
    for ( p = *list ; p->next != NULL ; p = p->next ) ;
    p->next = knode ;
  }
  return(0) ;
}



