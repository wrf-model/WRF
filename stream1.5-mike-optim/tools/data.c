#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#define rindex(X,Y) strrchr(X,Y)
#define index(X,Y) strchr(X,Y)
#define bzero(X,Y) memset(X,0,Y)
#else
#  include <strings.h>
#endif

#include "registry.h"
#include "protos.h"
#include "data.h"

#define MAXTOKENS 30

int
init_dim_table()
{
  Dim = NULL ;
  return(0) ;
}

node_t * 
new_node ( int kind )
{ node_t *p ; p = (node_t *)malloc(sizeof(node_t)) ; bzero(p,sizeof(node_t)); p->node_kind = kind ; return (p) ; }

int
add_node_to_end ( node_t * node , node_t ** list )
{
  node_t * p ;
  if ( *list == NULL ) 
    { *list = node ; }
  else
  {
    for ( p = *list ; p->next != NULL ; p = p->next ) ;
    p->next = node ;
  }
  return(0) ;
}

int
add_node_to_end_4d ( node_t * node , node_t ** list )
{
  node_t * p ;
  if ( *list == NULL ) 
    { *list = node ; }
  else
  {
    for ( p = *list ; p->next4d != NULL ; p = p->next4d ) ;
    p->next4d = node ;
  }
  return(0) ;
}

#if 0
int
show_nodelist( node_t * p )
{
  show_nodelist1( p , 0 ) ;
}

show_nodelist1( node_t * p , int indent )
{
  if ( p == NULL ) return(0) ;
  show_node1( p, indent) ;
  show_nodelist1( p->next, indent ) ;
}

int
show_node( node_t * p )
{
  return(show_node1(p,0)) ;
}

int
show_node1( node_t * p, int indent )
{
  char spaces[] = "                           " ;
  char tmp[25] , t1[25] , t2[25] ;
  char * x, *ca, *ld, *ss, *se, *sg ;
  char *nodekind ;
  node_t * q ;
  int nl ;
  int i ;

  if ( p == NULL ) return(1) ;
  strcpy(tmp, spaces) ;
  if ( indent >= 0 && indent < 20 ) tmp[indent] = '\0' ;

// this doesn't make much sense any more, ever since node_kind was 
// changed to a bit mask
  if      ( p->node_kind & RCONFIG ) nodekind = "RCONFIG" ; 
  else if ( p->node_kind & I1      ) nodekind = "I1" ;
  else if ( p->node_kind & FIELD   ) nodekind = "FIELD" ;
  else if ( p->node_kind & FOURD   ) nodekind = "FOURD" ;
  else if ( p->node_kind & MEMBER  ) nodekind = "MEMBER" ;
  else if ( p->node_kind & RCONFIG ) nodekind = "RCONFIG" ;

  if ( !p->scalar_array_member ) 
  {
  switch ( p->node_kind )
  {
  case RCONFIG :
  case I1      :
  case FIELD   :
  case FOURD   :
  case MEMBER  :
    fprintf(stderr,"%s%s : %10s ndims %1d\n",tmp,nodekind,p->name, p->ndims) ;
    for ( i = 0 ; i < p->ndims ; i++ )
    {
      sg = "" ;
      switch ( p->dims[i]->coord_axis ) {
	case COORD_X : ca = "X" ; if ( p->stag_x ) sg = "*" ; break ;
        case COORD_Y : ca = "Y" ; if ( p->stag_y ) sg = "*" ; break ;
	case COORD_Z : ca = "Z" ; if ( p->stag_z ) sg = "*" ; break ;
        case COORD_C : ca = "C" ; break ;
      }
      switch ( p->dims[i]->len_defined_how ) {
	case DOMAIN_STANDARD : ld = "STANDARD" ;  ss = "" ; se = "" ; break ;
	case NAMELIST        : ld = "NAMELIST" ;  ss = p->dims[i]->associated_namelist_variable ; se="" ; break ;
	case CONSTANT        : ld = "CONSTANT" ;  sprintf(t1,"%d",p->dims[i]->coord_start) ; ss = t1 ;
						  sprintf(t2,"%d",p->dims[i]->coord_end  ) ; se = t2 ;
						  break ;
      }
      fprintf(stderr,"      dim %1d: %c %2s%s %10s %10s %10s\n",i,p->dims[i]->dim_name,ca,sg,ld,ss,se) ;
    }
    nl = 0 ;
    if ( strlen( p->use     ) > 0 ) {
       nl = 1 ; fprintf(stderr,"      use: %s",p->use) ;
       if ( p->scalar_array_member ) fprintf(stderr,"(4D)") ;
    }
    if ( strlen( p->dname   ) > 0 ) { nl = 1 ; fprintf(stderr,"    dname: %s",p->dname) ;    }
    if ( strlen( p->descrip ) > 0 ) { nl = 1 ; fprintf(stderr,"  descrip: %s",p->descrip) ;    }
    if ( nl == 1 ) fprintf(stderr,"\n") ;
    show_node1( p->type, indent+1 ) ;
    break ;
  case TYPE  :
    x = "derived" ;
    if ( p->type_type == SIMPLE ) x = "simple" ;
    fprintf(stderr,"%sTYPE : %10s %s ndims %1d\n",tmp,p->name,x, p->ndims) ;
    show_nodelist1( p->fields, indent+1 ) ;
    break ;
  case DIM   :
    break ;
  default :
    break ;
  }
  }
  show_nodelist1( p->members , indent+2 ) ;
  return(0) ;
}
#endif

int
set_mark ( int val , node_t * lst )
{
  node_t * p ;
  if ( lst == NULL ) return(0) ;
  for ( p = lst ; p != NULL ; p = p->next )
  {
    p->mark = val ;
    set_mark( val , p->fields ) ;
    set_mark( val , p->members ) ;
  }
  return(0) ;
}

int
set_mark_4d ( int val , node_t * lst )
{
  node_t * p ;
  if ( lst == NULL ) return(0) ;
  for ( p = lst ; p != NULL ; p = p->next4d )
  {
    p->mark = val ;
    set_mark( val , p->fields ) ;
    set_mark( val , p->members ) ;
  }
  return(0) ;
}

