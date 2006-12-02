#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "protos.h"
#include "registry.h"
#include "data.h"

#define DUMMY 1
#define ACTUAL 2 
#define DUMMY_NEW 3
#define ACTUAL_NEW 4 

int
gen_actual_args ( char * dirname )
{
  int i ;
  
  for ( i = 0 ; i < get_num_cores() ; i++ )
    gen_args ( dirname , get_corename_i(i) , ACTUAL ) ; 
  return(0) ;
}

/* only generate actual args for the 4d arrays */
int
gen_actual_args_new ( char * dirname )
{
  int i ;

  for ( i = 0 ; i < get_num_cores() ; i++ )
    gen_args ( dirname , get_corename_i(i) , ACTUAL_NEW ) ;
  return(0) ;
}

int
gen_dummy_args ( char * dirname )
{
  int i ;
 
  for ( i = 0 ; i < get_num_cores() ; i++ )
    gen_args ( dirname , get_corename_i(i) , DUMMY ) ;
  return(0) ;
}

/* only generate dummy args for the 4d arrays */
int
gen_dummy_args_new ( char * dirname )
{
  int i ;

  for ( i = 0 ; i < get_num_cores() ; i++ )
    gen_args ( dirname , get_corename_i(i) , DUMMY_NEW ) ;
  return(0) ;
}

int
gen_args ( char * dirname , char * corename , int sw )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char * fn = "_args.inc" ;
  char * p ;
  int linelen ;
  char outstr[64*4096] ;

  if ( dirname == NULL || corename == NULL ) return(1) ;
  if ( strlen(dirname) > 0 ) 
   { sprintf(fname,"%s/%s%s%s%s",dirname,corename,
             (sw==ACTUAL||sw==ACTUAL_NEW)?"_actual":"_dummy",(sw==ACTUAL_NEW||sw==DUMMY_NEW)?"_new":"",fn) ; }
  else                       
   { sprintf(fname,"%s%s%s%s",corename,
             (sw==ACTUAL||sw==ACTUAL_NEW)?"_actual":"_dummy",(sw==ACTUAL_NEW||sw==DUMMY_NEW)?"_new":"",fn) ; }

  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  linelen = 0 ;
  strcpy(outstr,",") ;
  gen_args1 ( fp , outstr, (sw==ACTUAL||sw==ACTUAL_NEW)?"grid%":"", corename ,
              &Domain , &linelen , sw , 0 ) ;
  /* remove trailing comma */
  if ((p=rindex(outstr,','))!=NULL) *p = '\0' ;
  fputs(outstr,fp);fputs(" &\n",fp) ;
  close_the_file( fp ) ;
  return(0) ;
}

int
gen_args1 ( FILE * fp , char * outstr , char * structname , char * corename , 
            node_t * node , int *linelen , int sw , int deep )
{
  node_t * p ;
  int tag ;
  char post[NAMELEN] ;
  char fname[NAMELEN] ;
  char x[NAMELEN], y[NAMELEN] ;
  char indices[NAMELEN] ;
  int lenarg ; 
  int only4d = 0 ;

  if ( sw == ACTUAL_NEW ) { sw = ACTUAL ; only4d = 1 ; }
  if ( sw == DUMMY_NEW )  { sw = DUMMY  ; only4d = 1 ; }

  if ( node == NULL ) return(1) ;
  for ( p = node->fields ; p != NULL ; p = p->next )
  {
    if ( p->node_kind & I1 ) continue ;              /* short circuit any field that is not state */
                                                     /* short circuit scalars; shortening argument lists */
    if ( p->ndims == 0 && p->type->type_type != DERIVED && sw_limit_args ) continue ; 

    if (                 (
          (p->node_kind & FOURD)                   /* scalar arrays or... */
                                                   /* if it's a core specific field and we're doing that core or... */
       || (p->node_kind & FIELD && (!strncmp("dyn_",p->use,4)&&!strcmp(corename,p->use+4))) 
                                                   /* it is not a core specific field and it is not a derived type -ajb */
       || (p->node_kind & FIELD && (p->type->type_type != DERIVED) && ( strncmp("dyn_",p->use,4))) 
#if 0
                                                   /* it is a state variable */
       || (p->node_kind & RCONFIG )
#endif
                         )
       )
    {
      if ( !only4d || (p->node_kind & FOURD) || ( p->boundary_array ) ) {
        if      ( p->node_kind & FOURD ) { sprintf(post,",1)") ; }
        else if ( p->boundary_array )     { sprintf(post,")") ; }
        else                              { sprintf(post,")") ; }
        for ( tag = 1 ; tag <= p->ntl ; tag++ )
        {
          /* if this is a core-specific variable, prepend the name of the core to */
          /* the variable at the driver level */
          if (!strcmp( corename , p->use+4 ) && sw==ACTUAL)
            sprintf(fname,"%s_%s",corename,field_name(t4,p,(p->ntl>1)?tag:0)) ;
          else
            strcpy(fname,field_name(t4,p,(p->ntl>1)?tag:0)) ;
	  strcpy(indices,"") ;
          if ( sw_deref_kludge && sw==ACTUAL ) 
	    sprintf(indices, "%s",index_with_firstelem("(","",t2,p,post)) ;
          /* generate argument */
	  strcpy(y,structname) ; strcat(y,fname) ; strcat(y,indices) ; strcat(y,",") ;
	  lenarg = strlen(y) ;
	  if ( lenarg+*linelen > MAX_ARGLINE ) { strcat(outstr," &\n") ; *linelen = 0 ; }
	  strcat(outstr,y) ;
	  *linelen += lenarg ;
        }
      }
    }
    if ( p->type != NULL )
    {
      if ( p->type->type_type == DERIVED && !only4d )
      {
        if ( deep )
        {
          sprintf(x,"%s%s%%",structname,p->name ) ;
          gen_args1(fp, outstr, (sw==ACTUAL)?x:"", corename, p->type,linelen,sw,deep) ;
        }
        else
        {
          /* generate argument */
	  strcpy(y,structname) ; strcat(y,p->name) ; strcat(y,",") ;
	  lenarg = strlen(y) ;
	  if ( lenarg+*linelen > MAX_ARGLINE ) { strcat(outstr," &\n") ; *linelen = 0 ; }
	  strcat(outstr,y) ;
	  *linelen += lenarg ;
          p->mark = 1 ;
        }
      }
    }
  }
  return(0) ;
}

