#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "protos.h"
#include "registry.h"
#include "data.h"

enum sw_ranges { COLON_RANGE , ARGADJ , GRIDREF } ;
enum sw_pointdecl { POINTERDECL , NOPOINTERDECL } ;

int
gen_state_struct ( char * dirname )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char * fn = "state_struct.inc" ;

  strcpy( fname, fn ) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_decls ( fp , "", &Domain , COLON_RANGE , POINTERDECL , FIELD | RCONFIG | FOURD , DRIVER_LAYER ) ;
  close_the_file( fp ) ;
  return(0) ;
}

int
gen_state_subtypes ( char * dirname )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char * fn = "state_subtypes.inc" ;

  strcpy( fname, fn ) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }

  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_state_subtypes1( fp , &Domain , COLON_RANGE , POINTERDECL , FIELD | RCONFIG | FOURD ) ;
  close_the_file(fp) ;
  return(0) ;
}

int
gen_dummy_decls ( char * dn )
{
  int i ;
  FILE * fp ;
  char fname[NAMELEN] ;
  char corename[NAMELEN] ;
  char * fn = "_dummy_decl.inc" ;

  if ( dn == NULL ) return(1) ;
  for ( i = 0 ; i < get_num_cores() ; i++ )
  {
    strcpy( corename , get_corename_i(i) ) ;
    if ( strlen(dn) > 0 ) { sprintf(fname,"%s/%s%s",dn,corename,fn) ; }
    else                  { sprintf(fname,"%s%s",corename,fn) ; }
    if ((fp = fopen( fname , "w" )) == NULL ) continue ;
    print_warning(fp,fname) ;
    gen_decls ( fp, corename, &Domain , GRIDREF , NOPOINTERDECL , FIELD | RCONFIG | FOURD , MEDIATION_LAYER ) ;
    fprintf(fp,"#undef COPY_IN\n") ;
    fprintf(fp,"#undef COPY_OUT\n") ;
    close_the_file( fp ) ;
  }
  return(0);
}

int
gen_i1_decls ( char * dn )
{
  int i ;
  FILE * fp ;
  char  fname[NAMELEN], post[NAMELEN] ;
  char * fn = "_i1_decl.inc" ;
  char corename[NAMELEN] ;
  char * dimspec ;
  node_t * p ; 

  if ( dn == NULL ) return(1) ;
  for ( i = 0 ; i < get_num_cores() ; i++ )
  {
    strcpy(corename,get_corename_i(i)) ;
    if ( strlen(dn) > 0 ) { sprintf(fname,"%s/%s%s",dn,corename,fn) ; }
    else                  { sprintf(fname,"%s%s",corename,fn) ; }
    if ((fp = fopen( fname , "w" )) == NULL ) continue ;
    print_warning(fp,fname) ;
    gen_decls ( fp , corename, &Domain , GRIDREF , NOPOINTERDECL , I1 , MEDIATION_LAYER ) ;

    /* now generate tendencies for 4d vars if specified  */
    for ( p = FourD ; p != NULL ; p = p->next )
    {
      if ( p->node_kind & FOURD && p->has_scalar_array_tendencies )
      {
	sprintf(fname,"%s_tend",p->name) ;
        sprintf(post,",num_%s)",p->name) ;
	dimspec=dimension_with_ranges( "grid%",",DIMENSION(",t2,p,post,"" ) ;
        /*          type dim pdecl   name */
        fprintf(fp, "%-10s%-20s%-10s :: %s\n",
                    field_type( t1, p ) ,
                    dimspec ,
                    "" ,
                    fname ) ;
      }
    }
    close_the_file( fp ) ;
  }
  return(0) ;
}

int
gen_decls ( FILE * fp , char * corename , node_t * node , int sw_ranges, int sw_point , int mask , int layer )
{
  node_t * p ; 
  int tag, ipass ;
  char fname[NAMELEN], post[NAMELEN] ;
  char * dimspec ;

  if ( node == NULL ) return(1) ;

/* make two passes; the first is for scalars, second for arrays.                     */
/* do it this way so that the scalars get declared first (some compilers complain    */
/* if a scalar is used to declare an array before it's declared)                     */

  for ( ipass = 0 ; ipass < 2 ; ipass++ ) 
  {
  for ( p = node->fields ; p != NULL ; p = p->next )
  {
    if ( p->node_kind & mask )
    {
      /* add an extra dimension to the 4d arrays.                                       */
      /* note the call to dimension_with_colons, below, does this by itself             */
      /* but dimension_with_ranges needs help (since the last arg is not just a colon)  */

      if       ( p->node_kind & FOURD ) { 
          sprintf(post,",num_%s)",field_name(t4,p,0)) ;
      } else { 
          sprintf(post,")") ;
      }

      for ( tag = 1 ; tag <= p->ntl ; tag++ ) 
      {

        /* if this is a core-specific variable, if we are generating non-driver-layer              */
        /* declarations, and if this not a variable for the core named in corename, short-circuit  */
        if (!strncmp( p->use, "dyn_", 4 ) && layer != DRIVER_LAYER && strcmp( p->use+4, corename)) continue ;

        /* if this is a core-specific variable, prepend the name of the core to                    */
        /* the variable at the driver level                                                        */
        if (!strncmp( p->use, "dyn_", 4 ) && layer == DRIVER_LAYER )
          sprintf(fname,"%s_%s",p->use+4,field_name(t4,p,(p->ntl>1)?tag:0)) ;
        else
          strcpy(fname,field_name(t4,p,(p->ntl>1)?tag:0)) ;

        switch ( sw_ranges )
        {
	  case COLON_RANGE :
	    dimspec=dimension_with_colons( ",DIMENSION(",t2,p,")" ) ; break ;
	  case GRIDREF :
	    dimspec=dimension_with_ranges( "grid%",",DIMENSION(",t2,p,post,"" ) ; break ;
	  case ARGADJ :
	    dimspec=dimension_with_ranges( "",",DIMENSION(",t2,p,post,"" ) ; break ;
        }

        if ( !strcmp( dimspec, "" ) && ipass == 1 ) continue ; /* short circuit scalars on 2nd pass  */
        if (  strcmp( dimspec, "" ) && ipass == 0 ) continue ; /* short circuit arrays on 2nd pass   */

        /*          type dim pdecl   name */
        fprintf(fp, "%-10s%-20s%-10s :: %s\n",
                    field_type( t1, p ) ,
                    dimspec ,
                    (sw_point==POINTERDECL)?declare_array_as_pointer(t3,p):"" ,
                    fname ) ;
      }
    }
  }
  }
  return(0) ;
}

int
gen_state_subtypes1 ( FILE * fp , node_t * node , int sw_ranges , int sw_point , int mask )
{
  node_t * p ;
  int i ;
  int new;
  char TypeName [NAMELEN] ;
  char tempname [NAMELEN] ;
  if ( node == NULL ) return(1) ;
  for ( p = node->fields ; p != NULL ; p = p->next )
  {
    if ( p->type != NULL )
      if ( p->type->type_type == DERIVED )
      {
        new = 1 ;    /* determine if this is a new type -ajb */
        strcpy( tempname, p->type->name ) ;
        for ( i = 0 ; i < get_num_typedefs() ; i++ )        
        { 
          strcpy( TypeName, get_typename_i(i) ) ;
          if ( ! strcmp( TypeName, tempname ) ) new = 0 ;
        }

        if ( new )   /* add this type to the history and generate declarations -ajb */
        {
          add_typedef_name ( tempname ) ;
          gen_state_subtypes1 ( fp , p->type , sw_ranges , sw_point , mask ) ;
          fprintf(fp,"TYPE %s\n",p->type->name) ;
          gen_decls ( fp , "", p->type , sw_ranges , sw_point , mask , DRIVER_LAYER ) ;
          fprintf(fp,"END TYPE %s\n",p->type->name) ;
        }
      }
  }
  return(0) ;
}

/* old version of gen_state_subtypes1 -ajb */
/*
int
gen_state_subtypes1 ( FILE * fp , node_t * node , int sw_ranges , int sw_point , int mask )
{
  node_t * p ;
  int tag ;
  if ( node == NULL ) return(1) ;
  for ( p = node->fields ; p != NULL ; p = p->next )
  {
    if ( p->type != NULL )
      if ( p->type->type_type == DERIVED )
      {
        gen_state_subtypes1 ( fp , p->type , sw_ranges , sw_point , mask ) ;
        fprintf(fp,"TYPE %s\n",p->type->name) ;
        gen_decls ( fp , "", p->type , sw_ranges , sw_point , mask , DRIVER_LAYER ) ;
        fprintf(fp,"END TYPE %s\n",p->type->name) ;
      }
  }
  return(0) ;
}
*/
