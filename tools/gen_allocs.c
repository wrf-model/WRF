#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "protos.h"
#include "registry.h"
#include "data.h"

int
gen_alloc ( char * dirname )
{
  int i ;
  
  for ( i = 0 ; i < get_num_cores() ; i++ )
  {
    gen_alloc1( dirname , get_corename_i(i) ) ; 
  }
  return(0) ;
}

int
gen_alloc1 ( char * dirname , char * corename )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char * fn = "_allocs.inc" ;

  if ( dirname == NULL || corename == NULL ) return(1) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s%s",dirname,corename,fn) ; }
  else                       { sprintf(fname,"%s%s",corename,fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_alloc2( fp , "grid%", corename , &Domain ) ;
  close_the_file( fp ) ;
  return(0) ;
}

int
gen_alloc2 ( FILE * fp , char * structname , char * corename , node_t * node )
{
  node_t * p ;
  int tag ;
  char post[NAMELEN] ;
  char fname[NAMELEN] ;
  char x[NAMELEN] ;

  if ( node == NULL ) return(1) ;

  for ( p = node->fields ; p != NULL ; p = p->next )
  {
    if ( (p->ndims > 0 || p->boundary_array) && (  /* any array or a boundary array and...   */
          (p->node_kind & FOURD) ||                /* scalar arrays or...                    */
                                                   /* if it's a core specific field and we're doing that core or...  */
          (p->node_kind & FIELD && (!strncmp("dyn_",p->use,4)&&!strcmp(corename,p->use+4))) ||
                                                   /* it is not a core specific field        */
          (p->node_kind & FIELD && ( strncmp("dyn_",p->use,4)))
                         ))
    {
      if ( p->node_kind & FOURD ) { sprintf(post,",num_%s)",field_name(t4,p,0)) ; }
      else                        { sprintf(post,")") ; }
      for ( tag = 1 ; tag <= p->ntl ; tag++ )
      {
        /* if this is a core-specific variable, prepend the name of the core to   */
        /* the variable at the driver level                                       */
        if (!strcmp( corename , p->use+4 ))
          sprintf(fname,"%s_%s",corename,field_name(t4,p,(p->ntl>1)?tag:0)) ;
        else
          strcpy(fname,field_name(t4,p,(p->ntl>1)?tag:0)) ;

/* check for errors in memory allocation */

       fprintf(fp, "ALLOCATE(%s%s%s,STAT=ierr)\n if (ierr.ne.0) then\n CALL wrf_error_fatal ( &\n'frame/module_domain.f: Failed to allocate %s%s%s. ')\n endif\n", structname, fname,dimension_with_ranges( "", "(", t2, p, post, "model_config_rec%"), structname, fname,dimension_with_ranges( "", "(", t2, p, post, "model_config_rec%")); 

       fprintf(fp, "  %s%s=", structname , fname);
       if( p->type != NULL  &&   (!strcmp( p->type->name , "real" ) 
                               || !strcmp( p->type->name , "doubleprecision") ) )   
       /* if a real */
         fprintf(fp, "initial_data_value\n");
       else
         fprintf(fp, "0\n");

      }
    }
    if ( p->type != NULL )
    {
      if ( p->type->type_type == SIMPLE && p->ndims == 0 &&
               ((!strncmp("dyn_",p->use,4)&&!strcmp(corename,p->use+4)) || strncmp("dyn_",p->use,4)) &&
               (!strcmp(p->type->name,"integer") ||
                        !strcmp(p->type->name,"real") ||
                        !strcmp(p->type->name,"doubleprecision"))
              )
      {
          if (!strncmp( "dyn_" , p->use , 4 ))
          {
            if (!strcmp( corename , p->use+4 ))
              sprintf(fname,"%s_%s",corename,field_name(t4,p,(p->ntl>1)?tag:0)) ;
          }
          else
          {
            strcpy(fname,field_name(t4,p,(p->ntl>1)?tag:0)) ;
          }
          if( !strcmp( p->type->name , "real" ) || 
              !strcmp( p->type->name , "doubleprecision" )  )   /* if a real */
            fprintf(fp, "%s%s=initial_data_value\n",
                        structname ,
                        fname ) ;
	  else
            fprintf(fp, "%s%s=0\n",
                        structname ,
                        fname ) ;
      }
      else if ( p->type->type_type == DERIVED )
      {
        sprintf(x,"%s%s%%",structname,p->name ) ;
        gen_alloc2(fp,x, corename, p->type) ;
      }
    }
  }
  return(0) ;
}

