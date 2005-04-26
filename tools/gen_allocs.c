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
    gen_ddt_write( dirname, get_corename_i(i) ) ;
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
        if (      !strcmp( corename , p->use+4 )) {
          sprintf(fname,"%s_%s",corename,field_name(t4,p,(p->ntl>1)?tag:0)) ;
        } else if ( !strcmp ( p->use , "_4d_bdy_array_") ) {
          strcpy(fname,p->name) ;
        } else {
          strcpy(fname,field_name(t4,p,(p->ntl>1)?tag:0)) ;
        }

/* check for errors in memory allocation */

       if ( ! ( p->node_kind & FOURD ) && 
            ! ( p->io_mask & INTERP_DOWN || p->io_mask & FORCE_DOWN || p->io_mask & INTERP_UP || p->io_mask & SMOOTH_UP ) )
       {
	 fprintf(fp,"IF(.NOT.inter_domain)THEN\n",tag) ;
       }
       if ( p->ntl > 1 ) {
	 fprintf(fp,"IF(IAND(%d,tl).NE.0)THEN\n",tag) ;
       }
       fprintf(fp, "ALLOCATE(%s%s%s,STAT=ierr)\n if (ierr.ne.0) then\n CALL wrf_error_fatal ( &\n'frame/module_domain.f: Failed to allocate %s%s%s. ')\n endif\n",
                structname, fname,
                dimension_with_ranges( "", "(", t2, p, post, "model_config_rec%"), 
                structname, fname,
                dimension_with_ranges( "", "(", t2, p, post, "model_config_rec%")); 

       fprintf(fp, "  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) %s%s=", structname , fname);
       if( p->type != NULL  &&   (!strcmp( p->type->name , "real" ) 
                               || !strcmp( p->type->name , "doubleprecision") ) )   {
       /* if a real */
         fprintf(fp, "initial_data_value\n");
       } else if ( !strcmp( p->type->name , "logical" ) ) {
         fprintf(fp, ".FALSE.\n");
       } else if ( !strcmp( p->type->name , "integer" ) ) {
         fprintf(fp, "0\n");
       }
       if ( p->ntl > 1 ) {
	 fprintf(fp,"ELSE\n") ;

       fprintf(fp, "ALLOCATE(%s%s%s,STAT=ierr)\n if (ierr.ne.0) then\n CALL wrf_error_fatal ( &\n'frame/module_domain.f: Failed to allocate %s%s%s.  ')\n endif\n",
                structname, fname, dimension_with_ones( "(",t2,p,")" ), 
                structname, fname, dimension_with_ones( "(",t2,p,")" ) ) ;



	 fprintf(fp,"ENDIF\n") ;
       }
       if ( ! ( p->node_kind & FOURD ) && 
            ! ( p->io_mask & INTERP_DOWN || p->io_mask & FORCE_DOWN || p->io_mask & INTERP_UP || p->io_mask & SMOOTH_UP ) )
       {
	 fprintf(fp,"ELSE\n") ;
       fprintf(fp, "ALLOCATE(%s%s%s,STAT=ierr)\n if (ierr.ne.0) then\n CALL wrf_error_fatal ( &\n'frame/module_domain.f: Failed to allocate %s%s%s.  ')\n endif\n",
                structname, fname, dimension_with_ones( "(",t2,p,")" ), 
                structname, fname, dimension_with_ones( "(",t2,p,")" ) ) ;
	 fprintf(fp,"ENDIF\n") ;
       }

      }
    }
    if ( p->type != NULL )
    {
      if ( p->type->type_type == SIMPLE && p->ndims == 0 &&
               ((!strncmp("dyn_",p->use,4)&&!strcmp(corename,p->use+4)) || strncmp("dyn_",p->use,4)) &&
               (!strcmp(p->type->name,"integer") || 
                        !strcmp(p->type->name,"logical") || 
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
              !strcmp( p->type->name , "doubleprecision" )  ) { /* if a real */
            fprintf(fp, "IF ( setinitval .EQ. 3 ) %s%s=initial_data_value\n",
                        structname ,
                        fname ) ;
	  } else if ( !strcmp( p->type->name , "integer" ) ) {
            fprintf(fp, "IF ( setinitval .EQ. 3 ) %s%s=0\n",
                        structname ,
                        fname ) ;
          } else if ( !strcmp( p->type->name , "logical" ) ) {
            fprintf(fp, "IF ( setinitval .EQ. 3 ) %s%s=.FALSE.\n",
                        structname ,
                        fname ) ;
          }
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

int
gen_ddt_write ( char * dirname , char * corename )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char * fn = "_write_ddt.inc" ;

  if ( dirname == NULL || corename == NULL ) return(1) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s%s",dirname,corename,fn) ; }
  else                       { sprintf(fname,"%s%s",corename,fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_ddt_write1( fp , "grid%", corename , &Domain ) ;
  close_the_file( fp ) ;
  return(0) ;
}


int
gen_ddt_write1 ( FILE * fp , char * structname , char * corename , node_t * node )
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

       fprintf(fp, "write(iunit)%s%s\n",structname,fname) ;

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
          fprintf(fp, "write(iunit)%s%s\n",structname,fname) ;
      }
    }
  }
  return(0) ;
}

int
gen_dealloc ( char * dirname )
{
  int i ;
  
  for ( i = 0 ; i < get_num_cores() ; i++ )
  {
    gen_dealloc1( dirname , get_corename_i(i) ) ; 
  }
  return(0) ;
}

int
gen_dealloc1 ( char * dirname , char * corename )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char * fn = "_deallocs.inc" ;

  if ( dirname == NULL || corename == NULL ) return(1) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s%s",dirname,corename,fn) ; }
  else                       { sprintf(fname,"%s%s",corename,fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_dealloc2( fp , "grid%", corename , &Domain ) ;
  close_the_file( fp ) ;
  return(0) ;
}

int
gen_dealloc2 ( FILE * fp , char * structname , char * corename , node_t * node )
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

        fprintf(fp,
"IF ( ASSOCIATED( %s%s ) ) THEN \n", structname, fname ) ;
        fprintf(fp, 
"  DEALLOCATE(%s%s,STAT=ierr)\n if (ierr.ne.0) then\n CALL wrf_error_fatal ( &\n'frame/module_domain.f: Failed to dallocate %s%s. ')\n endif\n",
structname, fname, structname, fname ) ;
        fprintf(fp,
"  NULLIFY(%s%s)\n",structname, fname ) ;
        fprintf(fp,
"ENDIF\n" ) ;


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
      }
      else if ( p->type->type_type == DERIVED )
      {
        sprintf(x,"%s%s%%",structname,p->name ) ;
        gen_dealloc2(fp,x, corename, p->type) ;
      }
    }
  }
  return(0) ;
}
