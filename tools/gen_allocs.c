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
  gen_alloc1( dirname ) ; 
  gen_ddt_write( dirname ) ;
  return(0) ;
}

int
gen_alloc1 ( char * dirname )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char * fn = "allocs.inc" ;

  if ( dirname == NULL ) return(1) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_alloc2( fp , "grid%", &Domain, 1 ) ;
  close_the_file( fp ) ;
  return(0) ;
}

int
gen_alloc2 ( FILE * fp , char * structname , node_t * node, int sw ) /* 1 = allocate, 2 = just count */
{
  node_t * p ;
  int tag ;
  char post[NAMELEN], post_for_count[NAMELEN] ;
  char fname[NAMELEN] ;
  char x[NAMELEN] ;
  char tchar ;

  if ( node == NULL ) return(1) ;

  for ( p = node->fields ; p != NULL ; p = p->next )
  {
    if ( (p->ndims > 0 || p->boundary_array) && (  /* any array or a boundary array and...   */
          (p->node_kind & FIELD) ||                /* scalar arrays                          */
          (p->node_kind & FOURD) )                 /* scalar arrays                          */
                         )
    {
      if ( p->type != NULL ) {
        tchar = '?' ;
        if      ( !strcmp( p->type->name , "real" ) )            { tchar = 'R' ; }
	else if ( !strcmp( p->type->name , "doubleprecision" ) ) { tchar = 'D' ; }
	else if ( !strcmp( p->type->name , "logical" ) )         { tchar = 'L' ; }
	else if ( !strcmp( p->type->name , "integer" ) )         { tchar = 'I' ; }
	else { fprintf(stderr,"WARNING: what is the type for %s ?\n", p->name) ; }
      }
      if ( p->node_kind & FOURD ) { sprintf(post,           ",num_%s)",field_name(t4,p,0)) ; 
                                    sprintf(post_for_count, "*num_%s)",field_name(t4,p,0)) ; }
      else                        { sprintf(post,           ")" ) ; 
                                    sprintf(post_for_count, ")" ) ;   }
      for ( tag = 1 ; tag <= p->ntl ; tag++ )
      {
        if ( !strcmp ( p->use , "_4d_bdy_array_") ) {
          strcpy(fname,p->name) ;
        } else {
          strcpy(fname,field_name(t4,p,(p->ntl>1)?tag:0)) ;
        }

/* check for errors in memory allocation */

       if ( ! p->boundary_array ) { fprintf(fp,"IF(in_use_for_config(id,'%s')",fname) ; } 
       else                       { fprintf(fp,"IF(.TRUE.") ; }
       if ( ! ( p->node_kind & FOURD ) && sw == 1 &&
            ! ( p->io_mask & INTERP_DOWN || p->io_mask & FORCE_DOWN || p->io_mask & INTERP_UP || p->io_mask & SMOOTH_UP ) )
       {
	 fprintf(fp,".AND.(.NOT.inter_domain)",tag) ;
       }
       if ( p->ntl > 1 && sw == 1 ) {
	 fprintf(fp,".AND.(IAND(%d,tl).NE.0)",tag) ;
       }
       fprintf(fp,")THEN\n") ;
       if ( p->boundary_array && sw_new_bdys ) {
         int bdy ;
         for ( bdy = 1 ; bdy <= 4 ; bdy++ )
         {
           if( p->type != NULL && tchar != '?' ) {
	     fprintf(fp,"  num_bytes_allocated = num_bytes_allocated + &\n(%s) * %cWORDSIZE\n",
                         array_size_expression("", "(", bdy, t2, p, post_for_count, "model_config_rec%"),
                         tchar) ;
           }
	   if ( sw == 1 ) {
             fprintf(fp, "  ALLOCATE(%s%s%s%s,STAT=ierr)\n  if (ierr.ne.0) then\n    CALL wrf_error_fatal ( &\n    'frame/module_domain.f: Failed to allocate %s%s%s%s. ')\n  endif\n",
                structname, fname, bdy_indicator(bdy),
                dimension_with_ranges( "", "(", bdy, t2, p, post, "model_config_rec%"), 
                structname, fname, bdy_indicator(bdy),
                dimension_with_ranges( "", "(", bdy, t2, p, post, "model_config_rec%")); 
             fprintf(fp, "  IF ( setinitval .EQ. 1 .OR. setinitval .EQ. 3 ) %s%s%s=", structname , fname , bdy_indicator(bdy));
             if( p->type != NULL  &&   (!strcmp( p->type->name , "real" )
                                   || !strcmp( p->type->name , "doubleprecision") ) )   {
             /* if a real */
               fprintf(fp, "initial_data_value\n");
             } else if ( !strcmp( p->type->name , "logical" ) ) {
               fprintf(fp, ".FALSE.\n");
             } else if ( !strcmp( p->type->name , "integer" ) ) {
               fprintf(fp, "0\n");
             }
	   }
         }
       } else {
         if( p->type != NULL && tchar != '?' ) {
	   fprintf(fp,"  num_bytes_allocated = num_bytes_allocated + &\n(%s) * %cWORDSIZE\n",
                   array_size_expression("", "(", -1, t2, p, post_for_count, "model_config_rec%"),
                   tchar) ;
         }
	 if ( sw == 1 ) {
           fprintf(fp, "  ALLOCATE(%s%s%s,STAT=ierr)\n  if (ierr.ne.0) then\n    CALL wrf_error_fatal ( &\n    'frame/module_domain.f: Failed to allocate %s%s%s. ')\n  endif\n",
                structname, fname,
                dimension_with_ranges( "", "(", -1, t2, p, post, "model_config_rec%"), 
                structname, fname,
                dimension_with_ranges( "", "(", -1, t2, p, post, "model_config_rec%")); 
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
	 }
       }

       fprintf(fp,"ELSE\n") ;

       if ( p->boundary_array && sw_new_bdys ) {
         int bdy ;
         for ( bdy = 1 ; bdy <= 4 ; bdy++ )
         {
           fprintf(fp, "  ALLOCATE(%s%s%s%s,STAT=ierr)\n  if (ierr.ne.0) then\n    CALL wrf_error_fatal ( &\n    'frame/module_domain.f: Failed to allocate %s%s%s%s.  ')\n  endif\n",
                structname, fname,  bdy_indicator(bdy), dimension_with_ones( "(",t2,p,")" ), 
                structname, fname,  bdy_indicator(bdy), dimension_with_ones( "(",t2,p,")" ) ) ;
         }
       } else {
           fprintf(fp, "  ALLOCATE(%s%s%s,STAT=ierr)\n  if (ierr.ne.0) then\n    CALL wrf_error_fatal ( &\n    'frame/module_domain.f: Failed to allocate %s%s%s.  ')\n  endif\n",
                structname, fname, dimension_with_ones( "(",t2,p,")" ), 
                structname, fname, dimension_with_ones( "(",t2,p,")" ) ) ;

       }

       fprintf(fp,"ENDIF\n") ;  /* end of in_use conditional */

      }
    }
    if ( p->type != NULL )
    {
      if ( p->type->type_type == SIMPLE && p->ndims == 0 &&
               (!strcmp(p->type->name,"integer") || 
                        !strcmp(p->type->name,"logical") || 
                        !strcmp(p->type->name,"real") ||
                        !strcmp(p->type->name,"doubleprecision"))
              )
      {
          strcpy(fname,field_name(t4,p,(p->ntl>1)?tag:0)) ;
          if ( sw == 1 ) {
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
      }
      else if ( p->type->type_type == DERIVED )
      {
        sprintf(x,"%s%s%%",structname,p->name ) ;
        gen_alloc2(fp,x, p->type, sw) ;
      }
    }
  }
  return(0) ;
}

int
gen_alloc_count ( char * dirname )
{
  gen_alloc_count1( dirname ) ;
  return(0) ;
}

int
gen_alloc_count1 ( char * dirname )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char * fn = "alloc_count.inc" ;

  if ( dirname == NULL ) return(1) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_alloc2( fp , "grid%", &Domain, 0 ) ;
  close_the_file( fp ) ;
  return(0) ;
}

int
gen_ddt_write ( char * dirname )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char * fn = "write_ddt.inc" ;

  if ( dirname == NULL ) return(1) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_ddt_write1( fp , "grid%", &Domain ) ;
  close_the_file( fp ) ;
  return(0) ;
}

int
gen_ddt_write1 ( FILE * fp , char * structname , node_t * node )
{
  node_t * p ;
  int tag ;
  char post[NAMELEN] ;
  char fname[NAMELEN] ;
  char x[NAMELEN] ;

  if ( node == NULL ) return(1) ;

  for ( p = node->fields ; p != NULL ; p = p->next )
  {
    if ( (p->ndims > 1 && ! p->boundary_array) && (  /* any array or a boundary array and...   */
          (p->node_kind & FIELD) ||                  /* scalar arrays or...                    */
          (p->node_kind & FOURD) )                   /* scalar arrays or...                    */
                         )
    {
      if ( p->node_kind & FOURD ) { sprintf(post,",num_%s)",field_name(t4,p,0)) ; }
      else                        { sprintf(post,")") ; }
      for ( tag = 1 ; tag <= p->ntl ; tag++ )
      {
       strcpy(fname,field_name(t4,p,(p->ntl>1)?tag:0)) ;

       if ( p->node_kind & FOURD ) {
         fprintf(fp, "write(0,*)'%s',%s%s(IDEBUG,KDEBUG,JDEBUG,2)\n",fname,structname,fname) ;
       } else {
         if ( p->ndims == 2 ) fprintf(fp, "write(0,*)'%s',%s%s(IDEBUG,JDEBUG)\n",fname,structname,fname) ;
         if ( p->ndims == 3 ) fprintf(fp, "write(0,*)'%s',%s%s(IDEBUG,KDEBUG,JDEBUG)\n",fname,structname,fname) ;
       }

      }
    }
  }
  return(0) ;
}

int
gen_dealloc ( char * dirname )
{
  gen_dealloc1( dirname ) ; 
  return(0) ;
}

int
gen_dealloc1 ( char * dirname )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char * fn = "deallocs.inc" ;

  if ( dirname == NULL ) return(1) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_dealloc2( fp , "grid%", &Domain ) ;
  close_the_file( fp ) ;
  return(0) ;
}

int
gen_dealloc2 ( FILE * fp , char * structname , node_t * node )
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
          (p->node_kind & FIELD) ||                /* scalar arrays or                       */
          (p->node_kind & FOURD) )                 /* scalar arrays or                       */
                         )
    {
      if ( p->node_kind & FOURD ) { sprintf(post,",num_%s)",field_name(t4,p,0)) ; }
      else                        { sprintf(post,")") ; }
      for ( tag = 1 ; tag <= p->ntl ; tag++ )
      {
        strcpy(fname,field_name(t4,p,(p->ntl>1)?tag:0)) ;

        if ( p->boundary && sw_new_bdys ) {
          { int bdy ; 
            for ( bdy = 1 ; bdy <= 4 ; bdy++ ) {
                  fprintf(fp,
"IF ( ASSOCIATED( %s%s%s ) ) THEN \n", structname, fname, bdy_indicator(bdy) ) ;
                  fprintf(fp,
"  DEALLOCATE(%s%s%s,STAT=ierr)\n if (ierr.ne.0) then\n CALL wrf_error_fatal ( &\n'frame/module_domain.f: Failed to dallocate %s%s%s. ')\n endif\n",
          structname, fname, bdy_indicator(bdy), structname, fname, bdy_indicator(bdy) ) ;
                  fprintf(fp,
"  NULLIFY(%s%s%s)\n",structname, fname, bdy_indicator(bdy) ) ;
                  fprintf(fp, 
"ENDIF\n" ) ;
            }
          }
        } else {
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
    }
    if ( p->type != NULL )
    {
      if ( p->type->type_type == SIMPLE && p->ndims == 0 &&
               (!strcmp(p->type->name,"integer") ||
                        !strcmp(p->type->name,"real") ||
                        !strcmp(p->type->name,"doubleprecision"))
              )
      {
      }
      else if ( p->type->type_type == DERIVED )
      {
        sprintf(x,"%s%s%%",structname,p->name ) ;
        gen_dealloc2(fp,x, p->type) ;
      }
    }
  }
  return(0) ;
}
