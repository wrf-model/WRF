#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
# include <strings.h>
#endif

#include "protos.h"
#include "registry.h"
#include "data.h"
#include "sym.h"

int
gen_alloc ( char * dirname )
{
  gen_alloc1( dirname ) ; 
  gen_ddt_write( dirname ) ;
  return(0) ;
}

int
get_count_for_alloc( node_t *node , int *numguys, int *stats)  ;  /* forward */

int
gen_alloc1 ( char * dirname )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char * fn = "allocs.inc" ;
  int startpiece, fraction, iguy, numguys ;
  int stats[4] ;
#define FRAC 8

  if ( dirname == NULL ) return(1) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  startpiece = 0 ;
  fraction   = 0 ;
  numguys = 0 ;
  iguy = -1 ;
  stats[0] = 0 ; stats[1] = 0 ; stats[2] = 0 ; stats[3] = 0 ;
  get_count_for_alloc( &Domain, &numguys , stats) ;  /* howmany deez guys? */
  fprintf(stderr,"Registry INFO variable counts: 0d %d 1d %d 2d %d 3d %d\n",stats[0],stats[1],stats[2],stats[3]) ; 
  fprintf(fp,"#if 1\n") ;
  gen_alloc2( fp , "grid%", &Domain, &startpiece , &iguy, &fraction, numguys, FRAC, 1 ) ;
  fprintf(fp,"#endif\n") ;
  close_the_file( fp ) ;
  return(0) ;
}

int
get_count_for_alloc( node_t *node , int *numguys, int * stats ) 
{
  node_t * p ;
  for ( p = node->fields ; p != NULL ; p = p->next ) { 
    if        ( p->type != NULL && p->type->type_type == DERIVED ) {
      get_count_for_alloc( p->type , numguys, stats ) ;
    } else if (p->ndims >= 0) {
       (*numguys)++ ; 
       if        ( p->ndims == 0 ) {
         stats[p->ndims]++ ;
       } else if ( p->ndims == 1 ) {
         stats[p->ndims]++ ;
       } else if ( p->ndims == 2 ) {
         stats[p->ndims]++ ;
       } else if ( p->ndims == 3 ) {
         stats[p->ndims]++ ;
       }
    }
  }
  return 0; /* SamT: bug fix: return a value */
}

int
nolistthese( char * ) ;

int
gen_alloc2 ( FILE * fp , char * structname , node_t * node, int *j, int *iguy, int *fraction, int numguys, int frac, int sw ) /* 1 = allocate, 2 = just count */
{
  node_t * p ;
  int tag ;
  char post[NAMELEN], post_for_count[NAMELEN] ;
  char fname[NAMELEN], dname[NAMELEN], dname_tmp[NAMELEN] ;
  char x[NAMELEN] ;
  char dimname[3][NAMELEN] ;
  char tchar ;
  unsigned int *io_mask ;
  int nd ;
  int restart ;

  if ( node == NULL ) return(1) ;

  for ( p = node->fields ; p != NULL ; p = p->next )
  {
    (*iguy)++ ;

    if ( (*iguy % ((numguys+1)/frac+1)) == 0 ) {
      fprintf(fp,"#endif\n") ;
      fprintf(fp,"#if (NNN == %d)\n",(*j)++) ;
    }

    nd = p->ndims + ((p->node_kind & FOURD)?1:0) ;

    /* construct data name -- maybe same as vname if dname not spec'd  */
    if ( strlen(p->dname) == 0 || !strcmp(p->dname,"-") || p->dname[0] == ' ' ) 
                                                          { strcpy(dname_tmp,p->name) ; }
    else                                                  { strcpy(dname_tmp,p->dname) ; }
    make_upper_case(dname_tmp) ;

/*
   Generate error if input or output for two state variables would be generated with the same dataname

   example wrong:
     misc    tg      "SOILTB"   -> gen_tg,SOILTB
     misc    soiltb  "SOILTB"   -> gen_soiltb,SOILTB

*/
if ( tag == 1 )
{
     char dname_symbol[128] ;
     sym_nodeptr sym_node ;

     sprintf(dname_symbol, "DNAME_%s", dname_tmp ) ;
     /* check and see if it is in the symbol table already */

     if ((sym_node = sym_get( dname_symbol )) == NULL ) {
        /* add it */
      sym_node = sym_add ( dname_symbol ) ;
      strcpy( sym_node->internal_name , p->name ) ;
    } else {
      fprintf(stderr,"REGISTRY ERROR: Data-name collision on %s for %s -- %s\n",
      dname_tmp,p->name,p->dname ) ;
    }
}
/* end July 2004 */


    if ( p->ndims == 0 ) {
      if ( p->type->name[0] != 'c' && p->type->type_type != DERIVED && p->node_kind != RCONFIG && !nolistthese(p->name) ) {
        for ( tag = 1 ; tag <= p->ntl ; tag++ )
        {
          strcpy(fname,field_name(t4,p,(p->ntl>1)?tag:0)) ;
          if ( p->ntl > 1 ) sprintf(dname,"%s_%d",dname_tmp,tag) ;
          else              strcpy(dname,dname_tmp) ;

          fprintf(fp,"  IF (.NOT.grid%%is_intermediate) THEN\n") ;
          fprintf(fp,"   ALLOCATE( grid%%tail_statevars%%next )\n") ;
          fprintf(fp,"   grid%%tail_statevars => grid%%tail_statevars%%next\n") ;
          fprintf(fp,"   NULLIFY( grid%%tail_statevars%%next )\n" ) ;
          fprintf(fp,"   grid%%tail_statevars%%ProcOrient    = '  '\n") ;
          fprintf(fp,"   grid%%tail_statevars%%VarName = '%s'\n",fname ) ;
          fprintf(fp,"   grid%%tail_statevars%%DataName = '%s'\n",dname ) ;
          fprintf(fp,"   grid%%tail_statevars%%Description = '%s'\n",p->descrip ) ;
          fprintf(fp,"   grid%%tail_statevars%%Units = '%s'\n",p->units ) ;
          fprintf(fp,"   grid%%tail_statevars%%Type    = '%c'\n",p->type->name[0]) ;
          fprintf(fp,"   grid%%tail_statevars%%Ntl = %d\n",p->ntl<2?0:tag+p->ntl*100 ) ; /* if single tl, then 0, else tl itself */
          fprintf(fp,"   grid%%tail_statevars%%Restart  = %s\n", (p->restart)?".TRUE.":".FALSE." ) ;
          fprintf(fp,"   grid%%tail_statevars%%Ndim    = %d\n",p->ndims ) ;
          fprintf(fp,"   grid%%tail_statevars%%scalar_array  = .FALSE. \n" ) ;
          fprintf(fp,"   grid%%tail_statevars%%%cfield_%1dd => %s%s\n",p->type->name[0],p->ndims, structname, fname ) ;
          io_mask = p->io_mask ;
          if ( io_mask != NULL ) {
            int i ;
            for ( i = 0 ; i < IO_MASK_SIZE ; i++ ) {
              fprintf(fp,"  grid%%tail_statevars%%streams(%d) = %d ! %08x \n", i+1, io_mask[i], io_mask[i] ) ;
            }
          }
          fprintf(fp,"  ENDIF\n") ;
        }
      }
      if ( sw == 1 ) {
        for ( tag = 1 ; tag <= p->ntl ; tag++ )
        {
          strcpy(fname,field_name(t4,p,(p->ntl>1)?tag:0)) ;
          if ( p->ntl > 1 ) sprintf(dname,"%s_%d",dname_tmp,tag) ;
          else              strcpy(dname,dname_tmp) ;
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
    }
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

       if ( ! p->boundary_array ) { fprintf(fp,"IF(okay_to_alloc.AND.in_use_for_config(id,'%s')",fname) ; } 
       else                       { fprintf(fp,"IF(.TRUE.") ; }

       if ( ! ( p->node_kind & FOURD ) && sw == 1 &&
            ! ( p->nest_mask & INTERP_DOWN || p->nest_mask & FORCE_DOWN || p->nest_mask & INTERP_UP || p->nest_mask & SMOOTH_UP ) )
       {
	 fprintf(fp,".AND.(.NOT.grid%%is_intermediate)") ;
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

           if ( p->type->name[0] == 'l' && p->ndims >= 3 ) {
             fprintf(stderr,"ADVISORY: %1dd logical array %s is allowed but cannot be input or output\n",
                             p->ndims, p->name ) ;

           }

           if ( p->type->type_type != DERIVED && p->node_kind != RCONFIG && !nolistthese(p->name) &&
                ! ( p->type->name[0] == 'l' && p->ndims >= 3 ) )  /* dont list logical arrays larger than 2d  */
           { 
             char memord[NAMELEN], stagstr[NAMELEN] ;
             char *ornt ;

             if      ( p->proc_orient == ALL_X_ON_PROC ) ornt = "X" ;
             else if ( p->proc_orient == ALL_Y_ON_PROC ) ornt = "Y" ;
             else                                        ornt = " " ;

             strcpy(stagstr, "") ;
             if ( p->node_kind & FOURD ) {
               set_mem_order( p->members, memord , NAMELEN) ;
               if ( p->members->stag_x ) strcat(stagstr, "X") ;
               if ( p->members->stag_y ) strcat(stagstr, "Y") ;
               if ( p->members->stag_z ) strcat(stagstr, "Z") ;
             } else {
               set_mem_order( p, memord , NAMELEN) ;
               if ( p->stag_x ) strcat(stagstr, "X") ;
               if ( p->stag_y ) strcat(stagstr, "Y") ;
               if ( p->stag_z ) strcat(stagstr, "Z") ;
             }
             memord[3] = '\0' ; /* snip off any extra dimensions */

             if ( p->ntl > 1 ) sprintf(dname,"%s_%d",dname_tmp,tag) ;
             else                                    strcpy(dname,dname_tmp) ;

             fprintf(fp,"  IF (.NOT.grid%%is_intermediate) THEN\n") ; /*{*/
             fprintf(fp,"  ALLOCATE( grid%%tail_statevars%%next )\n" ) ;
             fprintf(fp,"  grid%%tail_statevars => grid%%tail_statevars%%next\n") ;
             fprintf(fp,"  NULLIFY( grid%%tail_statevars%%next )\n") ;
             fprintf(fp,"  grid%%tail_statevars%%VarName = '%s'\n", fname) ;
             fprintf(fp,"  grid%%tail_statevars%%DataName = '%s'\n", dname) ;
             fprintf(fp,"  grid%%tail_statevars%%Description = '%s'\n",p->descrip ) ;
             fprintf(fp,"  grid%%tail_statevars%%Units = '%s'\n",p->units ) ;
             fprintf(fp,"  grid%%tail_statevars%%Type    = '%c'\n", p->type->name[0]) ;
             fprintf(fp,"  grid%%tail_statevars%%ProcOrient    = '%s'\n", ornt) ;
             fprintf(fp,"  grid%%tail_statevars%%MemoryOrder  = '%s'\n", memord) ;
             fprintf(fp,"  grid%%tail_statevars%%Stagger      = '%s'\n", stagstr) ;
                           /* in next line for Ntl, if single tl, then zero, otherwise tl itself */
             fprintf(fp,"  grid%%tail_statevars%%Ntl     = %d\n", p->ntl<2?0:tag+p->ntl*100 ) ;
             fprintf(fp,"  grid%%tail_statevars%%Ndim    = %d\n", nd ) ;
             restart = 0 ;
             if ( p->node_kind & FOURD ) {
               node_t *q ;
               for ( q = p->members ; q->next != NULL ; q = q->next ) {  /* use the last one */
                 if ( q != NULL ) {
                   restart = q->restart ;
                 }
               }
             } else {
               restart = p->restart ;
             }
             fprintf(fp,"  grid%%tail_statevars%%Restart  = %s\n", (restart)?".TRUE.":".FALSE." ) ;
             fprintf(fp,"  grid%%tail_statevars%%scalar_array = %s\n", (p->node_kind & FOURD)?".TRUE.":".FALSE.") ;
             fprintf(fp,"  grid%%tail_statevars%%%cfield_%1dd => %s%s\n", p->type->name[0],nd, structname, fname ) ;
             if ( p->node_kind & FOURD ) {
               fprintf(fp,"  grid%%tail_statevars%%num_table => %s_num_table\n",   p->name ) ;
               fprintf(fp,"  grid%%tail_statevars%%index_table => %s_index_table\n",   p->name ) ;
               fprintf(fp,"  grid%%tail_statevars%%boundary_table => %s_boundary_table\n",   p->name ) ;
               fprintf(fp,"  grid%%tail_statevars%%dname_table => %s_dname_table\n",   p->name ) ;
               fprintf(fp,"  grid%%tail_statevars%%desc_table => %s_desc_table\n",   p->name ) ;
               fprintf(fp,"  grid%%tail_statevars%%units_table => %s_units_table\n",   p->name ) ;
               fprintf(fp,"  grid%%tail_statevars%%streams_table => %s_streams_table\n",   p->name ) ;
             } 

             if ( p->node_kind & FOURD ) {
               node_t *q ;
               io_mask = NULL ;
               for ( q = p->members ; q->next != NULL ; q = q->next ) {  /* use the last one */
                 if ( q != NULL ) {
                   io_mask = q->io_mask ;
                 }
               }
             } else {
               io_mask = p->io_mask ;
             }

             if ( io_mask != NULL ) {
               int i ;
               for ( i = 0 ; i < IO_MASK_SIZE ; i++ ) {
                 fprintf(fp,"  grid%%tail_statevars%%streams(%d) = %d ! %08x \n",  i+1, io_mask[i], io_mask[i] ) ;
               }
             }

             {
               char ddim[3][2][NAMELEN] ;
               char mdim[3][2][NAMELEN] ;
               char pdim[3][2][NAMELEN] ;

               set_dim_strs3( p, ddim, mdim, pdim , "", 0 ) ;           /* dimensions with staggering */

               fprintf(fp,"  grid%%tail_statevars%%sd1 = %s\n", ddim[0][0] ) ;
               fprintf(fp,"  grid%%tail_statevars%%ed1 = %s\n", ddim[0][1] ) ;
               fprintf(fp,"  grid%%tail_statevars%%sd2 = %s\n", ddim[1][0] ) ;
               fprintf(fp,"  grid%%tail_statevars%%ed2 = %s\n", ddim[1][1] ) ;
               fprintf(fp,"  grid%%tail_statevars%%sd3 = %s\n", ddim[2][0] ) ;
               fprintf(fp,"  grid%%tail_statevars%%ed3 = %s\n", ddim[2][1] ) ;
               fprintf(fp,"  grid%%tail_statevars%%sm1 = %s\n", mdim[0][0] ) ;
               fprintf(fp,"  grid%%tail_statevars%%em1 = %s\n", mdim[0][1] ) ;
               fprintf(fp,"  grid%%tail_statevars%%sm2 = %s\n", mdim[1][0] ) ;
               fprintf(fp,"  grid%%tail_statevars%%em2 = %s\n", mdim[1][1] ) ;
               fprintf(fp,"  grid%%tail_statevars%%sm3 = %s\n", mdim[2][0] ) ;
               fprintf(fp,"  grid%%tail_statevars%%em3 = %s\n", mdim[2][1] ) ;
               fprintf(fp,"  grid%%tail_statevars%%sp1 = %s\n", pdim[0][0] ) ;
               fprintf(fp,"  grid%%tail_statevars%%ep1 = %s\n", pdim[0][1] ) ;
               fprintf(fp,"  grid%%tail_statevars%%sp2 = %s\n", pdim[1][0] ) ;
               fprintf(fp,"  grid%%tail_statevars%%ep2 = %s\n", pdim[1][1] ) ;
               fprintf(fp,"  grid%%tail_statevars%%sp3 = %s\n", pdim[2][0] ) ;
               fprintf(fp,"  grid%%tail_statevars%%ep3 = %s\n", pdim[2][1] ) ;

             }
             {
               int i ;
               node_t * dimnode ;
               for ( i = 0 ; i < 3 ; i++ ) strcpy(dimname[i],"") ;
               for ( i = 0 ; i < 3 ; i++ )
               {
                 if (( dimnode = p->dims[i]) != NULL )
                 {
                   switch ( dimnode->coord_axis )
                   {
                   case (COORD_X) :
                     if ( ( ! sw_3dvar_iry_kludge && p->stag_x ) || ( sw_3dvar_iry_kludge && p->stag_y ) )
                      { sprintf( dimname[i] ,"%s_stag", dimnode->dim_data_name) ; }
                     else if ( p->dims[i]->subgrid )
                      { sprintf( dimname[i] ,"%s_subgrid", dimnode->dim_data_name) ; }
                     else
                      { strcpy( dimname[i], dimnode->dim_data_name) ; }
                     fprintf(fp,"  grid%%tail_statevars%%subgrid_x = %s\n",(p->dims[i]->subgrid)?".TRUE.":".FALSE.") ;
                     break ;
                   case (COORD_Y) :
                     if ( ( ! sw_3dvar_iry_kludge && p->stag_y ) || ( sw_3dvar_iry_kludge && p->stag_x ) )
                      { sprintf( dimname[i] ,"%s_stag", dimnode->dim_data_name) ; }
                     else if ( p->dims[i]->subgrid )
                      { sprintf( dimname[i] ,"%s_subgrid", dimnode->dim_data_name) ; }
                     else
                      { strcpy( dimname[i], dimnode->dim_data_name) ; }
                     fprintf(fp,"  grid%%tail_statevars%%subgrid_y = %s\n",(p->dims[i]->subgrid)?".TRUE.":".FALSE.") ;
                     break ;
                   case (COORD_Z) :
                     if ( p->stag_z )
                      { sprintf( dimname[i] ,"%s_stag", dimnode->dim_data_name) ; }
                     else if ( p->dims[i]->subgrid )
                      { sprintf( dimname[i] ,"%s_subgrid", dimnode->dim_data_name) ; }
                     else
                      { strcpy( dimname[i], dimnode->dim_data_name) ; }
                     break ;
                   }
                 }
               }
               fprintf(fp,"  grid%%tail_statevars%%dimname1 = '%s'\n", dimname[0] ) ;
               fprintf(fp,"  grid%%tail_statevars%%dimname2 = '%s'\n", dimname[1] ) ;
               fprintf(fp,"  grid%%tail_statevars%%dimname3 = '%s'\n", dimname[2] ) ;
             }
             fprintf(fp,"  ENDIF\n") ; /*}*/
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
      if ( p->type->type_type == DERIVED )
      {
        sprintf(x,"%s%s%%",structname,p->name ) ;
        gen_alloc2(fp,x, p->type, j, iguy, fraction, numguys, 1, sw) ;
      }
    }
  } /* fraction loop */
  return(0) ;
}

#if 0
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
#endif

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
#ifdef USE_ALLOCATABLES
                  fprintf(fp,
"IF ( ALLOCATED( %s%s%s ) ) THEN \n", structname, fname, bdy_indicator(bdy) ) ;
#else
                  fprintf(fp,
"IF ( ASSOCIATED( %s%s%s ) ) THEN \n", structname, fname, bdy_indicator(bdy) ) ;
#endif
                  fprintf(fp,
"  DEALLOCATE(%s%s%s,STAT=ierr)\n if (ierr.ne.0) then\n CALL wrf_error_fatal ( &\n'frame/module_domain.f: Failed to deallocate %s%s%s. ')\n endif\n",
          structname, fname, bdy_indicator(bdy), structname, fname, bdy_indicator(bdy) ) ;
#ifndef USE_ALLOCATABLES
                  fprintf(fp,
"  NULLIFY(%s%s%s)\n",structname, fname, bdy_indicator(bdy) ) ;
#endif
                  fprintf(fp, 
"ENDIF\n" ) ;
            }
          }
        } else {
#ifdef USE_ALLOCATABLES
        fprintf(fp,
"IF ( ALLOCATED( %s%s ) ) THEN \n", structname, fname ) ;
#else
        fprintf(fp,
"IF ( ASSOCIATED( %s%s ) ) THEN \n", structname, fname ) ;
#endif
        fprintf(fp, 
"  DEALLOCATE(%s%s,STAT=ierr)\n if (ierr.ne.0) then\n CALL wrf_error_fatal ( &\n'frame/module_domain.f: Failed to deallocate %s%s. ')\n endif\n",
structname, fname, structname, fname ) ;
#ifdef USE_ALLOCATABLES
        fprintf(fp,
"  NULLIFY(%s%s)\n",structname, fname ) ;
#endif
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

int
nolistthese( char * name )
{
   return(
             !strncmp(name,"auxhist",7)
          || !strncmp(name,"auxinput",8)
          || !strncmp(name,"oid",3)
         ) ;
}
