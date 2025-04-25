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
  FILE * fpCalls ;
  char  fname[NAMELEN] ;
  char * fn = "allocs.inc" ;
  char * fnCalls = "allocs_calls.inc" ;
  node_t *p;

  // Open array of allocs_[n].F
  int    numFiles = 32;
  int    idx      = 0;
  int    start    = 0;
  int    stop     = -1;
  int    primaryFields = 0;
  FILE * fpSub; 
  char * filename_prefix = "allocs_" ;

  if ( dirname == NULL ) return(1) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;

  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fnCalls) ; }
  else                       { sprintf(fname,"%s",fnCalls) ; }
  if ((fpCalls = fopen( fname , "w" )) == NULL ) return(1) ;

  for ( p = Domain.fields; p != NULL ; p = p->next ) { primaryFields++ ; }

  print_warning(fp,fname) ;
  fprintf(
          fp,
          "INTERFACE\n"
          );
  for ( idx = 0; idx < numFiles; idx++ )
  {
    if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s%d.F",dirname,filename_prefix,idx) ; }
    else                       { sprintf(fname,"%s%d.F",dirname,filename_prefix,idx ) ; }
    if ((fpSub = fopen( fname , "w" )) == NULL ) return(1) ;

    print_warning(fpSub,fname) ;
    fprintf(
            fp,
            "  SUBROUTINE %s%d( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &\n"
            "           sd31, ed31, sd32, ed32, sd33, ed33, &\n"
            "           sm31 , em31 , sm32 , em32 , sm33 , em33 , &\n"
            "           sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &\n"
            "           sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &\n"
            "           sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &\n"
            "           sm31x, em31x, sm32x, em32x, sm33x, em33x, &\n"
            "           sm31y, em31y, sm32y, em32y, sm33y, em33y )\n"
            "    USE module_domain_type\n"
            "    USE module_configure, ONLY : model_config_rec, grid_config_rec_type, in_use_for_config, model_to_grid_config_rec\n"
            "    USE module_scalar_tables ! this includes module_state_description too\n"
            "    IMPLICIT NONE\n"
            "    !  Input data.\n\n"
            "    TYPE(domain)               , POINTER          :: grid\n"
            "    INTEGER , INTENT(IN)            :: id\n"
            "    INTEGER , INTENT(IN)            :: setinitval_in   ! 3 = everything, 1 = arrays only, 0 = none\n"
            "    INTEGER , INTENT(IN)            :: sd31, ed31, sd32, ed32, sd33, ed33\n"
            "    INTEGER , INTENT(IN)            :: sm31, em31, sm32, em32, sm33, em33\n"
            "    INTEGER , INTENT(IN)            :: sp31, ep31, sp32, ep32, sp33, ep33\n"
            "    INTEGER , INTENT(IN)            :: sp31x, ep31x, sp32x, ep32x, sp33x, ep33x\n"
            "    INTEGER , INTENT(IN)            :: sp31y, ep31y, sp32y, ep32y, sp33y, ep33y\n"
            "    INTEGER , INTENT(IN)            :: sm31x, em31x, sm32x, em32x, sm33x, em33x\n"
            "    INTEGER , INTENT(IN)            :: sm31y, em31y, sm32y, em32y, sm33y, em33y\n\n"
            "    ! this argument is a bitmask. First bit is time level 1, second is time level 2, and so on.\n"
            "    ! e.g. to set both 1st and second time level, use 3\n"
            "    !      to set only 1st                        use 1\n"
            "    !      to set only 2st                        use 2\n"
            "    INTEGER , INTENT(IN)            :: tl_in\n\n"
            "    ! true if the allocation is for an intermediate domain (for nesting); only certain fields allocated\n"
            "    ! false otherwise (all allocated, modulo tl above)\n"
            "    LOGICAL , INTENT(IN)            :: inter_domain_in, okay_to_alloc_in\n\n"
            "    INTEGER(KIND=8) , INTENT(INOUT)         :: num_bytes_allocated\n"
            "  END SUBROUTINE %s%d\n",
            filename_prefix, idx, filename_prefix, idx
            );

    // Call the functions in the calls inc
    fprintf(
            fpCalls,
            "CALL %s%d( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &\n"
            "           sd31, ed31, sd32, ed32, sd33, ed33, &\n"
            "           sm31 , em31 , sm32 , em32 , sm33 , em33 , &\n"
            "           sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &\n"
            "           sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &\n"
            "           sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &\n"
            "           sm31x, em31x, sm32x, em32x, sm33x, em33x, &\n"
            "           sm31y, em31y, sm32y, em32y, sm33y, em33y )\n", 
            filename_prefix, idx
            );

    fprintf(
            fpSub,
            "SUBROUTINE %s%d( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &\n"
            "         sd31, ed31, sd32, ed32, sd33, ed33, &\n"
            "         sm31 , em31 , sm32 , em32 , sm33 , em33 , &\n"
            "         sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &\n"
            "         sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &\n"
            "         sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &\n"
            "         sm31x, em31x, sm32x, em32x, sm33x, em33x, &\n"
            "         sm31y, em31y, sm32y, em32y, sm33y, em33y )\n"
            "  USE module_domain_type\n"
            "  USE module_configure, ONLY : model_config_rec, grid_config_rec_type, in_use_for_config, model_to_grid_config_rec\n"
            "  USE module_scalar_tables ! this includes module_state_description too\n"
            "  IMPLICIT NONE\n"
            "  !  Input data.\n\n"
            "  TYPE(domain)               , POINTER          :: grid\n"
            "  INTEGER , INTENT(IN)            :: id\n"
            "  INTEGER , INTENT(IN)            :: setinitval_in   ! 3 = everything, 1 = arrays only, 0 = none\n"
            "  INTEGER , INTENT(IN)            :: sd31, ed31, sd32, ed32, sd33, ed33\n"
            "  INTEGER , INTENT(IN)            :: sm31, em31, sm32, em32, sm33, em33\n"
            "  INTEGER , INTENT(IN)            :: sp31, ep31, sp32, ep32, sp33, ep33\n"
            "  INTEGER , INTENT(IN)            :: sp31x, ep31x, sp32x, ep32x, sp33x, ep33x\n"
            "  INTEGER , INTENT(IN)            :: sp31y, ep31y, sp32y, ep32y, sp33y, ep33y\n"
            "  INTEGER , INTENT(IN)            :: sm31x, em31x, sm32x, em32x, sm33x, em33x\n"
            "  INTEGER , INTENT(IN)            :: sm31y, em31y, sm32y, em32y, sm33y, em33y\n\n"
            "  ! this argument is a bitmask. First bit is time level 1, second is time level 2, and so on.\n"
            "  ! e.g. to set both 1st and second time level, use 3\n"
            "  !      to set only 1st                        use 1\n"
            "  !      to set only 2st                        use 2\n"
            "  INTEGER , INTENT(IN)            :: tl_in\n\n"
            "  ! true if the allocation is for an intermediate domain (for nesting); only certain fields allocated\n"
            "  ! false otherwise (all allocated, modulo tl above)\n"
            "  LOGICAL , INTENT(IN)            :: inter_domain_in, okay_to_alloc_in\n\n"
            "  INTEGER(KIND=8) , INTENT(INOUT)         :: num_bytes_allocated\n"
            "  !  Local data.\n"
            "  INTEGER idum1, idum2, spec_bdy_width\n"
            "  REAL    initial_data_value\n"
            "  CHARACTER (LEN=256) message\n"
            "  INTEGER tl\n"
            "  LOGICAL inter_domain, okay_to_alloc\n"
            "  INTEGER setinitval\n"
            "  INTEGER sr_x, sr_y\n\n"
            "  !declare ierr variable for error checking ALLOCATE calls\n"
            "  INTEGER ierr\n\n"
            "  INTEGER                              :: loop\n"
            "  INTEGER(KIND=8)                      :: nba ! number of bytes allocated per variable\n"
            "  CHARACTER(LEN=256)                   :: message_string\n\n"
            "  ! Local data\n\n"
            "  TYPE ( grid_config_rec_type ) :: config_flags\n\n"
            "  INTEGER                         :: k_start , k_end, its, ite, jts, jte\n"
            "  INTEGER                         :: ids , ide , jds , jde , kds , kde , &\n"
            "                                    ims , ime , jms , jme , kms , kme , &\n"
            "                                    ips , ipe , jps , jpe , kps , kpe\n\n"
            "  INTEGER                         :: sids , side , sjds , sjde , skds , skde , &\n"
            "                                    sims , sime , sjms , sjme , skms , skme , &\n"
            "                                    sips , sipe , sjps , sjpe , skps , skpe\n\n"
            "  INTEGER ::              imsx, imex, jmsx, jmex, kmsx, kmex,    &\n"
            "                          ipsx, ipex, jpsx, jpex, kpsx, kpex,    &\n"
            "                          imsy, imey, jmsy, jmey, kmsy, kmey,    &\n"
            "                          ipsy, ipey, jpsy, jpey, kpsy, kpey\n\n"
            "  data_ordering : SELECT CASE ( model_data_order )\n"
            "    CASE  ( DATA_ORDER_XYZ )\n"
            "        ids = sd31 ; ide = ed31 ; jds = sd32 ; jde = ed32 ; kds = sd33 ; kde = ed33 ;\n"
            "        ims = sm31 ; ime = em31 ; jms = sm32 ; jme = em32 ; kms = sm33 ; kme = em33 ;\n"
            "        ips = sp31 ; ipe = ep31 ; jps = sp32 ; jpe = ep32 ; kps = sp33 ; kpe = ep33 ;\n"
            "        imsx = sm31x ; imex = em31x ; jmsx = sm32x ; jmex = em32x ; kmsx = sm33x ; kmex = em33x ;\n"
            "        ipsx = sp31x ; ipex = ep31x ; jpsx = sp32x ; jpex = ep32x ; kpsx = sp33x ; kpex = ep33x ;\n"
            "        imsy = sm31y ; imey = em31y ; jmsy = sm32y ; jmey = em32y ; kmsy = sm33y ; kmey = em33y ;\n"
            "        ipsy = sp31y ; ipey = ep31y ; jpsy = sp32y ; jpey = ep32y ; kpsy = sp33y ; kpey = ep33y ;\n"
            "    CASE  ( DATA_ORDER_YXZ )\n"
            "        ids = sd32  ; ide = ed32  ; jds = sd31  ; jde = ed31  ; kds = sd33  ; kde = ed33  ;\n"
            "        ims = sm32  ; ime = em32  ; jms = sm31  ; jme = em31  ; kms = sm33  ; kme = em33  ;\n"
            "        ips = sp32  ; ipe = ep32  ; jps = sp31  ; jpe = ep31  ; kps = sp33  ; kpe = ep33  ;\n"
            "        imsx = sm32x  ; imex = em32x  ; jmsx = sm31x  ; jmex = em31x  ; kmsx = sm33x  ; kmex = em33x  ;\n"
            "        ipsx = sp32x  ; ipex = ep32x  ; jpsx = sp31x  ; jpex = ep31x  ; kpsx = sp33x  ; kpex = ep33x  ;\n"
            "        imsy = sm32y  ; imey = em32y  ; jmsy = sm31y  ; jmey = em31y  ; kmsy = sm33y  ; kmey = em33y  ;\n"
            "        ipsy = sp32y  ; ipey = ep32y  ; jpsy = sp31y  ; jpey = ep31y  ; kpsy = sp33y  ; kpey = ep33y  ;\n"
            "    CASE  ( DATA_ORDER_ZXY )\n"
            "        ids = sd32  ; ide = ed32  ; jds = sd33  ; jde = ed33  ; kds = sd31  ; kde = ed31  ;\n"
            "        ims = sm32  ; ime = em32  ; jms = sm33  ; jme = em33  ; kms = sm31  ; kme = em31  ;\n"
            "        ips = sp32  ; ipe = ep32  ; jps = sp33  ; jpe = ep33  ; kps = sp31  ; kpe = ep31  ;\n"
            "        imsx = sm32x  ; imex = em32x  ; jmsx = sm33x  ; jmex = em33x  ; kmsx = sm31x  ; kmex = em31x  ;\n"
            "        ipsx = sp32x  ; ipex = ep32x  ; jpsx = sp33x  ; jpex = ep33x  ; kpsx = sp31x  ; kpex = ep31x  ;\n"
            "        imsy = sm32y  ; imey = em32y  ; jmsy = sm33y  ; jmey = em33y  ; kmsy = sm31y  ; kmey = em31y  ;\n"
            "        ipsy = sp32y  ; ipey = ep32y  ; jpsy = sp33y  ; jpey = ep33y  ; kpsy = sp31y  ; kpey = ep31y  ;\n"
            "    CASE  ( DATA_ORDER_ZYX )\n"
            "        ids = sd33  ; ide = ed33  ; jds = sd32  ; jde = ed32  ; kds = sd31  ; kde = ed31  ;\n"
            "        ims = sm33  ; ime = em33  ; jms = sm32  ; jme = em32  ; kms = sm31  ; kme = em31  ;\n"
            "        ips = sp33  ; ipe = ep33  ; jps = sp32  ; jpe = ep32  ; kps = sp31  ; kpe = ep31  ;\n"
            "        imsx = sm33x  ; imex = em33x  ; jmsx = sm32x  ; jmex = em32x  ; kmsx = sm31x  ; kmex = em31x  ;\n"
            "        ipsx = sp33x  ; ipex = ep33x  ; jpsx = sp32x  ; jpex = ep32x  ; kpsx = sp31x  ; kpex = ep31x  ;\n"
            "        imsy = sm33y  ; imey = em33y  ; jmsy = sm32y  ; jmey = em32y  ; kmsy = sm31y  ; kmey = em31y  ;\n"
            "        ipsy = sp33y  ; ipey = ep33y  ; jpsy = sp32y  ; jpey = ep32y  ; kpsy = sp31y  ; kpey = ep31y  ;\n"
            "    CASE  ( DATA_ORDER_XZY )\n"
            "        ids = sd31  ; ide = ed31  ; jds = sd33  ; jde = ed33  ; kds = sd32  ; kde = ed32  ;\n"
            "        ims = sm31  ; ime = em31  ; jms = sm33  ; jme = em33  ; kms = sm32  ; kme = em32  ;\n"
            "        ips = sp31  ; ipe = ep31  ; jps = sp33  ; jpe = ep33  ; kps = sp32  ; kpe = ep32  ;\n"
            "        imsx = sm31x  ; imex = em31x  ; jmsx = sm33x  ; jmex = em33x  ; kmsx = sm32x  ; kmex = em32x  ;\n"
            "        ipsx = sp31x  ; ipex = ep31x  ; jpsx = sp33x  ; jpex = ep33x  ; kpsx = sp32x  ; kpex = ep32x  ;\n"
            "        imsy = sm31y  ; imey = em31y  ; jmsy = sm33y  ; jmey = em33y  ; kmsy = sm32y  ; kmey = em32y  ;\n"
            "        ipsy = sp31y  ; ipey = ep31y  ; jpsy = sp33y  ; jpey = ep33y  ; kpsy = sp32y  ; kpey = ep32y  ;\n"
            "    CASE  ( DATA_ORDER_YZX )\n"
            "        ids = sd33  ; ide = ed33  ; jds = sd31  ; jde = ed31  ; kds = sd32  ; kde = ed32  ;\n"
            "        ims = sm33  ; ime = em33  ; jms = sm31  ; jme = em31  ; kms = sm32  ; kme = em32  ;\n"
            "        ips = sp33  ; ipe = ep33  ; jps = sp31  ; jpe = ep31  ; kps = sp32  ; kpe = ep32  ;\n"
            "        imsx = sm33x  ; imex = em33x  ; jmsx = sm31x  ; jmex = em31x  ; kmsx = sm32x  ; kmex = em32x  ;\n"
            "        ipsx = sp33x  ; ipex = ep33x  ; jpsx = sp31x  ; jpex = ep31x  ; kpsx = sp32x  ; kpex = ep32x  ;\n"
            "        imsy = sm33y  ; imey = em33y  ; jmsy = sm31y  ; jmey = em31y  ; kmsy = sm32y  ; kmey = em32y  ;\n"
            "        ipsy = sp33y  ; ipey = ep33y  ; jpsy = sp31y  ; jpey = ep31y  ; kpsy = sp32y  ; kpey = ep32y  ;\n"
            "  END SELECT data_ordering\n\n"
            "  CALL model_to_grid_config_rec ( id , model_config_rec , config_flags )\n\n"
            "  CALL nl_get_sr_x( id , sr_x )\n"
            "  CALL nl_get_sr_y( id , sr_y )\n\n"
            "  tl = tl_in\n"
            "  inter_domain = inter_domain_in\n"
            "  okay_to_alloc = okay_to_alloc_in\n\n"
            "#if ( RWORDSIZE == 8 )\n"
            "  initial_data_value = 0.\n"
            "#else\n"
            "  CALL get_initial_data_value ( initial_data_value )\n"
            "#endif\n\n"
            "#ifdef NO_INITIAL_DATA_VALUE\n"
            "  setinitval = 0\n"
            "#else\n"
            "  setinitval = setinitval_in\n"
            "#endif\n\n"
            "  CALL nl_get_spec_bdy_width( 1, spec_bdy_width )\n\n",
            filename_prefix, idx
            );

    // Determine start/stop fields
    start = stop + 1;
    if ( idx == numFiles - 1 )
    {
      // This should catch divisions that don't perfectly fit numFiles with at most numFiles 
      // extra fields in the last file
      stop = primaryFields;
    }
    else
    {
      stop  = start + ( primaryFields / numFiles );
    }
    gen_alloc2( fpSub , "grid%", NULL, &Domain, start, stop, 1 ) ;
    fprintf(
            fpSub,
            "END SUBROUTINE %s%d\n",
            filename_prefix, idx
            );
  }
  fprintf(
          fp,
          "END INTERFACE\n"
          );

  close_the_file( fpCalls ) ;
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
gen_alloc2 ( FILE * fp , char * structname , char * structname2 , node_t * node, int start, int stop, int sw ) /* 1 = allocate, 2 = just count */
{
  node_t * p ;
  int tag ;
  char post[NAMELEN], post_for_count[NAMELEN] ;
  char fname[NAMELEN], dname[NAMELEN], dname_tmp[NAMELEN] ;
  char x[NAMELEN] ;
  char x2[NAMELEN], fname2[NAMELEN] ;
  char dimname[3][NAMELEN] ;
  char tchar ;
  unsigned int *io_mask ;
  int nd ;
  int restart ;
  int currentIdx = -1;

  if ( node == NULL ) return(1) ;

  for ( p = node->fields ; p != NULL ; p = p->next )
  {
    // Skip if this field is not part of [start,stop] and stop != -1, so -1 can be used to force output
    currentIdx++;;
    if ( currentIdx < start && stop != -1 )
    {
      continue;
    }
    // We should be at [start] or forcing output via stop == -1
    if ( currentIdx > stop && stop != -1 )
    {
      // We passed stop and are not forcing, exit loop
      break;
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
        if ( structname2 != NULL ) {
          sprintf(fname2,"%s%s",structname2,fname) ;
        } else {
          strcpy(fname2,fname) ;
        }

/* check for errors in memory allocation */

       if ( ! p->boundary_array ) { fprintf(fp,"IF(okay_to_alloc.AND.in_use_for_config(id,'%s')",fname2) ; }
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
	   fprintf(fp,"  nba = &\n(%s) * %cWORDSIZE\n",
                   array_size_expression("", "(", -1, t2, p, post_for_count, "model_config_rec%"),
                   tchar) ;
           fprintf(fp,"#if ( SHOW_ALL_VARS_USED == 1 )\n") ;
           fprintf(fp,"  WRITE(message_string,fmt='(a,i12)') '%s: bytes = ',nba\n", fname) ;
           fprintf(fp,"  CALL wrf_message(message_string)\n") ;
           fprintf(fp,"#endif\n") ;
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
        sprintf(x2,"%s%%",p->name ) ;
        gen_alloc2(fp,x, x2, p->type, start, -1, sw) ;
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
  gen_alloc2( fp , "grid%", NULL, &Domain, 0 ) ;
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
  // Open array of deallocs_[n].inc
  int    numFiles = 12;
  int    idx      = 0;
  FILE * fpSub; 
  char * filename_prefix = "deallocs_" ;

  if ( dirname == NULL ) return(1) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;

  fprintf(
          fp,
          "INTERFACE\n"
          );
  if ( dirname == NULL ) return(1) ;
  for ( idx = 0; idx < numFiles; idx++ )
  {
    if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s%d.F",dirname,filename_prefix,idx) ; }
    else                       { sprintf(fname,"%s%d.F",dirname,filename_prefix,idx ) ; }
    if ((fpSub = fopen( fname , "w" )) == NULL ) return(1) ;
  
    print_warning(fpSub,fname) ;

    fprintf(
            fp,
            "  SUBROUTINE %s%d( grid )\n"
            "    USE module_wrf_error\n"
            "    USE module_domain_type\n"
            "    IMPLICIT NONE\n"
            "    TYPE( domain ), POINTER :: grid\n  END SUBROUTINE\n",
            filename_prefix, idx
            );

    fprintf(
            fpSub,
            "SUBROUTINE %s%d( grid )\n"
            "  USE module_wrf_error\n"
            "  USE module_domain_type\n"
            "  IMPLICIT NONE\n"
            "  TYPE( domain ), POINTER :: grid\n  INTEGER :: ierr\n",
            filename_prefix, idx
            );
    gen_dealloc2( fpSub, "grid%", &Domain, idx, numFiles );
    fprintf(
            fpSub,
            "END SUBROUTINE %s%d\n",
            filename_prefix, idx
            );
    close_the_file( fpSub ) ;
  }
  fprintf(
          fp,
          "END INTERFACE\n"
          );
  
  // Call the functions in the inc
  for ( idx = 0; idx < numFiles; idx++ )
  {
    fprintf(
          fp,
          "CALL %s%d( grid )\n", filename_prefix, idx
          );
  }
  close_the_file( fp ) ;
  return(0) ;
}

int
gen_dealloc2 ( FILE * fp , char * structname , node_t * node, int idx, int numFiles )
{
  node_t * p ;
  int tag ;
  char post[NAMELEN] ;
  char fname[NAMELEN] ;
  char x[NAMELEN] ;
  int currentIdx = -1;

  if ( node == NULL ) return(1) ;

  for ( p = node->fields ; p != NULL ; p = p->next )
  {
    // Modulo to divert each field based on index to a file
    // Skip if this field is not part of that index and idx != -1, so -1 can be used to force output
    currentIdx = ( currentIdx + 1 ) % numFiles;
    if ( currentIdx != idx && idx != -1 )
    {
      continue;
    }

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

  if (strcmp(fname,"chem_ic")==0)   continue ;  /* !!! add !!! */

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
#ifndef USE_ALLOCATABLES
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
        gen_dealloc2(fp,x, p->type, idx, -1) ;
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
