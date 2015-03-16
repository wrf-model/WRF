#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
# define rindex(X,Y) strrchr(X,Y)
# define index(X,Y) strchr(X,Y)
#else
# include <strings.h>
#endif

#include "protos.h"
#include "registry.h"
#include "data.h"
#include "sym.h"

static FILE * fp ;

#define GEN_INPUT  1
#define GEN_OUTPUT 2

#define OP_F(A,B) \
  fn = B ; \
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; } \
  else                       { sprintf(fname,"%s",fn) ; } \
  if ((A = fopen( fname , "w" )) == NULL ) return(1) ; \
  print_warning(A,fname) ; \
  sym_forget() ;

int
gen_wrf_io ( char * dirname )
{
  char  fname[NAMELEN], *fn ;

  if ( dirname == NULL ) return(1) ;

  OP_F(fp,"wrf_bdyout.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , GEN_OUTPUT ) ;
  close_the_file(fp) ;

  OP_F(fp,"wrf_bdyin.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , GEN_INPUT ) ;
  close_the_file(fp) ;

  return(0) ;
}

int
gen_wrf_io2 ( FILE * fp , char * fname, char * structname , char * fourdname, node_t * node , int sw_io )
{
  node_t * p ;
  int i , ii  ;
  char x[NAMELEN], tag[NAMELEN], dexes[NAMELEN] ;
  char dname[NAMELEN], dname_tmp[NAMELEN] ; 
  char vname[NAMELEN], vname_x[NAMELEN],vname_1[NAMELEN], vname_2[NAMELEN], memord[NAMELEN] ;
  char ddim[3][2][NAMELEN] ;
  char mdim[3][2][NAMELEN] ;
  char pdim[3][2][NAMELEN] ;
  char ddim_no[3][2][NAMELEN] ;
  char mdim_no[3][2][NAMELEN] ;
  char pdim_no[3][2][NAMELEN] ;
  char dimname[3][NAMELEN] ;
  char stagstr[NAMELEN] ;
  char * tend_tag ;

  char post[NAMELEN] ;
  char indices[NAMELEN] ;

  int pass, passes, stagx, stagy, stagz ;
  int xi, yi, zi ;
  node_t * dimnode ;
  int ok_to_collect_distribute ;

/* set a flag according to what the stream is, if we're running on dm processors, if the
   io layer cannot handle distributed data, and if we're selectively turning off the
   collect/distribute message passing so that history and restart I/O is to separate files
   but input and boundary I/O is unaffected */

  ok_to_collect_distribute = !sw_distrib_io_layer && sw_dm_parallel  ;

  if ( node == NULL ) return(1) ;
  if ( structname == NULL ) return(1) ;
  if ( fp == NULL ) return(1) ;

  for ( p = node ; p != NULL ; p = p->next )
  {


    if ( p->ndims > 3 && ! p->node_kind & FOURD ) continue ; /* short circuit anything with more than 3 dims, (not counting 4d arrays) */

    if ( p->node_kind & I1 ) continue ;  /* short circuit anything that's not a state var */

    set_dim_strs( p, ddim, mdim, pdim , "", 0 ) ;           /* dimensions with staggering */
    set_dim_strs( p, ddim_no, mdim_no, pdim_no , "", 1 ) ;  /* dimensions ignoring staggering */

    strcpy(stagstr, "") ;
    if ( p->stag_x ) strcat(stagstr, "X") ;
    if ( p->stag_y ) strcat(stagstr, "Y") ;
    if ( p->stag_z ) strcat(stagstr, "Z") ;


    if ( !strcmp(p->name,"-") ) continue ;

    if ( p->node_kind & FOURD )
    {
      node_t * nd , *pp ;
      char p1[NAMELEN], sv[NAMELEN], tl[25] ;


      set_dim_strs( p->members, ddim, mdim, pdim , "", 0 ) ;           /* dimensions with staggering */
      set_dim_strs( p->members, ddim_no, mdim_no, pdim_no , "", 1 ) ;  /* dimensions ignoring staggering */


/* BOUNDARY FOR 4-D TRACER */
      {
        int ibdy ;
        int idx ;
        node_t *fourd_bound_array ;
        char *bdytag, *xdomainend, *ydomainend, *zdomainend, bdytag2[10],fourd_bnd[NAMELEN] ;
        char *ds1,*de1,*ds2,*de2,*ds3,*de3,*ms1,*me1,*ms2,*me2,*ms3,*me3,*ps1,*pe1,*ps2,*pe2,*ps3,*pe3 ;

/* check for the existence of a fourd boundary array */
        sprintf(fourd_bnd,"%s_b",p->name) ;
        if (( fourd_bound_array = get_entry( fourd_bnd  ,Domain.fields)) != NULL ) {

          for ( i = 0 ; i < 3 ; i++ ) strcpy(dimname[i],"") ;
          strcpy( dimname[2] , "bdy_width" ) ;
          ds3 = "1" ; de3 = "config_flags%spec_bdy_width" ;
          ms3 = "1" ; me3 = "config_flags%spec_bdy_width" ;
          ps3 = "1" ; pe3 = "config_flags%spec_bdy_width" ;
          if (( dimnode = get_dimnode_for_coord( p , COORD_Z )) != NULL )
           { if ( p->stag_z ) { sprintf( dimname[1] ,"%s_stag", dimnode->dim_data_name) ; }
             else             { strcpy(  dimname[1], dimnode->dim_data_name) ; }
             if ( p->stag_z ) { zdomainend = "kde" ; }
             else             { zdomainend = "(kde-1)" ; }
             ds2 = "kds" ; de2 = zdomainend ;
             ms2 = "kds" ; me2 = "kde" ;   /* 20020924 */
             ps2 = "kds" ; pe2 = zdomainend ;
           }
          else
           {
             fprintf(stderr,"REGISTRY WARNING: 4D ARRAYS MUST HAVE VERT DIMENSION\n") ;
           }
          for ( pass = 0 ; pass < 2 ; pass++ ) {
fprintf(fp,"DO itrace = PARAM_FIRST_SCALAR , num_%s\n",p->name ) ;
/*fprintf(fp,"  IF (BTEST(%s_stream_table(grid%%id, itrace ) , switch )) THEN\n",p->name) ; */
fprintf(fp,"  IF ( %s_boundary_table(grid%%id, itrace ) ) THEN\n",p->name) ;
          for ( ibdy = 1 ; ibdy <= 4 ; ibdy++ )
          {
            if        ( pass == 0 && ibdy == 1 ) { bdytag = "_BXS" ;      /* west bdy   */
            } else if ( pass == 0 && ibdy == 2 ) { bdytag = "_BXE" ;      /* east bdy   */
            } else if ( pass == 0 && ibdy == 3 ) { bdytag = "_BYS" ;      /* south bdy   */
            } else if ( pass == 0 && ibdy == 4 ) { bdytag = "_BYE" ;      /* north bdy   */
            } else if ( pass == 1 && ibdy == 1 ) { bdytag = "_BTXS" ;      /* west bdy   */
            } else if ( pass == 1 && ibdy == 2 ) { bdytag = "_BTXE" ;      /* east bdy   */
            } else if ( pass == 1 && ibdy == 3 ) { bdytag = "_BTYS" ;      /* south bdy   */
            } else if ( pass == 1 && ibdy == 4 ) { bdytag = "_BTYE" ;      /* north bdy   */
            }
            if ( ibdy == 1 || ibdy == 2 ) {
              if (( dimnode = get_dimnode_for_coord( p , COORD_Y )) != NULL )
              {
                idx = get_index_for_coord( p , COORD_Y  ) ;
                if ( p->stag_y ) { ydomainend = "jde" ; } else { ydomainend = "(jde-1)" ; }
                ds1 = "1" ; de1 = ydomainend ;
                ms1 = "1" ; me1 = "MAX( ide , jde )" ;
                if ( sw_new_bdys ) {  /* 20070207 */
                  if ( ! sw_new_with_old_bdys ) { ms1 = "jms" ; me1 = "jme" ; }
                  if        ( sw_io == GEN_INPUT ) {
                    ps1 = "MAX(jms,jds)" ;
                    sprintf(t2,"MIN(jme,%s)",ydomainend) ; pe1 = t2 ;
                  } else if ( sw_io == GEN_OUTPUT ) {
                    ps1 = pdim[idx][0] ; pe1 = pdim[idx][1] ;
                  }
                } else {
                  if        ( sw_io == GEN_INPUT ) {
                    ps1 = "1" ; pe1 = ydomainend ;
                  } else if ( sw_io == GEN_OUTPUT ) {
                    ps1 = pdim[idx][0] ; pe1 = pdim[idx][1] ;
                  }
                }
                if ( p->stag_y ) { sprintf( dimname[0] ,"%s_stag", dimnode->dim_data_name) ; }
                else                   { strcpy( dimname[0], dimnode->dim_data_name) ; }
              }
            }
            if ( ibdy == 3 || ibdy == 4 ) {
              if (( dimnode = get_dimnode_for_coord( p , COORD_X )) != NULL )
              {
                idx = get_index_for_coord( p , COORD_X  ) ;
                if ( p->stag_x ) { xdomainend = "ide" ; } else { xdomainend = "(ide-1)" ; }
                ds1 = "1" ; de1 = xdomainend ;
                ms1 = "1" ; me1 = "MAX( ide , jde )" ;
                if ( sw_new_bdys ) {  /* 20070207 */
                  if ( ! sw_new_with_old_bdys ) { ms1 = "ims" ; me1 = "ime" ; }
                  if        ( sw_io == GEN_INPUT ) {
                    ps1 = "MAX(ims,ids)" ;
                    sprintf(t2,"MIN(ime,%s)",xdomainend) ; pe1 = t2 ;
                  } else if ( sw_io == GEN_OUTPUT ) {
                    ps1 = pdim[idx][0] ; pe1 = pdim[idx][1] ;
                  }
                } else {
                  if        ( sw_io == GEN_INPUT ) {
                    ps1 = "1" ; pe1 = xdomainend ;
                  } else if ( sw_io == GEN_OUTPUT ) {
                    ps1 = pdim[idx][0] ; pe1 = pdim[idx][1] ;
                  }
                }
                if ( p->stag_x ) { sprintf( dimname[0] ,"%s_stag", dimnode->dim_data_name) ; }
                else             { strcpy( dimname[0], dimnode->dim_data_name) ; }
              }
            }
            if      ( p->ndims == 3 ) sprintf(memord,"%sZ",bdytag+2+pass ) ;
            else if ( p->ndims == 2 ) sprintf(memord,"%s",bdytag+2+pass ) ;
            else                      sprintf(memord,"0") ;
fprintf(fp,"    CALL wrf_ext_%s_field (  &\n", (sw_io == GEN_INPUT)?"read":"write" ) ;
fprintf(fp,"          fid                             , &  ! DataHandle\n") ;
fprintf(fp,"          current_date(1:19)              , &  ! DateStr\n") ; 
fprintf(fp,"          TRIM(%s_dname_table( grid%%id, itrace )) // '%s', & !data name\n",p->name,bdytag) ;
            if ( ok_to_collect_distribute ) {
fprintf(fp,"                       globbuf_%s               , &  ! Field \n",p->members->type->name ) ;
            } else {
              strcpy(bdytag2,"") ;
              strncat(bdytag2,bdytag, pass+2) ;
if ( sw_new_bdys && ! sw_new_with_old_bdys ) { /* 20070207 */
  fprintf(fp,"          grid%%%s%s(%s,kds,1,itrace)  , &  ! Field\n",p->name,bdytag, ms1) ;
} else {
  fprintf(fp,"          grid%%%s%s(1,kds,1,%d,itrace)  , &  ! Field\n",p->name,bdytag2, ibdy) ;
}
            }
            if (!strncmp(p->members->type->name,"real",4)) {
              fprintf(fp,"                       WRF_FLOAT             , &  ! FieldType \n") ;
            } else {
              fprintf(fp,"                       WRF_%s             , &  ! FieldType \n" , p->members->type->name ) ;
            }
fprintf(fp,"          grid , &  ! grid\n") ;
fprintf(fp,"          grid%%domdesc       , &  ! Comm\n") ;
fprintf(fp,"          grid%%bdy_mask       , &  ! bdy_mask\n") ;
            if ( sw_io == GEN_OUTPUT ) {
fprintf(fp,"          dryrun             , &  ! flag\n") ;
            }
fprintf(fp,"          '%s'               , &  ! MemoryOrder\n",memord) ;
            strcpy(stagstr, "") ;
            if ( p->members->stag_x ) strcat(stagstr, "X") ;
            if ( p->members->stag_y ) strcat(stagstr, "Y") ;
            if ( p->members->stag_z ) strcat(stagstr, "Z") ;
fprintf(fp,"          '%s'                , &  ! Stagger\n",stagstr) ;
            if ( sw_io == GEN_OUTPUT ) {
fprintf(fp,"                       '%s'               , &  ! Dimname 1 \n",dimname[0] ) ;
fprintf(fp,"                       '%s'               , &  ! Dimname 2 \n",dimname[1] ) ;
fprintf(fp,"                       '%s'               , &  ! Dimname 3 \n",dimname[2] ) ;
fprintf(fp,"          %s_desc_table( grid%%id, itrace  ), & ! Desc\n",p->name) ;
fprintf(fp,"          %s_units_table( grid%%id, itrace  ), & ! Units\n",p->name) ;
            }
fprintf(fp,"'%s ext_write_field '//TRIM(%s_dname_table( grid%%id, itrace ))//' memorder %s' , & ! Debug message\n", fname, p->name,memord ) ;
fprintf(fp,"%s, %s, %s, %s, %s, %s, &\n",ds1,de1,ds2,de2,ds3,de3 ) ;
fprintf(fp,"%s, %s, %s, %s, %s, %s, &\n",ms1,me1,ms2,me2,ms3,me3 ) ;
fprintf(fp,"%s, %s, %s, %s, %s, %s, &\n",ps1,pe1,ps2,pe2,ps3,pe3 ) ;
fprintf(fp,"                         ierr )\n" ) ;
          }
fprintf(fp, "  ENDIF\n" ) ;
fprintf(fp, "ENDDO\n") ;
        }
      }
      } /* if fourd bound array associated with this tracer */
    }
    else if ( p->type != NULL )
    {

    if ( p->type->type == SIMPLE )
    {

/* ////////  BOUNDARY ///////////////////// */

      if (  p->boundary && strcmp( p->use, "_4d_bdy_array_" ) || ( p->boundary && fourdname ) )
      {
        int ibdy ;
        int idx ;
        char *bdytag, *xdomainend, *ydomainend, *zdomainend ;
        char *ds1,*de1,*ds2,*de2,*ds3,*de3,*ms1,*me1,*ms2,*me2,*ms3,*me3,*ps1,*pe1,*ps2,*pe2,*ps3,*pe3 ;
	char t1[64], t2[64] ;

        for ( i = 0 ; i < 3 ; i++ ) strcpy(dimname[i],"") ;
        strcpy( dimname[2] , "bdy_width" ) ;
        ds3 = "1" ; de3 = "config_flags%spec_bdy_width" ;
        ms3 = "1" ; me3 = "config_flags%spec_bdy_width" ;
        ps3 = "1" ; pe3 = "config_flags%spec_bdy_width" ;

        if (( dimnode = get_dimnode_for_coord( p , COORD_Z )) != NULL )
         { if ( p->stag_z ) { sprintf( dimname[1] ,"%s_stag", dimnode->dim_data_name) ; } 
           else             { strcpy(  dimname[1], dimnode->dim_data_name) ; }
           if ( p->stag_z ) { zdomainend = "kde" ; } 
           else             { zdomainend = "(kde-1)" ; }
           ds2 = "kds" ; de2 = zdomainend ;
           ms2 = "kds" ; me2 = "kde" ;   /* 20020924 */
           ps2 = "kds" ; pe2 = zdomainend ;
         }
        else
         { strcpy(dimname[1],dimname[2]) ;
           strcpy(dimname[2],"one_element") ; 
           ds2 = ds3 ; de2 = de3 ;
           ms2 = ms3 ; me2 = me3 ;
           ps2 = ps3 ; pe2 = pe3 ;
           ds3 = "1" ; de3 = "1" ;
           ms3 = "1" ; me3 = "1" ;
           ps3 = "1" ; pe3 = "1" ;
         }

        if ( strlen(p->dname) < 1 ) {
          fprintf(stderr,"gen_wrf_io.c: Registry WARNING: no data name for %s \n",p->name) ;
        }

        for ( ibdy = 1 ; ibdy <= 4 ; ibdy++ )
        {
          if        ( ibdy == 1 ) { bdytag = "XS" ;      /* west bdy   */
          } else if ( ibdy == 2 ) { bdytag = "XE" ;      /* east bdy   */
          } else if ( ibdy == 3 ) { bdytag = "YS" ;      /* south bdy   */
          } else if ( ibdy == 4 ) { bdytag = "YE" ;      /* north bdy   */
          }
          if ( strlen(p->dname)==0 || !strcmp(p->dname,"-") ) { sprintf(dname,"%s%s",p->name,bdytag)  ; }
          else                                                { sprintf(dname,"%s%s",p->dname,bdytag) ; }

          make_upper_case(dname) ;

          if ( ibdy == 1 || ibdy == 2 ) { 
            if (( dimnode = get_dimnode_for_coord( p , COORD_Y )) != NULL )
            {
              idx = get_index_for_coord( p , COORD_Y  ) ;
              if ( p->stag_y ) { ydomainend = "jde" ; } else { ydomainend = "(jde-1)" ; }
              ds1 = "1" ; de1 = ydomainend ;
              ms1 = "1" ; me1 = "MAX( ide , jde )" ;
	      if ( sw_new_bdys ) {  /* 20070207 */
                if ( ! sw_new_with_old_bdys ) { ms1 = "jms" ; me1 = "jme" ; }
                if        ( sw_io == GEN_INPUT ) {
		  ps1 = "MAX(jms,jds)" ;
		  sprintf(t2,"MIN(jme,%s)",ydomainend) ; pe1 = t2 ;
                } else if ( sw_io == GEN_OUTPUT ) {
                  ps1 = pdim[idx][0] ; pe1 = pdim[idx][1] ;
                }
	      } else {
                if        ( sw_io == GEN_INPUT ) {
                  ps1 = "1" ; pe1 = ydomainend ;
                } else if ( sw_io == GEN_OUTPUT ) {
                  ps1 = pdim[idx][0] ; pe1 = pdim[idx][1] ;
                }
	      }
              if ( p->stag_y ) { sprintf( dimname[0] ,"%s_stag", dimnode->dim_data_name) ; }
              else                   { strcpy( dimname[0], dimnode->dim_data_name) ; }
            }
          }
          if ( ibdy == 3 || ibdy == 4 ) {
            if (( dimnode = get_dimnode_for_coord( p , COORD_X )) != NULL )
            {
              idx = get_index_for_coord( p , COORD_X  ) ;
              if ( p->stag_x ) { xdomainend = "ide" ; } else { xdomainend = "(ide-1)" ; }
              ds1 = "1" ; de1 = xdomainend ;
              ms1 = "1" ; me1 = "MAX( ide , jde )" ;
	      if ( sw_new_bdys ) {  /* 20070207 */
                if ( ! sw_new_with_old_bdys ) { ms1 = "ims" ; me1 = "ime" ; }
                if        ( sw_io == GEN_INPUT ) {
		  ps1 = "MAX(ims,ids)" ;
		  sprintf(t2,"MIN(ime,%s)",xdomainend) ; pe1 = t2 ;
                } else if ( sw_io == GEN_OUTPUT ) {
                  ps1 = pdim[idx][0] ; pe1 = pdim[idx][1] ;
                }
	      } else {
                ms1 = "1" ; me1 = "MAX( ide , jde )" ;
                if        ( sw_io == GEN_INPUT ) {
                  ps1 = "1" ; pe1 = xdomainend ;
                } else if ( sw_io == GEN_OUTPUT ) {
                  ps1 = pdim[idx][0] ; pe1 = pdim[idx][1] ;
                }
	      }
              if ( p->stag_x ) { sprintf( dimname[0] ,"%s_stag", dimnode->dim_data_name) ; }
              else             { strcpy( dimname[0], dimnode->dim_data_name) ; }
            }
          }
          if      ( p->ndims == 3 ) sprintf(memord,"%sZ",bdytag ) ;
          else if ( p->ndims == 2 ) sprintf(memord,"%s",bdytag ) ;
          else                      sprintf(memord,"0") ;

        passes = 1 ;
        if ( fourdname != NULL ) passes = 2 ;
        for ( pass = 0 ; pass < passes ; pass++ ) {
          tend_tag = ( pass == 0 ) ? "_B" : "_BT" ;
	  if ( sw_io == GEN_INPUT )
	  {
            if ( ok_to_collect_distribute )
	      fprintf(fp,"IF ( wrf_dm_on_monitor() ) THEN\n") ;
            fprintf(fp,"CALL wrf_ext_read_field (  &\n") ;
            fprintf(fp,"                       fid                , &  ! DataHandle \n" ) ;
            fprintf(fp,"                       current_date(1:19) , &  ! DateStr \n" ) ;
            if ( fourdname == NULL ) {
              fprintf(fp,"                       '%s'               , &  ! Data Name \n", dname ) ;
	      if ( sw_new_bdys && ! sw_new_with_old_bdys ) { /* 20070207 */
                fprintf(fp,"                       %s%s%s(%s,kds,1)     , &  ! Field \n" , structname , p->name, bdy_indicator(ibdy), ms1 ) ;
	      } else {
                fprintf(fp,"                       %s%s(1,kds,1,%d)     , &  ! Field \n" , structname , p->name, ibdy ) ;
	      }
            } else {
              if ( strlen(p->dname)==0 || !strcmp(p->dname,"-") ) { sprintf(dname,"%s%s%s",p->name,tend_tag,bdytag)  ; }
              else                                                { sprintf(dname,"%s%s%s",p->dname,tend_tag,bdytag) ; }
              fprintf(fp,"                       '%s'               , &  ! Data Name \n", dname ) ;
	      if ( sw_new_bdys && ! sw_new_with_old_bdys ) { /* 20070207 */
                fprintf(fp,"                       %s%s%s%s(%s,kds,1,P_%s)     , &  ! Field \n" , 
                         structname , fourdname, tend_tag, bdy_indicator(ibdy), ms1, p->name ) ;
	      } else {
                fprintf(fp,"                       %s%s%s(1,kds,1,%d,P_%s)     , &  ! Field \n" , 
                         structname , fourdname, tend_tag, ibdy, p->name ) ;
	      }
            }
            if (!strncmp(p->type->name,"real",4)) {
              fprintf(fp,"                       WRF_FLOAT             , &  ! FieldType \n") ;
            } else {
              fprintf(fp,"                       WRF_%s             , &  ! FieldType \n" , p->type->name ) ;
            }
            fprintf(fp,"                       grid , &  ! grid\n") ;
            fprintf(fp,"                       grid%%domdesc      , &  ! Comm\n") ;
            fprintf(fp,"                       grid%%bdy_mask     , &  ! bdy_mask\n" ) ;
            fprintf(fp,"                       '%s'               , &  ! MemoryOrder\n",memord ) ;
            fprintf(fp,"                       '%s'               , &  ! Stagger\n",stagstr ) ;
            fprintf(fp,"'%s ext_read_field %s memorder %s' , & ! Debug message\n",fname,dname,memord ) ;
            /* global dimensions */
            fprintf(fp,"%s, %s, %s, %s, %s, %s, &\n",ds1,de1,ds2,de2,ds3,de3 ) ;
            fprintf(fp,"%s, %s, %s, %s, %s, %s, &\n",ms1,me1,ms2,me2,ms3,me3 ) ;
            fprintf(fp,"%s, %s, %s, %s, %s, %s, &\n",ps1,pe1,ps2,pe2,ps3,pe3 ) ;
            fprintf(fp,"                       ierr )\n") ;
            if ( ok_to_collect_distribute )
            {
	      fprintf(fp,"ENDIF\n") ;
	      fprintf(fp,"CALL wrf_dm_bcast_%s ( %s%s ( 1, 1 , 1 , %d ) , &\n",p->type->name, structname , p->name, ibdy) ;
              fprintf(fp," ((%s)-(%s)+1)*((%s)-(%s)+1)*((%s)-(%s)+1)  )\n",me1,ms1,me2,ms2,me3,ms3)  ;
            }
	  }
          else if ( sw_io == GEN_OUTPUT )
	  {
            if ( ok_to_collect_distribute )
              fprintf(fp,"IF ( wrf_dm_on_monitor() ) THEN\n") ;
            fprintf(fp,"CALL wrf_ext_write_field (  &\n") ;
            fprintf(fp,"                       fid                , &  ! DataHandle \n" ) ;
            fprintf(fp,"                       current_date(1:19) , &  ! DateStr \n" ) ;
            if ( fourdname == NULL ) {
              fprintf(fp,"                       '%s'               , &  ! Data Name \n", dname ) ;
	      if ( sw_new_bdys && ! sw_new_with_old_bdys ) { /* 20070207 */
                fprintf(fp,"                       %s%s%s(%s,kds,1)     , &  ! Field \n" , structname , p->name, bdy_indicator(ibdy), ms1 ) ;
              } else {
                fprintf(fp,"                       %s%s(1,kds,1,%d)     , &  ! Field \n" , structname , p->name, ibdy ) ;
	      }
            } else {
              if ( strlen(p->dname)==0 || !strcmp(p->dname,"-") ) { sprintf(dname,"%s%s%s",p->name,tend_tag,bdytag)  ; }
              else                                                { sprintf(dname,"%s%s%s",p->dname,tend_tag,bdytag) ; }
              fprintf(fp,"                       '%s'               , &  ! Data Name \n", dname ) ;
	      if ( sw_new_bdys && ! sw_new_with_old_bdys ) { /* 20070207 */
                fprintf(fp,"                       %s%s%s(%s,kds,1,P_%s)     , &  ! Field \n" , 
                                       structname , fourdname, tend_tag, ms1, bdy_indicator(ibdy) ) ;
              } else {
                fprintf(fp,"                       %s%s%s(1,kds,1,%d,P_%s)     , &  ! Field \n" , 
                                       structname , fourdname, tend_tag, ibdy, bdy_indicator(ibdy) ) ;
	      }
            }
            if (!strncmp(p->type->name,"real",4)) {
              fprintf(fp,"                       WRF_FLOAT          , &  ! FieldType \n") ;
            } else {
              fprintf(fp,"                       WRF_%s             , &  ! FieldType \n" , p->type->name ) ;
            }
            fprintf(fp,"                       grid , &  ! grid\n") ;
            fprintf(fp,"                       grid%%domdesc      , &  ! Comm\n") ;
            fprintf(fp,"                       grid%%bdy_mask     , &  ! bdy_mask\n" ) ;
            fprintf(fp,"                       dryrun             , &  ! flag\n" ) ;
            fprintf(fp,"                       '%s'               , &  ! MemoryOrder\n",memord ) ;
            fprintf(fp,"                       '%s'               , &  ! Stagger\n",stagstr ) ;
            fprintf(fp,"                       '%s'               , &  ! Dimname 1 \n",dimname[0] ) ;
            fprintf(fp,"                       '%s'               , &  ! Dimname 2 \n",dimname[1] ) ;
            fprintf(fp,"                       '%s'               , &  ! Dimname 3 \n",dimname[2] ) ;
            fprintf(fp,"                       '%s'               , &  ! Desc  \n",p->descrip ) ;
            fprintf(fp,"                       '%s'               , &  ! Units \n",p->units ) ;
            fprintf(fp,"'%s ext_write_field %s memorder %s' , & ! Debug message\n",fname,dname,memord ) ;
            /* global dimensions */
            fprintf(fp,"%s, %s, %s, %s, %s, %s, &\n",ds1,de1,ds2,de2,ds3,de3 ) ;
            fprintf(fp,"%s, %s, %s, %s, %s, %s, &\n",ms1,me1,ms2,me2,ms3,me3 ) ;
            fprintf(fp,"%s, %s, %s, %s, %s, %s, &\n",ps1,pe1,ps2,pe2,ps3,pe3 ) ;
            fprintf(fp,"                       ierr )\n") ;
            if ( ok_to_collect_distribute )
              fprintf(fp,"ENDIF\n") ;
	  }
        }
        }
      }

    }

    }
  }
  return(0) ;
}

