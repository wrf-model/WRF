#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "protos.h"
#include "registry.h"
#include "data.h"

static FILE * fp ;

#define GEN_INPUT  1
#define GEN_OUTPUT 2

#define OP_F(A,B) \
  fn = B ;  \
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; } \
  else                       { sprintf(fname,"%s",fn) ; } \
  if ((A = fopen( fname , "w" )) == NULL ) return(1) ; \
  print_warning(A,fname) ;

int
gen_wrf_io ( char * dirname )
{
  char  fname[NAMELEN], *fn ;

  if ( dirname == NULL ) return(1) ;

  OP_F(fp,"wrf_histout.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , HISTORY , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist1out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXHIST1 , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist2out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXHIST2 , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist3out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXHIST3 , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist4out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXHIST4 , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist5out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXHIST5 , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_inputout.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , INPUT   , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput1out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXINPUT1   , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput2out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXINPUT2   , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput3out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXINPUT3   , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput4out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXINPUT4   , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput5out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXINPUT5   , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_restartout.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , RESTART , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_bdyout.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , BOUNDARY , GEN_OUTPUT ) ;
  close_the_file(fp) ;

  OP_F(fp,"wrf_histin.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , HISTORY , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist1in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXHIST1 , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist2in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXHIST2 , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist3in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXHIST3 , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist4in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXHIST4 , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist5in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXHIST5 , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_inputin.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , INPUT   , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput1in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXINPUT1   , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput2in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXINPUT2   , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput3in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXINPUT3   , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput4in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXINPUT4   , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput5in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , AUXINPUT5   , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_restartin.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , RESTART , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_bdyin.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , Domain.fields , BOUNDARY , GEN_INPUT ) ;
  close_the_file(fp) ;

  return(0) ;
}

int
set_dim_strs ( node_t *node , char ddim[3][2][NAMELEN], char mdim[3][2][NAMELEN], char pdim[3][2][NAMELEN] , char * prepend , int sw_disregard_stag )
{
  int i, j ;
  node_t *p ;
  char d ;
  char * stag ;
  if ( node == NULL ) return(1) ;
  for ( i = 0 ; i < 3 ; i++ )
    for ( j = 0 ; j < 2 ; j++ )
      {
        strcpy(ddim[i][j],"1") ;
        strcpy(mdim[i][j],"1") ;
        strcpy(pdim[i][j],"1") ;
      }

  for ( i = 0 ; i < node->ndims ; i++ )
  {
    p = node->dims[i] ;
    if      ( p->len_defined_how == DOMAIN_STANDARD )
    {
      if ( sw_3dvar_iry_kludge ) {
        switch( p->coord_axis )
        {
                                                 /* vvv */
        case(COORD_X) : d = 'i' ; stag = (node->stag_y||sw_disregard_stag)?"%s%cde":"(%s%cde-1)" ; break ;
        case(COORD_Y) : d = 'j' ; stag = (node->stag_x||sw_disregard_stag)?"%s%cde":"(%s%cde-1)" ; break ;
                                                 /* ^^^ */
        case(COORD_Z) : d = 'k' ; stag = (node->stag_z||sw_disregard_stag)?"%s%cde":"(%s%cde-1)" ; break ;
        default : stag = "1" ; break ;
        }
      } else {
        switch( p->coord_axis )
        {
        case(COORD_X) : d = 'i' ; stag = (node->stag_x||sw_disregard_stag)?"%s%cde":"(%s%cde-1)" ; break ;
        case(COORD_Y) : d = 'j' ; stag = (node->stag_y||sw_disregard_stag)?"%s%cde":"(%s%cde-1)" ; break ;
        case(COORD_Z) : d = 'k' ; stag = (node->stag_z||sw_disregard_stag)?"%s%cde":"(%s%cde-1)" ; break ;
        default : stag = "1" ; break ;
        }
      }
       
      sprintf(ddim[i][0],"%s%cds",prepend,d) ;
      sprintf(ddim[i][1],stag,prepend,d) ;  /* note that stag has printf format info in it */
      sprintf(mdim[i][0],"%s%cms",prepend,d) ;
      sprintf(mdim[i][1],"%s%cme",prepend,d) ;
      sprintf(pdim[i][0],"%s%cps",prepend,d) ;
      if ( ! sw_disregard_stag )
        sprintf(pdim[i][1],"MIN( %s, %s%cpe )",ddim[i][1],prepend,d) ;
      else
        sprintf(pdim[i][1],"%s%cpe",prepend,d) ;
    }
    else if ( p->len_defined_how == NAMELIST )
    {
      if ( !strcmp( p->assoc_nl_var_s, "1" ) )
      {
        sprintf(ddim[i][0],"1") ;
        sprintf(mdim[i][0],"1") ;
        sprintf(pdim[i][0],"1") ;
      }
      else
      {
        sprintf(ddim[i][0],"config_flags%%%s",p->assoc_nl_var_s) ;
        sprintf(mdim[i][0],"config_flags%%%s",p->assoc_nl_var_s) ;
        sprintf(pdim[i][0],"config_flags%%%s",p->assoc_nl_var_s) ;
      }
      sprintf(ddim[i][1],"config_flags%%%s",p->assoc_nl_var_e) ;
      sprintf(mdim[i][1],"config_flags%%%s",p->assoc_nl_var_e) ;
      sprintf(pdim[i][1],"config_flags%%%s",p->assoc_nl_var_e) ;
    }
    else if ( p->len_defined_how == CONSTANT )
    {
      sprintf(ddim[i][0],"%d",p->coord_start ) ;
      sprintf(ddim[i][1],"%d",p->coord_end   ) ; 
      sprintf(mdim[i][0],"%d",p->coord_start ) ;
      sprintf(mdim[i][1],"%d",p->coord_end   ) ; 
      sprintf(pdim[i][0],"%d",p->coord_start ) ;
      sprintf(pdim[i][1],"%d",p->coord_end   ) ; 
    }
  }
  return(0) ;
}

int
gen_wrf_io2 ( FILE * fp , char * fname, char * structname , node_t * node , int io_mask , int sw_io )
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
  char core[NAMELEN] ;
  char stagstr[NAMELEN] ;

  char post[NAMELEN] ;
  char indices[NAMELEN] ;

  int pass, passes ;
  int xi, yi, zi ;
  node_t * dimnode ;
  int ok_to_collect_distribute ;

/* set a flag according to what the stream is, if we're running on dm processors, if the
   io layer cannot handle distributed data, and if we're selectively turning off the
   collect/distribute message passing so that history and restart I/O is to separate files
   but input and boundary I/O is unaffected */

  ok_to_collect_distribute = !sw_distrib_io_layer && 
                              sw_dm_parallel && 
                             !(sw_dm_serial_in_only && ((io_mask&HISTORY)  ||
                                                        (io_mask&AUXHIST1) ||
                                                        (io_mask&AUXHIST2) ||
                                                        (io_mask&AUXHIST3) ||
                                                        (io_mask&AUXHIST4) ||
                                                        (io_mask&AUXHIST5) ||
                                                        (io_mask&RESTART))) ;

  if ( node == NULL ) return(1) ;
  if ( structname == NULL ) return(1) ;
  if ( fp == NULL ) return(1) ;


  for ( p = node ; p != NULL ; p = p->next )
  {

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
      gen_wrf_io2 ( fp , fname, structname , p->members, io_mask, sw_io ) ; 
    }
    else if ( p->type != NULL )
    {

    if ( p->type->type == SIMPLE )
    {

/* ////////  BOUNDARY ///////////////////// */

      if ( p->io_mask & BOUNDARY && (io_mask & BOUNDARY) )
      {
        int ibdy ;
        char *bdytag, *xdomainend, *ydomainend, *zdomainend ;
        char *ds1,*de1,*ds2,*de2,*ds3,*de3,*ms1,*me1,*ms2,*me2,*ms3,*me3,*ps1,*pe1,*ps2,*pe2,*ps3,*pe3 ;

        if (!strncmp( p->use, "dyn_", 4))
          sprintf(core,"%s_",p->use+4) ;
        else
          strcpy(core,"") ;

        for ( i = 0 ; i < 3 ; i++ ) strcpy(dimname[i],"") ;
        strcpy( dimname[2] , "bdy_width" ) ;
        ds3 = "1" ; de3 = "config_flags%spec_bdy_width" ;
        ms3 = "1" ; me3 = "config_flags%spec_bdy_width" ;
        ps3 = "1" ; pe3 = "config_flags%spec_bdy_width" ;

        if (( dimnode = get_dimnode_for_coord( p , COORD_Z )) != NULL )
         { if ( p->stag_z ) { sprintf( dimname[1] ,"%s_stag", dimnode->dim_data_name) ; } 
           else             { strcpy( dimname[1], dimnode->dim_data_name) ; }
           if ( p->stag_z ) { zdomainend = "kde" ; } 
           else             { zdomainend = "(kde-1)" ; }
           ds2 = "kds" ; de2 = zdomainend ;
           ms2 = "kds" ; me2 = "kde" ;   /* 20020924 */
           ps2 = "kds" ; pe2 = zdomainend ;
         }
        else
         { 
           strcpy(dimname[2],"") ; 
           strcpy(dimname[1],"bdy_width") ; 
           ds3 = "1" ; de3 = "1" ;
           ms3 = "1" ; me3 = "1" ;
           ps3 = "1" ; pe3 = "1" ;
           ds2 = "1" ; de2 = "config_flags%spec_bdy_width" ;
           ms2 = "1" ; me2 = "config_flags%spec_bdy_width" ;
           ps2 = "1" ; pe2 = "config_flags%spec_bdy_width" ;
         }
        if ( strlen(dname) < 1 ) {
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
              if ( p->stag_y ) { ydomainend = "jde" ; } else { ydomainend = "(jde-1)" ; }
              ds1 = "1" ; de1 = ydomainend ;
              ms1 = "1" ; me1 = "MAX( ide , jde )" ;
              if        ( sw_io == GEN_INPUT ) {
                ps1 = "1" ; pe1 = ydomainend ;
              } else if ( sw_io == GEN_OUTPUT ) {
                ps1 = pdim[get_index_for_coord( p , COORD_Y )][0] ; pe1 = pdim[get_index_for_coord( p , COORD_Y )][1] ;
              }
              if ( p->stag_y ) { sprintf( dimname[0] ,"%s_stag", dimnode->dim_data_name) ; }
              else                   { strcpy( dimname[0], dimnode->dim_data_name) ; }
            }
          }
          if ( ibdy == 3 || ibdy == 4 ) {
            if (( dimnode = get_dimnode_for_coord( p , COORD_X )) != NULL )
            {
              if ( p->stag_x ) { xdomainend = "ide" ; } else { xdomainend = "(ide-1)" ; }
              ds1 = "1" ; de1 = xdomainend ;
              ms1 = "1" ; me1 = "MAX( ide , jde )" ;
              if        ( sw_io == GEN_INPUT ) {
                ps1 = "1" ; pe1 = xdomainend ;
              } else if ( sw_io == GEN_OUTPUT ) {
                ps1 = pdim[get_index_for_coord( p , COORD_X )][0] ; pe1 = pdim[get_index_for_coord( p , COORD_X )][1] ;
              }
              if ( p->stag_x ) { sprintf( dimname[0] ,"%s_stag", dimnode->dim_data_name) ; }
              else             { strcpy( dimname[0], dimnode->dim_data_name) ; }
            }
          }
          if      ( p->ndims == 3 ) sprintf(memord,"%sZ",bdytag ) ;
          else if ( p->ndims == 2 ) sprintf(memord,"%s",bdytag ) ;
          else                      sprintf(memord,"0") ;
	  if ( sw_io == GEN_INPUT )
	  {
	    if ( !strncmp( p->use, "dyn_", 4 ) ) 
	      fprintf(fp,"IF ( grid%%dyn_opt .EQ. %s ) THEN\n",p->use) ;
            if ( ok_to_collect_distribute )
	      fprintf(fp,"IF ( wrf_dm_on_monitor() ) THEN\n") ;
            fprintf(fp,"CALL wrf_ext_read_field (  &\n") ;
            fprintf(fp,"                       fid                , &  ! DataHandle \n" ) ;
            fprintf(fp,"                       current_date(1:19) , &  ! DateStr \n" ) ;
            fprintf(fp,"                       '%s'               , &  ! Data Name \n", dname ) ;
            fprintf(fp,"                       %s%s%s(1,kds,1,%d)     , &  ! Field \n" , structname , core , p->name, ibdy ) ;
          if ( !strcmp(p->type->name,"doubleprecision") )
            { fprintf(fp,"                       WRF_DOUBLE         , &  ! FieldType \n" ) ; }
          else
            { fprintf(fp,"                       WRF_%s             , &  ! FieldType \n" , p->type->name ) ; }
            fprintf(fp,"                       grid%%communicator , &  ! Comm\n") ;
            fprintf(fp,"                       grid%%iocommunicator , &  ! Comm\n") ;
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
	      fprintf(fp,"CALL wrf_dm_bcast_%s ( %s%s%s ( 1, 1 , 1 , %d ) , &\n",p->type->name, structname , core , p->name, ibdy) ;
              fprintf(fp," ((%s)-(%s)+1)*((%s)-(%s)+1)*((%s)-(%s)+1)  )\n",me1,ms1,me2,ms2,me3,ms3)  ;
            }
	    if ( !strncmp( p->use, "dyn_", 4 ) ) 
	      fprintf(fp,"END IF\n" ) ;
	  }
          else if ( sw_io == GEN_OUTPUT )
	  {
            if ( ok_to_collect_distribute )
              fprintf(fp,"IF ( wrf_dm_on_monitor() ) THEN\n") ;
            if ( !strncmp( p->use, "dyn_", 4 ) )
              fprintf(fp,"IF ( grid%%dyn_opt .EQ. %s ) THEN\n",p->use) ;
            fprintf(fp,"CALL wrf_ext_write_field (  &\n") ;
            fprintf(fp,"                       fid                , &  ! DataHandle \n" ) ;
            fprintf(fp,"                       current_date(1:19) , &  ! DateStr \n" ) ;
            fprintf(fp,"                       '%s'               , &  ! Data Name \n", dname ) ;
            fprintf(fp,"                       %s%s%s(1,kds,1,%d)     , &  ! Field \n" , structname , core , p->name, ibdy ) ;
          if ( !strcmp(p->type->name,"doubleprecision") )
            { fprintf(fp,"                       WRF_DOUBLE         , &  ! FieldType \n" ) ; }
          else
            { fprintf(fp,"                       WRF_%s             , &  ! FieldType \n" , p->type->name ) ; }
            fprintf(fp,"                       grid%%communicator , &  ! Comm\n") ;
            fprintf(fp,"                       grid%%iocommunicator , &  ! Comm\n") ;
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
            if ( !strncmp( p->use, "dyn_", 4 ) )
              fprintf(fp,"END IF\n" ) ;
            if ( ok_to_collect_distribute )
              fprintf(fp,"ENDIF\n") ;
	  }
        }
      }

/* ////////  NOT BOUNDARY ///////////////////// */

      else if ( (p->io_mask & io_mask) && ! (io_mask & BOUNDARY) && ! (p->node_kind & RCONFIG) )
      {
 
        if ( io_mask & RESTART && p->ntl > 1 ) passes = p->ntl ;
        else                                   passes = 1 ;

        for ( pass = 0 ; pass < passes ; pass++ )   /* for multi timelevel vars */
        {
          if (!strncmp( p->use, "dyn_", 4))
	    sprintf(core,"%s_",p->use+4) ;
	  else
	    strcpy(core,"") ;

		  /* for multi time level variables gen read for both levels
		     for restart, only _2 for others */
          if ( p->ntl > 1 ) {
	    if ( io_mask & RESTART ) sprintf(tag,"_%d",pass+1) ;
	    else                     sprintf(tag,"_%d",p->ntl) ;
          }
	  else              sprintf(tag,"") ; 

          /* construct variable name */
          if ( p->scalar_array_member )
	  {
	    strcpy(dexes,"") ;
            for (ii = 0; ii < p->ndims; ii++ )
	    {
	      switch(p->dims[ii]->coord_axis)
	      {
	      case(COORD_X): strcat(dexes,"ims,") ; break ;
	      case(COORD_Y): strcat(dexes,"jms,") ; break ;
	      case(COORD_Z): strcat(dexes,"kms,") ; break ;
	      default : break ;
	      }
	    }
            sprintf(vname,"%s%s%s(%sP_%s)",core,p->use,tag,dexes,p->name) ;
            sprintf(vname_2,"%s%s%s(%sP_%s)",core,p->use,"_2",":,:,:,",p->name) ;
            sprintf(vname_1,"%s%s%s(%sP_%s)",core,p->use,"_1",":,:,:,",p->name) ;
            sprintf(vname_x,"%s%s%s(%sP_%s)",core,p->use,tag,":,:,:,",p->name) ;
	  }
	  else
	  {
            sprintf(vname,"%s%s%s",core,p->name,tag) ;
            sprintf(vname_x,"%s%s%s",core,p->name,tag) ;
            sprintf(vname_1,"%s%s%s",core,p->name,"_1") ;
            sprintf(vname_2,"%s%s%s",core,p->name,"_2") ;
	  }

          /* construct data name -- maybe same as vname if dname not spec'd  */
          if ( strlen(p->dname) == 0 || !strcmp(p->dname,"-") ) { strcpy(dname_tmp,p->name) ; }
          else                                                  { strcpy(dname_tmp,p->dname) ; }
          make_upper_case(dname_tmp) ;

          if ( io_mask & RESTART &&  p->ntl > 1 ) sprintf(dname,"%s_%d",dname_tmp,pass+1) ;
          else                                    strcpy(dname,dname_tmp) ;

          set_mem_order( p, memord , NAMELEN) ;

/* kludge for WRF 3DVAR I/O with MM5 analysis kernel */
          if ( sw_3dvar_iry_kludge && !strcmp(memord,"XYZ") ) sprintf(memord,"YXZ") ;
          if ( sw_3dvar_iry_kludge && !strcmp(memord,"XY") ) sprintf(memord,"YX") ;

          if ( strlen(dname) < 1 ) {
            fprintf(stderr,"gen_wrf_io.c: Registry WARNING: no data name for %s \n",p->name) ;
          }
          if ( p->io_mask & io_mask && sw_io == GEN_INPUT )
          {
	    if ( !strncmp( p->use, "dyn_", 4 ) ) 
	      fprintf(fp,"IF ( grid%%dyn_opt .EQ. %s ) THEN\n",p->use) ;
	    if ( p->scalar_array_member )
	      fprintf(fp,"IF ( P_%s .GE. PARAM_FIRST_SCALAR ) THEN\n",p->name) ;
            if ( ok_to_collect_distribute )
              fprintf(fp,"IF ( wrf_dm_on_monitor() ) THEN\n") ;

            strcpy(indices,"") ;
            sprintf(post,")") ;
            if ( sw_io_deref_kludge && !(p->scalar_array_member) )   /* these aready have */
            {
              sprintf(indices, "%s",index_with_firstelem("(","grid%",t2,p,post)) ;
            }

	    fprintf(fp,"CALL wrf_ext_read_field (  &\n") ;
	    fprintf(fp,"                       fid                , &  ! DataHandle \n" ) ;
	    fprintf(fp,"                       current_date(1:19) , &  ! DateStr \n" ) ;
	    fprintf(fp,"                       '%s'               , &  ! Data Name \n", dname ) ;
            if ( p->ndims >= 2 && ok_to_collect_distribute )
	      fprintf(fp,"                       globbuf_%s               , &  ! Field \n" , p->type->name ) ;
            else
	      fprintf(fp,"                       %s%s%s               , &  ! Field \n" , structname , vname , indices) ;
          if ( !strcmp(p->type->name,"doubleprecision") )
            { fprintf(fp,"                       WRF_DOUBLE         , &  ! FieldType \n" ) ; }
          else
	    { fprintf(fp,"                       WRF_%s             , &  ! FieldType \n" , p->type->name ) ; }
	    fprintf(fp,"                       grid%%communicator  , &  ! Comm\n") ;
	    fprintf(fp,"                       grid%%iocommunicator  , &  ! Comm\n") ;
	    fprintf(fp,"                       grid%%domdesc       , &  ! Comm\n") ;
	    fprintf(fp,"                       grid%%bdy_mask     , &  ! bdy_mask\n") ;
	    fprintf(fp,"                       '%s'               , &  ! MemoryOrder\n",memord ) ;
	    fprintf(fp,"                       '%s'               , &  ! Stagger\n",stagstr ) ;
	    fprintf(fp,"'%s ext_read_field %s memorder %s' , & ! Debug message\n",fname,dname,memord ) ;
	    /* global dimensions */
	    for ( i = 0 ; i < 3 ; i++ ) { fprintf(fp,"%s , %s , ",ddim[i][0], ddim[i][1]) ; }
	    fprintf(fp," & \n") ;

/* the first two cases here have to do with if we're running on multiple distributed
   memory processors and the i/o api layer can't handle decomposed data. So code is
   generated to read the data on processor zero into a globally sized buffer. In this
   case, then the domain, memory, and patch dimensions for the globally sized buffer
   are all just the domain dimensions. Two D arrays are handled separately
   from three-d arrays because in threeD arrays the middle index is K.  In the last
   case, where the code is either calling a version of the API that supports parallelism
   or we aren't running in DM-parallel, the field itself and not a global buffer are
   passed, so we pass the domain, memory, and patch indices directly to the read routine. */

            if      ( p->ndims == 3 && ok_to_collect_distribute )
	    {
	      /* mem    dimensions are actually domain dimensions */
	      for ( i = 0 ; i < 3 ; i++ ) { fprintf(fp,"%s , %s , ",ddim_no[i][0], ddim_no[i][1]) ; }
	      fprintf(fp," & \n") ;
	      /* patch  dimensions are actually domain dimensions */
	      for ( i = 0 ; i < 3 ; i++ ) { fprintf(fp,"%s , %s , ",ddim   [i][0], ddim   [i][1]) ; }
	      fprintf(fp," & \n") ;
	    }
	    else if ( p->ndims == 2 && ok_to_collect_distribute )
	    {
	      if ((xi=get_index_for_coord(p,COORD_X))>=0&&(yi=get_index_for_coord(p,COORD_Y))>=0)
	      {
	        /* mem    dimensions are actually domain dimensions */
                fprintf(fp, "%s, %s, %s, %s, 1 , 1 , &\n",ddim_no[xi][0],ddim_no[xi][1],
							  ddim_no[yi][0],ddim_no[yi][1] ) ;
	      /* patch  dimensions are actually domain dimensions */
                fprintf(fp, "%s, %s, %s, %s, 1 , 1 , &\n",ddim   [xi][0],ddim   [xi][1],
							  ddim   [yi][0],ddim   [yi][1] ) ;
	      }
	    }
	    else
	    {
	      /* mem    dimensions */
	      for ( i = 0 ; i < 3 ; i++ ) { fprintf(fp,"%s , %s , ",mdim[i][0], mdim[i][1]) ; }
	      fprintf(fp," & \n") ;
	      /* patch  dimensions */
	      for ( i = 0 ; i < 3 ; i++ ) { fprintf(fp,"%s , %s , ",pdim[i][0], pdim[i][1]) ; }
	      fprintf(fp," & \n") ;
	    }
	    fprintf(fp,"                       ierr )\n") ;

            if ( ok_to_collect_distribute )
	      fprintf(fp,"END IF\n" ) ;

/* In case we have read into a global buffer, generate code to distribute the data just read in */
            if      ( p->ndims == 3 && ok_to_collect_distribute )
	    {
	      if ((xi=get_index_for_coord(p,COORD_X))>=0&&(yi=get_index_for_coord(p,COORD_Y))>=0&&(zi=get_index_for_coord(p,COORD_Z))>=0)
	      {
	        fprintf(fp,"call wrf_global_to_patch_%s ( globbuf_%s , %s%s , &\n",p->type->name,p->type->name,structname , vname ) ;
	        fprintf(fp,"       grid%%domdesc, %d, &\n",p->ndims) ;
                fprintf(fp, "%s, %s, %s, %s, %s, %s, &\n",ddim_no[xi][0],ddim_no[xi][1],
							  ddim_no[yi][0],ddim_no[yi][1],
							  ddim_no[zi][0],ddim_no[zi][1]) ;
                fprintf(fp, "%s, %s, %s, %s, %s, %s, &\n",mdim_no[xi][0],mdim_no[xi][1],
							  mdim_no[yi][0],mdim_no[yi][1],
							  mdim_no[zi][0],mdim_no[zi][1]) ;
                fprintf(fp, "%s, %s, %s, %s, %s, %s  )\n",pdim_no[xi][0],pdim_no[xi][1],
							  pdim_no[yi][0],pdim_no[yi][1],
							  pdim_no[zi][0],pdim_no[zi][1]) ;
	      }
	    }
	    else if ( p->ndims == 2 && ok_to_collect_distribute )
	    {
	      if ((xi=get_index_for_coord(p,COORD_X))>=0&&(yi=get_index_for_coord(p,COORD_Y))>=0)
	      {
	        fprintf(fp,"call wrf_global_to_patch_%s ( globbuf_%s , %s%s , &\n",p->type->name,p->type->name,structname , vname ) ;
	        fprintf(fp,"       grid%%domdesc, %d, &\n",p->ndims) ;
                fprintf(fp, "%s, %s, %s, %s, 1 , 1 , &\n",ddim_no[xi][0],ddim_no[xi][1],
							  ddim_no[yi][0],ddim_no[yi][1] ) ;
                fprintf(fp, "%s, %s, %s, %s, 1 , 1 , &\n",mdim_no[xi][0],mdim_no[xi][1],
							  mdim_no[yi][0],mdim_no[yi][1] ) ;
                fprintf(fp, "%s, %s, %s, %s, 1 , 1   )\n",pdim_no[xi][0],pdim_no[xi][1],
							  pdim_no[yi][0],pdim_no[yi][1] ) ;
	      }
	      else
	      {
	        fprintf(stderr,"gen_wrf_io.c: Registry WARNING (and possibly internal error) %s \n",p->name) ;
	      }
	    }
	    else if ( !strcmp(memord,"Z") && ok_to_collect_distribute )
	    {
	      fprintf(fp," call wrf_dm_bcast_%s ( %s%s , (%s)-(%s)+1 )\n",p->type->name,structname,vname,ddim[0][1],ddim[0][0] ) ;
	    }
	    else if ( !strcmp(memord,"0") && ok_to_collect_distribute )
	    {
	      fprintf(fp," call wrf_dm_bcast_%s ( %s%s , 1 )\n",p->type->name,structname,vname ) ;

	    }
	    else if ( ok_to_collect_distribute )
	    {
	      fprintf(stderr,"gen_wrf_io.c: Registry WARNING: can't figure out entry for %s (Memord %s)\n",p->name,memord) ;
	    }

	    if ( io_mask & INPUT && p->ntl > 1 ) {
	      /* copy time level two into time level one */
	      if ( p->ntl == 3 ) fprintf(fp, "grid%%%s = grid%%%s\n", vname_2 , vname_x ) ;
	      if ( p->ntl == 2 ) fprintf(fp, "grid%%%s = grid%%%s\n", vname_1 , vname_x ) ;
	    }

	    if ( p->scalar_array_member )
	    {
	      fprintf(fp,"END IF\n" ) ;
	    }

	    if ( !strncmp( p->use, "dyn_", 4 ) ) 
	      fprintf(fp,"END IF\n" ) ;
          }
          else if ( sw_io == GEN_OUTPUT )
	  {
	    if ( !strncmp( p->use, "dyn_", 4 ) ) 
	      fprintf(fp,"IF ( grid%%dyn_opt .EQ. %s ) THEN\n",p->use) ;
	    if ( p->scalar_array_member )
	      fprintf(fp,"IF ( P_%s .GE. PARAM_FIRST_SCALAR ) THEN\n",p->name) ;

/* Genereate code to write into a global buffer if it's DM-parallel and I/O API cannot handle distributed data  */

            if      ( p->ndims == 3 && ok_to_collect_distribute )
	    {
	      if ((xi=get_index_for_coord(p,COORD_X))>=0&&(yi=get_index_for_coord(p,COORD_Y))>=0&&(zi=get_index_for_coord(p,COORD_Z))>=0)
	      {
	        fprintf(fp,"IF ( .NOT. dryrun ) call wrf_patch_to_global_%s ( %s%s , globbuf_%s , &\n",p->type->name,structname,vname,p->type->name ) ;
	        fprintf(fp,"       grid%%domdesc, %d, &\n",p->ndims) ;
/*              fprintf(fp, "ids , ide , jds , jde , kds , kde ,                &\n")  ; */
                fprintf(fp, "%s, %s, %s, %s, %s, %s, &\n",ddim_no[xi][0],ddim_no[xi][1],
							  ddim_no[yi][0],ddim_no[yi][1],
							  ddim_no[zi][0],ddim_no[zi][1]) ;
                fprintf(fp, "%s, %s, %s, %s, %s, %s, &\n",mdim_no[xi][0],mdim_no[xi][1],
							  mdim_no[yi][0],mdim_no[yi][1],
							  mdim_no[zi][0],mdim_no[zi][1]) ;
                fprintf(fp, "%s, %s, %s, %s, %s, %s  )\n",pdim_no[xi][0],pdim_no[xi][1],
							  pdim_no[yi][0],pdim_no[yi][1],
							  pdim_no[zi][0],pdim_no[zi][1]) ;
	      }
	    }
	    else if ( p->ndims == 2 && ok_to_collect_distribute )
	    {
	      if ((xi=get_index_for_coord(p,COORD_X))>=0&&(yi=get_index_for_coord(p,COORD_Y))>=0)
	      {
	        fprintf(fp,"IF ( .NOT. dryrun ) call wrf_patch_to_global_%s ( %s%s , globbuf_%s , &\n",p->type->name,structname,vname,p->type->name ) ;
	        fprintf(fp,"       grid%%domdesc, %d, &\n",p->ndims) ;
/*              fprintf(fp, "ids , ide , jds , jde , 1 , 1 ,                &\n")  ; */
                fprintf(fp, "%s, %s, %s, %s, 1 , 1 , &\n",ddim_no[xi][0],ddim_no[xi][1],
							  ddim_no[yi][0],ddim_no[yi][1] ) ;
                fprintf(fp, "%s, %s, %s, %s, 1 , 1 , &\n",mdim_no[xi][0],mdim_no[xi][1],
							  mdim_no[yi][0],mdim_no[yi][1] ) ;
                fprintf(fp, "%s, %s, %s, %s, 1 , 1   )\n",pdim_no[xi][0],pdim_no[xi][1],
							  pdim_no[yi][0],pdim_no[yi][1] ) ;
	      }
	      else
	      {
	        fprintf(stderr,"gen_wrf_io.c: Registry WARNING (and possibly internal error) %s \n",p->name) ;
	      }
	    }
         
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
		  else 
		   { strcpy( dimname[i], dimnode->dim_data_name) ; }
		  break ;
	        case (COORD_Y) : 
		  if ( ( ! sw_3dvar_iry_kludge && p->stag_y ) || ( sw_3dvar_iry_kludge && p->stag_x ) )
		   { sprintf( dimname[i] ,"%s_stag", dimnode->dim_data_name) ; } 
		  else 
		   { strcpy( dimname[i], dimnode->dim_data_name) ; }
		  break ;
	        case (COORD_Z) : 
		  if ( p->stag_z ) 
		   { sprintf( dimname[i] ,"%s_stag", dimnode->dim_data_name) ; } 
		  else 
		   { strcpy( dimname[i], dimnode->dim_data_name) ; }
		  break ;
	        }
	      }
	    }

            if ( ok_to_collect_distribute )
              fprintf(fp,"IF ( wrf_dm_on_monitor() ) THEN\n") ;

            strcpy(indices,"") ;
            sprintf(post,")") ;
            if ( sw_io_deref_kludge && !(p->scalar_array_member) )   /* these aready have */
            {
              sprintf(indices, "%s",index_with_firstelem("(","grid%",t2,p,post)) ;
            }

	    fprintf(fp,"CALL wrf_ext_write_field (  &\n") ;
	    fprintf(fp,"                       fid                , &  ! DataHandle \n" ) ;
	    fprintf(fp,"                       current_date(1:19) , &  ! DateStr \n" ) ;
	    fprintf(fp,"                       '%s'               , &  ! Data Name \n", dname ) ;
            if ( p->ndims >= 2 && ok_to_collect_distribute )
	      fprintf(fp,"                       globbuf_%s               , &  ! Field \n" , p->type->name ) ;
            else
	      fprintf(fp,"                       %s%s%s               , &  ! Field \n" , structname , vname , indices ) ;
          if ( !strcmp(p->type->name,"doubleprecision") )
            { fprintf(fp,"                       WRF_DOUBLE         , &  ! FieldType \n" ) ; }
          else
	    { fprintf(fp,"                       WRF_%s             , &  ! FieldType \n" , p->type->name ) ; }
	    fprintf(fp,"                       grid%%communicator  , &  ! Comm\n") ;
	    fprintf(fp,"                       grid%%iocommunicator  , &  ! Comm\n") ;
	    fprintf(fp,"                       grid%%domdesc       , &  ! Comm\n") ;
	    fprintf(fp,"                       grid%%bdy_mask       , &  ! bdy_mask\n") ;
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
	    for ( i = 0 ; i < 3 ; i++ ) { fprintf(fp,"%s , %s , ",ddim[i][0], ddim[i][1]) ; }
	    fprintf(fp," & \n") ;

/* the first two cases here have to do with if we're running on multiple distributed
   memory processors and the i/o api layer can't handle decomposed data. So code is
   generated to read the data on processor zero into a globally sized buffer. In this
   case, then the domain, memory, and patch dimensions for the globally sized buffer
   are all just the domain domain dimensions. Two D arrays are handled separately
   from three-d arrays because in threeD arrays the middle index is K.  In the last
   case, where the code is either calling a version of the API that supports parallelism
   or we aren't running in DM-parallel, the field itself and not a global buffer are
   passed, so we pass the domain, memory, and patch indices directly to the read routine. */

            if      ( p->ndims == 3 && ok_to_collect_distribute )
	    {
	      /* mem    dimensions are actually domain dimensions */
	      for ( i = 0 ; i < 3 ; i++ ) { fprintf(fp,"%s , %s , ",ddim_no[i][0], ddim_no[i][1]) ; }
	      fprintf(fp," & \n") ;
	      /* patch  dimensions are actually domain dimensions */
/*	      for ( i = 0 ; i < 3 ; i++ ) { fprintf(fp,"%s , %s , ",ddim_no[i][0], ddim_no[i][1]) ; } */
	      for ( i = 0 ; i < 3 ; i++ ) { fprintf(fp,"%s , %s , ",ddim[i][0], ddim[i][1]) ; }
	      fprintf(fp," & \n") ;
	    }
	    else if ( p->ndims == 2 && ok_to_collect_distribute )
	    {
	      if ((xi=get_index_for_coord(p,COORD_X))>=0&&(yi=get_index_for_coord(p,COORD_Y))>=0)
	      {
	        /* mem    dimensions are actually domain dimensions */
                fprintf(fp, "%s, %s, %s, %s, 1 , 1 , &\n",ddim_no[xi][0],ddim_no[xi][1],
							  ddim_no[yi][0],ddim_no[yi][1] ) ;
	      /* patch  dimensions are actually domain dimensions */
/*              fprintf(fp, "%s, %s, %s, %s, 1 , 1 , &\n",ddim_no[xi][0],ddim_no[xi][1],
							  ddim_no[yi][0],ddim_no[yi][1] ) ; */
                fprintf(fp, "%s, %s, %s, %s, 1 , 1 , &\n",ddim[xi][0],ddim[xi][1],
							  ddim[yi][0],ddim[yi][1] ) ;
	      }
	    }
	    else
	    {
	      /* mem    dimensions */
	      for ( i = 0 ; i < 3 ; i++ ) { fprintf(fp,"%s , %s , ",mdim[i][0], mdim[i][1]) ; }
	      fprintf(fp," & \n") ;
	      /* patch  dimensions */
	      for ( i = 0 ; i < 3 ; i++ ) { fprintf(fp,"%s , %s , ",pdim[i][0], pdim[i][1]) ; }
	      fprintf(fp," & \n") ;
	    }
	    fprintf(fp,"                       ierr )\n") ;

            if ( ok_to_collect_distribute )
	      fprintf(fp,"END IF\n" ) ;

	    if ( p->scalar_array_member )
	      fprintf(fp,"END IF\n" ) ;
	    if ( !strncmp( p->use, "dyn_", 4 ) ) 
	      fprintf(fp,"END IF\n" ) ;

          }
        }
      }
    }
    if ( p->type->type_type == DERIVED )
    {
      sprintf(x,"%s%s%%",structname,p->name ) ;
      gen_wrf_io2(fp, fname, x, p->type, io_mask, sw_io ) ;
    }

    }
  }
  return(0) ;
}

