#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

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

#if 1

  OP_F(fp,"wrf_metaput_input.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields ,
      METADATA | INPUT , GEN_OUTPUT ) ;

  OP_F(fp,"wrf_metaput_restart.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields ,
      METADATA | RESTART , GEN_OUTPUT ) ;

  OP_F(fp,"wrf_metaput_history.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields ,
      METADATA | HISTORY , GEN_OUTPUT ) ;

  OP_F(fp,"wrf_metaput_boundary.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields ,
      METADATA | BOUNDARY , GEN_OUTPUT ) ;

  OP_F(fp,"wrf_histout.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , HISTORY , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist1out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST1 , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist2out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST2 , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist3out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST3 , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist4out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST4 , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist5out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST5 , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist6out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST6 , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist7out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST7 , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist8out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST8 , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist9out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST9 , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist10out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST10 , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist11out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST11 , GEN_OUTPUT ) ;
  close_the_file(fp) ;

  OP_F(fp,"wrf_inputout.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , INPUT   , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput1out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT1   , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput2out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT2   , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput3out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT3   , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput4out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT4   , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput5out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT5   , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput6out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT6   , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput7out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT7   , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput8out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT8   , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput9out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT9   , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput10out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT10   , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput11out.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT11   , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_restartout.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , RESTART , GEN_OUTPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_bdyout.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , BOUNDARY , GEN_OUTPUT ) ;
  close_the_file(fp) ;
#endif

#if 1
  OP_F(fp,"wrf_metaget_input.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , 
      METADATA | INPUT , GEN_INPUT ) ;

  OP_F(fp,"wrf_metaget_restart.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , 
      METADATA | RESTART , GEN_INPUT ) ;

  OP_F(fp,"wrf_metaget_history.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , 
      METADATA | HISTORY , GEN_INPUT ) ;

  OP_F(fp,"wrf_metaget_boundary.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , 
      METADATA | BOUNDARY , GEN_INPUT ) ;

  OP_F(fp,"wrf_histin.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , HISTORY , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist1in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST1 , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist2in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST2 , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist3in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST3 , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist4in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST4 , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist5in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST5 , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist6in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST6 , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist7in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST7 , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist8in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST8 , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist9in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST9 , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist10in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST10 , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxhist11in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXHIST11 , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_inputin.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , INPUT   , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput1in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT1   , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput2in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT2   , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput3in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT3   , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput4in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT4   , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput5in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT5   , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput6in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT6   , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput7in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT7   , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput8in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT8   , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput9in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT9   , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput10in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT10   , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_auxinput11in.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , AUXINPUT11   , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_restartin.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , RESTART , GEN_INPUT ) ;
  close_the_file(fp) ;
  OP_F(fp,"wrf_bdyin.inc") ;
  gen_wrf_io2 ( fp , fname, "grid%" , NULL, Domain.fields , BOUNDARY , GEN_INPUT ) ;
  close_the_file(fp) ;
#endif

  return(0) ;
}

static int
set_dim_strs_x ( node_t *node , char ddim[3][2][NAMELEN], char mdim[3][2][NAMELEN], char pdim[3][2][NAMELEN] , char * prepend , int sw_disregard_stag, int sw_reorder )
{
  int i, j, ii ;
  node_t *p ;
  char d, d1 ;
  char * stag ;
  char r1[NAMELEN] ;

  strcpy(r1,"grid%") ;
  if ( node == NULL ) return(1) ;
  for ( i = 0 ; i < 3 ; i++ )
    for ( j = 0 ; j < 2 ; j++ )
      {
        strcpy(ddim[i][j],"1") ;
        strcpy(mdim[i][j],"1") ;
        strcpy(pdim[i][j],"1") ;
      }

  for ( ii = 0 ; ii < ((node->ndims > 3)?3:node->ndims) ; ii++ )
  {
    p = node->dims[ii] ;
    if ( sw_reorder ) { 
      i = ii ;
    } else {
      switch( p->coord_axis )
      {
      case(COORD_X) : i = 0 ; break ;
      case(COORD_Y) : i = 2 ; break ;
      case(COORD_Z) : i = 1 ; break ;
      default :  break ;
      }
    }
    if      ( p->len_defined_how == DOMAIN_STANDARD )
    {
      if ( p->subgrid ) {
          switch( p->coord_axis )
          {
          case(COORD_X) : d = 'i' ;  d1 = 'x' ; break ;
          case(COORD_Y) : d = 'j' ;  d1 = 'y' ; break ;
          case(COORD_Z) : d = 'k' ;  d1 = 'z' ; break ;
          default :  break ;
          }

        sprintf(ddim[i][0],"%s%cds",prepend,d) ;
        sprintf(ddim[i][1],"%s%cde * %ssr_%c ",prepend,d,r1,d1) ;
        sprintf(mdim[i][0],"(%s%cms-1)*%ssr_%c+1",prepend,d,r1,d1) ;
        sprintf(mdim[i][1],"%s%cme*%ssr_%c",prepend,d,r1,d1) ;
        sprintf(pdim[i][0],"(%s%cps-1)*%ssr_%c+1",prepend,d,r1,d1) ;
        sprintf(pdim[i][1],"%s%cpe*%ssr_%c",prepend,d,r1,d1) ;

      } else {
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
set_dim_strs ( node_t *node , char ddim[3][2][NAMELEN], char mdim[3][2][NAMELEN], char pdim[3][2][NAMELEN] , char * prepend , int sw_disregard_stag )
{
  set_dim_strs_x ( node , ddim, mdim, pdim, prepend , sw_disregard_stag, 1 ) ; /* 1 = reorder according to strg order */            
}

/* version that doesn't permute according to index order -- always i, k, then j
   useful for standard argument lists -- e.g. calls to interp in nesting  */
int
set_dim_strs2 ( node_t *node , char ddim[3][2][NAMELEN], char mdim[3][2][NAMELEN], char pdim[3][2][NAMELEN] , char * prepend , int sw_disregard_stag )
{
  set_dim_strs_x ( node , ddim, mdim, pdim, prepend , sw_disregard_stag, 0 ) ; /* 0 = reorder according to strg order */
}

int
gen_wrf_io2 ( FILE * fp , char * fname, char * structname , char * fourdname, node_t * node , int io_mask , int sw_io )
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

  ok_to_collect_distribute = !sw_distrib_io_layer && 
                              sw_dm_parallel && 
                             !(sw_dm_serial_in_only && ((io_mask&HISTORY)  ||
                                                        (io_mask&AUXHIST1) ||
                                                        (io_mask&AUXHIST2) ||
                                                        (io_mask&AUXHIST3) ||
                                                        (io_mask&AUXHIST4) ||
                                                        (io_mask&AUXHIST5) ||
                                                        (io_mask&AUXHIST6) ||
                                                        (io_mask&AUXHIST7) ||
                                                        (io_mask&AUXHIST8) ||
                                                        (io_mask&AUXHIST9) ||
                                                        (io_mask&AUXHIST10) ||
                                                        (io_mask&AUXHIST11) ||
                                                        (io_mask&RESTART))) ;

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


      if ( ! ( io_mask & BOUNDARY ) )
      {
        int d ;
        char moredims[80], formatdims[80], temp[10], temp2[80],tx[80],r[80], *colon  ;
        set_mem_order( p->members, memord , NAMELEN) ;
        memord[3] = '\0' ; /* snip off any extra dimensions */
fprintf(fp,"DO itrace = PARAM_FIRST_SCALAR , num_%s\n",p->name ) ;
        strcpy(moredims,"") ; strcpy(formatdims,"") ;
        for ( d = 3 ; d < p->ndims ; d++ ) {
          strcpy(r,""); 
          range_of_dimension( r, tx , d , p , "config_flags%" ) ;
          colon = index(tx,':') ; if ( colon != NULL ) *colon = ',' ;
          sprintf(temp,"idim%d",d-2) ;
          strcat(moredims,",") ; strcat(moredims,temp) ;
          strcat(formatdims,"\"_\",I5.5,") ;
fprintf(fp,"  DO %s = %s\n",temp,tx ) ;
        }
        formatdims[strlen(formatdims)-1] = '\0' ;
        strcat(moredims,",") ;
fprintf(fp,"  IF (BTEST(%s_stream_table(grid%%id, itrace ) , switch )) THEN\n",p->name) ;
        if ( p->ndims > 3 ) {
           strcpy(temp2,moredims+1) ; temp2[strlen(temp2)-1] = '\0' ;
fprintf(fp,"    WRITE(extradims,'(%s)')%s\n",formatdims,temp2) ;
        }
fprintf(fp,"    CALL wrf_ext_%s_field (  &\n", (sw_io == GEN_INPUT)?"read":"write" ) ;
fprintf(fp,"          fid                             , &  ! DataHandle\n") ;
fprintf(fp,"          current_date(1:19)              , &  ! DateStr\n") ; 
        if ( p->ndims > 3 ) {
fprintf(fp,"          TRIM(%s_dname_table( grid%%id, itrace ))//TRIM(extradims), & !data name\n",p->name) ;
        } else {
fprintf(fp,"          TRIM(%s_dname_table( grid%%id, itrace )), & !data name\n",p->name) ;
        }
        strcpy( tl, "" ) ;
        if ( p->members->ntl > 1 && p->members->ntl <= 3 ) sprintf( tl, "_%d",p->members->ntl ) ;
        if ( ok_to_collect_distribute ) {
fprintf(fp,"                       globbuf_%s               , &  ! Field \n",p->members->type->name ) ;
        } else {
           if        ( !strcmp(memord,"XYZ") ) {
fprintf(fp,"          grid%%%s%s(ims,jms,kms%sitrace)  , &  ! Field\n",p->name,tl,moredims) ;
           } else if ( !strcmp(memord,"YXZ") ) {
fprintf(fp,"          grid%%%s%s(jms,ims,kms%sitrace)  , &  ! Field\n",p->name,tl,moredims) ;
           } else if ( !strcmp(memord,"XZY") ) {
fprintf(fp,"          grid%%%s%s(ims,kms,jms%sitrace)  , &  ! Field\n",p->name,tl,moredims) ;
           } else if ( !strcmp(memord,"YZX") ) {
fprintf(fp,"          grid%%%s%s(jms,kms,ims%sitrace)  , &  ! Field\n",p->name,tl,moredims) ;
           } else if ( !strcmp(memord,"ZXY") ) {
fprintf(fp,"          grid%%%s%s(kms,ims,jms%sitrace)  , &  ! Field\n",p->name,tl,moredims) ;
           } else if ( !strcmp(memord,"ZYX") ) {
fprintf(fp,"          grid%%%s%s(kms,jms,ims%sitrace)  , &  ! Field\n",p->name,tl,moredims) ;
           }
        }
        if (!strncmp(p->members->type->name,"real",4)) {
          fprintf(fp,"                       WRF_FLOAT             , &  ! FieldType \n") ;
        } else {
          fprintf(fp,"                       WRF_%s             , &  ! FieldType \n" , p->members->type->name ) ;
        }
fprintf(fp,"          grid%%communicator  , &  ! Comm\n") ;
fprintf(fp,"          grid%%iocommunicator  , &  ! Comm\n") ;
fprintf(fp,"          grid%%domdesc       , &  ! Comm\n") ;
fprintf(fp,"          grid%%bdy_mask       , &  ! bdy_mask\n") ;
        if ( sw_io == GEN_OUTPUT ) {
fprintf(fp,"          dryrun             , &  ! flag\n") ;
        }
/* fprintf(stderr,"name %s memord %s\n",p->name,memord) ; */
fprintf(fp,"          '%s'               , &  ! MemoryOrder\n",memord) ;
        strcpy(stagstr, "") ;
        if ( p->members->stag_x ) strcat(stagstr, "X") ;
        if ( p->members->stag_y ) strcat(stagstr, "Y") ;
        if ( p->members->stag_z ) strcat(stagstr, "Z") ;
fprintf(fp,"          '%s'                , &  ! Stagger\n",stagstr) ;
        if ( sw_io == GEN_OUTPUT ) {
          for ( i = 0 ; i < 3 ; i++ ) strcpy(dimname[i],"") ;
          for ( i = 0 ; i < 3 ; i++ )
          {
            if (( dimnode = p->members->dims[i]) != NULL )
            {
              switch ( dimnode->coord_axis )
              {
              case (COORD_X) :
                if ( ( ! sw_3dvar_iry_kludge && p->members->stag_x ) || ( sw_3dvar_iry_kludge && p->members->stag_y ) )
                 { sprintf( dimname[i] ,"%s_stag", dimnode->dim_data_name) ; }
                else
                 { strcpy( dimname[i], dimnode->dim_data_name) ; }
                break ;
              case (COORD_Y) :
                if ( ( ! sw_3dvar_iry_kludge && p->members->stag_y ) || ( sw_3dvar_iry_kludge && p->members->stag_x ) )
                 { sprintf( dimname[i] ,"%s_stag", dimnode->dim_data_name) ; }
                else
                 { strcpy( dimname[i], dimnode->dim_data_name) ; }
                break ;
              case (COORD_Z) :
                if ( p->members->stag_z )
                 { sprintf( dimname[i] ,"%s_stag", dimnode->dim_data_name) ; }
                else
                 { strcpy( dimname[i], dimnode->dim_data_name) ; }
                break ;
              }
            }
          }
fprintf(fp,"                       '%s'               , &  ! Dimname 1 \n",dimname[0] ) ;
fprintf(fp,"                       '%s'               , &  ! Dimname 2 \n",dimname[1] ) ;
fprintf(fp,"                       '%s'               , &  ! Dimname 3 \n",dimname[2] ) ;
fprintf(fp,"          %s_desc_table( grid%%id, itrace  ), & ! Desc\n",p->name) ;
fprintf(fp,"          %s_units_table( grid%%id, itrace  ), & ! Units\n",p->name) ;
        }
fprintf(fp,"'%s ext_write_field '//TRIM(%s_dname_table( grid%%id, itrace ))//' memorder %s' , & ! Debug message\n", fname, p->name, memord ) ;
        /* global dimensions */
        for ( i = 0 ; i < 3 ; i++ ) { fprintf(fp,"%s , %s , ",ddim[i][0], ddim[i][1]) ; }
        fprintf(fp," & \n") ;
        /* mem    dimensions */
        for ( i = 0 ; i < 3 ; i++ ) { fprintf(fp,"%s , %s , ",mdim[i][0], mdim[i][1]) ; }
        fprintf(fp," & \n") ;
        /* patch  dimensions */
        for ( i = 0 ; i < 3 ; i++ ) { fprintf(fp,"%s , %s , ",pdim[i][0], pdim[i][1]) ; }
        fprintf(fp," & \n") ;
fprintf(fp,"                         ierr )\n" ) ;
fprintf(fp, "  ENDIF\n" ) ;
        for ( d = 3 ; d < p->ndims ; d++ ) {
fprintf(fp,"  ENDDO ! idim%d \n",d-2 ) ;
        }
fprintf(fp, "ENDDO\n") ;
      } 
/* BOUNDARY FOR 4-D TRACER */
      else if ( io_mask & BOUNDARY )
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
fprintf(fp,"  IF (BTEST(%s_stream_table(grid%%id, itrace ) , switch )) THEN\n",p->name) ;
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
fprintf(fp,"          grid%%communicator  , &  ! Comm\n") ;
fprintf(fp,"          grid%%iocommunicator  , &  ! Comm\n") ;
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

      if (  p->io_mask & BOUNDARY && (io_mask & BOUNDARY) && !( io_mask & METADATA ) 
         && strcmp( p->use, "_4d_bdy_array_" ) || ( io_mask & BOUNDARY && fourdname ) )
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
                fprintf(fp,"                       %s%s%s%s(%s,kds,1,P_%s)     , &  ! Field \n" , 
                                       structname , fourdname, tend_tag, ms1, bdy_indicator(ibdy), p->name ) ;
              } else {
                fprintf(fp,"                       %s%s%s%s(1,kds,1,%d,P_%s)     , &  ! Field \n" , 
                                       structname , fourdname, tend_tag, ibdy, bdy_indicator(ibdy), p->name ) ;
	      }
            }
            if (!strncmp(p->type->name,"real",4)) {
              fprintf(fp,"                       WRF_FLOAT          , &  ! FieldType \n") ;
            } else {
              fprintf(fp,"                       WRF_%s             , &  ! FieldType \n" , p->type->name ) ;
            }
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
            if ( ok_to_collect_distribute )
              fprintf(fp,"ENDIF\n") ;
	  }
        }
        }
      }

/* ////////  NOT BOUNDARY ///////////////////// */
     else if ( (p->io_mask & io_mask) && ! (io_mask & BOUNDARY))
     {

/* Aug 2004

Namelist variables

The i r and h settings will be reenabled but it will work a little
differently than i/o of regular state variables:

1) rather than being read or written as records to the dataset, they
will be gotten or put as time invariant meta data; in other words, they
will only be written once when the dataset is created as the other
metadata is now. This has the benefit of reducing the amount of I/O
traffic on each write (I can't remember, but that may be why the
reading and writing of rconfig data was turned off in the first
place).

2) All the rconfig variables will be gotten/put as metadata to input,
restart, history, and boundary datasets, regardless of what the 'i',
'r', and 'h' settings are.  Instead those settings will control the
behavior with respect to the input-from-namelist vs input-from-dataset
precedence issue that Bill raised.

In other words, if an rconfig entry has an 'i', 'r', or 'h' in the
Registry, the dataset value takes precedence over the namelist value.
Otherwise, say it is missing the 'i', the reconfig variable's value
still appears as metadata in the dataset but the value of the variable
in the program does not change as a result of inputting the dataset.

*/

      if ( (p->node_kind & RCONFIG) && ( io_mask & METADATA ) )
      {
        char c ;
        char dname[NAMELEN] ;

        strcpy( dname, p->dname ) ; 
        make_upper_case( dname ) ;
        if      ( !strcmp( p->type->name , "integer" )         ) { c = 'i' ; }
        else if ( !strcmp( p->type->name , "real" )            ) { c = 'r' ; }
        else if ( !strcmp( p->type->name , "doubleprecision" ) ) { c = 'd' ; }
        else if ( !strcmp( p->type->name , "logical" )         ) { c = 'l' ; }
        else {
          fprintf(stderr,"REGISTRY WARNING: unknown type %s for %s\n",p->type->name,p->name ) ;
        }
        if ( sw_io == GEN_OUTPUT ) {
          if ( io_mask & p->io_mask ) {
            fprintf(fp,"CALL rconfig_get_%s ( grid%%id, %cbuf(1) )\n",p->name,c) ;
            fprintf(fp," CALL wrf_put_dom_ti_%s ( fid , '%s', %cbuf(1), 1, ierr )\n",p->type->name,dname,c) ;
          }
        } else {
          if ( io_mask & p->io_mask ) {
            fprintf(fp,"CALL wrf_get_dom_ti_%s ( fid , '%s', %cbuf(1), 1, ierr )\n",p->type->name,dname,c) ;
            fprintf(fp," WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_%s for %s returns ',%cbuf(1)\n",p->type->name,dname,c) ;
            fprintf(fp," CALL wrf_debug ( 300 , wrf_err_message )\n") ;
            fprintf(fp," CALL rconfig_set_%s ( grid%%id, %cbuf(1) )\n",p->name,c) ;
          }
        }
      }
/* end Aug 2004 */
#if 0
      else if ( ! (io_mask & METADATA) )   /* state vars */
#else
      else if ( ! (io_mask & METADATA) && ! (p->node_kind & RCONFIG) )   /* state vars */
#endif
      {
        if ( io_mask & RESTART && p->ntl > 1 ) passes = p->ntl ;
        else                                   passes = 1 ;

        for ( pass = 0 ; pass < passes ; pass++ )   /* for multi timelevel vars */
        {
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
            sprintf(vname,"%s%s(%sP_%s)",p->use,tag,dexes,p->name) ;
            sprintf(vname_2,"%s%s(%sP_%s)",p->use,"_2",":,:,:,",p->name) ;
            sprintf(vname_1,"%s%s(%sP_%s)",p->use,"_1",":,:,:,",p->name) ;
            sprintf(vname_x,"%s%s(%sP_%s)",p->use,tag,":,:,:,",p->name) ;
	  }
	  else
	  {
            sprintf(vname,"%s%s",p->name,tag) ;
            sprintf(vname_x,"%s%s",p->name,tag) ;
            sprintf(vname_1,"%s%s",p->name,"_1") ;
            sprintf(vname_2,"%s%s",p->name,"_2") ;
	  }


          /* construct data name -- maybe same as vname if dname not spec'd  */
          if ( strlen(p->dname) == 0 || !strcmp(p->dname,"-") ) { strcpy(dname_tmp,p->name) ; }
          else                                                  { strcpy(dname_tmp,p->dname) ; }
          make_upper_case(dname_tmp) ;

/*
   July 2004

   New code to generate error if input or output for two state variables would be generated with the same dataname

   example wrong:
     misc    tg      "SOILTB"   -> gen_tg,SOILTB
     misc    soiltb  "SOILTB"   -> gen_soiltb,SOILTB

*/
if ( pass == 0 )
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
            fprintf(stderr,"REGISTRY ERROR: Data-name collision on %s for %s \n",
              dname_tmp,p->name ) ;
          }
}
/* end July 2004 */

          if ( io_mask & RESTART &&  p->ntl > 1 ) sprintf(dname,"%s_%d",dname_tmp,pass+1) ;
          else                                    strcpy(dname,dname_tmp) ;

          set_mem_order( p, memord , NAMELEN) ;

/* kludge for WRF 3DVAR I/O with MM5 analysis kernel */
          if ( sw_3dvar_iry_kludge && !strcmp(memord,"XYZ") ) sprintf(memord,"YXZ") ;
          if ( sw_3dvar_iry_kludge && !strcmp(memord,"XY") ) sprintf(memord,"YX") ;

          if ( strlen(dname) < 1 ) {
            fprintf(stderr,"gen_wrf_io.c: Registry WARNING:: no data name for %s \n",p->name) ;
          }
          if ( p->io_mask & io_mask && sw_io == GEN_INPUT )
          {
	    if ( p->scalar_array_member )
	      fprintf(fp,"IF ( P_%s .GE. PARAM_FIRST_SCALAR ) THEN\n",p->name) ;
            if ( ok_to_collect_distribute )
              fprintf(fp,"IF ( wrf_dm_on_monitor() ) THEN\n") ;

            strcpy(indices,"") ;
            sprintf(post,")") ;
            if ( sw_io_deref_kludge && !(p->scalar_array_member) )   /* these aready have */
            {
              sprintf(indices, "%s",index_with_firstelem("(","grid%",-1,t2,p,post)) ;
            }

	    fprintf(fp,"IF ( in_use_for_config(grid%%id,'%s') ) THEN\n",vname) ;
	    fprintf(fp,"CALL wrf_ext_read_field (  &\n") ;
	    fprintf(fp,"                       fid                , &  ! DataHandle \n" ) ;
	    fprintf(fp,"                       current_date(1:19) , &  ! DateStr \n" ) ;
	    fprintf(fp,"                       '%s'               , &  ! Data Name \n", dname ) ;
            if ( p->ndims >= 2 && ok_to_collect_distribute )
	      fprintf(fp,"                       globbuf_%s               , &  ! Field \n" , p->type->name ) ;
            else
	      fprintf(fp,"                       %s%s%s               , &  ! Field \n" , structname , vname , indices) ;

            if (!strncmp(p->type->name,"real",4)) {
              fprintf(fp,"                       WRF_FLOAT             , &  ! FieldType \n") ;
            } else {
              fprintf(fp,"                       WRF_%s             , &  ! FieldType \n" , p->type->name ) ;
            }

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
	    fprintf(fp,"END IF\n" ) ;

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

          }
          else if ( sw_io == GEN_OUTPUT )
	  {
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
		  else if ( p->dims[i]->subgrid ) 
                   { sprintf( dimname[i] ,"%s_subgrid", dimnode->dim_data_name) ; }
		  else 
		   { strcpy( dimname[i], dimnode->dim_data_name) ; }
		  break ;
	        case (COORD_Y) : 
		  if ( ( ! sw_3dvar_iry_kludge && p->stag_y ) || ( sw_3dvar_iry_kludge && p->stag_x ) )
		   { sprintf( dimname[i] ,"%s_stag", dimnode->dim_data_name) ; } 
		  else if ( p->dims[i]->subgrid ) 
                   { sprintf( dimname[i] ,"%s_subgrid", dimnode->dim_data_name) ; }
		  else 
		   { strcpy( dimname[i], dimnode->dim_data_name) ; }
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

            if ( ok_to_collect_distribute )
              fprintf(fp,"IF ( wrf_dm_on_monitor() ) THEN\n") ;

            strcpy(indices,"") ;
            sprintf(post,")") ;
            if ( sw_io_deref_kludge && !(p->scalar_array_member) )   /* these aready have */
            {
              sprintf(indices, "%s",index_with_firstelem("(","grid%",-1,t2,p,post)) ;
            }

  if ( !(p->scalar_array_member) ) {
	    fprintf(fp,"IF ( in_use_for_config(grid%%id,'%s') ) THEN\n",vname) ;
	    fprintf(fp,"CALL wrf_ext_write_field (  &\n") ;
	    fprintf(fp,"                       fid                , &  ! DataHandle \n" ) ;
	    fprintf(fp,"                       current_date(1:19) , &  ! DateStr \n" ) ;
	    fprintf(fp,"                       '%s'               , &  ! Data Name \n", dname ) ;
            if ( p->ndims >= 2 && ok_to_collect_distribute )
	      fprintf(fp,"                       globbuf_%s               , &  ! Field \n" , p->type->name ) ;
            else
	      fprintf(fp,"                       %s%s%s               , &  ! Field \n" , structname , vname , indices ) ;
            if (!strncmp(p->type->name,"real",4)) {
              fprintf(fp,"                       WRF_FLOAT          , &  ! FieldType \n") ;
            } else {
              fprintf(fp,"                       WRF_%s             , &  ! FieldType \n" , p->type->name ) ;
            }
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
	    fprintf(fp,"ENDIF\n") ;

            if ( ok_to_collect_distribute )
	      fprintf(fp,"END IF\n" ) ;

/*
	    if ( p->scalar_array_member )
	      fprintf(fp,"END IF\n" ) ;
*/

  }
          }
        }
      }
    }
    }
    if ( p->type->type_type == DERIVED )
    {
      sprintf(x,"%s%s%%",structname,p->name ) ;
      gen_wrf_io2(fp, fname, x, NULL, p->type, io_mask, sw_io ) ;
    }

    }
  }
  return(0) ;
}

