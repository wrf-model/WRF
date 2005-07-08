#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "protos.h"
#include "registry.h"
#include "data.h"

int contains_str( char *s1, char *s2 )
{
  int i ;
  char *p, *q, *r ;
  if ( s2 == NULL || s1 == NULL ) return ( 0 ) ;
  if ( *s2 == '\0' || *s1 == '\0' ) return ( 0 ) ;
  p = s1 ;
  while ( *p ) {
    if ((r = (char *)index( p , *s2 )) == NULL ) { return( 0 ) ; }
    for ( q = s2 ; *q && *r == *q ; r++ , q++ )  ;
    if ( *q == '\0' ) return (1) ;
    p++ ;
  }
  return( 0 ) ;
}

int contains_tok( char *s1, char *s2, char *delims )
{
  char *p ;
  char tempstr[8092] ;

  strcpy( tempstr , s1 ) ;
  p = strtok ( tempstr, delims ) ;
  while ( p != NULL )
  {
    if ( !strcmp ( p , s2 ) ) {  return(1) ;}
    p = strtok( NULL, delims ) ;
  }
  return(0) ;
}
  

char halo_define[4*4096], halo_use[NAMELEN], halo_id[NAMELEN], upper_case_corename[NAMELEN], x[NAMELEN] ;

int 
gen_nest_interp ( char * dirname )
{
  char * corename ;
  char * fnlst[] = { "nest_forcedown_interp.inc" , "nest_interpdown_interp.inc" , 
                     "nest_feedbackup_interp.inc", "nest_feedbackup_smooth.inc",
                     0L } ;
  int down_path[] = { FORCE_DOWN , INTERP_DOWN , INTERP_UP, SMOOTH_UP } ;
  int ipath ;
  char ** fnp ; char * fn ;
  char fname[NAMELEN] ;
  FILE * fp ;
  int i ;

  for ( fnp = fnlst , ipath = 0 ; *fnp ; fnp++ , ipath++ )
  {
    fn = *fnp ;
    for ( i = 0 ; i < get_num_cores() ; i++ )
    {
      corename = get_corename_i(i) ;
      if ( dirname == NULL || corename == NULL ) return(1) ;
      if ( strlen(dirname) > 0 )
       { sprintf(fname,"%s/%s_%s",dirname,corename,fn) ; }
      else
       { sprintf(fname,"%s_%s",corename,fn) ; }
      if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
      print_warning(fp,fname) ;

strcpy( upper_case_corename , corename ) ;
make_upper_case( upper_case_corename ) ;
if      ( down_path[ipath] == INTERP_DOWN ) { sprintf(halo_id,"HALO_%s_INTERP_DOWN",upper_case_corename) ; }
else if ( down_path[ipath] == FORCE_DOWN  ) { sprintf(halo_id,"HALO_%s_FORCE_DOWN",upper_case_corename) ; }
else if ( down_path[ipath] == INTERP_UP   ) { sprintf(halo_id,"HALO_%s_INTERP_UP",upper_case_corename) ; }
else if ( down_path[ipath] == SMOOTH_UP   ) { sprintf(halo_id,"HALO_%s_INTERP_SMOOTH",upper_case_corename) ; }
sprintf(halo_define,"80:") ;
sprintf(halo_use,"dyn_%s",corename) ;

#if 0
      gen_nest_interp1 ( fp , Domain.fields, corename, NULL, down_path[ipath], (down_path[ipath]==FORCE_DOWN)?1:2 ) ;
#else
      gen_nest_interp1 ( fp , Domain.fields, corename, NULL, down_path[ipath], (down_path[ipath]==FORCE_DOWN)?2:2 ) ;
#endif

{
  node_t * comm_struct ;
  comm_struct = new_node( HALO ) ;
  strcpy( comm_struct->name        , halo_id     ) ;
  strcpy( comm_struct->use         , halo_use    ) ;
  strcpy( comm_struct->comm_define , halo_define ) ;
  add_node_to_end( comm_struct , &Halos ) ;
}


      close_the_file(fp) ;
    }
  }
  return(0) ; 
}


int
gen_nest_interp1 ( FILE * fp , node_t * node, char * corename , char * fourdname, int down_path , int use_nest_time_level )
{
  int i, ii ;
  char * fn = "nest_interp.inc" ;
  char fname[NAMELEN] ;
  node_t *p, *p1, *dim ;
  int d2, d3, xdex, ydex, zdex ;
  char ddim[3][2][NAMELEN] ;
  char mdim[3][2][NAMELEN] ;
  char pdim[3][2][NAMELEN] ;
  char ddim2[3][2][NAMELEN] ;
  char mdim2[3][2][NAMELEN] ;
  char pdim2[3][2][NAMELEN] ;
  char nddim[3][2][NAMELEN] ;
  char nmdim[3][2][NAMELEN] ;
  char npdim[3][2][NAMELEN] ;
  char nddim2[3][2][NAMELEN] ;
  char nmdim2[3][2][NAMELEN] ;
  char npdim2[3][2][NAMELEN] ;
  char vname[NAMELEN] ; char vname2[NAMELEN] ; char tag[NAMELEN], tag2[NAMELEN] ; char core[NAMELEN], core2[NAMELEN] ;
  char fcn_name[NAMELEN] ;
  char xstag[NAMELEN], ystag[NAMELEN] ;
  char dexes[NAMELEN] ;
  char ndexes[NAMELEN] ;
  char *maskstr ;


  for ( p1 = node ;  p1 != NULL ; p1 = p1->next )
  {

    if ( p1->node_kind & FOURD )
    {
       gen_nest_interp1 ( fp, p1->members, corename, p1->name, down_path, use_nest_time_level ) ;  /* RECURSE over members */
       continue ;
    }
    else
    {
      p = p1 ; 
    }

    if ( p->io_mask & down_path )
    {
      if ((!strncmp( p->use, "dyn_", 4) && !strcmp(p->use+4,corename)) || strncmp( p->use, "dyn_", 4))
      {

        if (!strncmp( p->use, "dyn_", 4))   sprintf(core,"%s_",corename,vname) ;
        else                                sprintf(core,"") ;

        if ( p->ntl > 1 ) { sprintf(tag,"_2") ; sprintf(tag2,"_%d", use_nest_time_level) ; }
        else              { sprintf(tag,"")   ; sprintf(tag2,"")                         ; }

        /* construct variable name */
        if ( p->scalar_array_member )
        {

sprintf(x, "%s%s", p->use, tag ) ;
if ( ! contains_tok ( halo_define , x , ":," ) ) {
 if ( halo_define[strlen(halo_define)-1] == ':' ) { strcat(halo_define,p->use) ; strcat(halo_define,tag) ; }
 else                                             { strcat(halo_define,",") ; strcat(halo_define,p->use) ; strcat(halo_define,tag) ; }
}

          strcpy(dexes,"grid%sm31,grid%sm32,grid%sm33,") ;
          sprintf(vname,"%s%s(%sP_%s)",p->use,tag,dexes,p->name) ;
          strcpy(ndexes,"ngrid%sm31,ngrid%sm32,ngrid%sm33,") ;
          sprintf(vname2,"%s%s%s(%sP_%s)",core,p->use,tag2,ndexes,p->name) ;

          if ( down_path & SMOOTH_UP ) {
            strcpy( fcn_name , p->smoothu_fcn_name ) ;
	  } else {
            strcpy( fcn_name , (down_path & INTERP_UP)?p->interpu_fcn_name:((down_path & FORCE_DOWN)?p->force_fcn_name:p->interpd_fcn_name) ) ;
          }
        }
        else
        {
          sprintf(vname,"%s%s",p->name,tag) ;

if ( ! contains_tok ( halo_define , vname  , ":," ) ) {
 if ( halo_define[strlen(halo_define)-1] == ':' ) { strcat(halo_define,vname) ; }
 else                                             { strcat(halo_define,",") ; strcat(halo_define,vname) ; }
}
          sprintf(vname2,"%s%s%s",core,p->name,tag2) ;
          if ( down_path & SMOOTH_UP ) {
            strcpy( fcn_name , p->smoothu_fcn_name ) ;
	  } else {
            strcpy( fcn_name , (down_path & INTERP_UP)?p->interpu_fcn_name:((down_path & FORCE_DOWN)?p->force_fcn_name:p->interpd_fcn_name) ) ;
	  }
        }

        set_dim_strs ( p , ddim , mdim , pdim , "c", 1 ) ;
        set_dim_strs ( p , ddim2 , mdim2 , pdim2 , "c", 0 ) ;
        set_dim_strs ( p , nddim , nmdim , npdim , "n", 1 ) ;
        set_dim_strs ( p , nddim2 , nmdim2 , npdim2 , "n", 0 ) ;

        zdex = get_index_for_coord( p , COORD_Z ) ;
        xdex = get_index_for_coord( p , COORD_X ) ;
        ydex = get_index_for_coord( p , COORD_Y ) ;

        if ( p->stag_x ) strcpy( xstag, ".TRUE." ) ; else strcpy( xstag, ".FALSE." ) ;
        if ( p->stag_y ) strcpy( ystag, ".TRUE." ) ; else strcpy( ystag, ".FALSE." ) ;

        if ( p->scalar_array_member )
	{
fprintf(fp,"IF ( P_%s .GE. PARAM_FIRST_SCALAR ) THEN\n",p->name) ;
	}

        if        ( p->stag_x && p->stag_y ) {
	  maskstr = "_xystag" ;
	} else if ( p->stag_x ) {
	  maskstr = "_xstag" ;
	} else if ( p->stag_y ) {
	  maskstr = "_ystag" ;
	} else {
	  maskstr = "_nostag" ;
	}


fprintf(fp,"CALL %s (                                                               &         \n", fcn_name ) ;

        if ( zdex >= 0 ) {

/* note this is only good for IKJ */

fprintf(fp,"                  %s,                                                           &         ! CD field\n",  vname) ;
fprintf(fp,"                 %s, %s, %s, %s, %s, %s,   &         ! CD dims\n",
                ddim[0][0], ddim[0][1], ddim[1][0], ddim[1][1], ddim[2][0], ddim[2][1] ) ;
fprintf(fp,"                 %s, %s, %s, %s, %s, %s,   &         ! CD dims\n",
                mdim[0][0], mdim[0][1], mdim[1][0], mdim[1][1], mdim[2][0], mdim[2][1] ) ;
fprintf(fp,"                 %s, %s, %s, %s, %s, %s,   &         ! CD dims\n",
                pdim[0][0], pdim[0][1], pdim2[1][0], pdim2[1][1], pdim[2][0], pdim[2][1] ) ;
if ( ! (down_path  & SMOOTH_UP)  ) {
fprintf(fp,"                  ngrid%%%s,                                                        &   ! ND field\n", vname2) ;
}
fprintf(fp,"                 %s, %s, %s, %s, %s, %s,   &         ! ND dims\n",
                nddim[0][0], nddim[0][1], nddim[1][0], nddim[1][1], nddim[2][0], nddim[2][1] ) ;
fprintf(fp,"                 %s, %s, %s, %s, %s, %s,   &         ! ND dims\n",
                nmdim[0][0], nmdim[0][1], nmdim[1][0], nmdim[1][1], nmdim[2][0], nmdim[2][1] ) ;
fprintf(fp,"                 %s, %s, %s, %s, %s, %s,   &         ! ND dims\n",
                npdim[0][0], npdim[0][1], npdim2[1][0], npdim2[1][1], npdim[2][0], npdim[2][1] ) ;

        } else {

/* note this is only good for IKJ */

fprintf(fp,"                  %s,                                                           &         ! CD field\n",  vname) ;
fprintf(fp,"                 %s, %s, %s, %s, %s, %s,   &         ! CD dims\n",
                ddim[0][0], ddim[0][1],          "1",         "1", ddim[1][0], ddim[1][1] ) ;
fprintf(fp,"                 %s, %s, %s, %s, %s, %s,   &         ! CD dims\n",
                mdim[0][0], mdim[0][1],          "1",         "1", mdim[1][0], mdim[1][1] ) ;
fprintf(fp,"                 %s, %s, %s, %s, %s, %s,   &         ! CD dims\n",
                pdim[0][0], pdim[0][1],          "1",         "1", pdim[1][0], pdim[1][1] ) ;
if ( ! (down_path  & SMOOTH_UP)  ) {
fprintf(fp,"                  ngrid%%%s,                                                        &   ! ND field\n", vname2) ;
}
fprintf(fp,"                 %s, %s, %s, %s, %s, %s,   &         ! ND dims\n",
                nddim[0][0], nddim[0][1],           "1",          "1", nddim[1][0], nddim[1][1] ) ;
fprintf(fp,"                 %s, %s, %s, %s, %s, %s,   &         ! ND dims\n",
                nmdim[0][0], nmdim[0][1],           "1",          "1", nmdim[1][0], nmdim[1][1] ) ;
fprintf(fp,"                 %s, %s, %s, %s, %s, %s,   &         ! ND dims\n",
                npdim[0][0], npdim[0][1],           "1",          "1", npdim[1][0], npdim[1][1] ) ;

        }

if ( ! (down_path  & SMOOTH_UP)  ) {
  if ( sw_deref_kludge == 1 ) {
fprintf(fp,"                  shw, ngrid%%imask%s(nims,njms),         &         ! stencil half width\n",maskstr) ;
  } else {
fprintf(fp,"                  shw, ngrid%%imask%s,         &         ! stencil half width\n",maskstr) ;
  }
}
fprintf(fp,"                  %s, %s,                                                &         ! xstag, ystag\n", xstag, ystag ) ;
fprintf(fp,"                  ngrid%%i_parent_start, ngrid%%j_parent_start,                     &\n") ;
fprintf(fp,"                  ngrid%%parent_grid_ratio, ngrid%%parent_grid_ratio                &\n") ;
   
        {
           char tmpstr[2048], *p1 ;
           node_t * nd ;
           strcpy( tmpstr , "" ) ;
           if        ( down_path & SMOOTH_UP ) {
             strcpy( tmpstr , p->smoothu_aux_fields ) ;
	   } else if ( down_path & INTERP_UP ) {
             strcpy( tmpstr , p->interpu_aux_fields ) ;
	   } else if ( down_path & FORCE_DOWN ) {
             /* by default, add the boundary and boundary tendency fields to the arg list */
             if ( ! p->scalar_array_member ) {
               sprintf( tmpstr , "%s_b,%s_bt,", p->name, p->name )  ;
             } else {
               sprintf( tmpstr , "%s_b,%s_bt,", fourdname, fourdname )  ;
             }
             strcat( tmpstr , p->force_aux_fields ) ;
	   } else if ( down_path & INTERP_DOWN ) {
             strcpy( tmpstr , p->interpd_aux_fields ) ;
	   }
           for ( p1 = strtok(tmpstr,",") ; p1 != NULL ; p1 = strtok(NULL,",") )
           {
             if (( nd = get_entry ( p1 , Domain.fields )) != NULL )
             {
  	       if (!strncmp( nd->use, "dyn_", 4))   sprintf(core2,"%s_",corename,vname) ;
	       else                                sprintf(core2,"") ;
               if ( strcmp( nd->use , "_4d_bdy_array_" ) ) {
                 fprintf(fp,",%s,ngrid%%%s%s  &\n", nd->name, core2, nd->name ) ;
               } else {
                 fprintf(fp,",%s(1,1,1,1,P_%s),ngrid%%%s%s(1,1,1,1,P_%s)  &\n", nd->name, p->name, core2, nd->name, p->name ) ;
               }
             }
             else
             {
	       fprintf(stderr,"REGISTRY WARNING: Don't know about %s in definition of %s\n",p1,vname) ;
	     }
           }
        }

fprintf(fp,"                  ) \n") ;

        if ( p->scalar_array_member )
	{
fprintf(fp,"ENDIF\n") ;
	}

        }
     }
  }

  return(0) ;
}

