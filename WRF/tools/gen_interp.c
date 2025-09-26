#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#ifdef _WIN32
#define rindex(X,Y) strrchr(X,Y)
#define index(X,Y) strchr(X,Y)
#endif

#include "protos.h"
#include "registry.h"
#include "data.h"

int as_long(char *str,long *l) {
  char *endptr=NULL;

  errno=0;
  *l=strtol(str,&endptr,10);
  if(!endptr || *endptr) return 0;
  return 1;
}

int as_finite_double(char *str,double *d) {
  char *endptr=NULL;

  errno=0;
  *d=strtod(str,&endptr);
  if(!endptr || *endptr) return 0;
  if(*d==*d && *d+1!=*d)
    return 1;
  else
    return 0; /* NaN or infinite */
}

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
  

 /* Had to increase size for SOA from 4*4096 to 4*7000 */
char halo_define[4*7000], halo_use[NAMELEN], halo_id[NAMELEN], x[NAMELEN] ;

/*KAL added this for vertical interpolation */
/*DJW 131202 modified to create files required for vertical interpolation from parent to nest */
int
gen_nest_v_interp ( char * dirname )
{
  char * fnlst[] = { "nest_forcedown_interp_vert.inc",
                     "nest_interpdown_interp_vert.inc",
                     0L };
  int down_path[] = { FORCE_DOWN , INTERP_DOWN };
  int ipath;
  char **fnp ; char *fn;
  char fname[NAMELEN];
  FILE *fp;

  for ( fnp=fnlst , ipath=0 ; *fnp ; fnp++, ipath++ )
  {
    fn = *fnp;
    if ( dirname == NULL ) return(1);
    if ( strlen(dirname) > 0 )
    { sprintf(fname,"%s/%s",dirname,fn); }
    else
    { sprintf(fname,"%s",fn); }
    if ((fp = fopen( fname, "w" )) == NULL ) return(1);
    print_warning(fp,fname);

    gen_nest_interp2( fp, Domain.fields, NULL, down_path[ipath], (down_path[ipath]==FORCE_DOWN)?2:2 );

    close_the_file(fp);
  }
  return(0);
}

int 
gen_nest_interp ( char * dirname )
{
  char * fnlst[] = { "nest_forcedown_interp.inc" , "nest_interpdown_interp.inc" , 
                     "nest_feedbackup_interp.inc", "nest_feedbackup_smooth.inc",
                     0L } ;
  int down_path[] = { FORCE_DOWN , INTERP_DOWN , INTERP_UP, SMOOTH_UP } ;
  int ipath ;
  char ** fnp ; char * fn ;
  char fname[NAMELEN] ;
  FILE * fp ;
  
  /*KAL FORCE_DOWN, etc are integers defined in registry.h, so down_path is an array of integers*/

  for ( fnp = fnlst , ipath = 0 ; *fnp ; fnp++ , ipath++ )
  {
    fn = *fnp ;
    if ( dirname == NULL ) return(1) ;
    if ( strlen(dirname) > 0 )
     { sprintf(fname,"%s/%s",dirname,fn) ; }
    else
     { sprintf(fname,"%s",fn) ; }
    if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
    print_warning(fp,fname) ;

if      ( down_path[ipath] == INTERP_DOWN ) { sprintf(halo_id,"HALO_INTERP_DOWN") ; }
else if ( down_path[ipath] == FORCE_DOWN  ) { sprintf(halo_id,"HALO_FORCE_DOWN") ; }
else if ( down_path[ipath] == INTERP_UP   ) { sprintf(halo_id,"HALO_INTERP_UP") ; }
else if ( down_path[ipath] == SMOOTH_UP   ) { sprintf(halo_id,"HALO_INTERP_SMOOTH") ; }
sprintf(halo_define,"80:") ;
sprintf(halo_use,"") ;
      gen_nest_interp1 ( fp , Domain.fields, NULL, down_path[ipath], (down_path[ipath]==FORCE_DOWN)?2:2 ) ;
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
  return(0) ; 
}


int
gen_nest_interp1 ( FILE * fp , node_t * node, char * fourdname, int down_path , int use_nest_time_level )
{
  int i, ii ;
  char * fn = "nest_interp.inc" ;
  char fname[NAMELEN] ;
  node_t *p, *p1, *dim ;
  int d2, d3, xdex, ydex, zdex, nest_mask ;
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
  char vname[NAMELEN], vname2[NAMELEN] ; 
  char tag[NAMELEN], tag2[NAMELEN] ; 
  char fcn_name[NAMELEN] ;
  char xstag[NAMELEN], ystag[NAMELEN] ;
  char dexes[NAMELEN] ;
  char ndexes[NAMELEN] ;
  char *maskstr ;
  char *grid ;
  char *colon, r[10],tx[80],temp[80],moredims[80] ; 
  int d ; 
  double real_store;
  long long_store;

  for ( p1 = node ;  p1 != NULL ; p1 = p1->next )
  {
    if ( p1->node_kind & FOURD )
    {
      if ( p1->members->next ) {
        nest_mask = p1->members->next->nest_mask ;
      } else {
        continue ;
      }
    }
    else
    {
      nest_mask = p1->nest_mask ;
    }
    p = p1 ;

    if ( nest_mask & down_path )
    {
        if ( p->ntl > 1 ) { sprintf(tag,"_2") ; sprintf(tag2,"_%d", use_nest_time_level) ; }
        else              { sprintf(tag,"")   ; sprintf(tag2,"")                         ; }

        /* construct variable name */
        if ( p->node_kind & FOURD ) {

sprintf(x, "%s%s", p->name, tag ) ;
if ( ! contains_tok ( halo_define , x , ":," ) ) {
 if ( halo_define[strlen(halo_define)-1] == ':' ) { strcat(halo_define,p->name) ; strcat(halo_define,tag) ; }
 else                                             { strcat(halo_define,",") ; strcat(halo_define,p->name) ; strcat(halo_define,tag) ; }
}
          strcpy(moredims,"") ;
          for ( d = 3 ; d < p->ndims ; d++ ) {
            sprintf(temp,"idim%d",d-2) ;
            strcat(moredims,",") ; strcat(moredims,temp) ;
          }
          strcat(moredims,",") ;

          strcpy(dexes,"grid%sm31,grid%sm32,grid%sm33") ;
          sprintf(vname,"%s%s(%s%sitrace)",p->name,tag,dexes,moredims) ;
          strcpy(ndexes,"ngrid%sm31,ngrid%sm32,ngrid%sm33") ;
          sprintf(vname2,"%s%s(%s%sitrace)",p->name,tag2,ndexes,moredims) ;

          if ( down_path & SMOOTH_UP ) {
            strcpy( fcn_name , p->members->next->smoothu_fcn_name ) ;
	  } else {
            strcpy( fcn_name , (down_path & INTERP_UP)?p->members->next->interpu_fcn_name:((down_path & FORCE_DOWN)?p->members->next->force_fcn_name:p->members->next->interpd_fcn_name) ) ;
          }
        }
        else
        {
          sprintf(vname,"%s%s",p->name,tag) ;

if ( ! contains_tok ( halo_define , vname  , ":," ) ) {
 if ( halo_define[strlen(halo_define)-1] == ':' ) { strcat(halo_define,vname) ; }
 else                                             { strcat(halo_define,",") ; strcat(halo_define,vname) ; }
}
          sprintf(vname2,"%s%s",p->name,tag2) ;
          if ( down_path & SMOOTH_UP ) {
            strcpy( fcn_name , p->smoothu_fcn_name ) ;
	  } else {
            strcpy( fcn_name , (down_path & INTERP_UP)?p->interpu_fcn_name:((down_path & FORCE_DOWN)?p->force_fcn_name:p->interpd_fcn_name) ) ;
	  }
        }

        if ( p1->node_kind & FOURD ) {
          grid = "" ;
          set_dim_strs2 ( p->members->next , ddim , mdim , pdim , "c", 1 ) ;
          set_dim_strs2 ( p->members->next , ddim2 , mdim2 , pdim2 , "c", 0 ) ;
          set_dim_strs2 ( p->members->next , nddim , nmdim , npdim , "n", 1 ) ;
          set_dim_strs2 ( p->members->next , nddim2 , nmdim2 , npdim2 , "n", 0 ) ;
          zdex = get_index_for_coord( p->members->next , COORD_Z ) ;
          xdex = get_index_for_coord( p->members->next , COORD_X ) ;
          ydex = get_index_for_coord( p->members->next , COORD_Y ) ;
          if ( p->members->next->stag_x ) strcpy( xstag, ".TRUE." ) ; else strcpy( xstag, ".FALSE." ) ;
          if ( p->members->next->stag_y ) strcpy( ystag, ".TRUE." ) ; else strcpy( ystag, ".FALSE." ) ;
          if ( p->members->next->stag_x && p->members->next->stag_y ) {
	    maskstr = "_xystag" ;
	  } else if ( p->stag_x ) {
	    maskstr = "_xstag" ;
	  } else if ( p->stag_y ) {
	    maskstr = "_ystag" ;
	  } else {
	    maskstr = "_nostag" ;
	  }
        } else {
          grid = "grid%" ;
          set_dim_strs2 ( p , ddim , mdim , pdim , "c", 1 ) ;
          set_dim_strs2 ( p , ddim2 , mdim2 , pdim2 , "c", 0 ) ;
          set_dim_strs2 ( p , nddim , nmdim , npdim , "n", 1 ) ;
          set_dim_strs2 ( p , nddim2 , nmdim2 , npdim2 , "n", 0 ) ;
          zdex = get_index_for_coord( p , COORD_Z ) ;
          xdex = get_index_for_coord( p , COORD_X ) ;
          ydex = get_index_for_coord( p , COORD_Y ) ;
          if ( p->stag_x ) strcpy( xstag, ".TRUE." ) ; else strcpy( xstag, ".FALSE." ) ;
          if ( p->stag_y ) strcpy( ystag, ".TRUE." ) ; else strcpy( ystag, ".FALSE." ) ;
          if ( p->stag_x && p->stag_y ) {
	    maskstr = "_xystag" ;
	  } else if ( p->stag_x ) {
	    maskstr = "_xstag" ;
	  } else if ( p->stag_y ) {
	    maskstr = "_ystag" ;
	  } else {
	    maskstr = "_nostag" ;
	  }
        }

        if ( p->node_kind & FOURD )
	{
            fprintf(fp,"DO itrace = PARAM_FIRST_SCALAR, num_%s\n",p->name ) ;
            for ( d = p->ndims-1 ; d >= 3 ; d-- ) {
              strcpy(r,"") ;
              range_of_dimension( r, tx, d, p, "config_flags%" ) ;
              colon = index(tx,':') ; *colon = ',' ;
              sprintf(temp,"idim%d",d-2) ;
              strcat(moredims,",") ; strcat(moredims,temp) ;
              fprintf(fp,"  DO %s = %s\n",temp,tx) ;
            }
            fprintf(fp,"IF ( SIZE( %s%s, %d ) * SIZE( %s%s, %d ) .GT. 1", p->name,tag,xdex+1,p->name,tag,ydex+1 ) ;
        } else {
          if ( !strcmp( fcn_name, "interp_mask_land_field" ) || !strcmp( fcn_name, "interp_mask_water_field" ) || 
               !strcmp( fcn_name, "interp_mask_field")       || !strcmp( fcn_name, "interp_mask_soil") ) {
            fprintf(fp,"IF ( .TRUE.") ;
          } else {
            fprintf(fp,"IF ( SIZE( %s%s, %d ) * SIZE( %s%s, %d ) .GT. 1", grid,vname2,xdex+1,grid,vname2,ydex+1 ) ;
          }
	}
        if(p->mp_var)
          fprintf(fp," .and. (interp_mp .eqv. .true.)");
        fprintf(fp," ) THEN \n");
#if NMM_CORE==1
        if(strcasecmp(fcn_name,"nointerp")) {
          /* We get here if we are calling any function other than
             "NoInterp," a placeholder function that does not
             interpolate and only exists to ensure the variable is
             allocated on the intermediate domain.  This is a
             workaround for a bug in the IBM compiler: when the
             interpolation routine is NoInterp, we do not generate the
             call.

             This workaround is only enabled for the NMM
             configurations since the other configurations do not use
             the NoInterp placeholder function. */
#endif
fprintf(fp,"CALL %s (  &         \n", fcn_name ) ;

if ( !strcmp( fcn_name, "interp_mask_land_field" ) || !strcmp( fcn_name, "interp_mask_water_field" ) ||
     !strcmp( fcn_name, "interp_mask_field")       || !strcmp( fcn_name, "interp_mask_soil") ) {
fprintf(fp,"  ( SIZE( %s%s , %d )*SIZE( %s%s , %d ) .GT. 1 ), & ! special argument needed because %s has bcasts in it\n",
                                                       grid,vname2,xdex+1,grid,vname2,ydex+1,fcn_name) ;
}
fprintf(fp,"                  %s%s,   &       ! CD field\n", grid, (p->node_kind & FOURD)?vname:vname2) ;
fprintf(fp,"                 %s, %s, %s, %s, %s, %s,   &         ! CD dims\n",
                ddim[0][0], ddim[0][1], ddim[1][0], ddim[1][1], ddim[2][0], ddim[2][1] ) ;
fprintf(fp,"                 %s, %s, %s, %s, %s, %s,   &         ! CD dims\n",
                mdim[0][0], mdim[0][1], mdim[1][0], mdim[1][1], mdim[2][0], mdim[2][1] ) ;
fprintf(fp,"                 %s, %s, %s, %s, %s, %s,   &         ! CD dims\n",
                pdim[0][0], pdim[0][1], pdim2[1][0], pdim2[1][1], pdim[2][0], pdim[2][1] ) ;
if ( ! (down_path  & SMOOTH_UP)  ) {
fprintf(fp,"                  ngrid%%%s,  &   ! ND field\n", vname2) ;
}
fprintf(fp,"                 %s, %s, %s, %s, %s, %s,   &         ! ND dims\n",
                nddim[0][0], nddim[0][1], nddim[1][0], nddim[1][1], nddim[2][0], nddim[2][1] ) ;
fprintf(fp,"                 %s, %s, %s, %s, %s, %s,   &         ! ND dims\n",
                nmdim[0][0], nmdim[0][1], nmdim[1][0], nmdim[1][1], nmdim[2][0], nmdim[2][1] ) ;
fprintf(fp,"                 %s, %s, %s, %s, %s, %s,   &         ! ND dims\n",
                npdim[0][0], npdim[0][1], npdim2[1][0], npdim2[1][1], npdim[2][0], npdim[2][1] ) ;

if ( ! (down_path  & SMOOTH_UP)  ) {
  if ( sw_deref_kludge == 1 ) {
fprintf(fp,"                  config_flags%%shw, ngrid%%imask%s(nims,njms),         &         ! stencil half width\n",maskstr) ;
  } else {
fprintf(fp,"                  config_flags%%shw, ngrid%%imask%s,         &         ! stencil half width\n",maskstr) ;
  }
}
fprintf(fp,"                  %s, %s,                                                &         ! xstag, ystag\n", xstag, ystag ) ;
fprintf(fp,"                  ngrid%%i_parent_start, ngrid%%j_parent_start,                     &\n") ;
fprintf(fp,"                  ngrid%%parent_grid_ratio, ngrid%%parent_grid_ratio                &\n") ;
   
        {
           char tmpstr[NAMELEN], *p1 ;
           node_t * nd, * pp  ;
           pp = NULL ;
           if ( p->node_kind & FOURD ) {
             if (  p->members->next ) {
               pp = p->members->next ;
             }
           } else {
             pp = p ;
           }
           if ( pp ) {
             strcpy( tmpstr , "" ) ;
             if        ( down_path & SMOOTH_UP ) {
               strcpy( tmpstr , pp->smoothu_aux_fields ) ;
	     } else if ( down_path & INTERP_UP ) {
               strcpy( tmpstr , pp->interpu_aux_fields ) ;
	     } else if ( down_path & FORCE_DOWN ) {
               /* by default, add the boundary and boundary tendency fields to the arg list */
               if ( ! p->node_kind & FOURD ) {
                 sprintf( tmpstr , "%s_b,%s_bt,", pp->name, pp->name )  ;
               } else {
                 sprintf( tmpstr , "%s_b,%s_bt,", p->name, p->name )  ;
               }
               strcat( tmpstr , pp->force_aux_fields ) ;
	     } else if ( down_path & INTERP_DOWN ) {
               strcpy( tmpstr , pp->interpd_aux_fields ) ;
	     }

             for ( p1 = strtok(tmpstr,",") ; p1 != NULL ; p1 = strtok(NULL,",") )
             {
               if(as_long(p1,&long_store)) {
                 /* Integer aux in registry (6, 0x5A, 0774, etc.).
                    Print in fortran-readable form. */
                 fprintf(fp,",%ld &\n",long_store);
               } else if(as_finite_double(p1,&real_store)) {
                 /* Real aux in registry (3.7, -3.105e+04, etc.).  Print in
                    fortran-readable form. */
                 fprintf(fp,",%5.9e &\n",real_store);
               } else if( (p1[0]=='l' || p1[0]=='L') && p1[1]=='%' && p1[2]!='\0') {
                 /* Local variable requested (l%varname). */
                 fprintf(fp,",%s &\n",p1+2);
               } else if( p1[0]=='@' && p1[1]!='\0' ) {
                 /* Local variable requested (@varname). */
                 fprintf(fp,",%s &\n",p1+1);
               } else if( p1[0]=='*' && p1[1]=='\0' ) {
                 /* Entire grid requested (*) */
                 fprintf(fp,",grid ,ngrid &\n");
               } else if( !strcasecmp(p1,"n%*") ) {
                 /* Nest grid requested (n%*) */
                 fprintf(fp,",ngrid &\n");
               } else if( !strcasecmp(p1,"c%*") ) {
                 /* Coarse grid requested (c%*) */
                 fprintf(fp,",grid &\n");
               } else { /* is n%varname, c%varname, varname or an error */
                 int want_nest=1;
                 int want_coarse=1;
                 char *subvar=p1;
                 if( (p1[0]=='n' || p1[0]=='N') && p1[1]=='%' && p1[2]!='\0' ) {
                   /* n%var, so we don't want the coarse domain var */
                   want_coarse=0;
                   subvar+=2;
                 } else if( (p1[0]=='c' || p1[0]=='C') && p1[1]=='%' && p1[2]!='\0' ) {
                   /* c%var, so we don't want the nest domain var */
                   want_nest=0;
                   subvar+=2;
                 } else {
                   /* either "varname" (so we give coarse and nest) or an error */
                 }

                 if (( nd = get_entry ( subvar , Domain.fields )) != NULL ) {
                   /* Variable name is valid */
                   if ( nd->boundary_array ) {
                     /* We're requesting boundary data, which may need
                        to be handled differently, depending on the
                        configuration. */
                     if ( sw_new_bdys ) {
                       int bdy ;
                       for ( bdy = 1 ; bdy <= 4 ; bdy++ ) {
                         if ( strcmp( nd->use , "_4d_bdy_array_" ) ) {
                           if(want_coarse) fprintf(fp,",dummy_%s ",bdy_indicator(bdy));
                           if(want_nest)   fprintf(fp,",ngrid%%%s%s ",nd->name, bdy_indicator(bdy));
                         } else {
                           char c ;
                           c = 'i' ; if ( bdy <= 2 ) c = 'j' ;
                           if(want_coarse) fprintf(fp,",%s%s(c%cms,1,1,itrace) ", nd->name, bdy_indicator(bdy), c);
                           if(want_nest)   fprintf(fp,",ngrid%%%s%s(n%cms,1,1,itrace) ", nd->name, bdy_indicator(bdy), c);
                         }
                         fprintf(fp,"&\n");
                       }
                     } else {
                       if ( strcmp( nd->use , "_4d_bdy_array_" ) ) {
                         if(want_coarse)   fprintf(fp,",%s ", nd->name) ;
                         if(want_nest)     fprintf(fp,",ngrid%%%s ", nd->name ) ;
                       } else {
                         if(want_coarse)   fprintf(fp,",%s(1,1,1,1,itrace) ", nd->name ) ;
                         if(want_nest)     fprintf(fp,",ngrid%%%s(1,1,1,1,itrace) ", nd->name ) ;
                       }
                       fprintf(fp,"&\n");
                     }
                   } else {
                     /* This is not a boundary array, so pass the
                        variable. */
                     if(want_coarse)       fprintf(fp,",grid%%%s",  nd->name ) ;
                     if(want_nest)         fprintf(fp,",ngrid%%%s",  nd->name ) ;
                     fprintf(fp,"&\n");
                   }
                 } else {
                   fprintf(stderr,"REGISTRY WARNING: %s: %s is not a variable or number; ignoring it\n",vname,p1) ;
                 }
               }
             }
           }
        }

fprintf(fp,"                  ) \n") ;
#if NMM_CORE==1
        } else {
          fprintf(fp,"CONTINUE ! do not call %s\n", fcn_name ) ;
        } /* end of "skip this if we would call NoInterp" */ 
#endif
        if ( p->node_kind & FOURD )
        {
fprintf(fp,"ENDIF\n") ;
            for ( d = p->ndims-1 ; d >= 3 ; d-- ) {
fprintf(fp,"ENDDO\n") ;
            }
fprintf(fp,"ENDDO\n") ;
        } else {
fprintf(fp,"ENDIF\n") ; /* in_use_from_config */
        }

     }
  }

  return(0) ;
}

/* DJW 131202 Modified this to include only variables that have a vertical dimension
 * (excluding soil layers and other extra dimensions) and inserts a different
 * function call depending on variable staggering in z. */
int
gen_nest_interp2 ( FILE * fp , node_t * node, char * fourdname, int down_path , int use_nest_time_level )
{
  node_t *p, *p1 ;
  int  xdex, ydex, nest_mask ;
  char ddim[3][2][NAMELEN] ;
  char mdim[3][2][NAMELEN] ;
  char pdim[3][2][NAMELEN] ;
  char vname[NAMELEN], vname2[NAMELEN] ; 
  char tag[NAMELEN], tag2[NAMELEN] ; 
  char dexes[NAMELEN] ;
  char ndexes[NAMELEN] ;
  char *grid ;
  char *colon,r[10],tx[80],temp[80],moredims[80] ; 
  int d ; 
  char zstag[NAMELEN];
  char fcn_name[NAMELEN];

  for ( p1 = node ;  p1 != NULL ; p1 = p1->next )
  {
  
    /* KAL-get the nest mask to see what path the variable is on */ 
    if ( p1->node_kind & FOURD )
    {
      if ( p1->members->next ) {
        nest_mask = p1->members->next->nest_mask ;
      } else {
        continue ;
      }
    }
    else
    {
      nest_mask = p1->nest_mask ;
    }
    p = p1 ;
 

   
    if ( nest_mask & down_path )
    {
        /*KAL get the dimensions of the variable and only work on ones with vertical extents*/
        if ( p1->node_kind & FOURD ) {
          set_dim_strs2 ( p->members->next , ddim , mdim , pdim , "", 1 ) ;
        } else {
          set_dim_strs2 ( p , ddim , mdim , pdim , "", 1 ) ;
        } 
        if ( !strcmp ( ddim[0][1], "kde") ||
	             ( ddim[1][1], "kde") ||
		         ( ddim[2][1], "kde")) {	
    
            if ( p->ntl > 1 ) { sprintf(tag,"_2") ; sprintf(tag2,"_%d", use_nest_time_level) ; }
            else              { sprintf(tag,"")   ; sprintf(tag2,"")                         ; }

            /* construct variable name */
            if ( p->node_kind & FOURD ) {
              sprintf(x, "%s%s", p->name, tag ) ;
              strcpy(moredims,"") ;
              for ( d = 3 ; d < p->ndims ; d++ ) {
          	  sprintf(temp,"idim%d",d-2) ;
        	  strcat(moredims,",") ; strcat(moredims,temp) ;
              }
              strcat(moredims,",") ;
              strcpy(dexes,"grid%sm31,grid%sm32,grid%sm33") ;
              sprintf(vname,"%s%s(%s%sitrace)",p->name,tag,dexes,moredims) ;
              strcpy(ndexes,"ngrid%sm31,ngrid%sm32,ngrid%sm33") ;
              sprintf(vname2,"%s%s(%s%sitrace)",p->name,tag2,ndexes,moredims) ;
            }
            else
            {
              sprintf(vname,"%s%s",p->name,tag) ;
              sprintf(vname2,"%s%s",p->name,tag2) ;  
            }

            if ( p1->node_kind & FOURD ) {
              grid = "" ;
	      xdex = get_index_for_coord( p->members->next , COORD_X ) ;
              ydex = get_index_for_coord( p->members->next , COORD_Y ) ;
            } else {
              grid = "grid%" ;
	      xdex = get_index_for_coord( p , COORD_X ) ;
              ydex = get_index_for_coord( p , COORD_Y ) ;
            }

            if ( p->stag_z ) {
                strcpy( zstag, ".TRUE." );
                strcpy(fcn_name,"vert_interp_vert_nesting_w");
            } else {
                strcpy( zstag, ".FALSE." );
                strcpy(fcn_name,"vert_interp_vert_nesting");
            }

 /* DJW 131202 The condition for the if-statement below is really really poorly written.
  * I'm attempting to say "if the variable has a vertical dimension that spans multiple
  * eta levels. Note that this is complicated because some variables have a vertical
  * dimension that describes the number of soil levels they use. There are also other
  * vertical dimensions that we need be wary of... hence my hack to make this work since
  * at the moment all the variables I want to interpolate have kde points in the vertical!*/
            if ( strcmp("kde",ddim[1][1]) == 0 ) {
                if ( p->node_kind & FOURD ) {
                    fprintf(fp,"DO itrace = PARAM_FIRST_SCALAR, num_%s\n",p->name ) ;
                    for ( d = p->ndims-1 ; d >= 3 ; d-- ) {
                    strcpy(r,"") ;
                    range_of_dimension( r, tx, d, p, "config_flags%" ) ;
                    colon = index(tx,':') ; *colon = ',' ;
                    sprintf(temp,"idim%d",d-2) ;
                    strcat(moredims,",") ; strcat(moredims,temp) ;
                    fprintf(fp,"  DO %s = %s\n",temp,tx) ;
                    }
                    fprintf(fp,"IF ( SIZE( %s%s, %d ) * SIZE( %s%s, %d ) .GT. 1 ) THEN \n", p->name,tag,xdex+1,p->name,tag,ydex+1 ) ;
                } else {
                    fprintf(fp,"IF ( SIZE( %s%s, %d ) * SIZE( %s%s, %d ) .GT. 1 ) THEN \n", grid,vname2,xdex+1,grid,vname2,ydex+1 ) ;
                }

                fprintf(fp,"    CALL %s( &\n",fcn_name);
                fprintf(fp,"                                  %s%s, & !CD field\n",grid,(p->node_kind & FOURD)?vname:vname2);
                fprintf(fp,"                                  %s, %s, %s, %s, %s, %s, & !CD dims\n",ddim[0][0],ddim[0][1],ddim[1][0],ddim[1][1],ddim[2][0],ddim[2][1]);
                fprintf(fp,"                                  %s, %s, %s, %s, %s, %s, & !CD dims\n",mdim[0][0],mdim[0][1],mdim[1][0],mdim[1][1],mdim[2][0],mdim[2][1]);
                fprintf(fp,"                                  %s, %s, %s, MIN( (%s-1), %s ), %s, %s, & !CD dims\n",pdim[0][0],pdim[0][1],pdim[1][0],ddim[1][1],pdim[1][1],pdim[2][0],pdim[2][1]);
                fprintf(fp,"                                  pgrid%%s_vert, pgrid%%e_vert, & !vertical dimension of the parent grid\n");
                if ( strcmp(zstag,".TRUE.") != 0 ) {
                    fprintf(fp,"                                  pgrid%%cf1, pgrid%%cf2, pgrid%%cf3, pgrid%%cfn, pgrid%%cfn1, & !coarse grid extrapolation constants\n");
                    fprintf(fp,"                                  alt_u_c, alt_u_n ) !coordinates for parent and nest\n");
                } else {
                    fprintf(fp,"                                  alt_w_c, alt_w_n ) !coordinates for parent and nest\n");
                }

                if ( p->node_kind & FOURD )
                {
                    fprintf(fp,"ENDIF\n") ;
                    for ( d = p->ndims-1 ; d >= 3 ; d-- ) {
                        fprintf(fp,"ENDDO\n") ;
                    }
                    fprintf(fp,"ENDDO\n") ;
                } else {
                    fprintf(fp,"ENDIF\n") ; /* in_use_from_config */
                }
            } /* end of if variable has > 1 vertical level*/
        } /* end of if variable has vertical dimension*/
     } /* end of mask for down_path*/
  } /*end of loop over nodes*/
  return(0) ;
}

