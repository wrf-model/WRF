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

static int
set_dim_strs_x ( node_t *node , char ddim[3][2][NAMELEN], char mdim[3][2][NAMELEN], char pdim[3][2][NAMELEN] , char * prepend , int sw_disregard_stag, int sw_reorder, int sw_no_prepend_r1 )
{
  int i, j, ii ;
  node_t *p ;
  char d, d1 ;
  char * stag ;
  char r1[NAMELEN] ;

  if ( sw_no_prepend_r1 ) {
    strcpy(r1,"") ;
  } else {
    strcpy(r1,"grid%") ;
  }
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
      char *ornt ;
      if      ( node->proc_orient == ALL_X_ON_PROC ) ornt = "x" ;
      else if ( node->proc_orient == ALL_Y_ON_PROC ) ornt = "y" ;
      else                                        ornt = "" ;

      if ( p->subgrid ) {


          switch( p->coord_axis )
          {
          case(COORD_X) : d = 'i' ;  d1 = 'x' ; break ;
          case(COORD_Y) : d = 'j' ;  d1 = 'y' ; break ;
          case(COORD_Z) : d = 'k' ;  d1 = 'z' ; break ;
          default :  break ;
          }

        sprintf(ddim[i][0],"%s%cds",prepend,d) ;
        sprintf(ddim[i][1],"MAX(1,%s%cde * %ssr_%c) ",prepend,d,r1,d1) ;
        sprintf(mdim[i][0],"(%s%cms-1)*%ssr_%c+1",prepend,d,r1,d1) ;
        sprintf(mdim[i][1],"MAX(1,%s%cme*%ssr_%c)",prepend,d,r1,d1) ;
        sprintf(pdim[i][0],"(%s%cps-1)*%ssr_%c+1",prepend,d,r1,d1) ;
        sprintf(pdim[i][1],"MAX(1,%s%cpe*%ssr_%c)",prepend,d,r1,d1) ;

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
        sprintf(mdim[i][0],"%s%cms%s",prepend,d,ornt) ;
        sprintf(mdim[i][1],"%s%cme%s",prepend,d,ornt) ;
        sprintf(pdim[i][0],"%s%cps%s",prepend,d,ornt) ;
        if ( ! sw_disregard_stag )
          sprintf(pdim[i][1],"MIN( %s, %s%cpe%s )",ddim[i][1],prepend,d,ornt) ;
        else
          sprintf(pdim[i][1],"%s%cpe%s",prepend,d,ornt) ;
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
  set_dim_strs_x ( node , ddim, mdim, pdim, prepend , sw_disregard_stag, 1, 0 ) ; /* 1 = reorder according to strg order */
  return 0; /* SamT: bug fix: return a value */
}

/* version that doesn't permute according to index order -- always i, k, then j
   useful for standard argument lists -- e.g. calls to interp in nesting  */
int
set_dim_strs2 ( node_t *node , char ddim[3][2][NAMELEN], char mdim[3][2][NAMELEN], char pdim[3][2][NAMELEN] , char * prepend , int sw_disregard_stag )
{
  set_dim_strs_x ( node , ddim, mdim, pdim, prepend , sw_disregard_stag, 0, 0 ) ; /* 0 = reorder according to strg order */
  return 0; /* SamT: bug fix: return a value */
}

int
set_dim_strs3 ( node_t *node , char ddim[3][2][NAMELEN], char mdim[3][2][NAMELEN], char pdim[3][2][NAMELEN] , char * prepend , int sw_disregard_stag )
{
  set_dim_strs_x ( node , ddim, mdim, pdim, prepend , sw_disregard_stag, 1, 1 ) ; /* 1 = reorder according to strg order */
  return 0; /* SamT: bug fix: return a value */
}
