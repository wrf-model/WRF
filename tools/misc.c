#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "protos.h"
#include "registry.h"
#include "data.h"

char *
dimension_with_colons( char * pre , char * tmp , node_t * p , char * post )
{
  int i ;
  if ( p == NULL ) return("") ;
  if ( p->ndims <= 0 && ! p->boundary_array ) return("") ;
  strcpy(tmp,"") ;
  if ( pre != NULL ) strcat(tmp,pre) ;
  if ( p->boundary_array )
  {
    if ( !strcmp( p->use , "_4d_bdy_array_" ) ) {
      strcat( tmp, ":,:,:,:,:" ) ;  /* boundary array for 4d tracer array */
    } else {
      strcat( tmp, ":,:,:,:" ) ;  /* most always have four dimensions */
    }
  }
  else
  {
    for ( i = 0 ; i < p->ndims ; i++ ) strcat(tmp,":,") ; 
    if ( p->node_kind & FOURD ) strcat(tmp,":,") ;       /* add an extra for 4d arrays */
    tmp[strlen(tmp)-1] = '\0' ;
  }
  if ( post != NULL ) strcat(tmp,post)  ;
  return(tmp) ;
}

char *
dimension_with_ones( char * pre , char * tmp , node_t * p , char * post )
{
  int i ;
  if ( p == NULL ) return("") ;
  if ( p->ndims <= 0 && ! p->boundary_array ) return("") ;
  strcpy(tmp,"") ;
  if ( pre != NULL ) strcat(tmp,pre) ;
  if ( p->boundary_array )
  {
    if ( !strcmp( p->use , "_4d_bdy_array_" ) ) {
      strcat( tmp, "1,1,1,1,1" ) ;  /* boundary array for 4d tracer array */
    } else {
      strcat( tmp, "1,1,1,1" ) ;  /* most always have four dimensions */
    }
  }
  else
  {
    for ( i = 0 ; i < p->ndims ; i++ ) strcat(tmp,"1,") ;
    if ( p->node_kind & FOURD ) strcat(tmp,"1,") ;       /* add an extra for 4d arrays */
    tmp[strlen(tmp)-1] = '\0' ;
  }
  if ( post != NULL ) strcat(tmp,post)  ;
  return(tmp) ;
}


char *
dimension_with_ranges( char * refarg , char * pre ,
                       char * tmp , node_t * p , char * post ,
                       char * nlstructname  )   /* added 20020130;
 						   provides name (with %) of structure in
						   which a namelist supplied dimension
                                                   should be dereference from, or ""  */
{
  int i ;
  char tx[NAMELEN] ;
  char r[NAMELEN],s[NAMELEN],four_d[NAMELEN] ;
  int   xdex, ydex, zdex ;
  node_t *xdim, *ydim, *zdim ;
  char *pp ;
  if ( p == NULL ) return("") ;
  if ( p->ndims <= 0 && !p->boundary_array ) return("") ;
  strcpy(tmp,"") ;
  if ( pre != NULL ) strcat(tmp,pre) ;
  strcpy(r,"") ;
  if ( refarg != NULL ) strcat(r,refarg) ;

  if ( p->boundary_array )
  {
    if ( p->ndims > 0 )
    {
      xdim = get_dimnode_for_coord( p , COORD_X ) ;
      ydim = get_dimnode_for_coord( p , COORD_Y ) ;
      zdim = get_dimnode_for_coord( p , COORD_Z ) ;
      if ( ydim == NULL )
       { fprintf(stderr,"dimension_with_ranges: y dimension not specified for %s\n",p->name) ; return("") ; }
      if ( xdim == NULL )
       { fprintf(stderr,"dimension_with_ranges: x dimension not specified for %s\n",p->name) ; return("") ; }
      
      xdex = xdim->dim_order ;
      ydex = ydim->dim_order ;

      if ( !strcmp( p->use , "_4d_bdy_array_" ) ) {   /* if a boundary array for a 4d tracer */
        strcpy(s, p->name ) ;  /* copy the name and then remove everything after last underscore */
        if ((pp=rindex( s, '_' )) != NULL ) *pp = '\0' ;
        sprintf( four_d, "num_%s,", s  ) ;
      } else {
        strcpy( four_d, "" ) ;
      }

      if ( zdim != NULL ) {
        zdex = zdim->dim_order ;
        sprintf(tx,"max(%sed3%d,%sed3%d),%ssd3%d:%sed3%d,%sspec_bdy_width,4,%s", r,xdex,r,ydex,r,zdex,r,zdex,r,four_d ) ;
      } else {
        sprintf(tx,"max(%sed3%d,%sed3%d),1,%sspec_bdy_width,4,%s", r,xdex,r,ydex,r,four_d ) ;
      }
    }
    else
    {
      sprintf(tx,"%sspec_bdy_width,",r ) ;
    }
    strcat(tmp,tx) ;
  }
  else
  {
    for ( i = 0 ; i < p->ndims ; i++ )
    {
      if ( p->dims[i] != NULL )
      {
        if      ( p->dims[i]->len_defined_how == DOMAIN_STANDARD )
	{
	  char *ornt ;
	  if      ( p->proc_orient == ALL_X_ON_PROC ) ornt = "x" ;
	  else if ( p->proc_orient == ALL_Y_ON_PROC ) ornt = "y" ;
	  else                                        ornt = "" ;
          sprintf(tx,"%ssm3%d%s:%sem3%d%s,",r,p->dims[i]->dim_order,ornt,r,p->dims[i]->dim_order,ornt) ;
	}
        else if ( p->dims[i]->len_defined_how == NAMELIST )
	{
#if 0
          if (!strcmp(p->dims[i]->assoc_nl_var_s,"1"))
            sprintf(tx,"%s%s,",r,p->dims[i]->assoc_nl_var_e ) ;
          else
            sprintf(tx,"%s%s:%s%s,",r,p->dims[i]->assoc_nl_var_s,r,p->dims[i]->assoc_nl_var_e ) ;
#else
          if (!strcmp(p->dims[i]->assoc_nl_var_s,"1"))
            sprintf(tx,"%s%s%s,",nlstructname,r,p->dims[i]->assoc_nl_var_e ) ;
          else
            sprintf(tx,"%s%s%s:%s%s%s,",nlstructname,r,p->dims[i]->assoc_nl_var_s,
                                        nlstructname,r,p->dims[i]->assoc_nl_var_e ) ;
#endif
        }
        else if ( p->dims[i]->len_defined_how == CONSTANT )
	{
          sprintf(tx,"%d:%d,",p->dims[i]->coord_start,p->dims[i]->coord_end) ;
	}
        strcat(tmp,tx) ;
      }
      else
      {
	fprintf(stderr,"WARNING: %s %d: something wrong with internal representation for dim %d\n",__FILE__,__LINE__,i) ;
      }
    }
  }
  tmp[strlen(tmp)-1] = '\0' ;
  if ( post != NULL ) strcat(tmp,post)  ;

  return(tmp) ;
}

char *
index_with_firstelem( char * pre , char * dref , char * tmp , node_t * p , char * post )
{
  int i ;
  char tx[NAMELEN] ;
  char tmp2[NAMELEN] ;
  if ( p == NULL ) return("") ;
  if ( p->ndims <= 0 ) return("") ;
  strcpy(tmp,"") ;
  if ( pre != NULL ) strcat(tmp,pre) ;

  if ( p->boundary_array )
  {
    if ( p->ndims > 0 )
    {
#if 0
      for ( i = 0 ; i < p->ndims ; i++ )
      {
	sprintf(tx,"1,") ;
        strcat(tmp,tx) ;
      }
#endif
      if ( !strcmp( p->use , "_4d_bdy_array_" ) ) {
        strcat(tmp,"1,1,1,1,1,") ;
      } else {
        strcat(tmp,"1,1,1,1,") ;
      }
    }
    else
    {
      sprintf(tx,"1," ) ;
      strcat(tmp,tx) ;
    }
  }
  else
  {
    for ( i = 0 ; i < p->ndims ; i++ )
    {
      if ( p->dims[i] != NULL )
      {
        switch ( p->dims[i]->len_defined_how )
	{
          case (DOMAIN_STANDARD) :
            {
            char *ornt ;
            if      ( p->proc_orient == ALL_X_ON_PROC ) ornt = "x" ;
            else if ( p->proc_orient == ALL_Y_ON_PROC ) ornt = "y" ;
            else                                        ornt = "" ;
              sprintf(tx,"%ssm3%d%s,",dref,p->dims[i]->dim_order,ornt) ;
              strcat(tmp,tx) ;
            }
	    break ;
          case (NAMELIST) :
	    strcat(tmp,p->dims[i]->assoc_nl_var_s) ;
	    strcat(tmp,",") ;
	    break ;
          case (CONSTANT) :
            sprintf(tmp2,"%d",p->dims[i]->coord_start) ;
	    strcat(tmp,tmp2) ;
	    strcat(tmp,",") ;
	    break ;
          default : break ;
	}
      }
      else
      {
	fprintf(stderr,"WARNING: %s %d: something wrong with internal representation for dim %d\n",__FILE__,__LINE__,i) ;
      }
    }
  }
  tmp[strlen(tmp)-1] = '\0' ;  /* remove trailing comma */
  if ( post != NULL ) strcat(tmp,post)  ;
  return(tmp) ;
}

char *
declare_array_as_pointer( char * tmp , node_t * p )
{
  strcpy( tmp , "" ) ;
  if ( p != NULL )
    if ( p->ndims > 0 || p->boundary_array ) strcpy ( tmp, ",POINTER" ) ;
  return(tmp);
}

char *
field_type( char * tmp , node_t * p )
{
  if ( p == NULL ) {
    strcpy( tmp , "" ) ;
  } else if ( p->type == NULL ) {
    strcpy( tmp , "" ) ;
  } else if ( p->type->type_type == SIMPLE ) {
    strcpy( tmp , p->type->name ) ;
  } else {
    sprintf( tmp , "TYPE(%s)", p->type->name ) ;
  }
  return( tmp ) ;
}

char *
field_name( char * tmp , node_t * p , int tag )
{
  if ( p == NULL ) return("") ;
  if ( tag < 1 )
  {
    strcpy(tmp,p->name) ;
    if ( p->scalar_array_member ) strcpy(tmp,p->use) ;
  }
  else
  {
    sprintf(tmp,"%s_%d",p->name,tag) ;
    if ( p->scalar_array_member ) sprintf(tmp,"%s_%d",p->use,tag) ;
  }
  return( tmp ) ;
}

int
print_warning( FILE * fp , char * fname )
{
fprintf(fp,"!STARTOFREGISTRYGENERATEDINCLUDE '%s'\n", fname) ;
fprintf(fp,"!\n") ;
fprintf(fp,"! WARNING This file is generated automatically by use_registry\n") ;
fprintf(fp,"! using the data base in the file named Registry.\n") ;
fprintf(fp,"! Do not edit.  Your changes to this file will be lost.\n") ;
fprintf(fp,"!\n") ;
return(0) ;
}

close_the_file( FILE * fp )
{
fprintf(fp,"!ENDOFREGISTRYGENERATEDINCLUDE\n") ;
fclose(fp) ;
}

int
make_entries_uniq ( char * fname )
{
  char tempfile[NAMELEN] ;
  char commline[4096] ;
  sprintf(tempfile,"regtmp1%d",getpid()) ;
  sprintf(commline,"%s < %s > %s ; %s %s %s ",
          UNIQSORT,fname,tempfile,
          MVCOMM,tempfile,fname ) ;
  return(system(commline)) ;
}

int
add_warning ( char * fname )
{
  FILE * fp ;
  char tempfile[NAMELEN] ;
  char tempfile1[NAMELEN] ;
  char commline[4096] ;
  sprintf(tempfile,"regtmp1%d",getpid()) ;
  sprintf(tempfile1,"regtmp2%d",getpid()) ;
  if (( fp = fopen( tempfile, "w" )) == NULL ) return(1) ;
  print_warning(fp,tempfile) ; 
  close_the_file(fp) ;
  sprintf(commline,"%s %s %s > %s ; %s %s %s ; %s %s ",
          CATCOMM,tempfile,fname,tempfile1,
          MVCOMM,tempfile1,fname,
          RMCOMM,tempfile) ;
  return(system(commline)) ;
}

static int NumCores ;
static char dyncores[MAX_DYNCORES][NAMELEN] ;

int
init_core_table()
{
  NumCores = 0 ;
  return(0) ;
}

int
get_num_cores()
{
  return( NumCores ) ;
}

char *
get_corename_i(int i)
{
  if ( i >= 0 && i < NumCores ) return( dyncores[i] ) ;
  return(NULL) ;
}

int
add_core_name ( char * name )
{
  if ( name == NULL ) return(1) ;
  if (get_core_name ( name ) == NULL )
  {
    if ( NumCores >= MAX_DYNCORES ) return(1) ;
    strcpy( dyncores[NumCores++] , name ) ;
  }
  return(0) ;
}

char *
get_core_name ( char * name )
{
  int i ;
  if ( name == NULL ) return(NULL) ;
  for ( i = 0 ; i < NumCores ; i++ )
  {
    if ( !strcmp(name,dyncores[i]) ) return( dyncores[i] ) ; 
  }
  return(NULL) ;
}

/* DESTRUCTIVE */
char *
make_upper_case ( char * str )
{
  char * p ;
  if ( str == NULL ) return (NULL) ;
  for ( p = str ; *p ; p++ ) *p = toupper(*p) ; 
  return(str) ;
}

/* DESTRUCTIVE */
char *
make_lower_case ( char * str )
{
  char * p ;
  if ( str == NULL ) return (NULL) ;
  for ( p = str ; *p ; p++ ) *p = tolower(*p) ; 
  return(str) ;
}

/* Routines for keeping typedef history  -ajb */

static int NumTypeDefs ;
static char typedefs[MAX_TYPEDEFS][NAMELEN] ;

int
init_typedef_history()
{
  NumTypeDefs = 0 ;
  return(0) ;
}

int
get_num_typedefs()
{
  return( NumTypeDefs ) ;
}

char *
get_typename_i(int i)
{
  if ( i >= 0 && i < NumTypeDefs ) return( typedefs[i] ) ;
  return(NULL) ;
}

int
add_typedef_name ( char * name )
{
  if ( name == NULL ) return(1) ;
  if ( get_typedef_name ( name ) == NULL )
  {
    if ( NumTypeDefs >= MAX_TYPEDEFS ) return(1) ;
    strcpy( typedefs[NumTypeDefs++] , name ) ;
  }
  return(0) ;
}

char *
get_typedef_name ( char * name )
{
  int i ;
  if ( name == NULL ) return(NULL) ;
  for ( i = 0 ; i < NumTypeDefs ; i++ )
  {
    if ( !strcmp(name,typedefs[i]) ) return( typedefs[i] ) ; 
  }
  return(NULL) ;
}
