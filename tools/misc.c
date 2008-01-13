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
    if ( ! sw_new_bdys ) { strcat( tmp,":,") ; }
    if ( !strcmp( p->use , "_4d_bdy_array_" ) ) {
      strcat( tmp, ":,:,:,:" ) ;  /* boundary array for 4d tracer array */
    } else {
      strcat( tmp, ":,:,:" ) ;  /* most always have four dimensions */
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
  char r[NAMELEN],s[NAMELEN],four_d[NAMELEN] ;
  char *pp ;
  if ( p == NULL ) return("") ;
  if ( p->ndims <= 0 && ! p->boundary_array ) return("") ;
  strcpy(tmp,"") ;
  if ( pre != NULL ) strcat(tmp,pre) ;

  if ( p->boundary_array )
  {
    if ( ! sw_new_bdys ) { strcpy( tmp,"(1,") ; }
    if ( !strcmp( p->use , "_4d_bdy_array_" ) ) {   /* if a boundary array for a 4d tracer */
      strcpy(s, p->name ) ;  /* copy the name and then remove everything after last underscore */
      if ((pp=rindex( s, '_' )) != NULL ) *pp = '\0' ;
      sprintf( four_d, "num_%s,", s  ) ;
    } else {
      strcpy( four_d, "" ) ;
    }

    if ( !strcmp( p->use , "_4d_bdy_array_" ) ) {
      sprintf( r, "1,1,1,%s", four_d ) ;  /* boundary array for 4d tracer array */
      strcat( tmp, r ) ;
    } else {
      strcat( tmp, "1,1,1," ) ;
    }
    tmp[strlen(tmp)-1] = '\0' ;
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
                       int bdy , /* as defined in data.h */
                       char * tmp , node_t * p , char * post ,
                       char * nlstructname  )   /* added 20020130;
 						   provides name (with %) of structure in
						   which a namelist supplied dimension
                                                   should be dereference from, or ""  */
{
  int i ;
  char tx[NAMELEN] ;
  char r[NAMELEN],s[NAMELEN],four_d[NAMELEN] ;
  int   bdex, xdex, ydex, zdex ;
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
      if ( sw_new_bdys ) {
        if      ( bdy == P_XSB || bdy == P_XEB ) { bdex = ydex ; } 
        else if ( bdy == P_YSB || bdy == P_YEB ) { bdex = xdex ; }
        else { fprintf(stderr,"REGISTRY WARNING: internal error %s %d, bdy=%d,%s,%d \n",__FILE__,__LINE__,bdy,p->name,p->boundary) ; }
        if ( zdim != NULL ) {
          zdex = zdim->dim_order ;
          sprintf(tx,"%ssm3%d:%sem3%d,%ssm3%d:%sem3%d,%sspec_bdy_width,%s", r,bdex,r,bdex,r,zdex,r,zdex,r,four_d ) ;
        } else {
          sprintf(tx,"%ssm3%d:%sem3%d,1,%sspec_bdy_width,%s", r,bdex,r,bdex,r,four_d ) ;
        }
      } else {
        if ( zdim != NULL ) {
          zdex = zdim->dim_order ;
          sprintf(tx,"max(%sed3%d,%sed3%d),%ssd3%d:%sed3%d,%sspec_bdy_width,4,%s", r,xdex,r,ydex,r,zdex,r,zdex,r,four_d ) ;
        } else {
          sprintf(tx,"max(%sed3%d,%sed3%d),1,%sspec_bdy_width,4,%s", r,xdex,r,ydex,r,four_d ) ;
        }
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
      range_of_dimension( r, tx , i , p , nlstructname ) ;
      strcat(tmp,tx) ;
      strcat(tmp,",") ;
    }
  }
  tmp[strlen(tmp)-1] = '\0' ;
  if ( post != NULL ) strcat(tmp,post)  ;

  return(tmp) ;
}

int
range_of_dimension ( char * r , char * tx , int i , node_t * p , char * nlstructname )
{
   char s[NAMELEN], e[NAMELEN] ;

   get_elem( r , nlstructname , s , i , p , 0 ) ;
   get_elem( r , nlstructname , e , i , p , 1 ) ;
   sprintf(tx,"%s:%s", s , e ) ;

}

char *
index_with_firstelem( char * pre , char * dref , int bdy ,  /* as defined in data.h */
                      char * tmp , node_t * p , char * post )
{
  int i ;
  char tx[NAMELEN] ;
  char tmp2[NAMELEN] ;
  int  bdex, xdex, ydex, zdex ;
  node_t *xdim, *ydim, *zdim ;
  char r[NAMELEN] ;

  if ( p == NULL ) return("") ;
  if ( p->ndims <= 0 ) return("") ;
  strcpy(tmp,"") ;
  if ( pre != NULL ) strcat(tmp,pre) ;

  strcpy(r,"") ;
  if ( dref != NULL ) strcat(r,dref) ;

  if ( p->boundary_array )
  {
    if ( sw_new_bdys ) {

      xdim = get_dimnode_for_coord( p , COORD_X ) ;
      ydim = get_dimnode_for_coord( p , COORD_Y ) ;
      zdim = get_dimnode_for_coord( p , COORD_Z ) ;
      if ( ydim == NULL )
       { fprintf(stderr,"dimension_with_ranges: y dimension not specified for %s\n",p->name) ; return("") ; }
      if ( xdim == NULL )
       { fprintf(stderr,"dimension_with_ranges: x dimension not specified for %s\n",p->name) ; return("") ; }

      xdex = xdim->dim_order ;
      ydex = ydim->dim_order ;

      if      ( bdy == P_XSB || bdy == P_XEB ) { bdex = ydex ; } 
      else if ( bdy == P_YSB || bdy == P_YEB ) { bdex = xdex ; }
      else { fprintf(stderr,"REGISTRY WARNING: internal error %s %d \n",__FILE__,__LINE__) ; }
      if ( p->ndims > 0 )
      {
        if ( !strcmp( p->use , "_4d_bdy_array_" ) ) {
          sprintf(tmp,"%ssm3%d,%ssm3%d,1,1", r,bdex,r,zdex ) ;
        } else {
          sprintf(tmp,"%ssm3%d,%ssm3%d,1", r,bdex,r,zdex ) ;
        }
      }
      else
      {
        sprintf(tx,"1," ) ;
        strcat(tmp,tx) ;
      }

    } else {
      if ( p->ndims > 0 )
      {
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
  }
  else
  {
    for ( i = 0 ; i < p->ndims ; i++ )
    {
      get_elem( dref, "", tx, i, p , 0 ) ;
      strcat( tmp, tx ) ;
      strcat(tmp,",") ;
    }
  }
  tmp[strlen(tmp)-1] = '\0' ;  /* remove trailing comma */
  if ( post != NULL ) strcat(tmp,post)  ;
  return(tmp) ;
}

get_elem ( char * structname , char * nlstructname , char * tx , int i , node_t * p , int first_last )
{
   char dref[NAMELEN], nlstruct[NAMELEN] ;
   char d, d1 ;

   if ( structname == NULL ) { strcpy( dref, "" ) ;}
   else                      { strcpy( dref, structname ) ; }
   if ( nlstructname == NULL ) { strcpy( nlstruct, "" ) ;}
   else                        { strcpy( nlstruct, nlstructname ) ; }
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

           switch( p->dims[i]->coord_axis )
           {
           case(COORD_X) : d = 'i' ;  d1 = 'x' ; break ;
           case(COORD_Y) : d = 'j' ;  d1 = 'y' ; break ;
           case(COORD_Z) : d = 'k' ;  d1 = 'z' ; break ;
           default :  break ;
           }

           if ( p->dims[i]->subgrid ) 
           {
             if ( first_last == 0 ) { /*first*/
               sprintf(tx,"(%ssm3%d%s-1)*%ssr_%c+1",dref,p->dims[i]->dim_order,ornt,dref,d1) ;
             }else{                   /*last*/
               sprintf(tx,"%sem3%d%s*%ssr_%c"      ,dref,p->dims[i]->dim_order,ornt,dref,d1) ;
             }
           }
           else
           {
             sprintf(tx,"%s%cm3%d%s",dref,first_last==0?'s':'e',p->dims[i]->dim_order,ornt) ;
           }
         }
         break ;
       case (NAMELIST) :
         if ( first_last == 0 ) { if ( !strcmp( p->dims[i]->assoc_nl_var_s , "1" ) ) {
                                    sprintf(tx,"%s",p->dims[i]->assoc_nl_var_s) ;
                                  } else {
                                    sprintf(tx,"%s%s%s",nlstructname,structname,p->dims[i]->assoc_nl_var_s) ; 
                                  }
                                }
         else                   { sprintf(tx,"%s%s%s",nlstructname,structname,p->dims[i]->assoc_nl_var_e) ; }
         break ;
       case (CONSTANT) :
         if ( first_last == 0 ) { sprintf(tx,"%d",p->dims[i]->coord_start) ; }
         else                   { sprintf(tx,"%d",p->dims[i]->coord_end) ; }
         break ;
       default : break ;
     }
   }
   else
   {
     fprintf(stderr,"WARNING: %s %d: something wrong with internal representation for dim %d\n",__FILE__,__LINE__,i) ;
   }
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
field_name( char * tmp , node_t * p , int tag  )
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

char *
field_name_bdy( char * tmp , node_t * p , int tag, int bdy  )
{
  if ( p == NULL ) return("") ;
  if ( tag < 1 )
  {
    strcpy(tmp,p->name) ;
    if ( p->scalar_array_member ) strcpy(tmp,p->use) ;
    if ( p->boundary_array ) strcat(tmp,bdy_indicator(bdy)) ;
  }
  else
  {
    sprintf(tmp,"%s_%d",p->name,tag) ;
    if ( p->scalar_array_member ) sprintf(tmp,"%s_%d",p->use,tag) ;
    if ( p->boundary_array ) strcat(tmp,bdy_indicator(bdy)) ;
  }
  return( tmp ) ;
}

static char *emp_str = "" ;
static char *xs_str = "xs" ;
static char *xe_str = "xe" ;
static char *ys_str = "ys" ;
static char *ye_str = "ye" ;

char *
bdy_indicator( int bdy )
{
  char * res ;
  res = emp_str ;
  if      ( bdy == P_XSB ) { res = xs_str ; }
  else if ( bdy == P_XEB ) { res = xe_str ; }
  else if ( bdy == P_YSB ) { res = ys_str ; }
  else if ( bdy == P_YEB ) { res = ye_str ; }
  return(res) ;
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

int
associated_with_4d_array( node_t * p ) 
{
  int res = 0 ;
  node_t * possble ;
  char * last_underscore ;
  char name_copy[128] ;
  if ( p != NULL )
  {
    /* check this variable and see if it is a boundary variable that is associated with a 4d array */
    strcpy( name_copy, p->name ) ;
    if (( last_underscore = rindex( name_copy , '_' )) != NULL ) {
      if ( !strcmp( last_underscore , "_b" ) || !strcmp( last_underscore , "_bt" ) ) {
        *last_underscore = '\0' ;
        if (( possble = get_entry( name_copy , Domain.fields )) != NULL ) {
          res = possble->node_kind & FOURD ;
        }
      }
    }
  }
  return(res) ;
}

char *
array_size_expression ( char * refarg , char * pre , 
                       int bdy , /* as defined in data.h */
                       char * tmp , node_t * p , char * post ,
                       char * nlstructname  )   /* provides name (with %) of structure in
						   which a namelist supplied dimension
                                                   should be dereference from, or ""  */
{
  int i ;
  char tx[NAMELEN] ;
  char r[NAMELEN],s[NAMELEN],four_d[NAMELEN] ;
  int   bdex, xdex, ydex, zdex ;
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
        sprintf( four_d, "*num_%s,", s  ) ;
      } else {
        strcpy( four_d, "" ) ;
      }
      if ( sw_new_bdys ) {
        if      ( bdy == P_XSB || bdy == P_XEB ) { bdex = ydex ; } 
        else if ( bdy == P_YSB || bdy == P_YEB ) { bdex = xdex ; }
        else { fprintf(stderr,"REGISTRY WARNING: internal error %s %d, bdy=%d,%s,%d \n",__FILE__,__LINE__,bdy,p->name,p->boundary) ; }
        if ( zdim != NULL ) {
          zdex = zdim->dim_order ;
          sprintf(tx,"(%sem3%d-%ssm3%d+1)*(%sem3%d-%ssm3%d+1)*(%sspec_bdy_width)%s", r,bdex,r,bdex,r,zdex,r,zdex,r,four_d ) ;
        } else {
          sprintf(tx,"(%sem3%d-%ssm3%d+1)*(%sspec_bdy_width)%s", r,bdex,r,bdex,r,four_d ) ;
        }
      } else {
        if ( zdim != NULL ) {
          zdex = zdim->dim_order ;
          sprintf(tx,"max(%sed3%d,%sed3%d)*(%sed3%d-%ssd3%d+1)*%sspec_bdy_width*4*%s", r,xdex,r,ydex,r,zdex,r,zdex,r,four_d ) ;
        } else {
          sprintf(tx,"max(%sed3%d,%sed3%d)*%sspec_bdy_width*4*%s", r,xdex,r,ydex,r,four_d ) ;
        }
        if ( tx[strlen(tx)-1] == '*' ) tx[strlen(tx)-1] = '\0' ;  /* chop trailing * if four_d is "" */
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
      dimension_size_expression( r, tx , i , p , nlstructname ) ;
      strcat(tmp,tx) ;
      strcat(tmp,")*(") ;
    }
  }
  if ( tmp[strlen(tmp)-1] == '(' ) {
     tmp[strlen(tmp)-3] = '\0' ;  /* get rid of trailing )*( */
  } else if ( tmp[strlen(tmp)-1] == ',' ) {
     tmp[strlen(tmp)-1] = '\0' ;
  }
  if ( post != NULL ) strcat(tmp,post)  ;

  return(tmp) ;
}

int
dimension_size_expression ( char * r , char * tx , int i , node_t * p , char * nlstructname )
{
   char s[NAMELEN], e[NAMELEN] ;

   get_elem( r , nlstructname , s , i , p , 0 ) ;
   get_elem( r , nlstructname , e , i , p , 1 ) ;
   sprintf(tx,"((%s)-(%s)+1)", e , s ) ;

}
