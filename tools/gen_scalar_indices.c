#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "protos.h"
#include "registry.h"
#include "data.h"


int
gen_scalar_indices ( char * dirname )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char * fn = "scalar_indices.inc" ;
  char * fn2 = "scalar_tables.inc" ;
  char * fn3 = "scalar_tables_init.inc" ;
  char * fn4 = "scalar_indices_init.inc" ;

  strcpy( fname, fn ) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  gen_scalar_indices1 ( fp ) ;
  close_the_file( fp ) ;

  strcpy( fname, fn2 ) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn2) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) { fprintf(stderr,"returning\n") ; return(1) ; }
  print_warning(fp,fname) ;
  gen_scalar_tables ( fp ) ;
  close_the_file( fp ) ;

  strcpy( fname, fn3 ) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn3) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) { fprintf(stderr,"returning\n") ; return(1) ; }
  print_warning(fp,fname) ;
  gen_scalar_tables_init ( fp ) ;
  close_the_file( fp ) ;

  strcpy( fname, fn4 ) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn4) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) { fprintf(stderr,"returning\n") ; return(1) ; }
  print_warning(fp,fname) ;
  gen_scalar_indices_init ( fp ) ;
  close_the_file( fp ) ;

  return(0) ;
}

int
gen_scalar_tables ( FILE * fp )
{
  node_t * p ;
  for ( p = FourD ; p != NULL ; p=p->next4d )
  {
    fprintf(fp,"  INTEGER :: %s_index_table( param_num_%s, max_domains )\n",p->name,p->name )  ;
    fprintf(fp,"  INTEGER :: %s_num_table( max_domains )\n", p->name,p->name ) ;
  }
  return(0) ;
}

int
gen_scalar_tables_init ( FILE * fp )
{
  node_t * p ;
  for ( p = FourD ; p != NULL ; p=p->next4d )
  {
    fprintf(fp,"  %s_num_table( j ) = 1\n",p->name )  ;
  }
  return(0) ;
}

int
gen_scalar_indices_init ( FILE * fp )
{
  node_t * p ;
  for ( p = FourD ; p != NULL ; p=p->next4d )
  {
    fprintf(fp,"  num_%s = %s_num_table( idomain )\n",p->name,p->name )  ;
  }
  return(0) ;
}

int
gen_scalar_indices1 ( FILE * fp )
{
  node_t * p, * memb , * pkg, * rconfig, * fourd ; 
  char * c , *pos1, *pos2 ;
  char assoc_namelist_var[NAMELEN], assoc_namelist_choice[NAMELEN], assoc_4d[NAMELEN] ;
  char scalars_str[NAMELEN] ;
  char * scalars ;

  for ( p = FourD ; p != NULL ; p = p->next )
   { for ( memb = p->members ; memb != NULL ; memb = memb->next )
      { if ( strcmp(memb->name,"-") ) fprintf(fp,"  P_%s = 1 ; F_%s = .FALSE. \n", memb->name, memb->name ) ; } }

  for ( pkg = Packages ; pkg != NULL ; pkg = pkg->next )
  {
    strcpy( assoc_namelist_var , pkg->pkg_assoc ) ;
    if ((c = index( assoc_namelist_var , '=' ))==NULL) continue ;
    *c = '\0' ; c += 2 ;
    strcpy( assoc_namelist_choice , c ) ;
    if ((rconfig=get_rconfig_entry ( assoc_namelist_var )) == NULL )
     { fprintf(stderr,
       "WARNING: There is no associated namelist variable %s\n",
        assoc_namelist_var) ; return(1) ; }
    fprintf(fp,"  IF (model_config_rec%%%s%s==%s)THEN\n",
		 assoc_namelist_var,
		 (atoi(rconfig->nentries)!=1)?"(idomain)":"",  /* a little tricky; atoi of nentries will be '0' for a string like max_domains */
		 assoc_namelist_choice) ;
    strcpy(scalars_str,pkg->pkg_4dscalars) ;

    if ((scalars = strtok_rentr(scalars_str,";", &pos1)) != NULL)
    {
      while ( scalars != NULL ) {

        if ((c = strtok_rentr(scalars,":",&pos2)) != NULL) strcpy(assoc_4d,c) ; /* get name of associated 4d array */
        if (strcmp(c,"-")) {
          if ((fourd=get_4d_entry( assoc_4d )) == NULL )
           { fprintf(stderr, "WARNING: There is no 4D array named %s\n",assoc_4d);continue;}
          for ( c = strtok_rentr(NULL,",",&pos2) ; c != NULL ; c = strtok_rentr(NULL,",",&pos2) )
          {
            if ( get_entry( c , fourd->members ) == NULL )
              { fprintf(stderr, "WARNING: %s is not a member of 4D array %s\n",c,assoc_4d);continue;}
            fprintf(fp,"   IF ( %s_index_table( PARAM_%s , idomain ) .lt. 1 ) THEN\n",assoc_4d,c) ;
            fprintf(fp,"     %s_num_table(idomain) = %s_num_table(idomain) + 1\n",assoc_4d,assoc_4d) ;
            fprintf(fp,"     P_%s = %s_num_table(idomain)\n",c,assoc_4d) ;
            fprintf(fp,"     %s_index_table( PARAM_%s , idomain ) = P_%s\n",assoc_4d,c,c) ;
            fprintf(fp,"   ELSE\n") ;
            fprintf(fp,"     P_%s = %s_index_table( PARAM_%s , idomain )\n",c,assoc_4d,c)  ;
            fprintf(fp,"   END IF\n") ;
            fprintf(fp,"   F_%s = .TRUE.\n",c) ;
          }
        }

        scalars = strtok_rentr(NULL,";", &pos1) ;

      }
    }

    fprintf(fp,"  END IF\n") ;
  }

  return(0) ;
}

