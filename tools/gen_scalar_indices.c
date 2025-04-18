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

#define NULLCHARPTR   (char *) 0

/* About the only place the stringify macro is useful is in the
   definition of the stringify_const macro.  Most other places, you
   can put the quotes on yourself.  */
#define stringify_const(value) stringify(value)
#define stringify(value) #value

int
gen_scalar_indices ( char * dirname )
{
  FILE * fp, *fp5[26] ;
  char  fname[NAMELEN], fname5[2 * NAMELEN + EXTRA_FOR_DEST_BUFFER] ;
  char * fn = "scalar_indices.inc" ;
  char * fn2 = "scalar_tables.inc" ;
  char * fn3 = "scalar_tables_init.inc" ;
  char * fn4 = "scalar_indices_init.inc" ;
  int i ;

  char fn5[26][NAMELEN] = { '\0' } ;

  strcpy( fname, fn ) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;

 /* hashing to make the run time function being generated faster */
  for ( i = 0 ; i < 26 ; i++ ) 
  { 
    sprintf(fn5[i],"in_use_for_config_%c.inc",'a'+i) ;
    strcpy( fname5, fn5[i] ) ;
    if ( strlen(dirname) > 0 ) {
      /* The stringify_const(NAMELEN) is to get something like %.512s
	 in the format string, so snprintf won't copy more than
	 NAMELEN elements of the entry of fn5 (each NAMELEN chars long) */
      snprintf(fname5,sizeof(fname5),"%s/%." stringify_const(NAMELEN) "s",dirname,fn5[i]) ;
    } else {
      snprintf(fname5, NAMELEN + 1, "%." stringify_const(NAMELEN) "s", fn5[i]);
    }
    if ((fp5[i] = fopen( fname5 , "w" )) == NULL ) return(1) ;
    print_warning(fp5[i],fname5) ;
  }
  gen_scalar_indices1 ( fp, fp5 ) ;
  close_the_file( fp ) ;
  for ( i = 0 ; i < 26 ; i++ ) 
  {
    close_the_file( fp5[i] ) ;
  }

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
    fprintf(fp,"  INTEGER, TARGET :: %s_index_table( param_num_%s, max_domains )\n",p->name,p->name )  ;
    fprintf(fp,"  INTEGER, TARGET :: %s_num_table( max_domains )\n", p->name ) ;
    fprintf(fp,"  TYPE(streamrec), TARGET :: %s_streams_table( max_domains, param_num_%s )\n", p->name,p->name ) ;
    fprintf(fp,"  LOGICAL, TARGET :: %s_boundary_table( max_domains, param_num_%s )\n", p->name,p->name ) ;
    fprintf(fp,"  CHARACTER*256, TARGET :: %s_dname_table( max_domains, param_num_%s )\n", p->name,p->name ) ;
    fprintf(fp,"  CHARACTER*256, TARGET :: %s_desc_table( max_domains, param_num_%s )\n", p->name,p->name ) ;
    fprintf(fp,"  CHARACTER*256, TARGET :: %s_units_table( max_domains, param_num_%s )\n", p->name,p->name ) ;
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
gen_scalar_indices1 ( FILE * fp, FILE ** fp2 )
{
  node_t * p, * memb , * pkg, * rconfig, * fourd, *x ; 
  char * c , *pos1, *pos2 ;
  char assoc_namelist_var[NAMELEN], assoc_namelist_choice[NAMELEN], assoc_4d[NAMELEN_LONG], fname[NAMELEN_LONG] ;
  char fname2[NAMELEN], tmp1[NAMELEN + EXTRA_FOR_DEST_BUFFER], tmp2[NAMELEN + EXTRA_FOR_DEST_BUFFER] ;
  char scalars_str[NAMELEN_LONG] ;
  char * scalars ;
  int i ;

/*  for ( p = FourD ; p != NULL ; p = p->next )
   { for ( memb = p->members ; memb != NULL ; memb = memb->next )
      { if ( strcmp(memb->name,"-") ) fprintf(fp,"  P_%s = 1 ; F_%s = .FALSE. \n", memb->name, memb->name ) ; } } */

  for ( p = FourD ; p != NULL ; p = p->next ) {
    if( strncmp( p->name,"irr_diag",8 ) ) {
      for ( memb = p->members ; memb != NULL ; memb = memb->next ) {
        if ( strcmp(memb->name,"-") ) {
	  fprintf(fp,"  P_%s = 1 ; F_%s = .FALSE. \n", memb->name, memb->name);
	}
      }
    }
  }

  fprintf(stderr,"Packages in gen_scalar_indices1\n");

  for ( pkg = Packages ; pkg != NULL ; pkg = pkg->next )
  {
    strcpy( assoc_namelist_var , pkg->pkg_assoc ) ;

    if ((c = index( assoc_namelist_var , '=' ))==NULL) continue ;
    *c = '\0' ; c += 2 ;
    strcpy( assoc_namelist_choice , c ) ;
    if ((rconfig=get_rconfig_entry ( assoc_namelist_var )) == NULL )
     { fprintf(stderr,
       "WARNING: There is no associated namelist variable %s\n",
        assoc_namelist_var) ; continue ; }
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
          if ( (fourd=get_4d_entry( assoc_4d )) != NULL || !strcmp( assoc_4d, "state" ) ) {
            for ( c = strtok_rentr(NULL,",",&pos2) ; c != NULL ; c = strtok_rentr(NULL,",",&pos2) )
            {
              if ( fourd != NULL && ( ( x = get_entry_r( c , NULL, fourd->members )) != NULL ) ) {
                fprintf(fp,"   IF ( %s_index_table( PARAM_%s , idomain ) .lt. 1 ) THEN\n",assoc_4d,c) ;
                fprintf(fp,"     %s_num_table(idomain) = %s_num_table(idomain) + 1\n",assoc_4d,assoc_4d) ;
                fprintf(fp,"     P_%s = %s_num_table(idomain)\n",c,assoc_4d) ;
                fprintf(fp,"     %s_index_table( PARAM_%s , idomain ) = P_%s\n",assoc_4d,c,c) ;
                fprintf(fp,"   ELSE\n") ;
                fprintf(fp,"     P_%s = %s_index_table( PARAM_%s , idomain )\n",c,assoc_4d,c)  ;
                fprintf(fp,"   END IF\n") ;
                {
                  char fourd_bnd[NAMELEN_LONG + EXTRA_FOR_DEST_BUFFER] = { '\0' } ;
                  /* check for the existence of a fourd boundary array associated with this 4D array */
                  /* set io_mask accordingly for gen_wrf_io to know that it should generate i/o for _b and _bt */
                  /* arrays */
                  snprintf(fourd_bnd, NAMELEN_LONG + EXTRA_FOR_DEST_BUFFER - 1, "%s_b",assoc_4d) ;
                  if ( get_entry_r( fourd_bnd, NULL, Domain.fields) != NULL ) {
                     x->boundary = 1 ;
                  }
                }
                fprintf(fp,"   %s_boundary_table( idomain, P_%s ) = %s\n",assoc_4d,c, (x->boundary==1)?".TRUE.":".FALSE." ) ;
                fprintf(fp,"   %s_dname_table( idomain, P_%s ) = '%s'\n",assoc_4d,c,x->dname) ;
                fprintf(fp,"   %s_desc_table( idomain, P_%s ) = '%s'\n",assoc_4d,c,x->descrip) ;
                fprintf(fp,"   %s_units_table( idomain, P_%s ) = '%s'\n",assoc_4d,c,x->units) ;


                for ( i = 0 ; i < IO_MASK_SIZE ; i++ ) {
                  fprintf(fp,"   %s_streams_table( idomain, P_%s )%%stream(%d) = %d ! %08x \n",assoc_4d,c,
                                                                          i+1,x->io_mask[i],x->io_mask[i] ) ;
                }

                fprintf(fp,"   F_%s = .TRUE.\n",c) ;
              } else if ((p = get_entry_r( c , NULL, Domain.fields )) != NULL ) {
                int tag, fo  ;
                for ( tag = 1 ; tag <= p->ntl ; tag++ )
                  {
                  if ( !strcmp ( p->use , "_4d_bdy_array_") ) {
                    strcpy(fname,p->name) ;
                  } else {
                    strcpy(fname,field_name(t4,p,(p->ntl>1)?tag:0)) ;
                  }
                  if ( strchr (c, '%') != NULLCHARPTR ) {
                    strcpy(fname2,c) ;
                  } else {
                    sprintf(tmp1,"%s_tend",p->name) ;
                    sprintf(tmp2,"%s_old",p->name) ;
                    if ( !strcmp(c, tmp1) ) {
                      strcpy(fname2,tmp1) ;
                    } else if ( !strcmp(c, tmp2) ) {
                      strcpy(fname2,tmp2) ;
                    } else {
                      strcpy(fname2,fname) ;
                    }
                  }

                  make_lower_case(fname2)  ;

                  fo = fname2[0]-'a' ;

                  fprintf(fp2[fo],"IF(TRIM(vname).EQ.'%s')THEN\n",fname2) ;
                  fprintf(fp2[fo],"  IF(uses.EQ.0)THEN\n");
                  fprintf(fp2[fo],"    in_use = model_config_rec%%%s%s.EQ.%s\n",assoc_namelist_var,(atoi(rconfig->nentries)!=1)?"(id)":"",assoc_namelist_choice) ;
                  fprintf(fp2[fo],"    uses = 1\n") ;
                  fprintf(fp2[fo],"  ELSE\n") ;
                  fprintf(fp2[fo],"    in_use = in_use.OR.model_config_rec%%%s%s.EQ.%s\n",assoc_namelist_var,(atoi(rconfig->nentries)!=1)?"(id)":"",assoc_namelist_choice) ;
                  fprintf(fp2[fo],"  ENDIF\n") ;
                  fprintf(fp2[fo],"ENDIF\n") ;

                }
              } else {
                fprintf(stderr, "WARNING: %s is not a member of 4D array %s\n",c,assoc_4d);continue;
              }
            }
          } else {
            fprintf(stderr, "WARNING: There is no 4D array named %s\n",assoc_4d);continue ;
          }
        }

        scalars = strtok_rentr(NULL,";", &pos1) ;

      }
    }

    fprintf(fp,"  END IF\n") ;
  }

  return(0) ;
}


