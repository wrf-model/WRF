#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "protos.h"
#include "registry.h"
#include "data.h"

/* For detecting variables that are members of a derived type */
#define NULLCHARPTR   (char *) 0
static int parent_type;

int
gen_halos ( char * dirname )
{
  node_t * p, * q ;
  node_t * dimd ;
  char commname[NAMELEN] ;
  char fname[NAMELEN] ;
  char tmp[4096], tmp2[4096], tmp3[4096] ;
  char commuse[4096] ;
  int maxstenwidth, stenwidth ;
  FILE * fp ;
  char * t1, * t2 ;
  char * pos1 , * pos2 ;
  char indices[NAMELEN], post[NAMELEN] ;
  int zdex ;
  int n2dR, n3dR ;
  int n2dI, n3dI ;
  int n2dD, n3dD ;
  int n4d ;
  int i ;
#define MAX_4DARRAYS 1000
  char name_4d[MAX_4DARRAYS][NAMELEN] ;

  if ( dirname == NULL ) return(1) ;

  for ( p = Halos ; p != NULL ; p = p->next )
  {
    strcpy( commname, p->name ) ;
    make_upper_case(commname) ;
    if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s.inc",dirname,commname) ; }
    else                       { sprintf(fname,"%s.inc",commname) ; }
    if ((fp = fopen( fname , "w" )) == NULL ) 
    {
      fprintf(stderr,"WARNING: gen_halos in registry cannot open %s for writing\n",fname ) ;
      continue ; 
    }
    /* get maximum stencil width */
    maxstenwidth = 0 ;
    strcpy( tmp, p->comm_define ) ;
    t1 = strtok_rentr( tmp , ";" , &pos1 ) ;
    while ( t1 != NULL )
    {
      strcpy( tmp2 , t1 ) ;
      if (( t2 = strtok_rentr( tmp2 , ":" , &pos2 )) == NULL )
       { fprintf(stderr,"unparseable description for halo %s\n", commname ) ; exit(1) ; }
      stenwidth = atoi (t2) ;
      if ( stenwidth == 0 )
       { fprintf(stderr,"* unparseable description for halo %s\n", commname ) ; exit(1) ; }
      if      ( stenwidth == 4   || stenwidth == 8  ) stenwidth = 1 ;
      else if ( stenwidth == 12  || stenwidth == 24 ) stenwidth = 2 ;
      else if ( stenwidth == 48 ) stenwidth = 3 ;
      else if ( stenwidth == 80 ) stenwidth = 4 ;
      else if ( stenwidth == 120 ) stenwidth = 5 ;
      else if ( stenwidth == 168 ) stenwidth = 6 ;
      else
       { fprintf(stderr,"%s: unknown stenci description or just too big: %d\n", commname, stenwidth ) ; exit(1) ; }
      if ( stenwidth > maxstenwidth ) maxstenwidth = stenwidth ;
      t1 = strtok_rentr( NULL , ";" , &pos1 ) ;
    }
    print_warning(fp,fname) ;

fprintf(fp,"CALL wrf_debug(2,'calling %s')\n",fname) ;

/* count up the number of 2d and 3d real arrays and their types */
    n2dR = 0 ; n3dR = 0 ;
    n2dI = 0 ; n3dI = 0 ;
    n2dD = 0 ; n3dD = 0 ;
    n4d = 0 ;
    strcpy( tmp, p->comm_define ) ;
    strcpy( commuse, p->use ) ;
    t1 = strtok_rentr( tmp , ";" , &pos1 ) ;
    for ( i = 0 ; i < MAX_4DARRAYS ; i++ ) strcpy(name_4d[i],"") ;  /* truncate all of these */
    while ( t1 != NULL )
    {
      strcpy( tmp2 , t1 ) ;
      if (( t2 = strtok_rentr( tmp2 , ":" , &pos2 )) == NULL )
       { fprintf(stderr,"unparseable description for halo %s\n", commname ) ; continue ; }
      t2 = strtok_rentr(NULL,",", &pos2) ;
      while ( t2 != NULL )
      {
        if ((q = get_entry_r( t2, commuse, Domain.fields )) == NULL )
          { fprintf(stderr,"WARNING 1 : %s in halo spec %s (%s) is not defined in registry.\n",t2,commname, commuse) ; }
        else
        {
          if      (  strcmp( q->type->name, "real") && strcmp( q->type->name, "integer") && strcmp( q->type->name, "doubleprecision") )
            { fprintf(stderr,"WARNING: only type 'real', 'doubleprecision', or 'integer' can be part of halo exchange. %s in %s is %s\n",t2,commname,q->type->name) ; }
          else if ( q->boundary_array )
            { fprintf(stderr,"WARNING: boundary array %s cannot be member of halo spec %s.\n",t2,commname) ; }
          else
          {
            if ( q->node_kind & FOURD ) {
#if 1
              if ( n4d < MAX_4DARRAYS ) {
                strcpy( name_4d[n4d], q->name ) ;
              } else { 
                fprintf(stderr,"REGISTRY ERROR: too many 4d arrays (> %d).\n", MAX_4DARRAYS ) ;
                fprintf(stderr,"That seems like a lot, but if you are sure, increase MAX_4DARRAYS\n" ) ;
                fprintf(stderr,"in external/RSL_LITE/gen_comms.c and recompile\n") ;
                exit(5) ;
              }
              n4d++ ;
#else
              node_t *member ;
              zdex = get_index_for_coord( q , COORD_Z ) ;
              if ( zdex >=1 && zdex <= 3 )
              {
                for ( member = q->members ; member != NULL ; member = member->next )
                {
                  if ( strcmp( member->name, "-" ) )
                  {
                    if        ( ! strcmp( q->type->name, "real") ) n3dR++ ;
                    else if   ( ! strcmp( q->type->name, "integer") ) n3dI++ ;
                    else if   ( ! strcmp( q->type->name, "doubleprecision") ) n3dD++ ;
                  }
                }
              }
              else
              {
                fprintf(stderr,"WARNING: %d some dimension info missing for 4d array %s\n",zdex,t2) ;
              }
#endif
            }
            else
            {
              if        ( ! strcmp( q->type->name, "real") ) {
                if         ( q->ndims == 3 )      n3dR++ ;
	        else    if ( q->ndims == 2 )      n2dR++ ;
	      } else if ( ! strcmp( q->type->name, "integer") ) {
                if         ( q->ndims == 3 )      n3dI++ ;
	        else    if ( q->ndims == 2 )      n2dI++ ;
	      } else if ( ! strcmp( q->type->name, "doubleprecision") ) {
                if         ( q->ndims == 3 )      n3dD++ ;
	        else    if ( q->ndims == 2 )      n2dD++ ;
	      }
	    }
	  }
	}
        t2 = strtok_rentr( NULL , "," , &pos2 ) ;
      }
      t1 = strtok_rentr( NULL , ";" , &pos1 ) ;
    }

/* generate the stencil init statement for Y transfer */
#if 0
fprintf(fp,"CALL wrf_debug(3,'calling RSL_LITE_INIT_EXCH %d for Y %s')\n",maxstenwidth,fname) ;
#endif
    fprintf(fp,"CALL RSL_LITE_INIT_EXCH ( %d , &\n",maxstenwidth) ;
    if ( n4d > 0 ) {
      fprintf(fp,  "     %d  &\n", n3dR ) ;
      for ( i = 0 ; i < n4d ; i++ ) {
	fprintf(fp,"   + num_%s   &\n", name_4d[i] ) ;
      }
      fprintf(fp,"     , %d, RWORDSIZE, &\n", n2dR ) ;
    } else {
      fprintf(fp,"     %d, %d, RWORDSIZE, &\n", n3dR, n2dR ) ;
    }
    fprintf(fp,"     %d, %d, IWORDSIZE, &\n", n3dI, n2dI ) ;
    fprintf(fp,"     %d, %d, DWORDSIZE, &\n", n3dD, n2dD ) ;
    fprintf(fp,"      0,  0, LWORDSIZE, &\n" ) ;
    fprintf(fp,"      mytask, ntasks, ntasks_x, ntasks_y,   &\n" ) ;
    fprintf(fp,"      ips, ipe, jps, jpe, kps, kpe    )\n") ;

/* generate packs prior to stencil exchange in Y */
    gen_packs( fp, p, maxstenwidth, 0, 0 ) ;
/* generate stencil exchange in Y */
#if 0
fprintf(fp,"CALL wrf_debug('calling RSL_LITE_EXCH_Y')\n") ;
#endif
    fprintf(fp,"   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )\n") ;
#if 0
fprintf(fp,"CALL wrf_debug('back from RSL_LITE_EXCH_Y')\n") ;
#endif
/* generate unpacks after stencil exchange in Y */
    gen_packs( fp, p, maxstenwidth, 0, 1 ) ;

/* generate the stencil init statement for X transfer */
#if 0
fprintf(fp,"CALL wrf_debug(3,'calling RSL_LITE_INIT_EXCH %d for X %s')\n",maxstenwidth,fname) ;
#endif
    fprintf(fp,"CALL RSL_LITE_INIT_EXCH ( %d , &\n",maxstenwidth) ;
    fprintf(fp,"     %d, %d, RWORDSIZE, &\n", n3dR, n2dR ) ;
    fprintf(fp,"     %d, %d, IWORDSIZE, &\n", n3dI, n2dI ) ;
    fprintf(fp,"     %d, %d, DWORDSIZE, &\n", n3dD, n2dD ) ;
    fprintf(fp,"      0,  0, LWORDSIZE, &\n" ) ;
    fprintf(fp,"      mytask, ntasks, ntasks_x, ntasks_y,   &\n" ) ;
    fprintf(fp,"      ips, ipe, jps, jpe, kps, kpe    )\n") ;
/* generate packs prior to stencil exchange in X */
    gen_packs( fp, p, maxstenwidth, 1, 0 ) ;
/* generate stencil exchange in X */
#if 0
fprintf(fp,"CALL wrf_debug('calling RSL_LITE_EXCH_X')\n") ;
#endif
    fprintf(fp,"   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )\n") ;
#if 0
fprintf(fp,"CALL wrf_debug('back from RSL_LITE_EXCH_X')\n") ;
#endif
/* generate unpacks after stencil exchange in X */
    gen_packs( fp, p, maxstenwidth, 1, 1 ) ;

#if 0
fprintf(fp,"CALL wrf_debug(2,'back from %s')\n",fname) ;
#endif

    close_the_file(fp) ;
  }
  return(0) ;
}

gen_packs ( FILE *fp , node_t *p, int shw, int xy /* 0=y,1=x */ , int pu /* 0=pack,1=unpack */ )   
{
  node_t * q ;
  node_t * dimd ;
  char commname[NAMELEN] ;
  char fname[NAMELEN] ;
  char tmp[4096], tmp2[4096], tmp3[4096] ;
  char commuse[4096] ;
  int maxstenwidth, stenwidth ;
  char * t1, * t2 , *wordsize ;
  char * pos1 , * pos2 ;
  char indices[NAMELEN], post[NAMELEN], memord[NAMELEN] ;
  int zdex ;

    strcpy( tmp, p->comm_define ) ;
    strcpy( commuse, p->use ) ;
    t1 = strtok_rentr( tmp , ";" , &pos1 ) ;
    while ( t1 != NULL )
    {
      strcpy( tmp2 , t1 ) ;
      if (( t2 = strtok_rentr( tmp2 , ":" , &pos2 )) == NULL )
       { fprintf(stderr,"unparseable description for halo %s\n", commname ) ; continue ; }
      t2 = strtok_rentr(NULL,",", &pos2) ;
      while ( t2 != NULL )
      {
        if ((q = get_entry_r( t2, commuse, Domain.fields )) == NULL )
          { fprintf(stderr,"WARNING 1 : %s in halo spec %s (%s) is not defined in registry.\n",t2,commname, commuse) ; }
        else
        {
          if      (  strcmp( q->type->name, "real") && strcmp( q->type->name, "integer") && strcmp( q->type->name, "doubleprecision") ) { ; }
          else if ( q->boundary_array ) { ; }
          else
          { 
            if      ( ! strcmp( q->type->name, "real") )            { wordsize = "RWORDSIZE" ; }
            else if ( ! strcmp( q->type->name, "integer") )         { wordsize = "IWORDSIZE" ; }
            else if ( ! strcmp( q->type->name, "doubleprecision") ) { wordsize = "DWORDSIZE" ; }
            if ( q->node_kind & FOURD )
            {
              node_t *member ;
              zdex = get_index_for_coord( q , COORD_Z ) ;
              if ( zdex >=1 && zdex <= 3 )
              {
                for ( member = q->members ; member != NULL ; member = member->next )
                {
                  if ( strcmp( member->name, "-" ) )
                  {
                    set_mem_order( member, memord , NAMELEN) ;

#if 0
fprintf(fp,"if ( P_%s .GT. 1 )CALL wrf_debug(3,'call RSL_LITE_PACK P_%s %s shw=%d ws=%s xy=%d pu=%d m=%s')\n",member->name,member->name,t2,shw,wordsize,xy,pu,memord) ;
fprintf(fp,"if ( P_%s .GT. 1 )write(wrf_err_message,*)' d ',ids, ide, jds, jde, kds, kde\n",member->name ) ;
fprintf(fp,"if ( P_%s .GT. 1 )CALL wrf_debug(3,wrf_err_message)\n",member->name) ;
fprintf(fp,"if ( P_%s .GT. 1 )write(wrf_err_message,*)' m ',ims, ime, jms, jme, kms, kme\n",member->name ) ;
fprintf(fp,"if ( P_%s .GT. 1 )CALL wrf_debug(3,wrf_err_message)\n",member->name) ;
fprintf(fp,"if ( P_%s .GT. 1 )write(wrf_err_message,*)' p ',ips, ipe, jps, jpe, kps, kpe\n",member->name ) ;
fprintf(fp,"if ( P_%s .GT. 1 )CALL wrf_debug(3,wrf_err_message)\n",member->name) ;
#endif

fprintf(fp,"if ( P_%s .GT. 1 ) CALL RSL_LITE_PACK ( %s ( grid%%sm31,grid%%sm32,grid%%sm33,P_%s), %d, %s, %d, %d, '%s', &\n",
                       member->name, t2 , member->name, shw, wordsize, xy, pu, memord ) ;
                    fprintf(fp,"mytask, ntasks, ntasks_x, ntasks_y,       &\n") ;
                    fprintf(fp,"ids, ide, jds, jde, kds, kde,             &\n") ;
                    fprintf(fp,"ims, ime, jms, jme, kms, kme,             &\n") ;
                    fprintf(fp,"ips, ipe, jps, jpe, kps, kpe              )\n") ;
#if 0
fprintf(fp,"if ( P_%s .GT. 1 )CALL wrf_debug(3,'back from RSL_LITE_PACK')\n",member->name) ;
#endif
                  }
                }
              }
              else
              {
                fprintf(stderr,"WARNING: %d some dimension info missing for 4d array %s\n",zdex,t2) ;
              }
            }
            else
            {
              set_mem_order( q, memord , NAMELEN) ;
#if 0
fprintf(fp,"CALL wrf_debug(3,'call RSL_LITE_PACK %s shw=%d ws=%s xy=%d pu=%d m=%s')\n",t2,shw,wordsize,xy,pu,memord) ;
fprintf(fp,"write(wrf_err_message,*)' d ',ids, ide, jds, jde, kds, kde\n" ) ;
fprintf(fp,"CALL wrf_debug(3,wrf_err_message)\n") ;
fprintf(fp,"write(wrf_err_message,*)' m ',ims, ime, jms, jme, kms, kme\n" ) ;
fprintf(fp,"CALL wrf_debug(3,wrf_err_message)\n") ;
fprintf(fp,"write(wrf_err_message,*)' p ',ips, ipe, jps, jpe, kps, kpe\n" ) ;
fprintf(fp,"CALL wrf_debug(3,wrf_err_message)\n") ;
#endif
              if       ( q->ndims == 3 ) {

                dimd = get_dimnode_for_coord( q , COORD_Z ) ;
                zdex = get_index_for_coord( q , COORD_Z ) ;
                if ( dimd != NULL )
                {
                  char s[256], e[256] ;

                  if      ( dimd->len_defined_how == DOMAIN_STANDARD ) {
#if 0
fprintf(fp,"write(wrf_err_message,*)' d ',ids, ide, jds, jde, kds, kde\n" ) ;
fprintf(fp,"CALL wrf_debug(3,wrf_err_message)\n") ;
fprintf(fp,"write(wrf_err_message,*)' m ',ims, ime, jms, jme, kms, kme\n" ) ;
fprintf(fp,"CALL wrf_debug(3,wrf_err_message)\n") ;
fprintf(fp,"write(wrf_err_message,*)' p ',ips, ipe, jps, jpe, kps, kpe\n" ) ;
fprintf(fp,"CALL wrf_debug(3,wrf_err_message)\n") ;
#endif
                    fprintf(fp,"CALL RSL_LITE_PACK ( %s, %d, %s, %d, %d, '%s', &\n", t2, shw, wordsize, xy, pu, memord ) ;
                    fprintf(fp,"mytask, ntasks, ntasks_x, ntasks_y,       &\n") ;
                    fprintf(fp,"ids, ide, jds, jde, kds, kde,             &\n") ;
                    fprintf(fp,"ims, ime, jms, jme, kms, kme,             &\n") ;
                    fprintf(fp,"ips, ipe, jps, jpe, kps, kpe              )\n") ;
                  }
                  else if ( dimd->len_defined_how == NAMELIST )
                  {
                    if ( !strcmp(dimd->assoc_nl_var_s,"1") ) {
                      strcpy(s,"1") ;
                      sprintf(e,"config_flags%%%s",dimd->assoc_nl_var_e) ;
                    } else {
                      sprintf(s,"config_flags%%%s",dimd->assoc_nl_var_s) ;
                      sprintf(e,"config_flags%%%s",dimd->assoc_nl_var_e) ;
                    }
#if 0
fprintf(fp,"write(wrf_err_message,*)' d ',ids, ide, jds, jde, %s, %s\n",s,e ) ;
fprintf(fp,"CALL wrf_debug(3,wrf_err_message)\n") ;
fprintf(fp,"write(wrf_err_message,*)' m ',ims, ime, jms, jme, %s, %s\n",s,e ) ;
fprintf(fp,"CALL wrf_debug(3,wrf_err_message)\n") ;
fprintf(fp,"write(wrf_err_message,*)' p ',ips, ipe, jps, jpe, %s, %s\n",s,e ) ;
fprintf(fp,"CALL wrf_debug(3,wrf_err_message)\n") ;
#endif
                    fprintf(fp,"CALL RSL_LITE_PACK ( %s, %d, %s, %d, %d, '%s', &\n", t2, shw, wordsize, xy, pu, memord ) ;
                    fprintf(fp,"mytask, ntasks, ntasks_x, ntasks_y,       &\n") ;
                    fprintf(fp,"ids, ide, jds, jde, %s, %s,             &\n",s,e) ;
                    fprintf(fp,"ims, ime, jms, jme, %s, %s,             &\n",s,e) ;
                    fprintf(fp,"ips, ipe, jps, jpe, %s, %s              )\n",s,e) ;
                  }
                  else if ( dimd->len_defined_how == CONSTANT )
                  {
#if 0
fprintf(fp,"write(wrf_err_message,*)' d ',ids, ide, jds, jde, %d, %d\n",dimd->coord_start,dimd->coord_end ) ;
fprintf(fp,"CALL wrf_debug(3,wrf_err_message)\n") ;
fprintf(fp,"write(wrf_err_message,*)' m ',ims, ime, jms, jme, %d, %d\n",dimd->coord_start,dimd->coord_end ) ;
fprintf(fp,"CALL wrf_debug(3,wrf_err_message)\n") ;
fprintf(fp,"write(wrf_err_message,*)' p ',ips, ipe, jps, jpe, %d, %d\n",dimd->coord_start,dimd->coord_end ) ;
fprintf(fp,"CALL wrf_debug(3,wrf_err_message)\n") ;
#endif
                    fprintf(fp,"CALL RSL_LITE_PACK ( %s, %d, %s, %d, %d, '%s', &\n", t2, shw, wordsize, xy, pu, memord ) ;
                    fprintf(fp,"mytask, ntasks, ntasks_x, ntasks_y,       &\n") ;
                    fprintf(fp,"ids, ide, jds, jde, %d, %d,             &\n",dimd->coord_start,dimd->coord_end) ;
                    fprintf(fp,"ims, ime, jms, jme, %d, %d,             &\n",dimd->coord_start,dimd->coord_end) ;
                    fprintf(fp,"ips, ipe, jps, jpe, %d, %d              )\n",dimd->coord_start,dimd->coord_end) ;
                  }
                }
              } else {
#if 0
fprintf(fp,"write(wrf_err_message,*)' d ',ids, ide, jds, jde, 1, 1\n" ) ;
fprintf(fp,"CALL wrf_debug(3,wrf_err_message)\n") ;
fprintf(fp,"write(wrf_err_message,*)' m ',ims, ime, jms, jme, 1, 1\n" ) ;
fprintf(fp,"CALL wrf_debug(3,wrf_err_message)\n") ;
fprintf(fp,"write(wrf_err_message,*)' p ',ips, ipe, jps, jpe, 1, 1\n" ) ;
fprintf(fp,"CALL wrf_debug(3,wrf_err_message)\n") ;
#endif
                fprintf(fp,"CALL RSL_LITE_PACK ( %s, %d, %s, %d, %d, '%s', &\n", t2, shw, wordsize, xy, pu, memord ) ;
                fprintf(fp,"mytask, ntasks, ntasks_x, ntasks_y,       &\n") ;
                fprintf(fp,"ids, ide, jds, jde, 1  , 1  ,             &\n") ;
                fprintf(fp,"ims, ime, jms, jme, 1  , 1  ,             &\n") ;
                fprintf(fp,"ips, ipe, jps, jpe, 1  , 1                )\n") ;
              }
#if 0
fprintf(fp,"CALL wrf_debug(3,'back from RSL_LITE_PACK')\n") ;
#endif
            }
          }
          
        }
        t2 = strtok_rentr( NULL , "," , &pos2 ) ;
      }
      t1 = strtok_rentr( NULL , ";" , &pos1 ) ;
    }
}

int
gen_periods ( char * dirname )
{
  node_t * p, * q ;
  char commname[NAMELEN] ;
  char fname[NAMELEN] ;
  char tmp[4096], tmp2[4096], commuse[4096] ;
  int maxperwidth, perwidth ;
  FILE * fp ;
  char * t1, * t2 ;
  char * pos1 , * pos2 ;

  if ( dirname == NULL ) return(1) ;

  for ( p = Periods ; p != NULL ; p = p->next )
  {
    strcpy( commname, p->name ) ;
    make_upper_case(commname) ;
    if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s.inc",dirname,commname) ; }
    else                       { sprintf(fname,"%s.inc",commname) ; }
    if ((fp = fopen( fname , "w" )) == NULL ) 
    {
      fprintf(stderr,"WARNING: gen_periods in registry cannot open %s for writing\n",fname ) ;
      continue ; 
    }
    close_the_file(fp) ;
  }
  return(0) ;
}

int
gen_xposes ( char * dirname )
{
  node_t * p, * q ;
  char commname[NAMELEN] ;
  char fname[NAMELEN] ;
  char tmp[4096], tmp2[4096], tmp3[4096] ;
  char commuse[4096] ;
  FILE * fp ;
  char * t1, * t2 ;
  char * pos1 , * pos2 ;
  char *xposedir[] = { "z2x" , "x2z" , "x2y" , "y2x" , "z2y" , "y2z" , 0L } ;
  char ** x ;
  char indices[NAMELEN], post[NAMELEN] ;

  if ( dirname == NULL ) return(1) ;

  for ( p = Xposes ; p != NULL ; p = p->next )
  {
    for ( x = xposedir ; *x ; x++ )
    {
      strcpy( commname, p->name ) ;
      make_upper_case(commname) ;
      if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s_%s.inc",dirname,commname, *x) ; }
      else                       { sprintf(fname,"%s_%s.inc",commname,*x) ; }
      if ((fp = fopen( fname , "w" )) == NULL ) 
      {
        fprintf(stderr,"WARNING: gen_halos in registry cannot open %s for writing\n",fname ) ;
        continue ; 
      }

      print_warning(fp,fname) ;
      close_the_file(fp) ;
    }
skiperific:
    ;
  }
  return(0) ;
}

int
gen_comm_descrips ( char * dirname )
{
  node_t * p ;
  char * fn = "dm_comm_cpp_flags" ;
  char commname[NAMELEN] ;
  char fname[NAMELEN] ;
  FILE * fp ;
  int ncomm ;

  if ( dirname == NULL ) return(1) ;

  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }

  if ((fp = fopen( fname , "w" )) == NULL )
  {
    fprintf(stderr,"WARNING: gen_comm_descrips in registry cannot open %s for writing\n",fname ) ;
  }

  return(0) ;
}

/*



*/

/* for each core, generate the halo updates to allow shifting all state data */
int
gen_shift (  char * dirname )
{
  int i, ncore ;
  FILE * fp ;
  node_t *p, *q, *dimd ;
  char * corename ;
  char **direction ;
  char *directions[] = { "x", "y", 0L } ;
  char fname[NAMELEN], vname[NAMELEN] ;
  char indices[NAMELEN], post[NAMELEN], tmp3[NAMELEN] ;
  int zdex ;

  for ( direction = directions ; *direction != NULL ; direction++ )
  {
  for ( ncore = 0 ; ncore < get_num_cores() ; ncore++ )
  {
    corename = get_corename_i(ncore) ;
    if ( dirname == NULL || corename == NULL ) return(1) ;
    if ( strlen(dirname) > 0 )
     { sprintf(fname,"%s/%s_shift_halo_%s.inc",dirname,corename,*direction) ; }
    else
     { sprintf(fname,"%s_shift_halo_%s.inc",corename,*direction) ; }
    if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
    print_warning(fp,fname) ;
    close_the_file(fp) ;
  }
  }
}

int
gen_datacalls ( char * dirname )
{
  int i ;
  FILE * fp ;
  char * corename ;
  char * fn = "data_calls.inc" ;
  char fname[NAMELEN] ;

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
    close_the_file(fp) ;
  }
  return(0) ;
}

int
gen_datacalls1 ( FILE * fp , char * corename , char * structname , int mask , node_t * node )
{
  node_t * p, * q  ;
  int i, member_number ;
  char tmp[NAMELEN] ;
  char indices[NAMELEN], post[NAMELEN] ;

  for ( p = node ; p != NULL ; p = p->next )
  {
    if ( ( mask & p->node_kind ) &&
        ((!strncmp(p->use,"dyn_",4) && !strcmp(corename,p->use+4)) || strncmp(p->use,"dyn_",4)))
    {
    if ( (p->subject_to_communication == 1) || ( p->type->type_type == DERIVED ) )
    {
      if ( p->type->type_type == SIMPLE )
      {
        for ( i = 1 ; i <= p->ntl ; i++ )
        {
/* IF (P_QI .ge. P_FIRST_SCALAR */
          if ( p->members != NULL )   /* a 4d array */
          {
            member_number = 0 ;
            for ( q = p->members ; q != NULL ; q = q->next )
            {
              sprintf(tmp, "(grid%%sm31,grid%%sm32,grid%%sm33,1+%d)", member_number ) ;
              if ( p->ntl > 1 ) fprintf(fp," IF(1+%d.LE.num_%s)CALL rsl_register_f90 ( %s%s_%d %s )\n",
                                             member_number,p->name,structname,p->name,i,tmp) ;
              else              fprintf(fp," CALL rsl_register_f90 ( %s%s %s )\n"   ,structname,p->name,tmp) ;
              member_number++ ;
            }
          }
          else
          {
            strcpy (indices,"");
            if ( sw_deref_kludge && parent_type == DERIVED ) 
            {
              sprintf(post,")") ;
              sprintf(indices, "%s",index_with_firstelem("(","",tmp,p,post)) ;
            }
            if ( p->ntl > 1 ) fprintf(fp," CALL rsl_register_f90 ( %s%s_%d%s )\n",structname,p->name,i,indices) ;
            else              fprintf(fp," CALL rsl_register_f90 ( %s%s%s )\n"   ,structname,p->name,indices) ;
          }
        }
      }
      else if ( p->type->type_type == DERIVED )
      {
        parent_type = DERIVED;
        sprintf( tmp , "%s%%", p->name ) ; 
        gen_datacalls1 ( fp , corename , tmp , mask, p->type->fields ) ;
      }
    }
  }
  }
  return(0) ;
}

/*****************/
/*****************/

gen_nest_packing ( char * dirname )
{
  gen_nest_pack( dirname ) ;   
  gen_nest_unpack( dirname ) ; 
}

#define PACKIT 1
#define UNPACKIT 2

int
gen_nest_pack ( char * dirname )
{
  int i ;
  FILE * fp ;
  char * corename ;
  char * fnlst[] = { "nest_interpdown_pack.inc" , "nest_forcedown_pack.inc" , "nest_feedbackup_pack.inc", 0L } ;
  int down_path[] = { INTERP_DOWN , FORCE_DOWN , INTERP_UP } ;
  int ipath ;
  char ** fnp ; char * fn ;
  char fname[NAMELEN] ;
  node_t *node, *p, *dim ;
  int xdex, ydex, zdex ;
  char ddim[3][2][NAMELEN] ;
  char mdim[3][2][NAMELEN] ;
  char pdim[3][2][NAMELEN] ;
  char vname[NAMELEN] ; char tag[NAMELEN] ; char core[NAMELEN] ;
  int d2, d3 ;

  for ( fnp = fnlst , ipath = 0 ; *fnp ; fnp++ , ipath++ )
  {
    fn = *fnp ;
    for ( i = 0 ; i < get_num_cores() ; i++ )
    {
      corename = get_corename_i(i) ;
      if ( dirname == NULL || corename == NULL ) return(1) ;
      if ( strlen(dirname) > 0 ) {
       if ( strlen( corename ) > 0 )
         { sprintf(fname,"%s/%s_%s",dirname,corename,fn) ; }
       else
         { sprintf(fname,"%s/%s",dirname,fn) ; }
      } else { 
       if ( strlen( corename ) > 0 ) 
          { sprintf(fname,"%s_%s",corename,fn) ; }
       else
          { sprintf(fname,"%s",fn) ; }
      }
      if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
      print_warning(fp,fname) ;
      close_the_file(fp) ;
    }
  }
  return(0) ;
}

int
gen_nest_unpack ( char * dirname )
{
  int i ;
  FILE * fp ;
  char * corename ;
  char * fnlst[] = { "nest_interpdown_unpack.inc" , "nest_forcedown_unpack.inc" , "nest_feedbackup_unpack.inc" , 0L } ;
  int down_path[] = { INTERP_DOWN , FORCE_DOWN , INTERP_UP } ;
  int ipath ;
  char ** fnp ; char * fn ;
  char fname[NAMELEN] ;
  node_t *node, *p, *dim ;
  int xdex, ydex, zdex ;
  char ddim[3][2][NAMELEN] ;
  char mdim[3][2][NAMELEN] ;
  char pdim[3][2][NAMELEN] ;
  char vname[NAMELEN] ; char tag[NAMELEN] ; char core[NAMELEN] ;
  int d2, d3 ;

  for ( fnp = fnlst , ipath = 0 ; *fnp ; fnp++ , ipath++ )
  {
    fn = *fnp ;
    for ( i = 0 ; i < get_num_cores() ; i++ )
    {
      d2 = 0 ;
      d3 = 0 ;
      node = Domain.fields ;

      corename = get_corename_i(i) ;
      if ( dirname == NULL || corename == NULL ) return(1) ;
      if ( strlen(dirname) > 0 )
       { sprintf(fname,"%s/%s_%s",dirname,corename,fn) ; }
      else
       { sprintf(fname,"%s_%s",corename,fn) ; }
      if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
      print_warning(fp,fname) ;
      close_the_file(fp) ;
    }
  }
  return(0) ;
}

/*****************/

int
count_fields ( node_t * node , int * d2 , int * d3 , char * corename , int down_path )
{
  node_t * p ;
  int zdex ;
/* count up the total number of levels from all fields */
  for ( p = node ;  p != NULL ; p = p->next )
  {
    if ( p->node_kind == FOURD ) 
    {
      count_fields( p->members , d2 , d3 , corename , down_path ) ;  /* RECURSE */
    }
    else
    {
      if ( p->io_mask & down_path )
      {
        if ((!strncmp( p->use, "dyn_", 4) && !strcmp(p->use+4,corename)) || strncmp( p->use, "dyn_", 4))
        {
          if ( p->node_kind == FOURD )
            zdex = get_index_for_coord( p->members , COORD_Z ) ;
          else
            zdex = get_index_for_coord( p , COORD_Z ) ;
  
          if ( zdex < 0 ) {
            (*d2)++ ;   /* if no zdex then only 2 d */
          } else {
            (*d3)++ ;   /* if has a zdex then 3 d */
          }
        }
      }
    }
  }
  return(0) ;
}

/*****************/

int
gen_comms ( char * dirname )
{
  if ( sw_dm_parallel )
    fprintf(stderr,"ADVISORY: RSL version of gen_comms is linked in with registry program.\n") ;

  gen_halos( "inc" ) ;
  gen_shift( "inc" ) ;
  gen_periods( "inc" ) ;
  gen_xposes( "inc" ) ;
  gen_comm_descrips( "inc" ) ;
  gen_datacalls( "inc" ) ;
  gen_nest_packing( "inc" ) ;

  return(0) ;
}

