#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#define index(X,Y) strchr(X,Y)
#endif

#include "protos.h"
#include "registry.h"
#include "data.h"

/* For detecting variables that are members of a derived type */
#define NULLCHARPTR   (char *) 0
static int parent_type;

/* print actual and dummy arguments and declarations for 4D and i1 arrays */
int print_4d_i1_decls ( FILE *fp , node_t *p, int ad /* 0=argument,1=declaration */, int du /* 0=dummy,1=actual */)   
{
  node_t * q ;
  node_t * dimd ;
  char fname[NAMELEN] ;
  char tmp[NAMELEN_LONG], tmp2[NAMELEN_LONG], tmp3[NAMELEN_LONG] ;
  char commuse[NAMELEN] ;
  int maxstenwidth, stenwidth ;
  char * t1, * t2 , *wordsize ;
  char varref[NAMELEN], moredims[80] ;
  char * pos1 , * pos2 ;
  char * dimspec ;
  char indices[NAMELEN], post[NAMELEN], memord[NAMELEN] ;
  int zdex, d ;

    set_mark( 0, Domain.fields ) ;

    strcpy( tmp, p->comm_define ) ;
    strcpy( commuse, p->use ) ;
    t1 = strtok_rentr( tmp , ";" , &pos1 ) ;
    while ( t1 != NULL )
    {
      strcpy( tmp2 , t1 ) ;
      if (( t2 = strtok_rentr( tmp2 , ":" , &pos2 )) == NULL )
       { 
         fprintf(stderr,"unparseable description for halo %s\n", p->name ) ; continue ;
       }
      t2 = strtok_rentr(NULL,",", &pos2) ;
      while ( t2 != NULL )
      {
        if ((q = get_entry_r( t2, commuse, Domain.fields )) == NULL )
          { fprintf(stderr,"WARNING 1a : %s in halo spec %s (%s) is not defined in registry.\n",t2,p->name, commuse) ; }
        else
        {
          strcpy( varref, t2 ) ;
          if ( q->node_kind & FIELD  && ! (q->node_kind & I1) ) {
             sprintf(varref,"grid%%%s",t2) ;
          }

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
                set_mem_order( q->members, memord , 3 ) ;
                if ( ad == 0 ) 
                /* actual or dummy argument */
                {
/* explicit dummy or actual arguments for 4D arrays */
if ( q->mark == 0 ) {
  fprintf(fp,"  num_%s, &\n",q->name) ;
  for ( d = 3 ; d < q->ndims ; d++ ) {
    char *colon, r[80],tx[80] ;
    strcpy(r,"") ;
    range_of_dimension(r,tx,d,q,du?"":"config_flags%") ;
    colon = index(tx,':') ; *colon = '\0' ; 
    if ( du ) { /* dummy args */
      fprintf(fp,"%s_sdim%d,%s_edim%d, &\n",q->name,d-2,q->name,d-2) ;
    } else {
      fprintf(fp,"%s,%s,&\n",tx,colon+1) ;
    }

  }
  q->mark = 1 ;
}
fprintf(fp,"  %s, &\n",varref) ;
                }
                else
                {
/* declaration of dummy arguments for 4D arrays */
if ( q->mark == 0 ) {
  fprintf(fp,"  INTEGER, INTENT(IN) :: num_%s\n",q->name) ;
  for ( d = 3 ; d < q->ndims ; d++ ) {
    fprintf(fp,"  INTEGER, INTENT(IN) :: %s_sdim%d,%s_edim%d\n",q->name,d-2,q->name,d-2) ;
  }

  q->mark = 1 ;
}
  strcpy(moredims,"") ;
  for ( d = 3 ; d < q->ndims ; d++ ) {
    char temp[80] ;
    sprintf(temp,",%s_sdim%d:%s_edim%d",q->name,d-2,q->name,d-2) ;
    strcat(moredims,temp) ;
  }
  strcat(moredims,",") ;

              dimspec=dimension_with_ranges( "grid%","",-1,tmp3,q,"","" ) ;
fprintf(fp,"  %s, INTENT(INOUT) :: %s ( %s %snum_%s)\n",
                     q->type->name , varref , dimspec, moredims, q->name ) ;
                }
              }
              else
              {
                fprintf(stderr,"WARNING: %d some dimension info missing for 4d array %s\n",zdex,t2) ;
              }
            }
            else if ( q->node_kind & I1 )
            {
              if ( ad == 0 ) 
              {
/* explicit dummy or actual arguments for i1 arrays */
fprintf(fp,"  %s, &\n",varref) ;
              }
              else
              {
/* declaration of dummy arguments for i1 arrays */
              strcpy(tmp3,"") ;
              dimspec=dimension_with_ranges( "grid%","(",-1,tmp3,q,")","" ) ;
fprintf(fp,"  %s, INTENT(INOUT) :: %s %s\n", q->type->name , varref , dimspec ) ;
              }
            }
          }
        }
        t2 = strtok_rentr( NULL , "," , &pos2 ) ;
      }
      t1 = strtok_rentr( NULL , ";" , &pos1 ) ;
    }
    return 0; /* SamT: bug fix: return a value */
}

int print_call_or_def( FILE * fp , node_t *p, char * callorsub, 
                       char * commname, char * communicator, 
                       int need_config_flags )
  {
  fprintf(fp,"%s %s_sub ( grid, &\n",callorsub,commname) ;
  if (need_config_flags == 1)
    fprintf(fp,"  config_flags, &\n") ;
  print_4d_i1_decls( fp, p, 0, (!strcmp("CALL",callorsub))?0:1 );
  fprintf(fp,"  %s, &\n",communicator) ;
  fprintf(fp,"  mytask, ntasks, ntasks_x, ntasks_y, &\n") ;
  fprintf(fp,"  ids, ide, jds, jde, kds, kde,       &\n") ;
  fprintf(fp,"  ims, ime, jms, jme, kms, kme,       &\n") ;
  fprintf(fp,"  ips, ipe, jps, jpe, kps, kpe )\n") ;
  return(0) ;
  }

int print_decl( FILE * fp , node_t *p, char * communicator, 
                int need_config_flags )
  {
  fprintf(fp,"  USE module_domain, ONLY:domain\n") ;
  fprintf(fp,"  USE module_configure, ONLY:grid_config_rec_type,in_use_for_config\n") ;
  fprintf(fp,"  USE module_state_description, ONLY:PARAM_FIRST_SCALAR\n") ;
  fprintf(fp,"  USE module_driver_constants\n") ;
  fprintf(fp,"  TYPE(domain) ,               INTENT(IN) :: grid\n") ;
  if (need_config_flags == 1) 
    fprintf(fp,"  TYPE(grid_config_rec_type) , INTENT(IN) :: config_flags\n") ;
  print_4d_i1_decls( fp, p, 1, 0 );
  fprintf(fp,"  INTEGER ,                    INTENT(IN) :: %s\n",communicator) ;
  fprintf(fp,"  INTEGER ,                    INTENT(IN) :: mytask, ntasks, ntasks_x, ntasks_y\n") ;
  fprintf(fp,"  INTEGER ,                    INTENT(IN) :: ids, ide, jds, jde, kds, kde\n") ;
  fprintf(fp,"  INTEGER ,                    INTENT(IN) :: ims, ime, jms, jme, kms, kme\n") ;
  fprintf(fp,"  INTEGER ,                    INTENT(IN) :: ips, ipe, jps, jpe, kps, kpe\n") ;
  fprintf(fp,"  INTEGER :: itrace\n") ;
  fprintf(fp,"  INTEGER :: rsl_sendw_p, rsl_sendbeg_p, rsl_recvw_p, rsl_recvbeg_p\n") ;
  fprintf(fp,"  INTEGER :: rsl_sendw_m, rsl_sendbeg_m, rsl_recvw_m, rsl_recvbeg_m\n") ;
  fprintf(fp,"  LOGICAL, EXTERNAL :: rsl_comm_iter\n") ;
  fprintf(fp,"  INTEGER :: idim1, idim2, idim3, idim4, idim5, idim6, idim7\n") ;
  return 0; /* SamT: bug fix: return a value */
  }

int print_body( FILE * fp, char * commname )
  {
  fprintf(fp,"  \n") ;
  fprintf(fp,"CALL push_communicators_for_domain( grid%%id )\n") ;
  fprintf(fp,"#ifdef DM_PARALLEL\n") ;
  fprintf(fp,"#include \"%s_inline.inc\"\n",commname) ;
  fprintf(fp,"#endif\n") ;
  fprintf(fp,"CALL pop_communicators_for_domain\n") ;
  fprintf(fp,"  \n") ;
  fprintf(fp,"  END SUBROUTINE %s_sub\n",commname) ;
  return 0; /* SamT: bug fix: return a value */
  }

int
gen_halos ( char * dirname , char * incname , node_t * halos, int split )
{
  node_t * p, * q ;
  node_t * dimd ;
  char commname[NAMELEN], subs_fname[NAMELEN] ;
  char fname[NAMELEN], fnamecall[NAMELEN], fnamesub[NAMELEN] ;
  char tmp[NAMELEN_LONG], tmp2[NAMELEN_LONG], tmp3[NAMELEN_LONG] ;
  char commuse[NAMELEN] ;
#define MAX_VDIMS 100
  char vdims[MAX_VDIMS][2][80] ;
  char s[NAMELEN], e[NAMELEN] ;
  int vdimcurs ;
  int maxstenwidth_int, stenwidth ;
  char maxstenwidth[NAMELEN] ;
  FILE * fp ;
  FILE * fpcall ;
  FILE * fpsub ;
  char * t1, * t2 ;
  char * pos1 , * pos2 ;
  char indices[NAMELEN], post[NAMELEN] ;
  int zdex ;
  int n2dR, n3dR ;
  int n2dI, n3dI ;
  int n2dD, n3dD ;
  int n4d ;
  int i, foundvdim ;
  int subgrid ;
  int need_config_flags;
#define MAX_4DARRAYS 1000
  char name_4d[MAX_4DARRAYS][NAMELEN] ;
#define FRAC 4
  int num_halos, fraction, ihalo, j ;
  int always_interp_mp = 1;

  if ( dirname == NULL ) return(1) ;

  if ( split ) {
    for ( p = halos, num_halos=0 ; p != NULL ; p = p-> next ) {  /* howmany deez guys? */
      if ( incname == NULL ) {
        strcpy( commname, p->name ) ;
        make_upper_case(commname) ;
      }
      else {
        strcpy( commname, incname ) ;
      }
      if ( !(   !strcmp(commname,"HALO_INTERP_DOWN") || !strcmp(commname,"HALO_FORCE_DOWN"    )
             || !strcmp(commname,"HALO_INTERP_UP"  ) || !strcmp(commname,"HALO_INTERP_SMOOTH" ) ) ) {
        num_halos++ ;
      }
    } 
  } 

#if (NMM_CORE==1)
  if (    !strcmp(commname,"HALO_INTERP_DOWN")
       || !strcmp(commname,"HALO_FORCE_DOWN")
       || !strcmp(commname,"HALO_INTERP_UP") 
       || !strcmp(commname,"HALO_INTERP_SMOOTH") )
    always_interp_mp=0;
#endif

  ihalo = 0 ;
  for ( p = halos ; p != NULL ; p = p->next )
  {
    need_config_flags = 0;  /* 0 = do not need, 1 = need */
    if ( incname == NULL ) {
      strcpy( commname, p->name ) ;
      make_upper_case(commname) ;
    } 
    else {
      strcpy( commname, incname ) ;
    }
    if ( incname == NULL ) {
      if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s_inline.inc",dirname,commname) ; }
      else                       { sprintf(fname,"%s_inline.inc",commname) ; }
      /* Generate call to custom routine that encapsulates inlined comm calls */
      if ( strlen(dirname) > 0 ) { sprintf(fnamecall,"%s/%s.inc",dirname,commname) ; }
      else                       { sprintf(fnamecall,"%s.inc",commname) ; }
      if ((fpcall = fopen( fnamecall , "w" )) == NULL ) 
      {
        fprintf(stderr,"WARNING: gen_halos in registry cannot open %s for writing\n",fnamecall ) ;
        continue ; 
      }
      print_warning(fpcall,fnamecall) ;

      if (   !strcmp(commname,"HALO_INTERP_DOWN") || !strcmp(commname,"HALO_FORCE_DOWN") 
          || !strcmp(commname,"HALO_INTERP_UP"  ) || !strcmp(commname,"HALO_INTERP_SMOOTH") ) {
         sprintf(subs_fname, "REGISTRY_COMM_NESTING_DM_subs.inc" ) ;
      } else {
         if ( split ) {
           j = ihalo / ((num_halos+1)/FRAC+1) ;  /* the compiler you save may be your own */
           sprintf(subs_fname, "REGISTRY_COMM_DM_%d_subs.inc", j ) ;
           ihalo++ ;
         } else {
           sprintf(subs_fname, "REGISTRY_COMM_DM_subs.inc" ) ;
         }
      }

      /* Generate definition of custom routine that encapsulates inlined comm calls */
      if ( strlen(dirname) > 0 ) { sprintf(fnamesub,"%s/%s",dirname,subs_fname) ; }
      else                       { sprintf(fnamesub,"%s",subs_fname) ; }
      if ((fpsub = fopen( fnamesub , "a" )) == NULL ) 
      {
        fprintf(stderr,"WARNING: gen_halos in registry cannot open %s for writing\n",fnamesub ) ;
        continue ; 
      }
      print_warning(fpsub,fnamesub) ;
    }
    else {
      /* for now, retain original behavior when called from gen_shift */
      if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s.inc",dirname,commname) ; }
      else                       { sprintf(fname,"%s.inc",commname) ; }
    }
    /* Generate inlined comm calls */
    if ((fp = fopen( fname , "w" )) == NULL ) 
    {
      fprintf(stderr,"WARNING: gen_halos in registry cannot open %s for writing\n",fname ) ;
      continue ; 
    }
    /* get maximum stencil width */
    maxstenwidth_int = 0 ;
    strcpy( tmp, p->comm_define ) ;
    t1 = strtok_rentr( tmp , ";" , &pos1 ) ;
    while ( t1 != NULL )
    {
      strcpy( tmp2 , t1 ) ;
      if (( t2 = strtok_rentr( tmp2 , ":" , &pos2 )) == NULL )
       { fprintf(stderr,"unparseable description for halo %s\n", commname ) ; exit(1) ; }
if ( !strcmp(t2,"SHW") ) {
   stenwidth = -99 ;
   maxstenwidth_int = -99 ;   /* use a run-time computed stencil width based on nest ratio */
   break ;                /* note that SHW is set internally by gen_shift, it should never be used in a Registry file */
} else {
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
      if ( stenwidth > maxstenwidth_int ) maxstenwidth_int = stenwidth ;
}
      t1 = strtok_rentr( NULL , ";" , &pos1 ) ;
    }

    if ( maxstenwidth_int == -99 ) {
      sprintf(maxstenwidth,"grid%%parent_grid_ratio") ;
    } else {
      sprintf(maxstenwidth,"%d",maxstenwidth_int) ;
    }

    print_warning(fp,fname) ;

fprintf(fp,"CALL wrf_debug(2,'calling %s')\n",fname) ;

/* count up the number of 2d and 3d real arrays and their types */
    n2dR = 0 ; n3dR = 0 ;
    n2dI = 0 ; n3dI = 0 ;
    n2dD = 0 ; n3dD = 0 ;
    n4d = 0 ;
    vdimcurs = 0 ;
    subgrid = -1 ;      /* watch to make sure we don't mix subgrid fields with non-subgrid fields in same halo */
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
          if ( subgrid == -1 ) {   /* first one */
            subgrid = q->subgrid ;
          } else if ( subgrid != q->subgrid ) {
            fprintf(stderr,"SERIOUS WARNING: you are mixing subgrid fields with non-subgrid fields in halo %s\n",commname) ;
          }
          if      (  strcmp( q->type->name, "real") && strcmp( q->type->name, "integer") && strcmp( q->type->name, "doubleprecision") )
            { fprintf(stderr,"WARNING: only type 'real', 'doubleprecision', or 'integer' can be part of halo exchange. %s in %s is %s\n",t2,commname,q->type->name) ; }
          else if ( q->boundary_array )
            { fprintf(stderr,"WARNING: boundary array %s cannot be member of halo spec %s.\n",t2,commname) ; }
          else
          {

            /* 20061004 -- collect all the vertical dimensions so we can use a MAX
	       on them when calling RSL_LITE_INIT_EXCH */

            if ( q->ndims == 3 || q->node_kind & FOURD ) {
              if ((dimd = get_dimnode_for_coord( q , COORD_Z )) != NULL ) {
                zdex = get_index_for_coord( q , COORD_Z ) ;
                if      ( dimd->len_defined_how == DOMAIN_STANDARD ) { 
                  strcpy(s,"kps") ;
                  strcpy(e,"kpe") ;
                }
                else if ( dimd->len_defined_how == NAMELIST ) {
                  need_config_flags = 1;
                  if ( !strcmp(dimd->assoc_nl_var_s,"1") ) {
                    strcpy(s,"1") ;
                    sprintf(e,"config_flags%%%s",dimd->assoc_nl_var_e) ;
                  } else {
                    sprintf(s,"config_flags%%%s",dimd->assoc_nl_var_s) ;
                    sprintf(e,"config_flags%%%s",dimd->assoc_nl_var_e) ;
                  }
                }
                else if ( dimd->len_defined_how == CONSTANT ) {
                  sprintf(s,"%d",dimd->coord_start) ;
                  sprintf(e,"%d",dimd->coord_end) ; 
                }
                for ( i = 0, foundvdim = 0 ; i < vdimcurs ; i++ ) {
                  if ( !strcmp( vdims[i][1], e ) ) {
                    foundvdim = 1 ; break ;
                  }
                }
                if ( ! foundvdim ) {
                  if (vdimcurs < 100 ) {
                    strcpy( vdims[vdimcurs][0], s ) ;
                    strcpy( vdims[vdimcurs][1], e ) ;
                    vdimcurs++ ;
                  } else {
                    fprintf(stderr,"REGISTRY ERROR: too many different vertical dimensions (> %d).\n", MAX_VDIMS ) ;
                    fprintf(stderr,"That seems like a lot, but if you are sure, increase MAX_VDIMS\n" ) ;
                    fprintf(stderr,"in external/RSL_LITE/gen_comms.c and recompile\n") ;
                    exit(5) ;
                  }
                }
              }
            }

            if ( q->node_kind & FOURD ) {
              if ( n4d < MAX_4DARRAYS ) {
                int d ;
                char temp[80], tx[80], r[10], *colon ;
                strcpy( name_4d[n4d], q->name ) ;
                for ( d = 3 ; d < q->ndims ; d++ ) {
                  sprintf(temp,"*(%s_edim%d-%s_sdim%d+1)",q->name,d-2,q->name,d-2) ;
                  strcat( name_4d[n4d],temp) ;
                }
              } else { 
                fprintf(stderr,"REGISTRY ERROR: too many 4d arrays (> %d).\n", MAX_4DARRAYS ) ;
                fprintf(stderr,"That seems like a lot, but if you are sure, increase MAX_4DARRAYS\n" ) ;
                fprintf(stderr,"in external/RSL_LITE/gen_comms.c and recompile\n") ;
                exit(5) ;
              }
              n4d++ ;
            }
            else
            {
              if        ( ! strcmp( q->type->name, "real") ) {
                if         ( q->ndims == 3 )      { n3dR++ ; }
	        else    if ( q->ndims == 2 )      { n2dR++ ; }
	      } else if ( ! strcmp( q->type->name, "integer") ) {
                if         ( q->ndims == 3 )      { n3dI++ ; }
	        else    if ( q->ndims == 2 )      { n2dI++ ; }
	      } else if ( ! strcmp( q->type->name, "doubleprecision") ) {
                if         ( q->ndims == 3 )      { n3dD++ ; }
	        else    if ( q->ndims == 2 )      { n2dD++ ; }
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
fprintf(fp,"CALL wrf_debug(3,'calling RSL_LITE_INIT_EXCH %s for Y %s')\n",maxstenwidth,fname) ;
#endif
    if ( subgrid != 0 ) {
      fprintf(fp,"IF ( grid%%sr_y .GT. 0 ) THEN\n") ;
    }

    fprintf(fp,"CALL rsl_comm_iter_init(%s,jps,jpe)\n",maxstenwidth) ;
    fprintf(fp,"DO WHILE ( rsl_comm_iter( grid%%id , grid%%is_intermediate, %s , &\n", maxstenwidth ) ; 
    fprintf(fp,"                         0 , jds,jde,jps,jpe, grid%%njds, grid%%njde , & \n" ) ;
    fprintf(fp,"     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & \n" ) ;
    fprintf(fp,"     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))\n" ) ;
    fprintf(fp," CALL RSL_LITE_INIT_EXCH ( local_communicator, %s, 0, &\n",maxstenwidth) ;
    fprintf(fp,"     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & \n" ) ;
    fprintf(fp,"     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & \n" ) ;
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
    if ( subgrid == 0 ) {
      fprintf(fp,"      ips, ipe, jps, jpe, kps, MAX(1,1&\n") ;
      for ( i = 0 ; i < vdimcurs ; i++ ) {
        fprintf(fp,",%s &\n",vdims[i][1] ) ;
      }
      fprintf(fp,"))\n") ;
    } else {
      fprintf(fp,"(ips-1)*grid%%sr_x+1,ipe*grid%%sr_x,(jps-1)*grid%%sr_y+1,jpe*grid%%sr_y,kps,kpe)\n") ;
    }

/* generate packs prior to stencil exchange in Y */
    gen_packs_halo( fp, p, maxstenwidth, 0, 0, "RSL_LITE_PACK", "local_communicator", always_interp_mp ) ;
/* generate stencil exchange in Y */
    fprintf(fp,"   CALL RSL_LITE_EXCH_Y ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &\n") ;
    fprintf(fp,"                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )\n" ) ;
/* generate unpacks after stencil exchange in Y */
    gen_packs_halo( fp, p, maxstenwidth, 0, 1 , "RSL_LITE_PACK", "local_communicator", always_interp_mp ) ;
    fprintf(fp,"ENDDO\n") ; 

/* generate the stencil init statement for X transfer */
    fprintf(fp,"CALL rsl_comm_iter_init(%s,ips,ipe)\n",maxstenwidth) ;
    fprintf(fp,"DO WHILE ( rsl_comm_iter( grid%%id , grid%%is_intermediate, %s , &\n", maxstenwidth ) ; 
    fprintf(fp,"                         1 , ids,ide,ips,ipe, grid%%nids, grid%%nide , & \n" ) ;
    fprintf(fp,"     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & \n" ) ;
    fprintf(fp,"     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p    ))\n" ) ;
    fprintf(fp," CALL RSL_LITE_INIT_EXCH ( local_communicator, %s, 1, &\n",maxstenwidth) ;
    fprintf(fp,"     rsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p,   & \n" ) ;
    fprintf(fp,"     rsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p,   & \n" ) ;
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
    if ( subgrid == 0 ) {
      fprintf(fp,"      ips, ipe, jps, jpe, kps, MAX(1,1&\n") ;
      for ( i = 0 ; i < vdimcurs ; i++ ) {
        fprintf(fp,",%s &\n",vdims[i][1] ) ;
      }
      fprintf(fp,"))\n") ;
    } else {
      fprintf(fp,"(ips-1)*grid%%sr_x+1,ipe*grid%%sr_x,(jps-1)*grid%%sr_y+1,jpe*grid%%sr_y,kps,kpe)\n") ;
    }
/* generate packs prior to stencil exchange in X */
    gen_packs_halo( fp, p, maxstenwidth, 1, 0, "RSL_LITE_PACK", "local_communicator", always_interp_mp ) ;
/* generate stencil exchange in X */
    fprintf(fp,"   CALL RSL_LITE_EXCH_X ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y, &\n") ;
    fprintf(fp,"                          rsl_sendw_m,  rsl_sendw_p, rsl_recvw_m,  rsl_recvw_p    )\n" ) ;
/* generate unpacks after stencil exchange in X */
    gen_packs_halo( fp, p, maxstenwidth, 1, 1, "RSL_LITE_PACK", "local_communicator", always_interp_mp ) ;
    fprintf(fp,"    ENDDO\n") ; 
    if ( subgrid != 0 ) {
      fprintf(fp,"ENDIF\n") ;
    }
    close_the_file(fp) ;
    if ( incname == NULL ) {
      /* Finish call to custom routine that encapsulates inlined comm calls */
      print_call_or_def(fpcall, p, "CALL", commname, "local_communicator", need_config_flags );
      close_the_file(fpcall) ;
      /* Generate definition of custom routine that encapsulates inlined comm calls */
      print_call_or_def(fpsub, p, "SUBROUTINE", commname, "local_communicator", need_config_flags );
      print_decl(fpsub, p, "local_communicator", need_config_flags );
      print_body(fpsub, commname);
      close_the_file(fpsub) ;
    }
  }
  return(0) ;
}

gen_packs_halo ( FILE *fp , node_t *p, char *shw, int xy /* 0=y,1=x */ , int pu /* 0=pack,1=unpack */, char * packname, char * commname, int always_interp_mp )   
{
  node_t * q ;
  node_t * dimd ;
  char fname[NAMELEN] ;
  char tmp[NAMELEN_LONG], tmp2[NAMELEN_LONG], tmp3[NAMELEN_LONG], tmp4[NAMELEN_LONG] ;
  char commuse[NAMELEN] ;
  int maxstenwidth, stenwidth ;
  char * t1, * t2 , *wordsize ;
  char varref[NAMELEN] ;
  char varname[NAMELEN] ;
  char * pos1 , * pos2 ;
  char indices[NAMELEN], post[NAMELEN], memord[NAMELEN] ;
  int xdex,ydex,zdex ;

    strcpy( tmp, p->comm_define ) ;
    strcpy( commuse, p->use ) ;
    t1 = strtok_rentr( tmp , ";" , &pos1 ) ;
    while ( t1 != NULL )
    {
      strcpy( tmp2 , t1 ) ;
      if (( t2 = strtok_rentr( tmp2 , ":" , &pos2 )) == NULL )
       { fprintf(stderr,"unparseable description for halo %s\n", p->name ) ; continue ; }
      t2 = strtok_rentr(NULL,",", &pos2) ;
      while ( t2 != NULL )
      {
        if ((q = get_entry_r( t2, commuse, Domain.fields )) == NULL )
          { fprintf(stderr,"WARNING 1b : %s in halo spec %s (%s) is not defined in registry.\n",t2,p->name, commuse) ; }
        else
        {

          strcpy( varname, t2 ) ;
          strcpy( varref, t2 ) ;
          if ( q->node_kind & FIELD  && ! (q->node_kind & I1) ) {
             sprintf(varref,"grid%%%s",t2) ;
          }

          if      (  strcmp( q->type->name, "real") && strcmp( q->type->name, "integer") && strcmp( q->type->name, "doubleprecision") ) { ; }
          else if ( q->boundary_array ) { ; }
          else
          { 
            if(!always_interp_mp && p->mp_var) {
              fprintf(fp,"if(interp_mp) then\n");
            }

            if      ( ! strcmp( q->type->name, "real") )            { wordsize = "RWORDSIZE" ; }
            else if ( ! strcmp( q->type->name, "integer") )         { wordsize = "IWORDSIZE" ; }
            else if ( ! strcmp( q->type->name, "doubleprecision") ) { wordsize = "DWORDSIZE" ; }
            if ( q->node_kind & FOURD )
            {
              node_t *member ;
              zdex = get_index_for_coord( q , COORD_Z ) ;
              dimd = get_dimnode_for_coord( q , COORD_Z ) ;
              if ( zdex >=1 && zdex <= 3 && dimd != NULL )
              {
                int d ;
                char * colon ;
                char moredims[80], tx[80], temp[10], r[80] ;
                char sd[256], ed[256] , sm[256], em[256] , sp[256], ep[256] ;

                set_mem_order( q->members, memord , 3 ) ;
fprintf(fp,"DO itrace = PARAM_FIRST_SCALAR, num_%s\n",q->name ) ;
                strcpy(moredims,"") ; 
                for ( d = q->ndims-1 ; d >= 3  ; d-- ) {
fprintf(fp,"  DO idim%d = %s_sdim%d,%s_edim%d\n",d-2,q->name,d-2,q->name,d-2 ) ;
                }
                for ( d = 3 ; d < q->ndims ; d++ ) {
                  strcpy(r,"") ;
                  range_of_dimension( r, tx, d, q, "config_flags%" ) ;
                  colon = index(tx,':') ; if ( colon != NULL ) *colon = ',' ;
                  sprintf(temp,"idim%d",d-2) ;
                  strcat(moredims,",") ; strcat(moredims,temp) ;
                }
                strcat(moredims,",") ;
                xdex = get_index_for_coord( q , COORD_X ) ;
                ydex = get_index_for_coord( q , COORD_Y ) ;
                if      ( dimd->len_defined_how == DOMAIN_STANDARD ) {
                  strcpy(sd,"kds") ; strcpy(ed,"kde" ) ;
                  strcpy(sm,"kms") ; strcpy(em,"kme" ) ;
                  strcpy(sp,"kps") ; strcpy(ep,"kpe" ) ;
                } else if ( dimd->len_defined_how == NAMELIST ) {
                  if ( !strcmp(dimd->assoc_nl_var_s,"1") ) {
                    strcpy(sd,"1") ;
                    sprintf(ed,"config_flags%%%s",dimd->assoc_nl_var_e) ;
                  } else {
                    sprintf(sd,"config_flags%%%s",dimd->assoc_nl_var_s) ;
                    sprintf(ed,"config_flags%%%s",dimd->assoc_nl_var_e) ;
                  }
                  strcpy(sm,sd) ; strcpy(em,ed ) ;
                  strcpy(sp,sd) ; strcpy(ep,ed ) ;
                } else if ( dimd->len_defined_how == CONSTANT ) {
                  sprintf(sd,"%d",dimd->coord_start) ; sprintf(ed,"%d",dimd->coord_end) ;
                  strcpy(sm,sd) ; strcpy(em,ed ) ;
                  strcpy(sp,sd) ; strcpy(ep,ed ) ;
                }
fprintf(fp," IF ( SIZE(%s,%d)*SIZE(%s,%d) .GT. 1 ) THEN\n",varref,xdex+1,varref,ydex+1 ) ; 
fprintf(fp,"  CALL %s ( %s,&\n%s ( %s%sitrace),%s,&\nrsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &\nrsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &\n%s, %d, %d, DATA_ORDER_%s, %d, &\n",
                       packname, commname, varref , index_with_firstelem("","grid%",-1,tmp4,q,""),moredims, shw, wordsize, xy, pu, memord, xy?(q->stag_x?1:0):(q->stag_y?1:0) ) ;
fprintf(fp,"mytask, ntasks, ntasks_x, ntasks_y,       &\n") ;
if ( !strcmp( packname, "RSL_LITE_PACK_SWAP" ) ||
     !strcmp( packname, "RSL_LITE_PACK_CYCLE" ) ) {
  fprintf(fp,"thisdomain_max_halo_width,                         &\n") ;
}
                if ( q->subgrid == 0 ) {
fprintf(fp,"ids, ide, jds, jde, %s, %s,             &\n",sd,ed) ;
fprintf(fp,"ims, ime, jms, jme, %s, %s,             &\n",sm,em) ;
fprintf(fp,"ips, ipe, jps, jpe, %s, %s              )\n",sp,ep) ;
                } else {
fprintf(fp,"ids, ide*grid%%sr_x, jds, jde*grid%%sr_y, %s, %s, &\n",sd,ed) ;
fprintf(fp,"(ims-1)*grid%%sr_x+1,ime*grid%%sr_x,(jms-1)*grid%%sr_y+1,jme*grid%%sr_y,%s,%s,&\n",sm,em) ;
fprintf(fp,"(ips-1)*grid%%sr_x+1,ipe*grid%%sr_x,(jps-1)*grid%%sr_y+1,jpe*grid%%sr_y,%s,%s)\n",sp,ep) ;
                }
fprintf(fp," ENDIF\n") ;
               for ( d = 3 ; d < q->ndims ; d++ ) {
fprintf(fp,"  ENDDO ! idim%d \n",d-2 ) ;
               }

fprintf(fp,"ENDDO\n") ;
              }
              else
              {
                fprintf(stderr,"WARNING: %d some dimension info missing for 4d array %s\n",zdex,t2) ;
              }
            }
            else
            {
              set_mem_order( q, memord , 3 ) ;
              if       ( q->ndims == 3 ) {

                dimd = get_dimnode_for_coord( q , COORD_Z ) ;
                xdex = get_index_for_coord( q , COORD_X ) ;
                ydex = get_index_for_coord( q , COORD_Y ) ;
                zdex = get_index_for_coord( q , COORD_Z ) ;
                fprintf(fp,"IF ( SIZE(%s,%d)*SIZE(%s,%d) .GT. 1 ) THEN\n",varref,xdex+1,varref,ydex+1 ) ; 
                fprintf(fp,"CALL %s ( %s,&\n %s, %s,&\nrsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &\nrsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &\n%s, %d, %d, DATA_ORDER_%s, %d, &\n", 
                        packname, commname, varref, shw, wordsize, xy, pu, memord, xy?(q->stag_x?1:0):(q->stag_y?1:0) ) ;
                fprintf(fp,"mytask, ntasks, ntasks_x, ntasks_y,       &\n") ;
                if ( dimd != NULL )
                {
                  char s[256], e[256] ;
                  if      ( dimd->len_defined_how == DOMAIN_STANDARD ) {
                    if ( q->subgrid == 0 ) {
                      fprintf(fp,"ids, ide, jds, jde, kds, kde,             &\n") ;
                      fprintf(fp,"ims, ime, jms, jme, kms, kme,             &\n") ;
                      fprintf(fp,"ips, ipe, jps, jpe, kps, kpe              )\n") ;
                    } else {
fprintf(fp,"ids, ide*grid%%sr_x, jds, jde*grid%%sr_y, kds, kde, &\n") ;
fprintf(fp,"(ims-1)*grid%%sr_x+1,ime*grid%%sr_x,(jms-1)*grid%%sr_y+1,jme*grid%%sr_y,kms,kme,&\n") ;
fprintf(fp,"(ips-1)*grid%%sr_x+1,ipe*grid%%sr_x,(jps-1)*grid%%sr_y+1,jpe*grid%%sr_y,kps,kpe)\n") ;
                    }
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
                    if ( q->subgrid == 0 ) {
                      fprintf(fp,"ids, ide, jds, jde, %s, %s,             &\n",s,e) ;
                      fprintf(fp,"ims, ime, jms, jme, %s, %s,             &\n",s,e) ;
                      fprintf(fp,"ips, ipe, jps, jpe, %s, %s              )\n",s,e) ;
                    } else {
fprintf(fp,"ids, ide*grid%%sr_x, jds, jde*grid%%sr_y, kds, kde, &\n") ;
fprintf(fp,"(ims-1)*grid%%sr_x+1,ime*grid%%sr_x,(jms-1)*grid%%sr_y+1,jme*grid%%sr_y,%s,%s,&\n",s,e) ;
fprintf(fp,"(ips-1)*grid%%sr_x+1,ipe*grid%%sr_x,(jps-1)*grid%%sr_y+1,jpe*grid%%sr_y,%s,%s)\n",s,e) ;
                    }
                  }
                  else if ( dimd->len_defined_how == CONSTANT )
                  {
                    if ( q->subgrid == 0 ) {
                      fprintf(fp,"ids, ide, jds, jde, %d, %d,             &\n",dimd->coord_start,dimd->coord_end) ;
                      fprintf(fp,"ims, ime, jms, jme, %d, %d,             &\n",dimd->coord_start,dimd->coord_end) ;
                      fprintf(fp,"ips, ipe, jps, jpe, %d, %d              )\n",dimd->coord_start,dimd->coord_end) ;
                    } else {
fprintf(fp,"ids, ide*grid%%sr_x, jds, jde*grid%%sr_y, kds, kde, &\n") ;
fprintf(fp,"(ims-1)*grid%%sr_x+1,ime*grid%%sr_x,(jms-1)*grid%%sr_y+1,jme*grid%%sr_y,%d,%d,&\n",dimd->coord_start,dimd->coord_end) ;
fprintf(fp,"(ips-1)*grid%%sr_x+1,ipe*grid%%sr_x,(jps-1)*grid%%sr_y+1,jpe*grid%%sr_y,%d,%d)\n",dimd->coord_start,dimd->coord_end) ;
                    }
                  }
                }
                fprintf(fp,"ENDIF\n") ;
              } else if ( q->ndims == 2 ) {
                xdex = get_index_for_coord( q , COORD_X ) ;
                ydex = get_index_for_coord( q , COORD_Y ) ;
                fprintf(fp,"IF ( SIZE(%s,%d)*SIZE(%s,%d) .GT. 1 ) THEN\n",varref,xdex+1,varref,ydex+1 ) ; 
                fprintf(fp,"CALL %s ( %s,&\n %s, %s,&\nrsl_sendbeg_m, rsl_sendw_m, rsl_sendbeg_p, rsl_sendw_p, &\nrsl_recvbeg_m, rsl_recvw_m, rsl_recvbeg_p, rsl_recvw_p, &\n%s, %d, %d, DATA_ORDER_%s, %d, &\n", 
                       packname, commname, varref, shw, wordsize, xy, pu, memord, xy?(q->stag_x?1:0):(q->stag_y?1:0) ) ;
                fprintf(fp,"mytask, ntasks, ntasks_x, ntasks_y,       &\n") ;
                if ( q->subgrid == 0 ) {
                  fprintf(fp,"ids, ide, jds, jde, 1  , 1  ,             &\n") ;
                  fprintf(fp,"ims, ime, jms, jme, 1  , 1  ,             &\n") ;
                  fprintf(fp,"ips, ipe, jps, jpe, 1  , 1                )\n") ;
                } else {
fprintf(fp,"ids, ide*grid%%sr_x, jds, jde*grid%%sr_y, kds, kde, &\n") ;
fprintf(fp,"(ims-1)*grid%%sr_x+1,ime*grid%%sr_x,(jms-1)*grid%%sr_y+1,jme*grid%%sr_y,1,1,&\n") ;
fprintf(fp,"(ips-1)*grid%%sr_x+1,ipe*grid%%sr_x,(jps-1)*grid%%sr_y+1,jpe*grid%%sr_y,1,1)\n") ;
                }
                fprintf(fp,"ENDIF\n") ;
              }
            }
            if(!always_interp_mp && p->mp_var) {
              fprintf(fp,"endif\n");
            }
          }
        }
        t2 = strtok_rentr( NULL , "," , &pos2 ) ;
      }
      t1 = strtok_rentr( NULL , ";" , &pos1 ) ;
    }
}

gen_packs ( FILE *fp , node_t *p, int shw, int xy /* 0=y,1=x */ , int pu /* 0=pack,1=unpack */, char * packname, char * commname )   
{
  node_t * q ;
  node_t * dimd ;
  char fname[NAMELEN] ;
  char tmp[NAMELEN_LONG], tmp2[NAMELEN_LONG], tmp3[NAMELEN_LONG] ;
  char commuse[NAMELEN] ;
  int maxstenwidth, stenwidth ;
  char * t1, * t2 , *wordsize ;
  char varref[NAMELEN] ;
  char varname[NAMELEN] ;
  char * pos1 , * pos2 ;
  char indices[NAMELEN], post[NAMELEN], memord[NAMELEN] ;
  int xdex,ydex,zdex ;

    strcpy( tmp, p->comm_define ) ;
    strcpy( commuse, p->use ) ;
    t1 = strtok_rentr( tmp , ";" , &pos1 ) ;
    while ( t1 != NULL )
    {
      strcpy( tmp2 , t1 ) ;
      if (( t2 = strtok_rentr( tmp2 , ":" , &pos2 )) == NULL )
       { fprintf(stderr,"unparseable description for halo %s\n", p->name ) ; continue ; }
      t2 = strtok_rentr(NULL,",", &pos2) ;
      while ( t2 != NULL )
      {
        if ((q = get_entry_r( t2, commuse, Domain.fields )) == NULL )
          { fprintf(stderr,"WARNING 1b : %s in halo spec %s (%s) is not defined in registry.\n",t2,p->name, commuse) ; }
        else
        {

          strcpy( varname, t2 ) ;
          strcpy( varref, t2 ) ;
          if ( q->node_kind & FIELD  && ! (q->node_kind & I1) ) {
             sprintf(varref,"grid%%%s",t2) ;
          }

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
                set_mem_order( q->members, memord , 3 ) ;
fprintf(fp,"DO itrace = PARAM_FIRST_SCALAR, num_%s\n",q->name ) ;
                xdex = get_index_for_coord( q , COORD_X ) ;
                ydex = get_index_for_coord( q , COORD_Y ) ;
fprintf(fp," IF ( SIZE(%s,%d)*SIZE(%s,%d) .GT. 1 ) THEN\n",varref,xdex+1,varref,ydex+1 ) ; 
fprintf(fp,"  CALL %s ( %s,&\n%s ( grid%%sm31,grid%%sm32,grid%%sm33,itrace), %d, %s, %d, %d, DATA_ORDER_%s, %d, &\n",
                       packname, commname, varref , shw, wordsize, xy, pu, memord, xy?(q->stag_x?1:0):(q->stag_y?1:0) ) ;
fprintf(fp,"mytask, ntasks, ntasks_x, ntasks_y,       &\n") ;
if ( !strcmp( packname, "RSL_LITE_PACK_SWAP" ) ||
     !strcmp( packname, "RSL_LITE_PACK_CYCLE" ) ) {
  fprintf(fp,"thisdomain_max_halo_width,                         &\n") ;
}
if ( q->subgrid == 0 ) {
fprintf(fp,"ids, ide, jds, jde, kds, kde,             &\n") ;
fprintf(fp,"ims, ime, jms, jme, kms, kme,             &\n") ;
fprintf(fp,"ips, ipe, jps, jpe, kps, kpe              )\n") ;
} else {
fprintf(fp,"ids, ide*grid%%sr_x, jds, jde*grid%%sr_y, kds, kde, &\n") ;
fprintf(fp,"(ims-1)*grid%%sr_x+1,ime*grid%%sr_x,(jms-1)*grid%%sr_y+1,jme*grid%%sr_y,kms,kme,&\n") ;
fprintf(fp,"(ips-1)*grid%%sr_x+1,ipe*grid%%sr_x,(jps-1)*grid%%sr_y+1,jpe*grid%%sr_y,kps,kpe)\n") ;
}
fprintf(fp," ENDIF\n") ;
fprintf(fp,"ENDDO\n") ;
              }
              else
              {
                fprintf(stderr,"WARNING: %d some dimension info missing for 4d array %s\n",zdex,t2) ;
              }
            }
            else
            {
              set_mem_order( q, memord , 3 ) ;
              if       ( q->ndims == 3 ) {

                dimd = get_dimnode_for_coord( q , COORD_Z ) ;
                xdex = get_index_for_coord( q , COORD_X ) ;
                ydex = get_index_for_coord( q , COORD_Y ) ;
                zdex = get_index_for_coord( q , COORD_Z ) ;
                fprintf(fp,"IF ( SIZE(%s,%d)*SIZE(%s,%d) .GT. 1 ) THEN\n",varref,xdex+1,varref,ydex+1 ) ; 
                if ( dimd != NULL )
                {
                  char s[256], e[256] ;

                  if      ( dimd->len_defined_how == DOMAIN_STANDARD ) {
                    fprintf(fp,"CALL %s ( %s,&\n %s, %d, %s, %d, %d, DATA_ORDER_%s, %d, &\n", packname, commname, varref, shw, wordsize, xy, pu, memord, xy?(q->stag_x?1:0):(q->stag_y?1:0) ) ;
                    fprintf(fp,"mytask, ntasks, ntasks_x, ntasks_y,       &\n") ;
                    if ( q->subgrid == 0 ) {
                      fprintf(fp,"ids, ide, jds, jde, kds, kde,             &\n") ;
                      fprintf(fp,"ims, ime, jms, jme, kms, kme,             &\n") ;
                      fprintf(fp,"ips, ipe, jps, jpe, kps, kpe              )\n") ;
                    } else {
fprintf(fp,"ids, ide*grid%%sr_x, jds, jde*grid%%sr_y, kds, kde, &\n") ;
fprintf(fp,"(ims-1)*grid%%sr_x+1,ime*grid%%sr_x,(jms-1)*grid%%sr_y+1,jme*grid%%sr_y,kms,kme,&\n") ;
fprintf(fp,"(ips-1)*grid%%sr_x+1,ipe*grid%%sr_x,(jps-1)*grid%%sr_y+1,jpe*grid%%sr_y,kps,kpe)\n") ;
                    }
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
                    fprintf(fp,"CALL %s ( %s,&\n %s, %d, %s, %d, %d, DATA_ORDER_%s, %d, &\n", packname, commname, varref, shw, wordsize, xy, pu, memord, xy?(q->stag_x?1:0):(q->stag_y?1:0) ) ;
                    fprintf(fp,"mytask, ntasks, ntasks_x, ntasks_y,       &\n") ;
                    if ( q->subgrid == 0 ) {
                      fprintf(fp,"ids, ide, jds, jde, %s, %s,             &\n",s,e) ;
                      fprintf(fp,"ims, ime, jms, jme, %s, %s,             &\n",s,e) ;
                      fprintf(fp,"ips, ipe, jps, jpe, %s, %s              )\n",s,e) ;
                    } else {
fprintf(fp,"ids, ide*grid%%sr_x, jds, jde*grid%%sr_y, kds, kde, &\n") ;
fprintf(fp,"(ims-1)*grid%%sr_x+1,ime*grid%%sr_x,(jms-1)*grid%%sr_y+1,jme*grid%%sr_y,%s,%s,&\n",s,e) ;
fprintf(fp,"(ips-1)*grid%%sr_x+1,ipe*grid%%sr_x,(jps-1)*grid%%sr_y+1,jpe*grid%%sr_y,%s,%s)\n",s,e) ;
                    }
                  }
                  else if ( dimd->len_defined_how == CONSTANT )
                  {
                    fprintf(fp,"CALL %s ( %s,&\n %s, %d, %s, %d, %d, DATA_ORDER_%s, %d, &\n", packname, commname, varref, shw, wordsize, xy, pu, memord, xy?(q->stag_x?1:0):(q->stag_y?1:0) ) ;
                    fprintf(fp,"mytask, ntasks, ntasks_x, ntasks_y,       &\n") ;
                    if ( q->subgrid == 0 ) {
                      fprintf(fp,"ids, ide, jds, jde, %d, %d,             &\n",dimd->coord_start,dimd->coord_end) ;
                      fprintf(fp,"ims, ime, jms, jme, %d, %d,             &\n",dimd->coord_start,dimd->coord_end) ;
                      fprintf(fp,"ips, ipe, jps, jpe, %d, %d              )\n",dimd->coord_start,dimd->coord_end) ;
                    } else {
fprintf(fp,"ids, ide*grid%%sr_x, jds, jde*grid%%sr_y, kds, kde, &\n") ;
fprintf(fp,"(ims-1)*grid%%sr_x+1,ime*grid%%sr_x,(jms-1)*grid%%sr_y+1,jme*grid%%sr_y,%d,%d,&\n",dimd->coord_start,dimd->coord_end) ;
fprintf(fp,"(ips-1)*grid%%sr_x+1,ipe*grid%%sr_x,(jps-1)*grid%%sr_y+1,jpe*grid%%sr_y,%d,%d)\n",dimd->coord_start,dimd->coord_end) ;
                    }
                  }
                }
                fprintf(fp,"ENDIF\n") ;
              } else if ( q->ndims == 2 ) {
                xdex = get_index_for_coord( q , COORD_X ) ;
                ydex = get_index_for_coord( q , COORD_Y ) ;
                fprintf(fp,"IF ( SIZE(%s,%d)*SIZE(%s,%d) .GT. 1 ) THEN\n",varref,xdex+1,varref,ydex+1 ) ; 
                fprintf(fp,"CALL %s ( %s,&\n %s, %d, %s, %d, %d, DATA_ORDER_%s, %d, &\n", packname, commname, varref, shw, wordsize, xy, pu, memord, xy?(q->stag_x?1:0):(q->stag_y?1:0) ) ;
                fprintf(fp,"mytask, ntasks, ntasks_x, ntasks_y,       &\n") ;
                if ( q->subgrid == 0 ) {
                  fprintf(fp,"ids, ide, jds, jde, 1  , 1  ,             &\n") ;
                  fprintf(fp,"ims, ime, jms, jme, 1  , 1  ,             &\n") ;
                  fprintf(fp,"ips, ipe, jps, jpe, 1  , 1                )\n") ;
                } else {
fprintf(fp,"ids, ide*grid%%sr_x, jds, jde*grid%%sr_y, kds, kde, &\n") ;
fprintf(fp,"(ims-1)*grid%%sr_x+1,ime*grid%%sr_x,(jms-1)*grid%%sr_y+1,jme*grid%%sr_y,1,1,&\n") ;
fprintf(fp,"(ips-1)*grid%%sr_x+1,ipe*grid%%sr_x,(jps-1)*grid%%sr_y+1,jpe*grid%%sr_y,1,1)\n") ;
                }
                fprintf(fp,"ENDIF\n") ;
              }
            }
          }
          
        }
        t2 = strtok_rentr( NULL , "," , &pos2 ) ;
      }
      t1 = strtok_rentr( NULL , ";" , &pos1 ) ;
    }
}

int
gen_periods ( char * dirname , node_t * periods )
{
  node_t * p, * q ;
  node_t * dimd ;
  char commname[NAMELEN] ;
  char fname[NAMELEN], fnamecall[NAMELEN], fnamesub[NAMELEN] ;
  char tmp[NAMELEN], tmp2[NAMELEN], tmp3[NAMELEN] ;
  char commuse[NAMELEN] ;
  int maxperwidth, perwidth ;
  FILE * fp ;
  FILE * fpcall ;
  FILE * fpsub ;
  char * t1, * t2 ;
  char varref[NAMELEN] ;
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

  /* Open and truncate REGISTRY_COMM_DM_PERIOD_subs.inc so file exists even if there are no periods. */
  if ( strlen(dirname) > 0 ) { sprintf(fnamesub,"%s/REGISTRY_COMM_DM_PERIOD_subs.inc",dirname) ; }
  else                       { sprintf(fnamesub,"REGISTRY_COMM_DM_PERIOD_subs.inc") ; }
  if ((fpsub = fopen( fnamesub , "w" )) == NULL ) 
  {
    fprintf(stderr,"WARNING: gen_periods in registry cannot open %s for writing\n",fnamesub ) ;
  }
  if ( fpsub != NULL ) {
    print_warning(fpsub,fnamesub) ;
    fclose(fpsub) ;
  }

  for ( p = periods ; p != NULL ; p = p->next )
  {
    strcpy( commname, p->name ) ;
    make_upper_case(commname) ;
    if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s_inline.inc",dirname,commname) ; }
    else                       { sprintf(fname,"%s_inline.inc",commname) ; }
    /* Generate call to custom routine that encapsulates inlined comm calls */
    if ( strlen(dirname) > 0 ) { sprintf(fnamecall,"%s/%s.inc",dirname,commname) ; }
    else                       { sprintf(fnamecall,"%s.inc",commname) ; }
    if ((fpcall = fopen( fnamecall , "w" )) == NULL ) 
    {
      fprintf(stderr,"WARNING: gen_periods in registry cannot open %s for writing\n",fnamecall ) ;
      continue ; 
    }
    print_warning(fpcall,fnamecall) ;
    print_call_or_def(fpcall, p, "CALL", commname, "local_communicator_periodic", 1 );
    close_the_file(fpcall) ;

    /* Generate definition of custom routine that encapsulates inlined comm calls */
    if ( strlen(dirname) > 0 ) { sprintf(fnamesub,"%s/REGISTRY_COMM_DM_PERIOD_subs.inc",dirname) ; }
    else                       { sprintf(fnamesub,"REGISTRY_COMM_DM_PERIOD_subs.inc") ; }
    if ((fpsub = fopen( fnamesub , "a" )) == NULL ) 
    {
      fprintf(stderr,"WARNING: gen_periods in registry cannot open %s for writing\n",fnamesub ) ;
      continue ; 
    }
    print_call_or_def(fpsub, p, "SUBROUTINE", commname, "local_communicator_periodic", 1 );
    print_decl(fpsub, p, "local_communicator_periodic", 1 );
    print_body(fpsub, commname);
    close_the_file(fpsub) ;

    /* Generate inlined comm calls */
    if ((fp = fopen( fname , "w" )) == NULL ) 
    {
      fprintf(stderr,"WARNING: gen_periods in registry cannot open %s for writing\n",fname ) ;
      continue ; 
    }
    /* get maximum period width */
    maxperwidth = 0 ;
    strcpy( tmp, p->comm_define ) ;
    t1 = strtok_rentr( tmp , ";" , &pos1 ) ;
    while ( t1 != NULL )
    {
      strcpy( tmp2 , t1 ) ;
      if (( t2 = strtok_rentr( tmp2 , ":" , &pos2 )) == NULL )
       { fprintf(stderr,"unparseable description for period %s\n", commname ) ; exit(1) ; }
      perwidth = atoi (t2) ;
      if ( perwidth > maxperwidth ) maxperwidth = perwidth ;
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
       { fprintf(stderr,"unparseable description for period %s\n", commname ) ; continue ; }
      t2 = strtok_rentr(NULL,",", &pos2) ;
      while ( t2 != NULL )
      {
        if ((q = get_entry_r( t2, commuse, Domain.fields )) == NULL )
          { fprintf(stderr,"WARNING 1 : %s in period spec %s (%s) is not defined in registry.\n",t2,commname, commuse) ; }
        else
        {
          if      (  strcmp( q->type->name, "real") && strcmp( q->type->name, "integer") && strcmp( q->type->name, "doubleprecision") )
            { fprintf(stderr,"WARNING: only type 'real', 'doubleprecision', or 'integer' can be part of period exchange. %s in %s is %s\n",t2,commname,q->type->name) ; }
          else if ( q->boundary_array )
            { fprintf(stderr,"WARNING: boundary array %s cannot be member of period spec %s.\n",t2,commname) ; }
          else
          {
            if ( q->node_kind & FOURD ) {
              if ( n4d < MAX_4DARRAYS ) {
                strcpy( name_4d[n4d], q->name ) ;
              } else { 
                fprintf(stderr,"REGISTRY ERROR: too many 4d arrays (> %d).\n", MAX_4DARRAYS ) ;
                fprintf(stderr,"That seems like a lot, but if you are sure, increase MAX_4DARRAYS\n" ) ;
                fprintf(stderr,"in external/RSL_LITE/gen_comms.c and recompile\n") ;
                exit(5) ;
              }
              n4d++ ;
            }
            else
            {
              if        ( ! strcmp( q->type->name, "real") ) {
                if         ( q->ndims == 3 )      { n3dR++ ; }
	        else    if ( q->ndims == 2 )      { n2dR++ ; }
	      } else if ( ! strcmp( q->type->name, "integer") ) {
                if         ( q->ndims == 3 )      { n3dI++ ; }
	        else    if ( q->ndims == 2 )      { n2dI++ ; }
	      } else if ( ! strcmp( q->type->name, "doubleprecision") ) {
                if         ( q->ndims == 3 )      { n3dD++ ; }
	        else    if ( q->ndims == 2 )      { n2dD++ ; }
	      }
	    }
	  }
	}
        t2 = strtok_rentr( NULL , "," , &pos2 ) ;
      }
      t1 = strtok_rentr( NULL , ";" , &pos1 ) ;
    }

    fprintf(fp,"IF ( config_flags%%periodic_x ) THEN\n") ;

/* generate the stencil init statement for X transfer */
    fprintf(fp,"CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, %d , &\n",maxperwidth) ;
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
/* generate packs prior to exchange in X */
    gen_packs( fp, p, maxperwidth, 1, 0, "RSL_LITE_PACK_PERIOD", "local_communicator_periodic" ) ;
/* generate exchange in X */
    fprintf(fp,"   CALL RSL_LITE_EXCH_PERIOD_X ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )\n") ;
/* generate unpacks after exchange in X */
    gen_packs( fp, p, maxperwidth, 1, 1, "RSL_LITE_PACK_PERIOD", "local_communicator_periodic" ) ;
    fprintf(fp,"END IF\n") ;


    fprintf(fp,"IF ( config_flags%%periodic_y ) THEN\n") ;
/* generate the init statement for Y transfer */
    fprintf(fp,"CALL RSL_LITE_INIT_PERIOD ( local_communicator_periodic, %d , &\n",maxperwidth) ;
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
/* generate packs prior to exchange in Y */
    gen_packs( fp, p, maxperwidth, 0, 0, "RSL_LITE_PACK_PERIOD", "local_communicator_periodic" ) ;  
/* generate exchange in Y */
    fprintf(fp,"   CALL RSL_LITE_EXCH_PERIOD_Y ( local_communicator_periodic , mytask, ntasks, ntasks_x, ntasks_y )\n") ;
/* generate unpacks after exchange in Y */
    gen_packs( fp, p, maxperwidth, 0, 1, "RSL_LITE_PACK_PERIOD", "local_communicator_periodic" ) ;  
    fprintf(fp,"END IF\n") ;

    close_the_file(fp) ;
  }
  return(0) ;
}

int
gen_swaps ( char * dirname , node_t * swaps )
{
  node_t * p, * q ;
  node_t * dimd ;
  char commname[NAMELEN] ;
  char fname[NAMELEN] ;
  char tmp[NAMELEN], tmp2[NAMELEN], tmp3[NAMELEN] ;
  char commuse[NAMELEN] ;
  FILE * fp ;
  char * t1, * t2 ;
  char * pos1 , * pos2 ;
  char indices[NAMELEN], post[NAMELEN] ;
  int zdex ;
  int n2dR, n3dR ;
  int n2dI, n3dI ;
  int n2dD, n3dD ;
  int n4d ;
  int i, xy ;
#define MAX_4DARRAYS 1000
  char name_4d[MAX_4DARRAYS][NAMELEN] ;

  if ( dirname == NULL ) return(1) ;

  for ( p = swaps ; p != NULL ; p = p->next )
  {
    strcpy( commname, p->name ) ;
    make_upper_case(commname) ;
    if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s.inc",dirname,commname) ; }
    else                       { sprintf(fname,"%s.inc",commname) ; }
    if ((fp = fopen( fname , "w" )) == NULL ) 
    {
      fprintf(stderr,"WARNING: gen_swaps in registry cannot open %s for writing\n",fname ) ;
      continue ; 
    }
    print_warning(fp,fname) ;

  for ( xy = 0 ; xy < 2 ; xy++ ) {

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
       { fprintf(stderr,"unparseable description for period %s\n", commname ) ; continue ; }
      t2 = strtok_rentr(NULL,",", &pos2) ;
      while ( t2 != NULL )
      {
        if ((q = get_entry_r( t2, commuse, Domain.fields )) == NULL )
          { fprintf(stderr,"WARNING 1 : %s in swap spec %s (%s) is not defined in registry.\n",t2,commname, commuse) ; }
        else
        {
          if      (  strcmp( q->type->name, "real") && strcmp( q->type->name, "integer") && strcmp( q->type->name, "doubleprecision") )
            { fprintf(stderr,"WARNING: only type 'real', 'doubleprecision', or 'integer' can be part of swaps exchange. %s in %s is %s\n",t2,commname,q->type->name) ; }
          else if ( q->boundary_array )
            { fprintf(stderr,"WARNING: boundary array %s cannot be member of swaps spec %s.\n",t2,commname) ; }
          else
          {
            if ( q->node_kind & FOURD ) {
              if ( n4d < MAX_4DARRAYS ) {
                strcpy( name_4d[n4d], q->name ) ;
              } else { 
                fprintf(stderr,"REGISTRY ERROR: too many 4d arrays (> %d).\n", MAX_4DARRAYS ) ;
                fprintf(stderr,"That seems like a lot, but if you are sure, increase MAX_4DARRAYS\n" ) ;
                fprintf(stderr,"in external/RSL_LITE/gen_comms.c and recompile\n") ;
                exit(5) ;
              }
              n4d++ ;
            }
            else
            {
              if        ( ! strcmp( q->type->name, "real") ) {
                if         ( q->ndims == 3 )      { n3dR++ ; }
	        else    if ( q->ndims == 2 )      { n2dR++ ; }
	      } else if ( ! strcmp( q->type->name, "integer") ) {
                if         ( q->ndims == 3 )      { n3dI++ ; }
	        else    if ( q->ndims == 2 )      { n2dI++ ; }
	      } else if ( ! strcmp( q->type->name, "doubleprecision") ) {
                if         ( q->ndims == 3 )      { n3dD++ ; }
	        else    if ( q->ndims == 2 )      { n2dD++ ; }
	      }
	    }
	  }
	}
        t2 = strtok_rentr( NULL , "," , &pos2 ) ;
      }
      t1 = strtok_rentr( NULL , ";" , &pos1 ) ;
    }

    fprintf(fp,"IF ( config_flags%%swap_%c ) THEN\n",(xy==1)?'x':'y') ;

/* generate the init statement for X swap */
    fprintf(fp,"CALL RSL_LITE_INIT_SWAP ( local_communicator, %d , &\n", xy ) ;
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
    fprintf(fp,"      thisdomain_max_halo_width, &\n" ) ;
    fprintf(fp,"      ids, ide, jds, jde, kds, kde,   &\n") ;
    fprintf(fp,"      ips, ipe, jps, jpe, kps, kpe    )\n") ;
/* generate packs prior to stencil exchange  */
    gen_packs( fp, p, 1, xy, 0, "RSL_LITE_PACK_SWAP", "local_communicator" ) ;
/* generate stencil exchange in X */
    fprintf(fp,"   CALL RSL_LITE_SWAP ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )\n") ;
/* generate unpacks after stencil exchange  */
    gen_packs( fp, p, 1, xy, 1, "RSL_LITE_PACK_SWAP", "local_communicator" ) ;

    fprintf(fp,"END IF\n") ;

  }
    close_the_file(fp) ;
  }
  return(0) ;
}

int
gen_cycles ( char * dirname , node_t * cycles )
{
  node_t * p, * q ;
  node_t * dimd ;
  char commname[NAMELEN] ;
  char fname[NAMELEN] ;
  char tmp[NAMELEN], tmp2[NAMELEN], tmp3[NAMELEN] ;
  char commuse[NAMELEN] ;
  FILE * fp ;
  char * t1, * t2 ;
  char * pos1 , * pos2 ;
  char indices[NAMELEN], post[NAMELEN] ;
  int zdex ;
  int n2dR, n3dR ;
  int n2dI, n3dI ;
  int n2dD, n3dD ;
  int n4d ;
  int i, xy, inout ;
#define MAX_4DARRAYS 1000
  char name_4d[MAX_4DARRAYS][NAMELEN] ;

  if ( dirname == NULL ) return(1) ;

  for ( p = cycles ; p != NULL ; p = p->next )
  {
    strcpy( commname, p->name ) ;
    make_upper_case(commname) ;
    if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s.inc",dirname,commname) ; }
    else                       { sprintf(fname,"%s.inc",commname) ; }
    if ((fp = fopen( fname , "w" )) == NULL ) 
    {
      fprintf(stderr,"WARNING: gen_cycles in registry cannot open %s for writing\n",fname ) ;
      continue ; 
    }

    /* get inout */
    inout = 0 ;
    strcpy( tmp, p->comm_define ) ;
    t1 = strtok_rentr( tmp , ";" , &pos1 ) ;
    strcpy( tmp2 , t1 ) ;
    if (( t2 = strtok_rentr( tmp2 , ":" , &pos2 )) == NULL )
       { fprintf(stderr,"unparseable description for cycle %s\n", commname ) ; exit(1) ; }
    inout = atoi (t2) ;

    print_warning(fp,fname) ;

  for ( xy = 0 ; xy < 2 ; xy++ ) {

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
       { fprintf(stderr,"unparseable description for period %s\n", commname ) ; continue ; }
      t2 = strtok_rentr(NULL,",", &pos2) ;
      while ( t2 != NULL )
      {
        if ((q = get_entry_r( t2, commuse, Domain.fields )) == NULL )
          { fprintf(stderr,"WARNING 1 : %s in cycle spec %s (%s) is not defined in registry.\n",t2,commname, commuse) ; }
        else
        {
          if      (  strcmp( q->type->name, "real") && strcmp( q->type->name, "integer") && strcmp( q->type->name, "doubleprecision") )
            { fprintf(stderr,"WARNING: only type 'real', 'doubleprecision', or 'integer' can be part of cycles exchange. %s in %s is %s\n",t2,commname,q->type->name) ; }
          else if ( q->boundary_array )
            { fprintf(stderr,"WARNING: boundary array %s cannot be member of cycles spec %s.\n",t2,commname) ; }
          else
          {
            if ( q->node_kind & FOURD ) {
              if ( n4d < MAX_4DARRAYS ) {
                strcpy( name_4d[n4d], q->name ) ;
              } else { 
                fprintf(stderr,"REGISTRY ERROR: too many 4d arrays (> %d).\n", MAX_4DARRAYS ) ;
                fprintf(stderr,"That seems like a lot, but if you are sure, increase MAX_4DARRAYS\n" ) ;
                fprintf(stderr,"in external/RSL_LITE/gen_comms.c and recompile\n") ;
                exit(5) ;
              }
              n4d++ ;
            }
            else
            {
              if        ( ! strcmp( q->type->name, "real") ) {
                if         ( q->ndims == 3 )      { n3dR++ ; }
	        else    if ( q->ndims == 2 )      { n2dR++ ; }
	      } else if ( ! strcmp( q->type->name, "integer") ) {
                if         ( q->ndims == 3 )      { n3dI++ ; }
	        else    if ( q->ndims == 2 )      { n2dI++ ; }
	      } else if ( ! strcmp( q->type->name, "doubleprecision") ) {
                if         ( q->ndims == 3 )      { n3dD++ ; }
	        else    if ( q->ndims == 2 )      { n2dD++ ; }
	      }
	    }
	  }
	}
        t2 = strtok_rentr( NULL , "," , &pos2 ) ;
      }
      t1 = strtok_rentr( NULL , ";" , &pos1 ) ;
    }

    fprintf(fp,"IF ( config_flags%%cycle_%c ) THEN\n",(xy==1)?'x':'y') ;

/* generate the init statement for X swap */
    fprintf(fp,"CALL RSL_LITE_INIT_CYCLE ( local_communicator, %d , %d, &\n", xy, inout ) ;
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
    fprintf(fp,"      thisdomain_max_halo_width,               &\n") ;
    fprintf(fp,"      ids, ide, jds, jde, kds, kde,   &\n") ;
    fprintf(fp,"      ips, ipe, jps, jpe, kps, kpe    )\n") ;
/* generate packs prior to stencil exchange  */
    gen_packs( fp, p, inout, xy, 0, "RSL_LITE_PACK_CYCLE", "local_communicator" ) ;
/* generate stencil exchange in X */
    fprintf(fp,"   CALL RSL_LITE_CYCLE ( local_communicator , mytask, ntasks, ntasks_x, ntasks_y )\n") ;
/* generate unpacks after stencil exchange  */
    gen_packs( fp, p, inout, xy, 1, "RSL_LITE_PACK_CYCLE", "local_communicator" ) ;

    fprintf(fp,"END IF\n") ;

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
  char post[NAMELEN], varname[NAMELEN], memord[10] ;
  char indices_z[NAMELEN], varref_z[NAMELEN] ;
  char indices_x[NAMELEN], varref_x[NAMELEN] ;
  char indices_y[NAMELEN], varref_y[NAMELEN] ;

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

      strcpy( tmp, p->comm_define ) ;
      strcpy( commuse, p->use ) ;
      t1 = strtok_rentr( tmp , ";" , &pos1 ) ;
      while ( t1 != NULL )
      {
        strcpy( tmp2 , t1 ) ;

/* Z array */
        t2 = strtok_rentr(tmp2,",", &pos2) ;
        if ((q = get_entry_r( t2, commuse, Domain.fields )) == NULL )    
         { fprintf(stderr,"WARNING 3 : %s in xpose spec %s (%s) is not defined in registry.\n",t2,commname,commuse) ; goto skiperific ; }
        strcpy( varref_z, t2 ) ;
        if ( q->node_kind & FIELD  && ! (q->node_kind & I1) ) {
           sprintf(varref_z,"grid%%%s",t2) ;
        }
        if ( q->proc_orient != ALL_Z_ON_PROC ) 
         { fprintf(stderr,"WARNING: %s in xpose spec %s is not ALL_Z_ON_PROC.\n",t2,commname) ; goto skiperific ; }
        if ( q->ndims != 3 )
         { fprintf(stderr,"WARNING: array %s must be 3D to be member of xpose spec %s.\n",t2,commname) ; goto skiperific ; }
        if ( q->boundary_array )
         { fprintf(stderr,"WARNING: boundary array %s cannot be member of xpose spec %s.\n",t2,commname) ; goto skiperific ; }
        strcpy (indices_z,"");
        if ( sw_deref_kludge &&  strchr (t2, '%') != NULLCHARPTR )
        {
          sprintf(post,")") ;
          sprintf(indices_z, "%s",index_with_firstelem("(","",-1,tmp3,q,post)) ;
        }
        if ( q->node_kind & FOURD ) {
           strcat( varref_z, "(grid%sm31,grid%sm32,grid%sm33,itrace )" ) ;
        }

/* X array */
        t2 = strtok_rentr( NULL , "," , &pos2 ) ;
        if ((q = get_entry_r( t2, commuse, Domain.fields )) == NULL )    
         { fprintf(stderr,"WARNING 4 : %s in xpose spec %s (%s) is not defined in registry.\n",t2,commname,commuse) ; goto skiperific ; }
        strcpy( varref_x, t2 ) ;
        if ( q->node_kind & FIELD  && ! (q->node_kind & I1) ) {
           sprintf(varref_x,"grid%%%s",t2) ;
        }
        if ( q->proc_orient != ALL_X_ON_PROC ) 
         { fprintf(stderr,"WARNING: %s in xpose spec %s is not ALL_X_ON_PROC.\n",t2,commname) ; goto skiperific ; }
        if ( q->ndims != 3 )
         { fprintf(stderr,"WARNING: array %s must be 3D to be member of xpose spec %s.\n",t2,commname) ; goto skiperific ; }
        if ( q->boundary_array )
         { fprintf(stderr,"WARNING: boundary array %s cannot be member of xpose spec %s.\n",t2,commname) ; goto skiperific ; }
        strcpy (indices_x,"");
        if ( sw_deref_kludge &&  strchr (t2, '%') != NULLCHARPTR )
        {
          sprintf(post,")") ;
          sprintf(indices_x, "%s",index_with_firstelem("(","",-1,tmp3,q,post)) ;
        }
        if ( q->node_kind & FOURD ) {
           strcat( varref_x, "(grid%sm31x,grid%sm32x,grid%sm33x,itrace )" ) ;
        }

/* Y array */
        t2 = strtok_rentr( NULL , "," , &pos2 ) ;
        if ((q = get_entry_r( t2, commuse, Domain.fields )) == NULL )    
         { fprintf(stderr,"WARNING 5 : %s in xpose spec %s (%s)is not defined in registry.\n",t2,commname,commuse) ; goto skiperific ; }
        strcpy( varref_y, t2 ) ;
        if ( q->node_kind & FIELD  && ! (q->node_kind & I1) ) {
           sprintf(varref_y,"grid%%%s",t2) ;
        }
        if ( q->proc_orient != ALL_Y_ON_PROC ) 
         { fprintf(stderr,"WARNING: %s in xpose spec %s is not ALL_Y_ON_PROC.\n",t2,commname) ; goto skiperific ; }
        if ( q->ndims != 3 )
         { fprintf(stderr,"WARNING: array %s must be 3D to be member of xpose spec %s.\n",t2,commname) ; goto skiperific ; }
        if ( q->boundary_array )
         { fprintf(stderr,"WARNING: boundary array %s cannot be member of xpose spec %s.\n",t2,commname) ; goto skiperific ; }
        strcpy (indices_y,"");
        if ( sw_deref_kludge &&  strchr (t2, '%') != NULLCHARPTR )
        {
          sprintf(post,")") ;
          sprintf(indices_y, "%s",index_with_firstelem("(","",-1,tmp3,q,post)) ;
        }
        if ( q->node_kind & FOURD ) {
           strcat( varref_y, "(grid%sm31y,grid%sm32y,grid%sm33y,itrace )" ) ;
        }

        t1 = strtok_rentr( NULL , ";" , &pos1 ) ;
      }
      set_mem_order( q, memord , 3 ) ;
      if        ( !strcmp( *x , "z2x" ) ) {
        fprintf(fp,"  call trans_z2x ( ntasks_x, local_communicator_x, 1, RWORDSIZE, IWORDSIZE, DATA_ORDER_%s , &\n", memord ) ;
        fprintf(fp,"                   %s, &  ! variable in Z decomp\n" , varref_z  ) ;
        fprintf(fp,"                   grid%%sd31, grid%%ed31, grid%%sd32, grid%%ed32, grid%%sd33, grid%%ed33, &\n"         ) ;
        fprintf(fp,"                   grid%%sp31, grid%%ep31, grid%%sp32, grid%%ep32, grid%%sp33, grid%%ep33, &\n"         ) ;
        fprintf(fp,"                   grid%%sm31, grid%%em31, grid%%sm32, grid%%em32, grid%%sm33, grid%%em33, &\n"         ) ;
        fprintf(fp,"                   %s, &  ! variable in X decomp\n" , varref_x  ) ;
        fprintf(fp,"                   grid%%sp31x, grid%%ep31x, grid%%sp32x, grid%%ep32x, grid%%sp33x, grid%%ep33x, &\n"   ) ;
        fprintf(fp,"                   grid%%sm31x, grid%%em31x, grid%%sm32x, grid%%em32x, grid%%sm33x, grid%%em33x ) \n"   ) ;
      } else if ( !strcmp( *x , "x2z" ) ) {
        fprintf(fp,"  call trans_z2x ( ntasks_x, local_communicator_x, 0, RWORDSIZE, IWORDSIZE, DATA_ORDER_%s , &\n", memord ) ;
        fprintf(fp,"                   %s, &  ! variable in Z decomp\n" , varref_z  ) ;
        fprintf(fp,"                   grid%%sd31, grid%%ed31, grid%%sd32, grid%%ed32, grid%%sd33, grid%%ed33, &\n"         ) ;
        fprintf(fp,"                   grid%%sp31, grid%%ep31, grid%%sp32, grid%%ep32, grid%%sp33, grid%%ep33, &\n"         ) ;
        fprintf(fp,"                   grid%%sm31, grid%%em31, grid%%sm32, grid%%em32, grid%%sm33, grid%%em33, &\n"         ) ;
        fprintf(fp,"                   %s, &  ! variable in X decomp\n" , varref_x  ) ;
        fprintf(fp,"                   grid%%sp31x, grid%%ep31x, grid%%sp32x, grid%%ep32x, grid%%sp33x, grid%%ep33x, &\n"   ) ;
        fprintf(fp,"                   grid%%sm31x, grid%%em31x, grid%%sm32x, grid%%em32x, grid%%sm33x, grid%%em33x ) \n"   ) ;
      } else if ( !strcmp( *x , "x2y" ) ) {
        fprintf(fp,"  call trans_x2y ( ntasks_y, local_communicator_y, 1, RWORDSIZE, IWORDSIZE, DATA_ORDER_%s , &\n", memord ) ;
        fprintf(fp,"                   %s, &  ! variable in X decomp\n" , varref_x  ) ;
        fprintf(fp,"                   grid%%sd31, grid%%ed31, grid%%sd32, grid%%ed32, grid%%sd33, grid%%ed33, &\n"         ) ;
        fprintf(fp,"                   grid%%sp31x, grid%%ep31x, grid%%sp32x, grid%%ep32x, grid%%sp33x, grid%%ep33x, &\n"   ) ;
        fprintf(fp,"                   grid%%sm31x, grid%%em31x, grid%%sm32x, grid%%em32x, grid%%sm33x, grid%%em33x, &\n"   ) ;
        fprintf(fp,"                   %s, &  ! variable in Y decomp\n" , varref_y  ) ;
        fprintf(fp,"                   grid%%sp31y, grid%%ep31y, grid%%sp32y, grid%%ep32y, grid%%sp33y, grid%%ep33y, &\n"   ) ;
        fprintf(fp,"                   grid%%sm31y, grid%%em31y, grid%%sm32y, grid%%em32y, grid%%sm33y, grid%%em33y ) \n"   ) ;
      } else if ( !strcmp( *x , "y2x" ) ) {
        fprintf(fp,"  call trans_x2y ( ntasks_y, local_communicator_y, 0, RWORDSIZE, IWORDSIZE, DATA_ORDER_%s , &\n", memord ) ;
        fprintf(fp,"                   %s, &  ! variable in X decomp\n" , varref_x  ) ;
        fprintf(fp,"                   grid%%sd31, grid%%ed31, grid%%sd32, grid%%ed32, grid%%sd33, grid%%ed33, &\n"         ) ;
        fprintf(fp,"                   grid%%sp31x, grid%%ep31x, grid%%sp32x, grid%%ep32x, grid%%sp33x, grid%%ep33x, &\n"   ) ;
        fprintf(fp,"                   grid%%sm31x, grid%%em31x, grid%%sm32x, grid%%em32x, grid%%sm33x, grid%%em33x, &\n"   ) ;
        fprintf(fp,"                   %s, &  ! variable in Y decomp\n" , varref_y  ) ;
        fprintf(fp,"                   grid%%sp31y, grid%%ep31y, grid%%sp32y, grid%%ep32y, grid%%sp33y, grid%%ep33y, &\n"   ) ;
        fprintf(fp,"                   grid%%sm31y, grid%%em31y, grid%%sm32y, grid%%em32y, grid%%sm33y, grid%%em33y ) \n"   ) ;
      } else if ( !strcmp( *x , "y2z" ) ) {
        fprintf(fp,"  call trans_x2y ( ntasks_y, local_communicator_y, 0, RWORDSIZE, IWORDSIZE, DATA_ORDER_%s , &\n", memord ) ;
        fprintf(fp,"                   %s, &  ! variable in X decomp\n" , varref_x  ) ;
        fprintf(fp,"                   grid%%sd31, grid%%ed31, grid%%sd32, grid%%ed32, grid%%sd33, grid%%ed33, &\n"         ) ;
        fprintf(fp,"                   grid%%sp31x, grid%%ep31x, grid%%sp32x, grid%%ep32x, grid%%sp33x, grid%%ep33x, &\n"   ) ;
        fprintf(fp,"                   grid%%sm31x, grid%%em31x, grid%%sm32x, grid%%em32x, grid%%sm33x, grid%%em33x, &\n"   ) ;
        fprintf(fp,"                   %s, &  ! variable in Y decomp\n" , varref_y  ) ;
        fprintf(fp,"                   grid%%sp31y, grid%%ep31y, grid%%sp32y, grid%%ep32y, grid%%sp33y, grid%%ep33y, &\n"   ) ;
        fprintf(fp,"                   grid%%sm31y, grid%%em31y, grid%%sm32y, grid%%em32y, grid%%sm33y, grid%%em33y ) \n"   ) ;
        fprintf(fp,"  call trans_z2x ( ntasks_x, local_communicator_x, 0, RWORDSIZE, IWORDSIZE, DATA_ORDER_%s , &\n", memord ) ;
        fprintf(fp,"                   %s, &  ! variable in Z decomp\n" , varref_z  ) ;
        fprintf(fp,"                   grid%%sd31, grid%%ed31, grid%%sd32, grid%%ed32, grid%%sd33, grid%%ed33, &\n"         ) ;
        fprintf(fp,"                   grid%%sp31, grid%%ep31, grid%%sp32, grid%%ep32, grid%%sp33, grid%%ep33, &\n"         ) ;
        fprintf(fp,"                   grid%%sm31, grid%%em31, grid%%sm32, grid%%em32, grid%%sm33, grid%%em33, &\n"         ) ;
        fprintf(fp,"                   %s, &  ! variable in X decomp\n" , varref_x  ) ;
        fprintf(fp,"                   grid%%sp31x, grid%%ep31x, grid%%sp32x, grid%%ep32x, grid%%sp33x, grid%%ep33x, &\n"   ) ;
        fprintf(fp,"                   grid%%sm31x, grid%%em31x, grid%%sm32x, grid%%em32x, grid%%sm33x, grid%%em33x)\n"   ) ;
      } else if ( !strcmp( *x , "z2y" ) ) {
        fprintf(fp,"  call trans_z2x ( ntasks_x, local_communicator_x, 1, RWORDSIZE, IWORDSIZE, DATA_ORDER_%s , &\n", memord ) ;
        fprintf(fp,"                   %s, &  ! variable in Z decomp\n" , varref_z  ) ;
        fprintf(fp,"                   grid%%sd31, grid%%ed31, grid%%sd32, grid%%ed32, grid%%sd33, grid%%ed33, &\n"         ) ;
        fprintf(fp,"                   grid%%sp31, grid%%ep31, grid%%sp32, grid%%ep32, grid%%sp33, grid%%ep33, &\n"         ) ;
        fprintf(fp,"                   grid%%sm31, grid%%em31, grid%%sm32, grid%%em32, grid%%sm33, grid%%em33, &\n"         ) ;
        fprintf(fp,"                   %s, &  ! variable in X decomp\n" , varref_x  ) ;
        fprintf(fp,"                   grid%%sp31x, grid%%ep31x, grid%%sp32x, grid%%ep32x, grid%%sp33x, grid%%ep33x, &\n"   ) ;
        fprintf(fp,"                   grid%%sm31x, grid%%em31x, grid%%sm32x, grid%%em32x, grid%%sm33x, grid%%em33x )\n"   ) ;
        fprintf(fp,"  call trans_x2y ( ntasks_y, local_communicator_y, 1, RWORDSIZE, IWORDSIZE, DATA_ORDER_%s , &\n", memord ) ;
        fprintf(fp,"                   %s, &  ! variable in X decomp\n" , varref_x  ) ;
        fprintf(fp,"                   grid%%sd31, grid%%ed31, grid%%sd32, grid%%ed32, grid%%sd33, grid%%ed33, &\n"         ) ;
        fprintf(fp,"                   grid%%sp31x, grid%%ep31x, grid%%sp32x, grid%%ep32x, grid%%sp33x, grid%%ep33x, &\n"   ) ;
        fprintf(fp,"                   grid%%sm31x, grid%%em31x, grid%%sm32x, grid%%em32x, grid%%sm33x, grid%%em33x, &\n"   ) ;
        fprintf(fp,"                   %s, &  ! variable in Y decomp\n" , varref_y  ) ;
        fprintf(fp,"                   grid%%sp31y, grid%%ep31y, grid%%sp32y, grid%%ep32y, grid%%sp33y, grid%%ep33y, &\n"   ) ;
        fprintf(fp,"                   grid%%sm31y, grid%%em31y, grid%%sm32y, grid%%em32y, grid%%sm33y, grid%%em33y ) \n"   ) ;
      }

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



int
gen_shift (  char * dirname )
{
  int i ;
  FILE * fp ;
  node_t *p, *q, *dimd ;
  char **direction ;
  char *directions[] = { "x", "y", 0L } ;
  char fname[NAMELEN], vname[NAMELEN] ;
  char indices[NAMELEN], post[NAMELEN], tmp3[NAMELEN] ;
  char memord[NAMELEN] ;
  int xdex,ydex,zdex ;
  node_t Shift ;
int said_it = 0 ;
int said_it2 = 0 ;

  for ( direction = directions ; *direction != NULL ; direction++ )
  {
    if ( dirname == NULL ) return(1) ;
    if ( sw_unidir_shift_halo ) {
       sprintf(fname,"shift_halo") ; /* SamT: bug fix: remove extra arg */
    } else {
       sprintf(fname,"shift_halo_%s_halo",*direction) ;
    }

    Shift.next = NULL ;
    sprintf( Shift.use, "" ) ;
    strcpy( Shift.comm_define, "SHW:" ) ;
    strcpy( Shift.name , fname ) ;
    if ( sw_move ) {
    for ( p = Domain.fields ; p != NULL ; p = p->next ) {
      if (( p->node_kind & (FIELD | FOURD) ) && p->ndims >= 2 && ! p->boundary_array )
      {

/* special cases in WRF */
if ( !strcmp( p->name , "xf_ens" ) || !strcmp( p->name , "pr_ens" ) ||
     !strcmp( p->name , "abstot" ) || !strcmp( p->name , "absnxt" ) ||
     !strcmp( p->name , "emstot" ) || !strcmp( p->name , "obs_savwt" ) ) {
  if ( sw_move && ! said_it ) { fprintf(stderr,"Info only - not an error: Moving nests not implemented for Grell Ens. Cumulus\n") ;
                                fprintf(stderr,"Info only - not an error: Moving nests not implemented for CAM radiation\n") ;
                                fprintf(stderr,"Info only - not an error: Moving nests not implemented for Observation Nudging\n") ;
  said_it = 1 ; }
  continue ;
}

/* make sure that the only things we are shifting are arrays that have a decomposed X and a Y dimension */
/* also make sure we don't shift or halo any transpose variables (ALL_X_ON_PROC or ALL_Y_ON_PROC) */
        if ( get_dimnode_for_coord( p , COORD_X ) && get_dimnode_for_coord( p , COORD_Y ) && 
             !(p->proc_orient == ALL_X_ON_PROC || p->proc_orient == ALL_Y_ON_PROC) ) {
          
if ( p->subgrid != 0 ) {  /* moving nests not implemented for subgrid variables */
  if ( sw_move && ! said_it2 ) { fprintf(stderr,"Info only - not an error: Moving nests not implemented for subgrid variables \n") ;
  said_it2 = 1 ; }
  continue ;
}
          if ( p->type->type_type == SIMPLE )
          {
            for ( i = 1 ; i <= p->ntl ; i++ )
            {
              if ( p->ntl > 1 ) sprintf(vname,"%s_%d",p->name,i ) ;
              else              sprintf(vname,"%s",p->name ) ;
              strcat( Shift.comm_define, vname ) ;
              strcat( Shift.comm_define, "," ) ;
            }
          }
        }
      }
    }
    if ( strlen(Shift.comm_define) > 0 )Shift.comm_define[strlen(Shift.comm_define)-1] = '\0' ;
    }

/* if unidir halo, then only generate on x pass */
    if ( ! ( sw_unidir_shift_halo && !strcmp(*direction,"y" ) ) ) {
      gen_halos( dirname , NULL, &Shift, 0 ) ;
    }

    sprintf(fname,"%s/shift_halo_%s.inc",dirname,*direction) ;
    if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;

/* now generate the shifts themselves */
    if ( sw_move ) {
    for ( p = Domain.fields ; p != NULL ; p = p->next )
    {

/* special cases in WRF */
if ( !strcmp( p->name , "xf_ens" ) || !strcmp( p->name , "pr_ens" ) ||
     !strcmp( p->name , "abstot" ) || !strcmp( p->name , "absnxt" ) ||
     !strcmp( p->name , "emstot" ) || !strcmp( p->name , "obs_savwt" ) ) {
  continue ;
}
/* do not shift transpose variables */
if ( p->proc_orient == ALL_X_ON_PROC || p->proc_orient == ALL_Y_ON_PROC ) continue ;

      if (( p->node_kind & (FIELD | FOURD) ) && p->ndims >= 2 && ! p->boundary_array )
      {

	if ( p->type->type_type == SIMPLE )
	{
	  for ( i = 1 ; i <= p->ntl ; i++ )
	  {
            
            if ( p->ntl > 1 ) sprintf(vname,"%s_%d",p->name,i ) ;
            else              sprintf(vname,"%s",p->name ) ;

	    if ( p->node_kind & FOURD )
            {
              node_t *member ;

              xdex = get_index_for_coord( p , COORD_X ) ;
              ydex = get_index_for_coord( p , COORD_Y ) ;
              zdex = get_index_for_coord( p , COORD_Z ) ;
              if ( zdex >=1 && zdex <= 3 )
              {
                int d ; 
                char r[10], tx[80], temp[80], moredims[80], *colon ;
                set_mem_order( p->members, memord , 3 ) ;
fprintf(fp, "  DO itrace = PARAM_FIRST_SCALAR, num_%s\n", p->name ) ;
                for ( d = p->ndims-1; d >= 3 ; d-- ) {
                  strcpy(r,"") ;
                  range_of_dimension( r, tx, d, p, "config_flags%") ;
                  colon = index(tx,':') ; *colon = ',' ;
fprintf(fp, "  DO idim%d = %s\n", d-2, tx ) ;
                }
                strcpy(moredims,"") ;
                for ( d = 3 ; d < p->ndims ; d++ ) {
                  sprintf(temp,"idim%d",d-2) ;
                  strcat(moredims,",") ; strcat(moredims,temp) ;
                }
                strcat(moredims,",") ;
                if ( !strcmp( *direction, "x" ) )
                {
                  char * stag = "" ;
                  stag = p->members->stag_x?"":"-1" ;
                  if        ( !strncmp( memord , "XYZ", 3 ) ) {
                    fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
                    fprintf(fp,"grid%%%s (ips:min(ide%s,ipe),jms:jme,:%sitrace) = grid%%%s (ips+px:min(ide%s,ipe)+px,jms:jme,:%sitrace)\n", vname, stag, moredims, vname, stag, moredims ) ;
                    fprintf(fp,"ENDIF\n") ;
                  } else if ( !strncmp( memord , "YXZ", 3 ) ) {
                    fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
                    fprintf(fp,"grid%%%s (jms:jme,ips:min(ide%s,ipe),:%sitrace) = grid%%%s (jms:jme,ips+px:min(ide%s,ipe)+px,:%sitrace)\n", vname, stag, moredims, vname, stag, moredims ) ;
                    fprintf(fp,"ENDIF\n") ;
                  } else if ( !strncmp( memord , "XZY", 3 ) ) {
                    fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
                    fprintf(fp,"grid%%%s (ips:min(ide%s,ipe),:,jms:jme%sitrace) = grid%%%s (ips+px:min(ide%s,ipe)+px,:,jms:jme%sitrace)\n", vname, stag, moredims, vname, stag, moredims ) ;
                    fprintf(fp,"ENDIF\n") ;
                  } else if ( !strncmp( memord , "YZX", 3 ) ) {
                    fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
                    fprintf(fp,"grid%%%s (jms:jme,:,ips:min(ide%s,ipe)%sitrace) = grid%%%s (jms:jme,:,ips+px:min(ide%s,ipe)+px%sitrace)\n", vname, stag, moredims, vname, stag, moredims ) ;
                    fprintf(fp,"ENDIF\n") ;
                  } else if ( !strncmp( memord , "ZXY", 3 ) ) {
                    fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
                    fprintf(fp,"grid%%%s (:,ips:min(ide%s,ipe),jms:jme%sitrace) = grid%%%s (:,ips+px:min(ide%s,ipe)+px,jms:jme%sitrace)\n", vname, stag, moredims, vname, stag, moredims ) ;
                    fprintf(fp,"ENDIF\n") ;
                  } else if ( !strncmp( memord , "ZYX", 3 ) ) {
                    fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
                    fprintf(fp,"grid%%%s (:,jms:jme,ips:min(ide%s,ipe)%sitrace) = grid%%%s (:,jms:jme,ips+px:min(ide%s,ipe)+px%sitrace)\n", vname, stag, moredims, vname, stag, moredims ) ;
                    fprintf(fp,"ENDIF\n") ;
                  } else if ( !strncmp( memord , "XY", 2 ) ) {
                    fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
                    fprintf(fp,"grid%%%s (ips:min(ide%s,ipe),jms:jme%sitrace) = grid%%%s (ips+px:min(ide%s,ipe)+px,jms:jme%sitrace)\n", vname, stag, moredims, vname, stag, moredims ) ;
                    fprintf(fp,"ENDIF\n") ;
                  } else if ( !strncmp( memord , "YX", 2 ) ) {
                    fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
                    fprintf(fp,"grid%%%s (jms:jme,ips:min(ide%s,ipe)%sitrace) = grid%%%s (jms:jme,ips+px:min(ide%s,ipe)+px%sitrace)\n", vname, stag, moredims, vname, stag, moredims ) ;
                    fprintf(fp,"ENDIF\n") ;
                  }
                }
                else
                {
                  char * stag = "" ;
                  stag = p->members->stag_y?"":"-1" ;
                  if        ( !strncmp( memord , "XYZ", 3 ) ) {
                    fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
	            fprintf(fp,"grid%%%s (ims:ime,jps:min(jde%s,jpe),:%sitrace) = grid%%%s (ims:ime,jps+py:min(jde%s,jpe)+py,:%sitrace)\n", vname, stag, moredims, vname, stag, moredims ) ;
                    fprintf(fp,"ENDIF\n") ;
                  } else if ( !strncmp( memord , "YXZ", 3 ) ) {
                    fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
	            fprintf(fp,"grid%%%s (jps:min(jde%s,jpe),ims:ime,:%sitrace) = grid%%%s (jps+py:min(jde%s,jpe)+py,ims:ime,:%sitrace)\n", vname, stag, moredims, vname, stag, moredims ) ;
                    fprintf(fp,"ENDIF\n") ;
                  } else if ( !strncmp( memord , "XZY", 3 ) ) {
                    fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
	            fprintf(fp,"grid%%%s (ims:ime,:,jps:min(jde%s,jpe)%sitrace) = grid%%%s (ims:ime,:,jps+py:min(jde%s,jpe)+py%sitrace)\n", vname, stag, moredims, vname, stag, moredims ) ;
                    fprintf(fp,"ENDIF\n") ;
                  } else if ( !strncmp( memord , "YZX", 3 ) ) {
                    fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
	            fprintf(fp,"grid%%%s (jps:min(jde%s,jpe),:,ims:ime%sitrace) = grid%%%s (jps+py:min(jde%s,jpe)+py,:,ims:ime%sitrace)\n", vname, stag, moredims, vname, stag, moredims ) ;
                    fprintf(fp,"ENDIF\n") ;
                  } else if ( !strncmp( memord , "ZXY", 3 ) ) {
                    fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
	            fprintf(fp,"grid%%%s (:,ims:ime,jps:min(jde%s,jpe)%sitrace) = grid%%%s (:,ims:ime,jps+py:min(jde%s,jpe)+py%sitrace)\n", vname, stag, moredims, vname, stag, moredims ) ;
                    fprintf(fp,"ENDIF\n") ;
                  } else if ( !strncmp( memord , "ZYX", 3 ) ) {
                    fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
	            fprintf(fp,"grid%%%s (:,jps:min(jde%s,jpe),ims:ime%sitrace) = grid%%%s (:,jps+py:min(jde%s,jpe)+py,ims:ime%sitrace)\n", vname, stag, moredims, vname, stag, moredims ) ;
                    fprintf(fp,"ENDIF\n") ;
                  } else if ( !strncmp( memord , "XY", 2 ) ) {
                    fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
	            fprintf(fp,"grid%%%s (ims:ime,jps:min(jde%s,jpe)%sitrace) = grid%%%s (ims:ime,jps+py:min(jde%s,jpe)+py%sitrace)\n", vname, stag, moredims, vname, stag, moredims ) ;
                    fprintf(fp,"ENDIF\n") ;
                  } else if ( !strncmp( memord , "YX", 2 ) ) {
                    fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
	            fprintf(fp,"grid%%%s (jps:min(jde%s,jpe),ims:ime%sitrace) = grid%%%s (jps+py:min(jde%s,jpe)+py,ims:ime%sitrace)\n", vname, stag, moredims, vname, stag, moredims ) ;
                    fprintf(fp,"ENDIF\n") ;
                  }
                }
                for ( d = p->ndims-1; d >= 3 ; d-- ) {
fprintf(fp, "  ENDDO\n" ) ;
                }
fprintf(fp, "  ENDDO\n" ) ;
              }
              else
              {
                fprintf(stderr,"WARNING: %d some dimension info missing for 4d array %s\n",zdex,t2) ;
              }
            }
            else
	    {
              xdex = get_index_for_coord( p , COORD_X ) ;
              ydex = get_index_for_coord( p , COORD_Y ) ;
              set_mem_order( p, memord , 3 ) ;
              if ( !strcmp( *direction, "x" ) ) {
                if        ( !strcmp( memord , "XYZ" ) ) {
                  fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
                  fprintf(fp,"grid%%%s (ips:min(ide%s,ipe),jms:jme,:) = grid%%%s (ips+px:min(ide%s,ipe)+px,jms:jme,:)\n", vname,  p->stag_x?"":"-1", vname, p->stag_x?"":"-1" ) ;
                  fprintf(fp,"ENDIF\n") ;
                } else if ( !strcmp( memord , "YXZ" ) ) {
                  fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
                  fprintf(fp,"grid%%%s (jms:jme,ips:min(ide%s,ipe),:) = grid%%%s (jms:jme,ips+px:min(ide%s,ipe)+px,:)\n", vname,  p->stag_x?"":"-1", vname, p->stag_x?"":"-1" ) ;
                  fprintf(fp,"ENDIF\n") ;
                } else if ( !strcmp( memord , "XZY" ) ) {
                  fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
                  fprintf(fp,"grid%%%s (ips:min(ide%s,ipe),:,jms:jme) = grid%%%s (ips+px:min(ide%s,ipe)+px,:,jms:jme)\n", vname,  p->stag_x?"":"-1", vname, p->stag_x?"":"-1" ) ;
                  fprintf(fp,"ENDIF\n") ;
                } else if ( !strcmp( memord , "YZX" ) ) {
                  fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
                  fprintf(fp,"grid%%%s (jms:jme,:,ips:min(ide%s,ipe)) = grid%%%s (jms:jme,:,ips+px:min(ide%s,ipe)+px)\n", vname,  p->stag_x?"":"-1", vname, p->stag_x?"":"-1" ) ;
                  fprintf(fp,"ENDIF\n") ;
                } else if ( !strcmp( memord , "ZXY" ) ) {
                  fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
                  fprintf(fp,"grid%%%s (:,ips:min(ide%s,ipe),jms:jme) = grid%%%s (:,ips+px:min(ide%s,ipe)+px,jms:jme)\n", vname,  p->stag_x?"":"-1", vname, p->stag_x?"":"-1" ) ;
                  fprintf(fp,"ENDIF\n") ;
                } else if ( !strcmp( memord , "ZYX" ) ) {
                  fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
                  fprintf(fp,"grid%%%s (:,jms:jme,ips:min(ide%s,ipe)) = grid%%%s (:,jms:jme,ips+px:min(ide%s,ipe)+px)\n", vname,  p->stag_x?"":"-1", vname, p->stag_x?"":"-1" ) ;
                  fprintf(fp,"ENDIF\n") ;
                } else if ( !strcmp( memord , "XY" ) ) {
                  fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
                  fprintf(fp,"grid%%%s (ips:min(ide%s,ipe),jms:jme) = grid%%%s (ips+px:min(ide%s,ipe)+px,jms:jme)\n", vname,  p->stag_x?"":"-1", vname, p->stag_x?"":"-1" ) ;
                  fprintf(fp,"ENDIF\n") ;
                } else if ( !strcmp( memord , "YX" ) ) {
                  fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
                  fprintf(fp,"grid%%%s (jms:jme,ips:min(ide%s,ipe)) = grid%%%s (jms:jme,ips+px:min(ide%s,ipe)+px)\n", vname,  p->stag_x?"":"-1", vname, p->stag_x?"":"-1" ) ;
                  fprintf(fp,"ENDIF\n") ;
                }
              } else {
                if        ( !strcmp( memord , "XYZ" ) ) {
                  fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
	          fprintf(fp,"grid%%%s (ims:ime,jps:min(jde%s,jpe),:) = grid%%%s (ims:ime,jps+py:min(jde%s,jpe)+py,:)\n", vname, p->stag_y?"":"-1", vname, p->stag_y?"":"-1" ) ;
                  fprintf(fp,"ENDIF\n") ;
                } else if ( !strcmp( memord , "YXZ" ) ) {
                  fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
	          fprintf(fp,"grid%%%s (jps:min(jde%s,jpe),ims:ime,:) = grid%%%s (jps+py:min(jde%s,jpe)+py,ims:ime,:)\n", vname, p->stag_y?"":"-1", vname, p->stag_y?"":"-1" ) ;
                  fprintf(fp,"ENDIF\n") ;
                } else if ( !strcmp( memord , "XZY" ) ) {
                  fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
	          fprintf(fp,"grid%%%s (ims:ime,:,jps:min(jde%s,jpe)) = grid%%%s (ims:ime,:,jps+py:min(jde%s,jpe)+py)\n", vname, p->stag_y?"":"-1", vname, p->stag_y?"":"-1" ) ;
                  fprintf(fp,"ENDIF\n") ;
                } else if ( !strcmp( memord , "YZX" ) ) {
                  fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
	          fprintf(fp,"grid%%%s (jps:min(jde%s,jpe),:,ims:ime) = grid%%%s (jps+py:min(jde%s,jpe)+py,:,ims:ime)\n", vname, p->stag_y?"":"-1", vname, p->stag_y?"":"-1" ) ;
                  fprintf(fp,"ENDIF\n") ;
                } else if ( !strcmp( memord , "ZXY" ) ) {
                  fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
	          fprintf(fp,"grid%%%s (:,ims:ime,jps:min(jde%s,jpe)) = grid%%%s (:,ims:ime,jps+py:min(jde%s,jpe)+py)\n", vname, p->stag_y?"":"-1", vname, p->stag_y?"":"-1" ) ;
                  fprintf(fp,"ENDIF\n") ;
                } else if ( !strcmp( memord , "ZYX" ) ) {
                  fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
	          fprintf(fp,"grid%%%s (:,jps:min(jde%s,jpe),ims:ime) = grid%%%s (:,jps+py:min(jde%s,jpe)+py,ims:ime)\n", vname, p->stag_y?"":"-1", vname, p->stag_y?"":"-1" ) ;
                  fprintf(fp,"ENDIF\n") ;
                } else if ( !strcmp( memord , "XY" ) ) {
                  fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
	          fprintf(fp,"grid%%%s (ims:ime,jps:min(jde%s,jpe)) = grid%%%s (ims:ime,jps+py:min(jde%s,jpe)+py)\n", vname, p->stag_y?"":"-1", vname, p->stag_y?"":"-1" ) ;
                  fprintf(fp,"ENDIF\n") ;
                } else if ( !strcmp( memord , "YX" ) ) {
                  fprintf(fp,"IF ( SIZE(grid%%%s,%d)*SIZE(grid%%%s,%d) .GT. 1 ) THEN\n",vname,xdex+1,vname,ydex+1) ; 
	          fprintf(fp,"grid%%%s (jps:min(jde%s,jpe),ims:ime) = grid%%%s (jps+py:min(jde%s,jpe)+py,ims:ime)\n", vname, p->stag_y?"":"-1", vname, p->stag_y?"":"-1" ) ;
                  fprintf(fp,"ENDIF\n") ;
                }
              }
            }
	  }
	}
      }
    }
    } /* if sw_move */
    close_the_file(fp) ;
  }
  return 0; /* SamT: bug fix: return a value */
}

int
gen_datacalls ( char * dirname )
{
  FILE * fp ;
  char * fn = "data_calls.inc" ;
  char fname[NAMELEN] ;

  if ( dirname == NULL ) return(1) ;
  if ( strlen(dirname) > 0 )
   { sprintf(fname,"%s/%s",dirname,fn) ; }
  else
   { sprintf(fname,"%s",fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;
  close_the_file(fp) ;
  return(0) ;
}

/*****************/
/*****************/

int
gen_nest_packing ( char * dirname )
{
  gen_nest_pack( dirname ) ;
  gen_nest_unpack( dirname ) ;
  return 0; /* SamT: bug fix: return a value */
}

#define PACKIT 1
#define UNPACKIT 2

int
gen_nest_pack ( char * dirname )
{
  int i ;
  FILE * fp ;
  char * fnlst[] = { "nest_interpdown_pack.inc" , "nest_forcedown_pack.inc" , "nest_feedbackup_pack.inc", 0L } ;
  int down_path[] = { INTERP_DOWN , FORCE_DOWN , INTERP_UP } ;
  int ipath ;
  char ** fnp ; char * fn ;
  char * parent ;
  char * shw_str ;
  char fname[NAMELEN] ;
  node_t *node, *p, *dim ;
  int xdex, ydex, zdex ;
  char ddim[3][2][NAMELEN] ;
  char mdim[3][2][NAMELEN] ;
  char pdim[3][2][NAMELEN] ;
  char vname[NAMELEN] ; char tag[NAMELEN], fourd_names[NAMELEN_LONG] ;
  int d2, d3, sw ;
  char *info_name ;
  int d2_mp, d3_mp;
  char fourd_names_mp[NAMELEN_LONG];

  for ( fnp = fnlst , ipath = 0 ; *fnp ; fnp++ , ipath++ )
  {
    fn = *fnp ;
      if ( dirname == NULL ) return(1) ;
      if ( strlen(dirname) > 0 ) {
        sprintf(fname,"%s/%s",dirname,fn) ;
      } else { 
        sprintf(fname,"%s",fn) ;
      }
      if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
      print_warning(fp,fname) ;

      d2 = d2_mp = 0 ;
      d3 = d3_mp = 0 ;
      node = Domain.fields ;

#if (NMM_CORE==1)
      count_fields ( node, &d2,    &d3,    fourd_names,    down_path[ipath], 0, 1);
      count_fields ( node, &d2_mp, &d3_mp, fourd_names_mp, down_path[ipath], 1, 0);
#else
      count_fields ( node , &d2 , &d3 , fourd_names, down_path[ipath] ,0,0) ;
#endif
      parent= "" ;
      if ( !strcmp(fn,"nest_feedbackup_pack.inc") ) parent="parent_" ; 

      if ( d2 + d3 > 0 ) {
        if ( down_path[ipath] == INTERP_UP )
        {
          info_name = "rsl_lite_to_parent_info" ;
          sw = 0 ;
        }
        else
        {
          info_name = "rsl_lite_to_child_info" ;
          sw = 1 ;
        }

        fprintf(fp,"msize = (%d + %s )* nlev + %d\n", d3, fourd_names, d2 ) ;
#if (NMM_CORE==1)
        fprintf(fp,"IF(interp_mp .eqv. .true.) then\n"
                "    msize=msize + (%d + %s )*nlev+%d\n"
                "ENDIF\n",
                d3_mp,fourd_names_mp,d2_mp);
#endif

/*        fprintf(fp,"CALL %s( local_communicator, msize*RWORDSIZE                               &\n",info_name ) ;  */
        fprintf(fp,"CALL %s( msize*RWORDSIZE                               &\n",info_name ) ;
        fprintf(fp,"                        ,cips,cipe,cjps,cjpe                               &\n") ;
if (sw) fprintf(fp,"                        ,iids,iide,ijds,ijde                               &\n") ;
        fprintf(fp,"                        ,nids,nide,njds,njde                               &\n") ;
if (sw) fprintf(fp,"                        ,pgr , sw                                          &\n") ;
        fprintf(fp,"                        ,nest_task_offsets(ngrid%%id)                      &\n") ;
        fprintf(fp,"                        ,nest_pes_x(%sgrid%%id)                            &\n",parent) ;
        fprintf(fp,"                        ,nest_pes_y(%sgrid%%id)                            &\n",parent) ; 
        fprintf(fp,"                        ,nest_pes_x(intermediate_grid%%id)                 &\n") ;
        fprintf(fp,"                        ,nest_pes_y(intermediate_grid%%id)                 &\n") ; 
        fprintf(fp,"                        ,thisdomain_max_halo_width                         &\n") ;
        fprintf(fp,"                        ,icoord,jcoord                                     &\n") ;
        fprintf(fp,"                        ,idim_cd,jdim_cd                                   &\n") ;
        fprintf(fp,"                        ,pig,pjg,retval )\n") ;

        fprintf(fp,"DO while ( retval .eq. 1 )\n") ;
  
        gen_nest_packunpack ( fp , Domain.fields, PACKIT, down_path[ipath] ) ;

/*        fprintf(fp,"CALL %s( local_communicator, msize*RWORDSIZE                               &\n",info_name ) ; */
        fprintf(fp,"CALL %s( msize*RWORDSIZE                               &\n",info_name ) ;
        fprintf(fp,"                        ,cips,cipe,cjps,cjpe                               &\n") ;
if (sw) fprintf(fp,"                        ,iids,iide,ijds,ijde                               &\n") ;
        fprintf(fp,"                        ,nids,nide,njds,njde                               &\n") ;
if (sw) fprintf(fp,"                        ,pgr , sw                                          &\n") ;
        fprintf(fp,"                        ,nest_task_offsets(ngrid%%id)                      &\n") ;
        fprintf(fp,"                        ,nest_pes_x(%sgrid%%id)                            &\n",parent) ;
        fprintf(fp,"                        ,nest_pes_y(%sgrid%%id)                            &\n",parent) ; 
        fprintf(fp,"                        ,nest_pes_x(intermediate_grid%%id)                 &\n") ;
        fprintf(fp,"                        ,nest_pes_y(intermediate_grid%%id)                 &\n") ; 
        fprintf(fp,"                        ,thisdomain_max_halo_width                         &\n") ;
        fprintf(fp,"                        ,icoord,jcoord                                     &\n") ;
        fprintf(fp,"                        ,idim_cd,jdim_cd                                   &\n") ;
        fprintf(fp,"                        ,pig,pjg,retval )\n") ;

        fprintf(fp,"ENDDO\n") ;
      }
      close_the_file(fp) ;
  }
  return(0) ;
}

int
gen_nest_unpack ( char * dirname )
{
  int i ;
  FILE * fp ;
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
  char *info_name ;
  char vname[NAMELEN] ; char tag[NAMELEN] ; char fourd_names[NAMELEN_LONG] ;
  int d2, d3 ;

  for ( fnp = fnlst , ipath = 0 ; *fnp ; fnp++ , ipath++ )
  {
    fn = *fnp ;
      d2 = 0 ;
      d3 = 0 ;
      node = Domain.fields ;

      if ( dirname == NULL ) return(1) ;
      if ( strlen(dirname) > 0 )
       { sprintf(fname,"%s/%s",dirname,fn) ; }
      else
       { sprintf(fname,"%s",fn) ; }
      if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
      print_warning(fp,fname) ;

      count_fields ( node , &d2 , &d3 , fourd_names, down_path[ipath], 0, 0 ) ;

      if ( d2 + d3 > 0 || strlen(fourd_names) > 0 ) {
        if ( down_path[ipath] == INTERP_UP )
        {
          info_name = "rsl_lite_from_child_info" ;
        }
        else
        {
          info_name = "rsl_lite_from_parent_info" ;
        }

        fprintf(fp,"CALL %s(pig,pjg,retval)\n", info_name ) ;
        fprintf(fp,"DO while ( retval .eq. 1 )\n") ;
#if (NMM_CORE == 1)
        if(down_path[ipath]==INTERP_UP) {
          fprintf(fp,"feedback_flag=cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. )\n");
          fprintf(fp,"feedback_flag_v=cd_feedback_mask_v( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, .FALSE., .FALSE. )\n");
        }
#endif
        gen_nest_packunpack ( fp , Domain.fields, UNPACKIT, down_path[ipath] ) ;
        fprintf(fp,"CALL %s(pig,pjg,retval)\n", info_name ) ;
        fprintf(fp,"ENDDO\n") ;
      }
      close_the_file(fp) ;
  }
  return(0) ;
}

int
gen_nest_packunpack ( FILE *fp , node_t * node , int dir, int down_path )
{
  int i, d1 ;
  node_t *p, *p1, *dim ;
  int d2, d3, xdex, ydex, zdex ;
  int nest_mask ;
  char * grid ; 
  const char * feed="NEST_INFLUENCE";
  char ddim[3][2][NAMELEN] ;
  char mdim[3][2][NAMELEN] ;
  char pdim[3][2][NAMELEN] ;
  char vname[NAMELEN], dexes[NAMELEN] ; char tag[NAMELEN] ; 
  char tx[80], moredims[80], temp[80], r[10], *colon ;
  char c, d ;
  int need_endif;

  need_endif=0;
  for ( p1 = node ;  p1 != NULL ; p1 = p1->next )
  {
      if(need_endif) {
        fprintf(fp,"endif\n");
        need_endif=0;
      }
    if ( p1->node_kind & FOURD )
    {
      if ( p1->members->next )
        nest_mask = p1->members->next->nest_mask ;
      else
        continue ;
    }
    else
    {
      nest_mask = p1->nest_mask ;
    }
    p = p1 ;

    if ( nest_mask & down_path && ! ( down_path==INTERP_UP && p->no_feedback ) )
    {
          if(p->mp_var) {
            fprintf(fp,"if(interp_mp .eqv. .true.) then\n");
            need_endif=1;
          }
        if ( p->node_kind & FOURD ) {
          if ( p->members->next->ntl > 1 ) sprintf(tag,"_2") ;
          else                             sprintf(tag,"") ;
          set_dim_strs ( p->members , ddim , mdim , pdim , "c", 0 ) ;
          zdex = get_index_for_coord( p->members , COORD_Z ) ;
          xdex = get_index_for_coord( p->members , COORD_X ) ;
          ydex = get_index_for_coord( p->members , COORD_Y ) ;
        } else {
          if ( p->ntl > 1 ) sprintf(tag,"_2") ;
          else              sprintf(tag,"") ;
          set_dim_strs ( p , ddim , mdim , pdim , "c", 0 ) ;
          zdex = get_index_for_coord( p , COORD_Z ) ;
          xdex = get_index_for_coord( p , COORD_X ) ;
          ydex = get_index_for_coord( p , COORD_Y ) ;
        }

        if ( down_path == INTERP_UP )
        {
          c = ( dir == PACKIT )?'n':'p' ;
          d = ( dir == PACKIT )?'2':'1' ;
        } else {
          c = ( dir == UNPACKIT )?'n':'p' ;
          d = ( dir == UNPACKIT )?'2':'1' ;
        }

        if ( zdex >= 0 ) {
          if      ( xdex == 0 && zdex == 1 && ydex == 2 )  sprintf(dexes,"pig,k,pjg") ;
          else if ( zdex == 0 && xdex == 1 && ydex == 2 )  sprintf(dexes,"k,pig,pjg") ;
          else if ( xdex == 0 && ydex == 1 && zdex == 2 )  sprintf(dexes,"pig,pjg,k") ;
        } else {
          if ( xdex == 0 && ydex == 1 )  sprintf(dexes,"pig,pjg") ;
          if ( ydex == 0 && xdex == 1 )  sprintf(dexes,"pjg,pig") ;
        }

        /* construct variable name */
        if ( p->node_kind & FOURD )
        {
          strcpy(moredims,"") ;
          for ( d1 = 3 ; d1 < p->ndims ; d1++ ) {
             sprintf(temp,"idim%d",d1-2) ;
             strcat(moredims,",") ; strcat(moredims,temp) ;
          }
          strcat(moredims,",") ;
          sprintf(vname,"%s%s(%s%sitrace)",p->name,tag,dexes,moredims) ;
        }
        else
        {
          sprintf(vname,"%s%s(%s)",p->name,tag,dexes) ;
        }

        grid = "grid%" ;
        if ( p->node_kind & FOURD )
	{
           grid = "" ;
fprintf(fp,"DO itrace =  PARAM_FIRST_SCALAR, num_%s\n", p->name) ;
           for ( d1 = p->ndims-1 ; d1 >= 3 ; d1-- ) {
             strcpy(r,"") ;
             range_of_dimension(r, tx, d1, p, "config_flags%" ) ;
             colon = index( tx, ':' ) ; *colon = ',' ; 
fprintf(fp,"DO idim%d = %s \n", d1-2, tx) ;
           }
        } else {
/* note that in the case if dir != UNPACKIT and down_path == INTERP_UP the data
   structure being used is intermediate_grid, not grid. However, intermediate_grid
   and grid share the same id (see module_dm.F) so it will not make a difference. */
#if 0
fprintf(fp,"IF ( in_use_for_config(grid%%id,'%s%s') ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c\n",p->name,tag) ; 
#else
fprintf(fp,"IF ( SIZE(%s%s%s) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c\n",grid,p->name,tag) ; 
#endif
	}

        if ( dir == UNPACKIT ) 
        {
          if ( down_path == INTERP_UP )
	  {
            char *sjl = "" ;
                  if (p->nmm_v_grid)
                    sjl = "_v" ;
            if ( zdex >= 0 ) {
fprintf(fp,"CALL rsl_lite_from_child_msg(((%s)-(%s)+1)*RWORDSIZE,xv) ;\n",ddim[zdex][1], ddim[zdex][0] ) ;
            } else {
fprintf(fp,"CALL rsl_lite_from_child_msg(RWORDSIZE,xv)\n" ) ;
            }
#if (NMM_CORE == 1)
                  if(p->stag_x || p->stag_y) {
#endif
fprintf(fp,"IF ( cd_feedback_mask%s( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, %s, %s ) ) THEN\n",
                 sjl ,
                 p->stag_x?".TRUE.":".FALSE." ,p->stag_y?".TRUE.":".FALSE." ) ;
#if ( NMM_CORE == 1)
                  } else {
                    fprintf(fp,"IF(feedback_flag%s) THEN\n",sjl);
                  }
#endif

#if ( NMM_CORE == 1)
            if ( node->full_feedback ) {
                feed="NEST_FULL_INFLUENCE";
            } else {
                feed="NEST_INFLUENCE";
            }
#endif
            if ( zdex >= 0 ) {
fprintf(fp,"DO k = %s,%s\n%s(%s%s,xv(k))\nENDDO\n", ddim[zdex][0], ddim[zdex][1], feed, grid, vname ) ;
            } else {
fprintf(fp,"%s(%s%s,xv(1))\n", feed, grid, vname ) ;
            }
fprintf(fp,"ENDIF\n") ;
          }
          else
          {
            if ( zdex >= 0 ) {
fprintf(fp,"CALL rsl_lite_from_parent_msg(((%s)-(%s)+1)*RWORDSIZE,xv)\nDO k = %s,%s\n%s%s = xv(k)\nENDDO\n",
                                    ddim[zdex][1], ddim[zdex][0], ddim[zdex][0], ddim[zdex][1], grid, vname) ;
            } else {
fprintf(fp,"CALL rsl_lite_from_parent_msg(RWORDSIZE,xv)\n%s%s = xv(1)\n", grid, vname) ;
            }
          }
        }
        else
        {
          if ( down_path == INTERP_UP )
	  {
            if ( zdex >= 0 ) {
fprintf(fp,"DO k = %s,%s\nxv(k)= intermediate_grid%%%s\nENDDO\nCALL rsl_lite_to_parent_msg(((%s)-(%s)+1)*RWORDSIZE,xv)\n",
                           ddim[zdex][0], ddim[zdex][1], vname, ddim[zdex][1], ddim[zdex][0] ) ;
            } else {
fprintf(fp,"xv(1)= intermediate_grid%%%s\nCALL rsl_lite_to_parent_msg(RWORDSIZE,xv)\n", vname) ;
            }
          }
          else
          {
            if ( zdex >= 0 ) {
fprintf(fp,"DO k = %s,%s\nxv(k)= %s%s\nENDDO\nCALL rsl_lite_to_child_msg(((%s)-(%s)+1)*RWORDSIZE,xv)\n",
                           ddim[zdex][0], ddim[zdex][1], grid, vname, ddim[zdex][1], ddim[zdex][0] ) ;
            } else {
fprintf(fp,"xv(1)=%s%s\nCALL rsl_lite_to_child_msg(RWORDSIZE,xv)\n", grid, vname) ;
            }
          }
        }
        if ( p->node_kind & FOURD )
	{
           for ( d1 = p->ndims-1 ; d1 >= 3 ; d1-- ) {
fprintf(fp,"ENDDO\n") ;
           }
fprintf(fp,"ENDDO\n") ;
	}
        else
	{
fprintf(fp,"ENDIF\n") ; /* in_use_for_config */
	}
    }
  }
  if(need_endif) {
    fprintf(fp,"endif\n");
    need_endif=0;
  }

  return(0) ;
}

/*****************/

/* STOPPED HERE -- need to include the extra dimensions in the count */

int
count_fields ( node_t * node , int * d2 , int * d3 ,  char * fourd_names, int down_path,
               int send_mp, int no_mp )
{
  node_t * p ;
  int zdex ;
  char temp[80], r[10], tx[80], *colon ;
  int d ;

  strcpy(fourd_names,"") ;  /* only works if non-recursive, but that is ifdefd out below */
/* count up the total number of levels from all fields */
  for ( p = node ;  p != NULL ; p = p->next )
  {
      if(send_mp && !p->mp_var) continue;
      if(no_mp && p->mp_var) continue;
    if ( p->node_kind == FOURD ) 
    {
#if 0
          count_fields( p->members , d2 , d3 , down_path, send_mp, no_mp ) ;  /* RECURSE */
#else
      if ( strlen(fourd_names) > 0 ) strcat(fourd_names," & \n + ") ;
      sprintf(temp,"((num_%s - PARAM_FIRST_SCALAR + 1)",p->name) ;
      strcat(fourd_names,temp) ;
      for ( d = 3 ; d < p->ndims ; d++ ) {
        strcpy(r,"") ;
        range_of_dimension(r,tx,d,p,"config_flags%") ;
        colon = index(tx,':') ; *colon = '\0' ;
        sprintf(temp," &\n  *((%s)-(%s)+1)",colon+1,tx) ;
        strcat(fourd_names,temp) ;
      }
      strcat(fourd_names,")") ;
#endif
    }
    else
    {
      if ( p->nest_mask & down_path )
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
  return(0) ;
}

/*****************/
/*****************/

int
gen_debug (  char * dirname )
{
  int i ;
  FILE * fp ;
  node_t *p, *q, *dimd ;
  char **direction ;
  char *directions[] = { "x", "y", 0L } ;
  char fname[NAMELEN], vname[NAMELEN] ;
  char indices[NAMELEN], post[NAMELEN], tmp3[NAMELEN] ;
  int zdex ;
  node_t Shift ;
int said_it = 0 ;
int said_it2 = 0 ;

    if ( dirname == NULL ) return(1) ;

    if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/debuggal.inc",dirname) ; }
    else                       { sprintf(fname,"debuggal.inc") ; }
    if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;

/* now generate the shifts themselves */
    for ( p = Domain.fields ; p != NULL ; p = p->next )
    {

/* special cases in WRF */
if ( !strcmp( p->name , "xf_ens" ) || !strcmp( p->name , "pr_ens" ) ||
     !strcmp( p->name , "abstot" ) || !strcmp( p->name , "absnxt" ) ||
     !strcmp( p->name , "emstot" ) || !strcmp( p->name , "obs_savwt" ) ) {
  continue ;
}

      if (( p->node_kind & (FIELD | FOURD) ) && p->ndims >= 2 && ! p->boundary_array )
      {

	if ( p->type->type_type == SIMPLE )
	{
	  for ( i = 1 ; i <= p->ntl ; i++ )
	  {
            
            if ( p->ntl > 1 ) sprintf(vname,"%s_%d",p->name,i ) ;
            else              sprintf(vname,"%s",p->name ) ;

	    if ( p->node_kind & FOURD  )
            {
#if 0
              node_t *member ;
              zdex = get_index_for_coord( p , COORD_Z ) ;
              if ( zdex >=1 && zdex <= 3 && strncmp(vname,"fdda",4)  )
              {
fprintf(fp, "  DO itrace = PARAM_FIRST_SCALAR, num_%s\n", p->name ) ;
fprintf(fp, "   write(0,*) AAA_AAA,BBB_BBB, '%s ', itrace , %s ( IDEBUG,KDEBUG,JDEBUG,itrace)\n", vname, vname ) ;
fprintf(fp, "  ENDDO\n" ) ;
              }
              else
              {
                fprintf(stderr,"WARNING: %d some dimension info missing for 4d array %s\n",zdex,t2) ;
              }
#endif
            }
            else
	    {
	      if ( p->ndims == 3 ) {
fprintf(fp, "   write(0,*) AAA_AAA,BBB_BBB, '%s ', grid%%%s ( IDEBUG,KDEBUG,JDEBUG)\n", vname, vname ) ;
              } else if ( p->ndims == 2 ) {
fprintf(fp, "   write(0,*) AAA_AAA,BBB_BBB, '%s ', grid%%%s ( IDEBUG,JDEBUG)\n", vname, vname ) ;
              }
            }
	  }
	}
      }
    }

    close_the_file(fp) ;
    return 0; /* SamT: bug fix: return a value */
}

/*****************/
/*****************/

int
gen_comms ( char * dirname )
{
  FILE *fpsub ; 
  if ( sw_dm_parallel )
    fprintf(stderr,"ADVISORY: RSL_LITE version of gen_comms is linked in with registry program.\n") ;

  /* truncate this file if it exists */
  if ((fpsub = fopen( "inc/REGISTRY_COMM_NESTING_DM_subs.inc" , "w" )) != NULL ) fclose(fpsub) ;
  if ((fpsub = fopen( "inc/REGISTRY_COMM_DM_subs.inc" , "w" )) != NULL ) fclose(fpsub) ;
  if ((fpsub = fopen( "inc/REGISTRY_COMM_DM_0_subs.inc" , "w" )) != NULL ) fclose(fpsub) ;
  if ((fpsub = fopen( "inc/REGISTRY_COMM_DM_1_subs.inc" , "w" )) != NULL ) fclose(fpsub) ;
  if ((fpsub = fopen( "inc/REGISTRY_COMM_DM_2_subs.inc" , "w" )) != NULL ) fclose(fpsub) ;
  if ((fpsub = fopen( "inc/REGISTRY_COMM_DM_3_subs.inc" , "w" )) != NULL ) fclose(fpsub) ;

  gen_halos( "inc" , NULL, Halos, 1 ) ;
  gen_shift( "inc" ) ;
  gen_periods( "inc", Periods ) ;
  gen_swaps( "inc", Swaps ) ;
  gen_cycles( "inc", Cycles ) ;
  gen_xposes( "inc" ) ;
  gen_comm_descrips( "inc" ) ;
  gen_datacalls( "inc" ) ;
  gen_nest_packing( "inc" ) ;
#if 0
  gen_debug( "inc" ) ;
#endif

  return(0) ;
}

