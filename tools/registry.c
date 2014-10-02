#include <stdio.h>
#include <stdlib.h>
#ifdef _WIN32
# include <io.h>
# define rindex(X,Y) strrchr(X,Y)
# define index(X,Y) strchr(X,Y)
#else
# include <sys/time.h>
# include <sys/resource.h>
# include <unistd.h>
# include <string.h>
# include <strings.h>
#endif

#define DEFINE_GLOBALS
#include "protos.h"
#include "registry.h"
#include "data.h"
#include "sym.h"

/* SamT: bug fix: main returns int */
int
main( int argc, char *argv[], char *env[] )
{
  char fname_in[NAMELEN], dir[NAMELEN], fname_tmp[NAMELEN], command[NAMELEN] ;
  FILE * fp_in, *fp_tmp ;
  char * thisprog  ;
  int mypid ;
#ifndef _WIN32
  struct rlimit rlim ;
#endif

  mypid = (int) getpid() ;
  strcpy( thiscom, argv[0] ) ;
  argv++ ;

  sw_deref_kludge           = 0 ;
  sw_io_deref_kludge        = 0 ;
  sw_3dvar_iry_kludge           = 0 ;
  sw_distrib_io_layer       = 1 ;
  sw_limit_args             = 0 ; /* usually set -- except for GRAPS */
  sw_dm_parallel            = 0 ;
  sw_all_x_staggered       = 0 ;
  sw_move                  = 0 ;
  sw_all_y_staggered       = 0 ;
  sw_fort_kludge          = 1 ;   /* unconditionally true for v3 */
  sw_dm_serial_in_only      = 0 ; /* input and bdy data set is distributed by node 0, 
                                     other data streams are written to file per process */
  sw_new_bdys              = 0 ;
  sw_unidir_shift_halo     = 0 ;

  strcpy( fname_in , "" ) ;

#ifndef _WIN32
  rlim.rlim_cur = RLIM_INFINITY ;
  rlim.rlim_max = RLIM_INFINITY ;
  setrlimit ( RLIMIT_STACK , &rlim ) ;
#endif

  sym_forget() ;
  thisprog = *argv ;
  while (*argv) {
    if (*argv[0] == '-') {  /* an option */
      if (!strncmp(*argv,"-D",2)) {
        char * p ;
        p = *argv ;
        sym_add(p+2) ;
      }
   
      if (!strcmp(*argv,"-DDEREF_KLUDGE")) {
        sw_deref_kludge = 1 ;
      }
      if (!strcmp(*argv,"-DIO_DEREF_KLUDGE")) {
        sw_io_deref_kludge = 1 ;
      }
      if (!strcmp(*argv,"-DLIMIT_ARGS")) {
        sw_limit_args = 1 ;
      }
      if (!strcmp(*argv,"-DMOVE_NESTS")) {
        sw_move = 1 ;
      }
      if (!strcmp(*argv,"-DMOVE_NL_OUTSIDE_MODULE_CONFIGURE")) {
        sw_fort_kludge = 1 ;
      }
      if (!strcmp(*argv,"-DD3VAR_IRY_KLUDGE")) {
#if 0
        sw_3dvar_iry_kludge = 1 ;
#else
        fprintf(stderr,"WARNING: -DD3VAR_IRY_KLUDGE option obsolete (it is now disabled by default). Ignored.\n") ;
#endif
      }
      if (!strcmp(*argv,"-DALL_X_STAGGERED")) {
        sw_all_x_staggered = 1 ;
      }
      if (!strcmp(*argv,"-DALL_Y_STAGGERED")) {
        sw_all_y_staggered = 1 ;
      }
      if (!strcmp(*argv,"-DDM_PARALLEL")) {
        sw_dm_parallel = 1 ;
      }
      if (!strcmp(*argv,"-DNEW_BDYS")) {
        sw_new_bdys = 1 ;
      }
      if (!strcmp(*argv,"-DEM_CORE=1")) {
        sw_unidir_shift_halo = 1 ;
      }
      if (!strcmp(*argv,"-DNEW_WITH_OLD_BDYS")) {
        sw_new_with_old_bdys = 1 ;
      }
      if (!strcmp(*argv,"-DDISTRIB_IO_LAYER")) {
#if 0
        sw_distrib_io_layer = 1 ;
#else
        fprintf(stderr,"WARNING: -DDISTRIB_IO_LAYER option obsolete (it is now default). Ignored.\n") ;
#endif
      }
      if (!strcmp(*argv,"-DDM_SERIAL_IN_ONLY")) {
        sw_dm_serial_in_only = 1 ;
      }
      if (!strncmp(*argv,"-h",2)) {
        fprintf(stderr,"Usage: %s [-DDEREF_KLUDGE] [-DDM_PARALLEL] [-DDISTRIB_IO_LAYER] [-DDM_SERIAL_IN_ONLY] [-DD3VAR_IRY_KLUDGE] registryfile\n",thisprog) ;
        exit(1) ;
      }
    }
    else  /* consider it an input file */
    {
      strcpy( fname_in , *argv ) ;
    }
    argv++ ;
  }

  gen_io_boilerplate() ;  /* 20091213 jm.  Generate the io_boilerplate_temporary.inc file */

  init_parser() ;
  init_type_table() ;
  init_dim_table() ;

  if ( !strcmp(fname_in,"") ) fp_in = stdin ;
  else
    if (( fp_in = fopen( fname_in , "r" )) == NULL )
    {
      fprintf(stderr,"Registry program cannot open %s for reading. Ending.\n", fname_in ) ;
      exit(2) ;
    }
  
  sprintf( fname_tmp , "Registry_tmp.%d",mypid) ;
  if (( fp_tmp = fopen( fname_tmp  , "w" )) == NULL )
  {
    fprintf(stderr,"Registry program cannot open temporary %s for writing. Ending.\n", fname_tmp ) ;
    exit(2) ;
  }

  { char *e ;
    strcpy( dir , fname_in ) ;
    if ( ( e = rindex ( dir , '/' ) ) != NULL ) { *e = '\0' ; } else { strcpy( dir, "." ) ; } 
  }
  if ( pre_parse( dir, fp_in, fp_tmp ) ) {
    fprintf(stderr,"Problem with Registry File %s\n", fname_in ) ;
    goto cleanup ;
  }
  sym_forget() ;

  fclose(fp_in) ;
  fclose(fp_tmp) ;

  if (( fp_tmp = fopen( fname_tmp , "r" )) == NULL )
  {
    fprintf(stderr,"Registry program cannot open %s for reading. Ending.\n", fname_tmp ) ;
    goto cleanup ;
  }


  reg_parse(fp_tmp) ;

  fclose(fp_tmp) ;

  check_dimspecs() ;

  gen_state_struct( "inc" ) ;
  gen_state_subtypes( "inc" ) ;
  gen_alloc( "inc" ) ;
  /* gen_alloc_count( "inc" ) ; */
  gen_dealloc( "inc" ) ;
  gen_scalar_indices( "inc" ) ;
  gen_module_state_description( "frame" ) ;
  gen_actual_args( "inc" ) ;
  gen_actual_args_new( "inc" ) ;
  gen_dummy_args( "inc" ) ;
  gen_dummy_args_new( "inc" ) ;
  gen_dummy_decls( "inc" ) ;
  gen_dummy_decls_new( "inc" ) ;
  gen_i1_decls( "inc" ) ;
  gen_namelist_statements("inc") ;
  gen_namelist_defines ( "inc", 0 ) ;  /* without dimension statements  */
  gen_namelist_defines ( "inc", 1 ) ;  /* with dimension statements     */
  gen_namelist_defaults ( "inc" ) ;
  gen_namelist_script ( "inc" ) ;
  gen_get_nl_config( "inc" ) ;
  gen_config_assigns( "inc" ) ;
  gen_config_reads( "inc" ) ;
  gen_wrf_io( "inc" ) ;
  gen_model_data_ord( "inc" ) ;
  gen_nest_interp( "inc" ) ;
  gen_nest_v_interp( "inc") ; /*KAL added this for vertical interpolation*/
  gen_scalar_derefs( "inc" ) ;
  gen_streams("inc") ;

/* this has to happen after gen_nest_interp, which adds halos to the AST */
  gen_comms( "inc" ) ;    /* this is either package supplied (by copying a */
                          /* gen_comms.c file into this directory) or a    */
                          /* stubs routine.                                */

cleanup:
#ifdef _WIN32
   sprintf(command,"del /F /Q %s\n",fname_tmp );
#else
   sprintf(command,"/bin/rm -f %s\n",fname_tmp );
#endif
   return system( command ) ;
}

