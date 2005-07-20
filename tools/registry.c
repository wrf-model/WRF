#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

#define DEFINE_GLOBALS
#include "protos.h"
#include "registry.h"
#include "data.h"

main( int argc, char *argv[], char *env[] )
{
  char fname_in[NAMELEN], fname_tmp[NAMELEN], command[NAMELEN] ;
  FILE * fp_in, *fp_tmp ;
  char * thisprog  ;
  char * strcpy() ;
  int mypid ;
  struct rlimit rlim ;

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
  sw_ifort_kludge          = 0 ;
  sw_dm_serial_in_only      = 0 ; /* input and bdy data set is distributed by node 0, 
                                     other data streams are written to file per process */

  strcpy( fname_in , "" ) ;

  rlim.rlim_cur = RLIM_INFINITY ;
  rlim.rlim_max = RLIM_INFINITY ;

  setrlimit ( RLIMIT_STACK , &rlim ) ;

  thisprog = *argv ;
  while (*argv) {
    if (*argv[0] == '-') {  /* an option */
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
      if (!strcmp(*argv,"-DIFORT_KLUDGE")) {
        sw_ifort_kludge = 1 ;
      }
      if (!strcmp(*argv,"-DD3VAR_IRY_KLUDGE")) {
        sw_3dvar_iry_kludge = 1 ;
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

  init_parser() ;
  init_type_table() ;
  init_dim_table() ;
  init_core_table() ;

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

  pre_parse( fp_in, fp_tmp ) ;

  fclose(fp_in) ;
  fclose(fp_tmp) ;

  if (( fp_tmp = fopen( fname_tmp , "r" )) == NULL )
  {
    fprintf(stderr,"Registry program cannot open %s for reading. Ending.\n", fname_tmp ) ;
    exit(2) ;
  }

  reg_parse(fp_tmp) ;

  fclose(fp_tmp) ;

  check_dimspecs() ;

  gen_state_struct( "inc" ) ;
  gen_state_subtypes( "inc" ) ;
  gen_alloc( "inc" ) ;
  gen_dealloc( "inc" ) ;
  gen_scalar_indices( "inc" ) ;
  gen_module_state_description( "frame" ) ;
  gen_actual_args( "inc" ) ;
  gen_dummy_args( "inc" ) ;
  gen_dummy_decls( "inc" ) ;
  gen_i1_decls( "inc" ) ;
  gen_namelist_statements("inc") ;
  gen_namelist_defines ( "inc", 0 ) ;  /* without dimension statements  */
  gen_namelist_defines ( "inc", 1 ) ;  /* with dimension statements     */
  gen_namelist_defaults ( "inc" ) ;
  gen_get_nl_config( "inc" ) ;
  gen_config_assigns( "inc" ) ;
  gen_config_reads( "inc" ) ;
  gen_wrf_io( "inc" ) ;
  gen_model_data_ord( "inc" ) ;
  gen_nest_interp( "inc" ) ;
  gen_scalar_derefs( "inc" ) ;

#if 1
  system( "touch inc/em_nest_feedbackup_smooth.inc" ) ;
  system( "touch inc/em_nest_feedbackup_unpack.inc" ) ;
#endif


/* this has to happen after gen_nest_interp, which adds halos to the AST */
  gen_comms( "inc" ) ;    /* this is either package supplied (by copying a */
                          /* gen_comms.c file into this directory) or a    */
                          /* stubs routine.                                */

cleanup:
  sprintf(command,"/bin/rm -f %s\n",fname_tmp );
  system( command ) ;

}

