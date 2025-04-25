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

// Helper macro to actually do return checks
#define EXIT_ON_NONZERO( A ) { int result = A; if ( result != 0 ) { printf( "Error in %s, zero return expected, received %i\n", #A, result ); exit(result); } }

/* SamT: bug fix: main returns int */
int
main( int argc, char *argv[], char *env[] )
{
  char fname_in[NAMELEN], dir[NAMELEN], fname_tmp[NAMELEN], command[NAMELEN] ;
  char fname_wrk[NAMELEN] ;
  FILE * fp_in, *fp_tmp ;
  char * thisprog  ;
  char *env_val ;
  int mypid ;
  int do_irr_diag ;
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

  EXIT_ON_NONZERO( gen_io_boilerplate() );  /* 20091213 jm.  Generate the io_boilerplate_temporary.inc file */

  EXIT_ON_NONZERO( init_parser() );
  EXIT_ON_NONZERO( init_type_table() );
  EXIT_ON_NONZERO( init_dim_table() );
//
//  possible IRR diagnostcis?
//
  do_irr_diag = 0;
  env_val = getenv( "WRF_CHEM" );
  if( env_val != NULL && !strncmp( env_val, "1", 1 ) ) {
    env_val = getenv( "WRF_KPP" );
    if( env_val != NULL && !strncmp( env_val, "1", 1 ) ) do_irr_diag = 1; 
  }
  if( do_irr_diag ) {
    if( access( fname_in,F_OK ) ) {
      fprintf(stderr,"Registry program %s does not exist. Ending.\n", fname_in ) ;
      exit(2) ;
    }
    { char *e ;
      strcpy( dir , fname_in ) ;
      if ( ( e = rindex ( dir , '/' ) ) != NULL ) { *e = '\0' ; } else { strcpy( dir, "." ) ; } 
      sprintf( fname_wrk,"%s/Registry_irr_diag",dir ) ;
    }
//  fprintf(stderr,"Registry tmp file = %s\n",fname_wrk);
    sprintf(command,"/bin/cp %s %s\n",fname_in,fname_wrk);
//  fprintf(stderr,"Command = %s\n",command);
    if( system( command ) ) {
      fprintf(stderr,"Could not copy %s to %s\n",fname_in,fname_wrk);
      exit(2) ;
    }
    if (( fp_tmp = fopen( fname_wrk , "a" )) == NULL )
    {
      fprintf(stderr,"Registry program cannot open %s for appending. Ending.\n", fname_tmp ) ;
      exit(2) ;
    }
    if( !access( "Registry/registry.irr_diag",F_OK ) ) {
      sprintf(command,"/bin/rm -f Registry/registry.irr_diag\n");
      if( system( command ) ) {
        fprintf(stderr,"Could not remove Registry/registry.irr_diag\n");
        exit(2) ;
      }
    }
    {
      int ndx = 0;
      int retcod;
      retcod = AppendReg( "mozcart",ndx );
      if( !retcod ) ndx++;
      retcod = AppendReg( "t1_mozcart",ndx );
      if( !retcod ) ndx++;
      retcod = AppendReg( "mozart_mosaic_4bin",ndx );
      if( !retcod ) ndx++;
      retcod = AppendReg( "mozart_mosaic_4bin_aq",ndx );
    }

    fprintf(fp_tmp,"\n");
    fprintf(fp_tmp,"include registry.irr_diag\n");
    fclose(fp_tmp);
    strcpy( fname_in,fname_wrk );
    irr_diag_scalar_indices( "inc" );
//  fprintf(stderr,"fname_in = %s\n",fname_in);
  }

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


  EXIT_ON_NONZERO( reg_parse(fp_tmp) );

  fclose(fp_tmp) ;

  check_dimspecs();

  EXIT_ON_NONZERO( gen_state_struct( "inc" ) );
  EXIT_ON_NONZERO( gen_state_subtypes( "inc" ) );
  EXIT_ON_NONZERO( gen_alloc( "inc" ) );
  /* gen_alloc_count( "inc" ) ; */
  EXIT_ON_NONZERO( gen_dealloc( "inc" ) ) ;
  EXIT_ON_NONZERO( gen_scalar_indices( "inc" ) ) ;
  EXIT_ON_NONZERO( gen_module_state_description( "frame" ) ) ;
  EXIT_ON_NONZERO( gen_actual_args( "inc" ) ) ;
  EXIT_ON_NONZERO( gen_actual_args_new( "inc" ) ) ;
  EXIT_ON_NONZERO( gen_dummy_args( "inc" ) ) ;
  EXIT_ON_NONZERO( gen_dummy_args_new( "inc" ) ) ;
  EXIT_ON_NONZERO( gen_dummy_decls( "inc" ) ) ;
  EXIT_ON_NONZERO( gen_dummy_decls_new( "inc" ) ) ;
  EXIT_ON_NONZERO( gen_i1_decls( "inc" ) ) ;
  EXIT_ON_NONZERO( gen_namelist_statements("inc") ; )
  EXIT_ON_NONZERO( gen_namelist_defines ( "inc", 0 ) ) ;  /* without dimension statements  */
  EXIT_ON_NONZERO( gen_namelist_defines ( "inc", 1 ) ) ;  /* with dimension statements     */
  EXIT_ON_NONZERO( gen_namelist_defaults ( "inc" ) ) ;
  EXIT_ON_NONZERO( gen_namelist_script ( "inc" ) ) ;
  EXIT_ON_NONZERO( gen_get_nl_config( "inc" ) ) ;
  EXIT_ON_NONZERO( gen_config_assigns( "inc" ) ) ;
  EXIT_ON_NONZERO( gen_config_reads( "inc" ) ) ;
  EXIT_ON_NONZERO( gen_wrf_io( "inc" ) ) ;
  EXIT_ON_NONZERO( gen_model_data_ord( "inc" ) ) ;
  EXIT_ON_NONZERO( gen_nest_interp( "inc" ) ) ;
  EXIT_ON_NONZERO( gen_nest_v_interp( "inc") ; ) /*KAL added this for vertical interpolation*/
  EXIT_ON_NONZERO( gen_scalar_derefs( "inc" ) ) ;
  EXIT_ON_NONZERO( gen_streams("inc") ; )

/* this has to happen after gen_nest_interp, which adds halos to the AST */
  EXIT_ON_NONZERO( gen_comms( "inc" ) );    /* this is either package supplied (by copying a */
                                            /* gen_comms.c file into this directory) or a    */
                                            /* stubs routine.                                */

cleanup:
#ifdef _WIN32
   if( do_irr_diag ) {
     sprintf(command,"del /F /Q %s\n",fname_wrk );
     system( command ) ;
   }
   sprintf(command,"del /F /Q %s\n",fname_tmp );
#else
   if( do_irr_diag ) {
     sprintf(command,"/bin/rm -f %s\n",fname_wrk );
     system( command ) ;
   }
   sprintf(command,"/bin/rm -f %s\n",fname_tmp );
#endif
   return system( command ) ;
}

