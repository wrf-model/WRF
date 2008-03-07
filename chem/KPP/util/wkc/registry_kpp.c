#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>

#define DEFINE_GLOBALS
#include "protos.h"
#include "protos_kpp.h"
#include "registry.h"
#include "data.h"
#include "sym.h"

main( int argc, char *argv[], char *env[] )
{
  char fname_in[NAMELEN], dir[NAMELEN], fname_tmp[NAMELEN], command[NAMELEN] ;
  FILE * fp_in, *fp_tmp ;
  char * thisprog  ;
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
  sw_fort_kludge          = 0 ;
  sw_dm_serial_in_only      = 0 ; /* input and bdy data set is distributed by node 0, 
                                     other data streams are written to file per process */

  strcpy( fname_in , "" ) ;

  rlim.rlim_cur = RLIM_INFINITY ;
  rlim.rlim_max = RLIM_INFINITY ;

  setrlimit ( RLIMIT_STACK , &rlim ) ;

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
      if (!strcmp(*argv,"-DIFORT_KLUDGE")) {
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
/*  init_core_table() ; */

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


        gen_kpp("inc", "chem/KPP/mechanisms"); 


cleanup:
  sprintf(command,"/bin/rm -f %s\n",fname_tmp );
  system( command ) ;

}

