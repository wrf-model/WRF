#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

#define DEFINE_GLOBALS
#include "protos.h"
#include "protos_kpp.h"
#include "registry.h"
#include "data.h"

main( int argc, char *argv[] )
{
char fname_in[NAMELEN], fname_tmp[NAMELEN]; 
 char run_flag[NAMELEN];
char * strcpy() ;
int mypid ;
char * thisprog  ;
FILE * fp_in, *fp_tmp ;

mypid = (int) getpid() ;


 
  thisprog = argv[0] ;

 if ( argc != 2 ){
   fprintf(stderr,"Usage: %s filename(Registry), \n",  thisprog);

   exit(1);
  }



   strcpy( fname_in , argv[1] ) ;


  init_parser() ;
  init_type_table() ;
  init_dim_table() ;
  init_core_table() ;



    if (( fp_in = fopen( fname_in , "r" )) == NULL )
    {
      fprintf(stderr,"%s: cannot open %s for reading. Ending.\n", thisprog, fname_in ) ;
      exit(2) ;
    }
  
  sprintf( fname_tmp , "Registry_tmp.%d_wkp",mypid) ;
  if (( fp_tmp = fopen( fname_tmp  , "w" )) == NULL )
  {
    fprintf(stderr,"%s: cannot open temporary %s for writing. Ending.\n", thisprog, fname_tmp ) ;
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


        gen_kpp("inc", "chem/KPP/mechanisms"); 

  /*fprintf(stderr,"hi\n"); */
  fprintf(stderr,"%s\n",fname_in);


 exit(0);


}

