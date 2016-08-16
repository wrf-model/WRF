#include <stdio.h>


#include "protos.h"
#include "protos_kpp.h"
#include "kpp_data.h"

#define DEBUGR 0

int
write_makefiles_kpp (  char* kpp_dirname, char * kpp_version  )
{
  knode_t * p1, * p2;

char kname[NAMELEN];
char mfname0[NAMELEN], mfname[NAMELEN];
FILE * mfile;


    strcpy( mfname0, "Makefile" );

   for ( p1 =   KPP_packs  ; p1 != NULL ; p1 = p1->next ) {
     p2 = p1->assoc_wrf_pack;
     if ( p2 ) {
 
       strcpy( kname, p1->name );
       sprintf( mfname, "%s/%s/%s",  kpp_dirname, kname, mfname0  );

 
       mfile = fopen (  mfname, "w" );

       /* fprintf(mfile," hh %s\n", mfname); */
         
       gen_kpp_warning(mfile, "tools/write_makefiles_kpp.c","#" );
       fprintf(mfile,"$(MODEL):\t$(MODEL).kpp\n");
       fprintf(mfile,"\t\t${KPP_HOME}/bin/kpp");

       fclose(mfile);
     }
   } 


 exit (0); 
  return(0) ; 

}
