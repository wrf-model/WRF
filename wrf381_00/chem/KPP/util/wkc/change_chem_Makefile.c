#include <stdio.h>


#include "protos.h"
#include "protos_kpp.h"
#include "kpp_data.h"




int
change_chem_Makefile (   )
{
knode_t * p1, * p2, * pm1;
 FILE * ch_Makefile, * t_Makefile; 
 char inln[NAMELEN]; 
 char kname[NAMELEN];
 char * org_Mf = "chem/Makefile_org";
 char * t_Mf = "chem/Makefile.temp";
 char * Mf = "chem/Makefile";
 char cp_command[NAMELEN];
 char sub_string[NAMELEN];
 char *p_string;
 int  slen;
 int  nchem_opts = 0;
 int  chem_opts_cnt;




 ch_Makefile = fopen(org_Mf, "r" );
 t_Makefile = fopen(t_Mf, "w" );


 sprintf(  cp_command,"cp %s %s",t_Mf,Mf);

 fprintf(t_Makefile,"#  \n");
 fprintf(t_Makefile,"# MANUAL CHANGES TO THIS FILE WILL BE LOST \n");
 fprintf(t_Makefile,"#  ... EDIT Makefile_org INSTEAD ...\n");
 fprintf(t_Makefile,"# this file was written by gen_kpp.c \n\n");



  /* loop over lines in chem/Makefile */
   while ( fgets ( inln , NAMELEN , ch_Makefile ) != NULL ){

     /* printf("%s ", inln ); */ 
	  fprintf(t_Makefile, inln);

	  /* if ( strncmp(inln, "MODULES",6) == 0){  */

	  if ( strncmp(inln, "        module_data_sorgam_vbs",29) == 0){

              for ( p1 =   KPP_packs  ; p1 != NULL ; p1 = p1->next ) {
               p2 = p1->assoc_wrf_pack;
               if ( p2 ) {

                  strcpy( kname, p1->name );
                 fprintf(t_Makefile, "  module_kpp_%s_Integr.o \\\n",kname );
                 fprintf(t_Makefile, "  module_kpp_%s_Precision.o \\\n",kname );
                 fprintf(t_Makefile, "  module_kpp_%s_Parameters.o \\\n",kname );                
                 fprintf(t_Makefile, "  module_kpp_%s_Jacobian.o \\\n",kname );
                 fprintf(t_Makefile, "  module_kpp_%s_JacobianSP.o \\\n",kname );
                 fprintf(t_Makefile, "  module_kpp_%s_Update_Rconst.o \\\n",kname );
                 fprintf(t_Makefile, "  module_kpp_%s_interface.o \\\n",kname );
               }
              }
                 fprintf(t_Makefile, "  module_wkppc_constants.o \\\n");
	  }

	  if ( strncmp(inln, "# DEPENDENCIES",14) == 0){

          for ( p1 =   KPP_packs  ; p1 != NULL ; p1 = p1->next,nchem_opts++ ) {

               p2 = p1->assoc_wrf_pack;

               if ( p2 ) {

                  strcpy( kname, p1->name );

                  fprintf(t_Makefile, "module_kpp_%s_Parameters.o:  module_kpp_%s_Precision.o   \n\n",kname, kname );
                  fprintf(t_Makefile, "module_kpp_%s_Update_Rconst.o:  module_kpp_%s_Parameters.o   \n\n",kname, kname );
                  fprintf(t_Makefile, "module_kpp_%s_Jacobian.o:  module_kpp_%s_Parameters.o module_kpp_%s_JacobianSP.o   \n\n",kname, kname, kname );
                  fprintf(t_Makefile, "module_kpp_%s_Integr.o: module_kpp_%s_Parameters.o module_kpp_%s_Jacobian.o module_kpp_%s_JacobianSP.o  module_kpp_%s_Update_Rconst.o  module_wkppc_constants.o  \n\n",kname, kname, kname, kname, kname );
                  fprintf(t_Makefile, "module_kpp_%s_interface.o: module_kpp_%s_Parameters.o module_kpp_%s_Precision.o module_kpp_%s_Integr.o  module_kpp_%s_Update_Rconst.o  module_wkppc_constants.o  \n\n",kname, kname, kname, kname, kname );

               }
          }
 

          fprintf(t_Makefile, "module_wkkpc_constants.o:\n\n");

          p_string = sub_string;
          chem_opts_cnt = 0;
          sprintf( sub_string,"kpp_mechanism_driver.o: " );
          for ( p1 =   KPP_packs  ; p1 != NULL ; p1 = p1->next ) {
               p2 = p1->assoc_wrf_pack;
               if ( p2 ) {
                  chem_opts_cnt++;
                  strcpy( kname, p1->name );
                  slen = strlen( sub_string );
                  sprintf( p_string+slen,"module_kpp_%s_interface.o ",kname );
                  slen = strlen( sub_string );
                  if( slen > 90 ) {
                    if( chem_opts_cnt < nchem_opts ) sprintf( p_string+slen,"\\");
                    fprintf(t_Makefile, "%s\n", sub_string );
                    sprintf( sub_string,"\t" );
                  }
               }
          }
          slen = strlen( sub_string );
          if( slen > 1 ) fprintf(t_Makefile, "%s\n\n", sub_string );
	  }

      if ( strncmp(inln, "OBJS",3) == 0){
	fprintf(t_Makefile, "\tkpp_mechanism_driver.o      \\\n");     
	
      }
    }



   fclose( t_Makefile );
   fclose( ch_Makefile );


   system(cp_command);

}
