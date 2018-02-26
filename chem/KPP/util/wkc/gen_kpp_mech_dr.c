
#include <stdio.h>


#include "protos.h"
#include "protos_kpp.h"
#include "kpp_data.h"



/*---------------------------------------------------------------------*/
int
gen_kpp_mechanism_driver ( )
{
knode_t * p1, * p2, * p3, * p4, * pm1, * pm3, * pm4;
 char kpp_mdr_fname[NAMELEN];
 FILE  * kpp_mdr;
 int countit;
 int is_driver;
 int max_per_line=6;


   sprintf( kpp_mdr_fname, "chem/kpp_mechanism_driver.F");
   
 
    kpp_mdr = fopen(kpp_mdr_fname, "w" );
  

    


    /* print warning THIS FILE WAS AUTOMATICALLY GENERATED ... */
      gen_kpp_warning(kpp_mdr, "tools/gen_kpp_mech_dr.c","!" );



    fprintf(kpp_mdr, " SUBROUTINE kpp_mechanism_driver(   &\n" );

    /* pass down variables (see gen_kpp_utils) */ 
    is_driver = 0;
    gen_kpp_pass_down( kpp_mdr, is_driver );


    fprintf(kpp_mdr, "     USE module_configure\n");
    fprintf(kpp_mdr, "     USE module_state_description\n\n");


     for ( p1 =   KPP_packs  ; p1 != NULL ; p1 = p1->next ) {
       p2 = p1->assoc_wrf_pack;
        if ( p2 ) {
	  fprintf(kpp_mdr, "     USE module_kpp_%s_interf \n",p2->name );
	}
     }


      fprintf(kpp_mdr, "\n       IMPLICIT NONE\n\n");

     /* declare variables */
     gen_kpp_decl ( kpp_mdr, is_driver );


    fprintf(kpp_mdr, "\n\n!--------\n\n\n");
    fprintf(kpp_mdr, "\n\n  kpp_chem_select: SELECT CASE(config_flags%%chem_opt)  \n\n");




    /* write calls to kpp interface routines */



   for ( p1 =   KPP_packs  ; p1 != NULL ; p1 = p1->next ) {
     p2 = p1->assoc_wrf_pack;
     if ( p2 ) {

 
       /* fprintf(stderr, "1 MATCHING PACKS:  %s \n", p2->name); */

       /* map wrf to kpp species */

 
       fprintf(kpp_mdr, "   CASE (%s_kpp) \n\n", p2->name );
       fprintf(kpp_mdr, "        CALL wrf_debug(15,'kpp_mechanism_driver: calling %s_interface') \n\n", p2->name ); 
       fprintf(kpp_mdr, "        CALL %s_interface(     &\n", p2->name ); 
       /* pass down variables */
       if( !strcmp( p2->name,"mozcart" ) || !strcmp( p2->name,"t1_mozcart")
           || !strcmp( p2->name,"mozart_mosaic_4bin" ) || !strcmp( p2->name,"mozart_mosaic_4bin_aq") )
         is_driver = 0;
       else
         is_driver = 1;
       gen_kpp_pass_down ( kpp_mdr, is_driver );



     }
   }


   fprintf(kpp_mdr, "\n     CASE  DEFAULT\n\n");

  fprintf(kpp_mdr, "     END SELECT kpp_chem_select\n\n");

  fprintf(kpp_mdr, " END SUBROUTINE kpp_mechanism_driver\n\n");





    fclose(kpp_mdr);

}



int
gen_kpp_call_to_mech_dr ( )
{
knode_t * p1, * p2, * p3, * p4, * pm1, * pm3, * pm4;
 char kpp_cmd_fname[NAMELEN];
 FILE  * kpp_cmd;
 int countit;
 int max_per_line=6;

   sprintf( kpp_cmd_fname, "inc/call_to_kpp_mech_drive.inc");
   
 
    kpp_cmd = fopen(kpp_cmd_fname, "w" );
  



    /* print warning THIS FILE WAS AUTOMATICALLY GENERATED ... */
      gen_kpp_warning(kpp_cmd, "tools/gen_kpp_mech_dr.c","!" );


 
    /* pass down all radicals */ 
    gen_kpp_argl_new( kpp_cmd,  WRFC_radicals );


    /* pass down jvals */ 
    gen_kpp_argl_new( kpp_cmd,  WRFC_jvals );



}

