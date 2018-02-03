#include <stdio.h>


#include "protos.h"
#include "protos_kpp.h"
#include "kpp_data.h"



int
gen_kpp_interface ( )
{
knode_t * p1, * p2, * pm1;
char kpp_interf_fname[NAMELEN];
FILE  * kpp_if;
int  is_driver;

 


   for ( p1 =   KPP_packs  ; p1 != NULL ; p1 = p1->next ) {
 

     p2 = p1->assoc_wrf_pack;
     if ( p2 ) {
 
     sprintf( kpp_interf_fname, "chem/module_kpp_%s_interface.F",p2->name);

   
 
    kpp_if = fopen(kpp_interf_fname, "w" );
  
    gen_kpp_warning(kpp_if, " tools/gen_kpp_interface.c","!");

 

    fprintf(kpp_if,"MODULE module_kpp_%s_interf \n\n\n",p2->name ); 

    fprintf(kpp_if,"  USE module_state_description\n");
    fprintf(kpp_if,"  USE module_configure\n\n");


    fprintf(kpp_if,"  USE %s_Parameters\n",p2->name );
    fprintf(kpp_if,"  USE %s_Precision\n",p2->name );
    fprintf(kpp_if,"  USE %s_UpdateRconstWRF\n",p2->name );
    fprintf(kpp_if,"  USE %s_Integrator\n\n",p2->name );

    fprintf(kpp_if,"  USE module_wkppc_constants\n\n" );
    if( !strcmp( p2->name,"mozcart" ) || !strcmp( p2->name,"t1_mozcart" ) 
        || !strcmp( p2->name,"mozart_mosaic_4bin" ) || !strcmp( p2->name,"mozart_mosaic_4bin_aq" ) ) 
      fprintf(kpp_if,"  USE module_irr_diag\n" );

 
    fprintf(kpp_if,"\n#include <kpp_mechd_u_%s.inc> \n\n\n",p2->name );


     /* define pointers to jvals */
     decl_jv_pointers ( kpp_if );


    fprintf(kpp_if,"CONTAINS \n\n");


    fprintf(kpp_if,"SUBROUTINE  %s_interface( &\n",p2->name );
    /* pass down variables (see gen_kpp_utils) */ 

    if( !strcmp( p2->name,"mozcart" ) || !strcmp( p2->name,"t1_mozcart")
        || !strcmp( p2->name,"mozart_mosaic_4bin" ) || !strcmp( p2->name,"mozart_mosaic_4bin_aq") )
      is_driver = 0;
    else 
      is_driver = 1;

    gen_kpp_pass_down( kpp_if, is_driver );

    fprintf(kpp_if,"    IMPLICIT NONE");

     /* declare variables */
     gen_kpp_decl ( kpp_if, is_driver );


    fprintf(kpp_if,"!local variables \n\n");


     /* declare local array for photolysis rates */
     decl_jv ( kpp_if );

     /* declare misc variables (esp. for kpp) */
      decl_misc ( kpp_if );

  
   fprintf(kpp_if,"\n#include <kpp_mechd_l_%s.inc> \n\n\n",p2->name );


       fprintf(kpp_if," \n\n");


       
       /* preliminaries (setting atol, rtol from atols, rtols) */
       wki_prelim ( kpp_if );



      fprintf(kpp_if,"\n\n");
      fprintf(kpp_if,"\n#include <kpp_mechd_b_%s.inc> \n\n\n",p2->name );
   
       /* start loop over 3-D fields */
       wki_start_loop ( kpp_if );


       /* 1-D water and 3rd body concentrations, temperature  */
       wki_one_d_vars ( kpp_if,  p1 );


       /* fprintf(stderr, "1 MATCHING PACKS:  %s \n", p2->name); */

      /* map jvals for KPP (currently all jvals are mapped) */
       gen_map_jval ( kpp_if );



       /* map wrf to kpp species */

       gen_map_wrf_to_kpp ( kpp_if, p1 );

        fprintf(kpp_if,"\n#include <kpp_mechd_ibu_%s.inc> \n\n",p2->name );


            fprintf(kpp_if, "\n\n\n\n   CALL %s_Update_Rconst(  &\n", p2->name );             fprintf(kpp_if, "!\n");
            fprintf(kpp_if, "#include <extra_args_to_update_rconst_%s.inc>\n", p2->name);
            fprintf(kpp_if, "!\n");
            fprintf(kpp_if, "#include <args_to_update_rconst.inc>\n");
            fprintf(kpp_if, "!\n)\n\n");


        fprintf(kpp_if,"\n#include <kpp_mechd_ib_%s.inc> \n\n",p2->name );

            fprintf(kpp_if, "\n\n\n\n  CALL %s_INTEGRATE(TIME_START, TIME_END, &  \n", p2->name );
            fprintf(kpp_if, "          FIX, VAR,  RCONST, ATOL, RTOL, IRR_WRK, & \n");
            fprintf(kpp_if, "          ICNTRL_U=icntrl, RCNTRL_U=rcntrl  )\n\n\n\n\n");


	    /*            fprintf(kpp_if, "          ICNTRL_U, RCNTRL_U, ISTATUS_U, RSTATUS_U, IERR_U )\n\n\n\n\n"); */


       fprintf(kpp_if,"\n#include <kpp_mechd_ia_%s.inc> \n\n",p2->name );



	    /* return values from kpp to wrf */
        gen_map_kpp_to_wrf ( kpp_if, p1 );



       /* end loop over 3-D fields */
       wki_end_loop( kpp_if );


      fprintf(kpp_if,"\n\n");
      fprintf(kpp_if,"\n#include <kpp_mechd_a_%s.inc> \n\n\n",p2->name );

    fprintf(kpp_if,"\n\nEND SUBROUTINE  %s_interface\n",p2->name ); 
    fprintf(kpp_if,"\n\nEND MODULE module_kpp_%s_interf \n",p2->name ); 

    fprintf(kpp_if,"\n#include <kpp_mechd_e_%s.inc> \n\n\n",p2->name );

	  fclose( kpp_if );
   

     }
   }

  

}

/*---------------------------------------------------------------------*/
