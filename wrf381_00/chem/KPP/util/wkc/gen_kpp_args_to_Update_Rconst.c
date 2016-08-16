#include <stdio.h>

#include "protos.h" 
#include "protos_kpp.h" 
#include "kpp_data.h"



int
gen_kpp_args_to_Update_Rconst( )
{
 FILE  * args_urc, * args_to_urc, * decls_urc; 
 char args_fname[NAMELEN], argst_fname[NAMELEN], decls_fname[NAMELEN];
   int n;  


      sprintf( args_fname, "inc/args_update_rconst.inc");
      sprintf( argst_fname, "inc/args_to_update_rconst.inc");
      sprintf( decls_fname, "inc/decls_update_rconst.inc");  


      args_to_urc = fopen(argst_fname, "w" );
      args_urc= fopen(args_fname, "w" );
      decls_urc = fopen(decls_fname, "w" );


      
        
 

               
        fprintf(args_to_urc,"             jv, njv, &\n" );
        fprintf(args_urc,"                j, nj,   &\n" ); 

        fprintf(args_to_urc,"             RCONST, &\n" );
        fprintf(args_urc,"                RCONST, &\n" ); 
      
 
        /* pass down pointers to photolysis rates */
        gen_kpp_pargs(args_to_urc, WRFC_jvals);
        gen_kpp_pargs(args_urc, WRFC_jvals);
   


   
         fprintf(args_to_urc,"             C_M, FIX(indf_H2O), TEMP & \n" );
         fprintf(args_urc,"                C_M, C_H2O, TEMP & \n" );


        fprintf(decls_urc,"\n   IMPLICIT NONE\n");


        fprintf(decls_urc,"\n  INTEGER, INTENT (IN ) :: nj \n\n" );
        fprintf(decls_urc,"    REAL(KIND=dp), DIMENSION(nj), INTENT(IN)  :: j\n\n\n");

        fprintf(decls_urc,"    REAL(KIND=dp), DIMENSION(NREACT), INTENT(OUT)  :: RCONST\n\n\n");


        fprintf(decls_urc,"    REAL(KIND=dp), INTENT(IN)  :: C_M, C_H2O,&\n");
        fprintf(decls_urc,"                                  TEMP\n\n\n");
    
        

        /* declare pointers to photolysis rates */
        gen_kpp_pdecl(decls_urc, WRFC_jvals);



	fclose( args_to_urc);
	fclose( args_urc);
        fclose( decls_urc);
  

}

