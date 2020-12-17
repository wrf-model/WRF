#include <stdio.h>


#include "protos.h"
#include "protos_kpp.h"
#include "kpp_data.h"


int 
decl_misc (  FILE * ofile )
{


 fprintf(ofile,"    REAL(KIND=dp):: TIME_START\n");
 fprintf(ofile,"    REAL(KIND=dp):: TIME_END\n\n");


 fprintf(ofile,"    INTEGER, DIMENSION(20) :: ICNTRL \n");
 fprintf(ofile,"    REAL(KIND=dp), DIMENSION(20) :: RCNTRL\n");
 fprintf(ofile,"    INTEGER, DIMENSION(20) :: ISTATUS \n");
 fprintf(ofile,"    REAL(KIND=dp), DIMENSION(20) :: RSTATUS\n");
 fprintf(ofile,"    INTEGER :: IERR_U\n\n");

 fprintf(ofile,"    REAL(KIND=dp), DIMENSION(NREACT):: RCONST \n\n");
 fprintf(ofile,"    REAL(KIND=dp), DIMENSION(NVAR) :: var\n"); 
 fprintf(ofile,"    REAL(KIND=dp), DIMENSION(NFIX) :: fix\n\n");
 
 fprintf(ofile,"     !temperature (K)\n");
 fprintf(ofile,"    REAL(KIND=dp)   :: TEMP \n\n");

 fprintf(ofile,"    REAL(KIND=dp), DIMENSION(NSPEC)  :: ATOL, RTOL\n");
 fprintf(ofile,"    REAL(KIND=dp), DIMENSION(NREACT) :: IRR_WRK\n\n");



 fprintf(ofile,"    REAL(KIND=dp) :: conv, oconv \n");

 fprintf(ofile,"    REAL(KIND=dp) :: C_M \n\n");

 fprintf(ofile,"    INTEGER :: i,j,k,n \n");

 fprintf(ofile," \n\n\n\n ");
}

int 
decl_jv (  FILE * ofile )
{
  int n;  

  n=count_members( WRFC_jvals ); 
 
 
 fprintf(ofile,"    INTEGER, PARAMETER :: njv=%i\n",n);
 fprintf(ofile,"    REAL(KIND=dp), DIMENSION(njv) :: jv\n\n\n");


  
}


int
count_members(  knode_t * nl )
{
 knode_t * pml;
 int n=0;

     for ( pml = nl -> members;  pml != NULL ; pml = pml->next ) {
      n=n+1; 
     }

     return(n);
}



int 
decl_jv_pointers (  FILE * ofile )
{
  knode_t * pl;
  char s1[NAMELEN], s2[NAMELEN];
  int count=1;
  int i;

    fprintf(ofile, "! locally define pointers to photolysis rates\n\n");

   for ( pl =  WRFC_jvals  -> members ; pl != NULL ; pl = pl->next ) {

     strcpy(s1,pl->name);

     for(i=0;i<50;i=i+1){
       s2[i]=s1[i+3]; 
       }

 
	fprintf(ofile, "     INTEGER, PARAMETER, PRIVATE :: Pj_%s = %i \n", s2, count); 

        count = count + 1;
   }


  fprintf(ofile," \n\n\n");

}


int 
gen_map_jval (  FILE * ofile )
{
  knode_t * pl;
  char s1[NAMELEN], s2[NAMELEN];
  int i;

   for ( pl =  WRFC_jvals  -> members ; pl != NULL ; pl = pl->next ) {

     strcpy(s1,pl->name);

     for(i=0;i<50;i=i+1){
       s2[i]=s1[i+3]; 
       }
	fprintf(ofile, "    jv(Pj_%s) = REAL(%s(i,k,j)/60., KIND=dp) \n", s2, s1); 

   }

     fprintf(ofile," \n\n\n");
}




int 
gen_map_wrf_to_kpp (  FILE * ofile,  knode_t * nl )
{
 knode_t * pml;

          for ( pml = nl -> members;  pml != NULL ; pml = pml->next ) {
         
		 if ( pml -> found_match == 1 ) {

		   if ( pml -> is_radical == 0 ){
		   fprintf(ofile, "    var(ind_%s) = conv  * REAL( MAX(chem(i,k,j,P_%s),0.), KIND=dp)  \n", pml->name, pml -> assoc_wrf_name ); 
                     }
	             else if  ( pml -> is_radical == 1 ){
                      fprintf(ofile, "    var(ind_%s) =  conv * REAL( MAX(%s(i,k,j),0.), KIND=dp)  \n", pml->name, pml -> assoc_wrf_name ); 
		     }

		 } 
                else if ( pml -> found_match != 2 ) {
		  fprintf(stderr, " FATAL ERROR");
                exit (0);
		 }
                
          }
}



int 
gen_map_kpp_to_wrf (  FILE * ofile,  knode_t * nl )
{
 knode_t * pml;

         for ( pml = nl -> members;  pml != NULL ; pml = pml->next ) {
         
		 if ( pml -> found_match == 1 ) {

                     if ( pml -> is_radical == 0 ){
		   fprintf(ofile, "    chem(i,k,j,P_%s) = MAX ( REAL (oconv * var(ind_%s), KIND=sp), 0.)  \n",  pml -> assoc_wrf_name, pml->name ); 
                     }
                    else if  ( pml -> is_radical == 1 ){
                  fprintf(ofile, "   %s(i,k,j) = MAX (REAL (oconv * var(ind_%s) , KIND=sp),0.) \n",  pml -> assoc_wrf_name, pml->name ); 
                    }

		 } 
                else if ( pml -> found_match != 2 ) {
                fprintf(stderr, " NOT found %s   \n", pml->name );
                exit (0);
		 }
                
          }
}


int 
gen_kpp_pargs( FILE * ofile, knode_t * nl  )
{
 knode_t * pml;
 char s1[NAMELEN], s2[NAMELEN];
 int countit;
 int max_per_line=5;
 int i;
             fprintf(ofile,"            ");

	         countit=0;   
              for ( pml = nl -> members;  pml != NULL ; pml = pml->next ) {


                  strcpy(s1,pml->name);

                   for(i=0;i<50;i=i+1){
                     s2[i]=s1[i+3]; 
                    }


                 fprintf(ofile," Pj_%s,", s2);
		 countit = countit+1;
                  if ( countit % max_per_line ==  0) {
		   fprintf(ofile," & \n            ");
		   } 
		 }   


                 if ( countit % max_per_line !=  0) {
		 fprintf(ofile,"  & \n"); 
		 }   


}


int 
gen_kpp_pdecl( FILE * ofile, knode_t * nl  )
{
 knode_t * pml;
  char s1[NAMELEN], s2[NAMELEN];
 int countit;
 int max_per_line=5;
 int i;

    fprintf(ofile, "\n\n\n        INTEGER, INTENT(IN ) ::  & \n              ");    

          
	         countit=0;   
              for ( pml = nl -> members;  pml != NULL ; pml = pml->next ) {

                  strcpy(s1,pml->name);

                   for(i=0;i<50;i=i+1){
                     s2[i]=s1[i+3]; 
                    }

		 if ( pml->next != NULL ){
                    fprintf(ofile," Pj_%s,", s2);
                  }
                  else{
                    fprintf(ofile," Pj_%s", s2);
		  } 
  
		    countit = countit+1;
                  if ( countit % max_per_line ==  0) {
                   if ( pml->next != NULL ){
		   fprintf(ofile," & \n              ");
                   }
		   } 
		 }   
}


int
wki_start_loop( FILE * ofile )
{

   fprintf(ofile,"\n    DO j=jts, jte\n");
   fprintf(ofile,"    DO k=kts, kte\n");
   fprintf(ofile,"    DO i=its, ite\n\n\n");
}

int
wki_end_loop( FILE * ofile )
{

   fprintf(ofile,"\n\n\n    END DO\n");
   fprintf(ofile,"    END DO\n");
   fprintf(ofile,"    END DO\n\n");  
}


int
wki_prelim( FILE * ofile )
{


  
   fprintf(ofile,"!initialization, individual parameters set below \n");
   fprintf(ofile,"      DO n=1, 20\n");
   fprintf(ofile,"         ICNTRL(n) = 0\n");
   fprintf(ofile,"         RCNTRL(n) = 0._dp\n");
   fprintf(ofile,"      END DO\n\n");


   fprintf(ofile,"! CURRENTLY FROM in chem/KPP/module_wkppc_constants.F \n");
   fprintf(ofile,"      DO n=1, NSPEC\n");
   fprintf(ofile,"         ATOL(n) = REAL(atols, KIND=dp)\n");
   fprintf(ofile,"         RTOL(n) = REAL(rtols, KIND=dp)\n");
   fprintf(ofile,"      END DO\n\n\n");

   fprintf(ofile,"      TIME_START = 0.0_dp \n");   
   fprintf(ofile,"      TIME_END =  REAL(dtstepc, KIND=dp) \n\n");   


   fprintf(ofile,"! SETTINGS FOR ICNTRL, RCNTRL IN in chem/KPP/inc/kpp_ctrl_default.inc \n");
   fprintf(ofile,"#include <kpp_ctrl_default.inc> \n\n");

}


int
wki_one_d_vars( FILE * ofile,   knode_t * pp )
{


  if ( pp -> got_air == 1 ) {
   fprintf(ofile,"      ! 3rd body concentration (molec/cm^3)\n");
   fprintf(ofile,"    FIX(indf_M)  = REAL(dens2con_a * rho_phy(i,k,j), KIND=dp)\n");
   fprintf(ofile,"    C_M = FIX(indf_M)\n\n");
  } else {
    fprintf(ofile,"    C_M = 0.0_dp ! not used \n\n");
  }

  if ( pp -> got_o2 == 1 ) {
   fprintf(ofile,"      ! o2 concentration (molec/cm^3)\n");
   fprintf(ofile,"    FIX(indf_O2)  = .209_dp*REAL(dens2con_a * rho_phy(i,k,j), KIND=dp)\n");
  }

  if ( pp -> got_n2 == 1 ) {
   fprintf(ofile,"      ! n2 concentration (molec/cm^3)\n");
   fprintf(ofile,"    FIX(indf_N2)  = .79_dp*REAL(dens2con_a * rho_phy(i,k,j), KIND=dp)\n");
  }

   fprintf(ofile,"      ! water concentration (molec/cm^3)\n");
   fprintf(ofile,"    FIX(indf_H2O) = REAL(dens2con_w * moist(i,k,j,P_QV) * rho_phy(i,k,j), KIND=dp)\n\n\n");



   fprintf(ofile,"      ! temperature (K)\n");
   fprintf(ofile,"    TEMP = REAL(t_phy(i,k,j), KIND=dp)\n\n");


   fprintf(ofile,"      ! convesion from ppmV to molecules/cm3 and back\n");
   fprintf(ofile,"     conv=1.E-6_dp*dens2con_a*rho_phy(i,k,j)\n");
   fprintf(ofile,"     oconv = 1.E0_dp/conv\n\n\n");



}





