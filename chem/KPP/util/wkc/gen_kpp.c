/*
 WRF-Chem to KPP coupler (WKC)

  Copyright (C) 2006 Marc Salzmann

  WKC is free software; you can redistribute it and/or modify it under the
  terms of the GNU General Public License as published by the Free Software
  Foundation (http://www.gnu.org/copyleft/gpl.html); either version 2 of the
  License, or (at your option) any later version.

  WKC is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along
  with this program; if not, consult http://www.gnu.org/copyleft/gpl.html or
  write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA  02111-1307,  USA.

  Marc Salzmann
  Max Planck Institute for Chemistry            
  Department of Atmospheric Chemistry          
  Postfach 3060
  55020 Mainz, Germany
  e-mail: salzmann@mpch-mainz.mpg.de
  www.mpch-mainz.mpg.de/~salzmann/my_home/index.html

.....................................................................

 assumed directory name in KPP corresponds to WRF package name
..

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <dirent.h> 
#include <unistd.h> 
#include <sys/stat.h>           


#include "protos.h"
#include "protos_kpp.h"
#include "registry.h"
#include "data.h"
#include "kpp_data.h"


#define DEBUGR 1

     



/* for data storage issues see kpp_data.h */


/*    cd chem/KPP/util/wkc; make -i -r CC="gcc" ; cd ../../../..
  in ~WRF:  chem/KPP/util/wkc/registry_kpp Registry/Registry          */


int
gen_kpp ( char * inc_dirname, char * kpp_dirname )
{



  /* put chem compound names defined in Registry into linked list WRFC_packs */

       if ( DEBUGR == 1 )   printf("next: get_wrf_chem_specs \n");
     get_wrf_chem_specs () ;
       if ( DEBUGR == 2 ) write_list_to_screen( WRFC_packs ) ;
   
      
       

  /* put radical names defined in Registry into linked list WRFC_radicals */

       if ( DEBUGR == 1 )   printf("next: get_wrf_radicals \n");
     get_wrf_radicals () ;
       if ( DEBUGR == 2 ) write_list_to_screen( WRFC_radicals ) ;


  /* put photolysis rates defined in Registry into linked list WRFC_jvals */

       if ( DEBUGR == 1 )   printf("next: get_wrf_jvals \n");
     get_wrf_jvals () ;
       if ( DEBUGR == 2 ) write_list_to_screen( WRFC_jvals ) ;


  /* read KPP species files and put compound names into linked list KPP_packs */     
       if ( DEBUGR == 1 )   printf("next: get_kpp_chem_specs \n");     
     get_kpp_chem_specs ( kpp_dirname ) ; 
       if ( DEBUGR == 2 ) {write_list_to_screen( KPP_packs ) ;} 



    

  /* define pointer from each KPP package to corresponding WRF-Chem chemistry package  and check whether variable names are consistent. If *_wrfkpp.equiv file exists in KPP directory use it for name matching */
 

       if ( DEBUGR == 1 )   printf("next: compare_kpp_to_species \n"); 
     compare_kpp_to_species ( kpp_dirname );



    
 
     /* write some output to screen  */
       if ( DEBUGR == 1 )   printf("next: screen_out \n");
     screen_out( );


     /* make sure that wrf and kpp variables match and stop if not.  */
      if ( DEBUGR == 1 )   printf("next: check_all \n");
     check_all ( kpp_dirname );

    

     /* add the kpp generated modules to the Makefile in the chem directory */        
           if ( DEBUGR == 1 )   printf("next: change_chem_Makefile  \n");
	     change_chem_Makefile ( ); 

     


  /* write the mechanism driver */
      if ( DEBUGR == 1 )   printf("next: gen_kpp_mechanism_driver (writing chem/kpp_mechanism_driver.F) \n");
     gen_kpp_mechanism_driver ( );


      if ( DEBUGR == 1 )   printf("next: gen_call_to_kpp_mechanism_driver (writing inc/call_to_kpp_mech_drive.inc) \n");
     gen_kpp_call_to_mech_dr ( );


     /* write arguments for call to KPPs Update_Rconst      */
          if ( DEBUGR == 1 )   printf("next: gen_kpp_args_to_Update_Rconst (writing inc/args_to_update_rconst.inc and inc/<decls_update_rconst.inc) \n");
     gen_kpp_args_to_Update_Rconst ( );
     

    /* write the interface */
      if ( DEBUGR == 1 )   printf("next: gen_kpp_interface (writing several module_kpp_* for each mechanism)\n");
     gen_kpp_interface ( );
     

    
      if ( DEBUGR == 1 )   printf("DONE gen_kpp \n");

  return(0) ;
}




/*---------------------------------------------------------------------*/
int
write_list_to_screen ( knode_t * starting_point )
{
knode_t * l1, * l2;
 for ( l1 = starting_point  ; l1 != NULL ; l1 = l1->next )
  {
    fprintf(stderr,"-- Mechanism  %s   ----- \n", l1->name);
         for ( l2 =  l1->members ; l2 != NULL ; l2 = l2->next )
     {
       fprintf(stderr,"%s ", l2->name);
       } 
  fprintf(stderr," \n \n ");
  }
}

/*---------------------------------------------------------------------*/
int
screen_out ( )
{
knode_t * p1, * p2, * pm1;
int count;

 count=0;

   for ( p1 =   KPP_packs  ; p1 != NULL ; p1 = p1->next ) {
 
     /* fprintf(stderr, "KPP PACK:  %s \n", p1->name); */

     p2 = p1->assoc_wrf_pack;
     if ( p2 ) {
     fprintf(stderr, "MATCHING PACK:  %s_kpp \n", p2->name);
     count =count+1;
          for ( pm1 = p1 -> members;  pm1 != NULL ; pm1 = pm1->next ) {
         
		 if ( pm1 -> found_match == 1 ) {
                  
         	    if ( pm1 -> is_radical  == 1 ) {
		      if ( DEBUGR == 2 ) {
                         fprintf(stderr, " 1 found (radical) %s %s  \n", pm1->name, pm1 -> assoc_wrf_name );
		      }
                    
                    } else{
		      if ( DEBUGR == 2 ){
		       fprintf(stderr, " 1 found %s %s  \n", pm1->name, pm1 -> assoc_wrf_name );
                      }
		    }
		 } 
                 else if ( pm1 -> found_match == 2 ) {
                  fprintf(stderr, " 1 found (special) %s %s  \n", pm1->name, pm1 -> assoc_wrf_name );
                 }
                 else {
                fprintf(stderr, " 0 NOT found %s   \n", pm1->name );
                /* exit (0); */
		 }
                
          }

     }

     /* if ( count == 0 ) {
       fprintf(stderr, " DIDN'T FIND ANY matching packages     \n");
       fprintf(stderr, "    .. add packages to Registry and to chem/KPP/mechanisms     \n");
       exit (0);
       } */


   }

  

}
/*---------------------------------------------------------------------*/
int
check_all( char* kpp_dirname )
{
knode_t * p1, * p2, * pm1;
   for ( p1 =   KPP_packs  ; p1 != NULL ; p1 = p1->next ) {
     p2 = p1->assoc_wrf_pack;
     if ( p2 ) {

          for ( pm1 = p1 -> members;  pm1 != NULL ; pm1 = pm1->next ) {
         
		 if ( pm1 -> found_match < 1 ) {


                fprintf(stderr, "\n FATAL ERROR MAPPING WRF TO KPP SPECIES FOR MECHANISM: %s \n", p2->name  );
                fprintf(stderr, " variable %s NOT FOUND   \n", pm1->name );
                fprintf(stderr, " Please check: \n");
                fprintf(stderr, "    (a) the Registry \n");
                fprintf(stderr, "    (b) ./%s/%s/%s.spc\n", kpp_dirname, p2->name, p2->name);
                fprintf(stderr, "         and  ./%s/%s/%s_wrfkpp.equiv (if present)  \n",kpp_dirname, p2->name, p2->name);
                fprintf(stderr, " EXITING  \n");
                exit(1);
                
               }
	  }
            
     }
   }
}
