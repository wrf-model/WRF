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


#define DEBUGR 0

/* variable name of molecular nitrogen in KPP file*/
char * kpp_n2 = "N2" ;

/* variable name of molecular oxygen in KPP file*/
char * kpp_o2 = "O2" ;

/* variable name of CO2 in KPP file*/
char * kpp_co2 = "CO2" ;

/* variable name of water vapor in KPP file*/
char * kpp_h2o = "H2O" ;


/* variable name of third body in KPP file*/
char * kpp_third_body = "M" ;


int
compare_kpp_to_species  ( char * kpp_dirname)
{
  knode_t * p1, *p2, * p, * pm1, * pm2, * p3, * pm3 , wrf_kpp_equivs;
  char name1[NAMELEN], name2[NAMELEN], name3[NAMELEN] ;
  char equivfilename[NAMELEN];
  FILE * equivFile;  
  char inln[NAMELEN], newln[NAMELEN];
  int in_comment, got_it;
  char wrf_name[NAMELEN], kpp_name[NAMELEN];
  int i;
  int got_h2o; 

  /*  first find matching packages */


  for ( p1 =   KPP_packs  ; p1 != NULL ; p1 = p1->next ) {
    for ( p2 =   WRFC_packs  ; p2 != NULL ; p2 = p2->next ) { 

      /* printf(" ... test0  %s %s\n",p1->name, p2->name ); */


       if ( strcmp (p1->name, p2->name) == 0) {

       

     /* point from a KPP-pack to the corresponding WRFC_pack */ 
	  p1->assoc_wrf_pack = p2;         

       }
       
    }
  } 




   for ( p1 =   KPP_packs  ; p1 != NULL ; p1 = p1->next ) {
     p2 = p1->assoc_wrf_pack;
        printf(" ... testing  %s %s\n",p1, p2 );
     if ( p2 ) {
     fprintf(stderr, "\n \n FOUND match between WRF-Chem/KPP for mechanism:  %s \n", p2->name);


     for ( pm1 = p1 -> members;  pm1 != NULL ; pm1 = pm1->next ) {
          pm1->found_match = 0; 
          pm1->is_radical =0;
     }



    /* if file *_wrfkpp.equiv exists store contents in wrf_kpp_equivs struct */

       sprintf(  equivfilename, "%s/%s/%s_wrfkpp.equiv", kpp_dirname, p2->name,  p2->name);

       /* printf("%s \n",  equivfilename ); */  

      equivFile = fopen (equivfilename, "r" );
       
    if ( equivFile == NULL ) {
       fprintf(stderr," Did NOT find file %s\n",equivfilename);
    }
    else
    {
	  fprintf(stderr," Found file %s\n",equivfilename);
	
     /* loop over lines in wrf_kpp_equiv file */
	 while ( fgets ( inln , NAMELEN , equivFile ) != NULL ){
          if (  DEBUGR == 1 ) printf(" i  %s ", inln );


           int j;
            for(j = 0; j < NAMELEN ; j++) wrf_name[j]='\0';
            for(j = 0; j < NAMELEN ; j++) kpp_name[j]='\0';

           int n=0;
           in_comment = 0;
              for(j = 0; j < NAMELEN ; j++) newln[j]='\0';

	    while ( inln[n] !=  '\0' ){
              if (inln[n] == '!') {
                in_comment = 1;
              }
	      if ( !in_comment ) {
		newln[n] =  inln[n];
              } 
	      n++;
	      } 


	 if (  DEBUGR == 1 ) printf(" n  %s ", newln );
             if ( !in_comment && strlen(newln) > 1 ) {
	          n=0;
                 while ( newln[n] !=  '\0' ){
		     i=0;
                    
		      while ( newln[n] !=  ' ' ){
			wrf_name[i]=newln[n];
		       i++;
                       n++;
		      }

                      n++;

		     i=0;
                      while ( newln[n] !=  ' ' && newln[n] != '\0'){
                        if  (newln[n] != '\n'){  
			kpp_name[i]=newln[n];
                        }
		       i++;
                       n++;
		      }
		      
		 }

                  
		 
		 

                 make_upper_case(wrf_name);
     
                 make_upper_case(kpp_name);

     /*  wrf_name=strtok(newln, " ");   
         kpp_name=strtok(NULL, " ");   */
	    printf(" kpp_name,  wrf_name   %s %s \n ", kpp_name, wrf_name  );
        
     

		 /* check whether wrf_name was found in Registry */
                 /*   either as associated 4D chem var in a package */ 

	        got_it=0;
               for ( pm2 = p2 -> members;  pm2 != NULL ; pm2 = pm2->next ) {

                     strcpy( name2, pm2->name );
                     make_upper_case(name2);

		 if (  DEBUGR > 1 ) {
                 fprintf(stderr,"comp %s %s \n",pm2->name, wrf_name);
                 }
                 if ( strcmp(name2, wrf_name) == 0 ) {
	           got_it=1;      
                       
  
                 }
               }




               
               /*  (b)  or declared as a non-transported radical */
  
      
	         p3 = WRFC_radicals;
		       
                      
               for ( pm3 = p3 -> members;  pm3 != NULL ; pm3 = pm3->next ) {


                   strcpy( name3, pm3->name );
                   make_upper_case(name3);		 


		 if (  DEBUGR > 1 ) {
                 fprintf(stderr,"comp ra %s %s \n",name3, wrf_name);
                 }
                 if ( strcmp(name3, wrf_name) == 0 ) {
	           got_it=1;      
                       
  
                 }
		 }   
	     


	        if ( got_it != 1 ) {
                  fprintf(stderr, "ERROR: variable name %s was found in file %s but not in the Registry \n", wrf_name, equivfilename);
                   exit (0); 
                }             


    /*-------------------------------------------------*/
     
	    /* check whether kpp_name was found in species file and 
               store name from wrf_kpp_equiv file in assoc_wrf_name */
               got_it=0;
             for ( pm1 = p1 -> members;  pm1 != NULL ; pm1 = pm1->next ) {


             	strcpy( name1, pm1->name );
                make_upper_case(name1);

               if ( strcmp(name1, kpp_name) == 0 ) {
	           got_it=1;      
                   pm1->found_match = 1;
                   strcpy( pm1->assoc_wrf_name,  wrf_name);
		  


             /* still have to check if it is a radical .. */
          
               for ( pm3 = p3 -> members;  pm3 != NULL ; pm3 = pm3->next ) {


                   strcpy( name3, pm3->name );
                   make_upper_case(name3);		 


		 if (  DEBUGR > 1 ) {
                 fprintf(stderr,"comp ra %s %s \n",name3, wrf_name);
                 }
                 if ( strcmp(name3, wrf_name) == 0 ) {
	           
                    pm1->is_radical = 1;    
  
                 }
		 }   

 
               if (  DEBUGR == 1 ) {
		 fprintf(stderr,"matching %s %s %i \n",pm1->name, kpp_name, got_it);
               }
                 }
               if (  DEBUGR > 1 ) {
		 fprintf(stderr,"comp %s %s %i \n",pm1->name, kpp_name, got_it);
               }

             }




                 if ( got_it != 1 ) {
                  fprintf(stderr, "ERROR:  variable name %s was not found in species file in directory %s/%s but it was found in  %s \n", kpp_name, kpp_dirname, p2->name, equivfilename);
                  exit (0); 
                 }   


	           
	    }  
	  }
    }	 
   
    

     /* compare compound  names, use variable */
     for ( pm1 = p1 -> members;  pm1 != NULL ; pm1 = pm1->next ) {


            if (  DEBUGR == 1 ) {
             fprintf(stderr, " SEARCHING FOR %s  \n", pm1->name );
	    }


             	strcpy( name1, pm1->name );
                make_upper_case(name1);

       for ( pm2 = p2 -> members;  pm2 != NULL ; pm2 = pm2->next ) {


          strcpy( name2, pm2->name );
          make_upper_case(name2);



         if ( strcmp (name1, name2) == 0) {
	  /* store matching name of WRF var in members of KPP_packs */
	  strcpy( pm1->assoc_wrf_name,  pm2->name); 
          pm1->found_match = 1; 
            if (  DEBUGR == 1 ) {
          fprintf(stderr, " matching names:  %s %s \n", pm1->name,  pm1->assoc_wrf_name);
	    }
         }
       }


       /* the same for radicals */
       if ( pm1->found_match != 1 ) {

	 if (  DEBUGR == 1 ){
            fprintf(stderr, " STILL SEARCHING FOR %s  \n", pm1->name ); 
         } 
         p3 = WRFC_radicals;


       for ( pm3 = p3 -> members;  pm3 != NULL ; pm3 = pm3->next ) {


          strcpy( name3, pm3->name );
          make_upper_case(name3);


	  /* fprintf(stderr, " comparing  radicals %s %s \n", name1, name3); */ 

         if ( strcmp (name1, name3) == 0) {
	  /* store matching name of WRF var in members of KPP_packs */
	  strcpy( pm1->assoc_wrf_name,  pm3->name); 
          pm1->found_match = 1;
          pm1->is_radical = 1; 
            if (  DEBUGR == 1 ) {
          fprintf(stderr, " matching radical name :  %s %s \n", pm1->name,  pm1->assoc_wrf_name);
	    }
         }
       }
       } 



     }

     /*  special handling for fixed species */

     p1 -> got_air = 0;
     p1 -> got_o2 = 0;
     p1 -> got_n2 = 0;
     p1 -> got_co2 = 0;
     got_h2o = 0;
     /* take care of water, third body, N2, O2 */
     for ( pm1 = p1 -> members;  pm1 != NULL ; pm1 = pm1->next ) {
    	  strcpy( name1, pm1->name );
          make_upper_case(name1);
       if ( strcmp (name1, kpp_third_body) == 0) {
          pm1->found_match = 2;
           strcpy( pm1->assoc_wrf_name,  "THIRD BODY");
           p1 -> got_air = 1;
       }
       if ( strcmp (name1, kpp_h2o) == 0) {
          pm1->found_match = 2;
          strcpy( pm1->assoc_wrf_name,  "WATER VAPOR");
          got_h2o = 1;
       }
       if ( strcmp (name1, kpp_n2) == 0) {
          pm1->found_match = 2;
           strcpy( pm1->assoc_wrf_name,  "MOLECULAR NITROGEN");
           p1 -> got_n2 = 1;
       }
       if ( strcmp (name1, kpp_o2) == 0) {
          pm1->found_match = 2;
           strcpy( pm1->assoc_wrf_name,  "MOLECULAR OXYGEN");
           p1 -> got_o2 = 1;
       }
       if ( strcmp (name1, kpp_co2) == 0) {
          pm1->found_match = 2;
           strcpy( pm1->assoc_wrf_name,  "CO2");
           p1 -> got_co2 = 1;
       }
     }

     if  ( p1 -> got_air != 1 ) {
                  fprintf(stderr, "ERROR: variable name for third body in KPP species file is expected to be  %s, but was not found in %s species file \n",  kpp_third_body,  p2->name);
                  /* exit (0); */
                } 

     if  ( got_h2o != 1 ) {
                  fprintf(stderr, "ERROR: variable name for water in KPP species file is expected to be  %s, but was not found in %s species file\n",  kpp_h2o,  p2->name);
                  /* exit (0); */
                } 
            

     

     } 
   }



   /*   now in screen_out
   for ( p1 =   KPP_packs  ; p1 != NULL ; p1 = p1->next ) {
     p2 = p1->assoc_wrf_pack;
     if ( p2 ) {
     fprintf(stderr, "1 MATCHING PACKS:  %s \n", p2->name);
          for ( pm1 = p1 -> members;  pm1 != NULL ; pm1 = pm1->next ) {
         
		 if ( pm1 -> found_match == 1 ) {
                   fprintf(stderr, " 1 found %s %s  \n", pm1->name, pm1 -> assoc_wrf_name );
		 } 
                 else if ( pm1 -> found_match == 2 ) {
                  fprintf(stderr, " 1 found %s %s  \n", pm1->name, pm1 -> assoc_wrf_name );
                 }
                 else {
                fprintf(stderr, " 0 NOT found %s   \n", pm1->name );
                exit (0); 
		 }
                
          }

     }
   }

    */


  return(0) ;

}
