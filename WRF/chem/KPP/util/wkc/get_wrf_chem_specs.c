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


int
get_wrf_chem_specs ( )
{
  node_t * pkg;
  char assoc_namelist_var[NAMELEN_LONG];
  char scalars_str[NAMELEN_LONG] ;
  char * scalar ;
  char * suffix;
  char pname[NAMELEN_LONG] ;
  int j;

  knode_t * q , * member  ;


  for ( pkg = Packages ; pkg != NULL ; pkg = pkg->next )
  {


      if ( !strncmp(  pkg->pkg_assoc ,"chem_opt", 8) )
	{



          suffix=strrchr(pkg->name, '_');
	  /* printf("suffix 0 %s \n",suffix ); */         


          /* only use packages ending on "_kpp" */

          if ( suffix == NULL ) continue;
          if (strlen(suffix) != 4)  continue;
          if (strcmp(suffix, "_kpp") != 0) continue; 
  
          /* clear string */   
          for(j = 0; j < NAMELEN ; j++) pname[j]='\0';

	  /* remove the _kpp at the end */
          strncpy(pname, pkg->name, strlen(pkg->name)-4);
          

	  /* printf("pname 0 %s %s %i \n",pname,pkg->name,  strlen(pkg->name)-4   );   */


         q = new_knode( );
         q->next = NULL ;
         strcpy( q->name,  pname );	
         add_knode_to_end( q , &(WRFC_packs) ) ;


         strcpy(scalars_str,pkg->pkg_4dscalars) ; 


          scalar=strtok(scalars_str, ":");
          scalar=strtok(NULL, ",");


        while (scalar != NULL)
          { 

           member = new_knode( ) ;
           strcpy( member->name , scalar ) ;
           member->next = NULL ;
	   add_knode_to_end( member , &(q->members) ) ;


               scalar = strtok(NULL, ",");
    
           }
 

        }  
   }




  return(0) ;
}
