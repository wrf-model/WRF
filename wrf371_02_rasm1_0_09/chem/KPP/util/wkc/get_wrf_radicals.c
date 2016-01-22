#include <stdio.h>

#include "protos.h"
#include "protos_kpp.h"
#include "registry.h"
#include "data.h"
#include "kpp_data.h"



int
get_wrf_radicals () 
{


  knode_t * q, * member;
  node_t *p ;


        q = new_knode( );
        q->next = NULL ;
         strcpy( q->name, "WRFC_radicals"  );	
         add_knode_to_end( q , &(WRFC_radicals) ); 





  for ( p =  Domain.fields ; p != NULL ; p = p->next )
  {

      if ( !strncmp(  p->descrip ,"Radicals",8) )
	{
   
	  /* fprintf(stdout, " %s: %s  \n", p->descrip, p->dname); */  

           member = new_knode( ) ;
           strcpy( member->name , p->dname ) ;
           member->next = NULL ;
	   add_knode_to_end( member , &(q->members) ) ;


        }
  }

    

  return(0) ;
}
