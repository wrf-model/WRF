#include <stdio.h>


#include "protos.h"
#include "protos_kpp.h"
#include "kpp_data.h"


int get_wrf_jvals( )
{


  knode_t * q, * member;
  node_t *p ;


        q = new_knode( );
        q->next = NULL ;
         strcpy( q->name, "WRFC_jvals"  );	
         add_knode_to_end( q , &(WRFC_jvals) ); 





  for ( p =  Domain.fields ; p != NULL ; p = p->next )
  {

      if ( !strncmp(  p->dname ,"PHOTR",5) )
	{
   
	  /* fprintf(stdout, " %s: %s  %s\n", p->dname,  p->name, p->dname); */  

           member = new_knode( ) ;
           strcpy( member->name , p->name ) ;
           member->next = NULL ;
	   add_knode_to_end( member , &(q->members) ) ;


        }
  }

  
  
return(0) ;
}

