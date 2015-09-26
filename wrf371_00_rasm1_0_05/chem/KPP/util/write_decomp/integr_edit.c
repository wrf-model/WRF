#include <stdio.h>
#include <stdlib.h>

#define NAMELEN 4096
 

/* replace decomp routine in KPP Integr file */


main( int argc, char *argv[] )
{

  char mechname[NAMELEN];
  char intfname[NAMELEN], incfname[NAMELEN], tfname[NAMELEN];
  char inln[NAMELEN], incln[NAMELEN];
  char callln[NAMELEN], endln[NAMELEN] ;
  char cp_command[NAMELEN];

  int copyit, add_sub;

  FILE * intf;
  FILE * incf;  
  FILE * ofile;
  

  argv++ ;

  if ( *argv )
    {
  strcpy( mechname, *argv );
    }
  else
    {
     printf("ERROR: USAGE: integr_edit mech_name\n");
     exit(11);
    }

  sprintf ( intfname, "module_kpp_%s_Integr.F", mechname);
  sprintf ( incfname, "decomp_%s.inc", mechname);
  sprintf ( tfname, "%s_new", intfname );

  

  sprintf(  cp_command,"cp %s %s",tfname, intfname);

  intf = fopen( intfname , "r" );
  incf = fopen( incfname , "r" );
  ofile = fopen( tfname , "w" );


  
  sprintf ( callln , "   CALL %s_KppDecomp\0", mechname );
  sprintf ( endln , "END MODULE");

     /* loop over lines in Integr file */
       while ( fgets ( inln , 4096 , intf ) != NULL ){

	   copyit=1;
      
        
	   /* replace call to decomp routine */

           if ( !strncmp (inln, callln, strlen(callln)-1) ) {

	  printf("   integr_edit: replacing  %s \n", inln);

          fprintf(ofile, "!!!  use direct adressing in decomp \n");
          fprintf(ofile, "!!! %s", inln);
          fprintf(ofile, "CALL decomp_%s ( A, ising )\n", mechname );


          add_sub=1;
	  copyit=0;
           }


	   /* add decomp routine w. direct referncing */

          if ( !strncmp (inln, endln, strlen(endln)-1) ) {

	    if ( add_sub ){
	    printf("  %s ", inln );
               while ( fgets ( incln , 4096 , incf ) != NULL ){

                fprintf(ofile, "%s", incln);

               }
            
                fprintf(ofile, " \n\n\n");
            }
          }




       /* copy line from original file */
	 if ( copyit ) {
 
	   fprintf(ofile, "%s", inln);

	 } 


     }


       if ( ! add_sub  ) {
	 printf(" integr_edit: Kept previous version. \n "); 
       }



  fclose( intf );
  fclose( incf );
  fclose( ofile );


   system(cp_command);

  exit (0);
}
