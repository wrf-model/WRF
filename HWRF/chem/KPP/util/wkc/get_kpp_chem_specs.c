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

int
get_kpp_chem_specs ( char* kpp_dirname )
{


knode_t * q , * member  ;
DIR * dir;		
struct dirent * entry;          
struct stat dir_stat;
char fulldirname[NAMELEN], spcfilename[NAMELEN];
char inln[NAMELEN], kpp_spec[NAMELEN];
FILE * spcFile;
int   in_comment, got_it;



  /* http://users.actcom.co.il/~choo/lupg/tutorials/handling-files/handling-files.html#directory_struct */


 
    dir = opendir(kpp_dirname);
    if (!dir) {
	fprintf(stderr, "WARNING from gen_kpp: Cannot read directory: %s \n", kpp_dirname);
	perror("");
	return;
    }


    /* loop through sub directories in KPP directory */

 while ((entry = readdir(dir))) {
   if (entry->d_name ) {

	 if ( strcmp(entry->d_name, ".") == 0) 
          continue;
	 if ( strcmp(entry->d_name, "..") == 0) 
           continue;
 


      sprintf(  fulldirname, "%s/%s", kpp_dirname, entry->d_name);
	
      printf("%s \n", fulldirname );

       /* check if the given entry is a directory. */
        if (stat(fulldirname, &dir_stat) == -1) {
	  fprintf(stderr, "WAA\n\n"); 
	   perror("WARNING from gen_kpp: "); 
	    continue;
        }
	    

        /* check if KPP species file is present. */

         sprintf(  spcfilename, "%s/%s/%s.spc", kpp_dirname, entry->d_name, entry->d_name);

     
	 ;fprintf(stderr, " spcfilename: %s \n",spcfilename);
    
      spcFile = fopen (spcfilename, "r" );
       
        if ( spcFile == NULL ) {
	  fprintf(stderr,"WARNING from gen_kpp: File %s not found. Skipping. \n", spcfilename);
           continue;
	}
        
   printf(" Found %s \n", spcfilename );




   /*----------------------------------------------------*/
  

        /* put KPP packagename into linked list */
 
         q = new_knode( );
         q->next = NULL ;
         strcpy( q->name, entry->d_name  );	
         add_knode_to_end( q , &(KPP_packs) ) ;

	 /* loop over lines in KPP species file */
	 while ( fgets ( inln , NAMELEN , spcFile ) != NULL ){
	      if ( DEBUGR == 1 ){    printf("%s  ", inln); } 
         /* strip from comments (loop through letters) */
	 int n=0;
         int nn = 0;
         int j;
         in_comment = 0;
         got_it = 0;

            for(j = 0; j < NAMELEN ; j++) kpp_spec[j]='\0';
	  while ( inln[n] !=  '\0' ){ 
	   if ( inln[n] == '{') in_comment=1;
	    if ( in_comment == 0 ) {
                    if (inln[n] == '=' || inln[n] == '#') {
                        got_it=1;
		    }
                      if ( got_it == 0  && inln[n] !=  ' '){
		    /* printf("%c %i \n ", inln[n], in_comment ); */

                       kpp_spec[nn]=inln[n];
                       nn++;

		      }
          }    

          if (inln[n] == '}') in_comment=0; 
	  n++;

          }

	  /* printf("spec: %s \n ", kpp_spec);  */
          
	  if (kpp_spec[0] != '\0'  && got_it == 1 )  {

            if ( DEBUGR == 1 ){
                  printf("spec: %s \n ", kpp_spec);
                  fprintf(stderr," p, name   %s %s \n", q->name, kpp_spec ); 
               }

           member = new_knode( ) ;
           strcpy( member->name , kpp_spec )   ;
           member->next = NULL ;
	   add_knode_to_end( member , &(q->members) ) ;
	  
	  }
         } 

	 

	 fclose(spcFile);
  
    }

 }

  return(0) ;
}

