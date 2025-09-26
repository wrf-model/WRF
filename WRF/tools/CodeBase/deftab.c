#include <stdio.h>
#include <string.h>

#define INLINELEN (4*8192)
#define VARLEN 128
#define MAXARGS (4*8192)

#define DIR "tools/code_dbase"

char inln[INLINELEN] ;

#define COMPARE(A,B) ( ! strncmp ( A , B , strlen( B ) ) )
#define COMPARE2(A,B) ( ! strcmp ( A , B ) )

char module_name[INLINELEN] ;
char subprogram_name[INLINELEN] ;
char in_a[INLINELEN] ;
char arg[MAXARGS][VARLEN] ;
char type[MAXARGS][VARLEN] ;
char from[MAXARGS][VARLEN] ;
char intent[MAXARGS][VARLEN] ;
char dimensions[MAXARGS][VARLEN] ;
char typedefs[MAXARGS][INLINELEN] ;
int ntypedefs = 0 ;
char tmp[VARLEN] ;
char infname[VARLEN] ;
int  nargs ;
char function_type[VARLEN] ;
int contained ;

char *ignore = "rsl" ;

int protex_state ;

set_attributes( char * inln, int nargs, char * typ )
{
  int i, j ;
  char *p, tmp[VARLEN] ;
  for ( i = 0 ; i < nargs ; i++ )
  {
    if ( contains_tok ( inln , arg[i], " ()," ) ) {
      strcpy( type[i], typ ) ;
      if (( j = contains_tok ( inln, "intent", " (),:" ))) {
        get_token_n ( inln , " (),:", j+1, intent[i] ) ;
      }
      else
      {
        strcpy(intent[i],"inout") ;
      }
      strcpy( dimensions[i], "" ) ;
      if ( find_str ( inln, "dimension", &p )) {
        j = 0 ;
        remove_whitespace( p ) ;
        while ( get_arg_n ( p , j, tmp ) ) {
          strcat( dimensions[i], tmp ) ;
          strcat( dimensions[i], "," ) ;
          j++ ;
        }
        if (( p = rindex( dimensions[i], ',' )) != NULL ) *p = '\0' ;
      }
    }
  }
}

handle_subprogram ( FILE **fp, FILE *ifp, int *nargs, char * sname , char * inln , int tokpos )
{
  char fname[VARLEN] ;
  int i ;

  if ( ! contained ) {
    sprintf(fname,"%s/%s",DIR, sname ) ;
    if ((*fp = fopen( fname , "w" )) == NULL ) {
      fprintf(stderr,"cannot open %s for writing\n",fname) ; exit(1) ;
    }
    fprintf(*fp,"sourcefile %s\n",infname ) ;
    if ( COMPARE( in_a, "function" ) ) {
      fprintf(*fp,"subprogram %s %s\n",in_a, function_type ) ;
    } else {
      fprintf(*fp,"subprogram %s\n",in_a ) ;
    }
    for ( i = 0 ; get_token_n ( inln , " (,)", i+tokpos, arg[i] ) ; i++ ) { strcpy( from[i], "dummyarg" ) ; }
    *nargs = i ;
    ntypedefs = 0 ;
    fprintf(*fp,"nargs %d\n", *nargs) ;
  }
  contained++ ;
}

main( int argc, char * argv[] )
{
  FILE *infl ;
  FILE *fp, *fpcalls, *fpdescription ;
  int i, j ;
  char callee[VARLEN] ;
  char fname[VARLEN] ;
  char description_name[VARLEN] ;
  char mess[INLINELEN] ;
  int in_interface ;
  int looking_scalar_derefs ;

  strcpy( module_name, "" ) ;
  strcpy( subprogram_name, "" ) ;
  strcpy( infname, "" ) ;

  infl = stdin ;
  if ( argc == 2 ) {
    strcpy( infname, argv[1] ) ;
  }
  sprintf(fname,"%s/calls",DIR ) ;
  if ( ( fpcalls = fopen( fname , "a" )) == NULL )
  {
    fprintf(stderr,"cannot open %s\n",fname) ;
    exit(1) ;
  }

  in_interface = 0 ;

  looking_scalar_derefs = 0 ;
 
  contained = 0 ;

  protex_state = 0 ;
  fpdescription = NULL ;

  while( fgets( inln, INLINELEN, infl ) != NULL )
  {
    if ( protex_state > 0 ) {   /* in a description */
       if ( contains_str ( inln, "</DESCRIPTION>" ) ) {
         protex_state = 0 ;
         if ( fpdescription != NULL ) fclose( fpdescription ) ; 
         fpdescription = NULL ;
	 continue ;
       }
       if ( fpdescription != NULL ) {
          remove_chars( inln, "!", ' ' ) ;
          if ( empty( inln ) ) {
            fprintf(fpdescription,"<p>\n") ;
          } else {
            fprintf(fpdescription,"%s",inln) ;
          }
          continue ;
       }
    }
    remove_nl ( inln ) ;
    lower_case_str ( inln ) ;
    if ( looking_scalar_derefs ) {
       if ( COMPARE ( inln, "grid%" ) ) {
         get_token_n ( inln , " ", 2, arg[nargs] ) ;
         strcpy( from[nargs] , "registry" ) ;
         nargs++ ;
       }
    }
    if ( in_interface ) {
       if ( COMPARE( inln , "end interface" ) ) in_interface = 0 ;
       /* ignore interface blocks */
       continue ;
    }
    if ( COMPARE( inln , "interface" ) ) {
       in_interface = 1 ;
    } else if ( COMPARE( inln , "module " ) ) {
       get_token_n ( inln , " (,", 1, module_name ) ;
    } else if ( COMPARE( inln , "end module" ) ) {
       strcpy( module_name, "" ) ;
    } else if ( COMPARE( inln , "program " ) ) {
       strcpy(in_a, "program") ;
       get_token_n ( inln , " (,", 1, subprogram_name ) ;
       handle_subprogram ( &fp, infl, &nargs, subprogram_name, inln, 2 ) ;
    } else if ( COMPARE( inln , "subroutine " ) ) {
       strcpy(in_a, "subroutine") ;
       get_token_n ( inln , " (,", 1, subprogram_name ) ;
       handle_subprogram ( &fp, infl, &nargs, subprogram_name, inln, 2 ) ;
    } else if ( COMPARE( inln , "function " ) ) {
       strcpy(in_a, "function") ;
       get_token_n ( inln , " (,", 1, subprogram_name ) ;
       handle_subprogram ( &fp, infl, &nargs, subprogram_name, inln, 2 ) ;
    } else if ( COMPARE( inln , "recursive subroutine " ) ) {
       strcpy(in_a, "recursive subroutine") ;
       get_token_n ( inln , " (,", 2, subprogram_name ) ;
       handle_subprogram ( &fp, infl, &nargs, subprogram_name, inln, 3 ) ;
    } else if ( contains_str ( inln, "startofregistrygeneratedinclude" ) && contains_str ( inln, "i1_decl.inc" )) {
       if ( strlen( subprogram_name ) > 0 ) {
         fprintf(fp, "contains_i1_declarations\n" ) ;
       }
    } else if ( contains_str ( inln, "! begin scalar derefs" ) ) {
       looking_scalar_derefs = 1 ;
    } else if ( contains_str ( inln, "! end scalar derefs" ) ) {
       looking_scalar_derefs = 0 ;
    } else if ( contains_str ( inln, "<description>" ) && protex_state == 0 ) {
       protex_state = 1 ;
       sprintf(description_name,"%s/%s_descrip",DIR, subprogram_name ) ;
       if ((fpdescription = fopen( description_name , "a" )) == NULL ) {
	 fprintf(stderr, "cannot open %s for writing\n", description_name ) ; exit(2) ; 
       }
       protex_state = 2 ;
    } else if ( contains_str ( inln, "</description>" ) ) {
       protex_state = 0 ;
       if ( fpdescription != NULL ) fclose( fpdescription ) ; 
       fpdescription = NULL ;
    } else if ( COMPARE( inln , "use " ) ) {
       if ( strlen( subprogram_name ) > 0 ) {
         get_token_n ( inln , " ", 1, tmp ) ;
         fprintf(fp, "use %s\n",tmp ) ;
       }
    } else if ( COMPARE( inln , "call " ) ) {
       get_token_n ( inln , " (,", 1, callee ) ;
       if ( ! contains_str( callee , ignore ) ) {
	 fprintf(fpcalls,"%s calls %s\n",subprogram_name, callee ) ;
         fprintf(fp,"%s calls %s\n",subprogram_name, callee ) ;
         for ( i = 0 ; get_arg_n ( inln , i, tmp ) ; i++ ) 
         { 
	   /* check to see if this is a dummy arg and print that info too */
	   strcpy(mess,"") ;
           for ( j = 0 ; j < nargs ; j++ )
	   {
	     if ( !strcmp( tmp, arg[j] ) )
	     {
	       sprintf( mess, " ( dummy arg %d, type %s ) ",j,type[j] ) ;
	       break ;
	     }
	   }
           fprintf(fp,"  actarg %d of callee %s is %s%s\n",i,callee, tmp,mess) ; 
         }
       }
    } else if ( COMPARE( inln , "integer " ) || COMPARE( inln , "real " ) || COMPARE( inln , "logical " ) ) {
         /* look for function */
       get_token_n ( inln , " ", 0, function_type ) ;
       get_token_n ( inln , " ,", 1, tmp ) ;
       if ( COMPARE( tmp, "function" ) )
       {
           strcpy(in_a,"function") ;
	   get_token_n ( inln, " (", 2, subprogram_name ) ;
           handle_subprogram ( &fp, infl, &nargs, subprogram_name, inln, 3 ) ;
       }
       else if ( strlen( subprogram_name ) > 0 && nargs > 0 ) {
         strcpy( typedefs[ntypedefs++], inln ) ;
       }
    } else if ( COMPARE( inln , "type " ) ) {
       if ( strlen( subprogram_name ) > 0 && nargs > 0 ) {
         strcpy( typedefs[ntypedefs++], inln ) ;
       }
    } else if ( COMPARE( inln , "end subroutine" ) ) {
       contained-- ;
       if ( contained == 0 ) {
         fprintf(fp,"Module: %s , Subroutine: %s \n",module_name, subprogram_name ) ;
         for ( i = 0 ; i < ntypedefs ; i++ )
         {
           if ( COMPARE( typedefs[i], "type" ) ) {
             get_token_n ( typedefs[i], ",", 0, tmp ) ;
             remove_whitespace( tmp ) ;
           } else {
             get_token_n ( typedefs[i], " ,", 0, tmp ) ;
           }
           set_attributes( typedefs[i], nargs, tmp ) ;
         }
         for ( i = 0 ; i < nargs ; i++ )
         {
	   fprintf(fp,"arg %d name %s type %s intent %s from %s dimensions %s\n", i, arg[i], type[i], intent[i], from[i], dimensions[i] ) ;
         }
         fclose(fp) ; fp = NULL ;
         strcpy( in_a, "" ) ;
         strcpy( subprogram_name, "" ) ;
       }
    } else if ( COMPARE( inln , "end function" ) ) {
       contained-- ;
       if ( contained == 0 ) {
         fprintf(fp,"Module: %s , Subroutine: %s \n",module_name, subprogram_name ) ;
         for ( i = 0 ; i < ntypedefs ; i++ )
         {
           get_token_n ( typedefs[i], " ,", 0, tmp ) ;
           set_attributes( typedefs[i], nargs, tmp ) ;
         }
         for ( i = 0 ; i < nargs ; i++ )
         {
           fprintf(fp,"arg %d name %s type %s intent %s from %s dimensions %s\n", i, arg[i], type[i], intent[i], from[i], dimensions[i] ) ;
         }
         fclose(fp) ; fp = NULL ;
         strcpy( in_a, "" ) ;
         strcpy( subprogram_name, "" ) ;
       }
    } else if ( COMPARE( inln , "end program" ) ) {
       contained-- ;
       if ( contained == 0 ) {
         fprintf(fp,"Module: %s , Subroutine: %s \n",module_name, subprogram_name ) ;
         for ( i = 0 ; i < ntypedefs ; i++ )
         {
           get_token_n ( typedefs[i], " ,", 0, tmp ) ;
           set_attributes( typedefs[i], nargs, tmp ) ;
         }
         for ( i = 0 ; i < nargs ; i++ )
         {
	   fprintf(fp,"arg %d name %s type %s intent %s from %s dimensions %s\n", i, arg[i], type[i], intent[i], from[i], dimensions[i] ) ;
         }
         fclose(fp) ; fp = NULL ;
         strcpy( in_a, "" ) ;
         strcpy( subprogram_name, "" ) ;
       }
#if 1
    } else if ( COMPARE( inln , "end" ) ) {  /* bare end -- take a chance and hope it's a subroutine */
       remove_whitespace( inln ) ;   /* make sure it's not an enddo, endif, etc */
       if ( COMPARE2 (inln , "end" ) ) {
         contained-- ;
         if ( contained == 0 ) {
           fprintf(fp,"Module: %s , Subroutine: %s \n",module_name, subprogram_name ) ;
           for ( i = 0 ; i < ntypedefs ; i++ )
           {
             if ( COMPARE( typedefs[i], "type" ) ) {
               get_token_n ( typedefs[i], ",", 0, tmp ) ;
               remove_whitespace( tmp ) ;
             } else {
               get_token_n ( typedefs[i], " ,", 0, tmp ) ;
             }
             set_attributes( typedefs[i], nargs, tmp ) ;
           }
           for ( i = 0 ; i < nargs ; i++ )
           {
             fprintf(fp,"arg %d name %s type %s intent %s from %s dimensions %s\n", i, arg[i], type[i], intent[i], from[i], dimensions[i] ) ;
           }
           fclose(fp) ; fp = NULL ;
           strcpy( in_a, "" ) ;
           strcpy( subprogram_name, "" ) ;
         }
       }
#endif
    }
  }
  fclose( fpcalls ) ; fpcalls = NULL ;
}

