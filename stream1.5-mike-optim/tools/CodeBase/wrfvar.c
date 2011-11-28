#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/wait.h>
#include <string.h>

#define COMPARE(A,B) ( ! strncmp ( A , B , strlen( B ) ) )
#define COMPARE2(A,B) ( ! strcmp ( A , B ) )
#define INLINELEN (4*8192)
#define VARLEN 128
#define MAXARGS (4*8192)


#define DBDIR "tools/code_dbase"

int sw_all  = 0 ;


main( int argc, char *argv[] )
{
  FILE *fp ;
  FILE *calls ; 
  char fname[VARLEN], syscom[VARLEN] ;
  char *rout , *vname ;
  if ( argc < 2 || argc > 5 || ( argc == 2 && ! COMPARE2( argv[1] , "rebuild" ) ) ) {
    printf("usage : wrfvar varname routinename\n" ) ;
    printf("        wrfvar rebuild\n" ) ;
    exit(2) ;
  }
  vname = argv[1] ;
  rout = argv[2] ;
  if ( argc == 4 && argv[3] != NULL ) {
    if ( COMPARE2( argv[3] , "all" ) ) sw_all = 1 ;
  }
  sprintf( fname, "%s/calls", DBDIR ) ;
  if (( fp = fopen ( fname , "r" )) == NULL || COMPARE2( argv[1], "rebuild" ) ) {
    int rc ;
    printf("Building code database ... please wait\n") ;
    sprintf( syscom, "cd tools/CodeBase ; make" ) ;
    rc = system( syscom ) ;
    if ( WEXITSTATUS( rc ) ) { exit(3) ; }
    sprintf( syscom, "tools/build_codebase" ) ;
    rc = system( syscom ) ;
    if ( WEXITSTATUS( rc ) ) { exit(3) ; }
    sprintf( syscom, "ln -sf tools/wrfvar ." ) ;
    sprintf( syscom, "ln -sf tools/subinfo ." ) ;
    if ( COMPARE2( argv[1] , "rebuild" ) ) exit ;
  }
  fclose( fp ) ;
  lower_case_str ( vname ) ;
  lower_case_str ( rout ) ;
  printf("<h4>Trace upwards through call tree for %s</h4><p>\n",vname ) ;
  wrfvar ( vname, rout, 0 ) ;
}

wrfvar ( char * vname, char *rout, int recursion_level )
{
  FILE *ROUT ;
  FILE *BBB ;
  FILE *ELEF ;
  FILE *CALLER ;
  FILE *CALLERS ;
  FILE *REGISTRY ;
  DIR  *dir ;
  char inln[INLINELEN], inln2[INLINELEN], inln3[INLINELEN] ;
  int i ;
  char fname[VARLEN], fname2[VARLEN], sf[VARLEN] ;
  char vv[VARLEN], vv2[VARLEN] ;
  char u0[VARLEN] , u1[VARLEN] , u2[VARLEN] ;
  char v0[VARLEN] , v1[VARLEN] , v2[VARLEN] ;
  char r[12][VARLEN], t[12][VARLEN], u[12][VARLEN], v[12][VARLEN] ;
  char routfile[VARLEN] ;
  char tmp[VARLEN], darg[VARLEN], dintent[VARLEN] ;
  char hamuna[VARLEN] ;
  char rout1[VARLEN], rout2[VARLEN], rout3[VARLEN] ;
  char sourcefile[VARLEN], sourcefile_caller[VARLEN] ;
  char s1[VARLEN], s2[VARLEN], s3[VARLEN] ;
  char * p, * q, * q1, prev ;
  int found_var, nargs_rout, argn, callno, more_calls, first_time ;
  int contains_i1_declarations ;

  if (( dir = opendir ( DBDIR )) == NULL ) {
    fprintf(stderr, "Must be in top level WRF directory\n") ; exit(2) ;
  } closedir( dir ) ;

  strcpy( rout1, rout ) ;
  strcpy( vv, vname ) ;
  strcpy( vv2, vname ) ;
  remove_whitespace( vv2 ) ;
  /* remove arguments */
  if ((q = strchr( vv2 , '(' )) != NULL ) *q = '\0' ;
  /* remove time level if there */
  if (( q = strrchr( vv2, '_' )) != NULL ) {
    if ( COMPARE2( q , "_1" ) || COMPARE2( q , "_2" ) || COMPARE2( q , "_3" ) ) *q = '\0' ;
  }
  if ( COMPARE( vv2, "grid%" ) || !strcmp( rout, "registry_i1" )) {
    if (( REGISTRY = fopen( "Registry/Registry" , "r" )) == NULL ) {
          fprintf(stderr,"can not open Registry/Registry\n") ; exit(2) ; }
    strcpy( inln, "" ) ;
    while ( fgets( inln2, INLINELEN, REGISTRY ) != NULL ) {
      int inquote ;
      strcat( inln, inln2 ) ;
      if (( q = strrchr ( inln, '\\' )) != NULL ) {   /* continuation */
        *q = '\0' ; continue ; 
      }
      if (( q = strchr( inln, '#' )) != NULL ) *q = '\0' ;
      inquote = 0 ;
      for ( p = inln, q = inln2 ; *p ; p++, q++ ) {
        if      ( ! inquote && *p == '"' ) { inquote = 1 ; *p = ' ' ; }
        else if (   inquote && *p == '"' ) { inquote = 0 ; *p = ' ' ; }
        if ( *p == ' ' && inquote ) { *q = '`' ; }
        else                        { *q = *p ; }
      }
      *q = '\0' ;
      for ( i = 0 ; i < 11 ; i++ ) {
        strcpy( r[i] , "" ) ;
        get_token_n( inln2, " ", i , r[i] ) ; remove_nl(r[i]) ; 
        if ( i < 10 ) lower_case_str( r[i] ) ;
      }
      if ( COMPARE2 ( r[0], "state" ) ) {
        if ( COMPARE ( r[4], "dyn_" ) ) {
          /* if core associated */
          sprintf(s1,"%s_",&(r[4][4])) ;
          i = strlen(&(r[4][4])) ;
#if 1
          { char *x , *y ; int j ; 
            for ( x = vv2+5 , y = s3 , j = 0 ; j < i ; j++ ) { *y++ = *x++ ; }
            *y = '\0' ;
          }
#else
/* is there a bug in this?? */
          strncpy( s3, vv2+5, i ) ;
fprintf(stderr,"X %s <- %s %d\n", s3, vv2, i ) ;
#endif
          sprintf(s2,"%s_",s3) ;
          if ( COMPARE2 ( s1, s2 )  &&
                COMPARE2 ( vv2+5+(strlen(r[4])-3), r[2] ) ) {
            for (p = r[9]  ; *p ; p++ ) { if ( *p == '`' ) *p = ' ' ; }
            for (p = r[10] ; *p ; p++ ) { if ( *p == '`' ) *p = ' ' ; }
            printf("%3d. <b>Registry-defined</b>: <class> %s <type> %s <varname> %s <description> \"%s\" <units> \"%s\"<br>\n",
                   recursion_level+1, r[0], r[1], r[2], r[9], r[10] ) ;
          }
        } else {
          /* if not core associated */
          if ( COMPARE2 ( vv2+5, r[2] ) ) {
            for (p = r[9]  ; *p ; p++ ) { if ( *p == '`' ) *p = ' ' ; }
            for (p = r[10] ; *p ; p++ ) { if ( *p == '`' ) *p = ' ' ; }
            printf("%3d. <b>Registry-defined</b>: <class> %s <type> %s <varname> %s <description> \"%s\" <units> \"%s\"<br>\n",
                   recursion_level+1, r[0], r[1], r[2], r[9], r[10] ) ;
          }
        }
      } else if ( COMPARE2 ( r[0], "rconfig" ) ) {
          if ( COMPARE2 ( vv2+5, r[2] ) ) {
            for (p = r[8]  ; *p ; p++ ) { if ( *p == '`' ) *p = ' ' ; }
            printf("%3d. <b>Registry-defined</b>: <class> %s <type> %s <varname> %s <description> \"%s\" <br>\n",
                   recursion_level+1, r[0], r[1], r[2], r[8] ) ;
          }
      } else if ( COMPARE2 ( r[0], "i1" ) && !strcmp( rout, "registry_i1" )) {
          if ( COMPARE2 ( vv2, r[2] ) ) {
            for (p = r[9]  ; *p ; p++ ) { if ( *p == '`' ) *p = ' ' ; }
            for (p = r[10] ; *p ; p++ ) { if ( *p == '`' ) *p = ' ' ; }
            printf("%3d. <b>Registry-defined</b>: <class> %s <type> %s <varname> %s <description> \"%s\" <units> \"%s\"<br>\n",
                   recursion_level+1, r[0], r[1], r[2], r[9], r[10] ) ;
          }
      }

      strcpy( inln, "" ) ;
    }
    fclose( REGISTRY ) ;
    return ;
  }

  sprintf( routfile, "%s/%s", DBDIR, rout ) ;
  strcpy ( sourcefile , "" ) ;
  found_var = 0 ;
  nargs_rout = 0 ;
  if (( ROUT = fopen( routfile, "r" )) == NULL ) return ;
  {
    contains_i1_declarations = 0 ;
    while ( fgets( inln, INLINELEN, ROUT ) != NULL ) {
      remove_nl ( inln ) ;
      /* find first non space */
      for ( p = inln ; *p ; p++ ) { if ( *p != ' ' ) break ; }
      /* change multiple spaces to single space */
      for ( q = p, q1 = q , prev = *p ; *q ; q++ ) 
	 { if ( prev == ' ' && *q == ' ' ) { continue ; } else { prev = *q ; *q1++ = *q ; } }
      strcpy( inln, p ) ;
      for ( i = 0 ; i < 11 ; i++ ) {
        strcpy( t[i] , "" ) ;
        get_token_n( inln, " ", i , t[i] ) ; remove_nl(t[i]) ; lower_case_str( t[i] ) ;
      }
      if ( COMPARE2( "contains_i1_declarations", t[0] ) ) {
        contains_i1_declarations = 1 ;
      } else if ( COMPARE2( "sourcefile" , t[0] ) ) {
	strcpy ( sourcefile , t[1] ) ;
      } else if ( COMPARE2( "arg" , t[0] ) ) {
	nargs_rout ++ ;
	if ( COMPARE2( t[3] , vname ) && ! COMPARE2( t[9] , "registry" ) ) {
	  argn = atoi( t[1] ) ;
	  printf("%3d. <b>%s</b> is dummy arg %d of %s (%s);\n", recursion_level+1, vname, argn+1, rout, sourcefile ) ;
	  found_var = 1 ;
	  fclose( ROUT ) ;
	  sprintf(fname,"%s/calls", DBDIR ) ;
	  strcpy( rout2, rout ) ;
	  if (( CALLERS = fopen( fname , "r" )) == NULL ) return ;
          while ( fgets( inln2, INLINELEN, CALLERS ) != NULL ) {
            for ( i = 0 ; i < 11 ; i++ ) {
              strcpy( u[i] , "" ) ;
              get_token_n( inln2, " ", i , u[i] ) ; remove_nl(u[i]) ; lower_case_str( u[i] ) ;
            }
	    if ( COMPARE2( u[2], rout2 ) ) {
	      strcpy( rout , u[0] ) ;
	      sprintf( fname, "%s/%s", DBDIR, rout ) ;
	      if (( ROUT = fopen( fname, "r" )) == NULL ) return ;
  	      strcpy ( sourcefile_caller, "" ) ;
	      callno = 1 ;
              more_calls = 0 ;
	      while ( fgets( inln3, INLINELEN, ROUT ) != NULL ) {
                for ( i = 0 ; i < 11 ; i++ ) {
                  strcpy( v[i] , "" ) ;
                  get_token_n( inln3, " ", i , v[i] ) ; remove_nl(v[i]) ; lower_case_str( v[i] ) ;
                }
		if        ( COMPARE2( v[0] , "sourcefile" ) ) {
		  strcpy( sourcefile_caller, v[1] ) ;
		} else if ( COMPARE2( v[0] , "actarg") && ( COMPARE2( v[4] , rout2 ) && atoi( v[1] ) == argn )) {
                  if ( callno == 1 || sw_all ) {
		    printf("     corresponding actual arg is <b>%s</b>, arg number %d in call %d by %s (%s).<br>\n",
                              v[6],argn,callno,rout2,sourcefile_caller) ;
		     /* RECURSION */
		    wrfvar ( v[6], rout, recursion_level+1 ) ;
                  } else if ( callno >= 2 ) {
                    more_calls = callno ;
                  }
		  callno++ ;
		}
	      }
	      fclose( ROUT ) ;
              if ( more_calls > 1 && recursion_level == 0 ) {
                printf("  there are %d more calls to %s from %s.  Try 'wrfvar %s %s all' to see all.\n", more_calls, rout2, rout, vname, rout2 ) ;
              }
	    }
	  }
	  fclose( CALLERS ) ;
	} else if ( COMPARE2( t[3] , vname ) && COMPARE2( t[9] , "registry" ) ) {
	  /* RECURSION */
          sprintf(tmp, "grid%%s", vname ) ;
	  wrfvar ( vname, "registry", recursion_level+1 ) ;
	  found_var = 1 ;
	}
      }
    }
  }
  if ( found_var == 0 ) {
    if ( contains_i1_declarations ) {
      /* take a look in the registry for i1 vars that might match */
      wrfvar ( vname, "registry_i1", recursion_level ) ;  /* recursion level does not increase here, since we're checking the registry */
    } else {
      printf("%s is not an argument to %s. May be local or use-associated.\n",vname,rout1 ) ;
      printf("%s has %d arguments\n",rout1,nargs_rout) ;
      fclose(ROUT) ;
      if (( ROUT = fopen( routfile , "r" )) == NULL ) return ;
      while ( fgets( inln2, INLINELEN, ROUT ) != NULL ) {
        remove_nl( inln2 ) ;
        /* find first non space */
        for ( p = inln2 ; *p ; p++ ) { if ( *p != ' ' ) break ; }
        /* change multiple spaces to single space */
        for ( q = p, q1 = q , prev = *p ; *q ; q++ ) { if ( prev == ' ' && *q == ' ' ) { continue ; } else { prev = *q ; *q1++ = *q ; } }
        for ( i = 0 ; i < 11 ; i++ ) {
          strcpy( r[i] , "" ) ;
          get_token_n( inln2, " ", i , r[i] ) ; remove_nl(r[i]) ; lower_case_str( r[i] ) ;
        }
        if ( COMPARE2( r[0] , "arg" ) ) {
	  i = atoi(r[1]) + 1 ;
	  printf("%3d. ",i) ; 
	  printf("%s of type %s intent %s\n",r[3],r[5],r[7]) ;
        }
      }
    fclose( ROUT ) ;
    }
  }

/* get a list of the routines this guy calls */

  if ( recursion_level == 0 ) {
    first_time = 1 ;
    if (( BBB = fopen( routfile, "r" )) == NULL ) return ;
    while ( fgets( inln2, INLINELEN, BBB ) != NULL ) {
      remove_nl( inln2 ) ;
      /* find first non space */
      for ( p = inln2 ; *p ; p++ ) { if ( *p != ' ' ) break ; }
      /* change multiple spaces to single space */
      for ( q = p, q1 = q , prev = *p ; *q ; q++ ) 
	   { if ( prev == ' ' && *q == ' ' ) { continue ; } else { prev = *q ; *q1++ = *q ; } }
      for ( i = 0 ; i < 11 ; i++ ) {
        strcpy( t[i] , "" ) ;
        get_token_n( inln2, " ", i , t[i] ) ; remove_nl(t[i]) ; lower_case_str( t[i] ) ;
      }
      if        ( COMPARE2( t[0] , rout1 ) && COMPARE2( t[1] , "calls" ) ) {
	strcpy( hamuna , t[2] ) ;
      } else if ( COMPARE2( t[0] , "actarg" ) && COMPARE2( t[6] , vname ) ) {
	if ( first_time ) {
	  printf("\n<h4>%s is an actual arg in calls to these routines from %s</h4>\n",vname,rout1) ;
	  first_time = 0 ;
	}
	sprintf(fname,"%s/%s",DBDIR,hamuna) ;
	if (( ELEF = fopen ( fname , "r" )) == NULL ) continue ;
	while ( fgets( inln3, INLINELEN, ELEF ) != NULL ) {
          remove_nl( inln3 ) ;
          /* find first non space */
          for ( p = inln3 ; *p ; p++ ) { if ( *p != ' ' ) break ; }
          /* change multiple spaces to single space */
          for ( q = p, q1 = q , prev = *p ; *q ; q++ ) 
	     { if ( prev == ' ' && *q == ' ' ) { continue ; } else { prev = *q ; *q1++ = *q ; } }
          for ( i = 0 ; i < 11 ; i++ ) {
            strcpy( u[i] , "" ) ;
            get_token_n( inln3, " ", i , u[i] ) ; remove_nl(u[i]) ; lower_case_str( u[i] ) ;
          }
	  if ( COMPARE2( u[0] , "arg" ) && COMPARE2( u[1] , t[1] ) ) {
	    strcpy( darg , u[3] ) ;
	    strcpy( dintent , u[7] ) ;
	    break ;
	  }
	}
	fclose( ELEF ) ;
	printf("  %s (argument %d ; matching dummy arg is %s with intent %s)\n",hamuna,atoi(t[1])+1,darg,dintent ) ;
      }
    }
    printf("\n") ;
    fclose(BBB) ;
  }
}



#if 0
#!/bin/perl

$dbdir = "tools/code_dbase" ;

if ( ! opendir( TOOLDIR, "tools") )  {
print "\nMust be in top level WRF directory\n" ;
exit ;
}
closedir TOOLDIR ;

if ( (scalar @ARGV  < 1 || scalar @ARGV > 3)  || (scalar @ARGV == 1 && @ARGV[0] ne "rebuild") )  {
print "usage: wrfvar varname routinename \n" ;
print "       wrfvar rebuild \n" ;
exit ;
}


if ( ! open( XXX, "$dbdir/calls" ) || $ARGV[0] eq "rebuild" )
{
  print "Building code database ... please wait.\n" ;
  system( "cd tools/CodeBase ; make" ) ;
  $rc = system( "tools/build_codebase" ) ;

  if ( ($rc >> 8) == 99 ) { exit ; }
  system( "ln -sf tools/wrfvar ." ) ;
  system( "ln -sf tools/subinfo ." ) ;

  if ( $ARGV[0] eq "rebuild" ) { exit ; }
}


$vname = lc $ARGV[0] ;
$vname1 = $vname ;
$rout1 = lc $ARGV[1] ;
$recursion_level = $ARGV[2] ;
$rout = $rout1 ;
#print $vname,"\n" ;
#print $rout,"\n" ;

$spc = "`" ;
$vv = $vname ;
$vv =~ s/\(.*// ;
#print $vv,"\n" ;
if ( substr($vv,0,5)  eq "grid%" ) {
    open REGISTRY, "< Registry/Registry" or die "cannot open Registry/Registry" ;
    while ( <REGISTRY> ) {

      $line = $_ ;
      $line =~ s/#.*// ;
      next if ( $line eq "" ) ;
      $line =~ s/[ \t][ \t]*/ /g ;
      $line = lc $line ;
      # fill in the blanks in quote delimited strings then remove 
      # the quotes so we can split on white space
      
      $inquote = 0 ;
      $newline = "" ;
      for ( $i = 0 ; $i < length($line) ; $i++ )
      {
        $ccc = substr($line,$i,1) ;
        if    ( ! $inquote && $ccc eq '"' ) { $inquote = 1 ; }
        elsif (   $inquote && $ccc eq '"' ) { $inquote = 0 ; }
        if ( $ccc eq " " && $inquote ) { $newline = $newline.$spc ; }
        else                           { $newline = $newline.$ccc ; }
      }
      $line = $newline ;
      $line =~ s/\"//g ;

      @r = split ( ' ',$line ) ;
      if ( ($r[0] eq state ) ) {
        if (( substr($r[4],0,4) eq "dyn_" && 
              substr($r[4],4,length($r[4])-4)."_" eq substr($vv,5,length($r[4])-4)."_" &&
              substr($vv,5+length($r[4])-4+1,length($r[2]))) eq $r[2] ) {

          $r[9] =~ s/`/ /g ;
          $r[9] = uc $r[9] ;
          $r[10] =~ s/`/ /g ;
          $r[10] = uc $r[10] ;
	  print "**  Registry Definition: <class> $r[0] <type> $r[1] <varname> ", uc $r[2]," <decription> \"$r[9]\" <units> \"$r[10]\"\n"
	}
      }
    }
    close REGISTRY ;
    exit ;
}

$routfile = $dbdir."/".$rout ;
open ROUT, "< $routfile" or die "can not open $routfile" ;

$sourcefile = "" ;
$found_var = 0 ;
$nargs_rout = 0 ;
while ( <ROUT> )
{
  s/^  *// ;
  s/  */ /g ;
  @t = split ' ' ;
  if ( $t[0] eq "sourcefile" ) {
    $sourcefile = $t[1] ;
  } elsif ( $t[0] eq "arg" ) {
    $nargs_rout++ ;
    if ( $t[3] eq $vname && $t[9] ne "registry" ) {
      $argn = $t[1] ;
      print "  ",uc $vname," is dummy argument $argn of $rout ($sourcefile)\n"  ;
      $found_var = 1 ;
      close ROUT ;
      system( "sort -u $dbdir/calls > /tmp/wrfvar-sort ; /bin/mv /tmp/wrfvar-sort $dbdir/calls" ) ;
      open CALLERS, "< $dbdir/calls" ;
      $rout2 = $rout ;
      while ( <CALLERS> ) {
        @u = split ' ' ;
        if ( $u[2] eq $rout2 )
        {
	  $rout = $u[0] ;
          $routfile = $dbdir."/".$rout ;
 	  open ROUT, "< $routfile" or die "can not open $routfile" ;
          $sourcefile_caller = "" ;
          $callno = 1 ;
	  while ( <ROUT> ) {
            @v = split ' ' ;
            if ( $v[0] eq "sourcefile" ) {
              $sourcefile_caller = $v[1] ;
            } elsif ( $v[0] eq 'actarg' && $v[4] eq $rout2 && $v[1] eq $argn ) {
 	      print ucfirst $rout2," call $callno by $rout ($sourcefile_caller) with actual argument $argn: ",uc $v[6],"\n" ;
              $callno++ ;
	      $vname = $v[6] ;
              ############## RECURSION ##############
              @sysargs = ( "tools/wrfvar" , $v[6], $rout, $recursion_level+1 )  ;
              system( @sysargs ) ; 
	    }
          }
          close ( ROUT ) ;
        }
      }
      close ( CALLERS ) ;
    } elsif ( $t[3] eq $vname && $t[9] eq "registry" ) {
      @sysargs = ( "tools/wrfvar" , "grid%".$vname, "registry", $recursion_level+1 ) ;
      ############## RECURSION ##############
      system( @sysargs ) ; 
      $found_var = 1 ;
    }
  }
}

if ( $found_var == 0 ) {
  print uc $vname , " is not an argument to ${rout1}.  May be local or use-associated.\n" ;
  print ucfirst $rout1," has $nargs_rout arguments.\n" ;
  close ROUT ;
  open ROUT, "< $routfile" or die "can not open $routfile" ;
  while ( <ROUT> )
  {
    s/^  *// ;
    s/  */ /g ;
    @t = split ' ' ;
    if ( $t[0] eq "arg" ) {
      $i = $t[1] + 1 ;
      printf("%3d. ",$i) ;
      print uc $t[3]," of type ", uc $t[5],", intent ",uc $t[7],"\n" ;
    }
  }
  close ROUT ;
}

# get a list of the routines this guy calls 

if ( $recursion_level == 0 ) {
$first_time = 1 ;
open BBB, "< $dbdir/$rout1" or die " cannot open $dbdir/$rout1" ;
while ( <BBB> ) {
  @t = split ' ' ;
  if      ( $t[0] eq "$rout1" && $t[1] eq calls ) {
     $hamuna = $t[2] ;
  } elsif ( $t[0] eq "actarg" && $t[6] eq $vname1 ) {
     if ( $first_time == 1 ) {
       print "\n",uc $vname1," is an actual argument in calls to these routines from ",uc $rout1," :\n" ;
       $first_time = 0 ;
     }
     open ELEF,"< $dbdir/$hamuna" or die "cannot open $dbdir/$hamuna"  ;
     while ( <ELEF> ) {
       @u = split ' ' ;
       if ( $u[0] eq arg && $u[1] eq $t[1] ) {
	  $darg = $u[3] ;
	  $dintent = $u[7] ;
       }
     }
     close ELEF ;
     print "  ", $hamuna," (argument ",$t[1]+1," ; matching dummy arg is ",uc $darg," with intent ",uc $dintent,") \n" ;
  }
}
print "\n" ;
close BBB ;
}
exit ;

#endif

