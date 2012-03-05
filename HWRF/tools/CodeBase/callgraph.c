#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include "sym.h"

#define DBDIR "tools/code_dbase"

#define COMPARE(A,B) ( ! strncmp ( A , B , strlen( B ) ) )
#define COMPARE2(A,B) ( ! strcmp ( A , B ) )
#define INLINELEN (4*8192)
#define VARLEN 256
#define MAXRECORDS 8192

int recno = 1 ;
char outbuf[MAXRECORDS][VARLEN] ;

main( int argc , char * argv[] )
{
  DIR *dir ;
  char root[VARLEN] ;
  int cut = 99 ;
  int nentries ;
  int i ;

  if (( dir = opendir ( DBDIR )) == NULL ) {
    fprintf(stderr, "Must be in top level WRF directory\n") ; exit(2) ;
  } closedir( dir ) ;
  if ( argc < 2 ) {
    fprintf(stderr,"usage: callgraph root\n") ;
  }
  sym_init() ;
  strcpy( root, argv[1] ) ;
  if ( argv[2] ) {
    cut = atoi( argv[2] ) ;
  } 
  upper_case_str( root ) ;
  lower_case_str( root ) ;
  callgraph( root , 0 , cut ) ;
  sprintf(outbuf[0],"var db = new makeArray(%d)\n",recno-1 ) ;
  for ( i = 0 ; i <= recno ; i++ )
    printf("%s",outbuf[i]) ; 
}

callgraph ( char * root , int indent , int cut )
{
  FILE * CALLEES ;
  char fname[VARLEN] ;
  char * p, * q, *q1, prev  ;
  char inln[INLINELEN] ;
  char t[12][VARLEN] ;
  int i ;
  char * doescall ;
  char tempbuf[VARLEN] ;
  int thisrec ;

  sprintf(fname,"%s/calls", DBDIR ) ;
/* skip some routines */
  if ( 
            COMPARE( root , "add_msg" ) ||  COMPARE( root , "reset_msgs" ) ||
            COMPARE( t[2] , "get_" )    ||  COMPARE( t[2] , "set_" )       ||
            COMPARE( t[2] , "mpi_" )    ||  COMPARE( t[2] , "ext_" )       ||
            COMPARE( root , "stencil" ) ||  COMPARE( root , "wrf_debug" )  ||
            COMPARE( root , "wrf_message" ) ||  COMPARE( root , "wrf_error" )
                                                                      )  return ;
  thisrec = recno++ ;
#if 0
  sprintf(tempbuf,"db[%4d] = new dbRecord( %%s, \"%s\", \"../../WRFV2/tools/code_dbase/%s.html\", %d )\n",
          thisrec,  root, root, indent ) ;
#else
  sprintf(tempbuf,"db[%4d] = new dbRecord( %%s, \"%s\", \"%s.html\", %d )\n",
          thisrec,  root, root, indent ) ;
#endif

  if (( CALLEES = fopen( fname , "r" )) == NULL ) return ;

  doescall = "false" ;
  while ( fgets( inln, INLINELEN, CALLEES ) != NULL ) {
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
    if ( COMPARE2( t[0] , root ) && COMPARE2( t[1] , "calls" ) && ! COMPARE2( t[0] , t[2] ) &&
         ! (
            COMPARE( root , "add_msg" ) ||  COMPARE( root , "reset_msgs" ) ||
            COMPARE( t[2] , "get_" )    ||  COMPARE( t[2] , "set_" )       ||
            COMPARE( t[2] , "mpi_" )    ||  COMPARE( t[2] , "ext_" )       ||
            COMPARE( root , "stencil" ) ||  COMPARE( root , "wrf_debug" )  ||
            COMPARE( root , "wrf_message" ) ||  COMPARE( root , "wrf_error" )
           )) {
      if ( indent <= cut && ( ! sym_get ( t[2] ) ) ) {
	sym_add( t[2] ) ;
        doescall = "true" ;
	callgraph ( t[2] , indent + 1, cut ) ;
      }
    }
  }
  sprintf(outbuf[thisrec],tempbuf,doescall) ;
  fclose( CALLEES ) ;
}

#if 0
/*******************************************************************/

/* open the file of calls and count them */
count_entries ( char * root , int * nentries )
{
  FILE * CALLEES ;
  char fname[VARLEN] ;
  char inln[INLINELEN] ;
  char * p, *q, *q1, prev ;
  char t[12][VARLEN] ;
  int i ;

  sprintf(fname,"%s/calls", DBDIR ) ;
  if (( CALLEES = fopen( fname , "r" )) == NULL ) return ;
  *nentries = 0 ;
  while ( fgets( inln, INLINELEN, CALLEES ) != NULL ) {
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
    if ( COMPARE2( t[0] , root ) && COMPARE2( t[1] , "calls" ) && ! COMPARE2( t[0] , t[2] ) &&
         ! (
            COMPARE( root , "add_msg" ) ||  COMPARE( root , "reset_msgs" ) ||
            COMPARE( t[2] , "get_" )    ||  COMPARE( t[2] , "set_" )       ||
            COMPARE( t[2] , "mpi_" )    ||  COMPARE( t[2] , "ext_" )       ||
            COMPARE( root , "stencil" ) ||  COMPARE( root , "wrf_debug" )  ||
            COMPARE( root , "wrf_message" ) ||  COMPARE( root , "wrf_error" )
           )) {
      sym_add( t[2] ) ;
      (*nentries)++ ;
    }
  }
  fclose( CALLEES ) ;
}



/* old version before adding javascript calltree */
main( int argc , char * argv[] )
{
  DIR *dir ;
  char root[VARLEN] ;
  int cut = 99 ;

  if (( dir = opendir ( DBDIR )) == NULL ) {
    fprintf(stderr, "Must be in top level WRF directory\n") ; exit(2) ;
  } closedir( dir ) ;
  if ( argc < 2 ) {
    fprintf(stderr,"usage: callgraph root\n") ;
  }
  sym_init() ;
  strcpy( root, argv[1] ) ;
  if ( argv[2] ) {
    cut = atoi( argv[2] ) ;
  } 
  upper_case_str( root ) ;
  printf("<html>\n" ) ;
  printf("<title> %s Call Tree </title>\n", root ) ;
  printf("<body>\n" ) ;
  printf("<h1> %s Call Tree </h1>\n", root ) ;
  printf("<p><a href=index.html>[1] </a> " ) ;
  printf("<a href=ct2.html>[2] </a> " ) ;
  printf("<a href=ct3.html>[3] </a> " ) ;
  printf("<a href=ct4.html>[4] </a> " ) ;
  printf("<a href=ct5.html>[5] </a> " ) ;
  printf("<a href=ctall.html>[all] </a><p>\n" ) ;
  lower_case_str( root ) ;
  callgraph( root , 0 , cut ) ;
  printf("</body>\n" ) ;
  printf("</html>\n" ) ;
}

callgraph ( char * root , int indent , int cut )
{
  FILE * CALLEES ;
  char fname[VARLEN] ;
  char * p, * q, *q1, prev  ;
  char inln[INLINELEN] ;
  char t[12][VARLEN] ;
  int i ;
  int recno = 1 ;

  sprintf(fname,"%s/calls", DBDIR ) ;
  if (( CALLEES = fopen( fname , "r" )) == NULL ) return ;
  if ( 
            COMPARE( root , "add_msg" ) ||  COMPARE( root , "reset_msgs" ) ||
            COMPARE( t[2] , "get_" )    ||  COMPARE( t[2] , "set_" )       ||
            COMPARE( t[2] , "mpi_" )    ||  COMPARE( t[2] , "ext_" )       ||
            COMPARE( root , "stencil" ) ||  COMPARE( root , "wrf_debug" )  ||
            COMPARE( root , "wrf_message" ) ||  COMPARE( root , "wrf_error" )
                                                                      )  return ;
  for ( i = 0 ; i < indent ; i++ ) printf(" ! &nbsp &nbsp &nbsp ") ;
  printf(" <a href=\"%s.html\"> %s </a><br>\n", root, root ) ;

  while ( fgets( inln, INLINELEN, CALLEES ) != NULL ) {
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
    if ( COMPARE2( t[0] , root ) && COMPARE2( t[1] , "calls" ) && ! COMPARE2( t[0] , t[2] ) &&
         ! (
            COMPARE( root , "add_msg" ) ||  COMPARE( root , "reset_msgs" ) ||
            COMPARE( t[2] , "get_" )    ||  COMPARE( t[2] , "set_" )       ||
            COMPARE( t[2] , "mpi_" )    ||  COMPARE( t[2] , "ext_" )       ||
            COMPARE( root , "stencil" ) ||  COMPARE( root , "wrf_debug" )  ||
            COMPARE( root , "wrf_message" ) ||  COMPARE( root , "wrf_error" )
           )) {
      if ( indent <= cut && ( ! sym_get ( t[2] ) ) ) {
	sym_add( t[2] ) ;
	callgraph ( t[2] , indent + 1, cut ) ;
      }

    }
  }
  fclose( CALLEES ) ;
}
#####################
# original old PERL code kept just for reference

$dbdir = "tools/code_dbase" ;

if ( ! opendir( TOOLDIR, "tools") )  {
  print "\nMust be in top level WRF directory\n" ;
  exit ;
}
closedir TOOLDIR ;

if ( ( scalar @ARGV  < 1 ) )  {
  print "usage: callgraph root\n" ;
  exit ;
}

$rout1 = lc $ARGV[0] ;
$rout = $rout1 ;

$routfile = $dbdir."/".$rout ;

if ( $indent == 0 ) {
print "<html>\n" ;
print "<title> ",uc $rout," Call Tree </title>\n" ;
print "<body>\n" ;
print "<h1> ",uc $rout," Call Tree </h1>\n" ;
}

$indent = 0 ;

$first = 1 ;

open CALLEES, "< $dbdir/calls" or die " cannot open $dbdir/calls " ;
while ( <CALLEES> ) { 
   @t = split ' ' ;
   if ( $t[0] eq lc $rout && $t[1] eq "calls" && ! ( $t[2] eq $t[0] ) && ! ($t[2] =~ add_msg) && !($t[2] =~ reset_msgs ) && ! ($t[2] =~ stencil) && !($t[2] =~ wrf_debug) ) {
     if ( $first == 1 ) {
       for ( $i = 0 ; $i < $indent ; $i++ ) { print "| &nbsp " ; }
       print "<a href=\"$rout\.html\"> $rout </a><br>\n" ;
       $first = 0 ;
     }
     $i2 = $indent + 1 ;
     if ( $i2 < 7 && (! $prune{$t[2]} ) ) {
        $prune{$t[2]} = "y" ;
        $opstr = "tools/callgraph $t[2] $i2 |" ;
        ####### RECURSE ##########
        open D, $opstr ;
        while ( <D> ) { print ; }
        close D ;
     }
   }
}
if ( $indent == 0 ) {
print "</body>\n" ;
print "</html>\n" ;
}

exit
#endif

