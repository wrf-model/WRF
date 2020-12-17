#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>

#define NAMELEN 132
#define JLEN 32

typedef struct tuv_node_struct {
  char name[NAMELEN];
  int  ndx;
  int  dup;
  struct tuv_node_struct *next;
} tuv_node;

typedef struct wrf_node_struct {
  char name[NAMELEN];
  int  ndx;
  struct wrf_node_struct *next;
  struct tuv_node_struct *tuv_node;
} wrf_node;

int tuv_match( wrf_node *head, char *match_name );

int main( int argc, char *argv[], char *env[] ) {
  char fname_in[NAMELEN], dir[NAMELEN], fname_inc[NAMELEN];
  char inln[NAMELEN], outln[NAMELEN], piece[JLEN];
  char squezzed[NAMELEN];
  char *wrf_jname, *cwrk, *tuv_jspec, *token;
  char *tuv_jname;
  FILE * fp_in, *fp_set, *fp_def;
  wrf_node *Wrf_node;
  wrf_node *Wrf_HEAD;
  tuv_node *Tuv_node, *Tuv_node_wrk;

  int l, m, n, nwrf, ntuv, nltuv;
  int j_o2_ndx;

  char mech[NAMELEN];

  strcpy( fname_in , "" ) ;

  argv++;
  strcpy( mech,*argv );

  fprintf(stderr,"tuv_kpp: Argument = %s\n",mech);
// open and write inc files

  if( !strcmp(mech,"LAST") )
    strcpy(fname_inc,"../../inc/tuv2wrf_jvals.inc");
  else
    strcpy(fname_inc,"../../../../inc/tuv2wrf_jvals.inc");
  if( (fp_set = fopen( fname_inc,"a" )) == NULL ) {
    fprintf(stderr,"Can not open %s\n",fname_inc );
    return(-1);
  }
  if( !strcmp(mech,"LAST") )
    strcpy(fname_inc,"../../inc/tuvdef_jvals.inc");
  else
    strcpy(fname_inc,"../../../../inc/tuvdef_jvals.inc");
  if( (fp_def = fopen( fname_inc,"a" )) == NULL ) {
    fprintf(stderr,"Can not open %s\n",fname_inc );
    return(-1);
  }

  if( !strcmp(mech,"FIRST") ) {
    fprintf(fp_set,"   select case( config_flags%%chem_opt )\n");
    fprintf(fp_def,"   select case( config_flags%%chem_opt )\n");
  }
  else if( !strcmp(mech,"LAST") ) {
    fprintf(fp_set,"   end select\n");
    fprintf(fp_def,"   end select\n");
  }
  else {
    fprintf(stderr,"tuv_kpp: Mechanism = %s\n",mech);
    sprintf( fname_in,"%s.tuv.jmap",mech );
    if( (fp_in = fopen( fname_in,"r" )) == NULL ) {
      fprintf(stderr,"File %s does not exist\n",fname_in );
      return(-1);
    }

    Wrf_node = (wrf_node *)malloc( sizeof(wrf_node) );
    if( Wrf_node == NULL ) {
      fprintf(stderr,"Failed to allocate Wrf_node\n");
      return(-1);
    }
    Wrf_HEAD = Wrf_node;

    nwrf = 0; ntuv = 0;
    while( fgets( inln,NAMELEN,fp_in ) != NULL ) {
      if( nwrf > 0 ) {
        Wrf_node->next = (wrf_node *)malloc( sizeof(wrf_node) );
        if( Wrf_node == NULL ) {
          fprintf(stderr,"Failed to allocate Wrf_node\n");
          return(-1);
        }
        Wrf_node = Wrf_node->next;
      }
// remove white space from input line
      l = 0;
      for( m = 0; m < strlen( inln ); m++ ) {
        if( inln[m] != ' ' )
          squezzed[l++] = inln[m]; 
      }
      squezzed[l-1] = '\0';
      tuv_jspec = index( squezzed,':' );
      if( tuv_jspec == NULL ) {
        fprintf(stderr,"Input j mapping is invalid\n");
        return(-1);
      }
      *tuv_jspec = '\0';
      tuv_jspec++;
      strcpy( Wrf_node->name,squezzed );
      nwrf++;
      Wrf_node->ndx = nwrf;
      token = strtok( tuv_jspec,"+" );
      nltuv = 0;
      for( ;; ) {
        if( token != NULL ) {
          Tuv_node_wrk = (tuv_node *)malloc( sizeof(tuv_node) );
          if( Tuv_node_wrk == NULL ) {
            fprintf(stderr,"Failed to allocate Tuv_node\n");
            return(-1);
          }
          strcpy( Tuv_node_wrk->name,token );
          n = tuv_match( Wrf_HEAD, token );
          if( n == 0 ) {
            ntuv++;
            Tuv_node_wrk->ndx = ntuv;
          }
          else {
            Tuv_node_wrk->dup = 1;
            Tuv_node_wrk->ndx = n;
          }
          if( nltuv == 0 ) Wrf_node->tuv_node = Tuv_node_wrk;
          else Tuv_node->next = Tuv_node_wrk;
          Tuv_node = Tuv_node_wrk;
          token = strtok( NULL,"+" );
          nltuv++;
        }
        else break;
      }
    }

    fclose( fp_in );

// enumerate the wrf jspecs

//  fprintf(stderr,"\n");
//  fprintf(stderr,"WRF photo rates\n");
//  for( Wrf_node = Wrf_HEAD; Wrf_node != NULL; Wrf_node = Wrf_node->next ) {
//    fprintf(stderr,"%s\n",Wrf_node->name);
//  }

// enumerate the tuv jspecs

//  fprintf(stderr,"\n");
//  fprintf(stderr,"TUV photo rates\n");
//  for( Wrf_node = Wrf_HEAD; Wrf_node != NULL; Wrf_node = Wrf_node->next ) {
//    for( Tuv_node = Wrf_node->tuv_node; Tuv_node != NULL; Tuv_node = Tuv_node->next ) {
//      fprintf(stderr,"%s\n",Tuv_node->name);
//    }
//}

// write inc files

    fprintf(fp_set,"     case( %s_kpp )\n",mech);
    fprintf(fp_def,"     case( %s_kpp )\n",mech);
    n = 0;
    j_o2_ndx = 0;
    for( Wrf_node = Wrf_HEAD; Wrf_node != NULL; Wrf_node = Wrf_node->next ) {
      sprintf(outln,"       ph_%s(i,kts:kte,j) = ",Wrf_node->name);
      if( Wrf_node == Wrf_HEAD) {
        fprintf(fp_def,"       nj = %d\n",ntuv);
        fprintf(fp_def,"       allocate( tuv_jname(nj) )\n");
      }
      nltuv = 0;
      for( Tuv_node = Wrf_node->tuv_node; Tuv_node != NULL; Tuv_node = Tuv_node->next ) {
        if( nltuv == 0 ) 
          sprintf(piece,"tuv_prate(kts:kte,%d)",Tuv_node->ndx);
        else
          sprintf(piece," + tuv_prate(kts:kte,%d)",Tuv_node->ndx);
        strcat( outln,piece );
        nltuv++;
        if( !Tuv_node->dup ) {
          n++;
          fprintf(fp_def,"       tuv_jname(%d) = '%s'\n",n,Tuv_node->name);
          if( !strcmp( Tuv_node->name,"j_o2" ) )
            j_o2_ndx = n;
        }
      }
      fprintf(fp_set,"%s\n",outln);
    }
    fprintf(fp_def,"       j_o2_ndx = %d\n",j_o2_ndx);
  }
  fclose( fp_set );
  fclose( fp_def );
  return(0);
}

int tuv_match( wrf_node *head, char *match_name ) {

  wrf_node *wrfnode;
  tuv_node *thisnode;

  for( wrfnode = head; wrfnode != NULL; wrfnode = wrfnode->next ) {
    for( thisnode = wrfnode->tuv_node; thisnode != NULL; thisnode = thisnode->next ) {
      if( !strcmp( thisnode->name,match_name ) ) 
        return( thisnode->ndx );
    }
  }

  return( 0 );
}
