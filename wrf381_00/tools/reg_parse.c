#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
# define rindex(X,Y) strrchr(X,Y)
# define index(X,Y) strchr(X,Y)
#else
# include <strings.h>
#endif

#include "registry.h"
#include "protos.h"
#include "data.h"
#include "sym.h"

/* read in the Registry file and build the internal representation of the registry */

#define MAXTOKENS 5000 /*changed MAXTOKENS from 1000 to 5000 by Manish Shrivastava on 01/28/2010*/

/* fields for state entries (note, these get converted to field entries in the
   reg_parse routine; therefore, only TABLE needs to be looked at */
#define TABLE 0

/* fields for field entries (TABLE="typedef" and, with some munging,  TABLE="state") */
#define FIELD_OF        1
#define FIELD_TYPE     2
#define FIELD_SYM      3
#define FIELD_DIMS     4
#define FIELD_USE      5
#define FIELD_NTL      6
#define FIELD_STAG     7
#define FIELD_IO       8
#define FIELD_DNAME    9
#define FIELD_DESCRIP 10
#define FIELD_UNITS   11

#define F_OF       0
#define F_TYPE     1
#define F_SYM      2
#define F_DIMS     3
#define F_USE      4
#define F_NTL      5
#define F_STAG     6
#define F_IO       7
#define F_DNAME    8
#define F_DESCRIP  9
#define F_UNITS   10

/* fields for rconfig entries (RCNF) */
#define RCNF_TYPE_PRE       1
#define RCNF_SYM_PRE        2
#define RCNF_HOWSET_PRE     3
#define RCNF_NENTRIES_PRE   4
#define RCNF_DEFAULT_PRE    5
#define RCNF_IO_PRE         6
#define RCNF_DNAME_PRE      7
#define RCNF_DESCRIP_PRE    8
#define RCNF_UNITS_PRE      9

#define RCNF_TYPE       2
#define RCNF_SYM        3
#define RCNF_USE        FIELD_USE
#define RCNF_IO         FIELD_IO
#define RCNF_DNAME      FIELD_DNAME
#define RCNF_DESCRIP    FIELD_DESCRIP
#define RCNF_UNITS      FIELD_UNITS
#define RCNF_HOWSET    20
#define RCNF_NENTRIES  21
#define RCNF_DEFAULT   22

/* fields for dimension entries (TABLE="dimspec") */
#define DIM_NAME       1
#define DIM_ORDER      2
#define DIM_SPEC       3
#define DIM_ORIENT     4
#define DIM_DATA_NAME  5

#define PKG_SYM            1
#define PKG_ASSOC          2
#define PKG_STATEVARS      3
#define PKG_4DSCALARS      4

#define COMM_ID            1
#define COMM_USE           2
#define COMM_DEFINE        3

static int ntracers = 0 ;
static char tracers[1000][100] ;

int
pre_parse( char * dir, FILE * infile, FILE * outfile )
{
  /* Decreased size for SOA from 8192 to 8000--double check if necessary, Manish Shrivastava 2010 */
  char inln[8000], parseline[8000], parseline_save[8000] ;
  int found ; 
  char *p, *q ;
  char *tokens[MAXTOKENS], *toktmp[MAXTOKENS], newdims[NAMELEN_LONG], newdims4d[NAMELEN_LONG],newname[NAMELEN_LONG] ;
  int i, ii, len_of_tok ;
  char x, xstr[NAMELEN_LONG] ;
  int is4d, wantstend, wantsbdy ;
  int ifdef_stack_ptr = 0 ;
  int ifdef_stack[100] ;
  int inquote, retval ;

  ifdef_stack[0] = 1 ;
  retval = 0 ;

  parseline[0] = '\0' ;
/* main parse loop over registry lines */
  /* Had to increase size for SOA from 4096 to 7000, Manish Shrivastava 2010 */
  while ( fgets ( inln , 7000 , infile ) != NULL )
  {

/*** preprocessing directives ****/
    /* look for an include statement */
    for ( p = inln ; ( *p == ' ' || *p == '	' ) && *p != '\0' ; p++ ) ;
    if ( !strncmp( p , "include", 7 ) &&  ! ( ifdef_stack_ptr >= 0 && ! ifdef_stack[ifdef_stack_ptr] ) ) {
      FILE *include_fp ;
      char include_file_name[128] ;
      p += 7 ; for ( ; ( *p == ' ' || *p == '	' ) && *p != '\0' ; p++ ) ;
      if ( strlen( p ) > 127 ) { fprintf(stderr,"Registry warning: invalid include file name: %s\n", p ) ; }
      else {
        sprintf( include_file_name , "%s/%s", dir , p ) ;
        if ( (p=index(include_file_name,'\n')) != NULL ) *p = '\0' ;
        fprintf(stderr,"opening %s\n",include_file_name) ;
        if (( include_fp = fopen( include_file_name , "r" )) != NULL ) {

          fprintf(stderr,"including %s\n",include_file_name ) ;
          pre_parse( dir , include_fp , outfile ) ;

          fclose( include_fp ) ;
        } else {
          fprintf(stderr,"Registry warning: cannot open %s. Ignoring.\n", include_file_name ) ;
        } 
      }
    }
    else if ( !strncmp( p , "ifdef", 5 ) ) {
      char value[32] ;
      p += 5 ; for ( ; ( *p == ' ' || *p == '	' ) && *p != '\0' ; p++ ) ;
      strncpy(value, p, 31 ) ; value[31] = '\0' ;
      if ( (p=index(value,'\n')) != NULL ) *p = '\0' ;
      if ( (p=index(value,' ')) != NULL ) *p = '\0' ; if ( (p=index(value,'	')) != NULL ) *p = '\0' ; 
      ifdef_stack_ptr++ ;
      ifdef_stack[ifdef_stack_ptr] = ( sym_get(value) != NULL && ifdef_stack[ifdef_stack_ptr-1] ) ;
      if ( ifdef_stack_ptr >= 100 ) { fprintf(stderr,"Registry fatal: too many nested ifdefs\n") ; exit(1) ; }
      continue ;
    }
    else if ( !strncmp( p , "ifndef", 6 ) ) {
      char value[32] ;
      p += 6 ; for ( ; ( *p == ' ' || *p == '	' ) && *p != '\0' ; p++ ) ;
      strncpy(value, p, 31 ) ; value[31] = '\0' ;
      if ( (p=index(value,'\n')) != NULL ) *p = '\0' ;
      if ( (p=index(value,' ')) != NULL ) *p = '\0' ; if ( (p=index(value,'	')) != NULL ) *p = '\0' ; 
      ifdef_stack_ptr++ ;
      ifdef_stack[ifdef_stack_ptr] = ( sym_get(value) == NULL && ifdef_stack[ifdef_stack_ptr-1] ) ;
      if ( ifdef_stack_ptr >= 100 ) { fprintf(stderr,"Registry fatal: too many nested ifdefs\n") ; exit(1) ; }
      continue ;
    }
    else if ( !strncmp( p , "endif", 5 ) ) {
      ifdef_stack_ptr-- ; 
      if ( ifdef_stack_ptr < 0 ) { fprintf(stderr,"Registry fatal: unmatched endif\n") ; exit(1) ; }
      continue ;
    }
    else if ( !strncmp( p , "define", 6 ) ) {
      char value[32] ;
      p += 6 ; for ( ; ( *p == ' ' || *p == '	' ) && *p != '\0' ; p++ ) ;
      strncpy(value, p, 31 ) ; value[31] = '\0' ;
      if ( (p=index(value,'\n')) != NULL ) *p = '\0' ;
      if ( (p=index(value,' ')) != NULL ) *p = '\0' ; if ( (p=index(value,'	')) != NULL ) *p = '\0' ; 
      sym_add( value ) ;
      continue ;
    }
    if ( ifdef_stack_ptr >= 0 && ! ifdef_stack[ifdef_stack_ptr] ) continue ;
/*** end of preprocessing directives ****/

    strcat( parseline , inln ) ;

    /* allow \ to continue the end of a line */
    if (( p = index( parseline,  '\\'  )) != NULL )
    {
      if ( *(p+1) == '\n' || *(p+1) == '\0' )
      {
        *p = '\0' ;
        continue ;  /* go get another line */
      }
    }
    make_lower( parseline ) ;

    if (( p = index( parseline , '\n' )) != NULL  ) *p = '\0' ; /* discard newlines */

    /* check line and zap any # characters that are in double quotes */

    for ( p = parseline, inquote = 0 ; *p ; p++ ) {
      if      ( *p == '"' && inquote ) inquote = 0 ;
      else if ( *p == '"' && !inquote ) inquote = 1 ;
      else if ( *p == '#' && inquote ) *p = ' ' ;
      else if ( *p == '#' && !inquote ) { *p = '\0' ; break ; }
    }
    if ( inquote ) { retval=1 ; fprintf(stderr,"Registry error: unbalanced quotes in line:\n%s\n",parseline) ;}

    for ( i = 0 ; i < MAXTOKENS ; i++ ) tokens[i] = NULL ;
    i = 0 ;

    strcpy( parseline_save, parseline ) ;

    if ((tokens[i] = my_strtok(parseline)) != NULL ) i++ ;
    while (( tokens[i] = my_strtok(NULL) ) != NULL && i < MAXTOKENS ) i++ ;
    if ( i <= 0 ) continue ;

    for ( i = 0 ; i < MAXTOKENS ; i++ )
    {
      if ( tokens[i] == NULL ) tokens[i] = "-" ;
    }
/* remove quotes from quoted entries */
    for ( i = 0 ; i < MAXTOKENS ; i++ )
    {
      char * pp ;
      if ( tokens[i][0] == '"' ) tokens[i]++ ;
      if ((pp=rindex( tokens[i], '"' )) != NULL ) *pp = '\0' ;
    }
    if      ( !strcmp( tokens[ TABLE ] , "state" ) )
    {
        int inbrace = 0 ;
        strcpy( newdims, "" ) ;
        strcpy( newdims4d, "" ) ;
        is4d = 0 ; wantstend = 0 ; wantsbdy = 0 ; 
        for ( i = 0 ; i < (len_of_tok = strlen(tokens[F_DIMS])) ; i++ )
        {
          x = tolower(tokens[F_DIMS][i]) ;
          if ( x == '{' ) { inbrace = 1 ; }
          if ( x == '}' ) { inbrace = 0 ; }
          if ( x >= 'a' && x <= 'z' && !inbrace ) {
            if ( x == 'f' ) { is4d = 1 ; }
            if ( x == 't' ) { wantstend = 1 ; }
            if ( x == 'b' ) { wantsbdy = 1 ; }
          }
          sprintf(xstr,"%c",x) ;
          if ( x != 'b' || inbrace ) strcat ( newdims , xstr ) ;
          if ( x != 'f' && x != 't' || inbrace ) strcat( newdims4d , xstr ) ;

        }
        if ( wantsbdy ) {


/* first re-gurg the original entry without the b in the dims */

 fprintf( outfile,"state %s %s %s %s %s %s %s \"%s\" \"%s\" \"%s\"\n",tokens[F_TYPE],tokens[F_SYM], newdims,
                  tokens[F_USE],tokens[F_NTL],tokens[F_STAG],tokens[F_IO],
                  tokens[F_DNAME],tokens[F_DESCRIP],tokens[F_UNITS] ) ;

          if ( strcmp( tokens[F_SYM] , "-" ) ) {  /* if not unnamed, as can happen with first 4d tracer */
/* next, output some additional entries for the boundary arrays for these guys */
            if ( is4d == 1 ) {
              for ( i = 0, found = 0 ; i < ntracers ; i++ ) {
	        if ( !strcmp( tokens[F_USE] , tracers[i] ) ) found = 1 ; 
              }
	      if ( found == 0 ) {
	        sprintf(tracers[ntracers],tokens[F_USE]) ;
	        ntracers++ ;

/* add entries for _b and _bt arrays */

 sprintf(newname,"%s_b",tokens[F_USE]) ;
 fprintf( outfile,"state %s %s %s %s %s %s %s \"%s\" \"bdy %s\" \"%s\"\n",tokens[F_TYPE],newname,newdims4d,
                  "_4d_bdy_array_","-",tokens[F_STAG],"b",
                  newname,tokens[F_DESCRIP],tokens[F_UNITS] ) ;

 sprintf(newname,"%s_bt",tokens[F_USE]) ;
 fprintf( outfile,"state %s %s %s %s %s %s %s \"%s\" \"bdy tend %s\" \"(%s)/dt\"\n",tokens[F_TYPE],newname,newdims4d,
                  "_4d_bdy_array_","-",tokens[F_STAG],"b",
                  newname,tokens[F_DESCRIP],tokens[F_UNITS] ) ;

  	      }
            } else {

/* add entries for _b and _bt arrays */

 sprintf(newname,"%s_b",tokens[F_SYM]) ;
 fprintf( outfile,"state %s %s %s %s %s %s %s \"%s\" \"bdy %s\" \"%s\"\n",tokens[F_TYPE],newname,tokens[F_DIMS],
                  tokens[F_USE],"-",tokens[F_STAG],"b",
                  newname,tokens[F_DESCRIP],tokens[F_UNITS] ) ;

 sprintf(newname,"%s_bt",tokens[F_SYM]) ;
 fprintf( outfile,"state %s %s %s %s %s %s %s \"%s\" \"bdy tend %s\" \"(%s)/dt\"\n",tokens[F_TYPE],newname,tokens[F_DIMS],
                  tokens[F_USE],"-",tokens[F_STAG],"b",
                  newname,tokens[F_DESCRIP],tokens[F_UNITS] ) ;

            }
          }
          parseline[0] = '\0' ;  /* reset parseline */
          continue ;
        }
    }
    /* otherwise output the line as is */
    fprintf(outfile,"%s\n",parseline_save) ;
    parseline[0] = '\0' ;  /* reset parseline */
  }
  return(retval) ;
}

int
reg_parse( FILE * infile )
{
  /* Had to increase size for SOA from 4096 to 7000, Manish Shrivastava 2010 */
  char inln[7000], parseline[7000] ;
  char *p, *q ;
  char *tokens[MAXTOKENS], *toktmp[MAXTOKENS] ; 
  int i, ii, idim ;
  int defining_state_field, defining_rconfig_field, defining_i1_field ;

  parseline[0] = '\0' ;

  max_time_level = 1 ;

/* main parse loop over registry lines */
/* Had to increase size for SOA from 4096 to 7000, Manish Shrivastava 2010 */
  while ( fgets ( inln , 7000 , infile ) != NULL )
  {
    strcat( parseline , inln ) ; 
    /* allow \ to continue the end of a line */
    if (( p = index( parseline,  '\\'  )) != NULL )
    {
      if ( *(p+1) == '\n' || *(p+1) == '\0' )
      {
	*p = '\0' ;
	continue ;  /* go get another line */
      }
    }

    make_lower( parseline ) ;
    if (( p = index( parseline , '#' ))  != NULL  ) *p = '\0' ; /* discard comments (dont worry about quotes for now) */
    if (( p = index( parseline , '\n' )) != NULL  ) *p = '\0' ; /* discard newlines */
    for ( i = 0 ; i < MAXTOKENS ; i++ ) tokens[i] = NULL ; 
    i = 0 ;

    if ((tokens[i] = my_strtok(parseline)) != NULL ) i++ ; 

    while (( tokens[i] = my_strtok(NULL) ) != NULL && i < MAXTOKENS ) i++ ;
    if ( i <= 0 ) continue ;

    for ( i = 0 ; i < MAXTOKENS ; i++ )
    {
      if ( tokens[i] == NULL ) tokens[i] = "-" ;
    }

/* remove quotes from quoted entries */
    for ( i = 0 ; i < MAXTOKENS ; i++ )
    {
      char * pp ;
      if ( tokens[i][0] == '"' ) tokens[i]++ ;
      if ((pp=rindex( tokens[i], '"' )) != NULL ) *pp = '\0' ;
    }

    defining_state_field = 0 ;
    defining_rconfig_field = 0 ;
    defining_i1_field = 0 ;

/* state entry */
    if      ( !strcmp( tokens[ TABLE ] , "state" ) )
    {
      /* turn a state entry into a typedef to define a field in the top-level built-in type domain */
      tokens[TABLE] = "typedef" ;
      for ( i = MAXTOKENS-1 ; i >= 2 ; i-- ) tokens[i] = tokens[i-1] ; /* shift the fields to the left */
      tokens[FIELD_OF] = "domain" ;
                 if ( !strcmp( tokens[FIELD_TYPE], "double" ) ) tokens[FIELD_TYPE] = "doubleprecision" ; 
      defining_state_field = 1 ;
    }
    if      ( !strcmp( tokens[ TABLE ] , "rconfig" ) )
    {

      char *pp, value[256] ;
      for ( pp = tokens[RCNF_SYM_PRE] ; (*pp == ' ' || *pp == '	') && *pp ; pp++ ) ;
      sprintf(value, "RCONFIG_%s" ,pp) ;
      if ( sym_get(value) == NULL ) {
        sym_add(value) ;
      } else {
        parseline[0] = '\0' ;  /* reset parseline */
        continue ;
      }

      /* turn a rconfig entry into a typedef to define a field in the top-level built-in type domain */
      for ( i = 0 ; i < MAXTOKENS ; i++ ) { toktmp[i] = tokens[i] ; tokens[i] = "-" ; }
      tokens[TABLE] = "typedef" ;
      tokens[FIELD_OF]       = "domain" ;
      tokens[RCNF_TYPE]      = toktmp[RCNF_TYPE_PRE] ;
                 if ( !strcmp( tokens[RCNF_TYPE], "double" ) ) tokens[RCNF_TYPE] = "doubleprecision" ; 
      tokens[RCNF_SYM]       = toktmp[RCNF_SYM_PRE] ;
      tokens[RCNF_IO]        = toktmp[RCNF_IO_PRE] ;
      tokens[RCNF_DNAME]     = toktmp[RCNF_DNAME_PRE] ;
      tokens[RCNF_USE]       = "-" ;
      tokens[RCNF_DESCRIP]   = toktmp[RCNF_DESCRIP_PRE] ;
      tokens[RCNF_UNITS]     = toktmp[RCNF_UNITS_PRE] ;
      tokens[RCNF_HOWSET]    = toktmp[RCNF_HOWSET_PRE] ;
      tokens[RCNF_NENTRIES]  = toktmp[RCNF_NENTRIES_PRE] ;
      tokens[RCNF_DEFAULT]   = toktmp[RCNF_DEFAULT_PRE] ;
      defining_rconfig_field = 1 ;
    }
    if      ( !strcmp( tokens[ TABLE ] , "i1" ) )
    {
      /* turn a state entry into a typedef to define a field in 
         the top-level built-in type domain */
      tokens[TABLE] = "typedef" ;
      /* shift the fields to the left */
      for ( i = MAXTOKENS-1 ; i >= 2 ; i-- ) tokens[i] = tokens[i-1] ; 
      tokens[FIELD_OF] = "domain" ;
                 if ( !strcmp( tokens[FIELD_TYPE], "double" ) ) tokens[FIELD_TYPE] = "doubleprecision" ; 
      defining_i1_field = 1 ;
    }

    /* NOTE: fall through */

/* typedef entry */
    if ( !strcmp( tokens[ TABLE ] , "typedef" ) )
    {
      node_t * field_struct ;
      node_t * type_struct ;

      if ( !defining_state_field && ! defining_i1_field && 
           !defining_rconfig_field && !strcmp(tokens[FIELD_OF],"domain") )
       { fprintf(stderr,"Registry warning: 'domain' is a reserved registry type name. Cannot 'typedef domain'\n") ; }

      type_struct = get_type_entry( tokens[ FIELD_OF ] ) ;
      if ( type_struct == NULL ) 
      {  
        type_struct = new_node( TYPE ) ;
        strcpy( type_struct->name, tokens[FIELD_OF] ) ;
        type_struct->type_type = DERIVED ;
        add_node_to_end( type_struct , &Type ) ;
      }

      if        ( defining_i1_field )      {
        field_struct = new_node( I1 ) ;
      } else if ( defining_rconfig_field ) {
        field_struct = new_node( RCONFIG ) ;
      } else {
        field_struct = new_node( FIELD ) ;
      }

      strcpy( field_struct->name, tokens[FIELD_SYM] ) ;

      if ( set_state_type( tokens[FIELD_TYPE], field_struct ) )
       { fprintf(stderr,"Registry warning: type %s used before defined \n",tokens[FIELD_TYPE] ) ; }

      if ( set_state_dims( tokens[FIELD_DIMS], field_struct ) )
       { fprintf(stderr,"Registry warning: some problem with dimstring %s\n", tokens[FIELD_DIMS] ) ; }

      if ( strcmp( tokens[FIELD_NTL], "-" ) ) /* that is, if not equal "-" */
       { field_struct->ntl = atoi(tokens[FIELD_NTL]) ; }
      field_struct->ntl = ( field_struct->ntl > 0 )?field_struct->ntl:1 ;
      /* calculate the maximum number of time levels and store in global variable */
      if ( field_struct->ntl > max_time_level && field_struct->ntl <= 3 ) max_time_level = field_struct->ntl ;

      field_struct->stag_x = 0 ; field_struct->stag_y = 0 ; field_struct->stag_z = 0 ;
      field_struct->mp_var = 0 ; field_struct->nmm_v_grid=0 ; field_struct->full_feedback = 0;
      field_struct->no_feedback = 0;
      for ( i = 0 ; i < strlen(tokens[FIELD_STAG]) ; i++ )
      {
	if ( tolower(tokens[FIELD_STAG][i]) == 'x' || sw_all_x_staggered ) field_struct->stag_x = 1 ;
	if ( tolower(tokens[FIELD_STAG][i]) == 'y' || sw_all_y_staggered ) field_struct->stag_y = 1 ;
	if ( tolower(tokens[FIELD_STAG][i]) == 'z' ) field_struct->stag_z = 1 ;
        if ( tolower(tokens[FIELD_STAG][i]) == 'v' )
          field_struct->nmm_v_grid = 1 ;
        if ( tolower(tokens[FIELD_STAG][i]) == 'm' )
          field_struct->mp_var = 1;
        if ( tolower(tokens[FIELD_STAG][i]) == 'f' )
          field_struct->full_feedback = 1;
        if ( tolower(tokens[FIELD_STAG][i]) == 'n' )
          field_struct->no_feedback = 1;
      }

      field_struct->restart  = 0 ; field_struct->boundary  = 0 ;
      for ( i = 0 ; i < MAX_STREAMS ; i++ ) { 
        reset_mask( field_struct->io_mask, i ) ;
      }

      {
	char prev = '\0' ;
	char x ;
        char tmp[NAMELEN], tmp1[NAMELEN], tmp2[NAMELEN] ;
	int len_of_tok ;
        char fcn_name[2048], aux_fields[2048] ;

        strcpy(tmp,tokens[FIELD_IO]) ;
        if (( p = index(tmp,'=') ) != NULL ) { *p = '\0' ; }
        for ( i = 0 ; i < strlen(tmp) ; i++ )
        {
	  x = tolower(tmp[i]) ;
          if ( x == 'h' || x == 'i' ) {
            char c, *p, *pp ;
            int unitid ;
            int stream ;
            unsigned int * mask ;
            stream = ( x == 'h' )?HISTORY_STREAM:INPUT_STREAM ;
            mask = field_struct->io_mask ;
            set_mask( mask , stream ) ;
            strcpy(tmp1, &(tmp[++i])) ;
            for ( p = tmp1  ; *p ; i++, p++ ) { 
              c = tolower(*p) ; if ( c >= 'a' && c <= 'z' ) { *p = '\0' ; i-- ; break ; }
              reset_mask( mask , stream ) ;
            }
            for ( p = tmp1  ; *p ; p++ ) { 
              x = *p ;
              if ( x >= '0' && x <= '9' ) { 
                set_mask( mask , stream + x - '0' ) ; 
              }
	      else if ( x == '{' ) {
                strcpy(tmp2,p+1) ;
                if (( pp = index(tmp2,'}') ) != NULL ) {
                  *pp = '\0' ;
                  unitid = atoi(tmp2) ;  /* JM 20100416 */
                  if ( unitid >= 0  || unitid < MAX_STREAMS && stream + unitid < MAX_HISTORY ) {
                    set_mask( mask , stream + unitid   ) ;
                  }
                  p = p + strlen(tmp2) + 1 ;
                } else {
                  fprintf(stderr,"registry syntax error: unmatched {} in the io string for definition of %s\n",tokens[FIELD_SYM]) ;
                  exit(9) ;
                }
              }
            }
          }
        }

        for ( i = 0 ; i < (len_of_tok = strlen(tokens[FIELD_IO])) ; i++ )
        {
          int unitid = -1 ;
	  x = tolower(tokens[FIELD_IO][i]) ;
	  if ( x == '{' ) {
            int ii,iii ;
            char * pp ;
            char tmp[NAMELEN] ;
            strcpy(tmp,tokens[FIELD_IO]) ;   

            if (( pp = index(tmp,'}') ) != NULL ) {
              *pp = '\0' ;
              iii = pp - (tmp + i + 1) ;
              unitid = atoi(tmp+i+1) ;  /* JM 20091102 */
              if ( unitid >= 0  || unitid < MAX_STREAMS  && unitid < MAX_HISTORY ) {
                if        ( prev == 'i' ) {
                  set_mask( field_struct->io_mask , unitid + MAX_HISTORY  ) ;
                } else if ( prev == 'h' ) {
                  set_mask( field_struct->io_mask , unitid   ) ;
                }
              }
/* avoid infinite loop.  iii can go negative if the '}' is at the end of the line. */
              if ( iii > 0 ) i += iii ;
              continue ;
            } else {
              fprintf(stderr,"registry syntax error: unmatched {} in the io string for definition of %s\n",tokens[FIELD_SYM]) ;
              exit(9) ;
            }

	  } else if ( x >= 'a' && x <= 'z' ) {
	    if ( x == 'r' ) { field_struct->restart = 1 ; set_mask( field_struct->io_mask , RESTART_STREAM   ) ; }
	    if ( x == 'b' ) { field_struct->boundary  = 1 ; set_mask( field_struct->io_mask , BOUNDARY_STREAM   ) ; }
	    if ( x == 'f' || x == 'd' || x == 'u' || x == 's' ) { 
                               strcpy(aux_fields,"") ;
                               strcpy(fcn_name,"") ; 
	                       if ( tokens[FIELD_IO][i+1] == '(' )     /* catch a possible error */
                               {
				 fprintf(stderr,
				    "Registry warning: syntax error in %c specifier of IO field for %s\n",x,tokens[FIELD_SYM]) ;
				 fprintf(stderr,
				    "                  equal sign needed before left paren\n") ;
			       }

	                       if ( tokens[FIELD_IO][i+1] == '=' ) 
			       {
				 int ii, jj, state ;
				 state = 0 ;
				 jj = 0 ;
				 for ( ii = i+3 ; ii < len_of_tok ; ii++ )
				 {
				   if ( tokens[FIELD_IO][ii] == ')' ) { if (state == 0 )fcn_name[jj] = '\0' ; aux_fields[jj] = '\0' ; break ; }
				   if ( tokens[FIELD_IO][ii] == ':' ) { fcn_name[jj] = '\0' ; jj= 0 ; state++ ; continue ;}
				   if ( tokens[FIELD_IO][ii] == ',' && state == 0 ) {
				     fprintf(stderr,
                                             "Registry warning: syntax error in %c specifier of IO field for %s\n",x,
                                             tokens[FIELD_SYM]) ;
				   }
				   if ( state == 0 )  /* looking for interpolation fcn name */
				   {
				     fcn_name[jj++] = tokens[FIELD_IO][ii] ;
				   }
				   if ( state > 0 )
				   {
				     aux_fields[jj++] = tokens[FIELD_IO][ii] ;
				   }
				 }
				 i = ii ;
			       }
                               else
			       {
#if NMM_CORE==1
                                 int found_interp=0;
                                 if(field_struct->type && field_struct->type->name
                                    && (x=='f'||x=='d'||x=='u'||x=='s')) {
                                   if(dims_ij_inner(field_struct)) {
                                     if(x=='u') {
                                       if(!strcasecmp(field_struct->type->name,"real"))
                                         found_interp=!!strcpy(fcn_name,"UpCopy");
                                       else if(!strcasecmp(field_struct->type->name,"integer"))
                                         found_interp=!!strcpy(fcn_name,"UpINear");
                                     } else if(x=='d') {
                                       if(!strcasecmp(field_struct->type->name,"real"))
                                         found_interp=!!strcpy(fcn_name,"DownCopy");
                                       else if(!strcasecmp(field_struct->type->name,"integer"))
                                         found_interp=!!strcpy(fcn_name,"DownINear");
                                     } else if(x=='f') {
                                       if(!strcasecmp(field_struct->type->name,"real"))
                                         found_interp=!!strcpy(fcn_name,"BdyCopy");
                                       else if(!strcasecmp(field_struct->type->name,"integer"))
                                         found_interp=!!strcpy(fcn_name,"BdyINear");
                                     } else if(x=='s') {
                                       if(!strcasecmp(field_struct->type->name,"real"))
                                         found_interp=!!strcpy(fcn_name,"nmm_smoother_ijk");
                                     }
                                   } else if(dims_ikj_inner(field_struct)) {
                                     if(x=='d') {
                                       if(!strcasecmp(field_struct->type->name,"real"))
                                         found_interp=!!strcpy(fcn_name,"DownNearIKJ");
                                     } else if(x=='s') {
                                       if(!strcasecmp(field_struct->type->name,"real"))
                                         found_interp=!!strcpy(fcn_name,"nmm_smoother_ikj");
                                     }
                                   }
                                 }
                                 if(!found_interp) {
                                   fprintf(stderr,"ERROR: %s %c function invalid.  You must specify the function to call in f=, d=, u= or s= when using the NMM cores.  The ARW interp functions do not correctly handle the E grid.\n",tokens[FIELD_SYM],x);
                                   exit(1);
                                 } else {
                                   /*  warning should no longer be needed 
                                      fprintf(stderr,"WARNING: %c interpolation unspecified for %s.  Using %s.\n",
                                           x,tokens[FIELD_SYM],fcn_name);
                                   */
                                 }
#else
				 if ( x == 'f' || x == 'd' ) strcpy(fcn_name,"interp_fcn") ;
				 if ( x == 'u' ) strcpy(fcn_name,"copy_fcn") ;
				 if ( x == 's' ) strcpy(fcn_name,"smoother") ;
#endif
			       }
#if NMM_CORE==1
                               if(dims_ikj_inner(field_struct) && !strcasestr(fcn_name,"ikj") && !strcasestr(fcn_name,"nointerp")) {
                                 fprintf(stderr,"ERROR: %s %c %s: you must use IKJ interpolators for IKJ arrays.\n",
                                         tokens[FIELD_SYM],x,fcn_name);
                                 exit(1);
                               }
                               if(dims_ij_inner(field_struct) && strcasestr(fcn_name,"ikj") && !strcasestr(fcn_name,"nointerp")) {
                                 fprintf(stderr,"ERROR: %s %c %s: you cannot use IKJ interpolators for IJ arrays.\n",
                                         tokens[FIELD_SYM],x,fcn_name);
                                 exit(1);
                               }
#endif
	                       if      ( x == 'f' )  { 
                                 field_struct->nest_mask |= FORCE_DOWN ; 
                                 strcpy(field_struct->force_fcn_name, fcn_name ) ;
                                 strcpy(field_struct->force_aux_fields, aux_fields ) ;
                               }
                               else if ( x == 'd' )  { 
                                 field_struct->nest_mask |= INTERP_DOWN ; 
                                 strcpy(field_struct->interpd_fcn_name, fcn_name ) ;
                                 strcpy(field_struct->interpd_aux_fields, aux_fields ) ;
                               }
                               else if ( x == 's' )  { 
                                 field_struct->nest_mask |= SMOOTH_UP ; 
                                 strcpy(field_struct->smoothu_fcn_name, fcn_name ) ;
                                 strcpy(field_struct->smoothu_aux_fields, aux_fields ) ;
                               }
                               else if ( x == 'u' )  { 
                                 field_struct->nest_mask |= INTERP_UP ; 
                                 strcpy(field_struct->interpu_fcn_name, fcn_name ) ;
                                 strcpy(field_struct->interpu_aux_fields, aux_fields ) ;
                               }
            }
	    prev = x ;
	  }
        }
      }

      field_struct->dname[0] = '\0' ;
      if ( strcmp( tokens[FIELD_DNAME], "-" ) ) /* that is, if not equal "-" */
        { strcpy( field_struct->dname , tokens[FIELD_DNAME] ) ; }
      strcpy(field_struct->descrip,"-") ;
      if ( strcmp( tokens[FIELD_DESCRIP], "-" ) ) /* that is, if not equal "-" */
        { strcpy( field_struct->descrip , tokens[FIELD_DESCRIP] ) ; }
      strcpy(field_struct->units,"-") ;
      if ( strcmp( tokens[FIELD_UNITS], "-" ) ) /* that is, if not equal "-" */
        { strcpy( field_struct->units , tokens[FIELD_UNITS] ) ; }
      strcpy(field_struct->use,"-") ;
      if ( strcmp( tokens[FIELD_USE], "-" ) ) /* that is, if not equal "-" */
        { strcpy( field_struct->use , tokens[FIELD_USE] ) ;
        }

      /* specific settings for RCONFIG entries */
      if ( defining_rconfig_field )
      {
	if ( strcmp( tokens[RCNF_NENTRIES] , "-" ) ) /* that is, if not equal "-" */
	{
	  strcpy(field_struct->nentries, tokens[RCNF_NENTRIES] ) ;
	} else {
	  strcpy(field_struct->nentries, "1" ) ;
	}
	if ( strcmp( tokens[RCNF_HOWSET] , "-" ) ) /* that is, if not equal "-" */
	{
	  strcpy(field_struct->howset,tokens[RCNF_HOWSET]) ;
	} else {
	  strcpy(field_struct->howset,"") ;
	}
	if ( strcmp( tokens[RCNF_DEFAULT] , "-" ) ) /* that is, if not equal "-" */
	{
	  strcpy(field_struct->dflt,tokens[RCNF_DEFAULT]) ;
	} else {
	  strcpy(field_struct->dflt,"") ;
	}
      }

      if ( field_struct->type != NULL )
        if ( field_struct->type->type_type == DERIVED && field_struct->ndims > 0 )
          { fprintf(stderr,"Registry warning: type item %s of type %s can not be multi-dimensional ",
	  		   tokens[FIELD_SYM], tokens[FIELD_TYPE] ) ; }

/**/  if ( ! field_struct->scalar_array_member )
      {
        add_node_to_end( field_struct , &(type_struct->fields) ) ;
      }
/**/  else   /* if ( field_struct->scalar_array_member ) */
      {
/* 
   Here we are constructing a list of nodes to represent the list of 4D scalar arrays in the model

   This list is rooted at the FourD pointer.
   Each array is represented by its own node; each node has a pointer, members, to the list
   of fields that make it up.  

*/
	node_t * q , * member  ;
	if (( q = get_4d_entry(field_struct->use )) == NULL )  /* first instance of a 4d array member */
	{
	  q = new_node( FOURD ) ;
	  *q = *field_struct ;  /* this overwrites the node */
	  strcpy( q->name, field_struct->use ) ;
	  strcpy( q->use, "" ) ;
	  q->node_kind = FOURD ;
	  q->scalar_array_member = 0 ;
	  q->next4d = NULL ;
	  q->next = NULL ;
                  /* add 4d q node to the list of fields of this type and also attach
                     it to the global list of 4d arrays */
	  add_node_to_end( q , &(type_struct->fields) ) ;
	  add_node_to_end_4d( q , &(FourD) ) ;
	}
	member = new_node( MEMBER ) ;
        *member = *q ;
	member->node_kind = MEMBER ;
	member->members = NULL ;
        member->scalar_array_member = 1 ;
	strcpy( member->name , field_struct->name ) ;
	strcpy( member->dname , field_struct->dname ) ;
	strcpy( member->use , field_struct->use ) ;
	strcpy( member->descrip , field_struct->descrip ) ;
	strcpy( member->units , field_struct->units ) ;
	member->next = NULL ;
        for ( i = 0 ; i < IO_MASK_SIZE ; i++ ) {
	  member->io_mask[i] = field_struct->io_mask[i] ;
        }
	member->nest_mask = field_struct->nest_mask ;
	member->ndims = field_struct->ndims ;
	member->restart = field_struct->restart ;
	member->boundary = field_struct->boundary ;
	strcpy( member->interpd_fcn_name, field_struct->interpd_fcn_name) ;
	strcpy( member->interpd_aux_fields,  field_struct->interpd_aux_fields)  ;
	strcpy( member->interpu_fcn_name, field_struct->interpu_fcn_name) ;
	strcpy( member->interpu_aux_fields,  field_struct->interpu_aux_fields)  ;
	strcpy( member->smoothu_fcn_name, field_struct->smoothu_fcn_name) ;
	strcpy( member->smoothu_aux_fields,  field_struct->smoothu_aux_fields)  ;
	strcpy( member->force_fcn_name, field_struct->force_fcn_name) ;
	strcpy( member->force_aux_fields,  field_struct->force_aux_fields)  ;
        for ( ii = 0 ; ii < member->ndims ; ii++ )
	  member->dims[ii] = field_struct->dims[ii] ;
	add_node_to_end( member , &(q->members) ) ;
        free(field_struct) ;  /* We've used all the information about this entry.
                                 It is not a field but the name of one of the members of
                                 a 4d field.  we have handled that here. Discard the original node. */
      }
    }

/* dimespec entry */
    else if ( !strcmp( tokens[ TABLE ] , "dimspec" ) )
    {
      node_t * dim_struct ;
      dim_struct = new_node( DIM ) ;
      if ( get_dim_entry ( tokens[DIM_NAME] ) != NULL )
        { fprintf(stderr,"Registry warning: dimspec (%s) already defined\n",tokens[DIM_NAME] ) ; }
      strcpy(dim_struct->dim_name,tokens[DIM_NAME]) ;
      if ( set_dim_order( tokens[DIM_ORDER], dim_struct ) )
        { fprintf(stderr,"Registry warning: problem with dimorder (%s)\n",tokens[DIM_ORDER] ) ; }
      if ( set_dim_len( tokens[DIM_SPEC], dim_struct ) )
        { fprintf(stderr,"Registry warning: problem with dimspec (%s)\n",tokens[DIM_SPEC] ) ; }
      if ( set_dim_orient( tokens[DIM_ORIENT], dim_struct ) )
        { fprintf(stderr,"Registry warning: problem with dimorient (%s)\n",tokens[DIM_ORIENT] ) ; }
      if ( strcmp( tokens[DIM_DATA_NAME], "-" ) ) /* that is, if not equal "-" */
        { strcpy( dim_struct->dim_data_name , tokens[DIM_DATA_NAME] ) ; }

      add_node_to_end( dim_struct , &Dim ) ;
    }

/* package */
    else if ( !strcmp( tokens[ TABLE ] , "package" ) )
    {
      node_t * package_struct ;
      package_struct = new_node( PACKAGE ) ;
      strcpy( package_struct->name          , tokens[PKG_SYM]       ) ;
      strcpy( package_struct->pkg_assoc     , tokens[PKG_ASSOC]     ) ;
      strcpy( package_struct->pkg_statevars , tokens[PKG_STATEVARS] ) ;
      strcpy( package_struct->pkg_4dscalars , tokens[PKG_4DSCALARS] ) ;

      add_node_to_end( package_struct , &Packages ) ;
    }

/* halo, period, xpose */
    else if ( !strcmp( tokens[ TABLE ] , "halo" ) )
    {
      node_t * comm_struct ;
      comm_struct = new_node( HALO ) ;
      strcpy( comm_struct->name        , tokens[COMM_ID]     ) ;
      strcpy( comm_struct->use         , tokens[COMM_USE]     ) ;
#if 1
      for ( i = COMM_DEFINE, q=comm_struct->comm_define ; strcmp(tokens[i],"-") ; i++ )  {
        for(p=tokens[i];*p;p++)if(*p!=' '&&*p!='\t'){*q++=*p;}
      } 
#else
      strcpy( comm_struct->comm_define , tokens[COMM_DEFINE] ) ;
#endif
      add_node_to_end( comm_struct , &Halos ) ;
    }
    else if ( !strcmp( tokens[ TABLE ] , "period" ) )
    {
      node_t * comm_struct ;
      comm_struct = new_node( PERIOD ) ;
      strcpy( comm_struct->name        , tokens[COMM_ID]     ) ;
      strcpy( comm_struct->use         , tokens[COMM_USE]     ) ;
#if 1
      for ( i = COMM_DEFINE, q=comm_struct->comm_define ; strcmp(tokens[i],"-") ; i++ )  {
        for(p=tokens[i];*p;p++)if(*p!=' '&&*p!='\t'){*q++=*p;}
      } 
#else
      strcpy( comm_struct->comm_define , tokens[COMM_DEFINE] ) ;
#endif
      add_node_to_end( comm_struct , &Periods ) ;
    }
    else if ( !strcmp( tokens[ TABLE ] , "xpose" ) )
    {
      node_t * comm_struct ;
      comm_struct = new_node( XPOSE ) ;
      strcpy( comm_struct->name        , tokens[COMM_ID]     ) ;
      strcpy( comm_struct->use         , tokens[COMM_USE]     ) ;
#if 1
      for ( i = COMM_DEFINE, q=comm_struct->comm_define ; strcmp(tokens[i],"-") ; i++ )  {
        for(p=tokens[i];*p;p++)if(*p!=' '&&*p!='\t'){*q++=*p;}
      } 
#else
      strcpy( comm_struct->comm_define , tokens[COMM_DEFINE] ) ;
#endif
      add_node_to_end( comm_struct , &Xposes ) ;
    }
    else if ( !strcmp( tokens[ TABLE ] , "swap" ) )
    {
      node_t * comm_struct ;
      comm_struct = new_node( SWAP ) ;
      strcpy( comm_struct->name        , tokens[COMM_ID]     ) ;
      strcpy( comm_struct->use         , tokens[COMM_USE]     ) ;
#if 1
      for ( i = COMM_DEFINE, q=comm_struct->comm_define ; strcmp(tokens[i],"-") ; i++ )  {
        for(p=tokens[i];*p;p++)if(*p!=' '&&*p!='\t'){*q++=*p;}
      }
#else
      strcpy( comm_struct->comm_define , tokens[COMM_DEFINE] ) ;
#endif
      add_node_to_end( comm_struct , &Swaps ) ;
    }
    else if ( !strcmp( tokens[ TABLE ] , "cycle" ) )
    {
      node_t * comm_struct ;
      comm_struct = new_node( CYCLE ) ;
      strcpy( comm_struct->name        , tokens[COMM_ID]     ) ;
      strcpy( comm_struct->use         , tokens[COMM_USE]     ) ;
#if 1
      for ( i = COMM_DEFINE, q=comm_struct->comm_define ; strcmp(tokens[i],"-") ; i++ )  {
        for(p=tokens[i];*p;p++)if(*p!=' '&&*p!='\t'){*q++=*p;}
      }
#else
      strcpy( comm_struct->comm_define , tokens[COMM_DEFINE] ) ;
#endif
      add_node_to_end( comm_struct , &Cycles ) ;
    }


#if 0
     fprintf(stderr,"vvvvvvvvvvvvvvvvvvvvvvvvvvv\n") ;
     show_nodelist( Type ) ;
     fprintf(stderr,"^^^^^^^^^^^^^^^^^^^^^^^^^^^\n") ;
#endif
     parseline[0] = '\0' ;  /* reset parseline */
  }

  Domain = *(get_type_entry( "domain" )) ;

#if 0
  show_node( &Domain ) ;
#endif

  return(0) ;

}

node_t *
get_dim_entry( char *s )
{
  node_t * p ;
  for ( p = Dim ; p != NULL ; p = p->next )
  {
    if ( !strcmp(p->dim_name, s ) ) {
      return( p ) ;
    }
  }
  return(NULL) ;
}

int
set_state_type( char * typename, node_t * state_entry )
{
  if ( typename == NULL ) return(1) ;
  return (( state_entry->type = get_type_entry( typename )) == NULL )  ;
}

int
set_dim_len ( char * dimspec , node_t * dim_entry )
{
  if      (!strcmp( dimspec , "standard_domain" ))
   { dim_entry->len_defined_how = DOMAIN_STANDARD ; }
  else if (!strncmp( dimspec, "constant=" , 9 ))
  {
    char *p, *colon, *paren ;
    p = &(dimspec[9]) ;
    /* check for colon */
    if (( colon = index(p,':')) != NULL )
    {
      *colon = '\0' ;
      if (( paren = index(p,'(')) !=NULL )
      {
        dim_entry->coord_start = atoi(paren+1) ;
      }
      else
      {
        fprintf(stderr,"WARNING: illegal syntax (missing opening paren) for constant: %s\n",p) ;
      }
      dim_entry->coord_end   = atoi(colon+1) ;
    }
    else
    {
      dim_entry->coord_start = 1 ;
      dim_entry->coord_end   = atoi(p) ;
    }
    dim_entry->len_defined_how = CONSTANT ;
  }
  else if (!strncmp( dimspec, "namelist=", 9 ))
  {
    char *p, *colon ;

    p = &(dimspec[9]) ;
    /* check for colon */
    if (( colon = index(p,':')) != NULL )
    {
      *colon = '\0' ;
      strcpy( dim_entry->assoc_nl_var_s, p ) ;
      strcpy( dim_entry->assoc_nl_var_e, colon+1 ) ;
    }
    else
    {
      strcpy( dim_entry->assoc_nl_var_s, "1" ) ;
      strcpy( dim_entry->assoc_nl_var_e, p ) ;
    }
    dim_entry->len_defined_how = NAMELIST ;
  }
  else
  {
    return(1) ;
  }
  return(0) ;
}

int
set_dim_orient ( char * dimorient , node_t * dim_entry )
{
  if      (!strcmp( dimorient , "x" ))
   { dim_entry->coord_axis = COORD_X ; }
  else if (!strcmp( dimorient , "y" )) 
   { dim_entry->coord_axis = COORD_Y ; }
  else if (!strcmp( dimorient , "z" )) 
   { dim_entry->coord_axis = COORD_Z ; }
  else
   { dim_entry->coord_axis = COORD_C ; }
  return(0) ;
}

/* integrity checking of dimension list; make sure that
   namelist specified dimensions have an associated namelist variable */
int
check_dimspecs()
{
  node_t * p, *q ;
  int ord ;

  for ( p = Dim ; p != NULL ; p = p->next )
  {
    if      ( p->len_defined_how == DOMAIN_STANDARD )
    {
      if ( p->dim_order < 1 || p->dim_order > 3 )
      {
        fprintf(stderr,"WARNING: illegal dim order %d for dimspec %s\n",p->dim_order,p->name) ;
      }
      ord = p->dim_order-1 ;
      if ( model_order[ord] != p->coord_axis )
      {
        if ( model_order[ord] == -1 ) model_order[ord] = p->coord_axis ;
        else
        {
          fprintf(stderr,"WARNING: coord-axis/dim-order for dimspec %s is inconsistent with previous dimspec.\n",p->name) ;
        }
      }
    }
    else if ( p->len_defined_how == NAMELIST )
    {
      if ( strcmp( p->assoc_nl_var_s, "1" ) )   /* if not equal to "1" */
      {
        if (( q = get_entry(p->assoc_nl_var_s,Domain.fields)) == NULL )
        {
	  fprintf(stderr,"WARNING: no namelist variable %s defined for dimension %s\n",
		  p->assoc_nl_var_s,p->name ) ;
	  return(1) ;
        }
        if ( ! q->node_kind & RCONFIG )
        {
	  fprintf(stderr,"WARNING: no namelist variable %s defined for dimension %s\n",
		  p->assoc_nl_var_s,p->name ) ;
	  return(1) ;
        }
        if ( strcmp( q->type->name , "integer" ) )   /* if not integer */
        {
	  fprintf(stderr,"WARNING: namelist variable %s must be an integer if used to define dimension %s\n",
		  p->assoc_nl_var_s,p->name ) ;
	  return(1) ;
        }
        if ( strcmp( q->nentries , "1" ) )   /* if not 1 entry */
        {
	  fprintf(stderr,"WARNING: namelist variable %s must have only one entry if used to define dimension %s\n",
		  p->assoc_nl_var_s,p->name ) ;
	  return(1) ;
        }
      }
      if (( q = get_entry(p->assoc_nl_var_e,Domain.fields)) == NULL )
      {
	fprintf(stderr,"WARNING: no namelist variable %s defined for dimension %s\n",
		p->assoc_nl_var_e,p->name ) ;
	return(1) ;
      }
      if ( ! q->node_kind & RCONFIG )
      {
	fprintf(stderr,"WARNING: no namelist variable %s defined for dimension %s\n",
		p->assoc_nl_var_e,p->name ) ;
	return(1) ;
      }
      if ( strcmp( q->type->name , "integer" ) )   /* if not integer */
      {
	fprintf(stderr,"WARNING: namelist variable %s must be an integer if used to define dimension %s\n",
		p->assoc_nl_var_e,p->name ) ;
	return(1) ;
      }
      if ( strcmp( q->nentries , "1" ) )   /* if not 1 entry */
      {
	fprintf(stderr,"WARNING: namelist variable %s must have only one entry if used to define dimension %s\n",
		p->assoc_nl_var_e,p->name ) ;
	return(1) ;
      }
    }
  }
  return(0) ;
}

int
set_dim_order ( char * dimorder , node_t * dim_entry )
{
  dim_entry->dim_order = atoi(dimorder) ;
  return(0) ;
}

int
init_parser()
{
  model_order[0] = -1 ;
  model_order[1] = -1 ;
  model_order[2] = -1 ;
  return(0) ;
}
