#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "registry.h"
#include "protos.h"
#include "data.h"

/* read in the Registry file and build the internal representation of the registry */

#define MAXTOKENS 1000

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


int
pre_parse( FILE * infile, FILE * outfile )
{
  char inln[4096], parseline[4096], parseline_save[4096] ;
  int ntracers = 0 ;
  int found ; 
  char tracers[1000][NAMELEN] ;
  char *p, *q ;
  char *tokens[MAXTOKENS], *toktmp[MAXTOKENS], newdims[NAMELEN], newdims4d[NAMELEN],newname[NAMELEN] ;
  int i, ii, len_of_tok ;
  char x, xstr[NAMELEN] ;
  int is4d, wantstend, wantsbdy ;

  parseline[0] = '\0' ;
/* main parse loop over registry lines */
  while ( fgets ( inln , 4096 , infile ) != NULL )
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
    make_lower( parseline ) ;

    if (( p = index( parseline , '#' ))  != NULL  ) *p = '\0' ; /* discard comments (dont worry about quotes for now) */
    if (( p = index( parseline , '\n' )) != NULL  ) *p = '\0' ; /* discard newlines */
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
        strcpy( newdims, "" ) ;
        strcpy( newdims4d, "" ) ;
        is4d = 0 ; wantstend = 0 ; wantsbdy = 0 ; 
        for ( i = 0 ; i < (len_of_tok = strlen(tokens[F_DIMS])) ; i++ )
        {
          x = tolower(tokens[F_DIMS][i]) ;
          if ( x >= 'a' && x <= 'z' ) {
            if ( x == 'f' ) { is4d = 1 ; }
            if ( x == 't' ) { wantstend = 1 ; }
            if ( x == 'b' ) { wantsbdy = 1 ; }
          }
          sprintf(xstr,"%c",x) ;
          if ( x != 'b' ) strcat ( newdims , xstr ) ;
          if ( x != 'f' && x != 't' ) strcat( newdims4d , xstr ) ;

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
normal:
    /* otherwise output the line as is */
    fprintf(outfile,"%s\n",parseline_save) ;
    parseline[0] = '\0' ;  /* reset parseline */
  }
  return(0) ;
}

int
reg_parse( FILE * infile )
{
  char inln[4096], parseline[4096] ;
  char *p, *q ;
  char *tokens[MAXTOKENS], *toktmp[MAXTOKENS] ; 
  int i, ii ;
  int defining_state_field, defining_rconfig_field, defining_i1_field ;

  parseline[0] = '\0' ;

  max_time_level = 1 ;

/* main parse loop over registry lines */
  while ( fgets ( inln , 4096 , infile ) != NULL )
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
      for ( i = 0 ; i < strlen(tokens[FIELD_STAG]) ; i++ )
      {
	if ( tolower(tokens[FIELD_STAG][i]) == 'x' || sw_all_x_staggered ) field_struct->stag_x = 1 ;
	if ( tolower(tokens[FIELD_STAG][i]) == 'y' || sw_all_y_staggered ) field_struct->stag_y = 1 ;
	if ( tolower(tokens[FIELD_STAG][i]) == 'z' ) field_struct->stag_z = 1 ;
      }

      field_struct->history  = 0 ; field_struct->input     = 0 ; 
      field_struct->auxhist1 = 0 ; field_struct->auxinput1 = 0 ; 
      field_struct->auxhist2 = 0 ; field_struct->auxinput2 = 0 ; 
      field_struct->auxhist3 = 0 ; field_struct->auxinput3 = 0 ; 
      field_struct->auxhist4 = 0 ; field_struct->auxinput4 = 0 ; 
      field_struct->auxhist5 = 0 ; field_struct->auxinput5 = 0 ; 
      field_struct->restart  = 0 ; field_struct->boundary  = 0 ;
      field_struct->io_mask  = 0 ;
      {
	char prev = '\0' ;
	char x ;
	int len_of_tok ;
        char fcn_name[2048], aux_fields[2048] ;

        for ( i = 0 ; i < (len_of_tok = strlen(tokens[FIELD_IO])) ; i++ )
        {
	  x = tolower(tokens[FIELD_IO][i]) ;
	  if ( x >= 'a' && x <= 'z' ) {
	    if ( x == 'h' ) {field_struct->history  = 10 ; field_struct->io_mask |= HISTORY ;}
	    if ( x == 'i' ) {field_struct->input    = 10 ; field_struct->io_mask |= INPUT   ;}
	    if ( x == 'r' ) {field_struct->restart  = 10 ; field_struct->io_mask |= RESTART ;}
	    if ( x == 'b' ) {field_struct->boundary = 10 ; field_struct->io_mask |= BOUNDARY ;}
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
				 if ( x == 'f' || x == 'd' ) strcpy(fcn_name,"interp_fcn") ;
				 if ( x == 'u' ) strcpy(fcn_name,"copy_fcn") ;
				 if ( x == 's' ) strcpy(fcn_name,"smoother") ;
			       }
	                       if      ( x == 'f' )  { 
                                 field_struct->io_mask |= FORCE_DOWN ; 
                                 strcpy(field_struct->force_fcn_name, fcn_name ) ;
                                 strcpy(field_struct->force_aux_fields, aux_fields ) ;
                               }
                               else if ( x == 'd' )  { 
                                 field_struct->io_mask |= INTERP_DOWN ; 
                                 strcpy(field_struct->interpd_fcn_name, fcn_name ) ;
                                 strcpy(field_struct->interpd_aux_fields, aux_fields ) ;
                               }
                               else if ( x == 's' )  { 
                                 field_struct->io_mask |= SMOOTH_UP ; 
                                 strcpy(field_struct->smoothu_fcn_name, fcn_name ) ;
                                 strcpy(field_struct->smoothu_aux_fields, aux_fields ) ;
                               }
                               else if ( x == 'u' )  { 
                                 field_struct->io_mask |= INTERP_UP ; 
                                 strcpy(field_struct->interpu_fcn_name, fcn_name ) ;
                                 strcpy(field_struct->interpu_aux_fields, aux_fields ) ;
                               }
            }
	    prev = x ;
	  } else if ( x >= '0' && x <= '9' )
	  {
	    if ( prev  == 'i' )
	    {
              field_struct->io_mask &= ! INPUT ;                /* turn off setting from 'i' */
	      field_struct->input = field_struct->input % 10 ;  /* turn off setting from 'i' */
	      if ( x == '0' ) field_struct->input = 1 ;
	      if ( x == '1' ) field_struct->auxinput1 = 1 ;
	      if ( x == '2' ) field_struct->auxinput2 = 1 ;
	      if ( x == '3' ) field_struct->auxinput3 = 1 ;
	      if ( x == '4' ) field_struct->auxinput4 = 1 ;
	      if ( x == '5' ) field_struct->auxinput5 = 1 ;
	    }
	    if ( prev  == 'h' )
	    {
              field_struct->io_mask &= ! HISTORY ;                  /* turn off setting from 'h' */
	      field_struct->history = field_struct->history % 10 ;  /* turn off setting from 'h' */
	      if ( x == '0' ) field_struct->history = 1 ;
	      if ( x == '1' ) field_struct->auxhist1 = 1 ;
	      if ( x == '2' ) field_struct->auxhist2 = 1 ;
	      if ( x == '3' ) field_struct->auxhist3 = 1 ;
	      if ( x == '4' ) field_struct->auxhist4 = 1 ;
	      if ( x == '5' ) field_struct->auxhist5 = 1 ;
	    }
	  }
        }
	if ( field_struct->history   > 0 ) { field_struct->history   = 1 ; field_struct->io_mask |= HISTORY   ; }
	if ( field_struct->auxhist1  > 0 ) { field_struct->auxhist1  = 1 ; field_struct->io_mask |= AUXHIST1  ; }
	if ( field_struct->auxhist2  > 0 ) { field_struct->auxhist2  = 1 ; field_struct->io_mask |= AUXHIST2  ; }
	if ( field_struct->auxhist3  > 0 ) { field_struct->auxhist3  = 1 ; field_struct->io_mask |= AUXHIST3  ; }
	if ( field_struct->auxhist4  > 0 ) { field_struct->auxhist4  = 1 ; field_struct->io_mask |= AUXHIST4  ; }
	if ( field_struct->auxhist5  > 0 ) { field_struct->auxhist5  = 1 ; field_struct->io_mask |= AUXHIST5  ; }

	if ( field_struct->input     > 0 ) { field_struct->input     = 1 ; field_struct->io_mask |= INPUT     ; }
	if ( field_struct->auxinput1 > 0 ) { field_struct->auxinput1 = 1 ; field_struct->io_mask |= AUXINPUT1 ; }
	if ( field_struct->auxinput2 > 0 ) { field_struct->auxinput2 = 1 ; field_struct->io_mask |= AUXINPUT2 ; }
	if ( field_struct->auxinput3 > 0 ) { field_struct->auxinput3 = 1 ; field_struct->io_mask |= AUXINPUT3 ; }
	if ( field_struct->auxinput4 > 0 ) { field_struct->auxinput4 = 1 ; field_struct->io_mask |= AUXINPUT4 ; }
	if ( field_struct->auxinput5 > 0 ) { field_struct->auxinput5 = 1 ; field_struct->io_mask |= AUXINPUT5 ; }

	if ( field_struct->restart   > 0 ) { field_struct->restart   = 1 ; field_struct->io_mask |= RESTART   ; }
	if ( field_struct->boundary  > 0 ) { field_struct->boundary  = 1 ; field_struct->io_mask |= BOUNDARY  ; }
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
          if ( ! defining_rconfig_field && ! field_struct->scalar_array_member && !strncmp( tokens[FIELD_USE], "dyn_", 4 ) )
             add_core_name( tokens[FIELD_USE]+4 ) ;
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
	member->io_mask = field_struct->io_mask ;
	member->ndims = field_struct->ndims ;
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
      if ( strlen( tokens[DIM_NAME] ) > 1 )
        { fprintf(stderr,"Registry warning: dimspec (%s) must be only one letter\n",tokens[DIM_NAME] ) ; }
      if ( get_dim_entry ( tokens[DIM_NAME][0] ) != NULL )
        { fprintf(stderr,"Registry warning: dimspec (%c) already defined\n",tokens[DIM_NAME][0] ) ; }
      dim_struct->dim_name = tokens[DIM_NAME][0] ;
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
get_dim_entry( char c )
{
  node_t * p ;
  for ( p = Dim ; p != NULL ; p = p->next )
  {
    if ( p->dim_name == c ) return( p ) ;
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

init_parser()
{
  model_order[0] = -1 ;
  model_order[1] = -1 ;
  model_order[2] = -1 ;
  return(0) ;
}
