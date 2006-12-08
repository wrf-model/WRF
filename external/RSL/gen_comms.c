#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "protos.h"
#include "registry.h"
#include "data.h"

/* For detecting variables that are members of a derived type */
#define NULLCHARPTR   (char *) 0
static int parent_type;

int
gen_halos ( char * dirname )
{
  node_t * p, * q ;
  node_t * dimd ;
  char commname[NAMELEN] ;
  char fname[NAMELEN] ;
  char tmp[4096], tmp2[4096], tmp3[4096] ;
  char commuse[4096] ;
  int maxstenwidth, stenwidth ;
  FILE * fp ;
  char * t1, * t2 ;
  char * pos1 , * pos2 ;
  char indices[NAMELEN], post[NAMELEN], varref[NAMELEN] ;
  int zdex ;

  if ( dirname == NULL ) return(1) ;

  for ( p = Halos ; p != NULL ; p = p->next )
  {
    strcpy( commname, p->name ) ;
    make_upper_case(commname) ;
    if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s.inc",dirname,commname) ; }
    else                       { sprintf(fname,"%s.inc",commname) ; }
    if ((fp = fopen( fname , "w" )) == NULL ) 
    {
      fprintf(stderr,"WARNING: gen_halos in registry cannot open %s for writing\n",fname ) ;
      continue ; 
    }
    /* get maximum stencil width */
    maxstenwidth = 0 ;
    strcpy( tmp, p->comm_define ) ;
    t1 = strtok_rentr( tmp , "; " , &pos1 ) ;
    while ( t1 != NULL )
    {
      strcpy( tmp2 , t1 ) ;
      if (( t2 = strtok_rentr( tmp2 , ": " , &pos2 )) == NULL )
       { fprintf(stderr,"unparseable description for halo %s\n", commname ) ; exit(1) ; }
      stenwidth = atoi (t2) ;
      if ( stenwidth == 0 )
       { fprintf(stderr,"* unparseable description for halo %s\n", commname ) ; exit(1) ; }
      if ( stenwidth > maxstenwidth ) maxstenwidth = stenwidth ;
      t1 = strtok_rentr( NULL , "; " , &pos1 ) ;
    }
    print_warning(fp,fname) ;
    fprintf(fp,"#ifndef DATA_CALLS_INCLUDED\n") ;
    fprintf(fp,"--- DELIBERATE SYNTAX ERROR: THIS ROUTINE SHOULD INCLUDE \"%s_data_calls.inc\"\n",p->use+4) ;
    fprintf(fp,"    BECAUSE IT CONTAINS AN RSL HALO OPERATION\n" ) ;
    fprintf(fp,"#endif\n") ;

    fprintf(fp,"IF ( grid%%comms( %s ) == invalid_message_value ) THEN\n",commname ) ;
    fprintf(fp,"  CALL wrf_debug ( 50 , 'set up halo %s' )\n",commname ) ;
    fprintf(fp,"  CALL setup_halo_rsl( grid )\n" ) ;
    fprintf(fp,"  CALL reset_msgs_%dpt\n", maxstenwidth ) ;

    /* pass through description again now and generate the calls  */
    strcpy( tmp, p->comm_define ) ;
    strcpy( commuse, p->use ) ;
    t1 = strtok_rentr( tmp , "; " , &pos1 ) ;
    while ( t1 != NULL )
    {
      strcpy( tmp2 , t1 ) ;
      if (( t2 = strtok_rentr( tmp2 , ": " , &pos2 )) == NULL )
       { fprintf(stderr,"unparseable description for halo %s\n", commname ) ; continue ; }
      stenwidth = atoi (t2) ;
      t2 = strtok_rentr(NULL,", ", &pos2) ;

      while ( t2 != NULL )
      {
        if ((q = get_entry_r( t2, commuse, Domain.fields )) == NULL )
        {
          fprintf(stderr,"WARNING 1 : %s in halo spec %s (%s) is not defined in registry.\n",t2,commname, commuse) ;
        }
        else
        {

          strcpy( varref, t2 ) ;
          if ( q->node_kind & FIELD  && ! (q->node_kind & I1) ) {
             if ( !strncmp( q->use,  "dyn_", 4 )) {
                  char * core ;
                  core = q->use+4 ;
                  sprintf(varref,"grid%%%s_%s",core,t2) ;
             } else {
                  sprintf(varref,"grid%%%s",t2) ;
             }
          }

          if      (  strcmp( q->type->name, "real") && strcmp( q->type->name, "integer") && strcmp( q->type->name, "doubleprecision") )
          {
            fprintf(stderr,"WARNING: only type 'real', 'doubleprecision', or 'integer' can be part of halo exchange. %s in %s is %s\n",t2,commname,q->type->name) ;
          }
          else if ( q->boundary_array )
          {
            fprintf(stderr,"WARNING: boundary array %s cannot be member of halo spec %s.\n",t2,commname) ;
          }
          else
          {
            if ( q->node_kind & FOURD )
            {
              node_t *member ;
              zdex = get_index_for_coord( q , COORD_Z ) ;
              if ( zdex >=1 && zdex <= 3 )
              {
                for ( member = q->members ; member != NULL ; member = member->next )
                {
                  if ( strcmp( member->name, "-" ) )
                  {
                    fprintf(fp,"  if ( P_%s .GT. 1 ) CALL add_msg_%dpt_%s ( %s ( grid%%sm31,grid%%sm32,grid%%sm33,P_%s), glen(%d) )\n", 
                       member->name, stenwidth, q->type->name, t2 , member->name, zdex+1 ) ;
                  }
                }
              }
              else
              {
                fprintf(stderr,"WARNING: %d some dimension info missing for 4d array %s\n",zdex,t2) ;
              }
            }
            else
            {
              strcpy (indices,"");
              if ( sw_deref_kludge ) /* &&  strchr (t2, '%') != NULLCHARPTR ) */
              {
                sprintf(post,")") ;
                sprintf(indices, "%s",index_with_firstelem("(","",tmp3,q,post)) ;
              }
              dimd = get_dimnode_for_coord( q , COORD_Z ) ;
              zdex = get_index_for_coord( q , COORD_Z ) ;
              if ( dimd != NULL )
              {
                char dimstrg[256] ;

                if      ( dimd->len_defined_how == DOMAIN_STANDARD )
                    sprintf(dimstrg,"(glen(%d))",zdex+1) ;
                else if ( dimd->len_defined_how == NAMELIST )
                {
                  if ( !strcmp(dimd->assoc_nl_var_s,"1") )
                    sprintf(dimstrg,"config_flags%%%s",dimd->assoc_nl_var_e) ;
                  else
                    sprintf(dimstrg,"(config_flags%%%s - config_flags%%%s + 1)",dimd->assoc_nl_var_e,dimd->assoc_nl_var_s) ;
                }
                else if ( dimd->len_defined_how == CONSTANT )
                    sprintf(dimstrg,"(%d - %d + 1)",dimd->coord_end,dimd->coord_start) ;

                fprintf(fp,"  CALL add_msg_%dpt_%s ( %s%s , %s )\n", stenwidth, q->type->name, varref, indices, dimstrg ) ;
              }
              else if ( q->ndims == 2 )  /* 2d */
              {
                fprintf(fp,"  CALL add_msg_%dpt_%s ( %s%s , %s )\n", stenwidth, q->type->name, varref, indices, "1" ) ;
              }
            }
          }
          q->subject_to_communication = 1 ;         /* Indicate that this field may be communicated */
        }
        t2 = strtok_rentr( NULL , ", " , &pos2 ) ;
      }
      t1 = strtok_rentr( NULL , "; " , &pos1 ) ;
    }
    fprintf(fp,"  CALL stencil_%dpt ( grid%%domdesc , grid%%comms ( %s ) )\n", maxstenwidth , commname ) ;
    fprintf(fp,"ENDIF\n") ;
    fprintf(fp,"  CALL wrf_debug ( 50 , 'exchange halo %s' )\n",commname ) ;
    fprintf(fp,"CALL rsl_exch_stencil ( grid%%domdesc , grid%%comms( %s ) )\n", commname ) ;

    close_the_file(fp) ;
  }
  return(0) ;
}

int
gen_periods ( char * dirname )
{
  node_t * p, * q ;
  char commname[NAMELEN] ;
  char fname[NAMELEN] ;
  char indices[NAMELEN], post[NAMELEN], varref[NAMELEN] ;
  char tmp[4096], tmp2[4096], tmp3[4096], commuse[4096] ;
  int maxperwidth, perwidth ;
  FILE * fp ;
  char * t1, * t2 ;
  char * pos1 , * pos2 ;
  node_t * dimd ;
  int zdex ;

  if ( dirname == NULL ) return(1) ;

  for ( p = Periods ; p != NULL ; p = p->next )
  {
    strcpy( commname, p->name ) ;
    make_upper_case(commname) ;
    if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s.inc",dirname,commname) ; }
    else                       { sprintf(fname,"%s.inc",commname) ; }
    if ((fp = fopen( fname , "w" )) == NULL ) 
    {
      fprintf(stderr,"WARNING: gen_periods in registry cannot open %s for writing\n",fname ) ;
      continue ; 
    }
    /* get maximum stencil width */
    maxperwidth = 0 ;
    strcpy( tmp, p->comm_define ) ;
    t1 = strtok_rentr( tmp , ";" , &pos1 ) ;
    while ( t1 != NULL )
    {
      strcpy( tmp2 , t1 ) ;
      if (( t2 = strtok_rentr( tmp2 , ":" , &pos2 )) == NULL )
       { fprintf(stderr,"unparseable description for halo %s\n", commname ) ; exit(1) ; }
      perwidth = atoi (t2) ;
      if ( perwidth > maxperwidth ) maxperwidth = perwidth ;
      t1 = strtok_rentr( NULL , ";" , &pos1 ) ;
    }
    print_warning(fp,fname) ;

    fprintf(fp,"#ifndef DATA_CALLS_INCLUDED\n") ;
    fprintf(fp,"--- DELIBERATE SYNTAX ERROR: THIS ROUTINE SHOULD INCLUDE \"%s_data_calls.inc\"\n",p->use+4) ;
    fprintf(fp,"    BECAUSE IT CONTAINS AN RSL PERIOD OPERATION\n" ) ;
    fprintf(fp,"#endif\n") ;
    fprintf(fp,"IF ( grid%%comms( %s ) == invalid_message_value .AND. (config_flags%%periodic_x .OR. config_flags%%periodic_y )) THEN\n",commname ) ;

    fprintf(fp,"  CALL wrf_debug ( 50 , 'setting up period %s' )\n",commname ) ;
    fprintf(fp,"  CALL setup_period_rsl( grid )\n" ) ;
    fprintf(fp,"  CALL reset_period\n") ;

    /* pass through description again now and generate the calls  */
    strcpy( tmp, p->comm_define ) ;
    strcpy( commuse, p->use ) ;
    t1 = strtok_rentr( tmp , ";" , &pos1 ) ;
    while ( t1 != NULL )
    {
      strcpy( tmp2 , t1 ) ;
      if (( t2 = strtok_rentr( tmp2 , ":" , &pos2 )) == NULL )
       { fprintf(stderr,"unparseable description for period %s\n", commname ) ; continue ; }
      perwidth = atoi (t2) ;
      t2 = strtok_rentr(NULL,",", &pos2) ;
      while ( t2 != NULL )
      {
        if ((q = get_entry_r( t2, commuse, Domain.fields )) == NULL )
        {
          fprintf(stderr,"WARNING 2 : %s in period spec %s is not defined in registry.\n",t2,commname) ;
        }
        else
        {
          if ( q->boundary_array )
          {
            fprintf(stderr,"WARNING: boundary array %s cannot be member of period spec %s.\n",t2,commname) ;
          }
          else
          {

            strcpy( varref, t2 ) ;
            if ( q->node_kind & FIELD  && ! (q->node_kind & I1) ) {
               if ( !strncmp( q->use,  "dyn_", 4 )) {
                    char * core ;
                    core = q->use+4 ;
                    sprintf(varref,"grid%%%s_%s",core,t2) ;
               } else {
                    sprintf(varref,"grid%%%s",t2) ;
               }
            }

            if ( q->node_kind & FOURD )
            {
              node_t *member ;
              zdex = get_index_for_coord( q , COORD_Z ) ;
              if ( zdex >=1 && zdex <= 3 )
              {
                for ( member = q->members ; member != NULL ; member = member->next )
                {
                  if ( strcmp( member->name, "-" ) )
                  {
                    fprintf(fp,"  if ( P_%s .GT. 1 ) CALL add_msg_period_%s ( %s ( grid%%sm31,grid%%sm32,grid%%sm33,P_%s), glen(%d) )\n",
                       member->name, q->type->name, t2 , member->name, zdex+1 ) ;
                  }
                }
              }
              else
              {
                fprintf(stderr,"WARNING: %d some dimension info missing for 4d array %s\n",zdex,t2) ;
              }
            }
            else
            {
              strcpy (indices,"");
              if ( sw_deref_kludge ) /* &&  strchr (t2, '%') != NULLCHARPTR ) */
              {
                sprintf(post,")") ;
                sprintf(indices, "%s",index_with_firstelem("(","",tmp3,q,post)) ;
              }
              dimd = get_dimnode_for_coord( q , COORD_Z ) ;
              zdex = get_index_for_coord( q , COORD_Z ) ;
              if ( dimd != NULL )
              {
                char dimstrg[256] ;

                if      ( dimd->len_defined_how == DOMAIN_STANDARD )
                    sprintf(dimstrg,"(glen(%d))",zdex+1) ;
                else if ( dimd->len_defined_how == NAMELIST )
                {
                  if ( !strcmp(dimd->assoc_nl_var_s,"1") )
                    sprintf(dimstrg,"config_flags%%%s",dimd->assoc_nl_var_e) ;
                  else
                    sprintf(dimstrg,"(config_flags%%%s - config_flags%%%s + 1)",dimd->assoc_nl_var_e,dimd->assoc_nl_var_s) ;
                }
                else if ( dimd->len_defined_how == CONSTANT )
                    sprintf(dimstrg,"(%d - %d + 1)",dimd->coord_end,dimd->coord_start) ;

                fprintf(fp,"  CALL add_msg_period_%s ( %s%s , %s )\n", q->type->name, varref, indices, dimstrg ) ;
              }
              else if ( q->ndims == 2 )  /* 2d */
              {
                fprintf(fp,"  CALL add_msg_period_%s ( %s%s , %s )\n", q->type->name, varref, indices, "1" ) ;
              }
            }
          }
          q->subject_to_communication = 1 ;         /* Indicate that this field may be communicated */
        }
        t2 = strtok_rentr( NULL , "," , &pos2 ) ;
      }
      t1 = strtok_rentr( NULL , ";" , &pos1 ) ;
    }
    fprintf(fp,"  CALL period_def ( grid%%domdesc , grid%%comms ( %s ) , %d )\n",commname , maxperwidth ) ;
    fprintf(fp,"ENDIF\n") ;
    fprintf(fp,"IF ( config_flags%%periodic_x ) THEN\n") ; 
    fprintf(fp,"  CALL wrf_debug ( 50 , 'exchanging period %s on x' )\n",commname ) ;
    fprintf(fp,"  CALL rsl_exch_period ( grid%%domdesc , grid%%comms( %s ) , x_period_flag )\n",commname ) ;
    fprintf(fp,"END IF\n") ; 
    fprintf(fp,"IF ( config_flags%%periodic_y ) THEN\n") ;
    fprintf(fp,"  CALL wrf_debug ( 50 , 'exchanging period %s on y' )\n",commname ) ;
    fprintf(fp,"  CALL rsl_exch_period ( grid%%domdesc , grid%%comms( %s ) , y_period_flag )\n",commname ) ;
    fprintf(fp,"END IF\n") ;

    close_the_file(fp) ;
  }
  return(0) ;
}

int
gen_xposes ( char * dirname )
{
  node_t * p, * q ;
  char commname[NAMELEN] ;
  char fname[NAMELEN] ;
  char tmp[4096], tmp2[4096], tmp3[4096] ;
  char commuse[4096] ;
  FILE * fp ;
  char * t1, * t2 ;
  char * pos1 , * pos2 ;
  char *xposedir[] = { "z2x" , "x2z" , "x2y" , "y2x" , "z2y" , "y2z" , 0L } ;
  char ** x ;
  char indices[NAMELEN], post[NAMELEN], varname[NAMELEN], varref[NAMELEN] ;

  if ( dirname == NULL ) return(1) ;

  for ( p = Xposes ; p != NULL ; p = p->next )
  {
    for ( x = xposedir ; *x ; x++ )
    {
      strcpy( commname, p->name ) ;
      make_upper_case(commname) ;
      if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s_%s.inc",dirname,commname, *x) ; }
      else                       { sprintf(fname,"%s_%s.inc",commname,*x) ; }
      if ((fp = fopen( fname , "w" )) == NULL ) 
      {
        fprintf(stderr,"WARNING: gen_halos in registry cannot open %s for writing\n",fname ) ;
        continue ; 
      }

      print_warning(fp,fname) ;
      fprintf(fp,"#ifndef DATA_CALLS_INCLUDED\n") ;
      fprintf(fp,"--- DELIBERATE SYNTAX ERROR: THIS ROUTINE SHOULD INCLUDE \"%s_data_calls.inc\"\n",p->use+4) ;
      fprintf(fp,"    BECAUSE IT CONTAINS AN RSL TRANSPOSE OPERATION\n" ) ;
      fprintf(fp,"#endif\n") ;
      fprintf(fp,"IF ( grid%%comms( %s ) == invalid_message_value ) THEN\n",commname ) ;

      fprintf(fp,"  CALL wrf_debug ( 50 , 'setting up xpose %s' )\n",commname ) ;
      fprintf(fp,"  CALL setup_xpose_rsl( grid )\n") ;
      fprintf(fp,"  CALL reset_msgs_xpose\n" ) ;

      strcpy( tmp, p->comm_define ) ;
      strcpy( commuse, p->use ) ;
      t1 = strtok_rentr( tmp , ";" , &pos1 ) ;
      while ( t1 != NULL )
      {
        strcpy( tmp2 , t1 ) ;

/* Z array */
        t2 = strtok_rentr(tmp2,",", &pos2) ;
        if ((q = get_entry_r( t2, commuse, Domain.fields )) == NULL )    
         { fprintf(stderr,"WARNING 3 : %s in xpose spec %s (%s) is not defined in registry.\n",t2,commname,commuse) ; goto skiperific ; }
        strcpy( varref, t2 ) ;
        if ( q->node_kind & FIELD  && ! (q->node_kind & I1) ) {
           if ( !strncmp( q->use,  "dyn_", 4 )) {
                char * core ;
                core = q->use+4 ;
                sprintf(varref,"grid%%%s_%s",core,t2) ;
           } else {
                sprintf(varref,"grid%%%s",t2) ;
           }
        }
        if ( q->proc_orient != ALL_Z_ON_PROC ) 
         { fprintf(stderr,"WARNING: %s in xpose spec %s is not ALL_Z_ON_PROC.\n",t2,commname) ; goto skiperific ; }
        if ( q->ndims != 3 )
         { fprintf(stderr,"WARNING: boundary array %s must be 3D to be member of xpose spec %s.\n",t2,commname) ; goto skiperific ; }
        if ( q->boundary_array )
         { fprintf(stderr,"WARNING: boundary array %s cannot be member of xpose spec %s.\n",t2,commname) ; goto skiperific ; }
        strcpy (indices,"");
        if ( sw_deref_kludge &&  strchr (t2, '%') != NULLCHARPTR )
        {
          sprintf(post,")") ;
          sprintf(indices, "%s",index_with_firstelem("(","",tmp3,q,post)) ;
        }
        fprintf(fp," CALL add_msg_xpose_%s ( %s%s ,", q->type->name, varref,indices ) ;
        q->subject_to_communication = 1 ;         /* Indicate that this field may be communicated */

/* X array */
        t2 = strtok_rentr( NULL , "," , &pos2 ) ;
        if ((q = get_entry_r( t2, commuse, Domain.fields )) == NULL )    
         { fprintf(stderr,"WARNING 4 : %s in xpose spec %s (%s) is not defined in registry.\n",t2,commname,commuse) ; goto skiperific ; }
        strcpy( varref, t2 ) ;
        if ( q->node_kind & FIELD  && ! (q->node_kind & I1) ) {
           if ( !strncmp( q->use,  "dyn_", 4 )) {
                char * core ;
                core = q->use+4 ;
                sprintf(varref,"grid%%%s_%s",core,t2) ;
           } else {
                sprintf(varref,"grid%%%s",t2) ;
           }
        }
        if ( q->proc_orient != ALL_X_ON_PROC ) 
         { fprintf(stderr,"WARNING: %s in xpose spec %s is not ALL_X_ON_PROC.\n",t2,commname) ; goto skiperific ; }
        if ( q->ndims != 3 )
         { fprintf(stderr,"WARNING: boundary array %s must be 3D to be member of xpose spec %s.\n",t2,commname) ; goto skiperific ; }
        if ( q->boundary_array )
         { fprintf(stderr,"WARNING: boundary array %s cannot be member of xpose spec %s.\n",t2,commname) ; goto skiperific ; }
        strcpy (indices,"");
        if ( sw_deref_kludge &&  strchr (t2, '%') != NULLCHARPTR )
        {
          sprintf(post,")") ;
          sprintf(indices, "%s",index_with_firstelem("(","",tmp3,q,post)) ;
        }
        fprintf(fp," %s%s ,", varref, indices ) ;
        q->subject_to_communication = 1 ;         /* Indicate that this field may be communicated */

/* Y array */
        t2 = strtok_rentr( NULL , "," , &pos2 ) ;
        if ((q = get_entry_r( t2, commuse, Domain.fields )) == NULL )    
         { fprintf(stderr,"WARNING 5 : %s in xpose spec %s (%s)is not defined in registry.\n",t2,commname,commuse) ; goto skiperific ; }
        strcpy( varref, t2 ) ;
        if ( q->node_kind & FIELD  && ! (q->node_kind & I1) ) {
           if ( !strncmp( q->use,  "dyn_", 4 )) {
                char * core ;
                core = q->use+4 ;
                sprintf(varref,"grid%%%s_%s",core,t2) ;
           } else {
                sprintf(varref,"grid%%%s",t2) ;
           }
        }
        if ( q->proc_orient != ALL_Y_ON_PROC ) 
         { fprintf(stderr,"WARNING: %s in xpose spec %s is not ALL_Y_ON_PROC.\n",t2,commname) ; goto skiperific ; }
        if ( q->ndims != 3 )
         { fprintf(stderr,"WARNING: boundary array %s must be 3D to be member of xpose spec %s.\n",t2,commname) ; goto skiperific ; }
        if ( q->boundary_array )
         { fprintf(stderr,"WARNING: boundary array %s cannot be member of xpose spec %s.\n",t2,commname) ; goto skiperific ; }
        strcpy (indices,"");
        if ( sw_deref_kludge &&  strchr (t2, '%') != NULLCHARPTR )
        {
          sprintf(post,")") ;
          sprintf(indices, "%s",index_with_firstelem("(","",tmp3,q,post)) ;
        }
        fprintf(fp," %s%s , 3 )\n", varref, indices ) ;
        q->subject_to_communication = 1 ;         /* Indicate that this field may be communicated */
        t1 = strtok_rentr( NULL , ";" , &pos1 ) ;
      }
      fprintf(fp,"  CALL define_xpose ( grid%%domdesc , grid%%comms ( %s ) )\n", commname ) ;
      fprintf(fp,"ENDIF\n") ;
      fprintf(fp,"CALL wrf_debug ( 50 , 'calling wrf_dm_xpose_%s for %s')\n",*x,commname ) ;
      fprintf(fp,"CALL wrf_dm_xpose_%s ( grid%%domdesc , grid%%comms, %s )\n", *x , commname ) ;

      close_the_file(fp) ;
    }
skiperific:
    ;
  }
  return(0) ;
}

int
gen_comm_descrips ( char * dirname )
{
  node_t * p ;
  char * fn = "dm_comm_cpp_flags" ;
  char commname[NAMELEN] ;
  char fname[NAMELEN] ;
  FILE * fp ;
  int ncomm ;

  if ( dirname == NULL ) return(1) ;

  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }

  if ((fp = fopen( fname , "w" )) == NULL )
  {
    fprintf(stderr,"WARNING: gen_comm_descrips in registry cannot open %s for writing\n",fname ) ;
  }

  ncomm = 1 ;
  for ( p = Halos ; p != NULL ; p = p->next )
  {
    strcpy( commname, p->name ) ;
    make_upper_case(commname) ;
    fprintf(fp,"-D%s=%d\n",commname,ncomm++) ;
  }
  for ( p = Periods ; p != NULL ; p = p->next )
  {
    strcpy( commname, p->name ) ;
    make_upper_case(commname) ;
    fprintf(fp,"-D%s=%d\n",commname,ncomm++) ;
  }
  for ( p = Xposes ; p != NULL ; p = p->next )
  {
    strcpy( commname, p->name ) ;
    make_upper_case(commname) ;
    fprintf(fp,"-D%s=%d\n",commname,ncomm++) ;
  }
  fprintf(fp,"-DWRF_RSL_NCOMMS=%d\n",ncomm-1 ) ;
  return(0) ;
}

/*



*/

/* for each core, generate the halo updates to allow shifting all state data */
int
gen_shift (  char * dirname )
{
  int i, ncore ;
  FILE * fp ;
  node_t *p, *q, *dimd ;
  char * corename ;
  char **direction ;
  char *directions[] = { "x", "y", 0L } ;
  char fname[NAMELEN], vname[NAMELEN], vname2[NAMELEN], core[NAMELEN] ;
  char indices[NAMELEN], post[NAMELEN], tmp3[NAMELEN] ;
  int zdex ;
int said_it = 0 ;

  for ( direction = directions ; *direction != NULL ; direction++ )
  {
  for ( ncore = 0 ; ncore < get_num_cores() ; ncore++ )
  {
    corename = get_corename_i(ncore) ;
    if ( dirname == NULL || corename == NULL ) return(1) ;
    if ( strlen(dirname) > 0 )
     { sprintf(fname,"%s/%s_shift_halo_%s.inc",dirname,corename,*direction) ; }
    else
     { sprintf(fname,"%s_shift_halo_%s.inc",corename,*direction) ; }
    if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
    print_warning(fp,fname) ;
    fprintf(fp,"IF ( grid%%shift_%s == invalid_message_value ) THEN\n",*direction ) ;
    fprintf(fp,"  CALL wrf_debug ( 50 , 'set up halo for %s shift' )\n",*direction ) ;
    fprintf(fp,"  CALL setup_halo_rsl( grid )\n" ) ;
    fprintf(fp,"  CALL reset_msgs_%s_shift\n", *direction ) ;

    for ( p = Domain.fields ; p != NULL ; p = p->next )
    {

/* special cases in WRF */
if ( !strcmp( p->name , "xf_ens" ) || !strcmp( p->name , "pr_ens" ) ||
     !strcmp( p->name , "abstot" ) || !strcmp( p->name , "absnxt" ) ||
     !strcmp( p->name , "emstot" ) || !strcmp( p->name , "obs_savwt" ) ) {
  if ( sw_move && ! said_it ) { fprintf(stderr,"Info only - not an error: Moving nests not implemented for Grell Ens. Cumulus\n") ;
                                fprintf(stderr,"Info only - not an error: Moving nests not implemented for CAM radiation\n") ;
                                fprintf(stderr,"Info only - not an error: Moving nests not implemented for Observation Nudging\n") ;
  said_it = 1 ; }
  continue ;
}

      if (( p->node_kind & (FIELD | FOURD) ) && p->ndims >= 2 && ! p->boundary_array &&
	  ((!strncmp(p->use,"dyn_",4) && !strcmp(corename,p->use+4)) || strncmp(p->use,"dyn_",4)))
      {

        if ( p->node_kind & FOURD ) {
          sprintf(core,"") ;
        } else {
          if (!strncmp( p->use, "dyn_", 4))   sprintf(core,"%s_",corename) ;
          else                                sprintf(core,"") ;
        }

/* make sure that the only things we are shifting are arrays that have a decomposed X and a Y dimension */
        if ( get_dimnode_for_coord( p , COORD_X ) && get_dimnode_for_coord( p , COORD_Y ) ) {
	  if ( p->type->type_type == SIMPLE )
	  {
	    for ( i = 1 ; i <= p->ntl ; i++ )
	    {
              if ( p->ntl > 1 ) sprintf(vname,"%s_%d",p->name,i ) ;
              else              sprintf(vname,"%s",p->name ) ;
              if ( p->ntl > 1 ) sprintf(vname2,"%s%s_%d",core,p->name,i ) ;
              else              sprintf(vname2,"%s%s",core,p->name ) ;
	      if ( p->node_kind & FOURD )
              {
                node_t *member ;
                zdex = get_index_for_coord( p , COORD_Z ) ;
                if ( zdex >=1 && zdex <= 3 )
                {
                  for ( member = p->members ; member != NULL ; member = member->next )
                  {
                    if ( strcmp( member->name, "-" ) )
                    {
                      fprintf(fp,
   "  if ( P_%s .GT. 1 ) CALL add_msg_%s_shift_%s ( %s ( grid%%sm31,grid%%sm32,grid%%sm33,P_%s), glen(%d) )\n",
                         member->name, *direction, p->type->name, vname, member->name, zdex+1 ) ;
                      p->subject_to_communication = 1 ;
                    }
                  }
                }
                else
                {
                  fprintf(stderr,"WARNING: %d some dimension info missing for 4d array %s\n",zdex,t2) ;
                }
              }
              else
	      {
                strcpy (indices,"");
                if ( sw_deref_kludge ) /* &&  strchr (p->name, '%') != NULLCHARPTR ) */
                {
                  sprintf(post,")") ;
                  sprintf(indices, "%s",index_with_firstelem("(","",tmp3,p,post)) ;
                }
                dimd = get_dimnode_for_coord( p , COORD_Z ) ;
                zdex = get_index_for_coord( p , COORD_Z ) ;
                if ( dimd != NULL )
                {
                  char dimstrg[256] ;

                  if      ( dimd->len_defined_how == DOMAIN_STANDARD )
                      sprintf(dimstrg,"(glen(%d))",zdex+1) ;
                  else if ( dimd->len_defined_how == NAMELIST )
                  {
                    if ( !strcmp(dimd->assoc_nl_var_s,"1") )
                      sprintf(dimstrg,"config_flags%%%s",dimd->assoc_nl_var_e) ;
                    else
                      sprintf(dimstrg,"(config_flags%%%s - config_flags%%%s + 1)",dimd->assoc_nl_var_e,dimd->assoc_nl_var_s) ;
                  }
                  else if ( dimd->len_defined_how == CONSTANT )
                      sprintf(dimstrg,"(%d - %d + 1)",dimd->coord_end,dimd->coord_start) ;
  
                  fprintf(fp,"  CALL add_msg_%s_shift_%s ( grid%%%s%s , %s )\n", *direction, p->type->name, vname2, indices, dimstrg ) ;
                  p->subject_to_communication = 1 ;
                }
                else if ( p->ndims == 2 )  /* 2d */
                {
                  fprintf(fp,"  CALL add_msg_%s_shift_%s ( grid%%%s%s , %s )\n", *direction, p->type->name, vname2, indices, "1" ) ;
                  p->subject_to_communication = 1 ;
                }
              }
            }
	  }
	}
      }
    }
    fprintf(fp,"  CALL stencil_%s_shift ( grid%%domdesc , grid%%shift_%s )\n", *direction , *direction ) ;
    fprintf(fp,"ENDIF\n") ;
    fprintf(fp,"  CALL wrf_debug ( 50 , 'exchange halo for %s shift' )\n",*direction ) ;
    fprintf(fp,"CALL rsl_exch_stencil ( grid%%domdesc , grid%%shift_%s )\n", *direction ) ;

    for ( p = Domain.fields ; p != NULL ; p = p->next )
    {

/* special cases in WRF */
if ( !strcmp( p->name , "xf_ens" ) || !strcmp( p->name , "pr_ens" ) ||
     !strcmp( p->name , "abstot" ) || !strcmp( p->name , "absnxt" ) ||
     !strcmp( p->name , "emstot" ) || !strcmp( p->name , "obs_savwt" ) ) {
  continue ;
}
      if ( p->node_kind & FOURD ) {
        sprintf(core,"") ;
      } else {
        if (!strncmp( p->use, "dyn_", 4))   sprintf(core,"%s_",corename) ;
        else                                sprintf(core,"") ;
      }

      if (( p->node_kind & (FIELD | FOURD) ) && p->ndims >= 2 && ! p->boundary_array &&
	  ((!strncmp(p->use,"dyn_",4) && !strcmp(corename,p->use+4)) || strncmp(p->use,"dyn_",4)))
      {
/* make sure that the only things we are shifting are arrays that have a decomposed X and a Y dimension */
        if ( get_dimnode_for_coord( p , COORD_X ) && get_dimnode_for_coord( p , COORD_Y ) ) {
  	  if ( p->type->type_type == SIMPLE )
	  {
	    for ( i = 1 ; i <= p->ntl ; i++ )
	    {
              if ( p->ntl > 1 ) sprintf(vname,"%s_%d",p->name,i ) ;
              else              sprintf(vname,"%s",p->name ) ;
              if ( p->ntl > 1 ) sprintf(vname2,"%s%s_%d",core,p->name,i ) ;
              else              sprintf(vname2,"%s%s",core,p->name ) ;

	      if ( p->node_kind & FOURD )
              {
                node_t *member ;
                zdex = get_index_for_coord( p , COORD_Z ) ;
                if ( zdex >=1 && zdex <= 3 )
                {
                  for ( member = p->members ; member != NULL ; member = member->next )
                  {
                    if ( strcmp( member->name, "-" ) )
                    {
                      if ( !strcmp( *direction, "x" ) )
                      {
                        fprintf(fp,
   "  if ( P_%s .GT. 1 ) %s ( ips:min(ide%s,ipe),:,jms:jme,P_%s) = %s (ips+px:min(ide%s,ipe)+px,:,jms:jme,P_%s)\n",
                         member->name, vname, member->stag_x?"":"-1", member->name, vname, member->stag_x?"":"-1", member->name ) ;
                      }
                      else
                      {
                        fprintf(fp,
   "  if ( P_%s .GT. 1 ) %s ( ims:ime,:,jps:min(jde%s,jpe),P_%s) = %s (ims:ime,:,jps+py:min(jde%s,jpe)+py,P_%s)\n",
                         member->name, vname, member->stag_y?"":"-1", member->name, vname, member->stag_y?"":"-1", member->name ) ;
                      }
                    }
                  }
                }
                else
                {
                  fprintf(stderr,"WARNING: %d some dimension info missing for 4d array %s\n",zdex,t2) ;
                }
              }
              else
	      {
	        char * vdim ;
	        vdim = "" ;
	        if ( p->ndims == 3 ) vdim = ":," ;
                if ( !strcmp( *direction, "x" ) )
                {
                  fprintf(fp,"grid%%%s (ips:min(ide%s,ipe),%sjms:jme) = grid%%%s (ips+px:min(ide%s,ipe)+px,%sjms:jme)\n", vname2,  p->stag_x?"":"-1", vdim, vname2, p->stag_x?"":"-1", vdim ) ;
                }
                else
	        {
                  fprintf(fp,"grid%%%s (ims:ime,%sjps:min(jde%s,jpe)) = grid%%%s (ims:ime,%sjps+py:min(jde%s,jpe)+py)\n", vname2, vdim,  p->stag_y?"":"-1", vname2, vdim, p->stag_y?"":"-1" ) ;
                }
              }
            }
	  }
	}
      }
    }
    close_the_file(fp) ;
  }
  }
}

int
gen_datacalls ( char * dirname )
{
  int i ;
  FILE * fp ;
  char * corename ;
  char * fn = "data_calls.inc" ;
  char fname[NAMELEN] ;

  for ( i = 0 ; i < get_num_cores() ; i++ )
  {
    corename = get_corename_i(i) ;
    if ( dirname == NULL || corename == NULL ) return(1) ;
    if ( strlen(dirname) > 0 )
     { sprintf(fname,"%s/%s_%s",dirname,corename,fn) ; }
    else
     { sprintf(fname,"%s_%s",corename,fn) ; }
    if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
    print_warning(fp,fname) ;
    fprintf(fp," CALL rsl_start_register_f90\n") ;
    parent_type = SIMPLE;
    gen_datacalls1( fp , corename, "grid%", FIELD , Domain.fields ) ;
    gen_datacalls1( fp , corename, "", FOURD , Domain.fields ) ;
    fprintf(fp,"#ifdef REGISTER_I1\n") ;
    gen_datacalls1( fp , corename, "", I1 , Domain.fields ) ;
    fprintf(fp,"#endif\n") ;
    fprintf(fp," CALL rsl_end_register_f90\n") ;
    fprintf(fp,"#define  DATA_CALLS_INCLUDED\n") ;
    close_the_file(fp) ;
  }
  return(0) ;
}

int
gen_datacalls1 ( FILE * fp , char * corename , char * structname , int mask , node_t * node )
{
  node_t * p, * q  ;
  int i, member_number ;
  char tmp[NAMELEN],tmp2[NAMELEN], tc ;
  char indices[NAMELEN], post[NAMELEN] ;
  char s0[NAMELEN], s1[NAMELEN], s2[NAMELEN] ;
  char e0[NAMELEN], e1[NAMELEN], e2[NAMELEN] ;

  for ( p = node ; p != NULL ; p = p->next )
  {
    if ( ( mask & p->node_kind ) &&
        ((!strncmp(p->use,"dyn_",4) && !strcmp(corename,p->use+4)) || strncmp(p->use,"dyn_",4)))
    {
    if ( (p->subject_to_communication == 1) || ( p->type->type_type == DERIVED ) )
    {
      if ( p->type->type_type == SIMPLE )
      {
        if ( !strcmp( p->type->name , "real" ) ) tc = 'R' ;
        if ( !strcmp( p->type->name , "double" ) ) tc = 'D' ;
        if ( !strcmp( p->type->name , "integer" ) ) tc = 'I' ;
        for ( i = 1 ; i <= p->ntl ; i++ )
        {
/* IF (P_QI .ge. P_FIRST_SCALAR */
          if ( p->members != NULL )   /* a 4d array */
          {
            member_number = 0 ;
            for ( q = p->members ; q != NULL ; q = q->next )
            {
              get_elem( "grid%", "", s0, 0, p , 0 ) ;
              get_elem( "grid%", "", s1, 1, p , 0 ) ;
              get_elem( "grid%", "", s2, 2, p , 0 ) ;

              get_elem( "grid%", "", e0, 0, p , 1 ) ;
              get_elem( "grid%", "", e1, 1, p , 1 ) ;
              get_elem( "grid%", "", e2, 2, p , 1 ) ;

              sprintf(tmp, "(%s,%s,%s,1+%d)", s0, s1, s2, member_number ) ;
              sprintf(tmp2, "(%s-%s+1)*(%s-%s+1)*(%s-%s+1)*%cWORDSIZE",e0,s0,e1,s1,e2,s2,tc) ;
              if ( p->ntl > 1 ) fprintf(fp," IF(1+%d.LE.num_%s)CALL rsl_register_f90_base_and_size ( %s%s_%d %s , &\n %s  )\n",
                                             member_number,p->name,structname,p->name,i,tmp,tmp2) ;
              else              fprintf(fp," IF(1+%d.LE.num_%s)CALL rsl_register_f90_base_and_size ( %s%s %s, &\n %s )\n",
                                             member_number,p->name,structname,p->name,tmp,tmp2) ;
              member_number++ ;
            }
          }
          else
          {
            char ca[NAMELEN] ;
            strcpy (indices,"");
            if ( sw_deref_kludge )
            {
              sprintf(post,")") ;
              sprintf(indices, "%s",index_with_firstelem("(","",tmp,p,post)) ;
            }
            strcpy( ca, "" ) ;
            if (!strncmp( p->use , "dyn_", 4 )) { char * cb ;  cb = p->use+4 ; sprintf(ca,"%s_", cb) ; }
            if ( p->ntl > 1 ) fprintf(fp," CALL rsl_register_f90_base_and_size ( %s%s%s_%d%s , SIZE( %s%s%s_%d ) * %cWORDSIZE )\n",
                                                                                   structname,ca,p->name,i,indices,
                                                                                   structname,ca,p->name,i,tc ) ;
            else              fprintf(fp," CALL rsl_register_f90_base_and_size ( %s%s%s%s , SIZE( %s%s%s  ) * %cWORDSIZE )\n",
                                                                                   structname,ca,p->name,indices,
                                                                                   structname,ca,p->name, tc) ;
          }
        }
      }
      else if ( p->type->type_type == DERIVED )
      {
        parent_type = DERIVED;
        sprintf( tmp , "grid%%%s%%", p->name ) ; 
        gen_datacalls1 ( fp , corename , tmp , mask, p->type->fields ) ;
      }
    }
  }
  }
  return(0) ;
}

/*****************/
/*****************/

gen_nest_packing ( char * dirname )
{
  gen_nest_pack( dirname ) ;   
  gen_nest_unpack( dirname ) ; 
}

#define PACKIT 1
#define UNPACKIT 2

int
gen_nest_pack ( char * dirname )
{
  int i ;
  FILE * fp ;
  char * corename ;
  char * fnlst[] = { "nest_interpdown_pack.inc" , "nest_forcedown_pack.inc" , "nest_feedbackup_pack.inc", 0L } ;
  int down_path[] = { INTERP_DOWN , FORCE_DOWN , INTERP_UP } ;
  int ipath ;
  char ** fnp ; char * fn ;
  char fname[NAMELEN] ;
  node_t *node, *p, *dim ;
  int xdex, ydex, zdex ;
  char ddim[3][2][NAMELEN] ;
  char mdim[3][2][NAMELEN] ;
  char pdim[3][2][NAMELEN] ;
  char vname[NAMELEN] ; char tag[NAMELEN] ; char core[NAMELEN] ;
  int d2, d3 ;

  for ( fnp = fnlst , ipath = 0 ; *fnp ; fnp++ , ipath++ )
  {
    fn = *fnp ;
    for ( i = 0 ; i < get_num_cores() ; i++ )
    {
      corename = get_corename_i(i) ;
      if ( dirname == NULL || corename == NULL ) return(1) ;
      if ( strlen(dirname) > 0 ) {
       if ( strlen( corename ) > 0 )
         { sprintf(fname,"%s/%s_%s",dirname,corename,fn) ; }
       else
         { sprintf(fname,"%s/%s",dirname,fn) ; }
      } else { 
       if ( strlen( corename ) > 0 ) 
          { sprintf(fname,"%s_%s",corename,fn) ; }
       else
          { sprintf(fname,"%s",fn) ; }
      }
      if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
      print_warning(fp,fname) ;

      d2 = 0 ;
      d3 = 0 ;
      node = Domain.fields ;

      count_fields ( node , &d2 , &d3 , corename , down_path[ipath] ) ;

      if ( d2 + d3 > 0 ) {
        if ( down_path[ipath] == INTERP_UP )
        {

          fprintf(fp,"msize = %d * nlev + %d\n", d3, d2 ) ;
          fprintf(fp,"CALL rsl_to_parent_info( grid%%domdesc, intermediate_grid%%domdesc ,  &\n") ;
          fprintf(fp,"                        msize*RWORDSIZE,                             &\n") ;
          fprintf(fp,"                        i,j,nig,njg,cm,cn,pig,pjg,retval )\n") ;
          fprintf(fp,"DO while ( retval .eq. 1 )\n") ;
 
          gen_nest_packunpack ( fp , Domain.fields, corename, PACKIT, down_path[ipath] ) ;

          fprintf(fp,"CALL rsl_to_parent_info( grid%%domdesc, intermediate_grid%%domdesc ,  &\n") ;
          fprintf(fp,"                         msize*RWORDSIZE,                             &\n") ;
          fprintf(fp,"                         i,j,nig,njg,cm,cn,pig,pjg,retval )\n") ;
          fprintf(fp,"ENDDO\n") ;

        }
        else
        {

          fprintf(fp,"msize = %d * nlev + %d\n", d3, d2 ) ;
          fprintf(fp,"CALL rsl_to_child_info( grid%%domdesc, intermediate_grid%%domdesc ,  &\n") ;
          fprintf(fp,"                        msize*RWORDSIZE,                             &\n") ;
          fprintf(fp,"                        i,j,pig,pjg,cm,cn,nig,njg,retval )\n") ;
          fprintf(fp,"DO while ( retval .eq. 1 )\n") ;
  
          gen_nest_packunpack ( fp , Domain.fields, corename, PACKIT, down_path[ipath] ) ;

          fprintf(fp,"CALL rsl_to_child_info( grid%%domdesc, intermediate_grid%%domdesc ,  &\n") ;
          fprintf(fp,"                        msize*RWORDSIZE,                             &\n") ;
          fprintf(fp,"                        i,j,pig,pjg,cm,cn,nig,njg,retval )\n") ;
          fprintf(fp,"ENDDO\n") ;

        }
      }

      close_the_file(fp) ;
    }
  }
  return(0) ;
}

int
gen_nest_unpack ( char * dirname )
{
  int i ;
  FILE * fp ;
  char * corename ;
  char * fnlst[] = { "nest_interpdown_unpack.inc" , "nest_forcedown_unpack.inc" , "nest_feedbackup_unpack.inc" , 0L } ;
  int down_path[] = { INTERP_DOWN , FORCE_DOWN , INTERP_UP } ;
  int ipath ;
  char ** fnp ; char * fn ;
  char fname[NAMELEN] ;
  node_t *node, *p, *dim ;
  int xdex, ydex, zdex ;
  char ddim[3][2][NAMELEN] ;
  char mdim[3][2][NAMELEN] ;
  char pdim[3][2][NAMELEN] ;
  char vname[NAMELEN] ; char tag[NAMELEN] ; char core[NAMELEN] ;
  int d2, d3 ;

  for ( fnp = fnlst , ipath = 0 ; *fnp ; fnp++ , ipath++ )
  {
    fn = *fnp ;
    for ( i = 0 ; i < get_num_cores() ; i++ )
    {
      d2 = 0 ;
      d3 = 0 ;
      node = Domain.fields ;

      corename = get_corename_i(i) ;
      if ( dirname == NULL || corename == NULL ) return(1) ;
      if ( strlen(dirname) > 0 )
       { sprintf(fname,"%s/%s_%s",dirname,corename,fn) ; }
      else
       { sprintf(fname,"%s_%s",corename,fn) ; }
      if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
      print_warning(fp,fname) ;

      count_fields ( node , &d2 , &d3 , corename , down_path[ipath] ) ;

      if ( d2 + d3 > 0 ) {
        if ( down_path[ipath] == INTERP_UP )
        {

          fprintf(fp,"CALL rsl_from_child_info(i,j,pig,pjg,cm,cn,nig,njg,retval)\n") ;
          fprintf(fp,"DO while ( retval .eq. 1 )\n") ;

          gen_nest_packunpack ( fp , Domain.fields, corename, UNPACKIT, down_path[ipath] ) ;

          fprintf(fp,"CALL rsl_from_child_info(i,j,pig,pjg,cm,cn,nig,njg,retval)\n") ;
          fprintf(fp,"ENDDO\n") ;

        }
        else
        {

          fprintf(fp,"CALL rsl_from_parent_info(i,j,nig,njg,cm,cn,pig,pjg,retval)\n") ;
          fprintf(fp,"DO while ( retval .eq. 1 )\n") ;
          gen_nest_packunpack ( fp , Domain.fields, corename, UNPACKIT, down_path[ipath] ) ;
          fprintf(fp,"CALL rsl_from_parent_info(i,j,nig,njg,cm,cn,pig,pjg,retval)\n") ;
          fprintf(fp,"ENDDO\n") ;

        }
      }

      close_the_file(fp) ;
    }
  }
  return(0) ;
}

int
gen_nest_packunpack ( FILE *fp , node_t * node , char * corename, int dir, int down_path )
{
  int i ;
  node_t *p, *p1, *dim ;
  int d2, d3, xdex, ydex, zdex ;
  char ddim[3][2][NAMELEN] ;
  char mdim[3][2][NAMELEN] ;
  char pdim[3][2][NAMELEN] ;
  char vname[NAMELEN], vname2[NAMELEN], dexes[NAMELEN] ; char tag[NAMELEN] ; char core[NAMELEN] ;
  char c, d ;

  for ( p1 = node ;  p1 != NULL ; p1 = p1->next )
  {

    if ( p1->node_kind & FOURD )
    {
      gen_nest_packunpack ( fp, p1->members, corename, dir , down_path ) ;  /* RECURSE over members */
      continue ;
    }
    else
    {
      p = p1 ;
    }

    if ( p->io_mask & down_path )
    {
      if ((!strncmp( p->use, "dyn_", 4) && !strcmp(p->use+4,corename)) || strncmp( p->use, "dyn_", 4))
      {

        if (!strncmp( p->use, "dyn_", 4))   sprintf(core,"%s",corename) ;
        else                                sprintf(core,"") ;

        if ( p->ntl > 1 ) sprintf(tag,"_2") ;
        else              sprintf(tag,"") ;

        set_dim_strs ( p , ddim , mdim , pdim , "c", 0 ) ;
        zdex = get_index_for_coord( p , COORD_Z ) ;
        xdex = get_index_for_coord( p , COORD_X ) ;
        ydex = get_index_for_coord( p , COORD_Y ) ;

        if ( down_path == INTERP_UP )
        {
          c = ( dir == PACKIT )?'n':'p' ;
          d = ( dir == PACKIT )?'2':'1' ;
        } else {
          c = ( dir == UNPACKIT )?'n':'p' ;
          d = ( dir == UNPACKIT )?'2':'1' ;
        }

        if ( zdex >= 0 ) {
          if      ( xdex == 0 && zdex == 1 && ydex == 2 )  sprintf(dexes,"pig,k,pjg") ;
          else if ( zdex == 0 && xdex == 1 && ydex == 2 )  sprintf(dexes,"k,pig,pjg") ;
          else if ( xdex == 0 && ydex == 1 && zdex == 2 )  sprintf(dexes,"pig,pjg,k") ;
        } else {
          if ( xdex == 0 && ydex == 1 )  sprintf(dexes,"pig,pjg") ;
          if ( ydex == 0 && xdex == 1 )  sprintf(dexes,"pjg,pig") ;
        }

        /* construct variable name */
        if ( p->scalar_array_member )
        {
          sprintf(vname,"%s%s(%s,P_%s)",p->use,tag,dexes,p->name) ;
          if ( strlen(core) > 0 )
            sprintf(vname2,"%s_%s%s(%s,P_%s)",core,p->use,tag,dexes,p->name) ;
          else
            sprintf(vname2,"%s%s(%s,P_%s)",p->use,tag,dexes,p->name) ;
        }
        else
        {
          sprintf(vname,"%s%s(%s)",p->name,tag,dexes) ;
          if ( strlen(core) > 0 )
            sprintf(vname2,"%s_%s%s(%s)",core,p->name,tag,dexes) ;
          else
            sprintf(vname2,"%s%s(%s)",p->name,tag,dexes) ;
        }

        if ( p->scalar_array_member )
	{
fprintf(fp,"IF ( P_%s .GE. PARAM_FIRST_SCALAR ) THEN\n",p->name) ;
	}

        if ( dir == UNPACKIT ) 
        {
          if ( down_path == INTERP_UP )
	  {
            if ( zdex >= 0 ) {
fprintf(fp,"CALL rsl_from_child_msg(((%s)-(%s)+1)*RWORDSIZE,xv) ;\n",ddim[zdex][1], ddim[zdex][0] ) ;
            } else {
fprintf(fp,"CALL rsl_from_child_msg(RWORDSIZE,xv)\n" ) ;
            }
fprintf(fp,"IF ( %s_cd_feedback_mask( pig, ips_save, ipe_save , pjg, jps_save, jpe_save, %s, %s ) ) THEN\n",
                 corename, p->stag_x?".TRUE.":".FALSE." ,p->stag_y?".TRUE.":".FALSE." ) ;
            if ( zdex >= 0 ) {
fprintf(fp,"DO k = %s,%s\nNEST_INFLUENCE(grid%%%s,xv(k))\nENDDO\n", ddim[zdex][0], ddim[zdex][1], vname2 ) ;
            } else {
fprintf(fp,"grid%%%s = xv(1) ;\n", vname2) ;
            }
fprintf(fp,"ENDIF\n") ;
          }
          else
          {
            if ( zdex >= 0 ) {
fprintf(fp,"CALL rsl_from_parent_msg(((%s)-(%s)+1)*RWORDSIZE,xv)\nDO k = %s,%s\ngrid%%%s = xv(k)\nENDDO\n",
                                    ddim[zdex][1], ddim[zdex][0], ddim[zdex][0], ddim[zdex][1], vname2) ;
            } else {
fprintf(fp,"CALL rsl_from_parent_msg(RWORDSIZE,xv)\ngrid%%%s = xv(1)\n", vname2) ;
            }
          }
        }
        else
        {
          if ( down_path == INTERP_UP )
	  {
            if ( zdex >= 0 ) {
fprintf(fp,"DO k = %s,%s\nxv(k)= intermediate_grid%%%s\nENDDO\nCALL rsl_to_parent_msg(((%s)-(%s)+1)*RWORDSIZE,xv)\n",
                           ddim[zdex][0], ddim[zdex][1], vname2, ddim[zdex][1], ddim[zdex][0] ) ;
            } else {
fprintf(fp,"xv(1)= intermediate_grid%%%s\nCALL rsl_to_parent_msg(RWORDSIZE,xv)\n", vname2) ;
            }
          }
          else
          {
            if ( zdex >= 0 ) {
fprintf(fp,"DO k = %s,%s\nxv(k)= grid%%%s\nENDDO\nCALL rsl_to_child_msg(((%s)-(%s)+1)*RWORDSIZE,xv)\n",
                           ddim[zdex][0], ddim[zdex][1], vname2, ddim[zdex][1], ddim[zdex][0] ) ;
            } else {
fprintf(fp,"xv(1)=grid%%%s\nCALL rsl_to_child_msg(RWORDSIZE,xv)\n", vname2) ;
            }
          }
        }
        if ( p->scalar_array_member )
	{
fprintf(fp,"ENDIF\n") ;
	}
      }
    }
  }

  return(0) ;
}

/*****************/

int
count_fields ( node_t * node , int * d2 , int * d3 , char * corename , int down_path )
{
  node_t * p ;
  int zdex ;
/* count up the total number of levels from all fields */
  for ( p = node ;  p != NULL ; p = p->next )
  {
    if ( p->node_kind == FOURD ) 
    {
      count_fields( p->members , d2 , d3 , corename , down_path ) ;  /* RECURSE */
    }
    else
    {
      if ( p->io_mask & down_path )
      {
        if ((!strncmp( p->use, "dyn_", 4) && !strcmp(p->use+4,corename)) || strncmp( p->use, "dyn_", 4))
        {
          if ( p->node_kind == FOURD )
            zdex = get_index_for_coord( p->members , COORD_Z ) ;
          else
            zdex = get_index_for_coord( p , COORD_Z ) ;
  
          if ( zdex < 0 ) {
            (*d2)++ ;   /* if no zdex then only 2 d */
          } else {
            (*d3)++ ;   /* if has a zdex then 3 d */
          }
        }
      }
    }
  }
  return(0) ;
}

/*****************/

int
gen_comms ( char * dirname )
{
  if ( sw_dm_parallel )
    fprintf(stderr,"ADVISORY: RSL version of gen_comms is linked in with registry program.\n") ;

  gen_halos( "inc" ) ;
  gen_shift( "inc" ) ;
  gen_periods( "inc" ) ;
  gen_xposes( "inc" ) ;
  gen_comm_descrips( "inc" ) ;
  gen_datacalls( "inc" ) ;
  gen_nest_packing( "inc" ) ;

  return(0) ;
}

