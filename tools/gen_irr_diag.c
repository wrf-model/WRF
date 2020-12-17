#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>

int  nChmOpts = 0;
char rxt_tbl[5][1000][128];
char chm_scheme[5][128];
int  rxt_cnt[5];

void strip_blanks( char *instring, char *outstring )
{
  int i, j, slen;
  char *c;

  slen = strlen( instring );
  strcpy( outstring,instring );
  c = instring;
  for( i=0,j=0; i < slen; i++ )
  {
    if( strncmp( c, " ", 1 ) )
      strncpy( (outstring+(j++)), c, 1 );
    c++;
  }
  c = index( outstring,'\n' );
  *c = '\0';
}

int AppendReg( char *chem_opt, int ndx )
{
   int Nrxt;
   int i,j;
   int slen;
   char *stradj = "abcdef";
   char *strt, *end;
   char *token;
   char *wstrg1;
   char path[256];
   char fname[256];
   char inln[1024],winln[1024],s[1024];
   char rxtstr[128];
   char rxtstr_tbl[1000][128];
   char buffer[128];
   char rxtsym[128];
   FILE *fp_eqn, *fp_reg;

   strcpy( fname,chem_opt );
   slen = sprintf( path, "chem/KPP/mechanisms/%s/%s.eqn",fname,fname );
   fprintf(stderr,"Using file:\n");
   fprintf(stderr,"%s\n", path);

//
//  open input *eqn file
//
   if( (fp_eqn = fopen( path, "r" )) == NULL )
   {
      fprintf(stderr,"Can not open %s for reading\n", path);
      return(-1);
   }
   fprintf(stderr,"\n");
//
//  open output registry file
//
   if( access( "Registry/registry.irr_diag",F_OK ) )
     fp_reg = fopen( "Registry/registry.irr_diag", "w" );
   else
     fp_reg = fopen( "Registry/registry.irr_diag", "a" );

   if( fp_reg == NULL ) {
     fprintf(stderr,"Can not open registry.irr_diag for writing\n");
     return(-2);
   }
   strcpy( buffer,"\"Integrated Reaction Rate\"  \"\"");
   fprintf(fp_reg,"state  real  -  ikjf  irr_diag_%s  -  -  -  - %s\n",fname,buffer);

   Nrxt = 0; 
//
//  loop over input lines
//
   while( fgets( inln, 1024, fp_eqn ) != NULL )
   {
      if( strncmp( inln, "//", 2 ) && strncmp( inln, "#", 1 ) )
      {
//
//  strip blanks from reaction string
//
        strip_blanks( inln, inln );
//
//  concatentate lines?
//
        if( rindex( inln,';' ) == NULL ) {
          do {
            fgets( winln, 1024, fp_eqn );
            strip_blanks( winln, winln );
            strcat( inln,winln );
          } while( rindex( inln,';' ) == NULL );
        }
        strt = strchr( inln,'{' );
        while( strt != NULL )
        {
          end = strchr( strt,'}' );
          slen = 0;
          if( end != NULL )
            slen = end - strt;
          if( slen > 0 )
          {
            for( i = 0; i < slen; i++ )
              if( !strncmp( strt+i,"=",1 ) ) *(strt+i) = '$';
          }
          strt += slen;
          strt = strchr( strt,'{' );
        }

        strt = strchr( inln, '}' );
        if( strt == NULL ) continue;
        strt++;
        end  = rindex( strt, '=' );
        if( end != NULL )
        {
          *end= '\0';
          slen = strlen( strt );
          wstrg1 = strt;
//
//  string to upper case
//
          slen = strlen( wstrg1 );
          for( i=0; i <= slen; i++ )
            wstrg1[i] = toupper( wstrg1[i] );
//
//  remove all text between {} pair including delimiters
//
          strt = strchr( wstrg1,'{' );
          if( strt != NULL )
          {
            end = index( wstrg1,'}' );
            if( end != NULL )
            {
              char c[128];
              *strt = '\0';
              strcpy( c,wstrg1 );
              end++;
              strcat( c,end );
              strcpy( wstrg1,c );
            }
          }

          strcpy( rxtstr_tbl[Nrxt],wstrg1 );
//
//  check for unique reaction string
//
          if( Nrxt > 0 ) 
          {
            int Nmatch = 0;
            for( i = 0; i < Nrxt; i++)
              if( !strcmp( wstrg1,rxtstr_tbl[i] ) )
                Nmatch++;
            
            if( Nmatch > 0 )
            {
              Nmatch--;
              strcat( wstrg1,"_" );
              strncat( wstrg1,stradj+Nmatch,1 );
            }
          }
          
          strcpy( rxtstr,wstrg1 );
          strcpy( rxtsym,wstrg1 );
          strcpy( rxt_tbl[ndx][Nrxt],wstrg1 );
//
//  change + to _
//
          for( i=0; i < slen; i++ )
          {
            if( ! strncmp( rxtsym+i, "+", 1 ) )
              strncpy( rxtsym+i, "_", 1 );
          }
          strcat( rxtsym,"_IRR" );
//
//  form output line
//
//        fprintf(fp_reg,"state  real  %s  ikjf  irr_diag_%s  1  -  rh9  \"%s\"  \"%s Integrated Reaction Rate\"  \"molecules/cm^3/s\"\n",rxtsym,fname,rxtstr,rxtstr);
          fprintf(fp_reg,"state  real  %s  ikjf  irr_diag_%s  1  -  rh9  \"%s\"  \"%s Integrated Reaction Rate\"  \"ppmv\"\n",rxtsym,fname,rxtsym,rxtstr);
          Nrxt++;
        }
      }
   }

   nChmOpts++;
   rxt_cnt[ndx] = Nrxt;
   strcpy( chm_scheme[ndx],chem_opt );

   fclose(fp_eqn);
   fclose(fp_reg);
   
   return(0);
}

int irr_diag_scalar_indices( char *dirname )
{
   int Nrxt;
   int i, j;
   int first, flush, s1;
   char fname[256];
   char line[132];
   char piece[132];
   char *blank = "                                                           ";
   FILE *fp_inc;

//
//  open output inc file
//
   sprintf( fname, "inc/scalar_indices_irr_diag_decls.inc");
   fp_inc = fopen( fname, "w" );

   if( fp_inc == NULL ) {
     fprintf(stderr,"Can not open %s for writing\n",fname);
     return(-2);
   }
   fprintf( fp_inc," \n");
   fprintf( fp_inc,"  INTEGER, PARAMETER :: nchm_opts = %d\n",nChmOpts);
   fprintf( fp_inc,"  INTEGER            :: chm_opt_ndx\n");
   fprintf( fp_inc," \n");
   fprintf( fp_inc,"  INTEGER            :: chm_opts_cnt(nchm_opts)\n");
   fprintf( fp_inc,"  INTEGER            :: chm_opts_ndx(nchm_opts)\n");
   fprintf( fp_inc,"  CHARACTER(len=32)  :: chm_opts_name(nchm_opts)\n");
   for( i = 0,j = 0; i < nChmOpts; i++ ) {
     if( rxt_cnt[i] > j )
       j = rxt_cnt[i];
   }
   fprintf( fp_inc,"  CHARACTER(len=64), TARGET  :: rxtsym(%d,nchm_opts)\n",j);
   fprintf( fp_inc," \n");
   fclose(fp_inc);
//
//  open output inc file
//
   sprintf( fname, "inc/scalar_indices_irr_diag.inc");
   fp_inc = fopen( fname, "w" );

   if( fp_inc == NULL ) {
     fprintf(stderr,"Can not open %s for writing\n",fname);
     return(-2);
   }

   fprintf( fp_inc," \n");
   sprintf( line,"  chm_opts_cnt(:nchm_opts) = (/ ");
   for( i = 0; i < nChmOpts; i++ ) {
     if( i == 0 ) 
       sprintf( piece," %d",rxt_cnt[i]);
     else
       sprintf( piece," ,%d",rxt_cnt[i]);
     strcat( line,piece );
   }
   strcat( line," /)\n" );
   fprintf( fp_inc,line );
   fprintf( fp_inc," \n");

   for( i = 0; i < nChmOpts; i++ ) {
     sprintf( line,"  chm_opts_name(%d) = '%s'\n",i+1,chm_scheme[i]);
     fprintf( fp_inc,line );
   }
   fprintf( fp_inc," \n");

   sprintf( line,"  chm_opts_ndx(:nchm_opts) = (/ ");
   for( i = 0; i < nChmOpts; i++ ) {
     if( i == 0 ) 
       sprintf( piece,"%s_kpp",chm_scheme[i]);
     else
       sprintf( piece," ,%s_kpp",chm_scheme[i]);
     strcat( line,piece );
   }
   strcat( line," /)\n" );
   fprintf( fp_inc,line );
   fprintf( fp_inc," \n");

   for( i = 0; i < nChmOpts,rxt_cnt[i] > 0; i++ ) {
     for( j = 0; j < rxt_cnt[i]; j++ ) {
       sprintf( line,"     rxtsym(%d,%d) = '%s'\n",j+1,i+1,rxt_tbl[i][j]);
       fprintf( fp_inc,"%s",line);
     }
     fprintf( fp_inc," \n");
   }
/*
   fprintf( fp_inc,"  IF( model_config_rec%%irr_opt(idomain) == 1 ) THEN\n");
   fprintf( fp_inc,"    CALL nl_get_chem_opt( idomain,chem_opt )\n");
   fprintf( fp_inc,"    DO chm_opt_ndx = 1,nchm_opts\n");
   fprintf( fp_inc,"      IF( chem_opt == chm_opts_ndx(chm_opt_ndx) ) THEN\n");
   fprintf( fp_inc,"        EXIT\n");
   fprintf( fp_inc,"      ENDIF\n");
   fprintf( fp_inc,"    ENDDO\n");
   fprintf( fp_inc,"    IF( chm_opt_ndx > nchm_opts ) THEN\n");
   fprintf( fp_inc,"      write(err_mes,*) 'IRR not supported for chem option ',chem_opt\n");
   fprintf( fp_inc,"      CALL wrf_error_fatal( trim(err_mes) )\n");
   fprintf( fp_inc,"    ENDIF\n");
   fprintf( fp_inc,"  ENDIF\n");
*/
   fclose(fp_inc);

   return(0);
}
