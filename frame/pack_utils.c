#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef CRAY
# ifdef NOUNDERSCORE
#      define INT_PACK_DATA  int_pack_data
#      define INT_GET_TI_HEADER_C  int_get_ti_header_c
#      define INT_GEN_TI_HEADER_C  int_gen_ti_header_c
#      define ADD_TO_BUFSIZE_FOR_FIELD_C  add_to_bufsize_for_field_c
#      define STORE_PIECE_OF_FIELD_C  store_piece_of_field_c
#      define RETRIEVE_PIECES_OF_FIELD_C  retrieve_pieces_of_field_c
#      define INIT_STORE_PIECE_OF_FIELD init_store_piece_of_field
#      define INIT_RETRIEVE_PIECES_OF_FIELD init_retrieve_pieces_of_field
# else
#   ifdef F2CSTYLE
#      define INT_PACK_DATA  int_pack_data__
#      define INT_GET_TI_HEADER_C  int_get_ti_header_c__
#      define INT_GEN_TI_HEADER_C  int_gen_ti_header_c__
#      define ADD_TO_BUFSIZE_FOR_FIELD_C  add_to_bufsize_for_field_c__
#      define STORE_PIECE_OF_FIELD_C  store_piece_of_field_c__
#      define RETRIEVE_PIECES_OF_FIELD_C  retrieve_pieces_of_field_c__
#      define INIT_STORE_PIECE_OF_FIELD init_store_piece_of_field__
#      define INIT_RETRIEVE_PIECES_OF_FIELD init_retrieve_pieces_of_field__
#   else
#      define INT_PACK_DATA  int_pack_data_
#      define INT_GET_TI_HEADER_C  int_get_ti_header_c_
#      define INT_GEN_TI_HEADER_C  int_gen_ti_header_c_
#      define ADD_TO_BUFSIZE_FOR_FIELD_C  add_to_bufsize_for_field_c_
#      define STORE_PIECE_OF_FIELD_C  store_piece_of_field_c_
#      define RETRIEVE_PIECES_OF_FIELD_C  retrieve_pieces_of_field_c_
#      define INIT_STORE_PIECE_OF_FIELD init_store_piece_of_field_
#      define INIT_RETRIEVE_PIECES_OF_FIELD init_retrieve_pieces_of_field_
#   endif
# endif
#endif

/*    CALL int_pack_data ( hdrbuf , hdrbufsiz * inttypesize , int_local_output_buffer, int_local_output_cursor ) */

INT_PACK_DATA ( unsigned char *buf , int *ninbytes , unsigned char *obuf, int *cursor )
{
  int i, lcurs ;
  lcurs = *cursor - 1 ;
  for ( i = 0 ; i < *ninbytes ; i++ )
  {
    obuf[lcurs++] = buf[i] ;
  }
  *cursor = lcurs+1 ;
}

int
INT_GEN_TI_HEADER_C ( char * hdrbuf, int * hdrbufsize,           /* hdrbufsize is in bytes */
                    int * itypesize, int * typesize,
                    int * DataHandle, char * Data,
                    int * Count, int * code )
{
  int i ;
  char * p ;
  p = hdrbuf ;
  p += sizeof(int) ;
  bcopy( code, p, sizeof(int) ) ; p += sizeof(int) ;       /* 2 */
  bcopy( DataHandle, p, sizeof(int) ) ; p += sizeof(int) ; /* 3 */
  bcopy( typesize, p, sizeof(int) ) ; p += sizeof(int) ;   /* 4 */
  bcopy( Count, p, sizeof(int) ) ; p += sizeof(int) ;      /* 5 */
  bcopy( Data, p, *Count * *typesize ) ; p += *Count * *typesize ; /* 6++ */
  *hdrbufsize = (int) (p - hdrbuf) ;
  bcopy( hdrbufsize, hdrbuf, sizeof(int) ) ;
  return(0) ;
}

int
INT_GET_TI_HEADER_C ( char * hdrbuf, int * hdrbufsize, int * n,  /* hdrbufsize and n are in bytes */
                    int * itypesize, int * typesize,
                    int * DataHandle, char * Data,
                    int * Count, int * code )
{
  int i ;
  char * p ;
  p = hdrbuf ;
  bcopy( p, hdrbufsize, sizeof(int) ) ;     p += sizeof(int) ;        /* 1 */
  bcopy( p, code, sizeof(int) ) ;           p += sizeof(int) ;        /* 2 */
  bcopy( p, DataHandle, sizeof(int) ) ;     p += sizeof(int) ;        /* 3 */
  bcopy( p, typesize, sizeof(int) ) ;       p += sizeof(int) ;        /* 4 */
  bcopy( p, Count, sizeof(int) ) ;          p += sizeof(int) ;        /* 5 */
  if ( *Count * *typesize > 0 ) {
  bcopy( p, Data, *Count * *typesize ) ;  p += *Count * *typesize ; /* 6++ */
  }
  *n = (int)( p - hdrbuf ) ;
  return(0) ;
}

#define MAX_FLDS 2000
static char fld_name[MAX_FLDS][256] ;
static char *fld_cache[MAX_FLDS] ;
static int fld_curs[MAX_FLDS] ;
static int fld_bufsize[MAX_FLDS] ;
static int fld     = 0 ;
static int numflds = 0 ;
static int frst = 1 ;

int INIT_STORE_PIECE_OF_FIELD ()
{
  int i ;
  if ( frst ) {
    for ( i = 0 ; i < MAX_FLDS ; i++ ) {
      fld_cache[i] = NULL ;
    }
    frst = 0 ; 
  }
  numflds = 0 ;
  for ( i = 0 ; i < MAX_FLDS ; i++ ) {
    strcpy( fld_name[i], "" ) ;
    if ( fld_cache[i] != NULL ) free( fld_cache[i] ) ;
    fld_cache[i] = NULL ;
    fld_curs[i] = 0 ;
    fld_bufsize[i] = 0 ;
  }
  return(0) ;
}

int INIT_RETRIEVE_PIECES_OF_FIELD ()
{
  fld = 0 ;
  return(0) ;
}

int
ADD_TO_BUFSIZE_FOR_FIELD_C ( int varname[], int * chunksize )
{
  int i, n ;
  int found ;
  char vname[256] ;

  n = varname[0] ;
  for ( i = 1; i <= n ; i++ ) { vname[i-1] = varname[i] ; }
  vname[n] = '\0' ;

  found = -1 ;
  for ( i = 0 ; i < numflds ; i++ ) { if ( !strcmp( fld_name[i], vname ) ) { found = i ; break ; } }
  if ( found == -1 ) {
    found = numflds++ ;
    strcpy( fld_name[found], vname ) ;
    fld_bufsize[found] = *chunksize ;
  }
  else
  {
    fld_bufsize[found] += *chunksize ;
  }
  if ( fld_cache[found] != NULL ) { free( fld_cache[found] ) ; }
  fld_cache[found] = NULL ;
}

int
STORE_PIECE_OF_FIELD_C ( char * buf , int varname[], int * chunksize, int *retval )
{
  int i, n ;
  int found ;
  char vname[256] ;

  n = varname[0] ;
  for ( i = 1; i <= n ; i++ ) { vname[i-1] = varname[i] ; }
  vname[n] = '\0' ;

  found = -1 ;
  for ( i = 0 ; i < numflds ; i++ ) { if ( !strcmp( fld_name[i], vname ) ) { found = i ; break ; } }
  if ( found == -1 ) { 
    fprintf(stderr,"frame/pack_utils.c: field (%s) not found; was not set up with add_to_bufsize_for_field\n",vname ) ;
    *retval = 1 ;
    return(0)  ;
  }

  if ( fld_cache[found] == NULL ) {
     fld_cache[found] = (char *) malloc( fld_bufsize[found] ) ;
     fld_curs[found] = 0 ;
  }

  if ( fld_curs[found] + *chunksize > fld_bufsize[found] ) {
    fprintf(stderr,
"frame/pack_utils.c: %s would overwrite %d + %d  > %d [%d]\n",vname, fld_curs[found], *chunksize, fld_bufsize[found], found ) ;
    *retval = 1 ;
    return(0)  ;
  }

  bcopy( buf, fld_cache[found]+fld_curs[found], *chunksize ) ;
  fld_curs[found] += *chunksize ;
  *retval = 0 ;
  return(0) ;
}

int
RETRIEVE_PIECES_OF_FIELD_C ( char * buf , int varname[], int * insize, int * outsize, int *retval )
{
  int i, n ;
  int found ;
  char vname[256] ;

  if ( fld < numflds ) {
    if ( fld_curs[fld] > *insize ) {
      fprintf(stderr,"retrieve: fld_curs[%d] (%d) > *insize (%d)\n",fld,fld_curs[fld], *insize ) ;
    }
    *outsize = ( fld_curs[fld] <= *insize ) ? fld_curs[fld] : *insize ;
    varname[0] = (int) strlen( fld_name[fld] ) ;
    for ( i = 1 ; i <= varname[0] ; i++ ) varname[i] = fld_name[fld][i-1] ;
    for ( i = 0 ; i < *outsize ; i++ )  buf[i] = fld_cache[fld][i] ;
    if ( fld_cache[fld] != NULL ) free ( fld_cache[fld] ) ;
    fld_cache[fld] = NULL ;
    fld_bufsize[fld] = 0 ;
    fld++ ;
    *retval = 0 ;
  }
  else {
    numflds = 0 ;
    *retval = -1 ;
  }
  return(0) ;
}

