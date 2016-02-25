#ifndef MS_SUA
# include <stdio.h>
# include <stdlib.h>
#endif
#include <string.h>
#include "../inc/streams.h"

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
#      define PERTURB_REAL perturb_real
#      define INSPECT_HEADER inspect_header
#      define RESET_MASK reset_mask
#      define SET_MASK set_mask
#      define GET_MASK get_mask
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
#      define PERTURB_REAL perturb_real__
#      define INSPECT_HEADER inspect_header__
#      define RESET_MASK reset_mask__
#      define SET_MASK set_mask__
#      define GET_MASK get_mask__
#   else
#      define INT_PACK_DATA  int_pack_data_
#      define INT_GET_TI_HEADER_C  int_get_ti_header_c_
#      define INT_GEN_TI_HEADER_C  int_gen_ti_header_c_
#      define ADD_TO_BUFSIZE_FOR_FIELD_C  add_to_bufsize_for_field_c_
#      define STORE_PIECE_OF_FIELD_C  store_piece_of_field_c_
#      define RETRIEVE_PIECES_OF_FIELD_C  retrieve_pieces_of_field_c_
#      define INIT_STORE_PIECE_OF_FIELD init_store_piece_of_field_
#      define INIT_RETRIEVE_PIECES_OF_FIELD init_retrieve_pieces_of_field_
#      define PERTURB_REAL perturb_real_
#      define INSPECT_HEADER inspect_header_
#      define RESET_MASK reset_mask_
#      define SET_MASK set_mask_
#      define GET_MASK get_mask_
#   endif
# endif
#endif

#ifdef MEMCPY_FOR_BCOPY
# define bcopy(A,B,C) memcpy((B),(A),(C))
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
  return(0) ;
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
#ifndef MS_SUA
    fprintf(stderr,"frame/pack_utils.c: field (%s) not found; was not set up with add_to_bufsize_for_field\n",vname ) ;
#endif
    *retval = 1 ;
    return(0)  ;
  }

  if ( fld_cache[found] == NULL ) {
     fld_cache[found] = (char *) malloc( fld_bufsize[found] ) ;
     fld_curs[found] = 0 ;
  }

  if ( fld_curs[found] + *chunksize > fld_bufsize[found] ) {
#ifndef MS_SUA
    fprintf(stderr,
"frame/pack_utils.c: %s would overwrite %d + %d  > %d [%d]\n",vname, fld_curs[found], *chunksize, fld_bufsize[found], found ) ;
#endif
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
#ifndef MS_SUA
    if ( fld_curs[fld] > *insize ) {
      fprintf(stderr,"retrieve: fld_curs[%d] (%d) > *insize (%d)\n",fld,fld_curs[fld], *insize ) ;
    }
#endif
    *outsize = ( fld_curs[fld] <= *insize ) ? fld_curs[fld] : *insize ;
    bcopy( fld_cache[fld], buf, *outsize ) ;
    varname[0] = (int) strlen( fld_name[fld] ) ;
    for ( i = 1 ; i <= varname[0] ; i++ ) varname[i] = fld_name[fld][i-1] ;
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

#define INDEX_2(A,B,NB)       ( (B) + (A)*(NB) )
#define INDEX_3(A,B,C)  INDEX_2( (A), INDEX_2( (B), (C), (me[1]-ms[1]+1) ), (me[1]-ms[1]+1)*(me[0]-ms[0]+1) )
/* flip low order bit of fp number */
int
PERTURB_REAL ( float * field, int ds[], int de[], int ms[], int me[], int ps[], int pe[] )
{
   int i,j,k ;
   int le ; /* index of little end */
   float x = 2.0 ;
   unsigned int y ; 
   unsigned char a[4], *p ;
   if ( sizeof(float) != 4 ) return(-1) ;
   /* check endianness of machine */
   bcopy ( &x, a, 4 ) ;
   le = 0 ;
   if ( a[0] == 0x40 ) le = 3 ;
   for ( k = ps[2]-ms[2] ; k <= pe[2]-ms[2] ; k++ )
     for ( j = ps[1]-ms[1] ; j <= pe[1]-ms[1] ; j++ )
       for ( i = ps[0]-ms[0] ; i <= pe[0]-ms[0] ; i++ )
       {
          /* do not change zeros */
          if ( field[ INDEX_3(k,j,i) ] != 0.0 ) {
             p = (unsigned char *)&(field[ INDEX_3(k,j,i) ] ) ; 
             if ( *(p+le) & 1 ) { *(p+le) &= 0x7e ; }
             else               { *(p+le) |= 1 ; }
          }
       }
   return(0) ;
}

int INSPECT_HEADER ( char * buf, int * sz, int * line )
{
    int i ;
#ifndef MS_SUA
    fprintf(stderr,"INSPECT_HEADER: line = %d ", *line ) ;
    if ( buf != NULL && sz != NULL ) {
      for ( i = 0 ; i < *sz && i < 256 ; i++ )  { if ( (buf[i] >= 'a' && buf[i] <= 'z') || buf[i] == '_' ||
                                             (buf[i] >= 'A' && buf[i] <= 'Z') ||
                                             (buf[i] >= '0' && buf[i] <= '9') ) fprintf(stderr,"%c",buf[i]) ;
                                    }
      fprintf(stderr,"\n") ;
   }
#endif
    return(0) ;
}

/* note that these work the same as the routines in tools/misc.c, but are Fortran callable.  
   They must be kept in sync, functionally. */

void
RESET_MASK ( unsigned int * mask , int *e )
{
   int w ;
   unsigned int m, n ;

   w = *e / (8*sizeof(int)-1) ;
   n = 1 ;
   m = ~( n << *e % (8*sizeof(int)-1) ) ;
   if ( w >= 0 && w < IO_MASK_SIZE ) {
     mask[w] &= m ;
   }
}

void
SET_MASK ( unsigned int * mask , int *e )
{
   int w ;
   unsigned int m, n ;

   w = *e / (8*sizeof(int)-1) ;
   n = 1 ;
   m = ( n << *e % (8*sizeof(int)-1) ) ;
   if ( w >= 0 && w < IO_MASK_SIZE ) {
     mask[w] |= m ;
   }
}

/* this is slightly different from in tools dir since it returns result as argument, not function */
/* definition of IO_MASK_SIZE comes from build and must be uniform with frame/module_domain_type.F and
   version of this function in tools dir */
void
GET_MASK ( unsigned int * mask , int *e , int * retval )
{
   int w ;
   unsigned int m, n ;

   w = *e / (8*sizeof(int)-1) ;   /* 8 is number of bits per byte */
   if ( w >= 0 && w < IO_MASK_SIZE ) {
     m = mask[w] ;
     n =  ( 1 << *e % (8*sizeof(int)-1) ) ;;
     *retval = ( (m & n) != 0 ) ;
   } else {
     *retval = 0 ;
   }
}

#ifdef WRAP_MALLOC
# ifndef WRAP_MALLOC_ALIGNMENT
#  define WRAP_MALLOC_ALIGNMENT 128
# endif
# define _XOPEN_SOURCE 600
# include <stdlib.h>
void *malloc(size_t size)
{
       void *tmp;
       if (posix_memalign(&tmp, WRAP_MALLOC_ALIGNMENT, size) == 0)
               return tmp;
       else {
               errno = ENOMEM;
               return NULL;
       }
}
#endif

#ifndef DM_PARALLEL
# ifndef CRAY
#  ifdef NOUNDERSCORE
#       define RSL_INTERNAL_MICROCLOCK  rsl_internal_microclock
#  else
#    ifdef F2CSTYLE
#       define RSL_INTERNAL_MICROCLOCK  rsl_internal_microclock__
#    else
#       define RSL_INTERNAL_MICROCLOCK  rsl_internal_microclock_
#    endif
#  endif
# endif
# if !defined(MS_SUA) && !defined(_WIN32)
#  include <sys/time.h>
RSL_INTERNAL_MICROCLOCK ()
{
    struct timeval tb ;
    struct timezone tzp ;
    int isec ;  /* seconds */
    int usec ;  /* microseconds */
    int msecs ;
    gettimeofday( &tb, &tzp ) ;
    isec = tb.tv_sec ;
    usec = tb.tv_usec ;
    msecs = 1000000 * isec + usec ;
    return(msecs) ;
}
# endif
#endif

