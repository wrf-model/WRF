/***********************************************************************
     
                              COPYRIGHT
     
     The following is a notice of limited availability of the code and 
     Government license and disclaimer which must be included in the 
     prologue of the code and in all source listings of the code.
     
     Copyright notice
       (c) 1977  University of Chicago
     
     Permission is hereby granted to use, reproduce, prepare 
     derivative works, and to redistribute to others at no charge.  If 
     you distribute a copy or copies of the Software, or you modify a 
     copy or copies of the Software or any portion of it, thus forming 
     a work based on the Software and make and/or distribute copies of 
     such work, you must meet the following conditions:
     
          a) If you make a copy of the Software (modified or verbatim) 
             it must include the copyright notice and Government       
             license and disclaimer.
     
          b) You must cause the modified Software to carry prominent   
             notices stating that you changed specified portions of    
             the Software.
     
     This software was authored by:
     
     Argonne National Laboratory
     J. Michalakes: (630) 252-6646; email: michalak@mcs.anl.gov
     Mathematics and Computer Science Division
     Argonne National Laboratory, Argonne, IL  60439
     
     ARGONNE NATIONAL LABORATORY (ANL), WITH FACILITIES IN THE STATES 
     OF ILLINOIS AND IDAHO, IS OWNED BY THE UNITED STATES GOVERNMENT, 
     AND OPERATED BY THE UNIVERSITY OF CHICAGO UNDER PROVISION OF A 
     CONTRACT WITH THE DEPARTMENT OF ENERGY.
     
                      GOVERNMENT LICENSE AND DISCLAIMER
     
     This computer code material was prepared, in part, as an account 
     of work sponsored by an agency of the United States Government.
     The Government is granted for itself and others acting on its 
     behalf a paid-up, nonexclusive, irrevocable worldwide license in 
     this data to reproduce, prepare derivative works, distribute 
     copies to the public, perform publicly and display publicly, and 
     to permit others to do so.  NEITHER THE UNITED STATES GOVERNMENT 
     NOR ANY AGENCY THEREOF, NOR THE UNIVERSITY OF CHICAGO, NOR ANY OF 
     THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR 
     ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, 
     COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, 
     PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
     NOT INFRINGE PRIVATELY OWNED RIGHTS.

***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "rsl.h"

/*@
  RSL_CREATE_MESSAGE - create a descriptor for an RSL message.

  Synopsis:
  RSL_CREATE_MESSAGE ( m )
  integer m

  Input parameter:
. m - message descriptor

  Notes:
  Create a descriptor for an RSL message.  On return, the
  integer Arg1 will contain a handle to a new RSL message.
  Messages exist only temporarily by themselves -- once a message
  has been asssociated with a stencil or a state vector, the handle
  becomes invalid.

  See also:
  RSL_BUILD_MESSAGE, RSL_DESCRIBE_STENCIL, RSL_DESCRIBE_STATE

@*/


RSL_CREATE_MESSAGE ( mh_p )
  int_p mh_p ;
{

  int i ;
  message_desc_t *msg ;

  /* NOTE: never return the 0th message descriptor */
  for ( i = 1 ; i < RSL_MAXDESCRIPTORS ; i++ )
    if ( mh_descriptors[i] == NULL ) break ;    /* got one */

  RSL_TEST_ERR( i == RSL_MAXDESCRIPTORS,
    "rsl_create_message: out of descriptors.\nAre you creating messages and then not associating them with\na stencil, bcast/merge, or state vector?" ) ;

  *mh_p = i ;
  msg = RSL_MALLOC(message_desc_t,1) ;
  msg->tag = MESSAGE_DESC ;
  msg->mh = *mh_p ;
  mh_descriptors[*mh_p] = msg ;
}

/* Only ever called internally to RSL.   This routine is for use
   by routines that associate messages with other RSL constructs.  Once
   the association is made, those constructs point to the messages.  We
   do *not* free the message description structures here, only free up
   the descriptors so they can be used again.  */

release_mh_descriptor (mh_p)
  int_p mh_p ;
{
  int mh ;

  mh = *mh_p ;

  if ( mh == RSL_INVALID ) return ;

  RSL_TEST_ERR( mh <= 0 || mh >= RSL_MAXDESCRIPTORS,
                "internal error.  Invalid message descriptor.") ;
  if ( mh_descriptors[mh] != NULL )
  {
    mh_descriptors[mh] = NULL ;
  }
  /* it can happen that this will be called for a descriptor that
     has already been nulled out.  Let it happen. */
}


/*
  RSL_BLANK_MESSAGE - (obsolete) describe a message for broadcast/merges.

  Synopsis:
  RSL_BLANK_MESSAGE ( m, len )
  integer m
  integer len

  Input parameter:
. m - message descriptor
. len - length, in bytes, of message

  Notes:
  Messages to be used in a broadcast or a merge operation
  for inter-domain communication do not have fields associated
  with them ahead of time, unlike messages that are used
  in stencil exchanges (see RSL_BUILD_MESSAGE).  Rather, the
  packing and unpacking routines are provided by the user,
  and the message is simply a stream of bytes.   RSL_BLANK_MESSAGE
  designates a message as a blank message and associates with
  the message its length in bytes.

  See also:
  RSL_BUILD_MESSAGE, RSL_COMP_BCAST, RSL_COMP_MERGE
*/


RSL_BLANK_MESSAGE ( mh_p, len_p )
  int_p mh_p ;          /* message handle */
  int_p len_p ;         /* length in bytes */
{
  int mh, len ;
  message_desc_t *msg ;

  mh = *mh_p ; len = *len_p ;

  RSL_TEST_ERR((mh <= 0)|| (mh >= RSL_MAXDESCRIPTORS),
                "rsl_blank_message: bad message handle" ) ;
  RSL_TEST_ERR((msg = (message_desc_t *)mh_descriptors[mh])==NULL,
               "descriptor for null message");
  RSL_TEST_ERR( msg->tag != MESSAGE_DESC,
       "rsl_blank_message: handle given is not for an rsl message def" ) ;

  msg->tag = BLANK_MESSAGE_DESC ;       /* change tag */
  msg->nbytes = len ;
  return ;
}

/*@
  RSL_BUILD_MESSAGE - Add a 2- or 3-dimensional field to a message.

  Notes:
  An RSL message is a set of fields that are to be communicated
  for a point in the model grid.  Describing messages in this way
  allows RSL to assume control of packing and unpacking of model data
  from and to local processor memory for efficient and transparent
  communication of stencil exchanges and state remappings.
    
  A field (Arg3), a multi-dimensional
  array of a given type (Arg2),
  is
  added
  to a message (Arg1) by repeated calls to RSL_BUILD_MESSAGE.
  Once constructed, messages may then be combined into stencils
  (RSL_DESCRIBE_STENCIL) for stencil exchanges (RSL_EXCH_STENCIL).
  A message may also be used to describe a state-vector (RSL_DESCRIBE_STATE)
  used in remapping for load balancing.

  The Arg4 argument gives the number of dimensions in the field
  being added to the message.  It is permissable to mix 2 and 3
  dimensional fields.  Regardless, however, 2 and only dimensions
  of the field
  must be decomposed.
  The three arguments Arg4, Arg5, and Arg6, are integer arrays
  of size Arg4.  The indicies of these arrays correspond to the
  dimensions of the field being added; index 1 is the most minor,
  and index ndim is the most major dimension.
  
  The values stored in the Arg4 array may be
  
  Verbatim:
$     RSL_NORTHSOUTH  -- decomposed over M
$     RSL_EASTWEST -- decomposed over N
$     RSL_NOTDECOMPOSED.  -- not decomposed.
BREAKTHEEXAMPLECODE

  This tells RSL whether the
  dimension in question is decomposed over a north/south column of
  processors in the mesh, an east-west row in the mesh, or -- as is the
  case with the vertical dimension in 3-d arrays -- not decomposed at
  all.  RSL_NORTHSOUTH, RSL_EASTWEST, and RSL_NOTDECOMPOSED are 
  defined in the RSL include file "rsl.inc".

  The values stored in the Arg5 array are the global, or
  undecomposed, sizes of each dimension of the field.  The values 
  stored in the Arg6 array are the local, or actual,
  sizes of the dimensions of the field as it exists in the processor's
  memory.  If the field is statically declared (say, in common) the
  sizes would be the sizes that were used to declare the array itself.
  If the array is dynamically allocated using a Fortran90 
  ALLOCATE() statement, the values of llen would be sizes that
  were specified to the ALLOCATE() statement.  In the latter case,
  if these sizes ever change during the course of a run, it would
  be necessary to destroy this message and reconstruct a new one
  for RSL, since it must always be able to determine the true size
  in memory of the data structures involved in messaging operations.

  Example:
$  integer m                           ! message descriptor
$  integer decomp(3), llen(3), glen(3) ! dimension descriptions
$  real ua(ix,jx,kx), va(ix,jx,kx)     ! locally dimensioned 3-d arrays
$  real psa(ix,jx,kx)                  ! locally dimensioned 2-d array

$  decomp(1) = RSL_NORTHSOUTH     ! how most minor dim decomposed
$  decomp(2) = RSL_EASTWEST       ! how next dim decomposed
$  decomp(3) = RSL_NOTDECOMPOSED  ! major dim (vertical) not decomposed

$  glen(1) = g_ix    ! global size in n/s
$  glen(2) = g_jx    ! global size in e/w
$  glen(3) = kx      ! size in vertical
$  llen(1) = ix      ! local size in n/s
$  llen(2) = jx      ! local size in e/w
$  llen(3) = kx      ! local size of vertical (same as global)

$  call rsl_create_message( m )
$  call rsl_build_message( m, RSL_REAL, ua,  3, decomp, glen, llen )
$  call rsl_build_message( m, RSL_REAL, va,  3, decomp, glen, llen )
$  call rsl_build_message( m, RSL_REAL, psa, 2, decomp, glen, llen )


BREAKTHEEXAMPLECODE

  In the above example, a message is created and then built by 
  adding two three-dimensional fields and one two-dimensional field.  
  The order of the construction is not important.  Subsequent
  to these statements, the completed message could be used to define
  one or more points of a stencil exchange to communicate ua,
  psa, and va between processors.


  See also:
  RSL_CREATE_MESSAGE, RSL_BLANK_MESSAGE, RSL_DESCRIBE_STENCIL,
  RSL_DESCRIBE_STATE

@*/

static struct f90_base_table_entry {
  char * base, * virt_base ;
  int size_in_bytes ;
} f90_base_table[ MAX_BASE_TABLE_ENTRIES ] ;
static int base_table_cursor = 1;
static int base_table_size = 1;

RSL_BUILD_MESSAGE ( mh_p, t_p, base, ndim_p, decomp, glen, llen )
  int_p 
     mh_p       /* (I) Message handle created by RSL_CREATE_MESSAGE. */
    ,t_p        /* (I) RSL type description. */
    ,ndim_p ;   /* (I) Number of dimensions of field being added to message.*/
  void *
    base ;      /* (I) Base address field in local memory. */
  int 
    decomp[] ;  /* (I) How decomposed. */
  int 
    glen[] ;    /* (I) Global (undecomposed) dimensions of field. */
  int 
    llen[] ;    /* (I) Local (decomposed) dimensions of field. */
{
  int mh, t, ndim, i ;
  message_desc_t *msg ;
  rsl_fldspec_t *fld ;
  int dim ;
  int f90_table_index ;
  char errmess[256] ;

  mh = *mh_p ; t = *t_p ; ndim = *ndim_p ;

  RSL_TEST_ERR( ndim < 0, "rsl_build_message: bad ndim argument" ) ;
  RSL_TEST_ERR( ndim > RSL_MAXDIM, 
"rsl_build_message: ndim too large.  Change RSL_MAXDIM; recompile librsl.a." ) ;
  RSL_TEST_ERR((mh <= 0)||(mh >= RSL_MAXDESCRIPTORS),
       "rsl_build_message: bad message handle" ) ;
  if ( (msg = (message_desc_t *)mh_descriptors[mh])==NULL )
  {
      RSL_TEST_ERR(1, "descriptor for null message");
  }
  RSL_TEST_ERR( msg->tag != MESSAGE_DESC,
       "rsl_build_message: handle given is not for an rsl message def" ) ;

  fld = RSL_MALLOC( rsl_fldspec_t, 1 ) ;

  fld->type = t ;
  fld->elemsz = elemsize( t ) ;
  for ( fld->memsize = fld->elemsz, i = 0 ; i < ndim ; i++ )
  {
    fld->memsize = fld->memsize * llen[i] ;
  }

  if ( t >= 100 )
  {
    if ( ! (fld->f90_table_index = get_index_for_base( base )) )
    { RSL_TEST_ERR(1,"Use of unregistered f90 typed variable") ; }
#if 0
    fld->base = (void *)((fld->f90_table_index-1) * F90_MAX_FLD_SIZE_IN_BYTES + 1) ; /* don't allow base of 0 */
#else
    fld->base = f90_base_table[ fld->f90_table_index ].virt_base ;
#endif
  }
  else
  {
    fld->base = base ;
  }
  fld->ndim = ndim ;
  for ( dim = 0 ; dim < ndim ; dim++ )
  {
    fld->decomp[dim] = decomp[dim] ;
    fld->gdex[dim] = RSL_INVALID ;	/* this gets filled in dynamically */
    if ( decomp[dim] == RSL_NOTDECOMPOSED && glen[dim] != llen[dim] )
    {
       sprintf(errmess,
"rsl_build_message: mesg %d: dim %d is RSL_NOTDECOMPOSED so glen(%d)=%d must eq llen(%d)=%d",
       mh, dim+1, dim+1, glen[dim], dim+1, llen[dim] ) ;
       RSL_TEST_WRN( 1, errmess ) ;
    }
    fld->glen[dim] = glen[dim] ;
    fld->llen[dim] = llen[dim] ;
    if ( decomp[dim] > 10 )
      fld->stag[dim] = 1 ;
    else 
      fld->stag[dim] = 0 ;
  }

  /* work out pack/unpack strategy for this field */
  switch ( ndim )
  {
  case 2 :
    if      ( decomp[0]%10 == RSL_M && 
	      decomp[1]%10 == RSL_N )
      fld->strategy = MINNS_MAJEW_2D ;
    else if ( decomp[1]%10 == RSL_M &&
	      decomp[0]%10 == RSL_N )
      fld->strategy = MINEW_MAJNS_2D ;
    else
      RSL_TEST_ERR(1,"rsl_build_message: unsupported decomposition strategy for 2d message") ;
    break ;
  case 3 :
    if      ( decomp[0]%10 == RSL_M &&
	      decomp[1]%10 == RSL_N &&
	      decomp[2]%10 == RSL_NOTDECOMPOSED)
      fld->strategy = MINNS_MAJEW_K_3D ;
    else if ( decomp[0]%10 == RSL_N &&
	      decomp[1]%10 == RSL_M &&
	      decomp[2]%10 == RSL_NOTDECOMPOSED)
      fld->strategy = MINEW_MAJNS_K_3D ;
    else if ( decomp[0]%10 == RSL_NOTDECOMPOSED &&
	      decomp[1]%10 == RSL_M &&
	      decomp[2]%10 == RSL_N )
      fld->strategy = K_MIDNS_MAJEW_3D ;
    else if ( decomp[0]%10 == RSL_M &&
	      decomp[1]%10 == RSL_NOTDECOMPOSED &&
	      decomp[2]%10 == RSL_N )
      fld->strategy = MINNS_K_MAJEW_3D ;
    else
      RSL_TEST_ERR(1,"rsl_build_message: unsupported decomposition strategy for 3d message") ;
    break ;
  default :
    sprintf(mess,"rsl_build_message: %d dimension flds not supported yet\n",
	    ndim ) ;
    RSL_TEST_ERR(1,mess) ;
    break ;
  }

  /* insert fldspec at beginning of list (note: we're not concerning
     ourselves with order of the fields -- this will reverse them from
     the order they were specified in.) */

  fld->next = msg->fldspecs ;
  msg->fldspecs = fld ;
  msg->nflds++ ;
}


RSL_REGISTER_F90 ( base )
  char * base ;
{
  if ( base_table_cursor < MAX_BASE_TABLE_ENTRIES )
  {
    f90_base_table[ base_table_cursor ].base = base ;
    base_table_cursor++ ;
  }
  else
  {
    RSL_TEST_ERR(1,"Exceeded MAX_BASE_TABLE_ENTRIES number of f90 fields") ;
  }
}

#define BASE_TABLE_PADDING sizeof(double) ;
RSL_REGISTER_F90_BASE_AND_SIZE ( base , size )
  char * base ;
  int * size ;
{
  if ( base_table_cursor < MAX_BASE_TABLE_ENTRIES )
  {
    f90_base_table[ base_table_cursor ].base = base ;
    f90_base_table[ base_table_cursor ].size_in_bytes = * size ;
    f90_base_table[ base_table_cursor ].virt_base =
           f90_base_table[ base_table_cursor-1 ].virt_base +
           f90_base_table[ base_table_cursor-1 ].size_in_bytes + BASE_TABLE_PADDING ;
    base_table_cursor++ ;
  }
  else
  {
    RSL_TEST_ERR(1,"Exceeded MAX_BASE_TABLE_ENTRIES number of f90 fields") ;
  }
}

RSL_END_REGISTER_F90 ()
{
  base_table_size = base_table_cursor ;
}

RSL_START_REGISTER_F90 ()
{
  base_table_cursor = 1 ;
  f90_base_table[ 0 ].virt_base = (char *) BASE_TABLE_PADDING ;
  f90_base_table[ 0 ].size_in_bytes = 0 ;
}

void *
get_base_for_index ( dex )
  int dex ;
{
  if ( dex < 1 || dex >= base_table_size )
  {
    sprintf(mess, "bad index %d into f90_base_table.  base_table_size %d\n", dex, base_table_size ) ;
    RSL_TEST_ERR( 1, mess ) ;
  }
  return( (void *) f90_base_table[dex].base ) ;
}

get_index_for_base ( base )
  char * base ;
{
  int i ;
  for ( i = 1 ; i < base_table_size ; i++ )
  { 
    if ( base == f90_base_table[ i ].base )
    {
      return( i ) ;
    }
  }
  return(0) ;
}

/* return the number of bytes this message will require 
   (only the data requirements -- descriptors not figured here */
int
message_size( msg )
  message_desc_t *msg ;
{
  int dim, dimlen ;
  int accum ;
  rsl_fldspec_t * fld ;

  if ( msg == NULL ) return(0) ;
  if ( msg->tag != MESSAGE_DESC ) return(-1) ;
  accum = 0 ;
  for ( fld = msg->fldspecs ; fld != NULL ; fld = fld->next )
  {
    accum += fldsize( fld ) ;
  }
  return(accum) ;
}

int
fldsize( fld )
  rsl_fldspec_t * fld ;
{
  int dim, dimlen ;
  int accum, fldaccum ;
  fldaccum = 1 ;
  for ( dim = 0 ; dim < fld->ndim ; dim++ )
  {
    fldaccum *= (fld->decomp[dim] == RSL_NOTDECOMPOSED)?fld->llen[dim]:1;
  }
  return (fldaccum * fld->elemsz) ;
}

/* only used internally within the RSL package... called by routines
   that destroy larger RSL constructs such as stencils, bcast/merges,
   state vectors (note: this routine was written using emacs, so
   be careful) */
destroy_message( msg )
  message_desc_t * msg ;
{
  rsl_fldspec_t *fld, *doomed ;
  if ( msg == NULL ) return ;
  RSL_TEST_ERR( msg->tag != MESSAGE_DESC, "destroy_message: arg not a msg.") ;

  for ( fld = msg->fldspecs ; fld != NULL ; )
  {
    doomed = fld ;
    fld = fld->next ;
    RSL_FREE( doomed ) ;
  }
  RSL_FREE( msg ) ;
  msg = NULL ;
}

int
elemsize( t )
  int t ;
{
  if ( t >= 100 ) t = t % 100 ; /* remove extra info */
  switch ( t )
  {
#ifdef T3D
  case RSL_REAL         : return(sizeof(double)) ;
#else
  case RSL_REAL         : return(sizeof(float)) ;
#endif
  case RSL_DOUBLE       : return(sizeof(double)) ;
#ifdef T3D
  case RSL_COMPLEX      : return(2*sizeof(double)) ;
#else
  case RSL_COMPLEX      : return(2*sizeof(float)) ;
#endif
  case RSL_INTEGER      : return(sizeof(int)) ;
  case RSL_CHARACTER    : return(sizeof(char)) ;
  }
  return(-1) ;
}

