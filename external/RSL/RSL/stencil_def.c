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
  RSL_CREATE_STENCIL -- Create a stencil descriptor.

  Notes:
  The output argument is the integer Arg1, a descriptor to 
  a new RSL stencil.  The stencil is then built (RSL_DESCRIBE_STENCIL)
  and used in stencil exchanges (RSL_EXCH_STENCIL) during the model
  run.

  See also:
  RSL_DESCRIBE_STENCIL, RSL_EXCH_STENCIL
@*/

RSL_CREATE_STENCIL ( sh_p )
  int_p sh_p ;     /* (O) New RSL stencil descriptor. */
{
  int i ;
  stencil_desc_t *sten ;
  
  /* NOTE: never return the 0th stencil */
  for ( i = 1 ; i < RSL_MAXDESCRIPTORS ; i++ )
    if ( sh_descriptors[i] == NULL ) break ;    /* got one */

  RSL_TEST_ERR( i == RSL_MAXDESCRIPTORS,
  "rsl_create_stencil:  out of descriptors.");

  *sh_p = i ;
  sten = RSL_MALLOC(stencil_desc_t,1) ;
  sten->tag = STENCIL_DESC ;
  sten->has_f90_fields = 0 ;
  sh_descriptors[*sh_p] = sten ;
  sten->sh = *sh_p ;
}

release_sh_descriptor (sh_p)
  int_p sh_p ;
{
  int sh ;

  sh = *sh_p ;
  RSL_TEST_ERR( sh < 0 || sh >= RSL_MAXDESCRIPTORS,
                "internal error.  Invalid stencil descriptor.") ;
  if ( sh_descriptors[sh] != NULL )
  {
    sh_descriptors[sh] = NULL ;
  }
}

/*@
  RSL_DESCRIBE_STENCIL -- Defines an RSL stencil exchange on a domain.

  Notes:
  This routine gives a stencil a size and shape, associates RSL messages
  with the stencil points, and asssociates the stencil with the domain
  Arg1.  The argument Arg2 is a stencil descriptor previously created
  by RSL_CREATE_STENCIL.  The shape of the stencil is specified by
  Arg3, which is one of RSL_4PT (N, W, E, S), RSL_8PT (NW, N, NE, W, E, SW,
  S, SE), RSL_24PT (a 5 by 5 stencil of around a given point), RSL_48PT
  (a 7 by 7 stencil), and RSL_168PT (a 13 by 13 stencil).

  Messages for each stencil point must have been previously created and
  built using RSL_CREATE_MESSAGE and RSL_BUILD_MESSAGE.  The message
  descriptors corresponding to 
  each point in the stencil are passed to RSL_DESCRIBE_STENCIL as elements
  of the
  the 1-dimensional integer array Arg4.  The size of Arg4 corresponds to
  the number of points (not counting the center) of the stencil.  They
  are ordered from west to east and then north to south.  A message
  may be associated with more than one stencil point.
  Unused
  points in a stencil can be set to RSL_INVALID to indicate that no messages
  are associated with those points.  
  Once
  RSL_DESCRIBE_STENCIL returns, all
  of the messages are invalidated and the message descriptors become
  undefined.

  Example:

$  integer m             ! message descriptor
$  integer messages(8)   ! array of messages for 8 pt stencil
$  integer sten          ! stencil descriptor

$ C Size and decomposition information for building messages
$  decomp(1) = RSL_NORTHSOUTH	     ! how most minor dim decomposed
$  decomp(2) = RSL_EASTWEST	       ! how next dim decomposed
$  decomp(3) = RSL_NOTDECOMPOSED	  ! major dim (vertical) not decomposed
$  glen(1) = g_ix	! global size in n/s
$  glen(2) = g_jx	! global size in e/w
$  glen(3) = kx		 ! size in vertical
$  llen(1) = ix	  ! local size in n/s
$  llen(2) = jx 		! local size in e/w
$  llen(3) = kx	  ! local size of vertical (same as global)
$ C Create a message and add fields UA, VA, and PSA
$  call rsl_create_message( m )
$  call rsl_build_message( m, RSL_REAL, ua,  3, decomp, glen, llen )
$  call rsl_build_message( m, RSL_REAL, va,  3, decomp, glen, llen )
$  call rsl_build_message( m, RSL_REAL, psa, 2, decomp, glen, llen )
$ C Construct stencil for W,SW,S exchange
$  messages(1) =    RSL_INVALID
$  messages(2) =                RSL_INVALID
$  messages(3) =                            RSL_INVALID
$  messages(4) =    m
$  messages(5) =                            RSL_INVALID
$  messages(6) =    m
$  messages(7) =                m
$  messages(8) =                            RSL_INVALID
$ C Create and describe stencil
$  call rsl_create_stencil( sten )
$  call rsl_describe_stencil( d, sten, RSL_8PT, messages )

BREAKTHEEXAMPLECODE
  In this example, an  exchange of surface pressure PSA
  and horizontal wind velocity
  components UA and VA on a south-west stencil is created.  Unused
  points in the eight-point stencil are marked with RSL_INVALID.  Because
  the same fields are communicated on each of the three active stencil
  points (W, SW, and S), the same message, M, is associated with each.
  Adding spaces to the statements that assign the array messages enhance
  readability by outlining visually the shape of the communication.

  See also:
  RSL_CREATE_STENCIL, RSL_EXCH_STENCIL, RSL_CREATE_MESSAGE, RSL_BUILD_MESSAGE

@*/

RSL_DESCRIBE_STENCIL ( d_p, sh_p, maskid_p, messages )
  int_p d_p,            /* (I) Domain descriptor. */
        sh_p,           /* (I) Stencil handle */
        maskid_p ;      /* (I) Stencil shape and size. */
  int messages[] ;      /* (I) Array of message descriptors. */
{
  int d, sh, mh, maskid ;
  rsl_domain_info_t * dinfo ;
  stencil_desc_t *sten ;
  message_desc_t *msg ;
  int pt ;

  d = *d_p ; sh = *sh_p ; maskid = *maskid_p ;

  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
    "rsl_describe_stencil: bad domain descriptor\n") ;
  dinfo = &(domain_info[d]) ;
  RSL_TEST_ERR(dinfo->valid != RSL_VALID,
    "rsl_describe_stencil: descriptor is not for a valid domain\n") ;

  RSL_TEST_ERR( sh < 0 || sh >= RSL_MAXDESCRIPTORS,
    "rsl_describe_stencil: bad stencil handle" ) ;
  sten = (stencil_desc_t *) sh_descriptors[sh] ;
  RSL_TEST_ERR( sten->tag != STENCIL_DESC,
       "rsl_describe_stencil: handle given is not for an rsl stencil def" ) ;

  switch ( maskid )
  {
  case RSL_4PT  : sten->f[d].ptfcn = rsl_4pt ;  sten->npts[d] = 4  ; break ;
  case RSL_8PT  : sten->f[d].ptfcn = rsl_8pt ;  sten->npts[d] = 8  ; break ;
  case RSL_12PT : sten->f[d].ptfcn = rsl_12pt ; sten->npts[d] = 12 ; break ;
  case RSL_24PT : sten->f[d].ptfcn = rsl_24pt ; sten->npts[d] = 24 ; break ;
  case RSL_48PT : sten->f[d].ptfcn = rsl_48pt ; sten->npts[d] = 48 ; break ;
  case RSL_80PT : sten->f[d].ptfcn = rsl_80pt ; sten->npts[d] = 80 ; break ;
  case RSL_120PT : sten->f[d].ptfcn = rsl_120pt ; sten->npts[d] = 120 ; break ;
#if ( ALLOW_RSL_168PT == 1 )
  case RSL_168PT : sten->f[d].ptfcn = rsl_168pt ; sten->npts[d] = 168 ; break ;
  default : RSL_TEST_ERR( 1, 
"rsl_describe_stencil: invalid maskid,\n  must be RSL_4PT, RSL_8PT, RSL_12PT, RSL_24PT, RSL_48PT, or RSL_168PT" ) ;
#else
  default : RSL_TEST_ERR( 1, 
"rsl_describe_stencil: invalid maskid,\n  must be RSL_4PT, RSL_8PT, RSL_12PT or RSL_24PT, RSL_48PT" ) ;
#endif
    return ;
  }

  sten->maskid[d] = maskid ;
  sten->compiled[d] = 0 ;

  for ( pt = 0 ; pt < sten->npts[d] ; pt++ )
  {
    mh = messages[ pt ] ;
    RSL_TEST_ERR( mh != RSL_INVALID && (mh < 0 || mh >=RSL_MAXDESCRIPTORS),
"rsl_describe_stencil: bad message handle in list,\n  must be either valid message or RSL_INVALID") ;
    if ( mh != RSL_INVALID )
    {
      msg = (message_desc_t *) mh_descriptors[ mh ] ;
      RSL_TEST_ERR( msg->tag != MESSAGE_DESC, 
       "rsl_describe_stencil: handle given in message list is not for an rsl mesage def" ) ;
      sten->msgs[d][pt] = msg ;
    }
    else
    {
      sten->msgs[d][pt] = NULL ;
    }
  }
  /* free up the message descriptors; they've done their job */
  for ( pt = 0 ; pt < sten->npts[d] ; pt++ )
  {
    release_mh_descriptor( &(messages[pt]) ) ;
  }
  /* add my descriptor to the list for the domain */
  dinfo->stenlist[dinfo->stencurs] = sh ;
  dinfo->stencurs++ ;  /* 970317 */
  if ( dinfo->stencurs >= RSL_MAXDESCRIPTORS )
  {
    sprintf(mess,
     "Domain %d doesn't have room for any more stencils, but the allowable\nlimit of %d should have been more than enough.\nYou might recompile RSL with a higher setting for RSL_MAXDESCRIPTORS, but\n it's likely something else is wrong.",
       d, RSL_MAXDESCRIPTORS ) ;
    RSL_TEST_ERR( 1, mess ) ;
     
  }
}

/* only used internally within the RSL package */
destroy_stencil( sten )
  stencil_desc_t * sten ;
{
  int d ;
  rsl_fldspec_t *fld, *doomed ;
  if ( sten == NULL ) return ;
  RSL_TEST_ERR( sten->tag != STENCIL_DESC, "destroy_stencil: arg not a stencil.") ;

  for ( d = 0 ; d < RSL_MAXDOMAINS ; d++ )
  {
    destroy_stencil_on_domain( d, sten ) ;
  }
  release_sh_descriptor (sten->sh) ;
  RSL_FREE( sten ) ;
}

destroy_stencil_on_domain( d, sten )
  int d ;
  stencil_desc_t * sten ;
{
  int i ;

  if ( sten == NULL ) return ;
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "destroy_stencil_on_domain: bad domain descriptor") ;
  for ( i = 0 ; i < RSL_MAXSTEN+1 ; i++ )
  {
    destroy_message( sten->msgs[d][i] ) ;
  }
  sten->f[d].ptfcn = NULL ;
  sten->npts[d] = 0 ;
  uncompile_stencil_on_domain( d, sten ) ;
}

uncompile_stencil_on_domain( d, sten )
  int d ;
  stencil_desc_t * sten ;
{
  int i ;

  if ( sten == NULL ) return ;
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "uncompile_stencil_on_domain: bad domain descriptor") ;
  sten->compiled[d] = 0 ;
  destroy_procrec_list( sten->procs[d] ) ;
  sten->procs[d] = NULL ; /* 970317 */
}

destroy_procrec_list( prec )
  rsl_procrec_t *prec ;
{ 
  rsl_procrec_t *p, *doomed ;
  int destroy_ptrec_list() ;
  if ( prec == NULL ) return ;
  for ( p = prec ; p != NULL ; )
  {
    doomed = p ;
    p = p->next ;
    destroy_list( &(doomed->point_list), destroy_ptrec_list ) ;
    RSL_FREE(doomed) ;
  }
}

destroy_ptrec_list( ptrec )
  rsl_ptrec_t *ptrec ;
{
  rsl_ptrec_t *p, *doomed ;
  if ( ptrec == NULL ) return ;
  for ( p = ptrec ; p != NULL ; )
  {
    doomed = p ;
    p = p->next ;
    destroy_list( &(doomed->send_messages), NULL ) ;
    destroy_list( &(doomed->recv_messages), NULL ) ;
    RSL_FREE(doomed) ;
  }
}

