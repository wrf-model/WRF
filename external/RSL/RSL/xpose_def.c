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
  RSL_CREATE_XPOSE -- Create a stencil descriptor.

  Notes:
  The output argument is the integer Arg1, a descriptor to 
  a new RSL stencil.  The stencil is then built (RSL_DESCRIBE_STENCIL)
  and used in stencil exchanges (RSL_EXCH_STENCIL) during the model
  run.

  See also:
  RSL_DESCRIBE_STENCIL, RSL_EXCH_STENCIL
@*/

RSL_CREATE_XPOSE ( xp_p )
  int_p xp_p ;     /* (O) New RSL xpose descriptor. */
{
  int i ;
  xpose_desc_t *xpose ;
  
  /* NOTE: never return the 0th stencil */
  for ( i = 1 ; i < RSL_MAXDESCRIPTORS ; i++ )
    if ( xp_descriptors[i] == NULL ) break ;    /* got one */

  RSL_TEST_ERR( i == RSL_MAXDESCRIPTORS,
  "rsl_create_xpose:  out of descriptors.");

  *xp_p = i ;
  xpose = RSL_MALLOC(xpose_desc_t,1) ;
  xpose->tag = XPOSE_DESC ;
  xpose->has_f90_fields = 0 ;
  xp_descriptors[*xp_p] = xpose ;
  xpose->xp = *xp_p ;
}

release_xp_descriptor (xp_p)
  int_p xp_p ;
{
  int xp ;

  xp = *xp_p ;
  RSL_TEST_ERR( xp < 0 || xp >= RSL_MAXDESCRIPTORS,
                "internal error.  Invalid xpose descriptor.") ;
  if ( xp_descriptors[xp] != NULL )
  {
    xp_descriptors[xp] = NULL ;
  }
}

/*@
  RSL_DESCRIBE_XPOSE -- Defines an RSL transpose exchange on a domain.

@*/

RSL_DESCRIBE_XPOSE ( d_p, xp_p, message_mn_p , message_mz_p , message_nz_p )
  int_p d_p,            /* (I) Domain descriptor. */
        xp_p,           /* (I) Xpose handle */
    message_mn_p,       /* (I) Message descriptor. */
    message_mz_p,       /* (I) Message descriptor. */
    message_nz_p   ;    /* (I) Message descriptor. */
{
  int d, xp, mh ;
  rsl_domain_info_t * dinfo ;
  xpose_desc_t *xpose ;
  message_desc_t *msg_mn, *msg_mz, *msg_nz ;
  int pt ;

  d = *d_p ; xp = *xp_p ;

  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
    "rsl_describe_xpose: bad domain descriptor\n") ;
  dinfo = &(domain_info[d]) ;
  RSL_TEST_ERR(dinfo->valid != RSL_VALID,
    "rsl_describe_xpose: descriptor is not for a valid domain\n") ;

  RSL_TEST_ERR( xp < 0 || xp >= RSL_MAXDESCRIPTORS,
    "rsl_describe_stencil: bad stencil handle" ) ;
  xpose = (xpose_desc_t *) xp_descriptors[xp] ;
  RSL_TEST_ERR( xpose->tag != XPOSE_DESC,
       "rsl_describe_xpose: handle given is not for an rsl xpose def" ) ;

  xpose->compiled[d] = 0 ;

  RSL_TEST_ERR(  (*message_mn_p <= 0 || *message_mn_p >=RSL_MAXDESCRIPTORS),
"rsl_describe_xpose: bad message handle in list,\n  must be valid message") ;
  msg_mn = (message_desc_t *) mh_descriptors[ *message_mn_p ] ;
  xpose->msgs_mn[d] = msg_mn ;

  RSL_TEST_ERR(  (*message_mz_p <= 0 || *message_mz_p >=RSL_MAXDESCRIPTORS),
"rsl_describe_xpose: bad message handle in list,\n  must be valid message") ;
  msg_mz = (message_desc_t *) mh_descriptors[ *message_mz_p ] ;
  xpose->msgs_mz[d] = msg_mz ;

  RSL_TEST_ERR(  (*message_nz_p <= 0 || *message_nz_p >=RSL_MAXDESCRIPTORS),
"rsl_describe_xpose: bad message handle in list,\n  must be valid message") ;
  msg_nz = (message_desc_t *) mh_descriptors[ *message_nz_p ] ;
  xpose->msgs_nz[d] = msg_nz ;

  /* free up the message descriptor; it has done its job */
  release_mh_descriptor( message_mn_p ) ;
  release_mh_descriptor( message_mz_p ) ;
  release_mh_descriptor( message_nz_p ) ;

  /* add my descriptor to the list for the domain */
  dinfo->xposelist[dinfo->xposecurs] = xp ;
  dinfo->xposecurs++ ;  /* 970317 */

  if ( dinfo->xposecurs >= RSL_MAXDESCRIPTORS )
  {
    sprintf(mess,
     "Domain %d doesn't have room for any more xposes, but the allowable\nlimit of %d should have been more than enough.\nYou might recompile RSL with a higher setting for RSL_MAXDESCRIPTORS, but\n it's likely something else is wrong.",
       d, RSL_MAXDESCRIPTORS ) ;
    RSL_TEST_ERR( 1, mess ) ;
     
  }
}

#if 0
/* some of these need to be converted for xposes; others need to be eliminated */
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
#endif
