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
  RSL_CREATE_PERIOD -- Create a period descriptor.

  Notes:

  See also:
@*/

RSL_CREATE_PERIOD ( pr_p )
  int_p pr_p ;     /* (O) New RSL period descriptor. */
{
  int i ;
  period_desc_t *per ;
  
  /* NOTE: never return the 0th period */
  for ( i = 1 ; i < RSL_MAXDESCRIPTORS ; i++ )
    if ( pr_descriptors[i] == NULL ) break ;    /* got one */

  RSL_TEST_ERR( i == RSL_MAXDESCRIPTORS,
  "rsl_create_period:  out of descriptors.");

  *pr_p = i ;
  per = RSL_MALLOC(period_desc_t,1) ;
  per->tag = PERIOD_DESC ;
  per->has_f90_fields = 0 ;
  pr_descriptors[*pr_p] = per ;
  per->pr = *pr_p ;
}

release_pr_descriptor (pr_p)
  int_p pr_p ;
{
  int pr ;

  pr = *pr_p ;
  RSL_TEST_ERR( pr < 0 || pr >= RSL_MAXDESCRIPTORS,
                "internal error.  Invalid period descriptor.") ;
  if ( pr_descriptors[pr] != NULL )
  {
    pr_descriptors[pr] = NULL ;
  }
}

/*@
  RSL_DESCRIBE_PERIOD -- Defines an RSL period exchange on a domain.

  Notes:

  See also:

@*/

RSL_DESCRIBE_PERIOD ( d_p, pr_p, bdyw_p, message_p )
  int_p d_p,            /* (I) Domain descriptor. */
        pr_p,           /* (I) Period handle */
        bdyw_p ;        /* (I) BDY width. */
  int_p message_p ;       /* (I) Array of message descriptors. */
{
  int d, pr, mh, bdyw ;
  rsl_domain_info_t * dinfo ;
  period_desc_t *per ;
  message_desc_t *msg ;
  int pt ;

  d = *d_p ; pr = *pr_p ; bdyw = *bdyw_p ;

  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
    "rsl_describe_period: bad domain descriptor\n") ;
  dinfo = &(domain_info[d]) ;
  RSL_TEST_ERR(dinfo->valid != RSL_VALID,
    "rsl_describe_period: descriptor is not for a valid domain\n") ;

  RSL_TEST_ERR( bdyw < 1 ,
    "rsl_describe_period: boundary width < 1" ) ;
  RSL_TEST_ERR( pr < 0 || pr >= RSL_MAXDESCRIPTORS,
    "rsl_describe_period: bad period handle" ) ;
  per = (period_desc_t *) pr_descriptors[pr] ;
  RSL_TEST_ERR( per->tag != PERIOD_DESC,
       "rsl_describe_period: handle given is not for an rsl period def" ) ;

  per->bdyw[d] = bdyw ;
  per->compiled[d] = 0 ;

  mh = *message_p ;
  RSL_TEST_ERR( mh < 0 || mh >=RSL_MAXDESCRIPTORS,
         "rsl_describe_period: bad message handle in list") ;
  msg = (message_desc_t *) mh_descriptors[ mh ] ;
  RSL_TEST_ERR( msg->tag != MESSAGE_DESC, 
   "rsl_describe_period: handle given in message list is not for an rsl mesage def" ) ;
  per->msgs[d] = msg ;
  
  release_mh_descriptor( message_p ) ;
  /* add my descriptor to the list for the domain */
  dinfo->periodlist[dinfo->periodcurs] = pr ;
  dinfo->periodcurs++ ;  /* 970317 */
  if ( dinfo->periodcurs >= RSL_MAXDESCRIPTORS )
  {
    sprintf(mess,
     "Domain %d doesn't have room for any more periods, but the allowable\nlimit of %d should have been more than enough.\nYou might recompile RSL with a higher setting for RSL_MAXDESCRIPTORS, but\n it's likely something else is wrong.",
       d, RSL_MAXDESCRIPTORS ) ;
    RSL_TEST_ERR( 1, mess ) ;
  }
}

/* only used internally within the RSL package */
destroy_period( per )
  period_desc_t * per ;
{
  int d ;
  rsl_fldspec_t *fld, *doomed ;
  if ( per == NULL ) return ;
  RSL_TEST_ERR( per->tag != PERIOD_DESC, "destroy_period: arg not a period desc.") ;

  for ( d = 0 ; d < RSL_MAXDOMAINS ; d++ )
  {
    destroy_period_on_domain( d, per ) ;
  }
  release_pr_descriptor (per->pr) ;
  RSL_FREE( per ) ;
}

destroy_period_on_domain( d, per )
  int d ;
  period_desc_t * per ;
{
  int i ;

  if ( per == NULL ) return ;
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "destroy_period_on_domain: bad domain descriptor") ;
  destroy_message( per->msgs[d] ) ;
  uncompile_period_on_domain( d, per ) ;
}

uncompile_period_on_domain( d, per )
  int d ;
  period_desc_t * per ;
{
  int i ;

  if ( per == NULL ) return ;
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "uncompile_period_on_domain: bad domain descriptor") ;
  per->compiled[d] = 0 ;
  destroy_procrec_list( per->procs[0][d] ) ;
  destroy_procrec_list( per->procs[1][d] ) ;
  per->procs[0][d] = NULL ; /* 970317 */
  per->procs[1][d] = NULL ; /* 970317 */
}

