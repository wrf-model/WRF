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

/* this routine is specific to MM5.  It's purpose is to read in boundary
   data from the boundary file and these records have unusual shapes.  There
   are also multiple fields per record.  For not it easier to do this this
   was, with a special routine, than to incorporate the necessary generality
   into rsl.  This particular implementation of this is especially 
   dreadful -- there is ample room for improvement as time permits. */

/* IMPORTANT -- note that the three-d variables have k (level) as their
   second, not third, dimension in MM5.  EG:  ueb(MIX,MKX,NSPGD)  2/1/94 */

/* rev: 2/3/94, added code to pass and receive extra boundary points to 
   neighboring processors along a 4 pt stencil */

/* rev: 7/14/94, changed to a 12 pt stencil to make sure we have enough
   points for the dot and the cross grids.   Also modified rsl_mm_io.c */

/* rev: 9/8/94, fixed memory leak wherein pbuf was only freed when 
   rsl_myproc was equal to mdest.  Change to free unconditionally
   for each of the boundaries.  */

/* rev: 1/10/95, added test to make sure domain is always c.d.  This
   function will not work properly on a nest and should never be
   called on one.  MM5 does not read in boundary data on a nest. */

int
handle_special1( req, buf_e, esz,
		      buf_w, wsz,
	              buf_n, nsz,
		      buf_s, ssz )
  rsl_read_req_t * req ;
  char **buf_e, **buf_w, **buf_n, **buf_s ;
  int *wsz, *esz, *nsz, *ssz ;
{
  int dim, i, k, ig, jg, d ;
  int maj, min, majlen, minlen ;
  int nelem_ns, nelem_ew, nbytes, typelen, len, cursor  ;
  int P ;
  int bwdth ;
  int mlen, mtag, mdest ;
  int psendto[ RSL_MAXPROC ] ;    /* size of messages to each processor */
  char *rbuf_e, *rbuf_w, *rbuf_n, *rbuf_s, *rbuf_tot ;
  rsl_point_t *domain ;
  int i_am_monitor ;

  RSL_C_IAMMONITOR( &i_am_monitor ) ;

  bwdth = req->speciala ;

  nelem_ns = bwdth * req->glen[1] * ( req->ndim==3?req->glen[2]:1 ) ;
  nelem_ew = bwdth * req->glen[0] * ( req->ndim==3?req->glen[2]:1 ) ;

  typelen = elemsize( req->type ) ;

  *wsz = nelem_ew * typelen ;
  *esz = nelem_ew * typelen ;
  *nsz = nelem_ns * typelen ;
  *ssz = nelem_ns * typelen ;

  /* figure out sizes of buffers needed */
              /*caller will free */
  rbuf_tot = RSL_MALLOC( char, 2*(nelem_ew+nelem_ns)*typelen+100) ; /* 100 is safety */
  *buf_e = &( rbuf_tot[ 0                                      ] ) ;
  *buf_w = &( rbuf_tot[ 1 * nelem_ew                 * typelen ] ) ;  
  *buf_n = &( rbuf_tot[ 2 * nelem_ew                 * typelen ] ) ;  
  *buf_s = &( rbuf_tot[(2 * nelem_ew + 1 * nelem_ns) * typelen ] ) ;  

  if ( i_am_monitor )
  {
    /* call fortran to read a record from the named unit */
    switch ( req->type )
    {
    case RSL_REAL :
      FORT_BDYIN_REAL ( &(req->unit), 
             *buf_e, *buf_w, *buf_n, *buf_s, &nelem_ew, &nelem_ns ) ;
      break ;
#ifndef T3D
    case RSL_DOUBLE :
      FORT_BDYIN_DBL ( &(req->unit),
             *buf_e, *buf_w, *buf_n, *buf_s, &nelem_ew, &nelem_ns ) ;
      break ;
#endif
    default :
      RSL_TEST_WRN(1,"read operation not yet implemented for this data type") ;
    }
  }

  d = req->domain ;
  RSL_TEST_ERR( d != 0,
		"attempt to read boundary data on domain other than c.d.") ;
  majlen = domain_info[d].len_n ;
  minlen = domain_info[d].len_m ;
  domain = domain_info[d].domain ;

  for ( i = 0 ; i < rsl_nproc_all ; i++ ) 	/* 95/02/22 */
  {
    psendto[i] = 0 ;
  }
  /*  figure out sizes for each processor */
  for ( jg = 0 ; jg < majlen ; jg++ )
  {
    for ( ig = 0 ; ig < minlen ; ig++ )
    {
        domain[INDEX_2(jg,ig,minlen)].info_1 = 0 ;
    }
  }
  for ( jg = 0 ; jg < majlen ; jg++ )
  {
    for ( ig = 0 ; ig < minlen ; ig++ )
    {
      if ( jg < bwdth )                             /* west  1 */
      {
        domain[INDEX_2(jg,ig,minlen)].info_1 = domain[INDEX_2(jg,ig,minlen)].info_1 | 1 ; /* mark on wbdy */
        psendto[domain[INDEX_2(jg,ig,minlen)].P] = 1 ;
      }
      if ( jg >= majlen - bwdth && jg <  majlen )   /* east  2 */
      {
        domain[INDEX_2(jg,ig,minlen)].info_1 = domain[INDEX_2(jg,ig,minlen)].info_1 | 2 ; /* mark on edy */
        psendto[domain[INDEX_2(jg,ig,minlen)].P] = 1 ;
      }
      if ( ig >= minlen - bwdth && ig <  minlen )   /* north 8 */
      {
        domain[INDEX_2(jg,ig,minlen)].info_1 = domain[INDEX_2(jg,ig,minlen)].info_1 | 8 ; /* mark on nbdy */
        psendto[domain[INDEX_2(jg,ig,minlen)].P] = 1 ;
      }
      if ( ig < bwdth )                             /* south 4 */
      {
        domain[INDEX_2(jg,ig,minlen)].info_1 = domain[INDEX_2(jg,ig,minlen)].info_1 | 4 ; /* mark on sbdy */
        psendto[domain[INDEX_2(jg,ig,minlen)].P] = 1 ;
      }
    }
  }

  if ( i_am_monitor )
  {
    for ( P = 0 ; P < rsl_nproc_all ; P++ )	/* 95/02/22 */
    {
      if ( psendto[P] != 0 )
      {
        mdest = rsl_c_comp2phys_proc( P ) ;
        if ( ! mdest == rsl_myproc )
        {
          mtag = MTYPE_FROMTO( MSG_SPECIAL1_RESPONSE, rsl_myproc, mdest ) ;
          mlen = 2*(nelem_ew+nelem_ns)*typelen ;
          RSL_SEND( rbuf_tot, mlen, mtag, mdest ) ;
        }
        }
    }
  }
  else
  {
    if ( psendto[rsl_c_phys2comp_proc(rsl_myproc)] )
    {
      mdest = RSL_C_MONITOR_PROC () ;
      mtag = MTYPE_FROMTO( MSG_SPECIAL1_RESPONSE, mdest, rsl_myproc ) ;
      mlen = 2*(nelem_ew+nelem_ns)*typelen ;
      RSL_RECV( rbuf_tot, mlen, mtag ) ;
    }
  }

  if ( psendto[rsl_c_phys2comp_proc(rsl_myproc)] )
    return( 2*(nelem_ew+nelem_ns)*typelen ) ;  /* whether to expect message */
  else
    return(0) ;

}

bdymark( d, ig, jg, hig, hjg, pt, ipt )
  int d, ig, jg, hig, hjg, pt, ipt ;
{ 
  int minlen ;

  minlen = domain_info[d].len_m ;
  if ( domain_info[d].domain[INDEX_2(jg,ig,minlen)].info_1 == 1 )
    domain_info[d].domain[INDEX_2(jg,ig,minlen)].info_2 = 1 ; /* mark for this proc */
}

