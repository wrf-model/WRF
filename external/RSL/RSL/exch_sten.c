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
  RSL_EXCH_STENCIL - Exchange data on an RSL stencil

  Notes:
  This routine is used to exchange data within domain Arg1 using
  an RSL stencil, Arg2.
  When this routine returns, data
  in the ghost areas around the local partition will have been
  updated with data from cells on surrounding processors, as
  described in the stencil.  A stencil must have been described
  in the context of a domain before it can be used on the domain
  (RSL_DESCRIBE_STENCIL).

  This routine generates interprocessor communication on message
  passing architectures.

  All processors must call RSL_EXCH_STENCIL at the same point in
  the code.

  See also:
  RSL_CREATE_STENCIL, RSL_DESCRIBE_STENCIL

@*/

#ifndef crayx1


RSL_EXCH_STENCIL ( d_p, s_p )
  int_p
    d_p             /* (I) Domain descriptor. */
   ,s_p ;           /* (I) Stencil descriptor. */
{
  int d, s ;
  stencil_desc_t *sten ;
  message_desc_t *msg ;
  rsl_procrec_t *procrec ;
  rsl_ptrec_t *ptrec ;
  rsl_list_t *lp, *lp1 ;
  rsl_index_t ig, jg ;
  rsl_point_hdr_t point_hdr ;
  int i, ipt, sp, j ;
  int curs ;
  int nprocs, npts ;
  int retval ;
  int mtype, mdest ;
  char * pbuf ;
  int P ;
  int Pque[RSL_MAXPROC] ;
  rsl_procrec_t *procrecque[RSL_MAXPROC ] ;
  int typeque[RSL_MAXPROC] ;
  int tqp, ndone ;
  void * base ;
  packrec_t * pr ;


  d = *d_p ; s = *s_p ;

  RSL_TEST_ERR(d < 0 || d >= RSL_MAXDOMAINS,
          "bad domain descriptor" ) ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
        "descriptor for invalid domain" ) ;

#ifdef UPSHOT
MPE_Log_event( 15, s, "sten begin" ) ;
#endif
#if 0
fprintf(stderr,"debug called RSL_EXCH_STENCIL %d\n",s ) ;
#endif

  if ((sten = (stencil_desc_t *) sh_descriptors[ s ]) == NULL )
  {
    RSL_TEST_ERR(1,"invalid or unspecified stencil descriptor" ) ;
  }

  /* if stencil has not been compiled, compile it now! */
  if ( sten->compiled[d] == 0 )
  {
    rsl_compile_stencil( d_p, s_p ) ;
  }

  /* post receives */
  /* iterate over procrecs for domain and post buffers */

  tqp = 0 ;
  for ( procrec = sten->procs[d] ; procrec != NULL ; procrec = procrec->next )
  {
    if ( procrec->unpack_table_nbytes > 0 )
    {
      P = procrec->P ;
      Pque[tqp] = P ;
      procrecque[tqp] = procrec ;
      pbuf = buffer_for_proc( P, procrec->unpack_table_nbytes, RSL_RECVBUF ) ;
      mtype = MTYPE_FROMTO( MSG_STENCOM, 
                            rsl_c_comp2phys_proc (procrec->P), 
                            rsl_myproc ) ;
      typeque[tqp] = mtype ;
      procrec->nrecvs++ ; /* diagnostic */
#if 0
fprintf(stderr,"debug posting async recv for %d bytes from %d\n", procrec->unpack_table_nbytes, rsl_c_comp2phys_proc (procrec->P) ) ;
#endif
      RSL_RECVBEGIN ( pbuf, procrec->unpack_table_nbytes, mtype ) ;
      tqp++ ;
    }
  }
  nprocs = tqp ;

  /* pack buffers and issue sends */

  for ( procrec = sten->procs[d] ; procrec != NULL ; procrec = procrec->next )
  {
    pbuf=buffer_for_proc(procrec->P, procrec->pack_table_nbytes, RSL_SENDBUF) ;
    pr = procrec->pack_table ;
    for ( curs = 0, i = 0 ; i < procrec->pack_table_size ; i++, pr++ )
    {
      if ( sten->has_f90_fields && procrec->pack_table_size > 0 )
        base = (void *) get_base_for_index ( pr->f90_table_index ) ;
      else
        base =  pr->base ;
#if 0
fprintf(stderr,"pack   base %lu, f90_index %d, sten=%d\n",base,pr->f90_table_index,s) ;
#endif
      for ( j = 0 ; j < pr->nelems ; j++ )
      {

#if 0
if ( rsl_debug_flg ) {
fprintf(stderr,"pck %08x, buf %08x, curs %5d, n %5d, off %5d, j %5d, s %5d\n",
(char *)(base) + pr->offset + j * pr->stride,
&(pbuf[curs]), curs, pr->n,
pr->offset, j, pr->stride ) ;
}
#endif

        bcopy((char *)(base) + pr->offset + j * pr->stride,
	      &(pbuf[curs]),pr->n) ;
        curs += pr->n ;
      }
    }
    if ( curs > 0 )
    {
      mdest = rsl_c_comp2phys_proc (procrec->P) ;
      mtype = MTYPE_FROMTO( MSG_STENCOM, rsl_myproc, mdest ) ;
      procrec->nsends++ ;
      if ( curs > procrec->pack_table_nbytes ) 
      {
        sprintf(mess,"pack buffer overflow %d > %d\n",curs,procrec->pack_table_nbytes) ;
        RSL_TEST_ERR(1,mess) ;
      }
#if 0
fprintf(stderr,"debug sending %d bytes to %d, sten=%d\n", curs, mdest, s ) ;
#endif
      RSL_SEND ( pbuf, curs, mtype, mdest ) ;
    }
    else if ( curs == 0 && procrec->pack_table_nbytes != 0 )
    {
      RSL_TEST_ERR(1,"internal error") ;
    }
  }

  /* wait on receives and unpack messages as they come in */
  ndone = 0 ;
  tqp = 0 ;
  retval = 1 ;

  while( ndone < nprocs )
  {
    if (tqp >= nprocs ) tqp = 0 ;
    if (typeque[tqp] != RSL_INVALID)
    {
      mtype = typeque[tqp] ;
      if ( rsl_noprobe == NULL )
        RSL_PROBE ( mtype, &retval ) ;
      /* else, retval will always be 1 */

      if ( retval )
      {
#ifdef PGON
/* on the Paragon, calling RSL_PROBE clears the message so this
   would bomb on an unknown message id.  Don't call unless the probe
   is disabled (rsl_noprobe != NULL). */
        if ( rsl_noprobe != NULL ) RSL_RECVEND ( mtype ) ;
#else  
        RSL_RECVEND ( mtype ) ;
#endif

	curs = 0 ;
        pbuf = buffer_for_proc( Pque[tqp], 0, RSL_RECVBUF ) ;
        procrec = procrecque[tqp] ;
        pr = procrec->unpack_table ;
        for ( curs = 0, i = 0 ; i < procrec->unpack_table_size ; i++, pr++ )
	{
          if ( sten->has_f90_fields && procrec->unpack_table_size > 0 )
            base = (void *) get_base_for_index ( pr->f90_table_index ) ;
          else
            base = pr->base ;
#if 0
fprintf(stderr,"unpack base %lu, f90_index %d, sten=%d\n",base,pr->f90_table_index,s) ;
#endif
          for ( j = 0 ; j < pr->nelems ; j++ )
          {
            bcopy(&(pbuf[curs]),
		 (char *)(base) + pr->offset + j * pr->stride, pr->n) ;
            curs += pr->n ;
	  }
	}
	if ( curs == 0 )
	{
	  RSL_TEST_ERR(1,"internal error") ;
	}
        if ( curs > procrec->unpack_table_nbytes )
        {
          sprintf(mess,"unpack buffer overflow %d > %d\n",curs,procrec->pack_table_nbytes) ;
          RSL_TEST_ERR(1,mess) ;
	}
#if 0
fprintf(stderr,"debug got message from %d and unpacked %d bytes; sten=%d\n", Pque[tqp], curs, s ) ;
#endif
        typeque[tqp] = RSL_INVALID ;
        ndone++ ;
      }
    }
    tqp++ ;
  }
#ifdef UPSHOT
MPE_Log_event( 16, s, "sten end" ) ;
#endif

}


#else


RSL_EXCH_STENCIL ( d_p, s_p )
  int_p
    d_p             /* (I) Domain descriptor. */
   ,s_p ;           /* (I) Stencil descriptor. */
{
  int d, s ;
  stencil_desc_t *sten ;
  rsl_procrec_t *procrec ;
  int i,j ;
  int curs ;
  int nprocs ;
  int retval ;
  int mtype, mdest ;
  char * pbuf ;
  int P ;
  int Pque[RSL_MAXPROC] ;
  rsl_procrec_t *procrecque[RSL_MAXPROC ] ;
  int typeque[RSL_MAXPROC] ;
  int tqp, ndone ;
  void * base ;

  packrec_t * pr ;

  d = *d_p ; s = *s_p ;

  RSL_TEST_ERR(d < 0 || d >= RSL_MAXDOMAINS,
          "bad domain descriptor" ) ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
        "descriptor for invalid domain" ) ;

#ifdef UPSHOT
MPE_Log_event( 15, s, "sten begin" ) ;
#endif
#if 0
fprintf(stderr,"debug called RSL_EXCH_STENCIL %d\n",s ) ;
#endif

  if ((sten = (stencil_desc_t *) sh_descriptors[ s ]) == NULL )
  {
    RSL_TEST_ERR(1,"invalid or unspecified stencil descriptor" ) ;
  }

  /* if stencil has not been compiled, compile it now! */
  if ( sten->compiled[d] == 0 )
  {
    rsl_compile_stencil( d_p, s_p ) ;
                                                                                       
    /* fill in curs value for pack and unpack buffers */
    for ( procrec = sten->procs[d] ; procrec != NULL ; procrec = procrec->next )
    {
      /* determine offset into pack buffer for each element */
      pr = procrec->pack_table ;
      if ( procrec->pack_table_nbytes > 0 )
      {
        for ( curs = 0, i = 0 ; i < procrec->pack_table_size ; i++, pr++ )
        {
          pr->curs = curs;
          curs += pr->nelems * pr->n;
        }
//        fprintf(stderr, "pack %d %d\n", curs, procrec->pack_table_nbytes);
      }
      /* determine offset into unpack buffer for each element */
      if ( procrec->unpack_table_nbytes > 0 )
      {
        pr = procrec->unpack_table ;
        for ( curs = 0, i = 0 ; i < procrec->unpack_table_size ; i++, pr++ )
        {
          pr->curs = curs;
          curs += pr->nelems * pr->n;
        }
//        fprintf(stderr, "unpack %d %d\n", curs, procrec->unpack_table_nbytes);
      }
    }
  }

  /* post receives */
  /* iterate over procrecs for domain and post buffers */

  tqp = 0 ;
  for ( procrec = sten->procs[d] ; procrec != NULL ; procrec = procrec->next )
  {
    if ( procrec->unpack_table_nbytes > 0 )
    {
      P = procrec->P ;
      Pque[tqp] = P ;
      procrecque[tqp] = procrec ;
      pbuf = buffer_for_proc( P, procrec->unpack_table_nbytes, RSL_RECVBUF ) ;
      mtype = MTYPE_FROMTO( MSG_STENCOM, 
                   rsl_c_comp2phys_proc (procrec->P), rsl_myproc ) ;
      typeque[tqp] = mtype ;
      procrec->nrecvs++ ; /* diagnostic */
#if 0
fprintf(stderr,"debug posting async recv for %d bytes from %d\n", procrec->unpack_table_nbytes, rsl_c_comp2phys_proc (procrec->P) ) ;
#endif
      RSL_RECVBEGIN ( pbuf, procrec->unpack_table_nbytes, mtype ) ;
      tqp++ ;
    }
  }
  nprocs = tqp ;

  /* pack buffers and issue sends */

  for ( procrec = sten->procs[d] ; procrec != NULL ; procrec = procrec->next )
  {

    pbuf=buffer_for_proc(procrec->P, procrec->pack_table_nbytes, RSL_SENDBUF) ;

#pragma csd parallel for private(i, pr, base)
    for ( i = 0 ; i < procrec->pack_table_size ; i++ )
    {
      int inc, nwrds;
      int *bufin, *bufout;

      pr = &procrec->pack_table[i];

      if ( sten->has_f90_fields && procrec->pack_table_size > 0 )
        base = (void *) get_base_for_index ( pr->f90_table_index ) ;
      else
        base =  pr->base ;
#if 0
fprintf(stderr,"pack   base %lu, f90_index %d, sten=%d\n",base,pr->f90_table_index,s) ;
#endif
#pragma no_cache_alloc bufin bufout
      bufin = (int *)(base) + (pr->offset >> 2);
      bufout = (int *)(pbuf) + (pr->curs >> 2);
      inc = pr->stride >> 2;
      nwrds = pr->n >> 2;
      if (nwrds < 64) {
        int j, k;
        for (j = 0; j < nwrds; j++) {
#pragma _CRI ivdep
#pragma prefervector 
           for (k = 0; k < pr->nelems; k++) {
              bufout[k*nwrds+j] = bufin[k*inc+j];
           }
        }
      }
      else {
        int j, k;
        int iwd = 0;
        int iwd2 = 0;
        for (j = 0; j < pr->nelems; j++) {
#pragma _CRI ivdep
          for (k = 0; k < nwrds; k++) {
            bufout[iwd++] = bufin[iwd2+k];
          }
          iwd2 += inc;
        }
      }
    }

    curs = procrec->pack_table_nbytes;
    if ( curs > 0 )
    {
      mdest = rsl_c_comp2phys_proc (procrec->P) ;
      mtype = MTYPE_FROMTO( MSG_STENCOM, rsl_myproc, mdest ) ;
      procrec->nsends++ ;
#if 0
fprintf(stderr,"debug sending %d bytes to %d, sten=%d\n", curs, mdest, s ) ;
#endif
      RSL_SEND ( pbuf, curs, mtype, mdest ) ;
    }
  }

  /* wait on receives and unpack messages as they come in */
  ndone = 0 ;
  tqp = 0 ;
  retval = 1 ;

  while( ndone < nprocs )
  {
    if (tqp >= nprocs ) tqp = 0 ;
    if (typeque[tqp] != RSL_INVALID)
    {
      mtype = typeque[tqp] ;
      if ( rsl_noprobe == NULL )
        RSL_PROBE ( mtype, &retval ) ;
      /* else, retval will always be 1 */

      if ( retval )
      {
#ifdef PGON
/* on the Paragon, calling RSL_PROBE clears the message so this
   would bomb on an unknown message id.  Don't call unless the probe
   is disabled (rsl_noprobe != NULL). */
        if ( rsl_noprobe != NULL ) RSL_RECVEND ( mtype ) ;
#else  
        RSL_RECVEND ( mtype ) ;
#endif

        pbuf = buffer_for_proc( Pque[tqp], 0, RSL_RECVBUF ) ;
        procrec = procrecque[tqp] ;
#pragma csd parallel for private(i, pr, base)
        for ( i = 0 ; i < procrec->unpack_table_size ; i++ )
        {
          int inc, nwrds;
          int *bufin, *bufout;
#pragma no_cache_alloc bufin bufout

          pr = &procrec->unpack_table[i] ;

          if ( sten->has_f90_fields && procrec->unpack_table_size > 0 )
            base = (void *) get_base_for_index ( pr->f90_table_index ) ;
          else
            base = pr->base ;
#if 0
fprintf(stderr,"unpack base %lu, f90_index %d, sten=%d\n",base,pr->f90_table_index,s) ;
#endif

          bufin = (int *)(pbuf) + (pr->curs >> 2);
          bufout = (int *)(base) + (pr->offset >> 2);
          inc = pr->stride >> 2;
          nwrds = pr->n >> 2;
          if (nwrds < 64) {
            int j, k;
            for (j = 0; j < nwrds; j++) {
#pragma _CRI ivdep
#pragma prefervector 
                for (k = 0; k < pr->nelems; k++) {
                bufout[k*inc+j] = bufin[k*nwrds+j];
              }
            }
          }
          else {
            int j, k;
            int iwd = 0;
            int iwd2 = 0;
            for (j = 0; j < pr->nelems; j++) {
#pragma _CRI ivdep
              for (k = 0; k < nwrds; k++) {
                bufout[iwd2+k] = bufin[iwd++];
              }
              iwd2 += inc;
            }
          }
        }

#if 0
fprintf(stderr,"debug got message from %d and unpacked %d bytes; sten=%d\n", Pque[tqp], curs, s ) ;
#endif
        typeque[tqp] = RSL_INVALID ;
        ndone++ ;
      }
    }
    tqp++ ;
  }
#ifdef UPSHOT
MPE_Log_event( 16, s, "sten end" ) ;
#endif
}


#endif
