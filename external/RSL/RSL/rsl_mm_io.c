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

/* this module is specific to MM5 */


/*@
  RSL_READ_REPL --- Fortran read of replicated, byte data into a buffer

  Notes:
  This does an unformatted (binary) Fortran read on a
  file specified by Arg1.  Data is read into the buffer specified
  by Arg2.  The length of the buffer, in bytes, is given by
  Arg3.  When the call returns, the data will be available on
  all processors.

@*/
RSL_READ_REPL ( unit_p, base, nbytes_p )
  int_p
    unit_p ;     /* (I) Fortran I/O unit number. */
  void *
    base ;       /* (O) Buffer. */
  int_p
    nbytes_p ;   /* (I) Buffer length in bytes. */
{
  int unit, nbytes ;
  rsl_read_req_t request ;
  rsl_read_resp_t resp ;
  rsl_processor_t P ;
  int mlen, nlen, d ;
  int mdest, mtag, msglen ;
  int i_am_monitor ;

  RSL_C_IAMMONITOR ( &i_am_monitor ) ;

#ifdef T3D
  fprintf(stderr,"RSL_READ_REPL not implemented on T3D\n") ;
  fprintf(stderr,"Use RSL_READ_REPLW instead\n") ;
  RSL_TEST_ERR(1,"") ;
  exit(2) ;
#else

  unit = *unit_p ;
  nbytes = *nbytes_p ;
  request.request_type = RSL_READ_SPECIAL2 ;
  request.speciala = nbytes ;
  request.myproc = rsl_myproc ;
  request.base = base ;
  request.unit = *unit_p ;
  request.sequence = io_seq_compute++ ;

  if ( i_am_monitor )
  {
    FORT_CHARACTERREAD ( &unit, base, &nbytes ) ;
    for ( P = 0 ; P < rsl_nproc_all ; P++ )     /* 95/02/22 */
    {
      mdest = rsl_c_comp2phys_proc(P) ;
      if ( mdest != rsl_myproc )
      {
        mtag = MTYPE_FROMTO( MSG_SPECIAL2_RESPONSE, rsl_myproc, mdest ) ;
        msglen = sizeof( resp ) ;
        RSL_SEND( &resp, msglen, mtag, mdest ) ;
        msglen = nbytes ;
        RSL_SEND( base, msglen, mtag, mdest ) ;
      }
    }
  }
  else
  {
    mdest = RSL_C_MONITOR_PROC () ;
    mtag = MTYPE_FROMTO( MSG_SPECIAL2_RESPONSE, mdest, rsl_myproc ) ;
    msglen = sizeof(resp) ;
    RSL_RECV( &resp, msglen, mtag ) ;
    msglen = nbytes ;
    RSL_RECV( base, msglen, mtag ) ;
  }
#endif
}

/*@
  RSL_READ_REPLW --- Fortran read of replicated, typed data into a buffer

  Notes:
  This does an unformatted (binary) Fortran read on a
  file specified by Arg1.  The element type of the data is
  given as Arg2.  It may be
  Verbatim:
$      RSL_REAL
$      RSL_INTEGER
$      RSL_DOUBLE
$      RSL_COMPLEX
$      RSL_CHARACTER
BREAKTHEEXAMPLECODE

  The buffer is provided as
  Arg3.  The length of the buffer, expressed as the number of
  elements to be read, is given by
  Arg4.  When the call returns, the data will be available on
  all processors.

@*/

RSL_READ_REPLW ( unit_p, type_p, base, nelems_p )
  int_p
    unit_p      /* (I) Fortran I/O unit number. */
   ,type_p ;    /* (I) Element type of data. */
  void *
    base ;      /* (O) Buffer. */
  int_p
    nelems_p ;  /* (I) Number of elements to be read. */
{
  int unit, nwords, type ;
  rsl_read_req_t request ;
  rsl_read_resp_t resp ;
  rsl_processor_t P ;

  int mdest, mtag, mlen ;
  int i_am_monitor ;

  RSL_C_IAMMONITOR ( &i_am_monitor ) ;

  unit = *unit_p ;
  nwords = *nelems_p ;
  type = *type_p ;
  request.request_type = RSL_READ_SPECIAL2 ;
  request.speciala = nwords ;
  request.myproc = rsl_myproc ;
  request.base = base ;
  request.unit = *unit_p ;
  request.sequence = io_seq_compute++ ;

  if ( i_am_monitor )
  {

    switch (type)
    {
    case RSL_REAL :
      FORT_REALREAD ( &unit, base, &nwords ) ;
      break ;
    case RSL_INTEGER :
      FORT_INTREAD ( &unit, base, &nwords ) ;
      break ;
#ifndef T3D
    case RSL_DOUBLE :
      FORT_DOUBLEREAD ( &unit, base, &nwords ) ; 
      break ;
#endif
    case RSL_COMPLEX :
      FORT_COMPLEXREAD ( &unit, base, &nwords ) ; 
      break ;
    case RSL_CHARACTER :
      FORT_CHARACTERREAD ( &unit, base, &nwords ) ;
      break ;
    default:
      RSL_TEST_ERR(1,"unsupported type argument") ;
    }
    for ( P = 0 ; P < rsl_nproc_all ; P++ )     /* 95/02/22 */
    {
      mdest = rsl_c_comp2phys_proc(P) ;
      if ( mdest != rsl_myproc )
      {
        mtag = MTYPE_FROMTO( MSG_SPECIAL2_RESPONSE, rsl_myproc, mdest ) ;
        mlen = sizeof( resp ) ;
        RSL_SEND( &resp, mlen, mtag, mdest ) ;
        mlen = elemsize(type)*nwords ;
        RSL_SEND( base, mlen, mtag, mdest ) ;
      }
    }
  }
  else
  {
    mdest = RSL_C_MONITOR_PROC () ;
    mtag = MTYPE_FROMTO( MSG_SPECIAL2_RESPONSE, mdest, rsl_myproc ) ;
    mlen = sizeof(resp) ;
    RSL_RECV( &resp, mlen, mtag ) ;
    mlen = elemsize(type)*nwords ;
    RSL_RECV( base, mlen, mtag ) ;
  }
}

/* this module is specific to MM5 -- yes, it is a kludge to the max */

/* rev: 9/8/94 -- fixed problem wherein the monitor would attempt to
   free buffers for boundaries it did not have (and so, had not been
   allocated storage by the call to handle_spec1).   The effect was
   a segmentation error in the call to free.    */
   

RSL_MM_BDY_IN ( unit_p, iotag_p,
                ebase, wbase, nbase, sbase,
                d_p, type_p,
                bdy_wdth_p,
                glen, llen )
  int_p unit_p ;
  int_p iotag_p ;
  int_p type_p ;
  int_p bdy_wdth_p ;
  int_p d_p ;
  char *ebase, *wbase, *nbase, *sbase ;
  int glen[], llen[] ;
{
  rsl_read_req_t request ;
  rsl_read_resp_t resp ;
  int cursor, mdest, mtag, msglen, dim ;
  int bwdth ;
  unsigned long ig, jg, min, maj, ioffset, joffset, tlen, k ;
  void *dex ;
  char *pbuf ;
  char *buf_w, *buf_e, *buf_n, *buf_s ;
  int wsz, esz, nsz, ssz ;
  rsl_read_resp_t resp_w, resp_e, resp_n, resp_s ;
  int P, mlen, nlen, d  ;
  int bdymark() ;
  int i_am_monitor, got_bdy ;
  rsl_point_t *domain ;

  d = *d_p ;
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "rsl_init_nextcell: bad domain") ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
     "rsl_init_nextcell: invalid domain") ;
  if ( domain_info[d].decomposed != 1 )
  {
    default_decomposition( d_p,
                           &(domain_info[*d_p].loc_m),
                           &(domain_info[*d_p].loc_n) ) ;
  }
  mlen = domain_info[d].len_m ;
  nlen = domain_info[d].len_n ;
  domain = domain_info[d].domain ;

  RSL_C_IAMMONITOR( &i_am_monitor ) ;

  bwdth = *bdy_wdth_p ;
  ioffset = domain_info[*d_p].ilocaloffset ;
  joffset = domain_info[*d_p].jlocaloffset ;
  tlen = elemsize( *type_p ) ;

  request.request_type = RSL_READ_SPECIAL1 ;
  request.speciala = bwdth ;
  request.myproc = rsl_myproc ;
  request.base = ebase ;        /* not used anyway */
  request.domain = *d_p ;
  request.unit = *unit_p ;
  request.type = *type_p ;
  request.iotag = *iotag_p ;
  request.sequence = io_seq_compute++ ;

  P = rsl_c_phys2comp_proc( rsl_myproc ) ;
  mlen = domain_info[*d_p].len_m ;
  nlen = domain_info[*d_p].len_n ;
  d = *d_p ;

  switch( *iotag_p )
  {
  case IO2D :
  case IO2D_IJ :
  case IO2D_JI :
    request.ndim = 2 ;
    break ;
  case IO3D :
  case IO3D_IJK :
  case IO3D_JIK :
    request.ndim = 3 ;
    break ;
  default:
      RSL_TEST_ERR(1,"rsl_mm_bdy_in: unknown data tag") ;
  }
  for ( dim = 0 ; dim < request.ndim ; dim++ )
  {
    request.glen[dim] = glen[dim] ;
    request.llen[dim] = llen[dim] ;
  }

  buf_w = NULL ;
  buf_e = NULL ;
  buf_n = NULL ;
  buf_s = NULL ;
  got_bdy = handle_special1( &request, &buf_e, &esz,
                                       &buf_w, &wsz,
                                       &buf_n, &nsz,
                                       &buf_s, &ssz ) ;
  if ( got_bdy )
  {
    int i, j, k, by, b ;
    int ix_g, jx_g, kx_g ;
    int ix_l, jx_l, kx_l ;
    ix_g = glen[0] ; jx_g = glen[1] ; kx_g = (request.ndim==3)?glen[2]:1 ;
    ix_l = llen[0] ; jx_l = llen[1] ; kx_l = (request.ndim==3)?glen[2]:1 ;

    /* west/east */
    for ( b = 0 ; b < bwdth ; b++ )
      for ( k = 0 ; k < kx_l ; k++ )
        for ( i = 0 ; i < ix_l ; i++ )
	{
          if ( i+ioffset >= 0 )
          {
            for ( by = 0 ; by < tlen ; by++ )
	    {
	      wbase[by+tlen*(i+k*ix_l+b*ix_l*kx_l)]=buf_w[by+tlen*((i+ioffset)+k*ix_g+b*ix_g*kx_g)];
	      ebase[by+tlen*(i+k*ix_l+b*ix_l*kx_l)]=buf_e[by+tlen*((i+ioffset)+k*ix_g+b*ix_g*kx_g)];
	    }
          }
	}

    /* north/south */
    for ( b = 0 ; b < bwdth ; b++ )
      for ( k = 0 ; k < kx_l ; k++ )
        for ( j = 0 ; j < jx_l ; j++ )
	{
          if ( j+joffset >= 0 )
          {
            for ( by = 0 ; by < tlen ; by++ )
	    {
	      nbase[by+tlen*(j+k*jx_l+b*jx_l*kx_l)]=buf_n[by+tlen*((j+joffset)+k*jx_g+b*jx_g*kx_g)];
	      sbase[by+tlen*(j+k*jx_l+b*jx_l*kx_l)]=buf_s[by+tlen*((j+joffset)+k*jx_g+b*jx_g*kx_g)];
	    }
	  }
	}
  }

  RSL_FREE( buf_e ) ;
}

#include "which_boundary.h"

RSL_MM_DIST_BDY ( unit_p, iotag_p, iorder_p, base, d_p,
                  type_p, bdy_wdth_p, bdy_height_p,
                  bdy_g_length_p, bdy_l_length_p )
  int_p
    unit_p
   ,iotag_p
   ,iorder_p ;
  char *
    base ;
  int_p
    d_p
   ,type_p
   ,bdy_wdth_p
   ,bdy_height_p
   ,bdy_g_length_p
   ,bdy_l_length_p ;

{
  rsl_read_req_t request ;
  rsl_read_resp_t resp ;
  int cursor, mdest, mtag, msglen, dim ;
  int bwdth ;
  unsigned long ig, jg, min, maj, ioffset, joffset, tlen, k ;
  void *dex ;
  char *buf ;
  int P, mlen, nlen, d  ;
  int i_am_monitor, got_bdy, iorder, which_boundary ;
  rsl_point_t *domain ;

  d = *d_p ;
  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
     "rsl_init_nextcell: bad domain") ;
  RSL_TEST_ERR( domain_info[d].valid != RSL_VALID,
     "rsl_init_nextcell: invalid domain") ;
  if ( domain_info[d].decomposed != 1 )
  {
    default_decomposition( d_p,
                           &(domain_info[*d_p].loc_m),
                           &(domain_info[*d_p].loc_n) ) ;
  }
  mlen = domain_info[d].len_m ;
  nlen = domain_info[d].len_n ;
  domain = domain_info[d].domain ;

  RSL_C_IAMMONITOR( &i_am_monitor ) ;

  iorder = *iorder_p ;
  bwdth = *bdy_wdth_p ;
  ioffset = domain_info[*d_p].ilocaloffset ;
  joffset = domain_info[*d_p].jlocaloffset ;
  tlen = elemsize( *type_p ) ;

  request.request_type = RSL_READ_SPECIAL1 ;
  request.speciala = bwdth ;
  request.myproc = rsl_myproc ;
  request.base = base ;        /* not used anyway */
  request.domain = *d_p ;
  request.unit = *unit_p ;
  request.type = *type_p ;
  request.iotag = *iotag_p ;
  request.sequence = io_seq_compute++ ;

  P = rsl_c_phys2comp_proc( rsl_myproc ) ;
  mlen = domain_info[*d_p].len_m ;
  nlen = domain_info[*d_p].len_n ;
  d = *d_p ;

  switch( *iotag_p )
  {
  case IO2D :
  case IO2D_IJ :
  case IO2D_JI :
    request.ndim = 2 ;
    break ;
  case IO3D :
  case IO3D_IJK :
  case IO3D_JIK :
    request.ndim = 3 ;
    break ;
  default:
      RSL_TEST_ERR(1,"rsl_mm_bdy_in: unknown data tag") ;
  }

#if 0
   /* east  2 */
   /* west  1 */
   /* north 8 */
   /* south 4 */
#endif

     /* set up some dimensioning for the call to handle_special3.  For regularity with
	other parts of the RSL code it uses the glen/llen construct for carrying this
	information, although in the case of n/s boundaries the i info is not used and
	in the case of the e/w boundaries the j info is not used.  The boundary width
	information is set above, in the assigment of request.speciala                   */

  switch ( iorder )
  {
  case RSL_MLOW :                    /* south */
    which_boundary = WHICH_BDY_SOUTH ;
  case RSL_MHIGH :                   /* north */    /* FALL THROUGH */
    which_boundary = WHICH_BDY_NORTH ;
    request.glen[0] =  0 ;                 /* I dimension NOT USED for n/s boundaries */
    request.glen[1] =  *bdy_g_length_p ;   /* Global length of boundary is global J */
    request.glen[2] =  *bdy_height_p ;     /* number of levels */
    request.llen[0] =  0 ;                 /* I dimension NOT USED for n/s boundaries */
    request.llen[1] =  *bdy_l_length_p ;   /* Local length of boundary is local J */
    request.llen[2] =  *bdy_height_p ;     /* number of levels */
    break ;
  case RSL_NLOW :                    /* west */
    which_boundary = WHICH_BDY_WEST ;
  case RSL_NHIGH :                   /* east */    /* FALL THROUGH */
    which_boundary = WHICH_BDY_EAST ;
    request.glen[0] =  *bdy_g_length_p ;   /* Global Length of boundary is global I */
    request.glen[1] =  0 ;                 /* J dimension NOT USED for e/w boundaries */
    request.glen[2] =  *bdy_height_p ;     /* number of levels */
    request.llen[0] =  *bdy_l_length_p ;   /* Local length of boundary is local I */
    request.llen[1] =  0 ;                 /* J dimension NOT USED for e/w boundaries */
    request.llen[2] =  *bdy_height_p ;     /* number of levels */
    break ;
  default :
    RSL_TEST_ERR(1,"Bad iorder spec for RSL_MM_DIST_BDY") ;
    break ;
  }


  buf = NULL ;
  got_bdy = handle_special3( &request, which_boundary, base, &buf ) ;

  if ( got_bdy )
  {
    int i, j, k, by, b ;
    int ix_g, jx_g, kx_g ;
    int ix_l, jx_l, kx_l ;
    ix_g = request.glen[0] ; jx_g = request.glen[1] ; kx_g = (request.ndim==3)?request.glen[2]:1 ;
    ix_l = request.llen[0] ; jx_l = request.llen[1] ; kx_l = (request.ndim==3)?request.glen[2]:1 ;

    switch ( iorder )
    {
    case RSL_NHIGH :
    /* east */
    for ( b = 0 ; b < bwdth ; b++ )
      for ( k = 0 ; k < kx_l ; k++ )
        for ( i = 0 ; i < ix_l ; i++ )
	{
          if ( i+ioffset >= 0 )
          {
            for ( by = 0 ; by < tlen ; by++ )
	    {
	      base[by+tlen*(i+k*ix_l+b*ix_l*kx_l)]=buf[by+tlen*((i+ioffset)+k*ix_g+b*ix_g*kx_g)];
	    }
          }
	}
        break ;

    case RSL_NLOW :
    /* west */
    for ( b = 0 ; b < bwdth ; b++ )
      for ( k = 0 ; k < kx_l ; k++ )
        for ( i = 0 ; i < ix_l ; i++ )
        {
          if ( i+ioffset >= 0 )
          {
            for ( by = 0 ; by < tlen ; by++ )
            {
	      base[by+tlen*(i+k*ix_l+b*ix_l*kx_l)]=buf[by+tlen*((i+ioffset)+k*ix_g+b*ix_g*kx_g)];
            }
          }
        }
        break ;

    case RSL_MHIGH :
    /* north */
    for ( b = 0 ; b < bwdth ; b++ )
      for ( k = 0 ; k < kx_l ; k++ )
        for ( j = 0 ; j < jx_l ; j++ )
	{
          if ( j+joffset >= 0 )
          {
            for ( by = 0 ; by < tlen ; by++ )
	    {
	      base[by+tlen*(j+k*jx_l+b*jx_l*kx_l)]=buf[by+tlen*((j+joffset)+k*jx_g+b*jx_g*kx_g)];
	    }
	  }
	}
        break ;

    case RSL_MLOW :
    /* south */
    for ( b = 0 ; b < bwdth ; b++ )
      for ( k = 0 ; k < kx_l ; k++ )
        for ( j = 0 ; j < jx_l ; j++ )
	{
          if ( j+joffset >= 0 )
          {
            for ( by = 0 ; by < tlen ; by++ )
	    {
	      base[by+tlen*(j+k*jx_l+b*jx_l*kx_l)]=buf[by+tlen*((j+joffset)+k*jx_g+b*jx_g*kx_g)];
	    }
	  }
	}
        break ;

    default:
        RSL_TEST_ERR(1,"what boundary was that??") ;
        break ;
    }
  }

  RSL_FREE( buf ) ;
}


