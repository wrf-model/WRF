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

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <sys/uio.h>
#include <string.h>
#ifdef T3D
#include <fortran.h>
#endif

       static int first_time_through = 1 ;


static char request_buf[ 2048 ] ;

/* hack for IBM/Chameleon, and other machines/api's that aren't
   completely competant about flushing FORTRAN I/O before shutting
   down.  We'll do that here, each time a shutdown occurs.  That
   entails keeping track of which files were written to, hence
   this data structure. */

#define NUNITS  128
static unsigned char unit_written[NUNITS] ;



RSL_IOSERVE ()
{
  int *rtype ;
  int msglen, mtag ;
  int done, nshutdown ;
  int i, x ;

  nshutdown = 0 ;
  done = 0 ;

  for ( i = 0 ; i < NUNITS ; i++ )
     unit_written[i] = '\0' ;

  while( !done )
  {
    msglen = 2048 ;
    mtag = MSG_MONITOR_REQUEST ;
    RSL_RECV( request_buf, msglen, mtag ) ;
    rtype = (int *) request_buf ;
    switch( *rtype )
    {
    case RSL_READ_REQUEST :
      handle_read_request( request_buf ) ;
      break ;
    case RSL_WRITE_REQUEST :
      handle_write_request( request_buf ) ;
      break ;
    case RSL_READ_SPECIAL1   :
      handle_special1( request_buf ) ;
      break ;
    case RSL_READ_SPECIAL2   :
      handle_special2( request_buf ) ;
      break ;
    case RSL_SHUTDOWN_REQUEST  :
      /* last processor causes shutdown */
      nshutdown++ ;
      if ( nshutdown == rsl_nproc ) 
        done = 1 ;
      break ;
    default :
      sprintf(mess,"rsl_ioserve: monitor received unknown request %d",*rtype) ;
      RSL_TEST_ERR(1,mess) ;
    }
  }

  for ( i = 0 ; i < NUNITS ; i++ )
  {
     if ( unit_written[i] != '\0' )
     {
        x = i + 1 ;
        RSL_FUNIT_CLOSE ( &x ) ;
     }
  }
}

int
handle_read_request( req, resp_me, pbuf_me )
  rsl_read_req_t * req ;
  char * resp_me ;
  char ** pbuf_me ;
{
  int dim, i, k, ig, jg, nelem ;
  int columnelems, nbytes, typelen, len, cursor  ;
  int P ;
  int msglen, mtag, mdest ;
  int mlen, nlen, minelems, majelems ;
  rsl_read_resp_t resp ;
  int psize[ RSL_MAXPROC ] ;    /* size of messages to each processor */
  char * rbuf ;
  char *pbuf ;
  rsl_point_t *domain ;
  int nelem_alloc ;

/* efficiency update from JM, 2002/05/24 */
  int numpts[RSL_MAXPROC], maxnumpts, iii ;
  int *iptlst, *jptlst, *ip1, *ip2  ;
  double *dp1, *dp2 ;

/* bug fix from AJB; rbuf needs to be as large as the
   domain size (with padding out to factor of 3 for nest
   dimensions) or may generate a seg-fault in bcopies below
   in loop that runs over the mlen/nlen dimensions
*/
  /* figure out size of read buffer needed (includes padding) */
  nelem_alloc = domain_info[req->domain].len_m * domain_info[req->domain].len_n ;
  switch ( req->iotag )
  {
  case IO2D_IJ  : break ;
  case IO2D_JI  : break ;
  case IO3D_IJK : nelem_alloc *= req->glen[2] ; break ;
  case IO3D_JIK : nelem_alloc *= req->glen[2] ; break ;
  case IO3D_KIJ : nelem_alloc *= req->glen[0] ; break ;
  case IO3D_IKJ : nelem_alloc *= req->glen[1] ; break ;
  }
  /* figure out number of elements to read into read buffer */
  nelem = 1 ;
  for  ( dim = 0 ; dim < req->ndim ; dim++ )
  {
    nelem *= req->glen[dim] ; 
  }
  typelen = elemsize( req->type ) ;
  nbytes = nelem_alloc * typelen ;

  rbuf = RSL_MALLOC( char, nbytes ) ;  

  /* call fortran to read a record from the named unit */
  if ( req->internal ) 
  {
    bcopy( req->unit_p, rbuf, nbytes ) ;
  }
  else
  {
    switch ( req->type )
    {
    case RSL_REAL :
      FORT_REALREAD ( &(req->unit), rbuf, &nelem ) ;
      break ;
    case RSL_INTEGER :
      FORT_INTREAD ( &(req->unit), rbuf, &nelem ) ;
      break ;
#ifndef T3D
    case RSL_DOUBLE :
      FORT_DOUBLEREAD ( &(req->unit), rbuf, &nelem ) ;
      break ;
#endif
    case RSL_COMPLEX :
      FORT_COMPLEXREAD ( &(req->unit), rbuf, &nelem ) ;
      break ;
    case RSL_CHARACTER :
      FORT_CHARACTERREAD ( &(req->unit), rbuf, &nelem ) ;
      break ;
    default :
      RSL_TEST_WRN(1,"read operation not yet implemented for this data type") ;
    }
  }
  /* global record is now stored -- ship it out */
  switch ( req->iotag )
  {
  case IO2D_IJ :
    columnelems = 1 ;
    minelems = req->glen[0] ;
    majelems = req->glen[1] ;
    break ;
  case IO2D_JI :
    columnelems = 1 ;
    minelems = req->glen[1] ;
    majelems = req->glen[0] ;
    break ;
  case IO3D_IJK :
    columnelems = req->glen[2] ;
    minelems = req->glen[0] ;
    majelems = req->glen[1] ;
    break ;
  case IO3D_JIK :
    columnelems = req->glen[2] ;
    minelems = req->glen[1] ;
    majelems = req->glen[0] ;
    break ;
  case IO3D_KIJ :
    columnelems = req->glen[0] ;
    minelems = req->glen[1] ;
    majelems = req->glen[2] ;
    break ;
  case IO3D_IKJ :
    columnelems = req->glen[1] ;
    minelems = req->glen[0] ;
    majelems = req->glen[2] ;
    break ;
  default:
    RSL_TEST_ERR(1,"handle_read_request: unknown data tag") ;
  }
  /*  figure out sizes for each processor */
  pbuf = NULL ;
  for ( i = 0 ; i < rsl_nproc_all ; i++ )       /* 95/02/22 */
  {
    psize[i] = 0 ;
    numpts[i] = 0 ;
  }
  mlen = domain_info[req->domain].len_m ;
  nlen = domain_info[req->domain].len_n ;
  domain = domain_info[req->domain].domain ;
  for ( jg = 0 ; jg < nlen ; jg++ )
  {
    for ( ig = 0 ; ig < mlen ; ig++ )
    {
      P = domain[INDEX_2(jg,ig,mlen)].P ;       /* 2002/05/24 */
      psize[P] += columnelems * typelen ;       /* 2002/05/24 */
      if ( P >= 0 && P < rsl_nproc_all ) numpts[P]++ ;   /* 2002/05/24 */
    }
  }
  maxnumpts = 0 ;                               /* 2002/05/24 */
  for ( i = 0 ; i < rsl_nproc_all ; i++ )       /* 2002/05/24 */
  {                                             /* 2002/05/24 */
    if ( maxnumpts < numpts[i] ) maxnumpts = numpts[i] ; /* 2002/05/24 */
  }                                             /* 2002/05/24 */

  iptlst = RSL_MALLOC( int, rsl_nproc_all * maxnumpts ) ;      /* 2002/05/24 */
  jptlst = RSL_MALLOC( int, rsl_nproc_all * maxnumpts ) ;      /* 2002/05/24 */
  for ( i = 0 ; i < rsl_nproc_all ; i++ ) numpts[i] = 0 ;      /* 2002/05/24 */
  for ( jg = 0 ; jg < nlen ; jg++ )                            /* 2002/05/24 */
  {                                                            /* 2002/05/24 */
   for ( ig = 0 ; ig < mlen ; ig++ )                           /* 2002/05/24 */
    {                                                          /* 2002/05/24 */
      P = domain[INDEX_2(jg,ig,mlen)].P ;                      /* 2002/05/24 */
      if ( P >= 0 && P < rsl_nproc_all )                       /* 2002/05/24 */
      {                                                        /* 2002/05/24 */
        iptlst[INDEX_2(P,numpts[P],maxnumpts)] = ig ;          /* 2002/05/24 */
        jptlst[INDEX_2(P,numpts[P],maxnumpts)] = jg ;          /* 2002/05/24 */
        numpts[P]++ ;                                          /* 2002/05/24 */
      }                                                        /* 2002/05/24 */
    }                                                          /* 2002/05/24 */
  }                                                            /* 2002/05/24 */

  for ( P = 0 ; P < rsl_nproc_all ; P++ )       /* 95/02/22 */
  {
    len = 0 ;
    len += psize[P] ;
    pbuf = RSL_MALLOC( char, len ) ;
    resp.response_type = RSL_READ_RESPONSE ;
    resp.sequence = req->sequence ;
    resp.tofollow = psize[P] ;
    cursor = 0 ;
    /*bcopy( &resp, &(pbuf[cursor]), sizeof( resp )) ; cursor += sizeof(resp) ; */

/* NOTE AND WARNING:  this code is quick and dirty and makes the very
   naive assumption that the data set being read in is point for point
   with the domain and is dimensioned to be exactly the same size!!!! 
   Only with this assumption can the ig, jg indices into the domain
   data structure be used in this way as indices into the data.  This
   will work for MM.  A more general approach will require modification. */

#ifndef vpp
if ( typelen == sizeof ( int ) ) {
    for ( iii = 0 ; iii < numpts[P] ; iii++ )
    {
      ig = iptlst[INDEX_2(P,iii,maxnumpts)] ;
      jg = jptlst[INDEX_2(P,iii,maxnumpts)] ;
          RSL_TEST_ERR( cursor >= len,
   "something wrong with read request: check glen, llen arrays in call") ;
          switch ( req->iotag )
          {
          case IO2D_IJ :
            ip1 = (int *) &(rbuf[typelen*(ig+jg*req->glen[0])]) ;
            ip2 = (int *) &(pbuf[cursor]) ;
            *ip2 = *ip1 ;
            cursor += typelen ;
            break ;
          case IO2D_JI :
            ip1 = (int *) &(rbuf[typelen*(jg+ig*req->glen[0])]) ;
            ip2 = (int *) &(pbuf[cursor]) ;
            *ip2 = *ip1 ;
            cursor += typelen ;
            break ;
          case IO3D_IJK :
            k = 0 ;
            ip1 = (int *) &(rbuf[typelen*(ig+req->glen[0]*(jg+k*req->glen[1]))]) ;
            ip2 = (int *) &(pbuf[cursor]) ;
            for ( k = 0 ; k < req->glen[2] ; k++ )
            {
              *ip2 = *ip1 ;
              ip1 += req->glen[0] * req->glen[1] ;
              ip2++ ;
            }
            cursor += typelen*req->glen[2]  ;
            break ;
          case IO3D_JIK :
            k = 0 ;
            ip1 = (int *) &(rbuf[typelen*(jg+req->glen[0]*(ig+k*req->glen[1]))]) ;
            ip2 = (int *) &(pbuf[cursor]) ;
            for ( k = 0 ; k < req->glen[2] ; k++ )
            {
              *ip2 = *ip1 ;
              ip1 += req->glen[0] * req->glen[1] ;
              ip2++ ;
            }
            cursor += typelen*req->glen[2]  ;
            break ;
          case IO3D_KIJ :
            k = 0 ;
            ip1 = (int *) &(rbuf[typelen*(k+req->glen[0]*(ig+jg*req->glen[1]))]) ;
            ip2 = (int *) &(pbuf[cursor]) ;
            for ( k = 0 ; k < req->glen[0] ; k++ )
            {
              *ip2 = *ip1 ;
              ip1++ ;
              ip2++ ;
            }
            cursor += typelen*req->glen[0] ;
            break ;
          case IO3D_IKJ :
            k = 0 ;
            ip1 = (int *) &(rbuf[typelen*(ig+req->glen[0]*(k+jg*req->glen[1]))]) ;
            ip2 = (int *) &(pbuf[cursor]) ;
            for ( k = 0 ; k < req->glen[1] ; k++ )
            {
              *ip2 = *ip1 ;
              ip1 += req->glen[0] ;
              ip2++ ;
            }
            cursor += typelen*req->glen[1] ;
            break ;
          }
    }
} else if ( typelen == sizeof ( double ) ) {
    for ( iii = 0 ; iii < numpts[P] ; iii++ )
    {
      ig = iptlst[INDEX_2(P,iii,maxnumpts)] ;
      jg = jptlst[INDEX_2(P,iii,maxnumpts)] ;
          RSL_TEST_ERR( cursor >= len,
   "something wrong with read request: check glen, llen arrays in call") ;
          switch ( req->iotag )
          {
          case IO2D_IJ :
            dp1 = (double *) &(rbuf[typelen*(ig+jg*req->glen[0])]) ;
            dp2 = (double *) &(pbuf[cursor]) ;
            *dp2 = *dp1 ;
            cursor += typelen ;
            break ;
          case IO2D_JI :
            dp1 = (double *) &(rbuf[typelen*(jg+ig*req->glen[0])]) ;
            dp2 = (double *) &(pbuf[cursor]) ;
            *dp2 = *dp1 ;
            cursor += typelen ;
            break ;
          case IO3D_IJK :
            k = 0 ;
            dp1 = (double *) &(rbuf[typelen*(ig+req->glen[0]*(jg+k*req->glen[1]))]) ;
            dp2 = (double *) &(pbuf[cursor]) ;
            for ( k = 0 ; k < req->glen[2] ; k++ )
            {
              *dp2 = *dp1 ;
              dp1 += req->glen[0] * req->glen[1] ;
              dp2++ ;
            }
            cursor += typelen*req->glen[2]  ;
            break ;
          case IO3D_JIK :
            k = 0 ;
            dp1 = (double *) &(rbuf[typelen*(jg+req->glen[0]*(ig+k*req->glen[1]))]) ;
            dp2 = (double *) &(pbuf[cursor]) ;
            for ( k = 0 ; k < req->glen[2] ; k++ )
            {
              *dp2 = *dp1 ;
              dp1 += req->glen[0] * req->glen[1] ;
              dp2++ ;
            }
            cursor += typelen*req->glen[2]  ;
            break ;
          case IO3D_KIJ :
            k = 0 ;
            dp1 = (double *) &(rbuf[typelen*(k+req->glen[0]*(ig+jg*req->glen[1]))]) ;
            dp2 = (double *) &(pbuf[cursor]) ;
            for ( k = 0 ; k < req->glen[0] ; k++ )
            {
              *dp2 = *dp1 ;
              dp1++ ;
              dp2++ ;
            }
            cursor += typelen*req->glen[0] ;
            break ;
          case IO3D_IKJ :
            k = 0 ;
            dp1 = (double *) &(rbuf[typelen*(ig+req->glen[0]*(k+jg*req->glen[1]))]) ;
            dp2 = (double *) &(pbuf[cursor]) ;
            for ( k = 0 ; k < req->glen[1] ; k++ )
            {
              *dp2 = *dp1 ;
              dp1 += req->glen[0] ;
              dp2++ ;
            }
            cursor += typelen*req->glen[1] ;
            break ;
          }
    }
}else{
    for ( iii = 0 ; iii < numpts[P] ; iii++ )
    {
      ig = iptlst[INDEX_2(P,iii,maxnumpts)] ;
      jg = jptlst[INDEX_2(P,iii,maxnumpts)] ;
          RSL_TEST_ERR( cursor >= len,
   "something wrong with read request: check glen, llen arrays in call") ;
          switch ( req->iotag )
          {
          case IO2D_IJ :
            bcopy(&(rbuf[typelen*(ig+jg*req->glen[0])]),
                  &(pbuf[cursor]),
                  typelen) ;
            cursor += typelen ;
            break ;
          case IO2D_JI :
            bcopy(&(rbuf[typelen*(jg+ig*req->glen[0])]),
                  &(pbuf[cursor]),
                  typelen) ;
            cursor += typelen ;
            break ;
          case IO3D_IJK :
            for ( k = 0 ; k < req->glen[2] ; k++ )
            {
              bcopy(&(rbuf[typelen*(ig+req->glen[0]*(jg+k*req->glen[1]))]),
                    &(pbuf[cursor]),
                    typelen) ;
              cursor += typelen ;
            }
            break ;
          case IO3D_JIK :
            for ( k = 0 ; k < req->glen[2] ; k++ )
            {
              bcopy(&(rbuf[typelen*(jg+req->glen[0]*(ig+k*req->glen[1]))]),
                    &(pbuf[cursor]),
                    typelen) ;
              cursor += typelen ;
            }
            break ;
          case IO3D_KIJ :
            for ( k = 0 ; k < req->glen[0] ; k++ )
            {
              bcopy(&(rbuf[typelen*(k+req->glen[0]*(ig+jg*req->glen[1]))]),
                    &(pbuf[cursor]),
                    typelen) ;
              cursor += typelen ;
            }
            break ;
          case IO3D_IKJ :
            for ( k = 0 ; k < req->glen[1] ; k++ )
            {
              bcopy(&(rbuf[typelen*(ig+req->glen[0]*(k+jg*req->glen[1]))]),
                    &(pbuf[cursor]),
                    typelen) ;
              cursor += typelen ;
            }
            break ;
          }
    }
}
#else
    for ( jg = 0 ; jg < nlen ;  jg++ )
    {
      if ( domain[INDEX_2(jg,0,mlen)].P == P )
      {
          switch ( req->iotag )
          {
          case IO2D_IJ :
            if ( req->type == RSL_REAL )
            {
              ig = 0 ;
              VRCOPY (&(rbuf[typelen*(ig+jg*req->glen[0])]),
                      &(pbuf[cursor]),
                      &mlen) ;
              cursor += typelen*mlen ;
            }
            else
            {
              for ( ig = 0 ; ig < mlen ; ig++ )
              {
                bcopy(&(rbuf[typelen*(ig+jg*req->glen[0])]),
                      &(pbuf[cursor]),
                      typelen) ;
                cursor += typelen ;
              }
            }
            break ;
          case IO2D_JI :
            if ( req->type == RSL_REAL )
            {
              for ( ig = 0 ; ig < mlen ; ig++ )
              {
                bcopy(&(rbuf[typelen*(jg+ig*req->glen[0])]),
                      &(pbuf[cursor]),
                      typelen) ;
                cursor += typelen ;
              }
            }
            break ;
          case IO3D_IJK :
            if ( req->type == RSL_REAL )
            {
              ig = 0 ;
              for ( k = 0 ; k < req->glen[2] ; k++ )
              {
                VRCOPY (&(rbuf[typelen*(ig+req->glen[0]*(jg+k*req->glen[1]))]),
                        &(pbuf[cursor]),
                        &mlen) ;
                cursor += typelen*mlen ;
              }
            }
            else
            {
              for ( k = 0 ; k < req->glen[2] ; k++ )
              {
                for ( ig = 0 ; ig < mlen ; ig++ )
                {
                  bcopy(&(rbuf[typelen*(ig+req->glen[0]*(jg+k*req->glen[1]))]),
                        &(pbuf[cursor]),
                        typelen) ;
                  cursor += typelen ;
                }
              }
            }
            break ;
          case IO3D_JIK :
            for ( ig = 0 ; ig < mlen ; ig++ )
            {
              for ( k = 0 ; k < req->glen[2] ; k++ )
              {
                bcopy(&(rbuf[typelen*(jg+req->glen[0]*(ig+k*req->glen[1]))]),
                      &(pbuf[cursor]),
                      typelen) ;
                cursor += typelen ;
              }
            }
            break ;
          case IO3D_KIJ :
            for ( ig = 0 ; ig < mlen ; ig++ )
            {
              for ( k = 0 ; k < req->glen[0] ; k++ )
              {
                bcopy(&(rbuf[typelen*(k+req->glen[0]*(ig+jg*req->glen[1]))]),
                      &(pbuf[cursor]),
                      typelen) ;
                cursor += typelen ;
              }
            }
            break ;
          case IO3D_IKJ :
            for ( ig = 0 ; ig < mlen ; ig++ )
            {
              for ( k = 0 ; k < req->glen[1] ; k++ )
              {
                bcopy(&(rbuf[typelen*(ig+req->glen[0]*(k+jg*req->glen[1]))]),
                      &(pbuf[cursor]),
                      typelen) ;
                cursor += typelen ;
              }
            }
            break ;

        }
      }
    }
#endif
    mdest = rsl_c_comp2phys_proc( P ) ;
    mtag = MTYPE_FROMTO( MSG_READ_RESPONSE, rsl_myproc, mdest ) ;
    msglen = sizeof( resp ) ;

#ifndef T3D
     { int i, j  ;
       for ( i = 0 ; i < msglen ; i++ )
       {
         j = j + pbuf[i] ;
       }
       dumdebug(j) ;
     }
#endif


    if ( rsl_myproc == mdest )
    {
      bcopy( &resp, resp_me, msglen ) ;
      *pbuf_me = pbuf ;
    }
    else
    {
      RSL_SEND( &resp, msglen, mtag, mdest ) ;
      msglen = resp.tofollow ;
      RSL_SEND( pbuf, msglen, mtag, mdest ) ;
      RSL_FREE( pbuf ) ;
    }
  }

  RSL_FREE (iptlst) ;   /* 20020524 */
  RSL_FREE (jptlst) ;   /* 20020524 */
  RSL_FREE( rbuf ) ;
  return(0) ;
}

static int wrt_sock_err = 0 ;

int
handle_write_request( req, nelem, psize_me, pbuf_me )
  rsl_write_req_t * req ;
  int nelem ;
  int psize_me ;
  char * pbuf_me ;
{
  int dim, i, k, ig, jg, nbytes ;
  int columnelems, typelen, len, cursor  ;
  int P ;
  int minelems, majelems ;
  int msglen, mtag, mtag2, mdest, mfrom ;
  int mlen, nlen ;
  rsl_read_resp_t resp ;
  int psize[ RSL_MAXPROC ] ;    /* size of messages to each processor */
  float * pr , * qr ;
  char * wbuf ;
  char *pbuf ;
  rsl_point_t *domain ;
  int is_write, ie_write, js_write, je_write ;
  int in_write ;

  typelen = elemsize( req->type ) ;
  nbytes = typelen * nelem ;
  wbuf = RSL_MALLOC( char, nbytes ) ;  

  mlen = domain_info[req->domain].len_m ;
  nlen = domain_info[req->domain].len_n ;
  domain = domain_info[req->domain].domain ;

  /* global record is now stored -- ship it out */
  switch ( req->iotag )
  {
  case IO2D_IJ :
  case IO2D_IJ_RAW :
  case IO2D_IJ_PORTAL :
  case IO2D_IJ_88 :
    columnelems = 1 ;
    minelems = req->glen[0] ;
    majelems = req->glen[1] ;
    break ;
  case IO2D_JI :
  case IO2D_JI_RAW :
  case IO2D_JI_PORTAL :
  case IO2D_JI_88 :
    columnelems = 1 ;
    minelems = req->glen[1] ;
    majelems = req->glen[0] ;
    break ;
  case IO3D_IJK :
  case IO3D_IJK_RAW :
  case IO3D_IJK_PORTAL :
  case IO3D_IJK_88 :
    columnelems = req->glen[2] ;
    minelems = req->glen[0] ;
    majelems = req->glen[1] ;
    break ;
  case IO3D_JIK :
  case IO3D_JIK_RAW :
  case IO3D_JIK_PORTAL :
  case IO3D_JIK_88 :
    columnelems = req->glen[2] ;
    minelems = req->glen[1] ;
    majelems = req->glen[0] ;
    break ;
  case IO3D_KIJ :
    columnelems = req->glen[0] ;
    minelems = req->glen[1] ;
    majelems = req->glen[2] ;
    break ;
  case IO3D_IKJ :
    columnelems = req->glen[1] ;
    minelems = req->glen[0] ;
    majelems = req->glen[2] ;
    break ;
  default:
    RSL_TEST_ERR(1,"handle_write_request: unknown data tag") ;
  }

  RSL_TEST_ERR( majelems <= 0, "Major dim spec on write is zero or less.") ;
  RSL_TEST_ERR( minelems <= 0, "Minor dim spec on write is zero or less.") ;
  if ( majelems > nlen )
  { sprintf(mess,"Major dim spec on write (%d) greater than global domain defini tion in that dimension (%d)\n",majelems,nlen) ;
    RSL_TEST_ERR(1,mess) ; }
  if ( minelems > mlen )
  { sprintf(mess,"Minor dim spec on write (%d) greater than global domain defini tion in that dimension (%d)\n",minelems,mlen) ;
    RSL_TEST_ERR(1,mess) ; }

  /*  figure out sizes for each processor */
  pbuf = NULL ;
  for ( i = 0 ; i < rsl_nproc_all ; i++ )       /* 95/02/22 */
  {
    psize[i] = (regular_decomp)?(4*sizeof(int)):0 ;
  }
  for ( jg = 0 ; jg < majelems ; jg++ )
  {
    for ( ig = 0 ; ig < minelems ; ig++ )
    {
      psize[domain[INDEX_2(jg,ig,mlen)].P] += columnelems * typelen ;
    }
  }

  for ( P = 0 ; P < rsl_nproc_all ; P++ )       /* 95/02/22 */
  {
    cursor = 0 ;
    mdest = rsl_c_comp2phys_proc( P ) ;
    if ( rsl_myproc != mdest )
    {
#ifdef RSL_SYNCIO
      /* send a short "go ahead" message */
      msglen = 1 ;
      mfrom = mdest ;
      mtag2 = MTYPE_FROMTO( MSG_WRITE_COMPUTE_RESPONSE, rsl_myproc, mfrom ) ;
      RSL_SEND( " ", msglen, mtag2, mfrom ) ;
#endif
      msglen = psize[P] ;
      pbuf = RSL_MALLOC( char, msglen ) ;
      mfrom = mdest ;
      mtag2 = MTYPE_FROMTO( MSG_WRITE_COMPUTE_RESPONSE, mfrom, rsl_myproc ) ;
      RSL_RECV( pbuf, msglen, mtag2 ) ;
    }
    else
    {
      sprintf(mess,"psize_me (%d) != psize[P] (%d)", psize_me,psize[P]) ;
      RSL_TEST_ERR( psize_me != psize[P], mess ) ;
      msglen = psize_me ;
      pbuf = pbuf_me ;
    }

    if ( regular_decomp )
    {

      bcopy( &(pbuf[cursor]), &is_write, sizeof(int) ) ; cursor += sizeof(int) ;
      bcopy( &(pbuf[cursor]), &ie_write, sizeof(int) ) ; cursor += sizeof(int) ;
      bcopy( &(pbuf[cursor]), &js_write, sizeof(int) ) ; cursor += sizeof(int) ;
      bcopy( &(pbuf[cursor]), &je_write, sizeof(int) ) ; cursor += sizeof(int) ;

      in_write = ie_write - is_write + 1 ;

      for ( jg = js_write ; jg <= je_write ; jg++ )
      {
        switch ( req->iotag )
        {
          case IO2D_IJ :
          case IO2D_IJ_RAW :
          case IO2D_IJ_PORTAL :
          case IO2D_IJ_88 :
            if ( req->type == RSL_REAL )
            {
               ig = is_write ;
               VRCOPY ( &(pbuf[cursor]),
                       &(wbuf[typelen*(ig+jg*req->glen[0])]),
                       &in_write ) ;
               cursor += in_write*typelen ;
            }
            else
            {
              for ( ig = is_write ; ig <= ie_write ; ig++ )
              {
                bcopy(&(pbuf[cursor]),
                      &(wbuf[typelen*(ig+jg*req->glen[0])]),
                      typelen) ;
                cursor += typelen ;
              }
            }
            break ;
          case IO2D_JI :
          case IO2D_JI_RAW :
          case IO2D_JI_PORTAL :
          case IO2D_JI_88 :
            for ( ig = is_write ; ig <= ie_write ; ig++ )
            {
              bcopy(&(pbuf[cursor]),
                    &(wbuf[typelen*(jg+ig*req->glen[0])]),
                    typelen) ;
              cursor += typelen ;
            }
            break ;
          case IO3D_IJK :
          case IO3D_IJK_RAW :
          case IO3D_IJK_PORTAL :
          case IO3D_IJK_88 :
            if ( req->type == RSL_REAL )
            {
              ig = is_write ;
              for ( k = 0 ; k < req->glen[2] ; k++ )   /* note reversal of i and k on vpp */
              {
                VRCOPY ( &(pbuf[cursor]),
                        &(wbuf[typelen*(ig+req->glen[0]*(jg+k*req->glen[1]))]),
                        &in_write ) ;
                cursor += typelen*in_write ;
              }
            }
            else
            {
              for ( k = 0 ; k < req->glen[2] ; k++ )   /* note reversal of i and k on vpp */
              {
                for ( ig = is_write ; ig <= ie_write ; ig++ )
                {
                  bcopy(&(pbuf[cursor]),
                        &(wbuf[typelen*(ig+req->glen[0]*(jg+k*req->glen[1]))]),
                        typelen) ;
                  cursor += typelen ;
                }
              }
            }
            break ;
          case IO3D_JIK :
          case IO3D_JIK_RAW :
          case IO3D_JIK_PORTAL :
          case IO3D_JIK_88 :
            for ( k = 0 ; k < req->glen[2] ; k++ )
            {
              for ( ig = is_write ; ig <= ie_write ; ig++ )
              {
                bcopy(&(pbuf[cursor]),
                      &(wbuf[typelen*(jg+req->glen[0]*(ig+k*req->glen[1]))]),
                      typelen) ;
                cursor += typelen ;
              }
            }
            break ;
          case IO3D_KIJ :
            for ( ig = is_write ; ig <= ie_write ; ig++ )
            {
              for ( k = 0 ; k < req->glen[0] ; k++ )
              {
                bcopy(&(pbuf[cursor]),
                      &(wbuf[typelen*(k+req->glen[0]*(ig+jg*req->glen[1]))]),
                      typelen) ;
                cursor += typelen ;
              }
            }
            break ;
          case IO3D_IKJ :
            for ( ig = is_write ; ig <= ie_write ; ig++ )
            {
              for ( k = 0 ; k < req->glen[1] ; k++ )
              {
                bcopy(&(pbuf[cursor]),
                      &(wbuf[typelen*(ig+req->glen[0]*(k+jg*req->glen[1]))]),
                      typelen) ;
                cursor += typelen ;
              }
            }
            break ;
        }
      }
    }
    else
    {
      for ( jg = 0 ; jg < majelems ; jg++ )
      {
        for ( ig = 0 ; ig < minelems ; ig++ )
        {
          if ( domain[INDEX_2(jg,ig,mlen)].P == P )
          {
            switch ( req->iotag )
            {
            case IO2D_IJ :
            case IO2D_IJ_RAW :
            case IO2D_IJ_PORTAL :
            case IO2D_IJ_88 :
              bcopy(&(pbuf[cursor]),
                    &(wbuf[typelen*(ig+jg*req->glen[0])]),
                    typelen) ;
              cursor += typelen ;
              break ;
            case IO2D_JI :
            case IO2D_JI_RAW :
            case IO2D_JI_PORTAL :
            case IO2D_JI_88 :
              bcopy(&(pbuf[cursor]),
                    &(wbuf[typelen*(jg+ig*req->glen[0])]),
                    typelen) ;
              cursor += typelen ;
              break ;
            case IO3D_IJK :
            case IO3D_IJK_RAW :
            case IO3D_IJK_PORTAL :
            case IO3D_IJK_88 :
              for ( k = 0 ; k < req->glen[2] ; k++ )
              {
                bcopy(&(pbuf[cursor]),
                      &(wbuf[typelen*(ig+req->glen[0]*(jg+k*req->glen[1]))]),
                      typelen) ;
                cursor += typelen ;
              }
              break ;
            case IO3D_JIK :
            case IO3D_JIK_RAW :
            case IO3D_JIK_PORTAL :
            case IO3D_JIK_88 :
              for ( k = 0 ; k < req->glen[2] ; k++ )
              {
                bcopy(&(pbuf[cursor]),
                      &(wbuf[typelen*(jg+req->glen[0]*(ig+k*req->glen[1]))]),
                      typelen) ;
                cursor += typelen ;
              }
              break ;
            case IO3D_KIJ :
              for ( k = 0 ; k < req->glen[0] ; k++ )
              {
                bcopy(&(pbuf[cursor]),
                      &(wbuf[typelen*(k+req->glen[0]*(ig+jg*req->glen[1]))]),
                      typelen) ;
                cursor += typelen ;
              }
              break ;
            case IO3D_IKJ :
              for ( k = 0 ; k < req->glen[1] ; k++ )
              {
                bcopy(&(pbuf[cursor]),
                      &(wbuf[typelen*(ig+req->glen[0]*(k+jg*req->glen[1]))]),
                      typelen) ;
                cursor += typelen ;
              }
              break ;

            }
          }
        }
      }
    }
    if ( rsl_myproc != rsl_c_comp2phys_proc( P ) ) 
    {
      RSL_FREE( pbuf ) ;   /* the monitor frees its own buffer outside
                              this routine */
    }
  }

  /* mark the unit as needing to be flushed */
  if ( ! req->internal )
  {
    unit_written[ req->unit - 1 ] = (unsigned char) 1 ;
  }

/* start 981228 AFWA_IO */
/* need some kind of graceful failure if the node runs out of memory */
  if ( rsl_buffer_output && ! req->internal )
  {
    if ( write_buffer_head == NULL && write_buffer_tail == NULL )
    {
      write_buffer_head = RSL_MALLOC( rsl_write_buffer_struct_t, 1 ) ;
      write_buffer_tail = write_buffer_head ;
    }
    else
    {
      write_buffer_tail->next = RSL_MALLOC( rsl_write_buffer_struct_t, 1 ) ;
      write_buffer_tail = write_buffer_tail->next ;
    }
    write_buffer_tail->req       = *req ;
    write_buffer_tail->nelem     = nelem ;
    write_buffer_tail->buf       = RSL_MALLOC( char, nelem * elemsize( req->type ) ) ;
    bcopy( wbuf, write_buffer_tail->buf, nelem * elemsize( req->type ) ) ;
  }
  else
  {
    send_to_output_device( req, wbuf, nelem ) ;
  }
  RSL_FREE( wbuf ) ;
  return(0) ;
}


/* these routines added for MM5 v3 */


RSL_WRITE_1D_DATA( unit_p,
                   buf,
                   nbuf_p,
                   type_p  )
  int_p  unit_p ;
  char * buf             ;  int_p nbuf_p   ;
  int_p  type_p ;
{
  rsl_write_req_t req ;
  int nelem ;
  int icurs ;
  char * wbuf ;
  int i_am_monitor ;
  int type, typelen ;

  RSL_C_IAMMONITOR( &i_am_monitor ) ;
  if ( ! i_am_monitor ) return ;

  nelem = *nbuf_p ;
  type = *type_p ;
  typelen = elemsize( type ) ;
          
  req.internal     = 0 ;
  req.request_type = RSL_WRITE_REQUEST ;
  req.request_mode = MSG_IO_FORTRAN    ;
  req.unit         = *unit_p           ;
  req.unit_p       = unit_p            ;
  req.iotag        = IO_REPL           ;
  req.type         = type     ;

  wbuf             = RSL_MALLOC( char, nelem*typelen ) ;

  icurs = 0 ;
  bcopy( buf  , wbuf, nelem*typelen ) ;

  if ( rsl_buffer_output && ! req.internal )
  {
    if ( write_buffer_head == NULL && write_buffer_tail == NULL )
    {
      write_buffer_head = RSL_MALLOC( rsl_write_buffer_struct_t, 1 ) ;
      write_buffer_tail = write_buffer_head ;
    }
    else
    {
      write_buffer_tail->next = RSL_MALLOC( rsl_write_buffer_struct_t, 1 ) ;
      write_buffer_tail = write_buffer_tail->next ;
    }
    write_buffer_tail->req       = req ;
    write_buffer_tail->nelem     = nelem ;
    write_buffer_tail->buf       = RSL_MALLOC( char, nelem*typelen ) ;
    bcopy( wbuf, write_buffer_tail->buf, nelem*typelen ) ;
  }
  else
  {
    send_to_output_device( &req, wbuf, nelem ) ;
  }

  RSL_FREE( wbuf ) ;
}

send_to_output_device( req, wbuf, nelem )
  rsl_write_req_t * req ;
  char * wbuf ;
  int nelem ;
{

  int nbytes, typelen, minelems, majelems, columnelems ;
  int ig, jg ;

  typelen = elemsize( req->type ) ;
  nbytes = typelen * nelem ;

  /* global record is now stored -- ship it out */
  switch ( req->iotag )
  {
  case IO2D_IJ :
  case IO2D_IJ_RAW :
  case IO2D_IJ_PORTAL :
  case IO2D_IJ_88 :
    columnelems = 1 ;
    minelems = req->glen[0] ;
    majelems = req->glen[1] ;
    break ;
  case IO2D_JI :
  case IO2D_JI_RAW :
  case IO2D_JI_PORTAL :
  case IO2D_JI_88 :
    columnelems = 1 ;
    minelems = req->glen[1] ;
    majelems = req->glen[0] ;
    break ;
  case IO3D_IJK :
  case IO3D_IJK_RAW :
  case IO3D_IJK_PORTAL :
  case IO3D_IJK_88 :
    columnelems = req->glen[2] ;
    minelems = req->glen[0] ;
    majelems = req->glen[1] ;
    break ;
  case IO3D_JIK :
  case IO3D_JIK_RAW :
  case IO3D_JIK_PORTAL :
  case IO3D_JIK_88 :
    columnelems = req->glen[2] ;
    minelems = req->glen[1] ;
    majelems = req->glen[0] ;
    break ;
  case IO3D_KIJ :
    columnelems = req->glen[0] ;
    minelems = req->glen[1] ;
    majelems = req->glen[2] ;
    break ;
  case IO3D_IKJ :
    columnelems = req->glen[1] ;
    minelems = req->glen[0] ;
    majelems = req->glen[2] ;
    break ;
  case IO_REPL :
    break ;
  default:
    RSL_TEST_ERR(1,"handle_write_request: unknown data tag") ;
  }

  if ( req->request_mode == MSG_IO_FORTRAN )
  {

  /* call fortran to write a record to the named unit */
    if ( req->internal )
    {
      bcopy( wbuf, req->unit_p, nbytes ) ;
    }
    else
    {
      /* call fortran to write a record to the named unit */
      switch ( req->type )
      {
      case RSL_REAL :
        FORT_REALWRITE ( &(req->unit), wbuf, &nelem ) ;
        break ;
      case RSL_INTEGER :
        FORT_INTWRITE ( &(req->unit), wbuf, &nelem ) ;
        break ;
#ifndef T3D
      case RSL_DOUBLE :
        FORT_DOUBLEWRITE ( &(req->unit), wbuf, &nelem ) ;
        break ;
#endif
      case RSL_COMPLEX :
        FORT_COMPLEXWRITE ( &(req->unit), wbuf, &nelem ) ;
        break ;
      case RSL_CHARACTER :
#ifndef T3D
        FORT_CHARACTERWRITE ( &(req->unit), wbuf, &nelem ) ;
#else
        {
          _fcd x ;
          x = _cptofcd( wbuf, nelem ) ;
          FORT_CHARACTERWRITE ( &(req->unit), x, &nelem ) ;
        }
#endif
        break ;
      default :
        RSL_TEST_WRN(1,"write operation not implemented for this data type") ;
      }
    }
  }
  else
  if ( req->request_mode == MSG_IO_SOCKET )
  {
    /* nbytes contains the number of bytes to be written,
       wbuf is the buffer to be written,
       req->unit is the socket id */

    int cw ;
    struct hdr_info_3d
    {
       int  typelen, xdim, ydim, zdim;
    } wbuf_header ;

    if ( req->request_mode2 == MSG_MODE2_RAW )
    {
      if ( write_sock( req->unit, wbuf, nbytes ) < 0 )
      {
        perror("writing on socket");
        RSL_TEST_WRN(1,"") ;
      }
    }
    else
    if ( req->request_mode2 == MSG_MODE2_FORTRAN )
    {
      /* simulate control words at beginning and end */
      cw = nbytes ;

      if (write_sock(req->unit , &cw, 4)  < 0)
      {
        perror("writing first control word on socket");
        RSL_TEST_WRN(1,"") ;
      }
      if (write_sock(req->unit , wbuf, nbytes )  < 0)
      {
        perror("writing wbuf on socket");
        RSL_TEST_WRN(1,"") ;
      }
      if (write_sock(req->unit , &cw, 4)  < 0)
      {
        perror("writing second control word on socket");
        RSL_TEST_WRN(1,"") ;
      }
    }
    else
    if ( req->request_mode2 == MSG_MODE2_PORTAL )
    {
      wbuf_header.typelen = typelen ;
      wbuf_header.xdim  = minelems ;
      wbuf_header.ydim  = majelems ;
      wbuf_header.zdim  = columnelems ;
      if (write_sock(req->unit , &wbuf_header , sizeof( wbuf_header ) )  < 0)
      {
        perror("writing wbuf header on socket");
        RSL_TEST_WRN(1,"") ;
      }
      if (write_sock(req->unit , wbuf , nbytes )  < 0)
      {
        perror("writing wbuf header on socket");
        RSL_TEST_WRN(1,"") ;
      }
    }
    else
    if ( req->request_mode2 == MSG_MODE2_88 )
    {
      int x,y,z ;
      char outline[256] ;
      for ( z = 0; z < columnelems ; z++ )
      {
        sprintf(outline,"%d %d\n",majelems,minelems) ;
        if (write_sock(req->unit , outline , strlen( outline ) )  < 0)
        {
          if ( ! wrt_sock_err )
          {
            wrt_sock_err = 1 ;
            perror("writing wbuf header on socket");
            RSL_TEST_WRN(1,"") ;
          }
        }
        else
        {
        for ( ig = 0 ; ig < minelems ; ig++ )
        {
          for ( jg = 0 ; jg < majelems ; jg++ )
          {
            if ( req->type == RSL_REAL )
            {
               float a ;
               bcopy(&(wbuf[typelen*(jg+ig*req->glen[0])]),&a,sizeof(float)) ;
               sprintf(outline,"%g\n",a) ;
            }
            else if ( req->type == RSL_DOUBLE )
            {
               double a ;
               bcopy(&(wbuf[typelen*(jg+ig*req->glen[0])]),&a,sizeof(double)) ;
               sprintf(outline,"%g\n",a) ;
            }
            else if ( req->type == RSL_INTEGER )
            {
               int a ;
               bcopy(&(wbuf[typelen*(jg+ig*req->glen[0])]),&a,sizeof(int)) ;
               sprintf(outline,"%d\n",a) ;
            }
            if (write_sock(req->unit , outline , strlen(outline) )  < 0)
            {
              if ( ! wrt_sock_err )
              {
                wrt_sock_err = 1 ;
                perror("writing wbuf header on socket");
                RSL_TEST_WRN(1,"") ;
              }
            }
          }
        }
        }
      }
    }
    else
    {
      sprintf(mess, "Unknown request request_mode2: %d\n",
              req->request_mode2 ) ;
      RSL_TEST_ERR(1,mess) ;
    }
  }
  else
  {
    sprintf(mess, "Unknown request request_mode: %d\n",
                  req->request_mode ) ;
    RSL_TEST_ERR(1,mess) ;
  }
}

RSL_OUTPUT_BUFFER_WRITE ()
{
  int i_am_monitor ;
  rsl_write_buffer_struct_t * p, * old ;
  RSL_C_IAMMONITOR( &i_am_monitor ) ;

  if ( rsl_buffer_output && i_am_monitor && write_buffer_head != NULL )
  {
    for ( p = write_buffer_head ; p ; )
    {
      send_to_output_device( &(p->req), p->buf, p->nelem ) ;
      RSL_FREE( p->buf ) ;
      old = p ;
      p = p->next ;
      RSL_FREE( old ) ;
    }
  }
  write_buffer_head = NULL  ;
  write_buffer_tail = NULL  ;
}

RSL_OUTPUT_BUFFER_YES ()
{
  rsl_buffer_output = 1 ;
}
RSL_OUTPUT_BUFFER_NO ()
{
  rsl_buffer_output = 0 ;
}

RSL_IO_NODE_YES ()
{
  rsl_io_node = 1 ;
}
RSL_IO_NODE_NO ()
{
  rsl_io_node = 0 ;
}



#include <signal.h>

write_sock( sd, buf, n )
  int sd ;
  char * buf ;
  int n ;
{
  static int errseen = 0 ;
  int todo, n_written ;
  char * p ;

  signal( SIGPIPE, SIG_IGN ) ;  /* if the receiver dies, we should cont */
  todo = n ;
  p = buf ;
  if ( ! errseen )
    do {
      if ((n_written = write(sd, p, todo)) < 0 )
      {
        errseen = 1 ;
        perror("write_sock") ;
        return( n_written ) ;
      }
      p += n_written ;
      todo -= n_written ;
    } while ( todo > 0 ) ;
  signal( SIGPIPE, SIG_DFL ) ;

  return(n) ;
}

/* On vpp from here to remainder of file, we may be bcopying character strings
   so undefine the substution to the vector bcopy */
#if defined(vpp) || defined(vpp2)
#undef bcopy
#endif


RSL_WRITE_MM5V3_SM_HEADER( unit_p,ndim_p,
                        s1_p,s2_p,s3_p,s4_p,
                        e1_p,e2_p,e3_p,e4_p,
                        iwordsize_p,
                        xtime_p,
                        rwordsize_p,
                        staggering_p,     nstaggering_p,
                        ordering_p,       nordering_p,
                        current_date_p,   ncurrent_date_p,
                        name_p,           nname_p,
                        units_p,          nunits_p,
                        description_p,    ndescription_p )
  int_p unit_p ;
  int_p ndim_p ;
  int_p s1_p, s2_p, s3_p, s4_p ;
  int_p e1_p, e2_p, e3_p, e4_p ;
  int_p  iwordsize_p ;
  char * xtime_p ;
  int_p  rwordsize_p ;
#ifndef T3D
  char * staggering_p     ;  int_p nstaggering_p   ;
  char * ordering_p       ;  int_p nordering_p      ;
  char * current_date_p   ;  int_p ncurrent_date_p  ;
  char * name_p           ;  int_p nname_p          ;
  char * units_p          ;  int_p nunits_p         ;
  char * description_p    ;  int_p ndescription_p   ;
#else
  _fcd staggering_p     ;  int_p nstaggering_p   ;
  _fcd ordering_p       ;  int_p nordering_p      ;
  _fcd current_date_p   ;  int_p ncurrent_date_p  ;
  _fcd name_p           ;  int_p nname_p          ;
  _fcd units_p          ;  int_p nunits_p         ;
  _fcd description_p    ;  int_p ndescription_p   ;
#endif
{
  rsl_write_req_t req ;
  int nelem ;
  int iwordsize ;
  int rwordsize ;
  int nstringbytes ;
  int icurs ;
  char * wbuf ;
  int i_am_monitor ;

  RSL_C_IAMMONITOR( &i_am_monitor ) ;
  if ( ! i_am_monitor ) return ;

  iwordsize = *iwordsize_p ;
  rwordsize = *rwordsize_p ;
  nstringbytes =  *nstaggering_p+ *nordering_p+ *ncurrent_date_p
                + *nname_p+ *nunits_p+ *ndescription_p ;
#ifndef T3D
  nelem = 9 * iwordsize + 1 * rwordsize + nstringbytes ;
#else
  nelem = 9 * iwordsize/2 + 1 * rwordsize/2 + nstringbytes ;
#endif

  req.internal     = 0 ;
  req.request_type = RSL_WRITE_REQUEST ;
  req.request_mode = MSG_IO_FORTRAN    ;
  req.unit         = *unit_p           ;
  req.unit_p       = unit_p            ;
  req.iotag        = IO_REPL           ;
  req.type         = RSL_CHARACTER     ;

  wbuf       = RSL_MALLOC( char, nelem ) ;

  icurs = 0 ;
#ifndef T3D
  bcopy( ndim_p, &(wbuf[icurs]), iwordsize ) ;   icurs += iwordsize ;
  bcopy( s1_p  , &(wbuf[icurs]), iwordsize ) ;   icurs += iwordsize ;
  bcopy( s2_p  , &(wbuf[icurs]), iwordsize ) ;   icurs += iwordsize ;
  bcopy( s3_p  , &(wbuf[icurs]), iwordsize ) ;   icurs += iwordsize ;
  bcopy( s4_p  , &(wbuf[icurs]), iwordsize ) ;   icurs += iwordsize ;
  bcopy( e1_p  , &(wbuf[icurs]), iwordsize ) ;   icurs += iwordsize ;
  bcopy( e2_p  , &(wbuf[icurs]), iwordsize ) ;   icurs += iwordsize ;
  bcopy( e3_p  , &(wbuf[icurs]), iwordsize ) ;   icurs += iwordsize ;
  bcopy( e4_p  , &(wbuf[icurs]), iwordsize ) ;   icurs += iwordsize ;
# ifdef SWAPBYTES
  rsl_swapbytes( wbuf, iwordsize, 9 ) ;
# endif
#else
#ifdef crayx1
  { int i ;
#else
  { short i ;
#endif
    i = *ndim_p ; bcopy( &i, &(wbuf[icurs]), iwordsize/2 ) ;   icurs += iwordsize/2 ;
    i = *s1_p   ; bcopy( &i, &(wbuf[icurs]), iwordsize/2 ) ;   icurs += iwordsize/2 ;
    i = *s2_p   ; bcopy( &i, &(wbuf[icurs]), iwordsize/2 ) ;   icurs += iwordsize/2 ;
    i = *s3_p   ; bcopy( &i, &(wbuf[icurs]), iwordsize/2 ) ;   icurs += iwordsize/2 ;
    i = *s4_p   ; bcopy( &i, &(wbuf[icurs]), iwordsize/2 ) ;   icurs += iwordsize/2 ;
    i = *e1_p   ; bcopy( &i, &(wbuf[icurs]), iwordsize/2 ) ;   icurs += iwordsize/2 ;
    i = *e2_p   ; bcopy( &i, &(wbuf[icurs]), iwordsize/2 ) ;   icurs += iwordsize/2 ;
    i = *e3_p   ; bcopy( &i, &(wbuf[icurs]), iwordsize/2 ) ;   icurs += iwordsize/2 ;
    i = *e4_p   ; bcopy( &i, &(wbuf[icurs]), iwordsize/2 ) ;   icurs += iwordsize/2 ;
  }
# ifdef SWAPBYTES
  rsl_swapbytes( wbuf, iwordsize/2, 9 ) ;
# endif
#endif

#ifndef T3D
  bcopy( xtime_p  , &(wbuf[icurs]), rwordsize ) ;  
# ifdef SWAPBYTES
  rsl_swapbytes( &(wbuf[icurs]), rwordsize, 1 ) ;
# endif
  icurs += rwordsize ;
#else
  { float x ; double y ;
    bcopy( xtime_p, &y, rwordsize ) ;
    x = y ;
    bcopy( &x  , &(wbuf[icurs]), rwordsize/2 ) ;
  }
# ifdef SWAPBYTES
  rsl_swapbytes( &(wbuf[icurs]), rwordsize/2, 1 ) ;
# endif
  icurs += rwordsize/2 ;
#endif

#ifndef T3D
  bcopy( staggering_p   , &(wbuf[icurs]),
                                 *nstaggering_p ) ;          icurs += *nstaggering_p ;
  bcopy( ordering_p     , &(wbuf[icurs]),
                                 *nordering_p    ) ;         icurs += *nordering_p   ;
  bcopy( current_date_p , &(wbuf[icurs]),
                                 *ncurrent_date_p    ) ;     icurs += *ncurrent_date_p   ;
  bcopy( name_p         , &(wbuf[icurs]),
                                 *nname_p    ) ;             icurs += *nname_p   ;
  bcopy( units_p        , &(wbuf[icurs]),
                                 *nunits_p    ) ;            icurs += *nunits_p   ;
  bcopy( description_p  , &(wbuf[icurs]),
                                 *ndescription_p    ) ;      icurs += *ndescription_p   ;
#else
  bcopy( _fcdtocp( staggering_p )  , &(wbuf[icurs]),
                                 *nstaggering_p ) ;          icurs += *nstaggering_p ;
  bcopy( _fcdtocp( ordering_p )    , &(wbuf[icurs]),
                                 *nordering_p    ) ;         icurs += *nordering_p   ;
  bcopy( _fcdtocp( current_date_p ), &(wbuf[icurs]),
                                 *ncurrent_date_p    ) ;     icurs += *ncurrent_date_p   ;
  bcopy( _fcdtocp( name_p )        , &(wbuf[icurs]),
                                 *nname_p    ) ;             icurs += *nname_p   ;
  bcopy( _fcdtocp( units_p )       , &(wbuf[icurs]),
                                 *nunits_p    ) ;            icurs += *nunits_p   ;
  bcopy( _fcdtocp( description_p ) , &(wbuf[icurs]),
                                 *ndescription_p    ) ;      icurs += *ndescription_p   ;
#endif

  if ( rsl_buffer_output && ! req.internal )
  {
    if ( write_buffer_head == NULL && write_buffer_tail == NULL )
    {
      write_buffer_head = RSL_MALLOC( rsl_write_buffer_struct_t, 1 ) ;
      write_buffer_tail = write_buffer_head ;
    }
    else
    {
      write_buffer_tail->next = RSL_MALLOC( rsl_write_buffer_struct_t, 1 ) ;
      write_buffer_tail = write_buffer_tail->next ;
    }
    write_buffer_tail->req       = req ;
    write_buffer_tail->nelem     = nelem ;
    write_buffer_tail->buf       = RSL_MALLOC( char, nelem ) ;
    bcopy( wbuf, write_buffer_tail->buf, nelem ) ;
  }
  else
  {
    send_to_output_device( &req, wbuf, nelem ) ;
  }

  RSL_FREE( wbuf ) ;
}

rsl_swapbytes ( buf, wordsz, nwords )
  char * buf ;
  int wordsz, nwords ;
{
  char tbuf[8] ;
  int i ;

  if ( wordsz == 4 )
  {
    for ( i = 0 ; i < nwords*wordsz ; i += wordsz  )   
    {
      tbuf[0] = buf[3+i] ;
      tbuf[1] = buf[2+i] ;
      tbuf[2] = buf[1+i] ;
      tbuf[3] = buf[0+i] ;
      buf[0+i] = tbuf[0] ;
      buf[1+i] = tbuf[1] ;
      buf[2+i] = tbuf[2] ;
      buf[3+i] = tbuf[3] ;
    }
  }
  else if ( wordsz == 8 )
  {
    for ( i = 0 ; i < nwords*wordsz ; i += wordsz )   
    {
      tbuf[0] = buf[7+i] ;
      tbuf[1] = buf[6+i] ;
      tbuf[2] = buf[5+i] ;
      tbuf[3] = buf[4+i] ;
      tbuf[4] = buf[3+i] ;
      tbuf[5] = buf[2+i] ;
      tbuf[6] = buf[1+i] ;
      tbuf[7] = buf[0+i] ;
      buf[0+i] = tbuf[0] ;
      buf[1+i] = tbuf[1] ;
      buf[2+i] = tbuf[2] ;
      buf[3+i] = tbuf[3] ;
      buf[4+i] = tbuf[4] ;
      buf[5+i] = tbuf[5] ;
      buf[6+i] = tbuf[6] ;
      buf[7+i] = tbuf[7] ;
    }
  }
  else
  {
    sprintf(mess,"invalid argument wordsz = %d",wordsz) ;
    RSL_TEST_ERR(1,mess) ;
  }
}

RSL_WRITE_MM5V3_BIG_HEADER( unit_p,
                         ibuf,nibuf_p,
                         rbuf,nrbuf_p,
                         cb1,ncb1_p, 
                         cb2,ncb2_p,
                         iwordsize_p,rwordsize_p )
  int_p  unit_p ;
  char * ibuf             ;  int_p nibuf_p   ;
  char * rbuf             ;  int_p nrbuf_p   ;
#ifndef T3D
  char * cb1              ;  int_p ncb1_p    ;
  char * cb2              ;  int_p ncb2_p    ;
#else
  _fcd   cb1              ;  int_p ncb1_p    ;
  _fcd   cb2              ;  int_p ncb2_p    ;
#endif
  int_p  iwordsize_p ;
  int_p  rwordsize_p ;
{
  rsl_write_req_t req ;
  int nelem ;
  int iwordsize ;
  int rwordsize ;
  int nstringbytes ;
  int icurs ;
  char * wbuf ;
  int i_am_monitor ;

  RSL_C_IAMMONITOR( &i_am_monitor ) ;
  if ( ! i_am_monitor ) return ;

  iwordsize = *iwordsize_p ;
  rwordsize = *rwordsize_p ;

#ifndef T3D
  nelem = *nibuf_p * iwordsize + 
          *nrbuf_p * rwordsize +
          *ncb1_p + *ncb2_p ;
#else
  nelem = *nibuf_p * iwordsize /2 + 
          *nrbuf_p * rwordsize /2 +
          *ncb1_p + *ncb2_p ;
#endif
          
  req.internal     = 0 ;
  req.request_type = RSL_WRITE_REQUEST ;
  req.request_mode = MSG_IO_FORTRAN    ;
  req.unit         = *unit_p           ;
  req.unit_p       = unit_p            ;
  req.iotag        = IO_REPL           ;
  req.type         = RSL_CHARACTER     ;

  wbuf       = RSL_MALLOC( char, nelem ) ;

  icurs = 0 ;
#ifdef SWAPBYTES
  rsl_swapbytes( ibuf, iwordsize, *nibuf_p ) ;
#endif
#ifndef T3D
  bcopy( ibuf  , &(wbuf[icurs]), *nibuf_p * iwordsize ) ;
  icurs += *nibuf_p * iwordsize ;
#else
#ifdef crayx1
  { long *p ; int *q ; int i ;
    p = (long *) ibuf ;
    q = (int *) ibuf ;
#else
  { long *p ; short *q ; int i ;
    p = (long *) ibuf ;
    q = (short *) ibuf ;
#endif
    for ( i = 0 ; i < *nibuf_p ; i++ )
    {
      *q = *p ; q++ ; p++ ;
    }
  }
  bcopy( ibuf  , &(wbuf[icurs]), *nibuf_p * iwordsize /2 ) ;
  icurs += *nibuf_p * iwordsize / 2 ;
#endif
#ifdef SWAPBYTES
  rsl_swapbytes( rbuf, rwordsize, *nrbuf_p ) ;
#endif
#ifndef T3D
  bcopy( rbuf  , &(wbuf[icurs]), *nrbuf_p * rwordsize ) ;
  icurs += *nrbuf_p * rwordsize ;
#else
  { double *p ; float *q ; int i ;
    p = (double *) rbuf ;
    q = (float *) rbuf ;
    for ( i = 0 ; i < *nrbuf_p ; i++ )
    {
      *q = *p ; q++ ; p++ ;
    }
  }
  bcopy( rbuf  , &(wbuf[icurs]), *nrbuf_p * rwordsize /2 ) ;
  icurs += *nrbuf_p * rwordsize / 2 ;
#endif
#ifndef T3D
  bcopy( cb1   , &(wbuf[icurs]), *ncb1_p   ) ;   icurs += *ncb1_p ;
  bcopy( cb2   , &(wbuf[icurs]), *ncb2_p   ) ;   icurs += *ncb2_p ;
#else
  bcopy( _fcdtocp( cb1 ), &(wbuf[icurs]), *ncb1_p   ) ;   icurs += *ncb1_p ;
  bcopy( _fcdtocp( cb2 ), &(wbuf[icurs]), *ncb2_p   ) ;   icurs += *ncb2_p ;
#endif
  if ( rsl_buffer_output && ! req.internal )
  {
    if ( write_buffer_head == NULL && write_buffer_tail == NULL )
    {
      write_buffer_head = RSL_MALLOC( rsl_write_buffer_struct_t, 1 ) ;
      write_buffer_tail = write_buffer_head ;
    }
    else
    {
      write_buffer_tail->next = RSL_MALLOC( rsl_write_buffer_struct_t, 1 ) ;
      write_buffer_tail = write_buffer_tail->next ;
    }
    write_buffer_tail->req       = req ;
    write_buffer_tail->nelem     = nelem ;
    write_buffer_tail->buf       = RSL_MALLOC( char, nelem ) ;
    bcopy( wbuf, write_buffer_tail->buf, nelem ) ;
  }
  else
  {
    send_to_output_device( &req, wbuf, nelem ) ;
  }
  RSL_FREE( wbuf ) ;
}
