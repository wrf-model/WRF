#define KLUDGE_20000821

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
  RSL_TO_OH_INFO -- Get the next cell in a packing sequence for forcing.

  Notes:

  See also:

@*/

static rsl_domain_info_t *s_tinfo, *s_oinfo ;
static int s_oig, s_ojg ;
static int s_p, s_t, s_o ;
static int s_msize ;
static struct rsl_hemi_rec * s_q, * s_q1 ;
static int s_nlen_o ;
static int s_mlen_o ;
static rsl_point_t *s_tdomain, *s_odomain ;
static char * s_pointbuf = NULL ;

RSL_TO_OH_INFO ( t_p, o_p, msize_p, seed_p,
                 oig_p, ojg_p, retval_p )
  int_p
     t_p            /* (I) RSL domain descriptor of this hemi. */
    ,o_p            /* (I) RSL domain descriptor of other hemi. */
    ,msize_p        /* (I) Message size in bytes. */
    ,seed_p         /* (I) =1 start the traversal; =0 (zero) continue traversal */
    ,oig_p          /* (O) Global M index of other domain point. */
    ,ojg_p          /* (O) Global N index of other domain point. */
    ,retval_p ;     /* (O) =1 if a valid point returned; =0 (zero) otherwise. */
{
  int kiddex ;
  int P ;
  rsl_hemi_rec_t * q ;
#ifdef KLUDGE_20000821
  rsl_hemi_rec_t * qnuke ;
  rsl_hemi_rec_t * prev ;
#endif
  int p, p1 ;
  int globalhemiPlist[RSL_MAXPROC][RSL_MAXPROC], work[RSL_MAXPROC][RSL_MAXPROC] ;

#ifndef STUBS
  s_msize = *msize_p ;
  s_t = *t_p ;
  s_o = *o_p ;
  RSL_TEST_ERR( s_t < 0 || s_t > RSL_MAXDOMAINS,
    "rsl_ready_bcast: bad 'this hemi' descriptor" ) ;
  RSL_TEST_ERR( s_o < 0 || s_o > RSL_MAXDOMAINS,
    "rsl_ready_bcast: bad 'other hemi' descriptor" ) ;
  RSL_TEST_ERR( s_t == s_o,
    "rsl_ready_bcast: hemispere cannot force itself" ) ;

  s_tinfo = &( domain_info[s_t]) ;
  s_oinfo = &( domain_info[s_o]) ;
  s_mlen_o = s_oinfo->len_m ;
  s_nlen_o = s_oinfo->len_n ;
  s_odomain = s_oinfo->domain ;

  if ( ! s_tinfo->other_hemi_proclist_built )
  {
    if ( *seed_p )
    {
      s_oig = 0 ; s_ojg = 0 ;
      for ( p = 0 ; p < RSL_MAXPROC ; p++ )
      {
#ifdef KLUDGE_20000821
        for ( q = s_tinfo->other_hemi_procbufs[p], prev = NULL ; q ; )
        {
           if ( q->data ) RSL_FREE( q->data ) ;
           qnuke = q ;
           q = q->next ;
           RSL_FREE( qnuke ) ;
        }
#endif
	s_tinfo->other_hemi_procbufs[p] = NULL ;
	s_tinfo->hemi_sendPlist[p] = 0 ;
        for ( p1 = 0 ; p1 < RSL_MAXPROC ; p1++ )
	{
	  globalhemiPlist[p][p1] = 0 ;
	}
      }
    }
    else
    {
      s_oig++ ;
      if ( s_oig >= s_oinfo->len_m )
      {
        s_oig = 0 ;
        s_ojg++ ;
        if ( s_ojg >= s_oinfo->len_n )
        {
	  *retval_p = 0 ;
#ifndef KLUDGE_20000821
	  s_tinfo->other_hemi_proclist_built = 1 ; /* FIX 20000818 JM */
 #endif

/* collapse the list and keep only entries that have data associated */
/* also fill entries for processors I must send to, indicating the number 
   of columns that go to each processors, or zero for processors I don't
   send to */
          for ( p = 0 ; p < RSL_MAXPROC ; p++ )
	  {
	    rsl_hemi_rec_t * prev ;
	    for ( q = s_tinfo->other_hemi_procbufs[p], prev = NULL ; q ; )
	    {
	      if ( q->data == NULL )
	      {
		if ( prev == NULL )
		{
		  s_tinfo->other_hemi_procbufs[p] = q->next ;
		  RSL_FREE(q) ;
		  q = s_tinfo->other_hemi_procbufs[p] ;
		}
		else if ( prev->next == q )
		{
		  prev->next = q->next ;
		  RSL_FREE(q) ;
		  q = prev->next ;
		}
		else
		  RSL_TEST_ERR(1,"internal error") ;
	      }
	      else
	      {
		s_tinfo->hemi_sendPlist[p]++ ;
		prev = q ;
		q = q->next ;
	      }
	    }
	  }

	  /* mpi all to all communication to share matrix of senders/receivers */
	  MPI_Gather( s_tinfo->hemi_sendPlist, RSL_MAXPROC, MPI_INT,
                      globalhemiPlist, RSL_MAXPROC, MPI_INT, 
                      0, rsl_mpi_communicator ) ;
	  /* transpose */
	  for ( p = 0 ; p < RSL_MAXPROC ; p++ )
	    for ( p1 = 0 ; p1 < RSL_MAXPROC ; p1++ )
	      work[p][p1] = globalhemiPlist[p1][p] ;
	  for ( p = 0 ; p < RSL_MAXPROC ; p++ )
	    for ( p1 = 0 ; p1 < RSL_MAXPROC ; p1++ )
	      globalhemiPlist[p][p1] = work[p][p1] ;
	  MPI_Scatter( globalhemiPlist, RSL_MAXPROC, MPI_INT, 
                       s_tinfo->hemi_recvPlist, RSL_MAXPROC, MPI_INT,
                       0, rsl_mpi_communicator ) ;

	  return ;       /* EARLY RETURN */
        }
      }
    }
    kiddex = INDEX_2(s_ojg,s_oig,s_mlen_o) ;
    P = s_odomain[ kiddex ].P ;
    if ( s_tinfo->other_hemi_procbufs[P] == NULL )
    {
      q = RSL_MALLOC( rsl_hemi_rec_t, 1 ) ;
    }
    else
    {
      q = RSL_MALLOC( rsl_hemi_rec_t, 1 ) ;
      q->next = s_tinfo->other_hemi_procbufs[P] ;
    }
    q->oig = s_oig ;
    q->ojg = s_ojg ;
    q->data = NULL ;
    s_tinfo->other_hemi_procbufs[P] = q ;
    s_q1 = q ;
  }
  else
  {
    int * x ;
    if ( *seed_p )
    {
      s_p = -1 ;
      s_q = NULL ;
    }
    if ( s_q == NULL )
    {
      s_p++ ;
      while ( s_tinfo->other_hemi_procbufs[s_p] == NULL ) s_p++ ;
      if ( s_p >= rsl_nproc_all )
      {
	*retval_p = 0 ;
	return ;       /* EARLY RETURN */
      }
      s_q = s_tinfo->other_hemi_procbufs[s_p] ;
    }
    s_oig = s_q->oig ;
    s_ojg = s_q->ojg ;
    s_q1 = s_q ;
    s_q = s_q->next ;
  }
  *oig_p = s_oig + 1 ;  /* C to Fortran */
  *ojg_p = s_ojg + 1 ;  /* C to Fortran */
  *retval_p = 1 ;
#else
   RSL_TEST_ERR( 1, "RSL_TO_OH_INFO STUBBED" ) ;
#endif
  return ;
}

/*@
  RSL_TO_OH_MSG -- Pack force data into a message for a nest point.

  Notes:
  See also:

@*/

RSL_TO_OH_MSG ( nbuf_p, buf )
  int_p
    nbuf_p ;     /* (I) Number of bytes to be packed. */
  char *
    buf ;        /* (I) Buffer containing the data to be packed. */
{
  int kiddex ;
  int nbuf ;
  int P ;

  RSL_TEST_ERR(buf==NULL,"2nd argument is NULL.  Field allocated?") ;
  nbuf = *nbuf_p ;
  if ( s_q1->data == NULL )
  {
    s_q1->data = RSL_MALLOC( char, s_msize ) ;
    s_q1->curs = 0 ;
  }
  if ( s_q1->curs+nbuf >= s_msize )
  {
    sprintf(mess,"RSL_TO_OH_MSG: store of %d bytes would overflow %d sized buffer.\n",nbuf,s_msize ) ;
    RSL_TEST_ERR(1,mess) ;
  }
  bcopy( buf, &(s_q1->data[s_q1->curs]), nbuf ) ;
  s_q1->curs += nbuf ;
}

/*@
  RSL_FORCE_HEMI -- Convey forcing data from this hemi to other hemi

  Notes:

  See also:
@*/

RSL_FORCE_HEMI ()
{
  int P ;
  int msglen, mdest, mtag ;
  int ii ;
  int ig, jg ;
  char * recvbuf, * sendbuf ;
  rsl_hemi_rec_t * q ;

  /* post receives */

  for ( P = 0 ; P < rsl_nproc_all ; P++ )
  {
    if ( s_tinfo->hemi_recvPlist[P] > 0 )
    {
      msglen = s_msize * s_tinfo->hemi_recvPlist[P] + 3*sizeof(int) ;
      recvbuf = buffer_for_proc( P, msglen,  RSL_RECVBUF ) ;
      mtag = MTYPE_FROMTO( MSG_FROM_PARENT, P, rsl_myproc ) ;
#ifdef DEBUGGAL
fprintf(stderr,"Posting receive on tag %d\n",mtag ) ;
#endif
      RSL_RECVBEGIN( recvbuf, msglen, mtag ) ;
      s_tinfo->hemi_recv_tags[P] = mtag ; /* store tag */
    }
  }

  /* do sends */

  for ( P = 0 ; P < rsl_nproc_all ; P++ )
  {
#ifdef DEBUGGAL
fprintf(stderr,"s_tinfo->hemi_sendPlist[P] %d\n",s_tinfo->hemi_sendPlist[P]) ;
#endif
    if ( s_tinfo->hemi_sendPlist[P] > 0 )
    {
      int curs ;
      int endofdata ;
	  
	  /* oig,ojg,nbytes,buffer * # of points + end of data */
      msglen = (3*sizeof(int)+s_msize)*s_tinfo->hemi_sendPlist[P]+1*sizeof(int) ;
      sendbuf = buffer_for_proc( P, msglen,  RSL_SENDBUF ) ;
      curs = 0 ;
      for ( q = s_tinfo->other_hemi_procbufs[P] ; q ; q = q->next )
      {
#ifdef DEBUGGAL
{
int *dp ;
dp = (int *) q->data ;
fprintf(stderr,"> curs %d, msglen %d msize %d (%d %d) data %d\n", curs, msglen, s_msize, q->oig, q->ojg, *dp ) ;
}
#endif
        bcopy( &(q->oig), &(sendbuf[curs]), sizeof(int)) ; curs += sizeof(int) ;
        bcopy( &(q->ojg), &(sendbuf[curs]), sizeof(int)) ; curs += sizeof(int) ;
        bcopy( &(q->curs), &(sendbuf[curs]), sizeof(int)) ; curs += sizeof(int) ;
        bcopy( q->data, &(sendbuf[curs]), q->curs) ; curs += q->curs ;
      }
      endofdata = RSL_INVALID ;
      bcopy( &endofdata, &(sendbuf[curs]), sizeof(int)) ; curs += sizeof(int) ;
      mtag = MTYPE_FROMTO( MSG_FROM_PARENT, rsl_myproc, P ) ;
#ifdef DEBUGGAL
fprintf(stderr,"sending sendbuf to %d, curs = %d\n",P,curs) ;
#endif
      RSL_SEND( sendbuf, curs, mtag, P ) ;
    }
  }
}

/*@
  RSL_FROM_TH_INFO -- Get the next cell in a unpacking sequence for forcing.

  Notes:

  See also:
@*/

static int s_endofdata, s_remaining, s_ndata, s_curs ;
static char * s_recvbuf ;

RSL_FROM_TH_INFO ( seed_p, oig_p, ojg_p, retval_p )
  int_p
    seed_p      /* (I) =1 if first call; =0 otherwise */
   ,oig_p        /* (O) Global index in M dimension of nest. */
   ,ojg_p        /* (O) Global index in N dimension of nest. */
   ,retval_p ;  /* (O) Return value; =1 valid point, =0 done. */
{
  int mtag ;

#ifdef DEBUGGAL
fprintf(stderr,"RSL_FROM_TH_INFO seed = %d, s_endofdata %d\n",*seed_p,s_endofdata) ;
#endif

  if ( *seed_p == 1 )
  {
    if ( s_pointbuf != NULL ) RSL_FREE(s_pointbuf) ;
    s_pointbuf = RSL_MALLOC( char, 2*s_msize ) ;  /* 2 times for safety */
    s_p = 0 ;
    s_endofdata = 1 ;
  }

nextproc:
  if ( s_endofdata )
  {
    while ( s_tinfo->hemi_recvPlist[s_p] <= 0 ) s_p++ ;
    if ( s_p >= rsl_nproc_all )
    {
      *retval_p = 0 ;
#ifdef DEBUGGAL
fprintf(stderr,"EARLY RETURN retval = 0\n") ;
#endif
      return ;          /* EARLY RETURN */
    }
    mtag = s_tinfo->hemi_recv_tags[s_p] ;
#ifdef DEBUGGAL
fprintf(stderr,"Waiting for receive on tag %d\n",mtag ) ;
#endif
    RSL_RECVEND ( mtag ) ;
#ifdef DEBUGGAL
fprintf(stderr,"got receive\n") ;
#endif
    s_recvbuf = buffer_for_proc( s_p, 0, RSL_RECVBUF ) ;
    s_p++ ;
    s_curs = 0 ;
    s_endofdata = 0 ;
  }

#ifdef DEBUGGAL
fprintf(stderr,"before bcopy  %d, s_recvbuf %08x\n",s_curs, s_recvbuf) ;
#endif

  bcopy ( &(s_recvbuf[s_curs]), oig_p, sizeof(int) ) ; s_curs += sizeof(int) ;
  if ( *oig_p == RSL_INVALID )
  {
#ifdef DEBUGGAL
fprintf(stderr,"hit end of data for s_p %d, %d\n", s_p, *oig_p ) ;
#endif
    s_endofdata = 1 ;
    goto nextproc ;
  }
  bcopy ( &(s_recvbuf[s_curs]), ojg_p, sizeof(int) ) ; s_curs += sizeof(int) ;
  bcopy ( &(s_recvbuf[s_curs]), &s_ndata, sizeof(int) ) ; s_curs += sizeof(int) ;
  bcopy ( &(s_recvbuf[s_curs]), s_pointbuf, s_ndata ) ; s_curs += s_ndata ;
  s_remaining = s_ndata ;
#ifdef DEBUGGAL
fprintf(stderr,"s_remaining = %d\n",s_remaining) ;
#endif

  (*oig_p) ++ ;
  (*ojg_p) ++ ;
#ifdef DEBUGGAL
fprintf(stderr,"RETURN oig ojg %d %d\n", *oig_p, *ojg_p ) ;
#endif
  *retval_p = 1 ;
  return ;
}

/*@
  RSL_FROM_TH_MSG -- Unpack feedback data into a nest point.

  Notes:

@*/
RSL_FROM_TH_MSG ( len_p, buf )
  int_p
    len_p ;          /* (I) Number of bytes to unpack. */
  char *
    buf ;            /* (O) Destination buffer. */
{
  if ( *len_p <= 0 ) return ;
  if ( *len_p > s_remaining ) 
  {
    sprintf(mess,
"RSL_FROM_TH_MSG:\n   Requested number of bytes (%d) exceeds %d, the number remaining for this point.\n", *len_p, s_remaining) ;
    RSL_TEST_WRN(1,mess) ;
  }
  bcopy( &(s_pointbuf[s_ndata-s_remaining]),
         buf,
         *len_p ) ;

  s_remaining -= *len_p ;
}

/* retval =1 if point is local, =0 otherwise */
RSL_POINT_ON_PROC ( d_p, ig_p, jg_p, retval_p )
  int_p d_p, ig_p, jg_p, retval_p ;
{
  int d ;
  int kiddex ;
  int P ;
  int ig, jg ;

  rsl_domain_info_t * info ;
  rsl_point_t       * domain ;
  ig = *ig_p - 1 ;
  jg = *jg_p - 1 ;

  d = *d_p ;
  RSL_TEST_ERR( d < 0 || d > RSL_MAXDOMAINS,
    "rsl_ready_bcast: bad 'this hemi' descriptor" ) ;
  info = &( domain_info[d]) ;
/* added 12/27/01 -- JM */
  if ( ig < 0 || ig >= info->len_m ||
       jg < 0 || jg >= info->len_n ) { *retval_p = 0 ; return ; }
  domain = info->domain ;
  kiddex = INDEX_2(jg,ig,info->len_m ) ;
  P = domain[ kiddex ].P ;
  *retval_p = 0 ;
  if ( P == rsl_myproc ) *retval_p = 1 ;
  return ;
}

/* given a global point, return the processor number */
RSL_PROC_FOR_POINT ( d_p, ig_p, jg_p, retval_p )
  int_p d_p, ig_p, jg_p, retval_p ;
{
  int d ;
  int kiddex ;
  int P ;
  int ig, jg ;

  rsl_domain_info_t * info ;
  rsl_point_t       * domain ;
  ig = *ig_p - 1 ;
  jg = *jg_p - 1 ;

  d = *d_p ;
  RSL_TEST_ERR( d < 0 || d > RSL_MAXDOMAINS,
    "rsl_point_on_proc: bad descriptor" ) ;
  info = &( domain_info[d]) ;
  domain = info->domain ;
  kiddex = INDEX_2(jg,ig,info->len_m ) ;
  *retval_p = domain[ kiddex ].P ;
  return ;
}


