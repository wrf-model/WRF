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

/* #define NOPACK   */
/* #define NOUNPACK */
/*
   rsl_remap_state

   Called to move partitions in memory and between processors
   after a domain is re-decomposed.  Assumes that a state vector
   has been associated with a domain.  If not, returns with warning.

 */
#include <stdio.h>
#include <stdlib.h>
#include "rsl.h"

extern rsl_list_t *point_move_receives[] ;	/* decomp.c */
extern rsl_list_t *point_move_sends[] ;

int debuggal_pack = 0 ;

static int sendsize[RSL_MAXPROC] ;
static int recvsize[RSL_MAXPROC] ;
static int recvtag[RSL_MAXPROC] ;
static int recvnpts[RSL_MAXPROC] ;

/*@
  RSL_REMAP_STATE --- Use previously defined state vectors to remap grid points.

  Notes:
  This routine is called at the point in a remapping at which two state vectors
  --- one for the source data structures and one for the destination
  data structures of a domain --- have 
  been defined for RSL using RSL_DESCRIBE_STATE.  Remapping can be done in-place
  (both state vectors may refer to the same data structures).  Also, a
  new decomposition should have been given to RSL using RSL_FDECOMPOSE.

  When this routine is called, RSL computes the difference between the
  old and new mappings and constructs a schedule of points that need
  to be moved between processors.  Using the schedule, it packs data
  for moving grid points into messages, exchanges the messages between
  processors, then unpacks the messages into their new locations.
  Points may be moved in a processor's memory, even if the
  points are not communicated (this allows RSL to make room for
  an influx of points in the subdomain, if necessary).  RSL bases
  packing and unpacking on the state vectors that have been previously
  defined.

  On return, old domain data structures may be discarded (assuming the
  remapping has not been done in-place), and computation may resume.
  All subsequent horizontal iteration, stencil-exchange, and
  broadcast-merge communications will be over the new mapping.
  The stencils and broadcast merges will automatically reconfigure
  themselves the first time they are used on the new mapping.  However,
  it is crucial that the loop macros (LoopMacros.m4) be re-initialized
  by an execution of RSL_INIT_RUNVARS, before iteration is begun.
  If the program is not using the loop macros and instead handling iteration
  explicitely, a new call to RSL_GET_RUN_INFO or RSL_GET_RUN_INFOP is
  required.  Iteration using the column-callable routines RSL_COMPUTE_CELLS
  and RSL_COMPUTE_MASK does not need to be re-initialized.

  Example:
$ C Construct state vector for current mapping.
$   dcp(1) = rsl_northsouth ; dcp(2) = rsl_eastwest ; dcp(3) = rsl_notdecomposed
$   gl(1)  = d%m            ; gl(2)  = d%n          ; gl(3)  = d%nlev
$   call rsl_create_message(ms)
$   call rsl_build_message(ms,rsl_real,d%psa,size(shape(d%psa)),
$                          dcp,gl,shape(d%psa))
$   call rsl_build_message(ms,rsl_real,d%ua,size(shape(d%ua)),
$                          dcp,gl,shape(d%ua))
$   call rsl_build_message(ms,rsl_real,d%va,size(shape(d%va)),
$                          dcp,gl,shape(d%va))
$   . . .
$   call rsl_describe_state(did,ms)
$ C
$ C New decomposition.
$   retval=rsl_fdecompose(did,mapping,p_lt,p_ln,timers,mloc,nloc)
$ C
$   if (retval .eq. 0 ) then
$ C
$ C Construct state vector for new mapping and associate with
$ C newly allocated data structures.
$     call allocate_domain(tmp,did,tmp%m,tmp%n,tmp%nlev,mloc,nloc)
$     call rsl_create_message(ms)
$     call rsl_build_message(ms,rsl_real,tmp%ua,size(shape(tmp%psa)),
$                            dcp,gl,shape(tmp%psa))
$     . . .
$     call rsl_describe_state(did,ms)
$ C
$ C Effect the remapping
$     call rsl_remap_state(did)

BREAKTHEEXAMPLECODE

  This example is from the dynamic load balancing code in MM90,
  the Fortran90 implementation of the Penn State/NCAR MM5.

  A state vector --- an RSL message definition that contains a list of
  all the fields that make up the state for a grid-column in the old
  decomposition --- is constructed with successive calls to
  RSL_BUILD_MESSAGE.  Then, the
  domain DID is decomposed using the MM90 routine, MAPPING, passed as a
  function to the RSL_FDECOMPOSE.  TIMERS is an MM90 array of timers for
  containing per-grid-column performance data that is used by MAPPING.
  TIMERS
  is passed directly to MAPPING when it is called from within RSL.
  RSL_FDECOMPOSE returns 0 for
  success if the new mapping improves on the current one.
  It also passes back MLOC and NLOC with the
  dimensions of arrays that will be needed to hold the arrays of the processor
  subdomain under the new decomposition.  Otherwise, the RSL_FDECOMPOSE
  returns a non-zero value, indicating that the program should continue
  to time step using the old decomposition.

  If a new decomposition is adopted, MM90 allocates a new domain
  structure, TMP, using MLOC and NLOC.  This will hold the remapped
  data.  (RSL permits remapping in place, without resizing memory, but
  this places restrictions on how far the remapping algorithm can go in
  moving work around).  The code defines a new state vector identical to
  the previous one except that it is associated with the new fields of TMP.

  The call to RSL_REMAP_STATE effects the remapping.  RSL compares the
  old and new mappings and generates the lists of moves between each
  processor.  RSL then uses the information in the first state
  vector to pack the columns to be moved into messages and sends the
  messages between the processors. On arrival, the incoming messages are
  unpacked using information from the second state vector.  When the call
  returns TMP contains the remapped state data. The program then
  uses pointer assignments (not shown) to swap old and new data into the
  D domain, then the old data structures are deallocated.  In the end, D
  points to the structure with the remapped data and the model resumes
  time stepping on the domain under the new mapping.

  See also:
  RSL_DESCRIBE_STATE, RSL_FDECOMPOSE, LoopMacros.m4
@*/

RSL_REMAP_STATE ( d_p )
  int_p d_p ; 	/* (I) RSL domain descriptor. */
{
  message_desc_t *old, *new ;
  rsl_domain_info_t * dinfo ;
  rsl_list_t *lp ;
  int m1, m2, msize, size, npts, curs ;
  int mtype, mdest ;
  int isaved, jsaved ;
  char *pbuf ;
  int P ;
  int d ;
  int i, id, ig, jg, d1 ;

  d = *d_p ;

  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
    "rsl_remap_state: bad domain descriptor\n") ;
  dinfo = &(domain_info[d]) ;
  RSL_TEST_ERR(dinfo->valid != RSL_VALID,
    "rsl_remap_state: descriptor is not for a valid domain\n") ;


  /* get old and new state vectors */

  RSL_TEST_ERR((old = dinfo->old_state_vect) == NULL,
	"no state message previously associated with domain" );
  RSL_TEST_ERR((new = dinfo->new_state_vect) == NULL,
	"no state message associated with domain" ) ;
  
  /* figure out size and post a recieve for each processor in the receive list */
  m1 = message_size( new ) ;
  m2 = message_size( old ) ;
  if ( m1 != m2 )
  {
    sprintf(mess,
    "old (%d) and new (%d) state vectors cannot be different sizes.",m2,m1) ;
    RSL_TEST_ERR(1,mess) ;
  }
  msize = m1 ;

  for ( P = 0 ; P < rsl_nproc_all ; P++ )
  {
    size = 0 ;
    npts = 0 ;
    for ( lp = point_move_receives[P] ; lp != NULL ; lp = lp->next )
    {
#ifdef crayx1
      size += msize + 2*sizeof(int) ;  /* size plus int for ig and jg */
#else
      size += msize + 2*sizeof(short) ;  /* size plus shorts for ig and jg */
#endif
      npts++ ;
    }
    if ( size > 0 )
    {
      if ( rsl_c_comp2phys_proc(P) != rsl_myproc )
      {
        pbuf = buffer_for_proc( P, size, RSL_RECVBUF ) ;
        mtype = MTYPE_FROMTO( MSG_REDISTCOM,
                              rsl_c_comp2phys_proc(P),
                              rsl_myproc ) ;
        RSL_RECVBEGIN( pbuf, size, mtype ) ;
	recvsize[P] = size ;
	recvtag[P] = mtype ;
        recvnpts[P] = npts ;
      }
      else
      {
	recvsize[P] = size ;
	recvtag[P] = mtype ;
        recvnpts[P] = npts ;
      }
    }
    else
    {
      recvsize[P] = 0 ;
      recvnpts[P] = 0 ;
      recvtag[P] = RSL_INVALID ;
    }
  }

  isaved = dinfo->ilocaloffset ;
  jsaved = dinfo->jlocaloffset ;
  dinfo->ilocaloffset = dinfo->old_ilocaloffset ;
  dinfo->jlocaloffset = dinfo->old_jlocaloffset ;

  debuggal_pack = 0 ;

  /* pack and send messages to each processor in the send list --
     buffer the ones for me */
  for ( P = 0 ; P < rsl_nproc_all ; P++ )
  {
    size = 0 ;
    npts = 0 ;
    for ( lp = point_move_sends[P] ; lp != NULL ; lp = lp->next )
    {
#ifdef crayx1
      size += msize + 2*sizeof(int) ;
#else
      size += msize + 2*sizeof(short) ;
#endif
      npts++ ;
    }
    if ( size > 0 )
    {
      pbuf = buffer_for_proc( P, size, RSL_SENDBUF ) ;
    }
    curs = 0 ;
    for ( lp = point_move_sends[P] ; lp != NULL ; lp = lp->next )
    {
#ifdef crayx1
      bcopy( &(lp->info1), &(pbuf[curs]), sizeof(int)) ;        /* point id */
      curs += sizeof(int) ;
      bcopy( &(lp->info2), &(pbuf[curs]), sizeof(int)) ;        /* point id */
      curs += sizeof(int) ;
#else
      bcopy( &(lp->info1), &(pbuf[curs]), sizeof(short)) ;      /* point id */
      curs += sizeof(short) ;
      bcopy( &(lp->info2), &(pbuf[curs]), sizeof(short)) ;      /* point id */
      curs += sizeof(short) ;
#endif
      ig = lp->info1 ;
      jg = lp->info2 ;
#ifndef NOPACK
      pack_message( old, pbuf, &curs, d, ig, jg ) ;
#else
      curs = size ;
#endif
      RSL_TEST_ERR(curs > size, "Buffer overflow") ;
    }

    if ( curs > 0 )
    {
      if ( rsl_myproc != rsl_c_comp2phys_proc(P) )
      {
        mtype = MTYPE_FROMTO( MSG_REDISTCOM,
			      rsl_myproc,
			      rsl_c_comp2phys_proc(P) ) ;
        mdest = rsl_c_comp2phys_proc (P) ;
        RSL_SEND( pbuf, curs, mtype, mdest ) ;
      }
      else
      {
	recvsize[P] = curs ;
	recvnpts[P] = npts ;
      }
    }
  }
  dinfo->ilocaloffset = isaved ;
  dinfo->jlocaloffset = jsaved ;

  /* receive points from other processors and unpack in new position */
  for ( P = 0 ; P < rsl_nproc_all ; P++ )
  {
    curs = 0 ;
    if ( recvsize[P] > 0 ) 
    {
      if ( rsl_c_comp2phys_proc( P ) != rsl_myproc )
      {
        RSL_RECVEND( recvtag[P] ) ;
	pbuf = buffer_for_proc( P, recvsize[P], RSL_RECVBUF ) ;
      }
      else
      {
	pbuf = buffer_for_proc( P, recvsize[P], RSL_SENDBUF ) ;
      }
      for ( i = 0 ; i < recvnpts[P] ; i++ )
      {
#ifdef crayx1
        int id ;
                                                                                                                    
        bcopy( &(pbuf[curs]), &id, sizeof(int)) ;       /* point id */
        curs += sizeof(int) ;
        ig = id ;
                                                                                                                    
        bcopy( &(pbuf[curs]), &id, sizeof(int)) ;       /* point id */
        curs += sizeof(int) ;
#else
        short id ;
                                                                                                                    
        bcopy( &(pbuf[curs]), &id, sizeof(short)) ;     /* point id */
        curs += sizeof(short) ;
        ig = id ;
                                                                                                                    
        bcopy( &(pbuf[curs]), &id, sizeof(short)) ;     /* point id */
        curs += sizeof(short) ;
#endif
	jg = id ;

#ifndef NOUNPACK
        unpack_message( new, pbuf, &curs, d, ig, jg ) ;
#endif
      }
    }
  }

  buffer_for_proc( rsl_c_phys2comp_proc(rsl_myproc), 0, RSL_FREEBUF ) ;

  debuggal_pack = 0 ;

}

/*@
  RSL_DESCRIBE_STATE --- Describe a state vector for use in remappping.

  Notes:
  This routine takes an RSL message, Arg2, and associates it with
  the domain specified by Arg1.   The message Arg2 is built using
  RSL_BUILD_MESSAGE.  The state vector is then stored internally
  within RSL and used in run-time remapping (RSL_REMAP_STATE) for
  dynamic load balancing.

  RSL keeps a maximum of two state vectors internally, one representing
  a new state and one representing an old.  Each call to RSL_DESCRIBE_STATE
  installs Arg2 as the new new state vector, pushing the previous 
  new state vector to the old position.   The previous old state vector
  is discarded.  RSL_REMAP_STATE uses the old state vector as it's
  guide for packing source data into messages
  for remapping.   The new state vector is used for unpacking into
  destination data structures.  See RSL_REMAP_STATE for a code example.

  See also:
  RSL_REMAP_STATE, RSL_BUILD_MESSAGES


@*/

RSL_DESCRIBE_STATE ( d_p, message )
  int_p d_p ;
  int_p message ;
{
  int d ;
  int mh ;
  rsl_domain_info_t * dinfo ;
  message_desc_t *msg ;

  d = *d_p ;

  RSL_TEST_ERR( d < 0 || d >= RSL_MAXDOMAINS,
    "rsl_describe_state: bad domain descriptor\n") ;
  dinfo = &(domain_info[d]) ;
  RSL_TEST_ERR(dinfo->valid != RSL_VALID,
    "rsl_describe_state: descriptor is not for a valid domain\n") ;

  mh = *message ;
  RSL_TEST_ERR( mh != RSL_INVALID && (mh < 0 || mh >=RSL_MAXDESCRIPTORS),
"rsl_describe_state: bad message handle in list,\n  must be either valid message or RSL_INVALID") ;
  dinfo->old_state_vect = dinfo->new_state_vect ;
  if ( mh != RSL_INVALID )
  {
    RSL_TEST_ERR((msg = (message_desc_t *) mh_descriptors[ mh ])==NULL,
       "rsl_describe_state: handle does not describe an active message") ;
    RSL_TEST_ERR( msg->tag != MESSAGE_DESC,
       "rsl_describe_state: handle given in message list is not for an rsl mesage def" ) ;
    dinfo->new_state_vect = msg ;
  }
  else
  {
    dinfo->new_state_vect = NULL ;
  }

/* If there was not an old state, make the old state the dup of the new state */
  if ( dinfo->old_state_vect == NULL )
  {
    dinfo->old_state_vect = dinfo->new_state_vect ;
  }

  release_mh_descriptor( &mh ) ;

}

void *
myloc( x )
  void * x ;
{
  return( x ) ;
}

