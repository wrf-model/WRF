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



#ifndef RSL_COMM_H
#define RSL_COMM_H

/* #define STUBS */

/***********************************************************************
Below is a mapping of the message passing macros that are used in RSL,
the library package that supports the model (there is no explicit
message passing in the model code itself -- all of that is encapsulated
within RSL).

# define RSL_OPEN0(A,B)          open0_( &(A), &(B), &rsl_idum )
# define RSL_CLOSE0()            close0_()
# define RSL_WHO(A,B)           who0_( &(A), &(B), &rsl_idum )
# define RSL_RECV(A,B,C)        recv0_( A, &(B), &(C) )
# define RSL_SEND(A,B,C,D)      send0_( A, &(B), &(C), &(D) )
# define RSL_RECVBEGIN(A,B,C)   recvbegin0_( A, &(B), &(C) )
# define RSL_SENDBEGIN(A,B,C,D) sendbegin0_( A, &(B), &(C), &(D) )
# define RSL_RECVEND(A)         recvend0_( &(A) )
# define RSL_SENDEND(A)         sendend0_( &(A) )
# define RSL_PROBE(A,B)         rsl_probe_( &(A), B )

In this particular fragment, the macros are defined to PICL calls, 
but that is just incidental.

RSL_OPEN0(A,B)
RSL_CLOSE0()

  This will map to whatever routine the underlying mp library requires
  for startup and initialization/shutdown.  It can just be a noop if the
  mp lib in use does not require initialization.  As far as RSL is concerned,
  arguments A and B are dummys.

RSL_WHO(A,B)

  This will map to the informational routine in the mp library that
  returns number of nodes in partition (returned as A) and the node
  number of the local processor (returned as B).

RSL_RECV(A,B,C)

  This is the synchronous recv (like crecv in NX).  A is a pointer
  to the buffer, B is the length in bytes, and C is the integer
  message tag.

RSL_SEND(A,B,C,D)

  This is the synchronous send (like csend in NX).  A is a pointer
  to the buffer, B is the length in bytes, and C is the integer
  message tag.  D is the node number of the destination processor.

RSL_RECVBEGIN(A,B,C)

  This is the asynchronous version of RSL_RECV.  It records the message
  descriptor returned by the OS, posts the receive and then returns
  control to the calling program.  In NX is it implemented using
  irecv.

RSL_RECVEND(A)

  This is the call the blocks until completion of the message started
  with RSL_RECVBEGIN.  The argument, A, is the integer tag of the 
  message (the third argument, C, of the original RSL_RECVBEGIN call).
  Using the tag, the routine looks up the OS descriptor for the original
  message and then issues the mp library call to block on that message
  (in the case of NX, the routine called is msgwait).


RSL_SENDBEGIN(A,B,C,D)
 
  This is the asynchronous version of RSL_SEND.  It records the message
  descriptor returned by the OS, posts the send and then returns
  control to the calling program.  In NX is it implemented using
  isend.
  
RSL_SENDEND(A)

  This is the call the blocks until completion of the message started
  with RSL_SENDBEGIN.  The argument, A, is the integer tag of the 
  message (the third argument, C, of the original RSL_SENDBEGIN call).
  Using the tag, the routine looks up the OS descriptor for the original
  message and then issues the mp library call to block on that message
  (in the case of NX, the routine called is msgwait).

RSL_PROBE(A,B)

  This maps to the mp library routine for checking on the status of
  an asynchronously posted receive.  This is *not* currently in use
  but may be needed at some point in the future.

---

John
***********************************************************************/

#ifdef STUBS

# define RSL_OPEN0              {RSL_TEST_WRN(1,"COMMUNICATIONS STUBBED!");}
# define RSL_CLOSE0()
# define RSL_WHO(A,B)           { A = 1 ; B = 0 ; }
# define RSL_RECV(A,B,C)
# define RSL_SEND(A,B,C,D)
# define RSL_RECVBEGIN(A,B,C)
# define RSL_SENDBEGIN(A,B,C,D)
# define RSL_RECVEND(A)
# define RSL_SENDEND(A)
# define RSL_PROBE(A,B)         { *(B) = 1 ; }

#else
#ifdef PGON

# define RSL_OPEN0(A,B)         rslNXInit()
# define RSL_CLOSE0
# define RSL_WHO(A,B)           { A = numnodes(); B = mynode(); }
# define RSL_RECV(A,B,C)        crecv ((long)(C),A,(long)(B))
# define RSL_SEND(A,B,C,D)      csend ((long)(C),A,(long)(B),(long)(D),(long)0)
# define RSL_RECVBEGIN(A,B,C)   rslNXIRecv ( A, B, C )
# define RSL_SENDBEGIN(A,B,C,D) rslNXISend ( A, B, C, D )
# define RSL_RECVEND(A)         rslNXWait ( A )
# define RSL_SENDEND(A)         rslNXWait ( A )
# define RSL_PROBE(A,B)         rslNXProbe ( A, B )
/* # define RSL_PROBE(A,B)         (*(B) = (iprobe ( A ) == 1)) */

#else
#ifdef MPL
/* map down to native MPL primitives of SP[12] */

#    ifdef __MPL_COMPAT__
       int rsl_mp_source ;
       int rsl_mp_nbytes ;
       int rsl_mp_n ;
       int rsl_mp_type ;

       int dontcare ;
       int allmsg ;
       int nulltask ;
       int allgrp ;
       int type_low ;
       int type_high ;

#    else
       extern int rsl_mp_source ;
       extern int rsl_mp_nbytes ;
       extern int rsl_mp_type ;
       extern int rsl_mp_n ;

       extern int dontcare ;
       extern int allmsg ;
       extern int nulltask ;
       extern int allgrp ;
       extern int type_low ;
       extern int type_high ;

#    endif

# define RSL_OPEN0(A,B)         rslMPLInit()
# define RSL_CLOSE0()
# define RSL_WHO(A,B)           mpc_environ( &(A), &(B) )

# define RSL_RECV(A,B,C)  \
  { int rc ; \
    rsl_mp_source = dontcare ;\
    rsl_mp_type   = C ;\
    rsl_mp_n      = B ;\
    if ( rsl_mp_type < type_low || rsl_mp_type > type_high )	\
    { \
      sprintf(mess,"RSL_RECV message type %d out of allowed range: %d..%d\n", \
      rsl_mp_type,type_low,type_high) ; \
      RSL_TEST_ERR( 1, mess ) ; \
    } \
    rc = mpc_brecv(A,rsl_mp_n,               \
                   &rsl_mp_source,            \
                   &rsl_mp_type,              \
                   &rsl_mp_nbytes) ;  \
    if ( rc ) {fprintf(stderr,"mpc_brecv returns %d\n",rc);exit(1);} \
    if ( rsl_mp_nbytes > (B) ) 	\
    {				\
      fprintf(stderr,"Message too large: tag %d, recvd %d, allocated %d\n", \
 	      C,rsl_mp_nbytes,(B));	\
    }  \
  }

# define RSL_SEND(A,B,C,D)       \
  { \
    int rc ; \
    rsl_mp_type = C ; \
    if ( rsl_mp_type < type_low || rsl_mp_type > type_high )	\
    { \
      sprintf(mess,"RSL_SEND message type %d out of allowed range: %d..%d\n", \
      rsl_mp_type,type_low,type_high) ; \
      RSL_TEST_ERR( 1, mess ) ; \
    } \
    if (0) fprintf(stderr,"mpc_bsend: nlen %10d  type %10d  dest %10d\n", \
                   B, rsl_mp_type, D ) ; \
    rc = mpc_bsend(A,B,D,C) ; \
    if ( rc ) {fprintf(stderr,"mpc_bsend returns %d\n",rc);exit(1);} \
  }

# define RSL_RECVBEGIN(A,B,C)   rslMPLIRecv ( A, B, C )
# define RSL_SENDBEGIN(A,B,C,D) rslMPLISend ( A, B, C, D )
# define RSL_RECVEND(A)         rslMPLWait ( A )
# define RSL_SENDEND(A)         rslMPLWait ( A )
# define RSL_PROBE(A,B)         rslMPLProbe( A, B )

#else
#ifdef MPI

#    include "mpi.h"
/* EXTERN is defined in rsl.h */

EXTERN MPI_Comm rsl_mpi_communicator ;

#    ifdef __MPI_COMPAT__
       MPI_Status mpi_status ;
#    else
       extern MPI_Status mpi_status ;
#    endif

# define RSL_OPEN0(A,B)         rslMPIInit() 
# define RSL_CLOSE0()           MPI_Finalize()
# define RSL_WHO(A,B)           rslMPIWho( &(A), &(B) )

# define RSL_RECV(A,B,C)  \
{ \
MPI_Recv(A,B,MPI_BYTE,MPI_ANY_SOURCE,C,rsl_mpi_communicator,&mpi_status) ; \
}

# define RSL_SEND(A,B,C,D)      MPI_Send(A,B,MPI_BYTE,D,C,rsl_mpi_communicator)
# define RSL_RECVBEGIN(A,B,C)   rslMPIIRecv ( A, B, C )
# define RSL_SENDBEGIN(A,B,C,D) rslMPIISend ( A, B, C, D )
# define RSL_RECVEND(A)         rslMPIWait ( A )
# define RSL_SENDEND(A)         rslMPIWait ( A )
# define RSL_PROBE(A,B)         /* rslMPITest ( A, B ) */

#else
#ifdef CHAMELEON

#include "tools.h" 
#include "comm/comm.h"

#   define RSL_OPEN0(A,B)         rslCHAMInit()
#   define RSL_CLOSE0
#   define RSL_WHO(A,B)           who0_( &(A), &(B), &rsl_idum )
#   define RSL_RECV(A,B,C)        PIbrecv( C, A, B, MSG_OTHER )
#   define RSL_SEND(A,B,C,D)      PIbsend( C, A, B, D, MSG_OTHER )
#   define RSL_RECVBEGIN(A,B,C)   rslCHAMRecv( A, B, C )
#   define RSL_SENDBEGIN(A,B,C,D) rslCHAMSend( A, B, C, D )
#   define RSL_RECVEND(A)         rslCHAMWait( A, 1 )
#   define RSL_SENDEND(A)         rslCHAMWait( A, 0 )
#   define RSL_PROBE(A,B)         ( &(A), B )


else

# ifndef NOUNDERSCORE

#   ifdef CHAMELEON_PICL
#     define RSL_OPEN0(A,B)    /* a noop */
#   else
#     define RSL_OPEN0(A,B)        open0_( &(A), &(B), &rsl_idum )
#   endif
#   define RSL_CLOSE0
#   define RSL_WHO(A,B)           who0_( &(A), &(B), &rsl_idum )
#   define RSL_RECV(A,B,C)        recv0_( A, &(B), &(C) )
#   define RSL_SEND(A,B,C,D)      send0_( A, &(B), &(C), &(D) )
#   define RSL_RECVBEGIN(A,B,C)   recvbegin0_( A, &(B), &(C) )
#   define RSL_SENDBEGIN(A,B,C,D) sendbegin0_( A, &(B), &(C), &(D) )
#   define RSL_RECVEND(A)         recvend0_( &(A) )
#   define RSL_SENDEND(A)         sendend0_( &(A) )
#   define RSL_PROBE(A,B)         rsl_probe_( &(A), B )


# else

#   ifdef CHAMELEON_PICL_ooo
#     define RSL_OPEN0(A,B)    /* a noop */
#   else
#     define RSL_OPEN0(A,B)        open0( &(A), &(B), &rsl_idum )
#   endif
#   define RSL_CLOSE0
#   define RSL_WHO(A,B)           who0( &(A), &(B), &rsl_idum )
#   define RSL_RECV(A,B,C)        recv0( A, &(B), &(C) )
#   define RSL_SEND(A,B,C,D)      send0( A, &(B), &(C), &(D) )
#   define RSL_RECVBEGIN(A,B,C)   recvbegin0( A, &(B), &(C) )
#   define RSL_SENDBEGIN(A,B,C,D) sendbegin0( A, &(B), &(C), &(D) )
#   define RSL_RECVEND(A)         recvend0( &(A) )
#   define RSL_SENDEND(A)         sendend0( &(A) )
#   define RSL_PROBE(A,B)         rsl_probe( &(A), B )

# endif  /* NOUNDERSCORE */

#endif  /* CHAMELEON */
#endif  /* MPI */
#endif  /* MPL */
#endif  /* PGON */
#endif  /* STUBS */



#endif  /* nothing after this line */

