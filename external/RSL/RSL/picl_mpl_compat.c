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

#ifdef MPL
#ifndef __MPL_COMPAT__
#define __MPL_COMPAT__

#include <stdio.h>

typedef unsigned int MPL_Request ;
typedef unsigned int MPL_Status ;

#define PICLHandleInc 128

struct tagsToHandles
       {
       int tag;
       MPL_Request Handle;
       int type ;		/* send = 1, recv = 2 */
       int nbytes ;
       };
struct piclMPLHandles
       {
       int nHandles;
       int nUsed;
       struct tagsToHandles *tags;
       } piclMPLHandleLUT;

/******************************************************
 * piclMPLInit ()
 *   do whatever initialization is necessary for the
 *   MPL port
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *  Adapted to MPL: J. Michalakes 7/13/94
 *  Adapted to PICL from RSL: J. Michalakes 7/13/94
 *
 *****************************************************/

static int argc_dummy = 0 ;
static char * argv_dummy = "" ;

int dontcare ;
int allmsg ;
int nulltask ;
int allgrp ;
int type_low ;
int type_high ;

/*
 *   setarc0
 */

void setarc0( i1, i2, i3, i4 ) 	/* noop */
  int *i1, *i2, *i3, *i4 ;
{ return ; }

/*
 *   check0
 */

void check0( i1 ) 	        /* noop */
  int *i1 ;
{ return ; }

/*
 *   open0
 */

void open0( nprocs, me, dum )
  int *nprocs, *me, *dum ;
{
  int nbuf[4] ;
   
  piclMPLHandleLUT.nHandles = PICLHandleInc;
  piclMPLHandleLUT.nUsed = 0;
  piclMPLHandleLUT.tags = (struct tagsToHandles *) 
                     malloc (sizeof (struct tagsToHandles) * PICLHandleInc);

  mpc_task_query( nbuf, 2, 2 ) ;
  type_low = nbuf[0] ;
  type_high= nbuf[1] ;
  mpc_task_query( nbuf, 4, 3 ) ;
  dontcare = nbuf[0] ;
  allmsg   = nbuf[1] ;
  nulltask = nbuf[2] ;
  allgrp   = nbuf[3] ;

#if 0
  fprintf(stderr,"piclMPLInit: \n") ;
  fprintf(stderr,"type_low:  %d\n",type_low) ;
  fprintf(stderr,"type_high: %d\n",type_high) ;
  fprintf(stderr,"dontcare:  %d\n",dontcare) ;
  fprintf(stderr,"allmsg:    %d\n",allmsg) ;
  fprintf(stderr,"nulltask:  %d\n",nulltask) ;
  fprintf(stderr,"allgrp:    %d\n",allgrp) ;
#endif


  if (piclMPLHandleLUT.tags == NULL)
  {
    fprintf (stderr, "Fatal Error: malloc failure in piclMPLInit\n");
    exit(1);
  }

  who0( nprocs, me, dum ) ;
}

/*
 *   who0
 */
who0( nprocs, me, dum )
  int *nprocs, *me, *dum ;
{
  mpc_environ( nprocs, me ) ;
}

/*
 *   clock0
 */
double clock0()
{
  fprintf(stderr,"Warning -- clock0 is stubbed in %s\n", __FILE__ ) ;
  return(0.0) ;		 /* stub for now */
}

/*
 *   recv0
 */

recv0( buf, len, type )
  char *buf ;
  int *len, *type ;
{
  int rc ;
  int rsl_mp_source ;
  int rsl_mp_type ;
  int rsl_mp_n ;

  rsl_mp_type = *type ;
  rsl_mp_n = *len ;

  if ( rsl_mp_type < type_low || rsl_mp_type > type_high )
  {
    sprintf(mess,"RSL_RECV message type %d out of allowed range: %d..%d\n",
            rsl_mp_type,type_low,type_high) ;
    RSL_TEST_ERR( 1, mess ) ;
  }
  rc = mpc_brecv(buf,rsl_mp_n,
                 &rsl_mp_source,
                 &rsl_mp_type,
                 &rsl_mp_nbytes) ;
  if ( rc ) {fprintf(stderr,"mpc_brecv returns %d\n",rc);exit(1);} 
  if ( rsl_mp_nbytes > (*len) )
  {
    fprintf(stderr,"Message too large: tag %d, recvd %d, allocated %d\n",
            *type,rsl_mp_nbytes,(*len));
  }
}

/*
 *   send0
 */

send0( buf, len, type, dest )
  char *buf ;
  int *len, *type, *dest ;
{
  int rc ;
  int rsl_mp_type ;

  rsl_mp_type = *type ;
  if ( rsl_mp_type < type_low || rsl_mp_type > type_high )
  {
    sprintf(mess,"RSL_SEND message type %d out of allowed range: %d..%d\n",
    rsl_mp_type,type_low,type_high) ;
    RSL_TEST_ERR( 1, mess ) ;
  }
  if (0) fprintf(stderr,"mpc_bsend: nlen %10d  type %10d  dest %10d\n", \
                 B, rsl_mp_type, D ) ; \
  rc = mpc_bsend(A,B,D,C) ; \
  if ( rc ) {fprintf(stderr,"mpc_bsend returns %d\n",rc);exit(1);} \
}




/******************************************************
 * piclMPLFindWaitH (tag)
 *  Use the LUT to find an MPL wait handle from a tag
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/

long piclMPLFindWaitH (tag, waitHandle, type, nbytes )
  int tag;   /* Tag for which we lookup a wait handle */
  MPL_Request *waitHandle ;
  int *type, *nbytes ;
{
  int i;
  long retVal = -1;

  for (i=0; i < piclMPLHandleLUT.nUsed; i++)
  {
    if (piclMPLHandleLUT.tags[i].tag == tag)
    {
      *waitHandle = piclMPLHandleLUT.tags[i].Handle;
      *type = piclMPLHandleLUT.tags[i].type;
      *nbytes = piclMPLHandleLUT.tags[i].nbytes;
      piclMPLHandleLUT.nUsed--;  /* Keep them contiguous */
      piclMPLHandleLUT.tags[i].tag=piclMPLHandleLUT.tags[piclMPLHandleLUT.nUsed].tag;
      piclMPLHandleLUT.tags[i].Handle=
                  piclMPLHandleLUT.tags[piclMPLHandleLUT.nUsed].Handle;
      piclMPLHandleLUT.tags[i].type=
		  piclMPLHandleLUT.tags[piclMPLHandleLUT.nUsed].type;
      piclMPLHandleLUT.tags[i].nbytes=
		  piclMPLHandleLUT.tags[piclMPLHandleLUT.nUsed].nbytes ;
      retVal = 0 ;
      break;
    }
  }
  return( retVal ) ;
}

/******************************************************
 * piclMPLSaveWaitH (tag, waitHandle)
 *  Use the LUT to save an MPL wait handle referenced by a tag
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/
 
void piclMPLSaveWaitH (tag, waitHandle,type,nbytes)
 int tag;
 MPL_Request * waitHandle;
 int type, nbytes ;
{
 /* Make sure there is enough space, if not, try a realloc */
 /* If the realloc fails we're in deep trouble */
 if (piclMPLHandleLUT.nUsed == piclMPLHandleLUT.nHandles)
   {
   struct tagsToHandles *tags;  /* Temp pointer */
   tags = (struct tagsToHandles *) 
      realloc (piclMPLHandleLUT.tags, 
      sizeof (struct tagsToHandles) * (piclMPLHandleLUT.nHandles + PICLHandleInc));
   if (tags != NULL)
     {
     piclMPLHandleLUT.tags = tags;
     piclMPLHandleLUT.nHandles += PICLHandleInc;
     }
   else
     {
#ifdef FATAL_ERRORS
     fprintf (stderr, "Fatal Error: realloc failure in piclMPLSaveWaitH\n");
     exit(1);
#endif
     return;
     }
   }
 /* Stash the handle */
 piclMPLHandleLUT.tags[piclMPLHandleLUT.nUsed].tag = tag;
 piclMPLHandleLUT.tags[piclMPLHandleLUT.nUsed].Handle = *waitHandle;
 piclMPLHandleLUT.tags[piclMPLHandleLUT.nUsed].type = type ;
 piclMPLHandleLUT.tags[piclMPLHandleLUT.nUsed].nbytes = nbytes ;
 piclMPLHandleLUT.nUsed++;
}

/******************************************************
 * piclMPLISend (buff, mlen, tag, dest)
 *  Post a non blocking send an stash a wait handle
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/
 
void piclMPLISend (buff, mlen, tag, dest)
 char *buff;
 int mlen;
 int tag;
 int dest;
 {
   MPL_Request waitHandle;
   int rc ;
 
   if ( tag < type_low || tag > type_high )
   {
sprintf(mess,"PICL_SENDBEGIN message type %d out of allowed range: %d..%d\n",
tag,type_low,type_high) ;
PICL_TEST_ERR( 1, mess ) ;
   }

   rc = mpc_send  (buff,
              mlen,
              dest,
              tag,
              &waitHandle); 

#if 0
fprintf(stderr,"mpc_send: nlen %10d  type %10d  dest %10d  handle %08x\n",
mlen, tag, dest, waitHandle ) ;
#endif

   if ( rc )
   {
     sprintf(mess,"mpc_send returns %d", rc ) ;
     PICL_TEST_ERR( 1, mess ) ;
   }

   piclMPLSaveWaitH (tag, &waitHandle,1,mlen);
 }
 
/******************************************************
 * piclMPLIRecv (buff, mlen, tag)
 *  Post a non blocking receive an stash a wait handle
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/
 
void piclMPLIRecv (buff, mlen, tag)
 char *buff;
 int mlen;
 int tag;
 {
   MPL_Request waitHandle;
   int source ;
   int tagloc ;
   int rc ;

   source = dontcare ;
   tagloc = tag ;

   if ( tag < type_low || tag > type_high )
   {
sprintf(mess,"PICL_RECVBEGIN message type %d out of allowed range: %d..%d\n",
tag,type_low,type_high) ;
PICL_TEST_ERR( 1, mess ) ;
   }


   rc = mpc_recv  (buff,
              mlen,
              &source,
              &tagloc,
              &waitHandle);

#if 0
fprintf(stderr,"mpc_recv: nlen %10d  type %10d  source %10d  handle %08x\n",
mlen, tag, source, waitHandle ) ;
#endif

   if ( rc )
   {
     sprintf(mess,"mpc_recv returns %d", rc ) ;
     PICL_TEST_ERR( 1, mess ) ;
   }

   piclMPLSaveWaitH (tag, &waitHandle, 2, mlen);
 }

/******************************************************
 * piclMPLWait (tag)
 *  Wait for a pending send/recv
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/

extern int mperrno ;
 
void piclMPLWait (tag)
 int tag;
{
  MPL_Request waitHandle;
  MPL_Status status ;
  int rc ;
  int type, nbytes;

  if ( piclMPLFindWaitH (tag, &waitHandle, &type, &nbytes ) != 0 )
  {
    fprintf(stderr,"piclMPLWait: tag %d not found by piclMPLFindWaitH\n",tag) ;
    exit(2) ;
  }

#if 0
  fprintf(stderr,"calling mpc_wait: tag %d, handle %08x, type %d (%s), original nbytes %d\n",
        tag, waitHandle, type, (type==1)?"send":((type==2)?"recv":"unknown"),
	nbytes) ;
#endif

  rc = mpc_wait ( &waitHandle, &status  );
  if ( rc )
  {
    fprintf(stderr,"mpc_wait fails: tag %d, handle %08x, type %d (%s), original nbytes %d, status %d, rc %d, mperrno = %d\n",
        tag, waitHandle, type, (type==1)?"send":((type==2)?"recv":"unknown"),
	nbytes, status, rc, mperrno) ;
    exit(2) ;
  }

#if 0
  fprintf(stderr,"mpc_wait : tag %d, handle %08x, status %d\n",
        tag, waitHandle, status) ;
#endif

}

#endif /* __MPL_COMPAT__ */
#endif /* MPL */
