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
#include "rsl.h"
#include "rsl_comm.h"

typedef int MPL_Request ;
typedef int MPL_Status ;

#define RSLHandleInc 32

#define FATAL_ERRORS


struct tagsToHandles
       {
       int tag;
       MPL_Request Handle;
       int type ;		/* send = 1, recv = 2 */
       int nbytes ;
       };
struct rslMPLHandles
       {
       int nHandles;
       int nUsed;
       struct tagsToHandles *tags;
       } rslMPLHandleLUT;

/******************************************************
 * rslMPLInit ()
 *   do whatever initialization is necessary for the
 *   MPL port
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *  Adapted to MPL: J. Michalakes 7/13/94
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

void rslMPLInit()
{
  int nbuf[4] ;
   
  rslMPLHandleLUT.nHandles = RSLHandleInc;
  rslMPLHandleLUT.nUsed = 0;
  rslMPLHandleLUT.tags = (struct tagsToHandles *) 
                     malloc (sizeof (struct tagsToHandles) * RSLHandleInc);

  mpc_task_query( nbuf, 2, 2 ) ;
  type_low = nbuf[0] ;
  type_high= nbuf[1] ;
  mpc_task_query( nbuf, 4, 3 ) ;
  dontcare = nbuf[0] ;
  allmsg   = nbuf[1] ;
  nulltask = nbuf[2] ;
  allgrp   = nbuf[3] ;

#if 0
  fprintf(stderr,"rslMPLInit: \n") ;
  fprintf(stderr,"type_low:  %d\n",type_low) ;
  fprintf(stderr,"type_high: %d\n",type_high) ;
  fprintf(stderr,"dontcare:  %d\n",dontcare) ;
  fprintf(stderr,"allmsg:    %d\n",allmsg) ;
  fprintf(stderr,"nulltask:  %d\n",nulltask) ;
  fprintf(stderr,"allgrp:    %d\n",allgrp) ;
#endif


#ifdef FATAL_ERRORS
  if (rslMPLHandleLUT.tags == NULL)
    {
    fprintf (stderr, "Fatal Error: malloc failure in rslMPLInit\n");
    exit(1);
    }
#endif
}


/******************************************************
 * rslMPLFindWaitH (tag)
 *  Use the LUT to find an MPL wait handle from a tag
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/

long rslMPLFindWaitH (tag, waitHandle, type, nbytes )
  int tag;   /* Tag for which we lookup a wait handle */
  MPL_Request *waitHandle ;
  int *type, *nbytes ;
{
  int i;
  long retVal = -1;

  for (i=0; i < rslMPLHandleLUT.nUsed; i++)
  {
    if (rslMPLHandleLUT.tags[i].tag == tag)
    {
      *waitHandle = rslMPLHandleLUT.tags[i].Handle;
      *type = rslMPLHandleLUT.tags[i].type;
      *nbytes = rslMPLHandleLUT.tags[i].nbytes;
      rslMPLHandleLUT.nUsed--;  /* Keep them contiguous */
      rslMPLHandleLUT.tags[i].tag=rslMPLHandleLUT.tags[rslMPLHandleLUT.nUsed].tag;
      rslMPLHandleLUT.tags[i].Handle=
                  rslMPLHandleLUT.tags[rslMPLHandleLUT.nUsed].Handle;
      rslMPLHandleLUT.tags[i].type=
		  rslMPLHandleLUT.tags[rslMPLHandleLUT.nUsed].type;
      rslMPLHandleLUT.tags[i].nbytes=
		  rslMPLHandleLUT.tags[rslMPLHandleLUT.nUsed].nbytes ;
      retVal = 0 ;
      break;
    }
  }
  return( retVal ) ;
}

/* same as above, but leaves list alone */
long rslMPLPeekWaitH (tag, waitHandle, type, nbytes )
  int tag;   /* Tag for which we lookup a wait handle */
  MPL_Request *waitHandle ;
  int *type, *nbytes ;
{
  int i;
  long retVal = -1;

  for (i=0; i < rslMPLHandleLUT.nUsed; i++)
  {
    if (rslMPLHandleLUT.tags[i].tag == tag)
    {
      *waitHandle = rslMPLHandleLUT.tags[i].Handle;
      *type = rslMPLHandleLUT.tags[i].type;
      *nbytes = rslMPLHandleLUT.tags[i].nbytes;
      retVal = 0 ;
      break;
    }
  }
  return( retVal ) ;
}


/******************************************************
 * rslMPLSaveWaitH (tag, waitHandle)
 *  Use the LUT to save an MPL wait handle referenced by a tag
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/
 
void rslMPLSaveWaitH (tag, waitHandle,type,nbytes)
 int tag;
 MPL_Request * waitHandle;
 int type, nbytes ;
{
 /* Make sure there is enough space, if not, try a realloc */
 /* If the realloc fails we're in deep trouble */
 if (rslMPLHandleLUT.nUsed == rslMPLHandleLUT.nHandles)
   {
   struct tagsToHandles *tags;  /* Temp pointer */
   tags = (struct tagsToHandles *) 
      realloc (rslMPLHandleLUT.tags, 
      sizeof (struct tagsToHandles) * (rslMPLHandleLUT.nHandles + RSLHandleInc));
   if (tags != NULL)
     {
     rslMPLHandleLUT.tags = tags;
     rslMPLHandleLUT.nHandles += RSLHandleInc;
     }
   else
     {
#ifdef FATAL_ERRORS
     fprintf (stderr, "Fatal Error: realloc failure in rslMPLSaveWaitH\n");
     exit(1);
#endif
     return;
     }
   }
 /* Stash the handle */
 rslMPLHandleLUT.tags[rslMPLHandleLUT.nUsed].tag = tag;
 rslMPLHandleLUT.tags[rslMPLHandleLUT.nUsed].Handle = *waitHandle;
 rslMPLHandleLUT.tags[rslMPLHandleLUT.nUsed].type = type ;
 rslMPLHandleLUT.tags[rslMPLHandleLUT.nUsed].nbytes = nbytes ;
 rslMPLHandleLUT.nUsed++;
}

/******************************************************
 * rslMPLISend (buff, mlen, tag, dest)
 *  Post a non blocking send an stash a wait handle
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/
 
void rslMPLISend (buff, mlen, tag, dest)
 char *buff;
 int mlen;
 int tag;
 int dest;
 {
   MPL_Request waitHandle;
   int rc ;
 
   if ( tag < type_low || tag > type_high )
   {
sprintf(mess,"RSL_SENDBEGIN message type %d out of allowed range: %d..%d\n",
tag,type_low,type_high) ;
RSL_TEST_ERR( 1, mess ) ;
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
     RSL_TEST_ERR( 1, mess ) ;
   }

   rslMPLSaveWaitH (tag, &waitHandle,1,mlen);
 }
 
/******************************************************
 * rslMPLIRecv (buff, mlen, tag)
 *  Post a non blocking receive an stash a wait handle
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/
 
void rslMPLIRecv (buff, mlen, tag)
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
sprintf(mess,"RSL_RECVBEGIN message type %d out of allowed range: %d..%d\n",
tag,type_low,type_high) ;
RSL_TEST_ERR( 1, mess ) ;
   }


   rc = mpc_recv  (buff,
              mlen,
              &source,
              &tagloc,
              &waitHandle);

   if ( rc )
   {
     sprintf(mess,"mpc_recv returns %d", rc ) ;
     RSL_TEST_ERR( 1, mess ) ;
   }

/* fprintf(stderr,"rslMPLIRecv tag = %d, handle = %d\n",tag,waitHandle) ; */

   rslMPLSaveWaitH (tag, &waitHandle, 2, mlen);
 }

/******************************************************
 * rslMPLWait (tag)
 *  Wait for a pending send/recv
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/

extern int mperrno ;
 
void rslMPLWait (tag)
 int tag;
{
  MPL_Request waitHandle;
  MPL_Status status ;
  int rc ;
  int type, nbytes;

  if ( rslMPLFindWaitH (tag, &waitHandle, &type, &nbytes ) != 0 )
  {
    fprintf(stderr,"rslMPLWait: tag %d not found by rslMPLFindWaitH\n",tag) ;
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


MPL_Request d1, d2, d3, waitHandle, d4, d5, d6 ;

rslMPLProbe( tag, retval )
  int tag, *retval ;
{ 
  MPL_Status status ;
  int rc ;
  int type, nbytes ;

  if ( rslMPLPeekWaitH (tag, &waitHandle, &type, &nbytes ) != 0 )
  {
    fprintf(stderr,"rslMPLWait: tag %d not found by rslMPLPeekWaitH\n",tag) ;
    exit(2) ;
  }
  rc = mpc_status( waitHandle ) ;

  if ( rc >= 0 )
  {
    *retval = 1 ;
  }
  else if ( rc == -1 )
  {
    *retval = 0 ;
  }
  else
  {
    sprintf(mess,"No outstanding message for tag %d (handle %d) rc=%d\n",tag,waitHandle,rc) ;
    RSL_TEST_ERR(1,mess) ;
  }
  return ;
}

#endif /* __MPL_COMPAT__ */
#endif /* MPL */
