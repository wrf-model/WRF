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

#ifdef PGON
#ifndef __PGON_COMPAT__
#define __PGON_COMPAT__
#include <stdio.h>
#include <nx.h>

#define RSLHandleInc 32

extern int errno ;

struct tagsToHandles
       {
       int tag;
       long nxHandle;
       };
struct rslNXHandles
       {
       int nHandles;
       int nUsed;
       struct tagsToHandles *tags;
       } rslNXHandleLUT;

/******************************************************
 * rslNXInit ()
 *   do whatever initialization is necessary for the
 *   NX port
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/
void rslNXInit()
  {
  rslNXHandleLUT.nHandles = RSLHandleInc;
  rslNXHandleLUT.nUsed = 0;
  rslNXHandleLUT.tags = (struct tagsToHandles *) 
                     malloc (sizeof (struct tagsToHandles) * RSLHandleInc);
#ifdef FATAL_ERRORS
  if (rslNXHandleLUT.tags == NULL)
    {
    fprintf (stderr, "Fatal Error: malloc failure in rslNXInit\n");
    exit(1);
    }
#endif
  }

/******************************************************
 * rslNXFindWaitH (tag)
 *  Use the LUT to find an NX wait handle from a tag
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/

long rslNXFindWaitH (tag)
  int tag;   /* Tag for which we lookup a wait handle */
  {
  int i;
  long retVal = -1;

  for (i=0; i < rslNXHandleLUT.nUsed; i++)
    {
    if (rslNXHandleLUT.tags[i].tag == tag)
      {
      retVal = rslNXHandleLUT.tags[i].nxHandle;
      rslNXHandleLUT.nUsed--;  /* Keep them contiguous */
      rslNXHandleLUT.tags[i].tag=rslNXHandleLUT.tags[rslNXHandleLUT.nUsed].tag;
      rslNXHandleLUT.tags[i].nxHandle=
                  rslNXHandleLUT.tags[rslNXHandleLUT.nUsed].nxHandle;
      break;
      }
    }
  return retVal;
  }

/******************************************************
 * rslNXPeekWaitH (tag)
 *  Use the LUT to find an NX wait handle from a tag
 *
 * same as above but does not remove from list.  JM. 9/27/94
 *
 *****************************************************/

long rslNXPeekWaitH (tag)
  int tag;   /* Tag for which we lookup a wait handle */
  {
  int i;
  long retVal = -1;

  for (i=0; i < rslNXHandleLUT.nUsed; i++)
    {
    if (rslNXHandleLUT.tags[i].tag == tag)
      {
      retVal = rslNXHandleLUT.tags[i].nxHandle;
      break;
      }
    }
  return retVal;
  }



/******************************************************
 * rslNXSaveWaitH (tag, waitHandle)
 *  Use the LUT to save an NX wait handle referenced by a tag
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/
 
void rslNXSaveWaitH (tag, waitHandle)
 int tag;
 long waitHandle;
 {
 /* Make sure there is enough space, if not, try a realloc */
 /* If the realloc fails we're in deep trouble */
 if (rslNXHandleLUT.nUsed == rslNXHandleLUT.nHandles)
   {
   struct tagsToHandles *tags;  /* Temp pointer */
   tags = (struct tagsToHandles *) 
      realloc (rslNXHandleLUT.tags, 
      sizeof (struct tagsToHandles) * (rslNXHandleLUT.nHandles + RSLHandleInc));
   if (tags != NULL)
     {
     rslNXHandleLUT.tags = tags;
     rslNXHandleLUT.nHandles += RSLHandleInc;
     }
   else
     {
#ifdef FATAL_ERRORS
     fprintf (stderr, "Fatal Error: realloc failure in rslNXSaveWaitH\n");
     exit(1);
#endif
     return;
     }
   }
 /* Stash the handle */
 rslNXHandleLUT.tags[rslNXHandleLUT.nUsed].tag = tag;
 rslNXHandleLUT.tags[rslNXHandleLUT.nUsed].nxHandle = waitHandle;
 rslNXHandleLUT.nUsed++;
 }

/******************************************************
 * rslNXISend (buff, mlen, tag, dest)
 *  Post a non blocking send an stash a wait handle
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/
 
void rslNXISend (buff, mlen, tag, dest)
 char *buff;
 int mlen;
 int tag;
 int dest;
 {
 long waitHandle;
 
 waitHandle = isend ((long) tag, buff, (long) mlen, (long) dest, (long) 0);
 rslNXSaveWaitH (tag, waitHandle);
 }
 
/******************************************************
 * rslNXIRecv (buff, mlen, tag)
 *  Post a non blocking receive an stash a wait handle
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/
 
void rslNXIRecv (buff, mlen, tag)
 char *buff;
 int mlen;
 int tag;
 {
 long waitHandle;

 waitHandle = irecv ((long) tag, buff, (long) mlen);
 rslNXSaveWaitH (tag, waitHandle);
 }

/******************************************************
 * rslNXWait (tag)
 *  Wait for a pending send/recv
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/
 
void rslNXWait (tag)
 int tag;
 {
 long waitHandle;

 waitHandle = rslNXFindWaitH (tag);
 (void) msgwait (waitHandle);
 }

/******************************************************
 * rslNXProbe (tag)
 *  check for pending receive
 *
 * added 9/27/94  jm
 *
 *****************************************************/
 
rslNXProbe( tag, retval )
  int tag, *retval ;
{
  long waitHandle;
  long status ;
  int rc ;
  int type, nbytes ;


  waitHandle = rslNXPeekWaitH (tag) ;
  rc = _msgdone( waitHandle ) ;

  if ( rc == 1 )
  {
/* message received... now make call that will remove tag from LUT.
(on other systems, there would be a message wait later after this
call that would do this, but on the Paragon there won't be.  Presently,
RSL_PROBE is only called in exch_sten.c, and there is special paragon
code there to ensure this.)  940927.  JM */
    waitHandle = rslNXFindWaitH (tag) ;
    *retval = 1 ;
  }
  else if ( rc == 0 )
  {
    *retval = 0 ;
  }
  else
  {
    fprintf(stderr,"Error in _msgdone(%d) for tag=%d.  errno=%d.\n",
		    waitHandle, tag, errno ) ;
    *retval = 0 ;
  }

  return ;
}


#endif /* __PGON_COMPAT__ */
#endif /* PGON */
