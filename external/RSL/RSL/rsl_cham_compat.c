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

/* this is still under construction  -- 11/94 */

#ifdef CHAMELEON
#ifndef __CHAM_COMPAT__
#define __CHAM_COMPAT__

#include <stdio.h>
#include "tools.h" 
#include "comm/comm.h"
#include "rsl.h"

#define RSLHandleInc 32

typedef long CHAM_request ;

struct tagsToHandles
       {
       int tag;
       char * buff ;
       int mlen ;
       int datatype ;
       CHAM_Request Handle;
       };
struct rslCHAMHandles
       {
       int nHandles;
       int nUsed;
       struct tagsToHandles *tags;
       } rslCHAMHandleLUT;

/******************************************************
 * rslCHAMInit ()
 *   do whatever initialization is necessary for the
 *   CHAM port
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *  Adapted to CHAM: J. Michalakes 11/94
 *
 *****************************************************/

static int dummy = 0 ;

void rslCHAMInit()
  {
  rslCHAMHandleLUT.nHandles = RSLHandleInc;
  rslCHAMHandleLUT.nUsed = 0;
  rslCHAMHandleLUT.tags = (struct tagsToHandles *) 
                     malloc (sizeof (struct tagsToHandles) * RSLHandleInc);

#ifdef FATAL_ERRORS
  if (rslCHAMHandleLUT.tags == NULL)
    {
    fprintf (stderr, "Fatal Error: malloc failure in rslCHAMInit\n");
    exit(1);
    }
#endif
  }


/******************************************************
 * rslCHAMWho ( numproc, myproc )
 *  Use the LUT to find an CHAM wait handle from a tag
 *
 *  Initial coding: J. Michalakes  7/13/94
 *
 *****************************************************/

long rslCHAMWho( numproc, myproc )
  int * numproc, * myproc ;
{
  *myproc = PImytid ;
  *numproc = PInumtids ;
  return( 0L ) ;
}

/******************************************************
 * rslCHAMFindWaitH (tag)
 *  Use the LUT to find an CHAM wait handle from a tag
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/

long rslCHAMFindWaitH (tag, waitHandle,
		       buff, mlen, datatype )
  int tag;   /* Tag for which we lookup a wait handle */
  CHAM_Request *waitHandle ;
  char ** buff ;
  int *mlen ;
  int *datatype ;
  {
  int i;
  long retVal = -1;

  for (i=0; i < rslCHAMHandleLUT.nUsed; i++)
    {
    if (rslCHAMHandleLUT.tags[i].tag == tag)
      {
      *buff       = rslCHAMHandleLUT.tags[i].buff;
      *mlen       = rslCHAMHandleLUT.tags[i].mlen;
      *datatype   = rslCHAMHandleLUT.tags[i].datatype;
      *waitHandle = rslCHAMHandleLUT.tags[i].Handle;

      rslCHAMHandleLUT.nUsed--;  /* Keep them contiguous */
      rslCHAMHandleLUT.tags[i].tag=rslCHAMHandleLUT.tags[rslCHAMHandleLUT.nUsed].tag;
      rslCHAMHandleLUT.tags[i].buff=
                  rslCHAMHandleLUT.tags[rslCHAMHandleLUT.nUsed].buff;
      rslCHAMHandleLUT.tags[i].mlen=
                  rslCHAMHandleLUT.tags[rslCHAMHandleLUT.nUsed].mlen;
      rslCHAMHandleLUT.tags[i].datatype=
                  rslCHAMHandleLUT.tags[rslCHAMHandleLUT.nUsed].datatype;
      rslCHAMHandleLUT.tags[i].Handle=
                  rslCHAMHandleLUT.tags[rslCHAMHandleLUT.nUsed].Handle;
      break;
      }
    }
  }

/******************************************************
 * rslCHAMSaveWaitH (tag, waitHandle)
 *  Use the LUT to save an CHAM wait handle referenced by a tag
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/
 
void rslCHAMSaveWaitH (tag, waitHandle)
 int tag;
 CHAM_Request * waitHandle;
 {
 /* Make sure there is enough space, if not, try a realloc */
 /* If the realloc fails we're in deep trouble */
 if (rslCHAMHandleLUT.nUsed == rslCHAMHandleLUT.nHandles)
   {
   struct tagsToHandles *tags;  /* Temp pointer */
   tags = (struct tagsToHandles *) 
      realloc (rslCHAMHandleLUT.tags, 
      sizeof (struct tagsToHandles) * (rslCHAMHandleLUT.nHandles + RSLHandleInc));
   if (tags != NULL)
     {
     rslCHAMHandleLUT.tags = tags;
     rslCHAMHandleLUT.nHandles += RSLHandleInc;
     }
   else
     {
#ifdef FATAL_ERRORS
     fprintf (stderr, "Fatal Error: realloc failure in rslCHAMSaveWaitH\n");
     exit(1);
#endif
     return;
     }
   }
 /* Stash the handle */
 rslCHAMHandleLUT.tags[rslCHAMHandleLUT.nUsed].tag = tag;
 rslCHAMHandleLUT.tags[rslCHAMHandleLUT.nUsed].Handle = *waitHandle;
 rslCHAMHandleLUT.nUsed++;
 }

/******************************************************
 * rslCHAMISend (buff, mlen, tag, dest)
 *  Post a non blocking send an stash a wait handle
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/
 
void rslCHAMISend (buff, mlen, tag, dest)
 char *buff;
 int mlen;
 int tag;
 int dest;
 {
   CHAM_Request waitHandle;
 
   PInsend(   tag,
	      buff,
	      mlen,
	      dest,
	      MSG_OTHER,
              &waitHandle); 

   rslCHAMSaveWaitH (tag, &waitHandle);
 }
 
/******************************************************
 * rslCHAMIRecv (buff, mlen, tag)
 *  Post a non blocking receive an stash a wait handle
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/
 
void rslCHAMIRecv (buff, mlen, tag)
 char *buff;
 int mlen;
 int tag;
 {
   CHAM_Request waitHandle;

   PInrecv   (tag,
              buff,
              mlen,
              MSG_OTHER,
              tag,
              &waitHandle);

   rslCHAMSaveWaitH (tag, waitHandle);
 }

/******************************************************
 * rslCHAMWait (tag)
 *  Wait for a pending send/recv
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/
 
void rslCHAMWait (tag, flag)
 int tag, flag;
 {
 CHAM_Request waitHandle;
 CHAM_Status status ;

 rslCHAMFindWaitH (tag, &waitHandle );
 if ( flag == 1 ) 		/* receive */
   CHAM_Wait ( &waitHandle, &status  );
 else				/* send */
   CHAM_Wait ( &waitHandle, &status  );
 }

#endif /* __CHAM_COMPAT__ */
#endif /* CHAM */
