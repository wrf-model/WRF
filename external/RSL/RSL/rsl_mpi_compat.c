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

#ifdef MPI
#ifndef __MPI_COMPAT__
#define __MPI_COMPAT__

#include <stdio.h>
#include "mpi.h" 
#include "rsl.h"

#define RSLHandleInc 32


struct tagsToHandles
       {
       int tag;
       MPI_Request Handle;
       };
struct rslMPIHandles
       {
       int nHandles;
       int nUsed;
       struct tagsToHandles *tags;
       } rslMPIHandleLUT;

/******************************************************
 * rslMPIInit ()
 *   do whatever initialization is necessary for the
 *   MPI port
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *  Adapted to MPI: J. Michalakes 7/13/94
 *
 *****************************************************/

static int dummy = 0 ;

#ifdef linux
int xargc ;
#endif

void rslMPIInit()
  {
  int flag ;
  rslMPIHandleLUT.nHandles = RSLHandleInc;
  rslMPIHandleLUT.nUsed = 0;
  rslMPIHandleLUT.tags = (struct tagsToHandles *) 
                     malloc (sizeof (struct tagsToHandles) * RSLHandleInc);

  MPI_Initialized( &flag ) ;

  if ( ! flag ) {

#ifndef linux
  MPI_INIT_F ( &dummy ) ;  /* call to fortran wrapper */
#else
  xargc = iargc_()+1;
#  ifdef F2CSTYLE
  mpi_init__( &dummy ) ;
#  else
  mpi_init_( &dummy ) ;
#  endif
#endif

  }

#ifdef FATAL_ERRORS
  if (rslMPIHandleLUT.tags == NULL)
    {
    fprintf (stderr, "Fatal Error: malloc failure in rslMPIInit\n");
    exit(1);
    }
#endif
  }


/******************************************************
 * rslMPIWho ( numproc, myproc )
 *  Use the LUT to find an MPI wait handle from a tag
 *
 *  Initial coding: J. Michalakes  7/13/94
 *
 *****************************************************/

long rslMPIWho( numproc, myproc )
  int * numproc, * myproc ;
{
  MPI_Comm_rank( rsl_mpi_communicator, myproc ) ;
  MPI_Comm_size( rsl_mpi_communicator, numproc ) ;
  return( 0L ) ;
}

/******************************************************
 * rslMPIFindWaitH (tag)
 *  Use the LUT to find an MPI wait handle from a tag
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/

long rslMPIFindWaitH (tag, waitHandle)
  int tag;   /* Tag for which we lookup a wait handle */
  MPI_Request *waitHandle ;
  {
  int i;
  long retVal = -1;

  for (i=0; i < rslMPIHandleLUT.nUsed; i++)
    {
    if (rslMPIHandleLUT.tags[i].tag == tag)
      {
      *waitHandle = rslMPIHandleLUT.tags[i].Handle;
      rslMPIHandleLUT.nUsed--;  /* Keep them contiguous */
      rslMPIHandleLUT.tags[i].tag=rslMPIHandleLUT.tags[rslMPIHandleLUT.nUsed].tag;
      rslMPIHandleLUT.tags[i].Handle=
                  rslMPIHandleLUT.tags[rslMPIHandleLUT.nUsed].Handle;
      break;
      }
    }
    return(0L) ;
  }

/******************************************************
 * rslMPISaveWaitH (tag, waitHandle)
 *  Use the LUT to save an MPI wait handle referenced by a tag
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/
 
void rslMPISaveWaitH (tag, waitHandle)
 int tag;
 MPI_Request * waitHandle;
 {
 /* Make sure there is enough space, if not, try a realloc */
 /* If the realloc fails we're in deep trouble */
 if (rslMPIHandleLUT.nUsed == rslMPIHandleLUT.nHandles)
   {
   struct tagsToHandles *tags;  /* Temp pointer */
   tags = (struct tagsToHandles *) 
      realloc (rslMPIHandleLUT.tags, 
      sizeof (struct tagsToHandles) * (rslMPIHandleLUT.nHandles + RSLHandleInc));
   if (tags != NULL)
     {
     rslMPIHandleLUT.tags = tags;
     rslMPIHandleLUT.nHandles += RSLHandleInc;
     }
   else
     {
#ifdef FATAL_ERRORS
     fprintf (stderr, "Fatal Error: realloc failure in rslMPISaveWaitH\n");
     exit(1);
#endif
     return;
     }
   }
 /* Stash the handle */
 rslMPIHandleLUT.tags[rslMPIHandleLUT.nUsed].tag = tag;
 rslMPIHandleLUT.tags[rslMPIHandleLUT.nUsed].Handle = *waitHandle;
 rslMPIHandleLUT.nUsed++;
 }

/******************************************************
 * rslMPIISend (buff, mlen, tag, dest)
 *  Post a non blocking send an stash a wait handle
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/
 
void rslMPIISend (buff, mlen, tag, dest)
 char *buff;
 int mlen;
 int tag;
 int dest;
 {
   MPI_Request waitHandle;
 
   MPI_Isend (buff,
              mlen,
              MPI_BYTE,
              dest,
              tag,
              rsl_mpi_communicator,
              &waitHandle); 

   rslMPISaveWaitH (tag, &waitHandle);
 }
 
/******************************************************
 * rslMPIIRecv (buff, mlen, tag)
 *  Post a non blocking receive an stash a wait handle
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/
 
void rslMPIIRecv (buff, mlen, tag)
 char *buff;
 int mlen;
 int tag;
 {
   MPI_Request waitHandle;

   MPI_Irecv (buff,
              mlen,
              MPI_BYTE,
              MPI_ANY_SOURCE,
              tag,
              rsl_mpi_communicator,
              &waitHandle);

   rslMPISaveWaitH (tag, &waitHandle);
 }

/******************************************************
 * rslMPIWait (tag)
 *  Wait for a pending send/recv
 *
 *  Initial coding: Leslie Hart, 22 Apr 94
 *
 *****************************************************/
 
void rslMPIWait (tag)
 int tag;
 {
 MPI_Request waitHandle;
 MPI_Status status ;

 rslMPIFindWaitH (tag, &waitHandle );
 (void) MPI_Wait ( &waitHandle, &status  );
 }

#endif /* __MPI_COMPAT__ */
#endif /* MPI */
