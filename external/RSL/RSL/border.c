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

static int zero = 0 ;

rsl_processor_t
locproc( i, m, numproc )
  rsl_index_t i ;
  rsl_dimlen_t m ;
  rsl_processor_t numproc ;
{
  int val ;
  rsl_processor_t retval ;

  int ii, im, inumproc ;

  ii = i ; im = m ; inumproc = numproc ;
  c_locproc( &ii, &im, &inumproc, &zero, &zero, &val ) ;
  retval = val ;
  return( retval ) ;
}
  


/***************************************************************************
 patchmap routine:
      Given position(p) in the global dimension (max) in the grid, with
      margins of size ml (left/top) and mr (right/bottom), divide it among
      the nproc processors in that dimension so the margins get at least
      the margin and no more than width+1 processes.

                          J A Mogill         19 April 1993
****************************************************************************/

c_locproc(p, max, nproc, ml, mr, ret)
int *p, *max, *nproc, *ml, *mr, *ret;

{ 
  int width, rem, ret2, bl, br, mid, adjust;

  int p_r, max_r, nproc_r, zero ;

  adjust = 0;
  rem = *max%*nproc;
  width = *max/(*nproc);
  mid = *max/2;

  if(rem>0 && ((rem%2==0 || rem>2) || *p<=mid ))
     width++;
  if(*p<=mid && rem%2!=0)
     adjust++;

  bl = maximum(width,*ml);
  br = maximum(width,*mr);

  if(*p<bl)
     *ret = 0;
  else if(*p>*max-br-1)
     *ret = *nproc-1;
  else
     {
       p_r = *p-bl ;
       max_r = *max-bl-br+adjust ;
       nproc_r = maximum(*nproc-2,1) ;
       zero = 0 ;

       c_locproc( &p_r, &max_r, &nproc_r, &zero, &zero, &ret2);
       *ret = ret2 + 1;
     }
}






int maximum(x,y)
int x,y;

 {
   if(x>=y)
     return(x);
   else
     return(y);
 }

#if 0
main()
{
   int i, m, numproc ;
   m = 61 ;
   numproc = 10 ;
   for ( i = 0; i < 61 ; i++ ) 
   {
   printf("locproc(%d,%d,%d) = %d\n",i,m,numproc,locproc(i,m,numproc)) ;
   }
}
#endif



