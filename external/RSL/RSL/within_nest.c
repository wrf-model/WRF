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

/* note -- only works for regular nests right now */
/* note also, these indices come is as FORTRAN  (1..n)*/

/* 1/10/95, BUG: should these be converted to use eff_m, eff_n
   instead of len_m, len_n? */

RSL_WITHIN_NESTED_BOUNDARY ( wdth_p, pd_p, nd_p, ig_p, jg_p, retval_p )
  int_p wdth_p,         /* width of boundary in nested domain cells */
        pd_p,           /* descriptor to parent -- this domain */
        nd_p,           /* descriptor for nest */
        ig_p, jg_p,     /* coordinates of coarse domain cell */
        retval_p ;      /* return 1 if on boundary, 0 otherwise */
{
  int wdth, pd, nd, ig, jg, retval ;
  int cwdth ;  /* coarse domain width (ceiling of wdth / IRAX) */
  int swi, swj, leni, lenj, rax ;

  wdth = *wdth_p ; pd = *pd_p ; nd = *nd_p ;
  ig = *ig_p - 1 ; jg = *jg_p - 1 ;

  rax = domain_info[nd].irax_m ;  /* TODO -- BUG -- if irax_m and irax_n differ, hozed */

  cwdth = wdth / rax + ((wdth%rax == 0)?0:1) ;
  swi = domain_info[nd].coord_m ;
  swj = domain_info[nd].coord_n ;
  leni = domain_info[nd].len_m /  domain_info[nd].irax_m ;
  lenj = domain_info[nd].len_n /  domain_info[nd].irax_n ;

  retval = 0 ;
  if ( jg >= swj && ig >= swi  &&
       jg < swj + lenj && ig < swi + leni )
  {
      
  /* check western */
  if      ( jg - swj < cwdth ) 
    retval = 1 ;
  /* check eastern */
  else if ( swj + lenj - jg <= cwdth )
    retval = 1 ;
  /* check southern */
  if      ( ig - swi < cwdth ) 
    retval = 1 ;
  /* check northern */
  else if ( swi + leni - ig <= cwdth )
    retval = 1 ;

  }

  *retval_p = retval ;
}

/* like above, but gives an extra row and column on north and east
   respectively */

RSL_WITHIN_NESTED_BETA ( wdth_p, pd_p, nd_p, ig_p, jg_p, retval_p )
  int_p wdth_p,         /* width of boundary in nested domain cells */
        pd_p,           /* descriptor to parent -- this domain */
        nd_p,           /* descriptor for nest */
        ig_p, jg_p,     /* coordinates of coarse domain cell */
        retval_p ;      /* return 1 if on boundary, 0 otherwise */
{
  int wdth, pd, nd, ig, jg, retval ;
  int cwdth ;  /* coarse domain width (ceiling of wdth / IRAX) */
  int swi, swj, leni, lenj, rax ;

  wdth = *wdth_p ; pd = *pd_p ; nd = *nd_p ;
  ig = *ig_p - 1 ; jg = *jg_p - 1 ;

  rax = domain_info[nd].irax_m ;  /* TODO -- BUG -- if irax_m and irax_n differ,
 hozed */

  cwdth = wdth / rax + ((wdth%rax == 0)?0:1) ;
  swi = domain_info[nd].coord_m ;
  swj = domain_info[nd].coord_n ;
  leni = domain_info[nd].len_m /  domain_info[nd].irax_m ;
  lenj = domain_info[nd].len_n /  domain_info[nd].irax_n ;

  retval = 0 ;
  if ( jg >= swj && ig >= swi  &&
       jg < swj + lenj && ig < swi + leni )
  {
      
  /* check western */
  if      ( jg - swj < cwdth ) 
    retval = 1 ;
  /* check eastern */
  else if ( swj + lenj - jg <= cwdth+1 )
    retval = 1 ;
  /* check southern */
  if      ( ig - swi < cwdth ) 
    retval = 1 ;
  /* check northern */
  else if ( swi + leni - ig <= cwdth+1 )
    retval = 1 ;

  }

  *retval_p = retval ;
}
