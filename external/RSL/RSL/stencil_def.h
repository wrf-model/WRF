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



#ifndef STENCIL_DEF_H
#define STENCIL_DEF_H

typedef struct stencil_desc {
  rsl_tag_t tag;                        /* should be STENCIL_DESC */
  int sh ;		/* my descriptor */
  int has_f90_fields ;
  rsl_tag_t compiled[RSL_MAXDOMAINS];
  rsl_tag_t npts[RSL_MAXDOMAINS] ;
  rsl_tag_t maskid[RSL_MAXDOMAINS] ;
  message_desc_t *msgs[RSL_MAXDOMAINS][ RSL_MAXSTEN+1 ] ;
  rsl_procrec_t *procs[RSL_MAXDOMAINS] ;
  struct {
    int (*ptfcn)() ;
  } f[RSL_MAXDOMAINS] ;
} stencil_desc_t ;

int rsl_4pt() ;		/* forward declarations for pt functions */
int rsl_8pt() ;		/* forward declarations for pt functions */
int rsl_12pt() ;	/* forward declarations for pt functions */
int rsl_24pt() ;	/* forward declarations for pt functions */
int rsl_48pt() ;	/* forward declarations for pt functions */
int rsl_80pt() ;	/* forward declarations for pt functions */
int rsl_120pt() ;	/* forward declarations for pt functions */
#if (ALLOW_RSL_168PT == 1)
int rsl_168pt() ;	/* forward declarations for pt functions */
#endif
int rsl_2ptm() ;	/* forward declarations for pt functions */
int rsl_4ptm() ;	/* forward declarations for pt functions */

#endif  /* nothing after this line */

