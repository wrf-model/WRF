CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     
C                              COPYRIGHT
C     
C     The following is a notice of limited availability of the code and 
C     Government license and disclaimer which must be included in the 
C     prologue of the code and in all source listings of the code.
C     
C     Copyright notice
C       (c) 1977  University of Chicago
C     
C     Permission is hereby granted to use, reproduce, prepare 
C     derivative works, and to redistribute to others at no charge.  If 
C     you distribute a copy or copies of the Software, or you modify a 
C     copy or copies of the Software or any portion of it, thus forming 
C     a work based on the Software and make and/or distribute copies of 
C     such work, you must meet the following conditions:
C     
C          a) If you make a copy of the Software (modified or verbatim) 
C             it must include the copyright notice and Government       
C             license and disclaimer.
C     
C          b) You must cause the modified Software to carry prominent   
C             notices stating that you changed specified portions of    
C             the Software.
C     
C     This software was authored by:
C     
C     Argonne National Laboratory
C     J. Michalakes: (630) 252-6646; email: michalak@mcs.anl.gov
C     Mathematics and Computer Science Division
C     Argonne National Laboratory, Argonne, IL  60439
C     
C     ARGONNE NATIONAL LABORATORY (ANL), WITH FACILITIES IN THE STATES 
C     OF ILLINOIS AND IDAHO, IS OWNED BY THE UNITED STATES GOVERNMENT, 
C     AND OPERATED BY THE UNIVERSITY OF CHICAGO UNDER PROVISION OF A 
C     CONTRACT WITH THE DEPARTMENT OF ENERGY.
C     
C                      GOVERNMENT LICENSE AND DISCLAIMER
C     
C     This computer code material was prepared, in part, as an account 
C     of work sponsored by an agency of the United States Government.
C     The Government is granted for itself and others acting on its 
C     behalf a paid-up, nonexclusive, irrevocable worldwide license in 
C     this data to reproduce, prepare derivative works, distribute 
C     copies to the public, perform publicly and display publicly, and 
C     to permit others to do so.  NEITHER THE UNITED STATES GOVERNMENT 
C     NOR ANY AGENCY THEREOF, NOR THE UNIVERSITY OF CHICAGO, NOR ANY OF 
C     THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR 
C     ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, 
C     COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, 
C     PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
C     NOT INFRINGE PRIVATELY OWNED RIGHTS.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C

      logical function rsl_iammonitor()           ! wrapper
      implicit none
      external rsl_c_iammonitor
      integer retval
      call rsl_c_iammonitor( retval ) 
      if ( retval .eq. 1 ) then
         rsl_iammonitor = .true.
      else
         rsl_iammonitor = .false.
      endif
      return
      end

      logical function rsl_iamcompute()           ! wrapper
      implicit none
      external rsl_c_iamcompute
      integer retval
      call rsl_c_iamcompute( retval )  
      if ( retval .eq. 1 ) then
         rsl_iamcompute = .true.
      else
         rsl_iamcompute = .false.
      endif
      return
      end

      integer function rsl_phys2comp( p )
      implicit none
      external rsl_phys2comp_c
      integer retval, p
      call  rsl_phys2comp_c( p, retval )
      rsl_phys2comp = retval
      return
      end

      integer function rsl_comp2phys( p )
      implicit none
      external rsl_comp2phys_c
      integer retval, p
      call  rsl_comp2phys_c( p, retval )
      rsl_comp2phys = retval
      return
      end
