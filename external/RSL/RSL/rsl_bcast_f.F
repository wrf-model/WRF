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

      subroutine rsl_bcast_chld( pd, nd, msize, mf, pf, upf )
      call cwrap_fbcast( pd, nd, msize, mf, pf, upf )
      return
      end

      subroutine rsl_f_bcast_chld( pd, nd, msize, buf, mf, pf, upf )
      implicit none
      integer pd                ! parent domain
      integer nd                ! nested domain
      integer msize             ! message size  in BYTES
      integer pf                ! packing function
      integer upf               ! unpacking function
      logical mf                ! mask function
      real buf(msize)
      include 'rsl.inc'
c local variables
      integer retval, i, j, ig, jg, dum
      integer njg, nig, n, pjg, pig, cn, cm

      call rsl_to_child_info( pd, nd, msize,
     +                        i,j,pig,pjg,cm,cn,nig,njg,retval )
      do while ( retval .eq. 1 )
        if ( mf( pd, nd, i, j, pig, pjg ) ) then
          dum = pf ( pd, nd, i, j, pig, pjg, 
     +              dum, dum,     nig, njg,
     +              cm, cn, buf, msize )
          call rsl_to_child_msg(msize,buf)
        endif
        call rsl_to_child_info( pd, nd, msize,
     +                          i,j,pig,pjg,cm,cn,nig,njg,retval )
      enddo

c exchange the messages
      call rsl_bcast_msgs

c unpack the messages from children of points in this domain

      call rsl_from_parent_info( i, j, nig, njg, cm, cn, pig, pjg, 
     +                           retval )
      do while ( retval .eq. 1 )
	call rsl_from_parent_msg( msize, buf )
        dum = upf( pd, nd, i, j, nig, njg, cm, cn, buf, msize )
        call rsl_from_parent_info( i, j, nig, njg, cm, cn, pig, pjg,
     +                             retval )
      enddo

      return
      end

