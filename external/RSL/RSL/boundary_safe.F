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

      subroutine boundary_safe( pid, bwdth, ix, jx, il, jl )
      implicit none

      integer bwdth		! boundary width
      integer ix, jx		! static dimensions
      integer il, jl		! effective dimensions
      integer pid( ix, jx )	! map array

      integer i, j, p, q, problem, strat, a, b

c boundary safe in east-west -- we do not want the e/w boundaries
c decomposed in this dimension. 

c check western boundary
      do i = 1, ix
        problem = 0
        p = pid(i,1)
        do j = 1,bwdth
          if ( p .ne. pid(i,j) .and. problem .eq. 0 ) problem = j
        enddo
        if ( problem .ne. 0 ) then

           q = pid(i,problem)
	   strat = 1
           do j = problem, problem+bwdth-1
             if ( pid(i,j) .ne. q ) then
	       if ( j .le. bwdth ) then		! no hope for it
                 write(0,*)'wb: problem with mapper: use patchmap'
                 goto 890
	       endif
	       strat = 2
             endif
           enddo

	   if ( strat .eq. 1 ) then		! stategy 1 -- move offending
						! point(s) from boundary area
             j = problem
             do q = 1, bwdth
               pid(i,q) = pid(i,j)
               j = j+1
             enddo
             j = bwdth+1
             do q = 1, problem-1
               pid(i,j) = p
               j = j+1
             enddo
	   else					! strategy 2 -- make all one
						! or the other
c	     write(0,*) 'wb using strat 2 on i=',i
	     a = problem - 1		! # points on first proc
	     b = bwdth - a 		! # points on second
	     if ( a .lt. b ) then
	       do j = 1, a
		 pid( i, j ) = pid( i, bwdth ) 
	       enddo
	     else
	       do j = a+1, bwdth
		 pid( i, j ) = pid( i, 1 ) 
	       enddo
	     endif
	   endif
        endif
      enddo
c check southern boundary
      do j = 1, jx
        problem = 0
        p = pid(1,j)
        do i = 1,bwdth
          if ( p .ne. pid(i,j) .and. problem .eq. 0 ) problem = i
        enddo
        if ( problem .ne. 0 ) then

           q = pid(problem,j)
           strat = 1
           do i = problem, problem+bwdth-1
             if ( pid(i,j) .ne. q ) then
               if ( i .le. bwdth ) then           ! no hope for it
                 write(0,*)'sb: problem with mapper: use patchmap'
                 goto 890
               endif
               strat = 2
             endif
           enddo

           if ( strat .eq. 1 ) then             ! stategy 1 -- move offending
                                                ! point(s) from boundary area
             i = problem
             do q = 1, bwdth
               pid(q,j) = pid(i,j)
               i = i+1
             enddo
             i = bwdth+1
             do q = 1, problem-1
               pid(i,j) = p
               i = i+1
             enddo
           else                                 ! strategy 2 -- make all one
                                                ! or the other
c             write(0,*) 'sb using strat 2 on j=',j
             a = problem - 1            ! # points on first proc
             b = bwdth - a              ! # points on second
             if ( a .lt. b ) then
               do i = 1, a
                 pid( i, j ) = pid( bwdth, j )
               enddo
             else
               do i = a+1, bwdth
                 pid( i, j ) = pid( 1, j )
               enddo
             endif
           endif
        endif
      enddo
c check eastern boundary
      do i = 1, ix
        problem = 0
        p = pid(i,jl)
        do j = jl,jl-bwdth,-1   ! this covers bwdth+1 cells for beta grid
          if ( p .ne. pid(i,j) .and. problem .eq. 0 ) problem = j
        enddo
        if ( problem .ne. 0 ) then

           q = pid(i,problem)
           strat = 1
           do j = problem, problem-bwdth, -1
             if ( pid(i,j) .ne. q ) then
               if ( j .ge. jl-bwdth+1 ) then              ! no hope for it
                 write(0,*)'eb: problem with mapper: use patchmap'
                 goto 890
               endif
               strat = 2
             endif
           enddo

           if ( strat .eq. 1 ) then             ! stategy 1 -- move offending
                                                ! point(s) from boundary area
             j = problem
             do q = jl,jl-bwdth,-1
               pid(i,q) = pid(i,j)
               j = j-1
             enddo
             j = jl-bwdth-1
             do q = jl,problem+1,-1
               pid(i,j) = p
               j = j-1
             enddo
           else                                 ! strategy 2 -- make all one
                                                ! or the other
c             write(0,*) 'eb using strat 2 on i=',i
             a = jl - problem           ! # points on first proc
             b = bwdth - a              ! # points on second
             if ( a .lt. b ) then
               do j = jl, jl-a+1, -1
                 pid( i, j ) = pid( i, jl-bwdth+1 )
               enddo
             else
               do j = jl-a, jl-bwdth+1, -1
                 pid( i, j ) = pid( i, jl )
               enddo
             endif
           endif
        endif
      enddo
c check northern boundary
      do j = 1, jx
        problem = 0
        p = pid(il,j)
        do i = il,il-bwdth,-1   ! this covers bwdth+1 cells for beta grid
          if ( p .ne. pid(i,j) .and. problem .eq. 0 ) problem = i
        enddo
        if ( problem .ne. 0 ) then

           q = pid(problem,j)
           strat = 1
           do i = problem, problem-bwdth, -1
             if ( pid(i,j) .ne. q ) then
               if ( i .ge. il-bwdth+1 ) then              ! no hope for it
                 write(0,*)'nb: problem with mapper: use patchmap'
                 goto 890
               endif
               strat = 2
             endif
           enddo

           if ( strat .eq. 1 ) then             ! stategy 1 -- move offending
                                                ! point(s) from boundary area
             i = problem
             do q = il,il-bwdth,-1
               pid(q,j) = pid(i,j)
               i = i-1
             enddo
             i = il-bwdth-1
             do q = il,problem+1,-1
               pid(i,j) = p
               i = i-1
             enddo
           else                                 ! strategy 2 -- make all one
                                                ! or the other
c             write(0,*) 'nb using strat 2 on j=',j
             a = il - problem           ! # points on first proc
             b = bwdth - a              ! # points on second
             if ( a .lt. b ) then
               do i = il, il-a+1, -1
                 pid( i, j ) = pid( il-bwdth+1, j )
               enddo
             else
               do i = il-a, il-bwdth+1, -1
                 pid( i, j ) = pid( il, j )
               enddo
             endif

           endif
        endif
      enddo

c boundary safe in north-south -- we do not want the n/s boundaries
c decomposed in this dimension

      return
 890  continue
      stop
      end


