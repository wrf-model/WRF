      subroutine hiresmap(iunit)
      dimension xlatmap(10000),xlonmap(10000)
c
c     ... the other end of this thing is in mrdclt, we only want
c         the continent outline color
c
      common /outcol/  cocolr,           ! color of continents
     *                 uscolr,           ! color of states
     *                 cncolr            ! color of countries
c
c     ... this comes from mrddet, we want the map line width
c
      common /motdet/  dtdmp,            ! T => draw the map with dots ?
     *                 maplw,            ! line width for map outlines
     *                 dtdsp             ! dash spacing for map outlines
      integer cocolr
c
c     ... this routine calls routine to read the high resolution
c         data once for each block of values
c     ... the NCARG routines MAPIT and MAPIQ plot out the lines
c         and flush the buffers after each line segment is drawn
c     ... the routine returns control when all of the data has
c         been processed
c     ... this dataset was received from Peter Neilley RAP/NCAR

      CALL GSPLCI (COCOLR)
      call setusv('LW',maplw)
      open(iunit,file='hipone.ascii',form='formatted')
      iblock=1
10    continue
      call readmap(numpoints,
     *                   xlatmin,xlatmax,xlonmin,xlonmax,
     *                   xlatmap,xlonmap,
     *                   iunit,iend)

      if(iend.eq.1) goto 1000
c     print *,'finished block ',iblock
      call mapit(xlatmap(1),xlonmap(1),0)
      do 200 ipoint=2,numpoints/2
         call mapit(xlatmap(ipoint),xlonmap(ipoint),2)
200   continue
      call mapiq        
      iblock=iblock+1   
      goto 10
1000  continue
      print *,'MAPDRV - End of data for hipone.ascii'
      close(iunit)
      return
      end


      subroutine readmap(numpoints,
     *                   xlatmin,xlatmax,xlonmin,xlonmax,
     *                   xlat,xlon,
     *                   iunit,iend)
c
c     ... this program reads the ASCII data and stores it in 
c         arrays of lat and lon, and the number of points read
c     ... the box surrounding the area are read in 
c
      dimension xlat(numpoints/2),xlon(numpoints/2),val(8)
c
      read(iunit,100,end=1000) numpoints,
     *                         xlatmin,xlatmax,xlonmin,xlonmax,
     *                         xlatstart,xlonstart
      xlat(1)=xlatstart
      xlon(1)=xlonstart
      numlinesfull=(numpoints-2)/8
c     print *,'number of full lines=',numlinesfull
      numleft=numpoints-2-8*numlinesfull
c     print *,'number of left overs=',numleft
      istart=1
      do 50 line=1,numlinesfull
         read(iunit,108) val
         do 40 loc=1,4
            xlat((line-1)*4+loc+istart)=val(loc*2-1)
            xlon((line-1)*4+loc+istart)=val(loc*2  )
40       continue
50    continue
      if     (numleft.eq.6) then
         read(iunit,106) (val(ii),ii=1,numleft)
      else if(numleft.eq.4) then
         read(iunit,104) (val(ii),ii=1,numleft)
      else if(numleft.eq.2) then
         read(iunit,102) (val(ii),ii=1,numleft)
      endif
      do 60 loc=1,numleft/2
         xlat(numlinesfull*4+loc+istart)=val(loc*2-1)
         xlon(numlinesfull*4+loc+istart)=val(loc*2  )
60    continue

c     print *,'read all data from this block'
      iend=0
      return

1000  continue
c     print *,'end of data'
      iend=1
      return

100   format(i4,14x,6f9.3)
102   format(2f9.3)
104   format(4f9.3)
106   format(6f9.3)
108   format(8f9.3)

      end
