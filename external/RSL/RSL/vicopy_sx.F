      subroutine vicopy( a, b, c )
      integer a(*), b(*)
      integer c
      do i = 1, c
        b(i) = a(i)
      enddo
      return
      end

      subroutine vrcopy( a, b, c )
      real a(*), b(*)
      integer c
      do i = 1, c
        b(i) = a(i)
      enddo
      return
      end

