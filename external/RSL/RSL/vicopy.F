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

      subroutine vbcopy( a, b, c )
      byte a(*), b(*)
      integer c
      do i = 1, c
        b(i) = a(i)
      enddo
      return
      end

      subroutine vizero( a, c )
      integer a(*)
      integer c
      do i = 1, c
        a(i) = 0
      enddo
      return
      end

      subroutine vbzero( a, c )
      byte a(*)
      integer c
      do i = 1, c
        a(i) = 0
      enddo
      return
      end

