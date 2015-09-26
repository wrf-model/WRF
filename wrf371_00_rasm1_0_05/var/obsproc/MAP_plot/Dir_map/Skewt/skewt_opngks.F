      subroutine skewt_opngks(fname)

      character*80 fname, cdum

      COMMON /SKWDRW/ ISKDRW

      ISKDRW = 0    !  Make sure to redraw the skew-t background.

      call gopks(6,idum)

      call gesc(-1391,1,fname,1,idum,cdum)
      call gopwk(2,9,3)
      call gopwk(1,3,1)
      call gacwk(1)
      call gsclip(0)
      call gscr(1, 1, 1., 1., 1.)
      call gscr(1, 2, 1., 0., 0.)  ! Red
      call gscr(1, 3, 1., .5, 0.)  ! Orange
      call gscr(1, 4, 1., 1., 0.)  ! Yellow
      call gscr(1, 5, 0., 1., 0.)  ! Green
      call gscr(1, 6, 0., 1., 1.)  ! Bright Blue (Cyan)
      call gscr(1, 8, 1., 1., 1.)
      call gscr(1, 9, 1., 1., 1.)

      return
      end
