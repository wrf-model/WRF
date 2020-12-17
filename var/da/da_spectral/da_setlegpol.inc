subroutine da_setlegpol(nj, max_wavenumber, alp_size, sinlat, coslat, alp)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   !  Method:
   !  Uses ECMWF recursion relation as opposed to Num Rec. one which can only go
   !  to m about 12 with single precision). However, still use NumRec one for
   !  m = 0 and 1 as ECMWF one requires m-2 for asslegpol of order m.
   !  Reference: Jarraud and Simmons (1990) and Belousov (1962).

   integer, intent(in)  :: nj                         ! Latitude dimension.
   integer, intent(in)  :: max_wavenumber             ! Maximum zonal wavenumber.
   integer, intent(in)  :: alp_size                   ! Ass. Leg. Pol. array size.
   real,    intent(in)  :: sinlat(1:nj)               ! sine(latitude).
   real,    intent(in)  :: coslat(1:nj)               ! cosine(latitude).
   real,    intent(out) :: alp(1:alp_size)            ! Associated Legendre Polynomial.

   integer              :: j, l, m, mm                ! Loop counters.
   integer              :: l1, l2, m2                 ! Markers.
   integer              :: index_j, index_m, index    ! Indexing counters for ALP array.
   integer              :: index_m2, index_l2m2       ! Indexing counters for ALP array.
   integer              :: index_l1m2, index_l1m      ! Indexing counters for ALP array.
   real                 :: s
   real                 :: c(2:max_wavenumber,2:max_wavenumber) ! Recursion coefficient.
   real                 :: d(2:max_wavenumber,2:max_wavenumber) ! Recursion coefficient.
   real                 :: e(2:max_wavenumber,2:max_wavenumber) ! Recursion coefficient.

   if (trace_use) call da_trace_entry("da_setlegpol")

   alp(:) = 0.0

   ! Compute Associated Legendre polynomials for latitude range:

   do j = 1, (nj + 1) / 2
      index_j = (j - 1) * (max_wavenumber + 1) * (max_wavenumber + 2) / 2

      ! use Num. Rec. recursion relation for m = 0, 1:

      do m = 0, 1
         index_m = m * (max_wavenumber + 1 - m) + m * (m + 1) / 2 + 1 - m
         do l = m, max_wavenumber
            index = index_m + index_j + l
            call da_asslegpol(l, m, sinlat(j), coslat(j), alp(index))

            ! Multiply by normalization constant 
            ! (to ensure 1/2 integral^1_-1 Pml Pml1 = 1 if l = l1):

            s = 1.0
            do mm = l-m+1, l+m
               s = s * real(mm)
            end do
            alp(index) = sqrt(real(2*l+1) / s) * alp(index)
         end do
      end do
   end do

   ! Jarraud recursion relation coefficients:

   do m = 2, max_wavenumber
      do l = m, max_wavenumber
         c(l,m) = sqrt ((real(2*l+1)/real(2*l-3)) * (real(l+m-1)/real(l+m)) * &
                  (real(l+m-3)/real(l+m-2)))
         d(l,m) = sqrt ((real(2*l+1)/real(2*l-1)) * (real(l+m-1)/real(l+m)) * &
                  (real(l-m+1)/real(l+m-2)))
         e(l,m) = sqrt ((real(2*l+1)/real(2*l-1)) * (real(l-m)  /real(l+m)))
      end do
   end do

   ! use Jarraud recursion relation for m>=2:

   do j = 1, (nj + 1) / 2
      index_j = (j - 1) * (max_wavenumber + 1) * (max_wavenumber + 2) / 2

      do m = 2, max_wavenumber
         index_m = m * (max_wavenumber + 1 - m) + m * (m + 1) / 2 + 1 - m
         m2 = m - 2
         index_m2 = m2 * (max_wavenumber + 1 - m2) + m2 * (m2+1) / 2 + 1 - m2

         do l = m, max_wavenumber
            l1 = l - 1
            l2 = l - 2
            index = index_j + index_m + l
            index_l2m2 = index_j + index_m2 + l2
            index_l1m2 = index_j + index_m2 + l1
            index_l1m  = index_j + index_m  + l1

            alp(index) = c(l,m) * alp(index_l2m2) - d(l,m) *  sinlat(j) * &
               alp(index_l1m2) + e(l,m) * sinlat(j) * alp(index_l1m)
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_setlegpol")

end subroutine da_setlegpol


