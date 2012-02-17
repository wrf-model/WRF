Subroutine da_gen_eigen(kz, covm, eve, val)

  implicit none

  integer, intent(in) :: kz
  real   , dimension(1:kz,1:kz), intent(inout) :: covm
  real   , dimension(1:kz,1:kz), intent(out)   :: eve
  real   , dimension(1:kz)     , intent(out)   :: val

  integer  :: k1, k2

!
      call da_1d_eigendecomposition( covm, eve, val )

      do k1 = 1, kz
        if ( val(k1) < 0.0 ) val(k1) = 0.0
      end do

      do k1 = 1, kz
        do k2 = k1, kz
          covm(k1,k2) = SUM( eve(k1,:) * val(:) * eve(k2,:) )
        end do
      end do
   
      do k1 = 2, kz
        do k2 = 1, k1-1
          covm(k1,k2) = covm(k2,k1)
        end do
      end do

      call da_1d_eigendecomposition( covm, eve, val )

      do k1 = 1, kz
        if ( val(k1) < 0.0 ) val(k1) = 0.0
      end do
!
end Subroutine da_gen_eigen
