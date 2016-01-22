subroutine da_interpolate_stats( kzs, kz, meanp_stats, meanp_xb, &
                                 be_evec_glo, be_eval_glo,       &
                                 be_lengthscale, e, l, s, dx )

!------------------------------------------------------------------------------
!  PURPOSE: Interpolate statistics to new domain.
!
!  METHOD:  Interpolation of vertical background error covariance.
!
!  HISTORY: 06/07/2001 - Creation of F90 version.           Dale Barker
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in)                  :: kzs            ! Number of levels in stats.
   integer, intent(in)                  :: kz             ! Number of levels in xb.
   real, dimension(kzs), intent(in)     :: meanp_stats    ! Mean pressure on stats levs.
   real, dimension(kz), intent(inout)   :: meanp_xb       ! Mean pressure on xb levs.
   real, dimension(kzs,kzs), intent(in) :: be_evec_glo    ! Eigenvectors of vert B.
   real, dimension(kzs), intent(in)     :: be_eval_glo    ! Eigenvalues of vert B.
   real, dimension(kzs), intent(in)     :: be_lengthscale ! Correlation scale.
   real, dimension(kz,kz), intent(out)  :: e              ! New eigenvectors.
   real, dimension(kz), intent(out)     :: l              ! New eigenvalues.
   real, dimension(kz), intent(out)     :: s              ! New lengthscales.
   real*4,              intent(in)      :: dx             ! Grid distance in m
     
   integer                     :: k1, k2, k, ks         ! Loop counters.
   integer                     :: ktrap_min, ktrap_max  ! Max/min limits of xb levels.
   integer                     :: k1s, k2s

   real                        :: b_native(1:kzs,1:kzs)
   real                        :: b(1:kz,1:kz)
   real                        :: weight(1:kz)
   integer                     :: k_above(1:kz)

!---------------------------------------------------------------------------
!  [1.0] Compare stats/xb levels and compute interpolation weights:
!---------------------------------------------------------------------------
!
   k_above(1:kz) = 0
   weight(1:kz) = 0.0
  
   do k = 1, kz
      if ( meanp_xb(k) <= meanp_stats(1) ) then
      ktrap_min = k
      go to 10
      end if
   end do
   print*,' problem in determining ktrap_min'
   stop
10  continue

   do k = kz, 1, -1
      if ( meanp_xb(k) >= meanp_stats(kzs) ) then
      ktrap_max = k
      go to 20
      end if
   end do
   print*,' problem in determining ktrap_max'
   stop
20  continue
   do k = ktrap_min, ktrap_max
      do ks = 1, kzs-1
         if ( meanp_xb(k) > meanp_stats(ks+1) .AND. &
              meanp_xb(k) <= meanp_stats(ks) ) then

            weight(k) = ( meanp_xb(k) - meanp_stats(ks+1) ) / &
                        ( meanp_stats(ks) - meanp_stats(ks+1) )
            k_above(k) = ks+1
            exit
         end if
      end do
   end do

   do k = 1, ktrap_min - 1
      k_above(k) = 2
      weight(k) = 1.0
   enddo
   do k = ktrap_max + 1, kz
      k_above(k) = kzs - 1 
      weight(k) = 0.0
   enddo

!---------------------------------------------------------------------------
!  [2.0] Interpolate lengthscales (in pressure) from original to new levels:
!---------------------------------------------------------------------------
   
!  [2.1] Levels average pressure within top/bottom of original stats:

   do k = 1, kz
      ks = k_above(k)
      
      s(k) = ( 1.0 - weight(k) ) * be_lengthscale(ks) + &
             weight(k) * be_lengthscale(ks-1)
!      write(6,'(a15,2x,2i4,4f12.5)')'lengthscale',k, ks, weight(k), be_lengthscale(ks), &
!                            be_lengthscale(ks-1), s(k)
!  Convert in equivalent grid distance
!  Global stats is at 62537 m at equator
   s(k) = s(k) * 62537.0 / dx
   end do
!---------------------------------------------------------------------------
!  [3.0] Recalculate native global vertical background error cov matrix:
!---------------------------------------------------------------------------

   do k1 = 1, kzs
      do k2 = k1, kzs
         b_native(k1,k2) = SUM( be_evec_glo(k1,:) * be_eval_glo(:) * &
                                be_evec_glo(k2,:) )
      end do
   end do
   
   do k1 = 2, kzs
      do k2 = 1, k1-1
         b_native(k1,k2) = b_native(k2,k1)
      end do
   end do

!---------------------------------------------------------------------------
!  [3.0] Interpolate vertical background error covariance matrix to xb domain:
!---------------------------------------------------------------------------

   b(1:kz,1:kz) = 0.0

!  [3.1] Interpolation to pressures within domain of original statistics:
 
   do k1 = 1, kz                 
      k1s = k_above(k1)
      do k2 = k1, kz
         k2s = k_above(k2)
         b(k1,k2) = (1.0-weight(k1)) * (1.0-weight(k2)) * b_native(k1s,k2s) + &
                    weight(k1) * (1.0-weight(k2)) * b_native(k1s-1,k2s) + &
                    weight(k2) * (1.0-weight(k1)) * b_native(k1s,k2s-1) + &
                    weight(k2) * weight(k1) * b_native(k1s-1,k2s-1)
      end do
   end do
      
   do k1 = 2, kz
      do k2 = 1, k1-1
         b(k1,k2) = b(k2,k1)
      end do
   end do
      
!---------------------------------------------------------------------------
!  [4.0] Calculate new global eigenvectors/eigenvalues:
!---------------------------------------------------------------------------

   call da_1d_eigendecomposition( b, e, l, kz )

!---------------------------------------------------------------------------
!  [5.0] Check for positive-definiteness:
!---------------------------------------------------------------------------

!  [5.1] Check for negative eigenvalues and zero if found:

   do k1 = 1, kz
      if ( l(k1) < 0.0 ) l(k1) = 0.0
   end do

!     [5.2] Calculate modified b(:,:)

   do k1 = 1, kz
      do k2 = k1, kz
         b(k1,k2) = SUM( e(k1,:) * l(:) * e(k2,:) )
      end do
   end do
   
   do k1 = 2, kz
      do k2 = 1, k1-1
         b(k1,k2) = b(k2,k1)
      end do
   end do
   

!  [5.3] Recalculate eigenvectors/eigenvalues:

   call da_1d_eigendecomposition( b, e, l, kz )

!  [5.4] Final elimination of small negative values:

   do k1 = 1, kz
      if ( l(k1) < 0.0 ) l(k1) = 0.0
   end do

end subroutine da_interpolate_stats
