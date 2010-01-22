subroutine da_interpolate_regcoeff( kzs, kz, meanp_stats, meanp_xb, &
                                    stats_regcoeff1, stats_regcoeff2, stats_regcoeff3, &
                                    xb_regcoeff1, xb_regcoeff2, xb_regcoeff3 )

!------------------------------------------------------------------------------
!  PURPOSE: Interpolate statistical regression coefficient to new domain.
!
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in)                       :: kzs               ! Number of levels in stats.
   integer, intent(in)                       :: kz                ! Number of levels in xb.
   real, dimension(kzs),         intent(in)  :: meanp_stats       ! Mean pressure on stats levs.
   real, dimension(kz),          intent(in)  :: meanp_xb          ! Mean pressure on xb levs.
   real, dimension(kzs),         intent(in)  :: stats_regcoeff1, stats_regcoeff2
   real, dimension(kzs,kzs),     intent(in)  :: stats_regcoeff3
   real, dimension(kz),          intent(out) :: xb_regcoeff1, xb_regcoeff2
   real, dimension(kz,kz),       intent(out) :: xb_regcoeff3

   integer                :: k1, k2, k, ks            ! Loop counters.
   integer                :: ktrap_min, ktrap_max     ! Max/min limits of xb rows.
   integer                :: k1s, k2s
   integer                :: k_above(1:kz)
   real                   :: weight(1:kz)

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
!  [3.0] Interpolate regression coefficient from stats to xb levels:
!---------------------------------------------------------------------------
      do k1 = 1, kz
         k1s = k_above(k1)
!print*,k1,' interpolating between ',k1s-1,k1s,' weights ',weight(k1),' stats ',stats_regcoeff1(k1s-1),stats_regcoeff1(k1s)
!            xb_regcoeff1(kz-k1+1) = &
            xb_regcoeff1(k1) = &
                               (1.0-weight(k1)) * stats_regcoeff1(k1s) + &
                                    weight(k1)  * stats_regcoeff1(k1s-1) 
!            xb_regcoeff2(kz-k1+1) = &
            xb_regcoeff2(k1) = &
                               (1.0-weight(k1)) * stats_regcoeff2(k1s) + &
                                    weight(k1)  * stats_regcoeff2(k1s-1) 
  
         do k2 = 1, kz
           k2s = k_above(k2)

!           xb_regcoeff3(kz-k1+1,kz-k2+1) = &
           xb_regcoeff3(k1,k2) = &
                              (1.0-weight(k1)) * (1.0-weight(k2)) * stats_regcoeff3(k1s,k2s) + &
                                   weight(k1)  * (1.0-weight(k2)) * stats_regcoeff3(k1s-1,k2s  ) + &
                              (1.0-weight(k1)) *      weight(k2)  * stats_regcoeff3(k1s  ,k2s-1) + &
                                   weight(k1)  *      weight(k2)  * stats_regcoeff3(k1s-1,k2s-1) 

         end do
      end do     
         
end subroutine da_interpolate_regcoeff

