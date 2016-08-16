subroutine da_check_vp_errors(vp1, vp2, ne, &
                               its,ite, jts,jte, kts,kte)

   !---------------------------------------------------------------------------
   ! Purpose: Test invertibility of transform to/from Vp or Vv
   !
   ! Method:  Perform statistics on differences in initial and final Vv or Vp
   !---------------------------------------------------------------------------

   implicit none

   type (vp_type), intent(in)     :: vp1         ! Test input
   type (vp_type), intent(in)     :: vp2         ! Test output.
   integer, intent(in)            :: ne          ! Ensemble size.
   integer, intent(in)            :: its,ite, jts,jte, kts,kte ! tile   dims.

   real                           :: inv_size    ! 1/size of array.
   real                           :: rms_fild    ! RMS of field.
   real                           :: rms_diff    ! RMS of differnce.

   real, dimension(its:ite, jts:jte, kts:kte) :: diff ! Difference
!  real                           :: diff_alpha(its:ite,jts:jte,kts:kte,1:ne)
   real                           :: diff_alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,1:ne)
   real                           :: inv_size_ens    ! 1/size of ensemble array.

   if (trace_use) call da_trace_entry("da_check_vp_errors")

   inv_size = 1.0 / real((ite-its+1) * (jte-jts+1) * (kte-kts+1))
   inv_size_ens = 1.0 / real((ite_int-its_int+1) * (jte_int-jts_int+1) * (kte_int-kts_int+1)*ne)

   !-------------------------------------------------------------------------
   ! [1.0]: Check v1 differences:
   !-------------------------------------------------------------------------

   diff(its:ite,jts:jte, kts:kte) = vp2 % v1(its:ite,jts:jte,kts:kte) - &
                                    vp1 % v1(its:ite,jts:jte,kts:kte)

   rms_fild = sqrt(sum(vp1 % v1(its:ite, jts:jte,kts:kte) &
                       * vp1 % v1(its:ite, jts:jte,kts:kte)) * inv_size)
   rms_diff = sqrt(sum(diff(its:ite, jts:jte,kts:kte) &
                       * diff(its:ite, jts:jte,kts:kte)) * inv_size)
     
   if (rms_fild == 0.0) then
      write(unit=stdout, fmt='(a)') ' v1 is zero ' 
   else
      write(unit=stdout, fmt='(a,1pe10.4)') &
           ' v1 RMS error/RMS field = ', rms_diff/rms_fild
   end if      

   !-------------------------------------------------------------------------
   ! [2.0]: Check v2 differences:
   !-------------------------------------------------------------------------

   diff(its:ite,jts:jte, kts:kte) = vp2 % v2(its:ite,jts:jte,kts:kte) - &
                                    vp1 % v2(its:ite,jts:jte,kts:kte)

   rms_fild = sqrt(sum(vp1 % v2(its:ite, jts:jte,kts:kte) &
                       * vp1 % v2(its:ite, jts:jte,kts:kte)) * inv_size)
   rms_diff = sqrt(sum(diff(its:ite, jts:jte,kts:kte) &
                       * diff(its:ite, jts:jte,kts:kte)) * inv_size)
     
   if (rms_fild == 0.0) then
      write(unit=stdout, fmt='(a)') ' v2 is zero ' 
   else
      write(unit=stdout, fmt='(a,1pe10.4)') &
           ' v2 RMS error/RMS field = ', rms_diff/rms_fild
   end if      

   !-------------------------------------------------------------------------
   ! [3.0]: Check v3 differences:
   !-------------------------------------------------------------------------

   diff(its:ite,jts:jte, kts:kte) = vp2 % v3(its:ite,jts:jte,kts:kte) - &
                                    vp1 % v3(its:ite,jts:jte,kts:kte)

   rms_fild = sqrt(sum(vp1 % v3(its:ite, jts:jte,kts:kte) &
                       * vp1 % v3(its:ite, jts:jte,kts:kte)) * inv_size)
   rms_diff = sqrt(sum(diff(its:ite, jts:jte,kts:kte) &
                       * diff(its:ite, jts:jte,kts:kte)) * inv_size)
     
   if (rms_fild == 0.0) then
      write(unit=stdout, fmt='(a)') ' v3 is zero ' 
   else
      write(unit=stdout, fmt='(a,1pe10.4)') &
         ' v3 RMS error/RMS field = ', rms_diff/rms_fild
   end if      

   !-------------------------------------------------------------------------
   ! [4.0]: Check v4 differences:
   !-------------------------------------------------------------------------

   diff(its:ite,jts:jte, kts:kte) = vp2 % v4(its:ite,jts:jte,kts:kte) - &
                                    vp1 % v4(its:ite,jts:jte,kts:kte)

   rms_fild = sqrt(sum(vp1 % v4(its:ite, jts:jte,kts:kte) &
                       * vp1 % v4(its:ite, jts:jte,kts:kte)) * inv_size)
   rms_diff = sqrt(sum(diff(its:ite, jts:jte,kts:kte) &
                       * diff(its:ite, jts:jte,kts:kte)) * inv_size)
     
   if (rms_fild == 0.0) then
      write(unit=stdout, fmt='(a)') ' v4 is zero ' 
   else
      write(unit=stdout, fmt='(a,1pe10.4)') &
         ' v4 RMS error/RMS field = ', rms_diff/rms_fild
   end if
      
   !-------------------------------------------------------------------------
   ! [5.0]: Check v5 differences:
   !-------------------------------------------------------------------------

   inv_size = 1.0 / real((ite-its+1) * (jte-jts+1))

   diff(its:ite, jts:jte,kts:kte) = vp2 % v5(its:ite, jts:jte,kts:kte) - &
      vp1 % v5(its:ite, jts:jte,kts:kte)

   rms_fild = sqrt(sum(vp1 % v5(its:ite, jts:jte,kts:kte) * &
      vp1 % v5(its:ite, jts:jte,kts:kte)) * inv_size)
   rms_diff = sqrt(sum(diff(its:ite, jts:jte,1) * &
      diff(its:ite, jts:jte,1)) * inv_size)
     
   if (rms_fild == 0.0) then
      write(unit=stdout, fmt='(a)') ' v5 is zero ' 
   else
      write(unit=stdout, fmt='(a,1pe10.4)') &
         ' v5 RMS error/RMS field = ', rms_diff/rms_fild
   end if    
      
   !-------------------------------------------------------------------------
   ! [6.0]: Check alpha differences:
   !-------------------------------------------------------------------------
 
   ! changed all dimensions below to reflect ensemble tiles
   if (ne > 0) then
!     inv_size = 1.0 / real((ite-its+1) * (jte-jts+1) * ne)
      diff_alpha(its:ite,jts:jte,kts:kte,1:ne) = vp2 % alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,1:ne) - &
                                         vp1 % alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,1:ne)
      rms_fild = sqrt(sum(vp1 % alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,1:ne) &
                          * vp1 % alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,1:ne)) * inv_size_ens)
      rms_diff = sqrt(sum(diff_alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,1:ne) &
                          * diff_alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,1:ne)) * inv_size_ens)
     
      if (rms_fild /= 0.0) then
         write(unit=stdout, fmt='(a,1pe10.4)') ' alpha RMS error/RMS field = ',&
            rms_diff/rms_fild
      end if
   end if

   if (trace_use) call da_trace_exit("da_check_vp_errors")

end subroutine da_check_vp_errors


