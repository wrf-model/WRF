subroutine da_setup_cv(be)

   !---------------------------------------------------------------------------
   ! Purpose: Calculate the size of the 1-dimensional control variable array.        
   !---------------------------------------------------------------------------

   implicit none

   type (be_type), intent(inout) :: be       ! background error.

   integer :: iy, jx   ! Local horizontal domain dimensions.
   integer :: ij       ! Product of horizontal dims.

   if (trace_use) call da_trace_entry("da_setup_cv")

   !--------------------------------------------------------------
   ! [1] Define standard control variable size:
   !--------------------------------------------------------------

   if (global) then
      be % cv % size1c = (be % v1 % max_wave+1) * (be % v1 % max_wave+2)/2
      be % cv % size2c = (be % v2 % max_wave+1) * (be % v2 % max_wave+2)/2
      be % cv % size3c = (be % v3 % max_wave+1) * (be % v3 % max_wave+2)/2
      be % cv % size4c = (be % v4 % max_wave+1) * (be % v4 % max_wave+2)/2
      be % cv % size5c = (be % v5 % max_wave+1) * (be % v5 % max_wave+2)/2

      be % cv % size1  = 2 * be % cv % size1c * be % v1 % mz
      be % cv % size2  = 2 * be % cv % size2c * be % v2 % mz
      be % cv % size3  = 2 * be % cv % size3c * be % v3 % mz
      be % cv % size4  = 2 * be % cv % size4c * be % v4 % mz
      be % cv % size5  = 2 * be % cv % size5c * be % v5 % mz
   else
      iy = ite - its + 1
      jx = jte - jts + 1
      if( use_rf )then		! Use recursive filters:
         ij = iy * jx		! cv space size same as state space size
      else			! cv space size slightly larger for dwtai2():
         ij = size(be%wsd,1)*size(be%wsd,2)
!        call da_setup_cv_iyjx2ij(iy,jx,ij,nij,nb,lf,"tile")
         write(unit=message(1),fmt='("da_setup_cv: using ",i0," 2D ",A1,i0," wavelets in ",i0," bands.")') ij,namw,lf,nb
         call da_message(message(1:1))
      endif			! if( use_rf )
      be % cv % size1 = ij * be % v1 % mz
      be % cv % size2 = ij * be % v2 % mz
      be % cv % size3 = ij * be % v3 % mz
      be % cv % size4 = ij * be % v4 % mz
      be % cv % size5 = ij * be % v5 % mz

      if ( cloud_cv_options >= 2 ) then
         be % cv % size6 = ij * be % v6 % mz
         be % cv % size7 = ij * be % v7 % mz
         be % cv % size8 = ij * be % v8 % mz
         be % cv % size9 = ij * be % v9 % mz
         be % cv % size10= ij * be % v10 % mz
      end if
      if ( use_cv_w ) then
         be % cv % size11i= ij * be % v11 % mz
      end if

   end if

   be % cv % size_jb = be % cv % size1 + be % cv % size2 + be % cv % size3 + &
      be % cv % size4 + be % cv % size5

   if ( cloud_cv_options >= 2 ) then
      be % cv % size_jb = be % cv % size_jb + be % cv % size6 + &
         be % cv % size7 + be % cv % size8 + be % cv % size9 + &
         be % cv % size10
   end if

   if ( use_cv_w ) then
      be % cv % size_jb = be % cv % size_jb + be % cv % size11i
   end if

   !--------------------------------------------------------------
   ! [1.1] Define 4D-Var lateral boundary condition control variable size:
   !--------------------------------------------------------------

   be % cv % size_jl = 0

   if ( .not. global .and. var4d ) then
      be % cv % size1l = be % cv % size1
      be % cv % size2l = be % cv % size2
      be % cv % size3l = be % cv % size3
      be % cv % size4l = be % cv % size4
      be % cv % size5l = be % cv % size5

      be % cv % size_jl = be % cv % size_jb 
   endif

   !--------------------------------------------------------------
   ! [2] Define flow-dependent control variable size:
   !--------------------------------------------------------------

   if ( be % ne > 0) then
      if (global) then
         be % cv % size_alphac = (be % alpha % max_wave + 1) * &
                                 (be % alpha % max_wave + 2)  / 2
         be % cv % size_je  = 2 * be % cv % size_alphac * be % alpha % mz
      else
         be % cv % size_alphac = ij * be % alpha % mz * be % ne
         be % cv % size_je = be % cv % size_alphac
      end if
   end if
   
   !--------------------------------------------------------------
   ! [3] Define domain-wide cv sizes for bit-repro option:
   !--------------------------------------------------------------

   cv_size_domain_jb = 0
   cv_size_domain_je = 0
   cv_size_domain_jl = 0

   if (.not. global) then
      iy = ide - ids + 1
      jx = jde - jds + 1
      if( use_rf )then
         ij = iy * jx
      else
         ij = size(be%wsd,1)*size(be%wsd,2)
      endif
      cv_size_domain_jb = ij * (be % v1 % mz + be % v2 % mz + be % v3 % mz + &
             be % v4 % mz + be % v5 % mz )
      if ( cloud_cv_options >= 2 ) then
         cv_size_domain_jb = cv_size_domain_jb + ij * (be % v6 % mz + &
             be % v7 % mz + be % v8 % mz + be % v9 % mz + be % v10 % mz )
      end if
      if ( use_cv_w ) then
         cv_size_domain_jb = cv_size_domain_jb + ij * be % v11 % mz
      end if
      cv_size_domain_je = ij * be % alpha % mz * be % ne
      if ( var4d ) then
         cv_size_domain_jl = cv_size_domain_jb 
      end if
   end if

   if (trace_use) call da_trace_exit("da_setup_cv")

end subroutine da_setup_cv


