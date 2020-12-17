subroutine da_get_innov_vector_ssmi_tb (it, grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,       intent(in)    :: it         ! External iteration.
   type(domain),  intent(in)    :: grid       ! first guess state.
   type(y_type),  intent(in)    :: ob         ! Observation structure.
   type(iv_type), intent(inout) :: iv         ! O-B structure.

   integer :: n           ! Loop counter.
   integer :: i, j        ! Index dimension.
   real    :: dx, dxm     ! Interpolation weights.
   real    :: dy, dym     ! Interpolation weights.
   real    :: model_tb19h ! Model value tb19h at oblocation.
   real    :: model_tb19v ! Model value tb19v at oblocation.
   real    :: model_tb22v ! Model value tb22v at oblocation.
   real    :: model_tb37h ! Model value tb37h at oblocation.
   real    :: model_tb37v ! Model value tb37v at oblocation.
   real    :: model_tb85h ! Model value tb85h at oblocation.
   real    :: model_tb85v ! Model value tb85v at oblocation.

   if (trace_use) call da_trace_entry("da_get_innov_vector_ssmi_tb")

   if ( it > 1 ) then
      do n=iv%info(ssmi_tb)%n1,iv%info(ssmi_tb)%n2
         if(iv % ssmi_tb(n) % tb19v % qc == fails_error_max ) iv % ssmi_tb(n) % tb19v % qc = 0
         if(iv % ssmi_tb(n) % tb19h % qc == fails_error_max ) iv % ssmi_tb(n) % tb19h % qc = 0
         if(iv % ssmi_tb(n) % tb22v % qc == fails_error_max ) iv % ssmi_tb(n) % tb22v % qc = 0
         if(iv % ssmi_tb(n) % tb37v % qc == fails_error_max ) iv % ssmi_tb(n) % tb37v % qc = 0
         if(iv % ssmi_tb(n) % tb37h % qc == fails_error_max ) iv % ssmi_tb(n) % tb37h % qc = 0
         if(iv % ssmi_tb(n) % tb85v % qc == fails_error_max ) iv % ssmi_tb(n) % tb85v % qc = 0
         if(iv % ssmi_tb(n) % tb85h % qc == fails_error_max ) iv % ssmi_tb(n) % tb85h % qc = 0
      end do
   end if

   do n=iv%info(ssmi_tb)%n1,iv%info(ssmi_tb)%n2
      ! compute innovation vector
      ! =========================

      !  Obs coordinates on model grid

      ! TB

      i   = iv%info(ssmi_tb)%i(1,n)
      j   = iv%info(ssmi_tb)%j(1,n)
      dx  = iv%info(ssmi_tb)%dx(1,n)
      dy  = iv%info(ssmi_tb)%dy(1,n)
      dxm = iv%info(ssmi_tb)%dxm(1,n)
      dym = iv%info(ssmi_tb)%dym(1,n)

      ! Tb19h

      if (abs(ob % ssmi_tb(n) % tb19h - missing_r) > 1.0) then
         model_tb19h = dym*(dxm*grid%xb%tb19h(i,j)   + dx*grid%xb%tb19h(i+1,j)) &
            + dy *(dxm*grid%xb%tb19h(i,j+1) + dx*grid%xb%tb19h(i+1,j+1))
         iv % ssmi_tb(n) % tb19h % inv = ob % ssmi_tb(n) % tb19h - &
            model_tb19h
      else
         iv % ssmi_tb(n) % tb19h % inv = 0.0
      end if

      ! Tb19v

      if (abs(ob % ssmi_tb(n) % tb19v - missing_r) > 1.0) then
         model_tb19v = dym*(dxm*grid%xb%tb19v(i,j)   + dx *grid%xb%tb19v(i+1,j)) &
            + dy *(dxm*grid%xb%tb19v(i,j+1) + dx *grid%xb%tb19v(i+1,j+1))
         iv % ssmi_tb(n) % tb19v % inv = ob % ssmi_tb(n) % tb19v - &
            model_tb19v
      else
         iv % ssmi_tb(n) % tb19v % inv = 0.0
      end if

     ! Tb19v

      if (abs(ob % ssmi_tb(n) % tb22v - missing_r) > 1.0) then
         model_tb22v = dym*(dxm*grid%xb%tb22v(i,j) + dx *grid%xb%tb22v(i+1,j)) &
            + dy *(dxm*grid%xb%tb22v(i,j+1) + dx *grid%xb%tb22v(i+1,j+1))
         iv % ssmi_tb(n) % tb22v % inv = ob % ssmi_tb(n) % tb22v - &
            model_tb22v
      else
         iv % ssmi_tb(n) % tb22v % inv = 0.0
      end if

      ! Tb37h

      if (abs(ob % ssmi_tb(n) % tb37h - missing_r) > 1.0) then
         model_tb37h = dym*(dxm*grid%xb%tb37h(i,j)  + dx *grid%xb%tb37h(i+1,j)) &
            + dy *(dxm*grid%xb%tb37h(i,j+1) + dx *grid%xb%tb37h(i+1,j+1))
         iv % ssmi_tb(n) % tb37h % inv = ob % ssmi_tb(n) % tb37h - &
            model_tb37h
      else
         iv % ssmi_tb(n) % tb37h % inv = 0.0
      end if

      ! Tb37v

      if (abs(ob % ssmi_tb(n) % tb37v - missing_r) > 1.0) then
         model_tb37v = dym*(dxm*grid%xb%tb37v(i,j)  + dx *grid%xb%tb37v(i+1,j)) &
            + dy *(dxm*grid%xb%tb37v(i,j+1) + dx *grid%xb%tb37v(i+1,j+1))
         iv % ssmi_tb(n) % tb37v % inv = ob % ssmi_tb(n) % tb37v - &
            model_tb37v
      else
         iv % ssmi_tb(n) % tb37v % inv = 0.0
      end if

      ! Tb85h

      if (abs(ob % ssmi_tb(n) % tb85h - missing_r) > 1.0) then
         model_tb85h = dym*(dxm*grid%xb%tb85h(i,j) + dx *grid%xb%tb85h(i+1,j)) &
            + dy *(dxm*grid%xb%tb85h(i,j+1) + dx *grid%xb%tb85h(i+1,j+1))
         iv % ssmi_tb(n) % tb85h % inv = ob % ssmi_tb(n) % tb85h - &
            model_tb85h
      else
         iv % ssmi_tb(n) % tb85h % inv = 0.0
      end if

      ! Tb85v

      if (abs(ob % ssmi_tb(n) % tb85v - missing_r) > 1.0) then
         model_tb85v = dym*(dxm*grid%xb%tb85v(i,j) + dx *grid%xb%tb85v(i+1,j)) &
            + dy *(dxm*grid%xb%tb85v(i,j+1) + dx *grid%xb%tb85v(i+1,j+1))
         iv % ssmi_tb(n) % tb85v % inv = ob % ssmi_tb(n) % tb85v -  &
            model_tb85v
      else
         iv % ssmi_tb(n) % tb85v % inv = 0.0
      end if
   end do

   !----------------------------------------------------------------
   !     Perform optional maximum error check:
   !----------------------------------------------------------------

   if (check_max_iv) call da_check_max_iv_ssmi_tb(iv, it)  
   
   if (trace_use) call da_trace_exit("da_get_innov_vector_ssmi_tb")

end subroutine da_get_innov_vector_ssmi_tb


