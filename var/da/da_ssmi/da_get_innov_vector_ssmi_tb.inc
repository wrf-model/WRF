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

   integer :: itb19v, itb19vf, &
              itb19h, itb19hf, &
              itb22v, itb22vf, &
              itb37v, itb37vf, &
              itb37h, itb37hf, &
              itb85v, itb85vf, &
              itb85h, itb85hf
   
   if (trace_use) call da_trace_entry("da_get_innov_vector_ssmi_tb")

   itb19v  = 0
   itb19vf = 0
   itb19h  = 0
   itb19hf = 0
   itb22v  = 0
   itb22vf = 0
   itb37v  = 0
   itb37vf = 0
   itb37h  = 0
   itb37hf = 0
   itb85v  = 0
   itb85vf = 0
   itb85h  = 0
   itb85hf = 0

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

   if (check_max_iv) then
      call da_check_max_iv_ssmi_tb(iv, it, &
         itb19v, itb19vf, itb19h, itb19hf, &
         itb22v, itb22vf,                  &
         itb37v, itb37vf, itb37h, itb37hf, &
         itb85v, itb85vf, itb85h, itb85hf )
   end if

   if (rootproc .and. check_max_iv_print) then
      write(unit = check_max_iv_unit, fmt ='(A,i5,A)')'For outer iteration ', &
         it,'Total Rejections for SSMI(Tbs) follows:'
      write(unit = check_max_iv_unit, fmt = '(2(A,I6))') &
         'Number of failed ssmi tb19v observations: ',itb19v, ' on ',itb19v, &
         'Number of failed ssmi tb19h observations: ',itb19h, ' on ',itb19h, &
         'Number of failed ssmi tb22v observations: ',itb22v, ' on ',itb22v, &
         'Number of failed ssmi tb37v observations: ',itb37v, ' on ',itb37v, &
         'Number of failed ssmi tb37h observations: ',itb37h, ' on ',itb37h, &
         'Number of failed ssmi tb85v observations: ',itb85v, ' on ',itb85v, &
         'Number of failed ssmi tb85h observations: ',itb85h, ' on ',itb85h
   end if
   
   if (trace_use) call da_trace_exit("da_get_innov_vector_ssmi_tb")

end subroutine da_get_innov_vector_ssmi_tb


