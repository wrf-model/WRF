subroutine da_max_error_qc_radar (it, info, n,field, max_error, ix, ixf, iunit, var, failed, print_details)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer          ,   intent (in)   :: it
   type (infa_type) ,   intent(in)    :: info
   integer,             intent(in)    :: n
   type (field_type),   intent(inout) :: field
   real             ,   intent(in)    :: max_error
   integer          ,   intent(inout) :: ix, ixf
   integer          ,   intent(in)    :: iunit
   character (len=*),   intent(in)    :: var
   logical          ,   intent(out)   :: failed
   logical          ,   intent(in)    :: print_details

   real                               :: err, err_max
   integer                            :: qc_flag

   if (trace_use) call da_trace_entry("da_max_error_qc_radar")

   failed = .false.

   qc_flag = field % qc
   err_max = field % error * max_error
   err     = field % inv
   err     = ABS (err)

   ix     = ix + 1
   if (it > 1 .and. qc_flag == fails_error_max) field%qc = 0
   if (err > err_max) then
      if (field % qc > fails_error_max) field % qc = fails_error_max 

      ixf = ixf + 1
      failed = .true.

      if (print_details .and. failed) then
         if (err_max .LE. 0.0) then
            write (iunit , fmt = '(A,3(F12.1,1X),A,A,A,A,A,3f10.2)')   &
               "Err_max < 0 ==> ",err,err_max,max_error, " for ", var, &
               " OBS ID: ", info%platform(n),     &
               " LA/LON/ELV:", info%lat(1,n), info%lon(1,n), info%elv(n)
            ! call da_error(__FILE__,__LINE__,(/"Erk"/))
         end if

         write (iunit , fmt = '(A,A,A,I5,A,I5,A,F4.1,A,A,A,2F12.1)') &
            "Err_max failed:ID=", info%platform(n),&
            "  Ix=", ix, "  Ixf=", ixf, " Err_max ratio =",err/err_max, &
            " for ", var, " inv, error:",field % inv, field % error
      end if
      field % inv = 0.0
   end if

   if (trace_use) call da_trace_exit("da_max_error_qc_radar")

end subroutine da_max_error_qc_radar


