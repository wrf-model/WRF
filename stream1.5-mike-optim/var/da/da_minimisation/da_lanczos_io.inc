subroutine da_lanczos_io (io_config, cv_size, ntmax, neign, eignvec, eignval, qhat)

   !-------------------------------------------------------------------------
   ! Purpose:        Read / Write Lanczos eigenpairs
   !
   ! Called from da_solve
   !
   ! History: 08/16/2010  Creation (Tom Auligne)
   !
   !-------------------------------------------------------------------------

   implicit none
   
   character,        intent(in)    :: io_config              ! 'r' = Read; 'w' = Write
   integer,          intent(inout) :: cv_size
   integer,          intent(in)    :: ntmax
   integer,          intent(inout) :: neign
   real*8,           intent(inout) :: eignvec(ntmax, ntmax)
   real*8,           intent(inout) :: eignval(ntmax)
   real,             intent(inout) :: qhat(1:cv_size, 0:ntmax)

   character(len=filename_len)     :: filename               ! I/O filename
   character*10                    :: cproc
   integer                         :: ep_unit
   integer                         :: i

   if (trace_use) call da_trace_entry("da_lanczos_io")
   
   write(cproc,fmt='(i3.3)') myproc
   filename = '../lanczos_eigenpairs.'//trim(adjustl(cproc))

   call da_get_unit (ep_unit)

   if (io_config == 'r') then
      write(*,*) 'Reading Lanczos eigenpairs'
      open (unit=ep_unit, file = filename, form = 'unformatted', status = 'old')
      read(unit=ep_unit) neign, cv_size
      do i = 1, neign
         read(unit=ep_unit) eignval(i)
         read(unit=ep_unit) eignvec(1:neign,i)
         read(unit=ep_unit) qhat(1:cv_size,i)
      end do
      close(unit=ep_unit)
   else if (io_config == 'w') then
      write(*,*) 'Writing Lanczos eigenpairs'
      open (unit=ep_unit, file = filename, form = 'unformatted', status = 'replace')
      write(unit=ep_unit) neign, cv_size
      do i = 1, neign
         write(unit=ep_unit) eignval(i)
         write(unit=ep_unit) eignvec(1:neign,i)
         write(unit=ep_unit) qhat(1:cv_size,i)
      end do
      close(unit=ep_unit)
   else
      write(*,*) 'Unknow configuration for Lanczos I/O routine'
   end if

   call da_free_unit (ep_unit)

   if (trace_use) call da_trace_exit ("da_lanczos_io")

end subroutine da_lanczos_io
