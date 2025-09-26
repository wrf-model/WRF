subroutine da_get_alpha_vertloc (xb, alpha_val, alpha_evec)

   !-----------------------------------------------------------------------
   ! Purpose: To get vertical localization information for alpha control variable
   ! History:
   !    07/2019 - initial version
   !-----------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)  :: xb               ! first guess structure
   real*8, intent(inout)       :: alpha_val(:)     ! alpha_vertloc eigenvalues
   real*8, intent(inout)       :: alpha_evec(:,:)  ! alpha_vertloc eigenvectors

   character(len=filename_len) :: filename
   logical                     :: fexist
   integer                     :: iunit, ier
   integer                     :: kz, nk, k, k1, i
   real*8                      :: kscale, kscale_invsq, kdist
   real*8                      :: totvar, totvar_inv
   real*8                      :: cutoff
   real,   allocatable         :: cov(:,:)
   real*8, allocatable         :: rho(:,:)
   real*8, allocatable         :: eval(:)
   real*8, allocatable         :: evec(:,:)
   real*8, allocatable         :: p_w(:)  ! pressure at full levels
   real*8, allocatable         :: ln_p_w(:)
   real*8, allocatable         :: dlnp(:,:)
   real*8, allocatable         :: kcutoff(:), kcutoff_smth(:)

   kz = xb%mkz

   if ( size(alpha_val(:)) /= kz ) then
      ! this should not happen because dimension check is already handled in
      ! the parent subroutine da_setup_be_regional
      write(unit=message(1),fmt='(a,a)')  &
            'Vertical dimension of the assimilation domain do not match that in ', trim(filename)
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   filename = 'be.vertloc.dat'

   select case ( alpha_vertloc_opt )

      case ( 1 )  ! reading in from be.vertloc.dat

         inquire (file=trim(filename), exist=fexist)
         if ( .not. fexist ) then
            write(unit=message(1),fmt='(a,a)') trim(filename), ' not found for reading vertloc info.'
            write(unit=message(2),fmt='(a)')   'Either make sure it exists in the working directory, or'
            write(unit=message(3),fmt='(a)')   'set alpha_vertloc_opt=2 (recommended) in namelist.input (&wrfvar16) to have WRFDA calculate log-P based vertloc.'
            call da_error(__FILE__,__LINE__,message(1:3))
         end if
         call da_get_unit(iunit)
         open(unit=iunit,file=trim(filename), status='old', form='unformatted')

         write(unit=message(1),fmt='(a)') ''
         write(unit=message(2),fmt='(a,a)') 'alpha_vertloc_opt=1: reading alpha_vertloc info from ', trim(filename)
         call da_message(message(1:2))

         read (iunit, iostat=ier) nk
         if (ier /= 0) then
            write (unit=message(1),fmt='(a,a)') 'Error in reading ', trim(filename)
            call da_error(__FILE__,__LINE__,message(1:1))
         end if
         if ( nk /= kz ) then
            write(unit=message(1),fmt='(a,a)')  &
               'Vertical dimension of the assimilation domain do not match that in ', trim(filename)
            write(unit=message(2),fmt='(3x,a,a,i4)') 'in ', trim(filename), ': nk = ', nk
            write(unit=message(3),fmt='(3x,a,i4)')   'in fg:        , kz = ', kz
            call da_error(__FILE__,__LINE__,message(1:4))
         end if
         read (iunit) alpha_val(1:nk)
         read (iunit) alpha_evec(1:nk,1:nk)
         close(iunit)
         call da_free_unit(iunit)

      case ( 2 ) ! log-P method

         allocate(p_w(1:kz+1))
         allocate(ln_p_w(1:kz+1))
         allocate(dlnp(1:kz,1:kz))
         allocate(kcutoff_smth(1:kz))
         allocate(kcutoff(1:kz))

         ! empirical settings
         cutoff   = 1.0/2.71828 !1/e

         do k = 1, kz+1
            p_w(k) = xb%znw(k)*(base_pres - xb%ptop) + xb%ptop
            ln_p_w(k) = log(max(p_w(k),0.0001))
         end do
         kcutoff(:) = 1.0  ! initialize
         do k = 1, kz
            do k1 = k+1, kz
               dlnp(k,k1) = abs(ln_p_w(k)-ln_p_w(k1))
               if ( dlnp(k,k1) <= cutoff ) then
                  kcutoff(k) = k1-k+1
               end if
            end do
         end do
         ! smoothing
         kcutoff_smth(:) = kcutoff(:)
         do i = 1, 2 ! two passes
            do k = 2, kz-1
               kcutoff_smth(k)  = kcutoff(k)+ 0.25 * ( kcutoff(k-1) + kcutoff(k+1) -2.0*kcutoff(k) )
            end do
            kcutoff(:) = kcutoff_smth(:)
         end do
         deallocate(kcutoff_smth)
         deallocate(dlnp)
         deallocate(ln_p_w)
         deallocate(p_w)

         allocate(rho(1:kz,1:kz))
         allocate(cov(1:kz,1:kz))
         allocate(eval(1:kz))
         allocate(evec(1:kz,1:kz))

         ! specify probability densities:
         do k = 1, kz
            kscale = kcutoff(k)
            kscale_invsq = 1.0 / ( kscale * kscale )
            do k1 = k, kz
               kdist = k1 - k
               rho(k,k1) = exp ( -1.0 * real(kdist * kdist) * kscale_invsq )
               rho(k1,k) = rho(k,k1)
            end do
         end do
         cov = rho

         deallocate(kcutoff)

         ! calculate eigenvectors/values:

         call da_eof_decomposition( kz, cov, evec, eval )

         ! eliminate negative eigenvalues
         totvar = sum(eval(1:kz))
         do k = 1, kz
            if ( eval(k) < 0.0 ) eval(k) = 0.0
         end do
         totvar_inv = 1.0 / sum(eval(1:kz))
         eval(:) = eval(:) * totvar * totvar_inv ! preserve total variance before -ve values removed.

         eval(:) = sqrt(eval(:))

         alpha_val(:)    = eval(:)
         alpha_evec(:,:) = evec(:,:)

         write(unit=message(1),fmt='(a)') ''
         write(unit=message(2),fmt='(a,a)') 'alpha_vertloc_opt=2: calculate and write out alpha_vertloc info to ', trim(filename)
         call da_message(message(1:2))

         ! writing it out for diagnostics
         call da_get_unit(iunit)
         open(unit=iunit,file=trim(filename), status='replace', form='unformatted')
         write(iunit) kz
         write(iunit) alpha_val(1:kz)
         write(iunit) alpha_evec(1:kz,1:kz)
         close(iunit)
         call da_free_unit(iunit)

         deallocate(evec)
         deallocate(eval)
         deallocate(cov)
         deallocate(rho)

      case default

         write (unit=message(1),fmt='(a,i3)') ' Unknown option alpha_vertloc_opt = ', alpha_vertloc_opt
         write (unit=message(2),fmt='(a)')    ' Please set alpha_vertloc_opt = 1 or alpha_vertloc_opt = 2.'
         call da_error(__FILE__,__LINE__,message(1:2))

   end select

end subroutine da_get_alpha_vertloc
