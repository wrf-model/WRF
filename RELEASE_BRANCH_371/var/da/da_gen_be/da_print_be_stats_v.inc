subroutine da_print_be_stats_v(outunit, variable, nk, num_bins2d, &
   e_vec, e_val, e_vec_loc, e_val_loc)

   !----------------------------------------------------------------------------
   ! Purpose: To print out the first global and local eigenvector and 
   !           eigenvalues for 'variable'.
   !
   ! Input   : outunit    --- output unit
   !           variable   --- variable name: psi, chi_u, t_u, rh
   !           nk         --- the number of vertical modes or levels
   !           num_bins2d --- the number of the 2d bins
   !           e_vec, e_val         --- global eigenvector and eigenvalues
   !           e_vec_loc, e_val_loc --- local eigenvectors and eigenvalues
   !
   ! Output  : fort.174,178,182,186,(190) --- The first 5 global eigenvectors
   !           fort.175,179,183,187,(191) --- The global eigenvalues
   !           fort.176,180,184,188,(192) --- The first 5 local eigenvectors
   !           fort.177,181,185,189,(193) --- The first 5 local eigenvalues
   !      
   !          * in parenthisis, the units for 2d fields ps_u (ps).
   !----------------------------------------------------------------------------

   implicit none

   integer, intent(inout)   :: outunit                    ! Output file unit.
   character*10, intent(in) :: variable                   ! Variable name
   integer, intent(in)      :: nk                         ! Vertical dimension.
   integer, intent(in)      :: num_bins2d                 ! # bins for 2D fields.
   real, intent(in)         :: e_vec(1:nk,1:nk)           ! Domain-averaged eigenvectors.
   real, intent(in)         :: e_val(1:nk)                ! Domain-averaged eigenvalues.
   real, intent(in) :: e_vec_loc(1:nk,1:nk,1:num_bins2d)  ! Latitudinally varying eigenvectors.
   real, intent(in) :: e_val_loc(1:nk,1:num_bins2d)       ! Latitudinally varying eigenvalues.

   integer                  :: k, m, b, mn                ! Loop counters.

   if (trace_use) call da_trace_entry("da_print_be_stats_v")

   if (nk > 5) then
      mn = 5
   else
      mn = nk
   end if

   ! 1, Global vectors:
   write(unit=stdout,fmt='(3a,i5)')' First 5 Global eigenvectors for variable ', trim(variable), &
                     ' in unit ', outunit

   open(unit=outunit)
   do k = 1, nk
      write(unit=outunit,fmt='(i4,5f15.8)') k, (e_vec(k,m), m = 1, mn)
   end do
   close(unit=outunit)

   ! 2, Global values:
   outunit = outunit + 1

   write(unit=stdout,fmt='(3a,i5)')' Global eigenvalues for variable ', trim(variable), &
                     ' in unit ', outunit

   open(unit=outunit)
   do k = 1, nk
      write(unit=outunit,fmt='(i4,1pe18.5)') k, e_val(k)
   end do
   close(unit=outunit)

   ! 3, Local vectors:

   outunit = outunit + 1

   write(unit=stdout,fmt='(3a,i5)')' First 5 local eigenvectors for variable ', trim(variable), &
                     ' in unit ', outunit

   open(unit=outunit)
   do b = 1, num_bins2d
     write(unit=outunit,fmt='(/"bin =",i6)') b
     do k = 1, nk
       write(unit=outunit,fmt='(i4,5f15.8)') k, (e_vec_loc(k,m,b), m = 1, mn)
     end do
   end do
   close(unit=outunit)

   ! 4. Local values:

   outunit = outunit + 1

   write(unit=stdout,fmt='(3a,i5)')' First 5 local eigenvalues for variable ', trim(variable), &
                     ' in unit ', outunit

   open(unit=outunit)
   do b = 1, num_bins2d
      write(unit=outunit,fmt='(i4,5(1pe18.5))') b, (e_val_loc(m,b), m = 1, mn)
   end do
   close(unit=outunit)

   outunit = outunit + 1 
   write(unit=stdout,fmt=*) ' '

   if (trace_use) call da_trace_exit("da_print_be_stats_v")

end subroutine da_print_be_stats_v


