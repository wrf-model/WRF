program gen_be_diags

   use da_control, only : stderr, stdout, filename_len
   use da_tools_serial, only : da_get_unit
   use da_gen_be, only : da_readwrite_be_stage2, da_readwrite_be_stage3, &
      da_readwrite_be_stage4

   implicit none

   character*10        :: variable                   ! Variable name
   character*8         :: uh_method                  ! Uh_method (power, scale)
   integer             :: n_smth_sl                  ! Number of smoothing for scale-length
   character(len=filename_len)        :: filename                   ! Input filename.
   integer             :: nk,nk_3d                   ! Dimensions read in.

   namelist / gen_be_diags_nl / uh_method, n_smth_sl

   integer :: ounit,iunit,namelist_unit

   stderr = 0
   stdout = 6

   call da_get_unit(ounit)
   call da_get_unit(iunit)
   call da_get_unit(namelist_unit)

   uh_method = 'scale'
   n_smth_sl = 0

   open(unit=namelist_unit, file='gen_be_diags_nl.nl', &
      form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_diags_nl)
   close(namelist_unit)

   filename = 'be.dat'
   open (ounit, file = filename, form='unformatted')

   write(6,'(/a)')' [1] Gather regression coefficients.'

   call da_readwrite_be_stage2( ounit, nk )

   write(6,'(/a)')' [2] Gather vertical error eigenvectors, eigenvalues.'

   variable = 'psi'
   call da_readwrite_be_stage3( ounit, nk, variable )

   variable = 'chi_u'
   call da_readwrite_be_stage3( ounit, nk, variable )

   variable = 't_u'
   call da_readwrite_be_stage3( ounit, nk, variable )

   variable = 'rh'
   call da_readwrite_be_stage3( ounit, nk, variable )

   ! To keep the dimension nk for 3d fields:
   nk_3d = nk

   if (uh_method /= 'power') then
      variable = 'ps_u'
      call da_readwrite_be_stage3( ounit,  1, variable )
   end if

   if (uh_method == 'power') then
      write(6,'(/a)')' [3] Gather horizontal error power spectra.'
   else if (uh_method == 'scale') then
      write(6,'(/a)')' [3] Gather horizontal scale length.'
   end if

   ! To assign the dimension nk for 3d fields:
   nk = nk_3d

   variable = 'psi'
   call da_readwrite_be_stage4( ounit, nk, uh_method, n_smth_sl, variable )

   variable = 'chi_u'
   call da_readwrite_be_stage4( ounit, nk, uh_method, n_smth_sl, variable )

   variable = 't_u'
   call da_readwrite_be_stage4( ounit, nk, uh_method, n_smth_sl, variable )

   variable = 'rh'
   call da_readwrite_be_stage4( ounit, nk, uh_method, n_smth_sl, variable )

   variable = 'ps_u'
   call da_readwrite_be_stage4( ounit, 1, uh_method, n_smth_sl, variable )

   close(ounit)

end program gen_be_diags
