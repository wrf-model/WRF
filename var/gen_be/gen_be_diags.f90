program gen_be_diags
!------------------------------------------------------------------------
!  Purpose: Gathers background error statistics generated at various 
!           stages WRFDA "gen_be"
!
!  Auothor: Syed RH Rizvi (MMM/NESL/NCAR)   Date: 02/01/2010
!
!  Note: Please acknowledge author/institute in work that uses this code.
!------------------------------------------------------------------------

   use da_control, only : do_normalize,filename_len,stderr,stdout,use_rf
   use da_tools_serial, only : da_get_unit
   use da_gen_be, only : da_readwrite_be_stage1, da_readwrite_be_stage2, da_readwrite_be_stage3, &
                         da_readwrite_be_stage4

   implicit none

   character*10        :: variable                   ! Variable name
   character*8         :: uh_method                  ! Uh_method (power, scale, wavelet)
   integer             :: n_smth_sl                  ! Number of smoothing for scale-length
   integer             :: cv_options                 ! WRFDA CV_OPTIONS    
   character(len=filename_len)        :: filename                   ! Input filename.
   integer             :: nk,nk_3d                   ! Dimensions read in.

   namelist / gen_be_diags_nl / cv_options, do_normalize, n_smth_sl, uh_method, use_rf

   integer :: ounit,iunit,namelist_unit

   stderr = 0
   stdout = 6
   cv_options = 5

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

   if( cv_options == 7) then
      write(6,'(/a)')' [1] Gather dimensions and bin information.'
      call da_readwrite_be_stage1( ounit, nk )

      write(6,'(/a)')' [2] Gather vertical error eigenvectors, eigenvalues.'

      variable = 'u'
      call da_readwrite_be_stage3( ounit, nk, variable )

      variable = 'v'
      call da_readwrite_be_stage3( ounit, nk, variable )

      variable = 't'
      call da_readwrite_be_stage3( ounit, nk, variable )

      variable = 'rh'
      call da_readwrite_be_stage3( ounit, nk, variable )

      ! To keep the dimension nk for 3d fields:
      nk_3d = nk

      if (uh_method /= 'power') then
         variable = 'ps'
         call da_readwrite_be_stage3( ounit,  1, variable )
      end if
      if (uh_method == 'power') then
         write(6,'(/a)')' [3] Gather horizontal error power spectra.'
      else if (uh_method == 'scale') then
         if( use_rf )then
            write(6,'(/a)')' [3] Gather horizontal scale length.'
         else
            uh_method = 'wavelet'
         end if
      end if
      if (uh_method == 'wavelet') write(6,'(/" [3] Gather horizontal wavelet std. devs.")')

      ! To assign the dimension nk for 3d fields:
      nk = nk_3d

      variable = 'u'
      call da_readwrite_be_stage4( ounit, nk, uh_method, n_smth_sl, variable )

      variable = 'v'
      call da_readwrite_be_stage4( ounit, nk, uh_method, n_smth_sl, variable )

      variable = 't'
      call da_readwrite_be_stage4( ounit, nk, uh_method, n_smth_sl, variable )

      variable = 'rh'
      call da_readwrite_be_stage4( ounit, nk, uh_method, n_smth_sl, variable )

      variable = 'ps'
      call da_readwrite_be_stage4( ounit, 1, uh_method, n_smth_sl, variable )
   else
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
      if( cv_options == 6) variable = 'rh_u'
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
         if( use_rf )then
            write(6,'(/a)')' [3] Gather horizontal scale length.'
         else
            uh_method = 'wavelet'
         end if
      end if
      if (uh_method == 'wavelet') write(6,'(/" [3] Gather horizontal wavelet std. devs.")')

      ! To assign the dimension nk for 3d fields:
      nk = nk_3d

      variable = 'psi'
      call da_readwrite_be_stage4( ounit, nk, uh_method, n_smth_sl, variable )

      variable = 'chi_u'
      call da_readwrite_be_stage4( ounit, nk, uh_method, n_smth_sl, variable )

      variable = 't_u'
      call da_readwrite_be_stage4( ounit, nk, uh_method, n_smth_sl, variable )

      variable = 'rh'
      if( cv_options == 6) variable = 'rh_u'
      call da_readwrite_be_stage4( ounit, nk, uh_method, n_smth_sl, variable )

      variable = 'ps_u'
      call da_readwrite_be_stage4( ounit, 1, uh_method, n_smth_sl, variable )
   end if

   close(ounit)

end program gen_be_diags
