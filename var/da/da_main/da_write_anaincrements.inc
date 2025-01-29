subroutine da_write_anaincrements(grid, config_flags)

  use module_domain, only : get_ijk_from_grid, program_name
  use da_control,    only : use_radarobs, use_rad, crtm_cloud, &
                            use_radar_rhv, use_radar_rqv
  use module_state_description, only : p_qv, p_qc, p_qr, p_qi, &
                                       p_qs, p_qg, &
                                       f_qc, f_qr, f_qi, f_qs, f_qg
  use module_model_constants, only: R_d, R_v, T0
#if (WRF_CHEM == 1)
  use module_domain_type, only : fieldlist
  use da_control,    only : use_chemic_surfobs, stdout
  use module_state_description, only : PARAM_FIRST_SCALAR, num_chem
#endif

  implicit none

  INTERFACE
    integer(c_int32_t) function copyfile(ifile, ofile) bind(c)
      import :: c_int32_t, C_CHAR
      CHARACTER(KIND=C_CHAR), DIMENSION(*), intent(in) :: ifile, ofile
    END function copyfile
  END INTERFACE

  include 'netcdf.inc'

  type(domain), intent(inout)             :: grid
  type(grid_config_rec_type),intent(inout) :: config_flags

! Declare local parameters
  character(len=120) :: file_name,filnam
  character(len=19)  :: DateStr1
  character(len=4)   :: staggering=' N/A'
  character(len=3)   :: ordering
  character(len=80), dimension(3)  ::  dimnames
  character(len=80)  :: rmse_var
  integer            :: dh1,dh0
  integer            :: i,j,k
  integer            :: ndim1
  integer            :: WrfType
  integer            :: it, ierr, Status, Status_next_time
  integer            :: wrf_real
  integer            :: nlon_regional,nlat_regional,nsig_regional
  integer            :: ids, ide, jds, jde, kds, kde,    &
                        ims, ime, jms, jme, kms, kme,    &
                        ips, ipe, jps, jpe, kps, kpe    
  integer, dimension(4)           :: start_index, end_index1
  real, dimension(:), allocatable :: globbuf
  real*4,allocatable :: field3(:,:,:),field2(:,:)
  real*4,allocatable :: field3u(:,:,:),field3v(:,:,:),field3ph(:,:,:)
  character(len=4) ::  fgname
  integer :: julyr, julday
  real    :: gmt
  real*4  :: gmt4
  real    :: qvf

      if ( grid%auxhist5_oid .NE. 0 ) then
         call close_dataset ( grid%auxhist5_oid , config_flags , "DATASET=AUXHIST5" )
      endif

      call open_w_dataset (grid%auxhist5_oid, trim(config_flags%auxhist5_outname), grid, config_flags, &
                            output_auxhist5, "DATASET=AUXHIST5", ierr)
      if ( ierr .NE. 0 ) CALL wrf_error_fatal('Error opening '//trim(filnam))

      start_date=current_date

      call geth_julgmt(julyr, julday, gmt)
      config_flags%gmt = gmt
      config_flags%julyr = julyr
      config_flags%julday = julday

      call output_auxhist5 (grid%auxhist5_oid, grid , config_flags , ierr)
      if ( ierr .NE. 0 ) CALL wrf_error_fatal('Error writing Gradient in auxhist5')
      call close_dataset (grid%auxhist5_oid, config_flags, "DATASET=AUXHIST5")

end subroutine da_write_anaincrements
