module module_interp_store
  ! MODULE module_interp_store
  ! PURPOSE: Stores interpolation information for the current
  !   high-resolution domain.  The actual information is not 
  !   stored; just pointers to the corresponding elements of
  !   the grid (domain) object.  To update the pointers, call
  !   store_interp_info.  Note that, regardless of the direction
  !   or type of interpolation, the high-resolution domain is
  !   the one stored (when interpolating from parent to nest or
  !   nest to parent, nest info is stored).
  ! AUTHOR: Samuel Trahan
  ! HISTORY:
  !   August, 2012 - initial creation
  !   December, 2013 - added kpres, which stores the pressure-sigma
  !       transition level
  implicit none

  integer, pointer, dimension(:,:) :: IIH,JJH,IIV,JJV
  real, pointer, dimension(:,:) :: HBWGT1,HBWGT2,HBWGT3,HBWGT4
  real, pointer, dimension(:,:) :: VBWGT1,VBWGT2,VBWGT3,VBWGT4

  integer :: grid_id, parent_grid_id
  integer, pointer, dimension(:,:,:) :: iinfo,parent_iinfo, &
                                        iinfo_bxs, iinfo_bxe, &
                                        iinfo_bys, iinfo_bye
  real, pointer, dimension(:,:,:) :: winfo,parent_winfo, &
                                     winfo_bxs, winfo_bxe, &
                                     winfo_bys, winfo_bye
  integer, pointer, dimension(:,:) :: hnear_i, hnear_j
  integer :: kpres
  real, pointer, dimension(:,:) :: parent_fis, nest_fis

end module module_interp_store

subroutine store_interp_info(grid, parent_grid)
  use module_domain_type, only : domain
  use module_interp_store
  implicit none
  type(domain), intent(in) :: grid, parent_grid
  
end subroutine store_interp_info

