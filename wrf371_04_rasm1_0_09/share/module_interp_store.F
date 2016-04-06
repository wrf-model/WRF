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
  
#if (NMM_CORE == 1 && NMM_NEST == 1)
  kpres=-99999
  grid_id=grid%id
  parent_grid_id=parent_grid%id

  parent_fis=>parent_grid%fis
  nest_fis=>grid%fis

  hnear_i=>grid%hnear_i
  hnear_j=>grid%hnear_j

  parent_iinfo=>parent_grid%iinfo
  iinfo=>grid%iinfo
  iinfo_bxs=>grid%iinfo_bxs
  iinfo_bxe=>grid%iinfo_bxe
  iinfo_bys=>grid%iinfo_bys
  iinfo_bye=>grid%iinfo_bye
  
  parent_winfo=>parent_grid%winfo
  winfo=>grid%winfo
  winfo_bxs=>grid%winfo_bxs
  winfo_bxe=>grid%winfo_bxe
  winfo_bys=>grid%winfo_bys
  winfo_bye=>grid%winfo_bye
  
  IIV=>grid%IIV
  JJV=>grid%JJV
  VBWGT1=>grid%VBWGT1
  VBWGT2=>grid%VBWGT2
  VBWGT3=>grid%VBWGT3
  VBWGT4=>grid%VBWGT4
  
  IIH=>grid%IIH
  JJH=>grid%JJH
  HBWGT1=>grid%HBWGT1
  HBWGT2=>grid%HBWGT2
  HBWGT3=>grid%HBWGT3
  HBWGT4=>grid%HBWGT4
#endif
end subroutine store_interp_info

