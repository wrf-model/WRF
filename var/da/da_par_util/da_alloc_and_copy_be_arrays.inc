subroutine da_alloc_and_copy_be_arrays (vg , v, jts,jte, kts,kte)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (be_subtype), intent(inout) :: vg       ! Global backgrd error struct.
   type (be_subtype), intent(inout) :: v        ! Local backgrd error struct.
   integer, intent(in)              :: jts,jte  ! Tile dimension.
   integer, intent(in)              :: kts,kte  ! Tile dimension.

   ! Allocate local-grid structure.
   v % mz = vg % mz
   if (v % mz > 0) then
      allocate  (v % val(jts:jte,1:v % mz))
      allocate  (v % evec(jts:jte,kts:kte,1:v % mz))
      allocate  (v % rf_alpha(1:v % mz))
   end if

   ! Make local copies from global arrays.
   if (v % mz > 0) then
       v % val(jts:jte,1:v % mz) = vg % val(jts:jte,1:v % mz)
       v % evec(jts:jte,kts:kte,1:v % mz) = vg % evec(jts:jte,kts:kte,1:v % mz)
       v % rf_alpha(1:v % mz) = vg % rf_alpha(1:v % mz)
   end if

   ! Deallocate global arrays.
   if (v % mz > 0) then 
      deallocate  (vg % val )
      deallocate  (vg % evec)
      deallocate  (vg % rf_alpha)
   end if

end subroutine da_alloc_and_copy_be_arrays 


