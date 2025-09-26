subroutine da_copy_tile_dims(grid)

   !---------------------------------------------------------------------------
   ! Purpose: Copy tile dimensions from grid structure.
   !---------------------------------------------------------------------------

   implicit none

   type(domain), intent(inout)         :: grid

   integer                  :: ij   ! Loop counter

   if (trace_use_dull) call da_trace_entry("da_copy_tile_dims")

   ! De-reference tile indices stored in the grid data structure.

   its = MINVAL( grid%i_start(1:grid%num_tiles) )
   ite = MAXVAL( grid%i_end(1:grid%num_tiles) )
   jts = MINVAL( grid%j_start(1:grid%num_tiles) )
   jte = MAXVAL( grid%j_end(1:grid%num_tiles) )
   kts = grid%xp%kds
   kte = grid%xp%kde

   grid%xp%its = its
   grid%xp%ite = ite
   grid%xp%jts = jts
   grid%xp%jte = jte
   grid%xp%kts = kts
   grid%xp%kte = kte

   if (grid%xp%ite > grid%xp%ide) grid%xp%ite = grid%xp%ide
   if (grid%xp%jte > grid%xp%jde) grid%xp%jte = grid%xp%jde
   if (grid%xp%kte > grid%xp%kde) grid%xp%kte = grid%xp%kde

   if (ite > grid%xp%ide) ite = grid%xp%ide
   if (jte > grid%xp%jde) jte = grid%xp%jde
   if (kte > grid%xp%kde) kte = grid%xp%kde

   do ij = 1 , grid%num_tiles
      if (print_detail_parallel) then
         write(unit=stdout, fmt='(/)')
         write(unit=stdout, fmt='(A,i3,A,5x,3(i3,A,i3,5x))') 'Tile ',ij, &
                 ' size:', its,':',ite, jts,':',jte, kts,':',kte
      end if
   end do

   if (trace_use_dull) call da_trace_exit("da_copy_tile_dims")

end subroutine da_copy_tile_dims


