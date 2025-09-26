subroutine da_copy_dims(grid)

   !---------------------------------------------------------------------------
   ! Purpose: Copy dimensioning information from grid structure.
   !---------------------------------------------------------------------------

   implicit none

   type(domain), intent(inout)         :: grid

   if (trace_use_dull) call da_trace_entry("da_copy_dims")

   ! De-reference dimension information stored in the grid data structure.

   ids = grid%sd31 
   ide = grid%ed31 - 1
   jds = grid%sd32 
   jde = grid%ed32 - 1
   kds = grid%sd33 
   kde = grid%ed33 - 1

   ims = grid%sm31 
   ime = grid%em31 
   jms = grid%sm32 
   jme = grid%em32 
   kms = grid%sm33 
   kme = grid%em33 

   ips = grid%sp31 
   ipe = grid%ep31 
   jps = grid%sp32 
   jpe = grid%ep32 
   kps = grid%sp33 
   kpe = grid%ep33 

   ! Indices for yz decomposition

   grid%xp%idsx = grid%sd31
   grid%xp%idex = grid%ed31 - 1
   grid%xp%jdsx = grid%sd32
   grid%xp%jdex = grid%ed32 - 1
   grid%xp%kdsx = grid%sd33
   grid%xp%kdex = grid%ed33 - 1

   grid%xp%imsx = grid%sm31x
   grid%xp%imex = grid%em31x
   grid%xp%jmsx = grid%sm32x
   grid%xp%jmex = grid%em32x
   grid%xp%kmsx = grid%sm33x
   grid%xp%kmex = grid%em33x

   grid%xp%itsx = grid%sp31x
   grid%xp%itex = grid%ep31x
   grid%xp%jtsx = grid%sp32x
   grid%xp%jtex = grid%ep32x
   grid%xp%ktsx = grid%sp33x
   grid%xp%ktex = grid%ep33x

   grid%xp%ipsx = grid%sp31x
   grid%xp%ipex = grid%ep31x
   grid%xp%jpsx = grid%sp32x
   grid%xp%jpex = grid%ep32x
   grid%xp%kpsx = grid%sp33x
   grid%xp%kpex = grid%ep33x

   ! Indices for xz decomposition

   grid%xp%idsy = grid%sd31
   grid%xp%idey = grid%ed31 - 1
   grid%xp%jdsy = grid%sd32
   grid%xp%jdey = grid%ed32 - 1
   grid%xp%kdsy = grid%sd33
   grid%xp%kdey = grid%ed33 - 1

   grid%xp%imsy = grid%sm31y
   grid%xp%imey = grid%em31y
   grid%xp%jmsy = grid%sm32y
   grid%xp%jmey = grid%em32y
   grid%xp%kmsy = grid%sm33y
   grid%xp%kmey = grid%em33y

   grid%xp%itsy = grid%sp31y
   grid%xp%itey = grid%ep31y
   grid%xp%jtsy = grid%sp32y
   grid%xp%jtey = grid%ep32y
   grid%xp%ktsy = grid%sp33y
   grid%xp%ktey = grid%ep33y

   grid%xp%ipsy = grid%sp31y
   grid%xp%ipey = grid%ep31y
   grid%xp%jpsy = grid%sp32y
   grid%xp%jpey = grid%ep32y
   grid%xp%kpsy = grid%sp33y
   grid%xp%kpey = grid%ep33y

   if (ipe > ide) ipe = ide
   if (jpe > jde) jpe = jde
   if (kpe > kde) kpe = kde

   ! Indices for yz decomposition

   if (grid%xp%itex > ide) grid%xp%itex = ide
   if (grid%xp%jtex > jde) grid%xp%jtex = jde
   if (grid%xp%ktex > kde) grid%xp%ktex = kde

   if (grid%xp%ipex > ide) grid%xp%ipex = ide
   if (grid%xp%jpex > jde) grid%xp%jpex = jde
   if (grid%xp%kpex > kde) grid%xp%kpex = kde

   ! Indices for xz decomposition

   if (grid%xp%itey > ide) grid%xp%itey = ide
   if (grid%xp%jtey > jde) grid%xp%jtey = jde
   if (grid%xp%ktey > kde) grid%xp%ktey = kde

   if (grid%xp%ipey > ide) grid%xp%ipey = ide
   if (grid%xp%jpey > jde) grid%xp%jpey = jde
   if (grid%xp%kpey > kde) grid%xp%kpey = kde

   ! Copy grid%xpose dimensions from grid structure to grid%xp structure.

   ! Indices for xy decomposition

   grid%xp%ids = ids
   grid%xp%ide = ide
   grid%xp%jds = jds
   grid%xp%jde = jde
   grid%xp%kds = kds
   grid%xp%kde = kde

   grid%xp%ims = ims
   grid%xp%ime = ime
   grid%xp%jms = jms
   grid%xp%jme = jme
   grid%xp%kms = kms
   grid%xp%kme = kme

   grid%xp%ips = ips
   grid%xp%ipe = ipe
   grid%xp%jps = jps
   grid%xp%jpe = jpe
   grid%xp%kps = kps
   grid%xp%kpe = kpe

   if (print_detail_parallel) then
      write(unit=stdout, fmt='(2(a, i4, 5x))') &
           'grid%xp%ids =', grid%xp%ids , 'grid%xp%ide =', grid%xp%ide , &
           'grid%xp%jds =', grid%xp%jds , 'grid%xp%jde =', grid%xp%jde , &
           'grid%xp%kds =', grid%xp%kds , 'grid%xp%kde =', grid%xp%kde
      write(unit=stdout, fmt='(//)')

      write(unit=stdout, fmt='(2(a, i4, 5x))') &
           'grid%xp%ims =', grid%xp%ims , 'grid%xp%ime =', grid%xp%ime , &
           'grid%xp%jms =', grid%xp%jms , 'grid%xp%jme =', grid%xp%jme , &
           'grid%xp%kms =', grid%xp%kms , 'grid%xp%kme =', grid%xp%kme
      write(unit=stdout, fmt='(//)')

      write(unit=stdout, fmt='(2(a, i4, 5x))') &
           'grid%xp%ips =', grid%xp%ips , 'grid%xp%ipe =', grid%xp%ipe , &
           'grid%xp%jps =', grid%xp%jps , 'grid%xp%jpe =', grid%xp%jpe , &
           'grid%xp%kps =', grid%xp%kps , 'grid%xp%kpe =', grid%xp%kpe
      write(unit=stdout, fmt='(//)')

      write(unit=stdout, fmt='(2(a, i4, 5x))') &
           'grid%xp%imsx =', grid%xp%imsx, 'grid%xp%imex=', grid%xp%imex, &
           'grid%xp%jmsx =', grid%xp%jmsx, 'grid%xp%jmex=', grid%xp%jmex, &
           'grid%xp%kmsx =', grid%xp%kmsx, 'grid%xp%kmex=', grid%xp%kmex
      write(unit=stdout, fmt='(//)')

      write(unit=stdout, fmt='(2(a, i4, 5x))') &
           'grid%xp%ipsx =', grid%xp%ipsx, 'grid%xp%ipex=', grid%xp%ipex, &
           'grid%xp%jpsx =', grid%xp%jpsx, 'grid%xp%jpex=', grid%xp%jpex, &
           'grid%xp%kpsx =', grid%xp%kpsx, 'grid%xp%kpex=', grid%xp%kpex
      write(unit=stdout, fmt='(//)')

      write(unit=stdout, fmt='(2(a, i4, 5x))') &
           'grid%xp%imsy =', grid%xp%imsy, 'grid%xp%imey=', grid%xp%imey, &
           'grid%xp%jmsy =', grid%xp%jmsy, 'grid%xp%jmey=', grid%xp%jmey, &
           'grid%xp%kmsy =', grid%xp%kmsy, 'grid%xp%kmey=', grid%xp%kmey
      write(unit=stdout, fmt='(//)')

      write(unit=stdout, fmt='(2(a, i4, 5x))') &
           'grid%xp%ipsy =', grid%xp%ipsy, 'grid%xp%ipey=', grid%xp%ipey, &
           'grid%xp%jpsy =', grid%xp%jpsy, 'grid%xp%jpey=', grid%xp%jpey, &
           'grid%xp%kpsy =', grid%xp%kpsy, 'grid%xp%kpey=', grid%xp%kpey
   end if

   if (trace_use_dull) call da_trace_exit("da_copy_dims")

end subroutine da_copy_dims


