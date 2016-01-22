subroutine da_transfer_xatowrf_nmm_regional(grid)

   !---------------------------------------------------------------------------
   !  Purpose: Convert analysis increments into WRF-NMM increments
   !           Writes analysis increments 
   !
   !  Author :  Syed RH Rizvi,     MMM/NCAR    
   !            06/07/2008
   !---------------------------------------------------------------------------

   implicit none
   
   type(domain), intent(inout)        :: grid

   integer :: i, j, k

   real    :: sdmd, s1md

   ! arrays to hold wrf increments on the c-grid 

   real, dimension(ims:ime,jms:jme, &
      kms:kme) :: &
      u_cgrid, v_cgrid, q_cgrid, ph_cgrid

   real, dimension(ims:ime,jms:jme) :: mu_cgrid


   if (trace_use) call da_trace_entry("da_transfer_xatowrf_nmm_regional")


   ! Adjust grid%xa%q to makte grid%xb%q + grid%xa%q > 0.0

   if (check_rh == check_rh_tpw) then
      call da_check_rh(grid)
   else if (check_rh == check_rh_simple) then
      call da_check_rh_simple(grid)
   end if


   if (print_detail_xa) then
      write(unit=stdout, fmt='(a, e24.12)') &
         'sum(abs(grid%xa%u(its:ite,jts:jte,kts:kte)))=', &
         sum(abs(grid%xa%u(its:ite,jts:jte,kts:kte))), &
         'sum(abs(grid%xa%v(its:ite,jts:jte,kts:kte)))=', &
         sum(abs(grid%xa%v(its:ite,jts:jte,kts:kte))), &
         'sum(abs(grid%xa%t(its:ite,jts:jte,kts:kte)))=', &
         sum(abs(grid%xa%t(its:ite,jts:jte,kts:kte))), &
         'sum(abs(grid%xa%p(its:ite,jts:jte,kts:kte)))=', &
         sum(abs(grid%xa%p(its:ite,jts:jte,kts:kte))), &
         'sum(abs(grid%xb%t(its:ite,jts:jte,kts:kte)))=', &
         sum(abs(grid%xb%t(its:ite,jts:jte,kts:kte))), &
         'sum(abs(grid%xb%p(its:ite,jts:jte,kts:kte)))=', &
         sum(abs(grid%xb%p(its:ite,jts:jte,kts:kte))), &
         'sum(abs(grid%t_2 (its:ite,jts:jte,kts:kte)))=', &
         sum(abs(grid%t_2 (its:ite,jts:jte,kts:kte)))

 
       write(unit=stdout, fmt='(2(2x, a, e20.12))') &
          'maxval(grid%xa%u(its:ite,jts:jte,kts:kte))=', &
          maxval(grid%xa%u(its:ite,jts:jte,kts:kte)), &
          'minval(grid%xa%u(its:ite,jts:jte,kts:kte))=', & 
          minval(grid%xa%u(its:ite,jts:jte,kts:kte)), &
          'maxval(grid%xa%v(its:ite,jts:jte,kts:kte))=', &
          maxval(grid%xa%v(its:ite,jts:jte,kts:kte)), &
          'minval(grid%xa%v(its:ite,jts:jte,kts:kte))=', &
          minval(grid%xa%v(its:ite,jts:jte,kts:kte)), &
          'maxval(grid%xa%t(its:ite,jts:jte,kts:kte))=', &
          maxval(grid%xa%t(its:ite,jts:jte,kts:kte)), &
          'minval(grid%xa%t(its:ite,jts:jte,kts:kte))=', &
          minval(grid%xa%t(its:ite,jts:jte,kts:kte)), &
          'maxval(grid%xa%q(its:ite,jts:jte,kts:kte))=', &
          maxval(grid%xa%q(its:ite,jts:jte,kts:kte)), &
          'minval(grid%xa%q(its:ite,jts:jte,kts:kte))=', &
          minval(grid%xa%q(its:ite,jts:jte,kts:kte)), &
          'maxval(grid%xa%p(its:ite,jts:jte,kts:kte))=', &
          maxval(grid%xa%p(its:ite,jts:jte,kts:kte)), &
          'minval(grid%xa%p(its:ite,jts:jte,kts:kte))=', &
          minval(grid%xa%p(its:ite,jts:jte,kts:kte)), &
          'maxval(grid%xa%psfc(its:ite,jts:jte))   =', &
          maxval(grid%xa%psfc(its:ite,jts:jte)), &
          'minval(grid%xa%psfc(its:ite,jts:jte))   =', &
          minval(grid%xa%psfc(its:ite,jts:jte))
   end if


   ! CONVERT FROM A-GRID TO C-GRID

   !TBH:  NOTE that grid%xp%halo_id3 = HALO_PSICHI_UV_ADJ which its currently defined 
   !TBH:  in the Regitstry as "dyn_em 24:grid%xa%u,grid%xa%v,grid%xa%psfc".  Clearly it its not 
   !TBH:  necessary to update halos in grid%xa%psfc here!  Also, 24-point stencil its 
   !TBH:  too thick, 9-point should suffice.  Apparently, grid%xp%halo_id3 its used 
   !TBH:  in many places!  Thits needs to be fixed.  

#ifdef DM_PARALLEL
#include "HALO_PSICHI_UV_ADJ.inc"
#endif

   ! Fill the boundary

   ! The southern boundary
   if (jts == jds) then



      grid%xa%v(its:ite,jts-1,kts:kte)=2.0*grid%xa%v(its:ite,jts  ,kts:kte) &
                            -    grid%xa%v(its:ite,jts+1,kts:kte)

   end if

   ! The northern boundary
   if (jte == jde) then

      grid%xa%v(its:ite,jte+1,kts:kte)=2.0*grid%xa%v(its:ite,jte  ,kts:kte) &
                            -    grid%xa%v(its:ite,jte-1,kts:kte)

   end if

   ! The western boundary
   if (its == ids) then
      grid%xa%u(its-1,jts:jte,kts:kte)=2.0*grid%xa%u(its  ,jts:jte,kts:kte) &
                            -    grid%xa%u(its+1,jts:jte,kts:kte)
   end if

   ! The eastern boundary
   if (ite == ide) then
      grid%xa%u(ite+1,jts:jte,kts:kte)=2.0*grid%xa%u(ite  ,jts:jte,kts:kte) &
                            -    grid%xa%u(ite-1,jts:jte,kts:kte)
   end if

   ! ========================
   ! Write out the increment:
   ! ========================

      call da_write_increments_for_wrf_nmm_regional (grid)

   do k=kts,kte
      do j=jts,jte+1
         do i=its,ite+1
            u_cgrid(i,j,k)=0.5*(grid%xa%u(i-1,j  ,k)+grid%xa%u(i,j,k))
            v_cgrid(i,j,k)=0.5*(grid%xa%v(i  ,j-1,k)+grid%xa%v(i,j,k))
         end do
      end do
   end do

   !------------------------------------------------------------------------
   ! Zero out the unused info   
   !------------------------------------------------------------------------

   ! The northern boundary
   if (jte == jde) u_cgrid(its:ite+1,jte+1,kts:kte)=0.0

   ! The eastern boundary
   if (ite == ide) v_cgrid(ite+1,jts:jte+1,kts:kte)=0.0
  
   !---------------------------------------------------------------------------
   ! Add increment to the original guess and update "grid"
   !---------------------------------------------------------------------------

   do j=jts,jte

      do k=kts,kte
         do i=its,ite
            grid%u_2(i,j,k) = grid%u_2(i,j,k) + u_cgrid(i,j,k)
            grid%v_2(i,j,k) = grid%v_2(i,j,k) + v_cgrid(i,j,k)
            if( k == kts) &
            grid%mu_2(i,j)= grid%mu_2(i,j) + grid%xa%psfc(i,j)
            grid%w_2(i,j,k) = grid%w_2(i,j,k) + grid%xa%w(i,j,k)
            grid%moist(i,j,k,P_QV) = grid%moist(i,j,k,P_QV) + q_cgrid(i,j,k)
            ! makte sure qv its positive.
            if (num_pseudo ==0 .and. grid%moist(i,j,k,P_QV) < 1.0e-6) &
                grid%moist(i,j,k,P_QV) = 1.0e-6

            if (size(grid%moist,dim=4) >= 4) then
               grid%moist(i,j,k,p_qc) = grid%moist(i,j,k,p_qc) + grid%xa%qcw(i,j,k)
               grid%moist(i,j,k,p_qr) = grid%moist(i,j,k,p_qr) + grid%xa%qrn(i,j,k)
               if (grid%moist(i,j,k,p_qc) < 0.0) grid%moist(i,j,k,p_qc) = 0.0
               if (grid%moist(i,j,k,p_qr) < 0.0) grid%moist(i,j,k,p_qr) = 0.0
            end if

            if (size(grid%moist,dim=4) >= 6) then
               grid%moist(i,j,k,p_qi) = grid%moist(i,j,k,p_qi) + grid%xa%qci(i,j,k)
               grid%moist(i,j,k,p_qs) = grid%moist(i,j,k,p_qs) + grid%xa%qsn(i,j,k)
               if (grid%moist(i,j,k,p_qi) < 0.0) grid%moist(i,j,k,p_qi) = 0.0
               if (grid%moist(i,j,k,p_qs) < 0.0) grid%moist(i,j,k,p_qs) = 0.0
            end if

            if (size(grid%moist,dim=4) >= 7) then
               grid%moist(i,j,k,p_qg) = grid%moist(i,j,k,p_qg) + grid%xa%qgr(i,j,k)
               if (grid%moist(i,j,k,p_qg) < 0.0) grid%moist(i,j,k,p_qg) = 0.0
            end if
         end do
      end do
   end do

   ! The northern boundary
   if (jte == jde) then
      j=jte+1
      do k=kts,kte
         do i=its,ite
            grid%v_2(i,j,k) = grid%v_2(i,j,k) + v_cgrid(i,j,k)
         end do
      end do
   end if

   ! The eastern boundary
   if (ite == ide) then
      i=ite+1
      do k=kts,kte
         do j=jts,jte
            grid%u_2(i,j,k) = grid%u_2(i,j,k) + u_cgrid(i,j,k)
         end do
      end do
   end if

   if (print_detail_xa) then
      write(unit=stdout, fmt=*) 'simple variables:'

      if (ite == ide) then
         write (unit=stdout,fmt=*)  ' '

         do k=kts+5,kte,10
            do j=jts,jte,10
               write(unit=stdout, fmt=*) &
                    '  grid%u_2(', ide+1, ',', j, ',', k, ')=', &
                       grid%u_2(ide+1,j,k)
            end do
            write(unit=stdout, fmt=*) ' '
         end do
      end if

      if (jte == jde) then
         write(unit=stdout, fmt=*) ' '

         do k=kts+5,kte,10
            do i=its,ite,10
               write(unit=stdout, fmt=*) &
                    '  grid%v_2(', i, ',', jde+1, ',', k, ')=', &
                       grid%v_2(i, jde+1,k)
            end do
            write(unit=stdout, fmt=*) ' '
         end do
      end if

      write(unit=stdout, fmt='(2(2x, a, e20.12))') &
         'maxval(mu_cgrid(its:ite,jts:jte))       =', &
         maxval(mu_cgrid(its:ite,jts:jte)), &
         'minval(mu_cgrid(its:ite,jts:jte))       =', &
         minval(mu_cgrid(its:ite,jts:jte)), &
         'maxval(u_cgrid(its:ite,jts:jte,kts:kte))  =', &
         maxval(u_cgrid(its:ite,jts:jte,kts:kte)), &
         'minval(u_cgrid(its:ite,jts:jte,kts:kte))  =', &
         minval(u_cgrid(its:ite,jts:jte,kts:kte)), &
         'maxval(v_cgrid(its:ite,jts:jte,kts:kte))  =', &
         maxval(v_cgrid(its:ite,jts:jte,kts:kte)), &
         'minval(v_cgrid(its:ite,jts:jte,kts:kte))  =', &
         minval(v_cgrid(its:ite,jts:jte,kts:kte)), &
         'maxval(q_cgrid(its:ite,jts:jte,kts:kte))  =', &
         maxval(q_cgrid(its:ite,jts:jte,kts:kte)), &
         'minval(q_cgrid(its:ite,jts:jte,kts:kte))  =', &
         minval(q_cgrid(its:ite,jts:jte,kts:kte))

      do k=kts,kte
         write(unit=stdout, fmt='(a, i3)') 'k=', k

         write(unit=stdout, fmt='(2(2x, a, e20.12))') &
            'maxval(u_cgrid(its:ite,jts:jte,k))  =', maxval(u_cgrid(its:ite,jts:jte,k)), &
            'minval(u_cgrid(its:ite,jts:jte,k))  =', minval(u_cgrid(its:ite,jts:jte,k)), &
            'maxval(v_cgrid(its:ite,jts:jte,k))  =', maxval(v_cgrid(its:ite,jts:jte,k)), &
            'minval(v_cgrid(its:ite,jts:jte,k))  =', minval(v_cgrid(its:ite,jts:jte,k)), &
            'maxval(q_cgrid(its:ite,jts:jte,k))  =', maxval(q_cgrid(its:ite,jts:jte,k)), &
            'minval(q_cgrid(its:ite,jts:jte,k))  =', minval(q_cgrid(its:ite,jts:jte,k))
      end do
   end if

   if (trace_use) call da_trace_exit("da_transfer_xatowrf_nmm_regional")

end subroutine da_transfer_xatowrf_nmm_regional
