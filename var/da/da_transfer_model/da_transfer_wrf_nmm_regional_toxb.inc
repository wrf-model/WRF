subroutine da_transfer_wrf_nmm_regional_toxb(xbx, grid)

   !---------------------------------------------------------------------------
   ! Purpose: Transfers fields from WRF-nmm model to first guess structure.
   ! Author :  Syed RH Rizvi,     MMM/NCAR    
   !           06/01/2008
   !---------------------------------------------------------------------------

   implicit none
   
   type (xbx_type), intent(inout) :: xbx        
   type(domain), intent(inout)    :: grid
   integer                        :: i, j, k
   character*19                   :: current_date
   real, allocatable              :: eta1(:), eta2(:)
   real                           :: tv, tmpvar

   if (trace_use) call da_trace_entry("da_transfer_wrf_nmm_regional_toxb")
               
   allocate (eta1(kts:kte+1))
   allocate (eta2(kts:kte+1))
   grid%xb % map  = grid%map_proj
   grid%xb % ds   = grid%dx  

   grid%xb % mix = grid%xp % ide - grid%xp % ids + 1
   grid%xb % mjy = grid%xp % jde - grid%xp % jds + 1
   grid%xb % mkz = grid%xp % kde - grid%xp % kds + 1


   ptop  = grid%p_top
   grid%xb % ptop = ptop
      
   eta1 = grid%znw
   eta2(kte+1) = 0.
   eta2(kts:kte) = grid%znu(kts:kte)
   if (print_detail_xb) then
      write(unit=stdout, fmt='(3a, i8)') &
         'file:', __FILE__, ', line:', __LINE__

      write(unit=stdout, fmt=*) 'its,ite=', its,ite
      write(unit=stdout, fmt=*) 'jts,jte=', jts,jte
      write(unit=stdout, fmt=*) 'kts,kte=', kts,kte

      write(unit=stdout, fmt='(/5a/)') &
         'lvl         eta1                eta2    '
      do k=kts,kte+1
         write(unit=stdout, fmt='(i3,8f16.8)') k, &
            eta1(k), eta2(k)    
      end do

      write(unit=stdout,fmt=*) &
         '  grid%u(its,jte,kts)=', grid%u_2(its,jte,kts)
      write(unit=stdout,fmt=*) &
         '  grid%v(ite,jts,kts)=', grid%v_2(ite,jts,kts)
      write(unit=stdout,fmt=*) &
         '  grid%t(its,jts,kts)=', grid%t_2(its,jts,kts)
      write(unit=stdout, fmt=*) &
         '  grid%sm31,grid%em31,grid%sm32,grid%em32, grid%sm33,grid%em33=', &
         grid%sm31,grid%em31,grid%sm32,grid%em32,grid%sm33,grid%em33

      write(unit=stdout, fmt=*) '  grid%p_top=', grid%p_top

      write(unit=stdout, fmt=*) '  num_moist=', num_moist
      write(unit=stdout, fmt=*) '  P_QV=', P_QV
      write(unit=stdout, fmt=*) '  P_QC=', P_QC
      write(unit=stdout, fmt=*) '  P_QR=', P_QR
      write(unit=stdout, fmt=*) '  P_QI=', P_QI
      write(unit=stdout, fmt=*) '  P_QS=', P_QS
      write(unit=stdout, fmt=*) '  P_QG=', P_QG

      write(unit=stdout, fmt=*) '  moist(its,jts,kts,p_qv)=', &
         grid%moist(its,jts,kts,p_qv)
      write(unit=stdout, fmt=*) ' '
   end if

   !---------------------------------------------------------------

   do j=jts,jte
      do k=kts,kte
         do i=its,ite+1
            grid%xa%u(i,j,k) = grid%u_2(i,j,k)
         end do
      end do
   end do

   do j=jts,jte+1
      do k=kts,kte
         do i=its,ite
            grid%xa%v(i,j,k) = grid%v_2(i,j,k)
         end do
      end do
   end do

   ! Fill the halo region for u and v.

#ifdef DM_PARALLEL
#include "HALO_PSICHI_UV_ADJ.inc"
#endif


   if (print_detail_xb) then
      write(unit=stdout, fmt=*) &
         ' ids,ide,jds,jde,kds,kde=', ids,ide,jds,jde,kds,kde
      write(unit=stdout, fmt=*) &
         ' its,ite,jts,jte,kts,kte=', its,ite,jts,jte,kts,kte
      write(unit=stdout, fmt=*) &
          ' ims,ime,jms,jme,kms,kme=', ims,ime,jms,jme,kms,kme
         
      write(unit=stdout, fmt=*) &
         ' lbound(grid%xb%u)=',   lbound(grid%xb%u)
      write(unit=stdout, fmt=*) &
         ' lbound(grid%xb%v)=',   lbound(grid%xb%v)
      write(unit=stdout, fmt=*) &
         ' lbound(grid%u)=', lbound(grid%u_2)
      write(unit=stdout, fmt=*) &
         ' lbound(grid%v)=', lbound(grid%v_2)
      write(unit=stdout, fmt=*) &
         ' ubound(grid%xb%u)=',   ubound(grid%xb%u)
      write(unit=stdout, fmt=*) &
         ' ubound(grid%xb%v)=',   ubound(grid%xb%v)
      write(unit=stdout, fmt=*) &
         ' ubound(grid%u)=', ubound(grid%u_2)
      write(unit=stdout, fmt=*) &
         ' ubound(grid%v)=', ubound(grid%v_2)
   end if

   do j=jts,jte

      do i=its,ite
         grid%xb%map_factor(i,j) = grid%msftx(i,j)
         grid%xb%cori(i,j) = grid%f(i,j)
         grid%xb%tgrn(i,j) = grid%sst(i,j)
         if (grid%xb%tgrn(i,j) < 100.0) &
            grid%xb%tgrn(i,j) = grid%tmn(i,j)
         grid%xb%lat(i,j) = grid%xlat(i,j)
         grid%xb%lon(i,j) = grid%xlong(i,j)
         grid%xb%terr(i,j) = grid%ht(i,j) 
         grid%xb%hf(i,j,kts) = grid%ht(i,j) 
!                               PD          +   PDTOP           + PTOP
         grid%xb%psfc(i,j) = grid%psfc(i,j) + grid%mu_2(i,j) + grid%p_top
      end do

      do k=kts,kte+1
        do i=its,ite
          grid%xb%p(i,j,k) = eta1(k)* grid%mu_2(i,j)  + eta2(k)*grid%psfc(i,j) + grid%p_top
          grid%xa%w (i,j,k) = grid%w_2(i,j,k)
          grid%xb%w (i,j,k) = grid%w_2(i,j,k)
        end do
      end do
      do k=kts+1,kte+1
        do i=its,ite
         tv = grid%t_2(i,j,k)*(1.0 + 0.61 * grid%moist(i,j,k,p_qv) )
         grid%xb%hf(i,j,k) =   grid%xb%hf(i,j,k-1)+ &
            ( gas_constant* tv * log(grid%xb%p(i,j,k-1)/grid%xb%p(i,j,k)) )/gravity 
        end do
      end do


      do k=kts,kte
        do i=its,ite
          grid%xb%u(i,j,k) = 0.5*(grid%xa%u(i,j,k)+grid%xa%u(i+1,j,k))
          grid%xb%v(i,j,k) = 0.5*(grid%xa%v(i,j,k)+grid%xa%v(i,j+1,k))
          grid%xb%wh(i,j,k)= 0.5*(grid%xb%w(i,j,k)+grid%xb%w(i,j,k+1))
          grid%xb%h(i,j,k) = 0.5*(grid%xb%hf(i,j,k)+grid%xb%hf(i,j,k+1))
          grid%xb%t(i,j,k) = grid%t_2(i,j,k)
          grid%xb%q(i,j,k) = grid%moist(i,j,k,P_QV)
          if( num_pseudo == 0 .and. grid%xb%q(i,j,k) < 1.0e-6) &
             grid%xb%q(i,j,k) = 1.0e-6
        end do
      end do

      do i=its,ite
        if (grid%xb%tgrn(i,j) < 100.0) &    
         grid%xb%tgrn(i,j) = grid%xb%t(i,j,kts)+ &
         0.0065*(grid%xb%h(i,j,kts)-grid%xb%hf(i,j,kts))
      end do
   end do     ! Loop over Latitudes (j)

   grid%xb%ztop = grid%xb%hf(its,jts,kte+1)

   do j=jts,jte
      do i=its,ite
         if (grid%xb%ztop < grid%xb%hf(i,j,kte+1)) &
             grid%xb%ztop = grid%xb%hf(i,j,kte+1)

         tmpvar = grid%xb%ds / grid%xb%map_factor(i,j)

         grid%xb % grid_box_area(i,j) = tmpvar*tmpvar
      end do
   end do
   if (print_detail_xb) then
      write(unit=stdout, fmt=*) ' '
      if (print_detail_xb) then
         write(unit=stdout, fmt='(/5a/)') &
            'lvl         h                 p                t'
         do k=kts,kte
            write(unit=stdout, fmt='(i3,8e20.12)') k, &
               grid%xb%h(its,jts,k), grid%xb%p(its,jts,k), grid%xb%t(its,jts,k)
         end do
      end if

      write(unit=stdout,fmt=*) ' '
      write(unit=stdout,fmt=*) 'grid%xb%u(its,jte,kte)=', grid%xb%u(its,jte,kte)
      write(unit=stdout,fmt=*) 'grid%xb%v(ite,jts,kte)=', grid%xb%v(ite,jts,kte)
      write(unit=stdout,fmt=*) 'grid%xb%t(its,jts,kte)=', grid%xb%t(its,jts,kte)
      write(unit=stdout,fmt=*) 'grid%xb%p(its,jts,kte)=', grid%xb%p(its,jts,kte)
      write(unit=stdout,fmt=*) 'grid%xb%q(its,jts,kte)=', grid%xb%q(its,jts,kte)
      write(unit=stdout,fmt=*) 'grid%xb%h(its,jts,kte)=', grid%xb%h(its,jts,kte)
      write(unit=stdout,fmt=*) 'grid%xb%w(its,jts,kte)=', grid%xb%w(its,jts,kte)
      write(unit=stdout,fmt=*) 'grid%xb%cori(its,jts)=', grid%xb%cori(its,jts)
      write(unit=stdout,fmt=*) &
         'grid%xb%hf(its,jts,kte)=', grid%xb%hf(its,jts,kte)
      write(unit=stdout,fmt=*) &
         'grid%xb%map_factor(its,jts)=', grid%xb%map_factor(its,jts)
      write(unit=stdout,fmt=*) 'grid%xb%tgrn(its,jts)=', grid%xb%tgrn(its,jts)
      write(unit=stdout,fmt=*) 'grid%xb%lat(its,jts)=', grid%xb%lat(its,jts)
      write(unit=stdout,fmt=*) 'grid%xb%lat(ite,jte)=', grid%xb%lat(ite,jte)
      write(unit=stdout,fmt=*) 'grid%xb%lon(its,jts)=', grid%xb%lon(its,jts)
      write(unit=stdout,fmt=*) 'grid%xb%lon(ite,jte)=', grid%xb%lon(ite,jte)
      write(unit=stdout,fmt=*) 'grid%xb%terr(its,jts)=', grid%xb%terr(its,jts)
      write(unit=stdout,fmt=*) ' '
   end if

   !---------------------------------------------------------------------------
   ! [3.0] Calculate vertical inner product for use in vertical transform:
   !---------------------------------------------------------------------------
      
   if (vertical_ip == vertical_ip_sqrt_delta_p) then
      ! Vertical inner product is sqrt(Delta p):
      do k=kts,kte
       grid%xb % vertical_inner_product(its:ite,jts:jte,k) = &
       sqrt(grid%xb%p(its:ite,jts:jte,k) -  grid%xb%p(its:ite,jts:jte,k+1) )
      end do 
   else if (vertical_ip == vertical_ip_delta_p) then

      ! Vertical inner product is Delta p:
      do k=1,grid%xb%mkz
       grid % xb % vertical_inner_product(its:ite,jts:jte,k) = &
       grid%xb%p(its:ite,jts:jte,k) -  grid%xb%p(its:ite,jts:jte,k+1) 
      end do
   end if


   current_date = 'yyyy-mm-dd_hh:mm:ss'

   write(current_date(1:19), fmt='(i4.4, 5(a1, i2.2))') &
      grid%start_year, '-', &
      grid%start_month, '-', &
      grid%start_day, '_', &
      grid%start_hour, ':', &
      grid%start_minute, ':', &
      grid%start_second


   if (print_detail_xb) then
      write(unit=stdout, fmt='(/a, e24.16)') &
         'grid%xb % ds=', grid%xb % ds

      write(unit=stdout, fmt='(a, e24.16/)') &
           'grid%xb % map_factor(its,jts)=', grid%xb % map_factor(its,jts)
   end if
   !---------------------------------------------------------------------------
   ! Calculate saturation vapour pressure and relative humidity:
   !---------------------------------------------------------------------------

   do j=jts,jte
      do k=kts,kte
         do i=its,ite
            call da_tpq_to_rh(grid%xb % t(i,j,k), grid%xb % p(i,j,k), &
               grid%xb % q(i,j,k), grid%xb %es(i,j,k), grid%xb %qs(i,j,k), &
               grid%xb %rh(i,j,k))
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_transfer_wrf_nmm_regional_toxb")

end subroutine da_transfer_wrf_nmm_regional_toxb

