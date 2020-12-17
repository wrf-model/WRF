subroutine da_allocate_observations_rain (iv)

   !---------------------------------------------------------------------------
   ! Purpose: Allocate components of rain observation structure.
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(inout) :: iv     ! Observation structure.

   integer :: i

   if (trace_use) call da_trace_entry("da_allocate_observations_rain")

   if (iv%info(rain)%nlocal    > 0) allocate(iv%rain   (1:iv%info(rain)%nlocal))
   if (iv%info(rain)%nlocal > 0) then
      allocate (iv%info(rain)%name(iv%info(rain)%nlocal))     
      allocate (iv%info(rain)%platform(iv%info(rain)%nlocal)) 
      allocate (iv%info(rain)%id(iv%info(rain)%nlocal))       
      allocate (iv%info(rain)%date_char(iv%info(rain)%nlocal))
      allocate (iv%info(rain)%levels(iv%info(rain)%nlocal))   
      allocate (iv%info(rain)%lat(iv%info(rain)%max_lev,iv%info(rain)%nlocal))    
      allocate (iv%info(rain)%lon(iv%info(rain)%max_lev,iv%info(rain)%nlocal))    
      allocate (iv%info(rain)%elv(iv%info(rain)%nlocal))      
      allocate (iv%info(rain)%pstar(iv%info(rain)%nlocal))    

      allocate (iv%info(rain)%slp(iv%info(rain)%nlocal))   
      allocate (iv%info(rain)%pw(iv%info(rain)%nlocal))    

      allocate (iv%info(rain)%x  (kms:kme,iv%info(rain)%nlocal))   
      allocate (iv%info(rain)%y  (kms:kme,iv%info(rain)%nlocal))   
      allocate (iv%info(rain)%i  (kms:kme,iv%info(rain)%nlocal))   
      allocate (iv%info(rain)%j  (kms:kme,iv%info(rain)%nlocal))      
      allocate (iv%info(rain)%dx (kms:kme,iv%info(rain)%nlocal))  
      allocate (iv%info(rain)%dxm(kms:kme,iv%info(rain)%nlocal)) 
      allocate (iv%info(rain)%dy (kms:kme,iv%info(rain)%nlocal))  
      allocate (iv%info(rain)%dym(kms:kme,iv%info(rain)%nlocal)) 
      allocate (iv%info(rain)%k  (iv%info(rain)%max_lev,iv%info(rain)%nlocal))
      allocate (iv%info(rain)%dz (iv%info(rain)%max_lev,iv%info(rain)%nlocal))  
      allocate (iv%info(rain)%dzm(iv%info(rain)%max_lev,iv%info(rain)%nlocal)) 
      allocate (iv%info(rain)%zk (iv%info(rain)%max_lev,iv%info(rain)%nlocal)) 
      allocate (iv%info(rain)%proc_domain(iv%info(rain)%max_lev,iv%info(rain)%nlocal)) 
      allocate (iv%info(rain)%thinned(iv%info(rain)%max_lev,iv%info(rain)%nlocal)) 
      allocate (iv%info(rain)%obs_global_index(iv%info(rain)%nlocal)) 

      iv%info(rain)%proc_domain(:,:)  = .false.
      iv%info(rain)%thinned(:,:)      = .false.
      iv%info(rain)%zk(:,:)           = missing_r
   end if

   if (trace_use) call da_trace_exit("da_allocate_observations_rain")

end subroutine da_allocate_observations_rain


