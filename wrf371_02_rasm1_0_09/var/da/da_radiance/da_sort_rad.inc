subroutine da_sort_rad (iv)

   !---------------------------------------------------------------------------
   ! Purpose: sorting radiance innovation to FGAT time bins
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type), intent (inout) :: iv

   integer                           :: i,j, n,t, error
   integer, allocatable              :: ifgat(:),landsea_mask(:)
   integer, allocatable              :: loc_i(:,:), loc_j(:,:),loc_k(:,:)
   real, allocatable                 :: loc_dx(:,:),loc_dy(:,:),loc_dz(:,:)
   real, allocatable                 :: loc_dxm(:,:),loc_dym(:,:),loc_dzm(:,:)
   character (len = 40), allocatable :: name(:)       
   character (len = 12), allocatable :: platform(:)   
   character (len =  5), allocatable :: id(:)          
   character (len = 19), allocatable :: date_char(:)   
   integer, allocatable              :: levels(:)      
   real, allocatable                 :: lat(:,:)         
   real, allocatable                 :: lon(:,:)         
   real, allocatable                 :: elv(:)        
   real, allocatable                 :: pstar(:)      
   real, allocatable                 :: scanpos(:), satzen(:), satazi(:), solzen(:)
   real, allocatable                 :: tb_inv(:,:)

   if (trace_use) call da_trace_entry("da_sort_rad")

   iv%info(radiance)%plocal(:) = 0
   if (num_fgat_time == 1) then
      do i=1,rtminit_nsensor
         iv%instid(i)%info%plocal(:) = 0
         iv%instid(i)%info%plocal(num_fgat_time) = iv%instid(i)%num_rad
         iv%info(radiance)%plocal(num_fgat_time) = iv%info(radiance)%plocal(num_fgat_time) + iv%instid(i)%num_rad
      end do
      if (trace_use) call da_trace_exit("da_sort_rad")
      return
   end if

   do i=1,rtminit_nsensor
      iv%instid(i)%info%plocal(:) = 0
      if (iv%instid(i)%num_rad < 1) cycle

      allocate (ifgat       (iv%instid(i)%num_rad)) 
      allocate (landsea_mask(iv%instid(i)%num_rad))
      allocate (loc_i       (kts:kte, iv%instid(i)%num_rad))
      allocate (loc_j       (kts:kte, iv%instid(i)%num_rad))
      allocate (loc_k       (iv%instid(i)%nlevels,iv%instid(i)%num_rad))
      allocate (loc_dx      (kts:kte, iv%instid(i)%num_rad))
      allocate (loc_dy      (kts:kte, iv%instid(i)%num_rad))
      allocate (loc_dz      (iv%instid(i)%nlevels,iv%instid(i)%num_rad))
      allocate (loc_dxm     (kts:kte, iv%instid(i)%num_rad))
      allocate (loc_dym     (kts:kte, iv%instid(i)%num_rad))
      allocate (loc_dzm     (iv%instid(i)%nlevels,iv%instid(i)%num_rad))
      allocate (name        (iv%instid(i)%num_rad))
      allocate (platform    (iv%instid(i)%num_rad))
      allocate (id          (iv%instid(i)%num_rad))
      allocate (date_char   (iv%instid(i)%num_rad))
      allocate (levels      (iv%instid(i)%num_rad))
      allocate (lat         (kts:kte,iv%instid(i)%num_rad))
      allocate (lon         (kts:kte,iv%instid(i)%num_rad))
      allocate (elv         (iv%instid(i)%num_rad))
      allocate (pstar       (iv%instid(i)%num_rad))
      allocate (scanpos     (iv%instid(i)%num_rad))
      allocate (satzen      (iv%instid(i)%num_rad))
      allocate (satazi      (iv%instid(i)%num_rad))
      allocate (solzen      (iv%instid(i)%num_rad))
      allocate (tb_inv      (iv%instid(i)%nchan,iv%instid(i)%num_rad))

      j = 0
      do t = 1,num_fgat_time
         do n = 1,iv%instid(i)%num_rad
            if (iv%instid(i)%ifgat(n) /= t) cycle
            j = j + 1
            ifgat(j)        = iv%instid(i)%ifgat(n)
            name(j)         = iv%instid(i)%info%name(n)
            platform(j)     = iv%instid(i)%info%platform(n)
            id(j)           = iv%instid(i)%info%id(n)
            date_char(j)    = iv%instid(i)%info%date_char(n)
            levels(j)       = iv%instid(i)%info%levels(n)
            elv(j)          = iv%instid(i)%info%elv(n)
            pstar(j)        = iv%instid(i)%info%pstar(n)

            lat    (:,j) = iv%instid(i)%info%lat(:,n)
            lon    (:,j) = iv%instid(i)%info%lon(:,n)
            loc_i  (:,j) = iv%instid(i)%info%i(:,n)
            loc_j  (:,j) = iv%instid(i)%info%j(:,n)
            loc_k  (:,j) = iv%instid(i)%info%k(:,n)
            loc_dx (:,j) = iv%instid(i)%info%dx(:,n)
            loc_dy (:,j) = iv%instid(i)%info%dy(:,n)
            loc_dxm(:,j) = iv%instid(i)%info%dxm(:,n)
            loc_dym(:,j) = iv%instid(i)%info%dym(:,n)
            loc_dz (:,j) = iv%instid(i)%info%dz(:,n)
            loc_dzm(:,j) = iv%instid(i)%info%dzm(:,n)

            landsea_mask(j) = iv%instid(i)%landsea_mask(n)
            scanpos(j)      = iv%instid(i)%scanpos(n)
            satzen(j)       = iv%instid(i)%satzen(n)
            satazi(j)       = iv%instid(i)%satazi(n)
            solzen(j)       = iv%instid(i)%solzen(n)

            tb_inv(1:iv%instid(i)%nchan,j) = iv%instid(i)%tb_inv(1:iv%instid(i)%nchan,n) 
         end do
         iv%instid(i)%info%plocal(t) = j
         !write (0,*) __FILE__,__LINE__,"i,t,iv%instid(i)%info%plocal(t)",i,t,iv%instid(i)%info%plocal(t)
      end do

      iv%info(radiance)%plocal = iv%info(radiance)%plocal + iv%instid(i)%info%plocal

      write(unit=stdout,fmt='(a,2x,a,2x,240i7)') &
         ' FGAT: ',iv%instid(i)%rttovid_string, iv%instid(i)%info%plocal(1:num_fgat_time)

      do n = 1,iv%instid(i)%num_rad
         iv%instid(i)%ifgat(n)        = ifgat(n)

         iv%instid(i)%info%name(n)	    = name(n)
         iv%instid(i)%info%platform(n)  = platform(n)
         iv%instid(i)%info%id(n) 	    = id(n)
         iv%instid(i)%info%date_char(n) = date_char(n)
         iv%instid(i)%info%levels(n)    = levels(n)
         iv%instid(i)%info%lat(:,n)     = lat(:,n)
         iv%instid(i)%info%lon(:,n)     = lon(:,n)
         iv%instid(i)%info%elv(n)	    = elv(n)
         iv%instid(i)%info%pstar(n)     = pstar(n)

         iv%instid(i)%info%i(:,n)    = loc_i(:,n)
         iv%instid(i)%info%j(:,n)    = loc_j(:,n)
         iv%instid(i)%info%k(:,n)    = loc_k(:,n)
         iv%instid(i)%info%dx(:,n)   = loc_dx(:,n)
         iv%instid(i)%info%dy(:,n)   = loc_dy(:,n)
         iv%instid(i)%info%dz(:,n)   = loc_dz(:,n)
         iv%instid(i)%info%dxm(:,n)  = loc_dxm(:,n)
         iv%instid(i)%info%dym(:,n)  = loc_dym(:,n)
         iv%instid(i)%info%dzm(:,n)  = loc_dzm(:,n)
         iv%instid(i)%landsea_mask(n) = landsea_mask(n)
         iv%instid(i)%scanpos(n)      = scanpos(n)
         iv%instid(i)%satzen(n)       = satzen(n)
         iv%instid(i)%satazi(n)       = satazi(n)
         iv%instid(i)%solzen(n)       = solzen(n)

         iv%instid(i)%tb_inv(1:iv%instid(i)%nchan,n) = tb_inv(1:iv%instid(i)%nchan,n)
      end do

      deallocate (ifgat) 
      deallocate (landsea_mask)
      deallocate (name)
      deallocate (platform)
      deallocate (id)
      deallocate (date_char)
      deallocate (levels)
      deallocate (lat)
      deallocate (lon)
      deallocate (elv)
      deallocate (pstar)
      deallocate (loc_i)
      deallocate (loc_j)
      deallocate (loc_k)
      deallocate (loc_dx)
      deallocate (loc_dy)
      deallocate (loc_dz)
      deallocate (loc_dxm)
      deallocate (loc_dym)
      deallocate (loc_dzm)
      deallocate (scanpos)
      deallocate (satzen)
      deallocate (satazi)
      deallocate (solzen)
      deallocate (tb_inv)
   end do

   if (trace_use) call da_trace_exit("da_sort_rad")

end subroutine da_sort_rad


