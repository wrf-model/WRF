subroutine da_read_obs_lightning (iv, filename, grid)

   !-----------------------------------------------------------------------
   ! Purpose: Read the lightning observation file
   ! Authors: Z Chen (zchen@fjnu.edu.cn), Jenny Sun (NCAR), X Qie (IAP)    
   !----------------------------------------------------------------------------------------!

   implicit none

   type (iv_type),    intent(inout) :: iv
   character(len=*),  intent(in)    :: filename
   type(domain),     intent(in)     :: grid     ! first guess state.
   
   integer                       :: i, n, iost
   integer                       :: iunit  
   
   integer                          :: i1, j1, k  ! Index dimension.
   real                             :: dx, dxm  ! Interpolation weights
   real                             :: dy, dym  ! Interpolation weights
   real                             :: zlcl
   
   type (lightning_multi_level_type) :: platform
   
   character (len = 120)         :: char_total_lightning, char_total_levels
   character (len = 160)         :: info_string
   integer                       :: total_lightning, nlevels, lightning_qc, rh_indicator
   real                          :: flashrate, wmax, lightning_error
   real, allocatable,dimension(:):: height, coff
   logical                       :: outside, outside_all
   integer                       :: nlocal

   if (trace_use) call da_trace_entry("da_read_obs_lightning")

   nlocal = 0

   ! 1. open file
   call da_get_unit(iunit)
   open(unit   = iunit,     &
        FILE   = trim(filename), &
        FORM   = 'FORMATTED',  &
        ACCESS = 'SEQUENTIAL', &
        iostat =  iost,     &
        STATUS = 'OLD')

   if (iost /= 0) then
      write(unit=message(1),fmt='(A,I5,A)') &
         "Error",iost," opening lightning obs file "//trim(filename)
      call da_warning(__FILE__,__LINE__,message(1:1))
      call da_free_unit(iunit)
      if (trace_use) call da_trace_exit("da_read_obs_lightning")
      return
   end if

   ! 2. read basic info 

   ! 2.1 read the number of total lightning observation and vertical layers 
   read (unit=iunit, fmt = '(A)', iostat = iost) char_total_lightning
   read (unit=iunit, fmt = '(A)', iostat = iost) char_total_levels
   read (unit=char_total_levels(9:15),fmt='(I7)', iostat = iost) nlevels
   
   ! skip one line
   read (unit=iunit, fmt = '(A)', iostat = iost)
   
   ! 2.2 read height and coefficient
   allocate(height(nlevels))
   allocate(coff(nlevels))
   do i = 1, nlevels
      read (unit = iunit, iostat = iost, fmt = '(2F12.3)') height(i), coff(i)
   end do
   
   ! 2.3 read header info
   head_info: do
      read (unit=iunit, fmt = '(A)', iostat = iost) info_string
      if (iost /= 0) then
         write(unit=message(1),fmt='(A,I3,A,I3)') &
            "Error",iost,"reading lightning obs header on unit",iunit
         call da_warning(__FILE__,__LINE__,message(1:1))
      if (trace_use) call da_trace_exit("da_scan_obs_lightning")
      return
      end if
      if (info_string(1:6) == 'data  ') exit
   end do head_info

   ! 2.4 read total lightning data info
   read (unit=char_total_lightning (8:14),fmt='(I7)', iostat = iost) total_lightning

   ! 2.5 skip one line
   read (unit=iunit, fmt = '(A)', iostat = iost)

   ! 3. read lightning data
   reports:   do n = 1, total_lightning
      ! 3.1 read station general info
      read (unit = iunit, iostat = iost, &
                   fmt = '(A12,1X,A19,1X,I6,2(F12.3,2X),F8.1,1X,A5)') &
                   platform%info%platform,  &
                   platform%info%date_char, &
                   platform%info%levels,    &
                   platform%info%lat,       &
                   platform%info%lon,       &
                   platform%info%elv,       &
                   platform%info%id 

     call da_llxy (platform%info, platform%loc, outside, outside_all)

! Height information is from xb, get horizontal interpolation weights:
      i1   = platform%loc%i
      j1   = platform%loc%j	  
      dx   = platform%loc%dx
      dy   = platform%loc%dy
      dxm  = platform%loc%dxm
      dym  = platform%loc%dym

      ! 3.2 read lightning flash rate and its qc and error info 
      read (unit = iunit, fmt = '(F12.3,I4,F12.3,I4)') flashrate, lightning_qc, lightning_error, rh_indicator
      
      !turn lighting flash rate into the maximum wmax
      if(flashrate .ge. min_flashrate .and. flashrate .le. min_flashrate+10.0)then
         wmax = 5!14.6
      end if
      if(flashrate .gt.min_flashrate+10.0 .and. flashrate .le. min_flashrate+20.0)then
         wmax = 8!17.07
      end if
      if(flashrate .gt.min_flashrate+20.0 .and. flashrate.le.min_flashrate+30.0)then
         wmax = 12!18.67
      end if	  
      if(flashrate .gt.min_flashrate+30.0)then
         wmax = 15!24.4 !m/s
      end if

      zlcl = 125.0*(grid%xb%t(i1,j1,1)-grid%xb%td(i1,j1,1)) + grid%xb%terr(i1,j1)
      zlcl = amax1(grid%xb%terr(i1,j1)+1000.,zlcl)
      zlcl = amin1(3000.     ,zlcl)
	  
      do i = 1, nlevels !vertical layers
         platform%each(i) = lightning_each_level_type(missing_r, missing, -1.0,       &
               field_type(missing_r, missing, missing_r, missing, missing_r), & ! w
               field_type(missing_r, missing, missing_r, missing, missing_r), & ! div
               field_type(missing_r, missing, missing_r, missing, missing_r))   ! qv
               
	platform%each(i)%height = grid%xb%h(i1,j1,k)    !height(i)
			   		   
        if(flashrate .ge. min_flashrate .and. i .gt. 1) then 
           ! vertical velocity
           platform%each(i)%w%inv = wmax*coff(i)   
           platform%each(i)%w%qc = 0
           platform%each(i)%w%error = amax1(1.0, 0.20*abs(platform%each(i)%w%inv))
           ! divergence
           platform%each(i)%div%inv = -wmax*(coff(i)-coff(i-1))/(height(i)-height(i-1))
           platform%each(i)%div%qc = 0          
           platform%each(i)%div%error = amax1(0.0001, 0.20*abs(platform%each(i)%div%inv))
        else
           platform%each(i)%w%qc = missing_data
           platform%each(i)%w%error = missing_r		  
           platform%each(i)%div%qc = missing_data
           platform%each(i)%div%error = missing_r
        end if	
        

        if(flashrate .ge. 10.0 .and. rh_indicator .gt. -1 .and. height(i) .ge. zlcl .and. height(i) .le.15000)then
           platform%each(i)%qv%inv = 0.01*(2*rh_indicator+lightning_min_rh)*grid%xb%qs(i1,j1,i)        
           platform%each(i)%qv%qc = 0          
           platform%each(i)%qv%error = amax1(0.001,0.20*platform%each(i)%qv%inv)
        else
           platform%each(i)%qv%qc = missing_data
	   platform%each(i)%qv%error = missing_r
        end if	
      end do  !vertical layers

      if(outside)then
        cycle
      end if
      nlocal = nlocal+1
	 
      iv%info(lightning)%levels(nlocal)    = nlevels
      iv%info(lightning)%name(nlocal)      = platform%info%name
      iv%info(lightning)%platform(nlocal)  = platform%info%platform
      iv%info(lightning)%id(nlocal)        = platform%info%id
      iv%info(lightning)%date_char(nlocal) = platform%info%date_char
      iv%info(lightning)%lat(:,nlocal)     = platform%info%lat
      iv%info(lightning)%lon(:,nlocal)     = platform%info%lon
      iv%info(lightning)%elv(nlocal)       = platform%info%elv
      iv%info(lightning)%pstar(nlocal)     = platform%info%pstar

      iv%info(lightning)%slp(nlocal)       = platform%loc%slp
      iv%info(lightning)%pw(nlocal)        = platform%loc%pw
      iv%info(lightning)%x(:,nlocal)       = platform%loc%x
      iv%info(lightning)%y(:,nlocal)       = platform%loc%y 
      iv%info(lightning)%i(:,nlocal)       = platform%loc%i 
      iv%info(lightning)%j(:,nlocal)       = platform%loc%j 
      iv%info(lightning)%dx(:,nlocal)      = platform%loc%dx
      iv%info(lightning)%dxm(:,nlocal)     = platform%loc%dxm
      iv%info(lightning)%dy(:,nlocal)      = platform%loc%dy
      iv%info(lightning)%dym(:,nlocal)     = platform%loc%dym
      iv%info(lightning)%proc_domain(:,nlocal) = platform%loc%proc_domain
      iv%info(lightning)%obs_global_index(nlocal) = nlocal

      allocate(iv%lightning(nlocal)%height(1:nlevels))
      allocate(iv%lightning(nlocal)%height_qc(1:nlevels))
      allocate(iv%lightning(nlocal)%w(1:nlevels))
      allocate(iv%lightning(nlocal)%qv(1:nlevels))          
      allocate(iv%lightning(nlocal)%div(1:nlevels))            

      do i = 1, nlevels
        iv%lightning(nlocal)%height(i)   = platform%each(i)%height
        iv%lightning(nlocal)%height_qc(i)= platform%each(i)%height_qc
	iv%lightning(nlocal)%w(i)        = platform%each(i)%w
        iv%lightning(nlocal)%qv(i)       = platform%each(i)%qv
        iv%lightning(nlocal)%div(i)      = platform%each(i)%div
      end do
	   
   end do reports
   deallocate(height)
   deallocate(coff)
   close(iunit)
   call da_free_unit(iunit)

   if (trace_use) call da_trace_exit("da_read_obs_lightning")


end subroutine da_read_obs_lightning
