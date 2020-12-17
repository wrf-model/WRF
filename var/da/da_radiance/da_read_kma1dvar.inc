subroutine da_read_kma1dvar (inst,iv, ob, infile)

   !---------------------------------------------------------------------------
   ! Purpose: read in kma 1dvar innovation output to innovation and obs structure
   !
   ! METHOD: use F90 sequantial data structure to avoid read file twice  
   !          so that da_scan_bufrtovs is not necessary any more.
   !          1. read radiance data in sequential data structure
   !          2. assign sequential data structure to innovation structure
   !                and deallocate sequential data structure
   !---------------------------------------------------------------------------

   implicit none

   ! subroutine argument
   integer           ,  intent (in)    :: inst
   character(20)     ,  intent (in)    :: infile
   type (y_type)     ,  intent (inout) :: ob
   type (iv_type)    ,  intent (inout) :: iv

   ! local variables
   integer          :: iost, n, i, j,inunit

   ! Declare local variables
   logical outside,inside_halo
   character(20)  :: instrument

   ! pixel information
   integer   ::  idate, npass, scanpos
   real      ::  rlat, rlon                         !  lat/lon in degrees   for Anfovs
   real      ::  satzen      !  scan angles for Anfovs
   integer   ::  landsea_mask
   real      ::  srf_height
   integer   ::  nchanl,knchan                      !  number of channels

   real      :: fastem(5)
   real , allocatable :: &         !  bright temperatures
                         otb(:),oerr(:),inov(:), emiss(:), &
                         tb(:), inv(:),err(:)
   integer, allocatable :: kchan(:),qc(:)

   type (datalink_type), pointer    :: head, p, current

   integer                        ::  size, error
   type(info_type)                ::  info
   type(model_loc_type)           ::  loc

   if (trace_use) call da_trace_entry("da_read_kma1dvar")

   !**************************************************************************
   ! Initialize variables

   nchanl = iv%instid(inst)%nchan

   allocate ( kchan(nchanl) )
   allocate ( otb(nchanl) )
   allocate ( oerr(nchanl) )
   allocate ( inov(nchanl) )
   allocate ( emiss(nchanl) )

   allocate ( tb(nchanl) )
   allocate ( inv(nchanl) )
   allocate ( err(nchanl) )
   allocate ( qc(nchanl) )

   ! 0.0  Open unit for kma 1dvar file and read file header
   !--------------------------------------------------------------

   call da_get_unit(inunit)
   open(unit=inunit,file=trim(infile),form='formatted', &
       iostat = iost, status = 'old')
   if (iost /= 0) then
      call da_error(__FILE__,__LINE__,&
         (/"Cannot open file "//infile/))
   end if

   read(unit=inunit,fmt='(A,I10,I12)') instrument,idate,npass

   ! Loop to read pixel and assign information to a sequential structure
   !-------------------------------------------------------------------------

   allocate ( head )
   nullify  ( head % next )
   p => head
   size = 0

   do j=1,npass
      read(inunit,'(2F12.4)')    rlat,rlon
      read(inunit,'(I5,20I5)')   knchan,(kchan(i),i=1,knchan)
      ! landsea_mask: 0:land ; 1:sea (same as RTTOV)
      read(inunit,'(I5,F12.4,I5,F12.4)')  scanpos,satzen,landsea_mask,srf_height
      read(inunit,'(20F12.4)') (oerr(i),i=1,knchan)
      read(inunit,'(20F12.4)') (emiss(i),i=1,knchan)
      read(inunit,'(5F12.4)')  (fastem(i),i=1,5)
      read(inunit,'(20F12.4)') (otb(i),i=1,knchan)
      read(inunit,'(20F12.4)') (inov(i),i=1,knchan)

      ! 1.0 Extract observation location and other required information
      !     judge if data is in the domain, read next record if not
      !-------------------------------------------------------------------

      info%lat  =  rlat
      info%lon  =  rlon
      call da_llxy (info, loc, outside, inside_halo )
      if (outside) cycle

      info%elv = srf_height
      write(unit=info%date_char, fmt='(i10)') idate 

      tb(1:nchanl) = missing_r
      inv(1:nchanl) = missing_r
      err(1:nchanl) = missing_r
      qc(1:nchanl) = qc_bad

      do i=1,knchan
         tb(kchan(i)) = otb(i)
         inv(kchan(i)) = inov(i)
         err(kchan(i)) = oerr(i)
         qc(kchan(i)) = qc_good
      end do 

      !  2.0   assign information to sequential radiance structure
      !--------------------------------------------------------------------

      allocate (p % tb_inv (1:nchanl))
      allocate (p % tb_error (1:nchanl))
      allocate (p % tb_qc (1:nchanl))
      allocate (p % tb_ob (1:nchanl))
      p%info             = info
      p%loc              = loc
      p%landsea_mask     = landsea_mask
      p%scanpos          = scanpos
      p%satzen           = satzen
      p%satazi           = missing_r
      p%solzen           = missing_r
      p%tb_inv(1:nchanl)     = inv(1:nchanl)
      p%tb_error(1:nchanl)   = err(1:nchanl)
      p%tb_qc(1:nchanl)      = qc(1:nchanl)
      p%tb_ob(1:nchanl)      = tb(1:nchanl)
      p%sensor_index         = inst

      size = size + 1
      allocate ( p%next, stat=error)   ! add next data
      if (error /= 0 ) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot allocate radiance structure"/))
      end if

      p => p%next
      nullify (p%next)
   end do
   
   iv%total_rad_pixel   = iv%total_rad_pixel + size
   iv%total_rad_channel = iv%total_rad_channel + size*nchanl

   deallocate (kchan)
   deallocate (otb)
   deallocate (oerr)
   deallocate (inov)
   deallocate (emiss)

   deallocate (tb)
   deallocate (inv)
   deallocate (err)
   deallocate (qc)

   close(inunit)
   call da_free_unit(inunit)

   !  3.0 allocate innovation and obs radiance structure
   !----------------------------------------------------------------
   iv%instid(inst)%num_rad = size
   ob%instid(inst)%num_rad = size
      
   write(unit=stdout,fmt='(a,i3,2x,a,3x,i10)')  ' allocating space for radiance innov structure', &
                             i, iv%instid(inst)%rttovid_string, iv%instid(inst)%num_rad

   !  4.0 assign sequential structure to innovation structure
   !-------------------------------------------------------------
   p => head
   allocate (iv%instid(inst)%tb_inv(1:nchanl,size))
   allocate (iv%instid(inst)%tb_error(1:nchanl,size))
   allocate (iv%instid(inst)%tb_qc(1:nchanl,size))
   allocate (ob%instid(inst)%tb(1:nchanl,size))
   do n = 1, size

      iv%instid(i)%info%name(n)	 = p%info%name
      iv%instid(i)%info%platform(n)  = p%info%platform
      iv%instid(i)%info%id(n) 	 = p%info%id
      iv%instid(i)%info%date_char(n) = p%info%date_char
      iv%instid(i)%info%levels(n)    = p%info%levels
      iv%instid(i)%info%lat(:,n)	 = p%info%lat
      iv%instid(i)%info%lon(:,n)	 = p%info%lon
      iv%instid(i)%info%elv(n)	 = p%info%elv
      iv%instid(i)%info%pstar(n)     = p%info%pstar

      iv%instid(inst)%info%i(:,n)    = p%loc%i
      iv%instid(inst)%info%j(:,n)    = p%loc%j
      iv%instid(inst)%info%dx(:,n)   = p%loc%dx
      iv%instid(inst)%info%dy(:,n)   = p%loc%dy
      iv%instid(inst)%info%dxm(:,n)  = p%loc%dxm
      iv%instid(inst)%info%dym(:,n)  = p%loc%dym

      iv%instid(inst)%landsea_mask(n) = p%landsea_mask
      iv%instid(inst)%scanpos(n)      = p%scanpos
      iv%instid(inst)%satzen(n) = p%satzen
      iv%instid(inst)%satazi(n) = p%satazi
      iv%instid(inst)%solzen(n) = p%solzen
      iv%instid(inst)%tb_inv(1:nchanl,n)   = p%tb_inv(1:nchanl)
      iv%instid(inst)%tb_error(1:nchanl,n) = p%tb_error(1:nchanl)
      iv%instid(inst)%tb_qc(1:nchanl,n)    = p%tb_qc(1:nchanl)
      ob%instid(inst)%tb(1:nchanl,n)       = p%tb_ob(1:nchanl)

      ! write(unit=stdout,*) inst, nread, iv%instid(inst)%tb_inv(1:nchanl,n)
      current => p
      p => p%next

      ! free current data
      deallocate (current % tb_ob)
      deallocate (current % tb_inv)
      deallocate (current % tb_error)
      deallocate (current % tb_qc)
      deallocate (current)
   end do

   ! check if sequential structure has been freed
   !
   ! p => head
   ! do i = 1, size
   !    write (unit=stdout,fmt=*)  i, p%tb(1:nchanl)%inv
   !    p => p%next
   ! end do

   if (trace_use) call da_trace_exit("da_readkma1dvar")

end subroutine da_read_kma1dvar


