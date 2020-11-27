subroutine da_qc_rad (it, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform quality control for radiance data.
   !
   ! METHOD:  separated QC for each sensor
   !---------------------------------------------------------------------------

   implicit none

   integer      ,  intent(in)      :: it         ! outer loop count
   type (y_type),  intent(in)      :: ob         ! Observation structure.
   type (iv_type), intent(inout)   :: iv         ! O-B structure.

   integer :: i, nchan,p,j
   logical   :: amsua, amsub, hirs, msu,airs, hsb, ssmis, mhs, iasi, seviri
   logical   :: mwts, mwhs, atms, amsr2, imager, ahi, mwhs2

   integer, allocatable :: index(:)
   integer :: num_tovs_avg
   integer, allocatable :: excess_count(:)
   integer, allocatable :: spare_count(:)
   integer :: transfer
   logical :: copy_found
   integer :: temp(num_procs)

   if (trace_use) call da_trace_entry("da_qc_rad")

   if ( .not. allocated(num_tovs_before) )  allocate (num_tovs_before(iv%num_inst,num_procs))
   if ( .not. allocated(num_tovs_after) )   allocate (num_tovs_after(iv%num_inst,num_procs))

   ! Cannot be more total send,receives than combination of processors
   if ( .not. allocated(tovs_copy_count) )  allocate (tovs_copy_count(iv%num_inst))
   if ( .not. allocated(tovs_send_pe) )     allocate (tovs_send_pe(iv%num_inst,num_procs*num_procs))
   if ( .not. allocated(tovs_recv_pe) )     allocate (tovs_recv_pe(iv%num_inst,num_procs*num_procs))
   if ( .not. allocated(tovs_send_start) )  allocate (tovs_send_start(iv%num_inst,num_procs*num_procs))
   if ( .not. allocated(tovs_send_count) )  allocate (tovs_send_count(iv%num_inst,num_procs*num_procs))
   if ( .not. allocated(tovs_recv_start) )  allocate (tovs_recv_start(iv%num_inst,num_procs*num_procs))

   call da_trace("da_qc_rad", message="allocated tovs redistibution arrays")

   if ( .not. allocated(index) )            allocate (index(num_procs))
   if ( .not. allocated(excess_count) )     allocate (excess_count(num_procs))
   if ( .not. allocated(spare_count) )      allocate (spare_count(num_procs))

   do i = 1, iv%num_inst

      !if (iv%instid(i)%info%n2 < iv%instid(i)%info%n1) cycle

      nchan    = iv%instid(i)%nchan

      amsua = trim(rttov_inst_name(rtminit_sensor(i))) == 'amsua'
      amsub = trim(rttov_inst_name(rtminit_sensor(i))) == 'amsub'
      hirs  = trim(rttov_inst_name(rtminit_sensor(i))) == 'hirs'
      msu   = trim(rttov_inst_name(rtminit_sensor(i))) == 'msu'
      airs  = trim(rttov_inst_name(rtminit_sensor(i))) == 'airs'
      hsb   = trim(rttov_inst_name(rtminit_sensor(i))) == 'hsb'
      ssmis = trim(rttov_inst_name(rtminit_sensor(i))) == 'ssmis'
      mhs   = trim(rttov_inst_name(rtminit_sensor(i))) == 'mhs'
      iasi  = trim(rttov_inst_name(rtminit_sensor(i))) == 'iasi'	  
      mwts  = trim(rttov_inst_name(rtminit_sensor(i))) == 'mwts'
      mwhs  = trim(rttov_inst_name(rtminit_sensor(i))) == 'mwhs'
      mwhs2 = trim(rttov_inst_name(rtminit_sensor(i))) == 'mwhs2'
      atms  = trim(rttov_inst_name(rtminit_sensor(i))) == 'atms'
      seviri = trim(rttov_inst_name(rtminit_sensor(i))) == 'seviri'
      amsr2 = trim(rttov_inst_name(rtminit_sensor(i))) == 'amsr2'
      imager = trim(rttov_inst_name(rtminit_sensor(i))) == 'imager'
         ahi = trim(rttov_inst_name(rtminit_sensor(i))) == 'ahi'

      if (hirs) then
         ! 1.0 QC for HIRS
         call da_qc_hirs(it, i,nchan,ob,iv)
      else if (airs) then
         call da_qc_airs(it, i,nchan,ob,iv)
      else if ( hsb ) then
         ! call da_qc_hsb(it, i,nchan,ob,iv)
         call da_warning(__FILE__,__LINE__,(/'QC Not implemented for HSB'/))
      else if (amsua) then
         call da_qc_amsua(it,i,nchan,ob,iv)
      else if ( amsub ) then
         call da_qc_amsub(it,i,nchan,ob,iv)
      else if (msu) then
         ! call da_qc_msu(it, i,nchan, ob,iv)
         call da_warning(__FILE__,__LINE__,(/'QC Not implemented for MSU'/))
      else if (ssmis) then
         call da_qc_ssmis(it, i,nchan,ob,iv)
      else if (mhs) then
         call da_qc_mhs(it,i,nchan,ob,iv)
      else if (iasi) then
         call da_qc_iasi(it,i,nchan,ob,iv)		 
      else if (mwhs) then
         call da_qc_mwhs(it,i,nchan,ob,iv)
      else if (mwhs2) then
         call da_qc_mwhs2(it,i,nchan,ob,iv)      
      else if (mwts) then
         call da_qc_mwts(it,i,nchan,ob,iv)
      else if (atms) then
         call da_qc_atms(it,i,nchan,ob,iv)
      else if (seviri) then
         call da_qc_seviri(it,i,nchan,ob,iv)
      else if (amsr2) then
         call da_qc_amsr2(it,i,nchan,ob,iv)
      else if (ahi) then
         call da_qc_ahi(it,i,nchan,ob,iv)
      else if (imager) then
         call da_qc_goesimg(it,i,nchan,ob,iv)
      else
         write(unit=message(1),fmt='(A,A)') &
            "Unrecognized instrument",trim(rttov_inst_name(rtminit_sensor(i)))
         call da_error(__FILE__,__LINE__,message(1:1))
      end if

      ! Report number of observations to other processors via rootproc

      num_tovs_before(i,:) = 0
      num_tovs_before(i,myproc+1)=iv%instid(i)%num_rad
      temp(:)= num_tovs_before(i,:)
      call da_proc_sum_ints(temp(:))

#ifdef DM_PARALLEL
      call wrf_dm_bcast_integer(temp(:),num_procs)
#endif
      num_tovs_before(i,:) = temp(:)

      num_tovs_after(i,:) = num_tovs_before(i,:)

      if (rootproc .and. print_detail_rad) then
         write(unit=message(1),fmt='(A,I1,A)') "Instrument ",i, &
            " initial tovs distribution"
         write(unit=message(2),fmt=*) num_tovs_before(i,:)
         call da_message(message(1:2))
      end if

      ! Decide how to reallocate observations

      num_tovs_avg=sum(num_tovs_before(i,:))/num_procs

      call da_trace_int_sort(num_tovs_before(i,:),num_procs,index)

      do p=1,num_procs
         excess_count(p)=num_tovs_before(i,index(p))-num_tovs_avg
         spare_count(p)=num_tovs_avg-num_tovs_before(i,index(p))
      end do

      tovs_copy_count(i) = 0
      tovs_send_start(i,:) = 0
      tovs_send_count(i,:) = 0

      do
         copy_found = .false.
         do p=1,num_procs
            if (spare_count(p) > tovs_min_transfer) then
               do j=num_procs,1,-1
                  if (excess_count(j) > tovs_min_transfer) then
                     copy_found = .true.
                     tovs_copy_count(i)=tovs_copy_count(i)+1
                     tovs_send_pe(i,tovs_copy_count(i)) = index(j)-1
                     tovs_recv_pe(i,tovs_copy_count(i)) = index(p)-1
                     transfer=min(spare_count(p),excess_count(j))
                     tovs_send_count(i,tovs_copy_count(i)) = transfer
                     tovs_recv_start(i,tovs_copy_count(i)) = num_tovs_after(i,index(p))+1
                     num_tovs_after(i,index(p))=num_tovs_after(i,index(p))+transfer
                     num_tovs_after(i,index(j))=num_tovs_after(i,index(j))-transfer
                     tovs_send_start(i,tovs_copy_count(i)) = num_tovs_after(i,index(j))+1
                     spare_count(p)=spare_count(p)-transfer
                     excess_count(j)=excess_count(j)-transfer
                     exit
                  end if   
               end do
            end if
         end do
         if (.not. copy_found) exit
      end do   

      if (print_detail_rad) then
         write(unit=message(1),fmt='(A,I1,A)') "Instrument ",i," final tovs distribution"
         write(unit=message(2),fmt=*) num_tovs_after(i,:)
         call da_message(message(1:2))
      end if

      iv % instid(i) % num_rad_glo = sum(num_tovs_after(i,:))
   end do

   deallocate (index)
   deallocate (excess_count)
   deallocate (spare_count)

   if (trace_use) call da_trace_exit("da_qc_rad")

end subroutine da_qc_rad


