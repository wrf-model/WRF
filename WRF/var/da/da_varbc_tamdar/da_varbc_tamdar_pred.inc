   subroutine da_varbc_tamdar_pred(iv, be, ob)

   !--------------------------------------!
   !  Calculate Predictors in Bias Model  !
   !--------------------------------------!
   
   implicit none

   type (iv_type), intent(inout) :: iv
   type (be_type), intent(inout) :: be
   type (y_type),  intent(inout) :: ob

   integer                       :: jt_start,size_jt
   integer                       :: n,nphase,iflt,iobs,ipred,isn,iqc,iphase
   real                          :: dhdt,dtdt,p1,p2,p3,p4

   integer, allocatable          :: tdiff(:),osn(:)
   real   , allocatable          :: op(:),ot(:),oh(:),mach(:)

   character(len=24)             :: otime,stime
   character(len=40)             :: stringn


   if (trace_use) call da_trace_entry("da_varbc_tamdar_pred")

   if (rootproc) write(unit=varbc_tamdar_unit,fmt='(//5X,A/)') &
                      'Calculating predictors'

   size_jt  = 0
   jt_start = be%cv%size_jb + be%cv%size_je + be%cv%size_jp + be%cv%size_js + be%cv%size_jl

   nphase = iv%varbc_tamdar%nphase

   allocate( op    (iv%varbc_tamdar%nmaxobs) )
   allocate( ot    (iv%varbc_tamdar%nmaxobs) )
   allocate( oh    (iv%varbc_tamdar%nmaxobs) )
   allocate( osn   (iv%varbc_tamdar%nmaxobs) )
   allocate( mach  (iv%varbc_tamdar%nmaxobs) )
   allocate( tdiff (iv%varbc_tamdar%nmaxobs) )

   do iflt = 1, iv%varbc_tamdar%nair
      if (iv%varbc_tamdar%nobs_sum(nphase+1,iflt) >= varbc_tamdar_nobsmin) then

          n = 0
          op(:) = 0.
          ot(:) = 0.
          oh(:) = 0.
          osn(:) = 0
          mach(:) = 0.
          tdiff(:) = 0

          if (iv%varbc_tamdar%nobs(nphase+1,iflt) > 1) then
              do iobs = 1, iv%varbc_tamdar%nobs(nphase+1,iflt) 

                 isn = iv%varbc_tamdar%obs_sn(iobs,nphase+1,iflt)
                 iqc = iv%tamdar(isn)%t(1)%qc 

                 if (iv%tamdar(isn)%p(1) > missing_r .and. iqc >= 0) then
                     n = n + 1
                     osn(n) = isn
                     op(n) = iv%tamdar(isn)%p(1)
                     ot(n) = ob%tamdar(isn)%t(1)
                     oh(n) = (1-(op(n)/101325.0)**0.190284)*145366.45*0.3048    ! pressure altitude

                     if (n == 1) stime = iv%info(tamdar)%date_char(isn)
                     otime = iv%info(tamdar)%date_char(isn)
                     tdiff(n) = int(da_diff_seconds(otime, stime))

		   ! May assign Mach number on the position of 'pw' in ob.ascii
                     if (op(n) >= 95000.0) mach(n) = 0.208
                     if (op(n) < 95000.0 .and. op(n) >= 90000.0) mach(n) = 0.265
                     if (op(n) < 90000.0 .and. op(n) >= 85000.0) mach(n) = 0.288
                     if (op(n) < 85000.0 .and. op(n) >= 80000.0) mach(n) = 0.319
                     if (op(n) < 80000.0 .and. op(n) >= 75000.0) mach(n) = 0.351
                     if (op(n) < 75000.0 .and. op(n) >= 70000.0) mach(n) = 0.387
                     if (op(n) < 70000.0 .and. op(n) >= 65000.0) mach(n) = 0.419
                     if (op(n) < 65000.0 .and. op(n) >= 60000.0) mach(n) = 0.446
                     if (op(n) < 60000.0 .and. op(n) >= 55000.0) mach(n) = 0.474
                     if (op(n) < 55000.0 .and. op(n) >= 50000.0) mach(n) = 0.492
                     if (op(n) < 50000.0 .and. op(n) >= 45000.0) mach(n) = 0.505
                     if (op(n) < 45000.0 .and. op(n) >= 40000.0) mach(n) = 0.529
                     if (op(n) < 40000.0 .and. op(n) >= 30000.0) mach(n) = 0.552
                     if (op(n) < 30000.0) mach(n) = 0.580
                 end if
              end do
          end if
             
          if (n > 1) then
              do iobs = 2, n

                 if (tdiff(iobs) == tdiff(iobs-1) .or. abs(tdiff(iobs) - tdiff(iobs-1)) > 360) cycle

                 dhdt = (oh(iobs)-oh(iobs-1))/10.0/(tdiff(iobs)-tdiff(iobs-1))
                 dtdt = -1.0*(ot(iobs)-ot(iobs-1))/(tdiff(iobs)-tdiff(iobs-1))

		 p1 = (dhdt + 2.0) / 5.0
		 p2 = 1.0 / (10.0 * mach(iobs))
		 p3 = dtdt * 5.0
		 p4 = ot(iobs) / 2000.0

                 if (dhdt < -0.2) then

                     iv%varbc_tamdar%nobs(1,iflt) = iv%varbc_tamdar%nobs(1,iflt) + 1
                     iv%varbc_tamdar%obs_sn(iv%varbc_tamdar%nobs(1,iflt),1,iflt) = osn(iobs)

                     if (varbc_tamdar_bm == 1) then
                         iv%varbc_tamdar%pred(2,1,iflt) = iv%varbc_tamdar%pred(2,1,iflt) + dhdt
		     elseif (varbc_tamdar_bm == 2) then
                         iv%varbc_tamdar%pred(2,1,iflt) = iv%varbc_tamdar%pred(2,1,iflt) + p1
                         iv%varbc_tamdar%pred(3,1,iflt) = iv%varbc_tamdar%pred(3,1,iflt) + p2
                         iv%varbc_tamdar%pred(4,1,iflt) = iv%varbc_tamdar%pred(4,1,iflt) + p3
                         iv%varbc_tamdar%pred(5,1,iflt) = iv%varbc_tamdar%pred(5,1,iflt) + p4
                     end if

                 elseif (dhdt > 0.3) then
 
                     iv%varbc_tamdar%nobs(2,iflt) = iv%varbc_tamdar%nobs(2,iflt) + 1
                     iv%varbc_tamdar%obs_sn(iv%varbc_tamdar%nobs(2,iflt),2,iflt) = osn(iobs)

                     if (varbc_tamdar_bm == 1) then
                         iv%varbc_tamdar%pred(2,2,iflt) = iv%varbc_tamdar%pred(2,2,iflt) + dhdt
		     elseif (varbc_tamdar_bm == 2) then
                         iv%varbc_tamdar%pred(2,2,iflt) = iv%varbc_tamdar%pred(2,2,iflt) + p1
                         iv%varbc_tamdar%pred(3,2,iflt) = iv%varbc_tamdar%pred(3,2,iflt) + p2
                         iv%varbc_tamdar%pred(4,2,iflt) = iv%varbc_tamdar%pred(4,2,iflt) + p3
                         iv%varbc_tamdar%pred(5,2,iflt) = iv%varbc_tamdar%pred(5,2,iflt) + p4
                     end if

                 else

                     iv%varbc_tamdar%nobs(3,iflt) = iv%varbc_tamdar%nobs(3,iflt) + 1
                     iv%varbc_tamdar%obs_sn(iv%varbc_tamdar%nobs(3,iflt),3,iflt) = osn(iobs)

                     if (varbc_tamdar_bm == 1) then
                         iv%varbc_tamdar%pred(2,3,iflt) = iv%varbc_tamdar%pred(2,3,iflt) + dhdt
		     elseif (varbc_tamdar_bm == 2) then
                         iv%varbc_tamdar%pred(2,3,iflt) = iv%varbc_tamdar%pred(2,3,iflt) + p1
                         iv%varbc_tamdar%pred(3,3,iflt) = iv%varbc_tamdar%pred(3,3,iflt) + p2
                         iv%varbc_tamdar%pred(4,3,iflt) = iv%varbc_tamdar%pred(4,3,iflt) + p3
                         iv%varbc_tamdar%pred(5,3,iflt) = iv%varbc_tamdar%pred(5,3,iflt) + p4
                     end if

                 end if

              end do
          end if

          do iphase = 1, nphase

             iv%varbc_tamdar%nobs_sum(iphase,iflt) = wrf_dm_sum_integer(iv%varbc_tamdar%nobs(iphase,iflt))

             if (iv%varbc_tamdar%nobs_sum(iphase,iflt) >= varbc_tamdar_nobsmin) then

                 do ipred = 1, iv%varbc_tamdar%npred
                    iv%varbc_tamdar%pred(ipred,iphase,iflt) = &
                       wrf_dm_sum_real(iv%varbc_tamdar%pred(ipred,iphase,iflt)) / &
                       iv%varbc_tamdar%nobs_sum(iphase,iflt) 
                    if (ipred == 1) iv%varbc_tamdar%pred(ipred,iphase,iflt) = varbc_tamdar_pred0
                 end do

                 if (iv%varbc_tamdar%nobs(iphase,iflt) > 0 .and. iv%varbc_tamdar%ifuse(iphase,iflt) > 0) then
                     do ipred = 1, iv%varbc_tamdar%npred 
                        size_jt = size_jt + 1
                        iv%varbc_tamdar%index(ipred,iphase,iflt) = jt_start + size_jt
                        iv%varbc_tamdar%vtox(ipred,ipred,iphase,iflt) = 1.0
                     end do
                 end if

              !   if (iv%varbc_tamdar%nobs(iphase,iflt) > 0 .and. iv%varbc_tamdar%ifuse(iphase,iflt) <= 0) then
              !       do iobs = 1, iv%varbc_tamdar%nobs(iphase,iflt)
              !          isn = iv%varbc_tamdar%obs_sn(iobs,iphase,iflt)
              !          if (iv%tamdar(isn)%t(1)%qc >= 0) iv%tamdar(isn)%t(1)%qc = fail_varbc_aircraft
              !       end do
              !   end if

             else

                 iv%varbc_tamdar%ifuse(iphase,iflt) = 0

                 if (iv%varbc_tamdar%nobs(iphase,iflt) > 0) then
                     do iobs = 1, iv%varbc_tamdar%nobs(iphase,iflt)
                        isn = iv%varbc_tamdar%obs_sn(iobs,iphase,iflt)
                        if (iv%tamdar(isn)%t(1)%qc >= 0) iv%tamdar(isn)%t(1)%qc = fail_varbc_aircraft 
                     end do
                 end if

             end if
          end do
  
      else

          iv%varbc_tamdar%ifuse(:,iflt) = 0

          if (iv%varbc_tamdar%nobs(nphase+1,iflt) > 0) then
              do iobs = 1, iv%varbc_tamdar%nobs(nphase+1,iflt)
                 isn = iv%varbc_tamdar%obs_sn(iobs,nphase+1,iflt)
                 if (iv%tamdar(isn)%t(1)%qc >= 0) iv%tamdar(isn)%t(1)%qc = fail_varbc_aircraft
              end do
	  end if

          if (rootproc) &
              write(unit=varbc_tamdar_unit,fmt='(10X,A,I4,A,I4)') &
                   'Measurement from ', iv%varbc_tamdar%tail_id(iflt),' is insufficient: ', &
                    iv%varbc_tamdar%nobs_sum(nphase+1,iflt)

      end if
   end do 

   be % cv % size_jt = size_jt

   if (rootproc .and. ANY(iv%varbc_tamdar%nobs_sum(nphase+1,:) >= varbc_tamdar_nobsmin)) &
       write(unit=varbc_tamdar_unit,fmt='(/10X,A)') " ID    Predictors(npred,nphase)  &  Obs Count(nphase)"

   write(stringn,'(I3)') iv%varbc_tamdar%npred
   stringn = '(10X,I4,3('//trim(ADJUSTL(stringn))//'f8.3),3I5)'
   stringn = trim(adjustl(stringn))

   do iflt = 1, iv%varbc_tamdar%nair
      if (rootproc .and. iv%varbc_tamdar%nobs_sum(nphase+1,iflt) >= varbc_tamdar_nobsmin) then
          write(unit=varbc_tamdar_unit,fmt=stringn) &
                iv%varbc_tamdar%tail_id(iflt), &
                iv%varbc_tamdar%pred(1:iv%varbc_tamdar%npred,1:nphase,iflt), &
                iv%varbc_tamdar%nobs_sum(1:nphase,iflt)
      end if
   end do

   deallocate(op,ot,osn,mach,tdiff)

   if (trace_use) call da_trace_exit("da_varbc_tamdar_pred")

   end subroutine da_varbc_tamdar_pred
