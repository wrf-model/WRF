subroutine da_write_oa_rad_ascii (it, ob, iv, re )

   !---------------------------------------------------------------------------
   ! Purpose: write out OMB and OMA vector structure for radiance data.
   !---------------------------------------------------------------------------

   implicit none

   integer      ,     intent(in)  :: it       ! outer loop count
   type (y_type),     intent(in)  :: ob       ! Observation structure.
   type (iv_type),    intent(in)  :: iv       ! O-B structure.
   type (y_type),     intent(in)  :: re       ! O-A structure.

   integer                        :: n        ! Loop counter.
   integer                        :: i, k     ! Index dimension.
   integer                        :: nlevelss ! Number of obs levels.

   integer            :: ios, oma_rad_unit
   character(len=filename_len)  :: filename
   character(len=7)   :: surftype
   integer            :: ndomain
   logical            :: amsr2

   if (trace_use) call da_trace_entry("da_write_oa_rad_ascii")

   write(unit=message(1),fmt='(A)') 'Writing radiance OMA ascii file'
   call da_message(message(1:1))

   do i = 1, iv%num_inst
      if (iv%instid(i)%num_rad < 1) cycle

      ! count number of obs within the proc_domain
      !---------------------------------------------
      ndomain = 0
      do n =1,iv%instid(i)%num_rad
         if (iv%instid(i)%info%proc_domain(1,n)) then
            ndomain = ndomain + 1
         end if
      end do
      if (ndomain < 1) cycle

      amsr2 = index(iv%instid(i)%rttovid_string,'amsr2') > 0

      write(unit=filename, fmt='(i2.2,a,i4.4)') it,'_oma_'//trim(iv%instid(i)%rttovid_string)//'.', myproc

      call da_get_unit(oma_rad_unit)
      open(unit=oma_rad_unit,file=trim(filename),form='formatted',iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open oma radiance file"//filename/))
      end if
      write(unit=oma_rad_unit,fmt='(a,a,i7,a,i5,a)') trim(iv%instid(i)%rttovid_string), &
                           ' number-of-pixels : ', ndomain, &
                           ' channel-number-of-each-pixel : ', iv%instid(i)%nchan, &
                           ' index-of-channels : '
      write(unit=oma_rad_unit,fmt='(10i5)') iv%instid(i)%ichan
      if ( amsr2 ) then
         write(unit=oma_rad_unit,fmt='(a)') ' pixel-info : i date scanpos landsea_mask  elv lat lon  satzen satazi clw'
      else
         write(unit=oma_rad_unit,fmt='(a)') ' pixel-info : i date scanpos landsea_mask  elv lat lon  satzen satazi '
      end if
      write(unit=oma_rad_unit,fmt='(a)') ' xb-surf-info : i t2m mr2m(ppmv) u10 v10 ps ts smois tslb snowh isflg &
                       & soiltyp vegtyp vegfra elev clwp'
      ndomain = 0
      do n=1,iv%instid(i)%num_rad
         if (iv%instid(i)%info%proc_domain(1,n)) then
            ndomain=ndomain+1
            if ( amsr2 ) then !write out clw
               write(unit=oma_rad_unit,fmt='(a,i7,2x,a,i6,i3,f6.0,4f8.2,f8.3)') 'INFO : ', ndomain, &
                                   iv%instid(i)%info%date_char(n), &
                                   iv%instid(i)%scanpos(n),        &
                                   iv%instid(i)%landsea_mask(n),   &
                                   iv%instid(i)%info%elv(n),       &
                                   iv%instid(i)%info%lat(1,n),     &
                                   iv%instid(i)%info%lon(1,n),     &
                                   iv%instid(i)%satzen(n),         &
                                   iv%instid(i)%satazi(n),         &
                                   iv%instid(i)%clw(n)
            else !no clw info
               write(unit=oma_rad_unit,fmt='(a,i7,2x,a,i6,i3,f6.0,4f8.2)') 'INFO : ', ndomain, &
                                   iv%instid(i)%info%date_char(n), &
                                   iv%instid(i)%scanpos(n),        &
                                   iv%instid(i)%landsea_mask(n),   &
                                   iv%instid(i)%info%elv(n),       &
                                   iv%instid(i)%info%lat(1,n),     &
                                   iv%instid(i)%info%lon(1,n),     &
                                   iv%instid(i)%satzen(n),         &
                                   iv%instid(i)%satazi(n)
            end if
            select case (iv%instid(i)%isflg(n))
            case (0) ;
               surftype = ' SEA : '
            case (1) ;
               surftype = ' ICE : '
            case (2) ;
               surftype = 'LAND : '
            case (3) ;
               surftype = 'SNOW : '
            case (4) ;
               surftype = 'MSEA : '
            case (5) ;
               surftype = 'MICE : '
            case (6) ;
               surftype = 'MLND : '
            case (7) ;
               surftype = 'MSNO : '
            end select
            write(unit=oma_rad_unit,fmt='(a,i7,9f10.2,3i3,f8.3,f10.2,f8.3)') surftype, n, &
                             iv%instid(i)%t2m(n), &
                             iv%instid(i)%mr2m(n),   &
                             iv%instid(i)%u10(n), &
                             iv%instid(i)%v10(n),  &
                             iv%instid(i)%ps(n),  &
                             iv%instid(i)%ts(n),  &
                             iv%instid(i)%smois(n),  &
                             iv%instid(i)%tslb(n),  &
                             iv%instid(i)%snowh(n), &
                             iv%instid(i)%isflg(n), &
                             nint(iv%instid(i)%soiltyp(n)), &
                             nint(iv%instid(i)%vegtyp(n)), &
                             iv%instid(i)%vegfra(n), &
                             iv%instid(i)%elevation(n), &
                             iv%instid(i)%clwp(n)

            write(unit=oma_rad_unit,fmt='(a)') 'OBS  : '
            write(unit=oma_rad_unit,fmt='(10f11.2)') ob%instid(i)%tb(:,n)
            write(unit=oma_rad_unit,fmt='(a)') 'BAK  : '
            write(unit=oma_rad_unit,fmt='(10f11.2)') iv%instid(i)%tb_xb(:,n)
            write(unit=oma_rad_unit,fmt='(a)') 'IVBC : '
            write(unit=oma_rad_unit,fmt='(10f11.2)')  iv%instid(i)%tb_inv(:,n)
            write(unit=oma_rad_unit,fmt='(a)') 'OMA  : '
            write(unit=oma_rad_unit,fmt='(10f11.2)')  re%instid(i)%tb(:,n)
            write(unit=oma_rad_unit,fmt='(a)') 'EMS  : '
            write(unit=oma_rad_unit,fmt='(10f11.2)')  iv%instid(i)%emiss(1:iv%instid(i)%nchan,n)
            write(unit=oma_rad_unit,fmt='(a)') 'ERR  : '
            write(unit=oma_rad_unit,fmt='(10f11.2)') iv%instid(i)%tb_error(:,n)
            write(unit=oma_rad_unit,fmt='(a)') 'QC   : '
            write(unit=oma_rad_unit,fmt='(10i11)') iv%instid(i)%tb_qc(:,n)

            if (write_profile) then
               nlevelss  = iv%instid(i)%nlevels
               if ( rtm_option == rtm_option_rttov ) then
#ifdef RTTOV
                  ! first, write RTTOV levels
                  write(unit=oma_rad_unit,fmt='(a)') 'RTM_level pres(mb) T(k) Q(ppmv)'
                  do k = 1, nlevelss
                     write(unit=oma_rad_unit,fmt='(i3,f10.2,f8.2,e11.4)') &
                        k, &                             ! RTTOV levels
                        coefs(i) % coef % ref_prfl_p(k) , &
                        iv%instid(i)%t(k,n) , &
                        iv%instid(i)%mr(k,n)
                  end do  ! end loop RTTOV levels
                  ! second, write WRF model levels
                  write(unit=oma_rad_unit,fmt='(a)') &
                     'WRF_level pres(mb) T(k) q(g/kg) clw(g/kg) rain(g/kg)'
                  do k=kts,kte
                     write(unit=oma_rad_unit,fmt='(i3,f10.2,f8.2,3e11.4)') &
                        k,  &                     ! WRF model levels
                        iv%instid(i)%pm(k,n) , &
                        iv%instid(i)%tm(k,n) , &
                        iv%instid(i)%qm(k,n)*1000 , &    
                        iv%instid(i)%qcw(k,n)*1000.0, &
                        iv%instid(i)%qrn(k,n)*1000.0
                  end do   ! end loop WRF model levels
#endif
               end if  ! end if rtm_option_rttov

               if ( rtm_option == rtm_option_crtm ) then
#ifdef CRTM
                  write(unit=oma_rad_unit,fmt='(a)') &
                     'level fullp(mb) halfp(mb) t(k) q(g/kg) water(mm) ice(mm) rain(mm) snow(mm) graupel(mm) hail(mm)'
                  if (crtm_cloud) then
                     do k=1,iv%instid(i)%nlevels-1
                        write(unit=oma_rad_unit,fmt='(i3,2f10.2,f8.2,13f8.3)') &
                           k,  &
                           iv%instid(i)%pf(k,n), &
                           iv%instid(i)%pm(k,n), &
                           iv%instid(i)%tm(k,n), &
                           iv%instid(i)%qm(k,n), &
                           iv%instid(i)%qcw(k,n), &
                           iv%instid(i)%qci(k,n), &
                           iv%instid(i)%qrn(k,n), &
                           iv%instid(i)%qsn(k,n), &
                           iv%instid(i)%qgr(k,n), &
                           iv%instid(i)%qhl(k,n), &
                           iv%instid(i)%rcw(k,n), &
                           iv%instid(i)%rci(k,n), &
                           iv%instid(i)%rrn(k,n), &
                           iv%instid(i)%rsn(k,n), &
                           iv%instid(i)%rgr(k,n), &
                           iv%instid(i)%rhl(k,n)
                     end do ! end loop profile
                  else  ! no cloud
                     do k=1,iv%instid(i)%nlevels-1
                        write(unit=oma_rad_unit,fmt='(i3,2f10.2,f8.2,7f8.3)') &
                           k,  &
                           iv%instid(i)%pf(k,n), &
                           iv%instid(i)%pm(k,n), &
                           iv%instid(i)%tm(k,n), &
                           iv%instid(i)%qm(k,n), &
                           0.0, &
                           0.0, &
                           0.0, &
                           0.0, &
                           0.0, &
                           0.0
                     end do ! end loop profile
                  end if ! end if crtm_cloud
#endif
               end if  ! end if crtm_option

            end if   ! end if write_profile
         end if    ! end if proc_domain
      end do     ! end do pixels
      close(unit=oma_rad_unit)
      call da_free_unit(oma_rad_unit)
   end do    !! end do instruments

   if (trace_use) call da_trace_exit("da_write_oa_rad_ascii")

end subroutine da_write_oa_rad_ascii


