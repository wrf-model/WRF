module module_HYDRO_drv
#ifdef MPP_LAND 
   use module_HYDRO_io, only:  mpp_output_rt, mpp_output_chrt, mpp_output_lakes
   USE module_mpp_land
#else
   use module_HYDRO_io, only:  output_rt, output_chrt, output_lakes
#endif
   use module_HYDRO_io, only: output_gw, restart_out_nc, restart_in_nc,  &
        get_file_dimension ,get2d_lsm_real, get2d_lsm_vegtyp, get2d_lsm_soltyp, &
        output_lsm
   use module_rt_data, only: rt_domain
   use module_GW_baseflow_data, only: gw2d
   use module_GW_baseflow, only: gw2d_allocate, gw2d_ini
   use module_namelist, only: nlst_rt
   use module_routing, only: getChanDim, landrt_ini
   use module_HYDRO_utils
   use module_namelist
   
   implicit none

   contains
   subroutine HYDRO_rst_out(did)
      implicit none
      integer:: rst_out  
      integer did, outflag
      character(len=19) out_date
      rst_out = -99
#ifdef MPP_LAND
   if(IO_id .eq. my_id) then
#endif
     if(nlst_rt(did)%dt .gt. nlst_rt(did)%rst_dt*60) then
        call geth_newdate(out_date, nlst_rt(did)%startdate, nint(nlst_rt(did)%dt*rt_domain(did)%rst_counts))
     else
        call geth_newdate(out_date, nlst_rt(did)%startdate, nint(nlst_rt(did)%rst_dt*60*rt_domain(did)%rst_counts))
     endif
     if ( out_date(1:19) == nlst_rt(did)%olddate(1:19) ) then
           rst_out = 99
           rt_domain(did)%rst_counts = rt_domain(did)%rst_counts + 1
     endif
! restart every month automatically.
     if ((nlst_rt(did)%olddate(9:10) == "01") .and. (nlst_rt(did)%olddate(12:13) == "00") .and. &
          (nlst_rt(did)%olddate(15:16) == "00").and. (nlst_rt(did)%olddate(18:19) == "00")) rst_out = 99
#ifdef MPP_LAND
   endif
     call mpp_land_bcast_int1(rst_out)
#endif
    if(rst_out .gt. 0) &
             call   RESTART_OUT_nc(trim("HYDRO_RST."//nlst_rt(did)%olddate(1:16)   &
                 //"_DOMAIN"//trim(nlst_rt(did)%hgrid)),  did)

#ifdef MPP_LAND
   if(IO_id .eq. my_id) then
#endif
#ifdef HYDRO_D
      write(6,*) "restartFile  =",  "RESTART."//nlst_rt(did)%olddate(1:16)   &
                 //"_DOMAIN"//trim(nlst_rt(did)%hgrid)
#endif
#ifdef MPP_LAND
   endif
#endif


   end subroutine HYDRO_rst_out

   subroutine HYDRO_out(did)
      implicit none
      integer did, outflag
      character(len=19) out_date, rt_out_date
      integer :: Kt, ounit

!    real, dimension(RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx):: soilmx_tmp, &
!           runoff1x_tmp, runoff2x_tmp, runoff3x_tmp,etax_tmp, &
!           EDIRX_tmp,ECX_tmp,ETTX_tmp,RCX_tmp,HX_tmp,acrain_tmp, &
!           ACSNOM_tmp, esnow2d_tmp, drip2d_tmp,dewfall_tmp, fpar_tmp, &
!           qfx_tmp, prcp_out_tmp, etpndx_tmp

   outflag = -99

#ifdef MPP_LAND
   if(IO_id .eq. my_id) then
#endif
      if(nlst_rt(did)%olddate(1:19) .eq. nlst_rt(did)%startdate(1:19) .and. rt_domain(did)%his_out_counts .eq. 0) then
#ifdef HYDRO_D
         write(6,*) "output hydrology at time : ",nlst_rt(did)%olddate(1:19), rt_domain(did)%his_out_counts
#endif
         call geth_newdate(rt_out_date, nlst_rt(did)%olddate, -1*nint(rt_domain(did)%his_out_counts*nlst_rt(did)%out_dt*60))
         outflag = 99
      else
         if(nlst_rt(did)%dt .gt. nlst_rt(did)%out_dt*60) then
             call geth_newdate(out_date, nlst_rt(did)%startdate, nint(nlst_rt(did)%dt*rt_domain(did)%out_counts))
         else
             call geth_newdate(out_date, nlst_rt(did)%startdate, nint(nlst_rt(did)%out_dt*60*rt_domain(did)%out_counts))
         endif
         if ( out_date(1:19) == nlst_rt(did)%olddate(1:19) ) then
#ifdef HYDRO_D
             write(6,*) "output hydrology at time : ",nlst_rt(did)%olddate(1:19)
#endif
             outflag = 99
             call geth_newdate(rt_out_date, nlst_rt(did)%olddate, -1*nint(rt_domain(did)%his_out_counts*nlst_rt(did)%out_dt*60))
         endif
      endif
#ifdef MPP_LAND
   endif
     call mpp_land_bcast_int1(outflag)
#endif


     call HYDRO_rst_out(did) 

     if (outflag .lt. 0) return

     rt_domain(did)%out_counts = rt_domain(did)%out_counts + 1
     rt_domain(did)%his_out_counts = rt_domain(did)%his_out_counts + 1

     if(nlst_rt(did)%out_dt*60 .gt. nlst_rt(did)%DT) then
        kt = rt_domain(did)%his_out_counts*nlst_rt(did)%out_dt*60/nlst_rt(did)%DT
     else
        kt = rt_domain(did)%his_out_counts
     endif




     call output_lsm(trim(nlst_rt(did)%olddate(1:4)//nlst_rt(did)%olddate(6:7)//nlst_rt(did)%olddate(9:10)  &
                 //nlst_rt(did)%olddate(12:13)//nlst_rt(did)%olddate(15:16)//  &
                 ".LSMOUT_DOMAIN"//trim(nlst_rt(did)%hgrid)),     &
                 did)

    

        if(nlst_rt(did)%SUBRTSWCRT .gt. 0 &
             .or. nlst_rt(did)%OVRTSWCRT .gt. 0 &
             .or. nlst_rt(did)%GWBASESWCRT .gt. 0 ) then
           if (nlst_rt(did)%HIRES_OUT.ge.1) then


#ifdef MPP_LAND
              call mpp_output_rt(rt_domain(did)%g_ixrt, rt_domain(did)%g_jxrt,    &
#else
              call output_rt(    &
#endif
                nlst_rt(did)%igrid, nlst_rt(did)%split_output_count, &
                RT_DOMAIN(did)%ixrt, RT_DOMAIN(did)%jxrt, &
                nlst_rt(did)%nsoil, &
!               nlst_rt(did)%startdate, nlst_rt(did)%olddate, RT_DOMAIN(did)%QSUBRT,&
                rt_out_date, nlst_rt(did)%olddate, RT_DOMAIN(did)%QSUBRT,&
                RT_DOMAIN(did)%ZWATTABLRT,RT_DOMAIN(did)%SMCRT,&
                RT_DOMAIN(did)%SUB_RESID,       &
                   RT_DOMAIN(did)%q_sfcflx_x,RT_DOMAIN(did)%q_sfcflx_y,&
                RT_DOMAIN(did)%soxrt,RT_DOMAIN(did)%soyrt,&
                RT_DOMAIN(did)%QSTRMVOLRT,RT_DOMAIN(did)%SFCHEADSUBRT, &
                nlst_rt(did)%geo_finegrid_flnm,nlst_rt(did)%DT,&
                RT_DOMAIN(did)%SLDPTH,RT_DOMAIN(did)%LATVAL,&
                RT_DOMAIN(did)%LONVAL,RT_DOMAIN(did)%dist,nlst_rt(did)%HIRES_OUT,&
                RT_DOMAIN(did)%QBDRYRT )

           if(nlst_rt(did)%GWBASESWCRT .eq. 3) then
	     
              call output_gw(    &
                nlst_rt(did)%igrid, nlst_rt(did)%split_output_count, &
                RT_DOMAIN(did)%ixrt, RT_DOMAIN(did)%jxrt,          &
                nlst_rt(did)%nsoil,                               &
!               nlst_rt(did)%startdate, nlst_rt(did)%olddate,    &
                rt_out_date, nlst_rt(did)%olddate,    &
                gw2d(did)%h, RT_DOMAIN(did)%SMCRT,                 &
                gw2d(did)%convgw, RT_DOMAIN(did)%SFCHEADSUBRT,     &
                nlst_rt(did)%geo_finegrid_flnm,nlst_rt(did)%DT, &
                RT_DOMAIN(did)%SLDPTH,RT_DOMAIN(did)%LATVAL,       &
                RT_DOMAIN(did)%LONVAL,rt_domain(did)%dist,           &
                nlst_rt(did)%HIRES_OUT)

	  endif
! BF end gw2d output section

           end if
#ifdef HYDRO_D
          write(6,*) "before call output_chrt"
#endif
     
           if (nlst_rt(did)%CHANRTSWCRT.eq.1.or.nlst_rt(did)%CHANRTSWCRT.eq.2) then 

#ifdef MPP_LAND
             call mpp_output_chrt(rt_domain(did)%mpp_nlinks,rt_domain(did)%nlinks_index, &
#else
             call output_chrt(  &
#endif
               nlst_rt(did)%igrid, nlst_rt(did)%split_output_count, &
               RT_DOMAIN(did)%NLINKS,RT_DOMAIN(did)%ORDER, &
!              nlst_rt(did)%startdate,nlst_rt(did)%olddate,RT_DOMAIN(did)%CHLON,&
               rt_out_date,nlst_rt(did)%olddate,RT_DOMAIN(did)%CHLON,&
               RT_DOMAIN(did)%CHLAT, &
               RT_DOMAIN(did)%HLINK, RT_DOMAIN(did)%ZELEV, &
               RT_DOMAIN(did)%QLINK,nlst_rt(did)%DT,Kt, &
               RT_DOMAIN(did)%STRMFRXSTPTS,nlst_rt(did)%order_to_write)

               if (RT_DOMAIN(did)%NLAKES.gt.0)  &
#ifdef MPP_LAND
                call mpp_output_lakes( RT_DOMAIN(did)%lake_index, &
#else
                call output_lakes(  &
#endif
                   nlst_rt(did)%igrid, nlst_rt(did)%split_output_count, &
                   RT_DOMAIN(did)%NLAKES, &
!                  trim(nlst_rt(did)%startdate), trim(nlst_rt(did)%olddate), &
                   trim(rt_out_date), trim(nlst_rt(did)%olddate), &
                   RT_DOMAIN(did)%LATLAKE,RT_DOMAIN(did)%LONLAKE, &
                   RT_DOMAIN(did)%ELEVLAKE,RT_DOMAIN(did)%QLAKEI, &
                   RT_DOMAIN(did)%QLAKEO, &
                   RT_DOMAIN(did)%RESHT,nlst_rt(did)%DT,Kt)
           endif
#ifdef HYDRO_D
           write(6,*) "end calling otuput functions"
#endif

        endif  ! end of routing switch


      end subroutine HYDRO_out


      subroutine HYDRO_rst_in(did)
        integer :: did
        integer:: flag 



   flag = -1
#ifdef MPP_LAND
   if(my_id.eq.IO_id) then
#endif
      if (trim(nlst_rt(did)%restart_file) /= "") then
          flag = 99
          rt_domain(did)%timestep_flag = 99   ! continue run
      endif 
#ifdef MPP_LAND
   endif 
   call mpp_land_bcast_int1(flag)
#endif

      
   if (flag.eq.99) then

#ifdef MPP_LAND
     if(my_id.eq.IO_id) then
#endif
#ifdef HYDRO_D
        write(6,*) "*** read restart data: ",trim(nlst_rt(did)%restart_file)
#endif
#ifdef MPP_LAND
     endif 
#endif
      call   RESTART_IN_nc(trim(nlst_rt(did)%restart_file), did)

  if (trim(nlst_rt(did)%restart_file) /= "") then 
          nlst_rt(did)%restart_file = ""
  endif
  endif
 end subroutine HYDRO_rst_in

     subroutine HYDRO_time_adv(did)
        implicit none
        character(len = 19) :: newdate 
        integer did
 
#ifdef MPP_LAND
   if(IO_id.eq.my_id) then
#endif
         call geth_newdate(newdate, nlst_rt(did)%olddate, nint( nlst_rt(did)%dt))
         nlst_rt(did)%olddate = newdate
#ifdef HYDRO_D
         write(6,*) "current time is ",newdate
#endif
#ifdef MPP_LAND
   endif
#endif
     end subroutine HYDRO_time_adv

  integer function nfeb_yw(year)
    !
    ! Compute the number of days in February for the given year.
    !
    implicit none
    integer, intent(in) :: year ! Four-digit year

    nfeb_yw = 28 ! By default, February has 28 days ...
    if (mod(year,4).eq.0) then
       nfeb_yw = 29  ! But every four years, it has 29 days ...
       if (mod(year,100).eq.0) then
          nfeb_yw = 28  ! Except every 100 years, when it has 28 days ...
          if (mod(year,400).eq.0) then
             nfeb_yw = 29  ! Except every 400 years, when it has 29 days ...
             if (mod(year,3600).eq.0) then
                nfeb_yw = 28  ! Except every 3600 years, when it has 28 days.
             endif
          endif
       endif
    endif
  end function nfeb_yw

  subroutine geth_newdate (ndate, odate, idt)
    implicit none

    !  From old date ("YYYY-MM-DD HH:MM:SS.ffff" or "YYYYMMDDHHMMSSffff") and 
    !  delta-time, compute the new date.

    !  on entry     -  odate  -  the old hdate.
    !                  idt    -  the change in time

    !  on exit      -  ndate  -  the new hdate.

    integer, intent(in)           :: idt
    character (len=*), intent(out) :: ndate
    character (len=*), intent(in)  :: odate

    !  Local Variables

    !  yrold    -  indicates the year associated with "odate"
    !  moold    -  indicates the month associated with "odate"
    !  dyold    -  indicates the day associated with "odate"
    !  hrold    -  indicates the hour associated with "odate"
    !  miold    -  indicates the minute associated with "odate"
    !  scold    -  indicates the second associated with "odate"

    !  yrnew    -  indicates the year associated with "ndate"
    !  monew    -  indicates the month associated with "ndate"
    !  dynew    -  indicates the day associated with "ndate"
    !  hrnew    -  indicates the hour associated with "ndate"
    !  minew    -  indicates the minute associated with "ndate"
    !  scnew    -  indicates the second associated with "ndate"

    !  mday     -  a list assigning the number of days in each month

    !  i        -  loop counter
    !  nday     -  the integer number of days represented by "idt"
    !  nhour    -  the integer number of hours in "idt" after taking out
    !              all the whole days
    !  nmin     -  the integer number of minutes in "idt" after taking out
    !              all the whole days and whole hours.
    !  nsec     -  the integer number of minutes in "idt" after taking out
    !              all the whole days, whole hours, and whole minutes.

    integer :: newlen, oldlen
    integer :: yrnew, monew, dynew, hrnew, minew, scnew, frnew
    integer :: yrold, moold, dyold, hrold, miold, scold, frold
    integer :: nday, nhour, nmin, nsec, nfrac, i, ifrc
    logical :: opass
    character (len=10) :: hfrc
    character (len=1) :: sp
    logical :: punct
    integer :: yrstart, yrend, mostart, moend, dystart, dyend
    integer :: hrstart, hrend, mistart, miend, scstart, scend, frstart
    integer :: units
    integer, dimension(12) :: mday = (/31,28,31,30,31,30,31,31,30,31,30,31/)
!yw    integer nfeb_yw   

    ! Determine if odate is "YYYY-MM-DD_HH ... " or "YYYYMMDDHH...."
    if (odate(5:5) == "-") then
       punct = .TRUE.
    else
       punct = .FALSE.
    endif

    !  Break down old hdate into parts

    hrold = 0
    miold = 0
    scold = 0
    frold = 0
    oldlen = LEN(odate)
    if (punct) then
       yrstart = 1
       yrend = 4
       mostart = 6
       moend = 7
       dystart = 9
       dyend = 10
       hrstart = 12
       hrend = 13
       mistart = 15
       miend = 16
       scstart = 18
       scend = 19
       frstart = 21
       select case (oldlen)
       case (10)
          ! Days
          units = 1
       case (13)
          ! Hours
          units = 2
       case (16)
          ! Minutes
          units = 3
       case (19)
          ! Seconds
          units = 4
       case (21)
          ! Tenths
          units = 5
       case (22)
          ! Hundredths
          units = 6
       case (23)
          ! Thousandths
          units = 7
       case (24)
          ! Ten thousandths
          units = 8
       case default
#ifdef HYDRO_D
          write(*,*) 'ERROR: geth_newdate:  odd length: #'//trim(odate)//'#'
          stop
#endif
       end select

       if (oldlen.ge.11) then
          sp = odate(11:11)
       else
          sp = ' '
       end if

    else

       yrstart = 1
       yrend = 4
       mostart = 5
       moend = 6
       dystart = 7
       dyend = 8
       hrstart = 9
       hrend = 10
       mistart = 11
       miend = 12
       scstart = 13
       scend = 14
       frstart = 15

       select case (oldlen)
       case (8)
          ! Days
          units = 1
       case (10)
          ! Hours
          units = 2
       case (12)
          ! Minutes
          units = 3
       case (14)
          ! Seconds
          units = 4
       case (15)
          ! Tenths
          units = 5
       case (16)
          ! Hundredths
          units = 6
       case (17)
          ! Thousandths
          units = 7
       case (18)
          ! Ten thousandths
          units = 8
       case default
#ifdef HYDRO_D
          write(*,*) 'ERROR: geth_newdate:  odd length: #'//trim(odate)//'#'
          stop
#endif
       end select
    endif

    !  Use internal READ statements to convert the CHARACTER string
    !  date into INTEGER components.

    read(odate(yrstart:yrend),  '(i4)') yrold
    read(odate(mostart:moend),  '(i2)') moold
    read(odate(dystart:dyend), '(i2)') dyold
    if (units.ge.2) then
       read(odate(hrstart:hrend),'(i2)') hrold
       if (units.ge.3) then
          read(odate(mistart:miend),'(i2)') miold
          if (units.ge.4) then
             read(odate(scstart:scend),'(i2)') scold
             if (units.ge.5) then
                read(odate(frstart:oldlen),*) frold
             end if
          end if
       end if
    end if

    !  Set the number of days in February for that year.

    mday(2) = nfeb_yw(yrold)

    !  Check that ODATE makes sense.

    opass = .TRUE.

    !  Check that the month of ODATE makes sense.

    if ((moold.gt.12).or.(moold.lt.1)) then
#ifdef HYDRO_D
       write(*,*) 'GETH_NEWDATE:  Month of ODATE = ', moold
#endif
       opass = .FALSE.
    end if

    !  Check that the day of ODATE makes sense.

    if ((dyold.gt.mday(moold)).or.(dyold.lt.1)) then
#ifdef HYDRO_D
       write(*,*) 'GETH_NEWDATE:  Day of ODATE = ', dyold
#endif
       opass = .FALSE.
    end if

    !  Check that the hour of ODATE makes sense.

    if ((hrold.gt.23).or.(hrold.lt.0)) then
#ifdef HYDRO_D
       write(*,*) 'GETH_NEWDATE:  Hour of ODATE = ', hrold
#endif
       opass = .FALSE.
    end if

    !  Check that the minute of ODATE makes sense.

    if ((miold.gt.59).or.(miold.lt.0)) then
#ifdef HYDRO_D
       write(*,*) 'GETH_NEWDATE:  Minute of ODATE = ', miold
#endif
       opass = .FALSE.
    end if

    !  Check that the second of ODATE makes sense.

    if ((scold.gt.59).or.(scold.lt.0)) then
#ifdef HYDRO_D
       write(*,*) 'GETH_NEWDATE:  Second of ODATE = ', scold
#endif
       opass = .FALSE.
    end if

    !  Check that the fractional part  of ODATE makes sense.
    if (.not.opass) then
#ifdef HYDRO_D
       write(*,*) 'Crazy ODATE: ', odate(1:oldlen), oldlen
       stop
#endif
    end if

    !  Date Checks are completed.  Continue.


    !  Compute the number of days, hours, minutes, and seconds in idt

    if (units.ge.5) then !idt should be in fractions of seconds
       ifrc = oldlen-(frstart)+1
       ifrc = 10**ifrc
       nday   = abs(idt)/(86400*ifrc)
       nhour  = mod(abs(idt),86400*ifrc)/(3600*ifrc)
       nmin   = mod(abs(idt),3600*ifrc)/(60*ifrc)
       nsec   = mod(abs(idt),60*ifrc)/(ifrc)
       nfrac = mod(abs(idt), ifrc)
    else if (units.eq.4) then  !idt should be in seconds
       ifrc = 1
       nday   = abs(idt)/86400 ! integer number of days in delta-time
       nhour  = mod(abs(idt),86400)/3600
       nmin   = mod(abs(idt),3600)/60
       nsec   = mod(abs(idt),60)
       nfrac  = 0
    else if (units.eq.3) then !idt should be in minutes
       ifrc = 1
       nday   = abs(idt)/1440 ! integer number of days in delta-time
       nhour  = mod(abs(idt),1440)/60
       nmin   = mod(abs(idt),60)
       nsec   = 0
       nfrac  = 0
    else if (units.eq.2) then !idt should be in hours
       ifrc = 1
       nday   = abs(idt)/24 ! integer number of days in delta-time
       nhour  = mod(abs(idt),24)
       nmin   = 0
       nsec   = 0
       nfrac  = 0
    else if (units.eq.1) then !idt should be in days
       ifrc = 1
       nday   = abs(idt)    ! integer number of days in delta-time
       nhour  = 0
       nmin   = 0
       nsec   = 0
       nfrac  = 0
    else
#ifdef HYDRO_D
       write(*,'(''GETH_NEWDATE: Strange length for ODATE: '', i3)') &
            oldlen
       write(*,*) '#'//odate(1:oldlen)//'#'
       stop
#endif
    end if

    if (idt.ge.0) then

       frnew = frold + nfrac
       if (frnew.ge.ifrc) then
          frnew = frnew - ifrc
          nsec = nsec + 1
       end if

       scnew = scold + nsec
       if (scnew .ge. 60) then
          scnew = scnew - 60
          nmin  = nmin + 1
       end if

       minew = miold + nmin
       if (minew .ge. 60) then
          minew = minew - 60
          nhour  = nhour + 1
       end if

       hrnew = hrold + nhour
       if (hrnew .ge. 24) then
          hrnew = hrnew - 24
          nday  = nday + 1
       end if

       dynew = dyold
       monew = moold
       yrnew = yrold
       do i = 1, nday
          dynew = dynew + 1
          if (dynew.gt.mday(monew)) then
             dynew = dynew - mday(monew)
             monew = monew + 1
             if (monew .gt. 12) then
                monew = 1
                yrnew = yrnew + 1
                ! If the year changes, recompute the number of days in February
                mday(2) = nfeb_yw(yrnew)
             end if
          end if
       end do

    else if (idt.lt.0) then

       frnew = frold - nfrac
       if (frnew .lt. 0) then
          frnew = frnew + ifrc
          nsec = nsec + 1
       end if

       scnew = scold - nsec
       if (scnew .lt. 00) then
          scnew = scnew + 60
          nmin  = nmin + 1
       end if

       minew = miold - nmin
       if (minew .lt. 00) then
          minew = minew + 60
          nhour  = nhour + 1
       end if

       hrnew = hrold - nhour
       if (hrnew .lt. 00) then
          hrnew = hrnew + 24
          nday  = nday + 1
       end if

       dynew = dyold
       monew = moold
       yrnew = yrold
       do i = 1, nday
          dynew = dynew - 1
          if (dynew.eq.0) then
             monew = monew - 1
             if (monew.eq.0) then
                monew = 12
                yrnew = yrnew - 1
                ! If the year changes, recompute the number of days in February
                mday(2) = nfeb_yw(yrnew)
             end if
             dynew = mday(monew)
          end if
       end do
    end if

    !  Now construct the new mdate

    newlen = LEN(ndate)

    if (punct) then

       if (newlen.gt.frstart) then
          write(ndate(1:scend),19) yrnew, monew, dynew, hrnew, minew, scnew
          write(hfrc,'(i10)') frnew+1000000000
          ndate = ndate(1:scend)//'.'//hfrc(31-newlen:10)

       else if (newlen.eq.scend) then
          write(ndate(1:scend),19) yrnew, monew, dynew, hrnew, minew, scnew
19        format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2,':',i2.2)

       else if (newlen.eq.miend) then
          write(ndate,16) yrnew, monew, dynew, hrnew, minew
16        format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2)

       else if (newlen.eq.hrend) then
          write(ndate,13) yrnew, monew, dynew, hrnew
13        format(i4,'-',i2.2,'-',i2.2,'_',i2.2)

       else if (newlen.eq.dyend) then
          write(ndate,10) yrnew, monew, dynew
10        format(i4,'-',i2.2,'-',i2.2)

       end if

    else

       if (newlen.gt.frstart) then
          write(ndate(1:scend),119) yrnew, monew, dynew, hrnew, minew, scnew
          write(hfrc,'(i10)') frnew+1000000000
          ndate = ndate(1:scend)//'.'//hfrc(31-newlen:10)

       else if (newlen.eq.scend) then
          write(ndate(1:scend),119) yrnew, monew, dynew, hrnew, minew, scnew
119       format(i4,i2.2,i2.2,i2.2,i2.2,i2.2)

       else if (newlen.eq.miend) then
          write(ndate,116) yrnew, monew, dynew, hrnew, minew
116       format(i4,i2.2,i2.2,i2.2,i2.2)

       else if (newlen.eq.hrend) then
          write(ndate,113) yrnew, monew, dynew, hrnew
113       format(i4,i2.2,i2.2,i2.2)

       else if (newlen.eq.dyend) then
          write(ndate,110) yrnew, monew, dynew
110       format(i4,i2.2,i2.2)

       end if

    endif

    if (punct .and. (oldlen.ge.11) .and. (newlen.ge.11)) ndate(11:11) = sp

  end subroutine geth_newdate
  
     subroutine HYDRO_exe(did)


        implicit none
        integer:: did
        integer:: rst_out


        call HYDRO_out(did)


! running land surface model
! cpl: 0--offline run; 
!      1-- coupling with WRF but running offline lsm; 
!      2-- coupling with WRF but do not run offline lsm  
!      3-- coupling with LIS and do not run offline lsm  
!      4:  coupling with CLM
!          if(nlst_rt(did)%SYS_CPL .eq. 0 .or. nlst_rt(did)%SYS_CPL .eq. 1 )then
!                  call drive_noahLSF(did,kt)
!          else
!              ! does not run the NOAH LASF model, only read the parameter
!              call read_land_par(did,lsm(did)%ix,lsm(did)%jx)
!          endif





           IF (nlst_rt(did)%GWBASESWCRT .ne. 0     &
               .or. nlst_rt(did)%SUBRTSWCRT .NE.0  &
               .or. nlst_rt(did)%OVRTSWCRT .NE. 0 ) THEN


! running routing model
                call exeRouting(did)

    END IF    ! End if for channel routing option


! advance to next time step
!      if(
           call HYDRO_time_adv(did)

      ! output for history 
           call HYDRO_out(did)


            
           RT_DOMAIN(did)%SOLDRAIN = 0
           RT_DOMAIN(did)%QSUBRT = 0



      end subroutine HYDRO_exe


      subroutine HYDRO_ini(ntime, did,ix0,jx0, vegtyp,soltyp)
        implicit none
        integer ntime, did
        integer rst_out, ix,jx
!        integer, OPTIONAL:: ix0,jx0
        integer:: ix0,jx0
        integer, dimension(ix0,jx0),OPTIONAL :: vegtyp, soltyp



#ifdef MPP_LAND
    call  MPP_LAND_INIT()
#endif
 

! read the namelist
! the lsm namelist will be read by rtland sequentially again.
     call read_rt_nlst(nlst_rt(did) )


             IF (nlst_rt(did)%GWBASESWCRT .eq. 0 &
               .and. nlst_rt(did)%SUBRTSWCRT .eq.0  &
               .and. nlst_rt(did)%OVRTSWCRT .eq. 0 ) return

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! get the dimension 
     call get_file_dimension(trim(nlst_rt(did)%geo_static_flnm), ix,jx)
   
       
#ifdef MPP_LAND

  if (nlst_rt(did)%sys_cpl .eq. 1 .or. nlst_rt(did)%sys_cpl .eq. 4) then
!sys_cpl: 1-- coupling with HRLDAS but running offline lsm; 
!         2-- coupling with WRF but do not run offline lsm  
!         3-- coupling with LIS and do not run offline lsm  
!         4:  coupling with CLM

! create 2 dimensiaon logical mapping of the CPUs for coupling with CLM or HRLDAS.
         call log_map2d()

         global_nx = ix  ! get from land model
         global_ny = jx  ! get from land model

         call mpp_land_bcast_int1(global_nx)
         call mpp_land_bcast_int1(global_ny)

!!! temp set global_nx to ix 
         rt_domain(did)%ix = global_nx
         rt_domain(did)%jx = global_ny

! over write the ix and jx
         call MPP_LAND_PAR_INI(1,rt_domain(did)%ix,rt_domain(did)%jx,&
              nlst_rt(did)%AGGFACTRT)
   else  
!  coupled with WRF, LIS
         numprocs = node_info(1,1)
         call wrf_LAND_set_INIT(node_info,numprocs,nlst_rt(did)%AGGFACTRT)

         rt_domain(did)%ix = local_nx
         rt_domain(did)%jx = local_ny
   endif

      rt_domain(did)%g_IXRT=global_rt_nx
      rt_domain(did)%g_JXRT=global_rt_ny
      rt_domain(did)%ixrt = local_rt_nx
      rt_domain(did)%jxrt = local_rt_ny

#ifdef HYDRO_D
      write(6,*) "rt_domain(did)%g_IXRT, rt_domain(did)%g_JXRT, rt_domain(did)%ixrt, rt_domain(did)%jxrt"
      write(6,*)  rt_domain(did)%g_IXRT, rt_domain(did)%g_JXRT, rt_domain(did)%ixrt, rt_domain(did)%jxrt
      write(6,*) "rt_domain(did)%ix, rt_domain(did)%jx "
      write(6,*) rt_domain(did)%ix, rt_domain(did)%jx 
      write(6,*) "global_nx, global_ny, local_nx, local_ny"
      write(6,*) global_nx, global_ny, local_nx, local_ny
#endif
#else
! sequential
      rt_domain(did)%ix = ix
      rt_domain(did)%jx = jx
      rt_domain(did)%ixrt = ix*nlst_rt(did)%AGGFACTRT 
      rt_domain(did)%jxrt = jx*nlst_rt(did)%AGGFACTRT
#endif

      
!      allocate rt arrays


       call getChanDim(did)
#ifdef HYDRO_D
       write(6,*) "finish getChanDim "
#endif

      if(nlst_rt(did)%GWBASESWCRT .eq. 3 ) then
          call gw2d_allocate(did,&
                             rt_domain(did)%ixrt,&
                             rt_domain(did)%jxrt,&
                             nlst_rt(did)%nsoil)
#ifdef HYDRO_D
       write(6,*) "finish gw2d_allocate"
#endif
      endif

! calculate the distance between grids for routing.
! decompose the land parameter/data 


      ix0= rt_domain(did)%ix
      jx0= rt_domain(did)%jx
      call lsm_input(did,ix0=ix0,jx0=jx0,vegtyp0=vegtyp,soltyp0=soltyp)

#ifdef HYDRO_D
       write(6,*) "finish decomposion"
#endif


     call get_dist_lsm(did) 
     call get_dist_lrt(did)


! rt model initilization
      call LandRT_ini(did)

#ifdef HYDRO_D
      write(6,*) "finish LandRT_ini"    
#endif

       
      if(nlst_rt(did)%GWBASESWCRT .eq. 3 ) then
	
          call gw2d_ini(did,&
                        nlst_rt(did)%dt,&
                        nlst_rt(did)%dxrt0)
#ifdef HYDRO_D                        
          write(6,*) "finish gw2d_ini"      
#endif
      endif
#ifdef HYDRO_D
       write(6,*) "finish LandRT_ini"
#endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     IF (nlst_rt(did)%TERADJ_SOLAR.EQ.1 .and. nlst_rt(did)%CHANRTSWCRT.NE.2) THEN   ! Perform ter rain adjustment of incoming solar
#ifdef MPP_LAND
          call MPP_seq_land_SO8(rt_domain(did)%SO8LD_D,rt_domain(did)%SO8LD_Vmax,&
             rt_domain(did)%TERRAIN, rt_domain(did)%dist_lsm,&
             rt_domain(did)%ix,rt_domain(did)%jx,global_nx,global_ny)
#else
          call seq_land_SO8(rt_domain(did)%SO8LD_D,rt_domain(did)%SO8LD_Vmax,&
                   rt_domain(did)%TERRAIN,rt_domain(did)%dist_lsm,&
                   rt_domain(did)%ix,rt_domain(did)%jx)
#endif
    endif


     IF (nlst_rt(did)%CHANRTSWCRT.EQ.1 .or. nlst_rt(did)%CHANRTSWCRT .eq. 2 .or. nlst_rt(did)%GWBASESWCRT .gt. 0) then
       call get_basn_area(did)
     endif

     IF (nlst_rt(did)%CHANRTSWCRT.EQ.1 .or. nlst_rt(did)%CHANRTSWCRT .eq. 2 ) then
!       call get_basn_area(did)
        call get_node_area(did)
     endif
     


!      if(rt_domain(did)%rst_flag .eq. -99 ) return
! restart the file
       call HYDRO_rst_in(did)

! output at the initial time
       call HYDRO_out(did)


      end subroutine HYDRO_ini

      subroutine lsm_input(did,ix0,jx0,vegtyp0,soltyp0)
         implicit none
         integer did, leng
         parameter(leng=100)
         integer :: i,j, nn
         integer, allocatable, dimension(:,:) :: soltyp
         real, dimension(leng) :: xdum1, MAXSMC,refsmc,wltsmc

        integer :: ix0,jx0
        integer, dimension(ix0,jx0),OPTIONAL :: vegtyp0, soltyp0

#ifdef HYDRO_D
         write(6,*) RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx
#endif

         allocate(soltyp(RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx) )

         soltyp = 0
         call get2d_lsm_soltyp(soltyp,RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx,trim(nlst_rt(did)%geo_static_flnm))

         call get2d_lsm_real("HGT",RT_DOMAIN(did)%TERRAIN,RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx,trim(nlst_rt(did)%geo_static_flnm))

         call get2d_lsm_real("XLAT",RT_DOMAIN(did)%lat_lsm,RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx,trim(nlst_rt(did)%geo_static_flnm))
         call get2d_lsm_real("XLONG",RT_DOMAIN(did)%lon_lsm,RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx,trim(nlst_rt(did)%geo_static_flnm))
         call get2d_lsm_vegtyp(RT_DOMAIN(did)%VEGTYP,RT_DOMAIN(did)%ix,RT_DOMAIN(did)%jx,trim(nlst_rt(did)%geo_static_flnm))



            if(nlst_rt(did)%sys_cpl .eq. 2) then
              ! coupling with WRF
                where(soltyp0 == 14) VEGTYP0 = 16
                where(VEGTYP0 == 16 ) soltyp0 = 14
                soltyp = soltyp0
                RT_DOMAIN(did)%VEGTYP = VEGTYP0
            endif

         where(soltyp == 14) RT_DOMAIN(did)%VEGTYP = 16
         where(RT_DOMAIN(did)%VEGTYP == 16 ) soltyp = 14

! LKSAT, 
! temporary set
       RT_DOMAIN(did)%SMCRTCHK = 0
       RT_DOMAIN(did)%SMCAGGRT = 0
       RT_DOMAIN(did)%STCAGGRT = 0
       RT_DOMAIN(did)%SH2OAGGRT = 0
     

       RT_DOMAIN(did)%zsoil(1:nlst_rt(did)%nsoil) = nlst_rt(did)%zsoil8(1:nlst_rt(did)%nsoil)
       do i = 2, nlst_rt(did)%nsoil
          RT_DOMAIN(did)%sldpth(i) = RT_DOMAIN(did)%zsoil(i-1)-RT_DOMAIN(did)%zsoil(i)
       enddo
       RT_DOMAIN(did)%SOLDEPRT = -1.0*RT_DOMAIN(did)%ZSOIL(nlst_rt(did)%NSOIL)

!      input OV_ROUGH from OVROUGH.TBL
#ifdef MPP_LAND
       if(my_id .eq. IO_id) then
#endif

       open(71,file="HYDRO.TBL", form="formatted") 
!read OV_ROUGH first
          read(71,*) nn
          read(71,*)    
          do i = 1, nn
             read(71,*) RT_DOMAIN(did)%OV_ROUGH(i)
          end do 
!read parameter for LKSAT
          read(71,*) nn
          read(71,*)    
          do i = 1, nn
             read(71,*) xdum1(i), MAXSMC(i),refsmc(i),wltsmc(i)
          end do 
       close(71)

#ifdef MPP_LAND
       endif
       call mpp_land_bcast_real(leng,RT_DOMAIN(did)%OV_ROUGH)
       call mpp_land_bcast_real(leng,xdum1)
       call mpp_land_bcast_real(leng,MAXSMC)
       call mpp_land_bcast_real(leng,refsmc)
       call mpp_land_bcast_real(leng,wltsmc)
#endif

       rt_domain(did)%lksat = 0.0
       do j = 1, RT_DOMAIN(did)%jx
             do i = 1, RT_DOMAIN(did)%ix
                rt_domain(did)%lksat(i,j) = xdum1(soltyp(i,j) ) * 1000.0
                IF(rt_domain(did)%VEGTYP(i,j) == 1 ) THEN   ! urban
                    rt_domain(did)%SMCMAX1(i,j) = 0.45
                    rt_domain(did)%SMCREF1(i,j) = 0.42
                    rt_domain(did)%SMCWLT1(i,j) = 0.40
                else
                    rt_domain(did)%SMCMAX1(i,j) = MAXSMC(soltyp(I,J))
                    rt_domain(did)%SMCREF1(i,j) = refsmc(soltyp(I,J))
                    rt_domain(did)%SMCWLT1(i,j) = wltsmc(soltyp(I,J))
                ENDIF
             end do
       end do

       deallocate(soltyp)


      end subroutine lsm_input


end module module_HYDRO_drv
      subroutine land_finish()
#ifdef HYDRO_D
#ifdef MPP_LAND
         call mpp_land_finish()
#else
         stop
#endif
#endif
     return
     end  subroutine land_finish

! stop the job due to the fatal error.
      subroutine HYDRO_stop()
        integer :: ierr
#ifdef HYDRO_D
      write(6,*) "The job is stoped due to the fatal error."
#ifdef MPP_LAND
         call mpp_land_abort()
         call MPI_finalize(ierr)
#else
         stop "Fatal Error"
#endif

#endif
     return
     end  subroutine HYDRO_stop  

