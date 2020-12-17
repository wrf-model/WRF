subroutine da_blacklist_rad (platform_name, satid, sensor_name, nchan, iuse)

   !---------------------------------------------------------------------------
   !  PURPOSE: black list for radiance data.
   !  METHOD:  based on black_ds.f90 provided by Paul Poli <Paul.Poli@ecmwf.int>
   !           -1 == CONSTANT
   !           -2 == EXPERIMENTAL
   !  HISTORY: 04/17/2012
   !---------------------------------------------------------------------------

   implicit none

   character(len=*),  intent(in)    :: platform_name, sensor_name
   integer,           intent(in)    :: satid, nchan
   integer,           intent(inout) :: iuse(nchan)

   character(len=8) :: cdate   !ccyymmdd
   character(len=6) :: ctime   !hhmmss

   cdate = analysis_date(1:4)//analysis_date(6:7)//analysis_date(9:10)
   ctime = analysis_date(12:13)//analysis_date(15:16)//analysis_date(18:19)

!============
! from ERA
!============
   select case ( trim(sensor_name) )
   case ( 'amsua' )
      iuse(1)  = -1
      iuse(2)  = -1
      iuse(3)  = -1
      iuse(4)  = -1
      iuse(14) = -1   ! NCEP
      iuse(15) = -1
   case ( 'amsub', 'mhs' )
      iuse(1)  = -1
      iuse(2)  = -1
   case ( 'hirs' )
      iuse(1)     = -1   ! NCEP
      iuse(16:19) = -1
   end select

   if ( trim(platform_name) == 'noaa' ) then
      select case ( satid )
      case ( 15 )
         if ( cdate < '19980801' )           iuse = -1   ! passive period
         if ( trim(sensor_name) == 'amsub' ) iuse = -1   ! bad quality noaa-15-amsub
         if ( trim(sensor_name) == 'hirs' ) then
            ! noisy for several periods, unknown reason
            if ( cdate == '19981006' .and. ctime == '060000' )   iuse = -1
            if ( cdate == '19990302' .and. ctime == '060000' )   iuse = -1
            if ( cdate >= '20000219' .and. cdate <= '20000220' ) iuse = -1
            ! unreliable since 7 June 2000 (filter wheel, ...)
            if ( cdate >= '20000607' ) iuse = -1
         end if   ! end if noaa-15-hirs
         if ( trim(sensor_name) == 'amsua' ) then
            ! NCEP 199811
            ! noisy, unknown reason
            if ( cdate == '19990101' .and. ctime == '060000' ) iuse = -1
            ! channel 14 failure
            if ( cdate >= '20001030' ) iuse(14) = -1
            if ( cdate >= '20020410' ) iuse(11) = -1
         end if   ! end if noaa-15-amsua
      case ( 16 )
         ! data unusable before 26 Oct 2000 due to calibration errors
         if ( cdate < '20001026' ) iuse = -1
         if ( trim(sensor_name) == 'hirs' ) then
            if ( cdate == '20001208' .and. ctime == '000000' ) iuse = -1
            if ( cdate >  '20040524' ) iuse = -1
         end if   ! end if noaa-16-hirs
         if ( trim(sensor_name) == 'amsua' ) then
            if ( cdate == '20010111' ) iuse = -1
            ! channel 8 noisy from the beginning
            iuse(8) = -1
            if ( cdate > '20030512' .and. cdate <  '20040701' ) iuse(9) = -1
            if ( cdate > '20050117' .and. cdate <  '20050125' ) iuse(9:14) = -1
            if ( cdate > '20021121' .and. cdate <= '20021203' ) iuse = -1
            if ( cdate > '20021203' .and. cdate <= '20021216' ) iuse(5:8) = -1
            !to be checked if ( cdate > '20070124' )
            ! most channel noisy due to solar eclipse
            if ( cdate >= '20090123' .and. cdate < '20090202' ) iuse = -1
            ! unexplained instrument anomaly
            if ( cdate >= '20090408' .and. cdate < '20090416' ) iuse = -1
            if ( cdate >  '20090603' ) iuse = -1
         end if   ! end if noaa-16-amsua
         if ( trim(sensor_name) == 'amsub' ) then
            if ( cdate >= '20070918' ) iuse = -1
         end if   ! end if noaa-16-amsub
      case ( 17 )
         if ( trim(sensor_name) == 'hirs' ) then
            ! channels 2, 3, 4, 5 became suddenly much more noisy
            if ( cdate >= '20101023' ) iuse(2:5) = -1
            ! increase noise, some channels are really bad, compromising cloud detection
            if ( cdate >= '20111204' ) iuse = -1
         end if   ! end if noaa-17-hirs
         if ( trim(sensor_name) == 'amsua' ) then
            if ( cdate >  '20030129' ) iuse(7) = -1
            ! unusable due to scan motor problems
            if ( cdate >= '20031028' ) iuse = -1
         end if   ! end if noaa-17-amsua
         if ( trim(sensor_name) == 'amsub' ) then
            ! blackbody/space view anomalies
            if ( cdate > '20091216' ) iuse = -1
         end if   ! end if noaa-17-amsub
      case ( 18 )
         if ( trim(sensor_name) == 'hirs' ) then
            ! noaa-18 hirs data were assimilated until 20090312 in EI, but really
            ! this blacklisting should be done from the beginning
            iuse = -1
         end if   ! end if noaa-18-hirs
         if ( trim(sensor_name) == 'amsua' ) then
            ! channel 9 briefly noisy
            if ( cdate > '20071115' .and. cdate < '20071219' ) iuse(9) = -1
            if ( cdate > '20101215' .and. cdate < '20101226' ) iuse(9) = -1
            ! orbit too close to noaa-19, so data heavily thnned out.
            ! poor sampling can lead to problems with VarBC
            !to be checked if ( cdate >= '20090804' .and. cdate < '20091224' ) iuse = -1
         end if   ! end if noaa-18-amsua
      case ( 19 )
         ! first noaa-19 data to arrive in opearions: 2009040700
         if ( cdate < '20090407' ) iuse = -1
         if ( cdate >= '20090407' .and. cdate < '20090501' ) iuse = -1
         if ( cdate >= '20090501' .and. cdate < '20090602' ) iuse = -1
         if ( trim(sensor_name) == 'amsua' ) then
            ! channel 8 excluded due to temporary noise issues
            if ( cdate >= '20090627' .and. cdate < '20090804' ) iuse(8) = -1
            if ( cdate >= '20091224' ) iuse(8) = -1
         end if   ! end if noaa-19-amsua
         if ( trim(sensor_name) == 'mhs' ) then
            ! allow bias adjustment after update of antenna pattern correction
            if ( cdate > '20090621' .and. cdate < '20090627' ) iuse = -1
            ! noise has slowly increased to above specifications
            if ( cdate >= '20090804' .and. cdate < '20100317' ) iuse = -1
            ! noise stayed high for channel 3
            if ( cdate >= '20100317' ) iuse(3) = -1
         end if   ! end if noaa-19-mhs
      end select
   end if   ! end if noaa

   if ( trim(platform_name) == 'eos' ) then
      select case ( satid )
      case ( 2 )
         if ( trim(sensor_name) == 'amsua' ) then
            iuse(7) = -1
            !to be checked if ( cdate > '20071001' )
            ! channel 5 noise has slowly increased too much
            if ( cdate >= '20100502' ) iuse(5) = -1
         end if   ! end if eos-2-amsua
         if ( trim(sensor_name) == 'airs' ) then
            ! airs data first appear on 2002102200
            if ( cdate < '20030401' ) iuse = -1    ! NCEP 200211
            ! data missing for a few days
            if ( cdate >= '20100101' .and. cdate <= '20100104' ) iuse = -1
            ! instrument anomaly: stand-by mode on 20100109, instrument restart on 20100121,
            ! data flow resumed on 20100128
            if ( cdate >= '20100109' .and. cdate < '20100128' ) iuse = -1
            if ( cdate >  '20070324' .and. cdate < '20070403' ) iuse = -1
            ! preventive blacklist of airs from 31 Oct 2003 onwards (solar storm)
            if ( cdate > '20031030' .and. cdate < '20031202' ) iuse = -1
         end if   ! end if eos-2-airs
      end select
   end if   ! end if eos

   if ( trim(platform_name) == 'metop' ) then
      select case ( satid )
      case ( 2 )
         if ( trim(sensor_name) == 'hirs' ) then
            ! first metop-2 hirs data arrived 20061130 12UTC
            if ( cdate <= '20061129' ) then
               iuse = -1
            else
               ! a few noisy days in Jan 2007
               if ( cdate < '20070201' ) iuse = -1
            end if
         end if   ! end if metop-2-hirs
         if ( trim(sensor_name) == 'amsua' ) then
            ! first metop-2 data arrived 20061102 00UTC for 3 days, then
            ! data gap util 20061129 12UTC
            ! NCEP 200706
            if ( cdate <= '20061128' ) then
               iuse = -1
            else
               !to be checked
               ! calculate varbc coefficients at the beginning and during
               ! a recalibration period
               if ( cdate < '20070111' .or. &
                    (cdate > '20070521' .and. cdate < '20070528') ) then
                  iuse = -1
               end if
            end if
            ! channel 7 gone too noisy
            if ( cdate > '20090105' ) iuse(7) = -1
         end if   ! end if metop-2-amsua
         if ( trim(sensor_name) == 'mhs' ) then
            ! first metop-2 data arrived 20061130 12UTC
            if ( cdate <= '20061129' ) then
               iuse = -1
            else
               !to be checked, calculate varbc coefficients
               if ( cdate < '20070111' ) then
                  iuse = -1
               end if
            end if
         end if   ! end if metop-2-mhs
      end select
   end if   ! end if metop

!============
! from NCEP
!============
   if ( trim(platform_name) == 'noaa' ) then
      select case ( satid )
      case ( 15 )
         if ( trim(sensor_name) == 'hirs' ) then
            if ( cdate <  '19981101' ) iuse = -1
            if ( cdate >= '20000701' ) iuse = -1
         end if   ! end if noaa-15-hirs
         if ( trim(sensor_name) == 'amsua' ) then
            if ( cdate <  '19981101' ) iuse = -1
            if ( cdate >= '20001101' ) iuse(11) = -1
         end if   ! end if noaa-15-amsua
         if ( trim(sensor_name) == 'amsub' ) then
            if ( cdate <  '20000201' ) iuse = -1
            if ( cdate >= '20060301' ) iuse(4) = -1
         end if   ! end if noaa-15-amsub
      case ( 16 )
         if ( trim(sensor_name) == 'hirs' ) then
            if ( cdate <  '20010201' ) iuse = -1
            if ( cdate >= '20040601' ) iuse = -1
         end if   ! end if noaa-16-hirs
         if ( trim(sensor_name) == 'amsua' ) then
            if ( cdate <  '20010201' ) iuse = -1
            if ( cdate >= '20050201' ) iuse(9:13) = -1
         end if   ! end if noaa-16-amsua
         if ( trim(sensor_name) == 'amsub' ) then
            if ( cdate <  '20010201' ) iuse = -1
         end if   ! end if noaa-16-amsub
      case ( 17 )
         if ( trim(sensor_name) == 'hirs' ) then
            if ( cdate <  '20021001' ) iuse = -1
         end if   ! end if noaa-17-hirs
         if ( trim(sensor_name) == 'amsua' ) then
            iuse = -1
         end if   ! end if noaa-17-amsua
         if ( trim(sensor_name) == 'amsub' ) then
            if ( cdate <  '20021001' ) iuse = -1
         end if   ! end if noaa-17-amsub
      case ( 18 )
         if ( trim(sensor_name) == 'hirs' ) then
            iuse = -1
         end if   ! end if noaa-18-hirs
         if ( trim(sensor_name) == 'amsua' ) then
            if ( cdate <  '20051101' ) iuse = -1
            if ( cdate >= '20071201' ) iuse(9) = -1
         end if   ! end if noaa-18-amsua
         if ( trim(sensor_name) == 'mhs' ) then
            if ( cdate < '20051101' ) iuse = -1
         end if   ! end if noaa-18-mhs
      case ( 19 )
         if ( trim(sensor_name) == 'amsua' ) then
         end if   ! end if noaa-19-amsua
         if ( trim(sensor_name) == 'mhs' ) then
         end if   ! end if noaa-19-mhs
      end select
   end if   ! end if noaa

   if ( trim(platform_name) == 'eos' ) then
      select case ( satid )
      case ( 2 )
         if ( trim(sensor_name) == 'amsua' ) then
            iuse(7) = -1
            if ( cdate < '20021101' ) iuse = -1
         end if   ! end if eos-2-amsua
         if ( trim(sensor_name) == 'airs' ) then
            if ( cdate < '20021101' ) iuse = -1
         end if   ! end if eos-2-airs
      end select
   end if   ! end if eos

   if ( trim(platform_name) == 'metop' ) then
      select case ( satid )
      case ( 2 )
         if ( trim(sensor_name) == 'hirs' ) then
            if ( cdate < '20070601' ) iuse = -1
         end if   ! end if metop-2-hirs
         if ( trim(sensor_name) == 'amsua' ) then
            if ( cdate < '20070601' ) iuse = -1
         end if   ! end if metop-2-amsua
         if ( trim(sensor_name) == 'mhs' ) then
            if ( cdate < '20070601' ) iuse = -1
         end if   ! end if metop-2-mhs
      end select
   end if   ! end if metop

end subroutine da_blacklist_rad
