!-----------------------------------------------------------------------
!
! Contains all subroutines to get time-varying mixing ratios of CO2, 
! N2O, CH4, CFC11 and CFC12 into radiation schemes.
! These subroutines enable the user to specify the mixing ratios
! in the file CAMtr_volume_mixing_ratio, giving the user an easy way
! to specify trace gases concentrations.
!
! The subroutines were first developed by and put together in this module
! by C. Carouge.
!-----------------------------------------------------------------------
MODULE module_ra_clWRF_support

  USE module_wrf_error

  IMPLICIT NONE
  PRIVATE
  INTEGER, PARAMETER               :: r8 = 8
  integer, parameter               :: cyr = 1800     ! maximum num. of lines in 'CAMtr_volume_mixing_ratio' file
  integer, DIMENSION(1:cyr), SAVE  :: yrdata
  real, DIMENSION(1:cyr), SAVE  :: juldata
  real(r8), DIMENSION(1:cyr), SAVE :: co2r, n2or, ch4r, cfc11r, cfc12r


  ! Same values as in module_ra_cam_support.F
  integer, parameter               :: VARCAM_in_years = 233
  integer                          :: yr_CAM(1:VARCAM_in_years) = &
       (/ 1869, 1870, 1871, 1872, 1873, 1874, 1875, &
       1876, 1877, 1878, 1879, 1880, 1881, 1882, &
       1883, 1884, 1885, 1886, 1887, 1888, 1889, &
       1890, 1891, 1892, 1893, 1894, 1895, 1896, &
       1897, 1898, 1899, 1900, 1901, 1902, 1903, &
       1904, 1905, 1906, 1907, 1908, 1909, 1910, &
       1911, 1912, 1913, 1914, 1915, 1916, 1917, &
       1918, 1919, 1920, 1921, 1922, 1923, 1924, &
       1925, 1926, 1927, 1928, 1929, 1930, 1931, &
       1932, 1933, 1934, 1935, 1936, 1937, 1938, &
       1939, 1940, 1941, 1942, 1943, 1944, 1945, &
       1946, 1947, 1948, 1949, 1950, 1951, 1952, &
       1953, 1954, 1955, 1956, 1957, 1958, 1959, &
       1960, 1961, 1962, 1963, 1964, 1965, 1966, &
       1967, 1968, 1969, 1970, 1971, 1972, 1973, &
       1974, 1975, 1976, 1977, 1978, 1979, 1980, &
       1981, 1982, 1983, 1984, 1985, 1986, 1987, &
       1988, 1989, 1990, 1991, 1992, 1993, 1994, &
       1995, 1996, 1997, 1998, 1999, 2000, 2001, &
       2002, 2003, 2004, 2005, 2006, 2007, 2008, &
       2009, 2010, 2011, 2012, 2013, 2014, 2015, &
       2016, 2017, 2018, 2019, 2020, 2021, 2022, &
       2023, 2024, 2025, 2026, 2027, 2028, 2029, &
       2030, 2031, 2032, 2033, 2034, 2035, 2036, &
       2037, 2038, 2039, 2040, 2041, 2042, 2043, &
       2044, 2045, 2046, 2047, 2048, 2049, 2050, &
       2051, 2052, 2053, 2054, 2055, 2056, 2057, &
       2058, 2059, 2060, 2061, 2062, 2063, 2064, &
       2065, 2066, 2067, 2068, 2069, 2070, 2071, &
       2072, 2073, 2074, 2075, 2076, 2077, 2078, &
       2079, 2080, 2081, 2082, 2083, 2084, 2085, &
       2086, 2087, 2088, 2089, 2090, 2091, 2092, &
       2093, 2094, 2095, 2096, 2097, 2098, 2099, &
       2100, 2101                               /)
  real                             :: co2r_CAM(1:VARCAM_in_years) = &
       (/ 289.263, 289.263, 289.416, 289.577, 289.745, 289.919, 290.102, &
       290.293, 290.491, 290.696, 290.909, 291.129, 291.355, 291.587, 291.824, &
       292.066, 292.313, 292.563, 292.815, 293.071, 293.328, 293.586, 293.843, &
       294.098, 294.35, 294.598, 294.842, 295.082, 295.32, 295.558, 295.797,   &
       296.038, 296.284, 296.535, 296.794, 297.062, 297.338, 297.62, 297.91,   &
       298.204, 298.504, 298.806, 299.111, 299.419, 299.729, 300.04, 300.352,  &
       300.666, 300.98, 301.294, 301.608, 301.923, 302.237, 302.551, 302.863,  &
       303.172, 303.478, 303.779, 304.075, 304.366, 304.651, 304.93, 305.206,  &
       305.478, 305.746, 306.013, 306.28, 306.546, 306.815, 307.087, 307.365,  &
       307.65, 307.943, 308.246, 308.56, 308.887, 309.228, 309.584, 309.956,   &
       310.344, 310.749, 311.172, 311.614, 312.077, 312.561, 313.068, 313.599, &
       314.154, 314.737, 315.347, 315.984, 316.646, 317.328, 318.026, 318.742, &
       319.489, 320.282, 321.133, 322.045, 323.021, 324.06, 325.155, 326.299,  &
       327.484, 328.698, 329.933, 331.194, 332.499, 333.854, 335.254, 336.69,  &
       338.15, 339.628, 341.125, 342.65, 344.206, 345.797, 347.397, 348.98,    &
       350.551, 352.1, 354.3637, 355.7772, 357.1601, 358.5306, 359.9046,       &
       361.4157, 363.0445, 364.7761, 366.6064, 368.5322, 370.534, 372.5798,    &
       374.6564, 376.7656, 378.9087, 381.0864, 383.2994, 385.548, 387.8326,    &
       390.1536, 392.523, 394.9625, 397.4806, 400.075, 402.7444, 405.4875,     &
       408.3035, 411.1918, 414.1518, 417.1831, 420.2806, 423.4355, 426.6442,   &
       429.9076, 433.2261, 436.6002, 440.0303, 443.5168, 447.06, 450.6603,     &
       454.3059, 457.9756, 461.6612, 465.3649, 469.0886, 472.8335, 476.6008,   &
       480.3916, 484.2069, 488.0473, 491.9184, 495.8295, 499.7849, 503.7843,   &
       507.8278, 511.9155, 516.0476, 520.2243, 524.4459, 528.7127, 533.0213,   &
       537.3655, 541.7429, 546.1544, 550.6005, 555.0819, 559.5991, 564.1525,   &
       568.7429, 573.3701, 578.0399, 582.7611, 587.5379, 592.3701, 597.2572,   &
       602.1997, 607.1975, 612.2507, 617.3596, 622.524, 627.7528, 633.0616,    &
       638.457, 643.9384, 649.505, 655.1568, 660.8936, 666.7153, 672.6219,     &
       678.6133, 684.6945, 690.8745, 697.1569, 703.5416, 710.0284, 716.6172,   &
       723.308, 730.1008, 736.9958, 743.993, 751.0975, 758.3183, 765.6594,     &
       773.1207, 780.702, 788.4033, 796.2249, 804.1667, 812.2289, 820.4118,    &
       828.6444, 828.6444 /)

  PUBLIC :: read_CAMgases

CONTAINS
  
  SUBROUTINE read_CAMgases(yr, julian, model, co2vmr, n2ovmr, ch4vmr, cfc11vmr, cfc12vmr) 

    INTEGER, INTENT(IN)            :: yr
    REAL, INTENT(IN)               :: julian
    CHARACTER(LEN=*), INTENT(IN)   :: model           ! Radiation scheme name
    REAL(r8), OPTIONAL, INTENT(OUT)    :: co2vmr, n2ovmr, ch4vmr, cfc11vmr, cfc12vmr

!Local
    
    INTEGER                                          :: yearIN, found_yearIN, iyear  &
                                                       ,yr1,yr2
    INTEGER                                         :: mondata(1:cyr)
    LOGICAL, EXTERNAL                                :: wrf_dm_on_monitor
    INTEGER, EXTERNAL                                :: get_unused_unit
      
    INTEGER                                          :: istatus, iunit, idata        
    INTEGER, SAVE                             :: VARCAM_in_years
    integer                                          :: nyrm, nyrp, njulm, njulp
    LOGICAL                                          :: exists
    LOGICAL, SAVE                                    :: READtrFILE=.FALSE.
    CHARACTER(LEN=256)                               :: message
    INTEGER                                   :: monday(13)=(/0,31,28,31,30,31,30,31,31,30,31,30,31/)
    INTEGER                                   :: mondayi(13)
    INTEGER                                   :: my1,my2,my3, tot_valid
    
! CLWRF-UC June.09  (Copy from share/wrf_tsin.F)
    IF ( .NOT. READtrFILE ) THEN
       READtrFILE= .TRUE.
       
       INQUIRE(FILE='CAMtr_volume_mixing_ratio', EXIST=exists)
       
       IF (exists) THEN
          iunit = get_unused_unit()
          IF ( iunit <= 0 ) THEN
             IF ( wrf_dm_on_monitor() ) THEN
                CALL wrf_error_fatal('Error in module_ra_rrtm: could not find a free Fortran unit.')
             END IF
          END IF
          
          ! Read volume mixing ratio 
          OPEN(UNIT=iunit, FILE='CAMtr_volume_mixing_ratio', FORM='formatted', &
               STATUS='old', IOSTAT=istatus)
          
          IF (istatus == 0) THEN
             ! Ignore first two lines which constitute a header
             READ(UNIT=iunit, FMT='(1X)')
             READ(UNIT=iunit, FMT='(1X)')
             
             istatus = 0
             idata = 1
             DO WHILE (istatus == 0)
                READ(UNIT=iunit, FMT='(I4, 1x, F8.3,1x, 4(F10.3,1x))', IOSTAT=istatus)    &
                     yrdata(idata), co2r(idata), n2or(idata), ch4r(idata), cfc11r(idata), &
                     cfc12r(idata)
                IF ( wrf_dm_on_monitor() ) THEN
                   WRITE(message,*)'CLWRF reading...: istatus:',istatus,' idata:',idata,   &
                        ' year:', yrdata(idata), ' co2: ',co2r(idata), ' n2o: ',&
                        n2or(idata),' ch4:',ch4r(idata)
                   call wrf_debug( 0, message) 
                ENDIF
                mondata(idata) = 6
                
                idata=idata+1
             END DO

             IF (istatus /= -1) THEN
                PRINT *,'CLWRF -- clwrf -- CLWRF ALERT!'
                PRINT *,"   Not normal ending of 'CAMtr_volume_mixing_ratio' file"
                PRINT *,"   Lecture ends with 'IOSTAT'=",istatus
             END IF
             VARCAM_in_years = idata - 1
             CLOSE(iunit)

             ! Calculate the julian day for each month.
             DO idata=1,VARCAM_in_years
                mondayi = monday
                MY1=MOD(yrdata(idata),4)
                MY2=MOD(yrdata(idata),100)
                MY3=MOD(yrdata(idata),400)
                IF(MY1.EQ.0.AND.MY2.NE.0.OR.MY3.EQ.0) mondayi(3)=29
                juldata(idata) = sum(mondayi(1:mondata(idata)))+real(mondayi(mondata(idata)+1))/2.-0.5   ! 1st Jan 00:00 = 0 julian day
             ENDDO
          ENDIF
       ELSE
          PRINT *,"CLWRF -- clwrf -- CLWRF ALERT!"
          PRINT *,"   'CAMtr_volume_mixing_ratio' does not exists"
          PRINT *,'   Recovering original concentrations'
          
          !ccc Fake yrdata. Won't be use but a value > yr is needed so that
          ! everything work.
          ! With negative mixing ratios in co2r, n2or and ch4r, no interpolation
          ! is done and we use the constant mixing ratios that were used before
          ! CLWRF modifications.
          yrdata = yr + 1
          juldata = 1
          co2r   = -9999.999
          n2or   = -9999.999
          ch4r   = -9999.999
          cfc11r = -9999.999
          cfc12r = -9999.999

          ! For CAM model, recovers original data sets.
          IF (model .eq. "CAM") THEN
             yrdata(1:VARCAM_in_years) = yr_CAM
             co2r(1:VARCAM_in_years)   = co2r_CAM
          ENDIF
       ENDIF ! CAMtr_volume_mixing_ratio exists
       
    ENDIF ! File already opened and read 
    
    found_yearIN=0
    iyear=1
    !ccc Crash if iyear get > cyr (max. # of years in the mixing ratio file) ?
    !DO WHILE (found_yearIN == 0) 
    DO WHILE (found_yearIN == 0 .and. iyear <= cyr)
       IF (yrdata(iyear) .GT. yr )  THEN
          yearIN=iyear
          found_yearIN=1
       ELSE IF ((yrdata(iyear) .EQ. yr) .AND. (juldata(iyear) .GT. julian)) THEN
          yearIN=iyear
          found_yearIN = 1
       ENDIF
       iyear=iyear+1
    ENDDO
    
    ! Prevent yr > last year in data
!    IF (yearIN .ge. VARCAM_in_years) yearIN=VARCAM_in_years-1
    IF (iyear .ge. VARCAM_in_years) then
       yearIN=VARCAM_in_years-1
       found_yearIN = 1
    ENDIF
    
    IF (found_yearIN .NE. 0 ) THEN
       if (yearIN .eq. 1) yearIN = yearIN + 1    ! To take 2 first lines of the file
       nyrm = yrdata(yearIN-1)
       njulm = juldata(yearIN-1)
       nyrp = yrdata(yearIN)
       njulp = juldata(yearIN)
    ENDIF

    IF (PRESENT(co2vmr)) THEN
       co2vmr=-9999.999
       if (found_yearIN /= 0) then
          ! Interpolate data only if we have at least 2 valid concentrations.
          tot_valid = count(co2r(1:VARCAM_in_years) > 0)
          IF (tot_valid >= 2 ) THEN
             CALL valid_years(yearIN, co2r, VARCAM_in_years,yr1, yr2)

             ! Set nyrm, njulm, nyrp, njulp
             nyrm = yrdata(yr1)
             njulm = juldata(yr1)
             nyrp = yrdata(yr2)
             njulp = juldata(yr2)

             CALL interpolate_CAMgases(yr, julian, nyrm, njulm, yr1, yr2, nyrp, njulp, co2r, co2vmr)
          ENDIF
       endif
       ! Verification of interpolated values. In case of no value 
       ! original values extracted from ghg_surfvals.F90 module

       IF (co2vmr < 0. .or. found_yearIN == 0) THEN
          CALL orig_val("CO2",model,co2vmr)
       ELSE
          ! If extrapolation, need to bound the data to pre-industrial concentrations
          if (co2vmr < 270.) co2vmr = 270.
          co2vmr=co2vmr*1.e-06
       END IF
    ENDIF

    IF (PRESENT(n2ovmr)) THEN
       n2ovmr=-9999.999
       if (found_yearIN /= 0) then
          tot_valid = count(n2or(1:VARCAM_in_years) > 0)
          IF (tot_valid >= 2 ) THEN
             CALL valid_years(yearIN, n2or, VARCAM_in_years,yr1, yr2)

             ! Set nyrm, njulm, nyrp, njulp
             nyrm = yrdata(yr1)
             njulm = juldata(yr1)
             nyrp = yrdata(yr2)
             njulp = juldata(yr2)

             CALL interpolate_CAMgases(yr, julian, nyrm, njulm, yr1, yr2, nyrp, njulp, n2or, n2ovmr)
          ENDIF
       endif
       
       IF (n2ovmr < 0. .or. found_yearIN == 0) THEN
          CALL orig_val("N2O",model,n2ovmr)
       ELSE
          ! If extrapolation, need to bound the data to pre-industrial concentrations
          if (n2ovmr < 270.) n2ovmr = 270.          
          n2ovmr=n2ovmr*1.e-09
       ENDIF
       
    ENDIF
    
    IF (PRESENT(ch4vmr)) THEN
       ch4vmr=-9999.999
       if (found_yearIN /= 0) then
          tot_valid = count(ch4r(1:VARCAM_in_years) > 0)
          IF (tot_valid >= 2 ) THEN
             CALL valid_years(yearIN, ch4r, VARCAM_in_years,yr1, yr2)

             ! Set nyrm, njulm, nyrp, njulp
             nyrm = yrdata(yr1)
             njulm = juldata(yr1)
             nyrp = yrdata(yr2)
             njulp = juldata(yr2)

             CALL interpolate_CAMgases(yr, julian, nyrm, njulm, yr1, yr2, nyrp, njulp, ch4r, ch4vmr)
          endif
       endif
       
       IF (ch4vmr < 0. .or. found_yearIN == 0) THEN
          CALL orig_val("CH4",model,ch4vmr)
       ELSE
          ! If extrapolation, need to bound the data to pre-industrial concentrations
          if (ch4vmr < 700. ) ch4vmr = 700.
          ch4vmr=ch4vmr*1.e-09
       ENDIF
    ENDIF
    
    IF (PRESENT(cfc11vmr)) THEN
       cfc11vmr = -9999.999
       if (found_yearIN /= 0) then
          tot_valid = count(cfc11r(1:VARCAM_in_years) > 0)
          IF (tot_valid >= 2 ) THEN
             CALL valid_years(yearIN, cfc11r, VARCAM_in_years,yr1, yr2)

             ! Set nyrm, njulm, nyrp, njulp
             nyrm = yrdata(yr1)
             njulm = juldata(yr1)
             nyrp = yrdata(yr2)
             njulp = juldata(yr2)

             CALL interpolate_CAMgases(yr, julian, nyrm, njulm, yr1, yr2, nyrp, njulp, cfc11r, cfc11vmr)
          endif
       endif
       
       IF (cfc11vmr < 0. .or. found_yearIN == 0) THEN
          CALL orig_val("CFC11",model,cfc11vmr)
       ELSE
          cfc11vmr=cfc11vmr*1.e-12
       ENDIF
    ENDIF
    
    IF (PRESENT(cfc12vmr)) THEN
       cfc12vmr = -9999.999
       if (found_yearIN /= 0) then
          tot_valid = count(cfc12r(1:VARCAM_in_years) > 0)
          IF (tot_valid >= 2 ) THEN
             CALL valid_years(yearIN, cfc12r, VARCAM_in_years,yr1, yr2)

             ! Set nyrm, njulm, nyrp, njulp
             nyrm = yrdata(yr1)
             njulm = juldata(yr1)
             nyrp = yrdata(yr2)
             njulp = juldata(yr2)

             CALL interpolate_CAMgases(yr, julian, nyrm, njulm, yr1, yr2, nyrp, njulp, cfc12r, cfc12vmr)
          endif
       endif
       
       IF (cfc12vmr < 0. .or. found_yearIN == 0) THEN
          CALL orig_val("CFC12",model,cfc12vmr)
       ELSE
          cfc12vmr=cfc12vmr*1.e-12
       ENDIF
    ENDIF
       
  END SUBROUTINE read_CAMgases
  
  SUBROUTINE valid_years(yearIN, gas, tot_years, yr1, yr2)

! Find 
    INTEGER, INTENT(IN) :: yearIN, tot_years
    INTEGER, INTENT(OUT) :: yr2, yr1
    REAL(r8), INTENT(IN) :: gas(:)

    ! Local variables
    INTEGER :: yr_loc, idata
    

    yr_loc = yearIN
    yr2 = yr_loc
    yr1 = yr_loc-1
    
    ! If all valid dates are > yearIN then find the 2 lowest dates with
    ! valid data.
    IF (count(gas(1:yr_loc-1) > 0.) == 0) THEN
!ccc       DO idata = yr_loc-1, tot_years-1
       DO idata = yr_loc, tot_years-1
          IF (gas(idata) > 0.) THEN
             yr1 = idata
             EXIT
          ENDIF
       ENDDO
!ccc       DO idata = yr1, tot_years
       DO idata = yr1+1, tot_years
          IF (gas(idata) > 0.) THEN
             yr2 = idata
             EXIT
          ENDIF
       ENDDO
    ELSE  ! There is at least 1 valid year below yearIN
       IF (gas(yr_loc) < 0.) THEN
          
          ! Find the closest valid year, look for higher year first
          IF (any(gas(yr_loc:tot_years) > 0)) THEN
             DO idata=yr_loc+1, tot_years
                IF (gas(idata) > 0) THEN
                   yr2 = idata
                   exit
                ENDIF
             ENDDO
          ELSE  ! Need to take lower years and extrapolate data.
             DO idata=yr_loc-1,2,-1
                IF (gas(idata) > 0) THEN
                   yr2 = idata
                   exit
                ENDIF
             ENDDO
          ENDIF
       ENDIF
       
       yr_loc = min(yr_loc-1, yr2-1)
       IF (gas(yr_loc) < 0.) THEN
          
          ! Find the closest valid year, lower than yr1
          IF (any(gas(1:yr_loc-1) > 0)) THEN
             DO idata=yr_loc-1,1,-1
                IF (gas(idata) > 0) THEN
                   yr1 = idata
                   exit
                ENDIF
             ENDDO
          ELSE  ! Need to take higher years and extrapolate data.
             print*, 'Problem: this case should never happen'
          ENDIF
       ELSE ! Then yr1 is yr_loc (first valid date < yr2)
          yr1 = yr_loc
       ENDIF
    ENDIF
  END SUBROUTINE valid_years

  SUBROUTINE interpolate_CAMgases(yr, julian, yeari, juli, yr1, yr2, yearf, julf, gas, interp_gas)
    IMPLICIT NONE
! These subroutine interpolates a trace gas concentration from a non-homogeneously 
! distributed gas concentration evolution
    INTEGER, INTENT (IN)                      :: yr, yeari, yr1, yr2, yearf, juli, julf
    REAL, INTENT (IN)                         :: julian
    REAL(r8), DIMENSION(500), INTENT (IN)         :: gas
    REAL(r8), INTENT (OUT)                    :: interp_gas
!Local
    REAL(r8)                                  :: yearini, yearend, gas1, gas2
    REAL                                      :: doymodel, doydatam, doydatap,    &
                                                 deltat, fact1, fact2, x
    INTEGER                                   :: ny, my1,my2,my3,nday, maxyear, minyear
 
    
    ! Add support for leap-years

    ! Find smallest and largest year: yearf >= yeari since the file is ordered with increasing dates.
    minyear = MIN(yr,yeari)
    maxyear = MAX(yr,yearf)

    ! Initialise with the day in the year for each date.
    fact2 = juli
    fact1 = julf
    x = julian

    ! Calculate the julian day (day 0 = 1 Jan minyear at 00:00)
    DO ny=minyear, maxyear-1
       nday = 365
       ! Leap-year?
       MY1=MOD(ny,4)
       MY2=MOD(ny,100)
       MY3=MOD(ny,400)
       IF(MY1.EQ.0.AND.MY2.NE.0.OR.MY3.EQ.0) nday=366
       
       if (ny < yeari) then
          fact2 = fact2+nday
       endif
       if (ny < yr) then
          x = x+nday
       endif
       if (ny < yearf) then
          fact1 = fact1+nday
       endif
    enddo

    deltat = fact1-fact2
    fact1 = (fact1 - x)/deltat
    fact2 = (x - fact2)/deltat

    interp_gas = gas(yr1)*fact1+gas(yr2)*fact2 

    IF (interp_gas .LT. 0. ) THEN
       interp_gas=-99999.
    ENDIF
    
  END SUBROUTINE interpolate_CAMgases

  SUBROUTINE interpolate_lin(x1,y1,x2,y2,x0,y)
    IMPLICIT NONE
! Program to interpolate values y=a+b*x with:
!       a=y1
!       b=(y2-y1)/(x2-x1)
!       x=abs(x1-x0)

    REAL, INTENT (IN)                       :: x1,x2,x0
    REAL(r8), INTENT (IN)                   :: y1,y2
    REAL(r8), INTENT (OUT)                  :: y
    REAL(r8)                                :: a,b,x
    
    a=y1
    b=(y2-y1)/(x2-x1)
    
    IF (x0 .GE. x1) THEN
       x=x0-x1
    ELSE
       x=x1-x0
       b=-b
    ENDIF
    
    y=a+b*x
    
  END SUBROUTINE interpolate_lin


  SUBROUTINE orig_val(tracer,model,out)

    CHARACTER(LEN=*), INTENT(IN) :: tracer  ! The trace gas name
    CHARACTER(LEN=*), INTENT(IN) :: model   ! The radiation scheme name
    REAL(r8), INTENT(INOUT)      :: out     ! The output value
!Local
    LOGICAL, EXTERNAL            :: wrf_dm_on_monitor
    CHARACTER(LEN=256)           :: message


    IF (model .eq. "CAM") THEN
       IF (tracer .eq. "CO2") THEN
          out = 3.55e-4

       ELSE IF (tracer .eq. "N2O") THEN
          out = 0.311e-6

       ELSE IF (tracer .eq. "CH4") THEN
          out = 1.714e-6

       ELSE IF (tracer .eq. "CFC11") THEN
          out = 0.280e-9

       ELSE IF (tracer .eq. "CFC12") THEN
          out = 0.503e-9

       ELSE
          IF ( wrf_dm_on_monitor() ) THEN
             WRITE(message,*) 'CLWRF : Trace gas ',tracer,' not valid for scheme ',model
             CALL wrf_error_fatal(message)
          ENDIF
       ENDIF

    ELSE IF (model .eq. "RRTM") THEN
       IF (tracer .eq. "CO2") THEN
          out = 3.300e-4

       ELSE IF ((tracer .eq. "N2O") .or. &
                (tracer .eq. "CH4")) THEN
          out = 0.
       ELSE
          IF ( wrf_dm_on_monitor() ) THEN
             WRITE(message,*) 'CLWRF : Trace gas ',tracer,' not valid for scheme ',model
             CALL wrf_error_fatal(message)
          ENDIF
       ENDIF

    ELSE IF (model .eq. "RRTMG") THEN
       IF (tracer .eq. "CO2") THEN
          out = 379.e-6

       ELSE IF (tracer .eq. "N2O") THEN
          out = 319e-9

       ELSE IF (tracer .eq. "CH4") THEN
          out = 1774.e-9

       ELSE IF (tracer .eq. "CFC11") THEN
          out = 0.251e-9

       ELSE IF (tracer .eq. "CFC12") THEN
          out = 0.538e-9

       ELSE
          IF ( wrf_dm_on_monitor() ) THEN
             WRITE(message,*) 'CLWRF : Trace gas ',tracer,' not valid for scheme ',model
             CALL wrf_error_fatal(message)
          ENDIF
       ENDIF

    ELSE
       IF ( wrf_dm_on_monitor() ) THEN
          WRITE(message,*) 'CLWRF not implemented for the ',model,' radiative scheme.'
          CALL wrf_error_fatal(message)
       ENDIF
    ENDIF

  END SUBROUTINE orig_val

END MODULE module_ra_clWRF_support
