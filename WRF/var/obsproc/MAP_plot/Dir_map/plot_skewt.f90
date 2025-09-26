subroutine plot_skewt(title,sound, num, qc0, n)
!
  USE DA_Define_Structures

  implicit none
!
  integer                ,                  intent(in) :: num, qc0, n
  type (multi_level_type), DIMENSION (num), intent(in) :: sound
  character*(*),                            intent(in) :: title
!
  real, parameter     :: badval = -888888.0, &
                         WOFFS  = 0.           ! Wind barb off the XM
  integer, parameter  :: LW   = 2000, & ! line width
                         ICTL = 2,    & ! color for T
                         ICDL = 5,    & ! color for Td
                         ICWL = 8       ! color for wind

  include    '../include/maps.incl'
  include    '../include/nestdmn.incl'

  integer                    :: i, k, mdate, iyr, imn, idy, ihr, &
                                num_ob, jend, iend, k_levels 
  real                       :: xtime, xlc, ylc, qv, uu, vv, rh, &
                                Xir, xwt, xet, Xsh, Xnh, Dxn
  real, dimension(max_sound) :: pres, temp, dwpt, wspd, wdir
  character (len=40)         :: istat

  print '(/a,1x,"num=",i4," qc=",i3," dm_id=",i2)', title, num, qc0, n
  print '("size of sound=",i5)', size(sound)

!
! model domain settings:
!
         JEND = (NESTJX(1)-1)*NRATIO(N) + 1  
         IEND = (NESTIX(1)-1)*NRATIO(N) + 1 
         Dxn  = DIS(N)
         Xir  = FLOAT(NRATIO(N))
         Xwt  = (XWEST(N) - FLOAT(JOFFST) - 1.)*XIR + 1.    
         Xet  = (XEAST(N) - FLOAT(JOFFST) - 1.)*XIR + 1.   
         Xsh  = (XSOUTH(N) -FLOAT(IOFFST) - 1.)*XIR + 1. 
         Xnh  = (XNORTH(N) -FLOAT(IOFFST) - 1.)*XIR + 1.  
         print 100, Title(1:10), n, Dxn, Xir, Xwt, Xet, Xsh, Xnh,  &
                    NESTJX(n), NESTIX(n), Jend, Iend
100      format(/15X,'===> PLOTTING SKEWT of ',a,' for Domain',I2, &
                /'  Dxn, Xir, Xwt, Xet, Xsh, Xnh:',6F8.2,          &
                 '  JX, IX, Jend,iend:',4I5)
!
         num_ob = 0
!
site_loop:do i = 1, num
         write(0,'(a,1x,"date_char=",a,"xxz")') sound(i) % info % id, &
                       sound(i) % info % date_char
 
         istat = sound(i) % info % id
         read(sound(i) % info % date_char, &
           '(2X,I2,1X,I2,1X,I2,1X,I2,1X,F2.0)') IYR,IMN,IDY,IHR, Xtime
         Mdate = 1000000*IYR + 10000*IMN + 100*IDY + IHR 
         CALL LLXY(sound(i) % info % lat, &
                   sound(i) % info % lon, Xlc,Ylc, iend, jend, Dxn)
         Xlc = Xlc - Xwt + 1.0
         Ylc = Ylc - Xsh + 1.0
         if (Xlc < 1.0 .or. Xlc > real(NESTJX(n)) .or.             &
             Ylc < 1.0 .or. Ylc > real(NESTIX(n)) ) then
             cycle site_loop
         else
             num_ob = num_ob + 1
         endif
         print 101, num_ob, i, istat(1:6), mdate, xlc, ylc,         &
                    sound(i) % info % lat, sound(i) % info % lon, &
                    sound(i) % info % elv
101      format('N=',I3,' I=',I3,'  ID=',A,2X,I8,'  x,y:',2F8.2,  &
                        '  LAT.LON/ELV:', 2F10.3,F10.1)

         k_levels = 0
         do k = 1, sound(i) % info % levels
           pres(k) = sound(i) % each(k) % pressure % data
           if (abs(pres(k)-badval) > .1 .and. &
               sound(i) % each(k) % pressure % qc >= qc0) then
               pres(k) = pres(k)/100.
           else
               pres(k) = badval
           endif

           temp(k) = sound(i) % each(k) % temperature % data
           if (abs(temp(k)-badval) > .1 .and. &
               sound(i) % each(k) % temperature % qc >= qc0) then
               temp(k) = temp(k)-273.16
           else
               temp(k) = badval
           endif

! Dew point calculation
!          qv      = sound(i) % each(k) % q % data
!          if (sound(i) % each(k) % q % qc < qc0) qv = badval
!          if (.not.(abs(Temp(k)-badval) <.1 .or. &
!                    abs(Pres(k)-badval) <.1 .or. &
!                    abs(qv-badval) <.1 )) then
!              CALL TTDRHQ(Temp(k),dwpt(k),Pres(k),RH,Qv,3)
!          else
!              dwpt(k) = badval
!          endif

           dwpt(k) = sound(i) % each(k) % dew_point % data -273.16
           if (abs(dwpt(k)-badval) > .1 .and. &
               sound(i) % each(k) % dew_point % qc >= qc0) then
               dwpt(k) = sound(i) % each(k) % dew_point % data -273.16
           else
               dwpt(k) = badval
           endif

! Wind speed and direction calcultations
!          uu      = sound(i) % each(k) % u % data
!          vv      = sound(i) % each(k) % v % data
!          if (sound(i) % each(k) % u % qc < qc0) uu = badval
!          if (sound(i) % each(k) % v % qc < qc0) vv = badval
!          if (.not.(abs(uu-badval) <.1 .and. &
!                    abs(vv-badval) <.1)) then
!              CALL FFDDUV(wspd(k), wdir(k), Uu, Vv, &
!                          sound(i) % info % lon,    &
!                          XLONC, PHIC, IPROJ, 0, 2)
!          else
!              wspd(k) = badval
!              wdir(k) = badval
!          endif

           wspd(k) = sound(i) % each(k) % speed % data
           wdir(k) = sound(i) % each(k) % direction % data

           if ((abs(wspd(k)-badval) <.1) .or. (abs(wdir(k)-badval) <.1) .OR. &
               (sound(i) % each(k) % speed     % qc < qc0)  .OR. &
               (sound(i) % each(k) % direction % qc < qc0)) THEN

               wspd(k) = badval
               wdir(k) = badval
           else
              wspd(k) = sound(i) % each(k) % speed     % data
              wdir(k) = sound(i) % each(k) % direction % data
           endif

! .. Check if there is meaningful data levels:
           if (temp(k).ne.badval .or. wspd(k).ne.badval) &
              k_levels = k_levels + 1

!           print 102,k,pres(k),temp(k),dwpt(k),wspd(k),wdir(k)
102        format('k=',I3,' p,t,td,ws,wd:',5F10.2)
       
         enddo

         if (k_levels > 0)                                           &
           CALL SKEWT(PRES, TEMP, DWPT, WSPD, WDIR,                  &
                      sound(i) % info % levels, title,               &
                      MDATE, Xtime, istat, xlc, ylc,                 &
                      sound(i) % info % lat, sound(i) % info % lon,  &
                      LW, ICTL,ICDL,ICWL, .TRUE., WOFFS, badval)
      enddo site_loop

end subroutine plot_skewt
