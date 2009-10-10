!
!WRF:MEDIATION_LAYER:IO
!
! Written by Zhan Zhang, Jan. 8, 2008

subroutine jdn_sec(jdn_in_sec,yy,mm,dd,hr,mi,se)
!Function: Converting from the Gregorian date to the Julian day number
      implicit none
! Local data
      integer :: yy,mm,dd,hr,mi,se
      integer (kind=8) :: jdn
      real(kind=8) :: jdn_in_sec
 
      jdn=(1461*(yy+4800+(mm-14)/12))/4+(367*(mm-2-12*((mm-14)/12)))/12-  &
       (3*((yy+4900+(mm-14)/12)/100))/4+dd-32075 
      jdn=(jdn*(24*3600)+hr*3600+mi*60+se)
      jdn_in_sec = jdn*1.d00
 
      return
end subroutine jdn_sec

subroutine jdn_ymd_hms(jdn_in_sec,y,m,d,hr,mi,se)
!
!   FUNCTION- 
!     Converting from the Julian day number to the Gregorian date
!
      implicit none
! Local data
      integer :: y,m,d,l,n,i,j,hr,mi,se
      integer :: hms,temp
      real(kind=8) :: jdn_in_sec,day_in_sec
      integer(kind=8) :: jdn
!
      day_in_sec = 24*3600
      hms=dmod(jdn_in_sec, day_in_sec)
      jdn=(jdn_in_sec-hms)/day_in_sec

      l=jdn+68569
      n=(4*l)/146097
      l=l-(146097*n+3)/4
      i=(4000*(l+1))/1461001
      l=l-(1461*i)/4+31
      j=(80*l)/2447
      d=l-(2447*j)/80
      l=j/11
      m=j+2-(12*l)
      y=100*(n-49)+i+l

      if(hms .ge. 3600) then
      temp=mod(hms,3600)
      hr=(hms-temp)/3600
      else
      temp=hms
      hr=0
      endif

      if(temp .ge. 60) then
      temp=mod(temp,60)
      mi=(hms-temp)/60
      else
      mi=0
      endif

      se=hms-hr*3600-mi*60

      return
end subroutine jdn_ymd_hms
