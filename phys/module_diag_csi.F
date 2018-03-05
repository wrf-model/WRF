!------------------------------------------------------------------------------
! NASA/GSFC, Computational and Information Science and Technology Office,
! Code 606
!------------------------------------------------------------------------------
!
! MODULE: module_diag_csi
!
! AUTHOR: 
! Eric Kemp, CISTO/SSAI (based on earlier version from Code 612)
!
! DESCRIPTION:
! Diagnoses stratiform and convective regions of precipitation.
!
! REVISION HISTORY:
! 18 December 2014 - Initial version - Eric Kemp
!------------------------------------------------------------------------------

module module_diag_csi

  implicit none
  private

  public :: cal_sepa_part1
  public :: cal_sepa_part2
  public :: cal_sepa_part3
  public :: var_sepa 

contains

  !----------------------------------------------------------------------------
  ! ROUTINE: 
  ! cal_sepa_part1
  !
  ! DESCRIPTION:
  ! First part of convective-stratiform diagnostic. Initialize variables
  ! for subsequent steps.
  !
  ! REVISION HISTORY:
  ! 18 December 2014 - Initial version - Eric Kemp
  !----------------------------------------------------------------------------

  subroutine cal_sepa_part1(dt_in,prrate,ri,it, &
       ids,ide, jds,jde, kds,kde,                     & ! domain dims
       ims,ime, jms,jme, kms,kme,                     & ! memory dims
       its,ite, jts,jte, kts,kte                      ) ! tile   dims

    implicit none

    ! Arguments
    integer,intent(in)               :: &
         ids,ide, jds,jde, kds,kde,  &
         ims,ime, jms,jme, kms,kme,  &
         its,ite, jts,jte, kts,kte
    real,intent(in)                  :: dt_in
    real,dimension( ims:ime , jms:jme ),                            &
         intent(in)               :: prrate
    real,dimension( ims:ime , jms:jme ),                            &
         intent(out)              :: ri
    integer,dimension( ims:ime, jms:jme ), &
         intent(out)              :: it

    ! Local variables
    integer :: i,j
    real :: dt_in_reciprocal

    dt_in_reciprocal = 1./dt_in
    do j=jts,jte
       do i=its,ite
          ri(i,j) = prrate(i,j) * 3600.0*1.0*dt_in_reciprocal ! in mm/hour
          it(i,j) = 0
       enddo ! i
    enddo ! j

  end subroutine cal_sepa_part1

  !----------------------------------------------------------------------------
  ! ROUTINE: 
  ! cal_sepa_part2
  !
  ! DESCRIPTION:
  ! Second part of convective-stratiform diagnostic. Two modes are available
  ! based on value of 'ipass' argument:
  ! ipass = 1: Classify convective areas based on local rainfall vs
  !            5x5 weighted average of area rainfall.
  ! ipass = 2: Classify convective areas based on local rainfall and proximity
  !            to nearby convection.
  ! Subroutine should be called twice with ipass=1 and then ipass=2. 
  ! If using WRF, a halo update should be made after each call to capture
  ! "ipass=1" convection detected on adjoining tiles--otherwise, final results
  ! will vary by tile size!
  !
  ! REVISION HISTORY:
  ! 18 December 2014 - Initial version - Eric Kemp
  !----------------------------------------------------------------------------

  subroutine cal_sepa_part2(ri,it,ipass, &
       ids,ide, jds,jde, kds,kde,                     & ! domain dims
       ims,ime, jms,jme, kms,kme,                     & ! memory dims
       its,ite, jts,jte, kts,kte                      ) ! tile   dims

    implicit none

     ! Arguments
    integer,intent(in)               :: &
         ids,ide, jds,jde, kds,kde,  &
         ims,ime, jms,jme, kms,kme,  &
         its,ite, jts,jte, kts,kte
    real,dimension( ims:ime , jms:jme ),                            &
         intent(in)                 :: ri
    integer,intent(in) :: ipass
    integer,dimension( ims:ime, jms:jme ), &
         intent(inout)              :: it

    ! Local variables
    integer :: i,j

    integer :: jpk,jp1,jp2,jm1,jm2
    integer :: ipk,ip1,ip2,im1,im2
    real :: rv
    real,parameter              :: C2D25   = 2./25.
    integer,dimension( its:ite, jts:jte ) :: it_new

    ! Two passes:
    ! Pass 1: Get convection based on local rainrate versus 5x5 weighted
    ! average of area rainfall.
    ! Pass 2: Get convection based on local rainrate and nearby convection.

    if (ipass .lt. 1 .or. &
        ipass .gt. 2) then
       call wrf_error_fatal('cal_sepa_part2: Illegal ipass argument!')
    end if

    do j=jts,jte
       
       jpk=j
       
       jp1=jpk+1
       jp2=jpk+2
       jm1=jpk-1
       jm2=jpk-2
       
       ! Assume zero gradient at lateral boundaries
       if(jm1.le.jds) jm1=jds
       if(jm2.le.jds) jm2=jds
       if(jp1.ge.jde-1) jp1=jde-1
       if(jp2.ge.jde-1) jp2=jde-1
       
       do i=its, ite
          ipk=i
          ip1=ipk+1
          ip2=ipk+2
          im1=ipk-1
          im2=ipk-2
          
          ! Assume zero gradient at lateral boundaries
          if(im1.le.ids) im1=ids
          if(im2.le.ids) im2=ids
          if(ip1.ge.ide-1) ip1=ide-1
          if(ip2.ge.ide-1) ip2=ide-1
          
          if (ipass .eq. 1) then
             ! Skip points with light rainfall
             if (ri(i,j) .lt. 0.001) cycle 

             ! Weighted average of area rainfall
             rv=ri(im2,jm2)+ri(im1,jm2)+ri(ipk,jm2)+ri(ip1,jm2)+ri(ip2,jm2)
             rv=rv+ri(im2,jm1)+ri(im1,jm1)+ri(ipk,jm1)+ri(ip1,jm1)+ri(ip2,jm1)
             rv=rv+ri(im2,jpk)+ri(im1,jpk)+ri(ipk,jpk)+ri(ip1,jpk)+ri(ip2,jpk)
             rv=rv+ri(im2,jp1)+ri(im1,jp1)+ri(ipk,jp1)+ri(ip1,jp1)+ri(ip2,jp1)
             rv=rv+ri(im2,jp2)+ri(im1,jp2)+ri(ipk,jp2)+ri(ip1,jp2)+ri(ip2,jp2)
             rv=C2D25*rv
             
             if (ri(ipk,jpk).ge.rv) then
                it(ipk,jpk)=3
             else
                if(it(ipk,jpk).ne.3) it(ipk,jpk)=2
             endif

          else if (ipass .eq. 2) then
             ! Set precipitating points adjacent to convective cores as 
             ! convective             
             it_new(ipk,jpk) = it(ipk,jpk) ! First guess
             
             ! Skip points with light local rainfall
             if (ri(ipk,jpk) .lt. 0.01) cycle
             
             ! Nearby convection test
             if ( it(im1,jm1) .eq. 3 .or. &
                  it(ipk,jm1) .eq. 3 .or. &
                  it(ip1,jm1) .eq. 3 .or. &
                  it(im1,jpk) .eq. 3 .or. &
                  it(ipk,jpk) .eq. 3 .or. &
                  it(ip1,jpk) .eq. 3 .or. &
                  it(im1,jp1) .eq. 3 .or. &
                  it(ipk,jp1) .eq. 3 .or. &
                  it(ip1,jp1) .eq. 3 ) then
                it_new(ipk,jpk)=3
             end if
          end if
       enddo !i
    enddo !j

    ! Important! Update the array with latest convection detection.
    if (ipass .eq. 2) then
       do j = jts,jte
          do i = its,ite
             it(i,j) = it_new(i,j)
          end do
       end do
    end if
    
  end subroutine cal_sepa_part2

  !----------------------------------------------------------------------------
  ! ROUTINE: 
  ! cal_sepa_part3
  !
  ! DESCRIPTION:
  ! Third part of convective-stratiform diagnostic. Calculate CSI index.
  ! Generalized for different microphysics schemes (requires cloud water and
  ! rain, but will also accept cloud ice, snow, graupel, and/or hail if
  ! available).
  !
  ! REVISION HISTORY:
  ! 18 December 2014 - Initial version - Eric Kemp
  !----------------------------------------------------------------------------

  subroutine cal_sepa_part3(w,qc,qr,qi,qs,qg,qh,th,   &
       rhobar, ri, it, csi, pii,                      &
       ids,ide, jds,jde, kds,kde,                     & ! domain dims
       ims,ime, jms,jme, kms,kme,                     & ! memory dims
       its,ite, jts,jte, kts,kte                      ) ! tile   dims

    implicit none

    ! Arguments
    integer,intent(in)               :: &
         ids,ide, jds,jde, kds,kde,     &
         ims,ime, jms,jme, kms,kme,     &
         its,ite, jts,jte, kts,kte
    real,dimension( ims:ime, jms:jme ), &
         intent(in)               :: ri
    real,dimension( ims:ime , kms:kme , jms:jme ),                  &
         intent(in)               :: qc, qr
    real,dimension( ims:ime , kms:kme , jms:jme ),                  &
         optional, intent(in)               :: qi, qs, qg, qh
    real,dimension( ims:ime , kms:kme , jms:jme ),                  &
         intent(in)               :: w, th, pii, rhobar
    integer,dimension( ims:ime, jms:jme ), &
         intent(inout)               :: it
    real,    dimension( ims:ime , jms:jme ),                            &
         intent(out)               :: csi

    !  Local variables
    integer :: lconv,lanvl,lnspt
    real    :: atotal5,atotal15,aco5,aco15,aan5,aan15

    integer :: i,j,k
    integer :: jpk,jp1,jp2,jm1,jm2
    integer :: ipk,ip1,ip2,im1,im2

    integer,dimension( its:ite, jts:jte, 4 ) :: ics5
    integer,dimension( its:ite, jts:jte ) :: jv

    real              :: tairc
    real              :: rianmax
    real              :: bigw
    real              :: wbig
    real              :: qcbig, qcbig1
    real              :: rzm, y3
    real              :: a1,a2,a3,a4,a5,a6
    integer           :: icloudw, icloudl, icloud 
    integer          :: icwwl, icwwu

    !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    !                                                                       
    !  Beginning of executable code...                                      
    !                                                                       
    !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

    do j=jts,jte
       do i=its,ite
          csi(i,j) = 0
          jv(i,j)=0
       enddo ! i
    enddo ! j

    !  ***  find the location and value of max sfc rainfall grid point
    rianmax=20.
    do j=jts, jte
       do i=its, ite
          if (ri(i,j) .ge. rianmax) it(i,j)=3
          if (ri(i,j) .lt. 0.01) it(i,j)=1
       enddo !i
    enddo !j

    !     ***  tao & simpson (1989) and tao et al (1991, 1993)   **************
    !     ****   search for the largest value of w and cloud water
    !   wbig=300.
    wbig=3.
    qcbig= 0.50e-3
    qcbig1=1.00e-3

    do j = jts, jte
       do i = its, ite
          icloudw=0
          icloudl=0
          icloud=0
          icwwl=0
          icwwu=0
          icwwu=0
          do  k = kts, kte
!             tairc=th(i,k,j)*pii(i,k,j)-t0
             tairc=th(i,k,j)*pii(i,k,j)-273.16
             !c  ***   lower cloudy region
             if (tairc .ge. 0.0) then
                if (qc(i,k,j) .ge. qcbig) icloudw=1
                if (qc(i,k,j) .ge. qcbig1) icloudl=1
                if (w(i,k,j) .ge. wbig) icwwl=1
             endif
             !c  ***   middle and upper cloudy region
             if (tairc .lt. 0.0) then
                if (w(i,k,j) .ge. wbig) icwwu=1
             endif
             !                rzm=rhobar(i,j,k)*1.e6*0.001
             RZM=1.
             ! Sum is flexible for different microphysics schemes
!             y3=rzm*(qc(i,k,j)+qi(i,k,j)+qr(i,k,j)+qs(i,k,j)+qg(i,k,j))
             y3=(qc(i,k,j)+qr(i,k,j))
             if (present(qi)) y3 = y3 + qi(i,k,j)
             if (present(qs)) y3 = y3 + qs(i,k,j)
             if (present(qg)) y3 = y3 + qg(i,k,j)
             if (present(qh)) y3 = y3 + qh(i,k,j)
             y3 = rzm*y3
             !                if (y3 .ge. .01) icloud=1
             if (y3 .ge. 0.1E-3) icloud=1
          enddo !k

          !   ***  middle-upper level w > 0.5*wbig m/s or low-level qc > 1 g/kg
          if (it(i,j).eq.1) then
             if (icwwl .eq. 1 .and. icloudw .eq. 1) it(i,j)=3
          endif
          if (it(i,j).eq.2) then
             if (icloudl .eq. 1) it(i,j)=3
             if (icwwu .eq. 1) it(i,j)=3
          endif
          if (it(i,j).eq.1 .and. icloud.eq.1) jv(i,j)=1

       enddo !i
    enddo !j

    !c   ***   ********************************************************
    !c   ***   it(i,j)=3  convective region
    !c   ***   it(i,j)=2  stratiform region
    !c   ***   it(i,j)=4  stratiform region but no sfc precipitation
    !c   ***   it(i,j)=1  cloud free region

    a1=0.0
    a2=0.0
    a3=0.0
    a4=0.0
    a5=0.0
    a6=0.0

    atotal5=0.0
    atotal15=0.0
    aco5=0.0
    aan5=0.0
    aco15=0.0
    aan15=0.0

    lconv=0
    lanvl=0
    lnspt=0

    do j = jts, jte
       do i = its, ite

          a5=a5+ri(i,j)
          a6=a6+qr(i,2,j)

          if (it(i,j) .eq. 2) then
             csi(i,j)=3
             ics5(i,j,3)=1
             lanvl=lanvl+1
             a2=a2+ri(i,j)
             a3=a3+qr(i,2,j)
          endif

          if (it(i,j) .eq. 3) then
             csi(i,j)=2
             ics5(i,j,2)=1
             lconv=lconv+1
             a1=a1+ri(i,j)
             a4=a4+qr(i,2,j)
          endif

          if (it(i,j) .eq. 1 ) then
             if  (jv(i,j) .eq. 1) then
                csi(i,j)=4
                ics5(i,j,4)=1
                it(i,j)=4
                lnspt=lnspt+1
             else
                csi(i,j)=1
                ics5(i,j,1)=1
             endif
          endif

       enddo !i
    enddo !j

    do j = jts, jte
       do i = its, ite
          if (csi(i,j) .eq. 4) ics5(i,j,4) = 1
          if (csi(i,j) .eq. 2) ics5(i,j,2) = 1
          if (csi(i,j) .eq. 3) ics5(i,j,3) = 1
       enddo !i
    enddo !j

    atotal5=atotal5+a5
    atotal15=atotal15+a6
    aco5=aco5+a1
    aan5=aan5+a2
    aco15=aco15+a4
    aan15=aan15+a3

    !   print *, 'Convective points=',lconv
    !   print *, 'Stratiform points=',lanvl
    !   print *, 'No-precip Stratiform points=',lnspt
    !   print *, 'Convective fractional coverage=',float(lconv)/float((nx-1)*(ny-1))*100.
    !   print *, 'Stratiform fractional coverage=',float(lanvl)/float((nx-1)*(ny-1))*100.
    !   print *, 'No-precip stratiform fractional coverage=',float(lnspt)/float((nx-1)*(ny-1))*100.
    !   print *, 'Convective precip fractional amount=', aco5/atotal5*100.
    !   print *, 'Stratiform  precip fractional amount=',aan5/atotal5*100.
    !   print *, 'Convective precip(QR) fractional amount=',aco15/atotal15*100.

    !  CALL  wrigar(csi,1,nx,1,ny,1,1,1,nx-1,1,ny-1,1,1,           &
    !             'csi x-y',0.0, 1 )                               

  end subroutine cal_sepa_part3

  !----------------------------------------------------------------------------
  ! ROUTINE: 
  ! var_sepa
  !
  ! DESCRIPTION:
  ! Calculate precipitation rate broken down by convective-stratiform
  ! classification.
  !
  ! REVISION HISTORY:
  ! 18 December 2014 - Initial version - Eric Kemp
  !----------------------------------------------------------------------------

  subroutine var_sepa(rainncv, csi,                    &
       rainncv_sepa, rainnc_sepa,        &
       ims,ime, jms,jme, kms,kme,                     & ! memory dims
       its,ite, jts,jte, kts,kte                      ) ! tile   dims

    implicit none

    ! Arguments
    integer, intent(in) :: ims,ime, jms,jme, kms,kme
    integer, intent(in) :: its,ite, jts,jte, kts,kte
    real,    dimension( ims:ime , jms:jme ),                            &
         intent(in )               :: rainncv, csi
    real,    dimension( ims:ime , 4, jms:jme ),                         &
         intent(out )               :: rainncv_sepa, rainnc_sepa

    ! Local variables
    integer :: i,j,k
    integer :: idx

    do i=its, ite
       do k=1,4
          do j=jts, jte
             rainncv_sepa(i,k,j)=0.0
          enddo !j
       enddo !k
    enddo !i

    do i=its, ite
       do j=jts, jte
          idx=csi(i,j)
          if (idx .gt. 0) then
             rainncv_sepa(i,idx,j)=rainncv(i,j)
             rainnc_sepa(i,idx,j)=rainnc_sepa(i,idx,j)+rainncv_sepa(i,idx,j)
          endif
       enddo !j
    enddo !i

  end subroutine var_sepa

end module module_diag_csi
