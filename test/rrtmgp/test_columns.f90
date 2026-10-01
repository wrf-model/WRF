PROGRAM test_rrtmgp_columns
  USE, INTRINSIC :: ieee_arithmetic, ONLY: ieee_is_finite
  USE mo_gas_optics_constants, ONLY: cp_dry, grav
  USE module_ra_rrtmgp, ONLY: rrtmgp_init, rrtmgp_lw_column, rrtmgp_sw_column
  IMPLICIT NONE
  INTEGER, PARAMETER :: nc=1, nl=3, nv=nl+1
  CHARACTER(LEN=512) :: data_path
  REAL :: play(nc,nl),plev(nc,nv),tlay(nc,nl),tlev(nc,nv),tsfc(nc)
  REAL :: h2o(nc,nl),co2(nc,nl),o3(nc,nl),n2o(nc,nl),ch4(nc,nl),o2(nc,nl)
  REAL :: emis(nc,1),cf(nc,nl),lwp(nc,nl),iwp(nc,nl),swp(nc,nl)
  REAL :: rel(nc,nl),rei(nc,nl),res(nc,nl)
  REAL :: lwup(nc,nv),lwdn(nc,nv),lwhr(nc,nl),lwupc(nc,nv),lwdnc(nc,nv),lwhrc(nc,nl)
  REAL :: swup(nc,nv),swdn(nc,nv),swhr(nc,nl),swupc(nc,nv),swdnc(nc,nv),swhrc(nc,nl)
  REAL :: direct(nc,nv),diffuse(nc,nv),directc(nc,nv)
  REAL :: visdir(nc,nv),visdif(nc,nv),nirdir(nc,nv),nirdif(nc,nv)
  REAL :: avdir(nc),avdif(nc),andir(nc),andif(nc),mu0(nc),solar
  REAL :: saved_up(nc,nv),saved_dn(nc,nv),saved_hr(nc,nl)
  REAL :: clear_lwup(nc,nv),clear_lwdn(nc,nv),clear_swup(nc,nv),clear_swdn(nc,nv)
  REAL :: saved_direct(nc,nv),saved_diffuse(nc,nv),saved_visdir(nc,nv),saved_nirdif(nc,nv)
  INTEGER :: overlap

  CALL get_command_argument(1,data_path)
  IF(LEN_TRIM(data_path)==0) ERROR STOP 'usage: test_rrtmgp_columns DATA_DIRECTORY'

  ! The surface is interface 1; pressure decreases toward the top of atmosphere.
  plev(1,:)=[1000.,700.,300.,1.]
  play(1,:)=[850.,500.,150.]
  tlay(1,:)=[285.,260.,230.]
  tlev(1,:)=[290.,275.,245.,210.]
  tsfc=290.
  h2o(1,:)=[.01,.003,.0001]
  co2=420.e-6; o3(1,:)=[.5e-6,1.e-6,5.e-6]
  n2o=330.e-9; ch4=1.8e-6; o2=.2095
  emis=.98
  cf=0.; lwp=0.; iwp=0.; swp=0.; rel=10.; rei=30.; res=30.
  avdir=.15; avdif=.10; andir=.25; andif=.20
  solar=1361.; mu0=.65

  CALL rrtmgp_init(TRIM(data_path))
  CALL rrtmgp_lw_column(play,plev,tlay,tlev,tsfc,h2o,co2,o3,n2o,ch4,o2,emis, &
       cf,lwp,iwp,swp,rel,rei,res,4,2,173,lwup,lwdn,lwhr,lwupc,lwdnc,lwhrc)
  CALL check_finite('LW clear',lwup,lwdn,lwhr)
  CALL check_close('LW clear/all up',lwup,lwupc,1.e-5)
  CALL check_close('LW clear/all down',lwdn,lwdnc,1.e-5)
  CALL check_heating('LW',plev,lwup,lwdn,lwhr)
  clear_lwup=lwup; clear_lwdn=lwdn

  CALL rrtmgp_sw_column(play,plev,tlay,h2o,co2,o3,n2o,ch4,o2,avdir,avdif,andir,andif,mu0,solar, &
       cf,lwp,iwp,swp,rel,rei,res,4,2,173,swup,swdn,swhr,swupc,swdnc,swhrc, &
       direct,diffuse,directc,visdir,visdif,nirdir,nirdif)
  CALL check_finite('SW clear',swup,swdn,swhr)
  CALL check_close('SW clear/all up',swup,swupc,1.e-5)
  CALL check_close('SW clear/all down',swdn,swdnc,1.e-5)
  CALL check_heating('SW',plev,swup,swdn,swhr)
  clear_swup=swup; clear_swdn=swdn
  CALL check_close('SW direct plus diffuse',direct+diffuse,swdn,2.e-5)
  CALL check_close('SW visible plus near infrared direct',visdir+nirdir,direct,2.e-5)
  CALL check_close('SW visible plus near infrared diffuse',visdif+nirdif,diffuse,2.e-5)

  ! A fully overcast liquid middle layer exercises cloud optics and sampling.
  cf(1,2)=1.; lwp(1,2)=100.
  CALL rrtmgp_lw_column(play,plev,tlay,tlev,tsfc,h2o,co2,o3,n2o,ch4,o2,emis, &
       cf,lwp,iwp,swp,rel,rei,res,4,2,991,lwup,lwdn,lwhr,lwupc,lwdnc,lwhrc)
  CALL check_finite('LW cloudy',lwup,lwdn,lwhr)
  CALL check_close('LW cloudy clear-sky up',lwupc,clear_lwup,1.e-5)
  CALL check_close('LW cloudy clear-sky down',lwdnc,clear_lwdn,1.e-5)
  CALL check_heating('LW cloudy',plev,lwup,lwdn,lwhr)
  saved_up=lwup; saved_dn=lwdn; saved_hr=lwhr
  CALL rrtmgp_lw_column(play,plev,tlay,tlev,tsfc,h2o,co2,o3,n2o,ch4,o2,emis, &
       cf,lwp,iwp,swp,rel,rei,res,4,2,991,lwup,lwdn,lwhr,lwupc,lwdnc,lwhrc)
  CALL check_close('LW seed reproducibility up',lwup,saved_up,0.)
  CALL check_close('LW seed reproducibility down',lwdn,saved_dn,0.)
  CALL check_close('LW seed reproducibility heating',lwhr,saved_hr,0.)

  CALL rrtmgp_sw_column(play,plev,tlay,h2o,co2,o3,n2o,ch4,o2,avdir,avdif,andir,andif,mu0,solar, &
       cf,lwp,iwp,swp,rel,rei,res,4,2,991,swup,swdn,swhr,swupc,swdnc,swhrc, &
       direct,diffuse,directc,visdir,visdif,nirdir,nirdif)
  CALL check_finite('SW cloudy',swup,swdn,swhr)
  CALL check_close('SW cloudy clear-sky up',swupc,clear_swup,1.e-5)
  CALL check_close('SW cloudy clear-sky down',swdnc,clear_swdn,1.e-5)
  IF(.NOT.(swdn(1,1)<swdnc(1,1))) CALL fail('cloud did not lower surface SW down flux')
  CALL check_heating('SW cloudy',plev,swup,swdn,swhr)
  CALL check_close('SW cloud direct plus diffuse',direct+diffuse,swdn,2.e-5)
  CALL check_close('SW cloud visible plus near infrared direct',visdir+nirdir,direct,2.e-5)
  CALL check_close('SW cloud visible plus near infrared diffuse',visdif+nirdif,diffuse,2.e-5)
  saved_up=swup; saved_dn=swdn; saved_hr=swhr; saved_direct=direct; saved_diffuse=diffuse
  saved_visdir=visdir; saved_nirdif=nirdif
  CALL rrtmgp_sw_column(play,plev,tlay,h2o,co2,o3,n2o,ch4,o2,avdir,avdif,andir,andif,mu0,solar, &
       cf,lwp,iwp,swp,rel,rei,res,4,2,991,swup,swdn,swhr,swupc,swdnc,swhrc, &
       direct,diffuse,directc,visdir,visdif,nirdir,nirdif)
  CALL check_close('SW seed reproducibility up',swup,saved_up,0.)
  CALL check_close('SW seed reproducibility down',swdn,saved_dn,0.)
  CALL check_close('SW seed reproducibility heating',swhr,saved_hr,0.)
  CALL check_close('SW seed reproducibility direct',direct,saved_direct,0.)
  CALL check_close('SW seed reproducibility diffuse',diffuse,saved_diffuse,0.)
  CALL check_close('SW seed reproducibility visible direct',visdir,saved_visdir,0.)
  CALL check_close('SW seed reproducibility near infrared diffuse',nirdif,saved_nirdif,0.)

  ! Night shortcut must zero broadband, direct, diffuse, and band partitions.
  mu0=0.
  CALL rrtmgp_sw_column(play,plev,tlay,h2o,co2,o3,n2o,ch4,o2,avdir,avdif,andir,andif,mu0,solar, &
       cf,lwp,iwp,swp,rel,rei,res,4,2,991,swup,swdn,swhr,swupc,swdnc,swhrc, &
       direct,diffuse,directc,visdir,visdif,nirdir,nirdif)
  IF(ANY(swup/=0.).OR.ANY(swdn/=0.).OR.ANY(swhr/=0.).OR.ANY(swupc/=0.).OR.ANY(swdnc/=0.).OR. &
     ANY(swhrc/=0.).OR.ANY(direct/=0.).OR.ANY(diffuse/=0.).OR.ANY(directc/=0.).OR. &
     ANY(visdir/=0.).OR.ANY(visdif/=0.).OR.ANY(nirdir/=0.).OR.ANY(nirdif/=0.)) CALL fail('night fluxes are nonzero')

  ! Exercise mixed liquid, ice, and snow with partial cloud in all overlap modes.
  ! Mode zero must preserve the clear result even when cloud fields are populated.
  DO overlap=0,3
    CALL exercise_overlap(overlap)
  END DO

  WRITE(*,'(A)') 'RRTMGP column smoke/regression checks passed.'
CONTAINS
  SUBROUTINE exercise_overlap(overlap_mode)
    INTEGER, INTENT(IN) :: overlap_mode
    CHARACTER(LEN=40) :: label
    WRITE(label,'("mixed-phase overlap ",I0)') overlap_mode
    mu0=.65
    cf(1,:)=[.4,.55,.7]
    lwp(1,:)=[15.,100.,40.]
    iwp(1,:)=[40.,20.,75.]
    swp(1,:)=[25.,60.,10.]

    CALL rrtmgp_lw_column(play,plev,tlay,tlev,tsfc,h2o,co2,o3,n2o,ch4,o2,emis, &
         cf,lwp,iwp,swp,rel,rei,res,4,overlap_mode,619,lwup,lwdn,lwhr,lwupc,lwdnc,lwhrc)
    CALL check_finite(TRIM(label)//' LW',lwup,lwdn,lwhr)
    CALL check_close(TRIM(label)//' LW clear up',lwupc,clear_lwup,1.e-5)
    CALL check_close(TRIM(label)//' LW clear down',lwdnc,clear_lwdn,1.e-5)
    CALL check_heating(TRIM(label)//' LW all sky',plev,lwup,lwdn,lwhr)
    CALL check_heating(TRIM(label)//' LW clear sky',plev,lwupc,lwdnc,lwhrc)
    IF(overlap_mode==0) THEN
      CALL check_close(TRIM(label)//' LW cloud-free up',lwup,clear_lwup,1.e-5)
      CALL check_close(TRIM(label)//' LW cloud-free down',lwdn,clear_lwdn,1.e-5)
    END IF
    saved_up=lwup; saved_dn=lwdn; saved_hr=lwhr
    CALL rrtmgp_lw_column(play,plev,tlay,tlev,tsfc,h2o,co2,o3,n2o,ch4,o2,emis, &
         cf,lwp,iwp,swp,rel,rei,res,4,overlap_mode,619,lwup,lwdn,lwhr,lwupc,lwdnc,lwhrc)
    CALL check_close(TRIM(label)//' LW repeatability',lwup,saved_up,0.)
    CALL check_close(TRIM(label)//' LW repeatability',lwdn,saved_dn,0.)
    CALL check_close(TRIM(label)//' LW repeatability',lwhr,saved_hr,0.)

    CALL rrtmgp_sw_column(play,plev,tlay,h2o,co2,o3,n2o,ch4,o2,avdir,avdif,andir,andif,mu0,solar, &
         cf,lwp,iwp,swp,rel,rei,res,4,overlap_mode,619,swup,swdn,swhr,swupc,swdnc,swhrc, &
         direct,diffuse,directc,visdir,visdif,nirdir,nirdif)
    CALL check_finite(TRIM(label)//' SW',swup,swdn,swhr)
    CALL check_close(TRIM(label)//' SW clear up',swupc,clear_swup,1.e-5)
    CALL check_close(TRIM(label)//' SW clear down',swdnc,clear_swdn,1.e-5)
    CALL check_heating(TRIM(label)//' SW all sky',plev,swup,swdn,swhr)
    CALL check_heating(TRIM(label)//' SW clear sky',plev,swupc,swdnc,swhrc)
    CALL check_close(TRIM(label)//' SW direct plus diffuse',direct+diffuse,swdn,2.e-5)
    CALL check_close(TRIM(label)//' SW visible plus near-infrared direct',visdir+nirdir,direct,2.e-5)
    CALL check_close(TRIM(label)//' SW visible plus near-infrared diffuse',visdif+nirdif,diffuse,2.e-5)
    IF(overlap_mode==0) THEN
      CALL check_close(TRIM(label)//' SW cloud-free up',swup,clear_swup,1.e-5)
      CALL check_close(TRIM(label)//' SW cloud-free down',swdn,clear_swdn,1.e-5)
    ELSE IF(.NOT.(swdn(1,1)<swdnc(1,1))) THEN
      CALL fail(TRIM(label)//' did not reduce surface SW down flux')
    END IF
    saved_up=swup; saved_dn=swdn; saved_hr=swhr; saved_direct=direct
    CALL rrtmgp_sw_column(play,plev,tlay,h2o,co2,o3,n2o,ch4,o2,avdir,avdif,andir,andif,mu0,solar, &
         cf,lwp,iwp,swp,rel,rei,res,4,overlap_mode,619,swup,swdn,swhr,swupc,swdnc,swhrc, &
         direct,diffuse,directc,visdir,visdif,nirdir,nirdif)
    CALL check_close(TRIM(label)//' SW repeatability',swup,saved_up,0.)
    CALL check_close(TRIM(label)//' SW repeatability',swdn,saved_dn,0.)
    CALL check_close(TRIM(label)//' SW repeatability',swhr,saved_hr,0.)
    CALL check_close(TRIM(label)//' SW repeatability',direct,saved_direct,0.)
  END SUBROUTINE exercise_overlap

  SUBROUTINE fail(message)
    CHARACTER(LEN=*), INTENT(IN) :: message
    WRITE(*,'(A)') 'FAIL: '//TRIM(message)
    ERROR STOP 1
  END SUBROUTINE fail

  SUBROUTINE check_close(label,a,b,tol)
    CHARACTER(LEN=*), INTENT(IN) :: label
    REAL, INTENT(IN) :: a(:,:),b(:,:),tol
    REAL :: scale
    IF(.NOT.ALL(ieee_is_finite(a)).OR..NOT.ALL(ieee_is_finite(b))) CALL fail(TRIM(label)//' is non-finite')
    scale=MAX(1.,MAXVAL(ABS(a)),MAXVAL(ABS(b)))
    IF(MAXVAL(ABS(a-b))>tol*scale) THEN
      WRITE(*,'(A,ES12.4)') TRIM(label)//' max error: ',MAXVAL(ABS(a-b))
      CALL fail(label)
    END IF
  END SUBROUTINE check_close

  SUBROUTINE check_finite(label,a,b,c)
    CHARACTER(LEN=*), INTENT(IN) :: label
    REAL, INTENT(IN) :: a(:,:),b(:,:),c(:,:)
    IF(.NOT.ALL(ieee_is_finite(a)).OR..NOT.ALL(ieee_is_finite(b)).OR..NOT.ALL(ieee_is_finite(c))) &
      CALL fail(TRIM(label)//' has non-finite values')
  END SUBROUTINE check_finite

  SUBROUTINE check_heating(label,p,up,dn,hr)
    CHARACTER(LEN=*), INTENT(IN) :: label
    REAL, INTENT(IN) :: p(:,:),up(:,:),dn(:,:),hr(:,:)
    REAL :: expected(nc,nl)
    INTEGER :: layer
    DO layer=1,nl
      expected(:,layer)=((up(:,layer+1)-up(:,layer))-(dn(:,layer+1)-dn(:,layer)))* &
           grav*86400./(cp_dry*100.*(p(:,layer+1)-p(:,layer)))
    END DO
    CALL check_close(TRIM(label)//' flux/heating energy consistency',hr,expected,2.e-5)
  END SUBROUTINE check_heating
END PROGRAM test_rrtmgp_columns
