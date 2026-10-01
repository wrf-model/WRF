! WRF adapter for the UFS-pinned RTE+RRTMGP CPU implementation.
! The legacy WRF column preparation supplies hPa, VMR, g/m2 and micrometers.
! Results deliberately use the legacy K/day contract; WRF converts to theta/s.
MODULE module_ra_rrtmgp
  USE mo_rte_kind, ONLY: wp, i8
  USE mo_gas_optics_rrtmgp, ONLY: ty_gas_optics_rrtmgp
  USE mo_gas_concentrations, ONLY: ty_gas_concs
  USE mo_optical_props, ONLY: ty_optical_props_1scl, ty_optical_props_2str
  USE mo_source_functions, ONLY: ty_source_func_lw
  USE mo_fluxes, ONLY: ty_fluxes_broadband
  USE mo_fluxes_byband, ONLY: ty_fluxes_byband
  USE mo_rte_lw, ONLY: rte_lw
  USE mo_rte_sw, ONLY: rte_sw
  USE mo_heating_rates, ONLY: compute_heating_rate
  USE mo_cloud_optics_rrtmgp, ONLY: ty_cloud_optics_rrtmgp
  USE mo_cloud_sampling, ONLY: sampled_mask_max_ran, draw_samples
  USE mo_load_coefficients, ONLY: load_and_init
  USE mo_load_cloud_coefficients, ONLY: load_cld_lutcoeff
  IMPLICIT NONE
  PRIVATE
  EXTERNAL :: wrf_error_fatal3
  PUBLIC :: rrtmgp_init, rrtmgp_lw_column, rrtmgp_sw_column
  TYPE(ty_gas_optics_rrtmgp), SAVE :: gas_lw, gas_sw
  TYPE(ty_cloud_optics_rrtmgp), SAVE :: cloud_lw, cloud_sw
  LOGICAL, SAVE :: ready = .FALSE.
  CHARACTER(LEN=3), PARAMETER :: names(6) = ['h2o','co2','o3 ','n2o','ch4','o2 ']
CONTAINS
  SUBROUTINE require(msg)
    CHARACTER(LEN=*), INTENT(IN) :: msg
    IF (LEN_TRIM(msg)>0) CALL wrf_error_fatal3('module_ra_rrtmgp.F',0,'RRTMGP: '//TRIM(msg))
  END SUBROUTINE

  SUBROUTINE rrtmgp_init(data_path)
    CHARACTER(LEN=*), INTENT(IN) :: data_path
    TYPE(ty_gas_concs) :: gases
    IF (ready) RETURN
    CALL require(gases%init(names))
    CALL load_and_init(gas_lw,TRIM(data_path)//'/rrtmgp-gas-lw-g128.nc',gases)
    CALL load_and_init(gas_sw,TRIM(data_path)//'/rrtmgp-gas-sw-g112.nc',gases)
    CALL load_cld_lutcoeff(cloud_lw,TRIM(data_path)//'/rrtmgp-clouds-lw-bnd.nc')
    CALL load_cld_lutcoeff(cloud_sw,TRIM(data_path)//'/rrtmgp-clouds-sw-bnd.nc')
    IF (.NOT.cloud_lw%bands_are_equal(gas_lw)) CALL require('LW cloud/gas spectral bands differ')
    IF (.NOT.cloud_sw%bands_are_equal(gas_sw)) CALL require('SW cloud/gas spectral bands differ')
    ready = .TRUE.
  END SUBROUTINE

  SUBROUTINE set_gases(gases,h2o,co2,o3,n2o,ch4,o2)
    TYPE(ty_gas_concs), INTENT(INOUT) :: gases
    REAL, INTENT(IN) :: h2o(:,:),co2(:,:),o3(:,:),n2o(:,:),ch4(:,:),o2(:,:)
    CALL require(gases%init(names))
    CALL require(gases%set_vmr('h2o',REAL(h2o,wp)))
    CALL require(gases%set_vmr('co2',REAL(co2,wp)))
    CALL require(gases%set_vmr('o3',REAL(o3,wp)))
    CALL require(gases%set_vmr('n2o',REAL(n2o,wp)))
    CALL require(gases%set_vmr('ch4',REAL(ch4,wp)))
    CALL require(gases%set_vmr('o2',REAL(o2,wp)))
  END SUBROUTINE

  SUBROUTINE cloud_mask(cf,ngpt,overlap,seed,mask)
    REAL, INTENT(IN) :: cf(:,:)
    INTEGER, INTENT(IN) :: ngpt,overlap,seed
    LOGICAL, INTENT(OUT) :: mask(SIZE(cf,1),SIZE(cf,2),ngpt)
    REAL(wp) :: randoms(ngpt,SIZE(cf,2),SIZE(cf,1))
    INTEGER(i8) :: state
    INTEGER :: c,k,g
    IF (overlap<0.OR.overlap>3) CALL require('cloud overlap supported: 0, 1, 2, 3')
    state=MODULO(INT(seed,i8),2147483646_i8)+1_i8
    DO c=1,SIZE(cf,1)
      DO k=1,SIZE(cf,2)
        DO g=1,ngpt
          state=MODULO(48271_i8*state,2147483647_i8)
          randoms(g,k,c)=REAL(state,wp)/2147483647._wp
        END DO
      END DO
    END DO
    SELECT CASE(overlap)
    CASE(0)
      mask=.FALSE.
    CASE(2)
      CALL require(sampled_mask_max_ran(randoms,REAL(cf,wp),mask))
    CASE DEFAULT
      DO c=1,SIZE(cf,1)
        DO k=1,SIZE(cf,2)
          DO g=1,ngpt
            IF(overlap==3) THEN
              mask(c,k,g)=randoms(g,1,c)>1._wp-REAL(cf(c,k),wp)
            ELSE
              mask(c,k,g)=randoms(g,k,c)>1._wp-REAL(cf(c,k),wp)
            END IF
          END DO
        END DO
      END DO
    END SELECT
  END SUBROUTINE

  ! The public ice LUT uses effective diameter. WRF's Fu dge=1.0315*r_ec;
  ! Thompson supplies effective radius directly. Convert both to diameter.
  FUNCTION ice_diameter(re,iceflag) RESULT(d)
    REAL, INTENT(IN) :: re(:,:)
    INTEGER, INTENT(IN) :: iceflag
    REAL(wp) :: d(SIZE(re,1),SIZE(re,2))
    d=2._wp*REAL(re,wp)
    IF(iceflag==3) d=d/1.0315_wp
  END FUNCTION

  SUBROUTINE rrtmgp_lw_column(play,plev,tlay,tlev,tsfc,h2o,co2,o3,n2o,ch4,o2,emis, &
                             cf,lwp,iwp,swp,rel,rei,res,iceflag,overlap,seed, &
                             up,dn,hr,upc,dnc,hrc)
    REAL, INTENT(IN) :: play(:,:),plev(:,:),tlay(:,:),tlev(:,:),tsfc(:),emis(:,:)
    REAL, INTENT(IN) :: h2o(:,:),co2(:,:),o3(:,:),n2o(:,:),ch4(:,:),o2(:,:)
    REAL, INTENT(IN) :: cf(:,:),lwp(:,:),iwp(:,:),swp(:,:),rel(:,:),rei(:,:),res(:,:)
    INTEGER, INTENT(IN) :: iceflag,overlap,seed
    REAL, INTENT(OUT) :: up(:,:),dn(:,:),hr(:,:),upc(:,:),dnc(:,:),hrc(:,:)
    TYPE(ty_gas_concs) :: gases
    TYPE(ty_optical_props_1scl) :: atmos,clouds,snow,sampled
    TYPE(ty_source_func_lw) :: source
    TYPE(ty_fluxes_broadband) :: flux
    REAL(wp), TARGET :: fu(SIZE(plev,1),SIZE(plev,2)),fd(SIZE(plev,1),SIZE(plev,2))
    REAL(wp) :: heat(SIZE(play,1),SIZE(play,2)),zero(SIZE(play,1),SIZE(play,2))
    REAL(wp) :: emissivity(gas_lw%get_nband(),SIZE(play,1))
    REAL(wp) :: rl(SIZE(play,1),SIZE(play,2)),di(SIZE(play,1),SIZE(play,2)),ds(SIZE(play,1),SIZE(play,2))
    LOGICAL :: mask(SIZE(play,1),SIZE(play,2),gas_lw%get_ngpt())
    INTEGER :: nc,nl
    IF(.NOT.ready) CALL require('initialization must precede parallel column calls')
    nc=SIZE(play,1); nl=SIZE(play,2); zero=0._wp
    IF(SIZE(emis,2)==1) THEN
      emissivity=SPREAD(REAL(emis(:,1),wp),1,gas_lw%get_nband())
    ELSE IF(SIZE(emis,2)==gas_lw%get_nband()) THEN
      emissivity=TRANSPOSE(REAL(emis,wp))
    ELSE
      CALL require('surface emissivity must be grey or match LW bands')
    END IF
    CALL set_gases(gases,h2o,co2,o3,n2o,ch4,o2)
    CALL require(atmos%alloc_1scl(nc,nl,gas_lw))
    CALL require(source%alloc(nc,nl,gas_lw))
    CALL require(gas_lw%gas_optics(REAL(play,wp)*100._wp,REAL(plev,wp)*100._wp, &
                 REAL(tlay,wp),REAL(tsfc,wp),gases,atmos,source,tlev=REAL(tlev,wp)))
    flux%flux_up=>fu; flux%flux_dn=>fd
    CALL require(rte_lw(atmos,.FALSE.,source,emissivity,flux))
    upc=REAL(fu); dnc=REAL(fd)
    CALL require(compute_heating_rate(fu,fd,REAL(plev,wp)*100._wp,heat))
    hrc=REAL(heat*86400._wp)
    IF(ANY(cf>0.).AND.overlap/=0) THEN
      CALL require(clouds%alloc_1scl(nc,nl,cloud_lw))
      CALL require(snow%alloc_1scl(nc,nl,cloud_lw))
      CALL require(sampled%alloc_1scl(nc,nl,gas_lw))
      rl=MAX(cloud_lw%get_min_radius_liq(),MIN(cloud_lw%get_max_radius_liq(),REAL(rel,wp)))
      di=MAX(cloud_lw%get_min_radius_ice(),MIN(cloud_lw%get_max_radius_ice(),ice_diameter(rei,iceflag)))
      ds=MAX(cloud_lw%get_min_radius_ice(),MIN(cloud_lw%get_max_radius_ice(),2._wp*REAL(res,wp)))
      CALL require(cloud_lw%cloud_optics(REAL(lwp,wp),REAL(iwp,wp),rl,di,clouds))
      CALL require(cloud_lw%cloud_optics(zero,REAL(swp,wp),rl,ds,snow))
      CALL require(snow%increment(clouds))
      CALL cloud_mask(cf,gas_lw%get_ngpt(),overlap,seed,mask)
      CALL require(draw_samples(mask,clouds,sampled))
      CALL require(sampled%increment(atmos))
    END IF
    CALL require(rte_lw(atmos,.FALSE.,source,emissivity,flux))
    up=REAL(fu); dn=REAL(fd)
    CALL require(compute_heating_rate(fu,fd,REAL(plev,wp)*100._wp,heat))
    hr=REAL(heat*86400._wp)
  END SUBROUTINE

  SUBROUTINE rrtmgp_sw_column(play,plev,tlay,h2o,co2,o3,n2o,ch4,o2, &
                             avdir,avdif,andir,andif,mu0,solar, &
                             cf,lwp,iwp,swp,rel,rei,res,iceflag,overlap,seed, &
                             up,dn,hr,upc,dnc,hrc,direct,diffuse,directc,visdir,visdif,nirdir,nirdif)
    REAL, INTENT(IN) :: play(:,:),plev(:,:),tlay(:,:)
    REAL, INTENT(IN) :: h2o(:,:),co2(:,:),o3(:,:),n2o(:,:),ch4(:,:),o2(:,:)
    REAL, INTENT(IN) :: avdir(:),avdif(:),andir(:),andif(:),mu0(:),solar
    REAL, INTENT(IN) :: cf(:,:),lwp(:,:),iwp(:,:),swp(:,:),rel(:,:),rei(:,:),res(:,:)
    INTEGER, INTENT(IN) :: iceflag,overlap,seed
    REAL, INTENT(OUT) :: up(:,:),dn(:,:),hr(:,:),upc(:,:),dnc(:,:),hrc(:,:)
    REAL, INTENT(OUT) :: direct(:,:),diffuse(:,:),directc(:,:)
    REAL, INTENT(OUT) :: visdir(:,:),visdif(:,:),nirdir(:,:),nirdif(:,:)
    TYPE(ty_gas_concs) :: gases
    TYPE(ty_optical_props_2str) :: atmos,clouds,snow,sampled
    TYPE(ty_fluxes_byband) :: flux
    REAL(wp), TARGET :: fu(SIZE(plev,1),SIZE(plev,2)),fd(SIZE(plev,1),SIZE(plev,2)), &
                        fdir(SIZE(plev,1),SIZE(plev,2)), &
                        bd(SIZE(plev,1),SIZE(plev,2),gas_sw%get_nband()), &
                        bdir(SIZE(plev,1),SIZE(plev,2),gas_sw%get_nband())
    REAL(wp) :: heat(SIZE(play,1),SIZE(play,2)),zero(SIZE(play,1),SIZE(play,2)), &
                rl(SIZE(play,1),SIZE(play,2)),di(SIZE(play,1),SIZE(play,2)),ds(SIZE(play,1),SIZE(play,2))
    REAL(wp) :: toa(SIZE(play,1),gas_sw%get_ngpt()),albdir(gas_sw%get_nband(),SIZE(play,1)), &
                albdif(gas_sw%get_nband(),SIZE(play,1)),bands(2,gas_sw%get_nband())
    LOGICAL :: mask(SIZE(play,1),SIZE(play,2),gas_sw%get_ngpt())
    INTEGER :: nc,nl,b
    IF(.NOT.ready) CALL require('initialization must precede parallel column calls')
    nc=SIZE(play,1); nl=SIZE(play,2); zero=0._wp
    up=0.; dn=0.; hr=0.; upc=0.; dnc=0.; hrc=0.; direct=0.; diffuse=0.; directc=0.
    visdir=0.; visdif=0.; nirdir=0.; nirdif=0.
    IF(ALL(mu0<=0.)) RETURN
    IF(ANY(mu0<=0.)) CALL require('daylight columns must be packed separately')
    CALL set_gases(gases,h2o,co2,o3,n2o,ch4,o2)
    CALL require(atmos%alloc_2str(nc,nl,gas_sw))
    CALL require(gas_sw%gas_optics(REAL(play,wp)*100._wp,REAL(plev,wp)*100._wp, &
                                 REAL(tlay,wp),gases,atmos,toa))
    toa=toa*(REAL(solar,wp)/SUM(toa(1,:)))
    bands=gas_sw%get_band_lims_wavenumber()
    DO b=1,gas_sw%get_nband()
      IF(bands(1,b)>=12850._wp) THEN
        albdir(b,:)=REAL(avdir,wp); albdif(b,:)=REAL(avdif,wp)
      ELSE
        albdir(b,:)=REAL(andir,wp); albdif(b,:)=REAL(andif,wp)
      END IF
    END DO
    flux%flux_up=>fu; flux%flux_dn=>fd; flux%flux_dn_dir=>fdir
    flux%bnd_flux_dn=>bd; flux%bnd_flux_dn_dir=>bdir
    CALL require(rte_sw(atmos,.FALSE.,REAL(mu0,wp),toa,albdir,albdif,flux))
    upc=REAL(fu); dnc=REAL(fd); directc=REAL(fdir)
    CALL require(compute_heating_rate(fu,fd,REAL(plev,wp)*100._wp,heat)); hrc=REAL(heat*86400._wp)
    IF(ANY(cf>0.).AND.overlap/=0) THEN
      CALL require(clouds%alloc_2str(nc,nl,cloud_sw))
      CALL require(snow%alloc_2str(nc,nl,cloud_sw))
      CALL require(sampled%alloc_2str(nc,nl,gas_sw))
      rl=MAX(cloud_sw%get_min_radius_liq(),MIN(cloud_sw%get_max_radius_liq(),REAL(rel,wp)))
      di=MAX(cloud_sw%get_min_radius_ice(),MIN(cloud_sw%get_max_radius_ice(),ice_diameter(rei,iceflag)))
      ds=MAX(cloud_sw%get_min_radius_ice(),MIN(cloud_sw%get_max_radius_ice(),2._wp*REAL(res,wp)))
      CALL require(cloud_sw%cloud_optics(REAL(lwp,wp),REAL(iwp,wp),rl,di,clouds))
      CALL require(cloud_sw%cloud_optics(zero,REAL(swp,wp),rl,ds,snow))
      CALL require(snow%increment(clouds))
      CALL require(clouds%delta_scale())
      CALL cloud_mask(cf,gas_sw%get_ngpt(),overlap,seed,mask)
      CALL require(draw_samples(mask,clouds,sampled))
      CALL require(sampled%increment(atmos))
    END IF
    CALL require(rte_sw(atmos,.FALSE.,REAL(mu0,wp),toa,albdir,albdif,flux))
    up=REAL(fu); dn=REAL(fd); direct=REAL(fdir); diffuse=REAL(fd-fdir)
    CALL require(compute_heating_rate(fu,fd,REAL(plev,wp)*100._wp,heat)); hr=REAL(heat*86400._wp)
    DO b=1,gas_sw%get_nband()
      IF(bands(1,b)>=12850._wp) THEN
        visdir=visdir+REAL(bdir(:,:,b)); visdif=visdif+REAL(bd(:,:,b)-bdir(:,:,b))
      ELSE
        nirdir=nirdir+REAL(bdir(:,:,b)); nirdif=nirdif+REAL(bd(:,:,b)-bdir(:,:,b))
      END IF
    END DO
  END SUBROUTINE
END MODULE module_ra_rrtmgp
