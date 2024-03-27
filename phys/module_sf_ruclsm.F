#define lsmruc_dbg_lvl 3000
!wrf:model_layer:physics
!
module module_sf_ruclsm

! notes for perturbations of soil properties (judith berner)
! perturbations are applied in subroutine soilprob to array hydro;
! soilprop is called from subroutine sfctmp which is called from subroutine lsmruc;
! subroutine lsmruc had two new 3d fields: pattern_spp_lsm (in) and field_sf(inout);
!    their vertical dimension is number of atmospheric levels (kms:kme) - (suboptimal, but easiest hack)
!    field_sf is used to pass perturbed fields of hydrop up to model (and output) driver;
! in argument list to sfctmp the arrays are passed as pattern_spp_lsm(i,1:nzs,j), and exist henceforth as
! column arrays;
! in the subroutines below sfctmp (snow and snowsoil) the fields are called rstochcol,fieldcol_sf
! to reflect their dimension rstochcol (1:nzs)


  use module_model_constants
  use module_wrf_error

! vegetation parameters
        integer :: lucats , bare, natural, crop, urban
        integer, parameter :: nlus=50
        character*8 lutype
        integer, dimension(1:nlus) :: ifortbl
        real, dimension(1:nlus) ::  snuptbl, rstbl, rgltbl, hstbl, laitbl,         &
                                    albtbl, z0tbl, lemitbl, pctbl, shdtbl, maxalb
        real ::   topt_data,cmcmax_data,cfactr_data,rsmax_data
! soil parameters
        integer :: slcats
        integer, parameter :: nsltype=30
        character*8 sltype
        real, dimension (1:nsltype) :: bb,drysmc,hc,                           &
        maxsmc, refsmc,satpsi,satdk,satdw, wltsmc,qtz

! lsm general parameters
        integer :: slpcats
        integer, parameter :: nslope=30
        real, dimension (1:nslope) :: slope_data
        real ::  sbeta_data,fxexp_data,csoil_data,salp_data,refdk_data,           &
                 refkdt_data,frzk_data,zbot_data,  smlow_data,smhigh_data,        &
                        czil_data

        character*256  :: err_message

      !-- options for snow conductivity: 1 - constant, 2 - Sturm et al.,1997
      !   integer, parameter :: isncond_opt = 1
      !
        integer, parameter :: isncond_opt=2

      !-- Snow fraction options
      !-- option 1: original formulation using threshold snow depth to compute snow fraction
      !integer, parameter :: isncovr_opt = 1 (default)
      !-- option 2: the tanh formulation from Niu,G.-Y.,and Yang,Z.-L., 2007,JGR,DOI:10.1029/2007JD008674.
      !integer, parameter :: isncovr_opt = 2
      !-- option 3: the tanh formulation from Niu,G.-Y.,and Yang,Z with
      !   vegetation-dependent parameters from Noah MP (personal communication with
      !   Mike Barlage)
      !integer, parameter :: isncovr_opt = 3
      !-- Values of parameters are scale-dependent, have to be tuned for a given application
      !-- Tables below are for 21-class MODI-RUC (MODIFIED_IGBP_MODIS_NOAH_15s is used in HRRR and RRFS)
      !-- for 3-km RRFS application
        real, dimension(30), parameter ::        sncovfac =     &
     &                    (/ 0.030, 0.030, 0.030, 0.030, 0.030, &
     &                       0.016, 0.016, 0.020, 0.020, 0.020, &
     &                       0.020, 0.014, 0.042, 0.026, 0.030, &
     &                       0.016, 0.030, 0.030, 0.030, 0.030, &
     &                       0.000, 0.000, 0.000, 0.000, 0.000, &
     &                       0.000, 0.000, 0.000, 0.000, 0.000 /)
       real, dimension(30), parameter ::         mfsno =        &
     &                  (/  1.00, 1.00, 1.00, 1.00, 2.00, 2.00, &
     &                      2.00, 2.00, 2.00, 2.00, 2.00, 2.00, &
     &                      3.00, 3.00, 2.00, 2.00, 2.00, 2.00, &
     &                      2.00, 2.00, 0.00, 0.00, 0.00, 0.00, &
     &                      0.00, 0.00, 0.00, 0.00, 0.00, 0.00 /)

      !--
        integer, parameter :: isncovr_opt=2
      !--  

contains

!-----------------------------------------------------------------
    subroutine lsmruc(spp_lsm,                                   &
#if (EM_CORE==1)
                   pattern_spp_lsm,field_sf,                     &
#endif
                   dt,ktau,nsl,                                  &
#if (EM_CORE==1)
                   lakemodel,lakemask,                           &
                   graupelncv,snowncv,rainncv,                   &
#endif
                   zs,rainbl,snow,snowh,snowc,frzfrac,frpcpn,    &
                   rhosnf,precipfr,                              & ! pass it out to module_diagnostics
                   z3d,p8w,t3d,qv3d,qc3d,rho3d,                  & !p8w in [pa]
                   glw,gsw,emiss,chklowq, chs,                   & 
                   flqc,flhc,mavail,canwat,vegfra,alb,znt,       &
                   z0,snoalb,albbck,lai,                         &  !new
                   mminlu, landusef, nlcat, mosaic_lu,           &
                   mosaic_soil, soilctop, nscat,                 &  !new
                   qsfc,qsg,qvg,qcg,dew,soilt1,tsnav,            &
                   tbot,ivgtyp,isltyp,xland,                     &
                   iswater,isice,xice,xice_threshold,            &
                   cp,rovcp,g0,lv,stbolt,                        &
                   soilmois,sh2o,smavail,smmax,                  &
                   tso,soilt,hfx,qfx,lh,                         &
                   sfcrunoff,udrunoff,acrunoff,sfcexc,           &
                   sfcevp,grdflx,snowfallac,acsnow,snom,         &
                   smfr3d,keepfr3dflag,                          &
                   myj,shdmin,shdmax,rdlai2d,                    &
                   ids,ide, jds,jde, kds,kde,                    &
                   ims,ime, jms,jme, kms,kme,                    &
                   its,ite, jts,jte, kts,kte                     )
!-----------------------------------------------------------------
   implicit none
!-----------------------------------------------------------------
!
! the ruc lsm model is described in:
!  Smirnova, T.G., J.M. Brown, and S.G. Benjamin, 1997: 
!     performance of different soil model configurations in simulating 
!     ground surface temperature and surface fluxes. 
!     mon. wea. rev. 125, 1870-1884.
!  Smirnova, T.G., J.M. Brown, and D. Kim, 2000: parameterization of 
!     cold-season processes in the maps land-surface scheme. 
!     j. geophys. res. 105, 4077-4086.
!-----------------------------------------------------------------
!-- dt            time step (second)
!        ktau - number of time step
!        nsl  - number of soil layers
!        nzs  - number of levels in soil
!        zs   - depth of soil levels (m)
!-- rainbl    - accumulated rain in [mm] between the pbl calls
!-- rainncv         one time step grid scale precipitation (mm/step)
!        snow - snow water equivalent [mm]
!        frazfrac - fraction of frozen precipitation
!-- precipfr (mm) - time step frozen precipitation
!-- snowc       flag indicating snow coverage (1 for snow cover)
!-- z3d         heights (m)
!-- p8w         3d pressure (pa)
!-- t3d         temperature (k)
!-- qv3d        3d water vapor mixing ratio (kg/kg)
!        qc3d - 3d cloud water mixing ratio (kg/kg)
!       rho3d - 3d air density (kg/m^3)
!-- glw         downward long wave flux at ground surface (w/m^2)
!-- gsw         absorbed short wave flux at ground surface (w/m^2)
!-- emiss       surface emissivity (between 0 and 1)
!        flqc - surface exchange coefficient for moisture (kg/m^2/s)
!        flhc - surface exchange coefficient for heat [w/m^2/s/degreek]     
!      sfcexc - surface exchange coefficient for heat [m/s]
!      canwat - canopy moisture content (mm)
!      vegfra - vegetation fraction (between 0 and 100)
!         alb - surface albedo (between 0 and 1)
!      snoalb - maximum snow albedo (between 0 and 1)
!      albbck - snow-free albedo (between 0 and 1)
!         znt - roughness length [m]
!-- tbot        soil temperature at lower boundary (k)
!      ivgtyp - usgs vegetation type (24 classes)
!      isltyp - stasgo soil type (16 classes)
!-- xland       land mask (1 for land, 2 for water)
!-- cp          heat capacity at constant pressure for dry air (j/kg/k)
!-- g0          acceleration due to gravity (m/s^2)
!-- lv          latent heat of melting (j/kg)
!-- stbolt      stefan-boltzmann constant (w/m^2/k^4)
!    soilmois - soil moisture content (volumetric fraction)
!         tso - soil temp (k)
!-- soilt       surface temperature (k)
!-- hfx         upward heat flux at the surface (w/m^2)
!-- qfx         upward moisture flux at the surface (kg/m^2/s)
!-- lh          upward latent heat flux (w/m^2)
!   sfcrunoff - ground surface runoff [mm]
!   udrunoff - underground runoff [mm]
!   acrunoff - run-total surface runoff [mm]
!   sfcevp - total evaporation in [kg/m^2]
!   grdflx - soil heat flux (w/m^2: negative, if downward from surface)
!   snowfallac - run-total snowfall accumulation [m]   
!   acsnow - run-toral swe of snowfall [mm]   
!-- chklowq - is either 0 or 1 (so far set equal to 1).
!--           used only in myjpbl. 
!-- tice - sea ice temperture (c)
!-- rhosice - sea ice density (kg m^-3)
!-- capice - sea ice volumetric heat capacity (j/m^3/k)
!-- thdifice - sea ice thermal diffusivity (m^2/s)
!--
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-------------------------------------------------------------------------
!   integer,     parameter            ::     nzss=5
!   integer,     parameter            ::     nddzs=2*(nzss-2)

   integer,     parameter            ::     nvegclas=24+3

   real,       intent(in   )    ::     dt
   logical,    intent(in   )    ::     myj,frpcpn
   integer,    intent(in   )    ::     spp_lsm
   integer,    intent(in   )    ::     nlcat, nscat, mosaic_lu, mosaic_soil
   integer,    intent(in   )    ::     ktau, nsl, isice, iswater, &
                                       ims,ime, jms,jme, kms,kme, &
                                       ids,ide, jds,jde, kds,kde, &
                                       its,ite, jts,jte, kts,kte

#if (EM_CORE==1)
   real,    dimension( ims:ime, kms:kme, jms:jme ),optional::    pattern_spp_lsm
   real,    dimension( ims:ime, kms:kme, jms:jme ),optional::    field_sf
#endif
   real,    dimension( ims:ime, 1  :nsl, jms:jme )         ::    field_sf_loc

   real,    dimension( ims:ime, kms:kme, jms:jme )            , &
            intent(in   )    ::                           qv3d, &
                                                          qc3d, &
                                                           p8w, &
                                                         rho3d, &
                                                           t3d, &
                                                           z3d

   real,       dimension( ims:ime , jms:jme ),                   &
               intent(in   )    ::                       rainbl, &
                                                            glw, &
                                                            gsw, &
                                                         albbck, &
                                                           flhc, &
                                                           flqc, &
                                                           chs , &
                                                           xice, &
                                                          xland, &
!                                                         albbck, &
                                                           tbot

!beka
   real,       dimension( ims:ime , jms:jme ),                   &
               intent(inout   )    ::                       vegfra


#if (EM_CORE==1)
   real,       optional, dimension( ims:ime , jms:jme ),         &
               intent(in   )    ::                   graupelncv, &
                                                        snowncv, &
                                                        rainncv
   real,       dimension( ims:ime , jms:jme ),                   &
               intent(in   )    ::                     lakemask
   integer,    intent(in   )    ::                    lakemodel
#endif

   real, dimension( ims:ime , jms:jme ), intent(in )::   shdmax
   real, dimension( ims:ime , jms:jme ), intent(in )::   shdmin
   logical, intent(in) :: rdlai2d

   real,       dimension( 1:nsl), intent(in   )      ::      zs

   real,       dimension( ims:ime , jms:jme ),                   &
               intent(inout)    ::                               &
                                                           snow, &
                                                          snowh, &
                                                          snowc, &
                                                         canwat, & ! new
                                                         snoalb, &
                                                            alb, &
                                                          emiss, &
                                                            lai, &
                                                         mavail, & 
                                                         sfcexc, &
                                                            z0 , &
                                                            znt

   real,       dimension( ims:ime , jms:jme ),                   &
               intent(in   )    ::                               &
                                                        frzfrac

   integer,    dimension( ims:ime , jms:jme ),                   &
               intent(in   )    ::                       ivgtyp, &
                                                         isltyp
   character(len=*), intent(in   )    ::                 mminlu
   real,     dimension( ims:ime , 1:nlcat, jms:jme ), intent(in):: landusef
   real,     dimension( ims:ime , 1:nscat, jms:jme ), intent(in):: soilctop

   real, intent(in   )          ::         cp,rovcp,g0,lv,stbolt,xice_threshold
 
   real,       dimension( ims:ime , 1:nsl, jms:jme )           , &
               intent(inout)    ::                 soilmois,sh2o,tso

   real,       dimension( ims:ime, jms:jme )                   , &
               intent(inout)    ::                        soilt, &
                                                            hfx, &
                                                            qfx, &
                                                             lh, &
                                                         sfcevp, &
                                                      sfcrunoff, &
                                                       udrunoff, &
                                                       acrunoff, &
                                                         grdflx, &
                                                         acsnow, &
                                                           snom, &
                                                            qvg, &
                                                            qcg, &
                                                            dew, &
                                                           qsfc, &
                                                            qsg, &
                                                        chklowq, &
                                                         soilt1, &
                                                          tsnav

   real,       dimension( ims:ime, jms:jme )                   , & 
               intent(inout)    ::                      smavail, &
                                                          smmax

   real,       dimension( its:ite, jts:jte )    ::               &
                                                             pc, &
                                                        runoff1, &
                                                        runoff2, &
                                                         emissl, &
                                                           zntl, &
                                                        lmavail, &
                                                          smelt, &
                                                           snoh, &
                                                          snflx, &
                                                           edir, &
                                                             ec, &
                                                            ett, &
                                                         sublim, &
                                                           sflx, &
                                                            smf, &
                                                          evapl, &
                                                          prcpl, &
                                                         seaice, &
                                                        infiltr
! energy and water budget variables:
   real,       dimension( its:ite, jts:jte )    ::               &
                                                         budget, &
                                                       acbudget, &
                                                    waterbudget, &
                                                  acwaterbudget, &
                                                       smtotold, &
                                                        snowold, &
                                                      canwatold


   real,       dimension( ims:ime, 1:nsl, jms:jme)               &
                                             ::    keepfr3dflag, &
                                                         smfr3d

   real,       dimension( ims:ime, jms:jme ), intent(out)     :: &
                                                         rhosnf, & !rho of snowfall
                                                       precipfr, & ! time-step frozen precip
                                                     snowfallac
!--- soil/snow properties
   real                                                          &
                             ::                           rhocs, &
                                                       rhonewsn, &
                                                          rhosn, &
                                                      rhosnfall, &
                                                           bclh, &
                                                            dqm, &
                                                           ksat, &
                                                           psis, &
                                                           qmin, &
                                                          qwrtz, &
                                                            ref, &
                                                           wilt, &
                                                        canwatr, &
                                                       snowfrac, &
                                                          snhei, &
                                                           snwe

   real                                      ::              cn, &
                                                         sat,cw, &
                                                           c1sn, &
                                                           c2sn, &
                                                         kqwrtz, &
                                                           kice, &
                                                            kwt


   real,     dimension(1:nsl)                ::          zsmain, &
                                                         zshalf, &
                                                         dtdzs2

   real,     dimension(1:2*(nsl-2))          ::           dtdzs

   real,     dimension(1:5001)               ::             tbq


   real,     dimension( 1:nsl )              ::         soilm1d, & 
                                                          tso1d, &
                                                        soilice, &
                                                        soiliqw, &
                                                       smfrkeep

   real,     dimension( 1:nsl )              ::          keepfr
                                                
   real,     dimension( 1:nlcat )            ::          lufrac
   real,     dimension( 1:nscat )            ::          soilfrac

   real                           ::                        rsm, &
                                                      snweprint, &
                                                     snheiprint

   real                           ::                     prcpms, &
                                                        newsnms, &
                                                      prcpncliq, &
                                                       prcpncfr, &
                                                      prcpculiq, &
                                                       prcpcufr, &
                                                           patm, &
                                                          patmb, &
                                                           tabs, &
                                                          qvatm, &
                                                          qcatm, &
                                                          q2sat, &
                                                         conflx, &
                                                            rho, &
                                                           qkms, &
                                                           tkms, &
                                                        snowrat, &
                                                       grauprat, &
                                                       graupamt, &
                                                         icerat, &
                                                          curat, &
                                                       infiltrp
   real      ::  cq,r61,r273,arp,brp,x,evs,eis
   real      ::  cropfr, cropsm, newsm, factor

   real      ::  meltfactor, ac,as, wb
   integer   ::  nroot
   integer   ::  iland,isoil,iforest
 
   integer   ::  i,j,k,nzs,nzs1,nddzs
   integer   ::  k1,l,k2,kp,km
   character (len=132) :: message

   real,dimension(ims:ime,1:nsl,jms:jme) :: rstoch 
!beka
   real,dimension(ims:ime,jms:jme)::emisso,vegfrao,albo,snoalbo
   real,dimension(its:ite,jts:jte)::emisslo

!-----------------------------------------------------------------
         nzs=nsl
         nddzs=2*(nzs-2)

         rstoch=0.0
         field_sf_loc=0.0
!beka added
#if (EM_CORE==1)
       if (spp_lsm==1) then
         do j=jts,jte
           do i=its,ite
             do k=1,nsl
               rstoch(i,k,j) = pattern_spp_lsm(i,k,j)
               field_sf_loc(i,k,j)=field_sf(i,k,j)
             enddo
           enddo
         enddo 
       endif  
#endif
!---- table tbq is for resolution of balance equation in vilka
        cq=173.15-.05
        r273=1./273.15
        r61=6.1153*0.62198
        arp=77455.*41.9/461.525
        brp=64.*41.9/461.525

        do k=1,5001
          cq=cq+.05
        evs=exp(17.67*(cq-273.15)/(cq-29.65))
        eis=exp(22.514-6.15e3/cq)
        if(cq.ge.273.15) then
! tbq is in mb
        tbq(k) = r61*evs
        else
        tbq(k) = r61*eis
        endif

        end do

!--- initialize soil/vegetation parameters
#if ( NMM_CORE == 1 )
     if(ktau+1.eq.1) then
#else
     if(ktau.eq.1) then
#endif
     do j=jts,jte
         do i=its,ite
            do k=1,nsl
       keepfr3dflag(i,k,j)=0.
            enddo
!--- initializing snow fraction, thereshold = 32 mm of snow water or ~100 mm of snow height
        if((soilt1(i,j) .lt. 170.) .or. (soilt1(i,j) .gt.400.)) then
         if(snowc(i,j).gt.0.) then
           soilt1(i,j)=0.5*(soilt(i,j)+tso(i,1,j))
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
        write ( message , fmt='(a,f8.3,2i6)' ) &
       'temperature inside snow is initialized in ruclsm ', soilt1(i,j),i,j
        call wrf_debug ( 0 , message )
    endif
            else
           soilt1(i,j) = tso(i,1,j)
         endif ! snowc
       endif ! soilt1
       !-- temperature inside snow is initialized
           tsnav(i,j) =0.5*(soilt(i,j)+tso(i,1,j))-273.15
           patmb=p8w(i,kms,j)*1.e-2
           qsg  (i,j) = qsn(soilt(i,j),tbq)/patmb
           if((qcg(i,j) < 0.) .or. (qcg(i,j) > 0.1)) then
             qcg  (i,j) = qc3d(i,1,j)
             if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
               write ( message , fmt='(a,3f8.3,2i6)' ) &
                'qvg is initialized in ruclsm ', qvg(i,j),mavail(i,j),qsg(i,j),i,j
             endif
           endif ! qcg

        if((qvg(i,j) .le. 0.) .or. (qvg(i,j) .gt.0.1)) then
           qvg  (i,j) = qsg(i,j)*mavail(i,j)
          if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
           write ( message , fmt='(a,3f8.3,2i6)' ) &
          'qvg is initialized in ruclsm ', qvg(i,j),mavail(i,j),qsg(i,j),i,j
           call wrf_debug ( 0 , message )
          endif
        endif
           qsfc(i,j) = qvg(i,j)/(1.+qvg(i,j))
           smelt(i,j) = 0.
           snom (i,j) = 0.
           snowfallac(i,j) = 0.
           precipfr(i,j) = 0.
           rhosnf(i,j) = -1.e3 ! non-zero flag
           snflx(i,j) = 0.
           dew  (i,j) = 0.
           pc   (i,j) = 0.
           zntl (i,j) = 0.
           runoff1(i,j) = 0.
           runoff2(i,j) = 0.
           sfcrunoff(i,j) = 0.
           udrunoff(i,j) = 0.
           acrunoff(i,j) = 0.
           emissl (i,j) = 0.
           budget(i,j) = 0.
           acbudget(i,j) = 0.
           waterbudget(i,j) = 0.
           acwaterbudget(i,j) = 0.
           smtotold(i,j)=0.
           canwatold(i,j)=0.

! for ruc lsm chklowq needed for myjpbl should 
! 1 because is actual specific humidity at the surface, and
! not the saturation value
           chklowq(i,j) = 1.
           infiltr(i,j) = 0.
           snoh  (i,j) = 0.
           edir  (i,j) = 0.
           ec    (i,j) = 0.
           ett   (i,j) = 0.
           sublim(i,j) = 0.
           sflx  (i,j) = 0.
           smf   (i,j) = 0.
           evapl (i,j) = 0.
           prcpl (i,j) = 0.
         enddo
     enddo

        do k=1,nsl
           soilice(k)=0.
           soiliqw(k)=0.
        enddo
     endif

!-----------------------------------------------------------------

        prcpms = 0.
        newsnms = 0.
        prcpncliq = 0.
        prcpculiq = 0.
        prcpncfr = 0.
        prcpcufr = 0.


   do j=jts,jte

      do i=its,ite

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,' in lsmruc ','ims,ime,jms,jme,its,ite,jts,jte,nzs', &
                ims,ime,jms,jme,its,ite,jts,jte,nzs
      print *,' ivgtyp, isltyp ', ivgtyp(i,j),isltyp(i,j)
      print *,' mavail ', mavail(i,j)
      print *,' soilt,qvg,p8w',soilt(i,j),qvg(i,j),p8w(i,1,j)
      print *, 'lsmruc, i,j,xland, qfx,hfx from sfclay',i,j,xland(i,j), &
                  qfx(i,j),hfx(i,j)
      print *, ' gsw, glw =',gsw(i,j),glw(i,j)
      print *, 'soilt, tso start of time step =',soilt(i,j),(tso(i,k,j),k=1,nsl)
      print *, 'soilmois start of time step =',(soilmois(i,k,j),k=1,nsl)
      print *, 'smfrozen start of time step =',(smfr3d(i,k,j),k=1,nsl)
      print *, ' i,j=, after sfclay chs,flhc ',i,j,chs(i,j),flhc(i,j)
      print *, 'lsmruc, ivgtyp,isltyp,alb = ', ivgtyp(i,j),isltyp(i,j),alb(i,j),i,j
      print *, 'lsmruc  i,j,dt,rainbl =',i,j,dt,rainbl(i,j)
      print *, 'xland ---->, ivgtype,isoiltyp,i,j',xland(i,j),ivgtyp(i,j),isltyp(i,j),i,j
    endif


         iland     = ivgtyp(i,j)
         isoil     = isltyp(i,j)
         tabs      = t3d(i,kms,j)
         qvatm     = qv3d(i,kms,j)
         qcatm     = qc3d(i,kms,j)
         patm      = p8w(i,kms,j)*1.e-5
!-- z3d(1) is thickness between first full sigma level and the surface, 
!-- but first mass level is at the half of the first sigma level 
!-- (u and v are also at the half of first sigma level)
         conflx    = z3d(i,kms,j)*0.5
         rho       = rho3d(i,kms,j)
! -- initialize snow, graupel and ice fractions in frozen precip
         snowrat = 0.
         grauprat = 0.
         icerat = 0.
         curat = 0.
       if(frpcpn) then
#if (EM_CORE==1)
         prcpncliq = rainncv(i,j)*(1.-frzfrac(i,j))
         prcpncfr = rainncv(i,j)*frzfrac(i,j)
!- apply the same frozen precipitation fraction to convective precip
!tgs - 31 mar17 - add safety temperature check in case thompson mp produces
!                 frozen precip at t > 273.
       if(frzfrac(i,j) > 0..and. tabs < 273.) then
         prcpculiq = max(0.,(rainbl(i,j)-rainncv(i,j))*(1.-frzfrac(i,j)))
         prcpcufr = max(0.,(rainbl(i,j)-rainncv(i,j))*frzfrac(i,j))
       else
          if(tabs < 273.) then
            prcpcufr = max(0.,(rainbl(i,j)-rainncv(i,j)))
            prcpculiq = 0.
          else
            prcpcufr = 0.
            prcpculiq = max(0.,(rainbl(i,j)-rainncv(i,j)))
          endif  ! tabs < 273.
       endif  ! frzfrac > 0.
!--- 1*e-3 is to convert from mm/s to m/s
         prcpms   = (prcpncliq + prcpculiq)/dt*1.e-3
         newsnms  = (prcpncfr + prcpcufr)/dt*1.e-3

         if ( present( graupelncv ) ) then
             graupamt = graupelncv(i,j)
         else
             graupamt = 0.
         endif

         if((prcpncfr + prcpcufr) > 0.) then
! -- calculate snow, graupel and ice fractions in falling frozen precip
         snowrat=min(1.,max(0.,snowncv(i,j)/(prcpncfr + prcpcufr)))
         grauprat=min(1.,max(0.,graupamt/(prcpncfr + prcpcufr)))
         icerat=min(1.,max(0.,(prcpncfr-snowncv(i,j)-graupamt) &
               /(prcpncfr + prcpcufr)))
         curat=min(1.,max(0.,(prcpcufr/(prcpncfr + prcpcufr))))
         endif
#else
         prcpms    = (rainbl(i,j)/dt*1.e-3)*(1-frzfrac(i,j))
         newsnms  = (rainbl(i,j)/dt*1.e-3)*frzfrac(i,j)
       if(newsnms == 0.) then
         snowrat = 0.
       else
         snowrat = min(1.,newsnms/(newsnms+prcpms))
       endif
#endif

       else  ! .not. frpcpn
          if (tabs.le.273.15) then
         prcpms    = 0.
         newsnms   = rainbl(i,j)/dt*1.e-3
!-- here no info about constituents of frozen precipitation,
!-- suppose it is all snow
         snowrat = 1.
          else
         prcpms    = rainbl(i,j)/dt*1.e-3
         newsnms   = 0.
          endif
       endif

! -- save time-step water equivalent of frozen precipitation in precipfr array to be used in
!    module_diagnostics
          precipfr(i,j) = newsnms * dt *1.e3

        if   (myj)   then
         qkms=chs(i,j)
         tkms=chs(i,j)
        else
!--- convert exchange coeff qkms to [m/s]
         qkms=flqc(i,j)/rho/mavail(i,j)
!         tkms=flhc(i,j)/rho/cp
         tkms=flhc(i,j)/rho/(cp*(1.+0.84*qvatm))  ! mynnsfc uses cpm
        endif
!--- convert incoming snow and canwat from mm to m
         snwe=snow(i,j)*1.e-3
         snhei=snowh(i,j)
         canwatr=canwat(i,j)*1.e-3

         snowfrac=snowc(i,j)
         rhosnfall=rhosnf(i,j)

         snowold(i,j)=snwe
!-----
             zsmain(1)=0.
             zshalf(1)=0.
          do k=2,nzs
             zsmain(k)= zs(k)
             zshalf(k)=0.5*(zsmain(k-1) + zsmain(k))
          enddo

          do k=1,nlcat
             lufrac(k) = landusef(i,k,j)
          enddo
          do k=1,nscat
             soilfrac(k) = soilctop(i,k,j)
          enddo

!------------------------------------------------------------
!-----  ddzs and dsdz1 are for implicit solution of soil eqns.
!-------------------------------------------------------------
        nzs1=nzs-1
!-----
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
         print *,' dt,nzs1, zsmain, zshalf --->', dt,nzs1,zsmain,zshalf
    endif

        do  k=2,nzs1
          k1=2*k-3
          k2=k1+1
          x=dt/2./(zshalf(k+1)-zshalf(k))
          dtdzs(k1)=x/(zsmain(k)-zsmain(k-1))
          dtdzs2(k-1)=x
          dtdzs(k2)=x/(zsmain(k+1)-zsmain(k))
        end do

        cw =4.183e6


!--- constants used in johansen soil thermal
!--- conductivity method

        kqwrtz=7.7
        kice=2.2
        kwt=0.57

!***********************************************************************
!--- constants for snow density calculations c1sn and c2sn

        c1sn=0.026
        c2sn=21.

!***********************************************************************

        nroot= 4 ! levels in root layer

        rhonewsn = 200.
       if(snow(i,j).gt.0. .and. snowh(i,j).gt.0.) then
        rhosn = snow(i,j)/snowh(i,j)
       else
        rhosn = 300.
       endif

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       if(ktau.eq.1 .and.(i.eq.358.and.j.eq.260)) &
           print *,'before soilvegin - z0,znt(195,254)',z0(i,j),znt(i,j)
    endif
!--- initializing soil and surface properties
     call soilvegin  ( mosaic_lu, mosaic_soil,soilfrac,nscat,shdmin(i,j),shdmax(i,j),&
                       nlcat,iland,isoil,iswater,myj,iforest,lufrac,vegfra(i,j),     &
                       emissl(i,j),pc(i,j),znt(i,j),lai(i,j),rdlai2d,                &
                       qwrtz,rhocs,bclh,dqm,ksat,psis,qmin,ref,wilt,i,j )
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      if(ktau.eq.1 .and.(i.eq.358.and.j.eq.260)) &
         print *,'after soilvegin - z0,znt(375,254),lai(375,254)',z0(i,j),znt(i,j),lai(i,j)

      if(ktau.eq.1 .and. (i.eq.358.and.j.eq.260)) then
         print *,'nlcat,iland,lufrac,emissl(i,j),pc(i,j),znt(i,j),lai(i,j)', &
                  nlcat,iland,lufrac,emissl(i,j),pc(i,j),znt(i,j),lai(i,j),i,j
         print *,'nscat,soilfrac,qwrtz,rhocs,bclh,dqm,ksat,psis,qmin,ref,wilt',&
                 nscat,soilfrac,qwrtz,rhocs,bclh,dqm,ksat,psis,qmin,ref,wilt,i,j
      endif
    endif

        cn=cfactr_data   ! exponent
        sat = 5.e-4  ! units [m]

!-- definition of number of soil levels in the rooting zone
     if(iforest.gt.2) then
!---- all vegetation types except evergreen and mixed forests
!18apr08 - define meltfactor for egglston melting limit:
! for open areas factor is 2, and for forests - factor is 0.85
! this will make limit on snow melting smaller and let snow stay 
! longer in the forests.
         meltfactor = 2.0

         do k=2,nzs
         if(zsmain(k).ge.0.4) then
            nroot=k
            goto  111
         endif
         enddo
     else
!---- evergreen and mixed forests
!18apr08 - define meltfactor
!         meltfactor = 1.5
! 28 march 11 - previously used value of metfactor= 1.5 needs to be further reduced 
! to compensate for low snow albedos in the forested areas. 
! melting rate in forests will reduce.
         meltfactor = 0.85

         do k=2,nzs
         if(zsmain(k).ge.1.1) then
            nroot=k
            goto  111
         endif
         enddo
     endif
 111   continue

!-----
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
         print *,' znt, lai, vegfra, sat, emis, pc --->',                &
                   znt(i,j),lai(i,j),vegfra(i,j),sat,emissl(i,j),pc(i,j)
         print *,' zs, zsmain, zshalf, conflx, cn, sat, --->', zs,zsmain,zshalf,conflx,cn,sat
         print *,'nroot, meltfactor, iforest, ivgtyp, i,j ', nroot,meltfactor,iforest,ivgtyp(i,j),i,j
    endif

#if (EM_CORE==1)
     if(lakemodel==1. .and. lakemask(i,j)==1.) goto 2999
!lakes
#endif

        if((xland(i,j)-1.5).ge.0.)then
!-- water 
           smavail(i,j)=1.0
             smmax(i,j)=1.0
             snow(i,j)=0.0
             snowh(i,j)=0.0
             snowc(i,j)=0.0
           lmavail(i,j)=1.0

           iland=iswater
           isoil=14

           patmb=p8w(i,1,j)*1.e-2
           qvg  (i,j) = qsn(soilt(i,j),tbq)/patmb
           qsfc(i,j) = qvg(i,j)/(1.+qvg(i,j))
           chklowq(i,j)=1.
           q2sat=qsn(tabs,tbq)/patmb

            do k=1,nzs
              soilmois(i,k,j)=1.0
              sh2o    (i,k,j)=1.0 
              tso(i,k,j)= soilt(i,j)
            enddo

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
              print*,'  water point, i=',i,                      &
              'j=',j, 'soilt=', soilt(i,j)
    endif

           else

! land point or sea ice
       if(xice(i,j).ge.xice_threshold) then
           seaice(i,j)=1.
       else
           seaice(i,j)=0.
       endif

         if(seaice(i,j).gt.0.5)then
!-- sea-ice case
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
              print*,' sea-ice at water point, i=',i,            &
              'j=',j
    endif
!            iland = 24
            iland = isice
            isoil = 16
            znt(i,j) = 0.011
            snoalb(i,j) = 0.75
            dqm = 1.
            ref = 1.
            qmin = 0.
            wilt = 0.
            emissl(i,j) = 0.98 

           patmb=p8w(i,1,j)*1.e-2
           qvg  (i,j) = qsn(soilt(i,j),tbq)/patmb
           qsg  (i,j) = qvg(i,j)
           qsfc(i,j) = qvg(i,j)/(1.+qvg(i,j))

            do k=1,nzs
               soilmois(i,k,j) = 1.
               smfr3d(i,k,j)   = 1.
               sh2o(i,k,j)     = 0.
               keepfr3dflag(i,k,j) = 0.
               tso(i,k,j) = min(271.4,tso(i,k,j))
            enddo
          endif

!  attention!!!!  ruc lsm uses soil moisture content minus residual (minimum
!  or dry soil moisture content for a given soil type) as a state variable.

           do k=1,nzs
! soilm1d - soil moisture content minus residual [m**3/m**3]
              soilm1d (k) = min(max(0.,soilmois(i,k,j)-qmin),dqm)
              tso1d   (k) = tso(i,k,j)
              soiliqw (k) = min(max(0.,sh2o(i,k,j)-qmin),soilm1d(k))
              soilice (k) =(soilm1d (k) - soiliqw (k))/0.9
           enddo 

           do k=1,nzs
              smfrkeep(k) = smfr3d(i,k,j)
              keepfr  (k) = keepfr3dflag(i,k,j)
           enddo

              lmavail(i,j)=max(0.00001,min(1.,soilm1d(1)/(ref-qmin)))

#if ( NMM_CORE == 1 )
     if(ktau+1.gt.1) then
#else
     if(ktau.gt.1) then
#endif
     endif

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
   print *,'land, i,j,tso1d,soilm1d,patm,tabs,qvatm,qcatm,rho',  &
                  i,j,tso1d,soilm1d,patm,tabs,qvatm,qcatm,rho
   print *,'conflx =',conflx 
   print *,'smfrkeep,keepfr   ',smfrkeep,keepfr
    endif

        smtotold(i,j)=0.
      do k=1,nzs-1
        smtotold(i,j)=smtotold(i,j)+(qmin+soilm1d(k))*             &
                    (zshalf(k+1)-zshalf(k))
      enddo

        smtotold(i,j)=smtotold(i,j)+(qmin+soilm1d(nzs))*           &
                    (zsmain(nzs)-zshalf(nzs))

        canwatold(i,j) = canwatr
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'before sfctmp, spp_lsm, rstoch, field_sf_loc',      &
      i,j,spp_lsm,(rstoch(i,k,j),k=1,nzs),(field_sf_loc(i,k,j),k=1,nzs)
    endif
!-----------------------------------------------------------------
         call sfctmp (spp_lsm,rstoch(i,:,j),field_sf_loc(i,:,j), & 
                dt,ktau,conflx,i,j,                              &
!--- input variables
                nzs,nddzs,nroot,meltfactor,                      &   !added meltfactor
                iland,isoil,xland(i,j),ivgtyp(i,j),isltyp(i,j),  &
                prcpms, newsnms,snwe,snhei,snowfrac,             &
                rhosn,rhonewsn,rhosnfall,                        &
                snowrat,grauprat,icerat,curat,                   &
                patm,tabs,qvatm,qcatm,rho,                       &
                glw(i,j),gsw(i,j),emissl(i,j),                   &
                qkms,tkms,pc(i,j),lmavail(i,j),                  &
                canwatr,vegfra(i,j),alb(i,j),znt(i,j),           &
                snoalb(i,j),albbck(i,j),lai(i,j),                &   !new
                myj,seaice(i,j),isice,                           &
!--- soil fixed fields
                qwrtz,                                           &
                rhocs,dqm,qmin,ref,                              &
                wilt,psis,bclh,ksat,                             &
                sat,cn,zsmain,zshalf,dtdzs,dtdzs2,tbq,           &
!--- constants
                cp,rovcp,g0,lv,stbolt,cw,c1sn,c2sn,              &
                kqwrtz,kice,kwt,                                 &
!--- output variables
                snweprint,snheiprint,rsm,                        &
                soilm1d,tso1d,smfrkeep,keepfr,                   &
                soilt(i,j),soilt1(i,j),tsnav(i,j),dew(i,j),      &
                qvg(i,j),qsg(i,j),qcg(i,j),smelt(i,j),           &
                snoh(i,j),snflx(i,j),snom(i,j),snowfallac(i,j),  &
                acsnow(i,j),edir(i,j),ec(i,j),ett(i,j),qfx(i,j), &
                lh(i,j),hfx(i,j),sflx(i,j),sublim(i,j),          &
                evapl(i,j),prcpl(i,j),budget(i,j),runoff1(i,j),  &
                runoff2(i,j),soilice,soiliqw,infiltrp,smf(i,j))
!-----------------------------------------------------------------

! irrigation: fraction of cropland category in the grid box should not have soil moisture below
! wilting point during the growing season.
! let's keep soil moisture 10% above wilting point for the fraction of grid box under
! croplands.
! this change violates lsm moisture budget, but
! can be considered as a compensation for irrigation not included into lsm. 

    if(mosaic_lu == 1) then
      ! greenness factor: between 0 for min greenness and 1 for max greenness.
      factor = max(0.,min(1.,(vegfra(i,j)-shdmin(i,j))/max(1.,(shdmax(i,j)-shdmin(i,j)))))

      if ((lufrac(crop) > 0 .or. lufrac(natural) > 0.).and. factor > 0.75) then
      ! cropland or grassland, apply irrigation during the growing seaspon when
      ! factor is > 0.75.
        do k=1,nroot
             cropsm=1.1*wilt - qmin
          cropfr = min(1.,lufrac(crop) + 0.4*lufrac(natural)) ! assume that 40% of natural is cropland
          newsm = cropsm*cropfr + (1.-cropfr)*soilm1d(k)
          if(soilm1d(k) < newsm) then
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
print * ,'soil moisture is below wilting in cropland category at time step',ktau  &
              ,'i,j,lufrac(crop),k,soilm1d(k),wilt,cropsm',                       &
                i,j,lufrac(crop),k,soilm1d(k),wilt,cropsm
    endif
            soilm1d(k) = newsm
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print * ,'added soil water to grassland category, i,j,k,soilm1d(k)',i,j,k,soilm1d(k)
    endif
          endif
        enddo
      endif ! crop or natural
    endif ! mosaic_lu

! fill in field_sf to pass perturbed field of hydraulic cond. up to model driver and output
#if (EM_CORE==1)
       if (spp_lsm==1) then
         do k=1,nsl
           field_sf(i,k,j)=field_sf_loc(i,k,j)
         enddo
       endif
#endif

!***  diagnostics
!--- available and maximum soil moisture content in the soil
!--- domain

        smavail(i,j) = 0.
        smmax (i,j)  = 0.  

      do k=1,nzs-1
        smavail(i,j)=smavail(i,j)+(qmin+soilm1d(k))*             &
                    (zshalf(k+1)-zshalf(k))
        smmax (i,j) =smmax (i,j)+(qmin+dqm)*                     &
                    (zshalf(k+1)-zshalf(k))
      enddo

        smavail(i,j)=smavail(i,j)+(qmin+soilm1d(nzs))*           &
                    (zsmain(nzs)-zshalf(nzs))
        smmax (i,j) =smmax (i,j)+(qmin+dqm)*                     &
                    (zsmain(nzs)-zshalf(nzs))

!--- convert the water unit into mm
        sfcrunoff(i,j) = sfcrunoff(i,j)+runoff1(i,j)*dt*1000.0
        udrunoff (i,j) = udrunoff(i,j)+runoff2(i,j)*dt*1000.0
        acrunoff(i,j)  = acrunoff(i,j)+runoff1(i,j)*dt*1000.0
        smavail  (i,j) = smavail(i,j) * 1000.
        smmax    (i,j) = smmax(i,j) * 1000.
        smtotold (i,j) = smtotold(i,j) * 1000.

        do k=1,nzs

             soilmois(i,k,j) = soilm1d(k) + qmin
             sh2o    (i,k,j) = min(soiliqw(k) + qmin,soilmois(i,k,j))
                  tso(i,k,j) = tso1d(k)
        enddo

        tso(i,nzs,j) = tbot(i,j)

        do k=1,nzs
             smfr3d(i,k,j) = smfrkeep(k)
           keepfr3dflag(i,k,j) = keepfr (k)
        enddo

        z0       (i,j) = znt (i,j)
        sfcexc   (i,j) = tkms
        patmb=p8w(i,1,j)*1.e-2
        q2sat=qsn(tabs,tbq)/patmb
        qsfc(i,j) = qvg(i,j)/(1.+qvg(i,j))
! for myj surface and pbl scheme
!      if (myj) then
! myjsfc expects qsfc as actual specific humidity at the surface
        if((qvatm.ge.q2sat*0.95).and.qvatm.lt.qvg(i,j))then
          chklowq(i,j)=0.
        else
          chklowq(i,j)=1.
        endif

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      if(chklowq(i,j).eq.0.) then
   print *,'i,j,chklowq',  &
                  i,j,chklowq(i,j)
      endif
    endif

        if(snow(i,j)==0.) emissl(i,j) = lemitbl(ivgtyp(i,j))
        emiss (i,j) = emissl(i,j)
! snow is in [mm], snwe is in [m]; canwat is in mm, canwatr is in m
        snow   (i,j) = snwe*1000.
        snowh  (i,j) = snhei 
        canwat (i,j) = canwatr*1000.

        infiltr(i,j) = infiltrp

        mavail (i,j) = lmavail(i,j)  
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,' land, i=,j=, qfx, hfx after sfctmp', i,j,lh(i,j),hfx(i,j)
    endif
        sfcevp (i,j) = sfcevp (i,j) + qfx (i,j) * dt
        grdflx (i,j) = -1. * sflx(i,j)

!       if(smf(i,j) .ne.0.) then
!tgs - smf.ne.0. when there is phase change in the top soil layer
! the heat of soil water freezing/thawing is not computed explicitly
! and is responsible for the residual in the energy budget.
!  print *,'budget',budget(i,j),i,j,smf(i,j)
!       endif

!--- snowc snow cover flag
       if(snowfrac > 0. .and. xice(i,j).ge.xice_threshold ) then
           snowfrac = snowfrac*xice(i,j)
       endif

       snowc(i,j)=snowfrac

!--- rhosnf - density of snowfall
       rhosnf(i,j)=rhosnfall

! accumulated moisture flux [kg/m^2]
       sfcevp (i,j) = sfcevp (i,j) + qfx (i,j) * dt

!       if(smf(i,j) .ne.0.) then
!tgs - smf.ne.0. when there is phase change in the top soil layer
! the heat of freezing/thawing of soil water is not computed explicitly
! and is responsible for the residual in the energy budget.
!       endif
!        budget(i,j)=budget(i,j)-smf(i,j)

       ac=0.
       as=0.

       ac=max(0.,canwat(i,j)-canwatold(i,j))
       as=max(0.,snwe-snowold(i,j))
       wb =rainbl(i,j)+smelt(i,j)*dt*1.e3 & ! source
                      -qfx(i,j)*dt &
                      -runoff1(i,j)*dt*1.e3-runoff2(i,j)*dt*1.e3 &
                      -ac-as - (smavail(i,j)-smtotold(i,j))

       waterbudget(i,j)=rainbl(i,j)+smelt(i,j)*dt*1.e3 & ! source
                      -qfx(i,j)*dt &
                      -runoff1(i,j)*dt*1.e3-runoff2(i,j)*dt*1.e3 &
                      -ac-as - (smavail(i,j)-smtotold(i,j))


       acwaterbudget(i,j)=acwaterbudget(i,j)+waterbudget(i,j)

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
  print *,'smf=',smf(i,j),i,j
  print *,'budget',budget(i,j),i,j
  print *,'runoff2= ', i,j,runoff2(i,j)
  print *,'water budget ', i,j,waterbudget(i,j)
  print *,'rainbl,qfx*dt,runoff1,smelt*dt*1.e3,smchange', &
          i,j,rainbl(i,j),qfx(i,j)*dt,runoff1(i,j)*dt*1.e3, &
          smelt(i,j)*dt*1.e3, &
          (smavail(i,j)-smtotold(i,j))

  print *,'snow,snowold',i,j,snwe,snowold(i,j)
  print *,'snow-snowold',i,j,max(0.,snwe-snowold(i,j))
  print *,'canwatold, canwat ',i,j,canwatold(i,j),canwat(i,j)
  print *,'canwat(i,j)-canwatold(i,j)',max(0.,canwat(i,j)-canwatold(i,j))
    endif


    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
   print *,'land, i,j,tso1d,soilm1d,soilt - end of time step',         &
                  i,j,tso1d,soilm1d,soilt(i,j)
   print *,'land, qfx, hfx after sfctmp', i,j,lh(i,j),hfx(i,j)
    endif

!--- end of a land or sea ice point
        endif
2999  continue ! lakes

      enddo

   enddo

!-----------------------------------------------------------------
   end subroutine lsmruc
!-----------------------------------------------------------------



   subroutine sfctmp (spp_lsm,rstochcol,fieldcol_sf,             &
                delt,ktau,conflx,i,j,                            &
!--- input variables
                nzs,nddzs,nroot,meltfactor,                      &
                iland,isoil,xland,ivgtyp,isltyp,prcpms,          &
                newsnms,snwe,snhei,snowfrac,                     &
                rhosn,rhonewsn,rhosnfall,                        &
                snowrat,grauprat,icerat,curat,                   &
                patm,tabs,qvatm,qcatm,rho,                       &
                glw,gsw,emiss,qkms,tkms,pc,                      &
                mavail,cst,vegfra,alb,znt,                       &
                alb_snow,alb_snow_free,lai,                      &
                myj,seaice,isice,                                &
!--- soil fixed fields
                qwrtz,rhocs,dqm,qmin,ref,wilt,psis,bclh,ksat,    &
                sat,cn,zsmain,zshalf,dtdzs,dtdzs2,tbq,           &
!--- constants
                cp,rovcp,g0,lv,stbolt,cw,c1sn,c2sn,              &
                kqwrtz,kice,kwt,                                 &
!--- output variables
                snweprint,snheiprint,rsm,                        &
                soilm1d,ts1d,smfrkeep,keepfr,soilt,soilt1,       &
                tsnav,dew,qvg,qsg,qcg,                           &
                smelt,snoh,snflx,snom,snowfallac,acsnow,         &
                edir1,ec1,ett1,eeta,qfx,hfx,s,sublim,            &
                evapl,prcpl,fltot,runoff1,runoff2,soilice,       &
                soiliqw,infiltr,smf)
!-----------------------------------------------------------------
       implicit none
!-----------------------------------------------------------------

!--- input variables

   integer,  intent(in   )   ::  isice,i,j,nroot,ktau,nzs ,      &
                                 nddzs                             !nddzs=2*(nzs-2)

   real,     intent(in   )   ::  delt,conflx,meltfactor
   real,     intent(in   )   ::  c1sn,c2sn
   logical,    intent(in   )    ::     myj
!--- 3-d atmospheric variables
   real                                                        , &
            intent(in   )    ::                            patm, &
                                                           tabs, &
                                                          qvatm, &
                                                          qcatm
   real                                                        , &
            intent(in   )    ::                             glw, &
                                                            gsw, &
                                                             pc, &
                                                         vegfra, &
                                                  alb_snow_free, &
                                                            lai, &
                                                         seaice, &
                                                          xland, &
                                                            rho, &
                                                           qkms, &
                                                           tkms
                                                             
   integer,   intent(in   )  ::                          ivgtyp, isltyp
!--- 2-d variables
   real                                                        , &
            intent(inout)    ::                           emiss, &
                                                         mavail, &
                                                       snowfrac, &
                                                       alb_snow, &
                                                            alb, &
                                                            cst

!--- soil properties
   real                      ::                                  &
                                                          rhocs, &
                                                           bclh, &
                                                            dqm, &
                                                           ksat, &
                                                           psis, &
                                                           qmin, &
                                                          qwrtz, &
                                                            ref, &
                                                            sat, &
                                                           wilt

   real,     intent(in   )   ::                              cn, &
                                                             cw, &
                                                             cp, &
                                                          rovcp, &
                                                             g0, &
                                                             lv, &
                                                         stbolt, &
                                                         kqwrtz, &
                                                           kice, &
                                                            kwt

   real,     dimension(1:nzs), intent(in)  ::            zsmain, &
                                                         zshalf, &
                                                         dtdzs2 

   real,     dimension(1:nzs), intent(in)  ::          rstochcol
   real,     dimension(1:nzs), intent(inout) ::     fieldcol_sf


   real,     dimension(1:nddzs), intent(in)  ::           dtdzs

   real,     dimension(1:5001), intent(in)  ::              tbq


!--- input/output variables
!-------- 3-d soil moisture and temperature
   real,     dimension( 1:nzs )                                , &
             intent(inout)   ::                            ts1d, & 
                                                        soilm1d, &
                                                       smfrkeep
   real,  dimension( 1:nzs )                                   , &
             intent(inout)   ::                          keepfr

   real,  dimension(1:nzs), intent(inout)  ::          soilice, &
                                                       soiliqw
          

   integer, intent(inout)    ::                     iland,isoil
   integer                   ::                     ilands

!-------- 2-d variables
   real                                                        , &
             intent(inout)   ::                             dew, &
                                                          edir1, &
                                                            ec1, &
                                                           ett1, &
                                                           eeta, &
                                                          evapl, &
                                                        infiltr, &
                                                          rhosn, & 
                                                       rhonewsn, &
                                                      rhosnfall, &
                                                        snowrat, &
                                                       grauprat, &
                                                         icerat, &
                                                          curat, &
                                                         sublim, &
                                                          prcpl, &
                                                            qvg, &
                                                            qsg, &
                                                            qcg, &
                                                            qfx, &
                                                            hfx, &
                                                          fltot, &
                                                            smf, &
                                                              s, &  
                                                        runoff1, &
                                                        runoff2, &
                                                         acsnow, &
                                                     snowfallac, &
                                                           snwe, &
                                                          snhei, &
                                                          smelt, &
                                                           snom, &
                                                           snoh, &
                                                          snflx, &
                                                          soilt, &
                                                         soilt1, &
                                                          tsnav, &
                                                            znt

   real,     dimension(1:nzs)              ::                    &
                                                           tice, &
                                                        rhosice, &
                                                         capice, &
                                                       thdifice, &
                                                          ts1ds, &
                                                       soilm1ds, &
                                                      smfrkeeps, &
                                                       soiliqws, & 
                                                       soilices, &
                                                        keepfrs
!-------- 1-d variables
   real :: &
                                                            dews, &
                                                        mavails,  &
                                                          edir1s, &
                                                            ec1s, &
                                                            csts, &
                                                           ett1s, &
                                                           eetas, &
                                                          evapls, &
                                                        infiltrs, &
                                                          prcpls, &
                                                            qvgs, &
                                                            qsgs, &
                                                            qcgs, &
                                                            qfxs, &
                                                            hfxs, &
                                                          fltots, &
                                                        runoff1s, &
                                                        runoff2s, &
                                                              ss, &
                                                          soilts

            
                     

   real,  intent(inout)                     ::              rsm, &  
                                                      snweprint, &
                                                     snheiprint
   integer,   intent(in)                    ::     spp_lsm     
!--- local variables

   integer ::  k,ilnb

   real    ::  bsn, xsn                                        , &
               rainf, snth, newsn, prcpms, newsnms             , &
               t3, upflux, xinet
   real    ::  snhei_crit, snhei_crit_newsn, keep_snow_albedo, snowfracnewsn
   real    ::  newsnowratio, dd1, snowfrac2, m

   real    ::  rhonewgr,rhonewice

   real    ::  rnet,gswnew,gswin,emissn,zntsn,emiss_snowfree
   real    ::  vegfrac, snow_mosaic, snfr, vgfr
   real    ::  cice, albice, albsn, drip, dripsn, dripliq
   real    ::  interw, intersn, infwater, intwratio

!-----------------------------------------------------------------
        integer,   parameter      ::      ilsnow=99 
        
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
        print *,' in sfctmp',i,j,nzs,nddzs,nroot,                 &
                 snwe,rhosn,snom,smelt,ts1d
    endif

     !-- snow fraction options
     !-- option 1: original formulation using critical snow depth to compute
     !-- snow fraction
     !-- option 2: the tanh formulation from niu,g.-y.,and yang,z.-l.
     !2007,jgr,doi:10.1029/2007jd008674.
     !-- option 3: the tanh formulation from niu,g.-y.,and yang,z.-l.
     !2007,jgr,doi:10.1029/2007jd008674.
     !   with vegetation dependent parameters from noah mp (personal
     !   communication with mike barlage)
     !-- snhei_crit is a threshold for fractional snow in isncovr_opt=1
         snhei_crit=0.01601*rhowater/rhosn
         snhei_crit_newsn=0.0005*rhowater/rhosn
     !--
        zntsn = z0tbl(isice)

        snow_mosaic=0.
        snfr = 1.
        newsn=0.
        newsnowratio = 0.
        snowfracnewsn=0.
        rhonewsn = 100.
        if(snhei == 0.) snowfrac=0.
        smelt = 0.
        rainf = 0.
        rsm=0.
        dd1=0.
        infiltr=0.
! jul 2016 -  Avissar and Pielke (1989)
! this formulation depending on lai defines relative contribution of the vegetation to
! the total heat fluxes between surface and atmosphere.
! with vegfra=100% and lai=3, vegfrac=0.86 meaning that vegetation contributes
! only 86% of the total surface fluxes.
!        vgfr=0.01*vegfra ! % --> fraction
!        vegfrac=2.*lai*vgfr/(1.+2.*lai*vgfr)
        vegfrac=0.01*vegfra
        drip = 0.
        dripsn = 0.
        dripliq = 0.
        smf = 0.
        interw=0.
        intersn=0.
        infwater=0.

!---initialize local arrays for sea ice
          do k=1,nzs
            tice(k) = 0.
            rhosice(k) = 0. 
            cice = 0.
            capice(k) = 0.
            thdifice(k) = 0.
          enddo

        gswnew=gsw
        gswin=gsw/(1.-alb)
        albice=alb_snow_free
        albsn=alb_snow
        emissn = 0.98
        emiss_snowfree = lemitbl(ivgtyp)

!--- sea ice properties
!--- n.n Zubov "arctic ice"
!--- no salinity dependence because we consider the ice pack
!--- to be old and to have low salinity (0.0002)
       if(seaice.ge.0.5) then
          do k=1,nzs
            tice(k) = ts1d(k) - 273.15
            rhosice(k) = 917.6/(1-0.000165*tice(k))
            cice = 2115.85 +7.7948*tice(k)
            capice(k) = cice*rhosice(k)
            thdifice(k) = 2.260872/capice(k)
           enddo
!-- sea ice alb dependence on ice temperature. when ice temperature is
!-- below critical value of -10c - no change to albedo.
!-- if temperature is higher that -10c then albedo is decreasing.
!-- the minimum albedo at t=0c for ice is 0.1 less.
       albice = min(alb_snow_free,max(alb_snow_free - 0.05,   &
               alb_snow_free - 0.1*(tice(1)+10.)/10. ))
       endif

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
        print *,'alb_snow_free',alb_snow_free
        print *,'gsw,gswnew,glw,soilt,emiss,alb,albice,snwe',&
                 gsw,gswnew,glw,soilt,emiss,alb,albice,snwe
    endif

	if(snhei.gt.0.0081*1.e3/rhosn) then
!*** update snow density for current temperature (koren et al. 1999)
        bsn=delt/3600.*c1sn*exp(0.08*min(0.,tsnav)-c2sn*rhosn*1.e-3)
       if(bsn*snwe*100..lt.1.e-4) goto 777
        xsn=rhosn*(exp(bsn*snwe*100.)-1.)/(bsn*snwe*100.)
        rhosn=min(max(58.8,xsn),500.) ! 13mar18 - switch from 76.9 to 58.8
 777   continue

      endif

      !-- snow_mosaic from the previous time step 
      if(snowfrac < 0.75) snow_mosaic = 1.

           newsn=newsnms*delt

       if(newsn.gt.0.) then
!       if(newsn.ge.1.e-8) then

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *, 'there is new snow, newsn', newsn
    endif

        newsnowratio = min(1.,newsn/(snwe+newsn))

!--- 27 feb 2014 - empirical formulations from john m. brown
!        rhonewsn=min(250.,rhowater/max(4.179,(13.*tanh((274.15-tabs)*0.3333))))
!--- 13 mar 2018 - formulation from trevor alcott
        rhonewsn=min(125.,1000.0/max(8.,(17.*tanh((276.65-tabs)*0.15))))
        rhonewgr=min(500.,rhowater/max(2.,(3.5*tanh((274.15-tabs)*0.3333))))
        rhonewice=rhonewsn

!--- compute density of "snowfall" from weighted contribution
!                 of snow, graupel and ice fractions

         rhosnfall = min(500.,max(58.8,(rhonewsn*snowrat +  &  ! 13mar18-switch from 76.9 to 58.8
                     rhonewgr*grauprat + rhonewice*icerat + rhonewgr*curat)))

! from now on rhonewsn is the density of falling frozen precipitation
         rhonewsn=rhosnfall

!*** define average snow density of the snow pack considering
!*** the amount of fresh snow (eq. 9 in koren et al.(1999) 
!*** without snow melt )
         xsn=(rhosn*snwe+rhonewsn*newsn)/                         &
             (snwe+newsn)
         rhosn=min(max(58.8,xsn),500.) ! 13mar18 - switch from 76.9 to 58.8

       endif ! end newsn > 0.

       if(prcpms.ne.0.) then

! prcpms is liquid precipitation rate
! rainf is a flag used for calculation of rain water
! heat content contribution into heat budget equation. rain's temperature
! is set equal to air temperature at the first atmospheric
! level.  

           rainf=1.
       endif

        drip = 0.
        intwratio=0.
     if(vegfrac > 0.01) then
! compute intercepted precipitation - eq. 1 Lawrence et al.,
! j. of hydrometeorology, 2006, clm.
         interw=0.25*delt*prcpms*(1.-exp(-0.5*lai))*vegfrac
         intersn=0.25*newsn*(1.-exp(-0.5*lai))*vegfrac
         infwater=prcpms - interw/delt
    if((interw+intersn) > 0.) then
       intwratio=interw/(interw+intersn)
    endif

! update water/snow intercepted by the canopy
         dd1=cst + interw + intersn
         cst=dd1
        if(cst.gt.sat) then
          cst=sat
          drip=dd1-sat
        endif
     else
         cst=0.
         drip=0.
         interw=0.
         intersn=0.
         infwater=prcpms
     endif ! vegfrac > 0.01

       if(newsn.gt.0.) then
!update snow on the ground
         snwe=max(0.,snwe+newsn-intersn)
! add drip to snow on the ground
      if(drip > 0.) then
       if (snow_mosaic==1.) then
         dripliq=drip*intwratio
         dripsn = drip - dripliq
         snwe=snwe+dripsn
         infwater=infwater+dripliq
         dripliq=0.
         dripsn = 0.
       else
         snwe=snwe+drip
       endif
      endif
         snhei=snwe*rhowater/rhosn
         newsn=newsn*rhowater/rhonewsn
       endif

   if(snhei.gt.0.0) then
!-- snow on the ground
!--- land-use category should be changed to snow/ice for grid points with snow>0
         iland=isice
!24nov15 - based on field exp on pleasant view soccer fields
!    if(meltfactor > 1.5) then ! all veg. types, except forests
!         snhei_crit=0.01601*1.e3/rhosn
! petzold - 1 cm of fresh snow overwrites effects from old snow.
! need to test snhei_crit_newsn=0.01
!         snhei_crit_newsn=0.01
!    else  ! forests
!         snhei_crit=0.02*1.e3/rhosn
!         snhei_crit_newsn=0.001*1.e3/rhosn
!    endif

      if(isncovr_opt == 1) then
         snowfrac=min(1.,snhei/(2.*snhei_crit))
      elseif(isncovr_opt == 2) then
        snowfrac=min(1.,snhei/(2.*snhei_crit))
        !if(ivgtyp == glacier .or. ivgtyp == bare) then
        !-- sparsely vegetated or land ice
        !  snowfrac2 = tanh( snhei/(2.5 * 0.2 *(rhosn/rhonewsn)**1.))
        !else
          !-- Niu&Yang: znt=0.01 m for 1 degree (100km) resolution tests
          !  on 3-km scale use actual roughness, but not higher than 0.2 m.
          !  the factor is 20 for forests (~100/dx = 33., dx=3 km)
        snowfrac2 = tanh( snhei/(2.5 * min(0.2,znt) *(rhosn/rhonewsn)**1.))
        !endif
        !-- snow fraction is average between method 1 and 2
        snowfrac = 0.5*(snowfrac+snowfrac2)
      else
      !-- isncovr_opt=3
        !m = mfsno(ivgtyp) ! vegetation dependent facsnf/msnf from noah mp
        m = 1.
        !-- for rrfs a factor 10. was added to 'facsnf' to get reasonal values of
        !   snow cover fractions on the 3-km scale.
        !   this factor is scale dependent.
        snowfrac = tanh( snhei/(10. * sncovfac(ivgtyp)*(rhosn/rhonewsn)**m))
      endif

       if(newsn > 0. ) then
         snowfracnewsn=min(1.,snowfallac*1.e-3/snhei_crit_newsn)
       endif

!24nov15 - snowfrac for urban category < 0.75 
      if(ivgtyp == urban) snowfrac=min(0.75,snowfrac)
!      if(meltfactor > 1.5) then
!         if(isltyp > 9 .and. isltyp < 13) then
!24nov15 clay soil types - snofrac < 0.9
!           snowfrac=min(0.9,snowfrac)
!         endif
!      else
!24nov15 - snowfrac for forests < 0.75 
!         snowfrac=min(0.85,snowfrac)
!      endif

       if(snowfrac < 0.75) snow_mosaic = 1.

         keep_snow_albedo = 0.
       if (snowfracnewsn > 0.99 .and. rhosnfall < 450.) then
! new snow
             keep_snow_albedo = 1.
             snow_mosaic=0. 
     endif

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'snhei_crit,snowfrac,snhei_crit_newsn,snowfracnewsn', &
               snhei_crit,snowfrac,snhei_crit_newsn,snowfracnewsn
    endif

!-- set znt for snow from VEGPARM table (snow/ice landuse), except for
!-- land-use types with higher roughness (forests, urban).
      if(newsn.eq.0. .and. znt.le.0.2 .and. ivgtyp.ne.isice) then
         if( snhei .le. 2.*znt)then
           znt=0.55*znt+0.45*z0tbl(iland)
         elseif( snhei .gt. 2.*znt .and. snhei .le. 4.*znt)then
           znt=0.2*znt+0.8*z0tbl(iland)
         elseif(snhei > 4.*znt) then
           znt=z0tbl(iland)
         endif
       endif

    if(seaice .lt. 0.5) then
!----- snow on soil
!-- alb dependence on snow depth
! alb_snow across canada's forested areas is very low - 0.27-0.35, this
! causes significant warm biases. limiting alb in these areas to be higher than 0.4
! hwlps with these biases.. 
     if( snow_mosaic == 1.) then
         albsn=alb_snow
         if(keep_snow_albedo > 0.9 .and. albsn < 0.4) then
         !-- Albedo correction with fresh snow and deep snow pack
         !-- will reduce warm bias in western Canada
         !-- and US West coast, where max snow albedo is low (0.3-0.5).
           !print *,'ALB increase to 0.7',alb_snow,snhei,snhei_crit,albsn,i,j
           albsn = 0.7
         endif

         emiss= emissn
     else
         albsn   = max(keep_snow_albedo*alb_snow,               &
                   min((alb_snow_free +                         &
           (alb_snow - alb_snow_free) * snowfrac), alb_snow))
            if(newsn > 0. .and. keep_snow_albedo > 0.9 .and. albsn < 0.4) then
            !-- Albedo correction with fresh snow and deep snow pack
            !-- will reduce warm bias in western Canada
            !-- and US West coast, where max snow albedo is low (0.3-0.5).
            !print *,'ALB increase to 0.7',alb_snow,snhei,snhei_crit,albsn,i,j
              albsn = 0.7
            !print *,'NO mosaic ALB increase to 0.7',alb_snow,snhei,snhei_crit,alb,i,j
            endif

         emiss   = max(keep_snow_albedo*emissn,                 &
                   min((emiss_snowfree +                         &
           (emissn - emiss_snowfree) * snowfrac), emissn))
     endif
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
  print *,'snow on soil albsn,emiss,snow_mosaic',i,j,albsn,emiss,snow_mosaic
    endif
!28mar11  if canopy is covered with snow to 95% of its capacity and snow depth is
! higher than patchy snow treshold - then snow albedo is not less than 0.55
! (inspired by the flight from fairbanks to seatle)
!test      if(cst.ge.0.95*sat .and. snowfrac .gt.0.99)then
!        albsn=max(alb_snow,0.55)
!      endif

!-- alb dependence on snow temperature. when snow temperature is
!-- below critical value of -10c - no change to albedo.
!-- if temperature is higher that -10c then albedo is decreasing.
!-- the minimum albedo at t=0c for snow on land is 15% less than
!-- albedo of temperatures below -10c.
     if(albsn.lt.0.4 .or. keep_snow_albedo==1) then
        alb=albsn
      else
!-- change albedo when no fresh snow and snow albedo is higher than 0.5
        alb = min(albsn,max(albsn - 0.1*(soilt - 263.15)/       &
                (273.15-263.15)*albsn, albsn - 0.05))
      endif
    else
!----- snow on ice
     if( snow_mosaic == 1.) then
         albsn=alb_snow
         emiss= emissn
     else
         albsn   = max(keep_snow_albedo*alb_snow,               &
                   min((albice + (alb_snow - albice) * snowfrac), alb_snow))
         emiss   = max(keep_snow_albedo*emissn,                 &
                   min((emiss_snowfree +                        &
           (emissn - emiss_snowfree) * snowfrac), emissn))
     endif

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
  print *,'snow on ice snow_mosaic,albsn,emiss',i,j,albsn,emiss,snow_mosaic
    endif
!-- alb dependence on snow temperature. when snow temperature is
!-- below critical value of -10c - no change to albedo.
!-- if temperature is higher that -10c then albedo is decreasing.
      if(albsn.lt.alb_snow .or. keep_snow_albedo .eq.1.)then
       alb=albsn
      else
!-- change albedo when no fresh snow
       alb = min(albsn,max(albsn - 0.15*albsn*(soilt - 263.15)/  &
                (273.15-263.15), albsn - 0.1))
      endif

    endif

    if (snow_mosaic==1.) then 
!may 2014 - treat separately snow-free and snow-covered areas

       if(seaice .lt. 0.5) then
!  land
! portion not covered with snow
! compute absorbed gsw for snow-free portion

         gswnew=gswin*(1.-alb_snow_free)
!--------------
         t3      = stbolt*soilt*soilt*soilt
         upflux  = t3 *soilt
         xinet   = emiss_snowfree*(glw-upflux)
         rnet    = gswnew + xinet
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     print *,'fractional snow - snowfrac=',snowfrac
     print *,'snowfrac<1 gswin,gswnew -',gswin,gswnew,'soilt, rnet',soilt,rnet
    endif
           do k=1,nzs
          soilm1ds(k) = soilm1d(k)
          ts1ds(k) = ts1d(k)
          smfrkeeps(k) = smfrkeep(k)
          keepfrs(k) = keepfr(k)
          soilices(k) = soilice(k)
          soiliqws(k) = soiliqw(k)
            enddo
          soilts = soilt
          qvgs = qvg
          qsgs = qsg
          qcgs = qcg
          csts = cst
          mavails = mavail
          smelt=0.
          runoff1s=0.
          runoff2s=0.
       
          ilands = ivgtyp
         call soil(spp_lsm,rstochcol,fieldcol_sf,               &
!--- input variables
            i,j,ilands,isoil,delt,ktau,conflx,nzs,nddzs,nroot,   &
            prcpms,rainf,patm,qvatm,qcatm,glw,gswnew,gswin,     &
            emiss_snowfree,rnet,qkms,tkms,pc,csts,dripliq,      &
            infwater,rho,vegfrac,lai,myj,                       &
!--- soil fixed fields 
            qwrtz,rhocs,dqm,qmin,ref,wilt,                      &
            psis,bclh,ksat,sat,cn,                              &
            zsmain,zshalf,dtdzs,dtdzs2,tbq,                     &
!--- constants
            lv,cp,rovcp,g0,cw,stbolt,tabs,                      &
            kqwrtz,kice,kwt,                                    &
!--- output variables for snow-free portion
            soilm1ds,ts1ds,smfrkeeps,keepfrs,                   &
            dews,soilts,qvgs,qsgs,qcgs,edir1s,ec1s,             &
            ett1s,eetas,qfxs,hfxs,ss,evapls,prcpls,fltots,runoff1s, &
            runoff2s,mavails,soilices,soiliqws,                 &
            infiltrs,smf)
        else
! sea ice
! portion not covered with snow
! compute absorbed gsw for snow-free portion

         gswnew=gswin*(1.-albice)
!--------------
         t3      = stbolt*soilt*soilt*soilt
         upflux  = t3 *soilt
         xinet   = emiss_snowfree*(glw-upflux)
         rnet    = gswnew + xinet
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     print *,'fractional snow - snowfrac=',snowfrac
     print *,'snowfrac<1 gswin,gswnew -',gswin,gswnew,'soilt, rnet',soilt,rnet
    endif
            do k=1,nzs
          ts1ds(k) = ts1d(k)
            enddo
          soilts = soilt
          qvgs = qvg
          qsgs = qsg
          qcgs = qcg
          smelt=0.
          runoff1s=0.
          runoff2s=0.
 
          call sice(                                            &
!--- input variables
            i,j,iland,isoil,delt,ktau,conflx,nzs,nddzs,nroot,   &
            prcpms,rainf,patm,qvatm,qcatm,glw,gswnew,           &
            0.98,rnet,qkms,tkms,rho,myj,                        &
!--- sea ice parameters
            tice,rhosice,capice,thdifice,                       &
            zsmain,zshalf,dtdzs,dtdzs2,tbq,                     &
!--- constants
            lv,cp,rovcp,cw,stbolt,tabs,                         &
!--- output variable
            ts1ds,dews,soilts,qvgs,qsgs,qcgs,                   &
            eetas,qfxs,hfxs,ss,evapls,prcpls,fltots             &
                                                                )
           edir1 = eeta*1.e-3
           ec1 = 0.
           ett1 = 0.
           runoff1 = prcpms
           runoff2 = 0.
           mavail = 1.
           infiltr=0.
           cst=0.
            do k=1,nzs
               soilm1d(k)=1.
               soiliqw(k)=0.
               soilice(k)=1.
               smfrkeep(k)=1.
               keepfr(k)=0.
            enddo
        endif ! seaice < 0.5

!return gswnew to incoming solar
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     print *,'gswnew,alb_snow_free,alb',gswnew,alb_snow_free,alb
    endif

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'incoming gswnew snowfrac<1 -',gswnew
    endif
    endif ! snow_mosaic=1.
                           
!--- recompute absorbed solar radiation and net radiation
!--- for updated value of snow albedo - alb
         gswnew=gswin*(1.-alb)
!      print *,'snow fraction gswnew',gswnew,'alb=',alb
!--------------
         t3      = stbolt*soilt*soilt*soilt
         upflux  = t3 *soilt
         xinet   = emiss*(glw-upflux)
         rnet    = gswnew + xinet
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
        print *,'rnet=',rnet
        print *,'snow - i,j,newsn,snwe,snhei,gsw,gswnew,glw,upflux,alb',&
                 i,j,newsn,snwe,snhei,gsw,gswnew,glw,upflux,alb
    endif

      if (seaice .lt. 0.5) then
! land
           if(snow_mosaic==1.)then
              snfr=1.
           else
              snfr=snowfrac
           endif
         call snowsoil (spp_lsm,rstochcol,fieldcol_sf,     & !--- input variables
            i,j,isoil,delt,ktau,conflx,nzs,nddzs,nroot,         &
            meltfactor,rhonewsn,snhei_crit,                     &  ! new
            iland,prcpms,rainf,newsn,snhei,snwe,snfr,           &
            rhosn,patm,qvatm,qcatm,                             &
            glw,gswnew,gswin,emiss,rnet,ivgtyp,                 &
            qkms,tkms,pc,cst,dripsn,infwater,                   &
            rho,vegfrac,alb,znt,lai,                            &
            myj,                                                &
!--- soil fixed fields
            qwrtz,rhocs,dqm,qmin,ref,wilt,psis,bclh,ksat,       &
            sat,cn,zsmain,zshalf,dtdzs,dtdzs2,tbq,              & 
!--- constants
            lv,cp,rovcp,g0,cw,stbolt,tabs,                      &
            kqwrtz,kice,kwt,                                    &
!--- output variables
            ilnb,snweprint,snheiprint,rsm,                      &
            soilm1d,ts1d,smfrkeep,keepfr,                       &
            dew,soilt,soilt1,tsnav,qvg,qsg,qcg,                 &
            smelt,snoh,snflx,snom,edir1,ec1,ett1,eeta,          &
            qfx,hfx,s,sublim,prcpl,fltot,runoff1,runoff2,       &
            mavail,soilice,soiliqw,infiltr                      )
       else
! sea ice
           if(snow_mosaic==1.)then
              snfr=1.
           else
              snfr=snowfrac
           endif

         call snowseaice (                                      &
            i,j,isoil,delt,ktau,conflx,nzs,nddzs,               &    
            meltfactor,rhonewsn,snhei_crit,                     &  ! new
            iland,prcpms,rainf,newsn,snhei,snwe,snfr,           &    
            rhosn,patm,qvatm,qcatm,                             &    
            glw,gswnew,emiss,rnet,                              &    
            qkms,tkms,rho,myj,                                  &    
!--- sea ice parameters
            alb,znt,                                            &
            tice,rhosice,capice,thdifice,                       &    
            zsmain,zshalf,dtdzs,dtdzs2,tbq,                     &    
!--- constants
            lv,cp,rovcp,cw,stbolt,tabs,                         &    
!--- output variables
            ilnb,snweprint,snheiprint,rsm,ts1d,                 &    
            dew,soilt,soilt1,tsnav,qvg,qsg,qcg,                 &    
            smelt,snoh,snflx,snom,eeta,                         &    
            qfx,hfx,s,sublim,prcpl,fltot                        &    
                                                                )    
           edir1 = eeta*1.e-3
           ec1 = 0.
           ett1 = 0.
           runoff1 = smelt
           runoff2 = 0.
           mavail = 1.
           infiltr=0.
           cst=0.
            do k=1,nzs
               soilm1d(k)=1.
               soiliqw(k)=0.
               soilice(k)=1.
               smfrkeep(k)=1.
               keepfr(k)=0.
            enddo
       endif


     if (snow_mosaic==1.) then
! may 2014 - now combine snow covered and snow-free land fluxes, soil temp, moist,
! etc.
        if(seaice .lt. 0.5) then
! land
   if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'soilt snow on land', ktau, i,j,soilt
      print *,'soilt on snow-free land', i,j,soilts
      print *,'ts1d,ts1ds',i,j,ts1d,ts1ds
      print *,' snow flux',i,j, snflx
      print *,' ground flux on snow-covered land',i,j, s
      print *,' ground flux on snow-free land', i,j,ss
      print *,' csts, cst', i,j,csts,cst
   endif
            do k=1,nzs
          soilm1d(k) = soilm1ds(k)*(1.-snowfrac) + soilm1d(k)*snowfrac
          ts1d(k) = ts1ds(k)*(1.-snowfrac) + ts1d(k)*snowfrac
          smfrkeep(k) = smfrkeeps(k)*(1.-snowfrac) + smfrkeep(k)*snowfrac
       if(snowfrac > 0.5) then
          keepfr(k) = keepfr(k)
       else
          keepfr(k) = keepfrs(k)
       endif
          soilice(k) = soilices(k)*(1.-snowfrac) + soilice(k)*snowfrac
          soiliqw(k) = soiliqws(k)*(1.-snowfrac) + soiliqw(k)*snowfrac
            enddo
          dew = dews*(1.-snowfrac) + dew*snowfrac
          soilt = soilts*(1.-snowfrac) + soilt*snowfrac
          qvg = qvgs*(1.-snowfrac) + qvg*snowfrac
          qsg = qsgs*(1.-snowfrac) + qsg*snowfrac
          qcg = qcgs*(1.-snowfrac) + qcg*snowfrac
          edir1 = edir1s*(1.-snowfrac) + edir1*snowfrac
          ec1 = ec1s*(1.-snowfrac) + ec1*snowfrac
          cst = csts*(1.-snowfrac) + cst*snowfrac
          ett1 = ett1s*(1.-snowfrac) + ett1*snowfrac
          eeta = eetas*(1.-snowfrac) + eeta*snowfrac
          qfx = qfxs*(1.-snowfrac) + qfx*snowfrac
          hfx = hfxs*(1.-snowfrac) + hfx*snowfrac
          s = ss*(1.-snowfrac) + s*snowfrac
          evapl = evapls*(1.-snowfrac)
          sublim = sublim*snowfrac
          prcpl = prcpls*(1.-snowfrac) + prcpl*snowfrac
          fltot = fltots*(1.-snowfrac) + fltot*snowfrac
!alb
          alb   = max(keep_snow_albedo*alb,              &
                  min((alb_snow_free + (alb - alb_snow_free) * snowfrac), alb))

          emiss = max(keep_snow_albedo*emissn,           &
                  min((emiss_snowfree +                  &
              (emissn - emiss_snowfree) * snowfrac), emissn))

          runoff1 = runoff1s*(1.-snowfrac) + runoff1*snowfrac
          runoff2 = runoff2s*(1.-snowfrac) + runoff2*snowfrac
          mavail = mavails*(1.-snowfrac) + 1.*snowfrac
          infiltr = infiltrs*(1.-snowfrac) + infiltr*snowfrac

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,' ground flux combined', i,j, s
      print *,'soilt combined on land', soilt
      print *,'ts combined on land', ts1d
    endif
       else
! sea ice
! now combine fluxes for snow-free sea ice and snow-covered area
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'soilt snow on ice', soilt
    endif
            do k=1,nzs
          ts1d(k) = ts1ds(k)*(1.-snowfrac) + ts1d(k)*snowfrac
            enddo
          dew = dews*(1.-snowfrac) + dew*snowfrac
          soilt = soilts*(1.-snowfrac) + soilt*snowfrac
          qvg = qvgs*(1.-snowfrac) + qvg*snowfrac
          qsg = qsgs*(1.-snowfrac) + qsg*snowfrac
          qcg = qcgs*(1.-snowfrac) + qcg*snowfrac
          eeta = eetas*(1.-snowfrac) + eeta*snowfrac
          qfx = qfxs*(1.-snowfrac) + qfx*snowfrac
          hfx = hfxs*(1.-snowfrac) + hfx*snowfrac
          s = ss*(1.-snowfrac) + s*snowfrac
          sublim = eeta
          prcpl = prcpls*(1.-snowfrac) + prcpl*snowfrac
          fltot = fltots*(1.-snowfrac) + fltot*snowfrac
!alb
          alb   = max(keep_snow_albedo*alb,              &
                  min((albice + (alb - alb_snow_free) * snowfrac), alb))

          emiss = max(keep_snow_albedo*emissn,           &
                  min((emiss_snowfree +                  &
              (emissn - emiss_snowfree) * snowfrac), emissn))

          runoff1 = runoff1s*(1.-snowfrac) + runoff1*snowfrac
          runoff2 = runoff2s*(1.-snowfrac) + runoff2*snowfrac
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'soilt combined on ice', soilt
    endif
       endif      
     endif ! snow_mosaic = 1.
 
     if(snhei.eq.0.) then
     !-- all snow is melted
       alb=alb_snow_free
       iland=ivgtyp
     else
     !-- snow on the ground
      if(isncovr_opt == 1) then
         snowfrac=min(1.,snhei/(2.*snhei_crit))
      elseif(isncovr_opt == 2) then
        snowfrac=min(1.,snhei/(2.*snhei_crit))
        !if(ivgtyp == glacier .or. ivgtyp == bare) then
        !-- sparsely vegetated or land ice
        !  snowfrac2 = tanh( snhei/(2.5 * 0.2 *(rhosn/rhonewsn)**1.))
        !else
          !-- niu&yang: znt=0.01 m for 1 degree (100km) resolution tests
          !  on 3-km scale use actual roughness, but not higher than 0.2 m.
          !  the factor is 20 for forests (~100/dx = 33.)
        snowfrac2 = tanh( snhei/(2.5 * min(0.2,znt) *(rhosn/rhonewsn)**1.))
        !endif
        !-- snow fraction is average between method 1 and 2
        snowfrac = 0.5*(snowfrac+snowfrac2)
      else
      !-- isncovr_opt=3
        !m = mfsno(ivgtyp) ! vegetation dependent facsnf/msnf from noah mp
        m = 1.
        !-- for rrfs a factor 10. was added to 'facsnf' to get reasonal values
        !of
        !   snow cover fractions on the 3-km scale.
        !   this factor is scale dependent.
        snowfrac = tanh( snhei/(10. * sncovfac(ivgtyp)*(rhosn/rhonewsn)**m))
      endif

     endif

     if(ivgtyp == urban) snowfrac=min(0.75,snowfrac)

!  run-total accumulated snow based on snowfall and snowmelt in [m]

      snowfallac = snowfallac + newsn * 1.e3    ! accumulated snow depth [mm], using variable snow den
      !snowfallac = snowfallac + max(0.,(newsn - rhowater/rhonewsn*smelt*delt*newsnowratio))

   else
!--- no snow
           snheiprint=0.
           snweprint=0.
           smelt=0.

!--------------
         t3      = stbolt*soilt*soilt*soilt
         upflux  = t3 *soilt
         xinet   = emiss*(glw-upflux)
         rnet    = gswnew + xinet
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     print *,'no snow on the ground gswnew -',gswnew,'rnet=',rnet
    endif

       if(seaice .lt. 0.5) then
!  land
         call soil(spp_lsm,rstochcol,fieldcol_sf,               &
!--- input variables
            i,j,iland,isoil,delt,ktau,conflx,nzs,nddzs,nroot,   &
            prcpms,rainf,patm,qvatm,qcatm,glw,gswnew,gswin,     &
            emiss,rnet,qkms,tkms,pc,cst,drip,infwater,          &
            rho,vegfrac,lai,myj,                                &
!--- soil fixed fields 
            qwrtz,rhocs,dqm,qmin,ref,wilt,                      &
            psis,bclh,ksat,sat,cn,                              &
            zsmain,zshalf,dtdzs,dtdzs2,tbq,                     &
!--- constants
            lv,cp,rovcp,g0,cw,stbolt,tabs,                      &
            kqwrtz,kice,kwt,                                    &
!--- output variables
            soilm1d,ts1d,smfrkeep,keepfr,                       &
            dew,soilt,qvg,qsg,qcg,edir1,ec1,                    &
            ett1,eeta,qfx,hfx,s,evapl,prcpl,fltot,runoff1,      &
            runoff2,mavail,soilice,soiliqw,                     &
            infiltr,smf)
        else
! sea ice
! if current ice albedo is not the same as from the previous time step, then
! update gsw, alb and rnet for surface energy budget
         if(alb.ne.albice) gswnew=gsw/(1.-alb)*(1.-albice)
         alb=albice
         rnet    = gswnew + xinet

          call sice(                                            &
!--- input variables
            i,j,iland,isoil,delt,ktau,conflx,nzs,nddzs,nroot,   &
            prcpms,rainf,patm,qvatm,qcatm,glw,gswnew,           &
            emiss,rnet,qkms,tkms,rho,myj,                       &
!--- sea ice parameters
            tice,rhosice,capice,thdifice,                       &
            zsmain,zshalf,dtdzs,dtdzs2,tbq,                     &
!--- constants
            lv,cp,rovcp,cw,stbolt,tabs,                         &
!--- output variables
            ts1d,dew,soilt,qvg,qsg,qcg,                         &
            eeta,qfx,hfx,s,evapl,prcpl,fltot                          &
                                                                )
           edir1 = eeta*1.e-3
           ec1 = 0.
           ett1 = 0.
           runoff1 = prcpms
           runoff2 = 0.
           mavail = 1.
           infiltr=0.
           cst=0.
            do k=1,nzs
               soilm1d(k)=1.
               soiliqw(k)=0.
               soilice(k)=1.
               smfrkeep(k)=1.
               keepfr(k)=0.
            enddo
        endif

        endif

!      return
!       end
!---------------------------------------------------------------
   end subroutine sfctmp
!---------------------------------------------------------------


       function qsn(tn,t)
!****************************************************************
   real,     dimension(1:5001),  intent(in   )   ::  t
   real,     intent(in  )   ::  tn

      real    qsn, r,r1,r2
      integer i

       r=(tn-173.15)/.05+1.
       i=int(r)
       if(i.ge.1) goto 10
       i=1
       r=1.
  10   if(i.le.5000) goto 20
       i=5000
       r=5001.
  20   r1=t(i)
       r2=r-i
       qsn=(t(i+1)-r1)*r2 + r1
!       print *,' in qsn, i,r,r1,r2,t(i+1),tn, qsn', i,r,r1,r2,t(i+1),tn,qsn
!       return
!       end
!-----------------------------------------------------------------------
  end function qsn
!------------------------------------------------------------------------


        subroutine soil (spp_lsm,rstochcol, fieldcol_sf,     &
!--- input variables
            i,j,iland,isoil,delt,ktau,conflx,nzs,nddzs,nroot,&
            prcpms,rainf,patm,qvatm,qcatm,                   &
            glw,gsw,gswin,emiss,rnet,                        &
            qkms,tkms,pc,cst,drip,infwater,rho,vegfrac,lai,  &
            myj,                                             &
!--- soil fixed fields
            qwrtz,rhocs,dqm,qmin,ref,wilt,psis,bclh,ksat,    &
            sat,cn,zsmain,zshalf,dtdzs,dtdzs2,tbq,           &
!--- constants
            xlv,cp,rovcp,g0_p,cw,stbolt,tabs,                &
            kqwrtz,kice,kwt,                                 &
!--- output variables
            soilmois,tso,smfrkeep,keepfr,                    &
            dew,soilt,qvg,qsg,qcg,                           &
            edir1,ec1,ett1,eeta,qfx,hfx,s,evapl,             &
            prcpl,fltot,runoff1,runoff2,mavail,soilice,      &
            soiliqw,infiltrp,smf)

!*************************************************************
!   energy and moisture budget for vegetated surfaces 
!   without snow, heat diffusion and richards eqns. in
!   soil
!
!     delt - time step (s)
!     ktau - numver of time step
!     conflx - depth of constant flux layer (m)
!     j,i - the location of grid point
!     ime, jme, kme, nzs - dimensions of the domain
!     nroot - number of levels within the root zone
!     prcpms - precipitation rate in m/s
!     patm - pressure [bar]
!     qvatm,qcatm - cloud and water vapor mixing ratio (kg/kg)
!                   at the first atm. level
!     glw, gsw - incoming longwave and absorbed shortwave
!                radiation at the surface (w/m^2)
!     emiss,rnet - emissivity of the ground surface (0-1) and net
!                  radiation at the surface (w/m^2)
!     qkms - exchange coefficient for water vapor in the
!              surface layer (m/s)
!     tkms - exchange coefficient for heat in the surface
!              layer (m/s)
!     pc - plant coefficient (resistance) (0-1)
!     rho - density of atmosphere near sueface (kg/m^3)
!     vegfrac - greeness fraction
!     rhocs - volumetric heat capacity of dry soil
!     dqm, qmin - porosity minus residual soil moisture qmin (m^3/m^3)
!     ref, wilt - field capacity soil moisture and the
!                 wilting point (m^3/m^3)
!     psis - matrix potential at saturation (m)
!     bclh - exponent for clapp-hornberger parameterization
!     ksat - saturated hydraulic conductivity (m/s)
!     sat - maximum value of water intercepted by canopy (m)
!     cn - exponent for calculation of canopy water
!     zsmain - main levels in soil (m)
!     zshalf - middle of the soil layers (m)
!     dtdzs,dtdzs2 - dt/(2.*dzshalf*dzmain) and dt/dzshalf in soil
!     tbq - table to define saturated mixing ration
!           of water vapor for given temperature and pressure
!     soilmois,tso - soil moisture (m^3/m^3) and temperature (k)
!     dew -  dew in kg/m^2s
!     soilt - skin temperature (k)
!     qsg,qvg,qcg - saturated mixing ratio, mixing ratio of
!                   water vapor and cloud at the ground
!                   surface, respectively (kg/kg)
!     edir1, ec1, ett1, eeta - direct evaporation, evaporation of
!            canopy water, transpiration in kg m-2 s-1 and total
!            evaporation in m s-1.
!     qfx, hfx - latent and sensible heat fluxes (w/m^2)
!     s - soil heat flux in the top layer (w/m^2)
!     runoff - surface runoff (m/s)
!     runoff2 - underground runoff (m)
!     mavail - moisture availability in the top soil layer (0-1)
!     infiltrp - infiltration flux from the top of soil domain (m/s)
!
!*****************************************************************
        implicit none
!-----------------------------------------------------------------

!--- input variables

   integer,  intent(in   )   ::  nroot,ktau,nzs                , &
                                 nddzs                    !nddzs=2*(nzs-2)
   integer,  intent(in   )   ::  i,j,iland,isoil
   real,     intent(in   )   ::  delt,conflx
   logical,  intent(in   )   ::  myj
!--- 3-d atmospheric variables
   real,                                                         &
            intent(in   )    ::                            patm, &
                                                          qvatm, &
                                                          qcatm
!--- 2-d variables
   real,                                                         &
            intent(in   )    ::                             glw, &
                                                            gsw, &
                                                          gswin, &
                                                          emiss, &
                                                            rho, &
                                                             pc, &
                                                        vegfrac, &
                                                            lai, &
                                                       infwater, &
                                                           qkms, &
                                                           tkms

!--- soil properties
   real,                                                         &
            intent(in   )    ::                           rhocs, &
                                                           bclh, &
                                                            dqm, &
                                                           ksat, &
                                                           psis, &
                                                           qmin, &
                                                          qwrtz, &
                                                            ref, &
                                                           wilt

   real,     intent(in   )   ::                              cn, &
                                                             cw, &
                                                         kqwrtz, &
                                                           kice, &
                                                            kwt, &
                                                            xlv, &
                                                            g0_p


   real,     dimension(1:nzs), intent(in)  ::            zsmain, &
                                                         zshalf, &
                                                         dtdzs2

   real,     dimension(1:nddzs), intent(in)  ::           dtdzs

   real,     dimension(1:5001), intent(in)  ::              tbq


!--- input/output variables
!-------- 3-d soil moisture and temperature
   real,     dimension( 1:nzs )                                , &
             intent(inout)   ::                             tso, &
                                                       soilmois, &
                                                       smfrkeep

   real,     dimension(1:nzs), intent(in)  ::          rstochcol
   real,     dimension(1:nzs), intent(inout) ::     fieldcol_sf


   real,     dimension( 1:nzs )                                , &
             intent(inout)   ::                          keepfr

!-------- 2-d variables
   real,                                                         &
             intent(inout)   ::                             dew, &
                                                            cst, &
                                                           drip, &
                                                          edir1, &
                                                            ec1, &
                                                           ett1, &
                                                           eeta, &
                                                          evapl, &
                                                          prcpl, &
                                                         mavail, &
                                                            qvg, &
                                                            qsg, &
                                                            qcg, &
                                                           rnet, &
                                                            qfx, &
                                                            hfx, &
                                                              s, &
                                                            sat, &
                                                        runoff1, &
                                                        runoff2, &
                                                          soilt

!-------- 1-d variables
   integer                   , intent(in)  ::      spp_lsm   
   real,     dimension(1:nzs), intent(out)  ::          soilice, &
                                                        soiliqw

!--- local variables

   real    ::  infiltrp, transum                               , &
               rainf,  prcpms                                  , &
               tabs, t3, upflux, xinet
   real    ::  cp,rovcp,g0,lv,stbolt,xlmelt,dzstop             , &
               can,epot,fac,fltot,ft,fq,hft                    , &
               q1,ras,rhoice,sph                               , &
               trans,zn,ci,cvw,tln,tavln,pi                    , &
               dd1,cmc2ms,drycan,wetcan                        , &
               infmax,riw, x
   real,     dimension(1:nzs)  ::  transp,cap,diffu,hydro      , &
                                   thdif,tranf,tav,soilmoism   , &
                                   soilicem,soiliqwm,detal     , &
                                   fwsat,lwsat,told,smold

   real                        ::  soiltold,smf
   real    :: soilres, alfa, fex, fex_fc, fc, psit

   integer ::  nzs1,nzs2,k

!-----------------------------------------------------------------

!-- define constants
        rhoice=900.
        ci=rhoice*2100.
        xlmelt=3.35e+5
        cvw=cw

        prcpl=prcpms

        smf=0.
        soiltold = soilt

        wetcan=0.
        drycan=1.

!--- initializing local arrays
        do k=1,nzs
          transp   (k)=0.
          soilmoism(k)=0.
          soilice  (k)=0.
          soiliqw  (k)=0.
          soilicem (k)=0.
          soiliqwm (k)=0.
          lwsat    (k)=0.
          fwsat    (k)=0.
          tav      (k)=0.
          cap      (k)=0.
          thdif    (k)=0.
          diffu    (k)=0.
          hydro    (k)=0.   
          tranf    (k)=0.
          detal    (k)=0.
          told     (k)=0.
          smold    (k)=0.
        enddo

          nzs1=nzs-1
          nzs2=nzs-2
        dzstop=1./(zsmain(2)-zsmain(1))
        ras=rho*1.e-3
        riw=rhoice*1.e-3

!--- computation of volumetric content of ice in soil 

         do k=1,nzs
!- main levels
         tln=log(tso(k)/273.15)
         if(tln.lt.0.) then
           soiliqw(k)=(dqm+qmin)*(xlmelt*                        &
         (tso(k)-273.15)/tso(k)/9.81/psis)                       &
          **(-1./bclh)-qmin
           soiliqw(k)=max(0.,soiliqw(k))
           soiliqw(k)=min(soiliqw(k),soilmois(k))
           soilice(k)=(soilmois(k)-soiliqw(k))/riw

!---- melting and freezing is balanced, soil ice cannot increase
       if(keepfr(k).eq.1.) then
           soilice(k)=min(soilice(k),smfrkeep(k))
           soiliqw(k)=max(0.,soilmois(k)-soilice(k)*riw)
       endif

         else
           soilice(k)=0.
           soiliqw(k)=soilmois(k)
         endif

          enddo

          do k=1,nzs1
!- middle of soil layers
         tav(k)=0.5*(tso(k)+tso(k+1))
         soilmoism(k)=0.5*(soilmois(k)+soilmois(k+1))
         tavln=log(tav(k)/273.15)

         if(tavln.lt.0.) then
           soiliqwm(k)=(dqm+qmin)*(xlmelt*                       &
         (tav(k)-273.15)/tav(k)/9.81/psis)                       &
          **(-1./bclh)-qmin
           fwsat(k)=dqm-soiliqwm(k)
           lwsat(k)=soiliqwm(k)+qmin
           soiliqwm(k)=max(0.,soiliqwm(k))
           soiliqwm(k)=min(soiliqwm(k), soilmoism(k))
           soilicem(k)=(soilmoism(k)-soiliqwm(k))/riw
!---- melting and freezing is balanced, soil ice cannot increase
       if(keepfr(k).eq.1.) then
           soilicem(k)=min(soilicem(k),                          &
                   0.5*(smfrkeep(k)+smfrkeep(k+1)))
           soiliqwm(k)=max(0.,soilmoism(k)-soilicem(k)*riw)
           fwsat(k)=dqm-soiliqwm(k)
           lwsat(k)=soiliqwm(k)+qmin
       endif

         else
           soilicem(k)=0.
           soiliqwm(k)=soilmoism(k)
           lwsat(k)=dqm+qmin
           fwsat(k)=0.
         endif

          enddo

          do k=1,nzs
           if(soilice(k).gt.0.) then
             smfrkeep(k)=soilice(k)
           else
             smfrkeep(k)=soilmois(k)/riw
           endif
          enddo
!******************************************************************
! soilprop computes thermal diffusivity, and diffusional and
!          hydraulic condeuctivities
!******************************************************************
          call soilprop(spp_lsm,rstochcol,fieldcol_sf,       &
!--- input variables
               nzs,fwsat,lwsat,tav,keepfr,                        &
               soilmois,soiliqw,soilice,                          &
               soilmoism,soiliqwm,soilicem,                       &
!--- soil fixed fields
               qwrtz,rhocs,dqm,qmin,psis,bclh,ksat,               &
!--- constants
               riw,xlmelt,cp,g0_p,cvw,ci,                         &
               kqwrtz,kice,kwt,                                   &
!--- output variables
               thdif,diffu,hydro,cap)

!********************************************************************
!--- calculation of canopy water (Smirnova et al., 1996, eq.16) and dew 
 
!        drip=0.
!        dd1=0.

        fq=qkms

        q1=-qkms*ras*(qvatm - qsg)

        dew=0.
        if(qvatm.ge.qsg)then
          dew=fq*(qvatm-qsg)
        endif

!        if(dew.ne.0.)then
!          dd1=cst+delt*(prcpms +dew*ras)
!        else
!          dd1=cst+                                          &
!            delt*(prcpms+ras*fq*(qvatm-qsg)                 &
!           *(cst/sat)**cn)
!        endif

!          dd1=cst+delt*prcpms

!       if(dd1.lt.0.) dd1=0.
!        if(vegfrac.eq.0.)then
!          cst=0.
!          drip=0.
!        endif
!        if (vegfrac.gt.0.) then
!          cst=dd1
!        if(cst.gt.sat) then
!          cst=sat
!          drip=dd1-sat
!        endif
!        endif
!
!--- wetcan is the fraction of vegetated area covered by canopy
!--- water, and drycan is the fraction of vegetated area where
!--- transpiration may take place.

          wetcan=min(0.25,max(0.,(cst/sat))**cn)
!          if(lai > 1.) wetcan=wetcan/lai
          drycan=1.-wetcan

!**************************************************************
!  transf computes transpiration function
!**************************************************************
           call transf(i,j,                                   &
!--- input variables
              nzs,nroot,soiliqw,tabs,lai,gswin,               &
!--- soil fixed fields
              dqm,qmin,ref,wilt,zshalf,pc,iland,              &
!--- output variables
              tranf,transum)


!--- save soil temp and moisture from the beginning of time step
          do k=1,nzs
           told(k)=tso(k)
           smold(k)=soilmois(k)
          enddo

! sakaguchi and zeng (2009) - dry soil resistance to evaporation
!      if (vgtype==11) then   ! modis wetland
        alfa=1.
!      else
        fex=min(1.,soilmois(1)/dqm)
        fex=max(fex,0.01)
        psit=psis*fex ** (-bclh)
        psit = max(-1.e5, psit)
        alfa=min(1.,exp(g*psit/r_v/soilt))
!      endif
        alfa=1.
! field capacity
        fc=max(qmin,ref*0.5)
        fex_fc=1.
      if((soilmois(1)+qmin) > fc .or. (qvatm-qvg) > 0.) then
        soilres = 1.
      else
        fex_fc=min(1.,(soilmois(1)+qmin)/fc)
        fex_fc=max(fex_fc,0.01)
        soilres=0.25*(1.-cos(piconst*fex_fc))**2.
      endif
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
!    if (i==421.and.j==280) then
     print *,'fex,psit,psis,bclh,g,r_v,soilt,alfa,mavail,soilmois(1),fc,ref,soilres,fex_fc', &
              fex,psit,psis,bclh,g,r_v,soilt,alfa,mavail,soilmois(1),fc,ref,soilres,fex_fc
    endif

!**************************************************************
!  soiltemp soilves heat budget and diffusion eqn. in soil
!**************************************************************

        call soiltemp(                                        &
!--- input variables
             i,j,iland,isoil,                                 &
             delt,ktau,conflx,nzs,nddzs,nroot,                &
             prcpms,rainf,                                    &
             patm,tabs,qvatm,qcatm,emiss,rnet,                &
             qkms,tkms,pc,rho,vegfrac, lai,                   &
             thdif,cap,drycan,wetcan,                         & 
             transum,dew,mavail,soilres,alfa,                 &
!--- soil fixed fields
             dqm,qmin,bclh,zsmain,zshalf,dtdzs,tbq,           &
!--- constants
             xlv,cp,g0_p,cvw,stbolt,                          &
!--- output variables
             tso,soilt,qvg,qsg,qcg,x)

!************************************************************************

!--- calculation of dew using new value of qsg or transp if no dew
        ett1=0.
        dew=0.

        if(qvatm.ge.qsg)then
          dew=qkms*(qvatm-qsg)
          ett1=0.
          do k=1,nzs
            transp(k)=0.
          enddo
        else

          do k=1,nroot
            transp(k)=vegfrac*ras*qkms*                       &
                    (qvatm-qsg)*                              &
                    tranf(k)*drycan/zshalf(nroot+1)
               if(transp(k).gt.0.) transp(k)=0.
            ett1=ett1-transp(k)
          enddo
          do k=nroot+1,nzs
            transp(k)=0.
          enddo
        endif

!-- recalculate volumetric content of frozen water in soil
         do k=1,nzs
!- main levels
           tln=log(tso(k)/273.15)
         if(tln.lt.0.) then
           soiliqw(k)=(dqm+qmin)*(xlmelt*                     &
          (tso(k)-273.15)/tso(k)/9.81/psis)                   & 
           **(-1./bclh)-qmin
           soiliqw(k)=max(0.,soiliqw(k))
           soiliqw(k)=min(soiliqw(k),soilmois(k))
           soilice(k)=(soilmois(k)-soiliqw(k))/riw
!---- melting and freezing is balanced, soil ice cannot increase
       if(keepfr(k).eq.1.) then
           soilice(k)=min(soilice(k),smfrkeep(k))
           soiliqw(k)=max(0.,soilmois(k)-soilice(k)*riw)
       endif

         else
           soilice(k)=0.
           soiliqw(k)=soilmois(k)
         endif
         enddo

!*************************************************************************
! soilmoist solves moisture budget (Smirnova et al., 1996, eq.22,28) 
!           and richards eqn.
!*************************************************************************
          call soilmoist (                                     &
!-- input
               delt,nzs,nddzs,dtdzs,dtdzs2,riw,                &
               zsmain,zshalf,diffu,hydro,                      &
               qsg,qvg,qcg,qcatm,qvatm,-infwater,              &
               qkms,transp,drip,dew,0.,soilice,vegfrac,        &
               0.,soilres,                                     &
!-- soil properties
               dqm,qmin,ref,ksat,ras,infmax,                   &
!-- output
               soilmois,soiliqw,mavail,runoff1,                &
               runoff2,infiltrp)
        
!--- keepfr is 1 when the temperature and moisture in soil
!--- are both increasing. in this case soil ice should not
!--- be increasing according to the freezing curve.
!--- some part of ice is melted, but additional water is
!--- getting frozen. thus, only structure of frozen soil is
!--- changed, and phase changes are not affecting the heat
!--- transfer. this situation may happen when it rains on the
!--- frozen soil.
 
        do k=1,nzs
       if (soilice(k).gt.0.) then
          if(tso(k).gt.told(k).and.soilmois(k).gt.smold(k)) then
              keepfr(k)=1.
          else
              keepfr(k)=0.
          endif
       endif
        enddo

!--- the diagnostics of surface fluxes 

          t3      = stbolt*soiltold*soiltold*soiltold
          upflux  = t3 * 0.5*(soiltold+soilt)
          xinet   = emiss*(glw-upflux)
          hft=-tkms*cp*rho*(tabs-soilt)
          hfx=-tkms*cp*rho*(tabs-soilt)                        &
               *(p1000mb*0.00001/patm)**rovcp
          q1=-qkms*ras*(qvatm - qsg)

          cmc2ms = 0.
        if (q1.le.0.) then
! ---  condensation
          ec1=0.
          edir1=0.
          ett1=0.
     if(myj) then
!-- moisture flux for coupling with myj pbl
          eeta=-qkms*ras*(qvatm/(1.+qvatm) - qsg/(1.+qsg))*1.e3
          cst= cst-eeta*delt*vegfrac
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
        print *,'cond myj eeta',eeta,eeta*xlv, i,j
    endif
     else ! myj
!-- actual moisture flux from ruc lsm
          eeta= - rho*dew
          cst=cst+delt*dew*ras * vegfrac
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'cond ruc lsm eeta',eeta,eeta*xlv, i,j
    endif
     endif ! myj
          qfx= xlv*eeta
          eeta= - rho*dew
        else
! ---  evaporation
          edir1 =-soilres*(1.-vegfrac)*qkms*ras*                      &
                  (qvatm-qvg)
          cmc2ms=cst/delt*ras
          ec1 = q1 * wetcan * vegfrac
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'cst before update=',cst
       print *,'ec1=',ec1,'cmc2ms=',cmc2ms
     endif
!    endif

          cst=max(0.,cst-ec1 * delt)

     if (myj) then
!-- moisture flux for coupling with myj pbl
          eeta=-soilres*qkms*ras*(qvatm/(1.+qvatm) - qvg/(1.+qvg))*1.e3
     else ! myj
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'qkms,ras,qvatm/(1.+qvatm),qvg/(1.+qvg),qsg ', &
                qkms,ras,qvatm/(1.+qvatm),qvg/(1.+qvg),qsg
       print *,'q1*(1.-vegfrac),edir1',q1*(1.-vegfrac),edir1
       print *,'cst,wetcan,drycan',cst,wetcan,drycan
       print *,'ec1=',ec1,'ett1=',ett1,'cmc2ms=',cmc2ms,'cmc2ms*ras=',cmc2ms*ras
    endif
!-- actual moisture flux from ruc lsm
          eeta = (edir1 + ec1 + ett1)*1.e3
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
        print *,'ruc lsm eeta',eeta,eeta*xlv
    endif
     endif ! myj
          qfx= xlv * eeta
          eeta = (edir1 + ec1 + ett1)*1.e3
        endif
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     print *,'potential temp hft ',hft
     print *,'abs temp hfx ',hfx
    endif

          evapl=eeta
          s=thdif(1)*cap(1)*dzstop*(tso(1)-tso(2))
! energy budget
          fltot=rnet-hft-xlv*eeta-s-x
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'soil - fltot,rnet,hft,qfx,s,x=',i,j,fltot,rnet,hft,xlv*eeta,s,x
       print *,'edir1,ec1,ett1,mavail,qkms,qvatm,qvg,qsg,vegfrac',&
                edir1,ec1,ett1,mavail,qkms,qvatm,qvg,qsg,vegfrac
    endif
    if(detal(1) .ne. 0.) then
! smf - energy of phase change in the first soil layer
!        smf=xlmelt*1.e3*(soiliqwm(1)-soiliqwmold(1))/delt
         smf=fltot
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     print *,'detal(1),xlmelt,soiliqwm(1),delt',detal(1),xlmelt,soiliqwm(1),delt
     print *,'implicit phase change in the first layer - smf=',smf
    endif
    endif


 222    continue

 1123    format(i5,8f12.3)
 1133    format(i7,8e12.4)
  123   format(i6,f6.2,7f8.1)
  122   format(1x,2i3,6f8.1,f8.3,f8.2)
!-------------------------------------------------------------------
   end subroutine soil
!-------------------------------------------------------------------

        subroutine sice (                                       &
!--- input variables
            i,j,iland,isoil,delt,ktau,conflx,nzs,nddzs,nroot,   &
            prcpms,rainf,patm,qvatm,qcatm,glw,gsw,              &
            emiss,rnet,qkms,tkms,rho,myj,                       &
!--- sea ice parameters
            tice,rhosice,capice,thdifice,                       &
            zsmain,zshalf,dtdzs,dtdzs2,tbq,                     &
!--- constants
            xlv,cp,rovcp,cw,stbolt,tabs,                        &
!--- output variables
            tso,dew,soilt,qvg,qsg,qcg,                          &
            eeta,qfx,hfx,s,evapl,prcpl,fltot                          &
                                                                )

!*****************************************************************
!   energy budget and  heat diffusion eqns. for
!   sea ice
!*************************************************************

        implicit none
!-----------------------------------------------------------------

!--- input variables

   integer,  intent(in   )   ::  nroot,ktau,nzs                , &
                                 nddzs                    !nddzs=2*(nzs-2)
   integer,  intent(in   )   ::  i,j,iland,isoil
   real,     intent(in   )   ::  delt,conflx
   logical,  intent(in   )   ::  myj
!--- 3-d atmospheric variables
   real,                                                         &
            intent(in   )    ::                            patm, &
                                                          qvatm, &
                                                          qcatm
!--- 2-d variables
   real,                                                         &
            intent(in   )    ::                             glw, &
                                                            gsw, &
                                                          emiss, &
                                                            rho, &
                                                           qkms, &
                                                           tkms
!--- sea ice properties
   real,    dimension(1:nzs)                                   , &
            intent(in   )    ::                                  &
                                                           tice, &
                                                        rhosice, &
                                                         capice, &
                                                       thdifice


   real,     intent(in   )   ::                                  &
                                                             cw, &
                                                            xlv


   real,     dimension(1:nzs), intent(in)  ::            zsmain, &
                                                         zshalf, &
                                                         dtdzs2

   real,     dimension(1:nddzs), intent(in)  ::           dtdzs

   real,     dimension(1:5001), intent(in)  ::              tbq


!--- input/output variables
!----soil temperature
   real,     dimension( 1:nzs ),  intent(inout)   ::        tso
!-------- 2-d variables
   real,                                                         &
             intent(inout)   ::                             dew, &
                                                           eeta, &
                                                          evapl, &
                                                          prcpl, &
                                                            qvg, &
                                                            qsg, &
                                                            qcg, &
                                                           rnet, &
                                                            qfx, &
                                                            hfx, &
                                                              s, &
                                                          soilt

!--- local variables
   real    ::  x,x1,x2,x4,tn,denom
   real    ::  rainf,  prcpms                                  , &
               tabs, t3, upflux, xinet

   real    ::  cp,rovcp,g0,lv,stbolt,xlmelt,dzstop             , &
               epot,fltot,ft,fq,hft,ras,cvw                    

   real    ::  fkt,d1,d2,d9,d10,did,r211,r21,r22,r6,r7,d11     , &
               pi,h,fkq,r210,aa,bb,pp,q1,qs1,ts1,tq2,tx2       , &
               tdenom,qgold,snoh

   real    ::  aa1,rhcs, icemelt


   real,     dimension(1:nzs)  ::   cotso,rhtso

   integer ::  nzs1,nzs2,k,k1,kn,kk

!-----------------------------------------------------------------

!-- define constants
        xlmelt=3.35e+5
        cvw=cw

        prcpl=prcpms

          nzs1=nzs-1
          nzs2=nzs-2
        dzstop=1./(zsmain(2)-zsmain(1))
        ras=rho*1.e-3

        do k=1,nzs
           cotso(k)=0.
           rhtso(k)=0.
        enddo

        cotso(1)=0.
        rhtso(1)=tso(nzs)

        do 33 k=1,nzs2
          kn=nzs-k
          k1=2*kn-3
          x1=dtdzs(k1)*thdifice(kn-1)
          x2=dtdzs(k1+1)*thdifice(kn)
          ft=tso(kn)+x1*(tso(kn-1)-tso(kn))                             &
             -x2*(tso(kn)-tso(kn+1))
          denom=1.+x1+x2-x2*cotso(k)
          cotso(k+1)=x1/denom
          rhtso(k+1)=(ft+x2*rhtso(k))/denom
   33  continue

!************************************************************************
!--- the heat balance equation (Smirnova et al., 1996, eq. 21,26)
        rhcs=capice(1)
        h=1.
        fkt=tkms
        d1=cotso(nzs1)
        d2=rhtso(nzs1)
        tn=soilt
        d9=thdifice(1)*rhcs*dzstop
        d10=tkms*cp*rho
        r211=.5*conflx/delt
        r21=r211*cp*rho
        r22=.5/(thdifice(1)*delt*dzstop**2)
        r6=emiss *stbolt*.5*tn**4
        r7=r6/tn
        d11=rnet+r6
        tdenom=d9*(1.-d1+r22)+d10+r21+r7                              &
              +rainf*cvw*prcpms
        fkq=qkms*rho
        r210=r211*rho
        aa=xls*(fkq+r210)/tdenom
        bb=(d10*tabs+r21*tn+xls*(qvatm*fkq                            &
        +r210*qvg)+d11+d9*(d2+r22*tn)                                 &
        +rainf*cvw*prcpms*max(273.15,tabs)                            &
         )/tdenom
        aa1=aa
        pp=patm*1.e3
        aa1=aa1/pp
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
        print *,' vilka-seaice1'
        print *,'d10,tabs,r21,tn,qvatm,fkq',                          &
                 d10,tabs,r21,tn,qvatm,fkq
        print *,'rnet, emiss, stbolt, soilt',rnet, emiss, stbolt, soilt
        print *,'r210,qvg,d11,d9,d2,r22,rainf,cvw,prcpms,tdenom',     &
                 r210,qvg,d11,d9,d2,r22,rainf,cvw,prcpms,tdenom
        print *,'tn,aa1,bb,pp,fkq,r210',                              &
                 tn,aa1,bb,pp,fkq,r210
    endif
        qgold=qsg
        call vilka(tn,aa1,bb,pp,qs1,ts1,tbq,ktau,i,j,iland,isoil)
!--- it is saturation over sea ice
        qvg=qs1
        qsg=qs1
        tso(1)=min(271.4,ts1)
        qcg=0.
!--- sea ice melting is not included in this simple approach
!--- soilt - skin temperature
          soilt=tso(1)
!---- final solution for soil temperature - tso
          do k=2,nzs
            kk=nzs-k+1
            tso(k)=min(271.4,rhtso(kk)+cotso(kk)*tso(k-1))
          end do
!--- calculation of dew using new value of qsg or transp if no dew
        dew=0.

!--- the diagnostics of surface fluxes 
          t3      = stbolt*tn*tn*tn
          upflux  = t3 *0.5*(tn+soilt)
          xinet   = emiss*(glw-upflux)
          hft=-tkms*cp*rho*(tabs-soilt)
          hfx=-tkms*cp*rho*(tabs-soilt)                        &
               *(p1000mb*0.00001/patm)**rovcp
          q1=-qkms*ras*(qvatm - qsg)
        if (q1.le.0.) then
! ---  condensation
     if(myj) then
!-- moisture flux for coupling with myj pbl
          eeta=-qkms*ras*(qvatm/(1.+qvatm) - qsg/(1.+qsg))*1.e3
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'myj eeta',eeta
    endif
     else ! myj
!-- actual moisture flux from ruc lsm
          dew=qkms*(qvatm-qsg)
          eeta= - rho*dew
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'ruc lsm eeta',eeta
    endif
     endif ! myj
          qfx= xls*eeta
          eeta= - rho*dew
        else
! ---  evaporation
     if(myj) then
!-- moisture flux for coupling with myj pbl
          eeta=-qkms*ras*(qvatm/(1.+qvatm) - qvg/(1.+qvg))*1.e3
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'myj eeta',eeta
    endif
     else ! myj
! to convert from m s-1 to kg m-2 s-1: *rho water=1.e3************
!-- actual moisture flux from ruc lsm
          eeta = q1*1.e3
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'ruc lsm eeta',eeta
    endif
     endif ! myj
          qfx= xls * eeta
          eeta = q1*1.e3
        endif
          evapl=eeta

          s=thdifice(1)*capice(1)*dzstop*(tso(1)-tso(2))
! heat storage in surface layer
        snoh=0.
! there is ice melt
         x= (cp*rho*r211+rhcs*zsmain(2)*0.5/delt)*(soilt-tn) +   &
            xls*rho*r211*(qsg-qgold)
         x=x &
! "heat" from rain
        -rainf*cvw*prcpms*(max(273.15,tabs)-soilt)

!-- excess energy spent on sea ice melt
        icemelt=rnet-xls*eeta -hft -s -x
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
        print *,'icemelt=',icemelt
    endif

          fltot=rnet-xls*eeta-hft-s-x-icemelt
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'sice - fltot,rnet,hft,qfx,s,snoh,x=', &
                       fltot,rnet,hft,xls*eeta,s,icemelt,x
    endif

!-------------------------------------------------------------------
   end subroutine sice
!-------------------------------------------------------------------



        subroutine snowsoil (spp_lsm,rstochcol,fieldcol_sf,&
!--- input variables
             i,j,isoil,delt,ktau,conflx,nzs,nddzs,nroot,       &
             meltfactor,rhonewsn,snhei_crit,                   & ! new
             iland,prcpms,rainf,newsnow,snhei,snwe,snowfrac,   &
             rhosn,                                            &
             patm,qvatm,qcatm,                                 &
             glw,gsw,gswin,emiss,rnet,ivgtyp,                  &
             qkms,tkms,pc,cst,drip,infwater,                   &
             rho,vegfrac,alb,znt,lai,                          & 
             myj,                                              &
!--- soil fixed fields
             qwrtz,rhocs,dqm,qmin,ref,wilt,psis,bclh,ksat,     &
             sat,cn,zsmain,zshalf,dtdzs,dtdzs2,tbq,            &
!--- constants
             xlv,cp,rovcp,g0_p,cw,stbolt,tabs,                 &
             kqwrtz,kice,kwt,                                  &
!--- output variables
             ilnb,snweprint,snheiprint,rsm,                    &
             soilmois,tso,smfrkeep,keepfr,                     &
             dew,soilt,soilt1,tsnav,                           &
             qvg,qsg,qcg,smelt,snoh,snflx,snom,                &
             edir1,ec1,ett1,eeta,qfx,hfx,s,sublim,             &
             prcpl,fltot,runoff1,runoff2,mavail,soilice,             &
             soiliqw,infiltrp                                  )

!***************************************************************
!   energy and moisture budget for snow, heat diffusion eqns.
!   in snow and soil, richards eqn. for soil covered with snow
!
!     delt - time step (s)
!     ktau - numver of time step
!     conflx - depth of constant flux layer (m)
!     j,i - the location of grid point
!     ime, jme,  nzs - dimensions of the domain
!     nroot - number of levels within the root zone
!     prcpms - precipitation rate in m/s
!     newsnow - pcpn in soilid form (m)
!     snhei, snwe - snow height and snow water equivalent (m)
!     rhosn - snow density (kg/m-3)
!     patm - pressure (bar)
!     qvatm,qcatm - cloud and water vapor mixing ratio
!                   at the first atm. level (kg/kg)
!     glw, gsw - incoming longwave and absorbed shortwave
!                radiation at the surface (w/m^2)
!     emiss,rnet - emissivity (0-1) of the ground surface and net
!                  radiation at the surface (w/m^2)
!     qkms - exchange coefficient for water vapor in the
!              surface layer (m/s)
!     tkms - exchange coefficient for heat in the surface
!              layer (m/s)
!     pc - plant coefficient (resistance) (0-1)
!     rho - density of atmosphere near surface (kg/m^3)
!     vegfrac - greeness fraction (0-1)
!     rhocs - volumetric heat capacity of dry soil (j/m^3/k)
!     dqm, qmin - porosity minus residual soil moisture qmin (m^3/m^3)
!     ref, wilt - field capacity soil moisture and the
!                 wilting point (m^3/m^3)
!     psis - matrix potential at saturation (m)
!     bclh - exponent for clapp-hornberger parameterization
!     ksat - saturated hydraulic conductivity (m/s)
!     sat - maximum value of water intercepted by canopy (m)
!     cn - exponent for calculation of canopy water
!     zsmain - main levels in soil (m)
!     zshalf - middle of the soil layers (m)
!     dtdzs,dtdzs2 - dt/(2.*dzshalf*dzmain) and dt/dzshalf in soil
!     tbq - table to define saturated mixing ration
!           of water vapor for given temperature and pressure
!     ilnb - number of layers in snow
!     rsm - liquid water inside snow pack (m)
!     soilmois,tso - soil moisture (m^3/m^3) and temperature (k)
!     dew -  dew in (kg/m^2 s)
!     soilt - skin temperature (k)
!     soilt1 - snow temperature at 7.5 cm depth (k)
!     tsnav - average temperature of snow pack (c)
!     qsg,qvg,qcg - saturated mixing ratio, mixing ratio of
!                   water vapor and cloud at the ground
!                   surface, respectively (kg/kg)
!     edir1, ec1, ett1, eeta - direct evaporation, evaporation of
!            canopy water, transpiration (kg m-2 s-1) and total
!            evaporation in (m s-1).
!     qfx, hfx - latent and sensible heat fluxes (w/m^2)
!     s - soil heat flux in the top layer (w/m^2)
!     sublim - snow sublimation (kg/m^2/s)
!     runoff1 - surface runoff (m/s)
!     runoff2 - underground runoff (m)
!     mavail - moisture availability in the top soil layer (0-1)
!     soilice - content of soil ice in soil layers (m^3/m^3)
!     soiliqw - lliquid water in soil layers (m^3/m^3)
!     infiltrp - infiltration flux from the top of soil domain (m/s)
!     xinet - net long-wave radiation (w/m^2)
!
!*******************************************************************

        implicit none
!-------------------------------------------------------------------
!--- input variables

   integer,  intent(in   )   ::  nroot,ktau,nzs     ,            &
                                 nddzs                         !nddzs=2*(nzs-2)
   integer,  intent(in   )   ::  i,j,isoil

   real,     intent(in   )   ::  delt,conflx,prcpms            , &
                                 rainf,newsnow,rhonewsn,         &
                                 snhei_crit,meltfactor

   logical,    intent(in   )    ::     myj

!--- 3-d atmospheric variables
   real,                                                         &
            intent(in   )    ::                            patm, &
                                                          qvatm, &
                                                          qcatm
!--- 2-d variables
   real                                                        , &
            intent(in   )    ::                             glw, &
                                                            gsw, &
                                                          gswin, &
                                                            rho, &
                                                             pc, &
                                                        vegfrac, &
                                                            lai, &
                                                       infwater, &
                                                           qkms, &
                                                           tkms

   integer,  intent(in   )   ::                          ivgtyp
!--- soil properties
   real                                                        , &
            intent(in   )    ::                           rhocs, &
                                                           bclh, &
                                                            dqm, &
                                                           ksat, &
                                                           psis, &
                                                           qmin, &
                                                          qwrtz, &
                                                            ref, &
                                                            sat, &
                                                           wilt

   real,     intent(in   )   ::                              cn, &
                                                             cw, &
                                                            xlv, &
                                                           g0_p, & 
                                                         kqwrtz, &
                                                           kice, &
                                                            kwt 


   real,     dimension(1:nzs), intent(in)  ::            zsmain, &
                                                         zshalf, &
                                                         dtdzs2

   real,     dimension(1:nddzs), intent(in)  ::           dtdzs

   real,     dimension(1:5001), intent(in)  ::              tbq

   real,     dimension(1:nzs), intent(in)  ::          rstochcol
   real,     dimension(1:nzs), intent(inout) ::     fieldcol_sf

!--- input/output variables
!-------- 3-d soil moisture and temperature
   real,     dimension(  1:nzs )                               , &
             intent(inout)   ::                             tso, &
                                                       soilmois, &
                                                       smfrkeep

   real,  dimension( 1:nzs )                                   , &
             intent(inout)   ::                          keepfr


   integer,  intent(inout)    ::                           iland


!-------- 2-d variables
   real                                                        , &
             intent(inout)   ::                             dew, &
                                                            cst, &
                                                           drip, &
                                                          edir1, &
                                                            ec1, &
                                                           ett1, &
                                                           eeta, &
                                                          rhosn, &
                                                         sublim, &
                                                          prcpl, &
                                                            alb, &
                                                          emiss, &
                                                            znt, &
                                                         mavail, &
                                                            qvg, &
                                                            qsg, &
                                                            qcg, &
                                                            qfx, &
                                                            hfx, &
                                                              s, &
                                                        runoff1, &
                                                        runoff2, &
                                                           snwe, &
                                                          snhei, &
                                                          smelt, &
                                                           snom, &
                                                           snoh, &
                                                          snflx, &
                                                          soilt, &
                                                         soilt1, &
                                                       snowfrac, &
                                                          tsnav

   integer, intent(inout)    ::                            ilnb

!-------- 1-d variables
   real,     dimension(1:nzs), intent(out)  ::          soilice, &
                                                        soiliqw

   real,     intent(out)                    ::              rsm, &
                                                      snweprint, &
                                                     snheiprint
   integer,  intent(in)                    ::       spp_lsm 
!--- local variables


   integer ::  nzs1,nzs2,k

   real    ::  infiltrp, transum                               , &
               snth, newsn                                     , &
               tabs, t3, upflux, xinet                         , &
               beta, snwepr,epdt,pp
   real    ::  cp,rovcp,g0,lv,xlvm,stbolt,xlmelt,dzstop        , &
               can,epot,fac,fltot,ft,fq,hft                    , &
               q1,ras,rhoice,sph                               , &
               trans,zn,ci,cvw,tln,tavln,pi                    , &
               dd1,cmc2ms,drycan,wetcan                        , &
               infmax,riw,deltsn,h,umveg

   real,     dimension(1:nzs)  ::  transp,cap,diffu,hydro      , &
                                   thdif,tranf,tav,soilmoism   , &
                                   soilicem,soiliqwm,detal     , &
                                   fwsat,lwsat,told,smold
   real                        ::  soiltold, qgold

   real                        ::  rnet, x

!-----------------------------------------------------------------

        cvw=cw
        xlmelt=3.35e+5
!-- heat of water vapor sublimation
        xlvm=xlv+xlmelt

!--- snow flag -- isice
!         iland=isice

!--- deltsn - is the threshold for splitting the snow layer into 2 layers.
!--- with snow density 400 kg/m^3, this threshold is equal to 7.5 cm,
!--- equivalent to 0.03 m snwe. for other snow densities the threshold is
!--- computed using snwe=0.03 m and current snow density.
!--- snth - the threshold below which the snow layer is combined with
!--- the top soil layer. snth is computed using snwe=0.016 m, and
!--- equals 4 cm for snow density 400 kg/m^3.

!save soilt and qvg
       soiltold=soilt
       qgold=qvg

       x=0.

           deltsn=0.05*1.e3/rhosn
           snth=0.01*1.e3/rhosn
!      print *,'deltsn,snhei,snth',i,j,deltsn,snhei,snth

! for 2-layer snow model when the snow depth is marginally higher than deltsn,
! reset deltsn to half of snow depth.
        if(snhei.ge.deltsn+snth) then
          if(snhei-deltsn-snth.lt.snth) deltsn=0.5*(snhei-snth)
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'deltsn is changed,deltsn,snhei,snth',i,j,deltsn,snhei,snth
    endif
        endif 

        rhoice=900.
        ci=rhoice*2100.
        ras=rho*1.e-3
        riw=rhoice*1.e-3
        rsm=0.

        do k=1,nzs
          transp     (k)=0.
          soilmoism  (k)=0.
          soiliqwm   (k)=0.
          soilice    (k)=0.
          soilicem   (k)=0.
          lwsat      (k)=0.
          fwsat      (k)=0.
          tav        (k)=0.
          cap        (k)=0.
          diffu      (k)=0.
          hydro      (k)=0.
          thdif      (k)=0.  
          tranf      (k)=0.
          detal      (k)=0.
          told       (k)=0.
          smold      (k)=0. 
        enddo

        snweprint=0.
        snheiprint=0.
        prcpl=prcpms

!*** deltsn is the depth of the top layer of snow where
!*** there is a temperature gradient, the rest of the snow layer
!*** is considered to have constant temperature


          nzs1=nzs-1
          nzs2=nzs-2
        dzstop=1./(zsmain(2)-zsmain(1))

!----- the calculation of thermal diffusivity, diffusional and ---
!----- hydraulic conductivity (Smirnova et al. 1996, eq.2,5,6) ---
!tgs - the following loop is added to define the amount of frozen
!tgs - water in soil if there is any
         do k=1,nzs

         tln=log(tso(k)/273.15)
         if(tln.lt.0.) then
           soiliqw(k)=(dqm+qmin)*(xlmelt*                          &
         (tso(k)-273.15)/tso(k)/9.81/psis)                         &
          **(-1./bclh)-qmin
           soiliqw(k)=max(0.,soiliqw(k))
           soiliqw(k)=min(soiliqw(k),soilmois(k))
           soilice(k)=(soilmois(k)-soiliqw(k))/riw

!---- melting and freezing is balanced, soil ice cannot increase
       if(keepfr(k).eq.1.) then
           soilice(k)=min(soilice(k),smfrkeep(k))
           soiliqw(k)=max(0.,soilmois(k)-soilice(k)*rhoice*1.e-3)
       endif

         else
           soilice(k)=0.
           soiliqw(k)=soilmois(k)
         endif

          enddo

          do k=1,nzs1

         tav(k)=0.5*(tso(k)+tso(k+1))
         soilmoism(k)=0.5*(soilmois(k)+soilmois(k+1))
         tavln=log(tav(k)/273.15)

         if(tavln.lt.0.) then
           soiliqwm(k)=(dqm+qmin)*(xlmelt*                         &
         (tav(k)-273.15)/tav(k)/9.81/psis)                         &
          **(-1./bclh)-qmin
           fwsat(k)=dqm-soiliqwm(k)
           lwsat(k)=soiliqwm(k)+qmin
           soiliqwm(k)=max(0.,soiliqwm(k))
           soiliqwm(k)=min(soiliqwm(k), soilmoism(k))
           soilicem(k)=(soilmoism(k)-soiliqwm(k))/riw
!---- melting and freezing is balanced, soil ice cannot increase
       if(keepfr(k).eq.1.) then
           soilicem(k)=min(soilicem(k),                            &
                    0.5*(smfrkeep(k)+smfrkeep(k+1)))
           soiliqwm(k)=max(0.,soilmoism(k)-soilicem(k)*riw)
           fwsat(k)=dqm-soiliqwm(k)
           lwsat(k)=soiliqwm(k)+qmin
       endif

         else
           soilicem(k)=0.
           soiliqwm(k)=soilmoism(k)
           lwsat(k)=dqm+qmin
           fwsat(k)=0.

         endif
          enddo

          do k=1,nzs
           if(soilice(k).gt.0.) then
             smfrkeep(k)=soilice(k)
           else
             smfrkeep(k)=soilmois(k)/riw
           endif
          enddo
!******************************************************************
! soilprop computes thermal diffusivity, and diffusional and
!          hydraulic condeuctivities
!******************************************************************
          call soilprop(spp_lsm,rstochcol,fieldcol_sf,      &
!--- input variables
               nzs,fwsat,lwsat,tav,keepfr,                       &
               soilmois,soiliqw,soilice,                         &
               soilmoism,soiliqwm,soilicem,                      &
!--- soil fixed fields
               qwrtz,rhocs,dqm,qmin,psis,bclh,ksat,              & 
!--- constants
               riw,xlmelt,cp,g0_p,cvw,ci,                        &
               kqwrtz,kice,kwt,                                  &
!--- output variables
               thdif,diffu,hydro,cap)

!******************************************************************** 
!--- calculation of canopy water (Smirnova et al., 1996, eq.16) and dew 
 
        smelt=0.
        h=mavail

        fq=qkms


!--- if vegfrac.ne.0. then part of falling snow can be
!--- intercepted by the canopy. 

        dew=0.
        umveg=1.-vegfrac
        epot = -fq*(qvatm-qsg) 

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'snwe after subtracting intercepted snow - snwe=',snwe,vegfrac,cst
    endif
          snwepr=snwe

!  check if all snow can evaporate during dt
         beta=1.
         epdt = epot * ras *delt*umveg
         if(epdt.gt.0. .and. snwepr.le.epdt) then 
            beta=snwepr/max(1.e-8,epdt)
            snwe=0.
         endif

          wetcan=min(0.25,max(0.,(cst/sat))**cn)
!          if(lai > 1.) wetcan=wetcan/lai
          drycan=1.-wetcan

!**************************************************************
!  transf computes transpiration function
!**************************************************************
           call transf(i,j,                                   &
!--- input variables
              nzs,nroot,soiliqw,tabs,lai,gswin,               &
!--- soil fixed fields
              dqm,qmin,ref,wilt,zshalf,pc,iland,              & 
!--- output variables
              tranf,transum)

!--- save soil temp and moisture from the beginning of time step
          do k=1,nzs
           told(k)=tso(k)
           smold(k)=soilmois(k)
          enddo

!**************************************************************
! snowtemp solves heat budget and diffusion eqn. in soil
!**************************************************************

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
print *, 'tso before calling snowtemp: ', tso
    endif
        call snowtemp(                                        &
!--- input variables
             i,j,iland,isoil,                                 &
             delt,ktau,conflx,nzs,nddzs,nroot,                &
             snwe,snwepr,snhei,newsnow,snowfrac,              &
             beta,deltsn,snth,rhosn,rhonewsn,meltfactor,      &  ! add meltfactor
             prcpms,rainf,                                    &
             patm,tabs,qvatm,qcatm,                           &
             glw,gsw,emiss,rnet,                              &
             qkms,tkms,pc,rho,vegfrac,                        &
             thdif,cap,drycan,wetcan,cst,                     &
             tranf,transum,dew,mavail,                        &
!--- soil fixed fields
             dqm,qmin,psis,bclh,                              &
             zsmain,zshalf,dtdzs,tbq,                         &
!--- constants
             xlvm,cp,rovcp,g0_p,cvw,stbolt,                   &
!--- output variables
             snweprint,snheiprint,rsm,                        &
             tso,soilt,soilt1,tsnav,qvg,qsg,qcg,              &
             smelt,snoh,snflx,s,ilnb,x)

!************************************************************************
!--- recalculation of dew using new value of qsg or transp if no dew
         dew=0.
         ett1=0.
         pp=patm*1.e3
         epot = -fq*(qvatm-qsg)
       if(epot.gt.0.) then
! evaporation
          do k=1,nroot
            transp(k)=vegfrac*ras*fq*(qvatm-qsg)              &
                     *tranf(k)*drycan/zshalf(nroot+1)
            ett1=ett1-transp(k)
          enddo
          do k=nroot+1,nzs
            transp(k)=0.
          enddo

        else
! sublimation
          dew=-epot
          do k=1,nzs
            transp(k)=0.
          enddo
        ett1=0.
        endif

!-- recalculating of frozen water in soil
         do k=1,nzs
         tln=log(tso(k)/273.15)
         if(tln.lt.0.) then
           soiliqw(k)=(dqm+qmin)*(xlmelt*                    &
         (tso(k)-273.15)/tso(k)/9.81/psis)                   &
          **(-1./bclh)-qmin
           soiliqw(k)=max(0.,soiliqw(k))
           soiliqw(k)=min(soiliqw(k),soilmois(k))
           soilice(k)=(soilmois(k)-soiliqw(k))/riw
!---- melting and freezing is balanced, soil ice cannot increase
       if(keepfr(k).eq.1.) then
           soilice(k)=min(soilice(k),smfrkeep(k))
           soiliqw(k)=max(0.,soilmois(k)-soilice(k)*riw)
       endif

         else
           soilice(k)=0.
           soiliqw(k)=soilmois(k)
         endif
         enddo

!*************************************************************************
!--- tqcan for solution of moisture balance (Smirnova et al. 1996, eq.22,28)
!    and tso,eta profiles
!*************************************************************************
                call soilmoist (                                   &
!-- input
               delt,nzs,nddzs,dtdzs,dtdzs2,riw,                    &
               zsmain,zshalf,diffu,hydro,                          &
               qsg,qvg,qcg,qcatm,qvatm,-infwater,                  &
               qkms,transp,0.,                                     &
               0.,smelt,soilice,vegfrac,                           &
               snowfrac,1.,                                        &
!-- soil properties
               dqm,qmin,ref,ksat,ras,infmax,                       &
!-- output
               soilmois,soiliqw,mavail,runoff1,                    &
               runoff2,infiltrp) 
 
!        endif

!-- restore land-use parameters if all snow is melted
         if(snhei.eq.0.)  then
          tsnav=soilt-273.15
         endif

! 21apr2009
! snom [mm] goes into the passed-in acsnom variable in the grid derived type
        snom=snom+smelt*delt*1.e3
!
!--- keepfr is 1 when the temperature and moisture in soil
!--- are both increasing. in this case soil ice should not
!--- be increasing according to the freezing curve.
!--- some part of ice is melted, but additional water is
!--- getting frozen. thus, only structure of frozen soil is
!--- changed, and phase changes are not affecting the heat
!--- transfer. this situation may happen when it rains on the
!--- frozen soil.

        do k=1,nzs
       if (soilice(k).gt.0.) then
          if(tso(k).gt.told(k).and.soilmois(k).gt.smold(k)) then
              keepfr(k)=1.
          else
              keepfr(k)=0.
          endif
       endif
        enddo
!--- the diagnostics of surface fluxes

        t3      = stbolt*soiltold*soiltold*soiltold
        upflux  = t3 *0.5*(soiltold+soilt)
        xinet   = emiss*(glw-upflux)   
        hfx=-tkms*cp*rho*(tabs-soilt)                        &
               *(p1000mb*0.00001/patm)**rovcp
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'potential temp hfx',hfx
    endif
        hft=-tkms*cp*rho*(tabs-soilt) 
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'abs temp hfx',hft
    endif
        q1 = - fq*ras* (qvatm - qsg)
        cmc2ms=0.
        if (q1.lt.0.) then
! ---  condensation
        edir1=0.
        ec1=0.
        ett1=0.
! ---  condensation
     if(myj) then
!-- moisture flux for coupling with myj pbl
          eeta=-qkms*ras*(qvatm/(1.+qvatm) - qsg/(1.+qsg))*1.e3
          cst= cst-eeta*delt*vegfrac
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'myj eeta cond', eeta
    endif
     else ! myj
!-- actual moisture flux from ruc lsm
          dew=qkms*(qvatm-qsg)
          eeta= - rho*dew
          cst=cst+delt*dew*ras * vegfrac
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'ruc lsm eeta cond',eeta
    endif
     endif ! myj
          qfx= xlvm*eeta
          eeta= - rho*dew
        else
! ---  evaporation
        edir1 = q1*umveg *beta
        cmc2ms=cst/delt*ras
        ec1 = q1 * wetcan * vegfrac

        cst=max(0.,cst-ec1 * delt)

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     print*,'q1,umveg,beta',q1,umveg,beta
     print *,'wetcan,vegfrac',wetcan,vegfrac
     print *,'ec1,cmc2ms',ec1,cmc2ms
    endif

     if(myj) then
!-- moisture flux for coupling with myj pbl
        eeta=-(qkms*ras*(qvatm/(1.+qvatm) - qsg/(1.+qsg))*1.e3)*beta
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'myj eeta', eeta*xlvm,eeta
    endif
     else ! myj
! to convert from m s-1 to kg m-2 s-1: *rho water=1.e3************
!-- actual moisture flux from ruc lsm
        eeta = (edir1 + ec1 + ett1)*1.e3
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'ruc lsm eeta',eeta*xlvm,eeta
    endif
     endif ! myj
        qfx= xlvm * eeta
        eeta = (edir1 + ec1 + ett1)*1.e3
       endif
        s=snflx
        sublim=edir1*1.e3
! energy budget
        fltot=rnet-hft-xlvm*eeta-s-snoh-x
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'snowsoil - fltot,rnet,hft,qfx,s,snoh,x=',fltot,rnet,hft,xlvm*eeta,s,snoh,x
       print *,'edir1,ec1,ett1,mavail,qkms,qvatm,qvg,qsg,vegfrac,beta',&
                edir1,ec1,ett1,mavail,qkms,qvatm,qvg,qsg,vegfrac,beta
    endif

 222     continue

 1123    format(i5,8f12.3)
 1133    format(i7,8e12.4)
  123   format(i6,f6.2,7f8.1)
 122    format(1x,2i3,6f8.1,f8.3,f8.2)

!-------------------------------------------------------------------
   end subroutine snowsoil
!-------------------------------------------------------------------

           subroutine snowseaice(                               &
            i,j,isoil,delt,ktau,conflx,nzs,nddzs,               &
            meltfactor,rhonewsn,snhei_crit,                     &  ! new
            iland,prcpms,rainf,newsnow,snhei,snwe,snowfrac,     &
            rhosn,patm,qvatm,qcatm,                             &
            glw,gsw,emiss,rnet,                                 &
            qkms,tkms,rho,myj,                                  &
!--- sea ice parameters
            alb,znt,                                            &
            tice,rhosice,capice,thdifice,                       &
            zsmain,zshalf,dtdzs,dtdzs2,tbq,                     &
!--- constants
            xlv,cp,rovcp,cw,stbolt,tabs,                        &
!--- output variables
            ilnb,snweprint,snheiprint,rsm,tso,                  &
            dew,soilt,soilt1,tsnav,qvg,qsg,qcg,                 &
            smelt,snoh,snflx,snom,eeta,                         &
            qfx,hfx,s,sublim,prcpl,fltot                        &
                                                                )
!***************************************************************
!   solving energy budget for snow on sea ice and heat diffusion 
!   eqns. in snow and sea ice
!***************************************************************


        implicit none
!-------------------------------------------------------------------
!--- input variables

   integer,  intent(in   )   ::  ktau,nzs     ,                  &
                                 nddzs                         !nddzs=2*(nzs-2)
   integer,  intent(in   )   ::  i,j,isoil

   real,     intent(in   )   ::  delt,conflx,prcpms            , &
                                 rainf,newsnow,rhonewsn,         &
                                 meltfactor, snhei_crit
   real                      ::  rhonewcsn

   logical,  intent(in   )   ::  myj
!--- 3-d atmospheric variables
   real,                                                         &
            intent(in   )    ::                            patm, &
                                                          qvatm, &
                                                          qcatm
!--- 2-d variables
   real                                                        , &
            intent(in   )    ::                             glw, &
                                                            gsw, &
                                                            rho, &
                                                           qkms, &
                                                           tkms

!--- sea ice properties
   real,     dimension(1:nzs)                                  , &
            intent(in   )    ::                                  &
                                                           tice, &
                                                        rhosice, &
                                                         capice, &
                                                       thdifice

   real,     intent(in   )   ::                                  &
                                                             cw, &
                                                            xlv

   real,     dimension(1:nzs), intent(in)  ::            zsmain, &
                                                         zshalf, &
                                                         dtdzs2

   real,     dimension(1:nddzs), intent(in)  ::           dtdzs

   real,     dimension(1:5001), intent(in)  ::              tbq

!--- input/output variables
!-------- 3-d soil moisture and temperature
   real,     dimension(  1:nzs )                               , &
             intent(inout)   ::                             tso

   integer,  intent(inout)    ::                           iland


!-------- 2-d variables
   real                                                        , &
             intent(inout)   ::                             dew, &
                                                           eeta, &
                                                          rhosn, &
                                                         sublim, &
                                                          prcpl, &
                                                            alb, &
                                                          emiss, &
                                                            znt, &
                                                            qvg, &
                                                            qsg, &
                                                            qcg, &
                                                            qfx, &
                                                            hfx, &
                                                              s, &
                                                           snwe, &
                                                          snhei, &
                                                          smelt, &
                                                           snom, &
                                                           snoh, &
                                                          snflx, &
                                                          soilt, &
                                                         soilt1, &
                                                       snowfrac, &
                                                          tsnav

   integer, intent(inout)    ::                            ilnb

   real,     intent(out)                    ::              rsm, &
                                                      snweprint, &
                                                     snheiprint
!--- local variables


   integer ::  nzs1,nzs2,k,k1,kn,kk
   real    ::  x,x1,x2,dzstop,ft,tn,denom

   real    ::  snth, newsn                                     , &
               tabs, t3, upflux, xinet                         , &
               beta, snwepr,epdt,pp
   real    ::  cp,rovcp,g0,lv,xlvm,stbolt,xlmelt               , &
               epot,fltot,fq,hft,q1,ras,rhoice,ci,cvw          , &
               riw,deltsn,h

   real    ::  rhocsn,thdifsn,                                   &
               xsn,ddzsn,x1sn,d1sn,d2sn,d9sn,r22sn

   real    ::  cotsn,rhtsn,xsn1,ddzsn1,x1sn1,ftsnow,denomsn
   real    ::  fso,fsn,                                          &
               fkt,d1,d2,d9,d10,did,r211,r21,r22,r6,r7,d11,      &
               fkq,r210,aa,bb,qs1,ts1,tq2,tx2,                   &
               tdenom,aa1,rhcs,h1,tsob, snprim,                  &
               snodif,soh,tnold,qgold,snohgnew
   real,     dimension(1:nzs)  ::  cotso,rhtso

   real                   :: rnet,rsmfrac,soiltfrac,hsn,icemelt,rr
   integer                ::      nmelt


!-----------------------------------------------------------------
        xlmelt=3.35e+5
!-- heat of sublimation of water vapor
        xlvm=xlv+xlmelt

!--- snow flag -- isice
!         iland=isice

!--- deltsn - is the threshold for splitting the snow layer into 2 layers.
!--- with snow density 400 kg/m^3, this threshold is equal to 7.5 cm,
!--- equivalent to 0.03 m snwe. for other snow densities the threshold is
!--- computed using snwe=0.03 m and current snow density.
!--- snth - the threshold below which the snow layer is combined with
!--- the top sea ice layer. snth is computed using snwe=0.016 m, and
!--- equals 4 cm for snow density 400 kg/m^3.

           deltsn=0.05*1.e3/rhosn
           snth=0.01*1.e3/rhosn

! for 2-layer snow model when the snow depth is marginlly higher than deltsn,
! reset deltsn to half of snow depth.
        if(snhei.ge.deltsn+snth) then
          if(snhei-deltsn-snth.lt.snth) deltsn=0.5*(snhei-snth)
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
        print *,'deltsn ice is changed,deltsn,snhei,snth', &
                                  i,j, deltsn,snhei,snth
    endif
        endif

        rhoice=900.
        ci=rhoice*2100.
        ras=rho*1.e-3
        riw=rhoice*1.e-3
        rsm=0.

        xlmelt=3.35e+5
        rhocsn=2090.* rhosn
!18apr08 - add rhonewcsn
        rhonewcsn=2090.* rhonewsn
        thdifsn = 0.265/rhocsn
        ras=rho*1.e-3

        soiltfrac=soilt

        smelt=0.
        soh=0.
        snodif=0.
        snoh=0.
        snohgnew=0.
        rsm = 0.
        rsmfrac = 0.
        fsn=1.
        fso=0.
        cvw=cw

          nzs1=nzs-1
          nzs2=nzs-2

        qgold=qsg
        tnold=soilt
        dzstop=1./(zsmain(2)-zsmain(1))

        snweprint=0.
        snheiprint=0.
        prcpl=prcpms

!*** deltsn is the depth of the top layer of snow where
!*** there is a temperature gradient, the rest of the snow layer
!*** is considered to have constant temperature


        h=1.
        smelt=0.

        fq=qkms
        snhei=snwe*1.e3/rhosn
          snwepr=snwe

!  check if all snow can evaporate during dt
         beta=1.
         epot = -fq*(qvatm-qsg)
         epdt = epot * ras *delt
         if(epdt.gt.0. .and. snwepr.le.epdt) then
            beta=snwepr/max(1.e-8,epdt)
            snwe=0.
         endif

!******************************************************************************
!       coefficients for thomas algorithm for tso
!******************************************************************************

        cotso(1)=0.
        rhtso(1)=tso(nzs)
        do 33 k=1,nzs2
          kn=nzs-k
          k1=2*kn-3
          x1=dtdzs(k1)*thdifice(kn-1)
          x2=dtdzs(k1+1)*thdifice(kn)
          ft=tso(kn)+x1*(tso(kn-1)-tso(kn))                           &
             -x2*(tso(kn)-tso(kn+1))
          denom=1.+x1+x2-x2*cotso(k)
          cotso(k+1)=x1/denom
          rhtso(k+1)=(ft+x2*rhtso(k))/denom
   33  continue
!--- the nzs element in cotso and rhtso will be for snow
!--- there will be 2 layers in snow if it is deeper than deltsn+snth
       if(snhei.ge.snth) then
        if(snhei.le.deltsn+snth) then
!-- 1-layer snow model
         ilnb=1
         snprim=max(snth,snhei)
         soilt1=tso(1)
         tsob=tso(1)
         xsn = delt/2./(zshalf(2)+0.5*snprim)
         ddzsn = xsn / snprim
         x1sn = ddzsn * thdifsn
         x2 = dtdzs(1)*thdifice(1)
         ft = tso(1)+x1sn*(soilt-tso(1))                              &
              -x2*(tso(1)-tso(2))
         denom = 1. + x1sn + x2 -x2*cotso(nzs1)
         cotso(nzs)=x1sn/denom
         rhtso(nzs)=(ft+x2*rhtso(nzs1))/denom
         cotsn=cotso(nzs)
         rhtsn=rhtso(nzs)
!*** average temperature of snow pack (c)
         tsnav=0.5*(soilt+tso(1))                                     &
                     -273.15

        else
!-- 2 layers in snow, soilt1 is temperasture at deltsn depth
         ilnb=2
         snprim=deltsn
         tsob=soilt1
         xsn = delt/2./(0.5*snhei)
         xsn1= delt/2./(zshalf(2)+0.5*(snhei-deltsn))
         ddzsn = xsn / deltsn
         ddzsn1 = xsn1 / (snhei-deltsn)
         x1sn = ddzsn * thdifsn
         x1sn1 = ddzsn1 * thdifsn
         x2 = dtdzs(1)*thdifice(1)
         ft = tso(1)+x1sn1*(soilt1-tso(1))                            &
              -x2*(tso(1)-tso(2))
         denom = 1. + x1sn1 + x2 - x2*cotso(nzs1)
         cotso(nzs)=x1sn1/denom
         rhtso(nzs)=(ft+x2*rhtso(nzs1))/denom
         ftsnow = soilt1+x1sn*(soilt-soilt1)                          &
               -x1sn1*(soilt1-tso(1))
         denomsn = 1. + x1sn + x1sn1 - x1sn1*cotso(nzs)
         cotsn=x1sn/denomsn
         rhtsn=(ftsnow+x1sn1*rhtso(nzs))/denomsn
!*** average temperature of snow pack (c)
         tsnav=0.5/snhei*((soilt+soilt1)*deltsn                       &
                     +(soilt1+tso(1))*(snhei-deltsn))                 &
                     -273.15
        endif
       endif

       if(snhei.lt.snth.and.snhei.gt.0.) then
!--- snow is too thin to be treated separately, therefore it
!--- is combined with the first sea ice layer.
         snprim=snhei+zsmain(2)
         fsn=snhei/snprim
         fso=1.-fsn
         soilt1=tso(1)
         tsob=tso(2)
         xsn = delt/2./((zshalf(3)-zsmain(2))+0.5*snprim)
         ddzsn = xsn /snprim
         x1sn = ddzsn * (fsn*thdifsn+fso*thdifice(1))
         x2=dtdzs(2)*thdifice(2)
         ft=tso(2)+x1sn*(soilt-tso(2))-                              &
                       x2*(tso(2)-tso(3))
         denom = 1. + x1sn + x2 - x2*cotso(nzs-2)
         cotso(nzs1) = x1sn/denom
         rhtso(nzs1)=(ft+x2*rhtso(nzs-2))/denom
         tsnav=0.5*(soilt+tso(1))                                    &
                     -273.15
         cotso(nzs)=cotso(nzs1)
         rhtso(nzs)=rhtso(nzs1)
         cotsn=cotso(nzs)
         rhtsn=rhtso(nzs)
       endif

!************************************************************************
!--- the heat balance equation 
!18apr08 nmelt is the flag for melting, and snoh is heat of snow phase changes
       nmelt=0
       snoh=0.

        epot=-qkms*(qvatm-qsg)
        rhcs=capice(1)
        h=1.
        fkt=tkms
        d1=cotso(nzs1)
        d2=rhtso(nzs1)
        tn=soilt
        d9=thdifice(1)*rhcs*dzstop
        d10=tkms*cp*rho
        r211=.5*conflx/delt
        r21=r211*cp*rho
        r22=.5/(thdifice(1)*delt*dzstop**2)
        r6=emiss *stbolt*.5*tn**4
        r7=r6/tn
        d11=rnet+r6

      if(snhei.ge.snth) then 
        if(snhei.le.deltsn+snth) then
!--- 1-layer snow
          d1sn = cotso(nzs)
          d2sn = rhtso(nzs)
        else
!--- 2-layer snow
          d1sn = cotsn
          d2sn = rhtsn
        endif
        d9sn= thdifsn*rhocsn / snprim
        r22sn = snprim*snprim*0.5/(thdifsn*delt)
      endif

       if(snhei.lt.snth.and.snhei.gt.0.) then
!--- thin snow is combined with sea ice
         d1sn = d1
         d2sn = d2
         d9sn = (fsn*thdifsn*rhocsn+fso*thdifice(1)*rhcs)/           &
                 snprim
         r22sn = snprim*snprim*0.5                                   &
                 /((fsn*thdifsn+fso*thdifice(1))*delt)
      endif

      if(snhei.eq.0.)then
!--- all snow is sublimated
        d9sn = d9
        r22sn = r22
        d1sn = d1
        d2sn = d2
      endif


!---- tdenom for snow
        tdenom = d9sn*(1.-d1sn +r22sn)+d10+r21+r7                    &
              +rainf*cvw*prcpms                                      &
              +rhonewcsn*newsnow/delt

        fkq=qkms*rho
        r210=r211*rho
        aa=xlvm*(beta*fkq+r210)/tdenom
        bb=(d10*tabs+r21*tn+xlvm*(qvatm*                             &
        (beta*fkq)                                                   &
        +r210*qvg)+d11+d9sn*(d2sn+r22sn*tn)                          &
        +rainf*cvw*prcpms*max(273.15,tabs)                           &
        + rhonewcsn*newsnow/delt*min(273.15,tabs)                    &
         )/tdenom
        aa1=aa
        pp=patm*1.e3
        aa1=aa1/pp
!18apr08  - the iteration start point
 212    continue
        bb=bb-snoh/tdenom
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
        print *,'vilka-snow on seaice'
        print *,'tn,aa1,bb,pp,fkq,r210',                             &
                 tn,aa1,bb,pp,fkq,r210
        print *,'tabs,qvatm,tn,qvg=',tabs,qvatm,tn,qvg
    endif

        call vilka(tn,aa1,bb,pp,qs1,ts1,tbq,ktau,i,j,iland,isoil)
!--- it is saturation over snow
        qvg=qs1
        qsg=qs1
        qcg=0.

!--- soilt - skin temperature
        soilt=ts1

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
        print *,' after vilka-snow on seaice'
        print *,' ts1,qs1: ', ts1,qs1
    endif
! solution for temperature at 7.5 cm depth and snow-seaice interface
       if(snhei.ge.snth) then
        if(snhei.gt.deltsn+snth) then
!-- 2-layer snow model
          soilt1=min(273.15,rhtsn+cotsn*soilt)
          tso(1)=min(271.4,(rhtso(nzs)+cotso(nzs)*soilt1))
          tsob=soilt1
        else
!-- 1 layer in snow
          tso(1)=min(271.4,(rhtso(nzs)+cotso(nzs)*soilt))
          soilt1=tso(1)
          tsob=tso(1)
        endif
       elseif  (snhei > 0. .and. snhei < snth) then
! blended
         tso(2)=min(271.4,(rhtso(nzs1)+cotso(nzs1)*soilt))
         tso(1)=min(271.4,(tso(2)+(soilt-tso(2))*fso))
         soilt1=tso(1)
         tsob=tso(2)
       else
! snow is melted
         tso(1)=min(271.4,soilt)
         soilt1=min(271.4,soilt)
         tsob=tso(1)
       endif
!---- final solution for tso in sea ice
       if (snhei > 0. .and. snhei < snth) then
! blended or snow is melted
          do k=3,nzs
            kk=nzs-k+1
            tso(k)=min(271.4,rhtso(kk)+cotso(kk)*tso(k-1))
          end do
       else
          do k=2,nzs
            kk=nzs-k+1
            tso(k)=min(271.4,rhtso(kk)+cotso(kk)*tso(k-1))
          end do
       endif
!--- for thin snow layer combined with the top soil layer
!--- tso(i,j,1) is computed by linear interpolation between soilt
!--- and tso(i,j,2)
!       if(snhei.lt.snth.and.snhei.gt.0.)then
!          tso(1)=min(271.4,tso(2)+(soilt-tso(2))*fso)
!          soilt1=tso(1)
!          tsob = tso(2)
!       endif

      if(nmelt.eq.1) go to 220

!--- if soilt > 273.15 f then melting of snow can happen
! if all snow can evaporate, then there is nothing to melt
   if(soilt.gt.273.15.and.snwepr-beta*epot*ras*delt.gt.0..and.snhei.gt.0.) then
!
        nmelt = 1
        soiltfrac=snowfrac*273.15+(1.-snowfrac)*min(271.4,soilt)

        qsg= qsn(soiltfrac,tbq)/pp
        t3      = stbolt*tnold*tnold*tnold
        upflux  = t3 * 0.5*(tnold+soiltfrac)
        xinet   = emiss*(glw-upflux)
         epot = -qkms*(qvatm-qsg)
         q1=epot*ras

        if (q1.le.0.) then
! ---  condensation
          dew=-epot

        qfx= xlvm*rho*dew
        eeta=qfx/xlvm
       else
! ---  evaporation
        eeta = q1 * beta *1.e3
! to convert from kg m-2 s-1 to m s-1: 1/rho water=1.e-3************
        qfx= - xlvm * eeta
       endif

         hfx=d10*(tabs-soiltfrac)

       if(snhei.ge.snth)then
         soh=thdifsn*rhocsn*(soiltfrac-tsob)/snprim
         snflx=soh
       else
         soh=(fsn*thdifsn*rhocsn+fso*thdifice(1)*rhcs)*                &
              (soiltfrac-tsob)/snprim
         snflx=soh
       endif
         x= (r21+d9sn*r22sn)*(soiltfrac-tnold) +                        &
            xlvm*r210*(qsg-qgold)
!-- snoh is energy flux of snow phase change
        snoh=rnet+qfx +hfx                                              &
                  +rhonewcsn*newsnow/delt*(min(273.15,tabs)-soiltfrac)  &
                  -soh-x+rainf*cvw*prcpms*                              &
                  (max(273.15,tabs)-soiltfrac)

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     print *,'snowseaice melt i,j,snoh,rnet,qfx,hfx,soh,x',i,j,snoh,rnet,qfx,hfx,soh,x
     print *,'rhonewcsn*newsnow/delt*(min(273.15,tabs)-soiltfrac)',     &
              rhonewcsn*newsnow/delt*(min(273.15,tabs)-soiltfrac)
     print *,'rainf*cvw*prcpms*(max(273.15,tabs)-soiltfrac)',           &
              rainf*cvw*prcpms*(max(273.15,tabs)-soiltfrac)
    endif
        snoh=amax1(0.,snoh)
!-- smelt is speed of melting in m/s
        smelt= snoh /xlmelt*1.e-3
        smelt=amin1(smelt,snwepr/delt-beta*epot*ras)
        smelt=amax1(0.,smelt)

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'1-smelt i,j',smelt,i,j
    endif
!18apr08 - egglston limit
!        smelt= amin1 (smelt, 5.6e-7*meltfactor*max(1.,(soilt-273.15)))
        smelt= amin1 (smelt, 5.6e-8*meltfactor*max(1.,(soilt-273.15)))
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'2-smelt i,j',smelt,i,j
    endif

! rr - potential melting
        rr=snwepr/delt-beta*epot*ras
        smelt=min(smelt,rr)
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'3- smelt i,j,smelt,rr',i,j,smelt,rr
    endif
        snohgnew=smelt*xlmelt*1.e3
        snodif=amax1(0.,(snoh-snohgnew))

        snoh=snohgnew

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print*,'soiltfrac,soilt,snohgnew,snodif=', &
            i,j,soiltfrac,soilt,snohgnew,snodif
       print *,'snoh,snodif',snoh,snodif
    endif

!*** from koren et al. (1999) 13% of snow melt stays in the snow pack
        rsmfrac=min(0.18,(max(0.08,snwepr/0.10*0.13)))
       if(snhei > 0.01) then
        rsm=rsmfrac*smelt*delt
       else
! do not keep melted water if snow depth is less that 1 cm
        rsm=0.
       endif
!18apr08 rsm is part of melted water that stays in snow as liquid
        smelt=amax1(0.,smelt-rsm/delt)
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'4-smelt i,j,smelt,rsm,snwepr,rsmfrac', &
                    i,j,smelt,rsm,snwepr,rsmfrac
    endif

!-- update liquid equivalent of snow depth
!-- for evaporation and snow melt
        snwe = amax1(0.,(snwepr-                                      &
                    (smelt+beta*epot*ras)*delt                        &
                                         ) )
        soilt=soiltfrac
!--- if there is no snow melting then just evaporation
!--- or condensation changes snwe
      else
       if(snhei.ne.0.) then
               epot=-qkms*(qvatm-qsg)
               snwe = amax1(0.,(snwepr-                               &
                    beta*epot*ras*delt))
       endif

      endif

! no iteration for snow on sea ice, because it will produce
! skin temperature higher than it is possible with snow on sea ice
!      if(nmelt.eq.1) goto 212  ! second iteration
 220  continue

       if(smelt > 0..and.  rsm > 0.) then
        if(snwe.le.rsm) then
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     print *,'seaice snwe<rsm snwe,rsm,smelt*delt,epot*ras*delt,beta', &
                              snwe,rsm,smelt*delt,epot*ras*delt,beta
    endif
        else
!*** update snow density on effect of snow melt, melted
!*** from the top of the snow. 13% of melted water
!*** remains in the pack and changes its density.
!*** eq. 9 (with my correction) in koren et al. (1999)

         xsn=(rhosn*(snwe-rsm)+1.e3*rsm)/                            &
             snwe
         rhosn=min(max(58.8,xsn),500.)

        rhocsn=2090.* rhosn
        thdifsn = 0.265/rhocsn
        endif
      endif

        snweprint=snwe
!                                              &
!--- if vegfrac.ne.0. then some snow stays on the canopy
!--- and should be added to snwe for water conservation
! 4 nov 07                    +vegfrac*cst
        snheiprint=snweprint*1.e3 / rhosn

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
print *, 'snweprint : ',snweprint
print *, 'd9sn,soilt,tsob : ', d9sn,soilt,tsob
    endif
      if(snhei.gt.0.) then
        if(ilnb.gt.1) then
          tsnav=0.5/snhei*((soilt+soilt1)*deltsn                     &
                    +(soilt1+tso(1))*(snhei-deltsn))                 &
                       -273.15
        else
          tsnav=0.5*(soilt+tso(1)) - 273.15
        endif
      endif
!--- recalculation of dew using new value of qsg
         dew=0.
         pp=patm*1.e3
         qsg= qsn(soilt,tbq)/pp
         epot = -fq*(qvatm-qsg)
       if(epot.lt.0.) then
! sublimation
          dew=-epot
        endif

        snom=snom+smelt*delt*1.e3

!--- the diagnostics of surface fluxes

        t3      = stbolt*tnold*tnold*tnold
        upflux  = t3 *0.5*(soilt+tnold)
        xinet   = emiss*(glw-upflux)
!        rnet    = gsw + xinet
        hft=-tkms*cp*rho*(tabs-soilt)
        hfx=-tkms*cp*rho*(tabs-soilt)                        &
               *(p1000mb*0.00001/patm)**rovcp
        q1 = - fq*ras* (qvatm - qsg)
        if (q1.lt.0.) then
! ---  condensation
      if(myj) then
!-- moisture flux for coupling with myj pbl
          eeta=-qkms*ras*(qvatm/(1.+qvatm) - qsg/(1.+qsg))*1.e3
      else ! myj
!-- actual moisture flux from ruc lsm
          dew=qkms*(qvatm-qsg)
          eeta= - rho*dew
      endif ! myj
          qfx= xlvm*eeta
          eeta= - rho*dew
          sublim = eeta
        else
! ---  evaporation
      if(myj) then
!-- moisture flux for coupling with myj pbl
          eeta=-qkms*ras*beta*(qvatm/(1.+qvatm) - qvg/(1.+qvg))*1.e3
      else ! myj
! to convert from m s-1 to kg m-2 s-1: *rho water=1.e3************
!-- actual moisture flux from ruc lsm
          eeta = q1*beta*1.e3
      endif ! myj
          qfx= xlvm * eeta
          eeta = q1*beta*1.e3
          sublim = eeta
        endif

        icemelt=0.
      if(snhei.ge.snth)then
         s=thdifsn*rhocsn*(soilt-tsob)/snprim
         snflx=s
       elseif(snhei.lt.snth.and.snhei.gt.0.) then
         s=(fsn*thdifsn*rhocsn+fso*thdifice(1)*rhcs)*                &
              (soilt-tsob)/snprim
         snflx=s
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'snow is thin, snflx',i,j,snflx
    endif
       else 
         snflx=d9sn*(soilt-tsob)
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'snow is gone, snflx',i,j,snflx
    endif
       endif

        snhei=snwe *1.e3 / rhosn

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'snhei,snoh',i,j,snhei,snoh
    endif
!
         x= (r21+d9sn*r22sn)*(soilt-tnold) +              &
            xlvm*r210*(qsg-qgold)
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     print *,'snowseaice storage ',i,j,x
     print *,'r21,d9sn,r22sn,soiltfrac,tnold,qsg,qgold,snprim', &
              r21,d9sn,r22sn,soiltfrac,tnold,qsg,qgold,snprim
    endif
         x=x &
        -rhonewcsn*newsnow/delt*(min(273.15,tabs)-soilt)        &
        -rainf*cvw*prcpms*(max(273.15,tabs)-soilt)

! -- excess energy is spent on ice melt
        icemelt = rnet-hft-xlvm*eeta-s-snoh-x
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
        print *,'snowseaice icemelt=',icemelt
    endif

        fltot=rnet-hft-xlvm*eeta-s-snoh-x-icemelt
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'i,j,snhei,qsg,soilt,soilt1,tso,tabs,qvatm', &
                i,j,snhei,qsg,soilt,soilt1,tso,tabs,qvatm
       print *,'snowseaice - fltot,rnet,hft,qfx,s,snoh,icemelt,snodif,x,soilt=' &
                      ,fltot,rnet,hft,xlvm*eeta,s,snoh,icemelt,snodif,x,soilt
    endif
!-- restore sea-ice parameters if snow is less than threshold
         if(snhei.eq.0.)  then
          tsnav=soilt-273.15
          emiss=0.98
          znt=0.011
          alb=0.55
         endif

!------------------------------------------------------------------------
!------------------------------------------------------------------------
   end subroutine snowseaice
!------------------------------------------------------------------------


           subroutine soiltemp(                             &
!--- input variables
           i,j,iland,isoil,                                 &
           delt,ktau,conflx,nzs,nddzs,nroot,                &
           prcpms,rainf,patm,tabs,qvatm,qcatm,              &
           emiss,rnet,                                      &
           qkms,tkms,pc,rho,vegfrac,lai,                    &
           thdif,cap,drycan,wetcan,                         &
           transum,dew,mavail,soilres,alfa,                 &
!--- soil fixed fields
           dqm,qmin,bclh,                                   &
           zsmain,zshalf,dtdzs,tbq,                         &
!--- constants
           xlv,cp,g0_p,cvw,stbolt,                          &
!--- output variables
           tso,soilt,qvg,qsg,qcg,x)

!*************************************************************
!   energy budget equation and heat diffusion eqn are 
!   solved here and
!
!     delt - time step (s)
!     ktau - numver of time step
!     conflx - depth of constant flux layer (m)
!     ime, jme, kme, nzs - dimensions of the domain 
!     nroot - number of levels within the root zone
!     prcpms - precipitation rate in m/s
!     cotso, rhtso - coefficients for implicit solution of
!                     heat diffusion equation
!     thdif - thermal diffusivity (m^2/s)
!     qsg,qvg,qcg - saturated mixing ratio, mixing ratio of
!                   water vapor and cloud at the ground
!                   surface, respectively (kg/kg)
!     patm -  pressure [bar]
!     qc3d,qv3d - cloud and water vapor mixing ratio
!                   at the first atm. level (kg/kg)
!     emiss,rnet - emissivity (0-1) of the ground surface and net
!                  radiation at the surface (w/m^2)
!     qkms - exchange coefficient for water vapor in the
!              surface layer (m/s)
!     tkms - exchange coefficient for heat in the surface
!              layer (m/s)
!     pc - plant coefficient (resistance)
!     rho - density of atmosphere near surface (kg/m^3)
!     vegfrac - greeness fraction (0-1)
!     cap - volumetric heat capacity (j/m^3/k)
!     drycan - dry fraction of vegetated area where
!              transpiration may take place (0-1)
!     wetcan - fraction of vegetated area covered by canopy
!              water (0-1)
!     transum - transpiration function integrated over the 
!               rooting zone (m)
!     dew -  dew in kg/m^2s
!     mavail - fraction of maximum soil moisture in the top
!               layer (0-1)
!     zsmain - main levels in soil (m)
!     zshalf - middle of the soil layers (m)
!     dtdzs - dt/(2.*dzshalf*dzmain)
!     tbq - table to define saturated mixing ration
!           of water vapor for given temperature and pressure
!     tso - soil temperature (k)
!     soilt - skin temperature (k)
!
!****************************************************************

        implicit none
!-----------------------------------------------------------------

!--- input variables

   integer,  intent(in   )   ::  nroot,ktau,nzs                , &
                                 nddzs                         !nddzs=2*(nzs-2)
   integer,  intent(in   )   ::  i,j,iland,isoil
   real,     intent(in   )   ::  delt,conflx,prcpms, rainf
   real,     intent(inout)   ::  drycan,wetcan,transum
!--- 3-d atmospheric variables
   real,                                                         &
            intent(in   )    ::                            patm, &
                                                          qvatm, &
                                                          qcatm
!--- 2-d variables
   real                                                        , &
            intent(in   )    ::                                  &
                                                          emiss, &
                                                            rho, &
                                                           rnet, &  
                                                             pc, &
                                                        vegfrac, &
                                                            lai, &
                                                            dew, & 
                                                           qkms, &
                                                           tkms

!--- soil properties
   real                                                        , &
            intent(in   )    ::                                  &
                                                           bclh, &
                                                            dqm, &
                                                           qmin
   real                                                        , &
            intent(in   )    ::                                  &
                                                   soilres,alfa


   real,     intent(in   )   ::                              cp, &
                                                            cvw, &
                                                            xlv, &
                                                         stbolt, &
                                                           tabs, &
                                                           g0_p


   real,     dimension(1:nzs), intent(in)  ::            zsmain, &
                                                         zshalf, &
                                                          thdif, &
                                                            cap

   real,     dimension(1:nddzs), intent(in)  ::           dtdzs

   real,     dimension(1:5001), intent(in)  ::              tbq


!--- input/output variables
!-------- 3-d soil moisture and temperature
   real,     dimension( 1:nzs )                                , &
             intent(inout)   ::                             tso

!-------- 2-d variables
   real                                                        , &
             intent(inout)   ::                                  &
                                                         mavail, &
                                                            qvg, &
                                                            qsg, &
                                                            qcg, &
                                                          soilt


!--- local variables

   real    ::  x,x1,x2,x4,dzstop,can,ft,sph                    , &
               tn,trans,umveg,denom,fex

   real    ::  fkt,d1,d2,d9,d10,did,r211,r21,r22,r6,r7,d11     , &
               pi,h,fkq,r210,aa,bb,pp,q1,qs1,ts1,tq2,tx2       , &
               tdenom

   real    ::  c,cc,aa1,rhcs,h1, qgold

   real,     dimension(1:nzs)  ::                   cotso,rhtso

   integer ::  nzs1,nzs2,k,k1,kn,kk, iter


!-----------------------------------------------------------------

        iter=0

          nzs1=nzs-1
          nzs2=nzs-2
        dzstop=1./(zsmain(2)-zsmain(1))

        qgold=qvg

        do k=1,nzs
           cotso(k)=0.
           rhtso(k)=0.
        enddo
!******************************************************************************
!       coefficients for thomas algorithm for tso
!******************************************************************************
!         did=2.*(zsmain(nzs)-zshalf(nzs))
!         h1=dtdzs(8)*thdif(nzs-1)*(zshalf(nzs)-zshalf(nzs-1))/did
!         cotso(1)=h1/(1.+h1)
!         rhtso(1)=(tso(nzs)+h1*(tso(nzs-1)-tso(nzs)))/
!     1         (1.+h1)
        cotso(1)=0.
        rhtso(1)=tso(nzs)
        do 33 k=1,nzs2
          kn=nzs-k
          k1=2*kn-3
          x1=dtdzs(k1)*thdif(kn-1)
          x2=dtdzs(k1+1)*thdif(kn)
          ft=tso(kn)+x1*(tso(kn-1)-tso(kn))                             &
             -x2*(tso(kn)-tso(kn+1))
          denom=1.+x1+x2-x2*cotso(k)
          cotso(k+1)=x1/denom
          rhtso(k+1)=(ft+x2*rhtso(k))/denom
   33  continue

!************************************************************************
!--- the heat balance equation (Smirnova et al., 1996, eq. 21,26)

        rhcs=cap(1)

        h=mavail

        trans=transum*drycan/zshalf(nroot+1)
        can=wetcan+trans
        umveg=(1.-vegfrac) * soilres
 2111   continue
        fkt=tkms
        d1=cotso(nzs1)
        d2=rhtso(nzs1)
        tn=soilt
        d9=thdif(1)*rhcs*dzstop
        d10=tkms*cp*rho
        r211=.5*conflx/delt
        r21=r211*cp*rho
        r22=.5/(thdif(1)*delt*dzstop**2)
        r6=emiss *stbolt*.5*tn**4
        r7=r6/tn
        d11=rnet+r6
        tdenom=d9*(1.-d1+r22)+d10+r21+r7                              &
              +rainf*cvw*prcpms
        fkq=qkms*rho
        r210=r211*rho
        c=vegfrac*fkq*can
        cc=c*xlv/tdenom
        aa=xlv*(fkq*umveg+r210)/tdenom
        bb=(d10*tabs+r21*tn+xlv*(qvatm*                               &
        (fkq*umveg+c)                                                 & 
        +r210*qvg)+d11+d9*(d2+r22*tn)                                 &
        +rainf*cvw*prcpms*max(273.15,tabs)                            &
         )/tdenom
        aa1=aa+cc
!        aa1=aa*alfa+cc
        pp=patm*1.e3
        aa1=aa1/pp
        call vilka(tn,aa1,bb,pp,qs1,ts1,tbq,ktau,i,j,iland,isoil)
        tq2=qvatm
        tx2=tq2*(1.-h)
        q1=tx2+h*qs1
!        q1=alfa*qs1
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
        print *,'vilka1 - ts1,qs1,tq2,h,tx2,q1',ts1,qs1,tq2,h,tx2,q1
    endif
!with alfa        go to 100
        if(q1.lt.qs1) goto 100
!--- if no saturation - goto 100
!--- if saturation - goto 90
   90   qvg=qs1
        qsg=qs1
        tso(1)=ts1
        qcg=max(0.,q1-qs1)
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
        print *,'90 qvg,qsg,qcg,tso(1)',qvg,qsg,qcg,tso(1)
    endif
        goto 200
  100   bb=bb-aa*tx2
        aa=(aa*h+cc)/pp
        call vilka(tn,aa,bb,pp,qs1,ts1,tbq,ktau,i,j,iland,isoil)
        q1=tx2+h*qs1
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
        print *,'vilka2 - ts1,qs1,tq2,h,tx2,q1',ts1,qs1,tq2,h,tx2,q1
    endif
        if(q1.ge.qs1) goto 90
!with alfa  100  continue
        qsg=qs1
        qvg=q1
!   if( qs1>qvatm .and. qvatm > qvg) then
! very dry soil 
!     print *,'very dry soils mavail,qvg,qs1,qvatm,ts1',i,j,mavail,qvg,qs1,qvatm,ts1
!        qvg = qvatm
!   endif
        tso(1)=ts1
        qcg=0.
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'q1,qsg,qvg,qvatm,alfa,h',q1,qsg,qvg,qvatm,alfa,h
    endif
  200   continue
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
        print *,'200 qvg,qsg,qcg,tso(1)',qvg,qsg,qcg,tso(1)
    endif

!--- soilt - skin temperature
          soilt=ts1

!---- final solution for soil temperature - tso
          do k=2,nzs
            kk=nzs-k+1
            tso(k)=rhtso(kk)+cotso(kk)*tso(k-1)
          end do

         x= (cp*rho*r211+rhcs*zsmain(2)*0.5/delt)*(soilt-tn) + &
            xlv*rho*r211*(qvg-qgold) 
!
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
        print*,'soiltemp storage, i,j,x,soilt,tn,qvg,qvgold', &
                                  i,j,x,soilt,tn,qvg,qgold
        print *,'temp term (cp*rho*r211+rhcs*zsmain(2)*0.5/delt)*(soilt-tn)',&
                 (cp*rho*r211+rhcs*zsmain(2)*0.5/delt)*(soilt-tn)
        print *,'qv term xlv*rho*r211*(qvg-qgold)',xlv*rho*r211*(qvg-qgold)
    endif
         x=x &
! "heat" from rain
        -rainf*cvw*prcpms*(max(273.15,tabs)-soilt)

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
        print *,'x=',x
    endif

!--------------------------------------------------------------------
   end subroutine soiltemp
!--------------------------------------------------------------------


           subroutine snowtemp(                                    & 
!--- input variables
           i,j,iland,isoil,                                        &
           delt,ktau,conflx,nzs,nddzs,nroot,                       &
           snwe,snwepr,snhei,newsnow,snowfrac,                     &
           beta,deltsn,snth,rhosn,rhonewsn,meltfactor,             &  ! add meltfactor
           prcpms,rainf,                                           &
           patm,tabs,qvatm,qcatm,                                  &
           glw,gsw,emiss,rnet,                                     &
           qkms,tkms,pc,rho,vegfrac,                               &
           thdif,cap,drycan,wetcan,cst,                            &
           tranf,transum,dew,mavail,                               &
!--- soil fixed fields
           dqm,qmin,psis,bclh,                                     &
           zsmain,zshalf,dtdzs,tbq,                                &
!--- constants
           xlvm,cp,rovcp,g0_p,cvw,stbolt,                          &
!--- output variables
           snweprint,snheiprint,rsm,                               &
           tso,soilt,soilt1,tsnav,qvg,qsg,qcg,                     &
           smelt,snoh,snflx,s,ilnb,x)

!********************************************************************
!   energy budget equation and heat diffusion eqn are 
!   solved here to obtain snow and soil temperatures
!
!     delt - time step (s)
!     ktau - numver of time step
!     conflx - depth of constant flux layer (m)
!     ime, jme, kme, nzs - dimensions of the domain 
!     nroot - number of levels within the root zone
!     prcpms - precipitation rate in m/s
!     cotso, rhtso - coefficients for implicit solution of
!                     heat diffusion equation
!     thdif - thermal diffusivity (w/m/k)
!     qsg,qvg,qcg - saturated mixing ratio, mixing ratio of
!                   water vapor and cloud at the ground
!                   surface, respectively (kg/kg)
!     patm - pressure [bar]
!     qcatm,qvatm - cloud and water vapor mixing ratio
!                   at the first atm. level (kg/kg)
!     emiss,rnet - emissivity (0-1) of the ground surface and net
!                  radiation at the surface (w/m^2)
!     qkms - exchange coefficient for water vapor in the
!              surface layer (m/s)
!     tkms - exchange coefficient for heat in the surface
!              layer (m/s)
!     pc - plant coefficient (resistance)
!     rho - density of atmosphere near surface (kg/m^3)
!     vegfrac - greeness fraction (0-1)
!     cap - volumetric heat capacity (j/m^3/k)
!     drycan - dry fraction of vegetated area where
!              transpiration may take place (0-1) 
!     wetcan - fraction of vegetated area covered by canopy
!              water (0-1)
!     transum - transpiration function integrated over the 
!               rooting zone (m)
!     dew -  dew in kg/m^2/s
!     mavail - fraction of maximum soil moisture in the top
!               layer (0-1)
!     zsmain - main levels in soil (m)
!     zshalf - middle of the soil layers (m)
!     dtdzs - dt/(2.*dzshalf*dzmain)
!     tbq - table to define saturated mixing ration
!           of water vapor for given temperature and pressure
!     tso - soil temperature (k)
!     soilt - skin temperature (k)
!
!*********************************************************************

        implicit none
!---------------------------------------------------------------------
!--- input variables

   integer,  intent(in   )   ::  nroot,ktau,nzs                , &
                                 nddzs                             !nddzs=2*(nzs-2)

   integer,  intent(in   )   ::  i,j,iland,isoil
   real,     intent(in   )   ::  delt,conflx,prcpms            , &
                                 rainf,newsnow,deltsn,snth     , &
                                 tabs,transum,snwepr           , &
                                 rhonewsn,meltfactor
   real                      ::  rhonewcsn

!--- 3-d atmospheric variables
   real,                                                         &
            intent(in   )    ::                            patm, &
                                                          qvatm, &
                                                          qcatm
!--- 2-d variables
   real                                                        , &
            intent(in   )    ::                             glw, &
                                                            gsw, &
                                                            rho, &
                                                             pc, &
                                                        vegfrac, &
                                                           qkms, &
                                                           tkms

!--- soil properties
   real                                                        , &
            intent(in   )    ::                                  &
                                                           bclh, &
                                                            dqm, &
                                                           psis, &
                                                           qmin

   real,     intent(in   )   ::                              cp, &
                                                          rovcp, &
                                                            cvw, &
                                                         stbolt, &
                                                           xlvm, &
                                                            g0_p


   real,     dimension(1:nzs), intent(in)  ::            zsmain, &
                                                         zshalf, &
                                                          thdif, &
                                                            cap, &
                                                          tranf 

   real,     dimension(1:nddzs), intent(in)  ::           dtdzs

   real,     dimension(1:5001), intent(in)  ::              tbq


!--- input/output variables
!-------- 3-d soil moisture and temperature
   real,     dimension(  1:nzs )                               , &
             intent(inout)   ::                             tso


!-------- 2-d variables
   real                                                        , &
             intent(inout)   ::                             dew, &
                                                            cst, &
                                                          rhosn, &
                                                          emiss, &
                                                         mavail, &
                                                            qvg, &
                                                            qsg, &
                                                            qcg, &
                                                           snwe, &
                                                          snhei, &
                                                       snowfrac, &
                                                          smelt, &
                                                           snoh, &
                                                          snflx, &
                                                              s, &
                                                          soilt, &
                                                         soilt1, &
                                                          tsnav

   real,     intent(inout)                  ::   drycan, wetcan           

   real,     intent(out)                    ::              rsm, &
                                                      snweprint, &
                                                     snheiprint
   integer,  intent(out)                    ::             ilnb
!--- local variables


   integer ::  nzs1,nzs2,k,k1,kn,kk

   real    ::  x,x1,x2,x4,dzstop,can,ft,sph,                     &
               tn,trans,umveg,denom

   real    ::  cotsn,rhtsn,xsn1,ddzsn1,x1sn1,ftsnow,denomsn

   real    ::  t3,upflux,xinet,ras,                              &
               xlmelt,rhocsn,thdifsn,                            &
               beta,epot,xsn,ddzsn,x1sn,d1sn,d2sn,d9sn,r22sn

   real    ::  fso,fsn,                                          &
               fkt,d1,d2,d9,d10,did,r211,r21,r22,r6,r7,d11,      &
               pi,h,fkq,r210,aa,bb,pp,q1,qs1,ts1,tq2,tx2,        &
               tdenom,c,cc,aa1,rhcs,h1,                          &
               tsob, snprim, sh1, sh2,                           &
               smeltg,snohg,snodif,soh,                          &
               cmc2ms,tnold,qgold,snohgnew                            

   real,     dimension(1:nzs)  ::  transp,cotso,rhtso
   real                        ::                         edir1, &
                                                            ec1, &
                                                           ett1, &
                                                           eeta, &
                                                            qfx, &
                                                            hfx

   real                        :: rnet,rsmfrac,soiltfrac,hsn,rr,keff,fact
   integer                     ::      nmelt, iter

!-----------------------------------------------------------------

       iter = 0

       do k=1,nzs
          transp   (k)=0.
          cotso    (k)=0.
          rhtso    (k)=0.
       enddo
       !-- options for snow conductivity:
       !-- 1 - constant
       !-- opt 2 -  Sturm et al., 1997
       keff = 0.265

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
print *, 'snowtemp: snhei,snth,soilt1: ',snhei,snth,soilt1,soilt 
    endif
        xlmelt=3.35e+5
        rhocsn=2090.* rhosn
!18apr08 - add rhonewcsn
        rhonewcsn=2090.* rhonewsn

        if(isncond_opt == 1) then
        !-- old version thdifsn = 0.265/rhocsn
        thdifsn = 0.265/rhocsn
        else
        !-- 07jun19 - thermal conductivity (k_eff) from Sturm et al.(1997)
        !-- keff = 10. ** (2.650 * rhosn*1.e-3 - 1.652)
           fact = 1.
           if(rhosn < 156. .or. (newsnow > 0. .and. rhonewsn < 156.)) then
             keff = 0.023 + 0.234 * rhosn * 1.e-3
           else
             keff = 0.138 - 1.01 * rhosn*1.e-3 + 3.233 * rhosn**2 * 1.e-6
           endif
           if(newsnow <= 0. .and. snhei > 1. .and. rhosn > 250.) then
           !-- some areas with large snow depth have unrealistically 
           !-- low snow density (in the rockie's with snow depth > 1 m). 
           !-- based on Sturm et al. keff=0.452 typical for hard snow slabs
           !-- with rhosn=488 kg/m^3. thdifsn = 0.452/(2090*488)=4.431718e-7
           !-- in future a better compaction scheme is needed for these areas.
             thdifsn = 4.431718e-7
           else
             thdifsn = keff/rhocsn * fact
           endif
         endif ! isncond_opt

        ras=rho*1.e-3

        soiltfrac=soilt

        smelt=0.
        soh=0.
        smeltg=0.
        snohg=0.
        snodif=0.
        rsm = 0.
        rsmfrac = 0.
        fsn=1.
        fso=0.

          nzs1=nzs-1
          nzs2=nzs-2

        qgold=qvg
        dzstop=1./(zsmain(2)-zsmain(1))

!******************************************************************************
!       coefficients for thomas algorithm for tso
!******************************************************************************
!         did=2.*(zsmain(nzs)-zshalf(nzs))
!         h1=dtdzs(8)*thdif(nzs-1)*(zshalf(nzs)-zshalf(nzs-1))/did
!         cotso(1)=h1/(1.+h1)
!         rhtso(1)=(tso(nzs)+h1*(tso(nzs-1)-tso(nzs)))/
!     1         (1.+h1)

        cotso(1)=0.
        rhtso(1)=tso(nzs)
        do 33 k=1,nzs2
          kn=nzs-k
          k1=2*kn-3
          x1=dtdzs(k1)*thdif(kn-1)
          x2=dtdzs(k1+1)*thdif(kn)
          ft=tso(kn)+x1*(tso(kn-1)-tso(kn))                           &
             -x2*(tso(kn)-tso(kn+1))
          denom=1.+x1+x2-x2*cotso(k)
          cotso(k+1)=x1/denom
          rhtso(k+1)=(ft+x2*rhtso(k))/denom
   33  continue
!--- the nzs element in cotso and rhtso will be for snow
!--- there will be 2 layers in snow if it is deeper than deltsn+snth
       if(snhei.ge.snth) then
        if(snhei.le.deltsn+snth) then
!-- 1-layer snow model
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'1-layer - snth,snhei,deltsn',snth,snhei,deltsn
    endif
         ilnb=1
         snprim=max(snth,snhei)
         tsob=tso(1)
         soilt1=tso(1)
         xsn = delt/2./(zshalf(2)+0.5*snprim)
         ddzsn = xsn / snprim
         x1sn = ddzsn * thdifsn
         x2 = dtdzs(1)*thdif(1)
         ft = tso(1)+x1sn*(soilt-tso(1))                              &
              -x2*(tso(1)-tso(2))
         denom = 1. + x1sn + x2 -x2*cotso(nzs1)
         cotso(nzs)=x1sn/denom
         rhtso(nzs)=(ft+x2*rhtso(nzs1))/denom
         cotsn=cotso(nzs)
         rhtsn=rhtso(nzs)
!*** average temperature of snow pack (c)
         tsnav=0.5*(soilt+tso(1))                                     &
                     -273.15

        else
!-- 2 layers in snow, soilt1 is temperasture at deltsn depth
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'2-layer - snth,snhei,deltsn',snth,snhei,deltsn
    endif
         ilnb=2
         snprim=deltsn
         tsob=soilt1
         xsn = delt/2./(0.5*deltsn)
         xsn1= delt/2./(zshalf(2)+0.5*(snhei-deltsn))
         ddzsn = xsn / deltsn
         ddzsn1 = xsn1 / (snhei-deltsn)
         x1sn = ddzsn * thdifsn
         x1sn1 = ddzsn1 * thdifsn
         x2 = dtdzs(1)*thdif(1)
         ft = tso(1)+x1sn1*(soilt1-tso(1))                            &
              -x2*(tso(1)-tso(2))
         denom = 1. + x1sn1 + x2 - x2*cotso(nzs1)
         cotso(nzs)=x1sn1/denom
         rhtso(nzs)=(ft+x2*rhtso(nzs1))/denom
         ftsnow = soilt1+x1sn*(soilt-soilt1)                          &
               -x1sn1*(soilt1-tso(1))
         denomsn = 1. + x1sn + x1sn1 - x1sn1*cotso(nzs)
         cotsn=x1sn/denomsn
         rhtsn=(ftsnow+x1sn1*rhtso(nzs))/denomsn
!*** average temperature of snow pack (c)
         tsnav=0.5/snhei*((soilt+soilt1)*deltsn                       &
                     +(soilt1+tso(1))*(snhei-deltsn))                 &
                     -273.15
        endif
       endif
       if(snhei.lt.snth.and.snhei.gt.0.) then
!--- snow is too thin to be treated separately, therefore it
!--- is combined with the first soil layer.
         snprim=snhei+zsmain(2)
         fsn=snhei/snprim
         fso=1.-fsn
         soilt1=tso(1)
         tsob=tso(2)
         xsn = delt/2./((zshalf(3)-zsmain(2))+0.5*snprim)
         ddzsn = xsn /snprim
         x1sn = ddzsn * (fsn*thdifsn+fso*thdif(1))
         x2=dtdzs(2)*thdif(2)
         ft=tso(2)+x1sn*(soilt-tso(2))-                              &
                       x2*(tso(2)-tso(3))
         denom = 1. + x1sn + x2 - x2*cotso(nzs-2)
         cotso(nzs1) = x1sn/denom
         rhtso(nzs1)=(ft+x2*rhtso(nzs-2))/denom
         tsnav=0.5*(soilt+tso(1))                                    &
                     -273.15
         cotso(nzs)=cotso(nzs1)
         rhtso(nzs)=rhtso(nzs1)
         cotsn=cotso(nzs)
         rhtsn=rhtso(nzs)

       endif

!************************************************************************
!--- the heat balance equation (Smirnova et al. 1996, eq. 21,26)
!18apr08 nmelt is the flag for melting, and snoh is heat of snow phase changes
       nmelt=0
       snoh=0.

        ett1=0.
        epot=-qkms*(qvatm-qgold)
        rhcs=cap(1)
        h=1.
        trans=transum*drycan/zshalf(nroot+1)
        can=wetcan+trans
        umveg=1.-vegfrac
        fkt=tkms
        d1=cotso(nzs1)
        d2=rhtso(nzs1)
        tn=soilt
        d9=thdif(1)*rhcs*dzstop
        d10=tkms*cp*rho
        r211=.5*conflx/delt
        r21=r211*cp*rho
        r22=.5/(thdif(1)*delt*dzstop**2)
        r6=emiss *stbolt*.5*tn**4
        r7=r6/tn
        d11=rnet+r6

      if(snhei.ge.snth) then
        if(snhei.le.deltsn+snth) then
!--- 1-layer snow
          d1sn = cotso(nzs)
          d2sn = rhtso(nzs)
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'1 layer d1sn,d2sn',i,j,d1sn,d2sn
    endif
        else
!--- 2-layer snow
          d1sn = cotsn
          d2sn = rhtsn
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'2 layers d1sn,d2sn',i,j,d1sn,d2sn
    endif
        endif
        d9sn= thdifsn*rhocsn / snprim
        r22sn = snprim*snprim*0.5/(thdifsn*delt)
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'1 or 2 layers d9sn,r22sn',d9sn,r22sn
    endif
      endif

       if(snhei.lt.snth.and.snhei.gt.0.) then
!--- thin snow is combined with soil
         d1sn = d1
         d2sn = d2
         d9sn = (fsn*thdifsn*rhocsn+fso*thdif(1)*rhcs)/              &
                 snprim
         r22sn = snprim*snprim*0.5                                   &
                 /((fsn*thdifsn+fso*thdif(1))*delt)
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,' combined  d9sn,r22sn,d1sn,d2sn: ',d9sn,r22sn,d1sn,d2sn
    endif
      endif
      if(snhei.eq.0.)then
!--- all snow is sublimated
        d9sn = d9
        r22sn = r22
        d1sn = d1
        d2sn = d2
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
        print *,' snhei = 0, d9sn,r22sn,d1sn,d2sn: ',d9sn,r22sn,d1sn,d2sn
    endif
      endif

 2211   continue

!18apr08  - the snow melt iteration start point
 212    continue

!---- tdenom for snow
        tdenom = d9sn*(1.-d1sn +r22sn)+d10+r21+r7                    &
              +rainf*cvw*prcpms                                      &
              +rhonewcsn*newsnow/delt

        fkq=qkms*rho
        r210=r211*rho
        c=vegfrac*fkq*can
        cc=c*xlvm/tdenom
        aa=xlvm*(beta*fkq*umveg+r210)/tdenom
        bb=(d10*tabs+r21*tn+xlvm*(qvatm*                             &
        (beta*fkq*umveg+c)                                           &
        +r210*qgold)+d11+d9sn*(d2sn+r22sn*tn)                        &
        +rainf*cvw*prcpms*max(273.15,tabs)                           &
        + rhonewcsn*newsnow/delt*min(273.15,tabs)                    &
         )/tdenom
        aa1=aa+cc
        pp=patm*1.e3
        aa1=aa1/pp
        bb=bb-snoh/tdenom

        call vilka(tn,aa1,bb,pp,qs1,ts1,tbq,ktau,i,j,iland,isoil)
        tq2=qvatm
        tx2=tq2*(1.-h)
        q1=tx2+h*qs1
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     print *,'vilka1 - ts1,qs1,tq2,h,tx2,q1',ts1,qs1,tq2,h,tx2,q1
    endif
        if(q1.lt.qs1) goto 100
!--- if no saturation - goto 100
!--- if saturation - goto 90
   90   qvg=qs1
        qsg=qs1
        qcg=max(0.,q1-qs1)
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     print *,'90 qvg,qsg,qcg,tso(1)',qvg,qsg,qcg,tso(1)
    endif
        goto 200
  100   bb=bb-aa*tx2
        aa=(aa*h+cc)/pp
        call vilka(tn,aa,bb,pp,qs1,ts1,tbq,ktau,i,j,iland,isoil)
        q1=tx2+h*qs1
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     print *,'vilka2 - ts1,qs1,h,tx2,q1',ts1,qs1,tq2,h,tx2,q1
    endif
        if(q1.gt.qs1) goto 90
        qsg=qs1
        qvg=q1
        qcg=0.
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     print *,'no saturation qvg,qsg,qcg,tso(1)',qvg,qsg,qcg,tso(1)
    endif
  200   continue

!--- soilt - skin temperature
        soilt=ts1

     if(nmelt==1 .and. snowfrac==1. .and. snwe > 0. .and. soilt > 273.15) then
     !--7feb22 on the second iteration when snoh is known and snwe > 0. after melting,
     !-- check if the snow skin temperature is =<tfrzk
     !-- when a grid cell is fully covered with snow (snowfrac=1) 
     !-- or with partial snow cover and snow_mosaic=1 (snowfrac=1).
         soilt = min(273.15,soilt)
     endif

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     if(i.eq.266.and.j.eq.447) then
            print *,'snwe,snhei,soilt,soilt1,tso',i,j,snwe,snhei,soilt,soilt1,tso
     endif
    endif
! solution for temperature at 7.5 cm depth and snow-soil interface
       if(snhei.ge.snth) then
        if(snhei.gt.deltsn+snth) then
!-- 2-layer snow model
          soilt1=min(273.15,rhtsn+cotsn*soilt)
          tso(1)=rhtso(nzs)+cotso(nzs)*soilt1
          tsob=soilt1
        else
!-- 1 layer in snow
          tso(1)=rhtso(nzs)+cotso(nzs)*soilt
          soilt1=tso(1)
          tsob=tso(1)
        endif
       elseif (snhei > 0. .and. snhei < snth) then
! blended 
         tso(2)=rhtso(nzs1)+cotso(nzs1)*soilt
         tso(1)=(tso(2)+(soilt-tso(2))*fso)
         soilt1=tso(1)
         tsob=tso(2)
       else
!-- very thin or zero snow. if snow is thin we suppose that
!--- tso(i,j,1)=soilt, and later we recompute tso(i,j,1)
         tso(1)=soilt
         soilt1=soilt
         tsob=tso(1)
       endif
       if(nmelt==1.and.snowfrac==1.) then
       !-- second iteration with full snow cover
         soilt1= min(273.15,soilt1)
         tso(1)= min(273.15,tso(1))
         tsob  = min(273.15,tsob)
       endif


!---- final solution for tso
       if (snhei > 0. .and. snhei < snth) then
! blended or snow is melted
          do k=3,nzs
            kk=nzs-k+1
            tso(k)=rhtso(kk)+cotso(kk)*tso(k-1)
          end do

       else
          do k=2,nzs
            kk=nzs-k+1
            tso(k)=rhtso(kk)+cotso(kk)*tso(k-1)
          end do
       endif
!--- for thin snow layer combined with the top soil layer
!--- tso(1) is recomputed by linear interpolation between soilt
!--- and tso(i,j,2)
!       if(snhei.lt.snth.and.snhei.gt.0.)then
!          tso(1)=tso(2)+(soilt-tso(2))*fso
!          soilt1=tso(1)
!          tsob = tso(2)
!       endif


    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
   print *,'soilt,soilt1,tso,tsob,qsg',i,j,soilt,soilt1,tso,tsob,qsg,'nmelt=',nmelt
    endif

     if(nmelt.eq.1) go to 220

!--- if soilt > 273.15 f then melting of snow can happen
!   if(soilt.gt.273.15.and.snhei.gt.0.) then
! if all snow can evaporate, then there is nothing to melt
   if(soilt.gt.273.15.and.beta==1..and.snhei.gt.0.) then
        nmelt = 1
        soiltfrac=snowfrac*273.15+(1.-snowfrac)*soilt
        qsg=min(qsg, qsn(soiltfrac,tbq)/pp)
        qvg=snowfrac*qsg+(1.-snowfrac)*qvg
        t3      = stbolt*tn*tn*tn
        upflux  = t3 * 0.5*(tn + soiltfrac)
        xinet   = emiss*(glw-upflux)
         epot = -qkms*(qvatm-qsg)
         q1=epot*ras

        if (q1.le.0..or.iter==1) then
! ---  condensation
          dew=-epot
          do k=1,nzs
            transp(k)=0.
          enddo

        qfx = -xlvm*rho*dew
        eeta = qfx/xlvm
       else
! ---  evaporation
          do k=1,nroot
            transp(k)=-vegfrac*q1                                     &
                      *tranf(k)*drycan/zshalf(nroot+1)
            ett1=ett1-transp(k)
          enddo
          do k=nroot+1,nzs
            transp(k)=0.
          enddo

        edir1 = q1*umveg * beta
        ec1 = q1 * wetcan * vegfrac
        cmc2ms=cst/delt*ras
        eeta = (edir1 + ec1 + ett1)*1.e3
! to convert from kg m-2 s-1 to m s-1: 1/rho water=1.e-3************ 
        qfx=  xlvm * eeta
       endif

         hfx=-d10*(tabs-soiltfrac)

       if(snhei.ge.snth)then
         soh=thdifsn*rhocsn*(soiltfrac-tsob)/snprim
         snflx=soh
       else
         soh=(fsn*thdifsn*rhocsn+fso*thdif(1)*rhcs)*                   &
              (soiltfrac-tsob)/snprim
         snflx=soh
       endif

!
         x= (r21+d9sn*r22sn)*(soiltfrac-tn) +                        &
            xlvm*r210*(qvg-qgold)
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'snowtemp storage ',i,j,x
      print *,'r21,d9sn,r22sn,soiltfrac,tn,qsg,qvg,qgold,snprim', &
              r21,d9sn,r22sn,soiltfrac,tn,qsg,qvg,qgold,snprim
    endif

!-- snoh is energy flux of snow phase change
        snoh=rnet-qfx -hfx - soh - x                                    & 
                  +rhonewcsn*newsnow/delt*(min(273.15,tabs)-soiltfrac)  &
                  +rainf*cvw*prcpms*(max(273.15,tabs)-soiltfrac) 
        snoh=amax1(0.,snoh)
!-- smelt is speed of melting in m/s
        smelt= snoh /xlmelt*1.e-3
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'1- smelt',i,j,smelt
    endif
      if(epot.gt.0. .and. snwepr.le.epot*ras*delt) then
!-- all snow can evaporate
        beta=snwepr/(epot*ras*delt)
        smelt=amin1(smelt,snwepr/delt-beta*epot*ras)
        snwe=0.
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'2- smelt',i,j,smelt
    endif
          goto 88
      endif

        smelt=amax1(0.,smelt)

!18apr08 - egglston limit
      !-- 22apr22 do not limit snow melting for hail (rhonewsn > 450), or dense snow
      !-- (rhosn > 350.) with very warm surface temperatures (>10c)
      if( (rhosn < 350. .or. (newsnow > 0. .and. rhonewsn < 450.)) .and. soilt < 283. ) then
!        smelt= amin1 (smelt, 5.6e-7*meltfactor*max(1.,(soilt-273.15)))
        smelt= amin1 (smelt, delt/60.*5.6e-8*meltfactor*max(1.,(soilt-273.15)))

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'3- smelt',i,j,smelt
    endif
      endif

! rr - potential melting
        rr=max(0.,snwepr/delt-beta*epot*ras)
        if(smelt > rr) then
        smelt=min(smelt,rr)
          snwe = 0.
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'4- smelt i,j,smelt,rr',i,j,smelt,rr
    endif
        endif

   88   continue
        snohgnew=smelt*xlmelt*1.e3
        snodif=amax1(0.,(snoh-snohgnew))

        snoh=snohgnew
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'snoh,snodif',snoh,snodif
    endif

      if( smelt > 0.) then
!*** from koren et al. (1999) 13% of snow melt stays in the snow pack
        rsmfrac=min(0.18,(max(0.08,snwepr/0.10*0.13)))
       if(snhei > 0.01 .and. rhosn < 350.) then
        rsm=rsmfrac*smelt*delt
       else
! do not keep melted water if snow depth is less that 1 cm
        rsm=0.
       endif
!18apr08 rsm is part of melted water that stays in snow as liquid
       if(rsm > 0.) then
        smelt=max(0.,smelt-rsm/delt)
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'5- smelt i,j,smelt,rsm,snwepr,rsmfrac', &
                        i,j,smelt,rsm,snwepr,rsmfrac
    endif
       endif ! rsm

      endif ! smelt > 0

!-- update of liquid equivalent of snow depth
!-- due to evaporation and snow melt
      if(snwe > 0.) then
        snwe = amax1(0.,(snwepr-                                      &
                    (smelt+beta*epot*ras)*delt                        &
                                         ) )
      endif

!--- if there is no snow melting then just evaporation
!--- or condensation cxhanges snwe
      else
       if(snhei.ne.0..and. beta == 1.) then
               epot=-qkms*(qvatm-qsg)
               snwe = amax1(0.,(snwepr-                               &
                    beta*epot*ras*delt))
       else
       !-- all snow is sublibated
         snwe = 0.
       endif

      endif
!18apr08 - if snow melt occurred then go into iteration for energy budget solution 
     if(nmelt.eq.1) goto 212  ! second interation
 220  continue

      if(smelt.gt.0..and.rsm.gt.0.) then
       if(snwe.le.rsm) then
    if ( 1==1 ) then
     print *,'snwe<rsm snwe,rsm,smelt*delt,epot*ras*delt,beta', &
                     snwe,rsm,smelt*delt,epot*ras*delt,beta
    endif
       else
!*** update snow density on effect of snow melt, melted
!*** from the top of the snow. 13% of melted water
!*** remains in the pack and changes its density.
!*** eq. 9 (with my correction) in koren et al. (1999)
          xsn=(rhosn*(snwe-rsm)+1.e3*rsm)/                            &
              snwe
          rhosn=min(max(58.8,xsn),500.)

          rhocsn=2090.* rhosn
        if(isncond_opt == 1) then
        !-- old version thdifsn = 0.265/rhocsn
          thdifsn = 0.265/rhocsn
        else
        !-- 07jun19 - thermal conductivity (k_eff) from Sturm et al.(1997)
        !-- keff = 10. ** (2.650 * rhosn*1.e-3 - 1.652)
           fact = 1.
           if(rhosn < 156. .or. (newsnow > 0. .and. rhonewsn < 156.)) then
             keff = 0.023 + 0.234 * rhosn * 1.e-3
           else
             keff = 0.138 - 1.01 * rhosn*1.e-3 + 3.233 * rhosn**2 * 1.e-6
           endif
           if(newsnow <= 0. .and. snhei > 1. .and. rhosn > 250.) then
           !-- some areas with large snow depth have unrealistically 
           !-- low snow density (in the rockie's with snow depth > 1 m). 
           !-- based on Sturm et al. keff=0.452 typical for hard snow slabs
           !-- with rhosn=488 kg/m^3. thdifsn = 0.452/(2090*488)=4.431718e-7
           !-- in future a better compaction scheme is needed for these areas.
             thdifsn = 4.431718e-7
           else
             thdifsn = keff/rhocsn * fact
           endif
        endif ! isncond_opt

        endif  
       endif

!--- compute flux in the top snow layer
       if(snhei.ge.snth)then
         s=thdifsn*rhocsn*(soilt-tsob)/snprim
         snflx=s
         s=d9*(tso(1)-tso(2))
       elseif(snhei.lt.snth.and.snhei.gt.0.) then
         s=(fsn*thdifsn*rhocsn+fso*thdif(1)*rhcs)*                   &
              (soilt-tsob)/snprim
         snflx=s
         s=d9*(tso(1)-tso(2))
       else
         s=d9sn*(soilt-tsob)
         snflx=s
         s=d9*(tso(1)-tso(2))
       endif

        snhei=snwe *1.e3 / rhosn
!--  if ground surface temperature
!--  is above freezing snow can melt from the bottom. the following
!--  piece of code will check if bottom melting is possible.

        if(tso(1).gt.273.15 .and. snhei > 0.) then
          if (snhei.gt.deltsn+snth) then
              hsn = snhei - deltsn
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print*,'2 layer snow - snhei,hsn',snhei,hsn
    endif
          else
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print*,'1 layer snow or blended - snhei',snhei
    endif
              hsn = snhei
          endif

         soiltfrac=snowfrac*273.15+(1.-snowfrac)*tso(1)

        snohg=(tso(1)-soiltfrac)*(cap(1)*zshalf(2)+                       &
               rhocsn*0.5*hsn) / delt
        snohg=amax1(0.,snohg)
        snodif=0.
        smeltg=snohg/xlmelt*1.e-3
! egglston - empirical limit on snow melt from the bottom of snow pack
      !9jun22-- the next line excludeis cases of summer hail from snowmelt limiting
      if( (rhosn < 350. .or. (newsnow > 0. .and. rhonewsn < 450.)) .and. soilt < 283. ) then
        smeltg=amin1(smeltg, 5.8e-9)
      endif

! rr - potential melting
        rr=snwe/delt
        smeltg=amin1(smeltg, rr)

        snohgnew=smeltg*xlmelt*1.e3
        snodif=amax1(0.,(snohg-snohgnew))
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'tso(1),soiltfrac,smeltg,snodif',tso(1),soiltfrac,smeltg,snodif
    endif

        snwe=max(0.,snwe-smeltg*delt)
        snhei=snwe *1.e3 / rhosn
        !-- add up all snow melt
        smelt = smelt + smeltg
      
        if(snhei > 0.) tso(1) = soiltfrac
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'melt from the bottom snwe,snhei',snwe,snhei
       if (snhei==0.) &
       print *,'snow is all melted on the warm ground'
    endif

       endif
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'snhei,snoh',i,j,snhei,snoh
    endif
!                                              &
        snweprint=snwe
        snheiprint=snweprint*1.e3 / rhosn

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
print *, 'snweprint : ',snweprint
print *, 'd9sn,soilt,tsob : ', d9sn,soilt,tsob
    endif

         x= (r21+d9sn*r22sn)*(soilt-tn) +                     &
            xlvm*r210*(qsg-qgold)
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'snowtemp storage ',i,j,x
      print *,'r21,d9sn,r22sn,soiltfrac,soilt,tn,qsg,qgold,snprim', &
              r21,d9sn,r22sn,soiltfrac,soilt,tn,qsg,qgold,snprim
    endif

         x=x &
! "heat" from snow and rain
        -rhonewcsn*newsnow/delt*(min(273.15,tabs)-soilt)         &
        -rainf*cvw*prcpms*(max(273.15,tabs)-soilt)
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     print *,'x=',x
     print *,'snhei=',snhei
     print *,'snflx=',snflx
    endif

      if(snhei.gt.0.) then
        if(ilnb.gt.1) then
          tsnav=0.5/snhei*((soilt+soilt1)*deltsn                     &
                    +(soilt1+tso(1))*(snhei-deltsn))                 &
                       -273.15
        else
          tsnav=0.5*(soilt+tso(1)) - 273.15
        endif
      else
          tsnav= soilt - 273.15
      endif

!------------------------------------------------------------------------
   end subroutine snowtemp
!------------------------------------------------------------------------


        subroutine soilmoist (                                  &
!--input parameters
              delt,nzs,nddzs,dtdzs,dtdzs2,riw,                  &
              zsmain,zshalf,diffu,hydro,                        &
              qsg,qvg,qcg,qcatm,qvatm,prcp,                     &
              qkms,transp,drip,                                 &
              dew,smelt,soilice,vegfrac,snowfrac,soilres,       &
!--soil properties
              dqm,qmin,ref,ksat,ras,infmax,                     &
!--output
              soilmois,soiliqw,mavail,runoff,runoff2,infiltrp)
!*************************************************************************
!   moisture balance equation and richards eqn.
!   are solved here 
!   
!     delt - time step (s)
!     ime,jme,nzs - dimensions of soil domain
!     zsmain - main levels in soil (m)
!     zshalf - middle of the soil layers (m)
!     dtdzs -  dt/(2.*dzshalf*dzmain)
!     dtdzs2 - dt/(2.*dzshalf)
!     diffu - diffusional conductivity (m^2/s)
!     hydro - hydraulic conductivity (m/s)
!     qsg,qvg,qcg - saturated mixing ratio, mixing ratio of
!                   water vapor and cloud at the ground
!                   surface, respectively (kg/kg)
!     qcatm,qvatm - cloud and water vapor mixing ratio
!                   at the first atm. level (kg/kg)
!     prcp - precipitation rate in m/s
!     qkms - exchange coefficient for water vapor in the
!              surface layer (m/s)
!     transp - transpiration from the soil layers (m/s)
!     drip - liquid water dripping from the canopy to soil (m)
!     dew -  dew in kg/m^2s
!     smelt - melting rate in m/s
!     soilice - volumetric content of ice in soil (m^3/m^3)
!     soiliqw - volumetric content of liquid water in soil (m^3/m^3)
!     vegfrac - greeness fraction (0-1)
!     ras - ration of air density to soil density
!     infmax - maximum infiltration rate (kg/m^2/s)
!    
!     soilmois - volumetric soil moisture, 6 levels (m^3/m^3)
!     mavail - fraction of maximum soil moisture in the top
!               layer (0-1)
!     runoff - surface runoff (m/s)
!     runoff2 - underground runoff (m)
!     infiltrp - point infiltration flux into soil (m/s)
!            /(snow bottom runoff) (mm/s)
!
!     cosmc, rhsmc - coefficients for implicit solution of
!                     richards equation
!******************************************************************
        implicit none
!------------------------------------------------------------------
!--- input variables
   real,     intent(in   )   ::  delt
   integer,  intent(in   )   ::  nzs,nddzs

! input variables

   real,     dimension(1:nzs), intent(in   )  ::         zsmain, &
                                                         zshalf, &
                                                          diffu, &
                                                          hydro, &
                                                         transp, &
                                                        soilice, &
                                                         dtdzs2

   real,     dimension(1:nddzs), intent(in)  ::           dtdzs

   real,     intent(in   )   ::    qsg,qvg,qcg,qcatm,qvatm     , &
                                   qkms,vegfrac,drip,prcp      , &
                                   dew,smelt,snowfrac          , &
                                   dqm,qmin,ref,ksat,ras,riw,soilres
                         
! output

   real,     dimension(  1:nzs )                               , &

             intent(inout)   ::                soilmois,soiliqw
                                                  
   real,     intent(inout)   ::  mavail,runoff,runoff2,infiltrp, &
                                                        infmax

! local variables

   real,     dimension( 1:nzs )  ::  cosmc,rhsmc

   real    ::  dzs,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10
   real    ::  refkdt,refdk,delt1,f1max,f2max
   real    ::  f1,f2,fd,kdt,val,ddt,px,fk,fkmax
   real    ::  qq,umveg,infmax1,trans
   real    ::  totliq,flx,flxsat,qtot
   real    ::  did,x1,x2,x4,denom,q2,q4
   real    ::  dice,fcr,acrt,frzx,sum,cvfrz

   integer ::  nzs1,nzs2,k,kk,k1,kn,ialp1,jj,jk

!******************************************************************************
!       coefficients for thomas algorithm for soilmois
!******************************************************************************
          nzs1=nzs-1                                                            
          nzs2=nzs-2

 118      format(6(10pf23.19))

           do k=1,nzs
            cosmc(k)=0.
            rhsmc(k)=0.
           enddo
 
        did=(zsmain(nzs)-zshalf(nzs))
        x1=zsmain(nzs)-zsmain(nzs1)

!7may09        did=(zsmain(nzs)-zshalf(nzs))*2.
!        denom=did/delt+diffu(nzs1)/x1
!        cosmc(1)=diffu(nzs1)/x1/denom
!        rhsmc(1)=(soilmois(nzs)*did/delt
!     1   +transp(nzs)-(hydro(nzs)*soilmois(nzs)
!     1   -hydro(nzs1)*soilmois(nzs1))*did
!     1   /x1) /denom

        denom=(1.+diffu(nzs1)/x1/did*delt+hydro(nzs)/(2.*did)*delt)
        cosmc(1)=delt*(diffu(nzs1)/did/x1                                &
                    +hydro(nzs1)/2./did)/denom
        rhsmc(1)=(soilmois(nzs)+transp(nzs)*delt/                         &
               did)/denom

!        rhsmc(1)=(soilmois(nzs)*did/delt  &
!        +transp(nzs)-(hydro(nzs)*soilmois(nzs) &
!        -hydro(nzs1)*soilmois(nzs1))*did &
!        /x1) /denom

!12 june 2014 - low boundary condition: 1 - zero diffusion below the lowest
! level; 2 - soil moisture at the low boundary can be lost due to the root uptake.
! so far - no interaction with the water table.

        denom=1.+diffu(nzs1)/x1/did*delt
        cosmc(1)=delt*(diffu(nzs1)/did/x1                                &  
                    +hydro(nzs1)/did)/denom
        rhsmc(1)=(soilmois(nzs)-hydro(nzs)*delt/did*soilmois(nzs) & 
                 +transp(nzs)*delt/did)/denom
        cosmc(1)=0.
        rhsmc(1)=soilmois(nzs)
!
        do 330 k=1,nzs2
          kn=nzs-k
          k1=2*kn-3
          x4=2.*dtdzs(k1)*diffu(kn-1)
          x2=2.*dtdzs(k1+1)*diffu(kn)
          q4=x4+hydro(kn-1)*dtdzs2(kn-1)
          q2=x2-hydro(kn+1)*dtdzs2(kn-1)
          denom=1.+x2+x4-q2*cosmc(k)
          cosmc(k+1)=q4/denom
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
          print *,'q2,soilmois(kn),diffu(kn),x2,hydro(kn+1),dtdzs2(kn-1),kn,k' &
                  ,q2,soilmois(kn),diffu(kn),x2,hydro(kn+1),dtdzs2(kn-1),kn,k
    endif
 330      rhsmc(k+1)=(soilmois(kn)+q2*rhsmc(k)                            &
                   +transp(kn)                                            &
                   /(zshalf(kn+1)-zshalf(kn))                             &
                   *delt)/denom

! --- moisture balance begins here

          trans=transp(1)
          umveg=(1.-vegfrac)*soilres

          runoff=0.
          runoff2=0.
          dzs=zsmain(2)
          r1=cosmc(nzs1)
          r2= rhsmc(nzs1)
          r3=diffu(1)/dzs
          r4=r3+hydro(1)*.5          
          r5=r3-hydro(2)*.5
          r6=qkms*ras
!-- total liquid water available on the top of soil domain
!-- without snow - 3 sources of water: precipitation,
!--         water dripping from the canopy and dew 
!-- with snow - only one source of water - snow melt

  191   format (f23.19)

!        totliq=umveg*prcp-drip/delt-umveg*dew*ras-smelt

        totliq=prcp-drip/delt-umveg*dew*ras-smelt
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
print *,'umveg*prcp,drip/delt,umveg*dew*ras,smelt', &
         umveg*prcp,drip/delt,umveg*dew*ras,smelt
    endif

        flx=totliq
        infiltrp=totliq

! -----------     frozen ground version    -------------------------
!   reference frozen ground parameter, cvfrz, is a shape parameter of
!   areal distribution function of soil ice content which equals 1/cv.
!   cv is a coefficient of spatial variation of soil ice content.
!   based on field data cv depends on areal mean of frozen depth, and it
!   close to constant = 0.6 if areal mean frozen depth is above 20 cm.
!   that is why parameter cvfrz = 3 (int{1/0.6*0.6})
!
!   current logic doesn't allow cvfrz be bigger than 3
         cvfrz = 3.

!-- schaake/koren expression for calculation of max infiltration
         refkdt=3.
         refdk=3.4341e-6
         delt1=delt/86400.
         f1max=dqm*zshalf(2)
         f2max=dqm*(zshalf(3)-zshalf(2))
         f1=f1max*(1.-soilmois(1)/dqm)
         dice=soilice(1)*zshalf(2)
         fd=f1
        do k=2,nzs1
         dice=dice+(zshalf(k+1)-zshalf(k))*soilice(k)
         fkmax=dqm*(zshalf(k+1)-zshalf(k))
         fk=fkmax*(1.-soilmois(k)/dqm)
         fd=fd+fk
        enddo
         kdt=refkdt*ksat/refdk
         val=(1.-exp(-kdt*delt1))
         ddt = fd*val
         px= - totliq * delt
         if(px.lt.0.0) px = 0.0
         if(px.gt.0.0) then
           infmax1 = (px*(ddt/(px+ddt)))/delt
         else
           infmax1 = 0.
         endif
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
  print *,'infmax1 before frozen part',infmax1
    endif

! -----------     frozen ground version    --------------------------
!    reduction of infiltration based on frozen ground parameters
!
! ------------------------------------------------------------------

         frzx= 0.15*((dqm+qmin)/ref) * (0.412 / 0.468)
         fcr = 1.
         if ( dice .gt. 1.e-2) then
           acrt = cvfrz * frzx / dice
           sum = 1.
           ialp1 = cvfrz - 1
           do jk = 1,ialp1
              k = 1
              do jj = jk+1, ialp1
                k = k * jj
              end do
              sum = sum + (acrt ** ( cvfrz-jk)) / float (k)
           end do
           fcr = 1. - exp(-acrt) * sum
         end if
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
          print *,'fcr--------',fcr
          print *,'dice=',dice
    endif
         infmax1 = infmax1* fcr
! -------------------------------------------------------------------

         infmax = max(infmax1,hydro(1)*soilmois(1))
         infmax = min(infmax, -totliq)
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
print *,'infmax,infmax1,hydro(1)*soiliqw(1),-totliq', &
         infmax,infmax1,hydro(1)*soiliqw(1),-totliq
    endif
!----
          if (-totliq.gt.infmax)then
            runoff=-totliq-infmax
            flx=-infmax
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
       print *,'flx,runoff1=',flx,runoff
    endif
          endif
! infiltrp is total infiltration flux in m/s
          infiltrp=flx
! solution of moisture budget
          r7=.5*dzs/delt
          r4=r4+r7
          flx=flx-soilmois(1)*r7
! r8 is for direct evaporation from soil, which occurs
! only from snow-free areas
!          r8=umveg*r6
          r8=umveg*r6*(1.-snowfrac)
          qtot=qvatm+qcatm
          r9=trans
          r10=qtot-qsg

!-- evaporation regime
          if(r10.le.0.) then
            qq=(r5*r2-flx+r9)/(r4-r5*r1-r10*r8/(ref-qmin))
            flxsat=-dqm*(r4-r5*r1-r10*r8/(ref-qmin))                &
                   +r5*r2+r9
          else
!-- dew formation regime
            qq=(r2*r5-flx+r8*(qtot-qcg-qvg)+r9)/(r4-r1*r5)
            flxsat=-dqm*(r4-r1*r5)+r2*r5+r8*(qtot-qvg-qcg)+r9
          end if

          if(qq.lt.0.) then
!  print *,'negative qq=',qq
            soilmois(1)=1.e-8

          else if(qq.gt.dqm) then
!-- saturation
            soilmois(1)=dqm
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
   print *,'flxsat,flx,delt',flxsat,flx,delt,runoff2
    endif
!            runoff2=(flxsat-flx)
            runoff=runoff+(flxsat-flx)
          else
            soilmois(1)=min(dqm,max(1.e-8,qq))
          end if

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
   print *,'soilmois,soiliqw, soilice',soilmois,soiliqw,soilice*riw
   print *,'cosmc,rhsmc',cosmc,rhsmc
    endif
!--- final solution for soilmois 
!          do k=2,nzs1
          do k=2,nzs
            kk=nzs-k+1
            qq=cosmc(kk)*soilmois(k-1)+rhsmc(kk)
!            qq=cosmc(kk)*soiliqw(k-1)+rhsmc(kk)

           if (qq.lt.0.) then
!  print *,'negative qq=',qq
            soilmois(k)=1.e-8 

           else if(qq.gt.dqm) then
!-- saturation
            soilmois(k)=dqm
             if(k.eq.nzs)then
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
   print *,'hydro(k),qq,dqm,k',hydro(k),qq,dqm,k
    endif
               runoff2=runoff2+((qq-dqm)*(zsmain(k)-zshalf(k)))/delt
             else
               runoff2=runoff2+((qq-dqm)*(zshalf(k+1)-zshalf(k)))/delt
             endif
           else
            soilmois(k)=min(dqm,max(1.e-8,qq))
           end if
          end do
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
   print *,'end soilmois,soiliqw,soilice',soilmois,soiliqw,soilice*riw
    endif

           mavail=max(.00001,min(1.,(soilmois(1)/(ref-qmin)*(1.-snowfrac)+1.*snowfrac)))

!        return
!        end
!-------------------------------------------------------------------
    end subroutine soilmoist
!-------------------------------------------------------------------


          subroutine soilprop(spp_lsm,rstochcol,fieldcol_sf, &
!--- input variables
         nzs,fwsat,lwsat,tav,keepfr,                              &
         soilmois,soiliqw,soilice,                                &
         soilmoism,soiliqwm,soilicem,                             &
!--- soil fixed fields
         qwrtz,rhocs,dqm,qmin,psis,bclh,ksat,                     &
!--- constants
         riw,xlmelt,cp,g0_p,cvw,ci,                               & 
         kqwrtz,kice,kwt,                                         &
!--- output variables
         thdif,diffu,hydro,cap)

!******************************************************************
! soilprop computes thermal diffusivity, and diffusional and
!          hydraulic condeuctivities
!******************************************************************
! nx,ny,nzs - dimensions of soil domain
! fwsat, lwsat - volumetric content of frozen and liquid water
!                for saturated condition at given temperatures (m^3/m^3)
! tav - temperature averaged for soil layers (k)
! soilmois - volumetric soil moisture at the main soil levels (m^3/m^3)
! soilmoism - volumetric soil moisture averaged for layers (m^3/m^3)
! soiliqwm - volumetric liquid soil moisture averaged for layers (m^3/m^3)
! soilicem - volumetric content of soil ice averaged for layers (m^3/m^3)
! thdif - thermal diffusivity for soil layers (w/m/k)
! diffu - diffusional conductivity (m^2/s)
! hydro - hydraulic conductivity (m/s)
! cap - volumetric heat capacity (j/m^3/k)
!
!******************************************************************

        implicit none
!-----------------------------------------------------------------

!--- soil properties
   integer, intent(in   )    ::                            nzs
   real                                                        , &
            intent(in   )    ::                           rhocs, &
                                                           bclh, &
                                                            dqm, &
                                                           ksat, &
                                                           psis, &
                                                          qwrtz, &  
                                                           qmin

   real,    dimension(  1:nzs )                                , &
            intent(in   )    ::                        soilmois, &
                                                         keepfr


   real,     intent(in   )   ::                              cp, &
                                                            cvw, &
                                                            riw, &  
                                                         kqwrtz, &
                                                           kice, &
                                                            kwt, &
                                                         xlmelt, &
                                                            g0_p

   real,     dimension(1:nzs), intent(in)  ::          rstochcol
   real,     dimension(1:nzs), intent(inout) ::      fieldcol_sf
   integer,  intent(in   )   ::                     spp_lsm      


!--- output variables
   real,     dimension(1:nzs)                                  , &
            intent(inout)  ::      cap,diffu,hydro             , &
                                   thdif,tav                   , &
                                   soilmoism                   , &
                                   soiliqw,soilice             , &
                                   soilicem,soiliqwm           , &
                                   fwsat,lwsat

!--- local variables
   real,     dimension(1:nzs)  ::  hk,detal,kasat,kjpl

   real    ::  x,x1,x2,x4,ws,wd,fact,fach,facd,psif,ci
   real    ::  tln,tavln,tn,pf,a,am,ame,h
   integer ::  nzs1,k

!-- for johansen thermal conductivity
   real    ::  kzero,gamd,kdry,kas,x5,sr,ke       
               

         nzs1=nzs-1

!-- constants for johansen (1975) thermal conductivity
         kzero =2.       ! if qwrtz > 0.2


         do k=1,nzs
            detal (k)=0.
            kasat (k)=0.
            kjpl  (k)=0.
            hk    (k)=0.
         enddo

           ws=dqm+qmin
           x1=xlmelt/(g0_p*psis)
           x2=x1/bclh*ws
           x4=(bclh+1.)/bclh
!--- next 3 lines are for johansen thermal conduct.
           gamd=(1.-ws)*2700.
           kdry=(0.135*gamd+64.7)/(2700.-0.947*gamd)
           !-- one more option from christa's paper
           if(qwrtz > 0.2) then
           kas=kqwrtz**qwrtz*kzero**(1.-qwrtz)
           else
             kas=kqwrtz**qwrtz*3.**(1.-qwrtz)
           endif

         do k=1,nzs1
           tn=tav(k) - 273.15
           wd=ws - riw*soilicem(k)
           psif=psis*100.*(wd/(soiliqwm(k)+qmin))**bclh            &
                * (ws/wd)**3.
!--- psif should be in [cm] to compute pf
           pf=log10(abs(psif))
           fact=1.+riw*soilicem(k)
!--- hk is for mccumber thermal conductivity
         if(pf.le.5.2) then
           hk(k)=420.*exp(-(pf+2.7))*fact
         else
           hk(k)=.1744*fact
         end if

           if(soilicem(k).ne.0.and.tn.lt.0.) then
!--- detal is taking care of energy spent on freezing or released from 
!          melting of soil water

              detal(k)=273.15*x2/(tav(k)*tav(k))*                  &
                     (tav(k)/(x1*tn))**x4

              if(keepfr(k).eq.1.) then
                 detal(k)=0.
              endif

           endif

!--- next 10 lines calculate johansen thermal conductivity kjpl
           kasat(k)=kas**(1.-ws)*kice**fwsat(k)                    &
                    *kwt**lwsat(k)

           x5=(soilmoism(k)+qmin)/ws
         if(soilicem(k).eq.0.) then
           sr=max(0.101,x5)
           ke=log10(sr)+1.
!--- next 2 lines - for coarse soils
!           sr=max(0.0501,x5)
!           ke=0.7*log10(sr)+1.
         else
           ke=x5
         endif

           kjpl(k)=ke*(kasat(k)-kdry)+kdry

!--- cap -volumetric heat capacity
            cap(k)=(1.-ws)*rhocs                                    &
                  + (soiliqwm(k)+qmin)*cvw                          &
                  + soilicem(k)*ci                                  &
                  + (dqm-soilmoism(k))*cp*1.2                       &
            - detal(k)*1.e3*xlmelt

           a=riw*soilicem(k)

        if((ws-a).lt.0.12)then
           diffu(k)=0.
        else
           h=max(0.,(soilmoism(k)+qmin-a)/(max(1.e-8,(ws-a))))
           facd=1.
        if(a.ne.0.)facd=1.-a/max(1.e-8,soilmoism(k))
          ame=max(1.e-8,ws-riw*soilicem(k))
!--- diffu is diffusional conductivity of soil water
          diffu(k)=-bclh*ksat*psis/ame*                             &
                  (ws/ame)**3.                                     &
                  *h**(bclh+2.)*facd
         endif

!--- thdif - thermal diffusivity
!           thdif(k)=hk(k)/cap(k)
!--- use thermal conductivity from johansen (1975)
            thdif(k)=kjpl(k)/cap(k)

         end do

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
   print *,'soilice*riw,soiliqw,soilmois,ws',soilice*riw,soiliqw,soilmois,ws
    endif
         do k=1,nzs

         if((ws-riw*soilice(k)).lt.0.12)then
            hydro(k)=0.
         else
            fach=1.
          if(soilice(k).ne.0.)                                     &
             fach=1.-riw*soilice(k)/max(1.e-8,soilmois(k))
         am=max(1.e-8,ws-riw*soilice(k))
!--- hydro is hydraulic conductivity of soil water
          hydro(k)=min(ksat,ksat/am*                                        & 
                  (soiliqw(k)/am)                                  &
                  **(2.*bclh+2.)                                   &
                  * fach)
          if(hydro(k)<1.e-10)hydro(k)=0.
         endif

       enddo

!-----------------------------------------------------------------------
   end subroutine soilprop
!-----------------------------------------------------------------------


           subroutine transf(i,j,                                &
!--- input variables
              nzs,nroot,soiliqw,tabs,lai,gswin,                  &
!--- soil fixed fields
              dqm,qmin,ref,wilt,zshalf,pc,iland,                 &
!--- output variables
              tranf,transum)

!-------------------------------------------------------------------
!--- tranf(k) - the transpiration function (Smirnova et al. 1996, eq. 18,19)
!*******************************************************************
! nx,ny,nzs - dimensions of soil domain
! soiliqw - volumetric liquid soil moisture at the main levels (m^3/m^3)
! tranf - the transpiration function at levels (m)
! transum - transpiration function integrated over the rooting zone (m)
!
!*******************************************************************
        implicit none
!-------------------------------------------------------------------

!--- input variables

   integer,  intent(in   )   ::  i,j,nroot,nzs, iland

   real                                                        , &
            intent(in   )    ::                gswin, tabs, lai
!--- soil properties
   real                                                        , &
            intent(in   )    ::                             dqm, &
                                                           qmin, &
                                                            ref, &
                                                             pc, &
                                                           wilt

   real,     dimension(1:nzs), intent(in)  ::          soiliqw,  &
                                                         zshalf

!-- output 
   real,     dimension(1:nzs), intent(out)  ::            tranf
   real,     intent(out)  ::                            transum  

!-- local variables
   real    ::  totliq, did
   integer ::  k

!-- for non-linear root distribution
   real    ::  gx,sm1,sm2,sm3,sm4,ap0,ap1,ap2,ap3,ap4
   real    ::  ftem, pctot, fsol, f1, cmin, cmax, totcnd
   real,     dimension(1:nzs)   ::           part
!--------------------------------------------------------------------

        do k=1,nzs
           part(k)=0.
           tranf(k)=0.
        enddo

        transum=0.
        totliq=soiliqw(1)+qmin
           sm1=totliq
           sm2=sm1*sm1
           sm3=sm2*sm1
           sm4=sm3*sm1
           ap0=0.299
           ap1=-8.152
           ap2=61.653
           ap3=-115.876
           ap4=59.656
           gx=ap0+ap1*sm1+ap2*sm2+ap3*sm3+ap4*sm4
          if(totliq.ge.ref) gx=1.
          if(totliq.le.0.) gx=0.
          if(gx.gt.1.) gx=1.
          if(gx.lt.0.) gx=0.
        did=zshalf(2)
          part(1)=did*gx
        if(totliq.gt.ref) then
          tranf(1)=did
        else if(totliq.le.wilt) then
          tranf(1)=0.
        else
          tranf(1)=(totliq-wilt)/(ref-wilt)*did
        endif 
!-- uncomment next line for non-linear root distribution
!          tranf(1)=part(1)

        do k=2,nroot
        totliq=soiliqw(k)+qmin
           sm1=totliq
           sm2=sm1*sm1
           sm3=sm2*sm1
           sm4=sm3*sm1
           gx=ap0+ap1*sm1+ap2*sm2+ap3*sm3+ap4*sm4
          if(totliq.ge.ref) gx=1.
          if(totliq.le.0.) gx=0.
          if(gx.gt.1.) gx=1.
          if(gx.lt.0.) gx=0.
          did=zshalf(k+1)-zshalf(k)
          part(k)=did*gx
        if(totliq.ge.ref) then
          tranf(k)=did
        else if(totliq.le.wilt) then
          tranf(k)=0.
        else
          tranf(k)=(totliq-wilt)                                &
                /(ref-wilt)*did
        endif
!-- uncomment next line for non-linear root distribution
!          tranf(k)=part(k)
        end do

! for lai> 3 =>  transpiration at potential rate (f.tardieu, 2013)
      if(lai > 4.) then
        pctot=0.8
      else
        pctot=pc
!- 26aug16-  next 2 lines could lead to lh increase and higher 2-m q during the day
!        pctot=min(0.8,pc*lai)
!        pctot=min(0.8,max(pc,pc*lai))
      endif
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     print *,'i,j,pctot,lai,pc',i,j,pctot,lai,pc
    endif
!---
!--- air temperature function
!     Avissar (1985) and Ax 7/95
        if (tabs .le. 302.15) then
          ftem = 1.0 / (1.0 + exp(-0.41 * (tabs - 282.05)))
        else
          ftem = 1.0 / (1.0 + exp(0.5 * (tabs - 314.0)))
        endif
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     print *,'i,j,tabs,ftem',i,j,tabs,ftem
    endif
!--- incoming solar function
     cmin = 1./rsmax_data
     cmax = 1./rstbl(iland)
    if(lai > 1.) then
     cmax = lai/rstbl(iland) ! max conductance
    endif
! noihlan & planton (1988)
       f1=0.
!    if(lai > 0.01) then
!       f1 = 1.1/lai*gswin/rgltbl(iland)! f1=0. when gswin=0.
!       fsol = (f1+cmin/cmax)/(1.+f1)
!       fsol=min(1.,fsol)
!    else
!       fsol=cmin/cmax
!    endif
!     totcnd = max(lai/rstbl(iland), pctot * ftem * f1) 
! Mahrer & Avissar (1982), Avissar et al. (1985)
     if (gswin < rgltbl(iland)) then
      fsol = 1. / (1. + exp(-0.034 * (gswin - 3.5)))
     else
      fsol = 1.
     endif
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     print *,'i,j,gswin,lai,f1,fsol',i,j,gswin,lai,f1,fsol
    endif
!--- total conductance
     totcnd =(cmin + (cmax - cmin)*pctot*ftem*fsol)/cmax

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
     print *,'i,j,iland,rgltbl(iland),rstbl(iland),rsmax_data,totcnd'  &
             ,i,j,iland,rgltbl(iland),rstbl(iland),rsmax_data,totcnd
    endif

!-- transum - total for the rooting zone
          transum=0.
        do k=1,nroot
! linear root distribution
         tranf(k)=max(cmin,tranf(k)*totcnd)
         transum=transum+tranf(k)
        end do
    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      print *,'i,j,transum,tranf',i,j,transum,tranf
    endif

!-----------------------------------------------------------------
   end subroutine transf
!-----------------------------------------------------------------


       subroutine vilka(tn,d1,d2,pp,qs,ts,tt,nstep,ii,j,iland,isoil)
!--------------------------------------------------------------
!--- vilka finds the solution of energy budget at the surface
!--- using table t,qs computed from clausius-klapeiron
!--------------------------------------------------------------
   real,     dimension(1:5001),  intent(in   )   ::  tt
   real,     intent(in  )   ::  tn,d1,d2,pp
   integer,  intent(in  )   ::  nstep,ii,j,iland,isoil

   real,     intent(out  )  ::  qs, ts

   real    ::  f1,t1,t2,rn
   integer ::  i,i1
     
       i=(tn-1.7315e2)/.05+1
       t1=173.1+float(i)*.05
       f1=t1+d1*tt(i)-d2
       i1=i-f1/(.05+d1*(tt(i+1)-tt(i)))
       i=i1
       if(i.gt.5000.or.i.lt.1) goto 1
  10   i1=i
       t1=173.1+float(i)*.05
       f1=t1+d1*tt(i)-d2
       rn=f1/(.05+d1*(tt(i+1)-tt(i)))
       i=i-int(rn)                      
       if(i.gt.5000.or.i.lt.1) goto 1
       if(i1.ne.i) goto 10
       ts=t1-.05*rn
       qs=(tt(i)+(tt(i)-tt(i+1))*rn)/pp
       goto 20
!   1   print *,'crash in surface energy budget - stop'
   1   print *,'     avost in vilka     table index= ',i
!       print *,tn,d1,d2,pp,nstep,i,tt(i),ii,j,iland,isoil
       print *,'i,j=',ii,j,'lu_index = ',iland, 'psfc[hpa] = ',pp, 'tsfc = ',tn
       call wrf_error_fatal ('  crash in surface energy budget  ' )
   20  continue
!-----------------------------------------------------------------------
   end subroutine vilka
!-----------------------------------------------------------------------

     subroutine soilvegin  ( mosaic_lu,mosaic_soil,soilfrac,nscat,   &
                     shdmin, shdmax,                                 &
                     nlcat,ivgtyp,isltyp,iswater,myj,                &
                     iforest,lufrac,vegfrac,emiss,pc,znt,lai,rdlai2d,&
                     qwrtz,rhocs,bclh,dqm,ksat,psis,qmin,ref,wilt,i,j)

!************************************************************************
!  set-up soil and vegetation parameters in the case when
!  snow disappears during the forecast and snow parameters
!  shold be replaced by surface parameters according to
!  soil and vegetation types in this point.
!
!        output:
!
!
!             soil parameters:
!               dqm: max soil moisture content - min (m^3/m^3)
!               ref:        reference soil moisture (m^3/m^3)
!               wilt: wilting pt soil moisture contents (m^3/m^3)
!               qmin: air dry soil moist content limits (m^3/m^3)
!               psis: sat soil potential coefs. (m)
!               ksat:  sat soil diffusivity/conductivity coefs. (m/s)
!               bclh: soil diffusivity/conductivity exponent.
!
! ************************************************************************
   implicit none
!---------------------------------------------------------------------------
      integer,   parameter      ::      nsoilclas=19
      integer,   parameter      ::      nvegclas=24+3
      integer,   parameter      ::      ilsnow=99

   integer,    intent(in   )    ::      nlcat, nscat, iswater, i, j

!---    soiltyp classification according to statsgo(nclasses=16)
!
!             1          sand                  sand
!             2          loamy sand            loamy sand
!             3          sandy loam            sandy loam
!             4          silt loam             silty loam
!             5          silt                  silty loam
!             6          loam                  loam
!             7          sandy clay loam       sandy clay loam
!             8          silty clay loam       silty clay loam
!             9          clay loam             clay loam
!            10          sandy clay            sandy clay
!            11          silty clay            silty clay
!            12          clay                  light clay
!            13          organic materials     loam
!            14          water
!            15          bedrock
!                        bedrock is reclassified as class 14
!            16          other (land-ice)
!            17          playa
!            18          lava
!            19          white sand
!
!----------------------------------------------------------------------
         real  lqma(nsoilclas),lrhc(nsoilclas),                       &
               lpsi(nsoilclas),lqmi(nsoilclas),                       &
               lbcl(nsoilclas),lkas(nsoilclas),                       &
               lwil(nsoilclas),lref(nsoilclas),                       &
               datqtz(nsoilclas)
!-- lqma rawls et al.[1982]
!        data lqma /0.417, 0.437, 0.453, 0.501, 0.486, 0.463, 0.398,
!     &  0.471, 0.464, 0.430, 0.479, 0.475, 0.439, 1.0, 0.20, 0.401/
!---
!-- clapp, r. and g. hornberger, 1978: empirical equations for some soil
!   hydraulic properties, water resour. res., 14, 601-604.

!-- clapp et al. [1978]
     data lqma /0.395, 0.410, 0.435, 0.485, 0.485, 0.451, 0.420,      &
                0.477, 0.476, 0.426, 0.492, 0.482, 0.451, 1.0,        &
                0.20,  0.435, 0.468, 0.200, 0.339/

!-- lref rawls et al.[1982]
!        data lref /0.091, 0.125, 0.207, 0.330, 0.360, 0.270, 0.255,
!     &  0.366, 0.318, 0.339, 0.387, 0.396, 0.329, 1.0, 0.108, 0.283/

!-- clapp et al. [1978]
        data lref /0.174, 0.179, 0.249, 0.369, 0.369, 0.314, 0.299,   &
                   0.357, 0.391, 0.316, 0.409, 0.400, 0.314, 1.,      &
                   0.1,   0.249, 0.454, 0.17,  0.236/

!-- lwil rawls et al.[1982]
!        data lwil/0.033, 0.055, 0.095, 0.133, 0.133, 0.117, 0.148,
!     &  0.208, 0.197, 0.239, 0.250, 0.272, 0.066, 0.0, 0.006, 0.029/

!-- clapp et al. [1978]
        data lwil/0.068, 0.075, 0.114, 0.179, 0.179, 0.155, 0.175,    &
                  0.218, 0.250, 0.219, 0.283, 0.286, 0.155, 0.0,      &
                  0.006, 0.114, 0.030, 0.006, 0.01/

!        data lqmi/0.010, 0.028, 0.047, 0.084, 0.084, 0.066, 0.067,
!     &  0.120, 0.103, 0.100, 0.126, 0.138, 0.066, 0.0, 0.006, 0.028/

!-- carsel and parrish [1988]
        data lqmi/0.045, 0.057, 0.065, 0.067, 0.034, 0.078, 0.10,     &
                  0.089, 0.095, 0.10,  0.070, 0.068, 0.078, 0.0,      &
                  0.004, 0.065, 0.020, 0.004, 0.008/

!-- lpsi cosby et al[1984]
!        data lpsi/0.060, 0.036, 0.141, 0.759, 0.759, 0.355, 0.135,
!     &  0.617, 0.263, 0.098, 0.324, 0.468, 0.355, 0.0, 0.069, 0.036/
!     &  0.617, 0.263, 0.098, 0.324, 0.468, 0.355, 0.0, 0.069, 0.036/

!-- clapp et al. [1978]
       data lpsi/0.121, 0.090, 0.218, 0.786, 0.786, 0.478, 0.299,     &
                 0.356, 0.630, 0.153, 0.490, 0.405, 0.478, 0.0,       &
                 0.121, 0.218, 0.468, 0.069, 0.069/

!-- lkas rawls et al.[1982]
!        data lkas/5.83e-5, 1.70e-5, 7.19e-6, 1.89e-6, 1.89e-6,
!     &  3.67e-6, 1.19e-6, 4.17e-7, 6.39e-7, 3.33e-7, 2.50e-7,
!     &  1.67e-7, 3.38e-6, 0.0, 1.41e-4, 1.41e-5/

!-- clapp et al. [1978]
        data lkas/1.76e-4, 1.56e-4, 3.47e-5, 7.20e-6, 7.20e-6,         &
                  6.95e-6, 6.30e-6, 1.70e-6, 2.45e-6, 2.17e-6,         &
                  1.03e-6, 1.28e-6, 6.95e-6, 0.0,     1.41e-4,         &
                  3.47e-5, 1.28e-6, 1.41e-4, 1.76e-4/

!-- lbcl cosby et al [1984]
!        data lbcl/2.79,  4.26,  4.74,  5.33,  5.33,  5.25,  6.66,
!     &  8.72,  8.17,  10.73, 10.39, 11.55, 5.25,  0.0, 2.79,  4.26/

!-- clapp et al. [1978]
        data lbcl/4.05,  4.38,  4.90,  5.30,  5.30,  5.39,  7.12,      &
                  7.75,  8.52, 10.40, 10.40, 11.40,  5.39,  0.0,       &
                  4.05,  4.90, 11.55,  2.79,  2.79/

        data lrhc /1.47,1.41,1.34,1.27,1.27,1.21,1.18,1.32,1.23,       &
                   1.18,1.15,1.09,1.21,4.18,2.03,2.10,1.09,2.03,1.47/

        data datqtz/0.92,0.82,0.60,0.25,0.10,0.40,0.60,0.10,0.35,      &
                    0.52,0.10,0.25,0.00,0.,0.60,0.0,0.25,0.60,0.92/

!--------------------------------------------------------------------------
!
!     usgs vegetation types
!
!    1:   urban and built-up land
!    2:   dryland cropland and pasture
!    3:   irrigated cropland and pasture
!    4:   mixed dryland/irrigated cropland and pasture
!    5:   cropland/grassland mosaic
!    6:   cropland/woodland mosaic
!    7:   grassland
!    8:   shrubland
!    9:   mixed shrubland/grassland
!   10:   savanna
!   11:   deciduous broadleaf forest
!   12:   deciduous needleleaf forest
!   13:   evergreen broadleaf forest
!   14:   evergreen needleleaf fores
!   15:   mixed forest
!   16:   water bodies
!   17:   herbaceous wetland
!   18:   wooded wetland
!   19:   barren or sparsely vegetated
!   20:   herbaceous tundra
!   21:   wooded tundra
!   22:   mixed tundra
!   23:   bare ground tundra
!   24:   snow or ice
!
!   25:   playa
!   26:   lava
!   27:   white sand

! modis vegetation categories from VEGPARM.TBL
!    1:   evergreen needleleaf forest
!    2:   evergreen broadleaf forest
!    3:   deciduous needleleaf forest
!    4:   deciduous broadleaf forest
!    5:   mixed forests
!    6:   closed shrublands
!    7:   open shrublands
!    8:   woody savannas
!    9:   savannas
!   10:   grasslands
!   11:   permanent wetlands
!   12:   croplands
!   13:   urban and built-up
!   14:   cropland/natural vegetation mosaic
!   15:   snow and ice
!   16:   barren or sparsely vegetated
!   17:   water
!   18:   wooded tundra
!   19:   mixed tundra
!   20:   barren tundra
!   21:   lakes


!----  below are the arrays for the vegetation parameters
         real lalb(nvegclas),lmoi(nvegclas),lemi(nvegclas),            &
              lrou(nvegclas),lthi(nvegclas),lsig(nvegclas),            &
              lpc(nvegclas)

!************************************************************************
!----     vegetation parameters
!
!-- usgs model
!
        data  lalb/.18,.17,.18,.18,.18,.16,.19,.22,.20,.20,.16,.14,     &
                   .12,.12,.13,.08,.14,.14,.25,.15,.15,.15,.25,.55,     &
                   .30,.16,.60 /
        data lemi/.88,4*.92,.93,.92,.88,.9,.92,.93,.94,                 &
                  .95,.95,.94,.98,.95,.95,.85,.92,.93,.92,.85,.95,      &
                  .85,.85,.90 /
!-- roughness length is changed for forests and some others
!        data lrou/.5,.06,.075,.065,.05,.2,.075,.1,.11,.15,.8,.85,       &
!                  2.0,1.0,.563,.0001,.2,.4,.05,.1,.15,.1,.065,.05/
         data lrou/.5,.06,.075,.065,.05,.2,.075,.1,.11,.15,.5,.5,       & 
                   .5,.5,.5,.0001,.2,.4,.05,.1,.15,.1,.065,.05,         &
                   .01,.15,.01 /

        data lmoi/.1,.3,.5,.25,.25,.35,.15,.1,.15,.15,.3,.3,            &
                  .5,.3,.3,1.,.6,.35,.02,.5,.5,.5,.02,.95,.40,.50,.40/
!
!---- still needs to be corrected
!
!       data lpc/ 15*.8,0.,.8,.8,.5,.5,.5,.5,.5,.0/
       data lpc /0.4,0.3,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,5*0.55,0.,0.55,0.55,                   &
                 0.3,0.3,0.4,0.4,0.3,0.,.3,0.,0./

! used in ruc       data lpc /0.6,6*0.8,0.7,0.75,6*0.8,0.,0.8,0.8,                   &
!                 0.5,0.7,0.6,0.7,0.5,0./


!***************************************************************************


   integer      ::                &
                                                         ivgtyp, &
                                                         isltyp
   integer,    intent(in   )    ::     mosaic_lu, mosaic_soil

   logical,    intent(in   )    ::     myj
   real,       intent(in )      ::   shdmax
   real,       intent(in )      ::   shdmin
   real,       intent(in )      ::   vegfrac
   real,     dimension( 1:nlcat ),  intent(in)::         lufrac
   real,     dimension( 1:nscat ),  intent(in)::         soilfrac

   real                                                        , &
            intent (  out)            ::                     pc

   real                                                        , &
            intent (inout   )         ::                  emiss, &
                                                            lai, &
                                                            znt
  logical, intent(in) :: rdlai2d
!--- soil properties
   real                                                        , &
            intent(  out)    ::                           rhocs, &
                                                           bclh, &
                                                            dqm, &
                                                           ksat, &
                                                           psis, &
                                                           qmin, &
                                                          qwrtz, &
                                                            ref, &
                                                           wilt
   integer, intent (  out)   ::                         iforest

!   integer, dimension( 1:(lucats) )                          , &
!            intent (  out)            ::                iforest


!   integer, dimension( 1:50 )   ::   if1
   integer   ::   kstart, kfin, lstart, lfin
   integer   ::   k
   real      ::   area,  factor, znt1, lb
   real,     dimension( 1:nlcat ) :: znttoday, laitoday, deltalai

!***********************************************************************
!        data zs1/0.0,0.05,0.20,0.40,1.6,3.0/   ! o -  levels in soil
!        data zs2/0.0,0.025,0.125,0.30,1.,2.3/   ! x - levels in soil

!        data if1/12*0,1,1,1,12*0/

!          do k=1,lucats
!             iforest(k)=if1(k)
!          enddo

        iforest = ifortbl(ivgtyp)

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      if(i.eq.375.and.j.eq.254)then
        print *,'ifortbl(ivgtyp),ivgtyp,laitbl(ivgtyp),z0tbl(ivgtyp)', &
            ifortbl(ivgtyp),ivgtyp,laitbl(ivgtyp),z0tbl(ivgtyp)
      endif
    endif

        deltalai(:) = 0.

! 11oct2012 - seasonal correction on znt for crops and lai for all veg. types
! factor = 1 with minimum greenness -->  vegfrac = shdmin (cold season)
! factor = 0 with maximum greenness -->  vegfrac = shdmax
! shdmax, shdmin and vegfrac are in % here.
      if((shdmax - shdmin) .lt. 1) then
        factor = 1. ! min greenness
      else
        factor = 1. - max(0.,min(1.,(vegfrac - shdmin)/max(1.,(shdmax-shdmin))))
      endif

! 18sept18 - laitbl and z0tbl are the max values
      do k = 1,nlcat
       if(ifortbl(k) == 1) deltalai(k)=min(0.2,0.8*laitbl(k))
       if(ifortbl(k) == 2 .or. ifortbl(k) == 7) deltalai(k)=min(0.5,0.8*laitbl(k))
       if(ifortbl(k) == 3) deltalai(k)=min(0.45,0.8*laitbl(k))
       if(ifortbl(k) == 4) deltalai(k)=min(0.75,0.8*laitbl(k))
       if(ifortbl(k) == 5) deltalai(k)=min(0.86,0.8*laitbl(k))

       if(k.ne.iswater) then
!-- 20aug18 - change in laitoday based on the greenness fraction for the current day
        laitoday(k) = laitbl(k) - deltalai(k) * factor

         if(ifortbl(k) == 7) then
!-- seasonal change of roughness length for crops
           znttoday(k) = z0tbl(k) - 0.125 * factor
         else
           znttoday(k) = z0tbl(k)
         endif
       else
        laitoday(k) = laitbl(k)
        znttoday(k) = znt ! do not overwrite z0 over water with the table value
       endif
      enddo

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      if(i.eq.358.and.j.eq.260)then
        print *,'ivgtyp,factor,vegfrac,shdmin,shdmax,deltalai,laitoday(ivgtyp),znttoday(ivgtyp)', &
         i,j,ivgtyp,factor,vegfrac,shdmin,shdmax,deltalai,laitoday(ivgtyp),znttoday(ivgtyp)
      endif
    endif

        emiss = 0.
        znt   = 0.
        znt1  = 0.
        pc    = 0.
        if(.not.rdlai2d) lai = 0.
        area  = 0.
!-- mosaic approach to landuse in the grid box
! use  mason (1988) eq.(15) to compute effective znt;
!  lb - blending height =  l/200., where l is the length scale of regions with varying z0 (lb = 5 if l=1000 m)
        lb = 5.
      if(mosaic_lu == 1) then
      do k = 1,nlcat
        area  = area + lufrac(k)
        emiss = emiss+ lemitbl(k)*lufrac(k)
        znt   = znt  + lufrac(k)/alog(lb/znttoday(k))**2.
! znt1 - weighted average in the grid box, not used, computed for comparison
        znt1  = znt1 + lufrac(k)*znttoday(k)
        if(.not.rdlai2d) lai = lai  + laitoday(k)*lufrac(k)
        pc    = pc   + pctbl(k)*lufrac(k)
      enddo

       if (area.gt.1.) area=1.
       if (area <= 0.) then
          print *,'bad area of grid box', area
          stop
       endif

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      if(i.eq.358.and.j.eq.260) then
        print *,'area=',area,i,j,ivgtyp,nlcat,(lufrac(k),k=1,nlcat),emiss,znt,znt1,lai,pc
      endif
    endif

        emiss = emiss/area
        znt1   = znt1/area
        znt = lb/exp(sqrt(1./znt))
        if(.not.rdlai2d) lai = lai/area
        pc    = pc /area

    if ( wrf_at_debug_level(lsmruc_dbg_lvl) ) then
      if(i.eq.358.and.j.eq.260) then
        print *,'mosaic=',i,j,ivgtyp,nlcat,(lufrac(k),k=1,nlcat),emiss,znt,znt1,lai,pc
      endif
    endif


      else
        emiss = lemitbl(ivgtyp)
        znt   = znttoday(ivgtyp)
        pc    = pctbl(ivgtyp)
        if(.not.rdlai2d) lai = laitoday(ivgtyp)
     endif

! parameters from soilparm.tbl
          rhocs  = 0.
          bclh   = 0.
          dqm    = 0.
          ksat   = 0.
          psis   = 0.
          qmin   = 0.
          ref    = 0.
          wilt   = 0.
          qwrtz  = 0.
          area   = 0.
! mosaic approach
       if(mosaic_soil == 1 ) then
            do k = 1, nscat
        if(k.ne.14) then  ! statsgo value for water
!exclude watrer points from this loop
          area   = area + soilfrac(k)
          rhocs  = rhocs + hc(k)*1.e6*soilfrac(k)
          bclh   = bclh + bb(k)*soilfrac(k)
          dqm    = dqm + (maxsmc(k)-                               &
                   drysmc(k))*soilfrac(k)
          ksat   = ksat + satdk(k)*soilfrac(k)
          psis   = psis - satpsi(k)*soilfrac(k)
          qmin   = qmin + drysmc(k)*soilfrac(k)
          ref    = ref + refsmc(k)*soilfrac(k)
          wilt   = wilt + wltsmc(k)*soilfrac(k)
          qwrtz  = qwrtz + qtz(k)*soilfrac(k)
        endif
            enddo
       if (area.gt.1.) area=1.
       if (area <= 0.) then
! area = 0. for water points
!          print *,'area of a grid box', area, 'iswater = ',iswater
          rhocs  = hc(isltyp)*1.e6
          bclh   = bb(isltyp)
          dqm    = maxsmc(isltyp)-                               &
                   drysmc(isltyp)
          ksat   = satdk(isltyp)
          psis   = - satpsi(isltyp)
          qmin   = drysmc(isltyp)
          ref    = refsmc(isltyp)
          wilt   = wltsmc(isltyp)
          qwrtz  = qtz(isltyp)
       else
          rhocs  = rhocs/area
          bclh   = bclh/area
          dqm    = dqm/area
          ksat   = ksat/area
          psis   = psis/area
          qmin   = qmin/area
          ref    = ref/area
          wilt   = wilt/area
          qwrtz  = qwrtz/area
       endif

! dominant category approach
        else
      if(isltyp.ne.14) then
          rhocs  = hc(isltyp)*1.e6
          bclh   = bb(isltyp)
          dqm    = maxsmc(isltyp)-                               &
                   drysmc(isltyp)
          ksat   = satdk(isltyp)
          psis   = - satpsi(isltyp)
          qmin   = drysmc(isltyp)
          ref    = refsmc(isltyp)
          wilt   = wltsmc(isltyp)
          qwrtz  = qtz(isltyp)
        endif
        endif

!--------------------------------------------------------------------------
   end subroutine soilvegin
!--------------------------------------------------------------------------

  subroutine ruclsminit( sh2o,smfr3d,tslb,smois,isltyp,ivgtyp,     &
                     mminlu, xice,mavail,nzs, iswater, isice,      &
                     znt, restart, allowed_to_read ,               &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )
#if ( wrf_chem == 1 )
  use module_data_gocart_dust
#endif
   implicit none


   integer,  intent(in   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte,  &
                                    nzs, iswater, isice
   character(len=*), intent(in   )    ::                 mminlu

   real, dimension( ims:ime, 1:nzs, jms:jme )                    , &
            intent(in)    ::                                 tslb, &
                                                            smois

   integer, dimension( ims:ime, jms:jme )                        , &
            intent(inout)    ::                     isltyp,ivgtyp

   real, dimension( ims:ime, 1:nzs, jms:jme )                    , &
            intent(inout)    ::                            smfr3d, &
                                                             sh2o

   real, dimension( ims:ime, jms:jme )                           , &
            intent(inout)    ::                       xice,mavail

   real, dimension( ims:ime, jms:jme )                           , &
            intent(  out)    ::                               znt

   real, dimension ( 1:nzs )  ::                           soiliqw

   logical , intent(in) :: restart, allowed_to_read 

!
  integer ::  i,j,l,itf,jtf
  real    ::  riw,xlmelt,tln,dqm,ref,psis,qmin,bclh

  character*8 :: mminluruc, mminsl

   integer                   :: errflag

        riw=900.*1.e-3
        xlmelt=3.35e+5

! initialize three  lsm related tables
   if ( allowed_to_read ) then
     call wrf_message( 'initialize three lsm related tables' )
      if(mminlu == 'USGS') then
        mminluruc='USGS-RUC'
      elseif(mminlu == 'MODIS' .or. &
        &    mminlu == 'MODIFIED_IGBP_MODIS_NOAH') then
        mminluruc='MODI-RUC'
      endif
        mminsl='STAS-RUC'
    print *,'ruclsminit uses ',mminluruc
     call ruclsm_soilvegparm( mminluruc, mminsl)   
   endif

!#if ( wrf_chem == 1 )
!
! need this parameter for dust parameterization in wrf/chem
!
!   do i=1,nsltype
!      porosity(i)=maxsmc(i)
!      drypoint(i)=drysmc(i)
!   enddo
!#endif
!
 if(.not.restart)then

   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   errflag = 0
   do j = jts,jtf
     do i = its,itf
       if ( isltyp( i,j ) .lt. 1 ) then
         errflag = 1
         write(err_message,*)"module_sf_ruclsm.f: lsminit: out of range isltyp ",i,j,isltyp( i,j )
         call wrf_message(err_message)
       endif
     enddo
   enddo
   if ( errflag .eq. 1 ) then
      call wrf_error_fatal( "module_sf_ruclsm.f: lsminit: out of range value "// &
                            "of isltyp. is this field in the input?" )
   endif

   do j=jts,jtf
       do i=its,itf

        znt(i,j)   = z0tbl(ivgtyp(i,j))

!--- computation of volumetric content of ice in soil
!--- and initialize mavail
          dqm    = maxsmc   (isltyp(i,j)) -                               &
                   drysmc   (isltyp(i,j))
          ref    = refsmc   (isltyp(i,j))
          psis   = - satpsi (isltyp(i,j))
          qmin   = drysmc   (isltyp(i,j))
          bclh   = bb       (isltyp(i,j))


    if(xice(i,j).gt.0.) then
!-- for ice
         do l=1,nzs
           smfr3d(i,l,j)=1.
           sh2o(i,l,j)=0.
           mavail(i,j) = 1.
         enddo
    else
       if(isltyp(i,j).ne.14 ) then
!-- land
           mavail(i,j) = max(0.00001,min(1.,(smois(i,1,j)-qmin)/(ref-qmin)))
         do l=1,nzs
!-- for land points initialize soil ice
         tln=log(tslb(i,l,j)/273.15)
          
          if(tln.lt.0.) then
           soiliqw(l)=(dqm+qmin)*(xlmelt*                        &
         (tslb(i,l,j)-273.15)/tslb(i,l,j)/9.81/psis)             &
          **(-1./bclh)
           soiliqw(l)=max(0.,soiliqw(l))
           soiliqw(l)=min(soiliqw(l),smois(i,l,j))
           sh2o(i,l,j)=soiliqw(l)
           smfr3d(i,l,j)=(smois(i,l,j)-soiliqw(l))/riw
         
          else
           smfr3d(i,l,j)=0.
           sh2o(i,l,j)=smois(i,l,j)
          endif
         enddo
    
       else
!-- for water  isltyp=14
         do l=1,nzs
           smfr3d(i,l,j)=0.
           sh2o(i,l,j)=1.
           mavail(i,j) = 1.
         enddo
       endif
    endif

    enddo
   enddo

 endif

!-----------------------------------------------------------------
  end subroutine ruclsminit
!-----------------------------------------------------------------
!
        subroutine ruclsm_soilvegparm( mminluruc, mminsl)
!-----------------------------------------------------------------

        implicit none

        integer :: LUMATCH, IINDEX, LC, NUM_SLOPE
        integer :: ierr
        INTEGER , PARAMETER :: OPEN_OK = 0

        character*8 :: MMINLURUC, MMINSL
        character*128 :: mess , message, vege_parm_string
        logical, external :: wrf_dm_on_monitor


!-----specify vegetation related characteristics :
!             albbck: sfc albedo (in percentage)
!                 z0: roughness length (m)
!               lemi: emissivity
!                 pc: plant coefficient for transpiration function
! -- the rest of the parameters are read in but not used currently
!             shdfac: green vegetation fraction (in percentage)
!  note: the albedo, z0, and shdfac values read from the following table
!          albedo, amd z0 are specified in land-use table; and shdfac is
!          the monthly green vegetation data
!             cmxtbl: max cnpy capacity (m)
!              rsmin: mimimum stomatal resistance (s m-1)
!              rsmax: max. stomatal resistance (s m-1)
!                rgl: parameters used in radiation stress function
!                 hs: parameter used in vapor pressure deficit functio
!               topt: optimum transpiration air temperature. (k)
!             cmcmax: maximum canopy water capacity
!             cfactr: parameter used in the canopy inteception calculati
!               snup: threshold snow depth (in water equivalent m) that
!                     implies 100% snow cover
!                lai: leaf area index (dimensionless)
!             maxalb: upper bound on maximum albedo over deep snow
!
!-----read in vegetaion properties from VEGPARM.TBL 
!                                                                       

       IF ( wrf_dm_on_monitor() ) THEN

        OPEN(19, FILE='VEGPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF(ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_sf_ruclsm.F: soil_veg_gen_parm: failure opening VEGPARM.TBL'
          CALL wrf_error_fatal ( message )
        END IF

        WRITE ( mess, * ) 'INPUT VEGPARM FOR ',MMINLURUC
        CALL wrf_message( mess )

        LUMATCH=0

 2000   FORMAT (A8)
        READ (19,'(A)') vege_parm_string
        outer : DO
           READ (19,2000,END=2002)LUTYPE
           READ (19,*)LUCATS,IINDEX

           WRITE( mess , * ) 'VEGPARM FOR ',LUTYPE,' FOUND', LUCATS,' CATEGORIES'
           CALL wrf_message( mess )

           IF(LUTYPE.NE.MMINLURUC)THEN    ! Skip over the undesired table
              write ( mess , * ) 'Skipping ', LUTYPE, ' table'
              CALL wrf_message( mess )
              DO LC=1,LUCATS
                 READ (19,*)
              ENDDO
              inner : DO               ! Find the next "Vegetation Parameters"
                 READ (19,'(A)',END=2002) vege_parm_string
                 IF (TRIM(vege_parm_string) .EQ. "Vegetation Parameters") THEN
                    EXIT inner
                 END IF
               ENDDO inner
           ELSE
              LUMATCH=1
              write ( mess , * ) 'Found ', LUTYPE, ' table'
              CALL wrf_message( mess )
              EXIT outer                ! Found the table, read the data
           END IF

        ENDDO outer

        IF (LUMATCH == 1) then
           write ( mess , * ) 'Reading ',LUTYPE,' table'
           CALL wrf_message( mess )
           DO LC=1,LUCATS
              READ (19,*)IINDEX,ALBTBL(LC),Z0TBL(LC),LEMITBL(LC),PCTBL(LC), &
                         SHDTBL(LC),IFORTBL(LC),RSTBL(LC),RGLTBL(LC),         &
                         HSTBL(LC),SNUPTBL(LC),LAITBL(LC),MAXALB(LC)
           ENDDO
!
           READ (19,*)
           READ (19,*)TOPT_DATA
           READ (19,*)
           READ (19,*)CMCMAX_DATA
           READ (19,*)
           READ (19,*)CFACTR_DATA
           READ (19,*)
           READ (19,*)RSMAX_DATA
           READ (19,*)
           READ (19,*)BARE
           READ (19,*)
           READ (19,*)NATURAL
           READ (19,*)
           READ (19,*)CROP
           READ (19,*)
           READ (19,*,iostat=ierr)URBAN
           if ( ierr /= 0 ) call wrf_message     (  "-------- VEGPARM.TBL READ ERROR --------")
           if ( ierr /= 0 ) call wrf_message     (  "Problem read URBAN from VEGPARM.TBL")
           if ( ierr /= 0 ) call wrf_message     (  " -- Use updated version of VEGPARM.TBL  ")
           if ( ierr /= 0 ) call wrf_error_fatal (  "Problem read URBAN from VEGPARM.TBL")

        ENDIF

 2002   CONTINUE
        CLOSE (19)
!-----
    IF ( wrf_at_debug_level(lsmruc_dbg_lvl) ) THEN
         print *,' LEMITBL, PCTBL, Z0TBL, LAITBL --->', LEMITBL, PCTBL, Z0TBL, LAITBL
    ENDIF

        IF (LUMATCH == 0) then
           CALL wrf_error_fatal ("Land Use Dataset '"//MMINLURUC//"' not found in VEGPARM.TBL.")
        ENDIF

      END IF

      CALL wrf_dm_bcast_string  ( LUTYPE  , 8 )
      CALL wrf_dm_bcast_integer ( LUCATS  , 1 )
      CALL wrf_dm_bcast_integer ( IINDEX  , 1 )
      CALL wrf_dm_bcast_integer ( LUMATCH , 1 )
      CALL wrf_dm_bcast_real    ( ALBTBL  , NLUS )
      CALL wrf_dm_bcast_real    ( Z0TBL   , NLUS )
      CALL wrf_dm_bcast_real    ( LEMITBL , NLUS )
      CALL wrf_dm_bcast_real    ( PCTBL   , NLUS )
      CALL wrf_dm_bcast_real    ( SHDTBL  , NLUS )
      CALL wrf_dm_bcast_real    ( IFORTBL , NLUS )
      CALL wrf_dm_bcast_real    ( RSTBL   , NLUS )
      CALL wrf_dm_bcast_real    ( RGLTBL  , NLUS )
      CALL wrf_dm_bcast_real    ( HSTBL   , NLUS )
      CALL wrf_dm_bcast_real    ( SNUPTBL , NLUS )
      CALL wrf_dm_bcast_real    ( LAITBL  , NLUS )
      CALL wrf_dm_bcast_real    ( MAXALB  , NLUS )
      CALL wrf_dm_bcast_real    ( TOPT_DATA    , 1 )
      CALL wrf_dm_bcast_real    ( CMCMAX_DATA  , 1 )
      CALL wrf_dm_bcast_real    ( CFACTR_DATA  , 1 )
      CALL wrf_dm_bcast_real    ( RSMAX_DATA  , 1 )
      CALL wrf_dm_bcast_integer ( BARE        , 1 )
      CALL wrf_dm_bcast_integer ( NATURAL     , 1 )
      CALL wrf_dm_bcast_integer ( CROP        , 1 )
      CALL wrf_dm_bcast_integer ( URBAN       , 1 )

!
!-----READ IN SOIL PROPERTIES FROM SOILPARM.TBL
!
      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(19, FILE='SOILPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF(ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_sf_ruclsm.F: soil_veg_gen_parm: failure opening SOILPARM.TBL'
          CALL wrf_error_fatal ( message )
        END IF

        WRITE(mess,*) 'INPUT SOIL TEXTURE CLASSIFICATION = ',MMINSL
        CALL wrf_message( mess )

        LUMATCH=0

        READ (19,*)
        READ (19,2000,END=2003)SLTYPE
        READ (19,*)SLCATS,IINDEX
        IF(SLTYPE.NE.MMINSL)THEN
          DO LC=1,SLCATS
              READ (19,*) IINDEX,BB(LC),DRYSMC(LC),HC(LC),MAXSMC(LC),&
                        REFSMC(LC),SATPSI(LC),SATDK(LC), SATDW(LC),   &
                        WLTSMC(LC), QTZ(LC)
          ENDDO
        ENDIF
        READ (19,*)
        READ (19,2000,END=2003)SLTYPE
        READ (19,*)SLCATS,IINDEX

        IF(SLTYPE.EQ.MMINSL)THEN
            WRITE( mess , * ) 'SOIL TEXTURE CLASSIFICATION = ',SLTYPE,' FOUND', &
                  SLCATS,' CATEGORIES'
            CALL wrf_message ( mess )
          LUMATCH=1
        ENDIF
            IF(SLTYPE.EQ.MMINSL)THEN
          DO LC=1,SLCATS
              READ (19,*) IINDEX,BB(LC),DRYSMC(LC),HC(LC),MAXSMC(LC),&
                        REFSMC(LC),SATPSI(LC),SATDK(LC), SATDW(LC),   &
                        WLTSMC(LC), QTZ(LC)
          ENDDO
           ENDIF

 2003   CONTINUE

        CLOSE (19)
      ENDIF

      CALL wrf_dm_bcast_integer ( LUMATCH , 1 )
      CALL wrf_dm_bcast_string  ( SLTYPE  , 8 )
      CALL wrf_dm_bcast_string  ( MMINSL  , 8 )  ! since this is reset above, see oct2 ^
      CALL wrf_dm_bcast_integer ( SLCATS  , 1 )
      CALL wrf_dm_bcast_integer ( IINDEX  , 1 )
      CALL wrf_dm_bcast_real    ( BB      , NSLTYPE )
      CALL wrf_dm_bcast_real    ( DRYSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( HC      , NSLTYPE )
      CALL wrf_dm_bcast_real    ( MAXSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( REFSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( SATPSI  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( SATDK   , NSLTYPE )
      CALL wrf_dm_bcast_real    ( SATDW   , NSLTYPE )
      CALL wrf_dm_bcast_real    ( WLTSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( QTZ     , NSLTYPE )

      IF(LUMATCH.EQ.0)THEN
          CALL wrf_message( 'SOIl TEXTURE IN INPUT FILE DOES NOT ' )
          CALL wrf_message( 'MATCH SOILPARM TABLE'                 )
          CALL wrf_error_fatal ( 'INCONSISTENT OR MISSING SOILPARM FILE' )
      ENDIF
!
!-----READ IN GENERAL PARAMETERS FROM GENPARM.TBL
!
      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(19, FILE='GENPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF(ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_sf_ruclsm.F: soil_veg_gen_parm: failure opening GENPARM.TBL'
          CALL wrf_error_fatal ( message )
        END IF

        READ (19,*)
        READ (19,*)
        READ (19,*) NUM_SLOPE

          SLPCATS=NUM_SLOPE

          DO LC=1,SLPCATS
              READ (19,*)SLOPE_DATA(LC)
          ENDDO

          READ (19,*)
          READ (19,*)SBETA_DATA
          READ (19,*)
          READ (19,*)FXEXP_DATA
          READ (19,*)
          READ (19,*)CSOIL_DATA
          READ (19,*)
          READ (19,*)SALP_DATA
          READ (19,*)
          READ (19,*)REFDK_DATA
          READ (19,*)
          READ (19,*)REFKDT_DATA
          READ (19,*)
          READ (19,*)FRZK_DATA
          READ (19,*)
          READ (19,*)ZBOT_DATA
          READ (19,*)
          READ (19,*)CZIL_DATA
          READ (19,*)
          READ (19,*)SMLOW_DATA
          READ (19,*)
          READ (19,*)SMHIGH_DATA
        CLOSE (19)
      ENDIF

      CALL wrf_dm_bcast_integer ( NUM_SLOPE    ,  1 )
      CALL wrf_dm_bcast_integer ( SLPCATS      ,  1 )
      CALL wrf_dm_bcast_real    ( SLOPE_DATA   ,  NSLOPE )
      CALL wrf_dm_bcast_real    ( SBETA_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( FXEXP_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( CSOIL_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( SALP_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( REFDK_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( REFKDT_DATA  ,  1 )
      CALL wrf_dm_bcast_real    ( FRZK_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( ZBOT_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( CZIL_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( SMLOW_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( SMHIGH_DATA  ,  1 )


!-----------------------------------------------------------------
      end subroutine ruclsm_soilvegparm
!-----------------------------------------------------------------


  subroutine soilin (isltyp, dqm, ref, psis, qmin, bclh )

!---    soiltyp classification according to statsgo(nclasses=16)
!
!             1          sand                  sand
!             2          loamy sand            loamy sand
!             3          sandy loam            sandy loam
!             4          silt loam             silty loam
!             5          silt                  silty loam
!             6          loam                  loam
!             7          sandy clay loam       sandy clay loam
!             8          silty clay loam       silty clay loam
!             9          clay loam             clay loam
!            10          sandy clay            sandy clay
!            11          silty clay            silty clay
!            12          clay                  light clay
!            13          organic materials     loam
!            14          water
!            15          bedrock
!                        bedrock is reclassified as class 14
!            16          other (land-ice)
! extra classes from fei chen
!            17          playa
!            18          lava
!            19          white sand
!
!----------------------------------------------------------------------
         integer,   parameter      ::      nsoilclas=19

         integer, intent ( in)  ::                          isltyp
         real,    intent ( out) ::               dqm,ref,qmin,psis

         real  lqma(nsoilclas),lref(nsoilclas),lbcl(nsoilclas),       &
               lpsi(nsoilclas),lqmi(nsoilclas)

!-- lqma rawls et al.[1982]
!        data lqma /0.417, 0.437, 0.453, 0.501, 0.486, 0.463, 0.398,
!     &  0.471, 0.464, 0.430, 0.479, 0.475, 0.439, 1.0, 0.20, 0.401/
!---
!-- clapp, r. and g. hornberger, empirical equations for some soil
!   hydraulic properties, water resour. res., 14,601-604,1978.
!-- clapp et al. [1978]
     data lqma /0.395, 0.410, 0.435, 0.485, 0.485, 0.451, 0.420,      &
                0.477, 0.476, 0.426, 0.492, 0.482, 0.451, 1.0,        &
                0.20,  0.435, 0.468, 0.200, 0.339/

!-- clapp et al. [1978]
        data lref /0.174, 0.179, 0.249, 0.369, 0.369, 0.314, 0.299,   &
                   0.357, 0.391, 0.316, 0.409, 0.400, 0.314, 1.,      &
                   0.1,   0.249, 0.454, 0.17,  0.236/

!-- carsel and parrish [1988]
        data lqmi/0.045, 0.057, 0.065, 0.067, 0.034, 0.078, 0.10,     &
                  0.089, 0.095, 0.10,  0.070, 0.068, 0.078, 0.0,      &
                  0.004, 0.065, 0.020, 0.004, 0.008/

!-- clapp et al. [1978]
       data lpsi/0.121, 0.090, 0.218, 0.786, 0.786, 0.478, 0.299,     &
                 0.356, 0.630, 0.153, 0.490, 0.405, 0.478, 0.0,       &
                 0.121, 0.218, 0.468, 0.069, 0.069/

!-- clapp et al. [1978]
        data lbcl/4.05,  4.38,  4.90,  5.30,  5.30,  5.39,  7.12,      &
                  7.75,  8.52, 10.40, 10.40, 11.40,  5.39,  0.0,       &
                  4.05,  4.90, 11.55,  2.79,  2.79/


          dqm    = lqma(isltyp)-                               &
                   lqmi(isltyp)
          ref    = lref(isltyp)
          psis   = - lpsi(isltyp)
          qmin   = lqmi(isltyp)
          bclh   = lbcl(isltyp)

  end subroutine soilin

end module module_sf_ruclsm
