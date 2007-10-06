Module module_add_emiss_burn
CONTAINS
       subroutine add_emis_burn(id,dtstep,ktau,dz8w,config_flags,rho_phy,chem,    &
            julday,gmt,xlat,xlong,t_phy,p_phy,                          &
            ebu_no,ebu_co,ebu_co2,ebu_eth,ebu_hc3,ebu_hc5,ebu_hc8,          &
            ebu_ete,ebu_olt,ebu_oli,ebu_pm25,ebu_pm10,ebu_dien,ebu_iso,     &
            ebu_api,ebu_lim,ebu_tol,ebu_xyl,ebu_csl,ebu_hcho,ebu_ald,       &
            ebu_ket,ebu_macr,ebu_ora1,ebu_ora2,                          &
            ids,ide, jds,jde, kds,kde,                                  &
            ims,ime, jms,jme, kms,kme,                                  &
            its,ite, jts,jte, kts,kte                                   )
  USE module_configure
  USE module_state_description
  IMPLICIT NONE


   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) :: id,julday,                               &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte
   INTEGER,      INTENT(IN   ) ::                                          &
                                  ktau
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::                                   chem
!
!
!
   REAL,  DIMENSION( ims:ime ,  jms:jme )         ,               &
          INTENT(IN   ) ::                                                 &
                                                      xlat,xlong
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::                                                 &
                                                      t_phy,               &
                                                      p_phy,               &
                                                      dz8w,                &
                                                    rho_phy
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::                                                 &
          ebu_no,ebu_co,ebu_co2,ebu_eth,ebu_hc3,ebu_hc5,ebu_hc8,          &
           ebu_ete,ebu_olt,ebu_oli,ebu_pm25,ebu_pm10,ebu_dien,ebu_iso,     &
           ebu_api,ebu_lim,ebu_tol,ebu_xyl,ebu_csl,ebu_hcho,ebu_ald,       &
           ebu_ket,ebu_macr,ebu_ora1,ebu_ora2

      REAL,      INTENT(IN   ) ::                                          &
                             dtstep,gmt
    integer ::imonth1,idate1,iyear1,itime1
    integer :: i,j,k
    real :: time,conv_rho
    integer :: iweek,idays
    real :: tign,timeq,r_q,r_antro
    real, dimension(7) :: week_CYCLE
    !                     dia da semana:  DOM   SEG   TER   QUA   QUI   SEX   SAB
    !                            iweek=   1     2     3     4     5     6     7
    !- dados cetesb/campinas/2005
    data (week_CYCLE(iweek),iweek=1,7) /0.67, 1.1, 1.1, 1.1, 1.1, 1.1, 0.83/ !total = 7
    real, parameter :: bx_bburn  = 18.041288 * 3600., & !- peak at 18 UTC
                  cx        =  2.184936 * 3600., &
                  rinti     =  2.1813936e-8    , &
                  ax        = 2000.6038        , &
                  bx_antro  = 15.041288 * 3600.    !- peak em 15 UTC
    !itime1 : initial time of simulation (hour*100)
    ! time  : time elapsed in seconds
    ! r_q : gaussian function in 1/sec

    !-------------biomass burning diurnal cycle --------------------
    !number of days of simulation
    idays = int(( float(itime1)/100. + time/3600.)/24.+.00001)
    tign  = real(idays)*24.*3600.
    ! Modulacao da queimada media durante o ciclo diurno(unidade: 1/s)
    ! com a int( r_q dt) (0 - 24h)= 1.
    timeq= ( time + float(itime1)*0.01*3600. - tign )
    timeq=gmt*3600.+float(ktau)*dtstep
    timeq=mod(timeq,86400.)
    r_q  = rinti*( ax * exp( -(timeq-bx_bburn)**2/(2.*cx**2) ) + 100. -  &
           5.6712963e-4*( timeq ))


    !------------- anthropogenic diurnal cycle (industrial,residencial, ...)
    ! weekly cycle
    ! week day
    iweek= int(((float(julday)/7. - &
           int(julday/7))*7.)) + 1
    if(iweek.gt.7) iweek = iweek-7
    !- weekly + diurnal cycle
    r_q=r_q*86400.

          do j=jts,jte
          do i=its,ite
           do k=kts+1,kte-1
        conv_rho=r_q*4.828e-4/rho_phy(i,k,j)*dtstep/60.
            if(i.eq.56.and.j.eq.132)write(0,*)k,ebu_co(i,k,j),conv_rho,r_q,dtstep
        chem(i,k,j,p_csl)  =  chem(i,k,j,p_csl)                        &
                         +ebu_csl(i,k,j)*conv_rho
        chem(i,k,j,p_iso)  = chem(i,k,j,p_iso)                         &
                         +ebu_iso(i,k,j)*conv_rho
        chem(i,k,j,p_no)   = chem(i,k,j,p_no)                          &
                         +ebu_no(i,k,j)*conv_rho
        chem(i,k,j,p_ald)  = chem(i,k,j,p_ald)                         &
                         +ebu_ald(i,k,j)*conv_rho
        chem(i,k,j,p_hcho) = chem(i,k,j,p_hcho)                        &
                         +ebu_hcho(i,k,j)*conv_rho
        chem(i,k,j,p_ora2)  = chem(i,k,j,p_ora2)                       &
                         +ebu_ora2(i,k,j)*conv_rho
        chem(i,k,j,p_hc3)  = chem(i,k,j,p_hc3)                         &
                         +ebu_hc3(i,k,j)*conv_rho
        chem(i,k,j,p_hc5)  = chem(i,k,j,p_hc5)                         &
                         +ebu_hc5(i,k,j)*conv_rho
        chem(i,k,j,p_hc8)  = chem(i,k,j,p_hc8)                         &
                         +ebu_hc8(i,k,j)*conv_rho
        chem(i,k,j,p_eth)  = chem(i,k,j,p_eth)                         &
                         +ebu_eth(i,k,j)*conv_rho
        chem(i,k,j,p_co)  = chem(i,k,j,p_co)                           &
                         +ebu_co(i,k,j)*conv_rho
        chem(i,k,j,p_olt)  = chem(i,k,j,p_olt)                         &
                         +ebu_olt(i,k,j)*conv_rho
        chem(i,k,j,p_oli)  = chem(i,k,j,p_oli)                         &
                         +ebu_oli(i,k,j)*conv_rho
        chem(i,k,j,p_tol)  = chem(i,k,j,p_tol)                         &
                         +ebu_tol(i,k,j)*conv_rho
        chem(i,k,j,p_xyl)  = chem(i,k,j,p_xyl)                         &
                         +ebu_xyl(i,k,j)*conv_rho
        chem(i,k,j,p_ket)  =  chem(i,k,j,p_ket)                        &
                         +ebu_ket(i,k,j)*conv_rho
        chem(i,k,j,p_pm_25)  =  chem(i,k,j,p_pm_25)                        &
                         +r_q*ebu_pm25(i,k,j)/rho_phy(i,k,j)*dtstep
        chem(i,k,j,p_pm_10)  =  chem(i,k,j,p_pm_10)                        &
                         +r_q*ebu_pm10(i,k,j)/rho_phy(i,k,j)*dtstep
        enddo
        k=kts
        conv_rho=r_q*4.828e-4/rho_phy(i,k,j)*dtstep/(dz8w(i,k,j)*60.)
        chem(i,k,j,p_csl)  =  chem(i,k,j,p_csl)                        &
                         +ebu_csl(i,k,j)*conv_rho
        chem(i,k,j,p_iso)  = chem(i,k,j,p_iso)                         &
                         +ebu_iso(i,k,j)*conv_rho
        chem(i,k,j,p_no)   = chem(i,k,j,p_no)                          &
                         +ebu_no(i,k,j)*conv_rho
        chem(i,k,j,p_ald)  = chem(i,k,j,p_ald)                         &
                         +ebu_ald(i,k,j)*conv_rho
        chem(i,k,j,p_hcho) = chem(i,k,j,p_hcho)                        &
                         +ebu_hcho(i,k,j)*conv_rho
        chem(i,k,j,p_ora2)  = chem(i,k,j,p_ora2)                       &
                         +ebu_ora2(i,k,j)*conv_rho
        chem(i,k,j,p_hc3)  = chem(i,k,j,p_hc3)                         &
                         +ebu_hc3(i,k,j)*conv_rho
        chem(i,k,j,p_hc5)  = chem(i,k,j,p_hc5)                         &
                         +ebu_hc5(i,k,j)*conv_rho
        chem(i,k,j,p_hc8)  = chem(i,k,j,p_hc8)                         &
                         +ebu_hc8(i,k,j)*conv_rho
        chem(i,k,j,p_eth)  = chem(i,k,j,p_eth)                         &
                         +ebu_eth(i,k,j)*conv_rho
        chem(i,k,j,p_co)  = chem(i,k,j,p_co)                           &
                         +ebu_co(i,k,j)*conv_rho
        chem(i,k,j,p_olt)  = chem(i,k,j,p_olt)                         &
                         +ebu_olt(i,k,j)*conv_rho
        chem(i,k,j,p_oli)  = chem(i,k,j,p_oli)                         &
                         +ebu_oli(i,k,j)*conv_rho
        chem(i,k,j,p_tol)  = chem(i,k,j,p_tol)                         &
                         +ebu_tol(i,k,j)*conv_rho
        chem(i,k,j,p_xyl)  = chem(i,k,j,p_xyl)                         &
                         +ebu_xyl(i,k,j)*conv_rho
        chem(i,k,j,p_ket)  =  chem(i,k,j,p_ket)                        &
                         +ebu_ket(i,k,j)*conv_rho
        chem(i,k,j,p_pm_25)  =  chem(i,k,j,p_pm_25)                        &
                         +r_q*ebu_pm25(i,k,j)/rho_phy(i,k,j)/dz8w(i,k,j)*dtstep
        chem(i,k,j,p_pm_10)  =  chem(i,k,j,p_pm_10)                        &
                         +r_q*ebu_pm10(i,k,j)/rho_phy(i,k,j)/dz8w(i,k,j)*dtstep
        enddo
        enddo


    END subroutine add_emis_burn

END Module module_add_emiss_burn
