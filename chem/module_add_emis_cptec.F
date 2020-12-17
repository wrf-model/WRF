Module module_add_emis_cptec
CONTAINS
       subroutine add_emis_cptec(id,dtstep,ktau,dz8w,config_flags,      &
            curr_secs,rho_phy,chem,                                     &
            julday,gmt,xlat,xlong,t_phy,p_phy,emis_ant,                 &
!         ebu_no,ebu_co,ebu_co2,ebu_eth,ebu_hc3,ebu_hc5,ebu_hc8,          &
!          ebu_ete,ebu_olt,ebu_oli,ebu_pm25,ebu_pm10,ebu_dien,ebu_iso,     &
!          ebu_api,ebu_lim,ebu_tol,ebu_xyl,ebu_csl,ebu_hcho,ebu_ald,       &
!          ebu_ket,ebu_macr,ebu_ora1,ebu_ora2,                          &
            ids,ide, jds,jde, kds,kde,                                  &
            ims,ime, jms,jme, kms,kme,                                  &
            its,ite, jts,jte, kts,kte                                   )
  USE module_configure
  USE module_state_description
  USE module_date_time

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
   REAL, DIMENSION( ims:ime, kms:config_flags%kemit, jms:jme,num_emis_ant ),            &
         INTENT(IN ) ::                                                    &
                                         emis_ant
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
!  REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
!         INTENT(IN   ) ::                                                 &
!         ebu_no,ebu_co,ebu_co2,ebu_eth,ebu_hc3,ebu_hc5,ebu_hc8,          &
!          ebu_ete,ebu_olt,ebu_oli,ebu_pm25,ebu_pm10,ebu_dien,ebu_iso,     &
!          ebu_api,ebu_lim,ebu_tol,ebu_xyl,ebu_csl,ebu_hcho,ebu_ald,       &
!          ebu_ket,ebu_macr,ebu_ora1,ebu_ora2

      REAL,      INTENT(IN   ) ::                                          &
                             dtstep,gmt

      REAL(KIND=8), INTENT(IN   ) :: curr_secs

    integer ::imonth1,idate1,iyear1,itime1
    integer :: i,j,k
    real :: time,conv_rho
    integer :: iweek,idays
    real :: tign,timeq,r_q,r_antro
    real, dimension(7) :: week_CYCLE
    integer :: century_year,month,day,hour,minute,second,ten_thousandth 

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
    call split_date_char(start_date,century_year,month,day,hour,minute, &
         second,ten_thousandth)
    itime1 = hour

    idays = int(( float(itime1) + time/3600.)/24.+.00001)
    tign  = real(idays)*24.*3600.
    ! Modulacao da queimada media durante o ciclo diurno(unidade: 1/s)
    ! com a int( r_q dt) (0 - 24h)= 1.
    timeq= ( time + float(itime1)*3600. - tign )
    timeq=mod(timeq,86400.)


    !------------- anthropogenic diurnal cycle (industrial,residencial, ...)
    ! weekly cycle
    ! week day
    iweek= int(((float(julday)/7. - &
           int(julday/7))*7.)) + 1
    if(iweek.gt.7) iweek = iweek-7
    !- diurnal cycle
    r_antro  =1.4041297e-05*(exp(-((timeq-bx_antro)**2)/(43200.**2))+0.1)
    !- weekly + diurnal cycle
    r_antro = 86400.*r_antro * week_CYCLE(iweek)

      do 100 j=jts,jte
      do 100 i=its,ite

      k=kts
!
!  r_antro makes it weird!!!
!
        conv_rho=r_antro*4.828e-4/rho_phy(i,k,j)*dtstep/(60.*dz8w(i,k,j))
!       if(i.eq.its.and.j.eq.jts)then
!         write(0,*)conv_rho,r_antro,rho_phy(i,k,j),dtstep,dz8w(i,k,j),emis_ant(i,k,j,p_e_co)
!       endif
        chem(i,k,j,p_csl)  =  chem(i,k,j,p_csl)                        &
                         +emis_ant(i,k,j,p_e_csl)*conv_rho
        chem(i,k,j,p_iso)  = chem(i,k,j,p_iso)                         &
                         +emis_ant(i,k,j,p_e_iso)*conv_rho
        chem(i,k,j,p_so2)  = chem(i,k,j,p_so2)                         &
                         +emis_ant(i,k,j,p_e_so2)*conv_rho
        chem(i,k,j,p_no)   = chem(i,k,j,p_no)                          &
                         +emis_ant(i,k,j,p_e_no)*conv_rho
        chem(i,k,j,p_ald)  = chem(i,k,j,p_ald)                         &
                         +emis_ant(i,k,j,p_e_ald)*conv_rho
        chem(i,k,j,p_hcho) = chem(i,k,j,p_hcho)                        &
                         +emis_ant(i,k,j,p_e_hcho)*conv_rho
        chem(i,k,j,p_ora2)  = chem(i,k,j,p_ora2)                       &
                         +emis_ant(i,k,j,p_e_ora2)*conv_rho
        chem(i,k,j,p_nh3)  = chem(i,k,j,p_nh3)                         &
                         +emis_ant(i,k,j,p_e_nh3)*conv_rho
        chem(i,k,j,p_hc3)  = chem(i,k,j,p_hc3)                         &
                         +emis_ant(i,k,j,p_e_hc3)*conv_rho
        chem(i,k,j,p_hc5)  = chem(i,k,j,p_hc5)                         &
                         +emis_ant(i,k,j,p_e_hc5)*conv_rho
        chem(i,k,j,p_hc8)  = chem(i,k,j,p_hc8)                         &
                         +emis_ant(i,k,j,p_e_hc8)*conv_rho
        chem(i,k,j,p_eth)  = chem(i,k,j,p_eth)                         &
                         +emis_ant(i,k,j,p_e_eth)*conv_rho
        chem(i,k,j,p_co)  = chem(i,k,j,p_co)                           &
                         +emis_ant(i,k,j,p_e_co)*conv_rho
        if(p_ol2.gt.1)chem(i,k,j,p_ol2)  = chem(i,k,j,p_ol2)           &
                         +emis_ant(i,k,j,p_e_ol2)*conv_rho
        if(p_ete.gt.1)chem(i,k,j,p_ete)  = chem(i,k,j,p_ete)           &
                         +emis_ant(i,k,j,p_e_ol2)*conv_rho
        chem(i,k,j,p_olt)  = chem(i,k,j,p_olt)                         &
                         +emis_ant(i,k,j,p_e_olt)*conv_rho
        chem(i,k,j,p_oli)  = chem(i,k,j,p_oli)                         &
                         +emis_ant(i,k,j,p_e_oli)*conv_rho
        chem(i,k,j,p_tol)  = chem(i,k,j,p_tol)                         &
                         +emis_ant(i,k,j,p_e_tol)*conv_rho
        chem(i,k,j,p_xyl)  = chem(i,k,j,p_xyl)                         &
                         +emis_ant(i,k,j,p_e_xyl)*conv_rho
        chem(i,k,j,p_ket)  =  chem(i,k,j,p_ket)                        &
                         +emis_ant(i,k,j,p_e_ket)*conv_rho
        chem(i,k,j,p_pm_25)  =  chem(i,k,j,p_pm_25)                        &
                         +r_antro*emis_ant(i,k,j,p_e_pm_25)/rho_phy(i,k,j)/dz8w(i,k,j)*dtstep
        chem(i,k,j,p_pm_10)  =  chem(i,k,j,p_pm_10)                        &
                         +r_antro*emis_ant(i,k,j,p_e_pm_10)/rho_phy(i,k,j)/dz8w(i,k,j)*dtstep
 100  continue


    END subroutine add_emis_cptec

END Module module_add_emis_cptec
