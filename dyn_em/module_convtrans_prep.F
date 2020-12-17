!------------------------------------------------------------------------
! original routinte by Georg Grell, adaptive timestepping by William Gustafson Jr. (PNNL), cloud fraction by Georg Grell (based on Stu's previous work wih so2so4 routine
!------------------------------------------------------------------------

Module module_convtrans_prep
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CONTAINS
subroutine convtrans_prep(gd_cloud,gd_cloud2,gd_cloud_a,         &
     gd_cloud_b,raincv,raincv_a,raincv_b,                        &
     cldfr,moist,p_QV,p_QC,p_qi,T_PHY,P_PHY,num_moist,                 &
     gd_cloud2_a,gd_cloud2_b,convtrans_avglen_m,         &
     adapt_step_flag,curr_secs,                    &
     ktau,dt,cu_phys,  &
     ids,ide, jds,jde, kds,kde,                                  &
     ims,ime, jms,jme, kms,kme,                                  &
     its,ite, jts,jte,kts,kte                                    )
    REAL, PARAMETER  ::  coef_p = 0.25, coef_gamm = 0.49, coef_alph = 100.

  INTEGER,      INTENT(IN   ) :: ids,ide, jds,jde, kds,kde,      &
                                 ims,ime, jms,jme, kms,kme,      &
                                 its,ite, jts,jte, kts,kte,      &
                                 p_QV,p_QC,p_qi,num_moist

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
       OPTIONAL,                                                 &
       INTENT(IN ) :: gd_cloud,gd_cloud2
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
       INTENT(IN ) :: t_phy,p_phy
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),       &
       INTENT(IN ) :: moist
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
       OPTIONAL,                                                 &
       INTENT(INOUT ) :: gd_cloud_a,gd_cloud_b,gd_cloud2_a,      &
                         cldfr,gd_cloud2_b
  REAL, DIMENSION( ims:ime, jms:jme ),                           &
       INTENT(IN ) :: raincv
  REAL, DIMENSION( ims:ime, jms:jme ),                           &
       INTENT(INOUT ) :: raincv_a,raincv_b
  INTEGER, INTENT(IN) :: ktau,cu_phys
  INTEGER  :: stepave
  INTEGER, SAVE  :: stepave_count
  REAL, INTENT(IN) :: curr_secs
  REAL, INTENT(IN) :: convtrans_avglen_m, dt
  LOGICAL, INTENT(IN) :: adapt_step_flag

  LOGICAL :: avg_end_flag, first_period_flag
  REAL :: satvp,rhgrid,h2oliq
  real :: pmax,pmin
!
! Determine where we are in relation to the averaging period...
!
!  convtrans_avglen_m = 30.  !Averaging time for convective transport in min.

  stepave=convtrans_avglen_m*60./dt
  avg_end_flag = .false.      !Initially assume we are not at the end
  first_period_flag = .false. !Nor at the beginning
  if( adapt_step_flag ) then
     !If end of period...
     if( curr_secs+real(dt,8)+0.01 >= &
          ( int( curr_secs/real(convtrans_avglen_m*60.,8) + 1_8, 8) &
            *real(convtrans_avglen_m*60.,8) ) ) &
          avg_end_flag = .true.
     if( curr_secs <= real(convtrans_avglen_m*60.,8) ) first_period_flag = .true.
  else
     if( mod(ktau,stepave)==0 ) avg_end_flag = .true.
     if( ktau <= stepave ) first_period_flag = .true.
  end if
!
! Initialize the averaging arrays at the simulation start
!
  if(ktau.le.1)then
     stepave_count             = 0
     raincv_a(its:ite,jts:jte) = 0.
     raincv_b(its:ite,jts:jte) = 0.
  end if
  if(present(gd_cloud2_a))then
     if(ktau.le.1) gd_cloud2_a(its:ite,kts:kte,jts:jte)=0.
  end if
  if(present(cldfr))then
     if(ktau.le.1) cldfr(its:ite,kts:kte,jts:jte)=0.
  end if
!
! no time average available in first half hour
!
  if( first_period_flag )then
     do j=jts,jte
        do i=its,ite
           raincv_b(i,j)=raincv(i,j)
        enddo
     enddo
  end if
!
! build time average, and stored in raincv_b to be used by convective transport routine
!
  stepave_count = stepave_count+1
  do j=jts,jte
     do i=its,ite
        raincv_a(i,j)=raincv_a(i,j)+raincv(i,j)
     enddo
  enddo
  if( avg_end_flag )then
     do j=jts,jte
        do i=its,ite
           raincv_b(i,j)=raincv_a(i,j)/real(stepave_count)
           raincv_a(i,j)=0.
        enddo
     enddo
  end if
!
! do the same for convective parameterization cloud water mix ratio,
! currently only for cu_physics=3,5, used by both photolysis and atmospheric radiation
!
  if(cu_phys.eq.3.or.cu_phys.eq.5.or.cu_phys.eq.93)then
! if(config_flags%cu_physics == GDSCHEME  .OR. &
!    config_flags%cu_physics == GFSCHEME  .OR. &
!    config_flags%cu_physics == GFSCHEME ) THEN
!    pmax=maxval(gd_cloud)
!    pmin=maxval(gd_cloud2)
!    print *,pmax,pmin
     if( first_period_flag )then
        do j=jts,jte
           do k=kts,kte
              do i=its,ite
                 gd_cloud_b(i,k,j)=gd_cloud(i,k,j)
                 gd_cloud2_b(i,k,j)=gd_cloud2(i,k,j)
              enddo
           enddo
        enddo
     end if   ! stepave
!
!
!
     do j=jts,jte

        do k=kts,kte
           do i=its,ite
              gd_cloud_a(i,k,j)=gd_cloud_a(i,k,j)+gd_cloud(i,k,j)
              gd_cloud2_a(i,k,j)=gd_cloud2_a(i,k,j)+gd_cloud2(i,k,j)
           enddo
        enddo
     enddo
     if( avg_end_flag )then
        do j=jts,jte
           do k=kts,kte
              do i=its,ite
                 gd_cloud_b(i,k,j)=.1*gd_cloud_a(i,k,j)/real(stepave_count)
                 gd_cloud_a(i,k,j)=0.
                 gd_cloud2_b(i,k,j)=.1*gd_cloud2_a(i,k,j)/real(stepave_count)
                 gd_cloud2_a(i,k,j)=0.
              enddo
           enddo
        enddo
!    pmax=maxval(gd_cloud_b)
!    pmin=maxval(gd_cloud2_b)
!    print *,'avg_end_flag ',pmax,pmin
     end if !stepave
  end if ! cu_rad_feedback
!
! Clear the accumulator counter if we just finished an average...
!
if( avg_end_flag ) stepave_count = 0
! Siebesma et al., JAS, Vol. 60, no. 10, 1201-1219, 2003 (based on LES comparisons with trade-wind cumulus from BOMEX)
! SAM: Note units of liquid water and saturation vapor pressure must be in g/kg
    ! within the Siebesma et al. cloud fraction scheme
if( first_period_flag .or. avg_end_flag )then

        do j=jts,jte
           do k=kts,kte
              do i=its,ite
                cldfr(i,k,j)=0.
!               if(gd_cloud_b(i,k,j).gt.0  .or. gd_cloud2_b(i,k,j).gt.0)then

                   if(p_qc.gt.1 .and. p_qi.le.1)then

                      satvp = 3.80*exp(17.27*(t_phy(i,k,j)-273.)/ &
                            (t_phy(i,k,j)-36.))/(.01*p_phy(i,k,j))
                      rhgrid = max(.1,MIN( .95, moist(i,k,j,p_qv) /satvp))
                       h2oliq=1000.*(gd_cloud_b(i,k,j) + moist(i,k,j,p_qc))
                       satvp=1000.*satvp
                       cldfr(i,k,j)=(1.-exp(-coef_alph*h2oliq/((1.-rhgrid)*satvp)**coef_gamm))*(rhgrid**coef_p)
                       cldfr(i,k,j)=max(0.,MIN(1.,cldfr(i,k,j)))
                       if(moist(i,k,j,p_qc).eq.0)cldfr(i,k,j)=cldfr(i,k,j)*.2
                     endif
                   if(p_qc.gt.1 .and. p_qi.gt.1)then
                      satvp = 3.80*exp(17.27*(t_phy(i,k,j)-273.)/ &
                            (t_phy(i,k,j)-36.))/(.01*p_phy(i,k,j))
                      rhgrid = max(.1,MIN( .95, moist(i,k,j,p_qv) /satvp))
                       h2oliq=1000.*(gd_cloud_b(i,k,j) + moist(i,k,j,p_qc) + &
                                     gd_cloud2_b(i,k,j) + moist(i,k,j,p_qi))
                       satvp=1000.*satvp
                       cldfr(i,k,j)=(1.-exp(-coef_alph*h2oliq/((1.-rhgrid)*satvp)**coef_gamm))*(rhgrid**coef_p)
                       cldfr(i,k,j)=max(0.,MIN(1.,cldfr(i,k,j)))
                       if(moist(i,k,j,p_qc).eq.0 .and. moist(i,k,j,p_qi).eq.0)cldfr(i,k,j)=cldfr(i,k,j)*.2
                     endif
!                  endif
              enddo
           enddo
        enddo
   endif

END subroutine convtrans_prep

END MODULE MODULE_CONVTRANS_prep

