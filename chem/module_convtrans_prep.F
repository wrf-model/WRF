!------------------------------------------------------------------------
! Notes for Georg and Steve P.:
! Note 1: I changed the logic so that the last time step of each
!         averaging period is now included in the average. Before, it was
!         dropped, which gave a 7% error in the average for a 2 min. time
!         step.
!
! Note 2: I have not modified the algorithm to give a weighted avg based
!         on the time step duration. Eventually, this should be done to
!         account for this issue with varying time steps. One will need
!         to consider the best way to carry the weights through when
!         doing restarts.
!
! Note 3: I made the initialization block thread safe.
!
! Note 4: The old methodology would break on the first averaging period
!         after a restart if the restart did not occur exactly at the
!         beginning of an averaging period. I think the new methodology
!         is more robust and should work with any restart time, but I
!         have not tested it yet.
!
! William.Gustafson@pnl.gov; 9-May-2008
!------------------------------------------------------------------------

Module module_convtrans_prep
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CONTAINS
subroutine convtrans_prep(gd_cloud,gd_cloud2,gd_cloud_a,         &
     gd_cloud_b,raincv,raincv_a,raincv_b,                        &
     gd_cloud2_a,gd_cloud2_b,convtrans_avglen_m,stepave,         &
     adapt_step_flag,curr_secs,stepave_count,                    &
     ktau,dt,cu_rad_feedback,cu_phys,  &
     ids,ide, jds,jde, kds,kde,                                  &
     ims,ime, jms,jme, kms,kme,                                  &
     its,ite, jts,jte,kts,kte                                    )

  INTEGER,      INTENT(IN   ) :: ids,ide, jds,jde, kds,kde,      &
                                 ims,ime, jms,jme, kms,kme,      &
                                 its,ite, jts,jte, kts,kte
 
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
       OPTIONAL,                                                 &
       INTENT(IN ) :: gd_cloud,gd_cloud2
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
       OPTIONAL,                                                 &
       INTENT(INOUT ) :: gd_cloud_a,gd_cloud_b,gd_cloud2_a,gd_cloud2_b
  REAL, DIMENSION( ims:ime, jms:jme ),                           &
       INTENT(IN ) :: raincv
  REAL, DIMENSION( ims:ime, jms:jme ),                           &
       INTENT(INOUT ) :: raincv_a,raincv_b
  INTEGER, INTENT(IN) :: ktau,cu_phys
  INTEGER, INTENT(INOUT) :: stepave, stepave_count
  REAL(KIND=8), INTENT(IN) :: curr_secs
  REAL, INTENT(IN) :: convtrans_avglen_m, dt
  LOGICAL, INTENT(IN) :: adapt_step_flag, cu_rad_feedback

  LOGICAL :: avg_end_flag, first_period_flag
!
! Determine where we are in relation to the averaging period...
!
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
  if(cu_rad_feedback.and.(cu_phys.eq.3.or.cu_phys.eq.5))then
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
                 gd_cloud_b(i,k,j)=gd_cloud_a(i,k,j)/real(stepave_count)
                 gd_cloud_a(i,k,j)=0.
                 gd_cloud2_b(i,k,j)=gd_cloud2_a(i,k,j)/real(stepave_count)
                 gd_cloud2_a(i,k,j)=0.
              enddo
           enddo
        enddo
     end if !stepave
  end if ! cu_rad_feedback
!
! Clear the accumulator counter if we just finished an average...
!
if( avg_end_flag ) stepave_count = 0

END subroutine convtrans_prep

END MODULE MODULE_CONVTRANS_prep
