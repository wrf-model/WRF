!
! This module contains routines to initialize tracers, handle boundary conditions
! and other stuff related to tracers. As of WRFV3.2 it will users should compile 
! WRF-Chem for tracer runs, if they want full dispersion. Only when 
! WRF-Chem is compiled will turbulent and non-resolved convective transport be treated.
! When compiled with WRF_CHEM, tracer transport should work properly with nesting or
! supplied boundary conditions. Without compiling WRF-Chem, option 
! TRACER_TEST1 will partially work (no boundary conditions, 
! no turbulent transport, no subgrid-scale convection)
!
! Original version of the module is written by Georg Grell (Dec 2009). 
! Options for TRACER_TEST1 and TRACER_TEST2 supplied by Jeff Lee (NCAR)
! Current tracer options:
!
!   (1) TRACER_SMOKE: This needs the biomass burning module to also be active. 
!       It will then use smoke (CO emissions from fire) as tracer. One
!       variabe only.
!
!   (2) TRACER_TEST1 and TRACER_TEST2: 8 tracers, the only difference inbetween 
!       these options are p_tr17_3 and p_tr17_4, which are also filled with
!       CO emissions from fire for TRACER_TEST2. The other tracers are defined as:
!
!            tr17_1 : horizontal boundaries tracer
!            tr17_2 : horizontal boundaries tracer decaying with e-folding time of 1 day
!            tr17_3 : surface tracer (smoke for TRACER_TEST2)
!            tr17_4 : surface tracer (smoke for TRACER_TEST2)
!                     decaying with e-folding time of 1 day
!            tr17_5 : stratosphere tracer
!            tr17_6 : stratosphere tracer decaying with e-folding time of 1 day
!            tr17_7 : boundary layer tracer     
!            tr17_8 : boundary layer tracer decaying with e-folding time of 1 day



MODULE module_input_tracer
USE module_input_tracer_data
#if ( WRF_CHEM == 1 )
USE module_state_description, only:tracer_smoke,tracer_test1,tracer_test2,param_first_scalar,p_tr17_1,p_tr17_2,p_tr17_3,p_tr17_4,p_tr17_5,p_tr17_6,p_tr17_7,p_tr17_8
#else
USE module_state_description, only:tracer_test1,tracer_test2,param_first_scalar,p_tr17_1,p_tr17_2,p_tr17_3,p_tr17_4,p_tr17_5,p_tr17_6,p_tr17_7,p_tr17_8
#endif
CONTAINS
   SUBROUTINE initialize_tracer (chem,chem_in_opt,         &
                                       tracer_opt,num_chem,&
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )
      INTEGER,      INTENT(IN   )    :: chem_in_opt,tracer_opt,num_chem
      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      REAL,  DIMENSION(ims:ime,kms:kme,jms:jme,num_chem ), INTENT(INOUT) :: chem
#if ( WRF_CHEM == 1 ) 
      if(chem_in_opt == 1 )return
#endif
      if     (tracer_opt == TRACER_TEST1)then
       chem(:,:,:,:)=.0
#if ( WRF_CHEM == 1 )
      else if(tracer_opt == TRACER_TEST2)then
       chem(:,:,:,:)=.0
      else if(tracer_opt == TRACER_SMOKE)then
       chem(:,:,:,:)=.08
#endif
      endif
   END SUBROUTINE initialize_tracer
#if (EM_CORE == 1 ) 
   SUBROUTINE flow_dep_bdy_tracer  (  chem,                                       &
                               chem_bxs,chem_btxs,                                  &
                               chem_bxe,chem_btxe,                                  &
                               chem_bys,chem_btys,                                  &
                               chem_bye,chem_btye,                                  &
                               dt,                                              &
                               spec_bdy_width,z,                                &
                               have_bcs_chem,                        & 
                               u, v, tracer_opt, alt, & 
                               t,pb,p,t0,p1000mb,rcp,ph,phb,g, &
                               spec_zone, ic,           &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

!  This subroutine sets zero gradient conditions for outflow and a set profile value
!  for inflow in the boundary specified region. Note that field must be unstaggered.
!  The velocities, u and v, will only be used to check their sign (coupled vels OK)
!  spec_zone is the width of the outer specified b.c.s that are set here.
!  (JD August 2000)

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: tracer_opt
      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_zone,spec_bdy_width,ic
      REAL,         INTENT(IN   )    :: dt


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: chem
      REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width), INTENT(IN   ) :: chem_bxs, chem_bxe, chem_btxs, chem_btxe
      REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width), INTENT(IN   ) :: chem_bys, chem_bye, chem_btys, chem_btye
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: z
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: alt
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: u
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: v
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,         &
          INTENT(IN   ) ::                                           &
                               ph,phb,t,pb,p
   real, INTENT (IN) :: g,rcp,t0,p1000mb

      INTEGER    :: i, j, k, numgas
      INTEGER    :: ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: i_inner, j_inner
      INTEGER    :: b_dist
      integer    :: i_bdy_method
      real tempfac,convfac
      logical, optional    :: have_bcs_chem

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1

! i_bdy_method determines which "bdy_chem_value" routine to use
!   1=smoke, CO background
      i_bdy_method = 0
        if (tracer_opt == TRACER_TEST1 ) then
          i_bdy_method = 2
        end if   
#if ( WRF_CHEM == 1 )
        if (tracer_opt == TRACER_TEST2 ) then
          i_bdy_method = 2
        end if   
        if (tracer_opt == TRACER_SMOKE ) then
          i_bdy_method = 1
        end if
      if (have_bcs_chem) i_bdy_method =6
#endif
      if (ic .lt. param_first_scalar) i_bdy_method = 0

      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary
        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          DO k = kts, ktf
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(v(i,k,j) .lt. 0.)THEN
                chem(i,k,j) = chem(i_inner,k,jbs+spec_zone)
              ELSE
                if (i_bdy_method .eq. 0) then
                   chem(i,k,j) = tracer_bv_def
                else if (i_bdy_method .eq. 1) then
                   chem(i,k,j)=tr_smoke_value
                else if (i_bdy_method .eq. 2) then
                   if (ic .eq. p_tr17_1 .or. ic .eq. p_tr17_2) then
                      chem(i,k,j)= tracer_bv_one
                   else
                      chem(i,k,j)= tracer_bv_def
                   endif
#if ( WRF_CHEM == 1 )
                else if (i_bdy_method .eq. 6) then
                   CALL bdy_tracer_value ( chem(i,k,j),chem_bys(i,k,1),chem_btys(i,k,1),dt,ic)
#endif
                else
                   chem(i,k,j) = tracer_bv_def
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 
! Y-end boundary 
        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          DO k = kts, ktf 
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(v(i,k,j+1) .gt. 0.)THEN
                chem(i,k,j) = chem(i_inner,k,jbe-spec_zone)
              ELSE
                if (i_bdy_method .eq. 0) then
                   chem(i,k,j) = tracer_bv_def
                else if (i_bdy_method .eq. 1) then
                   chem(i,k,j)=tr_smoke_value
                else if (i_bdy_method .eq. 2) then
                   if (ic .eq. p_tr17_1 .or. ic .eq. p_tr17_2) then
                      chem(i,k,j)= tracer_bv_one
                   else
                      chem(i,k,j)= tracer_bv_def
                   endif
#if ( WRF_CHEM == 1 )
                else if (i_bdy_method .eq. 6) then
                   CALL bdy_tracer_value ( chem(i,k,j),chem_bye(i,k,1),chem_btye(i,k,1),dt,ic)
#endif
                else
                   chem(i,k,j) = tracer_bv_def
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i,k,j) .lt. 0.)THEN
                chem(i,k,j) = chem(ibs+spec_zone,k,j_inner)
              ELSE
                if (i_bdy_method .eq. 0) then
                   chem(i,k,j) = tracer_bv_def
                else if (i_bdy_method .eq. 1) then
                   chem(i,k,j)=tr_smoke_value
                else if (i_bdy_method .eq. 2) then
                   if (ic .eq. p_tr17_1 .or. ic .eq. p_tr17_2) then
                      chem(i,k,j)= tracer_bv_one
                   else
                      chem(i,k,j)= tracer_bv_def
                   endif
#if ( WRF_CHEM == 1 )
                else if (i_bdy_method .eq. 6) then
                   CALL bdy_tracer_value ( chem(i,k,j),chem_bxs(j,k,1),chem_btxs(j,k,1),dt,ic)   
#endif
                else
                   chem(i,k,j) = tracer_bv_def
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (ibe - itf .lt. spec_zone) THEN
! X-end boundary
        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i+1,k,j) .gt. 0.)THEN
                chem(i,k,j) = chem(ibe-spec_zone,k,j_inner)
              ELSE
                if (i_bdy_method .eq. 0) then
                   chem(i,k,j) = tracer_bv_def
                else if (i_bdy_method .eq. 1) then
                   chem(i,k,j)=tr_smoke_value
                else if (i_bdy_method .eq. 2) then
                   if (ic .eq. p_tr17_1 .or. ic .eq. p_tr17_2) then
                      chem(i,k,j)= tracer_bv_one
                   else
                      chem(i,k,j)= tracer_bv_def
                   endif
#if ( WRF_CHEM == 1 )
                else if (i_bdy_method .eq. 6) then
                   CALL bdy_tracer_value ( chem(i,k,j),chem_bxe(j,k,1),chem_btxe(j,k,1),dt,ic)
#endif
                else
                   chem(i,k,j) = tracer_bv_def
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

   END SUBROUTINE flow_dep_bdy_tracer
#else
#if ( WRF_CHEM == 1 )
   SUBROUTINE flow_dep_bdy_tracer  (  chem, chem_b,chem_bt,dt,                    &
                               spec_bdy_width,z,                                &
                               ijds, ijde,have_bcs_chem,                        & 
                               u, v, tracer_opt, alt, & 
                               t,pb,p,t0,p1000mb,rcp,ph,phb,g, &
                               spec_zone, ic,           &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

!  This subroutine sets zero gradient conditions for outflow and a set profile value
!  for inflow in the boundary specified region. Note that field must be unstaggered.
!  The velocities, u and v, will only be used to check their sign (coupled vels OK)
!  spec_zone is the width of the outer specified b.c.s that are set here.
!  (JD August 2000)

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: tracer_opt
      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: ijds,ijde
      INTEGER,      INTENT(IN   )    :: spec_zone,spec_bdy_width,ic
      REAL,         INTENT(IN   )    :: dt


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: chem
      REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 ), INTENT(IN   ) :: chem_b
      REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 ), INTENT(IN   ) :: chem_bt
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: z
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: alt
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: u
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: v
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,         &
          INTENT(IN   ) ::                                           &
                               ph,phb,t,pb,p
   real, INTENT (IN) :: g,rcp,t0,p1000mb

      INTEGER    :: i, j, k, numgas
      INTEGER    :: ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: i_inner, j_inner
      INTEGER    :: b_dist
      integer    :: i_bdy_method
      real tempfac,convfac
      real       :: tracer_bv_def
      logical    :: have_bcs_chem

      tracer_bv_def = conmin
      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1

      i_bdy_method = 0
        if (config_flags%tracer_opt == TRACER_SMOKE ) then
          i_bdy_method = 1
        end if
      if (have_bcs_chem) i_bdy_method =6
      if (ic .lt. param_first_scalar) i_bdy_method = 0

!----------------------------------------------------------------------
      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary
        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          DO k = kts, ktf
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(v(i,k,j) .lt. 0.)THEN
                chem(i,k,j) = chem(i_inner,k,jbs+spec_zone)
              ELSE
                if (i_bdy_method .eq. 1) then
                   chem(i,k,j)=tr_smoke_value
                else if (i_bdy_method .eq. 6) then
                   CALL bdy_tracer_value ( chem(i,k,j),chem_b(i,k,1,P_YSB),chem_bt(i,k,1,P_YSB),dt,ic)
                else
                   chem(i,k,j) = tracer_bv_def
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 
! Y-end boundary 
        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          DO k = kts, ktf 
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(v(i,k,j+1) .gt. 0.)THEN
                chem(i,k,j) = chem(i_inner,k,jbe-spec_zone)
              ELSE
                if (i_bdy_method .eq. 1) then
                   chem(i,k,j)=tr_smoke_value
                else if (i_bdy_method .eq. 6) then
                   CALL bdy_tracer_value ( chem(i,k,j),chem_b(i,k,1,P_YEB),chem_bt(i,k,1,P_YEB),dt,ic)
                else
                   chem(i,k,j) = tracer_bv_def
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i,k,j) .lt. 0.)THEN
                chem(i,k,j) = chem(ibs+spec_zone,k,j_inner)
              ELSE
                if (i_bdy_method .eq. 1) then
                   chem(i,k,j)=tr_smoke_value
                else if (i_bdy_method .eq. 6) then
                   CALL bdy_tracer_value ( chem(i,k,j),chem_b(j,k,1,P_XSB),chem_bt(j,k,1,P_XSB),dt,ic)
                else
                   chem(i,k,j) = tracer_bv_def
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (ibe - itf .lt. spec_zone) THEN
! X-end boundary
        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i+1,k,j) .gt. 0.)THEN
                chem(i,k,j) = chem(ibe-spec_zone,k,j_inner)
              ELSE
                if (i_bdy_method .eq. 1) then
                   chem(i,k,j)=tr_smoke_value
                else if (i_bdy_method .eq. 6) then
                   CALL bdy_tracer_value ( chem(i,k,j),chem_b(j,k,1,P_XEB),chem_bt(j,k,1,P_XEB),dt,ic)
                else
                   chem(i,k,j) = tracer_bv_def
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

   END SUBROUTINE flow_dep_bdy_tracer
#endif
#endif
   SUBROUTINE set_tracer(dtstep,ktau,pbl_h,tracer,t,tracer_opt,num_tracer,&
                         z,ht,ids,ide, jds,jde, kds,kde,                  & 
                               ims,ime, jms,jme, kms,kme,                 & 
                               its,ite, jts,jte, kts,kte                  )
      INTEGER,      INTENT(IN   )    :: ktau,tracer_opt,num_tracer
      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      REAL,  DIMENSION(ims:ime,kms:kme,jms:jme,num_tracer ), INTENT(INOUT) :: tracer
      REAL,  DIMENSION(ims:ime,kms:kme,jms:jme ), INTENT(IN) :: t,z
      REAL,  DIMENSION(ims:ime,jms:jme ), INTENT(IN) :: PBL_H,HT
      REAL,  INTENT(IN) :: dtstep
      INTEGER:: count_trop,count_pbl
!
! this is for tracer options tracer_test1 and tracer_test2
!
    factor_decay = 1./(86400./dtstep)

!-- decay, every time step (ktau), whole domain

    tracer(its:ite,kts:kte,jts:jte,p_tr17_2) = &
       tracer(its:ite,kts:kte,jts:jte,p_tr17_2) * (1. - factor_decay)

    tracer(its:ite,kts:kte,jts:jte,p_tr17_4) = &
       tracer(its:ite,kts:kte,jts:jte,p_tr17_4) * (1. - factor_decay)

    tracer(its:ite,kts:kte,jts:jte,p_tr17_6) = &
       tracer(its:ite,kts:kte,jts:jte,p_tr17_6) * (1. - factor_decay)

    tracer(its:ite,kts:kte,jts:jte,p_tr17_8) = &
       tracer(its:ite,kts:kte,jts:jte,p_tr17_8) * (1. - factor_decay)
 IF (ktau .ge. 2) THEN
    
!-- every time step, every grid point, restore some tracer

!(1)level 1 restore to 1.0
    if(tracer_opt == TRACER_TEST1)then   
       tracer(its:ite,kts,jts:jte,p_tr17_3)     = 1.0
       tracer(its:ite,kts,jts:jte,p_tr17_4)     = 1.0
    endif
       
    do i= its,ite
    do j= jts,jte
 
!(2)every level above tropopause (t minimum), restore to 1.0

!-- get levels of tropopause (count_trop)

       count_trop = minloc(t(i,kts:kte,j),1)

       tracer(i,count_trop:kte,j,p_tr17_5) = 1.0
       tracer(i,count_trop:kte,j,p_tr17_6) = 1.0

!(3)every level below pblh, restore to 1.0

!-- get levels in pbl (count_pbl)

       count_pbl = 0

       do k=kts,kte
          if ( (z(i,k,j)-ht(i,j)) .le. pbl_h(i,j) ) then
             count_pbl = count_pbl + 1
          endif
       end do

       if (count_pbl .ge. 1) then
          tracer(i,kts:count_pbl,j,p_tr17_7) = 1.0
          tracer(i,kts:count_pbl,j,p_tr17_8) = 1.0
       endif

    end do   ! j
    end do   ! i

 ENDIF   ! ktau  
   END SUBROUTINE set_tracer
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE bdy_tracer_value ( trac, trac_b, trac_bt, dt,ic)
                                  
    IMPLICIT NONE

    REAL,    intent(OUT)  :: trac
    REAL,    intent(IN)   :: trac_b
    REAL,    intent(IN)   :: trac_bt
    REAL,    intent(IN)   :: dt
    INTEGER, intent(IN)   :: ic

    REAL                  :: epsilc = 1.E-12
!   CHARACTER (LEN=80) :: message
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!     if( ntracer .GT. numtracer) then
!       message = ' Input_tracer_profile: wrong number of tracers'
!       return
!       CALL WRF_ERROR_FATAL ( message )
!     endif
     
      trac=max(epsilc,trac_b + trac_bt * dt)

      RETURN
  END SUBROUTINE bdy_tracer_value
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE module_input_tracer
