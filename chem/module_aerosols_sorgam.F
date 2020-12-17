MODULE module_aerosols_sorgam
!
  USE module_state_description
  USE module_data_radm2
  USE module_data_sorgam
  USE module_radm
! USE module_isrpia, only: isoropia
!
      IMPLICIT NONE
#define cw_species_are_in_registry

CONTAINS
    SUBROUTINE sorgam_driver (id,ktau,dtstep,t_phy,moist,aerwrf,p8w,    &
               t8w,alt,p_phy,chem,rho_phy,dz8w,z,z_at_w,                &
               h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2, &
               cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,vcsulf_old,    &
               vdrog3,                                                  &
               kemit,                                                   &
               ids,ide, jds,jde, kds,kde,                               &
               ims,ime, jms,jme, kms,kme,                               &
               its,ite, jts,jte, kts,kte                                )

   INTEGER,      INTENT(IN   )    ::                             &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                      its,ite, jts,jte, kts,kte, &
                                      kemit,                     &
                                      id,ktau

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),        &
         INTENT(IN ) ::                                   moist

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),         &
         INTENT(INOUT ) ::                                   chem
!
! following are aerosol arrays that are not advected
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(INOUT ) ::                                             &
           h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2,    &
           cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2


   REAL,  DIMENSION(ims:ime,kms:kme-0,jms:jme,ldrog),                  &
           INTENT(IN   ) ::                                            &
                                                  VDROG3               
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,           &
          INTENT(IN   ) ::                                             &
                                                      t_phy,           &
                                                        alt,           &
                                                      p_phy,           &
                                                      dz8w,            &
                                                      z    ,           &
                                              t8w,p8w,z_at_w ,         &
                                                      aerwrf ,         &
                                                    rho_phy
   REAL,  DIMENSION( ims:ime , kms:kme-0 , jms:jme )         ,         &
          INTENT(IN   ) ::                                             &
             vcsulf_old
      REAL,      INTENT(IN   ) ::                                      &
                             dtstep

      REAL drog_in(ldrog)                                    ! anthropogenic AND
                                                             ! biogenic organic
                                                             ! aerosol precursor [ug m**-3 s**-1]

      REAL condvap_in(lspcv) !bs
                             !rs
                             ! condensable vapors [ug m**-3]
      REAL rgas
      DATA rgas/8.314510/
      REAL convfac,convfac2
!...BLKSIZE set to one in column model ciarev02

      INTEGER blksize
      PARAMETER (blksize=1)

!...number of aerosol species
!  number of species (gas + aerosol)
      INTEGER nspcsda
      PARAMETER (nspcsda=l1ae) !bs
! (internal aerosol dynamics)
!bs # of anth. cond. vapors in SORGAM
      INTEGER nacv
      PARAMETER (nacv=lcva) !bs # of anth. cond. vapors in CTM
!bs total # of cond. vapors in SORGAM
      INTEGER ncv
      PARAMETER (ncv=lspcv) !bs
!bs total # of cond. vapors in CTM
      REAL cblk(blksize,nspcsda) ! main array of variables
                                   ! particles [ug/m^3/s]
      REAL soilrat_in
                    ! emission rate of soil derived coars
                    ! input HNO3 to CBLK [ug/m^3]
      REAL nitrate_in
                    ! input NH3 to CBLK  [ug/m^3]
      REAL nh3_in
                    ! input SO4 vapor    [ug/m^3]
      REAL hcl_in

      REAL vsulf_in

      REAL so4rat_in
                    ! input SO4 formation[ug/m^3/sec]
      REAL epm25i(blksize),epm25j(blksize),epmcoarse(blksize)
                    ! Emission rate of i-mode EC [ug m**-3 s**-1]
      REAL eeci_in
                    ! Emission rate of j-mode EC [ug m**-3 s**-1]
      REAL eecj_in
                    ! Emission rate of j-mode org. aerosol [ug m**-
      REAL eorgi_in

      REAL eorgj_in
                    ! Emission rate of j-mode org. aerosol [ug m**-
                    ! pressure in cb
      REAL pres
                    ! temperature in K
      REAL temp
                    !bs
      REAL relhum
                    ! rel. humidity (0,1)   
      REAL ::p(kts:kte),t(kts:kte),rh(kts:kte)

!...molecular weights                   ciarev02

! molecular weight for SO4
      REAL mwso4
      PARAMETER (mwso4=96.0576)

! molecular weight for HNO3
      REAL mwhno3
      PARAMETER (mwhno3=63.01287)

! molecular weight for NH3
      REAL mwnh3
      PARAMETER (mwnh3=17.03061)

! molecular weight for HCL
      REAL mwhcl
      PARAMETER (mwhcl=36.46100)

!bs molecular weight for Organic Spec
!     REAL mworg
!     PARAMETER (mworg=175.0)

!bs molecular weight for Elemental Ca
      REAL mwec
      PARAMETER (mwec=12.0)

!rs molecular weight
      REAL mwaro1
      PARAMETER (mwaro1=150.0)

!rs molecular weight
      REAL mwaro2
      PARAMETER (mwaro2=150.0)

!rs molecular weight
      REAL mwalk1
      PARAMETER (mwalk1=140.0)

!rs molecular weight
      REAL mwalk2
      PARAMETER (mwalk2=140.0)

!rs molecular weight
!rs molecular weight
      REAL mwole1
      PARAMETER (mwole1=140.0)

!rs molecular weight
      REAL mwapi1
      PARAMETER (mwapi1=200.0)

!rs molecular weight
      REAL mwapi2
      PARAMETER (mwapi2=200.0)

!rs molecular weight
      REAL mwlim1
      PARAMETER (mwlim1=200.0)

!rs molecular weight
      REAL mwlim2
      PARAMETER (mwlim2=200.0)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   INTEGER :: i,j,k,l,debug_level
!
! convert advected aerosol variables to ug/m3 from mixing ratio
! they will be converted back at the end of this driver
!
   do l=p_so4aj,num_chem
      do j=jts,jte
         do k=kts,kte
            do i=its,ite
               chem(i,k,j,l)=max(epsilc,chem(i,k,j,l)/alt(i,k,j))
            enddo
         enddo
      enddo
   enddo
      do 100 j=jts,jte
         do 100 i=its,ite
           debug_level=0
            do k=kts,kte
               t(k) = t_phy(i,k,j)
               p(k) = .001*p_phy(i,k,j)
               rh(k) = MIN( 95.,100. * moist(i,k,j,p_qv) /        &
                        (3.80*exp(17.27*(t_phy(i,k,j)-273.)/      &
                        (t_phy(i,k,j)-36.))/(.01*p_phy(i,k,j)))   )
               rh(k)=max(.1,0.01*rh(k))
            enddo
            do k=kts,kte
!           if(timer.gt.2.)then
!                if((i.eq.12.and.j.eq.17.and.k.eq.1).or.       &
!                   (i.eq.12.and.j.eq.7.and.k.eq.2).or.       &
!                   (i.eq.1.and.j.eq.17.and.k.eq.2))iprt=1
!                if(debug_level.ge.1)print *,ktau,timer,i,j,k,p(k),t(k),dtstep,rgas,vcsulf_old(i,k,j),MWSO4,chem(i,k,j,p_sulf)
!           endif
               cblk=0.
               do l=1,ldrog
                  drog_in(l)=0.
               enddo
               do l=1,lspcv
                  condvap_in(l)=0.
               enddo
               convfac = p(k)/rgas/t(k)*1000.
               so4rat_in=(chem(i,k,j,p_sulf)-vcsulf_old(i,k,j))/dtstep*CONVFAC*MWSO4
               soilrat_in = 0.
               nitrate_in =max(epsilc,chem(i,k,j,p_hno3)*convfac*mwhno3)
               nh3_in = max(epsilc,chem(i,k,j,p_nh3)*convfac*mwnh3)
               hcl_in = max(epsilc,chem(i,k,j,p_hcl)*convfac*mwhcl)
!                   hcl_in = 0.
               vsulf_in = max(epsilc,chem(i,k,j,p_sulf)*convfac*mwso4)
!     if(i.eq.28.and.j.eq.25.and.k.eq.1)then
!       print *,'vsulfin = ',vsulf_in,chem(i,k,j,p_sulf),convfac,mwso4
!       print *,'nitratein = ',nitrate_in,chem(i,k,j,p_hno3)
!       print *,'nh3in = ',nh3_in,chem(i,k,j,p_nh3)
!       print *,'hclin = ',hcl_in,chem(i,k,j,p_hcl)
!       print *,'pso4ai = ',chem(i,k,j,p_so4aj),chem(i,k,j,p_no3aj),chem(i,k,j,p_nh4aj),chem(i,k,j,p_naaj)
!     endif
               
!rs
!rs * organic aerosol precursors
!rs * anthropogenic organics DeltaROG
        drog_in(PXYL ) = VDROG3(i,k,j,PXYL )
        drog_in(PTOL ) = VDROG3(i,k,j,PTOL )
        drog_in(PCSL1) = VDROG3(i,k,j,PCSL1)
        drog_in(PCSL2) = VDROG3(i,k,j,PCSL2)
        drog_in(PHC8 ) = VDROG3(i,k,j,PHC8 )
        drog_in(POLI1) = VDROG3(i,k,j,POLI1)
        drog_in(POLI2) = VDROG3(i,k,j,POLI2)
        drog_in(POLI3) = VDROG3(i,k,j,POLI3)
        drog_in(POLT1) = VDROG3(i,k,j,POLT1)
        drog_in(POLT2) = VDROG3(i,k,j,POLT2)
        drog_in(POLT3) = VDROG3(i,k,j,POLT3)
!rs * biogenic organics DeltaROG
        if(p_lim.eq.1)then
!            if(p_ete.eq.1)then
            drog_in(PAPI1) = 0.
            drog_in(PAPI2) = 0.
            drog_in(PAPI3) = 0.
            drog_in(PLIM1) = 0.
            drog_in(PLIM2) = 0.
            drog_in(PLIM3) = 0.
            condvap_in(PSOAAPI1) = 0.
            condvap_in(PSOAAPI2) = 0.
            condvap_in(PSOALIM1) = 0.
            condvap_in(PSOALIM2) = 0.
        elseif(p_lim.gt.1)then
!            elseif(p_ete.gt.1)then
            drog_in(PAPI1) = VDROG3(i,k,j,PAPI1)
            drog_in(PAPI2) = VDROG3(i,k,j,PAPI2)
            drog_in(PAPI3) = VDROG3(i,k,j,PAPI3)
            drog_in(PLIM1) = VDROG3(i,k,j,PLIM1)
            drog_in(PLIM2) = VDROG3(i,k,j,PLIM2)
            drog_in(PLIM3) = VDROG3(i,k,j,PLIM3)
            condvap_in(PSOAAPI1) = max(epsilc,cvapi1(i,k,j))
            condvap_in(PSOAAPI2) = max(epsilc,cvapi2(i,k,j))
            condvap_in(PSOALIM1) = max(epsilc,cvlim1(i,k,j))
            condvap_in(PSOALIM2) = max(epsilc,cvlim2(i,k,j))
        endif
        condvap_in(PSOAARO1) = max(epsilc,cvaro1(i,k,j))
        condvap_in(PSOAARO2) = max(epsilc,cvaro2(i,k,j))
        condvap_in(PSOAALK1) = max(epsilc,cvalk1(i,k,j))
        condvap_in(PSOAOLE1) = max(epsilc,cvole1(i,k,j))
      cblk(1,VORGARO1J) =   chem(i,k,j,p_orgaro1j)
      cblk(1,VORGARO1I) =   chem(i,k,j,p_orgaro1i)
      cblk(1,VORGARO2J) =   chem(i,k,j,p_orgaro2j)
      cblk(1,VORGARO2I) =   chem(i,k,j,p_orgaro2i)
      cblk(1,VORGALK1J) =   chem(i,k,j,p_orgalk1j)
      cblk(1,VORGALK1I) =   chem(i,k,j,p_orgalk1i)
      cblk(1,VORGOLE1J) =   chem(i,k,j,p_orgole1j)
      cblk(1,VORGOLE1I) =   chem(i,k,j,p_orgole1i)
      cblk(1,VORGBA1J ) =   chem(i,k,j,p_orgba1j)
      cblk(1,VORGBA1I ) =   chem(i,k,j,p_orgba1i)
      cblk(1,VORGBA2J ) =   chem(i,k,j,p_orgba2j)
      cblk(1,VORGBA2I ) =   chem(i,k,j,p_orgba2i)
      cblk(1,VORGBA3J ) =   chem(i,k,j,p_orgba3j)
      cblk(1,VORGBA3I ) =   chem(i,k,j,p_orgba3i)
      cblk(1,VORGBA4J ) =   chem(i,k,j,p_orgba4j)
      cblk(1,VORGBA4I ) =   chem(i,k,j,p_orgba4i)
      cblk(1,VORGPAJ  ) =   chem(i,k,j,p_orgpaj)
      cblk(1,VORGPAI  ) =   chem(i,k,j,p_orgpai)
      cblk(1,VECJ     ) =   chem(i,k,j,p_ecj)
      cblk(1,VECI     ) =   chem(i,k,j,p_eci)
      cblk(1,VP25AJ   ) =   chem(i,k,j,p_p25j)
      cblk(1,VP25AI   ) =   chem(i,k,j,p_p25i)
      cblk(1,VANTHA   ) =   chem(i,k,j,p_antha)
      cblk(1,VSEAS    ) =   chem(i,k,j,p_seas)
      cblk(1,VSOILA   ) =   chem(i,k,j,p_soila)
      cblk(1,VH2OAJ   ) =   max(epsilc,h2oaj(i,k,j))
      cblk(1,VH2OAI   ) =   max(epsilc,h2oai(i,k,j))
      cblk(1,VNU3     ) =   max(epsilc,nu3(i,k,j))
      cblk(1,VAC3     ) =   max(epsilc,ac3(i,k,j))
      cblk(1,VCOR3    ) =   max(epsilc,cor3(i,k,j))
      cblk(1,VCVARO1  ) =   max(epsilc,cvaro1(i,k,j))
      cblk(1,VCVARO2  ) =   max(epsilc,cvaro2(i,k,j))
      cblk(1,VCVALK1  ) =   max(epsilc,cvalk1(i,k,j))
      cblk(1,VCVOLE1  ) =   max(epsilc,cvole1(i,k,j))
!          cblk(1,VCVAPI1  ) =   0.
!          cblk(1,VCVAPI2  ) =   0.
!          cblk(1,VCVLIM1  ) =   0.
!          cblk(1,VCVLIM2  ) =   0.
      cblk(1,VCVAPI1  ) =   max(epsilc,cvapi1(i,k,j))
      cblk(1,VCVAPI2  ) =   max(epsilc,cvapi2(i,k,j))
      cblk(1,VCVLIM1  ) =   max(epsilc,cvlim1(i,k,j))
      cblk(1,VCVLIM2  ) =   max(epsilc,cvlim2(i,k,j))

!
! Set emissions to zero 
!
         epmcoarse(1) = 0.
         epm25i(1)    = 0.
         epm25j (1)   = 0.
         eeci_in      = 0.
         eecj_in      = 0.
         eorgi_in     = 0.
         eorgj_in     = 0.
         cblk(1,VSO4AJ   ) = chem(i,k,j,p_so4aj)
         cblk(1,VSO4AI   ) = chem(i,k,j,p_so4ai)
         cblk(1,VNO3AJ   ) = chem(i,k,j,p_no3aj)
         cblk(1,VNO3AI   ) = chem(i,k,j,p_no3ai)
         cblk(1,VNAAJ   )  = chem(i,k,j,p_naaj)
         cblk(1,VNAAI   )  = chem(i,k,j,p_naai)
         cblk(1,VCLAJ   )  = chem(i,k,j,p_claj)
         cblk(1,VCLAI   )  = chem(i,k,j,p_clai)
!        cblk(1,VCLAJ   )  = 0.
!        cblk(1,VCLAI   )  = 0.
!
! Set emissions to zero when above level kemit.
!
!     if( k > kemit ) then
!        epmcoarse(1) = 0.
!        epm25i(1)    = 0.
!        epm25j (1)   = 0.
!        eeci_in      = 0.
!        eecj_in      = 0.
!        eorgi_in     = 0.
!        eorgj_in     = 0.
!        cblk(1,VSO4AJ   ) = chem(i,k,j,p_so4aj)
!        cblk(1,VSO4AI   ) = chem(i,k,j,p_so4ai)
!        cblk(1,VNO3AJ   ) = chem(i,k,j,p_no3aj)
!        cblk(1,VNO3AI   ) = chem(i,k,j,p_no3ai)
!     else
!        epmcoarse(1) = emis_ant(i,k,j,p_e_pm_10)/dz8w(i,k,j)
!        epm25i(1)    = emis_ant(i,k,j,p_e_pm25i)/dz8w(i,k,j)
!        epm25j(1)    = emis_ant(i,k,j,p_e_pm25j)/dz8w(i,k,j)
!        eeci_in      = emis_ant(i,k,j,p_e_eci)/dz8w(i,k,j)
!        eecj_in      = emis_ant(i,k,j,p_e_ecj)/dz8w(i,k,j)
!        eorgi_in     = emis_ant(i,k,j,p_e_orgi)/dz8w(i,k,j)
!        eorgj_in     = emis_ant(i,k,j,p_e_orgj)/dz8w(i,k,j)
!        cblk(1,VSO4AJ   ) = chem(i,k,j,p_so4aj)+emis_ant(i,k,j,p_e_so4j)/dz8w(i,k,j)*dtstep
!        cblk(1,VSO4AI   ) = chem(i,k,j,p_so4ai)+emis_ant(i,k,j,p_e_so4i)/dz8w(i,k,j)*dtstep
!        cblk(1,VNO3AJ   ) = chem(i,k,j,p_no3aj)+emis_ant(i,k,j,p_e_no3j)/dz8w(i,k,j)*dtstep
!        cblk(1,VNO3AI   ) = chem(i,k,j,p_no3ai)+emis_ant(i,k,j,p_e_no3i)/dz8w(i,k,j)*dtstep
!     end if
!rs. nitrate, nh3, sulf
      cblk(1,vsulf) = vsulf_in
      cblk(1,vhno3) = nitrate_in
      cblk(1,vnh3) = nh3_in
      cblk(1,vhcl) = hcl_in
      cblk(1,VNH4AJ   ) =   chem(i,k,j,p_nh4aj)
      cblk(1,VNH4AI   ) =   chem(i,k,j,p_nh4ai)
      cblk(1,VNU0     ) =   max(1.e7,chem(i,k,j,p_nu0))
      cblk(1,VAC0     ) =   max(1.e7,chem(i,k,j,p_ac0))
      cblk(1,VCORN    ) =   chem(i,k,j,p_corn)


      if(debug_level.ge.1)then
     if(i.eq.its.and.j.eq.jts.and.k.eq.kts)then
        print*,'in a_mechanisms',i,j,k
        print*,'NSPCSDA, BLKSIZE',NSPCSDA, BLKSIZE
        print*,'k,DTA,PRES,TEMP,RELHUM',k,DTstep,10.*P(k),T(k),RH(k)
        print*,'nitrate_in, nh3_in, vsulf_in, so4rat_in', &
                nitrate_in, nh3_in, vsulf_in, so4rat_in
        print*,'drog_in,ldrog',drog_in,ldrog
        print*,'condvap_in,NCV,NACV',condvap_in,NCV,NACV
        print*,'eeci_in, eecj_in, eorgi_in, eorgj_in,convfac' &
            ,eeci_in, eecj_in, eorgi_in, eorgj_in,convfac
        print*,'CBLK',CBLK
       endif
    end if
      CALL rpmmod3(nspcsda,blksize,k,dtstep,10.*p(k),t(k),rh(k),nitrate_in,nh3_in, &
        vsulf_in,so4rat_in,drog_in,ldrog,condvap_in,ncv,nacv,eeci_in,eecj_in, &
        eorgi_in,eorgj_in,epm25i,epm25j,epmcoarse,soilrat_in,cblk,i,j,k)
      chem(i,k,j,p_so4aj) = cblk(1,VSO4AJ   )
      chem(i,k,j,p_so4ai) = cblk(1,VSO4AI   )
      chem(i,k,j,p_nh4aj) = cblk(1,VNH4AJ   )
      chem(i,k,j,p_nh4ai) = cblk(1,VNH4AI   )
      chem(i,k,j,p_no3aj) = cblk(1,VNO3AJ   )
      chem(i,k,j,p_no3ai) = cblk(1,VNO3AI   )
      chem(i,k,j,p_naaj) = cblk(1,VNAAJ   )
      chem(i,k,j,p_naai) = cblk(1,VNAAI   )

      chem(i,k,j,p_claj) = cblk(1,VCLAJ   )
      chem(i,k,j,p_clai) = cblk(1,VCLAI   )

      chem(i,k,j,p_orgaro1j) = cblk(1,VORGARO1J)
      chem(i,k,j,p_orgaro1i) = cblk(1,VORGARO1I)
      chem(i,k,j,p_orgaro2j) = cblk(1,VORGARO2J)
      chem(i,k,j,p_orgaro2i) = cblk(1,VORGARO2I)
      chem(i,k,j,p_orgalk1j) = cblk(1,VORGALK1J)
      chem(i,k,j,p_orgalk1i) = cblk(1,VORGALK1I)
      chem(i,k,j,p_orgole1j) = cblk(1,VORGOLE1J)
      chem(i,k,j,p_orgole1i) = cblk(1,VORGOLE1I)
      chem(i,k,j,p_orgba1j) = cblk(1,VORGBA1J )
      chem(i,k,j,p_orgba1i) = cblk(1,VORGBA1I )
      chem(i,k,j,p_orgba2j) = cblk(1,VORGBA2J )
      chem(i,k,j,p_orgba2i) = cblk(1,VORGBA2I )
      chem(i,k,j,p_orgba3j) = cblk(1,VORGBA3J )
      chem(i,k,j,p_orgba3i) = cblk(1,VORGBA3I )
      chem(i,k,j,p_orgba4j) = cblk(1,VORGBA4J )
      chem(i,k,j,p_orgba4i) = cblk(1,VORGBA4I )
      chem(i,k,j,p_orgpaj) = cblk(1,VORGPAJ  )
      chem(i,k,j,p_orgpai) = cblk(1,VORGPAI  )
      chem(i,k,j,p_ecj) = cblk(1,VECJ     )
      chem(i,k,j,p_eci) = cblk(1,VECI     )
      chem(i,k,j,p_p25j) = cblk(1,VP25AJ   )
      chem(i,k,j,p_p25i) = cblk(1,VP25AI   )
      chem(i,k,j,p_antha) =cblk(1,VANTHA   )
      chem(i,k,j,p_seas) = cblk(1,VSEAS    )
      chem(i,k,j,p_soila) = cblk(1,VSOILA   )
      chem(i,k,j,p_nu0) = max(1.e7,cblk(1,VNU0     ))
      chem(i,k,j,p_ac0) = max(1.e7,cblk(1,VAC0     ))
!     chem(i,k,j,p_ac0) = cblk(1,VAC0     )
      chem(i,k,j,p_corn) = cblk(1,VCORN    )
      h2oaj(i,k,j) = cblk(1,VH2OAJ   )
      h2oai(i,k,j) = cblk(1,VH2OAI   )
      nu3(i,k,j) = cblk(1,VNU3     )
      ac3(i,k,j) = cblk(1,VAC3     )
      cor3(i,k,j) = cblk(1,VCOR3    )
      cvaro1(i,k,j) = cblk(1,VCVARO1  )
      cvaro2(i,k,j) = cblk(1,VCVARO2  )
      cvalk1(i,k,j) = cblk(1,VCVALK1  )
      cvole1(i,k,j) = cblk(1,VCVOLE1  )
!          cvapi1(i,k,j) = 0.
!          cvapi2(i,k,j) = 0.
!          cvlim1(i,k,j) = 0.
!          cvlim2(i,k,j) = 0.
      cvapi1(i,k,j) = cblk(1,VCVAPI1  )
      cvapi2(i,k,j) = cblk(1,VCVAPI2  )
      cvlim1(i,k,j) = cblk(1,VCVLIM1  )
      cvlim2(i,k,j) = cblk(1,VCVLIM2  )

      chem(i,k,j,p_sulf)=max(epsilc,cblk(1,vsulf)/CONVFAC/MWSO4)
      chem(i,k,j,p_hno3)=max(epsilc,cblk(1,vhno3)/CONVFAC/MWHNO3)
      chem(i,k,j,p_nh3)=max(epsilc,cblk(1,vnh3)/CONVFAC/MWNH3)
      chem(i,k,j,p_hcl)=max(epsilc,cblk(1,vhcl)/CONVFAC/MWHCL)
!     if(i.eq.28.and.j.eq.25.and.k.eq.1)then
!       print *,vhcl
!       print *,'vsulfout = ',chem(i,k,j,p_sulf)
!       print *,'nitrateout = ',chem(i,k,j,p_hno3)
!       print *,'nh3out = ',chem(i,k,j,p_nh3)
!       print *,'hclout = ',cblk(1,vhcl)/CONVFAC/MWHCL
!       print *,'pso4ai = ',chem(i,k,j,p_so4aj),chem(i,k,j,p_no3aj),chem(i,k,j,p_nh4aj),chem(i,k,j,p_naaj)
!     endif
      enddo         ! k-loop
 100  continue      ! i,j-loop

!
! convert aerosol variables back to mixing ratio from ug/m3
!
      do l=p_so4aj,num_chem
         do j=jts,jte
            do k=kts,kte
               do i=its,ite
                  chem(i,k,j,l)=max(epsilc,chem(i,k,j,l)*alt(i,k,j))
               enddo
            enddo
         enddo
      enddo

    END SUBROUTINE sorgam_driver
! ///////////////////////////////////////////////////
    SUBROUTINE sum_pm_sorgam (                                         &
         alt, chem, h2oaj, h2oai,                                      &
         pm2_5_dry, pm2_5_water, pm2_5_dry_ec, pm10,                   &
         dust_opt,ids,ide, jds,jde, kds,kde,                                    &
         ims,ime, jms,jme, kms,kme,                                    &
         its,ite, jts,jte, kts,kte                                     )

   INTEGER,      INTENT(IN   )    ::  dust_opt,                        &
                                      ids,ide, jds,jde, kds,kde,       &
                                      ims,ime, jms,jme, kms,kme,       &
                                      its,ite, jts,jte, kts,kte

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) :: chem

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN ) :: alt,h2oaj,h2oai

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(OUT) :: pm2_5_dry,pm2_5_water,pm2_5_dry_ec,pm10

   INTEGER :: i,ii,j,jj,k,n
!
! sum up pm2_5 and pm10 output
!
      pm2_5_dry(its:ite, kts:kte, jts:jte)    = 0.
      pm2_5_water(its:ite, kts:kte, jts:jte)  = 0.
      pm2_5_dry_ec(its:ite, kts:kte, jts:jte) = 0.
      do j=jts,jte
         jj=min(jde-1,j)
      do k=kts,kte
      do i=its,ite
         ii=min(ide-1,i)
!        if(dust_opt == 1 ) then
! first add gocart dust and seasalt (aqmeii)
!        pm2_5_dry(i,k,j) = pm2_5_dry(i,k,j)+chem(ii,k,jj,p_dust_1)+ &
!                              .286*chem(ii,k,jj,p_dust_2) +            &
!                              chem(ii,k,jj,p_seas_1) +                 &
!                              .942*chem(ii,k,jj,p_seas_2)
!        endif
!
         do n=p_so4aj,p_p25i
            pm2_5_dry(i,k,j) = pm2_5_dry(i,k,j)+chem(ii,k,jj,n)
         enddo
         if( p_p25cwi .gt. p_p25i) then
         do n=p_so4cwj,p_p25cwi
            pm2_5_dry(i,k,j) = pm2_5_dry(i,k,j)+chem(ii,k,jj,n)
         enddo
         endif
         pm2_5_dry_ec(i,k,j) = pm2_5_dry_ec(i,k,j)+chem(ii,k,jj,p_ecj) &
                               + chem(ii,k,jj,p_eci)
         pm2_5_water(i,k,j) =  pm2_5_water(i,k,j)+h2oaj(i,k,j)       &
                               + h2oai(i,k,j)

         !Convert the units from mixing ratio to concentration (ug m^-3)
         pm2_5_dry(i,k,j)    = pm2_5_dry(i,k,j) / alt(ii,k,jj)
         pm2_5_dry_ec(i,k,j) = pm2_5_dry_ec(i,k,j) / alt(ii,k,jj)
         pm2_5_water(i,k,j)  = pm2_5_water(i,k,j) / alt(ii,k,jj)
      enddo
      enddo
      enddo
      do j=jts,jte
         jj=min(jde-1,j)
         do k=kts,kte
            do i=its,ite
               ii=min(ide-1,i)
!              if(dust_opt == 1 ) then
!              pm10(i,k,j) = pm2_5_dry(i,k,j)                       &
!                          + ( chem(ii,k,jj,p_antha)               &
!                          + .714*chem(ii,k,jj,p_dust_2)                 &
!                          + chem(ii,k,jj,p_dust_3)                 &
!                          + .058*chem(ii,k,jj,p_seas_2)                 &
!                          + chem(ii,k,jj,p_seas_3) ) / alt(ii,k,jj)
!              else
               pm10(i,k,j) = pm2_5_dry(i,k,j)                       &
                           + ( chem(ii,k,jj,p_antha)               &
                           + chem(ii,k,jj,p_soila)                 &
                           + chem(ii,k,jj,p_seas) ) / alt(ii,k,jj)
               if( p_p25cwi .gt. p_p25i) then
               pm10(i,k,j) = pm10(i,k,j)                       &
                           + ( chem(ii,k,jj,p_anthcw)               &
                           + chem(ii,k,jj,p_soilcw)                 &
                           + chem(ii,k,jj,p_seascw) ) / alt(ii,k,jj)
               endif
!              endif
            enddo
         enddo
      enddo
    END SUBROUTINE sum_pm_sorgam
! ///////////////////////////////////////////////////
    SUBROUTINE sorgam_depdriver (id,config_flags,ktau,dtstep,           &
               ust,t_phy,moist,p8w,t8w,rmol,znt,pbl,                    &
               alt,p_phy,chem,rho_phy,dz8w,z,z_at_w,                    &
               h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2, &
               cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,               &
               aer_res,vgsa,                                            &
               numgas,ddflx, &
               numaer,                                                  &
               ids,ide, jds,jde, kds,kde,                               &
               ims,ime, jms,jme, kms,kme,                               &
               its,ite, jts,jte, kts,kte                                )

   USE module_configure,only:  grid_config_rec_type
   TYPE (grid_config_rec_type) , INTENT (IN) :: config_flags
   INTEGER,      INTENT(IN   )    ::                             &
                                      numgas, &
                                      numaer,                    &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                      its,ite, jts,jte, kts,kte, &
                                      id,ktau

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),        &
         INTENT(IN ) ::                                   moist
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),         &
         INTENT(INOUT ) ::                                   chem
!
! following are aerosol arrays that are not advected
!
   REAL, DIMENSION( its:ite, jts:jte, numaer ),                       &
         INTENT(INOUT ) ::                                             &
         vgsa
        real, intent(inout),   &
                dimension( ims:ime, jms:jme, numgas+1:num_chem ) :: &
                ddflx

   REAL, DIMENSION( its:ite, jts:jte ),                       &
         INTENT(INOUT ) ::                                             &
         aer_res

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(INOUT ) ::                                             &
           h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2,    &
           cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          INTENT(IN   ) ::                                      &
                                                      t_phy,    &
                                                      alt,      &
                                                      p_phy,    &
                                                      dz8w,     &
                                                      z    ,    &
                                       t8w,p8w,z_at_w ,  &
                                                    rho_phy
   REAL,  DIMENSION( ims:ime ,  jms:jme )         ,    &
          INTENT(IN   ) ::                                      &
               ust,rmol, pbl, znt
      REAL,      INTENT(IN   ) ::                               &
                             dtstep
                                                                                               
      REAL rgas
      DATA rgas/8.314510/
      REAL convfac,convfac2
!...BLKSIZE set to one in column model ciarev02

      INTEGER blksize
      PARAMETER (blksize=1)

!...number of aerosol species
!  number of species (gas + aerosol)
      INTEGER nspcsda
      PARAMETER (nspcsda=l1ae) !bs
! (internal aerosol dynamics)
!bs # of anth. cond. vapors in SORGAM
      INTEGER nacv
      PARAMETER (nacv=lcva) !bs # of anth. cond. vapors in CTM
!bs total # of cond. vapors in SORGAM
      INTEGER ncv
      PARAMETER (ncv=lspcv) !bs
!bs total # of cond. vapors in CTM
      REAL cblk(blksize,nspcsda) ! main array of variables
                                   ! particles [ug/m^3/s]
      REAL soilrat_in
                    ! emission rate of soil derived coars
                    ! input HNO3 to CBLK [ug/m^3]
      REAL nitrate_in
                    ! input NH3 to CBLK  [ug/m^3]
      REAL nh3_in
!                   ! input hcl vapor
      REAL hcl_in
                    ! input SO4 vapor    [ug/m^3]
      REAL vsulf_in

      REAL so4rat_in
                    ! input SO4 formation[ug/m^3/sec]
                    ! pressure in cb
      REAL pres
                    ! temperature in K
      REAL temp
                    !bs
      REAL relhum
                    ! rel. humidity (0,1)   
      REAL ::p(kts:kte),t(kts:kte),rh(kts:kte)

!...molecular weights                   ciarev02

! molecular weight for SO4
      REAL mwso4
      PARAMETER (mwso4=96.0576)

! molecular weight for HNO3
      REAL mwhno3
      PARAMETER (mwhno3=63.01287)

! molecular weight for NH3
      REAL mwnh3
      PARAMETER (mwnh3=17.03061)

!molecular weight for HCL 
      REAL mwhcl
      PARAMETER (mwhcl=36.46100)

!bs molecular weight for Organic Spec
!     REAL mworg
!     PARAMETER (mworg=175.0)

!bs molecular weight for Elemental Ca
      REAL mwec
      PARAMETER (mwec=12.0)

!rs molecular weight
      REAL mwaro1
      PARAMETER (mwaro1=150.0)

!rs molecular weight
      REAL mwaro2
      PARAMETER (mwaro2=150.0)

!rs molecular weight
      REAL mwalk1
      PARAMETER (mwalk1=140.0)

!rs molecular weight
      REAL mwalk2
      PARAMETER (mwalk2=140.0)

!rs molecular weight
!rs molecular weight
      REAL mwole1
      PARAMETER (mwole1=140.0)

!rs molecular weight
      REAL mwapi1
      PARAMETER (mwapi1=200.0)

!rs molecular weight
      REAL mwapi2
      PARAMETER (mwapi2=200.0)

!rs molecular weight
      REAL mwlim1
      PARAMETER (mwlim1=200.0)

!rs molecular weight
      REAL mwlim2
      PARAMETER (mwlim2=200.0)
      INTEGER NUMCELLS  ! actual number of cells in arrays ( default is 1 in box model)
!ia                       kept to 1 in current version of column model

       PARAMETER( NUMCELLS = 1)


      REAL RA(BLKSIZE )             ! aerodynamic resistance [ s m**-1 ]
      REAL USTAR( BLKSIZE )         ! surface friction velocity [ m s**-1 ]
      REAL WSTAR( BLKSIZE )         ! convective velocity scale [ m s**-1 ]
      REAL PBLH( BLKSIZE )          ! PBL height (m)
      REAL ZNTT( BLKSIZE )          ! Surface roughness length (m)
      REAL RMOLM( BLKSIZE )         ! Inverse of Monin-Obukhov length (1/m)

      REAL BLKPRS(BLKSIZE)         ! pressure in cb
      REAL BLKTA(BLKSIZE)          ! temperature in K
      REAL BLKDENS(BLKSIZE)        ! Air density in kg/m3

!
! *** OUTPUT:
!     
! *** atmospheric properties
      
      REAL XLM( BLKSIZE )           ! atmospheric mean free path [ m ]
      REAL AMU( BLKSIZE )           ! atmospheric dynamic viscosity [ kg/m s ]
      
! *** followng is for future version       
      REAL VSED( BLKSIZE, NASPCSSED) ! sedimentation velocity [ m s**-1 ]
      REAL VDEP( BLKSIZE, NASPCSDEP) ! deposition velocity [ m s**-1 ]

! *** modal diameters: [ m ]
      
      REAL DGNUC( BLKSIZE )         ! nuclei mode geometric mean diameter  [ m ]
      REAL DGACC( BLKSIZE )         ! accumulation geometric mean diameter [ m ]
      REAL DGCOR( BLKSIZE )         ! coarse mode geometric mean diameter  [ m ]
      

! *** aerosol properties: 

! *** Modal mass concentrations [ ug m**3 ]
      
      REAL PMASSN( BLKSIZE )        ! mass concentration in Aitken mode
      REAL PMASSA( BLKSIZE )        ! mass concentration in accumulation mode
      REAL PMASSC( BLKSIZE )        ! mass concentration in coarse mode

! *** average modal particle densities  [ kg/m**3 ]

      REAL PDENSN( BLKSIZE )        ! average particle density in nuclei mode
      REAL PDENSA( BLKSIZE )        ! average particle density in accumulation mode
      REAL PDENSC( BLKSIZE )        ! average particle density in coarse mode

! *** average modal Knudsen numbers

      REAL KNNUC ( BLKSIZE )        ! nuclei mode  Knudsen number
      REAL KNACC ( BLKSIZE )        ! accumulation Knudsen number
      REAL KNCOR ( BLKSIZE )        ! coarse mode  Knudsen number


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   INTEGER :: i,j,k,l
!
!     print *,'in sorgdepdriver ',its,ite,jts,jte
      do l=1,numaer
      do i=its,ite
      do j=jts,jte
      vgsa(i,j,l)=0.
      enddo
      enddo
      enddo
      vdep=0.
      do 100 j=jts,jte
         do 100 i=its,ite
            cblk=epsilc
            do k=kts,kte
               t(k) = t_phy(i,k,j)
               p(k) = .001*p_phy(i,k,j)
               rh(k) = MIN( 100.,100. * moist(i,k,j,p_qv) / &
               (3.80*exp(17.27*(t_phy(i,k,j)-273.)/ &
               (t_phy(i,k,j)-36.))/(.01*p_phy(i,k,j))))
               rh(k)=max(.05,0.01*rh(k))
            enddo
!           do k=kts,kte
            k=kts
               convfac = p(k)/rgas/t(k)*1000.
               nitrate_in =chem(i,k,j,p_hno3)*convfac*mwhno3
               nh3_in = chem(i,k,j,p_nh3)*convfac*mwnh3
               vsulf_in = chem(i,k,j,p_sulf)*convfac*mwso4   
               hcl_in = chem(i,k,j,p_hcl)*convfac*mwhcl              
 
!rs. nitrate, nh3, sulf
      BLKPRS(BLKSIZE)   = 1.e3*P(K)                ! pressure in Pa
      BLKTA(BLKSIZE)   = T(K)         ! temperature in K
      USTAR(BLKSIZE) = max(1.e-1,UST(i,j))
      WSTAR(BLKSIZE) = 0.
      pblh(blksize) = pbl(i,j)
      zntt(blksize) = znt(i,j)
      rmolm(blksize)= rmol(i,j)
      convfac2=1./alt(i,k,j)
      BLKDENS(BLKSIZE)=convfac2
      cblk(1,vsulf) = max(epsilc,vsulf_in)
      cblk(1,vhno3) = max(epsilc,nitrate_in)
      cblk(1,vnh3) = max(epsilc,nh3_in)
      cblk(1,vhcl) = max(epsilc,hcl_in)
      cblk(1,VSO4AJ   ) =   max(epsilc,chem(i,k,j,p_so4aj)*convfac2)
      cblk(1,VSO4AI   ) =   max(epsilc,chem(i,k,j,p_so4ai)*convfac2)
      cblk(1,VNH4AJ   ) =   max(epsilc,chem(i,k,j,p_nh4aj)*convfac2)
      cblk(1,VNH4AI   ) =   max(epsilc,chem(i,k,j,p_nh4ai)*convfac2)
      cblk(1,VNO3AJ   ) =   max(epsilc,chem(i,k,j,p_no3aj)*convfac2)
      cblk(1,VNO3AI   ) =   max(epsilc,chem(i,k,j,p_no3ai)*convfac2)
      if (p_naai >= param_first_scalar) &
         cblk(1,VNAAI ) =   max(epsilc,chem(i,k,j,p_naai)*convfac2)
      if (p_naaj >= param_first_scalar) &
         cblk(1,VNAAJ ) =   max(epsilc,chem(i,k,j,p_naaj)*convfac2)
      if (p_clai >= param_first_scalar) &
         cblk(1,VCLAI ) =   max(epsilc,chem(i,k,j,p_clai)*convfac2)
      if (p_claj >= param_first_scalar) &
         cblk(1,VCLAJ ) =   max(epsilc,chem(i,k,j,p_claj)*convfac2)
      cblk(1,VORGARO1J) =   max(epsilc,chem(i,k,j,p_orgaro1j)*convfac2)
      cblk(1,VORGARO1I) =   max(epsilc,chem(i,k,j,p_orgaro1i)*convfac2)
      cblk(1,VORGARO2J) =   max(epsilc,chem(i,k,j,p_orgaro2j)*convfac2)
      cblk(1,VORGARO2I) =   max(epsilc,chem(i,k,j,p_orgaro2i)*convfac2)
      cblk(1,VORGALK1J) =   max(epsilc,chem(i,k,j,p_orgalk1j)*convfac2)
      cblk(1,VORGALK1I) =   max(epsilc,chem(i,k,j,p_orgalk1i)*convfac2)
      cblk(1,VORGOLE1J) =   max(epsilc,chem(i,k,j,p_orgole1j)*convfac2)
      cblk(1,VORGOLE1I) =   max(epsilc,chem(i,k,j,p_orgole1i)*convfac2)
      cblk(1,VORGBA1J ) =   max(epsilc,chem(i,k,j,p_orgba1j)*convfac2)
      cblk(1,VORGBA1I ) =   max(epsilc,chem(i,k,j,p_orgba1i)*convfac2)
      cblk(1,VORGBA2J ) =   max(epsilc,chem(i,k,j,p_orgba2j)*convfac2)
      cblk(1,VORGBA2I ) =   max(epsilc,chem(i,k,j,p_orgba2i)*convfac2)
      cblk(1,VORGBA3J ) =   max(epsilc,chem(i,k,j,p_orgba3j)*convfac2)
      cblk(1,VORGBA3I ) =   max(epsilc,chem(i,k,j,p_orgba3i)*convfac2)
      cblk(1,VORGBA4J ) =   max(epsilc,chem(i,k,j,p_orgba4j)*convfac2)
      cblk(1,VORGBA4I ) =   max(epsilc,chem(i,k,j,p_orgba4i)*convfac2)
      cblk(1,VORGPAJ  ) =   max(epsilc,chem(i,k,j,p_orgpaj)*convfac2)
      cblk(1,VORGPAI  ) =   max(epsilc,chem(i,k,j,p_orgpai)*convfac2)
      cblk(1,VECJ     ) =   max(epsilc,chem(i,k,j,p_ecj)*convfac2)
      cblk(1,VECI     ) =   max(epsilc,chem(i,k,j,p_eci)*convfac2)
      cblk(1,VP25AJ   ) =   max(epsilc,chem(i,k,j,p_p25j)*convfac2)
      cblk(1,VP25AI   ) =   max(epsilc,chem(i,k,j,p_p25i)*convfac2)
      cblk(1,VANTHA   ) =   max(epsilc,chem(i,k,j,p_antha)*convfac2)
      cblk(1,VSEAS    ) =   max(epsilc,chem(i,k,j,p_seas)*convfac2)
      cblk(1,VSOILA   ) =   max(epsilc,chem(i,k,j,p_soila)*convfac2)
      cblk(1,VNU0     ) =   max(epsilc,chem(i,k,j,p_nu0)*convfac2)
      cblk(1,VAC0     ) =   max(epsilc,chem(i,k,j,p_ac0)*convfac2)
      cblk(1,VCORN    ) =   max(epsilc,chem(i,k,j,p_corn)*convfac2)
      cblk(1,VH2OAJ   ) =   h2oaj(i,k,j)
      cblk(1,VH2OAI   ) =   h2oai(i,k,j)
      cblk(1,VNU3     ) =   nu3(i,k,j)
      cblk(1,VAC3     ) =   ac3(i,k,j)
      cblk(1,VCOR3    ) =   cor3(i,k,j)
      cblk(1,VCVARO1  ) =   cvaro1(i,k,j)
      cblk(1,VCVARO2  ) =   cvaro2(i,k,j)
      cblk(1,VCVALK1  ) =   cvalk1(i,k,j)
      cblk(1,VCVOLE1  ) =   cvole1(i,k,j)
!          cblk(1,VCVAPI1  ) =   0.
!          cblk(1,VCVAPI2  ) =   0.
!          cblk(1,VCVLIM1  ) =   0.
!          cblk(1,VCVLIM2  ) =   0.
      cblk(1,VCVAPI1  ) =   cvapi1(i,k,j)
      cblk(1,VCVAPI2  ) =   cvapi2(i,k,j)
      cblk(1,VCVLIM1  ) =   cvlim1(i,k,j)
      cblk(1,VCVLIM2  ) =   cvlim2(i,k,j)
!                                                                     
!rs.   get size distribution information                              
!                                                                     
!       if(i.eq.126.and.j.eq.99)then
!          print *,'in modpar ',i,j
!          print *,cblk,BLKTA,BLKPRS,USTAR
!          print *,'BLKSIZE, NSPCSDA, NUMCELLS'
!          print *,BLKSIZE, NSPCSDA, NUMCELLS
!          print *,'XLM, AMU,PDENSN, PDENSA, PDENSC'
!          print *,XLM, AMU,PDENSN, PDENSA, PDENSC
!          print *,'chem(i,k,j,p_orgpaj),chem(i,k,j,p_orgpai)',p_orgpaj,p_orgpai
!          print *,chem(i,k,j,p_orgpaj),chem(i,k,j,p_orgpai)
!       endif

        CALL MODPAR(  BLKSIZE, NSPCSDA, NUMCELLS,             &
             CBLK,                                            &
             BLKTA, BLKPRS,                                   &
             PMASSN, PMASSA, PMASSC,                          &
             PDENSN, PDENSA, PDENSC,                          &
             XLM, AMU,                                        &
             DGNUC, DGACC, DGCOR,                             &
             KNNUC, KNACC,KNCOR    )                                   
!       print *,'out modpar ',i,j
        if (config_flags%aer_drydep_opt == 11) then
        CALL VDVG(  BLKSIZE, NSPCSDA, NUMCELLS,k,CBLK, &
             BLKTA, BLKDENS, AER_RES(I,j), USTAR, WSTAR,  AMU,   &   
             DGNUC, DGACC, DGCOR,                      &
             KNNUC, KNACC,KNCOR,                       &
             PDENSN, PDENSA, PDENSC,                   &
             VSED, VDEP )                                             
        else
        CALL VDVG_2(  BLKSIZE, NSPCSDA, NUMCELLS,k,CBLK, &
             BLKTA, BLKDENS, AER_RES(I,j), USTAR, PBLH,&
             ZNTT, RMOLM, AMU, DGNUC, DGACC, DGCOR,XLM,&
             KNNUC, KNACC,KNCOR,                       &
             PDENSN, PDENSA, PDENSC,                   &
             VSED, VDEP )
        endif

        VGSA(i, j, VSO4AJ   )  =  VDEP(1, VDMACC )
        VGSA(i, j, VSO4AI   )  =  VDEP(1, VDMNUC )
        VGSA(i, j, VNH4AJ   )  =  VGSA(i, j, VSO4AJ )
        VGSA(i, j, VNH4AI   )  =  VGSA(i, j, VSO4AI )
        VGSA(i, j, VNO3AJ   )  =  VGSA(i, j, VSO4AJ )
        VGSA(i, j, VNO3AI   )  =  VGSA(i, j, VSO4AI )
        if (p_naai >= param_first_scalar) VGSA(i, j, VNAAI )  =  VGSA(i, j, VSO4AI )
        if (p_naaj >= param_first_scalar) VGSA(i, j, VNAAJ )  =  VGSA(i, j, VSO4AJ )
        if (p_clai >= param_first_scalar) VGSA(i, j, VCLAI )  =  VGSA(i, j, VSO4AI )
        if (p_claj >= param_first_scalar) VGSA(i, j, VCLAJ )  =  VGSA(i, j, VSO4AJ )
        VGSA(i, j, VORGARO1J)  =  VGSA(i, j, VSO4AJ )
        VGSA(i, j, VORGARO1I)  =  VGSA(i, j, VSO4AI )
        VGSA(i, j, VORGARO2J)  =  VGSA(i, j, VSO4AJ )
        VGSA(i, j, VORGARO2I)  =  VGSA(i, j, VSO4AI )
        VGSA(i, j, VORGALK1J)  =  VGSA(i, j, VSO4AJ )
        VGSA(i, j, VORGALK1I)  =  VGSA(i, j, VSO4AI )
        VGSA(i, j, VORGOLE1J)  =  VGSA(i, j, VSO4AJ )
        VGSA(i, j, VORGOLE1I)  =  VGSA(i, j, VSO4AI )
        VGSA(i, j, VORGBA1J )  =  VGSA(i, j, VSO4AJ )
        VGSA(i, j, VORGBA1I )  =  VGSA(i, j, VSO4AI )
        VGSA(i, j, VORGBA2J )  =  VGSA(i, j, VSO4AJ )
        VGSA(i, j, VORGBA2I )  =  VGSA(i, j, VSO4AI )
        VGSA(i, j, VORGBA3J )  =  VGSA(i, j, VSO4AJ )
        VGSA(i, j, VORGBA3I )  =  VGSA(i, j, VSO4AI )
        VGSA(i, j, VORGBA4J )  =  VGSA(i, j, VSO4AJ )
        VGSA(i, j, VORGBA4I )  =  VGSA(i, j, VSO4AI )
        VGSA(i, j, VORGPAJ  )  =  VGSA(i, j, VSO4AJ )
        VGSA(i, j, VORGPAI  )  =  VGSA(i, j, VSO4AI )
        VGSA(i, j, VECJ     )  =  VGSA(i, j, VSO4AJ )
        VGSA(i, j, VECI     )  =  VGSA(i, j, VSO4AI )
        VGSA(i, j, VP25AJ   )  =  VGSA(i, j, VSO4AJ )
        VGSA(i, j, VP25AI   )  =  VGSA(i, j, VSO4AI )
        VGSA(i, j, VANTHA   )  =  VDEP(1, VDMCOR )
        VGSA(i, j, VSEAS    )  =  VGSA(i, j, VANTHA )
        VGSA(i, j, VSOILA   )  =  VGSA(i, j, VANTHA )
        VGSA(i, j, VNU0     )  =  VDEP(1, VDNNUC )
        VGSA(i, j, VAC0     )  =  VDEP(1, VDNACC )
        VGSA(i, j, VCORN    )  =  VDEP(1, VDNCOR )
        if( config_flags%diagnostic_dep == 1) then
        ddflx(i,j,p_so4aj)=ddflx(i,j,p_so4aj)+chem(i,k,j,p_so4aj)/alt(i,k,j)*VGSA(i,j,VSO4AJ)*dtstep
        ddflx(i,j,p_so4ai)=ddflx(i,j,p_so4ai)+chem(i,k,j,p_so4ai)/alt(i,k,j)*VGSA(i,j,VSO4AI)*dtstep
        ddflx(i,j,p_nh4aj)=ddflx(i,j,p_nh4aj)+chem(i,k,j,p_nh4aj)/alt(i,k,j)*VGSA(i,j,VNH4AJ)*dtstep
        ddflx(i,j,p_nh4ai)=ddflx(i,j,p_nh4ai)+chem(i,k,j,p_nh4ai)/alt(i,k,j)*VGSA(i,j,VNH4Ai)*dtstep
        ddflx(i,j,p_no3aj)=ddflx(i,j,p_no3aj)+chem(i,k,j,p_no3aj)/alt(i,k,j)*VGSA(i,j,VNO3AJ)*dtstep
        ddflx(i,j,p_no3ai)=ddflx(i,j,p_no3ai)+chem(i,k,j,p_no3ai)/alt(i,k,j)*VGSA(i,j,VNO3AI)*dtstep
        ddflx(i,j,p_orgaro1j)=ddflx(i,j,p_orgaro1j)+chem(i,k,j,p_orgaro1j)/alt(i,k,j)*VGSA(i,j,VORGARO1J)*dtstep
        ddflx(i,j,p_orgaro1i)=ddflx(i,j,p_orgaro1i)+chem(i,k,j,p_orgaro1i)/alt(i,k,j)*VGSA(i,j,VORGARO1I)*dtstep
        ddflx(i,j,p_orgaro2j)=ddflx(i,j,p_orgaro2j)+chem(i,k,j,p_orgaro2j)/alt(i,k,j)*VGSA(i,j,VORGARO2J)*dtstep
        ddflx(i,j,p_orgaro2i)=ddflx(i,j,p_orgaro2i)+chem(i,k,j,p_orgaro2i)/alt(i,k,j)*VGSA(i,j,VORGARO2I)*dtstep
        ddflx(i,j,p_orgalk1j)=ddflx(i,j,p_orgalk1j)+chem(i,k,j,p_orgalk1j)/alt(i,k,j)*VGSA(i,j,VORGALK1J)*dtstep
        ddflx(i,j,p_orgalk1i)=ddflx(i,j,p_orgalk1i)+chem(i,k,j,p_orgalk1i)/alt(i,k,j)*VGSA(i,j,VORGALK1I)*dtstep
        ddflx(i,j,p_orgole1j)=ddflx(i,j,p_orgole1j)+chem(i,k,j,p_orgole1j)/alt(i,k,j)*VGSA(i,j,VORGOLE1J)*dtstep
        ddflx(i,j,p_orgole1i)=ddflx(i,j,p_orgole1i)+chem(i,k,j,p_orgole1i)/alt(i,k,j)*VGSA(i,j,VORGOLE1I)*dtstep
        ddflx(i,j,p_orgba1j)=ddflx(i,j,p_orgba1j)+chem(i,k,j,p_orgba1j)/alt(i,k,j)*VGSA(i,j,VORGBA1J)*dtstep
        ddflx(i,j,p_orgba1i)=ddflx(i,j,p_orgba1i)+chem(i,k,j,p_orgba1i)/alt(i,k,j)*VGSA(i,j,VORGBA1I)*dtstep
        ddflx(i,j,p_orgba2j)=ddflx(i,j,p_orgba2j)+chem(i,k,j,p_orgba2j)/alt(i,k,j)*VGSA(i,j,VORGBA2J)*dtstep
        ddflx(i,j,p_orgba2i)=ddflx(i,j,p_orgba2i)+chem(i,k,j,p_orgba2i)/alt(i,k,j)*VGSA(i,j,VORGBA2I)*dtstep
        ddflx(i,j,p_orgba3j)=ddflx(i,j,p_orgba3j)+chem(i,k,j,p_orgba3j)/alt(i,k,j)*VGSA(i,j,VORGBA3J)*dtstep
        ddflx(i,j,p_orgba3i)=ddflx(i,j,p_orgba3i)+chem(i,k,j,p_orgba3i)/alt(i,k,j)*VGSA(i,j,VORGBA3I)*dtstep
        ddflx(i,j,p_orgba4j)=ddflx(i,j,p_orgba4j)+chem(i,k,j,p_orgba4j)/alt(i,k,j)*VGSA(i,j,VORGBA4J)*dtstep
        ddflx(i,j,p_orgba4i)=ddflx(i,j,p_orgba4i)+chem(i,k,j,p_orgba4i)/alt(i,k,j)*VGSA(i,j,VORGBA4I)*dtstep
        ddflx(i,j,p_orgpaj)=ddflx(i,j,p_orgpaj)+chem(i,k,j,p_orgpaj)/alt(i,k,j)*VGSA(i,j,VORGPAJ)*dtstep
        ddflx(i,j,p_orgpai)=ddflx(i,j,p_orgpai)+chem(i,k,j,p_orgpai)/alt(i,k,j)*VGSA(i,j,VORGPAI)*dtstep
        ddflx(i,j,p_ecj)=ddflx(i,j,p_ecj)+chem(i,k,j,p_ecj)/alt(i,k,j)*VGSA(i,j,VECJ)*dtstep
        ddflx(i,j,p_eci)=ddflx(i,j,p_eci)+chem(i,k,j,p_eci)/alt(i,k,j)*VGSA(i,j,VECI)*dtstep
        ddflx(i,j,p_p25j)=ddflx(i,j,p_p25j)+chem(i,k,j,p_p25j)/alt(i,k,j)*VGSA(i,j,VP25AJ)*dtstep
        ddflx(i,j,p_p25i)=ddflx(i,j,p_p25i)+chem(i,k,j,p_p25i)/alt(i,k,j)*VGSA(i,j,VP25AI)*dtstep
        ddflx(i,j,p_naaj)=ddflx(i,j,p_naaj)+chem(i,k,j,p_naaj)/alt(i,k,j)*VGSA(i,j,VNAAJ)*dtstep
        ddflx(i,j,p_naai)=ddflx(i,j,p_naai)+chem(i,k,j,p_naai)/alt(i,k,j)*VGSA(i,j,VNAAI)*dtstep
        ddflx(i,j,p_claj)=ddflx(i,j,p_claj)+chem(i,k,j,p_claj)/alt(i,k,j)*VGSA(i,j,VCLAJ)*dtstep
        ddflx(i,j,p_clai)=ddflx(i,j,p_clai)+chem(i,k,j,p_clai)/alt(i,k,j)*VGSA(i,j,VCLAI)*dtstep
        ddflx(i,j,p_antha)=ddflx(i,j,p_antha)+chem(i,k,j,p_antha)/alt(i,k,j)*VGSA(i,j,VANTHA)*dtstep
        ddflx(i,j,p_seas)=ddflx(i,j,p_seas)+chem(i,k,j,p_seas)/alt(i,k,j)*VGSA(i,j,VSEAS)*dtstep
        ddflx(i,j,p_soila)=ddflx(i,j,p_soila)+chem(i,k,j,p_soila)/alt(i,k,j)*VGSA(i,j,VSOILA)*dtstep
        ddflx(i,j,p_nu0)=ddflx(i,j,p_nu0)+chem(i,k,j,p_nu0)/alt(i,k,j)*VGSA(i,j,VNU0)*dtstep
        ddflx(i,j,p_ac0)=ddflx(i,j,p_ac0)+chem(i,k,j,p_ac0)/alt(i,k,j)*VGSA(i,j,VAC0)*dtstep
        ddflx(i,j,p_corn)=ddflx(i,j,p_corn)+chem(i,k,j,p_corn)/alt(i,k,j)*VGSA(i,j,VCORN)*dtstep
        end if

!     enddo         ! k-loop
 100  continue      ! i,j-loop
                                                                     
END SUBROUTINE sorgam_depdriver
! ///////////////////////////////////////////////////
    SUBROUTINE actcof(cat,an,gama,molnu,phimult)

!-----------------------------------------------------------------------

! DESCRIPTION:

!  This subroutine computes the activity coefficients of (2NH4+,SO4--),
!  (NH4+,NO3-),(2H+,SO4--),(H+,NO3-),AND (H+,HSO4-) in aqueous
!  multicomponent solution, using Bromley's model and Pitzer's method.

! REFERENCES:

!   Bromley, L.A. (1973) Thermodynamic properties of strong electrolytes
!     in aqueous solutions.  AIChE J. 19, 313-320.

!   Chan, C.K. R.C. Flagen, & J.H.  Seinfeld (1992) Water Activities of
!     NH4NO3 / (NH4)2SO4 solutions, Atmos. Environ. (26A): 1661-1673.

!   Clegg, S.L. & P. Brimblecombe (1988) Equilibrium partial pressures
!     of strong acids over saline solutions - I HNO3,
!     Atmos. Environ. (22): 91-100

!   Clegg, S.L. & P. Brimblecombe (1990) Equilibrium partial pressures
!     and mean activity and osmotic coefficients of 0-100% nitric acid
!     as a function of temperature,   J. Phys. Chem (94): 5369 - 5380

!   Pilinis, C. and J.H. Seinfeld (1987) Continued development of a
!     general equilibrium model for inorganic multicomponent atmospheric
!     aerosols.  Atmos. Environ. 21(11), 2453-2466.




! ARGUMENT DESCRIPTION:

!     CAT(1) : conc. of H+    (moles/kg)
!     CAT(2) : conc. of NH4+  (moles/kg)
!     AN(1)  : conc. of SO4-- (moles/kg)
!     AN(2)  : conc. of NO3-  (moles/kg)
!     AN(3)  : conc. of HSO4- (moles/kg)
!     GAMA(2,1)    : mean molal ionic activity coeff for (2NH4+,SO4--)
!     GAMA(2,2)    :                                     (NH4+,NO3-)
!     GAMA(2,3)    :                                     (NH4+. HSO4-)
!     GAMA(1,1)    :                                     (2H+,SO4--)
!     GAMA(1,2)    :                                     (H+,NO3-)
!     GAMA(1,3)    :                                     (H+,HSO4-)
!     MOLNU   : the total number of moles of all ions.
!     PHIMULT : the multicomponent paractical osmotic coefficient.

! REVISION HISTORY:
!      Who       When        Detailed description of changes
!   ---------   --------  -------------------------------------------
!   S.Roselle   7/26/89   Copied parts of routine BROMLY, and began this
!                         new routine using a method described by Pilini
!                         and Seinfeld 1987, Atmos. Envirn. 21 pp2453-24
!   S.Roselle   7/30/97   Modified for use in Models-3
!   F.Binkowski 8/7/97    Modified coefficients BETA0, BETA1, CGAMA

!-----------------------------------------------------------------------

!     IMPLICIT NONE

!...........INCLUDES and their descriptions

!      INCLUDE SUBST_XSTAT     ! M3EXIT status codes
!....................................................................

! Normal, successful completion           
      INTEGER xstat0
      PARAMETER (xstat0=0)
! File I/O error                          
      INTEGER xstat1
      PARAMETER (xstat1=1)
! Execution error                         
      INTEGER xstat2
      PARAMETER (xstat2=2)
! Special  error                          
      INTEGER xstat3
      PARAMETER (xstat3=3)

      CHARACTER*120 xmsg

!...........PARAMETERS and their descriptions:

! number of cations             
      INTEGER ncat
      PARAMETER (ncat=2)

! number of anions              
      INTEGER nan
      PARAMETER (nan=3)

!...........ARGUMENTS and their descriptions

! tot # moles of all ions       
      REAL molnu
! multicomponent paractical osmo
      REAL phimult
      REAL cat(ncat) ! cation conc in moles/kg (input
      REAL an(nan) ! anion conc in moles/kg (input)
      REAL gama(ncat,nan) 
!...........SCRATCH LOCAL VARIABLES and their descriptions:

! mean molal ionic activity coef
      CHARACTER*16 & ! driver program name               
        pname
      SAVE pname

! anion indX                    
      INTEGER ian

      INTEGER icat
! cation indX                   

      REAL fgama
! ionic strength                
      REAL i

      REAL r

      REAL s

      REAL ta

      REAL tb

      REAL tc

      REAL texpv

      REAL trm
! 2*ionic strength              
      REAL twoi
! 2*sqrt of ionic strength      
      REAL twosri

      REAL zbar

      REAL zbar2

      REAL zot1
! square root of ionic strength 
      REAL sri
      REAL f2(ncat)
      REAL f1(nan)
      REAL zp(ncat) ! absolute value of charges of c
      REAL zm(nan) ! absolute value of charges of a
      REAL bgama(ncat,nan)
      REAL x(ncat,nan)
      REAL m(ncat,nan) ! molality of each electrolyte  
      REAL lgama0(ncat,nan) ! binary activity coefficients  
      REAL y(nan,ncat)
      REAL beta0(ncat,nan) ! binary activity coefficient pa
      REAL beta1(ncat,nan) ! binary activity coefficient pa
      REAL cgama(ncat,nan) ! binary activity coefficient pa
      REAL v1(ncat,nan) ! number of cations in electroly
      REAL v2(ncat,nan) 
! number of anions in electrolyt
      DATA zp/1.0, 1.0/
      DATA zm/2.0, 1.0, 1.0/
      DATA xmsg/' '/
      DATA pname/'ACTCOF'/

! *** Sources for the coefficients BETA0, BETA1, CGAMA:

! *** (1,1);(1,3)  - Clegg & Brimblecombe (1988)
! *** (2,3)        - Pilinis & Seinfeld (1987), cgama different
! *** (1,2)        - Clegg & Brimblecombe (1990)
! *** (2,1);(2,2)  - Chan, Flagen & Seinfeld (1992)

! *** now set the basic constants, BETA0, BETA1, CGAMA

      DATA beta0(1,1)/2.98E-2/, beta1(1,1)/0.0/, cgama(1,1)/4.38E-2 & 
        /
! 2H+SO4
      DATA beta0(1,2)/1.2556E-1/, beta1(1,2)/2.8778E-1/, cgama(1,2)/ -5.59E-3 & 
        /
! HNO3  
      DATA beta0(1,3)/2.0651E-1/, beta1(1,3)/5.556E-1/, cgama(1,3)/0.0 & 
        /
! H+HSO4
      DATA beta0(2,1)/4.6465E-2/, beta1(2,1)/ -0.54196/, &
        cgama(2,1)/ -1.2683E-3 & 
        /
! (NH4)2
      DATA beta0(2,2)/ -7.26224E-3/, beta1(2,2)/ -1.168858/, &
        cgama(2,2)/3.51217E-5 & 
        /
! NH4NO3
      DATA beta0(2,3)/4.494E-2/, beta1(2,3)/2.3594E-1/, cgama(2,3)/ -2.962E-3 & 
        /
! NH4HSO
      DATA v1(1,1), v2(1,1)/2.0, 1.0 & ! 2H+SO4-                  
        /
      DATA v1(2,1), v2(2,1)/2.0, 1.0 & ! (NH4)2SO4                
        /
      DATA v1(1,2), v2(1,2)/1.0, 1.0 & ! HNO3                     
        /
      DATA v1(2,2), v2(2,2)/1.0, 1.0 & ! NH4NO3                   
        /
      DATA v1(1,3), v2(1,3)/1.0, 1.0 & ! H+HSO4-                  
        /
      DATA v1(2,3), v2(2,3)/1.0, 1.0 & 
        /
!-----------------------------------------------------------------------
!  begin body of subroutine ACTCOF

!...compute ionic strength

! NH4HSO4                  
      i = 0.0

      DO icat = 1, ncat
        i = i + cat(icat)*zp(icat)*zp(icat)
      END DO

      DO ian = 1, nan
        i = i + an(ian)*zm(ian)*zm(ian)
      END DO

      i = 0.5*i

!...check for problems in the ionic strength

      IF (i==0.0) THEN

        DO ian = 1, nan
          DO icat = 1, ncat
            gama(icat,ian) = 0.0
          END DO
        END DO

!       xmsg = 'Ionic strength is zero...returning zero activities'
!       WRITE (6,*) xmsg
        RETURN

      ELSE IF (i<0.0) THEN
        xmsg = 'WARNING: Ionic strength below zero (= negative ion concentrations) - setting ion concentrations to zero.'
        call wrf_message(xmsg)
        DO ian = 1, nan
          DO icat = 1, ncat
            gama(icat,ian) = 0.0
          END DO
        END DO
        RETURN
		
      END IF

!...compute some essential expressions

      sri = sqrt(i)
      twosri = 2.0*sri
      twoi = 2.0*i
      texpv = 1.0 - exp(-twosri)*(1.0+twosri-twoi)
      r = 1.0 + 0.75*i
      s = 1.0 + 1.5*i
      zot1 = 0.511*sri/(1.0+sri)

!...Compute binary activity coeffs

      fgama = -0.392*((sri/(1.0+1.2*sri)+(2.0/1.2)*alog(1.0+1.2*sri)))

      DO icat = 1, ncat
        DO ian = 1, nan

          bgama(icat,ian) = 2.0*beta0(icat,ian) + (2.0*beta1(icat,ian)/(4.0*i) &
            )*texpv

!...compute the molality of each electrolyte for given ionic strength

          m(icat,ian) = (cat(icat)**v1(icat,ian)*an(ian)**v2(icat,ian))** &
            (1.0/(v1(icat,ian)+v2(icat,ian)))

!...calculate the binary activity coefficients

          lgama0(icat,ian) = (zp(icat)*zm(ian)*fgama+m(icat,ian)*(2.0*v1(icat, &
            ian)*v2(icat,ian)/(v1(icat,ian)+v2(icat,ian))*bgama(icat, &
            ian))+m(icat,ian)*m(icat,ian)*(2.0*(v1(icat,ian)* &
            v2(icat,ian))**1.5/(v1(icat,ian)+v2(icat,ian))*cgama(icat, &
            ian)))/2.302585093

        END DO
      END DO

!...prepare variables for computing the multicomponent activity coeffs

      DO ian = 1, nan
        DO icat = 1, ncat
          zbar = (zp(icat)+zm(ian))*0.5
          zbar2 = zbar*zbar
          y(ian,icat) = zbar2*an(ian)/i
          x(icat,ian) = zbar2*cat(icat)/i
        END DO
      END DO

      DO ian = 1, nan
        f1(ian) = 0.0
        DO icat = 1, ncat
          f1(ian) = f1(ian) + x(icat,ian)*lgama0(icat,ian) + &
            zot1*zp(icat)*zm(ian)*x(icat,ian)
        END DO
      END DO

      DO icat = 1, ncat
        f2(icat) = 0.0
        DO ian = 1, nan
          f2(icat) = f2(icat) + y(ian,icat)*lgama0(icat,ian) + &
            zot1*zp(icat)*zm(ian)*y(ian,icat)
        END DO
      END DO

!...now calculate the multicomponent activity coefficients

      DO ian = 1, nan
        DO icat = 1, ncat

          ta = -zot1*zp(icat)*zm(ian)
          tb = zp(icat)*zm(ian)/(zp(icat)+zm(ian))
          tc = (f2(icat)/zp(icat)+f1(ian)/zm(ian))
          trm = ta + tb*tc

          IF (trm>30.0) THEN
            gama(icat,ian) = 1.0E+30
!           xmsg = 'Multicomponent activity coefficient is extremely large'
!           WRITE (6,*) xmsg
          ELSE
            gama(icat,ian) = 10.0**trm
          END IF

        END DO
      END DO

      RETURN
!ia*********************************************************************
    END SUBROUTINE actcof
!ia
!ia     AEROSOL DYNAMICS DRIVER ROUTINE					*
!ia     based on MODELS3 formulation by FZB
!ia     Modified by IA in November 97
!ia
!ia     Revision history
!ia     When    WHO     WHAT
!ia     ----    ----    ----
!ia     ????    FZB     BEGIN
!ia     05/97   IA      Adapted for use in CTM2-S
!ia     11/97   IA      Modified for new model version
!ia                     see comments under iarev02
!ia
!ia     Called BY:      RPMMOD3
!ia
!ia     Calls to:       EQL3, MODPAR, COAGRATE, NUCLCOND, AEROSTEP
!ia                     GETVSED
!ia
!ia*********************************************************************
! actcof                                                      
    SUBROUTINE aeroproc(blksize,nspcsda,numcells,layer,cblk,dt,blkta,blkprs, &
        blkdens,blkrh,so4rat,orgaro1rat,orgaro2rat,orgalk1rat,orgole1rat, &
        orgbio1rat,orgbio2rat,orgbio3rat,orgbio4rat,drog,ldrog,ncv,nacv,epm25i, &
        epm25j,eorgi,eorgj,eeci,eecj,epmcoarse,esoil,eseas,xlm,amu,dgnuc, &
        dgacc,dgcor,pmassn,pmassa,pmassc,pdensn,pdensa,pdensc,knnuc,knacc, &
        kncor,fconcn,fconca,fconcn_org,fconca_org,dmdt,dndt,cgrn3,cgra3,urn00, &
        ura00,brna01,c30,deltaso4a,igrid,jgrid,kgrid)




!     IMPLICIT NONE
! dimension of arrays             
      INTEGER blksize
! number of species in CBLK       
      INTEGER nspcsda
! actual number of cells in arrays
      INTEGER numcells
! number of k-level               
      INTEGER layer
! of organic aerosol precursor  
      INTEGER ldrog
      REAL cblk(blksize,nspcsda) ! main array of variables (INPUT a

      REAL dt
! *** Meteorological information:

! synchronization time  [s]       
      REAL blkta(blksize) ! Air temperature [ K ]                  
      REAL blkprs(blksize) ! Air pressure in [ Pa ]                 
      REAL blkdens(blksize) ! Air density  [ kg/ m**3 ]              
      REAL blkrh(blksize) 
! *** Chemical production rates: [ ug / m**3 s ]

! Fractional relative humidity           
      REAL so4rat(blksize) 
!bs
! sulfate gas-phase production rate     
! total # of cond. vapors & SOA species 
      INTEGER ncv
!bs
      INTEGER nacv
!bs * organic condensable vapor production rate
! # of anthrop. cond. vapors & SOA speci
      REAL drog(blksize,ldrog) !bs
! *** anthropogenic organic aerosol mass production rates from aromatics
! Delta ROG conc. [ppm]              
      REAL orgaro1rat(blksize)

! *** anthropogenic organic aerosol mass production rates from aromatics
      REAL orgaro2rat(blksize)

! *** anthropogenic organic aerosol mass production rates from alkanes &
      REAL orgalk1rat(blksize)

! *** anthropogenic organic aerosol mass production rates from alkenes &
      REAL orgole1rat(blksize)

! *** biogenic organic aerosol production rates
      REAL orgbio1rat(blksize)

! *** biogenic organic aerosol production rates
      REAL orgbio2rat(blksize)

! *** biogenic organic aerosol production rates
      REAL orgbio3rat(blksize)

! *** biogenic organic aerosol production rates
      REAL orgbio4rat(blksize)

! *** Primary emissions rates: [ ug / m**3 s ]

! *** emissions rates for unidentified PM2.5 mass
      REAL epm25i(blksize) ! Aitken mode                         
      REAL epm25j(blksize) 
! *** emissions rates for primary organic aerosol
! Accumululaton mode                  
      REAL eorgi(blksize) ! Aitken mode                          
      REAL eorgj(blksize) 
! *** emissions rates for elemental carbon
! Accumululaton mode                   
      REAL eeci(blksize) ! Aitken mode                           
      REAL eecj(blksize) 
! *** emissions rates for coarse mode particles
! Accumululaton mode                    
      REAL esoil(blksize) ! soil derived coarse aerosols          
      REAL eseas(blksize) ! marine coarse aerosols                
      REAL epmcoarse(blksize) 

! *** OUTPUT:

! *** atmospheric properties

! anthropogenic coarse aerosols         
      REAL xlm(blksize) ! atmospheric mean free path [ m ]  
      REAL amu(blksize) 
! *** modal diameters: [ m ]

! atmospheric dynamic viscosity [ kg
      REAL dgnuc(blksize) ! nuclei mode geometric mean diamete
      REAL dgacc(blksize) ! accumulation geometric mean diamet
      REAL dgcor(blksize) 

! *** aerosol properties:

! *** Modal mass concentrations [ ug m**3 ]

! coarse mode geometric mean diamete
      REAL pmassn(blksize) ! mass concentration in Aitken mode 
      REAL pmassa(blksize) ! mass concentration in accumulation
      REAL pmassc(blksize) 
! *** average modal particle densities  [ kg/m**3 ]

! mass concentration in coarse mode 
      REAL pdensn(blksize) ! average particle density in nuclei
      REAL pdensa(blksize) ! average particle density in accumu
      REAL pdensc(blksize) 
! *** average modal Knudsen numbers

! average particle density in coarse
      REAL knnuc(blksize) ! nuclei mode  Knudsen number       
      REAL knacc(blksize) ! accumulation Knudsen number       
      REAL kncor(blksize) 
! ***  modal condensation factors ( see comments in NUCLCOND )

! coarse mode  Knudsen number       
      REAL fconcn(blksize)
      REAL fconca(blksize)
!bs
      REAL fconcn_org(blksize)
      REAL fconca_org(blksize)
!bs

! *** Rates for secondary particle formation:

! *** production of new mass concentration [ ug/m**3 s ]
      REAL dmdt(blksize) !                                 by particle formation

! *** production of new number concentration [ number/m**3 s ]

! rate of production of new mass concen
      REAL dndt(blksize) !                                 by particle formation

! *** growth rate for third moment by condensation of precursor
!      vapor on existing particles [ 3rd mom/m**3 s ]

! rate of producton of new particle num
      REAL cgrn3(blksize) !  Aitken mode                          
      REAL cgra3(blksize) 
! *** Rates for coaglulation: [ m**3/s ]

! *** Unimodal Rates:

!  Accumulation mode                    
      REAL urn00(blksize) ! Aitken mode 0th moment self-coagulation ra
      REAL ura00(blksize) 

! *** Bimodal Rates:  Aitken mode with accumulation mode ( d( Aitken mod

! accumulation mode 0th moment self-coagulat
      REAL brna01(blksize) 
! *** 3rd moment intermodal transfer rate replaces coagulation rate ( FS
! rate for 0th moment                     
      REAL c30(blksize)                                                        ! by intermodal c

! *** other processes

! intermodal 3rd moment transfer r
      REAL deltaso4a(blksize) !                                  sulfate aerosol by condensation   [ u


!      INTEGER NN, VV ! loop indICES

! increment of concentration added to   
! ////////////////////// Begin code ///////////////////////////////////




! concentration lower limit  
      CHARACTER*16 pname
      PARAMETER (pname=' AEROPROC       ')

      INTEGER unit
      PARAMETER (unit=20)
      integer igrid,jgrid,kgrid,isorop
!          isorop=0
      isorop=1



! *** get water, ammonium  and nitrate content:
!     for now, don't call if temp is below -40C (humidity
!     for this wrf version is already limited to 10 percent)

        if(blkta(1).ge.233.15.and.blkrh(1).ge.0.1 .and. isorop.eq.1)then
           CALL eql3(blksize,nspcsda,numcells,cblk,blkta,blkrh,igrid,jgrid,kgrid)
        else if (blkta(1).ge.233.15.and.blkrh(1).ge.0.1 .and. isorop.eq.0)then
           CALL eql4(blksize,nspcsda,numcells,cblk,blkta,blkrh)
        endif

! *** get size distribution information:

      CALL modpar(blksize,nspcsda,numcells,cblk,blkta,blkprs,pmassn,pmassa, &
        pmassc,pdensn,pdensa,pdensc,xlm,amu,dgnuc,dgacc,dgcor,knnuc,knacc, &
        kncor)

! *** Calculate coagulation rates for fine particles:

      CALL coagrate(blksize,nspcsda,numcells,cblk,blkta,pdensn,pdensa,amu, &
        dgnuc,dgacc,knnuc,knacc,urn00,ura00,brna01,c30)


! *** get condensation and particle formation (nucleation) rates:

      CALL nuclcond(blksize,nspcsda,numcells,cblk,dt,layer,blkta,blkprs,blkrh, &
        so4rat,orgaro1rat,orgaro2rat,orgalk1rat,orgole1rat,orgbio1rat, &
        orgbio2rat,orgbio3rat,orgbio4rat,drog,ldrog,ncv,nacv,dgnuc,dgacc, &
        fconcn,fconca,fconcn_org,fconca_org,dmdt,dndt,deltaso4a,cgrn3,cgra3)

!       if(dndt(1).lt.-10.)print*,'dndt in aeroproc',dndt
        
! *** advance forward in time  DT seconds:
      CALL aerostep(layer,blksize,nspcsda,numcells,cblk,dt,so4rat,orgaro1rat, &
        orgaro2rat,orgalk1rat,orgole1rat,orgbio1rat,orgbio2rat,orgbio3rat, &
        orgbio4rat,epm25i,epm25j,eorgi,eorgj,eeci,eecj,esoil,eseas,epmcoarse, &
        dgnuc,dgacc,fconcn,fconca,fconcn_org,fconca_org,pmassn,pmassa,pmassc, &
        dmdt,dndt,deltaso4a,urn00,ura00,brna01,c30,cgrn3,cgra3,igrid,jgrid,kgrid)


! *** get new distribution information:

      CALL modpar(blksize,nspcsda,numcells,cblk,blkta,blkprs,pmassn,pmassa, &
        pmassc,pdensn,pdensa,pdensc,xlm,amu,dgnuc,dgacc,dgcor,knnuc,knacc, &
        kncor)

      RETURN
    END SUBROUTINE aeroproc
!//////////////////////////////////////////////////////////////////
! *** Time stepping code advances the aerosol moments one timestep;


    SUBROUTINE aerostep(layer,blksize,nspcsda,numcells,cblk,dt,so4rat         &
       ,orgaro1rat,orgaro2rat,orgalk1rat,orgole1rat,orgbio1rat,orgbio2rat     &
       ,orgbio3rat,orgbio4rat,epm25i,epm25j,eorgi,eorgj,eeci,eecj,esoil,eseas &
       ,epmcoarse,dgnuc,dgacc,fconcn,fconca,fconcn_org,fconca_org,pmassn      &
       ,pmassa,pmassc,dmdt,dndt,deltaso4a,urn00,ura00,brna01,c30,cgrn3,cgra3, &
        igrid,jgrid,kgrid                                                     &
                                                                             )

!***********************************************************************

!      NOTE:

! ***  DESCRIPTION: Integrate the Number and Mass equations
!                   for each mode over the time interval DT.

!      PRECONDITIONS:
!       AEROSTEP() must follow calls to all other dynamics routines.

! ***   Revision history:
!       Adapted 3/95 by UAS and CJC from EAM2's code.
!       Revised 7/29/96 by FSB to use block structure
!       Revised 11/15/96 by FSB dropped flow-through and cast
!                           number solver into Riccati equation form.
!       Revised 8/8/97 by FSB to have mass in Aitken and accumulation mo
!                        each predicted rather than total mass and
!                        Aitken mode mass. Also used a local approximati
!                        the error function. Also added coarse mode.
!       Revised 9/18/97 by FSB to fix mass transfer from Aitken to
!                       accumulation mode by coagulation
!       Revised 10/27/97 by FSB to modify code to use primay emissions
!                        and to correct 3rd moment updates.
!                        Also added coarse mode.
!       Revised 11/4/97 by FSB to fix error in other anthropogenic PM2.5
!       Revised  11/5/97 by FSB to fix error in MSTRNSFR
!       Revised  11/6/97 FSB to correct the expression for FACTRANS to
!                        remove the 6/pi coefficient. UAS found this.
!       Revised 12/15/97 by FSB to change equations for mass concentrati
!                        to a chemical production form with analytic
!                        solutions for the Aitken mode and to remove
!                        time stepping of the 3rd moments. The mass conc
!                        in the accumulation mode is updated with a forw
!                        Euler step.
!       Revised 1/6/98   by FSB Lowered minimum concentration for
!                        sulfate aerosol to 0.1 [ ng / m**3 ].
!       Revised 1/12/98  C30 replaces BRNA31 as a variable. C30 represen
!                        intermodal transfer rate of 3rd moment in place
!                        of 3rd moment coagulation rate.
!       Revised 5/5/98   added new renaming criterion based on diameters
!       Added   3/23/98  by BS condensational groth factors for organics

!**********************************************************************

!     IMPLICIT NONE

!     Includes:



! *** ARGUMENTS:

! dimension of arrays             
      INTEGER blksize
! actual number of cells in arrays
      INTEGER numcells
! nmber of species in CBLK        
      INTEGER nspcsda
! model layer                     
      INTEGER layer
      REAL cblk(blksize,nspcsda) ! main array of variables          
      INTEGER igrid,jgrid,kgrid
      REAL dt
! *** Chemical production rates: [ ug / m**3 s ]

! time step [sec]                  
      REAL so4rat(blksize) 
! *** anthropogenic organic aerosol mass production rates from aromatics
! sulfate gas-phase production rate     
      REAL orgaro1rat(blksize)
      REAL orgaro2rat(blksize)

! *** anthropogenic organic aerosol mass production rates from alkanes &
      REAL orgalk1rat(blksize)
      REAL orgole1rat(blksize)

! *** biogenic organic aerosol production rates
      REAL orgbio1rat(blksize)
      REAL orgbio2rat(blksize)
      REAL orgbio3rat(blksize)
      REAL orgbio4rat(blksize)

! *** Primary emissions rates: [ ug / m**3 s ]

! *** emissions rates for unidentified PM2.5 mass
      REAL epm25i(blksize) ! Aitken mode                         
      REAL epm25j(blksize) 
! *** emissions rates for primary organic aerosol
! Accumululaton mode                  
      REAL eorgi(blksize) ! Aitken mode                          
      REAL eorgj(blksize) 
! *** emissions rates for elemental carbon
! Accumululaton mode                    
      REAL eeci(blksize) ! Aitken mode                           
      REAL eecj(blksize) 
! *** emissions rates for coarse mode particles
! Accumululaton mode                    
      REAL esoil(blksize) ! soil derived coarse aerosols          
      REAL eseas(blksize) ! marine coarse aerosols                
      REAL epmcoarse(blksize) 
! anthropogenic coarse aerosols         
      REAL dgnuc(blksize) ! nuclei mode mean diameter [ m ]
      REAL dgacc(blksize) 
! accumulation                          
      REAL fconcn(blksize)                                 ! Aitken mode  [ 1 / s ]
! reciprocal condensation rate          
      REAL fconca(blksize)                                 ! acclumulation mode [ 1 / s ]
! reciprocal condensation rate          
      REAL fconcn_org(blksize)                                 ! Aitken mode  [ 1 / s ]
! reciprocal condensation rate for organ
      REAL fconca_org(blksize)                                 ! acclumulation mode [ 1 / s ]
! reciprocal condensation rate for organ
      REAL dmdt(blksize)                                 ! by particle formation [ ug/m**3 /s ]
! rate of production of new mass concent
      REAL dndt(blksize)                                 ! by particle formation [ number/m**3 /s
! rate of producton of new particle numb
      REAL deltaso4a(blksize)                                 ! sulfate aerosol by condensation [ ug/m
! increment of concentration added to   
      REAL urn00(blksize) ! Aitken intramodal coagulation rate    
      REAL ura00(blksize) ! Accumulation mode intramodal coagulati
      REAL brna01(blksize) ! bimodal coagulation rate for number   
      REAL c30(blksize)       							! by intermodal coagulation
! intermodal 3rd moment transfer rate by
      REAL cgrn3(blksize) ! growth rate for 3rd moment for Aitken 
      REAL cgra3(blksize) 
! *** Modal mass concentrations [ ug m**3 ]

! growth rate for 3rd moment for Accumul
      REAL pmassn(blksize) ! mass concentration in Aitken mode 
      REAL pmassa(blksize) ! mass concentration in accumulation
      REAL pmassc(blksize) 

! *** Local Variables

! mass concentration in coarse mode 
      INTEGER l, lcell, & 
        spc
! ** following scratch variables are used for solvers



! *** variables needed for modal dynamics solvers:

! Loop indices                   
      REAL*8 a, b, c
      REAL*8 m1, m2, y0, y
      REAL*8 dhat, p, pexpdt, expdt
      REAL*8 loss, prod, pol, lossinv
! mass intermodal transfer by coagulation           
      REAL mstrnsfr

      REAL factrans

! *** CODE additions for renaming
      REAL getaf2
      REAL aaa, xnum, xm3, fnum, fm3, phnum, & ! Defined below
        phm3
      REAL erf, & ! Error and complementary error function   
        erfc

      REAL xx
! dummy argument for ERF and ERFC          
! a numerical value for a minimum concentration       

! *** This value is smaller than any reported tropospheric concentration


!     :::::::::::::::::::::::::::::::::::::
! *** Statement function given for error function. Source is
!     Meng, Z., and J.H.Seinfeld (1994) On the source of the submicromet
!      droplet mode of urban and regional aerosols. Aerosol Sci. and Tec
!      20:253-265. They cite Reasearch & Education Asociation (REA), (19
!      Handbook of Mathematical, Scientific, and Engineering Formulas,
!      Tables, Functions, Graphs, Transforms: REA, Piscataway, NJ. p. 49

      erf(xx) = sqrt(1.0-exp(-4.0*xx*xx/pirs))
      erfc(xx) = 1.0 - erf(xx)
!     ::::::::::::::::::::::::::::::::::::::::


! ///// begin code




! *** set up time-step integration

      DO l = 1, numcells

! *** code to move number forward by one time step.
! *** solves the Ricatti equation:

!     dY/dt = C - A * Y ** 2 - B * Y

!     Coded 11/21/96 by Dr. Francis S. Binkowski

! *** Aitken mode:

! *** coefficients

        a = urn00(l)
        b = brna01(l)*cblk(l,vac0)
        c = dndt(l) + factnumn*(anthfac*(epm25i(l)+eeci(l))+orgfac*eorgi(l)) 

! includes primary emissions 
        y0 = cblk(l,vnu0) 
! ***  trap on C = 0

! initial condition                           
        IF (c>0.0D0) THEN

          dhat = sqrt(b*b+4.0D0*a*c)

          m1 = 2.0D0*a*c/(b+dhat)

          m2 = -0.5D0*(b+dhat)

          p = -(m1-a*y0)/(m2-a*y0)

          pexpdt = p*exp(-dhat*dt)

          y = (m1+m2*pexpdt)/(a*(1.0D0+pexpdt)) 
! solution                       
        ELSE

! *** rearrange solution for NUMERICAL stability
!     note If B << A * Y0, the following form, although
!     seemingly awkward gives the correct answer.

          expdt = exp(-b*dt)
          IF (expdt<1.0D0) THEN
            y = b*y0*expdt/(b+a*y0*(1.0D0-expdt))
          ELSE
            y = y0
          END IF

        END IF
!       if(y.lt.nummin_i)then
!         print *,'a,b,y,y0,c,cblk(l,vnu0),dt,dndt(l),brna01(l),epm25i(l),eeci(l),eorgi(l)'
!         print *,'igrid,jgrid,kgrid = ',igrid,jgrid,kgrid
!         print *,a,b,y,y0,c,cblk(l,vnu0),dt,dndt(l),brna01(l),epm25i(l),eeci(l),eorgi(l)
!       endif

        cblk(l,vnu0) = max(nummin_i,y) 

! *** now do accumulation mode number

! *** coefficients

! update                     
        a = ura00(l)
        b = & ! NOTE B = 0.0                                         
          0.0D0
        c = factnuma*(anthfac*(epm25j(l)+eecj(l))+orgfac*eorgj(l)) 
! includes primary emissi
        y0 = cblk(l,vac0) 
! *** this equation requires special handling, because C can be zero.
!     if this happens, the form of the equation is different:

! initial condition                           
!       print *,vac0,y0,c,nummin_j,a
        IF (c>0.0D0) THEN

          dhat = sqrt(4.0D0*a*c)

          m1 = 2.0D0*a*c/dhat

          m2 = -0.5D0*dhat

          p = -(m1-a*y0)/(m2-a*y0)

!       print *,p,-dhat,dt,-dhat*dt
!       print *,exp(-dhat*dt)
          pexpdt = p*exp(-dhat*dt)

          y = (m1+m2*pexpdt)/(a*(1.0D0+pexpdt)) 
! solution                       
        ELSE

          y = y0/(1.0D0+dt*a*y0) 
!       print *,dhat,y0,dt,a
          y = y0/(1.+dt*a*y0) 
!       print *,y
! correct solution to equatio
        END IF

        cblk(l,vac0) = max(nummin_j,y) 
! *** now do coarse mode number neglecting coagulation
! update                     
!       print *,soilfac,seasfac,anthfac,esoil(l),eseas(l),epmcoarse(l)
        prod = soilfac*esoil(l) + seasfac*eseas(l) + anthfac*epmcoarse(l)

!       print *,cblk(l,vcorn),factnumc,prod
        cblk(l,vcorn) = cblk(l,vcorn) + factnumc*prod*dt


! *** Prepare to advance modal mass concentration one time step.

! *** Set up production and and intermodal transfer terms terms:
!       print *,cgrn3(l),epm25i(l),eeci(l),orgfac,eorgi(l)
        cgrn3(l) = cgrn3(l) + anthfac*(epm25i(l)+eeci(l)) + orgfac*eorgi(l) 

! includes growth from pri
        cgra3(l) = cgra3(l) + c30(l) + anthfac*(epm25j(l)+eecj(l)) + &
          orgfac*eorgj(l)                                              ! and transfer of 3rd momen
                                             ! intermodal coagulation

! *** set up transfer coefficients for coagulation between Aitken and ac


! *** set up special factors for mass transfer from the Aitken to accumu
!     intermodal coagulation. The mass transfer rate is proportional to
!     transfer rate, C30. The proportionality factor is p/6 times the th
!     density. The average particle density for a species is the species
!     divided by the particle volume concentration, pi/6 times the 3rd m
!     The p/6 coefficients cancel.

! includes growth from prim
!       print *,'loss',vnu3,c30(l),cblk(l,vnu3)
        loss = c30(l)/cblk(l,vnu3) 

! Normalized coagulation transfer r
        factrans = loss* &                             ! yields an estimate of the amount of mass t
          dt
                            ! the Aitken to the accumulation mode in the

! Multiplying this factor by the species con
!       print *,'factrans = ',factrans,loss
        expdt = exp(-factrans)                               ! decay term is common to all Aitken mode
!       print *,'factrans = ',factrans,loss,expdt
! variable name is re-used here. This expo
        lossinv = 1.0/ & 
          loss
! *** now advance mass concentrations one time step.


! ***  update sulfuric acid vapor concentration by removing mass concent
!      condensed sulfate and newly produced particles.
! *** The method follows Youngblood and Kreidenweis, Further Development
!     of a Bimodal Aerosol Dynamics Model, Colorado State University Dep
!     Atmospheric Science Paper Number 550, April,1994, pp 85-89.
! set up for multiplication rather than divi
        cblk(l,vsulf) = max(conmin,cblk(l,vsulf)-(deltaso4a(l)+dmdt(l)*dt))


! *** Solve Aitken-mode equations of form: dc/dt = P - L*c
! *** Solution is:     c(t0 + dt) = p/L + ( c(0) - P/L ) * exp(-L*dt)

! *** sulfate:

        mstrnsfr = cblk(l,vso4ai)*factrans
        prod = deltaso4a(l)*fconcn(l)/dt + dmdt(l) ! Condensed mass +
        pol = prod*lossinv
!       print *,'pol = ',prod,lossinv,deltaso4a(l),cblk(l,vso4ai),dmdt(l),mstrnsfr

        cblk(l,vso4ai) = pol + (cblk(l,vso4ai)-pol)*expdt

        cblk(l,vso4ai) = max(aeroconcmin,cblk(l,vso4ai))

        cblk(l,vso4aj) = cblk(l,vso4aj) + deltaso4a(l)*fconca(l) + mstrnsfr

! *** anthropogenic secondary organic:
!bs * anthropogenic secondary organics from aromatic precursors

        mstrnsfr = cblk(l,vorgaro1i)*factrans
        prod = orgaro1rat(l)*fconcn_org(l)
        pol = prod*lossinv

        cblk(l,vorgaro1i) = pol + (cblk(l,vorgaro1i)-pol)*expdt

        cblk(l,vorgaro1i) = max(conmin,cblk(l,vorgaro1i))

        cblk(l,vorgaro1j) = cblk(l,vorgaro1j) + orgaro1rat(l)*fconca_org(l)*dt &
          + mstrnsfr
!bs * second species from aromatics
        mstrnsfr = cblk(l,vorgaro2i)*factrans
        prod = orgaro2rat(l)*fconcn_org(l)
        pol = prod*lossinv

        cblk(l,vorgaro2i) = pol + (cblk(l,vorgaro2i)-pol)*expdt

        cblk(l,vorgaro2i) = max(conmin,cblk(l,vorgaro2i))

        cblk(l,vorgaro2j) = cblk(l,vorgaro2j) + orgaro2rat(l)*fconca_org(l)*dt &
          + mstrnsfr

!bs * anthropogenic secondary organics from alkanes & other precursors
!bs * higher alkanes
        mstrnsfr = cblk(l,vorgalk1i)*factrans
        prod = orgalk1rat(l)*fconcn_org(l)
        pol = prod*lossinv

        cblk(l,vorgalk1i) = pol + (cblk(l,vorgalk1i)-pol)*expdt

        cblk(l,vorgalk1i) = max(conmin,cblk(l,vorgalk1i))

        cblk(l,vorgalk1j) = cblk(l,vorgalk1j) + orgalk1rat(l)*fconca_org(l)*dt &
          + mstrnsfr
!bs * higher olefines
        mstrnsfr = cblk(l,vorgole1i)*factrans
        prod = orgole1rat(l)*fconcn_org(l)
        pol = prod*lossinv

        cblk(l,vorgole1i) = pol + (cblk(l,vorgole1i)-pol)*expdt

        cblk(l,vorgole1i) = max(conmin,cblk(l,vorgole1i))

        cblk(l,vorgole1j) = cblk(l,vorgole1j) + orgole1rat(l)*fconca_org(l)*dt &
          + mstrnsfr

! *** biogenic secondary organic

        mstrnsfr = cblk(l,vorgba1i)*factrans
        prod = orgbio1rat(l)*fconcn_org(l)
        pol = prod*lossinv

        cblk(l,vorgba1i) = pol + (cblk(l,vorgba1i)-pol)*expdt

        cblk(l,vorgba1i) = max(conmin,cblk(l,vorgba1i))

        cblk(l,vorgba1j) = cblk(l,vorgba1j) + orgbio1rat(l)*fconca_org(l)*dt + &
          mstrnsfr
!bs * second biogenic species
        mstrnsfr = cblk(l,vorgba2i)*factrans
        prod = orgbio2rat(l)*fconcn_org(l)
        pol = prod*lossinv

        cblk(l,vorgba2i) = pol + (cblk(l,vorgba2i)-pol)*expdt

        cblk(l,vorgba2i) = max(conmin,cblk(l,vorgba2i))

        cblk(l,vorgba2j) = cblk(l,vorgba2j) + orgbio2rat(l)*fconca_org(l)*dt + &
          mstrnsfr

!bs * third biogenic species
        mstrnsfr = cblk(l,vorgba3i)*factrans
        prod = orgbio3rat(l)*fconcn_org(l)
        pol = prod*lossinv

        cblk(l,vorgba3i) = pol + (cblk(l,vorgba3i)-pol)*expdt

        cblk(l,vorgba3i) = max(conmin,cblk(l,vorgba3i))

        cblk(l,vorgba3j) = cblk(l,vorgba3j) + orgbio3rat(l)*fconca_org(l)*dt + &
          mstrnsfr

!bs * fourth biogenic species
        mstrnsfr = cblk(l,vorgba4i)*factrans
        prod = orgbio4rat(l)*fconcn_org(l)
        pol = prod*lossinv

        cblk(l,vorgba4i) = pol + (cblk(l,vorgba4i)-pol)*expdt

        cblk(l,vorgba4i) = max(conmin,cblk(l,vorgba4i))

        cblk(l,vorgba4j) = cblk(l,vorgba4j) + orgbio4rat(l)*fconca_org(l)*dt + &
          mstrnsfr

! *** primary anthropogenic organic

        mstrnsfr = cblk(l,vorgpai)*factrans
        prod = eorgi(l)
        pol = prod*lossinv

        cblk(l,vorgpai) = pol + (cblk(l,vorgpai)-pol)*expdt

        cblk(l,vorgpai) = max(conmin,cblk(l,vorgpai))

        cblk(l,vorgpaj) = cblk(l,vorgpaj) + eorgj(l)*dt + mstrnsfr

! *** other anthropogenic PM2.5

        mstrnsfr = cblk(l,vp25ai)*factrans
        prod = epm25i(l)
        pol = prod*lossinv

        cblk(l,vp25ai) = pol + (cblk(l,vp25ai)-pol)*expdt

        cblk(l,vp25ai) = max(conmin,cblk(l,vp25ai))

        cblk(l,vp25aj) = cblk(l,vp25aj) + epm25j(l)*dt + mstrnsfr

! ***  elemental carbon

        mstrnsfr = cblk(l,veci)*factrans
        prod = eeci(l)
        pol = prod*lossinv

        cblk(l,veci) = pol + (cblk(l,veci)-pol)*expdt

        cblk(l,veci) = max(conmin,cblk(l,veci))

        cblk(l,vecj) = cblk(l,vecj) + eecj(l)*dt + mstrnsfr


! ***  coarse mode

! *** soil dust

        cblk(l,vsoila) = cblk(l,vsoila) + esoil(l)*dt
        cblk(l,vsoila) = max(conmin,cblk(l,vsoila))

! *** sea salt

        cblk(l,vseas) = cblk(l,vseas) + eseas(l)*dt
        cblk(l,vseas) = max(conmin,cblk(l,vseas))

! *** anthropogenic PM10 coarse fraction

        cblk(l,vantha) = cblk(l,vantha) + epmcoarse(l)*dt
        cblk(l,vantha) = max(conmin,cblk(l,vantha))



      END DO


! *** Check for mode merging,if Aitken mode is growing faster than j-mod
!     then merge modes by renaming.

! *** use Binkowski-Kreidenweis paradigm, now including emissions


! end of time-step loop for total mass                 
      DO lcell = 1, numcells

!       IF( CGRN3(LCELL) .GT. CGRA3(LCELL) .AND.
!    &      CBLK(LCELL,VNU0) .GT. CBLK(LCELL,VAC0) ) THEN ! check if mer
        IF (cgrn3(lcell)>cgra3(lcell) .OR. dgnuc(lcell)>.03E-6 .AND. cblk( &
            lcell,vnu0)>cblk(lcell,vac0)) & 
            THEN

! check if mer
          aaa = getaf(cblk(lcell,vnu0),cblk(lcell,vac0),dgnuc(lcell), &
            dgacc(lcell),xxlsgn,xxlsga,sqrt2)

! *** AAA is the value of ln( dd / DGNUC ) / ( SQRT2 * XXLSGN ), where
!        dd is the diameter at which the Aitken-mode and accumulation-mo
!        distributions intersect (overap).


          xnum = max(aaa,xxm3)                                    ! this means that no more than one ha
                                   ! total Aitken mode number may be tra
                                   ! per call.

! do not let XNUM become negative bec
          xm3 = xnum - & 
            xxm3
! set up for 3rd moment and mass tran
          IF (xm3>0.0) & 
              THEN
! do mode merging if  overlap is corr
            phnum = 0.5*(1.0+erf(xnum))
            phm3 = 0.5*(1.0+erf(xm3))
            fnum = 0.5*erfc(xnum)
            fm3 = 0.5*erfc(xm3)


!     In the Aitken mode:

! *** FNUM and FM3 are the fractions of the number and 3rd moment
!     distributions with  diameters greater than dd respectively.


! *** PHNUM and PHM3 are the fractions of the number and 3rd moment
!     distributions with diameters less than dd.


! *** rename the  Aitken mode particle number as accumulation mode
!     particle number

            cblk(lcell,vac0) = cblk(lcell,vac0) + fnum*cblk(lcell,vnu0)


! *** adjust the Aitken mode number

            cblk(lcell,vnu0) = phnum*cblk(lcell,vnu0)

! *** Rename mass from Aitken mode to acumulation mode. The mass transfe
!     to the accumulation mode is proportional to the amount of 3rd mome
!     transferred, therefore FM3 is used for mass transfer.

            cblk(lcell,vso4aj) = cblk(lcell,vso4aj) + cblk(lcell,vso4ai)*fm3

            cblk(lcell,vnh4aj) = cblk(lcell,vnh4aj) + cblk(lcell,vnh4ai)*fm3

            cblk(lcell,vno3aj) = cblk(lcell,vno3aj) + cblk(lcell,vno3ai)*fm3

            cblk(lcell,vorgaro1j) = cblk(lcell,vorgaro1j) + &
              cblk(lcell,vorgaro1i)*fm3

            cblk(lcell,vorgaro2j) = cblk(lcell,vorgaro2j) + &
              cblk(lcell,vorgaro2i)*fm3

            cblk(lcell,vorgalk1j) = cblk(lcell,vorgalk1j) + &
              cblk(lcell,vorgalk1i)*fm3

            cblk(lcell,vorgole1j) = cblk(lcell,vorgole1j) + &
              cblk(lcell,vorgole1i)*fm3

            cblk(lcell,vorgba1j) = cblk(lcell,vorgba1j) + &
              cblk(lcell,vorgba1i)*fm3

            cblk(lcell,vorgba2j) = cblk(lcell,vorgba2j) + &
              cblk(lcell,vorgba2i)*fm3

            cblk(lcell,vorgba3j) = cblk(lcell,vorgba3j) + &
              cblk(lcell,vorgba3i)*fm3

            cblk(lcell,vorgba4j) = cblk(lcell,vorgba4j) + &
              cblk(lcell,vorgba4i)*fm3

            cblk(lcell,vorgpaj) = cblk(lcell,vorgpaj) + &
              cblk(lcell,vorgpai)*fm3

            cblk(lcell,vp25aj) = cblk(lcell,vp25aj) + cblk(lcell,vp25ai)*fm3

            cblk(lcell,vecj) = cblk(lcell,vecj) + cblk(lcell,veci)*fm3

! *** update Aitken mode for mass loss to accumulation mode

            cblk(lcell,vso4ai) = cblk(lcell,vso4ai)*phm3


            cblk(lcell,vnh4ai) = cblk(lcell,vnh4ai)*phm3

            cblk(lcell,vno3ai) = cblk(lcell,vno3ai)*phm3

            cblk(lcell,vorgaro1i) = cblk(lcell,vorgaro1i)*phm3

            cblk(lcell,vorgaro2i) = cblk(lcell,vorgaro2i)*phm3

            cblk(lcell,vorgalk1i) = cblk(lcell,vorgalk1i)*phm3

            cblk(lcell,vorgole1i) = cblk(lcell,vorgole1i)*phm3

            cblk(lcell,vorgba1i) = cblk(lcell,vorgba1i)*phm3

            cblk(lcell,vorgba2i) = cblk(lcell,vorgba2i)*phm3

            cblk(lcell,vorgba3i) = cblk(lcell,vorgba3i)*phm3

            cblk(lcell,vorgba4i) = cblk(lcell,vorgba4i)*phm3

            cblk(lcell,vorgpai) = cblk(lcell,vorgpai)*phm3

            cblk(lcell,vp25ai) = cblk(lcell,vp25ai)*phm3

            cblk(lcell,veci) = cblk(lcell,veci)*phm3


          END IF
! end check on whether modal overlap is OK             

        END IF
! end check on necessity for merging                   

      END DO
!     set min value for all concentrations

! loop for merging                                       
      DO spc = 1, nspcsda
        DO lcell = 1, numcells
          cblk(lcell,spc) = max(cblk(lcell,spc),conmin)
        END DO
      END DO


      RETURN

!#######################################################################
    END SUBROUTINE aerostep
! aerostep                                                 
    SUBROUTINE awater(irhx,mso4,mnh4,mno3,wh2o)
! NOTE!!! wh2o is returned in micrograms / cubic meter
!         mso4,mnh4,mno3 are in microMOLES / cubic meter

!  This  version uses polynomials rather than tables, and uses empirical
! polynomials for the mass fraction of solute (mfs) as a function of wat
!   where:

!            mfs = ms / ( ms + mw)
!             ms is the mass of solute
!             mw is the mass of water.

!  Define y = mw/ ms

!  then  mfs = 1 / (1 + y)

!    y can then be obtained from the values of mfs as

!             y = (1 - mfs) / mfs


!     the aerosol is assumed to be in a metastable state if the rh is
!     is below the rh of deliquescence, but above the rh of crystallizat

!     ZSR interpolation is used for sulfates with x ( the molar ratio of
!     ammonium to sulfate in eh range 0 <= x <= 2, by sections.
!     section 1: 0 <= x < 1
!     section 2: 1 <= x < 1.5
!     section 3: 1.5 <= x < 2.0
!     section 4: 2 <= x
!     In sections 1 through 3, only the sulfates can affect the amount o
!     on the particles.
!     In section 4, we have fully neutralized sulfate, and extra ammoniu
!     allows more nitrate to be present. Thus, the ammount of water is c
!     using ZSR for ammonium sulfate and ammonium nitrate. Crystallizati
!     assumed to occur in sections 2,3,and 4. See detailed discussion be



! definitions:
!     mso4, mnh4, and mno3 are the number of micromoles/(cubic meter of
!      for sulfate, ammonium, and nitrate respectively
!     irhx is the relative humidity (%)
!     wh2o is the returned water amount in micrograms / cubic meter of a
!     x is the molar ratio of ammonium to sulfate
!     y0,y1,y1.5, y2 are the water contents in mass of water/mass of sol
!     for pure aqueous solutions with x equal 1, 1.5, and 2 respectively
!     y3 is the value of the mass ratio of water to solute for
!     a pure ammonium nitrate  solution.


!oded by Dr. Francis S. Binkowski, 4/8/96.

!     IMPLICIT NONE
      INTEGER irhx, irh
      REAL mso4, mnh4, mno3
      REAL tso4, tnh4, tno3, wh2o, x
      REAL aw, awc
!     REAL poly4, poly6
      REAL mfs0, mfs1, mfs15, mfs2
      REAL c0(4), c1(4), c15(4), c2(4)
      REAL y, y0, y1, y15, y2, y3, y40, y140, y1540, yc
      REAL kso4(6), kno3(6), mfsso4, mfsno3



      REAL mwso4, mwnh4, mwno3, mw2, mwano3

! *** molecular weights:
      PARAMETER (mwso4=96.0636,mwnh4=18.0985,mwno3=62.0049, &
        mw2=mwso4+2.0*mwnh4,mwano3=mwno3+mwnh4)

!     The polynomials use data for aw as a function of mfs from Tang and
!     Munkelwitz, JGR 99: 18801-18808, 1994.
!     The polynomials were fit to Tang's values of water activity as a
!     function of mfs.

! *** coefficients of polynomials fit to Tang and Munkelwitz data
!     now give mfs as a function of water activity.

      DATA c1/0.9995178, -0.7952896, 0.99683673, -1.143874/
      DATA c15/1.697092, -4.045936, 5.833688, -3.463783/
      DATA c2/2.085067, -6.024139, 8.967967, -5.002934/

! *** the following coefficients are a fit to the data in Table 1 of
!     Nair & Vohra, J. Aerosol Sci., 6: 265-271, 1975
!      data c0/0.8258941, -1.899205, 3.296905, -2.214749 /
! *** New data fit to data from
!       Nair and Vohra J. Aerosol Sci., 6: 265-271, 1975
!       Giaque et al. J.Am. Chem. Soc., 82: 62-70, 1960
!       Zeleznik J. Phys. Chem. Ref. Data, 20: 157-1200
      DATA c0/0.798079, -1.574367, 2.536686, -1.735297/


! *** polynomials for ammonium nitrate and ammonium sulfate are from:
!     Chan et al.1992, Atmospheric Environment (26A): 1661-1673.

      DATA kno3/0.2906, 6.83665, -26.9093, 46.6983, -38.803, 11.8837/
      DATA kso4/2.27515, -11.147, 36.3369, -64.2134, 56.8341, -20.0953/


! *** check range of per cent relative humidity
      irh = irhx
      irh = max(1,irh)
      irh = min(irh,100)
      aw = float(irh)/ & ! water activity = fractional relative h
        100.0
      tso4 = max(mso4,0.0)
      tnh4 = max(mnh4,0.0)
      tno3 = max(mno3,0.0)
      x = 0.0
! *** if there is non-zero sulfate calculate the molar ratio
      IF (tso4>0.0) THEN
        x = tnh4/tso4
      ELSE
! *** otherwise check for non-zero nitrate and ammonium
        IF (tno3>0.0 .AND. tnh4>0.0) x = 10.0
      END IF



! *** begin screen on x for calculating wh2o
      IF (x<1.0) THEN

        mfs0 = poly4(c0,aw)
        mfs1 = poly4(c1,aw)
        y0 = (1.0-mfs0)/mfs0
        y1 = (1.0-mfs1)/mfs1
        y = (1.0-x)*y0 + x*y1


      ELSE IF (x<1.5) THEN

        IF (irh>=40) THEN
          mfs1 = poly4(c1,aw)
          mfs15 = poly4(c15,aw)
          y1 = (1.0-mfs1)/mfs1
          y15 = (1.0-mfs15)/mfs15
          y = 2.0*(y1*(1.5-x)+y15*(x-1.0))
        ELSE
! *** set up for crystalization

! *** Crystallization is done as follows:
!      For 1.5 <= x, crystallization is assumed to occur at rh = 0.4
!      For x <= 1.0, crystallization is assumed to occur at an rh < 0.01
!      and since the code does not allow ar rh < 0.01, crystallization
!      is assumed not to occur in this range.
!      For 1.0 <= x <= 1.5 the crystallization curve is a straignt line
!      from a value of y15 at rh = 0.4 to a value of zero at y1. From
!      point B to point A in the diagram.
!      The algorithm does a double interpolation to calculate the amount
!      water.

!        y1(0.40)               y15(0.40)
!         +                     + Point B




!         +--------------------+
!       x=1                   x=1.5
!      Point A



          awc = 0.80*(x-1.0) ! rh along the crystallization curve.
          y = 0.0
          IF (aw>=awc) & ! interpolate using crystalization 
              THEN
            mfs1 = poly4(c1,0.40)
            mfs15 = poly4(c15,0.40)
            y140 = (1.0-mfs1)/mfs1
            y1540 = (1.0-mfs15)/mfs15
            y40 = 2.0*(y140*(1.5-x)+y1540*(x-1.0))
            yc = 2.0*y1540*(x-1.0) ! y along crystallization cur
            y = y40 - (y40-yc)*(0.40-aw)/(0.40-awc)
! end of checking for aw                             
          END IF

        END IF
! end of checking on irh                               
      ELSE IF (x<1.9999) THEN

        y = 0.0
        IF (irh>=40) THEN
          mfs15 = poly4(c15,aw)
          mfs2 = poly4(c2,aw)
          y15 = (1.0-mfs15)/mfs15
          y2 = (1.0-mfs2)/mfs2
          y = 2.0*(y15*(2.0-x)+y2*(x-1.5))

        END IF



! end of check for crystallization                    

      ELSE
! regime where ammonium sulfate and ammonium nitrate are in solution.

! *** following cf&s for both ammonium sulfate and ammonium nitrate
! *** check for crystallization here. their data indicate a 40% value
!     is appropriate.
! 1.9999 < x                                                 
        y2 = 0.0
        y3 = 0.0
        IF (irh>=40) THEN
          mfsso4 = poly6(kso4,aw)
          mfsno3 = poly6(kno3,aw)
          y2 = (1.0-mfsso4)/mfsso4
          y3 = (1.0-mfsno3)/mfsno3

        END IF


      END IF
! *** now set up output of wh2o

!      wh2o units are micrograms (liquid water) / cubic meter of air

! end of checking on x                                    
      IF (x<1.9999) THEN

        wh2o = y*(tso4*mwso4+mwnh4*tnh4)

      ELSE

! *** this is the case that all the sulfate is ammonium sulfate
!     and the excess ammonium forms ammonum nitrate

        wh2o = y2*tso4*mw2 + y3*tno3*mwano3

      END IF

      RETURN
    END SUBROUTINE awater
!//////////////////////////////////////////////////////////////////////

    SUBROUTINE coagrate(blksize,nspcsda,numcells,cblk,blkta,pdensn,pdensa,amu, &
        dgnuc,dgacc,knnuc,knacc,urn00,ura00,brna01,c30)
!***********************************************************************
!**    DESCRIPTION:  calculates aerosol coagulation rates for unimodal
!       and bimodal coagulation using E. Whitby 1990's prescription.

!.......   Rates for coaglulation:
!.......   Unimodal Rates:
!.......   URN00:  nuclei       mode 0th moment self-coagulation rate
!.......   URA00:  accumulation mode 0th moment self-coagulation rate

!.......   Bimodal Rates:  (only 1st order coeffs appear)
!.......   NA-- nuclei  with accumulation coagulation rates,
!.......   AN-- accumulation with nuclei coagulation rates
!.......   BRNA01:  rate for 0th moment ( d(nuclei mode 0) / dt  term)
!.......   BRNA31:           3rd        ( d(nuclei mode 3) / dt  term)
!**
!**
!**    Revision history:
!       prototype 1/95 by Uma and Carlie
!       Revised   8/95 by US for calculation of density from stmt func
!                 and collect met variable stmt funcs in one include fil
!      REVISED 7/25/96 by FSB to use block structure
!      REVISED 9/13/96 BY FSB for Uma's FIXEDBOTH case only.
!      REVISED 11/08/96 BY FSB the Whitby Shankar convention on signs
!                              changed. All coagulation coefficients
!                              returned with positive signs. Their
!                              linearization is also abandoned.
!                              Fixed values are used for the corrections
!                              to the free-molecular coagulation integra
!                              The code forces the harmonic means to be
!                              evaluated in 64 bit arithmetic on 32 bit
!     REVISED 11/14/96 BY FSB  Internal units are now MKS, moment / unit

!      REVISED 1/12/98 by FSB   C30 replaces BRNA31 as an array. This wa
!                              because BRNA31 can become zero on a works
!                              because of limited precision. With the ch
!                              aerostep to omit update of the 3rd moment
!                              C30 is the only variable now needed.
!                              the logic using ONE88 to force REAL*8 ari
!                              has been removed and all intermediates ar
!                              REAL*8.

!     IMPLICIT NONE

! dimension of arrays             
      INTEGER blksize
! actual number of cells in arrays
      INTEGER numcells

      INTEGER nspcsda

! nmber of species in CBLK        
      REAL cblk(blksize,nspcsda) ! main array of variables         
      REAL blkta(blksize) ! Air temperature [ K ]           
      REAL pdensn(blksize) ! average particel density in Aitk
      REAL pdensa(blksize) ! average particel density in accu
      REAL amu(blksize) ! atmospheric dynamic viscosity [ 
      REAL dgnuc(blksize) ! Aitken mode mean diameter [ m ] 
      REAL dgacc(blksize) ! accumulation mode mean diameter 
      REAL knnuc(blksize) ! Aitken mode Knudsen number      
      REAL knacc(blksize) 
! *** output:

! accumulation mode Knudsen number
      REAL urn00(blksize) ! intramodal coagulation rate (Ait
      REAL ura00(blksize) 
! intramodal coagulation rate (acc
      REAL brna01(blksize) ! intermodal coagulaton rate (numb
      REAL c30(blksize)                                                               ! by inter

! *** Local variables:
! intermodal 3rd moment transfer r
      REAL*8 kncnuc, & ! coeffs for unimodal NC coag rate      
        kncacc
      REAL*8 kfmnuc, & ! coeffs for unimodal FM coag rate      
        kfmacc
      REAL*8 knc, & ! coeffs for bimodal NC, FM coag rate   
        kfm
      REAL*8 bencnn, & ! NC 0th moment coag rate (both modes)  
        bencna
      REAL*8 & ! NC 3rd moment coag rate (nuc mode)    
        bencm3n
      REAL*8 befmnn, & ! FM 0th moment coag rate (both modes)  
        befmna
      REAL*8 & ! FM 3rd moment coag rate (nuc mode)    
        befm3n
      REAL*8 betann, & ! composite coag rates, mom 0 (both mode
        betana
      REAL*8 & ! intermodal coagulation rate for 3rd mo
        brna31
      REAL*8 & ! scratch subexpression                 
        s1
      REAL*8 t1, & ! scratch subexpressions                
        t2
      REAL*8 t16, & ! T1**6, T2**6                          
        t26
      REAL*8 rat, & ! ratio of acc to nuc size and its inver
        rin
      REAL*8 rsqt, & ! sqrt( rat ), rsqt**4                  
        rsq4
      REAL*8 rsqti, & ! sqrt( 1/rat ), sqrt( 1/rat**3 )       
        rsqi3
      REAL*8 & ! dgnuc**3                              
        dgn3
      REAL*8 & !                                 in 64 bit arithmetic
        dga3

! dgacc**3                              

      INTEGER lcell
! *** Fixed values for correctionss to coagulation
!      integrals for free-molecular case.
! loop counter                                      
      REAL*8 bm0
      PARAMETER (bm0=0.8D0)
      REAL*8 bm0i
      PARAMETER (bm0i=0.9D0)
      REAL*8 bm3i
      PARAMETER (bm3i=0.9D0)
      REAL*8 & ! approx Cunningham corr. factor      
        a
      PARAMETER (a=1.246D0)

!.......................................................................
!   begin body of subroutine  COAGRATE

!...........   Main computational grid-traversal loops
!...........   for computing coagulation rates.

! *** Both modes have fixed std devs.
      DO lcell = 1, & 
          numcells
! *** moment independent factors

!  loop on LCELL               
        s1 = two3*boltz*blkta(lcell)/amu(lcell)

! For unimodal coagualtion:

        kncnuc = s1
        kncacc = s1

        kfmnuc = sqrt(3.0*boltz*blkta(lcell)/pdensn(lcell))
        kfmacc = sqrt(3.0*boltz*blkta(lcell)/pdensa(lcell))

! For bimodal coagulation:

        knc = s1
        kfm = sqrt(6.0*boltz*blkta(lcell)/(pdensn(lcell)+pdensa(lcell)))



!...........   Begin unimodal coagulation rate calculations:

!...........   Near-continuum regime.

        dgn3 = dgnuc(lcell)**3
        dga3 = dgacc(lcell)**3

        t1 = sqrt(dgnuc(lcell))
        t2 = sqrt(dgacc(lcell))
        t16 = & ! = T1**6                               
          dgn3
        t26 = & 
          dga3
!.......   Note rationalization of fractions and subsequent cancellation
!.......   from the formulation in  Whitby et al. (1990)

! = T2**6                               
        bencnn = kncnuc*(1.0+esn08+a*knnuc(lcell)*(esn04+esn20))

        bencna = kncacc*(1.0+esa08+a*knacc(lcell)*(esa04+esa20))


!...........   Free molecular regime. Uses fixed value for correction
!               factor BM0


        befmnn = kfmnuc*t1*(en1+esn25+2.0*esn05)*bm0

        befmna = kfmacc*t2*(ea1+esa25+2.0*esa05)*bm0


!...........   Calculate half the harmonic mean between unimodal rates
!...........   free molecular and near-continuum regimes

! FSB       64 bit evaluation

        betann = bencnn*befmnn/(bencnn+befmnn)
        betana = bencna*befmna/(bencna+befmna)



        urn00(lcell) = betann
        ura00(lcell) = betana


! *** End of unimodal coagulation calculations.

!...........   Begin bimodal coagulation rate calculations:

        rat = dgacc(lcell)/dgnuc(lcell)
        rin = 1.0D0/rat
        rsqt = sqrt(rat)
        rsq4 = rat**2

        rsqti = 1.0D0/rsqt
        rsqi3 = rin*rsqti

!...........   Near-continuum coeffs:
!...........   0th moment nuc mode bimodal coag coefficient

        bencnn = knc*(2.0+a*knnuc(lcell)*(esn04+rat*esn16*esa04)+a*knacc(lcell &
          )*(esa04+rin*esa16*esn04)+(rat+rin)*esn04*esa04)

!...........   3rd moment nuc mode bimodal coag coefficient

        bencm3n = knc*dgn3*(2.0*esn36+a*knnuc(lcell)*(esn16+rat*esn04*esa04)+a &
          *knacc(lcell)*(esn36*esa04+rin*esn64*esa16)+rat*esn16*esa04+ &
          rin*esn64*esa04)



!...........   Free molecular regime coefficients:
!...........   Uses fixed value for correction
!               factor BM0I, BM3I


!...........   0th moment nuc mode coeff



        befmnn = kfm*bm0i*t1*(en1+rsqt*ea1+2.0*rat*en1*esa04+rsq4*esn09*esa16+ &
          rsqi3*esn16*esa09+2.0*rsqti*esn04*ea1)

!...........   3rd moment nuc mode coeff

        befm3n = kfm*bm3i*t1*t16*(esn49+rsqt*esn36*ea1+2.0*rat*esn25*esa04+ &
          rsq4*esn09*esa16+rsqi3*esn100*esa09+2.0*rsqti*esn64*ea1)


!...........   Calculate half the harmonic mean between bimodal rates
!...........   free molecular and near-continuum regimes

! FSB       Force 64 bit evaluation


        brna01(lcell) = bencnn*befmnn/(bencnn+befmnn)

        brna31 = bencm3n* & ! BRNA31 now is a scala
          befm3n/(bencm3n+befm3n)
        c30(lcell) = brna31*cblk(lcell,vac0)*cblk(lcell,vnu0)
!       print *,c30(lcell),brna31,cblk(lcell,vac0),cblk(lcell,vnu0)
                              ! 3d moment transfer by intermodal coagula

!         End bimodal coagulation rate.



      END DO
! end of main lop over cells                            
      RETURN
!------------------------------------------------------------------
    END SUBROUTINE coagrate
! subroutine  to find the roots of a cubic equation / 3rd order polynomi
! formulae can be found in numer. recip.  on page 145
!   kiran  developed  this version on 25/4/1990
!   dr. francis binkowski modified the routine on 6/24/91, 8/7/97
! ***
!234567
! coagrate                                     
    SUBROUTINE cubic(a2,a1,a0,nr,crutes)
!     IMPLICIT NONE
      INTEGER nr
      REAL*8 a2, a1, a0
      REAL crutes(3)
      REAL*8 qq, rr, a2sq, theta, sqrt3, one3rd
      REAL*8 dum1, dum2, part1, part2, part3, rrsq, phi, yy1, yy2, yy3
      REAL*8 costh, sinth
      DATA sqrt3/1.732050808/, one3rd/0.333333333/
!bs
      REAL*8 onebs
      PARAMETER (onebs=1.0)
!bs
      a2sq = a2*a2
      qq = (a2sq-3.*a1)/9.
      rr = (a2*(2.*a2sq-9.*a1)+27.*a0)/54.
! CASE 1 THREE REAL ROOTS or  CASE 2 ONLY ONE REAL ROOT
      dum1 = qq*qq*qq
      rrsq = rr*rr
      dum2 = dum1 - rrsq
      IF (dum2>=0.) THEN
! NOW WE HAVE THREE REAL ROOTS
        phi = sqrt(dum1)
        IF (abs(phi)<1.E-20) THEN
          print *, ' cubic phi small, phi = ',phi
          crutes(1) = 0.0
          crutes(2) = 0.0
          crutes(3) = 0.0
          nr = 0
          CALL wrf_error_fatal ( 'PHI < CRITICAL VALUE')
        END IF
        theta = acos(rr/phi)/3.0
        costh = cos(theta)
        sinth = sin(theta)
! *** use trig identities to simplify the expressions
! *** binkowski's modification
        part1 = sqrt(qq)
        yy1 = part1*costh
        yy2 = yy1 - a2/3.0
        yy3 = sqrt3*part1*sinth
        crutes(3) = -2.0*yy1 - a2/3.0
        crutes(2) = yy2 + yy3
        crutes(1) = yy2 - yy3
! *** SET NEGATIVE ROOTS TO A LARGE POSITIVE VALUE
        IF (crutes(1)<0.0) crutes(1) = 1.0E9
        IF (crutes(2)<0.0) crutes(2) = 1.0E9
        IF (crutes(3)<0.0) crutes(3) = 1.0E9
! *** put smallest positive root in crutes(1)
        crutes(1) = min(crutes(1),crutes(2),crutes(3))
        nr = 3
!     NOW HERE WE HAVE ONLY ONE REAL ROOT
      ELSE
! dum IS NEGATIVE                                           
        part1 = sqrt(rrsq-dum1)
        part2 = abs(rr)
        part3 = (part1+part2)**one3rd
        crutes(1) = -sign(onebs,rr)*(part3+(qq/part3)) - a2/3.
!bs     &        -sign(1.0,rr) * ( part3 + (qq/part3) ) - a2/3.
        crutes(2) = 0.
        crutes(3) = 0.
!IAREV02...ADDITIONAL CHECK on NEGATIVE ROOTS
! *** SET NEGATIVE ROOTS TO A LARGE POSITIVE VALUE
!     if(crutes(1) .lt. 0.0) crutes(1) = 1.0e9
        nr = 1
      END IF
      RETURN
!///////////////////////////////////////////////////////////////////////
    END SUBROUTINE cubic

!    Calculate the aerosol chemical speciation and water content.

! cubic                                                     
    SUBROUTINE eql3(blksize,nspcsda,numcells,cblk,blkta,blkrh,igrid,jgrid,kgrid)
!***********************************************************************
!**    DESCRIPTION:
!	Calculates the distribution of ammonia/ammonium, nitric acid/nitrate,
!	and water between the gas and aerosol phases as the total sulfate,
!	ammonia, and nitrate concentrations, relative humidity and
!	temperature change.  The evolution of the aerosol mass concentration
!	due to the change in aerosol chemical composition is calculated.
!**    REVISION HISTORY:
!       prototype 1/95 by Uma and Carlie
!       Revised   8/95 by US to calculate air density in stmt func
!                 and collect met variable stmt funcs in one include fil
!       Revised 7/26/96 by FSB to use block concept.
!       Revise 12/1896 to do do i-mode calculation.
!**********************************************************************

!     IMPLICIT NONE


! dimension of arrays             
      INTEGER blksize
! actual number of cells in arrays
      INTEGER numcells
! nmber of species in CBLK        
      INTEGER nspcsda,igrid,jgrid,kgrid
      REAL cblk(blksize,nspcsda) 
! *** Meteorological information in blocked arays:

! main array of variables         
      REAL blkta(blksize) ! Air temperature [ K ]                   
      REAL blkrh(blksize) 

! Fractional relative humidity            

      INTEGER lcell
! loop counter                                   
! air temperature                             
      REAL temp
!iamodels3
      REAL rh
! relative humidity                           
      REAL so4, no3, nh3, nh4, hno3
      REAL aso4, ano3, ah2o, anh4, gnh3, gno3
! Fraction of dry sulfate mass in i-mode         
      REAL fraci
!.......................................................................
      REAL fracj
!
! ISOROPIA variables double precision
!
      real(kind=8) wi(5),wt(5),wt_save(5)
      real(kind=8) rhi,tempi,cntrl(2)
      real(kind=8) gas(3),aerliq(12),aersld(9),other(6)
      character*15 scasi

!      WRITE(20,*) ' IN EQL 3 '



! Fraction of dry sulfate mass in j-mode         
      DO lcell = 1, &
          numcells
! *** Fetch temperature, fractional relative humidity, and
!     air density

!  loop on cells                    
        temp = blkta(lcell)
        rh = blkrh(lcell)

        rhi = amin1( rh,0.995 )
        tempi = temp
        cntrl(1) = 0.d0         ! 0 = forward problem
        cntrl(2) = 0.d0         ! 0 = solids and liquid allowed

        wi(1) = (cblk(lcell,vnaaj)  + cblk(lcell,vnaai))/mw_na_aer*1.e-6      ! sodium

        wi(2) = (cblk(lcell,vsulf)/(mw_so4_aer+2.) +                         &
                (cblk(lcell,vso4aj) +  cblk(lcell,vso4ai))/mw_so4_aer)*1.e-6        ! sulfate

        wi(3) = (cblk(lcell,vnh3)/(mw_nh4_aer-1.) +                               &
                (cblk(lcell,vnh4aj) +  cblk(lcell,vnh4ai))/mw_nh4_aer)*1.e-6        ! ammoinum

        wi(4) = (cblk(lcell,vhno3)/(mw_no3_aer+1.) +                             &
                (cblk(lcell,vno3aj) +  cblk(lcell,vno3ai))/mw_no3_aer)*1.e-6   ! nitrate

       
!       wi(5) = (cblk(lcell,vhcl)/(mw_cl_aer-1.) +                               &
        wi(5) = (cblk(lcell,vhcl)/(mw_cl_aer+1.) +                               &
                (cblk(lcell,vclaj)  + cblk(lcell,vclai))/mw_cl_aer)*1.e-6      ! chloride

! Following added: wi should be positive
        wi(1) = max(wi(1),0.)
        wi(2) = max(wi(2),0.)
        wi(3) = max(wi(3),0.)
        wi(4) = max(wi(4),0.)
        wi(5) = max(wi(5),0.)

        wt_save(1) = wi(1) ! sodium
        wt_save(2) = wi(2) ! sulfate
        wt_save(3) = wi(3) ! ammoinum
        wt_save(4) = wi(4) ! nitrate
        wt_save(5) = wi(5) ! chloride
        if(igrid.eq.28.and.jgrid.eq.24.and.kgrid.eq.1)then
         print *,vhcl,vclai
         print *,wi(1),wi(2),wi(3),wi(4),wi(5)
        endif

       call isoropia(wi,rhi,tempi,cntrl,wt,gas,aerliq,aersld,scasi,other)


! *** the following is an interim procedure. Assume the i-mode has the
!     same relative mass concentrations as the total mass. Use SO4 as
!     the surrogate. 


! *** update gas / vapor phase
        gas(1) = min(gas(1),wt_save(3))
        gas(2) = min(gas(2),wt_save(4))
        gas(3) = min(gas(3),wt_save(5))

        gas(1) = max(gas(1),0.)
        gas(2) = max(gas(2),0.)
        gas(3) = max(gas(3),0.)

!        cblk(lcell,vnh3)  = gas(1)*1.e6*17.
!        cblk(lcell,vhno3) = gas(2)*1.e6*63.
!        cblk(lcell,vhcl) = gas(3)*1.e6*36.
        cblk(lcell,vnh3)  = gas(1)*1.e6*(mw_nh4_aer-1.)
        cblk(lcell,vhno3) = gas(2)*1.e6*(mw_no3_aer+1.)
        cblk(lcell,vhcl) = gas(3)*1.e6*(mw_cl_aer+1.)
        if(igrid.eq.28.and.jgrid.eq.24.and.kgrid.eq.1)then
         print *,vhcl,vnh3,vhno3
         print *,cblk(lcell,vnh3),cblk(lcell,vhno3),cblk(lcell,vhcl)
        endif

! *** get modal fraction
        fraci = cblk(lcell,vso4ai)/(cblk(lcell,vso4aj)+cblk(lcell,vso4ai))

        fraci = min(fraci,1.0)
        fraci = max(fraci,0.0)

        fracj = 1.0 - fraci

! *** update do i-mode

!     correct mapping from (mol m-3) to (ug m-3)
        aerliq(8) = max(aerliq(8),0.)

        cblk(lcell,vh2oai) = fraci*aerliq(8)*18.*1.e6
        cblk(lcell,vnh4ai) = fraci*(wt_save(3) - gas(1))*mw_nh4_aer*1.e6
        cblk(lcell,vno3ai) = fraci*(wt_save(4) - gas(2))*mw_no3_aer*1.e6
        cblk(lcell,vclai)  = fraci*(wt_save(5) - gas(3))*mw_cl_aer*1.e6
        cblk(lcell,vnaai)  = fraci*wi(1)*mw_na_aer*1.e6

!        cblk(lcell,vh2oai) = fraci*aerliq(8)*18.*1.e6
!        cblk(lcell,vnh4ai) = fraci*(wt_save(3) - gas(1))
!        cblk(lcell,vno3ai) = fraci*(wt_save(4) - gas(2))
!        cblk(lcell,vclai)  = fraci*(wt_save(5) - gas(3))
!        cblk(lcell,vnaai)  = fraci*wi(1)

! *** update accumulation mode:
! correct mapping from (mol m-3) to (ug m-3)
        cblk(lcell,vh2oaj) = fracj*aerliq(8)*18.*1.e6
        cblk(lcell,vnh4aj) = fracj*(wt_save(3) - gas(1))*mw_nh4_aer*1.e6
        cblk(lcell,vno3aj) = fracj*(wt_save(4) - gas(2))*mw_no3_aer*1.e6
        cblk(lcell,vclaj)  = fracj*(wt_save(5) - gas(3))*mw_cl_aer*1.e6
        cblk(lcell,vnaaj)  = fracj*wi(1)*mw_na_aer*1.e6

!        cblk(lcell,vh2oaj) = fracj*aerliq(8)*18.*1.e6
!        cblk(lcell,vnh4aj) = fracj*(wt_save(3) - gas(1))
!        cblk(lcell,vno3aj) = fracj*(wt_save(4) - gas(2))
!        cblk(lcell,vclaj)  = fracj*(wt_save(5) - gas(3))
!        cblk(lcell,vnaaj)  = fracj*wi(1)
        if(igrid.eq.28.and.jgrid.eq.24.and.kgrid.eq.1)then
         print *,vh2oaj,vnh4aj,vno3aj,vclaj,vnaaj
         print *,cblk(lcell,vnh4aj),cblk(lcell,vno3aj),cblk(lcell,vclaj),aerliq(8)
        endif



      END DO
!  end loop on cells

      RETURN

!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
    END SUBROUTINE eql3
! eql3                                                    
!    Calculate the aerosol chemical speciation and water content.

! cubic                                                     
    SUBROUTINE eql4(blksize,nspcsda,numcells,cblk,blkta,blkrh)
!***********************************************************************
!**    DESCRIPTION:
!       Calculates the distribution of ammonia/ammonium, nitric acid/nitrate,
!       and water between the gas and aerosol phases as the total sulfate,
!       ammonia, and nitrate concentrations, relative humidity and
!       temperature change.  The evolution of the aerosol mass concentration
!       due to the change in aerosol chemical composition is calculated.
!**    REVISION HISTORY:
!       prototype 1/95 by Uma and Carlie
!       Revised   8/95 by US to calculate air density in stmt func
!                 and collect met variable stmt funcs in one include fil
!       Revised 7/26/96 by FSB to use block concept.
!       Revise 12/1896 to do do i-mode calculation.
!**********************************************************************

!     IMPLICIT NONE


! dimension of arrays             
      INTEGER blksize
! actual number of cells in arrays
      INTEGER numcells
! nmber of species in CBLK        
      INTEGER nspcsda
      REAL cblk(blksize,nspcsda) 
! *** Meteorological information in blocked arays:

! main array of variables         
      REAL blkta(blksize) ! Air temperature [ K ]                   
      REAL blkrh(blksize) 

! Fractional relative humidity            

      INTEGER lcell
! loop counter                                   
! air temperature                             
      REAL temp
!iamodels3
      REAL rh
! relative humidity                           
      REAL so4, no3, nh3, nh4, hno3
      REAL aso4, ano3, ah2o, anh4, gnh3, gno3
! Fraction of dry sulfate mass in i-mode         
      REAL fraci
!.......................................................................
      REAL fracj
! Fraction of dry sulfate mass in j-mode         
      DO lcell = 1, &
          numcells
! *** Fetch temperature, fractional relative humidity, and
!     air density

!  loop on cells                    
        temp = blkta(lcell)
        rh = blkrh(lcell)

! *** the following is an interim procedure. Assume the i-mode has the
!     same relative mass concentrations as the total mass. Use SO4 as
!     the surrogate. The results of this should be the same as those
!     from the original RPM.

! *** do total aerosol
        so4 = cblk(lcell,vso4aj) + cblk(lcell,vso4ai)

!iamodels3
        no3 = cblk(lcell,vno3aj) + cblk(lcell,vno3ai)
!    &                        + CBLK(LCELL, VHNO3)
      
        hno3 = cblk(lcell,vhno3)

!iamodels3

        nh3 = cblk(lcell,vnh3)
        
        nh4 = cblk(lcell,vnh4aj) + cblk(lcell,vnh4ai)
!    &                        + CBLK(LCELL, VNH3)

!bs           CALL rpmares(SO4,HNO3,NO3,NH3,NH4,RH,TEMP,
!bs     &             ASO4,ANO3,AH2O,ANH4,GNH3,GNO3)
!bs
!bs * call old version of rpmares
!bs
        CALL rpmares_old(so4,hno3,no3,nh3,nh4,rh,temp,aso4,ano3,ah2o,anh4, &
          gnh3,gno3)
!bs

! *** get modal fraction
        fraci = cblk(lcell,vso4ai)/(cblk(lcell,vso4aj)+cblk(lcell,vso4ai))
        fracj = 1.0 - fraci

! *** update do i-mode

        cblk(lcell,vh2oai) = fraci*ah2o
        cblk(lcell,vnh4ai) = fraci*anh4
        cblk(lcell,vno3ai) = fraci*ano3

! *** update accumulation mode:

        cblk(lcell,vh2oaj) = fracj*ah2o
        cblk(lcell,vnh4aj) = fracj*anh4
        cblk(lcell,vno3aj) = fracj*ano3


! *** update gas / vapor phase
        cblk(lcell,vnh3) = gnh3
        cblk(lcell,vhno3) = gno3

      END DO
!  end loop on cells                     
      RETURN

!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
    END SUBROUTINE eql4
! eql4                                                    

    SUBROUTINE fdjac(n,x,fjac,ct,cs,imw)
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs                                                                    !
!bs  Description:                                                      !
!bs                                                                    !
!bs  Get the Jacobian of the function                                  !
!bs                                                                    !
!bs         ( a1 * X1^2 + b1 * X1 + c1 )                               !
!bs         ( a2 * X2^2 + b2 * X1 + c2 )                               !
!bs         ( a3 * X3^2 + b3 * X1 + c3 )                               !
!bs  F(X) = ( a4 * X4^2 + b4 * X1 + c4 ) = 0.                          !
!bs         ( a5 * X5^2 + b5 * X1 + c5 )                               !
!bs         ( a6 * X6^2 + b6 * X1 + c6 )                               !
!bs                                                                    !
!bs   a_i = IMW_i                                                      !
!bs   b_i = SUM(X_j * IMW_j)_j.NE.i + CSAT_i * IMX_i - CTOT_i * IMW_i  !
!bs   c_i = - CTOT_i * [ SUM(X_j * IMW_j)_j.NE.i + M ]                 !
!bs                                                                    !
!bs          delta F_i    ( 2. * a_i * X_i + b_i           if i .EQ. j !
!bs  J_ij = ----------- = (                                            !
!bs          delta X_j    ( X_i * IMW_j - CTOT_i * IMW_j   if i .NE. j !
!bs                                                                    !
!bs                                                                    !
!bs  Called by:       NEWT                                             !
!bs                                                                    !
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs
!     IMPLICIT NONE
!bs
!bs
!dimension of problem                   
      INTEGER n
      REAL x(n) !bs
!     INTEGER NP                !bs maximum expected value of N
!     PARAMETER (NP = 6)
!bs initial guess of CAER               
      REAL ct(np)
      REAL cs(np)
      REAL imw(np)
!bs
      REAL fjac(n,n)
!bs
      INTEGER i, & !bs loop index                          
        j
      REAL a(np)
      REAL b(np)
      REAL b1(np)
      REAL b2(np)
      REAL sum_jnei
!bs
      DO i = 1, n
        a(i) = imw(i)
        sum_jnei = 0.
        DO j = 1, n
          sum_jnei = sum_jnei + x(j)*imw(j)
        END DO
        b1(i) = sum_jnei - (x(i)*imw(i))
        b2(i) = cs(i)*imw(i) - ct(i)*imw(i)
        b(i) = b1(i) + b2(i)
      END DO
      DO j = 1, n
        DO i = 1, n
          IF (i==j) THEN
            fjac(i,j) = 2.*a(i)*x(i) + b(i)
          ELSE
            fjac(i,j) = x(i)*imw(j) - ct(i)*imw(j)
          END IF
        END DO
      END DO
!bs
      RETURN
    END SUBROUTINE fdjac
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
    FUNCTION fmin(x,fvec,n,ct,cs,imw,m)
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs                                                                    !
!bs  Description:                                                      !
!bs                                                                    !
!bs  Adopted from Numerical Recipes in FORTRAN, Chapter 9.7, 2nd ed.   !
!bs                                                                    !
!bs  Returns f = 0.5 * F*F at X. SR FUNCV(N,X,F) is a fixed name,      !
!bs  user-supplied routine that returns the vector of functions at X.  !
!bs  The common block NEWTV communicates the function values back to   !
!bs  NEWT.                                                             !
!bs                                                                    !
!bs  Called by:       NEWT                                             !
!bs                                                                    !
!bs  Calls:           FUNCV                                            !
!bs                                                                    !
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!

!     IMPLICIT NONE

!bs
!bs
      INTEGER n
!     INTEGER NP
!     PARAMETER (NP = 6)
      REAL ct(np)
      REAL cs(np)
      REAL imw(np)
      REAL m,fmin
      REAL x(*), fvec(np)


      INTEGER i
      REAL sum

      CALL funcv(n,x,fvec,ct,cs,imw,m)
      sum = 0.
      DO i = 1, n
        sum = sum + fvec(i)**2
      END DO
      fmin = 0.5*sum
      RETURN
    END FUNCTION fmin
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
    SUBROUTINE funcv(n,x,fvec,ct,cs,imw,m)
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs                                                                    !
!bs  Description:                                                      !
!bs                                                                    !
!bs  Called by:       FMIN                                             !
!bs                                                                    !
!bs  Calls:           None                                             !
!bs                                                                    !
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs
!     IMPLICIT NONE
!bs
!bs
      INTEGER n
      REAL x(*)
      REAL fvec(n)
!bs
!     INTEGER NP
!     PARAMETER (NP = 6)
      REAL ct(np)
      REAL cs(np)
      REAL imw(np)
      REAL m
!bs
      INTEGER i, j
      REAL sum_jnei
      REAL a(np)
      REAL b(np)
      REAL c(np)
!bs
      DO i = 1, n
        a(i) = imw(i)
        sum_jnei = 0.
        DO j = 1, n
          sum_jnei = sum_jnei + x(j)*imw(j)
        END DO
        sum_jnei = sum_jnei - (x(i)*imw(i))
        b(i) = sum_jnei + cs(i)*imw(i) - ct(i)*imw(i)
        c(i) = -ct(i)*(sum_jnei+m)
        fvec(i) = a(i)*x(i)**2 + b(i)*x(i) + c(i)
      END DO
!bs
      RETURN
    END SUBROUTINE funcv
    REAL FUNCTION getaf(ni,nj,dgni,dgnj,xlsgi,xlsgj,sqrt2)
! *** set up new processor for renaming of particles from i to j modes
!     IMPLICIT NONE
      REAL aa, bb, cc, disc, qq, alfa, l, yji
      REAL ni, nj, dgni, dgnj, xlsgi, xlsgj, sqrt2

      alfa = xlsgi/xlsgj
      yji = log(dgnj/dgni)/(sqrt2*xlsgi)
      aa = 1.0 - alfa*alfa
      l = log(alfa*nj/ni)
      bb = 2.0*yji*alfa*alfa
      cc = l - yji*yji*alfa*alfa
      disc = bb*bb - 4.0*aa*cc
      IF (disc<0.0) THEN
        getaf = - & ! error in intersection                     
          5.0
        RETURN
      END IF
      qq = -0.5*(bb+sign(1.0,bb)*sqrt(disc))
      getaf = cc/qq
      RETURN
! *** subroutine to implement Kulmala, Laaksonen, Pirjola
    END FUNCTION getaf
!     Parameterization for sulfuric acid/water
!     nucleation rates, J. Geophys. Research (103), pp 8301-8307,
!     April 20, 1998.

!ia rev01 27.04.99 changes made to calculation of MDOT see RBiV p.2f
!ia rev02 27.04.99 security check on MDOT > SO4RAT


!ia      subroutine klpnuc( Temp, RH, H2SO4,NDOT, MDOT, M2DOT)
! getaf                                                     
    SUBROUTINE klpnuc(temp,rh,h2so4,ndot1,mdot1,so4rat)
!     IMPLICIT NONE


! *** Input:

! ambient temperature [ K ]                            
      REAL temp
! fractional relative humidity                         
      REAL rh
! sulfuric acid concentration [ ug / m**3 ]            
      REAL h2so4

      REAL so4rat
! *** Output:

!sulfuric acid production rate [ ug / ( m**3 s )]     
! particle number production rate [ # / ( m**3 s )]   
      REAL ndot1
! particle mass production rate [ ug / ( m**3 s )]    
      REAL mdot1
                 ! [ m**2 / ( m**3 s )]
      REAL m2dot

! *** Internal:

! *** NOTE, all units are cgs internally.
! particle second moment production rate               

      REAL ra
! fractional relative acidity                           
! sulfuric acid vaper concentration [ cm ** -3 ]        
      REAL nav
! water vapor concentration   [ cm ** -3 ]              
      REAL nwv
! equilibrium sulfuric acid vapor conc. [ cm ** -3 ]    
      REAL nav0
                ! to produce a nucleation rate of 1 [ cm ** -3  s ** -1
      REAL nac
! critical sulfuric acid vapor concentration [ cm ** -3 
! mole fractio of the critical nucleus                  
      REAL xal
      REAL nsulf, & ! see usage                                    
        delta
      REAL*8 & ! factor to calculate Jnuc                             
        chi
      REAL*8 & 
        jnuc
! nucleation rate [ cm ** -3  s ** -1 ]               
      REAL tt, & ! dummy variables for statement functions              
        rr
      REAL pi
      PARAMETER (pi=3.14159265)

      REAL pid6
      PARAMETER (pid6=pi/6.0)

! avogadro's constant [ 1/mol ]                   
      REAL avo
      PARAMETER (avo=6.0221367E23)

! universal gas constant [ j/mol-k ]         
      REAL rgasuniv
      PARAMETER (rgasuniv=8.314510)

! 1 atmosphere in pascals                               
      REAL atm
      PARAMETER (atm=1013.25E+02)

! formula weight for h2so4 [ g mole **-1 ]          
      REAL mwh2so4
      PARAMETER (mwh2so4=98.07948)

! diameter of a 3.5 nm particle in cm                  
      REAL d35
      PARAMETER (d35=3.5E-07)
      REAL d35sq
      PARAMETER (d35sq=d35*d35)
! volume of a 3.5 nm particle in cm**3                 
      REAL v35
      PARAMETER (v35=pid6*d35*d35sq)
!ia rev01

      REAL mp
! ***  conversion factors:
! mass of sulfate in a 3.5 nm particle               
                     ! number per cubic cm.
      REAL ugm3_ncm3
! micrograms per cubic meter to                    
      PARAMETER (ugm3_ncm3=(avo/mwh2so4)*1.0E-12)
!ia rev01
! molecules to micrograms                          
      REAL nc_ug
      PARAMETER (nc_ug=(1.0E6)*mwh2so4/avo)



! *** statement functions **************

      REAL pdens, & 
        rho_p
! particle density [ g / cm**3]                 
      REAL ad0, ad1, ad2, & 
        ad3
! coefficients for density expression    
      PARAMETER (ad0=1.738984,ad1=-1.882301,ad2=2.951849,ad3=-1.810427) 
! *** Nair and Vohra, Growth of aqueous sulphuric acid droplets
!     as a function of relative humidity,
!     J. Aerosol Science, 6, pp 265-271, 1975.

!ia rev01

! fit to Nair & Vohra data                  
                ! the mass of sulfate in a 3.5 nm particle
      REAL mp35
! arithmetic statement function to compute              
      REAL a0, a1, a2, & ! coefficients for cubic in mp35                 
        a3
      PARAMETER (a0=1.961385E2,a1=-5.564447E2,a2=8.828801E2,a3=-5.231409E2)

      REAL ph2so4, &                         ! for h2so4 and h2o vapor pressures [ Pa ]
        ph2o


! arithmetic statement functions                
      pdens(rr) = ad0 + rr*(ad1+rr*(ad2+rr*ad3))

      ph2o(tt) = exp(77.34491296-7235.4246512/tt-8.2*log(tt)+tt*5.7113E-03)

      ph2so4(tt) = exp(27.78492066-10156.0/tt)

! *** both ph2o and ph2so4 are  as in Kulmala et al.  paper

!ia rev01

! *** function for the mass of sulfate in   a 3.5 nm sphere
! *** obtained from a fit to the number of sulfate monomers in
!     a 3.5 nm particle. Uses data from Nair & Vohra
      mp35(rr) = nc_ug*(a0+rr*(a1+rr*(a2+rr*a3)))



! *** begin code:

!     The 1.0e-6 factor in the following converts from MKS to cgs units

! *** get water vapor concentration [ molecles / cm **3 ]

      nwv = rh*ph2o(temp)/(rgasuniv*temp)*avo*1.0E-6

! *** calculate the equilibrium h2so4 vapor concentration.

! *** use Kulmala corrections:


! ***

      nav0 = ph2so4(temp)/(rgasuniv*temp)*avo*1.0E-6

! *** convert sulfuric acid vapor concentration from micrograms
!     per cubic meter to molecules per cubic centimeter.

      nav = ugm3_ncm3*h2so4


! *** calculate critical concentration of sulfuric acid vapor

      nac = exp(-14.5125+0.1335*temp-10.5462*rh+1958.4*rh/temp)

! *** calculate relative acidity

      ra = nav/nav0

! *** calculate temperature correction

      delta = 1.0 + (temp-273.15)/273.14

! *** calculate molar fraction

      xal = 1.2233 - 0.0154*ra/(ra+rh) + 0.0102*log(nav) - 0.0415*log(nwv) + &
        0.0016*temp

! *** calculate Nsulf
      nsulf = log(nav/nac)

! *** calculate particle produtcion rate [ # / cm**3 ]

      chi = 25.1289*nsulf - 4890.8*nsulf/temp - 1743.3/temp - &
        2.2479*delta*nsulf*rh + 7643.4*xal/temp - 1.9712*xal*delta/rh

      jnuc = exp(chi) 
! [ # / cm**3 ]                                   
      ndot1 = (1.0E06)*jnuc
!      write(91,*) ' inside klpnuc '
!     write(91,*) ' Jnuc = ', Jnuc
!     write(91,*) ' NDOT = ', NDOT1

! *** calculate particle density


      rho_p = pdens(rh)

!     write(91,*) ' rho_p =', rho_p

! *** get the mass of sulfate in a 3.5 nm particle

      mp = mp35(rh)                      ! in a 3.5 nm particle at ambient RH

! *** calculate mass production rate [ ug / m**3]
!     assume that the particles are 3.5 nm in diameter.


!     MDOT1 =  (1.0E12) * rho_p * v35 * Jnuc

!ia rev01

! number of micrograms of sulfate                  
      mdot1 = mp*ndot1

!ia rev02

      IF (mdot1>so4rat) THEN

        mdot1 = & 
          so4rat
! limit nucleated mass by available ma
        ndot1 = mdot1/ & 
          mp
! adjust DNDT to this                 
      END IF


      IF (mdot1==0.) ndot1 = 0.

! *** calculate M2 production rate [ m**2 / (m**3 s)]

      m2dot = 1.0E-04*d35sq*ndot1

      RETURN

    END SUBROUTINE klpnuc
    SUBROUTINE lnsrch(ctot,n,xold,fold,g,p,x,f,stpmax,check,func, &
     fvec,ct,cs,imw,m)
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs                                                                    !
!bs  Description:                                                      !
!bs                                                                    !
!bs  Adopted from Numerical Recipes in FORTRAN, Chapter 9.7, 2nd ed.   !
!bs                                                                    !
!bs  Given an n-dimensional point XOLD(1:N), the value of the function !
!bs  and gradient there, FOLD and G(1:N), and a direction P(1:N),      !
!bs  finds a new point X(1:N) along the direction P from XOLD where    !
!bs  the function FUNC has decreased 'sufficiently'. The new function  !
!bs  value is returned in F. STPMAX is an input quantity that limits   !
!bs  the length of the steps so that you do not try to evaluate the    !
!bs  function in regions where it is undefined or subject to overflow. !
!bs  P is usually the Newton direction. The output quantity CHECK is   !
!bs  false on a normal; exit. It is true when X is too close to XOLD.  !
!bs  In a minimization algorithm, this usually signals convergence and !
!bs  can be ignored. However, in a zero-finding algorithm the calling  !
!bs  program should check whether the convergence is spurious.         !
!bs                                                                    !
!bs  Called by:       NEWT                                             !
!bs                                                                    !
!bs  Calls:           FUNC                                             !
!bs                                                                    !
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!

!     IMPLICIT NONE
!bs
!bs
      INTEGER n
      LOGICAL check
      REAL f, fold, stpmax
      REAL g(n), p(n), x(n), xold(n)
      REAL func
      REAL ctot(n)
      REAL alf
      REAL ct(np)
      REAL cs(np)
      REAL imw(np)
      REAL fvec(n)
      REAL m

      PARAMETER (alf=1.E-04)

      EXTERNAL func

      INTEGER i
      REAL a, alam, alam2, alamin, b, disc
      REAL f2, fold2, rhs1, rhs2, slope
      REAL sum, temp, test, tmplam

      check = .FALSE.
      sum = 0.
      DO i = 1, n
        sum = sum + p(i)*p(i)
      END DO
      sum = sqrt(sum)
      IF (sum>stpmax) THEN
        DO i = 1, n
          p(i) = p(i)*stpmax/sum
        END DO
      END IF
      slope = 0.
      DO i = 1, n
        slope = slope + g(i)*p(i)
      END DO
      test = 0.
      DO i = 1, n
        temp = abs(p(i))/max(abs(xold(i)),1.)
        IF (temp>test) test = temp
      END DO
      alamin = tolx/test
      alam = 1.

10    CONTINUE

!bs
!bs * avoid negative concentrations and set upper limit given by CTOT.
!bs
      DO i = 1, n
        x(i) = xold(i) + alam*p(i)
        IF (x(i)<=0.) x(i) = conmin
        IF (x(i)>ctot(i)) x(i) = ctot(i)
      END DO
      f = func(x,fvec,n,ct,cs,imw,m)
      IF (alam<alamin) THEN
        DO i = 1, n
          x(i) = xold(i)
        END DO
        check = .TRUE.
        RETURN
      ELSE IF (f<=fold+alf*alam*slope) THEN
        RETURN
      ELSE
        IF (alam==1.) THEN
          tmplam = -slope/(2.*(f-fold-slope))
        ELSE
          rhs1 = f - fold - alam*slope
          rhs2 = f2 - fold2 - alam2*slope
          a = (rhs1/alam**2-rhs2/alam2**2)/(alam-alam2)
          b = (-alam2*rhs1/alam**2+alam*rhs2/alam2**2)/(alam-alam2)
          IF (a==0.) THEN
            tmplam = -slope/(2.*b)
          ELSE
            disc = b*b - 3.*a*slope
            tmplam = (-b+sqrt(disc))/(3.*a)
          END IF
          IF (tmplam>0.5*alam) tmplam = 0.5*alam
        END IF
      END IF
      alam2 = alam
      f2 = f
      fold2 = fold
      alam = max(tmplam,0.1*alam)
      GO TO 10

    END SUBROUTINE lnsrch
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
    SUBROUTINE lubksb(a,n,np,indx,b)
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs                                                                    !
!bs  Description:                                                      !
!bs                                                                    !
!bs  Adopted from Numerical Recipes in FORTRAN, Chapter 2.3, 2nd ed.   !
!bs                                                                    !
!bs  Solves the set of N linear equations A * X = B. Here A is input,  !
!bs  not as the matrix A but rather as its LU decomposition,           !
!bs  determined by the routine LUDCMP. B(1:N) is input as the right-   !
!bs  hand side vector B, and returns with the solution vector X. A, N, !
!bs  NP, and INDX are not modified by this routine and can be left in  !
!bs  place for successive calls with different right-hand sides B.     !
!bs  This routine takes into account the possibilitythat B will begin  !
!bs  with many zero elements, so it is efficient for use in matrix     !
!bs  inversion.                                                        !
!bs                                                                    !
!bs  Called by:       NEWT                                             !
!bs                                                                    !
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs
!     IMPLICIT NONE
!bs
      INTEGER n, np, indx(n)
      REAL a(np,np), b(n)

      INTEGER i, ii, j, ll
      REAL sum

      ii = 0
      DO i = 1, n
        ll = indx(i)
        sum = b(ll)
        b(ll) = b(i)
        IF (ii/=0) THEN
          DO j = ii, i - 1
            sum = sum - a(i,j)*b(j)
          END DO
        ELSE IF (sum/=0) THEN
          ii = i
        END IF
        b(i) = sum
      END DO
      DO i = n, 1, -1
        sum = b(i)
        DO j = i + 1, n
          sum = sum - a(i,j)*b(j)
        END DO
        b(i) = sum/a(i,i)
      END DO

      RETURN
    END SUBROUTINE lubksb
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
    SUBROUTINE ludcmp(a,n,np,indx,d,klev)
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs                                                                    !
!bs  Description:                                                      !
!bs                                                                    !
!bs  Adopted from Numerical Recipes in FORTRAN, Chapter 2.3, 2nd ed.   !
!bs                                                                    !
!bs  Equation (2.3.14) Numerical Recipes, p 36:                        !
!bs   | b_11 b_12 b_13 b_14 |                                          !
!bs   | a_21 b_22 b_23 b_24 |                                          !
!bs   | a_31 a_32 b_33 b_34 |                                          !
!bs   | a_41 a_42 a_43 b_44 |                                          !
!bs                                                                    !
!bs  Given a matrix A(1:N,1:N), with physical dimension NP by NP, this !
!bs  routine replaces it by the LU decomposition of a rowwise          !
!bs  permutation of itself. A and N are input. A is output arranged as !
!bs  in equation (2.3.14) above; INDX(1:N) is an output vector that    !
!bs  records vector that records the row permutation effected by the   !
!bs  partial pivoting; D is output as +-1 depending on whether the     !
!bs  number of row interchanges was even or odd, respectively. This    !
!bs  routine is used in combination with SR LUBKSB to solve linear     !
!bs  equations or invert a matrix.                                     !
!bs                                                                    !
!bs  Called by:       NEWT                                             !
!bs                                                                    !
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs
!     IMPLICIT NONE
!bs
      INTEGER n, np, indx(n)
      INTEGER nmax
      PARAMETER (nmax=10) !largest expected N                    
      REAL d, a(np,np)
      REAL tiny
      PARAMETER (tiny=1.0E-20)

      INTEGER i, imax, j, k
      REAL aamax, dum, sum, vv(nmax)
      integer klev

      d = 1
      DO i = 1, n
        aamax = 0.
        DO j = 1, n
          IF (abs(a(i,j))>aamax) aamax = abs(a(i,j))
        END DO
        IF (aamax==0) THEN
          print *, 'Singular matrix in ludcmp, klev = ',klev
          a(1,1)=epsilc
!         STOP
        END IF
        vv(i) = 1./aamax
      END DO
      DO j = 1, n
        DO i = 1, j - 1
          sum = a(i,j)
          DO k = 1, i - 1
            sum = sum - a(i,k)*a(k,j)
          END DO
          a(i,j) = sum
        END DO
        aamax = 0.
        DO i = j, n
          sum = a(i,j)
          DO k = 1, j - 1
            sum = sum - a(i,k)*a(k,j)
          END DO
          a(i,j) = sum
          dum = vv(i)*abs(sum)
          IF (dum>=aamax) THEN
            imax = i
            aamax = dum
          END IF
        END DO
        IF (j/=imax) THEN
          DO k = 1, n
            dum = a(imax,k)
            a(imax,k) = a(j,k)
            a(j,k) = dum
          END DO
          d = -d
          vv(imax) = vv(j)
        END IF
        indx(j) = imax
        IF (a(j,j)==0.) a(j,j) = tiny
        IF (j/=n) THEN
          dum = 1./a(j,j)
          DO i = j + 1, n
            a(i,j) = a(i,j)*dum
          END DO
        END IF
      END DO

      RETURN
    END SUBROUTINE ludcmp

! //////////////////////////////////////////////////////////////////

    SUBROUTINE modpar(blksize,nspcsda,numcells,cblk,blkta,blkprs,pmassn, &
        pmassa,pmassc,pdensn,pdensa,pdensc,xlm,amu,dgnuc,dgacc,dgcor,knnuc, &
        knacc,kncor)
!***********************************************************************



!**    DESCRIPTION:
!       Calculates modal parameters and derived variables,
!       log-squared of std deviation, mode mean size, Knudsen number)
!       based on current values of moments for the modes.
! FSB   Now calculates the 3rd moment, mass, and density in all 3 modes.
!**
!**    Revision history:
!       Adapted 3/95 by US and CJC from EAM2's MODPAR and INIT3
!       Revised  7/23/96 by FSB to use COMMON blocks and small blocks
!        instead of large 3-d arrays, and to assume a fixed std.
!       Revised 12/06/96 by FSB to include coarse mode
!       Revised 1/10/97 by FSB to have arrays passed in call vector
!**********************************************************************

!     IMPLICIT NONE

!     Includes:


! *** input:

! dimension of arrays             
      INTEGER blksize
! actual number of cells in arrays
      INTEGER numcells

      INTEGER nspcsda

! nmber of species in CBLK        
      REAL cblk(blksize,nspcsda) ! main array of variables          
      REAL blkta(blksize) ! Air temperature [ K ]            
      REAL blkprs(blksize) 
! *** output:

! Air pressure in [ Pa ]           
! concentration lower limit [ ug/m*
! lowest particle diameter ( m )   
      REAL dgmin
      PARAMETER (dgmin=1.0E-09)

! lowest particle density ( Kg/m**3
      REAL densmin
      PARAMETER (densmin=1.0E03)

      REAL pmassn(blksize) ! mass concentration in nuclei mode 
      REAL pmassa(blksize) ! mass concentration in accumulation
      REAL pmassc(blksize) ! mass concentration in coarse mode 
      REAL pdensn(blksize) ! average particel density in Aitken
      REAL pdensa(blksize) ! average particel density in accumu
      REAL pdensc(blksize) ! average particel density in coarse
      REAL xlm(blksize) ! atmospheric mean free path [ m]   
      REAL amu(blksize) ! atmospheric dynamic viscosity [ kg
      REAL dgnuc(blksize) ! Aitken mode mean diameter [ m ]   
      REAL dgacc(blksize) ! accumulation                      
      REAL dgcor(blksize) ! coarse mode                       
      REAL knnuc(blksize) ! Aitken mode Knudsen number        
      REAL knacc(blksize) ! accumulation                      
      REAL kncor(blksize) 

! coarse mode                       

      INTEGER lcell
!      WRITE(20,*) ' IN MODPAR '

! *** set up  aerosol  3rd moment, mass, density

! loop counter                            
      DO lcell = 1, numcells

! *** Aitken-mode
!        cblk(lcell,vnu3) = max(conmin,(so4fac*cblk(lcell, & ! ghan
        cblk(lcell,vnu3) = so4fac*cblk(lcell, &
          vso4ai)+nh4fac*cblk(lcell,vnh4ai)+h2ofac*cblk(lcell, &
          vh2oai)+no3fac*cblk(lcell,vno3ai)+                   &
          nafac*cblk(lcell,vnaai)+  clfac*cblk(lcell,vclai)+   &
          orgfac*cblk(lcell, &
          vorgaro1i)+orgfac*cblk(lcell,vorgaro2i)+orgfac*cblk(lcell, &
          vorgalk1i)+orgfac*cblk(lcell,vorgole1i)+orgfac*cblk(lcell, &
          vorgba1i)+orgfac*cblk(lcell,vorgba2i)+orgfac*cblk(lcell, &
          vorgba3i)+orgfac*cblk(lcell,vorgba4i)+orgfac*cblk(lcell, &
          vorgpai)+anthfac*cblk(lcell,vp25ai)+anthfac*cblk(lcell,veci)
!          vorgpai)+anthfac*cblk(lcell,vp25ai)+anthfac*cblk(lcell,veci))) ! ghan



! *** Accumulation-mode

!        cblk(lcell,vac3) = max(conmin,(so4fac*cblk(lcell, & ! ghan
        cblk(lcell,vac3) = so4fac*cblk(lcell, &
          vso4aj)+nh4fac*cblk(lcell,vnh4aj)+h2ofac*cblk(lcell, &
          vh2oaj)+no3fac*cblk(lcell,vno3aj) +                  &
          nafac*cblk(lcell,vnaaj)+  clfac*cblk(lcell,vclaj)+   &
          orgfac*cblk(lcell, &
          vorgaro1j)+orgfac*cblk(lcell,vorgaro2j)+orgfac*cblk(lcell, &
          vorgalk1j)+orgfac*cblk(lcell,vorgole1j)+orgfac*cblk(lcell, &
          vorgba1j)+orgfac*cblk(lcell,vorgba2j)+orgfac*cblk(lcell, &
          vorgba3j)+orgfac*cblk(lcell,vorgba4j)+orgfac*cblk(lcell, &
          vorgpaj)+anthfac*cblk(lcell,vp25aj)+anthfac*cblk(lcell,vecj)
!          vorgpaj)+anthfac*cblk(lcell,vp25aj)+anthfac*cblk(lcell,vecj))) ! ghan

! *** coarse mode

!        cblk(lcell,vcor3) = max(conmin,(soilfac*cblk(lcell, & ! ghan rely on conmin applied to mass, not moment
!          vsoila)+seasfac*cblk(lcell,vseas)+anthfac*cblk(lcell,vantha)))
        cblk(lcell,vcor3) = soilfac*cblk(lcell, &
          vsoila)+seasfac*cblk(lcell,vseas)+anthfac*cblk(lcell,vantha)

! *** now get particle mass and density

! *** Aitken-mode:

! Na and Cl added to aitken mode mass conc

        pmassn(lcell) = max(conmin,(cblk(lcell,vso4ai)+cblk(lcell, &
          vnh4ai)+cblk(lcell,vh2oai)+cblk(lcell,vno3ai)+ &
          cblk(lcell,vnaai)+cblk(lcell,vclai)+cblk(lcell, &
          vorgaro1i)+cblk(lcell,vorgaro2i)+cblk(lcell,vorgalk1i)+cblk(lcell, &
          vorgole1i)+cblk(lcell,vorgba1i)+cblk(lcell,vorgba2i)+cblk(lcell, &
          vorgba3i)+cblk(lcell,vorgba4i)+cblk(lcell,vorgpai)+cblk(lcell, &
          vp25ai)+cblk(lcell,veci)))

!        pmassn(lcell) = max(conmin,(cblk(lcell,vso4ai)+cblk(lcell, &
!          vnh4ai)+cblk(lcell,vh2oai)+cblk(lcell,vno3ai)+cblk(lcell, &
!          vorgaro1i)+cblk(lcell,vorgaro2i)+cblk(lcell,vorgalk1i)+cblk(lcell, &
!          vorgole1i)+cblk(lcell,vorgba1i)+cblk(lcell,vorgba2i)+cblk(lcell, &
!          vorgba3i)+cblk(lcell,vorgba4i)+cblk(lcell,vorgpai)+cblk(lcell, &
!          vp25ai)+cblk(lcell,veci)))


! *** Accumulation-mode:

! ! Na and Cl added to accum mode mass conc

        pmassa(lcell) = max(conmin,(cblk(lcell,vso4aj)+cblk(lcell, &
          vnh4aj)+cblk(lcell,vh2oaj)+cblk(lcell,vno3aj)+ &
          cblk(lcell,vnaaj)+cblk(lcell,vclaj)+cblk(lcell, &
          vorgaro1j)+cblk(lcell,vorgaro2j)+cblk(lcell,vorgalk1j)+cblk(lcell, &
          vorgole1j)+cblk(lcell,vorgba1j)+cblk(lcell,vorgba2j)+cblk(lcell, &
          vorgba3j)+cblk(lcell,vorgba4j)+cblk(lcell,vorgpaj)+cblk(lcell, &
          vp25aj)+cblk(lcell,vecj)))

!        pmassa(lcell) = max(conmin,(cblk(lcell,vso4aj)+cblk(lcell, &
!          vnh4aj)+cblk(lcell,vh2oaj)+cblk(lcell,vno3aj)+cblk(lcell, &
!          vorgaro1j)+cblk(lcell,vorgaro2j)+cblk(lcell,vorgalk1j)+cblk(lcell, &
!          vorgole1j)+cblk(lcell,vorgba1j)+cblk(lcell,vorgba2j)+cblk(lcell, &
!          vorgba3j)+cblk(lcell,vorgba4j)+cblk(lcell,vorgpaj)+cblk(lcell, &
!          vp25aj)+cblk(lcell,vecj)))


! *** coarse mode:

        pmassc(lcell) = max(conmin,cblk(lcell,vsoila)+cblk(lcell,vseas)+cblk( &
          lcell,vantha))



      END DO
! *** now get particle density, mean free path, and dynamic viscosity

! aerosol  3rd moment and  mass                       
      DO lcell = 1, & 
          numcells
! *** density in [ kg m**-3 ]

! Density and mean free path              
        pdensn(lcell) = max(densmin,(f6dpim9*pmassn(lcell)/cblk(lcell,vnu3)))
        pdensa(lcell) = max(densmin,(f6dpim9*pmassa(lcell)/cblk(lcell,vac3)))
        pdensc(lcell) = max(densmin,(f6dpim9*pmassc(lcell)/cblk(lcell,vcor3)))

! *** Calculate mean free path [ m ]:

        xlm(lcell) = 6.6328E-8*pss0*blkta(lcell)/(tss0*blkprs(lcell))

! *** 6.6328E-8 is the sea level values given in Table I.2.8
! *** on page 10 of U.S. Standard Atmosphere 1962

! *** 	Calculate dynamic viscosity [ kg m**-1 s**-1 ]:

! *** U.S. Standard Atmosphere 1962 page 14 expression
!     for dynamic viscosity is:
!     dynamic viscosity =  beta * T * sqrt(T) / ( T + S)
!     where beta = 1.458e-6 [ kg sec^-1 K**-0.5 ], s = 110.4 [ K ].

        amu(lcell) = 1.458E-6*blkta(lcell)*sqrt(blkta(lcell))/ &
          (blkta(lcell)+110.4)


      END DO

!...............   Standard deviation fixed in both modes, so
!...............   diagnose diameter from 3rd moment and number concentr


!  density and mean free path 
      DO lcell = 1, & 
          numcells

! calculate diameters             
        dgnuc(lcell) = max(dgmin,(cblk(lcell,vnu3)/(cblk(lcell,vnu0)*esn36))** &
          one3)


        dgacc(lcell) = max(dgmin,(cblk(lcell,vac3)/(cblk(lcell,vac0)*esa36))** &
          one3)


        dgcor(lcell) = max(dgmin,(cblk(lcell,vcor3)/(cblk(lcell,vcorn)*esc36)) &
          **one3)

! when running with cloudborne aerosol, apply some very mild bounding
! to avoid unrealistic dg values
      if (cw_phase > 0) then
        dgnuc(lcell) = max( dgnuc(lcell), dginin*0.2  )  !  > 0.002 um
        dgnuc(lcell) = min( dgnuc(lcell), dginin*10.0 )  !  < 0.10  um
        dgacc(lcell) = max( dgacc(lcell), dginia*0.2  )  !  > 0.014 um
        dgacc(lcell) = min( dgacc(lcell), dginia*10.0 )  !  < 0.7 um
        dgcor(lcell) = max( dgcor(lcell), dginic*0.2  )  !  > 0.2 um
        dgcor(lcell) = min( dgcor(lcell), dginic*10.0 )  ! < 10.0 um
      end if

      END DO
! end loop on diameters                              
      DO lcell = 1, & 
          numcells
! Calculate Knudsen numbers           
        knnuc(lcell) = 2.0*xlm(lcell)/dgnuc(lcell)

        knacc(lcell) = 2.0*xlm(lcell)/dgacc(lcell)

        kncor(lcell) = 2.0*xlm(lcell)/dgcor(lcell)


      END DO

! end loop for  Knudsen numbers                       
      RETURN

!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
    END SUBROUTINE modpar
! modpar                                                  
    SUBROUTINE newt(layer,x,n,check,ctot,csat,imwcv,minitw,its)
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs                                                                    !
!bs  Description:                                                      !
!bs                                                                    !
!bs  Adopted from Numerical Recipes in FORTRAN, Chapter 9.7, 2nd ed.   !
!bs                                                                    !
!bs  Given an initial guess X(1:N) for a root in N dimensions, find    !
!bs  the root by globally convergent Newton's method. The vector of    !
!bs  functions to be zeroed, called FVEC(1:N) in the routine below. is !
!bs  retuned by a user-supplied function that must be called FUNCV and !
!bs  have the declaration SUBROUTINE FUNCV(NX,FVEC). The output        !
!bs  quantity CHECK is false on a normal return and true if the        !
!bs  routine has converged to a local minimum of the function FMIN     !
!bs  defined below. In this case try restarting from a different       !
!bs  initial guess.                                                    !
!bs                                                                    !
!bs  PARAMETERS                                                        !
!bs  NP     : maximum expected value of N                              !
!bs  MAXITS : maximum number of iterations                             !
!bs  TOLF   : convergence criterion on function values                 !
!bs  TOLMIN : criterion for decidingwhether spurios convergence to a   !
!bs           minimum of FMIN has ocurred                              !
!bs  TOLX   : convergence criterion on delta_X                         !
!bs  STPMX  : scaled maximum step length allowed in line searches      !
!bs                                                                    !
!bs  Called by:       SOA_PART                                         !
!bs                                                                    !
!bs  Calls:           FDJAC                                            !
!bs                   FMIN                                             !
!bs                   LNSRCH                                           !
!bs                   LUBKSB                                           !
!bs                   LUDCMP                                           !
!bs                                                                    !
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs
!     IMPLICIT NONE
!bs
!bs * includes
!bs
!bs
!bs * input variables
!bs
!bs model layer                           
      INTEGER layer
!bs dimension of problem                  
      INTEGER n
      REAL x(n) !bs initial guess of CAER                 
      LOGICAL check
      REAL ctot(n) !bs total concentration GAS + AER + PROD  
      REAL csat(n) !bs saturation conc. of cond. vapor [ug/m^
      REAL imwcv(n) !bs inverse molecular weights             
!bs
      REAL minitw
!bs * following Numerical recipes
!bs
!bs weighted initial mass                 
      INTEGER nn
!     INTEGER NP
!     PARAMETER (NP = 6)
      REAL fvec(np) !bs
!bs
!bs vector of functions to be zeroed
      REAL ct(np)
      REAL cs(np)
      REAL imw(np)
      REAL m
!bs
      INTEGER i, its, j, indx(np)
      REAL d, den, f, fold, stpmax, sum, temp, test
      REAL fjac(np,np)
      REAL g(np), p(np), xold(np)
!bs
!     EXTERNAL fmin
!bs
!bs * begin code
!bs
      m = minitw
      DO i = 1, n
        ct(i) = ctot(i)
        cs(i) = csat(i)
        imw(i) = imwcv(i)
      END DO
!bs
      nn = n
      f = fmin(x,fvec,nn,ct,cs,imw,m) !The vector FVEC is 
      test = & !Test for initial guess being a root. Us
        0.
      DO i = 1, & !stringent test than simply TOLF.       
          n
        IF (abs(fvec(i))>test) test = abs(fvec(i))
      END DO
      IF (test<0.01*tolf) RETURN
      sum = & !Calculate STPMAX for line searches     
        0.
      DO i = 1, n
        sum = sum + x(i)**2
      END DO
      stpmax = stpmx*max(sqrt(sum),float(n))
      DO its = 1, & !start of iteration loop                
          maxits
        CALL fdjac(n,x,fjac,ct,cs,imw) !get Jacobian              
        DO i = 1, & !compute Delta f for line search        
            n
          sum = 0.
          DO j = 1, n
            sum = sum + fjac(j,i)*fvec(j)
          END DO
          g(i) = sum
        END DO
        DO i = 1, & !store X                                
            n
          xold(i) = x(i)
        END DO
        fold = & !store F                                
          f
        DO i = 1, & !right-hand side for linear equations   
            n
          p(i) = -fvec(i)
        END DO
        CALL ludcmp(fjac,n,np,indx,d,layer) !solve linear equations by LU dec
        CALL lubksb(fjac,n,np,indx,p)
        CALL lnsrch(ctot,n,xold,fold,g, & !LNSRCH returns new X and F. It a
          p,x,f,stpmax, & !calculates FVEC at the new X whe
          check,fmin,fvec,ct,cs,imw,m) !calls FMIN                      
        test = 0.
        DO i = 1, n
          IF (abs(fvec(i))>test) test = abs(fvec(i))
        END DO
        IF (test<tolf) THEN
          check = .FALSE.
          RETURN
        END IF
        IF (check) & !Check for gradient of F zero,          
            THEN
          test = & !i.e., superious convergence.           
            0.
          den = max(f,0.5*n)
          DO i = 1, n
            temp = abs(g(i))*max(abs(x(i)),1.)/den
            IF (temp>test) test = temp
          END DO
          IF (test<tolmin) THEN
            check = .TRUE.
          ELSE
            check = .FALSE.
          END IF
          RETURN
        END IF
        test = & !Test for convergence on delta_x        
          0.
        DO i = 1, n
          temp = (abs(x(i)-xold(i)))/max(abs(x(i)),1.)
          IF (temp>test) test = temp
        END DO
        IF (test<tolx) RETURN
      END DO
!     WRITE (6,'(a,i2)') 'MAXITS exceeded in newt.F ! Layer: ', layer
!bs
    END SUBROUTINE newt
! //////////////////////////////////////////////////////////////////

    SUBROUTINE nuclcond(blksize,nspcsda,numcells,cblk,dt,layer,blkta,blkprs, &
        blkrh,so4rat,orgaro1rat,orgaro2rat,orgalk1rat,orgole1rat,orgbio1rat, &
        orgbio2rat,orgbio3rat,orgbio4rat,drog,ldrog,ncv,nacv,dgnuc,dgacc, &
        fconcn,fconca,fconcn_org,fconca_org,dmdt,dndt,deltaso4a,cgrn3,cgra3)
!***********************************************************************
!**    DESCRIPTION:  calculates aerosol nucleation and condensational
!**    growth rates using Binkowski and Shankar (1995) method.

! *** In this version, the method od RPM is followed where
!     the diffusivity, the average molecular ve3locity, and
!     the accomodation coefficient for sulfuric acid are used for
!     the organics. This is for consistency.
!       Future versions will use the correct values.  FSB 12/12/96


!**
!**    Revision history:
!       prototype 1/95 by Uma and Carlie
!       Corrected 7/95 by Uma for condensation of mass not nucleated
!       and mass conservation check
!       Revised   8/95 by US to calculate air density in stmt function
!                 and collect met variable stmt funcs in one include fil
!       Revised 7/25/96 by FSB to use block structure.
!       Revised 9/17/96 by FSB to use Y&K or K&W Nucleation mechanism
!       Revised 11/15/96 by FSB to use MKS,  and mom m^-3 units.
!       Revised 1/13/97 by FSB to pass arrays and simplify code.
!       Added   23/03/99 by BS growth factors for organics
!**********************************************************************

!     IMPLICIT NONE

!     Includes:


! *** arguments

! *** input;

! dimension of arrays             
      INTEGER blksize
      INTEGER layer
! number of species in CBLK       
      INTEGER nspcsda
! actual number of cells in arrays
      INTEGER numcells

      INTEGER ldrog
! # of organic aerosol precursor  
      REAL cblk(blksize,nspcsda) ! main array of variables         
! model time step in  SECONDS     
      REAL dt
      REAL blkta(blksize) ! Air temperature [ K ]           
      REAL blkprs(blksize) ! Air pressure in [ Pa ]          
      REAL blkrh(blksize) ! Fractional relative humidity    
      REAL so4rat(blksize)                                       ! rate [  ug/m**3 /s ]
!bs
! sulfate gas-phase production    
! total # of cond. vapors & SOA spe
      INTEGER ncv
!bs
      INTEGER nacv
!bs * anthropogenic organic condensable vapor production rate
! # of anthrop. cond. vapors & SOA 
      REAL drog(blksize,ldrog) !bs
! Delta ROG conc. [ppm]             
      REAL orgaro1rat(blksize)                                 ! production rate from aromatics [ug/m**
! anthropogenic organic aerosol mass    
      REAL orgaro2rat(blksize)                                 ! production rate from aromatics [ug/m**
! anthropogenic organic aerosol mass    
      REAL orgalk1rat(blksize)                                 ! production rate from alkanes & others
! anthropogenic organic aerosol mass    
      REAL orgole1rat(blksize)                                 ! production rate from alkenes & others
!bs * biogenic organic condensable vapor production rate
! anthropogenic organic aerosol mass    
      REAL orgbio1rat(blksize)                                 ! rate [  ug/m**3 /s ]
! biogenic organic aerosol production   
      REAL orgbio2rat(blksize)                                 ! rate [  ug/m**3 /s ]
! biogenic organic aerosol production   
      REAL orgbio3rat(blksize)                                 ! rate [  ug/m**3 /s ]
! biogenic organic aerosol production   
      REAL orgbio4rat(blksize)                                 ! rate [  ug/m**3 /s ]
!bs
! biogenic organic aerosol production   
      REAL dgnuc(blksize) ! accumulation                          
      REAL dgacc(blksize) 
! *** output:

! coarse mode                           
      REAL fconcn(blksize)                                 ! Aitken mode  [ 1 / s ]
! reciprocal condensation rate          
      REAL fconca(blksize)                                 ! acclumulation mode [ 1 / s ]
! reciprocal condensation rate          
      REAL fconcn_org(blksize)                                 ! Aitken mode  [ 1 / s ]
! reciprocal condensation rate          
      REAL fconca_org(blksize)                                 ! acclumulation mode [ 1 / s ]
! reciprocal condensation rate          
      REAL dmdt(blksize)                                 ! by particle formation [ ug/m**3 /s ]
! rate of production of new mass concent
      REAL dndt(blksize)                                 ! concentration by particle formation [#
! rate of producton of new particle numb
      REAL deltaso4a(blksize)                                 ! sulfate aerosol by condensation [ ug/m
! increment of concentration added to   
      REAL cgrn3(blksize)                                 ! Aitken mode [ 3rd mom/m **3 s ]
! growth rate for 3rd moment for        
      REAL cgra3(blksize)                                 ! Accumulation mode   

!...........    SCRATCH local variables and their descriptions:

! growth rate for 3rd moment for        

      INTEGER lcell

! LOOP INDEX                                     
! conv rate so2 --> so4 [mom-3/g/s]     
      REAL chemrat
! conv rate for organics [mom-3/g/s]    
      REAL chemrat_org
      REAL am1n, & ! 1st mom density (nuc, acc modes) [mom_
        am1a
      REAL am2n, & ! 2nd mom density (nuc, acc modes) [mom_
        am2a
      REAL gnc3n, & ! near-cont fns (nuc, acc) for mom-3 den
        gnc3a
      REAL gfm3n, & ! free-mol  fns (nuc, acc) for mom-3 den
        gfm3a
! total reciprocal condensation rate    
      REAL fconc

      REAL td
! d * tinf (cgs)                        
      REAL*8 & ! Cnstant to force 64 bit evaluation of 
        one88
      PARAMETER (one88=1.0D0)
!  *** variables to set up sulfate and organic condensation rates

! sulfuric acid vapor at current time step            
      REAL vapor1
!                                    chemistry and emissions
      REAL vapor2
! Sulfuric acid vapor prior to addition from          
!bs
      REAL deltavap
!bs * start update
!bs
! change to vapor at previous time step 
      REAL diffcorr

!bs *
      REAL csqt_org
!bs * end update
!bs

      REAL csqt
!.......................................................................
!   begin body of subroutine  NUCLCOND


!...........   Main computational grid-traversal loop nest
!...........   for computing condensation and nucleation:

      DO lcell = 1, & 
          numcells
! *** First moment:

!  1st loop over NUMCELLS                  
        am1n = cblk(lcell,vnu0)*dgnuc(lcell)*esn04
        am1a = cblk(lcell,vac0)*dgacc(lcell)*esa04

!..............   near-continuum factors [ 1 / sec ]
!bs
!bs * adopted from code of FSB
!bs * correction to DIFFSULF and DIFFORG for temperature and pressure
!bs
        diffcorr = (pss0/blkprs(lcell))*(blkta(lcell)/273.16)**1.
!bs
        gnc3n = cconc*am1n*diffcorr
        gnc3a = cconc*am1a*diffcorr


! *** Second moment:

        am2n = cblk(lcell,vnu0)*dgnuc(lcell)*dgnuc(lcell)*esn16
        am2a = cblk(lcell,vac0)*dgacc(lcell)*dgacc(lcell)*esa16

        csqt = ccofm*sqrt(blkta(lcell)) 
!...............   free molecular factors [ 1 / sec ]

! put in temperature fac
        gfm3n = csqt*am2n
        gfm3a = csqt*am2a

! *** Condensation factors in [ s**-1] for h2so4
! *** In the future, separate factors for condensing organics will
!      be included. In this version, the h2so4 values are used.

!...............   Twice the harmonic mean of fm, nc functions:

! *** Force 64 bit evaluation:

        fconcn(lcell) = one88*gnc3n*gfm3n/(gnc3n+gfm3n)
        fconca(lcell) = one88*gnc3a*gfm3a/(gnc3a+gfm3a)
        fconc = fconcn(lcell) + fconca(lcell)

! *** NOTE: FCONCN and FCONCA will be redefined below <<<<<<
!bs
!bs * start modifications for organcis
!bs
        gnc3n = cconc_org*am1n*diffcorr
        gnc3a = cconc_org*am1a*diffcorr
!bs
        csqt_org = ccofm_org*sqrt(blkta(lcell))
        gfm3n = csqt_org*am2n
        gfm3a = csqt_org*am2a
!bs
        fconcn_org(lcell) = one88*gnc3n*gfm3n/(gnc3n+gfm3n)
        fconca_org(lcell) = one88*gnc3a*gfm3a/(gnc3a+gfm3a)
!bs
!bs * end modifications for organics
!bs
! *** calculate the total change to sulfuric acid vapor from production
!                      and condensation

        vapor1 = cblk(lcell,vsulf) ! curent sulfuric acid vapor        
        vapor2 = cblk(lcell,vsulf) - so4rat(lcell)* & 
          dt
! vapor at prev
        vapor2 = max(0.0,vapor2)

        deltavap = max(0.0,(so4rat(lcell)/fconc-vapor2)*(1.0-exp(-fconc*dt)))

! *** Calculate increment in total sufate aerosol mass concentration

! *** This follows the method of Youngblood & Kreidenweis.

!bs
!bs        DELTASO4A( LCELL) = MAX( 0.0, SO4RAT(LCELL) * DT - DELTAVAP)
!bs
!bs * allow DELTASO4A to be negative, but the change must not be larger
!bs * than the amount of vapor available.
!bs
        deltaso4a(lcell) = max(-1.*cblk(lcell,vsulf), &
          so4rat(lcell)*dt-deltavap)
!bs

! *** zero out growth coefficients

        cgrn3(lcell) = 0.0
        cgra3(lcell) = 0.0


      END DO

! *** Select method of nucleation

! End 1st loop over NUMCELLS            
      IF (inucl==1) THEN

! *** Do Youngblood & Kreidenweis  Nucleation

!         CALL BCSUINTF(DT,SO4RAT,FCONCN,FCONCA,BLKTA,BLKRH,
!     &        DNDT,DMDT,NUMCELLS,BLKSIZE,
!     &        VAPOR1)
!       IF (firstime) THEN
!         WRITE (6,*)
!         WRITE (6,'(a,i2)') 'INUCL =', inucl
!         WRITE (90,'(a,i2)') 'INUCL =', inucl
!         firstime = .FALSE.
!       END IF

      ELSE IF (inucl==0) THEN

! *** Do Kerminen & Wexler Nucleation

!         CALL nuclKW(DT,SO4RAT,FCONCN,FCONCA,BLKTA,BLKRH,
!     &        DNDT,DMDT,NUMCELLS,BLKSIZE)
!       IF (firstime) THEN
!         WRITE (6,*)
!         WRITE (6,'(a,i2)') 'INUCL =', inucl
!         WRITE (90,'(a,i2)') 'INUCL =', inucl
!         firstime = .FALSE.
!       END IF


      ELSE IF (inucl==2) THEN

!bs ** Do Kulmala et al. Nucleation
!       if(dndt(1).lt.-10.)print *,'before klpnuc',blkta(1),blkrh(1),vapor1,dndt(1),dmdt(1),so4rat(1)

        if(blkta(1).ge.233.15.and.blkrh(1).ge.0.1)then
           CALL klpnuc(blkta(1),blkrh(1),vapor1,dndt(1),dmdt(1),so4rat(1))
        else
           dndt(1)=0.
           dmdt(1)=0.
        endif


!       CALL klpnuc(blkta(1),blkrh(1),vapor1,dndt(1),dmdt(1),so4rat(1))
!       if(dndt(1).lt.-10.)print *,'after klpnuc',dndt(1),dmdt(1)
        IF (dndt(1)==0.) dmdt(1) = 0.
        IF (dmdt(1)==0.) dndt(1) = 0.
!       IF (firstime) THEN
!         WRITE (6,*)
!         WRITE (6,'(a,i2)') 'INUCL =', inucl
!         WRITE (90,'(a,i2)') 'INUCL =', inucl
!         firstime = .FALSE.
!       END IF
!     ELSE
!       WRITE (6,'(a)') '*************************************'
!       WRITE (6,'(a,i2,a)') '  INUCL =', inucl, ',  PLEASE CHECK !!'
!       WRITE (6,'(a)') '        PROGRAM TERMINATED !!'
!       WRITE (6,'(a)') '*************************************'
!       STOP

      END IF
!bs
!bs * Secondary organic aerosol module (SORGAM)
!bs
! end of selection of nucleation method 
      CALL sorgam(layer,blkta,blkprs,orgaro1rat,orgaro2rat,orgalk1rat, &
        orgole1rat,orgbio1rat,orgbio2rat,orgbio3rat,orgbio4rat,drog,ldrog,ncv, &
        nacv,cblk,blksize,nspcsda,numcells,dt)
!bs
!bs *  Secondary organic aerosol module (SORGAM)
!bs

      DO lcell = 1, numcells

! *** redefine FCONCN & FCONCA to be the nondimensional fractionaL
!     condensation factors

        td = 1.0/(fconcn(lcell)+fconca(lcell))
        fconcn(lcell) = td*fconcn(lcell)
        fconca(lcell) = td*fconca(lcell)
!bs
        td = 1.0/(fconcn_org(lcell)+fconca_org(lcell))
        fconcn_org(lcell) = td*fconcn_org(lcell)
        fconca_org(lcell) = td*fconca_org(lcell)
!bs
      END DO

! ***  Begin second loop over cells

      DO lcell = 1, & 
          numcells
! *** note CHEMRAT includes  species other than sulfate.

! 3rd loop on NUMCELLS                     
        chemrat = so4fac*so4rat(lcell) ! [mom3 m**-3 s-
        chemrat_org = orgfac*(orgaro1rat(lcell)+orgaro2rat(lcell)+orgalk1rat( &
          lcell)+orgole1rat(lcell)+orgbio1rat(lcell)+orgbio2rat(lcell)+ &
          orgbio3rat(lcell)+orgbio4rat(lcell)) 
! *** Calculate the production rates for new particle

! [mom3 m**-3 s-
        cgrn3(lcell) = so4fac*dmdt(lcell) 
! Rate of increase of 3rd
        chemrat = chemrat - cgrn3(lcell)                                            !bs 3rd moment production fro

!bs Remove the rate of new pa
        chemrat = max(chemrat,0.0) 
! *** Now calculate the rate of condensation on existing particles.

! Prevent CHEMRAT from being negativ
        cgrn3(lcell) = cgrn3(lcell) + chemrat*fconcn(lcell) + &
          chemrat_org*fconcn_org(lcell)

        cgra3(lcell) = chemrat*fconca(lcell) + chemrat_org*fconca_org(lcell)

! ***

      END DO
!  end 2nd loop over NUMCELLS           
      RETURN

    END SUBROUTINE nuclcond
!23456789012345678901234567890123456789012345678901234567890123456789012

! nuclcond                              
    REAL FUNCTION poly4(a,x)
      REAL a(4), x

      poly4 = a(1) + x*(a(2)+x*(a(3)+x*(a(4))))
      RETURN
    END FUNCTION poly4
    REAL FUNCTION poly6(a,x)
      REAL a(6), x

      poly6 = a(1) + x*(a(2)+x*(a(3)+x*(a(4)+x*(a(5)+x*(a(6))))))
      RETURN
    END FUNCTION poly6


!-----------------------------------------------------------------------



    SUBROUTINE rpmares_old(so4,hno3,no3,nh3,nh4,rh,temp,aso4,ano3,ah2o,anh4, &
        gnh3,gno3)

!-----------------------------------------------------------------------

! Description:

!   ARES calculates the chemical composition of a sulfate/nitrate/
!   ammonium/water aerosol based on equilibrium thermodynamics.

!   This code considers two regimes depending upon the molar ratio
!   of ammonium to sulfate.

!   For values of this ratio less than 2,the code solves a cubic for
!   hydrogen ion molality, HPLUS,  and if enough ammonium and liquid
!   water are present calculates the dissolved nitric acid. For molal
!   ionic strengths greater than 50, nitrate is assumed not to be presen

!   For values of the molar ratio of 2 or greater, all sulfate is assume
!   to be ammonium sulfate and a calculation is made for the presence of
!   ammonium nitrate.

!   The Pitzer multicomponent approach is used in subroutine ACTCOF to
!   obtain the activity coefficients. Abandoned -7/30/97 FSB

!   The Bromley method of calculating the activity coefficients is s use
!    in this version

!   The calculation of liquid water
!   is done in subroutine water. Details for both calculations are given
!   in the respective subroutines.

!   Based upon MARS due to
!   P. Saxena, A.B. Hudischewskyj, C. Seigneur, and J.H. Seinfeld,
!   Atmos. Environ., vol. 20, Number 7, Pages 1471-1483, 1986.

!   and SCAPE due to
!   Kim, Seinfeld, and Saxeena, Aerosol Ceience and Technology,
!   Vol 19, number 2, pages 157-181 and pages 182-198, 1993.

! NOTE: All concentrations supplied to this subroutine are TOTAL
!       over gas and aerosol phases

! Parameters:

!  SO4   : Total sulfate in MICROGRAMS/M**3 as sulfate (IN)
!  HNO3  : Nitric Acid in MICROGRAMS/M**3 as nitric acid (IN)
!  NO3   : Total nitrate in MICROGRAMS/M**3 as nitric acid (IN)
!  NH3   : Total ammonia in MICROGRAMS/M**3 as ammonia (IN)
!  NH4   : Ammonium in MICROGRAMS/M**3 as ammonium (IN)
!  RH    : Fractional relative humidity (IN)
!  TEMP  : Temperature in Kelvin (IN)
!  GNO3  : Gas phase nitric acid in MICROGRAMS/M**3 (OUT)
!  GNH3  : Gas phase ammonia in MICROGRAMS/M**3 (OUT)
!  ASO4  : Aerosol phase sulfate in MICROGRAMS/M**3 (OUT)
!  ANO3  : Aerosol phase nitrate in MICROGRAMS/M**3 (OUT)
!  ANH4  : Aerosol phase ammonium in MICROGRAMS/M**3 (OUT)
!  AH2O  : Aerosol phase water in MICROGRAMS/M**3 (OUT)
!  NITR  : Number of iterations for obtaining activity coefficients  (OU
!  NR    : Number of real roots to the cubic in the low ammonia case (OU

! Revision History:
!      Who       When        Detailed description of changes
!   ---------   --------  -------------------------------------------
!   S.Roselle   11/10/87  Received the first version of the MARS code
!   S.Roselle   12/30/87  Restructured code
!   S.Roselle   2/12/88   Made correction to compute liquid-phase
!                         concentration of H2O2.
!   S.Roselle   5/26/88   Made correction as advised by SAI, for
!                         computing H+ concentration.
!   S.Roselle   3/1/89    Modified to operate with EM2
!   S.Roselle   5/19/89   Changed the maximum ionic strength from
!                         100 to 20, for numerical stability.
!   F.Binkowski 3/3/91    Incorporate new method for ammonia rich case
!                         using equations for nitrate budget.
!   F.Binkowski 6/18/91   New ammonia poor case which
!                         omits letovicite.
!   F.Binkowski 7/25/91   Rearranged entire code, restructured
!                         ammonia poor case.
!   F.Binkowski 9/9/91    Reconciled all cases of ASO4 to be output
!                         as SO4--
!   F.Binkowski 12/6/91   Changed the ammonia defficient case so that
!                         there is only neutralized sulfate (ammonium
!                         sulfate) and sulfuric acid.
!   F.Binkowski 3/5/92    Set RH bound on AWAS to 37 % to be in agreemen
!                          with the Cohen et al. (1987)  maximum molalit
!                          of 36.2 in Table III.( J. Phys Chem (91) page
!                          4569, and Table IV p 4587.)
!   F.Binkowski 3/9/92    Redid logic for ammonia defficient case to rem
!                         possibility for denomenator becoming zero;
!                         this involved solving for HPLUS first.
!                         Note that for a relative humidity
!                          less than 50%, the model assumes that there i
!                          aerosol nitrate.
!   F.Binkowski 4/17/95   Code renamed  ARES (AeRosol Equilibrium System
!                          Redid logic as follows
!                         1. Water algorithm now follows Spann & Richard
!                         2. Pitzer Multicomponent method used
!                         3. Multicomponent practical osmotic coefficien
!                            use to close iterations.
!                         4. The model now assumes that for a water
!                            mass fraction WFRAC less than 50% there is
!                            no aerosol nitrate.
!   F.Binkowski 7/20/95   Changed how nitrate is calculated in ammonia p
!                         case, and changed the WFRAC criterion to 40%.
!                         For ammonium to sulfate ratio less than 1.0
!                         all ammonium is aerosol and no nitrate aerosol
!                         exists.
!   F.Binkowski 7/21/95   Changed ammonia-ammonium in ammonia poor case
!                         allow gas-phase ammonia to exist.
!   F.Binkowski 7/26/95   Changed equilibrium constants to values from
!                         Kim et al. (1993)
!   F.Binkowski 6/27/96   Changed to new water format
!   F.Binkowski 7/30/97   Changed to Bromley method for multicomponent
!                         activity coefficients. The binary activity coe
!                         are the same as the previous version
!   F.Binkowski 8/1/97    Chenged minimum sulfate from 0.0 to 1.0e-6 i.e
!                         1 picogram per cubic meter

!-----------------------------------------------------------------------

!     IMPLICIT NONE

!...........INCLUDES and their descriptions

!cc      INCLUDE SUBST_CONST          ! constants

!...........PARAMETERS and their descriptions:

! molecular weight for NaCl          
      REAL mwnacl
      PARAMETER (mwnacl=58.44277)

! molecular weight for NO3           
      REAL mwno3
      PARAMETER (mwno3=62.0049)

! molecular weight for HNO3          
      REAL mwhno3
      PARAMETER (mwhno3=63.01287)

! molecular weight for SO4           
      REAL mwso4
      PARAMETER (mwso4=96.0576)

! molecular weight for HSO4          
      REAL mwhso4
      PARAMETER (mwhso4=mwso4+1.0080)

! molecular weight for H2SO4         
      REAL mh2so4
      PARAMETER (mh2so4=98.07354)

! molecular weight for NH3           
      REAL mwnh3
      PARAMETER (mwnh3=17.03061)

! molecular weight for NH4           
      REAL mwnh4
      PARAMETER (mwnh4=18.03858)

! molecular weight for Organic Specie
      REAL mworg
!
      PARAMETER (mworg=175.0)
!      PARAMETER (mworg=16.0)

! molecular weight for Chloride      
      REAL mwcl
      PARAMETER (mwcl=35.453)

! molecular weight for AIR           
      REAL mwair
      PARAMETER (mwair=28.964)

! molecular weight for Letovicite    
      REAL mwlct
      PARAMETER (mwlct=3.0*mwnh4+2.0*mwso4+1.0080)

! molecular weight for Ammonium Sulfa
      REAL mwas
      PARAMETER (mwas=2.0*mwnh4+mwso4)

! molecular weight for Ammonium Bisul
      REAL mwabs
      PARAMETER (mwabs=mwnh4+mwso4+1.0080)

!...........ARGUMENTS and their descriptions

!iamodels3
      REAL so4
! Total sulfate in micrograms / m**3 
! Total nitric acid in micrograms / m
      REAL hno3
! Total nitrate in micrograms / m**3 
      REAL no3
! Total ammonia in micrograms / m**3 
      REAL nh3
! Total ammonium in micrograms / m**3
      REAL nh4
! Fractional relative humidity       
      REAL rh
! Temperature in Kelvin              
      REAL temp
! Aerosol sulfate in micrograms / m**
      REAL aso4
! Aerosol nitrate in micrograms / m**
      REAL ano3
! Aerosol liquid water content water 
      REAL ah2o
! Aerosol ammonium in micrograms / m*
      REAL anh4
! Gas-phase nitric acid in micrograms
      REAL gno3

      REAL gnh3
!...........SCRATCH LOCAL VARIABLES and their descriptions:

! Gas-phase ammonia in micrograms / m
! Index set to percent relative humid
      INTEGER irh
! Number of iterations for activity c
      INTEGER nitr
! Loop index for iterations          
      INTEGER nnn

      INTEGER nr
! Number of roots to cubic equation f
      REAL*8 & ! Coefficients and roots of        
        a0
      REAL*8 & ! Coefficients and roots of        
        a1
      REAL*8 & ! Coefficients and roots of        
        a2
! Coefficients and discriminant for q
      REAL aa
! internal variables ( high ammonia c
      REAL bal
! Coefficients and discriminant for q
      REAL bb
! Variables used for ammonia solubili
      REAL bhat
! Coefficients and discriminant for q
      REAL cc
! Factor for conversion of units     
      REAL convt
! Coefficients and discriminant for q
      REAL dd
! Coefficients and discriminant for q
      REAL disc
! Relative error used for convergence
      REAL eror
!  Free ammonia concentration , that 
      REAL fnh3
! Activity Coefficient for (NH4+, HSO
      REAL gamaab
! Activity coefficient for (NH4+, NO3
      REAL gamaan
! Variables used for ammonia solubili
      REAL gamahat
! Activity coefficient for (H+ ,NO3-)
      REAL gamana
! Activity coefficient for (2H+, SO4-
      REAL gamas1
! Activity coefficient for (H+, HSO4-
      REAL gamas2
! used for convergence of iteration  
      REAL gamold
! internal variables ( high ammonia c
      REAL gasqd
! Hydrogen ion (low ammonia case) (mo
      REAL hplus
! Equilibrium constant for ammoniua t
      REAL k1a
! Equilibrium constant for sulfate-bi
      REAL k2sa
! Dissociation constant for ammonium 
      REAL k3
! Equilibrium constant for ammonium n
      REAL kan
! Variables used for ammonia solubili
      REAL khat
! Equilibrium constant for nitric aci
      REAL kna
! Henry's Law Constant for ammonia   
      REAL kph
! Equilibrium constant for water diss
      REAL kw
! Internal variable using KAN        
      REAL kw2
! Nitrate (high ammonia case) (moles 
      REAL man
! Sulfate (high ammonia case) (moles 
      REAL mas
! Bisulfate (low ammonia case) (moles
      REAL mhso4
! Nitrate (low ammonia case) (moles /
      REAL mna
! Ammonium (moles / kg water)        
      REAL mnh4
! Total number of moles of all ions  
      REAL molnu
! Sulfate (low ammonia case) (moles /
      REAL mso4
! Practical osmotic coefficient      
      REAL phibar
! Previous value of practical osmotic
      REAL phiold
! Molar ratio of ammonium to sulfate 
      REAL ratio
! Internal variable using K2SA       
      REAL rk2sa
! Internal variables using KNA       
      REAL rkna
! Internal variables using KNA       
      REAL rknwet
      REAL rr1
      REAL rr2
! Ionic strength                     
      REAL stion
! Internal variables for temperature 
      REAL t1
! Internal variables for temperature 
      REAL t2
! Internal variables of convenience (
      REAL t21
! Internal variables of convenience (
      REAL t221
! Internal variables for temperature 
      REAL t3
! Internal variables for temperature 
      REAL t4
! Internal variables for temperature 
      REAL t6
! Total ammonia and ammonium in micro
      REAL tnh4
! Total nitrate in micromoles / meter
      REAL tno3
! Tolerances for convergence test    
      REAL toler1
! Tolerances for convergence test    
      REAL toler2
! Total sulfate in micromoles / meter
      REAL tso4
! 2.0 * TSO4  (high ammonia case) (mo
      REAL twoso4
! Water mass fraction                
      REAL wfrac
                                   ! micrograms / meter **3 on output
      REAL wh2o
                                   ! internally it is 10 ** (-6) kg (wat
                                   ! the conversion factor (1000 g = 1 k
                                   ! for AH2O output
! Aerosol liquid water content (inter
! internal variables ( high ammonia c
      REAL wsqd
! Nitrate aerosol concentration in mi
      REAL xno3
! Variable used in quadratic solution
      REAL xxq
! Ammonium aerosol concentration in m
      REAL ynh4
! Water variable saved in case ionic 
      REAL zh2o

      REAL zso4
! Total sulfate molality - mso4 + mhs
      REAL cat(2) ! Array for cations (1, H+); (2, NH4+
      REAL an(3) ! Array for anions (1, SO4--); (2, NO
      REAL crutes(3) ! Coefficients and roots of          
      REAL gams(2,3) ! Array of activity coefficients     
! Minimum value of sulfate laerosol c
      REAL minso4
      PARAMETER (minso4=1.0E-6/mwso4)
      REAL floor
      PARAMETER (floor=1.0E-30) 
!-----------------------------------------------------------------------
!  begin body of subroutine RPMARES

!...convert into micromoles/m**3
!cc      WRITE( 10, * ) 'SO4, NO3, NH3 ', SO4, NO3, NH3
!iamodels3 merge NH3/NH4 , HNO3,NO3 here
! minimum concentration              
      tso4 = max(0.0,so4/mwso4)
      tno3 = max(0.0,(no3/mwno3+hno3/mwhno3))
      tnh4 = max(0.0,(nh3/mwnh3+nh4/mwnh4))
!cc      WRITE( 10, * ) 'TSO4, TNO3, TNH4, RH ', TSO4, TNO3, TNH4, RH

!...now set humidity index IRH as a percent

      irh = nint(100.0*rh)

!...Check for valid IRH

      irh = max(1,irh)
      irh = min(99,irh)
!cc      WRITE(10,*)'RH,IRH ',RH,IRH

!...Specify the equilibrium constants at  correct
!...  temperature.  Also change units from ATM to MICROMOLE/M**3 (for KA
!...  KPH, and K3 )
!...  Values from Kim et al. (1993) except as noted.

      convt = 1.0/(0.082*temp)
      t6 = 0.082E-9*temp
      t1 = 298.0/temp
      t2 = alog(t1)
      t3 = t1 - 1.0
      t4 = 1.0 + t2 - t1
      kna = 2.511E+06*exp(29.17*t3+16.83*t4)*t6
      k1a = 1.805E-05*exp(-1.50*t3+26.92*t4)
      k2sa = 1.015E-02*exp(8.85*t3+25.14*t4)
      kw = 1.010E-14*exp(-22.52*t3+26.92*t4)
      kph = 57.639*exp(13.79*t3-5.39*t4)*t6
!cc      K3   =  5.746E-17 * EXP( -74.38 * T3 + 6.12  * T4 ) * T6 * T6
      khat = kph*k1a/kw
      kan = kna*khat

!...Compute temperature dependent equilibrium constant for NH4NO3
!...  ( from Mozurkewich, 1993)

      k3 = exp(118.87-24084.0/temp-6.025*alog(temp))

!...Convert to (micromoles/m**3) **2

      k3 = k3*convt*convt

      wh2o = 0.0
      stion = 0.0
      ah2o = 0.0
      mas = 0.0
      man = 0.0
      hplus = 0.0
      toler1 = 0.00001
      toler2 = 0.001
      nitr = 0
      nr = 0
      ratio = 0.0
      gamaan = 1.0
      gamold = 1.0

!...set the ratio according to the amount of sulfate and nitrate
      IF (tso4>minso4) THEN
        ratio = tnh4/tso4

!...If there is no sulfate and no nitrate, there can be no ammonium
!...  under the current paradigm. Organics are ignored in this version.

      ELSE

        IF (tno3==0.0) THEN

! *** If there is very little sulfate and no nitrate set concentrations
!      to a very small value and return.
          aso4 = max(floor,aso4)
          ano3 = max(floor,ano3)
          wh2o = 0.0
          ah2o = 0.0
          gnh3 = max(floor,gnh3)
          gno3 = max(floor,gno3)
          RETURN
        END IF

!...For the case of no sulfate and nonzero nitrate, set ratio to 5
!...  to send the code to the high ammonia case

        ratio = 5.0
      END IF

!....................................
!......... High Ammonia Case ........
!....................................

      IF (ratio>2.0) THEN

        gamaan = 0.1

!...Set up twice the sulfate for future use.

        twoso4 = 2.0*tso4
        xno3 = 0.0
        ynh4 = twoso4

!...Treat different regimes of relative humidity

!...ZSR relationship is used to set water levels. Units are
!...  10**(-6) kg water/ (cubic meter of air)
!...  start with ammomium sulfate solution without nitrate

        CALL awater(irh,tso4,ynh4,tno3,ah2o) !**** note TNO3              
        wh2o = 1.0E-3*ah2o
        aso4 = tso4*mwso4
        ano3 = 0.0
        anh4 = ynh4*mwnh4
        wfrac = ah2o/(aso4+anh4+ah2o)
!cc        IF ( WFRAC .EQ. 0.0 )  RETURN   ! No water
        IF (wfrac<0.2) THEN

!... dry  ammonium sulfate and ammonium nitrate
!...  compute free ammonia

          fnh3 = tnh4 - twoso4
          cc = tno3*fnh3 - k3

!...check for not enough to support aerosol

          IF (cc<=0.0) THEN
            xno3 = 0.0
          ELSE
            aa = 1.0
            bb = -(tno3+fnh3)
            disc = bb*bb - 4.0*cc

!...Check for no real positive roots of the quadratic
!...  set nitrate to zero and RETURN if no real positive roots are found

            IF (disc<0.0) THEN
              xno3 = 0.0
              ah2o = 1000.0*wh2o
              ynh4 = twoso4
              gno3 = tno3*mwhno3
              gnh3 = (tnh4-ynh4)*mwnh3
              aso4 = tso4*mwso4
              ano3 = 0.0
              anh4 = ynh4*mwnh4
              RETURN
            END IF

!...to get here, BB .lt. 0.0, CC .gt. 0.0 always

            dd = sqrt(disc)
            xxq = -0.5*(bb+sign(1.0,bb)*dd)

!...Since both roots are positive, select smaller root.

            xno3 = min(xxq/aa,cc/xxq)

          END IF
          ah2o = 1000.0*wh2o
          ynh4 = 2.0*tso4 + xno3
          gno3 = (tno3-xno3)*mwhno3
          gnh3 = (tnh4-ynh4)*mwnh3
          aso4 = tso4*mwso4
          ano3 = xno3*mwno3
          anh4 = ynh4*mwnh4
          RETURN

        END IF

!...liquid phase containing completely neutralized sulfate and
!...  some nitrate.  Solve for composition and quantity.

        mas = tso4/wh2o
        man = 0.0
        xno3 = 0.0
        ynh4 = twoso4
        phiold = 1.0

!...Start loop for iteration

!...The assumption here is that all sulfate is ammonium sulfate,
!...  and is supersaturated at lower relative humidities.

        DO nnn = 1, 150
          nitr = nnn
          gasqd = gamaan*gamaan
          wsqd = wh2o*wh2o
          kw2 = kan*wsqd/gasqd
          aa = 1.0 - kw2
          bb = twoso4 + kw2*(tno3+tnh4-twoso4)
          cc = -kw2*tno3*(tnh4-twoso4)

!...This is a quadratic for XNO3 [MICROMOLES / M**3] of nitrate in solut

          disc = bb*bb - 4.0*aa*cc

!...Check for complex roots, if so set nitrate to zero and RETURN

          IF((disc<0.0) .OR. &
             (bb>0.0 .AND. aa>0.0 .AND.cc >0.0) .OR. &
             (bb<0.0 .AND. aa<0.0 .AND.cc <0.0)) THEN
            xno3 = 0.0
            ah2o = 1000.0*wh2o
            ynh4 = twoso4
            gno3 = tno3*mwhno3
            gnh3 = (tnh4-ynh4)*mwnh3
            aso4 = tso4*mwso4
            ano3 = 0.0
            anh4 = ynh4*mwnh4
!cc            WRITE( 10, * ) ' COMPLEX ROOTS '
            RETURN
          END IF

          dd = sqrt(disc)
          xxq = -0.5*(bb+sign(1.0,bb)*dd)
!PMA
          aa=max(aa,1.e-20)
          xxq=max(xxq,1.e-20)

          rr1 = xxq/aa
          rr2 = cc/xxq

          IF (rr1 <= 0.0 .AND. rr2 <= 0.0) THEN
            xno3 = 0.0
            ah2o = 1000.0*wh2o
            ynh4 = twoso4
            gno3 = tno3*mwhno3
            gnh3 = (tnh4-ynh4)*mwnh3
            aso4 = tso4*mwso4
            ano3 = 0.0
            anh4 = ynh4*mwnh4
!            WRITE(*,*) 'TWO NON-POSITIVE ROOTS!!! '
            RETURN
          END IF

!...choose minimum positve root

          IF ((rr1*rr2)<0.0) THEN
            xno3 = max(rr1,rr2)
          ELSE
            xno3 = min(rr1,rr2)
          END IF

          xno3 = min(xno3,tno3)

!...This version assumes no solid sulfate forms (supersaturated )
!...  Now update water

          CALL awater(irh,tso4,ynh4,xno3,ah2o)

!...ZSR relationship is used to set water levels. Units are
!...  10**(-6) kg water/ (cubic meter of air)
!...  The conversion from micromoles to moles is done by the units of WH

          wh2o = 1.0E-3*ah2o

!...Ionic balance determines the ammonium in solution.

          man = xno3/wh2o
          mas = tso4/wh2o
          mnh4 = 2.0*mas + man
          ynh4 = mnh4*wh2o

!...MAS, MAN and MNH4 are the aqueous concentrations of sulfate, nitrate
!...  and ammonium in molal units (moles/(kg water) ).
!PMA adds avoid cat and an to be < 0
          stion = 3.0*mas + man
          cat(1) = 0.0
          cat(2) = max(mnh4,0.0)
          an(1) = max(mas,0.0)
          an(2) = max(man,0.0)
          an(3) = 0.0
          CALL actcof(cat,an,gams,molnu,phibar)
          gamaan = gams(2,2)

!...Use GAMAAN for convergence control

          eror = abs(gamold-gamaan)/gamold
          gamold = gamaan

!...Check to see if we have a solution

          IF (eror<=toler1) THEN
!cc            WRITE( 11, * ) RH, STION, GAMS( 1, 1 ),GAMS( 1, 2 ), GAMS
!cc     &      GAMS( 2, 1 ), GAMS( 2, 2 ), GAMS( 2, 3 ), PHIBAR

            aso4 = tso4*mwso4
            ano3 = xno3*mwno3
            anh4 = ynh4*mwnh4
            gno3 = (tno3-xno3)*mwhno3
            gnh3 = (tnh4-ynh4)*mwnh3
            ah2o = 1000.0*wh2o
            RETURN
          END IF

        END DO

!...If after NITR iterations no solution is found, then:

        aso4 = tso4*mwso4
        ano3 = 0.0
        ynh4 = twoso4
        anh4 = ynh4*mwnh4
        CALL awater(irh,tso4,ynh4,xno3,ah2o)
        gno3 = tno3*mwhno3
        gnh3 = (tnh4-ynh4)*mwnh3
        RETURN

      ELSE

!......................................
!......... Low Ammonia Case ...........
!......................................

!...coded by Dr. Francis S. Binkowski 12/8/91.(4/26/95)

!...All cases covered by this logic
        wh2o = 0.0
        CALL awater(irh,tso4,tnh4,tno3,ah2o)
        wh2o = 1.0E-3*ah2o
        zh2o = ah2o
!...convert 10**(-6) kg water/(cubic meter of air) to micrograms of wate
!...  per cubic meter of air (1000 g = 1 kg)

        aso4 = tso4*mwso4
        anh4 = tnh4*mwnh4
        ano3 = 0.0
        gno3 = tno3*mwhno3
        gnh3 = 0.0

!...Check for zero water.

        IF (wh2o==0.0) RETURN
        zso4 = tso4/wh2o

!...ZSO4 is the molality of total sulfate i.e. MSO4 + MHSO4

!cc         IF ( ZSO4 .GT. 11.0 ) THEN

!...do not solve for aerosol nitrate for total sulfate molality
!...  greater than 11.0 because the model parameters break down
!...  greater than  9.0 because the model parameters break down

        IF (zso4>9.0) & ! 18 June 97                        
            THEN
          RETURN
        END IF

!...First solve with activity coeffs of 1.0, then iterate.

        phiold = 1.0
        gamana = 1.0
        gamas1 = 1.0
        gamas2 = 1.0
        gamaab = 1.0
        gamold = 1.0

!...All ammonia is considered to be aerosol ammonium.

        mnh4 = tnh4/wh2o

!...MNH4 is the molality of ammonium ion.

        ynh4 = tnh4
!...loop for iteration

        DO nnn = 1, 150
          nitr = nnn

!...set up equilibrium constants including activities
!...  solve the system for hplus first then sulfate & nitrate
!          print*,'gamas,gamana',gamas1,gamas2,gamana
          rk2sa = k2sa*gamas2*gamas2/(gamas1*gamas1*gamas1)
          rkna = kna/(gamana*gamana)
          rknwet = rkna*wh2o
          t21 = zso4 - mnh4
          t221 = zso4 + t21

!...set up coefficients for cubic

          a2 = rk2sa + rknwet - t21
          a1 = rk2sa*rknwet - t21*(rk2sa+rknwet) - rk2sa*zso4 - rkna*tno3
          a0 = -(t21*rk2sa*rknwet+rk2sa*rknwet*zso4+rk2sa*rkna*tno3)


          CALL cubic(a2,a1,a0,nr,crutes)

!...Code assumes the smallest positive root is in CRUTES(1)

          hplus = crutes(1)
          bal = hplus**3 + a2*hplus**2 + a1*hplus + a0
          mso4 = rk2sa*zso4/(hplus+rk2sa) ! molality of sulfat
          mhso4 = zso4 - & ! molality of bisulf
            mso4
          mna = rkna*tno3/(hplus+rknwet) ! molality of nitrat
          mna = max(0.0,mna)
          mna = min(mna,tno3/wh2o)
          xno3 = mna*wh2o
          ano3 = mna*wh2o*mwno3
          gno3 = (tno3-xno3)*mwhno3
!...Calculate ionic strength

          stion = 0.5*(hplus+mna+mnh4+mhso4+4.0*mso4)

!...Update water

          CALL awater(irh,tso4,ynh4,xno3,ah2o)

!...Convert 10**(-6) kg water/(cubic meter of air) to micrograms of wate
!...  per cubic meter of air (1000 g = 1 kg)
!PMA adds checker to avoid cat and an < 0.0
          wh2o = 1.0E-3*ah2o
          cat(1) = max(hplus,0.0)
          cat(2) = max(mnh4,0.0)
          an(1) = max(mso4,0.0)
          an(2) = max(mna,0.0)
          an(3) = max(mhso4,0.0)
!          print*,'actcof',cat(1),cat(2),an(1),an(2),an(3),gams,molnu,phibar
          CALL actcof(cat,an,gams,molnu,phibar)

          gamana = gams(1,2)
          gamas1 = gams(1,1)
          gamas2 = gams(1,3)
          gamaan = gams(2,2)

          gamahat = (gamas2*gamas2/(gamaab*gamaab))
          bhat = khat*gamahat
!cc          EROR = ABS ( ( PHIOLD - PHIBAR ) / PHIOLD )
!cc          PHIOLD = PHIBAR
          eror = abs(gamold-gamahat)/gamold
          gamold = gamahat

!...write out molalities and activity coefficient
!...  and return with good solution

          IF (eror<=toler2) THEN
!cc            WRITE(12,*) RH, STION,HPLUS,ZSO4,MSO4,MHSO4,MNH4,MNA
!cc            WRITE(11,*) RH, STION, GAMS(1,1),GAMS(1,2),GAMS(1,3),
!cc     &                  GAMS(2,1),GAMS(2,2),GAMS(2,3), PHIBAR
            RETURN
          END IF

        END DO

!...after NITR iterations, failure to solve the system, no ANO3

        gno3 = tno3*mwhno3
        ano3 = 0.0
        CALL awater(irh,tso4,tnh4,tno3,ah2o)
        RETURN


      END IF
! ratio .gt. 2.0                                         
! ///////////////////////////////////////////////////
    END SUBROUTINE rpmares_old
!ia*********************************************************
!ia                                                        *
!ia BEGIN OF AEROSOL ROUTINE				   *
!ia							   *
!ia*********************************************************

!***********************************************************************

!   	BEGIN OF AEROSOL CALCULATIONS

!***********************************************************************


!ia*********************************************************************
!ia  									*
!ia	MAIN AEROSOL DYNAMICS ROUTINE					*
!ia	based on MODELS3 formulation by FZB				*
!ia	Modified by IA in May 97					*
!ia     THIS PROGRAMME IS THE LINK BETWEEN GAS PHASE AND AEROSOL PHASE
!ia     CALCULATIONS IN THE COLUMN MODEL. IT CONVERTS ALL DATA AND
!ia     VARIABLES BETWEEN THE TWO PARTS AND DRIVES THE AEROSOL
!ia     CALCULATIONS.
!ia     INPUT DATA REQUIRED FOR AEROSOL DYNAMICS ARE SET UP HERE FOR
!ia     ONE GRID CELL!!!!
!ia     and passed to dynamics calcs. subroutines.
!ia									*
!ia	Revision history						*
!ia	When	WHO	WHAT						*
!ia	----	----	----						*
!ia	????	FZB	BEGIN						*
!ia	05/97	IA	Adapted for use in CTM2-S			*
!ia			Modified renaming/bug fixing			*
!ia     11/97   IA      Modified for new model version
!ia                     see comments under iarev02
!ia     03/98   IA      corrected error on pressure units
!ia									*
!ia	Called BY:	CHEM						*
!ia									*
!ia	Calls to:	OUTPUT1,AEROPRC					*
!ia									*
!ia*********************************************************************

! end RPMares                                                 
    SUBROUTINE rpmmod3(nspcsda,blksize,layer,dtsec,pres,temp,relhum, &
        nitrate_in,nh3_in,vsulf_in,so4rat_in,drog_in,ldrog,condvap_in,ncv, &
        nacv,eeci_in,eecj_in,eorgi_in,eorgj_in,epm25i,epm25j,epmcoarse,    &
        soilrat_in,cblk,igrid,jgrid,kgrid)



!     IMPLICIT NONE

!     Includes:

!iarev02       INCLUDE  AEROINCL.EXT 
! block size, set to 1 in column model  ciarev0
      INTEGER blksize
!ia   			  kept to 1 in current version of column model
      INTEGER numcells

! actual number of cells in arrays ( default is 
      PARAMETER (numcells=1)


      INTEGER layer
! number of layer (default is 1 in

      INTEGER ncell
! index for cell in blocked array (default is 1 in
      PARAMETER (ncell=1)
! *** inputs
! Input temperature [ K ]                      
      REAL temp
! Input relative humidity  [ fraction ]        
      REAL relhum
! Input pressure [ hPa ]                       
      REAL pres
! Input number for Aitken mode [ m**-3 ]       
      REAL numnuc_in
! Input number for accumulation mode [ m**-3 ] 
      REAL numacc_in
! Input number for coarse mode  [ m**-3 ]      
      REAL numcor_in
                         ! sulfuric acid [ ug m**-3 ]
      REAL vsulf_in
! total sulfate vapor as sulfuric acid as      
                         ! sulfuric acid [ ug m**-3 ]
      REAL asulf_in
! total sulfate aerosol as sulfuric acid as    
! i-mode sulfate input as sulfuric acid [ ug m*
      REAL asulfi_in
! ammonia gas [  ug m**-3 ]                    
      REAL nh3_in
! input value of nitric acid vapor [ ug m**-3 ]
      REAL nitrate_in
! Production rate of sulfuric acid   [ ug m**-3
      REAL so4rat_in
                         ! aerosol [ ug m**-3 s**-1 ]
      REAL soilrat_in
! Production rate of soil derived coarse       
! Emission rate of i-mode EC [ug m**-3 s**-1]  
      REAL eeci_in
! Emission rate of j-mode EC [ug m**-3 s**-1]  
      REAL eecj_in
! Emission rate of j-mode org. aerosol [ug m**-
      REAL eorgi_in

      REAL eorgj_in
!bs
! Emission rate of j-mode org. aerosol [ug m**-
! total # of cond. vapors & SOA species 
      INTEGER ncv
! # of anthrop. cond. vapors & SOA speci
      INTEGER nacv
! # of organic aerosol precursor        
      INTEGER ldrog
      REAL drog_in(ldrog)                                 ! organic aerosol precursor [ppm]
! Input delta ROG concentration of      
      REAL condvap_in(ncv) ! cond. vapor input [ug m^-3]           
      REAL drog(blksize,ldrog)                                 ! organic aerosol precursor [ppm]
!bs
! *** Primary emissions rates: [ ug / m**3 s ]

! *** emissions rates for unidentified PM2.5 mass
! Delta ROG concentration of            
      REAL epm25i(blksize) ! Aitken mode                         
      REAL epm25j(blksize) 
! *** emissions rates for primary organic aerosol
! Accumululaton mode                  
      REAL eorgi(blksize) ! Aitken mode                          
      REAL eorgj(blksize) 
! *** emissions rates for elemental carbon
! Accumululaton mode                   
      REAL eeci(blksize) ! Aitken mode                           
      REAL eecj(blksize) 
! *** Primary emissions rates [ ug m**-3 s -1 ] :

! Accumululaton mode                    
      REAL epm25(blksize) ! emissions rate for PM2.5 mass           
      REAL esoil(blksize) ! emissions rate for soil derived coarse a
      REAL eseas(blksize) ! emissions rate for marine coarse aerosol
      REAL epmcoarse(blksize) 
! emissions rate for anthropogenic coarse 

      REAL dtsec

! time step [ s ], PASSED FROM MAIN COLUMN MODE

      REAL newm3

      REAL totaersulf
! total aerosol sulfate                   
! loop index for time steps                     
      INTEGER numsteps

      REAL step

! *** arrays for aerosol model codes:

! synchronization time  [s]                     

      INTEGER nspcsda

! number of species in CBLK ciarev02           
      REAL cblk(blksize,nspcsda) 

! *** Meteorological information in blocked arays:

! *** Thermodynamic variables:

! main array of variables            
      REAL blkta(blksize) ! Air temperature [ K ]                     
      REAL blkprs(blksize) ! Air pressure in [ Pa ]                    
      REAL blkdens(blksize) ! Air density  [ kg m^-3 ]                  
      REAL blkrh(blksize) 


! *** Chemical production rates [ ug m**-3 s -1 ] :

! Fractional relative humidity              
      REAL so4rat(blksize)                                 ! rate [ug/m^3/s]
! sulfuric acid vapor-phase production  
      REAL orgaro1rat(blksize)                                 ! production rate from aromatics [ ug /
! anthropogenic organic aerosol mass    
      REAL orgaro2rat(blksize)                                 ! production rate from aromatics [ ug /
! anthropogenic organic aerosol mass    
      REAL orgalk1rat(blksize)                                 ! rate from alkanes & others [ ug / m^3
! anthropogenic organic aerosol mass pro
      REAL orgole1rat(blksize)                                 ! rate from alkanes & others [ ug / m^3
! anthropogenic organic aerosol mass pro
      REAL orgbio1rat(blksize)                                 ! rate [ ug / m^3 s ]
! biogenic organic aerosol production   
      REAL orgbio2rat(blksize)                                 ! rate [ ug / m^3 s ]
! biogenic organic aerosol production   
      REAL orgbio3rat(blksize)                                 ! rate [ ug / m^3 s ]
! biogenic organic aerosol production   
      REAL orgbio4rat(blksize)                                 ! rate [ ug / m^3 s ]
!bs
! *** atmospheric properties

! biogenic organic aerosol production   
      REAL xlm(blksize) ! atmospheric mean free path [ m ]  
      REAL amu(blksize) 
! *** aerosol properties:


! *** modal diameters:

! atmospheric dynamic viscosity [ kg
      REAL dgnuc(blksize) ! nuclei mode geometric mean diamete
      REAL dgacc(blksize) ! accumulation geometric mean diamet
      REAL dgcor(blksize) 

! *** Modal mass concentrations [ ug m**3 ]

! coarse mode geometric mean diamete
      REAL pmassn(blksize) ! mass concentration in Aitken mode 
      REAL pmassa(blksize) ! mass concentration in accumulation
      REAL pmassc(blksize) 
! *** average modal particle densities  [ kg/m**3 ]

! mass concentration in coarse mode 
      REAL pdensn(blksize) ! average particle density in nuclei
      REAL pdensa(blksize) ! average particle density in accumu
      REAL pdensc(blksize) 
! *** average modal Knudsen numbers

! average particle density in coarse
      REAL knnuc(blksize) ! nuclei mode  Knudsen number       
      REAL knacc(blksize) ! accumulation Knudsen number       
      REAL kncor(blksize) 
! *** reciprocal modal condensation rates for sulfuric acid [ 1/s ]

! coarse mode  Knudsen number       
      REAL fconcn(blksize) 
! reciprocal condensation rate Aitke
      REAL fconca(blksize) !bs
! reciprocal condensation rate acclu
      REAL fconcn_org(blksize)
      REAL fconca_org(blksize)
!bs

! *** Rates for secondary particle formation:

! *** production of new mass concentration [ ug/m**3 s ]
      REAL dmdt(blksize) !                                 by particle formation

! *** production of new number concentration [ number/m**3 s ]

! rate of production of new mass concen
      REAL dndt(blksize) !                                 by particle formation
! *** growth rate for third moment by condensation of precursor
!      vapor on existing particles [ 3rd mom/m**3 s ]

! rate of producton of new particle num
      REAL cgrn3(blksize) !  Aitken mode                          
      REAL cgra3(blksize) 
! *** Rates for coaglulation: [ m**3/s ]

! *** Unimodal Rates:

!  Accumulation mode                    
      REAL urn00(blksize) ! Aitken mode 0th moment self-coagulation ra
      REAL ura00(blksize) 

! *** Bimodal Rates:  Aitken mode with accumulation mode ( d( Aitken mod

! accumulation mode 0th moment self-coagulat
      REAL brna01(blksize) ! rate for 0th moment                     
      REAL brna31(blksize) 
! *** other processes

! rate for 3rd moment                     
      REAL deltaso4a(blksize) !                                  sulfate aerosol by condensation   [ u


! *** housekeeping variables:

! increment of concentration added to   
      INTEGER unit
      PARAMETER (unit=30)

      CHARACTER*16 pname
      PARAMETER (pname=' BOX            ')




      INTEGER isp,igrid,jgrid,kgrid

! loop index for species.                             
      INTEGER ii, iimap(8)
      DATA iimap/1, 2, 18, 19, 21, 22, 23, 24/


!   begin body  of program box

! *** Set up files and other info


! *** set up experimental conditions

! *** initialize model variables

!ia *** not required any more

!ia       DO ISP = 1, NSPCSDA
!ia       CBLK(BLKSIZE,ISP) = 1.0e-19 ! set CBLK to a very small number
!ia       END DO

      step = & ! set time step                                   
        dtsec
      blkta(blksize) = & ! T in Kelvin             
        temp
      blkprs(blksize) = pres* & ! P in  Pa (pres is given in 
        100.
      blkrh(blksize) = & ! fractional RH            
        relhum
      blkdens(blksize) = blkprs(blksize)/(rdgas*blkta(blksize)) !rs      CBLK(BLKSIZE,VSULF) = vsulf_in
!rs      CBLK(BLKSIZE,VHNO3) = nitrate_in
!rs      CBLK(BLKSIZE,VNH3) =  nh3_in
!bs
!rs      CBLK(BLKSIZE,VCVARO1) = condvap_in(PSOAARO1)
!rs      CBLK(BLKSIZE,VCVARO2) = condvap_in(PSOAARO2)
!rs      CBLK(BLKSIZE,VCVALK1) = condvap_in(PSOAALK1)
!rs      CBLK(BLKSIZE,VCVOLE1) = condvap_in(PSOAOLE1)
!rs      CBLK(BLKSIZE,VCVAPI1) = condvap_in(PSOAAPI1)
!rs      CBLK(BLKSIZE,VCVAPI2) = condvap_in(PSOAAPI2)
!rs      CBLK(BLKSIZE,VCVLIM1) = condvap_in(PSOALIM1)
!rs      CBLK(BLKSIZE,VCVLIM2) = condvap_in(PSOALIM2)
! dr
      DO isp = 1, ldrog
        drog(blksize,isp) = drog_in(isp)
      END DO
!      print*,'drog in rpm',drog
!bs
!ia *** 27/05/97 the following variables are transported quantities
!ia *** of the column-model now and thuse do not need this init.
!ia *** step.

!     CBLK(BLKSIZE,VNU0) = numnuc_in
!     CBLK(BLKSIZE,VAC0) = numacc_in
!     CBLK(BLKSIZE,VSO4A) =  asulf_in
!     CBLK(BLKSIZE,VSO4AI) = asulfi_in
!     CBLK(BLKSIZE, VCORN) = numcor_in


      so4rat(blksize) = so4rat_in

!...INITIALISE EMISSION RATES

!     epm25i(blksize) = & ! unidentified PM2.5 mass                  
!       0.0
!     epm25j(blksize) = & 
!       0.0
! unidentified PM2.5 m
      eorgi(blksize) = & ! primary organic     
        eorgi_in
      eorgj(blksize) = & 
        eorgj_in
! primary organic     
      eeci(blksize) = & ! elemental carbon    
        eeci_in
      eecj(blksize) = & 
        eecj_in
! elemental carbon    
      epm25(blksize) = & !currently from input file ACTIONIA        
        0.0
      esoil(blksize) = & ! ACTIONIA                          
        soilrat_in
      eseas(blksize) = & !currently from input file ACTIONIA        
        0.0
!     epmcoarse(blksize) = & !currently from input file ACTIONIA    
!       0.0
      dgnuc(blksize) = dginin
      dgacc(blksize) = dginia
      dgcor(blksize) = dginic
      newm3 = 0.0



! *** Set up initial total 3rd moment factors

      totaersulf = 0.0
      newm3 = 0.0
! ***  time loop

!       write(50,*) ' numsteps dgnuc dgacc ',
!    &      ' aso4 aso4i Ni Nj ah2o ah2oi M3i m3j'


! *** Call aerosol routines

      CALL aeroproc(blksize,nspcsda,numcells,layer,cblk,step,blkta,blkprs, &
        blkdens,blkrh,so4rat,orgaro1rat,orgaro2rat,orgalk1rat, &
        orgole1rat,orgbio1rat,orgbio2rat,orgbio3rat,orgbio4rat,drog,ldrog,ncv, &
        nacv,epm25i,epm25j,eorgi,eorgj,eeci,eecj,epmcoarse,esoil,eseas,xlm, &
        amu,dgnuc,dgacc,dgcor,pmassn,pmassa,pmassc,pdensn,pdensa,pdensc,knnuc, &
        knacc,kncor,fconcn,fconca,fconcn_org,fconca_org,dmdt,dndt,cgrn3,cgra3, &
        urn00,ura00,brna01,brna31,deltaso4a,igrid,jgrid,kgrid)

! *** write output

!      WRITE(UNIT,*) ' AFTER AEROPROC '
!      WRITE(UNIT,*) ' NUMSTEPS = ', NUMSTEPS

! *** Write out file for graphing.

!     write(50,*) NUMSTEPS, DGNUC,DGACC,(CBLK(1,iimap(ii)),ii=1,8)


! *** update sulfuric acid vapor
!ia 21.04.98 this update is not required here
!ia artefact from box model
!       CBLK(BLKSIZE,VSULF) = CBLK(BLKSIZE,VSULF) +
!    &            SO4RAT(BLKSIZE) * STEP

      RETURN

!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
    END SUBROUTINE rpmmod3
! main box model                                            
    SUBROUTINE soa_part(layer,blkta,blkprs,orgaro1rat,orgaro2rat,orgalk1rat, &
        orgole1rat,orgbio1rat,orgbio2rat,orgbio3rat,orgbio4rat,drog,ldrog,ncv, &
        nacv,cblk,blksize,nspcsda,numcells,dt)
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs                                                                    !
!bs  Description:                                                      !
!bs                                                                    !
!bs  SOA_PART calculates the formation and partitioning of secondary   !
!bs  organic aerosol based on (pseudo-)ideal solution thermodynamics.  !
!bs                                                                    !
!bs  This code considers two cases:                                    !
!bs   i) initil absorbing mass is existend in the aerosol phase        !
!bs  ii) a threshold has to be exeeded before partitioning (even below !
!bs      saturation) will take place.                                  !
!bs                                                                    !
!bs  The temperature dependence of the saturation concentrations are   !
!bs  calculated using the Clausius-Clapeyron equation.                 !
!bs                                                                    !
!bs  It is assumed that the condensable vapors also evaporate if the   !
!bs  saturation concentraion lowers e.g. due to temperature effects.   !
!bs  Therefor negative production rates (= evaporation rates) are      !
!bs  possible.                                                         !
!bs                                                                    !
!bs  If there is no absorbing mass at all the Pandis method is applied !
!bs  for the first steps.                                              !
!bs                                                                    !
!bs  References:                                                       !
!bs    Pankow (1994):                                                  !
!bs     An absorption model of the gas/aerosol                         !
!bs     partitioning involved in the formation of                      !
!bs     secondary organic aerosol, Atmos. Environ. 28(2),              !
!bs     189-193.                                                       !
!bs    Odum et al. (1996):                                             !
!bs     Gas/particle partitioning and secondary organic                !
!bs     aerosol yields,  Environ. Sci. Technol. 30,                    !
!bs     2580-2585.                                                     !
!bs    see also                                                        !
!bs    Bowman et al. (1997):                                           !
!bs     Mathematical model for gas-particle partitioning               !
!bs     of secondary organic aerosols, Atmos. Environ.                 !
!bs     31(23), 3921-3931.                                             !
!bs    Seinfeld and Pandis (1998):                                     !
!bs     Atmospheric Chemistry and Physics (0-471-17816-0)              !
!bs     chapter 13.5.2 Formation of binary ideal solution              !
!bs     with -- preexisting aerosol                                    !
!bs          -- other organic vapor                                    !
!bs                                                                    !
!bs  Called by:     SORGAM                                             !
!bs                                                                    !
!bs  Calls:         None                                               !
!bs                                                                    !
!bs  Arguments:     LAYER,                                             !
!bs                 BLKTA, BLKPRS,                                     !
!bs                 ORGARO1RAT, ORGARO2RAT,                            !
!bs                 ORGALK1RAT, ORGOLE1RAT,                            !
!bs                 ORGBIO1RAT, ORGBIO2RAT,                            !
!bs                 ORGBIO3RAT, ORGBIO4RAT,                            !
!bs                 DROG, LDROG, NCV, NACV,                            !
!bs                 CBLK, BLKSIZE, NSPCSDA, NUMCELLS,                  !
!bs                 DT                                                 !
!bs                                                                    !
!bs  Include files: AEROSTUFF.EXT                                      !
!bs                 AERO_internal.EXT                                  !
!bs                                                                    !
!bs  Data:          None                                               !
!bs                                                                    !
!bs  Input files:   None                                               !
!bs                                                                    !
!bs  Output files:  None                                               !
!bs                                                                    !
!bs--------------------------------------------------------------------!
!bs                                                                    !
!bs  History:                                                          !
!bs   No    Date    Author           Change                            !
!bs  ____  ______  ________________  _________________________________ !
!bs   01   170399   B.Schell         Set up                            !
!bs   02   050499   B.Schell         introduced SR NEWT                !
!bs   03   040599   B.Schell         include-file sorgam.inc           !
!bs                                                                    !
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs
!     IMPLICIT NONE
!bs
!bs * includes
!bs
!bs
!bs * input variables
!bs
! model layer                     
      INTEGER layer
! dimension of arrays             
      INTEGER blksize
! number of species in CBLK       
      INTEGER nspcsda
! actual number of cells in arrays
      INTEGER numcells
! # of organic aerosol precursor  
      INTEGER ldrog
! total # of cond. vapors & SOA sp
      INTEGER ncv
! # of anthrop. cond. vapors & SOA
      INTEGER nacv
      REAL cblk(blksize,nspcsda) ! main array of variables         
! model time step in  SECONDS     
      REAL dt
      REAL blkta(blksize) ! Air temperature [ K ]           
      REAL blkprs(blksize) ! Air pressure in [ Pa ]          
      REAL orgaro1rat(blksize)                                       ! rates from aromatics
! anth. organic vapor production  
      REAL orgaro2rat(blksize)                                       ! rates from aromatics
! anth. organic vapor production  
      REAL orgalk1rat(blksize)                                       ! rates from alkenes and others
! anth. organic vapor production  
      REAL orgole1rat(blksize)                                       ! rates from alkanes and others
! anth. organic vapor production  
      REAL orgbio1rat(blksize) ! bio. organic vapor production ra
      REAL orgbio2rat(blksize) ! bio. organic vapor production ra
      REAL orgbio3rat(blksize) ! bio. organic vapor production ra
      REAL orgbio4rat(blksize) ! bio. organic vapor production ra
      REAL drog(blksize,ldrog) !bs
!bs * local variable declaration
!bs
! Delta ROG conc. [ppm]           
!bs numerical value for a minimum thresh
      REAL thrsmin
      PARAMETER (thrsmin=1.E-19)
!bs numerical value for a minimum thresh
!bs
!bs universal gas constant [J/mol-K]    
      REAL rgas
      PARAMETER (rgas=8.314510)
!bs reference temperature T0 = 298 K    
      REAL tnull
      PARAMETER (tnull=298.)
!bs molecular weight for C              
      REAL mwc
      PARAMETER (mwc=12.0)
!bs molecular weight for organic species
      REAL mworg
      PARAMETER (mworg=175.0)
!bs molecular weight for SO4            
      REAL mwso4
      PARAMETER (mwso4=96.0576)
!bs molecular weight for NH4            
      REAL mwnh4
      PARAMETER (mwnh4=18.03858)
!bs molecular weight for NO3            
      REAL mwno3
      PARAMETER (mwno3=62.01287)
!bs relative tolerance for mass check   
      REAL rtol
      PARAMETER (rtol=1.E-04)
!bs      REAL DTMIN                !bs minimum time step in seconds
!bs      PARAMETER (DTMIN = 0.1)
!bs
!bs loop index                          
      INTEGER lcell
      INTEGER l, & !bs loop index                          
        n
!bs conversion factor ppm --> ug/m^3    
      REAL convfac
!bs difference of inverse temperatures  
      REAL ttinv
!bs weighted initial organic mass [10^-6
      REAL minitw
!bs weighted total organic mass [10^-6 m
      REAL mtotw
!bs weighted inorganic mass [10^-6 mol/m
      REAL mnonow
!bs 1. / MTOTW                          
      REAL imtotw
!bs initial organic mass [ug/m^3]       
      REAL minit
!bs inorganic mass [ug/m^3]             
      REAL mnono
!bs total organic mass [ug/m^3]         
      REAL mtot
!bs threshold for SOA formatio for low M
      REAL thres
!bs mass check ratio of input/output mas
      REAL mcheck
      REAL msum(ncv) !bs input total mass [ug/m^3]           
      REAL mwcv(ncv) !bs molecular weight of cond. vapors [g/
      REAL imwcv(ncv) !bs 1. / MWCV(NCV)                      
      REAL pnull(ncv) !bs vapor pres. of pure cond. vapor [Pa]
      REAL dhvap(ncv) !bs heat of vaporisation of compound i [
      REAL pvap(ncv) !bs vapor pressure cond. vapor [Pa]     
      REAL ctot(ncv) !bs total conc. of cond. vapor aerosol +
      REAL cgas(ncv) !bs gasphase concentration of cond. vapo
      REAL caer(ncv) !bs aerosolphase concentration of cond. 
      REAL asav(ncv) !bs saved CAER for iteration            
      REAL aold(ncv) !bs saved CAER for rate determination   
      REAL csat(ncv) !bs saturation conc. of cond. vapor [ug/
      REAL alpha(ncv) !bs molar yield for condensable vapors  
      REAL prod(ncv) !bs production of condensable vapor [ug/
      REAL p(ncv) !bs PROD(L) * TIMEFAC [ug/m^3]          
      REAL f(ldrog) !bs scaling factor for ind. oxidant     
!bs check convergence of SR NEWT        
      LOGICAL check
!bs
      INTEGER its
!bs * initialisation
!bs
!bs * DVAP data: average value calculated from C14-C18 monocarboxylic
!bs *      acids and C5-C6 dicarboxylic acids. Tao and McMurray (1989):
!bs *      Eniron. Sci. Technol. 1989, 23, 1519-1523.
!bs *      average value is 156 kJ/mol
!bs
!bs number of iterations in NEWT        
      dhvap(psoaaro1) = 156.0E03
      dhvap(psoaaro2) = 156.0E03
      dhvap(psoaalk1) = 156.0E03
      dhvap(psoaole1) = 156.0E03
      dhvap(psoaapi1) = 156.0E03
      dhvap(psoaapi2) = 156.0E03
      dhvap(psoalim1) = 156.0E03
      dhvap(psoalim2) = 156.0E03
!bs
!bs * MWCV data: average value calculated from C14-C18 monocarboxylic
!bs *      acids and C5-C6 dicarboxylic acids. Tao and McMurray (1989):
!bs *      Eniron. Sci. Technol. 1989, 23, 1519-1523.
!bs *      average value is 222.5 g/mol
!bs *
!bs * molecular weights used are estimates taking the origin (reactants)
!bs *      into account. This should be updated if more information abou
!bs *      the products is available.
!bs *      First hints are taken from Forstner et al. (1997), Environ. S
!bs *        Technol. 31(5), 1345-1358. and Forstner et al. (1997), Atmo
!bs *        Environ. 31(13), 1953-1964.
!bs *
!bs * !! these molecular weights should be identical with WTM in CTM !!
!bs
      mwcv(psoaaro1) = 150.
      mwcv(psoaaro2) = 150.
      mwcv(psoaalk1) = 140.
      mwcv(psoaole1) = 140.
      mwcv(psoaapi1) = 184.
      mwcv(psoaapi2) = 184.
      mwcv(psoalim1) = 200.
      mwcv(psoalim2) = 200.
!bs
!bs * aromatic yields from:
!bs * Odum J.R., T.P.W. Jungkamp, R.J. Griffin, R.C. Flagan, and
!bs *   J.H. Seinfeld: The atmospheric aerosol-forming potential of whol
!bs *   gasoline vapor, Science 276, 96-99, 1997.
!bs * Odum J.R., T.P.W. Jungkamp, R.J. Griffin, H.J.L. Forstner, R.C. Fl
!bs *   and J.H. Seinfeld: Aromatics, reformulated gasoline, and atmosph
!bs *   organic aerosol formation, Environ. Sci. Technol. 31, 1890-1897,
!bs *
!bs * !! yields provided by Odum are mass-based stoichiometric coefficen
!bs *    average for high and low yield aromatics
!bs *    alpha1 = 0.0545  K1 = 0.0475 m^3/ug
!bs *    alpha2 = 0.1525  K2 = 0.00165 m^3/ug
!bs *    change to molar yields using the model MW
!bs *    alpha1 * MW(XYL) / MW(PSOAARO1) = alpha1 * 106 / 150 = 0.0385
!bs *    alpha2 * MW(XYL) / MW(PSOAARO2) = alpha2 * 106 / 150 = 0.1077
!bs *   ALPHA(PSOAARO1) = 0.0385; ALPHA(PSOAARO2) = 0.1077
!bs *
!bs
!bs * alkane and alkene yields from:
!bs * Moucheron M.C. and J. Milford: Development and testing of a proces
!bs *    model for secondary organic aerosols. Air & Waste Manag. Assoc.
!bs *    for presentation at the 89th Annual Meeting & Exhibition, Nashv
!bs *    Tennessee, June 23-28, 96-FA130B.03, 1996.
!bs *  molar yields used instead of [ ug m^-3 / ppm ], calculation
!bs *    at T=298K, P=1.0133*10^5 Pa
!bs *    ALPHA(PSOAALK1) = 0.048; ALPHA(PSOAOLE1) = 0.008
!bs
!bs * biogenic yields from:
!bs * Griffin R.J., D.R. Cocker III, R.C. Flagan, and J.H. Seinfeld:
!bs *   Organic aerosol formation from the oxidation of biogenic hydro-
!bs *   carbons, JGR, 1999 in press.
!bs *   the yields given in Table 3 are mass yields [ ug m^-3 / ug m^-3
!bs *   change to molar yields via:
!bs *   molar yield = mass yield * ((R*T/M_soa*p) / (R*T/M_terp*p))
!bs *               = mass yield * (M_terp / M_soa)
!bs *               = mass yield * ( M(Terpenes) / M(pinonic acid) )
!bs *               = mass yield * 136 / 184
!bs * average for a-Pinene and Limonene, maybe splitted in future versio
!bs *    0.138 * 0.739 = 0.102; 0.345 * 0.739 = 0.254
!bs * values for a-Pinene (molar yield) alpha1 = 0.028, alpha2 = 0.241
!bs * values for limonene (molar yield) alpha1 = 0.163, alpha2 = 0.247
!bs
      alpha(psoaaro1) = 0.039
      alpha(psoaaro2) = 0.108
      alpha(psoaalk1) = 0.048
      alpha(psoaole1) = 0.008
!bs      ALPHA(PSOAAPI1) = 0.028
!bs      ALPHA(PSOAAPI2) = 0.241
      alpha(psoaapi1) = & !bs API + O3 only Griffin '99           
        0.092
      alpha(psoaapi2) = & !bs API + O3 only Griffin '99           
        0.075
      alpha(psoalim1) = 0.163
      alpha(psoalim2) = 0.247
!bs
!bs * P0 data in Pa for T = 298K:
!bs *    aromatics: Odum et al. (1997) using R = 8.314 J/(mol*K),
!bs *         DHvap = 156 kJ/mol, T = 313K, MW = 150 g/mol and averaged
!bs *         Ki's of high and low aromatics.
!bs *         T = 313   => PNULL(ARO1) = 1.7E-05, PNULL(ARO2) = 5.1E-04
!bs *         T = 307.4 => PNULL(ARO1) = 5.7E-05, PNULL(ARO2) = 1.6E-03
!bs *    biogenics: Hoffmann et al. (1997); Griffin et al. (1999);
!bs *         using R = 8.314 J/(mol*K),
!bs *         DHvap = 156 kJ/mol, T = 313, MW = 184 g/mol, and
!bs *         averaged Ki's of a-pinene and limonene
!bs *         p1(298K) = 6.1E-06; p2(298K) = 1.5E-04
!bs *         Ki's for a-pinene p1(298K) = 4.0E-06; p2(298K) = 1.7E-04
!bs *         Ki's for limonene p1(298K) = 2.5E-05; p2(298K) = 1.2E-04
!bs *    alkanes and alkenes: no data available, use low value to get cl
!bs *         to the Pandis yields, 1 ppt = 1*10^-7 Pa.
!bs
      pnull(psoaaro1) = 5.7E-05
      pnull(psoaaro2) = 1.6E-03
      pnull(psoaalk1) = 5.0E-06
      pnull(psoaole1) = 5.0E-06
!bs      PNULL(PSOAAPI1) = 4.0E-06
!bs      PNULL(PSOAAPI2) = 1.7E-04
      pnull(psoaapi1) = & !bs API + O3 only Griffin '99         
        2.488E-05
      pnull(psoaapi2) = & !bs API + O3 only Griffin '99         
        2.778E-05
      pnull(psoalim1) = 2.5E-05
      pnull(psoalim2) = 1.2E-04
!bs
!bs * scaling of contribution of individual oxidants to aerosol formatio
!bs
      f(pxyl) = & !bs * XYL + OH                          
        1.
      f(ptol) = & !bs * TOL + OH                          
        1.
      f(pcsl1) = & !bs * CSL + OH                          
        1.
      f(pcsl2) = & !bs * CSL + NO                          
        1.
      f(phc8) = & !bs * HC  + OH                          
        1.
      f(poli1) = & !bs * OLI + OH                          
        1.
      f(poli2) = & !bs * OLI + NO                          
        1.
      f(poli3) = & !bs * OLI + O3                          
        1.
      f(polt1) = & !bs * OLT + OH                          
        1.
      f(polt2) = & !bs * OLT + NO                          
        1.
      f(polt3) = & !bs      F(PAPI1) = 0.228          !bs * API + OH
        1.
!bs      F(PAPI2) = 0.             !bs * API + NO
!bs      F(PAPI3) = 0.771          !bs * API + O3
!bs * OLT + O3                          
      f(papi1) = & !bs * API + OH                          
        0.
      f(papi2) = & !bs * API + NO                          
        0.
      f(papi3) = & !bs * API + O3                          
        1.
      f(plim1) = & !bs * LIM + OH                          
        0.228
      f(plim2) = & !bs * LIM + NO                          
        0.
      f(plim3) = & !bs
        0.771
!bs * begin code -------------------------------------------------------
!bs
!bs * LIM + O3                          
      DO lcell = 1, numcells
        DO l = 1, ldrog
          drog(lcell,l) = f(l)*drog(lcell,l)
        END DO
        ttinv = 1./tnull - 1./blkta(lcell)
        convfac = blkprs(lcell)/(rgas*blkta(lcell))
        cgas(psoaaro1) = cblk(lcell,vcvaro1)
        cgas(psoaaro2) = cblk(lcell,vcvaro2)
        cgas(psoaalk1) = cblk(lcell,vcvalk1)
        cgas(psoaole1) = cblk(lcell,vcvole1)
        cgas(psoaapi1) = cblk(lcell,vcvapi1)
        cgas(psoaapi2) = cblk(lcell,vcvapi2)
        cgas(psoalim1) = cblk(lcell,vcvlim1)
        cgas(psoalim2) = cblk(lcell,vcvlim2)
        caer(psoaaro1) = cblk(lcell,vorgaro1j) + cblk(lcell,vorgaro1i)
        caer(psoaaro2) = cblk(lcell,vorgaro2j) + cblk(lcell,vorgaro2i)
        caer(psoaalk1) = cblk(lcell,vorgalk1j) + cblk(lcell,vorgalk1i)
        caer(psoaole1) = cblk(lcell,vorgole1j) + cblk(lcell,vorgole1i)
        caer(psoaapi1) = cblk(lcell,vorgba1j) + cblk(lcell,vorgba1i)
        caer(psoaapi2) = cblk(lcell,vorgba2j) + cblk(lcell,vorgba2i)
        caer(psoalim1) = cblk(lcell,vorgba3j) + cblk(lcell,vorgba3i)
        caer(psoalim2) = cblk(lcell,vorgba4j) + cblk(lcell,vorgba4i)
!bs
        prod(psoaaro1) = drog(lcell,pxyl) + drog(lcell,ptol) + &
          drog(lcell,pcsl1) + drog(lcell,pcsl2)
        prod(psoaaro2) = drog(lcell,pxyl) + drog(lcell,ptol) + &
          drog(lcell,pcsl1) + drog(lcell,pcsl2)
        prod(psoaalk1) = drog(lcell,phc8)
        prod(psoaole1) = drog(lcell,poli1) + drog(lcell,poli2) + &
!jdf bug  drog(lcell,poli3) + drog(lcell,polt1) + drog(lcell,poli2) + &
          drog(lcell,poli3) + drog(lcell,polt1) + drog(lcell,polt2) + &
          drog(lcell,polt3)
        prod(psoaapi1) = drog(lcell,papi1) + drog(lcell,papi2) + &
          drog(lcell,papi3)
        prod(psoaapi2) = drog(lcell,papi1) + drog(lcell,papi2) + &
          drog(lcell,papi3)
        prod(psoalim1) = drog(lcell,plim1) + drog(lcell,plim2) + &
          drog(lcell,plim3)
        prod(psoalim2) = drog(lcell,plim1) + drog(lcell,plim2) + &
          drog(lcell,plim3)
!bs
!bs * calculate actual production from gasphase reactions [ug/m^3]
!bs * calculate vapor pressure of pure compound as a liquid
!bs *   using the Clausius-Clapeyromn equation and the actual
!bs *   saturation concentration.
!bs * calculate the threshold for partitioning if no initial mass
!bs *   is present to partition into.
!bs
        thres = 0.
        mtot = 0.
        mtotw = 0.
        DO l = 1, ncv
          prod(l) = convfac*mwcv(l)*alpha(l)*prod(l)
          ctot(l) = prod(l) + cgas(l) + caer(l) !bs redefined below   
          p(l) = prod(l)
          msum(l) = cgas(l) + caer(l) + prod(l)
          aold(l) = caer(l)
          imwcv(l) = 1./mwcv(l)
          pvap(l) = pnull(l)*exp(dhvap(l)/rgas*ttinv)
          csat(l) = pvap(l)*mwcv(l)*1.0E06/(rgas*blkta(lcell))
          thres = thres + ((cgas(l)+prod(l))/csat(l))
          mtot = mtot + caer(l)
          mtotw = mtotw + caer(l)*imwcv(l)
        END DO
!bs
!bs * small amount of non-volatile absorbing mass is assumed to be
!bs * present (following Bowman et al. (1997) 0.01% of the inorganic
!bs * mass in each size section, here mode)
!bs
        mnono = 0.0001*(cblk(lcell,vso4aj)+cblk(lcell,vnh4aj)+cblk(lcell, &
          vno3aj))
        mnono = mnono + 0.0001*(cblk(lcell,vso4ai)+cblk(lcell,vnh4ai)+cblk( &
          lcell,vno3ai))
        mnonow = 0.0001*(cblk(lcell,vso4aj)/mwso4+cblk(lcell,vnh4aj)/mwnh4+ &
          cblk(lcell,vno3aj)/mwno3)
        mnonow = mnonow + 0.0001*(cblk(lcell,vso4ai)/mwso4+cblk(lcell,vnh4ai)/ &
          mwnh4+cblk(lcell,vno3ai)/mwno3)
        mnono = max(mnono,conmin)
        mnonow = max(mnonow,conmin)
!bs
!bs         MNONOW = 0.
!bs         MNONO  = 0.
!bs
        minit = cblk(lcell,vecj) + cblk(lcell,veci) + cblk(lcell,vorgpaj) + &
          cblk(lcell,vorgpai) + mnono
        minitw = (cblk(lcell,vecj)+cblk(lcell,veci))/mwc + &
          (cblk(lcell,vorgpaj)+cblk(lcell,vorgpai))/mworg + mnonow
!bs
!bs * If MINIT is set to zero partitioning will occur if the pure
!bs * saturation concentation is exceeded (Pandis et al. 1992).
!bs * If some amount of absorbing organic mass is formed gas/particle
!bs * partitioning will follow the ideal solution approach.
!bs
        minit = 0.
        minitw = 0.
!bs
        mtot = mtot + minit
        mtotw = mtotw + minitw
        imtotw = 1./mtotw
!bs
!bs * do the gas/particle partitioning
!bs
        IF ((thres>1 .AND. minitw<thrsmin) .OR. (minitw>thrsmin) .OR. &
            (mtot>thrsmin)) THEN
!bs
          DO l = 1, ncv
            ctot(l) = p(l) + cgas(l) + caer(l)
            caer(l) = ctot(l) !bs 'initial' guess      
          END DO
!bs
!bs * globally convergent method for nonlinear system of equations
!bs * adopted from Numerical Recipes 2nd Edition
!bs
          CALL newt(layer,caer,ncv,check,ctot,csat,imwcv,minitw,its)
!bs
          IF (check) THEN
!           WRITE (6,'(a,i2)') '!! Problems in SR NEWT !! K: ', layer
          END IF
!bs
!bs       IF (layer==1) WRITE (76,'(i3)') its
!bs
          DO l = 1, ncv
            IF (caer(l)<=tolmin) THEN
!             IF (abs(caer(l))>tolmin) WRITE (6,90000) l, caer(l)
              caer(l) = conmin
            END IF
            IF (caer(l)>ctot(l)) THEN
              IF (caer(l)-ctot(l)>tolmin) THEN
!                WRITE (6,90010)
              END IF
              caer(l) = ctot(l)
            END IF
            cgas(l) = ctot(l) - caer(l)
          END DO
!bs
!90000     FORMAT ('!! PROBLEMS WITH CAER, CAER < 0. !!',1X,I1,1PE14.6)
!90010     FORMAT ('!! PROBLEMS WITH CAER, CAER > CTOT !!')
!bs
!bs * assign values to CBLK array
!bs
          cblk(lcell,vcvaro1) = max(cgas(psoaaro1),conmin)
          cblk(lcell,vcvaro2) = max(cgas(psoaaro2),conmin)
          cblk(lcell,vcvalk1) = max(cgas(psoaalk1),conmin)
          cblk(lcell,vcvole1) = max(cgas(psoaole1),conmin)
          cblk(lcell,vcvapi1) = max(cgas(psoaapi1),conmin)
          cblk(lcell,vcvapi2) = max(cgas(psoaapi2),conmin)
          cblk(lcell,vcvlim1) = max(cgas(psoalim1),conmin)
          cblk(lcell,vcvlim2) = max(cgas(psoalim2),conmin)
          orgaro1rat(lcell) = (caer(psoaaro1)-aold(psoaaro1))/dt
          orgaro2rat(lcell) = (caer(psoaaro2)-aold(psoaaro2))/dt
          orgalk1rat(lcell) = (caer(psoaalk1)-aold(psoaalk1))/dt
          orgole1rat(lcell) = (caer(psoaole1)-aold(psoaole1))/dt
          orgbio1rat(lcell) = (caer(psoaapi1)-aold(psoaapi1))/dt
          orgbio2rat(lcell) = (caer(psoaapi2)-aold(psoaapi2))/dt
          orgbio3rat(lcell) = (caer(psoalim1)-aold(psoalim1))/dt
          orgbio4rat(lcell) = (caer(psoalim2)-aold(psoalim2))/dt
!bs
!bs
        ELSE
!bs            WRITE(6,'(a)') 'Pandis method in SR SOA_PART.F used!'
!bs            WRITE(6,1010) THRES, MINITW
!bs 1010       FORMAT('THRES =',1pe14.6,1X,'MINITW =',1pe14.6)
!bs
!bs do Pandis method                    
          DO l = 1, ncv
            caer(l) = ctot(l) - csat(l)
            caer(l) = max(caer(l),0.)
            cgas(l) = ctot(l) - caer(l)
          END DO
!bs
          cblk(lcell,vcvaro1) = cgas(psoaaro1)
          cblk(lcell,vcvaro2) = cgas(psoaaro2)
          cblk(lcell,vcvalk1) = cgas(psoaalk1)
          cblk(lcell,vcvole1) = cgas(psoaole1)
          cblk(lcell,vcvapi1) = cgas(psoaapi1)
          cblk(lcell,vcvapi2) = cgas(psoaapi2)
          cblk(lcell,vcvlim1) = cgas(psoalim1)
          cblk(lcell,vcvlim2) = cgas(psoalim2)
          orgaro1rat(lcell) = (caer(psoaaro1)-aold(psoaaro1))/dt
          orgaro2rat(lcell) = (caer(psoaaro2)-aold(psoaaro2))/dt
          orgalk1rat(lcell) = (caer(psoaalk1)-aold(psoaalk1))/dt
          orgole1rat(lcell) = (caer(psoaole1)-aold(psoaole1))/dt
          orgbio1rat(lcell) = (caer(psoaapi1)-aold(psoaapi1))/dt
          orgbio2rat(lcell) = (caer(psoaapi2)-aold(psoaapi2))/dt
          orgbio3rat(lcell) = (caer(psoalim1)-aold(psoalim1))/dt
          orgbio4rat(lcell) = (caer(psoalim2)-aold(psoalim2))/dt
!bs
        END IF
!bs
!bs * check mass conservation
!bs
        DO l = 1, ncv
!rs check is component exits
          IF (cgas(l)==0. .AND. caer(l)==0. .AND. msum(l)==0) THEN
            mcheck = 1.
          ELSE
            mcheck = (cgas(l)+caer(l))/msum(l)
          END IF
          IF ((mcheck<1.-rtol) .OR. (mcheck>1.+rtol)) THEN
!           WRITE (6,'(/,a)') 'Problems with mass conservation!'
!           WRITE (6,90020) layer, l, mcheck, cgas(l) + caer(l)
!           WRITE (6,'(a)') '!! CHECK RESULTS !!'
90020       FORMAT ('LAYER = ',I2,', L = ',I2,', MCHECK = ',E12.6,', MASS = ', &
              E12.6)
          END IF
        END DO
!bs
!bs
      END DO
!bs * end of SR SOA_PART
!bs
!bs loop over NUMCELLS                  
      RETURN
    END SUBROUTINE soa_part
    SUBROUTINE sorgam(layer,blkta,blkprs,orgaro1rat,orgaro2rat,orgalk1rat, &
        orgole1rat,orgbio1rat,orgbio2rat,orgbio3rat,orgbio4rat,drog,ldrog,ncv, &
        nacv,cblk,blksize,nspcsda,numcells,dt)
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs                                                                    !
!bs  Description:   Secondary organic aerosol module                   !
!bs                 This module calculates the gas/particle parti-     !
!bs                 tioning of semi-volatile organic vapors            !
!bs                                                                    !
!bs  Called by:     RPMMOD3                                            !
!bs                                                                    !
!bs  Calls:         SOA_PANDIS                                         !
!bs                 SOA_PART                                           !
!bs                                                                    !
!bs  Arguments:     LAYER, BLKTA,                                      !
!bs                 ORGARO1RAT, ORGARO2RAT,                            !
!bs                 ORGALK1RAT, ORGOLE1RAT,                            !
!bs                 ORGBIO1RAT, ORGBIO2RAT,                            !
!bs                 DROG, LDROG,                                       !
!bs                 CBLK, BLKSIZE, NSPCSDA, NUMCELLS,                  !
!bs                 DT                                                 !
!bs                                                                    !
!bs  Include files: AEROSTUFF.EXT                                      !
!bs                 AERO_internal.EXT                                  !
!bs                                                                    !
!bs  Data:                                                             !
!bs                                                                    !
!bs  Input files:   None                                               !
!bs                                                                    !
!bs  Output files:  UNIT 90: control output                            !
!bs                                                                    !
!bs--------------------------------------------------------------------!
!bs                                                                    !
!bs  History:                                                          !
!bs   No    Date    Author           Change                            !
!bs  ____  ______  ________________  _________________________________ !
!bs   01   040299   B.Schell         Set up                            !
!bs                                                                    !
!bs ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS ** ** BS ** BS *!
!bs
!bs * Literature:
!bs * Pandis et al. (1992): Secondary organic aerosol formation and
!bs *     transport. Atmos Environ. 26A, 2453-2466.
!bs * Seinfeld and Pandis (1998): Atmospheric Chemistry and Physics
!bs *     chapter 13.5.2 Noninteracting SOA compounds. (0-471-17816-0)
!bs * STI Report (Sonoma Technology, Inc.) (1998):
!bs *     Development of gas-phase chemistry, secondary organic aerosol,
!bs *     and aqueous-phase chemistry modules for PM modeling.
!bs *     By: R. Strader, C. Gurciullo, S. Pandis, N. Kumar, F. Lurmann
!bs *     Prepared for: Coordinating Research Council, Atlanta, Aug 24 1
!bs * Tao and McMurray (1989): Vapor pressures and surface free energies
!bs *     C14-C18 monocarboxylic acids and C5 and C6 dicarboxylic acids.
!bs *     Eniron. Sci. Technol. 23, 1519-1523.
!bs * Pankow (1994): An absorption model of gas/particle partitioning of
!bs *     organic compounds in the atmosphere. Atmos. Environ. 28, 185-1
!bs * Pankow (1994): An absorption model of gas/aerosol partitioning inv
!bs *     in the formation of secondary organic aerosol.
!bs *     Atmos. Environ. 28, 189-193.
!bs * Odum et al. (1996): Gas/particle partitioning and secondary organi
!bs *     aerosol yields. Environ. Sci. Technol. 30(8), 2580-2585.
!bs
!     IMPLICIT NONE
!bs
!bs
!bs * variable declaration
!bs
!bs
!bs * inputs
!bs
! dimension of arrays             
      INTEGER blksize
! number of species in CBLK       
      INTEGER nspcsda
! actual number of cells in arrays
      INTEGER numcells
! model layer                     
      INTEGER layer
! # of organic aerosol precursor  
      INTEGER ldrog
! total # of cond. vapors & SOA sp
      INTEGER ncv
! # of anthrop. cond. vapors & SOA
      INTEGER nacv
! model time step in  SECONDS     
      REAL dt
      REAL cblk(blksize,nspcsda) ! main array of variables         
      REAL blkta(blksize) ! Air temperature [ K ]           
      REAL blkprs(blksize) ! Air pressure in [ Pa ]          
      REAL orgaro1rat(blksize)                                       ! rates from aromatics
! anth. organic vapor production  
      REAL orgaro2rat(blksize)                                       ! rates from aromatics
! anth. organic vapor production  
      REAL orgalk1rat(blksize)                                       ! rates from alkanes and others
! anth. organic vapor production  
      REAL orgole1rat(blksize)                                       ! rates from alkenes and others
! anth. organic vapor production  
      REAL orgbio1rat(blksize) ! bio. organic vapor production ra
      REAL orgbio2rat(blksize) ! bio. organic vapor production ra
      REAL orgbio3rat(blksize) ! bio. organic vapor production ra
      REAL orgbio4rat(blksize) ! bio. organic vapor production ra
      REAL drog(blksize,ldrog) !bs
!bs * get some infos
!bs
!bs      INTEGER LL
!bs      IF (LAYER .EQ. 1) THEN
!bs         WRITE(75,4711) (CBLK(1,LL), LL = VORGARO1J, VORGOLE1I)
!bs         WRITE(75,4711) (CBLK(1,LL), LL = VORGBA1J , VORGBA4I )
!bs         WRITE(75,4712) (CBLK(1,LL), LL = VCVARO1, VCVLIM2)
!bs         WRITE(75,4712) (DROG(1,LL), LL =  1,    8)
!bs         WRITE(75,4712) (DROG(1,LL), LL =  9,   16)
!bs         WRITE(75,4714) (DROG(1,LL), LL = 17,LDROG)
!bs      ENDIF
!bs 4711 FORMAT(8(e12.6,1X))
!bs 4712 FORMAT(8(e12.6,1X))
!bs 4713 FORMAT(17(e12.6,1X))
!bs 4714 FORMAT(e12.6,/)
!bs
!bs * begin code
!bs
! ROG production rate [ug m^-3 s^-
      IF (orgaer==1) THEN
!       IF (firstime) THEN
!         WRITE (6,'(a)')
!         WRITE (6,'(a)') 'METHOD OF PANDIS USED FOR SOA FORMATION!'
!         WRITE (6,'(a)')
!         WRITE (90,'(a)')
!         WRITE (90,'(a)') 'METHOD OF PANDIS USED FOR SOA FORMATION!'
!         firstime = .FALSE.
!       END IF
!         CALL SOA_PANDIS(
!     &                   LAYER,
!     &                   BLKTA, BLKPRS,
!     &                   ORGARO1RAT, ORGARO2RAT,
!     &                   ORGALK1RAT, ORGOLE1RAT,
!     &                   ORGBIO1RAT, ORGBIO2RAT,
!     &                   ORGBIO3RAT, ORGBIO4RAT,
!     &                   DROG, LDROG, NCV, NACV,
!     &                   CBLK, BLKSIZE, NSPCSDA, NUMCELLS,
!     &                   DT
!     &                  )
      ELSE IF (orgaer==2) THEN
!       IF (firstime) THEN
!         WRITE (6,'(a)')
!         WRITE (6,'(a)') 'PANKOW/ODUM METHOD USED FOR SOA FORMATION!'
!         WRITE (6,'(a)')
!         WRITE (90,'(a)')
!         WRITE (90,'(a)') 'PANKOW/ODUM METHOD USED FOR SOA FORMATION!'
!         firstime = .FALSE.
!       END IF
        CALL soa_part(layer,blkta,blkprs,orgaro1rat,orgaro2rat,orgalk1rat, &
          orgole1rat,orgbio1rat,orgbio2rat,orgbio3rat,orgbio4rat,drog,ldrog, &
          ncv,nacv,cblk,blksize,nspcsda,numcells,dt)
      ELSE
!       WRITE (6,'(a)')
!       WRITE (6,'(a)') 'WRONG PARAMETER ORGAER !!'
!       WRITE (6,90000) orgaer
!       WRITE (6,'(a)') 'PROGRAM TERMINATED !!'
!       WRITE (6,'(a)')
!       STOP
      END IF
!bs
!bs      ORGARO1RAT(1) = 0.
!bs      ORGARO2RAT(1) = 0.
!bs      ORGALK1RAT(1) = 0.
!bs      ORGOLE1RAT(1) = 0.
!bs      ORGBIO1RAT(1) = 0.
!bs      ORGBIO2RAT(1) = 0.
!bs      ORGBIO3RAT(1) = 0.
!bs      ORGBIO4RAT(1) = 0.
!bs      WRITE(6,'(a)') '!!! ORGRATs SET TO 0. !!!'
!bs
!bs * formats
!bs
90000 FORMAT ('ORGAER = ',I2)
!bs
!bs * end of SR SORGAM
!bs
      RETURN
    END SUBROUTINE sorgam
!****************************************************************
!
!
!
!
! ///////////////////////////////
! *** this routine calculates the dry deposition and sedimentation 
!     velocities for the three modes. 
!     coded 1/23/97 by Dr. Francis S. Binkowski. Follows 
!     FSB's original method, i.e. uses Jon Pleim's expression for deposition
!     velocity but includes Marv Wesely's wstar contribution. 
!ia eliminated Stokes term for coarse mode deposition calcs.,
!ia see comments below
 
       SUBROUTINE VDVG(  BLKSIZE, NSPCSDA, NUMCELLS,           &
                     LAYER,                                    &
                     CBLK,                                     &  
                     BLKTA, BLKDENS, RA, USTAR, WSTAR,  AMU,   &
                     DGNUC, DGACC, DGCOR,                      &
                     KNNUC, KNACC,KNCOR,                       &    
                     PDENSN, PDENSA, PDENSC,                   &                 
                     VSED, VDEP )

! *** calculate size-averaged particle dry deposition and 
!     size-averaged sedimentation velocities.


!     IMPLICIT NONE

      INTEGER BLKSIZE                  ! dimension of arrays
      INTEGER NSPCSDA                  ! number of species in CBLK
      INTEGER NUMCELLS                ! actual number of cells in arrays 
      INTEGER LAYER                   ! number of layer

      REAL CBLK( BLKSIZE, NSPCSDA ) ! main array of variables      
      REAL BLKTA( BLKSIZE )         ! Air temperature [ K ]
      REAL BLKDENS(BLKSIZE) ! Air density  [ kg m^-3 ]      
      REAL RA(BLKSIZE )             ! aerodynamic resistance [ s m**-1 ]
      REAL USTAR( BLKSIZE )         ! surface friction velocity [ m s**-1 ]
      REAL WSTAR( BLKSIZE )         ! convective velocity scale [ m s**-1 ]
      REAL AMU( BLKSIZE )           ! atmospheric dynamic viscosity [ kg m**-1 s**-1 ]
      REAL DGNUC( BLKSIZE )         ! nuclei mode mean diameter [ m ]
      REAL DGACC( BLKSIZE )         ! accumulation  
      REAL DGCOR( BLKSIZE )         ! coarse mode
      REAL KNNUC( BLKSIZE )         ! nuclei mode Knudsen number 
      REAL KNACC( BLKSIZE )         ! accumulation  
      REAL KNCOR( BLKSIZE )         ! coarse mode
      REAL PDENSN( BLKSIZE )        ! average particel density in nuclei mode [ kg / m**3 ]
      REAL PDENSA( BLKSIZE )        ! average particel density in accumulation mode [ kg / m**3 ]
      REAL PDENSC( BLKSIZE )        ! average particel density in coarse mode [ kg / m**3 ]
       

! *** modal particle diffusivities for number and 3rd moment, or mass:

      REAL DCHAT0N( BLKSIZE), DCHAT0A(BLKSIZE), DCHAT0C(BLKSIZE)
      REAL DCHAT3N( BLKSIZE), DCHAT3A(BLKSIZE), DCHAT3C(BLKSIZE)

! *** modal sedimentation velocities for number and 3rd moment, or mass:
      
      REAL VGHAT0N( BLKSIZE), VGHAT0A(BLKSIZE), VGHAT0C(BLKSIZE)
      REAL VGHAT3N( BLKSIZE), VGHAT3A(BLKSIZE), VGHAT3C(BLKSIZE)

! *** deposition and sedimentation velocities

      REAL VDEP( BLKSIZE, NASPCSDEP) ! sedimantation velocity [ m s**-1 ]
      REAL VSED( BLKSIZE, NASPCSSED)  ! deposition  velocity [ m s**-1 ]
      
      
      INTEGER LCELL
      REAL DCONST1, DCONST1N, DCONST1A, DCONST1C
      REAL DCONST2, DCONST3N, DCONST3A,DCONST3C 
      REAL SC0N, SC0A, SC0C ! Schmidt numbers for number 
      REAL SC3N, SC3A, SC3C ! Schmidt numbers for 3rd moment
      REAL ST0N, ST0A, ST0C ! Stokes numbers for number 
      REAL ST3N, ST3A, ST3C ! Stokes numbers for 3rd moment
      REAL RD0N, RD0A, RD0C    ! canopy resistance for number
      REAL RD3N, RD3A, RD3C    ! canopy resisteance for 3rd moment 
      REAL UTSCALE   ! scratch function of USTAR and WSTAR.
      REAL NU        !kinematic viscosity [ m**2 s**-1 ]     
      REAL USTFAC      ! scratch function of USTAR, NU, and GRAV
      REAL BHAT
      PARAMETER( BHAT =  1.246 ) ! Constant from Cunningham slip correction.


! *** check layer value. 

         IF ( LAYER .EQ. 1 ) THEN ! calculate diffusitities and 
!                                    sedimentation velocities         
	        
         DO LCELL = 1, NUMCELLS
         
            DCONST1 = BOLTZ * BLKTA(LCELL) /                                         &
                    ( THREEPI * AMU(LCELL) )
            DCONST1N = DCONST1 / DGNUC( LCELL ) 
            DCONST1A = DCONST1 / DGACC( LCELL )
            DCONST1C = DCONST1 / DGCOR( LCELL )   
            DCONST2 = GRAV / ( 18.0 * AMU(LCELL) )
            DCONST3N = DCONST2 * PDENSN(LCELL) * DGNUC( LCELL )**2
            DCONST3A = DCONST2 * PDENSA(LCELL) * DGACC( LCELL )**2
            DCONST3C = DCONST2 * PDENSC(LCELL) * DGCOR( LCELL )**2

! *** i-mode 
 
            DCHAT0N(LCELL) =  DCONST1N                             &
               * ( ESN04 + BHAT * KNNUC( LCELL ) * ESN16 )
                
            DCHAT3N(LCELL) =  DCONST1N                             &
               * ( ESNM20 + BHAT * KNNUC( LCELL ) * ESNM32 )
            
            VGHAT0N(LCELL) = DCONST3N                             &
               * ( ESN16 + BHAT * KNNUC( LCELL ) * ESN04 )
                
            VGHAT3N(LCELL) = DCONST3N                             &
               * (ESN64 + BHAT * KNNUC( LCELL ) * ESN28 )

! *** j-mode

            DCHAT0A(LCELL) =  DCONST1A                             &
              * ( ESA04 + BHAT * KNACC( LCELL ) * ESA16 )
                
            DCHAT3A(LCELL) =  DCONST1A                             &
               * ( ESAM20 + BHAT * KNACC( LCELL ) * ESAM32 )           
            
            VGHAT0A(LCELL) = DCONST3A                             &
              * ( ESA16 + BHAT * KNACC( LCELL ) * ESA04 )
                
            VGHAT3A(LCELL) = DCONST3A                             &
              * ( ESA64 + BHAT * KNACC( LCELL ) * ESA28 )


! *** coarse mode

            DCHAT0C(LCELL)=  DCONST1C                             &
              * ( ESC04 + BHAT * KNCOR( LCELL ) * ESC16 )
                
            DCHAT3C(LCELL) = DCONST1C                             &
              * ( ESCM20 + BHAT * KNCOR( LCELL ) * ESCM32 )
            
            VGHAT0C(LCELL) = DCONST3C                             &
              * ( ESC16 + BHAT * KNCOR( LCELL ) * ESC04 )
                
            VGHAT3C(LCELL) = DCONST3C                             &
              * ( ESC64 + BHAT * KNCOR( LCELL ) * ESC28 )
        
        END DO
 
! *** now calculate the deposition and sedmentation velocities

!ia  07.05.98 
! *** NOTE In the deposition velocity for coarse mode,
!     the impaction term  10.0 ** (-3.0 / st) is eliminated because
!     coarse particles are likely to bounce on impact and the current
!     formulation does not account for this.


        DO LCELL = 1, NUMCELLS
        
         NU = AMU(LCELL) / BLKDENS(LCELL) 
         USTFAC = USTAR(LCELL) * USTAR(LCELL) / ( GRAV * NU)
         UTSCALE = USTAR(LCELL) +                             &
                 0.24 * WSTAR(LCELL) * WSTAR(LCELL) / USTAR(LCELL)

! *** first do number   
           
! *** nuclei or Aitken mode  ( no sedimentation velocity )      

        SC0N = NU / DCHAT0N(LCELL)      
        ST0N = MAX( VGHAT0N(LCELL) * USTFAC , 0.01)
        RD0N = 1.0 / ( UTSCALE *                             &
                  ( SC0N**(-TWO3) + 10.0**(-3.0 / ST0N) ) ) 
      
        VDEP(LCELL, VDNNUC) = VGHAT0N(LCELL) +                             &
               1.0 / (                             &
           RA(LCELL) + RD0N + RD0N * RA(LCELL) * VGHAT0N(LCELL) )

        VSED( LCELL, VSNNUC) = VGHAT0N(LCELL) 
     
! *** accumulation mode

        SC0A = NU / DCHAT0A(LCELL)      
        ST0A = MAX ( VGHAT0A(LCELL) * USTFAC, 0.01)
        RD0A = 1.0 / ( UTSCALE *                             &
                  ( SC0A**(-TWO3) + 10.0**(-3.0 / ST0A) ) ) 
      
        VDEP(LCELL, VDNACC) = VGHAT0A(LCELL) +                             &
               1.0 / (                             &
           RA(LCELL) + RD0A + RD0A * RA(LCELL) * VGHAT0A(LCELL) ) 

        VSED( LCELL, VSNACC) = VGHAT0A(LCELL) 

! *** coarse mode 

        SC0C = NU / DCHAT0C(LCELL)      
!ia        ST0C = MAX( VGHAT0C(LCELL) * USTFAC, 0.01 )
!ia        RD0C = 1.0 / ( UTSCALE * 
!ia     &            ( SC0C**(-TWO3) + 10.0**(-3.0 / ST0C) ) ) 
 
         RD0C = 1.0 / ( UTSCALE *                            &
                      ( SC0C ** ( -TWO3 )  ) ) ! eliminate impaction term
      
        VDEP(LCELL, VDNCOR) = VGHAT0C(LCELL) +                             &
               1.0 / (                             &
           RA(LCELL) + RD0C + RD0C * RA(LCELL) * VGHAT0C(LCELL) ) 

        VSED( LCELL, VSNCOR) = VGHAT0C(LCELL) 

! *** now do m3 for the deposition of mass 

! *** nuclei or Aitken mode  

        SC3N = NU / DCHAT3N(LCELL)      
        ST3N = MAX( VGHAT3N(LCELL) * USTFAC, 0.01) 
        RD3N = 1.0 / ( UTSCALE *                             &
                  ( SC3N**(-TWO3) + 10.0**(-3.0 / ST3N) ) ) 
      
        VDEP(LCELL, VDMNUC) = VGHAT3N(LCELL) +                             &
               1.0 / (                             &
           RA(LCELL) + RD3N + RD3N * RA(LCELL) * VGHAT3N(LCELL) ) 

        VSED(LCELL, VSMNUC) = VGHAT3N(LCELL)
     
! *** accumulation mode

        SC3A = NU / DCHAT3A(LCELL)      
        ST3A = MAX( VGHAT3A(LCELL) * USTFAC , 0.01 )
        RD3A = 1.0 / ( UTSCALE *                             &
                  ( SC3A**(-TWO3) + 10.0**(-3.0 / ST3A) ) ) 

       VDEP(LCELL, VDMACC) = VGHAT3A(LCELL) +                            &
               1.0 / (                            &
               RA(LCELL) + RD3A + RD3A * RA(LCELL) * VGHAT3A(LCELL) )
                
     
! *** fine mass deposition velocity: combine Aitken and accumulation 
!     mode deposition velocities. Assume density is the same
!     for both modes.


!       VDEP(LCELL,VDMFINE) = ( 
!    &    CBLK(LCELL,VNU3) * VDEP(LCELL, VDMNUC) + 
!    &    CBLK(LCELL,VAC3) * VDEP(LCELL, VDMACC) ) / 
!    &    ( CBLK(LCELL,VAC3) + CBLK(LCELL,VNU3) ) 
     
 
! *** fine mass sedimentation velocity

!       VSED( LCELL, VSMFINE) = (
!    &    CBLK(LCELL, VNU3) * VGHAT3N(LCELL) + 
!    &     CBLK(LCELL, VAC3) * VGHAT3A(LCELL) ) /
!    &    ( CBLK(LCELL, VNU3) + CBLK(LCELL, VAC3)  )     

        VSED( LCELL, VSMACC ) = VGHAT3A(LCELL)

! *** coarse mode 

        SC3C = NU / DCHAT3C(LCELL)
!ia        ST3C = MAX( VGHAT3C(LCELL) * USTFAC, 0.01 )
!ia        RD3C = 1.0 / ( UTSCALE * 
!ia     &            ( SC3C**(-TWO3) + 10.0**(-3.0 / ST3C) ) ) 
   
        RD3C = 1.0 / ( UTSCALE *                            &
                     ( SC3C ** ( -TWO3 ) ) ) ! eliminate impaction term   
        VDEP(LCELL, VDMCOR) = VGHAT3C(LCELL) +                             &
               1.0 / (                             &
           RA(LCELL) + RD3C + RD3C * RA(LCELL) * VGHAT3C(LCELL)) 

! *** coarse mode sedmentation velocity

        VSED( LCELL, VSMCOR) = VGHAT3C(LCELL) 


                                 
        END DO  
             
        ELSE   ! LAYER greater than 1
        
! *** for layer greater than 1 calculate  sedimentation velocities only 

         DO LCELL = 1, NUMCELLS
         
            DCONST2 = GRAV / ( 18.0 * AMU(LCELL) )
            
            DCONST3N = DCONST2 * PDENSN(LCELL) * DGNUC( LCELL )**2
            DCONST3A = DCONST2 * PDENSA(LCELL) * DGACC( LCELL )**2
            DCONST3C = DCONST2 * PDENSC(LCELL) * DGCOR( LCELL )**2

            VGHAT0N(LCELL) = DCONST3N                             &
               * ( ESN16 + BHAT * KNNUC( LCELL ) * ESN04 )
               
! *** nucleation mode number sedimentation velocity

            VSED( LCELL, VSNNUC) = VGHAT0N(LCELL)
 
            VGHAT3N(LCELL) = DCONST3N                             &
               * (ESN64 + BHAT * KNNUC( LCELL ) * ESN28 )

! *** nucleation mode volume sedimentation velocity

	    VSED( LCELL, VSMNUC) = VGHAT3N(LCELL)

            VGHAT0A(LCELL) = DCONST3A                             &
              * ( ESA16 + BHAT * KNACC( LCELL ) * ESA04 )

! *** accumulation mode number sedimentation velocity
     
            VSED( LCELL, VSNACC) = VGHAT0A(LCELL)      
                
            VGHAT3A(LCELL) = DCONST3A                            & 
              * ( ESA64 + BHAT * KNACC( LCELL ) * ESA28 )
     
! *** fine mass sedimentation velocity

!           VSED( LCELL, VSMFINE) = (
!    &       CBLK(LCELL, VNU3) * VGHAT3N(LCELL) + 
!    &        CBLK(LCELL, VAC3) * VGHAT3A(LCELL) ) /
!    &       ( CBLK(LCELL, VNU3) + CBLK(LCELL, VAC3)  )     

            VSED( LCELL, VSMACC) = VGHAT3A(LCELL)     
         
            VGHAT0C(LCELL) = DCONST3C                            & 
              * ( ESC16 + BHAT * KNCOR( LCELL ) * ESC04 )

! *** coarse mode sedimentation velocity
     
            VSED( LCELL, VSNCOR) = VGHAT0C(LCELL) 
       
                
            VGHAT3C(LCELL) = DCONST3C                             &
              * ( ESC64 + BHAT * KNCOR( LCELL ) * ESC28 )

! *** coarse mode mass sedimentation velocity

            VSED( LCELL, VSMCOR) = VGHAT3C(LCELL) 
        
         END DO 
         
         END IF ! check on layer 
         
END SUBROUTINE VDVG
!
! ///////////////////////////////
! *** this routine calculates the dry deposition and sedimentation 
!     velocities for the three modes. 
!   Stu McKeen 10/13/08
!   Gaussian Quadrature numerical integration over diameter range for each mode.
! Quadrature taken from Abramowitz and Stegun (1974), equation 25.4.46 and Table 25.10
! Quadrature points are the zeros of Hermite polynomials of order NGAUSdv
!   Numerical Integration allows more complete discription of the
!   Cunningham Slip correction factor, Interception Term (not included previously),
!   and the correction due to rebound for higher diameter particles.
!   Sedimentation velocities the same as original Binkowski code, also the
!   Schmidt number and Brownian diffusion efficiency dependence on Schmidt number the
!   same as Binkowski.
!   Stokes number, and efficiency dependence on Stokes number now according to
!   Peters and Eiden (1992).  Interception term taken from Slinn (1982) with
!   efficiency at .2 micron diam. (0.3%) tuned to yield .2 cm/s deposition velocitiy
!   for needleaf evergreen trees (Pryor et al., Tellus, 2008). Rebound correction
!   term is that of Slinn (1982)
!
!     Original code 1/23/97 by Dr. Francis S. Binkowski. Follows 
!     FSB's original method, i.e. uses Jon Pleim's expression for deposition
!     velocity but includes Marv Wesely's wstar contribution. 
!ia eliminated Stokes term for coarse mode deposition calcs.,
!ia see comments below

       SUBROUTINE VDVG_2(  BLKSIZE, NSPCSDA, NUMCELLS,           &
                     LAYER,                                    &
                     CBLK, BLKTA, BLKDENS,                     &
                     RA, USTAR, PBLH, ZNTT, RMOLM,  AMU,       &
                     DGNUC, DGACC, DGCOR, XLM,                 &
                     KNNUC, KNACC,KNCOR,                       &
                     PDENSN, PDENSA, PDENSC,                   &
                     VSED, VDEP)

! *** calculate size-averaged particle dry deposition and 
!     size-averaged sedimentation velocities.


!     IMPLICIT NONE

      INTEGER BLKSIZE                  ! dimension of arrays
      INTEGER NSPCSDA                  ! number of species in CBLK
      INTEGER NUMCELLS                ! actual number of cells in arrays 
      INTEGER LAYER                   ! number of layer
      INTEGER, PARAMETER :: iprnt = 0

      REAL CBLK( BLKSIZE, NSPCSDA ) ! main array of variables      
      REAL BLKTA( BLKSIZE )         ! Air temperature [ K ]
      REAL BLKDENS(BLKSIZE) ! Air density  [ kg m^-3 ]      
      REAL RA(BLKSIZE )             ! aerodynamic resistance [ s m**-1 ]
      REAL USTAR( BLKSIZE )         ! surface friction velocity [ m s**-1 ]
      REAL PBLH( BLKSIZE )          ! PBL height (m)
      REAL ZNTT( BLKSIZE )          ! Surface roughness length (m)
      REAL RMOLM( BLKSIZE )         ! Inverse of Monin-Obukhov length (1/m)
      REAL AMU( BLKSIZE )           ! atmospheric dynamic viscosity [ kg m**-1 s**-1 ]
      REAL XLM( BLKSIZE )           ! mean freepath of dry air [ m ]
      REAL DGNUC( BLKSIZE )         ! nuclei mode mean diameter [ m ]
      REAL DGACC( BLKSIZE )         ! accumulation  
      REAL DGCOR( BLKSIZE )         ! coarse mode
      REAL KNNUC( BLKSIZE )         ! nuclei mode Knudsen number 
      REAL KNACC( BLKSIZE )         ! accumulation  
      REAL KNCOR( BLKSIZE )         ! coarse mode
      REAL PDENSN( BLKSIZE )        ! average particel density in nuclei mode [ kg / m**3 ]
      REAL PDENSA( BLKSIZE )        ! average particel density in accumulation mode [ kg / m**3 ]
      REAL PDENSC( BLKSIZE )        ! average particel density in coarse mode [ kg / m**3 ]


! *** deposition and sedimentation velocities

      REAL VDEP( BLKSIZE, NASPCSDEP) ! sedimantation velocity [ m s**-1 ]
      REAL VSED( BLKSIZE, NASPCSSED)  ! deposition  velocity [ m s**-1 ]


      INTEGER LCELL,N
      REAL DCONST1, DCONST2, DCONST3, DCONST3N, DCONST3A,DCONST3C
      REAL UTSCALE,CZH   ! scratch functions of USTAR and WSTAR.
      REAL NU        !kinematic viscosity [ m**2 s**-1 ]     
      REAL BHAT
      PARAMETER( BHAT =  1.246 ) ! Constant from Binkowski-Shankar approx to Cunningham slip correction.
      REAL COLCTR_BIGD,COLCTR_SMALD
      PARAMETER( COLCTR_BIGD=2.E-3,COLCTR_SMALD=20.E-6)  ! Collector diameters in Stokes number and Interception Efficiency (Needleleaf Forest)
      REAL SUM0, SUM3, DQ, KNQ, CUNQ, VSEDQ, SCQ, STQ, RSURFQ, vdplim
      REAL Eff_dif, Eff_imp, Eff_int, RBcor
      INTEGER ISTOPvd0,IdoWesCor
      PARAMETER (ISTOPvd0 = 0)  ! ISTOPvd0 = 1 means dont call VDVG, particle dep. velocities are set = 0; ISTOPvd0 = 0 means do depvel calcs.
      PARAMETER (IdoWesCor = 0) ! IdoWesCor = 1 means do Wesley (85) convective correction to PM dry dep velocities; 0 means dont do correction
      IF (ISTOPvd0.EQ.1)THEN
      RETURN
      ENDIF
! *** check layer value. 

      IF(iprnt.eq.1) print *,'In VDVG, Layer=',LAYER
         IF ( LAYER .EQ. 1 ) THEN ! calculate diffusitities and 
!                                    sedimentation velocities         
                 
         DO LCELL = 1, NUMCELLS     
         
            DCONST1 = BOLTZ * BLKTA(LCELL) /                                         &
                    ( THREEPI * AMU(LCELL) )
            DCONST2 = GRAV / ( 18.0 * AMU(LCELL) )
            DCONST3 =  USTAR(LCELL)/(9.*AMU(LCELL)*COLCTR_BIGD)
 
! *** now calculate the deposition velocities at layer 1

         NU = AMU(LCELL) / BLKDENS(LCELL) 

         UTSCALE =  1.
        IF (IdoWesCor.EQ.1)THEN
! Wesley (1985) Monin-Obukov dependence for convective conditions (SAM 10/08)
         IF(RMOLM(LCELL).LT.0.)THEN
         CZH = -1.*PBLH(LCELL)*RMOLM(LCELL)
         IF(CZH.GT.30.0)THEN
         UTSCALE=0.45*CZH**0.6667
         ELSE
         UTSCALE=1.+(-300.*RMOLM(LCELL))**0.6667
         ENDIF
         ENDIF
        ENDIF   ! end of (IdoWesCor.EQ.1) test
         UTSCALE = USTAR(LCELL)*UTSCALE
      IF(iprnt.eq.1)THEN
      print *,'NGAUSdv,xxlsga,USTAR,UTSCALE'
      print *,NGAUSdv,xxlsga,USTAR(LCELL),UTSCALE
      print *,'DCONST2,PDENSA,DGACC,GRAV,AMU'
      print *,DCONST2,PDENSA(LCELL),DGACC(LCELL),GRAV,AMU(LCELL)
      endif
      
! *** nuclei mode 
      
        SUM0=0.
        SUM3=0.
        DO N=1,NGAUSdv
        DQ=DGNUC(LCELL)*EXP(Y_GQ(N)*sqrt2*xxlsgn)  ! Diameter (m) at quadrature point
        KNQ=2.*XLM(LCELL)/DQ  ! Knudsen number at quadrature point
        CUNQ=1.+KNQ*(1.257+.4*exp(-1.1/KNQ))  ! Cunningham correction factor; Pruppacher and Klett (1980) Eq (12-16)
        VSEDQ=PDENSN(LCELL)*DCONST2*CUNQ*DQ*DQ  ! Gravitational sedimentation velocity m/s
        SCQ=NU*DQ/DCONST1/CUNQ  ! Schmidt number, Brownian diffusion parameter - Same as Binkowski and Shankar
        Eff_dif=SCQ**(-TWO3)    ! Efficiency term for diffusion - Same as Binkowski and Shankar
        STQ=DCONST3*PDENSN(LCELL)*DQ**2  ! Stokes number, Peters and Eiden (1992)
        Eff_imp=(STQ/(0.8+STQ))**2   ! Efficiency term for impaction - Peters and Eiden (1992)
!       Eff_int=0.3*DQ/(COLCTR_SMALD+DQ) ! Slinn (1982) Interception term, 0.3 prefac insures .2 cm/s at .2 micron diam.
        Eff_int=(0.00116+0.0061*ZNTT(LCELL))*DQ/1.414E-7 ! McKeen(2008) Intercptn trm, val of .00421 @ ustr=0.475, diam=.1414 micrn, stable, needleleaf evergreen
        RBcor=1. ! Rebound correction factor
        vdplim=UTSCALE*(Eff_dif+Eff_imp+Eff_int)*RBcor
!       vdplim=.002*UTSCALE
        vdplim=min(vdplim,.02)
        RSURFQ=RA(LCELL)+1./vdplim
!       RSURFQ=RA(LCELL)+1./(UTSCALE*(Eff_dif+Eff_imp+Eff_int)*RBcor) ! Total surface resistence
!
!   limit this here to be consisten with the gocart routine, which bases this on Walcek et al. 1986
!
!       RSURFQ=max(RSURFQ,50.)
        SUM0=SUM0+WGAUS(N)*(VSEDQ + 1./RSURFQ)  ! Quadrature sum for 0 moment
        SUM3=SUM3+WGAUS(N)*(VSEDQ + 1./RSURFQ)*DQ**3  ! Quadrature sum for 3rd moment
        ENDDO
        VDEP(LCELL, VDNNUC) = SUM0/sqrtpi  ! normalize 0 moment vdep quadrature sum to sqrt(pi) (and number =1 per unit volume)
        VDEP(LCELL, VDMNUC) = SUM3/(sqrtpi*EXP((1.5*sqrt2*xxlsgn)**2)*DGNUC(LCELL)**3) !normalize 3 moment quad. sum to sqrt(pi) and 3rd moment analytic sum

! *** accumulation mode

        SUM0=0.
        SUM3=0.
        DO N=1,NGAUSdv
        DQ=DGACC(LCELL)*EXP(Y_GQ(N)*sqrt2*xxlsga)  ! Diameter (m) at quadrature point
        KNQ=2.*XLM(LCELL)/DQ  ! Knudsen number at quadrature point
        CUNQ=1.+KNQ*(1.257+.4*exp(-1.1/KNQ))  ! Cunningham correction factor; Pruppacher and Klett (1980) Eq (12-16)
        VSEDQ=PDENSA(LCELL)*DCONST2*CUNQ*DQ*DQ  ! Gravitational sedimentation velocity m/s
        SCQ=NU*DQ/DCONST1/CUNQ  ! Schmidt number, Brownian diffusion parameter - Same as Binkowski and Shankar
        Eff_dif=SCQ**(-TWO3)    ! Efficiency term for diffusion - Same as Binkowski and Shankar
        STQ=DCONST3*PDENSA(LCELL)*DQ**2  ! Stokes number, Peters and Eiden (1992)
        Eff_imp=(STQ/(0.8+STQ))**2   ! Efficiency term for impaction - Peters and Eiden (1992)
!       Eff_int=0.3*DQ/(COLCTR_SMALD+DQ) ! Slinn (1982) Interception term, 0.3 prefac insures .2 cm/s at .2 micron diam.
        Eff_int=(0.00116+0.0061*ZNTT(LCELL))*DQ/1.414E-7 ! McKeen(2008) Intercptn term, val of .00421 @ ustr=0.475, diam=.1414 micrn, stable, needleleaf evergreen
        RBcor=1. ! Rebound correction factor
        vdplim=UTSCALE*(Eff_dif+Eff_imp+Eff_int)*RBcor
        vdplim=min(vdplim,.02)
        RSURFQ=RA(LCELL)+1./vdplim
!       RSURFQ=RA(LCELL)+1./(UTSCALE*(Eff_dif+Eff_imp+Eff_int)*RBcor) ! Total surface resistence
!
!   limit this here to be consisten with the gocart routine, which bases this on Walcek et al. 1986
!
!       RSURFQ=max(RSURFQ,50.)
        SUM0=SUM0+WGAUS(N)*(VSEDQ + 1./RSURFQ)  ! Quadrature sum for 0 moment
        SUM3=SUM3+WGAUS(N)*(VSEDQ + 1./RSURFQ)*DQ**3  ! Quadrature sum for 3rd moment
      IF(iprnt.eq.1)THEN
      print *,'N,Y_GQ,WGAUS,DQ,KNQ,CUNQ,VSEDQ,SCQ,STQ,RSURFQ'
      print *,N,Y_GQ(N),WGAUS(N),DQ,KNQ,CUNQ,VSEDQ,SCQ,STQ,RSURFQ
      print *,'N,Eff_dif,imp,int,SUM0,SUM3'
      print *,N,Eff_dif,Eff_imp,Eff_int,SUM0,SUM3
      endif
        ENDDO
        VDEP(LCELL, VDNACC) = SUM0/sqrtpi  ! normalize 0 moment vdep quadrature sum to sqrt(pi) (and number =1 per unit volume)
        VDEP(LCELL, VDMACC) = SUM3/(sqrtpi*EXP((1.5*sqrt2*xxlsga)**2)*DGACC(LCELL)**3) !normalize 3 moment quad. sum to sqrt(pi) and 3rd moment analytic sum
        
! *** coarse mode 
        
        SUM0=0.
        SUM3=0.
        DO N=1,NGAUSdv
        DQ=DGCOR(LCELL)*EXP(Y_GQ(N)*sqrt2*xxlsgc)  ! Diameter (m) at quadrature point
        KNQ=2.*XLM(LCELL)/DQ  ! Knudsen number at quadrature point
        CUNQ=1.+KNQ*(1.257+.4*exp(-1.1/KNQ))  ! Cunningham correction factor; Pruppacher and Klett (1980) Eq (12-16)
        VSEDQ=PDENSC(LCELL)*DCONST2*CUNQ*DQ*DQ  ! Gravitational sedimentation velocity m/s
        SCQ=NU*DQ/DCONST1/CUNQ  ! Schmidt number, Brownian diffusion parameter - Same as Binkowski and Shankar
        Eff_dif=SCQ**(-TWO3)    ! Efficiency term for diffusion - Same as Binkowski and Shankar
        STQ=DCONST3*PDENSC(LCELL)*DQ**2  ! Stokes number, Peters and Eiden (1992)
        Eff_imp=(STQ/(0.8+STQ))**2   ! Efficiency term for impaction - Peters and Eiden (1992)
!       Eff_int=0.3*DQ/(COLCTR_SMALD+DQ) ! Slinn (1982) Interception term, 0.3 prefac insures .2 cm/s at .2 micron diam.
        Eff_int=(0.00116+0.0061*ZNTT(LCELL))*DQ/1.414E-7 ! McKeen(2008) Intercptn term, val of .00421 @ ustr=0.475, diam=.1414 micrn, stable, needleleaf evergreen
        EFF_int=min(1.,EFF_int)
        RBcor=exp(-2.0*(STQ**0.5)) ! Rebound correction factor used in Slinn (1982)
        vdplim=UTSCALE*(Eff_dif+Eff_imp+Eff_int)*RBcor
        vdplim=min(vdplim,.02)
        vdplim=max(vdplim,1e-35) !wig: add check since occasionally a lg particle causes overflow of rsurfq
        RSURFQ=RA(LCELL)+1./vdplim
!       RSURFQ=RA(LCELL)+1./(UTSCALE*(Eff_dif+Eff_imp+Eff_int)*RBcor) ! Total surface resistence
!
!   limit this here to be consisten with the gocart routine, which bases this on Walcek et al. 1986
!
!       RSURFQ=max(RSURFQ,50.)
        SUM0=SUM0+WGAUS(N)*(VSEDQ + 1./RSURFQ)  ! Quadrature sum for 0 moment
        SUM3=SUM3+WGAUS(N)*(VSEDQ + 1./RSURFQ)*DQ**3  ! Quadrature sum for 3rd moment
        ENDDO
        VDEP(LCELL, VDNCOR) = SUM0/sqrtpi  ! normalize 0 moment vdep quadrature sum to sqrt(pi) (and number =1 per unit volume)
        VDEP(LCELL, VDMCOR) = SUM3/(sqrtpi*EXP((1.5*sqrt2*xxlsgc)**2)*DGCOR(LCELL)**3) !normalize 3 moment quad. sum to sqrt(pi) and 3rd moment analytic sum
        
        END DO  
             
        ENDIF  ! ENDOF LAYER = 1 test
        
! *** Calculate gravitational sedimentation velocities for all layers - as in Binkowski and Shankar (1995)

         DO LCELL = 1, NUMCELLS
         
            DCONST2 = GRAV / ( 18.0 * AMU(LCELL) )
            DCONST3N = DCONST2 * PDENSN(LCELL) * DGNUC( LCELL )**2
            DCONST3A = DCONST2 * PDENSA(LCELL) * DGACC( LCELL )**2
            DCONST3C = DCONST2 * PDENSC(LCELL) * DGCOR( LCELL )**2
               
! *** nucleation mode number and mass sedimentation velociticies
            VSED( LCELL, VSNNUC) = DCONST3N                         &
               * ( ESN16 + BHAT * KNNUC( LCELL ) * ESN04 )
            VSED( LCELL, VSMNUC) = DCONST3N                         &
               * (ESN64 + BHAT * KNNUC( LCELL ) * ESN28 )
        
! *** accumulation mode number and mass sedimentation velociticies
            VSED( LCELL, VSNACC) = DCONST3A                          &
              * ( ESA16 + BHAT * KNACC( LCELL ) * ESA04 )
            VSED( LCELL, VSMACC) = DCONST3A                          &
              * ( ESA64 + BHAT * KNACC( LCELL ) * ESA28 )

! *** coarse mode number and mass sedimentation velociticies
            VSED( LCELL, VSNCOR) = DCONST3C                          &
              * ( ESC16 + BHAT * KNCOR( LCELL ) * ESC04 )
            VSED( LCELL, VSMCOR) = DCONST3C                          &
              * ( ESC64 + BHAT * KNCOR( LCELL ) * ESC28 )

         END DO


END SUBROUTINE VDVG_2



!
    SUBROUTINE aerosols_sorgam_init(chem,convfac,z_at_w,             &
         pm2_5_dry,pm2_5_water,pm2_5_dry_ec,                         &
         chem_in_opt,aer_ic_opt, is_aerosol,                         &
         ids,ide, jds,jde, kds,kde,                                  &
         ims,ime, jms,jme, kms,kme,                                  &
         its,ite, jts,jte, kts,kte, config_flags                     )

    USE module_configure,only:  grid_config_rec_type
!!! TUCCELLA (BUG, commented the line below)
    !USE module_prep_wetscav_sorgam,only: aerosols_sorgam_init_aercld_ptrs
 
   implicit none
   INTEGER,      INTENT(IN   ) :: chem_in_opt,aer_ic_opt
   INTEGER,      INTENT(IN   ) ::                               &
                                  ids,ide, jds,jde, kds,kde,    &
                                  ims,ime, jms,jme, kms,kme,    &
                                  its,ite, jts,jte, kts,kte
   LOGICAL, INTENT(OUT) :: is_aerosol(num_chem)
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme, num_chem ) ,     &
          INTENT(INOUT   ) ::                                      &
                              chem
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ) ,               &
          INTENT(INOUT      ) ::                                   &
                     pm2_5_dry,pm2_5_water,pm2_5_dry_ec
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ) ,               &
          INTENT(IN      ) ::                                      &
                   convfac
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ) ,               &
          INTENT(IN         ) ::                                   &
                     z_at_w
   TYPE (grid_config_rec_type) , INTENT (in) :: config_flags


     integer i,j,k,l,ii,jj,kk     
     real tempfac,mwso4,zz
!    real,dimension(its:ite,kts:kte,jts:jte) :: convfac
      REAL splitfac
                        !between gas and aerosol phase
      REAL so4vaptoaer
!factor for splitting initial conc. of SO4
!3rd moment i-mode [3rd moment/m^3]
      REAL m3nuc
!3rd MOMENT j-mode [3rd moment/m^3]
      REAL m3acc
!       REAL ESN36
      REAL m3cor
      DATA splitfac/.98/
      DATA so4vaptoaer/.999/


! *** Compute these once and they will all  be saved in COMMON
        xxlsgn = log(sginin)
        xxlsga = log(sginia)
        xxlsgc = log(sginic)

        l2sginin = xxlsgn**2
        l2sginia = xxlsga**2
        l2sginic = xxlsgc**2

        en1 = exp(0.125*l2sginin)
        ea1 = exp(0.125*l2sginia)
        ec1 = exp(0.125*l2sginic)

        esn04 = en1**4
        esa04 = ea1**4
        esc04 = ec1**4

        esn05 = esn04*en1
        esa05 = esa04*ea1

        esn08 = esn04*esn04
        esa08 = esa04*esa04
        esc08 = esc04*esc04

        esn09 = esn04*esn05
        esa09 = esa04*esa05

        esn12 = esn04*esn04*esn04
        esa12 = esa04*esa04*esa04
        esc12 = esc04*esc04*esc04

        esn16 = esn08*esn08
        esa16 = esa08*esa08
        esc16 = esc08*esc08

        esn20 = esn16*esn04
        esa20 = esa16*esa04
        esc20 = esc16*esc04

        esn24 = esn12*esn12
        esa24 = esa12*esa12
        esc24 = esc12*esc12

        esn25 = esn16*esn09
        esa25 = esa16*esa09

        esn28 = esn20*esn08
        esa28 = esa20*esa08
        esc28 = esc20*esc08


        esn32 = esn16*esn16
        esa32 = esa16*esa16
        esc32 = esc16*esc16

        esn36 = esn16*esn20
        esa36 = esa16*esa20
        esc36 = esc16*esc20

        esn49 = esn25*esn20*esn04
        esa49 = esa25*esa20*esa04

        esn52 = esn16*esn36
        esa52 = esa16*esa36

        esn64 = esn32*esn32
        esa64 = esa32*esa32
        esc64 = esc32*esc32

        esn100 = esn36*esn64

        esnm20 = 1.0/esn20
        esam20 = 1.0/esa20
        escm20 = 1.0/esc20

        esnm32 = 1.0/esn32
        esam32 = 1.0/esa32
        escm32 = 1.0/esc32


        xxm3 = 3.0*xxlsgn/ sqrt2
! factor used in error function cal
        nummin_i = facatkn_min*so4fac*aeroconcmin/(dginin**3*esn36)

        nummin_j = facacc_min*so4fac*aeroconcmin/(dginia**3*esa36)

        nummin_c = anthfac*aeroconcmin/(dginic**3*esc36)

! *** Note, DGVEM_I, DGVEM_J, DGVEM_C are for the mass (volume)
!     size distribution , then

!        vol = (p/6) * density * num * (dgemv_xx**3) *
!                            exp(- 4.5 * log( sgem_xx)**2 ) )
!        note minus sign!!

        factnumn = exp(4.5*log(sgem_i)**2)/dgvem_i**3
        factnuma = exp(4.5*log(sgem_j)**2)/dgvem_j**3
        factnumc = exp(4.5*log(sgem_c)**2)/dgvem_c**3
        ccofm = alphsulf*sqrt(pirs*rgasuniv/(2.0*mwh2so4))
        ccofm_org = alphaorg*sqrt(pirs*rgasuniv/(2.0*mworg))
        mwso4=96.03

! initialize pointers used by aerosol-cloud-interaction routines

! TUCCELLA (BUG, now aerosols_sorgam_init_aercld_ptrs is called chemics_init.F !
!                and was moved to module_prep_wetscav_sorgam.F)

        !call aerosols_sorgam_init_aercld_ptrs( &
        !   num_chem, is_aerosol, config_flags )
        

        pm2_5_dry(its:ite, kts:kte-1, jts:jte)    = 0.
        pm2_5_water(its:ite, kts:kte-1, jts:jte)  = 0.
        pm2_5_dry_ec(its:ite, kts:kte-1, jts:jte) = 0.

!SAM 10/08 Add in Gaussian quadrature points and weights - Use 7 points = NGAUSdv

        Y_GQ(1)=-2.651961356835233
        WGAUS(1)=0.0009717812450995
        Y_GQ(2)=-1.673551628767471
        WGAUS(2)=0.05451558281913
        Y_GQ(3)=-0.816287882858965
        WGAUS(3)=0.4256072526101
        Y_GQ(4)=-0.0
        WGAUS(4)=0.8102646175568
        Y_GQ(5)=0.816287882858965
        WGAUS(5)=WGAUS(3)
        Y_GQ(6)=1.673551628767471
        WGAUS(6)=WGAUS(2)
        Y_GQ(7)=2.651961356835233
        WGAUS(7)=WGAUS(1)
!
!
!  IF USING OLD SIMULATION, DO NOT REINITIALIZE!
!
!
        if(chem_in_opt == 1 .OR. config_flags%restart) return
        do l=p_so4aj,num_chem
         chem(ims:ime,kms:kme,jms:jme,l)=epsilc
        enddo
        chem(ims:ime,kms:kme,jms:jme,p_nu0)=1.e8
        chem(ims:ime,kms:kme,jms:jme,p_ac0)=1.e8
        do j=jts,jte
        jj=min(jde-1,j)
        do k=kts,kte-1
        kk=min(kde-1,k)
        do i=its,ite
        ii=min(ide-1,i)

!Option for alternate ic's
        if( aer_ic_opt == AER_IC_DEFAULT ) then
          chem(i,k,j,p_so4aj)=chem(ii,kk,jj,p_sulf)*CONVFAC(ii,kk,jj)*MWSO4*splitfac*so4vaptoaer
          chem(i,k,j,p_so4ai)=chem(ii,kk,jj,p_sulf)*CONVFAC(ii,kk,jj)*MWSO4* &
        (1.-splitfac)*so4vaptoaer
          chem(i,k,j,p_sulf)=chem(ii,kk,jj,p_sulf)*(1.-so4vaptoaer)
          chem(i,k,j,p_nh4aj) = 10.E-05
          chem(i,k,j,p_nh4ai) = 10.E-05
          chem(i,k,j,p_no3aj) = 10.E-05
          chem(i,k,j,p_no3ai) = 10.E-05
          chem(i,k,j,p_naaj)  = 10.E-05
          chem(i,k,j,p_naai)  = 10.E-05
          chem(i,k,j,p_claj)  = 10.E-05
          chem(i,k,j,p_clai)  = 10.E-05
        elseif( aer_ic_opt == AER_IC_PNNL ) then
           zz = (z_at_w(ii,k,jj)+z_at_w(ii,k+1,jj))*0.5
           call sorgam_init_aer_ic_pnnl(   &
                chem, zz, i,k,j, ims,ime,jms,jme,kms,kme )
        else
           call wrf_error_fatal(   &
                "aerosols_sorgam_init: unable to parse aer_ic_opt" )
        end if

!... i-mode
      m3nuc = so4fac*chem(i,k,j,p_so4ai) + nh4fac*chem(i,k,j,p_nh4ai) + &
        no3fac*chem(i,k,j,p_no3ai) +                                    &
        nafac*chem(i,k,j,p_naai) + clfac*chem(i,k,j,p_clai) +           &
        orgfac*chem(i,k,j,p_orgaro1i) + &
        orgfac*chem(i,k,j,p_orgaro2i) + orgfac*chem(i,k,j,p_orgalk1i) + &
        orgfac*chem(i,k,j,p_orgole1i) + orgfac*chem(i,k,j,p_orgba1i) + &
        orgfac*chem(i,k,j,p_orgba2i) + orgfac*chem(i,k,j,p_orgba3i) + &
        orgfac*chem(i,k,j,p_orgba4i) + orgfac*chem(i,k,j,p_orgpai) + &
        anthfac*chem(i,k,j,p_p25i) + anthfac*chem(i,k,j,p_eci)

!... j-mode
      m3acc = so4fac*chem(i,k,j,p_so4aj) + nh4fac*chem(i,k,j,p_nh4aj) + &
        no3fac*chem(i,k,j,p_no3aj) +                                    &
        nafac*chem(i,k,j,p_naaj) + clfac*chem(i,k,j,p_claj) +           & 
        orgfac*chem(i,k,j,p_orgaro1j) + &
        orgfac*chem(i,k,j,p_orgaro2j) + orgfac*chem(i,k,j,p_orgalk1j) + &
        orgfac*chem(i,k,j,p_orgole1j) + orgfac*chem(i,k,j,p_orgba1j) + &
        orgfac*chem(i,k,j,p_orgba2j) + orgfac*chem(i,k,j,p_orgba3j) + &
        orgfac*chem(i,k,j,p_orgba4j) + orgfac*chem(i,k,j,p_orgpaj) + &
        anthfac*chem(i,k,j,p_p25j) + anthfac*chem(i,k,j,p_ecj)

!...c-mode
      m3cor = soilfac*chem(i,k,j,p_soila) + seasfac*chem(i,k,j,p_seas) + &
        anthfac*chem(i,k,j,p_antha)


!...NOW CALCULATE INITIAL NUMBER CONCENTRATION
      chem(i,k,j,p_nu0) = m3nuc/((dginin**3)*esn36)

      chem(i,k,j,p_ac0) = m3acc/((dginia**3)*esa36)
        
      chem(i,k,j,p_corn) = m3cor/((dginic**3)*esc36)

      enddo
      enddo
      enddo


    return
    END SUBROUTINE aerosols_sorgam_init

!****************************************************************
!                                                               *
!   SUBROUTINE TO INITIALIZE AEROSOL VALUES USING THE           *
!   aer_ic_opt == aer_ic_pnnl OPTION.                           *
!                                                               *
!   wig, 21-Apr-2004, original version	                        *
!   rce, 25-apr-2004 - name changes for consistency with        *
!                          new aer_ic constants in Registry     *
!   wig,  7-May-2004, added height dependance                   *
!                                                               *
!   CALLS THE FOLLOWING SUBROUTINES: NONE                       *
!                                                               *
!   CALLED BY                      : aerosols_sorgam_init       *
!                                                               *
!****************************************************************
    SUBROUTINE sorgam_init_aer_ic_pnnl(                  &
         chem, z, i,k,j, ims,ime, jms,jme, kms,kme )

      USE module_configure,only:  num_chem, grid_config_rec_type
      implicit none

      INTEGER,INTENT(IN   ) :: i,k,j,                           &
                               ims,ime, jms,jme, kms,kme
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme, num_chem ),&
           INTENT(INOUT   ) :: chem

      real, intent(in     ) :: z
      real :: mult

!
! Determine height multiplier...
! This should mimic the calculation in sorgam_set_aer_bc_pnnl,
! mosaic_init_wrf_mixrats_opt2, and bdy_chem_value_mosaic
!!$!    Height(m)     Multiplier
!!$!    ---------     ----------
!!$!    <=2000        1.0
!!$!    2000<z<3000   linear transition zone to 0.5
!!$!    3000<z<5000   linear transision zone to 0.25
!!$!    >=3000        0.25
!!$!
!!$! which translates to:
!!$!    2000<z<3000   mult = 1.0 + (z-2000.)*(0.5-1.0)/(3000.-2000.)
!!$!    3000<z<5000   mult = 0.5 + (z-3000.)*(0.25-0.5)/(5000.-3000.)
!!$! or in reduced form:
!!$      if( z <= 2000. ) then
!!$         mult = 1.0
!!$      elseif( z > 2000. &
!!$           .and. z <= 3000. ) then
!!$         mult = 1.0 - 0.0005*(z-2000.)
!!$      elseif( z > 3000. &
!!$           .and. z <= 5000. ) then
!!$         mult = 0.5 - 1.25e-4*(z-3000.)
!!$      else
!!$         mult = 0.25
!!$      end if
! Updated aerosol profile multiplier 1-Apr-2005:
!    Height(m)     Multiplier
!    ---------     ----------
!    <=2000        1.0
!    2000<z<3000   linear transition zone to 0.25
!    3000<z<5000   linear transision zone to 0.125
!    >=5000        0.125
!
! which translates to:
!    2000<z<3000   mult = 1.00 + (z-2000.)*(0.25-1.0)/(3000.-2000.)
!    3000<z<5000   mult = 0.25 + (z-3000.)*(0.125-0.25)/(5000.-3000.)
! or in reduced form:
!jdf comment these values and have another profile consistent with mosaic
!       if( z <= 2000. ) then
!          mult = 1.0
!       elseif( z > 2000. &
!            .and. z <= 3000. ) then
!          mult = 1.0 - 0.00075*(z-2000.)
!       elseif( z > 3000. &
!            .and. z <= 5000. ) then
!          mult = 0.25 - 4.166666667e-5*(z-3000.)
!       else
!          mult = 0.125
!       end if
        if( z <= 500. ) then
           mult = 1.0
        elseif( z > 500. &
             .and. z <= 1000. ) then
           mult = 1.0 - 0.001074*(z-500.)
        elseif( z > 1000. &
             .and. z <= 5000. ) then
           mult = 0.463 - 0.000111*(z-1000.)
        else
           mult = 0.019
        end if

! These should match what is in sorgam_set_aer_bc_pnnl.
! Values as of 2-Dec-2004:
!jdf comment these values and have another profile consistent with mosaic
!     chem(i,k,j,p_sulf)     = mult*conmin
!     chem(i,k,j,p_so4aj)    = mult*2.375
!     chem(i,k,j,p_so4ai)    = mult*0.179
!     chem(i,k,j,p_nh4aj)    = mult*0.9604
!     chem(i,k,j,p_nh4ai)    = mult*0.0196
!     chem(i,k,j,p_no3aj)    = mult*0.0650
!     chem(i,k,j,p_no3ai)    = mult*0.0050
!     chem(i,k,j,p_ecj)      = mult*0.1630
!     chem(i,k,j,p_eci)      = mult*0.0120
!     chem(i,k,j,p_p25j)     = mult*0.6350
!     chem(i,k,j,p_p25i)     = mult*0.0490
!     chem(i,k,j,p_antha)    = mult*2.2970
!     chem(i,k,j,p_orgpaj)   = mult*0.9300
!     chem(i,k,j,p_orgpai)   = mult*0.0700
!     chem(i,k,j,p_orgaro1j) = conmin
!     chem(i,k,j,p_orgaro1i) = conmin
!     chem(i,k,j,p_orgaro2j) = conmin
!     chem(i,k,j,p_orgaro2i) = conmin
!     chem(i,k,j,p_orgalk1j) = conmin
!     chem(i,k,j,p_orgalk1i) = conmin
!     chem(i,k,j,p_orgole1j) = conmin
!     chem(i,k,j,p_orgole1i) = conmin
!     chem(i,k,j,p_orgba1j)  = conmin
!     chem(i,k,j,p_orgba1i)  = conmin
!     chem(i,k,j,p_orgba2j)  = conmin
!     chem(i,k,j,p_orgba2i)  = conmin
!     chem(i,k,j,p_orgba3j)  = conmin
!     chem(i,k,j,p_orgba3i)  = conmin
!     chem(i,k,j,p_orgba4j)  = conmin
!     chem(i,k,j,p_orgba4i)  = conmin
!     chem(i,k,j,p_seas)     = mult*0.229
      chem(i,k,j,p_sulf)     = mult*conmin
      chem(i,k,j,p_so4aj)    = mult*0.300*0.97
      chem(i,k,j,p_so4ai)    = mult*0.300*0.03
      chem(i,k,j,p_nh4aj)    = mult*0.094*0.97
      chem(i,k,j,p_nh4ai)    = mult*0.094*0.03
      chem(i,k,j,p_no3aj)    = mult*0.001*0.97
      chem(i,k,j,p_no3ai)    = mult*0.001*0.03
      chem(i,k,j,p_naaj)     = 10.E-05
      chem(i,k,j,p_naai)     = 10.E-05
      chem(i,k,j,p_claj)     = 10.E-05
      chem(i,k,j,p_clai)     = 10.E-05
      chem(i,k,j,p_ecj)      = mult*0.013*0.97
      chem(i,k,j,p_eci)      = mult*0.013*0.03
      chem(i,k,j,p_p25j)     = mult*4.500*0.97
      chem(i,k,j,p_p25i)     = mult*4.500*0.03
      chem(i,k,j,p_antha)    = mult*4.500/2.0
      chem(i,k,j,p_orgpaj)   = mult*0.088*0.97
      chem(i,k,j,p_orgpai)   = mult*0.088*0.03
      chem(i,k,j,p_orgaro1j) = conmin
      chem(i,k,j,p_orgaro1i) = conmin
      chem(i,k,j,p_orgaro2j) = conmin
      chem(i,k,j,p_orgaro2i) = conmin
      chem(i,k,j,p_orgalk1j) = conmin
      chem(i,k,j,p_orgalk1i) = conmin
      chem(i,k,j,p_orgole1j) = conmin
      chem(i,k,j,p_orgole1i) = conmin
      chem(i,k,j,p_orgba1j)  = conmin
      chem(i,k,j,p_orgba1i)  = conmin
      chem(i,k,j,p_orgba2j)  = conmin
      chem(i,k,j,p_orgba2i)  = conmin
      chem(i,k,j,p_orgba3j)  = conmin
      chem(i,k,j,p_orgba3i)  = conmin
      chem(i,k,j,p_orgba4j)  = conmin
      chem(i,k,j,p_orgba4i)  = conmin
      chem(i,k,j,p_seas)     = mult*1.75


    END SUBROUTINE sorgam_init_aer_ic_pnnl

!------------------------------------------------------------------------

SUBROUTINE sorgam_addemiss(                                             &
     id, dtstep, u10, v10, alt, dz8w, xland, chem,                      &
     ebu,                                   &
     slai,ust,smois,ivgtyp,isltyp,                                      &
     emis_ant,dust_emiss_active,                                        &
     seasalt_emiss_active,kemit,biom,num_soil_layers,emissopt,          &
     dust_opt,ktau,p8w,u_phy,v_phy,rho_phy,g,dx,erod,                   &
     ids,ide, jds,jde, kds,kde,                                         &
     ims,ime, jms,jme, kms,kme,                                         &
     its,ite, jts,jte, kts,kte                                          )
!
! Routine to apply aerosol emissions for MADE/SORGAM...
! William.Gustafson@pnl.gov; 3-May-2007
! Modified by
! steven.peckham@noaa.gov;   8-Jan-2008
!------------------------------------------------------------------------

  USE module_state_description, only:  num_chem

  INTEGER,      INTENT(IN   ) :: seasalt_emiss_active, kemit,emissopt,  &
                                 dust_emiss_active,num_soil_layers,id,  &
                                 ktau,dust_opt,                         &
                                 biom,ids,ide, jds,jde, kds,kde,        &
                                 ims,ime, jms,jme, kms,kme,             &
                                 its,ite, jts,jte, kts,kte

  REAL, INTENT(IN   ) ::    dtstep

! trace species mixing ratios (aerosol mass = ug/kg-air; number = #/kg-air)
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),               &
       INTENT(INOUT ) ::   chem
!
! aerosol emissions arrays ((ug/m3)*m/s)
!
   REAL, DIMENSION( ims:ime, kms:kemit, jms:jme,num_emis_ant ),        &
         INTENT(IN    ) ::                                             &
         emis_ant
!
! biomass burning aerosol emissions arrays ((ug/m3)*m/s)
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme,num_ebu ),             &
         INTENT(IN    ) ::                                             &
         ebu

! 1/(dry air density) and layer thickness (m)
  REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),                      &
       INTENT(IN   ) ::                                                 &
       alt, dz8w

! add for gocart dust
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                        &
         INTENT(IN    ) :: p8w,u_phy,v_phy,rho_phy 
   REAL, INTENT(IN    ) :: dx, g
   REAL, DIMENSION( ims:ime, jms:jme, 3 ),                              &
         INTENT(IN    ) :: erod
!

  REAL,  DIMENSION( ims:ime , jms:jme ),                                &
       INTENT(IN   ) ::                                                 &
       u10, v10, xland, slai, ust
  INTEGER,  DIMENSION( ims:ime , jms:jme ),                             &
       INTENT(IN   ) ::   ivgtyp, isltyp
  REAL, DIMENSION( ims:ime, num_soil_layers, jms:jme ),    &
       INTENT(INOUT) ::   smois

! Local variables...
  real, dimension(its:ite,kts:kte,jts:jte) :: factor
!
! Get the emissions unit conversion factor including the time step.
! Changes emissions from [ug/m3 m/s] to [ug/kg_dryair/timestep]
!
  factor(its:ite,kts:kte,jts:jte) = alt(its:ite,kts:kte,jts:jte)*dtstep/ &
                  dz8w(its:ite,kts:kte,jts:jte)
!
! Increment the aerosol numbers...
 if (emissopt .ne. 5) then
!
! Aitken mode first...

  chem(its:ite,kts:kemit,jts:jte,p_nu0) =                        &
       chem(its:ite,kts:kemit,jts:jte,p_nu0) +                   &
       factor(its:ite,kts:kemit,jts:jte)*factnumn*(              &
       anthfac*( emis_ant(its:ite,kts:kemit,jts:jte,p_e_pm25i) + &
       emis_ant(its:ite,kts:kemit,jts:jte,p_e_eci)  +            &
       emis_ant(its:ite,kts:kemit,jts:jte,p_e_naai) ) +          &
       orgfac*emis_ant(its:ite,kts:kemit,jts:jte,p_e_orgi) +     &
       so4fac*emis_ant(its:ite,kts:kemit,jts:jte,p_e_so4i)   +   &
       no3fac*emis_ant(its:ite,kts:kemit,jts:jte,p_e_no3i) )

! Accumulation mode next...
  
  chem(its:ite,kts:kemit,jts:jte,p_ac0) =                        &
       chem(its:ite,kts:kemit,jts:jte,p_ac0) +                   &
       factor(its:ite,kts:kemit,jts:jte)*factnuma*(              &
       anthfac*( emis_ant(its:ite,kts:kemit,jts:jte,p_e_pm25j) + &
       emis_ant(its:ite,kts:kemit,jts:jte,p_e_ecj)  +            &
       emis_ant(its:ite,kts:kemit,jts:jte,p_e_naaj) ) +          &
       orgfac*emis_ant(its:ite,kts:kemit,jts:jte,p_e_orgj) +     &
       so4fac*emis_ant(its:ite,kts:kemit,jts:jte,p_e_so4j)   +   &
       no3fac*emis_ant(its:ite,kts:kemit,jts:jte,p_e_no3j))

! And now the coarse mode...

  chem(its:ite,kts:kemit,jts:jte,p_corn) =                       &
       chem(its:ite,kts:kemit,jts:jte,p_corn) +                  &
       factor(its:ite,kts:kemit,jts:jte)*factnumc*anthfac*                           &
       emis_ant(its:ite,kts:kemit,jts:jte,p_e_pm_10)
!
! Increment the aerosol masses...
!
  chem(its:ite,kts:kemit,jts:jte,p_antha) =                      &
       chem(its:ite,kts:kemit,jts:jte,p_antha) +                 &
       emis_ant(its:ite,kts:kemit,jts:jte,p_e_pm_10)*factor(its:ite,kts:kemit,jts:jte)

  chem(its:ite,kts:kemit,jts:jte,p_p25j) =                       &
       chem(its:ite,kts:kemit,jts:jte,p_p25j) +                  &
       emis_ant(its:ite,kts:kemit,jts:jte,p_e_pm25j)*factor(its:ite,kts:kemit,jts:jte)

  chem(its:ite,kts:kemit,jts:jte,p_p25i) =                       &
       chem(its:ite,kts:kemit,jts:jte,p_p25i) +                  &
       emis_ant(its:ite,kts:kemit,jts:jte,p_e_pm25i)*factor(its:ite,kts:kemit,jts:jte)

  chem(its:ite,kts:kemit,jts:jte,p_ecj) =                        &
       chem(its:ite,kts:kemit,jts:jte,p_ecj) +                   &
       emis_ant(its:ite,kts:kemit,jts:jte,p_e_ecj)*factor(its:ite,kts:kemit,jts:jte)

  chem(its:ite,kts:kemit,jts:jte,p_eci) =                        &
       chem(its:ite,kts:kemit,jts:jte,p_eci) +                   &
       emis_ant(its:ite,kts:kemit,jts:jte,p_e_eci)*factor(its:ite,kts:kemit,jts:jte)
  chem(its:ite,kts:kemit,jts:jte,p_naaj) =                        &
       chem(its:ite,kts:kemit,jts:jte,p_naaj) +                   &
       emis_ant(its:ite,kts:kemit,jts:jte,p_e_naaj)*factor(its:ite,kts:kemit,jts:jte)
  chem(its:ite,kts:kemit,jts:jte,p_naai) =                        &
       chem(its:ite,kts:kemit,jts:jte,p_naai) +                   &
       emis_ant(its:ite,kts:kemit,jts:jte,p_e_naai)*factor(its:ite,kts:kemit,jts:jte)

  chem(its:ite,kts:kemit,jts:jte,p_orgpaj) =                     &
       chem(its:ite,kts:kemit,jts:jte,p_orgpaj) +                &
       emis_ant(its:ite,kts:kemit,jts:jte,p_e_orgj)*factor(its:ite,kts:kemit,jts:jte)

  chem(its:ite,kts:kemit,jts:jte,p_orgpai) =                     &
       chem(its:ite,kts:kemit,jts:jte,p_orgpai) +                &
       emis_ant(its:ite,kts:kemit,jts:jte,p_e_orgi)*factor(its:ite,kts:kemit,jts:jte)

  chem(its:ite,kts:kemit,jts:jte,p_so4aj) =                      &
       chem(its:ite,kts:kemit,jts:jte,p_so4aj) +                 &
       emis_ant(its:ite,kts:kemit,jts:jte,p_e_so4j)*factor(its:ite,kts:kemit,jts:jte)

  chem(its:ite,kts:kemit,jts:jte,p_so4ai) =                      &
       chem(its:ite,kts:kemit,jts:jte,p_so4ai) +                 &
       emis_ant(its:ite,kts:kemit,jts:jte,p_e_so4i)*factor(its:ite,kts:kemit,jts:jte)

  chem(its:ite,kts:kemit,jts:jte,p_no3aj) =                      &
       chem(its:ite,kts:kemit,jts:jte,p_no3aj) +                 &
       emis_ant(its:ite,kts:kemit,jts:jte,p_e_no3j)*factor(its:ite,kts:kemit,jts:jte)

  chem(its:ite,kts:kemit,jts:jte,p_no3ai) =                      &
       chem(its:ite,kts:kemit,jts:jte,p_no3ai) +                 &
       emis_ant(its:ite,kts:kemit,jts:jte,p_e_no3i)*factor(its:ite,kts:kemit,jts:jte)
  elseif(emissopt == 5)then
!
! Aitken mode first...

  chem(its:ite,kts:kemit,jts:jte,p_nu0) =                        &
       chem(its:ite,kts:kemit,jts:jte,p_nu0) +                   &
       factor(its:ite,kts:kemit,jts:jte)*factnumn*(                                  &
       anthfac*( .25*emis_ant(its:ite,kts:kemit,jts:jte,p_e_bc) ) +                      &
       orgfac*.25*emis_ant(its:ite,kts:kemit,jts:jte,p_e_oc) )

! Accumulation mode next...
  
  chem(its:ite,kts:kemit,jts:jte,p_ac0) =                        &
       chem(its:ite,kts:kemit,jts:jte,p_ac0) +                   &
       factor(its:ite,kts:kemit,jts:jte)*factnuma*(                                  &
       anthfac*( .75*emis_ant(its:ite,kts:kemit,jts:jte,p_e_bc) ) +                      &
       orgfac*.75*emis_ant(its:ite,kts:kemit,jts:jte,p_e_oc) )

!
! Increment the aerosol masses...
!

  chem(its:ite,kts:kemit,jts:jte,p_ecj) =                        &
       chem(its:ite,kts:kemit,jts:jte,p_ecj) +                   &
       .75*emis_ant(its:ite,kts:kemit,jts:jte,p_e_bc)*factor(its:ite,kts:kemit,jts:jte)

  chem(its:ite,kts:kemit,jts:jte,p_eci) =                        &
       chem(its:ite,kts:kemit,jts:jte,p_eci) +                   &
       .25*emis_ant(its:ite,kts:kemit,jts:jte,p_e_bc)*factor(its:ite,kts:kemit,jts:jte)

  chem(its:ite,kts:kemit,jts:jte,p_orgpaj) =                     &
       chem(its:ite,kts:kemit,jts:jte,p_orgpaj) +                &
       .75*emis_ant(its:ite,kts:kemit,jts:jte,p_e_oc)*factor(its:ite,kts:kemit,jts:jte)

  chem(its:ite,kts:kemit,jts:jte,p_orgpai) =                     &
       chem(its:ite,kts:kemit,jts:jte,p_orgpai) +                &
       .25*emis_ant(its:ite,kts:kemit,jts:jte,p_e_oc)*factor(its:ite,kts:kemit,jts:jte)

  endif
! add biomass burning emissions if present
!
  if(biom == 1 )then
!
! Aitken mode first...

  chem(its:ite,kts:kte,jts:jte,p_nu0) =                        &
       chem(its:ite,kts:kte,jts:jte,p_nu0) +                   &
       factor(its:ite,kts:kte,jts:jte)*factnumn*(              &
       anthfac*( .25*ebu(its:ite,kts:kte,jts:jte,p_ebu_pm25) +       &
              .25*ebu(its:ite,kts:kte,jts:jte,p_ebu_bc) ) +          &
       orgfac*.25*ebu(its:ite,kts:kte,jts:jte,p_ebu_oc) )

! Accumulation mode next...
  
  chem(its:ite,kts:kte,jts:jte,p_ac0) =                        &
       chem(its:ite,kts:kte,jts:jte,p_ac0) +                   &
       factor(its:ite,kts:kte,jts:jte)*factnuma*(              &
       anthfac*(.75*ebu(its:ite,kts:kte,jts:jte,p_ebu_pm25) +        &
      .75*ebu(its:ite,kts:kte,jts:jte,p_ebu_bc) ) +                  &
       orgfac*.75*ebu(its:ite,kts:kte,jts:jte,p_ebu_oc) )
! coarse
  chem(its:ite,kts:kte,jts:jte,p_corn) =                     &
       chem(its:ite,kts:kte,jts:jte,p_corn) +                  &
       factor(its:ite,kts:kte,jts:jte)*factnumc*anthfac*       &
       ebu(its:ite,kts:kte,jts:jte,p_ebu_pm10)

!
! Increment the aerosol masses...
!

  chem(its:ite,kts:kte,jts:jte,p_ecj) =                        &
       chem(its:ite,kts:kte,jts:jte,p_ecj) +                   &
       .75*ebu(its:ite,kts:kte,jts:jte,p_ebu_bc)*factor(its:ite,kts:kte,jts:jte)

  chem(its:ite,kts:kte,jts:jte,p_eci) =                        &
       chem(its:ite,kts:kte,jts:jte,p_eci) +                   &
       .25*ebu(its:ite,kts:kte,jts:jte,p_ebu_bc)*factor(its:ite,kts:kte,jts:jte)

  chem(its:ite,kts:kte,jts:jte,p_orgpaj) =                     &
       chem(its:ite,kts:kte,jts:jte,p_orgpaj) +                &
       .75*ebu(its:ite,kts:kte,jts:jte,p_ebu_oc)*factor(its:ite,kts:kte,jts:jte)

  chem(its:ite,kts:kte,jts:jte,p_orgpai) =                     &
       chem(its:ite,kts:kte,jts:jte,p_orgpai) +                &
       .25*ebu(its:ite,kts:kte,jts:jte,p_ebu_oc)*factor(its:ite,kts:kte,jts:jte)

  chem(its:ite,kts:kte,jts:jte,p_antha) =                      &
       chem(its:ite,kts:kte,jts:jte,p_antha) +                 &
       ebu(its:ite,kts:kte,jts:jte,p_ebu_pm10)*factor(its:ite,kts:kte,jts:jte)

  chem(its:ite,kts:kte,jts:jte,p_p25j) =                       &
       chem(its:ite,kts:kte,jts:jte,p_p25j) +                  &
       .75*ebu(its:ite,kts:kte,jts:jte,p_ebu_pm25)*factor(its:ite,kts:kte,jts:jte)

  chem(its:ite,kts:kte,jts:jte,p_p25i) =                       &
       chem(its:ite,kts:kte,jts:jte,p_p25i) +                  &
       .25*ebu(its:ite,kts:kte,jts:jte,p_ebu_pm25)*factor(its:ite,kts:kte,jts:jte)

   endif !end biomass burning
!
! Get the sea salt emissions...
!
  if( seasalt_emiss_active == 1 ) then
     call sorgam_seasalt_emiss(                                  &
          dtstep, u10, v10, alt, dz8w, xland, chem,              &
          ids,ide, jds,jde, kds,kde,                             &
          ims,ime, jms,jme, kms,kme,                             &
          its,ite, jts,jte, kts,kte                              )
  end if
  if( seasalt_emiss_active == 2 ) then
!     call Monahan_seasalt_emiss(                                 &
!          dtstep, u10, v10, alt, dz8w, xland, chem,              &
!          ids,ide, jds,jde, kds,kde,                             &
!          ims,ime, jms,jme, kms,kme,                             &
!          its,ite, jts,jte, kts,kte                              )
  end if
  if( dust_opt == 2 ) then
   !czhao+++++++++++++++++++++++++++
   call wrf_message("WARNING: You are calling DUSTRAN dust emission scheme with MOSAIC, which is highly experimental and not recommended for use. Please use dust_opt==13")
   !czhao---------------------------

      call sorgam_dust_emiss(                                     &
           slai, ust, smois, ivgtyp, isltyp,                      &
           id, dtstep, u10, v10, alt, dz8w,                       &
           xland, num_soil_layers, chem,                          &
           ids,ide, jds,jde, kds,kde,                             &
           ims,ime, jms,jme, kms,kme,                             &
           its,ite, jts,jte, kts,kte                              )
  end if
  !czhao ++++++++++++++++++++++++++
!     dust_opt changed to 13 since it conflicts with gocart/afwa
  if( dust_opt == 13 ) then
  !czhao -------------------------- 
      call sorgam_dust_gocartemis(                                &
           ktau,dtstep,num_soil_layers,alt,u_phy,                 &
           v_phy,chem,rho_phy,dz8w,smois,u10,v10,p8w,erod,        &
           ivgtyp,isltyp,xland,dx,g,                              &
           ids,ide, jds,jde, kds,kde,                             &
           ims,ime, jms,jme, kms,kme,                             &
           its,ite, jts,jte, kts,kte                              )
  end if

END SUBROUTINE sorgam_addemiss

!------------------------------------------------------------------------
SUBROUTINE sorgam_seasalt_emiss(                                        &
     dtstep, u10, v10, alt, dz8w, xland, chem,                          &
     ids,ide, jds,jde, kds,kde,                                         &
     ims,ime, jms,jme, kms,kme,                                         &
     its,ite, jts,jte, kts,kte                                          )
!
! Routine to calculate seasalt emissions for SORGAM over the time
! dtstep...
! William.Gustafson@pnl.gov; 10-May-2007
!------------------------------------------------------------------------

   USE module_mosaic_addemiss, only:    seasalt_emitfactors_1bin

   IMPLICIT NONE

   INTEGER,      INTENT(IN   ) :: ids,ide, jds,jde, kds,kde,            &
                                  ims,ime, jms,jme, kms,kme,            &
                                  its,ite, jts,jte, kts,kte

   REAL, INTENT(IN   ) ::    dtstep

! 10-m wind speed components (m/s)
   REAL,  DIMENSION( ims:ime , jms:jme ),                               &
          INTENT(IN   ) ::   u10, v10, xland

! trace species mixing ratios (aerosol mass = ug/kg; number = #/kg)
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),              &
         INTENT(INOUT ) ::   chem

! alt  = 1.0/(dry air density) in (m3/kg)
! dz8w = layer thickness in (m)
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),                     &
         INTENT(IN   ) ::   alt, dz8w

! local variables
   integer :: i, j, k, l, l_na, l_cl, n
    integer :: p1st

    real :: dum, dumdlo, dumdhi, dumoceanfrac, dumspd10
    real :: factaa, factbb, fraccl, fracna

    real :: ssemfact_numb_i, ssemfact_numb_j, ssemfact_numb_c
    real :: ssemfact_mass_i, ssemfact_mass_j, ssemfact_mass_c


!   Compute emissions factors for the Aitken mode...
!   Nope, we won't because the parameterization is only valid down to
!   0.1 microns.
!   Setup in units of cm.
!    dumdlo = 0.039e-4
!    dumdhi = 0.078e-4
    ssemfact_numb_i = 0.
    ssemfact_mass_i = 0.

!   Compute emissions factors for the accumulation mode...
!   Potentially, we could go down to 0.078 microns to match the bin
!   boundary for MOSAIC, but MOSAIC is capped at 0.1 too. The upper end
!   has been chosen to match the MOSAIC bin boundary closest to two
!   standard deviations from the default bin mean diameter for the coarse
!   mode.
    dumdlo = 0.1e-4
    dumdhi = 1.250e-4
    call seasalt_emitfactors_1bin( 1, dumdlo, dumdhi,   &
         ssemfact_numb_j, dum, ssemfact_mass_j )

!   Compute emissions factors for the coarse mode...
    dumdlo = 1.25e-4
    dumdhi = 10.0e-4
    call seasalt_emitfactors_1bin( 1, dumdlo, dumdhi,   &
         ssemfact_numb_c, dum, ssemfact_mass_c )

!   Convert mass emissions factor from (g/m2/s) to (ug/m2/s)
    ssemfact_mass_i = ssemfact_mass_i*1.0e6
    ssemfact_mass_j = ssemfact_mass_j*1.0e6
    ssemfact_mass_c = ssemfact_mass_c*1.0e6

!   Loop over i,j and apply seasalt emissions
    k = kts
    do j = jts, jte
    do i = its, ite

    !Skip this point if over land. xland=1 for land and 2 for water.
    !Also, there is no way to differentiate fresh from salt water.
    !Currently, this assumes all water is salty.
       if( xland(i,j) < 1.5 ) cycle

    !wig: As far as I can tell, only real.exe knows the fractional breakdown
    !     of land use. So, in wrf.exe, dumoceanfrac will always be 1.
       dumoceanfrac = 1. !fraction of grid i,j that is salt water
       dumspd10 = dumoceanfrac* &
            ( (u10(i,j)*u10(i,j) + v10(i,j)*v10(i,j))**(0.5*3.41) )

!   factaa is (s*m2/kg-air)
!   factaa*ssemfact_mass(n) is (s*m2/kg-air)*(ug/m2/s) = ug/kg-air
!   factaa*ssemfact_numb(n) is (s*m2/kg-air)*( #/m2/s) =  #/kg-air
       factaa = (dtstep/dz8w(i,k,j))*alt(i,k,j)
       factbb = factaa * dumspd10

!   Apportion seasalt mass emissions assumming that seasalt is pure NaCl
       fracna = mw_na_aer / (mw_na_aer + mw_cl_aer)
       fraccl = 1.0 - fracna

!   Add the emissions into the chem array...
       chem(i,k,j,p_naai) = chem(i,k,j,p_naai) +   &
                            factbb * ssemfact_mass_i * fracna
       chem(i,k,j,p_clai) = chem(i,k,j,p_clai) +   &
                            factbb * ssemfact_mass_i * fraccl
       chem(i,k,j,p_nu0)  = chem(i,k,j,p_nu0) +   &
                            factbb * ssemfact_numb_i

       chem(i,k,j,p_naaj) = chem(i,k,j,p_naaj) +   &
                            factbb * ssemfact_mass_j * fracna
       chem(i,k,j,p_claj) = chem(i,k,j,p_claj) +   &
                            factbb * ssemfact_mass_j * fraccl
       chem(i,k,j,p_ac0)  = chem(i,k,j,p_ac0) +   &
                            factbb * ssemfact_numb_j

       chem(i,k,j,p_seas) = chem(i,k,j,p_seas) +   &
                            factbb * ssemfact_mass_c
       chem(i,k,j,p_corn) = chem(i,k,j,p_corn) +   &
                            factbb * ssemfact_numb_c
    end do !i
    end do !j
END SUBROUTINE sorgam_seasalt_emiss
!----------------------------------------------------------------------

   subroutine sorgam_dust_emiss(  slai,ust, smois, ivgtyp, isltyp,         &
               id, dtstep, u10, v10, alt, dz8w, xland, num_soil_layers,    &
               chem,                                                       &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )
!
! adds dust emissions for mosaic aerosol species (i.e. emission tendencies
! over time dtstep are applied to the aerosol mixing ratios)
!
! This is a simple dust scheme based on Shaw et al. (2008) to appear in
! Atmospheric Environment, recoded by Jerome Fast
!
! NOTE: 
! 1) This version only works with the 8-bin version of MOSAIC.
! 2) Dust added to MOSAIC's other inorganic specie, OIN.  If Ca and CO3 are 
!    activated in the Registry, a small fraction also added to Ca and CO3.
! 3) The main departure from Shaw et al., is now alphamask is computed since
!    the land-use categories in that paper and in WRF differ.  WRF currently 
!    does not have that many land-use categories and adhoc assumptions had to
!    be made. This version was tested for Mexico in the dry season.  The main
!    land-use categories in WRF that are likely dust sources are grass, shrub,
!    and savannna (that WRF has in the desert regions of NW Mexico).  Having
!    dust emitted from these types for other locations and other times of the
!    year is not likely to be valid.
! 4) An upper bound on ustar was placed because the surface parameterizations
!    in WRF can produce unrealistically high values that lead to very high
!    dust emission rates.
! 5) Other departures' from Shaw et al. noted below, but are probably not as
!    important as 2) and 3).
!

   USE module_configure, only:  grid_config_rec_type
   USE module_state_description, only:  num_chem, param_first_scalar
   USE module_data_mosaic_asect

   IMPLICIT NONE

!  TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) :: id,num_soil_layers,                      &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte

   REAL, INTENT(IN   ) ::    dtstep

! 10-m wind speed components (m/s)
   REAL,  DIMENSION( ims:ime , jms:jme ),                                  &
          INTENT(IN   ) ::   u10, v10, xland, slai, ust
   INTEGER,  DIMENSION( ims:ime , jms:jme ),                               &
          INTENT(IN   ) ::   ivgtyp, isltyp

! trace species mixing ratios (aerosol mass = ug/kg; number = #/kg)
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::   chem

! alt  = 1.0/(dry air density) in (m3/kg)
! dz8w = layer thickness in (m)
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::   alt, dz8w

   REAL, DIMENSION( ims:ime, num_soil_layers, jms:jme ) ,     &
          INTENT(INOUT) ::   smois

! local variables
        integer i, j, k, l, l_oin, l_ca, l_co3, n, ii
        integer iphase, itype, izob
        integer p1st

        real dum, dumdlo, dumdhi, dumlandfrac, dumspd10
        real factaa, factbb, fracoin, fracca, fracco3, fractot
        real ustart, ustar1, ustart0
        real alphamask, f8, f50, f51, f52, wetfactor, sumdelta, ftot
        real smois_grav, wp, pclay
        real :: beta(4,7)
        real :: gamma(4), delta(4)
        real :: sz(8)
        real :: dustflux, densdust, mass1part
        real :: dp_meanvol_tmp
!
! from Nickovic et al., JGR, 2001 and Shaw et al. 2007
! beta: fraction of clay, small silt, large silt, and sand correcsponding to Zobler class (7)
! beta (1,*) for 0.5-1 um
! beta (2,*) for 1-10 um
! beta (3,*) for 10-25 um
! beta (4,*) for 25-50 um
!
        beta(1,1)=0.12
        beta(2,1)=0.04
        beta(3,1)=0.04
        beta(4,1)=0.80
        beta(1,2)=0.34
        beta(2,2)=0.28
        beta(3,2)=0.28
        beta(4,2)=0.10
        beta(1,3)=0.45
        beta(2,3)=0.15
        beta(3,3)=0.15
        beta(4,3)=0.25
        beta(1,4)=0.12
        beta(2,4)=0.09
        beta(3,4)=0.09
        beta(4,4)=0.70
        beta(1,5)=0.40
        beta(2,5)=0.05
        beta(3,5)=0.05
        beta(4,5)=0.50
        beta(1,6)=0.34
        beta(2,6)=0.18
        beta(3,6)=0.18
        beta(4,6)=0.30
        beta(1,7)=0.22
        beta(2,7)=0.09
        beta(3,7)=0.09
        beta(4,7)=0.60
        gamma(1)=0.08
        gamma(2)=1.00
        gamma(3)=1.00
        gamma(4)=0.12
!
! * Mass fractions for each size bin. These values were recommended by 
!   Natalie Mahowold, with bins 7 and 8 the same as bins 3 and 4 from CAM.
! * Changed slightly since Natelie's estimates do not add up to 1.0
! * This would need to be made more generic for other bin sizes.
!       sz(1)=0
!       sz(2)=1.78751e-06
!       sz(3)=0.000273786
!       sz(4)=0.00847978
!       sz(5)=0.056055
!       sz(6)=0.0951896
!       sz(7)=0.17
!       sz(8)=0.67
        sz(1)=0.0
        sz(2)=0.0
        sz(3)=0.0005
        sz(4)=0.0095
        sz(5)=0.03
        sz(6)=0.10
        sz(7)=0.18
        sz(8)=0.68

!   for now just do itype=1
        itype = 1
        iphase = ai_phase

!   loop over i,j and apply dust emissions
        k = kts
        do 1830 j = jts, jte
        do 1820 i = its, ite

    if( xland(i,j) > 1.5 ) cycle

! compute wind speed anyway, even though ustar is used below

        dumlandfrac = 1.
        dumspd10=(u10(i,j)*u10(i,j) + v10(i,j)*v10(i,j))**(0.5)
        if(dumspd10 >= 5.0) then
           dumspd10 = dumlandfrac* &
         ( dumspd10*dumspd10*(dumspd10-5.0))
         else
            dumspd10=0.
         endif

! part1 - compute vegetation mask
!
! * f8, f50, f51, f52 refer to vegetation classes from the Olsen categories
!   for desert, sand desert, grass aemi-desert, and shrub semi-desert
! * in WRF, vegetation types 7, 8 and 10 are grassland, shrubland, and savanna
!   that are dominate types in Mexico and probably have some erodable surface
!   during the dry season
! * currently modified these values so that only a small fraction of cell
!   area is erodable
! * these values are highly tuneable!

         alphamask=0.001
         if (ivgtyp(i,j) .eq. 7) then
           f8=0.005
           f50=0.00
           f51=0.10
           f51=0.066
           f52=0.00
           alphamask=(f8+f50)*1.0+(f51+f52)*0.5
         endif
         if (ivgtyp(i,j) .eq. 8) then
           f8=0.010
           f50=0.00
           f51=0.00
           f52=0.15
           f52=0.10
           alphamask=(f8+f50)*1.0+(f51+f52)*0.5
         endif
         if (ivgtyp(i,j) .eq. 10) then
           f8=0.00
           f50=0.00
           f51=0.01
           f52=0.00
           alphamask=(f8+f50)*1.0+(f51+f52)*0.5
         endif

! part2 - zobler
! 
! * in Shaw's paper, dust is computed for 4 size ranges:
!   0.5-1 um 
!    1-10 um  
!   10-25 um  
!   25-50 um
! * Shaw's paper also accounts for sub-grid variability in soil
!   texture, but here we just assume the same soil texture for each
!   grid cell
! * since MOSAIC is currently has a maximum size range up to 10 um,
!   neglect upper 2 size ranges and lowest size range (assume small)
! * map WRF soil classes arbitrarily to Zolber soil textural classes
! * skip dust computations for WRF soil classes greater than 13, i.e. 
!   do not compute dust over water, bedrock, and other surfaces
! * should be skipping for water surface at this point anyway
!
         izob=0
         if(isltyp(i,j).eq.1) izob=1
         if(isltyp(i,j).eq.2) izob=1
         if(isltyp(i,j).eq.3) izob=4
         if(isltyp(i,j).eq.4) izob=2
         if(isltyp(i,j).eq.5) izob=2
         if(isltyp(i,j).eq.6) izob=2
         if(isltyp(i,j).eq.7) izob=7
         if(isltyp(i,j).eq.8) izob=2
         if(isltyp(i,j).eq.9) izob=6
         if(isltyp(i,j).eq.10) izob=5
         if(isltyp(i,j).eq.11) izob=2
         if(isltyp(i,j).eq.12) izob=3
         if(isltyp(i,j).ge.13) izob=0
         if(izob.eq.0) goto 1840
!
! part3 - dustprod
!
         do ii=1,4
           delta(ii)=0.0
         enddo
         sumdelta=0.0
         do ii=1,4
           delta(ii)=beta(ii,izob)*gamma(ii)
           if(ii.lt.4) then
             sumdelta=sumdelta+delta(ii)
           endif
         enddo
         do ii=1,4
           delta(ii)=delta(ii)/sumdelta
         enddo

! part4 - wetness
!
! * assume dry for now, have passed in soil moisture to this routine
!   but needs to be included here
! * wetfactor less than 1 would reduce dustflux
! * convert model soil moisture (m3/m3) to gravimetric soil moisture
!   (mass of water / mass of soil in %) assuming a constant density 
!   for soil
         pclay=beta(1,izob)*100.
         wp=0.0014*pclay*pclay+0.17*pclay
         smois_grav=(smois(i,1,j)/2.6)*100.
         if(smois_grav.gt.wp) then
           wetfactor=sqrt(1.0+1.21*(smois_grav-wp)**0.68)
         else
           wetfactor=1.0
         endif
!        wetfactor=1.0

! part5 - dustflux
! lower bound on ustar = 20 cm/s as in Shaw et al, but set upper
! bound to 100 cm/s

         ustar1=ust(i,j)*100.0
         if(ustar1.gt.100.0) ustar1=100.0
         ustart0=20.0
         ustart=ustart0*wetfactor
         if(ustar1.le.ustart) then
           dustflux=0.0
         else
           dustflux=1.0e-14*(ustar1**4)*(1.0-(ustart/ustar1))
         endif
         dustflux=dustflux*10.0
! units kg m-2 s-1
         ftot=0.0
         do ii=1,2
           ftot=ftot+dustflux*alphamask*delta(ii)
         enddo
! convert to ug m-2 s-1
         ftot=ftot*1.0e+09

!   apportion other inorganics only
         factaa = (dtstep/dz8w(i,k,j))*alt(i,k,j)
         factbb = factaa * ftot
         fracoin = 1.00
!        fracca = 0.03*0.4
!        fracco3 = 0.03*0.6
         fracca = 0.0
         fracco3 = 0.0
         fractot = fracoin + fracca + fracco3
!   if (ivgtyp(i,j) .eq. 8) print*,'jdf',i,j,ustar1,ustart0,factaa,ftot
         chem(i,k,j,p_p25j)=chem(i,k,j,p_p25j) +   &
            factbb * (sz(3)+sz(4)+sz(5)+sz(6)) * fractot
!jdf        factbb * (sz(3)+sz(4)+sz(5)) * fractot
         chem(i,k,j,p_soila)=chem(i,k,j,p_soila) +   &
            factbb * (sz(7)+sz(8)) * fractot
!jdf        factbb * (sz(6)+sz(7)+sz(8)) * fractot
! mass1part is mass of a single particle in ug, density of dust ~2.5 g cm-3
         densdust=2.5
         dp_meanvol_tmp = 1.0e2*dginia*exp(1.5*l2sginia) ! accum 
         mass1part=0.523598*(dp_meanvol_tmp**3)*densdust*1.0e06
         chem(i,k,j,p_ac0)=chem(i,k,j,p_ac0) +   &
            factbb * (sz(3)+sz(4)+sz(5)+sz(6)) * fractot / mass1part
!jdf        factbb * (sz(3)+sz(4)+sz(5)) * fractot / mass1part
         dp_meanvol_tmp = 1.0e2*dginic*exp(1.5*l2sginic) ! coarse
         mass1part=0.523598*(dp_meanvol_tmp**3)*densdust*1.0e06
         chem(i,k,j,p_corn)=chem(i,k,j,p_corn) +   &
            factbb * (sz(7)+sz(8)) * fractot / mass1part
!jdf        factbb * (sz(6)+sz(7)+sz(8)) * fractot / mass1part

1840    continue

1820    continue
1830    continue

        return

   END subroutine sorgam_dust_emiss

!====================================================================================
!add another dust emission scheme following GOCART mechanism  --czhao  09/17/2009
!====================================================================================
  subroutine sorgam_dust_gocartemis (ktau,dt,num_soil_layers,alt,u_phy,    &
         v_phy,chem,rho_phy,dz8w,smois,u10,v10,p8w,erod,                   &
         ivgtyp,isltyp,xland,dx,g,                                         &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
  USE module_data_gocart_dust
  USE module_configure
  USE module_state_description
  USE module_model_constants, ONLY: mwdry
  USE module_data_mosaic_asect
  IMPLICIT NONE

   INTEGER,      INTENT(IN   ) :: ktau, num_soil_layers,           &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte
   INTEGER,DIMENSION( ims:ime , jms:jme )                  ,               &
          INTENT(IN   ) ::                                                 &
                                                     ivgtyp,               &
                                                     isltyp
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::                                   chem
  REAL, DIMENSION( ims:ime, num_soil_layers, jms:jme ) ,      &
      INTENT(INOUT) ::                               smois
   REAL,  DIMENSION( ims:ime , jms:jme, 3 )                   ,               &
          INTENT(IN   ) ::    erod
   REAL,  DIMENSION( ims:ime , jms:jme )                   ,               &
          INTENT(IN   ) ::                                                 &
                                                     u10,                  &
                                                     v10,                  &
                                                     xland
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),                        &
          INTENT(IN   ) ::                                                 &
                                                        alt,               &
                                                     dz8w,p8w,             &
                                              u_phy,v_phy,rho_phy

  REAL, INTENT(IN   ) :: dt,dx,g
!
! local variables
!
  integer :: nmx,i,j,k,ndt,imx,jmx,lmx
  integer ilwi, start_month
  real*8, DIMENSION (3) :: erodin
  real*8, DIMENSION (5) :: bems
  real*8  w10m,gwet,airden,airmas
  real*8  cdustemis,jdustemis,cdustcon,jdustcon
  real*8  cdustdens,jdustdens,mass1part,jdustdiam,cdustdiam,dp_meanvol_tmp
  real*8  dxy
  real*8  conver,converi
  real dttt
  real soilfacj,rhosoilj,rhosoilc
  real totalemis,accfrac,corfrac,rscale1,rscale2
  
  accfrac=0.07              ! assign 7% to accumulation mode
  corfrac=0.93              ! assign 93% to coarse mode
  rscale1=1.00  ! to account for the dust larger than 10um in radius
  rscale2=1.02  ! to account for the dust larger than 10um in radius
  accfrac=accfrac*rscale1
  corfrac=corfrac*rscale2

  rhosoilj=2.5e3
  rhosoilc=2.6e3
  soilfacj=soilfac*rhosoilj/rhosoilc

  conver=1.e-9
  converi=1.e9
!
! number of dust bins
  nmx=5
  k=kts
  do j=jts,jte
  do i=its,ite
!
! don't do dust over water!!!
     if(xland(i,j).lt.1.5)then

     ilwi=1
     start_month = 3   ! it doesn't matter, ch_dust is not a month dependent now, a constant
     w10m=sqrt(u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j))
     airmas=-(p8w(i,kts+1,j)-p8w(i,kts,j))*dx*dx/g   ! kg 

! we don't trust the u10,v10 values, if model layers are very thin near surface
     if(dz8w(i,kts,j).lt.12.)w10m=sqrt(u_phy(i,kts,j)*u_phy(i,kts,j)+v_phy(i,kts,j)*v_phy(i,kts,j))
    !erodin(1)=erod(i,j,1)/dx/dx   ! czhao erod shouldn't be scaled to the area, because it's a fraction
    !erodin(2)=erod(i,j,2)/dx/dx
    !erodin(3)=erod(i,j,3)/dx/dx
     erodin(1)=erod(i,j,1)
     erodin(2)=erod(i,j,2)
     erodin(3)=erod(i,j,3)
!
!  volumetric soil moisture over porosity
     gwet=smois(i,1,j)/porosity(isltyp(i,j))
     ndt=ifix(dt)
     airden=rho_phy(i,kts,j)
     dxy=dx*dx

    call sorgam_source_du( nmx, dt,i,j, &
                     erodin, ilwi, dxy, w10m, gwet, airden, airmas, &
                     bems,start_month,g)

!bems: kg/timestep/cell
    !sum up the dust emission from 0.1-10 um in radius 
    ! unit change from kg/timestep/cell to ug/m2/s
    totalemis=(sum(bems(1:5))/dt)*converi/dxy 
     ! to account for the particles larger than 10 um radius
     ! based on assumed size distribution
    jdustemis = totalemis*accfrac   ! accumulation mode
    cdustemis = totalemis*corfrac   ! coarse mode 

         cdustcon = sum(bems(1:5))*corfrac/airmas  ! kg/kg-dryair
         cdustcon = cdustcon * converi   ! ug/kg-dryair
         jdustcon = sum(bems(1:5))*accfrac/airmas  ! kg/kg-dryair
         jdustcon = jdustcon * converi   ! ug/kg-dryair

         chem(i,k,j,p_p25j)=chem(i,k,j,p_p25j) + jdustcon 
         chem(i,k,j,p_soila)=chem(i,k,j,p_soila) + cdustcon

! czhao doing dust number emission following pm10
! use soilfacj for accumulation mode because GOCART assign a less dense dust in
! accumulation mode
       chem(i,k,j,p_ac0) =  chem(i,k,j,p_ac0) + jdustcon * factnuma*soilfacj
       chem(i,k,j,p_corn) =  chem(i,k,j,p_corn) + cdustcon * factnumc*soilfac

     endif
  enddo
  enddo

end subroutine sorgam_dust_gocartemis

  SUBROUTINE sorgam_source_du( nmx, dt1,i,j, &
                     erod, ilwi, dxy, w10m, gwet, airden, airmas, &
                     bems,month,g0)

! ****************************************************************************
! *  Evaluate the source of each dust particles size classes  (kg/m3)        
! *  by soil emission.
! *  Input:
! *         EROD      Fraction of erodible grid cell                (-)
! *                   for 1: Sand, 2: Silt, 3: Clay
! *         DUSTDEN   Dust density                                  (kg/m3)
! *         DXY       Surface of each grid cell                     (m2)
! *         AIRVOL    Volume occupy by each grid boxes              (m3)
! *         NDT1      Time step                                     (s)
! *         W10m      Velocity at the anemometer level (10meters)   (m/s)
! *         u_tresh   Threshold velocity for particule uplifting    (m/s)
! *         CH_dust   Constant to fudge the total emission of dust  (s2/m2)
! *      
! *  Output:
! *         DSRC      Source of each dust type           (kg/timestep/cell) 
! *
! *  Working:
! *         SRC       Potential source                   (kg/m/timestep/cell)
! *
! ****************************************************************************

 USE module_data_gocart_dust

  INTEGER, INTENT(IN)    :: nmx
  REAL*8,    INTENT(IN)  :: erod(ndcls)
  INTEGER, INTENT(IN)    :: ilwi,month

  REAL*8,    INTENT(IN)    :: w10m, gwet
  REAL*8,    INTENT(IN)    :: dxy
  REAL*8,    INTENT(IN)    :: airden, airmas
  REAL*8,    INTENT(OUT)   :: bems(nmx)

  REAL*8    :: den(nmx), diam(nmx)
  REAL*8    :: tsrc, u_ts0, cw, u_ts, dsrc, srce
  REAL, intent(in)    :: g0
  REAL    :: rhoa, g,dt1
  INTEGER :: i, j, n, m, k

  ! default is 1 ug s2 m-5 == 1.e-9 kg s2 m-5
  !ch_dust(:,:)=0.8D-9   ! ch_dust is defined here instead of in the chemics_ini.F if with SORGAM  -czhao
   ch_dust(:,:)=1.0D-9  ! default 
  !ch_dust(:,:)=0.65D-9   ! ch_dust is tuned to match MODIS and AERONET measurements over Sahara 
  !ch_dust(:,:)=1.0D-9*0.36  ! ch_dust is scaled to sorgam total dust emission 

  ! executable statemenst
  DO n = 1, nmx
     ! Threshold velocity as a function of the dust density and the diameter from Bagnold (1941)
     den(n) = den_dust(n)*1.0D-3
     diam(n) = 2.0*reff_dust(n)*1.0D2
     g = g0*1.0E2
     ! Pointer to the 3 classes considered in the source data files
     m = ipoint(n)
     tsrc = 0.0
              rhoa = airden*1.0D-3
              u_ts0 = 0.13*1.0D-2*SQRT(den(n)*g*diam(n)/rhoa)* &
                   SQRT(1.0+0.006/den(n)/g/(diam(n))**2.5)/ &
                   SQRT(1.928*(1331.0*(diam(n))**1.56+0.38)**0.092-1.0)

              ! Case of surface dry enough to erode
             IF (gwet < 0.5) THEN  !  Pete's modified value
!              IF (gwet < 0.2) THEN
                 u_ts = MAX(0.0D+0,u_ts0*(1.2D+0+2.0D-1*LOG10(MAX(1.0D-3, gwet))))
              ELSE
                 ! Case of wet surface, no erosion
                 u_ts = 100.0
              END IF
              srce = frac_s(n)*erod(m)*dxy  ! (m2)
              IF (ilwi == 1 ) THEN
                 dsrc = ch_dust(n,month)*srce*w10m**2 &
                      * (w10m - u_ts)*dt1  ! (kg)
              ELSE
                 dsrc = 0.0
              END IF
              IF (dsrc < 0.0) dsrc = 0.0

              ! Update dust mixing ratio at first model level.
              !tc(n) = tc(n) + dsrc / airmas    !kg/kg-dryair -czhao
              bems(n) = dsrc     ! kg/timestep/cell

  ENDDO

END SUBROUTINE sorgam_source_du

!===========================================================================



!===========================================================================
!!!TUCCELLA (BUG, now wetscav_sorgam_driver is in module_prep_wetscav_sorgam.F)

!   subroutine wetscav_sorgam_driver (id,ktau,dtstep,ktauc,config_flags,      &
!               dtstepc,alt,t_phy,p8w,t8w,p_phy,chem,rho_phy,cldfra,        &
!	       qlsink,precr,preci,precs,precg,qsrflx,                      &
!	       gas_aqfrac, numgas_aqfrac,                                  &
!               ids,ide, jds,jde, kds,kde,                                  &
!               ims,ime, jms,jme, kms,kme,                                  &
!               its,ite, jts,jte, kts,kte                                   )

!  wet removal by grid-resolved precipitation
!  scavenging of cloud-phase aerosols and gases by collection, freezing, ...
!  scavenging of interstitial-phase aerosols by impaction
!  scavenging of gas-phase gases by mass transfer and reaction

!----------------------------------------------------------------------
!   USE module_configure
!   USE module_state_description
!   USE module_data_sorgam
!   USE module_mosaic_wetscav,only:  wetscav

!----------------------------------------------------------------------
!   IMPLICIT NONE

!   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

!   INTEGER,      INTENT(IN   )    ::                                &
!                                      ids,ide, jds,jde, kds,kde,    &
!                                      ims,ime, jms,jme, kms,kme,    &
!                                      its,ite, jts,jte, kts,kte,    &
!                                      id, ktau, ktauc, numgas_aqfrac
!      REAL,      INTENT(IN   ) :: dtstep,dtstepc
!
! all advected chemical species
!
!   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),          &
!         INTENT(INOUT ) ::                                chem

! fraction of gas species in cloud water
!   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, numgas_aqfrac ),     &
!         INTENT(IN ) ::                                   gas_aqfrac

!
!
! input from meteorology
!   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,        &
!          INTENT(IN   ) ::                                          &
!                                                        alt,        &
!                                                      t_phy,        &
!                                                      p_phy,        &
!                                                   t8w,p8w,         &
!	                            qlsink,precr,preci,precs,precg, &
!                                                    rho_phy,cldfra
!   REAL, DIMENSION( ims:ime, jms:jme, num_chem ),          &
!         INTENT(OUT ) ::                                qsrflx ! column change due to scavening

!   call wetscav (id,ktau,dtstep,ktauc,config_flags,                     &
!        dtstepc,alt,t_phy,p8w,t8w,p_phy,chem,rho_phy,cldfra,            &
!        qlsink,precr,preci,precs,precg,qsrflx,                          &
!        gas_aqfrac, numgas_aqfrac,                                      &
!        ntype_aer, nsize_aer, ncomp_aer,                                &
!        massptr_aer, dens_aer, numptr_aer,                              &
!        maxd_acomp, maxd_asize,maxd_atype, maxd_aphase, ai_phase, cw_phase, &
!        volumcen_sect, volumlo_sect, volumhi_sect,                      &
!        waterptr_aer, dens_water_aer,                                   &
!        scavimptblvol, scavimptblnum, nimptblgrow_mind, nimptblgrow_maxd, dlndg_nimptblgrow, &
!        ids,ide, jds,jde, kds,kde,                                      &
!        ims,ime, jms,jme, kms,kme,                                      &
!        its,ite, jts,jte, kts,kte                                       )

!   end subroutine wetscav_sorgam_driver

END Module module_aerosols_sorgam



