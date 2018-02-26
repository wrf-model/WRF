        module module_mosaic_soa_vbs


        use module_data_mosaic_kind, only: r8

        implicit none


        contains


!-------------------------------------------------------------------------------
        subroutine mosaic_soa_vbs_intr( &
           dtchem, p_atm, t_k, swdown_cell, &
           jaerosolstate, &
           aer, gas, water_a, area_wet_a, dp_wet_a, &
           kg, sat_soa, total_species, &
           ma, mc, mosaic_vars_aa )

        use module_data_mosaic_aero, only: &
           iasoaX_g, ipcg1_b_c_g, ismpa_g, &
           msoa_vbs_info, &
           naer, nanion, nbin_a_max, ncation, &
           ngas_aerchtot, ngas_ioa, ngas_soa, ngas_volatile, &
           mosaic_vars_aa_type

! arguments
        integer,  intent(inout), dimension(nbin_a_max)               :: jaerosolstate

        real(r8) :: dtchem
        real(r8) :: p_atm
        real(r8) :: t_k
        real(r8) :: swdown_cell

        real(r8), intent(inout), dimension(naer,3,nbin_a_max)        :: aer
        real(r8), intent(inout), dimension(ngas_aerchtot)            :: gas
        real(r8), intent(inout), dimension(nbin_a_max)               :: water_a
        real(r8), intent(inout), dimension(nbin_a_max)               :: area_wet_a
        real(r8), intent(inout), dimension(nbin_a_max)               :: dp_wet_a
        real(r8), intent(inout), dimension(ngas_aerchtot,nbin_a_max) :: kg
        real(r8), intent(inout), dimension(ngas_volatile)            :: sat_soa
        real(r8), intent(inout), dimension(ngas_volatile)            :: total_species
        real(r8), intent(inout), dimension(ncation,nbin_a_max)       :: mc
        real(r8), intent(inout), dimension(nanion,nbin_a_max)        :: ma

        type (mosaic_vars_aa_type), intent(inout) :: mosaic_vars_aa

! local
        integer :: ii, jj, kk
        integer :: start_svoc, nsoa_tmp
        integer :: vbs_nbin, vbs_uq_aqsoa, vbs_uq_par


        vbs_nbin = msoa_vbs_info(1)
        vbs_uq_aqsoa = msoa_vbs_info(2)
        vbs_uq_par = msoa_vbs_info(3)
       
        ii = mosaic_vars_aa%hostgridinfo(2)
        jj = mosaic_vars_aa%hostgridinfo(3)
        kk = mosaic_vars_aa%hostgridinfo(4)
        if (ii*jj*kk == 1) write(*,'(/a,3i10)') &
           'mosaic_soa_vbs_intr - vbs_nbin, uq_aqsoa, uq+par =', &
           vbs_nbin, vbs_uq_aqsoa, vbs_uq_par


!
! *** eventually need to do something about these glysoa subrs
!
!       if (glysoa_param == glysoa_param_simple)  call glysoa_simple(dtchem)
!       if (glysoa_param == glysoa_param_complex) call glysoa_complex(dtchem)

        start_svoc = 1
        nsoa_tmp       = 0
        if (vbs_nbin .eq. 0) then
           ! simple version, Hodzic and Jimenez, GMD, 2011
           start_svoc = ismpa_g
           ! 4-bin version, Knote et al., ACPD, 2014
        else if (vbs_nbin .eq. 4) then
           start_svoc = iasoaX_g
        else
           ! 9-bin version
           start_svoc = ipcg1_b_c_g
!          nsoa_tmp = ngas_volatile-start_svoc
        end if
        nsoa_tmp = ngas_ioa + ngas_soa - start_svoc + 1

!       call equilibrium(start_svoc,nsoa_tmp)
        call  equilibrium( start_svoc, nsoa_tmp, &
           aer, gas, kg, sat_soa, total_species )


        return
        end subroutine mosaic_soa_vbs_intr


!-------------------------------------------------------------------------------
! Calculates the equilibrium gas-particle partitioning for SOA species
        subroutine  equilibrium( start_ind, N, &
           aer, gas, kg, sat_soa, total_species )
!       subroutine  equilibrium( start_ind, N )
! This routine was implemented by Manish Shrivastava on 12/24/2009 to do gas-particle partitioning of SOA assuming thermodynamic equilibrium.
! Modified by Alma Hodzic 12/2012 to implement the partitioning for mozart-mosaic species (based on the initial code implemented by Manish Shrivastava and originated from CAMx) 
! This would give MOSAIC cpabilities of running both dynamic and equilibrium gas-particle partitioning
! Calls the subr soap. Subroutine soap calls subr spfcn
!        use module_data_mosaic_main
!        use module_data_mosaic_aero

        use module_data_mosaic_aero, only: &
           ioc_a, jtotal, &
           naer, nbin_a, nbin_a_max, ngas_aerchtot, ngas_volatile

        implicit none

! arguments
        integer, intent(in) :: start_ind, N
        real(r8), intent(inout), dimension(naer,3,nbin_a_max)        :: aer
        real(r8), intent(inout), dimension(ngas_aerchtot)            :: gas
        real(r8), intent(inout), dimension(ngas_aerchtot,nbin_a_max) :: kg
        real(r8), intent(inout), dimension(ngas_volatile)            :: sat_soa
        real(r8), intent(inout), dimension(ngas_volatile)            :: total_species

! local variables
!       integer, parameter :: N=ngas_soa !Total number of soa species
        integer, parameter :: itermax=2000
        integer :: idxfresh(N),idxaged(N)   !counter for fresh and aged soa species
        integer :: flagsoap(N) ! flagsoap determines if the species 'i' is fresh (flagsoap(i)=2) or aged(flagsoap(i)=1
        integer :: nsolfresh,nsolaged,ntrack,icontfresh,icontaged ! counters corresponding to fresh and aged species for mapping
        integer :: ibin,iter ! Bin nos.
        integer :: iv, jp
        integer :: i

        real(r8), parameter :: tinys=1.0d-15
        real(r8) :: dq,frqfresh(nbin_a),frqaged(nbin_a)
        real(r8) :: frqtotfresh,frqtotaged,frt
        real(r8) :: xsumfresh(nbin_a),xsumaged(nbin_a)
        real(r8) :: mnkfresh,mxkfresh,mnkaged,mxkaged
!       real betak
        real(r8) ::  Csatfresh(N), Ctotfresh(N)
        real(r8) ::  Cgasfresh(N),Caerfresh(N) ! Csat: Saturation conc., Ctot: Total organic mass
!       in gas+aerosol phase, Cgas:gas phase, Caer: Particle
        real(r8) ::    Csataged(N), Ctotaged(N)
        real(r8) ::  Cgasaged(N),Caeraged(N)
        real(r8) :: cpxfresh,cpxaged !Moles of pre-existing fresh and aged particle phase organic mass
        real(r8) :: dum, sum_dum, sum_soa, small_oc

!       real, parameter :: tolmin = 1.E-12
!       real, parameter :: conmin = 1.E-20
!       real totOA,minitw !total OA in particle phase
        real(r8) :: cpx !pre-existing OA umol/m3
        real(r8) :: Ctot(N),Caer(N),Cgas(N),Csat(N)
        real(r8) :: Paer(ngas_volatile)


!       LOGICAL check
        jp=jtotal
        iter=0
         cpxaged=0.0
        cpxfresh=0.0 ! Assume no pres-existing OA forms a solution
        nsolfresh=0
         nsolaged=0
         icontfresh=0
         icontaged=0
         dq=0.0

! Paer holds the organic aerosol values in each volatility bin (sum of all size bins)
          do iv=1,ngas_volatile
           Paer(iv)=0.0
          enddo

! Initialize flagsoap
          do i=1,N
             flagsoap(i)=1
             Ctot(i) = 0.0
             Ctotaged(i) = 0.0
             Ctotfresh(i) = 0.0
             Caer(i) = 0.0
             Caeraged(i) = 0.0
             Caerfresh(i) = 0.0
             Cgas(i) = 0.0
             Cgasaged(i) = 0.0
             Cgasfresh(i) = 0.0
             Csat(i) = 0.0
             Csataged(i) = 0.0
             Csatfresh(i) = 0.0
          enddo

! Calculate Ctot and Paer
!              do iv = ipcg1_b_c_g, ngas_volatile
!              do iv = start_ind, ngas_ioa + ngas_soa
              do iv = start_ind, (start_ind + N - 1)
        total_species(iv) = gas(iv)
        do ibin = 1, nbin_a
          total_species(iv) = total_species(iv) + aer(iv,jtotal,ibin)
           Paer(iv)=Paer(iv)+aer(iv,jtotal,ibin)
        enddo
      enddo

! Calculate pre-existing moles of OA (cpx) as sum of all size bins
        do ibin=1,nbin_a
        cpxaged= cpxaged+aer(ioc_a,jp,ibin)
         enddo

!  Maps arrays starting from start_ind or ipcg1_b_c_g on to corresponding arrays starting from 1 for just soa species
        do i=1,N
           Ctot(i)=total_species(start_ind+i-1)
           Caer(i)=Paer(start_ind+i-1)
           Csat(i)=sat_soa(start_ind+i-1)
           Cgas(i)=gas(start_ind+i-1)
         enddo

! Initialize mapping array indices
          do i=1,N
            idxfresh(i)=0
            idxaged(i)=0
          enddo

!     Seperate the fresh and aged species and treat them as 2 different solutions. Note this approach differes from PMCAMx
!     In PMCAMx if flagsoap(i) was set to zero those species were not considered solution forming.
         do i=1,N
            flagsoap(i)=1 ! Biomass burning(carbon and oxygen species) +traditional soa species
         enddo

!         do i=1,9
!            flagsoap(i)=1 ! Biomass burning(carbon and oxygen species) +traditional soa species
!          enddo
!         do i=10,18
!            flagsoap(i)=1 ! Biomass burning(carbon and oxygen species) +traditional soa species
!          enddo
!        do i=19,26
!            flagsoap(i)=1 ! Biomass burning(carbon and oxygen species) +traditional soa species
!          enddo
!        do i=27,34
!            flagsoap(i)=1 ! Biomass burning(carbon and oxygen species) +traditional soa species
!          enddo
!        do i=35,43
!            flagsoap(i)=1 ! Biomass burning(carbon and oxygen species) +traditional soa species
!          enddo
!         do i=44,52
!            flagsoap(i)=1 ! Biomass burning(carbon and oxygen species) +traditional soa species
!          enddo
!        do i=53,60
!            flagsoap(i)=1 ! Biomass burning(carbon and oxygen species) +traditional soa species
!          enddo
!      do i=61,68
!            flagsoap(i)=1 ! Biomass burning(carbon and oxygen species) +traditional soa species
!          enddo
!        do i=69,84
!            flagsoap(i)=1 !Oxidized fossil oxygen
!        enddo

      do i=1,N
         if (flagsoap(i).eq.2) then ! fresh primary species forming 1 solution
           icontfresh=icontfresh+1  ! count the number of fresh species
            idxfresh(icontfresh) = i  !Map the species
            Csatfresh(icontfresh)=Csat(i)
            Ctotfresh(icontfresh)=Ctot(i)
            Caerfresh(icontfresh)=Caer(i)
            Cgasfresh(icontfresh)=Cgas(i)
            nsolfresh=nsolfresh+1
         elseif (flagsoap(i).eq.1) then                       ! Aged SOA species forming another solution
            icontaged=icontaged+1
            idxaged(icontaged) = i
            Csataged(icontaged)=Csat(i)
            Ctotaged(icontaged)=Ctot(i)
            Caeraged(icontaged)=Caer(i)
            Cgasaged(icontaged)=Cgas(i)
            nsolaged=nsolaged+1
         endif
      enddo

!      Caluclate the initial equilibrium partitioning by the bisection method (CMU PMCAMx approach)
!       If all fresh abd aged species form a solution
!         call soap(ngas_soa,Ctot,Csat,Caer,Cgas,cpx)

!       if fresh and aged species form seperate solutions
      if (nsolfresh.gt.0)  call soap(nsolfresh,Ctotfresh, &
                    Csatfresh,Caerfresh,Cgasfresh,cpxfresh)
      if (nsolaged.gt.0)  call soap(nsolaged,Ctotaged, &
                  Csataged,Caeraged,Cgasaged,cpxaged)

!     Map the fresh and aged species back into original arrays
!     Now assign the equilibrium gas-particle partitioning arrays
        ntrack=0
       do i=1,N ! Map the fresh and aged species back into array from 1 to N after calculating equilibrium
         if (idxfresh(i).gt.0) then
         Caer(idxfresh(i))= Caerfresh(i)
         Cgas(idxfresh(i))= Cgasfresh(i)
         Ctot(idxfresh(i))= Ctotfresh(i)
         ntrack=ntrack+1
         endif
         if (idxaged(i).gt.0) then
         Caer(idxaged(i))= Caeraged(i)
         Cgas(idxaged(i))= Cgasaged(i)
         Ctot(idxaged(i))= Ctotaged(i)
         ntrack=ntrack+1
         endif
       enddo

!       Check for total number of species
        if (ntrack.ne.N) then
           call wrf_error_fatal('Error in mapping fresh and primary species arrays')
        endif

! From here on distribute the organic aerosol in size bins following Koo et al. 2003 " Integrated approaches to modeling
! the organic and inorganic atmospheric aerosol components"
! The original code from PMCAMx was modified to include 2 solutions for fresh and primary species
! by Manish Shrivastava on 01/11/2010
! Calculate total organic aerosol OA(in nmoles/m3) in each bin for either of fresh and aged aerosols

         do ibin=1,nbin_a
           xsumfresh(ibin)=0.0
           xsumaged(ibin)=0.0
              xsumaged(ibin)= xsumaged(ibin)+aer(ioc_a,jp,ibin)!Caluclate pre-existing primary in each bin for aged aerosol
!         do iv = start_ind, ngas_ioa + ngas_soa
         do iv = start_ind, (start_ind + N - 1)
           if (flagsoap(iv-start_ind+1).eq.2) then
               xsumfresh(ibin)= xsumfresh(ibin)+aer(iv,jtotal,ibin)
           elseif (flagsoap(iv-start_ind+1).eq.1) then
              xsumaged(ibin)= xsumaged(ibin)+aer(iv,jtotal,ibin)
                elseif (flagsoap(iv-start_ind+1).eq.0) then
                 write(*,*) 'Error in mapping flagsoap to start_ind'
           endif
         enddo
!         do iv = ipcg1_b_c_g, ngas_volatile
!           if (flagsoap(iv-ipcg1_b_c_g+1).eq.2) then
!               xsumfresh(ibin)= xsumfresh(ibin)+aer(iv,jtotal,ibin)
!           elseif (flagsoap(iv-ipcg1_b_c_g+1).eq.1) then
!              xsumaged(ibin)= xsumaged(ibin)+aer(iv,jtotal,ibin)
!                elseif (flagsoap(iv-ipcg1_b_c_g+1).eq.0) then
!                 write(*,*) 'Error in mapping flagsoap to ipcg1_b_c_g'
!           endif
!         enddo

! Give a small non-zero value to xsum if it is zero in the section
          if (xsumfresh(ibin).eq.0.0) xsumfresh(ibin)=tinys
          if (xsumaged(ibin).eq.0.0) xsumaged(ibin)=tinys
        enddo


! Calculate dq as (gas concentration) G(t)-G(t+h):
! Caluclate driving force at previous time step (Cgas,i-XiCsati) for both fresh and aged solutions
!          do iv = start_ind, ngas_ioa + ngas_soa
          do iv = start_ind, (start_ind + N - 1)
           if (Ctot(iv-start_ind+1).lt.1d-10) goto 120 ! If a given species concentration is too low skip
            dq=gas(iv)-Cgas(iv-start_ind+1) !Since both fresh and aged species have been remapped to an array going from 1 to N
!          do iv = ipcg1_b_c_g, ngas_volatile
!           if (Ctot(iv-ipcg1_b_c_g+1).lt.1d-10) goto 120 ! If a given species concentration is too low skip
!            dq=gas(iv)-Cgas(iv-ipcg1_b_c_g+1) !Since both fresh and aged species have been remapped to an array going from 1 to N
           frqtotfresh=0.0d0
           frqtotaged=0.0d0
           mnkfresh=0.0d0
           mnkaged=0.0d0
           mxkfresh=0.0d0
           mxkaged=0.0d0

             do ibin=1,nbin_a
! fraceq(iv,ibin) is calculated as the rate of mass transfer
! The weighting fractions frqfresh(ibin) amd frqaged(ibin) are caluclated assuming mole fractions from previous time step
! This assumtion could be relaxed by iterativetely solving this equation
           if (flagsoap(iv-start_ind+1).eq.2) then
              frqfresh(ibin)= kg(iv,ibin)*(gas(iv) & ! replaced fraceq(iv,ibin) by kg(iv,ibin) on 01/19/10
              -(aer(iv,jtotal,ibin))/xsumfresh(ibin) &
              *Csat(iv-start_ind+1))
          endif

           if (flagsoap(iv-start_ind+1).eq.1) then
              frqaged(ibin)= kg(iv,ibin)*(gas(iv) & ! replaced fraceq(iv,ibin) by kg(iv,ibin) on 01/19/10
             -(aer(iv,jtotal,ibin))/xsumaged(ibin) &
              *Csat(iv-start_ind+1))
          endif

!           if (flagsoap(iv-ipcg1_b_c_g+1).eq.2) then
!              frqfresh(ibin)= kg(iv,ibin)*(gas(iv) & ! replaced fraceq(iv,ibin) by kg(iv,ibin) on 01/19/10
!              -(aer(iv,jtotal,ibin))/xsumfresh(ibin) &
!              *Csat(iv-ipcg1_b_c_g+1))
!          endif
!
!           if (flagsoap(iv-ipcg1_b_c_g+1).eq.1) then
!              frqaged(ibin)= kg(iv,ibin)*(gas(iv) & ! replaced fraceq(iv,ibin) by kg(iv,ibin) on 01/19/10
!             -(aer(iv,jtotal,ibin))/xsumaged(ibin) &
!              *Csat(iv-ipcg1_b_c_g+1))
!          endif
            mnkfresh=min(mnkfresh,frqfresh(ibin))
            mnkaged=min(mnkaged,frqaged(ibin))

            mxkfresh=max(mxkfresh,frqfresh(ibin))
            mxkaged=max(mxkaged,frqaged(ibin))
          enddo ! for ibin

!          Repeat code from this point on for aged aerosol species
            if (flagsoap(iv-start_ind+1).eq.2) then
!            if (flagsoap(iv-ipcg1_b_c_g+1).eq.2) then
! Condensation is favored in the next time step in this bin
          if(dq.gt.0.and.mnkfresh.lt.0.and.mxkfresh.gt.0) then
             do ibin=1,nbin_a
               frqfresh(ibin)=max(frqfresh(ibin)-mnkfresh,0.0d0)
              enddo
! evaporation is favored in the next time step in this bin
          elseif(dq.lt.0.and.mxkfresh.gt.0.and.mnkfresh.lt.0) then
              do ibin=1,nbin_a
              frqfresh(ibin)=min(frqfresh(ibin)-mxkfresh,0.0d0)
              enddo
           endif
           do ibin=1,nbin_a
            frqtotfresh=frqtotfresh+frqfresh(ibin)
           enddo

! Re-normalize frqfresh(ibin)
! Additional code to check for frqtotfresh and frqtotaged
! Added by Manish Shrivastava on 02/19/2010

          do ibin=1,nbin_a
           frqfresh(ibin)=frqfresh(ibin)/frqtotfresh
           enddo
 
            elseif(flagsoap(iv-start_ind+1).eq.1) then
!            elseif(flagsoap(iv-ipcg1_b_c_g+1).eq.1) then
          if(dq.gt.0.and.mnkaged.lt.0.and.mxkaged.gt.0) then
             do ibin=1,nbin_a
               frqaged(ibin)=max(frqaged(ibin)-mnkaged,0.0d0)
              enddo
          elseif(dq.lt.0.and.mxkaged.gt.0.and.mnkaged.lt.0) then
              do ibin=1,nbin_a
              frqaged(ibin)=min(frqaged(ibin)-mxkaged,0.0d0)
              enddo
           endif

           do ibin=1,nbin_a
            frqtotaged=frqtotaged+frqaged(ibin)
           enddo

           do ibin=1,nbin_a
           frqaged(ibin)=frqaged(ibin)/frqtotaged
           enddo

           endif ! for flagsoap
!     Condense all condensing species
           if(dq.gt.0.0d0) then

            !  Map the species back into the original MOSAIC arrays
             do ibin=1,nbin_a
                 if (flagsoap(iv-start_ind+1).eq.2) then
                 aer(iv,jtotal,ibin)= aer(iv,jtotal,ibin)+dq*frqfresh(ibin)
                 endif
                if (flagsoap(iv-start_ind+1).eq.1) then
                aer(iv,jtotal,ibin)= aer(iv,jtotal,ibin)+dq*frqaged(ibin)
                endif
             enddo
! Set the gas phase species to equilibrium value
                gas(iv)=Cgas(iv-start_ind+1)

!             do ibin=1,nbin_a
!                 if (flagsoap(iv-ipcg1_b_c_g+1).eq.2) then
!                 aer(iv,jtotal,ibin)= aer(iv,jtotal,ibin)+dq*frqfresh(ibin)
!                 endif
!                if (flagsoap(iv-ipcg1_b_c_g+1).eq.1) then
!                aer(iv,jtotal,ibin)= aer(iv,jtotal,ibin)+dq*frqaged(ibin)
!                endif
!             enddo
!! Set the gas phase species to equilibrium value
!                gas(iv)=Cgas(iv-ipcg1_b_c_g+1)

!     Evaporate all evaporating species
         elseif(dq.lt.0.0d0) then
            iter=0
100         frt=1.0d0
               do ibin=1,nbin_a
                   if (flagsoap(iv-start_ind+1).eq.2) then
!                   if (flagsoap(iv-ipcg1_b_c_g+1).eq.2) then
! Cannot evaporate more than whats in the bin ie ratio (aer(iv,jtotal,ibin)/dq*frqfresh(ibin)) should be less than equal to 1
                 if(frqfresh(ibin).gt.0.0d0) &
         frt=MAX(MIN(aer(iv,jtotal,ibin)/abs(-dq*frqfresh(ibin)),frt),0.0d0)
!                  elseif(flagsoap(iv-ipcg1_b_c_g+1).eq.1) then
                  elseif(flagsoap(iv-start_ind+1).eq.1) then
               if(frqaged(ibin).gt.0.0d0) &
         frt=MAX(MIN(aer(iv,jtotal,ibin)/abs(-dq*frqaged(ibin)),frt),0.0d0)
                  endif ! for flagsoap
               enddo ! for ibin



         frqtotfresh=0.0d0
         frqtotaged=0.0d0

             do ibin=1,nbin_a
        if (flagsoap(iv-start_ind+1).eq.2) then
!        if (flagsoap(iv-ipcg1_b_c_g+1).eq.2) then
               aer(iv,jtotal,ibin)= &
! Since dq is negative this is evaporating aerosols
               MAX(aer(iv,jtotal,ibin)+frt*dq*frqfresh(ibin),0.0d0)
         if(aer(iv,jtotal,ibin).lt.tinys) frqfresh(ibin)=0.0d0
              frqtotfresh=frqtotfresh+frqfresh(ibin)
!        elseif (flagsoap(iv-ipcg1_b_c_g+1).eq.1) then
        elseif (flagsoap(iv-start_ind+1).eq.1) then
               aer(iv,jtotal,ibin)= &
               MAX(aer(iv,jtotal,ibin)+frt*dq*frqaged(ibin),0.0d0)
         if(aer(iv,jtotal,ibin).lt.tinys) frqaged(ibin)=0.0d0
              frqtotaged=frqtotaged+frqaged(ibin)
         endif ! for flagsoap
             enddo ! for ibin

! Check if we should evaporate more
          dq=(1.0d0-frt)*dq
!         if (flagsoap(iv-ipcg1_b_c_g+1).eq.2) then
         if (flagsoap(iv-start_ind+1).eq.2) then
           if(dq.lt.-1.d-8) then ! check if d-8 is better
             if(frqtotfresh.gt.tinys) then ! we have sections which are not empty
              if(iter.le.itermax) then ! check infinite loop
                iter = iter + 1
                do ibin = 1,nbin_a
                  frqfresh(ibin) = frqfresh(ibin) / frqtotfresh
                enddo ! for ibin
             goto 100
            endif ! for iter
          endif ! frqtotfresh.gt.tinys
           endif ! dq.lt.-1.d-7
!          elseif (flagsoap(iv-ipcg1_b_c_g+1).eq.1) then
          elseif (flagsoap(iv-start_ind+1).eq.1) then
           if(dq.lt.-1.d-8) then
             if(frqtotaged.gt.tinys) then ! we have sections which are not empty
              if(iter.le.itermax) then ! check infinite loop
                iter = iter + 1
                do ibin = 1,nbin_a
                  frqaged(ibin) = frqaged(ibin) / frqtotaged
                enddo
               goto 100
          endif
            endif
            endif

            ! we need to evaporate more to achieve equilibrium
            ! but we completely evaporated the species in all sections
            ! or exceeded itermax
           endif ! for flagsoap

! now set the gas species concentration conservatively
!           gas(iv)=Ctot(iv-ipcg1_b_c_g+1)
           gas(iv)=Ctot(iv-start_ind+1)
             do ibin=1,nbin_a
               gas(iv)=gas(iv)-aer(iv,jtotal,ibin)
             enddo
        endif ! if dq.gt.0

120       continue
           enddo ! for iv=start_ind

       end subroutine equilibrium


!--------------------------------------------------------------------------------
!---------------------------------
! Calculates the equilibrium gas-particle partitioning for SOA species when MOZART_MOSAIC_4BIN_KPP is used
! This routine was modified by Alma Hodzic based on the initial code implemented by Manish Shrivastava and originated from CAMx 

!++ alma - removed the subr  equilibrium_smp
!        subroutine  equilibrium_smp
!..
!       end subroutine equilibrium_smp
!--


!--------------------------------------------------------------------------------
!    This subr spfcn calculates the objective function fval to solve gas-particle partitioning of SOA
!    subr spfcn is called from within the subr soap
        subroutine spfcn(N,Ctot,Csat,Ca,cpx,tom,fval)
!        use module_data_mosaic_main
!        use module_data_mosaic_aero
      implicit none
       real(r8):: Ctot(N),Csat(N),Ca(N),tom,fval,cpx

         integer i,N
        fval=0.0
         do i=1, N
         Ca(i)=Ctot(i)*tom/(tom+Csat(i)/1)! Replace the divisor 1 by Molecular Weights if the units for Csat(i) are ug/m3 or ng/m3
        fval=fval+Ca(i)/1 ! The divisor is set to 1 as the species are in nmol/m3
        enddo
          fval=fval+cpx-tom
        return

       end subroutine spfcn


!--------------------------------------------------------------------------------
        subroutine soap(N,Ctot,Csat,Ca,Cgas,cpx)
! SOAP calculates the gas-partitioning of SOA. Adapted from PMCAMx and uses the bisection approach.
! SOAP calls subr spfcn which calculates the objective function for solving gas-particle partitioning
!        use module_data_mosaic_main
!        use module_data_mosaic_aero

        real(r8),  parameter :: xtol = 5.0e-5
          real(r8):: Ctot(N),Csat(N),cpx,Ca(N),Cgas(N)
          real(r8):: xend,dx,xmid,fend,fmid,sun
         integer i,N,znum
        
         sun=0.0
          do i=1,N
            if (Csat(i).gt.0) then
            sun=sun+Ctot(i)/Csat(i) !If a species does not exist its Csat is zero
           else
           endif
          enddo
         if(cpx.lt.1e-9.and.sun.le.1.0) then !if ctots for all species are less than corr. csats and cpre is negligible
           do i=1,N
             Cgas(i)=Ctot(i)
             Ca(i)=0.0
           enddo
         goto 900
        endif

       xend=0.0
       do i=1, N
         xend=xend+Ctot(i)/1 ! Replace the divisor 1 by molecular weight if the units of Ctot(i) are in ng/m3 or ug/m3
         enddo
        xend=xend+cpx ! total number of moles
       if (xend.gt.1e-10) then 
           call spfcn(N,Ctot,Csat,Ca,cpx,xend,fend) ! Calculates the objective function
        else
!          write(*,*) "Total no of moles less than 1e-10 bypassing soap" 
              goto 100
      endif
          if(abs(fend).le.xtol*xend) goto 99 ! Check for tolerance
          if (fend.gt.0.0) then ! The objective function is supposed to be less than equal to zero
         write(*,*) "Error in SOAP"
         goto 50
        endif
           dx=xend-cpx
        do znum=1,200
        dx=0.5*dx
         xmid=xend-dx ! Find the midpoint following the bisection approach
           call spfcn (N,Ctot,Csat,Ca,cpx,xmid,fmid) ! Re-calculate the objective function
          if(abs(fmid).le.xtol*xmid.or.dx.le.xtol*xmid) goto 100 ! converged
           if (fmid.lt.0.0) xend=xmid
         enddo
50         call wrf_message("Error in SOAP")
         call wrf_error_fatal("Error: max number of iterations reached")
      

99     xmid=xend
100    continue
        do i=1, N
        Ca(i)=min(Ctot(i), Ca(i))
        Cgas(i)=Ctot(i)-Ca(i)
       enddo
900   continue
        

!     write(*,*) xmid
     return

       end subroutine soap



!-------------------------------------------------------------------------------
      subroutine soa_vbs_load_params( ipass )
!
! sets species pointers (gas, aerosol, electrolyte) and 
!    physical parameters (molecular weight, density, ...)
!    that are different from those in the mosaic offline box-model code
!
      use module_data_mosaic_aero, only: &
         aer_name, gas_name, &
         dens_aer_mac, dens_comp_a, &
         mw_aer_mac, mw_comp_a, mw_gas, &
         ngas_aerchtot, partial_molar_vol, v_molar_gas

      use module_data_mosaic_aero, only: &
         ih2so4_g,     ihno3_g,      ihcl_g,      inh3_g,         &
         imsa_g,                                                  &
         iaro1_g,      iaro2_g,      ialk1_g,     iole1_g,        &
         iapi1_g,      iapi2_g,      ilim1_g,     ilim2_g,        &
         iso4_a,     ino3_a,     icl_a,     inh4_a,     ico3_a,   &
         imsa_a,     ina_a,      ica_a,     ioc_a,      ibc_a,    &
         ioin_a,     iaro1_a,    iaro2_a,   ialk1_a,    iole1_a,  &
         iapi1_a,    iapi2_a,    ilim1_a,   ilim2_a,              &
         jnh4so4,    jlvcite,    jnh4hso4,   jnh4no3,    jnh4cl,  &
         jna2so4,    jna3hso4,   jnahso4,    jnano3,     jnacl,   &
         jcaso4,     jcano3,     jcacl2,     jcaco3,     jh2so4,  &
         jhno3,      jhcl,       jhhso4,                          &
         jnh4msa,    jnamsa,     jcamsa2,    jmsa,                &
         joc,        jbc,        join,       jaro1,      jaro2,   &
         jalk1,      jole1,      japi1,      japi2,      jlim1,   &
         jlim2,      jh2o
  
      use module_data_mosaic_aero, only: &
         in2o5_g,      iclno2_g,                                 &
         ipcg1_b_c_g,  ipcg2_b_c_g,  ipcg3_b_c_g,  ipcg4_b_c_g,  &
         ipcg5_b_c_g,  ipcg6_b_c_g,  ipcg7_b_c_g,  ipcg8_b_c_g,  &
         ipcg9_b_c_g,                                            &
         ipcg1_b_o_g,  ipcg2_b_o_g,  ipcg3_b_o_g,  ipcg4_b_o_g,  &
         ipcg5_b_o_g,  ipcg6_b_o_g,  ipcg7_b_o_g,  ipcg8_b_o_g,  &
         ipcg9_b_o_g,                                            &
         iopcg1_b_c_g, iopcg2_b_c_g, iopcg3_b_c_g, iopcg4_b_c_g, &
         iopcg5_b_c_g, iopcg6_b_c_g, iopcg7_b_c_g, iopcg8_b_c_g, &
         iopcg1_b_o_g, iopcg2_b_o_g, iopcg3_b_o_g, iopcg4_b_o_g, &
         iopcg5_b_o_g, iopcg6_b_o_g, iopcg7_b_o_g, iopcg8_b_o_g, &
         ipcg1_f_c_g,  ipcg2_f_c_g,  ipcg3_f_c_g,  ipcg4_f_c_g,  &
         ipcg5_f_c_g,  ipcg6_f_c_g,  ipcg7_f_c_g,  ipcg8_f_c_g,  &
         ipcg9_f_c_g,                                            &
         ipcg1_f_o_g,  ipcg2_f_o_g,  ipcg3_f_o_g,  ipcg4_f_o_g,  &
         ipcg5_f_o_g,  ipcg6_f_o_g,  ipcg7_f_o_g,  ipcg8_f_o_g,  &
         ipcg9_f_o_g,                                            &
         iopcg1_f_c_g, iopcg2_f_c_g, iopcg3_f_c_g, iopcg4_f_c_g, &
         iopcg5_f_c_g, iopcg6_f_c_g, iopcg7_f_c_g, iopcg8_f_c_g, &
         iopcg1_f_o_g, iopcg2_f_o_g, iopcg3_f_o_g, iopcg4_f_o_g, &
         iopcg5_f_o_g, iopcg6_f_o_g, iopcg7_f_o_g, iopcg8_f_o_g, &
         iant1_c_g,    iant2_c_g,    iant3_c_g,    iant4_c_g,    &
         iant1_o_g,    iant2_o_g,    iant3_o_g,    iant4_o_g,    &
         ibiog1_c_g,   ibiog2_c_g,   ibiog3_c_g,   ibiog4_c_g,   &
         ibiog1_o_g,   ibiog2_o_g,   ibiog3_o_g,   ibiog4_o_g,   &
         ismpa_g,      ismpbb_g,                                 &
         iasoa1_g,     iasoa2_g,     iasoa3_g,     iasoa4_g,     &
         iasoaX_g,                                               &
         ibsoa1_g,     ibsoa2_g,     ibsoa3_g,     ibsoa4_g,     &
         ibsoaX_g,                                               &
         igly,         iho
!        iiepox_g,     igly_g

      use module_data_mosaic_aero, only: &
!        idust_a,                                                &
!        itr1r1_a,     itr1r2_a,     itr1r3_a,     itr1r4_a,     &
         ipcg1_b_c_a,  ipcg2_b_c_a,  ipcg3_b_c_a,  ipcg4_b_c_a,  &
         ipcg5_b_c_a,  ipcg6_b_c_a,  ipcg7_b_c_a,  ipcg8_b_c_a,  &
         ipcg9_b_c_a,                                            &
         ipcg1_b_o_a,  ipcg2_b_o_a,  ipcg3_b_o_a,  ipcg4_b_o_a,  &
         ipcg5_b_o_a,  ipcg6_b_o_a,  ipcg7_b_o_a,  ipcg8_b_o_a,  &
         ipcg9_b_o_a,                                            &
         iopcg1_b_c_a, iopcg2_b_c_a, iopcg3_b_c_a, iopcg4_b_c_a, &
         iopcg5_b_c_a, iopcg6_b_c_a, iopcg7_b_c_a, iopcg8_b_c_a, &
         iopcg1_b_o_a, iopcg2_b_o_a, iopcg3_b_o_a, iopcg4_b_o_a, &
         iopcg5_b_o_a, iopcg6_b_o_a, iopcg7_b_o_a, iopcg8_b_o_a, &
         ipcg1_f_c_a,  ipcg2_f_c_a,  ipcg3_f_c_a,  ipcg4_f_c_a,  &
         ipcg5_f_c_a,  ipcg6_f_c_a,  ipcg7_f_c_a,  ipcg8_f_c_a,  &
         ipcg9_f_c_a,                                            &
         ipcg1_f_o_a,  ipcg2_f_o_a,  ipcg3_f_o_a,  ipcg4_f_o_a,  &
         ipcg5_f_o_a,  ipcg6_f_o_a,  ipcg7_f_o_a,  ipcg8_f_o_a,  &
         ipcg9_f_o_a,                                            &
         iopcg1_f_c_a, iopcg2_f_c_a, iopcg3_f_c_a, iopcg4_f_c_a, &
         iopcg5_f_c_a, iopcg6_f_c_a, iopcg7_f_c_a, iopcg8_f_c_a, &
         iopcg1_f_o_a, iopcg2_f_o_a, iopcg3_f_o_a, iopcg4_f_o_a, &
         iopcg5_f_o_a, iopcg6_f_o_a, iopcg7_f_o_a, iopcg8_f_o_a, &
         iant1_c_a,    iant2_c_a,    iant3_c_a,    iant4_c_a,    &
         iant1_o_a,    iant2_o_a,    iant3_o_a,    iant4_o_a,    &
         ibiog1_c_a,   ibiog2_c_a,   ibiog3_c_a,   ibiog4_c_a,   &
         ibiog1_o_a,   ibiog2_o_a,   ibiog3_o_a,   ibiog4_o_a,   &
         ismpa_a,      ismpbb_a,                                 &
         iasoa1_a,     iasoa2_a,     iasoa3_a,     iasoa4_a,     &
         iasoaX_a,                                               &
         ibsoa1_a,     ibsoa2_a,     ibsoa3_a,     ibsoa4_a,     &
         ibsoaX_a,                                               &
         iglysoa_r1_a, iglysoa_r2_a, iglysoa_sfc_a, iglysoa_nh4_a, &
         iglysoa_oh_a
!        iiepox_a,     igly_a,       iiepoxos_a,   itetrol_a,    &
!        itanv_a,      isopnv_a,     iternv_a,     iseqnv_a,     &
!        isianv_a

      use module_data_mosaic_aero, only: &
!        jdust,                                          &
!        jtr1r1,     jtr1r2,     jtr1r3,     jtr1r4,     &
         jpcg1_b_c,  jpcg2_b_c,  jpcg3_b_c,  jpcg4_b_c,  &
         jpcg5_b_c,  jpcg6_b_c,  jpcg7_b_c,  jpcg8_b_c,  &
         jpcg9_b_c,                                      &
         jpcg1_b_o,  jpcg2_b_o,  jpcg3_b_o,  jpcg4_b_o,  &
         jpcg5_b_o,  jpcg6_b_o,  jpcg7_b_o,  jpcg8_b_o,  &
         jpcg9_b_o,                                      &
         jopcg1_b_c, jopcg2_b_c, jopcg3_b_c, jopcg4_b_c, &
         jopcg5_b_c, jopcg6_b_c, jopcg7_b_c, jopcg8_b_c, &
         jopcg1_b_o, jopcg2_b_o, jopcg3_b_o, jopcg4_b_o, &
         jopcg5_b_o, jopcg6_b_o, jopcg7_b_o, jopcg8_b_o, &
         jpcg1_f_c,  jpcg2_f_c,  jpcg3_f_c,  jpcg4_f_c,  &
         jpcg5_f_c,  jpcg6_f_c,  jpcg7_f_c,  jpcg8_f_c,  &
         jpcg9_f_c,                                      &
         jpcg1_f_o,  jpcg2_f_o,  jpcg3_f_o,  jpcg4_f_o,  &
         jpcg5_f_o,  jpcg6_f_o,  jpcg7_f_o,  jpcg8_f_o,  &
         jpcg9_f_o,                                      &
         jopcg1_f_c, jopcg2_f_c, jopcg3_f_c, jopcg4_f_c, &
         jopcg5_f_c, jopcg6_f_c, jopcg7_f_c, jopcg8_f_c, &
         jopcg1_f_o, jopcg2_f_o, jopcg3_f_o, jopcg4_f_o, &
         jopcg5_f_o, jopcg6_f_o, jopcg7_f_o, jopcg8_f_o, &
         jant1_c,    jant2_c,    jant3_c,    jant4_c,    &
         jant1_o,    jant2_o,    jant3_o,    jant4_o,    &
         jbiog1_c,   jbiog2_c,   jbiog3_c,   jbiog4_c,   &
         jbiog1_o,   jbiog2_o,   jbiog3_o,   jbiog4_o,   &
         jsmpa,      jsmpbb,                             &
         jasoa1,     jasoa2,     jasoa3,     jasoa4,     &
         jasoaX,                                         &
         jbsoa1,     jbsoa2,     jbsoa3,     jbsoa4,     &
         jbsoaX,                                         &
         jglysoa_r1, jglysoa_r2, jglysoa_sfc, jglysoa_nh4, &
         jglysoa_oh
!        jiepox,     jgly,       jiepoxos,   jtetrol,    &
!        jtanv,      jsopnv,     jternv,     jseqnv,     &
!        jsianv

! arguments
      integer, intent(in) :: ipass


      if (ipass == 1) then

!                                                 jdust      = 26 ! insoluble - part of naercomp
      ipcg1_b_c_a   =   6 ;  ipcg1_b_c_g  =  6 ;  jpcg1_b_c  = 26
      ipcg2_b_c_a   =   7 ;  ipcg2_b_c_g  =  7 ;  jpcg2_b_c  = 27
      ipcg3_b_c_a   =   8 ;  ipcg3_b_c_g  =  8 ;  jpcg3_b_c  = 28
      ipcg4_b_c_a   =   9 ;  ipcg4_b_c_g  =  9 ;  jpcg4_b_c  = 29
      ipcg5_b_c_a   =  10 ;  ipcg5_b_c_g  = 10 ;  jpcg5_b_c  = 30
      ipcg6_b_c_a   =  11 ;  ipcg6_b_c_g  = 11 ;  jpcg6_b_c  = 31
      ipcg7_b_c_a   =  12 ;  ipcg7_b_c_g  = 12 ;  jpcg7_b_c  = 32
      ipcg8_b_c_a   =  13 ;  ipcg8_b_c_g  = 13 ;  jpcg8_b_c  = 33
      ipcg9_b_c_a   =  14 ;  ipcg9_b_c_g  = 14 ;  jpcg9_b_c  = 34
      ipcg1_b_o_a   =  15 ;  ipcg1_b_o_g  = 15 ;  jpcg1_b_o  = 35
      ipcg2_b_o_a   =  16 ;  ipcg2_b_o_g  = 16 ;  jpcg2_b_o  = 36
      ipcg3_b_o_a   =  17 ;  ipcg3_b_o_g  = 17 ;  jpcg3_b_o  = 37
      ipcg4_b_o_a   =  18 ;  ipcg4_b_o_g  = 18 ;  jpcg4_b_o  = 38
      ipcg5_b_o_a   =  19 ;  ipcg5_b_o_g  = 19 ;  jpcg5_b_o  = 39
      ipcg6_b_o_a   =  20 ;  ipcg6_b_o_g  = 20 ;  jpcg6_b_o  = 40
      ipcg7_b_o_a   =  21 ;  ipcg7_b_o_g  = 21 ;  jpcg7_b_o  = 41
      ipcg8_b_o_a   =  22 ;  ipcg8_b_o_g  = 22 ;  jpcg8_b_o  = 42
      ipcg9_b_o_a   =  23 ;  ipcg9_b_o_g  = 23 ;  jpcg9_b_o  = 43

      iopcg1_b_c_a  =  24 ;  iopcg1_b_c_g = 24 ;  jopcg1_b_c = 44
      iopcg2_b_c_a  =  25 ;  iopcg2_b_c_g = 25 ;  jopcg2_b_c = 45
      iopcg3_b_c_a  =  26 ;  iopcg3_b_c_g = 26 ;  jopcg3_b_c = 46
      iopcg4_b_c_a  =  27 ;  iopcg4_b_c_g = 27 ;  jopcg4_b_c = 47
      iopcg5_b_c_a  =  28 ;  iopcg5_b_c_g = 28 ;  jopcg5_b_c = 48
      iopcg6_b_c_a  =  29 ;  iopcg6_b_c_g = 29 ;  jopcg6_b_c = 49
      iopcg7_b_c_a  =  30 ;  iopcg7_b_c_g = 30 ;  jopcg7_b_c = 50
      iopcg8_b_c_a  =  31 ;  iopcg8_b_c_g = 31 ;  jopcg8_b_c = 51
      iopcg1_b_o_a  =  32 ;  iopcg1_b_o_g = 32 ;  jopcg1_b_o = 52
      iopcg2_b_o_a  =  33 ;  iopcg2_b_o_g = 33 ;  jopcg2_b_o = 53
      iopcg3_b_o_a  =  34 ;  iopcg3_b_o_g = 34 ;  jopcg3_b_o = 54
      iopcg4_b_o_a  =  35 ;  iopcg4_b_o_g = 35 ;  jopcg4_b_o = 55
      iopcg5_b_o_a  =  36 ;  iopcg5_b_o_g = 36 ;  jopcg5_b_o = 56
      iopcg6_b_o_a  =  37 ;  iopcg6_b_o_g = 37 ;  jopcg6_b_o = 57
      iopcg7_b_o_a  =  38 ;  iopcg7_b_o_g = 38 ;  jopcg7_b_o = 58
      iopcg8_b_o_a  =  39 ;  iopcg8_b_o_g = 39 ;  jopcg8_b_o = 59

      ipcg1_f_c_a   =  40 ;  ipcg1_f_c_g  = 40 ;  jpcg1_f_c  = 60
      ipcg2_f_c_a   =  41 ;  ipcg2_f_c_g  = 41 ;  jpcg2_f_c  = 61
      ipcg3_f_c_a   =  42 ;  ipcg3_f_c_g  = 42 ;  jpcg3_f_c  = 62
      ipcg4_f_c_a   =  43 ;  ipcg4_f_c_g  = 43 ;  jpcg4_f_c  = 63
      ipcg5_f_c_a   =  44 ;  ipcg5_f_c_g  = 44 ;  jpcg5_f_c  = 64
      ipcg6_f_c_a   =  45 ;  ipcg6_f_c_g  = 45 ;  jpcg6_f_c  = 65
      ipcg7_f_c_a   =  46 ;  ipcg7_f_c_g  = 46 ;  jpcg7_f_c  = 66
      ipcg8_f_c_a   =  47 ;  ipcg8_f_c_g  = 47 ;  jpcg8_f_c  = 67
      ipcg9_f_c_a   =  48 ;  ipcg9_f_c_g  = 48 ;  jpcg9_f_c  = 68
      ipcg1_f_o_a   =  49 ;  ipcg1_f_o_g  = 49 ;  jpcg1_f_o  = 69
      ipcg2_f_o_a   =  50 ;  ipcg2_f_o_g  = 50 ;  jpcg2_f_o  = 70
      ipcg3_f_o_a   =  51 ;  ipcg3_f_o_g  = 51 ;  jpcg3_f_o  = 71
      ipcg4_f_o_a   =  52 ;  ipcg4_f_o_g  = 52 ;  jpcg4_f_o  = 72
      ipcg5_f_o_a   =  53 ;  ipcg5_f_o_g  = 53 ;  jpcg5_f_o  = 73
      ipcg6_f_o_a   =  54 ;  ipcg6_f_o_g  = 54 ;  jpcg6_f_o  = 74
      ipcg7_f_o_a   =  55 ;  ipcg7_f_o_g  = 55 ;  jpcg7_f_o  = 75
      ipcg8_f_o_a   =  56 ;  ipcg8_f_o_g  = 56 ;  jpcg8_f_o  = 76
      ipcg9_f_o_a   =  57 ;  ipcg9_f_o_g  = 57 ;  jpcg9_f_o  = 77

      iopcg1_f_c_a  =  58 ;  iopcg1_f_c_g = 58 ;  jopcg1_f_c = 78
      iopcg2_f_c_a  =  59 ;  iopcg2_f_c_g = 59 ;  jopcg2_f_c = 79
      iopcg3_f_c_a  =  60 ;  iopcg3_f_c_g = 60 ;  jopcg3_f_c = 80
      iopcg4_f_c_a  =  61 ;  iopcg4_f_c_g = 61 ;  jopcg4_f_c = 81
      iopcg5_f_c_a  =  62 ;  iopcg5_f_c_g = 62 ;  jopcg5_f_c = 82
      iopcg6_f_c_a  =  63 ;  iopcg6_f_c_g = 63 ;  jopcg6_f_c = 83
      iopcg7_f_c_a  =  64 ;  iopcg7_f_c_g = 64 ;  jopcg7_f_c = 84
      iopcg8_f_c_a  =  65 ;  iopcg8_f_c_g = 65 ;  jopcg8_f_c = 85
      iopcg1_f_o_a  =  66 ;  iopcg1_f_o_g = 66 ;  jopcg1_f_o = 86
      iopcg2_f_o_a  =  67 ;  iopcg2_f_o_g = 67 ;  jopcg2_f_o = 87
      iopcg3_f_o_a  =  68 ;  iopcg3_f_o_g = 68 ;  jopcg3_f_o = 88
      iopcg4_f_o_a  =  69 ;  iopcg4_f_o_g = 69 ;  jopcg4_f_o = 89
      iopcg5_f_o_a  =  70 ;  iopcg5_f_o_g = 70 ;  jopcg5_f_o = 90
      iopcg6_f_o_a  =  71 ;  iopcg6_f_o_g = 71 ;  jopcg6_f_o = 91
      iopcg7_f_o_a  =  72 ;  iopcg7_f_o_g = 72 ;  jopcg7_f_o = 92
      iopcg8_f_o_a  =  73 ;  iopcg8_f_o_g = 73 ;  jopcg8_f_o = 93

      ismpa_a       =  74 ;  ismpa_g      = 74 ;  jsmpa      = 94
      ismpbb_a      =  75 ;  ismpbb_g     = 75 ;  jsmpbb     = 95

      iant1_c_a     =  76 ;  iant1_c_g    = 76 ;  jant1_c    = 96
      iant2_c_a     =  77 ;  iant2_c_g    = 77 ;  jant2_c    = 97
      iant3_c_a     =  78 ;  iant3_c_g    = 78 ;  jant3_c    = 98
      iant4_c_a     =  79 ;  iant4_c_g    = 79 ;  jant4_c    = 99
      iant1_o_a     =  80 ;  iant1_o_g    = 80 ;  jant1_o    =100
      iant2_o_a     =  81 ;  iant2_o_g    = 81 ;  jant2_o    =101
      iant3_o_a     =  82 ;  iant3_o_g    = 82 ;  jant3_o    =102
      iant4_o_a     =  83 ;  iant4_o_g    = 83 ;  jant4_o    =103
      ibiog1_c_a    =  84 ;  ibiog1_c_g   = 84 ;  jbiog1_c   =104
      ibiog2_c_a    =  85 ;  ibiog2_c_g   = 85 ;  jbiog2_c   =105
      ibiog3_c_a    =  86 ;  ibiog3_c_g   = 86 ;  jbiog3_c   =106
      ibiog4_c_a    =  87 ;  ibiog4_c_g   = 87 ;  jbiog4_c   =107
      ibiog1_o_a    =  88 ;  ibiog1_o_g   = 88 ;  jbiog1_o   =108
      ibiog2_o_a    =  89 ;  ibiog2_o_g   = 89 ;  jbiog2_o   =109
      ibiog3_o_a    =  90 ;  ibiog3_o_g   = 90 ;  jbiog3_o   =110
      ibiog4_o_a    =  91 ;  ibiog4_o_g   = 91 ;  jbiog4_o   =111

      iasoaX_a      =  92 ;  iasoaX_g     = 92 ;  jasoaX     =112
      iasoa1_a      =  93 ;  iasoa1_g     = 93 ;  jasoa1     =113
      iasoa2_a      =  94 ;  iasoa2_g     = 94 ;  jasoa2     =114
      iasoa3_a      =  95 ;  iasoa3_g     = 95 ;  jasoa3     =115
      iasoa4_a      =  96 ;  iasoa4_g     = 96 ;  jasoa4     =116
      ibsoaX_a      =  97 ;  ibsoaX_g     = 97 ;  jbsoaX     =117
      ibsoa1_a      =  98 ;  ibsoa1_g     = 98 ;  jbsoa1     =118
      ibsoa2_a      =  99 ;  ibsoa2_g     = 99 ;  jbsoa2     =119
      ibsoa3_a      = 100 ;  ibsoa3_g     =100 ;  jbsoa3     =120
      ibsoa4_a      = 101 ;  ibsoa4_g     =101 ;  jbsoa4     =121

      iglysoa_r1_a  = 102 ;                       jglysoa_r1 =122
      iglysoa_r2_a  = 103 ;                       jglysoa_r2 =123
      iglysoa_sfc_a = 104 ;                       jglysoa_sfc=124
      iglysoa_nh4_a = 105 ;                       jglysoa_nh4=125
      iglysoa_oh_a  = 106 ;                       jglysoa_oh =126

                                                  jh2o       =127 ! water - part of naercomp

                             in2o5_g      =102  ! ioa --> NO3-
                             iclno2_g     =103  ! ioa N2O5+Cl- -->
                             igly         =104
                             iho          =105
!                            ico2_g       = 14  ! currently not used

      ico3_a        = 107  
      ina_a         = 108  
      ica_a         = 109  
      ioin_a        = 110  
      ioc_a         = 111  
      ibc_a         = 112  

!     iiepox_a      =  92 ;  iiepox_g     = 92 ;  jiepox     =113
!     igly_a        =  93 ;  igly_g       = 93 ;  jgly       =114
!                            in2o5_g      = 94 
!                            iclno2_g     = 95 
!     iiepoxos_a    =  94                      ;  jiepoxos   =115
!     itetrol_a     =  95                      ;  jtetrol    =116
!     itanv_a       =  96                      ;  jtanv      =117
!     isopnv_a      =  97                      ;  jsopnv     =118
!     iternv_a      =  98                      ;  jternv     =119
!     iseqnv_a      =  99                      ;  jseqnv     =120
!     isianv_a      = 100                      ;  jsianv     =121
!                                                 jh2o       =122 ! water - part of naercomp
!     idust_a       = 105  
!     itr1r1_a      = 108                      ;  jtr1r1     =123
!     itr1r2_a      = 109                      ;  jtr1r2     =124
!     itr1r3_a      = 110                      ;  jtr1r3     =125
!     itr1r4_a      = 111                      ;  jtr1r4     =126

      return
      end if ! (ipass == 1) then


      if (ipass == 2) then

! names of aer species
!     aer_name(idust_a)      = "dust"
!     aer_name(itr1r1_a)     = "tr1r1"
!     aer_name(itr1r2_a)     = "tr1r2"
!     aer_name(itr1r3_a)     = "tr1r3"
!     aer_name(itr1r4_a)     = "tr1r4"
!     aer_name(itanv_a)      = "tanv"
!     aer_name(isopnv_a)     = "sopnv"
!     aer_name(iternv_a)     = "ternv"
!     aer_name(iseqnv_a)     = "seqnv"
!     aer_name(isianv_a)     = "sianv"
      aer_name(ipcg1_b_c_a)  = "pcg1_b_c"
      aer_name(ipcg2_b_c_a)  = "pcg2_b_c"
      aer_name(ipcg3_b_c_a)  = "pcg3_b_c"
      aer_name(ipcg4_b_c_a)  = "pcg4_b_c"
      aer_name(ipcg5_b_c_a)  = "pcg5_b_c"
      aer_name(ipcg6_b_c_a)  = "pcg6_b_c"
      aer_name(ipcg7_b_c_a)  = "pcg7_b_c"
      aer_name(ipcg8_b_c_a)  = "pcg8_b_c"
      aer_name(ipcg9_b_c_a)  = "pcg9_b_c"
      aer_name(ipcg1_b_o_a)  = "pcg1_b_o"
      aer_name(ipcg2_b_o_a)  = "pcg2_b_o"
      aer_name(ipcg3_b_o_a)  = "pcg3_b_o"
      aer_name(ipcg4_b_o_a)  = "pcg4_b_o"
      aer_name(ipcg5_b_o_a)  = "pcg5_b_o"
      aer_name(ipcg6_b_o_a)  = "pcg6_b_o"
      aer_name(ipcg7_b_o_a)  = "pcg7_b_o"
      aer_name(ipcg8_b_o_a)  = "pcg8_b_o"
      aer_name(ipcg9_b_o_a)  = "pcg9_b_o"
      aer_name(iopcg1_b_c_a) = "opcg1_b_c"
      aer_name(iopcg2_b_c_a) = "opcg2_b_c"
      aer_name(iopcg3_b_c_a) = "opcg3_b_c"
      aer_name(iopcg4_b_c_a) = "opcg4_b_c"
      aer_name(iopcg5_b_c_a) = "opcg5_b_c"
      aer_name(iopcg6_b_c_a) = "opcg6_b_c"
      aer_name(iopcg7_b_c_a) = "opcg7_b_c"
      aer_name(iopcg8_b_c_a) = "opcg8_b_c"
      aer_name(iopcg1_b_o_a) = "opcg1_b_o"
      aer_name(iopcg2_b_o_a) = "opcg2_b_o"
      aer_name(iopcg3_b_o_a) = "opcg3_b_o"
      aer_name(iopcg4_b_o_a) = "opcg4_b_o"
      aer_name(iopcg5_b_o_a) = "opcg5_b_o"
      aer_name(iopcg6_b_o_a) = "opcg6_b_o"
      aer_name(iopcg7_b_o_a) = "opcg7_b_o"
      aer_name(iopcg8_b_o_a) = "opcg8_b_o"
      aer_name(ipcg1_f_c_a)  = "pcg1_f_c"
      aer_name(ipcg2_f_c_a)  = "pcg2_f_c"
      aer_name(ipcg3_f_c_a)  = "pcg3_f_c"
      aer_name(ipcg4_f_c_a)  = "pcg4_f_c"
      aer_name(ipcg5_f_c_a)  = "pcg5_f_c"
      aer_name(ipcg6_f_c_a)  = "pcg6_f_c"
      aer_name(ipcg7_f_c_a)  = "pcg7_f_c"
      aer_name(ipcg8_f_c_a)  = "pcg8_f_c"
      aer_name(ipcg9_f_c_a)  = "pcg9_f_c"
      aer_name(ipcg1_f_o_a)  = "pcg1_f_o"
      aer_name(ipcg2_f_o_a)  = "pcg2_f_o"
      aer_name(ipcg3_f_o_a)  = "pcg3_f_o"
      aer_name(ipcg4_f_o_a)  = "pcg4_f_o"
      aer_name(ipcg5_f_o_a)  = "pcg5_f_o"
      aer_name(ipcg6_f_o_a)  = "pcg6_f_o"
      aer_name(ipcg7_f_o_a)  = "pcg7_f_o"
      aer_name(ipcg8_f_o_a)  = "pcg8_f_o"
      aer_name(ipcg9_f_o_a)  = "pcg9_f_o"
      aer_name(iopcg1_f_c_a) = "opcg1_f_c"
      aer_name(iopcg2_f_c_a) = "opcg2_f_c"
      aer_name(iopcg3_f_c_a) = "opcg3_f_c"
      aer_name(iopcg4_f_c_a) = "opcg4_f_c"
      aer_name(iopcg5_f_c_a) = "opcg5_f_c"
      aer_name(iopcg6_f_c_a) = "opcg6_f_c"
      aer_name(iopcg7_f_c_a) = "opcg7_f_c"
      aer_name(iopcg8_f_c_a) = "opcg8_f_c"
      aer_name(iopcg1_f_o_a) = "opcg1_f_o"
      aer_name(iopcg2_f_o_a) = "opcg2_f_o"
      aer_name(iopcg3_f_o_a) = "opcg3_f_o"
      aer_name(iopcg4_f_o_a) = "opcg4_f_o"
      aer_name(iopcg5_f_o_a) = "opcg5_f_o"
      aer_name(iopcg6_f_o_a) = "opcg6_f_o"
      aer_name(iopcg7_f_o_a) = "opcg7_f_o"
      aer_name(iopcg8_f_o_a) = "opcg8_f_o"
      aer_name(ismpa_a)      = "smpa"
      aer_name(ismpbb_a)     = "smpbb"
!     aer_name(igly_a)       = "gly"
!     aer_name(iiepox_a)     = "iepox"
!     aer_name(iiepoxos_a)   = "iepoxos"
!     aer_name(itetrol_a)    = "tetrol"
      aer_name(iant1_c_a)    = "ant1_c"
      aer_name(iant2_c_a)    = "ant2_c"
      aer_name(iant3_c_a)    = "ant3_c"
      aer_name(iant4_c_a)    = "ant4_c"
      aer_name(iant1_o_a)    = "ant1_o"
      aer_name(iant2_o_a)    = "ant2_o"
      aer_name(iant3_o_a)    = "ant3_o"
      aer_name(iant4_o_a)    = "ant4_o"
      aer_name(ibiog1_c_a)   = "biog1_c"
      aer_name(ibiog2_c_a)   = "biog2_c"
      aer_name(ibiog3_c_a)   = "biog3_c"
      aer_name(ibiog4_c_a)   = "biog4_c"
      aer_name(ibiog1_o_a)   = "biog1_o"
      aer_name(ibiog2_o_a)   = "biog2_o"
      aer_name(ibiog3_o_a)   = "biog3_o"
      aer_name(ibiog4_o_a)   = "biog4_o"

      aer_name(iglysoa_r1_a) ="glysoa_r1"
      aer_name(iglysoa_r2_a) ="glysoa_r2"
      aer_name(iglysoa_sfc_a)="glysoa_sfc"
      aer_name(iglysoa_nh4_a)="glysoa_nh4"
      aer_name(iglysoa_oh_a) ="glysoa_oh"
      aer_name(iasoaX_a)     ="asoaX"
      aer_name(iasoa1_a)     ="asoa1"
      aer_name(iasoa2_a)     ="asoa2"
      aer_name(iasoa3_a)     ="asoa3"
      aer_name(iasoa4_a)     ="asoa4"
      aer_name(ibsoaX_a)     ="bsoaX"
      aer_name(ibsoa1_a)     ="bsoa1"
      aer_name(ibsoa2_a)     ="bsoa2"
      aer_name(ibsoa3_a)     ="bsoa3"
      aer_name(ibsoa4_a)     ="bsoa4"

! names of gas species
      gas_name(ipcg1_b_c_g)  = "pcg1_b_c"
      gas_name(ipcg2_b_c_g)  = "pcg2_b_c"
      gas_name(ipcg3_b_c_g)  = "pcg3_b_c"
      gas_name(ipcg4_b_c_g)  = "pcg4_b_c"
      gas_name(ipcg5_b_c_g)  = "pcg5_b_c"
      gas_name(ipcg6_b_c_g)  = "pcg6_b_c"
      gas_name(ipcg7_b_c_g)  = "pcg7_b_c"
      gas_name(ipcg8_b_c_g)  = "pcg8_b_c"
      gas_name(ipcg9_b_c_g)  = "pcg9_b_c"
      gas_name(ipcg1_b_o_g)  = "pcg1_b_o"
      gas_name(ipcg2_b_o_g)  = "pcg2_b_o"
      gas_name(ipcg3_b_o_g)  = "pcg3_b_o"
      gas_name(ipcg4_b_o_g)  = "pcg4_b_o"
      gas_name(ipcg5_b_o_g)  = "pcg5_b_o"
      gas_name(ipcg6_b_o_g)  = "pcg6_b_o"
      gas_name(ipcg7_b_o_g)  = "pcg7_b_o"
      gas_name(ipcg8_b_o_g)  = "pcg8_b_o"
      gas_name(ipcg9_b_o_g)  = "pcg9_b_o"
      gas_name(iopcg1_b_c_g) = "opcg1_b_c"
      gas_name(iopcg2_b_c_g) = "opcg2_b_c"
      gas_name(iopcg3_b_c_g) = "opcg3_b_c"
      gas_name(iopcg4_b_c_g) = "opcg4_b_c"
      gas_name(iopcg5_b_c_g) = "opcg5_b_c"
      gas_name(iopcg6_b_c_g) = "opcg6_b_c"
      gas_name(iopcg7_b_c_g) = "opcg7_b_c"
      gas_name(iopcg8_b_c_g) = "opcg8_b_c"
      gas_name(iopcg1_b_o_g) = "opcg1_b_o"
      gas_name(iopcg2_b_o_g) = "opcg2_b_o"
      gas_name(iopcg3_b_o_g) = "opcg3_b_o"
      gas_name(iopcg4_b_o_g) = "opcg4_b_o"
      gas_name(iopcg5_b_o_g) = "opcg5_b_o"
      gas_name(iopcg6_b_o_g) = "opcg6_b_o"
      gas_name(iopcg7_b_o_g) = "opcg7_b_o"
      gas_name(iopcg8_b_o_g) = "opcg8_b_o"
      gas_name(ipcg1_f_c_g)  = "pcg1_f_c"
      gas_name(ipcg2_f_c_g)  = "pcg2_f_c"
      gas_name(ipcg3_f_c_g)  = "pcg3_f_c"
      gas_name(ipcg4_f_c_g)  = "pcg4_f_c"
      gas_name(ipcg5_f_c_g)  = "pcg5_f_c"
      gas_name(ipcg6_f_c_g)  = "pcg6_f_c"
      gas_name(ipcg7_f_c_g)  = "pcg7_f_c"
      gas_name(ipcg8_f_c_g)  = "pcg8_f_c"
      gas_name(ipcg9_f_c_g)  = "pcg9_f_c"
      gas_name(ipcg1_f_o_g)  = "pcg1_f_o"
      gas_name(ipcg2_f_o_g)  = "pcg2_f_o"
      gas_name(ipcg3_f_o_g)  = "pcg3_f_o"
      gas_name(ipcg4_f_o_g)  = "pcg4_f_o"
      gas_name(ipcg5_f_o_g)  = "pcg5_f_o"
      gas_name(ipcg6_f_o_g)  = "pcg6_f_o"
      gas_name(ipcg7_f_o_g)  = "pcg7_f_o"
      gas_name(ipcg8_f_o_g)  = "pcg8_f_o"
      gas_name(ipcg9_f_o_g)  = "pcg9_f_o"
      gas_name(iopcg1_f_c_g) = "opcg1_f_c"
      gas_name(iopcg2_f_c_g) = "opcg2_f_c"
      gas_name(iopcg3_f_c_g) = "opcg3_f_c"
      gas_name(iopcg4_f_c_g) = "opcg4_f_c"
      gas_name(iopcg5_f_c_g) = "opcg5_f_c"
      gas_name(iopcg6_f_c_g) = "opcg6_f_c"
      gas_name(iopcg7_f_c_g) = "opcg7_f_c"
      gas_name(iopcg8_f_c_g) = "opcg8_f_c"
      gas_name(iopcg1_f_o_g) = "opcg1_f_o"
      gas_name(iopcg2_f_o_g) = "opcg2_f_o"
      gas_name(iopcg3_f_o_g) = "opcg3_f_o"
      gas_name(iopcg4_f_o_g) = "opcg4_f_o"
      gas_name(iopcg5_f_o_g) = "opcg5_f_o"
      gas_name(iopcg6_f_o_g) = "opcg6_f_o"
      gas_name(iopcg7_f_o_g) = "opcg7_f_o"
      gas_name(iopcg8_f_o_g) = "opcg8_f_o"
      gas_name(ismpa_g)      = "smpa"
      gas_name(ismpbb_g)     = "smpbb"
!     gas_name(igly_g)       = "gly"
!     gas_name(iiepox_g)     = "iepox"
      gas_name(iant1_c_g)    = "ant1_c"
      gas_name(iant2_c_g)    = "ant2_c"
      gas_name(iant3_c_g)    = "ant3_c"
      gas_name(iant4_c_g)    = "ant4_c"
      gas_name(iant1_o_g)    = "ant1_o"
      gas_name(iant2_o_g)    = "ant2_o"
      gas_name(iant3_o_g)    = "ant3_o"
      gas_name(iant4_o_g)    = "ant4_o"
      gas_name(ibiog1_c_g)   = "biog1_c"
      gas_name(ibiog2_c_g)   = "biog2_c"
      gas_name(ibiog3_c_g)   = "biog3_c"
      gas_name(ibiog4_c_g)   = "biog4_c"
      gas_name(ibiog1_o_g)   = "biog1_o"
      gas_name(ibiog2_o_g)   = "biog2_o"
      gas_name(ibiog3_o_g)   = "biog3_o"
      gas_name(ibiog4_o_g)   = "biog4_o"
      gas_name(in2o5_g)      = "n2o5 "
      gas_name(iclno2_g)     = "clno2"
      
      gas_name(iasoaX_g)="asoaX"
      gas_name(iasoa1_g)="asoa1"
      gas_name(iasoa2_g)="asoa2"
      gas_name(iasoa3_g)="asoa3"
      gas_name(iasoa4_g)="asoa4"
      gas_name(ibsoaX_g)="bsoaX"
      gas_name(ibsoa1_g)="bsoa1"
      gas_name(ibsoa2_g)="bsoa2"
      gas_name(ibsoa3_g)="bsoa3"
      gas_name(ibsoa4_g)="bsoa4"

! densities of compounds in g/cc
      dens_comp_a(:)        = 1.0       ! default

      dens_comp_a(jh2o)     = 1.0
      dens_comp_a(jnh4so4)  = 1.8
      dens_comp_a(jlvcite)  = 1.8
      dens_comp_a(jnh4hso4) = 1.8
      dens_comp_a(jnh4msa)  = 1.8       ! assumed same as nh4hso4
      dens_comp_a(jnh4no3)  = 1.7
      dens_comp_a(jnh4cl)   = 1.5
      dens_comp_a(jnacl)    = 2.2
      dens_comp_a(jnano3)   = 2.2
      dens_comp_a(jna2so4)  = 2.2
      dens_comp_a(jna3hso4) = 2.2
      dens_comp_a(jnahso4)  = 2.2
      dens_comp_a(jnamsa)   = 2.2       ! assumed same as nahso4
      dens_comp_a(jcaso4)   = 2.6
      dens_comp_a(jcamsa2)  = 2.6       ! assumed same as caso4
      dens_comp_a(jcano3)   = 2.6
      dens_comp_a(jcacl2)   = 2.6
      dens_comp_a(jcaco3)   = 2.6
      dens_comp_a(jh2so4)   = 1.8
      dens_comp_a(jhhso4)   = 1.8
      dens_comp_a(jhno3)    = 1.8
      dens_comp_a(jhcl)     = 1.8
      dens_comp_a(jmsa)     = 1.8       ! assumed same as h2so4
      dens_comp_a(joc)      = 1.0
      dens_comp_a(jbc)      = 1.8
      dens_comp_a(join)     = 2.6

      dens_comp_a(iasoaX_a) = 1.5
      dens_comp_a(iasoa1_a) = 1.5
      dens_comp_a(iasoa2_a) = 1.5
      dens_comp_a(iasoa3_a) = 1.5
      dens_comp_a(iasoa4_a) = 1.5
      dens_comp_a(ibsoaX_a) = 1.5
      dens_comp_a(ibsoa1_a) = 1.5
      dens_comp_a(ibsoa2_a) = 1.5
      dens_comp_a(ibsoa3_a) = 1.5
      dens_comp_a(ibsoa4_a) = 1.5

!     dens_comp_a(jdust)    = 2.6
!     dens_comp_a(jtr1r1)   = 2.6
!     dens_comp_a(jtr1r2)   = 2.6
!     dens_comp_a(jtr1r3)   = 2.6
!     dens_comp_a(jtr1r4)   = 2.6
!     dens_comp_a(jtanv)    = 1.0


! molecular weights of generic aerosol species
      mw_aer_mac(:)        = 250.0 ! default

      mw_aer_mac(iso4_a)   =  96.0
      mw_aer_mac(ino3_a)   =  62.0
      mw_aer_mac(icl_a)    =  35.5
      mw_aer_mac(imsa_a)   =  95.0 ! ch3so3
      mw_aer_mac(ico3_a)   =  60.0
      mw_aer_mac(inh4_a)   =  18.0
      mw_aer_mac(ina_a)    =  23.0
      mw_aer_mac(ica_a)    =  40.0
      mw_aer_mac(ioin_a)   =   1.0  ! not used
!     mw_aer_mac(idust_a)  =   1.0  ! not used
!     mw_aer_mac(itr1r1_a) =   1.0  ! not used
!     mw_aer_mac(itr1r2_a) =   1.0  ! not used
!     mw_aer_mac(itr1r3_a) =   1.0  ! not used
!     mw_aer_mac(itr1r4_a) =   1.0  ! not used
      mw_aer_mac(ibc_a)    =   1.0  ! not used

      mw_aer_mac(ioc_a)      = 250.0
!     mw_aer_mac(igly_a)     =  58.0
!     mw_aer_mac(iiepox_a)   = 118.0
!     mw_aer_mac(iiepoxos_a) = 216.0
!     mw_aer_mac(itetrol_a)  = 136.0


! molecular weights of compounds
      mw_comp_a(:)        = 250.0 ! default

      mw_comp_a(jh2o)     =  18.0
      mw_comp_a(jnh4so4)  = 132.0
      mw_comp_a(jlvcite)  = 247.0
      mw_comp_a(jnh4hso4) = 115.0
      mw_comp_a(jnh4msa)  = 113.0
      mw_comp_a(jnh4no3)  =  80.0
      mw_comp_a(jnh4cl)   =  53.5
      mw_comp_a(jnacl)    =  58.5
      mw_comp_a(jnano3)   =  85.0
      mw_comp_a(jna2so4)  = 142.0
      mw_comp_a(jna3hso4) = 262.0
      mw_comp_a(jnahso4)  = 120.0
      mw_comp_a(jnamsa)   = 118.0
      mw_comp_a(jcaso4)   = 136.0
      mw_comp_a(jcamsa2)  = 230.0
      mw_comp_a(jcano3)   = 164.0
      mw_comp_a(jcacl2)   = 111.0
      mw_comp_a(jcaco3)   = 100.0
      mw_comp_a(jh2so4)   =  98.0
      mw_comp_a(jhhso4)   =  98.0
      mw_comp_a(jhno3)    =  63.0
      mw_comp_a(jhcl)     =  36.5
      mw_comp_a(jmsa)     =  96.0
      mw_comp_a(joc)      = 250.0
      mw_comp_a(jbc)      =   1.0
      mw_comp_a(join)     =   1.0
!     mw_comp_a(jdust)    =   1.0

!     mw_comp_a(jtr1r1)   =   1.0
!     mw_comp_a(jtr1r2)   =   1.0
!     mw_comp_a(jtr1r3)   =   1.0
!     mw_comp_a(jtr1r4)   =   1.0
!     mw_comp_a(jgly)     =  58.0
!     mw_comp_a(jiepox)   = 118.0
!     mw_comp_a(jiepoxos) = 216.0
!     mw_comp_a(jtetrol)  = 136.0


! densities of generic aerosol species
      dens_aer_mac(:)        = 1.0        ! default

      dens_aer_mac(iso4_a)   = 1.8        ! used
      dens_aer_mac(ino3_a)   = 1.8        ! used
      dens_aer_mac(icl_a)    = 2.2        ! used
      dens_aer_mac(imsa_a)   = 1.8        ! used
      dens_aer_mac(ico3_a)   = 2.6        ! used
      dens_aer_mac(inh4_a)   = 1.8        ! used
      dens_aer_mac(ina_a)    = 2.2        ! used
      dens_aer_mac(ica_a)    = 2.6        ! used
      dens_aer_mac(ioin_a)   = 2.6        ! used
!     dens_aer_mac(idust_a)  = 2.6       ! used
!     dens_aer_mac(itr1r1_a) = 2.6      ! used
!     dens_aer_mac(itr1r2_a) = 2.6      ! used
!     dens_aer_mac(itr1r3_a) = 2.6      ! used
!     dens_aer_mac(itr1r4_a) = 2.6      ! used
      dens_aer_mac(ioc_a)    = 1.0        ! used
      dens_aer_mac(ibc_a)    = 1.7        ! used

      dens_aer_mac(iasoa1_a) = 1.5
      dens_aer_mac(iasoa2_a) = 1.5
      dens_aer_mac(iasoa3_a) = 1.5
      dens_aer_mac(iasoa4_a) = 1.5
      dens_aer_mac(iasoaX_a) = 1.5
      dens_aer_mac(ibsoa1_a) = 1.5
      dens_aer_mac(ibsoa2_a) = 1.5
      dens_aer_mac(ibsoa3_a) = 1.5
      dens_aer_mac(ibsoa4_a) = 1.5
      dens_aer_mac(ibsoaX_a) = 1.5


! kappa_aer_mac not used in previous wrfchem mosaic


! partial molar volumes of condensing species
      partial_molar_vol(:)        = 250.0  ! default

      partial_molar_vol(ih2so4_g) =  51.83
      partial_molar_vol(ihno3_g)  =  31.45
      partial_molar_vol(ihcl_g)   =  20.96
      partial_molar_vol(inh3_g)   =  24.03
      partial_molar_vol(imsa_g)   =  53.33

!     partial_molar_vol(iiepox_g) = 118.0
!     partial_molar_vol(igly_g)   =  58.0
      partial_molar_vol(in2o5_g)  = 200.0       ! assumed...
      partial_molar_vol(iclno2_g) = 200.0       ! assumed...


! used to calculate diffusivities of condensing gases
! (other values set in module_mosaic_init_aerpar are ok)
      v_molar_gas(in2o5_g)  = 60.40
      v_molar_gas(iclno2_g) = 52.70


! molecular weights of condensing gases
      mw_gas(1:ngas_aerchtot) = 250.0  ! default
      mw_gas(ih2so4_g) = 98.0
      mw_gas(ihno3_g)  = 63.0
      mw_gas(ihcl_g)   = 36.5
      mw_gas(inh3_g)   = 17.0
      mw_gas(imsa_g)   = 96.0
      mw_gas(in2o5_g)  = 108.0
      mw_gas(iclno2_g) = 81.5
!     mw_gas(igly)=58.0
!     mw_gas(iho)=17.0


      return
      end if ! (ipass == 2) then


      return
      end subroutine soa_vbs_load_params


!-------------------------------------------------------------------------------
      subroutine soa_vbs_update_thermcons( t_k, po_soa, sat_soa )

      use module_data_mosaic_aero, only: &
         msoa_vbs_info, &
         ngas_ioa, ngas_soa, ngas_volatile, &
         ipcg1_b_c_g,  ipcg2_b_c_g,  ipcg3_b_c_g,  ipcg4_b_c_g,  &
         ipcg5_b_c_g,  ipcg6_b_c_g,  ipcg7_b_c_g,  ipcg8_b_c_g,  &
         ipcg9_b_c_g,                                            &
         ipcg1_b_o_g,  ipcg2_b_o_g,  ipcg3_b_o_g,  ipcg4_b_o_g,  &
         ipcg5_b_o_g,  ipcg6_b_o_g,  ipcg7_b_o_g,  ipcg8_b_o_g,  &
         ipcg9_b_o_g,                                            &
         iopcg1_b_c_g, iopcg2_b_c_g, iopcg3_b_c_g, iopcg4_b_c_g, &
         iopcg5_b_c_g, iopcg6_b_c_g, iopcg7_b_c_g, iopcg8_b_c_g, &
         iopcg1_b_o_g, iopcg2_b_o_g, iopcg3_b_o_g, iopcg4_b_o_g, &
         iopcg5_b_o_g, iopcg6_b_o_g, iopcg7_b_o_g, iopcg8_b_o_g, &
         ipcg1_f_c_g,  ipcg2_f_c_g,  ipcg3_f_c_g,  ipcg4_f_c_g,  &
         ipcg5_f_c_g,  ipcg6_f_c_g,  ipcg7_f_c_g,  ipcg8_f_c_g,  &
         ipcg9_f_c_g,                                            &
         ipcg1_f_o_g,  ipcg2_f_o_g,  ipcg3_f_o_g,  ipcg4_f_o_g,  &
         ipcg5_f_o_g,  ipcg6_f_o_g,  ipcg7_f_o_g,  ipcg8_f_o_g,  &
         ipcg9_f_o_g,                                            &
         iopcg1_f_c_g, iopcg2_f_c_g, iopcg3_f_c_g, iopcg4_f_c_g, &
         iopcg5_f_c_g, iopcg6_f_c_g, iopcg7_f_c_g, iopcg8_f_c_g, &
         iopcg1_f_o_g, iopcg2_f_o_g, iopcg3_f_o_g, iopcg4_f_o_g, &
         iopcg5_f_o_g, iopcg6_f_o_g, iopcg7_f_o_g, iopcg8_f_o_g, &
         iant1_c_g,    iant2_c_g,    iant3_c_g,    iant4_c_g,    &
         iant1_o_g,    iant2_o_g,    iant3_o_g,    iant4_o_g,    &
         ibiog1_c_g,   ibiog2_c_g,   ibiog3_c_g,   ibiog4_c_g,   &
         ibiog1_o_g,   ibiog2_o_g,   ibiog3_o_g,   ibiog4_o_g,   &
         ismpa_g,      ismpbb_g,                                 &
         iasoa1_g,     iasoa2_g,     iasoa3_g,     iasoa4_g,     &
         iasoaX_g,                                               &
         ibsoa1_g,     ibsoa2_g,     ibsoa3_g,     ibsoa4_g,     &
         ibsoaX_g
!        iiepox_g,     igly_g

! arguments
      real(r8), intent(in   ) :: t_k
      real(r8), intent(inout) :: po_soa(ngas_volatile)
      real(r8), intent(inout) :: sat_soa(ngas_volatile)

! local variables
      integer :: itmpa, iv
      integer :: vbs_nbin(1)


      vbs_nbin(1) = msoa_vbs_info(1)

!
! vapor pressures of soa species
!
      po_soa = 1.0e5_r8  ! this default value gives csat=6.1e9 ug/m^3 (~1e9 ppbv) for 298K and MW=150

      if (vbs_nbin(1) .eq. 9) then ! default

      po_soa(ipcg1_b_c_g)  = fn_po(9.91d-9, 83.0d0, t_k) ! [pascal]
      po_soa(ipcg2_b_c_g)  = fn_po(9.91d-7, 106.0d0, t_k) ! [pascal]
      po_soa(ipcg3_b_c_g)  = fn_po(9.91d-6, 100.0d0, t_k) ! [pascal]
      po_soa(ipcg4_b_c_g)  = fn_po(9.91d-5, 94.0d0, t_k) ! [pascal]
      po_soa(ipcg5_b_c_g)  = fn_po(9.91d-4, 88.0d0, t_k) ! [pascal]
      po_soa(ipcg6_b_c_g)  = fn_po(9.91d-3, 82.0d0, t_k) ! [pascal]
      po_soa(ipcg7_b_c_g)  = fn_po(9.91d-2, 64.0d0, t_k) ! [pascal]
      po_soa(ipcg8_b_c_g)  = fn_po(9.91d-1, 70.0d0, t_k) ! [pascal]
      po_soa(ipcg9_b_c_g)  = fn_po(9.91d0,  64.0d0, t_k) ! [pascal]
      po_soa(iopcg1_b_c_g) = fn_po(9.91d-8, 112.0d0, t_k) ! [pascal]
      po_soa(iopcg2_b_c_g) = fn_po(9.91d-7, 106.0d0, t_k) ! [pascal]
      po_soa(iopcg3_b_c_g) = fn_po(9.91d-6, 100.0d0, t_k) ! [pascal]
      po_soa(iopcg4_b_c_g) = fn_po(9.91d-5, 94.0d0, t_k) ! [pascal]
      po_soa(iopcg5_b_c_g) = fn_po(9.91d-4, 88.0d0, t_k) ! [pascal]
      po_soa(iopcg6_b_c_g) = fn_po(9.91d-3, 82.0d0, t_k) ! [pascal]
      po_soa(iopcg7_b_c_g) = fn_po(9.91d-2, 76.0d0, t_k) ! [pascal]
      po_soa(iopcg8_b_c_g) = fn_po(9.91d-1, 70.0d0, t_k) ! [pascal]

      po_soa(ipcg1_b_o_g)  = fn_po(9.91d-8, 112.0d0, t_k) ! [pascal]
      po_soa(ipcg2_b_o_g)  = fn_po(9.91d-7, 106.0d0, t_k) ! [pascal]
      po_soa(ipcg3_b_o_g)  = fn_po(9.91d-6, 100.0d0, t_k) ! [pascal]
      po_soa(ipcg4_b_o_g)  = fn_po(9.91d-5, 94.0d0, t_k) ! [pascal]
      po_soa(ipcg5_b_o_g)  = fn_po(9.91d-4, 88.0d0, t_k) ! [pascal]
      po_soa(ipcg6_b_o_g)  = fn_po(9.91d-3, 82.0d0, t_k) ! [pascal]
      po_soa(ipcg7_b_o_g)  = fn_po(9.91d-2, 76.0d0, t_k) ! [pascal]
      po_soa(ipcg8_b_o_g)  = fn_po(9.91d-1, 70.0d0, t_k) ! [pascal]
      po_soa(ipcg9_b_o_g)  = fn_po(9.91d0,  64.0d0, t_k) ! [pascal]
      po_soa(iopcg1_b_o_g) = fn_po(9.91d-8, 112.0d0, t_k) ! [pascal]
      po_soa(iopcg2_b_o_g) = fn_po(9.91d-7, 106.0d0, t_k) ! [pascal]
      po_soa(iopcg3_b_o_g) = fn_po(9.91d-6, 100.0d0, t_k) ! [pascal]
      po_soa(iopcg4_b_o_g) = fn_po(9.91d-5, 94.0d0, t_k) ! [pascal]
      po_soa(iopcg5_b_o_g) = fn_po(9.91d-4, 88.0d0, t_k) ! [pascal]
      po_soa(iopcg6_b_o_g) = fn_po(9.91d-3, 82.0d0, t_k) ! [pascal]
      po_soa(iopcg7_b_o_g) = fn_po(9.91d-2, 76.0d0, t_k) ! [pascal]
      po_soa(iopcg8_b_o_g) = fn_po(9.91d-1, 70.0d0, t_k) ! [pascal]

      po_soa(ipcg1_f_c_g)  = fn_po(9.91d-8, 112.0d0, t_k) ! [pascal]
      po_soa(ipcg2_f_c_g)  = fn_po(9.91d-7, 106.0d0, t_k) ! [pascal]
      po_soa(ipcg3_f_c_g)  = fn_po(9.91d-6, 100.0d0, t_k) ! [pascal]
      po_soa(ipcg4_f_c_g)  = fn_po(9.91d-5, 94.0d0, t_k) ! [pascal]
      po_soa(ipcg5_f_c_g)  = fn_po(9.91d-4, 88.0d0, t_k) ! [pascal]
      po_soa(ipcg6_f_c_g)  = fn_po(9.91d-3, 82.0d0, t_k) ! [pascal]
      po_soa(ipcg7_f_c_g)  = fn_po(9.91d-2, 76.0d0, t_k) ! [pascal]
      po_soa(ipcg8_f_c_g)  = fn_po(9.91d-1, 70.0d0, t_k) ! [pascal]
      po_soa(ipcg9_f_c_g)  = fn_po(9.91d0,  64.0d0, t_k) ! [pascal]
      po_soa(iopcg1_f_c_g) = fn_po(9.91d-8, 112.0d0, t_k) ! [pascal]
      po_soa(iopcg2_f_c_g) = fn_po(9.91d-7, 106.0d0, t_k) ! [pascal]
      po_soa(iopcg3_f_c_g) = fn_po(9.91d-6, 100.0d0, t_k) ! [pascal]
      po_soa(iopcg4_f_c_g) = fn_po(9.91d-5, 94.0d0, t_k) ! [pascal]
      po_soa(iopcg5_f_c_g) = fn_po(9.91d-4, 88.0d0, t_k) ! [pascal]
      po_soa(iopcg6_f_c_g) = fn_po(9.91d-3, 82.0d0, t_k) ! [pascal]
      po_soa(iopcg7_f_c_g) = fn_po(9.91d-2, 76.0d0, t_k) ! [pascal]
      po_soa(iopcg8_f_c_g) = fn_po(9.91d-1, 70.0d0, t_k) ! [pascal]

      po_soa(ipcg1_f_o_g)  = fn_po(9.91d-8, 112.0d0, t_k) ! [pascal]
      po_soa(ipcg2_f_o_g)  = fn_po(9.91d-7, 106.0d0, t_k) ! [pascal]
      po_soa(ipcg3_f_o_g)  = fn_po(9.91d-6, 100.0d0, t_k) ! [pascal]
      po_soa(ipcg4_f_o_g)  = fn_po(9.91d-5, 94.0d0, t_k) ! [pascal]
      po_soa(ipcg5_f_o_g)  = fn_po(9.91d-4, 88.0d0, t_k) ! [pascal]
      po_soa(ipcg6_f_o_g)  = fn_po(9.91d-3, 82.0d0, t_k) ! [pascal]
      po_soa(ipcg7_f_o_g)  = fn_po(9.91d-2, 76.0d0, t_k) ! [pascal]
      po_soa(ipcg8_f_o_g)  = fn_po(9.91d-1, 70.0d0, t_k) ! [pascal]
      po_soa(ipcg9_f_o_g)  = fn_po(9.91d0,  64.0d0, t_k) ! [pascal]
      po_soa(iopcg1_f_o_g) = fn_po(9.91d-8, 112.0d0, t_k) ! [pascal]
      po_soa(iopcg2_f_o_g) = fn_po(9.91d-7, 106.0d0, t_k) ! [pascal]
      po_soa(iopcg3_f_o_g) = fn_po(9.91d-6, 100.0d0, t_k) ! [pascal]
      po_soa(iopcg4_f_o_g) = fn_po(9.91d-5, 94.0d0, t_k) ! [pascal]
      po_soa(iopcg5_f_o_g) = fn_po(9.91d-4, 88.0d0, t_k) ! [pascal]
      po_soa(iopcg6_f_o_g) = fn_po(9.91d-3, 82.0d0, t_k) ! [pascal]
      po_soa(iopcg7_f_o_g) = fn_po(9.91d-2, 76.0d0, t_k) ! [pascal]
      po_soa(iopcg8_f_o_g) = fn_po(9.91d-1, 70.0d0, t_k) ! [pascal]

      po_soa(iant1_o_g) = fn_po(9.91d-6, 100.0d0, t_k) ! [pascal]
      po_soa(iant2_o_g) = fn_po(9.91d-5, 94.0d0, t_k) ! [pascal]
      po_soa(iant3_o_g) = fn_po(9.91d-4, 88.0d0, t_k) ! [pascal]
      po_soa(iant4_o_g) = fn_po(9.91d-3, 82.0d0, t_k) ! [pascal]

      po_soa(iant1_c_g) = fn_po(9.91d-6, 106.0d0, t_k) ! [pascal]
      po_soa(iant2_c_g) = fn_po(9.91d-5, 100.0d0, t_k) ! [pascal]
      po_soa(iant3_c_g) = fn_po(9.91d-4, 94.0d0, t_k) ! [pascal]
      po_soa(iant4_c_g) = fn_po(9.91d-3, 88.0d0, t_k) ! [pascal]

      po_soa(ibiog1_c_g) = fn_po(9.91d-6, 106.0d0, t_k) ! [pascal]
      po_soa(ibiog2_c_g) = fn_po(9.91d-5, 100.0d0, t_k) ! [pascal]
      po_soa(ibiog3_c_g) = fn_po(9.91d-4, 94.0d0, t_k) ! [pascal]
      po_soa(ibiog4_c_g) = fn_po(9.91d-3, 88.0d0, t_k) ! [pascal]

      po_soa(ibiog1_o_g) = fn_po(9.91d-6, 106.0d0, t_k) ! [pascal]
      po_soa(ibiog2_o_g) = fn_po(9.91d-5, 100.0d0, t_k) ! [pascal]
      po_soa(ibiog3_o_g) = fn_po(9.91d-4, 94.0d0, t_k) ! [pascal]
      po_soa(ibiog4_o_g) = fn_po(9.91d-3, 88.0d0, t_k) ! [pascal]

      end if ! (vbs_nbin(1) .eq. 9) then ! default


      if (vbs_nbin(1).eq.4) then
        po_soa(iasoaX_g) = fn_po(9.91d-10, 40.0d0, T_K) ! [Pascal]
        po_soa(iasoa1_g) = fn_po(9.91d-6, dhr_approx(0.0d0), T_K) !  [Pascal]
        po_soa(iasoa2_g) = fn_po(9.91d-5, dhr_approx(1.0d0), T_K) !  [Pascal]
        po_soa(iasoa3_g) = fn_po(9.91d-4, dhr_approx(2.0d0), T_K) !  [Pascal]
        po_soa(iasoa4_g) = fn_po(9.91d-3, dhr_approx(3.0d0), T_K) !  [Pascal]
        po_soa(ibsoaX_g) = fn_po(9.91d-10, 40.0d0, T_K) ! [Pascal]
        po_soa(ibsoa1_g) = fn_po(9.91d-6, dhr_approx(0.0d0), T_K) !  [Pascal]
        po_soa(ibsoa2_g) = fn_po(9.91d-5, dhr_approx(1.0d0), T_K) !  [Pascal]
        po_soa(ibsoa3_g) = fn_po(9.91d-4, dhr_approx(2.0d0), T_K) !  [Pascal]
        po_soa(ibsoa4_g) = fn_po(9.91d-3, dhr_approx(3.0d0), T_K) !  [Pascal]
      endif


      if (vbs_nbin(1).eq.2) then
        po_soa(ipcg1_b_c_g)  = fn_po(9.91d-8, 83.0d0, t_k) ! [pascal]
        po_soa(ipcg2_b_c_g)  = fn_po(9.91d-1, 83.0d0, t_k) ! [pascal]
        po_soa(iopcg1_b_c_g) = fn_po(9.91d-8, 83.0d0, t_k) ! [pascal]
        po_soa(ipcg1_b_o_g)  = fn_po(9.91d-8, 83.0d0, t_k) ! [pascal]
        po_soa(ipcg2_b_o_g)  = fn_po(9.91d-1, 83.0d0, t_k) ! [pascal]
        po_soa(iopcg1_b_o_g) = fn_po(9.91d-8, 83.0d0, t_k) ! [pascal]
        po_soa(ipcg1_f_c_g)  = fn_po(9.91d-8, 83.0d0, t_k) ! [pascal]
        po_soa(ipcg2_f_c_g)  = fn_po(9.91d-1, 83.0d0, t_k) ! [pascal]
        po_soa(iopcg1_f_c_g) = fn_po(9.91d-8, 83.0d0, t_k) ! [pascal]
        po_soa(ipcg1_f_o_g)  = fn_po(9.91d-8, 83.0d0, t_k) ! [pascal]
        po_soa(ipcg2_f_o_g)  = fn_po(9.91d-1, 83.0d0, t_k) ! [pascal]
        po_soa(iopcg1_f_o_g) = fn_po(9.91d-8, 83.0d0, t_k) ! [pascal]
        po_soa(iant1_c_g)    = fn_po(9.91d-6, 83.0d0, t_k) ! [pascal]
        po_soa(iant1_o_g)    = fn_po(9.91d-6, 83.0d0, t_k) ! [pascal]
        po_soa(ibiog1_c_g)   = fn_po(9.91d-6, 83.0d0, t_k) ! [pascal]
        po_soa(ibiog1_o_g)   = fn_po(9.91d-6, 83.0d0, t_k) ! [pascal]
      endif

      if (vbs_nbin(1).eq.3) then
! these values for pcg and opcg gases are the same as vbs_nbin(1)==2
! above
        po_soa(ipcg1_b_c_g)  = fn_po(9.91d-8, 83.0d0, t_k) ! [pascal]
        po_soa(ipcg2_b_c_g)  = fn_po(9.91d-1, 83.0d0, t_k) ! [pascal]
        po_soa(iopcg1_b_c_g) = fn_po(9.91d-8, 83.0d0, t_k) ! [pascal]
        po_soa(ipcg1_b_o_g)  = fn_po(9.91d-8, 83.0d0, t_k) ! [pascal]
        po_soa(ipcg2_b_o_g)  = fn_po(9.91d-1, 83.0d0, t_k) ! [pascal]
        po_soa(iopcg1_b_o_g) = fn_po(9.91d-8, 83.0d0, t_k) ! [pascal]
        po_soa(ipcg1_f_c_g)  = fn_po(9.91d-8, 83.0d0, t_k) ! [pascal]
        po_soa(ipcg2_f_c_g)  = fn_po(9.91d-1, 83.0d0, t_k) ! [pascal]
        po_soa(iopcg1_f_c_g) = fn_po(9.91d-8, 83.0d0, t_k) ! [pascal]
        po_soa(ipcg1_f_o_g)  = fn_po(9.91d-8, 83.0d0, t_k) ! [pascal]
        po_soa(ipcg2_f_o_g)  = fn_po(9.91d-1, 83.0d0, t_k) ! [pascal]
        po_soa(iopcg1_f_o_g) = fn_po(9.91d-8, 83.0d0, t_k) ! [pascal]
! these values for ant and bio gases are from manish wrfchem 3.5
        po_soa(iant1_c_g)    = fn_po(9.91d-7, 106.0d0, t_k) ! [pascal]
        po_soa(iant2_c_g)    = fn_po(9.91d-6, 100.0d0, t_k) ! [pascal]
        po_soa(iant3_c_g)    = fn_po(9.91d-5,  94.0d0, t_k) ! [pascal]
        po_soa(iant4_c_g)    = fn_po(9.91d-4,  88.0d0, t_k) ! [pascal]
        po_soa(ibiog1_c_g)   = fn_po(9.91d-7, 106.0d0, t_k) ! [pascal]
        po_soa(ibiog2_c_g)   = fn_po(9.91d-6, 100.0d0, t_k) ! [pascal]
        po_soa(ibiog3_c_g)   = fn_po(9.91d-5,  94.0d0, t_k) ! [pascal]
        po_soa(ibiog1_o_g)   = fn_po(9.91d-7, 106.0d0, t_k) ! [pascal]
        po_soa(ibiog2_o_g)   = fn_po(9.91d-6, 100.0d0, t_k) ! [pascal]
      endif

! scm_temp - temporary for scm
!
! chem_opt=198 (vbs_nbin=2) a01 soa
! pcg1_b_c_a01,pcg2_b_c_a01,pcg1_b_o_a01,pcg2_b_o_a01,opcg1_b_c_a01,opcg1_b_o_a01,pcg1_f_c_a01,pcg2_f_c_a01,pcg1_f_o_a01,pcg2_f_o_a01,opcg1_f_c_a01,opcg1_f_o_a01,
! ant1_c_a01,ant1_o_a01,
! biog1_c_a01,biog1_o_a01,
! 
! chem_opt=204 (vbs_nbin=3) a01 soa
! pcg1_b_c_a01,pcg1_b_o_a01,opcg1_b_c_a01,opcg1_b_o_a01,pcg1_f_c_a01,pcg1_f_o_a01,opcg1_f_c_a01,opcg1_f_o_a01,
! ant1_c_a01, ant2_c_a01, ant3_c_a01, ant4_c_a01,
! biog1_c_a01, biog2_c_a01, biog3_c_a01, biog1_o_a01, biog2_o_a01,
! 
! to get chem_opt=204 to work, do the following in both the mosaic1 and mosaic2 code
!    duplicate the vbs_nbin=2 block above for vbs_nbin=3
!    add lines for ant2-4 and biog2-3 from the vbs_nbin=9 block
! *** best to not bother about this, as it is a manish problem ***


      if (vbs_nbin(1).eq.0) then
        po_soa(ismpa_g)    = fn_po(9.91d-8, 83.0d0, t_k) ! [pascal]
        po_soa(ismpbb_g)   = fn_po(9.91d-8, 83.0d0, t_k) ! [pascal]
        po_soa(ibiog1_c_g) = fn_po(9.91d-6, 83.0d0, t_k) ! [pascal]
        po_soa(ibiog1_o_g) = fn_po(9.91d-6, 83.0d0, t_k) ! [pascal]
      endif


!
! set saturation vapor concentrations
!
      if (vbs_nbin(1).eq.0) then
        itmpa = ismpa_g
      else if (vbs_nbin(1).eq.4) then
        itmpa = iasoaX_g
      else
        itmpa = ipcg1_b_c_g
      end if

      sat_soa(:) = 0.0
      do iv = itmpa, ngas_ioa + ngas_soa
        sat_soa(iv) = 1.e9*po_soa(iv)/(8.314*t_k)       !  [nmol/m^3(air)]
      enddo

 
      return
      end subroutine soa_vbs_update_thermcons


!-------------------------------------------------------------------------------
      ! Function to approximate enthalpy of vaporization for
      ! semi-volatile organic aerosols as a function of volatility
      ! from Epstein et al., ES&T, 2010
      ! http://pubs.acs.org/doi/abs/10.1021/es902497z
      real(r8) function dhr_approx(log10_Csat_298)

      real(r8), intent(in) :: log10_Csat_298

      dhr_approx = -11.0 * log10_Csat_298 + 131.0 ! kJ/mol

      end function dhr_approx


!-------------------------------------------------------------------------------
      real(r8) function fn_po(po_298, dh, t)        ! touch
! subr. arguments
      real(r8), intent(in) :: po_298, dh, t
! local variables (none)

      fn_po = po_298*exp(-(dh/8.314e-3)*(1./t - 3.354016435e-3))

      return
      end function fn_po



      end module module_mosaic_soa_vbs
