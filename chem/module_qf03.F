MODULE qf03
!
! Y. Shao, 29 Jan 2004 
!         
! JY Kang, 01 Dec 2008
! Modify the code for WRF_chem
!
! M. Klose, 2010-2015 Modifications
!-----------------------------------------------------------------------------------
! Calculate sediment flux for multi-particle size soils as a weighted average of Q(d) 
!           dust emission F(d) for covered and moisture soil
!
! Options for dust calculation:
! 1 Shao (2001)
! 2 Shao (2004)
! 3 Shao (2011): simplification of 2; added on 26 Sep 2009
!
!--------------------------------------------------------------------------------------
!
!      input: 
!      n:          number of particle size ranges.
!      dm:         median diameter of each particle size.                   [m]
!      m_fract:    Weight fraction of each particle range. Sum m_fract = 1
!      ustar:      Friciton velocity.                                       [m/s]
!      cf:         fraction area covered by roughness elements/vegetation cover
!      w:          surface soil moisture content [m^3/m^3]
!      wr:         air-dry soil moisture [m^3/m^3] from SOILPARM.TBL
!      c:          Owen's coefficient
!
!      output:  
!      ustart:     Mean threshold velocity of each particle range.
!      q:          Sand flux from each size range.
!      ffq:        Weighted sand flux from each size range.
!      qtotal:     Total sand flux (weighted average).
!      f:          Weighted dust flux ejected by single sand size range for given d_d. 
!      fff:        Weighted dust flux ejected by all sand size range from each dust size range.
!      ftotal:     Total dust flux (weighted average).
!--------------------------------------------------------------------------------------
!

  CONTAINS

  subroutine qf03_driver ( nmx, idst, g, rhop, rho, dt, &
                    ustar, w, cf, ust_min, imod, dz_lowest, &
                    soilc, tot_soilc, domsoilc,    &
                    tc, bems, rough_cor_in, smois_cor_in, wr, &
                    d0, dd, psd_m, dpsd_m, ppsd_m, psd_f, dpsd_f, ppsd_f, imax, stype)
       
       INTEGER, INTENT(IN   )                   :: nmx, imod, idst
       REAL   , INTENT(IN   )                   :: g, dt, rho, dz_lowest, ustar
       REAL(8), INTENT(IN   )                   :: w
       REAL(8), INTENT(INOUT)                   :: cf
       REAL(8), INTENT(  OUT)                   :: ust_min, rough_cor_in, smois_cor_in
       REAL   , INTENT(INOUT), DIMENSION(nmx)   :: tc
       REAL   , INTENT(  OUT), DIMENSION(nmx)   :: bems
       REAL   , INTENT(IN   ), DIMENSION(12)    :: wr

!kang [2009/01/07] soil class type
       REAL, INTENT(IN   )                      :: tot_soilc       
       REAL, DIMENSION(16), INTENT(IN)          :: soilc       
       INTEGER,  INTENT(IN  )                   :: domsoilc
       
       integer :: imax, stype
       real(8), dimension(0:imax), intent(in) :: d0
       real(8), dimension(imax), intent(in)   :: dd 
       real(8), dimension(imax,stype), intent(in)   :: psd_m, dpsd_m, ppsd_m
       real(8), dimension(imax,stype), intent(in)   :: psd_f, dpsd_f, ppsd_f
 
!local variables
!      particle-size distributions 
       real(8), dimension(imax)      :: psdm, dpsdm, ppsdm    ! minimally dispersed
       real(8), dimension(imax)      :: psdf, dpsdf, ppsdf    ! fully dispersed
       real(8), dimension(imax)      :: psds, dpsds, ppsds    ! sediment       

       real(8) :: smois_correc
       integer :: i, j, n, kk, ij, ffile
       real(8) :: total, qtotal, ftotal
       real(8) :: ftotalb, ftotalp
!
       real(8), dimension(imax)   :: beta_d, beta_s  ! beta1 and beta2 used by Shao et al. (1996) dust emission
       real(8), dimension(imax)   :: ustart, q, ffq, fff
       real(8), dimension(imax,imax) :: f
!
       real(8), parameter :: c_lambda=0.35
       real(8) :: h, lambda
       real(8) :: ghl, fc
       real(8) :: phl
       real(8) :: cys, u0, al0, rhos, smass, omega, rys
       real(8) :: ddm, a1, a2, a3  
       real(8) :: zeta, sigma_m                      ! u*sqrt(rhos/p), bombardment coefficient
!
       real(8) :: ustart0_out, qwhite_out, f_mb_out, f_hlys_out, pmass_out, vhlys_out
! 
       integer, parameter :: nbins = 5 
       real(8) :: sigma
       real, dimension(nbins)    :: dbin, fbin, cell_fbin
       integer, dimension(nbins) :: ibin
       data dbin/2.,3.6,6.,12.,20./ !size cut diameter (um) consistent with GOCART model
       real(8), intent(in) :: rhop
       real    :: cell_ftotal
       integer :: isl, cc
       character :: msg, soil
!*******************************************************************************************
!
! initialization
       cell_ftotal = 0.
       do n = 1, nbins
       cell_fbin(n) = 0.
       enddo
! 
       DO cc = 1, 12  ! soil category
           if (soilc(cc).eq.0.) then
              go to 103
           endif
!        
           psdm(:)=psd_m(:,cc)
           psdf(:)=psd_f(:,cc)
           dpsdm(:)=dpsd_m(:,cc)
           dpsdf(:)=dpsd_f(:,cc)
           ppsdm(:)=ppsd_m(:,cc)
           ppsdf(:)=ppsd_f(:,cc)

! default settings: 
       rhos = 1000.d0      ! bulk density of soil [kg/m3] 
       phl = 30000.        ! plastic pressure [N/m2]
       cys = 0.00001       ! cys : parameter

       sigma = rhop/rho    ! particle-air density ratio

!
!-------------------
! frontal area index
!-------------------
         if (cf .gt. 0.95) then 
            write(6,*) 'cover fraction too large, reset'
            cf = 0.95
         endif! as larger values lead to large lambda, which is problematic in Raupach scheme
         
         lambda = - c_lambda*dlog( 1.d0 - cf )
         call r_c(lambda, rough_cor_in)

! Matching WRF_soil class with Shao's class for moisture correction of Fecan (cc:WRF_soil class, isl:Shao's class)
! cc
! 1:sand, 2:loamy sand, 3:sandy loam, 4:silt loam, 5:silt, 6:loam, 7:sandy clay loam,
! 8:silty clay loam, 9:clay loam, 10:sandy clay, 11:silty clay, 12:clay
! isl
! 1:sand, 2:loamy sand, 3:sandy loam, 4:loam, 5:silt loam, 6: silt, 7:sandy clay loam, 
! 8:clay loam, 9:silty clay loam, 10:sandy clay, 11:silty clay, 12:clay
        if (cc.eq.1.or.cc.eq.2.or.cc.eq.3.or.cc.eq.7.or.cc.eq.10.or.  & 
        &   cc.eq.11.or.cc.eq.12) then
         isl = cc
        elseif (cc.eq.4) then
         isl = 5
        elseif (cc.eq.5) then
         isl = 6
        elseif (cc.eq.6) then
         isl = 4
        elseif (cc.eq.8) then
         isl = 9
        elseif (cc.eq.9) then
         isl = 8
        endif

         call h_c(w, wr(cc), isl, smois_correc)
!----------------------------------------------
! for each particle size group, estimate ustart
!----------------------------------------------
!
         ust_min = 999.0
         do i = 1, imax
            call ustart0(dd(i), sigma, g, rho, ustart0_out)
            ustart(i) = ustart0_out
            ustart(i) = rough_cor_in*smois_correc*ustart(i)
            ust_min = dmin1(ust_min, ustart(i))
            call qwhite(ustart(i), ustar, rho, g, qwhite_out)
            q(i) = qwhite_out
            q(i) = (1.d0-cf)*q(i)
         enddo
         if (cc.eq.domsoilc) then
          smois_cor_in = smois_correc
         endif
! 
! 
         IF ( ustar .le. ust_min ) THEN    ! no erosion goto 102
            q = 0.d0
            ffq = 0.d0
            qtotal = 0.d0
            fff = 0.d0
            ftotal = 0.d0
            fbin = 0.d0
            goto 102
         ELSE
            ghl = dexp( -(ustar - ust_min)**3.d0 )
            dpsds = ghl*dpsdm + (1.-ghl)*dpsdf
            psds  = ghl*psdm + (1.-ghl)*psdf
            ppsds = ghl*ppsdm + (1.-ghl)*ppsdf
!
            ffq = q*dpsds
            qtotal = sum(ffq)

!--------------
! dust emission
!--------------
! 
! size bin
            do n=1,nbins
             ibin(n)=0
             do i=imax,1,-1
              if(d0(i).ge.dbin(n)) ibin(n)=i
             enddo
             if(ibin(n).eq.0) stop 'wrong dust classes'
            enddo
! 
! 
! 
!--------------------------------
! Shao (2001) dust emission model
!--------------------------------
        IF (imod .eq. 1) THEN 
           do i = idst+1, imax
              ddm = dd(i)*1.d-6
              call pmass(rhop, ddm, pmass_out)                       ! mass of saltating particles
              smass = pmass_out
              u0 = 10*ustar
              al0 = 13.d0*3.14159d0/180.d0
              call vhlys(phl, 2, smass, al0, u0, ddm, vhlys_out)
              omega = vhlys_out     ![m3]
!               
              do j = 1, idst
                 rys = psdm(j)/psdf(j)
                 if (rys.gt.1.) then 
                     rys = 1.
                 endif
                 a1 = cys*( (1.-ghl) + ghl*rys )
                 a2 = ffq(i)*g/ustar**2/smass
                 
                 if ( dpsdf(j) .lt. dpsdm(j) ) then 
                    a3 = dpsdf(j)*rhos*omega
                 else 
                    a3 = dpsdf(j)*rhos*omega + (dpsdf(j)-dpsdm(j))*smass
                 endif
                 f(i,j) = a1*a2*a3    ![kg/m2/s]
              enddo
           enddo
!          
           ftotal = 0.0
           do j = 1, idst
              fff(j) = 0.
              do i = idst+1, imax
                 fff(j) = fff(j) + f(i,j)
              enddo
              fff(j) = (1.d0-cf)*fff(j)
              ftotal = ftotal + fff(j)
           enddo
! 
           do n=1,nbins
            j0=1
            if(n.gt.1) j0=ibin(n-1)+1
            fbin(n)=0
            do j=j0,ibin(n)
             fbin(n)=fbin(n)+fff(j)
            enddo
           enddo
! 
!--------------------------------
! Shao (2004) dust emission model
!--------------------------------
        ELSEIF (imod .eq. 2) THEN
          zeta = ustar*dsqrt( rhos/phl )
          sigma_m = 12.d0*zeta*zeta*(1.d0+14.d0*zeta)
! 
          do i = idst+1, imax
             do j = 1, idst
                rys = psdm(j)/psdf(j)
                if (rys .gt.1.) then
                    rys = 1.
                endif
                a1 = cys*dpsdf(j)*( (1.-ghl) + ghl*rys )
                a2 = (1.+sigma_m)
                a3 = ffq(i)*g/ustar**2
                f(i,j) = a1*a2*a3
             enddo
          enddo
!           
          ftotal = 0.0
          do j = 1, idst
             fff(j) = 0.
             do i = idst+1, imax
                fff(j) = fff(j) + f(i,j)
             enddo
             fff(j) = (1.d0-cf)*fff(j)             
             ftotal = ftotal + fff(j)
          enddo
!           
          do n=1,nbins
           j0=1
           if(n.gt.1) j0=ibin(n-1)+1
           fbin(n)=0
           do j=j0,ibin(n)
            fbin(n)=fbin(n)+fff(j) 
           enddo
          enddo
! 
! 
!--------------------------------------------------------------------------
! Shao (2011) minimal version, ghl = 1, Q independent of sand particle size
! 
! See Eq. (34) in 
! Shao, Y., M. Ishizuka, M. Mikami, J. Leys (2011): Parameterization of size-
! resolved dust emission and validation with measurements, JGR, 116, D08203, 
! doi: 10.1029/2010JD014527
!--------------------------------------------------------------------------
        ELSEIF (imod .eq. 3) THEN 
          zeta = ustar*dsqrt( rhos/phl )
          sigma_m = 12.d0*zeta*zeta*(1.d0+14.d0*zeta)
! 
          ftotal = 0.0          
! 
          do j = 1, idst
             a1 = cys*dpsdm(j)
             a2 = (1.+sigma_m)
             a3 = qtotal*g/ustar**2
             fff(j) = a1*a2*a3
             fff(j) = (1.d0-cf)*fff(j)
             ftotal = ftotal + fff(j)
          enddo
!           
          do n=1,nbins
           j0=1
           if(n.gt.1) j0=ibin(n-1)+1
           fbin(n)=0
           do j=j0,ibin(n)
            fbin(n)=fbin(n)+fff(j) 
           enddo
          enddo
! 
        ENDIF  ! dust scheme
      ENDIF    ! ust < ust_t
! 
! 
! 
102   continue

      do n = 1, nbins
      cell_fbin(n) = cell_fbin(n) + (soilc(cc)/tot_soilc)*fbin(n)
      enddo

103   continue  

      ENDDO  ! cc, soil category
      
      do n = 1, nbins
! fbin : [kg/m2/s],  dz_lowest : [m],  rho : [kg/m3],  dt : [s] -> tc : [kg/kg-dryair]
         tc(n) = tc(n) + cell_fbin(n)/dz_lowest/rho*dt     ![kg/kg-dryair]
         bems(n) = cell_fbin(n)     ![kg/m2/s]
      enddo


      END subroutine qf03_driver


!*****************************************************************************
      subroutine ustart0(dum, sigma, g, rho, ustart0_out)
!
! Y. Shao, 13 June 2000
!
! Calculate ustar0(d) using Shao and Lu (2000) for uncovered
! dry surface
!
! dum:    particle diameter			[um]
! ustar0: threshold friction velocity   	[m/s]
!
      real, intent(in)     :: g, rho
      real(8), intent(in)  :: dum, sigma
      real(8), intent(out) :: ustart0_out
      real(8) :: dm
      real(8), parameter :: gamma = 1.65d-4      ! a constant
      real(8), parameter :: f = 0.0123

      dm = dum*1d-6

      ustart0_out = f*(sigma*g*dm + gamma/(rho*dm) )
      ustart0_out = dsqrt( ustart0_out )
!      end function
      end subroutine ustart0
!*****************************************************************************
      subroutine qwhite(ust, ustar, rho, g, qwhite_out)
!
! Yaping Shao 17-07-99!
!
! White (1979) Sand Flux Equation
! Q = c*rho*u_*^3 over g (1 - u_*t over u_*)(1 + u_*t^2/u_*^2)
! qwhite: Streamwise Sand Flux;       [kg m-1 s-1]
! c     : 2.6
! ust   : threhold friction velocity  [m/s]
! ustar : friction velocity           [m/s]
!
      real(8) :: c
      real, intent(in) :: ustar, rho, g
      real(8), intent(in) :: ust
      real(8), intent(out) :: qwhite_out
      real(8) :: a, b
! default setting:       
      c = 2.3d0
      a = rho/g

      IF (ustar.lt.ust) THEN 
        qwhite_out = 0.d0
      ELSE
        b = ust/ustar 
        qwhite_out = c*a*ustar**3.*(1.-b)*(1.+b)**2.
      ENDIF

      END subroutine qwhite
!*****************************************************************************
      subroutine vhlys(p, k, xm, alpha, u, d, vhlys_out)
!
! Volume removal according to Lu and Shao (1999), Equation (8)
! alpha: impact angle             [^o]
! u    : impact velocity          [m/s]
! p    : plastic pressure         [N/m^2]
! xm   : particle mass            [kg]
! d    : particle diameter        [m]
!
!
      REAL(8),intent(in) :: alpha, xm, u, d, p
      REAL(8) :: beta
      REAL(8), PARAMETER :: pi=3.1415927d0
      REAL(8) :: t1, t2, t3
      INTEGER,intent(in) :: k
      real(8),intent(out) :: vhlys_out

      beta = dsqrt( p*k*d/xm )
      t1 = u*u/(beta*beta)*( dsin(2.d0*alpha) - 4.d0*dsin(alpha)*dsin(alpha) )
      t2 = u*dsin(alpha)/beta
      t3 = 7.5d0*pi*t2**3.d0/d
      vhlys_out = d*( t1 + t3 )
      
      END subroutine vhlys      
!*****************************************************************************
! A routine for correction of ust for soil moisture content
!
! w : volumetric soil moisture
! isl: soil texture type, ranging from 1 to 12
!
! Author: Yaping Shao, 5/05/2001
! Reference: Fecan et al. (1999), Ann. Geophysicae,17,149-157
!
! Data based on Shao and Jung, 2000, unpublished manuscript
! Data invented for sand, loamy sand, sandy loam, loam, clay loam, and clay
! Soil classes:
! 1 sand, 2 loamy sand, 3 sandy loam, 4 loam, 5 silty loam, 6 silt, 7 sandy clay loam, 8 clay loam, 
! 9 silty clay loam, 10 sandy clay, 11 silty clay, 12 clay
!----------------------------------------------------------------------
      subroutine h_c (w, wr, isl, h)

      real(8) :: a(12), b(12)!, thr(12) 
      real(8), intent(in)  :: w
      real(8), intent(out) :: h
      real, intent(in)     :: wr
      integer, intent(in)  :: isl
      character*100 :: msg  ! error message string

!       data thr/0.001, 0.003, 0.037, 0.049, 0.061, 0.072, 0.084, 0.095, 0.110, 0.126, 0.141, 0.156/
      data a /21.19, 33.03, 44.87, 17.79, 20.81, 23.83, 26.84, 29.86, 27.51, 25.17, 22.82, 20.47/
      data b / 0.68,  0.71,  0.85,  0.61,  0.66,  0.71,  0.75,  0.80,  0.75,  0.70,  0.64,  0.59/     
      
      if ( w.lt.0. ) then
        write(msg, *) 'soil moisture correction (h_c): w = ', w, ' <  0'
        call wrf_error_fatal(msg)
!         stop
      endif
      
      if ( w.le.wr ) then
         h = 1.0
      else
         h = sqrt( 1 + a(isl)*( w-wr )**b(isl) )         
      endif

      END subroutine h_c

!*****************************************************************************
      subroutine pmass(rhop, d, pmass_out)
!
! 	Particle Mass	
!     	rhop: particle density		[kg m^-3]
!     	d   : particle size		[m]
!	
      REAL(8), PARAMETER :: pi=3.1415927d0
      REAL(8),intent(in) :: rhop, d
      real(8),intent(out) :: pmass_out

      pmass_out = (pi*rhop*d**3.d0)/6.d0

      END subroutine pmass

!*****************************************************************************
      subroutine r_c (x,r)
!
!   Y. Shao 17-07-92
!   CORRECTION FUNCTION FOR UST(D) BASED ON Raupach et al. (1992)
!   x = frontal area index
!
!   R_C = (1 - sig m x)^{1/2} (1 + m beta x)^{1/2}
!         Note I deife R_C = u_{*tR}/u_{*tS}
!         While Raupach et al. defined 
!                      R_C = u_{*tS}/u_{*tR} and their R function is
!   R_C = (1 - sig m x)^{-1/2} (1 + m beta x)^{-1/2}
!                      
!   sig   : basal to frontal area; about 1
!   m     : parameter less than 1; about 0.5
!   beta  : a ratio of drag coef.; about 90. ; changed to 200 (recommendation by Y. Shao based on data by Gillies et al.)
!
      real(8) :: xc
      real(8), intent(in) :: x
      real(8), intent(out) :: r
      real(8), parameter :: sig=1., m=0.5, beta=200.
!
      xc = 1./(sig*m)
      IF (x.ge.xc) THEN
        r   = 999.           ! Full covered surface
      ELSE
        r   = dsqrt(1.-sig*m*x)*dsqrt(1.+m*beta*x)
      ENDIF
!
      END subroutine r_c



END MODULE qf03
