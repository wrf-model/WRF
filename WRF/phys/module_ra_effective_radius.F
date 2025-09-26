!
!-------------------------------------------------------------------------------
   module module_ra_effective_radius
!-------------------------------------------------------------------------------
!
!  abstract : 
!    calculate effective radius of cloud water, cloud ice, and snow
!             based on WSM/DM microphysics scheme 
!
!  history log:
!   2015-07-01  soo ya bae   initial setup
!   2015-10-01  soo ya bae   move effective radius to radiation 
!                            add subroutine for consistency among rad-cps-mps 
!   2016-01-01  soo ya bae   bug fix
!
!  reference :
!    bae et al. (2016, adv meteorol)
!
!-------------------------------------------------------------------------------
   use module_model_constants,  only: denr=>rhowater, dens=>rhosnow
!-------------------------------------------------------------------------------
   real, parameter, private :: n0s = 2.e6      ! temperature dependent 
                                               ! intercept parameter of snow
   real, parameter, private :: n0g = 4.e6
   real, parameter, private :: n0smax =  1.e11 ! maximum n0s (t=-90C unlimited)
   real, parameter, private :: nc0 = 3.e8      ! number conc. of cloud droplet
   real, parameter, private :: deng = 500.0    ! density of graupel
   real, parameter, private :: alpha = .12     ! .122 exponen factor for n0s
   real, parameter, private :: pi = 4.*atan(1.)
!
   contains
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   real function rgmma(x)
!-------------------------------------------------------------------------------
!
!  abstract : rgmma function
!             use infinite product form
!
!-------------------------------------------------------------------------------
!
   implicit none
!
   real, parameter :: euler = 0.577215664901532
   real            :: x, y
   integer         :: i
!-------------------------------------------------------------------------------
   if (x.eq.1.) then
     rgmma = 0.
   else
     rgmma = x*exp(euler*x)
     do i = 1,10000
       y = real(i)
       rgmma = rgmma*(1.000+x/y)*exp(-x/y)
     enddo
     rgmma = 1./rgmma
   endif
!
   end function rgmma
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   subroutine effectRad(t, qc, nc, qi, qs, qg, rho, qmin, t0c,                 &
                        qccps, f_qnc,                                          & 
                        re_qc, re_qi, re_qs, kts, kte)
!-------------------------------------------------------------------------------
!
!  abstract : 
!    compute radiation effective radii of cloud water, ice, and snow 
!             for double-moment microphysics.
!
!  input  :
!    kts, kte   - dimension
!    ii, jj
!    qmin       - minimum value of hydrometeor in kg/kg
!    t0c        - 273.15 K
!    t          - temperature
!    qc         - mixing ratio of cloud water in kg/kg
!    qi         - mixing ratio of cloud ice in kg/kg
!    qs         - mixing ratio of snow in kg/kg
!    qg         - mixing ratio of graupel in kg/kg
!    nc         - number concentratino of cloud water in 1/m3
!    rho        - air density of kg/m3
!
!  inout :
!    re_qc     - effective radius of cloud water in m
!    re_qi     - effective radius of cloud ice in m
!    re_qs     - effective radius of snow in m
!
!-------------------------------------------------------------------------------
!
   implicit none
!
   integer                 , intent(in   ) :: kts, kte
   real                    , intent(in   ) :: qmin, t0c
   real, dimension(kts:kte), intent(in   ) :: t
   real, dimension(kts:kte), intent(in   ) :: qc
   real, dimension(kts:kte), intent(in   ) :: qi
   real, dimension(kts:kte), intent(in   ) :: qs
   real, dimension(kts:kte), intent(in   ) :: qg
   real, dimension(kts:kte), intent(in   ) :: nc
   real, dimension(kts:kte), intent(in   ) :: qccps
   real, dimension(kts:kte), intent(in   ) :: rho
   real, dimension(kts:kte), intent(inout) :: re_qc
   real, dimension(kts:kte), intent(inout) :: re_qi
   real, dimension(kts:kte), intent(inout) :: re_qs
   logical                 , intent(in   ) :: f_qnc
!
! local variables
!
   integer                  :: i,k
   integer                  :: kte_in
   integer                  :: index
   real                     :: cdm2
   real                     :: temp
   real                     :: supcol, n0sfac, lamdas
   real                     :: diai      ! diameter of ice in m
   real                     :: corr
   real                     :: pidnc, pidn0s
   real                     :: pidn0g
   real                     :: lamdag
   double precision         :: lamc
   double precision         :: lammps
   logical                  :: has_qc, has_qi, has_qs
   real, dimension(kts:kte) :: ni
   real, dimension(kts:kte) :: rqc
   real, dimension(kts:kte) :: rnc
   real, dimension(kts:kte) :: rqi
   real, dimension(kts:kte) :: rni
   real, dimension(kts:kte) :: rqs
   real, dimension(kts:kte) :: rqg
   real, dimension(kts:kte) :: qsqg
   real, dimension(kts:kte) :: re_qg
   real, dimension(kts:kte) :: qcmps
   real, dimension(kts:kte) :: rqcmps
   real, dimension(kts:kte) :: nccps
!
! minimum microphys values
!
   real, parameter          :: r0 = 0.
   real, parameter          :: r1 = 1.e-12
   real, parameter          :: r2 = 1.e-6
!
! mass power law relations:  mass = am*d**bm
!
   real, parameter          :: bm_r = 3.0
   real, parameter          :: obmr = 1.0/bm_r
   real, parameter          :: cdm  = 5./3.
!-------------------------------------------------------------------------------
   has_qc = .false.   
   has_qi = .false.   
   has_qs = .false.

   ni=1.e3
   rnc=0.
   rni=0.
   rqc=0.
   rqi=0.
   rqs=0.
   rqg=0.
   qsqg=0.
   re_qg=25.e-6 
   qcmps=0.
   rqcmps=0.
   nccps=0.  

!
   kte_in = kte
   pidnc  = pi*denr/6.
   pidn0s = pi*dens*n0s
   pidn0g = pi*deng*n0g
   cdm2 = rgmma(cdm)
!
   do k = kts,kte_in
!
! for cloud
!
     rqc(k) = max(r1,qc(k)*rho(k))
     qcmps(k)  = qc(k)-qccps(k)
     rqcmps(k) = max(r1,qcmps(k)*rho(k))
     if(f_qnc) then
       lammps    = 2.*cdm2*(pidnc*MAX(r0,nc(k))/rqcmps(k))**obmr
     else
       lammps   = (pidnc*nc0/rqcmps(k))**obmr
     endif
     nccps(k)  = qccps(k)*6./pi*denr/rho(k)*lammps**3.
     rnc(k)    = max(r2,(nc(k)+nccps(k))*rho(k))
     if (rqc(k).gt.r1.and.rnc(k).gt.r2) has_qc = .true.
!
! for ice
!
     rqi(k) = max(r1,qi(k)*rho(k))
     temp   = (rho(k)*max(qi(k),qmin))
     temp   = sqrt(sqrt(temp*temp*temp))
     ni(k)  = min(max(5.38e7*temp,1.e3),1.e6)
     rni(k) = max(r2,ni(k)*rho(k))
     if (rqi(k).gt.r1.and.rni(k).gt.r2) has_qi = .true.
!
! for snow
!
     rqs(k) = max(r1,qs(k)*rho(k))
     rqg(k) = max(r1,qg(k)*rho(k))
     if (rqs(k).gt.r1.or.rqg(k).gt.r1) has_qs = .true.
   enddo
!
   if (has_qc) then
     do k = kts,kte_in
       if (rqc(k).le.r1.or.rnc(k).le.r2) cycle
       lamc = 2.*cdm2*(pidnc*(MAX(r0,nc(k))+nccps(k))/rqc(k))**obmr
       re_qc(k) =  max(2.51e-6,min(sngl(3.0/lamc),50.e-6))
     enddo
   endif
!
   if (has_qi) then
     do k = kts,kte_in
       if (rqi(k).le.r1.or.rni(k).le.r2) cycle
       diai = 11.9*sqrt(rqi(k)/ni(k))
       re_qi(k) = max(10.01e-6,min(0.75*0.163*diai,125.e-6))
     enddo
   endif
!
   if (has_qs) then
     do k = kts,kte_in
       if (rqs(k).le.r1) cycle
       supcol = t0c-t(k)
       n0sfac = max(min(exp(alpha*supcol),n0smax/n0s),1.)
       lamdas = sqrt(sqrt(pidn0s*n0sfac/rqs(k)))
       re_qs(k) = max(25.e-6,min(0.5*(1./lamdas),999.e-6))
     enddo
!
     do k = kts,kte_in
       if (rqg(k).le.r1) cycle
       lamdag = sqrt(sqrt(pidn0g*n0g/rqg(k)))
       re_qg(k) = max(25.e-6,min(1.5*(1./lamdag),999.e-6))
     enddo
!
     do k = kts,kte_in
       qsqg(k) = max(r1,qs(k)+qg(k))
       re_qs(k) = (re_qs(k)*max(r0,qs(k))+re_qg(k)*max(r0,qg(k)))/qsqg(k)
     enddo     
   endif
!
   end subroutine effectRad
!-------------------------------------------------------------------------------
!
!
!-------------------------------------------------------------------------------
   end module module_ra_effective_radius
!-------------------------------------------------------------------------------
