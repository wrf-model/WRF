Module module_chem_plumerise_scalar
! USE module_plumerise1
  use module_model_constants
  use module_zero_plumegen_coms
  real,parameter :: rgas=r_d
  real,parameter :: cpor=1./rcp
  real,parameter :: p00=p1000mb
!  real, external:: esat_pr
CONTAINS
subroutine plumerise(m1,m2,m3,ia,iz,ja,jz,firesize,mean_fct   &
                    ,nspecies,eburn_in,eburn_out      &
                    ,up,vp,wp,theta,pp,dn0,rv,zt_rams,zm_rams)
! use module_zero_plumegen_coms, only : ucon,vcon,wcon,thtcon,rvcon,picon,tmpcon&
!                          ,dncon,prcon,zcon,zzcon,scon
  
  implicit none


  integer :: ng,m1,m2,m3,ia,iz,ja,jz,ibcon,mynum,i,j,k,iveg_ag,&
            imm,k1,k2,ixx,ispc,nspecies

  integer :: ncall = 0 
  integer :: kmt
 real,dimension(m1,nspecies), intent(out) :: eburn_out
 real,dimension(nspecies), intent(in) :: eburn_in

  real,    dimension(m1,m2,m3) :: up, vp, wp,theta,pp,dn0,rv
				 
  real,    dimension(m1)       :: zt_rams,zm_rams

  real                         :: burnt_area,STD_burnt_area,dz_flam,rhodzi,dzi
  real,    dimension(2)        :: ztopmax

  real                         :: q_smold_kgm2

! From plumerise1.F routine
  integer, parameter :: nveg_agreg      = 4
  integer, parameter :: tropical_forest = 1
  integer, parameter :: boreal_forest   = 2
  integer, parameter :: savannah        = 3

  integer, parameter :: grassland       = 4
  real, dimension(nveg_agreg) :: firesize,mean_fct

  INTEGER, PARAMETER :: wind_eff = 1


  !Fator de conversao de unidades
  !!fcu=1.	  !=> kg [gas/part] /kg [ar]
  !!fcu =1.e+12   !=> ng [gas/part] /kg [ar]
  !!real,parameter :: fcu =1.e+6 !=> mg [gas/part] /kg [ar] 
  !----------------------------------------------------------------------
  !               indexacao para o array "plume(k,i,j)" 
  ! k 
  ! 1   => area media (m^2) dos focos  em biomas floresta dentro do gribox i,j 
  ! 2   => area media (m^2) dos focos  em biomas savana dentro do gribox i,j
  ! 3   => area media (m^2) dos focos  em biomas pastagem dentro do gribox i,j
  ! 4   => desvio padrao da area media (m^2) dos focos : floresta
  ! 5   => desvio padrao da area media (m^2) dos focos : savana
  ! 6   => desvio padrao da area media (m^2) dos focos : pastagem
  ! 7 a 9 =>  sem uso
  !10(=k_CO_smold) => parte da emissao total de CO correspondente a fase smoldering
  !11, 12 e 13 => este array guarda a relacao entre
  !               qCO( flaming, floresta) e a quantidade total emitida 
  !               na fase smoldering, isto e;
  !               qCO( flaming, floresta) =  plume(11,i,j)*plume(10,i,j)
  !               qCO( flaming, savana  ) =  plume(12,i,j)*plume(10,i,j)
  !               qCO( flaming, pastagem) =  plume(13,i,j)*plume(10,i,j)
  !20(=k_PM25_smold),21,22 e 23 o mesmo para PM25               
  !
  !24-n1 =>  sem uso
  !----------------------------------------------------------------------
! print *,' Plumerise_scalar 1',ncall
  if (ncall == 0) then
    ncall = 1
    call zero_plumegen_coms
  endif
! print *,' Plumerise_scalar 1',ncall
  
    
! print *,' Plumerise_scalar 2',m1
  j=1
  i=1
! do j = ja,jz           ! loop em j
!   do i = ia,iz         ! loop em i
     
     !- if the max value of flaming is close to zero => there is not emission with
     !- plume rise => cycle

 	 do k = 1,m1
 	   ucon  (k)=up(k,i,j) 	      ! u wind
 	   vcon  (k)=vp(k,i,j) 	      ! v wind
 	   !wcon  (k)=wp(k,i,j) 	      ! w wind
 	   thtcon(k)=theta(k,i,j)	      ! pot temperature
 	   picon (k)=pp(k,i,j)                ! exner function
 	   !tmpcon(k)=thtcon(k)*picon(k)/cp   ! temperature (K)
 	   !dncon (k)=dn0(k,i,j) 	      ! dry air density (basic state)
 	   !prcon (k)=(picon(k)/cp)**cpor*p00 ! pressure (Pa)
 	   rvcon (k)=rv(k,i,j)  	      ! water vapor mixing ratio
 	   zcon  (k)=zt_rams(k)               ! termod-point height
 	   zzcon (k)=zm_rams(k)               ! W-point height
!	   print*,'PL:',k,zcon(k),picon (k),thtcon(k),1000.*rvcon (k)
 	 enddo
         do ispc=1,nspecies
	      eburn_out(1,ispc) = eburn_in(ispc)
         enddo
 	
         !- get envinronmental state (temp, water vapor mix ratio, ...)
 	 call get_env_condition(1,m1,kmt,wind_eff)
 	    
         !- loop nos 4 biomas agregados com possivel queimada 
 	 do iveg_ag=1,nveg_agreg
!         print *,'iveg_ag = ',iveg_ag,mean_fct(iveg_ag)
  

         !- verifica a existencia de emissao flaming para um bioma especifico
    	 !orig: if( plume( k_CO_smold + iveg_ag ,i,j) < 1.e-6 ) cycle
           if(mean_fct(iveg_ag) < 1.e-6 ) cycle
 
	   ! burnt area and standard deviation
	   burnt_area    = firesize(iveg_ag)
	      
	   !not em use
	   !STD_burnt_area= plume(3+iveg_ag,i,j)
	    STD_burnt_area= 0.
	    
	   !- loop nos valores  minimo e maximo da taxa de calor
	   do imm=1,2
	   
!--------------------
                !ixx=iveg_ag*10 + imm
!               print*,'i j veg=',i, j, iveg_ag,imm
!--------------------             
	      
	     !- get fire properties (burned area, plume radius, heating rates ...)
 	     call get_fire_properties(imm,iveg_ag,burnt_area,STD_burnt_area)
     
             !------  generates the plume rise    ------
	     
	     !-- only one value for eflux of GRASSLAND
       !     if(iveg_ag == GRASSLAND .and. imm == 2) then 
	     if(iveg_ag == 4         .and. imm == 2) then 
 	        ztopmax(2)=ztopmax(1)
		ztopmax(1)=zzcon(1)
!               print *,'cycle',ztopmax(1),ztopmax(2)
                cycle
	     endif
	     
	     call makeplume (kmt,ztopmax(imm),ixx,imm)	      	      
	     	   
	   enddo ! enddo do loop em imm
	   
	   !- define o dominio vertical onde a emissao flaming ira ser colocada
	   call set_flam_vert(ztopmax,k1,k2,nkp,zzcon,W_VMD,VMD)
	   
	   !- espessura da camada vertical 
	   dz_flam=zzcon(k2)-zzcon(k1-1)
	   
	   !- distribui a emissao flaming entre os niveis k1 e k2	 
!          print *,'distribui, k1,k2,dz_flam',k1,k2,dz_flam
	   do k=k1,k2
	      !use this in case the emission src is already in mixing ratio
	      !rhodzi= 1./(dn0(k,i,j) * dz_flam)
	      !use this in case the emission src is tracer density
	       dzi= 1./( dz_flam)
	   
	    do ispc = 1, nspecies
	       
	      !- get back the smoldering emission in kg/m2  (actually in 1e-9 kg/m2) 
	      
	      !use this in case the emission src is already in mixing ratio
	      !q_smold_kgm2 = (1/dzt(2) *  dn0(2,i,j)	)*   &
	      ! 	     chem1_src_g(bburn,ispc,ng)%sc_src(2,i,j)	 
	      
	      !use this in case the emission src is tracer density
!       q_smold_kgm2 = ((zt_rams(2)-zt_rams(1))                 )*   &
!        	     eburn_in(ispc)
        q_smold_kgm2 = eburn_in(ispc)
	
	      ! units = already in ppbm,  don't need "fcu" factor 
	      eburn_out(k,ispc) = eburn_out(k,ispc) +&
	    						 mean_fct(iveg_ag)  *&
	        					 q_smold_kgm2 * &
							 dzi    !use this in case the emission src is tracer density
							!rhodzi !use this in case the emission src is already in mixing ratio
!             if(ispc.eq. 1) print *,' Plume: ',k,eburn_out(k,ispc),mean_fct(iveg_ag),q_smold_kgm2,dzi
							 
							 

	      !srcCO(k,i,j)=   srcCO(k,i,j) + plume(k_CO_smold+iveg_ag,i,j)*&
	      ! 			    plume(k_CO_smold	    ,i,j)*&
	      ! 			    rhodzi*fcu
	    enddo
	   
	   enddo
	   
         enddo ! enddo do loop em iveg_ag
     !-----  
 	 !stop 333
 	 !endif  	
     !-----
!      enddo  ! loop em i
! enddo   ! loop em j
!stop 4544
end subroutine plumerise
!-------------------------------------------------------------------------

subroutine get_env_condition(k1,k2,kmt,wind_eff)

!se module_zero_plumegen_coms
!use rconstants
implicit none
integer :: k1,k2,k,kcon,klcl,kmt,nk,nkmid,i
real :: znz,themax,tlll,plll,rlll,zlll,dzdd,dzlll,tlcl,plcl,dzlcl,dummy
integer :: n_setgrid = 0 
integer :: wind_eff


if( n_setgrid == 0) then
  n_setgrid = 1
  call set_grid ! define vertical grid of plume model
                ! zt(k) =  thermo and water levels
                ! zm(k) =  dynamical levels 
endif

znz=zcon(k2)
do k=nkp,1,-1
  if(zt(k).lt.znz)go to 13
enddo
stop ' envir stop 12'
13 continue
!-srf-mb
kmt=min(k,nkp-1)

nk=k2-k1+1
!call htint(nk, wcon,zzcon,kmt,wpe,zt)
 call htint(nk,  ucon,zcon,kmt,upe,zt)
 call htint(nk,  vcon,zcon,kmt,vpe,zt)
 call htint(nk,thtcon,zcon,kmt,the  ,zt)
 call htint(nk, rvcon,zcon,kmt,qvenv,zt)
do k=1,kmt
  qvenv(k)=max(qvenv(k),1e-8)
enddo

pke(1)=picon(1)
do k=1,kmt
  thve(k)=the(k)*(1.+.61*qvenv(k)) ! virtual pot temperature
enddo
do k=2,kmt
  pke(k)=pke(k-1)-g*2.*(zt(k)-zt(k-1))  & ! exner function
        /(thve(k)+thve(k-1))
enddo
do k=1,kmt
  te(k)  = the(k)*pke(k)/cp         ! temperature (K) 
  pe(k)  = (pke(k)/cp)**cpor*p00    ! pressure (Pa)
  dne(k)= pe(k)/(rgas*te(k)*(1.+.61*qvenv(k))) !  dry air density (kg/m3)
!  print*,'ENV=',qvenv(k)*1000., te(k)-273.15,zt(k)
!-srf-mb
  vel_e(k) = sqrt(upe(k)**2+vpe(k)**2)         !-env wind (m/s)
  !print*,'k,vel_e(k),te(k)=',vel_e(k),te(k)
enddo

!-ewe - env wind effect
if(wind_eff < 1)  vel_e(1:kmt) = 0.

!-use este para gerar o RAMS.out
! ------- print environment state
!print*,'k,zt(k),pe(k),te(k)-273.15,qvenv(k)*1000'
!do k=1,kmt
! write(*,100)  k,zt(k),pe(k),te(k)-273.15,qvenv(k)*1000.
! 100 format(1x,I5,4f20.12)
!enddo
!stop 333


!--------- nao eh necessario este calculo
!do k=1,kmt
!  call thetae(pe(k),te(k),qvenv(k),thee(k))
!enddo


!--------- converte press de Pa para kPa para uso modelo de plumerise
do k=1,kmt
 pe(k) = pe(k)*1.e-3
enddo 

return 
end subroutine get_env_condition

!-------------------------------------------------------------------------

subroutine set_grid()
!use module_zero_plumegen_coms  
implicit none
integer :: k,mzp

dz=100. ! set constant grid spacing of plume grid model(meters)

mzp=nkp
zt(1) = zsurf
zm(1) = zsurf
zt(2) = zt(1) + 0.5*dz
zm(2) = zm(1) + dz
do k=3,mzp
 zt(k) = zt(k-1) + dz ! thermo and water levels
 zm(k) = zm(k-1) + dz ! dynamical levels	
enddo
!print*,zsurf
!Print*,zt(:)
do k = 1,mzp-1
   dzm(k) = 1. / (zt(k+1) - zt(k))
enddo 
dzm(mzp)=dzm(mzp-1)

do k = 2,mzp
   dzt(k) = 1. / (zm(k) - zm(k-1))
enddo
dzt(1) = dzt(2) * dzt(2) / dzt(3)
   
!   dzm(1) = 0.5/dz
!   dzm(2:mzp) = 1./dz
return
end subroutine set_grid
!-------------------------------------------------------------------------

  SUBROUTINE set_flam_vert(ztopmax,k1,k2,nkp,zzcon,W_VMD,VMD)

    REAL    , INTENT(IN)  :: ztopmax(2)
    INTEGER , INTENT(OUT) :: k1
    INTEGER , INTENT(OUT) :: k2

    ! plumegen_coms
    INTEGER , INTENT(IN)  :: nkp
    REAL    , INTENT(IN)  :: zzcon(nkp)

    INTEGER imm,k
    INTEGER, DIMENSION(2)  :: k_lim

    !- version 2
    REAL    , INTENT(IN)  :: W_VMD(nkp,2)
    REAL    , INTENT(OUT) ::   VMD(nkp,2)
    real   w_thresold,xxx
    integer k_initial,k_final,ko,kk4,kl

    !- version 1
    DO imm=1,2
       ! checar 
       !    do k=1,m1-1
       DO k=1,nkp-1
          IF(zzcon(k) > ztopmax(imm) ) EXIT
       ENDDO
       k_lim(imm) = k
    ENDDO
    k1=MAX(3,k_lim(1))
    k2=MAX(3,k_lim(2))

    IF(k2 < k1) THEN
       !print*,'1: ztopmax k=',ztopmax(1), k1
       !print*,'2: ztopmax k=',ztopmax(2), k2
       k2=k1
       !stop 1234
    ENDIF
    
    !- version 2    
    !- vertical mass distribution
    !- 
    w_thresold = 1.
    DO imm=1,2

    
       VMD(1:nkp,imm)= 0.
       xxx=0.
       k_initial= 0
       k_final  = 0
    
       !- define range of the upper detrainemnt layer
       do ko=nkp-10,2,-1
     
        if(w_vmd(ko,imm) < w_thresold) cycle
     
        if(k_final==0) k_final=ko
     
        if(w_vmd(ko,imm)-1. > w_vmd(ko-1,imm)) then
          k_initial=ko
          exit
        endif
      
       enddo
       !- if there is a non zero depth layer, make the mass vertical distribution 
       if(k_final > 0 .and. k_initial > 0) then 
       
           k_initial=int((k_final+k_initial)*0.5)
       
           !- parabolic vertical distribution between k_initial and k_final
           kk4 = k_final-k_initial+2
           do ko=1,kk4-1
               kl=ko+k_initial-1
               VMD(kl,imm) = 6.* float(ko)/float(kk4)**2 * (1. - float(ko)/float(kk4))
           enddo
	   if(sum(VMD(1:NKP,imm)) .ne. 1.) then
 	       xxx= ( 1.- sum(VMD(1:NKP,imm)) )/float(k_final-k_initial+1)
 	       do ko=k_initial,k_final
 	         VMD(ko,imm) = VMD(ko,imm)+ xxx !- values between 0 and 1.
 	       enddo
               ! print*,'new mass=',sum(mass)*100.,xxx
               !pause
           endif
        endif !k_final > 0 .and. k_initial > 

    ENDDO
    
  END SUBROUTINE set_flam_vert
!-------------------------------------------------------------------------

subroutine get_fire_properties(imm,iveg_ag,burnt_area,STD_burnt_area)
!use module_zero_plumegen_coms  
implicit none
integer ::  moist,  i,  icount,imm,iveg_ag
real::   bfract,  effload,  heat,  hinc ,burnt_area,STD_burnt_area,heat_fluxW
real,    dimension(2,4) :: heat_flux
INTEGER, parameter :: use_last = 0


data heat_flux/  &
!---------------------------------------------------------------------
!  heat flux      !IGBP Land Cover	    ! 
! min  ! max      !Legend and		    ! reference
!    kW/m^2       !description  	    ! 
!--------------------------------------------------------------------
 30.0,	 80.0,   &! Tropical Forest         ! igbp 2 & 4
 30.0,   80.0,   &! Boreal forest           ! igbp 1 & 3
  4.4,	 23.0,   &! cerrado/woody savanna   | igbp  5 thru 9
  3.3,	  3.3    /! Grassland/cropland      ! igbp 10 thru 17
!--------------------------------------------------------------------


!-- fire at the surface
!
!area = 20.e+4   ! area of burn, m^2
area = burnt_area! area of burn, m^2

!fluxo de calor para o bioma
heat_fluxW = heat_flux(imm,iveg_ag) * 1000. ! converte para W/m^2

mdur = 53        ! duration of burn, minutes
bload = 10.      ! total loading, kg/m**2 
moist = 10       ! fuel moisture, %. average fuel moisture,percent dry
maxtime =mdur+2  ! model time, min
!heat = 21.e6    !- joules per kg of fuel consumed                   
!heat = 15.5e6   !joules/kg - cerrado
heat = 19.3e6    !joules/kg - floresta em alta floresta (mt)
!alpha = 0.1      !- entrainment constant
alpha = 0.05      !- entrainment constant

!-------------------- printout ----------------------------------------

!!WRITE ( * ,  * ) ' SURFACE =', ZSURF, 'M', '  LCL =', ZBASE, 'M'  
!
!PRINT*,'======================================================='
!print * , ' FIRE BOUNDARY CONDITION   :'  
!print * , ' DURATION OF BURN, MINUTES =',MDUR  
!print * , ' AREA OF BURN, HA	      =',AREA*1.e-4
!print * , ' HEAT FLUX, kW/m^2	      =',heat_fluxW*1.e-3
!print * , ' TOTAL LOADING, KG/M**2    =',BLOAD  
!print * , ' FUEL MOISTURE, %	      =',MOIST !average fuel moisture,percent dry
!print * , ' MODEL TIME, MIN.	      =',MAXTIME  
!
!
!
! ******************** fix up inputs *********************************
!
                                             
!IF (MOD (MAXTIME, 2) .NE.0) MAXTIME = MAXTIME+1  !make maxtime even
                                                  
MAXTIME = MAXTIME * 60  ! and put in seconds
!
RSURF = SQRT (AREA / 3.14159) !- entrainment surface radius (m)

FMOIST   = MOIST / 100.       !- fuel moisture fraction
!
!
! calculate the energy flux and water content at lboundary.
! fills heating() on a minute basis. could ask for a file at this po
! in the program. whatever is input has to be adjusted to a one
! minute timescale.
!
                        
  DO I = 1, ntime         !- make sure of energy release
    HEATING (I) = 0.0001  !- avoid possible divide by 0
  enddo  
!                                  
  TDUR = MDUR * 60.       !- number of seconds in the burn

  bfract = 1.             !- combustion factor

  EFFLOAD = BLOAD * BFRACT  !- patchy burning
  
!     spread the burning evenly over the interval
!     except for the first few minutes for stability
  ICOUNT = 1  
!
  if(MDUR > NTIME) STOP 'Increase time duration (ntime) in min - see file "plumerise_mod.f90"'

  DO WHILE (ICOUNT.LE.MDUR)                             
!  HEATING (ICOUNT) = HEAT * EFFLOAD / TDUR  ! W/m**2 
!  HEATING (ICOUNT) = 80000.  * 0.55         ! W/m**2 

   HEATING (ICOUNT) = heat_fluxW  * 0.55     ! W/m**2 (0.55 converte para energia convectiva)
   ICOUNT = ICOUNT + 1  
  ENDDO  
!     ramp for 5 minutes
 IF(use_last /= 1) THEN

    HINC = HEATING (1) / 4.  
    HEATING (1) = 0.1  
    HEATING (2) = HINC  
    HEATING (3) = 2. * HINC  
    HEATING (4) = 3. * HINC  
 ELSE
    IF(imm==1) THEN
       HINC = HEATING (1) / 4.  
       HEATING (1) = 0.1  
       HEATING (2) = HINC  
       HEATING (3) = 2. * HINC  
       HEATING (4) = 3. * HINC 
    ELSE 
       HINC = (HEATING (1) - heat_flux(imm-1,iveg_ag) * 1000. *0.55)/ 4.
       HEATING (1) = heat_flux(imm-1,iveg_ag) * 1000. *0.55 + 0.1  
       HEATING (2) = HEATING (1)+ HINC  
       HEATING (3) = HEATING (2)+ HINC  
       HEATING (4) = HEATING (3)+ HINC 
    ENDIF
 ENDIF

return
end subroutine get_fire_properties
!-------------------------------------------------------------------------------
!
SUBROUTINE MAKEPLUME ( kmt,ztopmax,ixx,imm)  
!
! *********************************************************************
!
!    EQUATION SOURCE--Kessler Met.Monograph No. 32 V.10 (K)
!    Alan Weinstein, JAS V.27 pp 246-255. (W),
!    Ogura and Takahashi, Monthly Weather Review V.99,pp895-911 (OT)
!    Roger Pielke,Mesoscale Meteorological Modeling,Academic Press,1984
!    Originally developed by: Don Latham (USFS)
!
!
! ************************ VARIABLE ID ********************************
!
!     DT=COMPUTING TIME INCREMENT (SEC)
!     DZ=VERTICAL INCREMENT (M)
!     LBASE=LEVEL ,CLOUD BASE
!
!     CONSTANTS:
!       G = GRAVITATIONAL ACCELERATION 9.80796 (M/SEC/SEC).
!       R = DRY AIR GAS CONSTANT (287.04E6 JOULE/KG/DEG K)
!       CP = SPECIFIC HT. (1004 JOULE/KG/DEG K)
!       HEATCOND = HEAT OF CONDENSATION (2.5E6 JOULE/KG)
!       HEATFUS = HEAT OF FUSION (3.336E5 JOULE/KG)
!       HEATSUBL = HEAT OF SUBLIMATION (2.83396E6 JOULE/KG)
!       EPS = RATIO OF MOL.WT. OF WATER VAPOR TO THAT OF DRY AIR (0.622)
!       DES = DIFFERENCE BETWEEN VAPOR PRESSURE OVER WATER AND ICE (MB)
!       TFREEZE = FREEZING TEMPERATURE (K)
!
!
!     PARCEL VALUES:
!       T = TEMPERATURE (K)
!       TXS = TEMPERATURE EXCESS (K)
!       QH = HYDROMETEOR WATER CONTENT (G/G DRY AIR)
!       QHI = HYDROMETEOR ICE CONTENT (G/G DRY AIR)
!       QC = WATER CONTENT (G/G DRY AIR)
!       QVAP = WATER VAPOR MIXING RATIO (G/G DRY AIR)
!       QSAT = SATURATION MIXING RATIO (G/G DRY AIR)
!       RHO = DRY AIR DENSITY (G/M**3) MASSES = RHO*Q'S IN G/M**3
!       ES = SATURATION VAPOR PRESSURE (kPa)
!
!     ENVIRONMENT VALUES:
!       TE = TEMPERATURE (K)
!       PE = PRESSURE (kPa)
!       QVENV = WATER VAPOR (G/G)
!       RHE = RELATIVE HUMIDITY FRACTION (e/esat)
!       DNE = dry air density (kg/m^3)
!
!     HEAT VALUES:
!       HEATING = HEAT OUTPUT OF FIRE (WATTS/M**2)
!       MDUR = DURATION OF BURN, MINUTES
!
!       W = VERTICAL VELOCITY (M/S)
!       RADIUS=ENTRAINMENT RADIUS (FCN OF Z)
!	RSURF = ENTRAINMENT RADIUS AT GROUND (SIMPLE PLUME, TURNER)
!	ALPHA = ENTRAINMENT CONSTANT
!       MAXTIME = TERMINATION TIME (MIN)
!
!
!**********************************************************************
!**********************************************************************               
!use module_zero_plumegen_coms 
implicit none 
!logical :: endspace  
character (len=10) :: varn
integer ::  izprint, iconv,  itime, k, kk, kkmax, deltak,ilastprint,kmt &
           ,ixx,nrectotal,i_micro,n_sub_step
real ::  vc, g,  r,  cp,  eps,  &
         tmelt,  heatsubl,  heatfus,  heatcond, tfreeze, &
         ztopmax, wmax, rmaxtime, es, esat, heat,dt_save !ESAT_PR,
character (len=2) :: cixx	 
! Set threshold to be the same as dz=100., the constant grid spacing of plume grid model(meters) found in set_grid()
    REAL :: DELZ_THRESOLD = 100. 

    INTEGER     :: imm

!  real, external:: esat_pr!
!
! ******************* SOME CONSTANTS **********************************
!
!      XNO=10.0E06 median volume diameter raindrop (K table 4)
!      VC = 38.3/(XNO**.125) mean volume fallspeed eqn. (K)
!
parameter (vc = 5.107387)  
parameter (g = 9.80796, r = 287.04, cp = 1004., eps = 0.622,  tmelt = 273.3)
parameter (heatsubl = 2.834e6, heatfus = 3.34e5, heatcond = 2.501e6)
parameter (tfreeze = 269.3)  
!
tstpf = 2.0  	!- timestep factor
viscosity = 500.!- viscosity constant (original value: 0.001)

nrectotal=150
!
!*************** PROBLEM SETUP AND INITIAL CONDITIONS *****************
mintime = 1  
ztopmax = 0. 
ztop    = 0. 
   time = 0.  
     dt = 1.
   wmax = 1. 
kkmax   = 10
deltaK  = 20
ilastprint=0
L       = 1   ! L initialization

!--- initialization
CALL INITIAL(kmt)  

!--- initial print fields:
izprint  = 0          ! if = 0 => no printout
if (izprint.ne.0) then
 write(cixx(1:2),'(i2.2)') ixx
 open(2, file = 'debug.'//cixx//'.dat')  
 open(19,file='plumegen9.'//cixx//'.gra',         &
     form='unformatted',access='direct',status='unknown',  &
     recl=4*nrectotal)  !PC   
!     recl=1*nrectotal) !sx6 e tupay
 call printout (izprint,nrectotal)
 ilastprint=2
endif     

! ******************* model evolution ******************************
rmaxtime = float(maxtime)
!
!print * ,' TIME=',time,' RMAXTIME=',rmaxtime
!print*,'======================================================='
 DO WHILE (TIME.LE.RMAXTIME)  !beginning of time loop

!   do itime=1,120

!-- set model top integration
    nm1 = min(kmt, kkmax + deltak)
                                    
!-- set timestep
    !dt = (zm(2)-zm(1)) / (tstpf * wmax)  
    dt = min(5.,(zm(2)-zm(1)) / (tstpf * wmax))
                                
!-- elapsed time, sec
    time = time+dt 
!-- elapsed time, minutes                                      
    mintime = 1 + int (time) / 60     
    wmax = 1.  !no zeroes allowed.
!************************** BEGIN SPACE LOOP **************************

!-- zerout all model tendencies
    call tend0_plumerise

!-- bounday conditions (k=1)
    L=1
    call lbound()

!-- dynamics for the level k>1 
!-- W advection 
!   call vel_advectc_plumerise(NM1,WC,WT,DNE,DZM)
    call vel_advectc_plumerise(NM1,WC,WT,RHO,DZM)
  
!-- scalars advection 1
    call scl_advectc_plumerise('SC',NM1)

!-- scalars advection 2
    !call scl_advectc_plumerise2('SC',NM1)

!-- scalars entrainment, adiabatic
    call scl_misc(NM1)
    
!-- scalars dinamic entrainment
     call  scl_dyn_entrain(NM1,nkp,wbar,w,adiabat,alpha,radius,tt,t,te,qvt,qv,qvenv,qct,qc,qht,qh,qit,qi,&
                    vel_e,vel_p,vel_t,rad_p,rad_t)

!-- gravity wave damping using Rayleigh friction layer fot T
    call damp_grav_wave(1,nm1,deltak,dt,zt,zm,w,t,tt,qv,qh,qi,qc,te,pe,qvenv)

!-- microphysics
!   goto 101 ! bypass microphysics
    dt_save=dt
    n_sub_step=3
    dt=dt/float(n_sub_step)

    do i_micro=1,n_sub_step
!-- sedim ?
     call fallpart(NM1)
!-- microphysics
     do L=2,nm1-1
        WBAR    = 0.5*(W(L)+W(L-1))
        ES      = ESAT_PR (T(L))            !BLOB SATURATION VAPOR PRESSURE, EM KPA
        QSAT(L) = (EPS * ES) / (PE(L) - ES)  !BLOB SATURATION LWC G/G DRY AIR
        EST (L) = ES  
        RHO (L) = 3483.8 * PE (L) / T (L) ! AIR PARCEL DENSITY , G/M**3
!srf18jun2005
!	IF (W(L) .ge. 0.) DQSDZ = (QSAT(L  ) - QSAT(L-1)) / (ZT(L  ) -ZT(L-1))
!	IF (W(L) .lt. 0.) DQSDZ = (QSAT(L+1) - QSAT(L  )) / (ZT(L+1) -ZT(L  ))
	IF (W(L) .ge. 0.) then 
	   DQSDZ = (QSAT(L+1) - QSAT(L-1)) / (ZT(L+1 )-ZT(L-1))
	ELSE
	   DQSDZ = (QSAT(L+1) - QSAT(L-1)) / (ZT(L+1) -ZT(L-1))
	ENDIF 
	
	call waterbal  
     enddo
    enddo
    dt=dt_save
!
    101 continue
!
!-- W-viscosity for stability 
    call visc_W(nm1,deltak,kmt)

!-- update scalars
    call update_plumerise(nm1,'S')
    
    call hadvance_plumerise(1,nm1,dt,WC,WT,W,mintime) 

!-- Buoyancy
    call buoyancy_plumerise(NM1, T, TE, QV, QVENV, QH, QI, QC, WT, SCR1)
 
!-- Entrainment 
    call entrainment(NM1,W,WT,RADIUS,ALPHA)

!-- update W
    call update_plumerise(nm1,'W')

    call hadvance_plumerise(2,nm1,dt,WC,WT,W,mintime) 


!-- misc
    do k=2,nm1
!    pe esta em kpa  - esat do rams esta em mbar = 100 Pa = 0.1 kpa
!    es       = 0.1*esat (t(k)) !blob saturation vapor pressure, em kPa
!    rotina do plumegen calcula em kPa
     es       = esat_pr (t(k))  !blob saturation vapor pressure, em kPa
     qsat(k) = (eps * es) / (pe(k) - es)  !blob saturation lwc g/g dry air
     est (k) = es  
     txs (k) = t(k) - te(k)
     rho (k) = 3483.8 * pe (k) / t (k) ! air parcel density , g/m**3
                                       ! no pressure diff with radius
				       
     if((abs(wc(k))).gt.wmax) wmax = abs(wc(k)) ! keep wmax largest w
    enddo  

! Gravity wave damping using Rayleigh friction layer for W
    call damp_grav_wave(2,nm1,deltak,dt,zt,zm,w,t,tt,qv,qh,qi,qc,te,pe,qvenv)
!---
       !- update radius
       do k=2,nm1
        radius(k) = rad_p(k)
       enddo
      !-- try to find the plume top (above surface height)
       kk = 1
       DO WHILE (w (kk) .GT. 1.)  
          kk = kk + 1  
          ztop =  zm(kk) 
          !print*,'W=',w (kk)
       ENDDO
       !
       ztop_(mintime) = ztop
       ztopmax = MAX (ztop, ztopmax) 
       kkmax   = MAX (kk  , kkmax  ) 
       !print * ,'ztopmax=', mintime,'mn ',ztop_(mintime), ztopmax

       !
       ! if the solution is going to a stationary phase, exit
       IF(mintime > 10) THEN                 
          !   if(mintime > 20) then                     
          !    if( abs(ztop_(mintime)-ztop_(mintime-10)) < DZ ) exit   
          IF( ABS(ztop_(mintime)-ztop_(mintime-10)) < DELZ_THRESOLD) then 
	   	  
	   	  !- determine W parameter to determine the VMD
           	  do k=2,nm1
	   	   W_VMD(k,imm) = w(k)
           	  enddo
	   	  EXIT ! finish the integration
	   ENDIF  
       ENDIF


    if(ilastprint == mintime) then
      call printout (izprint,nrectotal)  
      ilastprint = mintime+1
    endif      
                               

ENDDO   !do next timestep

!print * ,' ztopmax=',ztopmax,'m',mintime,'mn '
!print*,'======================================================='
!
!the last printout
if (izprint.ne.0) then
 call printout (izprint,nrectotal)  
 close (2)            
 close (19)            
endif

RETURN  
END SUBROUTINE MAKEPLUME
!-------------------------------------------------------------------------------
!
SUBROUTINE BURN(EFLUX, WATER)  
!	
!- calculates the energy flux and water content at lboundary
!use module_zero_plumegen_coms                               
!real, parameter :: HEAT = 21.E6 !Joules/kg
!real, parameter :: HEAT = 15.5E6 !Joules/kg - cerrado
real, parameter :: HEAT = 19.3E6 !Joules/kg - floresta em Alta Floresta (MT)
real :: eflux,water
!
! The emission factor for water is 0.5. The water produced, in kg,
! is then  fuel mass*0.5 + (moist/100)*mass per square meter.
! The fire burns for DT out of TDUR seconds, the total amount of
! fuel burned is AREA*BLOAD*(DT/TDUR) kg. this amount of fuel is
! considered to be spread over area AREA and so the mass burned per
! unit area is BLOAD*(DT/TDUR), and the rate is BLOAD/TDUR.
!        
IF (TIME.GT.TDUR) THEN !is the burn over?   
   EFLUX = 0.000001    !prevent a potential divide by zero
   WATER = 0.  
   RETURN  
ELSE  
!                                                   
   EFLUX = HEATING (MINTIME)                          ! Watts/m**2                                                   
!  WATER = EFLUX * (DT / HEAT) * (0.5 + FMOIST)       ! kg/m**2 
   WATER = EFLUX * (DT / HEAT) * (0.5 + FMOIST) /0.55 ! kg/m**2 
   WATER = WATER * 1000.                              ! g/m**2
!
!        print*,'BURN:',time,EFLUX/1.e+9
ENDIF  
!
RETURN  
END SUBROUTINE BURN
!-------------------------------------------------------------------------------
!
SUBROUTINE LBOUND ()  
!
! ********** BOUNDARY CONDITIONS AT ZSURF FOR PLUME AND CLOUD ********
!
! source of equations: J.S. Turner Buoyancy Effects in Fluids
!                      Cambridge U.P. 1973 p.172,
!                      G.A. Briggs Plume Rise, USAtomic Energy Commissio
!                      TID-25075, 1969, P.28
!
! fundamentally a point source below ground. at surface, this produces
! a velocity w(1) and temperature T(1) which vary with time. There is
! also a water load which will first saturate, then remainder go into
! QC(1).
! EFLUX = energy flux at ground,watt/m**2 for the last DT
!
!use module_zero_plumegen_coms  
implicit none
real, parameter :: g = 9.80796, r = 287.04, cp = 1004.6, eps = 0.622,tmelt = 273.3
real, parameter :: tfreeze = 269.3, pi = 3.14159, e1 = 1./3., e2 = 5./3.
real :: es,  esat, eflux, water,  pres, c1,  c2, f, zv,  denscor, xwater !,ESAT_PR
!  real, external:: esat_pr!

!            
QH (1) = QH (2)   !soak up hydrometeors
QI (1) = QI (2)              
QC (1) = 0.       !no cloud here
!
!
   CALL BURN (EFLUX, WATER)  
!
!  calculate parameters at boundary from a virtual buoyancy point source
!
   PRES = PE (1) * 1000.   !need pressure in N/m**2
                              
   C1 = 5. / (6. * ALPHA)  !alpha is entrainment constant

   C2 = 0.9 * ALPHA  

   F = EFLUX / (PRES * CP * PI)  
                             
   F = G * R * F * AREA  !buoyancy flux
                 
   ZV = C1 * RSURF  !virtual boundary height
                                   
   W (1) = C1 * ( (C2 * F) **E1) / ZV**E1  !boundary velocity
                                         
   DENSCOR = C1 * F / G / (C2 * F) **E1 / ZV**E2   !density correction

   T (1) = TE (1) / (1. - DENSCOR)    !temperature of virtual plume at zsurf
   
!
   WC(1) = W(1)
    VEL_P(1) = 0.
    rad_p(1) = rsurf

   !SC(1) = SCE(1)+F/1000.*dt  ! gas/particle (g/g)

! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!     match dw/dz,dt/dz at the boundary. F is conserved.
!
   !WBAR = W (1) * (1. - 1. / (6. * ZV) )  
   !ADVW = WBAR * W (1) / (3. * ZV)  
   !ADVT = WBAR * (5. / (3. * ZV) ) * (DENSCOR / (1. - DENSCOR) )  
   !ADVC = 0.  
   !ADVH = 0.  
   !ADVI = 0.  
   !ADIABAT = - WBAR * G / CP  
   VTH (1) = - 4.  
   VTI (1) = - 3.  
   TXS (1) = T (1) - TE (1)  

   VISC (1) = VISCOSITY  

   RHO (1) = 3483.8 * PE (1) / T (1)   !air density at level 1, g/m**3

   XWATER = WATER / (W (1) * DT * RHO (1) )   !firewater mixing ratio
                                            
   QV (1) = XWATER + QVENV (1)  !plus what's already there 


!  PE esta em kPa  - ESAT do RAMS esta em mbar = 100 Pa = 0.1 kPa
!  ES       = 0.1*ESAT (T(1)) !blob saturation vapor pressure, em kPa
!  rotina do plumegen ja calcula em kPa
   ES       = ESAT_PR (T(1))  !blob saturation vapor pressure, em kPa

   EST  (1)  = ES                                  
   QSAT (1) = (EPS * ES) / (PE (1) - ES)   !blob saturation lwc g/g dry air
  
   IF (QV (1) .gt. QSAT (1) ) THEN  
       QC (1) = QV   (1) - QSAT (1) + QC (1)  !remainder goes into cloud drops
       QV (1) = QSAT (1)  
   ENDIF  
!
   CALL WATERBAL  
!
RETURN  
END SUBROUTINE LBOUND
!-------------------------------------------------------------------------------
!
SUBROUTINE INITIAL ( kmt)  
!
! ************* SETS UP INITIAL CONDITIONS FOR THE PROBLEM ************
!use module_zero_plumegen_coms 
implicit none 
real, parameter :: tfreeze = 269.3
integer ::  isub,  k,  n1,  n2,  n3,  lbuoy,  itmp,  isubm1 ,kmt
real ::     xn1,  xi,  es,  esat!,ESAT_PR
!
N=kmt
! initialize temperature structure,to the end of equal spaced sounding,
  do k = 1, N			  
  TXS (k) = 0.0  
    W (k) = 0.0             
    T (k) = TE(k)       !blob set to environment		  
    WC(k) = 0.0
    WT(k) = 0.0
    QV(k) = QVENV (k)   !blob set to environment             
   VTH(k) = 0.		!initial rain velocity = 0	                     
   VTI(k) = 0.		!initial ice  velocity = 0	                     
    QH(k) = 0.		!no rain			     
    QI(k) = 0.		!no ice 			     
    QC(k) = 0.		!no cloud drops	                     
!  PE esta em kPa  - ESAT do RAMS esta em mbar = 100 Pa = 0.1 kPa
!  ES       = 0.1*ESAT (T(k)) !blob saturation vapor pressure, em kPa
!  rotina do plumegen calcula em kPa
   ES       = ESAT_PR (T(k))  !blob saturation vapor pressure, em kPa
   EST  (k) = ES  
   QSAT (k) = (.622 * ES) / (PE (k) - ES) !saturation lwc g/g
   RHO  (k) = 3483.8 * PE (k) / T (k) 	!dry air density g/m**3    
       VEL_P(k) = 0.
       rad_p(k) = 0.
  enddo  

! Initialize the entrainment radius, Turner-style plume
  radius(1) = rsurf
  do k=2,N
     radius(k) = radius(k-1)+(6./5.)*alpha*(zt(k)-zt(k-1))
  enddo
! Initialize the entrainment radius, Turner-style plume
    radius(1) = rsurf
    rad_p(1)  = rsurf
    DO k=2,N
       radius(k) = radius(k-1)+(6./5.)*alpha*(zt(k)-zt(k-1))
       rad_p(k)  = radius(k)
   ENDDO
    
!  Initialize the viscosity
   VISC (1) = VISCOSITY
   do k=2,N
     !VISC (k) = VISCOSITY!max(1.e-3,visc(k-1) - 1.* VISCOSITY/float(nkp))
     VISC (k) = max(1.e-3,visc(k-1) - 1.* VISCOSITY/float(nkp))
   enddo
!--   Initialize gas/concentration
  !DO k =10,20
  !   SC(k) = 20.
  !ENDDO
  !stop 333

   CALL LBOUND()

RETURN  
END SUBROUTINE INITIAL
!-------------------------------------------------------------------------------
!
subroutine damp_grav_wave(ifrom,nm1,deltak,dt,zt,zm,w,t,tt,qv,qh,qi,qc,te,pe,qvenv)
implicit none
integer nm1,ifrom,deltak
real dt
real, dimension(nm1) :: w,t,tt,qv,qh,qi,qc,te,pe,qvenv,dummy,zt,zm

if(ifrom==1) then
 call friction(ifrom,nm1,deltak,dt,zt,zm,t,tt    ,te)
!call friction(ifrom,nm1,dt,zt,zm,qv,qvt,qvenv)
 return
endif 

dummy(:) = 0.
if(ifrom==2) call friction(ifrom,nm1,deltak,dt,zt,zm,w,dummy ,dummy)
!call friction(ifrom,nm1,dt,zt,zm,qi,qit ,dummy)
!call friction(ifrom,nm1,dt,zt,zm,qh,qht ,dummy)
!call friction(ifrom,nm1,dt,zt,zm,qc,qct ,dummy)
return
end subroutine damp_grav_wave
!-------------------------------------------------------------------------------
!
subroutine friction(ifrom,nm1,deltak,dt,zt,zm,var1,vart,var2)
implicit none
real, dimension(nm1) :: var1,var2,vart,zt,zm
integer k,nfpt,kf,nm1,ifrom,deltak
real zmkf,ztop,distim,c1,c2,dt

!nfpt=50
!kf = nm1 - nfpt
!kf = nm1 - int(deltak/2)
 kf = nm1 - int(deltak)

zmkf = zm(kf) !old: float(kf )*dz
ztop = zm(nm1)
!distim = min(4.*dt,200.)
!distim = 60.
 distim = min(3.*dt,60.)

c1 = 1. / (distim * (ztop - zmkf))
c2 = dt * c1

if(ifrom == 1) then  
  do k = nm1,2,-1
   if (zt(k) .le. zmkf) cycle
   vart(k) = vart(k)   + c1 * (zt(k) - zmkf)*(var2(k) - var1(k))
  enddo
elseif(ifrom == 2) then
  do k = nm1,2,-1
   if (zt(k) .le. zmkf) cycle
   var1(k) =  var1(k) + c2 * (zt(k) - zmkf)*(var2(k) - var1(k))
  enddo
endif
return
end subroutine friction
!-------------------------------------------------------------------------------
!
subroutine vel_advectc_plumerise(m1,wc,wt,rho,dzm)

implicit none
integer :: k,m1
real, dimension(m1) :: wc,wt,flxw,dzm,rho
real, dimension(m1) :: dn0 ! var local
real :: c1z

!dzm(:)= 1./dz

dn0(1:m1)=rho(1:m1)*1.e-3 ! converte de cgs para mks

flxw(1) = wc(1) * dn0(1) 

do k = 2,m1-1
   flxw(k) = wc(k) * .5 * (dn0(k) + dn0(k+1))
enddo

! Compute advection contribution to W tendency

c1z = .5 

do k = 2,m1-2

   wt(k) = wt(k)  &
      + c1z * dzm(k) / (dn0(k) + dn0(k+1)) *     (   &
	(flxw(k) + flxw(k-1))  * (wc(k) + wc(k-1))   &
      - (flxw(k) + flxw(k+1))  * (wc(k) + wc(k+1))   &
      + (flxw(k+1) - flxw(k-1)) * 2.* wc(k)       )

enddo

return
end subroutine vel_advectc_plumerise
!-------------------------------------------------------------------------------
!
subroutine hadvance_plumerise(iac,m1,dt,wc,wt,wp,mintime)

implicit none
integer :: k,iac
integer :: m1,mintime
real, dimension(m1) :: dummy, wc,wt,wp
real eps,dt
!     It is here that the Asselin filter is applied.  For the velocities
!     and pressure, this must be done in two stages, the first when
!     IAC=1 and the second when IAC=2.


eps = .2
if(mintime == 1) eps=0.5

!     For both IAC=1 and IAC=2, call PREDICT for U, V, W, and P.
!
call predict_plumerise(m1,wc,wp,wt,dummy,iac,2.*dt,eps)
!print*,'mintime',mintime,eps
!do k=1,m1
!   print*,'W-HAD',k,wc(k),wp(k),wt(k)
!enddo
return
end subroutine hadvance_plumerise
!-------------------------------------------------------------------------------
!
subroutine predict_plumerise(npts,ac,ap,fa,af,iac,dtlp,epsu)
implicit none
integer :: npts,iac,m
real :: epsu,dtlp
real, dimension(*) :: ac,ap,fa,af

!     For IAC=3, this routine moves the arrays AC and AP forward by
!     1 time level by adding in the prescribed tendency. It also
!     applies the Asselin filter given by:

!              {AC} = AC + EPS * (AP - 2 * AC + AF)

!     where AP,AC,AF are the past, current and future time levels of A.
!     All IAC=1 does is to perform the {AC} calculation without the AF
!     term present.  IAC=2 completes the calculation of {AC} by adding
!     the AF term only, and advances AC by filling it with input AP
!     values which were already updated in ACOUSTC.
!

if (iac .eq. 1) then
   do m = 1,npts
      ac(m) = ac(m) + epsu * (ap(m) - 2. * ac(m))
   enddo
   return
elseif (iac .eq. 2) then
   do m = 1,npts
      af(m) = ap(m)
      ap(m) = ac(m) + epsu * af(m)
   enddo
!elseif (iac .eq. 3) then
!   do m = 1,npts
!      af(m) = ap(m) + dtlp * fa(m)
!   enddo
!   if (ngrid .eq. 1 .and. ipara .eq. 0) call cyclic(nzp,nxp,nyp,af,'T')
!   do m = 1,npts
!      ap(m) = ac(m) + epsu * (ap(m) - 2. * ac(m) + af(m))
!   enddo
endif

do m = 1,npts
  ac(m) = af(m)
enddo
return
end subroutine predict_plumerise
!-------------------------------------------------------------------------------
!
subroutine  buoyancy_plumerise(m1, T, TE, QV, QVENV, QH, QI, QC, WT, scr1)
implicit none
integer :: k,m1
real, parameter :: g = 9.8, eps = 0.622, gama = 0.5 ! mass virtual coeff.
real, dimension(m1) :: T, TE, QV, QVENV, QH, QI, QC, WT, scr1
real :: TV,TVE,QWTOTL,umgamai
real, parameter :: mu = 0.15 

!- orig
umgamai = 1./(1.+gama) ! compensa a falta do termo de aceleracao associado `as
                       ! das pertubacoes nao-hidrostaticas no campo de pressao

!- new                 ! Siesbema et al, 2004
!umgamai = 1./(1.-2.*mu)

do k = 2,m1-1

    TV =   T(k) * (1. + (QV(k)   /EPS))/(1. + QV(k)   )  !blob virtual temp.                                        	   
    TVE = TE(k) * (1. + (QVENV(k)/EPS))/(1. + QVENV(k))  !and environment

    QWTOTL = QH(k) + QI(k) + QC(k)                       ! QWTOTL*G is drag
!- orig
   !scr1(k)= G*( umgamai*(  TV - TVE) / TVE   - QWTOTL) 
    scr1(k)= G*  umgamai*( (TV - TVE) / TVE   - QWTOTL) 

    !if(k .lt. 10)print*,'BT',k,TV,TVE,TVE,QWTOTL
enddo

do k = 2,m1-2
    wt(k) = wt(k)+0.5*(scr1(k)+scr1(k+1))
!   print*,'W-BUO',k,wt(k),scr1(k),scr1(k+1)
enddo

end subroutine  buoyancy_plumerise
!-------------------------------------------------------------------------------
!
subroutine ENTRAINMENT(m1,w,wt,radius,ALPHA)
implicit none
integer :: k,m1
real, dimension(m1) :: w,wt,radius
REAL DMDTM,WBAR,RADIUS_BAR,umgamai,DYN_ENTR,ALPHA
real, parameter :: mu = 0.15 ,gama = 0.5 ! mass virtual coeff.

!- new - Siesbema et al, 2004
!umgamai = 1./(1.-2.*mu)

!- orig
!umgamai = 1
umgamai = 1./(1.+gama) ! compensa a falta do termo de aceleracao associado `as
                       ! das pertubacoes nao-hidrostaticas no campo de pressao

!
!-- ALPHA/RADIUS(L) = (1/M)DM/DZ  (W 14a)
  do k=2,m1-1

!-- for W: WBAR is only W(k)
!     WBAR=0.5*(W(k)+W(k-1))           
      WBAR=W(k)          
      RADIUS_BAR = 0.5*(RADIUS(k) + RADIUS(k-1))
! orig
     !DMDTM =           2. * ALPHA * ABS (WBAR) / RADIUS_BAR  != (1/M)DM/DT
      DMDTM = umgamai * 2. * ALPHA * ABS (WBAR) / RADIUS_BAR  != (1/M)DM/DT

!--  DMDTM*W(L) entrainment,
      wt(k) = wt(k)  - DMDTM*ABS (WBAR)
      !print*,'W-ENTR=',k,w(k),- DMDTM*ABS (WBAR)
      
      !if(VEL_P (k) - VEL_E (k) > 0.) cycle

       !-   dynamic entrainment
       DYN_ENTR =  (2./3.1416)*0.5*ABS (VEL_P(k)-VEL_E(k)+VEL_P(k-1)-VEL_E(k-1)) /RADIUS_BAR

       wt(k) = wt(k)  - DYN_ENTR*ABS (WBAR)
       
       !- entraiment acceleration for output only
       !dwdt_entr(k) =  - DMDTM*ABS (WBAR)- DYN_ENTR*ABS (WBAR)
  enddo
end subroutine  ENTRAINMENT
!-------------------------------------------------------------------------------
!
subroutine scl_advectc_plumerise(varn,mzp)
!use module_zero_plumegen_coms
implicit none
integer :: mzp
character(len=*) :: varn
real :: dtlto2
integer :: k

!  wp => w
!- Advect  scalars
   dtlto2   = .5 * dt
!  vt3dc(1) =      (w(1) + wc(1)) * dtlto2 * dne(1)
   vt3dc(1) =      (w(1) + wc(1)) * dtlto2 * rho(1)*1.e-3!converte de CGS p/ MKS
   vt3df(1) = .5 * (w(1) + wc(1)) * dtlto2 * dzm(1)

   do k = 2,mzp
!     vt3dc(k) =  (w(k) + wc(k)) * dtlto2 *.5 * (dne(k) + dne(k+1))
      vt3dc(k) =  (w(k) + wc(k)) * dtlto2 *.5 * (rho(k) + rho(k+1))*1.e-3
      vt3df(k) =  (w(k) + wc(k)) * dtlto2 *.5 *  dzm(k)
     !print*,'vt3df-vt3dc',k,vt3dc(k),vt3df(k)
   enddo

 
!-srf-24082005
!  do k = 1,mzp-1
  do k = 1,mzp
     vctr1(k) = (zt(k+1) - zm(k)) * dzm(k)
     vctr2(k) = (zm(k)   - zt(k)) * dzm(k)
!    vt3dk(k) = dzt(k) / dne(k)
     vt3dk(k) = dzt(k) /(rho(k)*1.e-3)
     !print*,'VT3dk',k,dzt(k) , dne(k)
  enddo

!      scalarp => scalar_tab(n,ngrid)%var_p
!      scalart => scalar_tab(n,ngrid)%var_t

!- temp advection tendency (TT)
   scr1=T
   call fa_zc_plumerise(mzp                   &
             	       ,T	  ,scr1  (1)  &
             	       ,vt3dc (1) ,vt3df (1)  &
             	       ,vt3dg (1) ,vt3dk (1)  &
             	       ,vctr1,vctr2	      )

   call advtndc_plumerise(mzp,T,scr1(1),TT,dt)

!- water vapor advection tendency (QVT)
   scr1=QV
   call fa_zc_plumerise(mzp                  &
             	       ,QV	  ,scr1  (1)  &
             	       ,vt3dc (1) ,vt3df (1)  &
             	       ,vt3dg (1) ,vt3dk (1)  &
             	       ,vctr1,vctr2	     )

   call advtndc_plumerise(mzp,QV,scr1(1),QVT,dt)

!- liquid advection tendency (QCT)
   scr1=QC
   call fa_zc_plumerise(mzp                  &
             	       ,QC	  ,scr1  (1)  &
             	       ,vt3dc (1) ,vt3df (1)  &
             	       ,vt3dg (1) ,vt3dk (1)  &
             	       ,vctr1,vctr2	     )

   call advtndc_plumerise(mzp,QC,scr1(1),QCT,dt)

!- ice advection tendency (QIT)
   scr1=QI
   call fa_zc_plumerise(mzp                  &
             	       ,QI	  ,scr1  (1)  &
             	       ,vt3dc (1) ,vt3df (1)  &
             	       ,vt3dg (1) ,vt3dk (1)  &
             	       ,vctr1,vctr2	     )

   call advtndc_plumerise(mzp,QI,scr1(1),QIT,dt)

!- hail/rain advection tendency (QHT)
!   if(ak1 > 0. .or. ak2 > 0.) then

      scr1=QH
      call fa_zc_plumerise(mzp                  &
             	          ,QH	    ,scr1  (1)  &
             	          ,vt3dc (1) ,vt3df (1)  &
             	          ,vt3dg (1) ,vt3dk (1)  &
             	          ,vctr1,vctr2	       )

      call advtndc_plumerise(mzp,QH,scr1(1),QHT,dt)
!   endif
    !- horizontal wind advection tendency (VEL_T)
    scr1=VEL_P
    call fa_zc_plumerise(mzp		      &
    			,VEL_P     ,scr1  (1)  &
    			,vt3dc (1) ,vt3df (1)  &
    			,vt3dg (1) ,vt3dk (1)  &
    			,vctr1,vctr2	     )

    call advtndc_plumerise(mzp,VEL_P,scr1(1),VEL_T,dt)

    !- vertical radius transport

    scr1=rad_p
    call fa_zc_plumerise(mzp                  &
             	        ,rad_p     ,scr1  (1)  &
             	        ,vt3dc (1) ,vt3df (1)  &
             	        ,vt3dg (1) ,vt3dk (1)  &
             	        ,vctr1,vctr2	     )

    call advtndc_plumerise(mzp,rad_p,scr1(1),rad_t,dt)


   return
!
!- gas/particle advection tendency (SCT)
!    if(varn == 'SC')return
   scr1=SC
   call fa_zc_plumerise(mzp		    &
   	     	       ,SC	 ,scr1  (1)  &
   	     	       ,vt3dc (1) ,vt3df (1)  &
   	     	       ,vt3dg (1) ,vt3dk (1)  &
   	     	       ,vctr1,vctr2	     )
   
   call advtndc_plumerise(mzp,SC,scr1(1),SCT,dt)


return
end subroutine scl_advectc_plumerise
!-------------------------------------------------------------------------------
!
subroutine fa_zc_plumerise(m1,scp,scr1,vt3dc,vt3df,vt3dg,vt3dk,vctr1,vctr2)

implicit none
integer :: m1,k
real :: dfact
real, dimension(m1) :: scp,scr1,vt3dc,vt3df,vt3dg,vt3dk
real, dimension(m1) :: vctr1,vctr2

dfact = .5

! Compute scalar flux VT3DG
      do k = 1,m1-1
         vt3dg(k) = vt3dc(k)                   &
                  * (vctr1(k) * scr1(k)        &
                  +  vctr2(k) * scr1(k+1)      &
                  +  vt3df(k) * (scr1(k) - scr1(k+1)))
      enddo
      
! Modify fluxes to retain positive-definiteness on scalar quantities.
!    If a flux will remove 1/2 quantity during a timestep,
!    reduce to first order flux. This will remain positive-definite
!    under the assumption that ABS(CFL(i)) + ABS(CFL(i-1)) < 1.0 if
!    both fluxes are evacuating the box.

do k = 1,m1-1
 if (vt3dc(k) .gt. 0.) then
   if (vt3dg(k) * vt3dk(k)    .gt. dfact * scr1(k)) then
	 vt3dg(k) = vt3dc(k) * scr1(k)
   endif
 elseif (vt3dc(k) .lt. 0.) then
   if (-vt3dg(k) * vt3dk(k+1) .gt. dfact * scr1(k+1)) then
	 vt3dg(k) = vt3dc(k) * scr1(k+1)
   endif
 endif

enddo

! Compute flux divergence

do k = 2,m1-1
    scr1(k) = scr1(k)  &
            + vt3dk(k) * ( vt3dg(k-1) - vt3dg(k) &
            + scp  (k) * ( vt3dc(k)   - vt3dc(k-1)))
enddo
return
end subroutine fa_zc_plumerise
!-------------------------------------------------------------------------------
!
subroutine advtndc_plumerise(m1,scp,sca,sct,dtl)
implicit none
integer :: m1,k
real :: dtl,dtli
real, dimension(m1) :: scp,sca,sct

dtli = 1. / dtl
do k = 2,m1-1
   sct(k) = sct(k) + (sca(k)-scp(k)) * dtli
enddo
return
end subroutine advtndc_plumerise
!-------------------------------------------------------------------------------
!
subroutine tend0_plumerise
!use module_zero_plumegen_coms, only: nm1,wt,tt,qvt,qct,qht,qit,sct
 wt(1:nm1)  = 0.
 tt(1:nm1)  = 0.
qvt(1:nm1)  = 0.
qct(1:nm1)  = 0.
qht(1:nm1)  = 0.
qit(1:nm1)  = 0.
vel_t(1:nm1)  = 0.
rad_t(1:nm1)  = 0.
!sct(1:nm1)  = 0.
end subroutine tend0_plumerise

!     ****************************************************************

subroutine scl_misc(m1)
!use module_zero_plumegen_coms
implicit none
real, parameter :: g = 9.81, cp=1004.
integer m1,k
real dmdtm

 do k=2,m1-1
      WBAR    = 0.5*(W(k)+W(k-1))  
!-- dry adiabat
      ADIABAT = - WBAR * G / CP 
!      
!-- entrainment     
      DMDTM = 2. * ALPHA * ABS (WBAR) / RADIUS (k)  != (1/M)DM/DT
      
!-- tendency temperature = adv + adiab + entrainment
      TT(k) = TT(K) + ADIABAT - DMDTM * ( T  (k) -    TE (k) ) 

!-- tendency water vapor = adv  + entrainment
      QVT(K) = QVT(K)         - DMDTM * ( QV (k) - QVENV (k) )

      QCT(K) = QCT(K)	      - DMDTM * ( QC (k)  )
      QHT(K) = QHT(K)	      - DMDTM * ( QH (k)  )
      QIT(K) = QIT(K)	      - DMDTM * ( QI (k)  )

      !-- tendency horizontal speed = adv  + entrainment
      VEL_T(K) = VEL_T(K)     - DMDTM * ( VEL_P (k) - VEL_E (k) )

      !-- tendency horizontal speed = adv  + entrainment
      rad_t(K) = rad_t(K)     + 0.5*DMDTM*(6./5.)*RADIUS (k)
!-- tendency gas/particle = adv  + entrainment
!      SCT(K) = SCT(K)         - DMDTM * ( SC (k) -   SCE (k) )

enddo
end subroutine scl_misc
!     ****************************************************************

  SUBROUTINE scl_dyn_entrain(m1,nkp,wbar,w,adiabat,alpha,radius,tt,t,te,qvt,qv,qvenv,qct,qc,qht,qh,qit,qi,&
                    vel_e,vel_p,vel_t,rad_p,rad_t)
    implicit none

    INTEGER , INTENT(IN)    :: m1

    ! plumegen_coms
    INTEGER , INTENT(IN)    :: nkp
    REAL    , INTENT(INOUT) :: wbar 
    REAL    , INTENT(IN)    :: w(nkp)
    REAL    , INTENT(INOUT) :: adiabat 
    REAL    , INTENT(IN)    :: alpha
    REAL    , INTENT(IN)    :: radius(nkp)
    REAL    , INTENT(INOUT) :: tt(nkp)
    REAL    , INTENT(IN)    :: t(nkp)
    REAL    , INTENT(IN)    :: te(nkp)
    REAL    , INTENT(INOUT) :: qvt(nkp)
    REAL    , INTENT(IN)    :: qv(nkp)
    REAL    , INTENT(IN)    :: qvenv(nkp)
    REAL    , INTENT(INOUT) :: qct(nkp)
    REAL    , INTENT(IN)    :: qc(nkp)
    REAL    , INTENT(INOUT) :: qht(nkp)
    REAL    , INTENT(IN)    :: qh(nkp)
    REAL    , INTENT(INOUT) :: qit(nkp)
    REAL    , INTENT(IN)    :: qi(nkp)

    REAL    , INTENT(IN)    :: vel_e(nkp)
    REAL    , INTENT(IN)    :: vel_p(nkp)
    REAL    , INTENT(INOUT) :: vel_t(nkp)
    REAL    , INTENT(INOUT) :: rad_T(nkp)
    REAL    , INTENT(IN)    :: rad_p(nkp)

    real, parameter :: g = 9.81, cp=1004., pi=3.1416

    integer k
    real dmdtm

    DO k=2,m1-1
      !      
      !-- tendency horizontal radius from dyn entrainment
     	   !rad_t(K) = rad_t(K)   +	(vel_e(k)-vel_p(k)) /pi
     	    rad_t(K) = rad_t(K)   + ABS((vel_e(k)-vel_p(k)))/pi
      
      !-- entrainment	  
     	   !DMDTM = (2./3.1416)  *     (VEL_E (k) - VEL_P (k)) / RADIUS (k)  
     	    DMDTM = (2./3.1416)  *  ABS(VEL_E (k) - VEL_P (k)) / RADIUS (k)  
      
      !-- tendency horizontal speed  from dyn entrainment
     	    VEL_T(K) = VEL_T(K)     - DMDTM * ( VEL_P (k) - VEL_E (k) )
      
      !     if(VEL_P (k) - VEL_E (k) > 0.) cycle
      
      !-- tendency temperature  from dyn entrainment
     	    TT(k) = TT(K)	    - DMDTM * ( T (k) - TE  (k) ) 
      
      !-- tendency water vapor  from dyn entrainment
   	    QVT(K) = QVT(K)	    - DMDTM * ( QV (k) - QVENV (k) )
      
     	    QCT(K) = QCT(K)	    - DMDTM * ( QC (k)  )
     	    QHT(K) = QHT(K)	    - DMDTM * ( QH (k)  )
     	    QIT(K) = QIT(K)	    - DMDTM * ( QI (k)  )
      
      !-- tendency gas/particle  from dyn entrainment
      !	 SCT(K) = SCT(K)	 - DMDTM * ( SC (k) - SCE (k) )
    
    ENDDO
   END SUBROUTINE scl_dyn_entrain

!     ****************************************************************

subroutine visc_W(m1,deltak,kmt)
!use module_zero_plumegen_coms
implicit none
integer m1,k,deltak,kmt,m2
real dz1t,dz1m,dz2t,dz2m,d2wdz,d2tdz  ,d2qvdz ,d2qhdz ,d2qcdz ,d2qidz ,d2scdz, &
 d2vel_pdz,d2rad_dz


!srf--- 17/08/2005
!m2=min(m1+deltak,kmt)
m2=min(m1,kmt)

!do k=2,m1-1
do k=2,m2-1
 DZ1T   = 0.5*(ZT(K+1)-ZT(K-1))
 DZ2T   = VISC (k) / (DZ1T * DZ1T)  
 DZ1M   = 0.5*(ZM(K+1)-ZM(K-1))
 DZ2M   = VISC (k) / (DZ1M * DZ1M)  
 D2WDZ  = (W  (k + 1) - 2 * W  (k) + W  (k - 1) ) * DZ2M  
 D2TDZ  = (T  (k + 1) - 2 * T  (k) + T  (k - 1) ) * DZ2T  
 D2QVDZ = (QV (k + 1) - 2 * QV (k) + QV (k - 1) ) * DZ2T  
 D2QHDZ = (QH (k + 1) - 2 * QH (k) + QH (k - 1) ) * DZ2T 
 D2QCDZ = (QC (k + 1) - 2 * QC (k) + QC (k - 1) ) * DZ2T  
 D2QIDZ = (QI (k + 1) - 2 * QI (k) + QI (k - 1) ) * DZ2T  
 !D2SCDZ = (SC (k + 1) - 2 * SC (k) + SC (k - 1) ) * DZ2T 
 d2vel_pdz=(vel_P  (k + 1) - 2 * vel_P  (k) + vel_P  (k - 1) ) * DZ2T
 d2rad_dz =(rad_p  (k + 1) - 2 * rad_p  (k) + rad_p  (k - 1) ) * DZ2T
 
  WT(k) =   WT(k) + D2WDZ 
  TT(k) =   TT(k) + D2TDZ                          
 QVT(k) =  QVT(k) + D2QVDZ 
 QCT(k) =  QCT(k) + D2QCDZ 
 QHT(k) =  QHT(k) + D2QHDZ 
 QIT(k) =  QIT(k) + D2QIDZ     
 vel_t(k) =   vel_t(k) + d2vel_pdz
 rad_t(k) =   rad_t(k) + d2rad_dz
 !SCT(k) =  SCT(k) + D2SCDZ
 !print*,'W-VISC=',k,D2WDZ
enddo  

end subroutine visc_W

!     ****************************************************************

subroutine update_plumerise(m1,varn)
!use module_zero_plumegen_coms
integer m1,k
character(len=*) :: varn
 
if(varn == 'W') then

 do k=2,m1-1
   W(k) =  W(k) +  WT(k) * DT  
 enddo
 return

else 
do k=2,m1-1
   T(k) =  T(k) +  TT(k) * DT  

  QV(k) = QV(k) + QVT(k) * DT  

  QC(k) = QC(k) + QCT(k) * DT !cloud drops travel with air 
  QH(k) = QH(k) + QHT(k) * DT  
  QI(k) = QI(k) + QIT(k) * DT 
! SC(k) = SC(k) + SCT(k) * DT 

!srf---18jun2005  
  QV(k) = max(0., QV(k))
  QC(k) = max(0., QC(k))
  QH(k) = max(0., QH(k))
  QI(k) = max(0., QI(k))
  
  VEL_P(k) =  VEL_P(k) + VEL_T(k) * DT  
  rad_p(k) =  rad_p(k) + rad_t(k) * DT  
! SC(k) = max(0., SC(k))

 enddo
endif
end subroutine update_plumerise
!-------------------------------------------------------------------------------
!
subroutine fallpart(m1)
!use module_zero_plumegen_coms
integer m1,k
real vtc, dfhz,dfiz,dz1
!srf==================================
!   verificar se o gradiente esta correto 
!  
!srf==================================
!
!     XNO=1.E7  [m**-4] median volume diameter raindrop,Kessler
!     VC = 38.3/(XNO**.125), median volume fallspeed eqn., Kessler
!     for ice, see (OT18), use F0=0.75 per argument there. rho*q
!     values are in g/m**3, velocities in m/s

real, PARAMETER :: VCONST = 5.107387, EPS = 0.622, F0 = 0.75  
real, PARAMETER :: G = 9.81, CP = 1004.
!
do k=2,m1-1

   VTC = VCONST * RHO (k) **.125   ! median volume fallspeed (KTable4)
                                
!  hydrometeor assembly velocity calculations (K Table4)
!  VTH(k)=-VTC*QH(k)**.125  !median volume fallspeed, water            
   VTH (k) = - 4.	    !small variation with qh
   
   VHREL = W (k) + VTH (k)  !relative to surrounding cloud
 
!  rain ventilation coefficient for evaporation
   CVH(k) = 1.6 + 0.57E-3 * (ABS (VHREL) ) **1.5  
!
!  VTI(k)=-VTC*F0*QI(k)**.125    !median volume fallspeed,ice             
   VTI (k) = - 3.                !small variation with qi

   VIREL = W (k) + VTI (k)       !relative to surrounding cloud
!
!  ice ventilation coefficient for sublimation
   CVI(k) = 1.6 + 0.57E-3 * (ABS (VIREL) ) **1.5 / F0  
!
!
   IF (VHREL.GE.0.0) THEN  
    DFHZ=QH(k)*(RHO(k  )*VTH(k  )-RHO(k-1)*VTH(k-1))/RHO(k-1)
   ELSE  
    DFHZ=QH(k)*(RHO(k+1)*VTH(k+1)-RHO(k  )*VTH(k  ))/RHO(k)
   ENDIF  
   !
   !
   IF (VIREL.GE.0.0) THEN  
    DFIZ=QI(k)*(RHO(k  )*VTI(k  )-RHO(k-1)*VTI(k-1))/RHO(k-1)
   ELSE  
    DFIZ=QI(k)*(RHO(k+1)*VTI(k+1)-RHO(k  )*VTI(k  ))/RHO(k)
   ENDIF
   
   DZ1=ZM(K)-ZM(K-1)
   
   qht(k) = qht(k) - DFHZ / DZ1 !hydrometeors don't
   		  
   qit(k) = qit(k) - DFIZ / DZ1  !nor does ice? hail, what about

enddo
end subroutine fallpart
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
subroutine printout (izprint,nrectotal)  
!use module_zero_plumegen_coms  
real, parameter :: tmelt = 273.3
integer, save :: nrec
data nrec/0/
integer :: ko,izprint,interval,nrectotal
real :: pea, btmp,etmp,vap1,vap2,gpkc,gpkh,gpki,deficit
interval = 1              !debug time interval,min

!
IF (IZPRINT.EQ.0) RETURN  

IF(MINTIME == 1) nrec = 0
!
WRITE (2, 430) MINTIME, DT, TIME  
WRITE (2, 431) ZTOP  
WRITE (2, 380)  
!
! do the print
!
 DO 390 KO = 1, nrectotal, interval  
                             
   PEA = PE (KO) * 10.       !pressure is stored in decibars(kPa),print in mb;
   BTMP = T (KO) - TMELT     !temps in Celsius
   ETMP = T (KO) - TE (KO)   !temperature excess
   VAP1 = QV (KO)   * 1000.  !printout in g/kg for all water,
   VAP2 = QSAT (KO) * 1000.  !vapor (internal storage is in g/g)
   GPKC = QC (KO)   * 1000.  !cloud water
   GPKH = QH (KO)   * 1000.  !raindrops
   GPKI = QI (KO)   * 1000.  !ice particles 
   DEFICIT = VAP2 - VAP1     !vapor deficit
!
   WRITE (2, 400) zt(KO)/1000., PEA, W (KO), BTMP, ETMP, VAP1, &
    VAP2, GPKC, GPKH, GPKI, VTH (KO), SC(KO)
!
!
!                                    !end of printout
   
  390 CONTINUE		  

   nrec=nrec+1
   write (19,rec=nrec) (W (KO), KO=1,nrectotal)
   nrec=nrec+1
   write (19,rec=nrec) (T (KO), KO=1,nrectotal)
   nrec=nrec+1
   write (19,rec=nrec) (TE(KO), KO=1,nrectotal)
   nrec=nrec+1
   write (19,rec=nrec) (QV(KO)*1000., KO=1,nrectotal)
   nrec=nrec+1
   write (19,rec=nrec) (QC(KO)*1000., KO=1,nrectotal)
   nrec=nrec+1
   write (19,rec=nrec) (QH(KO)*1000., KO=1,nrectotal)
   nrec=nrec+1
   write (19,rec=nrec) (QI(KO)*1000., KO=1,nrectotal)
   nrec=nrec+1
!   write (19,rec=nrec) (SC(KO), KO=1,nrectotal)
   write (19,rec=nrec) (QSAT(KO)*1000., KO=1,nrectotal)
   nrec=nrec+1
   write (19,rec=nrec) (QVENV(KO)*1000., KO=1,nrectotal)



!print*,'ntimes=',nrec/(9)
!
RETURN  
!
! ************** FORMATS *********************************************
!
  380 FORMAT(/,' Z(KM) P(MB) W(MPS) T(C)  T-TE   VAP   SAT   QC    QH' &
'     QI    VTH(MPS) SCAL'/)
!
  400 FORMAT(1H , F4.1,F7.2,F7.2,F6.1,6F6.2,F7.2,1X,F6.2)  
!
  430 FORMAT(1H ,//I5,' MINUTES       DT= ',F6.2,' SECONDS   TIME= ' &
        ,F8.2,' SECONDS')
  431 FORMAT(' ZTOP= ',F10.2)  
!
end subroutine printout
!
! *********************************************************************
SUBROUTINE WATERBAL  
!use module_zero_plumegen_coms  
!
                                        
IF (QC (L) .LE.1.0E-10) QC (L) = 0.  !DEFEAT UNDERFLOW PROBLEM
IF (QH (L) .LE.1.0E-10) QH (L) = 0.  
IF (QI (L) .LE.1.0E-10) QI (L) = 0.  
!
CALL EVAPORATE    !vapor to cloud,cloud to vapor  
!                             
CALL SUBLIMATE    !vapor to ice  
!                            
CALL GLACIATE     !rain to ice 
                           
CALL MELT         !ice to rain
!         
!if(ak1 > 0. .or. ak2 > 0.) &
CALL CONVERT () !(auto)conversion and accretion 
!CALL CONVERT2 () !(auto)conversion and accretion 
!

RETURN  
END SUBROUTINE WATERBAL
! *********************************************************************
SUBROUTINE EVAPORATE  
!
!- evaporates cloud,rain and ice to saturation
!
!use module_zero_plumegen_coms  
implicit none
!
!     XNO=10.0E06
!     HERC = 1.93*1.E-6*XN035        !evaporation constant
!
real, PARAMETER :: HERC = 5.44E-4, CP = 1.004, HEATCOND = 2.5E3  
real, PARAMETER :: HEATSUBL = 2834., TMELT = 273., TFREEZE = 269.3

real, PARAMETER :: FRC = HEATCOND / CP, SRC = HEATSUBL / CP

real :: evhdt, evidt, evrate, evap, sd,	quant, dividend, divisor, devidt

!
!
SD = QSAT (L) - QV (L)  !vapor deficit
IF (SD.EQ.0.0)  RETURN  
!IF (abs(SD).lt.1.e-7)  RETURN  


EVHDT = 0.  
EVIDT = 0.  
!evrate =0.; evap=0.; sd=0.0; quant=0.0; dividend=0.0; divisor=0.0; devidt=0.0
                                 
EVRATE = ABS (WBAR * DQSDZ)   !evaporation rate (Kessler 8.32)
EVAP = EVRATE * DT            !what we can get in DT
                                  

IF (SD.LE.0.0) THEN  !     condense. SD is negative

   IF (EVAP.GE.ABS (SD) ) THEN    !we get it all
                                  
      QC (L) = QC  (L) - SD  !deficit,remember?
      QV (L) = QSAT(L)       !set the vapor to saturation  
      T  (L) = T   (L) - SD * FRC  !heat gained through condensation
                                !per gram of dry air
      RETURN  

   ELSE  
                                 
      QC (L) = QC (L) + EVAP         !get what we can in DT 
      QV (L) = QV (L) - EVAP         !remove it from the vapor
      T  (L) = T  (L) + EVAP * FRC   !get some heat

      RETURN  

   ENDIF  
!
ELSE                                !SD is positive, need some water
!
! not saturated. saturate if possible. use everything in order
! cloud, rain, ice. SD is positive
                                         
   IF (EVAP.LE.QC (L) ) THEN        !enough cloud to last DT  
!
                                         
      IF (SD.LE.EVAP) THEN          !enough time to saturate
                                         
         QC (L) = QC (L) - SD       !remove cloud                                          
         QV (L) = QSAT (L)          !saturate
         T (L) = T (L) - SD * FRC   !cool the parcel                                          
         RETURN  !done
!
                                         
      ELSE   !not enough time
                                        
         SD = SD-EVAP               !use what there is
         QV (L) = QV (L) + EVAP     !add vapor
         T (L) = T (L) - EVAP * FRC !lose heat
         QC (L) = QC (L) - EVAP     !lose cloud
	                            !go on to rain.                                      
      ENDIF     
!
   ELSE                !not enough cloud to last DT
!      
      IF (SD.LE.QC (L) ) THEN   !but there is enough to sat
                                          
         QV (L) = QSAT (L)  !use it
         QC (L) = QC (L) - SD  
         T  (L) = T (L) - SD * FRC  
         RETURN  
	                              
      ELSE            !not enough to sat
         SD = SD-QC (L)  
         QV (L) = QV (L) + QC (L)  
         T  (L) = T (L) - QC (L) * FRC         
         QC (L) = 0.0  !all gone
                                          
      ENDIF       !on to rain                           
   ENDIF          !finished with cloud
!
!  but still not saturated, so try to use some rain
!  this is tricky, because we only have time DT to evaporate. if there
!  is enough rain, we can evaporate it for dt. ice can also sublimate
!  at the same time. there is a compromise here.....use rain first, then
!  ice. saturation may not be possible in one DT time.
!  rain evaporation rate (W12),(OT25),(K Table 4). evaporate rain first
!  sd is still positive or we wouldn't be here.


   IF (QH (L) .LE.1.E-10) GOTO 33                                  

!srf-25082005
!  QUANT = ( QC (L)  + QV (L) - QSAT (L) ) * RHO (L)   !g/m**3
   QUANT = ( QSAT (L)- QC (L) - QV (L)   ) * RHO (L)   !g/m**3
!
   EVHDT = (DT * HERC * (QUANT) * (QH (L) * RHO (L) ) **.65) / RHO (L)
!             rain evaporation in time DT
                                         
   IF (EVHDT.LE.QH (L) ) THEN           !enough rain to last DT

      IF (SD.LE.EVHDT) THEN  		!enough time to saturate	  
         QH (L) = QH (L) - SD   	!remove rain	  
         QV (L) = QSAT (L)  		!saturate	  
         T (L) = T (L) - SD * FRC  	!cool the parcel		  
	 
	 RETURN  			!done
!                       
      ELSE                               !not enough time
         SD = SD-EVHDT  		 !use what there is
         QV (L) = QV (L) + EVHDT  	 !add vapor
         T (L) = T (L) - EVHDT * FRC  	 !lose heat
         QH (L) = QH (L) - EVHDT  	 !lose rain

      ENDIF  				  !go on to ice.
!                                    
   ELSE  !not enough rain to last DT
!
      IF (SD.LE.QH (L) ) THEN             !but there is enough to sat
         QV (L) = QSAT (L)                !use it
         QH (L) = QH (L) - SD  
         T (L) = T (L) - SD * FRC  
         RETURN  
!                            
      ELSE                              !not enough to sat
         SD = SD-QH (L)  
         QV (L) = QV (L) + QH (L)  
         T (L) = T (L) - QH (L) * FRC    
         QH (L) = 0.0                   !all gone
                                          
      ENDIF                             !on to ice
!
                                          
   ENDIF                                !finished with rain
!
!
!  now for ice
!  equation from (OT); correction factors for units applied
!
   33    continue
   IF (QI (L) .LE.1.E-10) RETURN            !no ice there
!
   DIVIDEND = ( (1.E6 / RHO (L) ) **0.475) * (SD / QSAT (L) &
            - 1) * (QI (L) **0.525) * 1.13
   DIVISOR = 7.E5 + 4.1E6 / (10. * EST (L) )  
                                                 
   DEVIDT = - CVI(L) * DIVIDEND / DIVISOR   !rate of change
                                                  
   EVIDT = DEVIDT * DT                      !what we could get
!
! logic here is identical to rain. could get fancy and make subroutine
! but duplication of code is easier. God bless the screen editor.
!
                                         
   IF (EVIDT.LE.QI (L) ) THEN             !enough ice to last DT
!
                                         
      IF (SD.LE.EVIDT) THEN  		  !enough time to saturate
         QI (L) = QI (L) - SD   	  !remove ice
         QV (L) = QSAT (L)  		  !saturate
         T (L) = T (L) - SD * SRC  	  !cool the parcel
	 
         RETURN  			  !done
!
                                          
      ELSE                                !not enough time
                                          
         SD = SD-EVIDT  		  !use what there is
         QV (L) = QV (L) + EVIDT  	  !add vapor
          T (L) =  T (L) - EVIDT * SRC  	  !lose heat
         QI (L) = QI (L) - EVIDT  	  !lose ice
                                          
      ENDIF  				  !go on,unsatisfied
!                                          
   ELSE                                   !not enough ice to last DT
!                                         
      IF (SD.LE.QI (L) ) THEN             !but there is enough to sat
                                          
         QV (L) = QSAT (L)                !use it
         QI (L) = QI   (L) - SD  
          T (L) =  T   (L) - SD * SRC  
	  
         RETURN  
!
      ELSE                                 !not enough to sat
         SD = SD-QI (L)  
         QV (L) = QV (L) + QI (L)  
         T (L) = T (L) - QI (L) * SRC             
         QI (L) = 0.0                      !all gone

      ENDIF                                !on to better things
                                           !finished with ice
   ENDIF  
!                                 
ENDIF                                      !finished with the SD decision
!
RETURN  
!
END SUBROUTINE EVAPORATE
!
! *********************************************************************
SUBROUTINE CONVERT ()  
!
!- ACCRETION AND AUTOCONVERSION
!
!use module_zero_plumegen_coms  
!
real,      PARAMETER ::  AK1 = 0.001    !conversion rate constant
real,      PARAMETER ::  AK2 = 0.0052   !collection (accretion) rate
real,      PARAMETER ::  TH  = 0.5      !Kessler threshold
integer,   PARAMETER ::iconv = 1        !- Kessler conversion (=0)
                                     
!real, parameter :: ANBASE =  50.!*1.e+6 !Berry-number at cloud base #/m^3(maritime)
 real, parameter :: ANBASE =100000.!*1.e+6 !Berry-number at cloud base #/m^3(continental)
!real, parameter :: BDISP = 0.366       !Berry--size dispersion (maritime)
 real, parameter :: BDISP = 0.146       !Berry--size dispersion (continental)
real, parameter :: TFREEZE = 269.3  !ice formation temperature
!
real ::   accrete, con, q, h, bc1,   bc2,  total


IF (T (L)  .LE. TFREEZE) RETURN  !process not allowed above ice
!
IF (QC (L) .EQ. 0.     ) RETURN  

ACCRETE = 0.  
CON = 0.  
Q = RHO (L) * QC (L)  
H = RHO (L) * QH (L)  
!
!     selection rules
!                         
!            
IF (QH (L) .GT. 0.     ) ACCRETE = AK2 * Q * (H**.875)  !accretion, Kessler
!
IF (ICONV.NE.0) THEN   !select Berry or Kessler
!
!old   BC1 = 120.  
!old   BC2 = .0266 * ANBASE * 60.  
!old   CON = BDISP * Q * Q * Q / (BC1 * Q * BDISP + BC2) 	  

   CON = Q*Q*Q*BDISP/(60.*(5.*Q*BDISP+0.0366*ANBASE))
!
ELSE  
!                             
!   CON = AK1 * (Q - TH)   !Kessler autoconversion rate
!      
!   IF (CON.LT.0.0) CON = 0.0   !havent reached threshold
 
   CON = max(0.,AK1 * (Q - TH)) ! versao otimizada
!
ENDIF  
!
!
TOTAL = (CON + ACCRETE) * DT / RHO (L)  

!
IF (TOTAL.LT.QC (L) ) THEN  
!
   QC (L) = QC (L) - TOTAL  
   QH (L) = QH (L) + TOTAL    !no phase change involved
   RETURN  
!
ELSE  
!              
   QH (L) = QH (L) + QC (L)    !uses all there is
   QC (L) = 0.0  
!
ENDIF  
!
RETURN  
!
END SUBROUTINE CONVERT
!
!**********************************************************************
!
SUBROUTINE CONVERT2 ()  
!use module_zero_plumegen_coms  
implicit none
LOGICAL  AEROSOL
parameter(AEROSOL=.true.)
!
real, parameter :: TNULL=273.16, LAT=2.5008E6 &
                  ,EPSI=0.622 ,DB=1. ,NB=1500. !ALPHA=0.2 
real :: KA,KEINS,KZWEI,KDREI,VT	
real :: A,B,C,D, CON,ACCRETE,total 
      
real Y(6),ROH
      
A=0.
B=0.
Y(1) = T(L)
Y(4) = W(L)
y(2) = QC(L)
y(3) = QH(L)
Y(5) = RADIUS(L)
ROH =  RHO(L)*1.e-3 ! dens (MKS) ??


! autoconversion

KA = 0.0005 
IF( Y(1) .LT. 258.15 )THEN
!   KEINS=0.00075
    KEINS=0.0009 
    KZWEI=0.0052
    KDREI=15.39
ELSE
    KEINS=0.0015
    KZWEI=0.00696
    KDREI=11.58
ENDIF
      
!   ROH=PE/RD/TE
VT=-KDREI* (Y(3)/ROH)**0.125

 
IF (Y(4).GT.0.0 ) THEN
 IF (AEROSOL) THEN
   A = 1/y(4)  *  y(2)*y(2)*1000./( 60. *( 5. + 0.0366*NB/(y(2)*1000.*DB) )  )
 ELSE
   IF (y(2).GT.(KA*ROH)) THEN
   !print*,'1',y(2),KA*ROH
   A = KEINS/y(4) *(y(2) - KA*ROH )
   ENDIF
 ENDIF
ELSE
   A = 0.0
ENDIF

! accretion

IF(y(4).GT.0.0) THEN
   B = KZWEI/(y(4) - VT) * MAX(0.,y(2)) *   &
       MAX(0.001,ROH)**(-0.875)*(MAX(0.,y(3)))**(0.875)
ELSE
   B = 0.0
ENDIF
   
   
      !PSATW=610.7*EXP( 17.25 *( Y(1) - TNULL )/( Y(1)-36. ) )
      !PSATE=610.7*EXP( 22.33 *( Y(1) - TNULL )/( Y(1)- 2. ) )

      !QSATW=EPSI*PSATW/( PE-(1.-EPSI)*PSATW )
      !QSATE=EPSI*PSATE/( PE-(1.-EPSI)*PSATE )
      
      !MU=2.*ALPHA/Y(5)

      !C = MU*( ROH*QSATW - ROH*QVE + y(2) )
      !D = ROH*LAT*QSATW*EPSI/Y1/Y1/RD *DYDX1

      
      !DYDX(2) = - A - B - C - D  ! d rc/dz
      !DYDX(3) = A + B            ! d rh/dz
 
 
      ! rc=rc+dydx(2)*dz
      ! rh=rh+dydx(3)*dz
 
CON      = A
ACCRETE  = B
 
TOTAL = (CON + ACCRETE) *(1/DZM(L)) /ROH     ! DT / RHO (L)  

!print*,'L=',L,total,QC(L),dzm(l)

!
IF (TOTAL.LT.QC (L) ) THEN  
!
   QC (L) = QC (L) - TOTAL  
   QH (L) = QH (L) + TOTAL    !no phase change involved
   RETURN  
!
ELSE  
!              
   QH (L) = QH (L) + QC (L)    !uses all there is
   QC (L) = 0.0  
!
ENDIF  
!
RETURN  
!
END SUBROUTINE CONVERT2
! ice - effect on temperature
!      TTD = 0.0 
!      TTE = 0.0  
!       CALL ICE(QSATW,QSATE,Y(1),Y(2),Y(3), &
!               TTA,TTB,TTC,DZ,ROH,D,C,TTD,TTE)
!       DYDX(1) = DYDX(1) + TTD  + TTE ! DT/DZ on Temp
!
!**********************************************************************
!
SUBROUTINE SUBLIMATE  
!
! ********************* VAPOR TO ICE (USE EQUATION OT22)***************
!use module_zero_plumegen_coms  
!
real, PARAMETER :: EPS = 0.622, HEATFUS = 334., HEATSUBL = 2834., CP = 1.004
real, PARAMETER :: SRC = HEATSUBL / CP, FRC = HEATFUS / CP, TMELT = 273.3
real, PARAMETER :: TFREEZE = 269.3

real ::dtsubh,  dividend,divisor, subl
!
DTSUBH = 0.  
!
!selection criteria for sublimation
IF (T (L)  .GT. TFREEZE  ) RETURN  
IF (QV (L) .LE. QSAT (L) ) RETURN  
!
!     from (OT); correction factors for units applied
!
 DIVIDEND = ( (1.E6 / RHO (L) ) **0.475) * (QV (L) / QSAT (L) &
            - 1) * (QI (L) **0.525) * 1.13
 DIVISOR = 7.E5 + 4.1E6 / (10. * EST (L) )  
!
                                         
 DTSUBH = ABS (DIVIDEND / DIVISOR)   !sublimation rate
 SUBL = DTSUBH * DT                  !and amount possible
!
!     again check the possibilities
!
IF (SUBL.LT.QV (L) ) THEN  
!
   QV (L) = QV (L) - SUBL             !lose vapor
   QI (L) = QI (L) + SUBL  	      !gain ice
   T (L) = T (L) + SUBL * SRC         !energy change, warms air

	 !print*,'5',l,qi(l),SUBL

   RETURN  
!
ELSE  
!                                     
   QI (L) = QV (L)                    !use what there is
   T  (L) = T (L) + QV (L) * SRC      !warm the air
   QV (L) = 0.0  
	 !print*,'6',l,qi(l)
!
ENDIF  
!
RETURN  
END SUBROUTINE SUBLIMATE
!
! *********************************************************************
!
SUBROUTINE GLACIATE  
!
! *********************** CONVERSION OF RAIN TO ICE *******************
!     uses equation OT 16, simplest. correction from W not applied, but
!     vapor pressure differences are supplied.
!
!use module_zero_plumegen_coms  
!
real, PARAMETER :: HEATFUS = 334., CP = 1.004, EPS = 0.622, HEATSUBL = 2834.
real, PARAMETER :: FRC = HEATFUS / CP, FRS = HEATSUBL / CP, TFREEZE =  269.3
real, PARAMETER :: GLCONST = 0.025   !glaciation time constant, 1/sec
real dfrzh
!
                                    
 DFRZH = 0.    !rate of mass gain in ice
!
!selection rules for glaciation
IF (QH (L) .LE. 0.       ) RETURN  
IF (QV (L) .LT. QSAT (L) ) RETURN                                        
IF (T  (L) .GT. TFREEZE  ) RETURN  
!
!      NT=TMELT-T(L)
!      IF (NT.GT.50) NT=50
!
                                    
 DFRZH = DT * GLCONST * QH (L)    ! from OT(16)
!
IF (DFRZH.LT.QH (L) ) THEN  
!
   QI (L) = QI (L) + DFRZH  
   QH (L) = QH (L) - DFRZH  
   T (L) = T (L) + FRC * DFRZH  !warms air
   
   	 !print*,'7',l,qi(l),DFRZH

   
   RETURN  
!
ELSE  
!
   QI (L) = QI (L) + QH (L)  
   T  (L) = T  (L) + FRC * QH (L)  
   QH (L) = 0.0  

 !print*,'8',l,qi(l), QH (L)  
!
ENDIF  
!
RETURN  
!
END SUBROUTINE GLACIATE
!
!
! *********************************************************************
SUBROUTINE MELT  
!
! ******************* MAKES WATER OUT OF ICE **************************
!use module_zero_plumegen_coms  
!                                              
real, PARAMETER :: FRC = 332.27, TMELT = 273., F0 = 0.75   !ice velocity factor
real DTMELT
!                                    
 DTMELT = 0.   !conversion,ice to rain
!
!selection rules
IF (QI (L) .LE. 0.0  ) RETURN  
IF (T (L)  .LT. TMELT) RETURN  
!
                                                      !OT(23,24)
 DTMELT = DT * (2.27 / RHO (L) ) * CVI(L) * (T (L) - TMELT) * ( (RHO(L)  &
         * QI (L) * 1.E-6) **0.525) * (F0** ( - 0.42) )
                                                      !after Mason,1956
!
!     check the possibilities
!
IF (DTMELT.LT.QI (L) ) THEN  
!
   QH (L) = QH (L) + DTMELT  
   QI (L) = QI (L) - DTMELT  
   T  (L) = T (L) - FRC * DTMELT     !cools air
   	 !print*,'9',l,qi(l),DTMELT

   
   RETURN  
!
ELSE  
!
   QH (L) = QH (L) + QI (L)   !get all there is to get
   T  (L) = T (L) - FRC * QI (L)  
   QI (L) = 0.0  
   	 !print*,'10',l,qi(l)
!
ENDIF  
!
RETURN  
!
END SUBROUTINE MELT

SUBROUTINE htint (nzz1, vctra, eleva, nzz2, vctrb, elevb)
  IMPLICIT NONE
  INTEGER, INTENT(IN ) :: nzz1
  INTEGER, INTENT(IN ) :: nzz2
  REAL,    INTENT(IN ) :: vctra(nzz1)
  REAL,    INTENT(OUT) :: vctrb(nzz2)
  REAL,    INTENT(IN ) :: eleva(nzz1)
  REAL,    INTENT(IN ) :: elevb(nzz2)

  INTEGER :: l
  INTEGER :: k
  INTEGER :: kk
  REAL    :: wt

  l=1

  DO k=1,nzz2
     DO
        IF ( (elevb(k) <  eleva(1)) .OR. &
             ((elevb(k) >= eleva(l)) .AND. (elevb(k) <= eleva(l+1))) ) THEN
           wt       = (elevb(k)-eleva(l))/(eleva(l+1)-eleva(l))
           vctrb(k) = vctra(l)+(vctra(l+1)-vctra(l))*wt
           EXIT
        ELSE IF ( elevb(k) >  eleva(nzz1))  THEN
           wt       = (elevb(k)-eleva(nzz1))/(eleva(nzz1-1)-eleva(nzz1))
           vctrb(k) = vctra(nzz1)+(vctra(nzz1-1)-vctra(nzz1))*wt
           EXIT
        END IF

        l=l+1
        IF(l == nzz1) THEN
           PRINT *,'htint:nzz1',nzz1
           DO kk=1,l
              PRINT*,'kk,eleva(kk),elevb(kk)',eleva(kk),elevb(kk)
           END DO
           STOP 'htint'
        END IF
     END DO
  END DO
END SUBROUTINE htint
!-----------------------------------------------------------------------------
FUNCTION ESAT_PR (TEM)  
!
! ******* Vapor Pressure  A.L. Buck JAM V.20 p.1527. (1981) ***********
!
real, PARAMETER :: CI1 = 6.1115, CI2 = 22.542, CI3 = 273.48
real, PARAMETER :: CW1 = 6.1121, CW2 = 18.729, CW3 = 257.87, CW4 = 227.3
real, PARAMETER :: TMELT = 273.3

real ESAT_PR
real temc , tem,esatm
!
!     formulae from Buck, A.L., JAM 20,1527-1532
!     custom takes esat wrt water always. formula for h2o only
!     good to -40C so:
!
!
TEMC = TEM - TMELT  
IF (TEMC.GT. - 40.0) GOTO 230  
ESATM = CI1 * EXP (CI2 * TEMC / (TEMC + CI3) )  !ice, millibars  
ESAT_PR = ESATM / 10.	!kPa			  

RETURN  
!
230 ESATM = CW1 * EXP ( ( (CW2 - (TEMC / CW4) ) * TEMC) / (TEMC + CW3))
                          
ESAT_PR = ESATM / 10.	!kPa			  
RETURN  
END function ESAT_PR
!     ******************************************************************

!      ------------------------------------------------------------------------
END Module module_chem_plumerise_scalar
