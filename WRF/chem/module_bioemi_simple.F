MODULE module_bioemi_simple
! ..
! make sure that whatever you put in here agrees with dry_dep_simple
! and met model luse stuff. This should be improved, but currently,
! there is only usgs in wrf
!
  USE module_data_radm2
      INTEGER, PARAMETER ::  nlu = 25,  &
        iswater_temp = 16,isice_temp = 24
      REAL :: aefiso(nlu), aefmter(nlu), aefovoc(nlu), aef_n(nlu)
      CHARACTER (4),PARAMETER :: mminlu_loc = 'USGS'
      INTEGER :: ixxxlu(nlu)


    CONTAINS
      SUBROUTINE bio_emissions(id,ktau,dtstep,DX,                         &
               config_flags,                                              &
               gmt,julday,t_phy,moist,p8w,t8w,                            &
               e_bio,p_phy,chem,rho_phy,dz8w,ne_area,                     &
               ivgtyp,gsw,vegfra,rmol,ust,znt,xlat,xlong,z_at_w,          &
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, kts,kte                                  )
  USE module_configure
  USE module_state_description
  IMPLICIT NONE
   INTEGER,      INTENT(IN   ) :: id,julday, ne_area,                     &
                                  ids,ide, jds,jde, kds,kde,              &
                                  ims,ime, jms,jme, kms,kme,              &
                                  its,ite, jts,jte, kts,kte
   INTEGER,      INTENT(IN   ) ::                                         &
                                  ktau
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),               &
         INTENT(IN ) ::                                   moist
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                &
         INTENT(INOUT ) ::                                   chem
   REAL, DIMENSION( ims:ime, jms:jme, ne_area ),                          &
         INTENT(INOUT ) ::                               e_bio
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,              &
          INTENT(IN   ) ::                                                &
                                                      t_phy,              &
                                                      p_phy,              &
                                                      dz8w,               &
                                              t8w,p8w,z_at_w ,            &
                                                    rho_phy
   INTEGER,DIMENSION( ims:ime , jms:jme )                  ,              &
          INTENT(IN   ) ::                                                &
                                                     ivgtyp
   REAL,  DIMENSION( ims:ime , jms:jme )                   ,              &
          INTENT(IN   ) ::                                                &
                                                     gsw,                 &
                                                  vegfra,                 &
                                                     rmol,                &
                                                     ust,                 &
                                                     xlat,                &
                                                     xlong,               &
                                                     znt
      REAL,      INTENT(IN   ) ::                                         &
                             dtstep,dx,gmt
!--- deposition and emissions stuff
! .. Parameters ..
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

! ..
! .. Local Arrays ..
! .. Parameters ..
!     INTEGER, PARAMETER ::  nlu = 25,  &
!       nseason = 1, nseasons = 2
!     REAL :: aefiso(nlu), aefmter(nlu), aefovoc(nlu), aef_n(nlu)
      REAL :: emiss_bio(ne_area) !wig, 1-May-2007: switched to ne_area from numgas to make more general
      LOGICAL :: highnh3, rainflag, vegflag, wetflag
      CHARACTER (4) :: luse_typ
! ..
! .. Local Scalars ..
      REAL ::  clwchem,eiso,eisoc,emter,emterc,eovoc,eovocc,e_n,e_nn,  &
        pa,rad, rhchem, ta, ustar, vegfrac, vocsc, xtimin, z1,zntt
      INTEGER :: i,j,iland, iprt, iseason, n, nr, ipr,jpr,nvr


! .. Intrinsic Functions ..
      INTRINSIC max, min
!
      luse_typ=mminlu_loc
!     print *,'luse_typ,iswater',luse_typ,iswater_temp
      iseason=1
      if(julday.lt.90.or.julday.gt.270)then
        iseason=2
        CALL wrf_debug(100,'setting iseason in bio_emi to 2')
      endif
        
                         
!  test program to test chemics stuff in 1-d                   
                         
!     first prepare for biogenic emissions                      
                         
      CALL bioemiin(iseason,luse_typ,vegflag) 
      do 100 j=jts,jte  
      do 100 i=its,ite  
      iland = ivgtyp(i,j)
      ta = t_phy(i,kts,j)      
      rad = gsw(i,j)
      vegfrac = vegfra(i,j)
      pa = .01*p_phy(i,kts,j)
      clwchem = moist(i,kts,j,p_qc)
      ustar = ust(i,j) 
      zntt = znt(i,j)                                                 
      z1 = z_at_w(i,kts+1,j)-z_at_w(i,kts,j)                          
                                                                      
!     Set logical default values                                      
      rainflag = .FALSE.                                              
      wetflag = .FALSE.                                               
      highnh3 = .FALSE.                                               
                                                                      
      if(moist(i,kts,j,p_qr).gt.0.)rainflag = .true.                  
!     if(raincv(i,kts,j).gt.0. .and. rainncv(i,kts,j).gt.0. )rainflag = .true.      
                                                                      
!     qvs  = 380.*exp(17.27*(tair-273.)/(tair-36.))/pressure          
      rhchem = MIN( 100.,100. * moist(i,kts,j,p_qv) / &               
               (3.80*exp(17.27*(t_phy(i,kts,j)-273.)/(t_phy(i,kts,j)-36.))/pa))     
      rhchem = max(rhchem,5.)
      if (rhchem >= 95.) wetflag = .true.                             
!     print *,chem(i,kts,j,p_nh3),chem(i,kts,j,p_so2)
      if(chem(i,kts,j,p_nh3).gt.2.*chem(i,kts,j,p_so2))highnh3 = .true.
      iseason = 1                                                     
!--- biogenic emissions
      emiss_bio=0.
      CALL biogen(iland,ta,rad,eiso,emter,eovoc,e_n,vocsc,eisoc,emterc,eovocc, &
        e_nn,pa,luse_typ,iseason,vegflag)
!     if(i.eq.5.and.j.eq.5)then
!         print *,iland
!         print *,ta,rad,vocsc,pa,luse_typ,aefiso,aefovoc,aefmter, &
!                aef_n,ixxxlu,vegflag,isice_temp,iswater_temp
!         PRINT *, eiso, emter, eovoc, e_n, eisoc, eovocc
!     endif

!     PRINT *, eiso, emter, eovoc, e_n, eisoc, eovocc                 
      CALL biosplit(iland,eiso,emter,eovoc,e_n,emiss_bio,ne_area,vegfrac, &
        config_flags, luse_typ,vegflag)
!     PRINT *, 'emiss_bio(liso)  emiss_bio(lald)  emiss_bio(lhcho) ', &
!       ' emiss_bio(lhc3)'
!     PRINT *, emiss_bio(liso), emiss_bio(lald), emiss_bio(lhcho), &
!       emiss_bio(lhc3)
      DO n = 1, ne_area-2
        e_bio(i,j,n) = emiss_bio(n)
!       if(i.eq.5.and.j.eq.5)print *,emiss_bio(n)
      END DO
 100  continue
END SUBROUTINE bio_emissions
! **********************************************************************
! **********************  SUBROUTINE BIOEMIIN **************************
! **********************************************************************
      SUBROUTINE bioemiin(isn,mminlu,vegflag)
!**********************************************************************
!     THIS SUBROUTINE INITIALIZES THE EMISSION FACTORS
!     AND THE SIMPLIFIED LANDUSE SCHEME
!     FOR THE BIOGENIC EMISSION AND DEPOSITION SUBROUTINES
!     WRITTEN BY: WINFRIED SEIDL (MARCH 2000)
!     CALLED BY:
!     CALLS:      -
!**********************************************************************
!**********************************************************************
!     REFERENCES FOR EMISSION FACTORS:
!     (S+R)  T. Schoenemeyer and K. Richter
!     (S95)  D. Simpson, A. Guenther, C. N. Hewitt, and R. Steinbrecher
!            J. Geophysical Research 100D (1995), 22875-22890
!     (G94)  A. Guenther, P. Zimmerman and M. Wildermuth
!            Atmospheric Environment 28 (1994), 1197-1210
!     (Z88)  P. R. Zimmerman, J. P. Greenberg, and C. E. Westberg
!            J. Geophysical Research 93D (1988), 1407-1416
!     (K88)  W. A. Kaplan, S. C. Wofsy, M. Keller, and J. M. da Costa
!            J. Geophysical Research 93D (1988), 1389-1395
!     (K94)  L. F. Klinger, P. R. Zimmermann, J. P. Greenberg, L. E. Hei
!            and A. B. Guenther
!            J. Geophysical Research 99D (1994), 1469-1494
!     ---------------------------------------------------------
!     PCU/NCAR landuse categories:
!        1 Highrise urban area
!        2 Agricultural land
!        3 Grassland, rangeland
!        4 Deciduous forest
!        5 Coniferous forest
!        6 Mixed forest (including wetland)
!        7 Water
!        8 Wet rangeland, nonforested wetland
!        9 Desert
!       10 Tundra
!       11 Permanent ice
!       12 Tropical  forest land
!       13 Savannah
!     ---------------------------------------------------------
!     USGS landuse categories:
!        1 Urban and built-up land
!        2 Dryland cropland and pasture
!        3 Irrigated cropland and pasture
!        4 Mix. dry/irrg. cropland and pasture
!        5 Cropland/grassland mosaic
!        6 Cropland/woodland mosaic
!        7 Grassland
!        8 Shrubland
!        9 Mixed shrubland/grassland
!       10 Savanna
!       11 Deciduous broadleaf forest
!       12 Deciduous needleleaf forest
!       13 Evergreen broadleaf forest
!       14 Evergreen needleleaf forest
!       15 Mixed Forest
!       16 Water Bodies
!       17 Herbaceous wetland
!       18 Wooded wetland
!       19 Barren or sparsely vegetated
!       20 Herbaceous Tundra
!       21 Wooded Tundra
!       22 Mixed Tundra
!       23 Bare Ground Tundra
!       24 Snow or Ice
!       25 No data
!     ---------------------------------------------------------
!     SiB landuse categories:
!        1 Evergreen broadleaf trees
!        2 Broadleaf deciduous trees
!        3 Deciduous and evergreen trees
!        4 Evergreen needleleaf trees
!        5 Deciduous needleleaf trees
!        6 Ground cover with trees and shrubs
!        7 Ground cover only
!        8 Broadleaf shrub with Perennial ground cover
!        9 Broadleaf shrub with bare soil
!       10 Groundcover with dwarf trees and shrubs
!       11 Bare soil
!       12 Agriculture or C3 grassland
!       13 Persistent Wetland
!       14 Dry coastal complexes
!       15 Water
!       16 Ice cap and glacier
!       17 No data
!--------------------------------------------------------------
! .. Scalar Arguments ..
        LOGICAL :: vegflag
        CHARACTER (4) :: mminlu
        INTEGER :: isn
! ..
! .. Array Arguments ..
!       REAL :: aefiso(nlu), aefmter(nlu), aefovoc(nlu), aef_n(nlu)
!       INTEGER :: ixxxlu(nlu)
! ..
! .. Local Scalars ..
!       INTEGER :: nseas
        INTEGER :: sum
! ..
!**********************************************************************
!     Emission Factors for Isoprene in ug C/(m*m*h)
!       PRINT *, 'mminlu = ', mminlu
        IF (mminlu=='OLD ') THEN
! urban                                 
          aefiso(1) = 0.
! agriculture (S+R)                     
          aefiso(2) = 8.
! grassland (S+R)                       
          aefiso(3) = 0.
! deciduous (G94)                       
          aefiso(4) = 4400.
! coniferous (G94)                      
          aefiso(5) = 780.
! mixed forest (G94)                    
          aefiso(6) = 5775.
! water                                 
          aefiso(7) = 0.
! wetland, emission unknown             
          aefiso(8) = 0.
! desert                                
          aefiso(9) = 0.
! tundra (K94)                          
          aefiso(10) = 70.
! ice                                   
          aefiso(11) = 0.
! tropical forest (Z88)                 
          aefiso(12) = 3100.
! savanna (Z88)                         
          aefiso(13) = 0
        END IF
        IF (mminlu=='USGS') THEN
! urban                                 
          aefiso(1) = 0.
! agriculture (S+R)                     
          aefiso(2) = 8.
! agriculture (S+R)                     
          aefiso(3) = 8.
! agriculture (S+R)                     
          aefiso(4) = 8.
! half agriculture/grassland assumed    
          aefiso(5) = 4.
! half agriculture/deciduous assumed    
          aefiso(6) = 2204.
! grassland (S+R)                       
          aefiso(7) = 0.
! grassland assumed                     
          aefiso(8) = 0.
! grassland assumed                     
          aefiso(9) = 0.
! savanna (Z88)                         
          aefiso(10) = 0.
! deciduous (G94)                       
          aefiso(11) = 4400.
! coniferous (G94)                      
          aefiso(12) = 780.
! deciduous (G94)                       
          aefiso(13) = 4400.
! coniferous (G94)                      
          aefiso(14) = 780.
! mixed forest (G94)                    
          aefiso(15) = 5775.
! water                                 
          aefiso(16) = 0.
! wetland emission unknown              
          aefiso(17) = 0.
! mixed forest assumed                  
          aefiso(18) = 5775.
! barren                                
          aefiso(19) = 0.
! tundra (K94) assumed                  
          aefiso(20) = 70.
! tundra (K94) assumed                  
          aefiso(21) = 70.
! tundra (K94) assumed                  
          aefiso(22) = 70.
! barren tundra                         
          aefiso(23) = 0.
! ice                                   
          aefiso(24) = 0.
! no data                               
          aefiso(25) = 0.
        END IF
        IF (mminlu=='SiB ') THEN
! deciduous (G94)                       
          aefiso(1) = 4400.
! deciduous (G94)                       
          aefiso(2) = 4400.
! deciduous (G94)                       
          aefiso(3) = 4400.
! coniferous (G94)                      
          aefiso(4) = 780.
! coniferous (G94)                      
          aefiso(5) = 780.
! grassland assumed                     
          aefiso(6) = 0.
! grassland assumed                     
          aefiso(7) = 0.
! grassland assumed                     
          aefiso(8) = 0.
! grassland assumed                     
          aefiso(9) = 0.
! grassland assumed                     
          aefiso(10) = 0.
! bare soil                             
          aefiso(11) = 0.
! agriculture (S+R)                     
          aefiso(12) = 8.
! wetland, emission unknown             
          aefiso(13) = 0.
! dry, coastal                          
          aefiso(14) = 0.
! water                                 
          aefiso(15) = 0.
! ice                                   
          aefiso(16) = 0.
! no data                               
          aefiso(17) = 0.
        END IF
!     ---------------------------------------------------------
!     Emission Factors for Monoterpenes in ug C/(m*m*h)

        IF (mminlu=='OLD ') THEN
! urban                                 
          aefmter(1) = 0.
! agriculture (S+R)                     
          aefmter(2) = 20.
! grassland (S+R)                       
          aefmter(3) = 20.
! deciduous (G94)                       
          aefmter(4) = 385.
! coniferous (G94)                      
          aefmter(5) = 1380.
! mixed forest (G94)                    
          aefmter(6) = 1001.
! water                                 
          aefmter(7) = 0.
! wetland, emission unknown             
          aefmter(8) = 0.
! desert                                
          aefmter(9) = 0.
! tundra (K94)                          
          aefmter(10) = 0.
! ice                                   
          aefmter(11) = 0.
! tropical forest (Z88)                 
          aefmter(12) = 270.
! savanna (Z88)                         
          aefmter(13) = 0
        END IF
        IF (mminlu=='USGS') THEN
! urban                                 
          aefmter(1) = 0.
! agriculture (S+R)                     
          aefmter(2) = 20.
! agriculture (S+R)                     
          aefmter(3) = 20.
! agriculture (S+R)                     
          aefmter(4) = 20.
! half agriculture/grassland assumed    
          aefmter(5) = 20.
! half agriculture/deciduous assumed    
          aefmter(6) = 202.5
! grassland (S+R)                       
          aefmter(7) = 20.
! grassland assumed                     
          aefmter(8) = 20.
! grassland assumed                     
          aefmter(9) = 20.
! savanna (Z88)                         
          aefmter(10) = 0
! deciduous (G94)                       
          aefmter(11) = 385.
! coniferous (G94)                      
          aefmter(12) = 1380.
! deciduous (G94)                       
          aefmter(13) = 385.
! coniferous (G94)                      
          aefmter(14) = 1380.
! mixed forest (G94)                    
          aefmter(15) = 1001.
! water                                 
          aefmter(16) = 0.
! wetland emission unknown              
          aefmter(17) = 0.
! mixed forest assumed                  
          aefmter(18) = 1001.
! barren                                
          aefmter(19) = 0.
! tundra (K94) assumed                  
          aefmter(20) = 0.
! tundra (K94) assumed                  
          aefmter(21) = 0.
! tundra (K94) assumed                  
          aefmter(22) = 0.
! barren tundra                         
          aefmter(23) = 0.
! ice                                   
          aefmter(24) = 0.
! no data                               
          aefmter(25) = 0.
        END IF
        IF (mminlu=='SiB ') THEN
! deciduous (G94)                       
          aefmter(1) = 385.
! deciduous (G94)                       
          aefmter(2) = 385.
! deciduous (G94)                       
          aefmter(3) = 385.
! coniferous (G94)                      
          aefmter(4) = 1380.
! coniferous (G94)                      
          aefmter(5) = 1380.
! grassland assumed                     
          aefmter(6) = 20.
! grassland assumed                     
          aefmter(7) = 20.
! grassland assumed                     
          aefmter(8) = 20.
! grassland assumed                     
          aefmter(9) = 20.
! grassland assumed                     
          aefmter(10) = 20.
! bare soil                             
          aefmter(11) = 0.
! agriculture (S+R)                     
          aefmter(12) = 20.
! wetland, emission unknown             
          aefmter(13) = 0.
! dry, coastal                          
          aefmter(14) = 0.
! water                                 
          aefmter(15) = 0.
! ice                                   
          aefmter(16) = 0.
! no data                               
          aefmter(17) = 0.
        END IF
!     ---------------------------------------------------------
!     Emission Factors for Other VOCs in ug C/(m*m*h)

        IF (mminlu=='OLD ') THEN
! urban                                 
          aefovoc(1) = 0.
! agriculture (S+R)                     
          aefovoc(2) = 12.
! grassland (S+R)                       
          aefovoc(3) = 80.
! deciduous (G94)                       
          aefovoc(4) = 715.
! coniferous (G94)                      
          aefovoc(5) = 840.
! mixed forest (G94)                    
          aefovoc(6) = 924.
! water                                 
          aefovoc(7) = 0.
! wetland, emission unknown             
          aefovoc(8) = 0.
! desert                                
          aefovoc(9) = 0.
! tundra (K94)                          
          aefovoc(10) = 0.
! ice                                   
          aefovoc(11) = 0.
! tropical forest (Z88)                 
          aefovoc(12) = 0.
! savanna (Z88)                         
          aefovoc(13) = 0
        END IF
        IF (mminlu=='USGS') THEN
! urban                                 
          aefovoc(1) = 0.
! agriculture (S+R)                     
          aefovoc(2) = 12.
! agriculture (S+R)                     
          aefovoc(3) = 12.
! agriculture (S+R)                     
          aefovoc(4) = 12.
! half agriculture/grassland assumed    
          aefovoc(5) = 46.
! half agriculture/deciduous assumed    
          aefovoc(6) = 363.5
! grassland (S+R)                       
          aefovoc(7) = 80.
! grassland assumed                     
          aefovoc(8) = 80.
! grassland assumed                     
          aefovoc(9) = 80.
! savanna (Z88)                         
          aefovoc(10) = 0
! deciduous (G94)                       
          aefovoc(11) = 715.
! coniferous (G94)                      
          aefovoc(12) = 840.
! deciduous (G94)                       
          aefovoc(13) = 715.
! coniferous (G94)                      
          aefovoc(14) = 840.
! mixed forest (G94)                    
          aefovoc(15) = 924.
! water                                 
          aefovoc(16) = 0.
! wetland emission unknown              
          aefovoc(17) = 0.
! mixed forest assumed                  
          aefovoc(18) = 924.
! barren                                
          aefovoc(19) = 0.
! tundra (K94) assumed                  
          aefovoc(20) = 0.
! tundra (K94) assumed                  
          aefovoc(21) = 0.
! tundra (K94) assumed                  
          aefovoc(22) = 0.
! barren tundra                         
          aefovoc(23) = 0.
! ice                                   
          aefovoc(24) = 0.
! no data                               
          aefovoc(25) = 0.
        END IF
        IF (mminlu=='SiB ') THEN
! deciduous (G94)                       
          aefovoc(1) = 715.
! deciduous (G94)                       
          aefovoc(2) = 715.
! deciduous (G94)                       
          aefovoc(3) = 715.
! coniferous (G94)                      
          aefovoc(4) = 840.
! coniferous (G94)                      
          aefovoc(5) = 840.
! grassland assumed                     
          aefovoc(6) = 80.
! grassland assumed                     
          aefovoc(7) = 80.
! grassland assumed                     
          aefovoc(8) = 80.
! grassland assumed                     
          aefovoc(9) = 80.
! grassland assumed                     
          aefovoc(10) = 80.
! bare soil                             
          aefovoc(11) = 0.
! agriculture (S+R)                     
          aefovoc(12) = 12.
! wetland, emission unknown             
          aefovoc(13) = 0.
! dry, coastal                          
          aefovoc(14) = 0.
! water                                 
          aefovoc(15) = 0.
! ice                                   
          aefovoc(16) = 0.
! no data                               
          aefovoc(17) = 0.
        END IF
!     ---------------------------------------------------------
!     Emission Factors for Nitrogen in ng N /(m*m*sec)

        IF (mminlu=='OLD ') THEN
! urban                                 
          aef_n(1) = 0.
! agriculture (S+R)                     
          aef_n(2) = 9.
! grassland (S+R)                       
          aef_n(3) = 0.9
! deciduous (G94)                       
          aef_n(4) = 0.07
! coniferous (G94)                      
          aef_n(5) = 0.07
! mixed forest (G94)                    
          aef_n(6) = 0.07
! water                                 
          aef_n(7) = 0.
! wetland, emission unknown             
          aef_n(8) = 0.
! desert                                
          aef_n(9) = 0.
! tundra (K94)                          
          aef_n(10) = 0.
! ice                                   
          aef_n(11) = 0.
! tropical forest (Z88)                 
          aef_n(12) = 1.78
! savanna (Z88)                         
          aef_n(13) = 0
        END IF
        IF (mminlu=='USGS') THEN
! urban                                 
          aef_n(1) = 0.
! agriculture (S+R)                     
          aef_n(2) = 9.
! agriculture (S+R)                     
          aef_n(3) = 9.
! agriculture (S+R)                     
          aef_n(4) = 9.
! half agriculture/grassland assumed    
          aef_n(5) = 4.95
! half agriculture/deciduous assumed    
          aef_n(6) = 4.535
! grassland (S+R)                       
          aef_n(7) = 0.9
! grassland assumed                     
          aef_n(8) = 0.07
! grassland assumed                     
          aef_n(9) = 0.07
! savanna (Z88)                         
          aef_n(10) = 0.
! deciduous (G94)                       
          aef_n(11) = 0.07
! coniferous (G94)                      
          aef_n(12) = 0.07
! deciduous (G94)                       
          aef_n(13) = 0.07
! coniferous (G94)                      
          aef_n(14) = 0.07
! mixed forest (G94)                    
          aef_n(15) = 0.07
! water                                 
          aef_n(16) = 0.
! wetland emission unknown              
          aef_n(17) = 0.
! mixed forest assumed                  
          aef_n(18) = 0.07
! barren                                
          aef_n(19) = 0.
! tundra (K94) assumed                  
          aef_n(20) = 0.
! tundra (K94) assumed                  
          aef_n(21) = 0.
! tundra (K94) assumed                  
          aef_n(22) = 0.
! barren tundra                         
          aef_n(23) = 0.
! ice                                   
          aef_n(24) = 0.
! no data                               
          aef_n(25) = 0.
        END IF
        IF (mminlu=='SiB ') THEN
! deciduous (G94)                       
          aef_n(1) = 0.07
! deciduous (G94)                       
          aef_n(2) = 0.07
! deciduous (G94)                       
          aef_n(3) = 0.07
! coniferous (G94)                      
          aef_n(4) = 0.07
! coniferous (G94)                      
          aef_n(5) = 0.07
! natural vegetation assumed            
          aef_n(6) = 0.07
! grassland assumed                     
          aef_n(7) = 0.9
! natural vegetation assumed            
          aef_n(8) = 0.07
! natural vegetation assumed            
          aef_n(9) = 0.07
! natural vegetation assumed            
          aef_n(10) = 0.07
! bare soil                             
          aef_n(11) = 0.
! agriculture (S+R)                     
          aef_n(12) = 9.
! wetland, emission unknown             
          aef_n(13) = 0.
! dry, coastal                          
          aef_n(14) = 0.
! water                                 
          aef_n(15) = 0.
! ice                                   
          aef_n(16) = 0.
! no data                               
          aef_n(17) = 0.
        END IF
!     *********************************************************

!     Simplified landuse scheme for deposition and biogenic emission
!     subroutines
!     (ISWATER and ISICE are already defined elsewhere,
!     therefore water and ice are not considered here)

!     1 urban or bare soil
!     2 agricultural
!     3 grassland
!     4 deciduous forest
!     5 coniferous and mixed forest
!     6 other natural landuse categories


        IF (mminlu=='OLD ') THEN
          ixxxlu(1) = 1
          ixxxlu(2) = 2
          ixxxlu(3) = 3
          ixxxlu(4) = 4
          ixxxlu(5) = 5
          ixxxlu(6) = 5
          ixxxlu(7) = 0
          ixxxlu(8) = 6
          ixxxlu(9) = 1
          ixxxlu(10) = 6
          ixxxlu(11) = 0
          ixxxlu(12) = 4
          ixxxlu(13) = 6
        END IF
        IF (mminlu=='USGS') THEN
          ixxxlu(1) = 1
          ixxxlu(2) = 2
          ixxxlu(3) = 2
          ixxxlu(4) = 2
          ixxxlu(5) = 2
          ixxxlu(6) = 4
          ixxxlu(7) = 3
          ixxxlu(8) = 6
          ixxxlu(9) = 3
          ixxxlu(10) = 6
          ixxxlu(11) = 4
          ixxxlu(12) = 5
          ixxxlu(13) = 4
          ixxxlu(14) = 5
          ixxxlu(15) = 5
          ixxxlu(16) = 0
          ixxxlu(17) = 6
          ixxxlu(18) = 4
          ixxxlu(19) = 1
          ixxxlu(20) = 6
          ixxxlu(21) = 4
          ixxxlu(22) = 6
          ixxxlu(23) = 1
          ixxxlu(24) = 0
          ixxxlu(25) = 1
        END IF
        IF (mminlu=='SiB ') THEN
          ixxxlu(1) = 4
          ixxxlu(2) = 4
          ixxxlu(3) = 4
          ixxxlu(4) = 5
          ixxxlu(5) = 5
          ixxxlu(6) = 6
          ixxxlu(7) = 3
          ixxxlu(8) = 6
          ixxxlu(9) = 6
          ixxxlu(10) = 6
          ixxxlu(11) = 1
          ixxxlu(12) = 2
          ixxxlu(13) = 6
          ixxxlu(14) = 1
          ixxxlu(15) = 0
          ixxxlu(16) = 0
          ixxxlu(17) = 1
        END IF


!**********************************************************************
! Calculation of seasonal dependence of emissions
!**********************************************************************
! (if the season is variable during the model run,
! this section should be placed in the beginning of subroutine BIOGEN)
!**********************************************************************


        IF (mminlu=='OLD ') THEN
! WINTER                              
          IF (isn==2) THEN
! agriculture                         
            aefiso(2) = 0.
! deciduous                           
            aefiso(4) = 0.
! mixed forest                        
            aefiso(6) = 5775./2.
! tundra                              
            aefiso(10) = 0.
! agriculture                         
            aefmter(2) = 0.
! deciduous                           
            aefmter(4) = 0.
! mixed forest                        
            aefmter(6) = 1001./2.
! agriculture                         
            aefovoc(2) = 0.
! deciduous                           
            aefovoc(4) = 0.
! mixed forest                        
            aefovoc(6) = 924./2.
          END IF
        END IF

        IF (mminlu=='USGS') THEN
!       DOES VEGETATION FRACTION EXIST?
          sum = 0.
!       DO J=1,jl-1
!         DO I=1,il-1
!           SUM=SUM+VEGFRC(I,J)
!         END DO
!       END DO
          IF (sum>1) THEN
            vegflag = .TRUE.
          ELSE
            vegflag = .FALSE.
          END IF
!         VEGFLAG=.FALSE.
          IF (( .NOT. vegflag) .AND. (isn==2)) THEN
!       IF ((.NOT.VEGFLAG)) THEN
!         VEGETATION FRACTION DOES NOT EXIST,
!         CORRECTION FOR WINTER SEASON
! agriculture                         
            aefiso(2) = 0.
! agriculture                         
            aefiso(3) = 0.
! agriculture                         
            aefiso(4) = 0.
! half agriculture/grassland assumed  
            aefiso(5) = 0.
! half agriculture/deciduous assumed  
            aefiso(6) = 0.
! deciduous broadleaf                 
            aefiso(11) = 0.
! deciduous needleleaf                
            aefiso(12) = 0.
! mixed forest                        
            aefiso(15) = 5775./2.
! mixed forest assumed                
            aefiso(18) = 5775./2.
! tundra                              
            aefiso(20) = 0.
! tundra                              
            aefiso(21) = 0.
! tundra                              
            aefiso(22) = 0.
! agriculture                         
            aefmter(2) = 0.
! agriculture                         
            aefmter(3) = 0.
! agriculture                         
            aefmter(4) = 0.
! half agriculture/grassland assumed  
            aefmter(5) = 10.
! half agriculture/deciduous assumed  
            aefmter(6) = 0.
! deciduous broadleaf                 
            aefmter(11) = 0.
! deciduous needleleaf                
            aefmter(12) = 0.
! mixed forest                        
            aefmter(15) = 1001./2.
! mixed forest assumed                
            aefmter(18) = 1001./2.
! agriculture                         
            aefovoc(2) = 0.
! agriculture                         
            aefovoc(3) = 0.
! agriculture                         
            aefovoc(4) = 0.
! half agriculture/grassland assumed  
            aefovoc(5) = 40.
! half agriculture/deciduous assumed  
            aefovoc(6) = 0.
! deciduous broadleaf                 
            aefovoc(11) = 0.
! deciduous needleleaf                
            aefovoc(12) = 0.
! mixed forest                        
            aefovoc(15) = 924./2.
! mixed forest assumed                
            aefovoc(18) = 924./2.
          END IF
        END IF

        IF (mminlu=='SiB ') THEN
! WINTER                              
          IF (isn==2) THEN
! deciduous                           
            aefiso(1) = 0.
! deciduous                           
            aefiso(2) = 0.
! deciduous                           
            aefiso(3) = 0.
! agriculture                         
            aefiso(12) = 0.
! deciduous                           
            aefmter(1) = 0.
! deciduous                           
            aefmter(2) = 0.
! deciduous                           
            aefmter(3) = 0.
! agriculture                         
            aefmter(12) = 0.
! deciduous                           
            aefovoc(1) = 0.
! deciduous                           
            aefovoc(2) = 0.
! deciduous                           
            aefovoc(3) = 0.
! agriculture                         
            aefovoc(12) = 0.
          END IF
        END IF

      END SUBROUTINE bioemiin
! **********************************************************************
! ***********************  SUBROUTINE BIOGEN  **************************
! **********************************************************************
      SUBROUTINE biogen(iland,ta,rad,eiso,emter,eovoc,e_n,vocsc,eisoc,emterc, &
          eovocc,e_nn,pa,mminlu,isn,vegflag)

!     THIS PROGRAMM COMPUTES THE ACTUAL BIOGENIC EMISSION RATE FOR
!     ISOPRENE, MONTERPENES, OTHER ORGANIC COMPOUNDS, AND NITROGEN FOR
!     EACH GRID CELL DEPENDING ON TEMPERATURE AND GLOBAL RADIATION
!***********************************************************************
!     PROGRAM DEVELOPED BY:- THOMAS SCHOENEMEYER  (5 JANUARY 1995)
!     MODIFIED BY:         - THOMAS SCHOENEMEYER (21 AUGUST 1996)
!                            UND KLAUS RICHTER
!                            NACH SIMPSON ET AL.
!                          - WINFRIED SEIDL (JUNE 1997)
!                            ADAPTATION FOR USE IN MM5
!                          - WINFRIED SEIDL (MARCH 2000)
!                            MODIFICATION FOR MM5 VERSION 3
!                          - Georg Grell (March 2002) for f90 and WRF
!***********************************************************************
!...PROGRAM VARIABLES...
!        ILAND      - Land use category
!        TA         - Air temperature in K
!        RAD        - Solare global radiation in W/m2
!        EISO       - Emission von Isopren in ppm*m/min
!        EMTER      - Emission von Monoterpenen in ppm*m/min
!        EOVOC      - Emission sonstiger fluechtiger Kohlenwasserstoffe
!                      in ppm*m/min
!        E_N        - Emission von Stickstoff in ppm*m/min
!        AEFISO(NLU) - Emissionsfaktor fuer Isopren fuer die Land-
!                      nutzungsart K, standardisiert auf 303 K und
!                      voller Sonneneinstrahlung in ug C /(m*m*h)
!        AEFOVOC(NLU)- Emissionsfaktor fuer sonstige fluechtige
!                      Kohlenwasserstoffe in ug C /(m*m*h)
!        AEFMTER(NLU)- Emissionsfaktor fuer MONOTERPENE
!                      in ug C /(m*m*h)
!        AEF_N(NLU)  - Emissionsfaktor fuer Stickstoff
!                      in ng N /(m*m*sec)
!        ECF_ISO    - dimensionsloser Korrekturfaktor fuer Isopren,
!                      abhaengig von Temperatur und Strahlung
!        ECF_OVOC     dimensionsloser Korrekturfaktor fuer die
!                      sonstigen fluechtigen Kohlenwasserstoffe
!        ECF_MTER     dimensionsloser Korrekturfaktor fuer die
!                      MONOTERPENE
!        ECF_N      - dimensionsloser Korrekturfaktor fuer
!                      Stickstoff
! .. Scalar Arguments ..
        REAL :: eiso, eisoc, emter, emterc, eovoc, eovocc, e_n, e_nn, pa, rad, &
          ta, vocsc
        INTEGER :: iland, isn
        LOGICAL :: vegflag
        CHARACTER (4) :: mminlu
! ..
! .. Array Arguments ..
!       REAL :: aefiso(nlu), aefmter(nlu), aefovoc(nlu), aef_n(nlu)
!       INTEGER :: ixxxlu(nlu)
! ..
! .. Local Scalars ..
        REAL :: alpha, beta, cl, cl1, coniso, conn, conovoc, conter, ct, ct1, &
          ct2, ecf_iso, ecf_mter, ecf_n, ecf_ovoc, par, r, rat, tm, ts, tsoil
! ..
! .. Intrinsic Functions ..
        INTRINSIC exp, sqrt
! ..
! empirischer Koeffizient                     
        alpha = 0.0027
! empirischer Koeffizient                     
        cl1 = 1.066
! Gaskonstante in J/(K*mol)                   
        r = 8.314
! empirischer Koeffizient in J/mol            
        ct1 = 95000
! empirischer Koeffizient in J/mol            
        ct2 = 230000
! empirischer Koeffizient in K                
        tm = 314.
! faktoren bestimmt werden
        ts = 303.
! Standardtemperatur bei der Emissions-       
        beta = 0.09
!**********************************************************************
!**********************************************************************
!  Temperature and Radiation Dependent Correction Factors
!  for Emissions
!**********************************************************************
!**********************************************************************


!     *****************************************************************
!     Forest land use categories

! empirischer TemperaturKoeffizient           
        IF ((ixxxlu(iland)==4) .OR. (ixxxlu(iland)==5)) THEN
!                             ! = photosynthetisch aktive Strahlung;
          par = 2.0*rad
!                             ! Umrechnungsfaktor: 2.0 uE/J (beruecksich
! auch, dass PAR ein kleinerer Wellenlaeng
! bereich ist als die Globalstrahlung.
! Langholz und Haeckl, 1985, Met. Rundscha

! PAR flux in Mikromol je m**2 und s      
          cl = alpha*cl1*par/sqrt(1+alpha*alpha*par*par)
          ct = exp(ct1*(ta-ts)/(r*ts*ta))/(1+exp(ct2*(ta-tm)/(r*ts*ta)))

          ecf_iso = cl*ct
! Korrekturfaktor fuer Isopr
          ecf_mter = exp(beta*(ta-ts)) ! Korrekturfaktor fuer MTER 
          ecf_ovoc = ecf_mter
! Korrekturfaktor fuer OVOC 
          tsoil = 0.84*(ta-273.15) + 3.6
          ecf_n = exp(0.071*tsoil)
! Korrekturfaktor fuer N    
        END IF

!     *****************************************************************
!     Agricultural land use category

        IF (ixxxlu(iland)==2) THEN
          ecf_iso = exp(0.1*(ta-30.-273.15)) ! vgl. Hahn et al. 
          ecf_mter = ecf_iso
          ecf_ovoc = ecf_iso

          tsoil = 0.72*(ta-273.15) + 5.8
          ecf_n = exp(0.071*tsoil)
        END IF

!     *****************************************************************
!     Grassland and natural nonforested land use categories

        IF ((ixxxlu(iland)==3) .OR. (ixxxlu(iland)==6)) THEN
          ecf_iso = exp(0.1*(ta-30.-273.15)) ! vgl. Hahn et al. 
          ecf_mter = ecf_iso
          ecf_ovoc = ecf_iso

          tsoil = 0.66*(ta-273.15) + 8.8
          ecf_n = exp(0.071*tsoil)
        END IF

!     *****************************************************************
!     Non-emitting land use categories

        IF ((ixxxlu(iland)==1) .OR. (iland==iswater_temp) .OR. (iland==isice_temp)) THEN
          ecf_iso = 0.
          ecf_mter = 0.
          ecf_ovoc = 0.
          ecf_n = 0.
        END IF
!**********************************************************************
!**********************************************************************
!  Calculation of Emissions
!**********************************************************************
!**********************************************************************

!       CONVERSION FROM MICROGRAM C/M2/H TO PPM*M/MIN
!       CORRECTION TERM FOR TEMP(K)  AND PRESSURE
!       K = (T/P) *R)/(MW*60)
!       R = 8.3143E-2 m3 mbar/(K mole)

        rat = ta/pa
!     *****************************************************************
!     Isopren:

        coniso = rat*2.3095E-5
        eisoc = aefiso(iland)*ecf_iso
        eiso = coniso*eisoc

!     *****************************************************************
!     Monoterpenes:

        conter = rat*1.1548E-5
        emterc = aefmter(iland)*ecf_mter
        emter = conter*emterc

!     *****************************************************************
!     Other VOCs:

!     as 3-hexenyl-acetate (C=96g/mole)

        conovoc = rat*1.4435E-5
        eovocc = aefovoc(iland)*ecf_ovoc
        eovoc = conovoc*eovocc
!     *****************************************************************
!     SUM OF ALL VOCS

        vocsc = eisoc + emterc + eovocc

!     *****************************************************************
!     Nitrogen:

!       CONVERSION FROM NANOGRAM N/M2/SEC TO PPM*M/MIN
!       CORRECTION TERM FOR TEMP(K)  AND PRESSURE
!       INVENTORY AS N
!       INPUT TO THE MODEL ASSUMED AS NO
!       K = (T/P) *R*60)/(MW*1000)
!       R = 8.3143E-2 m3 mbar/(K mole)

        conn = rat*3.5633E-4
        e_nn = aef_n(iland)*ecf_n
        e_n = conn*e_nn


      END SUBROUTINE biogen
! **********************************************************************
! ***********************  SUBROUTINE BIOSPLIT *************************
! **********************************************************************
      SUBROUTINE biosplit(iland,eiso,emter,eovoc,e_n,emiss_bio,ne_area, &
          vegfrc, &
          config_flags, mminlu,vegflag)
!     THIS PROGRAMM SPLITS THE BIOGENIC EMISSION RATES FOR
!     MONOTERPENES AND OTHER ORGANIC COMPOUNDS INTO THE
!     COMPOUND CLASSES OF THE CHEMISTRY MODEL
!     --- VERSION FOR RADM2 AND RACM CHEMISTRY ---
!***********************************************************************
!     PROGRAM DEVELOPED BY:- WINFRIED SEIDL  (JULY 1997)
!     MODIFIED BY:         - WINFRIED SEIDL  (JULY 1998)
!                            FOR RACM-CHEMISTRY
!                          - WINFRIED SEIDL  (MARCH 2000)
!                            FOR MM5 VERSION 3
!***********************************************************************
!...PROGRAM VARIABLES...
!        ILAND      - Land use category
!        EISO       - Emission von Isopren in ppm*m/min
!        EMTER      - Emission von Monoterpenen in ppm*m/min
!        EOVOC      - Emission sonstiger fluechtiger Kohlenwasserstoffe
!                      in ppm*m/min
!        E_N        - Emission von Stickstoff in ppm*m/min
!***********************************************************************
!...Comments...
!        The split of the monoterpenes and the other VOCs into RADM clas
!        is mostly rather uncertain. Every plant species emitts a differ
!        mix of chemical substances. So e.g. different types of deciduou
!        trees show totally different emissions. By taking the MM5
!        land use categories, the kind of biogenic emissions can be
!        estimated only roughly. Especially for the other VOCs little
!        is known, so the splits presented here have to be regarded as
!        a preliminary assumption.
!        Some literature on this field:
!        Arey et al., J. Geophys. Res. 96D (1991), 9329-9336
!        Arey et al., Atmospheric Environment 25A (1991), 1063-1075
!        Koenig et al., Atmospheric Environment 29 (1995), 861-874
!        Isidorov et al., Atmospheric Environment 19 (1985), 1-8
!        Martin et al., Abstract Air & Waste Management Association''s
!        90th Annual Meeting & Exhibition, Toronto 1997, Paper 97-RP139.
!        Winer et al., Final Report 1983, California Air Resources Bord,
!        Contract No. AO-056-32
!        For the RADM 2 chemistry, most of the monoterpenes are grouped
!        into the OLI class
!        (Middleton et al., Atmospheric Environment 24A (1990), 1107-113
!        with a few exceptions:
!        ISO -- myrcene, ocimene
!        XYL -- p-cymene
!        For the RACM chemistry, the monoterpenes are split
!        between the API, LIM, ISO and XYL classes:
!        API -- a-pinene, b-pinene, D3-carene, sabinene, camphene,
!               1,8-cineole, a-terpineole, thujene
!        LIM -- limonene, terpinene, phellandrene, terpinolene
!        ISO -- myrcene, ocimene
!        XYL -- p-cymene
!        The other VOCs are grouped according to Middleton et al. (1990)
!***********************************************************************
  USE module_configure
  USE module_state_description

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags


! .. Scalar Arguments ..
        REAL :: eiso, emter, eovoc, e_n, vegfrc
        INTEGER :: iland, ne_area
!       INTEGER :: lald, lhc3, lhc5, lhc8, lhcho, liso, lket, lno, &
!         loli, lolt, lora1, lora2, lxyl
! ..
! .. Array Arguments ..
        REAL :: emiss_bio(ne_area)
!       INTEGER :: ixxxlu(nlu)
! ..
! .. Local Scalars ..
        LOGICAL :: vegflag
        CHARACTER (4) :: mminlu
! ..
!     *****************************************************************
!     Correction for vegetation fraction
        IF ((mminlu=='USGS') .AND. (vegflag)) THEN
          eiso = eiso*vegfrc/100.
          emter = emter*vegfrc/100.
          eovoc = eovoc*vegfrc/100.
        END IF

!     *****************************************************************
!     Isoprene and NO

        emiss_bio(liso) = eiso
        emiss_bio(lno) = emiss_bio(lno) + e_n

      if (config_flags%chem_opt == CB05_SORG_AQ_KPP .OR. &
          config_flags%chem_opt == CB05_SORG_VBS_AQ_KPP ) then

      emiss_bio(ltpan) = emter ! Use tpan to represent terpene

!     *****************************************************************
!     Agricultural land

        IF (ixxxlu(iland)==2) THEN
          emiss_bio(lhc5) = emiss_bio(lhc5) + 0.16*eovoc
          emiss_bio(lhc8) = emiss_bio(lhc8) + 0.27*eovoc
          emiss_bio(lolt) = emiss_bio(lolt) + 0.05*eovoc
          emiss_bio(loli) = emiss_bio(loli) + 0.37*eovoc
          emiss_bio(lket) = emiss_bio(lket) + 0.03*eovoc
          emiss_bio(lald) = emiss_bio(lald) + 0.12*eovoc
        END IF

!     *****************************************************************
!     Grassland

        IF (ixxxlu(iland)==3) THEN
          emiss_bio(lhc5) = emiss_bio(lhc5) + 0.09*eovoc
          emiss_bio(lolt) = emiss_bio(lolt) + 0.07*eovoc
          emiss_bio(loli) = emiss_bio(loli) + 0.51*eovoc
          emiss_bio(lket) = emiss_bio(lket) + 0.15*eovoc
          emiss_bio(lald) = emiss_bio(lald) + 0.18*eovoc
        END IF

!     *****************************************************************
!     Deciduous forest

        IF (ixxxlu(iland)==4) THEN
          emiss_bio(lhcho) = emiss_bio(lhcho) + 0.19*eovoc
          emiss_bio(lald) = emiss_bio(lald) + 0.13*eovoc
          emiss_bio(lxyl) = emiss_bio(lxyl) + 0.04*emter
          emiss_bio(lhc5) = emiss_bio(lhc5) + 0.03*eovoc
          emiss_bio(loli) = emiss_bio(loli) + 0.07*eovoc
          emiss_bio(lora1) = emiss_bio(lora1) + 0.23*eovoc
          emiss_bio(lora2) = emiss_bio(lora2) + 0.35*eovoc
        END IF

!     *****************************************************************
!     Coniferous forest and mixed forest


        IF (ixxxlu(iland)==5) THEN
          emiss_bio(lhcho) = emiss_bio(lhcho) + 0.04*eovoc
          emiss_bio(lhcho) = emiss_bio(lhcho) + 0.04*eovoc
          emiss_bio(lald) = emiss_bio(lald) + 0.14*eovoc
          emiss_bio(lhc3) = emiss_bio(lhc3) + 0.07*eovoc
          emiss_bio(lhc5) = emiss_bio(lhc5) + 0.07*eovoc
          emiss_bio(lolt) = emiss_bio(lolt) + 0.07*eovoc
          emiss_bio(loli) = emiss_bio(loli) + 0.50*eovoc
          emiss_bio(lket) = emiss_bio(lket) + 0.03*eovoc
          emiss_bio(lora1) = emiss_bio(lora1) + 0.03*eovoc
          emiss_bio(lora2) = emiss_bio(lora2) + 0.05*eovoc
        END IF

      else
      

!     *****************************************************************
!     Agricultural land

        IF (ixxxlu(iland)==2) THEN
          emiss_bio(loli) = emiss_bio(loli) + 0.80*emter
          emiss_bio(liso) = emiss_bio(liso) + 0.20*emter
          emiss_bio(lhc5) = emiss_bio(lhc5) + 0.16*eovoc
          emiss_bio(lhc8) = emiss_bio(lhc8) + 0.27*eovoc
          emiss_bio(lolt) = emiss_bio(lolt) + 0.05*eovoc
          emiss_bio(loli) = emiss_bio(loli) + 0.37*eovoc
          emiss_bio(lket) = emiss_bio(lket) + 0.03*eovoc
          emiss_bio(lald) = emiss_bio(lald) + 0.12*eovoc
        END IF

!     *****************************************************************
!     Grassland

        IF (ixxxlu(iland)==3) THEN
          emiss_bio(loli) = emiss_bio(loli) + 0.98*emter
          emiss_bio(liso) = emiss_bio(liso) + 0.02*emter
          emiss_bio(lhc5) = emiss_bio(lhc5) + 0.09*eovoc
          emiss_bio(lolt) = emiss_bio(lolt) + 0.07*eovoc
          emiss_bio(loli) = emiss_bio(loli) + 0.51*eovoc
          emiss_bio(lket) = emiss_bio(lket) + 0.15*eovoc
          emiss_bio(lald) = emiss_bio(lald) + 0.18*eovoc
        END IF

!     *****************************************************************
!     Deciduous forest

        IF (ixxxlu(iland)==4) THEN
          emiss_bio(loli) = emiss_bio(loli) + 0.94*emter
          emiss_bio(liso) = emiss_bio(liso) + 0.02*emter
          emiss_bio(lhcho) = emiss_bio(lhcho) + 0.19*eovoc
          emiss_bio(lald) = emiss_bio(lald) + 0.13*eovoc
          emiss_bio(lxyl) = emiss_bio(lxyl) + 0.04*emter
          emiss_bio(lhc5) = emiss_bio(lhc5) + 0.03*eovoc
          emiss_bio(loli) = emiss_bio(loli) + 0.07*eovoc
          emiss_bio(lora1) = emiss_bio(lora1) + 0.23*eovoc
          emiss_bio(lora2) = emiss_bio(lora2) + 0.35*eovoc
        END IF

!     *****************************************************************
!     Coniferous forest and mixed forest


        IF (ixxxlu(iland)==5) THEN
          emiss_bio(loli) = emiss_bio(loli) + 0.85*emter
          emiss_bio(liso) = emiss_bio(liso) + 0.15*emter
          emiss_bio(lhcho) = emiss_bio(lhcho) + 0.04*eovoc
          emiss_bio(lald) = emiss_bio(lald) + 0.14*eovoc
          emiss_bio(lhc3) = emiss_bio(lhc3) + 0.07*eovoc
          emiss_bio(lhc5) = emiss_bio(lhc5) + 0.07*eovoc
          emiss_bio(lolt) = emiss_bio(lolt) + 0.07*eovoc
          emiss_bio(loli) = emiss_bio(loli) + 0.50*eovoc
          emiss_bio(lket) = emiss_bio(lket) + 0.03*eovoc
          emiss_bio(lora1) = emiss_bio(lora1) + 0.03*eovoc
          emiss_bio(lora2) = emiss_bio(lora2) + 0.05*eovoc
        END IF

!     *****************************************************************
!     Tropical forest (not available in SiB and USGS)

        IF ((mminlu=='OLD ') .AND. (iland==12)) THEN
          emiss_bio(loli) = emiss_bio(loli) + emter
        END IF

      end if

      END SUBROUTINE biosplit

    END MODULE module_bioemi_simple
