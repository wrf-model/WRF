!
! CRTM_FASTEM3
!
! Wrapper module for the Fastem model in the CRTM.
! The code is adopted from RTTOV_8_5 Fastem version 3.
!
! Copyright:                                                                    
!    Fastem3 was developed within the context of                          
!    the EUMETSAT Satellite Application Facility on                             
!    Numerical Weather Prediction (NWP SAF), under the                          
!    Cooperation Agreement dated 25 November 1998, between                      
!    EUMETSAT and the Met Office, UK, by one or more partners                   
!    within the NWP SAF. The partners in the NWP SAF are                        
!    the Met Office, ECMWF, KNMI and MeteoFrance.                               
!                                                                               
!    Copyright 2002, EUMETSAT, All Rights Reserved.                             
!                                                                               
! Method:                                                                       
! FASTEM-1 English and Hewison 1998.                                            
! FASTEM-2 Deblonde and English 2001.                                           
! FASTEM-3 English 2003.                                                        
! http://www.metoffice.com/research/interproj/nwpsaf/rtm/evalfastems.pdf        
! Current Code Owner: SAF NWP                                                   
!                                                                               
! History:                                                                      
! Version   Date     Comment                                                    
! -------   ----     -------                                                    
!  1.0       01/12/2002  New F90 code with structures (P Brunel A Smith)        
!  1.1       02/01/2003  Comments added (R Saunders)                            
!  1.2       24/01/2003  error return code changed to array size (P Brunel)     
!                        No more test on negative values for  emissivity input  
!  1.3       26/09/2003  Added polarimetric code and Fastem-3 (S English)       
!
! REFERENCES:
!
!   [1] Hollinger, J. P., Passive microwave measurements of sea surface roughness, IEEE Transactions on
!       Geoscience Electronics, GE-9(3), 165-169, 1971.
!
!   [2] English, S., Estimation of temperature and humidity profile information from microwave radiances
!        over different surface types, J. Appl. Meteorolo., 38, 1526-1541, 1999.
!
!   [3] Liu, Q., and F. Weng, Retrieval of sea surface wind vector from simulated
!         satellite microwave polarimetric measurements, 38, 8078-8085, Radio Science, 2003.
!
!------------------------------------------------------------------------------------------------------------

MODULE CRTM_Fastem3

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,      ONLY: fp
  USE CRTM_Parameters, ONLY: ZERO, ONE, &
                             PI, DEGREES_TO_RADIANS
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Science routines
  PUBLIC :: Fastem3
  PUBLIC :: Fastem3_TL
  PUBLIC :: Fastem3_AD

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_Fastem3.f90 4801 2009-09-01 14:52:31Z quanhua.liu@noaa.gov $'
  ! Literal constants
  REAL(fp), PARAMETER :: HUNDRED = 100.0_fp
  
  
  ! -------------------------
  ! Module variables and data
  ! -------------------------
  INTEGER :: i

  REAL(fp), SAVE, TARGET :: Fastem3_Coef(1:524) 
  REAL(fp), SAVE, TARGET :: Fastem3_Coef_NESDIS(1:524)

! FASTEM Version 8_7
   DATA (Fastem3_Coef(i),i=1,75) / &
   0.175350E+02_fp,-0.617670E+00_fp, 0.894800E-02_fp, 0.318420E+01_fp, 0.191890E-01_fp, &
  -0.108730E-01_fp, 0.258180E-03_fp, 0.683960E+02_fp,-0.406430E+00_fp, 0.228320E-01_fp, &
  -0.530610E-03_fp, 0.476290E+01_fp, 0.154100E+00_fp,-0.337170E-01_fp, 0.844280E-03_fp, &
   0.782870E+02_fp,-0.434630E-02_fp, 0.531250E+01_fp,-0.114770E-01_fp, 0.314160E+01_fp, &
  -0.100000E+01_fp, 0.195000E-04_fp, 0.255000E+01_fp,-0.182390E+01_fp,-0.434790E-02_fp, &
   0.646320E-04_fp, 0.278640E+01_fp, 0.878460E-02_fp,-0.102670E-03_fp,-0.101890E+01_fp, &
  -0.426820E-02_fp, 0.396520E-04_fp, 0.730720E-01_fp, 0.261790E-02_fp,-0.950500E-05_fp, &
   0.295330E-03_fp, 0.443690E-05_fp,-0.140160E-07_fp,-0.717940E-01_fp,-0.267870E-02_fp, &
   0.949560E-05_fp,-0.334690E+00_fp, 0.951660E-02_fp, 0.964400E-05_fp, 0.470780E+00_fp, &
  -0.148970E-01_fp,-0.987460E-05_fp,-0.142750E+00_fp, 0.565380E-02_fp, 0.118850E-05_fp, &
  -0.137840E+00_fp,-0.216950E-02_fp, 0.793130E-05_fp, 0.237840E-04_fp, 0.869500E-06_fp, &
   0.282490E-08_fp, 0.138790E+00_fp, 0.209470E-02_fp,-0.797900E-05_fp,-0.637180E+01_fp, &
   0.253920E-01_fp, 0.357570E-04_fp, 0.942930E+01_fp,-0.332840E-01_fp,-0.647720E-04_fp, &
  -0.329280E+01_fp, 0.965450E-02_fp, 0.281590E-04_fp, 0.252680E+00_fp, 0.343870E-02_fp, &
  -0.156360E-04_fp,-0.156670E-03_fp, 0.139490E-04_fp,-0.407630E-07_fp,-0.141320E+00_fp/


   DATA (Fastem3_Coef(i),i=76,140) / &
  -0.356560E-02_fp, 0.142870E-04_fp,-0.240700E+01_fp,-0.563890E-01_fp, 0.325230E-03_fp, &
   0.296010E+01_fp, 0.704680E-01_fp,-0.426440E-03_fp,-0.751250E+00_fp,-0.191930E-01_fp, &
   0.125940E-03_fp,-0.288250E+00_fp,-0.102650E-02_fp, 0.226700E-05_fp,-0.119070E-02_fp, &
  -0.263170E-04_fp, 0.114600E-06_fp, 0.406300E+00_fp, 0.200030E-02_fp,-0.781640E-05_fp, &
  -0.675700E-01_fp, 0.214600E+00_fp,-0.363000E-02_fp, 0.636730E+01_fp, 0.900610E+00_fp, &
  -0.524880E+00_fp,-0.370920E+01_fp,-0.143310E+01_fp, 0.397450E+00_fp, 0.823100E-01_fp, &
  -0.255980E+00_fp, 0.552000E-02_fp, 0.208000E+01_fp, 0.244920E+01_fp,-0.456420E+00_fp, &
  -0.224900E-01_fp, 0.616900E-01_fp,-0.344000E-02_fp,-0.507570E+01_fp,-0.360670E+01_fp, &
   0.118750E+01_fp, 0.124950E+00_fp, 0.121270E+00_fp, 0.714000E-02_fp, 0.736620E+01_fp, &
  -0.114060E+00_fp,-0.272910E+00_fp,-0.504350E+01_fp,-0.336450E+00_fp, 0.161260E+00_fp, &
  -0.154290E+00_fp,-0.141070E+00_fp,-0.809000E-02_fp, 0.395290E+01_fp, 0.958580E+00_fp, &
  -0.159080E+00_fp, 0.368500E-01_fp, 0.307100E-01_fp, 0.810000E-03_fp,-0.619960E+01_fp, &
  -0.172580E+01_fp, 0.641360E+00_fp, 0.100000E+01_fp, 0.200000E-01_fp, 0.300000E+00_fp/

   DATA (Fastem3_Coef(i),i=141,200) / &
  -5.85336e-05_fp, 0.000141135_fp, 3.41558e-06_fp, 1.63655e-08_fp, & 
   0.000184676_fp,-9.56046e-05_fp, 5.44262e-06_fp,-1.21126e-07_fp, & 
  -4.53125e-05_fp, 1.54844e-05_fp,-8.12972e-07_fp, 1.60754e-08_fp, & 
  -5.90621e-06_fp, 7.21022e-05_fp, 3.31280e-06_fp,-1.16781e-09_fp, & 
   0.000790312_fp,-0.000345218_fp, 1.46029e-05_fp,-3.12496e-07_fp, & 
   3.12823e-05_fp,-1.38377e-05_fp, 2.25909e-08_fp, 2.29783e-09_fp, & 
   5.50691e-05_fp,-0.000106330_fp, 4.53266e-07_fp, 9.09021e-09_fp, & 
   0.000694857_fp,-0.000286702_fp, 9.44863e-06_fp,-2.17880e-07_fp, & 
   6.12423e-05_fp,-3.15418e-05_fp, 1.07982e-06_fp,-2.50954e-08_fp, & 
   2.12483e-05_fp,-5.98084e-06_fp,-6.09132e-07_fp, 1.59695e-08_fp, & 
  -0.000613516_fp, 0.000263937_fp,-7.62252e-06_fp, 1.80749e-07_fp, & 
   3.39556e-06_fp,-2.35102e-06_fp, 4.89815e-07_fp,-1.39383e-08_fp, & 
  -4.08839e-05_fp, 0.000115903_fp, 1.33087e-05_fp,-2.32691e-07_fp, & 
   0.000212243_fp,-0.000106434_fp, 4.65887e-06_fp,-4.61192e-08_fp, & 
  -6.21221e-05_fp, 1.77511e-05_fp, 7.99048e-08_fp,-5.76266e-09_fp/   

   DATA (Fastem3_Coef(i),i=201,260) / &
   3.81961e-05_fp, 5.85374e-05_fp, 7.56621e-06_fp,-1.10985e-07_fp, &
    0.00129621_fp,-0.000554118_fp, 1.73120e-05_fp,-2.76256e-07_fp, &
   5.15273e-05_fp,-2.07117e-05_fp, 4.87256e-08_fp,-2.35559e-09_fp, &
   0.000152246_fp,-0.000159825_fp,-5.00807e-06_fp, 1.26266e-07_fp, &
    0.00102881_fp,-0.000414324_fp, 9.23915e-06_fp,-1.50247e-07_fp, &
   8.88053e-05_fp,-3.92334e-05_fp,-6.88354e-07_fp, 2.93177e-08_fp, &
   5.04310e-05_fp,-1.91818e-05_fp, 4.90998e-07_fp,-1.58696e-08_fp, &
  -0.000615485_fp, 0.000257073_fp,-4.67360e-06_fp, 6.76351e-08_fp, &
   2.47840e-06_fp,-1.54153e-06_fp, 3.33460e-07_fp,-7.84914e-09_fp, &
  -6.21877e-05_fp, 0.000124143_fp, 1.70023e-05_fp,-3.68643e-07_fp, &
   0.000101425_fp,-6.30114e-05_fp, 4.35736e-06_fp,-1.01644e-07_fp, &
  -7.96174e-05_fp, 2.65038e-05_fp, 5.37454e-08_fp,-1.45468e-08_fp, &
   4.42053e-05_fp, 4.59572e-05_fp, 9.71810e-06_fp,-1.70817e-07_fp, &
    0.00133357_fp,-0.000557281_fp, 1.42888e-05_fp,-1.71095e-07_fp, &
   5.31728e-05_fp,-2.17787e-05_fp, 2.45581e-07_fp,-1.01500e-08_fp/

   DATA (Fastem3_Coef(i),i=261,320) / &
   0.000115092_fp,-0.000140989_fp,-1.03311e-05_fp, 2.55829e-07_fp, &
    0.00109355_fp,-0.000439655_fp, 8.47483e-06_fp,-1.00246e-07_fp, &
   0.000148653_fp,-6.29077e-05_fp, 1.14331e-06_fp,-2.15387e-08_fp, &
   1.32775e-05_fp,-4.18720e-06_fp,-1.05548e-06_fp, 2.89720e-08_fp, &
  -0.000572372_fp, 0.000234306_fp,-2.64171e-06_fp, 3.48850e-09_fp, &
   1.55316e-05_fp,-8.23120e-06_fp, 1.06889e-06_fp,-2.98319e-08_fp, &
   8.63755e-06_fp, 5.95888e-05_fp, 2.54421e-05_fp,-4.13468e-07_fp, &
   0.000227688_fp,-0.000113986_fp, 7.25093e-06_fp,-1.27069e-07_fp, &
  -3.37521e-05_fp, 4.37196e-06_fp, 2.56526e-06_fp,-7.49534e-08_fp, &
   7.14562e-05_fp, 2.01237e-05_fp, 1.38150e-05_fp,-1.88276e-07_fp, &
    0.00130476_fp,-0.000520253_fp, 4.63495e-06_fp, 3.20702e-08_fp, &
   3.91178e-05_fp,-1.55648e-05_fp,-4.61218e-07_fp,-3.61295e-09_fp, &
   0.000111352_fp,-0.000122809_fp,-1.86779e-05_fp, 3.25278e-07_fp, &
    0.00100263_fp,-0.000378765_fp, 2.46420e-07_fp, 3.17558e-08_fp, &
   0.000127648_fp,-4.98883e-05_fp,-8.67037e-07_fp, 1.47253e-08_fp/

   DATA (Fastem3_Coef(i),i=321,380) / &
    1.07976e-05_fp,-3.82161e-06_fp,-9.49457e-07_fp, 1.58475e-08_fp, &
   -0.000478420_fp, 0.000182279_fp, 8.81766e-07_fp,-3.59735e-08_fp, &
    1.86481e-05_fp,-5.47143e-06_fp, 4.98428e-07_fp,-8.26455e-09_fp, &
   -5.85336e-05_fp, 0.000141135_fp, 3.41558e-06_fp, 1.63655e-08_fp, &
    0.000184676_fp,-9.56046e-05_fp, 5.44262e-06_fp,-1.21126e-07_fp, &
   -4.53125e-05_fp, 1.54844e-05_fp,-8.12972e-07_fp, 1.60754e-08_fp, &
   -5.90621e-06_fp, 7.21022e-05_fp, 3.31280e-06_fp,-1.16781e-09_fp, &
    0.000790312_fp,-0.000345218_fp, 1.46029e-05_fp,-3.12496e-07_fp, &
    3.12823e-05_fp,-1.38377e-05_fp, 2.25909e-08_fp, 2.29783e-09_fp, &
    5.50691e-05_fp,-0.000106330_fp, 4.53266e-07_fp, 9.09021e-09_fp, &
    0.000694857_fp,-0.000286702_fp, 9.44863e-06_fp,-2.17880e-07_fp, &
    6.12423e-05_fp,-3.15418e-05_fp, 1.07982e-06_fp,-2.50954e-08_fp, &
    2.12483e-05_fp,-5.98084e-06_fp,-6.09132e-07_fp, 1.59695e-08_fp, &
   -0.000613516_fp, 0.000263937_fp,-7.62252e-06_fp, 1.80749e-07_fp, &
    3.39556e-06_fp,-2.35102e-06_fp, 4.89815e-07_fp,-1.39383e-08_fp/

   DATA (Fastem3_Coef(i),i=381,440) / &
  -4.08839e-05_fp, 0.000115903_fp, 1.33087e-05_fp,-2.32691e-07_fp, &
   0.000212243_fp,-0.000106434_fp, 4.65887e-06_fp,-4.61192e-08_fp, &
  -6.21221e-05_fp, 1.77511e-05_fp, 7.99048e-08_fp,-5.76266e-09_fp, &
   3.81961e-05_fp, 5.85374e-05_fp, 7.56621e-06_fp,-1.10985e-07_fp, &
    0.00129621_fp,-0.000554118_fp, 1.73120e-05_fp,-2.76256e-07_fp, &
   5.15273e-05_fp,-2.07117e-05_fp, 4.87256e-08_fp,-2.35559e-09_fp, &
   0.000152246_fp,-0.000159825_fp,-5.00807e-06_fp, 1.26266e-07_fp, &
    0.00102881_fp,-0.000414324_fp, 9.23915e-06_fp,-1.50247e-07_fp, &
   8.88053e-05_fp,-3.92334e-05_fp,-6.88354e-07_fp, 2.93177e-08_fp, &
   5.04310e-05_fp,-1.91818e-05_fp, 4.90998e-07_fp,-1.58696e-08_fp, &
  -0.000615485_fp, 0.000257073_fp,-4.67360e-06_fp, 6.76351e-08_fp, &
   2.47840e-06_fp,-1.54153e-06_fp, 3.33460e-07_fp,-7.84914e-09_fp, &
  -6.21877e-05_fp, 0.000124143_fp, 1.70023e-05_fp,-3.68643e-07_fp, &
   0.000101425_fp,-6.30114e-05_fp, 4.35736e-06_fp,-1.01644e-07_fp, &
  -7.96174e-05_fp, 2.65038e-05_fp, 5.37454e-08_fp,-1.45468e-08_fp/

   DATA (Fastem3_Coef(i),i=441,524) / &
   4.42053e-05_fp, 4.59572e-05_fp, 9.71810e-06_fp,-1.70817e-07_fp, &
    0.00133357_fp,-0.000557281_fp, 1.42888e-05_fp,-1.71095e-07_fp, &
   5.31728e-05_fp,-2.17787e-05_fp, 2.45581e-07_fp,-1.01500e-08_fp, &
   0.000115092_fp,-0.000140989_fp,-1.03311e-05_fp, 2.55829e-07_fp, &
    0.00109355_fp,-0.000439655_fp, 8.47483e-06_fp,-1.00246e-07_fp, &
   0.000148653_fp,-6.29077e-05_fp, 1.14331e-06_fp,-2.15387e-08_fp, &
   1.32775e-05_fp,-4.18720e-06_fp,-1.05548e-06_fp, 2.89720e-08_fp, &
  -0.000572372_fp, 0.000234306_fp,-2.64171e-06_fp, 3.48850e-09_fp, &
   1.55316e-05_fp,-8.23120e-06_fp, 1.06889e-06_fp,-2.98319e-08_fp, &
   8.63755e-06_fp, 5.95888e-05_fp, 2.54421e-05_fp,-4.13468e-07_fp, &
   0.000227688_fp,-0.000113986_fp, 7.25093e-06_fp,-1.27069e-07_fp, &
  -3.37521e-05_fp, 4.37196e-06_fp, 2.56526e-06_fp,-7.49534e-08_fp, &
   7.14562e-05_fp, 2.01237e-05_fp, 1.38150e-05_fp,-1.88276e-07_fp, &
    0.00130476_fp,-0.000520253_fp, 4.63495e-06_fp, 3.20702e-08_fp, &
   3.91178e-05_fp,-1.55648e-05_fp,-4.61218e-07_fp,-3.61295e-09_fp, &
   0.000111352_fp,-0.000122809_fp,-1.86779e-05_fp, 3.25278e-07_fp, &
    0.00100263_fp,-0.000378765_fp, 2.46420e-07_fp, 3.17558e-08_fp, &
   0.000127648_fp,-4.98883e-05_fp,-8.67037e-07_fp, 1.47253e-08_fp, &
   1.07976e-05_fp,-3.82161e-06_fp,-9.49457e-07_fp, 1.58475e-08_fp, &
  -0.000478420_fp, 0.000182279_fp, 8.81766e-07_fp,-3.59735e-08_fp, &
   1.86481e-05_fp,-5.47143e-06_fp, 4.98428e-07_fp,-8.26455e-09_fp/

! FASTEM Version 8_5

   DATA (Fastem3_Coef_NESDIS(i),i=1,75) / &
   0.175350E+02_fp,-0.617670E+00_fp, 0.894800E-02_fp, 0.318420E+01_fp, 0.191890E-01_fp, &
  -0.108730E-01_fp, 0.258180E-03_fp, 0.683960E+02_fp,-0.406430E+00_fp, 0.228320E-01_fp, &
  -0.530610E-03_fp, 0.476290E+01_fp, 0.154100E+00_fp,-0.337170E-01_fp, 0.844280E-03_fp, &
   0.782870E+02_fp,-0.434630E-02_fp, 0.531250E+01_fp,-0.114770E-01_fp, 0.314160E+01_fp, &
  -0.100000E+01_fp, 0.195000E-04_fp, 0.255000E+01_fp,-0.182390E+01_fp,-0.434790E-02_fp, &
   0.646320E-04_fp, 0.278640E+01_fp, 0.878460E-02_fp,-0.102670E-03_fp,-0.101890E+01_fp, &
  -0.426820E-02_fp, 0.396520E-04_fp, 0.730720E-01_fp, 0.261790E-02_fp,-0.950500E-05_fp, &
   0.295330E-03_fp, 0.443690E-05_fp,-0.140160E-07_fp,-0.717940E-01_fp,-0.267870E-02_fp, &
   0.949560E-05_fp,-0.334690E+00_fp, 0.951660E-02_fp, 0.964400E-05_fp, 0.470780E+00_fp, &
  -0.148970E-01_fp,-0.987460E-05_fp,-0.142750E+00_fp, 0.565380E-02_fp, 0.118850E-05_fp, &
  -0.137840E+00_fp,-0.216950E-02_fp, 0.793130E-05_fp, 0.237840E-04_fp, 0.869500E-06_fp, &
   0.282490E-08_fp, 0.138790E+00_fp, 0.209470E-02_fp,-0.797900E-05_fp,-0.637180E+01_fp, &
   0.253920E-01_fp, 0.357570E-04_fp, 0.942930E+01_fp,-0.332840E-01_fp,-0.647720E-04_fp, &
  -0.329280E+01_fp, 0.965450E-02_fp, 0.281590E-04_fp, 0.252680E+00_fp, 0.343870E-02_fp, &
  -0.156360E-04_fp,-0.156670E-03_fp, 0.139490E-04_fp,-0.407630E-07_fp,-0.141320E+00_fp/


   DATA (Fastem3_Coef_NESDIS(i),i=76,140) / &
  -0.356560E-02_fp, 0.142870E-04_fp,-0.240700E+01_fp,-0.563890E-01_fp, 0.325230E-03_fp, &
   0.296010E+01_fp, 0.704680E-01_fp,-0.426440E-03_fp,-0.751250E+00_fp,-0.191930E-01_fp, &
   0.125940E-03_fp,-0.288250E+00_fp,-0.102650E-02_fp, 0.226700E-05_fp,-0.119070E-02_fp, &
  -0.263170E-04_fp, 0.114600E-06_fp, 0.406300E+00_fp, 0.200030E-02_fp,-0.781640E-05_fp, &
  -0.675700E-01_fp, 0.214600E+00_fp,-0.363000E-02_fp, 0.636730E+01_fp, 0.900610E+00_fp, &
  -0.524880E+00_fp,-0.370920E+01_fp,-0.143310E+01_fp, 0.397450E+00_fp, 0.823100E-01_fp, &
  -0.255980E+00_fp, 0.552000E-02_fp, 0.208000E+01_fp, 0.244920E+01_fp,-0.456420E+00_fp, &
  -0.224900E-01_fp, 0.616900E-01_fp,-0.344000E-02_fp,-0.507570E+01_fp,-0.360670E+01_fp, &
   0.118750E+01_fp, 0.124950E+00_fp, 0.121270E+00_fp, 0.714000E-02_fp, 0.736620E+01_fp, &
  -0.114060E+00_fp,-0.272910E+00_fp,-0.504350E+01_fp,-0.336450E+00_fp, 0.161260E+00_fp, &
  -0.154290E+00_fp,-0.141070E+00_fp,-0.809000E-02_fp, 0.395290E+01_fp, 0.958580E+00_fp, &
  -0.159080E+00_fp, 0.368500E-01_fp, 0.307100E-01_fp, 0.810000E-03_fp,-0.619960E+01_fp, &
  -0.172580E+01_fp, 0.641360E+00_fp, 0.100000E+01_fp, 0.200000E-01_fp, 0.300000E+00_fp/

   DATA (Fastem3_Coef_NESDIS(i),i=141,200) / &
   0.0000000E+00_fp, 9.6367119E-04_fp,-8.6629021E-05_fp, 3.5241480E-06_fp, &
   0.0000000E+00_fp, 3.5252990E-04_fp,-5.2672411E-05_fp, 2.0316800E-06_fp, &
   0.0000000E+00_fp, 2.2038745E-04_fp,-2.4462388E-05_fp, 8.5336325E-07_fp, &
  -3.3152331E-19_fp, 2.3004825E-06_fp, 1.0002010E-05_fp,-1.3208830E-07_fp, &
   2.1486068E-18_fp,-1.4900997E-03_fp, 1.4101672E-04_fp,-3.8482417E-06_fp, &
  -3.3571981E-20_fp,-1.8981404E-05_fp,-1.1606203E-07_fp, 1.4367055E-07_fp, &
  -2.1486068E-18_fp,-7.0646871E-04_fp, 5.1532206E-05_fp,-2.2805100E-06_fp, &
   0.0000000E+00_fp,-1.7889790E-03_fp, 1.8547164E-04_fp,-5.5604542E-06_fp, &
   5.3715170E-19_fp,-2.5556661E-04_fp, 2.5583146E-05_fp,-7.1436358E-07_fp, &
   2.6857585E-19_fp,-7.4668518E-05_fp, 7.4670102E-06_fp,-2.1484851E-07_fp, &
  -1.0743034E-18_fp, 5.3075602E-04_fp,-4.5194156E-05_fp, 1.1187092E-06_fp, &
  -6.7143963E-20_fp,-2.7395514E-05_fp, 3.4097948E-06_fp,-1.1315773E-07_fp, &
  -2.1486068E-18_fp, 1.0461950E-03_fp,-1.0481702E-04_fp, 4.2781035E-06_fp, &
  -5.3715170E-19_fp, 1.7721432E-04_fp,-2.8288761E-05_fp, 1.1141899E-06_fp, &
  -5.3715170E-19_fp, 2.1553210E-04_fp,-2.3157652E-05_fp, 6.7211363E-07_fp/

   DATA (Fastem3_Coef_NESDIS(i),i=201,260) / &
  -1.3428793E-19_fp, 8.5177599E-05_fp,-2.7158806E-06_fp, 5.3669021E-07_fp, &
   0.0000000E+00_fp,-1.6235097E-03_fp, 1.7159861E-04_fp,-5.2640794E-06_fp, &
   1.3428793E-19_fp,-3.3005555E-05_fp, 5.3371377E-06_fp,-2.6520649E-07_fp, &
  -1.0743034E-18_fp,-6.2686688E-04_fp, 5.2948293E-05_fp,-2.0691875E-06_fp, &
   0.0000000E+00_fp,-1.8096643E-03_fp, 1.9881855E-04_fp,-6.3007792E-06_fp, &
  -5.3715170E-19_fp,-2.6226207E-04_fp, 3.0438592E-05_fp,-1.0121776E-06_fp, &
  -1.3428793E-19_fp,-8.2037135E-05_fp, 8.6260243E-06_fp,-2.6006015E-07_fp, &
   0.0000000E+00_fp, 8.1354310E-04_fp,-7.9703750E-05_fp, 2.2798401E-06_fp, &
   0.0000000E+00_fp,-1.5302876E-05_fp, 2.2375450E-06_fp,-8.2889947E-08_fp, &
  -4.2972136E-18_fp, 8.8842469E-04_fp,-9.0366309E-05_fp, 3.6307895E-06_fp, &
   2.6857585E-19_fp,-1.1291779E-04_fp, 1.5071875E-05_fp,-5.8729313E-07_fp, &
  -2.6857585E-19_fp, 1.2378076E-04_fp,-1.5059099E-05_fp, 5.1323855E-07_fp, &
  -2.6857585E-19_fp, 9.3039009E-05_fp,-4.6806563E-06_fp, 5.2923480E-07_fp, &
   0.0000000E+00_fp,-1.4273858E-03_fp, 1.6772017E-04_fp,-5.6148806E-06_fp, &
   1.3428793E-19_fp,-6.6658904E-05_fp, 7.5730845E-06_fp,-2.5306989E-07_fp/

   DATA (Fastem3_Coef_NESDIS(i),i=261_fp,320) / &
   1.0743034E-18_fp,-4.6949918E-04_fp, 4.3699492E-05_fp,-1.6267036E-06_fp, &
  -2.1486068E-18_fp,-1.2050214E-03_fp, 1.3262848E-04_fp,-4.1836370E-06_fp, &
   2.6857585E-19_fp,-1.7871779E-04_fp, 2.0996566E-05_fp,-6.9040675E-07_fp, &
  -1.3428793E-19_fp,-6.5443186E-05_fp, 6.9365788E-06_fp,-2.1015163E-07_fp, &
  -2.1486068E-18_fp, 8.6167530E-04_fp,-9.0416106E-05_fp, 2.7453514E-06_fp, &
  -4.1964977E-21_fp, 7.9797638E-07_fp, 2.1066239E-07_fp,-1.3731659E-08_fp, &
   2.1486068E-18_fp, 4.7592065E-04_fp,-4.1933003E-05_fp, 1.7061523E-06_fp, &
  -2.6857585E-19_fp,-1.4941466E-04_fp, 2.0847674E-05_fp,-7.7670478E-07_fp, &
   6.7143963E-20_fp, 4.5608740E-05_fp,-5.2874129E-06_fp, 1.7786310E-07_fp, &
  -1.3428793E-19_fp, 8.8924979E-05_fp,-4.5037646E-06_fp, 4.2915158E-07_fp, &
   2.1486068E-18_fp,-1.0042187E-03_fp, 1.2018813E-04_fp,-4.0317241E-06_fp, &
   0.0000000E+00_fp,-5.4802953E-05_fp, 5.9069498E-06_fp,-1.7767948E-07_fp, &
   0.0000000E+00_fp,-3.2320089E-04_fp, 2.9299059E-05_fp,-1.0298019E-06_fp, &
   0.0000000E+00_fp,-9.6928241E-04_fp, 1.0809712E-04_fp,-3.4336240E-06_fp, &
   2.6857585E-19_fp,-1.1435949E-04_fp, 1.3661255E-05_fp,-4.5356333E-07_fp/

   DATA (Fastem3_Coef_NESDIS(i),i=321,380) / &
   0.0000000E+00_fp,-5.5538603E-05_fp, 5.9531321E-06_fp,-1.8146621E-07_fp, &
   1.0743034E-18_fp, 7.7915547E-04_fp,-8.4307081E-05_fp, 2.6227408E-06_fp, &
   4.1964977E-21_fp, 1.5456346E-06_fp, 2.3738908E-08_fp,-5.5841576E-09_fp, &
  -2.1486068E-18_fp, 9.5922314E-04_fp,-7.0579918E-05_fp, 3.4287407E-06_fp, &
   0.0000000E+00_fp,-3.6054554E-03_fp, 3.7707775E-04_fp,-1.1455817E-05_fp, &
   4.0286376E-19_fp, 8.9993380E-05_fp,-1.2344805E-05_fp, 4.2902698E-07_fp, &
   4.2972136E-18_fp, 2.0437825E-03_fp,-1.4944069E-04_fp, 7.6004808E-06_fp, &
   2.1486068E-18_fp,-1.5425570E-03_fp, 1.5629345E-04_fp,-4.8887769E-06_fp, &
   1.0743034E-18_fp, 2.0045871E-04_fp,-3.5661302E-05_fp, 1.4191859E-06_fp, &
   5.3715170E-19_fp,-2.8867502E-04_fp, 3.7523914E-05_fp,-5.9417414E-07_fp, &
  -8.5944272E-18_fp, 3.4857739E-03_fp,-3.6124646E-04_fp, 1.0726928E-05_fp, &
  -6.7143963E-20_fp, 4.2254247E-05_fp,-8.5833926E-06_fp, 2.9045614E-07_fp, &
   2.6857585E-19_fp, 1.1665349E-04_fp,-1.0411461E-05_fp, 2.5978341E-07_fp, &
   0.0000000E+00_fp,-7.0890581E-04_fp, 6.0394337E-05_fp,-1.5117034E-06_fp, &
  -1.3428793E-19_fp, 4.0762196E-05_fp,-3.3305901E-06_fp, 7.6333542E-08_fp/

   DATA (Fastem3_Coef_NESDIS(i),i=381,440) / &
   0.0000000E+00_fp, 1.2638206E-03_fp,-9.2418246E-05_fp, 4.2094848E-06_fp, &
   0.0000000E+00_fp,-3.6313292E-03_fp, 4.0482081E-04_fp,-1.2888126E-05_fp, &
   0.0000000E+00_fp, 1.1470204E-04_fp,-1.4243233E-05_fp, 5.3426191E-07_fp, &
   4.2972136E-18_fp, 1.8283002E-03_fp,-1.4125633E-04_fp, 6.4938622E-06_fp, &
  -8.5944272E-18_fp,-2.8090945E-03_fp, 3.3690213E-04_fp,-1.1361085E-05_fp, &
  -1.3428792E-18_fp, 1.1620186E-04_fp,-2.4893927E-05_fp, 1.1141415E-06_fp, &
   0.0000000E+00_fp,-4.0809700E-04_fp, 4.2275362E-05_fp,-1.1897024E-06_fp, &
   4.2972136E-18_fp, 2.1905091E-03_fp,-2.3178745E-04_fp, 7.0958445E-06_fp, &
  -6.7143963E-20_fp,-4.1995667E-05_fp,-3.5756034E-06_fp, 2.9889267E-07_fp, &
  -2.6857585E-19_fp, 1.4311979E-04_fp,-1.3321765E-05_fp, 3.5014591E-07_fp, &
   0.0000000E+00_fp,-9.7198994E-04_fp, 9.1230191E-05_fp,-2.5189418E-06_fp, &
   1.3428793E-19_fp, 5.5606695E-05_fp,-4.7709354E-06_fp, 1.1805540E-07_fp, &
   0.0000000E+00_fp, 1.4754632E-03_fp,-1.1259706E-04_fp, 5.0381686E-06_fp, &
   0.0000000E+00_fp,-2.5219414E-03_fp, 2.9103569E-04_fp,-9.4800143E-06_fp, &
   2.6857585E-19_fp, 1.3551887E-04_fp,-1.4374317E-05_fp, 4.1168889E-07_fp/

   DATA (Fastem3_Coef_NESDIS(i),i=441,524) / &
   0.0000000E+00_fp, 1.4272348E-03_fp,-1.0901905E-04_fp, 5.0143981E-06_fp, &
  -8.5944272E-18_fp,-2.5480080E-03_fp, 3.1332066E-04_fp,-1.0652217E-05_fp, &
   8.0572753E-19_fp, 1.1005379E-04_fp,-2.0928974E-05_fp, 8.5007872E-07_fp, &
  -1.0743034E-18_fp,-4.8486964E-04_fp, 3.9792045E-05_fp,-1.2952516E-06_fp, &
   1.0743034E-18_fp, 6.6632900E-04_fp,-6.4228952E-05_fp, 1.8348775E-06_fp, &
  -1.3428793E-19_fp,-5.3845830E-05_fp,-2.5095460E-06_fp, 2.7416090E-07_fp, &
  -2.6857585E-19_fp, 1.3403864E-04_fp,-1.2231048E-05_fp, 3.1954929E-07_fp, &
  -2.1486068E-18_fp,-9.7465812E-04_fp, 9.5752745E-05_fp,-2.7626340E-06_fp, &
   0.0000000E+00_fp, 5.1830575E-05_fp,-3.9670404E-06_fp, 8.7025597E-08_fp, &
  -2.1486068E-18_fp, 1.4252769E-03_fp,-1.1444394E-04_fp, 4.9676260E-06_fp, &
   0.0000000E+00_fp,-1.4356287E-03_fp, 1.7286405E-04_fp,-5.8191267E-06_fp, &
  -2.6857585E-19_fp, 1.1677676E-04_fp,-1.2273499E-05_fp, 3.2656803E-07_fp, &
   2.1486068E-18_fp, 1.1046909E-03_fp,-8.4853076E-05_fp, 3.8606386E-06_fp, &
   0.0000000E+00_fp,-1.6349442E-03_fp, 2.0932616E-04_fp,-7.3307024E-06_fp, &
  -6.7143963E-20_fp, 2.7486552E-05_fp,-7.8327766E-06_fp, 3.2908164E-07_fp, &
   1.0743034E-18_fp,-4.8348849E-04_fp, 4.0686133E-05_fp,-1.4441447E-06_fp, &
   2.6857585E-19_fp, 1.8759405E-04_fp,-1.3405025E-05_fp, 2.7262971E-07_fp, &
  -1.3428793E-19_fp,-7.7096702E-05_fp, 2.2303739E-06_fp, 9.0923407E-08_fp, &
   2.6857585E-19_fp, 1.1571196E-04_fp,-1.0632459E-05_fp, 2.8094118E-07_fp, &
   2.1486068E-18_fp,-8.5648254E-04_fp, 8.6417691E-05_fp,-2.5496240E-06_fp, &
   6.7143963E-20_fp, 4.8174403E-05_fp,-3.7887589E-06_fp, 8.6373880E-08_fp/

CONTAINS


!------------------------------------------------------------------------------------------------------------

  SUBROUTINE Fastem3(Frequency,         & ! INPUT
                     Sat_Zenith_Angle,  & ! INPUT
                     Sat_Azimuth_Angle, & ! INPUT in degree
                     SST,               & ! INPUT
                     Wind_Speed,        & ! INPUT
                     Wind_Direction,    & ! INPUT in degree
                     Transmittance,     & ! INPUT
                     Fastem_Version,    & ! INPUT
                     Emissivity,        & ! OUTPUT
                     Reflectivity)        ! OUTPUT
! ---------------------------------------------------------------------------------------------------
!
  INTEGER, INTENT( IN ) :: Fastem_Version
  REAL(fp), INTENT( IN ) ::  Frequency, Sat_Zenith_Angle, Sat_Azimuth_Angle
  REAL(fp), INTENT( IN ) ::  SST, Wind_Speed, Transmittance, Wind_Direction
  REAL(fp), INTENT( OUT ) :: Emissivity(:), Reflectivity(:)


  ! local variables

  !local constants:
  REAL(fp), Parameter :: freqfixed(4) = Reshape( &
       & (/ 7.0_fp, 10.0_fp, 19.0_fp, 37.0_fp /), (/4/) )

  !local variables:
  REAL(fp) :: tcelsius,coszen,coszen_sq,sinzen,seczen,seczen_sq
  REAL(fp) :: tcelsius_sq
  REAL(fp) :: tcelsius_cu
  REAL(fp) :: einf                  ! Debye parameter Epsilon infinity
  REAL(fp) :: fen,fen_sq            ! intermediate Debye variable
  REAL(fp) :: del1,del2             ! intermediate Debye variable
  REAL(fp) :: den1,den2             ! intermediate Debye variable
  REAL(fp) :: f1,f2                 ! intermediate Debye variable
  REAL(fp) :: perm_free             ! permittivity (space)
  REAL(fp) :: sigma                 ! saline water conductivity
  REAL(fp) :: perm_real1,perm_real2 ! permittivity (real part)
  REAL(fp) :: perm_imag1,perm_imag2,perm_imag3 !    .... imaginary part
  REAL(fp) :: perm_Real,perm_imag   ! permittivity (real, imaginary part)
  REAL(fp) :: perm_static           ! static land permittivity
  REAL(fp) :: perm_infinite         ! infinite frequency land permittivity
  REAL(fp) :: freq_ghz,freq_ghz_sq  ! frequency in GHz , and squared
  REAL(fp) :: fresnel_v_Real,fresnel_v_imag
  REAL(fp) :: fresnel_h_Real,fresnel_h_imag
  REAL(fp) :: fresnel_v,fresnel_h
  REAL(fp) :: small_rough_cor,foam_cor
  REAL(fp) :: large_rough_cor(2)
  REAL(fp) :: small_rough,large_rough ! small and large scale roughness
  REAL(fp) :: variance,varm
  REAL(fp) :: wind10
  REAL(fp) :: wind10_sq,windsec, windratio
  REAL(fp) :: wind10_direction, windangle ! Note wind azimuth is in radians
  REAL(fp) :: opdpsfc,freqr
  REAL(fp) :: zrough_v,zrough_h
  REAL(fp) :: zreflmod_v,zreflmod_h
  REAL(fp) :: delta,delta2
  REAL(fp) :: qdepol,emissfactor
  REAL(fp) :: emissfactor_v,emissfactor_h
  REAL(fp) :: zc(12)    ! large scale correction
  REAL(fp) :: zx(9)     ! effective path coefficients
  REAL(fp) :: azimuthal_emiss,u19,phi,dfreq
  REAL(fp) :: tbfixed(4,4,3)   ! Surface brightness temperature azimuthal variation terms for 37, 19, 10, 7 GHz
  REAL(fp) :: efixed(4,4,3)     ! Emissivity azimuthal variation terms for 7, 10, 19, 37 GHz
  REAL(fp) :: einterpolated(4,3)   ! Emissivity azimuthal variation terms for interpolated to required frequency
  REAL(fp) :: a1e,a2e,a3e    ! coefficients used in azimuthal emissivity model
  REAL(fp), Pointer :: c(:)   ! pointer to FASTEM coefs
  COMPLEX(fp) :: perm1,perm2  ! permittivity
  COMPLEX(fp) :: rhth,rvth    ! Fresnel reflectivity complex variables
  COMPLEX(fp) :: permittivity ! permittivity
  Integer :: i,j,chan,istokes,ifreq,m,jcof,jcofm1
  Integer :: i_freq,j_stokes,ich,ichannel   ! indices used in azimuthal emissivity model
  ! == pol_id +1
  !   1 average of vertical and horizontal
  !   2 nominal vertical at nadir, rotating
  !      with view angle
  !   3 nominal horizontal at nadir, rotating
  !      with view angle
  !   4 vertical
  !   5 horizontal
  !   6 vertical and horizontal
  !   7 full stokes vector
  !- End of header --------------------------------------------------------

    sinzen = SIN( Sat_Zenith_Angle * DEGREES_TO_RADIANS )                                   
    coszen = COS( Sat_Zenith_Angle * DEGREES_TO_RADIANS )                                   
    coszen_sq = coszen ** 2                                                                 
    seczen = ONE / coszen                                                           
    seczen_sq = seczen ** 2                                                                 
    c => Fastem3_Coef                                                                       

    wind10    = Wind_Speed                                                                  
    wind10_sq = wind10 ** 2                                                                 
    wind10_direction = Wind_Direction                                                       
    windsec          = wind10 * seczen                                                      

    !Set values for temperature polynomials (convert from kelvin to celsius)                
    tcelsius = SST - 273.15_fp                                                         
    tcelsius_sq = tcelsius * tcelsius     !quadratic                                        
    tcelsius_cu = tcelsius_sq * tcelsius  !cubic                                            

    !Define two relaxation frequencies, f1 and f2                                           
    f1 = c(1) + c(2) * tcelsius + c(3) * tcelsius_sq                                        
    f2 = c(4) + c(5) * tcelsius + c(6) * tcelsius_sq + c(7) * tcelsius_cu                   

    !Static permittivity estatic = del1+del2+einf                                           
    del1 = c(8)  + c(9)  * tcelsius + c(10) * tcelsius_sq + c(11) * tcelsius_cu             
    del2 = c(12) + c(13) * tcelsius + c(14) * tcelsius_sq + c(15) * tcelsius_cu             
    einf = c(18) + c(19) * tcelsius                                                         


    freq_ghz    = Frequency                                                                 
    freq_ghz_sq = freq_ghz * freq_ghz                                                       

    !-----------------------------------------------------                                  
    !1.2 calculate permittivity using double-debye formula                                  
    !-----------------------------------------------------                                  

    fen          = 2.0_fp * c(20) * freq_ghz * 0.001_fp                           
    fen_sq       = fen*fen                                                                  
    den1         = ONE + fen_sq * f1 * f1                                           
    den2         = ONE + fen_sq * f2 * f2                                           
    perm_real1   = del1 / den1                                                              
    perm_real2   = del2 / den2                                                              
    perm_imag1   = del1 * fen * f1 / den1                                                   
    perm_imag2   = del2 * fen * f2 / den2                                                   
    ! perm_free = 8.854E-3_fp not 8.854E-12 as multiplied by 1E9 for GHz               
    perm_free    = 8.854E-3_fp                                                         
    sigma        = 2.906_fp + 0.09437_fp * tcelsius                               
    perm_imag3   = sigma / (2.0_fp * c(20) * perm_free * freq_ghz)                     
    perm_Real    = perm_real1 + perm_real2 + einf                                           
    perm_imag    = perm_imag1 + perm_imag2 + perm_imag3                                     
    permittivity = Cmplx(perm_Real,perm_imag,fp)                                       


    !-------------------------------------------------------------                          
    !1.3 calculate complex reflection coefficients and corrections                          
    !-------------------------------------------------------------                          


    !1.3.1) Fresnel reflection coefficients                                                 
    !------                                                                                 

    perm1          = sqrt(permittivity - sinzen**2)                                         
    perm2          = permittivity * coszen                                                  
    rhth           = (coszen-perm1) / (coszen+perm1)                                        
    rvth           = (perm2-perm1) / (perm2+perm1)                                          
    fresnel_v_Real = Dble(rvth)                                                             
    fresnel_v_imag = Aimag(rvth)                                                            
    fresnel_v      = fresnel_v_Real * fresnel_v_Real + &                                    
         & fresnel_v_imag * fresnel_v_imag                                                  
    fresnel_h_Real = Dble(rhth)                                                             
    fresnel_h_imag = Aimag(rhth)                                                            
    fresnel_h      = fresnel_h_Real * fresnel_h_Real + &                                    
         & fresnel_h_imag * fresnel_h_imag                                                  


    !1.3.2) Small scale correction to reflection coefficients                               
    !------                                                                                 

    If (freq_ghz >= 15.0_fp) Then                                                              
       small_rough_cor = Exp( c(21) * wind10 * coszen_sq / (freq_ghz_sq) )                  
    Else                                                                                    
       small_rough_cor = ONE                                                                
    End If                                                                                  

    !1.3.3) Large scale geometric correction                                                
    !------                                                                                 

    ! If the coefficent file contains FASTEM 2 it contains                                  
    ! also FASTEM 1 but the version choosen is given                                        
    ! by Fastem_Version                                                                     

    !Point to correct coefficients for this version. There are 36 altogether.               
    !Those for FASTEM-2/3 are stored in section 24:59 of the array, those for                 
    !FASTEM1 in section 60:95.                                                              
    If ( Fastem_Version == 2 .or. Fastem_Version == 3 ) Then                                                         
       c => Fastem3_Coef(24:59)                                                             
    Else                                                                                    
       c => Fastem3_Coef(60:95)                                                             
    End If                                                                                  
    Do j = 1, 12                                                                            
       zc(j) = c(j*3-2) + c(j*3-1)*freq_ghz + c(j*3)*freq_ghz_sq                            
    End Do                                                                                  
    !Point back to all coefficients again                                                   
    c => Fastem3_Coef                                                                       

    large_rough_cor(1) = &                                                                  
         & zc(1)                  + &                                                       
         & zc(2) * seczen    + &                                                            
         & zc(3) * seczen_sq + &                                                            
         & zc(4) * wind10         + &                                                       
         & zc(5) * wind10_sq      + &                                                       
         & zc(6) * windsec                                                                  
    large_rough_cor(2) = &                                                                  
         & zc(7)                   + &                                                      
         & zc(8)  * seczen    + &                                                           
         & zc(9)  * seczen_sq + &                                                           
         & zc(10) * wind10         + &                                                      
         & zc(11) * wind10_sq      + &                                                      
         & zc(12) * windsec                                                                 
    large_rough_cor(:) = large_rough_cor(:) * 0.01_fp                                  

    ! For Fastem-3 do not compute rough surface effects if theta > 60 degrees               
    If (Fastem_Version <= 2 .or. (Fastem_Version == 3 .And. coszen >= 0.5_fp)) Then    
       Emissivity(1) = ONE - fresnel_v * small_rough_cor + large_rough_cor(1)       
       Emissivity(2) = ONE - fresnel_h * small_rough_cor + large_rough_cor(2)       
    Else                                                                                    
       Emissivity(1) = ONE - fresnel_v                                              
       Emissivity(2) = ONE - fresnel_h                                              
    End If                                                                                  

    Emissivity(3) = ZERO                                          
    Emissivity(4) = ZERO                                          

    !Apply foam correction                                               
    foam_cor  = c(22) * ( wind10 ** c(23) )                              
    Emissivity(1) = Emissivity(1) - foam_cor*Emissivity(1) + foam_cor    
    Emissivity(2) = Emissivity(2) - foam_cor*Emissivity(2) + foam_cor    

    If ( Fastem_Version == 3 .AND. Sat_Azimuth_Angle >= (-360.0_fp) ) then                               
      ! Add azimuthal component from Fuzhong Weng (NOAA/NESDIS) based on work by Dr. Gene Poe (NRL)   
      ! Angle between wind direction and satellite azimuthal view angle                               

! version 8_5      phi = (wind10_direction-Sat_Azimuth_Angle)*pi/180.0_fp  
      phi = PI - (wind10_direction-Sat_Azimuth_Angle)*pi/180.0_fp    ! version 8_7                                 
      ! Assume 19m wind = 10m wind for now (fix later).                                               
      u19=wind10                                                                                      
      Do ich = 0,15                                                                                   
         a1e = c(141+ich*12) + u19*(c(142+ich*12)+ u19*(c(143+ich*12)+u19*c(144+ich*12)))             
         a2e = c(145+ich*12) + u19*(c(146+ich*12)+ u19*(c(147+ich*12)+u19*c(148+ich*12)))             
         a3e = c(149+ich*12) + u19*(c(150+ich*12)+ u19*(c(151+ich*12)+u19*c(152+ich*12)))             
         i_freq = int(ich/4) + 1   ! 37, 19, 10, 7 GHz                                                
         j_stokes = mod(ich,4) + 1                                                                    
         tbfixed(j_stokes,i_freq,1) = a1e !* SST                                                      
         tbfixed(j_stokes,i_freq,2) = a2e !* SST                                                      
         tbfixed(j_stokes,i_freq,3) = a3e !* SST                                                      
      End Do                                                                                          

      Do M = 1, 3                                                                 
        Do istokes=1,4                                                        
          efixed(1,istokes,M)= tbfixed(istokes,4,M) !/SST  ! 7  GHz            
          efixed(2,istokes,M)= tbfixed(istokes,3,M) !/SST  ! 10  GHz           
          efixed(3,istokes,M)= tbfixed(istokes,2,M) !/SST  ! 19  GHz           
          efixed(4,istokes,M)= tbfixed(istokes,1,M) !/SST  ! 37  GHz           
        End Do                                                                

      ! Interpolate results to required frequency based on 7, 10, 19, 37 GHz   

        If (freq_ghz.le.freqfixed(1)) Then                                    
          Do istokes=1,4                                                       
             einterpolated(istokes,M)=efixed(1,istokes,M)                      
          End Do                                                               
        Else If(freq_ghz.ge.freqfixed(4)) then                                
          Do istokes=1,4                                                       
             einterpolated(istokes,M)=efixed(4,istokes,M)                      
          End Do                                                               
        Else
          If(freq_ghz.lt.freqfixed(2)) ifreq=2
          If(freq_ghz.lt.freqfixed(3).and.freq_ghz.ge.freqfixed(2)) ifreq=3
          If(freq_ghz.ge.freqfixed(3)) ifreq=4
          dfreq=(freq_ghz-freqfixed(ifreq-1))/(freqfixed(ifreq)-freqfixed(ifreq-1))
          Do istokes=1,4
            einterpolated(istokes,M)=efixed(ifreq-1,istokes,M)+dfreq*  &
              (efixed(ifreq,istokes,M)-efixed(ifreq-1,istokes,M))
          End Do
        End If
      End Do

         Do istokes = 1,4
            azimuthal_emiss=ZERO
            Do M=1,3
         If(istokes.le.2) Then
            azimuthal_emiss=azimuthal_emiss+einterpolated(istokes,M)*cos(m*phi)*  &
              (ONE-coszen)/(ONE - 0.6018_fp)
         Else
            azimuthal_emiss=azimuthal_emiss+einterpolated(istokes,M)*sin(m*phi)*  &
               (ONE-coszen)/(ONE - 0.6018_fp)
         End If
            End Do
            Emissivity(istokes)=Emissivity(istokes)+azimuthal_emiss
         End Do
        End If

        ! Only apply non-specular correction for Fastem-3 if theta < 60 degrees
        If ((Fastem_Version == 2 .or. (Fastem_Version == 3 .And. coszen >= 0.5_fp)) .And. &
             & Transmittance < 0.9999_fp .And. &
             & Transmittance  > 0.00001_fp ) Then

           !Convert windspeed to slope variance using the Cox and Munk model
           variance = 0.00512_fp * wind10 + 0.0030_fp
           varm     = variance * c(138)
           variance = varm * ( c(139) * freq_ghz + c(140) )
           If ( variance > varm ) variance = varm
           If ( variance < ZERO  ) variance = ZERO

           !Compute surface to space optical depth
           opdpsfc = -log(Transmittance ) / seczen

           !Define nine predictors for the effective angle calculation
           zx(1) = ONE
           zx(2) = variance
           zx(4) = ONE / coszen
           zx(3) = zx(2) * zx(4)
           zx(5) = zx(3) * zx(3)
           zx(6) = zx(4) * zx(4)
           zx(7) = zx(2) * zx(2)
           zx(8) = log(opdpsfc)
           zx(9) = zx(8) * zx(8)

           zrough_v = ONE
           zrough_h = ONE
           Do jcof = 1,7
              jcofm1 = jcof-1
              !Switched h to v Deblonde SSMIS june 7, 2001
              zrough_h = zrough_h + &
                   & zx(jcof) * ( c(96+jcofm1*3) &
                   & + zx(8)    *   c(97+jcofm1*3) &
                   & + zx(9)    *   c(98+jcofm1*3) )
              zrough_v = zrough_v + &
                   & zx(jcof) * ( c(117+jcofm1*3) &
                   & + zx(8)    *   c(118+jcofm1*3) &
                   & + zx(9)    *   c(119+jcofm1*3) )
           End Do

           zreflmod_v = (ONE-Transmittance ** zrough_v) &
              & / (ONE-Transmittance )
           zreflmod_h = (ONE-Transmittance ** zrough_h) &
              & / (ONE-Transmittance ) 
           Reflectivity(1)  = zreflmod_v * (ONE-Emissivity(1))
           Reflectivity(2)  = zreflmod_h * (ONE-Emissivity(2))
           Reflectivity(3)  = ZERO
           Reflectivity(4)  = ZERO
        Else
           Reflectivity(1) = ONE - Emissivity(1)
           Reflectivity(2) = ONE - Emissivity(2)
           Reflectivity(3) = ZERO
           Reflectivity(4) = ZERO
        End If

End SUBROUTINE Fastem3
!
!
  SUBROUTINE Fastem3_TL(Frequency,                                      & ! INPUT
                        Sat_Zenith_Angle,                                & ! INPUT
                        Sat_Azimuth_Angle,                               & ! INPUT
                        SST,                                             & ! INPUT
                        Wind_Speed,                                      & ! INPUT
                        Wind_Direction,                                  & ! INPUT
                        Transmittance,                                   & ! INPUT
                        SST_TL,                                          & ! INPUT
                        Wind_Speed_TL,                                   & ! INPUT
                        Wind_Direction_TL,                               & ! INPUT
                        Transmittance_TL,                                & ! INPUT
                        Fastem_Version,                                  & ! INPUT
                        Emissivity_TL,                                   & ! OUTPUT
                        Reflectivity_TL)                                   ! OUTPUT
  ! Description:
  ! Tangent Linear of rttov_calcemis_mw
  ! To compute MW surface emissivities for all channels and all
  ! profiles if desired
  !
  ! Copyright:
  !    This software was developed within the context of
  !    the EUMETSAT Satellite Application Facility on
  !    Numerical Weather Prediction (NWP SAF), under the
  !    Cooperation Agreement dated 25 November 1998, between
  !    EUMETSAT and the Met Office, UK, by one or more partners
  !    within the NWP SAF. The partners in the NWP SAF are
  !    the Met Office, ECMWF, KNMI and MeteoFrance.
  !
  !    Copyright 2002, EUMETSAT, All Rights Reserved.
  !
  ! Method:
  ! FASTEM-1 English and Hewison 1998.
  ! FASTEM-2 Deblonde and English 2001.
  ! FASTEM-3 English 2003.
  ! http://www.metoffice.com/research/interproj/nwpsaf/rtm/evalfastems.pdf
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       01/12/2002  New F90 code with structures (P Brunel A Smith)
  !  1.1       02/01/2003  Comments added (R Saunders)
  !  1.2       26/09/2003  Polarimetric code and Fastem-3 (S English)
  !  1.3       18/08/2004  Added some _fp to constants (S English)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  !
  ! Imported Parameters:

  INTEGER, INTENT( IN ) :: Fastem_Version
  REAL(fp), INTENT( IN ) ::  Frequency, Sat_Zenith_Angle, Sat_Azimuth_Angle
  REAL(fp), INTENT( IN ) ::  SST, Wind_Speed, Transmittance, Wind_Direction
  REAL(fp), INTENT( IN ) ::  SST_TL, Wind_Speed_TL, Transmittance_TL, Wind_Direction_TL
  REAL(fp), INTENT( OUT ) :: Emissivity_TL(:), Reflectivity_TL(:)

  !local constants:
  REAL(fp), Parameter :: quadcof(4,2) = Reshape( &
       & (/ ZERO, ONE, ONE, 2.0_fp, &
       & ONE, -ONE, ONE, -ONE  /), (/4,2/) )
  REAL(fp), Parameter :: freqfixed(4) = Reshape( &
       & (/ 7.0_fp, 10.0_fp, 19.0_fp, 37.0_fp /), (/4/) )

  !local variables:
  REAL(fp) :: Emissivity(4)
  REAL(fp) :: tcelsius,coszen,coszen_sq,sinzen,seczen,seczen_sq
  REAL(fp) :: tcelsius_sq
  REAL(fp) :: tcelsius_cu
  REAL(fp) :: f1,f2
  REAL(fp) :: del1,del2
  REAL(fp) :: einf
  REAL(fp) :: fen,fen_sq
  REAL(fp) :: den1,den2
  REAL(fp) :: perm_free
  REAL(fp) :: sigma
  REAL(fp) :: perm_real1,perm_real2
  REAL(fp) :: perm_imag1,perm_imag2,perm_imag3
  REAL(fp) :: perm_Real,perm_imag
  REAL(fp) :: perm_static,perm_infinite
  REAL(fp) :: freq_ghz,freq_ghz_sq
  REAL(fp) :: fresnel_v_Real,fresnel_v_imag
  REAL(fp) :: fresnel_h_Real,fresnel_h_imag
  REAL(fp) :: fresnel_v,fresnel_h
  REAL(fp) :: small_rough_cor,foam_cor
  REAL(fp) :: large_rough_cor(2)
  REAL(fp) :: small_rough,large_rough
  REAL(fp) :: variance,varm
  REAL(fp) :: wind10
  REAL(fp) :: wind10_sq,windsec
  REAL(fp) :: wind10_direction, windangle, windratio ! Note wind azimuth is in radians
  REAL(fp) :: opdpsfc,freqr
  REAL(fp) :: zrough_v,zrough_h
  REAL(fp) :: zreflmod_v,zreflmod_h
  REAL(fp) :: delta,delta2
  REAL(fp) :: qdepol,emissfactor
  REAL(fp) :: emissfactor_v,emissfactor_h
  REAL(fp) :: zc(12),zx(9)
  REAL(fp) :: azimuthal_emiss,u19,phi,dfreq
  REAL(fp) :: tbfixed(4,4,3)   ! Surface brightness temperature azimuthal variation terms for 37, 19, 10, 7 GHz
  REAL(fp) :: efixed(4,4,3)     ! Emissivity azimuthal variation terms for 7, 10, 19, 37 GHz
  REAL(fp) :: einterpolated(4,3)   ! Emissivity azimuthal variation terms for interpolated to required frequency
  REAL(fp) :: a1e,a2e,a3e    ! coefficients used in azimuthal emissivity model
  REAL(fp), Pointer :: c(:)
  COMPLEX(fp) :: perm1,perm2
  COMPLEX(fp) :: rhth,rvth
  COMPLEX(fp) :: permittivity
  INTEGER :: i,j,chan,istokes,ifreq,m
  INTEGER :: iquadrant    ! Determines which quadrant (NE, SE, SW, NW) the wind is blowing to
  INTEGER :: pol_id    ! polarisation indice
  INTEGER :: i_freq,j_stokes,ich,ichannel   ! indices used in azimuthal emissivity model
  INTEGER :: jcof,jcofm1


  REAL(fp) :: tcelsius_tl
  REAL(fp) :: tcelsius_sq_tl
  REAL(fp) :: tcelsius_cu_tl
  REAL(fp) :: f1_tl, f2_tl
  REAL(fp) :: del1_tl, del2_tl
  REAL(fp) :: einf_tl
  REAL(fp) :: fen_tl, fen_sq_tl
  REAL(fp) :: den1_tl, den2_tl
  REAL(fp) :: sigma_tl
  REAL(fp) :: perm_real1_tl, perm_real2_tl
  REAL(fp) :: perm_imag1_tl, perm_imag2_tl, perm_imag3_tl
  REAL(fp) :: perm_Real_tl, perm_imag_tl
  REAL(fp) :: perm_static_tl, perm_infinite_tl
  REAL(fp) :: fresnel_v_Real_tl, fresnel_v_imag_tl
  REAL(fp) :: fresnel_h_Real_tl, fresnel_h_imag_tl
  REAL(fp) :: fresnel_v_tl, fresnel_h_tl
  REAL(fp) :: small_rough_cor_tl, foam_cor_tl
  REAL(fp) :: large_rough_cor_tl(2)
  REAL(fp) :: small_rough_tl, large_rough_tl
  REAL(fp) :: variance_tl, varm_tl
  REAL(fp) :: wind10_tl
  REAL(fp) :: wind10_sq_tl, windsec_tl
  REAL(fp) :: wind10_direction_tl, windangle_tl, windratio_tl ! Note wind azimuth is in radians
  REAL(fp) :: opdpsfc_tl, freqr_tl
  REAL(fp) :: zrough_v_tl, zrough_h_tl
  REAL(fp) :: zreflmod_v_tl, zreflmod_h_tl
  REAL(fp) :: delta_tl, delta2_tl
  REAL(fp) :: qdepol_tl, emissfactor_tl
  REAL(fp) :: emissfactor_v_tl, emissfactor_h_tl
  REAL(fp) :: zx_tl(9)
  REAL(fp) :: azimuthal_emiss_tl,u19_tl,phi_tl
  REAL(fp) :: tbfixed_tl(4,4,3)   ! Surface brightness temperature azimuthal variation terms for 37, 19, 10, 7 GHz
  REAL(fp) :: efixed_tl(4,4,3)     ! Emissivity azimuthal variation terms for 7, 10, 19, 37 GHz
  REAL(fp) :: einterpolated_tl(4,3)   ! Emissivity azimuthal variation terms for interpolated to required frequency
  REAL(fp) :: a1e_tl,a2e_tl,a3e_tl,atot     ! coefficients used in azimuthal emissivity model
  COMPLEX(fp) :: perm1_tl, perm2_tl
  COMPLEX(fp) :: rhth_tl, rvth_tl
  COMPLEX(fp) :: permittivity_tl
  INTEGER :: iii  ! user fastem version request
  !-------------------------------------------------------------------------------

   sinzen = SIN( Sat_Zenith_Angle * DEGREES_TO_RADIANS )
     coszen = COS( Sat_Zenith_Angle * DEGREES_TO_RADIANS )
     coszen_sq = coszen ** 2
     seczen = ONE / coszen
     seczen_sq = seczen ** 2

     c => Fastem3_Coef

        ! no TL on wind direction, but TL on wind speed
        wind10    = Wind_Speed
        wind10_sq = wind10 ** 2
        wind10_direction = Wind_Direction
        windsec          = wind10 * seczen

        wind10_tl = Wind_Speed_TL
        wind10_sq_tl = 2 * wind10 * wind10_tl
        wind10_direction_tl = Wind_Direction_TL
        windsec_tl   = wind10_tl * seczen

        !Set values for temperature polynomials (convert from kelvin to celsius)
        tcelsius = SST - 273.15_fp
        tcelsius_sq = tcelsius * tcelsius     !quadratic
        tcelsius_cu = tcelsius_sq * tcelsius  !cubic

        tcelsius_tl    = SST_TL
        tcelsius_sq_tl = 2 * tcelsius * tcelsius_tl
        tcelsius_cu_tl = 3 * tcelsius_sq * tcelsius_tl

        !Define two relaxation frequencies, f1 and f2
        f1 = c(1) + c(2) * tcelsius + c(3) * tcelsius_sq
        f2 = c(4) + c(5) * tcelsius + c(6) * tcelsius_sq + c(7) * tcelsius_cu
        f1_tl =  c(2) * tcelsius_tl + c(3) * tcelsius_sq_tl
        f2_tl =  c(5) * tcelsius_tl + c(6) * tcelsius_sq_tl + c(7) * tcelsius_cu_tl


        !Static permittivity estatic = del1+del2+einf
        del1 = c(8)  + c(9)  * tcelsius + c(10) * tcelsius_sq + c(11) * tcelsius_cu
        del2 = c(12) + c(13) * tcelsius + c(14) * tcelsius_sq + c(15) * tcelsius_cu
        einf = c(18) + c(19) * tcelsius
        del1_tl = c(9)  * tcelsius_tl + c(10) * tcelsius_sq_tl + c(11) * tcelsius_cu_tl
        del2_tl = c(13) * tcelsius_tl + c(14) * tcelsius_sq_tl + c(15) * tcelsius_cu_tl
        einf_tl = c(19) * tcelsius_tl


        freq_ghz    = Frequency
        freq_ghz_sq = freq_ghz * freq_ghz

        !-----------------------------------------------------
        !1.2 calculate permittivity using double-debye formula
        !-----------------------------------------------------

        fen          = 2.0_fp * c(20) * freq_ghz * 0.001_fp
        fen_sq       = fen*fen
        den1         = ONE + fen_sq * f1 * f1
        den2         = ONE + fen_sq * f2 * f2
        perm_real1   = del1 / den1
        perm_real2   = del2 / den2
        perm_imag1   = del1 * fen * f1 / den1
        perm_imag2   = del2 * fen * f2 / den2
        perm_free    = 8.854E-03_fp
        sigma        = 2.906_fp + 0.09437_fp * tcelsius
        perm_imag3   = sigma / (2.0_fp * c(20) * perm_free * freq_ghz)
        perm_Real    = perm_real1 + perm_real2 + einf
        perm_imag    = perm_imag1 + perm_imag2 + perm_imag3
        permittivity = Cmplx(perm_Real,perm_imag,fp)

        den1_tl         = 2 * fen_sq * f1 * f1_tl
        den2_tl         = 2 * fen_sq * f2 * f2_tl
        perm_real1_tl   = (den1 * del1_tl - del1 * den1_tl) / (den1 * den1)
        perm_real2_tl   = (den2 * del2_tl - del2 * den2_tl) / (den2 * den2)
        perm_imag1_tl   = fen * ( den1 * ( del1_tl * f1 + del1 * f1_tl)&
              & - (del1 * f1 * den1_tl) )  / (den1 * den1)
        perm_imag2_tl   = fen * ( den2 * ( del2_tl * f2 + del2 * f2_tl)&
              & - (del2 * f2 * den2_tl) )  / (den2 * den2)
        sigma_tl        = 0.09437_fp * tcelsius_tl
        perm_imag3_tl   = sigma_tl / (2.0_fp * c(20) * perm_free * freq_ghz)
        perm_Real_tl    = perm_real1_tl + perm_real2_tl + einf_tl
        perm_imag_tl    = perm_imag1_tl + perm_imag2_tl + perm_imag3_tl
        permittivity_tl = Cmplx(perm_Real_tl,perm_imag_tl,fp)

        !-------------------------------------------------------------
        !1.3 calculate complex reflection coefficients and corrections
        !-------------------------------------------------------------


        !1.3.1) Fresnel reflection coefficients
        !------

        perm1          = sqrt(permittivity - sinzen ** 2)
        perm2          = permittivity * coszen
        rhth           = (coszen-perm1) / (coszen+perm1)
        rvth           = (perm2-perm1) / (perm2+perm1)
        !    fresnel_v_real = dble(rvth)
        fresnel_v_Real = Real(rvth)
        fresnel_v_imag = Aimag(rvth)
        fresnel_v      = fresnel_v_Real * fresnel_v_Real + &
             & fresnel_v_imag * fresnel_v_imag
        !    fresnel_h_real = dble(rhth)
        fresnel_h_Real = Real(rhth)
        fresnel_h_imag = Aimag(rhth)
        fresnel_h      = fresnel_h_Real * fresnel_h_Real + &
             & fresnel_h_imag * fresnel_h_imag


        perm1_tl          = 0.5_fp * permittivity_tl / perm1
        perm2_tl          = permittivity_tl * coszen
        rhth_tl           = - 2 * coszen * perm1_tl / (coszen+perm1)**2
        rvth_tl           = 2 * (perm1 * perm2_tl - perm1_tl * perm2) / (perm2+perm1)**2
        !    fresnel_v_real_tl = dble(rvth_tl)
        fresnel_v_Real_tl = Real(rvth_tl)
        fresnel_v_imag_tl = Aimag(rvth_tl)
        fresnel_v_tl      = 2 * fresnel_v_Real * fresnel_v_Real_tl + &
              & 2 * fresnel_v_imag * fresnel_v_imag_tl
        !    fresnel_h_real_tl = dble(rhth_tl)
        fresnel_h_Real_tl = Real(rhth_tl)
        fresnel_h_imag_tl = Aimag(rhth_tl)
        fresnel_h_tl      = 2 * fresnel_h_Real * fresnel_h_Real_tl + &
              & 2 * fresnel_h_imag * fresnel_h_imag_tl

        !1.3.2) Small scale correction to reflection coefficients
        !------

        If (freq_ghz >= 15.0) Then
           small_rough_cor = Exp( c(21) * wind10 * coszen_sq / (freq_ghz_sq) )
           small_rough_cor_tl = small_rough_cor * c(21) * wind10_tl * coszen_sq / (freq_ghz_sq)
        Else
           small_rough_cor    = 1.0
           small_rough_cor_tl = 0.0
        End If

        !1.3.3) Large scale geometric correction
        !------

        ! If the coefficent file contains FASTEM 2 it contains
        ! also FASTEM 1 but the version choosen is given
        ! by coef Fastem_Version

        !Point to correct coefficients for this version. There are 36 altogether.
        !Those for FASTEM-2/3 are stored in section 24:59 of the array, those for
        !FASTEM1 in section 60:95.
        If ( Fastem_Version == 2 .or. Fastem_Version == 3 ) Then
           c => Fastem3_Coef(24:59)
        Else
           c => Fastem3_Coef(60:95)
        End If
        Do j = 1, 12
           zc(j) = c(j*3-2) + c(j*3-1)*freq_ghz + c(j*3)*freq_ghz_sq
        End Do
        !Point back to all coefficients again
        c => Fastem3_Coef

        large_rough_cor(1) = &
             & (zc(1)                  + &
              & zc(2) * seczen    + &
              & zc(3) * seczen_sq + &
              & zc(4) * wind10         + &
              & zc(5) * wind10_sq      + &
              & zc(6) * windsec) / HUNDRED
        large_rough_cor(2) = &
             & (zc(7)                   + &
              & zc(8)  * seczen    + &
              & zc(9)  * seczen_sq + &
              & zc(10) * wind10         + &
              & zc(11) * wind10_sq      + &
              & zc(12) * windsec) / HUNDRED
        !    large_rough_cor(:) = large_rough_cor(:) * 0.01

        large_rough_cor_tl(1) =   &
             & (zc(4) * wind10_tl     + &
              & zc(5) * wind10_sq_tl  + &
              & zc(6) * windsec_tl ) /HUNDRED
        large_rough_cor_tl(2) =      &
             & (zc(10) * wind10_tl    + &
              & zc(11) * wind10_sq_tl + &
              & zc(12) * windsec_tl) /HUNDRED

        ! For Fastem-3 do not compute rough surface effects if theta > 60 degrees
        If ( Fastem_Version <= 2 .or. (Fastem_Version == 3 .And. coszen >= 0.5_fp)) then
           Emissivity(1) = ONE - fresnel_v * small_rough_cor + large_rough_cor(1)
           Emissivity(2) = ONE - fresnel_h * small_rough_cor + large_rough_cor(2)
           Emissivity_TL(1) = - fresnel_v_tl * small_rough_cor    &
                 & - fresnel_v    * small_rough_cor_tl &
                 & + large_rough_cor_tl(1)
           Emissivity_TL(2) = - fresnel_h_tl * small_rough_cor    &
                 & - fresnel_h    * small_rough_cor_tl &
                 & + large_rough_cor_tl(2)
        Else
           Emissivity(1) = ONE - fresnel_v
                 Emissivity(2) = ONE - fresnel_h
                 Emissivity_TL(1) = - fresnel_v_tl
                 Emissivity_TL(2) = - fresnel_h_tl
        End If

        Emissivity(3) = ZERO
        Emissivity(4) = ZERO
        Emissivity_TL(3) = ZERO
        Emissivity_TL(4) = ZERO

        !Apply foam correction
        foam_cor  = c(22) * ( wind10 ** c(23) )
        foam_cor_tl  =  c(22) * c(23) * wind10_tl * ( wind10 ** (c(23)-ONE) )


        ! Be careful do TL first because the next 2 lines of the direct model
        ! have variables in input/output of the statement

        Emissivity_TL(1) = Emissivity_TL(1)-foam_cor_tl*Emissivity(1)-foam_cor*Emissivity_TL(1)+foam_cor_tl
        Emissivity_TL(2) = Emissivity_TL(2)-foam_cor_tl*Emissivity(2)-foam_cor*Emissivity_TL(2)+foam_cor_tl
        Emissivity(1) = Emissivity(1) - foam_cor*Emissivity(1) + foam_cor
        Emissivity(2) = Emissivity(2) - foam_cor*Emissivity(2) + foam_cor

        If ( Fastem_Version == 3 .AND. Sat_Azimuth_Angle >= (-360.0)) then
           ! Add azimuthal component from Fuzhong Weng (NOAA/NESDIS) based on work by Dr. Gene Poe (NRL)
           ! Assume 19m wind = 10m wind for now (fix later)
           u19=wind10

           ! Angle between wind direction and satellite azimuthal view angle
           phi = PI - (wind10_direction-Sat_Azimuth_Angle)*pi/180.0_fp
           phi_tl = -wind10_direction_tl*pi/180.0_fp
           u19_tl = wind10_tl
           tbfixed(:,:,:) = ZERO
           tbfixed_tl(:,:,:) = ZERO
           Do ich = 0,15
              a1e = c(141+ich*12) + u19*(c(142+ich*12)+ u19*(c(143+ich*12)+u19*c(144+ich*12)))
              a2e = c(145+ich*12) + u19*(c(146+ich*12)+ u19*(c(147+ich*12)+u19*c(148+ich*12)))
              a3e = c(149+ich*12) + u19*(c(150+ich*12)+ u19*(c(151+ich*12)+u19*c(152+ich*12)))
              a1e_tl = u19_tl*(c(142+ich*12)+u19*(2.0*c(143+ich*12)+3.0*u19*c(144+ich*12)))
              a2e_tl = u19_tl*(c(146+ich*12)+u19*(2.0*c(147+ich*12)+3.0*u19*c(148+ich*12)))
              a3e_tl = u19_tl*(c(150+ich*12)+u19*(2.0*c(151+ich*12)+3.0*u19*c(152+ich*12)))
              i_freq = int(ich/4) + 1   ! 37, 19, 10, 7 GHz
              j_stokes = mod(ich,4) + 1
              tbfixed(j_stokes,i_freq,1) = a1e
              tbfixed(j_stokes,i_freq,2) = a2e
              tbfixed(j_stokes,i_freq,3) = a3e
              tbfixed_tl(j_stokes,i_freq,1) = a1e_tl
              tbfixed_tl(j_stokes,i_freq,2) = a2e_tl
              tbfixed_tl(j_stokes,i_freq,3) = a3e_tl
           End Do
           efixed_tl(:,:,:)=ZERO
           einterpolated_tl(:,:)=ZERO

           Do M=1,3
              Do istokes=1,4
                efixed(1,istokes,M)= tbfixed(istokes,4,M)  ! 7   GHz
                efixed(2,istokes,M)= tbfixed(istokes,3,M)  ! 10  GHz
                efixed(3,istokes,M)= tbfixed(istokes,2,M)  ! 19  GHz
                efixed(4,istokes,M)= tbfixed(istokes,1,M)  ! 37  GHz
                efixed_tl(1,istokes,M)= tbfixed_tl(istokes,4,M)  ! 7  GHz
                efixed_tl(2,istokes,M)= tbfixed_tl(istokes,3,M)  ! 10  GHz
                efixed_tl(3,istokes,M)= tbfixed_tl(istokes,2,M)  ! 19  GHz
                efixed_tl(4,istokes,M)= tbfixed_tl(istokes,1,M)  ! 37  GHz
              End Do

              ! Interpolate results to required frequency based on 7, 10, 19, 37 GHz
              If (freq_ghz.le.freqfixed(1)) Then
                einterpolated(:,M)=efixed(1,:,M)
                einterpolated_tl(:,M)=efixed_tl(1,:,M)
              Else If(freq_ghz.ge.freqfixed(4)) then
                einterpolated(:,M)=efixed(4,:,M)
                einterpolated_tl(:,M)=efixed_tl(4,:,M)
              Else
                If(freq_ghz.lt.freqfixed(2)) ifreq=2
                If(freq_ghz.lt.freqfixed(3).and.freq_ghz.ge.freqfixed(2)) ifreq=3
                If(freq_ghz.ge.freqfixed(3)) ifreq=4
                dfreq=(freq_ghz-freqfixed(ifreq-1))/(freqfixed(ifreq)-freqfixed(ifreq-1))
                einterpolated(:,M)=efixed(ifreq-1,:,M)+dfreq*(efixed(ifreq,:,M)-efixed(ifreq-1,:,M))
                einterpolated_tl(:,M)=efixed_tl(ifreq-1,:,M)+dfreq*(efixed_tl(ifreq,:,M)-efixed_tl(ifreq-1,:,M))
              End If
           End Do

           Do istokes = 1,4
              azimuthal_emiss=ZERO
              azimuthal_emiss_tl=ZERO
              Do M=1,3
                 If(istokes.le.2) Then
                    azimuthal_emiss=azimuthal_emiss+einterpolated(istokes,M)*cos(m*phi)*&
                    &(ONE-coszen)/(ONE - 0.6018_fp)
                    azimuthal_emiss_tl=azimuthal_emiss_tl+(einterpolated_tl(istokes,M)*cos(m*phi) -&
                      & einterpolated(istokes,M)*m*sin(m*phi)*phi_tl)*(ONE-coszen)/(ONE - 0.6018_fp)
                 Else
                    azimuthal_emiss=azimuthal_emiss+einterpolated(istokes,M)*sin(m*phi)*(ONE-coszen)/&
                    &(ONE - 0.6018_fp)
                    azimuthal_emiss_tl=azimuthal_emiss_tl+(einterpolated_tl(istokes,M)*sin(m*phi) +&
                      & einterpolated(istokes,M)*m*cos(m*phi)*phi_tl)*(ONE-coszen)/(ONE - 0.6018_fp)
                 End If
              End Do
              Emissivity_TL(istokes)=Emissivity_TL(istokes)+azimuthal_emiss_tl
           End Do
        End If

! Only apply non-specular correction for Fastem-3 if theta < 60 degrees
        If ((Fastem_Version == 2 .or. (Fastem_Version == 3 .And. coszen >= 0.5_fp)) .And. &
             & Transmittance < 0.9999_fp .And. Transmittance > 0.00001_fp ) Then

           !Convert windspeed to slope variance using the Cox and Munk model
           variance = 0.00512_fp * wind10 + 0.0030_fp
           varm     = variance * c(138)
           variance = varm * ( c(139) * freq_ghz + c(140) )

           variance_tl = 0.00512_fp * wind10_tl
           varm_tl     = variance_tl * c(138)
           variance_tl = varm_tl * ( c(139) * freq_ghz + c(140) )

           If ( variance > varm ) Then
              variance    = varm
              variance_tl = varm_tl
           Endif
           If ( variance < ZERO  ) Then
              variance    = ZERO
              variance_tl = ZERO
           Endif

           !Compute surface to space optical depth
           opdpsfc    = -log(Transmittance) / seczen
           opdpsfc_tl = -Transmittance_TL / ( Transmittance * seczen )

           !Define nine predictors for the effective angle calculation
           zx(1) = ONE
           zx(2) = variance
           zx(4) = ONE / coszen
           zx(3) = zx(2) * zx(4)
           zx(5) = zx(3) * zx(3)
           zx(6) = zx(4) * zx(4)
           zx(7) = zx(2) * zx(2)
           zx(8) = log(opdpsfc)
           zx(9) = zx(8) * zx(8)

           zx_tl(1) = ZERO
           zx_tl(2) = variance_tl
           zx_tl(4) = ZERO
           zx_tl(3) = zx_tl(2) * zx(4)
           zx_tl(5) = 2 * zx_tl(3) * zx(3)
           zx_tl(6) = 2 * zx_tl(4) * zx(4)
           zx_tl(7) = 2 * zx_tl(2) * zx(2)
           zx_tl(8) = opdpsfc_tl / opdpsfc
           zx_tl(9) = 2 * zx_tl(8) * zx(8)

           zrough_v = ONE
           zrough_h = ONE

           zrough_v_tl = ZERO
           zrough_h_tl = ZERO

           Do jcof = 1,7
              jcofm1 = jcof-1
              !Switched h to v Deblonde SSMIS june 7, 2001
              zrough_h = zrough_h + &
                   & zx(jcof) * ( c(96+jcofm1*3) &
                   & + zx(8)    *   c(97+jcofm1*3) &
                   & + zx(9)    *   c(98+jcofm1*3) )
              zrough_v = zrough_v + &
                   & zx(jcof) * ( c(117+jcofm1*3) &
                   & + zx(8)    *   c(118+jcofm1*3) &
                   & + zx(9)    *   c(119+jcofm1*3) )

              zrough_h_tl = zrough_h_tl +         &
                   & zx(jcof)     * (                  &
                   & zx_tl(8)  *   c(97+jcofm1*3)   &
                   & + zx_tl(9)  *   c(98+jcofm1*3) ) &
                   & +  zx_tl(jcof) * ( c(96+jcofm1*3)   &
                   & + zx(8)  *   c(97+jcofm1*3)   &
                   & + zx(9)  *   c(98+jcofm1*3) )
              zrough_v_tl = zrough_v_tl +          &
                   & zx(jcof)     * (                   &
                   & zx_tl(8)  *   c(118+jcofm1*3)   &
                   & + zx_tl(9)  *   c(119+jcofm1*3) ) &
                   & +  zx_tl(jcof) * ( c(117+jcofm1*3)   &
                   & + zx(8)  *   c(118+jcofm1*3)   &
                   & + zx(9)  *   c(119+jcofm1*3) )
           End Do

           zreflmod_v = (ONE-Transmittance**zrough_v)/(ONE-Transmittance)
           zreflmod_h = (ONE-Transmittance**zrough_h)/(ONE-Transmittance)

           zreflmod_v_tl = Transmittance_tl  *&
                 & (-zrough_v * Transmittance**(zrough_v-ONE) * &
                 & (ONE-Transmittance)+&
                          & ( ONE-Transmittance**zrough_v)) &
                 & / (ONE-Transmittance)**2

           zreflmod_v_tl = zreflmod_v_tl - &
                & ( Transmittance**zrough_v * Log(Transmittance) * zrough_v_tl ) / &
                & (ONE-Transmittance)

           zreflmod_h_tl = Transmittance_tl  *&
                & (-zrough_h * Transmittance**(zrough_h-1.0) * (1.0-Transmittance) +  &
                &   ( 1.0-Transmittance**zrough_h)    ) &
                & / (1.0-Transmittance)**2
           zreflmod_h_tl = zreflmod_h_tl - &
                & ( Transmittance**zrough_h * Log(Transmittance) * zrough_h_tl ) / &
                & (1.0-Transmittance)

           Reflectivity_tl(1)  = zreflmod_v_tl * (1.0-Emissivity(1)) - zreflmod_v * Emissivity_TL(1)
           Reflectivity_tl(2)  = zreflmod_h_tl * (1.0-Emissivity(2)) - zreflmod_h * Emissivity_TL(2)
           Reflectivity_tl(3)  = -Emissivity_TL(3)
           Reflectivity_tl(4)  = -Emissivity_TL(4)
       Else
           Reflectivity_tl(:) = - Emissivity_TL(:)
       End If

  END SUBROUTINE Fastem3_TL


  SUBROUTINE Fastem3_AD(Frequency,                                      & ! INPUT
                        Sat_Zenith_Angle,                                & ! INPUT
                        Sat_Azimuth_Angle,                               & ! INPUT
                        SST,                                             & ! INPUT
                        Wind_Speed,                                      & ! INPUT
                        Wind_Direction,                                  & ! INPUT
                        Transmittance,                                   & ! INPUT
                        Emissivity,                                      & ! INPUT
                        Reflectivity,                                    & ! INPUT
                        Fastem_Version,                                  & ! INPUT
                        Emissivity_AD,                                   & ! INPUT/OUTPUT
                        Reflectivity_AD,                                 & ! INPUT/OUTPUT
                        SST_AD,                                          & ! OUTPUT
                        Wind_Speed_AD,                                   & ! OUTPUT
                        Wind_Direction_AD,                               & ! OUTPUT
                        Transmittance_AD)                                  ! INPUT
  ! Description:
  ! K matrix of rttov_calcemis_mw
  ! To compute MW surface emissivities for all channels and all
  ! profiles if desired
  !
  ! Copyright:
  !    This software was developed within the context of
  !    the EUMETSAT Satellite Application Facility on
  !    Numerical Weather Prediction (NWP SAF), under the
  !    Cooperation Agreement dated 25 November 1998, between
  !    EUMETSAT and the Met Office, UK, by one or more partners
  !    within the NWP SAF. The partners in the NWP SAF are
  !    the Met Office, ECMWF, KNMI and MeteoFrance.
  !
  !    Copyright 2002, EUMETSAT, All Rights Reserved.
  !
  ! Method:
  ! FASTEM-1 English and Hewison 1998.
  ! FASTEM-2 Deblonde and English 2001.
  ! FASTEM-3 English 2003
  ! http://www.metoffice.com/research/interproj/nwpsaf/rtm/evalfastems.pdf
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       01/12/2002  New F90 code with structures (P Brunel A Smith)
  !  1.1       02/01/2003  Comments added (R Saunders)
  !  1.2       26/09/2003  Polarimetric code and Fastem-3 (S. English)!
  !  1.3       18/08/2004  Corrected bug in K code (S English)
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  !
  ! Imported Parameters:

  !subroutine arguments:

  INTEGER, INTENT( IN ) :: Fastem_Version
  REAL(fp), INTENT( IN ) ::  Frequency, Sat_Zenith_Angle, Sat_Azimuth_Angle
  REAL(fp), INTENT( IN ) ::  SST, Wind_Speed, Transmittance, Wind_Direction
  REAL(fp), INTENT( IN ) :: Emissivity(:), Reflectivity(:)
  REAL(fp), INTENT( IN OUT ) :: Emissivity_AD(:), Reflectivity_AD(:)
  REAL(fp), INTENT( OUT ) ::  SST_AD, Wind_Speed_AD, Transmittance_AD, Wind_Direction_AD

  !local constants:
  REAL(fp), Parameter :: quadcof(4,2) = Reshape( &
       & (/ ZERO, ONE, ONE, 2.0_fp, &
       & ONE, -ONE, ONE, -ONE  /), (/4,2/) )
  REAL(fp), Parameter :: freqfixed(4) = Reshape( &
       & (/ 7.0_fp, 10.0_fp, 19.0_fp, 37.0_fp /), (/4/) )

  !local variables:
  REAL(fp) :: tcelsius,coszen,coszen_sq,sinzen,sinzen_sq,seczen,seczen_sq
  REAL(fp) :: tcelsius_sq
  REAL(fp) :: tcelsius_cu
  REAL(fp) :: f1,f2
  REAL(fp) :: del1,del2
  REAL(fp) :: einf
  REAL(fp) :: fen,fen_sq
  REAL(fp) :: den1,den2
  REAL(fp) :: perm_free
  REAL(fp) :: sigma
  REAL(fp) :: perm_real1,perm_real2
  REAL(fp) :: perm_imag1,perm_imag2,perm_imag3
  REAL(fp) :: perm_Real,perm_imag
  REAL(fp) :: perm_static,perm_infinite
  REAL(fp) :: freq_ghz,freq_ghz_sq
  REAL(fp) :: fresnel_v_Real,fresnel_v_imag
  REAL(fp) :: fresnel_h_Real,fresnel_h_imag
  REAL(fp) :: fresnel(4)
  REAL(fp) :: small_rough_cor,foam_cor(4)
  REAL(fp) :: large_rough_cor(4)
  REAL(fp) :: small_rough,large_rough
  REAL(fp) :: emiss_save(4)
  REAL(fp) :: variance,varm
  REAL(fp) :: wind10
  REAL(fp) :: wind10_sq,windsec
  REAL(fp) :: wind10_direction, windangle, windratio ! Note wind azimuth is in radians
  REAL(fp) :: emissstokes(4)
  REAL(fp) :: emissstokes_ad(4)
  REAL(fp) :: reflectstokes_ad(4)
  REAL(fp) :: u19,phi,dfreq
  REAL(fp) :: tbfixed(4,4,3)! Surface brightness temperature azimuthal variation terms for 37, 19, 10, 7 GHz
  REAL(fp) :: efixed(4,4,3) ! Emissivity azimuthal variation terms for 7, 10, 19, 37 GHz
  REAL(fp) :: einterpolated(4,3)! Emissivity azimuthal variation terms for interpolated to required frequency
  REAL(fp) :: a1e,a2e,a3e     ! coefficients used in azimuthal emissivity model
  REAL(fp) :: zrough_v,zrough_h
  REAL(fp) :: zreflmod_v,zreflmod_h
  REAL(fp) :: delta,delta2
  REAL(fp) :: qdepol,emissfactor
  REAL(fp) :: emissfactor_v,emissfactor_h
  REAL(fp) :: zc(12),zx(9)
  REAL(fp) :: opdpsfc,freqr
  REAL(fp), Pointer :: c(:)
  COMPLEX(fp) :: perm1,perm2
  COMPLEX(fp) :: rhth,rvth
  COMPLEX(fp) :: permittivity
  INTEGER :: i,j,chan,istokes,ifreq,m
  INTEGER :: iquadrant    ! Determines which quadrant (NE, SE, SW, NW) the wind is blowing to
  INTEGER :: pol_id  ! polarisation indice
  INTEGER :: i_freq,j_stokes,ich,ichannel   ! indices used in azimuthal emissivity model
  INTEGER :: jcof,jcofm1

  REAL(fp) :: tcelsius_ad
  REAL(fp) :: tcelsius_sq_ad
  REAL(fp) :: tcelsius_cu_ad
  REAL(fp) :: f1_ad, f2_ad
  REAL(fp) :: del1_ad, del2_ad
  REAL(fp) :: einf_ad
  REAL(fp) :: fen_ad, fen_sq_ad
  REAL(fp) :: den1_ad, den2_ad
  REAL(fp) :: sigma_ad
  REAL(fp) :: perm_real1_ad, perm_real2_ad
  REAL(fp) :: perm_imag1_ad, perm_imag2_ad, perm_imag3_ad
  REAL(fp) :: perm_Real_ad, perm_imag_ad
  REAL(fp) :: perm_static_ad, perm_infinite_ad
  REAL(fp) :: fresnel_v_Real_ad, fresnel_v_imag_ad
  REAL(fp) :: fresnel_h_Real_ad, fresnel_h_imag_ad
  REAL(fp) :: fresnel_v_ad, fresnel_h_ad
  REAL(fp) :: small_rough_cor_ad, foam_cor_ad
  REAL(fp) :: large_rough_cor_ad(2)
  REAL(fp) :: small_rough_ad, large_rough_ad
  REAL(fp) :: variance_ad, varm_ad
  REAL(fp) :: wind10_ad
  REAL(fp) :: wind10_sq_ad, windsec_ad
  REAL(fp) :: wind10_direction_ad, windangle_ad, windratio_ad ! Note wind azimuth is in radians
  REAL(fp) :: azimuthal_emiss_ad, azimuthal_emiss,u19_ad,phi_ad
  REAL(fp) :: tbfixed_ad(4,4,3)   ! Surface brightness temperature azimuthal variation terms for 37, 19, 10, 7 GHz
  REAL(fp) :: efixed_ad(4,4,3)   ! Emissivity azimuthal variation terms for 7, 10, 19, 37 GHz
  REAL(fp) :: einterpolated_ad(4,3)   ! Emissivity azimuthal variation terms for interpolated to required frequency
  REAL(fp) :: a1e_ad,a2e_ad,a3e_ad     ! coefficients used in azimuthal emissivity model
  REAL(fp) :: opdpsfc_ad, freqr_ad
  REAL(fp) :: zrough_v_ad, zrough_h_ad
  REAL(fp) :: zreflmod_v_ad, zreflmod_h_ad
  REAL(fp) :: delta_ad, delta2_ad
  REAL(fp) :: qdepol_ad, emissfactor_ad
  REAL(fp) :: emissfactor_v_ad, emissfactor_h_ad
  REAL(fp) :: zx_ad(9)
  COMPLEX(fp) :: perm1_ad, perm2_ad
  COMPLEX(fp) :: rhth_ad, rvth_ad
  COMPLEX(fp) :: permittivity_ad
  REAL(fp) :: test_variance
  !-------------------------------------------------------------------------------

  !If a TL value of emissivity is passed to the routine
  !Loop over channels

  phi_ad=ZERO
  efixed_ad(:,:,:)=ZERO

  Transmittance_AD = ZERO
  !-------------------------------
  !0. Point to fastem coefficients
  !-------------------------------
  sinzen = SIN( Sat_Zenith_Angle * DEGREES_TO_RADIANS )
  coszen = COS( Sat_Zenith_Angle * DEGREES_TO_RADIANS )
  coszen_sq = coszen ** 2
  sinzen_sq = ONE - coszen_sq
  seczen = ONE / coszen
  seczen_sq = seczen ** 2

  c => Fastem3_Coef

  reflectstokes_ad(:) = ZERO
  emissstokes_ad(:)   = ZERO

  wind10_ad = ZERO
  wind10_direction_ad = ZERO

  reflectstokes_ad(:) = reflectivity_ad(:)
  emissstokes_ad(:)   = emissivity_ad(:)


     !-------------------------------------------
     !1.1 Calculate channel independent variables
     !-------------------------------------------

     wind10 = Wind_Speed
     wind10_sq = Wind_Speed ** 2
     wind10    = Sqrt( wind10_sq )
     windsec   = wind10 * seczen
 
     wind10_direction = Wind_Direction
     !Set values for temperature polynomials (convert from kelvin to celsius)
     tcelsius = SST - 273.15_fp
     tcelsius_sq = tcelsius * tcelsius     !quadratic
     tcelsius_cu = tcelsius_sq * tcelsius  !cubic

     !Define two relaxation frequencies, f1 and f2
     f1 = c(1) + c(2) * tcelsius + c(3) * tcelsius_sq
     f2 = c(4) + c(5) * tcelsius + c(6) * tcelsius_sq + c(7) * tcelsius_cu

     !Static permittivity estatic = del1+del2+einf
     del1 = c(8)  + c(9)  * tcelsius + c(10) * tcelsius_sq + c(11) * tcelsius_cu
     del2 = c(12) + c(13) * tcelsius + c(14) * tcelsius_sq + c(15) * tcelsius_cu
     einf = c(18) + c(19) * tcelsius

     freq_ghz    = Frequency
     freq_ghz_sq = freq_ghz * freq_ghz

     !-----------------------------------------------------
     !1.2 calculate permittivity using double-debye formula
     !-----------------------------------------------------

     fen          = 2.0_fp * c(20) * freq_ghz * 0.001_fp
     fen_sq       = fen*fen
     den1         = ONE + fen_sq * f1 * f1
     den2         = ONE + fen_sq * f2 * f2
     perm_real1   = del1 / den1
     perm_real2   = del2 / den2
     perm_imag1   = del1 * fen * f1 / den1
     perm_imag2   = del2 * fen * f2 / den2
     perm_free    = 8.854E-03_fp
     sigma        = 2.906_fp + 0.09437_fp * tcelsius
     perm_imag3   = sigma / (2.0_fp * c(20) * perm_free * freq_ghz)
     perm_Real    = perm_real1 + perm_real2 + einf
     perm_imag    = perm_imag1 + perm_imag2 + perm_imag3
     permittivity = Cmplx(perm_Real,perm_imag,fp)

     !-------------------------------------------------------------
     !1.3 calculate complex reflection coefficients and corrections
     !-------------------------------------------------------------


     !1.3.1) Fresnel reflection coefficients
     !------

     perm1          = sqrt(permittivity - sinzen_sq)
     perm2          = permittivity * coszen
     rhth           = (coszen-perm1) / (coszen+perm1)
     rvth           = (perm2-perm1) / (perm2+perm1)
     !    fresnel_v_real = dble(rvth)
     fresnel_v_Real = Real(rvth)
     fresnel_v_imag = Aimag(rvth)
     fresnel(1)     = fresnel_v_Real * fresnel_v_Real + &
          & fresnel_v_imag * fresnel_v_imag
     !    fresnel_h_real = dble(rhth)
     fresnel_h_Real = Real(rhth)
     fresnel_h_imag = Aimag(rhth)
     fresnel(2)      = fresnel_h_Real * fresnel_h_Real + &
          & fresnel_h_imag * fresnel_h_imag
     fresnel(3)      = ZERO
     fresnel(4)      = ZERO

     !1.3.2) Small scale correction to reflection coefficients
     !------

     If (freq_ghz >= 15.0) Then
        small_rough_cor = Exp( c(21) * wind10 * coszen_sq / (freq_ghz_sq) )
     Else
        small_rough_cor = 1.0
     End If

     !1.3.3) Large scale geometric correction
     !------

     ! If the coefficent file contains FASTEM 2 it contains
     ! also FASTEM 1 but the version choosen is given
     ! by Fastem_Version

     !Point to correct coefficients for this version. There are 36 altogether.
     !Those for FASTEM-2/3 are stored in section 24:59 of the array, those for
     !FASTEM1 in section 60:95.
     If ( Fastem_Version == 2 .or. Fastem_Version == 3 ) Then
        c => Fastem3_Coef(24:59)
     Else
        c => Fastem3_Coef(60:95)
     End If
     Do j = 1, 12
        zc(j) = c(j*3-2) + c(j*3-1)*freq_ghz + c(j*3)*freq_ghz_sq
     End Do
     !Point back to all coefficients again
     c => Fastem3_Coef

     large_rough_cor(1) = &
          & (zc(1)                  + &
           & zc(2) * seczen    + &
           & zc(3) * seczen_sq + &
           & zc(4) * wind10         + &
           & zc(5) * wind10_sq      + &
           & zc(6) * windsec) / HUNDRED
     large_rough_cor(2) = &
          & (zc(7)                   + &
           & zc(8)  * seczen    + &
           & zc(9)  * seczen_sq + &
           & zc(10) * wind10         + &
           & zc(11) * wind10_sq      + &
           & zc(12) * windsec) / HUNDRED
     large_rough_cor(3) = ZERO
     large_rough_cor(4) = ZERO

     ! Introduce emiss_v_save and emiss_h_save arrays to be able
     ! to simplify further AD code
     emiss_save(:) = 1.0 - fresnel(:) * small_rough_cor + large_rough_cor(:)

     !Apply foam correction
     foam_cor(1)  = c(22) * ( wind10 ** c(23) )
     foam_cor(2)  = c(22) * ( wind10 ** c(23) )
     !Currently ignore foam effects on 3rd and 4th elements.
     foam_cor(3)  = ZERO
     foam_cor(4)  = ZERO

     emissstokes(:) = emiss_save(:) - foam_cor(:)*emiss_save(:) + foam_cor(:)

     ! Only apply non-specular correction for Fastem-3 if theta < 60 degrees
     If ((Fastem_Version == 2 .or. (Fastem_Version == 3 .And. seczen <= 2.0_fp)) .And. &
       & Transmittance < 0.9999_fp .And. Transmittance > 0.00001_fp ) Then

        !Convert windspeed to slope variance using the Cox and Munk model
        variance = 0.00512_fp * wind10 + 0.0030_fp
        varm     = variance * c(138)
        variance = varm * ( c(139) * freq_ghz + c(140) )

        test_variance = variance
        If ( variance > varm ) Then
           variance    = varm
        Endif
        If ( variance < ZERO  ) Then
           variance    = ZERO
        Endif

        !Compute surface to space optical depth
        opdpsfc    = -log(Transmittance) / seczen

        !Define nine predictors for the effective angle calculation
        zx(1) = ONE
        zx(2) = variance
        zx(4) = ONE / coszen
        zx(3) = zx(2) * zx(4)
        zx(5) = zx(3) * zx(3)
        zx(6) = zx(4) * zx(4)
        zx(7) = zx(2) * zx(2)
        zx(8) = log(opdpsfc)
        zx(9) = zx(8) * zx(8)

        zrough_v = ONE
        zrough_h = ONE

        Do jcof = 1,7
           jcofm1 = jcof-1
           !Switched h to v Deblonde SSMIS june 7, 2001
           zrough_h = zrough_h + &
                & zx(jcof) * ( c(96+jcofm1*3) &
                & + zx(8)    *   c(97+jcofm1*3) &
                & + zx(9)    *   c(98+jcofm1*3) )
           zrough_v = zrough_v + &
                & zx(jcof) * ( c(117+jcofm1*3) &
                & + zx(8)    *   c(118+jcofm1*3) &
                & + zx(9)    *   c(119+jcofm1*3) )
        End Do
        zreflmod_v = (ONE-Transmittance**zrough_v) / (ONE-Transmittance)
        zreflmod_h = (ONE-Transmittance**zrough_h) / (ONE-Transmittance)

     End If

        !.......end of forward part....................................
        !
        ! * Now run adjoint code of fastem
        !
     ! Only apply non-specular correction for Fastem-3 if theta < 60 degrees
     If ((Fastem_Version == 2 .or. (Fastem_Version == 3 .And. coszen > 0.5_fp)) .And. &
          & Transmittance < 0.9999_fp .And. Transmittance > 0.00001_fp ) Then

        zreflmod_v_ad = reflectstokes_ad(1) * (ONE-emissstokes(1))
        zreflmod_h_ad = reflectstokes_ad(2) * (ONE-emissstokes(2))
        emissstokes_ad(4) = emissstokes_ad(4) - reflectstokes_ad(4)
        emissstokes_ad(3) = emissstokes_ad(3) - reflectstokes_ad(3)
        emissstokes_ad(2) = emissstokes_ad(2) - reflectstokes_ad(2) * zreflmod_h
        emissstokes_ad(1) = emissstokes_ad(1) - reflectstokes_ad(1) * zreflmod_v
        zrough_h_ad = - zreflmod_h_ad * &
              & ( Transmittance**zrough_h * Log(Transmittance) ) / &
              & (ONE-Transmittance)

        Transmittance_AD = Transmittance_AD + zreflmod_h_ad *&
          & (-zrough_h * Transmittance**(zrough_h-ONE) * &
                        & (ONE-Transmittance) +  &
          &     ( ONE-TRansmittance**zrough_h)          ) &
          & / (ONE-Transmittance)**2

        zrough_v_ad = -zreflmod_v_ad * &
          & ( Transmittance**zrough_v * Log(Transmittance) ) / &
          & (ONE-Transmittance)

        Transmittance_AD = Transmittance_AD + zreflmod_v_ad *&
          & (-zrough_v * Transmittance**(zrough_v-ONE) * &
                  & (ONE-Transmittance) +  &
          &    ( ONE-Transmittance**zrough_v)         ) &
          & / (ONE-Transmittance)**2

        zx_ad(:) = ZERO
        Do jcof = 1,7
           jcofm1 = jcof-1
           !Switched h to v Deblonde SSMIS june 7, 2001



           zx_ad(9) = zx_ad(9) + zrough_v_ad * zx(jcof) * c(119+jcofm1*3)
           zx_ad(8) = zx_ad(8) + zrough_v_ad * zx(jcof) * c(118+jcofm1*3)
           zx_ad(jcof) = zrough_v_ad *&
                 & (          c(117+jcofm1*3)   &
                  & + zx(8) * c(118+jcofm1*3)   &
                  & + zx(9) * c(119+jcofm1*3) )

           zx_ad(9) = zx_ad(9) + zrough_h_ad * zx(jcof) * c(98+jcofm1*3)
           zx_ad(8) = zx_ad(8) + zrough_h_ad * zx(jcof) * c(97+jcofm1*3)
           zx_ad(jcof) = zx_ad(jcof) + zrough_h_ad *&
                 & (             c(96+jcofm1*3)   &
                  & + zx(8)  *   c(97+jcofm1*3)   &
                   & + zx(9)  *   c(98+jcofm1*3) )

        End Do
        zrough_v_ad = ZERO
        zrough_h_ad = ZERO

        !Define nine predictors for the effective angle calculation
        zx_ad(8) = zx_ad(8) + zx_ad(9) * 2 * zx(8)

        opdpsfc_ad = zx_ad(8) / opdpsfc
        zx_ad(2) = zx_ad(2) + zx_ad(7) * 2 * zx(2)

        zx_ad(4) = zx_ad(4) + zx_ad(6) * 2 * zx(4)

        zx_ad(3) = zx_ad(3) + zx_ad(5) * 2 * zx(3)

        zx_ad(2) = zx_ad(2) + zx_ad(3) * zx(4)

        zx_ad(4) = ZERO

        variance_ad = zx_ad(2)

        zx_ad(1) = ZERO

        !Compute surface to space optical depth
        Transmittance_AD = Transmittance_AD - opdpsfc_ad /&
              & ( Transmittance_AD * seczen )

        If ( test_variance < varm ) Then
           varm_ad = variance_ad * ( c(139) * freq_ghz + c(140) )
        Else
           varm_ad = variance_ad
        Endif

        variance_ad = varm_ad * c(138)
        wind10_ad = wind10_ad + variance_ad * 0.00512_fp
     Else
        emissstokes_ad(:) =  emissstokes_ad(:) - reflectstokes_ad(:)
     End If

     If ( Fastem_Version == 3 .AND. Sat_Azimuth_Angle >= (-360.0)) then
       ! Add azimuthal component from Fuzhong Weng (NOAA/NESDIS) based on work by Dr. Gene Poe (NRL)
       ! Angle between wind direction and satellite azimuthal view angle
       ! Assume 19m wind = 10m wind for now (fix later).
       phi = PI - (wind10_direction-Sat_AZimuth_Angle)*pi/180.0_fp
       u19=wind10
       Do ich = 0,15
          a1e = c(141+ich*12) + u19*(c(142+ich*12)+ u19*(c(143+ich*12)+u19*c(144+ich*12)))
          a2e = c(145+ich*12) + u19*(c(146+ich*12)+ u19*(c(147+ich*12)+u19*c(148+ich*12)))
          a3e = c(149+ich*12) + u19*(c(150+ich*12)+ u19*(c(151+ich*12)+u19*c(152+ich*12)))

          i_freq = int(ich/4) + 1    ! 37, 19, 10, 7 GHz
          j_stokes = mod(ich,4) + 1
          tbfixed(j_stokes,i_freq,1) = a1e
          tbfixed(j_stokes,i_freq,2) = a2e
          tbfixed(j_stokes,i_freq,3) = a3e
       End Do

       Do M=1,3
          Do istokes=1,4
             efixed(1,istokes,M)= tbfixed(istokes,4,M)  ! 7   GHz
             efixed(2,istokes,M)= tbfixed(istokes,3,M)  ! 10  GHz
             efixed(3,istokes,M)= tbfixed(istokes,2,M)  ! 19  GHz
             efixed(4,istokes,M)= tbfixed(istokes,1,M)  ! 37  GHz
          End Do

          ! Interpolate results to required frequency based on 7, 10, 19, 37 GHz
          If (freq_ghz.le.freqfixed(1)) Then
            einterpolated(:,M)=efixed(1,:,M)
          Else If(freq_ghz.ge.freqfixed(4)) then
            einterpolated(:,M)=efixed(4,:,M)
          Else
            If(freq_ghz.lt.freqfixed(2)) ifreq=2
            If(freq_ghz.lt.freqfixed(3).and.freq_ghz.ge.freqfixed(2)) ifreq=3
            If(freq_ghz.ge.freqfixed(3)) ifreq=4
            dfreq=(freq_ghz-freqfixed(ifreq-1))/(freqfixed(ifreq)-freqfixed(ifreq-1))
            einterpolated(:,M)=efixed(ifreq-1,:,M)+dfreq*(efixed(ifreq,:,M)-efixed(ifreq-1,:,M))
          EndIf
       EndDo

       Do istokes = 1,4
          azimuthal_emiss=ZERO
          Do M=1,3
             If(istokes.le.2) Then
                azimuthal_emiss=azimuthal_emiss+einterpolated(istokes,M)*cos(m*phi)*(ONE-coszen)&
               &/(ONE - 0.6018_fp)
             Else
                azimuthal_emiss=azimuthal_emiss+einterpolated(istokes,M)*sin(m*phi)*(ONE-coszen)&
               &/(ONE - 0.6018_fp)
             End If
          End Do
          emissstokes(istokes)=emissstokes(istokes)+azimuthal_emiss
       End Do

       azimuthal_emiss_ad = ZERO
       phi_ad             = ZERO
       Do istokes=1,4
          azimuthal_emiss_ad=emissstokes_ad(istokes)
          Do M=1,3
             If(istokes.le.2) Then
                einterpolated_ad(istokes,M)=azimuthal_emiss_ad*cos(m*phi)*(ONE-coszen)/&
               &(ONE - 0.6018_fp)
                phi_ad= phi_ad - azimuthal_emiss_ad*einterpolated(istokes,M)*m*sin(m*phi)*(ONE-coszen)/&
               &(ONE - 0.6018_fp)
             Else
                einterpolated_ad(istokes,M)=azimuthal_emiss_ad*sin(m*phi)*(ONE-coszen)/(ONE - 0.6018_fp)
                phi_ad= phi_ad + azimuthal_emiss_ad*einterpolated(istokes,M)*m*cos(m*phi)*(ONE-coszen)/&
               &(ONE - 0.6018_fp)
             End If
          Enddo
       End Do

       efixed_ad(:,:,:) = ZERO
       Do M=1,3
          If (freq_ghz.le.freqfixed(1)) Then
             efixed_ad(1,:,M)=efixed_ad(1,:,M)+einterpolated_ad(:,M)
          Else If(freq_ghz.ge.freqfixed(4)) then
             efixed_ad(4,:,M)=efixed_ad(4,:,M)+einterpolated_ad(:,M)
          Else
             If(freq_ghz.lt.freqfixed(2)) ifreq=2
             If(freq_ghz.lt.freqfixed(3).and.freq_ghz.ge.freqfixed(2)) ifreq=3
             If(freq_ghz.ge.freqfixed(3)) ifreq=4
             dfreq=(freq_ghz-freqfixed(ifreq-1))/(freqfixed(ifreq)-freqfixed(ifreq-1))
             efixed_ad(ifreq,:,M)=efixed_ad(ifreq,:,M)+einterpolated_ad(:,M)*dfreq
             efixed_ad(ifreq-1,:,M)=efixed_ad(ifreq-1,:,M)+einterpolated_ad(:,M)*(1.0-dfreq)
          End If

          Do istokes=1,4
             tbfixed_ad(istokes,4,M)= efixed_ad(1,istokes,M)  ! 7   GHz
             tbfixed_ad(istokes,3,M)= efixed_ad(2,istokes,M)  ! 10  GHz
             tbfixed_ad(istokes,2,M)= efixed_ad(3,istokes,M)  ! 19  GHz
             tbfixed_ad(istokes,1,M)= efixed_ad(4,istokes,M)  ! 37  GHz
          End Do
       End Do

       u19_ad = ZERO
       Do ich = 0,15
          i_freq = int(ich/4) + 1    ! 37, 19, 10, 7 GHz
          j_stokes = mod(ich,4) + 1
          a3e_ad = tbfixed_ad(j_stokes,i_freq,3)
          a2e_ad = tbfixed_ad(j_stokes,i_freq,2)
          a1e_ad = tbfixed_ad(j_stokes,i_freq,1)
          u19_ad = u19_ad + a3e_ad*(c(150+ich*12)+u19*(2.0*c(151+ich*12)+3.0*u19*c(152+ich*12)))
          u19_ad = u19_ad + a2e_ad*(c(146+ich*12)+u19*(2.0*c(147+ich*12)+3.0*u19*c(148+ich*12)))
          u19_ad = u19_ad + a1e_ad*(c(142+ich*12)+u19*(2.0*c(143+ich*12)+3.0*u19*c(144+ich*12)))
       End Do
       wind10_ad = wind10_ad + u19_ad
       wind10_direction_ad = -phi_ad*pi/180.0_fp
     End If

     ! Be careful do TL first because the next 2 lines of the direct model
     ! have variables in input/output of the statement

     foam_cor_ad = ZERO
     Do Ich=1,4
        foam_cor_ad   = foam_cor_ad + emissstokes_ad(ich) * (ONE - emiss_save(ich))
        emissstokes_ad(Ich) = emissstokes_ad(ich) * (ONE - foam_cor(ich))
     End Do

     !Apply foam correction
     wind10_ad = wind10_ad + foam_cor_ad *&
           & c(22) * c(23) * ( wind10 ** (c(23)-ONE) )

     !1.3.3) Large scale geometric correction
     !------
     IF ( Fastem_Version <= 2 .or. (Fastem_Version == 3 .And. coszen >= 0.5_fp)) THEN
       fresnel_v_ad          = -emissstokes_ad(1) * small_rough_cor
       small_rough_cor_ad    = -emissstokes_ad(1) * fresnel(1)
       large_rough_cor_ad(1) =  emissstokes_ad(1)

       fresnel_h_ad          = -emissstokes_ad(2) * small_rough_cor
       small_rough_cor_ad    =  small_rough_cor_ad - emissstokes_ad(2) * fresnel(2)
       large_rough_cor_ad(2) =  emissstokes_ad(2)
     ELSE
       fresnel_v_ad          = -emissstokes_ad(1)
       fresnel_h_ad          = -emissstokes_ad(2)     
     END IF
     
     windsec_ad   =             large_rough_cor_ad(2) * zc(12) / HUNDRED
     wind10_sq_ad =             large_rough_cor_ad(2) * zc(11) / HUNDRED
     wind10_ad    = wind10_ad + large_rough_cor_ad(2) * zc(10) / HUNDRED


     windsec_ad   = windsec_ad   + large_rough_cor_ad(1) * zc(6) / HUNDRED
     wind10_sq_ad = wind10_sq_ad + large_rough_cor_ad(1) * zc(5) / HUNDRED
     wind10_ad    = wind10_ad    + large_rough_cor_ad(1) * zc(4) / HUNDRED


     !1.3.2) Small scale correction to reflection coefficients
     !------

     If (freq_ghz >= 15.0_fp) Then
        wind10_ad = wind10_ad + small_rough_cor_ad *&
             & small_rough_cor * c(21) * coszen_sq / (freq_ghz_sq)
     Else
        small_rough_cor    = 1.0
        small_rough_cor_AD = 0.0
     End If

     !1.3.1) Fresnel reflection coefficients
     !------

     fresnel_h_real_ad = fresnel_h_ad * 2 * fresnel_h_real
     fresnel_h_imag_ad = fresnel_h_ad * 2 * fresnel_h_imag

     rhth_ad = CMPLX(fresnel_h_real_ad, -fresnel_h_imag_ad,fp)

     fresnel_v_real_ad = fresnel_v_ad * 2 * fresnel_v_real
     fresnel_v_imag_ad = fresnel_v_ad * 2 * fresnel_v_imag

     rvth_ad = CMPLX(fresnel_v_real_ad, -fresnel_v_imag_ad,fp)

     perm1_ad = - rvth_ad * 2 * perm2 / (perm2+perm1)**2
     perm2_ad =   rvth_ad * 2 * perm1 / (perm2+perm1)**2

     perm1_ad = perm1_ad - rhth_ad * 2 * coszen / (coszen+perm1)**2

     permittivity_ad = perm2_ad * coszen

     permittivity_ad = permittivity_ad + perm1_ad * 0.5_fp / perm1

     !-----------------------------------------------------
     !1.2 calculate permittivity using double-debye formula
     !-----------------------------------------------------

     perm_Real_ad =  Real(  permittivity_ad )
     perm_imag_ad = -Aimag( permittivity_ad )

     perm_imag1_ad = perm_imag_ad
     perm_imag2_ad = perm_imag_ad
     perm_imag3_ad = perm_imag_ad

     einf_ad       = perm_real_ad
     perm_real1_ad = perm_real_ad
     perm_real2_ad = perm_real_ad

     sigma_ad = perm_imag3_ad / (2.0_fp * c(20) * perm_free * freq_ghz)
     tcelsius_ad = 0.09437_fp * sigma_ad

     del2_ad =  perm_imag2_ad * fen * den2 * f2  / (den2 * den2)
     den2_ad = -perm_imag2_ad * fen * del2 * f2  / (den2 * den2)
     f2_ad   =  perm_imag2_ad * fen * den2 * del2/ (den2 * den2)

     del1_ad =  perm_imag1_ad * fen * den1 * f1  / (den1 * den1)
     den1_ad = -perm_imag1_ad * fen * del1 * f1  / (den1 * den1)
     f1_ad   =  perm_imag1_ad * fen * den1 * del1/ (den1 * den1)


     del2_ad = del2_ad + perm_real2_ad * den2 / (den2 * den2)
     den2_ad = den2_ad - perm_real2_ad * del2 / (den2 * den2)

     del1_ad = del1_ad + perm_real1_ad * den1 / (den1 * den1)
     den1_ad = den1_ad - perm_real1_ad * del1 / (den1 * den1)


     f2_ad = f2_ad + den2_ad * 2 * fen_sq * f2
     f1_ad = f1_ad + den1_ad * 2 * fen_sq * f1

     !Static permittivity estatic = del1+del2+einf
     tcelsius_ad    = tcelsius_ad + c(19) * einf_ad
     tcelsius_ad    = tcelsius_ad + del2_ad * c(13)
     tcelsius_sq_ad = del2_ad * c(14)
     tcelsius_cu_ad = del2_ad * c(15)

     tcelsius_ad    = tcelsius_ad    + del1_ad * c(9)
     tcelsius_sq_ad = tcelsius_sq_ad + del1_ad * c(10)
     tcelsius_cu_ad = tcelsius_cu_ad + del1_ad * c(11)


     !Define two relaxation frequencies, f1 and f2
     tcelsius_ad    = tcelsius_ad    + f2_ad * c(5)
     tcelsius_sq_ad = tcelsius_sq_ad + f2_ad * c(6)
     tcelsius_cu_ad = tcelsius_cu_ad + f2_ad * c(7)

     tcelsius_ad    = tcelsius_ad    + f1_ad * c(2)
     tcelsius_sq_ad = tcelsius_sq_ad + f1_ad * c(3)


     !Set values for temperature polynomials (convert from kelvin to celsius)
     tcelsius_ad    = tcelsius_ad + tcelsius_cu_ad * 3 * tcelsius_sq

     tcelsius_ad    = tcelsius_ad + tcelsius_sq_ad * 2 * tcelsius
     SST_AD = SST_AD + tcelsius_ad

     wind10_ad = wind10_ad + windsec_ad * seczen

     wind10_ad = wind10_ad + wind10_sq_ad * 2 * wind10

     Wind_Speed_AD = wind10_AD
     Wind_Direction_AD = wind10_direction_ad 


  END SUBROUTINE Fastem3_AD

END MODULE CRTM_Fastem3
