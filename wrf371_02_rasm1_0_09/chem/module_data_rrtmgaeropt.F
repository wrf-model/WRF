
MODULE module_data_rrtmgaeropt
!
      IMPLICIT NONE
      INTEGER nswbands,nlwbands   ! wave bands for rrtmg radiation scheme
      PARAMETER (nswbands =4,nlwbands=16)
      
!*************************************************************
!czhao hard coding the refractive index of water and aerosols
!
! * Most of the wavelength refractive indices below are based on values
!   used in the Community Atmosphere Model (CAM)
! * For now, shortwave refractive index is not wavelength depenedent
!   and set to 0.003 as described in Zhao et al. ACP (2010)
! * Wavelength dependant shortwave refractive index used by CAM is 
!   commented out for now
!
      !water
      real,dimension(1:nswbands),save ::  refrwsw,refiwsw
      real,dimension(1:nlwbands),save ::  refrwlw,refiwlw
      data refrwsw /1.35,1.34,1.33,1.33/
      data refiwsw /1.524e-8,2.494e-9,1.638e-9,3.128e-6/
      data refrwlw /1.532,1.524,1.420,1.274,1.161,1.142,1.232,1.266,1.296, & 
      1.321,1.342,1.315,1.330,1.339,1.350,1.408/ 
      data refiwlw / 0.336,0.360,0.426,0.403,0.321,0.115,0.0471,0.039,0.034, &
      0.0344,0.092,0.012,0.013,0.01,0.0049,0.0142/

      !dust
      real,dimension(1:nswbands),save ::  refrsw_dust,refisw_dust
      real,dimension(1:nlwbands),save ::  refrlw_dust,refilw_dust
      !data refrsw_dust /nswbands*1.530/
      data refrsw_dust /nswbands*1.550/
!     data refisw_dust /0.024,0.0135,0.0063,0.004/
      data refisw_dust /nswbands*0.003/
      data refrlw_dust /2.340,2.904,1.748,1.508,1.911,1.822,2.917,1.557, &
      1.242,1.447,1.432,1.473,1.495,1.5,1.5,1.51/
      data refilw_dust /0.7,0.857,0.462,0.263,0.319,0.26,0.65,0.373,0.093, &
      0.105,0.061,0.0245,0.011,0.008,0.0068,0.018/ 

      !BC
      real,dimension(1:nswbands),save ::  refrsw_bc,refisw_bc
      real,dimension(1:nlwbands),save ::  refrlw_bc,refilw_bc
      data refrsw_bc /nswbands*1.95/
      data refisw_bc /nswbands*0.79/
      data refrlw_bc /nlwbands*1.95/
      data refilw_bc /nlwbands*0.79/

      !OC
      real,dimension(1:nswbands),save ::  refrsw_oc,refisw_oc
      real,dimension(1:nlwbands),save ::  refrlw_oc,refilw_oc
      !data refrsw_oc /1.53,1.53,1.53,1.52/
      data refrsw_oc /nswbands*1.45/
      !data refisw_oc /0.00776,0.005,0.00567,0.0156/
      data refisw_oc /nswbands*0.0/
      data refrlw_oc /1.86,1.91,1.988,1.439,1.606,1.7,1.888,2.489,1.219, &
      1.419,1.426,1.446,1.457,1.458,1.455,1.443/ 
      data refilw_oc /0.5,0.268,0.185,0.198,0.059,0.0488,0.11,0.3345,0.065, &
      0.058,0.0261,0.0142,0.013,0.01,0.005,0.0057/
   
      !Sea-salt
      real,dimension(1:nswbands),save ::  refrsw_seas,refisw_seas
      real,dimension(1:nlwbands),save ::  refrlw_seas,refilw_seas
      data refrsw_seas /1.51,1.5,1.5,1.47/
      data refisw_seas /0.866e-6,7.019e-8,1.184e-8,0.00015/
      data refrlw_seas /1.74,1.76,1.78,1.456,1.41,1.48,1.56,1.63,1.4,1.43, &
      1.56,1.45,1.485,1.486,1.48,1.48 / 
      data refilw_seas /0.1978,0.1978,0.129,0.038,0.019,0.014,0.016,0.03,0.012, &
      0.0064,0.0196,0.0029,0.0017,0.0014,0.0014,0.00176/
  
      !Sulfate 
      real,dimension(1:nswbands),save ::  refrsw_sulf,refisw_sulf
      real,dimension(1:nlwbands),save ::  refrlw_sulf,refilw_sulf
      !data refrsw_sulf /1.468,1.442,1.43,1.422/
      data refrsw_sulf /nswbands*1.52/
      data refisw_sulf /3*1.0e-9,1.75e-6/
      data refrlw_sulf /1.89,1.91,1.93,1.586,1.678,1.758,1.855,1.597,1.15, & 
      1.26,1.42,1.35,1.379,1.385,1.385,1.367/
      data refilw_sulf /0.22,0.152,0.0846,0.2225,0.195,0.441,0.696,0.695, & 
      0.459,0.161,0.172,0.14,0.12,0.122,0.126,0.158/

!*************************************************************
      !wavelength
      real, save :: wavmin(nswbands) ! Min wavelength (um) of interval
      !data wavmin /3.077,2.500,2.150,1.942,1.626,1.299, &
      data wavmin /0.25,0.35,0.55,0.998/
      real, save :: wavmax(nswbands) ! Max wavelength (um) of interval
      !data wavmax/3.846,3.077,2.500,2.150,1.942,1.626, &
      data wavmax/0.35,0.45,0.65,1.000/
      real, save :: wavenumber1_longwave(nlwbands) !Longwave limits (cm-1)
      data wavenumber1_longwave /10.,350.,500.,630.,700.,820.,980.,1080.,1180.,1390.,1480.,1800.,2080.,2250.,2390.,2600./
      real, save :: wavenumber2_longwave(nlwbands) !Longwave limits (cm-1)
      data wavenumber2_longwave /350.,500.,630.,700.,820.,980.,1080.,1180.,1390.,1480.,1800.,2080., 2250.,2390.,2600.,3250./

      !mode or size bin
      integer,parameter :: maxd_amode=3
      integer,parameter :: ntot_amode=3
      integer,parameter :: maxd_bin=8
      integer,parameter :: ntot_bin=8

      !Chebychev polynomial
      !integer,parameter :: prefr=7,prefi=10
      integer,parameter :: prefr=7,prefi=7
      integer,parameter :: ncoef=50
      real,parameter :: rmmin=0.005e-4,rmmax=50.e-4  ! cm 
      real,save:: refrtabsw(prefr,nswbands)
      real,save:: refitabsw(prefi,nswbands)
      real,save:: refrtablw(prefr,nlwbands)
      real,save:: refitablw(prefi,nlwbands)
      !coefficients for parameterizing aerosol radiative properties
      !in terms of refractive index and wet radius
      real,save:: extpsw(ncoef,prefr,prefi,nswbands) !specific extinction
      real,save:: abspsw(ncoef,prefr,prefi,nswbands) !specific absorption
      real,save:: ascatpsw(ncoef,prefr,prefi,nswbands) !specific scattering
      real,save:: asmpsw(ncoef,prefr,prefi,nswbands) !asymmetry factor
      real,save:: sbackpsw(ncoef,prefr,prefi,nswbands)
      real,save:: pmom2psw(ncoef,prefr,prefi,nswbands) 
      real,save:: pmom3psw(ncoef,prefr,prefi,nswbands)
      real,save:: pmom4psw(ncoef,prefr,prefi,nswbands)
      real,save:: pmom5psw(ncoef,prefr,prefi,nswbands)
      real,save:: pmom6psw(ncoef,prefr,prefi,nswbands)
      real,save:: pmom7psw(ncoef,prefr,prefi,nswbands)
      real,save:: extplw(ncoef,prefr,prefi,nlwbands) !specific extinction
      real,save:: absplw(ncoef,prefr,prefi,nlwbands) !specific absorption
      real,save:: ascatplw(ncoef,prefr,prefi,nlwbands) !specific scattering
      real,save:: asmplw(ncoef,prefr,prefi,nlwbands) !asymmetry factor

      real,save :: wavmidsw(nswbands)
      data wavmidsw  / 0.30e-4, 0.40e-4, 0.60e-4 ,0.999e-04 /
      !now czhao use 0.45 instead of 0.40 becaues of incorrect AOD from 0.40
      !data wavmidsw  / 0.30e-4, 0.45e-4, 0.60e-4 ,0.999e-04 /
      real,save :: wavmidlw(nlwbands)
      complex, save :: crefwsw(nswbands) ! complex refractive index fro water
      complex, save :: crefwlw(nlwbands)



END MODULE module_data_rrtmgaeropt
