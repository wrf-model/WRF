MODULE module_wkppc_constants


! A few constants 

     REAL, PARAMETER  ::  navgdro = 6.022e23   ! molecules/mol

     REAL, PARAMETER  ::   &   !molecular weights
           mwh = 1.0079,  mwo = 15.9994,  mwair = 28.97


     REAL, PARAMETER :: mwh2o = 2*mwh + mwo


!dens2con air
      REAL, PARAMETER ::          dens2con_a = 1.e-3     &! kg/m3 -> g/cm3
                                   * (1./mwair)          &! -> mole/cm3
                                   * navgdro              ! -> molec/cm3
   


!dens2con water
      REAL, PARAMETER ::          dens2con_w = 1.e-3     &! kg/m3 -> g/cm3
                                    * (1./mwh2o)         &! -> mole/cm3
                                    * navgdro             ! -> molec/cm3







! constants for KPP
! should be moved to namelist input in the future 
! additional settings in ./inc/kpp_ctrl_default.inc  

 
     REAL, PARAMETER ::  rtols=1.E-3 ! 1e-2 means two digits 
     REAL, PARAMETER ::  atols=1.





END MODULE module_wkppc_constants
