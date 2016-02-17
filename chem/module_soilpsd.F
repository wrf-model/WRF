MODULE module_soilpsd
! Soil particle-size distributions, based on data collected by G.H. McTainsh (g.mctainsh@griffith.edu.au) and collegues, 
! Griffith University, Australia. The data has been processed by Harry Butler (harry.butler@usq.edu.au), 
! University of Southern Queensland, Australia, and Martina Klose (mklose@uni-koeln.de), University of Cologne, Germany. 
       integer, parameter :: mmax=4

       real(8), dimension(3, mmax) :: csandm         ! Coefs for minimally dispersed, Simpson Desert, sand                             
       data csandm / 0.0287,  3.6153,  0.2775, &
      &              0.2811,  4.9918,  0.3023, &
      &              0.0516,  3.9315,  0.1417, &
      &              0.6387,  4.7173,  0.2432  /
!
       real(8), dimension(3, mmax) :: closam         ! Coefs for minimally dispersed, Eulo, loamy sand (orig sandy loam)                                 
       data closam / 0.5000,  6.0674,  0.4039, &
      &              0.1997,  4.3282,  0.3998, &
      &              0.2191,  5.2793,  0.3488, &
      &              0.0812,  6.6426,  0.2216  /
!
       real(8), dimension(3, mmax) :: csalom         ! Coefs for minimally dispersed, Tambo, sandy loam                                
       data csalom / 0.0240,  6.6566,  0.1000, &
      &              0.0536,  6.0663,  0.1227, &
      &              0.3184,  5.1840,  0.7462, &
      &              0.6039,  6.0685,  0.4063  /
!       
       real(8), dimension(3, mmax) :: csilom         ! Coefs for minimally dispersed, Thargominda, silt loam (orig loam)                                
       data csilom / 0.0278,  5.2068,  0.1921, &
      &              0.5000,  4.3275,  0.4544, &
      &              0.3054,  3.5848,  1.0721, &
      &              0.1669,  4.1432,  0.1877  /
!       
       real(8), dimension(3, mmax) :: csiltm         ! Coefs for minimally dispersed, not available, use same as for silt loam                       
       data csiltm / 0.0278,  5.2068,  0.1921, &
      &              0.5000,  4.3275,  0.4544, &
      &              0.3054,  3.5848,  1.0721, &
      &              0.1669,  4.1432,  0.1877  /
!       
       real(8), dimension(3, mmax) :: cloamm         ! Coefs for minimally dispersed, Wellington, loam                                 
       data cloamm / 0.0695,  6.9010,  0.1000, &
      &              0.1047,  6.6666,  0.1614, &
      &              0.5000,  5.5720,  0.7295, &
      &              0.3258,  6.3005,  0.3549  /
!
       real(8), dimension(3, mmax) :: csclom         ! Coefs for minimally dispersed, Yandama, sandy clay loam (orig clay loam)                               
       data csclom / 0.4999,  5.1720,  0.3064, &
      &              0.2490,  4.6158,  0.2783, &
      &              0.0139,  4.9110,  0.1000, &
      &              0.2372,  5.0185,  0.9259  /
!       
       real(8), dimension(3, mmax) :: csiclm         ! Coefs for minimally dispersed, Yandama (McDonald et al., 1990), silty clay loam, not normalized                                   
       data csiclm / 1.2597,  4.7986,  0.3751, &
      &              0.8107,  5.2549,  0.3047, &
      &              0.4482,  5.1246,  1.2550, &
      &              0.,      0.,      0.      /
!       
       real(8), dimension(3, mmax) :: ccloam         ! Coefs for minimally dispersed, Manilla, clay loam                               
       data ccloam / 0.1842,  6.3110,  0.2071, &
      &              0.4243,  6.0792,  0.4049, &
      &              0.3273,  5.5946,  0.7726, &
      &              0.0642,  6.5793,  0.1000  /
!
       real(8), dimension(3, mmax) :: csaclm         ! Coefs for minimally dispersed, Thargomindah (McDonald et al., 1990), sandy clay, not normalized                             
       data csaclm / 0.3124,  4.1426,  0.1717, &
      &              0.9564,  3.9501,  1.7750, &
      &              1.0340,  4.3088,  0.4340, &
      &              0.,      0.,      0.      /
!       
       real(8), dimension(3, mmax) :: csilcm         ! Coefs for minimally dispersed, Cooper Floodplain - Windorah, silty clay (orig light clay)                             
       data csilcm / 0.2709,  4.9160,  0.1971, &
      &              0.0631,  4.5807,  0.1635, &
      &              0.1350,  3.8960,  0.8092, &
      &              0.5310,  4.5301,  0.4887  /
! 
       real(8), dimension(3, mmax) :: cclaym         ! Coefs for minimally dispersed, Walgett, clay                                   
       data cclaym / 0.2408,  4.5855,  0.6331, &
      &              0.0594,  3.3126,  1.1665, &
      &              0.0273,  5.3894,  0.1000, &
      &              0.6725,  5.3148,  0.3924  /
!
       real(8), dimension(3, mmax) :: csandf         ! Coefs for fully dispersed, Simpson Desert, sand                                 
       data csandf / 0.0231,  3.6724,  0.2341, &
      &              0.0362,  3.9598,  0.1257, &
      &              0.2628,  4.9933,  0.2986, &
      &              0.6779,  4.7374,  0.2498  /
!
       real(8), dimension(3, mmax) :: closaf         ! Coefs for fully dispersed, Eulo, loamy sand (orig sandy loam)                     
       data closaf / 0.1354,  5.5976,  0.4288, &
      &              0.1073,  2.3499,  1.0898, &
      &              0.1692,  4.0550,  0.2113, &
      &              0.5880,  4.1982,  0.7748  /
!
       real(8), dimension(3, mmax) :: csalof         ! Coefs for fully dispersed, Tambo, sandy loam                                    
       data csalof / 0.0115,  5.3900,  0.1141, &
      &              0.3043,  4.6980,  1.0132, &
      &              0.0840,  6.6115,  0.1339, &
      &              0.6002,  6.0494,  0.3178  /
!       
       real(8), dimension(3, mmax) :: csilof         ! Coefs for fully dispersed, Thargominda, silt loam (orig loam)                                    
       data csilof / 0.1816,  3.1175,  1.0169, &
      &              0.4454,  4.3491,  0.6154, &
      &              0.0568,  4.1250,  0.1000, &
      &              0.3162,  4.1594,  0.3017  /      
      !       
       real(8), dimension(3, mmax) :: csiltf         ! Coefs for fully dispersed, not available, use same as for silt loam                                    
       data csiltf / 0.1816,  3.1175,  1.0169, &
      &              0.4454,  4.3491,  0.6154, &
      &              0.0568,  4.1250,  0.1000, &
      &              0.3162,  4.1594,  0.3017  /      
!       
       real(8), dimension(3, mmax) :: cloamf         ! Coefs for fully dispersed, Wellington, loam                                     
       data cloamf / 0.0378,  5.0205,  0.5601, &
      &              0.0511,  0.5580,  0.3886, &
      &              0.4003,  1.7677,  0.6877, &
      &              0.5108,  2.9973,  0.5489  /
!
       real(8), dimension(3, mmax) :: csclof         ! Coefs for fully dispersed, Yandama, sandy clay loam (orig clay loam)                                   
       data csclof / 0.1364,  3.3869,  1.3277, &
      &              0.0642,  6.1715,  0.3463, &
      &              0.0767,  4.4147,  0.2243, &
      &              0.7227,  4.9219,  0.4983  /
!       
       real(8), dimension(3, mmax) :: csiclf         ! Coefs for fully dispersed, Yandama (McDonald et al., 1990), silty clay loam, not normalized                                   
       data csiclf / 0.5844,  4.6079,  0.6141, &
      &              0.3304,  5.2050,  0.2897, &
      &              0.0522,  7.0553,  1.0000, &
      &              0.0330,  0.6931,  1.0000  /
!       
       real(8), dimension(3, mmax) :: ccloaf         ! Coefs for fully dispersed, Manilla, clay loam                                   
       data ccloaf / 0.3988,  2.0568,  0.9444, &
      &              0.5000,  3.5758,  0.6679, &
      &              0.0145,  4.4626,  0.1000, &
      &              0.0867,  5.0406,  0.4590  /
!       
       real(8), dimension(3, mmax) :: csaclf         ! Coefs for fully dispersed, Thargomindah (McDonald et al., 1990), sandy clay, not normalized                             
       data csaclf / 1.1285,  4.2288,  0.4296, &
      &              0.7275,  3.8558,  1.2898, &
      &              0.2438,  4.1222,  0.1379, &
      &              0.,      0.,      0.      /
!       
       real(8), dimension(3, mmax) :: csilcf         ! Coefs for fully dispersed, Cooper Floodplain - Windorah, silt clay (orig light clay)                                 
       data csilcf / 0.3927,  3.6159,  0.5702, &
      &              0.1160,  1.3131,  0.2704, &
      &              0.4588,  2.3768,  0.6913, &
      &              0.0325,  1.0150,  0.1154  /
!
       real(8), dimension(3, mmax) :: cclayf         ! Coefs for fully dispersed, Walgett, clay                                       
       data cclayf / 0.5000,  3.0166,  0.7156, &
      &              0.1124,  0.6297,  0.4159, &
      &              0.3698,  1.5791,  0.6059, &
      &              0.0178,  3.8284,  0.1509  /
! 
END MODULE module_soilpsd
