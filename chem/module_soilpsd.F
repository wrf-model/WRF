MODULE module_soilpsd
       integer, parameter :: mmax=4

       real(8), dimension(3, mmax) :: csandm         ! Coefs for sand minimally dispersed
       data csandm /0.,     0.,     0.,     &
      &             0.0329, 4.3733, 0.8590, &
      &             0.9671, 5.7689, 0.2526, &
      &             0.,     0.,     0.      /
!
       real(8), dimension(3, mmax) :: cloamm         ! Coefs for loam minimally dispersed
       data cloamm /0.1114, 4.3565, 0.4257, &
      &             0.4554, 5.1674, 0.3824, &
      &             0.4331, 5.4092, 1.0000, &
      &             0.,     0.,     0.      /
!
       real(8), dimension(3, mmax) :: csloam        ! Coefs for sandy clay loam minimally dispersed, very dusty
       data csloam /0.0722, 2.2675, 1.0000, &
      &             0.6266, 4.9654, 0.3496, &
      &             0.3012, 5.5819, 0.5893, &
      &             0.,     0.,     0.      /
!
       real(8), dimension(3, mmax) :: cclaym         ! Coefs for clay minimally dispersed
       data cclaym /0.3902, 3.5542, 1.0000, &
      &             0.2813, 4.2239, 0.2507, & 
      &             0.3286, 5.1638, 0.4632, &
      &             0.,     0.,     0.      /
!
       real(8), dimension(3, mmax) :: csandf         ! Coefs for sand fully dispersed
       data csandf /0.,     0.,     0.,     &
      &             0.0338, 0.6931, 1.0000, &
      &             0.9662, 5.6300, 0.2542, &
      &             0.,     0.,     0.      /
!
       real(8), dimension(3, mmax) :: cloamf         ! Coefs for loam fully dispersed
       data cloamf /0.5844, 4.6079, 0.6141, &
      &             0.3304, 5.2050, 0.2897, &
      &             0.0522, 7.0553, 1.0000, &
      &             0.0330, 0.6931, 1.0000  /
!
       real(8), dimension(3, mmax) :: csloaf         ! Coefs for sandy clay loam fully dispersed
       data csloaf /0.2344, 1.8079, 0.6141, &
      &             0.3634, 4.2050, 0.2897, &
      &             0.4022, 5.6553, 1.0000, &
      &             0.,     0.,     0.      /
!
       real(8), dimension(3, mmax) :: cclayf         ! Coefs for clay fully dispersed
       data cclayf /0.0872, 0.6931, 1.0000, &
      &             0.4464, 3.9323, 0.9181, &
      &             0.4665, 5.4486, 0.3916, &
      &             0.,     0.,     0.      /
! 
       real(8), dimension(3, mmax) :: cjadef         ! Coefs for fully dispersed, JADE site, loam sand
       data cjadef /0.228,  5.42,   0.350,  &
      &             0.277,  4.86,   0.595,  &
      &             0.295,  3.08,   1.050,  &
      &             0.200,  1.30,   1.400   /
! 
       real(8), dimension(3, mmax) :: cjadem         ! Coefs for minimally dispersed, JADE site, loam sand
       data cjadem /0.35,   5.40,   0.345,  &
      &             0.32,   4.63,   0.490,  &
      &             0.23,   4.10,   0.650,  &
      &             0.10,   2.75,   0.950   /
!
END MODULE module_soilpsd
