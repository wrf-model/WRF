   !  The following parameters are used as flags to indicate either a
   !  MISSING data value (real number) in the "measurement" TYPE or an
   !  "end of data" flag (two fields with end_data  flag mean end of
   !  measurements for one observation).  The undefined values are when
   !  a GTS observation is read but determined to be in error (not according
   !  to the code).  Geopotential height is referenced as a positive value
   !  undefined, which explains the duplicate.

   INTEGER , PARAMETER                            ::  undefined1    =  888888
   REAL    , PARAMETER                            ::  undefined1_r  =  888888.
   INTEGER , PARAMETER                            ::  undefined2    = -888888
   REAL    , PARAMETER                            ::  undefined2_r  = -888888.
   INTEGER , PARAMETER                            ::  missing       = -888888
   REAL    , PARAMETER                            ::  missing_r     = -888888.
   INTEGER , PARAMETER                            ::  end_data      = -777777
   REAL    , PARAMETER                            ::  end_data_r    = -777777.

! The cutoff height for Aircraft data: 
!     h >  aircraft_cut, to retrive the pressure from standard atmosphere;
!     h <= aircraft_cut, to keep Aircraft observed height.
!          
   REAL    , PARAMETER                            ::  aircraft_cut  =    3000.

   !  Define various ways for bad data to be flagged.  Each of these is
   !  2^n, so that the errors may be summed.

   INTEGER, PARAMETER ::  &

   outside_of_domain       = 2 ** 18, & ! Data outside horizontal domain 
                                        ! or time window, data set to missing_r
   wrong_direction         = 2 ** 17, & ! Wind vector direction <0 or> 360 
                                        ! => direction set to missing_r
   negative_spd            = 2 ** 16, & ! Wind vector norm is negative 
                                        ! => norm set to missing_r
   zero_spd                = 2 ** 15, & ! Wind vector norm is zero 
                                        ! => norm set to missing_r
   wrong_wind_data         = 2 ** 14, & ! Spike in wind profile 
                                        ! =>direction and norm set to missing_r 
   zero_t_td               = 2 ** 13, & ! t or td = 0 => t or td, rh and qv 
                                        ! are set to missing_r, 
   t_fail_supa_inver       = 2 ** 12, & ! t, td, rh and qv fail vertical 
                                        ! consistency =>all are set to missing_r
   wrong_t_sign            = 2 ** 11, & ! superadiabatic and inversion 
                                        ! in temperature curve => no action
   above_model_lid         = 2 ** 10, & ! heigh above model lid
                                        ! =>Datum can be removed
   reference_atmosphere    = 2 **  7, & ! Missing h, p or t
                                        ! =>Datum interpolated from standard atm
   from_background         = 2 **  6, & ! Missing h, p or t
                                        ! =>Datum interpolated from model ref st
   convective_adjustment   = 2 **  2, & ! convective adjustement check
                                        ! =>Apply correction on t, td, rh and qv
   Hydrostatic_recover     = 3      , & ! Height from hydrostaic assumption with
                                        ! the OBS data calibration
   surface_correction      = 2 **  1, & ! Surface datum
                                        ! =>Apply correction on datum
   Reference_OBS_scaled    = 2 **  0    ! Height from reference state with OBS
                                        ! data calibration
                                        ! =>Apply correction on datum

