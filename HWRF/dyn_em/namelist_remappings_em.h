!WRF:DRIVER_LAYER:CORE_SPECIFIC

! Namelist remappings.  This file is included by the initial_config
! routine in frame/module_configure.F and provides em_core specific
! remappings of namelist variables

! allow fdda specific namelist variables to map onto auxinput streams 9 and 10
      model_config_rec%auxinput10_begin_d     =       model_config_rec%gfdda_begin_d
      model_config_rec%auxinput10_begin_h     =       model_config_rec%gfdda_begin_h
      model_config_rec%auxinput10_begin_m     =       model_config_rec%gfdda_begin_m
      model_config_rec%auxinput10_begin_s     =       model_config_rec%gfdda_begin_s
      model_config_rec%auxinput10_begin_y     =       model_config_rec%gfdda_begin_y
      model_config_rec%auxinput10_end_d       =       model_config_rec%gfdda_end_d
      model_config_rec%auxinput10_end_h       =       model_config_rec%gfdda_end_h
      model_config_rec%auxinput10_end_m       =       model_config_rec%gfdda_end_m
      model_config_rec%auxinput10_end_s       =       model_config_rec%gfdda_end_s
      model_config_rec%auxinput10_end_y       =       model_config_rec%gfdda_end_y
      model_config_rec%auxinput10_inname      =       model_config_rec%gfdda_inname
      model_config_rec%auxinput10_interval    =       model_config_rec%gfdda_interval
      model_config_rec%auxinput10_interval_d  =       model_config_rec%gfdda_interval_d
      model_config_rec%auxinput10_interval_h  =       model_config_rec%gfdda_interval_h
      model_config_rec%auxinput10_interval_m  =       model_config_rec%gfdda_interval_m
      model_config_rec%auxinput10_interval_s  =       model_config_rec%gfdda_interval_s
      model_config_rec%auxinput10_interval_y  =       model_config_rec%gfdda_interval_y
      model_config_rec%io_form_auxinput10     =       model_config_rec%io_form_gfdda
      model_config_rec%auxinput9_begin_d      =       model_config_rec%sgfdda_begin_d
      model_config_rec%auxinput9_begin_h      =       model_config_rec%sgfdda_begin_h
      model_config_rec%auxinput9_begin_m      =       model_config_rec%sgfdda_begin_m
      model_config_rec%auxinput9_begin_s      =       model_config_rec%sgfdda_begin_s
      model_config_rec%auxinput9_begin_y      =       model_config_rec%sgfdda_begin_y
      model_config_rec%auxinput9_end_d        =       model_config_rec%sgfdda_end_d
      model_config_rec%auxinput9_end_h        =       model_config_rec%sgfdda_end_h
      model_config_rec%auxinput9_end_m        =       model_config_rec%sgfdda_end_m
      model_config_rec%auxinput9_end_s        =       model_config_rec%sgfdda_end_s
      model_config_rec%auxinput9_end_y        =       model_config_rec%sgfdda_end_y
      model_config_rec%auxinput9_inname       =       model_config_rec%sgfdda_inname
      model_config_rec%auxinput9_interval     =       model_config_rec%sgfdda_interval
      model_config_rec%auxinput9_interval_d   =       model_config_rec%sgfdda_interval_d
      model_config_rec%auxinput9_interval_h   =       model_config_rec%sgfdda_interval_h
      model_config_rec%auxinput9_interval_m   =       model_config_rec%sgfdda_interval_m
      model_config_rec%auxinput9_interval_s   =       model_config_rec%sgfdda_interval_s
      model_config_rec%auxinput9_interval_y   =       model_config_rec%sgfdda_interval_y
      model_config_rec%io_form_auxinput9      =       model_config_rec%io_form_sgfdda
      IF (model_config_rec%prec_acc_dt(1) .gt. 0.) model_config_rec%prec_acc_opt = 1
      IF (model_config_rec%bucket_mm .gt. 0.) model_config_rec%bucketr_opt = 1
#ifdef PLANET
!***************** special conversion for timesteps *********************
! 2004-12-07 ADT Notes
! NB: P2SI needs to defined in multiple places.  Right now this
! requirement is a kludge, and if I can find something more elegant
! I will try to implement it later.
!
! Beware: dt as the namelist timestep is now obsolete.  The new
! variable "timestep" (which is an *integer* number of seconds),
! with the (optional) additional specification of a fraction (to
! make non-integer timesteps) now acts as the true timestep.
! In share/set_timekeeping.F the integer(s) are converted to a real
! number and put back in dt anyway!
! We will deal with the case of the integer variables in
! share/set_timekeeping.F itself.  For now, since they left dt in
! the namelist definition, I will leave this here just in case ...
      model_config_rec%dt    = dt    * P2SI
! All of the following variables are told to be input in *MINUTES*
! These values are converted to units of timesteps in the various
! init routines in phys/module_physics_init.F by dividing by the
! formula STEP = (xxDT*60./dt).  So it seems safe to multiply them
! by P2SI here (with the exception of adding roundoff error later).
! See notes in phys/module_radiation_driver for the radt example.
      model_config_rec%radt  = radt  * P2SI
      model_config_rec%bldt  = bldt  * P2SI
      model_config_rec%cudt  = cudt  * P2SI
      model_config_rec%gsmdt = gsmdt * P2SI
!************************************************************************
#endif

