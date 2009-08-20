! WRF:MEDIATION_LAYER:FIRE_MODEL

!*** Jan Mandel August 2007 - February 2008
!*** email: jmandel@ucar.edu or Jan.Mandel@gmail.com or Jan.Mandel@cudenver.edu

! For support please subscribe to the wrf-fire mailing list at NCAR at
! http://mailman.ucar.edu/mailman/listinfo/wrf-fire
!
! ALL RESPONSES TO INQUIRIES ABOUT THIS CODE WILL BE COPIED TO THE wrf-fire LIST

! This module is the only entry point from WRF-ARW to the wildland 
! fire model. The call to sfire_driver advances the fire model by 
! one timestep. The fire model inputs the wind and outputs 
! temperature and humidity tendencies. The fire model also inputs a 
! number of constant arrays (fuel data, topography). Additional 
! arguments are model state (for data assimilation) and constant arrays 
! the model gives to WRF for safekeeping because it is not allowed 
! to save anything.

! This model is described in [1]. The fire model is coupled with WRF 
! but the fire code itself is not dependent on WRF in any way other 
! than calls to few WRF utilities from module_fr_sfire_util. This 
! model uses a level set function method for advancing the fireline. 
! It is a reimplementation of an earlier model, which used fireline 
! propagation by tracers and was coupled with the Clark-Hall 
! atmospheric code, described in [2]. For WRF documentation see [3].

! Acknowledgements: Contributions to the level set method by Mijeong 
! Kim. The fire physics is adapted from an earlier code by Terry  
! L. Clark, Janice L. Coen, and Don Latham. The coupling with WRF is 
! adapted from a code by Ned Patton for coupling of the earlier fire
! model with WRF, with contributions by Jonathan D. Beezley. The
! WRF build and execution environment was set up by Jonathan Beezley.

! [1] Jan Mandel, Jonathan D. Beezley, Janice L. Coen, and Minjeong Kim,
! Data Asimilation for Wildland Fires: Ensemble Kalman filters in 
! coupled atmosphere-surface models, IEEE Control Systems Magazine, 
! submitted, 2007

! [2] T. L. Clark, J. Coen, and D. Latham, Description of a coupled 
! atmosphere-fire model, Intl. J. Wildland Fire, vol. 13, pp. 49-64, 
! 2004
!
! [3] http://www.mmm.ucar.edu/wrf/OnLineTutorial/Introduction/index.html

#define DEBUG_OUT

module module_fr_sfire_driver

use module_model_constants, only: cp,xlv
use module_fr_sfire_model
use module_fr_sfire_phys
use module_fr_sfire_atm
use module_fr_sfire_util

contains


subroutine sfire_driver_em ( grid , config_flags                   & 
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                        &
            ,ifms,ifme, jfms,jfme                        &
            ,ifps,ifpe, jfps,jfpe                       &
            ,rho,z_at_w,dz8w                            &
)
!*** purpose: driver from grid structure

! Driver layer modules
    USE module_domain
    USE module_configure
    USE module_driver_constants
    USE module_machine
    USE module_tiles
    USE module_dm

    implicit none
!*** arguments
    TYPE(domain) , TARGET          :: grid   ! data
    !  Structure that contains run-time configuration (namelist) data for domain
    TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags
    integer, intent(in):: &
             ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                        &
            ,ifms,ifme, jfms,jfme                        &
            ,ifps,ifpe, jfps,jfpe
    real,intent(in),dimension(ims:ime, kms:kme, jms:jme)::rho,  &! air density  (kg/m^3) (cell based, atm grid) 
                                z_at_w,dz8w                 ! ????????

!*** local
    INTEGER:: fire_num_ignitions
    integer, parameter::fire_max_ignitions=5
    REAL, DIMENSION(fire_max_ignitions)::  fire_ignition_start_x, &
        fire_ignition_start_y, &
        fire_ignition_end_x, &
        fire_ignition_end_y, &
        fire_ignition_time, &
        fire_ignition_radius
    integer::fire_ifun,fire_ifun_start,ir,jr,fire_ignition_longlat,istep,itimestep
    logical::need_lfn_update
    !real, dimension(ifms:ifme, jfms:jfme)::uf,vf,lfn_out  
    ! uf vf only do not need to be in the state but we need halo on them
    real, dimension(ifms:ifme, jfms:jfme)::lfn_out  
    real::lat_ctr,lon_ctr

    CALL wrf_error_fatal ( 'WRF Fire code not released' )

end subroutine sfire_driver_em

end module module_fr_sfire_driver
