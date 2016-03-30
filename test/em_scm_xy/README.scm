This is a single column model test case.

README updated 9 Feb. 2009.

-------------------------------------------------------------------------

Author: Josh Hacker, NCAR

-------------------------------------------------------------------------

Please communicate your changes so that we can all benefit from model
improvements.

-------------------------------------------------------------------------

Description:  The SCM-WRF runs on a 3x3 stencil with periodic lateral
boundary conditions in X and Y.  No horizontal gradients are supported
in this configuration so advection is absent unless explicitly imposed.
The formulation for advection in the current version, coded in
module_scm_force.F, is an upstream relaxation following Ghan et al. (1999).

-------------------------------------------------------------------------

Initialization: As with the other test cases, initialization is handled
with text files input to ideal.exe (via module_initialize_scm_xy.F).
Here we use both files input_sounding and input_soil.  The format for
input_sounding is the surface followed by the sounding:

z_terrain u_10 v_10 t_2 q_2 psfc
z u v theta qv
. . .   .    .
. . .   .    .
. . .   .    .

Here z is in meters, u and v are in m/s, theta is in K, and qv is in
kg^3/kg^3.  The number of levels does not matter as long as z_top <=
the top level in input_sounding.  The format for input soil is the skin
and deep soil temperatures followed by the soil level data:

0.0  TSK   TMN
z    SOILT SOILM
.    .     .
.    .     .
.    .     .

Here z is positive downward, in meters.  TSK, SOILT, and TMN are in K,
and SOILM is the volumetric soil moisture fraction.  Initialization of
the soil uses the modules for real-data cases with your specified LSM,
so use wisely.

-------------------------------------------------------------------------

Forcing:  Solar forcing will follow the dates as specified in the
namelist.  All other forcing (U_g, V_g, W, and advection) are specified
in a file that comes in via an auxilliary input.  For example:

&time_control
auxinput3_inname                    = "force_ideal.nc"
auxinput3_interval_h                = 59

This file looks very much like wrfbdy_d01 and in fact is used in the
same way, with values specified at the intervals and tendencies on
those values valid for the period within the corresponding interval.
Thus time-dependent forcing on time-dependent height coordinates is
possible; one simply needs to use multiple intervals.

A script to produce a two-time level (i.e. constant) forcing is
available, and is called make_scm_forcing.ncl.  See the documentation
at the beginning of this.  The example that comes with the release is
documented below.  I have a much more sophisticated script to create
forcing from a series of met_em files.  Please contact me if you would
like to work with me on that (hacker at ucar.edu).

-------------------------------------------------------------------------

Namelist: For namelist parameters, see README.namelist.  Non-SCM namelist
parameters that must be set to get the expected behavior are:

&dynamics
pert_coriolis                       = .true.,

&bdy_control
periodic_x                          = .true.,
periodic_y                          = .true.,
(all others false)

-------------------------------------------------------------------------

Example:  This example is based on the GABLS II intercomparison case.
It it not the GABLS II intercomparison case for a number of reasons,
but the two most important are (1) no advection, and (2) it does not
specify surface fluxes for forcing.  The advection in GABLES is specified
differently and this approach was not implemented in this version.
Also, code to force with surface fluxes still needs to be developed.
Again, please communicate with me if you would like to do this.

