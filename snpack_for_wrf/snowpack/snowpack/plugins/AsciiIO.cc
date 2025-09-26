/*
 *  SNOWPACK stand-alone
 *
 *  Copyright WSL Institute for Snow and Avalanche Research SLF, DAVOS, SWITZERLAND
*/
/*  This file is part of Snowpack.
    Snowpack is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Snowpack is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Snowpack.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <snowpack/plugins/AsciiIO.h>
#include <snowpack/Utils.h>
#include <snowpack/snowpackCore/Canopy.h>
#include <snowpack/Constants.h>
#include <snowpack/Hazard.h>
#include <snowpack/Laws_sn.h>
#include <snowpack/snowpackCore/Metamorphism.h>
#include <snowpack/snowpackCore/Aggregate.h>

#define MAX_STRING_LENGTH 256

using namespace std;
using namespace mio;

/************************************************************
 * static section                                           *
 ************************************************************/

/// @brief Defines whether surface temperature is included in comparison; If no
///        values are provided in Master-file, they will be extrapolated
const bool AsciiIO::t_srf = false;

/// @brief Defines whether snow/ground temperature is included in comparison; If no
///        values are provided in Master-file, they will be extrapolated
const bool AsciiIO::t_gnd = false;

/************************************************************
 * non-static section                                       *
 ************************************************************/

/**
 * @page snoold_format SNOOLD single profile
 * @section snoold_structure General structure
 * The snow/soil layers file has the structure described below:
 * - a header section containing the metadata for the location;
 * - a data section containing description of the layers (if any). Please note that the layers are given from the bottom to the top.
 * - a hazard data section.
 *
 * The following points are important to remember:
 * - the ProfileDate will be used as starting date for the simulation. Therefore, make sure you have meteorological data from this point on!
 * - the number of soil and snow layers <b>must</b> be right!
 *
 * @section snoold_fields Fields definition
 * <center><table border="0">
 * <caption>initial snow profile fields description</caption>
 * <tr><td>
 * <table border="1">
 * <tr><th>Field</th><th>Description</th></tr>
 * <tr><th>YYYY</th><td>Year</td></tr>
 * <tr><th>MM</th><td>Month</td></tr>
 * <tr><th>DD</th><td>Day</td></tr>
 * <tr><th>HH</th><td>Hour</td></tr>
 * <tr><th>MI</th><td>Minutes</td></tr>
 * <tr><th>Layer_Thick</th><td>layer thickness [m]</td></tr>
 * <tr><th>T</th><td>layer temperature [K]</td></tr>
 * <tr><th>Vol_Frac_I</th><td>fractional ice volume [0-1]</td></tr>
 * <tr><th>Vol_Frac_W</th><td>fractional water volume [0-1]</td></tr>
 * <tr><th>Vol_Frac_WP</th><td>fractional preferential flow water volume [0-1]</td></tr>
 * <tr><th>Vol_Frac_V</th><td>fractional voids volume [0-1]</td></tr>
 * <tr><th>Vol_Frac_S</th><td>fractional soil volume [0-1]</td></tr>
 * <tr><th> <br></th><td> </td></tr>
 * </table></td><td><table border="1">
 * <tr><th>Field</th><th>Description</th></tr>
 * <tr><th>Rho_S</th><td>soil density [kg/m3]</td></tr>
 * <tr><th>Conduc_S</th><td>mineral phase soil thermal conductivity [w/(mK)]</td></tr>
 * <tr><th>HeatCapac_S</th><td>mineral phase soil thermal capacity [J/(kg*K)]</td></tr>
 * <tr><th>rg</th><td>grain radius [mm]</td></tr>
 * <tr><th>rb</th><td>bond radius [mm]</td></tr>
 * <tr><th>dd</th><td>dendricity [0-1]</td></tr>
 * <tr><th>sp</th><td>spericity [0-1]</td></tr>
 * <tr><th>mk</th><td>marker, see Metamorphism.cc</td></tr>
 * <tr><th>mass_hoar</th><td>mass of surface hoar []</td></tr>
 * <tr><th>ne</th><td>number of elements</td></tr>
 * <tr><th>CDot</th><td>stress change rate (initialize with 0.)</td></tr>
 * <tr><th>metamo</th><td>currently unused</td></tr>
 * </table></td></tr>
 * </table></center>
 *
 * @section snoold_example Example
 * Usually, simulations are started at a point in time when no snow is on the ground, therefore not requiring the definition of snow layers. An example is given below with two soil layers:
 * @code
 * [SNOWPACK_INITIALIZATION]
 * StationName=Davos:Baerentaelli
 * ProfileDate=1999 10 01 00 00
 * HS_Last=0.0
 * Latitude=46.701
 * Longitude=9.82
 * Altitude=2560
 * SlopeAngle=0
 * SlopeAzi=0
 * nSoilLayerData=2
 * nSnowLayerData=0
 * SoilAlbedo=0.5
 * BareSoil_z0=0.02
 * CanopyHeight=0.
 * CanopyLeafAreaIndex=0.
 * CanopyDirectThroughfall=0
 * WindScalingFactor=1.00
 * ErosionLevel=0
 * TimeCountDeltaHS=0.00
 * YYYY MM DD HH MI Layer_Thick  T  Vol_Frac_I  Vol_Frac_W  Vol_Frac_V  Vol_Frac_S Rho_S Conduc_S HeatCapac_S  rg  rb  dd  sp  mk mass_hoar ne CDot metamo
 * 1980 10 01 00 00 10 278.15 0.00 0.02 0.01 0.97 2400.0 0.3 900.0 10000 0.0 0.0 0.0 0 0.0 50 0.0 0.0
 * 1980 10 01 00 00 0.5 278.15 0.00 0.02 0.01 0.97 2400.0 0.3 900.0 10000 0.0 0.0 0.0 0 0.0 20 0.0 0.0
 * SurfaceHoarIndex
 * 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
 * DriftIndex
 * 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
 * ThreeHourNewSnow
 * 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
 * TwentyFourHourNewSnow
 * 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
 * End
 * @endcode
 *
 */

/**
 * @page pro_format PRO profiles time series
 * @section pro_structure General structure
 * The PRO profile time series file has the structure described below:
 * - a header section containing the metadata for the location;
 * - another header defining the properties and attributing them a code;
 * - a data section containing the layers' properties prefixed by the property code.
 *
 * @section pro_example Example
 * The time resolved snow profiles are stored in <i>".pro"</i> files structured as following:
 * @code
 * [STATION_PARAMETERS]
 * StationName      = Davos:Baerentaelli
 * Latitude         = 46.701
 * Longitude        = 9.82
 * Altitude         = 2560
 * SlopeAngle= 0.00
 * SlopeAzi= 0.00
 *
 * [HEADER]
 * #2012-06-11T16:37, Snowpack DEFAULT version 20120611.193 run by "bavay" (research mode)
 * 0500,Date
 *
 * Full profile, labels 0nnn (elements)
 * 0501,nElems,height [> 0: top, < 0: bottom of elem.] (cm)
 * 0502,nElems,element density (kg m-3)
 * 0503,nElems,element temperature (degC)
 * 0506,nElems,liquid water content by volume (%)
 * 0507,nElems,liquid preferential flow water content by volume (%)
 * 0508,nElems,dendricity (1)
 * 0509,nElems,sphericity (1)
 * 0510,nElems,coordination number (1)
 * 0511,nElems,bond size (mm)
 * 0512,nElems,grain size (mm)
 * 0513,nElems+1,grain type (Swiss Code F1F2F3), including SH on surface
 * 0514,3,grain type, grain size (mm), and density (kg m-3) of SH at surface
 * 0515,nElems,ice volume fraction (%)
 * 0516,nElems,air volume fraction (%)
 * 0517,nElems,stress in (kPa)
 * 0518,nElems,viscosity (GPa s)
 * 0519,nElems,soil volume fraction (%)
 * 0520,nElems,temperature gradient (K m-1)
 * 0521,nElems,thermal conductivity (W K-1 m-1)
 * 0522,nElems,absorbed shortwave radiation (W m-2)
 * 0523,nElems,viscous deformation rate (1.e-6 s-1)
 * 0530,8,position (cm) and minimum stability indices:
 *		profile type, stability class, z_Sdef, Sdef, z_Sn38, Sn38, z_Sk38, Sk38
 * 0531,nElems,deformation rate stability index Sdef
 * 0532,nElems,natural stability index Sn38
 * 0533,nElems,stability index Sk38
 * 0534,nElems,hand hardness either (N) or index steps (1)
 * 0535,nElems,optical equivalent grain size (mm)
 * 0540,nElems,bulk salinity (g/kg)
 * 0541,nElems,brine salinity (g/kg)
 * 0601,nElems,snow shear strength (kPa)
 * 0602,nElems,grain size difference (mm)
 * 0603,nElems,hardness difference (1)
 * 0604,nElems,structural stability index SSI
 * 0605,nElems,inverse texture index ITI (Mg m-4)
 * 0606,nElems,critical cut length (m)
 * 0621,nElems,dsm (for NIED only)
 * 0622,nElems,Sigdsm (for NIED only)
 * 0623,nElems,S_dsm (for NIED only)
 *
 * [DATA]
 * @endcode
 * The each data line starts with a code as described in the header followed by the number of elements (except for the date line) and
 * for each element, the value of the matching parameter. For example, the lines:
 * @code
 * 0500,10.12.1995 12:30
 * 0501,31,27.21,29.07,30.62,31.57,33.30,35.25,37.46,39.82,40.92,42.86,44.22,45.74,47.41,49.15,50.63,52.46,54.58
 * 0502,17,277.7,274.2,268.6,267.0,258.4,248.4,233.5,218.1,207.8,225.1,185.9,176.0,162.5,155.0,127.7,122.7,114.4
 * @endcode
 * provide the date and time (line starting with 0500), then the elements heights for each of the 17 elements (line starting with 0501) and the elements densities (line starting with 0502).
 *
 */

/**
 * @page met_format MET meteorological time series
 * @section met_structure General structure
 * The MET  time series of meteorological data has the structure defined below:
 * - a header  section containing the metadata for the location;
 * - another header defining the properties and their order;
 * - a data section containing the various meteorological parameters and fluxes.
 *
 * @section met_example Example
 * @code
 * [STATION_PARAMETERS]
 * StationName= Weissfluhjoch:StudyPlot_MST
 * Latitude= 46.83
 * Longitude= 9.81
 * Altitude= 2540
 * SlopeAngle= 0.00
 * SlopeAzi= 0.00
 * DepthTemp= 0
 *
 * [HEADER]
 * #2012-06-11T16:37, Snowpack DEFAULT version 20120611.193 run by "bavay" (research mode)
 * ,,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100
 * ID,Date,Sensible heat,Latent heat,Outgoing longwave radiation,Incoming longwave radiation,Net absorbed longwave radiation,Reflected shortwave radiation,Incoming shortwave radiation,Net absorbed shortwave radiation,Modelled surface albedo,Air temperature,Modeled surface temperature,Measured surface temperature,Temperature at bottom of snow or soil pack,Heat flux at bottom of snow or soil pack,Ground surface temperature,Heat flux at ground surface,Heat advected to the surface by liquid precipitation,Global solar radiation (horizontal),Global solar radiation on slope,Direct solar radiation on slope,Diffuse solar radiation on slope,Measured surface albedo,Relative humidity,Wind speed,Max wind speed at snow station or wind speed at ridge station,Wind direction at snow station,Precipitation rate at surface (solid only),Modelled snow depth (vertical),Enforced snow depth (vertical),Surface hoar size,24h Drift index (vertical),Height of new snow HN (24h vertical),3d sum of daily height of new snow (vertical),Total
snowpack mass,Eroded mass,Rain rate,Surface runoff (without soil infiltration),Sublimation,Evaporation,Temperature 1 (modelled),Temperature 1 (measured),Temperature 2 (modelled),Temperature 2 (measured),Temperature 3 (modelled),Temperature 3 (measured),Temperature 4 (modelled),Temperature 4 (measured),Temperature 5 (modelled),Temperature 5 (measured),Measured snow depth HS or Solute load at soil surface,SWE (of snowpack),Liquid Water Content (of snowpack),Profile type,Stability class,z_Sdef,Deformation rate stability index Sdef,z_Sn38,Natural stability index Sn38,z_Sk38,Skier stability index Sk38,z_SSI,Structural Stability index SSI,z_S5,Stability index S5,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Soil runoff,Internal energy change,Surface input (sum fluxes),Measured new snow density,Modeled new snow density,Crust thickness (S-slope),Measured sensible heat,Measured latent heat
 * ,,W m-2,W m-2,W m-2,W m-2,W m-2,W m-2,W m-2,W m-2,1,degC,degC,degC,degC,W m-2,degC,W m-2,W m-2,W m-2,W m-2,W m-2,W m-2,1,%,m s-1,m s-1,deg,kg m-2 h-1,cm,cm,mm,cm,cm,cm,kg m-2,kg m-2 h-1,kg m-2 h-1,kg m-2,kg m-2,kg m-2,degC,degC,degC,degC,degC,degC,degC,degC,degC,degC,cm or kg m-2,kg m-2,kg m-2,-,-,cm,1,cm,1,cm,1,cm,1,cm,1,,,,,,,,,,,,,,,,,,,,,,,,,,,,,kg m-2,kJ m-2,kJ m-2,kg m-3,kg m-3,cm,W m-2,W m-2
 *
 * [DATA]
 * 0203,01.11.1995 00:30,0.795426,-4.160588,308.899297,293.706000,-15.193297,0.000000,0.000000,0.000000,0.090000,0.000000,-0.100000,0.200000,-0.100000,-999.000000,-0.100000,-999.000000,0.000000,0.000000,0.000000,0.000000,0.000000,-999.000000,95.800000,0.800000,0.800000,278.200000,0.000000,0.00,0.00,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,0.000000,,,,,,,,,,,0.00,0.000000,0.000000,-1,-1,0.0,6.00,0.0,6.00,0.0,6.00,0.0,6.00,0.0,0.00,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,-999.000000,-16.702613,-0.0,-151.3,0.000000,,
 * @endcode
 * Data lines start with an id, followed by the date and the other fields, as shown in the header.
 *
 * @section met_keywords Keywords
 * This plugin uses the following keywords:
 * - in the [Output] section:
 *     - AVGSUM_TIME_SERIES
 *     - EXPERIMENT
 *     - METEOPATH
 *     - SNOWPATH
 *     - TS_DAYS_BETWEEN
 *     - OUT_CANOPY
 *     - OUT_HAZ
 *     - OUT_HEAT
 *     - OUT_LOAD
 *     - OUT_LW
 *     - OUT_MASS
 *     - OUT_METEO
 *     - OUT_SOILEB
 *     - OUT_STAB
 *     - OUT_SW
 *     - OUT_T
 *     - AGGREGATE_PRF
 *
 * - in the [SnowpackAdvanced] section:
 *     - HOAR_DENSITY_SURF
 *     - HOAR_MIN_SIZE_SURF
 *     - MIN_DEPTH_SUBSURF
 *     - PERP_TO_SLOPE
 *     - RESEARCH
 *     - VARIANT
 *
 */

/**
 * @page prf_format PRF profiles time series
 * @section prf_structure General structure
 * This format should make it easier to extract data out of simulated snow profiles, but it <b>does not work with soil layers</b>. Each new snow profile is <b>appended</b> to
 * the current file. Each new data block starts with commented out metadata: first a line giving the fields names and then a
 * line giving the units. Then the data lines follow until either an empty line (end of the profile for this date) or a new
 * metadata block (followed by new data).
 *
 * It starts with the date of the profile and location informations (station name, aspect, slope, etc). If there is a profile
 * for this date, then the stability indexes are provided in the second block. This second block is followed by a block about
 * the layer propreties, each starting with the layer deposition date.
 *
 * @section prf_example Example
 * @code
 * #Date,JulianDate,station,aspect,slope,Nlayers,hs,swe,lwc_sum,ts,tg
 * #-,-,-,deg,deg,1,cm,kg m-2,degC,degC
 * 1995-11-02T00:00:00,2450023.500000,Weissfluhjoch:StudyPlot_MST,0.0,0.0,6,5.1,4.2,0.0,-3.8,-0.1
 * #Stab,stab_height,stab_index,stab_class1,stab_class2
 * # ,cm,1,1,1
 * deformation,1.8,0.54,-1,-1
 * natural,1.8,6.00
 * ssi,5.1,6.00
 * S4,5.1,6.00
 * S5,0.0,6.00
 * #DepositionDate,DepositionJulianDate,Hn,Tn,gradT,rho,theta_i,theta_w,ogs,gsz,bsz,dd,sp,class,mk,hardness
 * #-,-,cm,degC,K m-1,kg m-3,1,mm,mm,mm,1,1,1,1,1
 * 1995-11-01T22:45:00,2450023.447917,1.8,-1.43,-72.4,88.0,0.096,0.000,0.1,0.1,0.3,0.96,0.49,110,0,1.0
 * 1995-11-01T23:00:00,2450023.458333,2.4,-1.89,-78.6,82.0,0.089,0.000,0.1,0.1,0.3,0.98,0.50,110,0,1.0
 * 1995-11-01T23:15:00,2450023.468750,3.0,-2.36,-77.7,80.5,0.088,0.000,0.1,0.1,0.3,0.99,0.50,110,0,1.0
 * 1995-11-01T23:30:00,2450023.479167,3.6,-2.81,-74.9,79.3,0.086,0.000,0.1,0.1,0.3,0.99,0.50,110,0,1.0
 * 1995-11-01T23:45:00,2450023.489583,4.3,-3.31,-69.4,79.0,0.086,0.000,0.1,0.1,0.3,0.99,0.50,110,0,1.0
 * 1995-11-02T00:00:00,2450023.500000,5.1,-3.77,-62.5,78.5,0.086,0.000,0.1,0.1,0.3,1.00,0.50,110,0,1.0
 * @endcode
 *
 * @section prf_keywords Keywords
 * This plugin uses the following keywords:
 * - AGGREGATE_PRF: if enabled, layers are aggregated in order to reduce their number, in the [Output] section;
 *
 */

AsciiIO::AsciiIO(const SnowpackConfig& cfg, const RunInfo& run_info)
         : setAppendableFiles(), metamorphism_model(), variant(), experiment(), sw_mode(),
           inpath(), snowfile(), i_snowpath(), outpath(), o_snowpath(),
           info(run_info), vecProfileFmt(), aggregate_prf(false),
           fixedPositions(), numberMeasTemperatures(0), maxNumberMeasTemperatures(0), numberTags(0), numberFixedSensors(0),
           totNumberSensors(0), time_zone(0.), calculation_step_length(0.), hazard_steps_between(0.), ts_days_between(0.),
           min_depth_subsurf(0.), hoar_density_surf(0.), hoar_min_size_surf(0.), enable_pref_flow(false),
           avgsum_time_series(false), useCanopyModel(false), useSoilLayers(false), research_mode(false), perp_to_slope(false), useReferenceLayer(false),
           out_heat(false), out_lw(false), out_sw(false), out_meteo(false), out_haz(false), out_mass(false), out_t(false),
           out_load(false), out_stab(false), out_canopy(false), out_soileb(false), r_in_n(false)
{
	//Defines how heights/depths of snow or/and soil temperatures are read in and output \n
	// Snowpack section
	cfg.getValue("CALCULATION_STEP_LENGTH", "Snowpack", calculation_step_length);
	cfg.getValue("CANOPY", "Snowpack", useCanopyModel);
	cfg.getValue("SNP_SOIL", "Snowpack", useSoilLayers);
	cfg.getValue("SW_MODE", "Snowpack", sw_mode);

	// Input section
	cfg.getValue("METEOPATH", "Input", inpath, IOUtils::nothrow);
	const std::string in_snowpath = cfg.get("SNOWPATH", "Input", "");
	cfg.getValue("TIME_ZONE", "Input", time_zone);

	// Output section
	cfg.getValue("AVGSUM_TIME_SERIES", "Output", avgsum_time_series, IOUtils::nothrow);
	cfg.getValue("EXPERIMENT", "Output", experiment);
	cfg.getValue("HAZARD_STEPS_BETWEEN", "Output", hazard_steps_between);
	cfg.getValue("METEOPATH", "Output", outpath, IOUtils::nothrow);
	cfg.getValue("OUT_CANOPY", "Output", out_canopy);
	cfg.getValue("OUT_HAZ", "Output", out_haz);
	cfg.getValue("OUT_HEAT", "Output", out_heat);
	cfg.getValue("OUT_LOAD", "Output", out_load);
	cfg.getValue("OUT_LW", "Output", out_lw);
	cfg.getValue("OUT_MASS", "Output", out_mass);
	cfg.getValue("OUT_METEO", "Output", out_meteo);
	cfg.getValue("OUT_SOILEB", "Output", out_soileb);
	cfg.getValue("OUT_STAB", "Output", out_stab);
	cfg.getValue("OUT_SW", "Output", out_sw);
	cfg.getValue("OUT_T", "Output", out_t);
	cfg.getValue("HARDNESS_IN_NEWTON", "Output", r_in_n, IOUtils::nothrow);
	const std::string out_snowpath = cfg.get("SNOWPATH", "Output", "");
	cfg.getValue("TS_DAYS_BETWEEN", "Output", ts_days_between);
	cfg.getValue("PROF_FORMAT", "Output", vecProfileFmt);
	cfg.getValue("AGGREGATE_PRF", "Output", aggregate_prf);
	cfg.getValue("USEREFERENCELAYER", "Output", useReferenceLayer, IOUtils::nothrow);

	// SnowpackAdvanced section
	cfg.getValue("HOAR_DENSITY_SURF", "SnowpackAdvanced", hoar_density_surf); // Density of SH at surface node (kg m-3)
	cfg.getValue("HOAR_MIN_SIZE_SURF", "SnowpackAdvanced", hoar_min_size_surf); // Minimum size to show SH on surface (mm)
	cfg.getValue("METAMORPHISM_MODEL", "SnowpackAdvanced", metamorphism_model, IOUtils::nothrow);
	cfg.getValue("MIN_DEPTH_SUBSURF", "SnowpackAdvanced", min_depth_subsurf);
	cfg.getValue("PERP_TO_SLOPE", "SnowpackAdvanced", perp_to_slope);
	cfg.getValue("RESEARCH", "SnowpackAdvanced", research_mode);
	cfg.getValue("VARIANT", "SnowpackAdvanced", variant);
	cfg.getValue("PREF_FLOW", "SnowpackAdvanced", enable_pref_flow);

	i_snowpath = (in_snowpath.empty())? inpath : in_snowpath;
	o_snowpath = (out_snowpath.empty())? outpath : out_snowpath;
}

AsciiIO& AsciiIO::operator=(const AsciiIO& source) {
	if (this != &source) {
		setAppendableFiles = source.setAppendableFiles;
		variant = source.variant;
		experiment = source.experiment;
		sw_mode = source.sw_mode;
		inpath = source.inpath;
		snowfile = source.snowfile;
		i_snowpath = source.i_snowpath;
		outpath = source.outpath;
		o_snowpath = source.o_snowpath;
		//info = source.info;
		vecProfileFmt = source.vecProfileFmt;
		aggregate_prf = source.aggregate_prf;
		fixedPositions = source.fixedPositions;
		numberMeasTemperatures = source.numberMeasTemperatures;
		maxNumberMeasTemperatures = source.maxNumberMeasTemperatures;
		numberTags = source.numberTags;
		numberFixedSensors = source.numberFixedSensors;
		totNumberSensors = source.totNumberSensors;
		time_zone = source.time_zone;
		calculation_step_length = source.calculation_step_length;
		hazard_steps_between = source.hazard_steps_between;
		ts_days_between = source.ts_days_between;
		min_depth_subsurf = source.min_depth_subsurf;
		hoar_density_surf = source.hoar_density_surf;
		hoar_min_size_surf = source.hoar_min_size_surf;
		avgsum_time_series = source.avgsum_time_series;
		useCanopyModel = source.useCanopyModel;
		useSoilLayers = source.useSoilLayers;
		research_mode = source.research_mode;
		perp_to_slope = source.perp_to_slope;
		out_heat = source.out_heat;
		out_lw = source.out_lw;
		out_sw = source.out_sw;
		out_meteo = source.out_meteo;
		out_haz = source.out_haz;
		out_mass = source.out_mass;
		out_t = source.out_t;
		out_load = source.out_load;
		out_stab = source.out_stab;
		out_canopy = source.out_canopy;
		out_soileb = source.out_soileb;
		r_in_n = source.r_in_n;
	}
	return *this;
}

/**
 * @brief This routine checks if the specified snow cover data exists
 * @param i_snowfile file containing the initial state of the snowpack
 * @param stationID
 * @return true if the file exists
 */
bool AsciiIO::snowCoverExists(const std::string& i_snowfile, const std::string& /*stationID*/) const
{
	string snofilename = getFilenamePrefix(i_snowfile, i_snowpath, false);
	if (snofilename.rfind(".snoold") == string::npos) {
		snofilename += ".snoold";
	}

	return FileUtils::fileExists(snofilename);
}

/**
 * @brief This routine reads the status of the snow cover at program start
 * @version 10.02
 * @note reads the old-styled sno-file format
 * @param i_snowfile file containing the initial state of the snowpack
 * @param stationID
 * @param SSdata
 * @param Zdata
 * @param read_salinity
 */
void AsciiIO::readSnowCover(const std::string& i_snowfile, const std::string& stationID,
                            SN_SNOWSOIL_DATA& SSdata, ZwischenData& Zdata, const bool&)
{
	string snofilename = getFilenamePrefix(i_snowfile, i_snowpath, false);
	if (snofilename.rfind(".snoold") == string::npos) {
		snofilename += ".snoold";
	}

	FILE *fin = fopen(snofilename.c_str(), "r");
	if (fin == NULL)
		throw IOException("Cannot open input profile "+snofilename, AT);

	// Header, Station Name and Julian Date
	char station_name[MAX_STRING_LENGTH];
	if (fscanf(fin, " %*s") != 0) {
		fclose(fin);
		throw InvalidFormatException("Can not read header of file "+snofilename, AT);
	}
	if (fscanf(fin, "\nStationName= %128s", station_name) != 1) {
		fclose(fin);
		throw InvalidFormatException("Can not read StationName in file "+snofilename, AT);
	}
	int YYYY, MM, DD, HH, MI, dum;
	if (fscanf(fin, "\nProfileDate= %4d %2d %2d %2d %2d", &YYYY, &MM, &DD, &HH, &MI) != 5) {
		fclose(fin);
		throw InvalidFormatException("Can not read ProfileDate in file "+snofilename, AT);
	}
	SSdata.profileDate = Date::rnd(Date(YYYY, MM, DD, HH, MI, time_zone), 1.);

	// Last checked calculated snow depth used for albedo control
	if (fscanf(fin, "\nHS_Last=%lf", &SSdata.HS_last) != 1) {
		fclose(fin);
		throw InvalidFormatException("Can not read HS_Last in file "+snofilename, AT);
	}
	double latitude, longitude, altitude;
	if (fscanf(fin, "\nLatitude=%lf", &latitude) != 1) {
		fclose(fin);
		throw InvalidFormatException("Can not read Latitude in file "+snofilename, AT);
	}
	if (fscanf(fin, "\nLongitude=%lf", &longitude) != 1) {
		fclose(fin);
		throw InvalidFormatException("Can not read Longitude in file "+snofilename, AT);
	}
	if (fscanf(fin, "\nAltitude=%lf", &altitude) != 1) {
		fclose(fin);
		throw InvalidFormatException("Can not read Altitude in file "+snofilename, AT);
	}
	double slope_angle, azi;
	if (fscanf(fin, "\nSlopeAngle=%lf", &slope_angle) != 1) {
		fclose(fin);
		throw InvalidFormatException("Can not read SlopeAngle in file "+snofilename, AT);
	}
	if (fscanf(fin, "\nSlopeAzi=%lf", &azi) != 1) {
		fclose(fin);
		throw InvalidFormatException("Can not read SlopeAzi in file "+snofilename, AT);
	}

	mio::Coords tmppos;
	tmppos.setLatLon(latitude, longitude, altitude);
	SSdata.meta.setStationData(tmppos, stationID, station_name);
	SSdata.meta.setSlope(slope_angle, azi);

	// Check consistency with radiation switch
	if ((sw_mode == "BOTH") && perp_to_slope && (SSdata.meta.getSlopeAngle() > Constants::min_slope_angle)) {
        fclose(fin);
		prn_msg(__FILE__, __LINE__, "wrn", Date(),
		        "You want to use measured albedo in a slope steeper than 3 deg  with PERP_TO_SLOPE set!");
		throw IOException("Do not generate Xdata from file "+snofilename, AT);
	}

	// Check consistency of nXLayerData
	if (fscanf(fin, "\nnSoilLayerData=%d", &dum) != 1) {
        fclose(fin);
		prn_msg(__FILE__, __LINE__, "err", Date(), "Missing 'nSoilLayerData'");
		throw InvalidFormatException("Cannot generate Xdata from file "+snofilename, AT);
	}
	if (dum < 0) {
	    fclose(fin);
		prn_msg(__FILE__, __LINE__, "err", Date(), "'nSoilLayerData' < 0 !!!");
		throw InvalidFormatException("Cannot generate Xdata from file "+snofilename, AT);
	} else if (useSoilLayers && (dum < 1)) {
	    fclose(fin);
		prn_msg(__FILE__, __LINE__, "err", Date(), "useSoilLayers set but 'nSoilLayerData' < 1 !!!");
		throw InvalidFormatException("Cannot generate Xdata from file "+snofilename, AT);
	} else if (!useSoilLayers && (dum > 0)) {
	    fclose(fin);
		prn_msg(__FILE__, __LINE__, "err", Date(), "useSoilLayers not set but 'nSoilLayerData' > 0 !!!");
		throw InvalidFormatException("Cannot generate Xdata from file "+snofilename, AT);
	}
	SSdata.nLayers = static_cast<size_t>(dum); //we checked that it is >0
	if (fscanf(fin, "\nnSnowLayerData=%d", &dum) != 1) {
	    fclose(fin);
		prn_msg(__FILE__, __LINE__, "err", Date(), "Missing 'nSnowLayerData'");
		throw InvalidFormatException("Cannot generate Xdata from file "+snofilename, AT);
	}
	if (dum < 0) {
	    fclose(fin);
		prn_msg(__FILE__, __LINE__, "err", Date(), "'nSnowLayerData' < 0  !!!");
		throw InvalidFormatException("Cannot generate Xdata from file "+snofilename, AT);
	}
	SSdata.nLayers += static_cast<size_t>(dum); //we checked that it is >0

	if (fscanf(fin, "\nSoilAlbedo=%lf", &SSdata.SoilAlb) != 1) {
        fclose(fin);
		prn_msg(__FILE__, __LINE__, "err", Date(), "Missing 'SoilAlbedo'");
		throw InvalidFormatException("Cannot generate Xdata from file "+snofilename, AT);
	}
	if (fscanf(fin, "\nBareSoil_z0=%lf", &SSdata.BareSoil_z0) != 1) {
	    fclose(fin);
		prn_msg(__FILE__, __LINE__, "err", Date(), "Missing 'BareSoil_z0'");
		throw InvalidFormatException("Cannot generate Xdata from file "+snofilename, AT);
	}
	if (SSdata.BareSoil_z0==0.) {
		prn_msg(__FILE__, __LINE__, "wrn", Date(), "'BareSoil_z0'=0 from %s, reset to 0.02", snowfile.c_str());
		SSdata.BareSoil_z0=0.02;
	}
	if (SSdata.HS_last > 0.05) {
		SSdata.Albedo = 0.9;
	} else {
		SSdata.Albedo = SSdata.SoilAlb;
	}

	if (fscanf(fin, "\nCanopyHeight=%lf",&SSdata.Canopy_Height) != 1) {
		fclose(fin);
		throw InvalidFormatException("Can not read CanopyHeight in file "+snofilename, AT);
	}
	if (fscanf(fin, "\nCanopyLeafAreaIndex=%lf",&SSdata.Canopy_LAI) != 1) {
		fclose(fin);
		throw InvalidFormatException("Can not read CanopyLeafAreaIndex in file "+snofilename, AT);
	}
	if (fscanf(fin, "\nCanopyDirectThroughfall=%lf",&SSdata.Canopy_Direct_Throughfall) != 1) {
		fclose(fin);
		throw InvalidFormatException("Can not read CanopyDirectThroughfall in file "+snofilename, AT);
	}

	if (fscanf(fin, "\nWindScalingFactor=%lf",&SSdata.WindScalingFactor) != 1) {
		fclose(fin);
		throw InvalidFormatException("Can not read WindScalingFactor in file "+snofilename, AT);
	}
	if (fscanf(fin, "\nErosionLevel=%d",&SSdata.ErosionLevel) != 1) {
		fclose(fin);
		throw InvalidFormatException("Can not read ErosionLevel in file "+snofilename, AT);
	}
	if (fscanf(fin, "\nTimeCountDeltaHS=%lf",&SSdata.TimeCountDeltaHS) != 1) {
		fclose(fin);
		throw InvalidFormatException("Can not read TimeCountDeltaHS in file "+snofilename, AT);
	}

	if (fscanf(fin,"\nYYYY") < 0) {
        fclose(fin);
		prn_msg(__FILE__, __LINE__, "err", Date(), "Failed reading layer header starting with 'YYYY'");
		throw InvalidFormatException("Cannot generate Xdata from file "+snofilename, AT);
	}
	if (fscanf(fin, "%*[^\n]") != 0) {
		fclose(fin);
		throw InvalidFormatException("Can not read header end in file "+snofilename, AT);
	}

	int nFields = 0;
	Date prev_depositionDate( 0., 1. );
	if (SSdata.nLayers > 0)
		SSdata.Ldata.resize(SSdata.nLayers, LayerData());
	for (size_t ll = 0; ll < SSdata.nLayers; ll++) {
		if ((nFields = fscanf(fin, " %d %d %d %d %d", &YYYY, &MM, &DD, &HH, &MI)) != 5) {
            fclose(fin);
			prn_msg(__FILE__, __LINE__, "err", Date(), "Failed reading date: read %d fields", nFields);
			throw InvalidFormatException("Cannot generate Xdata from file "+snofilename, AT);
		}
		SSdata.Ldata[ll].depositionDate = Date::rnd(Date(YYYY, MM, DD, HH, MI, time_zone), 1.);
		if (SSdata.Ldata[ll].depositionDate > SSdata.profileDate) {
		    fclose(fin);
			prn_msg(__FILE__, __LINE__, "err", Date(),
			        "Layer %u from bottom is younger (%s) than ProfileDate (%s) !!!",
			        ll+1, SSdata.Ldata[ll].depositionDate.toString(Date::ISO).c_str(), SSdata.profileDate.toString(Date::ISO).c_str());
			throw IOException("Cannot generate Xdata from file "+snofilename, AT);
		}
		if (SSdata.Ldata[ll].depositionDate < prev_depositionDate) {
			prn_msg(__FILE__, __LINE__, "err", Date(),
				   "Layer %d is younger (%s) than layer above (%s) !!!",
				   ll, prev_depositionDate.toString(Date::ISO).c_str(), SSdata.profileDate.toString(Date::ISO).c_str());
			throw IOException("Cannot generate Xdata from file "+snofilename, AT);
		}
		prev_depositionDate = SSdata.Ldata[ll].depositionDate;

		if ((nFields = fscanf(fin, " %lf %lf %lf %lf %lf %lf",
		                      &SSdata.Ldata[ll].hl, &SSdata.Ldata[ll].tl, &SSdata.Ldata[ll].phiIce,
		                      &SSdata.Ldata[ll].phiWater, &SSdata.Ldata[ll].phiVoids, &SSdata.Ldata[ll].phiSoil)) != 6) {
            fclose(fin);
			prn_msg(__FILE__, __LINE__, "err", Date(), "Failed reading hl etc: read %d of 6 fields", nFields);
			throw InvalidFormatException("Cannot generate Xdata from file "+snofilename, AT);
		}
		SSdata.Ldata[ll].phiWaterPref = 0.;
		if (SSdata.Ldata[ll].tl < 100.) {
			SSdata.Ldata[ll].tl = IOUtils::C_TO_K(SSdata.Ldata[ll].tl);
		}
		if ((nFields = fscanf(fin, "%lf %lf %lf", &SSdata.Ldata[ll].SoilRho, &SSdata.Ldata[ll].SoilK,
		                      &SSdata.Ldata[ll].SoilC)) != 3) {
            fclose(fin);
			prn_msg(__FILE__, __LINE__, "err", Date(), "Failed reading SoilRho etc: read %d of 3 fields", nFields);
			throw InvalidFormatException("Cannot generate Xdata from file "+snofilename, AT);
		}
		unsigned int ne_tmp;
		if ((nFields = fscanf(fin, "%lf %lf %lf %lf %hu %lf %u", &SSdata.Ldata[ll].rg, &SSdata.Ldata[ll].rb,
		                                 &SSdata.Ldata[ll].dd, &SSdata.Ldata[ll].sp, &SSdata.Ldata[ll].mk,
		                                 &SSdata.Ldata[ll].hr, &ne_tmp)) != 7) {
			fclose(fin);
			prn_msg(__FILE__, __LINE__, "err", Date(), "Failed reading rg etc: read %d of 7 fields", nFields);
			throw InvalidFormatException("Cannot generate Xdata from file "+snofilename, AT);
		}
		SSdata.Ldata[ll].ne = static_cast<size_t>(ne_tmp);
		if (SSdata.Ldata[ll].phiSoil==0. && (SSdata.Ldata[ll].rg<=0. || SSdata.Ldata[ll].rb<=0.)) { //Test only for snow layers
		    fclose(fin);
			std::stringstream ss;
			ss << "Invalid grain specification in layer " << ll+1 << " (from bottom) of file " << snofilename << ": ";
			ss << "grain radius = " << SSdata.Ldata[ll].rg << " bond radius = " << SSdata.Ldata[ll].rb;
			ss << " (they should be > 0).";
			throw InvalidArgumentException(ss.str(), AT);
		}
		if (SSdata.Ldata[ll].rg>0. && SSdata.Ldata[ll].rb >= SSdata.Ldata[ll].rg) {
			//HACK To avoid surprises in lwsn_ConcaveNeckRadius()
			SSdata.Ldata[ll].rb = Metamorphism::max_grain_bond_ratio * SSdata.Ldata[ll].rg;
			prn_msg(__FILE__, __LINE__, "wrn", Date(), "Layer %u from bottom: bond radius rb/rg larger than Metamorphism::max_grain_bond_ratio=%f (rb=%f mm, rg=%f mm)! Reset to Metamorphism::max_grain_bond_ratio",
			        ll+1, Metamorphism::max_grain_bond_ratio, SSdata.Ldata[ll].rb, SSdata.Ldata[ll].rg);
		}
		if ((nFields = fscanf(fin, "%lf %lf", &SSdata.Ldata[ll].CDot, &SSdata.Ldata[ll].metamo)) != 2) {
		    fclose(fin);
			prn_msg(__FILE__, __LINE__, "err", Date(), "Failed reading CDot etc: read %d of 2 fields", nFields);
			throw InvalidFormatException("Cannot generate Xdata from file "+snofilename, AT);
		}
		for (size_t ii = 0; ii < SnowStation::number_of_solutes; ii++) {
			if ((nFields = fscanf(fin," %lf %lf %lf %lf ",
			                      &SSdata.Ldata[ll].cIce[ii], &SSdata.Ldata[ll].cWater[ii],
			                      &SSdata.Ldata[ll].cVoids[ii], &SSdata.Ldata[ll].cSoil[ii])) != 4) {
                fclose(fin);
				prn_msg(__FILE__, __LINE__, "err", Date(),
				        "Failed reading impurity concentrations: read %d of 4 fields", nFields);
				throw InvalidFormatException("Cannot generate Xdata from file "+snofilename, AT);
			}
		}
	}

	// Read the hoar, drift, and snowfall hazard data info (Zdata, needed for flat field only)
	if (fscanf(fin,"%*s ") != 0) {
		fclose(fin);
		throw InvalidFormatException("Can not read spacing in file "+snofilename, AT);
	}
	for (size_t ii = 0; ii < 48; ii++) {
		if (fscanf(fin," %lf ", &Zdata.hoar24[ii]) != 1) {
            fclose(fin);
			prn_msg(__FILE__, __LINE__, "err", Date(), "While reading hoar data (48) !!!");
			throw InvalidFormatException("Cannot generate Xdata from file "+snofilename, AT);
		}
	}
	if (fscanf(fin,"%*s ") != 0) {
		fclose(fin);
		throw InvalidFormatException("Can not read spacing in file "+snofilename, AT);
	}
	for (size_t ii = 0; ii < 48; ii++) {
		if (fscanf(fin," %lf ", &Zdata.drift24[ii]) != 1) {
            fclose(fin);
			prn_msg(__FILE__, __LINE__, "err", Date(), "While reading drift data (48)  !!!");
			throw InvalidFormatException("Cannot generate Xdata from file "+snofilename, AT);
		}
	}
	if (fscanf(fin,"%*s ") != 0) {
		fclose(fin);
		throw InvalidFormatException("Can not read spacing in file "+snofilename, AT);
	}
	for (size_t ii = 0; ii < 144; ii++) {
		if (fscanf(fin," %lf ", &Zdata.hn3[ii]) != 1) {
            fclose(fin);
			prn_msg(__FILE__, __LINE__, "err", Date(), "While reading hn(3h) data (144) !!!");
			throw InvalidFormatException("While reading Zdata (hns3) !!!", AT);
		}
	}
	if (fscanf(fin,"%*s ") != 0) {
		fclose(fin);
		throw InvalidFormatException("Can not read spacing in file "+snofilename, AT);
	}
	for (size_t ii = 0; ii < 144; ii++) {
		if (fscanf(fin," %lf ", &Zdata.hn24[ii]) != 1) {
            fclose(fin);
			prn_msg(__FILE__, __LINE__, "err", Date(), "While reading hn(24h) data (144)  !!!");
			throw InvalidFormatException("Cannot generate Xdata from file "+snofilename, AT);
		}
	}

	SSdata.nN = 1;
	SSdata.Height = 0.;
	for (size_t ll = 0; ll < SSdata.nLayers; ll++) {
		SSdata.nN    += SSdata.Ldata[ll].ne;
		SSdata.Height += SSdata.Ldata[ll].hl;
	}

	fclose(fin);
}

/**
 * @brief This routine writes the status of the snow cover at program termination and at specified backup times
 * @note original SNOWPACK format (*.snoold)
 * @version 11.02
 * @param date current
 * @param Xdata
 * @param Zdata
 * @param forbackup dump Xdata on the go
 */
void AsciiIO::writeSnowCover(const mio::Date& date, const SnowStation& Xdata,
                             const ZwischenData& Zdata, const bool& forbackup)
{
	string snofilename = getFilenamePrefix(Xdata.meta.getStationID().c_str(), o_snowpath) + ".snoold";
	if (forbackup){
		stringstream ss;
		ss << "" << (int)(date.getJulian() + 0.5);
		snofilename += ss.str();
	}

	const vector<ElementData>& EMS = Xdata.Edata;
	std::ofstream fout;
	fout.open(snofilename.c_str(), std::ios::out);
	if (fout.fail()) {
		prn_msg(__FILE__, __LINE__, "err", date,"Cannot open profile OUTPUT file: %s", snofilename.c_str());
		throw AccessException("Cannot dump final Xdata to file "+snofilename, AT);
	}

	// Header, Station Name and Julian Day
	fout << fixed;
	fout << "[SNOWPACK_INITIALIZATION]\n";
	fout << "StationName= " << Xdata.meta.getStationName() << "\n";

	int yyyy,mm,dd,hh,mi;
	date.getDate(yyyy,mm,dd,hh,mi);
	fout << "ProfileDate= " << setfill('0') << setw(4) << yyyy << " " << setw(2) << mm << " " << setw(2) << dd << " " << setw(2) << hh << " " << setw(2) << mi << "\n";

	// Last checked calculated snow depth used for albedo control of next run
	fout << "HS_Last= " << Xdata.cH - Xdata.Ground << "\n";

	// Latitude, Longitude, Altitude, Slope Angle, Slope Azimut
	fout << "Latitude= " << fixed << std::setw(11) << std::setprecision(8) << Xdata.meta.position.getLat() << "\n";
	fout << "Longitude= "<< fixed << std::setw(11) << std::setprecision(8) << Xdata.meta.position.getLon() << "\n";
	fout << "Altitude= " << fixed << setprecision(0) <<  Xdata.meta.position.getAltitude() << "\n";
	fout << "SlopeAngle= " << fixed << setprecision(2) << Xdata.meta.getSlopeAngle() << "\n";
	fout << "SlopeAzi= " << fixed << setprecision(2) << Xdata.meta.getAzimuth() << "\n";

	// Number of Soil Layer Data; in case of no soil used to store the erosion level
	fout << "nSoilLayerData= " << Xdata.SoilNode << "\n";
	// Number of Snow Layer Data
	fout << "nSnowLayerData= " << Xdata.getNumberOfElements() - Xdata.SoilNode << "\n";

	// Ground Characteristics (introduced June 2006)
	fout << "SoilAlbedo= " << setprecision(2) << Xdata.SoilAlb << "\n";
	fout << "BareSoil_z0= " << setprecision(3) << Xdata.BareSoil_z0 << "\n";
	// Canopy Characteristics
	fout << "CanopyHeight= " << setprecision(2) << Xdata.Cdata.height << "\n";
	fout << "CanopyLeafAreaIndex= " << setprecision(6) << Xdata.Cdata.lai << "\n";
	fout << "CanopyDirectThroughfall= " << setprecision(2) << Xdata.Cdata.direct_throughfall << "\n";
	// Additional parameters
	fout << "WindScalingFactor= " << Xdata.WindScalingFactor << "\n";
	fout << "ErosionLevel= " << Xdata.ErosionLevel << "\n";
	fout << "TimeCountDeltaHS= " << Xdata.TimeCountDeltaHS << "\n";

	// Layer Data
	fout << "YYYY MM DD HH MI Layer_Thick           T  Vol_Frac_I  Vol_Frac_W  Vol_Frac_V";
	fout << "  Vol_Frac_S    Rho_S Conduc_S HeatCapac_S         rg        rb        dd        sp";
	fout << "    mk    mass_hoar  ne           CDot         metamo";
	for (size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
		fout << "             cIce            cWater              cAir             cSoil";
	}
	fout << "\n";
	const size_t nrElems = Xdata.getNumberOfElements();
	for (size_t e = 0; e < nrElems; e++) {
		int YYYY, MM, DD, HH, Min;
		EMS[e].depositionDate.getDate(YYYY, MM, DD, HH, Min);

		fout << noshowpoint << noshowpos << setfill('0');
		fout << setw(4) << YYYY << " " << setw(2) << MM << " " << setw(2) << DD << " " << setw(2) << HH << " " << setw(2) << Min << " ";

		fout << setfill(' ') << showpoint << showpos;
		fout << setw(11) << setprecision(6) << EMS[e].L << " " << Xdata.Ndata[e+1].T << " " << setw(19) << setprecision(14) << EMS[e].theta[ICE] << " " << EMS[e].theta[WATER] << " " << EMS[e].theta[AIR];
		fout << setw(19) << setprecision(14) << EMS[e].theta[SOIL] << " " << setw(8) << setprecision(1) << EMS[e].soil[SOIL_RHO] << " " << EMS[e].soil[SOIL_K] << " " << setw(11) << setprecision(1) << EMS[e].soil[SOIL_C] << " " << setw(10) << setprecision(6) << EMS[e].rg << " " << setw(9) << setprecision(6) << EMS[e].rb << " " << EMS[e].dd << " " << EMS[e].sp << " " << setw(6) << EMS[e].mk << " " << setw(12) << setprecision(6) << Xdata.Ndata[e+1].hoar << "    1";
		fout << " " << setw(14) << setprecision(6) << EMS[e].CDot << " " << EMS[e].metamo;

		for (size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
			fout << " " << setw(16) << setprecision(6) << EMS[e].conc(ICE,ii) << " " << setw(17) << setprecision(6) << EMS[e].conc(WATER,ii) << " " << EMS[e].conc(AIR,ii) << " " << EMS[e].conc(SOIL,ii);
		}
		fout << "\n";
	}

	fout << setfill(' ') << showpoint << showpos << setw(10) << setprecision(6);
	// Print out the hoar hazard data info, contained in Zdata (needed for flat field only)
	fout << "SurfaceHoarIndex\n";
	for (size_t ii = 0; ii < 48; ii++) {
		fout << " " << Zdata.hoar24[ii] << " ";
	}
	// Print out the drift hazard data info
	fout << "\nDriftIndex\n";
	for (size_t ii = 0; ii < 48; ii++) {
		fout << " " << Zdata.drift24[ii] << " ";
	}
	// Print out the 3 hour new snowfall hazard data info
	fout << "\nThreeHourNewSnow\n";
	for (size_t ii = 0; ii < 144; ii++) {
		fout << " " << Zdata.hn3[ii] << " ";
	}
	// Print out the 24 hour new snowfall hazard data info
	fout << "\nTwentyFourHourNewSnow\n";
	for (size_t ii = 0; ii < 144; ii++) {
		fout << " " << Zdata.hn24[ii] << " ";
	}
	fout << "\nEnd";

	fout.close();
}

// complete filename_prefix
std::string AsciiIO::getFilenamePrefix(const std::string& fnam, const std::string& path, const bool addexp) const
{
	//TODO: read only once (in constructor)
	const string filename_prefix = path + "/" + fnam;

	if (addexp && (experiment != "NO_EXP")) //NOTE usually, experiment == NO_EXP in operational mode
		return filename_prefix + "_" + experiment;

	return filename_prefix;
}

/**
 * @brief Write the Snow Profile Results, snow depth being taken VERTICALLY
 * Prepare Output File for JAVA visualization (SNOWPACK format, *.pro)
 * @note Parameters marked by an asterisk are available in RESEARCH visualisation only!
 * @version 12.04
 * @param i_date the current date
 * @param Xdata
 */
void AsciiIO::writeProfile(const mio::Date& i_date, const SnowStation& Xdata)
{
	for (size_t ii=0; ii<vecProfileFmt.size(); ii++) {
		if (vecProfileFmt[ii] == "PRO") {
			writeProfilePro(i_date, Xdata, aggregate_prf);
		} else if (vecProfileFmt[ii] == "PRF") {
			writeProfilePrf(i_date, Xdata, aggregate_prf);
		} else if (vecProfileFmt[ii] == "IMIS") {
			;
		} else {
			throw InvalidArgumentException("Key PROF_FORMAT in section [Output] takes only PRO, PRF or IMIS formats", AT);
		}
	}
}

void AsciiIO::writeProfilePro(const mio::Date& i_date, const SnowStation& Xdata, const bool& /*aggregate*/)
{
//TODO: optimize this method. For high-res outputs, we spend more than 50% of the time in this method...
	const string filename( getFilenamePrefix(Xdata.meta.getStationID(), outpath) + ".pro" );
	const size_t nN = Xdata.getNumberOfNodes();
	const size_t nE = nN-1;
	const vector<ElementData>& EMS = Xdata.Edata;
	const vector<NodeData>& NDS = Xdata.Ndata;

	//Check whether file exists, if so check whether data can be appended
	//or file needs to be deleted
	if (FileUtils::fileExists(filename)) {
		const bool append = appendFile(filename, i_date, "pro");
		if (!append && remove(filename.c_str()) != 0)
			prn_msg(__FILE__, __LINE__, "msg-", Date(), "Could not work on file %s", filename.c_str());
	}

	if (!checkHeader(Xdata, filename, "pro", "[STATION_PARAMETERS]")) {
		prn_msg(__FILE__, __LINE__, "err", i_date,"Checking header in file %s", filename.c_str());
		throw IOException("Cannot dump profiles in " + filename, AT);
	}

	std::ofstream fout(filename.c_str(),  std::ios::out | std::ofstream::app);
	if (fout.fail()) {
		prn_msg(__FILE__, __LINE__, "err", i_date,
			   "Cannot open profile series file: %s", filename.c_str());
		throw IOException("Cannot dump profiles in " + filename + "for visualisation", AT);
	}

	fout << "\n0500," << i_date.toString(Date::DIN);
	const double cos_sl = Xdata.cos_sl;
	const bool no_snow = (nE == Xdata.SoilNode);

	// Are we using sea ice variant? Check if the object is defined via the pointer:
	const bool SeaIce = (Xdata.Seaice==NULL)?(false):(true);
	// Offset profile [m]:
	const double offset = (SeaIce)?(4.):(0.);
	// Check reference level: either a marked reference level, or, if non existent, the sea level (if sea ice module is used), otherwise 0:
	const double ReferenceLevel = (  Xdata.findMarkedReferenceLayer()==IOUtils::nodata || !useReferenceLayer  )  ?  (  (Xdata.Seaice==NULL)?(0.):(Xdata.Seaice->SeaLevel)  )  :  (Xdata.findMarkedReferenceLayer()  - Xdata.Ground);
	// Number of fill elements for offset (only 0 or 1 is supported now):
	const size_t Noffset = (SeaIce)?(1):(0);

	//  501: height [> 0: top, < 0: bottom of elem.] (cm)
	const size_t nz = (useSoilLayers)? nN : nE;
	if(nE==0) {
		fout << "\n0501,1,0";
		fout.close();
		return;
	} else {
		fout << "\n0501," << nz + Noffset;
	}
	if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << M_TO_CM(offset - ReferenceLevel/cos_sl);
	for (size_t n = nN-nz; n < nN; n++) {
		if (SeaIce) {
			//Correct for sea level:
			fout << "," << std::fixed << std::setprecision(2) << M_TO_CM((NDS[n].z+NDS[n].u - NDS[Xdata.SoilNode].z - ReferenceLevel)/cos_sl + offset);
		} else {
			fout << "," << std::fixed << std::setprecision(2) << M_TO_CM((NDS[n].z+NDS[n].u - NDS[Xdata.SoilNode].z)/cos_sl);
		}
	}
	// 0502: element density (kg m-3)
	fout << "\n0502," << nE + Noffset;
	if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
	for (size_t e = 0; e < nE; e++)
		fout << "," << std::fixed << std::setprecision(1) << EMS[e].Rho;
	// 0503: element temperature (degC)
	fout << "\n0503," << nE + Noffset;
	if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
	for (size_t e = 0; e < nE; e++)
		fout << "," << std::fixed << std::setprecision(2) << IOUtils::K_TO_C(EMS[e].Te);
	// 0504: element ID
	fout << "\n0504," << nE;
	for (size_t e = 0; e < nE; e++)
		fout << "," << std::fixed << std::setprecision(0) << EMS[e].ID;
	// 0506: liquid water content by volume (%)
	fout << "\n0506," << nE + Noffset;
	if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
	for (size_t e = 0; e < nE; e++)
		fout << "," << std::fixed << std::setprecision(1) << 100.*EMS[e].theta[WATER];
	// 0507: liquid preferential flow water content by volume (%)
	if(enable_pref_flow) {
		fout << "\n0507," << nE + Noffset;
		if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
		for (size_t e = 0; e < nE; e++)
			fout << "," << std::fixed << std::setprecision(3) << 100.*EMS[e].theta[WATER_PREF];
	}
	// 0508: snow dendricity (1)
	if (no_snow) {
		fout << "\n0508,1,0";
	} else {
		fout << "\n0508," << nE-Xdata.SoilNode + Noffset;
		if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
		for (size_t e = Xdata.SoilNode; e < nE; e++)
			fout << "," << std::fixed << std::setprecision(2) << EMS[e].dd;
	}
	// 0509: snow sphericity (1)
	if (no_snow) {
		fout << "\n0509,1,0";
	} else {
		fout << "\n0509," << nE-Xdata.SoilNode + Noffset;
		if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
		for (size_t e = Xdata.SoilNode; e < nE; e++)
			fout << "," << std::fixed << std::setprecision(2) << EMS[e].sp;
	}
	// 0510: snow coordination number (1)
	if (no_snow) {
		fout << "\n0510,1,0";
	} else {
		fout << "\n0510," << nE-Xdata.SoilNode + Noffset;
		if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
		for (size_t e = Xdata.SoilNode; e < nE; e++)
			fout << "," << std::fixed << std::setprecision(1) << EMS[e].N3;
	}
	// 0511: snow bond size (mm)
	if (no_snow) {
		fout << "\n0511,1,0";
	} else {
		fout << "\n0511," << nE-Xdata.SoilNode + Noffset;
		if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
		for (size_t e = Xdata.SoilNode; e < nE; e++)
			fout << "," << std::fixed << std::setprecision(2) << 2.*EMS[e].rb;
	}
	// 0512: snow grain size (mm)
	if (no_snow) {
		fout << "\n0512,1,0";
	} else {
		fout << "\n0512," << nE-Xdata.SoilNode + Noffset;
		if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
		for (size_t e = Xdata.SoilNode; e < nE; e++)
			fout << "," << std::fixed << std::setprecision(2) << 2.*EMS[e].rg;
	}
	// 0513: snow grain type (Swiss code F1F2F3), dumps either 1,0 or 1,660 if no snow on the ground!
	fout << "\n0513," << nE+1-Xdata.SoilNode + Noffset;
	if (Noffset == 1) fout << "," << std::fixed << std::setfill ('0') << std::setw (3) << 0;
	for (size_t e = Xdata.SoilNode; e < nE; e++)
		fout << "," << std::fixed << std::setfill ('0') << std::setw (3) << EMS[e].type;
	// surface hoar at surface? (depending on boundary conditions)
	if (M_TO_MM(NDS[nN-1].hoar/hoar_density_surf) > hoar_min_size_surf) {
		fout << ",660";
		// 0514: grain type, grain size (mm), and density (kg m-3) of SH at surface
		fout << "\n0514,3";
		fout << ",660," << std::fixed << std::setprecision(1) << M_TO_MM(NDS[nN-1].hoar/hoar_density_surf);
		fout << "," << std::fixed << std::setprecision(0) << hoar_density_surf;
	} else {
		fout << ",0";
		fout << "\n0514,3,-999,-999.0,-999.0";
	}
	// 0515: ice volume fraction (%)
	fout << "\n0515," << nE + Noffset;
	if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
	for (size_t e = 0; e < nE; e++)
		fout << "," << std::fixed << std::setprecision(0) << 100.*EMS[e].theta[ICE];
	// 0516: air volume fraction (%)
	fout << "\n0516," << nE + Noffset;
	if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
	for (size_t e = 0; e < nE; e++)
		fout << "," << std::fixed << std::setprecision(0) << 100.*EMS[e].theta[AIR];
	// 0517: stress (kPa)
	fout << "\n0517," << nE + Noffset;
	if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
	for (size_t e = 0; e < nE; e++)
		fout << "," << std::scientific << std::setprecision(3) << 1.e-3*EMS[e].C;
	// 0518: viscosity (GPa s)
	fout << "\n0518," << nE + Noffset;
	if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
	for (size_t e = 0; e < nE; e++)
		fout << "," << std::scientific << std::setprecision(3) << 1.e-9*EMS[e].k[SETTLEMENT];
	// 0519: soil volume fraction (%)
	fout << "\n0519," << nE + Noffset;
	if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
	for (size_t e = 0; e < nE; e++)
		fout << "," << std::fixed << std::setprecision(0) <<100.*EMS[e].theta[SOIL];
	// 0520: temperature gradient (K m-1)
	fout << "\n0520," << nE + Noffset;
	if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
	for (size_t e = 0; e < nE; e++)
		fout << "," << std::scientific << std::setprecision(3) << EMS[e].gradT;
	// 0521: thermal conductivity (W K-1 m-1)
	fout << "\n0521," << nE + Noffset;
	if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
	for (size_t e = 0; e < nE; e++)
		fout << "," << std::scientific << std::setprecision(3) << EMS[e].k[TEMPERATURE];
	// 0522: snow absorbed shortwave radiation (W m-2)
	if (no_snow) {
		fout << "\n0522,1,0";
	} else {
		fout << "\n0522," << nE-Xdata.SoilNode + Noffset;
		if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
		for (size_t e = Xdata.SoilNode; e < nE; e++)
			fout << "," << std::fixed << std::setprecision(1) << EMS[e].sw_abs;
	}
	// 0523: snow viscous deformation rate (1.e-6 s-1)
	if (no_snow) {
		fout << "\n0523,1,0";
	} else {
		fout << "\n0523," << nE-Xdata.SoilNode + Noffset;
		if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
		for (size_t e = Xdata.SoilNode; e < nE; e++)
			fout << "," << std::fixed << std::setprecision(1) << 1.e6*EMS[e].Eps_vDot;
	}
	// 0530: position (cm) and minimum stability indices
	fout << "\n0530,8";
	fout << "," << std::fixed << +Xdata.S_class1 << "," << +Xdata.S_class2; //force printing type char as numerica value
	fout << "," <<  std::setprecision(1) << M_TO_CM(Xdata.z_S_d/cos_sl) << "," << std::setprecision(2) << Xdata.S_d;
	fout << "," << std::fixed << std::setprecision(1) << M_TO_CM(Xdata.z_S_n/cos_sl) << "," << std::setprecision(2) <<  Xdata.S_n;
	fout << "," << std::setprecision(1) << M_TO_CM(Xdata.z_S_s/cos_sl) << "," << std::fixed << std::setprecision(2) << Xdata.S_s;
	// 0531: deformation rate stability index Sdef
	if (no_snow) {
		fout << "\n0531,1,0";
	} else {
		fout << "\n0531," << nE-Xdata.SoilNode + Noffset;
		if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
		for (size_t e = Xdata.SoilNode; e < nE; e++)
			fout << "," << std::fixed << std::setprecision(2) << EMS[e].S_dr;
	}
	// 0532: natural stability index Sn38
	if (no_snow) {
		fout << "\n0532,1,0";
	} else {
		fout << "\n0532," << nE-Xdata.SoilNode + Noffset;
		if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
		for (size_t e = Xdata.SoilNode;  e < nE; e++)
			fout << "," << std::fixed << std::setprecision(2) << NDS[e+1].S_n;
	}
	// 0533: stability index Sk38
	if (no_snow) {
		fout << "\n0533,1,0";
	} else {
		fout << "\n0533," << nE-Xdata.SoilNode + Noffset;
		if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
		for (size_t e = Xdata.SoilNode; e < nE; e++)
			fout << "," << std::fixed << std::setprecision(2) << NDS[e+1].S_s;
	}
	// 0534: hand hardness ...
	if (no_snow) {
		fout << "\n0534,1,0";
	} else {
		fout << "\n0534," << nE-Xdata.SoilNode + Noffset;
		if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
		if (r_in_n) { // ... either converted to newtons according to the ICSSG 2009
			for (size_t e = Xdata.SoilNode; e < nE; e++)
				fout << "," << std::fixed << std::setprecision(1) << -1.*(19.3*pow(EMS[e].hard, 2.4));
		} else { // ... or in index steps (1)
			for (size_t e = Xdata.SoilNode; e < nE; e++)
				fout << "," << std::fixed << std::setprecision(1) << -EMS[e].hard;
		}
	}
	if (Xdata.Seaice!=NULL) {
		// 0540: bulk salinity (g/kg)
		if (no_snow) {
			fout << "\n0540,1,0";
		} else {
			fout << "\n0540," << nE-Xdata.SoilNode + Noffset;
			if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
			for (size_t e = Xdata.SoilNode; e < nE; e++)
				fout << "," << std::fixed << std::setprecision(2) << EMS[e].salinity;
		}
		// 0541: bulk salinity (g/kg)
		if (no_snow) {
			fout << "\n0541,1,0";
		} else {
			fout << "\n0541," << nE-Xdata.SoilNode + Noffset;
			if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
			for (size_t e = Xdata.SoilNode; e < nE; e++)
				fout << "," << std::fixed << std::setprecision(2) << ((EMS[e].theta[WATER] == 0.) ? (mio::IOUtils::nodata) : (EMS[e].salinity / EMS[e].theta[WATER]));
		}
	}
	// 0535: optical equivalent grain size OGS (mm)
	if (no_snow) {
		fout << "\n0535,1,0";
	} else {
		fout << "\n0535," << nE-Xdata.SoilNode + Noffset;
		if (Noffset == 1) fout << "," << std::fixed << std::setprecision(2) << mio::IOUtils::nodata;
		for (size_t e = Xdata.SoilNode; e < nE; e++)
			fout << "," << std::fixed << std::setprecision(2) << EMS[e].ogs;
	}
	if (variant == "CALIBRATION")
		writeProfileProAddCalibration(Xdata, fout);
	else
		writeProfileProAddDefault(Xdata, fout);

	fout.close();
}

/**
 * @brief Default: dump special profiles to *.pro output file
 * @author Charles Fierz
 * @version 10.04
 * @param Xdata
 * @param *fout Output file
 */
void AsciiIO::writeProfileProAddDefault(const SnowStation& Xdata, std::ofstream &fout)
{
	const size_t nE = Xdata.getNumberOfElements();
	const vector<ElementData>& EMS = Xdata.Edata;
	const vector<NodeData>& NDS = Xdata.Ndata;

	if (out_load) {
		// 06nn: e.g. solute concentration
		for (size_t jj = 2; jj < N_COMPONENTS-1; jj++) {
			for (size_t ii = 0; ii < Xdata.number_of_solutes; ii++) {
				fout << "\n06" << std::fixed << std::setfill('0') << std::setw(2) << 10*jj + ii << "," << nE-Xdata.SoilNode;
				for (size_t e = Xdata.SoilNode; e < nE; e++) {
					fout << "," << std::fixed << std::setprecision(1) << EMS[e].conc(ii,jj);
				}
			}
		}
	} else if (nE > Xdata.SoilNode) { // snow on the ground
		// 0600-profile specials
		// 0601: snow shear strength (kPa)
		fout << "\n0601," << nE-Xdata.SoilNode;
		for (size_t e = Xdata.SoilNode; e < nE; e++)
			fout << "," << std::fixed << std::setprecision(2) << EMS[e].s_strength;
		// 0602: grain size difference (mm)
		fout << "\n0602," << nE-Xdata.SoilNode;
		for (size_t e = Xdata.SoilNode; e < nE-1; e++)
			fout << "," << std::fixed << std::setprecision(2) << 2.*fabs(EMS[e].rg - EMS[e+1].rg);
		fout << ",0.";
		// 0603: hardness difference (1)
		fout << "\n0603," << nE-Xdata.SoilNode;
		for (size_t e = Xdata.SoilNode; e < nE-1; e++)
			fout << "," << std::fixed << std::setprecision(2) << fabs(EMS[e].hard - EMS[e+1].hard);
		fout << ",0.";
		// 0604: structural stability index SSI
		fout << "\n0604," << nE-Xdata.SoilNode;
		for (size_t e = Xdata.SoilNode; e < nE; e++)
			fout << "," << std::fixed << std::setprecision(2) << NDS[e+1].ssi;
		// 0605: inverse texture index ITI (Mg m-4)
		fout << "\n0605," << nE-Xdata.SoilNode;
		for (size_t e = Xdata.SoilNode; e < nE; e++) {
			if (EMS[e].dd < 0.005)
				fout << "," << std::fixed << std::setprecision(1) << -1.*EMS[e].Rho/(2.*MM_TO_M(EMS[e].rg));
			else
				fout << "," << std::fixed << std::setprecision(1) << 0.;
		}
		// 0606: critical cut length (m)
		fout << "\n0606," << nE-Xdata.SoilNode;
		for (size_t e = Xdata.SoilNode; e < nE; e++) {
			fout << "," << std::fixed << std::setprecision(2) << EMS[e].crit_cut_length;
		}
		if (metamorphism_model == "NIED") {
			// 0621: Dry snow metamorphism factor
			fout << "\n0621," << nE-Xdata.SoilNode;
			for (size_t e = Xdata.SoilNode; e < nE; e++) {
				fout << "," << std::fixed << std::setprecision(2) << EMS[e].dsm;
			}
			// 0622: Sigdsm
			fout << "\n0622," << nE-Xdata.SoilNode;
			for (size_t e = Xdata.SoilNode; e < nE; e++) {
				fout << "," << std::fixed << std::setprecision(2) << NDS[e+1].Sigdsm;
			}
			// 0623: S_dsm
			fout << "\n0623," << nE-Xdata.SoilNode;
			for (size_t e = Xdata.SoilNode; e < nE; e++) {
				fout << "," << std::fixed << std::setprecision(2) << NDS[e+1].S_dsm;
			}
		}
	} else {
		for (size_t jj = 1; jj < 7; jj++) {
			fout << "\n060" << jj << ",1,0";
		}
		if (metamorphism_model == "NIED") {
			for (size_t jj = 1; jj < 4; jj++) {
			      fout << "\n062" << jj << ",1,0";
			}
		}

	}
}

/**
 * @brief Calibration: dump special profiles to *.pro output file
 * @author Charles Fierz
 * @version 10.04
 * @param Xdata
 * @param *fout Output file
 */
void AsciiIO::writeProfileProAddCalibration(const SnowStation& Xdata, std::ofstream &fout)
{
	const size_t nE = Xdata.getNumberOfElements();
	const vector<ElementData>& EMS = Xdata.Edata;
	const vector<NodeData>& NDS = Xdata.Ndata;
	if (nE > Xdata.SoilNode) { // snow on the ground
		// 0600-profile specials
		// 0601: snow shear strength (kPa)
		fout << "\n0601," << nE-Xdata.SoilNode;
		for (size_t e = Xdata.SoilNode; e < nE; e++)
			fout << "," << std::fixed << std::setprecision(2) << EMS[e].s_strength;
		// 0602: grain size difference (mm)
		fout << "\n0602," << nE-Xdata.SoilNode;
		for (size_t e = Xdata.SoilNode; e < nE-1; e++)
			fout << "," << std::fixed << std::setprecision(2) << 2.*fabs(EMS[e].rg - EMS[e+1].rg);
		fout << ",0.";
		// 0603: hardness difference (1)
		fout << "\n0603," << nE-Xdata.SoilNode;
		for (size_t e = Xdata.SoilNode; e < nE-1; e++)
			fout << "," << std::fixed << std::setprecision(2) << fabs(EMS[e].hard - EMS[e+1].hard);
		fout << ",0.";
		// 0604: structural stability index SSI
		fout << "\n0604," << nE-Xdata.SoilNode;
		for (size_t e = Xdata.SoilNode; e < nE; e++)
			fout << "," << std::fixed << std::setprecision(2) << NDS[e+1].ssi;
		// 0605: inverse texture index ITI (Mg m-4)
		fout << "\n0605," << nE-Xdata.SoilNode;
		for (size_t e = Xdata.SoilNode; e < nE; e++) {
			if (EMS[e].dd < 0.005)
				fout << "," << std::fixed << std::setprecision(1) << -1.*EMS[e].Rho/(2.*MM_TO_M(EMS[e].rg));
			else
				fout << "," << std::fixed << std::setprecision(1) << 0.;
		}
		// 0606: critical cut length (m)
		fout << "\n0606," << nE-Xdata.SoilNode;
		for (size_t e = Xdata.SoilNode; e < nE; e++) {
			fout << "," << std::fixed << std::setprecision(2) << EMS[e].crit_cut_length;
		}

		// 700-profile specials for settling comparison
		// 0701: SNOWPACK: settling rate due to metamorphism (sig0) (% h-1)
		fout << "\n0701," << nE-Xdata.SoilNode;
		for (size_t e=Xdata.SoilNode; e<nE; e++)
			fout << "," << std::fixed << std::setprecision(2) << -100.*H_TO_S(NDS[e].f);
		// 0702: SNOWPACK: reaction to overload (% h-1) //ratio -Sig0 to load EMS[e].C (1)
		fout << "\n0702," << nE-Xdata.SoilNode;
		for(size_t e=Xdata.SoilNode; e<nE; e++)
			fout << "," << std::fixed << std::setprecision(2) << -100.*H_TO_S(EMS[e].Eps_Dot);
		// 0703: SNOWPACK: settling rate due to load (% h-1)
		fout << "\n0703," << nE-Xdata.SoilNode;
		for (size_t e=Xdata.SoilNode; e<nE; e++)
			fout << "," << std::fixed << std::setprecision(2) << -100.*H_TO_S(NDS[e].udot);
		// 0704: SNOWPACK: total settling rate (% h-1)
		fout << "\n0704," << nE-Xdata.SoilNode;
		for (size_t e=Xdata.SoilNode; e<nE; e++)
			fout << "," << std::fixed << std::setprecision(2) <<  -100.*H_TO_S(EMS[e].Eps_vDot);
		// 0705: SNOWPACK: bond to grain ratio (1)
		fout << "\n0705," << nE-Xdata.SoilNode;
		for (size_t e=Xdata.SoilNode; e<nE; e++)
			fout << "," << std::fixed << std::setprecision(4) <<  EMS[e].rb / EMS[e].rg;
		// 0706: SNOWPACK: addLoad to load (%)
		fout << "\n0706," << nE-Xdata.SoilNode;
		for (size_t e=Xdata.SoilNode; e<nE; e++)
			fout << "," << std::fixed << std::setprecision(4) << 100.*EMS[e].S;

		// SNTHERM.89
		// 0891: SNTHERM: settling rate due to load (% h-1)
		fout << "\n0891," << nE-Xdata.SoilNode;
		for (size_t e=Xdata.SoilNode; e<nE; e++) {
			const double eta_sntherm = (3.6e6*exp(0.08*(273.15-EMS[e].Te))*exp(0.021*EMS[e].Rho));
			fout << "," << std::fixed << std::setprecision(2) << -100.*H_TO_S(EMS[e].C/eta_sntherm);
		}
		// 0892: SNTHERM: settling rate due to metamorphism (% h-1)
		fout << "\n0892," << nE-Xdata.SoilNode;
		for (size_t e=Xdata.SoilNode; e<nE; e++) {
			double evdot = -2.778e-6*exp(-0.04*(273.15 - EMS[e].Te));
			if (EMS[e].Rho > 150.)
				evdot *= exp(-0.046*(EMS[e].Rho-150.));
			if (EMS[e].theta[WATER] > 0.01 )
				evdot *= 2.;
			fout << "," << std::fixed << std::setprecision(2) << -100.*H_TO_S(evdot);
		}
		// 0893: SNTHERM: viscosity (GPa s)
		fout << "\n0893," << nE-Xdata.SoilNode;
		for (size_t e=Xdata.SoilNode; e<nE; e++) {
			const double eta_sntherm = (3.6e6*exp(0.08*(273.15-EMS[e].Te))*exp(0.021*EMS[e].Rho));
			fout << "," << std::fixed << std::setprecision(2) << 1.e-9*eta_sntherm;
		}
	} else {
		for (size_t jj = 1; jj < 7; jj++) {
			fout << "\n060" << jj << ",1,0";
		}
		for (size_t jj = 1; jj < 7; jj++) {
			fout << "\n070" << jj << ",1,0";
		}
		for (size_t jj = 1; jj < 4; jj++) {
			fout << "\n089" << jj << ",1,0";
		}
	}
}

void AsciiIO::writeProfilePrf(const mio::Date& dateOfProfile, const SnowStation& Xdata, const bool& aggregate)
{
	if (Xdata.getNumberOfElements() == Xdata.SoilNode) { //only, so nothing to write
		return;
	}

	//open profile filestream
	const std::string ext = (aggregate)? ".aprf" : ".prf";
	const std::string filename( getFilenamePrefix(Xdata.meta.getStationID(), outpath) + ext );

	//Check whether file exists, if so check whether data can be appended
	//or file needs to be deleted
	if (FileUtils::fileExists(filename)) {
		const bool append = appendFile(filename, dateOfProfile, "prf");
		if (!append && remove(filename.c_str()) != 0)
			prn_msg(__FILE__, __LINE__, "msg-", Date(), "Could not work on file %s", filename.c_str());
	}

	if (!checkHeader(Xdata, filename, "prf", "[TABULAR_PROFILES]")) {
		prn_msg(__FILE__, __LINE__, "err", dateOfProfile,"Checking header in file %s", filename.c_str());
		throw IOException("Cannot dump tabular profiles in " + filename, AT);
	}

	std::ofstream ofs(filename.c_str(), std::ios::out | std::fstream::app);
	if (!ofs) throw AccessException("[E] Can not open file " + filename, AT);

	ofs << "#Date,JulianDate,station,aspect,slope,Nlayers,hs,swe,lwc_sum,ts,tg\n";
	ofs << "#-,-,-,deg,deg,1,cm,kg m-2,degC,degC\n";
	ofs << fixed << dateOfProfile.toString(Date::ISO) << "," << setprecision(6) << dateOfProfile.getJulian() << ",";
	ofs << Xdata.meta.getStationName() << "," << setprecision(1) << Xdata.meta.getAzimuth() << "," << Xdata.meta.getSlopeAngle() << ",";

	vector<SnowProfileLayer> Pdata( SnowProfileLayer::generateProfile(dateOfProfile, Xdata, hoar_density_surf, hoar_min_size_surf) );
	if (aggregate) {
		Aggregate::aggregate(Pdata);
	}
	const double cos_sl = Xdata.cos_sl;
	const size_t nL = Pdata.size();
	ofs << nL << "," << setprecision(1) << Pdata[nL-1].height << "," << Xdata.swe << "," << Xdata.lwc_sum << ",";
	ofs << Pdata[nL-1].T << "," << IOUtils::K_TO_C(Xdata.Ndata[Xdata.SoilNode].T) << "\n";

	//Minima of stability indices at their respective depths as well as stability classifications
	ofs << "#Stab,stab_height,stab_index,stab_class1,stab_class2\n";
	ofs << "# ,cm,1,1,1\n";
	ofs << "deformation," << setprecision(1) << M_TO_CM(Xdata.z_S_d/cos_sl) << "," << setprecision(2) << Xdata.S_d << ",";
	ofs << +Xdata.S_class1 << "," << +Xdata.S_class2 << "\n"; //force printing type char as numerica value
	ofs << "natural," << setprecision(1) << M_TO_CM(Xdata.z_S_n/cos_sl) << "," << setprecision(2) << Xdata.S_n << "\n";
	ofs << "ssi," << setprecision(1) << M_TO_CM(Xdata.z_S_s/cos_sl) << "," << setprecision(2) << Xdata.S_s << "\n";
	ofs << "S4," << setprecision(1) << M_TO_CM(Xdata.z_S_4/cos_sl) << "," << setprecision(2) << Xdata.S_4 << "\n";
	ofs << "S5," << setprecision(1) << M_TO_CM(Xdata.z_S_5/cos_sl) << "," << setprecision(2) << Xdata.S_5 << "\n";

	//Now write all layers starting from the ground
	if (aggregate)
		ofs << "#Aggregated profile\n";
	else
		ofs << "#Full profile\n";
	ofs << "#DepositionDate,DepositionJulianDate,Hn,Tn,gradT,rho,theta_i,theta_w,ogs,gsz,bsz,dd,sp,class,mk,hardness\n";
	ofs << "#-,-,cm,degC,K m-1,kg m-3,1,mm,mm,mm,1,1,1,1,1\n";
	for(size_t ll=0; ll<nL; ll++) {
		ofs << Pdata[ll].depositionDate.toString(Date::ISO) << "," << setprecision(6) << Pdata[ll].depositionDate.getJulian() << ",";
		ofs << setprecision(2) << Pdata[ll].height << "," << setprecision(2) << Pdata[ll].T << "," << setprecision(1) << Pdata[ll].gradT << ",";
		ofs << setprecision(1) << Pdata[ll].rho << "," << setprecision(3) << Pdata[ll].theta_i << "," << Pdata[ll].theta_w << ",";
		ofs << setprecision(1) << Pdata[ll].ogs << "," << Pdata[ll].bond_size << "," << Pdata[ll].grain_size << ",";
		ofs << setprecision(2) << Pdata[ll].dendricity << "," << Pdata[ll].sphericity << ",";
		ofs << Pdata[ll].type << "," << Pdata[ll].marker << "," << setprecision(1) << Pdata[ll].hard << "\n";
	}
	ofs << "\n\n";

	ofs.close();
}

/**
 * @brief Dumps modelled (and measured) temperature at a given vertical position z_vert (m) \n
 * Dumps also vertical height (cm) in case of fixed settling rate sensors
 * @author Charles Fierz
 * @version 10.05
 * @param fout Output file stream
 * @param z_vert Position of sensor measured vertically (m)
 * @param T Measured temperature (K)
 * @param ii Sensor number
 * @param *Xdata
 * @return Number of items dumped to file
 */
size_t AsciiIO::writeTemperatures(std::ofstream &fout, const double& z_vert, const double& T,
                                  const size_t& ii, const SnowStation& Xdata)
{
	size_t jj=2;
	double perp_pos;

	if (ii < fixedPositions.size()) {
		perp_pos = compPerpPosition(z_vert, Xdata.cH, Xdata.Ground, Xdata.cos_sl);
	} else {
		/// @note Initial height of snow needed to compute sensor position from ground if FIXED_RATES is set // HACK
		const double INITIAL_HS=0;
		perp_pos = compPerpPosition(z_vert, INITIAL_HS, Xdata.Ground, Xdata.cos_sl);
		if (perp_pos == Constants::undefined) {
			fout << ",";
		} else {
			fout << "," << std::fixed << std::setprecision(2) << M_TO_CM(perp_pos)/Xdata.cos_sl;
		}
		jj++;
	}

	fout << "," << std::fixed << std::setprecision(2) << Xdata.getModelledTemperature(perp_pos);
	if (ii < numberMeasTemperatures) {
		const double tmp = checkMeasuredTemperature(T, perp_pos, Xdata.mH);
		fout << "," << std::fixed << std::setprecision(2) << tmp;
	} else {
		fout << ",";
	}
	return jj;
}

/**
 * @brief Returns sensor position perpendicular to slope (m) \n
 * Negative vertical height indicates depth from either snow or ground surface \n
 * NOTE: Depth from snow surface cannot be used with SNP_SOIL set
 * @author Charles Fierz
 * @version 10.02
 * @param z_vert Vertical position of the sensor (m)
 * @param hs_ref Height of snow to refer to (m)
 * @param Ground Ground level (m)
 * @param slope_angle (deg)
 */
double AsciiIO::compPerpPosition(const double& z_vert, const double& hs_ref, const double& ground, const double& cos_sl)
{
	double pos=0.;
	if (z_vert == mio::IOUtils::nodata) {
		pos = Constants::undefined;
	} else if (!useSoilLayers && (z_vert < 0.)) {
		pos = hs_ref + z_vert * cos_sl;
		if (pos < 0.)
			pos = Constants::undefined;
	} else {
		pos = ground + z_vert * cos_sl;
		if (pos < -ground)
			pos = Constants::undefined;
	}
	return pos;
}

/**
 * @brief Checks whether measured internal snow or/and soil temperature (instantaneous value) is valid \n
 * The temperature defaults to Constants::undefined if
 *  - the sensor is not covered by more than min_depth_subsurf snow (measured perpendicular to slope)
 * @author Charles Fierz
 * @version 10.01
 * @param T Measured temperature (K)
 * @param z Sensor position perpendicular to slope (m)
 * @param mH Measured snow height (m)
 * @return Measured temperature (degC) if OK, Constants::undefined else
 */
double AsciiIO::checkMeasuredTemperature(const double& T, const double& z, const double& mH)
{
	if ((T != mio::IOUtils::nodata) && (z != Constants::undefined) && (z <= (mH - min_depth_subsurf)))
		return IOUtils::K_TO_C(T);
	else
		return Constants::undefined;
}

/**
 * @brief Find element with corresponding tag or return -1 if not found
 * @version 10.04
 * @param tag to look for
 * @param Xdata
 * @return Index of tagged element, (size_t)-1 if not found
 */
size_t AsciiIO::findTaggedElement(const size_t& tag, const SnowStation& Xdata)
{
	for (size_t e=0; e<Xdata.getNumberOfElements(); e++) {
		if (Xdata.Edata[e].mk/100 == tag)
			return e;
	}
	return static_cast<size_t>(-1);
}

/**
 * @brief Dumps modelled and measured temperature for tag(ged layer)
 * @author Charles Fierz
 * @version 10.02
 * @param fout Output file stream
 * @param tag Tag number;
 * @param *Mdata
 * @param *Xdata
 * @return Number of dumped values
 */
size_t AsciiIO::writeHeightTemperatureTag(std::ofstream &fout, const size_t& tag,
                                          const CurrentMeteo& Mdata, const SnowStation& Xdata)
{
	const size_t e = findTaggedElement(tag, Xdata);
	const double cos_sl = Xdata.cos_sl;
	if (e != static_cast<size_t>(-1)) {
		const double perp_pos = ((Xdata.Ndata[e].z + Xdata.Ndata[e].u + Xdata.Ndata[e+1].z
		                + Xdata.Ndata[e+1].u)/2. - Xdata.Ground);
		fout << "," << std::fixed << std::setprecision(2) << M_TO_CM(perp_pos) / cos_sl << "," << IOUtils::K_TO_C(Xdata.Edata[e].Te);
	} else {
		fout << ",," << std::fixed << std::setprecision(2) << Constants::undefined;
	}
	size_t jj = 2;
	const size_t ii = numberFixedSensors + (tag-1);
	if (ii < numberMeasTemperatures) {
		const double perp_pos = compPerpPosition(Mdata.zv_ts.at(ii), Xdata.cH, Xdata.Ground, Xdata.meta.getSlopeAngle());
		if (perp_pos == Constants::undefined) {
			fout << ",," << std::fixed << std::setprecision(2) << Constants::undefined;
		} else {
			fout << "," << std::fixed << std::setprecision(2) <<  M_TO_CM(perp_pos)/cos_sl;
			const double temp = checkMeasuredTemperature(Mdata.ts.at(ii), perp_pos, Xdata.mH);
			fout << "," << std::fixed << std::setprecision(2) << temp;
		}
		jj += 2;
	}
	return jj;
}

/**
 * @brief Parse through a met file and read the last date for which meteo data has been written
 * @param eoln A char that represents the end of line character
 * @param start_date Holds the start date of this simulation
 * @param fin The file input stream to use
 * @param ftmp The output stream of a temporary file
 * @return TRUE if file may be appended, false if file needs to be overwritten
 */
bool AsciiIO::parseMetFile(const char& eoln, const mio::Date& start_date, std::istream& fin, std::ostream& ftmp)
{
	string tmpline;
	vector<string> vecTmp;
	Date current_date;

	bool append       = false; //true if file may be appended and false otherwise
	bool insert_endl  = false;
	bool data_started = false;

	do { //Loop going through the lines of the file
		getline(fin, tmpline, eoln); //read complete line

		if (data_started) {
			if (tmpline.length() > 20) {//the last line is without a carriage return
				IOUtils::trim(tmpline);
				IOUtils::readLineToVec(tmpline, vecTmp, ',');
				if ((vecTmp.size() >= 2) && (vecTmp[1].length() >= 16)) {
					const string tmpdate = vecTmp[1].substr(6,4) + "-" + vecTmp[1].substr(3,2) + "-" + vecTmp[1].substr(0,2)
						+ "T" + vecTmp[1].substr(11,2) + ":" + vecTmp[1].substr(14,2);
					IOUtils::convertString(current_date, tmpdate, time_zone);

					if (current_date.getJulian() < (start_date.getJulian()-0.00001)) {
						append=true;
					} else {
						break; //the start date of the simulation is newer/equal than current_date
					}
				}
			}
		} else {
			IOUtils::trim(tmpline);
			if (tmpline == "[DATA]")
				data_started = true;
		}

		if (insert_endl)
			ftmp << endl;
		else
			insert_endl = true;

		ftmp << tmpline; //copy line to tmpfile

	} while(!fin.eof());

	return append;
}

/**
 * @brief Parse through a pro file and check whether it can be appended for current simulation
 * @param eoln A char that represents the end of line character
 * @param start_date Holds the start date of this simulation
 * @param fin The file input stream to use
 * @param ftmp The output stream of a temporary file
 * @return TRUE if file may be appended, false if file needs to be overwritten
 */
bool AsciiIO::parseProFile(const char& eoln, const mio::Date& start_date, std::istream& fin, std::ostream& ftmp)
{
	string tmpline;
	vector<string> vecTmp;
	Date current_date;

	bool append = false; //true if file may be appended and false otherwise
	bool insert_endl = false;
	bool data_started = false;

	do { //Loop going through the lines of the file
		getline(fin, tmpline, eoln); //read complete line
		IOUtils::readLineToVec(tmpline, vecTmp, ',');

		if (data_started) {
			if (vecTmp.size() >= 2) {
				if (vecTmp[0] == "0500"){ //The date tag
					if (vecTmp[1].length() >= 16) {
						const string tmpdate = vecTmp[1].substr(6,4) + "-" + vecTmp[1].substr(3,2) + "-" + vecTmp[1].substr(0,2)
						                 + "T" + vecTmp[1].substr(11,2) + ":" + vecTmp[1].substr(14,2);
						IOUtils::convertString(current_date, tmpdate, time_zone);

						if (current_date.getJulian() < (start_date.getJulian()-1.e-5)){
							append=true;
						} else {
							break; //the start date of the simulation is newer/equal than current_date
						}
					}
				}
			}
		} else {
			IOUtils::trim(tmpline);
			if (tmpline == "[DATA]") data_started = true;
		}

		if (insert_endl)
			ftmp << endl;
		else
			insert_endl = true;

		ftmp << tmpline; //copy line to tmpfile
	} while( !fin.eof() );

	return append;
}

/**
 * @brief Parse through a prf file and check whether it can be appended for current simulation
 * @param eoln A char that represents the end of line character
 * @param start_date Holds the start date of this simulation
 * @param fin The file input stream to use
 * @param ftmp The output stream of a temporary file
 * @return TRUE if file may be appended, false if file needs to be overwritten
 */
bool AsciiIO::parsePrfFile(const char& eoln, const mio::Date& start_date, std::istream& fin, std::ostream& ftmp)
{
	string tmpline;
	vector<string> vecTmp;
	Date current_date;

	bool append = false; //true if file may be appended and false otherwise
	bool insert_endl = false;
	bool data_started = false;

	do { //Loop going through the lines of the file
		getline(fin, tmpline, eoln); //read complete line
		IOUtils::readLineToVec(tmpline, vecTmp, ',');

		if (data_started) {
			if (vecTmp.size() >= 2) {
				if (vecTmp[0] == "#Date"){ //The date tag
					IOUtils::readLineToVec(tmpline, vecTmp, ',');
					IOUtils::readLineToVec(tmpline, vecTmp, ',');
					if (vecTmp[1].length() >= 16) {
						const string tmpdate = vecTmp[1].substr(0,15


						);
						IOUtils::convertString(current_date, tmpdate, time_zone);

						if (current_date.getJulian() < (start_date.getJulian()-0.00001)){
							append=true;
						} else {
							break; //the start date of the simulation is newer/equal than current_date
						}
					}
				}
			}
		} else {
			IOUtils::trim(tmpline);
			if (tmpline == "#Date") data_started = true;
		}

		if (insert_endl)
			ftmp << endl;
		else
			insert_endl = true;

		ftmp << tmpline; //copy line to tmpfile
	} while( !fin.eof() );

	return append;
}

/**
 * @brief Check whether data can be appended to a file, or whether file needs to be deleted and recreated
 *        The following logic is implemented if the file already contains data:
 *        - if the startdate lies before the data written in the file, overwrite the file
 *        - if the startdate lies within the data in the file then append from that date on, delete the rest
 *        - if the startdate is after the data in the file then simply append
 * @param filename The file to check (must exist)
 * @param startdate The start date of the data to be written
 * @param ftype A string representing the type of file, i.e. "pro" or "met"
 * @return A boolean, true if file can be appended, false otherwise
 */
bool AsciiIO::appendFile(const std::string& filename, const mio::Date& startdate, const std::string& ftype)
{
	//Check if file has already been checked
	const set<string>::const_iterator it = setAppendableFiles.find(filename);
	if (it != setAppendableFiles.end()) //file was already checked
		return true;

	// Go through file and parse meteo data date if current date
	// is newer than the last one in the file, appending is possible
	ifstream fin;
	ofstream fout; //for the tmp file
	const string filename_tmp = filename + ".tmp";

	fin.open (filename.c_str());
	fout.open(filename_tmp.c_str());

	if (fin.fail()) throw AccessException(filename, AT);
	if (fout.fail()) throw AccessException(filename_tmp, AT);

	const char eoln = FileUtils::getEoln(fin); //get the end of line character for the file

	try {
		bool append_possible = false; //the temporary file will be copied

		if (ftype == "pro") {
			append_possible = parseProFile(eoln, startdate, fin, fout);
		} else if (ftype == "prf") {
			append_possible = parsePrfFile(eoln, startdate, fin, fout);
		} else if (ftype == "met") {
			append_possible = parseMetFile(eoln, startdate, fin, fout);
		}

		fin.close();
		fout.close();

		if (append_possible)
			FileUtils::copy_file(filename_tmp, filename);

		remove(filename_tmp.c_str()); //delete temporary file

		setAppendableFiles.insert(filename); //remember, that this file has been checked already
		return append_possible;
	} catch(...) {
		if (fin.is_open())  fin.close();
		if (fout.is_open()) fout.close();
		return false;
	}
}

void AsciiIO::setNumberSensors(const CurrentMeteo& Mdata)
{
	numberMeasTemperatures = Mdata.getNumberMeasTemperatures();
	maxNumberMeasTemperatures = Mdata.getMaxNumberMeasTemperatures();
	Mdata.getFixedPositions(fixedPositions);
	numberFixedSensors = fixedPositions.size() + Mdata.getNumberFixedRates();
	totNumberSensors = numberFixedSensors /*+Mdata.getNumberTags()*/;
}

/**
 * @brief Write all Time Series results (*.met)
 * All depths and water equivalents (mass) are taken VERTICALLY. \n
 * If AVGSUM_TIME_SERIES is set, mean fluxes and cumulated masses since last dump are written, \n
 * else current energy fluxes, cumulated masses over last computation_step_length (recommended setting in operational mode).
 * If CUMSUM_MASS is set, current value of cumulated masses since begin of run are dumped. \n
 * NOTE:
 * 	-# neither AVGSUM_TIME_SERIES nor CUMSUM_MASS can be set if NUMBER_SLOPES > 1.
 * Precipitations are dumped as rates (kg m-2 h-1) if PRECIP_RATES is set, otherwise as sum over the output time step. \n
 * NOTE:
 * 	-# The units of the corresponding SN_GUI plot will always appear as rate!
 * When running SNOW_REDISTRIBUTION on virtual slopes, eroded mass will be dumped
 *     to the windward *.met file and added to the solid precipitations of the lee *.met file!
 * \li DO NOT change the order of parameters below! Additional parameters may be dumped at pos.
 *     93[94] to 100 in writeTimeSeriesAddXXX()
 * @param Xdata
 * @param Sdata
 * @param Mdata
 * @param Hdata
 * @param wind_trans24 eroded snow from either flat field (present time step) or windward virtual slope (previous time step)
 */
void AsciiIO::writeTimeSeries(const SnowStation& Xdata, const SurfaceFluxes& Sdata,
                              const CurrentMeteo& Mdata, const ProcessDat& Hdata,
                              const double wind_trans24)
{
	const std::string filename( getFilenamePrefix(Xdata.meta.getStationID(), outpath) + ".met" );
	const vector<NodeData>& NDS = Xdata.Ndata;
	const size_t nN = Xdata.getNumberOfNodes();
	const double cos_sl = Xdata.cos_sl;

	//Check whether file exists, if so check whether data can be appended or file needs to be deleted
	if (FileUtils::fileExists(filename)) {
		const bool append = appendFile(filename, Mdata.date, "met");
		if (!append && remove(filename.c_str()) != 0)
			prn_msg(__FILE__, __LINE__, "msg-", Date(), "Could not work on file %s", filename.c_str());
	}

	// Check for availability of measured snow/soil temperatures
	setNumberSensors(Mdata);

	// Correction for snow depth. If we have a marked reference layer, then subtract the height of the reference layer in the output.
	const double HScorrC = (Xdata.findMarkedReferenceLayer()==IOUtils::nodata || !useReferenceLayer) ? (0.) : (Xdata.findMarkedReferenceLayer() - Xdata.Ground);

	// Check file for header
	if (!checkHeader(Xdata, filename, "met", "[STATION_PARAMETERS]")) {
		prn_msg(__FILE__, __LINE__, "err", Mdata.date, "Checking header in file %s", filename.c_str());
		throw InvalidFormatException("Writing Time Series data failed", AT);
	}

	std::ofstream fout;
	fout.open(filename.c_str(),  std::ios::out | std::ofstream::app);
	if (fout.fail()) {
		prn_msg(__FILE__, __LINE__, "err", Mdata.date, "Cannot open time series file: %s", filename.c_str());
		throw AccessException(filename, AT);
	}
	// Print time stamp
	fout << "\n0203," << Mdata.date.toString(Date::DIN);
	fout << std::fixed << std::setprecision(6);
	if (out_heat)
		// 1-2: Turbulent fluxes (W m-2)
		fout << "," <<  Sdata.qs << "," << Sdata.ql;
	else
		fout << ",,";
	if (out_lw)
		// 3-5: Longwave radiation fluxes (W m-2)
		fout << "," <<  Sdata.lw_out << "," << Sdata.lw_in << "," << Sdata.lw_net;
	else
		fout << ",,,";
	if (out_sw)
		// 6-9: Shortwave radiation fluxes (W m-2) and computed albedo (1)
		fout << "," << Sdata.sw_out << "," << Sdata.sw_in << "," << Sdata.qw << "," << Sdata.pAlbedo;
	else
		fout << ",,,,";
	if (out_meteo)
		// 10-13: Air temperature, snow surface temperature (modeled and measured), temperature at bottom of snow/soil pack (degC)
		fout << "," << IOUtils::K_TO_C(Mdata.ta) << "," << IOUtils::K_TO_C(NDS[nN-1].T) << "," << IOUtils::K_TO_C(Mdata.tss) << "," << IOUtils::K_TO_C(NDS[0].T);
	else
		fout << ",,,,";
	if (out_heat)
		// 14-17: Heat flux at lower boundary (W m-2), ground surface temperature (degC),
		//        Heat flux at gound surface (W m-2), rain energy (W m-2)
		fout << "," << Sdata.qg << "," << IOUtils::K_TO_C(NDS[Xdata.SoilNode].T) << "," << Sdata.qg0 << "," << Sdata.qr;
	else
		fout << ",,,,";
	if (out_sw)
		// 18-22: projected solar radiation (W m-2), meas. albedo (1)
		fout  << "," << Sdata.sw_hor << "," << Sdata.sw_in << "," << Sdata.sw_dir << "," << Sdata.sw_diff << "," << Sdata.mAlbedo;
	else
		fout << ",,,,,";
	if (out_meteo) {
		// 23-26: rH (%), wind (m s-1), wind_drift (m s-1), wind_dir (deg),
		// 27: solid precipitation rate (kg m-2 h-1),
		// 28-29: modeled and enforced vertical snow depth (cm); see also 51
		fout  << "," << 100.*Mdata.rh << "," << Mdata.vw << "," << Mdata.vw_drift << "," << Mdata.dw << "," << Sdata.mass[SurfaceFluxes::MS_HNW];
		fout << "," << std::fixed << std::setprecision(2) << M_TO_CM((Xdata.cH - Xdata.Ground - HScorrC)/cos_sl) << ",";
		if (Xdata.mH!=Constants::undefined)
			fout << M_TO_CM((Xdata.mH - Xdata.Ground)/cos_sl) << std::setprecision(6);
		else
			fout << IOUtils::nodata << std::setprecision(6);
	} else
		fout << ",,,,,,,";
	if (out_haz) {
		// 30-33: surface hoar size (mm), 24h drift index (cm), height of new snow HN (cm), 3d sum of daily new snow depths (cm)
		if (!perp_to_slope)
			fout << "," << Hdata.hoar_size << "," << wind_trans24 << "," << Hdata.hn24 << "," << Hdata.hn72_24;
		else
			// dump vertical values if PERP_TO_SLOPE
			fout << "," << Hdata.hoar_size << "," << wind_trans24 << "," << Hdata.hn24/cos_sl << "," << Hdata.hn72_24/cos_sl;
	} else {
		if(out_soileb) {
			// 30-33: soil energy balance variables
			size_t nCalcSteps = 1;
			nCalcSteps = static_cast<size_t>(ts_days_between / M_TO_D(calculation_step_length) + 0.5);
			fout << "," << (Sdata.dIntEnergySoil * static_cast<double>(nCalcSteps)) / 1000. << "," << (Sdata.meltFreezeEnergySoil * static_cast<double>(nCalcSteps)) / 1000. << "," << Xdata.ColdContentSoil/1E6 << "," << Hdata.hn72_24;
		} else {
			fout << ",,,,";
		}
	}
	if (out_mass) {
		// 34-39: SWE, eroded mass, rain rate, runoff at bottom of snowpack, sublimation and evaporation, all in kg m-2 except rain as rate: kg m-2 h-1; see also 52 & 93
		fout  << "," << Sdata.mass[SurfaceFluxes::MS_SWE]/cos_sl << "," << Sdata.mass[SurfaceFluxes::MS_WIND]/cos_sl << "," << Sdata.mass[SurfaceFluxes::MS_RAIN];
		fout << "," << Sdata.mass[SurfaceFluxes::MS_SNOWPACK_RUNOFF]/cos_sl << "," << Sdata.mass[SurfaceFluxes::MS_SUBLIMATION]/cos_sl << "," << Sdata.mass[SurfaceFluxes::MS_EVAPORATION]/cos_sl;
	} else {
		fout << ",,,,,,";
	}
	// 40-49: Internal Temperature Time Series at fixed heights, modeled and measured, all in degC
	if (out_t && (fixedPositions.size() || Mdata.getNumberFixedRates())) {
		const size_t nrFixedPositions = std::min((size_t)5, fixedPositions.size());
		if (Mdata.zv_ts.size()!=nrFixedPositions || Mdata.ts.size()!=nrFixedPositions) {
			std::ostringstream ss;
			ss << "Configured " << nrFixedPositions << " fixed positions but found " << Mdata.zv_ts.size() << " snow temperatures depths and ";
			ss << Mdata.ts.size() << " snow temperatures";
			throw IndexOutOfBoundsException(ss.str(), AT);
		}

		size_t jj = 0;
		for (size_t ii = 0; ii < nrFixedPositions; ii++)
			jj += writeTemperatures(fout, Mdata.zv_ts.at(ii), Mdata.ts.at(ii), ii, Xdata);
		for (; jj < 10; jj++)
			fout << ",";
	} else {
		fout << ",,,,,,,,,,";
	}
	if (maxNumberMeasTemperatures == 5) {
		// 50: Solute load at ground surface
		if (out_load && !Sdata.load.empty())
			fout << "," << Sdata.load[0];
		else
			fout << ",";
		// 51: input snow depth HS (cm); see also 28-29
		if (out_meteo)
			fout << "," << std::fixed << std::setprecision(2) << M_TO_CM(Mdata.hs)/cos_sl << std::setprecision(6);
		else
			fout << ",";
		// 52: LWC (kg m-2); see also 34-39
		if (out_mass)
			fout << "," <<  Sdata.mass[SurfaceFluxes::MS_WATER]/cos_sl;
		else
			fout << ",";
		// 53-64: Stability Time Series, heights in cm
		if (out_stab) {
			fout << "," << +Xdata.S_class1 << "," << +Xdata.S_class2 << std::fixed; //profile type and stability class, force printing type char as numerica value
			fout << "," << std::setprecision(1) << M_TO_CM(Xdata.z_S_d/cos_sl) << "," << std::setprecision(2) << Xdata.S_d;
			fout << "," << std::setprecision(1) << M_TO_CM(Xdata.z_S_n/cos_sl) << "," << std::setprecision(2) << Xdata.S_n;
			fout << "," << std::setprecision(1) << M_TO_CM(Xdata.z_S_s/cos_sl) << "," << std::setprecision(2) << Xdata.S_s;
			fout << "," << std::setprecision(1) << M_TO_CM(Xdata.z_S_4/cos_sl) << "," << std::setprecision(2) << Xdata.S_4;
			fout << "," << std::setprecision(1) << M_TO_CM(Xdata.z_S_5/cos_sl) << "," << std::setprecision(2) << Xdata.getLiquidWaterIndex() /*Xdata.S_5*/;
			fout << std::setprecision(6);
		} else {
			fout << ",,,,,,,,,,,,";
		}
		// 65-92 (28 columns)
		if (out_canopy && useCanopyModel)
			Canopy::DumpCanopyData(fout, &Xdata.Cdata, &Sdata, cos_sl);
		else {
			if (variant == "SEAICE" && Xdata.Seaice != NULL) {
				// Total thickness (m), Ice thickness (m), snow thickness (m), snow thickness wrt reference (m), freeboard (m), sea level (m), bulk salinity, average bulk salinity, brine salinity, average brine salinity, bottom salinity flux, top salinity flux
				fout << "," << std::setprecision(3) << Xdata.cH - Xdata.Ground;
				fout << "," << std::setprecision(3) << Xdata.Ndata[Xdata.Seaice->IceSurfaceNode].z - Xdata.Ground;
				fout << "," << std::setprecision(3) << Xdata.Ndata[Xdata.getNumberOfNodes()-1].z - Xdata.Ndata[Xdata.Seaice->IceSurfaceNode].z;
				// Check reference level: either a marked reference level, or, if non existent, the sea level (if sea ice module is used), otherwise 0:
				const double ReferenceLevel = (  Xdata.findMarkedReferenceLayer()==IOUtils::nodata || !useReferenceLayer  )  ?  (  (Xdata.Seaice==NULL)?(0.):(Xdata.Seaice->SeaLevel)  )  :  (Xdata.findMarkedReferenceLayer() - Xdata.Ground);
				fout << "," << std::setprecision(3) << Xdata.Ndata[Xdata.getNumberOfNodes()-1].z - ReferenceLevel;
				fout << "," << std::setprecision(3) << Xdata.Seaice->FreeBoard;
				fout << "," << std::setprecision(3) << Xdata.Seaice->SeaLevel;
				fout << "," << std::setprecision(3) << Xdata.Seaice->getTotSalinity(Xdata);
				fout << "," << std::setprecision(3) << Xdata.Seaice->getAvgBulkSalinity(Xdata);
				fout << "," << std::setprecision(3) << Xdata.Seaice->getAvgBrineSalinity(Xdata);
				fout << "," << std::setprecision(3) << Xdata.Seaice->BottomSalFlux;
				fout << "," << std::setprecision(3) << Xdata.Seaice->TopSalFlux;
				fout << ",,,,,,,,,,,,,,,,";
			} else {
				fout << ",,,,,,,,,,,,,,,,,,,,,,,,,,,,";
			}
		}
	} else if (out_t) {
		// 50-93 (44 columns)
		size_t ii, jj = 0;
		for (ii = std::min((size_t)5, fixedPositions.size()); ii < numberFixedSensors; ii++) {
			if ((jj += writeTemperatures(fout, Mdata.zv_ts.at(ii), Mdata.ts.at(ii), ii, Xdata)) > 44) {
				prn_msg(__FILE__, __LINE__, "err", Mdata.date,
				        "There is not enough space to accomodate your temperature sensors: j=%u > 44!", jj);
				throw IOException("Writing Time Series data failed", AT);
			}
		}
		for (; jj < 44; jj++)
			fout << ",";
	} else {
		fout << ",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,";
	}
	// 93[94]-100 (8 or 7 free columns)
	size_t nCalcSteps = 1;
	double crust = 0., dhs_corr = 0., mass_corr = 0.;
	if (!avgsum_time_series)
		nCalcSteps = static_cast<size_t>(ts_days_between / M_TO_D(calculation_step_length) + 0.5);
	if (out_haz) {
		crust = Hdata.crust;
		dhs_corr = Hdata.dhs_corr;
		mass_corr = Hdata.mass_corr;
	}
	if (variant == "CALIBRATION") {
		writeTimeSeriesAddCalibration(Xdata, Sdata, Mdata, crust, dhs_corr, mass_corr, nCalcSteps, fout);
	} else if (variant == "ANTARCTICA") {
		writeTimeSeriesAddDefault(Xdata, Sdata, Mdata, crust, dhs_corr, mass_corr, nCalcSteps, fout);
	} else {
		writeTimeSeriesAddDefault(Xdata, Sdata, Mdata, crust, dhs_corr, mass_corr, nCalcSteps, fout);
	}

	fout.close();
}

/**
 * @brief Default: last 8 time series (columns 93 to 100) dumped to *.met output file
 * @version 11.05
 * @param Xdata
 * @param Sdata
 * @param Mdata
 * @param crust height
 * @param dhs_corr correction for height of snow in (operational mode only)
 * @param mass_corr mass correction due to dhs_corr (operational mode only)
 * @param nCalcSteps between outputs
 * @param fout Output file stream
 */
void AsciiIO::writeTimeSeriesAddDefault(const SnowStation& Xdata, const SurfaceFluxes& Sdata,
                                        const CurrentMeteo& Mdata, const double crust,
                                        const double dhs_corr, const double mass_corr,
                                        const size_t nCalcSteps, std::ofstream &fout)
{
	// 93: Soil Runoff (kg m-2); see also 34-39 & 51-52
	if (useSoilLayers)
		fout << "," << Sdata.mass[SurfaceFluxes::MS_SOIL_RUNOFF] / Xdata.cos_sl;
	else
		fout << ",";
	// 94-95:
	if (out_heat) {
		// 94: change of internal energy (kJ m-2)
		if (Xdata.getNumberOfElements() > Xdata.SoilNode)
			fout << "," << std::fixed << std::setprecision(3) <<  ((Sdata.dIntEnergy * static_cast<double>(nCalcSteps))
		                             - (Sdata.qg0 * D_TO_S(ts_days_between))) / 1000. << std::setprecision(6);
		else
			fout << "," << Constants::undefined;
		// 95: sum of energy fluxes at surface (kJ m-2)
		fout << "," << ((Sdata.qw + Sdata.lw_net + Sdata.qs + Sdata.ql + Sdata.qr)
		                * D_TO_S(ts_days_between)) / 1000.;
	} else {
		fout << ",,";
	}
	// 96-97: new snow densities, measured and in use (kg m-3)
	if(Sdata.cRho_hn > 0.) {
		fout << "," << std::fixed << std::setprecision(1) <<  Sdata.mRho_hn << "," << Sdata.cRho_hn << std::setprecision(6);
	} else {
		if(Mdata.rho_hn != mio::IOUtils::nodata)
			fout << "," << std::fixed << std::setprecision(1) << -Mdata.rho_hn << "," << Sdata.cRho_hn << std::setprecision(6);
		else
			fout << "," << std::fixed << std::setprecision(1) << Constants::undefined << "," << Sdata.cRho_hn << std::setprecision(6);
	}
	// 98: crust height (S-slope) (cm)
	fout << "," << crust;
	// 99-100:
	if (!research_mode) {
		// snow depth (cm) and mass correction (kg m-2)
		fout << "," << M_TO_CM(dhs_corr) << "," << mass_corr;
	} else {
		// for example, measured turbulent fluxes (W m-2); see also 1-2
		fout << ",," << (Sdata.meltFreezeEnergy * static_cast<double>(nCalcSteps)) / 1000.;
		// output melt and refreeze mass (kg m-2)
		fout << "," << Sdata.meltMass;
		fout << "," << Sdata.refreezeMass;
	}
}

/**
 * @brief Antarctic: last [8]7 time series (columns [93]94 to 100) dumped to *.met output file
 * @version 11.05
 * @param Xdata
 * @param Sdata
 * @param Mdata
 * @param crust not available; replaced by potential erosion level
 * @param dhs_corr not available
 * @param mass_corr not available
 * @param nCalcSteps between outputs
 * @param fout Output file stream
 */
void AsciiIO::writeTimeSeriesAddAntarctica(const SnowStation& Xdata, const SurfaceFluxes& Sdata,
                                           const CurrentMeteo& Mdata, const double /*crust*/,
                                           const double /*dhs_corr*/, const double /*mass_corr*/,
                                           const size_t nCalcSteps, std::ofstream &fout)
{
	if (maxNumberMeasTemperatures == 5) // then there is room for the measured HS at pos 93
		fout << "," << std::fixed << std::setprecision(2) << M_TO_CM(Mdata.hs)/Xdata.cos_sl << std::setprecision(6);
	// 94-95:
	if (out_heat) {
		// 94: change of internal energy (kJ m-2)
		if (Xdata.getNumberOfElements() > Xdata.SoilNode)
			fout << "," << std::fixed << std::setprecision(3) << ((Sdata.dIntEnergy * static_cast<double>(nCalcSteps))
		                             - (Sdata.qg0 * D_TO_S(ts_days_between))) / 1000. << std::setprecision(6);
		else
			fout << "," << Constants::undefined;
		// 95: sum of energy fluxes at surface (kJ m-2)
		fout << "," << ((Sdata.qw + Sdata.lw_net + Sdata.qs + Sdata.ql + Sdata.qr)
		                * D_TO_S(ts_days_between)) / 1000.;
	} else {
		fout << ",,";
	}
	// 96-97: new snow densities, measured and in use (kg m-3)
	if(Sdata.cRho_hn > 0.) {
		fout << "," << std::fixed << std::setprecision(1) << Sdata.mRho_hn << "," << Sdata.cRho_hn << std::setprecision(6);
	} else {
		double mRho_hn = Constants::undefined;
		if (Mdata.rho_hn != mio::IOUtils::nodata)
			mRho_hn = -Mdata.rho_hn;
		fout << "," << std::fixed << std::setprecision(1) << mRho_hn << "," << Sdata.cRho_hn << std::setprecision(6);
	}
	// 98: potential erosion level below surface (cm)
	fout << "," << M_TO_CM(Xdata.Ndata[Xdata.ErosionLevel+1].z - Xdata.cH);
	// 99-100
	if (out_meteo)
		// mean over 100 h of air humidity (%) and mean wind speed (m s-1)
		fout << "," << std::fixed << std::setprecision(2) << 100. * Mdata.rh_avg << "," << Mdata.vw_avg << std::setprecision(6);
	else
		fout << ",,";
}

/**
 * @brief Calibration: last [8]7 time series (columns [93]94 to 100) dumped to *.met output file
 * @version 11.05
 * @param Xdata
 * @param Sdata not used
 * @param Mdata
 * @param crust height, not used
 * @param dhs_corr not available
 * @param mass_corr not available
 * @param nCalcSteps
 * @param fout Output file stream
 */
void AsciiIO::writeTimeSeriesAddCalibration(const SnowStation& Xdata, const SurfaceFluxes& Sdata,
                                            const CurrentMeteo& Mdata, const double /*crust*/,
                                            const double /*dhs_corr*/, const double /*mass_corr*/,
                                            const size_t nCalcSteps, std::ofstream &fout)
{
	const double t_surf = std::min(IOUtils::C_TO_K(-0.1), Xdata.Ndata[Xdata.getNumberOfNodes()-1].T);
	if (maxNumberMeasTemperatures == 5) // then there is room for the measured HS at pos 93
		fout << "," << std::fixed << std::setprecision(2) << M_TO_CM(Mdata.hs)/Xdata.cos_sl << std::setprecision(6);
	// 94-95:
	if (out_heat) {
		// 94: change of internal energy (kJ m-2)
		if (Xdata.getNumberOfElements() > Xdata.SoilNode)
			fout << "," << std::fixed << std::setprecision(3) << ((Sdata.dIntEnergy * static_cast<double>(nCalcSteps))
			                         - (Sdata.qg0 * D_TO_S(ts_days_between))) / 1000. << std::setprecision(6);
		else
			fout <<  "," << Constants::undefined;
		// 95: sum of energy fluxes at surface (kJ m-2)
		fout << "," << ((Sdata.qw + Sdata.lw_net + Sdata.qs + Sdata.ql + Sdata.qr)
		                * D_TO_S(ts_days_between)) / 1000.;
	} else {
		fout << ",,";
	}
	// 96-100: new snow densities: measured, in use, newLe, bellaire, and crocus (kg m-3)
	double rho_hn, signRho;
	if (Sdata.cRho_hn > 0.) {
		fout << "," << std::fixed << std::setprecision(1) << Sdata.mRho_hn << "," << Sdata.cRho_hn << std::setprecision(6);
		signRho = 1.;
	} else {
		const double mRho_hn = (Mdata.rho_hn != mio::IOUtils::nodata) ? -Mdata.rho_hn : Constants::undefined;
		fout << "," << std::fixed << std::setprecision(1) << mRho_hn << "," << Sdata.cRho_hn << std::setprecision(6);
		signRho = -1.;
	}
	rho_hn = SnLaws::compNewSnowDensity("PARAMETERIZED", "LEHNING_NEW", Constants::undefined, Mdata, Xdata, t_surf, variant);
	fout << "," << std::fixed << std::setprecision(1) << signRho*rho_hn << std::setprecision(6);
	rho_hn = SnLaws::compNewSnowDensity("PARAMETERIZED", "BELLAIRE", Constants::undefined, Mdata, Xdata, t_surf, variant);
	fout << "," << std::fixed << std::setprecision(1) << signRho*rho_hn << std::setprecision(6);
	rho_hn = SnLaws::compNewSnowDensity("PARAMETERIZED", "PAHAUT", Constants::undefined, Mdata, Xdata, t_surf, variant);
	fout << "," << std::fixed << std::setprecision(1) << signRho*rho_hn << std::setprecision(6);
}

void AsciiIO::writeMETHeader(const SnowStation& Xdata, std::ofstream &fout) const
{
	const string stationname = Xdata.meta.getStationName();
	fout << "[STATION_PARAMETERS]";
	fout <<  "\nStationName= " << stationname;
	fout << "\nLatitude= " << std::fixed << std::setprecision(8) <<  Xdata.meta.position.getLat();
	fout << "\nLongitude= " << std::fixed << std::setprecision(8) <<  Xdata.meta.position.getLon();
	fout << "\nAltitude= " << std::fixed << std::setprecision(0) <<  Xdata.meta.position.getAltitude();
	fout << "\nSlopeAngle= " << std::fixed << std::setprecision(2) << Xdata.meta.getSlopeAngle();
	fout << "\nSlopeAzi= " << std::fixed << std::setprecision(2) << Xdata.meta.getAzimuth();
	fout << "\nDepthTemp= " << std::fixed << std::setprecision(1) << (Xdata.SoilNode > 0);

	for (size_t ii = 0; ii < fixedPositions.size(); ii++)
		fout << "," << std::fixed << std::setprecision(3) << fixedPositions[ii] << std::setprecision(6);
	fout << "\n\n[HEADER]";
	if (out_haz) { // HACK To avoid troubles in A3D
		fout << "\n#" << info.computation_date.toString(Date::ISO) << ", Snowpack " << variant << " version " << info.version << " run by \"" << info.user << "\"";
		if (research_mode)
			fout <<  " (research mode)";
		else
			fout <<  " (operational mode)";
	}
	fout << "\n,,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102";
	fout << "\nID,Date,Sensible heat,Latent heat,Outgoing longwave radiation,Incoming longwave radiation,Net absorbed longwave radiation,Reflected shortwave radiation,Incoming shortwave radiation,Net absorbed shortwave radiation,Modelled surface albedo,Air temperature,Modeled surface temperature,Measured surface temperature,Temperature at bottom of snow or soil pack,Heat flux at bottom of snow or soil pack,Ground surface temperature,Heat flux at ground surface,Heat advected to the surface by liquid precipitation,Global solar radiation (horizontal)";
	if(out_haz==true || out_soileb==false) {
		fout << ",Global solar radiation on slope,Direct solar radiation on slope,Diffuse solar radiation on slope,Measured surface albedo,Relative humidity,Wind speed,Max wind speed at snow station or wind speed at ridge station,Wind direction at snow station,Precipitation rate at surface (solid only),Modelled snow depth (vertical),Enforced snow depth (vertical),Surface hoar size,24h Drift index (vertical),Height of new snow HN (24h vertical),3d sum of daily height of new snow (vertical),SWE (of snowpack),Eroded mass,Rain rate,Snowpack runoff (virtual lysimeter)";
	} else {
		fout << ",Global solar radiation on slope,Direct solar radiation on slope,Diffuse solar radiation on slope,Measured surface albedo,Relative humidity,Wind speed,Max wind speed at snow station or wind speed at ridge station,Wind direction at snow station,Precipitation rate at surface (solid only),Modelled snow depth (vertical),Enforced snow depth (vertical),Internal energy change soil,Melt freeze part of internal energy change soil,Cold content soil,,SWE (of snowpack),Eroded mass,Rain rate,Snowpack runoff (virtual lysimeter)";
	}
	fout << ",Sublimation,Evaporation,Temperature 1 (modelled),Temperature 1 (measured),Temperature 2 (modelled),Temperature 2 (measured),Temperature 3 (modelled),Temperature 3 (measured),Temperature 4 (modelled),Temperature 4 (measured),Temperature 5 (modelled),Temperature 5 (measured)";
	if (maxNumberMeasTemperatures == 5) {
		fout << ",Solute load at soil surface,Measured snow depth HS,Liquid Water Content (of snowpack),Profile type,Stability class,z_Sdef,Deformation rate stability index Sdef,z_Sn38,Natural stability index Sn38,z_Sk38,Skier stability index Sk38,z_SSI,Structural Stability index SSI,z_S5,Stability index S5";
		if (useCanopyModel && out_canopy) {
			Canopy::DumpCanopyHeader(fout);
		} else {
			if (variant == "SEAICE" && Xdata.Seaice != NULL) {
				fout << ",Total thickness,Ice thickness,Snow thickness,Snow thickness wrt reference,Freeboard,Sea level,Tot salinity,Average bulk salinity,Average Brine Salinity,Bottom Sal Flux,Top Sal Flux";
				fout << ",-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-";
			} else {
				// 28 empty fields
				fout << ",-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-";
			}
		}
	} else if (out_t) {
		size_t jj = 0;
		for (size_t ii = std::min((size_t)5, fixedPositions.size()); ii < numberFixedSensors; ii++) {
			size_t i_prn;
			if (ii < fixedPositions.size()) {
				i_prn = ii + 1;
				fout <<  ",Temperature " << i_prn << " (modelled)";
			} else {
				i_prn = (ii-fixedPositions.size())+1;
				fout << ",Hfr " << i_prn;
				fout <<  ",Tfr " << i_prn << " (modelled)";
				jj++;
			}
			if (ii < numberMeasTemperatures) {
				if (ii < fixedPositions.size()) {
					fout << ",Temperature " << i_prn << " (measured)";
				} else {
					fout << ",Tfr " << i_prn << " (measured)";
				}
			} else {
				fout << ",";
			}
			jj += 2;
		}
		for (; jj < 44; jj++) {
			fout << ",-";
		}
	} else {
		fout << ",-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-";
	}

	if (variant == "ANTARCTICA") {
		if (maxNumberMeasTemperatures == 5)
			fout <<  ",Measured snow depth HS";
		fout << ",Internal energy change,Surface input (sum fluxes),Measured new snow density,Modeled new snow density,Erosion level (from srf),Running mean relative humidity (100h),Running mean wind speed (100h)";
	} else if (variant == "CALIBRATION") {
		if (maxNumberMeasTemperatures == 5)
			fout << ",Measured snow depth HS";
		fout << "Internal energy change,Surface input (sum fluxes),rho_hn(measured),rho_hn(Zwart),rho_hn(Lehning),rho_hn(Bellaire),rho_hn(PAHAUT)";
	} else {
		fout << ",Soil runoff,Internal energy change,Surface input (sum fluxes),Measured new snow density,Modeled new snow density,Crust thickness (S-slope)";
		if (!research_mode)
			fout << ",Snow depth correction,Mass change";
		else
			fout << ",-,Melt freeze part of internal energy change,Melted mass,Refrozen mass";
	}

	if(out_haz==true || out_soileb==false) {
		fout << "\n,,W m-2,W m-2,W m-2,W m-2,W m-2,W m-2,W m-2,W m-2,1,degC,degC,degC,degC,W m-2,degC,W m-2,W m-2,W m-2,W m-2,W m-2,W m-2,1,%,m s-1,m s-1,deg,kg m-2 h-1,cm,cm,mm,cm,cm,cm,kg m-2,kg m-2 h-1,kg m-2 h-1,kg m-2,kg m-2,kg m-2,degC,degC,degC,degC,degC,degC,degC,degC,degC,degC";
	} else {
		fout << "\n,,W m-2,W m-2,W m-2,W m-2,W m-2,W m-2,W m-2,W m-2,1,degC,degC,degC,degC,W m-2,degC,W m-2,W m-2,W m-2,W m-2,W m-2,W m-2,1,%,m s-1,m s-1,deg,kg m-2 h-1,cm,cm,kJ m-2,kJ m-2,MJ m-2,,kg m-2,kg m-2 h-1,kg m-2 h-1,kg m-2,kg m-2,kg m-2,degC,degC,degC,degC,degC,degC,degC,degC,degC,degC";
	}
	if (maxNumberMeasTemperatures == 5) {
		fout << ",kg m-2,cm,kg m-2,-,-,cm,1,cm,1,cm,1,cm,1,cm,1";
		if (out_canopy && useCanopyModel) {
			Canopy::DumpCanopyUnits(fout);
		} else {
			if (variant == "SEAICE" && Xdata.Seaice != NULL) {
				fout << ",m,m,m,m,m,m,g m-2,g kg-1,g kg-1,g m-2,g m-2";
				fout << ",,,,,,,,,,,,,,,,,";
			} else {
				fout << ",,,,,,,,,,,,,,,,,,,,,,,,,,,,";
			}
		}
	} else if (out_t) {
		size_t jj = 0;
		for (size_t ii = std::min((size_t)5, fixedPositions.size()); ii < numberFixedSensors; ii++) {
			if (ii >= fixedPositions.size()) {
				fout << ",cm";
				jj++;
			}
			fout << ",degC";
			jj++;
			if (ii < numberMeasTemperatures) {
				fout << ",degC";
				jj++;
			}
		}
		for (; jj < 44; jj++)
			fout <<",";
	} else {
		fout << ",,,,,,,,,,,,,,,,,,,,,,,,,,,,";

	}
	if (variant == "ANTARCTICA") {
		if (maxNumberMeasTemperatures == 5)
			fout << ",cm";
		fout << ",kJ m-2,kJ m-2,kg m-3,kg m-3,cm,%,m s-1";
	} else if (variant == "CALIBRATION") {
		if (maxNumberMeasTemperatures == 5)
			fout << ",cm";
		fout << ",kJ m-2,kJ m-2,kg m-3,kg m-3,kg m-3,kg m-3,kg m-3";
	} else {
		fout << ",kg m-2,kJ m-2,kJ m-2,kg m-3,kg m-3,cm";
		if (!research_mode)
			fout << ",cm,kg m-2";
		else
			fout << ",-,kJ m-2,kg m-2,kg m-2";
	}

	fout << "\n\n[DATA]";
}

void AsciiIO::writeProHeader(const SnowStation& Xdata, std::ofstream &fout) const
{
	const string stationname = Xdata.meta.getStationName();
	fout << "[STATION_PARAMETERS]";
	fout <<  "\nStationName= " << stationname;
	fout << "\nLatitude= " << std::fixed << std::setprecision(8) <<  Xdata.meta.position.getLat();
	fout << "\nLongitude= " << std::fixed << std::setprecision(8) <<  Xdata.meta.position.getLon();
	fout << "\nAltitude= " << std::fixed << std::setprecision(0) <<  Xdata.meta.position.getAltitude();
	fout << "\nSlopeAngle= " << std::fixed << std::setprecision(2) << Xdata.meta.getSlopeAngle();
	fout << "\nSlopeAzi= " << std::fixed << std::setprecision(2) << Xdata.meta.getAzimuth();

	fout << "\n\n[HEADER]";
	if (out_haz) { // HACK To avoid troubles in A3D
		fout << "\n#" << info.computation_date.toString(Date::ISO) << ", Snowpack " << variant << " version " << info.version << " run by \"" << info.user << "\"";
		if (research_mode)
			fout <<  " (research mode)";
		else
			fout <<  " (operational mode)";
	}

	fout << "\n0500,Date";
	fout << "\n0501,nElems,height [> 0: top, < 0: bottom of elem.] (cm)";
	fout << "\n0502,nElems,element density (kg m-3)";
	fout << "\n0503,nElems,element temperature (degC)";
	fout << "\n0504,nElems,element ID (1)";
	fout << "\n0506,nElems,liquid water content by volume (%)";
	if(enable_pref_flow) fout << "\n0507,nElems,liquid preferential flow water content by volume (%)";
	fout << "\n0508,nElems,dendricity (1)";
	fout << "\n0509,nElems,sphericity (1)";
	fout << "\n0510,nElems,coordination number (1)";
	fout << "\n0511,nElems,bond size (mm)";
	fout << "\n0512,nElems,grain size (mm)";
	fout << "\n0513,nElems,grain type (Swiss Code F1F2F3)";
	fout << "\n0514,3,grain type, grain size (mm), and density (kg m-3) of SH at surface";
	fout << "\n0515,nElems,ice volume fraction (%)";
	fout << "\n0516,nElems,air volume fraction (%)";
	fout << "\n0517,nElems,stress in (kPa)";
	fout << "\n0518,nElems,viscosity (GPa s)";
	fout << "\n0519,nElems,soil volume fraction (%)";
	fout << "\n0520,nElems,temperature gradient (K m-1)";
	fout << "\n0521,nElems,thermal conductivity (W K-1 m-1)";
	fout << "\n0522,nElems,absorbed shortwave radiation (W m-2)";
	fout << "\n0523,nElems,viscous deformation rate (1.e-6 s-1)";
	fout << "\n0530,8,position (cm) and minimum stability indices:";
	fout << "\n       profile type, stability class, z_Sdef, Sdef, z_Sn38, Sn38, z_Sk38, Sk38";
	fout << "\n0531,nElems,deformation rate stability index Sdef";
	fout << "\n0532,nElems,natural stability index Sn38";
	fout << "\n0533,nElems,stability index Sk38";
	fout << "\n0534,nElems,hand hardness either (N) or index steps (1)";
	fout << "\n0535,nElems,optical equivalent grain size (mm)";
	if (Xdata.Seaice != NULL) {
		fout << "\n0540,nElems,bulk salinity (g/kg)";
		fout << "\n0541,nElems,brine salinity (g/kg)";
	}
	fout << "\n0601,nElems,snow shear strength (kPa)";
	fout << "\n0602,nElems,grain size difference (mm)";
	fout << "\n0603,nElems,hardness difference (1)";
	fout << "\n0604,nElems,ssi";
	fout << "\n0605,nElems,inverse texture index ITI (Mg m-4)";
	fout << "\n0606,nElems,critical cut length (m)";
	if (metamorphism_model == "NIED") {
		fout << "\n0621,nElems,dry snow metamorphism factor (dsm)";
		fout << "\n0622,nElems,Sigdsm";
		fout << "\n0623,nElems,S_dsm";
	}
	if (variant == "CALIBRATION") {
		fout << "\n0701,nElems,SNOWPACK: total settling rate (% h-1)";
		fout << "\n0702,nElems,SNOWPACK: settling rate due to load (% h-1)";
		fout << "\n0703,nElems,SNOWPACK: settling rate due to metamorphism (sig0) (% h-1)";
		fout << "\n0704,nElems,SNOWPACK: ratio -Sig0 to load EMS[e].C (1)";
		fout << "\n0705,nElems,SNOWPACK: bond to grain ratio (1)";
		fout << "\n0891,nElems,SNTHERM: settling rate due to load (% h-1)";
		fout << "\n0892,nElems,SNTHERM: settling rate due to metamorphism (% h-1)";
		fout << "\n0893,nElems,SNTHERM: viscosity (GPa s)";
	}
	fout << "\n\n[DATA]";
}

void AsciiIO::writePrfHeader(const SnowStation& Xdata, std::ofstream &fout) const
{
	const string stationname = Xdata.meta.getStationName();
	fout << "[TABULAR_PROFILES]\n";
}

/**
 * @brief This routine:
 * -# Checks for header in fnam by testing for first_string
 * -# If header is missing:
 *    - writes header in fnam according to file type (ext)
 *    - returns -1 (ext=="none")
 * @author Charles Fierz \n Mathias Bavay
 * @version 10.02
 * @param *fnam Filename
 * @param *first_string First string to be found in header
 * @param Hdata
 * @param *ext File extension
 * @return status
 */
bool AsciiIO::checkHeader(const SnowStation& Xdata, const std::string& filename, const std::string& ext, const std::string& signature) const
{
	 std::ifstream fin;
	 fin.open (filename.c_str(), std::ifstream::in);

	if (!fin.fail()) {
		// Check header of existing file
		string dummy;
		if (!(fin >> dummy) || dummy.empty()) {
			fin.close();
			throw InvalidFormatException("Can not read header of file \'"+filename+"\'", AT);
		}
		if (signature!=dummy) {
			prn_msg(__FILE__, __LINE__, "err", Date(), "Header in %s should read %s, not %s", filename.c_str(), signature.c_str(), dummy.c_str());
			return false;
		}
		fin.close();
	} else if (ext=="none") {
		return false;
	} else {
		std::ofstream fout;
		fout.open(filename.c_str(),  std::ios::out);
		if (fout.fail())
			return false;

		if (ext=="err") {
			fout << "[SNOWPACK_ERROR_LOG]\n";
			fout <<  "         RUNTIME :  STN LOC LINE MSG [JULIAN]";
		} else if (ext=="met") {
			writeMETHeader(Xdata, fout);
		} else if (ext=="pro") {
			writeProHeader(Xdata, fout);
		} else if (ext=="prf") {
			writePrfHeader(Xdata, fout);
		} else {
			prn_msg(__FILE__, __LINE__, "wrn", Date(), "No header defined for files *.%s", ext.c_str());
		}
		fout.close();
	}

	return true;
}

bool AsciiIO::writeHazardData(const std::string& /*stationID*/, const std::vector<ProcessDat>& /*Hdata*/,
                              const std::vector<ProcessInd>& /*Hdata_ind*/, const size_t& /*num*/)
{
	throw IOException("Nothing implemented here!", AT);
}
