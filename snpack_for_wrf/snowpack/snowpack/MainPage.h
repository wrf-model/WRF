/***********************************************************************************/
/*  Copyright 2009-2010 WSL Institute for Snow and Avalanche Research    SLF-DAVOS */
/***********************************************************************************/
/* This file is part of SNOWPACK.
    SNOWPACK is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    SNOWPACK is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with SNOWPACK.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MAINPAGE_H
#define MAINPAGE_H

//groups
/*! \defgroup postprocessing Post-processing
   Documentation for all method applied as post-processing, after the profile has been computed.
*/

/*! \defgroup data_structures Data structures
   Documentation for the internal data structures.
*/


 /**
 * @mainpage Table of content
 * -# External Links
 *    -# <A HREF="https://models.slf.ch/p/snowpack/">Snowpack's home page</A>
 *          -# <A HREF="https://models.slf.ch/p/snowpack/page/Getting-started/">Installation, compilation</A>
 *          -# <A HREF="https://models.slf.ch/p/snowpack/page/GettingHelp/">Getting help</A>
 * -# End User documentation
 *    -# \subpage getting_started "Getting Started"
 *    -# Model principles
 *        -# \subpage general "General concepts"
 *        -# \subpage references "References"
 *        -# \subpage uses "Use cases"
 *    -# Inputs / Outputs
 *        -# \subpage requirements "Data requirements"
 *        -# \subpage snowpackio "Data file formats"
 *    -# Simulation tools
 *        -# \subpage configuration "Configuring a simulation"
 *        -# \subpage snowpack_visualization "Visualization of the results"
 * -# Programing using %Snowpack
 *        -# \subpage libsnowpack_basics "Programming with libsnowpack"
 * -# Expanding %Snowpack
 *        -# \subpage coding_style "Coding style"
 *        -# \subpage adding_extra_models "Adding extra models"
 * 
 * <center><hr></center>
 * <center><i><small><p>
 * SNOWPACK is a multi-purpose snow and land-surface model, which focusses on a detailed description of the mass and energy exchange between the snow,
 * the atmosphere and optionally with the vegetation cover and the soil. It also includes a detailed treatment of mass and energy fluxes within these media.
 *
 * SNOWPACK has originally been developed to support avalanche warning (Lehning et al., 1999) and thus features a very detailed description of snow properties
 * including weak layer characterization (Stoessel et al., 2009), phase changes and water transport in snow (Hirashima et al., 2010).
 * A particular feature is the treatment of soil and snow as a continuum with a choice of a few up to several hundred layers. While a main application
 * is still on avalanche warning in countries from Switzerland (Schirmer et al., 2009) to Japan (Nishimura et al., 2005), the applications range
 * from climate change assessments (Rasmus et al., 2004; Bavay et al., 2009) and superimposed ice simulations (Obleitner and Lehning, 2004) to
 * permafrost sensitivity studies (Luetschg et al., 2008) and the simulation of snow storage (Olefs and Lehning, 2010).
 *</p>
 * <br>
 * <p>
 * In order to ease the integration of SNOWPACK into other models, it is now structured as a library (libsnowpack) and an application that uses the library to perform
 * simulations (snowpack). This library is available under LGPL version 3 or above, see <a href="https://www.gnu.org/licenses/lgpl.txt">www.gnu.org</a>.
 * The Visual C++ version uses a BSD-licensed port of getopt for Visual C++, with a \subpage getopt_copyright "BSD copyright notice".
 * </p></small></i></center>
 */

/**
 * @page getting_started Getting Started
 * After you installed a binary package or compiled and installed %Snowpack, you can run your first simulation.
 * Please make sure you properly set the proper environement variables for your operating system:
 *      - on osX: set \em PATH and \em DYLD_FALLBACK_LIBRARY_PATH
 *      - on Linux: set \em PATH and \em LD_LIBRARY_PATH if you install the package to a non-standard location
 *      - on Windows: set \em PATH
 * How to do this (and much more) is explained in the online documentation at https://models.slf.ch/p/snowpack/page/Getting-started/.
 *
 * @section Running_an_example Running an example simulation
 * In order to run an example simulation, please follow the steps below:
 * -# First, copy the examples as provided with this documentation (the whole \b doc/examples directory) to a directory where you have write access.
 * -# Open a terminal and go to the directory where you copied the simulation you want to run
 * -# Select one of the examples and run its start script, for example <i>./run_res1exp.sh</i> (on Windows, you have to open the script file and copy
 *    the last command line it contains into a terminal). You can also manually run %Snowpack, by typing something like
 *    <i>snowpack -c {ini file with path} -e {simulation end date in ISO format}</i>.
 * -# Once the simulation is finished, the results are available in the \b output directory. This directory \b must exist before you run the simulation!
 * -# The results can be visualized using the \ref sngui_config "sngui tool" by opening the <b>.pro</b> file that was generated in \b output.
 *
 * @section Running_own_simulation Running your own simulation
 * Once you have been able to run an example simulation, you can try to run your own simulation. This involves the following steps:
 * -# First, gather the meteorological data that you need to drive the simulation. Please have a look at \subpage requirements "Data requirements";
 * -# Then, write the data in a format that <a href="https://models.slf.ch/p/meteoio">meteoio</a> can read for %Snowpack, for example SMET (see the file
 *    format specification included in the meteoio's documentation and follow it);
 * -# Once your data is ready, you can \subpage configuration "configure your simulation", using <a href="https://models.slf.ch/p/inishell">inishell</a>. Please keep in
 *    mind that the default choices in inishell are such that if you don't change them, a simple simulation should work. And do \b not change parameters in
 *    the SnowpackAdvanced section! (this section is reserved for some specific use cases where a deeper control on the operation of the model is required).
 * -# Then, run the simulation from a terminal (after going to the directory where your simulation is) with a command line such as
 *    <i>snowpack -c {ini file with path} -e {simulation end date in ISO format}</i>.
 * -# Once the simulation is finished, the results are available in the \b output directory. This directory \b must exist before you run the simulation!
 * -# The results can be visualized using the \ref sngui_config "sngui tool" by opening the <b>.pro</b> file that was generated in \b output.
 *
 * @section model_workflow Simulation workflow
 * When running a simulation, it is important to keep in mind that the model is organized as several modules that interract together. It is possible to configure
 * some parameters for the various modules and to enable/disable modules. Some modules can be used outside of Snowpack (like
 * <A HREF="https://models.slf.ch">MeteoIO</A> that is used in various applications or libSnowpack that is used by <A HREF="https://models.slf.ch">Alpine3D</A>) .
 *
 * \image html simulation_workflow.png "Simulation workflow"
 * \image latex simulation_workflow.eps "Simulation workflow" width=0.9\textwidth
 */

/**
 * @page general General concepts
 * The one-dimensional snow cover model SNOWPACK (Lehning et al., 1999; Bartelt and Lehning, 2002; Lehning et al., 2002a, b), 
 * was primarily developed for the support of avalanche warning in Switzerland.
 * However, this physical model is also used for other applications such as permafrost investigations (Lütschg et al., 2003),
 * the assessment of snow – vegetation interactions, climate research (Rasmus and Räisänen, 2003; Bavay et al., 2009), mass- and energy balance
 * calculations for arctic areas (Meirold-Mautner and Lehning, 2003) and calculations of chemical solute transport in snow (Waldner et al., 2003).
 *
 * @section physical_processes Physical processes
 * \image html physical_processes.png "Principal physical processes included in the SNOWPACK model"
 * \image latex physical_processes.eps "Principal physical processes included in the SNOWPACK model" width=0.9\textwidth
 *
 * A graphical review of the physical processes described by the SNOWPACK model is given in the above figure. SNOWPACK is based on a Lagrangian
 * finite element implementation and solves the instationary heat transfer and settlement equations. Phase changes and transport of water vapor and
 * liquid water are included. Special attention is paid to the metamorphism of snow and its connection to mechanical properties such as thermal
 * conductivity and viscosity. At present, SNOWPACK is the most advanced snow cover models worldwide in terms of microstructural detail.
 * Therefore, first attempts are being made to estimate snow cover stability from SNOWPACK simulations (Lehning et al., 2003).
 *
 * @section model_structure Structure of the physical modeling
 * @subsection model_foundations Model Foundations
 * \image html snowpack_column.png "The SNOWPACK soil/snow/canopy column"
 * \image latex snowpack_column.eps "The SNOWPACK soil/snow/canopy column" width=0.5\textwidth
 * The SNOWPACK model is built around a 1D soil/snow/canopy column (see figure above). This in effect neglects lateral transfers and only considers vertical
 * gradients and transfers. The snow is modeled as a three phase porous medium (ice/liquid water/water vapor) but can also contain an arbitrary amount of soil
 * in order to simulate from purely soil layers to snow layers, including ice lenses, permafrost, ponding, etc. An arbitrary number of layers can be simulated,
 * according to needs. Because of its lagragian grid, SNOWPACK can simulate very thin layers if needed (ice crust, hoar).
 *
 *
 * @subsection model_hierarchy Model Hierarchy
 * At the core of the model, are a few state variables. These are used to model the microstructure, including its
 * metamorphic developments. This in turns allows to build bulk constitutive properties that are necessary to compute the core conservation equations. Finally, a few
 * "side models" provide the necessary connections to meteorological forcing, real world measurements and parameters and other properties. Therefore the model
 * can be described by the following hierarchical structure (see figure below):
 *    - a few state variables: density, temperature, liquid water content that must be known for each layer;
 *    - from these state variables, four primary (independent) microstructure parameters are derived: grain size, bond radius, sphericity and dendricity. As a matter
 *      of convenience, the coordination number will also be computed and used. Equilibrium growth metamorphism and kinetic metamorphism will drive the
 *      temporal evolution of these parameters according to various models.
 *    - from the microstructure parameters, the macroscopic properties will be computed: thermal conductivity and viscosity. This will include pressure sintering,
 *      the strain amplification on the necks and its feedback on the metamorphism as well as both linear and non-linear viscosity ranges.
 *    - these bulk properties then allow computing the core equations: mass and energy conservation as well as the settling. The energy balance will include
 *      the solar radiation absorption, the sublimation/deposition of water vapor, the melting and refreezing of water as well as the heat conduction. By enforcing
 *      the three phase approach, only two mass conservations will be required: for the liquid water and the water vapor.
 *    - finally, auxiliary models provide the necessary "glue": the snow surface and ground boundary conditions (using either Neumann to enforce fluxes or Dirichlet to
 *      enforce temperatures or even swapping between the two depending on the conditions), a new snow density parametrization (that depends on the local climate),
 *      an albedo and short wave absorption parametrization and a snowdrift model.
 *    - some post-processing models will be added to provide more relevant outputs: a hardness model, several snow stability index, a snow classification.
 *
 * \image html snowpack_physics.png "Structure of the SNOWPACK model"
 * \image latex snowpack_physics.eps "Structure of the SNOWPACK model" width=0.9\textwidth
 *
 * The user can configure variants of these basic model concepts. The way of interaction is primarily through a configuration file but also changes to the source code by
 * users are in principle possible. In any case, a sufficient understanding of the modeling concept will ensure a safe operation of the model.
 *
 * @subsection model_ebalance Energy Balance
 * The figure below shows the various fluxes that are part of the energy balance of the SNOWPACK model. These are available in the output files as well as
 * through the sngui interface.
 *
 * \image html energy_balance.png "Energy Balance components of the SNOWPACK model"
 * \image latex energy_balance.eps "Energy Balance components of the SNOWPACK model" width=0.9\textwidth
 *
 */

/**
 * @page references References
 * In the following some important papers related to the SNOWPACK model are listed. Additional information can be found on the web:
 * www.slf.ch/lwr/prozessmodelle/aufgaben-en.html.
 *
 * - Lehning, M., Bartelt, P., Brown, R.L., Russi, T., Stöckli, U., Zimmerli, M., <i>%Snowpack Model Calculations for Avalanche Warning based upon
 *   a new Network of Weather and Snow Stations</i>, 1999, Cold Reg. Sci. Technol., \b 30, 145-157.
 * - Lehning, M, Bartelt, P.B., Brown, R.L., Fierz, C., Satyawali, P., <i>A physical SNOWPACK model for the Swiss Avalanche Warning Services.
 *   Part II: Snow Microstructure</i>, 2002, Cold Reg. Sci. Technol., \b 35/3, 147-167.
 * - Lehning, M, Bartelt, P.B., Brown, R.L., Fierz, C., Satyawali, P., <i>A physical SNOWPACK model for the Swiss Avalanche Warning Services.
 *   Part III: Meteorological Boundary Conditions, Thin Layer Formation and Evaluation</i>, 2002, Cold Reg. Sci. Technol., \b 35/3, 169-184.
 * - Fierz, C., P. Riber, E.A. Adams, A.R. Curran, P.M.B. Föhn, M. Lehning and C. Plüss, <i>Evaluation of snow-surface energy balance models in alpine terrain</i>,
 *   2003, J. Hydrol., \b 282, 76–94.
 * - Obleitner, F., Lehning, M., <i>Measurements and simulation of snow and superimposed ice at the Kongsvegen glacier, Svalbard (Spitzbergen)</i>,
 *   2004, J. Geophys. Res., \b 109D, D04106.
 * - Yamaguchi, S., Sato, A., Lehning, M., <i>Application of the numerical snowpack model (SNOWPACK) to the wet snow region in Japan</i>,
 *   2004, Ann. Glac., \b 38, 266-272.
 * - Rasmus, S., Räisänen J., Lehning, M., <i>Estimating snow conditions in Finland in the late 21st century using the SNOWPACK model
 *   with regional climate scenario data as input</i>, 2004, Ann. Glac., \b 38, 238-244.
 * - Hirashima, H., Nishimura, K., Baba, E., Hachikubo, A., Lehning, M., <i>SNOWPACK model simulations for snow in Hokkaido, Japan</i>, 2004, Ann. Glac., \b 38, 123–129.
 * - Nishimura, K., Baba, E., Hirashima, H., Lehning, M., <i>Application of SNOWPACK model to snow avalanche warning in Niseko, Japan</i>, 2005,
 *   Cold Reg. Sci. Technol., \b 43, 62-70.
 * - Rasmus, S., Gronholm, T., Lehning, M., <i>Validation of the SNOWPACK model in five different snow zones in Finland</i>, 2007, Boreal Env. Res., \b 12(4), 467-488.
 * - Lehning, M., Fierz, C., <i>Assessment of snow transport in avalanche terrain</i>, 2008, Cold Reg. Sci. Technol., \b 51, 240-252, DOI: 10.1016/j.coldregions.2007.05.012.
 * - Luetschg, M., Lehning, M., Haeberli, W., <i>A sensitivity study of factors influencing warm/thin permafrost in the Alps</i>, 2008, J. Glaciol., \b 54/187, 696-704.
 * - Schirmer, M., Lehning, M., Schweizer, J., <i>Statistical forecasting of avalanche danger using simulated snow cover data</i>, 2009, J. Glaciol., \b 55/193, 761-768.
 * - Stoessel, F., Manes, C., Guala, M., Fierz, C., Lehning, M., <i>Micrometeorological and morphological observations of surface hoar dynamics on a
 *   mountain snow covers</i>, 2009, Water Resour. Res., doi:10.1029/2009WR008198.
 * - Olefs, M., Lehning, M., <i>Textile protection of snow and ice: Measured and simulated effects on the energy and massbalance</i>, 2010, Cold Reg. Sci. Technol.,
 *   doi:10.1016/j.coldregions.2010.03.011.
 * - Schirmer, M., Lehning, M., Schweizer, J., <i>Statistical evaluation of local to regional snowpack stability using simulated snow-cover data</i>,
 *   2010, Cold Reg. Sci. Technol., \b 64/2, 110-118, doi: 10.1016/j.coldregions.2010.04.012.
 * - Hirashima, H., Yamaguchi, S., Sato, A., Lehning, M., <i>Numerical modeling of liquid water movement through layered snow based on
 *  new measurements of the water retention curve</i>, 2010, Cold Reg. Sci. Technol., \b 64/2, 94-103, doi: 10.1016/j.coldregions.2010.09.003.
 * - Bellaire, S., J.B. Jamieson, and C. Fierz, <i>Forcing the snow-cover model SNOWPACK with forecasted weather data</i>, 2011,
 *   The Cryosphere, \b 5, 1115–1125, 2011, http://dx.doi.org/10.5194/tc-5-1115-2011.
 *
 */

/**
 * @page uses Use cases
 * SNOWPACK is widely used for research purposes all around the world in more than 35 institutions. The model has been successfully applied to the Alps,
 * Scandinavia, Northen America, Japan, Russia, even Antartica. It has lead (together with Alpine3D, its spatially distributed derivative) to more than
 * 60 ISI publications. It has been used for hydrological studies, climate change impact studies, snow stability questions, road weather applications,
 * permafrost research, snow farming...
 *
 * @section current_op_usage Operational usage experience
 * SNOWPACK runs operationally
 * on a network of high Alpine automatic weather and snow measurement stations. Presently approximately 100 sites are in operation. These stations
 * measure wind, air temperature, relative humidity, snow depth, surface temperature, ground (soil) temperature, reflected short wave radiation and
 * three temperatures within the snowpack. The measurements are hourly transferred to the SLF and drive the model. SNOWPACK produces supplementary
 * information regarding the state of the snowpack at the sites of the automatic stations. The model is connected to a relational data base which
 * stores the measurements as well as the model results. Validations of SNOWPACK suggest that the calculations are reliable in terms of the mass balance
 * and the energy budget. The implemented snow metamorphism formulations yield reasonable grain types and are able to reproduce important processes
 * such as the formation of depth or surface hoar.
 *
 * @section other_uses Other uses
 * In addition to the stand-alone applications, SNOWPACK is increasingly used in a distributed way (Kuonen et al., 2010).
 * SNOWPACK has been coupled with atmospheric flow and snow drift modules as well as with spatial energy balance models. The coupled models are
 * used to investigate snow deposition and snow cover development in steep terrain (Lehning and others, 2000) and to forecast ski run conditions for racing
 * (however, the current version of SN_GUI does not include the visualization of distributed SNOWPACK calculations).
 *
 * In order to make it easier to integrate %Snowpack in other models, it has been repackaged as a library. You can therefore use the %Snowpack library in another
 * model. More details are given in \subpage libsnowpack_basics "Programming with libsnowpack".
 */

/**
 * @page getopt_copyright BSD copyright notice
 * This copyright notice applies to files applications/snowpack/getopt.* and getopt_long.* as used on the MS Windows
 * platform. All other files in this product are exclusively covered by the <a href="https://www.gnu.org/licenses/lgpl.txt">LGPL version 3</a> 
 * or above, or <a href="https://www.gnu.org/licenses/gpl.txt">GPL version 3</a> or above unless otherwise specified.
 *
 * Copyright (c) 1987, 1993, 1994 The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

/**
 * @page requirements Data requirements
 * %Snowpack performs physical modeling of the various processes taking place between the soil, snow cover and atmosphere in order to
 * simulate the evolution of the snow cover based on meteorological input data. It requires the following meteorological parameters:
 * - air temperature (TA)
 * - relative humidity (RH)
 * - wind speed (VW)
 * - incoming short wave radiation (ISWR) <i>and/or</i> reflected short wave radiation (RSWR)
 * - incoming long wave radiation (ILWR) <i>and/or</i> surface temperature (TSS)
 * - precipitation (PSUM) <i>and/or</i> snow height (HS)
 * - ground temperature (TSG, if available. Otherwise, you will have to use <a href="https://models.slf.ch">MeteoIO</A>'s 
 * data generators to generate a value) <i>or</i> geothermal heat flux
 * - snow temperatures at various depths (TS1, TS2, etc if available and only for comparisons, see section \ref SnowSoilTemperatures)
 *
 * These parameters <b>should best</b> be available at a hourly time step and preferably in MKSA units 
 * (please check the MeteoIO plugins documentation for specific cases, like GRIB, NetCDF... that are automatically handled). Please have a look 
 * at the \ref snowpackio "other input parameters" that are required to run your simulation!
 *
 * @section data_preparation Data preparation
 * In order to help %Snowpack handle the (sometimes broken) data sets to be used in a simulation, the <a href="https://models.slf.ch/p/meteoio">MeteoIO library</a> is used.
 * This enables %Snowpack to get data from a variety of sources (several input file formats, connection to a database, connection to a web service) and to
 * pre-process real-world data, by filtering the data on the fly and by resampling the data on the fly. Please read the MeteoIO documentation (available 
 * <A HREF="https://models.slf.ch/docserver/meteoio/html/index.html">online</A> for the last official release) to learn about
 * the supported file formats, the available filters and resampling/re-accumulation strategies as well as the available parametrizations that can help generate
 * some otherwise missing data (either from other parameters or fully synthetic, as last resort).
 * 
 * It is recommended to prepare the data in the
 * <A HREF="https://models.slf.ch/docserver/meteoio/html/smetio.html">SMET</A> file format for its ease of use.
 *
 * @section data_recomendations Data recommendations
 * In case incoming and reflected short wave radiation as well as incoming long wave radiation are all
 * measured under ventilated and heated conditions, the best approach in terms of energy flux calculations seems to be by using:
 * @code
 * SW_MODE = "BOTH"    ; that is, incoming and reflected short wave radiation are both measured)
 * CHANGE_BC = false   ; that is, Neumann boundary conditions throughout
 * @endcode
 *
 * In case you only have reflected short wave and snow surface temperature, using Dirichlet boundary condition would be recommended:
 * @code
 * SW_MODE = "REFLECTED" (that is, reflected short wave radiation is available)
 * MEAS_TSS = true    ;our surface temperature measurement is good and can be used for validation criteria
 * CHANGE_BC = true
 * @endcode
 * For energy balance interpretation the change of internal energy is for that case better than the sum of fluxes.
 *
 * @section data_checks Data checks
 * Please keep in mind that any inaccuracy on the input parameters will have an impact on
 * the quality of the simulation. Since the modelling is physically-based, manually re-constructing missing data can lead to completely wrong results if not
 * done with great care (the model performing various checks on the physical consistency of the input data, it \b will exclude data points that are not consistent
 * with the other parameters. For example, precipitation occuring simultaneously with quite dry air will be refused).
 *
 * \image html clear_sky.png "Data consistency check"
 * \image latex clear_sky.eps "Data consistency check" width=0.9\textwidth
 * For example, the figure above allows to check the following points:
 * - the (solid) precipitation are synchronized with the major snow height increase - this is consistent;
 * - the precipitation happen during hight relative humidity periods - this is consistent;
 * - during times of precipitation, the air and surface temperatures remain close - this means that the sky is cloudy, this is consistent;
 * - a few periods of low wind speed coincide with high relative humidity, which could lead to surface hoar formation - look for it in the simulation outputs;
 * - early in the season, two periods of high wind speed could have lead to drifting snow. The first one occurs during a large snow fall and therefore might be hidden in the data while the second period that experiences a strong snow height decrease could also be driven by rapidly increasing air temperatures - the precipitation might show a large undercatch while the snow might have been wind pressed;
 * - late in the data set, the snow height measurements fail for an extended period of time at a time of high wind speed - some snow drift might have gone unnoticed.
 *
 * When using spurious data or when faced with a bad behaving simulation, one should <b>first look at the consistency of the input data</b>. The vast majority of the problems can be traced back to some low quality data (either for sensor issues or spurious data manipulation at some stage).
 *
 * @section SnowSoilTemperatures Snow and/or soil temperatures
 * Up to 5 snow and/or soil temperatures, either measured or modelled or both can be monitored.
 * Measured temperatures are read in from the input file. If you use the smet format, do not forget to properly
 * label the columns as TS1, TS2, TS3, etc. If you use the snio format, refer to the documentation.
 * User defined positions (m) should be provided in the SnowpackAdvanced section of the \em "io.ini" file,
 *   for example, FIXED_POSITIONS = "0.25 0.50 -0.10":
 *   - positive values refer to heigths measured from the ground surface (snow only)
 *   - negative values refer to depths measured from either the ground surface or the snow surface in case no soil
 *       layers are present
 *   - A sensor must at least be covered by MIN_DEPTH_SUBSURF (m) snow for its temperature to be output.
 *       This parameter can be set in the SnowpackAdvanced section of the io.ini file.
 * 
 * @section PrecipPhase Precipitation phase
 * If the precipitation phase is available, you can provide it along the meteorological input under the name "PSUM_PH". This phase represents
 * the fraction of liquid precipitation; for example:
 *      - it is set to zero for fully solid precipitation;
 *      - it is set to one for fully liquid precipitation;
 *      - it is set to 0.75 for mixed precipitation made of 75% liquid and 25% solid.
 * 
 * If no precipitation phase is provided, Snowpack will rely on a fixed threshold approach: for air temperatures above 
 * the value given by THRESH_RAIN in the SnowpackAdvanced section of the \em "io.ini" file, liquid precipitation is 
 * assumed while in the contrary fully solid precipitation is assumed.
 * 
 * It is possible to use other spliting schemes by defining a <b>data generator</b> in the \em "io.ini" file (see in MeteoIO's documentation 
 * the section <i>"Available data generators and usage"</i> for the full list of available generators):
 * @code
 * [Generators]
 * PSUM_PH::generators   = PPHASE
 * PSUM_PH::pphase::type = RANGE
 * PSUM_PH::pphase::snow = 273.35
 * PSUM_PH::pphase::rain = 275.35
 * @endcode
 * 
 */

/**
 * @page configuration Configuring a simulation
 * The configuration for a given simulation is kept in a <i>".ini"</i> file (see http://en.wikipedia.org/wiki/INI_file). This is an ascii file that contains
 * keys/values structured by sections. This can be easily edited with a simple text editor. More information about the structure of the file and how to generally deal
 * with it can be found in MeteoIO's documentation (section "How to build your io.ini configuration file"). However, it is recommended to use the inishell tool for
 * generating the configuration file for %Snowpack in order to prevent missing important keys, etc Please read <a href="https://models.slf.ch">MeteoIO</A>'s documentation (specially the "general
 * Concepts" introduction)!
 *
 * @section inishell_config The inishell tool
 * It is highly recommended to use the <a href="https://models.slf.ch/p/inishell">Inishell</a> tool to generate these ini files
 * in order to reduce editing errors. This tool also allows you to edit an existing file in order to change the configuration.
 * \image html inishell.png "inishell overview"
 * \image latex inishell.eps "inishell overview" width=0.9\textwidth
 * Each ini file section is shown in a separate tab, each key on its own line. Keys that appear in red are mandatory and will trigger a warning box if trying to
 * visualize/save an ini file before providing a value for these keys. A help text describing the key is shown on the right. Once all has been configured,
 * the configuration can be saved to an ini file, ready to be interpreted by %Snowpack.
 *
 * @section advanced_cfg Advanced configuration
 * The configuration files being an ascii format (<a href="https://en.wikipedia.org/wiki/INI_file">INI format</a>), it is possible to manually
 * copy/paste whole sections of such files between simulations in order to run similar simulations without the need to re-type the whole configuration.
 * It is also possible to use inishell in order to find out which keys are available, which values they can take and what they mean and manually type
 * them in an existing ini file.
 *
 * The %Snowpack_advanced section contains settings that previously required to edit the source code and recompile the model. Since these settings
 * deeply transform the operation of the model, please <b>refrain from using them</b> if you are not absolutely sure of what you are doing.
 *
 */

/**
 * @page snowpack_visualization Visualization tools
 * The simulation outputs are usually saved in \a ".pro" files for the time resolved profiles and \a ".met" files for the meteorological data time series
 * (see section \subpage snowpackio "Snowpack file formats"). These files can be processed with some scripts, relying on GNU plot or R for generating graphs
 * but are usually viewed with a graphical application. Two such applications are currently available: the legacy SnGUI Java tool and the newly developed 
 * SnopViz javascript tool.
 * 
 * @section sngui_config The sngui tool
 * This java application can be  <a href="https://models.slf.ch/p/sngui/">downloaded</a> after registering (and requesting access) on the web site.
 * \image html sngui_overview_small.png "sngui overview"
 * \image latex sngui_overview.eps "sngui overview" width=0.9\textwidth
 *
 * @section snopviz The SnopViz tool
 * This javascript application work in any sufficiently recent web browser ( firefox >= 33.0, Safari >= 5.1, Internet Explorer >= 11.0, 
 * Chrome >= 38). You can either use it <a href="https://models.slf.ch/snopviz">online</a> and then open your profile to visualize or you can 
 * <a href="https://models.slf.ch/snopviz">download</a> a pre-packaged version that can be installed for offline use on your computer.
 * \image html snopviz_small.png "SnopViz overview"
 * \image latex snopviz.eps "SnopVizi overview" width=0.9\textwidth
 * 
 */

/**
 * @page libsnowpack_basics Programming with libsnowpack
 * %Snowpack is now distributed as a very simple application that delegates most of the work to a library, libsnowpack. This C++ library can easily be integrated in
 * other models/applications. In order to do so, the following header have to be included:
 * @code
 * #include <snowpack/libsnowpack.h>
 * #include <meteoio/MeteoIO.h>
 * @endcode
 * Usually, MeteoIO is used to get the meteorological data and the meteoio meteo data class (mio::MeteoData) is converted to %Snowpack meteo data class (CurrentMeteo).
 * The %Snowpack specific configuration options are stored in a SnowpackConfig class that is passed to the various other objects.
 *
 * The main computation is performed by the Snowpack class that needs the following data for its Snowpack::runSnowpackModel call:
 * - the meteo data for the current timestamp as a CurrentMeteo object;
 * - the information about the specific location where the snowpack is simulated contained in a SnowStation object;
 * - the boundary conditions in a BoundCond object;
 * - the surface fluxes in a SurfaceFluxes object.
 *
 * In order to initialize some of these objects from data stored in files, a helper class has been designed: SnowpackIO. Of interest are the following calls:
 * SnowpackIO::readSnowCover, SnowpackIO::writeSnowCover, SnowpackIO::writeProfile, SnowpackIO::writeTimeSeries. Please keep in mind that the %Snowpack
 * parameters are internally always given **perpendicularly to the ground**.
 *
 * In order to compute hazard relevant data, the Hazard class has been designed. The stability data is computed by the Stability class. Some information has
 * to be exchanged between the SnowpackIO object and the Hazard and/or Stability objects. This is handled by the SN_SNOWSOIL_DATA and ZwischenData classes.
 *
 * In order to ease debugging, these classes redefine the "<<" operator to show in a compact way their relevant content. For example,
 * @code
 * SnowStation station;
 * std::cout << station;
 * @endcode
 * shows the relevant parameters of "station":
 * @code
 * <SnowStation>
 * <station>
 * <Coords>
 * Altitude        -999
 * Lat/Long        (-999°0'0.000000" , -999°0'0.000000")
 * Lat/Long        (-999 , -999)
 * X/Y_coords      (-999 , -999)
 * I/J_indices     (-999 , -999)
 * Projection      NULL NULL
 * EPSG            -1
 * </Coords>
 * ID: MST96
 * Name: MST96
 * Slope: -999 bearing: -999
 * </station>
 * 0 element(s) and 0 node(s). Soil=false canopy=false
 * Soil:   SoilNode=0 depth=0 BareSoil_z0=0
 * Albedo: mAlbedo=0 cAlbedo=0 SoilAlb=0
 * Snow:   Measured HS=0 Calculated HS=0 New snow=0 of density=0
 * Energy: ColdContent=0 dIntEnergy=0 SubSurfaceMelt=x SubSurfaceFrze=x
 * Snowdrift:      windward=0 ErosionLevel=0 ErosionMass=0
 * Stability:      S_d(0)=0 S_n(0)=0 S_s(0)=0 S_1=0 S_2=0 S_4(0)=0 S_5(0)=0
 * Kt= 0
 * </SnowStation>
 * @endcode
 *
 * @note As a starting point, you can have a look at the SnowpackInterface and SnowpackInterfaceWorker classes of <a href="https://models.slf.ch/p/alpine3d/source/tree/HEAD/trunk">Alpine3D</A>.
 */

/**
 * @page coding_style Coding style
 * @section coding_sty Recomended coding style
 * The recommended coding style for MeteoIO is the <A HREF="http://www.kernel.org/doc/Documentation/CodingStyle">Kernel coding style</A> with a few exceptions:
 * - we don't enforce strict 80 characters line width. try to remain reasonable, but don't necessarily cut everything off at 80 characters
 * - try to intelligently use spaces to visually group elements of a complex formula. If the formula can be split into meaningful elements,
 *   please do it (using some "const double element = " constructs).
 * - try to properly qualify variables: for example, if a variable will not be changed, will never be negative and always integer,
 *   then use "const unsigned int". When some specific types are used for some standard library calls, try to properly use these types (for example, "size_t")
 * - use C++ method naming convention: a method name starts with lowercase letter and each individual word in a name starts capitalized.
 *   Usually, no underscores are used in a method. For example, a method that would return the lapse rate contained in an object would be named "getLapseRate()"
 * - qualify variables and parameters with "const" when appropriate (see <A HREF="http://jriddell.org/const-in-cpp.html">const-in-cpp</A>).
 *
 * A few important points to emphasize (from the <A HREF="http://www.kernel.org/doc/Documentation/CodingStyle">Kernel coding style</A>):
 * - Functions should be short and sweet, and do just one thing.  They should fit on one or two screenfuls of text, and do one thing and do that well.
 * - If you have a complex function, and you suspect that a less-than-gifted first-year high-school student might not even understand
 *   what the function is all about, you should adhere to the maximum limits all the more closely.  Use helper functions with descriptive names.
 * - Comments are good, but there is also a danger of over-commenting.  NEVER try to explain HOW your code works in a comment:
 *   it's much better to write the code so that the _working_ is obvious, and it's a waste of time to explain badly written code.
 *
 * @section code_indentation Indentation
 * Since every user has his/her own preference for the ideal indentation width, please use <A HREF="http://www.emacswiki.org/emacs/SmartTabs">"smart tabs"</A>.
 * That practically means:
 * - indent with tabs
 * - align with spaces
 *
 * This way, each developer can set his/her indentation size as he/she wishes without forcing his/her choice to others...
 *
 * @section containers Memory management and Containers
 * Please do NOT manage memory manually but use <A HREF="https://secure.wikimedia.org/wikipedia/en/wiki/Standard_Template_Library">Standard Template Library (STL)
 * </A> <A HREF="http://www.cplusplus.com/reference/stl/">containers</A> instead.
 * This dramatically reduces memory errors (ie: <A HREF="https://secure.wikimedia.org/wikipedia/en/wiki/Segmentation_fault">segfaults</A>), often
 * offers more performance and provides you with lots of <A HREF="http://www.cplusplus.com/reference/algorithm/">associated algorithms</A>
 * (like sorting, search, filling, etc).
 *
 * When you need your own data class, please design it based on these STL containers (like grid2DObject is based on std::vector). Basically, this means
 * that you will replace mallocs and arrays by vectors (for 1d, 2d, 3d grids), maps (for multiple key/value pairs), lists (for unordered table), etc
 *
 * @section exceptions_handling Exceptions handling
 * The recommended C++ usage should be followed: <b>"throw by value, catch by reference"</b> (as specified in <i>C++ Coding Standards: 101 Rules, Guidelines,
 * and Best Practices</i>, Herb Sutter, Andrei Alexandrescu, 2004, Addison-Wesley Professional). Moreover, we should consider catching by
 * <b>const reference</b> and not even declaring a variable if not doing anything with it: something like `catch(const IOException&)` would often be enough.
 *
 */

/**
 * @page adding_extra_models Adding extra models
 * Various processes can already be simulated using different models as configured by the user. This result is achieved by providing a specific model of
 * the process of interest, together with the proper entry in a std::map container that links a model keyword with its implementation. In order to look at the
 * required steps, we will take as an example the hand hardness implementation in the StabilityAlgorithms class. Please keep in mind that when adding a new model to
 * a process that already has multiple available choices, only the first and the third steps are required, the other one being already done.
 *
 * @section model_implementation Model implementation
 * A method has to be implemented in the class with the same prototype as the original method. In our example, the original method (setHandHardnessMONTI)
 * has the following prototype:
 * @code
 * double setHandHardnessMONTI(const ElementData& Edata, const double& buried_hoar_density);
 * @endcode
 * so any alternative implementation must use the same prototype. If some parameters would be ignored by some implementation, simply comment out the unused variable:
 * @code
 * double my_method(const double& param1, const double /*unused_param*/);
 * @endcode
 *
 * @section function_pointer Function pointer typedef
 * All these methods sharing the same prototype, a generic function pointer type can be defined in the Stability class:
 * @code
 * typedef double (*StabMemFn)(const ElementData&, const double&);
 * @endcode
 *
 * @section model_map Model map
 * Once an alternative implementation has been written (and properly declared in the "StabilityAlgorithms.h" header %file), it must be "registered" in the model map. In our exmaple, this map
 * is defined in the "Stability.h" header %file:
 * @code
 * static std::map<std::string, StabMemFn> mapHandHardness;
 * @endcode
 * and statically filled in the initStaticData() method as following:
 * @code
 * const bool Stability::__init = Stability::initStaticData();
 * bool Stability::initStaticData() {
 * 	mapHandHardness["MONTI"]    = &StabilityAlgorithms::setHandHardnessMONTI;
 * 	mapHandHardness["BELLAIRE"]  = &StabilityAlgorithms::setHandHardnessBELLAIRE;
 * 	mapHandHardness["ASARC"]    = &StabilityAlgorithms::setHandHardnessASARC;
 * 	return true;
 * }
 * @endcode
 * This way of fillinf the map ensures that it will be initialized only once and for all, making it consistent and efficient.
 *
 * @section model_user_choice User model configuration
 * The user selection of model must be connected with the proper model implementation. The user selects the model he wants through a key in his
 * configuration file. We need to read this key and activate the proper implementation, knowing that the proper key <-> implementation matching is done
 * through the map:
 * @code
 * cfg.getValue("HARDNESS_PARAMETERIZATION", "SnowpackAdvanced", hardness_parameterization);
 * map<string, StabMemFn>::const_iterator it1 = mapHandHardness.find(hardness_parameterization);
 * if (it1 == mapHandHardness.end()) throw InvalidArgumentException("Unknown hardness parameterization: "+hardness_parameterization, AT);
 * @endcode
 * This means that in the section "SnowpackAdvanced" of his ini file, the key "HARDNESS_PARAMETERIZATION" must contain one of the strings given in the mapHandHardness
 * above (ie. either "DEFAULT" or "MONTI" or "ASARC").
 *
 * @section calling_model Model call
 * Finally, the process model has to be called where needed, so each time the hand hardness has to be computed, the call becomes:
 * @code
 * hardness = (mapHandHardness[hardness_parameterization]))(EMS[e], hoar_density_buried);
 * @endcode
 *
 */

#endif







