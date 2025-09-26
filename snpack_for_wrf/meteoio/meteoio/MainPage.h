/***********************************************************************************/
/*  Copyright 2009-2010 WSL Institute for Snow and Avalanche Research    SLF-DAVOS */
/***********************************************************************************/
/* This file is part of MeteoIO.
    MeteoIO is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MeteoIO is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with MeteoIO.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MAINPAGE_H
#define MAINPAGE_H

namespace mio {
//groups
/*! \defgroup meteoLaws Meteorological Laws
   Documentation for meteorological laws and constants.
*/

/*! \defgroup plugins IO plugins
   Documentation for available IO plugins. Please consider having a look at the \ref plugins "Available plugins and usage" page and the \ref dev_plugins "How to write a Plugin" page.
*/

/*! \defgroup stats Statistical calculations
   Documentation for available statistical calculations. This is heavily used by the \ref processing "Available data processing elements" as well as the \ref resampling "1D interpolations" and \ref interpol2d "2D interpolations".
*/

/*! \defgroup processing Data processing elements
   Documentation for available data processing components. These can be used on incoming meteorological data. See \ref processing "Available data processing elements".
*/

/*! \defgroup parametrizations Parametrizations
   Documentation for available parametrizations components that compute given parameters from other parameters. These used for DataCreator and DataGenerator.
*/

/*! \defgroup spatialization Spatial interpolations
   Documentation for available spatial interpolations algorithms components that compute the spatial distribution of a given parameter. These are mostly wrappers
   around \ref interpol2d "2D interpolations".
*/

/*! \defgroup data_str Data classes
   Documentation for available data classes.
*/

/*! \defgroup graphics Graphical elements and operations
   Documentation for available classes and methods for dealing with graphical elements, color conversions, etc
*/

 /**
 * @mainpage Table of content
 * -# General overview
 *    -# \subpage general "General concepts"
 *    -# \subpage configuration "Configuration file"
 *    -# \subpage build_io "How to build your io.ini configuration file"
 *    -# External Links
 *         -# <A HREF="https://models.slf.ch/p/meteoio/">MeteoIO's home page</A>
 *               -# <A HREF="https://models.slf.ch/p/meteoio/page/Getting-started/">Installation, compilation</A>
 *               -# <A HREF="https://models.slf.ch/p/meteoio/page/GettingHelp/">Getting help</A>
 * -# Processing steps documentation
 *    -# \subpage data_sources "Data input and sources"
 *    -# \subpage raw_data_editing "Raw Data Editing"
 *    -# \subpage processing "Available processing elements" and usage
 *    -# \subpage resampling "Available temporal interpolations" and usage
 *    -# \subpage generators "Available data creators and generators" and usage
 *    -# \subpage interpol2d "Available spatial interpolations" and usage
 *    -# \subpage spatial_resampling "Spatial resampling"
 * -# Advanced: Programing using MeteoIO
 *    -# \subpage workflow "Example Workflow"
 *    -# \subpage quick_overview "Quick overview" of the functionnality provided by MeteoIO
 *    -# <A HREF="modules.html">Modules list</a>
 *    -# \subpage examples "Usage examples"
 * -# Advanced: Expanding MeteoIO
 *    -# How to \subpage dev_coords "write a coordinate system support"
 *    -# How to \subpage dev_plugins "write a Plugin"
 *    -# How to \subpage dev_processing "write a processing element"
 *    -# How to \subpage dev_1Dinterpol "write a resampling algorithm"
 *    -# How to \subpage dev_DataGenerator "Write a data generator"
 *    -# How to \subpage dev_2Dinterpol "write a spatial interpolation algorithm"
 * 
 * <center><hr></center>
 * <p><center><i><small>
 * This library aims at making data access easy and safe for numerical simulations in environmental sciences requiring general meteorological data. A full
 * description of its design goals and its architecture can be found in <br>
 * M. Bavay and T. Egger, <a href="http://www.geosci-model-dev.net/7/3135/2014/gmd-7-3135-2014.pdf"><i>"MeteoIO 2.4. 2: a preprocessing library for meteorological data."</i></a>, Geoscientific Model Development, <b>7.6</b>, 2014, pp 3135-3151.
 * <br>
 * This library is available under LPGL version 3 or above, see <a href="http://www.gnu.org/licenses/lgpl.txt">www.gnu.org</a>.
 * </small></i></center></p>
 */

 /**
 * @page general General concepts
 * A large number of the problems encountered by users of numerical models working on large meteorological data sets can be
 * traced back to the Input/Output functionality. This comes from the focus of the model developers on the core modeling issues at the expanse
 * of the I/O routines that are costly to properly implement. Therefore the I/O routines often lack flexibility and robustness. When using
 * numerical models in operational applications, this becomes a major drawback and a regular source of problems.
 *
 * The MeteoIO library has been designed to address this issue. It is an additional layer between the data and the numerical model,
 * handling the retrieval of data from various data sources as well as the data pre-processing.
 *
 * It is built as a library, which means that it can be transparently integrated within other softwares along other libraries. This would be similar to
 * the way a car is made of several components: there is a frame (for example, for us it could be the MeteoIO library), there is an engine (for a model
 * such as Snowpack, it could be its "libSnowpack" library that does all the heavy work) and on top there is the whole bodywork (all the "glue" that
 * is needed to make the libSnowpack usable by an end user as a standalone application, here called "snowpack-app"). Therefore the same components
 * can be used in other models (the same engine might be used in several cars while the same bodywork might contain different engines).
 *
 * \image html library_analogy.png "Software library analogy: a software is made of several components (libraries)"
 * \image latex library_analogy.eps "Software library analogy: a software is made of several components (libraries)" width=0.4\textwidth
 *
 * @section MeteoIO_structure General MeteoIO structure
 * @anchor general_structure
 * \image html meteoio_workflow.png "MeteoIO workflow"
 * \image latex meteoio_workflow.eps "MeteoIO workflow" width=0.9\textwidth
 * MeteoIO can be seen as a set of modules that is focused on the handling of input/output operations (including data preparation) for numerical simulations in the realm of earth sciences. On the visible side, it offers the following modules, working on a pre-determined set of \ref meteoparam "meteorological parameters" or on parameters added by the developer:
 * - a set of \ref plugins "plugins" for accessing the data (for example, a plugin might be responsible for fetching the raw data from a given database)
 * - a set of \ref raw_data_editing "raw data editing" methods to select/merge/convert the raw data
 * - a set of \ref processing "filters and processing elements" for applying transformations to the data (for example, a filter might remove all data that is out of range)
 * - a set of \ref resampling "resampling" algorithms to temporally interpolate the data at the required timestamp
 * - a set of \ref generators "parametrizations" to generate data/meteorological parameters when they could not be interpolated
 * - a set of \ref interpol2d "spatial interpolation algorithms" (for example, such an algorithm might perform Inverse Distance Weighting for filling a grid with spatially interpolated data)
 * - it is also possible to \ref spatial_resampling "spatially resample" the data  (for example to extract points out of gridded data or generate data from neighbouring stations)
 *
 * Each of these steps can be configured and fine tuned according to the needs of the model and the wishes of the user. Moreover, a few
 * assumptions are made about the data that you are using: each data point has to be associated with a geographic location (defined by some sort
 * of coordinates) and very often you will also need to provide a Digital Elevation Model.
 *
 * @section typical_setup Typical setup
 * \image html typical_setup.png "typical setup of MeteoIO for operational applications"
 * \image latex typical_setup.eps "typical setup of MeteoIO for operational applications" width=0.9\textwidth
 * MeteoIO has been designed to accomodate both the needs of carefully crafted simulations for a specific purpose/study and for the needs of operational
 * simulations that run automatically and unattended. A typical setup for such operational applications consists of a data acquisition system
 * (made of various sensors, usually mounted on a common mast, thus seen as belonging to a station and some system to bring the data back to some sort
 * of data repository), a data storage system that usually has some way of also distributing the data (often a database but sometimes only data files on a disk)
 * and is mostly seen as the data source by the application, some applications using the data and producing results that are published to
 * their end users (either to an automated system that one can connect to or to some visualization tool that one can use to explore the results).
 *
 * In this setup, MeteoIO is the "glue" between the numerical model at the core of the application and the data sources on one hand and the
 * publication system on the other hand.
 *
 */

 /**
 * @page configuration Configuration file
 * @anchor config_doc
 * Since MeteoIO is a library, you, as an end user, will not be directly exposed to it: the library is called by the program that you are using,
 * not directly by yourself. You will basically have to set some parameters in a configuration file that defines how MeteoIO has to behave.
 * This configuration file is often named "io.ini" and follows the <A HREF="http://en.wikipedia.org/wiki/INI_file">INI file format</A> standard.
 *
 * It is highly recommended that you first understand the \ref general_structure "general structure" of MeteoIO before moving forward. We will then show the
 * configuration file syntax and then the configuration file structure.
 *
 * @section Config_syntax Configuration file syntax
 * The configuration inputs/outputs file is divided in sections. Each section name is enclosed in brackets.
 * @code
 * [My_section]
 * bla bla bla
 * @endcode
 *
 * Within each section, you might have comments and/or key/value pairs. The comments start with a "#" or a ";" sign and run until the end of the line. A whole line might be commented out, or only a fraction of it. A key/value pair is a keyword, followed by a "=" sign, followed by the value to associate with this key.
 * @code
 * #This is a commented out line
 * PI = 3.14159   #key/value pair and a comment
 * @endcode
 *
 * A valid value can be an integer, a float, or string, a list of keywords, a mixed list of keywords and numbers...
 * @code
 * TA::algorithms = IDW_LAPSE CST_LAPSE
 * @endcode
 * 
 * A value can also be an environment variable, another key or an arithmetic expression (see the \ref Config "Config class documentation" for more details):
 * @code
 * USERNAME = ${env:LOGNAME}
 * FIRST_NAME = John
 * FULL_NAME = ${FIRST_NAME}_${USERNAME}
 * AGE = ${{3*20-18+pi/18}}
 * @endcode
 *
 * @section Config_structure Configuration file structure
 * MeteoIO imposes a minimum structure to the configuration %file: It must contain the [General], [Input] and [Output] sections. If any filter is to be used, a [Filters] section has to be present and if any spatial interpolation is to be used, an [Interpolations2D] section has to be present. A minimal set of keys has to be there, and potentially a number of optional keys. Moreover, the program that you are using might also impose you some specific keys or sections.
 * The keys and their location in the configuration file (ie: to which section they belong) depends on the module that is actually using them. The optional keys depend on the specific options handled by each specific module (or plugin, or algorithm). Therefore, we can draw the following skeleton:
 * @code
 * [General]
 *
 * [Input]
 * COORDSYS	= CH1903	#mandatory: which coordinate system is used for the geographic coordinates
 * COORDPARAM	= -999		#extra arguments for the chosen coordinate system (often, none)
 * TIME_ZONE    = +1		#default time zone for inputs
 *
 * DEM		= ARC		#plugin to use for reading DEM information
 * #this might be followed by any number of arguments that are specific to this plugin
 *
 * METEO	= A3D		#plugin to use for reading meteorological data
 * #this might be followed by any number of arguments that are specific to this plugin
 *
 * [Output]
 * COORDSYS	= CH1903
 *
 * GRID2D	= ARC		#plugin to use for writing 2D grids
 *
 * [Filters]
 * TA::filter1	= min_max	#first filter to use on the parameter TA
 * TA::arg1::min	= 230	#arguments for this first filter
 * TA::arg1::max	= 330	#arguments for this first filter
 * TA::filter2	= rate		#second filter to use (in chronological order)
 * TA::arg2::max	= 0.01	#arguments for this second filter
 * #add any extra filter that you want to use. They will be applied serially
 *
 * [Interpolations2D]
 * TA::algorithms = IDW_LAPSE CST_LAPSE	#list of algorithms to consider for use for spatially interpolating parameter TA
 * TA::cst_lapse::rate = -0.008 		#parameter for a specific interpolation algorithm for parameter TA
 *
 * @endcode
 *
 * It is also possible (for advanced use) to import another configuration file, see in the \ref config_import "Config class" documentation.
 *
 * @section Finding_docs Where to find the proper documentation
 * As can be seen from the previous example, each plugin, each filter or each interpolation algorithm might have its own parameters. Therefore,
 * this is the documentation of each specific plugin/filter/algorithm that has to be used in order to figure out what can be configured when it is used
 * (see the next sections in the welcome page).
 *
 */

//TODO: Get rid of ?? in [General]
/**
 * @page build_io How to build your io.ini configuration file
 * As shown in \ref config_doc , the operation of MeteoIO is driven by a configuration file. Please note that
 * <b>it is highly recommended to use <a href="https://models.slf.ch/p/inishell">inishell</a></b> to build your io.ini configuration file, since this significantly
 * reduces the number of errors and provides help text for each keys.
 *
 * Anyway, this section will show you how to manually set up a configuration file. Please read
 * \ref general documentation page before starting!
 *
 * You first need to create the various sections:
 * - [General] : The documentation about this section is found in \ref Config. It currently contains the PLUGIN_PATH key that
 *               points to the place where to find the plugins as well as some buffering keys (see BufferedIOHandler).
 *
 * - [Input] : This section contains the list of all the plugins that you want to use as well as their parameters. You can
 *             use one plugin for the meteorological data (key=METEO), one for grids (key=GRID2D), one for the Points Of Interest
 *             (key=POI), one for data assimilation (key=DA), one for landuse (key=LANDUSE) and one for Digital
 *             Elevation Model (key=DEM). Please see \ref plugins for the available plugins. Afterwards, each plugin comes
 *             with its own set of keys, as specified in the plugin's documentation. Morevover, the geographic coordinate
 *             system should often be specified, as explained in \ref coords. For the meteorological parameters, it is also
 *             possible to perform some editing on the raw data, see \ref raw_data_editing.
 *
 * - [Output] : This section is very similar to the [Input] section, but (obviously) for outputing the data.
 *
 * - [Filters] : This section lists the pre-processing that has to be performed on the incoming meteorological data.
 *                It builds a stack of processing elements one after the other one, for each meteorological parameter.
 *                See \ref processing for more information.
 *
 * - [Interpolations1D] : This section deals with temporal resampling of the incoming meteorological data. The goal is
 *                         to be able to take in data at any sampling rate and to extract values at any user given time step
 *                         according to the resampling specifications of the user. The search window size can be given with
 *                         key WINDOW_SIZE that expresses (in seconds) how far a valid point can be searched for when
 *                         re-interpolating a missing value (up to WINDOW_SIZE/2 before and after the requested point).
 *                         See \ref resampling .
 *
 * - [Interpolations2D] : This section deals with the spatial interpolation of meteorological data, based on a provided
 *                         Digital Elevation Model. The goal is to populate two dimensional grids with meteorological
 *                         parameters from point measurements, according to the specifications of the user.
 *                         See \ref interpol2d .
 *
 * The application that you are using might also need its own section(s), check this with your application.
 *
 */

 /**
 * @page workflow Workflow
 * Here is a workflow example showing how meteorological data is requested by the user's application and delivered. This is a simplified view, in order to show the general structure. Requesting grids (2D grids, DEM, etc) is very similar but does not perfom filtering or resampling.
 * \image html workflow_meteoreading.png "simplified meteo reading workflow"
 * \image latex workflow_meteoreading.eps "simplified meteo reading workflow" width=0.9\textwidth
 */

 /**
 * @page quick_overview Quick overview
 * This library contains various classes that have been designed to deal with various sets of problems. This page shows the different sets of problems and what kind of functionnality the library offers to tackle them.
 *
 * @section class_structure Class structure
 * \image html structure.png "simplified class structure"
 * \image latex structure.eps "simplified class structure" width=0.9\textwidth
 *
 * @section iohandler_sec Data reading
 * The class IOHandler provides the meteorological data from the sources selected by the user in its configuration file. This class inherits from IOInterface and is implemented through plugins that are responsible for implementing a given data access (see \ref dev_plugins "Plugins developer's guide" for more information). It therefore proposes a uniform, standardized access to the data that can be meteorological data, gridded data (including Digital Elevation Model (DEM) data or variations like for landuse codes) and tables of coordinates (for special processing at users selected locations). A buffered version of this class exists: BufferedIOHandler that should be preferred. The description of the plugins and their usage can be found in \ref plugins "Available plugins".
 * This class also transparently calls the filtering class, FilterAlgorithms in order to filter the data according to the configuration of the user.
 *
 *
 * @section meteo Meteorological data
 * The data structures designed for storing meteorological data have been split into two classes: MeteoData and StationData.
 * @subsection meteodata MeteoData
 * The class MeteoData stores the measurement data coming from some idealized station. It contains the widest set of meteorological measurements. It can be compared, assigned and set (either using a constructor or by calling a set method). Its meteorological parameters can be directly accessed or using a param() method that takes a enum in order to be able to cycle through the parameters.
 * @subsection stationdata StationData
 * The class StationData contains the metadata of a weather station, that is mostly its location. It supports the comparison operators.
 * @subsection getmeteo_sec Getting the data
 * The getMeteoData method defined in the IOHandler class provides a vector of MeteoData and StationData for the requested time step. More details are given in \ref iohandler_sec .
 *
 *
 * @section arrays Arrays related functionnalities
 * @subsection arrays_sec Arrays
 * The classes Array, Array2D and Array3D are designed for the storage and handling of 1D, 2D, 3D arrays in memory. These classes provide access to a given element (read/write), sizing or resizing of an existing array as well as clearing an array. They also provide the minimum and the maximum of the values that are stored in the array. Finally, a subset of an array can be extracted.
 * @subsection grids_sec Grids
 * Built on top of the arrays, defined as classes Grid2DObject and Grid3DObject, the grids add the geolocalization. This means that the coordinates of the lower-left corner of the array are stored as well as the cellsize. They can be built manually, or by providing an array. A subset constructor is available, allowing to extract a subset of the grid. It is also possible to get the lat/long (in WGS84) coordinates matching an (i,j) coordinate in the grid. Finally, It is possible to test for geolocalization equality (ie: do two grids have the same geolocalization).
 * @subsection dem_sec Digital Elevation Models
 * The last layer for gridded data is class DEMObject. Various parameters that are specific to Digital Elevation Models (DEM) are added: for each grid point, the slope, the azimuth, the curvature as well as the normal vector are defined (an optional parameter can be used to select the algorithm to be used). The minimums and maximums (over the grid) for each of these parameters are available. A subset of the DEM can be extracted using the subset constructor.
 *
 *
 * @section proj_sec Geographic projections
 * The class Coords is dedicated to geographic projections. It can use projections provided by <a href="http://trac.osgeo.org/proj/">libproj4</a> or
 * internal implementations provided by the CoordsAlgorithms static class alongside a set of helper algorithms (such as grid rotation, datum conversion, etc).
 * @subsection coord_conv Coordinate conversion
 * The class Coords takes one or two arguments describing the coordinate system of the input data and then converts back and forth with lat/long WGS84. It can be used to construct a local coordinate system, that is to say a metric grid whose origin is chosen by the user (through the lat/long parameters provided to the constructor). This is useful when working with multiple gridded coordinate system in order to get a common system that would still allow easy distances calculations. See the supported \ref Coordinate_types "projections".
 * @subsection dist_sec Distances
 * A few method used internally to work with custom, local grids are exposed to the user in order to easily compute distances beetwen points (using their lat/long). The algorithms can optionnaly be chosen (otherwise a default choice is used).
 *
 *
 * @section interpol_sec Interpolations
 * @subsection interpol2d_sec Spatial interpolations
 * The class Meteo2DInterpolator receives a Digital Elevation Model (DEM) in its constructor as well as two vectors, one of MeteoData the other one of StationData. Then it allows filling 2D grid (as Grid2DObject) with spatially interpolated meteorological parameters.
 * @subsection interpol1d_sec 1D interpolations
 * The ResamplingAlgorithms class uses the Interpol1D class to perform temporal interpolations (for resampling).
 *
 * @section config_sec Configuration files handling
 * In order to offer a consistent interface to the user as well as make it easy to read configuration parameters, the class Config is exposed. Once constructed with a configuration file name, each key's parameter can be retrieved with a call to the templatized getValue() method.
 *
 *
 * @section date_sec Dates handling
 * Dates should be constructed as a Date object. Then, it is easy to built a new date from a julian date, from an ISO formatted date string, from a date split in fields or from a UNIX date (number of seconds since epoch). Then the various representation of the date can be retrieved, the date arithmetics can be done (for example, get the date that is 1 year, 3 months, 15 hours after 2008-12-01T11:54:00) as well as comparisons. The date printing can be controlled by keywords.
 *
 *
 * @section exceptions_sec Exceptions
 * A few customized exceptions have been defined in IOException : these exceptions have to do with I/O, parameter parsing, argument validity, etc and consistently print useful debuging information when thrown.
 *
 *
 * @section misc_sec Miscellaneous
 * The IOUtils class is a static class that contains a few helper functions, in order to deal with things as diverse as units conversions, checking for file presence, equality within a given epsilon, string parsing.
 *
 */

//Plugins overview given in IOHandler.cc
//Filters overview given in ProcessingBlock.cc
//Plugin development given in IOInterface.h
//2D interpolation development given in Meteo2DInterpolator.h

 /**
 * @page dev_processing Processing elements developer's guide
 *
 * In order to add a new filter/processing element to the already existing set of components, the developer only needs to
 * add a class derived from either (in meteoio/meteoFilters):
 *    - ProcessingBlock, for filters that only look at one point at a time;
 *    - or WindowedFilter, for filters that work on a whole time window at a time. It provides members to read the
 * time window definition as well as to compute the proper indices matching the user-defined time window;
 *
 * Of course, each developer is free to bypass the helper methods provided in these two classes and reimplement his/her own if the
 * provided ones don't fit with the needs of a specific processing element (but it must at least be derived from ProcessingBlock).
 * Templates header and code files are available to get you started, look into the
 * "meteoFilters" subdirectory of the source directory (files "template.cc" and "template.h"). It is important to understand that the
 * processing elements operate on a "per parameter" basis. This means that an element might be executed for the parameter TA
 * and another one for the parameter PSUM, so the algorithm only has to deal with a generic processing method based on double values.
 *
 * To implement a new processing element, the following steps are necessary:
 *
 * -# Create a derived class of ProcessingBlock or WindowedFilter in the meteoFilters subdirectory of the source code.
 *      You can copy and rename the template files "template.cc" and "template.h" that are in the  "meteoFilters" subdirectory. 
 *      Please do not forget to rename all occurences of "TEMPLATE" in these files! Keep the <b>process</b> method as it is for now.
 * -# Add the created implementation file to meteoFilters/CMakeLists.txt in a similar way as for the other filters
 * -# Add the filter in the processing loop, in meteoFilters/ProcessingBlock.cc in the BlockFactory::getBlock()
 * method by adding three lines similar to:
 *    @code
 *     else if (blockname == "MIN_MAX"){
 *     		return new FilterMinMax(vecArgs, blockname);
 * 	}
 *    @endcode
 *    The key (here the string "MIN_MAX") is the key that the user will put in his io.ini to select the processing block.
 * -# Include the filter's header file in meteoFilters/ProcessingBlock.cc
 * -# Try to compile and run your filter on a test data set (for example with the "meteo_reading" example)
 * -# Then really implement your filter. Its class contains two public methods: a constructor and a "process" method and at least one private method,
 *    "parse_args" to read the arguments from a provided vector of strings.
 *    -# The <b>constructor</b> takes a vector of strings containing the element's arguments and a constant string (that contains
 *    the block name, for example for printing in an error message from which filter/block it comes). The constructor
 *    would therefore have a declaration similar to:
 *    @code
 *    FilterMax::FilterMax(const std::vector< std::pair<std::string, std::string> >& vecArgs, const std::string& name)
 *    @endcode
 *    -# The <b>process</b> method applies the element to the provided vector of values, for a meteo parameter pointed to by index.
 *    This index is the MeteoData parameter that this filter shall be run upon (see MeteoData for the enumeration of
 *    parameters). The constructor must set up processing.stage to mark if the filter should be applied only during the
 *    first pass (ie before the resampling), or both at the first and second pass (ie before and after resampling).
 *    Its declaration is such as:
 *    @code
 *    process(const unsigned int& index, const std::vector<MeteoData>& ivec, std::vector<MeteoData>& ovec)
 *    @endcode
 *    -# The private <b>parse_args</b> method reads the arguments from a vector of pairs of strings, with the following declaration:
 *    @code
 *    parse_args(const std::vector< std::pair<std::string, std::string> >& vecArgs)
 *    @endcode
 *
 * Although you are encouraged to use the provided templates (files "template.cc" and "template.h" in the meteoFilters subdirectory),
 * the class FilterMax can be used as an example of implementation of a basic filter that will check whether a
 * value is greater than an argument
 * supplied to the filter and if so changes the value either to IOUtils::nodata (normal operation) or to the
 * maximum value supplied in the argument (soft mode of operation). An example section in the io.ini file supplied
 * to the Config could look like this:
 * @code
 * [Filters]
 * TA::filter1    = max
 * TA::arg1::soft = true
 * TA::arg1::max  = 280
 * @endcode
 * Which has the following interpretation: Apply filter max (max-value-filter) to the parameter TA (air temperature)
 * in case that a value is greater than 280 degrees Kelvin change that value to 280.
 * A more customized operation could be:
 * @code
 * [Filters]
 * TA::filter1         = max
 * TA::arg1::soft      = true
 * TA::arg1::max       = 280
 * TA::arg1::max_reset = 260
 * @endcode
 * Which will replace any value greater than 280 Kelvin by 260 Kelvin.
 *
 * Another good example is the FilterUnheatedPSUM that uses a combination of several meteorological parameters to filter the
 * precipitation.
 */

/**
 * @page examples Examples
 * This page shows you how to integrate MeteoIO within your own aplication. Several of the basics calls are shown 
 * here below in the code examples. Finally, some tips are given in order to polish such integration.
 * 
 * Please keep in mind that the given examples are very simple, in order to keep them compact. For a real application, 
 * you will need to add some error checking code (as shown in the "tips" section).
 * 
 * @section reading_meteo_data Reading meteorological time series
 * Here is a simple exmaple showing how to get some meteorological data into the MeteoData vectors.
 * \code
 * #include <iostream>
 * #include <meteoio/MeteoIO.h>
 * 
 * using namespace mio; //The MeteoIO namespace is called mio
 * 
 * //This is the most basic example. It does not check any exceptions, it only tries to be as c-like as possible
 * //provide date as ISO formatted, for example 2008-12-01T15:35:00 and
 * //it will retrieve the data for this date according to the io.ini configuration file
 * int main(int argc, char** argv) {
 * 	Date d1;
 * 	std::vector<MeteoData> vecMeteo;
 * 
 * 	Config cfg("io.ini");
 * 	IOManager io(cfg);
 * 
 * 	//we assume that the time given on the command line is in TZ=+1
 * 	IOUtils::convertString(d1,argv[1], 1.);
 * 	//io.setProcessingLevel(IOManager::raw); //set the processing level: raw, filtered or resampled
 * 	io.getMeteoData(d1, vecMeteo);
 * 
 * 	std::cout << vecMeteo.size() << " stations with an average sampling rate of " << io.getAvgSamplingRate() << " or 1 point every " << 1./(io.getAvgSamplingRate()*60.+1e-12) << " minutes\n";
 * 	//writing some data out in order to prove that it really worked!
 * 	for (unsigned int ii=0; ii < vecMeteo.size(); ii++) {
 * 		std::cout << "---------- Station: " << (ii+1) << " / " << vecMeteo.size() << std::endl;
 * 		std::cout << vecMeteo[ii].toString() << std::endl;
 * 	}
 * 
 * 	return 0;
 * }
 * \endcode
 *
 * @section reading_dem_example Reading Digital Elevation Models
 * Now, we can also read a Digital Elevation Model, print some information about it, write it back to disk with (potentially) another plugin as well as the slope and azimuth:
 *
 * \code
 *  #include <meteoio/MeteoIO.h>
 * 
 * using namespace mio; //The MeteoIO namespace is called mio
 * 
 * //This is a basic example of using as dem: the dem is read, the grid coordinates of a point given by its (lat,long) are retrieved
 * //and a sub-dem is extracted starting at these coordinates and extending dist_x and dist_y and written out.
 * int main(void) {
 * 	DEMObject dem;
 * 	Config cfg("io.ini");
 * 	IOManager io(cfg);
 * 
 * 	//reading dem
 * 	dem.setUpdatePpt(DEMObject::SLOPE);
 * 	io.readDEM(dem);
 * 
 * 	//writing some statistics about this dem
 * 	//dem.grid2D.getMin() scans the DEM grid to get the min, while dem.min_altitude is cached and therefore very cheap
 * 	//The raw content of the 2D grids can also be accessed, for example dem.grid2D.getMin(IOUtils::RAW_NODATA). In this case, there would be no interpretation of some values as nodata.
 * 	std::cout << "DEM information: \n";
 * 	std::cout << "\tmin=" << dem.grid2D.getMin() << " max=" << dem.grid2D.getMax() << " mean=" << dem.grid2D.getMean() << "\n";
 * 	std::cout << "\tmin slope=" << dem.min_slope << " max slope=" << dem.max_slope << std::endl;
 * 
 * 	io.write2DGrid(dem, MeteoGrids::DEM, Date(0.));
 * 	
 * 	Grid2DObject slope(dem.cellsize, dem.llcorner, dem.slope);
 * 	io.write2DGrid(slope, MeteoGrids::SLOPE, Date(0.));
 * 	Grid2DObject azi(dem.cellsize, dem.llcorner, dem.azi);
 * 	io.write2DGrid(azi,"azi.png");
 * 
 * 	return 0;
 * }
 * \endcode
 *
 * @section examples_tips Programming Tips
 * First, more examples are provided in <b>doc/examples</b> (see the readme.txt file), alongside with an example io.ini as well as some data sets
 * (7 weather stations as well as one DEM).
 * 
 * Then, for a real world application, the following would also be needed:
 * 	+ wrapping up the MeteoIO calls in a <i>try/catch</i> block (at least for calls such as <i>getMeteoData</i>). This is particularly required for Windows and Mac platforms since uncaught exceptions on these plateforms won't print any error message on the screen.
 * 	+ checking the data returned by <i>getMeteoData</i> against your application's minimum requirements. For example, you might want to check that there is at least one air temperature and one wind velocity at each time step. If your application's requirements are not fulfilled, then print an error message and exit (or throw an exception with a proper error message).
 * 	+ if your application is written in another language (for example C or Fortran), then you need a wrapper that will wrap the call to MeteoIO and copy the returned data into your own data structures. 
 * 	+ finally, it might be a good idea to print the MeteoIO version information somewhere in your application's output. This could help with support and debugging. Such version information is returned by <i>getLibVersion()</i>.
 *
 */

} //end namespace mio
#endif
