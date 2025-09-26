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

#ifndef SNOWPACKIO_H
#define SNOWPACKIO_H

#include <meteoio/MeteoIO.h>

#include <snowpack/DataClasses.h>
#include <snowpack/SnowpackConfig.h>
#include <snowpack/plugins/SnowpackIOInterface.h>

#include <string>
#include <vector>

/**
 * @page snowpackio Snowpack data formats
 *
 * @section snowpack_inputs_outputs Snowpack inputs versus outputs
 * %Snowpack needs several kind of information to be provided for a simulation and then writes out
 * several kind of information. Some formats can be used for both the inputs and the outputs while some
 * others are restricted to either one or the other (simply because %Snowpack does not read or write
 * out the parameters contained in the said format).
 *
 * @subsection Snowpack_inputs Snowpack required inputs
 *  Several kind of information need to be given to %Snowpack for a simulation:
 * -# the description of the place where the snow pack has to be simulated: latitutde, longitude, elevation, slope, ...
 * -# the time series of the various meteorological parameters
 * -# the intial state of the various soil and snow layers
 *
 * Very often, 1) and 2) are provided together. But this depends ultimately on the file format that is used ot provide such data (SMET, INP, etc). These two points are
 * handled by <a href="https://models.slf.ch/p/meteoio">MeteoIO</a>, so please check its documentation (for the last official release, it is available
 * <A HREF="https://models.slf.ch/docserver/meteoio/html/index.html">online</A>), in the <i>"Available plugins and usage"</i> section for the relevant formats.
 * It is recommended to prepare the data in the <A HREF="https://models.slf.ch/docserver/meteoio/html/smetio.html">SMET</A> file format for its ease of use.
 *
 * Please also check the \ref requirements "Data requirements" page.
 *
 * @subsection Snowpack_outputs Snowpack outputs
 * %Snowpack creates various output files:
 * - the current state of its soil and snow layers in <i>".sno"</i> files;
 * - the current state of its hazard relevant data in <i>".haz"</i> files;
 * - a time serie of snow profiles;
 * - a time serie of the meteorological data and fluxes as used in the model.
 * 
 * Depending on the chosen output format, 1) and 2) might be provided as one file or two files. Moreover, since %Snowpack pre-processes all the 
 * meteorological input data with <A HREF="https://models.slf.ch/p/meteoio">MeteoIO</A>, the forcing data that is seen in the core of the model might be different than 
 * the provided input data. In order to better fine tune the parameters of this pre-processing, it is possible to request a copy of the
 * pre-processed meteorological data by setting the key WRITE_PROCESSED_METEO to TRUE in the [Output] section.
 *
 * @section available_single_profile_plugins Single snow profiles
 * The %Snowpack specific data are supported directly in %Snowpack and the formats listed in the table below
 * are available, both for input and output of snow profiles with the <b>"SNOW"</b> keyword.
 * Please read the documentation for each plugin in order to know the plugin-specific keywords!
 * <center><table border="1">
 * <tr><th>Key</th><th>Description</th><th>Extra requirements</th></tr>
 * <tr><td>\subpage smet "SMET"</td><td>SMET based profile (including the hazard data), recommended</td><td></td></tr>
 * <tr><td>\subpage caaml "CAAML"</td><td>CAAML profile</td><td><A HREF="http://xmlsoft.org/">libxml</A></td></tr>
 * <tr><td>\subpage snoold_format "SNOOLD"</td><td>legacy %Snowpack profile (including the hazard data)</td><td></td></tr>
 * </table></center>
 *
 * @section available_profile_ts_plugins Snow profiles time series
 * The %Snowpack specific data are supported directly in %Snowpack and the formats listed in the table below
 * are available for output of snow profiles time series with the <b>"PROF_FORMAT"</b> keyword. Note that the 
 * keys AGGREGATE_PRO and AGGREGATE_PRF will allow to aggregate model layers to a smaller number.
 * Please read the documentation for each plugin in order to know the plugin-specific keywords!
 * <center><table border="1">
 * <tr><th>Key</th><th>Description</th><th>Extra requirements</th></tr>
 * <tr><td>\subpage pro_format "PRO"</td><td>legacy %Snowpack profile time series for visualization with <A HREF="snopviz.org">SnopViz</A> and sngui</td><td></td></tr>
 * <tr><td>\subpage prf_format "PRF"</td><td>tabular profile time series</td><td></td></tr>
 * <tr><td>\subpage profile_imis "IMIS"</td><td>write profile time series to the IMIS database</td><td><A HREF="http://docs.oracle.com/cd/B12037_01/appdev.101/b10778/introduction.htm">Oracle's OCCI library</A></td></tr>
 * </table></center>
 * 
 * When the snow grain shapes are provided as <b>Swiss Code</b>, it means the following: the code is made of three decimal numbers, noted as <i>F1F2F3</i>. Here <i>F1</i> 
 * represents the primary grain shape and <i>F2</i> the secondary grain shape. The grain shapes can be any of the following:
 * <center><table border="1">
 * <tr><th>Code</th><th>Grain shape</th><th>Code</th><th>Grain shape</th></tr>
 * <tr><td>1</td><td>Precipitation particules (PP)</td><td>6</td><td>Surface hoar (SH)</td></tr>
 * <tr><td>2</td><td>Decomposing fragmented PP (DF)</td><td>7</td><td>Melt forms (MF)</td></tr>
 * <tr><td>3</td><td>Rounded grains (RG)</td><td>8</td><td>Ice formations (IF)</td></tr>
 * <tr><td>4</td><td>Faceted crystals (FC)</td><td>9</td><td>Rounding faceted particules (FCxr)</td></tr>
 * <tr><td>5</td><td>Depth hoar (DH)</td><td></td><td></td></tr>
 * </table></center>
 * 
 *
 * @section available_met_ts Fluxes time series
 * %Snowpack computes various meteorological parameters as well as fluxes and can write them out as time series.
 * <center><table border="1">
 * <tr><th>Key</th><th>Description</th><th>Extra requirements</th></tr>
 * <tr><td>\subpage met_format "MET"</td><td>legacy %Snowpack time series for visualization with sngui</td><td></td></tr>
 * <tr><td>\subpage smet "SMET"</td><td>smet formatted time series for visualization with <A HREF="snopviz.org">SnopViz</A></td><td></td></tr>
 * </table></center>
 *
 */
class SnowpackIO : public SnowpackIOInterface {

	public:
		SnowpackIO(const SnowpackConfig& cfg);
		SnowpackIO(const SnowpackIO& source);
		~SnowpackIO();

		virtual bool snowCoverExists(const std::string& i_snowfile, const std::string& stationID) const;

		virtual void readSnowCover(const std::string& i_snowfile, const std::string& stationID,
		                           SN_SNOWSOIL_DATA& SSdata, ZwischenData& Zdata, const bool& read_salinity);

		virtual void writeSnowCover(const mio::Date& date, const SnowStation& Xdata,
                                    const ZwischenData& Zdata, const bool& forbackup=false);

		virtual void writeTimeSeries(const SnowStation& Xdata, const SurfaceFluxes& Sdata, const CurrentMeteo& Mdata,
		                             const ProcessDat& Hdata, const double wind_trans24);

		virtual void writeProfile(const mio::Date& date, const SnowStation& Xdata);

		virtual bool writeHazardData(const std::string& stationID, const std::vector<ProcessDat>& Hdata,
                                     const std::vector<ProcessInd>& Hdata_ind, const size_t& num);

		std::vector<std::string> getExtensions();
		
		SnowpackIO& operator=(const SnowpackIO& source);

	private:
		std::vector< std::string > vecExtension; ///< file extensions for the user selected plugins
		SnowpackIOInterface *imisdbio;
		SnowpackIOInterface *caamlio;
		SnowpackIOInterface *smetio;
		SnowpackIOInterface *asciiio;
		bool input_snow_as_smet, output_snow_as_smet;
		bool input_snow_as_caaml, output_snow_as_caaml;
		bool input_snow_as_ascii, output_snow_as_ascii;
		bool output_prf_as_ascii, output_prf_as_caaml, output_prf_as_imis;
		bool output_ts_as_ascii, output_ts_as_smet, output_haz_as_imis;
};

#endif //End of SnowpackIO.h
