/***********************************************************************************/
/*  Copyright 2010 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef SMETIO_H
#define SMETIO_H

#include <meteoio/IOInterface.h>
#include <meteoio/plugins/libsmet.h>

#include <string>
#include <vector>

#ifdef _MSC_VER
	#pragma warning(disable:4512) //we don't need any = operator!
#endif

namespace mio {
/**
 * @class SMETIO
 * @brief Reads meteo data in the SMET ASCII or binary format.
 *
 * @ingroup plugins
 * @author Mathias Bavay
 * @date   2010-06-14
 */
class SMETIO : public IOInterface {
	public:
		SMETIO(const std::string& configfile);
		SMETIO(const SMETIO&);
		SMETIO(const Config& cfgreader);

		virtual void readStationData(const Date& date, std::vector<StationData>& vecStation);
		virtual void readMeteoData(const Date& dateStart, const Date& dateEnd,
		                           std::vector< std::vector<MeteoData> >& vecMeteo);

		virtual void writeMeteoData(const std::vector< std::vector<MeteoData> >& vecMeteo,
		                            const std::string& name="");

		virtual void readPOI(std::vector<Coords>& pts);

	private:
		/** This structure contains the metadata associated with a SMET variable in order to plot it */
		typedef struct PLOT_ATTR {
			PLOT_ATTR() : units(), description(), color(), min(IOUtils::nodata), max(IOUtils::nodata), param(mio::IOUtils::npos) {} //please do NOT use this constructor!
			PLOT_ATTR(const std::string& i_units, const std::string& i_description, const std::string& i_color) : units(i_units), description(i_description), color(i_color), min(IOUtils::nodata), max(IOUtils::nodata), param(mio::IOUtils::npos) {}
			PLOT_ATTR(const std::string& i_units, const std::string& i_description, const std::string& i_color, const double& i_min, const double& i_max, const size_t& i_param) : units(i_units), description(i_description), color(i_color), min(i_min), max(i_max), param(i_param) {}

			std::string units; ///< unit string representation
			std::string description; ///< plot label
			std::string color; ///< plot color (hex. rgb)
			double min; ///< axis minimum
			double max; ///< axis maximum
			size_t param; ///< parameter index
		} plot_attr;
		
		static std::map<size_t, plot_attr> initPlotParams();
		static double getSnowpackSlope(const std::string& id);
		void read_meta_data(const smet::SMETReader& myreader, StationData& meta);
		void identify_fields(const std::vector<std::string>& fields, std::vector<size_t>& indexes,
		                     bool& julian_present, MeteoData& md);
		void populateMeteo(const smet::SMETReader& myreader, const std::vector<std::string>& timestamps,
		               const std::vector<double>& mydata, std::vector<MeteoData>& vecMeteo);

		void parseInputOutputSection();
		bool checkConsistency(const std::vector<MeteoData>& vecMeteo, StationData& sd);
		size_t getNrOfParameters(const std::string& stationname, const std::vector<MeteoData>& vecMeteo);
		void checkForUsedParameters(const std::vector<MeteoData>& vecMeteo, const size_t& nr_parameters, double& smet_timezone,
		                            std::vector<bool>& vecParamInUse, std::vector<std::string>& vecColumnName);
		void getPlotProperties(const size_t& param, std::ostringstream &plot_units, std::ostringstream &plot_description, std::ostringstream &plot_color, std::ostringstream &plot_min, std::ostringstream &plot_max) const;
		static void getFormatting(const size_t& param, int& prec, int& width);
		double olwr_to_tss(const double& olwr);
		void generateHeaderInfo(const StationData& sd, const bool& i_outputIsAscii, const bool& isConsistent,
		                        const double& smet_timezone, const size_t& nr_of_parameters,
		                        const std::vector<bool>& vecParamInUse,
		                        const std::vector<std::string>& vecColumnName,
		                        smet::SMETWriter& mywriter);

		static const char* dflt_extension;
		static const double snVirtualSlopeAngle;
		const Config cfg;
		std::map<size_t, plot_attr> plot_ppt; ///< properties for plotting the SMET parameters
		std::string coordin, coordinparam, coordout, coordoutparam; //default projection parameters
		std::vector<smet::SMETReader> vec_smet_reader;
		std::vector<std::string> vecFiles;  //read from the Config [Input] section
		std::string outpath;                //read from the Config [Output] section
		double out_dflt_TZ;     //default time zone
		double plugin_nodata;
		bool outputIsAscii, outputPlotHeaders, randomColors, allowAppend, allowOverwrite, snowpack_slopes;//read from the Config [Output] section
};

} //namespace
#endif
