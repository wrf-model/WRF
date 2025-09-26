/***********************************************************************************/
/*  Copyright 2014 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
/***********************************************************************************/
/* This file is part of Snowpack.
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
#ifndef CAAMLIO_H
#define CAAMLIO_H

#include <meteoio/MeteoIO.h>
#include <snowpack/Constants.h>
#include <snowpack/Hazard.h>
#include <snowpack/plugins/SnowpackIOInterface.h>
#include <snowpack/plugins/SmetIO.h>

#include <string>

#include <libxml/parser.h>
#include <libxml/xpath.h>
#include <libxml/xmlwriter.h>

/**
 * @class CaaMLIO
 * @brief Reading snow profile data in CAAML format.
 * Reads in CAAML snow profile data, the CAA-IACS international standard
 *
 * @author Charles Fierz (Mathias Bavay)
 * @date   2014
 */
class CaaMLIO : public SnowpackIOInterface {

	public:
		CaaMLIO(const SnowpackConfig& i_cfg, const RunInfo& run_info);
		CaaMLIO(const CaaMLIO&);
		~CaaMLIO() throw();

		CaaMLIO& operator=(const CaaMLIO&); ///<Assignement operator, required because of pointer member

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

	private:
		void cleanup() throw();
		void init(const SnowpackConfig& cfg);
		void openIn_CAAML(const std::string& in_snowfile);
		void closeIn_CAAML() throw();
		void setBasicHeader(const SnowStation& Xdata, const std::string& fields, smet::SMETWriter& smet_writer) const;
		void setSnoSmetHeader(const SnowStation& Xdata, const SN_SNOWSOIL_DATA& SSdata, const mio::Date& date, smet::SMETWriter& smet_writer) const;
		void setFormatting(const size_t& nr_solutes, std::vector<int>& vec_width, std::vector<int>&  vec_precision) const;

		std::string getFilenamePrefix(const std::string& fnam, const std::string& path, const bool addexp=true) const;
		bool read_snocaaml(const std::string& snofilename, const std::string& stationID, SN_SNOWSOIL_DATA& SSdata);
		void writeSnowFile(const std::string& snofilename, const mio::Date& date, const SnowStation& Xdata,
		                   const bool aggregate);

		const RunInfo info;
		std::string i_snowpath, sw_mode, o_snowpath, experiment;
		bool useSoilLayers, perp_to_slope, aggregate_caaml;
		/*static const*/ double in_tz; //plugin specific time zones
		std::string snow_prefix, snow_ext; //for the file naming scheme
		double caaml_nodata; //plugin specific no data value

		xmlDocPtr in_doc;
		xmlXPathContextPtr in_xpathCtx;
		xmlCharEncoding in_encoding;
		static const xmlChar *xml_ns_caaml, *xml_ns_abrev_caaml;
		static const xmlChar *xml_ns_gml, *xml_ns_abrev_gml;
		static const xmlChar *xml_ns_xsi, *xml_ns_abrev_xsi;
		static const xmlChar *xml_ns_slf, *xml_ns_abrev_slf;
		static const xmlChar *xml_ns_snp, *xml_ns_abrev_snp;
		static const std::string TimeData_xpath, StationMetaData_xpath, SnowData_xpath;

		xmlNodeSetPtr xmlGetData(const std::string& path);
		mio::Date xmlGetDate();
		mio::StationData xmlGetStationData(const std::string& stationID);
		double xmlSetVal(const std::string& xpath, const std::string& property, const double& dflt);
		int xmlSetVal(const std::string& xpath, const std::string& property, const int& dflt);
		void setCustomSnowSoil(SN_SNOWSOIL_DATA& Xdata,const std::string& in_snowFilename);
		bool getLayersDir();
		LayerData xmlGetLayer(xmlNodePtr cur);
		void getProfiles(const std::string path, std::vector<double> &depths, std::vector<double> &val);
		void setProfileVal(std::vector<LayerData> &Layers, std::vector<std::vector<double> > depths, std::vector<std::vector<double> > val);
		void setCustomLayerData(LayerData &Layer);
		void setDepositionDates(std::vector<LayerData> &Layers, const mio::Date);

		void xmlWriteElement(const xmlTextWriterPtr writer, const char* name, const char* content, const char* att_name, const char* att_val);
		// void writeDate(const xmlTextWriterPtr writer, const mio::Date date);
		void writeDate(const xmlTextWriterPtr writer, const char* att_name, const char* att_val);
		void writeCustomSnowSoil(const xmlTextWriterPtr writer, const SnowStation& Xdata);
		void writeLayers(const xmlTextWriterPtr writer, const SnowStation& Xdata);
		void writeCustomLayerData(const xmlTextWriterPtr writer, const ElementData& Edata, const NodeData& Ndata);
		void writeProfiles(const xmlTextWriterPtr writer, const SnowStation& Xdata);
		void writeStationData(const xmlTextWriterPtr writer, const SnowStation& Xdata);

		double lwc_codeToVal(const char* code);
		std::string lwc_valToCode(const double val);
		double hardness_codeToVal(char* code);
		std::string hardness_valToCode(const double code);
		void grainShape_codeToVal(const std::string& code, double &sp, double &dd, unsigned short int &mk);
		std::string grainShape_valToAbbrev(const unsigned int var);
		std::string grainShape_valToAbbrev_old(const double* var);

		char layerDepthTopStr[10], layerThicknessStr[10], layerValStr[10], valueStr[10], dateStr[30];

};

#endif //End of CAAMLIO.h
