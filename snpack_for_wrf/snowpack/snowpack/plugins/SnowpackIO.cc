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

#include <snowpack/plugins/SnowpackIO.h>

#include <snowpack/plugins/SmetIO.h>
#include <snowpack/plugins/AsciiIO.h>

/* #undef PLUGIN_IMISIO */
/* #undef PLUGIN_CAAMLIO */

#ifdef PLUGIN_CAAMLIO
	#include <snowpack/plugins/CaaMLIO.h>
#endif
#ifdef PLUGIN_IMISIO
	#include <snowpack/plugins/ImisDBIO.h>
#endif

using namespace std;
using namespace mio;

SnowpackIO::SnowpackIO(const SnowpackConfig& cfg):
	vecExtension(), imisdbio(NULL), caamlio(NULL), smetio(NULL), asciiio(NULL),
	input_snow_as_smet(false), output_snow_as_smet(false),
	input_snow_as_caaml(false), output_snow_as_caaml(false),
	input_snow_as_ascii(false), output_snow_as_ascii(false),
	output_prf_as_ascii(false), output_prf_as_caaml(false), output_prf_as_imis(false),
	output_ts_as_ascii(false), output_ts_as_smet(false), output_haz_as_imis(false)

{
	//Format of initial snow profile:
	const std::string in_snow = cfg.get("SNOW", "Input", "SMET");
	if (in_snow == "SNOOLD") {
		input_snow_as_ascii = true;
	} else if (in_snow == "CAAML") {
		input_snow_as_caaml = true;
	} else if (in_snow == "SMET") {
		input_snow_as_smet = true;
	} else
		throw InvalidArgumentException("Invalid input snow profile format '"+in_snow+"'. Please choose from SMET, CAAML, SNOOLD", AT);

	//Format of transitional and final snow profile(s):
	const bool snow_out = cfg.get("SNOW_WRITE", "Output");
	const bool a3d_pts = cfg.get("ALPINE3D_PTS", "SnowpackAdvanced");
	if (snow_out || a3d_pts) {
		const string out_snow = cfg.get("SNOW", "Output");
		if (out_snow == "SNOOLD") {
			output_snow_as_ascii = true;
			vecExtension.push_back("snoold");	//Snow-cover profile file (I/O)
		} else if (out_snow == "CAAML") {
			output_snow_as_caaml = true;
			vecExtension.push_back("haz");	//Snow-cover profile file (I/O)
			vecExtension.push_back("caaml");	//Snow-cover profile file (I/O & SnopViz)
			vecExtension.push_back("acaaml");	//Aggregated snow-cover profile file (I/O & SnopViz)
		} else if (out_snow == "SMET") {
			output_snow_as_smet = true;
			vecExtension.push_back("haz");	//Snow-cover profile file (I/O)
			vecExtension.push_back("sno");	//Snow-cover profile file (I/O)
		} else
			throw InvalidArgumentException("Invalid output snow profile format '"+out_snow+"'. Please choose from SMET, CAAML, SNOOLD", AT);
	}

	const std::vector<string> vecProfileFmt = cfg.get("PROF_FORMAT", "Output");
	if (vecProfileFmt.size() > 3) {
		throw InvalidArgumentException("The key PROF_FORMAT in [Output] can take three values at most", AT);
	} else {
		for (size_t ii=0; ii<vecProfileFmt.size(); ii++) {
			if (vecProfileFmt[ii] == "PRO") {
				output_prf_as_ascii = true;
				vecExtension.push_back("pro");	//Time series of full modeled snow-profile data for SnopViz [and SN_GUI]
				vecExtension.push_back("apro");	//Time series of aggregated modeled snow-profile data for SnopViz [and SN_GUI]
			} else if (vecProfileFmt[ii] == "PRF") {
				output_prf_as_ascii  = true;
				vecExtension.push_back("prf");	//Time series of full modeled snow-profile data in tabular form
				vecExtension.push_back("aprf");	//Time series of aggregated modeled snow-profile data in tabular form
			} else if (vecProfileFmt[ii] == "IMIS") {
				output_prf_as_imis  = true;
			} else {
				throw InvalidArgumentException("The key PROF_FORMAT in [Output] takes only PRO, PRF or IMIS as value", AT);
			}
		}
	}

	//Format of meteo time series:
	const bool ts_out = cfg.get("TS_WRITE", "Output");
	if (ts_out==true) {
		const std::string ts_format = cfg.get("TS_FORMAT", "Output", "SMET");
		if (ts_format=="SMET") {
			output_ts_as_smet = true;
			vecExtension.push_back("smet");	//Classical time series (meteo, snow temperatures, etc.)
		} else if (ts_format=="MET") {
			output_ts_as_ascii = true;
			vecExtension.push_back("met");	//Classical time series (meteo, snow temperatures, etc.)
		} else
			throw InvalidArgumentException("The key TS_FORMAT in [Output] takes only SMET or MET as value", AT);
	}

	vecExtension.push_back("ini");	//Record of run configuration

	//set the "plugins" pointers
	RunInfo run_info;
	if (input_snow_as_smet || output_snow_as_smet || output_ts_as_smet) smetio = new SmetIO(cfg, run_info);
	if (input_snow_as_ascii || output_snow_as_ascii || output_prf_as_ascii || output_ts_as_ascii) asciiio = new AsciiIO(cfg, run_info);
#ifdef PLUGIN_CAAMLIO
	if (input_snow_as_caaml || output_snow_as_caaml) caamlio = new CaaMLIO(cfg, run_info);
#endif
#ifdef PLUGIN_IMISIO
	output_haz_as_imis = output_prf_as_imis;
	if (output_prf_as_imis || output_haz_as_imis) imisdbio = new ImisDBIO(cfg, run_info);
#endif
}

SnowpackIO::SnowpackIO(const SnowpackIO& source) :
	vecExtension(source.vecExtension), imisdbio(source.imisdbio), caamlio(source.caamlio), smetio(source.smetio), asciiio(source.asciiio),
	input_snow_as_smet(source.input_snow_as_smet), output_snow_as_smet(source.input_snow_as_smet),
	input_snow_as_caaml(source.input_snow_as_caaml), output_snow_as_caaml(source.output_snow_as_caaml),
	input_snow_as_ascii(source.input_snow_as_ascii), output_snow_as_ascii(source.output_snow_as_ascii),
	output_prf_as_ascii(source.output_prf_as_ascii), output_prf_as_caaml(source.output_prf_as_caaml), output_prf_as_imis(source.output_prf_as_imis),
	output_ts_as_ascii(source.output_ts_as_ascii), output_ts_as_smet(source.output_ts_as_smet), output_haz_as_imis(source.output_haz_as_imis)
{}

SnowpackIO::~SnowpackIO()
{
	if (smetio != NULL) delete smetio;
	if (asciiio != NULL) delete asciiio;
	if (caamlio != NULL) delete caamlio;
	if (imisdbio != NULL) delete imisdbio;
}

std::vector<std::string> SnowpackIO::getExtensions()
{
	return vecExtension;
}

bool SnowpackIO::snowCoverExists(const std::string& i_snowfile, const std::string& stationID) const
{
	if (input_snow_as_ascii) {
		return asciiio->snowCoverExists(i_snowfile, stationID);
#ifdef PLUGIN_CAAMLIO
	} else if (input_snow_as_caaml){
		return caamlio->snowCoverExists(i_snowfile, stationID);
#endif
	} else {
		return smetio->snowCoverExists(i_snowfile, stationID);
	}
}

void SnowpackIO::readSnowCover(const std::string& i_snowfile, const std::string& stationID,
                               SN_SNOWSOIL_DATA& SSdata, ZwischenData& Zdata, const bool& read_salinity)
{
	if (input_snow_as_ascii) {
		asciiio->readSnowCover(i_snowfile, stationID, SSdata, Zdata, read_salinity);
#ifdef PLUGIN_CAAMLIO
	} else if (input_snow_as_caaml) {
		caamlio->readSnowCover(i_snowfile, stationID, SSdata, Zdata, read_salinity);
#endif
	} else {
		smetio->readSnowCover(i_snowfile, stationID, SSdata, Zdata, read_salinity);
	}
}

void SnowpackIO::writeSnowCover(const mio::Date& date, const SnowStation& Xdata,
                                const ZwischenData& Zdata, const bool& forbackup)
{
	if (output_snow_as_ascii) {
		asciiio->writeSnowCover(date, Xdata, Zdata, forbackup);
#ifdef PLUGIN_CAAMLIO
	} else if (output_snow_as_caaml) {
		caamlio->writeSnowCover(date, Xdata, Zdata, forbackup);
#endif
	} else {
		smetio->writeSnowCover(date, Xdata, Zdata, forbackup);
	}
}

void SnowpackIO::writeTimeSeries(const SnowStation& Xdata, const SurfaceFluxes& Sdata, const CurrentMeteo& Mdata,
                                 const ProcessDat& Hdata, const double wind_trans24)
{
	if (output_ts_as_ascii)
		asciiio->writeTimeSeries(Xdata, Sdata, Mdata, Hdata, wind_trans24);
	else if (output_ts_as_smet)
		smetio->writeTimeSeries(Xdata, Sdata, Mdata, Hdata, wind_trans24);
}

void SnowpackIO::writeProfile(const mio::Date& date, const SnowStation& Xdata)
{
	if (output_prf_as_ascii)
		asciiio->writeProfile(date, Xdata);

#ifdef PLUGIN_CAAMLIO
	if (output_prf_as_caaml)
		caamlio->writeProfile(date, Xdata);
#endif

#ifdef PLUGIN_IMISIO
	if (output_prf_as_imis)
		imisdbio->writeProfile(date, Xdata);
#endif
}

#ifdef PLUGIN_IMISIO
bool SnowpackIO::writeHazardData(const std::string& stationID, const std::vector<ProcessDat>& Hdata,
                                 const std::vector<ProcessInd>& Hdata_ind, const size_t& num)
{
	if(output_haz_as_imis)
		return imisdbio->writeHazardData(stationID, Hdata, Hdata_ind, num);
	return false;
}
#else
bool SnowpackIO::writeHazardData(const std::string& /*stationID*/, const std::vector<ProcessDat>& /*Hdata*/,
                                 const std::vector<ProcessInd>& /*Hdata_ind*/, const size_t& /*num*/)
{
	return false;
}
#endif

SnowpackIO& SnowpackIO::operator=(const SnowpackIO& source)
{
	if(this != &source) {
		imisdbio = source.imisdbio;
		caamlio = source.caamlio;
		asciiio = source.asciiio;
		smetio = source.smetio;
		output_prf_as_ascii = source.output_prf_as_ascii;
		output_prf_as_caaml = source.output_prf_as_caaml;
		output_prf_as_imis = source.output_prf_as_imis;
		output_snow_as_caaml = source.output_snow_as_caaml;
		output_snow_as_smet = source.output_snow_as_smet;
		input_snow_as_caaml = source.input_snow_as_caaml;
		input_snow_as_smet = source.input_snow_as_smet;
		output_ts_as_ascii = source.output_ts_as_ascii;
		output_haz_as_imis = source.output_haz_as_imis;
	}
	return *this;
}
