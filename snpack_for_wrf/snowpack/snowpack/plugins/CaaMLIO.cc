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
#include <snowpack/plugins/CaaMLIO.h>
#include <snowpack/Utils.h>
//#include <meteoio/meteolaws/Atmosphere.h>

#include <sstream>
#include <fstream>
#include <iostream>

#include <libxml/parserInternals.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>
#include <libxml/encoding.h>
#include <libxml/xmlwriter.h>
#if !defined(LIBXML_XPATH_ENABLED)
	#error Please enable XPATH in your version of libxml!
#endif
#if !defined(LIBXML_SAX1_ENABLED)
	#error Please enable SAX1 in your version of libxml!
#endif
#if !defined(LIBXML_TREE_ENABLED)
	#error Please enable TREE in your version of libxml!
#endif

using namespace std;
using namespace mio;

/**
 * @page caaml CAAML
 * @section caaml_format Format
 * This plugin reads the CAAML files as generated according <A HREF="http://caaml.org/">CAAML V5.0</A>'s
 * <A HREF="http://caaml.org/Schemas/V5.0/Profiles/SnowProfileIACS">specification</A>. In order to validate
 * a CAAML file, download the <A HREF="http://caaml.org/Schemas/V5.0/Profiles/SnowProfileIACS/CAAMLv5_SnowProfileIACS.xsd">xsd</A>
 * file and use either an online XML validator or an offline tool (such as Notepad++'s XML tool).
 *
 * @section caaml_keywords Keywords
 * This plugin uses the following keywords:
 * - COORDSYS:  input coordinate system (see Coords) specified in the [Input] section
 * - SNOW:     specify COSMOCAAML for [Input] section
 * - SNOWPATH: string containing the path to the xml files to be read, specified in the [Input] section
 * - SNOWFILE: specify the xml file to read the data from (optional)
 * - SNOW_PREFIX: file name prefix appearing before the date (optional)
 * - SNOW_EXT: file extension (default: ".xml", give "none" to get an empty string)
 * - STATION#: ID of the station to read
 * - IMIS_STATIONS: if set to true, all station IDs provided above will be stripped of their number (to match MeteoCH naming scheme)
 * - USE_MODEL_LOC: if set to false, the true station location (lat, lon, altitude) is used. Otherwise, it uses the model location (default)
 * - XML_ENCODING: force the input file encoding, overriding the file's own encoding declaration (optional, see \ref caaml_encoding "XML encoding" below)
 *
 * If no SNOWFILE is provided, all "*.caaml" files in the SNOWPATH directory will be read, if they match the SNOW_PREFIX and SNOW_EXT.
 * They <i>must</i> contain the date of the first data formatted as ISO8601 numerical UTC date in their file name. For example, a file containing simulated
 * meteorological fields from 2014-03-03T12:00 until 2014-03-05T00:00 could be named such as "cosmo_201403031200.xml"
 * If some numbers appear <i>before</i> the numerical date, they must be provided as part of SNOW_PREFIX so the plugin can
 * properly extract the date (for MeteoSwiss, this must be set to "VNMH49").
 *
 * Example:
 * @code
 * [Input]
 * COORDSYS	= CH1903
 * SNOW	= CAAML
 * SNOWPATH	= ./input/snowCAAMLdata
 * SNOWFILE	= 5WJ_20120229.caaml
 * @endcode
 *
 * @subsection caaml_encoding XML encoding
 * Each XML document should specify its encoding. However this information might sometimes be missing or even worse, be false. This makes the XML document non-compliant.
 * Normally, CAAML reads the file encoding in the file itself. If this does not work (one of the two cases given above), it is possible to force the
 * encoding of the input file by using the "XML_ENCODING" option. This option takes one of the following values
 * ("LE" stands for "Little Endian" and "BE" for "Big Endian"):
 *  - for UTF/UCS: UTF-8, UTF-16-LE, UTF-16-BE, UCS-4-LE, UCS-4-BE, UCS-4-2143, UCS-4-3412, UCS-2, EBCDIC
 *  - for ISO-8859: ISO-8859-1, ISO-8859-2, ISO-8859-3, ISO-8859-4, ISO-8859-5, ISO-8859-6, ISO-8859-7, ISO-8859-8, ISO-8859-9
 *  - for Japanses: ISO-2022-JP, SHIFT-JIS, EUC-JP
 *  - for ascii: ASCII

 */

//Define namespaces and abbreviations
const xmlChar* CaaMLIO::xml_ns_caaml = (const xmlChar*) "http://caaml.org/Schemas/V5.0/Profiles/SnowProfileIACS";
const xmlChar* CaaMLIO::xml_ns_abrev_caaml = (const xmlChar*) "caaml";
const xmlChar* CaaMLIO::xml_ns_gml = (const xmlChar*) "http://www.opengis.net/gml";
const xmlChar* CaaMLIO::xml_ns_abrev_gml = (const xmlChar*) "gml";
const xmlChar* CaaMLIO::xml_ns_xsi = (const xmlChar*) "http://www.w3.org/2001/XMLSchema-instance";
const xmlChar* CaaMLIO::xml_ns_abrev_xsi = (const xmlChar*) "xsi";
const xmlChar* CaaMLIO::xml_ns_slf = (const xmlChar*) "http://www.slf.ch/snowprofile/1.0";
const xmlChar* CaaMLIO::xml_ns_abrev_slf = (const xmlChar*) "slf";
const xmlChar* CaaMLIO::xml_ns_snp = (const xmlChar*) "http://www.slf.ch/snowpack/1.0";
const xmlChar* CaaMLIO::xml_ns_abrev_snp = (const xmlChar*) "snp";
// const std::string xml_schemaLocation_snp = "http://www.slf.ch/snowpack/snowpack.xsd";
const std::string namespaceCAAML = "caaml";
const std::string namespaceSNP = "snp";
//Define paths in xml-file
const std::string CaaMLIO::TimeData_xpath = "/caaml:SnowProfile/caaml:validTime";
const std::string CaaMLIO::StationMetaData_xpath = "/caaml:SnowProfile/caaml:locRef/caaml:ObsPoint";
const std::string CaaMLIO::SnowData_xpath = "/caaml:SnowProfile/caaml:snowProfileResultsOf/caaml:SnowProfileMeasurements";

CaaMLIO::CaaMLIO(const SnowpackConfig& cfg, const RunInfo& run_info)
           : info(run_info),
             i_snowpath(), sw_mode(), o_snowpath(), experiment(),
             useSoilLayers(false), perp_to_slope(false), aggregate_caaml(false), in_tz(),
             snow_prefix(), snow_ext(".caaml"), caaml_nodata(-999.),
             in_doc(NULL), in_xpathCtx(NULL), in_encoding(XML_CHAR_ENCODING_NONE)
{
	init(cfg);
}

void CaaMLIO::init(const SnowpackConfig& cfg)
{
	std::string tmpstr;

	LIBXML_TEST_VERSION //check lib versions and call xmlInitParser()

	cfg.getValue("SW_MODE", "Snowpack", sw_mode);
	cfg.getValue("SNP_SOIL", "Snowpack", useSoilLayers);
	cfg.getValue("PERP_TO_SLOPE", "SnowpackAdvanced", perp_to_slope);
	cfg.getValue("TIME_ZONE", "Input", in_tz);

	cfg.getValue("SNOW_EXT", "INPUT", snow_ext, IOUtils::nothrow);
	//	if ( IOUtils::strToUpper(snow_ext)=="NONE" ) snow_ext="";
	cfg.getValue("METEOPATH", "Input", tmpstr, IOUtils::nothrow);
	cfg.getValue("SNOWPATH", "Input", i_snowpath, IOUtils::nothrow);
	if (i_snowpath.empty())
		i_snowpath = tmpstr;

	cfg.getValue("AGGREGATE_CAAML", "Output", aggregate_caaml);
	cfg.getValue("EXPERIMENT", "Output", experiment);
	cfg.getValue("METEOPATH", "Output", tmpstr, IOUtils::nothrow);
	cfg.getValue("SNOWPATH", "Output", o_snowpath, IOUtils::nothrow);
	if (o_snowpath.empty())
		o_snowpath = tmpstr;

	//input encoding forcing, inherited from CosmoXMLIO
	tmpstr.clear();
	cfg.getValue("XML_ENCODING", "INPUT", tmpstr, IOUtils::nothrow);
	if (!tmpstr.empty()) {
		if (tmpstr=="UTF-8") in_encoding=XML_CHAR_ENCODING_UTF8;
		else if (tmpstr=="UTF-16-LE") in_encoding=XML_CHAR_ENCODING_UTF16LE;
		else if (tmpstr=="UTF-16-BE") in_encoding=XML_CHAR_ENCODING_UTF16BE;
		else if (tmpstr=="UCS-4-LE") in_encoding=XML_CHAR_ENCODING_UCS4LE;
		else if (tmpstr=="UCS-4-BE") in_encoding=XML_CHAR_ENCODING_UCS4BE;
		else if (tmpstr=="EBCDIC") in_encoding=XML_CHAR_ENCODING_EBCDIC;
		else if (tmpstr=="UCS-4-2143") in_encoding=XML_CHAR_ENCODING_UCS4_2143;
		else if (tmpstr=="UCS-4-3412") in_encoding=XML_CHAR_ENCODING_UCS4_3412;
		else if (tmpstr=="UCS-2") in_encoding=XML_CHAR_ENCODING_UCS2;
		else if (tmpstr=="ISO-8859-1") in_encoding=XML_CHAR_ENCODING_8859_1;
		else if (tmpstr=="ISO-8859-2") in_encoding=XML_CHAR_ENCODING_8859_2;
		else if (tmpstr=="ISO-8859-3") in_encoding=XML_CHAR_ENCODING_8859_3;
		else if (tmpstr=="ISO-8859-4") in_encoding=XML_CHAR_ENCODING_8859_4;
		else if (tmpstr=="ISO-8859-5") in_encoding=XML_CHAR_ENCODING_8859_5;
		else if (tmpstr=="ISO-8859-6") in_encoding=XML_CHAR_ENCODING_8859_6;
		else if (tmpstr=="ISO-8859-7") in_encoding=XML_CHAR_ENCODING_8859_7;
		else if (tmpstr=="ISO-8859-8") in_encoding=XML_CHAR_ENCODING_8859_8;
		else if (tmpstr=="ISO-8859-9") in_encoding=XML_CHAR_ENCODING_8859_9;
		else if (tmpstr=="ISO-2022-JP") in_encoding=XML_CHAR_ENCODING_2022_JP;
		else if (tmpstr=="SHIFT-JIS") in_encoding=XML_CHAR_ENCODING_SHIFT_JIS;
		else if (tmpstr=="EUC-JP") in_encoding=XML_CHAR_ENCODING_EUC_JP;
		else if (tmpstr=="ASCII") in_encoding=XML_CHAR_ENCODING_ASCII;
		else
			throw InvalidArgumentException("Encoding \""+tmpstr+"\" is not supported!", AT);
	}
}

CaaMLIO& CaaMLIO::operator=(const CaaMLIO& source) {
	if (this != &source) {
		caaml_nodata = source.caaml_nodata;
		in_doc = NULL;
		in_xpathCtx = NULL;
	}
	return *this;
}

CaaMLIO::~CaaMLIO() throw()
{
	closeIn_CAAML();
}

void CaaMLIO::openIn_CAAML(const std::string& in_snowfile)
{
//	if (in_doc!=NULL) return; //the file has already been read
	xmlInitParser();
	xmlKeepBlanksDefault(0);

	if (in_encoding==XML_CHAR_ENCODING_NONE) {
		in_doc = xmlParseFile(in_snowfile.c_str());
	} else {
		const xmlParserCtxtPtr ctxt( xmlCreateFileParserCtxt( in_snowfile.c_str() ) );
		xmlSwitchEncoding( ctxt, in_encoding);
		xmlParseDocument( ctxt);
		in_doc = ctxt->myDoc;
	}

	if (in_xpathCtx != NULL) xmlXPathFreeContext(in_xpathCtx); //free variable if this was not freed before
	in_xpathCtx = xmlXPathNewContext(in_doc);
	if (in_xpathCtx == NULL) {
		closeIn_CAAML();
		throw IOException("Unable to create new XPath context", AT);
	}

	if (xmlXPathRegisterNs(in_xpathCtx, xml_ns_abrev_caaml, xml_ns_caaml) != 0) {
		throw IOException("Unable to register namespace with prefix", AT);
	}

	if (xmlXPathRegisterNs(in_xpathCtx, xml_ns_abrev_slf, xml_ns_slf) != 0) {
		throw IOException("Unable to register namespace with prefix", AT);
	}

	if (xmlXPathRegisterNs(in_xpathCtx, xml_ns_abrev_snp, xml_ns_snp) != 0) {
		throw IOException("Unable to register namespace with prefix", AT);
	}
}

void CaaMLIO::closeIn_CAAML() throw()
{
	if (in_xpathCtx!=NULL) {
		xmlXPathFreeContext(in_xpathCtx);
		in_xpathCtx = NULL;
	}
	if (in_doc!=NULL) {
		xmlFreeDoc(in_doc);
		in_doc = NULL;
	}
	xmlCleanupParser();
}

/**
 * @brief This routine checks if the specified snow cover data exists
 * @param i_snowfile file containing the initial state of the snowpack
 * @param stationID
 * @return true if the file exists
 */
bool CaaMLIO::snowCoverExists(const std::string& i_snowfile, const std::string& /*stationID*/) const
{
	std::string snofilename( getFilenamePrefix(i_snowfile, i_snowpath, false) );

	if (snofilename.rfind(".caaml") == string::npos) {
		snofilename += ".caaml";
	}

	return FileUtils::fileExists(snofilename);
}

/**
 * @brief This routine reads the status of the snow cover at program start
 * @param i_snowfile file containing the initial state of the snowpack
 * @param stationID
 * @param SSdata
 * @param Zdata
 * @param read_salinity
 */
void CaaMLIO::readSnowCover(const std::string& i_snowfile, const std::string& stationID,
                            SN_SNOWSOIL_DATA& SSdata, ZwischenData& Zdata, const bool&)
{
	std::string snofilename( getFilenamePrefix(i_snowfile, i_snowpath, false) );
	std::string hazfilename(snofilename);

	if (snofilename.rfind(".caaml") == string::npos) {
		snofilename += ".caaml";
		hazfilename += ".haz";
	} else {
		hazfilename.replace(hazfilename.rfind(".caaml"), 6, ".haz");
	}

	read_snocaaml(snofilename, stationID, SSdata);
	Zdata.reset();
}

// complete filename_prefix
std::string CaaMLIO::getFilenamePrefix(const std::string& fnam, const std::string& path, const bool addexp) const
{
	//TODO: read only once (in constructor)
	std::string filename_prefix( path + "/" + fnam );

	if (addexp && (experiment != "NO_EXP"))
		filename_prefix += "_" + experiment;

	return filename_prefix;
}

//Read CAAML file
bool CaaMLIO::read_snocaaml(const std::string& in_snowFilename, const std::string& stationID, SN_SNOWSOIL_DATA& SSdata)
{
	// Read CAAML snow profile file
	openIn_CAAML(in_snowFilename);

	//Read profile date
	SSdata.profileDate = xmlGetDate();

	//Read station metadata
	SSdata.meta = xmlGetStationData(stationID);

	//Snow-Soil properties: set to default if not available in file
	setCustomSnowSoil(SSdata, in_snowFilename);

	//Read quantity profiles
	std::list<std::string> xpaths;
	xpaths.push_back("/caaml:tempProfile/caaml:Obs");
	xpaths.push_back("/caaml:densityProfile/caaml:Layer");
	xpaths.push_back("/caaml:hardnessProfile/caaml:Layer");
	std::vector<std::vector<double> > depths( xpaths.size() ); //store 3 profiles: obs, density and hardness
	std::vector<std::vector<double> > val( xpaths.size() );

	//Loop on the paths to read corresponding profile
	size_t jj = 0;
	for (std::list<std::string>::iterator path=xpaths.begin(); path!=xpaths.end(); path++, jj++) {
		getProfiles(*path, depths[jj], val[jj]);
	}

	//Read layers
	const xmlNodeSetPtr data( xmlGetData(SnowData_xpath+"/caaml:stratProfile/caaml:Layer") );

	SSdata.nLayers = static_cast<size_t>( data->nodeNr );
	SSdata.Ldata.resize(SSdata.nLayers, LayerData());

	//Loop on the layer nodes to set their properties
	jj = 0;
	if (SSdata.nLayers>0) {
		const bool reverse = getLayersDir(); //Read profile direction
		if (!reverse) {
			for (size_t ii = 0; ii < SSdata.nLayers; ii++, jj++)
				SSdata.Ldata[jj] = xmlGetLayer(data->nodeTab[ii]);
		} else {
			for (size_t ii = SSdata.nLayers; ii-- > 0; jj++)
				SSdata.Ldata[jj] = xmlGetLayer(data->nodeTab[ii]);
		}
	}

	//Set temperature, density and hardness from the profiles
	setProfileVal(SSdata.Ldata, depths, val);

	//Layer default values
 	for (size_t ii = 0; ii < SSdata.nLayers; ii++) {
		//Layer properties: set to default if not available in file
		setCustomLayerData(SSdata.Ldata[ii]);
		SSdata.Ldata[ii].phiVoids = 1. - SSdata.Ldata[ii].phiSoil - SSdata.Ldata[ii].phiWater - SSdata.Ldata[ii].phiWaterPref - SSdata.Ldata[ii].phiIce;
	}

	//Set deposition date from the layers
	setDepositionDates(SSdata.Ldata,SSdata.profileDate);

	//Compute total number of layers and height
	SSdata.nN = 1;
	SSdata.Height = 0.;
	for (size_t ii = 0; ii < SSdata.nLayers; ii++) {
		SSdata.nN += SSdata.Ldata[ii].ne;
		SSdata.Height += SSdata.Ldata[ii].hl;
	}
	SSdata.HS_last = SSdata.Height;

	closeIn_CAAML();

	return true;
}

xmlNodeSetPtr CaaMLIO::xmlGetData(const std::string& path)
{
	const xmlXPathObjectPtr xpathObj( xmlXPathEvalExpression((const xmlChar*)path.c_str(),in_xpathCtx) );
	if (xpathObj == NULL) {
		throw NoDataException("Invalid xpath expression: '"+path+"'", AT);
	}

	xmlNodeSetPtr &data = xpathObj->nodesetval;
 	if (xmlXPathNodeSetIsEmpty(data) || data->nodeNr==0) {
		xmlXPathFreeObject(xpathObj);
 		throw NoDataException("No data found for '"+path+"'", AT);
	}

	return data;
}

Date CaaMLIO::xmlGetDate()
{
	const xmlXPathObjectPtr xpathObj( xmlXPathEvalExpression((const xmlChar*)TimeData_xpath.c_str(),in_xpathCtx) );
	if (xpathObj == NULL) {
		throw NoDataException("Invalid xpath expression: '"+TimeData_xpath+"'", AT);
	}

 	if (xmlXPathNodeSetIsEmpty(xpathObj->nodesetval)) {
		xmlXPathFreeObject(xpathObj);
 		throw NoDataException("No data found for '"+TimeData_xpath+"'", AT);
	}

	const std::string date_str( (char*) xmlNodeGetContent(xpathObj->nodesetval->nodeTab[0]) );

	Date date;
	IOUtils::convertString(date, date_str, in_tz);
	return date;
}

StationData CaaMLIO::xmlGetStationData(const std::string& stationID)
{
	double x=IOUtils::nodata, y=IOUtils::nodata, z=IOUtils::nodata;
	double slopeAngle=IOUtils::nodata, azimuth=IOUtils::nodata;
	std::string stationName;

	const xmlNodeSetPtr data( xmlGetData(StationMetaData_xpath) );
	for (xmlNode *cur_c = data->nodeTab[0]->children; cur_c; cur_c = cur_c->next) {
		if (cur_c->type != XML_TEXT_NODE) {
			const std::string field_name( (const char*)cur_c->name );
			//Ignore some fields
			if (field_name!="customData" && field_name!="comment" && field_name!="metaDataProperty") {
				if (field_name=="name") {
				    stationName = std::string((const char*)xmlNodeGetContent(cur_c));
				} else if (field_name=="validElevation") {
				    sscanf((const char*)xmlNodeGetContent(cur_c),"%lf",&z);
				} else if (field_name=="validSlopeAngle") {
				    sscanf((const char*)xmlNodeGetContent(cur_c),"%lf",&slopeAngle);
				} else if (field_name=="validAspect") {
				    azimuth = IOUtils::bearing( string((const char*)xmlNodeGetContent(cur_c)) );
				} else if (field_name=="pointLocation") {
				    sscanf((const char*)xmlNodeGetContent(cur_c),"%lf %lf",&x,&y);
				}
			}
		}
	}

	Coords tmppos;
	tmppos.setLatLon(x, y, z);
	StationData metatmp(tmppos, stationID, stationName);
	metatmp.setSlope(slopeAngle, azimuth);
	return metatmp;
}

double CaaMLIO::xmlSetVal(const string& xpath, const string& property, const double& dflt)
{
	const std::string path( SnowData_xpath+xpath+":"+property );
	const xmlXPathObjectPtr xpathObj( xmlXPathEvalExpression((const xmlChar*)path.c_str(), in_xpathCtx) );
	double val = IOUtils::nodata;

	if (xpathObj->nodesetval->nodeNr > 0)
		sscanf((const char*)xmlNodeGetContent(xpathObj->nodesetval->nodeTab[0]), "%lf", &val);
	else
		val = dflt;

	xmlXPathFreeObject(xpathObj);
	return val;
}

int CaaMLIO::xmlSetVal(const string& xpath, const std::string& property, const int& dflt)
{
	const std::string path( SnowData_xpath+xpath+":"+property );
	const xmlXPathObjectPtr xpathObj( xmlXPathEvalExpression((const xmlChar*)path.c_str(), in_xpathCtx) );
	int val = IOUtils::inodata;

	if (xpathObj->nodesetval->nodeNr > 0)
		sscanf((const char*)xmlNodeGetContent(xpathObj->nodesetval->nodeTab[0]), "%d", &val);
	else
		val = dflt;

	xmlXPathFreeObject(xpathObj);
	return val;
}

void CaaMLIO::setCustomSnowSoil(SN_SNOWSOIL_DATA& SSdata, const std::string& in_snowFilename)
{
	const std::string xpath( "/caaml:customData/snp" );


	/// REQUIRED PARAMETERS, an error will be thrown if no parameter are provided

	SSdata.Albedo = xmlSetVal(xpath,"Albedo",mio::IOUtils::nodata);
	if (SSdata.Albedo == mio::IOUtils::nodata) {
		throw InvalidFormatException("Can not read Albedo in file "+in_snowFilename, AT);
	}
	SSdata.SoilAlb = xmlSetVal(xpath,"SoilAlb",mio::IOUtils::nodata);
	if (SSdata.SoilAlb == mio::IOUtils::nodata) {
		throw InvalidFormatException("Can not read SoilAlb in file "+in_snowFilename, AT);
	}
	SSdata.BareSoil_z0 = xmlSetVal(xpath,"BareSoil_z0",mio::IOUtils::nodata);
	if (SSdata.BareSoil_z0 == mio::IOUtils::nodata) {
		throw InvalidFormatException("Can not read BareSoil_z0 in file "+in_snowFilename, AT);
	}
	SSdata.Canopy_Height = xmlSetVal(xpath,"CanopyHeight",mio::IOUtils::nodata);
	if (SSdata.Canopy_Height == mio::IOUtils::nodata) {
		throw InvalidFormatException("Can not read CanopyHeight in file "+in_snowFilename, AT);
	}
	SSdata.Canopy_LAI = xmlSetVal(xpath,"CanopyLAI",mio::IOUtils::nodata);
	if (SSdata.Canopy_LAI == mio::IOUtils::nodata) {
		throw InvalidFormatException("Can not read CanopyLAI in file "+in_snowFilename, AT);
	}
	SSdata.Canopy_BasalArea = xmlSetVal(xpath,"CanopyBasalArea",mio::IOUtils::nodata);
	if (SSdata.Canopy_BasalArea == mio::IOUtils::nodata) {
		throw InvalidFormatException("Can not read CanopyBasalArea in file "+in_snowFilename, AT);
	}
	SSdata.Canopy_Direct_Throughfall = xmlSetVal(xpath,"CanopyDirectThroughfall",mio::IOUtils::nodata);
	if (SSdata.Canopy_Direct_Throughfall == mio::IOUtils::nodata) {
		throw InvalidFormatException("Can not read CanopyDirectThroughfall in file "+in_snowFilename, AT);
	}
	SSdata.WindScalingFactor = xmlSetVal(xpath,"WindScalingFactor",mio::IOUtils::nodata);
	if (SSdata.WindScalingFactor == mio::IOUtils::nodata) {
		throw InvalidFormatException("Can not read WindScalingFactor in file "+in_snowFilename, AT);
	}
	SSdata.ErosionLevel = xmlSetVal(xpath,"ErosionLevel",static_cast<int>(mio::IOUtils::nodata));
	if (SSdata.ErosionLevel == mio::IOUtils::nodata) {
		throw InvalidFormatException("Can not read ErosionLevel in file "+in_snowFilename, AT);
	}
	SSdata.TimeCountDeltaHS = xmlSetVal(xpath,"TimeCountDeltaHS",mio::IOUtils::nodata);
	if (SSdata.TimeCountDeltaHS == mio::IOUtils::nodata) {
		throw InvalidFormatException("Can not read TimeCountDeltaHS in file "+in_snowFilename, AT);
	}

	/// CANOPY OPTIONNAL PARAMETERS, a warning will be thrown in CANOPY::Initialze if no value is provided

	SSdata.TimeCountDeltaHS = xmlSetVal(xpath,"CanopySnowIntCapacity",mio::IOUtils::nodata);
	SSdata.TimeCountDeltaHS = xmlSetVal(xpath,"CanopyAlbedoDry",mio::IOUtils::nodata);
	SSdata.TimeCountDeltaHS = xmlSetVal(xpath,"CanopyAlbedoWet",mio::IOUtils::nodata);
	SSdata.TimeCountDeltaHS = xmlSetVal(xpath,"CanopyAlbedoSnow",mio::IOUtils::nodata);
	SSdata.TimeCountDeltaHS = xmlSetVal(xpath,"CanopyDiameter",mio::IOUtils::nodata);
	SSdata.TimeCountDeltaHS = xmlSetVal(xpath,"CanopyFracLAIUpperLayer",mio::IOUtils::nodata);
	SSdata.TimeCountDeltaHS = xmlSetVal(xpath,"CanopyBasalArea",mio::IOUtils::nodata);

}

//Direction in which the layers should be read and stored in SSdata
bool CaaMLIO::getLayersDir()
{
	const xmlXPathObjectPtr xpathObj( xmlXPathEvalExpression((const xmlChar*)SnowData_xpath.c_str(),in_xpathCtx) );
	const std::string direction( (const char*)xmlGetProp(xpathObj->nodesetval->nodeTab[0],(const xmlChar*)"dir") );

	return (direction!="bottom up"); //standard direction -> false, otherwise "true" for Reverse direction
}

LayerData CaaMLIO::xmlGetLayer(xmlNodePtr cur)
{
	std::string code;

	LayerData Layer;
	if (cur->type == XML_ELEMENT_NODE) {
		//Loop on the children
		for (xmlNode *cur_c = cur->children; cur_c; cur_c = cur_c->next) {
			if (cur_c->type != XML_TEXT_NODE) {
				const std::string field_name( (const char*)cur_c->name );
				//Ignore some fields
				if (field_name!="customData" && field_name!="comment" && field_name!="metaDataProperty") {
					//Default reading
					if (field_name!="grainSize") {
						const xmlChar* unit;
						if (strcmp((const char*)cur_c->ns->prefix,"slf")) {
							unit = (const xmlChar*) "uom";
						} else {
							unit = (const xmlChar*) "unit";
						}
						if (!strcmp((const char*) cur_c->name, "depthTop")) {
							double z;
							sscanf((const char*) xmlNodeGetContent(cur_c),"%lf",&z);
						} else if (!strcmp((const char*) cur_c->name, "thickness")) {
							double temp;
							sscanf((const char*) xmlNodeGetContent(cur_c),"%lf",&temp);
							Layer.hl = unitConversion(temp,(char*)xmlGetProp(cur_c,unit),strdup("m"));
							Layer.ne = (size_t) ceil(Layer.hl/0.02);
						} else if (!strcmp((const char*) cur_c->name, "hardness")) {
							//const double hard = hardness_codeToVal((char*) xmlNodeGetContent(cur_c));
						} else if (!strcmp((const char*) cur_c->name, "lwc")) {
							Layer.phiWater = lwc_codeToVal((char*) xmlNodeGetContent(cur_c));
							Layer.phiWaterPref = 0.;
						} else if (!strcmp((const char*) cur_c->name, "grainFormPrimary")) {
							//code = (char*) xmlNodeGetContent(cur_c);
							code = std::string( (char*)xmlNodeGetContent(cur_c) );
							grainShape_codeToVal(code, Layer.sp, Layer.dd, Layer.mk);
						}
					//Treating "grainSize" field
					} else {
						for (xmlNode *cur_cc = cur_c->children; cur_cc; cur_cc = cur_cc->next) {
							if (cur_cc->type != XML_TEXT_NODE) {
								for (xmlNode *cur_ccc = cur_cc->children; cur_ccc; cur_ccc = cur_ccc->next) {
									if (cur_ccc->type != XML_TEXT_NODE) {
										if (!strcmp((const char*) cur_ccc->name, "avg")) {
											sscanf((const char*) xmlNodeGetContent(cur_ccc),"%lf",&Layer.rg);
											Layer.rg = unitConversion(Layer.rg,(char*)xmlGetProp(cur_c,(const xmlChar*)"uom"),strdup("mm"))/2.;
											Layer.rb = Layer.rg/4.;
										}
									}
								}
							}
						}
					}
				}
			}
		}
	} else {
		cur = cur->next;
	}

	if (Layer.rg == 0.) {
	    if (code=="IF") {
		Layer.rg = 3./2.;
		Layer.rb = 3./8.;
	    } else {
		throw IOException("Grain size missing for a non-ice layer!", AT);
	    }
	}

	return Layer;
}

void CaaMLIO::getProfiles(const std::string path, std::vector<double> &depths, std::vector<double> &val)
{
	const xmlNodeSetPtr data( xmlGetData(SnowData_xpath+path) );
	const size_t nrElem = static_cast<size_t>(data->nodeNr);
	depths.resize(nrElem);
	val.resize(nrElem);

	//double l;
	//Loop on the nodes
 	for (size_t ii=0; ii<nrElem; ++ii) {
		if (data->nodeTab[ii]->type == XML_ELEMENT_NODE) {
			//Loop on the children
			for (xmlNode *cur_c = data->nodeTab[ii]->children; cur_c; cur_c = cur_c->next) {
				if (cur_c->type != XML_TEXT_NODE) {
					const std::string field_name( (const char*)cur_c->name );
					//Ignore some fields
					if (field_name!="customData" && field_name!="comment" && field_name!="metaDataProperty") {
						std::string name( field_name );
						if (name.compare(0,4,"snow")==0) name.erase(0,4);

						if (!name.empty()) name[0] = (const char)std::toupper( name[0] );
						const std::string unitname( "uom"+name );

						if (name=="Temp" || name=="Density" || name=="Hardness") {
							sscanf((const char*) xmlNodeGetContent(cur_c), "%lf", &val[ii]);
							if (name=="Temp")
								val[ii] = unitConversion(val[ii],(char*)xmlGetProp(data->nodeTab[ii]->parent,(const xmlChar*)unitname.c_str()),strdup("K"));
						} else if (name.compare(0,5,"Depth")==0) {
							sscanf((const char*) xmlNodeGetContent(cur_c), "%lf", &depths[ii]);
							depths[ii] = unitConversion(depths[ii],(char*)xmlGetProp(data->nodeTab[ii]->parent,(const xmlChar*)unitname.c_str()),strdup("m"));
							/*if (ii>0 && k>0) {
								if (abs(depths[ii-1]-depths[ii]) != l) {
									cout << "Warning: inconsistent " << name << " layers (depths and thicknesses do not match)." << endl;
								}
							}*/
						} else if (name=="Thickness") {
							//sscanf((const char*) xmlNodeGetContent(cur_c), "%lf", &l);
						}
					}
				}
			}
		}
	}

	//If necessary, reverse order
	if (depths.size()>=2 && depths[0]<depths[1]) {
		double temp;
		for (size_t ii=0; ii<floor(nrElem/2); ++ii) {
			temp = depths[ii];
			depths[ii] = depths[nrElem-ii-1];
			depths[nrElem-ii-1] = temp;
			temp = val[ii];
			val[ii] = val[nrElem-ii-1];
			val[nrElem-ii-1] = temp;
		}
	}
}

void CaaMLIO::setProfileVal(std::vector<LayerData> &Layers, std::vector<std::vector<double> > depths, std::vector<std::vector<double> > val)
{
	double z = 0.;
	for (size_t ii=0; ii<Layers.size(); ii++) { //loop over the number of layers
		//profile 0 is the obs profile, 1 is the density and 2 is the hardness
		z += Layers[ii].hl;
		//Compute temperature at the top of the layer
		size_t ind = 0;
		while (ind<depths[0].size() && z<depths[0][ind])
			ind++;

		Layers[ii].tl = val[0][ind];
		if (ind>0 && z>depths[0][ind])
			Layers[ii].tl += (val[0][ind]-val[0][ind-1])*(z-depths[0][ind])/(depths[0][ind]-depths[0][ind-1]);

		//Compute average density and hardness in the layer
		for (size_t k=1; k<3; k++) {
			ind = 0;
			double zprev=z, cumsum=0, weights=0;
			while (ind<depths[k].size() &&  depths[k][ind]>z-Layers[ii].hl) {
				ind++;
				if (depths[k][ind]-z < 1e-12) {
					if (depths[k][ind]<=z-Layers[ii].hl) {
						cumsum += val[k][ind-1]*(zprev-(z-Layers[ii].hl));
						weights += (zprev-(z-Layers[ii].hl));
					} else {
						cumsum += val[k][ind-1]*(zprev-depths[k][ind]);
						weights += (zprev-depths[k][ind]);
						zprev = depths[k][ind];
					}
				}
			}
			if (k==1) {
				Layers[ii].phiIce = (cumsum/weights)/Constants::density_ice;
			}
		}
	}
}

void CaaMLIO::setCustomLayerData(LayerData &Layer) {
	const std::string xpath( "/caaml:stratProfile/caaml:Layer/caaml:customData/snp" );
	Layer.phiSoil = xmlSetVal(xpath,"phiSoil",0.);
	Layer.hr = xmlSetVal(xpath,"SurfaceHoarMass",0.);
	Layer.CDot = xmlSetVal(xpath,"StressRate",0.);
	Layer.metamo = xmlSetVal(xpath,"Metamorphism",0.);
}

//Set the deposition date of the layers based on their arrangement (if no data in the file)
void CaaMLIO::setDepositionDates(std::vector<LayerData> &Layers, const Date profileDate)
{
	for (size_t ii=0; ii<Layers.size(); ii++) {
		if (xmlXPathEvalExpression((const xmlChar*)(SnowData_xpath+"/caaml:stratProfile/caaml:Layer/caaml:customData/snp:DepositionDate").c_str(),in_xpathCtx)->nodesetval->nodeNr) {
			const std::string date_str( (char*) xmlNodeGetContent(xmlGetData(SnowData_xpath+"/caaml:stratProfile/caaml:Layer/caaml:customData/snp:DepositionDate")->nodeTab[0]) );
			Date date;
			IOUtils::convertString(date, date_str, in_tz);
			Layers[ii].depositionDate = date;
		} else {
			const unsigned int snowType = ElementData::snowType(Layers[ii].dd,Layers[ii].sp,Layers[ii].rg,Layers[ii].mk,Layers[ii].phiWater,ElementData::snowResidualWaterContent(Layers[ii].phiIce));
			const unsigned int a = (unsigned int) (snowType/100.);
			if (ii==0) {
				if (a==6) {
					Layers[ii].depositionDate = profileDate;
				} else if (a==0 || a==1) {
					Layers[ii].depositionDate = profileDate-(Date)1./2440638.;
				} else {
					Layers[ii].depositionDate = profileDate-(Date)2./2440638.;
				}
			} else {
				if ((a==0 || a==1) && (Layers[ii-1].depositionDate > profileDate-(Date)2./2440638.)) {
					Layers[ii].depositionDate = profileDate-(Date)1./2440638.;
				} else {
					Layers[ii].depositionDate = profileDate-(Date)2./2440638.;
				}
			}
		}
	}
}

/**
 * @brief This routine writes the status of the snow cover at program termination and at specified backup times
 * @param date current
 * @param Xdata
 * @param Zdata
 * @param forbackup dump Xdata on the go
 */
void CaaMLIO::writeSnowCover(const Date& date, const SnowStation& Xdata,
                             const ZwischenData& Zdata, const bool& forbackup)
{
	std::string snofilename( getFilenamePrefix(Xdata.meta.getStationID().c_str(), o_snowpath) + ".caaml" );
	std::string hazfilename( getFilenamePrefix(Xdata.meta.getStationID().c_str(), o_snowpath) + ".haz" );

	if (forbackup) {
		stringstream ss;
		ss << (int)(date.getJulian() + 0.5); //HACK
		snofilename += ss.str();
		hazfilename += ss.str();
	}

	writeSnowFile(snofilename, date, Xdata, aggregate_caaml);
	SmetIO::writeHazFile(hazfilename, date, Xdata, Zdata);
}

void CaaMLIO::writeSnowFile(const std::string& snofilename, const Date& date, const SnowStation& Xdata,
                            const bool /*aggregate*/)
{
	xmlTextWriterPtr writer( xmlNewTextWriterFilename(snofilename.c_str(), 0) );
	xmlTextWriterSetIndent(writer,3);
	xmlTextWriterStartDocument(writer, NULL, "UTF-8", NULL);

	xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":SnowProfile").c_str());
	xmlTextWriterWriteAttribute(writer,(const xmlChar*)("xmlns:"+string((const char*)xml_ns_abrev_caaml)).c_str(),xml_ns_caaml);
	xmlTextWriterWriteAttribute(writer,(const xmlChar*)("xmlns:"+string((const char*)xml_ns_abrev_gml)).c_str(),xml_ns_gml);
	xmlTextWriterWriteAttribute(writer,(const xmlChar*)("xmlns:"+string((const char*)xml_ns_abrev_xsi)).c_str(),xml_ns_xsi);
	// xmlTextWriterWriteAttribute(writer,(const xmlChar*)"xsi:schemaLocation",(const xmlChar*)(string((const char*)xml_ns_snp)+" "+xml_schemaLocation_snp).c_str());
	xmlTextWriterWriteAttribute(writer,(const xmlChar*)"xmlns:snp",(const xmlChar*)xml_ns_snp);
	xmlTextWriterWriteAttribute(writer,(const xmlChar*)("xmlns:"+string((const char*)xml_ns_abrev_slf)).c_str(),xml_ns_slf);
	xmlTextWriterWriteAttribute(writer,(const xmlChar*)"gml:id",(const xmlChar*)("SLF_"+Xdata.meta.stationID).c_str());

	//Required fields
	xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":metaDataProperty").c_str());
	xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":MetaData").c_str());
	time_t now; //HACK
	time(&now);
	struct tm *timeinfo = localtime(&now);
	strftime(dateStr,30,"%FT%T.000+01:00",timeinfo);
	writeDate(writer,":dateTimeReport",dateStr);
	xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":srcRef").c_str());
	xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":Operation").c_str());
	xmlTextWriterWriteAttribute(writer,(const xmlChar*)"gml:id",(const xmlChar*)"OPERATION_ID");
	xmlWriteElement(writer,(namespaceCAAML+":name").c_str(),"SNOWPACK","","");
	xmlTextWriterEndElement(writer);
	xmlTextWriterEndElement(writer);
	xmlTextWriterEndElement(writer);
	xmlTextWriterEndElement(writer);

	// Write profile date
	xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":validTime").c_str());
	xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":TimeInstant").c_str());
	const double tz = date.getTimeZone();
	sprintf(dateStr,"%s.000%+03d:%02d",date.toString(Date::ISO).c_str(),(int) tz,(int) (60*(tz-(int)tz))); //HACK: not (int)tz but floor(tz)!
	writeDate(writer,":timePosition",dateStr);
	xmlTextWriterEndElement(writer);
	xmlTextWriterEndElement(writer);

	// Write stratigraphic profile
	xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":snowProfileResultsOf").c_str());
	xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":SnowProfileMeasurements").c_str());
	xmlTextWriterWriteAttribute(writer,(const xmlChar*)"dir",(const xmlChar*)"top down");

	//Write custom snow/soil data
	xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":customData").c_str());
	writeCustomSnowSoil(writer,Xdata);
	xmlTextWriterEndElement(writer);

	// Write profile depth
	sprintf(valueStr,"%.4f",100.*Xdata.cH);
	xmlWriteElement(writer,(namespaceCAAML+":profileDepth").c_str(),valueStr,"uom","cm");

	//Write height of snow and Snow Water Equivalent (SWE)
	xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":hS").c_str());
	xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":Components").c_str());
	sprintf(valueStr,"%.4f",100.*(Xdata.cH - Xdata.Ground));
	xmlWriteElement(writer,(namespaceCAAML+":snowHeight").c_str(),valueStr,"uom","cm");
	sprintf(valueStr,"%.2f",Xdata.swe);
	xmlWriteElement(writer,(namespaceCAAML+":swe").c_str(),valueStr,"uom","mm");
	xmlTextWriterEndElement(writer);
	xmlTextWriterEndElement(writer);

	//Write layers and quantity profiles
	writeLayers(writer,Xdata);
	writeProfiles(writer,Xdata);

	xmlTextWriterEndElement(writer);
	xmlTextWriterEndElement(writer); // end stratigraphic profile

	// Write station data
	writeStationData(writer,Xdata);

	xmlTextWriterEndElement(writer);

	xmlTextWriterEndDocument(writer);
	xmlFreeTextWriter(writer);
}

void CaaMLIO::xmlWriteElement(const xmlTextWriterPtr writer, const char* name, const char* content, const char* att_name, const char* att_val)
{
	xmlTextWriterStartElement(writer, (const xmlChar*) name);
	if (strcmp(att_name,"")) //ie: string not empty
		xmlTextWriterWriteAttribute(writer, (const xmlChar*) att_name, (const xmlChar*) att_val);
	xmlTextWriterWriteString(writer, (const xmlChar*) content);
	xmlTextWriterEndElement(writer);
}

// void CaaMLIO::writeDate(const xmlTextWriterPtr writer, const Date date)
void CaaMLIO::writeDate(const xmlTextWriterPtr writer, const char* att_name, const char* att_val)
{
	xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+att_name).c_str());
	xmlTextWriterWriteAttribute(writer,(const xmlChar*)"xmlns:xs",(const xmlChar*)("http://www.w3.org/2001/XMLSchema"));
	xmlTextWriterWriteAttribute(writer,(const xmlChar*)"xsi:type",(const xmlChar*)("xs:dateTime")); //,timeNow);
	xmlTextWriterWriteString(writer, (const xmlChar*) att_val);
	xmlTextWriterEndElement(writer);
}

void CaaMLIO::writeCustomSnowSoil(const xmlTextWriterPtr writer, const SnowStation& Xdata)
{
	sprintf(valueStr,"%.4f",Xdata.Albedo);
	xmlWriteElement(writer,(namespaceSNP+":Albedo").c_str(),valueStr,"","");
	sprintf(valueStr,"%.4f",Xdata.SoilAlb);
	xmlWriteElement(writer,(namespaceSNP+":SoilAlb").c_str(),valueStr,"","");
	sprintf(valueStr,"%.4f",Xdata.BareSoil_z0);
	xmlWriteElement(writer,(namespaceSNP+":BareSoil_z0").c_str(),valueStr,"uom","m");
	sprintf(valueStr,"%.4f",Xdata.Cdata.height);
	xmlWriteElement(writer,(namespaceSNP+":CanopyHeight").c_str(),valueStr,"uom","m");
	sprintf(valueStr,"%.4f",Xdata.Cdata.lai);
	xmlWriteElement(writer,(namespaceSNP+":CanopyLAI").c_str(),valueStr,"","");
	sprintf(valueStr,"%.4f",Xdata.Cdata.BasalArea);
	xmlWriteElement(writer,(namespaceSNP+":CanopyBasalArea").c_str(),valueStr,"","");
	sprintf(valueStr,"%.4f",Xdata.Cdata.throughfall);
	xmlWriteElement(writer,(namespaceSNP+":CanopyDirectThroughfall").c_str(),valueStr,"","");
	sprintf(valueStr,"%.4f",Xdata.Cdata.int_cap_snow);
	xmlWriteElement(writer,(namespaceSNP+":CanopySnowIntCapacity").c_str(),valueStr,"","");
	sprintf(valueStr,"%.4f",Xdata.Cdata.can_alb_dry);
	xmlWriteElement(writer,(namespaceSNP+":CanopyAlbedoDry").c_str(),valueStr,"","");
	sprintf(valueStr,"%.4f",Xdata.Cdata.can_alb_wet);
	xmlWriteElement(writer,(namespaceSNP+":CanopyAlbedoWet").c_str(),valueStr,"","");
	sprintf(valueStr,"%.4f",Xdata.Cdata.can_alb_snow);
	xmlWriteElement(writer,(namespaceSNP+":CanopyAlbedoSnow").c_str(),valueStr,"","");
	sprintf(valueStr,"%.4f",Xdata.Cdata.can_diameter);
	xmlWriteElement(writer,(namespaceSNP+":CanopyDiameter").c_str(),valueStr,"","");
	sprintf(valueStr,"%.4f",Xdata.Cdata.lai_frac_top_default);
	xmlWriteElement(writer,(namespaceSNP+":CanopyFracLAIUpperLayer").c_str(),valueStr,"","");
	sprintf(valueStr,"%.4f",Xdata.Cdata.BasalArea);
	xmlWriteElement(writer,(namespaceSNP+":CanopyBasalArea").c_str(),valueStr,"","");
	sprintf(valueStr,"%.4f",Xdata.WindScalingFactor);
	xmlWriteElement(writer,(namespaceSNP+":WindScalingFactor").c_str(),valueStr,"","");
	sprintf(valueStr,"%d",static_cast<unsigned int>(Xdata.ErosionLevel));
	xmlWriteElement(writer,(namespaceSNP+":ErosionLevel").c_str(),valueStr,"","");
	sprintf(valueStr,"%.4f",Xdata.TimeCountDeltaHS);
	xmlWriteElement(writer,(namespaceSNP+":TimeCountDeltaHS").c_str(),valueStr,"","");
}

void CaaMLIO::writeLayers(const xmlTextWriterPtr writer, const SnowStation& Xdata)
{
	xmlTextWriterStartElement( writer,(const xmlChar*)(namespaceCAAML+":stratProfile").c_str() );
	if (!Xdata.Edata.empty()) {
		for (size_t ii = Xdata.Edata.size(); ii-->0;) {
			const bool snowLayer = (ii >= Xdata.SoilNode);
			xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":Layer").c_str());

			// Write custom layer data
			xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":customData").c_str());
			writeCustomLayerData(writer,Xdata.Edata[ii],Xdata.Ndata[ii]);
			xmlTextWriterEndElement(writer);

			// Write snow and soil layer data
			sprintf(layerDepthTopStr,"%.4f",100.*(Xdata.cH - Xdata.Ndata[ii+1].z));
			xmlWriteElement(writer,(namespaceCAAML+":depthTop").c_str(),layerDepthTopStr,"uom","cm");
			sprintf(layerThicknessStr,"%.4f",100.*Xdata.Edata[ii].L);
			xmlWriteElement(writer,(namespaceCAAML+":thickness").c_str(),layerThicknessStr,"uom","cm");

			if (snowLayer) {
				//const unsigned int snowType = ElementData::snowType(Xdata.Edata[ii].dd, Xdata.Edata[ii].sp, Xdata.Edata[ii].rg, Xdata.Edata[ii].mk, Xdata.Edata[ii].theta[WATER],  Xdata.Edata[ii].res_wat_cont);
				const unsigned int snowType = Xdata.Edata[ii].getSnowType();
				const unsigned int a = snowType/100;
				const unsigned int b = (snowType-100*a)/10;
				const unsigned int c = snowType-100*a-10*b;
				if (c != 2) {
					xmlWriteElement(writer,(namespaceCAAML+":grainFormPrimary").c_str(),grainShape_valToAbbrev(a).c_str(),"","");
				} else {
					xmlWriteElement(writer,(namespaceCAAML+":grainFormPrimary").c_str(),"MFcr","","");
				}
				xmlWriteElement(writer,(namespaceCAAML+":grainFormSecondary").c_str(),grainShape_valToAbbrev(b).c_str(),"","");
			}
			xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":grainSize").c_str());
			xmlTextWriterWriteAttribute(writer,(const xmlChar*)"uom",(const xmlChar*)"mm");
			xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":Components").c_str());
			sprintf(layerValStr,"%.3f",2.*Xdata.Edata[ii].rg);
			xmlWriteElement(writer,(namespaceCAAML+":avg").c_str(),layerValStr,"","");
			xmlTextWriterEndElement(writer);
			xmlTextWriterEndElement(writer);
			xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":validFormationTime").c_str());
			xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":TimeInstant").c_str());
			const double tz = Xdata.Edata[ii].depositionDate.getTimeZone();
			sprintf(dateStr,"%s.000%+03d:%02d",Xdata.Edata[ii].depositionDate.toString(Date::ISO).c_str(),(int) tz,(int) (60*(tz-(int)tz)));
			writeDate(writer,":timePosition",dateStr);
			xmlTextWriterEndElement(writer);
			xmlTextWriterEndElement(writer);
			if (snowLayer) {
				xmlWriteElement(writer,(namespaceCAAML+":hardness").c_str(),hardness_valToCode(Xdata.Edata[ii].hard).c_str(),"uom",""); //HACK: check values... seem always the same!
			}
			xmlWriteElement(writer,(namespaceCAAML+":lwc").c_str(),lwc_valToCode(Xdata.Edata[ii].theta[WATER]).c_str(),"uom","");
			sprintf(layerValStr,"%.2f",Xdata.Edata[ii].Rho);
			xmlWriteElement(writer,(namespaceCAAML+":density").c_str(),layerValStr,"uom","kgm-3");
			// snow properties only
			if (snowLayer) {
				sprintf(layerValStr,"%.2f",Xdata.Edata[ii].ogs);
				xmlWriteElement(writer,(namespaceCAAML+":specSurfArea").c_str(),layerValStr,"uom","m2kg-1");
				xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":layerStrength").c_str());
				xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":Components").c_str());
				sprintf(layerValStr,"%.2f",Xdata.Edata[ii].s_strength);
				xmlWriteElement(writer,(namespaceCAAML+":strengthValue").c_str(),layerValStr,"uom","Nm-2");
				xmlTextWriterEndElement(writer);
				xmlTextWriterEndElement(writer);
				// sprintf(layerValStr,"%.2f",Xdata.Edata[ii].solute);
				// xmlWriteElement(writer,(namespaceCAAML+":impurities").c_str(),layerValStr,"uom","");
			}
			xmlTextWriterEndElement(writer);
		}
	}
	xmlTextWriterEndElement(writer);
}

void CaaMLIO::writeCustomLayerData(const xmlTextWriterPtr writer, const ElementData& Edata, const NodeData& Ndata)
{
	// const double tz = Edata.depositionDate.getTimeZone();
	// sprintf(dateStr,"%s:00.000%+03d:%02d",Edata.depositionDate.toString(Date::ISO).c_str(),(int) tz,(int) (60*(tz-(int)tz)));
	// xmlWriteElement(writer,(namespaceSNP+":DepositionDate").c_str(),dateStr,"","");
	sprintf(valueStr,"%.4f",Edata.theta[SOIL]);
	xmlWriteElement(writer,(namespaceSNP+":phiSoil").c_str(),valueStr,"","");
	sprintf(valueStr,"%.4f",Edata.soil[2]);
	xmlWriteElement(writer,(namespaceSNP+":SoilRho").c_str(),valueStr,"uom","kgm-3");
	sprintf(valueStr,"%.4f",Edata.soil[0]);
	xmlWriteElement(writer,(namespaceSNP+":SoilK").c_str(),valueStr,"uom","Wm-1s-1");
	sprintf(valueStr,"%.4f",Edata.soil[1]);
	xmlWriteElement(writer,(namespaceSNP+":SoilC").c_str(),valueStr,"uom","Jkg-1");
	sprintf(valueStr,"%.4f",Edata.rb);
	xmlWriteElement(writer,(namespaceSNP+":bondSize").c_str(),valueStr,"","");
	sprintf(valueStr,"%.2f",Edata.dd);
	xmlWriteElement(writer,(namespaceSNP+":dendricity").c_str(),valueStr,"","");
	sprintf(valueStr,"%.2f",Edata.sp);
	xmlWriteElement(writer,(namespaceSNP+":sphericity").c_str(),valueStr,"","");
	sprintf(valueStr,"%4lu",Edata.mk);
	xmlWriteElement(writer,(namespaceSNP+":marker").c_str(),valueStr,"","");
	sprintf(valueStr,"%.4f",Ndata.hoar);
	xmlWriteElement(writer,(namespaceSNP+":SurfaceHoarMass").c_str(),valueStr,"uom","kgm-2");
	xmlWriteElement(writer,(namespaceSNP+":ne").c_str(),"1","","");
	sprintf(valueStr,"%.4f",Edata.CDot);
	xmlWriteElement(writer,(namespaceSNP+":StressRate").c_str(),valueStr,"uom","Nm-2s-1");
	sprintf(valueStr,"%.4f",Edata.metamo);
	xmlWriteElement(writer,(namespaceSNP+":Metamorphism").c_str(),valueStr,"","");
}

void CaaMLIO::writeProfiles(const xmlTextWriterPtr writer, const SnowStation& Xdata)
{
	// temperature profile
	xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":tempProfile").c_str());
		xmlTextWriterWriteAttribute(writer,(const xmlChar*)"uomDepth",(const xmlChar*)"cm");
		xmlTextWriterWriteAttribute(writer,(const xmlChar*)"uomTemp",(const xmlChar*)"degC");
		if (!Xdata.Ndata.empty()) {
			for (size_t ii = Xdata.Ndata.size(); ii-->0;) {
				xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":Obs").c_str());
				sprintf(layerDepthTopStr,"%.4f",100*(Xdata.cH - Xdata.Ndata[ii].z));
				xmlWriteElement(writer,(namespaceCAAML+":depth").c_str(),layerDepthTopStr,"","");
				sprintf(valueStr,"%.3f",unitConversion(Xdata.Ndata[ii].T,strdup("degK"),strdup("degC")));
				xmlWriteElement(writer,(namespaceCAAML+":snowTemp").c_str(),valueStr,"","");
				xmlTextWriterEndElement(writer);
			}
		}
	xmlTextWriterEndElement(writer);	//end temperature profile

	// density profile; not needed, erase later
	xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":densityProfile").c_str());
		xmlTextWriterWriteAttribute(writer,(const xmlChar*)"uomDepthTop",(const xmlChar*)"cm");
		xmlTextWriterWriteAttribute(writer,(const xmlChar*)"uomThickness",(const xmlChar*)"cm");
		xmlTextWriterWriteAttribute(writer,(const xmlChar*)"uomDensity",(const xmlChar*)"kgm-3");
		if (!Xdata.Edata.empty()) {
			for (size_t ii = Xdata.Edata.size(); ii-->0;) {
				xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":Layer").c_str());
				sprintf(layerDepthTopStr,"%.4f",100*(Xdata.cH - Xdata.Ndata[ii+1].z));
				xmlWriteElement(writer,(namespaceCAAML+":depthTop").c_str(),layerDepthTopStr,"","");
				sprintf(layerThicknessStr,"%.4f",100*Xdata.Edata[ii].L);
				xmlWriteElement(writer,(namespaceCAAML+":thickness").c_str(),layerThicknessStr,"","");
				sprintf(valueStr,"%.2f",Xdata.Edata[ii].Rho);
				xmlWriteElement(writer,(namespaceCAAML+":density").c_str(),valueStr,"","");
				xmlTextWriterEndElement(writer);
			}
		}
	xmlTextWriterEndElement(writer);	//end densityProfile
}

void CaaMLIO::writeStationData(const xmlTextWriterPtr writer, const SnowStation& Xdata)
{
	xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":locRef").c_str());	//start locRef
		xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":ObsPoint").c_str());	//start ObsPoint
			xmlTextWriterWriteAttribute(writer,(const xmlChar*)"gml:id",(const xmlChar*)("SLF_"+Xdata.meta.stationID+"_1").c_str());
			xmlWriteElement(writer,(namespaceCAAML+":name").c_str(),(const char*) Xdata.meta.stationName.c_str(),"","");
			xmlWriteElement(writer,(namespaceCAAML+":obsPointSubType").c_str(),"","","");

			xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":validElevation").c_str());	//start validElevation
				xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":ElevationPosition").c_str());	//start ElevationPosition
					xmlTextWriterWriteAttribute(writer,(const xmlChar*)"uom",(const xmlChar*)"m");
					char elevStr[5];
					sprintf(elevStr,"%.0f",Xdata.meta.position.getAltitude());
					xmlWriteElement(writer,(namespaceCAAML+":position").c_str(),elevStr,"","");
				xmlTextWriterEndElement(writer);	//end ElevationPosition
			xmlTextWriterEndElement(writer);	//end validElevation

			xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":validAspect").c_str());	//start validAspect
				xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":AspectPosition").c_str());	//start AspectPosition
					xmlWriteElement(writer,(namespaceCAAML+":position").c_str(),IOUtils::bearing(Xdata.meta.getAzimuth()).c_str(),"","");
				xmlTextWriterEndElement(writer);	//end AspectPosition
			xmlTextWriterEndElement(writer);	//end validAspect

			xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":validSlopeAngle").c_str());	//start validSlopeAngle
				xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":SlopeAnglePosition").c_str());	//start SlopeAnglePosition
					xmlTextWriterWriteAttribute(writer,(const xmlChar*)"uom",(const xmlChar*)"deg");
					char slopStr[5];
					sprintf(slopStr,"%.0f",Xdata.meta.getSlopeAngle());
					xmlWriteElement(writer,(namespaceCAAML+":position").c_str(),slopStr,"","");
				xmlTextWriterEndElement(writer);	//end SlopeAnglePosition
			xmlTextWriterEndElement(writer);	//end validSlopeAngle

			xmlTextWriterStartElement(writer,(const xmlChar*)(namespaceCAAML+":pointLocation").c_str());	//start pointLocation
				xmlTextWriterStartElement(writer,(const xmlChar*)"gml:Point");	//start gml:Point
					xmlTextWriterWriteAttribute(writer,(const xmlChar*)"gml:id",(const xmlChar*)("SLF_"+Xdata.meta.stationID+"_2").c_str());
					xmlTextWriterWriteAttribute(writer,(const xmlChar*)"srsName",(const xmlChar*)"urn:ogc:def:crs:OGC:1.3:CRS84");
					xmlTextWriterWriteAttribute(writer,(const xmlChar*)"srsDimension",(const xmlChar*)"2");
					char posStr[30];
					sprintf(posStr,"%f %f",Xdata.meta.position.getLat(),Xdata.meta.position.getLon());
					xmlWriteElement(writer,"gml:pos",posStr,"","");
				xmlTextWriterEndElement(writer);	//end gml:Point
			xmlTextWriterEndElement(writer);	//end pointLocation

		xmlTextWriterEndElement(writer);	//end ObsPoint
	xmlTextWriterEndElement(writer);	//end locRef
}

void CaaMLIO::writeTimeSeries(const SnowStation& /*Xdata*/, const SurfaceFluxes& /*Sdata*/, const CurrentMeteo& /*Mdata*/,
                              const ProcessDat& /*Hdata*/, const double /*wind_trans24*/)
{
	throw IOException("Nothing implemented here!", AT);
}

void CaaMLIO::writeProfile(const Date& /*date*/, const SnowStation& /*Xdata*/)
{
	throw IOException("Nothing implemented here!", AT);
}

bool CaaMLIO::writeHazardData(const std::string& /*stationID*/, const std::vector<ProcessDat>& /*Hdata*/,
                             const std::vector<ProcessInd>& /*Hdata_ind*/, const size_t& /*num*/)
{
	throw IOException("Nothing implemented here!", AT);
}

/**
 * @brief Convert from liquid water content code to value
 * @author Adrien Gaudard
 * @param code Liquid water content code (one character)
 * return Liquid water content value (fraction)
 */
double CaaMLIO::lwc_codeToVal(const char* code)
{
	if (!strcmp(code,"D")) return 0.;
	if (!strcmp(code,"M")) return 0.01;
	if (!strcmp(code,"W")) return 0.03;
	if (!strcmp(code,"V")) return 0.08;
	if (!strcmp(code,"S")) return 0.15;

	throw IOException("Unrecognized liquid water content code.", AT);
}

/**
 * @brief Convert from liquid water content value to code
 * @author Adrien Gaudard
 * @param val Liquid water content value (fraction)
 * return Liquid water content code (one character)
 */
std::string CaaMLIO::lwc_valToCode(const double val)
{
	if (val == 0.00) return "D";
	if (val <  0.03) return "M";
	if (val <  0.08) return "W";
	if (val <  0.15) return "V";
	if (val <  1.00) return "S";

	throw IOException("Invalid liquid water content value.", AT);
}

/**
 * @brief Convert from hardness code to value
 * @author Adrien Gaudard
 * @param code Hardness code
 * return Hardness value (1 to 6)
 */
double CaaMLIO::hardness_codeToVal(char* code)
{
	double val = 0.;
	unsigned int n = 0;
	char* c[2];
	c[0] = strtok(code,"-");
	c[1] = strtok(NULL,"-");

	for (size_t i=0; i<2; i++) {
		if (c[i]) {
			n++;
			if (!strcmp(c[i],"F")) {
				val += 1.;
			} else if (!strcmp(c[i],"4F")) {
				val += 2.;
			} else if (!strcmp(c[i],"1F")) {
				val += 3.;
			} else if (!strcmp(c[i],"P")) {
				val += 4.;
			} else if (!strcmp(c[i],"K")) {
				val += 5.;
			} else if (!strcmp(c[i],"I")) {
				val += 6.;
			} else {
				throw IOException("Unrecognized hardness code.", AT);
			}
		}
	}
	return val/n;
}

/**
 * @brief Convert from hardness value to code
 * @author Adrien Gaudard
 * @param val Hardness value (1 to 6)
 * return Hardness code
 */
std::string CaaMLIO::hardness_valToCode(const double val)
{
	if (val == 1.0) return "F";
	if (val == 1.5) return "F-4F";
	if (val == 2.0) return "4F";
	if (val == 2.5) return "4F-1F";
	if (val == 3.0) return "1F";
	if (val == 3.5) return "1F-P";
	if (val == 4.0) return "P";
	if (val == 4.5) return "P-K";
	if (val == 5.0) return "K";
	if (val == 5.5) return "K-I";
	if (val == 6.0) return "I";

	throw IOException("Unrecognized hardness value.", AT);
}

/**
 * @brief Convert from grain shape code to values (sphericity, dendricity, marker)
 * @author Adrien Gaudard
 * @param[in] code Grain shape code
 * @param[out] sp sphericity
 * @param[out] dd dendricity
 * @param[out] mk micro-structure marker
 */
void CaaMLIO::grainShape_codeToVal(const std::string& code, double &sp, double &dd, unsigned short int &mk)
{
	if (code=="PP") {
		sp = 0.5; dd = 1.; mk = 0;
	} else if (code=="DF") {
		sp = 0.5; dd = 0.5; mk = 0;
	} else if (code=="RG") {
		sp = 1.; dd = 0.; mk = 2;
	} else if (code=="FC") {
		sp = 0.; dd = 0.; mk = 1;
	} else if (code=="DH") {
		sp = 0.; dd = 0.; mk = 1;
	} else if (code=="SH") {
		sp = 0.; dd = 0.; mk = 1;
	} else if (code=="MF") {
		sp = 1.; dd = 0.; mk = 2;
	} else if (code=="IF") {
		sp = 1.; dd = 0.; mk = 2;
	} else {
		throw IOException("Unrecognized grain shape code.", AT);
	}
}

/**
 * @brief Convert from grain shape value to code
 * @author Adrien Gaudard
 * @param var Grain shape value
 * return Grain shape code
 */
std::string CaaMLIO::grainShape_valToAbbrev(const unsigned int var)
{
	if (var == 0) return "PPgp";
	if (var == 1) return "PP";
	if (var == 2) return "DF";
	if (var == 3) return "RG";
	if (var == 4) return "FC";
	if (var == 5) return "DH";
	if (var == 6) return "SH";
	if (var == 7) return "MF";
	if (var == 8) return "IF";
	if (var == 9) return "FCxr";

	throw IOException("Unrecognized grain shape code.", AT);
}

/**
 * @brief Convert from grain shape values (sphericity, dendricity, marker) to two-character code
 * @author Adrien Gaudard
 * @param var Grain shape values (sphericity, dendricity, marker) (AMBIGUOUS)
 * return Grain shape two-character code (NOT ALL REPRESENTED)
 */
std::string CaaMLIO::grainShape_valToAbbrev_old(const double* var)
{
	const double sp = ((int)(var[0]*10+0.5))/10.;
	const double dd = ((int)(var[1]*10+0.5))/10.;
	const double mk = ((int)(var[2]*10+0.5))/10.;

	if (sp == 0.5 && dd == 1. && mk == 0.) return "PP";
	if (sp == 0.5 && dd == 0.5 && mk == 0.) return "DF";
	if (sp == 1. && dd == 0. && (mk == 2. || mk == 12.)) return "RG";
	if (sp == 0. && dd == 0. && mk == 1.) return "FC";
	if (sp == 0. && dd == 0. && mk == 1.) return "DH";
	if (sp == 0. && dd == 0. && mk == 1.) return "SH";
	if (sp == 1. && dd == 0. && mk == 2.) return "MF";
	if (sp == 1. && dd == 0. && mk == 2.) return "IF";

	throw IOException("Unrecognized set of grain shape values.", AT);
}
