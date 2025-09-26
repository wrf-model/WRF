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

#include <snowpack/plugins/ImisDBIO.h>
#include <snowpack/Utils.h>
#include <snowpack/snowpackCore/Aggregate.h>

#include <algorithm>

using namespace std;
using namespace mio;
using namespace oracle;
using namespace oracle::occi;

/**
 * @page profile_imis IMIS
 * @section imis_description Description
 * This plugin writes the profiles time series and meteorological time series and fluxes in the 
 * <A HREF="http://www.slf.ch/ueber/organisation/warnung_praevention/warn_informationssysteme/messnetze_daten/imis/index_EN">IMIS</A>
 * database and therefore provides the simulation data for the <A HREF="https://www.ifkis.ch/">IFKIS</A> platform.
 * @note The profiles are simplifed (ie. aggregated) in order to reduce their number of layers in this plugin.
 *
 * @section imis_keywords Keywords
 * The following keywords are used by this plugin:
 * - DBNAME: which database to use, using the Oracle SID name as defined in tns_names, [Output] section;
 * - DBUSER: user name, [Output] section;
 * - DBPASS: user password, [Output] section;
 * - HOAR_DENSITY_SURF: density of the surface hoar (kg/m3), [SnowpackAdvanced] section;
 * - HOAR_MIN_SIZE_SURF: minimum size to show surface hoar on the surface (mm), [SnowpackAdvanced] section.
 * 
 */

const double ImisDBIO::time_zone = 1.; //All IMIS data is in gmt+1

const string ImisDBIO::sqlDeleteHdata = "DELETE FROM snowpack.ams_pmod WHERE stat_abk=:1 and stao_nr=:2 and datum>=:3 and datum<=:4";

const string ImisDBIO::sqlDeleteProfile = "DELETE FROM snowpack.ams_pmod_profile WHERE stat_abk=:1 and stao_nr=:2 and datum>=:3 and datum<=:4";

const string ImisDBIO::sqlInsertHdata = "INSERT INTO snowpack.ams_pmod(datum,stat_abk,stao_nr,dewpt_def,hoar_ind6,hoar_ind24,wind_trans,hns3,hns6,hns12,hns24,hns72,hns72_24,wc3,wc6,wc12,wc24,wc72,hoar_size,wind_trans24,stab_class1,stab_class2,stab_index1,stab_height1,stab_index2,stab_height2,stab_index3,stab_height3,stab_index4,stab_height4,stab_index5,stab_height5,ch,crust,en_bal,sw_net,t_top1,t_top2,snowpack_version,calc_date,swe,tot_lwc,runoff,lwi_n,lwi_s) values (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14,:15,:16,:17,:18,:19,:20,:21,:22,:23,:24,:25,:26,:27,:28,:29,:30,:31,:32,:33,:34,:35,:36,:37,:38,:39,:40,:41,:42,:43,:44,:45)";

const string ImisDBIO::sqlInsertProfile = "INSERT INTO snowpack.ams_pmod_profile(datum,stat_abk,stao_nr,height,layer_date,rho,tem,tem_grad,strain_rate,theta_w,theta_i,dendricity,sphericity,coordin_num,grain_dia,bond_dia,grain_class,snowpack_version,calc_date) values (:1,:2,:3,:4,:5,:6,:7,:8,:9,:10,:11,:12,:13,:14,:15,:16,:17,:18,:19)";

double ImisDBIO::hoar_density_surf = 0.0;
double ImisDBIO::hoar_min_size_surf = 0.0;

ImisDBIO::ImisDBIO(const SnowpackConfig& cfg, const RunInfo& run_info)
         : info(run_info), env(NULL), conn(NULL), stmt(NULL),
           oracleDB(getKey(cfg, "DBNAME", "Output")), oracleUser(getKey(cfg, "DBUSER", "Output")),
           oraclePassword(getKey(cfg, "DBPASS", "Output"))
{
	//Density of surface hoar (-> hoar index of surface node) (kg m-3)
	cfg.getValue("HOAR_DENSITY_SURF", "SnowpackAdvanced", hoar_density_surf);

	//Minimum size to show surface hoar on surface (mm)
	cfg.getValue("HOAR_MIN_SIZE_SURF", "SnowpackAdvanced", hoar_min_size_surf);

	openDB();
}

std::string ImisDBIO::getKey(const SnowpackConfig& i_cfg, const std::string& key, const std::string& section)
{
	string tmp;
	i_cfg.getValue(key, section, tmp);
	return tmp;
}

ImisDBIO::ImisDBIO(const ImisDBIO& in)
         : info(in.info), env(in.env), conn(in.conn), stmt(in.stmt),
           oracleDB(in.oracleDB), oracleUser(in.oracleUser), oraclePassword(in.oraclePassword) {}

ImisDBIO& ImisDBIO::operator=(const ImisDBIO& in) {
	closeDB();
	env = in.env;
	conn = in.conn;
	stmt = in.stmt;

	return *this;
}

ImisDBIO::~ImisDBIO()
{ //in case of a throw, this will be called automatically if the exception is later caught
//(remember that now Snowpack wraps its main in a try/catch block)
	closeDB();
}

void ImisDBIO::closeDB() {
	if(stmt!=NULL && conn!=NULL)
		conn->terminateStatement(stmt);
	if(conn!=NULL && env!=NULL)
		env->terminateConnection(conn);
	if(env!=NULL)
		Environment::terminateEnvironment(env); // static OCCI function
}

void ImisDBIO::openDB() {
	//initialize Oracle connection. We will still check that stmt!=NULL later on
	env = Environment::createEnvironment();// static OCCI function
	conn = env->createConnection(oracleUser, oraclePassword, oracleDB);
	stmt = conn->createStatement();
}

/**
 * @brief This routine checks if the specified snow cover data exists
 * @param i_snowfile file containing the initial state of the snowpack
 * @param stationID
 * @return true if the file exists
 */
bool ImisDBIO::snowCoverExists(const std::string& /*i_snowfile*/, const std::string& /*stationID*/) const
{
	throw IOException("Nothing implemented here!", AT);
}

void ImisDBIO::readSnowCover(const std::string& /*i_snowfile*/, const std::string& /*stationID*/,
                             SN_SNOWSOIL_DATA& /*SSdata*/, ZwischenData& /*Zdata*/)
{
	throw IOException("Nothing implemented here!", AT);
}

void ImisDBIO::writeSnowCover(const mio::Date& /*date*/, const SnowStation& /*Xdata*/,
                              const ZwischenData& /*Zdata*/, const bool& /*forbackup*/)
{
	throw IOException("Nothing implemented here!", AT);
}

void ImisDBIO::writeTimeSeries(const SnowStation& /*Xdata*/, const SurfaceFluxes& /*Sdata*/,
                               const CurrentMeteo& /*Mdata*/, const ProcessDat& /*Hdata*/,
                               const double /*wind_trans24*/)
{
	throw IOException("Nothing implemented here!", AT);
}

//delete the profile records that we will resubmit
void ImisDBIO::deleteProfile(const std::string& stationName, const unsigned char& stationNumber,
                             const mio::Date& dateStart, const mio::Date& dateEnd)
{
	stmt->setSQL(sqlDeleteProfile);
	stmt->setAutoCommit(true);

	stmt->setString(1, stationName);   // set 1st variable's value (station name)
	stmt->setUInt(2, static_cast<unsigned int>(stationNumber)); // set 2nd variable's value (station number)
	stmt->setDate(3, OracleDate(dateStart));       // set 4rd variable's value (begin date)
	stmt->setDate(4, OracleDate(dateEnd));         // set 5th variable's value (enddate)

	try {
		stmt->executeUpdate();
	}  catch (const exception& e) {
		cerr << "[E] SDB for station " << stationName << stationNumber << " from " << dateStart.toString(mio::Date::ISO) << " to " << dateEnd.toString(mio::Date::ISO);
		cerr << "\tError deleting previous profile data\n";
		throw; //rethrow the exception
	}
}

void ImisDBIO::insertProfile(const std::vector<SnowProfileLayer> &Pdata)
{
	if(Pdata.empty())
		return;

	const occi::Date profDate( OracleDate( Pdata[0].profileDate ) );
	const occi::Date calcDate( OracleDate(info.computation_date) );
	const std::string stat_abk( Pdata[0].stationname );
	const unsigned char stao_nr = Pdata[0].loc_for_snow;
	const double version = atof( info.version.c_str() );

	//check that the station can really be an IMIS station
	if(stat_abk.size()>4 || stat_abk.find_first_of("0123456789")!=string::npos)
		throw IOException("Is station \""+stat_abk+"\" really an IMIS station?", AT);

	stmt->setSQL( sqlInsertProfile );
	stmt->setAutoCommit(false);

	const size_t nL = Pdata.size();
	for(size_t ii=0; ii<nL; ++ii) {
		stmt->setDate(1, profDate);
		stmt->setString(2, stat_abk);
		stmt->setUInt(3, static_cast<unsigned int>(stao_nr));
		stmt->setNumber(4, Pdata[ii].height);

		stmt->setDate(5, OracleDate(Pdata[ii].depositionDate));
		stmt->setNumber(6, Pdata[ii].rho);
		stmt->setNumber(7, Pdata[ii].T);
		stmt->setNumber(8, Pdata[ii].gradT);
		stmt->setNumber(9, Pdata[ii].v_strain_rate);
		stmt->setInt(10, static_cast<int>( mio::Optim::round(100.*Pdata[ii].theta_w)) );
		stmt->setInt(11, static_cast<int>( mio::Optim::round(100.*Pdata[ii].theta_i)) );
		stmt->setNumber(12, Pdata[ii].dendricity);
		stmt->setNumber(13, Pdata[ii].sphericity);
		stmt->setNumber(14, Pdata[ii].coordin_num);
		stmt->setNumber(15, Pdata[ii].grain_size);
		stmt->setNumber(16, Pdata[ii].bond_size);
		stmt->setUInt(17, Pdata[ii].type);
		stmt->setNumber(18, version);
		stmt->setDate(19, calcDate);

		try {
			stmt->executeUpdate(); // execute the statement stmt
		} catch (const exception& e) {
			cerr << "[E] SDB profile for station " << stat_abk << stao_nr << " at " << Pdata[0].profileDate.toString(mio::Date::ISO);
			cerr << "\tsnowpack_version: " << fixed << setw(12) << setprecision(3) << info.version << "\tcalculation_date: " << Pdata[ii].depositionDate.toString(mio::Date::ISO);
			print_Profile_query(Pdata[ii]);
			throw; //rethrow the exception
		}

		if((ii == (nL-1)) || (ii>0 && (ii%500 == 0))) { //commit every 500 lines as well at the last line
			try {
				(stmt->getConnection())->commit();
			} catch (const exception& e) {
				cerr << "[E] SDB profile for station " << stat_abk << stao_nr << " at " << Pdata[0].profileDate.toString(mio::Date::ISO);
				cerr << "\tsnowpack_version: " << fixed << setw(12) << setprecision(3) << info.version << "\tcalculation_date: " << Pdata[ii].depositionDate.toString(mio::Date::ISO);
				throw; //rethrow the exception
			}
		}
	}
}

/**
 * @brief Dump aggregated profile to database
 */
void ImisDBIO::writeProfile(const mio::Date& dateOfProfile, const SnowStation& Xdata)
{
	const size_t nE = Xdata.getNumberOfElements();
	if ((Xdata.sector != 0) || (nE == 0)) {
		return;
	}
	std::vector<SnowProfileLayer> Pdata( SnowProfileLayer::generateProfile(dateOfProfile, Xdata, hoar_density_surf, hoar_min_size_surf) );
	Aggregate::aggregate(Pdata);

	try {
		deleteProfile(Pdata[0].stationname, Pdata[0].loc_for_snow, dateOfProfile, dateOfProfile);
		insertProfile(Pdata);
	} catch (const exception& e){
		prn_msg(__FILE__, __LINE__, "err", mio::Date(), ":");
		prn_msg(__FILE__, __LINE__, "msg", mio::Date(), "while writing profile data for %s%d to %s,",
		        Pdata[0].stationname.c_str(), Pdata[0].loc_for_snow, oracleDB.c_str());
		prn_msg(__FILE__, __LINE__, "msg-", mio::Date(), "at %s",
		        dateOfProfile.toString(mio::Date::ISO).c_str());
		throw IOException("Oracle Error: " + string(e.what()), AT); //Translation of OCCI exception to IOException
	}
}

bool ImisDBIO::writeHazardData(const std::string& stationID, const std::vector<ProcessDat>& Hdata,
                               const std::vector<ProcessInd>& Hdata_ind, const size_t& num)
{
	//num is incremented after each new data is added. It is therefore the index of the next element to write
	//Hdata has been allocated nr_timesteps elements, but it might be that only num<nr_timesteps have been filled
	if ((num == 0) || (num > Hdata.size())){
		prn_msg(__FILE__, __LINE__, "msg", mio::Date(), "%s: No hazard data either deleted from or inserted into %s: %d steps while Hdata.size=%d", stationID.c_str(), oracleDB.c_str(), num, Hdata.size());
		return false; //nothing to do
	}

	std::string stationName, stationNumber;
	parseStationName(stationID, stationName, stationNumber);

	try {
		deleteHdata(stationName, stationNumber, Hdata[0].date, Hdata[num-1].date);
		insertHdata(stationName, stationNumber, Hdata, Hdata_ind, num);

	} catch (const exception& e){
		prn_msg(__FILE__, __LINE__, "err", mio::Date(), ":");
		prn_msg(__FILE__, __LINE__, "msg", mio::Date(), "while writing hazard data for %s to %s from %s to %s",
		        stationID.c_str(), oracleDB.c_str(), Hdata[0].date.toString(mio::Date::ISO).c_str(), Hdata[num-1].date.toString(mio::Date::ISO).c_str());
		throw IOException("Oracle Error: " + string(e.what()), AT); //Translation of OCCI exception to IOException
	}
	return true;
}

/**
 * @brief This function breaks up the station name into two components (a string and a number e.g. KLO2 -> "KLO","2")
 * @param stationName The full name of the station (e.g. "KLO2")
 * @param stName      The string part of the name  (e.g. "KLO")
 * @param stNumber    The integer part of the name (e.g. "2")
 */
void ImisDBIO::parseStationName(const std::string& stationName, std::string& stName, std::string& stNumber)
{
	stName    = stationName.substr(0, stationName.length()-1); //The station name: e.g. KLO
	stNumber  = stationName.substr(stationName.length()-1, 1); //The station number: e.g. 2
	if(!std::isdigit(stNumber[0])) {
		//the station is one of these non-imis stations that don't contain a number...
		stName = stationName;
		stNumber = "0";
	}
}

void ImisDBIO::deleteHdata(const std::string& stationName, const std::string& stationNumber,
                           const mio::Date& dateStart, const mio::Date& dateEnd)
{
	std::vector< std::vector<std::string> > vecResult;

	stmt->setSQL(sqlDeleteHdata);
	stmt->setAutoCommit(true);

	stmt->setString(1, stationName);   // set 1st variable's value (station name)
	stmt->setString(2, stationNumber); // set 2nd variable's value (station number)
	stmt->setDate(3, OracleDate(dateStart));       // set 4rd variable's value (begin date)
	stmt->setDate(4, OracleDate(dateEnd));         // set 5th variable's value (enddate)

	const unsigned int rows_deleted = stmt->executeUpdate();
	prn_msg(__FILE__, __LINE__, "msg-", mio::Date(), "Deleted %d rows in %s!", rows_deleted, oracleDB.c_str());
}

void ImisDBIO::print_Profile_query(const SnowProfileLayer& Pdata) const
{
	const size_t posE = sqlInsertProfile.find_last_of('(');
	cerr << "\n[E] SDB querry       :  " << sqlInsertProfile.substr(0, posE) << "(";
	
	string profileDate(  "to_date('" + Pdata.profileDate.toString(mio::Date::ISO) +  "', 'yyyy-mm-dd hh24:mi:ss')" );
	std::replace( profileDate.begin(), profileDate.end(), 'T', ' ');
	string depositionDate( "to_date('" + Pdata.depositionDate.toString(mio::Date::ISO) +  "', 'yyyy-mm-dd hh24:mi:ss')" );
	std::replace( depositionDate.begin(), depositionDate.end(), 'T', ' ');
	string compDate(  "to_date('" + info.computation_date.toString(mio::Date::ISO) +  "', 'yyyy-mm-dd hh24:mi:ss')" );
	std::replace( compDate.begin(), compDate.end(), 'T', ' ');

	cerr << profileDate << ",'" << Pdata.stationname << "'," << +Pdata.loc_for_snow << ",";
	cerr << setw(12) << setprecision(8) << Pdata.height << "," << depositionDate << ",";
	cerr << Pdata.rho << "," << Pdata.T << "," << Pdata.gradT << ",";
	cerr << Pdata.v_strain_rate << ",";
	cerr << static_cast<int>( mio::Optim::round(100.*Pdata.theta_w)) << "," << static_cast<int>( mio::Optim::round(100.*Pdata.theta_i)) << ",";
	cerr << Pdata.dendricity << "," << Pdata.sphericity << "," << Pdata.coordin_num << ",";
	cerr << Pdata.grain_size << "," << Pdata.bond_size << "," << Pdata.type << ",";
	cerr << info.version << "," << compDate;

	cerr << ")\n";
}

void ImisDBIO::print_Hdata_query(const ProcessDat& Hdata, const ProcessInd& Hdata_ind) const
{
	const size_t posE=sqlInsertHdata.find_last_of('(');
	cerr << "\n[E] SDB querry       :  " << sqlInsertHdata.substr(0, posE) << "(";
	
	string profileDate( "to_date('" + Hdata.date.toString(mio::Date::ISO) +  "', 'yyyy-mm-dd hh24:mi:ss')" );
	std::replace( profileDate.begin(), profileDate.end(), 'T', ' ');
	string compDate(  "to_date('" + info.computation_date.toString(mio::Date::ISO) +  "', 'yyyy-mm-dd hh24:mi:ss')" );
	std::replace( compDate.begin(), compDate.end(), 'T', ' ');
	
	const char stao_nr = (!Hdata.stat_abbrev.empty())? *Hdata.stat_abbrev.rbegin() : ' ';
	const string stat_abk = (isdigit(stao_nr))? Hdata.stat_abbrev.substr(0, Hdata.stat_abbrev.size()-1) : Hdata.stat_abbrev;
	
	cerr << profileDate << ",'" << stat_abk << "'," << stao_nr << ",";
	cerr << setw(12) << setprecision(8);
	if (Hdata_ind.dewpt_def) cerr << Hdata.dewpt_def << ",";
	else cerr << "NULL,";
	if (Hdata_ind.hoar_ind6) cerr << Hdata.hoar_ind6 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.hoar_ind24) cerr << Hdata.hoar_ind24 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.wind_trans) cerr << Hdata.wind_trans << ",";
	else cerr << "NULL,";

	cerr << "\t";
	if (Hdata_ind.hn3)       cerr << Hdata.hn3 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.hn6)       cerr << Hdata.hn6 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.hn12)      cerr << Hdata.hn12 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.hn24)      cerr << Hdata.hn24 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.hn72)      cerr << Hdata.hn72 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.hn72_24)   cerr << Hdata.hn72_24 << ",";
	else cerr << "NULL,";
	cerr << "\t";
	if (Hdata_ind.psum3)        cerr << Hdata.psum3 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.psum6)        cerr << Hdata.psum6 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.psum12)       cerr << Hdata.psum12 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.psum24)       cerr << Hdata.psum24 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.psum72)       cerr << Hdata.psum72 << ",";
	else cerr << "NULL,";

	cerr << "\t";
	if (Hdata_ind.hoar_size)  cerr << Hdata.hoar_size << ",";
	else cerr << "NULL,";
	if (Hdata_ind.wind_trans24) cerr << Hdata.wind_trans24 << ",";
	else cerr << "NULL,";

	cerr << "\t";
	if (Hdata_ind.stab_class1)  cerr << Hdata.stab_class1 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.stab_class2)  cerr << Hdata.stab_class2 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.stab_index1)  cerr << Hdata.stab_index1 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.stab_height1) cerr << Hdata.stab_height1 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.stab_index2)  cerr << Hdata.stab_index2 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.stab_height2) cerr << Hdata.stab_height2 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.stab_index3)  cerr << Hdata.stab_index3 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.stab_height3) cerr << Hdata.stab_height3 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.stab_index4)  cerr << Hdata.stab_index4 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.stab_height4) cerr << Hdata.stab_height4 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.stab_index5)  cerr << Hdata.stab_index5 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.stab_height5) cerr << Hdata.stab_height5 << ",";
	else cerr << "NULL,";

	cerr << "\t";
	if (Hdata_ind.ch)     cerr << Hdata.ch << ",";
	else cerr << "NULL,";
	if (Hdata_ind.crust)  cerr << Hdata.crust << ",";
	else cerr << "NULL,";
	if (Hdata_ind.en_bal) cerr << Hdata.en_bal << ",";
	else cerr << "NULL,";
	if (Hdata_ind.sw_net)  cerr << Hdata.sw_net << ",";
	else cerr << "NULL,";
	cerr << "\t";
	if (Hdata_ind.t_top1)  cerr << Hdata.t_top1 << ",";
	else cerr << "NULL,";
	if (Hdata_ind.t_top2)  cerr << Hdata.t_top2 << "";
	else cerr << "NULL,";
	cerr << "\t";
	cerr << info.version << "," << compDate << ",";
	
	if (Hdata_ind.swe)      cerr << Hdata.swe << ",";
	else cerr << "NULL,";
	if (Hdata_ind.tot_lwc)  cerr << Hdata.tot_lwc << ",";
	else cerr << "NULL,";
	if (Hdata_ind.runoff)   cerr << Hdata.runoff << ",";
	else cerr << "NULL";
	if (Hdata_ind.lwi_N)   cerr << Hdata.lwi_N << ",";
	else cerr << "NULL";
	if (Hdata_ind.lwi_S)   cerr << Hdata.lwi_S << "";
	else cerr << "NULL";

	cerr << ")\n";
}

void ImisDBIO::insertHdata(const std::string& stationName, const std::string& stationNumber,
                           const std::vector<ProcessDat>& Hdata, const std::vector<ProcessInd>& Hdata_ind,
                           const size_t& num)
{
	unsigned int rows_inserted = 0;
	const double version = atof( info.version.c_str() );
	int statNum = 0;
	IOUtils::convertString(statNum, stationNumber);
	const occi::Date computationdate( OracleDate( info.computation_date ) );

	stmt->setSQL( sqlInsertHdata );
	stmt->setAutoCommit(false);

	for (size_t i = 0; i<num; ++i){ //loop over the available timesteps
		if (Hdata[i].date == mio::Date()) break; //catch the case that not all Hdata has been set properly

		unsigned int param = 1;

		stmt->setDate(param++, OracleDate(Hdata[i].date));
		stmt->setString(param++, stationName);
		stmt->setNumber(param++, statNum);

		if (Hdata_ind[i].dewpt_def)  stmt->setNumber(param++, Hdata[i].dewpt_def);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].hoar_ind6)	stmt->setNumber(param++, Hdata[i].hoar_ind6);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].hoar_ind24) stmt->setNumber(param++, Hdata[i].hoar_ind24);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].wind_trans) stmt->setNumber(param++, Hdata[i].wind_trans);
		else stmt->setNull(param++, occi::OCCINUMBER);

		if (Hdata_ind[i].hn3)       stmt->setNumber(param++, Hdata[i].hn3);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].hn6)       stmt->setNumber(param++, Hdata[i].hn6);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].hn12)      stmt->setNumber(param++, Hdata[i].hn12);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].hn24)      stmt->setNumber(param++, Hdata[i].hn24);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].hn72)      stmt->setNumber(param++, Hdata[i].hn72);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].hn72_24)   stmt->setNumber(param++, Hdata[i].hn72_24);
		else stmt->setNull(param++, occi::OCCINUMBER);

		if (Hdata_ind[i].psum3)        stmt->setNumber(param++, Hdata[i].psum3);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].psum6)        stmt->setNumber(param++, Hdata[i].psum6);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].psum12)       stmt->setNumber(param++, Hdata[i].psum12);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].psum24)       stmt->setNumber(param++, Hdata[i].psum24);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].psum72)       stmt->setNumber(param++, Hdata[i].psum72);
		else stmt->setNull(param++, occi::OCCINUMBER);

		if (Hdata_ind[i].hoar_size)  stmt->setNumber(param++, Hdata[i].hoar_size);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].wind_trans24) stmt->setNumber(param++, Hdata[i].wind_trans24);
		else stmt->setNull(param++, occi::OCCINUMBER);

		if (Hdata_ind[i].stab_class1)  stmt->setNumber(param++, Hdata[i].stab_class1);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].stab_class2)  stmt->setNumber(param++, Hdata[i].stab_class2);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].stab_index1)  stmt->setNumber(param++, Hdata[i].stab_index1);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].stab_height1) stmt->setNumber(param++, Hdata[i].stab_height1);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].stab_index2)  stmt->setNumber(param++, Hdata[i].stab_index2);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].stab_height2) stmt->setNumber(param++, Hdata[i].stab_height2);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].stab_index3)  stmt->setNumber(param++, Hdata[i].stab_index3);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].stab_height3) stmt->setNumber(param++, Hdata[i].stab_height3);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].stab_index4)  stmt->setNumber(param++, Hdata[i].stab_index4);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].stab_height4) stmt->setNumber(param++, Hdata[i].stab_height4);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].stab_index5)  stmt->setNumber(param++, Hdata[i].stab_index5);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].stab_height5) stmt->setNumber(param++, Hdata[i].stab_height5);
		else stmt->setNull(param++, occi::OCCINUMBER);

		if (Hdata_ind[i].ch)     stmt->setNumber(param++, Hdata[i].ch);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].crust)  stmt->setNumber(param++, Hdata[i].crust);
		else stmt->setNull(param++, occi::OCCINUMBER);

		if (Hdata_ind[i].en_bal) stmt->setNumber(param++, Hdata[i].en_bal);
		else stmt->setNull(param++, occi::OCCINUMBER);

		if (Hdata_ind[i].sw_net)  stmt->setNumber(param++, Hdata[i].sw_net);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].t_top1)  stmt->setNumber(param++, Hdata[i].t_top1);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].t_top2)  stmt->setNumber(param++, Hdata[i].t_top2);
		else stmt->setNull(param++, occi::OCCINUMBER);

		stmt->setNumber(param++, version);
		stmt->setDate(param++, computationdate);

		if (Hdata_ind[i].swe)      stmt->setNumber(param++, Hdata[i].swe);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].tot_lwc)  stmt->setNumber(param++, Hdata[i].tot_lwc);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].runoff)   stmt->setNumber(param++, Hdata[i].runoff);
		else stmt->setNull(param++, occi::OCCINUMBER);

		if (Hdata_ind[i].lwi_N)  stmt->setNumber(param++, Hdata[i].lwi_N);
		else stmt->setNull(param++, occi::OCCINUMBER);
		if (Hdata_ind[i].lwi_S)  stmt->setNumber(param++, Hdata[i].lwi_S);
		else stmt->setNull(param++, occi::OCCINUMBER);

		try {
			rows_inserted += stmt->executeUpdate(); // execute the statement stmt
		} catch (const exception& e) {
			cerr << "[E] SDB for station " << stationName << statNum << " at " << Hdata[i].date.toString(mio::Date::ISO);
			cerr << "\tsnowpack_version: " << fixed << setw(12) << setprecision(3) << info.version << "\tcalculation_date: " << info.computation_date.toString(mio::Date::ISO);
			print_Hdata_query(Hdata[i], Hdata_ind[i]);
			throw; //rethrow the exception
		}

		if((i == (num-1)) || (i>0 && (i%500 == 0))) { //commit every 500 lines as well at the last line
			try {
				(stmt->getConnection())->commit();
			} catch (const exception& e) {
				cerr << "[E] Commit to SDB failed for station " << stationName << statNum << " after " << Hdata[i].date.toString(mio::Date::ISO);
				cerr << "\tsnowpack_version: " << fixed << setw(12) << setprecision(3) << info.version << "\tcalculation_date: " << info.computation_date.toString(mio::Date::ISO);
				throw; //rethrow the exception
			}
		}
	}
	prn_msg(__FILE__, __LINE__, "msg-", mio::Date(), "Inserted %d rows into %s!", rows_inserted, oracleDB.c_str());
}

oracle::occi::Date ImisDBIO::OracleDate(mio::Date in_date) const
{
	std::vector<int> date( 5 );
	in_date.setTimeZone(time_zone);
	in_date.getDate(date[0], date[1], date[2], date[3], date[4]);

	//Oracle can't deal with an integer for the hour of 24, hence the following workaround
	if(date[3] == 24) {
		const mio::Date tmpDate = in_date + 3.0/(60*60*24); //add three seconds to omit 24 for 00
		tmpDate.getDate(date[0], date[1], date[2], date[3], date[4]);
	}

	const occi::Date occi_date(env, date[0], date[1], date[2], date[3], date[4]);
	return occi_date;
}
