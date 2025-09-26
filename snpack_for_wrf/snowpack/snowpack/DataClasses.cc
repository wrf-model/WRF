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
/**
 * @file DataClasses.cc
 * @version 11.03
 * @brief This module contains the definitions of data classes
 */

#include <snowpack/DataClasses.h>
#include <snowpack/Utils.h>
#include <snowpack/snowpackCore/Canopy.h>
#include <snowpack/snowpackCore/Metamorphism.h>
#include <snowpack/snowpackCore/Solver.h>
#include <snowpack/Laws_sn.h>
#include <snowpack/snowpackCore/Aggregate.h>

#include <cstdio>
#include <fstream>
#include <sstream>
#include <assert.h>

using namespace mio;
using namespace std;

/// Number of top elements left untouched by the join functions
const size_t SnowStation::number_top_elements = 5;
unsigned short SnowStation::number_of_solutes = 0;

/// Snow elements with a LWC above this threshold are considered at least to be moist
const double SnowStation::thresh_moist_snow = 0.003;
const double SnowStation::thresh_moist_soil = 0.0001;

/// The default ratio between height_new_elem and comb_thresh_l, in case comb_thresh_l is not explicitly defined.
const double SnowStation::comb_thresh_l_ratio = 0.75;

/// Volumetric ice content (1), i.e., about 46 kg m-3
const double SnowStation::comb_thresh_ice = 0.05;
const double SnowStation::comb_thresh_water = 0.01; ///< Water content (1)
const double SnowStation::comb_thresh_dd = 0.2;     ///< Dendricity (1)
const double SnowStation::comb_thresh_sp = 0.05;    ///< Sphericity (1)
const double SnowStation::comb_thresh_rg = 0.125;   ///< Grain radius (mm)

RunInfo::RunInfo()
            : version(SN_VERSION), computation_date(getRunDate()),
              compilation_date(getCompilationDate()), user(IOUtils::getLogName()) {}

RunInfo::RunInfo(const RunInfo& orig)
            : version(orig.version), computation_date(orig.computation_date),
              compilation_date(orig.compilation_date), user(orig.user) {}

mio::Date RunInfo::getRunDate()
{
	Date localdate;
	localdate.setFromSys();
	return localdate;
}

std::string RunInfo::getCompilationDate()
{
	std::stringstream ss;
	ss << __DATE__ << ", " << __TIME__;
	return ss.str();
}

void ZwischenData::reset()
{
	hoar24.resize(48, 0.0);
	drift24.resize(48, 0.0);
	hn3.resize(144, 0.0);
	hn24.resize(144, 0.0);
}

std::ostream& operator<<(std::ostream& os, const ZwischenData& data)
{
	const size_t s_hoar24 = data.hoar24.size();
	os.write(reinterpret_cast<const char*>(&s_hoar24), sizeof(size_t));
	os.write(reinterpret_cast<const char*>(&data.hoar24[0]), static_cast<streamsize>(s_hoar24*sizeof(data.hoar24[0])));
	os.write(reinterpret_cast<const char*>(&data.drift24[0]), static_cast<streamsize>(s_hoar24*sizeof(data.drift24[0])));

	const size_t s_hn3 = data.hn3.size();
	os.write(reinterpret_cast<const char*>(&s_hn3), sizeof(size_t));
	os.write(reinterpret_cast<const char*>(&data.hn3[0]), static_cast<streamsize>(s_hn3*sizeof(data.hn3[0])));
	os.write(reinterpret_cast<const char*>(&data.hn24[0]), static_cast<streamsize>(s_hn3*sizeof(data.hn24[0])));
	return os;
}

std::istream& operator>>(std::istream& is, ZwischenData& data)
{
	size_t s_hoar24, s_hn3;
	is.read(reinterpret_cast<char*>(&s_hoar24), sizeof(size_t));
	data.hoar24.resize(s_hoar24);
	data.drift24.resize(s_hoar24);
	is.read(reinterpret_cast<char*>(&data.hoar24[0]), static_cast<streamsize>(s_hoar24*sizeof(data.hoar24[0])));
	is.read(reinterpret_cast<char*>(&data.drift24[0]), static_cast<streamsize>(s_hoar24*sizeof(data.drift24[0])));

	is.read(reinterpret_cast<char*>(&s_hn3), sizeof(size_t));
	data.hn3.resize(s_hn3);
	data.hn24.resize(s_hn3);
	is.read(reinterpret_cast<char*>(&data.hn3[0]), static_cast<streamsize>(s_hn3*sizeof(data.hn3[0])));
	is.read(reinterpret_cast<char*>(&data.hn24[0]), static_cast<streamsize>(s_hn3*sizeof(data.hn24[0])));
	return is;
}

SnowProfileLayer::SnowProfileLayer()
                  : profileDate(), stationname(), loc_for_snow(0), loc_for_wind(0),
                    depositionDate(), height(0.), rho(0.), T(0.), gradT(0.), v_strain_rate(0.),
                    theta_i(0.), theta_w(0.), theta_a(0.),
                    grain_size(0.), bond_size(0.), dendricity(0.), sphericity(0.), ogs(0.),
                    coordin_num(0.), marker(0), type(0), hard(IOUtils::nodata) {}

/**
 * @brief Generates a snow profile layer from element and upper node data
 * @param dateOfProfile
 * @param Edata
 * @param Ndata
 */
void SnowProfileLayer::generateLayer(const ElementData& Edata, const NodeData& Ndata)
{
	depositionDate = Edata.depositionDate;
	height = M_TO_CM(Ndata.z + Ndata.u);
	T = IOUtils::K_TO_C(Ndata.T);
	gradT = Edata.gradT;
	rho = Edata.Rho;
	theta_i = Edata.theta[ICE];
	theta_w = Edata.theta[WATER];
	theta_a = Edata.theta[AIR];
	grain_size = 2. * Edata.rg;
	bond_size = 2. * Edata.rb;
	dendricity = Edata.dd;
	sphericity = Edata.sp;
	ogs = Edata.ogs; // in mm
	coordin_num = Edata.N3;
	marker = static_cast<unsigned short int>( Edata.mk%100 );
	type = Edata.type;
	v_strain_rate = fabs(Edata.Eps_vDot);
	hard = Edata.hard;
}

/**
 * @brief Generates a surface hoar layer from top element and node data
 * @param dateOfProfile
 * @param Edata
 * @param Ndata
 */
void SnowProfileLayer::generateLayer(const ElementData& Edata, const NodeData& Ndata, const mio::Date& dateOfProfile, const double hoar_density_surf)
{
	const double hoar_size = Ndata.hoar/hoar_density_surf; // (m)

	depositionDate = dateOfProfile;
	height = M_TO_CM(Ndata.z + Ndata.u) + M_TO_CM(hoar_size);
	rho = hoar_density_surf;
	T = IOUtils::K_TO_C(Ndata.T + (2./3.)*hoar_size*Edata.gradT);
	gradT = Edata.gradT;
	v_strain_rate = 0.;
	theta_i = hoar_density_surf/Constants::density_ice;
	theta_w = 0.;
	theta_a = 1. - theta_i;
	grain_size = M_TO_MM(hoar_size);
	bond_size = grain_size/3.;
	dendricity = 0.;
	sphericity = 0.;
	ogs = std::min(4.e-1, grain_size); // in mm, see opticalEquivalentGrainSize();
	coordin_num = 2.;
	marker = 3;
	type = 660;
	hard = 1;
}

/**
 * @brief Generates a snow profile from snow station data (1 element = 1 layer)
 * @param dateOfProfile
 * @param Xdata
 * @param hoar_density_surf
 * @param hoar_min_size_surf
 */
std::vector<SnowProfileLayer> SnowProfileLayer::generateProfile(const mio::Date& dateOfProfile, const SnowStation& Xdata, const double hoar_density_surf, const double hoar_min_size_surf)
{
	const size_t nE = Xdata.getNumberOfElements();
	const vector<NodeData>& NDS = Xdata.Ndata;
	const vector<ElementData>& EMS = Xdata.Edata;
	const double cos_sl = Xdata.cos_sl;
	const bool surf_hoar = (NDS[nE].hoar > (hoar_density_surf * MM_TO_M(hoar_min_size_surf)));

	// Generate the profile data from the element data (1 layer = 1 element)
	unsigned char snowloc = 0;
	string mystation = Xdata.meta.getStationID();
	if (isdigit(mystation[mystation.length()-1])) {
		snowloc = static_cast<unsigned char>( mystation[mystation.length()-1] - '0' ); //trick to convert the number as char to a number
		if (mystation.length() > 2)
			mystation = mystation.substr(0, mystation.length()-1);
	}

	const size_t nL = surf_hoar? (nE+1 - Xdata.SoilNode) : (nE - Xdata.SoilNode);
	std::vector<SnowProfileLayer> Pdata(nL);

	for(size_t ll=0, e=Xdata.SoilNode; ll<nL; ll++, e++) { // We dump only snow layers
		// Write profile meta data
		Pdata[ll].profileDate = dateOfProfile;
		Pdata[ll].stationname = mystation;
		Pdata[ll].loc_for_snow = snowloc;
		Pdata[ll].loc_for_wind = 1;

		// Write snow layer data
		if (ll < nE) {
			Pdata[ll].generateLayer(EMS[e], NDS[e+1]);
		} else { // add a SH layer
			Pdata[ll].generateLayer(EMS[nE-1], NDS[nE], dateOfProfile, hoar_density_surf);
		}
		Pdata[ll].height = (Pdata[ll].height - Xdata.Ground)/cos_sl;
	}

	return Pdata;
}

/**
 * @brief Determines the averaged quantities of the current layer with another layer
 * @param Lp1 Thickness (weight) of layer Pdata
 * @param Lp0 Thickness (weight) of current layer
 * @param profile_layer to average with
 */
void SnowProfileLayer::average(const double& Lp0, const double& Lp1, const SnowProfileLayer& profile_layer)
{
	const double layerThickness = Lp0 + Lp1;

	height += Lp1;
	if (Lp1 > Lp0) {
		depositionDate = profile_layer.depositionDate;
	}
	rho         = (Lp1*profile_layer.rho + Lp0*rho) / layerThickness;
	T           = profile_layer.T;
	gradT       = (Lp1*profile_layer.gradT + Lp0*gradT) / layerThickness;
	v_strain_rate = (Lp1*profile_layer.v_strain_rate + Lp0*v_strain_rate) / layerThickness;
	theta_w     = (Lp1*profile_layer.theta_w + Lp0*theta_w) / layerThickness;
	theta_i     = (Lp1*profile_layer.theta_i + Lp0*theta_i) / layerThickness;
	dendricity  = (Lp1*profile_layer.dendricity + Lp0*dendricity) / layerThickness;
	sphericity  = (Lp1*profile_layer.sphericity + Lp0*sphericity) / layerThickness;
	coordin_num = (Lp1*profile_layer.coordin_num + Lp0*coordin_num) / layerThickness;
	grain_size  = (Lp1*profile_layer.grain_size + Lp0*grain_size) / layerThickness;
	ogs         = (Lp1*profile_layer.ogs + Lp0*ogs) / layerThickness;
	bond_size   = (Lp1*profile_layer.bond_size + Lp0*bond_size) / layerThickness;
	hard        = (Lp1*profile_layer.hard + Lp0*hard) / layerThickness;
	marker      = std::max(profile_layer.marker, marker);
}

const std::string BoundCond::toString() const
{
	std::ostringstream os;
	os << "<BoundCond>\n";
	os << "\tlw_out=" << lw_out << " lw_net=" << lw_net << "\n";
	os << "\tQsensible=" << qs << " Qlatent=" << ql << " Qrain=" << qr << " Qgeo=" << qg << "\n";
	os <<"</BoundCond>\n";
	return os.str();
}

void BoundCond::reset()
{
  lw_out=0;  ///< outgoing longwave radiation
  lw_net=0;  ///< net longwave radiation
  qs=0;      ///< sensible heat
  ql=0;      ///< latent heat
  qr=0;      ///< rain energy
  qg=0;
}



SurfaceFluxes::SurfaceFluxes()
  : lw_in(0.), lw_out(0.), lw_net(0.), qs(0.), ql(0.), hoar(0.), qr(0.), qg(0.), qg0(0.), sw_hor(0.),
    sw_in(0.), sw_out(0.), qw(0.), sw_dir(0.), sw_diff(0.), pAlbedo(0.), mAlbedo(0.), dIntEnergy(0.), dIntEnergySoil(0.), meltFreezeEnergy(0.), meltFreezeEnergySoil(0.),
    meltMass(0.), refreezeMass(0.),
    drift(0.), mass(N_MASS_CHANGES, 0.), load(SnowStation::number_of_solutes), dhs_corr(0.), cRho_hn(Constants::undefined), mRho_hn(Constants::undefined) {}

void SurfaceFluxes::reset(const bool& cumsum_mass)
{
	if (cumsum_mass) { // Do not reset cumulated mass balance
		lw_in   = 0.;
		lw_out  = 0.;
		lw_net  = 0.;
		qs      = 0.;
		ql      = 0.;
		qr      = 0.;
		qg      = 0.;
		qg0     = 0.;
		sw_hor  = 0.;
		sw_in   = 0.;
		sw_out  = 0.;
		qw      = 0.;
		sw_dir  = 0.;
		sw_diff = 0.;
		pAlbedo = 0.;
		mAlbedo = 0.;
		dIntEnergy = 0.;
		dIntEnergySoil = 0.;
		meltFreezeEnergy = 0.;
		meltFreezeEnergySoil = 0.;
		meltMass = 0;
		refreezeMass = 0;
		mass[MS_HNW] = 0.;
		mass[MS_RAIN] = 0.;
	} else {
		*this = SurfaceFluxes(); //reset everything
	}
}

/**
* @brief Compute ground heat flux at soil/snow boundary
* @param Xdata
*/
void SurfaceFluxes::compSnowSoilHeatFlux(const SnowStation& Xdata) {
	if (Xdata.SoilNode > 0) { // with soil
		const ElementData& E_snow = Xdata.Edata[Xdata.SoilNode];
		const ElementData& E_soil = Xdata.Edata[Xdata.SoilNode-1];

		if (Xdata.getNumberOfElements()-1 < Xdata.SoilNode) { // with soil but no snow
			qg0 += -E_soil.k[TEMPERATURE] * E_soil.gradT;
		} else { // with soil & snow
			qg0 += ( ( -E_snow.k[TEMPERATURE] * E_snow.gradT ) + ( -E_soil.k[TEMPERATURE] * E_soil.gradT ) ) / 2.;
			// Take care of energy flow between snow and soil in case of shortwave absorption by the soil:
			qg0 -= E_soil.sw_abs;
		}

	} else if (Xdata.getNumberOfElements() > 0) { // without soil but with snow
		if ((Xdata.getNumberOfElements() < 3) && (Xdata.Edata[0].theta[WATER] >= 0.9 * Xdata.Edata[0].res_wat_cont)) {
			qg0 += 0.;
		} else {
			qg0 += -Xdata.Edata[0].k[TEMPERATURE] * Xdata.Edata[0].gradT;
		}
	} else { // neither soil nor snow
		qg0 = Constants::undefined;
	}
}

/**
 * @brief Assign surface data from SnowStation and BoundCond to SurfaceFluxes.
 * @param Bdata
 * @param Xdata
 * @param Mdata
 */
void SurfaceFluxes::collectSurfaceFluxes(const BoundCond& Bdata,
                                         SnowStation& Xdata, const CurrentMeteo& Mdata)
{
	// 1) Short wave fluxes and Albedo.
	//     Depending on settings (sw_mode) and conditions,
	//     sw_in and sw_out may differ slightly from the original input
	sw_in  += Mdata.iswr;
	sw_out += Mdata.rswr;
	qw     += Mdata.iswr - Mdata.rswr;

	pAlbedo += Xdata.pAlbedo;
	if (Mdata.mAlbedo != Constants::undefined && mAlbedo != Constants::undefined)
		mAlbedo += Mdata.mAlbedo;
	else
		mAlbedo = Constants::undefined;

	// 2) Long wave fluxes.
	lw_out += Bdata.lw_out;
	lw_net += Bdata.lw_net;
	lw_in  += Constants::emissivity_snow*Mdata.ilwr_v; //Atmosphere::blkBody_Radiation(Mdata.ea, Mdata.ta);

	// 3) Turbulent fluxes.
	qs += Bdata.qs;
	//ql += Bdata.ql; //HACK needed because latent heat ql not linearized w/ respect to Tss!!!
	qr += Bdata.qr;

	// 4) Ground heat fluxes
	//    The ground heat flux at soil/snow boundary is computed after compTemperatureProfile
	qg += Bdata.qg;

	// 5) Change of internal energy
	if (Xdata.getNumberOfElements() > Xdata.SoilNode) {
		dIntEnergy += Xdata.dIntEnergy;
		meltFreezeEnergy += Xdata.meltFreezeEnergy;
	}
	if(Xdata.SoilNode>0) {
		dIntEnergySoil += Xdata.dIntEnergySoil;
		// Now take care of the source and sink terms:
		dIntEnergySoil += (mass[MS_SOIL_RUNOFF] * Constants::specific_heat_water * (Xdata.Edata[0].Te - Constants::meltfreeze_tk));
		if (Xdata.SoilNode <  Xdata.getNumberOfElements()) {
			dIntEnergySoil -= mass[MS_SNOWPACK_RUNOFF] * Constants::specific_heat_water * (Xdata.Edata[Xdata.SoilNode].Te - Constants::meltfreeze_tk);
		}
		if (Xdata.SoilNode == Xdata.getNumberOfElements()) {
			//Note: at this stage, MS_RAIN is still in kg/m^2! In Main.cc, it is recalculated to kg/m^2/h if PRECIP_RATES==TRUE.
			dIntEnergySoil -= (mass[MS_RAIN] + mass[MS_EVAPORATION] + mass[MS_SUBLIMATION]) * Constants::specific_heat_water * (Xdata.Edata[Xdata.SoilNode-1].Te - Constants::meltfreeze_tk);
		}
		meltFreezeEnergySoil += Xdata.meltFreezeEnergySoil;
	}

	// 6) Collect total masses of snowpack
	mass[MS_TOTALMASS] = mass[MS_SWE] = mass[MS_WATER] = 0.;
	Xdata.compSnowpackMasses();
	mass[MS_TOTALMASS] = Xdata.mass_sum;
	mass[MS_SWE] = Xdata.swe;
	mass[MS_WATER] = Xdata.lwc_sum;
	if (Xdata.Seaice != NULL) {
		mass[MS_FLOODING] += Xdata.Seaice->TotalFloodingBucket;
		Xdata.Seaice->TotalFloodingBucket = 0.;
	} else {
		mass[MS_FLOODING] = Constants::undefined;
	}

	// 7) Melt and refreeze mass
	meltMass += Xdata.meltMassTot;
	refreezeMass += Xdata.refreezeMassTot;
}

/**
 * @brief If multiple surface fluxes have been summed over multiple time steps, the
 * fluxes then need to be averaged by the number of steps. The albedos are also
 * averaged in this method.
 * @param factor Averaging factor (for example, 1/Nsteps)
 */
void SurfaceFluxes::multiplyFluxes(const double& factor)
{
	lw_in *= factor;
	lw_out *= factor;
	lw_net *= factor;
	qs *= factor;
	ql *= factor;
	qr *= factor;
	qg *= factor;
	qg0 *= factor;
	sw_hor *= factor;
	sw_in *= factor;
	sw_out *= factor;
	qw *= factor;
	sw_dir *= factor;
	sw_diff *= factor;
	if (pAlbedo != Constants::undefined)
		pAlbedo *= factor;
	if (mAlbedo != Constants::undefined)
		mAlbedo *= factor;
}

std::ostream& operator<<(std::ostream& os, const SurfaceFluxes& data)
{
	os.write(reinterpret_cast<const char*>(&data.lw_in), sizeof(data.lw_in));
	os.write(reinterpret_cast<const char*>(&data.lw_out), sizeof(data.lw_out));
	os.write(reinterpret_cast<const char*>(&data.lw_net), sizeof(data.lw_net));
	os.write(reinterpret_cast<const char*>(&data.qs), sizeof(data.qs));
	os.write(reinterpret_cast<const char*>(&data.ql), sizeof(data.ql));
	os.write(reinterpret_cast<const char*>(&data.hoar), sizeof(data.hoar));
	os.write(reinterpret_cast<const char*>(&data.qr), sizeof(data.qr));
	os.write(reinterpret_cast<const char*>(&data.qg), sizeof(data.qg));
	os.write(reinterpret_cast<const char*>(&data.qg0), sizeof(data.qg0));
	os.write(reinterpret_cast<const char*>(&data.sw_hor), sizeof(data.sw_hor));
	os.write(reinterpret_cast<const char*>(&data.sw_in), sizeof(data.sw_in));
	os.write(reinterpret_cast<const char*>(&data.sw_out), sizeof(data.sw_out));
	os.write(reinterpret_cast<const char*>(&data.qw), sizeof(data.qw));
	os.write(reinterpret_cast<const char*>(&data.sw_dir), sizeof(data.sw_dir));
	os.write(reinterpret_cast<const char*>(&data.sw_diff), sizeof(data.sw_diff));
	os.write(reinterpret_cast<const char*>(&data.pAlbedo), sizeof(data.pAlbedo));
	os.write(reinterpret_cast<const char*>(&data.mAlbedo), sizeof(data.mAlbedo));
	os.write(reinterpret_cast<const char*>(&data.dIntEnergy), sizeof(data.dIntEnergy));
	os.write(reinterpret_cast<const char*>(&data.dIntEnergySoil), sizeof(data.dIntEnergySoil));
	os.write(reinterpret_cast<const char*>(&data.meltFreezeEnergy), sizeof(data.meltFreezeEnergy));
	os.write(reinterpret_cast<const char*>(&data.meltFreezeEnergySoil), sizeof(data.meltFreezeEnergySoil));
	os.write(reinterpret_cast<const char*>(&data.meltMass), sizeof(data.meltMass));
	os.write(reinterpret_cast<const char*>(&data.refreezeMass), sizeof(data.refreezeMass));

	os.write(reinterpret_cast<const char*>(&data.drift), sizeof(data.drift));

	const size_t s_mass = data.mass.size();
	os.write(reinterpret_cast<const char*>(&s_mass), sizeof(size_t));
	os.write(reinterpret_cast<const char*>(&data.mass[0]), static_cast<streamsize>(s_mass*sizeof(data.mass[0])));

	const size_t s_load = data.load.size();
	os.write(reinterpret_cast<const char*>(&s_load), sizeof(size_t));
	os.write(reinterpret_cast<const char*>(&data.load[0]), static_cast<streamsize>(s_load*sizeof(data.load[0])));

	os.write(reinterpret_cast<const char*>(&data.dhs_corr), sizeof(data.dhs_corr));
	os.write(reinterpret_cast<const char*>(&data.cRho_hn), sizeof(data.cRho_hn));
	os.write(reinterpret_cast<const char*>(&data.mRho_hn), sizeof(data.mRho_hn));
	return os;
}

std::istream& operator>>(std::istream& is, SurfaceFluxes& data)
{
	is.read(reinterpret_cast<char*>(&data.lw_in), sizeof(data.lw_in));
	is.read(reinterpret_cast<char*>(&data.lw_out), sizeof(data.lw_out));
	is.read(reinterpret_cast<char*>(&data.lw_net), sizeof(data.lw_net));
	is.read(reinterpret_cast<char*>(&data.qs), sizeof(data.qs));
	is.read(reinterpret_cast<char*>(&data.ql), sizeof(data.ql));
	is.read(reinterpret_cast<char*>(&data.hoar), sizeof(data.hoar));
	is.read(reinterpret_cast<char*>(&data.qr), sizeof(data.qr));
	is.read(reinterpret_cast<char*>(&data.qg), sizeof(data.qg));
	is.read(reinterpret_cast<char*>(&data.qg0), sizeof(data.qg0));
	is.read(reinterpret_cast<char*>(&data.sw_hor), sizeof(data.sw_hor));
	is.read(reinterpret_cast<char*>(&data.sw_in), sizeof(data.sw_in));
	is.read(reinterpret_cast<char*>(&data.sw_out), sizeof(data.sw_out));
	is.read(reinterpret_cast<char*>(&data.qw), sizeof(data.qw));
	is.read(reinterpret_cast<char*>(&data.sw_dir), sizeof(data.sw_dir));
	is.read(reinterpret_cast<char*>(&data.sw_diff), sizeof(data.sw_diff));
	is.read(reinterpret_cast<char*>(&data.pAlbedo), sizeof(data.pAlbedo));
	is.read(reinterpret_cast<char*>(&data.mAlbedo), sizeof(data.mAlbedo));
	is.read(reinterpret_cast<char*>(&data.dIntEnergy), sizeof(data.dIntEnergy));
	is.read(reinterpret_cast<char*>(&data.dIntEnergySoil), sizeof(data.dIntEnergySoil));
	is.read(reinterpret_cast<char*>(&data.meltFreezeEnergy), sizeof(data.meltFreezeEnergy));
	is.read(reinterpret_cast<char*>(&data.meltFreezeEnergySoil), sizeof(data.meltFreezeEnergySoil));
	is.read(reinterpret_cast<char*>(&data.meltMass), sizeof(data.meltMass));
	is.read(reinterpret_cast<char*>(&data.refreezeMass), sizeof(data.refreezeMass));

	is.read(reinterpret_cast<char*>(&data.drift), sizeof(data.drift));

	size_t s_mass;
	is.read(reinterpret_cast<char*>(&s_mass), sizeof(size_t));
	data.mass.resize(s_mass);
	is.read(reinterpret_cast<char*>(&data.mass[0]), static_cast<streamsize>(s_mass*sizeof(data.mass[0])));

	size_t s_load;
	is.read(reinterpret_cast<char*>(&s_load), sizeof(size_t));
	data.load.resize(s_load);
	is.read(reinterpret_cast<char*>(&data.load[0]), static_cast<streamsize>(s_load*sizeof(data.load[0])));

	is.read(reinterpret_cast<char*>(&data.dhs_corr), sizeof(data.dhs_corr));
	is.read(reinterpret_cast<char*>(&data.cRho_hn), sizeof(data.cRho_hn));
	is.read(reinterpret_cast<char*>(&data.mRho_hn), sizeof(data.mRho_hn));
	return is;
}



/**
 * @brief Initialize all the CData elements value with values by default or
 *with values read from the SNO file.
 *This function is called in SnowStation::initialize, whch is called by XXX in the main().
 * @param snow soil data SN_SNOWSOIL_DATA& SSdata created in the main.
 * @author Adrien Michel
 */

void CanopyData::initialize(const SN_SNOWSOIL_DATA& SSdata, const bool useCanopyModel){

  int_cap_snow = SSdata.Canopy_int_cap_snow; //iMax in Gouttevin,2015
  if(useCanopyModel &&  (int_cap_snow < 0.0 || int_cap_snow == mio::IOUtils::nodata ))
  {
    std::stringstream msg;
    msg << "Value provided for CanopySnowIntCapacity(" << int_cap_snow << ") in soil file is not valid, the default value of 5.9 sill be used.";
    prn_msg(__FILE__, __LINE__, "wrn", Date(),msg.str().c_str());
    int_cap_snow = 5.9;
  }

  /// Specific interception capacity for rain (I_LAI) (mm/LAI)
  int_cap_rain = 0.3;
  /** Coef in interception function, see (Pomeroy et al,1998) where a value of 0.7 was
   * found to be appropriate for hourly time-step, but smaller time steps require smaller
   * values, 0.5 was found reasoanble by using the SnowMIP2 data (2007-12-09)
  */
  interception_timecoef = 0.5;

  /// RADIATION BALANCE
  can_alb_dry = SSdata.Canopy_alb_dry;  // Albedo of dry canopy (calibr: 0.09, Alptal)
  if(useCanopyModel &&  (can_alb_dry < 0.0 || can_alb_dry> 1.0 || can_alb_dry == mio::IOUtils::nodata ))
  {
    std::stringstream msg;
    msg << "Value provided for CanopyAlbedoDry (" << can_alb_dry << ") in soil file is not valid, the default value of 0.11 will be used.";
    prn_msg(__FILE__, __LINE__, "wrn", Date(),msg.str().c_str());
    can_alb_dry = 0.11;
  }

  can_alb_wet = SSdata.Canopy_alb_wet;  // Albedo of wet canopy (calibr: 0.09, Alptal)
  if(useCanopyModel &&  (can_alb_wet < 0.0 || can_alb_wet > 1.0 ||  can_alb_wet == mio::IOUtils::nodata ))
  {
    std::stringstream msg;
    msg << "Value provided for CanopyAlbedoWet (" << can_alb_wet << ") in soil file is not valid, the default value of 0.11 will be used.";
    prn_msg(__FILE__, __LINE__, "wrn", Date(),msg.str().c_str());
    can_alb_wet = 0.11;
  }

  can_alb_snow = SSdata.Canopy_alb_snow;  // Albedo of snow covered albedo (calibr: 0.35, Alptal)
  if(useCanopyModel &&  (can_alb_snow < 0.0 || can_alb_snow > 1.0 || can_alb_snow == mio::IOUtils::nodata ))
  {
    std::stringstream msg;
    msg << "Value provided for CanopyAlbedoSnow (" << can_alb_snow << ") in soil file is not valid, the default value of 0.35 will be used.";
    prn_msg(__FILE__, __LINE__, "wrn", Date(),msg.str().c_str());
    can_alb_snow = 0.35;
  }

  krnt_lai = .75;       // Radiation transmissivity parameter, in the range 0.4-0.8 if the true LAI is used; higher if optical LAI is used.
                                            // (calibrated on Alptal)
  can_diameter = SSdata.Canopy_diameter;  // average canopy (tree) diameter [m], parameter in the new radiation transfer model
  if(useCanopyModel &&  (can_diameter < 0.0 || can_diameter == mio::IOUtils::nodata ))
  {
    std::stringstream msg;
    msg << "Value provided for CanopyDiameter (" << can_diameter << ") in soil file is not valid, the default value of 1.0 will be used.";
    prn_msg(__FILE__, __LINE__, "wrn", Date(),msg.str().c_str());
    can_diameter = 1.0;
  }

  ///  ENERGY BALANCE
  /// parameters for HeatMass and 2layercanopy
  biomass_heat_capacity = 2800.;	// from Linroth et al., 2013 (J Kg-1 K-1)
  biomass_density = 900.;		// from Linroth et al., 2013 (Kg m-3)

  lai_frac_top_default = SSdata.Canopy_lai_frac_top_default;	// fraction of total LAI that is attributed to the uppermost layer. Here calibrated for Alptal.
  if(useCanopyModel &&  (lai_frac_top_default < 0.0 || lai_frac_top_default > 1.0 || lai_frac_top_default == mio::IOUtils::nodata ))
  {
    std::stringstream msg;
    msg << "Value provided for CanopyFracLAIUpperLayer (" << lai_frac_top_default << ") in soil file is not valid, the default value of 0.5 will be used.";
    prn_msg(__FILE__, __LINE__, "wrn", Date(),msg.str().c_str());
    lai_frac_top_default = 0.5;
  }

  trunk_frac_height = 0.2;		// (optional) fraction of total tree height occupied by trunks,
                                    // used to calculate direct solar insolation of trunks.
  trunkalb = 0.09;			// trunk albedo
  et = 1.;				// trunk emissivity
  ///  TURBULENT HEAT EXCHANGE
  /// Stab. corr. aerodyn. resist. above and below canopy: 0=off and 1=on (Monin-Obukhov formulation)
  canopy_stabilitycorrection = true;
  /// Ratio between canopy height and roughness length
  roughmom_to_canopyheight_ratio = 0.10;
  /// As above for displacement height
  displ_to_canopyheight_ratio = 0.6667;
  /**
   * Fractional increase of aerodynamic resistance for evaporation of intercepted snow.
   * - 10.0 from Koivusalo and Kokkonen (2002)
   * - 8.0 calibration with Alptal data
   */
  raincrease_snow = 10.0;

  /// @brief Maximum allowed canopy temperature change (K hr-1)
  canopytemp_maxchange_perhour = 7.0;
  /// @brief (~=1, but Not allowed to be exactly 1)
  roughheat_to_roughmom_ratio = 0.9999;
  /// @brief minimum heat exchange (Wm-2K-1) at zero wind
  can_ch0 = 3.;
  /// @brief 1+CAN_RS_MULT = maximum factor to increase Cdata->rs below canopy
  can_rs_mult = 3.0;
  /// @brief TRANSPIRATION
  /// @brief Minimum canopy surface resistance, 500 (sm-1) is for needle leaf treas van den Hurk et al (2000) *75% Gustafsson et al (2003)
  rsmin = 375.0;
  /**
   * @brief gd (Pa-1) parameter for canopy surface resistance response to vapour pressure:
   * - 0.0003 = trees (needle or broadleafs)
   * - 0=crops, grass, tundra etc
   */
  f3_gd = 0.0003;
  /// @brief Root depth, determining the soil layers influenced by root water uptake
  rootdepth = 1.0;
  /// @brief Wilting point, defined as a fraction of water content at field capacity (-)
  wp_fraction = 0.17;
  /// @brief Wilting point pressure head, when using Richards equation for soil.
  h_wilt = -1.55E6;
  //@}

  // INITIALIZE CANOPY DATA
  // State variable
  temp = 273.15; // temperature (K)
  storage = 0.0; // intercepted water (kg m-2 or mm Water Equivalent)  temp;        ///< temperature (K)
  ec = 1.0;          ///< longwave emissivity (1)
  // parameters
  lai = SSdata.Canopy_LAI;
  if(useCanopyModel &&  (lai < 0.0 || lai == mio::IOUtils::nodata ))
  {
    std::stringstream msg;
    msg << "Value provided for LAI (" << lai << ") in soil file is not valid.";
		throw UnknownValueException(msg.str(), AT);
  }

  z0m = height*0.1;
  z0h = z0m*0.1;
  zdispl = height*0.66;

  height =  SSdata.Canopy_Height;
  if(useCanopyModel &&  (height < 0.0 || height == mio::IOUtils::nodata ))
  {
    std::stringstream msg;
    msg << "Value provided for height (" << height << ") in soil file is not ialid.";
		throw UnknownValueException(msg.str(), AT);
  }

  direct_throughfall = SSdata.Canopy_Direct_Throughfall;
  if(useCanopyModel &&  (direct_throughfall < 0.0 || direct_throughfall >1.0 || direct_throughfall == mio::IOUtils::nodata ))
  {
    std::stringstream msg;
    msg << "Value provided for direct throughfall (" << direct_throughfall << ") in soil file is not valid.";
		throw UnknownValueException(msg.str(), AT);
  }

  sigf= 1.-exp(-krnt_lai * (lai));;        ///< radiation transmissivity (1)

  // aerodynamic resistances
  ra = 0.;          ///< from canopy air to reference height
  rc = 0.;          ///< from canopy to canopy air
  rs = 0.;          ///< from subsurface to canpopy air
  rstransp = 0.;    ///< stomatal surface resistance for transpiration
  // Averaged variables
  canopyalb = can_alb_dry;  ///< canopy albedo [-]
  totalalb=0;    ///< total albedo above canopy and snow/soil surface [-]
  wetfraction = 0.; ///< fraction of canopy covered by interception [-]
  intcapacity = 0.; ///< maximum interception storage [mm]
  // Radiations
  rswrac = 0.;      ///< upward shortwave above canopy
  iswrac = 0.;	    ///< downward shortwave radiation above canopy
  rswrbc = 0.;      ///< upward shortwave below canopy
  iswrbc = 0.;      ///< downward shortwave radiation below canopy
  ilwrac = 0.;      ///< downward longwave radiation ABOVE canopy
  rlwrac = 0.;      ///< upward longwave radiation ABOVE canopy
  ilwrbc = 0.;      ///< downward longwave radiation BELOW canopy
  rlwrbc = 0.;      ///< upward longwave radiation BELOW canopy
  rsnet = 0.;       ///< net shortwave radiation
  rlnet = 0.;       ///< net longwave radiation
  // Turbulent fluxes
  sensible = 0.;
  latent = 0.;
  latentcorr = 0.;
  // Evap fluxes
  transp = 0.;
  intevap = 0.;
  // Mass fluxes
  interception = 0.;
  throughfall = 0.;
  snowunload = 0.;

  snowfac = 0.;     ///< snowfall above canopy
  rainfac = 0.;     ///< rainfall above canopy
  liquidfraction = 0.;
  sigftrunk = 0.;   ///< radiation interception cross section for trunk layer ()
  Ttrunk = 273.15; // trunk temperature (K)
  CondFluxCanop = 0.; ///< biomass heat storage flux towards Canopy (if 1L) towards Leaves (if 2L). (>0 towards canopy)
  CondFluxTrunks = 0.; ///< biomass heat storage flux towards Trunks (if 2L)
  LWnet_Trunks = 0.; ///< net LW to trunks (>0 towards trunks)
  SWnet_Trunks= 0.; ///< net SW to trunks (>0 towards trunks)
  QStrunks = 0.;      ///< sensible heat flux from trunks (>0 if heat lost from trunk)
  forestfloor_alb = 0.; ///< albedo of the forest floor
  BasalArea =  SSdata.Canopy_BasalArea; ///< basal area of trees on the stand
  if(useCanopyModel &&  (BasalArea < 0.0 || BasalArea == mio::IOUtils::nodata ))
  {
    std::stringstream msg;
    msg << "Value provided for CanopyBasalArea (" << BasalArea << ") in soil file is not valid, the default value of 0.004 will be used.";
    prn_msg(__FILE__, __LINE__, "wrn", Date(),msg.str().c_str());
    BasalArea=0.004;
  }

  HMLeaves=3.*4190.;     ///< Leaves heat mass (J K-1 /m2 ground surface)
  HMTrunks=30.*4190.;     ///< Trunks heat mass (J K-1 /m2 ground surface)
}

void CanopyData::reset(const bool& cumsum_mass)
{
	if (cumsum_mass) { // Do not reset cumulated mass balance
		// radiation
		rswrac=0.;
		iswrac=0.;
		rswrbc=0.;
		iswrbc=0.;
		ilwrac=0.;
		rlwrac=0.;
		ilwrbc=0.;
		rlwrbc=0.;
		rsnet=0.;
		rlnet=0.;
		// turbulent heat fluxes
		sensible=0.0;
		latent=0.0;
		latentcorr=0.0;
		// 2layer canopy model
		CondFluxCanop = 0.;
		CondFluxTrunks = 0.;
		QStrunks = 0.;
		LWnet_Trunks = 0.;
		SWnet_Trunks = 0.;
		forestfloor_alb = 0.;
		// auxiliaries
		canopyalb=0.0;
		totalalb=0.0;
		intcapacity=0.0;
	} else {
		initializeSurfaceExchangeData();
	}
}

/**
 * @brief If multiple fluxes have been summed over multiple time steps, the
 * fluxes then need to be averaged by the number of steps. The albedos are also
 * averaged in this method.
 * @param factor Averaging factor (for example, 1/Nsteps)
 */
void CanopyData::multiplyFluxes(const double& factor)
{
	rswrac *= factor;
	iswrac *= factor;
	rswrbc *= factor;
	iswrbc *= factor;
	ilwrac *= factor;
	rlwrac *= factor;
	ilwrbc *= factor;
	rlwrbc *= factor;
	rsnet *= factor;
	rlnet *= factor;
	// turbulent heat fluxes
	sensible *= factor;
	latent *= factor;
	latentcorr *= factor;
	// 2Layer canopy model
	CondFluxCanop *= factor;
	CondFluxTrunks *= factor;
	QStrunks *= factor;
	LWnet_Trunks *= factor;
	SWnet_Trunks *= factor;
	forestfloor_alb *= factor;
	// auxiliaries
	canopyalb *= factor;
	totalalb *= factor;
	intcapacity *= factor;
}

std::ostream& operator<<(std::ostream& os, const CanopyData& data)
{
	os.write(reinterpret_cast<const char*>(&data.storage), sizeof(data.storage));
	os.write(reinterpret_cast<const char*>(&data.temp), sizeof(data.temp));
	os.write(reinterpret_cast<const char*>(&data.sigf), sizeof(data.sigf));
	os.write(reinterpret_cast<const char*>(&data.ec), sizeof(data.ec));

	os.write(reinterpret_cast<const char*>(&data.lai), sizeof(data.lai));
	os.write(reinterpret_cast<const char*>(&data.z0m), sizeof(data.z0m));
	os.write(reinterpret_cast<const char*>(&data.z0h), sizeof(data.z0h));
	os.write(reinterpret_cast<const char*>(&data.zdispl), sizeof(data.zdispl));
	os.write(reinterpret_cast<const char*>(&data.height), sizeof(data.height));
	os.write(reinterpret_cast<const char*>(&data.direct_throughfall), sizeof(data.direct_throughfall));

	os.write(reinterpret_cast<const char*>(&data.ra), sizeof(data.ra));
	os.write(reinterpret_cast<const char*>(&data.rc), sizeof(data.rc));
	os.write(reinterpret_cast<const char*>(&data.rs), sizeof(data.rs));
	os.write(reinterpret_cast<const char*>(&data.rstransp), sizeof(data.rstransp));
	os.write(reinterpret_cast<const char*>(&data.canopyalb), sizeof(data.canopyalb));
	os.write(reinterpret_cast<const char*>(&data.totalalb), sizeof(data.totalalb));
	os.write(reinterpret_cast<const char*>(&data.wetfraction), sizeof(data.wetfraction));
	os.write(reinterpret_cast<const char*>(&data.intcapacity), sizeof(data.intcapacity));

	os.write(reinterpret_cast<const char*>(&data.rswrac), sizeof(data.rswrac));
	os.write(reinterpret_cast<const char*>(&data.iswrac), sizeof(data.iswrac));
	os.write(reinterpret_cast<const char*>(&data.rswrbc), sizeof(data.rswrbc));
	os.write(reinterpret_cast<const char*>(&data.iswrbc), sizeof(data.iswrbc));
	os.write(reinterpret_cast<const char*>(&data.ilwrac), sizeof(data.ilwrac));
	os.write(reinterpret_cast<const char*>(&data.rlwrac), sizeof(data.rlwrac));
	os.write(reinterpret_cast<const char*>(&data.ilwrbc), sizeof(data.ilwrbc));
	os.write(reinterpret_cast<const char*>(&data.rlwrbc), sizeof(data.rlwrbc));
	os.write(reinterpret_cast<const char*>(&data.rsnet), sizeof(data.rsnet));
	os.write(reinterpret_cast<const char*>(&data.rlnet), sizeof(data.rlnet));

	os.write(reinterpret_cast<const char*>(&data.sensible), sizeof(data.sensible));
	os.write(reinterpret_cast<const char*>(&data.latent), sizeof(data.latent));
	os.write(reinterpret_cast<const char*>(&data.latentcorr), sizeof(data.latentcorr));

	os.write(reinterpret_cast<const char*>(&data.transp), sizeof(data.transp));
	os.write(reinterpret_cast<const char*>(&data.intevap), sizeof(data.intevap));

	os.write(reinterpret_cast<const char*>(&data.interception), sizeof(data.interception));
	os.write(reinterpret_cast<const char*>(&data.throughfall), sizeof(data.throughfall));
	os.write(reinterpret_cast<const char*>(&data.snowunload), sizeof(data.snowunload));

	os.write(reinterpret_cast<const char*>(&data.snowfac), sizeof(data.snowfac));
	os.write(reinterpret_cast<const char*>(&data.rainfac), sizeof(data.rainfac));
	os.write(reinterpret_cast<const char*>(&data.liquidfraction), sizeof(data.liquidfraction));
	os.write(reinterpret_cast<const char*>(&data.sigftrunk), sizeof(data.sigftrunk));
	os.write(reinterpret_cast<const char*>(&data.Ttrunk), sizeof(data.Ttrunk));
	os.write(reinterpret_cast<const char*>(&data.CondFluxCanop), sizeof(data.CondFluxCanop));
	os.write(reinterpret_cast<const char*>(&data.CondFluxTrunks), sizeof(data.CondFluxTrunks));
	os.write(reinterpret_cast<const char*>(&data.LWnet_Trunks), sizeof(data.LWnet_Trunks));
	os.write(reinterpret_cast<const char*>(&data.SWnet_Trunks), sizeof(data.SWnet_Trunks));
	os.write(reinterpret_cast<const char*>(&data.QStrunks), sizeof(data.QStrunks));
	os.write(reinterpret_cast<const char*>(&data.forestfloor_alb), sizeof(data.forestfloor_alb));
	os.write(reinterpret_cast<const char*>(&data.BasalArea), sizeof(data.BasalArea));
	os.write(reinterpret_cast<const char*>(&data.HMLeaves), sizeof(data.HMLeaves));
	os.write(reinterpret_cast<const char*>(&data.HMTrunks), sizeof(data.HMTrunks));

	os.write(reinterpret_cast< const char*>(&data.int_cap_snow), sizeof(data.int_cap_snow));
	os.write(reinterpret_cast< const char*>(&data.int_cap_rain), sizeof(data.int_cap_rain));
	os.write(reinterpret_cast< const char*>(&data.interception_timecoef), sizeof(data.interception_timecoef));
	os.write(reinterpret_cast<const char*>(&data.can_alb_dry), sizeof(data.can_alb_dry));
	os.write(reinterpret_cast<const char*>(&data.can_alb_wet), sizeof(data.can_alb_wet));
	os.write(reinterpret_cast<const char*>(&data.can_alb_snow), sizeof(data.can_alb_snow));
	os.write(reinterpret_cast<const char*>(&data.krnt_lai), sizeof(data.krnt_lai));
	os.write(reinterpret_cast<const char*>(&data.can_diameter), sizeof(data.can_diameter));
	os.write(reinterpret_cast<const char*>(&data.biomass_heat_capacity), sizeof(data.biomass_heat_capacity));
	os.write(reinterpret_cast<const char*>(&data.lai_frac_top_default), sizeof(data.lai_frac_top_default));
	os.write(reinterpret_cast<const char*>(&data.trunk_frac_height), sizeof(data.trunk_frac_height));
	os.write(reinterpret_cast<const char*>(&data.trunkalb), sizeof(data.trunkalb));
	os.write(reinterpret_cast<const char*>(&data.et), sizeof(data.et));
	os.write(reinterpret_cast<const char*>(&data.canopy_stabilitycorrection), sizeof(data.canopy_stabilitycorrection));
	os.write(reinterpret_cast<const char*>(&data.roughmom_to_canopyheight_ratio), sizeof(data.roughmom_to_canopyheight_ratio));
	os.write(reinterpret_cast<const char*>(&data.displ_to_canopyheight_ratio), sizeof(data.displ_to_canopyheight_ratio));
	os.write(reinterpret_cast<const char*>(&data.raincrease_snow), sizeof(data.raincrease_snow));
	os.write(reinterpret_cast<const char*>(&data.canopytemp_maxchange_perhour), sizeof(data.canopytemp_maxchange_perhour));
	os.write(reinterpret_cast<const char*>(&data.roughheat_to_roughmom_ratio), sizeof(data.roughheat_to_roughmom_ratio));
	os.write(reinterpret_cast<const char*>(&data.can_ch0), sizeof(data.can_ch0));
	os.write(reinterpret_cast<const char*>(&data.can_rs_mult), sizeof(data.can_rs_mult));
	os.write(reinterpret_cast<const char*>(&data.rsmin), sizeof(data.rsmin));
	os.write(reinterpret_cast<const char*>(&data.f3_gd), sizeof(data.f3_gd));
	os.write(reinterpret_cast<const char*>(&data.rootdepth), sizeof(data.rootdepth));
	os.write(reinterpret_cast<const char*>(&data.wp_fraction), sizeof(data.wp_fraction));
	os.write(reinterpret_cast<const char*>(&data.h_wilt), sizeof(data.h_wilt));

	return os;
}

std::istream& operator>>(std::istream& is, CanopyData& data)
{
	is.read(reinterpret_cast<char*>(&data.storage), sizeof(data.storage));
	is.read(reinterpret_cast<char*>(&data.temp), sizeof(data.temp));
	is.read(reinterpret_cast<char*>(&data.sigf), sizeof(data.sigf));
	is.read(reinterpret_cast<char*>(&data.ec), sizeof(data.ec));

	is.read(reinterpret_cast<char*>(&data.lai), sizeof(data.lai));
	is.read(reinterpret_cast<char*>(&data.z0m), sizeof(data.z0m));
	is.read(reinterpret_cast<char*>(&data.z0h), sizeof(data.z0h));
	is.read(reinterpret_cast<char*>(&data.zdispl), sizeof(data.zdispl));
	is.read(reinterpret_cast<char*>(&data.height), sizeof(data.height));
	is.read(reinterpret_cast<char*>(&data.direct_throughfall), sizeof(data.direct_throughfall));

	is.read(reinterpret_cast<char*>(&data.ra), sizeof(data.ra));
	is.read(reinterpret_cast<char*>(&data.rc), sizeof(data.rc));
	is.read(reinterpret_cast<char*>(&data.rs), sizeof(data.rs));
	is.read(reinterpret_cast<char*>(&data.rstransp), sizeof(data.rstransp));
	is.read(reinterpret_cast<char*>(&data.canopyalb), sizeof(data.canopyalb));
	is.read(reinterpret_cast<char*>(&data.totalalb), sizeof(data.totalalb));
	is.read(reinterpret_cast<char*>(&data.wetfraction), sizeof(data.wetfraction));
	is.read(reinterpret_cast<char*>(&data.intcapacity), sizeof(data.intcapacity));

	is.read(reinterpret_cast<char*>(&data.rswrac), sizeof(data.rswrac));
	is.read(reinterpret_cast<char*>(&data.iswrac), sizeof(data.iswrac));
	is.read(reinterpret_cast<char*>(&data.rswrbc), sizeof(data.rswrbc));
	is.read(reinterpret_cast<char*>(&data.iswrbc), sizeof(data.iswrbc));
	is.read(reinterpret_cast<char*>(&data.ilwrac), sizeof(data.ilwrac));
	is.read(reinterpret_cast<char*>(&data.rlwrac), sizeof(data.rlwrac));
	is.read(reinterpret_cast<char*>(&data.ilwrbc), sizeof(data.ilwrbc));
	is.read(reinterpret_cast<char*>(&data.rlwrbc), sizeof(data.rlwrbc));
	is.read(reinterpret_cast<char*>(&data.rsnet), sizeof(data.rsnet));
	is.read(reinterpret_cast<char*>(&data.rlnet), sizeof(data.rlnet));

	is.read(reinterpret_cast<char*>(&data.sensible), sizeof(data.sensible));
	is.read(reinterpret_cast<char*>(&data.latent), sizeof(data.latent));
	is.read(reinterpret_cast<char*>(&data.latentcorr), sizeof(data.latentcorr));

	is.read(reinterpret_cast<char*>(&data.transp), sizeof(data.transp));
	is.read(reinterpret_cast<char*>(&data.intevap), sizeof(data.intevap));

	is.read(reinterpret_cast<char*>(&data.interception), sizeof(data.interception));
	is.read(reinterpret_cast<char*>(&data.throughfall), sizeof(data.throughfall));
	is.read(reinterpret_cast<char*>(&data.snowunload), sizeof(data.snowunload));

	is.read(reinterpret_cast<char*>(&data.snowfac), sizeof(data.snowfac));
	is.read(reinterpret_cast<char*>(&data.rainfac), sizeof(data.rainfac));
	is.read(reinterpret_cast<char*>(&data.liquidfraction), sizeof(data.liquidfraction));
	is.read(reinterpret_cast<char*>(&data.sigftrunk), sizeof(data.sigftrunk));
	is.read(reinterpret_cast<char*>(&data.Ttrunk), sizeof(data.Ttrunk));
	is.read(reinterpret_cast<char*>(&data.CondFluxCanop), sizeof(data.CondFluxCanop));
	is.read(reinterpret_cast<char*>(&data.CondFluxTrunks), sizeof(data.CondFluxTrunks));
	is.read(reinterpret_cast<char*>(&data.LWnet_Trunks), sizeof(data.LWnet_Trunks));
	is.read(reinterpret_cast<char*>(&data.SWnet_Trunks), sizeof(data.SWnet_Trunks));
	is.read(reinterpret_cast<char*>(&data.QStrunks), sizeof(data.QStrunks));
	is.read(reinterpret_cast<char*>(&data.forestfloor_alb), sizeof(data.forestfloor_alb));
	is.read(reinterpret_cast<char*>(&data.BasalArea), sizeof(data.BasalArea));
	is.read(reinterpret_cast<char*>(&data.HMLeaves), sizeof(data.HMLeaves));
	is.read(reinterpret_cast<char*>(&data.HMTrunks), sizeof(data.HMTrunks));

	is.read(reinterpret_cast<char*>(&data.int_cap_snow), sizeof(data.int_cap_snow));
	is.read(reinterpret_cast<char*>(&data.int_cap_rain), sizeof(data.int_cap_rain));
	is.read(reinterpret_cast<char*>(&data.interception_timecoef), sizeof(data.interception_timecoef));
	is.read(reinterpret_cast<char*>(&data.can_alb_dry), sizeof(data.can_alb_dry));
	is.read(reinterpret_cast<char*>(&data.can_alb_wet), sizeof(data.can_alb_wet));
	is.read(reinterpret_cast<char*>(&data.can_alb_snow), sizeof(data.can_alb_snow));
	is.read(reinterpret_cast<char*>(&data.krnt_lai), sizeof(data.krnt_lai));
	is.read(reinterpret_cast<char*>(&data.can_diameter), sizeof(data.can_diameter));
	is.read(reinterpret_cast<char*>(&data.biomass_heat_capacity), sizeof(data.biomass_heat_capacity));
 	is.read(reinterpret_cast<char*>(&data.lai_frac_top_default), sizeof(data.lai_frac_top_default));
	is.read(reinterpret_cast<char*>(&data.trunk_frac_height), sizeof(data.trunk_frac_height));
 	is.read(reinterpret_cast<char*>(&data.trunkalb), sizeof(data.trunkalb));
 	is.read(reinterpret_cast<char*>(&data.et), sizeof(data.et));
 	is.read(reinterpret_cast<char*>(&data.canopy_stabilitycorrection), sizeof(data.canopy_stabilitycorrection));
	is.read(reinterpret_cast<char*>(&data.roughmom_to_canopyheight_ratio), sizeof(data.roughmom_to_canopyheight_ratio));
	is.read(reinterpret_cast<char*>(&data.displ_to_canopyheight_ratio), sizeof(data.displ_to_canopyheight_ratio));
	is.read(reinterpret_cast<char*>(&data.raincrease_snow), sizeof(data.raincrease_snow));
	is.read(reinterpret_cast<char*>(&data.canopytemp_maxchange_perhour), sizeof(data.canopytemp_maxchange_perhour));
	is.read(reinterpret_cast<char*>(&data.roughheat_to_roughmom_ratio), sizeof(data.roughheat_to_roughmom_ratio));
	is.read(reinterpret_cast<char*>(&data.can_ch0), sizeof(data.can_ch0));
	is.read(reinterpret_cast<char*>(&data.can_rs_mult), sizeof(data.can_rs_mult));
	is.read(reinterpret_cast<char*>(&data.rsmin), sizeof(data.rsmin));
	is.read(reinterpret_cast<char*>(&data.f3_gd), sizeof(data.f3_gd));
	is.read(reinterpret_cast<char*>(&data.rootdepth), sizeof(data.rootdepth));
	is.read(reinterpret_cast<char*>(&data.wp_fraction), sizeof(data.wp_fraction));
	is.read(reinterpret_cast<char*>(&data.h_wilt), sizeof(data.h_wilt));

	return is;
}

const std::string CanopyData::toString() const
{
	std::ostringstream os;
	os << "<CanopyData>" << "\n";
	os << "\tstorage:                           " << storage << "\n";
	os << "\ttemp:                              " << temp << "\n";
	os << "\tsigf:                              " <<  sigf << "\n";
	os << "\tec:                                " << ec << "\n";
	os << "\theight:                            " << height << "\n";
	os << "\tlai:                               " << lai << "\n";
 	os << "\tdirect_throughfall:                " << direct_throughfall << "\n";
	os << "\tz0m:                               " << z0m <<  "\n";
	os << "\tz0h:                               " <<  z0h << "\n";
	os << "\tzdispl:                            " << zdispl << "\n";
	os << "_____________________________________" <<  "\n";
	os << "\tra:                                " << ra << "\n";
	os << "\trc:                                " <<  rc << "\n";
	os << "\tdrs:                               " << rs << "\n";
	os << "\trstransp:                          " << rstransp << "\n";
	os << "\tcanopyalb:                         " << canopyalb << "\n";
	os << "\ttotalalb:                          " << totalalb << "\n";
	os << "\twetfraction:                       " << wetfraction << "\n";
	os << "\tintcapacity:                       " << intcapacity << "\n";
	os << "\trswrac:                            " << rswrac << "\n";
	os << "\tiswrac:                            " << iswrac << "\n";
	os << "_____________________________________" <<  "\n";
	os << "\tiswrbc:                            " << iswrbc << "\n";
	os << "\tilwrac:                            " << ilwrac << "\n";
	os << "\trlwrac:                            " << rlwrac << "\n";
	os << "\tilwrbc:                            " << ilwrbc << "\n";
	os << "\trlwrbc:                            " << rlwrbc << "\n";
	os << "\trsnet:                             " << rsnet << "\n";
	os << "\trlnet:                             " << rlnet << "\n";
	os << "\tsensible:                          " << sensible << "\n";
	os << "\tlatent:                            " << latent << "\n";
	os << "\tlatentcorr:                        " << latentcorr << "\n";
	os << "_____________________________________" <<  "\n";
	os << "\ttransp:                            " << transp << "\n";
	os << "\tintevap:                           " << intevap << "\n";
	os << "\tinterception:                      " << interception << "\n";
	os << "\tthroughfall:                       " << throughfall << "\n";
	os << "\tsnowunload:                        " << snowunload << "\n";
	os << "\tint_cap_snow:                      " << int_cap_snow << "\n";
  	os << "\tint_cap_rain:                      " << int_cap_rain << "\n";
  	os << "\tinterception_timecoef:             " << interception_timecoef << "\n";
	os << "\tcan_alb_dry:                       " << can_alb_dry << "\n";
	os << "\tcan_alb_wet:                       " << can_alb_wet << "\n";
	os << "_____________________________________" <<  "\n";
	os << "\tcan_alb_snow:                      " << can_alb_snow << "\n";
	os << "\tkrnt_lai:                          " << krnt_lai << "\n";
	os << "\tcan_diameter:                      " << can_diameter << "\n";
	os << "\tbiomass_heat_capacity:             " << biomass_heat_capacity << "\n";
	os << "\tbiomass_density:                   " << biomass_density << "\n";
	os << "\tlai_frac_top_default:              " << lai_frac_top_default << "\n";
	os << "\ttrunk_frac_height:                 " << trunk_frac_height << "\n";
	os << "\ttrunkalb:                          " << trunkalb << "\n";
	os << "\tet:                                " << et << "\n";
	os << "\tcanopy_stabilitycorrection:        " << canopy_stabilitycorrection << "\n";
	os << "_____________________________________" <<  "\n";
	os << "\troughmom_to_canopyheight_ratio:    " << roughmom_to_canopyheight_ratio << "\n";
	os << "\tdispl_to_canopyheight_ratio:       " << displ_to_canopyheight_ratio << "\n";
	os << "\traincrease_snow:                   " << raincrease_snow << "\n";
	os << "\tcanopytemp_maxchange_perhour:      " << canopytemp_maxchange_perhour << "\n";
	os << "\troughheat_to_roughmom_ratio:       " << roughheat_to_roughmom_ratio << "\n";
	os << "\tcan_ch0:                           " << can_ch0 << "\n";
	os << "\tcan_rs_mult:                       " << can_rs_mult << "\n";
	os << "\trsmin:                             " << rsmin << "\n";
	os << "\tf3_gd:                             " << f3_gd << "\n";
	os << "\trootdepth:                         " << rootdepth << "\n";
	os << "_____________________________________" <<  "\n";
	os << "\twp_fraction:                       " << wp_fraction << "\n";
	os << "\th_wilt:                            " << h_wilt << "\n";
	os << "\tsnowfac:                           " << snowfac << "\n";
	os << "\trainfac:                           " << rainfac << "\n";
	os << "\tliquidfraction:                    " << liquidfraction << "\n";
	os << "\tsigftrunk:                         " << sigftrunk << "\n";
	os << "\tTtrunk:                            " << Ttrunk << "\n";
	os << "\tCondFluxCanop:                     " << CondFluxCanop << "\n";
	os << "\tCondFluxTrunks:                    " << CondFluxTrunks << "\n";
	os << "\tLWnet_Trunks:                      " << LWnet_Trunks << "\n";
	os << "_____________________________________" <<  "\n";
	os << "\tSWnet_Trunks:                      " << SWnet_Trunks << "\n";
	os << "\tQStrunks:                          " << QStrunks << "\n";
	os << "\tforestfloor_alb:                   " << forestfloor_alb << "\n";
	os << "\tBasalArea:                         " << BasalArea << "\n";
	os << "\tHMLeaves:                          " << HMLeaves << "\n";
	os << "\tHMTrunks:                          " << HMLeaves << "\n";

	os << "</CanopyData>\n";
	return os.str();
}


/**
 * @brief Function called to initialize the canopy "Surface" exchange
 * data (to enable accumulated mass and energy fluxes)
 */
void CanopyData::initializeSurfaceExchangeData()
{
	// radiation
	rswrac=0.; // upward shortwave above canopy
	iswrac=0.; // downward shortwave radiation above canopy
	rswrbc=0.; // upward shortwave below canopy
	iswrbc=0.; // downward shortwave radiation below canopy
	ilwrac=0.; // downward longwave radiation ABOVE canopy
	rlwrac=0.; // upward longwave radiation ABOVE canopy
	ilwrbc=0.; // downward longwave radiation BELOW canopy
	rlwrbc=0.; // upward longwave radiation BELOW canopy
	rsnet=0.;  // net shortwave radiation absorbed by canopy
	rlnet=0.;  // net longwave radiation absorbed by canopy
	// turbulent heat fluxes
	sensible = 0.0;
	latent = 0.0;
	latentcorr = 0.0;
	// 2layer canopy model
	CondFluxCanop = 0.;
	CondFluxTrunks = 0.;
	LWnet_Trunks = 0.;
	SWnet_Trunks = 0.;
	forestfloor_alb = 0.;
	QStrunks = 0.;
	// mass fluxes
	transp = 0.0;
	intevap = 0.0;
	interception = 0.0;
	throughfall = 0.0;
	snowunload = 0.0;
	snowfac = 0.0; // snowfall above canopy
	rainfac = 0.0; // rainfall above canopy
	// auxiliaries
	canopyalb = 0.0;
	totalalb = 0.0;
	intcapacity = 0.0;
}

// Class ElementData
const unsigned short int ElementData::noID = static_cast<unsigned short int>(-1);
ElementData::ElementData(const unsigned short int& in_ID) : depositionDate(), L0(0.), L(0.),
                             Te(0.), gradT(0.), meltfreeze_tk(Constants::meltfreeze_tk),
                             theta((size_t)N_COMPONENTS), h(Constants::undefined), conc((size_t)N_COMPONENTS, SnowStation::number_of_solutes), k((size_t)N_SN_FIELDS), c((size_t)N_SN_FIELDS), soil((size_t)N_SOIL_FIELDS),
                             Rho(0.), M(0.), sw_abs(0.),
                             rg(0.), dd(0.), sp(0.), ogs(0.), rb(0.), N3(0.), mk(0),
                             type(0), metamo(0.), salinity(0.), dth_w(0.), res_wat_cont(0.), Qmf(0.), QIntmf(0.),
                             dEps(0.), Eps(0.), Eps_e(0.), Eps_v(0.), Eps_Dot(0.), Eps_vDot(0.), E(0.),
                             S(0.), C(0.), CDot(0.), ps2rb(0.),
                             s_strength(0.), hard(0.), S_dr(0.), crit_cut_length(Constants::undefined), soot_ppmv(0.), VG(*this), lwc_source(0.), PrefFlowArea(0.), SlopeParFlux(0.), Qph_up(0.), Qph_down(0.), dsm(0.), ID(in_ID) {}

ElementData::ElementData(const ElementData& cc) :
                             depositionDate(cc.depositionDate), L0(cc.L0), L(cc.L),
                             Te(cc.Te), gradT(cc.gradT), meltfreeze_tk(cc.meltfreeze_tk),
                             theta(cc.theta), h(cc.h), conc(cc.conc), k(cc.k), c(cc.c), soil(cc.soil),
                             Rho(cc.Rho), M(cc.M), sw_abs(cc.sw_abs),
                             rg(cc.rg), dd(cc.dd), sp(cc.sp), ogs(cc.ogs), rb(cc.rb), N3(cc.N3), mk(cc.mk),
                             type(cc.type), metamo(cc.metamo), salinity(cc.salinity), dth_w(cc.dth_w), res_wat_cont(cc.res_wat_cont), Qmf(cc.Qmf), QIntmf(cc.QIntmf),
                             dEps(cc.dEps), Eps(cc.Eps), Eps_e(cc.Eps_e), Eps_v(cc.Eps_v), Eps_Dot(cc.Eps_Dot), Eps_vDot(cc.Eps_vDot), E(cc.E),
                             S(cc.S), C(cc.C), CDot(cc.CDot), ps2rb(cc.ps2rb),
                             s_strength(cc.s_strength), hard(cc.hard), S_dr(cc.S_dr), crit_cut_length(cc.crit_cut_length), soot_ppmv(cc.soot_ppmv), VG(*this), lwc_source(cc.lwc_source), PrefFlowArea(cc.PrefFlowArea), SlopeParFlux(cc.SlopeParFlux), Qph_up(cc.Qph_up), Qph_down(cc.Qph_down), dsm(cc.dsm), ID(cc.ID) {}

std::ostream& operator<<(std::ostream& os, const ElementData& data)
{
	os << data.depositionDate;
	os.write(reinterpret_cast<const char*>(&data.L0), sizeof(data.L0));
	os.write(reinterpret_cast<const char*>(&data.L), sizeof(data.L));
	os.write(reinterpret_cast<const char*>(&data.Te), sizeof(data.Te));
	os.write(reinterpret_cast<const char*>(&data.gradT), sizeof(data.gradT));
	os.write(reinterpret_cast<const char*>(&data.meltfreeze_tk), sizeof(data.meltfreeze_tk));

	const size_t s_theta = data.theta.size();
	os.write(reinterpret_cast<const char*>(&s_theta), sizeof(size_t));
	os.write(reinterpret_cast<const char*>(&data.theta[0]), static_cast<streamsize>(s_theta*sizeof(data.theta[0])));
	os.write(reinterpret_cast<const char*>(&data.h), sizeof(data.h));
	os << data.conc;

	const size_t s_k = data.k.size();
	os.write(reinterpret_cast<const char*>(&s_k), sizeof(size_t));
	os.write(reinterpret_cast<const char*>(&data.k[0]), static_cast<streamsize>(s_k*sizeof(data.k[0])));

	const size_t s_c = data.c.size();
	os.write(reinterpret_cast<const char*>(&s_c), sizeof(size_t));
	os.write(reinterpret_cast<const char*>(&data.c[0]), static_cast<streamsize>(s_c*sizeof(data.c[0])));

	const size_t s_soil = data.soil.size();
	os.write(reinterpret_cast<const char*>(&s_soil), sizeof(size_t));
	os.write(reinterpret_cast<const char*>(&data.soil[0]), static_cast<streamsize>(s_soil*sizeof(data.soil[0])));

	os.write(reinterpret_cast<const char*>(&data.Rho), sizeof(data.Rho));
	os.write(reinterpret_cast<const char*>(&data.M), sizeof(data.M));
	os.write(reinterpret_cast<const char*>(&data.sw_abs), sizeof(data.sw_abs));
	os.write(reinterpret_cast<const char*>(&data.rg), sizeof(data.rg));
	os.write(reinterpret_cast<const char*>(&data.dd), sizeof(data.dd));
	os.write(reinterpret_cast<const char*>(&data.sp), sizeof(data.sp));
	os.write(reinterpret_cast<const char*>(&data.ogs), sizeof(data.ogs));
	os.write(reinterpret_cast<const char*>(&data.rb), sizeof(data.rb));
	os.write(reinterpret_cast<const char*>(&data.N3), sizeof(data.N3));
	os.write(reinterpret_cast<const char*>(&data.mk), sizeof(data.mk));
	os.write(reinterpret_cast<const char*>(&data.type), sizeof(data.type));
	os.write(reinterpret_cast<const char*>(&data.metamo), sizeof(data.metamo));
	os.write(reinterpret_cast<const char*>(&data.salinity), sizeof(data.salinity));
	os.write(reinterpret_cast<const char*>(&data.dth_w), sizeof(data.dth_w));
	os.write(reinterpret_cast<const char*>(&data.res_wat_cont), sizeof(data.res_wat_cont));
	os.write(reinterpret_cast<const char*>(&data.Qmf), sizeof(data.Qmf));
	os.write(reinterpret_cast<const char*>(&data.QIntmf), sizeof(data.QIntmf));

	os.write(reinterpret_cast<const char*>(&data.dEps), sizeof(data.dEps));
	os.write(reinterpret_cast<const char*>(&data.Eps), sizeof(data.Eps));
	os.write(reinterpret_cast<const char*>(&data.Eps_e), sizeof(data.Eps_e));
	os.write(reinterpret_cast<const char*>(&data.Eps_v), sizeof(data.Eps_v));
	os.write(reinterpret_cast<const char*>(&data.Eps_Dot), sizeof(data.Eps_Dot));
	os.write(reinterpret_cast<const char*>(&data.Eps_vDot), sizeof(data.Eps_vDot));
	os.write(reinterpret_cast<const char*>(&data.E), sizeof(data.E));

	os.write(reinterpret_cast<const char*>(&data.S), sizeof(data.S));
	os.write(reinterpret_cast<const char*>(&data.C), sizeof(data.C));
	os.write(reinterpret_cast<const char*>(&data.CDot), sizeof(data.CDot));
	os.write(reinterpret_cast<const char*>(&data.ps2rb), sizeof(data.ps2rb));
	os.write(reinterpret_cast<const char*>(&data.s_strength), sizeof(data.s_strength));
	os.write(reinterpret_cast<const char*>(&data.hard), sizeof(data.hard));
	os.write(reinterpret_cast<const char*>(&data.S_dr), sizeof(data.S_dr));
	os.write(reinterpret_cast<const char*>(&data.crit_cut_length), sizeof(data.crit_cut_length));
	os.write(reinterpret_cast<const char*>(&data.soot_ppmv), sizeof(data.soot_ppmv));
	os.write(reinterpret_cast<const char*>(&data.VG), sizeof(data.VG));
	os.write(reinterpret_cast<const char*>(&data.lwc_source), sizeof(data.lwc_source));
	os.write(reinterpret_cast<const char*>(&data.PrefFlowArea), sizeof(data.PrefFlowArea));
	os.write(reinterpret_cast<const char*>(&data.SlopeParFlux), sizeof(data.SlopeParFlux));
	os.write(reinterpret_cast<const char*>(&data.Qph_up), sizeof(data.Qph_up));
	os.write(reinterpret_cast<const char*>(&data.Qph_down), sizeof(data.Qph_down));
	os.write(reinterpret_cast<const char*>(&data.dsm), sizeof(data.dsm));
	os.write(reinterpret_cast<const char*>(&data.ID), sizeof(data.ID));
	return os;
}

std::istream& operator>>(std::istream& is, ElementData& data)
{
	is >> data.depositionDate;
	is.read(reinterpret_cast<char*>(&data.L0), sizeof(data.L0));
	is.read(reinterpret_cast<char*>(&data.L), sizeof(data.L));
	is.read(reinterpret_cast<char*>(&data.Te), sizeof(data.Te));
	is.read(reinterpret_cast<char*>(&data.gradT), sizeof(data.gradT));
	is.read(reinterpret_cast<char*>(&data.meltfreeze_tk), sizeof(data.meltfreeze_tk));

	size_t s_theta;
	is.read(reinterpret_cast<char*>(&s_theta), sizeof(size_t));
	data.theta.resize(s_theta);
	is.read(reinterpret_cast<char*>(&data.theta[0]), static_cast<streamsize>(s_theta*sizeof(data.theta[0])));
	is.read(reinterpret_cast<char*>(&data.h), sizeof(data.h));
	is >> data.conc;

	size_t s_k;
	is.read(reinterpret_cast<char*>(&s_k), sizeof(size_t));
	data.k.resize(s_k);
	is.read(reinterpret_cast<char*>(&data.k[0]), static_cast<streamsize>(s_k*sizeof(data.k[0])));

	size_t s_c;
	is.read(reinterpret_cast<char*>(&s_c), sizeof(size_t));
	data.c.resize(s_c);
	is.read(reinterpret_cast<char*>(&data.c[0]), static_cast<streamsize>(s_c*sizeof(data.c[0])));

	size_t s_soil;
	is.read(reinterpret_cast<char*>(&s_soil), sizeof(size_t));
	data.soil.resize(s_soil);
	is.read(reinterpret_cast<char*>(&data.soil[0]), static_cast<streamsize>(s_soil*sizeof(data.soil[0])));

	is.read(reinterpret_cast<char*>(&data.Rho), sizeof(data.Rho));
	is.read(reinterpret_cast<char*>(&data.M), sizeof(data.M));
	is.read(reinterpret_cast<char*>(&data.sw_abs), sizeof(data.sw_abs));
	is.read(reinterpret_cast<char*>(&data.rg), sizeof(data.rg));
	is.read(reinterpret_cast<char*>(&data.dd), sizeof(data.dd));
	is.read(reinterpret_cast<char*>(&data.sp), sizeof(data.sp));
	is.read(reinterpret_cast<char*>(&data.ogs), sizeof(data.ogs));
	is.read(reinterpret_cast<char*>(&data.rb), sizeof(data.rb));
	is.read(reinterpret_cast<char*>(&data.N3), sizeof(data.N3));
	is.read(reinterpret_cast<char*>(&data.mk), sizeof(data.mk));
	is.read(reinterpret_cast<char*>(&data.type), sizeof(data.type));
	is.read(reinterpret_cast<char*>(&data.metamo), sizeof(data.metamo));
	is.read(reinterpret_cast<char*>(&data.salinity), sizeof(data.salinity));
	is.read(reinterpret_cast<char*>(&data.dth_w), sizeof(data.dth_w));
	is.read(reinterpret_cast<char*>(&data.res_wat_cont), sizeof(data.res_wat_cont));
	is.read(reinterpret_cast<char*>(&data.Qmf), sizeof(data.Qmf));
	is.read(reinterpret_cast<char*>(&data.QIntmf), sizeof(data.QIntmf));

	is.read(reinterpret_cast<char*>(&data.dEps), sizeof(data.dEps));
	is.read(reinterpret_cast<char*>(&data.Eps), sizeof(data.Eps));
	is.read(reinterpret_cast<char*>(&data.Eps_e), sizeof(data.Eps_e));
	is.read(reinterpret_cast<char*>(&data.Eps_v), sizeof(data.Eps_v));
	is.read(reinterpret_cast<char*>(&data.Eps_Dot), sizeof(data.Eps_Dot));
	is.read(reinterpret_cast<char*>(&data.Eps_vDot), sizeof(data.Eps_vDot));
	is.read(reinterpret_cast<char*>(&data.E), sizeof(data.E));

	is.read(reinterpret_cast<char*>(&data.S), sizeof(data.S));
	is.read(reinterpret_cast<char*>(&data.C), sizeof(data.C));
	is.read(reinterpret_cast<char*>(&data.CDot), sizeof(data.CDot));
	is.read(reinterpret_cast<char*>(&data.ps2rb), sizeof(data.ps2rb));
	is.read(reinterpret_cast<char*>(&data.s_strength), sizeof(data.s_strength));
	is.read(reinterpret_cast<char*>(&data.hard), sizeof(data.hard));
	is.read(reinterpret_cast<char*>(&data.S_dr), sizeof(data.S_dr));
	is.read(reinterpret_cast<char*>(&data.crit_cut_length), sizeof(data.crit_cut_length));
	is.read(reinterpret_cast<char*>(&data.soot_ppmv), sizeof(data.soot_ppmv));
	is.read(reinterpret_cast<char*>(&data.VG), sizeof(data.VG));
	is.read(reinterpret_cast<char*>(&data.lwc_source), sizeof(data.lwc_source));
	is.read(reinterpret_cast<char*>(&data.PrefFlowArea), sizeof(data.PrefFlowArea));
	is.read(reinterpret_cast<char*>(&data.SlopeParFlux), sizeof(data.SlopeParFlux));
	is.read(reinterpret_cast<char*>(&data.Qph_up), sizeof(data.Qph_up));
	is.read(reinterpret_cast<char*>(&data.Qph_down), sizeof(data.Qph_down));
	is.read(reinterpret_cast<char*>(&data.dsm), sizeof(data.dsm));
	is.read(reinterpret_cast<char*>(&data.ID), sizeof(data.ID));
	return is;
}

double ElementData::getYoungModule(const double& rho_slab, const Young_Modulus& model)
{
	switch (model) {
		case Sigrist: {//This is the parametrization by Sigrist, 2006
			static const double A = 968.e6; //in Pa
			const double E_local = A * pow( rho_slab/Constants::density_ice, 2.94 ); //in Pa
			return E_local;
		}
		case Pow: {
			const double E_local = 5.07e9 * (pow((rho_slab/Constants::density_ice), 5.13));
			return E_local;
		}
		case Exp: {
			const double E_local = 1.873e5 * exp(0.0149*(rho_slab));
			return E_local;
		}
		default:
			throw mio::UnknownValueException("Selected Young's modulus model has not been implemented", AT);
	}
}

/**
 * @brief Check volumetric content
 * @version 11.01
 * @return sum of volumetric contents (1)
 */
bool ElementData::checkVolContent()
{
	bool ret = true;
	/*if(fabs(L*Rho - M) > 0.001) {
		prn_msg(__FILE__, __LINE__, "wrn", Date(), "Inconsistent mass: M = %1.4f, L*Rho = %1.4f * %1.4f = %1.4f", M, L, Rho, L*Rho);
		ret = false;
	}*/

	double sum = 0.;
	for (unsigned int i = 0; i < N_COMPONENTS; i++) {
		sum += theta[i];
	}
	if (sum <= 1. - Constants::eps || sum >= 1. + Constants::eps) {
		prn_msg(__FILE__, __LINE__, "wrn", Date(), "SUM of volumetric contents = %1.4f", sum);
		ret = false;
	}
	if(theta[SOIL] < -Constants::eps) {
		prn_msg(__FILE__, __LINE__, "wrn", Date(), "Negative SOIL volumetric content: %1.4f", theta[SOIL]);
		ret = false;
	}
	if(theta[ICE] < -Constants::eps) {
		prn_msg(__FILE__, __LINE__, "wrn", Date(), "Negative ICE volumetric content: %1.4f", theta[ICE]);
		ret = false;
	}
	if(theta[WATER] < -Constants::eps) {
		prn_msg(__FILE__, __LINE__, "wrn", Date(), "Negative WATER volumetric content: %1.4f", theta[WATER]);
		ret = false;
	}
	if(theta[AIR] < -Constants::eps) {
		prn_msg(__FILE__, __LINE__, "wrn", Date(), "Negative AIR volumetric content: %1.4f", theta[AIR]);
		ret = false;
	}

	// Take care of small rounding errors, in case large rounding errors do not exist.
	if (ret == true) {
		theta[ICE] = std::min(1., std::max(0., theta[ICE]));
		theta[WATER] = std::min(1., std::max(0., theta[WATER]));
		theta[WATER_PREF] = std::min(1., std::max(0., theta[WATER_PREF]));
		theta[AIR] = (1. - theta[ICE] - theta[WATER] - theta[WATER_PREF] - theta[SOIL]);
		if (theta[AIR] < 0.) {
			if (theta[ICE] > 1. - Constants::eps) {
				theta[ICE] += theta[AIR];
			} else if (theta[WATER] > 1. - Constants::eps) {
				theta[WATER] += theta[AIR];
			} else {
				prn_msg(__FILE__, __LINE__, "wrn", Date(), "SUM of volumetric contents = %1.20f, theta[ICE] = %.20f, theta[WATER] = %.20f, theta[WATER_PREF] = %.20f, theta[SOIL] = %.20f, theta[AIR] = %.20f", sum, theta[ICE], theta[WATER], theta[WATER_PREF], theta[SOIL], theta[AIR]);
			}
			theta[AIR] = 0.;
		}
	}

	return ret;
}

/**
 * @brief Computes heat capacity of an element based on volumetric contents
 * @version 11.01
 * set the effective heat capacity (J kg-1 K-1)
 */
void ElementData::heatCapacity()
{
	double c_p;

	c_p  = Constants::density_air * theta[AIR] * Constants::specific_heat_air;
	c_p += Constants::density_ice * theta[ICE] * Constants::specific_heat_ice;
	c_p += Constants::density_water * (theta[WATER] + theta[WATER_PREF]) * Constants::specific_heat_water;
	c_p += soil[SOIL_RHO] * theta[SOIL] * soil[SOIL_C];
	c_p /= Rho;
	c[TEMPERATURE] = c_p;
}

/**
 * @brief Computes cold content of an element, taking meltfreeze_tk as reference
 * @version 10.08
 * @return Cold content (J m-2)
 */
double ElementData::coldContent() const
{
	return (Rho * c[TEMPERATURE] * (Te - Constants::meltfreeze_tk) * L);
}

/**
 * @brief Updates element density
 * @version 17.12
 */
void ElementData::updDensity()
{
	// Calculate element density
	Rho = theta[ICE] * Constants::density_ice +
              (theta[WATER] + theta[WATER_PREF]) * Constants::density_water +
              salinity * SeaIce::betaS +						//salinity = bulk salinity
              theta[SOIL] * soil[SOIL_RHO];
	M = Rho * L;
	return;
}

/**
 * @brief Opical equivalent grain size\n
 * CROCUS implementation as described in Vionnet et al., 2012. The detailed snowpack scheme Crocus and
 * its implementation in SURFEX v7.2, Geosci. Model Dev., 5, 773-791, 10.5194/gmd-5-773-2012. (see section 3.6)
 */
void ElementData::opticalEquivalentGrainSize()
{
	// NOTE Be careful regarding dimension!!!
	if (dd > Constants::eps2)
		ogs = 2. * (1.e-1 * (0.5 * (dd + (1. - dd) * (4. - sp)))); // (mm)
	else
		ogs = 2. * (0.5 * ((2. * rg * sp) + (1. - sp) * std::max(4.e-1, rg))); // rg in mm
}

/**
 * @brief Density dependent extinction coefficient -> Michi's magic trick... out of his magic hat
 * @version 9Y.mm
 * @return Density dependent extinction coefficient (m-1)
 */
double ElementData::extinction() const
{
	return(Rho/10. + 30.);
	//return(Edata->Rho/10. + 30.);
	//return(Edata->Rho/7.  + 70.);
	//return(Edata->Rho/7.  + 75. - 0.0*Edata->theta[WATER]);
}

/**
 * @brief Estimate the residual water content RWC by Vol \n
 * From work by Coleou and Lesaffre, 1998, Ann. Glaciol., 26, 64-68. \n
 * Experimental range:
 * - density unsoaked: 235 to 580
 * - density soaked: 328 to 589 kg m-3
 * - RWC by Mass 0.049 to 0.029
 * @note That function will limit range to 0.0264 to 0.08 RWC by Vol
 * @version 11.01
 * @return residual water content of snow element (1)
 */
void ElementData::snowResidualWaterContent()
{
	res_wat_cont = snowResidualWaterContent(theta[ICE]);
}

double ElementData::snowResidualWaterContent(const double& theta_i)
{
	double resWatCont;

	static const double fraction = Constants::density_water/Constants::density_ice;
	static const double limit_theta_i = 1. - fraction * ((1. + 0.0165 * fraction) - sqrt((1. + 0.0165 * fraction)*(1. + 0.0165 * fraction) - 4. * fraction * 0.0264)) / (2. * fraction);	// abc-formula

	if (theta_i > limit_theta_i) {
		// This case is the limiting case where:
		//            theta_i + (theta_r * (Constants::density_water/Constants::density_ice)) >= 1.0
		// In that case, set the residual water content equal to the pore space
		resWatCont = (1. - theta_i) * (Constants::density_ice/Constants::density_water);
	} else {
		if (theta_i > 0.23) {
			resWatCont = 0.0264 + 0.0099 * (1. - theta_i) / theta_i;
		} else {
			resWatCont = 0.08 - 0.1023 * (theta_i - 0.03);
		}
	}
	return std::min(resWatCont, 0.08); //NOTE: std::min() only needed in case of theta_i < 0.03
}

/**
 * @brief Field Capacity Soil is dependent on grain properties.
 * This nice formulation is based on some tedious curve fitting by
 * Martina Luetschg. The data stems from Scheffer und Schachtschabel
 * (Richtwerte Baugrund), which once more proves that "nomen est omen".
 * If my name was "Schachtschabel", I would never ever be dealing with
 * wet soils and Baugrund.
 * For the USDA soil classes used if Richards equation is used, the field capacity is obtained using the Saxton formula (K.E. Saxton et al., 1986, Estimating generalized soil-water characteristics from texture. Soil Sci. Soc. Amer. J. 50(4):1031-1036) and defined in the VG class
 * @author Michael Lehning & Adrien michel
 * @version 9Y.mm
 * @return Soil field capacity (-)
 */
double ElementData::soilFieldCapacity() const
{
	double fc;
  if (VG.defined == true)
  {
    fc=VG.field_capacity;
  }
  else{
  	if (!(rg > 0.)) {
  		fc = std::min(SnLaws::field_capacity_soil, (1. - theta[SOIL]) * 0.1);
  	} else {
  		//Follow implementation by Tobias Hipp master thesis.
  		//Note that the value of 0.0976114 is more precise and the value of 60.8057 is
  		//slightly different from what is mentioned in thesis, to make the function continuous over rg.
  		if(rg<17.0) {
  			fc = std::min(0.95, 0.32 / sqrt(rg) + 0.02);
  		} else {
  			if(rg<60.8057) {
  				fc=0.0976114-0.002*(rg-17.0);
  			} else {
  				fc=0.01;
  			}
  		}
  	}
  }
	return std::min(1. - theta[SOIL], fc);		// Ensure that the field capacity does not exceed the pore space.
}

/**
 * @brief RelativeHumidity
 * @author Nander Wever et al.
 * @brief Relative humidity in soil.
 * The formulation is based on Saito et al., 2006 "Numerical analysis of
 * coupled water vapor and heat transport in the vadose zone".
 * Calculated from the pressure head using a thermodynamic relationship
 * between liquid water and water vapor in soil pores (Philip and de Vries, 1957)
 * @author Margaux Couttet
 * @param Edata element data
 * @param Temperature temperature (K)
 * @return Soil relative humidity (-)
 */

double ElementData::RelativeHumidity() const
{
	if (VG.defined == true) {
		return (std::max(0., std::min(1., exp(h * Constants::g / (Constants::gas_constant * Te))))); //see eq. [18] from Saito et al., 2006
	} else {
		if ((theta[WATER] + theta[WATER_PREF]) < soilFieldCapacity() ) {
			return (0.5 * ( 1. - cos (std::min(Constants::pi, (theta[WATER] + theta[WATER_PREF]) * Constants::pi / (soilFieldCapacity() * 1.6)))));
		} else {
			return 1.;
		}
	}
}

/**
 * @brief SNOW ELASTICITY  :  This important routine was programmed by Marc Christen, who took it directly
 * from Mellor's famous 1975 paper on SNOW MECHANICS in the GRINDLEWALD symposium. Dimensions
 * are in [Pa]. (Presently, it is NOT temperature dependent.)
 * @version 9Y.mm
 * @return Module of elasticity (Pa)
 */
double ElementData::snowElasticity() const
{
	if (Rho > 1000.)
		return Constants::big;

	const double g = (Rho >= 70.)? ((Rho/1000.0)*8.235)-0.47 : ((70./1000.0)*8.235 )-0.47;
	const double he = pow(10.0, g);
	return (he * 100000.0);
}

/**
 * @brief Computes the enhancement of hydrostatically applied stress (overburden) in the necks (or bonds)
 * @version 11.01
 * @return Enhancement factor for neck stress (1))
 */
double ElementData::neckStressEnhancement() const
{
	const double stressEnhance = (4. / (N3 * theta[ICE])) * Optim::pow2(rg/rb);
	return stressEnhance;
}

/**
 * @brief A non-generic function to compute the concave neck radius (mm). \n
 * It is assumed that the neck is bound by a sphere fitting between the two grains side by side
 * such as the sphere goes to RB from the axis (this is a quick and dirty approximation)
 * @author Mathias Bavay
 * @version 9.mm
 * @return Concave neck radius (mm)
 */
double ElementData::concaveNeckRadius() const
{
	if ( (rg - rb) < Constants::eps ) {
		prn_msg(__FILE__, __LINE__, "wrn", Date(), "Infinite radius of curvature, rg(%lf) = rb(%lf); return Constants::big!", rg, rb);
		return Constants::big;
	} else {
		return Optim::pow2(rb) / (2. * (rg - rb));
	}
}

/**
 * @brief Computes the neck (or bond) length (mm)
 * @version 11.01
 * @return Neck length (mm)
 */
double ElementData::neckLength() const
{
	const double rc = concaveNeckRadius();
	return ((2. * rg * rc) / (rg + rc));
}

/**
 * @brief Relates the neck strain to the global volumetric strain
 * @version 11.01
 * @return Macro factor (1)
 */
double ElementData::neck2VolumetricStrain() const
{
	const double Ln = neckLength();
	return (Ln / (2. * rg + Ln));
}

/**
 * @brief Determine the type of snow \n
 * First revisited by Fierz and Bellaire 2006 and 2007
 * TODO needs to be adapted to international classification
 * @version 11.11
 * @return snow type code according to old-fashioned Swiss tradition
 */

void ElementData::snowType()
{
	type = snowType(dd, sp, 2.*rg, static_cast<unsigned short int>(mk%100), theta[WATER], res_wat_cont);
}

unsigned short int ElementData::getSnowType() const
{
	return snowType(dd, sp, 2.*rg, static_cast<unsigned short int>(mk%100), theta[WATER], res_wat_cont);
}

unsigned short int ElementData::snowType(const double& dendricity, const double& sphericity,
                          const double& grain_size, const unsigned short int& marker, const double& theta_w, const double& res_wat_cont_loc)
{
	int a=-1,b=-1,c=0;

	// Dry snow
	if (dendricity > 0.) { // Dry dendritic (new) snow: dendricity and sphericityhericity determine the class
		const int sw2 = (int)(sphericity*10.);
		if (dendricity > 0.80 ) { // ori 0.90, 27 Nov 2007 sb
			a = 1; b = 1; c = 0;
		} else if (dendricity > 0.70) { // ori 0.85, 27 Nov 2007 sb
			a = 1; b = 2; c = 1;
		} else if (dendricity > 0.65) { // ori 0.75, 27 Nov 2007 sb
			a = 2; b = 1; c = 0;
		} else if (dendricity > 0.60) { // ori 0.70, 27 Nov 2007 sb
			a = 2; b = 1; c = 1;
		} else if (dendricity > 0.30) {
			a = 2; b = 2; c = 0;
		} else if (dendricity > 0.05) {
			a = 2;
			switch (sw2) {
				case 0: case 1: case 2:
					b = 4; c = 0; break;
				case 3: case 4:
					b = 4; c = 1; break;
				case 5: case 6:
					b = 3; c = 1; break;
				default:
					b = 3; c = 0;
			}
		} else {
			switch (sw2) {
				case 0: case 1:
					a = 4; b = 4; c = 0; break;
				case 2: case 3: case 4:
					a = 4; b = 2; c = 1; break;
				case 5: case 6: case 7:
					a = 3;  b = 2; c = 1; break;
				default:
					a = 3; b = 3; c = 0;
			}
		}
	} else if (marker <= 2) {
		// Dry non-dendritic snow
		// Sphericity is most important for "a", while the marker is most important for "b","c"
		if (grain_size < 0.7) {
			const int sw2 = (int)(sphericity*10.);
			switch (sw2) {
				case 0: case 1:
					a = 4; b = 4; c = 0; break;
				case 2: case 3:
					a = 4; b = 3; c = 1; break;
				case 4: case 5:
					a = 4;  b = 3; c = 0; break;
				case 6: case 7:
					a = 3;  b = 4; c = 1; break;
				default:
					a = 3; b = 3; c = 0;
			}
		} else if (grain_size < 1.1) {
			if (sphericity < 0.2) {
				a = 4; b = 4; c = 0;
			} else if (sphericity < 0.4) {
				a = 4; b = 9; c = 0;
			} else { // sphericityhericity limited to sphericity_max=0.5 in Metamorphism.c
				a = 9; b = 9 ; c = 0;
			}
		} else if (grain_size < 1.5) {
			if (sphericity < 0.2) {
				a = 4; b = 5; c = 0;
			} else if (sphericity < 0.4) {
				a = 4; b = 9; c = 1;
			} else { // sphericityhericity limited to sphericity_max=0.5 in Metamorphism.c
				a = 9; b = 9 ; c = 0;
			}
		} else {
			if (sphericity < 0.2) {
				a = 5; b = 5; c = 0;
			} else if (sphericity < 0.4) {
				a = 5; b = 9; c = 1;
			} else { // sphericityhericity limited to sphericity_max=0.5 in Metamorphism.c
				a = 9; b = 5 ; c = 1;
			}
		}
	}
	// Melt forms
	if (marker >= 10) {
		if (dendricity > 0.) { // Wet dendritic snow
			if (sphericity > 0.7) {
				b = a; a = 7; c = 0;
			} else {
				b = 7 ; c = 1;
			}
		} else { // Wet non-dendritic snow
			b = 7; c = 0;
			if (sphericity > 0.75) {
				a = 7;
			} else if (sphericity > 0.4 ) {
				if (grain_size <= 0.7) {
					b = a; a = 7;
				} else if (marker != 13 ) {
					if (grain_size <= 1.5) {
						a = 7; b = 9; c = 1;
					} else {
						a = 7; b = 5; c = 1;
					}
				} else {
					a = 7; b = 6;  c = 1;
				}
			} else {
				if (grain_size <= 1.5) {
					a = 4;
				} else {
					a = 5;
				}
				if (sphericity <= 0.2) {
					c = 1;
				}
			}
		}
	}
	// Now treat a couple of exceptions - note that the order is important
	if (b < 0) b = a;
	if ((marker >= 20) && (theta_w < 0.1 * res_wat_cont_loc)) { // MFcr Melt-Freeze
		c = 2;
	}
	switch (marker) {
		case 3: // SH   Surface Hoar
			a = 6; b = 6; c = 0;
			break;
		case 4: // PPgp Graupel
			a = 0; b = 0; c = 0;
			break;
		case 6: // technical Snow
			a = 0; b = 0; c= 6;
			break;
		case 7: case 8: case 17: case 18: case 27: case 28: // Glacier ice & IFil, that is, ice layers within the snowpack
			a = 8; b = 8; c = 0;
			break;
		case 9: // water layer
			a = 0; b = 0; c = 0;
			break;
		default: // do nothing since we take care of exceptions here
			break;
	}

	return static_cast<unsigned short int>(a*100 + b*10 + c);
}

const std::string ElementData::toString() const
{
	std::ostringstream os;
	os << "<ElementData>\t";
	os << std::fixed << std::showpoint;
	os << "" << depositionDate.toString(mio::Date::ISO) << "\n";
	os << "\tL=" << setprecision(4) << L << " type=" << type << " marker=" << setprecision(2) << mk << " Density=" << Rho << " Mass=" << M << "\n";;

	os << "\tVolumetric contents: soil=" << setprecision(2) << theta[SOIL] << " ice=" << theta[ICE] << " water=" << theta[WATER] << " air=" << theta[AIR] << "\n";
	os << "\tGrains: gsz=2*rg=" <<  2.*rg << " ogs=" << ogs << " rb=" <<  rb << " dd=" <<  dd << " sp=" <<  sp << " N3=" << N3 << "\n";
	os << "\tMetamorphism: ps2rb=" << ps2rb << " metamo=" << metamo << " sw_abs=" << sw_abs << "\n";
	os << "\tMelting: dth_w=" << dth_w << " Qmf=" << Qmf << " res_wat_cont=" << res_wat_cont << "\n";
	os << "\tSoil: density=" << soil[SOIL_RHO] << " Conductivity=" << soil[SOIL_K] << " Capacity=" << soil[SOIL_C] << "\n";

	os << "\tStrains: S=" <<  S << " C=" << C << " s_strength=" << s_strength << "\n";
	os << "\tStrains: dEps=" << dEps << " Eps=" <<  Eps << " Eps_e=" <<  Eps_e << " Eps_v=" <<  Eps_v << "\n";
	os << "\tYoung's modulus of elasticity=" << E << "\n";
	os << "\tStrain rates Eps_Dot=" <<  Eps_Dot << " Eps_vDpt=" <<  Eps_vDot << " CDot=" <<  CDot << "\n";
	os << "\tStability: S_dr=" << S_dr << " hard=" << hard << " dsm=" << dsm << "\n";
	os << "</ElementData>\n";
	return os.str();
}

std::ostream& operator<<(std::ostream& os, const NodeData& data)
{
	os.write(reinterpret_cast<const char*>(&data.z), sizeof(data.z));
	os.write(reinterpret_cast<const char*>(&data.u), sizeof(data.u));
	os.write(reinterpret_cast<const char*>(&data.f), sizeof(data.f));
	os.write(reinterpret_cast<const char*>(&data.udot), sizeof(data.udot));
	os.write(reinterpret_cast<const char*>(&data.T), sizeof(data.T));
	os.write(reinterpret_cast<const char*>(&data.S_n), sizeof(data.S_n));
	os.write(reinterpret_cast<const char*>(&data.S_s), sizeof(data.S_s));
	os.write(reinterpret_cast<const char*>(&data.ssi), sizeof(data.ssi));
	os.write(reinterpret_cast<const char*>(&data.hoar), sizeof(data.hoar));

	os.write(reinterpret_cast<const char*>(&data.dsm), sizeof(data.dsm));
	os.write(reinterpret_cast<const char*>(&data.S_dsm), sizeof(data.S_dsm));
	os.write(reinterpret_cast<const char*>(&data.Sigdsm), sizeof(data.Sigdsm));
	return os;
}

std::istream& operator>>(std::istream& is, NodeData& data)
{
	is.read(reinterpret_cast<char*>(&data.z), sizeof(data.z));
	is.read(reinterpret_cast<char*>(&data.u), sizeof(data.u));
	is.read(reinterpret_cast<char*>(&data.f), sizeof(data.f));
	is.read(reinterpret_cast<char*>(&data.udot), sizeof(data.udot));
	is.read(reinterpret_cast<char*>(&data.T), sizeof(data.T));
	is.read(reinterpret_cast<char*>(&data.S_n), sizeof(data.S_n));
	is.read(reinterpret_cast<char*>(&data.S_s), sizeof(data.S_s));
	is.read(reinterpret_cast<char*>(&data.ssi), sizeof(data.ssi));
	is.read(reinterpret_cast<char*>(&data.hoar), sizeof(data.hoar));

	is.read(reinterpret_cast<char*>(&data.dsm), sizeof(data.dsm));
	is.read(reinterpret_cast<char*>(&data.S_dsm), sizeof(data.S_dsm));
	is.read(reinterpret_cast<char*>(&data.Sigdsm), sizeof(data.Sigdsm));
	return is;
}

const std::string NodeData::toString() const
{
	std::ostringstream os;
	os << std::fixed << std::showpoint;
	os << "<NodeData>\n";
	os << "\tz=" << z << " T=" << T << " hoar=" << hoar << "\n";
	os << "\tCreep: u=" << u << " udot=" << udot << " f=" << f << "\n";
	os << "\tStability: S_n=" << S_n << " S_s=" << S_s << " ssi=" << ssi << "\n";
	os << "</NodeData>\n";
	return os.str();
}

SnowStation::SnowStation(const bool& i_useCanopyModel, const bool& i_useSoilLayers, const bool& i_useSeaIceModule) :
	meta(), cos_sl(1.), sector(0), Cdata(), Seaice(NULL), pAlbedo(0.), Albedo(0.),
	SoilAlb(0.), SoilEmissivity(0.), BareSoil_z0(0.), SoilNode(0), Ground(0.),
	cH(0.), mH(0.), mass_sum(0.), swe(0.), lwc_sum(0.), hn(0.), rho_hn(0.), ErosionLevel(0), ErosionMass(0.),
	S_class1(0), S_class2(0), S_d(0.), z_S_d(0.), S_n(0.), z_S_n(0.),
	S_s(0.), z_S_s(0.), S_4(0.), z_S_4(0.), S_5(0.), z_S_5(0.),
	Ndata(), Edata(), Kt(NULL), ColdContent(0.), ColdContentSoil(0.), dIntEnergy(0.), dIntEnergySoil(0.), meltFreezeEnergy(0.), meltFreezeEnergySoil(0.), meltMassTot(0.), refreezeMassTot(0.),
	ReSolver_dt(-1), windward(false),
	WindScalingFactor(1.), TimeCountDeltaHS(0.),
	nNodes(0), nElems(0), maxElementID(0), useCanopyModel(i_useCanopyModel), useSoilLayers(i_useSoilLayers)
{
	if (i_useSeaIceModule)
		Seaice = new SeaIce;
	else
		Seaice = NULL;
}

SnowStation::SnowStation(const SnowStation& c) :
	meta(c.meta), cos_sl(c.cos_sl), sector(c.sector), Cdata(c.Cdata), Seaice(c.Seaice), pAlbedo(c.pAlbedo), Albedo(c.Albedo),
	SoilAlb(c.SoilAlb), SoilEmissivity(c.SoilEmissivity), BareSoil_z0(c.BareSoil_z0), SoilNode(c.SoilNode), Ground(c.Ground),
	cH(c.cH), mH(c.mH), mass_sum(c.mass_sum), swe(c.swe), lwc_sum(c.lwc_sum), hn(c.hn), rho_hn(c.rho_hn), ErosionLevel(c.ErosionLevel), ErosionMass(c.ErosionMass),
	S_class1(c.S_class1), S_class2(c.S_class2), S_d(c.S_d), z_S_d(c.z_S_d), S_n(c.S_n), z_S_n(c.z_S_n),
	S_s(c.S_s), z_S_s(c.z_S_s), S_4(c.S_4), z_S_4(c.z_S_4), S_5(c.S_5), z_S_5(c.z_S_5),
	Ndata(c.Ndata), Edata(c.Edata), Kt(NULL), ColdContent(c.ColdContent), ColdContentSoil(c.ColdContentSoil), dIntEnergy(c.dIntEnergy), dIntEnergySoil(c.dIntEnergySoil), meltFreezeEnergy(c.meltFreezeEnergy), meltFreezeEnergySoil(c.meltFreezeEnergySoil), meltMassTot(c.meltMassTot), refreezeMassTot(c.refreezeMassTot),
	ReSolver_dt(-1), windward(c.windward),
	WindScalingFactor(c.WindScalingFactor), TimeCountDeltaHS(c.TimeCountDeltaHS),
	nNodes(c.nNodes), nElems(c.nElems), maxElementID(c.maxElementID), useCanopyModel(c.useCanopyModel), useSoilLayers(c.useSoilLayers) {
	if (c.Seaice != NULL) {
		// Deep copy pointer to sea ice object
		Seaice = new SeaIce(*c.Seaice);
	} else {
		Seaice = NULL;
	}
}

SnowStation& SnowStation::operator=(const SnowStation& source) {
	if(this != &source) {
		meta = source.meta;
		cos_sl = source.cos_sl;
		sector = source.sector;
		Cdata = source.Cdata;
		if (source.Seaice != NULL) {
			// Deep copy pointer to sea ice object
			Seaice = new SeaIce(*source.Seaice);
		} else {
			Seaice = NULL;
		}
		pAlbedo = source.pAlbedo;
		Albedo = source.Albedo;
		SoilAlb = source.SoilAlb;
		SoilEmissivity = source.SoilEmissivity;
		BareSoil_z0 = source.BareSoil_z0;
		SoilNode = source.SoilNode;
		Ground = source.Ground;
		cH = source.cH;
		mH = source.mH;
		mass_sum = source.mass_sum;
		swe = source.swe;
		lwc_sum = source.lwc_sum;
		hn = source.hn;
		rho_hn = source.rho_hn;
		ErosionLevel = source.ErosionLevel;
		ErosionMass = source.ErosionMass;
		S_class1 = source.S_class1;
		S_class2 = source.S_class2;
		S_d = source.S_d;
		z_S_d = source.z_S_d;
		S_n = source.S_n;
		z_S_n = source.z_S_n;
		S_s = source.S_s;
		z_S_s = source.z_S_s;
		S_4 = source.S_4;
		z_S_4 = source.z_S_4;
		S_5 = source.S_5;
		z_S_5 = source.z_S_5;
		Ndata = source.Ndata;
		Edata = source.Edata;
		Kt = NULL;
		ColdContent = source.ColdContent;
		ColdContentSoil = source.ColdContentSoil;
		dIntEnergy = source.dIntEnergy;
		dIntEnergySoil = source.dIntEnergySoil;
		meltFreezeEnergy = source.meltFreezeEnergy;
		meltFreezeEnergySoil = source.meltFreezeEnergySoil;
		meltMassTot = source.meltMassTot;
		refreezeMassTot = source.refreezeMassTot;
		ReSolver_dt = source.ReSolver_dt;
		windward = source.windward;
		WindScalingFactor = source.WindScalingFactor;
		TimeCountDeltaHS = source.TimeCountDeltaHS;
		nNodes = source.nNodes;
		nElems = source.nElems;
		maxElementID = source.maxElementID;
		useCanopyModel = source.useCanopyModel;
		useSoilLayers = source.useSoilLayers;
	}
	return *this;
}

SnowStation::~SnowStation()
{
	SD_MATRIX_DATA* pMat = (SD_MATRIX_DATA*) Kt;

	if (pMat != NULL) {
		if ( pMat->State == ConMatrix ){
			ReleaseConMatrix(&pMat->Mat.Con);
		} else if ( pMat->State == BlockMatrix  ){
			ReleaseBlockMatrix(&pMat->Mat.Block);
		}
		free(pMat);
	}

	if (Seaice != NULL) {
		delete Seaice;
		Seaice = NULL;
	}
}

/**
* @brief Computes the actual total masses of the snowpack (kg m-2)
*/
void SnowStation::compSnowpackMasses()
{
	mass_sum = swe = lwc_sum = 0.;
	for (size_t e = SoilNode; e < nElems; e++) {
		mass_sum += Edata[e].M;
		swe += Edata[e].L * Edata[e].Rho;
		lwc_sum += Edata[e].L * ((Edata[e].theta[WATER] + Edata[e].theta[WATER_PREF]) * Constants::density_water);
	}
}

/**
 * @brief Computes the internal energy change of the snowpack during one computation time step (J m-2)
 */
void SnowStation::compSnowpackInternalEnergyChange(const double& sn_dt)
{
	if (nElems > SoilNode) {
		const double i_meltFreezeEnergy = meltFreezeEnergy;
		meltFreezeEnergy = 0.;
		const double i_cold_content = ColdContent;
		ColdContent = 0.;
		meltMassTot = 0.;
		refreezeMassTot = 0.;
		for (size_t e=SoilNode; e<nElems; e++) {
			meltMassTot += (Edata[e].Qmf < 0.) ? (-Edata[e].Qmf * Edata[e].L * sn_dt / Constants::lh_fusion) : (0.);
			refreezeMassTot += (Edata[e].Qmf > 0.) ? (Edata[e].Qmf * Edata[e].L * sn_dt / Constants::lh_fusion) : (0.);
			meltFreezeEnergy -= Edata[e].Qmf * Edata[e].L * sn_dt;
			ColdContent += Edata[e].coldContent();
		}
		dIntEnergy += (ColdContent - i_cold_content) + (meltFreezeEnergy - i_meltFreezeEnergy);
	} else {
                meltFreezeEnergy = 0.;
		ColdContent = 0.;
		dIntEnergy = 0.;
	}
}

/**
 * @brief Computes the internal energy change of the soil during one computation time step (J m-2)
 */
void SnowStation::compSoilInternalEnergyChange(const double& sn_dt)
{
	if (SoilNode > 0) {
		const double i_meltFreezeEnergy = meltFreezeEnergySoil;
		meltFreezeEnergySoil = 0.;
		const double i_cold_content = ColdContentSoil;
		ColdContentSoil = 0.;
		double tmp_sum_QIntmf = 0.;
		for (size_t e=0; e<SoilNode; e++) {
			tmp_sum_QIntmf += Edata[e].QIntmf * Edata[e].L * sn_dt;
			Edata[e].QIntmf=0.;
			meltFreezeEnergySoil -= Edata[e].Qmf * Edata[e].L * sn_dt;
			ColdContentSoil += Edata[e].coldContent();
		}
		dIntEnergySoil += (ColdContentSoil - i_cold_content) + (meltFreezeEnergySoil - i_meltFreezeEnergy) - tmp_sum_QIntmf;
	} else {
                meltFreezeEnergySoil = 0.;
		ColdContentSoil = 0.;
		dIntEnergySoil = 0.;
	}
}

/**
 * @brief Computes the liquid water index defined as the ratio of total liquid water content (in mm w.e.) to calculated snow depth (in mm) divided by 0.03. Unit: (1)
 */
double SnowStation::getLiquidWaterIndex() const
{
	return (cH > Constants::eps) ? lwc_sum / (M_TO_MM(cH) * 0.03) : Constants::undefined;
}

/**
 * @brief Returns modelled internal snow or/and soil temperature (instantaneous value; degC),
 *        at a given position z perpendicular to slope (m) \n
 *        z must be less than computed height (Xdata->cH), otherwise modeled temperature is set to Constants::undefined
 * @version 11.03
 * @param z Sensor position perpendicular to slope (m)
 */
double SnowStation::getModelledTemperature(const double& z) const
{
	if ((z == Constants::undefined) || !((getNumberOfNodes() > 1) && (z < cH))) {
		return Constants::undefined;
	} else {
		const size_t n_up = findUpperNode(z, Ndata, getNumberOfNodes()); // Upper node number
		const double z_low = (Ndata[n_up-1].z + Ndata[n_up-1].u); // Lower node around position z of sensor
		const double z_up = (Ndata[n_up].z + Ndata[n_up].u); // Upper node around position z of sensor
		const double T_low = Ndata[n_up-1].T;
		const double T_up = Ndata[n_up].T;
		const double T = T_low + (T_up-T_low)*(z-z_low)/(z_up-z_low);
		return IOUtils::K_TO_C( T );
	}
}

/**
 * @brief Reallocate element and node data \n
 * Edata and Ndata as well as nElems and nNodes are reallocated or reset, respectively.
 * @param number_of_elements The new number of elements
 */
void SnowStation::resize(const size_t& number_of_elements)
{
	const size_t nEdata_old = Edata.size();
	try {
		Edata.resize(number_of_elements, ElementData(ElementData::noID));
		Ndata.resize(number_of_elements + 1);
	} catch(const exception& e){
                std::cout << number_of_elements << std::endl;
		throw IOException(e.what(), AT); //this will catch all allocation exceptions
	}

	if (number_of_elements>nEdata_old) {
		for(size_t ii=nEdata_old; ii<Edata.size(); ii++) {
			if (Edata[ii].ID==ElementData::noID) Edata[ii].ID = ++maxElementID;
		}
	}

	nElems = Edata.size();
	nNodes = Ndata.size();
}

size_t SnowStation::getNumberOfElements() const
{
	return nElems;
}

size_t SnowStation::getNumberOfNodes() const
{
	return nNodes;
}

/**
 * @brief Find element with corresponding tag or return IOUtils::npos if not found
 * @param tag Tag to look for
 * @return Index of tagged element, IOUtils::npos if not found
 */
size_t SnowStation::find_tag(const size_t& tag) const
{
	for (size_t e=0; e<nElems; e++) {
		if (Edata[e].mk/100 == tag) {
			return e;
		}
	}

	return IOUtils::npos;
}

bool SnowStation::hasSoilLayers() const
{
	return useSoilLayers;
}

/**
 * @brief If more than NUMBER_TOP_ELEMENTS snow elements exist, attempt to reduce their number in the FEM mesh,
 * leaving NUMBER_TOP_ELEMENTS surface elements untouched \n
 * Pairs of elements within the snow cover satisfying the conditions of combineCondition() are combined
 * by placing everything in the lower element, setting the density of upper element to Constants::undefined,
 * and getting rid of node in between. \n
 * The elements being very similar and thus the microstructure parameters being approximately equal
 * as defined in combineCondition(), simply average the microstructure properties \n
 * NOTE that the condense element check is placed at the end of a time step, allowing elements do develop on their own.
 * @param i_number_top_elements The number of surface elements to be left untouched
 * @param reduce_n_elements When >0: enable more "aggressive" combining for layers deeper in the snowpack, to reduce the number of elements and thus the computational load. Values >=1 denote levels of aggressivity.
 * @param cond Condition to use to determine whether or not to combine: 1 = combineCondition, 2 = Aggregate::joinSimilarLayers, 3 = Aggregate::mergeThinLayer
 * @param comb_thresh_l Only used for cond == 1: both elements must be smaller than this value for an action to be taken.
 */
void SnowStation::combineElements(const size_t& i_number_top_elements, const int& reduce_n_elements, const size_t& cond, const double& comb_thresh_l)
{
	if (nElems - SoilNode < i_number_top_elements+1) {
		return;
	}

	size_t nRemove=0;       // Number of elements to be removed
	bool merge=false;
	for (size_t eLower = SoilNode, eUpper = SoilNode+1; eLower < nElems-i_number_top_elements; eLower++, eUpper++) {
		switch (cond) {
			case 1:	// merging WaterTransport
				merge = (combineCondition(Edata[eLower], Edata[eUpper], cH-Ndata[eUpper].z, reduce_n_elements, comb_thresh_l));
				break;
			case 2:	// aggregate first round
				merge = (Aggregate::joinSimilarLayers(Edata[eUpper], Edata[eLower]));
				break;
			case 3:	// aggregate second round
				merge = (Aggregate::mergeThinLayer(Edata[eUpper], Edata[eLower]));
				break;
			default:
				merge = false;
				break;
		}
		if (merge) {
			mergeElements(Edata[eLower], Edata[eUpper], true, (eUpper==nElems-1));
			nRemove++;
			Edata[eUpper].Rho = Constants::undefined;
			eLower++; eUpper++;
		}
	}
	if (nRemove > 0) {
		const size_t rnE = nElems - nRemove; //Reduced number of elements
		reduceNumberOfElements(rnE);
	}
}

/**
 * @brief Remove the upper "marked" element of two (snow only) \n
 * -# Merging two elements:
 *     -# density is undefined
 *     -# take the uppermost node of both
 * -# Removing melted or thin elements
 *     -# density is undefined AND length negative (*= -1.) as the latter will be used!
 *     -# keep upper node of lowest element
 * @param rnE Reduced number of elements
 */
void SnowStation::reduceNumberOfElements(const size_t& rnE)
{
	size_t eNew = SoilNode; // New element index
	double dL=0.;

	for (size_t e = SoilNode; e < nElems; e++) {
		if (Edata[e].Rho == Constants::undefined) {
			if (Edata[e].L > 0.0) { // Merging elements
				Ndata[eNew] = Ndata[e+1];
				Ndata[eNew].z = Ndata[e+1].z + Ndata[e+1].u + dL;
				Ndata[eNew].u = Ndata[e].udot = 0.;
				Ndata[eNew].ssi = Ndata[e+1].ssi;
				Ndata[eNew].S_s = Ndata[e+1].S_s;
				Ndata[eNew].S_n = Ndata[e+1].S_n;
			} else { // Removing elements for negative length L
				dL += Edata[e].L;
			}
		} else {
			if (eNew < e) {
				Edata[eNew] = Edata[e];
				Ndata[eNew+1] = Ndata[e+1];
			}
			Ndata[eNew+1].z = Ndata[e+1].z + Ndata[e+1].u + dL;
			Ndata[eNew+1].u = Ndata[e+1].udot = 0.;
			Ndata[eNew+1].ssi = Ndata[e+1].ssi;
			Ndata[eNew+1].S_s = Ndata[e+1].S_s;
			Ndata[eNew+1].S_n = Ndata[e+1].S_n;
			eNew++;
		}
	}

	resize(rnE);

	const double cH_old = cH;
	cH = Ndata[nNodes-1].z + Ndata[nNodes-1].u;
	if (mH!=Constants::undefined) mH -= (cH_old - cH);
	ErosionLevel = std::max(SoilNode, std::min(ErosionLevel, rnE-1));
}

/**
 * @brief This routine initializes the snow cover structure which contains all information about a station
 * including element, nodal and canopy data \n
 * Because you are working with layers, the first node is a special case; the rest can be generated in a loop ....
 * The bottom temperature at the beginning of the computation is given by the temperature at the top of the
 * lowest soil or snow layer \n
 * IMPORTANT: it is very important for Alpine3D that Cdata.height is initialized even if CANOPY = 0,
 * otherwise SnowInterface will not recognize the canopy grids (David 2007-06-25).
 * @version 10.02
 * @param SSdata
 * @param i_sector defines the exposition sector of the slope (width 360./number_slopes)
 */
void SnowStation::initialize(const SN_SNOWSOIL_DATA& SSdata, const size_t& i_sector)
{
	Albedo = SSdata.Albedo;
	SoilAlb = SSdata.SoilAlb;
	BareSoil_z0 = SSdata.BareSoil_z0;

	SoilEmissivity = (SSdata.Emissivity_soil == mio::IOUtils::nodata) ? Constants::emissivity_soil : SSdata.Emissivity_soil;

	WindScalingFactor = SSdata.WindScalingFactor;
	TimeCountDeltaHS = SSdata.TimeCountDeltaHS;

	meta = SSdata.meta;
	cos_sl = cos(meta.getSlopeAngle()*mio::Cst::to_rad);
	sector = i_sector;

	mH = cH = SSdata.Height;

	nNodes = SSdata.nN;
	nElems = SSdata.nN-1;
	resize(nElems);

	SoilNode = 0;
	Ground = 0.0;
	Ndata.front().z = 0.;
	Ndata.front().T = (SSdata.nLayers > 0)? SSdata.Ldata.front().tl : Constants::meltfreeze_tk;
	Ndata.front().u = 0.;
	Ndata.front().f = 0.;
	Ndata.front().udot = 0.;
	Ndata.front().hoar = 0.;
	Ndata.front().S_n=6.;   // Interface static natural stability index
	Ndata.front().S_s=6.;   // Interface stability index Sk38 (skier)

	bool real_soil_no_sandwich = true;  // Switch to count real soil layers
	for (size_t ll = 0, n = 1; ll < SSdata.nLayers; ll++) {
		// Update ground heigth and SoilNode number
		if (SSdata.Ldata[ll].phiSoil > 0.0 && real_soil_no_sandwich) {
			Ground += SSdata.Ldata[ll].hl;
			SoilNode += SSdata.Ldata[ll].ne;
		} else {
			real_soil_no_sandwich = false;
		}

		const double dT = (ll>0)? (SSdata.Ldata[ll].tl - SSdata.Ldata[ll-1].tl) / static_cast<double>(SSdata.Ldata[ll].ne) : 0.;

		for (size_t le = 0; le < SSdata.Ldata[ll].ne; le++, n++ ) {
			Ndata[n].z = Ndata[n-1].z + SSdata.Ldata[ll].hl / static_cast<double>(SSdata.Ldata[ll].ne);
			Ndata[n].T = Ndata[n-1].T + dT;
			Ndata[n].u = 0.;
			Ndata[n].f = 0.;
			Ndata[n].udot = 0.;
			Ndata[n].S_n = INIT_STABILITY;   // Static natural stability index
			Ndata[n].S_s = INIT_STABILITY;   // Alternative Stability Index (skier stability)
		}
	}

	if (SoilNode == 0 && useSoilLayers) {
		prn_msg(__FILE__, __LINE__, "err", Date(), "SNP_SOIL set but no soil layers given");
		throw IOException("Snowpack Initialization failed", AT);
	}

	// INITIALIZE THE ELEMENT DATA
	for (size_t ll = 0, e = 0; ll<SSdata.nLayers; ll++) {
		for (size_t le = 0; le < SSdata.Ldata[ll].ne; le++, e++) {
			// Element's JulianQ Date
			Edata[e].depositionDate = Date::rnd(SSdata.Ldata[ll].depositionDate, 1.);
			// Temperature data
			Edata[e].Te = (Ndata[e].T+Ndata[e+1].T) / 2.;
			Edata[e].L0 = Edata[e].L = (Ndata[e+1].z - Ndata[e].z);
			Edata[e].gradT = (Ndata[e+1].T-Ndata[e].T) / Edata[e].L;
			// Creep data
			Edata[e].E = Edata[e].Eps = Edata[e].S = Edata[e].Eps_Dot=0.0;
			Edata[e].Eps_v = Edata[e].Eps_e = Edata[e].Eps_vDot=0.0;
			// Very important to initialize the increments in length and strain
			Edata[e].dEps = 0.0;
			// Volumetric Components
			Edata[e].theta[SOIL]  = SSdata.Ldata[ll].phiSoil;
			Edata[e].theta[AIR]   = SSdata.Ldata[ll].phiVoids;
			Edata[e].theta[ICE]   = SSdata.Ldata[ll].phiIce;
			Edata[e].theta[WATER] = SSdata.Ldata[ll].phiWater;
			Edata[e].theta[WATER_PREF] = SSdata.Ldata[ll].phiWaterPref;
			Edata[e].soil[SOIL_RHO] = SSdata.Ldata[ll].SoilRho;
			Edata[e].soil[SOIL_K]   = SSdata.Ldata[ll].SoilK;
			Edata[e].soil[SOIL_C]   = SSdata.Ldata[ll].SoilC;
			for (size_t ii = 0; ii < SnowStation::number_of_solutes; ii++) {
				Edata[e].conc[SOIL][ii]  = SSdata.Ldata[ll].cSoil[ii];
				Edata[e].conc[ICE][ii]  = SSdata.Ldata[ll].cIce[ii];
				Edata[e].conc[WATER][ii] = SSdata.Ldata[ll].cWater[ii];
				Edata[e].conc[AIR][ii]  = SSdata.Ldata[ll].cVoids[ii];
			}
			Edata[e].updDensity();
			assert(Edata[e].Rho >= 0. || Edata[e].Rho==IOUtils::nodata); //we want positive density
			// conductivities, specific heat and moisture content
			Edata[e].k[TEMPERATURE] = Edata[e].k[SEEPAGE] = Edata[e].k[SETTLEMENT] = 0.;
			Edata[e].heatCapacity();
			Edata[e].c[SEEPAGE] = Edata[e].c[SETTLEMENT] = 0.;
			Edata[e].snowResidualWaterContent();
			// Set the initial short wave radiation to zero
			Edata[e].sw_abs = 0.;
			// Phase change variables
			Edata[e].Qmf = 0.;
			Edata[e].QIntmf = 0.;
			Edata[e].dth_w = 0.;
			// Micro-structure data
			Edata[e].dd = SSdata.Ldata[ll].dd;
			Edata[e].sp = SSdata.Ldata[ll].sp;
			Edata[e].rg = SSdata.Ldata[ll].rg;
			Edata[e].opticalEquivalentGrainSize();
			Edata[e].rb = SSdata.Ldata[ll].rb;
			Edata[e].N3 = Metamorphism::getCoordinationNumberN3(Edata[e].Rho);
			Edata[e].mk = SSdata.Ldata[ll].mk;
			Edata[e].snowType();
			Ndata[e+1].hoar = SSdata.Ldata[ll].hr;
			// Memories, memories
			Edata[e].CDot = SSdata.Ldata[ll].CDot;
			Edata[e].metamo = SSdata.Ldata[ll].metamo;
			Edata[e].salinity = SSdata.Ldata[ll].salinity;
			Edata[e].h = SSdata.Ldata[ll].h;
			Edata[e].S_dr = INIT_STABILITY;
			Edata[e].hard = IOUtils::nodata;
			Edata[e].M = Edata[e].Rho * Edata[e].L0;
			assert(Edata[e].M >= (-Constants::eps2)); //mass must be positive

			// Check if pore space is available when water would freeze
			const double porespace = (1. - Edata[e].theta[ICE] - Edata[e].theta[SOIL]) * (Constants::density_ice / Constants::density_water);
			if(Edata[e].theta[WATER] + Edata[e].theta[WATER_PREF] > porespace) {
				const double tmp_sum = Edata[e].theta[WATER] + Edata[e].theta[WATER_PREF];
				Edata[e].theta[WATER] *= porespace / tmp_sum;
				Edata[e].theta[WATER_PREF] *= porespace / tmp_sum;
			}
			Edata[e].theta[AIR] = (1. - Edata[e].theta[ICE] - Edata[e].theta[WATER] - Edata[e].theta[WATER_PREF] - Edata[e].theta[SOIL]);
		} // end of element layer for
	} // end of layer for

	ErosionLevel = (SSdata.ErosionLevel > 0)? static_cast<size_t>(SSdata.ErosionLevel) : std::max(SoilNode, nElems-1);

	// Find the real Cauchy stresses
	double SigC = 0.0;
	for(size_t e = nElems; e -->0; ) {
		if (e < nElems-1)
			SigC -= (.5*Edata[e+1].M) * Constants::g * cos_sl;
		SigC -= (.5*Edata[e].M) * Constants::g * cos_sl;

		Edata[e].C = SigC;
		assert(Edata[e].C<0.);
	}

	// Sea ice initializations
	if (Seaice != NULL) {
		Seaice->updateFreeboard(*this);
		for (size_t e = nElems; e -->0; ) {
			const double br_sal = (Edata[e].theta[WATER] + Edata[e].theta[WATER_PREF] == 0.) ? (0.) : (Edata[e].salinity / (Edata[e].theta[WATER] + Edata[e].theta[WATER_PREF]));
			if (Edata[e].salinity > 0.) {
				Edata[e].meltfreeze_tk = -SeaIce::mu * br_sal + Constants::meltfreeze_tk;
			}
			if (Edata[e].h == Constants::undefined) {
				Edata[e].h = Seaice->SeaLevel - .5 * (Ndata[e].z + Ndata[e+1].z);
			} else {
				// Initialize 
				if (e >= SoilNode) {		//Snow
					Edata[e].VG.SetVGParamsSnow(vanGenuchten::YAMAGUCHI2012, vanGenuchten::CALONNE, /*matrix*/ true, /*seaice*/ true);
				} else {			//Soil
					Edata[e].VG.SetVGParamsSoil();
				}
				// If pressure head indicates full saturation, make sure no rounding errors exists from writing/reading sno files.
				if (Edata[e].h >= Edata[e].VG.h_e) {
					Edata[e].theta[WATER] = (1. - Edata[e].theta[ICE] - Edata[e].theta[SOIL]) * (Constants::density_ice / Constants::density_water) - Edata[e].theta[WATER_PREF];
					Edata[e].theta[AIR] = 1. - Edata[e].theta[ICE] - Edata[e].theta[WATER] - Edata[e].theta[WATER_PREF] - Edata[e].theta[SOIL];
					Edata[e].updDensity();
				}
			}
		}
	}

	// Cold content and snowpack masses
	compSnowpackInternalEnergyChange(60.); // Time (900 s) will not matter as Qmf == 0. for all layers
	compSoilInternalEnergyChange(60.); // Time (900 s) will not matter as Qmf == 0. for all layers
	compSnowpackMasses();

	Cdata.initialize(SSdata, useCanopyModel);

	// Set time step to -1, so we can determine the first time ReSolver1d is called.
	ReSolver_dt = -1.;
}

/**
 * @brief Determine flexible maximum element length for combining two elements
 * - Function required for REDUCE_N_ELEMENTS function for "aggressive" combining for layers deeper in the
 *   snowpack, to reduce the number of elements and thus the computational load.
 * @param depth Distance of the element from the snow surface
 * @return Maximum element length.
 */
double SnowStation::flexibleMaxElemLength(const double& depth, const double& comb_thresh_l)
{
	static const double upper_limit_length=1.0;
	const double calc_length = static_cast<double>( int( int(depth * 100.) / 10) + 1) * comb_thresh_l;
	return std::min(calc_length, upper_limit_length);
}

/**
 * @brief Boolean routine to check whether two snow elements can be combined
 * - \b no \b action will be taken if one of the two elements is
 *      - a soil element
 *      - larger than comb_thresh_l
 *      - tagged
 *      - dry surface hoar (mk=3)
 *      - dendritic but not both
 * - \b otherwise we use criteria for dendricity, sphericity, volumetric ice or water content, grain size and marker
 * - Whatever type of thin elements are treated in WaterTransport::mergingElements()
 *
 * @param Edata0 Lower element
 * @param Edata1 Upper element
 * @param depth Distance of the element from the snow surface
 * @param reduce_n_elements When >0: enable more "aggressive" combining for layers deeper in the snowpack, to reduce the number of elements and thus the computational load. Values >=1 denote levels of aggressivity.
 * @return true if the two elements should be combined, false otherwise
 */
bool SnowStation::combineCondition(const ElementData& Edata0, const ElementData& Edata1, const double& depth, const int& reduce_n_elements, const double& comb_thresh_l)
{
	// Default max_elem_l
	double max_elem_l = comb_thresh_l;

	// When aggressive combining is activated, override max_elem_l when necessary
	if (reduce_n_elements > 0) {
		max_elem_l = flexibleMaxElemLength(depth, comb_thresh_l);
	}

	if ( (Edata0.L > max_elem_l) || (Edata1.L > max_elem_l) )
		return false;

	if ( (Edata0.mk%100 != Edata1.mk%100) && (depth < 10. || reduce_n_elements == 1) )
		return false;

	double comb_thresh_sp_flex;
	if (reduce_n_elements <= 1) {
		comb_thresh_sp_flex = comb_thresh_sp;
	} else {
		if (depth <= 10.) {
			comb_thresh_sp_flex = comb_thresh_sp;
		} else if (depth >= 20.) {
			comb_thresh_sp_flex = 1.01;
		} else {
			comb_thresh_sp_flex = (((1.01 - comb_thresh_sp) / 10.) * depth) + (2. * comb_thresh_sp - 1.01);
		}
	}

	if ( fabs(Edata0.sp - Edata1.sp) > comb_thresh_sp_flex )
		return false;

	if ( Edata0.theta[SOIL] > 0. || Edata1.theta[SOIL] > 0. )
		return false;

	if ( (Edata0.mk >= 100 && int(Edata0.mk/1000)!=9) || (Edata1.mk >= 100 && int(Edata0.mk/1000)!=9) )
		return false;

	if ( (Edata0.mk%100 == 3) || (Edata1.mk%100 == 3) )
		return false;

	if ( (Edata0.dd > comb_thresh_dd || Edata1.dd > comb_thresh_dd) &&
		!(Edata0.dd > comb_thresh_dd && Edata1.dd > comb_thresh_dd) ) {
		return false;
	} else if ( fabs(Edata0.dd - Edata1.dd) > comb_thresh_dd ) {
		return false;
	}

	double comb_thresh_ice_flex;
	if (reduce_n_elements <= 1) {
		comb_thresh_ice_flex = comb_thresh_ice;
	} else {
		if (depth <= 50.) {
			comb_thresh_ice_flex = comb_thresh_ice;
		} else if (depth >= 150.) {
			comb_thresh_ice_flex = comb_thresh_ice * 5.;
		} else {
			comb_thresh_ice_flex = (((4. * comb_thresh_ice) / 100.) * depth) - comb_thresh_ice;
		}
	}

	if ( fabs(Edata0.theta[ICE] - Edata1.theta[ICE]) > comb_thresh_ice_flex )
		return false;

	if ( fabs(Edata0.theta[WATER] - Edata1.theta[WATER]) > comb_thresh_water )
		return false;

	double comb_thresh_rg_flex;
	if (reduce_n_elements <= 1) {
		comb_thresh_rg_flex = comb_thresh_rg;
	} else {
		if (depth <= 10.) {
			comb_thresh_rg_flex = comb_thresh_rg;
		} else if ((depth > 10.) && (depth <= 50.)) {
			comb_thresh_rg_flex = ((4. * comb_thresh_rg / 40.) * depth);
		} else if ((depth > 50.) && (depth <= 150.)) {
			comb_thresh_rg_flex = ((45. * comb_thresh_rg / 100.) * depth) - (17.5 * comb_thresh_rg);
		} else {
			comb_thresh_rg_flex = comb_thresh_rg * 50.;
		}
	}

	if ( fabs(Edata0.rg - Edata1.rg) > comb_thresh_rg_flex )
		return false;

	return true;
}

/**
 * @brief Split the element provided as first argument.
 */
void SnowStation::splitElement(const size_t& e)
{
	resize(nElems+1);
	if(e!=nElems-2) { // If it is not the top node that needs splitting ...     (Note that we have to reference nElems-2, as resize has been called (thus nElems increased) already.)
		// then shift all elements and nodes above upward
		for(size_t ee = nElems-1; ee >= e+2; ee--) {
			Edata[ee]=Edata[ee-1];
			Ndata[ee+1]=Ndata[ee];
			Ndata[ee]=Ndata[ee-1];
		}
	}
	// Fill info of new element
	Edata[e+1]=Edata[e];
	// Half the element
	Edata[e].L*=0.5;
	Edata[e].L0*=0.5;
	Edata[e+1].L*=0.5;
	Edata[e+1].L0*=0.5;
	Edata[e].M*=0.5;
	Edata[e+1].M*=0.5;
	// Fill info of new node
	Ndata[e+2]=Ndata[e+1];
	Ndata[e+1].hoar=0.;
	Ndata[e+1].T=Edata[e].Te;
	// Position the new node correctly in the domain
	Ndata[e+1].z=(Ndata[e+2].z+Ndata[e].z)/2.;
	Ndata[e+2].u*=0.5;
	Ndata[e+1].u*=0.5;
	// Correct pressure head in case of saturation
	if(Edata[e].h > Edata[e].VG.h_e) {
		Edata[e].h+=.5*Edata[e].L;
		Edata[e+1].h-=.5*Edata[e+1].L;
	}
}

/**
 * @brief Split elements when they are near the top of the snowpack, when REDUCE_N_ELEMENTS is used.
 * - This function split elements when they are getting closer to the top of the snowpack. This is required
 *   when using the "aggressive" merging option (REDUCE_N_ELEMENTS). When snow melt brings elements back to the
 *   snow surface, smaller layer spacing is required to accurately describe temperature and moisture gradients.
 * @param max_element_length If positive: maximum allowed element length (m), above which splitting is applied.
 *                           If argument is not positive: use function flexibleMaxElemLength.
 */
void SnowStation::splitElements(const double& max_element_length, const double& comb_thresh_l)
{
	//Return when no snow present
	if (nElems == SoilNode) return;

	for (size_t e = SoilNode; e < nElems; e++) {
		double max_elem_l = comb_thresh_l;
		const double depth = cH - Ndata[e].z;
		// If max_element_length > 0: take its value, else use function flexibleMaxElemLength.
		max_elem_l = (max_element_length > 0) ? (0.5 * max_element_length) : (flexibleMaxElemLength(depth, comb_thresh_l));
		if(0.5*(Edata[e].L) > max_elem_l) {
			splitElement(e);
			e--;			// Make sure the same element gets checked again, in case 1 split is not sufficient
		}
	}
}

/**
 * @brief Keep simulated snow depth within MAX_SIMULATED_HS range.
 * - This function deletes elements at the bottom of the snowpack when the simulated snow depth exceeds max_simulated_hs.
 *   Useful for simulations of Firn, in studies where one is only interested in the near surface processes.
 *   Note that this function will always keep at least one snow element.
 * @param max_simulated_hs If positive: maximum allowed modelled snow depth, above which elements will be removed.
 */
void SnowStation::CheckMaxSimHS(const double& max_simulated_hs) {
	if(max_simulated_hs > 0. && cH > max_simulated_hs) {
		double tmp_height = 0.;
		size_t e = nElems;
		for ( ; e-- > SoilNode; ) {
			// Find which element exceeds the max_simulated_hs
			tmp_height += Edata[e].L;
			if (tmp_height > max_simulated_hs) {
				if ( e+1 < nElems ) { //Check for the case where the top element L already exceeds max_simulated_hs, in case we skip element deletion.
					size_t i_offset = e+1;				// The new indexing offset
					size_t i = i_offset;
					const double dH = Ndata[i].z;			// Height change of domain
					for ( ; i < nElems; i++) {			// Go from the element above the one that was exceeding max_simulated_hs to the top element
						Edata[i-i_offset] = Edata[i];		// Shift all the elements down
						Ndata[i-i_offset] = Ndata[i];		// Shift the node down
						Ndata[i-i_offset].z -= dH;		// Correct nodal position
					}
					Ndata[nElems-i_offset] = Ndata[nElems];		// Take care of the remaining top node
					resize(i-i_offset);
					cH -= dH;					// Correct calculated snow height
				}
				break;
			}
		}
	}
	return;
}

/**
 * @brief Merging two elements
 * - Joining:
 * 	- Keep the lower element, that is, the lowest snow element always survives!
 * 	- The properties of the upper and lower elements are (depth) averaged in an appropriate way.
 * 	- The new length is the sum of both
 * 	- Keep the birthday of the upper element
 * 	- Keep the tag of the upper element only if the lower is untagged (mk >= 100)
 * 	- @note Joining two elements may cause the tag (marker >= 100) to "jump" abruptly
 * - Removing:
 * 	- Remaining ice, liquid water, solutes, etc. are added to the lower element
 * 	- The length of the lower element is kept
 * 	- Keep the birthday of the lower element
 * @param EdataLower Properties of lower element
 * @param EdataUpper Properties of upper element
 * @param merge True if upper element is to be joined with lower one, false if upper element is to be removed
 * @param topElement set to true if the upper element is at the very top of the snow pack
 */
void SnowStation::mergeElements(ElementData& EdataLower, const ElementData& EdataUpper, const bool& merge, const bool& topElement)
{
	const double L_lower = EdataLower.L; //Thickness of lower element
	const double L_upper = EdataUpper.L; //Thickness of upper element
	double LNew = L_lower;               //Thickness of "new" element

	if (merge) {
		// Determine new element length under the condition of keeping the density of the lower element constant, if the density of the lower element is larger than the upper element.
		// This is only in case we are considering the top element, to deal with the common situation where top elements are being removed due to low
		// ice content as a result of melt. We don't want to transfer this low ice content to lower layers.
		if (EdataUpper.Rho != Constants::undefined && EdataLower.Rho != Constants::undefined && EdataUpper.Rho < EdataLower.Rho && topElement==true) {	// Check if densities are defined, which may not be the case if elements are already marked for removal (may happen when removing multiple adjacent elements).
			LNew += (EdataUpper.Rho * L_upper) / EdataLower.Rho;
		} else {
			LNew += L_upper;
		}
		EdataLower.depositionDate = EdataUpper.depositionDate;
		if (EdataLower.theta[ICE] + EdataUpper.theta[ICE] > 0.) {
			EdataLower.dd = ( EdataLower.theta[ICE]*L_lower*EdataLower.dd + EdataUpper.theta[ICE]*L_upper*EdataUpper.dd ) / (EdataLower.theta[ICE]*L_lower + EdataUpper.theta[ICE]*L_upper);
			EdataLower.sp = ( EdataLower.theta[ICE]*L_lower*EdataLower.sp + EdataUpper.theta[ICE]*L_upper*EdataUpper.sp ) / (EdataLower.theta[ICE]*L_lower + EdataUpper.theta[ICE]*L_upper);
			EdataLower.rg = ( EdataLower.theta[ICE]*L_lower*EdataLower.rg + EdataUpper.theta[ICE]*L_upper*EdataUpper.rg ) / (EdataLower.theta[ICE]*L_lower + EdataUpper.theta[ICE]*L_upper);
			EdataLower.rb = ( EdataLower.theta[ICE]*L_lower*EdataLower.rb + EdataUpper.theta[ICE]*L_upper*EdataUpper.rb ) / (EdataLower.theta[ICE]*L_lower + EdataUpper.theta[ICE]*L_upper);
			EdataLower.CDot = ( EdataLower.theta[ICE]*L_lower*EdataLower.CDot + EdataUpper.theta[ICE]*L_upper*EdataUpper.CDot ) / (EdataLower.theta[ICE]*L_lower + EdataUpper.theta[ICE]*L_upper);
		}
		EdataLower.opticalEquivalentGrainSize();
		EdataLower.Eps = EdataLower.Eps_v; //HACK: why?
		EdataLower.Eps_e = 0.0; // TODO (very old) Check whether not simply add the elastic
		                     //                 and viscous strains of the elements and average the stress? E is kept from Lower
	} else {
		EdataLower.E = EdataLower.Eps_e = EdataLower.Eps = EdataLower.Eps_v = EdataLower.dEps = 0.0;
	}

	EdataLower.L0 = EdataLower.L = LNew;
	EdataLower.M += EdataUpper.M;
	EdataLower.theta[ICE] = (L_upper*EdataUpper.theta[ICE] + L_lower*EdataLower.theta[ICE]) / LNew;
	EdataLower.theta[WATER] = (L_upper*EdataUpper.theta[WATER] + L_lower*EdataLower.theta[WATER]) / LNew;
	EdataLower.theta[WATER_PREF] = (L_upper*EdataUpper.theta[WATER_PREF] + L_lower*EdataLower.theta[WATER_PREF]) / LNew;
	EdataLower.theta[AIR] = 1.0 - EdataLower.theta[WATER] - EdataLower.theta[WATER_PREF] - EdataLower.theta[ICE] - EdataLower.theta[SOIL];
	EdataLower.salinity = (L_upper * EdataUpper.salinity + L_lower * EdataLower.salinity) / LNew;
	// For snow, check if there is enough space to store all ice if all water would freeze. This also takes care of cases where theta[AIR]<0.
	if ((merge==false && topElement==true) && EdataLower.theta[SOIL]<Constants::eps2 && EdataLower.theta[AIR] < (EdataLower.theta[WATER]+EdataLower.theta[WATER_PREF])*((Constants::density_water/Constants::density_ice)-1.)) {
		// Note: we can only do this for the uppermost snow element, as otherwise it is not possible to adapt the element length.
		// If there is not enough space, adjust element length:
		EdataLower.theta[AIR] = (EdataLower.theta[WATER]+EdataLower.theta[WATER_PREF])*((Constants::density_water/Constants::density_ice)-1.);
		const double tmpsum = EdataLower.theta[AIR]+EdataLower.theta[ICE]+EdataLower.theta[WATER]+EdataLower.theta[WATER_PREF];
		// Ensure that the element does not become larger than the sum of lengths of the original ones (no absolute element "growth")!
		LNew = std::min(LNew * tmpsum, L_lower + L_upper);
		EdataLower.L0 = EdataLower.L = LNew;
		EdataLower.theta[AIR] /= tmpsum;
		EdataLower.theta[ICE] /= tmpsum;
		EdataLower.theta[WATER] /= tmpsum;
		EdataLower.theta[WATER_PREF] /= tmpsum;
	}
	EdataLower.snowResidualWaterContent();
	EdataLower.updDensity();

	for (size_t ii = 0; ii < SnowStation::number_of_solutes; ii++) {
		for (size_t kk = 0; kk < N_COMPONENTS; kk++) {
			EdataLower.conc(kk,ii) = (L_upper*EdataUpper.conc(kk,ii) + L_lower*EdataLower.conc(kk,ii)) / LNew;
		}
	}
	EdataLower.dth_w = (L_upper*EdataUpper.dth_w + L_lower*EdataLower.dth_w) / LNew;
	EdataLower.Qmf = (EdataUpper.Qmf*L_upper + EdataLower.Qmf*L_lower) / LNew;	//Note: Qmf has units W/m^3, so it needs to be scaled with element lengths.
	EdataLower.sw_abs += EdataUpper.sw_abs;
	if ((EdataUpper.mk >= 100) && (EdataLower.mk < 100)) {
		EdataLower.mk += static_cast<short unsigned int>( (EdataUpper.mk/100)*100 );
	}
	EdataLower.heatCapacity();
}

/**
 * @brief returns if a snow profile can be considered as a glacier.
 * Practically, the hydrological criteria is that if a pixel contains more than 2 m
 * of pure ice anywhere, it is considered to be glaciated. The standard criteria is that
 * if the top 5 layers are made of pure ice, the pixel is glaciated.
 * Therefore, a glacier covered by seasonal snow is glaciated in regard to the hydrological criteria
 * but non-glaciated in regard to the standard criteria.
 * @param hydro if true, use an hydrologist criteria (default: false)
 * @return true if the profile belongs to a glacier
 */
bool SnowStation::isGlacier(const bool& hydro) const
{
	if (hydro) {
		//if more than 2m of pure ice in the whole profile -> hydrologically, glacier melt
		static const double ice_depth_glacier = 2.;
		double sum_ice_depth=0.;
		for (size_t layer_index=0; layer_index<nElems; layer_index++) {
			if ((Edata[layer_index].type==880) || (Edata[layer_index].mk % 10 == 7) || (Edata[layer_index].mk % 10 == 8))
				sum_ice_depth += Edata[layer_index].L;
		}

		return (sum_ice_depth>=ice_depth_glacier);
	} else {
		bool is_pure_ice=true;
		static const size_t check_depth=5;
		const size_t top_index = nElems-1;
		const size_t top_index_toCheck = top_index - check_depth;
		const size_t soil_index = SoilNode-1;
		const size_t end_index = (top_index_toCheck>soil_index)? top_index_toCheck : soil_index;

		if (nElems==0 || top_index==soil_index) return false; //there are only soil layers or none

		for (size_t layer_index=top_index+1; layer_index-- > end_index; ) { //because it is decremented right away when testing...
			const bool is_ice = (Edata[layer_index].type==880) || (Edata[layer_index].mk % 10 == 7) || (Edata[layer_index].mk % 10 == 8);
			if (!is_ice)  {
				is_pure_ice=false;
				break;
			}
		}

		return is_pure_ice;
	}
}

/**
 * @brief returns the height of a marked reference layer inside the model domain
 * Searches for the layer that is marked using (int(mk/1000)==9, e.g. 9000 or 9028) inside model domain
 * This is for example used to interpret snow height measurements with an arbitrary reference level
 * (i.e., not necessarily 0.) on a glacier, ice sheets or sea ice using the snow height driven mode.
 * @return height of top node of marked reference layer
 */
double SnowStation::findMarkedReferenceLayer() const
{
	if(nElems == 0) {
		return Constants::undefined;
	}
	for (size_t e = SoilNode; e < nElems; e++) {
		if (int(Edata[e].mk/1000) == 9) {
			return Ndata[e+1].z;
		}
	}
	return Constants::undefined;
}

std::ostream& operator<<(std::ostream& os, const SnowStation& data)
{
	os << data.meta;
	os.write(reinterpret_cast<const char*>(&data.cos_sl), sizeof(data.cos_sl));
	os.write(reinterpret_cast<const char*>(&data.sector), sizeof(data.sector));

	os << data.Cdata;
	//os << data.Seaice;	//HACK how to do this with a pointer?
	os.write(reinterpret_cast<const char*>(&data.pAlbedo), sizeof(data.pAlbedo));
	os.write(reinterpret_cast<const char*>(&data.Albedo), sizeof(data.Albedo));
	os.write(reinterpret_cast<const char*>(&data.SoilAlb), sizeof(data.SoilAlb));
	os.write(reinterpret_cast<const char*>(&data.SoilEmissivity), sizeof(data.SoilEmissivity));
	os.write(reinterpret_cast<const char*>(&data.BareSoil_z0), sizeof(data.BareSoil_z0));
	os.write(reinterpret_cast<const char*>(&data.SoilNode), sizeof(data.SoilNode));
	os.write(reinterpret_cast<const char*>(&data.Ground), sizeof(data.Ground));
	os.write(reinterpret_cast<const char*>(&data.cH), sizeof(data.cH));
	os.write(reinterpret_cast<const char*>(&data.mH), sizeof(data.mH));
	os.write(reinterpret_cast<const char*>(&data.mass_sum), sizeof(data.mass_sum));
	os.write(reinterpret_cast<const char*>(&data.swe), sizeof(data.swe));
	os.write(reinterpret_cast<const char*>(&data.lwc_sum), sizeof(data.lwc_sum));
	os.write(reinterpret_cast<const char*>(&data.hn), sizeof(data.hn));
	os.write(reinterpret_cast<const char*>(&data.rho_hn), sizeof(data.rho_hn));
	os.write(reinterpret_cast<const char*>(&data.ErosionLevel), sizeof(data.ErosionLevel));
	os.write(reinterpret_cast<const char*>(&data.ErosionMass), sizeof(data.ErosionMass));
	os.write(reinterpret_cast<const char*>(&data.S_class1), sizeof(data.S_class1));
	os.write(reinterpret_cast<const char*>(&data.S_class2), sizeof(data.S_class2));
	os.write(reinterpret_cast<const char*>(&data.S_d), sizeof(data.S_d));
	os.write(reinterpret_cast<const char*>(&data.z_S_d), sizeof(data.z_S_d));
	os.write(reinterpret_cast<const char*>(&data.S_n), sizeof(data.S_n));
	os.write(reinterpret_cast<const char*>(&data.z_S_n), sizeof(data.z_S_n));
	os.write(reinterpret_cast<const char*>(&data.S_s), sizeof(data.S_s));
	os.write(reinterpret_cast<const char*>(&data.z_S_s), sizeof(data.z_S_s));
	os.write(reinterpret_cast<const char*>(&data.S_4), sizeof(data.S_4));
	os.write(reinterpret_cast<const char*>(&data.z_S_4), sizeof(data.z_S_4));
	os.write(reinterpret_cast<const char*>(&data.S_5), sizeof(data.S_5));
	os.write(reinterpret_cast<const char*>(&data.z_S_5), sizeof(data.z_S_5));

	const size_t s_Ndata = data.Ndata.size();
	os.write(reinterpret_cast<const char*>(&s_Ndata), sizeof(size_t));
	for (size_t ii=0; ii<s_Ndata; ii++) os << data.Ndata[ii];

	const size_t s_Edata = data.Edata.size();
	os.write(reinterpret_cast<const char*>(&s_Edata), sizeof(size_t));
	for (size_t ii=0; ii<s_Edata; ii++) os << data.Edata[ii];

	// void *Kt
	os.write(reinterpret_cast<const char*>(&data.ColdContent), sizeof(data.ColdContent));
	os.write(reinterpret_cast<const char*>(&data.ColdContentSoil), sizeof(data.ColdContentSoil));
	os.write(reinterpret_cast<const char*>(&data.dIntEnergy), sizeof(data.dIntEnergy));
	os.write(reinterpret_cast<const char*>(&data.dIntEnergySoil), sizeof(data.dIntEnergySoil));
	os.write(reinterpret_cast<const char*>(&data.meltFreezeEnergy), sizeof(data.meltFreezeEnergy));
	os.write(reinterpret_cast<const char*>(&data.meltFreezeEnergySoil), sizeof(data.meltFreezeEnergySoil));
	os.write(reinterpret_cast<const char*>(&data.meltMassTot), sizeof(data.meltMassTot));
	os.write(reinterpret_cast<const char*>(&data.refreezeMassTot), sizeof(data.refreezeMassTot));
	os.write(reinterpret_cast<const char*>(&data.ReSolver_dt), sizeof(data.ReSolver_dt));
	os.write(reinterpret_cast<const char*>(&data.windward), sizeof(data.windward));
	os.write(reinterpret_cast<const char*>(&data.WindScalingFactor), sizeof(data.WindScalingFactor));
	os.write(reinterpret_cast<const char*>(&data.TimeCountDeltaHS), sizeof(data.TimeCountDeltaHS));

	//static member variables
	/*os.write(reinterpret_cast<const char*>(&data.comb_thresh_l), sizeof(data.comb_thresh_l));
	os.write(reinterpret_cast<const char*>(&data.comb_thresh_ice), sizeof(data.comb_thresh_ice));
	os.write(reinterpret_cast<const char*>(&data.comb_thresh_water), sizeof(data.comb_thresh_water));
	os.write(reinterpret_cast<const char*>(&data.comb_thresh_dd), sizeof(data.comb_thresh_dd));
	os.write(reinterpret_cast<const char*>(&data.comb_thresh_sp), sizeof(data.comb_thresh_sp));
	os.write(reinterpret_cast<const char*>(&data.comb_thresh_rg), sizeof(data.comb_thresh_rg));
	os.write(reinterpret_cast<const char*>(&data.thresh_moist_snow), sizeof(data.thresh_moist_snow));
	os.write(reinterpret_cast<const char*>(&data.thresh_moist_soil), sizeof(data.thresh_moist_soil));
	os.write(reinterpret_cast<const char*>(&data.number_top_elements), sizeof(data.number_top_elements));
	os.write(reinterpret_cast<const char*>(&data.number_of_solutes), sizeof(data.number_of_solutes));*/

	// private member variables:
	os.write(reinterpret_cast<const char*>(&data.nNodes), sizeof(data.nNodes));
	os.write(reinterpret_cast<const char*>(&data.nElems), sizeof(data.nElems));
	os.write(reinterpret_cast<const char*>(&data.maxElementID), sizeof(data.maxElementID));
	os.write(reinterpret_cast<const char*>(&data.useCanopyModel), sizeof(data.useCanopyModel));
	os.write(reinterpret_cast<const char*>(&data.useSoilLayers), sizeof(data.useSoilLayers));
	return os;
}

std::istream& operator>>(std::istream& is, SnowStation& data)
{
	// HACK: nothing is done for the void* Kt

	is >> data.meta;
	is.read(reinterpret_cast<char*>(&data.cos_sl), sizeof(data.cos_sl));
	is.read(reinterpret_cast<char*>(&data.sector), sizeof(data.sector));

	is >> data.Cdata;
	//is >> data.Seaice;	//HACK how to do this with a pointer?
	is.read(reinterpret_cast<char*>(&data.pAlbedo), sizeof(data.pAlbedo));
	is.read(reinterpret_cast<char*>(&data.Albedo), sizeof(data.Albedo));
	is.read(reinterpret_cast<char*>(&data.SoilAlb), sizeof(data.SoilAlb));
	is.read(reinterpret_cast<char*>(&data.SoilEmissivity), sizeof(data.SoilEmissivity));
	is.read(reinterpret_cast<char*>(&data.BareSoil_z0), sizeof(data.BareSoil_z0));
	is.read(reinterpret_cast<char*>(&data.SoilNode), sizeof(data.SoilNode));
	is.read(reinterpret_cast<char*>(&data.Ground), sizeof(data.Ground));
	is.read(reinterpret_cast<char*>(&data.cH), sizeof(data.cH));
	is.read(reinterpret_cast<char*>(&data.mH), sizeof(data.mH));
	is.read(reinterpret_cast<char*>(&data.mass_sum), sizeof(data.mass_sum));
	is.read(reinterpret_cast<char*>(&data.swe), sizeof(data.swe));
	is.read(reinterpret_cast<char*>(&data.lwc_sum), sizeof(data.lwc_sum));
	is.read(reinterpret_cast<char*>(&data.hn), sizeof(data.hn));
	is.read(reinterpret_cast<char*>(&data.rho_hn), sizeof(data.rho_hn));
	is.read(reinterpret_cast<char*>(&data.ErosionLevel), sizeof(data.ErosionLevel));
	is.read(reinterpret_cast<char*>(&data.ErosionMass), sizeof(data.ErosionMass));
	is.read(reinterpret_cast<char*>(&data.S_class1), sizeof(data.S_class1));
	is.read(reinterpret_cast<char*>(&data.S_class2), sizeof(data.S_class2));
	is.read(reinterpret_cast<char*>(&data.S_d), sizeof(data.S_d));
	is.read(reinterpret_cast<char*>(&data.z_S_d), sizeof(data.z_S_d));
	is.read(reinterpret_cast<char*>(&data.S_n), sizeof(data.S_n));
	is.read(reinterpret_cast<char*>(&data.z_S_n), sizeof(data.z_S_n));
	is.read(reinterpret_cast<char*>(&data.S_s), sizeof(data.S_s));
	is.read(reinterpret_cast<char*>(&data.z_S_s), sizeof(data.z_S_s));
	is.read(reinterpret_cast<char*>(&data.S_4), sizeof(data.S_4));
	is.read(reinterpret_cast<char*>(&data.z_S_4), sizeof(data.z_S_4));
	is.read(reinterpret_cast<char*>(&data.S_5), sizeof(data.S_5));
	is.read(reinterpret_cast<char*>(&data.z_S_5), sizeof(data.z_S_5));

	size_t s_Ndata;
	is.read(reinterpret_cast<char*>(&s_Ndata), sizeof(size_t));
	data.Ndata.resize(s_Ndata);
	for (size_t ii=0; ii<s_Ndata; ii++) is >> data.Ndata[ii];

	size_t s_Edata;
	is.read(reinterpret_cast<char*>(&s_Edata), sizeof(size_t));
	data.Edata.resize( s_Edata, ElementData(ElementData::noID) );
	for (size_t ii=0; ii<s_Edata; ii++) is >> data.Edata[ii];

	data.Kt = NULL;

	is.read(reinterpret_cast<char*>(&data.ColdContent), sizeof(data.ColdContent));
	is.read(reinterpret_cast<char*>(&data.ColdContentSoil), sizeof(data.ColdContentSoil));
	is.read(reinterpret_cast<char*>(&data.dIntEnergy), sizeof(data.dIntEnergy));
	is.read(reinterpret_cast<char*>(&data.dIntEnergySoil), sizeof(data.dIntEnergySoil));
	is.read(reinterpret_cast<char*>(&data.meltFreezeEnergy), sizeof(data.meltFreezeEnergy));
	is.read(reinterpret_cast<char*>(&data.meltFreezeEnergySoil), sizeof(data.meltFreezeEnergySoil));
	is.read(reinterpret_cast<char*>(&data.meltMassTot), sizeof(data.meltMassTot));
	is.read(reinterpret_cast<char*>(&data.refreezeMassTot), sizeof(data.refreezeMassTot));
	is.read(reinterpret_cast<char*>(&data.ReSolver_dt), sizeof(data.ReSolver_dt));
	is.read(reinterpret_cast<char*>(&data.windward), sizeof(data.windward));
	is.read(reinterpret_cast<char*>(&data.WindScalingFactor), sizeof(data.WindScalingFactor));
	is.read(reinterpret_cast<char*>(&data.TimeCountDeltaHS), sizeof(data.TimeCountDeltaHS));

	//static member variables
	/*is.read(reinterpret_cast<char*>(&data.comb_thresh_l), sizeof(data.comb_thresh_l));
	is.read(reinterpret_cast<char*>(&data.comb_thresh_ice), sizeof(data.comb_thresh_ice));
	is.read(reinterpret_cast<char*>(&data.comb_thresh_water), sizeof(data.comb_thresh_water));
	is.read(reinterpret_cast<char*>(&data.comb_thresh_dd), sizeof(data.comb_thresh_dd));
	is.read(reinterpret_cast<char*>(&data.comb_thresh_sp), sizeof(data.comb_thresh_sp));
	is.read(reinterpret_cast<char*>(&data.comb_thresh_rg), sizeof(data.comb_thresh_rg));
	is.read(reinterpret_cast<char*>(&data.thresh_moist_snow), sizeof(data.thresh_moist_snow));
	is.read(reinterpret_cast<char*>(&data.thresh_moist_soil), sizeof(data.thresh_moist_soil));
	is.read(reinterpret_cast<char*>(&data.number_top_elements), sizeof(data.number_top_elements));
	is.read(reinterpret_cast<char*>(&data.number_of_solutes), sizeof(data.number_of_solutes));*/

	// private member variables:
	is.read(reinterpret_cast<char*>(&data.nNodes), sizeof(data.nNodes));
	is.read(reinterpret_cast<char*>(&data.nElems), sizeof(data.nElems));
	is.read(reinterpret_cast<char*>(&data.maxElementID), sizeof(data.maxElementID));
	is.read(reinterpret_cast<char*>(&data.useCanopyModel), sizeof(data.useCanopyModel));
	is.read(reinterpret_cast<char*>(&data.useSoilLayers), sizeof(data.useSoilLayers));
	return is;
}

const std::string SnowStation::toString() const
{
	std::ostringstream os;
	os << "<SnowStation>" << "\n";
	os << "" << meta.toString();
	os << setprecision(4);
	//os << fixed;
	os << "" << nElems << " element(s) and " << nNodes << " node(s).";
	if(useSoilLayers)
		os << " Soil=true";
	else
		os << " Soil=false";
	if(useCanopyModel)
		os << " canopy=true";
	else
		os << " canopy=false";
	os << "\n";

	os << "Soil:\tSoilNode=" << SoilNode  << " depth=" << Ground << " BareSoil_z0=" << BareSoil_z0 << " SoilAlb=" << SoilAlb << " SoilEmissivity=" << SoilEmissivity <<  "\n";
	os << "Snow:\tMeasured HS=" << mH << " Calculated HS=" << cH << " SWE=" << swe << " LWCtot" << lwc_sum << " New snow=" << hn << " of density=" << rho_hn << "\n";
	os << "Snow Albedo:\tAlbedo=" << Albedo << " parametrized Albedo=" << pAlbedo << "\n";
	os << "Energy:\tColdContent=" << ColdContent << " dIntEnergy=" << dIntEnergy;
	os << "Snowdrift:\tsector=" << sector << " windward=" << windward << " ErosionLevel=" << ErosionLevel << " ErosionMass=" << ErosionMass << "\n";
	os << "WindScalingFactor:          " << WindScalingFactor << "\n";
	os << "TimeCountDeltaHS:           " << TimeCountDeltaHS << "\n";
	os << "Stability:\tS_d(" << z_S_d << ")=" << S_d << " S_n(" << z_S_n << ")=" << S_n << " S_s(" << z_S_s << ")=" << S_s;
	os << " S_1=" << S_class1 << " S_2=" << S_class2 << " S_4(" << z_S_4 << ")=" << S_4 << " S_5(" << z_S_5 << ")=" << S_5 << "\n";

	if(Kt==NULL)
		os << "Kt= NULL\n";
	else
		os << "Kt= " << hex << Kt << dec << "\n";
	/*for (unsigned int ii=1; ii<Ndata.size(); ii++) {
		os << Ndata[ii].toString();
	}
	for (unsigned int ii=1; ii<Edata.size(); ii++) {
		os << Edata[ii].toString();
	}*/
	//os << "Canopy=" << Cdata;

	os << "</SnowStation>\n";
	return os.str();
}

CurrentMeteo::CurrentMeteo()
        : date(), ta(0.), rh(0.), rh_avg(IOUtils::nodata), vw(0.), vw_avg(IOUtils::nodata), vw_max(0.), dw(0.),
          vw_drift(0.), dw_drift(0.), ustar(0.), z0(0.), psi_s(0.), psi_m(0.),
          iswr(0.), rswr(0.), mAlbedo(0.), diff(0.), dir_h(0.), elev(0.), ea(0.), tss(0.), tss_a12h(0.), tss_a24h(0.), ts0(0.),
          liq_psum(0.), solid_psum(0.), psum_tech(IOUtils::nodata), hs(0.), hs_a3h(0.), hs_rate(0.), geo_heat(IOUtils::nodata), adv_heat(IOUtils::nodata),
          surf_melt(0.), snowdrift(0.), sublim(0.), odc(0.), p(0.),
          ts(), zv_ts(), conc(SnowStation::number_of_solutes, 0.), rho_hn(0.),
          fixedPositions(), minDepthSubsurf(), maxNumberMeasTemperatures(),
          numberMeasTemperatures(mio::IOUtils::unodata), numberFixedRates()
{}


CurrentMeteo::CurrentMeteo(const SnowpackConfig& cfg)
        : date(), ta(0.), rh(0.), rh_avg(IOUtils::nodata), vw(0.), vw_avg(IOUtils::nodata), vw_max(0.), dw(0.),
          vw_drift(0.), dw_drift(0.), ustar(0.), z0(0.), psi_s(0.), psi_m(0.),
          iswr(0.), rswr(0.), mAlbedo(0.), diff(0.), dir_h(0.), elev(0.), ea(0.), tss(0.), tss_a12h(0.), tss_a24h(0.), ts0(0.),
          liq_psum(0.),solid_psum(0.), psum_tech(IOUtils::nodata), hs(0.), hs_a3h(0.), hs_rate(0.), geo_heat(IOUtils::nodata), adv_heat(IOUtils::nodata),
          surf_melt(0.), snowdrift(0.), sublim(0.), odc(0.), p(0.),
          ts(), zv_ts(), conc(SnowStation::number_of_solutes, 0.), rho_hn(0.),
          fixedPositions(), minDepthSubsurf(), maxNumberMeasTemperatures(),
          numberMeasTemperatures(mio::IOUtils::unodata), numberFixedRates()
{
	maxNumberMeasTemperatures = cfg.get("MAX_NUMBER_MEAS_TEMPERATURES", "SnowpackAdvanced");
	cfg.getValue("FIXED_POSITIONS", "SnowpackAdvanced", fixedPositions);
	minDepthSubsurf = cfg.get("MIN_DEPTH_SUBSURF", "SnowpackAdvanced");
	numberFixedRates = cfg.get("NUMBER_FIXED_RATES", "SnowpackAdvanced");
}

void CurrentMeteo::reset(const SnowpackConfig& i_cfg)
{
	*this = CurrentMeteo(i_cfg);
}

/* Description:
* - Measured and/or modelled temperatures can be monitored at fixed positions (m).
* - At most MAX_NUMBER_MEAS_TEMPERATURES can be monitored (by default 5). Measured temperatures
*     are read in from the input file. If you use the smet format, do not forget to properly
*     label the columns: TS1, TS2, TS3, etc.
* - User defined positions (m) should be provided in the advanced section, for example,
*     FIXED_POSITIONS = "0.25 0.50 -0.10":
* 	- positive values refer to heigths measured from the ground surface (snow only)
* 	- negative values refer to depths measured from either the ground surface or the snow surface in case no soil
*      layers are present
* 	- There may be be more FIXED_POSITIONS than measured temperatures. In that case, the first positions are
*      associated with measured values of TS1, TS2, etc. and the following will be associated with modelled
*      temperatures only
* @note:
* 	- A sensor must at least be covered by MIN_DEPTH_SUBSURF (m) snow for its temperature to be output
*/
void CurrentMeteo::setMeasTempParameters(const mio::MeteoData& md)
{
	for (size_t jj = maxNumberMeasTemperatures; jj-- > 0; ) {
		stringstream ss;
		ss << "HTS" << jj+1;
		if (md.param_exists(ss.str()) && (md(ss.str()) != Constants::undefined)) {
			fixedPositions.insert(fixedPositions.begin(), md(ss.str()));
		}
	}
	if (numberMeasTemperatures == IOUtils::unodata) {
		numberMeasTemperatures = getNumberMeasTemperatures(md);
	}
	if (numberMeasTemperatures > maxNumberMeasTemperatures) {
		prn_msg(__FILE__, __LINE__, "wrn", Date(),
		        "Too many measured temperatures (%u). Only the first %u will be used. Check input file!",
		        numberMeasTemperatures, maxNumberMeasTemperatures);
		numberMeasTemperatures = maxNumberMeasTemperatures;
	}
	if ((numberMeasTemperatures > 0) && (fixedPositions.empty())) {
		prn_msg(__FILE__, __LINE__, "wrn", Date(),
		        "%u measured temperatures available but no positions. Check FIXED_POSITIONS in SnowpackAdvanced section!",
		        numberMeasTemperatures);
	}
	if (fixedPositions.size() > maxNumberMeasTemperatures) {
		fixedPositions.resize(maxNumberMeasTemperatures);
		prn_msg(__FILE__, __LINE__, "wrn", Date(),
		        "Vector of positions resized to MAX_NUMBER_MEAS_TEMPERATURES (%u). Check FIXED_POSITIONS in SnowpackAdvanced section!",
		        maxNumberMeasTemperatures);
	}

	const size_t number_ts = std::max(numberMeasTemperatures, fixedPositions.size());
	ts.resize(number_ts, mio::IOUtils::nodata);
	zv_ts.resize(number_ts, mio::IOUtils::nodata);
}

/**
* @brief Returns the number of measured snow/soil temperatures stored in MeteoData
*/
size_t CurrentMeteo::getNumberMeasTemperatures() const
{
	return numberMeasTemperatures;
}

size_t CurrentMeteo::getNumberMeasTemperatures(const mio::MeteoData& md)
{
	size_t nrMeasTemperatures = 0;
	const size_t numberParams = md.getNrOfParameters();
	for (size_t ii=0; ii<numberParams; ii++) {
		stringstream ss;
		ss << "TS" << nrMeasTemperatures+1;
		if (md.getNameForParameter(ii) == ss.str()) {
			nrMeasTemperatures++;
		}
	}
	return nrMeasTemperatures;
}

void CurrentMeteo::getFixedPositions(std::vector<double>& positions) const
{
	positions = fixedPositions;
}

size_t CurrentMeteo::getNumberFixedPositions() const
{
	return fixedPositions.size();
}

size_t CurrentMeteo::getNumberFixedRates() const
{
	return numberFixedRates;
}

size_t CurrentMeteo::getMaxNumberMeasTemperatures() const
{
	return maxNumberMeasTemperatures;
}

void CurrentMeteo::copySnowTemperatures(const mio::MeteoData& md, const unsigned int& current_slope)
{
	std::vector<double> positions;
	getFixedPositions(positions);
	for (size_t jj=0; jj < positions.size(); jj++) {
		zv_ts[jj] = positions[jj];
		ts[jj] = mio::IOUtils::nodata;
		if (current_slope == 0) {
			stringstream ss;
			ss << "TS" << jj+1;
			if (md.param_exists(ss.str()) && (md(ss.str()) != mio::IOUtils::nodata)) {
				ts[jj] = md(ss.str());
			}
		}
	}
}

void CurrentMeteo::copySolutes(const mio::MeteoData& md, const size_t& i_number_of_solutes)
{
	if (i_number_of_solutes > 0) {
		for (size_t jj=0; jj < i_number_of_solutes; jj++) {
			conc[jj] = mio::IOUtils::nodata;
			stringstream ss;
			ss << "CONC" << jj;
			conc[jj] = md(ss.str());
		}
	} else {
		return;
	}
}

std::ostream& operator<<(std::ostream& os, const CurrentMeteo& data)
{
	os << data.date;
	os.write(reinterpret_cast<const char*>(&data.ta), sizeof(data.ta));
	os.write(reinterpret_cast<const char*>(&data.rh), sizeof(data.rh));
	os.write(reinterpret_cast<const char*>(&data.rh_avg), sizeof(data.rh_avg));
	os.write(reinterpret_cast<const char*>(&data.vw), sizeof(data.vw));
	os.write(reinterpret_cast<const char*>(&data.vw_avg), sizeof(data.vw_avg));
	os.write(reinterpret_cast<const char*>(&data.vw_max), sizeof(data.vw_max));
	os.write(reinterpret_cast<const char*>(&data.dw), sizeof(data.dw));
	os.write(reinterpret_cast<const char*>(&data.vw_drift), sizeof(data.vw_drift));
	os.write(reinterpret_cast<const char*>(&data.dw_drift), sizeof(data.dw_drift));
	os.write(reinterpret_cast<const char*>(&data.ustar), sizeof(data.ustar));
	os.write(reinterpret_cast<const char*>(&data.z0), sizeof(data.z0));
	os.write(reinterpret_cast<const char*>(&data.psi_s), sizeof(data.psi_s));
	os.write(reinterpret_cast<const char*>(&data.psi_m), sizeof(data.psi_m));
	os.write(reinterpret_cast<const char*>(&data.iswr), sizeof(data.iswr));
	os.write(reinterpret_cast<const char*>(&data.rswr), sizeof(data.rswr));
	os.write(reinterpret_cast<const char*>(&data.mAlbedo), sizeof(data.mAlbedo));
	os.write(reinterpret_cast<const char*>(&data.diff), sizeof(data.diff));
	os.write(reinterpret_cast<const char*>(&data.dir_h), sizeof(data.dir_h));
	os.write(reinterpret_cast<const char*>(&data.elev), sizeof(data.elev));
	os.write(reinterpret_cast<const char*>(&data.ea), sizeof(data.ea));
	os.write(reinterpret_cast<const char*>(&data.tss), sizeof(data.tss));
	os.write(reinterpret_cast<const char*>(&data.tss_a12h), sizeof(data.tss_a12h));
	os.write(reinterpret_cast<const char*>(&data.tss_a24h), sizeof(data.tss_a24h));
	os.write(reinterpret_cast<const char*>(&data.ts0), sizeof(data.ts0));
	os.write(reinterpret_cast<const char*>(&data.liq_psum), sizeof(data.liq_psum));
	os.write(reinterpret_cast<const char*>(&data.solid_psum), sizeof(data.solid_psum));
	os.write(reinterpret_cast<const char*>(&data.psum_tech), sizeof(data.psum_tech));
	os.write(reinterpret_cast<const char*>(&data.hs), sizeof(data.hs));
	os.write(reinterpret_cast<const char*>(&data.hs_a3h), sizeof(data.hs_a3h));
	os.write(reinterpret_cast<const char*>(&data.hs_rate), sizeof(data.hs_rate));
	os.write(reinterpret_cast<const char*>(&data.geo_heat), sizeof(data.geo_heat));
	os.write(reinterpret_cast<const char*>(&data.adv_heat), sizeof(data.adv_heat));
	os.write(reinterpret_cast<const char*>(&data.surf_melt), sizeof(data.surf_melt));
	os.write(reinterpret_cast<const char*>(&data.snowdrift), sizeof(data.snowdrift));
	os.write(reinterpret_cast<const char*>(&data.sublim), sizeof(data.sublim));
	os.write(reinterpret_cast<const char*>(&data.odc), sizeof(data.odc));
	os.write(reinterpret_cast<const char*>(&data.p), sizeof(data.p));

	const size_t s_ts = data.ts.size();
	os.write(reinterpret_cast<const char*>(&s_ts), sizeof(size_t));
	for (size_t ii=0; ii<s_ts; ii++) os << "" << data.ts[ii];

	const size_t s_zv_ts = data.zv_ts.size();
	os.write(reinterpret_cast<const char*>(&s_zv_ts), sizeof(size_t));
	for (size_t ii=0; ii<s_zv_ts; ii++) os << "" << data.zv_ts[ii];

	const size_t s_conc = data.conc.size();
	os.write(reinterpret_cast<const char*>(&s_conc), sizeof(size_t));
	for (size_t ii=0; ii<s_conc; ii++) os << "" << data.conc[ii];

	os.write(reinterpret_cast<const char*>(&data.rho_hn), sizeof(data.rho_hn));

	const size_t s_fixedPositions = data.fixedPositions.size();
	os.write(reinterpret_cast<const char*>(&s_fixedPositions), sizeof(size_t));
	for (size_t ii=0; ii<s_fixedPositions; ii++) os << "" << data.fixedPositions[ii];

	os.write(reinterpret_cast<const char*>(&data.minDepthSubsurf), sizeof(data.minDepthSubsurf));
	os.write(reinterpret_cast<const char*>(&data.maxNumberMeasTemperatures), sizeof(data.maxNumberMeasTemperatures));
	os.write(reinterpret_cast<const char*>(&data.numberMeasTemperatures), sizeof(data.numberMeasTemperatures));
	os.write(reinterpret_cast<const char*>(&data.numberFixedRates), sizeof(data.numberFixedRates));
	return os;
}

std::istream& operator>>(std::istream& is, CurrentMeteo& data)
{
	is >> data.date;
	is.read(reinterpret_cast<char*>(&data.ta), sizeof(data.ta));
	is.read(reinterpret_cast<char*>(&data.rh), sizeof(data.rh));
	is.read(reinterpret_cast<char*>(&data.rh_avg), sizeof(data.rh_avg));
	is.read(reinterpret_cast<char*>(&data.vw), sizeof(data.vw));
	is.read(reinterpret_cast<char*>(&data.vw_avg), sizeof(data.vw_avg));
	is.read(reinterpret_cast<char*>(&data.vw_max), sizeof(data.vw_max));
	is.read(reinterpret_cast<char*>(&data.dw), sizeof(data.dw));
	is.read(reinterpret_cast<char*>(&data.vw_drift), sizeof(data.vw_drift));
	is.read(reinterpret_cast<char*>(&data.dw_drift), sizeof(data.dw_drift));
	is.read(reinterpret_cast<char*>(&data.ustar), sizeof(data.ustar));
	is.read(reinterpret_cast<char*>(&data.z0), sizeof(data.z0));
	is.read(reinterpret_cast<char*>(&data.psi_s), sizeof(data.psi_s));
	is.read(reinterpret_cast<char*>(&data.psi_m), sizeof(data.psi_m));
	is.read(reinterpret_cast<char*>(&data.iswr), sizeof(data.iswr));
	is.read(reinterpret_cast<char*>(&data.rswr), sizeof(data.rswr));
	is.read(reinterpret_cast<char*>(&data.mAlbedo), sizeof(data.mAlbedo));
	is.read(reinterpret_cast<char*>(&data.diff), sizeof(data.diff));
	is.read(reinterpret_cast<char*>(&data.dir_h), sizeof(data.dir_h));
	is.read(reinterpret_cast<char*>(&data.elev), sizeof(data.elev));
	is.read(reinterpret_cast<char*>(&data.ea), sizeof(data.ea));
	is.read(reinterpret_cast<char*>(&data.tss), sizeof(data.tss));
	is.read(reinterpret_cast<char*>(&data.tss_a12h), sizeof(data.tss_a12h));
	is.read(reinterpret_cast<char*>(&data.tss_a24h), sizeof(data.tss_a24h));
	is.read(reinterpret_cast<char*>(&data.ts0), sizeof(data.ts0));
	is.read(reinterpret_cast<char*>(&data.liq_psum), sizeof(data.liq_psum));
	is.read(reinterpret_cast<char*>(&data.solid_psum), sizeof(data.solid_psum));
	is.read(reinterpret_cast<char*>(&data.psum_tech), sizeof(data.psum_tech));
	is.read(reinterpret_cast<char*>(&data.hs), sizeof(data.hs));
	is.read(reinterpret_cast<char*>(&data.hs_a3h), sizeof(data.hs_a3h));
	is.read(reinterpret_cast<char*>(&data.hs_rate), sizeof(data.hs_rate));
	is.read(reinterpret_cast<char*>(&data.geo_heat), sizeof(data.geo_heat));
	is.read(reinterpret_cast<char*>(&data.adv_heat), sizeof(data.adv_heat));
	is.read(reinterpret_cast<char*>(&data.surf_melt), sizeof(data.surf_melt));
	is.read(reinterpret_cast<char*>(&data.snowdrift), sizeof(data.snowdrift));
	is.read(reinterpret_cast<char*>(&data.sublim), sizeof(data.sublim));
	is.read(reinterpret_cast<char*>(&data.odc), sizeof(data.odc));
	is.read(reinterpret_cast<char*>(&data.p), sizeof(data.p));

	size_t s_ts;
	is.read(reinterpret_cast<char*>(&s_ts), sizeof(size_t));
	data.ts.resize(s_ts);
	for (size_t ii=0; ii<s_ts; ii++) is >> data.ts[ii];

	size_t s_zv_ts;
	is.read(reinterpret_cast<char*>(&s_zv_ts), sizeof(size_t));
	data.zv_ts.resize(s_zv_ts);
	for (size_t ii=0; ii<s_zv_ts; ii++) is >> data.zv_ts[ii];

	size_t s_conc;
	is.read(reinterpret_cast<char*>(&s_conc), sizeof(size_t));
	data.conc.resize(s_conc);
	for (size_t ii=0; ii<s_conc; ii++) is >> data.conc[ii];

	is.read(reinterpret_cast<char*>(&data.rho_hn), sizeof(data.rho_hn));

	size_t s_fixedPositions;
	is.read(reinterpret_cast<char*>(&s_fixedPositions), sizeof(size_t));
	data.fixedPositions.resize(s_fixedPositions);
	for (size_t ii=0; ii<s_fixedPositions; ii++) is >> data.fixedPositions[ii];

	is.read(reinterpret_cast<char*>(&data.minDepthSubsurf), sizeof(data.minDepthSubsurf));
	is.read(reinterpret_cast<char*>(&data.maxNumberMeasTemperatures), sizeof(data.maxNumberMeasTemperatures));
	is.read(reinterpret_cast<char*>(&data.numberMeasTemperatures), sizeof(data.numberMeasTemperatures));
	is.read(reinterpret_cast<char*>(&data.numberFixedRates), sizeof(data.numberFixedRates));
	return is;
}

const std::string CurrentMeteo::toString() const
{
	std::ostringstream os;
	const double to_deg = 180. / mio::Cst::PI;
	os << "<CurrentMeteo>" << "\n";
	os << "" << date.toString(Date::ISO) << "\n";

	os << setw(8) << "TA=" << ta << " TSS=" << tss << " TSG=" << ts0 << "\n";
	os << setw(8) << "RH=" << rh << " rh_avg=" << rh_avg << "\n";
	os << setw(8) << "ISWR=" << iswr << " RSWR=" << rswr << " mAlbedo=" << mAlbedo << "\n";
	os << setw(8) << "diff=" << diff << " dir_h=" << dir_h << " Sun_elev=" << elev*to_deg << "° EA=" << ea << "\n";
	os << setw(8) << "LIQ_PSUM=" << liq_psum << "SOLID_PSUM=" << solid_psum << " HS=" << hs << " rho_hn=" << rho_hn << " PSUM_TECH=" << psum_tech << "\n";
	os << setw(8) << "VW=" << vw << " vw_avg=" << vw_avg << " vw_max=" << vw_max << " vw_drift=" << vw_drift << "\n";
	os << setw(8) << "DW=" << dw << "\n";
	os << setw(8) << "U*=" << ustar << " z0=" << z0 << " psi_s=" << psi_s << " psi_m=" << psi_m << "\n";

	//os << std::setprecision(10);
	if(!ts.empty()) os << "     ";
	for (unsigned int ii=0; ii<ts.size(); ii++) {
		os << "ts(" << zv_ts[ii] << ")=" << ts[ii] << " ";
	}
	if(!ts.empty()) os << "\n";
	if(conc.size()>0) os << "     ";
	for (unsigned int ii=0; ii<conc.size(); ii++) {
		os << "conc[" << ii << "]=" << conc[ii] << " ";
	}
	if(!conc.empty()) os << "\n";

	os << "</CurrentMeteo>\n";
	return os.str();
}

std::ostream& operator<<(std::ostream& os, const SN_SNOWSOIL_DATA& data)
{
	os << data.meta;
	os << data.profileDate;
	os.write(reinterpret_cast<const char*>(&data.nN), sizeof(data.nN));
	os.write(reinterpret_cast<const char*>(&data.Height), sizeof(data.Height));
	os.write(reinterpret_cast<const char*>(&data.nLayers), sizeof(data.nLayers));

	const size_t s_Ldata = data.Ldata.size();
	os.write(reinterpret_cast<const char*>(&s_Ldata), sizeof(size_t));
	for (size_t ii=0; ii<s_Ldata; ii++) os << data.Ldata[ii];

	os.write(reinterpret_cast<const char*>(&data.HS_last), sizeof(data.HS_last));
	os.write(reinterpret_cast<const char*>(&data.Albedo), sizeof(data.Albedo));
	os.write(reinterpret_cast<const char*>(&data.SoilAlb), sizeof(data.SoilAlb));
	os.write(reinterpret_cast<const char*>(&data.BareSoil_z0), sizeof(data.BareSoil_z0));
	os.write(reinterpret_cast<const char*>(&data.Canopy_Height), sizeof(data.Canopy_Height));
	os.write(reinterpret_cast<const char*>(&data.Canopy_LAI), sizeof(data.Canopy_LAI));
	os.write(reinterpret_cast<const char*>(&data.Canopy_BasalArea), sizeof(data.Canopy_BasalArea));
	os.write(reinterpret_cast<const char*>(&data.Canopy_Direct_Throughfall), sizeof(data.Canopy_Direct_Throughfall));
	os.write(reinterpret_cast<const char*>(&data.Canopy_diameter), sizeof(data.Canopy_diameter));
	os.write(reinterpret_cast<const char*>(&data.Canopy_lai_frac_top_default), sizeof(data.Canopy_lai_frac_top_default));
	os.write(reinterpret_cast<const char*>(&data.Canopy_int_cap_snow), sizeof(data.Canopy_int_cap_snow));
	os.write(reinterpret_cast<const char*>(&data.Canopy_alb_dry), sizeof(data.Canopy_alb_dry));
	os.write(reinterpret_cast<const char*>(&data.Canopy_alb_wet), sizeof(data.Canopy_alb_wet));
	os.write(reinterpret_cast<const char*>(&data.Canopy_alb_snow), sizeof(data.Canopy_alb_snow));
	os.write(reinterpret_cast<const char*>(&data.Emissivity_soil), sizeof(data.Emissivity_soil));
	os.write(reinterpret_cast<const char*>(&data.WindScalingFactor), sizeof(data.WindScalingFactor));
	os.write(reinterpret_cast<const char*>(&data.ErosionLevel), sizeof(data.ErosionLevel));
	os.write(reinterpret_cast<const char*>(&data.TimeCountDeltaHS), sizeof(data.TimeCountDeltaHS));
	return os;
}

std::istream& operator>>(std::istream& is, SN_SNOWSOIL_DATA& data)
{
	is >> data.meta;
	is >> data.profileDate;
	is.read(reinterpret_cast<char*>(&data.nN), sizeof(data.nN));
	is.read(reinterpret_cast<char*>(&data.Height), sizeof(data.Height));
	is.read(reinterpret_cast<char*>(&data.nLayers), sizeof(data.nLayers));

	size_t s_Ldata;
	is.read(reinterpret_cast<char*>(&s_Ldata), sizeof(size_t));
	data.Ldata.resize(s_Ldata);
	for (size_t ii=0; ii<s_Ldata; ii++) is >> data.Ldata[ii];

	is.read(reinterpret_cast<char*>(&data.HS_last), sizeof(data.HS_last));
	is.read(reinterpret_cast<char*>(&data.Albedo), sizeof(data.Albedo));
	is.read(reinterpret_cast<char*>(&data.SoilAlb), sizeof(data.SoilAlb));
	is.read(reinterpret_cast<char*>(&data.BareSoil_z0), sizeof(data.BareSoil_z0));
	is.read(reinterpret_cast<char*>(&data.Canopy_Height), sizeof(data.Canopy_Height));
	is.read(reinterpret_cast<char*>(&data.Canopy_LAI), sizeof(data.Canopy_LAI));
	is.read(reinterpret_cast<char*>(&data.Canopy_BasalArea), sizeof(data.Canopy_BasalArea));
	is.read(reinterpret_cast<char*>(&data.Canopy_Direct_Throughfall), sizeof(data.Canopy_Direct_Throughfall));
	is.read(reinterpret_cast<char*>(&data.Canopy_diameter), sizeof(data.Canopy_diameter));
	is.read(reinterpret_cast<char*>(&data.Canopy_lai_frac_top_default), sizeof(data.Canopy_lai_frac_top_default));
	is.read(reinterpret_cast<char*>(&data.Canopy_int_cap_snow), sizeof(data.Canopy_int_cap_snow));
	is.read(reinterpret_cast<char*>(&data.Canopy_alb_dry), sizeof(data.Canopy_alb_dry));
	is.read(reinterpret_cast<char*>(&data.Canopy_alb_wet), sizeof(data.Canopy_alb_wet));
	is.read(reinterpret_cast<char*>(&data.Canopy_alb_snow), sizeof(data.Canopy_alb_snow));
	is.read(reinterpret_cast<char*>(&data.Emissivity_soil), sizeof(data.Emissivity_soil));
	is.read(reinterpret_cast<char*>(&data.WindScalingFactor), sizeof(data.WindScalingFactor));
	is.read(reinterpret_cast<char*>(&data.ErosionLevel), sizeof(data.ErosionLevel));
	is.read(reinterpret_cast<char*>(&data.TimeCountDeltaHS), sizeof(data.TimeCountDeltaHS));
	return is;
}

const std::string SN_SNOWSOIL_DATA::toString() const
{
	std::ostringstream os;
	os << "<SN_SNOWSOIL_DATA>\n";
	os << "" << meta.toString()   << "\n";
	os << "profileDate:                  " << profileDate.toString(Date::ISO) << "\n";
	os << "nN:                           " << nN << "\n";
	os << "Height:                       " << Height << "\n";
	os << "nLayers:                      " << nLayers << "\n";

	for(size_t ii=0; ii<nLayers; ii++)
		os << "" << Ldata[ii].toString();

	os << "HS_last:                      " << HS_last << "\n";
	os << "Albedo:                       " << Albedo << "\n";
	os << "SoilAlb:                      " << SoilAlb << "\n";
	os << "BareSoil_z0:                  " << BareSoil_z0 << "\n";
	os << "Canopy_Height:                " << Canopy_Height << "\n";
	os << "Canopy_LAI:                   " << Canopy_LAI << "\n";
	os << "Canopy_BasalArea:             " << Canopy_BasalArea << "\n";
	os << "Canopy_diameter:              " << Canopy_diameter << "\n";
	os << "Canopy_lai_frac_top_default:  " << Canopy_lai_frac_top_default << "\n";
	os << "Canopy_int_cap_snow:          " << Canopy_int_cap_snow << "\n";
	os << "Canopy_alb_dry:               " << Canopy_alb_dry << "\n";
	os << "Canopy_alb_wet:               " << Canopy_alb_wet << "\n";
	os << "Canopy_alb_snow:              " << Canopy_alb_snow << "\n";
	os << "Soil_Emissivity:              " << Emissivity_soil << "\n";
	os << "WindScalingFactor:            " << WindScalingFactor << "\n";
	os << "ErosionLevel:                 " << ErosionLevel << "\n";
	os << "TimeCountDeltaHS:             " << TimeCountDeltaHS << "\n";

	os << "</SN_SNOWSOIL_DATA>\n";
	return os.str();
}

const std::string SurfaceFluxes::toString() const
{
	std::ostringstream os;
	os << "<SurfaceFluxes>" << "\n";
	os << std::setprecision(10);
	os << "Long wave: lw_in=" << lw_in << " lw_out=" << lw_out << " lw_net=" << lw_net << "\n";
	os << "Short wave: sw_in=" << sw_in << " sw_out=" << sw_out << " qw=" << qw << "\n";
	os << "Short wave: sw_hor=" << sw_hor << " sw_dir=" << sw_dir << " sw_diff=" << sw_diff << "\n";
	os << "Albedo: mAlbedo=" << mAlbedo << " pAlbedo=" << pAlbedo << "\n";
	os << "Energy: qs=" << qs << " ql=" << ql << " qw=" << qw << " qr=" << qr << " qg=" << qg << " qg0=" << qg0 << "\n";
	os << "Energy: dIntEnergy=" << dIntEnergy << "\n";
	os << "Mass change: hoar=" << hoar << " drift=" << drift << " snow_depth_correction=" << dhs_corr << "\n";
	os << "Snow: mRho_hn=" << mRho_hn << " cRho_hn=" << cRho_hn << "\n";

	os << "" << mass.size() << " mass fluxes: ";
	for (unsigned int ii=1; ii<mass.size(); ii++) {
		os << "" << mass[ii] << " ";
	}
	os << "\n";
	os << "" << load.size() << " solutes fluxes: ";
	for (unsigned int ii=1; ii<load.size(); ii++) {
		os << "" << load[ii] << " ";
	}
	os << "\n";
	os << "</SurfaceFluxes>\n";

	return os.str();
}

LayerData::LayerData() : depositionDate(), hl(0.), ne(0), tl(0.),
                     phiSoil(0.), phiIce(0.), phiWater(0.), phiWaterPref(0.), phiVoids(0.),
                     cSoil(SnowStation::number_of_solutes), cIce(SnowStation::number_of_solutes), cWater(SnowStation::number_of_solutes), cVoids(SnowStation::number_of_solutes),
                     SoilRho(0.), SoilK(0.), SoilC(0.),
                     rg(0.), sp(0.), dd(0.), rb(0.), mk(0), hr(0.), CDot(0.), metamo(0.), salinity(0.), h(Constants::undefined)
{
}

std::ostream& operator<<(std::ostream& os, const LayerData& data)
{
	os << data.depositionDate;
	os.write(reinterpret_cast<const char*>(&data.hl), sizeof(data.hl));
	os.write(reinterpret_cast<const char*>(&data.ne), sizeof(data.ne));
	os.write(reinterpret_cast<const char*>(&data.tl), sizeof(data.tl));
	os.write(reinterpret_cast<const char*>(&data.phiSoil), sizeof(data.phiSoil));
	os.write(reinterpret_cast<const char*>(&data.phiIce), sizeof(data.phiIce));
	os.write(reinterpret_cast<const char*>(&data.phiWater), sizeof(data.phiWater));
	os.write(reinterpret_cast<const char*>(&data.phiWaterPref), sizeof(data.phiWaterPref));
	os.write(reinterpret_cast<const char*>(&data.phiVoids), sizeof(data.phiVoids));

	const size_t s_csoil = data.cSoil.size();
	os.write(reinterpret_cast<const char*>(&s_csoil), sizeof(size_t));
	os.write(reinterpret_cast<const char*>(&data.cSoil[0]), static_cast<streamsize>(s_csoil*sizeof(data.cSoil[0])));

	const size_t s_cice = data.cIce.size();
	os.write(reinterpret_cast<const char*>(&s_cice), sizeof(size_t));
	os.write(reinterpret_cast<const char*>(&data.cIce[0]), static_cast<streamsize>(s_cice*sizeof(data.cIce[0])));

	const size_t s_cwater = data.cWater.size();
	os.write(reinterpret_cast<const char*>(&s_cwater), sizeof(size_t));
	os.write(reinterpret_cast<const char*>(&data.cWater[0]), static_cast<streamsize>(s_cwater*sizeof(data.cWater[0])));

	const size_t s_cvoids = data.cVoids.size();
	os.write(reinterpret_cast<const char*>(&s_cvoids), sizeof(size_t));
	os.write(reinterpret_cast<const char*>(&data.cVoids[0]), static_cast<streamsize>(s_cvoids*sizeof(data.cVoids[0])));

	os.write(reinterpret_cast<const char*>(&data.SoilRho), sizeof(data.SoilRho));
	os.write(reinterpret_cast<const char*>(&data.SoilK), sizeof(data.SoilK));
	os.write(reinterpret_cast<const char*>(&data.SoilC), sizeof(data.SoilC));
	os.write(reinterpret_cast<const char*>(&data.rg), sizeof(data.rg));
	os.write(reinterpret_cast<const char*>(&data.sp), sizeof(data.sp));
	os.write(reinterpret_cast<const char*>(&data.dd), sizeof(data.dd));
	os.write(reinterpret_cast<const char*>(&data.rb), sizeof(data.rb));
	os.write(reinterpret_cast<const char*>(&data.mk), sizeof(data.mk));

	os.write(reinterpret_cast<const char*>(&data.hr), sizeof(data.hr));
	os.write(reinterpret_cast<const char*>(&data.CDot), sizeof(data.CDot));
	os.write(reinterpret_cast<const char*>(&data.metamo), sizeof(data.metamo));
	os.write(reinterpret_cast<const char*>(&data.salinity), sizeof(data.salinity));
	os.write(reinterpret_cast<const char*>(&data.h), sizeof(data.h));
	return os;
}

std::istream& operator>>(std::istream& is, LayerData& data)
{
	is >> data.depositionDate;
	is.read(reinterpret_cast<char*>(&data.hl), sizeof(data.hl));
	is.read(reinterpret_cast<char*>(&data.ne), sizeof(data.ne));
	is.read(reinterpret_cast<char*>(&data.tl), sizeof(data.tl));
	is.read(reinterpret_cast<char*>(&data.phiSoil), sizeof(data.phiSoil));
	is.read(reinterpret_cast<char*>(&data.phiIce), sizeof(data.phiIce));
	is.read(reinterpret_cast<char*>(&data.phiWater), sizeof(data.phiWater));
	is.read(reinterpret_cast<char*>(&data.phiWaterPref), sizeof(data.phiWaterPref));
	is.read(reinterpret_cast<char*>(&data.phiVoids), sizeof(data.phiVoids));

	size_t s_csoil;
	is.read(reinterpret_cast<char*>(&s_csoil), sizeof(size_t));
	data.cSoil.resize(s_csoil);
	is.read(reinterpret_cast<char*>(&data.cSoil[0]), static_cast<streamsize>(s_csoil*sizeof(data.cSoil[0])));

	size_t s_cice;
	is.read(reinterpret_cast<char*>(&s_cice), sizeof(size_t));
	data.cIce.resize(s_cice);
	is.read(reinterpret_cast<char*>(&data.cIce[0]), static_cast<streamsize>(s_cice*sizeof(data.cIce[0])));

	size_t s_cwater;
	is.read(reinterpret_cast<char*>(&s_cwater), sizeof(size_t));
	data.cWater.resize(s_cwater);
	is.read(reinterpret_cast<char*>(&data.cWater[0]), static_cast<streamsize>(s_cwater*sizeof(data.cWater[0])));

	size_t s_cvoids;
	is.read(reinterpret_cast<char*>(&s_cvoids), sizeof(size_t));
	data.cVoids.resize(s_cvoids);
	is.read(reinterpret_cast<char*>(&data.cVoids[0]), static_cast<streamsize>(s_cvoids*sizeof(data.cVoids[0])));

	is.read(reinterpret_cast<char*>(&data.SoilRho), sizeof(data.SoilRho));
	is.read(reinterpret_cast<char*>(&data.SoilK), sizeof(data.SoilK));
	is.read(reinterpret_cast<char*>(&data.SoilC), sizeof(data.SoilC));
	is.read(reinterpret_cast<char*>(&data.rg), sizeof(data.rg));
	is.read(reinterpret_cast<char*>(&data.sp), sizeof(data.sp));
	is.read(reinterpret_cast<char*>(&data.dd), sizeof(data.dd));
	is.read(reinterpret_cast<char*>(&data.rb), sizeof(data.rb));
	is.read(reinterpret_cast<char*>(&data.mk), sizeof(data.mk));

	is.read(reinterpret_cast<char*>(&data.hr), sizeof(data.hr));
	is.read(reinterpret_cast<char*>(&data.CDot), sizeof(data.CDot));
	is.read(reinterpret_cast<char*>(&data.metamo), sizeof(data.metamo));
	is.read(reinterpret_cast<char*>(&data.salinity), sizeof(data.salinity));
	is.read(reinterpret_cast<char*>(&data.h), sizeof(data.h));
	return is;
}

const std::string LayerData::toString() const
{
	std::ostringstream os;
	os << "<LayerData>\n";

	os << "" << depositionDate.toString(mio::Date::ISO) << "\n";
	os << "\theight:" << hl << " (" << ne << "elements) at " << tl << "K\n";
	os << "\tvolumetric contents: " << phiIce << " ice, " << phiWater << " water, " << phiWaterPref << " water_pref, " << phiVoids << " voids, ";
	os << "" << phiSoil << " soil, total = " << phiIce+phiWater+phiWaterPref+phiVoids+phiSoil << "%\n";
	os << "\tSoil properties: " << SoilRho << " kg/m^3, " << SoilK << " W/(m*K), " << SoilC << " J/K\n";
	os << "\tSoil microstructure: rg=" << rg << " sp=" << sp << " dd=" << dd << " rb=" << rb << " mk=" << mk << "\n";
	os << "\tStability: surface hoar=" << hr << " kg/m^2, stress rate=" << CDot << " Pa/s, metamo=" << metamo << "\n";
	os << "\tNumber of solutes: " << cSoil.size() << " in soil, " << cIce.size() << " in ice, " << cWater.size() << " in water, " << cVoids.size() << " in voids\n";

	os << "</LayerData>\n";
	return os.str();
}
