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

#include <snowpack/vanGenuchten.h>
#include <snowpack/snowpackCore/ReSolver1d.h>
#include <snowpack/Constants.h>
#include <snowpack/Utils.h>

/**
 * @file vanGenuchten.cc
 * @version 17.06
 * @brief This module contains the van Genuchten model for the water retention curve
 */


/**
 * @brief Class constructor \n
 * @author Nander Wever
 * @param pEMS pointer to the ElementData class which owns the van Genuchten class, so the van Genuchten class can access objects from the ElementData class.
 */
vanGenuchten::vanGenuchten(ElementData& pEMS) :
	EMS(&pEMS), theta_r(0.), theta_s(1.), alpha(0.), n(0.), m(0.), h_e(0.), Sc(0.), ksat(0.), field_capacity(0), defined(false) {}


/**
 * @brief Copy constructor \n
 * @author Nander Wever
 * @param c Class to copy
 */
vanGenuchten::vanGenuchten(const vanGenuchten& c) :
	EMS(c.EMS), theta_r(c.theta_r), theta_s(c.theta_s), alpha(c.alpha), n(c.n), m(c.m), h_e(c.h_e), Sc(c.Sc), ksat(c.ksat), field_capacity(c.field_capacity), defined(c.defined) {}


/**
 * @brief Assignment operator \n
 * @author Nander Wever
 * @param c Class to assign
 */
vanGenuchten& vanGenuchten::operator=(const vanGenuchten& source) {
	if(this != &source) {
		theta_r = source.theta_r;
		theta_s = source.theta_s;
		alpha = source.alpha;
		n = source.n;
		m = source.m;
		h_e = source.h_e;
		Sc = source.Sc;
		ksat = source.ksat;

		defined = source.defined;
	}
	return *this;
}

std::iostream& operator<<(std::iostream& os, const vanGenuchten& data)
{
	os.write(reinterpret_cast<const char*>(&data.EMS), sizeof(data.EMS));
	os.write(reinterpret_cast<const char*>(&data.theta_r), sizeof(data.theta_r));
	os.write(reinterpret_cast<const char*>(&data.theta_s), sizeof(data.theta_s));
	os.write(reinterpret_cast<const char*>(&data.alpha), sizeof(data.alpha));
	os.write(reinterpret_cast<const char*>(&data.n), sizeof(data.n));
	os.write(reinterpret_cast<const char*>(&data.m), sizeof(data.m));
	os.write(reinterpret_cast<const char*>(&data.h_e), sizeof(data.h_e));
	os.write(reinterpret_cast<const char*>(&data.Sc), sizeof(data.Sc));
	os.write(reinterpret_cast<const char*>(&data.ksat), sizeof(data.ksat));
	os.write(reinterpret_cast<const char*>(&data.defined), sizeof(data.defined));
	return os;
}

std::iostream& operator>>(std::iostream& is, vanGenuchten& data)
{
	is.write(reinterpret_cast<const char*>(&data.EMS), sizeof(data.EMS));
	is.write(reinterpret_cast<const char*>(&data.theta_r), sizeof(data.theta_r));
	is.write(reinterpret_cast<const char*>(&data.theta_s), sizeof(data.theta_s));
	is.write(reinterpret_cast<const char*>(&data.alpha), sizeof(data.alpha));
	is.write(reinterpret_cast<const char*>(&data.n), sizeof(data.n));
	is.write(reinterpret_cast<const char*>(&data.m), sizeof(data.m));
	is.write(reinterpret_cast<const char*>(&data.h_e), sizeof(data.h_e));
	is.write(reinterpret_cast<const char*>(&data.Sc), sizeof(data.Sc));
	is.write(reinterpret_cast<const char*>(&data.ksat), sizeof(data.ksat));
	is.write(reinterpret_cast<const char*>(&data.defined), sizeof(data.defined));
	return is;
}

/**
 * @brief Calculate air entry pressure head \n
 * Air entry pressure head in [m] that corresponds to a maximum pore size (using Young-Laplace Equation).\n
 * This is a required value for specifying water retention curves, see Ippisch et al. (2006).\n
 * @author Nander Wever
 * @param MaximumPoreSize Maximum pore size (diameter, not radius!) [m]
 * @param Temperature Temperature for determining surface tension [K]
 */
double vanGenuchten::AirEntryPressureHead(const double MaximumPoreSize, const double Temperature)
{
	//Surface tension is dependent on the temperature. Most simulations will be in the temperature range of -20 - +20 degC.
	//Source: http://en.wikipedia.org/wiki/Surface_tension
	//Surface tension of water in N/m.
	const double SurfaceTension = (Temperature > 293.)? 0.07197 : 0.07564; //Value for 25 degC vs for 0 degC
	const double delta_P=-1.*(2.*SurfaceTension)/(0.5*MaximumPoreSize);
	const double air_entry_head=delta_P/(Constants::density_water*Constants::g);

	return air_entry_head;
}


/**
 * @brief Set soil parameters for a given soil type \n
 * Set soil parameters for a given soil type \n
 * @author Nander Wever
 * @param type Soil type
 */
void vanGenuchten::SetSoil(const SoilTypes type)
{
	double MaximumPoreSize=0.;	//Maximum pore size (diameter) in [m]

	//Set van Genuchten parameters
	switch (type) {
		case ORGANIC:
			//Organic: Nemes (2001), Development of Soil Hydraulic Pedotransfer Functions on a European scale: Their Usefulness in the Assessment of Soil Quality.
			theta_r=0.01;
			theta_s=0.766;
			alpha=1.3;
			n=1.2039;
			ksat=8.000/(365.*24.*60.*60.);
			MaximumPoreSize=0.005;
			field_capacity=0.2;
			break;

		//ROSETTA Class Average Hydraulic Parameters: http://ars.usda.gov/Services/docs.htm?docid=8955
		//Field capacity computed from: K.E. Saxton et al., 1986, Estimating generalized soil-water characteristics from texture. Soil Sci. Soc. Amer. J. 50(4):1031-1036
		case CLAY:
			theta_r=0.098;
			theta_s=0.459;
			n=1.253;
			alpha=1.496;
			ksat=0.14757/(24.*60.*60.);
			MaximumPoreSize=0.005;
			field_capacity=0.479;
			break;

		case CLAYLOAM:
			theta_r=0.079;
			theta_s=0.442;
			n=1.416;
			alpha=1.581;
			ksat=0.0818/(24.*60.*60.);
			MaximumPoreSize=0.005;
			field_capacity=0.336;
			break;

		case LOAM:
			theta_r=0.061;
			theta_s=0.399;
			alpha=1.11;
			n=1.47;
			ksat=0.02947/(24.*60.*60.);
			MaximumPoreSize=0.005;
			field_capacity=0.262;
			break;

		case LOAMYSAND:
			theta_r=0.049;
			theta_s=0.39;
			n=1.746;
			alpha=3.475;
			ksat=1.052/(24.*60.*60.);
			MaximumPoreSize=0.005;
			field_capacity=0.171;
			break;

		case SAND:
			theta_r=0.053;
			theta_s=0.375;
			n=3.177;
			alpha=3.524;
			ksat=6.427/(24.*60.*60.);
			MaximumPoreSize=0.005;
			field_capacity=0.132;
			break;

		case SANDYCLAY:
			theta_r=0.117;
			theta_s=0.385;
			n=1.208;
			alpha=3.342;
			ksat=0.1135/(24.*60.*60.);
			MaximumPoreSize=0.005;
			field_capacity=0.368;
			break;

		case SANDYCLAYLOAM:
			theta_r=0.063;
			theta_s=0.384;
			n=1.330;
			alpha=2.109;
			ksat=0.1318/(24.*60.*60.);
			MaximumPoreSize=0.005;
			field_capacity=0.272;
			break;

		case SANDYLOAM:
			theta_r=0.039;
			theta_s=0.387;
			n=1.4488;
			alpha=2.667;
			ksat=0.3828/(24.*60.*60.);
			MaximumPoreSize=0.005;
			field_capacity=0.205;
			break;

		case SILT:
			theta_r=0.050;
			theta_s=0.489;
			n=1.6788;
			alpha=0.6577;
			ksat=0.4375/(24.*60.*60.);
			MaximumPoreSize=0.005;
			field_capacity=0.316;
			break;

		case SILTYCLAY:
			theta_r=0.111;
			theta_s=0.481;
			n=1.321;
			alpha=1.622;
			ksat=0.09616/(24.*60.*60.);
			MaximumPoreSize=0.005;
			field_capacity=0.452;
			break;

		case SILTYCLAYLOAM:
			theta_r=0.090;
			theta_s=0.482;
			n=1.5205;
			alpha=0.8395;
			ksat=0.1112/(24.*60.*60.);
			MaximumPoreSize=0.005;
			field_capacity=0.367;
			break;

		case SILTLOAM:
			theta_r=0.065;
			theta_s=0.439;
			n=1.6634;
			alpha=0.5058;
			ksat=0.1824/(24.*60.*60.);
			MaximumPoreSize=0.005;
			field_capacity=0.292;
			break;

		case WFJGRAVELSAND: //Gravel/sand
			theta_r=0.01;
			theta_s=0.35;
			n=4.5;
			alpha=3.5;
			ksat=0.000003171; //Equal to 100 m/year, for clean sand and silty sand, according to: http://web.ead.anl.gov/resrad/datacoll/conuct.htm
			MaximumPoreSize=0.005;
			field_capacity=0.07;
			break;

		default:
			throw mio::UnknownValueException("Unknown soil type", AT);

	}

	h_e=vanGenuchten::AirEntryPressureHead(MaximumPoreSize, 273.);
	m=(n-1.)/(n);

	return;
}


/**
 * @brief Calculating pressure head from water content \n
 * The following function calculates the pressure head belonging to a given water content \n
 * @author Nander Wever
 * @param theta Water content (m^3/m^3)
 * @param h_d Dry limit of pressure head
 */
double vanGenuchten::fromTHETAtoH(const double theta, const double h_d)
{
	//Inverse of Van Genuchten (1980), Equation 21:
	double returnvalue;
	if (theta<=theta_r) {
		returnvalue=h_d;
	} else {
		if (theta >= theta_s) {
			returnvalue=h_e;
		} else {
			returnvalue=-1.*(1./alpha)*pow( (pow(Sc*((theta-theta_r)/(theta_s-theta_r)), (-1./m)) - 1.), (1./n));
		}
	}
	return returnvalue;
}


/**
 * @brief Calculating pressure head from water content when ice is present \n
 * The following function calculates the pressure head belonging to a given water content when ice is present \n
 * @author Nander Wever
 * @param theta Water content (m^3/m^3)
 * @param h_d Dry limit of pressure head
 * @param theta_i Ice content (m^3/m^3)
 */
double vanGenuchten::fromTHETAtoHforICE(const double theta, const double h_d, const double theta_i)
{
	//To have same return value as fromTHETAtoH, call this function with theta_i==0.
	return fromTHETAtoH(theta+(theta_i*(Constants::density_ice/Constants::density_water)), h_d);
}


/**
 * @brief Calculating volumetric water content from pressure head \n
 * The following function calculates the volumetric water content belonging to a given pressure head \n
 * @author Nander Wever
 * @param h Pressure head (m)
 */
double vanGenuchten::fromHtoTHETA(const double h)
{
	double returnvalue=0.;
	//Van Genuchten (1980), Equation 21:
	if (h>h_e) {		//Saturation
		returnvalue=theta_s;
	} else {
		returnvalue=theta_r+( (theta_s-theta_r)*(1./Sc)*pow(1.+pow((alpha*fabs(h)),n),(-1.*m)) );
	}
	return returnvalue;
}


/**
 * @brief Calculating volumetric water content from pressure head when ice is present \n
 * The following function calculates the volumetric water content belonging to a given pressure head when ice is present \n
 * @author Nander Wever
 * @param h Pressure head (m)
 * @param theta_i Ice content (m^3/m^3)
 */
double vanGenuchten::fromHtoTHETAforICE(const double h, const double theta_i)
{
	//To have same return value as fromHtoTHETA, call this function with theta_i==0.
	return fromHtoTHETA(h)-(theta_i*(Constants::density_ice/Constants::density_water));
}


/**
 * @brief Specific moisture capacity\n
 * This function should return *exact* value of the derivative d.theta / d.h, otherwise RE-solver is unstable.
 * When modifying the water retention curve, for example in the dry limit, the specific moisture capacity function needs to be modified too, to remain an exact derivative.\n
 * @author Nander Wever
 * @param h Pressure head (m)
 */
double vanGenuchten::dtheta_dh(const double h) {
	// To determine derivative in wxMaxima, do the following:
	// > theta(h) = theta_r + ( (theta_s-theta_r)*(1./Sc)*(1.+((alpha*abs(h))^n))^(-m) )
	// > diff(%o1, h)
	// result: -(pow((alpha*fabs(h)),n)*pow((pow((alpha*fabs(h)), n)+1.),-m-1.)*m*n*(theta_s-theta_r))/(h*Sc), rewrites to:
	return alpha*n*m*((theta_s-theta_r)/Sc)*(pow((alpha*fabs(h)), (n-1.)))*(pow(1.+pow((alpha*fabs(h)), n), (-m-1.)));
}


/**
 * @brief Initialize van Genuchten model for snow layers\n
 * @author Nander Wever
 * @param VGModelTypeSnow van Genuchten model parameterization to use
 * @param K_PARAM hydraulic conductivity parameterization to use
 * @param matrix true: set parameters for matrix domain. false: set parameters for preferential flow domain
 */
void vanGenuchten::SetVGParamsSnow(const VanGenuchten_ModelTypesSnow VGModelTypeSnow, const K_Parameterizations K_PARAM, const bool& matrix, const bool& seaice)
{
	if (EMS->theta[ICE] > 0.75) {
		theta_r=0.;
	} else {
		if(matrix==true) {
			//Scaling theta_r between 0 and 0.02:
			const double TuningFactor=0.75;				//Tuning factor for scaling
			//Increase theta_r in case of wetting:
			theta_r=std::max(0., std::min(0.02, std::max(theta_r, TuningFactor*EMS->theta[WATER])));
			//Decrease theta_r in case of refreezing:
			theta_r=std::max(0., std::min(theta_r, EMS->theta[WATER]-(ReSolver1d::REQUIRED_ACCURACY_THETA/10.)));
		} else {
			//For preferential flow, we fix theta_r to 0:
			theta_r=0.;
		}
	}

	theta_s=(1. - EMS->theta[ICE])*(Constants::density_ice/Constants::density_water);

	if(theta_s<Constants::eps2) {
		// This case is not handled well
		std::cout << "ERROR: theta_s= " << theta_s << "   theta_r=" << theta_r << "\n";
		throw;
	}
	//Make sure theta_r << theta_s
	if(theta_s<theta_r+0.001) {
		std::cout << "WARNING: theta_s= " << theta_s << "   theta_r=" << theta_r << "\n";
		// theta_r is only allowed to decrease when this condition occurs
		theta_r=std::min(theta_s/4., theta_r);
	}

	//Note: rg is in mm, and it is the radius (confirmed by Charles, see DataClasses.h)
	const double tmprg=EMS->rg;	//Backup original grain size value

	switch ( VGModelTypeSnow ) {	//Set Van Genuchten parameters for snow, depending on the chosen model for snow.

		case YAMAGUCHI2012:
		{
			//Calculate ratio density/grain size (see Yamaguchi (2012)):
			double tmp_rho_d=(EMS->theta[ICE]*Constants::density_ice)/( (2.*EMS->rg) / 1000.);
			//Limit tmp_rho_d to reasonable values, so alpha and especially n remain in numerically stable bounds:
			if(seaice) {
				tmp_rho_d=std::max(100000., tmp_rho_d);
			} else {
				tmp_rho_d=std::max(2000., tmp_rho_d);
			}
			alpha=4.4E6*pow(tmp_rho_d, -0.98);	//See Eq. 6 in Yamaguchi (2012).
			n=1.+2.7E-3*pow(tmp_rho_d, 0.61);	//See Eq. 7 in Yamaguchi (2012).
			break;
		}

		case YAMAGUCHI2010:
		{
			//Limit grain size, to stay within the bounds of the Van Genuchten parameterizations for snow.
			const double GRAINRADIUSLOWERTHRESHOLD=0.0;		//Lower threshold
			const double GRAINRADIUSUPPERTHRESHOLD=2.0;		//Upper threshold. 2.02 is value for n>1, which is required.
			//Now limit grain sizes
			if(EMS->rg>GRAINRADIUSUPPERTHRESHOLD) EMS->rg=GRAINRADIUSUPPERTHRESHOLD;
			if(EMS->rg<GRAINRADIUSLOWERTHRESHOLD) EMS->rg=GRAINRADIUSLOWERTHRESHOLD;

			//Note: rg is in mm, and it is the radius (confirmed by Charles, see DataClasses.h)
			alpha=7.3*(2.*EMS->rg)+1.9;			//See Eq. 12 (note d is defined as diameter in mm!) in Yamaguchi (2010).
			n=-3.3*(2.*EMS->rg)+14.4;			//See Eq. 11 (note d is defined as diameter in mm!) in Yamaguchi (2010).
			break;
		}

		case YAMAGUCHI2010_ADAPTED:
		{
			//Limit grain size, the parameterizations still hold, but high values of alpha and small values of n are causing numerical troubles.
			const double GRAINRADIUSLOWERTHRESHOLD=0.0;		//Lower threshold
			const double GRAINRADIUSUPPERTHRESHOLD=4.0;		//Upper threshold
			//Now limit grain sizes
			if(EMS->rg>GRAINRADIUSUPPERTHRESHOLD) EMS->rg=GRAINRADIUSUPPERTHRESHOLD;
			if(EMS->rg<GRAINRADIUSLOWERTHRESHOLD) EMS->rg=GRAINRADIUSLOWERTHRESHOLD;

			alpha=7.3*(2.*EMS->rg)+1.9;			//See Eq. 12 (note d is defined as diameter in mm!) in Yamaguchi (2010).
			//Instead of the linear fit in Yamaguchi (2010), Hirashima (2011) approximated the data with a power law fit, valid for the whole range of grain sizes:
			n=15.68*exp(-0.46*(2.*EMS->rg)) + 1.;		//Hirashima (2011), Eq. 17
			break;
		}

		case DAANEN:
		{
			const double GRAINRADIUSLOWERTHRESHOLD=0.0;		//Equal to Yamaguchi adapted
			const double GRAINRADIUSUPPERTHRESHOLD=4.0;		//Equal to Yamaguchi adapted
			//Now limit grain sizes
			if(EMS->rg>GRAINRADIUSUPPERTHRESHOLD) EMS->rg=GRAINRADIUSUPPERTHRESHOLD;
			if(EMS->rg<GRAINRADIUSLOWERTHRESHOLD) EMS->rg=GRAINRADIUSLOWERTHRESHOLD;

			alpha=30.*(2.*EMS->rg)+12.;
			n=0.800*(2.*EMS->rg)+3.;
			break;
		}
		default:
			throw mio::UnknownValueException("Unknown Van Genuchten parameter for snow", AT);

	}


	const double tmp_dynamic_viscosity_water=0.001792;				//In Pa/s, from WaterTransport code by Hirashima: 0.001792
	if (1. - EMS->theta[ICE] > 0.25) {						//For low density
		switch ( K_PARAM ) {	//Set saturated hydraulic conductivity

		case SHIMIZU:
			//This formulation for ksat is proposed by Shimizu (1970), and is valid up to 450 kg/m^3. See Equation 5 in Jordan, 1999 + conversion from hydraulic permeability to hydraulic conductivity.
			if(EMS->theta[ICE] * Constants::density_ice>450.) {
				ksat=0.077 * (2.*EMS->rg / 1000.)*(2.*EMS->rg / 1000.) * exp(-0.0078 * 450.) * (Constants::g * Constants::density_water) / tmp_dynamic_viscosity_water;
			} else {
				ksat=0.077 * (2.*EMS->rg / 1000.)*(2.*EMS->rg / 1000.) * exp(-0.0078 * EMS->theta[ICE] * Constants::density_ice) * (Constants::g * Constants::density_water) / tmp_dynamic_viscosity_water;
			}
			break;

		case CALONNE:
			//See: Calonne et al., 3-D image-based numerical computations of snow permeability: links to specific surface area, density, and microstructural anisotropy, TC, 2012.
			ksat=0.75 * (EMS->ogs / 1000.)*(EMS->ogs / 1000.) * exp(-0.013 * EMS->theta[ICE] * Constants::density_ice) * (Constants::g * Constants::density_water) / tmp_dynamic_viscosity_water;
			break;

		default:
			throw mio::UnknownValueException("Unknown hydraulic conductivity parameter", AT);

		}
	} else {									//For high density
		// Eq. 5 in Golden, K. M., H. Eicken, A. L. Heaton, J. Miner, D. J. Pringle, and J. Zhu (2007), Thermal evolution of permeability and microstructure in sea ice, Geophys. Res. Lett., 34, L16501, doi:10.1029/2007GL030447:
		ksat = 3E-8 * pow((1. - std::min(1., EMS->theta[ICE])), 3.) * (Constants::g * Constants::density_water) / tmp_dynamic_viscosity_water;
	}

	if (seaice) ksat = std::min(ksat, 1E-3);

	//Set air entry pressure
	h_e=vanGenuchten::AirEntryPressureHead(0.005, 273.);

	//Calculate m:
	m=(n-1.)/n;

	//Calculate saturation at cut-off point h_e (see Ippisch et al (2006)).
	Sc=pow((1.+pow(alpha*fabs(h_e), n)), -1.*m);

	//Restore original grain size value from backup
	EMS->rg=tmprg;

	//The VG model has been initialized
	defined=true;

	return;
}


/**
 * @brief Initialize van Genuchten model for soil layers, based on index approach via grain size\n
 * @author Nander Wever
 */
void vanGenuchten::SetVGParamsSoil()
{
	if(EMS->rg < 0.5) {
		SetSoil(ORGANIC);
	} else if (EMS->rg < 1.) {
		SetSoil(CLAY);
	} else if (EMS->rg < 2.) {
		SetSoil(CLAYLOAM);
	} else if (EMS->rg < 3.) {
		SetSoil(LOAM);
	} else if (EMS->rg < 4.) {
		SetSoil(LOAMYSAND);
	} else if (EMS->rg < 5.) {
		SetSoil(SAND);
	} else if (EMS->rg < 6.) {
		SetSoil(SANDYCLAY);
	} else if (EMS->rg < 7.) {
		SetSoil(SANDYCLAYLOAM);
	} else if (EMS->rg < 8.) {
		SetSoil(SANDYLOAM);
	} else if (EMS->rg < 9.) {
		SetSoil(SILT);
	} else if (EMS->rg < 10.) {
		SetSoil(SILTYCLAY);
	} else if (EMS->rg < 11.) {
		SetSoil(SILTYCLAYLOAM);
	} else if (EMS->rg < 12.) {
		SetSoil(SILTLOAM);
	} else {
		SetSoil(WFJGRAVELSAND);
	}

	//Calculate m:
	m=(n-1.)/n;

	//Calculate saturation at cut-off point h_e (see Ippisch et al (2006)).
	Sc=pow((1.+pow(alpha*fabs(h_e), n)), -1.*m);

	//The VG model has been initialized
	defined=true;

	//I encountered the following problem: fully saturated soil and freezing water: there is not enough place to store the ice!!!
	//In the old snowpack code, this problem was solved by keeping the increase in volume when all the water in the element would freeze, free as theta[AIR].
	//However, this will not work in the Richards, as theta[WATER] is varying per time step. So we keep free a volume as if the soil is saturated AND will freeze:
	EMS->theta[SOIL]=1.-((Constants::density_water/Constants::density_ice)*theta_s);	//Determine the soil content based on the pore space

	//The VG model has been initialized
	defined=true;

	return;
}
