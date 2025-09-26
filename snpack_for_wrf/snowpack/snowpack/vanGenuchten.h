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
 * @file vanGenuchten.h
 */
#ifndef VANGENUCHTEN_H
#define VANGENUCHTEN_H

#include <sstream>

/**
 * @class vanGenuchten
 * @version 1.0
 * @brief This module contains the van Genuchten model for the water retention curve
 * @author Nander Wever
 */
class ElementData;

class vanGenuchten {
	ElementData *EMS;	// Reference to the ElementData where the vanGenuchten class belongs to

	public:
		vanGenuchten(ElementData &pEMS);
		vanGenuchten(const vanGenuchten& c);
		virtual ~vanGenuchten() {}
		vanGenuchten& operator=(const vanGenuchten&); ///<Assignement operator
		friend std::iostream& operator<<(std::iostream& os, const vanGenuchten& data);
		friend std::iostream& operator>>(std::iostream& is, vanGenuchten& data);

		//Soil types
		enum SoilTypes{ORGANIC, CLAY, CLAYLOAM, LOAM, LOAMYSAND, SAND, SANDYCLAY, SANDYCLAYLOAM, SANDYLOAM, SILT, SILTYCLAY, SILTYCLAYLOAM, SILTLOAM, WFJGRAVELSAND};
		//Van genuchten model types
		enum VanGenuchten_ModelTypesSnow{YAMAGUCHI2012, YAMAGUCHI2010, YAMAGUCHI2010_ADAPTED, DAANEN};
		//Hydraulic conductivity parameterizations
		enum K_Parameterizations{SHIMIZU, CALONNE};

		// Functions
		static double AirEntryPressureHead(double MaximumPoreSize, double Temperature);

		// Van Genuchten functions
		double fromTHETAtoH(const double theta, const double h_d);
		double fromTHETAtoHforICE(const double theta, const double h_d, const double theta_i);
		double fromHtoTHETA(const double h);
		double fromHtoTHETAforICE(const double h, const double theta_i);
		double dtheta_dh(const double h);

		// Functions to initialize the van Genuchten model
		void SetVGParamsSnow(VanGenuchten_ModelTypesSnow VGModelTypeSnow, K_Parameterizations K_PARAM, const bool& matrix, const bool& seaice);
		void SetVGParamsSoil();

		double theta_r;	//Soil property, residual water content.
		double theta_s;	//Soil property, saturation water content.
		double alpha;	//Soil property in Van Genuchten model. [m^-1]
		double n;	//Soil property in Van Genuchten model.
		double m;	//Soil property in Van Genuchten model.
		double h_e;	//Soil property, air entry pressure, see Ippisch (2006) for details.
		double Sc;	//Saturation at cut-off point h_e (see Ippisch et al (2006)).
		double ksat;	//Soil property. Saturation hydraulic conductivity.
		double field_capacity; //Soil property, grain size
		bool defined;	//true: the van Genuchten model has been initialized for this layer, false: the van Genuchten model is not initialized and should not be used.

	private:
		void SetSoil(SoilTypes type);
};
#endif // End of vanGenuchten.h}
