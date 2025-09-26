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

#include <snowpack/snowpackCore/Aggregate.h>
#include <snowpack/Constants.h>

/************************************************************
 * static section                                           *
 ************************************************************/

const double Aggregate::limit_dry     = 0.001; ///< Distinguishes between dry and wet snow layers (1)
const double Aggregate::diff_theta_w  = 0.7;   ///< Maximum water difference for aggregation (% by volume)
const double Aggregate::diff_jul      = 1.0;   ///< Maximum  age difference for aggregation (d)
const double Aggregate::diff_dg       = 0.25;  ///< Maximum  grain size difference for aggregation (mm)
const double Aggregate::diff_dg_rel   = 0.2;   ///< Maximum  relative grain size difference for aggregation (mm)
const double Aggregate::diff_sp       = 0.2;   ///< Maximum  sphericity difference for aggregation (1)
const double Aggregate::diff_dd       = 0.2;   ///< Maximum  dendricity difference for aggregation (1)
const double Aggregate::min_l_element = 0.3;   ///< Minimum length of element to be kept separate (cm)

/**
 * @brief Eliminate the "empty" layers shift the remaining layers to form a compact snowpack
 * @param nL_ini Iinitial number of layers prior to aggregation
 * @param Pdata A vector of SnowProfileLayer
 */
void Aggregate::shift(const size_t& nL_ini, std::vector<SnowProfileLayer>& Pdata)
{
	for (size_t ll=1, l_new=1; ll<nL_ini; ll++) {
		if (Pdata[ll].height != Constants::undefined) {
			if (l_new != ll) {
				Pdata[l_new].depositionDate = Pdata[ll].depositionDate;
				Pdata[l_new].height = Pdata[ll].height;
				Pdata[l_new].rho = Pdata[ll].rho;
				Pdata[l_new].T = Pdata[ll].T;
				Pdata[l_new].gradT = Pdata[ll].gradT;
				Pdata[l_new].v_strain_rate = Pdata[ll].v_strain_rate;
				Pdata[l_new].theta_i = Pdata[ll].theta_i;
				Pdata[l_new].theta_w = Pdata[ll].theta_w;
				Pdata[l_new].grain_size = Pdata[ll].grain_size;
				Pdata[l_new].dendricity = Pdata[ll].dendricity;
				Pdata[l_new].sphericity = Pdata[ll].sphericity;
				Pdata[l_new].ogs = Pdata[ll].ogs;
				Pdata[l_new].bond_size = Pdata[ll].bond_size;
				Pdata[l_new].coordin_num = Pdata[ll].coordin_num;
				Pdata[l_new].marker = Pdata[ll].marker;
				Pdata[l_new].hard = Pdata[ll].hard;
			}
			l_new++;
		}
	}
}

/**
* @brief Decide whether to join two similar layers
* @param l_upper index of upper layer
* @param Pdata A vector of SnowProfileLayer
*/
bool Aggregate::joinSimilarLayers(const size_t& l_upper, std::vector<SnowProfileLayer>& Pdata)
{
	const size_t l_lower = l_upper-1;

	if ((Pdata[l_upper].theta_w < limit_dry) || (Pdata[l_lower].theta_w < limit_dry)) {
		if (fabs(Pdata[l_upper].theta_w - Pdata[l_lower].theta_w) > limit_dry)
			return false;
	} else {
		if (fabs(Pdata[l_upper].theta_w - Pdata[l_lower].theta_w) > diff_theta_w)
			return false;
	}

	if (fabs(Pdata[l_upper].depositionDate.getJulian() - Pdata[l_lower].depositionDate.getJulian()) > diff_jul)
		return false;

	if (Pdata[l_upper].marker != Pdata[l_lower].marker)
		return false;

	// do not combine layers which are of quite different hardness 020917;Fz
	if (fabs(Pdata[l_upper].hard - Pdata[l_lower].hard) > 1.0)
		return false;

	if ((Pdata[l_upper].dendricity == 0) && (Pdata[l_lower].dendricity == 0)){
		if ( fabs(Pdata[l_upper].sphericity - Pdata[l_lower].sphericity) > diff_sp)
			return false;

		if (fabs(Pdata[l_upper].grain_size - Pdata[l_lower].grain_size) > std::max(diff_dg, diff_dg_rel * Pdata[l_upper].grain_size))
			return false;
	} else {
		if (fabs(Pdata[l_upper].sphericity - Pdata[l_lower].sphericity) > diff_sp)
			return false;

		if (fabs(Pdata[l_upper].dendricity - Pdata[l_lower].dendricity) > diff_dd)
			return false;
	}

	return true;
}

bool Aggregate::joinSimilarLayers(ElementData& Edata_upper, ElementData& Edata_lower)
{
	if ((Edata_upper.theta[WATER] < limit_dry) || (Edata_lower.theta[WATER] < limit_dry)) {
		if (fabs(Edata_upper.theta[WATER] - Edata_lower.theta[WATER]) > limit_dry)
			return false;
	} else {
		if (fabs(Edata_upper.theta[WATER] - Edata_lower.theta[WATER]) > diff_theta_w)
			return false;
	}

	if (fabs(Edata_upper.depositionDate.getJulian() - Edata_lower.depositionDate.getJulian()) > diff_jul)
		return false;

	if (Edata_upper.mk != Edata_lower.mk)
		return false;

	// do not combine layers which are of quite different hardness 020917;Fz
	if (fabs(Edata_upper.hard - Edata_lower.hard) > 1.0)
		return false;

	if ((Edata_upper.dd == 0) && (Edata_lower.dd == 0)){
		if ( fabs(Edata_upper.sp - Edata_lower.sp) > diff_sp)
			return false;

		if (fabs(Edata_upper.rg - Edata_lower.rg) > std::max(diff_dg, diff_dg_rel * Edata_upper.rg))
			return false;
	} else {
		if (fabs(Edata_upper.sp - Edata_lower.sp) > diff_sp)
			return false;

		if (fabs(Edata_upper.dd - Edata_lower.dd) > diff_dd)
			return false;
	}

	return true;
}

/**
 * @brief Decide whether to merge thin layers
 * @param l_lower Index of upper layer
 * @param Pdata A vector of SnowProfileLayer
 */
bool Aggregate::mergeThinLayer(const size_t& l_lower, std::vector<SnowProfileLayer>& Pdata)
{
	const size_t l_upper = l_lower+1;

	// if a dry layer is involved
	if ((Pdata[l_lower].theta_w < limit_dry) || (Pdata[l_upper].theta_w < limit_dry)){
		// do not combine dry with moist layers
		if (fabs(Pdata[l_lower].theta_w - Pdata[l_upper].theta_w) > limit_dry)
			return false;

		// do not combine layers which are of quite different age
		if (fabs(Pdata[l_lower].depositionDate.getJulian() - Pdata[l_upper].depositionDate.getJulian()) > 2 * diff_jul)
			return false;

		// do not combine layers with different grain classes
		if (Pdata[l_lower].marker != Pdata[l_upper].marker) {
			return false;
		}
		// do not combine layers which are of quite different hardness
		if (fabs(Pdata[l_lower].hard - Pdata[l_upper].hard) > 1.5) {
			return false;
		}
	}
	// for two wet layers
	else {
		if ((Pdata[l_lower].grain_size < 0.75) || (Pdata[l_lower].sphericity < 0.5) ||
			(Pdata[l_upper].grain_size < 0.75) || (Pdata[l_upper].sphericity < 0.5)) {
			if (Pdata[l_lower].marker != Pdata[l_upper].marker) {
				return false;
			}
		}
		else {
			if (!( (Pdata[l_lower].marker > 9 &&  Pdata[l_lower].marker < 13) ||
				(Pdata[l_lower].marker > 19 &&  Pdata[l_lower].marker < 23) )) {
				//no need to test on l_upper since marker(lower) must be = marker(upper)
				if (Pdata[l_lower].marker != Pdata[l_upper].marker)
					return false;
			}
		}
	}
	return true;
}

bool Aggregate::mergeThinLayer(ElementData& Edata_upper, ElementData& Edata_lower)
{
	size_t upper_mk=Edata_upper.mk%100;
	size_t lower_mk=Edata_lower.mk%100;

	// if a dry layer is involved
	if ((Edata_lower.theta[WATER] < limit_dry) || (Edata_upper.theta[WATER] < limit_dry)){
		// do not combine dry with moist layers
		if (fabs(Edata_lower.theta[WATER] - Edata_upper.theta[WATER]) > limit_dry)
			return false;

		// do not combine layers which are of quite different age
		if (fabs(Edata_lower.depositionDate.getJulian() - Edata_upper.depositionDate.getJulian()) > 2 * diff_jul)
			return false;

		// do not combine layers with different grain classes
		if (lower_mk != upper_mk) {
			return false;
		}
		// do not combine layers which are of quite different hardness
		if (fabs(Edata_lower.hard - Edata_upper.hard) > 1.5) {
			return false;
		}
	}
	// for two wet layers
	else {
		if ((Edata_lower.rg < 0.75) || (Edata_lower.sp < 0.5) ||
			(Edata_upper.rg < 0.75) || (Edata_upper.sp < 0.5)) {
			if (Edata_lower.mk != Edata_upper.mk) {
				return false;
			}
		}
		else {
			if (!( (lower_mk > 9 &&  lower_mk < 13) ||
				(lower_mk > 19 &&  lower_mk < 23) )) {
				//no need to test on l_upper since marker(lower) must be = marker(upper)
				if (lower_mk != upper_mk)
					return false;
			}
		}
	}
	return true;
}

/**
 * @brief Aggregate snow profile layers and compute the grain class
 * @note Leave top 5 elements untouched
 * @param Pdata A vector of SnowProfileLayer
 */
size_t Aggregate::aggregate(std::vector<SnowProfileLayer>& Pdata)
{
	size_t nL = Pdata.size();

	// Initialize number of layers and aggregate only if more than 5 layers
	if (nL > 5) {
		// First Run - aggregate similar layers
		// keep track of the coordinates and length of elements
		double L0_lower = (Pdata[nL-2].height -  Pdata[nL-3].height);

		for (size_t l_upper=nL-2; l_upper > 0; l_upper--) {
			const double L0_upper = L0_lower;
			const size_t l_lower = l_upper-1;
			if (l_lower > 0) {
				L0_lower = (Pdata[l_lower].height -  Pdata[l_lower-1].height);
			} else {
				L0_lower = Pdata[l_lower].height;
			}
			// if two layers are similar combine them; keep SH though
			if ((Pdata[l_lower].marker != 3) && (Pdata[l_upper].marker != 3)) {
				if (joinSimilarLayers(l_upper, Pdata)) {
					Pdata[l_lower].average(L0_lower, L0_upper, Pdata[l_upper]);
					Pdata.erase(Pdata.begin()+static_cast<std::ptrdiff_t>(l_upper));
					L0_lower += L0_upper;
				}
			}
		}

		nL = Pdata.size();
		// Second Run - aggregate remaining very thin layers
		if (nL > 2) {
			bool flag = false;
			L0_lower = (Pdata[nL-2].height -  Pdata[nL-3].height); //reset L0_lower
			for(size_t l_upper = nL-2; l_upper > 0; l_upper--) {
				const size_t l_lower = l_upper-1;
				const double L0_upper = L0_lower;
				if (l_lower > 0) {
					L0_lower = (Pdata[l_lower].height -  Pdata[l_lower-1].height);
				} else {
					L0_lower = Pdata[l_lower].height;
				}
				if ((Pdata[l_lower].marker != 3) && (Pdata[l_upper].marker != 3)) {
					// trick to try to join with upper or lower level -> use flag to mark thin layer
					if (flag || (L0_lower < (sqrt(Pdata[nL-1].height-Pdata[l_lower].height)/4.))
						         || (L0_lower < min_l_element)) {
						// if two layers are similar or one layer is very very small combine them
						if (mergeThinLayer(l_upper, Pdata)
							    || (L0_lower < min_l_element) || (L0_upper < min_l_element)) {
							Pdata[l_lower].average(L0_lower, L0_upper, Pdata[l_upper]);
							Pdata.erase(Pdata.begin()+static_cast<std::ptrdiff_t>(l_upper));
							L0_lower += L0_upper;
							flag = false;
						} else {
							flag = true;
						}
					} else {
						flag = false;
					}
				} // if not surface hoar layer
				else {
					flag = false;
				}
			}  // for all elements
		} // if nL_ini > 2
	} // if more than 5 layers

	nL = Pdata.size();
	// Update snow type
	for(size_t ll=0; ll<nL; ll++) {
		Pdata[ll].type = ElementData::snowType(Pdata[ll].dendricity, Pdata[ll].sphericity,
                                               Pdata[ll].grain_size, Pdata[ll].marker, Pdata[ll].theta_w/100.,
                                               ElementData::snowResidualWaterContent(Pdata[ll].theta_i/100.));
	}
	return nL;
}
