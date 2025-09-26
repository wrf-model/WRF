/***********************************************************************************/
/*  Copyright 2016 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef LIBMATIOWRAPPER_H
#define LIBMATIOWRAPPER_H

#include <meteoio/dataClasses/Array2D.h>

#include <matio.h>
#include <string>

namespace matWrap {
	void listFields(matvar_t *matvar);
	void printStructure(matvar_t *matvar);
	std::string readString(const std::string &filename, const std::string &fieldname, mat_t *matfp, matvar_t *matvar);
	double readDouble(const std::string &filename, const std::string &fieldname, mat_t *matfp, matvar_t *matvar);
	std::vector<std::string> readStringVector(const std::string &filename, const std::string &fieldname, mat_t *matfp, matvar_t *matvar);
	std::vector<double> readDoubleVector(const std::string &filename, const std::string &fieldname, mat_t *matfp, matvar_t *matvar);
	void readDoubleArray(const std::string &filename, const std::string &fieldname, mat_t *matfp, matvar_t *matvar, mio::Array2D<double> &array);
	void printFileStructure(const std::string& filename, const double& TZ);
} //namespace

#endif