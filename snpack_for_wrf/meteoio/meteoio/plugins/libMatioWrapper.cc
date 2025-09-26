/***********************************************************************************/
/*  Copyright 2009 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#include <meteoio/plugins/libMatioWrapper.h>
#include <meteoio/FileUtils.h>

using namespace std;
using namespace mio;

namespace matWrap {

/**********************************************************************************
 * Here we define some wrappers around libmatio. These are not declared as class methods in
 * order to avoid having to expose matio.h when including OshdIO.h
 **********************************************************************************/
void listFields(matvar_t *matvar)
{
	const unsigned int nrFields = Mat_VarGetNumberOfFields(matvar);
	char * const *fields = Mat_VarGetStructFieldnames(matvar);
	for (unsigned int ii=0; ii<nrFields; ii++)
		printf("field[%d] = %s\n", ii, fields[ii]);
}

void printStructure(matvar_t *matvar)
{
	//Mat_VarPrint(field, 0);
	printf("name=%s class_type=%d data_type=%d rank=%d", matvar->name, matvar->class_type, matvar->data_type, matvar->rank);
	for (int ii=0; ii<matvar->rank; ii++)
		printf("\tdims[%d]=%d", ii, (int)matvar->dims[ii]);
	printf("\n");
}


std::string readString(const std::string &filename, const std::string &fieldname, mat_t *matfp, matvar_t *matvar)
{
	matvar_t *field = Mat_VarGetStructFieldByName(matvar, fieldname.c_str(), 0);
	if (matvar==NULL) throw NotFoundException("could not read field '"+fieldname+"' in file '"+filename+"'", AT);
	if (Mat_VarReadDataAll(matfp, field))
		throw InvalidFormatException("could not read field '"+fieldname+"' in file '"+filename+"'", AT);
	if (field->class_type!=MAT_C_CHAR) throw InvalidFormatException("field '"+fieldname+"' in file '"+filename+"' is not a type string", AT);

	return std::string( static_cast<char*>(field->data) );
}

double readDouble(const std::string &filename, const std::string &fieldname, mat_t *matfp, matvar_t *matvar)
{
	matvar_t *field = Mat_VarGetStructFieldByName(matvar, fieldname.c_str(), 0);
	if (matvar==NULL) throw NotFoundException("could not read field '"+fieldname+"' in file '"+filename+"'", AT);
	if (Mat_VarReadDataAll(matfp, field))
		throw InvalidFormatException("could not read field '"+fieldname+"' in file '"+filename+"'", AT);
	if (field->class_type!=MAT_C_DOUBLE) throw InvalidFormatException("field '"+fieldname+"' in file '"+filename+"' is not a type double", AT);

	if (field->rank!=2) throw InvalidFormatException("invalid rank for field '"+fieldname+"' in file '"+filename+"'", AT);

	const size_t nrows = field->dims[0];
	const size_t ncols = field->dims[1];
	if (nrows!=1 || ncols!=1) throw InvalidFormatException("invalid nrows/ncols for field '"+fieldname+"' in file '"+filename+"'", AT);

	const double* matData( static_cast<double*>( field->data ) );
	return matData[0];
}

std::vector<std::string> readStringVector(const std::string &filename, const std::string &fieldname, mat_t *matfp, matvar_t *matvar)
{
	matvar_t *field = Mat_VarGetStructFieldByName(matvar, fieldname.c_str(), 0);
	if (matvar==NULL) 	throw NotFoundException("could not read field '"+fieldname+"' in file '"+filename+"'", AT);
	if (Mat_VarReadDataAll(matfp, field))
		throw InvalidFormatException("could not read field '"+fieldname+"' in file '"+filename+"'", AT);

	if (field->class_type!=MAT_C_CELL) throw InvalidFormatException("field '"+fieldname+"' in file '"+filename+"' is not a cell type", AT);
	if (field->data_type!=MAT_T_CELL) throw InvalidFormatException("field '"+fieldname+"' in file '"+filename+"' is not a cell array data type", AT);

	matvar_t *cell = Mat_VarGetCell(matvar, 0);
	if (cell==NULL) throw InvalidFormatException("could not read data in field '"+fieldname+"' in file '"+filename+"'", AT);
	if (field->rank!=2) throw InvalidFormatException("invalid rank for field '"+fieldname+"' in file '"+filename+"'", AT);

	const size_t nrows = field->dims[0];
	const size_t ncols = field->dims[1];
	if (nrows!=1) throw InvalidFormatException("invalid nrows for field '"+fieldname+"' in file '"+filename+"'", AT);

	std::vector<std::string> vecString( ncols );
	for (size_t ii=0; ii<ncols; ii++) {
		cell = Mat_VarGetCell(field, static_cast<int>(ii));
		if (cell->rank!=2) throw InvalidFormatException("invalid cell rank in file '"+filename+"'", AT);
		if (cell->class_type!=MAT_C_CHAR) throw InvalidFormatException("field '"+fieldname+"' in file '"+filename+"' is not a type string", AT);
		vecString[ii] = static_cast<char*>(cell->data);
	}

	return vecString;
}

std::vector<double> readDoubleVector(const std::string &filename, const std::string &fieldname, mat_t *matfp, matvar_t *matvar)
{
	matvar_t *field = Mat_VarGetStructFieldByName(matvar, fieldname.c_str(), 0);
	if (matvar==NULL) 	throw NotFoundException("could not read field '"+fieldname+"' in file '"+filename+"'", AT);
	if (Mat_VarReadDataAll(matfp, field))
		throw InvalidFormatException("could not read field '"+fieldname+"' in file '"+filename+"'", AT);

	if (field->class_type!=MAT_C_DOUBLE) throw InvalidFormatException("field '"+fieldname+"' in file '"+filename+"' is not a double type", AT);
	if (field->rank!=2) throw InvalidFormatException("invalid rank for field '"+fieldname+"' in file '"+filename+"'", AT);

	const size_t nrows = field->dims[0];
	const size_t ncols = field->dims[1];
	if (nrows!=1) throw InvalidFormatException("invalid nrows for field '"+fieldname+"' in file '"+filename+"'", AT);

	std::vector<double> vecData( ncols );
	const double* matData( static_cast<double*>( field->data ) );
	for (size_t ii=0; ii<ncols; ii++) {
		vecData[ii] = matData[ii];
	}

	return vecData;
}

void readDoubleArray(const std::string &filename, const std::string &fieldname, mat_t *matfp, matvar_t *matvar, mio::Array2D<double> &array)
{
	array.clear();
	const double nodata( readDouble(filename, "NODATA_value", matfp, matvar) );

	matvar_t *field = Mat_VarGetStructFieldByName(matvar, fieldname.c_str(), 0);
	if (matvar==NULL) 	throw NotFoundException("could not read field '"+fieldname+"' in file '"+filename+"'", AT);
	if (Mat_VarReadDataAll(matfp, field))
		throw InvalidFormatException("could not read field '"+fieldname+"' in file '"+filename+"'", AT);

	if (field->class_type!=MAT_C_DOUBLE) throw InvalidFormatException("field '"+fieldname+"' in file '"+filename+"' is not a double type", AT);
	if (field->rank!=2) throw InvalidFormatException("invalid rank for field '"+fieldname+"' in file '"+filename+"'", AT);

	const size_t nrows = field->dims[0];
	const size_t ncols = field->dims[1];
	array.resize(ncols, nrows);

	const double* matData( static_cast<double*>( field->data ) );
	for (size_t ii=0; ii<ncols; ii++) {
		for (size_t jj=0; jj<nrows; jj++) {
			const double tmp( matData[ii*nrows+jj] );
			if (tmp!=nodata) array(ii,jj) = tmp;
			else array(ii,jj) = IOUtils::nodata;
		}
	}
}

void printFileStructure(const std::string& filename, const double& TZ)
{
	mat_t *matfp = Mat_Open(filename.c_str(), MAT_ACC_RDONLY);
	if ( NULL == matfp ) throw AccessException(filename, AT);

	std::cout << "<" << FileUtils::getFilename( filename ) << ">\n";
	matvar_t *matvar;
	while ( (matvar = Mat_VarReadNextInfo(matfp)) != NULL ) {
		std::cout << "\t" << matvar->name << " [";
		for (int ii=0; ii<matvar->rank; ii++) {
			std::cout << (int)matvar->dims[ii];
			if (ii<(matvar->rank-1)) std::cout << "x";
		}
		std::cout << "]\n";

		const unsigned int nrFields = Mat_VarGetNumberOfFields(matvar);
		char * const *fields = Mat_VarGetStructFieldnames(matvar);
		for (unsigned int ii=0; ii<nrFields; ii++) {
			const std::string field_name( fields[ii] );
			matvar_t *field = Mat_VarGetStructFieldByName(matvar, field_name.c_str(), 0);
			const std::string prefix = (ii<(nrFields-1))? "├──" : "└──";
			std::cout << "\t" << prefix << field_name;
			if (field->class_type==MAT_C_CHAR)
				std::cout << " = \"" << readString(filename, field_name, matfp, matvar) << "\"";
			if (field->class_type==MAT_C_DOUBLE) {
				std::cout << " [";
				size_t count=1;
				for (int jj=0; jj<field->rank; jj++) {
					std::cout << field->dims[jj];
					if (jj<(field->rank-1)) std::cout << "x";
					count *= field->dims[jj];
				}
				std::cout << "]";
				if (count==1) {
					if (Mat_VarReadDataAll(matfp, field))
						throw InvalidFormatException("could not read field '"+field_name+"' in file '"+filename+"'", AT);
					const double val = static_cast<double*>(field->data)[0];
					if (field_name=="time") {
						Date timestep;
						timestep.setMatlabDate( val, TZ );
						std::cout << " = " << timestep.toString(Date::ISO_TZ);
					} else
						std::cout << " = " << val;
				}
			}
			if (field->class_type==MAT_C_CELL) {
				std::cout << " [";
				for (int jj=0; jj<field->rank; jj++) {
					std::cout << field->dims[jj];
					if (jj<(field->rank-1)) std::cout << "x";
				}
				std::cout << "]";
			}

			std::cout << "\n";
		}
	}
	std::cout << "</" << FileUtils::getFilename( filename ) << ">\n\n";
	Mat_VarFree(matvar);
	matvar = NULL;
	Mat_Close(matfp);
}

} //namespace
