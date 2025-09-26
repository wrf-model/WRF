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
#ifndef ARRAY3D_H
#define ARRAY3D_H

#include <meteoio/IOUtils.h>
#include <meteoio/IOExceptions.h>

#include <vector>
#include <limits>
#include <iostream>
#include <numeric>
#include <algorithm>

//forward declaration
namespace mio { template <class T> class Array3D; template <class T> class Array3DProxy2; }
#include <meteoio/dataClasses/Array2D.h>

namespace mio {
/**
 * @class Array3DProxy
 * @brief The template class Array3DProxy is a helper class for the template class Array3D
 *        with the purpose of adding the [][] operator to Array3D
 *
 * @author Thomas Egger
 */
template <class T> class Array3DProxy {
 	public:
		friend class Array3D<T>;
		Array3DProxy2<T> operator[](const size_t& i_any) {
			return Array3DProxy2<T>(array3D, anx, i_any);
		}

 	private:
 		Array3DProxy(Array3D<T>& i_array3D, const size_t& i_anx) : array3D(i_array3D), anx(i_anx){}
		Array3D<T>& array3D;
		const size_t anx;
};

/**
 * @class Array3DProxy2
 * @brief The template class Array3DProxy2 is a helper class for the template class Array3D
 *        with the purpose of adding the [][][] operator to Array3D
 *
 * @author Thomas Egger
 */
template <class T> class Array3DProxy2 {
 	public:
		friend class Array3DProxy<T>;
		T& operator[](const size_t& i_anz) {
			return array3D(anx, any, i_anz);
		}

	private:
 		Array3DProxy2(Array3D<T>& i_array3D, const size_t& i_anx,
				    const size_t& i_any) : array3D(i_array3D), anx(i_anx), any(i_any){}
		Array3D<T>& array3D;
		const size_t anx;
		const size_t any;
};

/**
 * @class Array3D
 * @brief The template class Array3D is a 3D Array (Tensor) able to hold any type of object as datatype.
 * It relies on the Array3DProxy2 class to provide the [][][] operator (slower than the (i,j,k) call).
 * If the compilation flag NOSAFECHECKS is used, bounds check is turned off (leading to increased performances).
 * @ingroup data_str
 * @date  2009-07-19
 * @author Thomas Egger
 */
template<class T> class Array3D {
	public:
		Array3D();

		/**
		* A constructor that can be used to create an Array3D object that is contained in the
		* one passed as i_array3D argument. The resulting Array3D object is a by value copy of
		* a subvolume of the volume spanned by the i_array3D
		* @param i_array3D array containing to extract the values from
		* @param i_nx lower left corner cell X index
		* @param i_ny lower left corner cell Y index
		* @param i_nz lower left corner cell Z index
		* @param i_ncols number of columns of the new array
		* @param i_nrows number of rows of the new array
		* @param i_ndepth number of depths of the new array
		*/
		Array3D(const Array3D<T>& i_array3D,
		        const size_t& i_nx, const size_t& i_ny, const size_t& i_nz,
		        const size_t& i_ncols, const size_t& i_nrows, const size_t& i_ndepth);

		/**
		* A constructor that creates an array of a given size
		* @param anx number of columns of the new array
		* @param any number of rows of the new array
		* @param anz number of rows of the new array
		*/
		Array3D(const size_t& anx, const size_t& any, const size_t& anz);

		/**
		* A constructor that creates an array filled with constant values
		* @param anx number of columns of the new array
		* @param any number of rows of the new array
		* @param anz number of depths of the new array
		* @param init initial value to fill the array with
		*/
		Array3D(const size_t& anx, const size_t& any, const size_t& anz, const T& init);

		/**
		* A method that can be used to create an Array3D object that is contained in the
		* one passed as i_array3D argument. The resulting Array3D object is a by value copy of
		* a subvolume of the volume spanned by the i_array3D
		* @param i_array3D array containing to extract the values from
		* @param i_nx lower left corner cell X index
		* @param i_ny lower left corner cell Y index
		* @param i_nz lower left corner cell Z index
		* @param i_ncols number of columns of the new array
		* @param i_nrows number of rows of the new array
		* @param i_ndepth number of depths of the new array
		*/
		void subset(const Array3D<T>& i_array3D,
		            const size_t& i_nx, const size_t& i_ny, const size_t& i_nz,
		            const size_t& i_ncols, const size_t& i_nrows, const size_t& i_ndepth);

		/**
		* @brief A method that can be used to insert a subplane into an existing Array3D object
		* that is passed as i_array3D argument. This is exactly the opposite of the subset method
		* an can be used to rebuild an array from subsets.
		* @param i_array3D array containing to extract the values from
		* @param i_nx lower left corner cell X index
		* @param i_ny lower left corner cell Y index
		* @param i_nz lower left corner cell Z index
		* @param i_ncols number of columns of the new array
		* @param i_nrows number of rows of the new array
		* @param i_ndepth number of depths of the new array
		*/
		void fill(const Array3D<T>& i_array3D,
		          const size_t& i_nx, const size_t& i_ny, const size_t& i_nz,
		          const size_t& i_ncols, const size_t& i_nrows, const size_t& i_ndepth);

		void fill(const Array3D<T>& i_array3D, const size_t& i_nx, const size_t& i_ny, const size_t& i_nz);

		/**
		* @brief insert a 2D layer in an Array3D object
		* The (nx ,ny) dimensions of the 2D and the 3D arrays must match and the insertion depth must
		* exist in the Array3D.
		* @param layer layer to insert
		* @param depth depth where the insertion should take place
		*/
		void insert(const Array2D<T>& layer, const size_t& depth);

		/**
		* @brief set how to process nodata values (ie: as nodata or as normal numbers)
		* @param i_keep_nodata true means that NODATA is interpreted as NODATA, false means that it is a normal number
		By default, arrays keep nodata.
		*/
		void setKeepNodata(const bool i_keep_nodata);

		/**
		* @brief get how to process nodata values (ie: as nodata or as normal numbers)
		* @return true means that NODATA is interpreted as NODATA, false means that it is a normal number
		*/
		bool getKeepNodata() const;

		void resize(const size_t& anx, const size_t& any, const size_t& anz);
		void resize(const size_t& anx, const size_t& any, const size_t& anz, const T& init);
		void size(size_t& anx, size_t& any, size_t& anz) const;
		size_t size() const;
		size_t getNx() const;
		size_t getNy() const;
		size_t getNz() const;
		void clear();
		bool empty() const;

		/**
		* @brief returns the minimum value contained in the grid
		* @return minimum value
		*/
		T getMin() const;
		/**
		* @brief returns the maximum value contained in the grid
		* @return maximum value
		*/
		T getMax() const;
		/**
		* @brief returns the mean value contained in the grid
		* @return mean value
		*/
		T getMean() const;
		/**
		* @brief returns the number of points contained in the grid.
		* If setNodataHandling(IOUtils::RAW_NODATA), then the number of points is the size of the grid.
		* If setNodataHandling(IOUtils::PARSE_NODATA), then it is the number of non-nodata values in the grid
		* @return count
		*/
		size_t getCount() const;
		/**
		* @brief returns the grid of the absolute value of values contained in the grid
		* @return grid of abs(grid)
		*/
		const Array3D<T> getAbs() const;
		void abs();

		const std::string toString() const;
		template<class P> friend std::ostream& operator<<(std::ostream& os, const Array3D<P>& array);
		template<class P> friend std::istream& operator>>(std::istream& is, Array3D<P>& array);

		bool checkEpsilonEquality(const Array3D<double>& rhs, const double& epsilon) const;
		static bool checkEpsilonEquality(const Array3D<double>& rhs1, const Array3D<double>& rhs2, const double& epsilon);

		T& operator ()(const size_t& i);
		const T operator ()(const size_t& i) const;
		T& operator ()(const size_t& x, const size_t& y, const size_t& z);
		const T operator ()(const size_t& x, const size_t& y, const size_t& z) const;
		Array3DProxy<T> operator[](const size_t& i);

		Array3D<T>& operator =(const Array3D<T>&);
		Array3D<T>& operator =(const T& value);

		Array3D<T>& operator+=(const T& rhs);
		const Array3D<T> operator+(const T& rhs) const;
		Array3D<T>& operator+=(const Array3D<T>& rhs);
		const Array3D<T> operator+(const Array3D<T>& rhs) const;

		Array3D<T>& operator-=(const T& rhs);
		const Array3D<T> operator-(const T& rhs) const;
		Array3D<T>& operator-=(const Array3D<T>& rhs);
		const Array3D<T> operator-(const Array3D<T>& rhs) const;

		Array3D<T>& operator*=(const T& rhs);
		const Array3D<T> operator*(const T& rhs) const;
		Array3D<T>& operator*=(const Array3D<T>& rhs);
		const Array3D<T> operator*(const Array3D<T>& rhs) const;

		Array3D<T>& operator/=(const T& rhs);
		const Array3D<T> operator/(const T& rhs) const;
		Array3D<T>& operator/=(const Array3D<T>& rhs);
		const Array3D<T> operator/(const Array3D<T>& rhs) const;

		bool operator==(const Array3D<T>&) const; ///<Operator that tests for equality
		bool operator!=(const Array3D<T>&) const; ///<Operator that tests for inequality

	protected:
		std::vector<T> vecData; ///< The actual objects are stored in a one-dimensional vector
		size_t nx;
		size_t ny;
		size_t nz;
		size_t nxny; //nx times ny
		bool keep_nodata;
};

template<class T> inline T& Array3D<T>::operator()(const size_t& i) {
#ifndef NOSAFECHECKS
	return vecData.at(i);
#else
	return vecData[i];
#endif
}

template<class T> inline const T Array3D<T>::operator()(const size_t& i) const {
#ifndef NOSAFECHECKS
	return vecData.at(i);
#else
	return vecData[i];
#endif
}

template<class T> inline T& Array3D<T>::operator()(const size_t& x, const size_t& y, const size_t& z) {
#ifndef NOSAFECHECKS
	if ((x >= nx) || (y >= ny) || (z >= nz))  {
		std::ostringstream ss;
		ss << "Trying to access array(" << x << "," << y << "," << z << ")";
		ss << " while array is (" << nx << "," << ny << "," << nz << ")";
		throw IndexOutOfBoundsException(ss.str(), AT);
	}
#endif

	//ROW-MAJOR alignment of the vector: fully C-compatible memory layout
	return vecData[x + y*nx + z*nxny];
}

template<class T> inline const T Array3D<T>::operator()(const size_t& x, const size_t& y, const size_t& z) const {
#ifndef NOSAFECHECKS
	if ((x >= nx) || (y >= ny) || (z >= nz))  {
		std::ostringstream ss;
		ss << "Trying to access array(" << x << "," << y << "," << z << ")";
		ss << " while array is (" << nx << "," << ny << "," << nz << ")";
		throw IndexOutOfBoundsException(ss.str(), AT);
	}
#endif
	return vecData[x + y*nx + z*nxny];
}

template<class T> Array3DProxy<T> Array3D<T>::operator[](const size_t& i) {
	return Array3DProxy<T>(*this, i);
}

template<class T> Array3D<T>::Array3D() : vecData(), nx(0), ny(0), nz(0), nxny(0), keep_nodata(true)
{
}

template<class T> Array3D<T>::Array3D(const Array3D<T>& i_array3D,
                                      const size_t& i_nx, const size_t& i_ny, const size_t& i_nz,
                                      const size_t& i_ncols, const size_t& i_nrows, const size_t& i_ndepth)
                               : vecData(i_ncols*i_nrows*i_ndepth), nx(i_ncols), ny(i_nrows), nz(i_ndepth), nxny(i_ncols*i_nrows), keep_nodata(true)
{
	subset(i_array3D, i_nx, i_ny, i_nz, i_ncols, i_nrows, i_ndepth);
}

template<class T> void Array3D<T>::subset(const Array3D<T>& i_array3D,
                                     const size_t& i_nx, const size_t& i_ny, const size_t& i_nz,
                                     const size_t& i_ncols, const size_t& i_nrows, const size_t& i_ndepth)
{

	if (((i_nx+i_ncols) > i_array3D.nx) || ((i_ny+i_nrows) > i_array3D.ny) || ((i_nz+i_ndepth) > i_array3D.nz)) {
		std::ostringstream ss;
		ss << "Trying to cut an array of size (" << i_array3D.nx << "," << i_array3D.ny << "," << i_array3D.nz << ") ";
		ss << "to size (" << i_ncols << "," << i_nrows << "," << i_ndepth << ") ";
		ss << "starting at (" << i_nx << "," << i_ny << "," << i_nz << ")";
		throw IndexOutOfBoundsException(ss.str(), AT);
	}

	if ((i_ncols == 0) || (i_nrows == 0) || (i_ndepth == 0)) //the space has to make sense
		throw IndexOutOfBoundsException("Trying to cut an array into a null sized array!", AT);

	resize(i_ncols, i_nrows, i_ndepth); //create new Array3D object
	//Copy by value subspace
	for (size_t ii=0; ii<nz; ii++) {
		for (size_t jj=0; jj<ny; jj++) {
			for (size_t kk=0; kk<nx; kk++) {
				//Running through the vector in order of memory alignment
				operator()(kk,jj,ii) = i_array3D(i_nx+kk, i_ny+jj, i_nz+ii);
			}
		}
	}
}

template<class T> void Array3D<T>::fill(const Array3D<T>& i_array3D, const size_t& i_nx, const size_t& i_ny, const size_t& i_nz)
{
	size_t i_ncols, i_nrows, i_ndepth;
	i_array3D.size(i_ncols, i_nrows, i_ndepth);
	fill(i_array3D, i_nx, i_ny, i_nz, i_ncols, i_nrows, i_ndepth);
}

template<class T> void Array3D<T>::fill(const Array3D<T>& i_array3D,
                                     const size_t& i_nx, const size_t& i_ny, const size_t& i_nz,
                                     const size_t& i_ncols, const size_t& i_nrows, const size_t& i_ndepth)
{

	if (((i_nx+i_ncols) > i_array3D.nx) || ((i_ny+i_nrows) > i_array3D.ny) || ((i_nz+i_ndepth) > i_array3D.nz)) {
		std::ostringstream ss;
		ss << "Filling an array of size (" << nx << "," << ny << "," << nz << ") ";
		ss << "with an array of size (" << i_ncols << "," << i_nrows << "," << i_ndepth << ") ";
		ss << "starting at (" << i_nx << "," << i_ny << "," << i_nz << ")";
		throw IndexOutOfBoundsException(ss.str(), AT);
	}

	if ((i_ncols == 0) || (i_nrows == 0) || (i_ndepth == 0)) //the space has to make sense
		throw IndexOutOfBoundsException("Filling an array with a null sized array!", AT);

	//Copy by value subspace
	for (size_t ii=i_nz; ii<(i_nz+i_ndepth); ii++) {
		const size_t iz = ii-i_nz;
		for (size_t jj=i_ny; jj<(i_ny+i_nrows); jj++) {
			const size_t iy = jj-i_ny;
			for (size_t kk=i_nx; kk<(i_nx+i_ncols); kk++) {
				const size_t ix = kk-i_nx;
				operator()(kk,jj,ii) = i_array3D(ix, iy, iz);
			}
		}
	}
}

template<class T> void Array3D<T>::insert(const Array2D<T>& layer, const size_t& depth)
{
	if (depth>nz) {
		std::ostringstream ss;
		ss << "Trying to insert layer at depth " << depth << " ";
		ss << "in an array of size (" << nx << "," << ny << "," << nz << ") ";
		throw IndexOutOfBoundsException(ss.str(), AT);
	}
	if (layer.getNx()!=nx || layer.getNy()!=ny) {
		std::ostringstream ss;
		ss << "Trying to insert layer of size (" << layer.getNx() << "," << layer.getNy() << ") ";
		ss << "in an array of size (" << nx << "," << ny << "," << nz << ") ";
		throw IndexOutOfBoundsException(ss.str(), AT);
	}
	//copy plane in the correct position
	for (size_t jj=0; jj<ny; jj++) {
		for (size_t ii=0; ii<nx; ii++) {
			operator()(ii,jj,depth) = layer(ii, jj);
		}
	}
}

template<class T> Array3D<T>::Array3D(const size_t& anx, const size_t& any, const size_t& anz)
                             : vecData(anx*any*anz), nx(anx), ny(any), nz(anz), nxny(anx*any), keep_nodata(true)
{
	//resize(anx, any, anz);
}

template<class T> Array3D<T>::Array3D(const size_t& anx, const size_t& any, const size_t& anz, const T& init)
                             : vecData(anx*any*anz, init), nx(anx), ny(any), nz(anz), nxny(anx*any), keep_nodata(true)
{
	//resize(anx, any, anz, init);
}

template<class T> void Array3D<T>::setKeepNodata(const bool i_keep_nodata) {
	keep_nodata = i_keep_nodata;
}

template<class T> bool Array3D<T>::getKeepNodata() const {
	return keep_nodata;
}

template<class T> void Array3D<T>::resize(const size_t& anx, const size_t& any, const size_t& anz) {
	clear();  //we won't be able to "rescue" old values, so we reset the whole vector
	vecData.resize(anx*any*anz);
	nx = anx;
	ny = any;
	nz = anz;
	nxny = nx*ny;
}

template<class T> void Array3D<T>::resize(const size_t& anx, const size_t& any, const size_t& anz, const T& init) {
	clear();  //we won't be able to "rescue" old values, so we reset the whole vector
	vecData.resize(anx*any*anz, init);
	nx = anx;
	ny = any;
	nz = anz;
	nxny = nx*ny;
}

template<class T> void Array3D<T>::size(size_t& anx, size_t& any, size_t& anz) const {
	anx=nx;
	any=ny;
	anz=nz;
}

template<class T> size_t Array3D<T>::size() const {
	return nx*ny*nz;
}

template<class T> size_t Array3D<T>::getNx() const {
	return nx;
}

template<class T> size_t Array3D<T>::getNy() const {
	return ny;
}

template<class T> size_t Array3D<T>::getNz() const {
	return nz;
}

template<class T> void Array3D<T>::clear() {
	vecData.clear();
	nx = ny = nz = nxny = 0;
}

template<class T> bool Array3D<T>::empty() const {
	return (nx==0 && ny==0 && nz==0);
}

template<class T> const std::string Array3D<T>::toString() const {
	std::ostringstream os;
	os << "<array3d>\n";
	for (size_t kk=0; kk<nz; kk++) {
		os << "depth[" << kk << "]\n";
		for (size_t jj=0; jj<ny; jj++) {
			for (size_t ii=0; ii<nx; ii++) {
				os << operator()(ii,jj,kk) << " ";
			}
			os << "\n";
		}
	}
	os << "</array3d>\n";
	return os.str();
}

template<class P> std::ostream& operator<<(std::ostream& os, const Array3D<P>& array) {
	os.write(reinterpret_cast<const char*>(&array.keep_nodata), sizeof(array.keep_nodata));
	os.write(reinterpret_cast<const char*>(&array.nx), sizeof(array.nx));
	os.write(reinterpret_cast<const char*>(&array.ny), sizeof(array.ny));
	os.write(reinterpret_cast<const char*>(&array.nz), sizeof(array.nz));
	os.write(reinterpret_cast<const char*>(&array.vecData[0]), static_cast<std::streamsize>(array.nx*array.ny*array.nz*sizeof(P)));
	return os;
}

template<class P> std::istream& operator>>(std::istream& is, Array3D<P>& array) {
	is.read(reinterpret_cast<char*>(&array.keep_nodata), sizeof(array.keep_nodata));
	is.read(reinterpret_cast<char*>(&array.nx), sizeof(array.nx));
	is.read(reinterpret_cast<char*>(&array.ny), sizeof(array.ny));
	is.read(reinterpret_cast<char*>(&array.nz), sizeof(array.nz));
	array.vecData.resize(array.nx*array.ny*array.nz);
	is.read(reinterpret_cast<char*>(&array.vecData[0]), static_cast<std::streamsize>(array.nx*array.ny*array.nz*sizeof(P))); //30 times faster than assign() or copy()
	return is;
}

template<class T> T Array3D<T>::getMin() const {

	T min = std::numeric_limits<T>::max();
	const size_t nxyz = ny*nx*nz;

	if (keep_nodata==false) {
		min = *min_element(vecData.begin(), vecData.end());
		if (min!=std::numeric_limits<T>::max()) return min;
		else return (T)IOUtils::nodata;
	} else {
		for (size_t jj=0; jj<nxyz; jj++) {
			const T val = vecData[jj];
			if (val!=IOUtils::nodata && val<min) min=val;
		}
		if (min!=std::numeric_limits<T>::max()) return min;
		else return (T)IOUtils::nodata;
	}
}

template<class T> T Array3D<T>::getMax() const {

	T max = -std::numeric_limits<T>::max();
	const size_t nxyz = ny*nx*nz;

	if (keep_nodata==false) {
		max = *max_element(vecData.begin(), vecData.end());
		if (max!=-std::numeric_limits<T>::max()) return max;
		else return (T)IOUtils::nodata;
	} else {
		for (size_t jj=0; jj<nxyz; jj++) {
			const T val = vecData[jj];
			if (val!=IOUtils::nodata && val>max) max=val;
		}
		if (max!=-std::numeric_limits<T>::max()) return max;
		else return (T)IOUtils::nodata;
	}
}

template<class T> T Array3D<T>::getMean() const {

	T mean = 0;
	const size_t nxyz = nx*ny*nz;

	if (keep_nodata==false) {
		if (nxyz>0) return std::accumulate(vecData.begin(), vecData.end(), 0.) / (T)(nxyz);
		else return (T)IOUtils::nodata;
	} else {
		size_t count = 0;
		for (size_t jj=0; jj<nxyz; jj++) {
			const T val = vecData[jj];
			if (val!=IOUtils::nodata) {
				mean += val;
				count++;
			}
		}
		if (count>0) return mean/(T)(count);
		else return (T)IOUtils::nodata;
	}
}

template<class T> size_t Array3D<T>::getCount() const
{
	const size_t nxyz = nx*ny*nz;

	if (keep_nodata==false) {
		return (size_t)nxyz;
	} else {
		size_t count = 0;
		for (size_t ii=0; ii<nxyz; ii++) {
			if (vecData[ii]!=IOUtils::nodata) count++;
		}
		return count;
	}
}

template<class T> void Array3D<T>::abs() {
	if (std::numeric_limits<T>::is_signed) {
		const size_t nxyz = nx*ny*nz;
		if (keep_nodata==false) {
			for (size_t jj=0; jj<nxyz; jj++) {
				T& val = vecData[jj];
				if (val<0) val=-val;
			}
		} else {
			for (size_t jj=0; jj<nxyz; jj++) {
				T& val = vecData[jj];
				if (val<0 && val!=IOUtils::nodata) val=-val;
			}
		}
	}
}

template<class T> const Array3D<T> Array3D<T>::getAbs() const {
	Array3D<T> result(*this); //make a copy
	result.abs(); //already implemented

	return result;
}

//arithmetic operators
template<class T> bool Array3D<T>::checkEpsilonEquality(const Array3D<double>& rhs, const double& epsilon) const {
	if (nx!=rhs.nx || ny!=rhs.ny || nz!=rhs.nz) return false;

	const size_t nxyz = nx*ny*nz;
	for (size_t jj=0; jj<nxyz; jj++)
		if (IOUtils::checkEpsilonEquality(vecData[jj], rhs.vecData[jj], epsilon)==false) return false;

	return true;
}

template<class T> bool Array3D<T>::checkEpsilonEquality(const Array3D<double>& rhs1, const Array3D<double>& rhs2, const double& epsilon) {
	return rhs1.checkEpsilonEquality(rhs2, epsilon);
}

template<class T> Array3D<T>& Array3D<T>::operator=(const Array3D<T>& source) {
	if (this != &source) {
		keep_nodata = source.keep_nodata;
		nx = source.nx;
		ny = source.ny;
		nz = source.nz;
		nxny = source.nxny;
		vecData = source.vecData;
	}
	return *this;
}

template<class T> Array3D<T>& Array3D<T>::operator=(const T& value) {
	std::fill(vecData.begin(), vecData.end(), value);
	return *this;
}

template<class T> Array3D<T>& Array3D<T>::operator+=(const Array3D<T>& rhs)
{
	//They have to have equal size
	if ((rhs.nx != nx) || (rhs.ny != ny) || (rhs.nz != nz)) {
		std::ostringstream ss;
		ss << "Trying to add two Array3D objects with different dimensions: ";
		ss << "(" << nx << "," << ny << "," << nz << ") + (" << rhs.nx << "," << rhs.ny << "," << rhs.nz << ")";
		throw IOException(ss.str(), AT);
	}
	//Add to every single member of the Array3D<T>
	const size_t nxyz = nx*ny*nz;
	if (keep_nodata==false) {
		for (size_t jj=0; jj<nxyz; jj++) {
			vecData[jj] += rhs(jj);
		}
	} else {
		for (size_t jj=0; jj<nxyz; jj++) {
			if (vecData[jj]==IOUtils::nodata || rhs(jj)==IOUtils::nodata)
				vecData[jj] = IOUtils::nodata;
			else
				vecData[jj] += rhs(jj);
		}
	}

	return *this;
}

template<class T> const Array3D<T> Array3D<T>::operator+(const Array3D<T>& rhs) const
{
	Array3D<T> result(*this); //make a copy
	result += rhs; //already implemented

	return result;
}

template<class T> Array3D<T>& Array3D<T>::operator+=(const T& rhs)
{
	if (rhs==0.) return *this;
	
	//Add to every single member of the Array3D<T>
	const size_t nxyz = nx*ny*nz;
	if (keep_nodata==false) {
		for (size_t jj=0; jj<nxyz; jj++) {
			vecData[jj] += rhs;
		}
	} else {
		for (size_t jj=0; jj<nxyz; jj++) {
			if (vecData[jj]!=IOUtils::nodata)
				vecData[jj] += rhs;
		}
	}

	return *this;
}

template<class T> const Array3D<T> Array3D<T>::operator+(const T& rhs) const
{
	Array3D<T> result(*this);
	result += rhs; //already implemented

	return result;
}

template<class T> Array3D<T>& Array3D<T>::operator-=(const Array3D<T>& rhs)
{
	//They have to have equal size
	if ((rhs.nx != nx) || (rhs.ny != ny) || (rhs.nz != nz)) {
		std::ostringstream ss;
		ss << "Trying to substract two Array3D objects with different dimensions: ";
		ss << "(" << nx << "," << ny << "," << nz << ") - (" << rhs.nx << "," << rhs.ny << "," << rhs.nz << ")";
		throw IOException(ss.str(), AT);
	}
	//Substract to every single member of the Array3D<T>
	const size_t nxyz = nx*ny*nz;
	if (keep_nodata==false) {
		for (size_t jj=0; jj<nxyz; jj++) {
			vecData[jj] -= rhs(jj);
		}
	} else {
		for (size_t jj=0; jj<nxyz; jj++) {
			if (vecData[jj]==IOUtils::nodata || rhs(jj)==IOUtils::nodata)
				vecData[jj] = IOUtils::nodata;
			else
				vecData[jj] -= rhs(jj);
		}
	}

	return *this;
}

template<class T> const Array3D<T> Array3D<T>::operator-(const Array3D<T>& rhs) const
{
	Array3D<T> result(*this); //make a copy
	result -= rhs; //already implemented

	return result;
}

template<class T> Array3D<T>& Array3D<T>::operator-=(const T& rhs)
{
	*this += -rhs;
	return *this;
}

template<class T> const Array3D<T> Array3D<T>::operator-(const T& rhs) const
{
	Array3D<T> result(*this);
	result += -rhs; //already implemented

	return result;
}

template<class T> Array3D<T>& Array3D<T>::operator*=(const Array3D<T>& rhs)
{
	//They have to have equal size
	if ((rhs.nx != nx) || (rhs.ny != ny) || (rhs.nz != nz)) {
		std::ostringstream ss;
		ss << "Trying to multiply two Array3D objects with different dimensions: ";
		ss << "(" << nx << "," << ny << "," << nz << ") * (" << rhs.nx << "," << rhs.ny << "," << rhs.nz << ")";
		throw IOException(ss.str(), AT);
	}
	//Multiply every single member of the Array3D<T>
	const size_t nxyz = nx*ny*nz;
	if (keep_nodata==false) {
		for (size_t jj=0; jj<nxyz; jj++) {
			vecData[jj] *= rhs(jj);
		}
	} else {
		for (size_t jj=0; jj<nxyz; jj++) {
			if (vecData[jj]==IOUtils::nodata || rhs(jj)==IOUtils::nodata)
				vecData[jj] = IOUtils::nodata;
			else
				vecData[jj] *= rhs(jj);
		}
	}

	return *this;
}

template<class T> const Array3D<T> Array3D<T>::operator*(const Array3D<T>& rhs) const
{
	Array3D<T> result(*this); //make a copy
	result *= rhs; //already implemented

	return result;
}

template<class T> Array3D<T>& Array3D<T>::operator*=(const T& rhs)
{
	if (rhs==1.) return *this;
	
	//Multiply every single member of the Array3D<T>
	const size_t nxyz = nx*ny*nz;
	if (keep_nodata==false) {
		for (size_t jj=0; jj<nxyz; jj++) {
			vecData[jj] *= rhs;
		}
	} else {
		for (size_t jj=0; jj<nxyz; jj++) {
			if (vecData[jj]!=IOUtils::nodata)
				vecData[jj] *= rhs;
		}
	}

	return *this;
}

template<class T> const Array3D<T> Array3D<T>::operator*(const T& rhs) const
{
	Array3D<T> result(*this);
	result *= rhs; //already implemented

	return result;
}

template<class T> Array3D<T>& Array3D<T>::operator/=(const Array3D<T>& rhs)
{
	//They have to have equal size
	if ((rhs.nx != nx) || (rhs.ny != ny) || (rhs.nz != nz)) {
		std::ostringstream ss;
		ss << "Trying to divide two Array3D objects with different dimensions: ";
		ss << "(" << nx << "," << ny << "," << nz << ") / (" << rhs.nx << "," << rhs.ny << "," << rhs.nz << ")";
		throw IOException(ss.str(), AT);
	}
	//Divide every single member of the Array3D<T>
	const size_t nxyz = nx*ny*nz;
	if (keep_nodata==false) {
		for (size_t jj=0; jj<nxyz; jj++) {
			vecData[jj] /= rhs(jj);
		}
	} else {
		for (size_t jj=0; jj<nxyz; jj++) {
			if (vecData[jj]==IOUtils::nodata || rhs(jj)==IOUtils::nodata)
				vecData[jj] = IOUtils::nodata;
			else
				vecData[jj] /= rhs(jj);
		}
	}

	return *this;
}

template<class T> const Array3D<T> Array3D<T>::operator/(const Array3D<T>& rhs) const
{
	Array3D<T> result(*this); //make a copy
	result /= rhs; //already implemented

	return result;
}

template<class T> Array3D<T>& Array3D<T>::operator/=(const T& rhs)
{
	*this *= (1./rhs);
	return *this;
}

template<class T> const Array3D<T> Array3D<T>::operator/(const T& rhs) const
{
	Array3D<T> result(*this);
	result *= (1./rhs); //already implemented

	return result;
}

template<class T> bool Array3D<T>::operator==(const Array3D<T>& in) const {
	const size_t in_nx=in.getNx(), in_ny=in.getNy(), in_nz=in.getNz();

	if (nx!=in_nx || ny!=in_ny || nz!=in_nz)
		return false;

	const size_t nxyz = nx*ny*nz;
	for (size_t jj=0; jj<nxyz; jj++)
		if ( !IOUtils::checkEpsilonEquality( vecData[jj] , in.vecData[jj], 1e-6) ) return false;

	return true;
}

template<class T> bool Array3D<T>::operator!=(const Array3D<T>& in) const {
	return !(*this==in);
}

} //end namespace mio

#endif
