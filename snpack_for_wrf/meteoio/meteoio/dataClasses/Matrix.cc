/***********************************************************************************/
/*  Copyright 2011 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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

#include <meteoio/dataClasses/Matrix.h>
#include <meteoio/IOUtils.h>
#include <meteoio/IOExceptions.h>

#include <time.h> //needed for random()
#include <cmath> //needed for fabs()
#include <iostream>
#include <iomanip>

namespace mio {

/*
 * Sketch of the matrix class with proposed indexing:
 *
 *     M(row, col)                            ______________________
 *   ~ M(i, j)        <-- ncols -->          |(1,1)|(1,2)|(1,3)|...
 *   ~ M(y, x)           getNx()             |-----|-----|-----|
 *                      _________            |(2,1)|(2,2)|...
 *                  ^  |_|_|_|_|_| i,        |-----|-----|              _________
 *           nrows  |  |_|_|_|_|_| y         |(3,1)|...                |0|1|2|3|4|
 *          getNy() |  |_|_|_|_|_| |                                   |5|6|.|.|.|
 *                  |  |_|_|_|_|_| v                                   |_|_|_|_|_|
 *                  v  |_|_|_|_|_|      vecData uses a rolling index:  |_|_|_|_|_|
 *                     j, x -->             M(i, j)=(i-1)*ncols + j-1  |_|21_|_|_|
 */

const double Matrix::epsilon = 1e-9; //for considering a determinant to be zero, etc
const double Matrix::epsilon_mtr = 1e-6; //for comparing two matrices

Matrix::Matrix(const int& rows, const int& cols) : vecData(), ncols(0), nrows(0)
{
	if (rows<0 || cols<0) {
		std::ostringstream tmp;
		tmp << "Trying to construct a matrix with negative dimensions: ";
		tmp << "(" << rows << "," << cols << ")";
		throw IOException(tmp.str(), AT);
	}
	resize((unsigned)rows,(unsigned)cols);
}

Matrix::Matrix(const size_t& n, const double& init) : vecData(n*n, 0.), ncols(n), nrows(n)
{
	for (size_t ii=1; ii<=n; ii++) operator()(ii,ii) = init;
}

void Matrix::identity(const size_t& n, const double& init)
{
	resize(n,n,0.);
	for (size_t ii=1; ii<=n; ii++) operator()(ii,ii) = init;
}

void Matrix::resize(const size_t& rows, const size_t& cols)
{
	clear();
	vecData.resize(rows*cols);
	nrows = rows;
	ncols = cols;
}

void Matrix::resize(const size_t& rows, const size_t& cols, const double& init)
{
	clear();
	vecData.resize(rows*cols, init);
	ncols = cols;
	nrows = rows;
}

void Matrix::resize(const size_t& rows, const size_t& cols, const std::vector<double>& data)
{
#ifndef NOSAFECHECKS
	if (rows*cols!=data.size()) {
		std::ostringstream ss;
		ss << "Trying to init matrix data that does not fit size " << rows << "x" << cols;
		throw IndexOutOfBoundsException(ss.str(), AT);
	}
#endif
	clear();
	vecData = data;
	ncols = cols;
	nrows = rows;
}

void Matrix::size(size_t& rows, size_t& cols) const
{
	rows=nrows;
	cols=ncols;
}

void Matrix::clear()
{
	vecData.clear();
	nrows=ncols=0;
}

void Matrix::random(const double& range)
{
	srand( static_cast<unsigned>(time(NULL)) );

	for (size_t ii=0; ii<vecData.size(); ii++)
		vecData[ii] = (double)rand()/(double)RAND_MAX*range;
}

double& Matrix::operator ()(const size_t& ii, const size_t& jj)
{
#ifndef NOSAFECHECKS
	if ((ii<1) || (ii > nrows) || (jj<1) || (jj > ncols)) {
		std::ostringstream ss;
		ss << "Trying to access matrix[" << ii << "," << jj << "]";
		throw IndexOutOfBoundsException(ss.str(), AT);
	}
#endif
	return vecData[(jj-1) + (ii-1)*ncols];
}

double Matrix::operator ()(const size_t& ii, const size_t& jj) const
{
#ifndef NOSAFECHECKS
	if ((ii<1) || (ii > nrows) || (jj<1) || (jj > ncols)) {
		std::ostringstream ss;
		ss << "Trying to access matrix[" << ii << "," << jj << "]";
		throw IndexOutOfBoundsException(ss.str(), AT);
	}
#endif
	return vecData[(jj-1) + (ii-1)*ncols];
}

const std::string Matrix::toString(const int& precision, const bool& prettify) const
{
	std::ostringstream os;
	const int wd=precision+4;
	if (prettify) {
		os << "\n┌ ";
		for (size_t jj=1; jj<=(ncols*(wd+1)); jj++)
			os << " ";
		os << " ┐\n";
		for (size_t ii=1; ii<=nrows; ii++) {
			os << "│ ";
			for (size_t jj=1; jj<=ncols; jj++) {
				os << std::setw(wd) << std::fixed << std::setprecision(precision) << operator()(ii,jj) << " ";
			}
			os << " │\n";
		}
		os << "└ ";
		for (size_t jj=1; jj<=(ncols*(wd+1)); jj++)
			os << " ";
		os << " ┘\n";
	} else {
		for (size_t ii=1; ii<=nrows; ii++) {
			for (size_t jj=1; jj<=ncols; jj++)
				os << std::setw(wd) << std::fixed << std::setprecision(precision) << operator()(ii,jj) << " ";
		os << "\n";
		}
	}
	return os.str();
}

bool Matrix::operator==(const Matrix& in) const
{
	size_t in_nrows, in_ncols;
	in.size(in_nrows, in_ncols);

	if (nrows!=in_nrows || ncols!=in_ncols)
		return false;

	for (size_t ii=0; ii<vecData.size(); ii++)
		if ( !IOUtils::checkEpsilonEquality( vecData[ii] , in.vecData[ii], epsilon_mtr) ) return false;

	return true;
}

bool Matrix::operator!=(const Matrix& in) const
{
	return !(*this==in);
}

Matrix& Matrix::operator+=(const Matrix& rhs)
{
	//check dimensions compatibility
	if (nrows!=rhs.nrows || ncols!=rhs.ncols) {
		std::ostringstream tmp;
		tmp << "Trying to add two matrices with incompatible dimensions: ";
		tmp << "(" << nrows << "," << ncols << ") * ";
		tmp << "(" << rhs.nrows << "," << rhs.ncols << ")";
		throw IOException(tmp.str(), AT);
	}

	//fill sum matrix
	for (size_t ii=0; ii<vecData.size(); ii++)
		vecData[ii] += rhs.vecData[ii];

	return *this;
}

const Matrix Matrix::operator+(const Matrix& rhs) const
{
	Matrix result = *this;
	result += rhs; //already implemented

	return result;
}

Matrix& Matrix::operator+=(const double& rhs)
{
	//fill sum matrix
	for (size_t ii=0; ii<vecData.size(); ii++)
		vecData[ii] += rhs;

	return *this;
}

const Matrix Matrix::operator+(const double& rhs) const
{
	Matrix result = *this;
	result += rhs; //already implemented

	return result;
}

Matrix& Matrix::operator-=(const Matrix& rhs)
{
	//check dimensions compatibility
	if (nrows!=rhs.nrows || ncols!=rhs.ncols) {
		std::ostringstream tmp;
		tmp << "Trying to substract two matrices with incompatible dimensions: ";
		tmp << "(" << nrows << "," << ncols << ") * ";
		tmp << "(" << rhs.nrows << "," << rhs.ncols << ")";
		throw IOException(tmp.str(), AT);
	}

	//fill sum matrix
	for (size_t ii=0; ii<vecData.size(); ii++)
		vecData[ii] -= rhs.vecData[ii];

	return *this;
}

const Matrix Matrix::operator-(const Matrix& rhs) const
{
	Matrix result = *this;
	result -= rhs; //already implemented

	return result;
}

Matrix& Matrix::operator-=(const double& rhs)
{
	*this += -rhs;

	return *this;
}

const Matrix Matrix::operator-(const double& rhs) const
{
	Matrix result = *this;
	result += -rhs; //already implemented

	return result;
}

Matrix& Matrix::operator*=(const Matrix& rhs)
{
	//check dimensions compatibility
	if (ncols!=rhs.nrows) {
		std::ostringstream tmp;
		tmp << "Trying to multiply two matrices with incompatible dimensions: ";
		tmp << "(" << nrows << "," << ncols << ") * ";
		tmp << "(" << rhs.nrows << "," << rhs.ncols << ")";
		throw IOException(tmp.str(), AT);
	}

	//create new matrix
	Matrix result(nrows, rhs.ncols);

	//fill product matrix
	for (size_t ii=1; ii<=result.nrows; ii++) {
		for (size_t jj=1; jj<=result.ncols; jj++) {
			double sum=0.;
			for (size_t idx=1; idx<=ncols; idx++) {
				sum += operator()(ii,idx) * rhs(idx,jj);
			}
			result(ii,jj) = sum;
		}
	}

	*this = result;
	return *this;
}

const Matrix Matrix::operator*(const Matrix& rhs) const
{
	Matrix result = *this;
	result *= rhs; //already implemented

	return result;
}

Matrix& Matrix::operator*=(const double& rhs)
{
	for (size_t ii=0; ii<vecData.size(); ii++)
		vecData[ii] *= rhs;

	return *this;
}

const Matrix Matrix::operator*(const double& rhs) const
{
	Matrix result = *this;
	result *= rhs; //already implemented

	return result;
}

Matrix& Matrix::operator/=(const double& rhs)
{
	*this *= (1./rhs);
	return *this;
}

const Matrix Matrix::operator/(const double& rhs) const
{
	Matrix result = *this;
	result *= 1./rhs; //already implemented

	return result;
}

double Matrix::scalar(const Matrix& m)
{
	return m.scalar();
}

double Matrix::scalar() const
{
	if (ncols!=1 || nrows!=1) {
		std::ostringstream tmp;
		tmp << "Trying to get scalar value of a non (1x1) matrix ";
		tmp << "(" << nrows << "," << ncols << ") !";
		throw IOException(tmp.str(), AT);
	}
	return operator()(1,1);
}

double Matrix::dot(const Matrix& A, const Matrix& B)
{
	size_t Acols, Arows, Bcols, Brows;
	A.size(Arows, Acols);
	B.size(Brows, Bcols);

	if (Acols!=1 || Bcols!=1) {
		std::ostringstream tmp;
		tmp << "Trying to get dot product of non vector matrix ";
		tmp << "(" << Arows << "," << Acols << ") · ";
		tmp << "(" << Brows << "," << Bcols << ") · ";
		throw IOException(tmp.str(), AT);
	}
	if (Arows!=Brows) {
		std::ostringstream tmp;
		tmp << "Trying to get dot product of incompatible matrix ";
		tmp << "(" << Arows << "," << Acols << ") · ";
		tmp << "(" << Brows << "," << Bcols << ") · ";
		throw IOException(tmp.str(), AT);
	}

	double sum=0.;
	for (size_t ii=1; ii<=Arows; ii++) {
		sum += A(ii,1)*B(ii,1);
	}

	return sum;
}

Matrix Matrix::T(const Matrix& m)
{
	return m.getT();
}

Matrix Matrix::getT() const
{
	Matrix result(ncols, nrows);
	for (size_t ii=1; ii<=result.nrows; ii++) {
		for (size_t jj=1; jj<=result.ncols; jj++) {
			result(ii,jj) = operator()(jj,ii);
		}
	}
	return result;
}

void Matrix::T()
{
	Matrix tmp(*this);
	*this = tmp.getT();
}

double Matrix::det() const
{
	if (nrows!=ncols) {
		std::ostringstream tmp;
		tmp << "Trying to calculate the determinant of a non-square matrix ";
		tmp << "(" << nrows << "," << ncols << ") !";
		throw IOException(tmp.str(), AT);
	}
	Matrix L,U;
	if (LU(L,U)==false) return 0.;

	double product=1.;
	for (size_t ii=1; ii<=nrows; ii++) product *= U(ii,ii);

	return product;
}

bool Matrix::LU(Matrix& L, Matrix& U) const
{
//Dolittle algorithm, cf http://mathfaculty.fullerton.edu/mathews/n2003/CholeskyMod.html
//HACK: there is no permutation matrix, so it might not be able to give a decomposition...
	if (nrows!=ncols) {
		std::ostringstream tmp;
		tmp << "Trying to calculate the LU decomposition of a non-square matrix ";
		tmp << "(" << nrows << "," << ncols << ") !";
		throw IOException(tmp.str(), AT);
	}

	const size_t n = nrows;
	U = *this;
	L.identity(n, 1.); //initialized as identity matrix, then populated
	const Matrix& A = *this;

	for (size_t kk=1; kk<=n; kk++) {
		//compute U elements
		for (size_t jj=1; jj<kk; jj++) {
			U(kk,jj) = 0.;
		}
		for (size_t jj=kk; jj<=n; jj++) {
			double sum=0.;
			for (size_t mm=1; mm<=(kk-1); mm++) sum += L(kk,mm)*U(mm,jj);
			U(kk,jj) = A(kk,jj) - sum;
		}

		if ( kk<n && IOUtils::checkEpsilonEquality(U(kk,kk), 0., epsilon) ) return false; //we can not compute L
		//compute L elements
		for (size_t ii=kk+1; ii<=n; ii++) {
			double sum=0.;
			for (size_t mm=1; mm<=(kk-1); mm++) sum += L(ii,mm)*U(mm,kk);
			L(ii,kk) = (A(ii,kk) - sum) / U(kk,kk);
		}
	}
	return true;
}

Matrix Matrix::getInv() const
{
//This uses an LU decomposition followed by backward and forward solving for the inverse
//See for example Press, William H.; Flannery, Brian P.; Teukolsky, Saul A.; Vetterling, William T. (1992), "LU Decomposition and Its Applications", Numerical Recipes in FORTRAN: The Art of Scientific Computing (2nd ed.), Cambridge University Press, pp. 34–42
	if (nrows!=ncols) {
		std::ostringstream tmp;
		tmp << "Trying to invert a non-square matrix ";
		tmp << "(" << nrows << "," << ncols << ") !";
		throw IOException(tmp.str(), AT);
	}
	const size_t n = nrows;

	Matrix U;
	Matrix L;
	if (LU(L, U)==false) {
		throw IOException("LU decomposition of given matrix not possible", AT);
	}

	//we solve AX=I with X=A-1. Since A=LU, then LUX = I
	//we start by forward solving LY=I with Y=UX
	Matrix Y(n, n);
	for (size_t ii=1; ii<=n; ii++) {
		if (IOUtils::checkEpsilonEquality(L(ii,ii), 0., epsilon)) {
			throw IOException("The given matrix can not be inverted", AT);
		}
		Y(ii,ii) = 1./L(ii,ii); //jj==ii
		for (size_t jj=1; jj<ii; jj++) { //jj<ii
			double sum=0.;
			for (size_t kk=ii-1; kk>=1; kk--) { //equivalent to 1 -> ii-1
				sum += L(ii,kk) * Y(kk,jj);
			}
			Y(ii,jj) = -1./L(ii,ii) * sum;
		}
		for (size_t jj=ii+1; jj<=n; jj++) { //jj>i
			Y(ii,jj) = 0.;
		}
	}

	//now, we backward solve UX=Y
	Matrix X(n,n);
	for (size_t ii=n; ii>=1; ii--) { //lines
		if (IOUtils::checkEpsilonEquality(U(ii,ii), 0., epsilon)) { //HACK: actually, only U(n,n) needs checking
			throw IOException("The given matrix is singular and can not be inverted", AT);
		}
		for (size_t jj=1; jj<=n; jj++) { //lines
			double sum=0.;
			for (size_t kk=ii+1; kk<=n; kk++) {
				sum += U(ii,kk) * X(kk,jj);
			}
			X(ii,jj) = (Y(ii,jj) - sum) / U(ii,ii);
		}
	}

	return X;
}

bool Matrix::inv()
{
//same as getInv() const but we write the final result on top of the input matrix
	if (nrows!=ncols) {
		std::ostringstream tmp;
		tmp << "Trying to invert a non-square matrix ";
		tmp << "(" << nrows << "," << ncols << ") !";
		throw IOException(tmp.str(), AT);
	}
	const size_t n = nrows;

	Matrix U;
	Matrix L;
	if (LU(L, U)==false) {
		return false;
	}

	//we solve AX=I with X=A-1. Since A=LU, then LUX = I
	//we start by forward solving LY=I with Y=UX
	Matrix Y(n, n);
	for (size_t ii=1; ii<=n; ii++) {
		if (IOUtils::checkEpsilonEquality(L(ii,ii), 0., epsilon)) {
			return false;
		}
		Y(ii,ii) = 1./L(ii,ii); //jj==ii
		for (size_t jj=1; jj<ii; jj++) { //jj<ii
			double sum=0.;
			for (size_t kk=ii-1; kk>=1; kk--) { //equivalent to 1 -> ii-1
				sum += L(ii,kk) * Y(kk,jj);
			}
			Y(ii,jj) = -1./L(ii,ii) * sum;
		}
		for (size_t jj=ii+1; jj<=n; jj++) { //jj>ii
			Y(ii,jj) = 0.;
		}
	}

	//now, we backward solve UX=Y
	Matrix& X = *this; //we write the solution over the input matrix
	for (size_t ii=n; ii>=1; ii--) { //lines
		if (IOUtils::checkEpsilonEquality(U(ii,ii), 0., epsilon)) { //actually, only U(n,n) needs checking
			return false;
		}
		for (size_t jj=1; jj<=n; jj++) { //lines
			double sum=0.;
			for (size_t kk=ii+1; kk<=n; kk++) {
				sum += U(ii,kk) * X(kk,jj);
			}
			X(ii,jj) = (Y(ii,jj) - sum) / U(ii,ii);
		}
	}

	return true;
}

Matrix Matrix::getRow(const size_t ii) const
{
#ifndef NOSAFECHECKS
	if ((ii<1) || (ii > nrows) ) {
		std::ostringstream ss;
		ss << "Trying to access matrix row " << ii;
		throw IndexOutOfBoundsException(ss.str(), AT);
	}
#endif
	Matrix mRet((size_t)1, ncols);
	for (size_t jj=0; jj<ncols; jj++) {
		mRet(1, jj+1) = vecData[(ii-1)*nrows+jj];
	}
	return mRet;
}

void Matrix::setRow(const size_t ii, const Matrix& row)
{
#ifndef NOSAFECHECKS
	if ((ii<1) || (ii > nrows) ) {
		std::ostringstream ss;
		ss << "Trying to access matrix row " << ii;
		throw IndexOutOfBoundsException(ss.str(), AT);
	}
	if (row.nrows!=1) {
		std::ostringstream ss;
		ss << "Trying to set row " << ii << ", but the given row is not a row vector.";
		throw InvalidArgumentException(ss.str(), AT);
	}
#endif
	for (size_t jj=0; jj<ncols; jj++) {
		vecData[(ii-1)*nrows+jj] = row(1, jj+1);
	}
}

Matrix Matrix::getCol(const size_t jj) const
{
#ifndef NOSAFECHECKS
	if ((jj<1) || (jj > ncols)) {
		std::ostringstream ss;
		ss << "Trying to access matrix column " << jj;
		throw IndexOutOfBoundsException(ss.str(), AT);
	}
#endif
	Matrix mRet(nrows, (size_t)1);
	for (size_t ii=0; ii<nrows; ii++) {
		mRet(ii+1, 1) = vecData[(jj-1)+ii*ncols];
	}
	return mRet;
}

void Matrix::setCol(const size_t jj, const Matrix& col)
{
#ifndef NOSAFECHECKS
	if ((jj<1) || (jj > ncols) ) {
		std::ostringstream ss;
		ss << "Trying to access matrix column " << jj;
		throw IndexOutOfBoundsException(ss.str(), AT);
	}
	if (col.ncols!=1) {
		std::ostringstream ss;
		ss << "Trying to set column " << jj << ", but the given column is not a column vector.";
		throw InvalidArgumentException(ss.str(), AT);
	}
#endif
	for (size_t ii=0; ii<nrows; ii++) {
		vecData[(jj-1)*ncols+ii] = col(ii+1, 1);
	}
}

Matrix Matrix::getDiagonal() const
{
#ifndef NOSAFECHECKS
	if (nrows!=ncols) {
		std::ostringstream ss;
		ss << "Trying to take diagonal from a non-square matrix.";
		throw IndexOutOfBoundsException(ss.str(), AT);
	}
#endif
	Matrix mRet((size_t)1, ncols);
	for (size_t ii=0; ii<ncols; ii++) {
		mRet(1, ii+1)=vecData[ii+ii*ncols];
	}
	return mRet;
}

Matrix Matrix::extract(size_t r_low, size_t r_high, size_t c_low, size_t c_high) const
{ //extract submatrix to new matrix
	//npos means "from beginning" or "until end":
	r_low=(r_low==IOUtils::npos)? 1 : r_low;
	r_high=(r_high==IOUtils::npos)? nrows : r_high;
	c_low=(c_low==IOUtils::npos)? 1 : c_low;
	c_high=(c_high==IOUtils::npos)? ncols : c_high;

	//TODO: safechecks?
	Matrix mRet(r_high-r_low+1, c_high-c_low+1);
	for (size_t ii=1; ii<=mRet.getNy(); ii++) {
		for (size_t jj=1; jj<=mRet.getNx(); jj++)
			mRet(ii, jj)=operator()(ii+r_low-1, jj+c_low-1);
	}
	return mRet;
}

bool Matrix::solve(const Matrix& A, const Matrix& B, Matrix& X)
{
//This uses an LU decomposition followed by backward and forward solving for A·X=B
	size_t Anrows,Ancols, Bnrows, Bncols;
	A.size(Anrows, Ancols);
	if (Anrows!=Ancols) {
		std::ostringstream tmp;
		tmp << "Trying to solve A·X=B with A non square matrix ";
		tmp << "(" << Anrows << "," << Ancols << ") !";
		throw IOException(tmp.str(), AT);
	}
	B.size(Bnrows, Bncols);
	if (Anrows!=Bnrows)  {
		std::ostringstream tmp;
		tmp << "Trying to solve A·X=B with A and B of incompatible dimensions ";
		tmp << "(" << Anrows << "," << Ancols << ") and (";
		tmp << "(" << Bnrows << "," << Bncols << ") !";
		throw IOException(tmp.str(), AT);
	}
	const size_t nn = Anrows;
	const size_t mm = Bncols;

	Matrix U;
	Matrix L;
	if (A.LU(L, U)==false) {
		return false;
	}

	//we solve AX=B. Since A=LU, then LUX = B
	//we start by forward solving LY=B with Y=UX
	Matrix Y(nn, mm);
	for (size_t ii=1; ii<=nn; ii++) {
		if (IOUtils::checkEpsilonEquality(L(ii,ii), 0., epsilon)) {
			return false;
		}
		for (size_t jj=1; jj<=mm; jj++) {
			double sum=0.;
			for (size_t kk=1; kk<ii; kk++) {
				sum += L(ii,kk) * Y(kk,jj);
			}
			Y(ii,jj) = (B(ii,jj) - sum) / L(ii,ii);
		}
	}

	//now, we backward solve UX=Y
	X.resize(nn,mm); //we need to ensure that X has the correct dimensions
	for (size_t ii=nn; ii>=1; ii--) { //lines
		if (IOUtils::checkEpsilonEquality(U(ii,ii), 0., epsilon)) { //actually, only U(n,n) needs checking
			//singular matrix
			return false;
		}
		for (size_t jj=1; jj<=mm; jj++) {
			double sum = 0.;
			for (size_t kk=ii+1; kk<=nn; kk++) {
				sum += U(ii,kk) * X(kk,jj);
			}
			X(ii,jj) = (Y(ii,jj) - sum) / U(ii,ii);
		}
	}

	return true;
}

Matrix Matrix::solve(const Matrix& A, const Matrix& B)
{//This uses an LU decomposition followed by backward and forward solving for A·X=B
	Matrix X;
	if (!solve(A, B, X))
		throw IOException("Matrix inversion failed!", AT);
	return X;
}

bool Matrix::TDMA_solve(const Matrix& A, const Matrix& B, Matrix& X)
{ //Thomas algorithm for tridiagonal matrix solving of A·X=B
	size_t Anrows,Ancols, Bnrows, Bncols;
	A.size(Anrows, Ancols);
	if (Anrows!=Ancols) {
		std::ostringstream tmp;
		tmp << "Trying to solve A·X=B with A non square matrix ";
		tmp << "(" << Anrows << "," << Ancols << ") !";
		throw IOException(tmp.str(), AT);
	}
	B.size(Bnrows, Bncols);
	if (Anrows!=Bnrows)  {
		std::ostringstream tmp;
		tmp << "Trying to solve A·X=B with A and B of incompatible dimensions ";
		tmp << "(" << Anrows << "," << Ancols << ") and (";
		tmp << "(" << Bnrows << "," << Bncols << ") !";
		throw IOException(tmp.str(), AT);
	}
	if (Bncols!=1) {
		std::ostringstream tmp;
		tmp << "Trying to solve A·X=B but B is not a vector! It is ";
		tmp << "(" << Bnrows << "," << Bncols << ") !";
		throw IOException(tmp.str(), AT);
	}

	const size_t n = Anrows;
	std::vector<double> b(n+1), c(n+1), v(n+1); //so we can keep the same index as for the matrix

	b[1] = A(1,1); v[1] = B(1,1); //otherwise they would never be defined
	for (size_t ii=2; ii<=n; ii++) {
		if (IOUtils::checkEpsilonEquality(b[ii-1], 0., epsilon))
			return false;
		const double b_i = A(ii,ii);
		const double v_i = B(ii,1);
		const double a_i = A(ii,ii-1);
		const double m = a_i / b[ii-1];
		c[ii-1] = A(ii-1,ii);
		b[ii] = b_i - m * c[ii-1];
		v[ii] = v_i - m * v[ii-1];
	}

	X.resize(n,1); //we need to ensure that X has the correct dimensions
	X(n,1) = v[n] / b[n];
	for (size_t ii=n-1; ii>=1; ii--) {
		X(ii,1) = ( v[ii] - c[ii]*X(ii+1,1) ) / b[ii];
	}

	return true;
}

Matrix Matrix::TDMA_solve(const Matrix& A, const Matrix& B)
{
//This uses the Thomas algorithm for tridiagonal matrix solving of A·X=B
	Matrix X;
	if (TDMA_solve(A, B, X))
		return X;
	else
		throw IOException("Matrix inversion failed!", AT);
}

void Matrix::gaussElimination(Matrix& MM, std::vector<size_t>& pp)
{ //Gaussian elimination with partial pivoting (row-swapping)
	const size_t dim=MM.getNx();
	pp.resize(dim+1); //start at 1 like the matrix class does
	for (size_t ii=1; ii<=dim; ii++)
		pp[ii]=ii; //no permutations yet

	for (size_t jj=1; jj<dim; jj++) { //pivoting, last column remains unchanged
		size_t ipiv=jj;
		double piv=MM(pp[jj], jj);
		for (size_t ii=jj+1; ii<=dim; ii++) { //rows below diagonal
			if (fabs(MM(pp[ii], jj))>fabs(piv)) { //biggest element for stability
				ipiv=ii;
				piv=MM(pp[ii], jj);
			}
		}
		const size_t tmp=pp[jj]; //virtual row swapping
		pp[jj]=pp[ipiv];
		pp[ipiv]=tmp;
		for (size_t ii=jj+1; ii<=dim; ii++) {
			const double ff=MM(pp[ii], jj)/MM(pp[jj], jj); //elimination factor
			MM(pp[ii], jj)=ff; //save factor instead of produced zero
			for (size_t x=jj+1; x<=dim; ++x) //multiply all elements to the right
				MM(pp[ii], x)=MM(pp[ii], x)-ff*MM(pp[jj], x);
		}
	} //endfor j
}

bool Matrix::gaussSolve(Matrix& MM, Matrix& AA, Matrix& XX) //solve M·X=A
{ //solve an equation system with Gauss elimination and partial pivoting
	if (MM.getNx()!=MM.getNy())
		throw IOException("Trying to solve M·X=A for non-square matrix M.", AT);
	if (MM.getNy()!=AA.getNy())
		throw IOException("Trying to solve M·X=A, but the dimensions of M and A do not match.", AT);

	const size_t dim=MM.getNx();
	const size_t sys=AA.getNx();
	XX.resize(dim, sys);
	std::vector<size_t> pp;
	gaussElimination(MM, pp);

	double det=1.;
	for (size_t ii=1; ii<=dim; ii++) //multiply diagonal elements
		det*=MM(pp[ii], ii); //determinant changes sign for each permutation, but we only check against 0
	if (IOUtils::checkEpsilonEquality(det, 0., epsilon))
		return false; //singular matrix

	for (size_t ii=1; ii<dim; ii++) { //repeat elimination for solution matrix
		for (size_t jj=ii+1; jj<=dim; jj++) {
			for (size_t xx=1; xx<=sys; xx++)
				AA(pp[jj], xx)=AA(pp[jj], xx)-MM(pp[jj], ii)*AA(pp[ii], xx); //make use of saved elimination factor
		}
	}

	for (size_t xx=1; xx<=sys; xx++) { //backwards substitution
		for (size_t ii=dim; ii>=1; ii--) {
			XX(ii, xx)=AA(pp[ii], xx);
			for (size_t jj=ii+1; jj<=dim; jj++)
				XX(ii, xx)=XX(ii, xx)-MM(pp[ii], jj)*XX(jj, xx); //put in X without permutation for right order
			XX(ii, xx)=XX(ii, xx)/MM(pp[ii], ii); //divide by the coefficient's factor
		}
	}

	return true;
}

bool Matrix::gaussSolve(const Matrix& MM, const Matrix& AA, Matrix& XX)
{
	Matrix NN(MM), BB(AA); //copy matrices to not destroy originals
	return gaussSolve(NN, BB, XX);
}

bool Matrix::gaussInverse(Matrix& MM)
{
	Matrix II;
	II.identity(MM.getNx());
	Matrix Inv;
	const bool success=gaussSolve(MM, II, Inv);
	MM=Inv;
	return success;
}

bool Matrix::gaussInverse(const Matrix& MM, Matrix& Inv)
{
	Matrix NN(MM); //copy matrix to not destroy original
	const bool success=gaussInverse(NN);
	Inv=NN;
	return success;
}

double Matrix::gaussDet(Matrix& MM)
{
	std::vector<size_t> pp;
	gaussElimination(MM, pp);
	double det=1.;
	for (size_t ii=1; ii<=MM.getNx(); ii++) //multiply diagonal elements
		det *= MM(pp[ii], ii);

	for (size_t ii=1; ii<=MM.getNx(); ii++) {
		while (ii!=pp[ii]) { //roll back permutations
			const size_t tmp=pp[ii];
			pp[ii]=pp[tmp];
			pp[tmp]=tmp;
			det*=-1.; //determinant changes sign for each permutation
		}
	}
	return det;
}

double Matrix::normEuclid(const Matrix& vv)
{
	double sum=0.;
	if (vv.getNx()==1) {
		for (size_t jj=1; jj<=vv.getNy(); jj++)
			sum+=vv(jj, 1)*vv(jj, 1);
		return sqrt(sum);
	} else if (vv.getNy()==1) {
		for (size_t ii=1; ii<=vv.getNx(); ii++)
			sum+=vv(1, ii)*vv(1, ii);
		return sqrt(sum);
	} else {
		throw IOException("Euclidean norm l2 is only possible for vectors.", AT);
	}
}

bool Matrix::isIdentity() const
{
	if (nrows!=ncols) {
		std::ostringstream tmp;
		tmp << "A non-square matrix ";
		tmp << "(" << nrows << "," << ncols << ") can not be the identity matrix!";
		throw IOException(tmp.str(), AT);
	}

	bool is_identity=true;
	for (size_t ii=1; ii<=nrows; ii++) {
		for (size_t jj=1; jj<=ncols; jj++) {
			const double val = operator()(ii,jj);
			if (ii!=jj) {
				if (IOUtils::checkEpsilonEquality(val,0.,epsilon_mtr)==false) {
					is_identity=false;
					break;
				}
			} else {
				if (IOUtils::checkEpsilonEquality(val,1.,epsilon_mtr)==false) {
					is_identity=false;
					break;
				}
			}
		}
	}

	return is_identity;
}

bool Matrix::isIdentity(const Matrix& A)
{
	return A.isIdentity();
}

void Matrix::partialPivoting(std::vector<size_t>& pivot_idx)
{
	pivot_idx.clear();

	//bad luck: if a row has several elements that are max of their columns,
	//we don't optimize its position. Ie: we can end up with a small element
	//on the diagonal
	for (size_t jj=1; jj<=ncols; jj++) {
		const size_t old_ii = jj;
		const size_t new_ii = findMaxInCol(jj);
		if (new_ii!=jj) { //ie: pivoting needed
			swapRows(old_ii, new_ii);
			pivot_idx.push_back(new_ii);
		} else
			pivot_idx.push_back(old_ii);
	}
}

void Matrix::partialPivoting()
{
	std::vector<size_t> pivot_idx;
	partialPivoting(pivot_idx);
}

void Matrix::maximalPivoting()
{
	std::vector<size_t> pivot_idx;
	Matrix tmp( *this );

	for (size_t ii=1; ii<=nrows; ii++) {
		const double scale = operator()(ii,findMaxInRow(ii));
		for (size_t jj=1; jj<=ncols; jj++) {
			operator()(ii,jj) /= scale;
		}
	}
	tmp.partialPivoting(pivot_idx);

	//pivot on original matrix //HACK: not finished yet!
	throw IOException("Method not implemented yet!!", AT);
}

/*void Matrix::bidiagonalize() {
	//Matrix e(1,ncols);
	std::vector<double> e(ncols+1); //so we remain compatible with matrix index
	double g=0., x=0.;

	for (size_t ii=1; ii<=ncols; ii++) {
		e[i]=g; s=0.; l=ii+1;
		for (size_t jj=ii; jj<=m; jj++) s += ( operator()(ii,jj)*operator()(ii,jj) );
	}
}*/

//return the index of the line containing the highest absolute value at column col
size_t Matrix::findMaxInCol(const size_t &col)
{
	size_t row_idx = 0;
	double max_val=0.;

	for (size_t ii=1; ii<=nrows; ii++) {
		const double val = fabs( operator()(ii,col) );
		if ( val>max_val) {
			max_val = val;
			row_idx = ii;
		}
	}
	return row_idx;
}

//return the index of the line containing the highest absolute value at column col
size_t Matrix::findMaxInRow(const size_t &row)
{
	size_t col_idx = 0;
	double max_val=0.;

	for (size_t jj=1; jj<=ncols; jj++) {
		const double val = fabs( operator()(row,jj) );
		if ( val>max_val ) {
			max_val=val;
			col_idx=jj;
		}
	}
	return col_idx;
}

double Matrix::maxCoeff(size_t& max_row, size_t& max_col) const
{
	double max=vecData[0];
	max_row=max_col=1;
	for (size_t ii=0; ii<nrows; ++ii) {
		for (size_t jj=0; jj<ncols; ++jj) {
			if (vecData[ii*nrows+jj] > max) {
				max = vecData[ii*nrows+jj];
				max_row = ii+1;
				max_col = jj+1;
			}
		}
	}
	return max;
}

void Matrix::swapRows(const size_t &i1, const size_t &i2)
{
	for (size_t jj=1; jj<=ncols; jj++) {
		const double tmp = operator()(i2,jj);
		operator()(i2,jj) = operator()(i1,jj);
		operator()(i1,jj) = tmp;
	}
}

void Matrix::swapCols(const size_t &j1, const size_t &j2)
{
	for (size_t ii=1; ii<=nrows; ii++) {
		const double tmp = operator()(ii,j2);
		operator()(ii,j2) = operator()(ii,j1);
		operator()(ii,j1) = tmp;
	}
}

unsigned int Matrix::eigenvaluesJacobi(Matrix& AA, Matrix& DD)
{
	/*
	 * Cf. http://physik.uni-graz.at/~uxh/teaching/computational-physics1/kapitel11.pdf with similar notation. In short:
	 * Find Q for which Q^-1·A·Q=D, with D=diag(lambda_1, lambda_2, ...) the eigenvalues.
	 * Q is built iteratively, i. e. Q=Q_1·Q_2·...·Q_N <-- the elements of Q are beta, ss, cc
	 * The eigenvalue problem is solved because
	 * A·Q=Q·D=Q·diag(lambda_1, lambda_2, ...) --> Q=(v_1, v_2, ...) ==> A(v_1, v_2, ...)=(lambda_1, lambda_2, ...)
	 * Define A'=Q^T·A·Q, the result of a similarity transformation <-- beta, ss, cc are chosen such that a'_pq=0
	 */

	const size_t dim=AA.getNx(); //A assumed to be real and symmetrical! This is not checked here!
	DD.identity(dim);

	unsigned int counter=0; //count iterations
	while(jacobiEpsilon(AA)>epsilon) {
		counter++;
		for (size_t pp=1; pp<dim; pp++) {
			for (size_t qq=pp+1; qq<=dim; qq++) {
				if ( IOUtils::checkEpsilonEquality(fabs(AA(pp, qq)), 0., epsilon) )
					continue; //nothing to transform

				const double beta=(AA(qq, qq)-AA(pp, pp))/(2.*AA(pp, qq)); //calculate all the constants, i. e. the elements of Q
				const double ssquare=0.5-0.5*beta*(1./sqrt(1.+beta*beta)); //negative root solution is more stable
				const double csquare=0.5+0.5*beta*(1./sqrt(1.+beta*beta));
				const double sc=0.5*(1./sqrt(1.+beta*beta));
				const double ss=sqrt(ssquare);
				const double cc=sqrt(csquare);

				for (size_t ii=1; ii<=dim; ii++) { //calculate a'_ip, a'_iq and fill symmetrically
					if ((ii!=pp) && (ii!=qq)) {
						const double aold=AA(ii, pp);
						AA(ii, pp)=aold*cc-AA(ii, qq)*ss;
						AA(ii, qq)=aold*ss+AA(ii, qq)*cc;
						AA(pp, ii)=AA(ii, pp);
						AA(qq, ii)=AA(ii, qq);
					}
				}

				const double old=AA(pp, pp); //calculate a'_ip, a'_iq and fill symmetrically
				AA(pp, pp)=old*csquare+AA(qq, qq)*ssquare-2.*AA(pp, qq)*sc;
				AA(qq, qq)=old*ssquare+AA(qq, qq)*csquare+2.*AA(pp, qq)*sc;
				AA(pp, qq)=0.;
				AA(qq, pp)=0.;

				for (size_t jj=1; jj<=dim; jj++) { //build eigenvectors
					const double dold=DD(jj, pp);
					DD(jj, pp)=DD(jj, pp)*cc-DD(jj, qq)*ss;
					DD(jj, qq)=dold*ss+DD(jj, qq)*cc;
				}
			} //endfor qq
		} //endfor pp
	} //endwhile

   return counter;
}

double Matrix::jacobiEpsilon(Matrix& AA) //halting criteria for Jacobi eigenvalue search
{
	double s1=0, s2=0;
	for (size_t ii=1; ii<=AA.getNx(); ++ii) {
		for (size_t jj=1; jj<=AA.getNx(); ++jj) {
			s1+=AA(ii, jj)*AA(ii, jj); //sum of squares of matrix elements
			if (ii!=jj)
				s2+=AA(ii, jj)*AA(ii, jj); //sum of squares of non-diagonal elements
		}
	}
	return s2/s1;
}

void Matrix::svdJacobi(const Matrix& MM, Matrix& UU, Matrix& SS, Matrix& VV)
{
	UU.resize(MM.getNy(), MM.getNy(), 0.); //init sizes: A(i, j)=U(i, i)·S(i, j)·V(j, j)^T
	VV.resize(MM.getNx(), MM.getNx(), 0.);
	SS.resize(MM.getNy(), MM.getNx(), 0.);

	Matrix EE(MM*MM.getT()); //E is symmetrical and will be transformed to eigenvalues

	(void) Matrix::eigenvaluesJacobi(EE, UU); //E gets eigenvalues at diagonal, UU are the eigenvectors
	Matrix LL(EE.getDiagonal().getT()); //extract eigenvalues as column vector

	Matrix::sortEigenvalues(LL, UU); //put at diagonal from biggest to smallest
	for (size_t ii=1; ii<=SS.getNx(); ii++) { //the diagonal matrix S is the square root of the sorted eigenvalues
		SS(ii, ii)=sqrt(LL(ii, 1));
		const Matrix colV=MM.getT()*UU.getCol(ii)/SS(ii, ii); //A^T·v_i=sigma_i·u_i with v_i being eigenvectors of V
		VV.setCol(ii, colV); //eigenvectors of A^T·A and A·A^T are not independent -> no Jacobi recalculation
	}
	VV.T();
} //cf. http://web.mit.edu/be.400/www/SVD/Singular_Value_Decomposition.htm

void Matrix::sortEigenvalues(Matrix& EE, Matrix& VV)
{ //Bubblesort
	bool swapped;
	do {
		swapped=false;
		for (size_t ii=2; ii<=EE.getNy(); ii++) {
			if (EE(ii-1, 1)<EE(ii, 1)) { //swap pair-wise
				const double tmp=EE(ii, 1);
				EE(ii, 1)=EE(ii-1, 1);
				EE(ii-1, 1)=tmp;
				swapped=true;
				VV.swapCols(ii, ii-1); //also swap the eigenvector columns
			}
		}
	} while (swapped);
}

} //end namespace
