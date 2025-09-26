/***********************************************************************************/
/*  Copyright 2010 WSL Institute for Snow and Avalanche Research    SLF-DAVOS      */
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
#ifndef MATRIX_H
#define MATRIX_H

#include <vector>
#include <iostream>

namespace mio {

/**
 * @class Matrix
 * @brief This class implements the basic operations on matrices.
 * Elements are access in matrix notation: that is A(1,2) represents the second element of the
 * first line. Index go from 1 to nrows/ncols.
 *
 * It might not be the best ever such implementation, but the goal is to provide a standalone matrix class.
 * It might be later possible to chose between using the embedded implementation or to act as a
 * front end to BLAS for those who have it installed on their system.
 *
 * If the compilation flag NOSAFECHECKS is used, bounds check is turned off (leading to increased performances).
 *
 * @ingroup data_str
 * @author Mathias Bavay
 */
class Matrix {
	public:
		Matrix() : vecData(), ncols(0), nrows(0) {}

		/**
		* @brief A constructor that creates a matrix of a given size
		* @param rows number of rows of the new matrix
		* @param cols number of columns of the new matrix
		*/
		Matrix(const int& rows, const int& cols);
		Matrix(const size_t& rows, const size_t& cols) : vecData(rows*cols), ncols(cols), nrows(rows) {}

		/**
		* @brief A constructor that creates a matrix filled with constant values
		* @param rows number of rows of the new matrix
		* @param cols number of columns of the new matrix
		* @param init initial value to fill the matrix with
		*/
		Matrix(const size_t& rows, const size_t& cols, const double& init) : vecData(rows*cols, init), ncols(cols), nrows(rows) {}

		/**
		* @brief A constructor that creates a diagonal matrix of size n
		* @param n dimension of the new square matrix
		* @param init initial value to fill the matrix with
		*/
		Matrix(const size_t& n, const double& init);

		/**
		* @brief A constructor that takes a data vector to fill the matrix
		* @param rows number of rows of the new matrix
		* @param cols number of columns of the new matrix
		* @param data data vector to init the matrix (must be of size rows*cols)
		*/
		Matrix(const size_t& rows, const size_t& cols, const std::vector<double> data) : vecData(data), ncols(cols), nrows(rows) {}

		/**
		* @brief Convert the current matrix to a identity matrix of size n
		* @param n dimension of the new square matrix
		* @param init initial value to fill the matrix diagonal with
		*/
		void identity(const size_t& n, const double& init = 1.);

		void resize(const size_t& rows, const size_t& cols);
		void resize(const size_t& rows, const size_t& cols, const double& init);
		void resize(const size_t& rows, const size_t& cols, const std::vector<double>& data);

		/**
		* @brief get the dimensions of the current object
		* @param rows number of rows of the matrix
		* @param cols number of columns of the matrix
		*/
		void size(size_t& rows, size_t& cols) const;

		size_t getNx() const {return ncols;}
		size_t getNy() const {return nrows;}

		/**
		* @brief free the memory and set the matrix dimensions to (0,0)
		*/
		void clear();

		/**
		* @brief fill the matrix with random numbers.
		* @param range range of the randoms numbers (they will be between -range and +range)
		*/
		void random(const double& range);

		double& operator ()(const size_t& x, const size_t& y);
		double operator ()(const size_t& x, const size_t& y) const;

		/**
		* @brief Converts a 1x1 matrix to a scalar.
		* @return scalar value
		*/
		double scalar() const;
		static double scalar(const Matrix& m);

		/**
		* @brief Dot product.
		* @return scalar value
		*/
		static double dot(const Matrix& A, const Matrix& B);

		/**
		* @brief matrix transpose.
		* @return transposed matrix
		*/
		Matrix getT() const;
		static Matrix T(const Matrix& m);
		void T();

		/**
		* @brief matrix invert.
		* It first performs LU decomposition and then computes the inverse by
		* backward and forward solving of LU * A-1 = I
		* see Press, William H.; Flannery, Brian P.; Teukolsky, Saul A.; Vetterling, William T. (1992), "LU Decomposition and Its Applications", Numerical Recipes in FORTRAN: The Art of Scientific Computing (2nd ed.), Cambridge University Press, pp. 34–42
		* @return inversed matrix
		*/
		Matrix getInv() const;
		bool inv();

		/**
		* @brief Copy a matrix row to a new 1-row-matrix
		* @param i Row index
		* @return The row as a new matrix
		*/
		Matrix getRow(const size_t i) const;

		/**
		* @brief Set a matrix row from a 1-row-matrix
		* @param i Row index
		* @param row The new row
		*/
		void setRow(const size_t i, const Matrix& row);

		/**
		* @brief Copy a matrix column to a new 1-column-matrix
		* @param j Column index
		* @return The column as a new matrix
		*/
		Matrix getCol(const size_t j) const;

		/**
		* @brief Set a matrix column from a 1-column-matrix
		* @param j Column index
		* @param col The new column
		*/
		void setCol(const size_t j, const Matrix& col);

		/**
		* @brief Copy the matrix diagonal to a new 1-column-matrix
		* @return The diagonal as a new matrix
		*/
		Matrix getDiagonal() const;

		/**
		* @brief Extract a submatrix to a new matrix.
		* IOUtils::npos for an index means "from beginning" (i. e. 1) or "until end"
		* (i. e. ncols resp. nrows).
		* @param r_low Start from this row
		* @param r_high End at this row
		* @param c_low Start from this column 
		* @param c_high End at this column
		* @return The extracted submatrix
		*/
		Matrix extract(size_t r_low, size_t r_high, size_t c_low, size_t c_high) const;
		
		/**
		* @brief Find the first maximum value in the matrix
		* @param max_row Row index of the maximum
		* @param max_col Column index of the maximum
		* @return The maximum value
		*/
		double maxCoeff(size_t& max_row, size_t& max_col) const;

		/**
		* @brief matrix solving for A·X=B.
		* It first performs LU decomposition and then solves A·X=B by
		* backward and forward solving of LU * X = B
		* @param A A matrix
		* @param B B matrix
		* @return solution matrix
		*/
		static Matrix solve(const Matrix& A, const Matrix& B);

		/**
		* @brief matrix solving for A·X=B.
		* It first performs LU decomposition and then solves A·X=B by
		* backward and forward solving of LU * X = B
		* @param A A matrix
		* @param B B matrix
		* @param X solution matrix
		* @return true is success
		*/
		static bool solve(const Matrix& A, const Matrix& B, Matrix& X);

		/**
		* @brief Solving system of equations using Thomas Algorithm
		* The following function solves a A·X=B with X and B being vectors
		* and A a tridiagonal matrix, using Thomas Algorithm
		* (see http://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm)
		* @author Nander Wever, Mathias Bavay
		* @param A A matrix
		* @param B B matrix
		* @return solution matrix
		*/
		static Matrix TDMA_solve(const Matrix& A, const Matrix& B);

		/**
		* @brief Solving system of equations using Thomas Algorithm
		* The following function solves a A·X=B with X and B being vectors
		* and A a tridiagonal matrix, using Thomas Algorithm
		* (see http://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm)
		* @author Nander Wever, Mathias Bavay
		* @param A A matrix
		* @param B B matrix
		* @param X solution matrix
		* @return true is success
		*/
		static bool TDMA_solve(const Matrix& A, const Matrix& B, Matrix& X);

		/**
		* @brief matrix determinant
		* @return determinant
		*/
		double det() const;

		/**
		* @brief Gaussian elimimination with partial pivoting.
		* This function performs row reduction with (virtual) row-swapping.
		* Attention: The original matrix is destroyed!
		* @author Michael Reisecker
		* @param[in,out] M The matrix to reduce
		* @param[out] p Used permutation vector (ignore p[0])
		*/
		static void gaussElimination(Matrix& M, std::vector<size_t>& p);

		/**
		* @brief Equation system solver via Gaussian elimination.
		* This function solves a set of equations M·X=A via Gauss elimination with partial pivoting.
		* Attention: The original matrices are destroyed!
		* @author Michael Reisecker
		* @param[in] M The matrix M in M·X=A
		* @param[in] A The solution vector (or matrix for multiple equations) A in M·X=A
		* @param[out] X The calculated solution X in M·X=A
		* @return False if the equations are linearly dependent
		*/
		static bool gaussSolve(Matrix& M, Matrix& A, Matrix& X);

		/**
		* @brief Convenience call to the solver with input matrix preservation.
		* This function makes a copy of the input matrices and invokes the solver on that, i. e. they
		* are not changed in the process.
		* @param[in] M The matrix M in M·X=A
		* @param[in] A The solution vector (or matrix for multiple equations) A in M·X=A
		* @param[out] X The calculated solution X in M·X=A
		* @return False if the equations are linearly dependent
		*/
		static bool gaussSolve(const Matrix& M, const Matrix& A, Matrix& X);

		/**
		* @brief Matrix inversion via Gaussian elimination.
		* Attention: The original matrix is destroyed!
		* @author Michael Reisecker
		* @param[in,out] M The matrix to invert
		* @return False if the matrix is singular
		*/
		static bool gaussInverse(Matrix& M);

		/**
		* @brief Convenience call for matrix inversion with input matrix preservation.
		* This function makes a copy of the input matrix, i. e. it is left unchanged by the solver.
		* @param[in] M The matrix to invert
		* @param[out] Inv The inverse matrix
		* @return False if the matrix is singular
		*/
		static bool gaussInverse(const Matrix& M, Matrix& Inv);

		/**
		* @brief Get the matrix determinant via Gaussian elimination.
		* Attention: The original matrix is destroyed!
		* @author Michael Reisecker
		* @param[in] M Matrix to calculate the determinant for
		* @return The determinant
		*/
		static double gaussDet(Matrix& M);

		/**
		* @brief Calculate Euclidean norm (l2) for a vector.
		* The matrix must either be a row or a column vector.
		* @param[in] vv Vector to calculate norm for
		* @return L2 norm
		*/
		static double normEuclid(const Matrix& vv);

		/**
		* @brief matrix LU decomposition.
		* Perform LU decomposition by the Dolittle algorithm,
		* (cf http://math.fullerton.edu/mathews/numerical/linear/dol/dol.html)
		* @param L lower diagonal matrix
		* @param U upper diagonal matrix
		* @return false if the decomposition can not be performed (division by zero)
		*/
		bool LU(Matrix& L, Matrix& U) const;

		/**
		* @brief matrix partial pivoting.
		* This reorders the rows so that each diagonal element is the maximum in its column
		* (see https://secure.wikimedia.org/wikipedia/en/wiki/Pivot_element)
		* @param pivot_idx new indices (to apply when solving A * X = B, for example
		*/
		void partialPivoting(std::vector<size_t>& pivot_idx);
		void partialPivoting();

		void maximalPivoting();

		/**
		* @brief matrix bidiagonalization
		* This uses Householder's reduction, see Golub, 1970.
		* (see https://secure.wikimedia.org/wikipedia/en/wiki/Bidiagonalization)
		*/
		//void bidiagonalize();

		/**
		* @brief Eigenvalue and eigenvector computation of symmetrical matrices.
		* This function performs the iterative Jacobi method, cf. https://en.wikipedia.org/wiki/Jacobi_eigenvalue_algorithm
		* Attention: The given matrix must be symmetrical on input!
		* @author Michael Reisecker
		* @param[in,out] AA The matrix to search eigenvalues for. They will be found on its diagonal after transformation (unsorted).
		* @param[out] DD Eigenvectors
		* @return Number of iterations that were needed
		*/
		static unsigned int eigenvaluesJacobi(Matrix& AA, Matrix& DD);
		static double jacobiEpsilon(Matrix& AA);

		/**
		* @brief Singular Value Decomposition.
		* This function calculates the SVD of a matrix. It uses an eigenvector search on M·M^T, which means problems may be squared.
		* @author Michael Reisecker
		* @param[in] MM The matrix to factorize.
		* @param[out] UU Left singular vectors
		* @param[out] SS Diagonal matrix with the singular values
		* @param[out] VV Right singular vectors
		*/
		static void svdJacobi(const Matrix& MM, Matrix& UU, Matrix& SS, Matrix& VV);
		static void sortEigenvalues(Matrix& EE, Matrix& VV);
		void swapCols(const size_t &j1, const size_t &j2); //used by svd

		const std::string toString(const int& precision=2, const bool& prettify=true) const;

		Matrix& operator+=(const Matrix& rhs);
		const Matrix operator+(const Matrix& rhs) const;
		Matrix& operator+=(const double& rhs);
		const Matrix operator+(const double& rhs) const;

		Matrix& operator-=(const Matrix& rhs);
		const Matrix operator-(const Matrix& rhs) const;
		Matrix& operator-=(const double& rhs);
		const Matrix operator-(const double& rhs) const;

		Matrix& operator*=(const Matrix& rhs);
		const Matrix operator*(const Matrix& rhs) const;
		Matrix& operator*=(const double& rhs);
		const Matrix operator*(const double& rhs) const;

		Matrix& operator/=(const double& rhs);
		const Matrix operator/(const double& rhs) const;

		bool operator==(const Matrix&) const; ///<Operator that tests for equality
		bool operator!=(const Matrix&) const; ///<Operator that tests for inequality

		/**
		* @brief check if a matrix is the identity matrix
		* @return true if it is I
		*/
		bool isIdentity() const;
		static bool isIdentity(const Matrix& A);

		static const double epsilon, epsilon_mtr;

	protected:
		std::vector<double> vecData;
		size_t ncols;
		size_t nrows;

		size_t findMaxInCol(const size_t &col);
		size_t findMaxInRow(const size_t &row);
		void swapRows(const size_t &i1, const size_t &i2);

};

} //end namespace

#endif
