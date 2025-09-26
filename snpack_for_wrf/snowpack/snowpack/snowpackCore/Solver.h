/* **********************************************************************************************/
/*                                        stand-alone                                          */
/*                               Derived from RESEARCH VERSION 9.0                             */
/* **********************************************************************************************/
/* **********************************************************************************/
/*  Copyright WSL Institute for Snow and Avalanche Research    SLF-DAVOS           */
/* **********************************************************************************/
/* This file is part of Snowpack.
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

#ifndef  SOLVER_H
#define  SOLVER_H

#include <cstddef> //needed for int

/**
 * @file Solver.h
 * This module define the interface functions to access the direct solver and explain the
 * sequence of function calls requested to solve a linear system of equations. Solves the
 * linear equation system: [A]{X} = {B} --> {X} := [A]-1{B}, where [A] is a sparse symmetric
 * or unsymmetric, structure-symmetric NxN matrix, {B} is a single right-hand-side vector and
 * {X} is the sough solution vector both of dimension N. In the solution process the
 * right-hand-side vector {B} is overwritten by the solution vector {X}
 * When the linear system of equations stam from a FE method for a vector field or a similar
 * one is possible and advantageous to define a multiplicity factor M which is nothing but the
 * dimension of the vector field. In this case the linear system of equations is defined as we
 * would work with a scalar field instead of a vector field. In this case each defined
 * equation and unknown is replaced by a set of M equations and M unknowns and each
 * coefficients of the matrix [A] is replaced by a full (MxM) matrix. Thus, if a the
 * multiplicity factor greather than one has been defined the true number of equations and
 * unknonw is given by the product (NxM). E.g. if you want to solve a linear system of
 * dimension DIM with a full matrix [A], simply define only one equation N=1 with multiplicity
 * M=DIM
 * The algorithm is based on the LU-factorisation of the matrix [A]=[L][U] with permutations
 * of the rows and columns of [A] such as to minimize fill-in acording to the
 * multiple-minimum-degree algorithm described by: [Alan Geroge, Joseph W.H. Liu, The
 * Evolution of the minimum degree ordering algorithm, SIAM Review, Vol. 31, No. 1 pp 1-19,
 * March 1989].
 * All direct-solver (ds-)functions return 0 = FALSE if succesfull, respectively 1 = TRUE with
 * an error message on standard output if an error has occurred.
 *
 * @author GUIDO SARTORIS  ETH ZUERICH
 */

typedef struct
{
	int     nChunks;
	int     pChunksSize;
	char    **pChunks;
	int     TotChunkSize;
} SD_CHUNK_DATA;

typedef struct
{
	int                Row0;
	int                Row1;
	int                nCol;
	int                nColBlock;
	int                iColBlock;
	int                iFloat;
}  SD_ROW_BLOCK_DATA;

typedef struct
{
	int                Dim;
	int                *pPerm;
	int                 nRowBlock;
	SD_ROW_BLOCK_DATA  *pRowBlock;
	int                 nColBlock;
	int                *pFirstColBlock;
	int                *pSizeColBlock;
	int                 SizeBlockJump;
	int                *pBlockJump;
	int                 SizeUpper;
	double              *pUpper;
}  SD_BLOCK_MATRIX_DATA;

typedef struct SD_COL_DATA
{
	int                 Col;
	struct SD_COL_DATA  *Next;
} SD_COL_DATA;

typedef struct SD_ROW_DATA
{
	SD_COL_DATA  *Col;
}  SD_ROW_DATA;

typedef struct
{
	int                   nRow;
	int                   *pPerm;
	int                   *pPermInv;
	int                    nSupernode;
	int                   *pSupernode;

	SD_ROW_DATA           *pRow;
	SD_CHUNK_DATA          PoolCol;
	SD_COL_DATA           *FreeCol;
	int                    nFreeCol;
	int                    nCol;
}  SD_CON_MATRIX_DATA;

typedef struct SD_COL_BLOCK_DATA
{
	int Col0, Col1;
	struct SD_COL_BLOCK_DATA  *Next;
} SD_COL_BLOCK_DATA;

typedef union
{
	SD_ROW_BLOCK_DATA     UnusedData;
	struct
	{
		int                Row0;
		int                Row1;
	}  Any;
	struct
	{
		int                Row0;
		int                Row1;
		SD_COL_BLOCK_DATA *ColBlock;
	}  Data;
}  SD_TMP_ROW_BLOCK_DATA;

typedef struct
{
	int                   nRow;
	int                   *pPerm;

	int                    nRowBlock;
	SD_TMP_ROW_BLOCK_DATA *pRowBlock;
	int                    nColBlock;
	SD_CHUNK_DATA          PoolColBlock;
	SD_COL_BLOCK_DATA     *FreeColBlock;
}  SD_TMP_CON_MATRIX_DATA;

/**
 * @struct SD_MATRIX_DATA
* @brief When the user define a matrix, the software return a pointer to an opaque type i.e. a
* pointer to void as index to reference the matrix. This pointer is actually the pointer to
* the SD_MATRIX_DATA data structure. This date structure is defined as a union of differnet
* matrix data representations, and the type of data actually stored depend on the evolution
* of the algorithn.
*/

typedef enum StateType {ConMatrix, BlockConMatrix, BlockMatrix}  StateType;

typedef  struct
{
	int   nEq;
	int   nDeletedEq;

	StateType State;
	union
	{  SD_CON_MATRIX_DATA      Con;
	SD_TMP_CON_MATRIX_DATA     TmpCon;
	SD_BLOCK_MATRIX_DATA       Block;
	}  Mat;
}  SD_MATRIX_DATA;

typedef enum SD_MATRIX_WHAT
{
   /*
    * For symbolically factorizing and optimizing the structure of the [L] and [U] matrices
    * after the connectivity of the matrix [A] has been defined by calling
    * ds_DefineConnectivity (...)
    */
   SymbolicFactorize = 1,
   /*
    * For numerically factorizing the matrix [A] = [L][U] in to the lower resp. upper
    * triangular matrices [L] resp. [U] after [A] has been symbolically factorized by calling
    * ds_Solve(SymbolicFactorize,...) and assembled by calling ds_AssemblLocalMatrix(...) for
    * each element.
    */
   NumericFactorize  = 2,
   /*
    * For solving for a new right hand side load vector {B} after the matrix [A] has been
    * numerically factorized by calling ds_Solve(NumericFactorize,...)
    */
   BackForwardSubst  = 4,
   /*
    * For both numerically factorizing the matrix [A] and solving for a 1st right hand side
    * vector {B} i.e. is equivalent to the calls ds_Solve(NumericFactorize,...) &
    * ds_Solve(BackForwardSubst,...)
    */
   ComputeSolution   = NumericFactorize | BackForwardSubst,
   /*
    * For resetting all real coefficients of the matrix [A] to zero before reassembling a new
    * matrix [A] with identical connectivity by calling ds_AssembleLocalMatrix(...) with
    * changed local element matrices [ElMat] but the same element equations list Eq[...]
    */
   ResetMatrixData   = 8,
   /*
    * For releasing all storage space needed for solving the current problem defined by the
    * data pointed to by *pMat
    */
   ReleaseMatrixData = 16

} SD_MATRIX_WHAT;

////////////////////////////////////////////
//functions that are really used

/**
* The next function represents the computational kernel of this direct solver. Its
* functionality depends on the input parameter: Code whose meanings are given below.
* Generally once the matrix as been defined and the element connectivity assembled, the user
* first perform a symbolic factorization process. After assembling the numerical matrix, a
* numerical factorization step follows together with a back- and for-ward substitution to
* compute a numerical solution of the linear system. The matrix data can be resetted in order
* to solve other linear systems having the same matrix connectivity without the need to newly
* symbolic factorize the matrix.
* NOTE: If a multiplicity factor has been defined we assume that the vector components are
* clustered together in the vector pVec.
*/

/**
 * @brief This function is needed for defining the system (matrix) connectivity i.e. the non-zero
 * coefficients of the matrix [A] i.e. which equation is connected to which one. For each
 * (finite) element we have to specifies a list of equations. Here, we assume that all
 * equations in the list are connected to eachother and thus lead to non-zero coefficients in
 * the matrix i.e. the element list of equations form a crique
 * This function is generally called for a single element (nEl = 1) or for a group of nEl
 * elements having criques of equal dimension. To contemporary define the element connectivity
 * for more elements set nEl>1 and store the list equations for each element in the array:
 * Eq[][Dim]. Of course nEl<=Dim must holds.
 * If a multiplicity factor greather than 1 has been defined we have only to define the
 * element connectivity for the first representative equations or first vector field component
 * i.e. as we would do for a scalar field, and thus all equations in the list must have a
 * number less than the specified matrix dimension
 * E.g. for an element with 4 nodes and multiplicity 3 we have a total true number of 12
 * equations forming a crique but only 4 = ( 12 / 3 ) equations must be specified and the
 * values must not exceed the specified dimension of the matrix [A].
 * After the list of equations for all elements have been specified the matrix [A] should be
 * symbolically factorized by calling the function: ds_Solve(SymbolicFactorize, ... ).
 * NOTE: Except the definition of the multiplicity in ds_Initialize(), all steps performed to
 * define the structure of matrix [A] are stricktly independent from the multiplicity
 *
 * @param [in] pMat0 pointer to the matrix [A] opaque data returned by ds_Initialize()
 * @param [in] nEq No. of equations for one element forming a crique
 * @param [in] Eq Element list of equations for more elements with equal no. of eqs.
 * @param [in] nEl No. of elements  ( 0 <= i "<" nEq ;  0 <= e "<" nEl )
 * @param [in] Dim first dimension of the 2D-array Eq[][Dim]
 */

int ds_DefineConnectivity( SD_MATRIX_DATA *const pMat0, const int& nEq, int Eq[], const int& nEl, const int& Dim );

/**
 * @brief This is the first function to be called prior to begin to work with any matrix. The user
 * must gives the number of equations i.e. the dimension of the matrix, the type of matrix
 * i.e. a symmetric or unsymmetric matrix and the multiplicity. The multiplicity specifies
 * that each coefficient of the matrix is actually a square matrix and each equation and
 * unknown is actually a set of equations and unknowns all of dimension equal to the defined
 * multiplicity factor. Thus the true dimension of the linear system is given by the specified
 * matrix dimension times the multiplicity. Of course a multiplicity value of one is the most
 * general case, but sometines especially by vector field computations the multiplicity is
 * simply given by the dimension of the vector to be computed and this allow to speed up many
 * integer operations. Only define a multiplicity factor greather than one, if the vector
 * field components, or a subset, are fully coupled to eachother. In fact, by definition of a
 * multiplicity we always reserve memory for the full coupled system among each vector
 * component, thus the memory requirement increase quadratically with the defined multiplicity
 * factor. The function return a pointer to an opaque data type as matrix identifier.
 *
 * @param MatDim dimension of the matrix [A]. The true number of equations
 * and unknowns is given by: MatDim * Multiplicity
 * @param ppMat A pointer to an opaque data type storing data related to the matrix [A]
 */
int ds_Initialize( const int& MatDim, SD_MATRIX_DATA **ppMat );

/**
* @brief This function assemble the element square matrix [ElMat] for one element with nEq*M x nEq*M
* real coefficients in to the global matrix [A]. If a multiplicity factor M greather than 1
* has been defined the numerical values in the element matrix [ElMat] must be forall vector
* components clustered together. E.g. for a multiplicity of 3 i.e. a 3D vector field, the 3x3
* left-upper submatrix of [ElMat] must represent the coupling between the 3 vector field
* components.
* To perform this task we also newly require the element connectivity. The list of element
* equations should be the same or a subset of the list used previously when the matrix
* connectivity has been defined with a call to: ds_DefineConnectivity().
* ATTENTION: no error is detected if some of the the element connectivity defined when
* calling ds_AssembleLocalMatrix() are not included in those previously specified when
* calling ds_DefineConnectivity()
* If the matrix [A] has been declared as symmetric only the upper triangular part of
* [ElMat], i.e. only ElMat[i][j] = ElMat[Dim*i+j] with i<=j and i,j = 0...M*nEq-1 are used
* and need to be defined. In the unsymmetric case all M*nEq x M*nEq coefficients are used. It
* is assumed that in the calling program the array [ElMat] is dimensioned as ElMat[...][Dim]
* NOTICE: Of course the parameter Dim can be greater than M*nEq: Dim >= M*nEq
* After all element matrices have been assembled the matrix [A] = [L][U] can be factorised in
* to the lower [L] and upper [U] tringular matrices by calling ds_Solve(NumericFactorize,
* ).
 * @param [in] pMat0 pointer to the matrix [A] opaque data returned by ds_Initialize()
 * @param [in] nEq no. of equations for one element forming a crique
 * @param [in] Eq Element list of equations for one element.
 * @param [in] Dim first dimension of the 2D-array ElMat[][Dim]
 * @param [in] ElMat element square matrix to be assembled in the matrix [A]
*/
int ds_AssembleMatrix( SD_MATRIX_DATA *pMat0, const int& nEq, int Eq[], const int& Dim, const double *ElMat );

/**
 * @param [in] Code functionlaity code defined above
 * @param [in] pMat pointer to the matrix [A] opaque data
 * @param [in] pX right hand side vector {B} to be overwritten by the solution vector {X}:  B[i] := X[i]
 * @param [out] return false whenever the solve produced NaNs in the solution vector
 */
bool ds_Solve(const SD_MATRIX_WHAT& Code, SD_MATRIX_DATA *pMat, double *pX);

int ReleaseConMatrix( SD_CON_MATRIX_DATA * pMat );
int ReleaseBlockMatrix( SD_BLOCK_MATRIX_DATA * pMat );
#endif
