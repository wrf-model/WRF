/* GENERAL INFO
* AUTHOR: GUIDO SARTORIS ETH ZUERICH
*/
/*
* In this module are defined the function the user call
* in order to solve a linear system of equations
*/

#include <snowpack/snowpackCore/Solver.h>
#include <meteoio/MeteoIO.h>
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <cstring> //for memset
#include <math.h> //for isnan

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wcast-align"
#endif

#ifdef __GNUG__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#endif

static bool gd_MemErr;

typedef struct  {
	int *pC0, *pSize;
} pBLOCK;

int Permute(  int N ,
     int * Perm ,
     double * Vector
);
int InvertMatrix(  SD_BLOCK_MATRIX_DATA * pMat
  );
int InverseMatrixVector (  SD_BLOCK_MATRIX_DATA * pMat ,
     double * X
  );
int BuildSparseConFormat(SD_CON_MATRIX_DATA *pMat, int *pRowStart0, int *pColumn0);
static void  RunMmd(int neqns, int *xadj, int *adjncy, int *invp, int *perm, int delta, int *head, int *qsize, int *nsize, int *list, int *marker, int maxint, int *ncsub);
int ComputeTmpConMatrix(SD_CON_MATRIX_DATA *pMat0, SD_TMP_CON_MATRIX_DATA *pMat);
int  ComputeFillIn(SD_TMP_CON_MATRIX_DATA *pMat);
int ComputeBlockMatrix( SD_TMP_CON_MATRIX_DATA *pTmpMat, SD_BLOCK_MATRIX_DATA *pMat);

#define GD_MALLOC( pointer, TYPE, N, MSG )                                                     \
{                                                                                              \
	pointer = (TYPE *)malloc( sizeof(TYPE)*(N+1) );                                        \
	if ( pointer  ) {                                                                      \
		gd_MemErr = false;                                                             \
	} else {                                                                               \
		gd_MemErr = true; fprintf(stderr, "\n+++++ %s: %s\n",  "NO SPACE TO ALLOCATE", MSG);               \
	}                                                                                      \
}

#define GD_REALLOC( pointer, TYPE, N, MSG )                                                    \
{                                                                                              \
	if ( pointer )  {                                                                      \
  		pointer = (TYPE *)realloc( (char*)pointer, sizeof(TYPE)*(N+1) );               \
  		if ( pointer  ) {                                                                      \
			gd_MemErr = false;                                                             \
		} else {                                                                               \
			gd_MemErr = true; fprintf(stderr, "\n+++++ %s: %s\n", "NO SPACE TO REALLOCATE", MSG);               \
		}                                                                                   \
	} else {                                                                               \
  		GD_MALLOC(  pointer, TYPE, N, MSG );                                           \
	}                                                                                      \
}

#define GD_FREE( pointer )                                                                     \
{                                                                                              \
	if ( pointer ) {                                                                       \
   		free ( (char*) pointer );                                                      \
   		pointer = NULL;                                                                \
	}                                                                                      \
}                                                                                              \

/*
 * This section contains macros which are high vectorizable. On some computers they can be
 * substitued by appropriated function calls ( BLAS routines )
 */
#define VD_AXPY_JUMP(N_B, N, JUMP, A, X, Y ) /* Y[] = A*X[] + Y[] BLOCK-WISE IN Y */           \
{  double  a_=A, *x_=X, *y_=Y+JUMP[0];                                                         \
   int  n_;                                                                                    \
   for (n_=0; n_<N_B; y_+= JUMP[++n_])                                                         \
   for (int k_=N[n_]; 0<k_--;  ) *y_++ += (a_)*(*x_++) ;                                       \
}

#define VD_AXPY(N, A, X, Y )      /* Y[] = A*X[] + Y[] */                                      \
{  double  a_, *x_, *y_;                                                               \
   int  k_;                                                                           \
   for (x_=X, y_=Y, a_=A, k_=N; 0<k_--; ) *y_++ += (a_)*(*x_++) ;                              \
}

#define VD_AXPY_POS(N_B, N, POS, A, X, Y ) /* Y[] = A*X[] + Y[] BLOCK-WISE IN Y */             \
{  double  a_, *x_, *y_;                                                               \
   int  n_, k_;                                                                       \
   for (x_=X, y_=Y+POS[0], a_=A, n_=0; n_<N_B; y_= Y+POS[++n_])                                \
   for (k_=N[n_]; 0<k_--;  ) *y_++ += (a_)*(*x_++) ;                                           \
}

#define VD_DOT_POS(N_B, N, POS, X, Y, RESULT) /* RESULT = X[]*Y[]  BLOCK-WISE IN Y */          \
{  double  r_, *x_, *y_;                                                               \
   int  n_, k_;                                                                       \
   for (x_=X, y_=Y+POS[0], r_=0.0, n_=0; n_<N_B; y_= Y+POS[++n_])                              \
   for (k_=N[n_]; 0<k_--;  ) r_ += (*x_++)*(*y_++) ;                                           \
   RESULT = r_;                                                                                \
}

/**
 * @struct SD_CHUNK_DATA
* @brief The CHUNK_DATA is used to keep track of allocated memory chunks ( block of memory ). For
* each allocated chunk we store the pointer returned from the memory allocator, so that a
* later deallocation of the memory will be possible.
*/

#define SD_CHUNK_REALLOC_SIZE  250

#define SD_ALLOC_CHUNK(CHUNK,SIZE)                                                             \
{  if ( CHUNK.nChunks >= CHUNK.pChunksSize )                                                   \
   {  CHUNK.pChunksSize += SD_CHUNK_REALLOC_SIZE;                                              \
      GD_REALLOC( CHUNK.pChunks, char*, CHUNK.pChunksSize, "Chunk pointer Data" );             \
   }                                                                                           \
   GD_MALLOC( CHUNK.pChunks[ CHUNK.nChunks ], char, SIZE, "Chunk Data" );                      \
   CHUNK.TotChunkSize += SIZE;                                                                 \
   CHUNK.nChunks++;                                                                            \
}

#define SD_DESTROY_CHUNK(CHUNK)                                                                \
{  int i_;                                                                                     \
   for(i_=0; i_<CHUNK.nChunks; i_++) GD_FREE(CHUNK.pChunks[i_]);                               \
   GD_FREE(CHUNK.pChunks);                                                                     \
   CHUNK.TotChunkSize = 0;                                                                     \
}

/*
* BLOCK MATRIX DATA USED FOR NUMERICAL FACTORIZATION
*/

/**
 * @struct SD_ROW_BLOCK_DATA
* @brief The data structure to store the matrix for numerical factorization is a simple one. The
* matrix structure is after the mmd sorting algorithm and the symbolic factorization mainly
* composed of clustered non-zero matrix coefficients which form blocks. In this case we use a
* data strucutre to represent these row and column blocks.
* NOTE: The row blocks are simply what in the literature is specified as supernodes. We have
* kept the data structure as simple as possible to minimize the numerical operations and so
* the execution time.
* NOTE: When we define a system we have the possibility to define a multiplicity factor. This
* allows us to perform all initialization tasks ( input of incidences, symbolic
* factorization, block format calculation ) with all indices modulo the multiplicity factor
* to save time and memory. Thus the computed block format is always the same for any
* specified multiplicity factor only the block bounds are different.
* ATTENTION: The size of the permutation vector is only Dim/(Multiplicity Factor).
*/

#define  SD_MARKED    (1<<30) /* An flag bit used for permutation. Use:(1<<15) on PC */

#define SD_P_FIRST_COL_BLOCK(pMAT,pROW) (pMAT->pFirstColBlock + pROW->iColBlock)
#define SD_P_SIZE_COL_BLOCK(pMAT,pROW)  (pMAT->pSizeColBlock  + pROW->iColBlock)

#define SD_SEARCH_BLOCK_ROW(ROW, pROW_LOW, pROW_HIGH, pROW)                                    \
{  SD_ROW_BLOCK_DATA *low_, *high_, *mid_;                                                     \
   low_ = pROW_LOW; high_ = pROW_HIGH;                                                         \
   while( low_<=high_ )                                                                        \
   {  mid_ = low_ + ( high_ - low_ ) / 2;                                                      \
      if      ( ROW < mid_->Row0 ) high_ = mid_ - 1;                                           \
      else if ( ROW > mid_->Row1 ) low_  = mid_ + 1;                                           \
      else { pROW=mid_;  break;  }                                                             \
   }                                                                                           \
}

/*
* SPARSE MATRIX DATA USED INITIALLY TO READ THE CONNECTIVITY MATRIX
*/

#define SD_ROW(     NUM, pMAT)    ( (pMAT)->pRow[NUM]    )
#define SD_COL(pCOL)  ( (pCOL)->Col )

/*
* DATA FOR COLUMN AND COLUMN BLOCK ALLOCATION
*/
#define SD_N_ALLOC_COL  500

#define SD_ALLOC_COL(N_COL, pMAT)                                                              \
{  SD_ALLOC_CHUNK((pMAT)->PoolCol, sizeof(SD_COL_DATA)*N_COL);                                 \
   (pMAT)->FreeCol = ( SD_COL_DATA * ) (pMAT)->PoolCol.pChunks[ (pMAT)->PoolCol.nChunks-1 ];   \
   (pMAT)->nFreeCol = N_COL;                                                                   \
}

#define SD_GET_COL(pCOL, pMAT)                                                                 \
{  if  ( !(pMAT)->nFreeCol )  SD_ALLOC_COL(SD_N_ALLOC_COL, pMAT);                              \
   pCOL = (pMAT)->FreeCol++; (pMAT)->nFreeCol--;                                               \
}

/*
* To little speed-up memory operations for the SD_COL_BLOCK_DATA, we only allocate chunks of
* SD_COL_BLOCK_DATA and put each cell in a linked LIFO list of free SD_COL_BLOCK_DATA. When
* we need one cell of SD_COL_BLOCK_DATA we take it from the free list, by release of the data
* we put it again in the free list. A LIFO list also called a stack is important to avoid
* eccessive scattering of data in memory.
*/

#define SD_FREE_COL_BLOCK_0(pCOL_BLOCK, pMAT)                                                  \
{  (pCOL_BLOCK)->Next = (pMAT)->FreeColBlock; (pMAT)->FreeColBlock = (pCOL_BLOCK);  }

#define SD_FREE_COL_BLOCK(pCOL_BLOCK, pMAT)                                                    \
{  SD_FREE_COL_BLOCK_0(pCOL_BLOCK, pMAT); pMAT->nColBlock--;  }

#define SD_ALLOC_COL_BLOCK(N_COL_BLOCK, pMAT)                                                  \
{  SD_COL_BLOCK_DATA *pColBlock_;                                                              \
   SD_ALLOC_CHUNK((pMAT)->PoolColBlock, sizeof(SD_COL_BLOCK_DATA)*N_COL_BLOCK);                \
   pColBlock_ = ( SD_COL_BLOCK_DATA * )                                                        \
                (pMAT)->PoolColBlock.pChunks[ (pMAT)->PoolColBlock.nChunks-1 ];                \
   for(int i_=N_COL_BLOCK; 0<i_--; pColBlock_++) SD_FREE_COL_BLOCK_0(pColBlock_, pMAT);        \
}

#define SD_N_ALLOC_COL_BLOCK  500

#define SD_GET_COL_BLOCK(pCOL_BLOCK, pMAT)                                                     \
{  if  ( !(pMAT)->FreeColBlock )  SD_ALLOC_COL_BLOCK(SD_N_ALLOC_COL_BLOCK, pMAT);              \
   pCOL_BLOCK = (pMAT)->FreeColBlock; (pMAT)->FreeColBlock = (pMAT)->FreeColBlock->Next;       \
   pMAT->nColBlock++;                                                                          \
}

/*
* COLUMN DATA MANAGEMENT
*/

/**
* @brief The SD_FIND_COL macro, accept pROOT_COL as the first node of the list to start the search
* of the node with value COL. If the node is found the variable FOUND is set to TRUE and
* ppCOL will point to this node, if not found FOUND is set to FALSE and ppCOL will point to
* the entry point.
* NOTE. For 2D meshes is better to not enable SPARSE_BINARY_TREE and use a linear list of
* column coefficients instead of a binary tree of column coefficients.
*/

#define SD_FIND_COL(pROOT_COL, COL, ppCOL, FOUND)                                              \
{  SD_COL_DATA *pC_ ;                                                                          \
   FOUND   = 0;                                                                                \
   pC_     = (pROOT_COL);                                                                      \
   ppCOL   = &(pROOT_COL);                                                                     \
   while ( pC_ )                                                                               \
   {  if ( COL > SD_COL(pC_)  )  { ppCOL = &pC_->Next; pC_ = pC_->Next;  }                     \
      else { if ( COL == SD_COL(pC_) ) FOUND = 1;  break;  }                                   \
   }                                                                                           \
}

#define SD_INSERT_COL(ppCOL, pCOL, COL)                                                        \
{  pCOL->Col  = COL;                                                                           \
   pCOL->Next = *ppCOL;                                                                        \
   *ppCOL     = pCOL;                                                                          \
}

#define ERROR_SOLVER(MSG)      { printf("++++Errror:%s:%d:%s\n", __FILE__, __LINE__, MSG); return(1); }
#define USER_ERROR(MSG) { printf("++++Errror:gs_SolveMatrix:%s\n", MSG); return 1; }
#define EXIT(MSG)  {  printf("++++Exit:%s:%d:%s\n", __FILE__, __LINE__, MSG);  exit(1);   }

#define BLOCK_INIT(BLOCK,pCOL0,pSIZE) { BLOCK.pC0 = pCOL0; BLOCK.pSize = pSIZE; }
#define BLOCK_NEXT(BLOCK)             ( BLOCK.pC0++,       BLOCK.pSize++ )
#define BLOCK_C0(BLOCK)                 BLOCK.pC0[0]
#define BLOCK_C1(BLOCK)                (BLOCK.pC0[0]+BLOCK.pSize[0])
#define BLOCK_SIZE(BLOCK)               BLOCK.pSize[0]

#define pC0_FIRST_COL(  pROW)  (pMatFirstColBlock + pROW->iColBlock)
#define pSIZE_FIRST_COL(pROW)  (pMatSizeColBlock  + pROW->iColBlock)

#define FIND_COL_BLOCK(pFIRST_BLK, COL, ppBLK, FOUND)                                          \
{                                                                                              \
   FOUND      = 0;                                                                             \
   SD_COL_BLOCK_DATA *pB_ = (pFIRST_BLK);                                                      \
   int Col0_  = COL+1;                                                                      \
   int Col1_  = COL-1;                                                                      \
   while ( pB_ )                                                                               \
   {  if      ( Col1_ >  pB_->Col1  ) { ppBLK = &pB_->Next; pB_ = pB_->Next; }                 \
      else if ( Col0_ >= pB_->Col0  )                                                          \
      {  FOUND = 1;                                                                            \
         if      ( Col0_==pB_->Col0 ) pB_->Col0 = COL;                                         \
         else if ( Col1_==pB_->Col1 ) pB_->Col1 = COL;                                         \
         break;                                                                                \
      }                                                                                        \
      else   break;                                                                            \
   }                                                                                           \
}

/*
* Macros to compute the triangular factorization on a symmetric matrix stored packed row-wise
* in a one dimensional array. i.e the lower matrix coefficient are not stored. This macro is
* used to invert the pivot row block if its size is greater than 1.
*/

#define FACT_SYM_MAT(MAT,N_ROW,N_COL)                                                          \
{                                                                                              \
   if ( N_ROW>1 ) {                                                                            \
   const int m_n_1=N_COL-N_ROW+1;                                                              \
   double *Mat_k=MAT;                                                                           \
   for ( int n_k=N_COL; n_k>=m_n_1; n_k-- )                                                    \
   {  double Pivot = 1./(*Mat_k);                                                               \
      double *Mat_i = Mat_k++;                                                                  \
      for ( int n_i=n_k; n_i>m_n_1; Mat_k++ )                                                  \
      {  Mat_i += n_i--;  VD_AXPY(n_i, -(*Mat_k)*Pivot, Mat_k, Mat_i);  }                      \
      Mat_k += m_n_1 - 1;                                                                      \
   }                                                                                           \
  }                                                                                            \
}

/*
* This macro performs a binary search for the row: ROW. The block containing this row is
* returned by pROW
*/

#define FIRST_BLOCK_ROW(pMAT) ( pMAT->pRowBlock )
#define LAST_BLOCK_ROW(pMAT)  ( pMAT->pRowBlock + pMAT->nRowBlock - 1 )

#define SEARCH_ROW(ROW, pROW_LOW, pROW_HIGH, pROW)                                             \
{  SD_ROW_BLOCK_DATA *low_, *high_, *mid_;                                                     \
   low_ = pROW_LOW; high_ = pROW_HIGH;                                                         \
   while( low_<=high_ )                                                                        \
   {  mid_ = low_ + ( high_ - low_ ) / 2;                                                      \
      if      ( ROW < mid_->Row0 ) high_ = mid_ - 1;                                           \
      else if ( ROW > mid_->Row1 ) low_  = mid_ + 1;                                           \
      else { pROW=mid_;  break;  }                                                             \
   }                                                                                           \
}

/*
* This macro compute the block jump offsets between two rows. The first row must be a subset
* of the second one i.e. all coefficients of the first row must be present in the second one.
*/
#define BLOCK_JUMP(nCOL0, pCOL0, pSIZE0, pCOL1, pSIZE1, JUMP)                                  \
{  int i_, *pCol0_, *pCol1_, *pSize0_,  *pSize1_, Size_, Col1_0_, Col1_1_;                     \
   pCol0_  = pCOL0;                                                                            \
   pCol1_  = pCOL1;                                                                            \
   pSize0_ = pSIZE0;                                                                           \
   pSize1_ = pSIZE1;                                                                           \
   Col1_0_ = pCol1_[0];                                                                        \
   Col1_1_ = Col1_0_ + pSize1_[0];                                                             \
   for(i_=0; i_<nCOL0; i_++)                                                                   \
   {  Size_   = 0;                                                                             \
      while( pCol1_[0] + pSize1_[0] < pCol0_[0] )                                              \
      {  Size_ += Col1_1_ - Col1_0_;                                                           \
         Col1_1_  = ( Col1_0_ = (++pCol1_)[0] ) + (++pSize1_)[0];   }                          \
      JUMP[i_] = Size_ + pCol0_[0] - Col1_0_;                                                  \
      Col1_0_  = (pCol0_++)[0] + (pSize0_++)[0];                                               \
   }                                                                                           \
}

/*
* This macro compute for a matrix stored packed row-wise in a one dimensional array the
* position of a diagonal element in a given row.
*/

#define DIAGONAL(DIM,K) ( (K)*(DIM) -( (K)*((K)-1) )/2 )

/*
* A linear search is performed in the row pROW to find the column COL. This macro use the
* column value of the next column block to determine in which column block the column is to
* be found. In this case the dimension of the search array is set to the number of column
* block minus one. If the column block is not found, the block can only be the last defined
* column block. NOTE: Here we are forced to perform a linear search because we have to
* compute the total number of column coefficients defined prior the founded column block. A
* binary search could be used if instead of the column block size we store the sum of defined
* column coefficients. This is of course possible and only little change in the software are
* necessary, however, in this case we can no more pack in one integer the data for a column
* block definition.
*/

#define SEARCH_COL(COL, ROW, pMAT, pROW, FOUND, OFFSET)                                        \
{  int *col_, *size_;                                                   \
   col_     = SD_P_FIRST_COL_BLOCK(pMAT,pROW);                                                 \
   size_    = SD_P_SIZE_COL_BLOCK( pMAT,pROW);                                                 \
   const int delta_   = ROW - pROW->Row0;                                                                \
   OFFSET   = pROW->iFloat + DIAGONAL(pROW->nCol, delta_);                                     \
   {  ++col_;                                                                                  \
      for(int i_=pROW->nColBlock-1; (i_--)>0; OFFSET += size_[0], col_++, size_++)                 \
      {  if ( COL < col_[0] )  break;  }                                                       \
       --col_;                                                                                 \
      if ( COL >= col_[0]+size_[0] ) {  FOUND = 0; }                                           \
      else                           {  FOUND = 1;  OFFSET += COL - col_[0] - delta_;  }       \
   }                                                                                           \
}


/**
* @brief Allocate the matrix row data in order to store the connectivity matrix data.
 * @param Dim int
 * @param pMat SD_CON_MATRIX_DATA
 * @return int
*/
inline int AllocateConData( int Dim, SD_CON_MATRIX_DATA *pMat )
{
	pMat->nRow = Dim;
	GD_MALLOC( pMat->pRow, SD_ROW_DATA, pMat->nRow, "Row Allocation");
	memset( pMat->pRow, 0, sizeof(SD_ROW_DATA)*pMat->nRow );
	if ( gd_MemErr ) {
		ERROR_SOLVER("Memory Error");
	}

	return 0;

}  // AllocateConData

int ReleaseConMatrix( SD_CON_MATRIX_DATA *pMat )
{
	GD_FREE(pMat->pPerm);
	GD_FREE(pMat->pSupernode);
	GD_FREE(pMat->pPermInv);
	GD_FREE(pMat->pRow);
	SD_DESTROY_CHUNK(pMat->PoolCol);

	return 0;

}  // ReleaseConMatrix

/*
 * INTERFACE FUNCITONS TO ACCESS THE SOLVER
 */
int ds_Initialize(const int& MatDim, SD_MATRIX_DATA **ppMat)
{
	SD_MATRIX_DATA  *pMat = NULL;

	GD_MALLOC( pMat, SD_MATRIX_DATA, 1, "Matrix Data");
	memset( pMat,0,sizeof(SD_MATRIX_DATA) );
	pMat->nEq = MatDim;
	if ( AllocateConData( MatDim, &pMat->Mat.Con ) )
		 return 1;

	pMat->State = ConMatrix;

	*ppMat = pMat;

	return 0;

}  /* ds_Initialize */

/*
* This function compute the triangular factorization for a block of rows of dimension N_PIVOT
* onto another block of rows of dimension N_ROW for a block symmetric matrix stored packed
* row-wise in a one dimensional array. Schenatically we have:
*
*    N_PIVOT            N_ROW                       N[1]            N[2]
*     <--->           <-------->                 <-------->         <-->
*     X X X  -  -  -  *  *  *  *  *  *  *        *  *  *  *         *  *
*     X X X  -  -  -  *  *  *  *  *  *  *        *  *  *  *         *  *
*     X X X  -  -  -  *  *  *  *  *  *  *        *  *  *  *         *  *
*     <--------------><----------------->
*        TOT_ROW            N_COL         JUMP[1]           JUMP[2]
*                                        <------>          <------->
*                     *  *  *  *  *  *  *  -  -  *  *  *  *  -   -  *  *
*                        *  *  *  *  *  *  -  -  *  *  *  *  -   -  *  *
*                           *  *  *  *  *  -  -  *  *  *  *  -   -  *  *
*                              *  *  *  *  -  -  *  *  *  *  -   -  *  *
*
* The first values of both row block are specified by MAT0 and MAT1, N[0] and JUMP[0] are
* unused. ATTENTION: This function change the value of N[0] which is first set to N_COL and then
* is changed continously.
*/
inline void FACT_SYM_MAT_BLOCK (int N_PIVOT, int TOT_ROW, int N_ROW, int N_COL, double *MAT0, int DIM0,
                         double *MAT1, int DIM1, int N_BLOCK, int *N, int *JUMP)
{
	int n_k, i_, k__;
	double *Mat_k0, *Mat_k, *Mat_i;
	for ( Mat_k0 = MAT0, k__ = 0, n_k = N_PIVOT; n_k > 0; n_k--, k__++ ) {
		const double Pivot = 1. / (*Mat_k0);
		Mat_k = Mat_k0 + TOT_ROW - k__;
		Mat_i = MAT1;
		int dim_i = DIM1;
		N[0]  = N_COL;
		for ( i_ = N_ROW; i_ > 0; Mat_k++, i_-- ) {
			VD_AXPY_JUMP(N_BLOCK, N, JUMP, -(*Mat_k)*Pivot, Mat_k, Mat_i );
			N[0]-- ;
			Mat_i += (dim_i)--;
		}
		Mat_k0  += DIM0 - k__;
	}
}

/**
* @brief This function compute the permutation array and thus run the mmd algorithm.
 * @param pMat SD_CON_MATRIX_DATA
 * @return int
*/
inline int ComputePermutation( SD_CON_MATRIX_DATA *pMat )
{
	int *head;     /* array 0..maxN */
	int *list;     /* array 0..maxN */
	int *marker;   /* array 0..maxN */
	int *xadj;     /* array 0..maxN */
	int *adjncy;   /* array 0..maxCol */
	static const int maxint = 32000;   /* use a better value */
	int ncsub;
	int delta;
	int nEq;

	// Allocate memory requested by the mmd algorithm
	nEq = pMat->nRow;

	GD_MALLOC( pMat->pSupernode, int, nEq , "supernode size vector");
	GD_MALLOC( pMat->pPerm,      int, nEq , "permutation vector");
	GD_MALLOC( pMat->pPermInv,   int, nEq , "inverse permutation vector");
	GD_MALLOC( head,             int, nEq*4 + 1 + pMat->nCol, "mmd tmp memory");
	list   = head + 1*nEq;
	marker = head + 2*nEq;
	xadj   = head + 3*nEq;
	adjncy = head + 4*nEq + 1;

	/*
	* Run the mmd algorithm. The mmd implementation is a translation of a FORTRAN version so
	* that we pass all array pointers decremented by one.
	*/
	delta = 0;   // Is this a good value?

	BuildSparseConFormat(pMat, xadj, adjncy);

	RunMmd(pMat->nRow, xadj-1, adjncy-1, pMat->pPerm-1, pMat->pPermInv-1, delta,
		head-1, pMat->pSupernode-1, &pMat->nSupernode, list-1, marker-1, maxint, &ncsub );

	// Release temporary data used only by mmd
	GD_FREE(head);


	return 0;

}  // ComputePermutation

inline int SymbolicFact(SD_MATRIX_DATA *pMat)
{
	SD_TMP_CON_MATRIX_DATA  TmpConMat;
	SD_BLOCK_MATRIX_DATA    BlockMat;

	ComputePermutation( &pMat->Mat.Con);
	ComputeTmpConMatrix(&pMat->Mat.Con, &TmpConMat);
	ComputeFillIn(&TmpConMat);
	ComputeBlockMatrix(&TmpConMat, &BlockMat);
	pMat->State     = BlockMatrix;
	pMat->Mat.Block = BlockMat;

	return 0;

}  // SymbolicFact

bool ds_Solve(const SD_MATRIX_WHAT& Code, SD_MATRIX_DATA *pMat, double *X)
{
  bool success = true;

	// SymbolicFactorize
	if ( Code & SymbolicFactorize ){
		if ( Code & NumericFactorize ){
			USER_ERROR("You cannot invert the matrix symbolically and numerically contemporary");
		}

		if ( pMat->State != ConMatrix ){
			USER_ERROR("Bad Matrix Format for Symbolic Factorization");
		}

		SymbolicFact(pMat);
	}

	// NumericFactoriz
	if ( Code & NumericFactorize ){
		if (  pMat->State != BlockMatrix ){
			USER_ERROR("Bad Matrix Format for Numerical Factorization");
		}
		InvertMatrix( &pMat->Mat.Block );
	}

	// BackForwardSubst
	if ( Code & BackForwardSubst ){
		int DimTot, i;
		if (  pMat->State != BlockMatrix ){
			USER_ERROR("Bad Matrix Format for Back- For-ward Substitution");
		}
		DimTot = pMat->Mat.Block.Dim + pMat->nDeletedEq;

		Permute( DimTot, pMat->Mat.Block.pPerm, X );
		InverseMatrixVector( &pMat->Mat.Block, X );
		for(i=pMat->Mat.Block.Dim; i<DimTot; i++){
			X[i] = 0.;
		}
		Permute( DimTot, pMat->Mat.Block.pPerm, X );

    // Check for NaN
    success = !std::isnan(X[0]);

	}

	// ResetMatrixData
   	if ( Code & ResetMatrixData ){
		if ( Code != ResetMatrixData ){
			USER_ERROR("You cannot reset the matrix together with other operations");
		}

		if ( pMat->State != BlockMatrix ){
			USER_ERROR("Bad Matrix Format to reset matrix");
		}

		memset( pMat->Mat.Block.pUpper, 0, pMat->Mat.Block.SizeUpper * sizeof(double) );
	}

   	// ReleaseMatrixData
   	if ( Code & ReleaseMatrixData ){
		if ( pMat->State == ConMatrix ){
			ReleaseConMatrix(&pMat->Mat.Con);
		} else if ( pMat->State == BlockMatrix  ){
			ReleaseBlockMatrix(&pMat->Mat.Block);
		} else ERROR_SOLVER("Unknown matrix state");{
			GD_FREE(pMat);
		}
	}

  return success;

}  /* ds_Solve */

/**
 * @brief This function assemble the element matrix for one element and must be called for each
 * (finite) element after the element connectivity have been assembled and the matrix symbolic
 * factorized. To perform this task we also newly require the element incidences. The
 * variable: Dim specifies the dimension of the matrix: Mat which is not required to be equal
 * to the numer of element incidences: nEq.
 * ATTENTION: This function do not generate a run time error if the specified incidences have
 * not been previously defined.
 * NOTE: If the matrix has been specified as symmetric we always use only the upper part of
 * the element matrix.
 * @param [in] pMat0 SD_MATRIX_DATA
 * @param [in] nEq int
 * @param [in] Eq int
 * @param [in] Dim int
 * @param [in] ElMat double
 * @return int
 */
int ds_AssembleMatrix(SD_MATRIX_DATA *pMat0, const int& nEq, int Eq[], const int& Dim, const double *ElMat)
{
	SD_BLOCK_MATRIX_DATA *pMat=NULL;
	SD_ROW_BLOCK_DATA *pRow=NULL;
	int Row, Col, PermRow, PermCol, Found, Index;

	pMat = &pMat0->Mat.Block;

	for (Row = 0; Row < nEq; Row++) {
		PermRow = pMat->pPerm[ Eq[Row] ];
		SEARCH_ROW(PermRow, FIRST_BLOCK_ROW(pMat), LAST_BLOCK_ROW(pMat), pRow);
		for (Col = 0; Col < nEq; Col++) {
			PermCol = pMat->pPerm[ Eq[Col] ];
			if ( PermCol < PermRow ) {
				continue;
			}
			SEARCH_COL(PermCol, PermRow, pMat, pRow, Found, Index);
			if ( Found ) {
				if ( Row<Col ) {
					pMat->pUpper[Index] += ElMat[ Row*Dim + Col ];
				} else {
					pMat->pUpper[Index] += ElMat[ Col*Dim + Row ];
				}
			}
		}
	}
	return 0;

}  /* ds_AssembleMatrix */


/**
 * @brief This function permute a vector, for a given permutation vector and compute the inverse
 * permutation vector. Of course this is a trivial task if we have a second vector to store
 * the new permuted vector, but we do not want to allocated extra memory so that we use a
 * somewhat expensive algorithm which do not yet require a second storage vector. Here we
 * assume that the dimension of permutation array is less than 2**31, because we use the 31th
 * bit of each index of the permutation vector to store a flag, which tells us that we have
 * already permuted that element. The algorithm is very simple, we take one element, look if
 * we have already permuted that element, and if not we permute all elements connected to this
 * element by a permutation cycle i.e. we shift right by one all elements of that cycle.
 * ATTENTION: This function not only permute the given vector, but also compute the inverse
 * permutation vector which is stored at the place of the permutation vector.
 * @param N int
 * @param Perm int
 * @param Vector double
 * @return int
 */
int Permute(int N, int * Perm, double * Vector)
{
	int   i, To, From, ToNext;
	double ValueTo, Value;

	for (i = 0; i < N;  Perm[i++] &= (~SD_MARKED) ) {
		if ( Perm[i] & SD_MARKED ) {
			continue;
		}
		ValueTo = Vector[i];
		From = i;
		To = Perm[i];
		while (1) {  // follows the cycle, until we find an already permuted element
			Value      = Vector[To];
			Vector[To] = ValueTo;
			ToNext     = Perm[To];
			Perm  [To] = From | SD_MARKED;
			if ( ToNext & SD_MARKED ) {
				break;
			}
			ValueTo    = Value;
			From       = To;
			To         = ToNext;
		}
	}
	return 0;

}  // Permute

/**
 * @brief This function is the kernel of the solution algorithm, and compute the LU triangular
 * factorization of a block matrix where the blocks are so defined that there is no more
 * fill-in during the factorizatio process. The LU factorization of the matrix substitute the
 * original matrix data. The LU factorization of a matrix is very similar to the inverse of
 * the matrix and from this fact we have derived the name for this function.
 * @param pMat SD_BLOCK_MATRIX_DATA
 * @return int
*/
int  InvertMatrix( SD_BLOCK_MATRIX_DATA *pMat )
{
	SD_ROW_BLOCK_DATA *pPivotRow, *pSearchRow, *pLastSearchRow;
	int nPivotRow,*pBlockJump, *pMatFirstColBlock, *pMatSizeColBlock;
	double *Upper;

	pMatFirstColBlock = pMat->pFirstColBlock;
	pMatSizeColBlock = pMat->pSizeColBlock  ;
	pLastSearchRow = pMat->pRowBlock + pMat->nRowBlock - 1;
	pBlockJump = pMat->pBlockJump;
	Upper = pMat->pUpper;

	/*
	* This is the pivot block loop. For each row block defined in the matrix which are
	* considered sequentially we set the selected row block to be the pivot block. If the
	* pivot block has a size which is greather than one we factorize the pivot block, note
	* this is always a dense matrix operation. Then for each other column block in the pivot
	* block we search for the corresponding row block in the list of all row blocks and
	* factorize the column-row block pair.
	*/
	for ( pPivotRow = pMat->pRowBlock, nPivotRow = pMat->nRowBlock; (nPivotRow--)>0; pPivotRow++) {
		int iColBlock, nColBlock, nCol, Row_i0, Row_i1, RowDelta, DimCol, DimCol0, TotRow, DimPivot, DimRow;
		double *PivotUpper, *RowUpper;
		pBLOCK pColBlock;

		// Initilize some data for this new pivot block
		pSearchRow  = pPivotRow;
		nColBlock   = pPivotRow->nColBlock;
		nCol        = pPivotRow->nCol;
		PivotUpper  = Upper + pPivotRow->iFloat;
		BLOCK_INIT(pColBlock, pC0_FIRST_COL(pPivotRow), pSIZE_FIRST_COL(pPivotRow) );

		// Factorize the Pivot row block, this is a dense matrix operation
		DimPivot = pPivotRow->Row1 - pPivotRow->Row0 + 1;
		FACT_SYM_MAT(PivotUpper, DimPivot, nCol);

		/*
		* Process each column block defined in the pivot row block. In this case for each
		* column block we have to found out the corresponding row block . This is an expensive
		* search operation and as been optimized. First we restrict to a minimum the search
		* interval, the lower bound is set to the last found row block and the upper bound is
		* set to the last one defined in the matrix. Before we start a the binary search for a
		* given col block we first check if the previous founded one, is still the right one.
		* For each column block is possible that we have to process more than one row blocks so
		* that this result in a double nested for loop.
		*/
		for ( iColBlock = nColBlock, TotRow    = DimPivot, Row_i0 = BLOCK_C0(pColBlock) + TotRow,
			Row_i1 = BLOCK_C1(pColBlock); iColBlock>0; BLOCK_NEXT(pColBlock),
			Row_i0 = BLOCK_C0(pColBlock), Row_i1 = BLOCK_C1(pColBlock), iColBlock-- ) {

			for ( ; Row_i0<Row_i1; TotRow += DimRow, Row_i0 += DimRow, BLOCK_SIZE(pColBlock) = DimCol0 ) {
				if ( Row_i0 > pSearchRow->Row1 ) {
					pSearchRow++;
					if ( Row_i0 > pSearchRow->Row1 ) {
						pSearchRow++;
						SEARCH_ROW(Row_i0, pSearchRow, pLastSearchRow, pSearchRow);
					}
				}

				RowDelta = Row_i0 - pSearchRow->Row0;
				DimCol   = Row_i1 - Row_i0;
				DimRow   = pSearchRow->Row1 - Row_i0 + 1;
				if ( DimCol < DimRow ) {
					DimRow = DimCol;
				}

				BLOCK_JUMP( iColBlock, pColBlock.pC0, pColBlock.pSize, pC0_FIRST_COL(pSearchRow),
					    pSIZE_FIRST_COL(pSearchRow), pBlockJump);
				pBlockJump[0] = 0 ;
				DimCol0       = BLOCK_SIZE(pColBlock); // save this value because it will change

				RowUpper = Upper + pSearchRow->iFloat + DIAGONAL(pSearchRow->nCol, RowDelta);
				FACT_SYM_MAT_BLOCK(DimPivot, TotRow, DimRow, DimCol, PivotUpper, nCol,
							RowUpper,   pSearchRow->nCol-RowDelta,
      							iColBlock,  pColBlock.pSize, pBlockJump);
			}
		}
	}
	return 0;

} // InvertMatrix


/**
 * @brief This function perform the backward and forward substitution for a LU factorized block
 * matrix. Of course this operation is the same as multipying the inverse matrix with a vector
 * and inf fact the implementation does not much differs from a matrix vector multiplication.
 * @param pMat SD_BLOCK_MATRIX_DATA
 * @param X double
 * @return int
 */
int  InverseMatrixVector( SD_BLOCK_MATRIX_DATA *pMat, double *X )
{
	SD_ROW_BLOCK_DATA *pPivotRow;
	double *Upper;
	int nPivotRow, *pMatFirstColBlock, *pMatSizeColBlock, nRow;

	pMatFirstColBlock = pMat->pFirstColBlock;
	pMatSizeColBlock  = pMat->pSizeColBlock;
	Upper             = pMat->pUpper;

	// Forward substitution, solve the lower triangular system, where in the diagonal we have all ones.
	for (nRow = 0, pPivotRow = pMat->pRowBlock, nPivotRow = pMat->nRowBlock; (nPivotRow--)>0; pPivotRow++) {
		int         DimPivot, nCol, dim;
		double      *PivotUpper, Alpha;
		pBLOCK      pColBlock;

		DimPivot    = pPivotRow->Row1 - pPivotRow->Row0 + 1;
		nCol        = pPivotRow->nCol;
		PivotUpper  = Upper + pPivotRow->iFloat;
		BLOCK_INIT(pColBlock, pC0_FIRST_COL(pPivotRow), pSIZE_FIRST_COL(pPivotRow) );

		for (dim = 0; dim < DimPivot; dim++) {
			Alpha = -X[nRow++] / (*PivotUpper++);
			pColBlock.pSize[0]--;
			pColBlock.pC0[0]++;

			//printf("Alpha %f Mat0 %f Vec0 %f\n", Alpha, PivotUpper[0], X[pColBlock.pC0[0]]);
			VD_AXPY_POS(pPivotRow->nColBlock, pColBlock.pSize, pColBlock.pC0, Alpha, PivotUpper, X );
			PivotUpper +=  nCol - dim -1;

			// { int i; for(i=0; i<pMat->Dim; i++) printf("%f\n", X[i]); printf("\n");  }
		}
		pColBlock.pSize[0] += dim;
		pColBlock.pC0[0]   -= dim;
	}

	// Backward substitution, solve the upper triangular system
	for (nRow = pMat->Dim - 1, nPivotRow = pMat->nRowBlock,
		pPivotRow = pMat->pRowBlock + nPivotRow - 1; (nPivotRow--)>0; pPivotRow--) {
		int         DimPivot, nCol, dim;
		double      *PivotUpper, VectorDot;
		pBLOCK      pColBlock;

		DimPivot    = pPivotRow->Row1 - pPivotRow->Row0 + 1;
		nCol        = pPivotRow->nCol;
		PivotUpper  = Upper + pPivotRow->iFloat + DIAGONAL(nCol,DimPivot-1) + 1;
		BLOCK_INIT(pColBlock, pC0_FIRST_COL(pPivotRow), pSIZE_FIRST_COL(pPivotRow) );

		pColBlock.pC0[0]   += DimPivot;
		pColBlock.pSize[0] -= DimPivot;
		for(dim = DimPivot-1; dim >= 0; nRow--, dim--) {
			VD_DOT_POS(pPivotRow->nColBlock, pColBlock.pSize, pColBlock.pC0, PivotUpper, X, VectorDot );

			// printf("Dot %f Mat0 %f Vec0 %f\n", VectorDot, PivotUpper[0], X[pColBlock.pC0[0]]);

			pColBlock.pC0[0]--;
			pColBlock.pSize[0]++;
			X[nRow] -= VectorDot;
			PivotUpper--;
			X[nRow] /= PivotUpper[0];
			PivotUpper -=  nCol - dim ;

			//   { int i; for(i=0; i<pMat->Dim; i++) printf("%f\n", X[i]); printf("\n");  }
		}
	}

	return 0;

} // InverseMatrixVector

/*
 * MMD FUNCTIONS
 */

/**
* @brief MmdUpdate ---- multiple minimum degree update
* purpose -- this routine updates the degrees of nodes after a multiple elimination step.
* input parameters --
 * @param ehead -- int the beginning of the list of eliminated nodes (i.e., newly formed elements).
 * @param neqns -- int number of equations.
 * @param (xadj,adjncy) --int  adjacency structure.
 * @param delta -- int tolerance value for multiple elimination.
 * @param maxint -- int maximum machine representable (short) integer.
 * updated parameters --
 * @param mdeg -- int new minimum degree after degree update.
 * @param (head,forward,backward) -- int degree doubly linked structure.
 * @param qsize -- int size of supernode.
 * @param list,marker vector for degree update.
 * @param tag -- int tag value.
*/
static void MmdUpdate( int ehead,  int neqns,  int *xadj,  int *adjncy,  int delta,  int *mdeg,
                       int *head, int *forward,  int *backward, int *qsize, int *list,  int *marker,  int maxint,  int *tag)
{
	int deg, deg0, element, enode, fnode, i, iq2, istop,
	    istart, j, jstop, jstart, link, mdeg0, mtag, nabor,
	    node, q2head, qxhead;


	mdeg0 = *mdeg + delta;
	element = ehead;

n100:
	if ( element <= 0 ) {
		return;
	}

	// for each of the newly formed element, do the following.
	// reset tag value if necessary.
	mtag = *tag + mdeg0;
	if ( mtag >= maxint ) {
		*tag = 1;
		for ( i = 1; i <= neqns; i++ ) {
			if ( marker[i] < maxint ) {
				marker[i] = 0;
			}
		}
		mtag = *tag + mdeg0;
	}

	/*
	* create two linked lists from nodes associated with 'element':
	* one with two nabors (q2head) in the adjacency structure, and the
	* other with more than two nabors (qxhead). also compute 'deg0',
	* number of nodes in this element.
	*/
	q2head = 0;
	qxhead = 0;
	deg0   = 0;
	link   = element;

n400:
	istart = xadj[link];
	istop = xadj[link+1] - 1;
	for ( i = istart; i <= istop; i++ ) {
		enode = adjncy[i];
		link = -enode;
		if ( enode < 0 )  {
			goto n400;
		}
		if ( enode == 0 ) {
			break;
		}
		if ( qsize[enode] != 0 ) {
			deg0 += qsize[enode];
			marker[enode] = mtag;

			//'enode' requires a degree update
			if ( backward[enode] == 0 ) {
				// place either in qxhead or q2head list.
				if ( forward[enode] != 2 ) {
					list[enode] = qxhead;
					qxhead = enode;
				} else {
					list[enode] = q2head;
					q2head = enode;
				}
			}
		}
	}

	// for each node in q2 list, do the following.
	enode = q2head;
	iq2   = 1;

n900:
	if ( enode <= 0 ) {
		goto n1500;
	}
	if ( backward[enode] != 0 ) {
		goto n2200;
	}
	(*tag)++;
	deg = deg0;

	// identify the other adjacent element nabor.
	istart = xadj[enode];
	nabor = adjncy[istart];
	if ( nabor == element ) {
		nabor = adjncy[istart+1];
	}
	link = nabor;
	if ( forward[nabor] >= 0 ) {
		// nabor is uneliminated, increase degree count.
		deg += qsize[nabor];
		goto n2100;
	}

	// the nabor is eliminated. for each node in the 2nd element
	// do the following.
n1000:
	istart = xadj[link];
	istop = xadj[link+1] - 1;
	for ( i = istart; i <= istop; i++ ) {
		node = adjncy[i];
		link = -node;
		if ( node != enode ) {
			if ( node < 0  ) {
				goto n1000;
			}
			if ( node == 0 ) {
				goto n2100;
			}
			if ( qsize[node] != 0 ) {
				if ( marker[node] < *tag ) {
					// 'node' is not yet considered.
					marker[node] = *tag;
					deg += qsize[node];
				} else {
					if ( backward[node] == 0 ) {
						if ( forward[node] == 2 ) {
							// 'node' is indistinguishable from 'enode'.
							// merge them into a new supernode.
							qsize[enode] += qsize[node];
							qsize[node] = 0;
							marker[node] = maxint;
							forward[node] = -enode;
							backward[node] = -maxint;
						} else {
							// 'node' is outmacthed by 'enode'
							if (backward[node]==0) {
								backward[node] = -maxint;
							}
						}
					}
				}
			}
		}
	}
	goto n2100;

n1500:
	// for each 'enode' in the 'qx' list, do the following.
	enode = qxhead;
	iq2 = 0;

n1600:
	if ( enode <= 0 )  {
		goto n2300;
	}
	if ( backward[enode] != 0 ) {
		goto n2200;
	}
	(*tag)++;
	deg = deg0;

	// for each unmarked nabor of 'enode', do the following.
	istart = xadj[enode];
	istop = xadj[enode+1] - 1;
	for ( i = istart; i <= istop; i++ ) {
		nabor = adjncy[i];
		if ( nabor == 0 ) {
			break;
		}
		if ( marker[nabor] < *tag ) {
			marker[nabor] = *tag;
			link = nabor;
			if ( forward[nabor] >= 0 ) {
				// if uneliminated, include it in deg count.
				deg += qsize[nabor];
			} else {
n1700:
				// if eliminated, include unmarked nodes in this
				// element into the degree count.
				jstart = xadj[link];
				jstop = xadj[link+1] - 1;
				for ( j = jstart; j <= jstop; j++ ) {
					node = adjncy[j];
					link = -node;
					if ( node < 0 ) {
						goto n1700;
					}
					if ( node == 0 ) {
						break;
					}
					if ( marker[node] < *tag ) {
						marker[node] = *tag;
						deg += qsize[node];
					}
				}
			}
		}
	}

n2100:
	// update external degree of 'enode' in degree structure,
	// and '*mdeg' if necessary.
	deg = deg - qsize[enode] + 1;
	fnode = head[deg];
	forward[enode] = fnode;
	backward[enode] = -deg;
	if ( fnode > 0 ) {
		backward[fnode] = enode;
	}
	head[deg] = enode;
	if ( deg < *mdeg ) {
		*mdeg = deg;
	}

n2200:
	// get next enode in current element.
	enode = list[enode];
	if ( iq2 == 1 ) {
		goto n900;
	}
	goto n1600;

n2300:
	// get next element in the list.
	*tag = mtag;
	element = list[element];
	goto n100;

}  // MmdUpdate


/**
* @brief MmdElimin -- multiple minimum degree elimination
* Purpose -- This routine eliminates the node mdeg_node of minimum degree from the adjacency
* structure, which is stored in the quotient graph format. It also transforms the quotient
* graph representation of the elimination graph.
* Input parameters --
 * @param mdeg_node -- node of minimum degree.
 * @param maxint -- estimate of maximum representable (short) integer.
 * @param tag -- tag value.
* Updated parameters --
 * @param (xadj,adjncy) -- updated adjacency structure.
 * @param (head,forward,backward) -- degree doubly linked structure.
 * @param qsize -- size of supernode.
 * @param marker -- marker vector.
 * @param list -- temporary linked list of eliminated nabors.
*/
static void MmdElimin(int mdeg_node, int *xadj, int *adjncy, int *head, int *forward, int *backward,
				  int *qsize, int *list, int *marker, int maxint, int tag)
{
	int element, i, istop, istart, j,
	    jstop, jstart, link,
	    nabor, node, npv, nqnbrs, nxnode,
	    pvnode, rlmt, rloc, rnode, xqnbr;


	// find the reachable set of 'mdeg_node' and
	// place it in the data structure.
	marker[mdeg_node] = tag;
	istart = xadj[mdeg_node];
	istop = xadj[mdeg_node+1] - 1;

	// 'element' points to the beginning of the list of
	// eliminated nabors of 'mdeg_node', and 'rloc' gives the
	// storage location for the next reachable node.
	element = 0;
	rloc = istart;
	rlmt = istop;
	for ( i = istart; i <= istop; i++ ) {
		nabor = adjncy[i];
		if ( nabor == 0 ) {
			break;
		}
		if ( marker[nabor] < tag ) {
			marker[nabor] = tag;
			if ( forward[nabor] < 0 ) {
				list[nabor] = element;
				element = nabor;
			} else {
				adjncy[rloc] = nabor;
				rloc++;
			}
		}
	}

	// merge with reachable nodes from generalized elements.
	while ( element > 0 ) {
		adjncy[rlmt] = -element;
		link = element;

		n400:
		jstart = xadj[link];
		jstop = xadj[link+1] - 1;
		for ( j = jstart; j <= jstop; j++ ) {
			node = adjncy[j];
			link = -node;
			if ( node < 0 ) {
				goto n400;
			}
			if ( node == 0 ) {
				break;
			}
			if ((marker[node]<tag)&&(forward[node]>=0)) {
				marker[node] = tag;
				//use storage from eliminated nodes if necessary.
				while ( rloc >= rlmt ) {
					link = -adjncy[rlmt];
					rloc = xadj[link];
					rlmt = xadj[link+1] - 1;
				}
				adjncy[rloc] = node;
				rloc++;
			}
		}
		element = list[element];
	}
	if ( rloc <= rlmt ) {
		adjncy[rloc] = 0;
	}
	// for each node in the reachable set, do the following.
	link = mdeg_node;

n1100:
	istart = xadj[link];
	istop = xadj[link+1] - 1;
	for ( i = istart; i <= istop; i++ ) {
		rnode = adjncy[i];
		link = -rnode;
		if ( rnode < 0 ) {
			goto n1100;
		}
		if ( rnode == 0 ) {
			return;
		}

		// 'rnode' is in the degree list structure.
		pvnode = backward[rnode];
		if (( pvnode != 0 ) && ( pvnode != (-maxint) )) {
			// then remove 'rnode' from the structure.
			nxnode = forward[rnode];
			if ( nxnode > 0 ) {
				backward[nxnode] = pvnode;
			}
			if ( pvnode > 0 ) {
				forward[pvnode] = nxnode;
			}
			npv = -pvnode;
			if ( pvnode < 0 ) {
				head[npv] = nxnode;
			}
		}

		// purge inactive quotient nabors of 'rnode'.
		jstart = xadj[rnode];
		jstop = xadj[rnode+1] - 1;
		xqnbr = jstart;
		for ( j = jstart; j <= jstop; j++ ) {
			nabor = adjncy[j];
			if ( nabor == 0 ) {
				break;
			}
			if ( marker[nabor] < tag ) {
				adjncy[xqnbr] = nabor;
				xqnbr++;
			}
		}

		// no active nabor after the purging.
		nqnbrs = xqnbr - jstart;
		if ( nqnbrs <= 0 ) {
			// merge 'rnode' with 'mdeg_node'.
			qsize[mdeg_node] += qsize[rnode];
			qsize[rnode] = 0;
			marker[rnode] = maxint;
			forward[rnode] = -mdeg_node;
			backward[rnode] = -maxint;
		} else {
			// flag 'rnode' for degree update, and
			// add 'mdeg_node' as a nabor of 'rnode'.
			forward[rnode] = nqnbrs + 1;
			backward[rnode] = 0;
			adjncy[xqnbr] = mdeg_node;
			xqnbr++;
			if ( xqnbr <= jstop ) {
				adjncy[xqnbr] = 0;
			}
		}
	}
	return;

} // MmdElimin

/**
* @brief MmdInit -- mult minimum degree initialization
* purpose -- this routine performs initialization for the multiple elimination version of the
*   minimum degree algorithm.
* input parameters --
 * @param neqns -- number of equations.
 * @param xadj -- adjacency structure.
* output parameters --
 * @param (head,forward,backward) -- degree doubly linked structure.
 * @param qsize -- size of supernode ( initialized to one).
 * @param list -- linked list.
 * @param marker -- marker vector.
*/
static int  MmdInit(int neqns, int *xadj, int *head, int *forward, int *backward, int *qsize, int *list, int *marker)
{
	int  fnode, ndeg, node;

	for ( node = 1; node <= neqns; node++ ) {
		head[node]   = 0;
		qsize[node]  = 1;
		marker[node] = 0;
		list[node]   = 0;
	}

	// initialize the degree doubly linked lists.
	for ( node = 1; node <= neqns; node++ ) {
		ndeg = xadj[node+1] - xadj[node] + 1;
		fnode = head[ndeg];
		forward[node] = fnode;
		head[ndeg] = node;
		if ( fnode > 0 ) {
			backward[fnode] = node;
		}
		backward[node] = -ndeg;
	}
	return 0;

} // MmdInit

/**
* @brief MmdNumbering -- multi minimum degree numbering
* purpose -- this routine performs the final step in producing the permutation and inverse
* permutation vectors in the multiple elimination version of the minimum degree ordering
* algorithm.
* input parameters --
* @param neqns -- number of equations.
* @param qsize -- size of supernodes at elimination.
* updated parameters --
* @param invp -- inverse permutation vector. on input, if qsize[node] = 0, then node has been
* output parameters --
* @param perm -- the permutation vector.
* @param nsize -- number of supernodes
*/
static void  MmdNumbering(int neqns, int *perm, int *invp, int *qsize, int *nsize)
{
	int father, nextf, node, nqsize, num, root;

	for ( *nsize=0, node = 1; node <= neqns; node++ ) {
		nqsize = qsize[node];
		if ( nqsize <= 0 ) {
			perm[node] = invp[node];
		} else {
			perm[node] = -invp[node];
			(*nsize)++;
		}
	}

	// for each node which has been merged, do the following.
	for ( node = 1; node <= neqns; node++ ) {
		if ( perm[node] <= 0 ) {
			// trace the merged tree until one which has not
			// been merged, call it root.
			father = node;
			while ( perm[father] <= 0 ) {
				father = - perm[father];
			}

			// number node after root.
			root = father;
			num = perm[root] + 1;
			invp[node] = -num;
			perm[root] = num;

			// shorten the merged tree.
			father = node;
			nextf = - perm[father];
			while ( nextf > 0 ) {
				perm[father] = -root;
				father = nextf;
				nextf = -perm[father];
			}
		}
	}

	// ready to compute perm. USE C NOTATION !!! MODIFIED GS
	for ( node = 1; node <= neqns; node++ ) {
		num        = -invp[node];
		invp[node] = num  - 1;
		perm[num ] = node - 1;
	}
	return;

}  // MmdNumbering

/**
* @brief RunMmd -- multiple minimum external degree
* purpose -- this routine implements the minimum degree algorithm. it makes use of the
* implicit representation of elimination graphs by quotient graphs, and the notion of
* indistinguishable nodes. It also implements the modifications by multiple elimination and
* minimum external degree. Caution -- the adjacency vector adjncy will be destroyed.
* Input parameters --
 *  @param neqns -- number of equations.
 *  @param (xadj, adjncy) -- the adjacency structure.
 *  @param delta -- tolerance value for multiple elimination.
 *  @param maxint -- maximum machine representable (short) integer (any smaller estimate will do)
*   for marking nodes.
* Output parameters --
 *  @param nsize -- number of supernodes.
 *  @param perm -- the minimum degree ordering.
 *  @param invp -- the inverse of perm.
 *  @param ncsub -- an upper bound on the number of nonzero subscripts for the compressed storage scheme.
*  Working parameters --
 *  @param head -- vector for head of degree lists.
 *  @param invp -- used temporarily for degree forward link.
 *  @param perm -- used temporarily for degree backward link.
 *  @param qsize -- vector for size of supernodes.
 *  @param list -- vector for temporary linked lists.
 *  @param marker -- a temporary marker vector.
* Subroutines used -- MmdElimin, MmdInit, MmdNumbering, MmdUpdate.
*/
static void  RunMmd(int neqns, int *xadj, int *adjncy, int *invp, int *perm, int delta, int *head, int *qsize, int *nsize, int *list, int *marker, int maxint, int *ncsub)
{
	int  ehead, i, mdeg, mdlmt, mdeg_node, nextmd, num, tag;
	if ( neqns <= 0 ) {
		return;
	}

	// initialization for the minimum degree algorithm.
	*ncsub = 0;
	MmdInit( neqns, xadj, head, invp, perm, qsize, list, marker );

	//  'num' counts the number of ordered nodes plus 1.
	num = 1;

	// eliminate all isolated nodes.
	nextmd = head[1];
	while  ( nextmd > 0 ) {
		mdeg_node = nextmd;
		nextmd = invp[mdeg_node];
		marker[mdeg_node] = maxint;
		invp[mdeg_node] = -num;
		num = num + 1;
	}

	// search for node of the minimum degree. 'mdeg' is the current
	// minimum degree; 'tag' is used to facilitate marking nodes.
	if ( num > neqns ) {
		goto n1000;
	}
	tag = 1;
	head[1] = 0;
	mdeg = 2;

	// infinite loop here !
	while ( 1 ) {
		while ( head[mdeg] <= 0 ) {
			mdeg++;
		}

		// use value of 'delta' to set up 'mdlmt', which governs
		// when a degree update is to be performed.
		mdlmt = mdeg + delta;
		ehead = 0;

n500:
		mdeg_node = head[mdeg];
		while ( mdeg_node <= 0 ) {
			mdeg++;
			if ( mdeg > mdlmt ) {
				goto n900;
			}
			mdeg_node = head[mdeg];
		}

		//  remove 'mdeg_node' from the degree structure.
		nextmd = invp[mdeg_node];
		head[mdeg] = nextmd;
		if ( nextmd > 0 ) {
			perm[nextmd] = -mdeg;
		}
		invp[mdeg_node] = -num;
		*ncsub += mdeg + qsize[mdeg_node] - 2;
		if ( (num+qsize[mdeg_node]) > neqns ) {
			goto n1000;
		}

		//  eliminate 'mdeg_node' and perform quotient graph
		//  transformation. reset 'tag' value if necessary.
		tag++;
		if ( tag >= maxint ) {
			tag = 1;
			for ( i = 1; i <= neqns; i++ ) {
				if ( marker[i] < maxint ) {
					marker[i] = 0;
				}
			}
		}
		MmdElimin( mdeg_node, xadj, adjncy, head, invp, perm, qsize, list, marker, maxint, tag );
		num += qsize[mdeg_node];
		list[mdeg_node] = ehead;
		ehead = mdeg_node;
		if ( delta >= 0 ) {
			goto n500;
		}

n900:
		// update degrees of the nodes involved in the
		// minimum degree nodes elimination.
		if ( num > neqns ) {
			goto n1000;
		}
		MmdUpdate( ehead, neqns, xadj, adjncy, delta, &mdeg, head, invp, perm, qsize, list, marker, maxint, &tag );

	}

n1000:

	MmdNumbering ( neqns, perm, invp, qsize, nsize );

} // RunMmd

/**
* @brief This function build a compressed sparse matrix format which is the common used one. The
* value pRowStart[Row] specifies the offset in the array pColumn[] where the column
* coefficients for the row: Row are starting to be defined. The column coefficients for a row
* are always sorted. Of course the array pRowStart has a dimension of plus one with respect
* to the matrix dimension. ATTENTION: Here we use a FORTRAN notation, all values in the
* arrays pRowStart and pColumn are incremented by one.
 * @param pMat SD_CON_MATRIX_DATA
 * @param pRowStart0 int
 * @param pColumn0 int
 * @return int
*/
int BuildSparseConFormat(SD_CON_MATRIX_DATA *pMat, int *pRowStart0, int *pColumn0)
{
	 int          i, *pRowStart, *pColumn;
	SD_ROW_DATA *pRow;


	for (i = pMat->nRow, pRow = pMat->pRow, pRowStart = pRowStart0,
	      pColumn = pColumn0, *pRowStart =1; i>0; i--,pRow++, pRowStart++) {
		int nCol;
		SD_COL_DATA *pCol;
		nCol = 0;
		pCol = pRow->Col;
		while ( pCol ) {
			*pColumn++ = SD_COL(pCol)+1;
			pCol = pCol->Next; nCol++;
		}
		pRowStart[1] = pRowStart[0] + nCol;
	}


	return 0;

}  // BuildSparseConFormat

/**
* @brief This routine set up a temporary adjacency block matrix format for the purpose to speed-up
* the symbolic factorization process and is called after the permutation vector has been
* computed. From this step all indices for rows and columns are permuted indices. The row
* blocks are a result of the mmd algorithm and so need not to be computed.
* Here we have to compute the column blocks for the upper matrix including the diagonal. Of
* course the matrix is still sparse because at this time the fill-in is still unknown but it
* is however possible that blocks with size larger than one will appear.
* NOTE: All data allocated in pMat0 is released.
 * @param pMat0 SD_CON_MATRIX_DATA
 * @param pMat SD_TMP_CON_MATRIX_DATA
 * @return int
*/
int ComputeTmpConMatrix(SD_CON_MATRIX_DATA *pMat0, SD_TMP_CON_MATRIX_DATA *pMat)
{
	SD_TMP_ROW_BLOCK_DATA *pRowBlock;
	SD_ROW_DATA           *pRow;
	int                   *pPerm, *pPermInv, *pSupernode;

	if ( sizeof(SD_ROW_BLOCK_DATA)     != sizeof(SD_TMP_ROW_BLOCK_DATA ) ||
		sizeof(SD_TMP_ROW_BLOCK_DATA) >  sizeof(SD_ROW_BLOCK_DATA)         ) {
		EXIT("DATA STRUCTURE INCOMPATIBILITY");
	}

	// First allocate definetively the matrix row block data
	memset( pMat, 0, sizeof(SD_TMP_CON_MATRIX_DATA) );
	pMat->nRowBlock = pMat0->nSupernode;
	GD_MALLOC( pRowBlock, SD_TMP_ROW_BLOCK_DATA, pMat->nRowBlock, "Row Block Allocation");
	pMat->nRow      = pMat0->nRow;
	pMat->pPerm     = pMat0->pPerm;
	pMat->pRowBlock = pRowBlock;

	pSupernode      = pMat0->pSupernode;
	pPerm           = pMat0->pPerm;
	pPermInv        = pMat0->pPermInv;
	pRow            = pMat0->pRow;

	/*
	* Set the temporary adjacency block data. For each row block process each single column
	* coefficients and the diagonal one.
	*/
	for (int PermRow = 0; PermRow < pMat->nRow; PermRow++) {
		int Supernode, Row, Found;
		SD_COL_BLOCK_DATA **ppColBlock, *pColBlock, *pFreeColBlock, *pStartColBlock;

		Row                  = pPermInv  [PermRow];
		Supernode            = pSupernode[Row];
		if ( Supernode <= 0 ) {
			continue;
		}
		pRowBlock->Data.Row0  = PermRow;
		pRowBlock->Data.Row1  = PermRow + Supernode - 1;

		// Insert the diagonal block as first block
		SD_GET_COL_BLOCK(pColBlock, pMat);
		pColBlock->Next = 0;
		pColBlock->Col0 =  pColBlock->Col1 = PermRow;
		pRowBlock->Data.ColBlock = pColBlock;

		/*
		* Insert the other column blocks. Foreach defined column coefficient first discard all
		* column coefficients lower the diagonal. Then check if the last defined column block
		* can be used as first one to start the linear search.
		* ATTENTION: It is possible that if the block is found and the upper bound is
		* incremented by one that the updated block will be adjacent to the next column blocks.
		* This condition is checked and if acknolegded one block (the second one) is removed.
		* NOTE: We have not to correctly initilize the value of ppColBlock to the true first
		* pointer of the list of clumn block because if a block is not found this will never be
		* the first one.
		*/
		for ( SD_COL_DATA *pCol = pRow[Row].Col; pCol; pCol = pCol->Next ) {
			const int PermCol = pPerm[ SD_COL(pCol) ];
			if ( PermRow > PermCol ) {
				continue;
			}
			if ( PermCol < pColBlock->Col0  ) {
				pStartColBlock = pRowBlock->Data.ColBlock;
			} else {
				pStartColBlock = pColBlock;
			}
			ppColBlock = &pStartColBlock;
			FIND_COL_BLOCK(pStartColBlock, PermCol, ppColBlock, Found);
			if ( !Found ) {
				SD_GET_COL_BLOCK(pColBlock, pMat);
				pColBlock->Next = *ppColBlock;
				*ppColBlock     = pColBlock;
				pColBlock->Col0 = pColBlock->Col1 = PermCol;
			} else {
				pColBlock     = (*ppColBlock);
				pFreeColBlock = pColBlock->Next;
				if ( pFreeColBlock && ( pColBlock->Col1 +1 ) >= pFreeColBlock->Col0 ) {
					pColBlock->Col1 = pFreeColBlock->Col1;
					pColBlock->Next = pFreeColBlock->Next;
					SD_FREE_COL_BLOCK(pFreeColBlock, pMat);
				}
			}
		}

		// Process next row block
		pRowBlock++;
	}

	// Release all the previously used adjaceny matrix data
	GD_FREE(pMat0->pRow);
	GD_FREE(pMat0->pPermInv);
	GD_FREE(pMat0->pSupernode);
	SD_DESTROY_CHUNK(pMat0->PoolCol);

	return 0;

}  // ComputeTmpConMatrix


inline void MERGE_COL_BLOCK(SD_COL_BLOCK_DATA *pCOL0, SD_COL_BLOCK_DATA **ppCOL1, SD_TMP_CON_MATRIX_DATA *pMAT)
{
	SD_COL_BLOCK_DATA  *pUp_, *pLo_, **ppLo_, *pC_;

	for ( ppLo_ =  ppCOL1, pLo_  = *ppLo_, pUp_ = pCOL0;  pUp_ ; pUp_=pUp_->Next ) {
		// skeep all lower blocks not intersecting and ahead of the upper block
		while ( pLo_ && ( pLo_->Col1 + 1 < pUp_->Col0 ) ){
				ppLo_= &pLo_->Next;
				pLo_ = pLo_->Next;
		}

		/*
		* check for a nil lower block or a lower block non intersecting and beyond the upper one.
		* If acknowledged insert a new lower block equal to the upper one and continue with the
		* next upper block
		*/
		if ( !pLo_ || pLo_->Col0 > pUp_->Col1 + 1 ) {
			SD_GET_COL_BLOCK(pC_, pMAT);
			pC_->Col0 = pUp_->Col0 ;
			pC_->Col1 = pUp_->Col1 ;
			pC_->Next = pLo_;
			*ppLo_    = pC_;
			ppLo_     = &pC_->Next;
			continue;
		}

		// the lower and upper block are now intersecting, take the larger bounds for the lower block
		if ( pLo_->Col0 > pUp_->Col0 ) {
			pLo_->Col0 = pUp_->Col0;
		}
		if ( pLo_->Col1 < pUp_->Col1 ) {
			pLo_->Col1 = pUp_->Col1;
		}

		// free all next lower blocks if fully included in the upper one
		while ( pC_ = pLo_->Next, pC_ && ( pC_->Col1 <= pUp_->Col1 ) ) {
			pLo_->Next = pC_->Next;
			SD_FREE_COL_BLOCK(pC_, pMAT);
		}
		// free the next lower block if partially included or adjacent to the upper one
		if ( pC_ = pLo_->Next, pC_ && ( pC_->Col0 - 1 <= pUp_->Col1 ) ) {
			pLo_->Next = pC_->Next;
			pLo_->Col1 = pC_->Col1;
			SD_FREE_COL_BLOCK(pC_, pMAT);
		}
	}
}

/**
* @brief This function compute the fill-in by factorizing symbolically the matrix.
 * @param pMat SD_TMP_CON_MATRIX_DATA
 * @return int
*/
int  ComputeFillIn(SD_TMP_CON_MATRIX_DATA *pMat)
{
	SD_ROW_BLOCK_DATA *pPivotRow, *pSearchRow, *pLastSearchRow;
	int               nPivotRow;

	#define pFIRST_COL_BLOCK(pROW_BLOCK)  ( ( (SD_TMP_ROW_BLOCK_DATA *)pROW_BLOCK)->Data.ColBlock )

	/*
	* This is the pivot block loop. For each row block defined in the matrix which are
	* considered sequentially we set the selected row block to be the pivot block.
	*/
	pPivotRow      = ( SD_ROW_BLOCK_DATA *) pMat->pRowBlock;
	pLastSearchRow = pPivotRow + pMat->nRowBlock - 1;

	for ( nPivotRow = pMat->nRowBlock; (nPivotRow--)>0; pPivotRow++) {
		int                Row_i0, Row_i1, DimRow, Col0;
		SD_COL_BLOCK_DATA  *pColBlock;

		/*
		* Process each column block defined in the pivot row block.In this case for each column
		* block we have to found out the corresponding row block. This is an expensive search
		* operation and has been optimized. First we restrict to a minimum the search interval,
		* the lower bound is set to the last found row block and the upper bound is set to the
		* last one defined in the matrix. Before we start the binary search for a given column
		* block we first check if the previous founded one, is still the right one. For each
		* column block is possible that we have to process more than one row blocks so that
		* this result in a double nested for loop.
		*/
		pSearchRow = pPivotRow;
		pColBlock  = pFIRST_COL_BLOCK(pPivotRow); /* the first pColBlock is never nil !!! */
		Row_i0     = pColBlock->Col0;
		Row_i1     = pColBlock->Col1;
		DimRow     = pPivotRow->Row1 - pPivotRow->Row0 + 1;
		goto  SkeepFirstMergeOperation;

		do {
			Row_i0  = pColBlock->Col0;
			Row_i1  = pColBlock->Col1;
			for ( ; Row_i0<=Row_i1; Row_i0 += DimRow ) {
				if ( Row_i0 > pSearchRow->Row1 ) {
					pSearchRow++;
					if ( Row_i0 > pSearchRow->Row1 ) {
						pSearchRow++;
						SD_SEARCH_BLOCK_ROW(Row_i0, pSearchRow, pLastSearchRow, pSearchRow);
					}
				}
				DimRow = pSearchRow->Row1 - Row_i0 + 1;

				Col0 = pFIRST_COL_BLOCK(pSearchRow)->Col0;
				MERGE_COL_BLOCK(pColBlock, &pFIRST_COL_BLOCK(pSearchRow), pMat);
				pFIRST_COL_BLOCK(pSearchRow)->Col0 = Col0;

				SkeepFirstMergeOperation:;
			}
		} while ( pColBlock = pColBlock->Next, pColBlock );
	}

	return 0;

} // ComputeFillIn

/**
* @brief Here, we define the matrix as used by the numerical factorization algorithm. If the
* multiplicity factor is not 1 at this time we define the true row and column blocks.
 * @param pTmpMat SD_TMP_CON_MATRIX_DATA
 * @param pMat SD_BLOCK_MATRIX_DATA
 * @return int
*/
int ComputeBlockMatrix( SD_TMP_CON_MATRIX_DATA *pTmpMat, SD_BLOCK_MATRIX_DATA *pMat)
{
	SD_TMP_ROW_BLOCK_DATA *pTmpRowBlock;
	SD_ROW_BLOCK_DATA     *pRowBlock;
	SD_COL_BLOCK_DATA     *pColBlock;
	int                    k, nTotCol, nTotColBlock, MaxColBlock, *pFirstColBlock, *pSizeColBlock;

	memset( pMat, 0, sizeof(SD_BLOCK_MATRIX_DATA) );
	pMat->Dim        = pTmpMat->nRow;
	pMat->pPerm      = pTmpMat->pPerm;
	pMat->nRowBlock  = pTmpMat->nRowBlock;
	pMat->nColBlock  = pTmpMat->nColBlock;
	pMat->pRowBlock  = (SD_ROW_BLOCK_DATA*)pTmpMat->pRowBlock;

	/*
	* Alloc memory to store new column block data. Traverse all the temporary column blocks
	* and store the data newly in compact form.
	*/
	GD_MALLOC( pMat->pFirstColBlock, int, pMat->nColBlock, "Matrix First Col. Block" );
	GD_MALLOC( pMat->pSizeColBlock,  int, pMat->nColBlock, "Matrix Size  Col. Block" );
	if ( gd_MemErr )
		return 1;

	for (k = pMat->nRowBlock, pFirstColBlock = pMat->pFirstColBlock, pSizeColBlock = pMat->pSizeColBlock, nTotCol = 0,
		nTotColBlock = 0, MaxColBlock = 0, pTmpRowBlock = pTmpMat->pRowBlock; k>0; k--, pTmpRowBlock++ )
	{
		int Delta, nCol, nColBlock;

		for ( pColBlock = pTmpRowBlock->Data.ColBlock, nCol = 0, nColBlock = 0; pColBlock;
				pColBlock = pColBlock->Next, nColBlock++, pFirstColBlock++, pSizeColBlock++ ) {
			pFirstColBlock[0] = pColBlock->Col0;
			pSizeColBlock [0] = ( pColBlock->Col1 - pColBlock->Col0 + 1 );
			nCol             += pSizeColBlock[0];
		}

		pRowBlock = (SD_ROW_BLOCK_DATA*)pTmpRowBlock;
		pRowBlock->nCol      = nCol;
		pRowBlock->nColBlock = nColBlock;
		pRowBlock->iColBlock = nTotColBlock;
		pRowBlock->iFloat    = nTotCol;
		if ( MaxColBlock < nColBlock ) {
			MaxColBlock = nColBlock;
		}
		Delta            = ( pRowBlock->Row1 - pRowBlock->Row0 + 1 );
		pRowBlock->Row1  = ( pRowBlock->Row1 + 1 ) - 1;
		nTotCol         += Delta * nCol - ( Delta*(Delta-1) )/2;
		nTotColBlock    += nColBlock;
	}
	if ( nTotColBlock != pTmpMat->nColBlock ) {
		EXIT("Wrong column block count");
	}

	/*
	* Release old column block data in the temporary connectivity matrix: pTmpMat but not the
	* row block data which is the same one used by the new block matrix: pMat.
	*/
	SD_DESTROY_CHUNK(pTmpMat->PoolColBlock);

	// Alloc memory to store numerical matrix data
	pMat->SizeUpper = nTotCol;
	GD_MALLOC( pMat->pUpper, double, pMat->SizeUpper, "Matrix Data" );
	memset( pMat->pUpper, 0, sizeof(double)*pMat->SizeUpper);

	pMat->SizeBlockJump = MaxColBlock;
	GD_MALLOC( pMat->pBlockJump, int, pMat->SizeBlockJump, "Jump Data" );

	if ( gd_MemErr )
		return 1;

	return 0;

}  // ComputeBlockMatrix

/**
* @brief Release all the data allocated for the numerical factorization algorithm.
 * @param pMat SD_BLOCK_MATRIX_DATA
 * @return int
*/
int ReleaseBlockMatrix( SD_BLOCK_MATRIX_DATA *pMat )
{
	GD_FREE(pMat->pFirstColBlock);
	GD_FREE(pMat->pSizeColBlock);
	GD_FREE(pMat->pRowBlock);
	GD_FREE(pMat->pBlockJump);
	GD_FREE(pMat->pUpper);
	GD_FREE(pMat->pPerm);

	return 0;

}  // ReleaseBlockMatrix

/**
* @brief This function assemble the element connnectivity for one or more elements in order to build
* a sparse matrix format. Of course we only store the upper part of the connectivity matrix
* because we only consider structure symmetric matrices.
 * @param pMat0 SD_MATRIX_DATA
 * @param nEq int
 * @param Eq (int [])
 * @param nEl int
 * @param Dim int
 * @return int
*/
int ds_DefineConnectivity(SD_MATRIX_DATA *pMat0, const int& nEq, int Eq[], const int& nEl, const int& Dim )
{
	int e, i, j;
	int Row_i;
	SD_ROW_DATA        *pRow_i;
	SD_CON_MATRIX_DATA *pMat;

	pMat = &pMat0->Mat.Con;

	for (e = 0; e < nEl; Eq += Dim, e++) {
		for (i = 0; i < nEq; i++) {
			Row_i  = Eq[i];
			pRow_i = &SD_ROW(Row_i, pMat);

			for (j = 0; j < nEq; j++) {
				int Col_j, Found;
				SD_COL_DATA **ppC, *pCol;
				Col_j  = Eq[j];
				if ( Row_i == Col_j ) {
					continue;
				}
				SD_FIND_COL(pRow_i->Col, Col_j, ppC, Found);
				if ( !Found ) {
					SD_GET_COL(pCol, pMat);
					SD_INSERT_COL(ppC, pCol, Col_j);
					pMat->nCol++;
					// printf("Inserting %d %d\n", Row_i, Col_j);
				}
			}
		}
	}
	return 0;

}  // ds_DefineConnectivity

#ifdef __clang__
#pragma clang diagnostic pop
#endif
#ifdef __GNUG__
#pragma GCC diagnostic pop
#endif

/*
 * End of SymbFact.c
 */
