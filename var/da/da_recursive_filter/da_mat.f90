!						*****************
!						*    MAT1.FOR	*
!						*  PURSER 1994	*
!						*****************
!   Routines for basic algebraic operations on general matrices and vectors
!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!  These routines, perform basic algebraic operations on real vectors and
!  matrices. The task performed by each routine is, as far as possible,
!  encoded in each routine's name; three letters describe the
!  operation, the remainder defining the type of operand and, if needed to
!  an ambiguity, the type of result.
!
!  OPERATIONS:
!   ADD     add first two operands, return result as third argument
!   ADF     add first two operands, "flip" (reverse order), put into 3rd.
!   ANF     add negative of 1st to negative of 2nd and "flip" result into 3rd
!   CNF     copy negative of flipped (order-reversed) 1st operand to 2nd
!   CPF     copy positive of flipped (order-reversed) 1st operand to 2nd
!   CON     copy negative
!   COP     copy positive
!   DAD     dot product of first two operands added to third
!   DET     evaluate log-determinant
!   DIF     differentiate
!   DIV     divide first operand by second
!   DOT     dot product of first two operands
!   DSB     dot product of first two operands subtracted from third
!   FAD     "flip" 1st operand, (reverse order of elements), add to 2nd
!   FDA     "flip" 1st operand, dot product with 2nd, add to 3rd
!   FDO     "flip" (reverse order of) 1st operand and form dot product with 2nd
!   FDS     "flip" 1st operand, dot product with 2nd,  subtract result from 3rd
!   FSB     "flip" 1st operand, subtract 2nd (cf. FAD)
!   FSN     "flip" 1st operand, subtract it from 2nd operand (negtve of FSB)
!   INT     integrate
!   INV     invert the matrix, or linear system involving the matrix operand
!   L1L     Cholesky LU decomposition, where U is just L-transpose
!   L1U     L-U decomposition of first arg, with 1's along diagonal of L and U
!   LDL     Cholesky LDU decomposition, where U is just L-transpose and D diag.
!   LDU     LDU decomposition
!   MAD     multiply first two operands, but then add result to third
!   MUL     multiply first two operands, return result as third argument
!   MSB     multiply first two operands, but then subtract result from third
!   NEG     replace operand by its negative
!   NOR     evaluate norm of operand
!   POL     polynomial (first argument) of second argument
!   POW     raise operand to some integer power
!   SBF     subtract from 1st operand the 2nd, "flip" result into 3rd (cf ADF)
!   SUB     subtract first two operands, return result as third argument
!   SUM     sum the elements of first operand
!   SWP     swap first two operands
!   TPS     replace operand by its transpose
!   TRC     evaluate trace of operand
!   U1L     back substitution with matrix decomposed into LU form, 1's on diag.
!   UDL     back substitution with matrix decomposed into LDU form
!   WRT     write out
!   ZER     set operand to zero
!
!  OPERAND TYPES:
!   B	    banded matrix
!   C	    circulant matrix
!   D	    diagonal matrix
!   H	    symmetric or hermitian matrix
!   L	    lower triangular matrix
!   M	    matrix (rectangular, in general)
!   P	    polynomial or power-series coefficient vector
!   Q	    sQuare matrix with Fortan dimension same as logical dimension
!   R	    row of a matrix
!   S	    scalar
!   T	    transpose of the matrix
!   U	    upper triangular matrix
!   V	    vector, or column of a matrix
!   X	    field of parallel X-vectors (aligned like "columns" of a matrix)
!   Y	    field of parallel Y-vectors (aligned like "rows" of a matrix)
!
!   For those matrix routines with a "Q" in the last part of the name,
!   denoting operation upon a square matrix, the Fortran dimensions and
!   matrix order are implicitly assumed identical. If this is not the case,
!   the general matrix routines, (those without a "Q" in the name) allow
!   the arguments, MI, MJ, which denote the algebraic dimensions, to be
!   different from each other (for rectangular matrices) and different from
!   the Fortran dimensions NA, NB, etc., which are therefore listed as
!   additional parameters.
!
!------------------------------------------------------------------------------
      SUBROUTINE ZERV(D,M)    ! set elements of a vector to zero
      DIMENSION D(M)
      DO I=1,M
       D(I)=0.
      ENDDO
      RETURN
      ENTRY NEGV(D,M)	      ! Replace vector by its negative
      DO I=1,M
       D(I)=-D(I)
      ENDDO
      RETURN
      ENTRY COPSD(S,D,M)      ! Copy a scalar to a diagonal matrix
      DO I=1,M
       D(I)=S
      ENDDO
      RETURN
      ENTRY CONSD(S,D,M)      ! Copy negative of a scalar to a diagonal
      DO I=1,M
       D(I)=-S
      ENDDO
      RETURN
      END
      SUBROUTINE ZERM(A,MI,MJ,NA) ! Set the elements of general matrix to zero
      DIMENSION A(NA,*)
      DO J=1,MJ
       CALL ZERV(A(1,J),MI)
      ENDDO
      RETURN
      ENTRY NEGM(A,MI,MJ,NA) ! Replace general matrix A by its negative
      DO J=1,MJ
       CALL NEGV(A(1,J),MI)
      ENDDO
      RETURN
      END

      SUBROUTINE ADDRR(A,B,C,M,NA,NB,NC)
      DIMENSION A(NA,*),B(NB,*),C(NC,*)
      DO I=1,M
       C(1,I)=A(1,I)+B(1,I)
      ENDDO
      RETURN
      ENTRY	 SUBRR(A,B,C,M,NA,NB,NC)
      DO I=1,M
       C(1,I)=A(1,I)-B(1,I)
      ENDDO
      RETURN
      ENTRY	 ADNRR(A,B,C,M,NA,NB,NC)
      DO I=1,M
       C(1,I)=-A(1,I)-B(1,I)
      ENDDO
      RETURN
      ENTRY	 MULRR(A,B,C,M,NA,NB,NC)
      DO I=1,M
       C(1,I)=A(1,I)*B(1,I)
      ENDDO
      RETURN
      ENTRY	 MADRR(A,B,C,M,NA,NB,NC)
      DO I=1,M
       C(1,I)=C(1,I)+A(1,I)*B(1,I)
      ENDDO
      RETURN
      ENTRY	 MSBRR(A,B,C,M,NA,NB,NC)
      DO I=1,M
       C(1,I)=C(1,I)-A(1,I)*B(1,I)
      ENDDO
      RETURN
      ENTRY	 FADRR(A,B,C,M,NA,NB,NC)
      MP=M+1
      DO I=1,M
       C(1,I)=B(1,I)+A(1,MP-I)
      ENDDO
      RETURN
      ENTRY	 FSBRR(A,B,C,M,NA,NB,NC)
      MP=M+1
      DO I=1,M
       C(1,I)=A(1,MP-I)-B(1,I)
      ENDDO
      RETURN
      ENTRY	 FSNRR(A,B,C,M,NA,NB,NC)
      MP=M+1
      DO I=1,M
       C(1,I)=-A(1,MP-I)+B(1,I)
      ENDDO
      RETURN
      ENTRY	 ADFRR(A,B,C,M,NA,NB,NC)
      MP=M+1
      DO I=1,M
       C(1,MP-I)=A(1,I)+B(1,I)
      ENDDO
      RETURN
      ENTRY	 ANFRR(A,B,C,M,NA,NB,NC)
      MP=M+1
      DO I=1,M
       C(1,MP-I)=-A(1,I)-B(1,I)
      ENDDO
      RETURN
      ENTRY	 SBFRR(A,B,C,M,NA,NB,NC)
      MP=M+1
      DO I=1,M
       C(1,MP-I)=A(1,I)-B(1,I)
      ENDDO
      RETURN
      ENTRY	 ZERR(A,M,NA)
      DO I=1,M
       A(1,I)=0.
      ENDDO
      RETURN
      END

      SUBROUTINE COPR(A,B,M,NA,NB)
      DIMENSION A(NA,M),B(NB,M)
      DO I=1,M
       B(1,I)=A(1,I)
      ENDDO
      RETURN
      ENTRY	 CONR(A,B,M,NA,NB)
      DO I=1,M
       B(1,I)=-A(1,I)
      ENDDO
      RETURN
      ENTRY	 CPFR(A,B,M,NA,NB)
      MP=M+1
      DO I=1,M
       B(1,I)=A(1,MP-I)
      ENDDO
      RETURN
      ENTRY	 CNFR(A,B,M,NA,NB)
      MP=M+1
      DO I=1,M
       B(1,I)=-A(1,MP-I)
      ENDDO
      RETURN
      END
      SUBROUTINE COPVR(D,A,M,NA)
      DIMENSION D(M),A(NA,M),B(NB,M)
      DO I=1,M
       A(1,I)=D(I)
      ENDDO
      RETURN
      ENTRY	 CONVR(D,A,M,NA)
      DO I=1,M
       A(1,I)=-D(I)
      ENDDO
      RETURN
      ENTRY	 COPRV(A,D,M,NA)
      DO I=1,M
       D(I)=A(1,I)
      ENDDO
      RETURN
      ENTRY	 CONRV(A,D,M,NA)
      DO I=1,M
       D(I)=-A(1,I)
      ENDDO
      RETURN

      ENTRY	 MULVR(D,A,B,M,NA,NB)
      ENTRY	 MULDR(D,A,B,M,NA,NB)
      DO I=1,M
       B(1,I)=D(I)*A(1,I)
      ENDDO
      RETURN
      ENTRY	 MADVR(D,A,B,M,NA,NB)
      ENTRY	 MADDR(D,A,B,M,NA,NB)
      DO I=1,M
       B(1,I)=B(1,I)+D(I)*A(1,I)
      ENDDO
      RETURN
      ENTRY	 MSBVR(D,A,B,M,NA,NB)
      ENTRY	 MSBDR(D,A,B,M,NA,NB)
      DO I=1,M
       B(1,I)=B(1,I)-D(I)*A(1,I)
      ENDDO
      RETURN
      END

      SUBROUTINE MULVS(D,S,E,M)
      DIMENSION D(*),E(*)
      DO I=1,M
       E(I)=D(I)*S
      ENDDO
      RETURN
      ENTRY MADVS(D,S,E,M)
      DO I=1,M
       E(I)=E(I)+D(I)*S
      ENDDO
      RETURN
      ENTRY MSBVS(D,S,E,M)
      DO I=1,M
       E(I)=E(I)-D(I)*S
      ENDDO
      RETURN
      END

      SUBROUTINE MULRS(A,S,B,M,NA,NB)
      DIMENSION A(NA,*),B(NB,*)
      DO I=1,M
       B(1,I)=A(1,I)*S
      ENDDO
      RETURN
      ENTRY MADRS(A,S,B,M,NA,NB)
      DO I=1,M
       B(1,I)=B(1,I)+A(1,I)*S
      ENDDO
      RETURN
      ENTRY MSBRS(A,S,B,M,NA,NB)
      DO I=1,M
       B(1,I)=B(1,I)-A(1,I)*S
      ENDDO
      RETURN
      END

      FUNCTION DOT(D,E,M)
      DIMENSION D(M),E(M)
      DOT=0.
      DO I=1,M
       DOT=DOT+D(I)*E(I)
      ENDDO
      RETURN
      END

      SUBROUTINE DOTVV(D,E,S,M)
      DIMENSION D(M),E(M)
      S=0.
      ENTRY DADVV(D,E,S,M)
      DO I=1,M
       S=S+D(I)*E(I)
      ENDDO
      RETURN
      ENTRY DSBVV(D,E,S,M)
      DO I=1,M
       S=S-D(I)*E(I)
      ENDDO
      RETURN
      END

      SUBROUTINE DOTVR(D,A,S,M,NA)
      DIMENSION D(M),A(NA,*)
      S=0.
      ENTRY DADVR(D,A,S,M,NA)
      DO I=1,M
       S=S+D(I)*A(1,I)
      ENDDO
      RETURN
!      ENTRY DSBVR(D,A,S,M,NA)
!      DO I=1,M
!       S=S-D(I)*A(1,I)
!      ENDDO
!      RETURN
      END

      SUBROUTINE DOTRR(A,B,S,M,NA,NB)
      DIMENSION A(NA,*),B(NB,*)
      S=0.
      ENTRY DADRR(A,B,S,M,NA,NB)
      DO I=1,M
       S=S+A(1,I)*B(1,I)
      ENDDO
      RETURN
      ENTRY DSBRR(A,B,S,M,NA,NB)
      DO I=1,M
       S=S-A(1,I)*B(1,I)
      ENDDO
      RETURN
      END

      FUNCTION PRO333(D,E,F) ! TRIPLE PRODUCT OF 3 3-VECTORS
      DIMENSION D(3),E(3),F(3),G(3)
      CALL CRO33(E,F,G)
      CALL DOTVV(D,G,PRO333,3)
      RETURN
      END

      SUBROUTINE NORV(D,S,M) ! NORM OF VECTOR..
      DIMENSION D(M)
      S=SQRT(DOT(D,D,M))
      RETURN
      ENTRY	 NORQ(D,S,M)	 ! ...OF SQUARE MATRIX.
      S=SQRT(DOT(Q,Q,M*M))
      RETURN
      END

      SUBROUTINE NORR(A,S,M,NA)
      DIMENSION A(NA,M)
      CALL DOTRR(A,A,S,M,NA,NA)
      S=SQRT(S)
      RETURN
      END

      SUBROUTINE CRO33(A,B,C) ! SPECIAL CASE OF 3-DIMENSIONS: CROSS-PRODUCT
      DIMENSION A(3),B(3),C(3)
      C(1)=A(2)*B(3)-A(3)*B(2)
      C(2)=A(3)*B(1)-A(1)*B(3)
      C(3)=A(1)*B(2)-A(2)*B(1)
      RETURN
      END

      SUBROUTINE MULVV(A,B,C,M)
      DIMENSION A(*),B(*),C(*)
      DO I=1,M
       C(I)=A(I)*B(I)
      ENDDO
      RETURN
      ENTRY MADVV(A,B,C,M)
      DO I=1,M
       C(I)=C(I)+A(I)*B(I)
      ENDDO
      RETURN
      ENTRY MSBVV(A,B,C,M)
      DO I=1,M
       C(I)=C(I)-A(I)*B(I)
      ENDDO
      RETURN
      ENTRY ADDVV(A,B,C,M)
      DO I=1,M
       C(I)=A(I)+B(I)
      ENDDO
      RETURN
      ENTRY SUBVV(A,B,C,M)
      DO I=1,M
       C(I)=A(I)-B(I)
      ENDDO
      RETURN
      ENTRY ADNVV(A,B,C,M)
      DO I=1,M
       C(I)=-A(I)-B(I)
      ENDDO
      RETURN
      ENTRY FADVV(A,B,C,M)
      MP=M+1
      DO I=1,M
       C(I)=B(I)+A(MP-I)
      ENDDO
      RETURN
      ENTRY FSBVV(A,B,C,M)
      MP=M+1
      DO I=1,M
       C(I)=A(MP-I)-B(I)
      ENDDO
      RETURN
      ENTRY FSNVV(A,B,C,M)
      MP=M+1
      DO I=1,M
       C(I)=-A(MP-I)+B(I)
      ENDDO
      RETURN
      ENTRY DIVVV(A,B,C,M)
      DO I=1,M
       C(I)=A(I)/B(I)
      ENDDO
      RETURN
      ENTRY COPV(A,B,M)
      DO I=1,M
       B(I)=A(I)
      ENDDO
      RETURN
      ENTRY CONV(A,B,M)
      DO I=1,M
       B(I)=-A(I)
      ENDDO
      RETURN
      ENTRY ADFVV(A,B,C,M)
      MP=M+1
      DO I=1,M
       C(MP-I)=A(I)+B(I)
      ENDDO
      RETURN
      ENTRY ANFVV(A,B,C,M)
      MP=M+1
      DO I=1,M
       C(MP-I)=-A(I)-B(I)
      ENDDO
      RETURN
      ENTRY SBFVV(A,B,C,M)
      MP=M+1
      DO I=1,M
       C(MP-I)=A(I)-B(I)
      ENDDO
      RETURN
      END

      SUBROUTINE SWPVV(D,E,M)
      DIMENSION D(M),E(M)
      DO I=1,M
       T=D(I)
       D(I)=E(I)
       E(I)=T
      ENDDO
      RETURN
      END

      SUBROUTINE SWPRR(A,B,M,NA,NB)    ! Row swap
      DIMENSION A(NA,*),B(NB,*)
      DO J=1,M
       T=A(1,J)
       A(1,J)=B(1,J)
       B(1,J)=T
      ENDDO
      RETURN
      END

      SUBROUTINE TPSM(A,MI,MJ,NA)  ! Transpose, in place, a general matrix
      DIMENSION A(NA,*)
      M=MAX(MI,MJ)
      IF(M.GT.NA)STOP
!     &'first array bound in TPSM too small to allow transpose to fit'
      NAP=NA+1
      DO I=1,M-1
       IP=I+1
       CALL SWPRR(A(IP,1),A(1,IP),M-I,NAP,NAP)
      ENDDO
      RETURN
      END

      SUBROUTINE MULMV(A,D,E,MI,MJ,NA)
      DIMENSION A(NA,*),D(*),E(*)
      CALL ZERV(E,MJ)
      ENTRY MADMV(A,D,E,MI,MJ,NA)
      DO J=1,MJ
       CALL MADVS(A(1,J),D(J),E,MI)
      ENDDO
      RETURN
      ENTRY MSBMV(A,D,E,MI,MJ,NA)
      DO J=1,MJ
       CALL MSBVS(A(1,J),D(J),E,MI)
      ENDDO
      RETURN
      END

      SUBROUTINE MULVM(D,A,E,MI,MJ,NA)
      DIMENSION A(NA,*),D(*),E(*)
      CALL ZERV(E,MJ)
      ENTRY MADVM(D,A,E,MI,MJ,NA)
      DO I=1,MI
       CALL MADRS(A,D(I),E,MJ,NA,1)
      ENDDO
      RETURN
      ENTRY MSBVM(D,A,E,MI,MJ,NA)
      DO I=1,MI
       CALL MSBRS(A,D(I),E,MJ,NA,1)
      ENDDO
      RETURN
      END

      SUBROUTINE MULMM(A,B,C,MI,MJ,MK,NA,NB,NC)
      DIMENSION A(NA,*),B(NB,*),C(NC,*)
      CALL ZERM(C,MI,MK,NC)
      ENTRY MADMM(A,B,C,MI,MJ,MK,NA,NB,NC)
      DO K=1,MK
      DO J=1,MJ
       CALL MADVS(A(1,J),B(J,K),C(1,K),MI)
      ENDDO
      ENDDO
      RETURN
      ENTRY MSBMM(A,B,C,MI,MJ,MK,NA,NB,NC)
      DO K=1,MK
      DO J=1,MJ
       CALL MSBVS(A(1,J),B(J,K),C(1,K),MI)
      ENDDO
      ENDDO
      RETURN

      ENTRY MULMT(A,B,C,MI,MJ,MK,NA,NB,NC)
      CALL ZERM(C,MI,MK,NC)
      ENTRY MADMT(A,B,C,MI,MJ,MK,NA,NB,NC)
      DO K=1,MK
      DO J=1,MJ
       CALL MADVS(A(1,J),B(K,J),C(1,K),MI)
      ENDDO
      ENDDO
      RETURN
      ENTRY MSBMT(A,B,C,MI,MJ,MK,NA,NB,NC)
      DO K=1,MK
      DO J=1,MJ
       CALL MSBVS(A(1,J),B(K,J),C(1,K),MI)
      ENDDO
      ENDDO
      RETURN

      ENTRY MULTM(A,B,C,MI,MJ,MK,NA,NB,NC)
      CALL ZERM(C,MI,MK,NC)
      ENTRY MADTM(A,B,C,MI,MJ,MK,NA,NB,NC)
      DO K=1,MK
      DO J=1,MJ
       CALL MADRS(A(J,1),B(J,K),C(1,K),MI,NA,1)
      ENDDO
      ENDDO
      RETURN
      ENTRY MSBTM(A,B,C,MI,MJ,MK,NA,NB,NC)
      DO K=1,MK
      DO J=1,MJ
       CALL MSBRS(A(J,1),B(J,K),C(1,K),MI,NA,1)
      ENDDO
      ENDDO
      RETURN

      ENTRY MULTT(A,B,C,MI,MJ,MK,NA,NB,NC)
      CALL ZERM(C,MI,MK,NC)
      ENTRY MADTT(A,B,C,MI,MJ,MK,NA,NB,NC)
      DO K=1,MK
      DO J=1,MJ
       CALL MADRS(A(J,1),B(K,J),C(1,K),MI,NA,1)
      ENDDO
      ENDDO
      RETURN
      ENTRY MSBTT(A,B,C,MI,MJ,MK,NA,NB,NC)
      DO K=1,MK
      DO J=1,MJ
       CALL MSBRS(A(J,1),B(K,J),C(1,K),MI,NA,1)
      ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE ADDMM(A,B,C,MI,MJ,NA,NB,NC)
      DIMENSION A(NA,*),B(NB,*),C(NC,*)
      DO J=1,MJ
      DO I=1,MI
       C(I,J)=A(I,J)+B(I,J)
      ENDDO
      ENDDO
      RETURN
      ENTRY ADDMT(A,B,C,MI,MJ,NA,NB,NC)
      DO J=1,MJ
      DO I=1,MI
       C(I,J)=A(I,J)+B(J,I)
      ENDDO
      ENDDO
      RETURN
      ENTRY ADDTM(A,B,C,MI,MJ,NA,NB,NC)
      DO J=1,MJ
      DO I=1,MI
       C(I,J)=A(J,I)+B(I,J)
      ENDDO
      ENDDO
      RETURN
      ENTRY ADDTT(A,B,C,MI,MJ,NA,NB,NC)
      DO J=1,MJ
      DO I=1,MI
       C(I,J)=A(J,I)+B(J,I)
      ENDDO
      ENDDO
      RETURN
      ENTRY SUBMM(A,B,C,MI,MJ,NA,NB,NC)
      DO J=1,MJ
      DO I=1,MI
       C(I,J)=A(I,J)-B(I,J)
      ENDDO
      ENDDO
      RETURN
      ENTRY SUBMT(A,B,C,MI,MJ,NA,NB,NC)
      DO J=1,MJ
      DO I=1,MI
       C(I,J)=A(I,J)-B(J,I)
      ENDDO
      ENDDO
      RETURN
      ENTRY SUBTM(A,B,C,MI,MJ,NA,NB,NC)
      DO J=1,MJ
      DO I=1,MI
       C(I,J)=A(J,I)-B(I,J)
      ENDDO
      ENDDO
      RETURN
      ENTRY SUBTT(A,B,C,MI,MJ,NA,NB,NC)
      DO J=1,MJ
      DO I=1,MI
       C(I,J)=A(J,I)-B(J,I)
      ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE COPM(A,B,MI,MJ,NA,NB)
      DIMENSION A(NA,*),B(NB,*)
      DO J=1,MJ
      DO I=1,MI
       B(I,J)=A(I,J)
      ENDDO
      ENDDO
      RETURN
      ENTRY CONM(A,B,MI,MJ,NA,NB)
      DO J=1,MJ
      DO I=1,MI
       B(I,J)=-A(I,J)
      ENDDO
      ENDDO
      RETURN
      ENTRY MULMS(A,SS,B,MI,MJ,NA,NB)
      DO J=1,MJ
      DO I=1,MI
       B(I,J)=A(I,J)*SS
      ENDDO
      ENDDO
      RETURN
      ENTRY COPT(A,B,MI,MJ,NA,NB)
      DO J=1,MJ
      DO I=1,MI
       B(I,J)=A(J,I)
      ENDDO
      ENDDO
      RETURN
      ENTRY CONT(A,B,MI,MJ,NA,NB)
      DO J=1,MJ
      DO I=1,MI
       B(I,J)=-A(J,I)
      ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE MULMD(A,D,B,MI,MJ,NA,NB)
      DIMENSION A(NA,*),B(NB,*),D(*)
      DO J=1,MJ
       CALL MULVS(A(1,J),D(J),B(1,J),MI)
      ENDDO
      RETURN
      ENTRY MULTD(A,D,B,MI,MJ,NA,NB)
      DO J=1,MJ
       CALL MULRS(A(J,1),D(J),B(1,J),MI,NA,1)
      ENDDO
      RETURN
      ENTRY MULDM(D,A,B,MI,MJ,NA,NB)
      DO I=1,MI
       CALL MULRS(A(I,1),D(I),B(I,1),MJ,NA,NB)
      ENDDO
      RETURN
      ENTRY MULDT(D,A,B,MI,MJ,NA,NB)
      DO I=1,MI
       CALL MULRS(A(1,I),D(I),B(I,1),MJ,1,NB)
      ENDDO
      RETURN
      ENTRY MADMD(A,D,B,MI,MJ,NA,NB)
      DO J=1,MJ
       CALL MADVS(A(1,J),D(J),B(1,J),MI)
      ENDDO
      RETURN
      ENTRY MADTD(A,D,B,MI,MJ,NA,NB)
      DO J=1,MJ
       CALL MADRS(A(J,1),D(J),B(1,J),MI,NA,1)
      ENDDO
      RETURN
      ENTRY MADDM(D,A,B,MI,MJ,NA,NB)
      DO I=1,MI
       CALL MADRS(A(I,1),D(I),B(I,1),MJ,NA,NB)
      ENDDO
      RETURN
      ENTRY MADDT(D,A,B,MI,MJ,NA,NB)
      DO I=1,MI
       CALL MADRS(A(1,I),D(I),B(I,1),MJ,1,NB)
      ENDDO
      RETURN
      ENTRY MSBMD(A,D,B,MI,MJ,NA,NB)
      DO J=1,MJ
       CALL MSBVS(A(1,J),D(J),B(1,J),MI)
      ENDDO
      RETURN
      ENTRY MSBTD(A,D,B,MI,MJ,NA,NB)
      DO J=1,MJ
       CALL MSBRS(A(J,1),D(J),B(1,J),MI,NA,1)
      ENDDO
      RETURN
      ENTRY MSBDM(D,A,B,MI,MJ,NA,NB)
      DO I=1,MI
       CALL MSBRS(A(I,1),D(I),B(I,1),MJ,NA,NB)
      ENDDO
      RETURN
      ENTRY MSBDT(D,A,B,MI,MJ,NA,NB)
      DO I=1,MI
       CALL MSBRS(A(1,I),D(I),B(I,1),MJ,1,NB)
      ENDDO
      RETURN
      END

      FUNCTION MCMAX(A,B,M)
      DIMENSION A(0:M),B(0:M)
      MCMAX=0		       ! Default for when ALL elements of C are zero
      DO MA=M,0,-1	       ! Seek last nonzero coefficient of polynomial A
       IF(A(MA).NE.0.)THEN
	DO MB=M,0,-1	       ! Seek last nonzero coefficient of polynomial B
	 IF(B(MB).NE.0.)THEN
	  MCMAX=MIN(M,MA+MB)+1 ! Hence, 1+last non-0 element of their product
	  RETURN
	 ENDIF
	ENDDO
	RETURN
       ENDIF
      ENDDO
      RETURN
      END

      SUBROUTINE MULPP(A,B,C,M) !  multiply polynomials, possibly in place
      DIMENSION A(0:M),B(0:M),C(0:M)
      MCP=MCMAX(A,B,M)
      DO I=MCP,M
       C(I)=0.
      ENDDO
      DO J=MCP,1,-1
       CALL FDOVV(A,B,S,J)
       C(J-1)=S
      ENDDO
      RETURN
      ENTRY MADPP(A,B,C,M)
      MCP=MCMAX(A,B,M)
      DO J=MCP,1,-1
       CALL FDOVV(A,B,S,J)
       C(J-1)=C(J-1)+S
      ENDDO
      RETURN
      ENTRY MSBPP(A,B,C,M)
      MCP=MCMAX(A,B,M)
      DO J=MCP,1,-1
       CALL FDOVV(A,B,S,J)
       C(J-1)=C(J-1)-S
      ENDDO
      RETURN
      ENTRY DIFP(A,B,M) ! Symbolically differentiate polynomial
      DO I=1,M		! possibly with coincident storage for A and B
       B(I-1)=I*A(I)
      ENDDO
      B(M)=0.
      RETURN
      ENTRY INTP(A,B,M) ! Symbolically integrate polynomial
      DO I=M,1,-1	! possibly with coincident storage for A and B
       B(I)=A(I-1)/I
      ENDDO
      B(0)=0.
      RETURN
      ENTRY INVP(A,B,M)  ! Invert polynomial or power-series
      B0=1./A(0)	 ! Storage of A and B must NOT be the same
      B(0)=B0
      DO I=1,M
       CALL FDOVV(B,A(1),S,I)
       B(I)=-B0*S
      ENDDO
      RETURN
      END

      SUBROUTINE PRGV(D,M)
#if ( DWORDSIZE != RWORDSIZE )
      PARAMETER(CRIT=1.E-30)
#else
      PARAMETER(CRIT=1.E-60)
#endif
      DIMENSION D(*)
      DO I=1,M
       IF(ABS(D(I)).LE.CRIT)D(I)=0.
      ENDDO
      RETURN
      END

      SUBROUTINE FDOVV(D,E,S,M) ! dot-product of "flipped" vector D with E
      DIMENSION D(*),E(*)	! (order of the elements of E is reversed)
      S=0.
      ENTRY FDAVV(D,E,S,M)	! like FDOVV but result added to existing S
      MP=M+1
      DO I=1,M
       S=S+D(MP-I)*E(I)
      ENDDO
      RETURN
      ENTRY FDSVV(D,E,S,M)	! like FDAVV but result subtracted from S
      MP=M+1
      DO I=1,M
       S=S-D(MP-I)*E(I)
      ENDDO
      RETURN
      END

      SUBROUTINE FDORR(A,B,S,M,NA,NB)
      DIMENSION A(NA,M),B(NB,M)
      S=0.
      ENTRY	 FDARR(A,B,S,M,NA,NB)
      MP=M+1
      DO I=1,M
       S=S+A(1,MP-I)*B(1,I)
      ENDDO
      RETURN
      ENTRY	 FDSRR(A,B,S,M,NA,NB)
      MP=M+1
      DO I=1,M
       S=S-A(1,MP-I)*B(1,I)
      ENDDO
      RETURN
      END

      SUBROUTINE POWP(A,B,N,M)	       ! Raise power series A to the power
      DIMENSION A(0:M),B(0:M),C(0:M)   ! of N and output as B
      B(0)=1.
      CALL ZERV(B(1),M)
      DO K=1,N
       CALL MULPP(A,B,B,M)
      ENDDO
      RETURN
      ENTRY POLPS(A,S1,S2,M) ! Apply series A to scalar S1 to obtain S2
      S2=A(M)
      DO K=M-1,0,-1
       S2=S2*S1+A(K)
      ENDDO
      RETURN
      ENTRY POLPP(A,B,C,M) ! Apply power series A to power series B and put
      C(0)=A(M) 	   ! the result out as power-series C.
      CALL ZERV(C(1),M)
      DO K=M-1,0,-1
       CALL MULPP(B,C,C,M)
       C(0)=C(0)+A(K)
      ENDDO
      RETURN
      END

      SUBROUTINE MULCC(A,B,C,M)  ! Multiply circulant matrices of period M
      DIMENSION A(0:M-1),B(0:M-1),C(0:M-1)
      CALL ZERV(C,M)
      ENTRY MADCC(A,B,C,M)
      MM=M-1
      DO J=0,MM
       MMJ=M-J
       CALL MADVS(A,B(J),C(J),MMJ)
       CALL MADVS(A(MMJ),B(J),C,J)
      ENDDO
      RETURN
      ENTRY MSBCC(A,B,C,M)
      MM=M-1
      DO J=0,MM
       MMJ=M-J
       CALL MSBVS(A,B(J),C(J),MMJ)
       CALL MSBVS(A(MMJ),B(J),C,J)
      ENDDO
      RETURN
      END

      SUBROUTINE ADDQQ(A,B,C,M)
      DIMENSION A(M,M),B(M,M),C(M,M),D(M)
      CALL ADDVV(A,B,C,M*M)
      RETURN
      ENTRY ADDQT(A,B,C,M)
      CALL ADDMT(A,B,C,M,M,M,M,M)
      RETURN
      ENTRY SUBQQ(A,B,C,M)
      CALL SUBVV(A,B,C,M*M)
      RETURN
      ENTRY SUBQT(A,B,C,M)
      CALL SUBMT(A,B,C,M,M,M,M,M)
      RETURN
      ENTRY SUBTQ(A,B,C,M)
      CALL SUBTM(A,B,C,M,M,M,M,M)
      RETURN
      ENTRY INVQ(A,M)
      CALL INVM(A,A,M,M,M)
      RETURN
      ENTRY LINQV(A,D,M)
      CALL LINMM(A,D,M,1,M,M)
      RETURN
      END

      SUBROUTINE TPSQ(A,M)  !  Transpose, in place, a square matrix.
      DIMENSION A(M,M)
      MP=M+1
      DO I=1,M-1
       IP=I+1
       CALL SWPRR(A(IP,1),A(1,IP),M-I,MP,MP)
      ENDDO
      RETURN
      END

      FUNCTION TRCQ(A,M)	    ! Trace of square matrix A
      DIMENSION A(M,M)
      TRCQ=0.
      DO I=1,M
       TRCQ=TRCQ+A(I,I)
      ENDDO
      RETURN
      END

      SUBROUTINE ZERQ(A,M)	! Set elements of a square matrix to zero
      DIMENSION A(M,M)
      CALL ZERV(A,M*M)
      RETURN
      ENTRY ZERL(A,M) ! Zero out the strictly lower triangle of elements
      DO J=1,M
      DO I=J+1,M
       A(I,J)=0.
      ENDDO
      ENDDO
      RETURN
      ENTRY ZERU(A,M) ! Zero out the strictly upper triangle of elements
      DO J=1,M
      DO I=1,J-1
       A(I,J)=0.
      ENDDO
      ENDDO
      RETURN
      ENTRY NEGQ(A,M)	! Replace square matrix A by its negative
      CALL  NEGV(A,M*M)
      RETURN
      END

      SUBROUTINE MULQQ(A,B,C,M)
      DIMENSION A(M,M),B(M,M),C(M,M)
      CALL ZERQ(C,M)
      ENTRY MADQQ(A,B,C,M)
      DO K=1,M
      DO J=1,M
       CALL MADVS(A(1,J),B(J,K),C(1,K),M)
      ENDDO
      ENDDO
      RETURN
      ENTRY MSBQQ(A,B,C,M)
      DO K=1,M
      DO J=1,M
       CALL MSBVS(A(1,J),B(J,K),C(1,K),M)
      ENDDO
      ENDDO
      RETURN
      ENTRY MULQT(A,B,C,M)
      CALL ZERQ(C,M)
      ENTRY MADQT(A,B,C,M)
      DO K=1,M
      DO J=1,M
       CALL MADVS(A(1,J),B(K,J),C(1,K),M)
      ENDDO
      ENDDO
      RETURN
      ENTRY MSBQT(A,B,C,M)
      DO K=1,M
      DO J=1,M
       CALL MSBVS(A(1,J),B(K,J),C(1,K),M)
      ENDDO
      ENDDO
      RETURN
      ENTRY MULTQ(A,B,C,M)
      CALL ZERQ(C,M)
      ENTRY MADTQ(A,B,C,M)
      DO K=1,M
      DO J=1,M
       CALL MADRS(A(J,1),B(J,K),C(1,K),M,M,1)
      ENDDO
      ENDDO
      RETURN
      ENTRY MSBTQ(A,B,C,M)
      DO K=1,M
      DO J=1,M
       CALL MSBRS(A(J,1),B(J,K),C(1,K),M,M,1)
      ENDDO
      ENDDO
      RETURN
      END
!			  WD23JP
!							  *****************
!							  *   MAT2.FOR	  *
!							  *  PURSER 1996  *
!							  *****************
!
      SUBROUTINE COPQ(A,B,M)
      DIMENSION A(M,M),B(M,M),D(M),E(M)
      CALL COPV(A,B,M*M)
      RETURN
      ENTRY CONQ(A,B,M)
      CALL CONV(A,B,M*M)
      RETURN
      ENTRY COPTQ(A,B,M)
      DO J=1,M
      DO I=1,M
       B(I,J)=A(J,I)
      ENDDO
      ENDDO
      RETURN
      ENTRY CONTQ(A,B,M)
      DO J=1,M
      DO I=1,M
       B(I,J)=-A(J,I)
      ENDDO
      ENDDO
      RETURN
      ENTRY COPDQ(D,A,M)
      CALL ZERV(A,M*M)
      DO I=1,M
       A(I,I)=D(I)
      ENDDO
      RETURN
      ENTRY CONDQ(D,A,M)
      CALL ZERV(A,M*M)
      DO I=1,M
       A(I,I)=-D(I)
      ENDDO
      RETURN
      ENTRY COPSQ(S,A,M)
      CALL ZERV(A,M*M)
      DO I=1,M
       A(I,I)=S
      ENDDO
      RETURN
      ENTRY CONSQ(S,A,M)
      CALL ZERV(A,M*M)
      DO I=1,M
       A(I,I)=-S
      ENDDO
      RETURN
      ENTRY ADDQD(A,B,D,M)
      CALL COPV(A,B,M*M)
      DO I=1,M
       B(I,I)=B(I,I)+D(I)
      ENDDO
      RETURN
      ENTRY SUBQD(A,B,D,M)
      CALL COPV(A,B,M*M)
      DO I=1,M
       B(I,I)=B(I,I)-D(I)
      ENDDO
      RETURN
      ENTRY ADDQS(A,B,S,M)
      CALL COPV(A,B,M*M)
      DO I=1,M
       B(I,I)=B(I,I)+S
      ENDDO
      RETURN
      ENTRY SUBQS(A,B,S,M)
      CALL COPV(A,B,M*M)
      DO I=1,M
       B(I,I)=B(I,I)-S
      ENDDO
      RETURN
      ENTRY MULQS(A,B,S,M)
      CALL  MULVS(A,B,S,M*M)
      RETURN
      ENTRY MADQS(A,B,S,M)
      CALL  MADVS(A,B,S,M*M)
      RETURN
      ENTRY MSBQS(A,B,S,M)
      CALL  MSBVS(A,B,S,M*M)
      ENTRY MULQD(A,B,D,M)
      DO J=1,M
       CALL MULVS(A(1,J),B(1,J),D(J),M)
      ENDDO
      RETURN
      ENTRY MADQD(A,B,D,M)
      DO J=1,M
       CALL MADVS(A(1,J),B(1,J),D(J),M)
      ENDDO
      RETURN
      ENTRY MSBQD(A,B,D,M)
      DO J=1,M
       CALL MSBVS(A(1,J),B(1,J),D(J),M)
      ENDDO
      RETURN
      ENTRY MULDQ(D,A,B,M)
      CALL  MULDM(D,A,B,M,M,M,M)
      RETURN
      ENTRY MADDQ(D,A,B,M)
      CALL  MADDM(D,A,B,M,M,M,M)
      RETURN
      ENTRY MSBDQ(D,A,B,M)
      CALL  MSBDM(D,A,B,M,M,M,M)
      RETURN
      ENTRY MULQV(A,D,E,M)
      CALL ZERV(E,M)
      ENTRY MADQV(A,D,E,M)
      DO J=1,M
       CALL MADVS(A(1,J),D(J),E,M)
      ENDDO
      RETURN
      ENTRY MSBQV(A,D,E,M)
      DO J=1,M
       CALL MSBVS(A(1,J),D(J),E,M)
      ENDDO
      RETURN
      ENTRY MULVQ(D,A,E,M)
      CALL ZERV(E,M)
      ENTRY MADVQ(D,A,E,M)
      CALL  MADVM(D,A,E,M,M,M)
      RETURN
      ENTRY MSBVQ(D,A,E,M)
      CALL  MSBVM(D,A,E,M,M,M)
      RETURN
      END



      SUBROUTINE L1LM(A,B,MI,NA,NB) ! Cholesky, M -> L*U, U(i,j)=L(j,i)
      DIMENSION A(NA,*),B(NB,*)
      DO J=1,MI
       JM=J-1
       JP=J+1
       S=A(J,J)
       CALL DSBRR(B(J,1),B(J,1),S,JM,NB,NB)
       IF(S.LE.0.)THEN
 PRINT'('' L1LM detects non-positivity at diagonal index'',i2)',J
	STOP
       ENDIF
       B(J,J)=SQRT(S)
       BJJI=1./B(J,J)
       DO I=JP,MI
	S=A(I,J)
	CALL DSBRR(B(I,1),B(J,1),S,JM,NB,NB)
	B(I,J)=S*BJJI
       ENDDO
       CALL ZERV(B(1,J),JM)
      ENDDO
      RETURN
      END

      SUBROUTINE LDLM(A,B,M,NA,NB) ! M -> L*D*U, U(i,j)=L(j,i)
      DIMENSION A(NA,*),B(NB,*)
      DO J=1,M
       JM=J-1
       JP=J+1
       S=A(J,J)
       CALL DSBVR(B(1,J),B(J,1),S,JM,NB)
       B(J,J)=S
       IF(S.EQ.0.)THEN
	PRINT'('' LDLML detects singularity at diagonal index'',i2)',J
	STOP
       ENDIF
       BJJI=1./S
       DO I=JP,M
	S=A(I,J)
	CALL DSBVR(B(1,J),B(I,1),S,JM,NB)
	B(J,I)=S
	B(I,J)=S*BJJI
       ENDDO
      ENDDO
      DO J=1,M
       CALL ZERV(B(1,J),J-1)
      ENDDO
      RETURN
      END

      SUBROUTINE L1LQ(A,B,M) ! Cholesky decompose Q --> L*U, U(i,j)=L(j,i)
      DIMENSION A(M,M),B(M,M)  ! Fortran dimensions same as matrix order.
      DO J=1,M
       JM=J-1
       JP=J+1
       S=A(J,J)
       CALL DSBRR(B(J,1),B(J,1),S,JM,M,M)
       IF(S.LE.0.)THEN
	PRINT'('' L1LQ detects non-positivity at diagonal index'',i2)',J
	STOP
       ENDIF
       B(J,J)=SQRT(S)
       BJJI=1./B(J,J)
       DO I=JP,M
	S=A(I,J)
	CALL DSBRR(B(I,1),B(J,1),S,JM,M,M)
	B(I,J)=S*BJJI
       ENDDO
       CALL ZERV(B(1,J),JM)
      ENDDO
      RETURN
      END

      SUBROUTINE LDLQ(A,B,M) ! Cholesky decompose Q --> L*D*U, U(i,j)=L(j,i)
      DIMENSION A(M,M),B(M,M)	! "Q" signifies square matrix form, Fortran
      DO J=1,M			!  dimensions same as the order of the matrix.
       JM=J-1
       JP=J+1
       S=A(J,J)
       CALL DSBVR(B(1,J),B(J,1),S,JM,M)
       B(J,J)=S
       IF(S.EQ.0.)THEN
	PRINT'('' LDLQ detects singularity at diagonal index'',i2)',J
	STOP
       ENDIF
       BJJI=1./S
       DO I=JP,M
	S=A(I,J)
	CALL DSBVR(B(1,J),B(I,1),S,JM,M)
	B(J,I)=S
	B(I,J)=S*BJJI
       ENDDO
      ENDDO
      CALL ZERU(B,M)
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!   wd23jp@sun1.wwb.noaa.gov					      1996
!		    SUBROUTINE DFCO
!
!  Compute one row of the coefficients for either the compact differencing or
!  quadrature scheme characterized by matrix equation of the form,
!			 A.d = B.c			       (*)
!  In either case, d is the derivative of c.
!
! --> ZA:   coordinates of d-points used in this row of (*)
! --> ZB:   coordinates of c-points used in this row of (*)
! --> Z0:   nominal point of application of this row of (*)
! --> NA:   number of d-points operated on by this row of the A of (*)
! --> NB:   number of c-points operated on by this row of the B of (*)
! <-- AB:   the NC=NA+NB concatenated coefficients A and B for this row
! <-- W:    work-array of size NC**2. On exit, the first element contains
!	    a measure of the conditioning of the matrix of the linear system
!	    (determinant/{product of row-lengths}).
!------------------------------------------------------------------------------
      SUBROUTINE DFCO(ZA,ZB,Z0,NA,NB,AB,W)
      DIMENSION ZA(*),ZB(*),AB(*),W(NA+NB,*)
      NC=NA+NB
      DO J=1,NA
       W(1,J)=1.
       W(2,J)=0.
       W(3,J)=0.
       Z=ZA(J)-Z0
       P=Z
       DO I=4,NC
	W(I,J)=P*(I-2)
	P=P*Z
       ENDDO
      ENDDO
      DO J=1,NB
       W(1,NA+J)=0.
       Z=ZB(J)-Z0
       P=-1.
       DO I=2,NC
	W(I,NA+J)=P
	P=P*Z
       ENDDO
      ENDDO
      AB(1)=1.
      DO I=2,NC
       AB(I)=0.
      ENDDO
      AB(3)=-1.
      CALL LINVAN(W,AB,NC) ! Solve the linear system
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!   wd23jp@sun1.wwb.noaa.gov					      1996
!		    ENTRY      AVCO
!
!  Compute one row of the coefficients for the compact mid-interval
!  interpolation scheme characterized by matrix equation of the form,
!			 A.t = B.s			       (*)
!  Where s is the vector of "source" values, t the staggered "target" values.
!
! --> ZA:   coordinates of t-points used in this row of (*)
! --> ZB:   coordinates of s-points used in this row of (*)
! --> Z0:   nominal point of application of this row of (*)
! --> NA:   number of t-points operated on by this row of the A of (*)
! --> NB:   number of s-points operated on by this row of the B of (*)
! <-- AB:   the NC=NA+NB concatenated coefficients A and B for this row
! <-- W:    work-array of size NC**2. On exit, the first element contains
!	    a measure of the conditioning of the matrix of the linear system
!	    (determinant/{product of row-lengths}).
!------------------------------------------------------------------------------
      SUBROUTINE AVCO(ZA,ZB,Z0,NA,NB,AB,W)
      DIMENSION ZA(*),ZB(*),AB(*),W(NA+NB,*)
      NC=NA+NB
      DO J=1,NA
       W(1,J)=1.
       Z=ZA(J)-Z0
       P=1.
       DO I=2,NC
	W(I,J)=P
	P=P*Z
       ENDDO
      ENDDO
      DO J=1,NB
       W(1,NA+J)=0.
       Z=ZB(J)-Z0
       P=-1.
       DO I=2,NC
	W(I,NA+J)=P
	P=P*Z
       ENDDO
      ENDDO
      AB(1)=1.
      DO I=2,NC
       AB(I)=0.
      ENDDO
      CALL LINVAN(W,AB,NC) ! Solve the linear system
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!   wd23jp@sun1.wwb.noaa.gov					      1996
!		    ENTRY      DFCO2
!
!  Compute one row of the coefficients for either the compact second-
!  differencing scheme characterized by matrix equation of the form,
!			 A.d = B.c			       (*)
!  Where d is the second-derivative of c.
!
! --> ZA:   coordinates of d-points used in this row of (*)
! --> ZB:   coordinates of c-points used in this row of (*)
! --> Z0:   nominal point of application of this row of (*)
! --> NA:   number of d-points operated on by this row of the A of (*)
! --> NB:   number of c-points operated on by this row of the B of (*)
! <-- AB:   the NC=NA+NB concatenated coefficients A and B for this row
! <-- W:    work-array of size NC**2. On exit, the first element contains
!	    a measure of the conditioning of the matrix of the linear system
!	    (determinant/{product of row-lengths}).
!------------------------------------------------------------------------------
      SUBROUTINE DFCO2(ZA,ZB,Z0,NA,NB,AB,W)
      DIMENSION ZA(*),ZB(*),AB(*),W(NA+NB,*)
      NC=NA+NB
      DO J=1,NA
       W(1,J)=1.
       W(2,J)=0.
       W(3,J)=0.
       W(4,J)=0.
       Z=ZA(J)-Z0
       P=Z
       DO I=5,NC
	W(I,J)=P*(I-2)*(I-3)
	P=P*Z
       ENDDO
      ENDDO
      DO J=1,NB
       W(1,NA+J)=0.
       Z=ZB(J)-Z0
       P=-1.
       DO I=2,NC
	W(I,NA+J)=P
	P=P*Z
       ENDDO
      ENDDO
      AB(1)=1.
      DO I=2,NC
       AB(I)=0.
      ENDDO
      AB(4)=-2.
      CALL LINVAN(W,AB,NC) ! Solve the linear system
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, NCEP, Washington D.C.	1996
!		    SUBROUTINE	LDUM
!  perform l-d-u decomposition of square matrix a in place with
!  IMPLICIT pivoting
!
!  --> a    square matrix to be factorized
!  <-- ipiv array encoding the pivoting sequence
!  <-- d    indicator for possible sign change of determinant
!  --> m    degree of (active part of) a
!  --> na   first fortran dimension of a
!
!   LIMITATION:
!	S is an array, internal to this routine, containing the
!    scaling factors of each row used for pivoting decisions. It is given a
!    fortran dimension of NN=500 in the parameter statement below.
!    If the order of the linear system exceeds NN, increase NN.
!------------------------------------------------------------------------------
      SUBROUTINE LDUM(A,IPIV,D,M,NA)
      PARAMETER(NN=500)
      DIMENSION A(NA,*),IPIV(*),S(NN)
!      IF(M.GT.NN)STOP'MATRIX TOO LARGE FOR LDUM'
      IF(M.GT.NN)STOP
      DO I=1,M
       AAM=0.
       DO J=1,M
	AA=ABS(A(I,J))
	IF(AA.GT.AAM)AAM=AA
       ENDDO
       IF(AAM.EQ.0.)THEN
	PRINT'('' ROW '',I3,'' OF MATRIX IN LUFM VANISHES'')',I
	STOP
       ENDIF
       S(I)=1./AAM
      ENDDO
      D=1.
      IPIV(M)=M
      DO J=1,M-1
       JP=J+1
       ABIG=S(J)*ABS(A(J,J))
       IBIG=J
       DO I=JP,M
	AA=S(I)*ABS(A(I,J))
	IF(AA.GT.ABIG)THEN
	 IBIG=I
	 ABIG=AA
	ENDIF
       ENDDO
!  swap rows, recording changed sign of determinant
       IPIV(J)=IBIG
       IF(IBIG.NE.J)THEN
	D=-D
	DO K=1,M
	 T=A(J,K)
	 A(J,K)=A(IBIG,K)
	 A(IBIG,K)=T
	ENDDO
	S(IBIG)=S(J)
       ENDIF
       AJJ=A(J,J)
       IF(AJJ.EQ.0.)THEN
			   JM=J-1
  PRINT'('' FAILURE IN LDUM:''/'' MATRIX SINGULAR, RANK='',i3)',JM
			   STOP
       ENDIF
       AJJI=1./AJJ
       DO I=JP,M
	AIJ=AJJI*A(I,J)
	A(I,J)=AIJ
	DO K=JP,M
	 A(I,K)=A(I,K)-AIJ*A(J,K)
	ENDDO
       ENDDO
      ENDDO
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1993
!		    SUBROUTINE UDLMM
!  use l-u factors in a to back-substitute for mm rhs in b, using ipiv to
!  define the pivoting permutation used in the l-u decomposition.
!
!  --> A    L-D-U factorization of linear system matrux
!  <-> B    right-hand-sides on entry, corresponding matrix of solution
!	    vectors on return
!  --> IPIV array encoding the pivoting sequence
!  --> M    degree of (active part of) B and A
!  --> MM   number of right-hand-side vectors (active columns of B)
!  --> NA   first fortran dimension of A
!  --> NB   first fortran dimension of B
!------------------------------------------------------------------------------
      SUBROUTINE UDLMM(A,B,IPIV,M,MM,NA,NB)
      DIMENSION A(NA,*),B(NB,*),IPIV(*)
      DO K=1,MM !loop over columns of B
       DO I=1,M
	L=IPIV(I)
	S=B(L,K)
	B(L,K)=B(I,K)
	CALL DSBVR(B(1,K),A(I,1),S,I-1,NA)
	B(I,K)=S
       ENDDO
       B(M,K)=B(M,K)/A(M,M)
       DO I=M-1,1,-1
	AIII=1./A(I,I)
	CALL DSBVR(B(I+1,K),A(I,I+1),B(I,K),M-I,NA)
	B(I,K)=B(I,K)*AIII
       ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE INVM(B,A,M,NB,NA) ! copy matrix B to A, in-place invert A.
      PARAMETER (NN=500)
      DIMENSION IPIV(NN)
      DIMENSION A(NA,*),B(NB,*)
      CALL COPM(B,A,M,M,NB,NA)
      CALL LDUM(A,IPIV,D,M,NA)

!  INVERT U IN PLACE:
      DO I=1,M
       A(I,I)=1./A(I,I)
      ENDDO
      DO I=1,M-1
       DO J=I+1,M
	CALL DOTVR(A(I,J),A(I,I),S,J-I,NA)
	A(I,J)=-A(J,J)*S
       ENDDO
      ENDDO

!  INVERT L IN PLACE ASSUMING IMPLICITLY DIAGONAL ELEMENTS OF UNITY
      DO J=1,M-1
       JP=J+1
       DO I=J+1,M
	CALL DADVR(A(JP,J),A(I,JP),A(I,J),I-JP,NA)
	A(I,J)=-A(I,J)
       ENDDO
      ENDDO

!  FORM THE PRODUCT OF U**-1 AND L**-1 IN PLACE
      DO J=1,M-1
       JP=J+1
       DO I=1,J
	CALL DADVR(A(JP,J),A(I,JP),A(I,J),M-J,NA)
       ENDDO
       DO I=JP,M
	CALL DOTVR(A(I,J),A(I,I),S,M+1-I,NA)
	A(I,J)=S
       ENDDO
      ENDDO

!  PERMUTE COLUMNS ACCORDING TO IPIV
      DO J=M-1,1,-1
       L=IPIV(J)
       CALL SWPVV(A(1,J),A(1,L),M)
      ENDDO
      RETURN

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1993
!		    ENTRY LINMM
!  invert linear systems sharing the same square system matrix
!
!  <-> A    square system matrix on entry, its L-D-U factorization on return
!  <-> B    right-hand-sides on entry, corresponding matrix of solution
!	    vectors on return
!  --> M    degree of (active part of) B and A
!  --> MM   number of right-hand-side vectors (active columns of b)
!  --> NA   first fortran dimension of A
!  --> NB   first fortran dimension of B
!------------------------------------------------------------------------------
!      ENTRY			       LINMM(a,b,m,mm,na,nb)
!      CALL LDUM(A,IPIV,D,M,NA)
!      CALL UDLMM(A,B,IPIV,M,MM,NA,NB)
!      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1993
!		    SUBROUTINE INVH
!  Invert, possibly in place, a symmetric matrix
!
!  --> B    symmetric square input matrix
!  <-- A    inverse of B
!  --> M    degree of (active part of) A and B
!  --> NB   first fortran dimension of B
!  --> NA   first fortran dimension of A
!
!  LIMITATION
!     This routine incorporates no pivoting - it is intended for matrices
!     that are already diagonally dominant
!------------------------------------------------------------------------------
      SUBROUTINE INVH(B,A,M,NB,NA)
      DIMENSION B(NB,*),A(NA,*)

!  PERFORM L.D.U DECOMPOSITION OF THE SYMMETRIC MATRIX:
      CALL LDLM(B,A,M,NB,NA)

!  INVERT (IN PLACE) THE LOWER TRIANGULAR PART OF A, (ASSUMING UNIT
!  DIAGONAL ELEMENTS), AND INVERT THE DIAGONAL PART OF A (ASSUMING
!  ZERO OFF-DIAGONAL ELEMENTS). PUT TRANSPOSE OF LOWER, TIMES DIAGONAL,
!  INTO UPPER PART OF A.
      DO K=1,M
       KP=K+1
       A(K,K)=1./A(K,K)
       DO I=KP,M
	CALL DADVR(A(KP,K),A(I,KP),A(I,K),I-KP,NA)
	A(I,K)=-A(I,K)
       ENDDO
      ENDDO

!  MULTIPLY: THE TRANSPOSE OF THE LOWER PART OF A (ASSUMING UNIT DIAGS),
!  TIMES THE DIAGONAL PART (ASSUMING ZERO OFF-DIAGS), TIMES THE LOWER
!  PART. THIS PRODUCT IS THE SYMMETRIC INVERSE OF THE ORIGINAL B.
      DO I=2,M
       CALL MULRS(A(I,1),A(I,I),A(1,I),I-1,NA,1)
      ENDDO
      DO I=1,M
       IP=I+1
       DO J=1,I-1
	CALL DADVR(A(IP,I),A(J,IP),A(J,I),M-I,NA)
	A(I,J)=A(J,I)
       ENDDO
       CALL DADVR(A(IP,I),A(I,IP),A(I,I),M-I,NA)
      ENDDO

      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE INVL
!     Invert lower triangular matrix, possibly in place if A and B are same
!------------------------------------------------------------------------------
      SUBROUTINE INVL(A,B,M,NA,NB)
      DIMENSION A(NA,*),B(NB,*)
      DO J=M,1,-1
       CALL ZERV(B(1,J),J-1)
       B(J,J)=1./A(J,J)
       DO I=J+1,M
	IM=I-1
	CALL DOTVR(B(J,J),A(I,J),S,I-J,NA)
	B(I,J)=-B(I,I)*S
       ENDDO
      ENDDO
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE LINLV
!     Solve linear system involving lower triangular (LINLV) or upper
!     triangular (LINUV) matrix, right-hand-side vector U and output vector V
!
!------------------------------------------------------------------------------
      SUBROUTINE LINLV(A,U,V,M,NA)
      DIMENSION A(NA,*),U(*),V(*)
      DO I=1,M
       S=U(I)
       CALL DSBVR(V,A(I,1),S,I-1,NA)
       V(I)=S/A(I,I)
      ENDDO
      RETURN
      ENTRY LINUV(A,U,V,M,NA)
      DO J=M,1,-1
       JP=J+1
       S=U(J)
       CALL DSBVV(A(JP,J),V(JP),S,M-J)
       V(J)=S/A(J,J)
      ENDDO
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!   wd23jp@sun1.wwb.noaa.gov					      1996
!		    SUBROUTINE LINVAN
!
!   Take square matrix W and seek row and column scalings to produce non-
!   vanishing elements of rescaled W having magnitudes as close to unity
!   as possible. The approach is make the geometric mean of the nonvanishing
!   elements of each row and of each column +1 or -1. Having rescaled the
!   matrix and the r.h.s. vector AB, compute the product P of row-vector
!   norms, then compute the determinant D and solve the linear system.
!   Rescale the solution vector (now AB) and put the conditioning indicator
!   formed by the ratio D/P into the first element of W.
!
! <-> W:    Generalized Vandermonde matrix in, conditioning indicator out.
! <-> AB:   R.h.s. vector in, solution vector of numerical coefficients out.
! --> NC:   Order of the linear problem.
!------------------------------------------------------------------------------
      SUBROUTINE LINVAN(W,AB,NC)
      PARAMETER(NN=20,NIT=20)
      DIMENSION W(NC,*),AB(NC),D1(NN),D2(NN),IPIV(NN)&
      ,W2(NN,NN),V(NN)
      CALL COPM(W,W2,NC,NC,NC,NN) ! Preserve original W and AB for use
      CALL COPV(AB,V,NC)	  ! in later "clean-up" operation.

      DO I=1,NC
       D1(I)=1. 	    !	Row scaling factors set to default
       D2(I)=1. 	    !	Column scaling factors set to default
      ENDDO
      C=1.E-16	! Set initial criterion for "negligible" elements of W

! In first attempt to estimate row and column scalings, use logarithms
! to avoid the risk of under- or over-flows of the line products of W:
      DO I=1,NC
       P=0.
       E=0.
       DO J=1,NC
	DW=ABS(W(I,J))
	IF(DW.GT.C)THEN
	  E=E+1.
	  P=P+LOG(DW)
	ENDIF
       ENDDO
!       IF(E.EQ.0.)STOP'W effectively singular in LINVAN'
       IF(E.EQ.0.)STOP
       D1(I)=EXP(-P/E)
      ENDDO
      CALL MULVX(D1,W2,W,NC,NC,NN,NC) ! Rescale rows of W by D1

      DO J=1,NC
       P=0.
       E=0.
       DO I=1,NC
	DW=ABS(W(I,J))
	IF(DW.GT.C)THEN
	  E=E+1.
	  P=P+LOG(DW)
	ENDIF
       ENDDO
!       IF(E.EQ.0.)STOP'W effectively singular in LINVAN'
       IF(E.EQ.0.)STOP
       D2(J)=EXP(-P/E)
      ENDDO
      CALL MULVY(D2,W,W,NC,NC,NC,NC) ! Rescale columns of W by D2

      C=1.E-8  ! reset the criterion for "negligible" elements

! Revert to iterations of the more efficient method without logarithms:
      DO JT=1,2
      DO IT=1,NIT	    !	Perform NIT relaxation iterations
       DO I=1,NC	    !	Do rows:
	P=1.
	E=0.
	DO J=1,NC
	 DW=ABS(W(I,J))
	 IF(DW.GT.C)THEN
	  E=E+1.
	  P=P*DW
	 ENDIF
	ENDDO
	P=1./(P**(1./E))
	CALL MULRS(W(I,1),P,W(I,1),NC,NC,NC) ! Rescale this row of W..
	D1(I)=D1(I)*P			     ! ..and update D1 consistently
       ENDDO
       DO J=1,NC	    !	Do columns:
	P=1.
	E=0.
	D2J=D2(J)
	DO I=1,NC
	 DW=ABS(W(I,J))
	 IF(DW.GT.C)THEN
	  E=E+1.
	  P=P*DW
	 ENDIF
	ENDDO
	P=1./(P**(1./E))
	CALL MULVS(W(1,J),P,W(1,J),NC) ! Rescale this column of W..
	D2(J)=D2(J)*P		       ! ..and update D2 consistently
       ENDDO
      ENDDO
      C=1.E-3	    ! Final setting for criterion for "negligible" elements
      ENDDO
      CALL MULVV(D1,AB,AB,NC)	     ! Rescale r.h.s vector by D1
      P=1.			     ! P becomes product of row-lengths:
      DO I=1,NC
       CALL NORR(W(I,1),S,NC,NC)
       P=P*S
      ENDDO
      CALL LDUM(W,IPIV,D,NC,NC)
      DO I=1,NC
       D=D*W(I,I)		      ! D becomes the determinant of W
      ENDDO
      CALL UDLMM(W,AB,IPIV,NC,1,NC,NC)
      CALL MULVV(AB,D2,AB,NC)	      ! Rescale solution vector by D2
!  Note: it is very likely that round-off errors have accumulated during
!  the iterative rescaling of W. We invoke original matrix elements W2 and
!  substitute the tentative solution vector into the original (unscaled)
!  equation in order to estimate the residual components of roundoff error.

!  Begin "clean-up" process. Substitute solution vector in original
!  equation and leave the residual difference in V
      CALL MSBMM(W2,AB,V,NC,NC,1,NN,NC,NC)
      CALL MULVV(V,D1,V,NC)	      ! Rescale the residual vector by D1
      CALL UDLMM(W,V,IPIV,NC,1,NC,NC) ! Solve linear system with THIS rhs.
      CALL MADVV(V,D2,AB,NC)  ! Add residual solution vector, scaled, to AB
			      ! This will remove most of the round-off error.
      W(1,1)=D/P  ! this ratio is an indicator of the overall conditioning
      RETURN	  ! When D/P is very small, treat the results with suspicion!
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE LDUB
!  Compute [L]*[D**-1]*[U] decomposition of asymmetric band-matrix
!
! <-> A input as the asymmetric band matrix. On output, it contains
!     the [L]*[D**-1]*[U] factorization of the input matrix, where
!     [L] is lower triangular with unit main diagonal
!     [D] is a diagonal matrix
!     [U] is upper triangular with unit main diagonal
! --> M the number of rows assumed for [A]
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> NA the first fortran dimension of A
!------------------------------------------------------------------------------
      SUBROUTINE LDUB(A,M,MAH1,MAH2,NA)
      DIMENSION A(NA,-MAH1:MAH2)
      DO J=1,M
       IMOST=MIN(M,J+MAH1)
       JMOST=MIN(M,J+MAH2)
       JP=J+1
       AJJ=A(J,0)
       IF(AJJ.EQ.0.)THEN
 PRINT'('' Failure in LDUB:''/'' Matrix requires pivoting or is singular'')'
	STOP
       ENDIF
       AJJI=1./AJJ
       A(J,0)=AJJI
       DO I=JP,IMOST
	AIJ=AJJI*A(I,J-I)
	A(I,J-I)=AIJ
	DO K=JP,JMOST
	 A(I,K-I)=A(I,K-I)-AIJ*A(J,K-J)
	ENDDO
       ENDDO
       DO K=JP,JMOST
	A(J,K-J)=AJJI*A(J,K-J)
       ENDDO
      ENDDO
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1996
!		    SUBROUTINE L1UBB
!  Form the [L]*[D]*[U] decomposition of asymmetric band-matrix  [A] replace
!  lower triangular elements of [A] by [D**-1]*[L]*[D], the upper by [U],
!  replace matrix [B] by [D**-1]*[B].
!
! <-> A input as band matrix, output as lower and upper triangulars with 1s
!     implicitly assumed to lie on the main diagonal. The product of these
!     triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
! <-> B in as band matrix, out as same but premultiplied by diagonal [D**-1]
! --> M number of rows of A and B
! --> MAH1 left half-width of fortran array A
! --> MAH2 right half-width of fortran array A
! --> MBH1 left half-width of fortran array B
! --> MBH2 right half-width of fortran array B
! --> NA first fortran dimension of A
! --> NB first fortran dimension of B
!------------------------------------------------------------------------------
      SUBROUTINE L1UBB(A,B,M,MAH1,MAH2,MBH1,MBH2,NA,NB)
      DIMENSION A(NA,-MAH1:MAH2),B(NB,-MBH1:MBH2)
      DO J=1,M
       IMOST=MIN(M,J+MAH1)
       JMOST=MIN(M,J+MAH2)
       JLEAST=MAX(1,J-MAH1)
       JP=J+1
       AJJ=A(J,0)
!       IF(AJJ.EQ.0.)STOP'failure in L1UBB'
       IF(AJJ.EQ.0.)STOP
       AJJI=1./AJJ
       DO K=JLEAST,JMOST
	A(J,K-J)=AJJI*A(J,K-J)
       ENDDO
       DO I=JP,IMOST
	AIJ=A(I,J-I)
	DO K=JP,JMOST
	 A(I,K-I)=A(I,K-I)-AIJ*A(J,K-J)
	ENDDO
       ENDDO
       A(J,0)=1.
       DO K=-MBH1,MBH2
	B(J,K)=AJJI*B(J,K)
       ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE MULVX(D,V1,V2,M,MY,NV1,NV2)
      DIMENSION D(M),V1(NV1,MY),V2(NV2,MY)
      DO I=1,M
       DI=D(I)
       DO IY=1,MY
	V2(I,IY)=DI*V1(I,IY)
       ENDDO
      ENDDO
      RETURN
      ENTRY	 MADVX(D,V1,V2,M,MY,NV1,NV2)
      DO I=1,M
       DI=D(I)
       DO IY=1,MY
	V2(I,IY)=V2(I,IY)+DI*V1(I,IY)
       ENDDO
      ENDDO
      RETURN
      ENTRY	 MSBVX(D,V1,V2,M,MY,NV1,NV2)
      DO I=1,M
       DI=D(I)
       DO IY=1,MY
	V2(I,IY)=V2(I,IY)-DI*V1(I,IY)
       ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE MULVY(D,V1,V2,M,MX,NV1,NV2)
      DIMENSION D(M),V1(NV1,M),V2(NV2,M)
      DO I=1,M
       DI=D(I)
       DO IX=1,MX
	V2(IX,I)=DI*V1(IX,I)
       ENDDO
      ENDDO
      RETURN
      ENTRY	 MADVY(D,V1,V2,M,MX,NV1,NV2)
      DO I=1,M
       DI=D(I)
       DO IX=1,MX
	V2(IX,I)=V2(IX,I)+DI*V1(IX,I)
       ENDDO
      ENDDO
      RETURN
      ENTRY	 MSBVY(D,V1,V2,M,MX,NV1,NV2)
      DO I=1,M
       DI=D(I)
       DO IX=1,MX
	V2(IX,I)=V2(IX,I)-DI*V1(IX,I)
       ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE SUMM(A,S,MI,MJ,NA)
      DIMENSION A(NA,*),D(*)
      S=0.
      DO J=1,MJ
      DO I=1,MI
       S=S+A(I,J)
      ENDDO
      ENDDO
      RETURN
      ENTRY	 SUMR(A,S,M,NA)
      S=0.
      DO J=1,M
       S=S+A(1,J)
      ENDDO
      RETURN
      ENTRY	 SUMV(D,S,M)
      S=0.
      DO I=1,M
       S=S+D(I)
      ENDDO
      RETURN
      ENTRY SUMQ(A,S,NA)
      S=0.
      DO J=1,NA
      DO I=1,NA
       S=S+A(I,J)
      ENDDO
      ENDDO
      RETURN
      END
!						*****************
!						*    MAT3.FOR	*
!						*  PURSER 1993	*
!						*****************
!   DOUBLE PRECISION VERSIONS OF ROUTINES IN MAT1.FOR
!   Routines for basic algebraic operations on general matrices and vectors
!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!  These routines, perform basic algebraic operations on real vectors and
!  matrices. The task performed by each routine is, as far as possible,
!  encoded in each routine's name; three letters describe the
!  operation, the remainder defining the type of operand and, if needed to
!  an ambiguity, the type of result.
!
!  OPERATIONS:
!   ADD     add first two operands, return result as third argument
!   CON     copy negative
!   COP     copy positive
!   DAD     dot product of first two operands added to third
!   DET     evaluate log-determinant
!   DIF     differentiate
!   DIV     divide first operand by second
!   DOT     dot product of first two operands
!   DSB     dot product of first two operands subtracted from third
!   FDA     folded dot product of first two operands added to third
!   FDO     "flip" (reverse order of) 1st operand and form dot product with 2nd
!   FDS     "flip" 1st operand, dot product with 2nd, subtract result from 3rd
!   INT     integrate
!   INV     invert the matrix, or linear system involving the matrix operand
!   L1L     Cholesky LU decomposition, where U is just L-transpose
!   L1U     L-U decomposition of first arg, with 1's along diagonal of L and U
!   LDL     Cholesky LDU decomposition, where U is just L-transpose and D diag.
!   LDU     LDU decomposition
!   MAD     multiply first two operands, but then add result to third
!   MUL     multiply first two operands, return result as third argument
!   MSB     multiply first two operands, but then subtract result from third
!   NEG     replace operand by its negative
!   NOR     evaluate norm of operand
!   POL     polynomial (first argument) of second argument
!   POW     raise operand to some integer power
!   SUB     subtract first two operands, return result as third argument
!   SWP     swap first two operands
!   TPS     replace operand by its transpose
!   TRC     evaluate trace of operand
!   U1L     back substitution with matrix decomposed into LU form, 1's on diag.
!   UDL     back substitution with matrix decomposed into LDU form
!   WRT     write out
!   ZER     set operand to zero
!
!  OPERAND TYPES:
!   B	    banded matrix
!   C	    circulant matrix
!   D	    diagonal matrix
!   H	    symmetric or hermitian matrix
!   L	    lower triangular matrix
!   M	    matrix (rectangular, in general)
!   P	    polynomial or power-series coefficient vector
!   Q	    sQuare matrix with Fortan dimension same as logical dimension
!   R	    row of a matrix
!   S	    scalar
!   T	    transpose of the matrix
!   U	    upper triangular matrix
!   V	    vector, or column of a matrix
!   X	    field of parallel X-vectors (aligned like "columns" of a matrix)
!   Y	    field of parallel Y-vectors (aligned like "rows" of a matrix)
!
!   For those matrix routines with a "Q" in the last part of the name,
!   denoting operation upon a square matrix, the Fortran dimensions and
!   matrix order are implicitly assumed identical. If this is not the case,
!   the general matrix routines, (those without a "Q" in the name) allow
!   the arguments, MI, MJ, which denote the algebraic dimensions, to be
!   different from each other (for rectangular matrices) and different from
!   the Fortran dimensions NA, NB, etc., which are therefore listed as
!   additional parameters.
!
!------------------------------------------------------------------------------
      SUBROUTINE DZERV(D,M)    ! set elements of a vector to zero
      DIMENSION D(M)
      DO I=1,M
       D(I)=0.
      ENDDO
      RETURN
      ENTRY DNEGV(D,M)	      ! Replace vector by its negative
      DO I=1,M
       D(I)=-D(I)
      ENDDO
      RETURN
      ENTRY DCOPSD(S,D,M)      ! Copy a scalar to a diagonal matrix
      DO I=1,M
       D(I)=S
      ENDDO
      RETURN
      ENTRY DCONSD(S,D,M)      ! Copy negative of a scalar to a diagonal
      DO I=1,M
       D(I)=-S
      ENDDO
      RETURN
      END
      SUBROUTINE DZERM(A,MI,MJ,NA) ! Set the elements of general matrix to 0
      DIMENSION A(NA,*)
      DO J=1,MJ
       CALL DZERV(A(1,J),MI)
      ENDDO
      RETURN
      ENTRY DNEGM(A,MI,MJ,NA) ! Replace general matrix A by its negative
      DO J=1,MJ
       CALL DNEGV(A(1,J),MI)
      ENDDO
      RETURN
      END

      SUBROUTINE DZERR(A,M,NA)
      DIMENSION A(NA,*)
      DO I=1,M
       A(1,I)=0.
      ENDDO
      RETURN
      END

      SUBROUTINE DCOPR(A,B,M,NA,NB)
      DIMENSION A(NA,M),B(NB,M)
      DO I=1,M
       B(1,I)=A(1,I)
      ENDDO
      RETURN
      ENTRY	 DCONR(A,B,M,NA,NB)
      DO I=1,M
       B(1,I)=-A(1,I)
      ENDDO
      RETURN
      END

      SUBROUTINE DMULVS(D,S,E,M)
      DIMENSION D(*),E(*)
      DO I=1,M
       E(I)=D(I)*S
      ENDDO
      RETURN
      ENTRY DMADVS(D,S,E,M)
      DO I=1,M
       E(I)=E(I)+D(I)*S
      ENDDO
      RETURN
      ENTRY DMSBVS(D,S,E,M)
      DO I=1,M
       E(I)=E(I)-D(I)*S
      ENDDO
      RETURN
      END

      SUBROUTINE DMULRS(A,S,B,M,NA,NB)
      DIMENSION A(NA,*),B(NB,*)
      DO I=1,M
       B(1,I)=A(1,I)*S
      ENDDO
      RETURN
      ENTRY DMADRS(A,S,B,M,NA,NB)
      DO I=1,M
       B(1,I)=B(1,I)+A(1,I)*S
      ENDDO
      RETURN
      ENTRY DMSBRS(A,S,B,M,NA,NB)
      DO I=1,M
       B(1,I)=B(1,I)-A(1,I)*S
      ENDDO
      RETURN
      END

      FUNCTION DDOT(D,E,M)
      DIMENSION D(M),E(M)
      DDOT=0.
      DO I=1,M
       DDOT=DDOT+D(I)*E(I)
      ENDDO
      RETURN
      END

      SUBROUTINE DDOTVV(D,E,S,M)
      DIMENSION D(M),E(M)
      S=0.
      ENTRY DDADVV(D,E,S,M)
      DO I=1,M
       S=S+D(I)*E(I)
      ENDDO
      RETURN
      ENTRY DDSBVV(D,E,S,M)
      DO I=1,M
       S=S-D(I)*E(I)
      ENDDO
      RETURN
      END

      SUBROUTINE DDOTVR(D,A,S,M,NA)
      DIMENSION D(M),A(NA,*)
      S=0.
      ENTRY DDADVR(D,A,S,M,NA)
      DO I=1,M
       S=S+D(I)*A(1,I)
      ENDDO
      RETURN
      ENTRY DDSBVR(D,A,S,M,NA)
      DO I=1,M
       S=S-D(I)*A(1,I)
      ENDDO
      RETURN
      END

      SUBROUTINE DDOTRR(A,B,S,M,NA,NB)
      DIMENSION A(NA,*),B(NB,*)
      S=0.
      ENTRY DDADRR(A,B,S,M,NA,NB)
      DO I=1,M
       S=S+A(1,I)*B(1,I)
      ENDDO
      RETURN
      ENTRY DDSBRR(A,B,S,M,NA,NB)
      DO I=1,M
       S=S-A(1,I)*B(1,I)
      ENDDO
      RETURN
      END

      FUNCTION DPRO333(D,E,F) ! TRIPLE PRODUCT OF 3 3-VECTORS
      DIMENSION D(3),E(3),F(3),G(3)
      CALL DCRO33(E,F,G)
      CALL DDOTVV(D,G,DPRO333,3)
      RETURN
      END

      SUBROUTINE DNORV(D,S,M) ! NORM OF VECTOR..
      DIMENSION D(M)
      S=SQRT(DDOT(D,D,M))
      RETURN
      ENTRY	 DNORQ(D,S,M)	 ! ...OF SQUARE MATRIX.
      S=SQRT(DDOT(Q,Q,M*M))
      RETURN
      END

      SUBROUTINE DNORR(A,S,M,NA)
      DIMENSION A(NA,M)
      CALL DDOTRR(A,A,S,M,NA,NA)
      S=SQRT(S)
      RETURN
      END

      SUBROUTINE DCRO33(A,B,C) ! SPECIAL CASE OF 3-DIMENSIONS: CROSS-PRODUCT
      DIMENSION A(3),B(3),C(3)
      C(1)=A(2)*B(3)-A(3)*B(2)
      C(2)=A(3)*B(1)-A(1)*B(3)
      C(3)=A(1)*B(2)-A(2)*B(1)
      RETURN
      END

      SUBROUTINE DMULVV(A,B,C,M)
      DIMENSION A(*),B(*),C(*)
      DO I=1,M
       C(I)=A(I)*B(I)
      ENDDO
      RETURN
      ENTRY DMADVV(A,B,C,M)
      DO I=1,M
       C(I)=C(I)+A(I)*B(I)
      ENDDO
      RETURN
      ENTRY DMSBVV(A,B,C,M)
      DO I=1,M
       C(I)=C(I)-A(I)*B(I)
      ENDDO
      RETURN
      ENTRY DADDVV(A,B,C,M)
      DO I=1,M
       C(I)=A(I)+B(I)
      ENDDO
      RETURN
      ENTRY DSUBVV(A,B,C,M)
      DO I=1,M
       C(I)=A(I)-B(I)
      ENDDO
      RETURN
      ENTRY DDIVVV(A,B,C,M)
      DO I=1,M
       C(I)=A(I)/B(I)
      ENDDO
      RETURN
      ENTRY DCOPV(A,B,M)
      DO I=1,M
       B(I)=A(I)
      ENDDO
      RETURN
      ENTRY DCONV(A,B,M)
      DO I=1,M
       B(I)=-A(I)
      ENDDO
      RETURN
      END

      SUBROUTINE DSWPVV(D,E,M)
      DIMENSION D(M),E(M)
      DO I=1,M
       T=D(I)
       D(I)=E(I)
       E(I)=T
      ENDDO
      RETURN
      END

      SUBROUTINE DSWPRR(A,B,M,NA,NB)	! Row swap
      DIMENSION A(NA,*),B(NB,*)
      DO J=1,M
       T=A(1,J)
       A(1,J)=B(1,J)
       B(1,J)=T
      ENDDO
      RETURN
      END

      SUBROUTINE DTPSM(A,MI,MJ,NA)  ! Transpose, in place, a general matrix
      DIMENSION A(NA,*)
      M=MAX(MI,MJ)
      IF(M.GT.NA)STOP
!     &'first array bound in DTPSM too small to allow transpose to fit'
      NAP=NA+1
      DO I=1,M-1
       IP=I+1
       CALL DSWPRR(A(IP,1),A(1,IP),M-I,NAP,NAP)
      ENDDO
      RETURN
      END

      SUBROUTINE DMULMV(A,D,E,MI,MJ,NA)
      DIMENSION A(NA,*),D(*),E(*)
      CALL DZERV(E,MJ)
      ENTRY DMADMV(A,D,E,MI,MJ,NA)
      DO J=1,MJ
       CALL DMADVS(A(1,J),D(J),E,MI)
      ENDDO
      RETURN
      ENTRY DMSBMV(A,D,E,MI,MJ,NA)
      DO J=1,MJ
       CALL DMSBVS(A(1,J),D(J),E,MI)
      ENDDO
      RETURN
      END

      SUBROUTINE DMULVM(D,A,E,MI,MJ,NA)
      DIMENSION A(NA,*),D(*),E(*)
      CALL DZERV(E,MJ)
      ENTRY DMADVM(D,A,E,MI,MJ,NA)
      DO I=1,MI
       CALL DMADRS(A,D(I),E,MJ,NA,1)
      ENDDO
      RETURN
      ENTRY DMSBVM(D,A,E,MI,MJ,NA)
      DO I=1,MI
       CALL DMSBRS(A,D(I),E,MJ,NA,1)
      ENDDO
      RETURN
      END

      SUBROUTINE DMULMM(A,B,C,MI,MJ,MK,NA,NB,NC)
      DIMENSION A(NA,*),B(NB,*),C(NC,*)
      CALL DZERM(C,MI,MK,NC)
      ENTRY DMADMM(A,B,C,MI,MJ,MK,NA,NB,NC)
      DO K=1,MK
      DO J=1,MJ
       CALL DMADVS(A(1,J),B(J,K),C(1,K),MI)
      ENDDO
      ENDDO
      RETURN
      ENTRY DMSBMM(A,B,C,MI,MJ,MK,NA,NB,NC)
      DO K=1,MK
      DO J=1,MJ
       CALL DMSBVS(A(1,J),B(J,K),C(1,K),MI)
      ENDDO
      ENDDO
      RETURN

      ENTRY DMULMT(A,B,C,MI,MJ,MK,NA,NB,NC)
      CALL DZERM(C,MI,MK,NC)
      ENTRY DMADMT(A,B,C,MI,MJ,MK,NA,NB,NC)
      DO K=1,MK
      DO J=1,MJ
       CALL DMADVS(A(1,J),B(K,J),C(1,K),MI)
      ENDDO
      ENDDO
      RETURN
      ENTRY DMSBMT(A,B,C,MI,MJ,MK,NA,NB,NC)
      DO K=1,MK
      DO J=1,MJ
       CALL DMSBVS(A(1,J),B(K,J),C(1,K),MI)
      ENDDO
      ENDDO
      RETURN

      ENTRY DMULTM(A,B,C,MI,MJ,MK,NA,NB,NC)
      CALL DZERM(C,MI,MK,NC)
      ENTRY DMADTM(A,B,C,MI,MJ,MK,NA,NB,NC)
      DO K=1,MK
      DO J=1,MJ
       CALL DMADRS(A(J,1),B(J,K),C(1,K),MI,NA,1)
      ENDDO
      ENDDO
      RETURN
      ENTRY DMSBTM(A,B,C,MI,MJ,MK,NA,NB,NC)
      DO K=1,MK
      DO J=1,MJ
       CALL DMSBRS(A(J,1),B(J,K),C(1,K),MI,NA,1)
      ENDDO
      ENDDO
      RETURN

      ENTRY DMULTT(A,B,C,MI,MJ,MK,NA,NB,NC)
      CALL DZERM(C,MI,MK,NC)
      ENTRY DMADTT(A,B,C,MI,MJ,MK,NA,NB,NC)
      DO K=1,MK
      DO J=1,MJ
       CALL DMADRS(A(J,1),B(K,J),C(1,K),MI,NA,1)
      ENDDO
      ENDDO
      RETURN
      ENTRY DMSBTT(A,B,C,MI,MJ,MK,NA,NB,NC)
      DO K=1,MK
      DO J=1,MJ
       CALL DMSBRS(A(J,1),B(K,J),C(1,K),MI,NA,1)
      ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE DADDMM(A,B,C,MI,MJ,NA,NB,NC)
      DIMENSION A(NA,*),B(NB,*),C(NC,*)
      DO J=1,MJ
      DO I=1,MI
       C(I,J)=A(I,J)+B(I,J)
      ENDDO
      ENDDO
      RETURN
      ENTRY DADDMT(A,B,C,MI,MJ,NA,NB,NC)
      DO J=1,MJ
      DO I=1,MI
       C(I,J)=A(I,J)+B(J,I)
      ENDDO
      ENDDO
      RETURN
      ENTRY DADDTM(A,B,C,MI,MJ,NA,NB,NC)
      DO J=1,MJ
      DO I=1,MI
       C(I,J)=A(J,I)+B(I,J)
      ENDDO
      ENDDO
      RETURN
      ENTRY DADDTT(A,B,C,MI,MJ,NA,NB,NC)
      DO J=1,MJ
      DO I=1,MI
       C(I,J)=A(J,I)+B(J,I)
      ENDDO
      ENDDO
      RETURN
      ENTRY DSUBMM(A,B,C,MI,MJ,NA,NB,NC)
      DO J=1,MJ
      DO I=1,MI
       C(I,J)=A(I,J)-B(I,J)
      ENDDO
      ENDDO
      RETURN
      ENTRY DSUBMT(A,B,C,MI,MJ,NA,NB,NC)
      DO J=1,MJ
      DO I=1,MI
       C(I,J)=A(I,J)-B(J,I)
      ENDDO
      ENDDO
      RETURN
      ENTRY DSUBTM(A,B,C,MI,MJ,NA,NB,NC)
      DO J=1,MJ
      DO I=1,MI
       C(I,J)=A(J,I)-B(I,J)
      ENDDO
      ENDDO
      RETURN
      ENTRY DSUBTT(A,B,C,MI,MJ,NA,NB,NC)
      DO J=1,MJ
      DO I=1,MI
       C(I,J)=A(J,I)-B(J,I)
      ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE DL1LM(A,B,MI,NA,NB) ! Cholesky, M -> L*U, U(i,j)=L(j,i)
      DIMENSION A(NA,*),B(NB,*)
      DO J=1,MI
       JM=J-1
       JP=J+1
       S=A(J,J)
       CALL DDSBRR(B(J,1),B(J,1),S,JM,NB,NB)
       IF(S.LE.0.)THEN
	PRINT'('' DL1LM detects non-positivity at diagonal index'',i2)',J
	STOP
       ENDIF
       B(J,J)=SQRT(S)
       BJJI=1./B(J,J)
       DO I=JP,MI
	S=A(I,J)
	CALL DDSBRR(B(I,1),B(J,1),S,JM,NB,NB)
	B(I,J)=S*BJJI
       ENDDO
       CALL DZERV(B(1,J),JM)
      ENDDO
      RETURN
      END

      SUBROUTINE DLDLM(A,B,M,NA,NB) ! M -> L*D*U, U(i,j)=L(j,i)
      DIMENSION A(NA,*),B(NB,*)
      DO J=1,M
       JM=J-1
       JP=J+1
       S=A(J,J)
       CALL DDSBVR(B(1,J),B(J,1),S,JM,NB)
       B(J,J)=S
       IF(S.EQ.0.)THEN
	PRINT'('' DLDLM detects singularity at diagonal index'',i2)',J
	STOP
       ENDIF
       BJJI=1./S
       DO I=JP,M
	S=A(I,J)
	CALL DDSBVR(B(1,J),B(I,1),S,JM,NB)
	B(J,I)=S
	B(I,J)=S*BJJI
       ENDDO
      ENDDO
      DO J=1,M
       CALL DZERV(B(1,J),J-1)
      ENDDO
      RETURN
      END

      SUBROUTINE DCOPM(A,B,MI,MJ,NA,NB)
      DIMENSION A(NA,*),B(NB,*)
      DO J=1,MJ
      DO I=1,MI
       B(I,J)=A(I,J)
      ENDDO
      ENDDO
      RETURN
      ENTRY DCONM(A,B,MI,MJ,NA,NB)
      DO J=1,MJ
      DO I=1,MI
       B(I,J)=-A(I,J)
      ENDDO
      ENDDO
      RETURN
      ENTRY DMULMS(A,SS,B,MI,MJ,NA,NB)
      DO J=1,MJ
      DO I=1,MI
       B(I,J)=A(I,J)*SS
      ENDDO
      ENDDO
      RETURN
      ENTRY DCOPT(A,B,MI,MJ,NA,NB)
      DO J=1,MJ
      DO I=1,MI
       B(I,J)=A(J,I)
      ENDDO
      ENDDO
      RETURN
      ENTRY DCONT(A,B,MI,MJ,NA,NB)
      DO J=1,MJ
      DO I=1,MI
       B(I,J)=-A(J,I)
      ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE DMULMD(A,D,B,MI,MJ,NA,NB)
      DIMENSION A(NA,*),B(NB,*),D(*)
      DO J=1,MJ
       CALL DMULVS(A(1,J),D(J),B(1,J),MI)
      ENDDO
      RETURN
      ENTRY DMULTD(A,D,B,MI,MJ,NA,NB)
      DO J=1,MJ
       CALL DMULRS(A(J,1),D(J),B(1,J),MI,NA,1)
      ENDDO
      RETURN
      ENTRY DMULDM(D,A,B,MI,MJ,NA,NB)
      DO I=1,MI
       CALL DMULRS(A(I,1),D(I),B(I,1),MJ,NA,NB)
      ENDDO
      RETURN
      ENTRY DMULDT(D,A,B,MI,MJ,NA,NB)
      DO I=1,MI
       CALL DMULRS(A(1,I),D(I),B(I,1),MJ,1,NB)
      ENDDO
      RETURN
      ENTRY DMADMD(A,D,B,MI,MJ,NA,NB)
      DO J=1,MJ
       CALL DMADVS(A(1,J),D(J),B(1,J),MI)
      ENDDO
      RETURN
      ENTRY DMADTD(A,D,B,MI,MJ,NA,NB)
      DO J=1,MJ
       CALL DMADRS(A(J,1),D(J),B(1,J),MI,NA,1)
      ENDDO
      RETURN
      ENTRY DMADDM(D,A,B,MI,MJ,NA,NB)
      DO I=1,MI
       CALL DMADRS(A(I,1),D(I),B(I,1),MJ,NA,NB)
      ENDDO
      RETURN
      ENTRY DMADDT(D,A,B,MI,MJ,NA,NB)
      DO I=1,MI
       CALL DMADRS(A(1,I),D(I),B(I,1),MJ,1,NB)
      ENDDO
      RETURN
      ENTRY DMSBMD(A,D,B,MI,MJ,NA,NB)
      DO J=1,MJ
       CALL DMSBVS(A(1,J),D(J),B(1,J),MI)
      ENDDO
      RETURN
      ENTRY DMSBTD(A,D,B,MI,MJ,NA,NB)
      DO J=1,MJ
       CALL DMSBRS(A(J,1),D(J),B(1,J),MI,NA,1)
      ENDDO
      RETURN
      ENTRY DMSBDM(D,A,B,MI,MJ,NA,NB)
      DO I=1,MI
       CALL DMSBRS(A(I,1),D(I),B(I,1),MJ,NA,NB)
      ENDDO
      RETURN
      ENTRY DMSBDT(D,A,B,MI,MJ,NA,NB)
      DO I=1,MI
       CALL DMSBRS(A(1,I),D(I),B(I,1),MJ,1,NB)
      ENDDO
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, NCEP, Washington D.C.	1996
!		    SUBROUTINE DLDUM
!  perform l-d-u decomposition of square matrix a in place with
!  IMPLICIT pivoting
!
!  --> a    square matrix to be factorized
!  <-- ipiv array encoding the pivoting sequence
!  <-- d    indicator for possible sign change of determinant
!  --> m    degree of (active part of) a
!  --> na   first fortran dimension of a
!
!   LIMITATION:
!	S is an array, internal to this routine, containing the
!    scaling factors of each row used for pivoting decisions. It is given a
!    fortran dimension of NN=500 in the parameter statement below.
!    If the order of the linear system exceeds NN, increase NN.
!------------------------------------------------------------------------------
      SUBROUTINE DLDUM(A,IPIV,D,M,NA)
      PARAMETER(NN=500)
      DIMENSION A(NA,*),IPIV(*),S(NN)
!      IF(M.GT.NN)STOP'MATRIX TOO LARGE FOR LDUM'
      IF(M.GT.NN)STOP
      DO I=1,M
       AAM=0.
       DO J=1,M
	AA=ABS(A(I,J))
	IF(AA.GT.AAM)AAM=AA
       ENDDO
       IF(AAM.EQ.0.)THEN
	PRINT'('' ROW '',I3,'' OF MATRIX IN LUFM VANISHES'')',I
	STOP
       ENDIF
       S(I)=1./AAM
      ENDDO
      D=1.
      IPIV(M)=M
      DO J=1,M-1
       JP=J+1
       ABIG=S(J)*ABS(A(J,J))
       IBIG=J
       DO I=JP,M
	AA=S(I)*ABS(A(I,J))
	IF(AA.GT.ABIG)THEN
	 IBIG=I
	 ABIG=AA
	ENDIF
       ENDDO
!  swap rows, recording changed sign of determinant
       IPIV(J)=IBIG
       IF(IBIG.NE.J)THEN
	D=-D
	DO K=1,M
	 T=A(J,K)
	 A(J,K)=A(IBIG,K)
	 A(IBIG,K)=T
	ENDDO
	S(IBIG)=S(J)
       ENDIF
       AJJ=A(J,J)
       IF(AJJ.EQ.0.)THEN
		   JM=J-1
 PRINT'('' FAILURE IN LDUM:''/'' MATRIX SINGULAR, RANK='',i3)',JM
			   STOP
       ENDIF
       AJJI=1./AJJ
       DO I=JP,M
	AIJ=AJJI*A(I,J)
	A(I,J)=AIJ
	DO K=JP,M
	 A(I,K)=A(I,K)-AIJ*A(J,K)
	ENDDO
       ENDDO
      ENDDO
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1993
!		    SUBROUTINE DUDLMM
!  use l-u factors in a to back-substitute for mm rhs in b, using ipiv to
!  define the pivoting permutation used in the l-u decomposition.
!
!  --> A    L-D-U factorization of linear system matrux
!  <-> B    right-hand-sides on entry, corresponding matrix of solution
!	    vectors on return
!  --> IPIV array encoding the pivoting sequence
!  --> M    degree of (active part of) B and A
!  --> MM   number of right-hand-side vectors (active columns of B)
!  --> NA   first fortran dimension of A
!  --> NB   first fortran dimension of B
!------------------------------------------------------------------------------
      SUBROUTINE DUDLMM(A,B,IPIV,M,MM,NA,NB)
      DIMENSION A(NA,*),B(NB,*),IPIV(*)
      DO K=1,MM !loop over columns of B
       DO I=1,M
	L=IPIV(I)
	S=B(L,K)
	B(L,K)=B(I,K)
	CALL DDSBVR(B(1,K),A(I,1),S,I-1,NA)
	B(I,K)=S
       ENDDO
       B(M,K)=B(M,K)/A(M,M)
       DO I=M-1,1,-1
	AIII=1./A(I,I)
	CALL DDSBVR(B(I+1,K),A(I,I+1),B(I,K),M-I,NA)
	B(I,K)=B(I,K)*AIII
       ENDDO
      ENDDO
      RETURN
      END
!			  WD23JP
!							  *****************
!							  *    MAT4.FOR	  *
!							  *  PURSER 1996  *
!							  *****************
!
      SUBROUTINE DINVM(B,A,M,NB,NA) ! copy matrix B to A, in-place invert A.
      PARAMETER (NN=500)
      DIMENSION IPIV(NN)
      DIMENSION A(NA,*),B(NB,*)
      CALL DCOPM(B,A,M,M,NB,NA)
      CALL DLDUM(A,IPIV,D,M,NA)

!  INVERT U IN PLACE:
      DO I=1,M
       A(I,I)=1./A(I,I)
      ENDDO
      DO I=1,M-1
       DO J=I+1,M
	CALL DDOTVR(A(I,J),A(I,I),S,J-I,NA)
	A(I,J)=-A(J,J)*S
       ENDDO
      ENDDO

!  INVERT L IN PLACE ASSUMING IMPLICITLY DIAGONAL ELEMENTS OF UNITY
      DO J=1,M-1
       JP=J+1
       DO I=J+1,M
	CALL DDADVR(A(JP,J),A(I,JP),A(I,J),I-JP,NA)
	A(I,J)=-A(I,J)
       ENDDO
      ENDDO

!  FORM THE PRODUCT OF U**-1 AND L**-1 IN PLACE
      DO J=1,M-1
       JP=J+1
       DO I=1,J
	CALL DDADVR(A(JP,J),A(I,JP),A(I,J),M-J,NA)
       ENDDO
       DO I=JP,M
	CALL DDOTVR(A(I,J),A(I,I),S,M+1-I,NA)
	A(I,J)=S
       ENDDO
      ENDDO

!  PERMUTE COLUMNS ACCORDING TO IPIV
      DO J=M-1,1,-1
       L=IPIV(J)
       CALL DSWPVV(A(1,J),A(1,L),M)
      ENDDO
      RETURN
!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1993
!		    ENTRY DLINMM
!  invert linear systems sharing the same square system matrix
!
!  <-> A    square system matrix on entry, its L-D-U factorization on return
!  <-> B    right-hand-sides on entry, corresponding matrix of solution
!	    vectors on return
!  --> M    degree of (active part of) B and A
!  --> MM   number of right-hand-side vectors (active columns of b)
!  --> NA   first fortran dimension of A
!  --> NB   first fortran dimension of B
!------------------------------------------------------------------------------
      ENTRY			       DLINMM(a,b,m,mm,na,nb)
      CALL DLDUM(A,IPIV,D,M,NA)
      CALL DUDLMM(A,B,IPIV,M,MM,NA,NB)
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1993
!		    SUBROUTINE DINVH
!  Invert, possibly in place, a symmetric matrix
!
!  --> B    symmetric square input matrix
!  <-- A    inverse of B
!  --> M    degree of (active part of) A and B
!  --> NB   first fortran dimension of B
!  --> NA   first fortran dimension of A
!
!  LIMITATION
!     This routine incorporates no pivoting - it is intended for matrices
!     that are already diagonally dominant
!------------------------------------------------------------------------------
      SUBROUTINE DINVH(B,A,M,NB,NA)
      DIMENSION B(NB,*),A(NA,*)

!  PERFORM L.D.U DECOMPOSITION OF THE SYMMETRIC MATRIX:
      CALL DLDLM(B,A,M,NB,NA)

!  INVERT (IN PLACE) THE LOWER TRIANGULAR PART OF A, (ASSUMING UNIT
!  DIAGONAL ELEMENTS), AND INVERT THE DIAGONAL PART OF A (ASSUMING
!  ZERO OFF-DIAGONAL ELEMENTS). PUT TRANSPOSE OF LOWER, TIMES DIAGONAL,
!  INTO UPPER PART OF A.
      DO K=1,M
       KP=K+1
       A(K,K)=1./A(K,K)
       DO I=KP,M
	CALL DDADVR(A(KP,K),A(I,KP),A(I,K),I-KP,NA)
	A(I,K)=-A(I,K)
       ENDDO
      ENDDO

!  MULTIPLY: THE TRANSPOSE OF THE LOWER PART OF A (ASSUMING UNIT DIAGS),
!  TIMES THE DIAGONAL PART (ASSUMING ZERO OFF-DIAGS), TIMES THE LOWER
!  PART. THIS PRODUCT IS THE SYMMETRIC INVERSE OF THE ORIGINAL B.
      DO I=2,M
       CALL DMULRS(A(I,1),A(I,I),A(1,I),I-1,NA,1)
      ENDDO
      DO I=1,M
       IP=I+1
       DO J=1,I-1
	CALL DDADVR(A(IP,I),A(J,IP),A(J,I),M-I,NA)
	A(I,J)=A(J,I)
       ENDDO
       CALL DDADVR(A(IP,I),A(I,IP),A(I,I),M-I,NA)
      ENDDO

      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE DINVL
!     Invert lower triangular matrix, possibly in place if A and B are same
!------------------------------------------------------------------------------
      SUBROUTINE DINVL(A,B,M,NA,NB)
      DIMENSION A(NA,*),B(NB,*)
      DO J=M,1,-1
       CALL DZERV(B(1,J),J-1)
       B(J,J)=1./A(J,J)
       DO I=J+1,M
	IM=I-1
	CALL DDOTVR(B(J,J),A(I,J),S,I-J,NA)
	B(I,J)=-B(I,I)*S
       ENDDO
      ENDDO
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE DLINLV
!     Solve linear system involving lower triangular (LINLV) or upper
!     triangular (LINUV) matrix, right-hand-side vector U and output vector V
!
!------------------------------------------------------------------------------
      SUBROUTINE DLINLV(A,U,V,M,NA)
      DIMENSION A(NA,*),U(*),V(*)
      DO I=1,M
       S=U(I)
       CALL DDSBVR(V,A(I,1),S,I-1,NA)
       V(I)=S/A(I,I)
      ENDDO
      RETURN
      ENTRY DLINUV(A,U,V,M,NA)
      DO J=M,1,-1
       JP=J+1
       S=U(J)
       CALL DDSBVV(A(JP,J),V(JP),S,M-J)
       V(J)=S/A(J,J)
      ENDDO
      RETURN
      END

      FUNCTION MCMAXD(A,B,M)
      DIMENSION A(0:M),B(0:M)
      MCMAXD=0		       ! Default for when ALL elements of C are zero
      DO MA=M,0,-1	       ! Seek last nonzero coefficient of polynomial A
       IF(A(MA).NE.0.)THEN
	DO MB=M,0,-1	       ! Seek last nonzero coefficient of polynomial B
	 IF(B(MB).NE.0.)THEN
	  MCMAXD=MIN(M,MA+MB)+1 ! Hence, 1+last non-0 element of their product
	  RETURN
	 ENDIF
	ENDDO
	RETURN
       ENDIF
      ENDDO
      RETURN
      END

      SUBROUTINE DMULPP(A,B,C,M) !  multiply polynomials, possibly in place
      DIMENSION A(0:M),B(0:M),C(0:M)
      MCP=MCMAXD(A,B,M)
      DO I=MCP,M
       C(I)=0.
      ENDDO
      DO J=MCP,1,-1
       CALL DFDOVV(A,B,S,J)
       C(J-1)=S
      ENDDO
      RETURN
      ENTRY DMADPP(A,B,C,M)
      MCP=MCMAXD(A,B,M)
      DO J=MCP,1,-1
       CALL DFDOVV(A,B,S,J)
       C(J-1)=C(J-1)+S
      ENDDO
      RETURN
      ENTRY DMSBPP(A,B,C,M)
      MCP=MCMAXD(A,B,M)
      DO J=MCP,1,-1
       CALL DFDOVV(A,B,S,J)
       C(J-1)=C(J-1)-S
      ENDDO
      RETURN
      ENTRY DDIFP(A,B,M) ! Symbolically differentiate polynomial
      DO I=1,M		! possibly with coincident storage for A and B
       B(I-1)=I*A(I)
      ENDDO
      B(M)=0.
      RETURN
      ENTRY DINTP(A,B,M) ! Symbolically integrate polynomial
      DO I=M,1,-1	! possibly with coincident storage for A and B
       B(I)=A(I-1)/I
      ENDDO
      B(0)=0.
      RETURN
      ENTRY DINVP(A,B,M)  ! Invert polynomial or power-series
      B0=1./A(0)	 ! Storage of A and B must NOT be the same
      B(0)=B0
      DO I=1,M
       CALL DFDOVV(B,A(1),S,I)
       B(I)=-B0*S
      ENDDO
      RETURN
      END

      SUBROUTINE DPRGV(D,M)
#if ( DWORDSIZE != RWORDSIZE )
      PARAMETER(CRIT=1.E-30)
#else
      PARAMETER(CRIT=1.E-60)
#endif
      DIMENSION D(*)
      DO I=1,M
       IF(ABS(D(I)).LE.CRIT)D(I)=0.
      ENDDO
      RETURN
      END

      SUBROUTINE DFDOVV(D,E,S,M) ! Folded dot-product
      DIMENSION D(*),E(*)
      S=0.
      ENTRY DFDAVV(D,E,S,M)
      MP=M+1
      DO I=1,M
       S=S+D(MP-I)*E(I)
      ENDDO
      RETURN
      ENTRY DFDSVV(D,E,S,M)
      MP=M+1
      DO I=1,M
       S=S-D(MP-I)*E(I)
      ENDDO
      RETURN
      END
      SUBROUTINE DFDORR(A,B,S,M,NA,NB)
      DIMENSION A(NA,M),B(NB,M)
      S=0.
      ENTRY	 DFDARR(A,B,S,M,NA,NB)
      MP=M+1
      DO I=1,M
       S=S+A(1,MP-I)*B(1,I)
      ENDDO
      RETURN
      ENTRY	 DFDSRR(A,B,S,M,NA,NB)
      MP=M+1
      DO I=1,M
       S=S-A(1,MP-I)*B(1,I)
      ENDDO
      RETURN
      END

      SUBROUTINE DPOWP(A,B,N,M)	       ! Raise power series A to the power
      DIMENSION A(0:M),B(0:M),C(0:M)   ! of N and output as B
      B(0)=1.
      CALL DZERV(B(1),M)
      DO K=1,N
       CALL DMULPP(A,B,B,M)
      ENDDO
      RETURN
      ENTRY DPOLPS(A,S1,S2,M) ! Apply series A to scalar S1 to obtain S2
      S2=A(M)
      DO K=M-1,0,-1
       S2=S2*S1+A(K)
      ENDDO
      RETURN
      ENTRY DPOLPP(A,B,C,M) ! Apply power series A to power series B and put
      C(0)=A(M) 	   ! the result out as power-series C.
      CALL DZERV(C(1),M)
      DO K=M-1,0,-1
       CALL DMULPP(B,C,C,M)
       C(0)=C(0)+A(K)
      ENDDO
      RETURN
      END

      SUBROUTINE DMULCC(A,B,C,M)  ! Multiply circulant matrices of period M
      DIMENSION A(0:M-1),B(0:M-1),C(0:M-1)
      CALL DZERV(C,M)
      ENTRY DMADCC(A,B,C,M)
      MM=M-1
      DO J=0,MM
       MMJ=M-J
       CALL DMADVS(A,B(J),C(J),MMJ)
       CALL DMADVS(A(MMJ),B(J),C,J)
      ENDDO
      RETURN
      ENTRY DMSBCC(A,B,C,M)
      MM=M-1
      DO J=0,MM
       MMJ=M-J
       CALL DMSBVS(A,B(J),C(J),MMJ)
       CALL DMSBVS(A(MMJ),B(J),C,J)
      ENDDO
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!   wd23jp@sun1.wwb.noaa.gov					      1996
!		    SUBROUTINE DLINVAN
!
!   Take square matrix W and seek row and column scalings to produce non-
!   vanishing elements of rescaled W having magnitudes as close to unity
!   as possible. The approach is make the geometric mean of the nonvanishing
!   elements of each row and of each column +1 or -1. Having rescaled the
!   matrix and the r.h.s. vector AB, compute the product P of row-vector
!   norms, then compute the determinant D and solve the linear system.
!   Rescale the solution vector (now AB) and put the conditioning indicator
!   formed by the ratio D/P into the first element of W.
!
! <-> W:    Generalized Vandermonde matrix in, conditioning indicator out.
! <-> AB:   R.h.s. vector in, solution vector of numerical coefficients out.
! --> NC:   Order of the linear problem.
!------------------------------------------------------------------------------
      SUBROUTINE DLINVAN(W,AB,NC)
      PARAMETER(NN=20,NIT=20)
      DIMENSION W(NC,*),AB(NC),D1(NN),D2(NN),IPIV(NN)&
      ,W2(NN,NN),V(NN)
      CALL DCOPM(W,W2,NC,NC,NC,NN) ! Preserve original W and AB for use
      CALL DCOPV(AB,V,NC)	  ! in later "clean-up" operation.

      DO I=1,NC
       D1(I)=1. 	    !	Row scaling factors set to default
       D2(I)=1. 	    !	Column scaling factors set to default
      ENDDO
      C=1.D-16	! Set initial criterion for "negligible" elements of W

! In first attempt to estimate row and column scalings, use logarithms
! to avoid the risk of under- or over-flows of the line products of W:
      DO I=1,NC
       P=0.
       E=0.
       DO J=1,NC
	DW=ABS(W(I,J))
	IF(DW.GT.C)THEN
	  E=E+1.
	  P=P+LOG(DW)
	ENDIF
       ENDDO
!      IF(E.EQ.0.)STOP'W effectively singular in LINVAN'
       IF(E.EQ.0.)STOP
       D1(I)=EXP(-P/E)
      ENDDO
      CALL DMULVX(D1,W2,W,NC,NC,NN,NC) ! Rescale rows of W by D1

      DO J=1,NC
       P=0.
       E=0.
       DO I=1,NC
	DW=ABS(W(I,J))
	IF(DW.GT.C)THEN
	  E=E+1.
	  P=P+LOG(DW)
	ENDIF
       ENDDO
!       IF(E.EQ.0.)STOP'W effectively singular in LINVAN'
       IF(E.EQ.0.)STOP
       D2(J)=EXP(-P/E)
      ENDDO
      CALL DMULVY(D2,W,W,NC,NC,NC,NC) ! Rescale columns of W by D2

      C=1.D-8  ! reset the criterion for "negligible" elements

! Revert to iterations of the more efficient method without logarithms:
      DO JT=1,2
      DO IT=1,NIT	    !	Perform NIT relaxation iterations
       DO I=1,NC	    !	Do rows:
	P=1.
	E=0.
	DO J=1,NC
	 DW=ABS(W(I,J))
	 IF(DW.GT.C)THEN
	  E=E+1.
	  P=P*DW
	 ENDIF
	ENDDO
	P=1./(P**(1./E))
	CALL DMULRS(W(I,1),P,W(I,1),NC,NC,NC) ! Rescale this row of W..
	D1(I)=D1(I)*P			     ! ..and update D1 consistently
       ENDDO
       DO J=1,NC	    !	Do columns:
	P=1.
	E=0.
	D2J=D2(J)
	DO I=1,NC
	 DW=ABS(W(I,J))
	 IF(DW.GT.C)THEN
	  E=E+1.
	  P=P*DW
	 ENDIF
	ENDDO
	P=1./(P**(1./E))
	CALL DMULVS(W(1,J),P,W(1,J),NC) ! Rescale this column of W..
	D2(J)=D2(J)*P		       ! ..and update D2 consistently
       ENDDO
      ENDDO
      C=1.D-3	    ! Final setting for criterion for "negligible" elements
      ENDDO
      CALL DMULVV(D1,AB,AB,NC)	     ! Rescale r.h.s vector by D1
      P=1.			     ! P becomes product of row-lengths:
      DO I=1,NC
       CALL DNORR(W(I,1),S,NC,NC)
       P=P*S
      ENDDO
      CALL DLDUM(W,IPIV,D,NC,NC)
      DO I=1,NC
       D=D*W(I,I)		      ! D becomes the determinant of W
      ENDDO
      CALL DUDLMM(W,AB,IPIV,NC,1,NC,NC)
      CALL DMULVV(AB,D2,AB,NC)	      ! Rescale solution vector by D2
!  Note: it is very likely that round-off errors have accumulated during
!  the iterative rescaling of W. We invoke original matrix elements W2 and
!  substitute the tentative solution vector into the original (unscaled)
!  equation in order to estimate the residual components of roundoff error.

!  Begin "clean-up" process. Substitute solution vector in original
!  equation and leave the residual difference in V
      CALL DMSBMM(W2,AB,V,NC,NC,1,NN,NC,NC)
      CALL DMULVV(V,D1,V,NC)	      ! Rescale the residual vector by D1
      CALL DUDLMM(W,V,IPIV,NC,1,NC,NC) ! Solve linear system with THIS rhs.
      CALL DMADVV(V,D2,AB,NC)  ! Add residual solution vector, scaled, to AB
			      ! This will remove most of the round-off error.
      W(1,1)=D/P  ! this ratio is an indicator of the overall conditioning
      RETURN	  ! When D/P is very small, treat the results with suspicion!
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!   wd23jp@sun1.wwb.noaa.gov					      1996
!		    SUBROUTINE DTSINI
!!!!!!!!!!!! THIS IS PROBABLY OBSOLETE - SEE FUNCTION SINMINT !!!!!
!
!  Initialize the Taylor coefficients used by DFUNTS to evaluate the
!  integrals of integer powers of the sine function near the origin.
!	 NI is the maximum number of non-trivial coefficients available
!	 NK is the largest power of the sine function accommodated
!------------------------------------------------------------------------------
      SUBROUTINE DTSINI
      PARAMETER(NI=30,NK=20)
      COMMON/DTSCOF/ TS(0:NI,NK)
!  SET UP TAYLOR SERIES OF SIN(X)**K.....
#if ( DWORDSIZE != RWORDSIZE )
      PARAMETER(CRIT=1.E-30)
#else
      PARAMETER(CRIT=1.E-60)
#endif
      T=1
      TS(0,1)=T
      KSIG=-1
      DO I=1,NI
       IF(ABS(T).LE.CRIT)THEN
	TS(I,1)=0
       ELSE
	I2=I*2
	T=-T/(I2*(I2+1))
	TS(I,1)=T
       ENDIF
      ENDDO
      DO K=2,NK
       CALL DMULPP(TS(0,1),TS(0,K-1),TS(0,K),NI)
      ENDDO
!  ...AND INTEGRATE:
      DO K=1,NK
       DO I=0,NI
	TS(I,K)=TS(I,K)/(I*2+K+1)
       ENDDO
      ENDDO
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!   wd23jp@sun1.wwb.noaa.gov					      1996
!		    FUNCTION DFUNTS
!!!!!!!!!!!!!!!! THIS FUNCTION IS PROBABLY OBSOLETE - SEE FUNCTION SINMINT
!   Evaluate the integral of sin(A)**K using a power series in A**2
!------------------------------------------------------------------------------
      FUNCTION DFUNTS(A,K)
      PARAMETER(NI=30,NK=20)
      COMMON/DTSCOF/ TS(0:NI,NK)
#if ( DWORDSIZE != RWORDSIZE )
      PARAMETER(CRIT=1.E-30)
#else
      PARAMETER(CRIT=1.E-60)
#endif
      IF(K.EQ.0)THEN
       DFUNTS=A
      ELSE
       DFUNTS=0.
       AP=A**(K+1)
       AS=A*A
       DO I=0,NI
	IF(ABS(AP).LE.CRIT)RETURN
	DFUNTS=DFUNTS+AP*TS(I,K)
	AP=AP*AS
       ENDDO
      ENDIF
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!   wd23jp@sun1.wwb.noaa.gov					      1996
!		    SUBROUTINE DCNVLV
!  Convolve two series in such a way that the output can, if necessary,
!  occupy the same space as one of the inputs and with a check to ensure that
!  extremely small products are reset to zero. SINGLE	PRECISION of DCNVLV
!
! ?-> A:   first input series (possibly overwritten as "C" on output)
! ?-> B:   second input (possibly overwritten as "C" on output)
! <-- C:   output series
! --> N:   maximum index of the series whose first index is zero
!------------------------------------------------------------------------------
!      SUBROUTINE DCNVLV(A,B,C,N)
!      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!      DIMENSION A(0:N),B(0:N),C(0:N)
!      DO IA=N,0,-1
!	NA=IA
!	IF(A(IA).NE.0.)GOTO 300
!      ENDDO
!300   DO IB=N,0,-1
!	NB=IB
!	IF(B(IB).NE.0.)GOTO 301
!      ENDDO
!301   DO IC=0,N
!	C(IC)=0
!      ENDDO
!      NC=MIN(N,NA+NB)
!      DO IA=0,NA
!	AIA=A(IA)
!	IF(AIA.NE.0.)THEN
!	DO IB=0,MIN(NC-IA,NB)
!	 IC=IA+IB
!	 C(IC)=C(IC)+AIA*B(IB)
!	ENDDO
!	ENDIF
!      ENDDO
!      DO IC=0,NC
!	IF(ABS(C(IC)).LE.1.D-60)C(IC)=0.
!      ENDDO
!      RETURN
!      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!   wd23jp@sun1.wwb.noaa.gov					      1996
!		    SUBROUTINE DDFCO
!
!  Compute one row of the coefficients for either the compact differencing or
!  quadrature scheme characterized by matrix equation of the form,
!			 A.d = B.c			       (*)
!  In either case, d is the derivative of c.
!
! --> ZA:   coordinates of d-points used in this row of (*)
! --> ZB:   coordinates of c-points used in this row of (*)
! --> Z0:   nominal point of application of this row of (*)
! --> NA:   number of d-points operated on by this row of the A of (*)
! --> NB:   number of c-points operated on by this row of the B of (*)
! <-- AB:   the NC=NA+NB concatenated coefficients A and B for this row
! <-- W:    work-array of size NC**2. On exit, the first element contains
!	    a measure of the conditioning of the matrix of the linear system
!	    (determinant/{product of row-lengths}).
!------------------------------------------------------------------------------
      SUBROUTINE DDFCO(ZA,ZB,Z0,NA,NB,AB,W)
      DIMENSION ZA(*),ZB(*),AB(*),W(NA+NB,*)
      NC=NA+NB
      DO J=1,NA
       W(1,J)=1.
       W(2,J)=0.
       W(3,J)=0.
       Z=ZA(J)-Z0
       P=Z
       DO I=4,NC
	W(I,J)=P*(I-2)
	P=P*Z
       ENDDO
      ENDDO
      DO J=1,NB
       W(1,NA+J)=0.
       Z=ZB(J)-Z0
       P=-1.
       DO I=2,NC
	W(I,NA+J)=P
	P=P*Z
       ENDDO
      ENDDO
      AB(1)=1.
      DO I=2,NC
       AB(I)=0.
      ENDDO
      AB(3)=-1.
      CALL DLINVAN(W,AB,NC) ! Solve the linear system
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!   wd23jp@sun1.wwb.noaa.gov					      1996
!		    ENTRY      DAVCO
!
!  Compute one row of the coefficients for the compact mid-interval
!  interpolation scheme characterized by matrix equation of the form,
!			 A.t = B.s			       (*)
!  Where s is the vector of "source" values, t the staggered "target" values.
!
! --> ZA:   coordinates of t-points used in this row of (*)
! --> ZB:   coordinates of s-points used in this row of (*)
! --> Z0:   nominal point of application of this row of (*)
! --> NA:   number of t-points operated on by this row of the A of (*)
! --> NB:   number of s-points operated on by this row of the B of (*)
! <-- AB:   the NC=NA+NB concatenated coefficients A and B for this row
! <-- W:    work-array of size NC**2. On exit, the first element contains
!	    a measure of the conditioning of the matrix of the linear system
!	    (determinant/{product of row-lengths}).
!------------------------------------------------------------------------------
      SUBROUTINE DAVCO(ZA,ZB,Z0,NA,NB,AB,W)
      DIMENSION ZA(*),ZB(*),AB(*),W(NA+NB,*)
      NC=NA+NB
      DO J=1,NA
       W(1,J)=1.
       Z=ZA(J)-Z0
       P=1.
       DO I=2,NC
	W(I,J)=P
	P=P*Z
       ENDDO
      ENDDO
      DO J=1,NB
       W(1,NA+J)=0.
       Z=ZB(J)-Z0
       P=-1.
       DO I=2,NC
	W(I,NA+J)=P
	P=P*Z
       ENDDO
      ENDDO
      AB(1)=1.
      DO I=2,NC
       AB(I)=0.
      ENDDO
      CALL DLINVAN(W,AB,NC) ! Solve the linear system
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!   wd23jp@sun1.wwb.noaa.gov					      1996
!		    ENTRY      DDFCO2
!
!  Compute one row of the coefficients for either the compact second-
!  differencing scheme characterized by matrix equation of the form,
!			 A.d = B.c			       (*)
!  Where d is the second-derivative of c.
!
! --> ZA:   coordinates of d-points used in this row of (*)
! --> ZB:   coordinates of c-points used in this row of (*)
! --> Z0:   nominal point of application of this row of (*)
! --> NA:   number of d-points operated on by this row of the A of (*)
! --> NB:   number of c-points operated on by this row of the B of (*)
! <-- AB:   the NC=NA+NB concatenated coefficients A and B for this row
! <-- W:    work-array of size NC**2. On exit, the first element contains
!	    a measure of the conditioning of the matrix of the linear system
!	    (determinant/{product of row-lengths}).
!------------------------------------------------------------------------------
      SUBROUTINE DDFCO2(ZA,ZB,Z0,NA,NB,AB,W)
      DIMENSION ZA(*),ZB(*),AB(*),W(NA+NB,*)
      NC=NA+NB
      DO J=1,NA
       W(1,J)=1.
       W(2,J)=0.
       W(3,J)=0.
       W(4,J)=0.
       Z=ZA(J)-Z0
       P=Z
       DO I=5,NC
	W(I,J)=P*(I-2)*(I-3)
	P=P*Z
       ENDDO
      ENDDO
      DO J=1,NB
       W(1,NA+J)=0.
       Z=ZB(J)-Z0
       P=-1.
       DO I=2,NC
	W(I,NA+J)=P
	P=P*Z
       ENDDO
      ENDDO
      AB(1)=1.
      DO I=2,NC
       AB(I)=0.
      ENDDO
      AB(4)=-2.
      CALL DLINVAN(W,AB,NC) ! Solve the linear system
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Centers for Environmental Prediction, Washington D.C.
!   wd23jp@sun1.wwb.noaa.gov					      1996
!		    SUBROUTINE DYQDCO
!
!  Compute one row of the coefficients for the special compact differencing or
!  quadrature scheme characterized by matrix equation of the form,
!			 A.d = B.c			       (*)
!!!!!!!! THIS ROUTINE IS PROBABLY OBSOLETE !!!!!!!!!!!!!
!!!!	 SEE SUBROUTINE YQDBB
!
! --> YA:   latitudes of d-points used in this row of (*)
! --> YB:   latitudes of c-points used in this row of (*)
! --> Y0:   nominal point of application of this row of (*)
! --> NA:   number of d-points operated on by this row of the A of (*)
! --> NB:   number of c-points operated on by this row of the B of (*)
! <-- AB:   the NC=NA+NB concatenated coefficients A and B for this row
! <-- W:    work-array of size NC**2. On exit, the first element contains
!	    a measure of the conditioning of the matrix of the linear system
!	    (determinant/{product of row-lengths}).
!------------------------------------------------------------------------------
!      SUBROUTINE DYQDCO(YA,YB,Y0,NA,NB,AB,W)
!      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!      LOGICAL LFIRST
!      SAVE
!      DIMENSION YA(*),YB(*),AB(*),W(NA+NB,*)
!      DATA LFIRST/.TRUE./
!
!!  IF THIS IS THE FIRST INVOKATION, INITIALIZE TAYLOR COEFs USED BY DFUNTS
!      IF(LFIRST)THEN
!	CALL DTSINI
!	LFIRST=.FALSE.
!      ENDIF
!
!      NC=NA+NB
!      CY0=COS(Y0)
!      SY0=SIN(Y0)
!      DO J=1,NA      ! Begin constructing generalized Vandermonde matrix
!	W(1,J)=1.     ! associated with this row of this particular
!	W(2,J)=0.     ! compact finite differencing scheme.
!	W(3,J)=0.     ! First three rows are mandatory.
!	YR=YA(J)-Y0
!	Z=SIN(YR)
!	P=Z
!	DO I=4,NC
!	W(I,J)=P
!	P=P*Z
!	ENDDO
!      ENDDO
!      DO J=1,NB
!	W(1,NA+J)=0.
!	W(2,NA+J)=-1.
!	YR=YB(J)-Y0
!	Z=SIN(YR)
!	DO I=3,NC
!	M=I-2
!	W(I,NA+J)=SY0*DFUNTS(YR,M)-CY0*Z**M/M
!	ENDDO
!      ENDDO
!      AB(1)=1.	    !  Construct the right-hand-side vector
!      DO I=2,NC     !	for this problem
!	AB(I)=0.
!      ENDDO
!      AB(3)=-1.
!      CALL DLINVAN(W,AB,NC) ! Solve the linear system.
!      RETURN
!      END

      SUBROUTINE DMULVX(D,V1,V2,M,MY,NV1,NV2)
      DIMENSION D(M),V1(NV1,MY),V2(NV2,MY)
      DO I=1,M
       DI=D(I)
       DO IY=1,MY
	V2(I,IY)=DI*V1(I,IY)
       ENDDO
      ENDDO
      RETURN
      ENTRY	 DMADVX(D,V1,V2,M,MY,NV1,NV2)
      DO I=1,M
       DI=D(I)
       DO IY=1,MY
	V2(I,IY)=V2(I,IY)+DI*V1(I,IY)
       ENDDO
      ENDDO
      RETURN
      ENTRY	 DMSBVX(D,V1,V2,M,MY,NV1,NV2)
      DO I=1,M
       DI=D(I)
       DO IY=1,MY
	V2(I,IY)=V2(I,IY)-DI*V1(I,IY)
       ENDDO
      ENDDO
      RETURN
      END

      SUBROUTINE DMULVY(D,V1,V2,M,MX,NV1,NV2)
      DIMENSION D(M),V1(NV1,M),V2(NV2,M)
      DO I=1,M
       DI=D(I)
       DO IX=1,MX
	V2(IX,I)=DI*V1(IX,I)
       ENDDO
      ENDDO
      RETURN
      ENTRY	 DMADVY(D,V1,V2,M,MX,NV1,NV2)
      DO I=1,M
       DI=D(I)
       DO IX=1,MX
	V2(IX,I)=V2(IX,I)+DI*V1(IX,I)
       ENDDO
      ENDDO
      RETURN
      ENTRY	 DMSBVY(D,V1,V2,M,MX,NV1,NV2)
      DO I=1,M
       DI=D(I)
       DO IX=1,MX
	V2(IX,I)=V2(IX,I)-DI*V1(IX,I)
       ENDDO
      ENDDO
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE DLDUB
!  Compute [L]*[D**-1]*[U] decomposition of asymmetric band-matrix
!
! <-> A input as the asymmetric band matrix. On output, it contains
!     the [L]*[D**-1]*[U] factorization of the input matrix, where
!     [L] is lower triangular with unit main diagonal
!     [D] is a diagonal matrix
!     [U] is upper triangular with unit main diagonal
! --> M the number of rows assumed for [A]
! --> MAH1 the left half-bandwidth of fortran array A
! --> MAH2 the right half-bandwidth of fortran array A
! --> NA the first fortran dimension of A
!------------------------------------------------------------------------------
      SUBROUTINE DLDUB(A,M,MAH1,MAH2,NA)
      DIMENSION A(NA,-MAH1:MAH2)
      DO J=1,M
       IMOST=MIN(M,J+MAH1)
       JMOST=MIN(M,J+MAH2)
       JP=J+1
       AJJ=A(J,0)
       IF(AJJ.EQ.0.)THEN
 PRINT'('' Failure in LDUB:''/'' Matrix requires pivoting or is singular'')'
	STOP
       ENDIF
       AJJI=1./AJJ
       A(J,0)=AJJI
       DO I=JP,IMOST
	AIJ=AJJI*A(I,J-I)
	A(I,J-I)=AIJ
	DO K=JP,JMOST
	 A(I,K-I)=A(I,K-I)-AIJ*A(J,K-J)
	ENDDO
       ENDDO
       DO K=JP,JMOST
	A(J,K-J)=AJJI*A(J,K-J)
       ENDDO
      ENDDO
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1996
!		    SUBROUTINE DL1UBB
!  Form the [L]*[D]*[U] decomposition of asymmetric band-matrix  [A] replace
!  lower triangular elements of [A] by [D**-1]*[L]*[D], the upper by [U],
!  replace matrix [B] by [D**-1]*[B].
!
! <-> A input as band matrix, output as lower and upper triangulars with 1s
!     implicitly assumed to lie on the main diagonal. The product of these
!     triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
! <-> B in as band matrix, out as same but premultiplied by diagonal [D**-1]
! --> M number of rows of A and B
! --> MAH1 left half-width of fortran array A
! --> MAH2 right half-width of fortran array A
! --> MBH1 left half-width of fortran array B
! --> MBH2 right half-width of fortran array B
! --> NA first fortran dimension of A
! --> NB first fortran dimension of B
!------------------------------------------------------------------------------
      SUBROUTINE DL1UBB(A,B,M,MAH1,MAH2,MBH1,MBH2,NA,NB)
      DIMENSION A(NA,-MAH1:MAH2),B(NB,-MBH1:MBH2)
      DO J=1,M
       IMOST=MIN(M,J+MAH1)
       JMOST=MIN(M,J+MAH2)
       JLEAST=MAX(1,J-MAH1)
       JP=J+1
       AJJ=A(J,0)
!       IF(AJJ.EQ.0.)STOP'failure in DL1UBB'
       IF(AJJ.EQ.0.)STOP
       AJJI=1./AJJ
       DO K=JLEAST,JMOST
	A(J,K-J)=AJJI*A(J,K-J)
       ENDDO
       DO I=JP,IMOST
	AIJ=A(I,J-I)
	DO K=JP,JMOST
	 A(I,K-I)=A(I,K-I)-AIJ*A(J,K-J)
	ENDDO
       ENDDO
       A(J,0)=1.
       DO K=-MBH1,MBH2
	B(J,K)=AJJI*B(J,K)
       ENDDO
      ENDDO
      RETURN
      END

!------------------------------------------------------------------------------
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE DWRTM
!  Write out contents of a real matrix
!
!  --> A	matrix
!  --> MI,MJ	number of active rows and columns of matrix
!  --> JCOLS	number of columns output at a time
!  --> NA	first fortran dimension of matrix
!------------------------------------------------------------------------------
      SUBROUTINE DWRTM(A,MI,MJ,JCOLS,NA)
      DIMENSION A(NA,*)
      DO J1=1,MJ,JCOLS
       J2=MIN(MJ,J1+JCOLS-1)
       PRINT'(8X,10(I3,9X))',(J,J=J1,J2)
       DO  I=1,MI
	PRINT'(1X,I3,10(1X,E11.5))',I,(A(I,J),J=J1,J2)
       ENDDO
       PRINT'()'
      ENDDO
      RETURN
      END
