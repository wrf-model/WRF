# Set here the Fortran77 compiler and the desired optimization options
COMPILER = GNU

FC_INTEL   = ifort
FOPT_INTEL = -O -f77rtl -mp -pc80 -prec_div -tpp7
FC_PGF     = pgf77
FOPT_PGF   = -O -fast -pc 80 -Kieee
FC_GNU     = g77
FOPT_GNU   = -O -Wall -Wimplicit -ffast-math -funroll-loops \
	         -malign-double -ffortran-bounds-check
FC_HPUX    = f90
FOPT_HPUX  = -O

FC   = $(FC_$(COMPILER))
FOPT = $(FOPT_$(COMPILER)) 

# Mathematical libraries: blas and lapack
# MATHLIB   = -L/usr/local/lib  -llapack_g77  -L/usr/lib -lblas_g77
MATHLIB   = -llapack  -lblas 

# To create Matlab mex gateway routines
# Note: use $(FC) as the mex Fortran compiler
MEX  = mex

HEADERS = KPP_ROOT_Global.h  KPP_ROOT_Parameters.h  KPP_ROOT_Sparse.h

SPSRC = KPP_ROOT_JacobianSP.f \
	KPP_ROOT_HessianSP.f  \
	KPP_ROOT_StoichiomSP.f

SPOBJ = KPP_ROOT_JacobianSP.o \
	KPP_ROOT_HessianSP.o  \
	KPP_ROOT_StoichiomSP.o


SRC =   KPP_ROOT_Main.f      KPP_ROOT_Integrator.f   \
        KPP_ROOT_Function.f  KPP_ROOT_Initialize.f   \
	KPP_ROOT_Jacobian.f  KPP_ROOT_LinearAlgebra.f\
	KPP_ROOT_Rates.f     KPP_ROOT_Hessian.f      \
	KPP_ROOT_Stoichiom.f KPP_ROOT_Util.f         \
	KPP_ROOT_Monitor.f   

OBJ =   KPP_ROOT_Main.o      KPP_ROOT_Integrator.o   \
        KPP_ROOT_Function.o  KPP_ROOT_Initialize.o   \
	KPP_ROOT_Jacobian.o  KPP_ROOT_LinearAlgebra.o\
	KPP_ROOT_Rates.o     KPP_ROOT_Hessian.o      \
	KPP_ROOT_Stoichiom.o KPP_ROOT_Util.o         \
	KPP_ROOT_Monitor.o

all:    exe mex

exe:	$(HEADERS) $(SPOBJ) $(OBJ)
	$(FC) $(FOPT) $(SPOBJ) $(OBJ) $(MATHLIB) -o KPP_ROOT.exe

mex:    $(HEADERS) $(SPOBJ) $(OBJ)
	$(MEX) FC#$(FC) -fortran -O KPP_ROOT_mex_Fun.f     $(SPOBJ) $(OBJ)
	$(MEX) FC#$(FC) -fortran -O KPP_ROOT_mex_Jac_SP.f  $(SPOBJ) $(OBJ)
	$(MEX) FC#$(FC) -fortran -O KPP_ROOT_mex_Hessian.f $(SPOBJ) $(OBJ)
	
clean:
	rm -f $(SPOBJ) $(OBJ) KPP_ROOT.exe KPP_ROOT.map  \
	KPP_ROOT.dat 

distclean:
	rm -f $(SPOBJ) $(OBJ) KPP_ROOT.exe KPP_ROOT.map  \
	KPP_ROOT.dat KPP_ROOT*.f  KPP_ROOT*.h 

KPP_ROOT_Monitor.o: KPP_ROOT_Monitor.f $(HEADERS)
	$(FC) $(FOPT) -c $<

KPP_ROOT_JacobianSP.o: KPP_ROOT_JacobianSP.f $(HEADERS)
	$(FC) $(FOPT) -c $<

KPP_ROOT_HessianSP.o: KPP_ROOT_HessianSP.f  $(HEADERS)
	$(FC) $(FOPT) -c $<

KPP_ROOT_StoichiomSP.o: KPP_ROOT_StoichiomSP.f $(HEADERS)
	$(FC) $(FOPT) -c $<

KPP_ROOT_Main.o: KPP_ROOT_Main.f  KPP_ROOT_Initialize.o $(HEADERS) $(SPOBJ)
	$(FC) $(FOPT) -c $<

KPP_ROOT_Integrator.o: KPP_ROOT_Integrator.f  $(HEADERS) $(SPOBJ)
	$(FC) $(FOPT) -c $<

KPP_ROOT_Initialize.o: KPP_ROOT_Initialize.f  $(HEADERS) $(SPOBJ)
	$(FC) $(FOPT) -c $<

KPP_ROOT_Function.o: KPP_ROOT_Function.f  $(HEADERS) $(SPOBJ)
	$(FC) $(FOPT) -c $<

KPP_ROOT_Jacobian.o: KPP_ROOT_Jacobian.f  $(HEADERS) $(SPOBJ)
	$(FC) $(FOPT) -c $<

KPP_ROOT_LinearAlgebra.o: KPP_ROOT_LinearAlgebra.f  $(HEADERS) $(SPOBJ)
	$(FC) $(FOPT) -c $<

KPP_ROOT_Rates.o: KPP_ROOT_Rates.f  $(HEADERS) $(SPOBJ)
	$(FC) $(FOPT) -c $<

KPP_ROOT_Hessian.o:  KPP_ROOT_Hessian.f $(HEADERS) $(SPOBJ)
	$(FC) $(FOPT) -c $<

KPP_ROOT_Stoichiom.o: KPP_ROOT_Stoichiom.f  $(HEADERS) $(SPOBJ)
	$(FC) $(FOPT) -c $<

KPP_ROOT_Util.o: KPP_ROOT_Util.f  $(HEADERS) $(SPOBJ)
	$(FC) $(FOPT) -c $<

