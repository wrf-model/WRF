#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User: Set here the F90 compiler and options
#       Pedefined compilers: INTEL, PGF, HPUX, LAHEY
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

COMPILER = GNU
#COMPILER = LAHEY
#COMPILER = INTEL
#COMPILER = PGF
#COMPILER = HPUX

FC_GNU     = g95
FOPT_GNU   = -cpp -O -pg -fbounds-check 
FC_LAHEY   = lf95
FOPT_LAHEY = -Cpp --pca
#FOPT_LAHEY = -Cpp --chk a,e,s,u --pca --ap -O0 -g --trap
FC_INTEL   = ifort 
FOPT_INTEL = -cpp -O -mp -pc80 -prec_div -tpp7 -implicitnone
FC_PGF     = pgf90
FOPT_PGF   = -Mpreprocess -O -fast -pc 80 -Kieee
FC_HPUX    = f90
FOPT_HPUX  = -O -u +Oall +check=on

# define FULL_ALGEBRA for non-sparse integration
FC   = $(FC_$(COMPILER))
FOPT = $(FOPT_$(COMPILER)) # -DFULL_ALGEBRA

LIBS =
#LIBS = -llapack -lblas

# Command to create Matlab mex gateway routines 
# Note: use $(FC) as the mex Fortran compiler
MEX  = mex

GENSRC = KPP_ROOT_Precision.f90  \
	 KPP_ROOT_Parameters.f90     \
	 KPP_ROOT_Global.f90  

GENOBJ = KPP_ROOT_Precision.o    \
	 KPP_ROOT_Parameters.o       \
	 KPP_ROOT_Global.o     

FUNSRC = KPP_ROOT_Function.f90 
FUNOBJ = KPP_ROOT_Function.o 

JACSRC = KPP_ROOT_JacobianSP.f90  KPP_ROOT_Jacobian.f90
JACOBJ = KPP_ROOT_JacobianSP.o    KPP_ROOT_Jacobian.o

HESSRC = KPP_ROOT_HessianSP.f90   KPP_ROOT_Hessian.f90
HESOBJ = KPP_ROOT_HessianSP.o     KPP_ROOT_Hessian.o

STMSRC = KPP_ROOT_StoichiomSP.f90 KPP_ROOT_Stoichiom.f90 
STMOBJ = KPP_ROOT_StoichiomSP.o   KPP_ROOT_Stoichiom.o

UTLSRC = KPP_ROOT_Rates.f90 KPP_ROOT_Util.f90 KPP_ROOT_Monitor.f90
UTLOBJ = KPP_ROOT_Rates.o   KPP_ROOT_Util.o   KPP_ROOT_Monitor.o

LASRC  = KPP_ROOT_LinearAlgebra.f90 
LAOBJ  = KPP_ROOT_LinearAlgebra.o   

STOCHSRC = KPP_ROOT_Stochastic.f90 
STOCHOBJ = KPP_ROOT_Stochastic.o 

MAINSRC = KPP_ROOT_Main.f90   KPP_ROOT_Initialize.f90   KPP_ROOT_Integrator.f90 KPP_ROOT_Model.f90
MAINOBJ = KPP_ROOT_Main.o     KPP_ROOT_Initialize.o     KPP_ROOT_Integrator.o   KPP_ROOT_Model.o 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User: modify the line below to include only the
#       objects needed by your application
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ALLOBJ = $(GENOBJ) $(FUNOBJ) $(JACOBJ) $(HESOBJ) $(STMOBJ) \
	 $(UTLOBJ) $(LAOBJ)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User: modify the line below to include only the
#       executables needed by your application
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all:    exe

exe:	$(ALLOBJ) $(MAINOBJ) 
	$(FC) $(FOPT) $(ALLOBJ) $(MAINOBJ) $(LIBS) -o KPP_ROOT.exe

stochastic:$(ALLOBJ) $(STOCHOBJ) $(MAINOBJ)
	$(FC) $(FOPT) $(ALLOBJ) $(STOCHOBJ) $(MAINOBJ) $(LIBS) \
	-o KPP_ROOT_stochastic.exe

mex:    $(ALLOBJ)
	$(MEX) FC#$(FC) -fortran -O KPP_ROOT_mex_Fun.f90     $(ALLOBJ)
	$(MEX) FC#$(FC) -fortran -O KPP_ROOT_mex_Jac_SP.f90  $(ALLOBJ)
	$(MEX) FC#$(FC) -fortran -O KPP_ROOT_mex_Hessian.f90 $(ALLOBJ)

clean:
	rm -f KPP_ROOT*.o KPP_ROOT*.mod \
	KPP_ROOT*.dat KPP_ROOT.exe KPP_ROOT*.mexglx \
	KPP_ROOT.map

distclean:
	rm -f KPP_ROOT*.o KPP_ROOT*.mod \
	KPP_ROOT*.dat KPP_ROOT.exe KPP_ROOT.map \
	KPP_ROOT*.f90 KPP_ROOT_*.mexglx

KPP_ROOT_Precision.o: KPP_ROOT_Precision.f90 
	$(FC) $(FOPT) -c $<

KPP_ROOT_Parameters.o: KPP_ROOT_Parameters.f90 \
	            KPP_ROOT_Precision.o
	$(FC) $(FOPT) -c $<

KPP_ROOT_Monitor.o: KPP_ROOT_Monitor.f90 \
	             KPP_ROOT_Precision.o
	$(FC) $(FOPT) -c $<

KPP_ROOT_Global.o: KPP_ROOT_Global.f90 \
	            KPP_ROOT_Parameters.o KPP_ROOT_Precision.o
	$(FC) $(FOPT) -c $<

KPP_ROOT_Initialize.o: KPP_ROOT_Initialize.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $<

KPP_ROOT_Function.o: KPP_ROOT_Function.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $<

KPP_ROOT_Stochastic.o: KPP_ROOT_Stochastic.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $<

KPP_ROOT_JacobianSP.o: KPP_ROOT_JacobianSP.f90 $(GENOBJ)
	$(FC) $(FOPT) -c $<

KPP_ROOT_Jacobian.o: KPP_ROOT_Jacobian.f90  $(GENOBJ) KPP_ROOT_JacobianSP.o
	$(FC) $(FOPT) -c $<

KPP_ROOT_LinearAlgebra.o: KPP_ROOT_LinearAlgebra.f90 $(GENOBJ) KPP_ROOT_JacobianSP.o
	$(FC) $(FOPT) -c $<

KPP_ROOT_Rates.o: KPP_ROOT_Rates.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $<

KPP_ROOT_HessianSP.o: KPP_ROOT_HessianSP.f90  $(GENOBJ)
	$(FC) $(FOPT) -c $<

KPP_ROOT_Hessian.o:  KPP_ROOT_Hessian.f90 $(GENOBJ) KPP_ROOT_HessianSP.o
	$(FC) $(FOPT) -c $<

KPP_ROOT_StoichiomSP.o: KPP_ROOT_StoichiomSP.f90 $(GENOBJ)
	$(FC) $(FOPT) -c $<

KPP_ROOT_Stoichiom.o: KPP_ROOT_Stoichiom.f90  $(GENOBJ) KPP_ROOT_StoichiomSP.o
	$(FC) $(FOPT) -c $<

KPP_ROOT_Util.o: KPP_ROOT_Util.f90  $(GENOBJ) KPP_ROOT_Monitor.o
	$(FC) $(FOPT) -c $<

KPP_ROOT_Main.o: KPP_ROOT_Main.f90  $(ALLOBJ) KPP_ROOT_Initialize.o KPP_ROOT_Model.o KPP_ROOT_Integrator.o
	$(FC) $(FOPT) -c $<

KPP_ROOT_Model.o: KPP_ROOT_Model.f90  $(ALLOBJ) KPP_ROOT_Integrator.o
	$(FC) $(FOPT) -c $<

KPP_ROOT_Integrator.o: KPP_ROOT_Integrator.f90  $(ALLOBJ)
	$(FC) $(FOPT) -c $<
