
$(MODEL)_Integrator.f90:	
		$(KPP) $(MODEL).kpp
		cp -f $(MODEL)_Integrator.f90 ../../../module_kpp_$(MODEL)_Integr.F
		cp -f $(MODEL)_Parameters.f90 ../../../module_kpp_$(MODEL)_Parameters.F
		cp -f $(MODEL)_Precision.f90 ../../../module_kpp_$(MODEL)_Precision.F
		cp -f $(MODEL)_JacobianSP.f90 ../../../module_kpp_$(MODEL)_JacobianSP.F
		cp -f $(MODEL)_Jacobian.f90 ../../../module_kpp_$(MODEL)_Jacobian.F
		cp -f $(MODEL)_Update_Rconst.f90 ../../../module_kpp_$(MODEL)_Update_Rconst.F
		touch ../../util/run_wkc


# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)


$(MODEL)_Integrator.f90: \
	$(MODEL).spc $(MODEL).eqn $(MODEL).def $(MODEL).kpp $(MODEL)_wrfkpp.equiv



