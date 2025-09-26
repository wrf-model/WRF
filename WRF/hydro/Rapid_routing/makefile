#*******************************************************************************
#makefile
#*******************************************************************************

#Purpose:
#This file, along with the make utility allows compiling/linking RAPID
#Author:
#Cedric H. David, 2008-2015


#*******************************************************************************
#PETSc and TAO rules and variables (where environment variables and options are)
#*******************************************************************************

#-------------------------------------------------------------------------------
#Default RAPID - includes optimization with TAO 
#-------------------------------------------------------------------------------
#FPPFLAGS=
#include ${TAO_DIR}/conf/tao_base

#-------------------------------------------------------------------------------
#If want to use RAPID without TAO, in which case the optimization is unavailable
#-------------------------------------------------------------------------------
FPPFLAGS=-D NO_TAO
include ${PETSC_DIR}/conf/variables
include ${PETSC_DIR}/conf/rules
#include ${PETSC_DIR}/lib/petsc/conf/variables
#include ${PETSC_DIR}/lib/petsc/conf/rules
#PETSC_FC_INCLUDES=-I/work/02151/peirongl/_code_wrfhydro/wrf_hydro_model/trunk/NDHMS/petsc-3.6.2/include/
#PETSC_LIB=-L/work/02151/peirongl/_code_wrfhydro/wrf_hydro_model/trunk/NDHMS/petsc-3.6.2/lib/

#*******************************************************************************
#Location of netCDF include and lib directories
#*******************************************************************************
NETCDF_LIB=-L ${TACC_NETCDF_LIB} -lnetcdf
NETCDF_INCLUDE=-I ${TACC_NETCDF_INC}


#*******************************************************************************
#makefile instructions 
#*******************************************************************************

#-------------------------------------------------------------------------------
#Test that environment variables are properly read by make
#-------------------------------------------------------------------------------
dummy: 
	echo ${FLINKER} ${FPPFLAGS}

#-------------------------------------------------------------------------------
#Link RAPID main
#-------------------------------------------------------------------------------
rapid:	rapid_main.o \
	rapid_init.o \
	rapid_read_namelist.o \
	rapid_arrays.o \
	rapid_create_obj.o \
	rapid_create_Qout_file.o \
	rapid_open_Qout_file.o \
	rapid_open_Vlat_file.o \
	rapid_open_Qobs_file.o \
	rapid_open_Qfor_file.o \
	rapid_open_Qhum_file.o \
	rapid_write_Qout_file.o \
	rapid_read_Vlat_file.o \
	rapid_read_Qobs_file.o \
	rapid_read_Qfor_file.o \
	rapid_read_Qhum_file.o \
	rapid_close_Qout_file.o \
	rapid_close_Vlat_file.o \
	rapid_close_Qobs_file.o \
	rapid_close_Qfor_file.o \
	rapid_close_Qhum_file.o \
	rapid_get_Qdam.o \
	rapid_set_Qext0.o \
	rapid_hsh_mat.o \
	rapid_net_mat.o \
	rapid_net_mat_brk.o \
	rapid_obs_mat.o \
	rapid_routing.o \
	rapid_routing_param.o \
	rapid_phiroutine.o \
	rapid_destro_obj.o \
	rapid_final.o \
	rapid_var.o \
	hrldas_RAPID_wrapper.o \
	hrldas_RAPID_drv.o
	${FLINKER} ${FPPFLAGS} -o \
	rapid \
	rapid_main.o \
	rapid_init.o \
	rapid_read_namelist.o \
	rapid_arrays.o \
	rapid_create_obj.o \
	rapid_create_Qout_file.o \
	rapid_open_Qout_file.o \
	rapid_open_Vlat_file.o \
	rapid_open_Qobs_file.o \
	rapid_open_Qfor_file.o \
	rapid_open_Qhum_file.o \
	rapid_write_Qout_file.o \
	rapid_read_Vlat_file.o \
	rapid_read_Qobs_file.o \
	rapid_read_Qfor_file.o \
	rapid_read_Qhum_file.o \
	rapid_close_Qout_file.o \
	rapid_close_Vlat_file.o \
	rapid_close_Qobs_file.o \
	rapid_close_Qfor_file.o \
	rapid_close_Qhum_file.o \
	rapid_get_Qdam.o \
	rapid_set_Qext0.o \
	rapid_hsh_mat.o \
	rapid_net_mat.o \
	rapid_net_mat_brk.o \
	rapid_routing.o \
	rapid_routing_param.o \
	rapid_obs_mat.o \
	rapid_phiroutine.o \
	rapid_destro_obj.o \
	rapid_final.o \
	rapid_var.o \
	hrldas_RAPID_wrapper.o \
	hrldas_RAPID_drv.o \
	${TAO_FORTRAN_LIB} ${TAO_LIB} ${PETSC_LIB} ${NETCDF_LIB}
	${RM} *.o *.mod 
#	ln -sf ../src/rapid ../run/rapid
#	ln -sf ../src/rapid ../rtk/rapid
#----LPR: uncomment the link because no RAPID executable will be generated when
#---------coupled with WRF-Hydro

#-------------------------------------------------------------------------------
#Compile RAPID
#-------------------------------------------------------------------------------
rapid_final.o:		rapid_final.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_final.F90 ${PETSC_FC_INCLUDES}

rapid_destro_obj.o: 	rapid_destro_obj.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_destro_obj.F90 ${PETSC_FC_INCLUDES} ${TAO_INCLUDE}

rapid_phiroutine.o: 	rapid_phiroutine.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_phiroutine.F90 ${PETSC_FC_INCLUDES} ${TAO_INCLUDE} 

rapid_routing.o: 	rapid_routing.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_routing.F90 ${PETSC_FC_INCLUDES} ${NETCDF_INCLUDE}

rapid_init.o: 		rapid_read_namelist.o rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_init.F90 ${PETSC_FC_INCLUDES}

rapid_routing_param.o: 	rapid_routing_param.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_routing_param.F90 ${PETSC_FC_INCLUDES}

rapid_obs_mat.o: 	rapid_obs_mat.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_obs_mat.F90 ${PETSC_FC_INCLUDES}

rapid_net_mat_brk.o: 	rapid_net_mat_brk.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_net_mat_brk.F90 ${PETSC_FC_INCLUDES}

rapid_net_mat.o: 	rapid_net_mat.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_net_mat.F90 ${PETSC_FC_INCLUDES}

rapid_hsh_mat.o: 	rapid_hsh_mat.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_hsh_mat.F90 ${PETSC_FC_INCLUDES}

rapid_get_Qdam.o: 	rapid_get_Qdam.F90 rapid_var.mod 
	${FLINKER} ${FPPFLAGS} -c rapid_get_Qdam.F90 ${PETSC_FC_INCLUDES}

rapid_set_Qext0.o: 	rapid_set_Qext0.F90 rapid_var.mod 
	${FLINKER} ${FPPFLAGS} -c rapid_set_Qext0.F90 ${PETSC_FC_INCLUDES}

rapid_close_Qfor_file.o: 	rapid_close_Qfor_file.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_close_Qfor_file.F90

rapid_close_Qhum_file.o: 	rapid_close_Qhum_file.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_close_Qhum_file.F90

rapid_close_Qobs_file.o: 	rapid_close_Qobs_file.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_close_Qobs_file.F90

rapid_close_Vlat_file.o: 	rapid_close_Vlat_file.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_close_Vlat_file.F90 ${NETCDF_INCLUDE} 

rapid_close_Qout_file.o: 	rapid_close_Qout_file.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_close_Qout_file.F90 ${NETCDF_INCLUDE} 

rapid_read_Qfor_file.o: 	rapid_read_Qfor_file.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_read_Qfor_file.F90 ${PETSC_FC_INCLUDES}

rapid_read_Qhum_file.o: 	rapid_read_Qhum_file.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_read_Qhum_file.F90 ${PETSC_FC_INCLUDES}

rapid_read_Qobs_file.o: 	rapid_read_Qobs_file.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_read_Qobs_file.F90 ${PETSC_FC_INCLUDES}

rapid_read_Vlat_file.o: 	rapid_read_Vlat_file.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_read_Vlat_file.F90 ${PETSC_FC_INCLUDES} ${NETCDF_INCLUDE}

rapid_write_Qout_file.o: 	rapid_write_Qout_file.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_write_Qout_file.F90 ${PETSC_FC_INCLUDES} ${NETCDF_INCLUDE}

rapid_open_Qfor_file.o: 	rapid_open_Qfor_file.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_open_Qfor_file.F90 

rapid_open_Qhum_file.o: 	rapid_open_Qhum_file.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_open_Qhum_file.F90 

rapid_open_Qobs_file.o: 	rapid_open_Qobs_file.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_open_Qobs_file.F90 

rapid_open_Vlat_file.o: 	rapid_open_Vlat_file.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_open_Vlat_file.F90 ${NETCDF_INCLUDE}

rapid_open_Qout_file.o: 	rapid_open_Qout_file.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_open_Qout_file.F90 ${NETCDF_INCLUDE}

rapid_create_Qout_file.o: 	rapid_create_Qout_file.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_create_Qout_file.F90 ${NETCDF_INCLUDE}

rapid_create_obj.o: 	rapid_create_obj.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_create_obj.F90 ${PETSC_FC_INCLUDES} ${TAO_INCLUDE}

rapid_arrays.o:	rapid_arrays.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_arrays.F90 ${PETSC_FC_INCLUDES}
	
rapid_read_namelist.o:	rapid_read_namelist.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c rapid_read_namelist.F90
	
rapid_var.o rapid_var.mod:	rapid_var.F90
	${FLINKER} ${FPPFLAGS} -c rapid_var.F90 ${PETSC_FC_INCLUDES} ${TAO_INCLUDE} 

hrldas_RAPID_wrapper.mod hrldas_RAPID_wrapper.o:     hrldas_RAPID_wrapper.F90 rapid_var.mod
	${FLINKER} ${FPPFLAGS} -c hrldas_RAPID_wrapper.F90  ${PETSC_FC_INCLUDES} ${TAO_INCLUDE}

rapid_main.o:   rapid_main.F90 rapid_var.mod hrldas_RAPID_wrapper.mod
	${FLINKER} ${FPPFLAGS} -c rapid_main.F90 ${PETSC_FC_INCLUDES} \
						${TAO_INCLUDE} ${NETCDF_INCLUDE}

hrldas_RAPID_drv.o:     hrldas_RAPID_drv.F90 hrldas_RAPID_wrapper.mod
	${FLINKER} ${FPPFLAGS} -c hrldas_RAPID_drv.F90  ${PETSC_FC_INCLUDE} ${TAO_INCLUDE}
	
#-------------------------------------------------------------------------------
#Clean
#-------------------------------------------------------------------------------
clean::
	${RM} *.o *.mod rapid ../run/rapid ../rtk/rapid

