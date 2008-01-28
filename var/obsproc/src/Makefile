#	Lower level Makefile for 3dvar_obs

# Compilation macros

include ../compiler_macros
include ../configure.rules


MODULES		=	module_date.o \
			module_namelist.o


MODULES_OBS	=	\
			module_type.o \
			module_func.o \
			module_inside.o \
			module_obs_merge.o \
			module_per_type.o \
			module_duplicate.o \
			module_sort.o \
			module_write.o \
			module_complete.o \
			module_recoverp.o \
			module_diagnostics.o \
			module_recoverh.o \
			module_icao.o \
			module_qc.o \
			module_err_afwa.o \
			module_err_ncep.o \
                        module_thin_ob.o

MODULES_GTS	=	module_decoded.o

MODULES_MM5	=	module_mm5.o \
			module_map.o \
			module_map_utils.o \
			module_intp.o

OBJS    	=	error_handler.o \
			fm_decoder.o \
			sort_platform.o \
			qc_reduction.o \
			check_obs.o \
			setup.o


#	Source code dependencies

cray dec hp ibm sgi sun default:	default
#		@echo "you need to be up a directory and then enter: 'make your_platform'"

default:		3dvar_obs.exe

all:			3dvar_obs.exe

3dvar_obs.exe:		$(MODULES) $(MODULES_MM5) $(MODULES_OBS) \
			$(MODULES_GTS) \
			$(OBJS) \
			3dvar_obs.o \
			Makefile ../Makefile
			$(FC) -o $@ \
			$(MODULES) $(MODULES_MM5) $(MODULES_OBS) \
			$(MODULES_GTS) \
			$(OBJS) \
			3dvar_obs.o $(LDFLAGS) $(LOCAL_LIBRARIES)

#------------------------------------------------------------------------------

3dvar_obs.o:		module_namelist.o module_date.o \
			module_decoded.o \
			module_type.o \
			module_per_type.o \
			module_sort.o \
			module_duplicate.o \
			module_write.o \
			module_complete.o \
			module_diagnostics.o \
			module_recoverp.o \
			module_recoverh.o \
			module_qc.o \
			sort_platform.o \
			qc_reduction.o \
			check_obs.o \
                        setup.o \
			module_err_afwa.o \
			module_err_ncep.o \
			Makefile ../Makefile

#------------------------------------------------------------------------------

error_handler.o:	\
			Makefile ../Makefile

fm_decoder.o:		\
			Makefile ../Makefile

sort_platform.o:	\
			module_type.o module_func.o \
			Makefile ../Makefile

qc_reduction.o:		missing.inc \
			module_type.o module_func.o \
			Makefile ../Makefile

check_obs.o:		missing.inc platform_interface.inc \
			module_type.o module_func.o \
			module_per_type.o \
			Makefile ../Makefile

setup.o:		constants.inc \
			Makefile ../Makefile

module_namelist.o:	\
			module_date.o error_handler.o \
			module_mm5.o \
			module_map.o \
			module_map_utils.o \
			Makefile ../Makefile

module_date.o:		\
			Makefile ../Makefile

module_mm5.o:		constants.inc \
			Makefile ../Makefile

module_map.o:		constants.inc \
			module_mm5.o \
			Makefile ../Makefile

module_map_utils.o:	constants.inc \
			Makefile ../Makefile

module_intp.o:		\
		 	Makefile ../Makefile

module_decoded.o:	missing.inc \
			module_date.o \
			module_type.o module_func.o \
			module_inside.o  module_obs_merge.o \
			module_per_type.o \
			module_icao.o \
			fm_decoder.o error_handler.o \
		 	Makefile ../Makefile

module_type.o:	\
			Makefile ../Makefile

module_func.o:	\
			module_date.o \
		 	Makefile ../Makefile

module_inside.o:	\
			module_type.o module_func.o \
			module_map.o  module_mm5.o \
		 	Makefile ../Makefile

module_obs_merge.o:	missing.inc \
			module_type.o module_func.o \
		 	Makefile ../Makefile

module_per_type.o:	\
		 	Makefile ../Makefile

module_sort.o:		\
			module_type.o module_func.o \
		 	Makefile ../Makefile

module_duplicate.o:	\
			module_date.o \
			module_type.o module_func.o \
		 	Makefile ../Makefile

module_complete.o:	missing.inc platform_interface.inc\
			module_type.o module_func.o \
			module_per_type.o \
		 	Makefile ../Makefile
module_write.o:	\
			module_date.o \
			module_type.o module_func.o \
			module_decoded.o \
		 	Makefile ../Makefile

module_diagnostics.o:	missing.inc constants.inc \
			module_type.o module_func.o \
			module_map.o \
		 	Makefile ../Makefile

module_qc.o:		missing.inc constants.inc platform_interface.inc \
			module_type.o module_func.o \
			module_per_type.o \
		 	Makefile ../Makefile

module_recoverp.o:	missing.inc constants.inc \
			module_type.o \
			module_func.o \
			module_mm5.o \
			error_handler.o \
			Makefile ../Makefile

module_recoverh.o:	missing.inc constants.inc platform_interface.inc \
			module_type.o \
			module_func.o \
			module_per_type.o \
			module_mm5.o \
			module_icao.o \
			error_handler.o \
			Makefile ../Makefile

module_err_afwa.o:	missing.inc constants.inc platform_interface.inc \
			module_type.o module_func.o \
			module_err_ncep.o module_intp.o \
			Makefile ../Makefile

module_err_ncep.o:	missing.inc constants.inc \
			module_type.o module_func.o \
			module_intp.o \
			Makefile ../Makefile

module_thin_ob.:	module_mm5.o \
			module_func.o \
                        module_type.o \
			module_map.o
#-----------------------------------------------------------------------------#
clean:
			$(RM) *.o *.f core *.kmo ctmi* *.mod *.lst \
			*_out_* output *.out \
			3dvar_obs.exe
