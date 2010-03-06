#	Lower level Makefile for obsproc

# Compilation macros

include ../../../configure.wrf


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

BUFR_LIB    	=	../../external/bufr/libbufr.a

#	Source code dependencies

cray dec hp ibm sgi sun default:	default
#		@echo "you need to be up a directory and then enter: 'make your_platform'"

default:		obsproc.exe

all:			obsproc.exe

obsproc.exe:		$(MODULES) $(MODULES_MM5) $(MODULES_OBS) \
			$(MODULES_GTS) \
			$(OBJS) \
			obsproc.o
			if [ "$(BUFR)" = "-DBUFR" ] ; then \
			$(SFC) -o $@ \
			$(MODULES) $(MODULES_MM5) $(MODULES_OBS) \
			$(MODULES_GTS) \
			$(OBJS) \
			obsproc.o $(LDFLAGS) $(LOCAL_LIBRARIES) $(BUFR_LIB); \
			else \
			$(SFC) -o $@ \
			$(MODULES) $(MODULES_MM5) $(MODULES_OBS) \
			$(MODULES_GTS) \
			$(OBJS) \
			obsproc.o $(LDFLAGS) $(LOCAL_LIBRARIES) ; \
			fi

#------------------------------------------------------------------------------

obsproc.o:		module_namelist.o module_date.o \
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
			module_err_ncep.o

#------------------------------------------------------------------------------

sort_platform.o:	\
			module_type.o module_func.o

qc_reduction.o:		missing.inc \
			module_type.o module_func.o 

check_obs.o:		missing.inc platform_interface.inc \
			module_type.o module_func.o \
			module_per_type.o 

setup.o:		constants.inc 

module_namelist.o:	\
			module_date.o error_handler.o \
			module_mm5.o \
			module_map.o \
			module_map_utils.o

module_mm5.o:		constants.inc

module_map.o:		constants.inc \
			module_mm5.o

module_map_utils.o:	constants.inc

module_decoded.o:	missing.inc \
			module_date.o \
			module_type.o module_func.o \
			module_inside.o  module_obs_merge.o \
			module_per_type.o \
			module_icao.o \
			fm_decoder.o error_handler.o 

module_func.o:	\
			module_date.o

module_inside.o:	\
			module_type.o module_func.o \
			module_map.o  module_mm5.o 

module_obs_merge.o:	missing.inc \
			module_type.o module_func.o


module_sort.o:		\
			module_type.o module_func.o

module_duplicate.o:	\
			module_date.o \
			module_type.o module_func.o

module_complete.o:	missing.inc platform_interface.inc\
			module_type.o module_func.o \
			module_per_type.o 

module_write.o:	\
			module_date.o \
			module_type.o module_func.o \
			module_decoded.o 

module_diagnostics.o:	missing.inc constants.inc \
			module_type.o module_func.o \
			module_map.o

module_qc.o:		missing.inc constants.inc platform_interface.inc \
			module_type.o module_func.o \
			module_per_type.o 

module_recoverp.o:	missing.inc constants.inc \
			module_type.o \
			module_func.o \
			module_mm5.o \
			error_handler.o 

module_recoverh.o:	missing.inc constants.inc platform_interface.inc \
			module_type.o \
			module_func.o \
			module_per_type.o \
			module_mm5.o \
			module_icao.o \
			error_handler.o 

module_err_afwa.o:	missing.inc constants.inc platform_interface.inc \
			module_type.o module_func.o \
			module_err_ncep.o module_intp.o

module_err_ncep.o:	missing.inc constants.inc \
			module_type.o module_func.o \
			module_intp.o 

module_thin_ob.:	module_mm5.o \
			module_func.o \
                        module_type.o \
			module_map.o

#       The user should not need to modify any of this.

#       Suffixes with which the Makefiles are to be concerned.

.SUFFIXES: .F .F90 .i .o .f .c

#       Build rules for the OBSPROC programs.

.f.o:
	$(RM) $@
	$(SFC) -c $(FCFLAGS) -I../../build  $*.f
	$(RM) $*.f

.F90.f:
	$(RM) $@
	$(CPP) $(CPPFLAGS) $*.F90 > $@

#-----------------------------------------------------------------------------#
clean:
			$(RM) *.o *.f core *.kmo ctmi* *.mod *.lst \
			*_out_* output *.out \
			obsproc.exe
