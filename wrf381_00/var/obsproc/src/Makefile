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

MODULES_GTS	=	module_decoded.o \
			module_stntbl.o

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

check_obs.o : check_obs.F90 platform_interface.inc missing.inc module_per_type.o module_func.o module_type.o 
error_handler.o : error_handler.F90 
fm_decoder.o : fm_decoder.F90 
module_complete.o : module_complete.F90 platform_interface.inc missing.inc module_per_type.o module_func.o module_type.o 
module_date.o : module_date.F90 
module_decoded.o : module_decoded.F90 platform_interface.inc missing.inc module_obs_merge.o module_icao.o module_per_type.o module_date.o module_namelist.o module_inside.o module_func.o module_type.o module_stntbl.o
module_diagnostics.o : module_diagnostics.F90 constants.inc missing.inc module_intp.o module_map.o module_mm5.o module_func.o module_type.o 
module_duplicate.o : module_duplicate.F90 platform_interface.inc module_per_type.o module_obs_merge.o module_date.o module_func.o module_type.o 
module_err_afwa.o : module_err_afwa.F90 constants.inc missing.inc platform_interface.inc module_namelist.o module_intp.o module_err_ncep.o module_func.o module_type.o 
module_err_ncep.o : module_err_ncep.F90 constants.inc missing.inc module_intp.o module_func.o module_type.o 
module_func.o : module_func.F90 module_date.o module_type.o 
module_gpspw.o : module_gpspw.F90 module_gpspw_gst.o module_gpspw_caa.o 
module_gpspw_caa.o : module_gpspw_caa.F90 missing.inc module_per_type.o module_inside.o module_type.o module_date.o 
module_gpspw_gst.o : module_gpspw_gst.F90 missing.inc module_per_type.o module_inside.o module_type.o module_date.o 
module_icao.o : module_icao.F90 missing.inc constants.inc 
module_inside.o : module_inside.F90 module_date.o module_map.o module_namelist.o module_map_utils.o module_mm5.o module_func.o module_type.o 
module_intp.o : module_intp.F90 
module_map.o : module_map.F90 constants.inc module_mm5.o 
module_map_utils.o : module_map_utils.F90 
module_mm5.o : module_mm5.F90 constants.inc 
module_namelist.o : module_namelist.F90 missing.inc module_map_utils.o module_mm5.o module_date.o 
module_obs_merge.o : module_obs_merge.F90 missing.inc module_func.o module_type.o 
module_per_type.o : module_per_type.F90 
module_qc.o : module_qc.F90 constants.inc missing.inc platform_interface.inc module_mm5.o module_per_type.o module_func.o module_type.o 
module_recoverh.o : module_recoverh.F90 constants.inc platform_interface.inc missing.inc module_mm5.o module_per_type.o module_func.o module_type.o 
module_recoverp.o : module_recoverp.F90 platform_interface.inc missing.inc module_mm5.o module_func.o module_type.o 
module_sort.o : module_sort.F90 module_func.o module_type.o 
module_stntbl.o : module_stntbl.F90
module_thin_ob.o : module_thin_ob.F90 module_namelist.o module_map_utils.o module_map.o module_mm5.o module_func.o module_type.o 
module_type.o : module_type.F90 
module_write.o : module_write.F90 constants.inc module_map.o module_decoded.o module_date.o module_func.o module_type.o 
obsproc.o : obsproc.F90 module_write.o module_thin_ob.o module_complete.o module_err_afwa.o module_err_ncep.o module_qc.o module_diagnostics.o module_recoverh.o module_recoverp.o module_per_type.o module_duplicate.o module_sort.o module_icao.o module_func.o module_type.o module_decoded.o module_namelist.o module_map_utils.o module_map.o module_mm5.o 
qc_reduction.o : qc_reduction.F90 missing.inc module_func.o module_type.o module_stntbl.o
setup.o : setup.F90 constants.inc 
sort_platform.o : sort_platform.F90 platform_interface.inc module_type.o 

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
