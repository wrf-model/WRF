#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f


MODULES =                           \
  module_chem_utilities.o           \
  module_data_radm2.o               \
  module_data_racm.o                \
  module_data_sorgam.o              \
  module_data_cbmz.o                \
  module_data_mosaic_asect.o        \
  module_data_mosaic_other.o        \
  module_data_mosaic_therm.o        \
  module_data_cmu_bulkaqchem.o      \
  module_peg_util.o                 \
  module_mosaic_csuesat.o           \
  module_cbmz_lsodes_solver.o       \
  module_cbmz_rodas3_solver.o       \
  module_cbmz_rodas_prep.o          \
  module_radm.o                     \
  module_racm.o                     \
  module_cbmz.o                     \
  module_phot_mad.o                 \
  module_dep_simple.o               \
  module_bioemi_simple.o            \
  module_bioemi_beis311.o           \
  module_vertmx_wrf.o               \
  module_aerosols_sorgam.o          \
  module_mosaic_movesect.o          \
  module_mosaic_therm.o             \
  module_mosaic_newnuc.o            \
  module_mosaic_coag.o              \
  module_mosaic_driver.o            \
  module_mosaic_drydep.o            \
  module_cbmz_addemiss.o            \
  module_mosaic_addemiss.o          \
  module_input_chem_data.o          \
  module_input_chem_bioemiss.o      \
  module_fastj_data.o               \
  module_fastj_mie.o                \
  module_phot_fastj.o               \
  module_ctrans_aqchem.o            \
  module_ctrans_grell.o             \
  module_emissions_anthropogenics.o \
  module_cbmz_initmixrats.o         \
  module_mosaic_initmixrats.o       \
  module_mosaic_wetscav.o           \
  module_cmu_dvode_solver.o         \
  module_cmu_bulkaqchem.o           \
  module_mosaic_cloudchem.o         \
  module_wetscav_driver.o           \
  module_mixactivate_wrappers.o


OBJS    =                           \
        chemics_init.o              \
        chem_driver.o               \
        cloudchem_driver.o          \
        photolysis_driver.o         \
        mechanism_driver.o          \
        emissions_driver.o          \
        dry_dep_driver.o            \
        aerosol_driver.o 


LIBTARGET    =  chemics
TARGETDIR    =  ./
$(LIBTARGET) :  $(MODULES) $(OBJS)
		$(AR) ../main/libwrflib.a $(MODULES) $(OBJS)

include ../configure.wrf

CPP = cpp

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

module_mixactivate_wrappers.o: ../phys/module_mixactivate.o

module_data_radm2.o: 

module_data_racm.o: 

module_chem.utilities.o: 

module_radm.o: 

module_racm.o: 

module_data_cbmz.o: 

module_peg_util.o: 

module_cbmz.o: 

module_cbmz_addemiss.o: 

module_cbmz_rodas_prep.o: 

module_cbmz_initmixrats.o: 

module_phot_mad.o: 

module_phot_fastj.o:

module_input_chem_data.o: module_aerosols_sorgam.o

module_input_chem_bioemiss.o: 

module_dep_simple.o: 

module_bioemi_simple.o: 

module_vertmx_wrf.o: 

module_emissions_anthropogenics.o: 

module_data_sorgam.o: 

module_aerosols_sorgam.o: 

module_cbmz_lsodes_solver.o: 

module_cbmz_rodas3_solver.o: 

module_mosaic_csuesat.o:

module_data_mosaic_asect.o:

module_data_mosaic_other.o:

module_data_mosaic_therm.o:

module_mosaic_addemiss.o: module_data_mosaic_asect.o

module_mosaic_drydep.o: module_peg_util.o module_data_mosaic_asect.o module_data_mosaic_other.o

module_mosaic_initmixrats.o: module_peg_util.o module_data_mosaic_asect.o module_data_mosaic_other.o

module_mosaic_movesect.o: module_peg_util.o module_data_mosaic_asect.o module_data_mosaic_other.o

module_mosaic_therm.o: module_peg_util.o module_data_mosaic_asect.o module_data_mosaic_other.o module_data_mosaic_therm.o module_mosaic_movesect.o

module_mosaic_newnuc.o: module_peg_util.o module_data_mosaic_asect.o module_data_mosaic_other.o module_mosaic_movesect.o

module_mosaic_coag.o: module_peg_util.o module_data_mosaic_asect.o module_data_mosaic_other.o module_mosaic_movesect.o

module_mosaic_driver.o: module_peg_util.o module_mosaic_csuesat.o module_data_mosaic_asect.o module_data_mosaic_other.o module_data_mosaic_therm.o module_mosaic_movesect.o module_mosaic_therm.o module_mosaic_newnuc.o module_mosaic_coag.o module_mosaic_wetscav.o

module_cmu_dvode_solver.o:

module_data_cmu_bulkaqchem.o:

module_cmu_bulkaqchem.o: module_data_cmu_bulkaqchem.o module_cmu_dvode_solver.o

module_mosaic_cloudchem.o: module_data_cmu_bulkaqchem.o module_cmu_bulkaqchem.o module_data_mosaic_asect.o module_mosaic_movesect.o module_mosaic_driver.o

module_fastj_data.o:

module_fastj_mie.o:

module_ctrans_grell.o:

chem_driver.o: module_radm.o module_racm.o module_data_racm.o module_chem_utilities.o module_data_radm2.o module_dep_simple.o module_bioemi_simple.o module_vertmx_wrf.o module_phot_mad.o module_aerosols_sorgam.o module_data_cbmz.o module_cbmz.o module_wetscav_driver.o

chemics_init.o: module_phot_mad.o module_aerosols_sorgam.o

aerosol_driver.o: module_aerosols_sorgam.o

cloudchem_driver.o: module_mosaic_cloudchem.o

photolysis_driver.o: module_phot_mad.o module_phot_fastj.o

mechanism_driver.o: module_data_radm2.o module_radm.o module_data_racm.o module_aerosols_sorgam.o module_data_cbmz.o module_cbmz.o

emissions_driver.o: module_data_radm2.o module_radm.o module_bioemi_simple.o module_bioemi_beis311.o module_emissions_anthropogenics.o module_cbmz_addemiss.o module_mosaic_addemiss.o

dry_dep_driver.o: module_data_radm2.o module_dep_simple.o module_aerosols_sorgam.o module_mosaic_drydep.o ../phys/module_mixactivate.o

convert_bioemiss : convert_bioemiss.o
	$(RANLIB) ../main/libwrflib.a
	$(FC) -o convert_bioemiss.exe $(LDFLAGS) convert_bioemiss.o ../main/libwrflib.a $(LIB)

convert_emiss : convert_emiss.o
	$(RANLIB) ../main/libwrflib.a
	$(FC) -o convert_emiss.exe $(LDFLAGS) convert_emiss.o ../main/libwrflib.a $(LIB)
