#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f


MODULES =                           \
  module_chem_utilities.o           \
  module_data_radm2.o               \
  module_data_racm.o                \
  module_data_sorgam.o              \
  module_data_isrpia.o              \
  module_isrpia.o                   \
  module_input_chem_emissopt3.o     \
  module_plumerise1.o     \
  module_add_emiss_burn.o     \
  module_zero_plumegen_coms.o     \
  module_chem_plumerise_scalar.o     \
  module_data_cbmz.o                \
  module_data_mosaic_asect.o        \
  module_data_mosaic_other.o        \
  module_data_mosaic_therm.o        \
  module_data_cmu_bulkaqchem.o      \
  module_data_megan2.o              \
  module_data_mgn2mech.o            \
  module_peg_util.o                 \
  module_mosaic_csuesat.o           \
  module_cbmz_lsodes_solver.o       \
  module_cbmz_rodas3_solver.o       \
  module_cbmz_rodas_prep.o          \
  module_data_gocart_seas.o        \
  module_data_gocartchem.o        \
  module_gocart_chem.o                     \
  module_gocart_dust.o                     \
  module_gocart_seasalt.o                     \
  module_gocart_dmsemis.o                     \
  module_gocart_settling.o                     \
  module_gocart_aerosols.o                     \
  module_gocart_drydep.o                     \
  module_radm.o                     \
  module_racm.o                     \
  module_cbmz.o                     \
  module_phot_mad.o                 \
  module_dep_simple.o               \
  module_bioemi_simple.o            \
  module_bioemi_beis313.o           \
  module_bioemi_megan2.o            \
  module_vertmx_wrf.o               \
  module_mosaic_movesect.o          \
  module_mosaic_therm.o             \
  module_mosaic_newnuc.o            \
  module_mosaic_coag.o              \
  module_mosaic_driver.o            \
  module_mosaic_drydep.o            \
  module_sorgam_cloudchem.o         \
  module_aerosols_sorgam.o          \
  module_add_emis_cptec.o           \
  module_cbmz_addemiss.o            \
  module_cbm4_addemiss.o            \
  module_mosaic_addemiss.o          \
  module_input_chem_data.o          \
  module_input_chem_bioemiss.o      \
  module_input_dust_errosion.o      \
  module_input_gocart_background.o  \
  module_input_gocart_dms.o         \
  module_fastj_data.o               \
  module_fastj_mie.o                \
  module_phot_fastj.o               \
  module_ftuv_driver.o              \
  module_ftuv_subs.o                \
  module_wave_data.o                \
  module_ctrans_aqchem.o            \
  module_ctrans_grell.o             \
  module_convtrans_prep.o             \
  module_emissions_anthropogenics.o \
  module_cbmz_initmixrats.o         \
  module_cbm4_initmixrats.o         \
  module_optical_averaging.o        \
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
        optical_driver.o         \
        mechanism_driver.o          \
        emissions_driver.o          \
        dry_dep_driver.o            \
        aerosol_driver.o 


LIBTARGET    =  chemics
TARGETDIR    =  ./
$(LIBTARGET) :  $(MODULES) $(OBJS)
		$(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(MODULES) $(OBJS)

include ../configure.wrf

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

module_mixactivate_wrappers.o: ../phys/module_mixactivate.o

module_data_radm2.o: 

module_data_gocartchem.o: 

module_data_racm.o: 

module_chem.utilities.o: 

module_radm.o: 

module_gocart_dust.o: module_data_gocart_dust.o

module_gocart_seasalt.o: module_data_gocart_seas.o

module_gocart_chem.o: module_data_gocartchem.o module_phot_mad.o

module_gocart_settling.o: module_data_gocart_dust.o

module_gocart_aerosols.o: 

module_gocart_drydep.o: 

module_gocart_dmsemis.o: 

module_racm.o: 

module_data_cbmz.o: 

module_peg_util.o: 

module_cbmz.o:

module_chem_plumerise_scalar.o: 

module_plumerise1.o: module_zero_plumegen_coms.o module_chem_plumerise_scalar.o

module_add_emiss_burn.o:

module_zero_plumegen_coms.o: 

module_add_emis_cptec.o: 

module_cbmz_addemiss.o: module_aerosols_sorgam.o

module_cbm4_addemiss.o: 

module_cbmz_rodas_prep.o: 

module_cbmz_initmixrats.o: 

module_cbm4_initmixrats.o: 

module_phot_mad.o: 

module_phot_fastj.o:

module_ftuv_driver.o: module_wave_data.o module_ftuv_subs.o

module_ftuv_subs.o:  

module_wave_data.o:  

module_input_chem_data.o: module_aerosols_sorgam.o

module_input_chem_emissopt3.o: 

module_input_chem_bioemiss.o: 

module_input_dust_errosion.o:

module_input_gocart_background.o:

module_input_gocart_dms.o:

module_dep_simple.o: 

module_bioemi_simple.o: 

module_data_megan2.o: 

module_data_mgn2mech.o: 

module_bioemi_megan2.o: module_data_megan2.o module_data_mgn2mech.o

module_vertmx_wrf.o: 

module_emissions_anthropogenics.o: 

module_data_sorgam.o: 

module_data_isrpia.o:

module_isrpia.o: module_data_isrpia.o module_mosaic_addemiss.o

module_aerosols_sorgam.o: module_isrpia.o

module_cbmz_lsodes_solver.o: 

module_cbmz_rodas3_solver.o: 

module_mosaic_csuesat.o:

module_data_mosaic_asect.o:

module_data_mosaic_other.o:

module_data_mosaic_therm.o:

module_mosaic_addemiss.o: module_data_mosaic_asect.o 

module_aerosols_sorgam.o: module_mosaic_addemiss.F

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

module_sorgam_cloudchem.o: module_data_cmu_bulkaqchem.o module_cmu_bulkaqchem.o module_data_sorgam.o module_aerosols_sorgam.o

module_fastj_data.o:

module_fastj_mie.o:

module_ctrans_grell.o:

module_convtrans_prep.o:

chem_driver.o: module_radm.o module_convtrans_prep.o module_racm.o module_data_racm.o module_chem_utilities.o module_data_radm2.o module_dep_simple.o module_bioemi_simple.o module_vertmx_wrf.o module_phot_mad.o module_aerosols_sorgam.o module_data_cbmz.o module_cbmz.o module_wetscav_driver.o dry_dep_driver.o emissions_driver.o

chemics_init.o: module_phot_mad.o module_gocart_chem.o module_aerosols_sorgam.o module_mixactivate_wrappers.o

aerosol_driver.o: module_data_sorgam.o module_aerosols_sorgam.o module_mosaic_driver.o

cloudchem_driver.o: module_mosaic_cloudchem.o module_sorgam_cloudchem.o

photolysis_driver.o: module_phot_mad.o module_phot_fastj.o module_ftuv_driver.o

mechanism_driver.o: module_data_radm2.o module_radm.o module_data_racm.o module_aerosols_sorgam.o module_data_cbmz.o module_cbmz.o

emissions_driver.o: module_add_emiss_burn.o module_data_radm2.o module_radm.o module_bioemi_simple.o module_bioemi_beis313.o module_bioemi_megan2.o module_emissions_anthropogenics.o module_cbmz_addemiss.o module_mosaic_addemiss.o module_aerosols_sorgam.o module_plumerise1.o module_gocart_dust.o module_gocart_seasalt.o

dry_dep_driver.o: module_data_radm2.o module_dep_simple.o module_aerosols_sorgam.o module_mosaic_drydep.o ../phys/module_mixactivate.o

convert_gocart : convert_gocart.o
	$(RANLIB) ../main/$(LIBWRFLIB)
	$(FC) -o convert_gocart.exe $(LDFLAGS) convert_gocart.o ../main/$(LIBWRFLIB) $(LIB)

convert_dms : convert_dms.o
	$(RANLIB) ../main/$(LIBWRFLIB)
	$(FC) -o convert_dms.exe $(LDFLAGS) convert_dms.o ../main/$(LIBWRFLIB) $(LIB)

convert_dust : convert_dust.o
	$(RANLIB) ../main/$(LIBWRFLIB)
	$(FC) -o convert_dust.exe $(LDFLAGS) convert_dust.o ../main/$(LIBWRFLIB) $(LIB)

convert_bioemiss : convert_bioemiss.o
	$(RANLIB) ../main/$(LIBWRFLIB)
	$(FC) -o convert_bioemiss.exe $(LDFLAGS) convert_bioemiss.o ../main/$(LIBWRFLIB) $(LIB)

convert_bioemiss_megan2 : convert_bioemiss_megan2.o module_input_chem_bioemiss.o 
	$(RANLIB) ../main/$(LIBWRFLIB)
	$(FC) -o convert_bioemiss_megan2.exe $(LDFLAGS) convert_bioemiss_megan2.o ../main/$(LIBWRFLIB) $(LIB)

convert_emiss : convert_emiss.o
	$(RANLIB) ../main/$(LIBWRFLIB)
	$(FC) -o convert_emiss.exe $(LDFLAGS) convert_emiss.o ../main/$(LIBWRFLIB) $(LIB)

convert_fireemiss : convert_fireemiss.o
	$(RANLIB) ../main/$(LIBWRFLIB)
	$(FC) -o convert_fireemiss.exe $(LDFLAGS) convert_fireemiss.o ../main/$(LIBWRFLIB) $(LIB)
