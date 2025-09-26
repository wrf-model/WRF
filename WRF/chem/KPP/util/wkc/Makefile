.SUFFIXES: .c .o

#CC = cc

include ../../configure.kpp

CFLAGS = #-ansi
LDFLAGS =
DEBUG = -g 
OBJ = registry_kpp.o  my_strtok.o data.o type.o misc.o reg_parse.o \
      gen_kpp.o get_wrf_chem_specs.o gen_kpp_mech_dr.o gen_kpp_interface.o \
      get_kpp_chem_specs.o compare_kpp_to_species.o get_wrf_radicals.o \
      get_wrf_jvals.o gen_kpp_utils.o change_chem_Makefile.o  \
      gen_kpp_interf_utils.o gen_kpp_args_to_Update_Rconst.o  kpp_data.o \
      sym.o symtab_gen.o



registry : $(OBJ)
	$(SCC) -o registry_kpp $(DEBUG) $(LDFLAGS) $(OBJ)

.c.o :
	$(SCC) -I../../../../inc $(CFLAGS) -c $(DEBUG) $<

clean:
	@/bin/rm -f $(OBJ) gen_comms.c

gen_comms.c : gen_comms.stub
	/bin/cp gen_comms.stub gen_comms.c

# regenerate this list with "makedepend -Y *.c"

# DO NOT DELETE THIS LINE -- make depend depends on it.

data.o: registry.h protos.h data.h  protos_kpp.h
reg_parse.o: registry.h protos.h data.h  protos_kpp.h
registry.o: protos.h registry.h data.h  protos_kpp.h
kpp_data.o: registry.h protos.h data.h kpp_data.h  protos_kpp.h
gen_kpp.o:registry.h protos.h data.h  protos_kpp.h kpp_data.h \
          gen_kpp_mech_dr.o \
          gen_kpp_interface.o  get_wrf_chem_specs.o get_kpp_chem_specs.o \
          compare_kpp_to_species.o get_wrf_radicals.o  get_wrf_jvals.o \
          gen_kpp_utils.o change_chem_Makefile.o gen_kpp_interf_utils.o \
          gen_kpp_args_to_Update_Rconst.o copy_makefiles_kpp.o
gen_kpp_mech_dr.o:protos.h kpp_data.h gen_kpp_utils.o  protos_kpp.h
gen_kpp_interface.o:registry.h protos.h data.h kpp_data.h  protos_kpp.h
get_wrf_chem_specs.o:registry.h protos.h data.h kpp_data.h  protos_kpp.h
get_kpp_chem_specs.o:registry.h protos.h data.h kpp_data.h protos_kpp.h
compare_kpp_to_species.o:data.h kpp_data.h
get_wrf_radicals.o:registry.h protos.h data.h kpp_data.h  protos_kpp.h 
get_wrf_jvals.o:registry.h protos.h data.h kpp_data.h  protos_kpp.h
copy_makefiles_kpp.o:protos.h kpp_data.h  protos_kpp.h
gen_kpp_utils.o:registry.h kpp_data.h 
gen_kpp_interf_utils.o: protos.h kpp_data.h  protos_kpp.h
change_chem_Makefile.o: protos.h kpp_data.h  protos_kpp.h
gen_kpp_args_to_Update_Rconst.o: protos.h kpp_data.h  protos_kpp.h
sym.o: sym.h

