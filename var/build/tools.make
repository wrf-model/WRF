# tools

TOOLS_OBJ = registry.o my_strtok.o reg_parse.o data.o type.o misc.o \
      gen_defs.o gen_allocs.o gen_mod_state_descr.o gen_scalar_indices.o \
      gen_args.o gen_config.o sym.o symtab_gen.o gen_wrf_io.o \
      gen_model_data_ord.o gen_interp.o gen_comms.o gen_scalar_derefs.o

$(TOOLS_OBJ) :
	$(RM) $@
	$(CC_TOOLS) -c $(CCFLAGS) $*.c

registry : $(TOOLS_OBJ)
	$(CC_TOOLS) -o registry $(TOOLS_OBJ)

advance_cymdh.exe : advance_cymdh.o 
	$(RM) $@
	$(SFC) $(LDFLAGS) -o advance_cymdh.exe advance_cymdh.o

data.o: registry.h protos.h data.h
gen_allocs.o: protos.h registry.h data.h
gen_args.o: protos.h registry.h data.h
gen_scalar_derefs.o: protos.h registry.h data.h
gen_config.o: protos.h registry.h data.h
gen_defs.o: protos.h registry.h data.h
gen_mod_state_descr.o: protos.h registry.h data.h
gen_model_data_ord.o: protos.h registry.h data.h
gen_scalar_indices.o: protos.h registry.h data.h
gen_wrf_io.o: protos.h registry.h data.h
misc.o: protos.h registry.h data.h
my_strtok.o: registry.h protos.h data.h
reg_parse.o: registry.h protos.h data.h
registry.o: protos.h registry.h data.h
sym.o: sym.h
type.o: registry.h protos.h data.h
gen_interp.o: registry.h protos.h data.h
