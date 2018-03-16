.SUFFIXES: .c .o

CC_TOOLS = cc
CFLAGS = $(CC_TOOLS_CFLAGS) #-ansi
LDFLAGS =
DEBUG = -g 
OBJ = registry.o my_strtok.o reg_parse.o data.o type.o misc.o \
      gen_defs.o gen_allocs.o gen_mod_state_descr.o gen_scalar_indices.o \
      gen_args.o gen_config.o sym.o symtab_gen.o gen_irr_diag.o \
      gen_model_data_ord.o gen_interp.o gen_comms.o gen_scalar_derefs.o set_dim_strs.o gen_wrf_io.o\
      gen_streams.o

registry : $(OBJ) standard.exe
	$(CC_TOOLS) -o registry $(DEBUG) $(LDFLAGS) $(OBJ)

standard.exe : standard.o
	$(CC_TOOLS) -o standard.exe $(DEBUG) $(LDFLAGS) standard.o

.c.o :
	$(CC_TOOLS) $(CFLAGS) -c $(DEBUG) $<

clean:
	/bin/rm -f $(OBJ) gen_comms.c standard.o

gen_comms.c : gen_comms.stub
	/bin/cp gen_comms.stub gen_comms.c

# regenerate this list with "makedepend -Y *.c"

# DO NOT DELETE THIS LINE -- make depend depends on it.

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
gen_streams.o: registry.h protos.h data.h
