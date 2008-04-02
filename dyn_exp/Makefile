#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f


MODULES =                 		\
        module_exp.o         		\
	$(CASE_MODULE)

# possible CASE_MODULE settings
#	module_initialize_exp.o        

OBJS    = 		          	\
	solve_exp.o init_modules_exp.o

LIBTARGET    =  dyn_exp
TARGETDIR    =  ./
$(LIBTARGET) :  $(MODULES) $(OBJS)
		$(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(MODULES) $(OBJS)

include ../configure.wrf

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

solve_exp.o:   module_exp.o

