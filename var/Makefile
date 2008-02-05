
clean:
	( cd da; /bin/rm -f *.o *.mod *.f *.c *.m4 *.stb *.out core *~ mpif.h da_generic_boilerplate.inc )
	( cd gen_be; /bin/rm -f *.o *.f core *~ )

superclean :
	make clean
	( cd da; /bin/rm -f mpif.h; /bin/rm -f *.inc; /bin/rm -f *.f90 *.c *.m4 *.exe )
	( cd gen_be; /bin/rm -f *.exe )
