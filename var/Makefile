
clean:
	( cd build; /bin/rm -f *.o *.mod *.f *.c *.m4 *.stb *.out core *~ mpif.h da_generic_boilerplate.inc )
	( cd gen_be; /bin/rm -f *.o *.f core *~ )

superclean :
	( cd build; /bin/rm -f mpif.h; /bin/rm -f *.inc *.prm; /bin/rm -f *.f90 *.c *.m4 *.exe; \
          /bin/rm -rf *.o *.mod *.b *.a *.f frame/*.F diffwrf *.f90.tmp *.f.tmp *.bb \
          Registry_tmp* module_state_description.f90 \
          inc/dm_comm_cpp_flags testall.csh *.exe *.f90 *.F90 *.F inc/*.inc *.int *.c *.h *.m4 *.code \
          *.sed inc/rsl_cpp_flags inc/version_decl module_dm_warning \
          Test1.out.correct \
          links README* landread.c.dist \
          depend inc/*.inc inc/*.h preproc.sh )
	( cd da; /bin/rm -f *.exe )
	( cd external/bufr; make clean )
	( cd external/crtm_2.2.3; make clean )
	( cd external/wavelet ; make superclean )
	( cd da/makedepf90-2.8.8; /bin/rm -f makedepf90 config.log Makefile config.status )
	( cd gen_be; /bin/rm -f *.exe )
