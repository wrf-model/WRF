
#FC = f90 -O2 -O -w -g
#FC = f90 -O -g
#FC = f90 -O
#----------------------
# PC Linux
#---------------------
#FC = pgf90 -byteswapio

#----------------------
# IBM XLF
#---------------------
FC = xlf90 -qsuffix=f=f90 -qrealsize=8

all:
	@ ${FC} -o da_bias_sele.exe rad_bias.f90 da_bias_sele.f90
	@ ${FC} -o da_bias_scan.exe rad_bias.f90 da_bias_scan.f90
	@ ${FC} -o da_bias_airmass.exe rad_bias.f90 pythag.f90 tqli.f90 tred2.f90 regress_one.f90 da_bias_airmass.f90
	@ ${FC} -o da_bias_verif.exe rad_bias.f90 da_bias_verif.f90
clean:
	@ rm *.mod *.exe
        
