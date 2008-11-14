!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for references and terms of use
!**********************************************************************************  
!   file module_data_mosaic_other.f
!-----------------------------------------------------------------------

	module module_data_mosaic_other


! rce 11-sep-2004 changes
!    increased lmaxd,l2maxd 
!    initialize khno3,...,ktemp,ltot,ltot2 with bogus values;
!    added aboxtest_... variables
!    no initialization of name()
! rce 06-may-2005 - increased lmaxd from 184 (no cw) to 283 (cw);
!    added rcldwtr_sub


	integer, parameter :: imaxd=1, jmaxd=1, kmaxd=100

	integer, parameter :: lmaxd=283, l2maxd=283

	integer, parameter :: nsubareamaxd = 1

!   rce 2005-mar-09 - added k_pegbegin
!   k_pegbegin = starting k index for pegasus arrays
	integer, parameter :: k_pegbegin = 1


	integer, save :: khno3   = -999888777
	integer, save :: kh2so4  = -999888777
	integer, save :: knh3    = -999888777
	integer, save :: khcl    = -999888777
	integer, save :: ko3     = -999888777
	integer, save :: kh2o    = -999888777
	integer, save :: ktemp   = -999888777

!   rce 2005-apr-12 - added for cldchem - kso2, kh2o2, khcho, khcooh, 
!   koh, kho2, kno3, kno, kno2, khono, kpan, kch3o2, kch3oh, kch3ooh
	integer, save :: kso2    = -999888777
	integer, save :: kh2o2   = -999888777
	integer, save :: khcho   = -999888777
	integer, save :: khcooh  = -999888777
	integer, save :: koh     = -999888777
	integer, save :: kho2    = -999888777
	integer, save :: kno3    = -999888777
	integer, save :: kno     = -999888777
	integer, save :: kno2    = -999888777
	integer, save :: khono   = -999888777
	integer, save :: kpan    = -999888777
	integer, save :: kch3o2  = -999888777
	integer, save :: kch3oh  = -999888777
	integer, save :: kch3ooh = -999888777

!   the following values are correct for wrfchem simulations
	integer, save :: lunerr=-1, lunout=-1

	integer, save :: ltot=+999888777, ltot2=+999888777

	integer, save :: itot, jtot, ktot
	integer, save :: isvode, jsvode, ksvode, msvode
	integer, save :: iymdcur, ihmscur
	integer, save :: ncorecnt
	integer, save :: nsubareas


	real, parameter :: pi = 3.14159265

	real, save :: afracsubarea(kmaxd,nsubareamaxd)
	real, save :: cairclm(kmaxd)
	real, save :: ptotclm(kmaxd)
	real, save :: rclm(kmaxd,l2maxd)
	real, save :: relhumclm(kmaxd)
	real, save :: rcldwtr_sub(kmaxd,nsubareamaxd)
	real, save :: rsub(l2maxd,kmaxd,nsubareamaxd)
	real, save :: t


	character(len=20), save :: name(l2maxd)


!   control variables for box-model testing
!   the following values are correct for wrfchem simulations
	integer, save :: aboxtest_testmode = 0
	integer, save :: aboxtest_units_convert = 1
	integer, save :: aboxtest_rh_method = 1
	integer, save :: aboxtest_map_method = 1
	integer, save :: aboxtest_gases_fixed = 0

	real, save :: aboxtest_min_temp = 233.0
	real, save :: aboxtest_min_relhum = 0.05
	real, save :: aboxtest_max_relhum = 0.98


	end module module_data_mosaic_other
