!*******************************************************************************
!Subroutine - rapid_read_namelist
!*******************************************************************************
subroutine rapid_read_namelist

!Purpose:
!This subroutine allows to read the RAPID namelist and hence to run the model
!multiple times without ever have to recompile.  Some information on the options
!used is also printed in the stdout.
!Author: 
!Cedric H. David, 2011-2015. 


!*******************************************************************************
!Declaration of variables
!*******************************************************************************
use rapid_var, only :                                                          &
                     NL_namelist,namelist_file


implicit none


!*******************************************************************************
!Read namelist file 
!*******************************************************************************
open(88,file=namelist_file,status='old',form='formatted')
read(88, NL_namelist)
close(88)


!*******************************************************************************
!Optional prints what was read 
!*******************************************************************************
!print *, namelist_file


end subroutine rapid_read_namelist
