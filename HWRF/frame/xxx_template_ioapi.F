!
! This is a template for adding a package-dependent implemetnation of
! the I/O API.  You can use the name xxx since that is already set up
! as a placeholder in module_io.F, md_calls.m4, and the Registry, or
! you can change the name here and in those other places.  For additional
! information on adding a package to WRF, see the latest version of the
! WRF Design and Implementation Document 1.1 (Draft). June 21, 2001
!

!--- ioinit
SUBROUTINE ext_xxx_ioinit( SysDepInfo,Status )
  IMPLICIT NONE
  CHARACTER (LEN=80), INTENT(IN) :: SysDepInfo
  INTEGER, INTENT(INOUT) :: Status
  RETURN  
END SUBROUTINE ext_xxx_ioinit

SUBROUTINE ext_xxx_inquiry( Inquiry, Result,Status )
  IMPLICIT NONE
  CHARACTER *(*), INTENT(IN) :: Inquiry
  CHARACTER *(*), INTENT(OUT) :: Result
  INTEGER, INTENT(INOUT) :: Status
  RETURN  
END SUBROUTINE ext_xxx_inquiry

!--- ioexit
SUBROUTINE ext_xxx_ioexit( Status )
  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: Status
  RETURN  
END SUBROUTINE

!--- open_for_write_begin
SUBROUTINE ext_xxx_open_for_write_begin( FileName , Comm_compute, Comm_io, SysDepInfo, &
                                     DataHandle , Status )
  IMPLICIT NONE
  CHARACTER *(*), INTENT(IN)  :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER *(*), INTENT(IN)  :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  RETURN  
END SUBROUTINE ext_xxx_open_for_write_begin

!--- open_for_write_commit
SUBROUTINE ext_xxx_open_for_write_commit( DataHandle , Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN ) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  RETURN  
END SUBROUTINE ext_xxx_open_for_write_commit

!--- open_for_read 
SUBROUTINE ext_xxx_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               DataHandle , Status )
  IMPLICIT NONE
  CHARACTER *(*), INTENT(IN)  :: FileName
  INTEGER ,       INTENT(IN)  :: Comm_compute , Comm_io
  CHARACTER *(*), INTENT(IN)  :: SysDepInfo
  INTEGER ,       INTENT(OUT) :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  RETURN  
END SUBROUTINE ext_xxx_open_for_read

!--- inquire_opened
SUBROUTINE ext_xxx_inquire_opened ( DataHandle, FileName , FileStatus, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER *(*), INTENT(IN)  :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status
  RETURN
END SUBROUTINE ext_xxx_inquire_opened

!--- inquire_filename
SUBROUTINE ext_xxx_inquire_filename ( DataHandle, FileName , FileStatus, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER *(*), INTENT(OUT) :: FileName
  INTEGER ,       INTENT(OUT) :: FileStatus
  INTEGER ,       INTENT(OUT) :: Status
END SUBROUTINE ext_xxx_inquire_filename

!--- sync
SUBROUTINE ext_xxx_iosync ( DataHandle, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  RETURN
END SUBROUTINE ext_xxx_iosync

!--- close
SUBROUTINE ext_xxx_ioclose ( DataHandle, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  INTEGER ,       INTENT(OUT) :: Status
  RETURN
END SUBROUTINE ext_xxx_ioclose

!--- get_next_time (not defined for IntIO )
SUBROUTINE ext_xxx_get_next_time ( DataHandle, DateStr, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(OUT) :: DateStr
  INTEGER ,       INTENT(OUT) :: Status
  RETURN
END SUBROUTINE ext_xxx_get_next_time

!--- set_time
SUBROUTINE ext_xxx_set_time ( DataHandle, DateStr, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  INTEGER ,       INTENT(OUT) :: Status
  RETURN
END SUBROUTINE ext_xxx_set_time

!--- get_next_var  (not defined for IntIO)
SUBROUTINE ext_xxx_get_next_var ( DataHandle, VarName, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(OUT) :: VarName
  INTEGER ,       INTENT(OUT) :: Status
  RETURN
END SUBROUTINE ext_xxx_get_next_var

!--- get_dom_ti_real
SUBROUTINE ext_xxx_get_dom_ti_real ( DataHandle,Element,   Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
RETURN
END SUBROUTINE ext_xxx_get_dom_ti_real 

!--- put_dom_ti_real
SUBROUTINE ext_xxx_put_dom_ti_real ( DataHandle,Element,   Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_dom_ti_real 

!--- get_dom_ti_double
SUBROUTINE ext_xxx_get_dom_ti_double ( DataHandle,Element,   Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_dom_ti_double 

!--- put_dom_ti_double
SUBROUTINE ext_xxx_put_dom_ti_double ( DataHandle,Element,   Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_dom_ti_double 

!--- get_dom_ti_integer
SUBROUTINE ext_xxx_get_dom_ti_integer ( DataHandle,Element,   Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  integer ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_dom_ti_integer 

!--- put_dom_ti_integer
SUBROUTINE ext_xxx_put_dom_ti_integer ( DataHandle,Element,   Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  integer ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_dom_ti_integer 

!--- get_dom_ti_logical
SUBROUTINE ext_xxx_get_dom_ti_logical ( DataHandle,Element,   Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  logical ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_dom_ti_logical 

!--- put_dom_ti_logical
SUBROUTINE ext_xxx_put_dom_ti_logical ( DataHandle,Element,   Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_dom_ti_logical 

!--- get_dom_ti_char
SUBROUTINE ext_xxx_get_dom_ti_char ( DataHandle,Element,   Data,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(OUT) :: Data
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_dom_ti_char 

!--- put_dom_ti_char
SUBROUTINE ext_xxx_put_dom_ti_char ( DataHandle,Element,   Data,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN) :: Data
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_dom_ti_char 

!--- get_dom_td_real
SUBROUTINE ext_xxx_get_dom_td_real ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  real ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_dom_td_real 

!--- put_dom_td_real
SUBROUTINE ext_xxx_put_dom_td_real ( DataHandle,Element, DateStr,  Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_dom_td_real 

!--- get_dom_td_double
SUBROUTINE ext_xxx_get_dom_td_double ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_dom_td_double 

!--- put_dom_td_double
SUBROUTINE ext_xxx_put_dom_td_double ( DataHandle,Element, DateStr,  Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_dom_td_double 

!--- get_dom_td_integer
SUBROUTINE ext_xxx_get_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  integer ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_dom_td_integer 

!--- put_dom_td_integer
SUBROUTINE ext_xxx_put_dom_td_integer ( DataHandle,Element, DateStr,  Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  integer ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_dom_td_integer 

!--- get_dom_td_logical
SUBROUTINE ext_xxx_get_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  logical ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_dom_td_logical 

!--- put_dom_td_logical
SUBROUTINE ext_xxx_put_dom_td_logical ( DataHandle,Element, DateStr,  Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_dom_td_logical 

!--- get_dom_td_char
SUBROUTINE ext_xxx_get_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(OUT) :: Data
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_dom_td_char 

!--- put_dom_td_char
SUBROUTINE ext_xxx_put_dom_td_char ( DataHandle,Element, DateStr,  Data,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN) :: Data
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_dom_td_char 

!--- get_var_ti_real
SUBROUTINE ext_xxx_get_var_ti_real ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_var_ti_real 

!--- put_var_ti_real
SUBROUTINE ext_xxx_put_var_ti_real ( DataHandle,Element,  Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_var_ti_real 

!--- get_var_ti_double
SUBROUTINE ext_xxx_get_var_ti_double ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_var_ti_double 

!--- put_var_ti_double
SUBROUTINE ext_xxx_put_var_ti_double ( DataHandle,Element,  Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_var_ti_double 

!--- get_var_ti_integer
SUBROUTINE ext_xxx_get_var_ti_integer ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  integer ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_var_ti_integer 

!--- put_var_ti_integer
SUBROUTINE ext_xxx_put_var_ti_integer ( DataHandle,Element,  Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  integer ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_var_ti_integer 

!--- get_var_ti_logical
SUBROUTINE ext_xxx_get_var_ti_logical ( DataHandle,Element,  Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  logical ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_var_ti_logical 

!--- put_var_ti_logical
SUBROUTINE ext_xxx_put_var_ti_logical ( DataHandle,Element,  Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_var_ti_logical 

!--- get_var_ti_char
SUBROUTINE ext_xxx_get_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  CHARACTER*(*) , INTENT(OUT) :: Data
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_var_ti_char 

!--- put_var_ti_char
SUBROUTINE ext_xxx_put_var_ti_char ( DataHandle,Element,  Varname, Data,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  CHARACTER*(*) , INTENT(IN) :: Data
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_var_ti_char 

!--- get_var_td_real
SUBROUTINE ext_xxx_get_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_var_td_real 

!--- put_var_td_real
SUBROUTINE ext_xxx_put_var_td_real ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_var_td_real 

!--- get_var_td_double
SUBROUTINE ext_xxx_get_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real*8 ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_var_td_double 

!--- put_var_td_double
SUBROUTINE ext_xxx_put_var_td_double ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  real*8 ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_var_td_double 

!--- get_var_td_integer
SUBROUTINE ext_xxx_get_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  integer ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_var_td_integer 

!--- put_var_td_integer
SUBROUTINE ext_xxx_put_var_td_integer ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  integer ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_var_td_integer 

!--- get_var_td_logical
SUBROUTINE ext_xxx_get_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count, Outcount, Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  logical ,            INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT)  :: OutCount
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_var_td_logical 

!--- put_var_td_logical
SUBROUTINE ext_xxx_put_var_td_logical ( DataHandle,Element,  DateStr,Varname, Data, Count,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  logical ,            INTENT(IN) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_var_td_logical 

!--- get_var_td_char
SUBROUTINE ext_xxx_get_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  CHARACTER*(*) , INTENT(OUT) :: Data
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_var_td_char 

!--- put_var_td_char
SUBROUTINE ext_xxx_put_var_td_char ( DataHandle,Element,  DateStr,Varname, Data,  Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) , INTENT(IN)  :: Element
  CHARACTER*(*) , INTENT(IN)  :: DateStr
  CHARACTER*(*) , INTENT(IN)  :: VarName 
  CHARACTER*(*) , INTENT(IN) :: Data
  INTEGER ,       INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_put_var_td_char 

!--- read_field
SUBROUTINE ext_xxx_read_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm &
                            DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) , INTENT(INOUT) :: DateStr
  CHARACTER*(*) , INTENT(INOUT) :: VarName
  INTEGER ,       INTENT(INOUT) :: Field(*)
  integer                       ,intent(in)    :: FieldType
  integer                       ,intent(inout) :: Comm
  integer                       ,intent(inout) :: IOComm
  integer                       ,intent(in)    :: DomainDesc
  character*(*)                 ,intent(in)    :: MemoryOrder
  character*(*)                 ,intent(in)    :: Stagger
  character*(*) , dimension (*) ,intent(in)    :: DimNames
  integer ,dimension(*)         ,intent(in)    :: DomainStart, DomainEnd
  integer ,dimension(*)         ,intent(in)    :: MemoryStart, MemoryEnd
  integer ,dimension(*)         ,intent(in)    :: PatchStart,  PatchEnd
  integer                       ,intent(out)   :: Status
RETURN
END SUBROUTINE ext_xxx_read_field

!--- write_field
SUBROUTINE ext_xxx_write_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm , &
                             DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                             DomainStart , DomainEnd ,                                    &
                             MemoryStart , MemoryEnd ,                                    &
                             PatchStart , PatchEnd ,                                      &
                             Status )
  IMPLICIT NONE
  INTEGER ,       INTENT(IN)    :: DataHandle 
  CHARACTER*(*) , INTENT(IN)    :: DateStr
  CHARACTER*(*) , INTENT(IN)    :: VarName
  INTEGER ,       INTENT(IN)    :: Field(*)
  integer                       ,intent(in)    :: FieldType
  integer                       ,intent(inout) :: Comm
  integer                       ,intent(inout) :: IOComm
  integer                       ,intent(in)    :: DomainDesc
  character*(*)                 ,intent(in)    :: MemoryOrder
  character*(*)                 ,intent(in)    :: Stagger
  character*(*) , dimension (*) ,intent(in)    :: DimNames
  integer ,dimension(*)         ,intent(in)    :: DomainStart, DomainEnd
  integer ,dimension(*)         ,intent(in)    :: MemoryStart, MemoryEnd
  integer ,dimension(*)         ,intent(in)    :: PatchStart,  PatchEnd
  integer                       ,intent(out)   :: Status
RETURN
END SUBROUTINE ext_xxx_write_field

!--- get_var_info  (not implemented for IntIO)
SUBROUTINE ext_xxx_get_var_info ( DataHandle , VarName , NDim , MemoryOrder , Stagger , &
                              DomainStart , DomainEnd , Status )
  IMPLICIT NONE
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: VarName
  integer               ,intent(out)    :: NDim
  character*(*)         ,intent(out)    :: MemoryOrder
  character*(*)         ,intent(out)    :: Stagger
  integer ,dimension(*) ,intent(out)    :: DomainStart, DomainEnd
  integer               ,intent(out)    :: Status
RETURN
END SUBROUTINE ext_xxx_get_var_info


!--- end_of_frame
SUBROUTINE ext_xxx_end_of_frame(DataHandle, Status)
  IMPLICIT NONE
  INTEGER, INTENT(IN)  :: DataHandle
  INTEGER, INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_end_of_frame


!--- error_str
SUBROUTINE ext_xxx_error_str( Code, ReturnString, Status)
  IMPLICIT NONE
  INTEGER       , INTENT(IN)  :: Code
  CHARACTER *(*), INTENT(OUT) :: ReturnString
  INTEGER       , INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_error_str


!--- get_previous_time
SUBROUTINE ext_xxx_get_previous_time(DataHandle, DateStr, Status)
  IMPLICIT NONE
  INTEGER      , INTENT(IN)  :: DataHandle
  CHARACTER*(*), INTENT(OUT) :: DateStr
  INTEGER      , INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_get_previous_time


!--- open_for_read_begin
SUBROUTINE ext_xxx_open_for_read_begin( FileName, Comm, IOComm, SysDepInfo, DataHandle, Status)
  IMPLICIT NONE
  CHARACTER*(*), INTENT(IN)      :: FileName
  INTEGER      , INTENT(IN)      :: Comm
  INTEGER      , INTENT(IN)      :: IOComm
  CHARACTER*(*), INTENT(IN)      :: SysDepInfo
  INTEGER      , INTENT(OUT)     :: DataHandle
  INTEGER      , INTENT(OUT)     :: Status
RETURN
END SUBROUTINE ext_xxx_open_for_read_begin


!--- open_for_read_commit
SUBROUTINE ext_xxx_open_for_read_commit(DataHandle, Status)
  IMPLICIT NONE
  INTEGER, INTENT(IN)  :: DataHandle
  INTEGER, INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_open_for_read_commit


!--- open_for_update
SUBROUTINE ext_xxx_open_for_update( FileName, Comm, IOComm, SysDepInfo, DataHandle, Status)
  IMPLICIT NONE
  CHARACTER*(*), INTENT(IN)  :: FileName
  INTEGER      , INTENT(IN)  :: Comm
  INTEGER      , INTENT(IN)  :: IOComm
  CHARACTER*(*), INTENT(IN)  :: SysDepInfo
  INTEGER      , INTENT(OUT) :: DataHandle
  INTEGER      , INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_open_for_update


!--- open_for_write
SUBROUTINE ext_xxx_open_for_write (DatasetName, Comm1, Comm2, &
                                   SysDepInfo, DataHandle, Status)
  IMPLICIT NONE
  CHARACTER *(*), INTENT(IN)  :: DatasetName
  INTEGER       , INTENT(IN)  :: Comm1, Comm2
  CHARACTER *(*), INTENT(IN)  :: SysDepInfo
  INTEGER       , INTENT(OUT) :: DataHandle
  INTEGER       , INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_open_for_write


!--- warning_str
SUBROUTINE ext_xxx_warning_str( Code, ReturnString, Status)
  IMPLICIT NONE
  INTEGER       , INTENT(IN)  :: Code
  CHARACTER *(*), INTENT(OUT) :: ReturnString
  INTEGER       , INTENT(OUT) :: Status
RETURN
END SUBROUTINE ext_xxx_warning_str

