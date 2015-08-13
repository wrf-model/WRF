module grib2tbls_types

!*
!* This module will hold data from a grib2map.tbl table
!*   The variables will be accessible by all functions (provided they have a
!*   "USE grb2tbls" line).
!*

  integer, parameter                          :: MaxNames = 40
  integer, parameter                          :: maxLineSize = 500

  TYPE :: grib2Entries_type
     integer                                  :: Disc
     integer                                  :: Category
     integer                                  :: ParmNum
     character(len=maxLineSize)               :: WRFNameString
     character(len=30), dimension(MaxNames)   :: WRFNames
     integer                                  :: numWRFNames
     character(len=200)                       :: Description
     integer                                  :: DecScl
     integer                                  :: BinScl
     TYPE(grib2Entries_type), pointer         :: next
     TYPE(grib2Entries_type), pointer         :: previous
  END TYPE grib2Entries_type

  TYPE :: grib2tbls_type
     integer                                  :: center
     integer                                  :: subcenter
     integer                                  :: MasterTblV
     integer                                  :: LocalTblV
     integer                                  :: numEntries
     TYPE(grib2Entries_type),pointer          :: ParmHead
     TYPE(grib2Entries_type),pointer          :: ParmTail
     TYPE(grib2tbls_type),pointer             :: next
     TYPE(grib2tbls_type),pointer             :: previous
  END TYPE grib2tbls_type

  TYPE(grib2tbls_type), pointer               :: TblHead
  TYPE(grib2tbls_type), pointer               :: TblTail

end module grib2tbls_types

