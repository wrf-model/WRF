PROGRAM test_read_grib2map

  USE grib2tbls_types
  Implicit None

  INTERFACE
     Subroutine load_grib2map (filename, grib2tbls)
       USE grib2tbls_types
       character*(*), intent(in)                   :: filename
       TYPE(grib2tbls_type), dimension(:), pointer :: grib2tbls
     END subroutine load_grib2map
  END INTERFACE

  TYPE(grib2tbls_type), dimension(:), pointer :: grib2tbls
  character(200)                              :: filename = 'grib2map.tbl'

  character*30              :: parmname
  integer                   :: center, subcenter, MasterTblV, LocalTblV, &
       Disc, Category, ParmNum, DecScl, BinScl
  integer                   :: ierr


  CALL load_grib2map(filename,grib2tbls)

  parmname = 'T2'
  CALL get_parminfo(parmname, center, subcenter, MasterTblV, &
     LocalTblV, Disc, Category, ParmNum, DecScl, BinScl, ierr)

  if (ierr .ne. 0) then
     print *,'Could not find match for parameter: ',parmname
  else
     print *,'Found match: '
     print *, '  center:    ',center
     print *, '  subcenter: ',subcenter
     print *, '  MasterTblV ',MasterTblV
     print *, '  LocalTblV  ',LocalTblV
     print *, '  Disc       ',Disc
     print *, '  Category   ',Category
     print *, '  ParmNum    ',ParmNum
     print *, '  DecScl     ',DecScl
     print *, '  BinScl     ',BinScl
  endif
  
  CALL free_grib2map(grib2tbls)

END PROGRAM
