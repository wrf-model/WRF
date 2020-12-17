module module_stntbl
   implicit none
   character(len=5),  allocatable, dimension(:) :: id_tbl
   real, allocatable, dimension(:) :: rlat_tbl
   real, allocatable, dimension(:) :: rlon_tbl
   real, allocatable, dimension(:) :: elev_tbl
   logical :: use_msfc_tbl
   integer :: num_stations_msfc

contains

   subroutine read_msfc_table(table_name)
      implicit none
      character(len=*), intent(in) :: table_name
      character(len=80) :: cbuf
      integer :: itmp
      logical :: isfile
      integer :: iunit
      integer :: istatus
      integer :: icount

      use_msfc_tbl = .false.
      iunit = 33
      icount = 0
      istatus = 0
      !table_name = 'msfc.tbl'
      inquire(file=trim(table_name), exist=isfile)
      if ( .not. isfile ) then
         write(0,*) 'file ', trim(table_name), ' not found.'
         return
      end if
      open(unit=iunit, file=trim(table_name), form='formatted', status='old')
      ! first find the number of stations
      do while ( istatus == 0 )
         read(iunit,'(a80)', iostat=istatus) cbuf
         if ( istatus == 0 ) then
            if ( cbuf(1:1) /= '!' ) then
               icount = icount + 1
            end if
         end if
      end do 
      num_stations_msfc = icount
      ! read the file again
      rewind(iunit)
      icount = 0
      istatus = 0
      if ( num_stations_msfc > 0 ) then
         use_msfc_tbl = .true.
         allocate(id_tbl(num_stations_msfc))
         allocate(rlat_tbl(num_stations_msfc))
         allocate(rlon_tbl(num_stations_msfc))
         allocate(elev_tbl(num_stations_msfc))
         do while ( istatus == 0 )
            read(iunit,'(a80)', iostat=istatus) cbuf
            if ( istatus == 0 ) then
               if ( cbuf(1:1) /= '!' ) then
                  icount = icount + 1
                  id_tbl(icount) = cbuf(1:5)
                  read(cbuf(56:60), '(i5)') itmp
                  rlat_tbl(icount) = itmp*0.01
                  read(cbuf(62:67), '(i6)') itmp
                  rlon_tbl(icount) = itmp*0.01
                  read(cbuf(69:73), '(i6)') itmp
                  elev_tbl(icount) = itmp*1.0
               end if
            end if
         end do 
      end if
      close(iunit)
   end subroutine read_msfc_table
end module module_stntbl
