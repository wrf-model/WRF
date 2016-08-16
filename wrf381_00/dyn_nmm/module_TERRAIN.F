module module_TERRAIN
  private
  public :: terrain_for, nmm_terrain

  type nmm_terrain
     integer :: nx,ny,level,input_type,io_form
     real, pointer, dimension(:,:) :: avc,lnd,lah,loh
     logical :: initialized
  end type nmm_terrain

  logical, save :: initialized=.false.
  integer, parameter :: minlevel=0,maxlevel=20

  type(nmm_terrain), target, save :: terrain(minlevel:maxlevel)
contains
  function terrain_for(level,input_type,io_form) result(tr)
    implicit none
    type(nmm_terrain), pointer :: tr
    character*256 :: message
    integer, intent(in) :: level,input_type,io_form
    integer i

    if(level<minlevel .or. level>maxlevel) then
3304   format("INVALID NESTING LEVEL ",I0,": only ",I0," through ",I0," are allowed.")
       write(message,3304) level,minlevel,maxlevel
       call wrf_error_fatal(message)
    endif

    if(.not. initialized) then
       call wrf_debug(3,'initialize...')
       do i=minlevel,maxlevel
          tr=>terrain(i)
          tr%nx=0 ; tr%ny=0
          tr%level=i
          tr%initialized=.false.
          nullify(tr%avc)
          nullify(tr%lnd)
          nullify(tr%lah)
          nullify(tr%loh)
       end do
       initialized=.true.
       call wrf_debug(3,'done with init.')
    endif

    call wrf_debug(3,'get terrain for this level')
    tr=>terrain(level)

    if(.not. tr%initialized) then
       call wrf_debug(1,'terrain_for: need to read terrain')
       call read_terrain(tr,input_type,io_form)
    endif

    call wrf_debug(3,'check input type and io form')
    if(input_type /= tr%input_type) then
3306   format("MISMATCH IN INPUT_TYPE AT LEVEL ",I0,": input_type=",I0," and ",I0," both requested.")
       write(message,3306) level,tr%input_type,input_type
       call wrf_error_fatal(message)
    endif

    if(io_form /= tr%io_form) then
3309   format("MISMATCH IN IO_FORM AT LEVEL ",I0,": io_form=",I0," and ",I0," both reqested.")
       write(message,3309) level,tr%io_form,io_form
       call wrf_error_fatal(message)
    endif
    call wrf_debug(1,'terrain_for: returning')
  end function terrain_for

  subroutine read_terrain(tr,input_type,io_form)
    USE module_domain
    USE module_configure
    USE module_timing
    USE wrfsi_static

    implicit none

    type(nmm_terrain), pointer :: tr
    integer, intent(in) :: io_form, input_type
    integer, parameter :: IO_BIN=1, IO_NET=2, IO_PNC=11
    CHARACTER(LEN=6)                  :: nestpath
    character(len=128)                :: input_fname
    integer :: comm_1,comm_2, handle,istatus
    integer :: level
    character (len=32)                :: cname
    integer                           :: ndim
    character (len=3)                 :: memorder
    character (len=32)                :: stagger
    integer, dimension(3)             :: domain_start, domain_end
    integer                           :: wrftype,n,i,j
    character (len=128), dimension(3) :: dimnames
    character*256                     :: message
    real, allocatable, dimension(:,:,:) :: real_domain
    character (len=10), parameter  :: name(24) = (/ "XLAT_M    ", &
                                                    "XLONG_M   ", &
                                                    "XLAT_V    ", &
                                                    "XLONG_V   ", &
                                                    "E         ", &
                                                    "F         ", &
                                                    "LANDMASK  ", &
                                                    "LANDUSEF  ", &
                                                    "LU_INDEX  ", &
                                                    "HCNVX     ", &
                                                    "HSTDV     ", &
                                                    "HASYW     ", &
                                                    "HASYS     ", &
                                                    "HASYSW    ", &
                                                    "HASYNW    ", &
                                                    "HLENW     ", &
                                                    "HLENS     ", &
                                                    "HLENSW    ", &
                                                    "HLENNW    ", &
                                                    "HANIS     ", &
                                                    "HSLOP     ", &
                                                    "HANGL     ", &
                                                    "HZMAX     ", & 
                                                    "HGT_M     " /)


    level=tr%level
    write(nestpath,"(a4,i1,a1)") 'nest',level,'/'

    input_types: if (input_type == 1) then
       !
       !        si version of the static file
       !
       CALL get_wrfsi_static_dims(nestpath,tr%nx,tr%ny)
       ALLOCATE (tr%avc(tr%nx,tr%ny))
       ALLOCATE (tr%lnd(tr%nx,tr%ny))
       ALLOCATE (tr%lah(tr%nx,tr%ny))
       ALLOCATE (tr%loh(tr%nx,tr%ny))
       CALL get_wrfsi_static_2d(nestpath, 'avc', tr%avc)
       CALL get_wrfsi_static_2d(nestpath, 'lnd', tr%lnd)
       CALL get_wrfsi_static_2d(nestpath, 'lah', tr%lah)
       CALL get_wrfsi_static_2d(nestpath, 'loh', tr%loh)

    else if (input_type == 2) then
       !
       !        WPS version of the static file
       !
       call wrf_debug(3,'wps static file')
#ifdef INTIO
       if (io_form == IO_BIN) write(input_fname,"(A,I2.2,A)") "geo_nmm_nest.l",level,".int"
#endif
#ifdef NETCDF
       if (io_form == IO_PNC) call wrf_message("WARNING: module_TERRAIN.F: pNetCDF specified, but using serial NetCDF to read geo_nmm_nest file")
       if (io_form == IO_NET .OR. io_form == IO_PNC) write(input_fname,"(A,I2.2,A)") "geo_nmm_nest.l",level,".nc"
#endif

       comm_1 = 1
       comm_2 = 1

#ifdef INTIO
       if (io_form == IO_BIN) &
            call ext_int_open_for_read(trim(input_fname), comm_1, comm_2, 'sysdep info', handle, istatus)
#endif
#ifdef NETCDF
       if (io_form == IO_NET .OR. io_form == IO_PNC ) &
            call ext_ncd_open_for_read(trim(input_fname), comm_1, comm_2, 'sysdep info', handle, istatus)
#endif
!idealized KWON       if (istatus /= 0) CALL wrf_error_fatal('NEST_TERRAIN error after ext_XXX_open_for_read '//trim(input_fname))


       read_loop: do n=1,24

          cname = name(n)

          domain_start = 1
          domain_end = 1
#ifdef INTIO
          if (io_form == IO_BIN) &
               call ext_int_get_var_info(handle, cname, ndim, memorder, stagger, domain_start, domain_end, wrftype, istatus)
#endif
#ifdef NETCDF
          if (io_form == IO_NET .OR. io_form == IO_PNC ) &
               call ext_ncd_get_var_info(handle, cname, ndim, memorder, stagger, domain_start, domain_end, wrftype, istatus)
#endif

          if (allocated(real_domain)) deallocate(real_domain)
          allocate(real_domain(domain_start(1):domain_end(1), domain_start(2):domain_end(2), domain_start(3):domain_end(3)))

#ifdef INTIO
          if (io_form == IO_BIN) then
             call ext_int_read_field(handle, '0000-00-00_00:00:00', cname, real_domain, wrftype, &
                  1, 1, 0, memorder, stagger, &
                  dimnames, domain_start, domain_end, domain_start, domain_end, &
                  domain_start, domain_end, istatus)
          end if
#endif
#ifdef NETCDF
          if (io_form == IO_NET .OR. io_form == IO_PNC ) then
             call ext_ncd_read_field(handle, '0000-00-00_00:00:00', cname, real_domain, wrftype, &
                  1, 1, 0, memorder, stagger, &
                  dimnames, domain_start, domain_end, domain_start, domain_end, &
                  domain_start, domain_end, istatus)
          end if
#endif

          write(message,'("domain nx=",I0," ny=",I0)') domain_end(1),domain_end(2)
          tr%nx = domain_end(1)
          tr%ny = domain_end(2)
          write(message,'("nx=",I0," ny=",I0)') tr%nx,tr%ny
          if (cname(1:10) == "XLAT_M    ") then
             call wrf_debug(10,'tr%lah...')
             ALLOCATE (tr%lah(tr%nx,tr%ny))
             call wrf_debug(10,'allocated...')
             do j=1,tr%ny
                do i=1,tr%nx
                   tr%lah(i,j) = real_domain(i,j,1)
                end do
             end do
             call wrf_debug(10,'tr%lah.')
          else if (cname(1:10) == "XLONG_M   ") then
             call wrf_debug(10,'tr%loh...')
             ALLOCATE (tr%loh(tr%nx,tr%ny))
             call wrf_debug(10,'allocated...')
             do j=1,tr%ny
                do i=1,tr%nx
                   tr%loh(i,j) = real_domain(i,j,1)
                end do
             end do
             call wrf_debug(10,'tr%loh.')
          else if (cname(1:10) == "LANDMASK  ") then
             call wrf_debug(10,'tr%lnd...')
             ALLOCATE (tr%lnd(tr%nx,tr%ny))
             call wrf_debug(10,'allocated...')
             do j=1,tr%ny
                do i=1,tr%nx
                   tr%lnd(i,j) = real_domain(i,j,1)
                end do
             end do
             call wrf_debug(10,'tr%lnd')
          else if (cname(1:10) == "HGT_M     ") then
             call wrf_debug(10,'tr%avc...')
             ALLOCATE (tr%avc(tr%nx,tr%ny))
             call wrf_debug(10,'allocated...')
             do j=1,tr%ny
                do i=1,tr%nx
                   tr%avc(i,j) = real_domain(i,j,1)
                end do
             end do
             call wrf_debug(10,'tr%avc.')
          end if

       end do read_loop
       call wrf_debug(10,"past read loop")
       if(allocated(real_domain))  deallocate(real_domain)
       call wrf_debug(10,'past deallocate')
#ifdef INTIO
       if (io_form == IO_BIN) then
          call ext_int_ioclose(handle, istatus)
       end if
#endif
#ifdef NETCDF
       if (io_form == IO_NET .OR. io_form == IO_PNC ) then
          call ext_ncd_ioclose(handle, istatus)
       end if
#endif
       call wrf_debug(10,"past close")
       if(.not. associated(tr%lah))    call readfail(tr,input_fname,'lah')
       if(.not. associated(tr%loh))    call readfail(tr,input_fname,'loh')
       if(.not. associated(tr%lnd))    call readfail(tr,input_fname,'lnd')
       if(.not. associated(tr%avc))    call readfail(tr,input_fname,'avc')
       
    else
       CALL wrf_error_fatal('NEST_TERRAIN wrong input_type')
    end if input_types
    

    tr%input_type=input_type
    tr%io_form=io_form
    tr%initialized=.true.
    call wrf_debug(10,"done in read_terrain")
  end subroutine read_terrain

  subroutine readfail(tr,input_fname,what)
    implicit none
    type(nmm_terrain), pointer :: tr
    character*256 :: message
    character*3 :: what
    character(len=128) :: input_fname
    
3123 format('Did not find "',A,'" in file "',A,'".')
    write(message,3123) trim(what),trim(input_fname)
    call wrf_error_fatal(message)
  end subroutine readfail
end module module_TERRAIN
