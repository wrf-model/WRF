program da_vp_bilin

!----------------------------------------------------------------------
! Purpose: Regridding from low to high resolution in control variable space
!                       by using bilinear interpolation
!
! where n  is the grid number in x or y
!       ns is the refinement ratio between two resulotions
!
! Method: follow da_bilin.f90
!
! Compile:
!
!   pgf90 -o da_vp_bilin.exe da_vp_bilin.f90
!
! liuz@ucar.edu , 2016-08, NCAR/MMM
!----------------------------------------------------------------------

  !use netcdf

  implicit none

  !These variables' incremental will be regridded by default
  character (len=6), dimension(1:19) :: vNam 

  integer :: ix, jy, kz, k, status
  integer :: ixh, jyh, kzh
  integer :: nLat, nLon, oLat, oLon
  integer :: sLat, eLat, sLon, eLon
  integer :: rLat, rLon

  real, dimension(:,:,:), allocatable :: v1, v2, v3, v4, v5
  real, dimension(:,:,:), allocatable :: v6, v7, v8, v9, v10, v11
  real, dimension(:,:,:), allocatable :: v1h, v2h, v3h, v4h, v5h
  real, dimension(:,:,:), allocatable :: v6h, v7h, v8h, v9h, v10h, v11h
  real, dimension(:,:),   allocatable :: iVar, oVar

  character (len = 255)  :: appname   = ""
  character (len = 255)  :: arg       = ""
  character (len = 19)  :: analysis_date
  character (len = 255)  :: input_file= "vp_output.global"
  character (len = 255)  :: output_file= "vp_output.global_hires"

  integer, parameter :: vp_unit = 8
  integer, parameter :: vp_hires_unit = 9
  integer :: ratio ! resolution ratio
  integer :: cloud_cv_options ! 2 or 3 with cloud cv variables
  integer :: use_cv_w         ! =1 for w control variable
  integer :: io_status
  integer iargc

  LOGICAL :: file_exists

  !These variables' incremental will be regridded by default

  !call getarg(0, appname)
  !n=index(appname, '/', BACK=.true.)
  !appname = trim(appname(n+1:))

  call getarg(1, arg)
  call getarg(2, arg)
  read(arg, '(i3)') ratio

  call getarg(3, arg)
  call getarg(4, arg)
  read(arg, '(i3)') cloud_cv_options

  call getarg(5, arg)
  call getarg(6, arg)
  read(arg, '(i3)') use_cv_w


  write (*, *) 'ratio = ', ratio, 'cloud_cv_options = ', cloud_cv_options, &
               'use_cv_w = ', use_cv_w


!  read vp file
!--------------------
  inquire(FILE=trim(input_file), EXIST=file_exists)

  if ( .not. file_exists ) then
    Write(*,*) "\nError: "//trim(input_file)//" not exists\n"
    call exit(-1)
  else
    Write(*,*) "Found: "//trim(input_file)
  endif
  
  open(unit=vp_unit,file=trim(input_file),iostat=io_status,form='UNFORMATTED',status='OLD')
  if (io_status /= 0) then
     write(*,*) "Error ",io_status," opening vp file "//trim(input_file)
     call exit(-1)
  end if
  write(*,*) 'Reading vp from : '//trim(input_file)
  !read(vp_unit) analysis_date
  !print *, 'analysis_date = ', analysis_date
  read(vp_unit) ix, jy, kz ! domain dimension (unstagered)
  print *, "input file: ix, jy, kz = ", ix, jy, kz
  
  allocate ( v1 (1:ix,1:jy,1:kz))
  allocate ( v2 (1:ix,1:jy,1:kz))
  allocate ( v3 (1:ix,1:jy,1:kz))
  allocate ( v4 (1:ix,1:jy,1:kz))
  allocate ( v5 (1:ix,1:jy,1:kz))

  read(vp_unit) v1, v2, v3, v4, v5

  if ( cloud_cv_options >= 2 ) then
    allocate ( v6 (1:ix,1:jy,1:kz))
    allocate ( v7 (1:ix,1:jy,1:kz))
    allocate ( v8 (1:ix,1:jy,1:kz))
    allocate ( v9 (1:ix,1:jy,1:kz))
    allocate ( v10 (1:ix,1:jy,1:kz))
    read(vp_unit) v6, v7, v8, v9, v10
  end if 

  if ( use_cv_w == 1 ) then
    allocate ( v11 (1:ix,1:jy,1:kz))
    read(vp_unit) v11
  end if

  write(*,*) 'End Reading vp from : '//trim(input_file)
  close(vp_unit)
!-----------------------------
! end read vp file
!----------------------

    nLon = ix + 2 ! 52
    nLat = jy + 2 ! 52

    rLon = ix * ratio ! 150
    rLat = jy * ratio ! 150

    oLon = ( nLon - 1 ) * ratio + 1 ! 154
    oLat = ( nLat - 1 ) * ratio + 1

    elat = (oLat - rLat) / 2       ! 2
    slat =  oLat - rLat - elat + 1 ! 3

    elon = (oLon - rLon) / 2
    slon =  oLon - rLon - elon + 1

    allocate(iVar(nLon, nLat), stat=status)
    allocate(oVar(oLon, oLat), stat=status)


  ixh = ix*ratio
  jyh = jy*ratio

  allocate ( v1h (1:ixh,1:jyh,1:kz))
  allocate ( v2h (1:ixh,1:jyh,1:kz))
  allocate ( v3h (1:ixh,1:jyh,1:kz))
  allocate ( v4h (1:ixh,1:jyh,1:kz))
  allocate ( v5h (1:ixh,1:jyh,1:kz))

  if ( cloud_cv_options >= 2 ) then
    allocate ( v6h (1:ixh,1:jyh,1:kz))
    allocate ( v7h (1:ixh,1:jyh,1:kz))
    allocate ( v8h (1:ixh,1:jyh,1:kz))
    allocate ( v9h (1:ixh,1:jyh,1:kz))
    allocate ( v10h (1:ixh,1:jyh,1:kz))
  end if

  if ( use_cv_w == 1 ) then
    allocate ( v11h (1:ixh,1:jyh,1:kz))
  end if

  do k = 1, kz
     iVar(2:nlon-1,2:nlat-1) = v1(:,:,k)
     iVar(1,:)               = iVar(2,:)
     iVar(nlon,:)            = iVar(nlon-1,:)
     iVar(:,1)               = iVar(:,2)
     iVar(:,nlat)            = iVar(:,nlat-1)
     call bilin(iVar,nLon,nLat,ratio,oVar,oLon,oLat)
     v1h(:,:,k) = oVar(slon:olon-elon,slat:olat-elat)

     iVar(2:nlon-1,2:nlat-1) = v2(:,:,k)
     iVar(1,:)               = iVar(2,:)
     iVar(nlon,:)            = iVar(nlon-1,:)
     iVar(:,1)               = iVar(:,2)
     iVar(:,nlat)            = iVar(:,nlat-1)
     call bilin(iVar,nLon,nLat,ratio,oVar,oLon,oLat)
     v2h(:,:,k) = oVar(slon:olon-elon,slat:olat-elat)

     iVar(2:nlon-1,2:nlat-1) = v3(:,:,k)
     iVar(1,:)               = iVar(2,:)
     iVar(nlon,:)            = iVar(nlon-1,:)
     iVar(:,1)               = iVar(:,2)
     iVar(:,nlat)            = iVar(:,nlat-1)
     call bilin(iVar,nLon,nLat,ratio,oVar,oLon,oLat)
     v3h(:,:,k) = oVar(slon:olon-elon,slat:olat-elat)

     iVar(2:nlon-1,2:nlat-1) = v4(:,:,k)
     iVar(1,:)               = iVar(2,:)
     iVar(nlon,:)            = iVar(nlon-1,:)
     iVar(:,1)               = iVar(:,2)
     iVar(:,nlat)            = iVar(:,nlat-1)
     call bilin(iVar,nLon,nLat,ratio,oVar,oLon,oLat)
     v4h(:,:,k) = oVar(slon:olon-elon,slat:olat-elat)

     iVar(2:nlon-1,2:nlat-1) = v5(:,:,k)
     iVar(1,:)               = iVar(2,:)
     iVar(nlon,:)            = iVar(nlon-1,:)
     iVar(:,1)               = iVar(:,2)
     iVar(:,nlat)            = iVar(:,nlat-1)
     call bilin(iVar,nLon,nLat,ratio,oVar,oLon,oLat)
     v5h(:,:,k) = oVar(slon:olon-elon,slat:olat-elat)

   if ( cloud_cv_options >= 2 ) then
     iVar(2:nlon-1,2:nlat-1) = v6(:,:,k)
     iVar(1,:)               = iVar(2,:)
     iVar(nlon,:)            = iVar(nlon-1,:)
     iVar(:,1)               = iVar(:,2)
     iVar(:,nlat)            = iVar(:,nlat-1)
     call bilin(iVar,nLon,nLat,ratio,oVar,oLon,oLat)
     v6h(:,:,k) = oVar(slon:olon-elon,slat:olat-elat)

     iVar(2:nlon-1,2:nlat-1) = v7(:,:,k)
     iVar(1,:)               = iVar(2,:)
     iVar(nlon,:)            = iVar(nlon-1,:)
     iVar(:,1)               = iVar(:,2)
     iVar(:,nlat)            = iVar(:,nlat-1)
     call bilin(iVar,nLon,nLat,ratio,oVar,oLon,oLat)
     v7h(:,:,k) = oVar(slon:olon-elon,slat:olat-elat)

     iVar(2:nlon-1,2:nlat-1) = v8(:,:,k)
     iVar(1,:)               = iVar(2,:)
     iVar(nlon,:)            = iVar(nlon-1,:)
     iVar(:,1)               = iVar(:,2)
     iVar(:,nlat)            = iVar(:,nlat-1)
     call bilin(iVar,nLon,nLat,ratio,oVar,oLon,oLat)
     v8h(:,:,k) = oVar(slon:olon-elon,slat:olat-elat)

     iVar(2:nlon-1,2:nlat-1) = v9(:,:,k)
     iVar(1,:)               = iVar(2,:)
     iVar(nlon,:)            = iVar(nlon-1,:)
     iVar(:,1)               = iVar(:,2)
     iVar(:,nlat)            = iVar(:,nlat-1)
     call bilin(iVar,nLon,nLat,ratio,oVar,oLon,oLat)
     v9h(:,:,k) = oVar(slon:olon-elon,slat:olat-elat)

     iVar(2:nlon-1,2:nlat-1) = v10(:,:,k)
     iVar(1,:)               = iVar(2,:)
     iVar(nlon,:)            = iVar(nlon-1,:)
     iVar(:,1)               = iVar(:,2)
     iVar(:,nlat)            = iVar(:,nlat-1)
     call bilin(iVar,nLon,nLat,ratio,oVar,oLon,oLat)
     v10h(:,:,k) = oVar(slon:olon-elon,slat:olat-elat)
   end if

   if ( use_cv_w == 1 ) then
     iVar(2:nlon-1,2:nlat-1) = v11(:,:,k)
     iVar(1,:)               = iVar(2,:)
     iVar(nlon,:)            = iVar(nlon-1,:)
     iVar(:,1)               = iVar(:,2)
     iVar(:,nlat)            = iVar(:,nlat-1)
     call bilin(iVar,nLon,nLat,ratio,oVar,oLon,oLat)
     v11h(:,:,k) = oVar(slon:olon-elon,slat:olat-elat)
   end if
  enddo

     open(unit=vp_hires_unit,file=trim(output_file),iostat=io_status,form='UNFORMATTED',status='UNKNOWN')
     if (io_status /= 0) then
        write(*,*) "Error ",io_status," opening vp file "//trim(output_file)
        call exit(-1)
     end if
     write(*,*) 'Writting vp on hires. to : '//trim(output_file)

    print *, 'output file: ixh, jyh, kz=', ixh, jyh, kz
    write(vp_hires_unit) ixh, jyh, kz
    write(vp_hires_unit) v1h,v2h,v3h,v4h,v5h
    if ( cloud_cv_options >= 2 ) then
      write(vp_hires_unit) v6h,v7h,v8h,v9h,v10h
    end if
    if ( use_cv_w == 1 ) then
      write(vp_hires_unit) v11h
    end if

    deallocate(v1, stat=status)
    deallocate(v2, stat=status)
    deallocate(v3, stat=status)
    deallocate(v4, stat=status)
    deallocate(v5, stat=status)

    deallocate(v1h, stat=status)
    deallocate(v2h, stat=status)
    deallocate(v3h, stat=status)
    deallocate(v4h, stat=status)
    deallocate(v5h, stat=status)

  if ( cloud_cv_options >= 2 ) then
    deallocate(v6, stat=status)
    deallocate(v7, stat=status)
    deallocate(v8, stat=status)
    deallocate(v9, stat=status)
    deallocate(v10, stat=status)

    deallocate(v6h, stat=status)
    deallocate(v7h, stat=status)
    deallocate(v8h, stat=status)
    deallocate(v9h, stat=status)
    deallocate(v10h, stat=status)
  end if

  if ( use_cv_w == 1 ) then
    deallocate(v11, stat=status)
    deallocate(v11h, stat=status)
  end if

  Write(*,*) "Regridding increment completed successfully"

contains
  subroutine show_usage()
     Write(*,*) 'Usage :'//trim(appname)// &
     '[-h] [-fg_lores filename] [-an_lores filename] [-fg_hires filename] [-ns n ] [-o outputfile]'
     Write(*,*) "  -fg_lores    Optional, low resulotion first guess file,                 default - fg"
     Write(*,*) "  -an_lores    Optional, low resulotion analysis file comes from wrfvar,  default - wrfvar_output"
     Write(*,*) "  -fg_hires    Optional, high resultion first guess file,                 default - wrfinput_hires"
     Write(*,*) "  -ns          Optional, the refinement ratio between two resulotions,    default - 3"
     Write(*,*) "  -o           Optional, output high resulotion analysis file,            default - wrfvar_output_hires"
     Write(*,*) "  -h           Show this help"
  end subroutine show_usage

  !subroutine nf90_handle_err(status, errmsg)
  !  integer,          intent(in) :: status
  !  character(len=*), intent(in) :: errmsg
!
!    if(status /= nf90_noerr) then
!      print *, trim(nf90_strerror(status))//" : "//trim(errmsg)
!      Stop
!    end if
!  end subroutine nf90_handle_err

  subroutine bilin(old,xi,yi,ns,new,xo,yo)

! assume: xo = (xi-1)*ns + 1, xi=50, xo=49*3+1=148
!         yo = (yi-1)*ns + 1

    implicit none

    integer,                 intent(in) :: xi,yi,xo,yo
    real, dimension(xi,yi),  intent(in) :: old
    integer,                 intent(in) :: ns
    real, dimension(xo,yo),  intent(out):: new

    real  :: im(1:ns+1,2)
!    real  :: imm(1:ns+3,2)
    integer:: i,j,jm1,im1,ix1,ix2,iy1,iy2

    forall(i=1:ns+1) im(i,2) = real(i-1)/ns
      im(:,1) = 1 - im(:,2)

    do j=2,yi
      jm1 = j - 1
      iy2 = jm1 * ns + 1
      iy1 = iy2 - ns
      do i=2,xi
        im1 = i - 1
        ix2 = im1 * ns + 1
        ix1 = ix2 - ns
        new(ix1:ix2,iy1:iy2) = matmul(im,matmul(old(im1:i,jm1:j),transpose(im)))
      end do
    end do


   !   ns = ns + 2
   ! forall(i=1:ns+1) imm(i,2) = real(i-1)/ns
   !   imm(:,1) = 1 - imm(:,2)
!
!      j=yi
!      jm1 = j - 1
!      iy2 = jm1 * ns + 1
!      iy1 = iy2 - ns
!
!      i=xi
!        im1 = i - 1
!        ix2 = im1 * ns + 1
!        ix1 = ix2 - ns
!        new(ix1:ix2,iy1:iy2) = matmul(imm,matmul(old(im1:i,jm1:j),transpose(imm)))
!      end do
!    end do

  end subroutine bilin

end program da_vp_bilin
