program add_increments_for_wrf_nmm 

   !----------------------------------------------------------------------
   ! Author: Sujata Pattanayak, CAS, IIT Delhi, India
   ! Date:   20th May 2008.
   !
   ! Purpose: read  analysis increments written by WRF-VAr for WRF-NMM model
   !  How to compile?
   !  On IBM:
   !   xlf90 -qrealsize=8 -o add_increments_for_wrf_nmm.exe add_increments_for_wrf_nmm.f -I/usr/local/netcdf/include -L/usr/local/netcdf/lib -lnetcdf !don't use   
   !   xlf90 -o add_increments_for_wrf_nmm.exe add_increments_for_wrf_nmm.f -I/usr/local/netcdf/include -L/usr/local/netcdf/lib -lnetcdf   
   !----------------------------------------------------------------------

   implicit none
   include 'netcdf.inc'

   integer            :: i, j, k, ix, jy, kz
   integer            :: nx, ny, nz, nnx, nny, nnz
   integer, parameter :: anl_inc_unit=10
   character (len=100):: wrfinput_d01_nmm
   character(len=512) :: input_file
   logical            :: debug
   integer            :: ndims
   integer, dimension (4) :: dims

   character*19   :: analysis_date 
   real*8, allocatable  :: u(:,:,:)    
   real*8, allocatable  :: v(:,:,:)    
   real*8, allocatable  :: t(:,:,:)    
   real*8, allocatable  :: q(:,:,:)    
   real*8, allocatable  :: pd(:,:)   
   real*8, allocatable  :: xlat(:,:)   
   real*8, allocatable  :: xlon(:,:)   
   real, allocatable, dimension(:,:,:) :: u_e, v_e, t_e, q_e
   real, allocatable, dimension(:,:)   :: pd_e
   input_file = 'wrfinput_d01'

      open(unit=anl_inc_unit, file= &
 'analysis_increments_for_wrf-nmm', form='unformatted', status='old')

     read (unit=anl_inc_unit,err=100,end=99) analysis_date 
     print*,' read analysis_date ', analysis_date 

      read (unit=anl_inc_unit,err=100,end=99) ix,jy, kz 

      print*,' ix, jy kz ',ix,jy, kz 

      allocate (u (1:ix, 1:jy, 1:kz))
      allocate (v (1:ix, 1:jy, 1:kz))
      allocate (t (1:ix, 1:jy, 1:kz))
      allocate (q (1:ix, 1:jy, 1:kz))
      allocate (pd(1:ix,1:jy))
      allocate (xlat(1:ix,1:jy))
      allocate (xlon(1:ix,1:jy))

      read (unit=anl_inc_unit, err=100, end=99)   u 
!  do k = 1, kz
!     do j = 1, jy
!        do i = 1, ix
!        print*, 'U increment values are', u(i,j,k)
!   print 111,' U at lon ',i,' = ',u(i, 1, 1)
! 111    format(1x,a,i4,a,f12.6)
!        end do
!     end do
!   end do

      read (unit=anl_inc_unit, err=100, end=99)   v 
      read (unit=anl_inc_unit, err=100, end=99)   t
!  do i = 1, ix
!   print 112,' T at lon ',i,' = ',t(i, 14, 10)
! 112    format(1x,a,i4,a,f12.6)
!   end do
      read (unit=anl_inc_unit, err=100, end=99)   q
      read (unit=anl_inc_unit, err=100, end=99)   pd
!      read (unit=anl_inc_unit, err=100, end=99) xlat
!  do i = 1, ix
!   print 112,' XLAT at lon ',i,' = ',xlat(i, 1)
! 112    format(1x,a,i4,a,f12.6)
!   end do
!      read (unit=anl_inc_unit, err=100, end=99) xlon

99    continue
      close(anl_inc_unit)

!--------------------------------------------------------------
!-------Reading and writing netcdf data----------------
!--------------------------------------------------------------
!-------Three-dimensional Variables--------------------
!--------------------------------------------------------------
      call get_dims_cdf(input_file,'U', dims, ndims, debug)
      print*,'U ndims ',ndims
      print*,'U dims ',dims
        allocate(u_e(dims(1), dims(2), dims(3)))
      call get_var_3d_real_cdf(input_file, 'U',u_e, &
                             dims(1), dims(2), dims(3),1, debug )
       print*,' u_e dimensions are', dims(1), dims(2), dims(3)
      call get_dims_cdf(input_file,'V', dims, ndims, debug)
      print*,'V ndims ',ndims
      print*,'V dims ',dims
        allocate(v_e(dims(1), dims(2), dims(3)))
      call get_var_3d_real_cdf(input_file, 'V',v_e, &
                             dims(1), dims(2), dims(3),1, debug )
       print*,' v_e dimensions are', dims(1), dims(2), dims(3)
      call get_dims_cdf(input_file,'T', dims, ndims, debug)
      print*,'T ndims ',ndims
      print*,'T dims ',dims
        allocate(t_e(dims(1), dims(2), dims(3)))
      call get_var_3d_real_cdf(input_file, 'T',t_e, &
                             dims(1), dims(2), dims(3),1, debug )
       print*,' t_e dimensions are', dims(1), dims(2), dims(3)
      call get_dims_cdf(input_file,'Q', dims, ndims, debug)
      print*,'Q ndims ',ndims
      print*,'Q dims ',dims
        allocate(q_e(dims(1), dims(2), dims(3)))
      call get_var_3d_real_cdf(input_file, 'Q',q_e, &
                             dims(1), dims(2), dims(3),1, debug )
       print*,' q_e dimensions are', dims(1), dims(2), dims(3)
        nx = dims(1)
        ny = dims(2)
        nz = dims(3)
      print*,'nx, ny, nz values are', nx, ny, nz
!Filling at the even points for wind 
       do k = 1, nz
          do j = 1, ny, 2
             do i = 1, nx-1
                u_e(i,j,k)=u_e(i,j,k)+u(2*i,j,k)
                v_e(i,j,k)=v_e(i,j,k)+v(2*i,j,k)
!             print*,'inc_u value is',u(2*i,j,k)
!             print*,'new_u value is', u_e(i,j,k)
!             print*,'u_e values at even points are',u_e(i,j,k)
             end do
          end do
       end do
!Filling at the odd points for wind 
       do k = 1, nz
          do j = 2, ny-1, 2
             do i = 1, nx
                u_e(i,j,k)=u_e(i,j,k)+u(2*i-1,j,k)
                v_e(i,j,k)=v_e(i,j,k)+v(2*i-1,j,k)
!             print*,'u_e values at odd points are',u_e(i,j,k)
             end do
          end do
       end do
!Filling at the even points for mass 
       do k = 1, nz
          do j = 2, ny, 2
             do i = 1, nx
                t_e(i,j,k)=t_e(i,j,k)+t(2*i,j,k)
                q_e(i,j,k)=q_e(i,j,k)+q(2*i,j,k)
!             print*,'t_e values at even points are',t_e(i,j,k)
             end do
          end do
       end do
!Filling at the odd points for mass 
       do k = 1, nz
          do j = 1, ny, 2
             do i = 1, nx
                t_e(i,j,k)=t_e(i,j,k)+t(2*i-1,j,k)
                q_e(i,j,k)=q_e(i,j,k)+q(2*i-1,j,k)
!             print*,'t_e values at odd points are',t_e(i,j,k)
             end do
          end do
       end do
      call put_var_3d_real_cdf(input_file, 'U', u_e, &
                             nx, ny, nz, 1, debug )
      call put_var_3d_real_cdf(input_file, 'V', v_e, &
                             nx, ny, nz, 1, debug )
      call put_var_3d_real_cdf(input_file, 'T', t_e, &
                             nx, ny, nz, 1, debug )
      call put_var_3d_real_cdf(input_file, 'Q', q_e, &
                             nx, ny, nz, 1, debug )
!--------------------------------------------------------------
!---------Two-dimensional Variables--------------------
!--------------------------------------------------------------
      call get_dims_cdf(input_file,'PD', dims, ndims, debug)
      print*,'PD ndims ',ndims
      print*,'PD dims ',dims
        allocate(pd_e(dims(1), dims(2)))
      call get_var_2d_real_cdf(input_file, 'PD',pd_e, &
                             dims(1), dims(2),1, debug )
       print*,' pd_e dimensions are', dims(1), dims(2), dims(3)
        nx = dims(1)
        ny = dims(2)
!Filling at the even points for mass 
          do j = 2, ny, 2
             do i = 1, nx
                pd_e(i,j)=pd_e(i,j)+pd(2*i,j)
!             print*,'pd_e values at even points are',pd_e(i,j)
             end do
          end do
!Filling at the odd points for mass 
          do j = 1, ny, 2
             do i = 1, nx
                pd_e(i,j)=pd_e(i,j)+pd(2*i-1,j)
!             print*,'pd_e values at odd points are',pd_e(i,j)
             end do
          end do
      call put_var_2d_real_cdf(input_file, 'PD', pd_e, &
                             nx, ny, 1, debug )

!      deallocate(u)
!      deallocate(v)
!      deallocate(t)
!      deallocate(q)
!      deallocate(pd)
      stop
100   print*,' read error on unit ', anl_inc_unit
end program add_increments_for_wrf_nmm 
!------------------------------------------------------------
  subroutine get_dims_cdf( file, var, dims, ndims, debug )

  implicit none

  include 'netcdf.inc'

  character (len=80), intent(in) :: file
  character (len=*), intent(in) :: var
  logical, intent(in ) :: debug
  integer, intent(out), dimension(4) :: dims
  integer, intent(out) :: ndims

  integer cdfid, rcode, id_time
  character (len=80) :: varnam, time1
  integer :: natts, istart(10),iend(10), dimids(10)
  integer :: i, ivtype

  cdfid = ncopn(file, NCNOWRIT, rcode )

  if( rcode == 0) then
    if(debug) write(6,*) ' open netcdf file ', trim(file)
  else
    write(6,*) ' error openiing netcdf file ', trim(file)
    stop
  end if

  id_time = ncvid( cdfid, var, rcode )

  rcode = nf_inq_var( cdfid, id_time, varnam, ivtype, ndims, dimids, natts )
  if(debug) then
    write(6,*) ' number of dims for ',var,' ',ndims
  endif
  do i=1,ndims
    rcode = nf_inq_dimlen( cdfid, dimids(i), dims(i) )
    if(debug) write(6,*) ' dimension ',i,dims(i)
  enddo

  call ncclos(cdfid,rcode)

  end subroutine get_dims_cdf
!-------------------------------------------------------
  subroutine get_var_2d_real_cdf( file, var, data, &
                                  i1, i2, time, debug )

  implicit none

  include 'netcdf.inc'

  integer, intent(in)  ::  i1, i2, time
  character (len=80), intent(in) :: file
  logical, intent(in ) :: debug
  character (len=*), intent(in) :: var
  real, dimension(i1,i2), intent(out) :: data
  real(kind=8), dimension(i1,i2) :: tmp

  integer cdfid, rcode, id_data
  character (len=80) :: varnam, time1
  integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
  integer :: i, ivtype

  cdfid = ncopn(file, NCNOWRIT, rcode )

  if( rcode /= 0) then
    write(unit=*, fmt='(2a)') ' error openiing netcdf file ', trim(file)
    stop
  end if

  id_data = ncvid( cdfid, var, rcode )

  rcode = nf_inq_var( cdfid, id_data, varnam, ivtype, ndims, dimids, natts )

  if(debug) then
    write(unit=*, fmt='(3a,i6)') ' get_var_2d_real_cdf: dims for ',var,' ',ndims
  endif

  do i=1,ndims
    rcode = nf_inq_dimlen( cdfid, dimids(i), idims(i) )
    if(debug) then
      write(unit=*, fmt='(a,2i6)') ' dimension ',i,idims(i)
      write(unit=*, fmt='(a,i6)') ' ivtype=', ivtype
      write(unit=*, fmt='(a, a)') ' varnam=', trim(varnam)
    endif
  enddo

!  check the dimensions

   if( (i1 /= idims(1)) .or.  &
       (i2 /= idims(2)) .or.  &
       (time > idims(3))     )  then

     write(6,*) ' error in 2d_var_real read, dimension problem '
     write(6,*) i1, idims(1)
     write(6,*) i2, idims(2)
     write(6,*) time, idims(4)
     write(6,*) ' error stop '
     stop
   end if

!  get the data

    istart(1) = 1
    iend(1) = i1
    istart(2) = 1
    iend(2) = i2
    istart(3) = time
    iend(3) = 1

    if((ivtype == NF_REAL) .and. (kind(data) == 4)) then
       call ncvgt( cdfid,id_data,istart,iend,data,rcode)
    else if((ivtype == NF_DOUBLE) .and. (kind(data) == 8)) then
       call ncvgt( cdfid,id_data,istart,iend,data,rcode)
    else if((ivtype == NF_DOUBLE) .and. (kind(data) == 4)) then
       call ncvgt( cdfid,id_data,istart,iend,tmp,rcode)
       data = tmp
    else
       write(unit=*, fmt='(a, i6)') &
            'Unrecognizable ivtype:', ivtype
       stop
    endif

    if(debug) then
       write(unit=*, fmt='(a,e24.12)') ' Sample data=', data(1,1)
    endif

    call ncclos(cdfid,rcode)

  end subroutine get_var_2d_real_cdf

!--------------------------------------------------------------------
  subroutine put_var_2d_real_cdf( file, var, data, &
                                  i1, i2, time, debug )

    implicit none

    include 'netcdf.inc'

    integer, intent(in)  ::  i1, i2, time
    character (len=80), intent(in) :: file
    logical, intent(in ) :: debug
    character (len=*), intent(in) :: var
    real, dimension(i1,i2), intent(in) :: data
    real(kind=8), dimension(i1,i2) :: tmp

    integer :: cdfid, rcode, id_data
    character (len=80) :: varnam, time1
    integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
    integer :: i, ivtype

    cdfid = ncopn(file, NCWRITE, rcode )

    if( rcode == 0) then
      if(debug) write(6,*) ' open netcdf file ', trim(file)
    else
      write(6,*) ' error openiing netcdf file ', trim(file)
      stop
    end if

    id_data = ncvid( cdfid, var, rcode )

    rcode = nf_inq_var( cdfid, id_data, varnam, ivtype, ndims, dimids, natts )
    if(debug) then
      write(6,*) ' number of dims for ',var,' ',ndims
    endif
    do i=1,ndims
      rcode = nf_inq_dimlen( cdfid, dimids(i), idims(i) )
      if(debug) write(6,*) ' dimension ',i,idims(i)
    enddo

!---check the dimensions

    if((i1 /= idims(1)) .or.  &
       (i2 /= idims(2)) .or.  &
       (time > idims(3))     )  then

       write(6,*) ' error in 3d_var_real read, dimension problem '
       write(6,*) i1, idims(1)
       write(6,*) i2, idims(2)
       write(6,*) time, idims(3)
       write(6,*) ' error stop '
       stop
     end if

!----get the data

     istart(1) = 1
     iend(1) = i1
     istart(2) = 1
     iend(2) = i2
     istart(3) = time
     iend(3) = 1

     if((ivtype == NF_REAL) .and. (kind(data) == 4)) then
        call ncvpt( cdfid,id_data,istart,iend,data,rcode)
     else if((ivtype == NF_DOUBLE) .and. (kind(data) == 8)) then
        tmp = data
        call ncvpt( cdfid,id_data,istart,iend,tmp,rcode)
     else if((ivtype == NF_DOUBLE) .and. (kind(data) == 4)) then
        tmp = data
        call ncvpt( cdfid,id_data,istart,iend,tmp,rcode)
     else
        write(unit=*, fmt='(a, i6)') &
            'Unrecognizable ivtype:', ivtype
        stop
     endif

     call ncclos(cdfid,rcode)

  end subroutine put_var_2d_real_cdf
!---------------------------------------------------------------
  subroutine get_var_3d_real_cdf( file, var, data, &
                                  i1, i2, i3, time, debug )

  implicit none

  include 'netcdf.inc'

  integer, intent(in)  ::  i1, i2, i3, time
  character (len=80), intent(in) :: file
  logical, intent(in ) :: debug
  character (len=*), intent(in) :: var
  real, dimension(i1,i2,i3), intent(out) :: data
  real(kind=8), dimension(i1,i2,i3) :: tmp

  character (len=80) :: varnam, time1

  integer :: cdfid, rcode, id_data
  integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
  integer :: i, ivtype

  cdfid = ncopn(file, NCNOWRIT, rcode )

  if( rcode /= 0) then
    write(6,*) ' error openiing netcdf file ', trim(file)
    stop
  end if

  id_data = ncvid( cdfid, var, rcode )
  rcode = nf_inq_var( cdfid, id_data, varnam, ivtype, ndims, dimids, natts )

  if(debug) then
    write(unit=*, fmt='(3a,i6)') ' get_var_3d_real_cdf: dims for ',var,' ',ndims
    write(unit=*, fmt='(a,i6)') ' ivtype=', ivtype
    write(unit=*, fmt='(a, a)') ' varnam=', trim(varnam)
    write(unit=*, fmt='(a,i6)') ' kind(data)=', kind(data)
  endif

  print*,'get var',var,' ',ndims
  do i=1,ndims
    rcode = nf_inq_dimlen( cdfid, dimids(i), idims(i) )
    if(debug) write(unit=*, fmt='(a,2i6)') ' dimension ',i,idims(i)
  enddo
    print*, ' rcode ', rcode
    print*, ' i1, i2, i3 ', i1, i2, i3

!  check the dimensions

   if( (i1 /= idims(1)) .or.  &
       (i2 /= idims(2)) .or.  &
       (i3 /= idims(3)) .or.  &
       (time > idims(4))     )  then

     write(6,*) ' error in 3d_var_real read, dimension problem '
     write(6,*) i1, idims(1)
     write(6,*) i2, idims(2)
     write(6,*) i3, idims(3)
     write(6,*) time, idims(4)
     write(6,*) ' error stop '
     stop

   end if
!  get the data

    istart(1) = 1
    iend(1) = i1
    istart(2) = 1
    iend(2) = i2
    istart(3) = 1
    iend(3) = i3
    istart(4) = time
    iend(4) = 1

    if((ivtype == NF_REAL) .and. (kind(data) == 4)) then
       call ncvgt( cdfid,id_data,istart,iend,data,rcode)
    else if((ivtype == NF_DOUBLE) .and. (kind(data) == 4)) then
       call ncvgt( cdfid,id_data,istart,iend,tmp,rcode)
       data = tmp
    else if((ivtype == NF_DOUBLE) .and. (kind(data) == 8)) then
       call ncvgt( cdfid,id_data,istart,iend,data,rcode)
    else
       write(unit=*, fmt='(a, i6)') &
            'Unrecognizable ivtype:', ivtype
       stop
    endif

    if(debug) then
       write(unit=*, fmt='(a,e24.12)') ' Sample data=', data(1,1,1)
    endif

    call ncclos(cdfid,rcode)

  end subroutine get_var_3d_real_cdf
!------------------------------------------------------------
  subroutine put_var_3d_real_cdf( file, var, data, &
                                  i1, i2, i3, time, debug )

  implicit none

  include 'netcdf.inc'

  integer, intent(in)  ::  i1, i2, i3, time
  character (len=80), intent(in) :: file

  logical, intent(in ) :: debug
  character (len=*), intent(in) :: var
  real, dimension(i1,i2,i3), intent(in) :: data
  real(kind=8), dimension(i1,i2,i3) :: tmp

  integer cdfid, rcode, id_data
  character (len=80) :: varnam, time1
  integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
  integer :: i, ivtype

  cdfid = ncopn(file, NCWRITE, rcode )
  if( rcode /= 0) then
    write(unit=*, fmt='(2a)') ' error openiing netcdf file ', trim(file)
    stop
  end if

  id_data = ncvid( cdfid, var, rcode )

  rcode = nf_inq_var( cdfid, id_data, varnam, ivtype, ndims, dimids, natts )

  if(debug) then
    write(unit=*, fmt='(3a,i6)') ' put_var_3d_real_cdf: dims for ',var,' ',ndims
  endif

  do i=1,ndims
    rcode = nf_inq_dimlen( cdfid, dimids(i), idims(i) )
    if(debug) write(6,*) ' dimension ',i,idims(i)
  enddo

!  check the dimensions
   if( (i1 /= idims(1)) .or.  &
       (i2 /= idims(2)) .or.  &
!       (i3 /= idims(3)) .or.  &
       (time > idims(4))     )  then

     write(6,*) ' error in 3d_var_real read, dimension problem '
     write(6,*) i1, idims(1)
     write(6,*) i2, idims(2)
     write(6,*) i3, idims(3)
     write(6,*) time, idims(4)
     write(6,*) ' error stop '
     stop

   end if
!  get the data

    istart(1) = 1
    iend(1) = i1
    istart(2) = 1
    iend(2) = i2
    istart(3) = 1
    iend(3) = i3
    istart(4) = time
    iend(4) = 1

    if((ivtype == NF_REAL) .and. (kind(data) == 4)) then
       call ncvpt( cdfid,id_data,istart,iend,data,rcode)
    else if((ivtype == NF_DOUBLE) .and. (kind(data) == 8)) then
       tmp = data
       call ncvpt( cdfid,id_data,istart,iend,tmp,rcode)
    else if((ivtype == NF_DOUBLE) .and. (kind(data) == 4)) then
       tmp = data
       call ncvpt( cdfid,id_data,istart,iend,tmp,rcode)
    else
       write(unit=*, fmt='(a, i6)') &
            'Unrecognizable ivtype:', ivtype
       stop
    endif

     call ncclos(cdfid,rcode)

  end subroutine put_var_3d_real_cdf

