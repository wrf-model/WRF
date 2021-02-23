  module mod_clddet_geoir
!   use netcdf
!  use mod_para
  use da_control, only: missing_r
  implicit none 
  contains

  subroutine qc_SDob(nlongitude,nlatitude,tbb,SDob)
   ! ------------QC: step 1 ----
   ! Kozo Okamoto, QJ, 2017 and Zhuge and Zou, JAMC, 2016
   ! 10.4wm(Okamoto, QJ, 2017),Chan13, 11.2(Zhuge and Zou, JAMC, 2016) for AHI Chan14
   ! only 10.8 for AGRI, !!!cha12!!!
    implicit none
    integer, intent(in):: nlongitude,nlatitude
    real, intent(in)    :: tbb(nlongitude,nlatitude)
    real, intent(inout) :: SDob(nlongitude,nlatitude)
! temp
    integer, parameter :: npixs_sdob = 3*3    ! pixles
    real :: temp_tb(npixs_sdob), temp_std
    integer :: i, j

    SDob=missing_r
!    call SD_ob(nlongitude,nlatitude,tbb, SDob)
     do i=2, nlongitude-1
       do j=2, nlatitude-1
         temp_tb(1) = tbb(i-1,j-1)
         temp_tb(2) = tbb(i,  j-1)
         temp_tb(3) = tbb(i+1,j-1)
         temp_tb(4) = tbb(i-1,j)
         temp_tb(5) = tbb(i,  j)
         temp_tb(6) = tbb(i+1,j)
         temp_tb(7) = tbb(i-1,j+1)
         temp_tb(8) = tbb(i,  j+1)
         temp_tb(9) = tbb(i+1,j+1)
         call find_std(nlongitude,nlatitude,npixs_sdob, temp_tb, temp_std, missing_r, missing_r)
         SDob(i,j) = temp_std
       end do
     end do

   end subroutine

  subroutine qc_RTCT(nlongitude,nlatitude,tbb,ter,rtct,rtct2)
   ! ------------QC: step 2 ----
   ! 11.2(Zhuge and Zou, JAMC, 2016) for AHI Chan14
   ! Relative thermal Contrast test 
   ! 11.2(Zhuge and Zou, JAMC, 2016) for AHI Chan14, and terrain
   ! only 10.8 for AGRI, !!!cha12!!!
    implicit none
    integer, intent(in):: nlongitude,nlatitude
    real, intent(in)    :: tbb(nlongitude,nlatitude)
    real, intent(in)    :: ter(nlongitude,nlatitude)
    real, intent(out) :: rtct(nlongitude,nlatitude)
    real, intent(inout) :: rtct2(nlongitude,nlatitude)
!    real, intent(inout) :: rtct2(nlongitude,nlatitude)
! temp
    integer, parameter :: npixs_rtct = 3*3    ! pixles
    real :: temp_ter(npixs_rtct), temp_tb(npixs_rtct), temp_std
    integer :: i, j, m, n

    rtct=missing_r  ! all missing_r for variables are same
!    call SD_ob(nlongitude,nlatitude,tbb, SDob)
     do i=2, nlongitude-1
       do j=2, nlatitude-1
         temp_ter(1) = ter(i-1,j-1)
         temp_ter(2) = ter(i,  j-1)
         temp_ter(3) = ter(i+1,j-1)
         temp_ter(4) = ter(i-1,j  )
         temp_ter(5) = ter(i,  j  )
         temp_ter(6) = ter(i+1,j  )
         temp_ter(7) = ter(i-1,j+1)
         temp_ter(8) = ter(i,  j+1)
         temp_ter(9) = ter(i+1,j+1)
         temp_tb(1) = tbb(i-1,j-1 )
         temp_tb(2) = tbb(i,  j-1 )
         temp_tb(3) = tbb(i+1,j-1 )
         temp_tb(4) = tbb(i-1,j   )
         temp_tb(5) = tbb(i,  j   )
         temp_tb(6) = tbb(i+1,j   )
         temp_tb(7) = tbb(i-1,j+1 )
         temp_tb(8) = tbb(i,  j+1 )
         temp_tb(9) = tbb(i+1,j+1 )
         m = COUNT( temp_ter /= missing_r)
         n = COUNT( temp_tb /= 0. )
         if ( n == npixs_rtct .and. m == npixs_rtct) then 
           call find_std(nlongitude,nlatitude,npixs_rtct, temp_ter, temp_std, missing_r, missing_r)
           rtct(i,j) = ( maxval(temp_tb)-tbb(i,j) ) - 3*temp_std*7*0.001
           rtct2(i,j) =  3*temp_std*7*0.001
         else 
           rtct(i,j) = 1 !missing_r
         end if 
       end do
     end do

  end subroutine

  subroutine qc_rtmt(nlongitude,nlatitude,tb12,tb13,rtmt) 
   ! ------------QC: step 2 ----
   ! 11.2(Zhuge and Zou, JAMC, 2016) for AHI Chan14
   ! Relative fourteen minus fifteen test for AHI
   ! Relative thirteen minus twelve for AGRI
   implicit none
    integer, intent(in):: nlongitude,nlatitude
   real, intent(in)  :: tb12(nlongitude,nlatitude)
   real, intent(in)  :: tb13(nlongitude,nlatitude)
   real, intent(out) :: rtmt(nlongitude,nlatitude)
   integer, parameter :: npixs_rtmt = 11*11  ! pixles
   real :: rtmt_tb12(npixs_rtmt),rtmt_tb13(npixs_rtmt)
   real :: max_tb12,max_tb13
   integer :: nstar, nend
   integer :: i, j, ij, m
          RTMT = missing_r
     do i=6, nlongitude-5   ! 11*11
       do j=6, nlatitude-5   ! 11*11
              ij=1
          do m=-5, 5 
              nstar = (ij-1)*11+1
              nend  = ij*11
              rtmt_tb12(nstar:nend) = tb12(i-5:i+5,j+m)
              rtmt_tb13(nstar:nend) = tb13(i-5:i+5,j+m)
              ij = ij + 1
          end do 
          call calc_rtmt(npixs_rtmt,rtmt_tb12,rtmt_tb13,max_tb12,max_tb13)
          if (max_tb12/=missing_r .and. max_tb13/=missing_r) then 
             rtmt(i,j)=abs( (tb12(i,j)-tb13(i,j))- & 
                    (max_tb12-max_tb13) )
          else 
             rtmt(i,j)=missing_r
          end if 
       end do
     end do
  end subroutine 

  subroutine qc_cwvt(nlongitude,nlatitude,tb10,tb12,cwvt1)
   ! ------------QC: step 3 ----
   ! 11.2(Zhuge and Zou, JAMC, 2016) for AHI Chan14
   ! Cirrus water vapor test for AHI using Ch10, Ch14 5*5pixel
   ! Ch10, Ch12 for AGRI
   implicit none
    integer, intent(in):: nlongitude,nlatitude
   real, intent(in) :: tb10(nlongitude,nlatitude)
   real, intent(in) :: tb12(nlongitude,nlatitude)
   real, intent(out) :: cwvt1(nlongitude,nlatitude)
!   real, intent(out) :: cwvt2(nlongitude,nlatitude)
   integer, parameter :: npixs_rtmt = 5*5  ! pixles
   real :: cwvt_tb10(npixs_rtmt),cwvt_tb12(npixs_rtmt)
   real :: cwvtmp
   integer :: nstar, nend
   integer :: i, j, ij, m
          cwvt1 = missing_r
          cwvtmp = 0 !missing_r
     do i=3, nlongitude-2   ! 11*11
       do j=3, nlatitude-2   ! 11*11
              ij=1
          do m=-2, 2
              nstar = (ij-1)*5+1
              nend  = ij*5
              cwvt_tb10(nstar:nend) = tb10(i-2:i+2,j+m)
              cwvt_tb12(nstar:nend) = tb12(i-2:i+2,j+m)
            ij = ij + 1
          end do		  
          call calc_correlation (npixs_rtmt,cwvt_tb10,cwvt_tb12,cwvtmp)
		  cwvt1(i,j)=cwvtmp
!          call calc_correlation2(npixs_rtmt,cwvt_tb10,cwvt_tb12,cwvt2(i,j))
       end do
      end do
  end subroutine

      subroutine qc_tit(nlongitude,nlatitude,tbb,tbb15m,tit,missing_r)
   ! ------------QC: step 4 ----
   ! 11.2(Zhuge and Zou, JAMC, 2016) for AHI Chan14
   ! Cirrus water vapor test for AHI using Ch14, Ch14min before
   ! Ch12, Ch12 for AGRI
   implicit none
    integer, intent(in):: nlongitude,nlatitude
   real, intent(in) :: tbb(nlongitude,nlatitude)
   real, intent(in) :: tbb15m(nlongitude,nlatitude)
   real, intent(in) :: missing_r
   real, intent(out) :: tit(nlongitude,nlatitude)
   integer :: nstar, nend
   integer :: i, j, ij, m 

   tit=tbb15m-tbb

  end subroutine


  subroutine find_std(nlongitude,nlatitude,n, arr, std_dev, miss1, miss2)
    implicit none
    integer, intent(in):: nlongitude,nlatitude
    integer, intent(in) :: n
    real, intent(in) :: arr(n)
    real, intent(in) :: miss1, miss2
    real, intent(out):: std_dev
    real :: variance, avg
    real :: arr_temp(n)
    integer :: i, m

    avg = 0.
    m=0
    do i=1, n
      if(arr(i)/=miss1 .and. arr(i)/=miss2)then
        avg = avg + arr(i)
        arr_temp(m+1)=arr(i)
        m=m+1
      end if
    end do

    if (m>=6) then
      avg=avg/m	
      variance=0.
      do i=1,m
        variance=variance+(arr_temp(i)-avg)**2
      end do
      variance=variance/m
      std_dev=sqrt(variance)
    else
      std_dev=missing_r
    end if
  end subroutine

  subroutine calc_rtmt(npixs_rtmt,temp_tb12,temp_tb13,max_tb12,max_tb13)
    implicit none
    integer, intent(in) :: npixs_rtmt
    real, intent(in) :: temp_tb12(npixs_rtmt),temp_tb13(npixs_rtmt)
    real, intent(out):: max_tb12, max_tb13
    real :: variance, avg
    real :: arr_temp12(npixs_rtmt),arr_temp13(npixs_rtmt)
    integer :: i, m

    m=0
    do i=1, npixs_rtmt
      if( temp_tb12(i) /=missing_r .and. temp_tb12(i) /=missing_r .and. & 
          temp_tb13(i) /=missing_r .and. temp_tb13(i) /=missing_r )then
          arr_temp12(m+1)=temp_tb12(i)
          arr_temp13(m+1)=temp_tb13(i)
        m=m+1
      end if
    end do
    ! 80 per 121 would be ok 
    if (m>=80) then
      max_tb12=maxval(arr_temp12(1:m))
      max_tb13=maxval(arr_temp13(1:m))
    else
      max_tb12=missing_r
      max_tb13=missing_r
    end if
  end subroutine

  subroutine calc_correlation( n, arr1, arr2, r)
    implicit none 

    integer, intent(in) :: n 
    real, intent(in)    :: arr1(n)
    real, intent(in)    :: arr2(n)
    real, intent(out) :: r
! below variables can be output: m, t(significant)
    integer, parameter :: lag = 0
    real    :: t
    integer :: m 
! temp variables 
    real    :: arr1_tem(n), arr2_tem(n)
!    real, allocatable :: x(:)
!    real, allocatable :: y(:) 
!    real, allocatable :: xdev(:)
!    real, allocatable :: ydev(:)
!    real, allocatable :: xdevydev(:)
!    real, allocatable :: xdevxdev(:)
!    real, allocatable :: ydevydev(:)
    real :: x(3*n)
    real :: y(3*n) 
    real :: xdev(3*n)
    real :: ydev(3*n)
    real :: xdevydev(3*n)
    real :: xdevxdev(3*n)
    real :: ydevydev(3*n)
    real :: xmn
    real :: ymn
    real :: COVxy
    real :: Sx
    real :: Sy
    integer :: i
     	
    arr1_tem=arr1
    arr2_tem=arr2
!    allocate( x(1:3*N) )
!    allocate( y(1:3*N) )

    x(:) = missing_r
    y(:) = missing_r
    x(N+1:2*N) = arr1_tem(:)
    y(N+1:2*N) = arr2_tem(:)

    y = EOSHIFT( y, SHIFT = -lag, BOUNDARY = missing_r )

    WHERE ( x == missing_r ) y = missing_r
    WHERE ( y == missing_r ) x = missing_r

    m = COUNT( x /= missing_r )
    r = missing_r
  if (m >= 16) then

    xmn = sum( x, dim = 1, mask = x /= missing_r ) / real(m)
    ymn = sum( y, dim = 1, mask = y /= missing_r ) / real(m)
!    allocate ( xdev(1:3*N) )
!    allocate ( ydev(1:3*N) )
    xdev(:) = x(:) - xmn
    where ( x == missing_r ) xdev(:) = missing_r
    ydev(:) = y(:) - ymn
    where ( y == missing_r ) ydev(:) = missing_r
!    allocate ( xdevydev(1:3*N) )
    xdevydev(:) = xdev(:) * ydev(:)
    where ( x == missing_r ) xdevydev(:) = missing_r
  
    COVxy = sum( xdevydev, dim = 1, mask = xdevydev /= missing_r ) / real(m)

!    allocate ( xdevxdev(1:3*N) )
    xdevxdev(:) = xdev(:) * xdev(:)
    where ( x == missing_r ) xdevxdev(:) = missing_r
    Sx = sqrt( sum( xdevxdev, dim = 1, mask = xdevxdev /= missing_r ) / real(m) )

!    allocate ( ydevydev(1:3*N) )
    ydevydev(:) = ydev(:) * ydev(:)
    where ( y == missing_r ) ydevydev(:) = missing_r
    Sy = SQRT( SUM( ydevydev, DIM = 1, MASK = ydevydev /= missing_r ) / REAL(m) )
    if ( Sx /= missing_r .and. Sy/= missing_r .and. (Sx * Sy/=0) ) r = COVxy / ( Sx * Sy )
  else 
    r = missing_r
  end if
!   deallocate( x, y )

  end subroutine 



	


end module 
