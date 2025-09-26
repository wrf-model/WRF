module aero_mod

implicit none
private

public :: get_aero_info, read_wrf_arw_aero

contains

  subroutine get_aero_info(process_aero,aeros_to_process,num_aeros)
   implicit none
   integer,parameter :: num_aeros_max = 200
   logical, intent(out) :: process_aero
   character(len=40),intent(out) :: aeros_to_process(1:num_aeros_max)
   integer, intent(out) :: num_aeros
   logical :: isfile
   namelist / be_for_aero_nl / process_aero,aeros_to_process

   ! Initialize variables
   process_aero = .false.  
   aeros_to_process = 'missing' 
   num_aeros = 1 

   inquire(file='be_for_aero.nl', exist=isfile)
   if ( isfile ) then
      open(unit=98, file='be_for_aero.nl', &
         form='formatted', status='old', action='read')
      read(98, be_for_aero_nl)
      close(98)
      if ( process_aero ) then
         write(6,fmt='(a)') 'Computing BE for aerosols'
         num_aeros = count(aeros_to_process.ne.'missing')
         write(6,fmt='(a,a)') 'Aerosol species are ',aeros_to_process(1:num_aeros)
      endif

   endif

  end subroutine get_aero_info

  subroutine read_wrf_arw_aero(filename,nx,ny,nz,mype,sf,vp,t,q,qm,p,num_aeros,aero)
   implicit none

   character(len=*), intent(in)      :: filename
   integer, intent(in)      :: nx, ny, nz, mype

   real, intent(out)        :: sf(1:nx,1:ny,1:nz)
   real, intent(out)        :: vp(1:nx,1:ny,1:nz)
   real, intent(out)        :: t(1:nx,1:ny,1:nz)
   real, intent(out)        :: q(1:nx,1:ny,1:nz)
   real, intent(out)        :: qm(1:nx,1:ny,1:nz)
   real, intent(out)        :: p(1:nx,1:ny)
   integer, intent(in)      :: num_aeros
   real, intent(out)        :: aero(1:num_aeros,1:nx,1:ny,1:nz)

   real                     :: ds
   character (len=10) :: date
   integer            :: nrec

   integer            :: i,j,k
!
    nrec = 0 ! CSS...originally 1
    open(unit=24,file=trim(filename),form='unformatted', &
             status='old', action='read')

! Psi
   read(24,err=100,end=99)sf
   nrec=nrec+1
! Chi
   read(24,err=100,end=99)vp
   nrec=nrec+1
! Fill zero vp fileds with adjacent value
   vp(:,1,:) =2*vp(:,2,:) - vp(:,3,:)
   vp(1,:,:) =2*vp(2,:,:) - vp(3,:,:)
! Temp
   read(24,err=100,end=99)t
   nrec=nrec+1
! Moisture
   read(24,err=100,end=99)q
   nrec=nrec+1
! Moisturer_2
   read(24,err=100,end=99)qm
   nrec=nrec+1
! Surface pressure (in whatever units it is writtenn stage0_gsi) 
   read(24,err=100,end=99)p
   nrec=nrec+1
! Aerosols
   read(24,err=100,end=99)aero
   nrec=nrec+1

   close (24)

   return
99  write(6,*)'Unexpected end of file ',trim(filename),' in read_wrf_arw record recs: ',nrec
   stop
100 write(6,*)'read error in ',trim(filename),' in read_wrf_arw record recs: ',nrec
   stop

  end subroutine read_wrf_arw_aero


end module aero_mod
