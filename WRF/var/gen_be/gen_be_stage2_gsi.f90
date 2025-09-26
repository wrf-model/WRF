program gen_be_stage2_gsi
!-----------------------------------------------------------------------
! Purpose: Creating WRF-ARW BE statistics for GSI                        
! 
! Author: Syed RH Rizvi,  NCAR/ESSL/MMM/DAG  08/06/2009
! Please acknowledge author/institute in work that uses this code.
!-----------------------------------------------------------------------
!
   use aero_mod, only : get_aero_info, read_wrf_arw_aero
   implicit none

#ifdef DM_PARALLEL
  include 'mpif.h'
#endif

   real, parameter      :: omega2=2.*7.292e-5
!  real, parameter      :: deg2rad=atan(1.)/45.,rad2deg=1.0/deg2rad
   real                 :: deg2rad,rad2deg
!
   real, allocatable    :: xlat(:,:),xlon(:,:),mapfac_x(:,:),mapfac_y(:,:)
   real, allocatable    :: wnew(:,:),bnew(:,:),anew(:,:)
   real, allocatable    :: f(:), count(:), sigl(:), rlat(:)
   integer, allocatable :: nlat(:,:)
!
   integer              :: nx,ny,nz,ni,nj,nk,dim1,dim2,dim3,ncat
   integer              :: nml_unit,ncases,ncount,member
   real                 :: lat_bins_in_deg
   integer              :: less_levels_from_top, debug
   real                 :: ds, angle,ncount_inv,min_xlat 
   logical              :: fstat

   character(len=120),allocatable  :: filen(:)       
   character(len=120)   :: stage1_gsi_dir, filename
   character(len=10)    :: date                    
   character(len=3)     :: ce                     

   integer              :: i,j,n, ierror, npes, mype
!
   namelist /gen_be_stage2_gsi_nl/debug,stage1_gsi_dir,nx,ny,nz, fstat,&
             less_levels_from_top, lat_bins_in_deg

   integer              :: num_aeros
   integer, parameter   :: num_aeros_max = 200
   character (len=40)   :: aeros_to_process(1:num_aeros_max)
   logical              :: process_aero


!---------------------------------------------------------------------------------
!--------
! MPI setup
#ifdef DM_PARALLEL
      call mpi_init(ierror)

      call mpi_comm_size(mpi_comm_world,npes,ierror)

      call mpi_comm_rank(mpi_comm_world,mype,ierror)
#else
     npes = 1
     mype = 0
#endif
!------------------------------------------------------------
!  Set default valuse for namelist parameters
!   
!------------------------------------------------------------

    stage1_gsi_dir = '/ptmp/rizvi/data/con200/run_gen_be_gsi/working/stage1_gsi'
    nx = 44 ; ny = 44 ; nz = 27 ; lat_bins_in_deg = 1.0
    less_levels_from_top = 0 ; fstat= .false. ; debug = 0
 
   call get_aero_info(process_aero,aeros_to_process,num_aeros)

   deg2rad=atan(1.)/45.
   rad2deg=1.0/deg2rad

   nml_unit=5
   open(unit=nml_unit, file='gen_be_stage2_gsi_nl.nl',form='formatted', status='old', action='read')
   read(nml_unit, gen_be_stage2_gsi_nl)
   close ( nml_unit)
   if(mype==0) write(6,nml= gen_be_stage2_gsi_nl)

   filename=trim(stage1_gsi_dir)//'/fixed_field'


    open(unit=10,file=trim(filename),form='unformatted', &
             status='old', action='read')

    read(10)date, dim1, dim2, dim3, ncases, ds
    if(mype==0) write(6,*)' ds = ',ds,' lat_bins (deg) = ',lat_bins_in_deg
    close (10)
    if(mype==0) write(6,*)' read dims: ',dim1,dim2,dim3,' ncases= ',ncases,' ds= ',ds

    if( nx /= dim1 .or. ny /= dim2 .or. nz /= dim3) then
    write(6,*)' mismatch dims in fixed_field= ',dim1,dim2,dim3,' in gen_be_stage2_gsi_nl.nl ',nx,ny,nz
    stop
    end if

    allocate( sigl(1:nz))
    allocate( mapfac_x(1:nx,1:ny) ,mapfac_y(1:nx,1:ny))
    allocate( xlat(1:nx,1:ny) ,xlon(1:nx,1:ny))
    allocate( nlat(1:nx,1:ny) )

    if( debug > 0  .and. mype==0)write(6,*)'calling get_fixed_fields'
    call get_fixed_fields(filename,nx,ny,nz, mype,ncat,mapfac_x,mapfac_y,sigl,&
                      lat_bins_in_deg,xlat,xlon,min_xlat)
!------------------------------------------
! Fix coriolis parameter
!------------------------------------------
    allocate( rlat(1:ncat))
    allocate( f(1:ncat))
     rlat(1) = min_xlat + lat_bins_in_deg/2.
     do i=2,ncat
     !hcl rlat(i) = min_xlat + (i-1)*lat_bins_in_deg
     rlat(i) = rlat(1) + (i-1)*lat_bins_in_deg
     end do

    if( fstat) then
     do i=1,ncat
      f(i)=sin(rlat(i)*deg2rad)*omega2
     enddo
     do i=1,ncat
      f(i)=f(i)/f(ncat/2) 
     enddo
    else
     f = 1.0
    end if
! Nullify the impact of coriolis parameter 
!------------------------------------------
!  Fix count & nlat-array 
!------------------------------------------
   allocate ( count(1:ncat) )
   count=0.0  ! How many points per latitude bin
   do j=1,ny
     do i=1,nx
       n=int((xlat(i,j)-min_xlat)/lat_bins_in_deg) + 1
       
       nlat(i,j)=min(ncat,max(1,n)) ! Which bin each latitude point falls into

      if( mype==0 .and. debug > 0) then
       if( n < 1 .or. n > ncat ) &
      write(6,*)i,j,' xlat= ',xlat(i,j),' got cat = ',n,' set cat = ',nlat(i,j)
      end if
       count(nlat(i,j))=count(nlat(i,j))+1.0
     end do
   end do
  if( mype == 0 .and. debug > 0) then
     do i=1,ncat
     if(count(i) <= 0) write(6,*)i,' Stats lat/counts and f = ',rlat(i),count(i),f(i)
     end do
  end if
    filename=trim(stage1_gsi_dir)//'/pert_files'

    open(unit=10,file=trim(filename),form='formatted', &
             status='old', action='read')

    allocate(filen(1:ncases))
    do i = 1, ncases
     read(10,'(a)')filen(i)
    end do
    close (10)
    allocate( wnew(1:ncat,1:nz),anew(1:nz,1:nz), bnew(1:ncat,1:nz)) 

!rizvi
! compute_reg_coeff does folowing:
! generates reg_psi_chi coeff [bnew(ncat,nz)-array] 
! generates reg_psi_ps  coeff [wnew(ncat,nz)-array] 
! generates reg_psi_t   coeff [anew(nz,nz)-array] 
! Note: reg_psi_ps holds same value for all category (1st dim)
!rizvi

    if( debug > 0  .and. mype==0)write(6,*)'calling compute_reg_coeff'
     call compute_reg_coeff &
         (nx,ny,nz,ncat,npes,mype, debug, ncases,filen, &
          sigl,f, anew,bnew,wnew,nlat,count)

    if( debug > 0  .and. mype==0)write(6,*)'calling compute_variance_and_len_scale'
     call compute_variance_and_len_scales(nx,ny,nz,ncat,&
         npes,mype,debug, ncases, less_levels_from_top,filen, &
         mapfac_x,mapfac_y, ds, rlat, xlat,xlon,sigl,f,anew,bnew,wnew, nlat,count, &
         process_aero,num_aeros,aeros_to_process(1:num_aeros) )

#ifdef DM_PARALLEL
    call mpi_finalize(ierror)
#endif

    deallocate(filen)               
    deallocate(sigl,mapfac_x,mapfac_y,xlat,xlon,nlat)
    deallocate(wnew,bnew,anew)

    stop
    end

    subroutine compute_reg_coeff &
              (nx,ny,nz,ncat,npes,mype,debug, ncases,filen, &
               sigl,f, anew,bnew,wnew,nlat,count)

    implicit none

#ifdef DM_PARALLEL
    include 'mpif.h'

    integer       :: mpi_rtype
    parameter(mpi_rtype=mpi_real8)


!    real, parameter            :: mpi_rtype=mpi_real8
#endif

    integer, intent(in)        :: nx,ny,nz,ncat,npes,mype,debug,ncases
    character(120), intent(in) :: filen(1:ncases)
    integer, intent(in)        :: nlat(1:nx,1:ny)
    real, intent(in)           :: sigl(1:nz)
    real, intent(in)           :: count(1:ncat)
    real, intent(in)           :: f(1:ncat)

    real                       :: sf(1:nx,1:ny,1:nz),vp(1:nx,1:ny,1:nz)
    real                       :: t1(1:nx,1:ny,1:nz), q(1:nx,1:ny,1:nz)
    real                       :: vor(1:nx,1:ny,1:nz),q2(1:nx,1:ny,1:nz)
    real                       :: t(1:nx,1:ny),p(1:nx,1:ny),rpb(1:nx,1:ny)

    real                       :: tu(1:nz,1:nz),tut(1:nz,1:nz)
    real                       :: td2(1:nz,1:nz),td3(1:nz,1:nz)
    real                       :: atilde(1:nz,1:nz)
    real                       :: vud(1:ncat,1:nz,1:2),vudt(1:ncat,1:nz,1:2)
    real                       :: td(1:nz,1:nz,1:ncat),tdt(1:nz,1:nz,1:ncat)
    real                       :: pu(1:ncat,1:nz),put(1:ncat,1:nz)
    real                       :: tlflt(1:nz,1:nz,1:2),tlfl(1:nz,1:nz,1:2)
    real                       :: fkflt(1:nz,1:nz,1:ncat),fkfl(1:nz,1:nz,1:ncat)
    real                       :: pdflt(1:ncat,1:nz,1:3),pdfl(1:ncat,1:nz,1:3)               

    real                       :: wnew(1:ncat,1:nz),bnew(1:ncat,1:nz)
    real                       :: anew(1:nz,1:nz)

    real                       :: func(1:nz,1:nz),funcs(1:nz,1:nz),biga8(1:nz,1:nz)
    real                       :: p8(1:nz),p88(1:nz),b8(1:nz),x8(1:nz)

!   
    integer                    :: npeloop, moop, loop 
    integer                    :: i,j,l,ll,n, ierror
    integer                    :: k,k2,km,kn,kp,kz,kv,kd,kt,kq,kq2,kvd,kvp,kvt,kvq
!

    if( debug > 0  .and. mype==0)write(6,*)'in compute_reg_coef'

!if(mype==3)then
!tmp4=f
!write(963)tmp4
!endif
!!!!!!!!!!!!!
!!!!!!!!!!!!!
! rizvi
! What vert_corr does
!
! a) Computes <psi,psi> vertical error covariance matrix (nz x nz)
! b) Normalize the covariance matrix
! c) Computes vertical length-scales rlen(l)= sqrt{1.0/(2-corr(k-1,l)-corr(k+1,l)}
! d) Normalize length-scale (rlen) with log(sigl)
! e) Builts vertical correlation matrix using exp{-0.5*(var/rlen)**2.0}
! f) Generate Chelosky decomposition factors for this correlation matrix in funcs
! g) Generate EOF's for correlation matrix in func
! h) Diagonal terms for this correlation matrix are in p8
! rizvi


    if( debug > 0  .and. mype==0)write(6,*)'calling vert_corr'
    call vert_corr(nx,ny,nz,ncases,ncat,npes,mype,&
                   func,funcs,p8,filen,nlat,count,sigl)


!!!!!!!!!!!!!

    vudt=0.
    put=0.
    tut=0.
    tdt=0.


       npeloop=ncases/npes
     if(npes*npeloop .ne. ncases) npeloop=npeloop+1

         do moop=1,npeloop
      loop=npes*(moop-1)+mype+1
       if (loop.le.ncases)then
    if( debug > 0  .and. mype==0)write(6,*)' calling read_wrf_arw for case: ',moop 

      call read_wrf_arw(trim(filen(loop)),nx,ny,nz, mype, sf,vp,t1,q,q2,p) 

! rizvi
! compute <psi,chi> and <psi,psi> covariance matrix
!  <psi,chi> & <psi,psi> in vudt(:,:,1) & vudt(:,:,2) arrays
! rizvi
   do k=1,nz
      do j=1,ny
      do i=1,nx
       n=nlat(i,j)
       vudt(n,k,1)=vudt(n,k,1)+sf(i,j,k)*vp(i,j,k)
       vudt(n,k,2)=vudt(n,k,2)+sf(i,j,k)*sf(i,j,k)
      enddo
      enddo
   enddo

! rizvi
!  psi gets projected on <psi,psi> vertical error corr basis vectors (func)
!  Thus after psi_tilde sf-array holds psi_coeff
! rizvi

    if( debug > 0  .and. mype==0)write(6,*)' calling psi_tilde'    
   call psi_tilde(nx,ny,nz,sf,func)
    if( debug > 0  .and. mype==0)write(6,*)' called  psi_tilde'    

   do k=1,nz
    do j=1,ny
    do i=1,nx
      n=nlat(i,j)
!        put(n,k)=put(n,k)+sf(i,j,k)*p(i,j)*f(n)
       put(1,k)=put(1,k)+sf(i,j,k)*p(i,j)
    enddo
    enddo
   enddo

    if( debug > 0  .and. mype==0)write(6,*)' done <sf,ps> computation'
!rizvi
! tut-array holds <psi_coeff,t> weighted with normalized coriolis parameter
!  Note: No variation with latitudes (not for all ncat)
! tdt-array holds <psi_coeff,psi_coeff>
!rizvi

   do l=1,nz
    do k=1,nz
      do j=1,ny
      do i=1,nx
       n=nlat(i,j)
       tut(l,k)=tut(l,k)+sf(i,j,k)*t1(i,j,l)*f(n)
       tdt(k,l,n)=tdt(k,l,n)+sf(i,j,k)*sf(i,j,l)
      enddo
      enddo
    enddo
   enddo
    if( debug > 0  .and. mype==0)write(6,*)' done <sf,t> computation'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  endif !loop<ncases
 ENDDO ! DO LOOP

    if( debug > 0  .and. mype==0)write(6,*)' done case loop' 
! rizvi
! After mpi_reduce sum is stored in
! a) td-array             <psi_coeff,psi_coeff>
! b) tu-array             <psi_coeff,t>    No lat variation but weighted with f
! c) vud(:,:,1 & 2)-array <psi,chi> & <psi,psi>
! d) pu-array             <psi_coeff,ps>   Effectively no lat variation
! rizvi

#ifdef DM_PARALLEL
    if( debug > 0  .and. mype==0)write(6,*)' calling mpi_reduce for td'
      call mpi_allreduce(tdt,td,ncat*nz*nz,mpi_rtype,mpi_sum, &
         mpi_comm_world,ierror)
    if( debug > 0  .and. mype==0)write(6,*)' calling mpi_reduce for tu'
      call mpi_allreduce(tut,tu,nz*nz,mpi_rtype,mpi_sum, &
         mpi_comm_world,ierror)
    if( debug > 0  .and. mype==0)write(6,*)' calling mpi_reduce for vud'
      call mpi_allreduce(vudt,vud,ncat*nz*2,mpi_rtype,mpi_sum, &
         mpi_comm_world,ierror)
    if( debug > 0  .and. mype==0)write(6,*)' calling mpi_reduce for pu'
      call mpi_allreduce(put,pu,ncat*nz,mpi_rtype,mpi_sum, &
         mpi_comm_world,ierror)
#else
   td = tdt
   tu = tut
   vud = vudt
   pu = put
#endif

!rizvi
! td2-array <psi_coeff,psi_coeff> weighted with f
! td3-array <psi_coeff,psi_coeff>
!
!rizvi

   td2=0.
   td3=0.
   do ll=1,ncat
  td2(:,:)=td2(:,:)+td(:,:,ll)*f(ll)*f(ll)
  td3(:,:)=td3(:,:)+td(:,:,ll)
!rizvi
! bnew-array holds category wise <psi,chi>/<psi,psi>
!rizvi

!  if(ncount(ll).ne.0.)then
!    anorm=1./float(ncount(ll)*nz*ncases)
! td(:,:,ll)=td(:,:,ll)*anorm
!! tu(ll,:,:)=tu(ll,:,:)*anorm
! pu(ll,:)=pu(ll,:)*anorm
!  else
!  write(0,*)'count=',ll,ncount(ll)
!  endif
   enddo    ! Loop over ll (category)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!turn off smooth !!!!!!!!!!
!go to 876
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!      rwl=5.
!      call rfpa(rwl,ndeg,alpha,betas)
!      call set_turn(ndeg,alpha,betas,turn)
!
! neh=next/2
! nehp=neh+1
!do l=1,3
!do k=1,nz
!     smooth(1:neh)=pdfl(1,k,l)
!     smooth(nehp:ncat+neh)=pdfl(1:ncat,k,l)
!     smooth(ncat+nehp:ncat+next)=pdfl(ncat,k,l)
!    call rfgy(smooth,1,ncat+next,ndeg,alpha,betas,turn)
!     pdfl(1:ncat,k,l)=smooth(nehp:ncat+neh)
!enddo
!enddo
!
!do k=1,nz
!do l=1,nz
!     smooth(1:neh)=fkfl(l,k,1)
!     smooth(nehp:ncat+neh)=fkfl(l,k,1:ncat)
!     smooth(ncat+nehp:ncat+next)=fkfl(l,k,ncat)
!    call rfgy(smooth,1,ncat+next,ndeg,alpha,betas,turn)
!     fkfl(l,k,1:ncat)=smooth(nehp:ncat+neh)
!enddo
!enddo
!
!!do k=1,nz
!!do l=1,nz
!!     smooth(1:neh)=tlfl(1,l,k)
!!     smooth(nehp:ncat+neh)=tlfl(1:ncat,l,k)
!!     smooth(ncat+nehp:ncat+next)=tlfl(ncat,l,k)
!!    call rfgy(smooth,1,ncat+next,ndeg,alpha,betas,turn)
!!     tlfl(1:ncat,l,k)=smooth(nehp:ncat+neh)
!!enddo
!!enddo
!
!876 continue
!
!!!!!!!!!!!!!!!!!! smooth !!!!!!!!
!      call  RFDPARV(vwl,RATE,alv,nz,ndeg)
!   call smoothz(w,nz,1 ,ndeg,alv,be,dssv,1)
!!!!!!!!!!!!!!!!!! smooth !!!!!!!!
          bnew=0.
           do k=1,nz
         do l=1,ncat
          bnew(l,k)=bnew(l,k)+vud(l,k,1)/vud(l,k,2)
         enddo
         enddo
wnew=0.
anew=0.
!do j=1,ncat
  do j=1,1    
   biga8=0.
   do k=1,nz
   do l=1,nz
   biga8(l,k)=td3(l,k)
   enddo
   enddo
   do k=1,nz
!   p88(k)=sqrt(td(k,k,j))
   p88(k)=sqrt(td3(k,k))
   biga8(k,k)=biga8(k,k)*1.001_8
   enddo
!rizvi 
! After choldc
! a) biga8-array is chol-factor of <psi_coeff,psi_coeff>
! b) p88-array diagonal for chol-factor of <psi_coeff,psi_coeff>
!
!rizvi 

    if( debug > 0  .and. mype==0)write(6,*)' calling choldc'       
    call choldc(biga8,nz,p88,ierror)

!        get bigbhat, the pressure expansion coefs

    do k=1,nz
     b8(k)=pu(j,k)
    end do

!rizvi 
! Solves          biag8        * x8 = b8
!        <psi_coeff,psi_coeff> * x8 = <psi_coef,ps>  No lat variation
! After solving x8-array holds reg_psi_coeff_ps coeffs
!rizvi
    if( debug > 0  .and. mype==0)write(6,*)' calling cholsl'       
    call cholsl(biga8,nz,p88,b8,x8)

!rizvi
! wnew-array holds reg_psi_coeff_ps * basis of vertical error covar <psi,psi>
! Thus effectively the actual reg coeff of <psi,ps>
! Note: Same array is filled for all lats (ncat)
!rizvi



   do k=1,nz
   do n=1,nz
   wnew(j,k)=wnew(j,k)+x8(n)*func(k,n)
   end do
   end do

  enddo   ! Loop over j=1

    do k=1,nz
      wnew(2:ncat,k)=wnew(1,k)
   enddo
!        the temperature expansion coefs
   do k=1,nz
   do l=1,nz
   biga8(l,k)=td2(l,k)
   enddo
   enddo
   do k=1,nz
   p88(k)=sqrt(td2(k,k))
   biga8(k,k)=biga8(k,k)*1.001_8
   enddo
!rizvi 
! After choldc
! a) biga8-array is chol-factor of <psi_coeff,psi_coeff> weighted with f
! b) p88-array diagonal for chol-factor of <psi_coeff,psi_coeff> weighted with f
!

    if( debug > 0  .and. mype==0)write(6,*)' calling choldc'       
    call choldc(biga8,nz,p88,ierror)


   do kz=1,nz
    do k=1,nz
     b8(k)=tu(kz,k)
    end do
    if( debug > 0  .and. mype==0)write(6,*)' calling cholsl'       
    call cholsl(biga8,nz,p88,b8,x8)
     do n=1,nz
      atilde(n,kz)=x8(n)
     end do
   end do
!rizvi
! anew-array holds reg_psi_coeff_t  * basis of vertical error covar <psi,psi>
! Thus effectively the actual reg coeff of <psi,t> level wise
! Note: No latitudinal variation of <psi,t> reg coeffs
!rizvi

    do k=1,nz
     do kp=1,nz
      anew(k,kp)=0.
      do n=1,nz
      anew(k,kp)=anew(k,kp)+atilde(n,k)*func(kp,n)
      end do
     end do
    end do
  return
  end subroutine compute_reg_coeff         
 

  subroutine compute_variance_and_len_scales(nx,ny,nz,ncat, &
              npes,mype,debug, ncases, less_levels_from_top, filen, &
              mapfac_x,mapfac_y, ds,rlat, &
              xlat,xlon,sigl,f,anew,bnew,wnew, nlat,count,process_aero,num_aeros,aeros_to_process) ! added aeros
!
   use aero_mod , only : read_wrf_arw_aero
   implicit none

#ifdef DM_PARALLEL
   include 'mpif.h'
 
     integer       :: mpi_rtype
    parameter(mpi_rtype=mpi_real8)

!   real, parameter           :: mpi_rtype=mpi_real8
#endif

   real   , parameter    :: rsmall = 1.0e-30          ! small number to assign with hydrometeor
   real   , parameter    :: rbig   = 1.0e30           ! big number to assign with hydrometeor

   integer, intent(in)       :: nx,ny,nz,ncat
   integer, intent(in)       :: npes,mype,debug,ncases,less_levels_from_top
   character(120), intent(in):: filen(ncases)
   integer, intent(in)       :: nlat(1:nx,1:ny)
   real,    intent(in)       :: mapfac_x(1:nx,1:ny),mapfac_y(1:nx,1:ny)
   real,    intent(in)       :: sigl(1:nz)
   real,    intent(in)       :: xlat(1:nx,1:ny),xlon(1:nx,1:ny)
   real, intent(in)          :: f(1:ncat), rlat(1:ncat)
   real, intent(in)          :: anew(1:nz,1:nz),wnew(1:ncat,1:nz),bnew(1:ncat,1:nz)
   real, intent(in)          :: count(1:ncat)
   real, intent(in)          :: ds

   real                      :: sf(1:nx,1:ny,1:nz),vp(1:nx,1:ny,1:nz)
   real                      :: t1(1:nx,1:ny,1:nz), q(1:nx,1:ny,1:nz)
   real                      :: vor(1:nx,1:ny,1:nz),q2(1:nx,1:ny,1:nz)
   real                      :: t(1:nx,1:ny),p(1:nx,1:ny),rpb(1:nx,1:ny)
   real                      :: bms(1:ncat,1:nz*3*2+3),bmst(1:ncat,1:nz*3*2+3)
   real                      :: chi_u(1:nx,1:ny,1:nz),t_u(1:nx,1:ny,1:nz)
   real                      :: ps_u(1:nx,1:ny)

   real                      :: tu(1:nz,1:nz),tut(1:nz,1:nz)
   real                      :: td2(1:nz,1:nz),td3(1:nz,1:nz)
   real                      :: atilde(1:nz,1:nz)
   real                      :: td(1:nz,1:nz,1:ncat),tdt(1:nz,1:nz,1:ncat)
   real                      :: pu(1:ncat,1:nz),put(1:ncat,1:nz)
   real                      :: tlflt(1:nz,1:nz,1:2),tlfl(1:nz,1:nz,1:2)
   real                      :: fkflt(1:nz,1:nz,1:ncat),fkfl(1:nz,1:nz,1:ncat)
   real                      :: pdflt(1:ncat,1:nz,1:3),pdfl(1:ncat,1:nz,1:3)               

   real                      :: amp(1:ncat,1:nz*5+1) ,ampt(1:ncat,1:nz*5+1)

   real                      :: rsig(1:nz),dsig(1:nz)
   real                      :: qcount(1:ncat,1:nz),qcountt(1:ncat,1:nz)
   real                      :: hl1(1:ncat),hl(1:ncat,1:nz*4+1),hlt(1:ncat,1:nz*4+1)
   real                      :: vorr(1:2,1:nz*4),vorrt(1:2,1:nz*4)
   real                      :: vl(1:nz*4)

   integer                   :: ierror, npeloop, moop, loop 
   integer                   :: nz2,nz3,nz4,nz5,nz6

   integer                   :: i,j,jb,l,lq,ld,lt,ll,m,n
   integer                   :: k,k1,k2,kb,km,kn,kp,kz,kv,kd,kt,kq,kq2,kvd,kvp,kvt,kvq
   real                      :: rncas, rnorm, a, b, c
   real                      :: xmax, xmin

   logical, intent(in)       :: process_aero
   integer, intent(in)       :: num_aeros
   character(len=40), intent(in) :: aeros_to_process(1:num_aeros)
   real                      :: aero(1:num_aeros,1:nx,1:ny,1:nz)
   integer                   :: kk,jj,nn,nvars_tot
   real                      :: amp_aero(1:num_aeros,1:ncat,1:nz),ampt_aero(1:num_aeros,1:ncat,1:nz)
   real                      :: vorr_aero(1:num_aeros,1:2,1:nz), vorrt_aero(1:num_aeros,1:2,1:nz)
   real                      :: hl_aero(1:num_aeros,1:ncat,1:nz), hlt_aero(1:num_aeros,1:ncat,1:nz)
   real                      :: vl_aero(1:num_aeros,1:nz)

!----------------------------------------------------------------------------
!  Single precission variables
!-----------------------------------------------------------------------------
   real*4               :: sigl4(1:nz),rlat4(1:ncat), tmp4(1:ncat)
   real*4, allocatable  :: agv(:,:,:),wgv(:,:),bv(:,:)
   real*4, allocatable  :: vz(:,:),vd(:,:),vt(:,:),vq(:,:)
   real*4, allocatable  :: corz(:,:),cord(:,:),corh(:,:),corq(:,:),corq2(:,:)
   real*4, allocatable  :: hwll(:,:,:),hwllp(:),corp(:)
!----------------------------------------------------------------------------
   integer, parameter   :: len_vars_out = 5
   integer, parameter   :: nvars = 5
   integer, allocatable :: nsig(:)
   real*4, allocatable  :: stds(:,:,:), vzs(:,:,:)
   character(len=len_vars_out), allocatable :: vars(:)

   real*4, allocatable  :: hwll_aero(:,:,:)
   real*4, allocatable  :: v_aero(:,:,:)
   real*4               :: ampt_aero4(1:num_aeros,1:ncat,1:nz)
   character(len=len_vars_out) :: vars_aeros(1:num_aeros)
!----------------------------------------------------------------------------

   if ( process_aero) then
      allocate(vars(nvars+num_aeros) )  ! meteo variables plus a specified number of aerosols
      allocate(nsig(nvars+num_aeros) )
   else
      allocate(vars(1:nvars) )
      allocate(nsig(1:nvars) )
   endif

   vars(1)='sf';vars(2)='vp'; vars(3)='t'; vars(4)='q'; vars(5)='ps'
   nsig(1)= nz ; nsig(2)=nz ; nsig(3)=nz ; nsig(4)=nz ; nsig(5)=1
   allocate( stds(4,ncat,nz) )
   allocate( vzs(4,nz,0:ncat+1) )

    amp =0. ; qcount=0. ; hl=0. ; vorr=0. ; bms=0.
    nz2=nz*2 ; nz3=nz*3 ; nz4=nz*4 ; nz5=nz*5 ; nz6=nz*6 
    if ( process_aero) amp_aero = 0. ; hl_aero = 0. ; vorr_aero = 0.

    npeloop=ncases/npes
    if(npes*npeloop .ne. ncases) npeloop=npeloop+1

      do moop=1,npeloop
      loop=npes*(moop-1)+mype+1
       if (loop.le.ncases)then
   if ( mype == 0 .and. debug > 0) write(6,*)'Computing lenscale for case : ',moop
   if ( process_aero ) then
      call read_wrf_arw_aero(trim(filen(loop)),nx,ny,nz, mype, sf,vp,t1,q,q2,p,num_aeros,aero)
   else
      call read_wrf_arw(trim(filen(loop)),nx,ny,nz, mype, sf,vp,t1,q,q2,p)
   end if
     
       rpb=0.
          do k=1,nz
          do j=1,ny
          do i=1,nx
          n=nlat(i,j)
          rpb(i,j)=rpb(i,j)+wnew(n,k)*sf(i,j,k)
          enddo
          enddo
          enddo

          do j=1,ny
          do i=1,nx
            n=nlat(i,j)
             a=p(i,j)*p(i,j)
             b=rpb(i,j)*rpb(i,j)
             rpb(i,j)=p(i,j)-rpb(i,j)
             c=rpb(i,j)*rpb(i,j)
             bms(n,nz6+1)=bms(n,nz6+1)+a
             bms(n,nz6+2)=bms(n,nz6+2)+b
             bms(n,nz6+3)=bms(n,nz6+3)+c
          amp(n,1)=amp(n,1)+c
          end do
          end do
     ps_u=rpb
       call horz_lenscale(nx,ny,rpb,ds,mapfac_x,mapfac_y,hl1,nlat,ncat)
       hl(1:ncat,1)= hl(1:ncat,1)+ hl1(1:ncat)
 
!!!!!!!!!!!!sf
   do l=1,nz
     kv=1+l
          do j=1,ny       
          do i=1,nx
            n=nlat(i,j)
          amp(n,kv)=amp(n,kv)+sf(i,j,l)*sf(i,j,l)
          enddo
          enddo

         kb=max(l-1,1)
       do k=kb,l
          m=k-l+2
          do j=1,ny
          do i=1,nx
            vorr(m,l)=vorr(m,l)+sf(i,j,k)*sf(i,j,l)
          enddo
          enddo
       enddo

        call horz_lenscale(nx,ny,sf(:,:,l),ds,mapfac_x,mapfac_y,hl1,nlat,ncat)
        hl(1:ncat,kv)= hl(1:ncat,kv)+ hl1(1:ncat)
   enddo
 
!!!!!!!!!!!!vp
   do l=1,nz
     kd=nz+1+l
     ld=nz+l
       rpb=0.
          do j=1,ny
          do i=1,nx
            n=nlat(i,j)
            rpb(i,j)=rpb(i,j)+bnew(n,l)*sf(i,j,l)
          enddo
          enddo

          do j=1,ny       
          do i=1,nx
            n=nlat(i,j)
             bms(n,l)=bms(n,l)+vp(i,j,l)*vp(i,j,l)
             bms(n,nz+l)=bms(n,nz+l)+rpb(i,j)*rpb(i,j)
             rpb(i,j)=vp(i,j,l)-rpb(i,j)
             c=rpb(i,j)*rpb(i,j)
             bms(n,nz2+l)=bms(n,nz2+l)+c
             amp(n,kd)=amp(n,kd)+c

          end do
          end do

       chi_u(:,:,l)=rpb(:,:)
       call horz_lenscale(nx,ny,rpb,ds,mapfac_x,mapfac_y,hl1,nlat,ncat)
       hl(1:ncat,kd)= hl(1:ncat,kd)+ hl1(1:ncat)

       if(l==1) p=rpb
         kb=max(l-1,1)
       do k=kb,l
          m=k-l+2
          do j=1,ny
          do i=1,nx
            vorr(m,ld)=vorr(m,ld)+rpb(i,j)*p(i,j)
          enddo
          enddo
      if(k<l) p=rpb
       enddo

   end do

!!!!!!!!!!!!t
   do l=1,nz
      kt=nz*2+1+l
      lt=nz*2+l
         rpb=0.
          do k=1,nz  
          do j=1,ny       
          do i=1,nx
           n=nlat(i,j)
           rpb(i,j)=rpb(i,j)+anew(l,k)*sf(i,j,k)*f(n)
          enddo
          enddo
          enddo

          do j=1,ny       
          do i=1,nx
            n=nlat(i,j)
             bms(n,nz3+l)=bms(n,nz3+l)+t1(i,j,l)*t1(i,j,l)
             bms(n,nz4+l)=bms(n,nz4+l)+rpb(i,j)*rpb(i,j)
             rpb(i,j)=t1(i,j,l)-rpb(i,j)
             c=rpb(i,j)*rpb(i,j)
             bms(n,nz5+l)=bms(n,nz5+l)+c
             amp(n,kt)=amp(n,kt)+c
          end do
          end do
       t_u(:,:,l)=rpb(:,:)
        call horz_lenscale(nx,ny,rpb,ds,mapfac_x,mapfac_y,hl1,nlat,ncat)
        hl(1:ncat,kt)= hl(1:ncat,kt)+ hl1(1:ncat)
       if(l==1) p=rpb
         kb=max(l-1,1)
       do k=kb,l
          m=k-l+2
          do j=1,ny
          do i=1,nx
             vorr(m,lt)=vorr(m,lt)+rpb(i,j)*p(i,j)
          enddo
          enddo
      if(k<l) p=rpb
       enddo

   enddo
!!!!!!!!!!!!q
   do l=1,nz- less_levels_from_top
        kq=nz*4+1+l
          do j=1,ny
          do i=1,nx
      n=q2(i,j,l)*20.+1
      if(n< ncat .and. n> 0.)then
            amp(n,kq)=amp(n,kq)+q(i,j,l)*q(i,j,l)
            qcount(n,l)=qcount(n,l)+1.
      endif
          end do
          end do
   end do

   do l=1,nz- less_levels_from_top
       kq=nz*3+1+l
       lq=nz*3+l
          do j=1,ny
          do i=1,nx
             n=nlat(i,j)
             amp(n,kq)=amp(n,kq)+q(i,j,l)*q(i,j,l)
          end do
          end do
         kb=max(l-1,1)
       do k=kb,l
          m=k-l+2  ! k-l = -1 or 0, so m = 1 or 2
          do j=1,ny
          do i=1,nx
            vorr(m,lq)=vorr(m,lq)+q(i,j,k)*q(i,j,l)
          enddo
          enddo
       enddo
      call horz_lenscale(nx,ny,q(:,:,l),ds,mapfac_x,mapfac_y,hl1,nlat,ncat)
      hl(1:ncat,kq)= hl(1:ncat,kq)+ hl1(1:ncat)
   enddo

!!! CSS Add Aerosol Stuff in here!
   if ( process_aero ) then
   
      do kk = 1,num_aeros
         do l=1,nz
            do j=1,ny       
               do i=1,nx
                  n=nlat(i,j)
                  amp_aero(kk,n,l)=amp_aero(kk,n,l)+aero(kk,i,j,l)*aero(kk,i,j,l)
               end do
            end do

            kb=max(l-1,1)
            do k=kb,l
               m=k-l+2  ! k-l = -1 or 0, so m = 1 or 2
               do j=1,ny
                  do i=1,nx
                     ! cov/var at level below l (m=1) or at l (m=2)
                     vorr_aero(kk,m,l)=vorr_aero(kk,m,l)+aero(kk,i,j,k)*aero(kk,i,j,l)
                  end do
               end do
            end do

            call horz_lenscale(nx,ny,aero(kk,:,:,l),ds,mapfac_x,mapfac_y,hl1,nlat,ncat)
            hl_aero(kk,1:ncat,l)= hl_aero(kk,1:ncat,l)+ hl1(1:ncat)
         end do 
      end do ! end loop over kk

   end if ! end if process_aero

!!!! CSS End Aerosol Stuff
   
  endif !loop<ncases
ENDDO   ! LOOP 

#ifdef DM_PARALLEL
      call mpi_allreduce(bms,bmst,ncat*(nz*6+3),mpi_rtype,mpi_sum, &
         mpi_comm_world,ierror)
      call mpi_allreduce(qcount,qcountt,ncat*nz,mpi_rtype,mpi_sum, &
         mpi_comm_world,ierror)
      call mpi_allreduce(amp,ampt,ncat*(nz*5+1),mpi_rtype,mpi_sum, &
         mpi_comm_world,ierror)
      call mpi_allreduce(vorr,vorrt,2*nz*4,mpi_rtype,mpi_sum, &
         mpi_comm_world,ierror)
      call mpi_allreduce(hl,hlt,ncat*(nz*4+1),mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)
#else
      bmst = bms
      qcountt = qcount
      ampt = amp
      vorrt = vorr
      hlt = hl
#endif
      hlt = hlt/float(ncases)

      if ( process_aero ) then

#ifdef DM_PARALLEL
         call mpi_allreduce(amp_aero,ampt_aero,num_aeros*ncat*nz,mpi_rtype,mpi_sum, &
            mpi_comm_world,ierror)
         call mpi_allreduce(vorr_aero,vorrt_aero,num_aeros*2*nz,mpi_rtype,mpi_sum, &
            mpi_comm_world,ierror)
         call mpi_allreduce(hl_aero,hlt_aero,num_aeros*ncat*nz,mpi_rtype,mpi_sum, &
            mpi_comm_world,ierror)
#else
         ampt_aero = amp_aero
         vorrt_aero = vorr_aero
         hlt_aero = hl_aero
#endif
         hlt_aero = hlt_aero/(float(ncases))
      end if

   do l= nz - less_levels_from_top +1 , nz
       kq2=nz*4+1+l
       kq=nz*3+1+l
       lq=nz*3+l
       ampt(1:ncat,kq)   = rsmall                       
       ampt(1:ncat,kq2)  = rsmall                       
       vorrt(:,lq)       = rsmall      
       hlt(1:ncat,kq)    = rsmall      
       qcountt(1:ncat,l) = 0.
   end do
! rizvi
! Structure of bmst array
! Levelwise covar of vp(full, bal, ubal) and t(full, bal, unbal)
!  bmst(ncat,   1  to nz   ) covar of full  vp
!  bmst(ncat, nz+1 to nz2  ) covar of bal   vp
!  bmst(ncat,nz2+1 to nz3  ) covar of unbal vp
!  bmst(ncat,nz3+1 to nz4  ) covar of full  t
!  bmst(ncat,nz4+1 to nz5  ) covar of bal   t
!  bmst(ncat,nz5+1 to nz6  ) covar of unbal t
!  bmst(ncat,nz6+1 to nz6+1) covar of full  p
!  bmst(ncat,nz6+2 to nz6+2) covar of bal   p
!  bmst(ncat,nz6+3 to nz6+3) covar of unbal p
! Structure of ampt array
!  ampt(ncat,      1 to      1) Horizontal covar unbal of ps
!  ampt(ncat,      2 to   nz+1) Horizontal covar       of sf
!  ampt(ncat,   nz+2 to 2*nz+1) Horizontal covar unbal of vp
!  ampt(ncat, 2*nz+2 to 3*nz+1) Horizontal covar unbal of t
!  ampt(ncat, 3*nz+2 to 4*nz+1) Horizontal covar       of q
!  vorrt(1:2,      1 :   nz )   covar with one level below and var of nz level psi
!  ampt(ncat, 4*nz+2 to 5*nz+1) Horizontal covar       of q2
! Structure of hlt array
!  hlt(ncat,    1 to     1)   Lengthscale for unbal ps
!  hlt(ncat,    2 to  nz+1)   Lengthscale for full  sf
!  hlt(ncat, nz+2 to nz2+1)   Lengthscale for unbal vp
!  hlt(ncat,nz2+2 to nz3+1)   Lengthscale for unbal t
!  hlt(ncat,nz3+2 to nz4+1)   Lengthscale for full  q
! Structure for vorrt
!   real, dimension(2,1:nz) :: vorr,vorrt
!  vorrt(1:2,      1 :   nz )   covar with one level below and var of nz level psi
!  vorrt(1:2,   nz+1 : 2*nz )   covar with one level below and var of nz level chi_u
!  vorrt(1:2, 2*nz+1 : 3*nz )   covar with one level below and var of nz level t_u
!  vorrt(1:2, 3*nz+1 : 4*nz )   covar with one level below and var of nz level q
! rizvi
!deallocate(t1,q,sf,vp)

  do k=1,nz*4
  vl(k)=1./sqrt(vorrt(2,k)) ! standard deviations at level k
!  do n=1,ncat
!  vl(n,k)=1./sqrt(vorrt(n,2,k))
!  enddo
  enddo

  if ( process_aero ) then
     do kk = 1,num_aeros
        do k = 1,nz
           vl_aero(kk,k) = 1./sqrt(vorrt_aero(kk,2,k))
        end do
     end do
  end if

!!!!!!!!!!!

  do l=1,4
  ll=(l-1)*nz
  do k=1,nz
  k1=ll+k
  do m=1,2
  km=max(k+m-2,1)
  k2=ll+km
! rizvi
! Divide by the sqrt(variance) to normalize
! rizvi

  vorrt(m,k+ll)=vorrt(m,ll+k)*vl(k1)*vl(k2)
!  do n=1,ncat
!  vorrt(n,m,k+ll)=vorrt(n,m,ll+k)*vl(n,k1)*vl(n,k2)
!  enddo
  enddo
  enddo
  enddo
! print *,vorrt(2,:)  ! CSS ... this will be 1 !
! CSS vorrt(1,:) is the CORRELATION between adjacent levels

  if ( process_aero ) then
     do kk = 1,num_aeros
        l = 1
        ll=(l-1)*nz
        do k=1,nz
           k1=ll+k   ! CSS  ... note that k+ll = k1
           do m=1,2
              km=max(k+m-2,1)
              k2=ll+km  ! CSS ... when m = 1, k2 = k1-1 ; when m = 2, k2 = k1
              vorrt_aero(kk,m,k+ll) = vorrt_aero(kk,m,k+ll)*vl_aero(kk,k1)*vl_aero(kk,k2) ! CSS...dimensionless, correlation
           end do
        end do
     end do
  end if

! Fix vert corr for psi, chi_u & t_u
  do l=1,3
  ll=(l-1)*nz
  do k=1,nz
  kp=k+1
  km=k
  if(k==1)km=2
  if(kp==nz+1)kp=nz-1
  km=km+ll
  kp=kp+ll
  vl(k+ll)=sqrt(1./(abs(2.-vorrt(1,km)-vorrt(1,kp) )))
  enddo
  enddo

! Now fix vert corr for q
  l = 4    
  ll=(l-1)*nz
  do k=1,nz-less_levels_from_top
  kp=k+1
  km=k
  if(k==1)km=2
  if(kp==nz-less_levels_from_top+1)kp=nz-less_levels_from_top-1
  km=km+ll
  kp=kp+ll
  if( abs(2.-vorrt(1,km)-vorrt(1,kp) ) > 0 ) then
  vl(k+ll)=sqrt(1./(abs(2.-vorrt(1,km)-vorrt(1,kp) )))
  else
  write(6,*)' Hit wrong vert moisture corr fixed it  0.01'
  write(6,*)' Level = ',k,' km/p= ',km,kp,' vorrt(1,km & kp) = ',vorrt(1,km),vorrt(1,kp)
  stop          
  end if
  enddo

! CSS fix vert corr for aero
  if ( process_aero ) then
     do kk = 1,num_aeros
        l=1
        ll=(l-1)*nz
        do k=1,nz 
           kp=k+1  
           km=k   
           if(k==1)km=2  
           if(kp==nz+1)kp=nz-1 
           km=km+ll  
           kp=kp+ll ! CSS kp = km + 1 ; k+ll = km+ll
           vl_aero(kk,k+ll)=sqrt(1./(abs(2.-vorrt_aero(kk,1,km)-vorrt_aero(kk,1,kp) )))
        end do
     end do
  end if
! end CSS fix vert corr for aero

!!!! normalize !!!!!!!!!!!
   rncas=1./float(ncases)
   do ll=1,ncat
  if(count(ll).ne.0.)then
 rnorm=rncas/count(ll)
 ampt(ll,1:nz*4+1)=ampt(ll,1:nz*4+1)*rnorm
  else
  write(0,*)'count=',ll,count(ll)
  endif
   enddo
   write(123,*)'normalized ampt'
   call statww(ampt,ncat)
   call statww(ampt(1,2),ncat*nz)
   call statww(ampt(1,nz+2),ncat*nz)
   call statww(ampt(1,nz*2+2),ncat*nz)
   call statww(ampt(1,nz*3+2),ncat*nz)

   if ( process_aero ) then
      do kk = 1,num_aeros
         do ll=1,ncat
            if(count(ll).ne.0.)then
               rnorm=rncas/count(ll)
               ampt_aero(kk,ll,1:nz) = ampt_aero(kk,ll,1:nz)*rnorm
            else
               write(0,*)'count=',ll,count(ll)
            end if
         end do
      end do
   end if

   do k=1,nz
     kq=nz*4+1+k
    do ll=1,ncat
     if(qcountt(ll,k) > 0 )then
     ampt(ll,kq)=ampt(ll,kq)/qcountt(ll,k)
     endif
    enddo
   enddo

  allocate(  hwll(0:ncat+1,nz,4),hwllp(0:ncat+1)       )
  allocate(  vz(nz,0:ncat+1),vt(nz,0:ncat+1)         )
  allocate(  vd(nz,0:ncat+1),vq(nz,0:ncat+1)         )
  allocate(  agv(0:ncat+1,nz,nz),wgv(0:ncat+1,nz)       )
  allocate(  bv(0:ncat+1,nz)                           )
  allocate(  corz(ncat,nz),cord(ncat,nz)         )
  allocate(  corh(ncat,nz),corq(ncat,nz)         )
  allocate(  corp(ncat),corq2(ncat,nz)                   )

  if ( process_aero ) allocate( hwll_aero(1:num_aeros,0:ncat+1,nz) )
  if ( process_aero ) allocate( v_aero(1:num_aeros,nz,0:ncat+1) )

  do j=1,ncat
  rlat4(j)=rlat(j)          
  enddo


  do k=1,nz
    do l=1,nz
      do i=0,ncat+1
       agv(i,l,k)=anew(l,k)
      enddo
    enddo
  enddo

 do k=1,nz
   j=0
   wgv(j,k)=wnew(1,k)
   bv(j,k)=bnew(1,k)
  do j=1,ncat
   wgv(j,k)=wnew(j,k)
   bv(j,k)=bnew(j,k)
  enddo
   j=ncat+1
   wgv(j,k)=wnew(ncat,k)
   bv(j,k)=bnew(ncat,k)
 enddo

   hwllp(0)=hlt(1,1)  ! for pressure
   hwllp(1:ncat)=hlt(1:ncat,1)
   hwllp(ncat+1)=hlt(ncat,1)
  do n=1,4
   do k=1,nz
    kn=(n-1)*nz+k+1
    hwll(0,k,n)=hlt(1,kn)
    hwll(1:ncat,k,n)=hlt(1:ncat,kn)
    hwll(ncat+1,k,n)=hlt(ncat,kn)
   enddo
  enddo

   if ( process_aero ) then
      do kk = 1,num_aeros
         do k = 1,nz
            hwll_aero(kk,0,k) = hlt_aero(kk,1,k)
            hwll_aero(kk,1:ncat,k) = hlt_aero(kk,1:ncat,k)
            hwll_aero(kk,ncat+1,k) = hlt_aero(kk,ncat,k)
         end do
      end do
   end if

   ampt=sqrt(ampt)
   if ( process_aero ) ampt_aero = sqrt(ampt_aero)  ! CSS ... standard deviation

   write(123,*)'sqrt ampt     '
   call statww(ampt,ncat)
   call statww(ampt(1,2),ncat*nz)
   call statww(ampt(1,nz+2),ncat*nz)
   call statww(ampt(1,nz*2+2),ncat*nz)
   call statww(ampt(1,nz*3+2),ncat*nz)

  do j=1,ncat
   corp(j)=ampt(j,1)
  enddo

   sigl4 = sigl
    do k=1,nz
     rsig(k)=log(sigl(k))
    enddo

   dsig(1)=rsig(1)-rsig(2)
   do k=2,nz-1
   dsig(k)=0.5*(rsig(k-1)-rsig(k+1))
   enddo
   dsig(nz)=rsig(nz-1)-rsig(nz)
   
   do k=1,nz  
    kv=k+1
    kd=k+1+nz
    kt=k+1+nz*2
    kq=k+1+nz*3
    kq2=k+1+nz*4
    kvd=k+nz
    kvt=k+nz*2
    kvq=k+nz*3
     vz(k,0:ncat+1)=1./vl(k)/dsig(k)
     vd(k,0:ncat+1)=1./vl(kvd)/dsig(k)
     vt(k,0:ncat+1)=1./vl(kvt)/dsig(k)
!i For moisture monitor negative scale-lengths
    do j=0,ncat+1
     if( vl(kvq) > 0. ) then
     vq(k,j)=1./vl(kvq)/dsig(k)
     else
     write(6,*)' For cat ',n,' at lev= ',k,' Hit very small moisture vl= ',vl(kvq)
     vq(k,j)=rbig    
     end if     
    enddo
!
    do j=1,ncat
       corz(j,k)=ampt(j,kv)   ! sf
       cord(j,k)=ampt(j,kd)   ! vp
       corh(j,k)=ampt(j,kt)   ! t
       corq(j,k)=ampt(j,kq)   ! q
       corq2(j,k)=ampt(j,kq2) ! q2
    enddo
   
    if ( process_aero ) then
       do kk = 1,num_aeros
          v_aero(kk,k,0:ncat+1) = 1./vl_aero(kk,k)/dsig(k)
          ampt_aero4(kk,1:ncat,k) = ampt_aero(kk,1:ncat,k)
       end do
    end if

   enddo

   do k=1,nz
    jb=ncat
    do j=ncat,1,-1
     if(corq2(j,k)>0.)then
     jb=j
     exit
     endif
    enddo
    do j=jb+1,ncat
     corq2(j,k)=corq2(jb,k)
    enddo
   enddo

  if(mype==0)then
    call statww4(corz,ncat*nz)
    call statww4(cord,ncat*nz)
    call statww4(corh,ncat*nz)
    call statww4(corq,ncat*nz)
    call statww4(corp,ncat)
    write(123,*)'q2'
    call statww4(corq2,ncat*nz)
    write(123,*)'hwll'
    call statww4(hwll,(ncat+2)*nz*4)
    call statww4(hwllp,(ncat+2))
    write(123,*)'vz4'
    call statww4(vz,(ncat+2)*nz)
    call statww4(vd,(ncat+2)*nz)
    call statww4(vt,(ncat+2)*nz)
    call statww4(vq,(ncat+2)*nz)
    write(123,*)'agv'
    call statww4(agv,(ncat+2)*nz*nz)
    call statww4(bv ,(ncat+2)*nz)
    call statww4(wgv,(ncat+2)*nz)

   stds(1,:,:) = corz  ! 'sf'
   stds(2,:,:) = cord  ! 'vp'
   stds(3,:,:) = corh  ! 't'
   stds(4,:,:) = corq  ! 'q'
   vzs(1,:,:) = vz  ! 'sf'
   vzs(2,:,:) = vd  ! 'vp'
   vzs(3,:,:) = vt  ! 't'
   vzs(4,:,:) = vq  ! 'q'

   if ( process_aero) then
      do jj = 1,num_aeros
         nn = len_trim(aeros_to_process(jj))    
         if ( nn.gt.len_vars_out .and. nn.lt.9) then
            vars(nvars+jj) = aeros_to_process(jj)(1:4)//aeros_to_process(jj)(6:6)
         else if ( nn.ge.9 ) then
            vars(nvars+jj) = aeros_to_process(jj)(1:3)//aeros_to_process(jj)(5:5)
         else
            vars(nvars+jj) = aeros_to_process(jj)
         end if
      end do
      vars_aeros(1:num_aeros) = vars(nvars+1:nvars+num_aeros)
      nsig(nvars+1:nvars+num_aeros) = nz
   end if


   do kk = 1,2 ! CSS ... once for big_endian(default) other time for little

   if ( kk.eq.1 ) then
      open(333,file='wrf-arw-gsi_be.gcv',form='unformatted') 
   else if ( kk.eq.2 ) then
      open(333,file='wrf-arw-gsi_be.gcv_little_endian',form='unformatted', &
             convert='little_endian') 
   end if

   rewind 333
   write(333)nz,ncat
   write(333)rlat4,sigl4
   write(333)agv,bv,wgv  ! anew,bnew,wnew arrays

   if ( process_aero ) then
      nvars_tot = nvars+num_aeros
   else
      nvars_tot = nvars
   end if

   do jj = 1, nvars_tot
      write(6,*) vars(jj)
      write(333)vars(jj),nsig(jj)

      if ( trim(vars(jj)).eq.'q' ) then
         write(333)corq,corq2
      else if ( trim(vars(jj)).eq.'ps' ) then
         write(333)corp
      else if ( any(vars_aeros.eq.trim(vars(jj)) ) ) then
         write(333) ampt_aero4(jj-nvars,:,:)
      else
         write(333)stds(jj,:,:) !corz,cord,corh,corp, ! needs to correspond with vars
      endif

      if ( nsig(jj) == 1 ) then
         write(333)hwllp  ! From the hlt array
      else if ( any(vars_aeros.eq.trim(vars(jj)) ) ) then
         write(333) hwll_aero(jj-nvars,:,:)
         write(333) v_aero(jj-nvars,:,:)
      else
         write(333)hwll(:,:,jj)  ! From the hlt array 
         write(333)vzs(jj,:,:)
      endif
   end do
   close(333)
   end do  ! kk loop for big_endian and little_endian

   deallocate(vars,nsig,stds,vzs)

!============================================================
! Write surface pressure bal & unbal contributions 
! total (over all latitudes) in terms of 
! % variance <pb,pb>/<p,p> & <p_u,p_u>/<p,p>
!============================================================
    open(unit=200,file='ps_u.ps.dat',form='formatted',&
            status='replace',action='write')
     a=0.
     b=0.
     c=0.
    do n=1,ncat
      a=a+bmst(n,nz6+1)
      b=b+bmst(n,nz6+2)
      c=c+bmst(n,nz6+3)
    enddo
    write(200,'(2f10.3)')(b/a),(c/a)
   close (200)
!============================================================
! Write levelwise chi & temp bal & unbal contributions 
! total (over all latitudes) in terms of 
! % variance <chi_b,chi_b>/<chi,chi> & <chi_u,chi_u>/<chi,chi>
! % variance <t_b,chi_b>/<chi,chi> & <chi_u,chi_u>/<chi,chi>
!============================================================
    open(unit=200,file='chi_u.chi.dat',form='formatted',&
            status='replace',action='write')
  do k=1,nz
     a=0.
     b=0.
     c=0.
    do n=1,ncat
      a=a+bmst(n,k)
      b=b+bmst(n,nz+k)
      c=c+bmst(n,nz2+k)
    enddo
    write(200,'(i3,2x,2f10.3)')k,(b/a),(c/a)
  enddo
  close (200)

    open(unit=200,file='t_u.t.dat',form='formatted',&
            status='replace',action='write')
  do k=1,nz
     a=0.
     b=0.
     c=0.
    do n=1,ncat
      a=a+bmst(n,nz3+k)
      b=b+bmst(n,nz4+k)
      c=c+bmst(n,nz5+k)
    enddo

    write(200,'(i3,2x,2f10.3)')k,(b/a),(c/a)
  enddo
  close (200)

  end if      ! end of write by mype = 0 

  deallocate(hwll,hwllp,vz,vt,vd,vq,agv,wgv,bv)
  if ( process_aero ) deallocate( hwll_aero, v_aero )
  return
  end subroutine compute_variance_and_len_scales

   subroutine statww(x,n) 
   implicit none

   integer, intent(in)   :: n 
   real,    intent(in)   :: x(1:n)
   real                  ::  xmax, xmin, xavg
   integer               :: i    
    xmax=-99999.
    xmin=99999.
    xavg=0.
    do i=1,n
    xavg=xavg+x(i)
    xmax=max(x(i),xmax)
    xmin=min(x(i),xmin)
    enddo
    xavg=xavg/float(n)
    write( 123,*)'max= ',xmax,'xmin= ',xmin,'avg= ',xavg
   return
   end subroutine statww 

   subroutine statww4(x,nx)
   implicit none
   real*4,dimension(nx):: x
   real*4 wmean,xmin,xmax
   integer i,imin,imax,nx
!
        xmax=-1e+9
        xmin=1e+9
        wmean=0.
        do i=1,nx
        wmean=wmean+x(i)
        if(x(i).gt.xmax)then
         xmax=x(i)
         imax=i
        else if (x(i).lt.xmin)then
        xmin=x(i)
         imin=i
           endif
          enddo
       wmean=wmean/float(nx)
      write(123,*)'max,min=',xmax,imax,xmin,imin,wmean
   return
   end subroutine statww4


   subroutine get_fixed_fields(filename,nx,ny,nz,mype,ncat,mapfac_x,mapfac_y,sigl,&
                      lat_bins_in_deg,xlat,xlon,min_xlat)
   implicit none

   character(len=*), intent(in)      :: filename       
   integer, intent(in)               :: nx, ny, nz, mype
   real, intent(in)                  :: lat_bins_in_deg                 
   real, intent(out)                 :: mapfac_x(1:nx,1:ny)
   real, intent(out)                 :: mapfac_y(1:nx,1:ny)
   real, intent(out)                 :: xlat(1:nx,1:ny)
   real, intent(out)                 :: xlon(1:nx,1:ny)
   real, intent(out)                 :: sigl(1:nz)
   real, intent(out)                 :: min_xlat     
   integer, intent(out)              :: ncat

   character (len=10)           :: date 
   integer                      :: i,j,n,nrec,dim1,dim2,dim3, ncases, nmax, nmin
   real                         :: ds, xmax, xmin

   open(unit=10,file=trim(filename),form='unformatted', &
             status='old', action='read')

   read(10, err=100,end=99)date, dim1, dim2, dim3, ncases, ds

   nrec = 1
   nmin=999
   nmax=-999
   xmin=999.
   xmax=-999.
   
   nrec = nrec + 1
!  Halflevel sigma field 
   read(10, err=100,end=99)sigl
   nrec = nrec + 1
! Lat
   read(10,err=100,end=99)xlat 
   nrec=nrec+1
! Lon
   read(10,err=100,end=99)xlon
   nrec=nrec+1
! mafac_x
   read(10,err=100,end=99)mapfac_x
   nrec=nrec+1
! mafac_y
   read(10,err=100,end=99)mapfac_y
   close (10)

! Compute Category
     do j=1,ny
        do i=1,nx
        xmax=max(xmax,xlat(i,j))
        xmin=min(xmin,xlat(i,j))
         n=int(xlat(i,j))
         nmin=min0(nmin,n)
         nmax=max0(nmax,n)
        end do
     end do
   min_xlat = xmin
   
   ncat  =(xmax-xmin)/lat_bins_in_deg + 1

   if(mype==0) then
    write(6,*)'For this domain min/max latitude = ',xmin,xmax
    write(6,*)' nmin,nmax = ',nmin,nmax,' ncat = ',ncat
   end if
   return
99  write(6,*)'Unexpected end of file in ',trim(filename),' in get_fixed_fileds record recs: ',nrec
   stop
100 write(6,*)'read error in ',trim(filename),' in get_fixed_fileds record recs: ',nrec
   stop
   end subroutine get_fixed_fields

   subroutine horz_lenscale(nx,ny,x,ds,mapfac_x,mapfac_y,tlength,nlat,ncat)
!---------------------------------------------------------------------
! Purpose: Computes horizontal scalelength of a 2D field
!
! Method : L=[8{Del**2(Var(field)/Del**2(Var(Del**2(field))}]**0.25  
!
! Reference: Wan-Shu et al, MWR 2002, Wan-Shu et al
!                           
! Author: Syed RH Rizvi,  NCAR/ESSL/MMM/DAG  08/06/2009
! Please acknowledge author/institute in work that uses this code.
!-----------------------------------------------------------------------

   implicit none
   integer, intent(in)       :: nx, ny   
   real,    intent(in)       :: x(nx,ny) 
   real,    intent(in)       :: ds
   real,    intent(in)       :: mapfac_x(nx,ny) 
   real,    intent(in)       :: mapfac_y(nx,ny) 
   real,    intent(out)      :: tlength(1:ncat)
   integer, intent(in)       :: nlat(nx,ny) 
   integer, intent(in)       :: ncat 

   real    :: lx(nx,ny) 
   real    :: xx(1:nx+2,1:ny+2) 
   real    :: corr(ncat),lcorr(ncat)
   real    :: dx, dy, sumx, sumlx
   integer :: i,j , n 

! Fill xx array
! Middle 
    xx(2:nx+1,2:ny+1)=x(1:nx,1:ny)
! Left Col 
    xx(1,2:ny+1)=x(1,1:ny)     
! Right Col
    xx(nx+2,2:ny+1)=x(nx,1:ny)     
! Bottom Row
    xx(2:nx+1,1) = x(1:nx,1) 
! Top Row
    xx(2:nx+1,ny+2) = x(1:nx,ny)
! Corners
    xx(1,1) = x(1,1) ; xx(nx+2,1) = x(nx,1) ; xx(nx+2,ny+2)=x(nx,ny) ; xx(1,ny+2)=x(1,ny)

! Now compute Laplacian of x  
    lx = 0

    do i=2,nx+1
    do j=2,ny+1
    dx=ds/mapfac_x(i-1,j-1)
    dx = dx* dx
    dy=ds/mapfac_y(i-1,j-1)
    dy = dy* dy
    lx(i-1,j-1) = ( (xx(i-1,j)+xx(i+1,j)-2.*xx(i,j))/dx + & 
                     (xx(i,j-1)+xx(i,j+1)-2.*xx(i,j))/dy   )  
    enddo
    enddo
! compute variance of field & its laplacian
    corr=0.
    lcorr=0.
    do j=1,ny
    do i=1,nx
    n=nlat(i,j)
    corr(n)=corr(n)+x(i,j)*x(i,j)
    lcorr(n)=lcorr(n)+lx(i,j)*lx(i,j)
    enddo
    enddo

    do n=1,ncat
    if( lcorr(n) > 0 ) tlength(n)=(8.*corr(n)/lcorr(n))**.25 
    enddo
!  
!
!! Average out      
!    sumx=0.   
!    sumlx=0.
!    do n=4,ncat-3
!    if( corr(n) > 0 .and. lcorr(n) > 0) then
!    sumx  = sumx  + corr(n)
!    sumlx = sumlx + lcorr(n)
!    else
!    end if
!    enddo
!    if( sumx == 0 .or. sumlx == 0.0 ) Then
!    tlength(1) = 1.
!    else
!    tlength(1) = (8.*sumx/sumlx)**.25 
!    end if
!    tlength(2:ncat)=tlength(1)

  end subroutine horz_lenscale  

subroutine get_basis(nz,rlenzsig_psi,zsig,bigxkm,vng,p8)

!   compute vertical function basis used to compute functional form
!        of t-psi and psfc-psi coupling constants
! input nx,ny,nz
! input rlenzsig_psi: vertical length of psi in zsig unit
! input zsig: R/G*log(sigl)
! output bigxkm: sum vng(i,k)*vng(i,m)
! output vng: basis function; normal decay btw 2 leyers
! output p8: sqrt(X(k,k)) 

  implicit none

#ifdef DM_PARALLEL
  include 'mpif.h'
#endif

  integer, intent(in) :: nz               
  real             :: arg8
  real             :: eps8
  real             :: bigxkm(1:nz,1:nz),vng(1:nz,1:nz)
  real             :: rlenzsig_psi(1:nz),zsig(1:nz),p8(1:nz)
  integer          :: i,ierror,k,kz,m,n
  real,allocatable :: biga8(:,:)
!
allocate(biga8(1:nz,1:nz) )
do n=1,nz     
 do i=1,nz
  arg8=.5_8*((zsig(i)-zsig(n))/rlenzsig_psi(n))**2
  vng(i,n)=0._8
  if(arg8.lt.75._8) then
   vng(i,n)=exp(-arg8)
  end if
 end do
end do

!  compute bigxkm

do m=1,nz
 do k=1,nz
  bigxkm(k,m)=0._8
  do kz=1,nz
   bigxkm(k,m)=bigxkm(k,m)+vng(kz,k)*vng(kz,m)
  end do
 end do
end do
   
eps8=.0001_8
do m=1,nz
 p8(m)=sqrt(bigxkm(m,m))
end do
do m=1,nz
 do k=1,nz
  biga8(k,m)=bigxkm(k,m)/(p8(k)*p8(m))
  if(k.eq.m) biga8(k,m)=eps8+biga8(k,m)
 end do
end do
do m=1,nz
 do k=1,nz
  bigxkm(k,m)=biga8(k,m)*p8(k)*p8(m)
 end do
end do
call choldc(bigxkm,nz,p8,ierror)

deallocate(biga8)

return
end subroutine get_basis

subroutine choldc(a,n,p,ierror)

!  form cholesky factors of symmetric matrix a

  integer, intent(in)   :: n
  real, intent(inout)   ::  a(n,n),p(n)
  integer, intent(out)  :: ierror

  integer              :: i,j,k
  real                 :: sum
!

  ierror=0
  do i=1,n
   do j=i,n
    sum=a(i,j)
    do k=i-1,1,-1
     sum=sum-a(i,k)*a(j,k)
    end do
    if(i.eq.j) then
     if(sum.le.0._8) then
      ierror=1
      return
     end if
     p(i)=sqrt(sum)
    else
     a(j,i)=sum/p(i)
    end if
   end do
  end do

return
end subroutine choldc

subroutine cholsl(a,n,p,b,x)

!   solve A*x = b, where a has been cholseky factored.

  implicit none              
  integer, intent(in)  :: n
  real, intent(inout)  ::  a(n,n),p(n),b(n),x(n)

  integer              :: i,j,k
  real                 :: sum
!
  do i=1,n
   sum=b(i)
   do k=i-1,1,-1
    sum=sum-a(i,k)*x(k)
   end do
   x(i)=sum/p(i)
  end do
  do i=n,1,-1
   sum=x(i)
   do k=i+1,n
    sum=sum-a(k,i)*x(k)
   end do
   x(i)=sum/p(i)
  end do

  return
  end subroutine cholsl

  subroutine cholsl_check(a,n,b,x)

!   check cholesky solution of Ax = b, by computing Ax and comparing to b

  implicit none              
  integer, intent(in)  :: n
  real, intent(inout)  ::  a(n,n),b(n),x(n)

  integer              :: j,k
  real                 :: sum, errormax, bmax

  errormax=0._8
  bmax=0._8
  do k=1,n
   sum=0.
   do j=1,n
    sum=sum+a(k,j)*x(j)
   end do
   errormax=max(abs(sum-b(k)),errormax)
   bmax=max(abs(b(k)),bmax)
  end do
  return
  end subroutine cholsl_check

  subroutine psi_tilde(nx,ny,nz,sf,func)
  implicit none

  integer, intent(in)  :: nx, ny, nz
  real, intent(inout)  :: sf(1:nx,1:ny,1:nz)
  real, intent(in)     :: func(1:nz,1:nz)

  real                 :: sft(1:nx,1:ny,1:nz)
  integer              :: i,j,k, n
!

   sft=0.
   do n=1,nz
    do k=1,nz
     do j=1,ny
      do i=1,nx
       sft(i,j,k)=sft(i,j,k)+func(n,k)*sf(i,j,n)
      end do
     end do
    end do
   end do
   sf=sft
  return
  end subroutine psi_tilde

   subroutine read_wrf_arw(filename,nx,ny,nz,mype,sf,vp,t,q,qm,p) 
   implicit none

   character(len=*), intent(in)      :: filename       
   integer, intent(in)      :: nx, ny, nz, mype

   real, intent(out)        :: sf(1:nx,1:ny,1:nz)
   real, intent(out)        :: vp(1:nx,1:ny,1:nz)
   real, intent(out)        :: t(1:nx,1:ny,1:nz)
   real, intent(out)        :: q(1:nx,1:ny,1:nz)
   real, intent(out)        :: qm(1:nx,1:ny,1:nz)
   real, intent(out)        :: p(1:nx,1:ny)

   real                     :: ds          
   character (len=10) :: date 
   integer            :: nrec

   integer            :: i,j,k
!
   nrec = 1
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

   close (24)

   return
99  write(6,*)'Unexpected end of file ',trim(filename),' in read_wrf_arw record recs: ',nrec
   stop
100 write(6,*)'read error in ',trim(filename),' in read_wrf_arw record recs: ',nrec
   stop
   end subroutine read_wrf_arw

    subroutine vert_corr(nx,ny,nz,ncases,ncat,npes,mype,&
              vng,bigxkm,p8,filen,nlat,count,sigl)

#ifdef DM_PARALLEL
   include 'mpif.h'

      integer ierror, mpi_rtype
      parameter(mpi_rtype=mpi_real8)
#endif

   integer, intent(in)       :: nx,ny,nz,ncases,ncat,npes,mype
   character(120),intent(in) :: filen(1:ncases)
   real, intent(in)          :: count(1:ncat)
   integer, intent(in)       :: nlat(1:nx,1:ny)

   real                      :: sf(1:nx,1:ny,1:nz),vp(1:nx,1:ny,1:nz)
   real                      :: vor(1:nx,1:ny,1:nz),q2(1:nx,1:ny,1:nz)
   real                      :: t1(1:nx,1:ny,1:nz),q(1:nx,1:ny,1:nz)
   real                      :: t(1:nx,1:ny),p(1:nx,1:ny),rpb(1:nx,1:ny)
   real                      :: mapfac_x(1:nx,1:ny),mapfac_y(1:nx,1:ny)
   real                      :: bigxkm(1:nz,1:nz),vng(1:nz,1:nz)
   real                      :: tlfl(1:nz,1:nz),tlflt(1:nz,1:nz)
   real                      :: fkfl(1:nz,1:nz,1:ncat)
   real                      :: sigl(1:nz),zsig(1:nz),p8(1:nz)        
   real                      :: pdfl(1:nz),rlen(1:nz)        
   real                      :: mean_field, inv_nxny

   real                      :: w(nz,nz)

   inv_nxny= 1.0/real(nx*ny)

   tlfl=0.
   fkfl=0.

   npeloop=ncases/npes
   if(npes*npeloop .lt. ncases) npeloop=npeloop+1

   do moop=1,npeloop
    loop=npes*(moop-1)+mype+1
     if (loop.le.ncases)then

      call read_wrf_arw(trim(filen(loop)),nx,ny,nz, mype, sf,vp,t1,q,q2,p)

    ! Remove mean field:
    do k = 1, nz
      mean_field = sum(sf(1:nx,1:ny,k)) * inv_nxny
      sf(1:nx,1:ny,k) = sf(1:nx,1:ny,k) - mean_field
    end do


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     do l=1,nz
      do k=1,nz
         do j=1,ny
         do i=1,nx
          tlfl(l,k)=tlfl(l,k)+sf(i,j,k)*sf(i,j,l)
         enddo
         enddo
      enddo
     enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     endif !loop<ncases
   end do! Loop over moop

#ifdef DM_PARALLEL
      call mpi_allreduce(tlfl,tlflt,nz*nz,mpi_rtype,mpi_sum, &
      mpi_comm_world,ierror)
#else
      tlflt = tlfl
#endif

   do l=1,nz
    pdfl(l)=1./sqrt(tlflt(l,l))
   enddo


    do k=1,nz
     do l=1,nz
      tlflt(l,k)=tlflt(l,k)*pdfl(k)*pdfl(l)
     enddo
    enddo

   do l=1,nz
    lp=l+1
    lm=l-1
     if(lm==0)lm=2
     if(lp==nz+1)lp=nz-1
      rlen(l)=sqrt( 1./( abs(2.-tlflt(lm,l)-tlflt(lp,l)) ))
   enddo

!!!!!!convert from grid units to zsig units !!!!!!!!!!!

   do k=1,nz
   zsig(k)=-log(sigl(k))
   enddo


   do l=1,nz
     lp=min(nz,l+1)
     lm=max(1,l-1) 
     rlen(l)=rlen(l)*(zsig(lp)-zsig(lm))/float(lp-lm)
   enddo

   call get_basis(nz,rlen,zsig,bigxkm,vng,p8)

  return
  end subroutine vert_corr

