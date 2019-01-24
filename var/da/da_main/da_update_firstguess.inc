subroutine da_update_firstguess(grid, out_filename) 

!---------------------------------------------------------------------------
!  Purpose: Only replace the fields touched by WRFDA, rather than re-generate 
!           whole wrfvar_output from the scratch.
!
!  Update  :   jliu@ucar.edu Apr 1, 2013
!              Minor change for Purpose
!              Added copy file mods instead of copying file outside 
!              Requires Fortran 2003 standard compiler
!  Creator :   Junmei Ban, Mar 14, 2013
!         
!  The following WRF fields are modified:
!    grid%u_2
!    grid%v_2
!    grid%w_2
!    grid%mu_2
!    grid%ph_2
!    grid%t_2
!    grid%moist
!    grid%p
!    grid%psfc
!    grid%t2, grid%q2, grid%u10, grid%v10, grid%th2
!
!---------------------------------------------------------------------------

  use module_domain, only : get_ijk_from_grid, program_name
  use da_control,    only : use_radarobs, use_rad, crtm_cloud, &
                            use_radar_rhv, use_radar_rqv    
  use module_state_description, only : p_qv, p_qc, p_qr, p_qi, &
                                       p_qs, p_qg, &
                                       f_qc, f_qr, f_qi, f_qs, f_qg

  implicit none

  INTERFACE
    integer(c_int32_t) function copyfile(ifile, ofile) bind(c)
      import :: c_int32_t, C_CHAR
      CHARACTER(KIND=C_CHAR), DIMENSION(*), intent(in) :: ifile, ofile
    END function copyfile
  END INTERFACE

  include 'netcdf.inc'

  type(domain), intent(in)             :: grid
  character(*), intent(in), optional   :: out_filename

! Declare local parameters
  character(len=120) :: file_name 
  character(len=19)  :: DateStr1
  character(len=4)   :: staggering=' N/A'
  character(len=3)   :: ordering  
  character(len=80), dimension(3)  ::  dimnames
  character(len=31)  :: rmse_var  
  integer            :: dh1
  integer            :: i,j,k
  integer            :: ndim1
  integer            :: WrfType
  integer            :: it, ierr, Status, Status_next_time
  integer            :: wrf_real
  integer            :: nlon_regional,nlat_regional,nsig_regional
  integer            :: ids, ide, jds, jde, kds, kde,    &
                        ims, ime, jms, jme, kms, kme,    &
                        ips, ipe, jps, jpe, kps, kpe    
  integer, dimension(4)           :: start_index, end_index1
  real, dimension(:), allocatable :: globbuf  
  real*4,allocatable :: field3(:,:,:),field2(:,:)
  real*4,allocatable :: field3u(:,:,:),field3v(:,:,:),field3ph(:,:,:)
  character(len=4) ::  fgname
  integer :: julyr, julday
  real    :: gmt
  real*4  :: gmt4

  wrf_real=104
  end_index1=0

  call get_ijk_from_grid (  grid ,                         &
                          ids, ide, jds, jde, kds, kde,    &
                          ims, ime, jms, jme, kms, kme,    &
                          ips, ipe, jps, jpe, kps, kpe    )

!
! update wrfvar_output file with analysis variables from 3dvar
!
  if (present(out_filename)) then
     file_name = trim(out_filename)
     if (file_name == 'ana02') then
        fgname = 'fg02'
     else
        fgname = 'fg'
     endif
  else
     file_name = 'wrfvar_output'
     fgname = 'fg'
  endif

  if (rootproc) then
     ierr = copyfile(trim(fgname)//C_NULL_CHAR, trim(file_name)//C_NULL_CHAR)
     if ( ierr /= 0 ) then
        write(unit=message(1),fmt='(a)') "Failed to create "//trim(file_name)//" from "//trim(fgname)
        call da_error(__FILE__,__LINE__,message(1:1))
     endif

     call ext_ncd_open_for_update( trim(file_name), 0, 0, "", dh1, Status)
     if ( Status /= 0 )then
        write(unit=message(1),fmt='(a)') "Failed to open "//trim(file_name)
        call da_error(__FILE__,__LINE__,message(1:1))
     endif

!-------------  get date info

     call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
     if ( var4d .or. num_fgat_time == 1 )  then ! Don't do it for FGAT
        if ( DateStr1 /= start_date )then
           ! impossible scenario
           ! start_date is set to be equal to file date in da_med_initialdata_input.inc
           write(unit=message(1),fmt='(a)') 'date info mismatch '//trim(DateStr1)//" != "//trim(start_date)
           call da_error(__FILE__,__LINE__,message(1:1))
        endif
     endif

     ! update analysis time info in global attributes
     ! needs to be done before the ext_ncd_write_field calls
     if ( var4d .or. num_fgat_time == 1 ) then  ! For 4dvar or 3dvar
        call get_julgmt(start_date, julyr, julday, gmt)
        CALL ext_ncd_put_dom_ti_char (dh1, 'TITLE', ' OUTPUT FROM '//trim(program_name), ierr)
        CALL ext_ncd_put_dom_ti_char (dh1, 'START_DATE', trim(start_date), ierr)
        CALL ext_ncd_put_dom_ti_char (dh1, 'SIMULATION_START_DATE', trim(start_date), ierr)
     else                                  ! For FGAT
        call get_julgmt(DateStr1//'     ', julyr, julday, gmt)
        CALL ext_ncd_put_dom_ti_char (dh1, 'TITLE', ' OUTPUT FROM '//trim(program_name), ierr)
        CALL ext_ncd_put_dom_ti_char (dh1, 'START_DATE', trim(DateStr1), ierr)
        CALL ext_ncd_put_dom_ti_char (dh1, 'SIMULATION_START_DATE', trim(DateStr1), ierr)
     endif
     gmt4 = real(gmt)
     CALL ext_ncd_put_dom_ti_real    (dh1, 'GMT',    gmt4,   1, ierr)
     CALL ext_ncd_put_dom_ti_integer (dh1, 'JULYR',  julyr,  1, ierr)
     CALL ext_ncd_put_dom_ti_integer (dh1, 'JULDAY', julday, 1, ierr)

!-------------  get grid info
     rmse_var='T'
     call ext_ncd_get_var_info (dh1,rmse_var,ndim1,ordering,staggering, &
                               start_index,end_index1, WrfType, ierr    )
     nlon_regional=end_index1(1)
     nlat_regional=end_index1(2)
     nsig_regional=end_index1(3)

!    write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
     allocate(field2(nlon_regional,nlat_regional),field3(nlon_regional,nlat_regional,nsig_regional))
     allocate(field3u(nlon_regional+1,nlat_regional,nsig_regional))
     allocate(field3v(nlon_regional,nlat_regional+1,nsig_regional))
     allocate(field3ph(nlon_regional,nlat_regional,nsig_regional+1))

  end if ! end of rootproc 

!
! update MU
! 
#ifdef DM_PARALLEL
  if (rootproc) then
     ALLOCATE(globbuf((ide-1-ids+3)*3*(jde-1-jds+3)))    
  else
     ALLOCATE(globbuf(1))
  end if
  globbuf=0.0
  call wrf_patch_to_global_double(grid%mu_2,globbuf,1,' ','xy', &
                                  ids, ide-1,          jds, jde-1,         1, 1,  &
                                  ims, ime,            jms, jme,           1, 1,  &
                                  ips, min(ipe,ide-1), jps, min(jpe,jde-1),1, 1)
#endif  
  if (rootproc) then
     do j= 1,nlat_regional 
        do i= 1,nlon_regional
#ifdef DM_PARALLEL
           field2(i,j)=globbuf(i+(j-1)*(nlon_regional+1))     
#else
           field2(i,j)=grid%mu_2(i,j)
#endif
        end do 
     end do     
     rmse_var='MU'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index1, WrfType, ierr    )
!    write(6,*)' rmse_var=',trim(rmse_var)
!    write(6,*)' ordering=',ordering
!    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!    write(6,*)' ndim1=',ndim1
!    write(6,*)' staggering=',staggering
!    write(6,*)' start_index=',start_index
!    write(6,*)' end_index1=',end_index1
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),       &
                             field2,WRF_REAL,0,0,0,ordering,   &
                             staggering, dimnames ,              &
                             start_index,end_index1,             & !dom
                             start_index,end_index1,             & !mem
                             start_index,end_index1,             & !pat
                             ierr                               )
  end if  

#ifdef DM_PARALLEL
  deallocate(globbuf) 
#endif

!
! update PSFC 
!
#ifdef DM_PARALLEL
  if (rootproc) then
     ALLOCATE(globbuf((ide-1-ids+3)*3*(jde-1-jds+3)))    ! global_mu_2(ids:ide-1,jds:jde-1) )
  else
     ALLOCATE(globbuf(1))
  end if
  globbuf=0.0
  call wrf_patch_to_global_double(grid%psfc,globbuf,1,' ','xy', &
                                  ids, ide-1,          jds, jde-1,         1, 1,  &
                                  ims, ime,            jms, jme,           1, 1,  &
                                  ips, min(ipe,ide-1), jps, min(jpe,jde-1),1, 1)
#endif  
  if (rootproc) then
     do j= 1,nlat_regional
        do i= 1,nlon_regional
#ifdef DM_PARALLEL
           field2(i,j)=globbuf(i+(j-1)*(nlon_regional+1)) 
#else
           field2(i,j)=grid%psfc(i,j)
#endif
        end do
     end do
     rmse_var='PSFC'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
                               start_index,end_index1, WrfType, ierr    )
!    write(6,*)' rmse_var=',trim(rmse_var)
!    write(6,*)' ordering=',ordering
!    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!    write(6,*)' ndim1=',ndim1
!    write(6,*)' staggering=',staggering
!    write(6,*)' start_index=',start_index
!    write(6,*)' end_index1=',end_index1
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),        &
                             field2,WRF_REAL,0,0,0,ordering,    &
                             staggering, dimnames ,               &
                             start_index,end_index1,              & !dom
                             start_index,end_index1,              & !mem
                             start_index,end_index1,              & !pat
                             ierr                   )
  end if

#ifdef DM_PARALLEL
  deallocate(globbuf) 
#endif

!
! update T2 
!
#ifdef DM_PARALLEL
  if (rootproc) then
     ALLOCATE(globbuf((ide-1-ids+3)*3*(jde-1-jds+3)))    
  else
     ALLOCATE(globbuf(1))
  end if
  globbuf=0.0
  call wrf_patch_to_global_double(grid%t2,globbuf,1,' ','xy', &
                                  ids, ide-1,          jds, jde-1,         1, 1,  &
                                  ims, ime,            jms, jme,           1, 1,  &
                                  ips, min(ipe,ide-1), jps, min(jpe,jde-1),1, 1)
#endif
  if (rootproc) then
     do j= 1,nlat_regional
        do i= 1,nlon_regional
#ifdef DM_PARALLEL
           field2(i,j)=globbuf(i+(j-1)*(nlon_regional+1))
#else
           field2(i,j)=grid%t2(i,j)
#endif
        end do
     end do
     rmse_var='T2'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index1, WrfType, ierr    )
!    write(6,*)' rmse_var=',trim(rmse_var)
!    write(6,*)' ordering=',ordering
!    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!    write(6,*)' ndim1=',ndim1
!    write(6,*)' staggering=',staggering
!    write(6,*)' start_index=',start_index
!    write(6,*)' end_index1=',end_index1
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),         &
                             field2,WRF_REAL,0,0,0,ordering,     &
                             staggering, dimnames ,                &
                             start_index,end_index1,               & !dom
                             start_index,end_index1,               & !mem
                             start_index,end_index1,               & !pat
                             ierr                                 )
  end if

#ifdef DM_PARALLEL
  deallocate(globbuf) 
#endif
  
!
! update TH2
!
#ifdef DM_PARALLEL
  if (rootproc) then
     ALLOCATE(globbuf((ide-1-ids+3)*3*(jde-1-jds+3)))    
  else
     ALLOCATE(globbuf(1))
  end if
  globbuf=0.0
  call wrf_patch_to_global_double(grid%th2,globbuf,1,' ','xy', &
                                  ids, ide-1,          jds, jde-1,         1, 1,  &
                                  ims, ime,            jms, jme,           1, 1,  &
                                  ips, min(ipe,ide-1), jps, min(jpe,jde-1),1, 1)
#endif  
  if (rootproc) then
     do j= 1,nlat_regional
        do i= 1,nlon_regional
#ifdef DM_PARALLEL
           field2(i,j)=globbuf(i+(j-1)*(nlon_regional+1)) 
#else
           field2(i,j)=grid%th2(i,j)
#endif
        end do
     end do
     rmse_var='TH2'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index1, WrfType, ierr    )
!    write(6,*)' rmse_var=',trim(rmse_var)
!    write(6,*)' ordering=',ordering
!    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!    write(6,*)' ndim1=',ndim1
!    write(6,*)' staggering=',staggering
!    write(6,*)' start_index=',start_index
!    write(6,*)' end_index1=',end_index1
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),         &
                             field2,WRF_REAL,0,0,0,ordering,     &
                             staggering, dimnames ,                &
                             start_index,end_index1,               & !dom
                             start_index,end_index1,               & !mem
                             start_index,end_index1,               & !pat
                             ierr                                 )
  end if

#ifdef DM_PARALLEL
  deallocate(globbuf) 
#endif

!
! update Q2
!
#ifdef DM_PARALLEL
  if (rootproc) then
     ALLOCATE(globbuf((ide-1-ids+3)*3*(jde-1-jds+3)))   
  else
     ALLOCATE(globbuf(1))
  end if
  globbuf=0.0
  call wrf_patch_to_global_double(grid%q2,globbuf,1,' ','xy', &
                                  ids, ide-1,          jds, jde-1,         1, 1,  &
                                  ims, ime,            jms, jme,           1, 1,  &
                                  ips, min(ipe,ide-1), jps, min(jpe,jde-1),1, 1)
#endif  
  if (rootproc) then
     do j= 1,nlat_regional
        do i= 1,nlon_regional
#ifdef DM_PARALLEL
           field2(i,j)=globbuf(i+(j-1)*(nlon_regional+1))   
#else
           field2(i,j)=grid%q2(i,j) 
#endif
        end do
     end do
     rmse_var='Q2'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index1, WrfType, ierr    )
!    write(6,*)' rmse_var=',trim(rmse_var)
!    write(6,*)' ordering=',ordering
!    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!    write(6,*)' ndim1=',ndim1
!    write(6,*)' staggering=',staggering
!    write(6,*)' start_index=',start_index
!    write(6,*)' end_index1=',end_index1
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),        &
                             field2,WRF_REAL,0,0,0,ordering,    &
                             staggering, dimnames ,               &
                             start_index,end_index1,              & !dom
                             start_index,end_index1,              & !mem
                             start_index,end_index1,              & !pat
                             ierr                                 )
  end if

#ifdef DM_PARALLEL
  deallocate(globbuf) 
#endif

!
! update U10
!
#ifdef DM_PARALLEL
  if (rootproc) then
     ALLOCATE(globbuf((ide-1-ids+3)*3*(jde-1-jds+3)))    ! global_mu_2(ids:ide-1,jds:jde-1) )
  else
     ALLOCATE(globbuf(1))
  end if
  globbuf=0.0
  call wrf_patch_to_global_double(grid%u10,globbuf,1,' ','xy', &
                                  ids, ide-1,          jds, jde-1,         1, 1,  &
                                  ims, ime,            jms, jme,           1, 1,  &
                                  ips, min(ipe,ide-1), jps, min(jpe,jde-1),1, 1)
#endif  
  if (rootproc) then
     do j= 1,nlat_regional
        do i= 1,nlon_regional
#ifdef DM_PARALLEL
           field2(i,j)=globbuf(i+(j-1)*(nlon_regional+1))   
#else
           field2(i,j)=grid%u10(i,j)
#endif 
        end do
     end do
     rmse_var='U10'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index1, WrfType, ierr    )
!    write(6,*)' rmse_var=',trim(rmse_var)
!    write(6,*)' ordering=',ordering
!    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!    write(6,*)' ndim1=',ndim1
!    write(6,*)' staggering=',staggering
!    write(6,*)' start_index=',start_index
!    write(6,*)' end_index1=',end_index1
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),         &
                             field2,WRF_REAL,0,0,0,ordering,     &
                             staggering, dimnames ,                &
                             start_index,end_index1,               & !dom
                             start_index,end_index1,               & !mem
                             start_index,end_index1,               & !pat
                             ierr                                 )
  end if

#ifdef DM_PARALLEL
  deallocate(globbuf) 
#endif
  
!
! update V10
!
#ifdef DM_PARALLEL
  if (rootproc) then
     ALLOCATE(globbuf((ide-1-ids+3)*3*(jde-1-jds+3)))   
  else
     ALLOCATE(globbuf(1))
  end if
  globbuf=0.0
  call wrf_patch_to_global_double(grid%v10,globbuf,1,' ','xy', &
                                  ids, ide-1,          jds, jde-1,         1, 1,  &
                                  ims, ime,            jms, jme,           1, 1,  &
                                  ips, min(ipe,ide-1), jps, min(jpe,jde-1),1, 1)
#endif  
  if (rootproc) then
     do j= 1,nlat_regional
        do i= 1,nlon_regional
#ifdef DM_PARALLEL	
           field2(i,j)=globbuf(i+(j-1)*(nlon_regional+1))   
#else
           field2(i,j)=grid%v10(i,j)
#endif 	   
        end do
     end do
     rmse_var='V10'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index1, WrfType, ierr    )
!    write(6,*)' rmse_var=',trim(rmse_var)
!    write(6,*)' ordering=',ordering
!    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!    write(6,*)' ndim1=',ndim1
!    write(6,*)' staggering=',staggering
!    write(6,*)' start_index=',start_index
!    write(6,*)' end_index1=',end_index1
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),         &
                             field2,WRF_REAL,0,0,0,ordering,     &
                             staggering, dimnames ,                &
                             start_index,end_index1,               & !dom
                             start_index,end_index1,               & !mem
                             start_index,end_index1,               & !pat
                             ierr                                 )
  end if
  
#ifdef DM_PARALLEL
  deallocate(globbuf) 
#endif

!
!  update P
!
#ifdef DM_PARALLEL
  if (rootproc) then
    allocate( globbuf((ide-1-ids+3)*(jde-1-jds+3)*(kde-1-kds+3)))
  else
    allocate( globbuf( 1 ) )
  endif
  globbuf=0.0

  CALL wrf_patch_to_global_double ( grid%p, globbuf, 1, '', 'xyz' ,             &
                                    ids, ide-1,          jds, jde-1,          kds, kde-1, &
                                    ims, ime,            jms, jme,            kms, kme,   &
                                    ips, min(ipe,ide-1), jps, min(jpe,jde-1), kps, kpe-1  )
#endif
  if (rootproc) then 
     do k= 1,nsig_regional 
        do j= 1,nlat_regional 
           do i= 1,nlon_regional
#ifdef DM_PARALLEL
              field3(i,j,k)=globbuf(i+(j-1)*(nlon_regional+1)+(k-1)*(nlon_regional+1)*(nlat_regional+1))
#else
              field3(i,j,k)=grid%p(i,j,k)
#endif
           end do 
        end do
     end do 
     rmse_var='P'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index1, WrfType, ierr    )
!    write(6,*)' rmse_var=',trim(rmse_var)
!    write(6,*)' ordering=',ordering
!    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!    write(6,*)' ndim1=',ndim1
!    write(6,*)' staggering=',staggering
!    write(6,*)' start_index=',start_index
!    write(6,*)' end_index1=',end_index1
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),         &
                             field3,WRF_REAL,0,0,0,ordering,     &
                             staggering, dimnames ,                &
                             start_index,end_index1,               & !dom
                             start_index,end_index1,               & !mem
                             start_index,end_index1,               & !pat
                             ierr                                 )
  end if  ! end of rootproc
  
#ifdef DM_PARALLEL
  deallocate(globbuf) 
#endif

!
!  update T
!
#ifdef DM_PARALLEL
  if (rootproc) then
    allocate( globbuf((ide-1-ids+3)*(jde-1-jds+3)*(kde-1-kds+3)))
  else
    allocate( globbuf( 1 ) )
  endif
  globbuf=0.0
  CALL wrf_patch_to_global_double ( grid%t_2, globbuf, 1, '', 'xyz' ,                       &
                                    ids, ide-1,          jds, jde-1,          kds, kde-1, &
                                    ims, ime,            jms, jme,            kms, kme,   &
                                    ips, min(ipe,ide-1), jps, min(jpe,jde-1), kps, kpe-1  )
#endif
  if (rootproc) then 
     do k= 1,nsig_regional 
        do j= 1,nlat_regional 
           do i= 1,nlon_regional
#ifdef DM_PARALLEL	   
              field3(i,j,k)=globbuf(i+(j-1)*(nlon_regional+1)+(k-1)*(nlon_regional+1)*(nlat_regional+1))
#else
              field3(i,j,k)=grid%t_2(i,j,k)
#endif
           end do 
        end do
     end do 
     rmse_var='T'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index1, WrfType, ierr    )
!    write(6,*)' rmse_var=',trim(rmse_var)
!    write(6,*)' ordering=',ordering
!    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!    write(6,*)' ndim1=',ndim1
!    write(6,*)' staggering=',staggering
!    write(6,*)' start_index=',start_index
!    write(6,*)' end_index1=',end_index1
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),         &
                             field3,WRF_REAL,0,0,0,ordering,       &
                             staggering, dimnames ,                &
                             start_index,end_index1,               & !dom
                             start_index,end_index1,               & !mem
                             start_index,end_index1,               & !pat
                             ierr                                 )

! Update moist potential temperature: for now, it is identical to T with use_theta_m=0.
     rmse_var='THM'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index1, WrfType, ierr    )
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),         &
                             field3,WRF_REAL,0,0,0,ordering,       &
                             staggering, dimnames ,                &
                             start_index,end_index1,               & !dom
                             start_index,end_index1,               & !mem
                             start_index,end_index1,               & !pat
                             ierr                                 )

  end if  
  
#ifdef DM_PARALLEL
  deallocate(globbuf) 
#endif

!
!  update QVAPOR
!
#ifdef DM_PARALLEL
  if (rootproc) then
    allocate( globbuf((ide-1-ids+3)*(jde-1-jds+3)*(kde-1-kds+3)))
  else
    allocate( globbuf( 1 ) )
  endif
  globbuf=0.0
  CALL wrf_patch_to_global_double ( grid%moist(:,:,:,p_qv), globbuf, 1, '', 'xyz' ,         &
                                    ids, ide-1,          jds, jde-1,          kds, kde-1, &
                                    ims, ime,            jms, jme,            kms, kme,   &
                                    ips, min(ipe,ide-1), jps, min(jpe,jde-1), kps, kpe-1  )
#endif
  if (rootproc) then 
     do k= 1,nsig_regional 
        do j= 1,nlat_regional 
           do i= 1,nlon_regional
#ifdef DM_PARALLEL	   
              field3(i,j,k)=globbuf(i+(j-1)*(nlon_regional+1)+(k-1)*(nlon_regional+1)*(nlat_regional+1))
#else
              field3(i,j,k)=grid%moist(i,j,k,p_qv)
#endif
           end do 
        end do
     end do 
     rmse_var='QVAPOR'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index1, WrfType, ierr    )
!    write(6,*)' rmse_var=',trim(rmse_var)
!    write(6,*)' ordering=',ordering
!    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!    write(6,*)' ndim1=',ndim1
!    write(6,*)' staggering=',staggering
!    write(6,*)' start_index=',start_index
!    write(6,*)' end_index1=',end_index1
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),         &
                             field3,WRF_REAL,0,0,0,ordering,       &
                             staggering, dimnames ,                &
                             start_index,end_index1,               & !dom
                             start_index,end_index1,               & !mem
                             start_index,end_index1,               & !pat
                             ierr                                 )
  end if  ! end of rootproc

#ifdef DM_PARALLEL
  deallocate(globbuf) 
#endif

!
!  update PH
!
#ifdef DM_PARALLEL
  if (rootproc) then
    allocate( globbuf((ide-1-ids+3)*(jde-1-jds+3)*(kde-1-kds+3)))
  else
    allocate( globbuf( 1 ) )
  endif
  globbuf=0.0
  CALL wrf_patch_to_global_double ( grid%ph_2, globbuf, 1, 'Z', 'xyz' ,                   &
                                    ids, ide-1,          jds, jde-1,          kds, kde, &
                                    ims, ime,            jms, jme,            kms, kme, &
                                    ips, min(ipe,ide-1), jps, min(jpe,jde-1), kps, kpe  )
#endif
  if (rootproc) then 
     do k= 1,nsig_regional+1 
        do j= 1,nlat_regional 
           do i= 1,nlon_regional
#ifdef DM_PARALLEL	   
              field3ph(i,j,k)=globbuf(i+(j-1)*(nlon_regional+1)+(k-1)*(nlon_regional+1)*(nlat_regional+1))
#else
              field3ph(i,j,k)=grid%ph_2(i,j,k)
#endif
           end do 
        end do
     end do 
     rmse_var='PH'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index1, WrfType, ierr    )
!    write(6,*)' rmse_var=',trim(rmse_var)
!    write(6,*)' ordering=',ordering
!    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!    write(6,*)' ndim1=',ndim1
!    write(6,*)' staggering=',staggering
!    write(6,*)' start_index=',start_index
!    write(6,*)' end_index1=',end_index1
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),         &
                             field3ph,WRF_REAL,0,0,0,ordering,     &
                             staggering, dimnames ,                &
                             start_index,end_index1,               & !dom
                             start_index,end_index1,               & !mem
                             start_index,end_index1,               & !pat
                             ierr                                 )
  end if  ! end of rootproc

#ifdef DM_PARALLEL
  deallocate(globbuf) 
#endif

!
!  update U
!
#ifdef DM_PARALLEL
  if (rootproc) then
    allocate( globbuf((ide-1-ids+3)*(jde-1-jds+3)*(kde-1-kds+3)))
  else
    allocate( globbuf( 1 ) )
  endif
  globbuf=0.0
  CALL wrf_patch_to_global_double ( grid%u_2, globbuf, 1, 'X', 'xyz' ,                    &
                                    ids, ide,          jds, jde-1,          kds, kde-1, &
                                    ims, ime,          jms, jme,            kms, kme,   &
                                    ips, min(ipe,ide), jps, min(jpe,jde-1), kps, kpe-1  )
#endif
  if (rootproc) then 
     do k= 1,nsig_regional 
        do j= 1,nlat_regional 
           do i= 1,nlon_regional+1
#ifdef DM_PARALLEL	   
              field3u(i,j,k)=globbuf(i+(j-1)*(nlon_regional+1)+(k-1)*(nlon_regional+1)*(nlat_regional+1))
#else
              field3u(i,j,k)=grid%u_2(i,j,k)
#endif
           end do 
        end do
     end do 
     rmse_var='U'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index1, WrfType, ierr    )
!    write(6,*)' rmse_var=',trim(rmse_var)
!    write(6,*)' ordering=',ordering
!    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!    write(6,*)' ndim1=',ndim1
!    write(6,*)' staggering=',staggering
!    write(6,*)' start_index=',start_index
!    write(6,*)' end_index1=',end_index1
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),         &
                             field3u,WRF_REAL,0,0,0,ordering,      &
                             staggering, dimnames ,                &
                             start_index,end_index1,               & !dom
                             start_index,end_index1,               & !mem
                             start_index,end_index1,               & !pat
                             ierr                                 )
  end if  ! end of rootproc

#ifdef DM_PARALLEL
  deallocate(globbuf) 
#endif

!
!  update V
!
#ifdef DM_PARALLEL
  if (rootproc) then
    allocate( globbuf((ide-1-ids+3)*(jde-1-jds+3)*(kde-1-kds+3)))
  else
    allocate( globbuf( 1 ) )
  endif
  globbuf=0.0
  CALL wrf_patch_to_global_double ( grid%v_2, globbuf, 1, 'Y', 'xyz' ,                    &
                                    ids, ide-1,          jds, jde,          kds, kde-1, &
                                    ims, ime,            jms, jme,          kms, kme,   &
                                    ips, min(ipe,ide-1), jps, min(jpe,jde), kps, kpe-1  )
#endif
  if (rootproc) then 
     do k= 1,nsig_regional 
        do j= 1,nlat_regional+1 
           do i= 1,nlon_regional
#ifdef DM_PARALLEL	   
              field3v(i,j,k)=globbuf(i+(j-1)*(nlon_regional+1)+(k-1)*(nlon_regional+1)*(nlat_regional+1))
#else
              field3v(i,j,k)=grid%v_2(i,j,k)
#endif
           end do 
        end do
     end do 
     rmse_var='V'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index1, WrfType, ierr    )
!    write(6,*)' rmse_var=',trim(rmse_var)
!    write(6,*)' ordering=',ordering
!    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!    write(6,*)' ndim1=',ndim1
!    write(6,*)' staggering=',staggering
!    write(6,*)' start_index=',start_index
!    write(6,*)' end_index1=',end_index1
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),         &
                             field3v,WRF_REAL,0,0,0,ordering,      &
                             staggering, dimnames ,                &
                             start_index,end_index1,               & !dom
                             start_index,end_index1,               & !mem
                             start_index,end_index1,               & !pat
          ierr                                 )
  end if  ! end of rootproc

#ifdef DM_PARALLEL
  deallocate(globbuf) 
#endif

!
!  update W
!
#ifdef DM_PARALLEL
  if (rootproc) then
    allocate( globbuf((ide-1-ids+3)*(jde-1-jds+3)*(kde-1-kds+3)))
  else
    allocate( globbuf( 1 ) )
  endif
  globbuf=0.0
  CALL wrf_patch_to_global_double ( grid%w_2, globbuf, 1, 'Z', 'xyz' ,                    &
                                    ids, ide-1,          jds, jde-1,          kds, kde, &
                                    ims, ime,            jms, jme,            kms, kme, &
                                    ips, min(ipe,ide-1), jps, min(jpe,jde-1), kps, kpe  )
#endif
  if (rootproc) then 
     do k= 1,nsig_regional+1 
        do j= 1,nlat_regional 
           do i= 1,nlon_regional
#ifdef DM_PARALLEL	   
              field3ph(i,j,k)=globbuf(i+(j-1)*(nlon_regional+1)+(k-1)*(nlon_regional+1)*(nlat_regional+1))
#else
              field3ph(i,j,k)=grid%w_2(i,j,k)
#endif
           end do 
        end do
     end do 
     rmse_var='W'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index1, WrfType, ierr    )
!    write(6,*)' rmse_var=',trim(rmse_var)
!    write(6,*)' ordering=',ordering
!    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!    write(6,*)' ndim1=',ndim1
!    write(6,*)' staggering=',staggering
!    write(6,*)' start_index=',start_index
!    write(6,*)' end_index1=',end_index1
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),         &
                             field3ph,WRF_REAL,0,0,0,ordering,     &
                             staggering, dimnames ,                &
                             start_index,end_index1,               & !dom
                             start_index,end_index1,               & !mem
                             start_index,end_index1,               & !pat
                             ierr                                  )
  end if    

#ifdef DM_PARALLEL
  deallocate(globbuf) 
#endif

!-------------Update QCLOUD, QRAIN, QICE, QSNOW & QGROUP 
if ( cloud_cv_options >= 1 ) then ! update qcw and qrn
   if ( f_qc ) then
!
!  update QCLOUD
!
#ifdef DM_PARALLEL
      if (rootproc) then
         allocate( globbuf((ide-1-ids+3)*(jde-1-jds+3)*(kde-1-kds+3)))
      else
         allocate( globbuf( 1 ) )
      endif
      globbuf=0.0
      CALL wrf_patch_to_global_double ( grid%moist(:,:,:,p_qc), globbuf, 1, '', 'xyz' ,     &
                                    ids, ide-1,          jds, jde-1,          kds, kde-1, &
                                    ims, ime,            jms, jme,            kms, kme,   &
                                    ips, min(ipe,ide-1), jps, min(jpe,jde-1), kps, kpe-1  )
#endif
      if (rootproc) then 
         do k= 1,nsig_regional 
            do j= 1,nlat_regional 
               do i= 1,nlon_regional
#ifdef DM_PARALLEL	   
                  field3(i,j,k)=globbuf(i+(j-1)*(nlon_regional+1)+(k-1)*(nlon_regional+1)*(nlat_regional+1))
#else
                  field3(i,j,k)=grid%moist(i,j,k,p_qc)
#endif
               end do 
            end do
         end do 
         rmse_var='QCLOUD'
         call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
              start_index,end_index1, WrfType, ierr    )
!        write(6,*)' rmse_var=',trim(rmse_var)
!        write(6,*)' ordering=',ordering
!        write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!        write(6,*)' ndim1=',ndim1
!        write(6,*)' staggering=',staggering
!        write(6,*)' start_index=',start_index
!        write(6,*)' end_index1=',end_index1
         call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),         &
                                 field3,WRF_REAL,0,0,0,ordering,       &
                                 staggering, dimnames ,                &
                                 start_index,end_index1,               & !dom
                                 start_index,end_index1,               & !mem
                                 start_index,end_index1,               & !pat
                                 ierr                                  )
      end if  ! end of rootproc

#ifdef DM_PARALLEL
      deallocate(globbuf) 
#endif
   end if ! f_qc

   if ( f_qr ) then
!
!  update QRAIN
!
#ifdef DM_PARALLEL
      if (rootproc) then
         allocate( globbuf((ide-1-ids+3)*(jde-1-jds+3)*(kde-1-kds+3)))
      else
         allocate( globbuf( 1 ) )
      endif
      globbuf=0.0
      CALL wrf_patch_to_global_double ( grid%moist(:,:,:,p_qr), globbuf, 1, '', 'xyz' ,     &
                                    ids, ide-1,          jds, jde-1,          kds, kde-1, &
                                    ims, ime,            jms, jme,            kms, kme,   &
                                    ips, min(ipe,ide-1), jps, min(jpe,jde-1), kps, kpe-1  )
#endif
      if (rootproc) then 
         do k= 1,nsig_regional 
            do j= 1,nlat_regional 
               do i= 1,nlon_regional
#ifdef DM_PARALLEL	   
                  field3(i,j,k)=globbuf(i+(j-1)*(nlon_regional+1)+(k-1)*(nlon_regional+1)*(nlat_regional+1))
#else
                  field3(i,j,k)=grid%moist(i,j,k,p_qr)
#endif
               end do 
            end do
         end do 
         rmse_var='QRAIN'
         call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
              start_index,end_index1, WrfType, ierr    )
!        write(6,*)' rmse_var=',trim(rmse_var)
!        write(6,*)' ordering=',ordering
!        write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!        write(6,*)' ndim1=',ndim1
!        write(6,*)' staggering=',staggering
!        write(6,*)' start_index=',start_index
!        write(6,*)' end_index1=',end_index1
         call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),         &
                                 field3,WRF_REAL,0,0,0,ordering,       &
                                 staggering, dimnames ,                &
                                 start_index,end_index1,               & !dom
                                 start_index,end_index1,               & !mem
                                 start_index,end_index1,               & !pat
                                 ierr                                  )
      end if  ! end of rootproc

#ifdef DM_PARALLEL
      deallocate(globbuf) 
#endif

   end if ! f_qr
end if ! cloud_cv_options >= 1

if ( cloud_cv_options >= 2 ) then ! update qci, qsn, qgr
   if ( f_qi ) then
!
!  update QICE
!
#ifdef DM_PARALLEL
      if (rootproc) then
         allocate( globbuf((ide-1-ids+3)*(jde-1-jds+3)*(kde-1-kds+3)))
      else
         allocate( globbuf( 1 ) )
      endif
      globbuf=0.0
      CALL wrf_patch_to_global_double ( grid%moist(:,:,:,p_qi), globbuf, 1, '', 'xyz' ,     &
                                    ids, ide-1,          jds, jde-1,          kds, kde-1, &
                                    ims, ime,            jms, jme,            kms, kme,   &
                                    ips, min(ipe,ide-1), jps, min(jpe,jde-1), kps, kpe-1  )
#endif
      if (rootproc) then 
         do k= 1,nsig_regional 
            do j= 1,nlat_regional 
               do i= 1,nlon_regional
#ifdef DM_PARALLEL	   
                  field3(i,j,k)=globbuf(i+(j-1)*(nlon_regional+1)+(k-1)*(nlon_regional+1)*(nlat_regional+1))
#else
                  field3(i,j,k)=grid%moist(i,j,k,p_qi)
#endif
               end do 
            end do
         end do 
         rmse_var='QICE'
         call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
              start_index,end_index1, WrfType, ierr    )
!        write(6,*)' rmse_var=',trim(rmse_var)
!        write(6,*)' ordering=',ordering
!        write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!        write(6,*)' ndim1=',ndim1
!        write(6,*)' staggering=',staggering
!        write(6,*)' start_index=',start_index
!        write(6,*)' end_index1=',end_index1
         call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),         &
                                 field3,WRF_REAL,0,0,0,ordering,       &
                                 staggering, dimnames ,                &
                                 start_index,end_index1,               & !dom
                                 start_index,end_index1,               & !mem
                                 start_index,end_index1,               & !pat
                                 ierr                                 )
      end if  ! end of rootproc

#ifdef DM_PARALLEL
      deallocate(globbuf) 
#endif

   end if ! f_qi

   if ( f_qs ) then
!
!  update QSNOW
!
#ifdef DM_PARALLEL
      if (rootproc) then
         allocate( globbuf((ide-1-ids+3)*(jde-1-jds+3)*(kde-1-kds+3)))
      else
         allocate( globbuf( 1 ) )
      endif
      globbuf=0.0
      CALL wrf_patch_to_global_double ( grid%moist(:,:,:,p_qs), globbuf, 1, '', 'xyz' ,     &
                                    ids, ide-1,          jds, jde-1,          kds, kde-1, &
                                    ims, ime,            jms, jme,            kms, kme,   &
                                    ips, min(ipe,ide-1), jps, min(jpe,jde-1), kps, kpe-1  )
#endif
      if (rootproc) then 
         do k= 1,nsig_regional 
            do j= 1,nlat_regional 
               do i= 1,nlon_regional
#ifdef DM_PARALLEL	   
                  field3(i,j,k)=globbuf(i+(j-1)*(nlon_regional+1)+(k-1)*(nlon_regional+1)*(nlat_regional+1))
#else
                  field3(i,j,k)=grid%moist(i,j,k,p_qs)
#endif
               end do 
            end do
         end do 
         rmse_var='QSNOW'
         call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
              start_index,end_index1, WrfType, ierr    )
!        write(6,*)' rmse_var=',trim(rmse_var)
!        write(6,*)' ordering=',ordering
!        write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!        write(6,*)' ndim1=',ndim1
!        write(6,*)' staggering=',staggering
!        write(6,*)' start_index=',start_index
!        write(6,*)' end_index1=',end_index1
         call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),         &
                                 field3,WRF_REAL,0,0,0,ordering,       &
                                 staggering, dimnames ,                &
                                 start_index,end_index1,               & !dom
                                 start_index,end_index1,               & !mem
                                 start_index,end_index1,               & !pat
                                 ierr                                 )
      end if  ! end of rootproc

#ifdef DM_PARALLEL
      deallocate(globbuf) 
#endif

   end if ! f_qs

   if ( f_qg ) then
!
!  update QGRAUP
!
#ifdef DM_PARALLEL
      if (rootproc) then
         allocate( globbuf((ide-1-ids+3)*(jde-1-jds+3)*(kde-1-kds+3)))
      else
         allocate( globbuf( 1 ) )
      endif
      globbuf=0.0
      CALL wrf_patch_to_global_double ( grid%moist(:,:,:,p_qg), globbuf, 1, '', 'xyz' ,     &
                                    ids, ide-1,          jds, jde-1,          kds, kde-1, &
                                    ims, ime,            jms, jme,            kms, kme,   &
                                    ips, min(ipe,ide-1), jps, min(jpe,jde-1), kps, kpe-1  )
#endif
      if (rootproc) then 
         do k= 1,nsig_regional 
            do j= 1,nlat_regional 
               do i= 1,nlon_regional
#ifdef DM_PARALLEL	   
                  field3(i,j,k)=globbuf(i+(j-1)*(nlon_regional+1)+(k-1)*(nlon_regional+1)*(nlat_regional+1))
#else
                  field3(i,j,k)=grid%moist(i,j,k,p_qg)
#endif
               end do 
            end do
         end do 
         rmse_var='QGRAUP'
         call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
              start_index,end_index1, WrfType, ierr    )
!        write(6,*)' rmse_var=',trim(rmse_var)
!        write(6,*)' ordering=',ordering
!        write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!        write(6,*)' ndim1=',ndim1
!        write(6,*)' staggering=',staggering
!        write(6,*)' start_index=',start_index
!        write(6,*)' end_index1=',end_index1
         call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),         &
                                 field3,WRF_REAL,0,0,0,ordering,       &
                                 staggering, dimnames ,                &
                                 start_index,end_index1,               & !dom
                                 start_index,end_index1,               & !mem
                                 start_index,end_index1,               & !pat
                                 ierr                                 )
      end if  ! end of rootproc

#ifdef DM_PARALLEL
      deallocate(globbuf) 
#endif

   end if ! f_qg
end if ! cloud_cv_options >= 2
!-------------End of update QCLOUD, QRAIN, QICE, QSNOW & QGROUP

  if (rootproc) then
     deallocate(field2,field3,field3u,field3v,field3ph)
     call ext_ncd_ioclose(dh1, Status)
  end if   ! end of rootproc
  
end subroutine da_update_firstguess 
