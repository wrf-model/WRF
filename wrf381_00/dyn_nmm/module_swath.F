module module_swath   
#if ( HWRF == 1 )

#ifdef DM_PARALLEL
  use module_dm, only: wrf_dm_sum_integer, local_communicator, &
       getrealmpitype
#endif
  use module_domain, only : domain,get_ijk_from_grid
  use module_state_description, only: vt_ncep_2013, vt_ncep_2014

  implicit none

  private

  public :: update_interest, init_swath

contains

  subroutine init_swath(grid,config_flags,init)
    USE MODULE_CONFIGURE, ONLY : grid_config_rec_type
    type(domain), intent(inout) :: grid
    type(grid_config_rec_type), intent(in) :: config_flags
    logical, intent(in) :: init ! .true. = first initialization in wrf.exe
    character*255 :: message
    if(init) then
3088   format('Grid ',I0,' is resetting swath data.')
       write(message,3088) grid%id
       call wrf_message(message)
       if(size(grid%interesting)>1)   grid%interesting=0
       if(size(grid%precip_swath)>1)  grid%precip_swath=0
       if(size(grid%windsq_swath)>1)  grid%windsq_swath=0
    endif
  end subroutine init_swath

  function dx_at(grid, i,j,  ips,ipe,jps,jpe) result(dx)
    include 'mpif.h'
    type(domain), intent(inout) :: grid
    real :: dx, dx_local
    integer, intent(in) :: ips,ipe,jps,jpe, i,j
    integer :: in,jn,ierr
    if(i>=ips .and. i<=ipe .and. j>=jps .and. j<=jpe) then
       dx_local=max(0.,grid%dx_nmm(i,j))
    else
       dx_local=0
    endif
#ifdef DM_PARALLEL
    call mpi_allreduce(dx_local,dx,1,getrealmpitype(),MPI_MAX,local_communicator,ierr)
#else
    dx=dx_local
#endif
  end function dx_at

  subroutine storm_interest(grid)
    use module_tracker, only: update_tracker_post_move
    type(domain), intent(inout) :: grid
    integer :: ids,ide,jds,jde,kds,kde
    integer :: ims,ime,jms,jme,kms,kme
    integer :: ips,ipe,jps,jpe,kps,kpe
    integer :: i,j
    real :: sdistsq

    call get_ijk_from_grid(grid,  &
         ids,ide,jds,jde,kds,kde, &
         ims,ime,jms,jme,kms,kme, &
         ips,ipe,jps,jpe,kps,kpe  )
    
    sdistsq=grid%interest_rad_storm**2*1e6
    do j=max(jps,jds),min(jpe,jde)
       do i=max(ips,ids),min(ipe,ide)
          if(grid%tracker_distsq(i,j)<=sdistsq .and. grid%tracker_distsq(i,j)>1e-5) then
             grid%interesting(i,j) = ior(grid%interesting(i,j),1)
          endif
       enddo
    enddo
  end subroutine storm_interest

  subroutine kid_scanner(parent,nest)
    ! Sets parent%interest to 1 within nest%intrest_rad_parent
    ! kilometers of the nest parent center.
    type(domain), intent(inout) :: parent,nest
    integer :: ni1,nj1,ni2,nj2, nimid, njmid
    integer :: nims,nime,njms,njme,nkms,nkme
    integer :: nids,nide,njds,njde,nkds,nkde
    integer :: nips,nipe,njps,njpe,nkps,nkpe
    integer :: pims,pime,pjms,pjme,pkms,pkme
    integer :: pids,pide,pjds,pjde,pkds,pkde
    integer :: pips,pipe,pjps,pjpe,pkps,pkpe
    real :: dx,dy, dy2dx2, maxflatdist,flatdist, xshift, xfar,yfar,far
    integer :: ispan,istart,iend, jspan,jstart,jend, orwhat
    integer :: ki1,ki2,kj1,kj2,i,j
    character*255 :: message

#ifdef DM_PARALLEL
    integer :: yin,yang ! dummy variables for wrf_dm_maxval_real
    yin=-1
    yang=1
#endif

    call get_ijk_from_grid(nest,     &
         nids,nide,njds,njde,nkds,nkde, &
         nims,nime,njms,njme,nkms,nkme, &
         nips,nipe,njps,njpe,nkps,nkpe  )

    call get_ijk_from_grid(parent,     &
         pids,pide,pjds,pjde,pkds,pkde, &
         pims,pime,pjms,pjme,pkms,pkme, &
         pips,pipe,pjps,pjpe,pkps,pkpe  )

    ki1=nest%i_parent_start
    kj1=nest%j_parent_start
    ki2=ki1 + (nide-nids+1)/3
    kj2=kj1 + (njde-njds+1)/3
    nimid = (ki1 + ki2) / 2
    njmid = (kj1 + kj2) / 2

    dy=parent%dy_nmm
    dx=dx_at(parent,nimid,njmid, pips,pipe,pjps,pjpe)
    if(dx<1e-5) then
       write(message,30) nest%id, nimid,njmid, parent%id, ki1,kj1,ki2,kj2
       call wrf_error_fatal(message)
30     format("Nest ",I0," middle point ",I0,",",I0," is not inside parent ", &
              I0," (ki1=",I0," kj1=",I0," ki2=",I0," kj2=",I0,")")
    endif

    ispan =ceiling(1e3*nest%interest_rad_parent/dx)+1
    istart=max(pids,  nimid-ispan)
    iend  =min(pide-1,nimid+ispan)

    jspan =ceiling(1e3*nest%interest_rad_parent/dy)+1
    jstart=max(pjds,  njmid-jspan)
    jend  =min(pjde-1,njmid+jspan)

    dy2dx2 = dy*dy / (dx*dx)
    maxflatdist=nest%interest_rad_parent**2*1e6
    if(nest%id>0 .and. nest%id<=20) then
       orwhat=ishft(1,nest%id)
    else
       orwhat=ishft(1,21)
    endif
    
    if(jstart<=pjpe .or. jend>=pjps .or. istart<=pipe .or. iend>=pipe) then
       do j=pjps,min(pjpe,pjde-1)
          if(mod(j,2)==1) then
             xshift=1.
          else
             xshift=-1.
          endif
          do i=pips,min(pipe,pide-1)
             xfar=(i-nimid)*parent%dx_nmm(i,j)*2
             yfar=(j-njmid)*dy
             if(mod(njmid-j,2) /= 0) then
                xfar=xfar + parent%dx_nmm(i,j)*xshift
             endif
             far = xfar*xfar + yfar*yfar
             if(far<maxflatdist) then
                parent%interesting(i,j) = ior(parent%interesting(i,j),orwhat)
             endif
          enddo
       enddo
    endif
  end subroutine kid_scanner


  subroutine print_interest(grid)
    type(domain), intent(inout) :: grid
    integer :: ids,ide,jds,jde,kds,kde
    integer :: ims,ime,jms,jme,kms,kme
    integer :: ips,ipe,jps,jpe,kps,kpe
    integer :: i,j, count, total
    character*255 :: message
    ! Sets interesting=1 within interest_rad_self km of the domain center

    call get_ijk_from_grid(grid,     &
         ids,ide,jds,jde,kds,kde, &
         ims,ime,jms,jme,kms,kme, &
         ips,ipe,jps,jpe,kps,kpe  )
    total=(ide-ids)*(jde-jds)
    count=0
    do j=jps,min(jpe,jde-1)
       do i=ips,min(ipe,ide-1)
          if(grid%interesting(i,j)/=0) count=count+1
       enddo
    enddo
#ifdef DM_PARALLEL
    count=wrf_dm_sum_integer(count)
#endif
308 format('grid ',I0,': ',I0,' of ',I0,' points (',F0.2,'%) are in area of interest.')
    write(message,308) grid%id,count,total,real(count)/total
    call wrf_debug(1,message)
  end subroutine print_interest

  subroutine self_interest(grid)
    type(domain), intent(inout) :: grid
    real :: dx,dy, maxflatdist,flatdist, xfar,yfar,far
    integer :: ids,ide,jds,jde,kds,kde
    integer :: ims,ime,jms,jme,kms,kme
    integer :: ips,ipe,jps,jpe,kps,kpe
    integer :: imid, jmid, orwhat, i,j
    ! Sets interesting=1 within interest_rad_self km of the domain center

    call get_ijk_from_grid(grid,     &
         ids,ide,jds,jde,kds,kde, &
         ims,ime,jms,jme,kms,kme, &
         ips,ipe,jps,jpe,kps,kpe  )

    imid=(ide-ids)/2
    jmid=(jde-jds)/2
    dx=dx_at(grid,imid,jmid,ips,ipe,jps,jpe)
    dy=grid%dy_nmm

    maxflatdist = grid%interest_rad_self**2*1e6

    if(grid%id>0 .and. grid%id<=20) then
       orwhat=ishft(1,grid%id)
    else
       orwhat=ishft(1,21)
    endif

    do j=jps,min(jpe,jde-1)
       do i=ips,min(ipe,ide-1)
          if(grid%distsq(i,j) <= maxflatdist) &
               grid%interesting(i,j) = ior(grid%interesting(i,j),orwhat)
       enddo
    enddo
  end subroutine self_interest

  subroutine update_interest(grid,config_flags)
    USE MODULE_CONFIGURE, ONLY : grid_config_rec_type
    type(domain), intent(inout) :: grid
    type(grid_config_rec_type), intent(in) :: config_flags
    integer :: max_dom, nestid, parent_id, ikid, ki0,kj0,kni,knj
    logical :: nestless

    grid%interesting=0

    likes_kids: if(config_flags%interest_kids==1) then
       do ikid=1,grid%num_nests
          if(associated(grid%nests(ikid)%ptr)) &
               call kid_scanner(grid,grid%nests(ikid)%ptr)
       enddo
    endif likes_kids

    likes_storms: if(config_flags%interest_storms==1 .and. &
         ( grid%vortex_tracker == vt_ncep_2013 .or. &
         grid%vortex_tracker == vt_ncep_2014 ) ) then
       ! Region near cyclone is flagged as "interesting"
       call storm_interest(grid)
    endif likes_storms

    if(config_flags%interest_self==1) &
         call self_interest(grid)

    call print_interest(grid)
  end subroutine update_interest
#else
  ! Make sure the module is not empty in non-HWRF mode.
contains
  subroutine swath_dummy()
  end subroutine swath_dummy
#endif
end module module_swath
