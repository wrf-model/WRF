! Description:
!     Routines for setting upper boundary values for :
!     o3, no, no2, hno3, ch4, co, n2o and n2o5
!     From the default pressure level of 50 mb to the top level of the model,
!     the mixing ratio was set to a value from the monthly climatology 
!     which varied only in latitude (from ub_vals_moz3.nc file).
!     Between the tropopause level and the default pressure level of 50 mb,
!     the mixing ratio was relaxed with a 10-day relaxation factor.
!     The tropopause level was determined by the primary routine of 
!     the subroutine tropopause_twmo and the backup routine of
!     the subroutine tropopause_climate (using data from clim_p_trop.nc file).

! Author: 
!     Adopted from module mo_fstrat of CAM-Chem 
!     and put into the current format by Jeff Lee, Feb. 2011

module module_upper_bc_driver
     
      implicit none

      private

      public  :: upper_bc_init
      public  :: upper_bc_driver

      save

      real,    parameter :: mb2Pa    = 100. 
      real,    parameter :: vmr2ppmv = 1.e6

      real,    parameter :: tau_relax = 864000.   ! 10 days relaxation
      real               :: fac_relax
      real               :: fixed_ubc_press

      integer :: iend 
      integer :: jend

      integer :: ub_month_n      ! # of month   in ubc file
      integer :: ub_lev_n        ! # of lev     in ubc file
      integer :: ub_lat_n        ! # of lat     in ubc file
      integer :: ub_species_n    ! # of species in ubc file
      integer :: ub_nchar_n      ! # of nchar   in ubc file
      integer :: max_dom         ! wrf domain count

      real    , allocatable :: ub_p_m(:)        ! ubc midpoint levels pressure(Pa)
      real    , allocatable :: ub_p_e(:)        ! ubc edge     levels pressure(Pa)
      real    , allocatable :: ub_vmr(:,:,:,:)  ! values of vmr in ubc file
      real    , allocatable :: ub_lat(:)        ! values of lat in ubc file
      integer , allocatable :: ub_map(:)        ! species indices for ubc species

      integer :: nox_ndx   = -1
      integer :: ox_ndx    = -1

!----------------------------------------------------
!     private variables
!----------------------------------------------------

      type chem_upper_bc
          real, pointer :: vmr(:,:,:,:,:)  ! values of vmr in ubc file 
          logical       :: is_allocated 
      end type chem_upper_bc

      type(chem_upper_bc), private, allocatable :: upper_bc(:)

      contains

!==========================================================================
      subroutine upper_bc_driver( id, dtstep, current_date_char,   &
                                  chem, p_phy, p8w, tropo_lev,     &
                                  ids, ide, jds, jde, kds, kde,    &
                                  ims, ime, jms, jme, kms, kme,    &
                                  its, ite, jts, jte, kts, kte     )
      use module_configure
      use module_state_description
      use module_model_constants

!----------------------------------------------------
!     input arguments
!----------------------------------------------------
      integer, intent(in) :: id,                           &
                             ids,ide, jds,jde, kds,kde,    &
                             ims,ime, jms,jme, kms,kme,    &
                             its,ite, jts,jte, kts,kte

      real, dimension(ims:ime, kms:kme, jms:jme ),         &
               intent(in) :: p_phy,  & ! p at mid-level (Pa)
                             p8w       ! p at interface (Pa)
      integer, dimension(ims:ime, jms:jme ),               &
               intent(in) :: tropo_lev   ! tropopause level

      real,    intent(in) :: dtstep

      CHARACTER (LEN=256)       , intent(in) :: current_date_char

      real, dimension(ims:ime,kms:kme,jms:jme,num_chem ),  &
               intent(inout) :: chem

!----------------------------------------------------
!     local variables
!----------------------------------------------------
      integer :: next, last
      integer :: kk, km
      integer :: kmax
      integer :: lev_relax
      integer :: astat
      integer :: i, j, k, m, n
      integer :: ku(kts:kte)
      integer :: kl(kts:kte)
      integer :: del_p(kts:kte)

      real    :: del_time
      real    :: vmr_relax
      real    :: nox_ubc, xno, xno2, rno
      real    :: pint_vals(2)
      real, allocatable :: table_ox(:)
      real, allocatable :: p8w_temp(:)
      real, allocatable :: chem_temp(:)
      character(len=132) :: message

!-----------------------------------------------------------------------
! tile dimensions, needed for nest domains!!
!-------------------------------------------
      iend = min(ite,ide-1)
      jend = min(jte,jde-1)

!------------------------------------------------------------------------
!...  setup for time interpolation
!------------------------------------------------------------------------
      call get_time_interp_factors ( current_date_char, last, next, del_time )

!--------------------------------------------------------
! ... setup for ox conserving interp
!--------------------------------------------------------
      if( ox_ndx > 0 ) then
        allocate( table_ox(ub_lev_n-2),stat=astat )
        if( astat /= 0 ) then
           write(message,*) 'upper_bc_driver: table_ox allocation error = ',astat
           CALL wrf_message( trim(message) )
           call wrf_abort
        end if
      end if

j_tile_loop : &
   do j = jts, jend
i_tile_loop : &
   do i = its, iend

      !--------------------------------------------------------
      ! setup for the pressure interpolation
      !--------------------------------------------------------
lev_loop : &
      do k = kte,kts,-1     ! from top to bottom

         if( p_phy(i,k,j) <= ub_p_m(1) ) then
             kl(k) = 1
             ku(k) = 1
             del_p(k) = 0.
         else if(  p_phy(i,k,j) >= ub_p_m(ub_lev_n) ) then
             kl(k) = ub_lev_n
             ku(k) = ub_lev_n
             del_p(k) = 0.
         else             
             do kk = 2,ub_lev_n    ! from top to bottom of ub_p_m
                if(  p_phy(i,k,j) <= ub_p_m(kk) ) then
                   ku(k) = kk
                   kl(k) = kk - 1
                   del_p(k) = log( p_phy(i,k,j)/ub_p_m(kl(k)) ) &
                           / log( ub_p_m(ku(k))/ub_p_m(kl(k)) )
                   exit
                end if
             end do
         end if
      end do lev_loop

      !--------------------------------------------------------
      ! find max level less than fixed_ubc_press (default 50 mb)
      ! fixed UB vals from top of model to this level
      !--------------------------------------------------------

      if( p_phy(i,kte,j) > fixed_ubc_press ) then
        kmax = kte + 1
      else
        do kmax = kte,kts+1,-1                        ! from top to bottom
          if( p_phy(i,kmax,j) > fixed_ubc_press ) then
            exit
          end if
        end do
        kmax = kmax + 1
      endif

      !--------------------------------------------------------
      ! above fixed_ubc_press (default is 50 mb), 
      ! set the mixing ratios as fixed values
      !--------------------------------------------------------
   species_loop : &
      do m = 1,ub_species_n

   ub_overwrite : &
        if( ub_map(m) > 0 .and. kmax <= kte ) then
      !-----------------------------------------------------------
      ! o3
      !-----------------------------------------------------------
          if( m == ox_ndx ) then
            table_ox(1:ub_lev_n-2) =                             &  ! from top to bottom
              upper_bc(id)%vmr(i,j,m,last,2:ub_lev_n-1)          &
           + (upper_bc(id)%vmr(i,j,m,next,2:ub_lev_n-1)          &
           -  upper_bc(id)%vmr(i,j,m,last,2:ub_lev_n-1))*del_time

            km = kte - kmax + 1

            allocate ( p8w_temp (km+1) )
            allocate ( chem_temp(km) )
      !-----------------------------------------------------------
      ! upper_bc_rebin requires "top-down" ordering
      !-----------------------------------------------------------
            p8w_temp (1:km+1) = p8w (i,kte:kmax-1:-1,j)

            call upper_bc_rebin( ub_lev_n-2, km, ub_p_e, p8w_temp, &
                                 table_ox, chem_temp               )

            chem(i,kmax:kte,j,p_o3) = chem_temp(km:1:-1)

            deallocate( p8w_temp )
            deallocate( chem_temp )

            cycle species_loop
          end if
      !-----------------------------------------------------------
      ! others
      !-----------------------------------------------------------        
lev_loop_a : &
          do k = kte,kmax,-1    ! from top to bottom

            pint_vals(1) = upper_bc(id)%vmr(i,j,m,last,kl(k))  &
                         + (upper_bc(id)%vmr(i,j,m,last,ku(k)) &
                         -  upper_bc(id)%vmr(i,j,m,last,kl(k)))*del_p(k)
            pint_vals(2) = upper_bc(id)%vmr(i,j,m,next,kl(k))  &
                         + (upper_bc(id)%vmr(i,j,m,next,ku(k)) &
                         -  upper_bc(id)%vmr(i,j,m,next,kl(k)))*del_p(k)

      !-----------------------------------------------------------
      ! others: hno3, ch4, co, n2o, n2o5
      !-----------------------------------------------------------
            if( m /= nox_ndx ) then
              chem(i,k,j,ub_map(m)) = pint_vals(1)     &
                                    + (pint_vals(2) - pint_vals(1))*del_time
      !-----------------------------------------------------------
      ! others: no and no2
      !-----------------------------------------------------------
            else
              nox_ubc = pint_vals(1) + del_time * (pint_vals(2) - pint_vals(1))
              if( p_no >= param_first_scalar ) then
                xno = chem(i,k,j,p_no)
              else
                xno = 0.
              end if

              if( p_no2 >= param_first_scalar ) then
                xno2 = chem(i,k,j,p_no2)
              else
                xno2 = 0.
              end if

              if( xno > 0. .or. xno2 > 0.) then
                rno = xno / (xno + xno2)
              end if

              if( p_no >= param_first_scalar ) then
                chem(i,k,j,p_no) = rno * nox_ubc
              end if
              if( p_no2 >= param_first_scalar ) then
                chem(i,k,j,p_no2) = (1. - rno) * nox_ubc
              end if
            end if
          end do lev_loop_a
           
        end if ub_overwrite
      end do species_loop

   ub_relax : &
      if( tropo_lev(i,j) > 0 .and. tropo_lev(i,j) < kmax ) then
      !--------------------------------------------------------
      !... relax lower stratosphere to extended ubc
      !    check to make sure ubc is not being imposed too low
      !    lev_relax = lowest model level (highest pressure)
      !               in which to relax to ubc
      !--------------------------------------------------------
        lev_relax = tropo_lev(i,j)

        do while( p_phy(i,lev_relax,j) > ub_p_m(ub_lev_n) ) ! ub_p_m(ub_lev_n)= 440 mb
          lev_relax = lev_relax + 1                         ! move level upward                       
        end do

        if( lev_relax /= tropo_lev(i,j) ) then
          write(message,*) 'upper_bc_driver:Warning,raised ubc: at j,i,=  ',j,i
          call wrf_message( trim(message) )
          write(message,*) 'from ', tropo_lev(i,j),nint(p_phy(i,tropo_lev(i,j)-1,j)/mb2pa),' mb'
          call wrf_message( trim(message) )
          write(message,*) 'to   ', lev_relax,     nint(p_phy(i,lev_relax,j)     /mb2pa),' mb'
          call wrf_message( trim(message) )
        endif

   species_loop_a : &
        do m = 1,ub_species_n
   has_ubc : &
          if( ub_map(m) > 0 ) then
lev_loop_b: do k = kmax-1,lev_relax,-1      ! from top to bottom

              pint_vals(1) = upper_bc(id)%vmr(i,j,m,last,kl(k))         &
                           + (upper_bc(id)%vmr(i,j,m,last,ku(k))        &
                           -  upper_bc(id)%vmr(i,j,m,last,kl(k)))*del_p(k)
              pint_vals(2) = upper_bc(id)%vmr(i,j,m,next,kl(k))         &
                           + (upper_bc(id)%vmr(i,j,m,next,ku(k))        &
                           -  upper_bc(id)%vmr(i,j,m,next,kl(k)))*del_p(k)
          
              vmr_relax = pint_vals(1) &
                        + (pint_vals(2) - pint_vals(1))*del_time

              if( m /= nox_ndx ) then
                chem(i,k,j,ub_map(m)) = chem(i,k,j,ub_map(m)) &
                      + (vmr_relax - chem(i,k,j,ub_map(m))) * fac_relax
              else 
                if( p_no >= param_first_scalar ) then
                  xno  = chem(i,k,j,p_no)
                else
                  xno  = 0.
                endif

                if( p_no2 >= param_first_scalar ) then
                  xno2 = chem(i,k,j,p_no2)
                else
                  xno2 = 0.
                endif

                if( xno > 0. .or. xno2 > 0. ) then
                  rno  = xno / (xno + xno2)
                endif

                nox_ubc = xno + xno2
                nox_ubc = nox_ubc + (vmr_relax - nox_ubc) * fac_relax

                if( p_no >= param_first_scalar ) then
                  chem(i,k,j,p_no)  = rno * nox_ubc
                endif

                if( p_no2 >= param_first_scalar ) then
                  chem(i,k,j,p_no2)= (1. - rno) * nox_ubc
                endif
              endif
            end do lev_loop_b
          endif has_ubc
        end do species_loop_a

      endif ub_relax
 
     end do i_tile_loop
   end do j_tile_loop

   if( allocated( table_ox ) ) then
     deallocate( table_ox )
   endif

   end subroutine upper_bc_driver

!===========================================================================

      subroutine upper_bc_init(id, xlat, dt, config_flags,    &
                               ids,ide, jds,jde, kds,kde,     &
                               ims,ime, jms,jme, kms,kme,     &
                               its,ite, jts,jte, kts,kte      )
!---------------------------------------------------------------------
!       ... new initialization routine for upper boundary condition
!---------------------------------------------------------------------

      use module_domain    ! needed for p_co, p_o3....
      use module_state_description, only : param_first_scalar
      use module_configure,         only : grid_config_rec_type

      implicit none

!---------------------------------------------------------------------
!       ... dummy arguments
!---------------------------------------------------------------------
      integer, intent(in)  :: id,                          &
                              ids,ide, jds,jde, kds,kde,   &
                              ims,ime, jms,jme, kms,kme,   &
                              its,ite, jts,jte, kts,kte
      real, intent(in)     :: dt
      real,    intent(in)  :: xlat(ims:ime, jms:jme)       ! unit: degree
      type(grid_config_rec_type), intent(in) :: config_flags

!---------------------------------------------------------------------
!       ... local variables
!---------------------------------------------------------------------

      integer :: astat
      integer :: ncid
      integer :: varid
      integer :: dimid(4)
      integer :: start(4)
      integer :: count(4)
      integer :: dimid_lev
      integer :: dimid_lat
      integer :: dimid_month
      integer :: dimid_species
      integer :: ndims  

      character(len=132) :: message
      character(len=128) :: err_msg
      character(len=64)  :: filename
      character(len=3)   :: id_num
      character(len=25), allocatable :: ub_specname(:)

      character(len=80) :: attribute

 
      integer :: lati
      integer, allocatable :: lat_interp(:,:)
      real,    allocatable :: lat_del(:,:)

      integer :: i, j, k, m
      integer :: j1, j2


      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
!-------------------------------------------------------------------------

#ifdef NETCDF
include 'netcdf.inc'
#else
    call wrf_message( 'upper_bc_init: requires netcdf' )
    call wrf_abort
#endif

      !-------------------------------------
      !... note: grid%XLAT(ide,:)  =0.
      !... note: grid%XLONG(:,jde) =0.
      !...       => iend = min(ite,ide-1)
      !             jend = min(jte,jde-1)
      !-------------------------------------

      iend = min(ite,ide-1)
      jend = min(jte,jde-1)

!---------------------------------------------------------------------
! ... allocate chem_upper_bc type
!---------------------------------------------------------------------
  if( id == 1 .and. .not. allocated(upper_bc) ) then
    CALL nl_get_max_dom( 1,max_dom )

    allocate( upper_bc(max_dom),stat=astat )
    if( astat /= 0 ) then
      CALL wrf_message( 'upper_bc_init: failed to allocate  upper_bc' )
      CALL wrf_abort
    end if

    upper_bc(:)%is_allocated = .false.
  endif

upper_bc_allocated : &
  if( .not. upper_bc(id)%is_allocated ) then
!---------------------------------------------------------------------
!... allocate upper_bc type
!--------------------------------------------------------------------

is_d01 : &
    IF( id == 1 ) then
master_proc : &
      IF( wrf_dm_on_monitor() ) THEN
        write(id_num,'(i3)') 100+id
        write(message,*) 'upper_bc_init: intializing domain ' // id_num(2:3)
        CALL wrf_message( trim(message) )
       
!---------------------------------------------------------------------
! ... open upper_bc netcdf file
!---------------------------------------------------------------------
        filename = config_flags%fixed_ubc_inname
        if( filename == ' ' ) then
          call wrf_message( 'upper_bc_init: input filename not specified in namelist' )
          call wrf_abort
        endif

        err_msg = 'upper_bc_init: failed to open file ' // trim(filename)
        call handle_ncerr( nf_open( trim(filename), nf_noclobber, ncid ), trim(err_msg) )
        write(message,*) 'upper_bc_init: id, open filename= ',id, filename
        CALL wrf_message( trim(message) )
!---------------------------------------------------------------------
! ... get dimensions
!---------------------------------------------------------------------
       err_msg = 'upper_bc_init: failed to get month id'
       call handle_ncerr( nf_inq_dimid( ncid, 'month', dimid_month ), trim(err_msg) )
       err_msg = 'upper_bc_init: failed to get month'
       call handle_ncerr( nf_inq_dimlen( ncid, dimid_month, ub_month_n ), trim(err_msg) )

      err_msg = 'upper_bc_init: failed to get lat id'
      call handle_ncerr( nf_inq_dimid( ncid, 'lat', dimid_lat ), trim(err_msg) ) 
      err_msg = 'upper_bc_init: failed to get lat'
      call handle_ncerr( nf_inq_dimlen( ncid, dimid_lat, ub_lat_n ), trim(err_msg) ) 

      err_msg = 'upper_bc_init: failed to get lev id'
      call handle_ncerr( nf_inq_dimid( ncid, 'lev', dimid_lev ), trim(err_msg) ) 
      err_msg = 'upper_bc_init: failed to get lev'
      call handle_ncerr( nf_inq_dimlen( ncid, dimid_lev, ub_lev_n ), trim(err_msg) )

      err_msg = 'upper_bc_init: failed to get species id'
      call handle_ncerr( nf_inq_dimid( ncid, 'species', dimid_species ), trim(err_msg) ) 
      err_msg = 'upper_bc_init: failed to get species'
      call handle_ncerr( nf_inq_dimlen( ncid, dimid_species, ub_species_n ), trim(err_msg) )

      err_msg = 'upper_bc_init: failed to get nchar id'
      call handle_ncerr( nf_inq_dimid( ncid, 'nchar', dimid(1) ), trim(err_msg) ) 
      err_msg = 'upper_bc_init: failed to get nchar'
      call handle_ncerr( nf_inq_dimlen( ncid, dimid(1), ub_nchar_n ), trim(err_msg) )

   END IF master_proc
!---------------------------------------------------------------------
! ... bcast the dimensions
!---------------------------------------------------------------------
#ifdef DM_PARALLEL
      CALL wrf_dm_bcast_integer ( ub_month_n   , 1 )
      CALL wrf_dm_bcast_integer ( ub_lat_n     , 1 )
      CALL wrf_dm_bcast_integer ( ub_lev_n     , 1 )
      CALL wrf_dm_bcast_integer ( ub_species_n , 1 )
      CALL wrf_dm_bcast_integer ( ub_nchar_n   , 1 )
#endif
!---------------------------------------------------------------------
! ... allocate module arrays 
!---------------------------------------------------------------------
      allocate( ub_lat(ub_lat_n), stat=astat )
      if( astat /= 0 ) then
         call wrf_message( 'upper_bc_init: failed to allocate ub_lat' )
         call wrf_abort
      end if

      allocate( ub_vmr(ub_lat_n,ub_species_n,ub_month_n,ub_lev_n), stat=astat )
      if( astat /= 0 ) then
         call wrf_message( 'upper_bc_init: failed to allocate ub_vmr' )
         call wrf_abort
      end if

      allocate( ub_p_m(ub_lev_n), stat=astat )
      if( astat /= 0 ) then
         call wrf_message( 'upper_bc_init: failed to allocate ub_p_m' )
         call wrf_abort
      end if

      allocate( ub_p_e(ub_lev_n-1), stat=astat )
      if( astat /= 0 ) then
         call wrf_message( 'upper_bc_init: failed to allocate ub_p_e' )
         call wrf_abort
      end if

      allocate( ub_map(ub_species_n), stat=astat )
      if( astat /= 0 ) then
         call wrf_message( 'upper_bc_init: failed to allocate ub_map' )
         call wrf_abort
      end if

!---------------------------------------------------------------------
! ... read arrays
!---------------------------------------------------------------------
master_proc_a : &
   IF ( wrf_dm_on_monitor() ) THEN

!lev
      err_msg = 'upper_bc_init: failed to get lev variable id'
      call handle_ncerr( nf_inq_varid( ncid, 'lev', varid ), trim(err_msg) )
      err_msg = 'upper_bc_init: failed to read lev variable'
      call handle_ncerr( nf_get_var_real( ncid, varid, ub_p_m ), trim(err_msg) )

      !-------- check unit
      attribute(:) = ' '
      astat = nf_get_att_text( ncid, varid, 'units', attribute )
      if( astat == nf_noerr )then
         if( trim(attribute) == 'mb' .or. trim(attribute) == 'hpa' )then
            write(message,*) 'upper_bc_init: units for lev = ',trim(attribute),'... converting to pa'
            call wrf_message( trim(message) )
            ub_p_m(:) = mb2Pa * ub_p_m(:)
         else if( trim(attribute) /= 'pa' .and. trim(attribute) /= 'pa' )then
            write(message,*) 'upper_bc_init: unknown units for lev, units=*',trim(attribute),'*'
            call wrf_message( trim(message) )
            call wrf_abort
         end if
      else
         write(message,*) 'upper_bc_init: warning! units attribute for lev missing, assuming mb'
         call wrf_message( trim(message) )
         ub_p_m(:) = mb2Pa * ub_p_m(:)
      end if

! ... set values of ub_p_e

      ub_p_e(1:ub_lev_n-1) = .5*(ub_p_m(1:ub_lev_n-1) + ub_p_m(2:ub_lev_n))

!---------------------------
!lat
      err_msg = 'upper_bc_init: failed to get lat variable id'
      call handle_ncerr( nf_inq_varid( ncid, 'lat', varid ), trim(err_msg) )
      err_msg = 'upper_bc_init: failed to read lat variable'
      call handle_ncerr( nf_get_var_real( ncid, varid, ub_lat ), trim(err_msg) )

!---------------------------
!specname
      allocate( ub_specname(ub_species_n), stat=astat )
      if( astat /= 0 ) then
         call wrf_message( 'upper_bc_init: failed to allocate ub_specname' )
         call wrf_abort
      end if

species_loop : &
      do i = 1,ub_species_n
        start(:2) = (/ 1, i /)
        count(:2) = (/ ub_nchar_n, 1 /)

        ub_specname(i)(:) = ' '

        err_msg = 'upper_bc_init: failed to get specname variable id'
        call handle_ncerr( nf_inq_varid( ncid, 'specname', varid ), trim(err_msg) )
        err_msg = 'upper_bc_init: failed to read ub_specname variable'
        call handle_ncerr( nf_get_vara_text( ncid, varid, start(:2), count(:2), ub_specname(i) ), trim(err_msg) )

        ub_map(i) = 0
         
        if( trim(ub_specname(i)) ==       'HNO3' .and. p_hno3 >= param_first_scalar ) then              
          ub_map(i) = p_hno3
        else if ( trim(ub_specname(i)) == 'CH4'  .and. p_ch4  >= param_first_scalar ) then
          ub_map(i) = p_ch4
        else if ( trim(ub_specname(i)) == 'CO'   .and. p_co   >= param_first_scalar ) then
          ub_map(i) = p_co
        else if ( trim(ub_specname(i)) == 'N2O'  .and. p_n2o  >= param_first_scalar ) then
          ub_map(i) = p_n2o
        else if ( trim(ub_specname(i)) == 'N2O5' .and. p_n2o5 >= param_first_scalar ) then
          ub_map(i) = p_n2o5
        else if ( trim(ub_specname(i)) == 'OX'   .and. p_o3   >= param_first_scalar ) then
          ub_map(i) = p_o3
          ox_ndx    = i
        else if ( trim(ub_specname(i)) == 'NOX'  .and. &
                (p_no >= param_first_scalar .or. p_no2 >= param_first_scalar) ) then
          ub_map(i) = p_no
          nox_ndx   = i
        endif
        
        if( ub_map(i) == 0 ) then
         write(message,*) 'upper_bc_init: ubc table species ',trim(ub_specname(i)), ' not used'
         call wrf_message( trim(message) )
        end if
      end do species_loop      

!--------------------------
!vmr    
      err_msg = 'upper_bc_init: failed to get vmr variable id'
      call handle_ncerr( nf_inq_varid( ncid, 'vmr', varid ), trim(err_msg) )

      ! check dimensions

      err_msg = 'upper_bc_init: failed to get ndims of vmr variable'
      call handle_ncerr( nf_inq_varndims( ncid, varid, ndims ), trim(err_msg) )

      if( ndims /= 4 ) then
         write(message,*) 'upper_bc_init: error! variable vmr has ndims = ',ndims,', expecting 4'
         call wrf_message( trim(message) )
         call wrf_abort
      end if

      err_msg = 'upper_bc_init: failed to get dimid of vmr variable'
      call handle_ncerr( nf_inq_vardimid( ncid, varid, dimid ), trim(err_msg) )

      if( dimid(1) /= dimid_lat   .or. dimid(2) /= dimid_species .or. &
          dimid(3) /= dimid_month .or. dimid(4) /= dimid_lev )then
          write(message,*) 'upper_bc_init: error! dimensions in wrong order for variable vmr,'// &
               'expecting (lat,species,month,lev)'
          call wrf_message( trim(message) )
          call wrf_abort
      end if

!------------------------------------------------------------------
! ... read in the ub mixing ratio values
!------------------------------------------------------------------
      err_msg = 'upper_bc_init: failed to read vmr variable'
      call handle_ncerr( nf_get_var_real( ncid, varid, ub_vmr ), trim(err_msg) )

!---------------------------------------------------------------------
!... close input netcdf file
!---------------------------------------------------------------------
      err_msg = 'upper_bc_init: failed to close file ' // trim(filename)
      call handle_ncerr( nf_close( ncid ), trim(err_msg) )

     deallocate( ub_specname, stat=astat )
     if( astat /= 0 ) then
        write(message,*) ': failed to deallocate ub_specnmae; astat = ',astat
        call wrf_message( trim(message) )
        call wrf_abort
     end if

   ENDIF master_proc_a

!---------------------------------------------------------------------
! ... bcast the variables
!---------------------------------------------------------------------
#ifdef DM_PARALLEL   
      CALL wrf_dm_bcast_integer ( nox_ndx   , 1 )
      CALL wrf_dm_bcast_integer ( ox_ndx    , 1 )

      CALL wrf_dm_bcast_integer ( ub_map, size(ub_map) )
      CALL wrf_dm_bcast_real    ( ub_p_m, size(ub_p_m) )
      CALL wrf_dm_bcast_real    ( ub_p_e, size(ub_p_e) )
      CALL wrf_dm_bcast_real    ( ub_lat, size(ub_lat) )
      CALL wrf_dm_bcast_real    ( ub_vmr, size(ub_vmr) )
#endif
     fixed_ubc_press = config_flags%fixed_ubc_press*mb2Pa
   ENDIF is_d01

   upper_bc(id)%is_allocated = .true.

   write(message,*) 'upper_bc_init: ub_vmr(1,1,1,:)'
   call wrf_message( trim(message) )
   do k = 1,ub_lev_n,5
     write(message,'(1p,5g15.7)') ub_vmr(1,1,1,k:min(k+4,ub_lev_n))
     call wrf_message( trim(message) )
   end do

!---------------------------------------------------------------------
! ... allocate domain specific arrays
!---------------------------------------------------------------------
   allocate( lat_del(its:iend,jts:jend), stat=astat )
   if( astat /= 0 ) then
     call wrf_message( 'upper_bc_init: failed to allocate lat_del' )
     call wrf_abort
   end if

   allocate( lat_interp(its:iend,jts:jend), stat=astat )
   if( astat /= 0 ) then
     call wrf_message( 'upper_bc_init: failed to allocate lat_interp' )
     call wrf_abort
   end if

!------------------------------------------------------------------
! ... regrid  mixing ratio to model latitudes
!------------------------------------------------------------------
!-------------------------------------
!... get lat_del, lat_interp and lati
!-------------------------------------
   call get_lat_interp_factors( id, xlat, jend, iend,       &
                                ub_lat_n, ub_lat,           &
                                lat_del, lat_interp, lati,  &
                                ids,ide, jds,jde, kds,kde,  &
                                ims,ime, jms,jme, kms,kme,  &
                                its,ite, jts,jte, kts,kte   )

!---------------------------------------------------------------------
! ... allocate upper_bc(id) component array
!---------------------------------------------------------------------
      allocate( upper_bc(id)%vmr(its:iend,jts:jend,ub_species_n,ub_month_n,ub_lev_n), stat=astat )
      if( astat /= 0 ) then
         call wrf_message( 'upper_bc_init: failed to allocate upper_bc(id)%vrm' )
         call wrf_abort
      end if  

!--------------------------------------------------------------------
!... regrid from ub_vmr to upper_bc(id)%vmr
!--------------------------------------------------------------------
   do j = jts,jend
     do i = its,iend
       j1 = lat_interp(i,j)
       j2 = j1 + lati
       upper_bc(id)%vmr(i,j,:,:,:) = ub_vmr(j1,:,:,:) + &
                        lat_del(i,j) * (ub_vmr(j2,:,:,:) - ub_vmr(j1,:,:,:))
     end do
   end do

!----------------------------------------------------
!      transform to ppm vmr
!----------------------------------------------------
    upper_bc(id)%vmr(:,:,:,:,:) = upper_bc(id)%vmr(:,:,:,:,:)*vmr2ppmv 

    write(message,*) 'upper_bc_init: upper_bc(id)%vmr(its+5,jts+5,1,6,:)'
    call wrf_message( trim(message) )
    do k = 1,ub_lev_n,5
      write(message,'(1p,5g15.7)') upper_bc(id)%vmr(its+5,jts+5,1,6,k:min(k+4,ub_lev_n))
      call wrf_message( trim(message) )
    end do

!--------------------------------------------------------
! ... set up the relaxation for lower stratosphere
!--------------------------------------------------------
! ... tau_relax = relaxation timescale (in sec)
!     fac_relax = fractional relaxation towards ubc
!             1 => use ubc
!             0 => ignore ubc, use model concentrations
!--------------------------------------------------------

    fac_relax = 1. - exp( -real(dt)/tau_relax )

!--------------------------------------------------------

      if( id == max_dom ) then
        deallocate( ub_vmr)
        deallocate( ub_lat)
      endif
      deallocate( lat_del)
      deallocate( lat_interp)
      
      call wrf_message( ' ' )
      write(message,*) 'upper_bc_init: DONE intialized domain ',id
      call wrf_message( trim(message) )
      call wrf_message( ' ' )

  endif upper_bc_allocated

  end subroutine upper_bc_init

!===========================================================================

      subroutine handle_ncerr( ret, mes )
!---------------------------------------------------------------------
!       ... netcdf error handling routine
!---------------------------------------------------------------------

      implicit none

!---------------------------------------------------------------------
!       ... dummy arguments
!---------------------------------------------------------------------
      integer, intent(in) :: ret
      character(len=*), intent(in) :: mes

#ifdef NETCDF
include 'netcdf.inc'
#endif

      if( ret /= nf_noerr ) then
         call wrf_message( trim(mes) )
         call wrf_message( trim(nf_strerror(ret)) )
         call wrf_abort
      end if

      end subroutine handle_ncerr

!===========================================================================

      subroutine get_time_interp_factors ( current_date_char, last, next, del_time) 

      use module_date_time, only : get_julgmt

      implicit none

!-----------------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------------
      CHARACTER (LEN=256),intent(in) :: current_date_char

      integer, intent(out) :: next, last
      real,    intent(out) :: del_time
!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------

      integer, parameter :: day_of_year(12) = (/ 16, 45, 75, 105, 136, 166, 197, &
                                                 228, 258, 289, 319, 350        /)
      INTEGER :: julyr , julday 
      REAL    :: gmt
      real    :: calday

      integer :: m
!------------------------------------------------------------------------------

      call get_julgmt ( current_date_char, julyr, julday, gmt )

      calday = real(julday) + gmt

      if( calday < day_of_year(1) ) then
         next = 1
         last = 12
         del_time = (365. + calday - day_of_year(12)) &
                / (365. + day_of_year(1) - day_of_year(12))
      else if( calday >= day_of_year(12) ) then
         next = 1
         last = 12
         del_time = (calday - day_of_year(12)) &
                / (365. + day_of_year(1) - day_of_year(12))
      else
         do m = 11,1,-1
            if( calday >= day_of_year(m) ) then
               exit
            end if
         end do
         last = m
         next = m + 1
         del_time = (calday - day_of_year(m)) / (day_of_year(m+1) - day_of_year(m))
      end if

      del_time = max( min( 1.,del_time ),0. )

      end subroutine get_time_interp_factors

!=========================================================================== 

  subroutine upper_bc_rebin( nsrc, ntrg, src_x, trg_x, src, trg )
    !---------------------------------------------------------------
    !   ... rebin src to trg
    !---------------------------------------------------------------

    implicit none

    !---------------------------------------------------------------
    !   ... dummy arguments
    !---------------------------------------------------------------
    integer, intent(in)   :: nsrc                  ! dimension source array
    integer, intent(in)   :: ntrg                  ! dimension target array
    real, intent(in)      :: src_x(nsrc+1)         ! source coordinates
    real, intent(in)      :: trg_x(ntrg+1)         ! target coordinates
    real, intent(in)      :: src(nsrc)             ! source array
    real, intent(out)     :: trg(ntrg)             ! target array

    !---------------------------------------------------------------
    !   ... local variables
    !---------------------------------------------------------------
    integer  :: i, l
    integer  :: si, si1
    integer  :: sil, siu
    real :: y
    real :: sl, su
    real :: tl, tu

    do i = 1,ntrg
       tl = trg_x(i)
       if( tl < src_x(nsrc+1) ) then
          do sil = 1,nsrc+1
             if( tl <= src_x(sil) ) then
                exit
             end if
          end do
          tu = trg_x(i+1)
          do siu = 1,nsrc+1
             if( tu <= src_x(siu) ) then
                exit
             end if
          end do
          y   = 0.
          sil = max( sil,2 )
          siu = min( siu,nsrc+1 )
          do si = sil,siu
             si1 = si - 1
             sl  = max( tl,src_x(si1) )
             su  = min( tu,src_x(si) )
             y   = y + (su - sl)*src(si1)
          end do
          trg(i) = y/(trg_x(i+1) - trg_x(i))
       else
          trg(i) = 0.
       end if
    end do

  end subroutine upper_bc_rebin

!===========================================================================

  subroutine get_lat_interp_factors( id, xlat, jend, iend,          &
                                     from_lat_n, from_lat,          &
                                     lat_del, lat_interp, lati,     &
                                     ids, ide, jds, jde, kds, kde,  &
                                     ims, ime, jms, jme, kms, kme,  &
                                     its, ite, jts, jte, kts, kte   )
                                  
!---------------------------------------------------------------------
!	... Determine indicies and deltas for transform
!           Note : it is assumed that the latitude and longitude
!                  arrays are monotonic
!--------------------------------------------------------------------

      implicit none

!---------------------------------------------------------------------
!	... Dummy args
!---------------------------------------------------------------------
      integer, intent(in) :: id,                           &
                             ids,ide, jds,jde, kds,kde,    &
                             ims,ime, jms,jme, kms,kme,    &
                             its,ite, jts,jte, kts,kte

      real,    intent(in)  :: xlat(ims:ime, jms:jme)       ! unit: degree
      integer, intent(in)  :: jend
      integer, intent(in)  :: iend
      integer, intent(in)  :: from_lat_n
      real ,   intent(in)  :: from_lat(from_lat_n)

      integer, intent(out) :: lati
      integer, intent(out) :: lat_interp(its:iend,jts:jend)
      real,    intent(out) :: lat_del(its:iend,jts:jend)      

!---------------------------------------------------------------------
!	... Local variables
!---------------------------------------------------------------------
      integer :: astat

      real    :: to_lat(jts:jend)       ! values of wrf lat, 1d

      integer :: to_lon_n
      real    :: countx

      real    :: target_lat
      integer :: latl, latu                                       
      real    :: max_lat, min_lat
      logical :: from_lats_mono_pos

      integer :: i, j
      integer :: m, n

!---------------------------------------------------------------------
!... set "module" variable, converter(id)%lati
!---------------------------------------------------------------------

     max_lat = from_lat(from_lat_n)
     min_lat = from_lat(1)

     if( from_lat(from_lat_n) >=  from_lat(1)) then  ! increasing lat
	latl = 1
	latu = from_lat_n
	lati = 1
	from_lats_mono_pos = .true.
     else                                            ! decreasing lat
	latl = from_lat_n
	latu = 1
	lati = -1
	from_lats_mono_pos = .false.
     end if
 
!---------------------------------------------------------------------
! ... Set interpolation latitude indicies and del_lat
!---------------------------------------------------------------------
      do j = jts,jend
      do i = its,iend
         target_lat = xlat(i,j)

	 if( target_lat <= min_lat ) then
	    lat_del(i,j) = 0. 
	    lat_interp(i,j)  = latl 
	 else if( target_lat >= max_lat ) then
	    lat_del(i,j) = 1. 
	    if( from_lats_mono_pos ) then
	       lat_interp(i,j) = latu - 1
	    else
	       lat_interp(i,j) = latu + 1
	    end if
	 else
	    do m = latl,latu,lati
	       if( target_lat < from_lat(m) ) then
		  n = m - lati
	          lat_interp(i,j) = min( from_lat_n,max( 1,n ) )
	          lat_del(i,j)    = &
                      (target_lat - from_lat(n))/(from_lat(m) - from_lat(n))
	          exit
	       end if
	    end do
         end if
      end do
      end do

      end subroutine get_lat_interp_factors

end module module_upper_bc_driver

