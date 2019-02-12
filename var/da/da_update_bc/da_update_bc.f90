program da_update_bc

   !-----------------------------------------------------------------------
   ! Purpose: update BC file from wrfvar output.
   ! current version reads only wrf-netcdf file format
   !
   ! Y.-R. Guo, 03/18/2008:
   !   1) Fixed the bug for low_bdy_only;
   !   2) Introducing another namelist variable: update_lsm
   !      update_lsm = .true. --- The LSM predicted variables: 
   !                         TSLB, SMOIS, SNOW, SH2O, RHOSN, CANWAT, SNOWH
   !                              will be updated based on wrf_input file
   !                 = .false. -- no updated, default.
   !
   !-----------------------------------------------------------------------

   use da_netcdf_interface, only : da_get_var_3d_real_cdf, &
      da_put_var_3d_real_cdf, da_get_dims_cdf, da_put_var_2d_real_cdf, &
      da_get_var_2d_real_cdf, da_get_var_2d_int_cdf, da_get_bdytimestr_cdf, &
      da_get_times_cdf, da_get_bdyfrq, stderr, stdout, da_put_var_2d_int_cdf, &
      da_get_var_1d_real_cdf, da_get_gl_att_int_cdf

   use da_module_couple_uv, only : da_couple_uv

   implicit none

   include 'netcdf.inc'

   integer, parameter :: max_3d_variables = 20, &
                         max_2d_variables = 25
 
   character(len=512) :: da_file,      &
                         da_file_02,   &
                         wrf_bdy_file, &
                         wrf_input
 
   character(len=20) :: var_pref, var_name, vbt_name

   character(len=20) :: var3d(max_3d_variables), &
                        varsf(max_2d_variables)

   character(len=10), dimension(4) :: bdyname, tenname

   ! for WRF hybrid coordinate
   integer           :: nlevf, nlevh
   integer           :: hybrid_opt, use_theta_m
   real, allocatable :: c1f(:), c2f(:), c1h(:), c2h(:)

   integer           :: ids, ide, jds, jde, kds, kde
   integer           :: num3d, num2d, ndims
   integer           :: time_level
   integer           :: i,j,k,l,m,n

   integer, dimension(4) :: dims
 
   real, allocatable, dimension(:,:,:) :: tend3d, scnd3d, frst3d, full3d, full3d2

   real, allocatable, dimension(:,:,:) :: u, v, u2, v2

   real, allocatable, dimension(:,  :) :: mu, mub, msfu, msfv, msfm, &
                                          mu2, tend2d, scnd2d, frst2d, full2d

   real, allocatable, dimension(:,  :) :: tsk, tsk_wrfvar
   real, allocatable, dimension(:,:)   :: snow, snowc, snowh

   integer, allocatable, dimension(:,:) :: ivgtyp, full2dint

   character(len=80), allocatable, dimension(:) :: times, &
                                                   thisbdytime, nextbdytime
 
   integer :: east_end, north_end, io_status, cdfid, varid, domain_id, iswater
   integer :: iostatus(4)

   logical :: debug, update_lateral_bdy, update_low_bdy, update_lsm, keep_tsk_wrf
   logical :: keep_snow_wrf, var4d_lbc

   real :: bdyfrq

   character(len=512) :: wrfvar_output_file    ! obsolete. Kept for backward compatibility
   logical            :: cycling, low_bdy_only ! obsolete. Kept for backward compatibility

   integer, parameter :: namelist_unit = 7, &
                         ori_unit = 11, &
                         new_unit = 12

   namelist /control_param/ da_file,      &
                            da_file_02,   &
                            wrf_bdy_file, &
                            wrf_input, domain_id, var4d_lbc, &
                            debug, update_lateral_bdy, update_low_bdy, update_lsm, &
                            keep_tsk_wrf, keep_snow_wrf, iswater, &
                            wrfvar_output_file, cycling, low_bdy_only

   da_file            = 'wrfvar_output'
   da_file_02         = 'ana02'
   wrf_bdy_file       = 'wrfbdy_d01'
   wrf_input          = 'wrfinput_d01'
   domain_id          = 1

   var4d_lbc          = .false.
   debug              = .false. 
   update_lateral_bdy = .true.
   update_low_bdy     = .true.
   update_lsm         = .false.
   keep_tsk_wrf       = .true.
   keep_snow_wrf      = .true.
   iswater            = 16      ! USGS water index: 16, MODIS water index: 17

   wrfvar_output_file = 'OBSOLETE'
   cycling            = .false.
   low_bdy_only       = .false.

   !---------------------------------------------------------------------
   ! Read namelist
   !---------------------------------------------------------------------
   io_status = 0

   open(unit = namelist_unit, file = 'parame.in', &
          status = 'old' , access = 'sequential', &
          form   = 'formatted', action = 'read', &
          iostat = io_status)

   if (io_status /= 0) then
      write(unit=stdout,fmt=*) 'Error to open namelist file: parame.in.'
      write(unit=stdout,fmt=*) 'Will work for updating lateral boundary only.'
   else
      read(unit=namelist_unit, nml = control_param , iostat = io_status)

      if (io_status /= 0) then
         write(unit=stdout,fmt=*) 'Error to read control_param. Stopped.'
         stop
      end if

      ! deal with the old namelist
      if ( index(wrfvar_output_file, 'OBSOLETE') <= 0 ) then
         ! wrfvar_output_file is set in the user's parame.in
         ! reset the settings
         da_file = wrfvar_output_file
         if ( domain_id > 1 ) then
            low_bdy_only = .true.
         end if
         if ( cycling .and. domain_id == 1 ) then
            update_lateral_bdy = .true.
            update_low_bdy     = .true.
         else
            if ( low_bdy_only ) then
               update_lateral_bdy = .false.
               update_low_bdy     = .true.
            else
               update_lateral_bdy = .true.
               update_low_bdy     = .false.
            end if
         end if
      end if

      WRITE(unit=stdout, fmt='(2a)') &
           'da_file       = ', trim(da_file), &
           'da_file_02    = ', trim(da_file_02), &
           'wrf_bdy_file  = ', trim(wrf_bdy_file), &
           'wrf_input     = ', trim(wrf_input)

      WRITE(unit=stdout, fmt='(2(a, L10))')             &
           'update_lateral_bdy = ', update_lateral_bdy, &
           'update_low_bdy     = ', update_low_bdy

      if ( update_lsm ) keep_snow_wrf = .false.

      close(unit=namelist_unit)
   end if

   ! 3D need update
   num3d=5
   var3d(1)='U'
   var3d(2)='V'
   var3d(3)='T'
   var3d(4)='PH'
   var3d(5)='QVAPOR'
!  var3d(6)='W'

   ! 2D need update
   num2d=23
   varsf(1)='MUB'
   varsf(2)='MU'
   varsf(3)='MAPFAC_U'
   varsf(4)='MAPFAC_V'
   varsf(5)='MAPFAC_M'
   varsf(6)='TMN'
   varsf(7)='SST'
   varsf(8)='TSK'
   varsf(9)='VEGFRA'
   varsf(10)='ALBBCK'
   varsf(11)='TSLB'
   varsf(12)='SMOIS'
   varsf(13)='SNOW'
   varsf(14)='SEAICE'
   varsf(15)='SH2O'
   varsf(16)='CANWAT'
   varsf(17)='RHOSN'
   varsf(18)='SNOWH'
   varsf(19)='LANDMASK'
   varsf(20)='IVGTYP'
   varsf(21)='ISLTYP'
   varsf(22)='SNOWC'
   varsf(23)='XLAND'

   if ( domain_id > 1 ) then
      write(unit=stdout, fmt='(a,i2)') 'Nested domain ID=',domain_id
      write(unit=stdout, fmt='(a)') &
        'No wrfbdy file needed, only low boundary need to be updated.'
      if ( update_lateral_bdy ) then
         write(unit=stdout, fmt='(a)') &
            'Re-setting update_lateral_bdy to be false for nested domain.'
         update_lateral_bdy = .false.
      end if
      update_low_bdy     = .true.
   end if

   if ( update_lateral_bdy ) then
   ! First, the boundary times
   call da_get_dims_cdf(wrf_bdy_file, 'Times', dims, ndims, debug)

   if (debug) then
      write(unit=stdout, fmt='(a,i2,2x,a,4i6)') &
           'Times: ndims=', ndims, 'dims=', (dims(i), i=1,ndims)
   end if

   time_level = dims(2)

   if (time_level < 1) then
      write(unit=stdout, fmt='(a,i2/a)') &
           'time_level = ', time_level, &
           'We need at least one time-level BDY.'
      stop 'Wrong BDY file.'
   end if

   allocate(times(dims(2)))
   allocate(thisbdytime(dims(2)))
   allocate(nextbdytime(dims(2)))

   call da_get_times_cdf(wrf_bdy_file, times, dims(2), dims(2), debug)

   call da_get_bdytimestr_cdf(wrf_bdy_file, 'thisbdytime', thisbdytime, dims(2), debug)
   call da_get_bdytimestr_cdf(wrf_bdy_file, 'nextbdytime', nextbdytime, dims(2), debug)

   call da_get_bdyfrq(thisbdytime(1), nextbdytime(1), bdyfrq, debug)

   if (debug) then
      do n=1, dims(2)
         write(unit=stdout, fmt='(3(a, i2, 2a,2x))') &
           '       times(', n, ')=', trim(times(n)), &
           'thisbdytime (', n, ')=', trim(thisbdytime(n)), &
           'nextbdytime (', n, ')=', trim(nextbdytime(n))
      end do
   end if

   end if

   east_end=0
   north_end=0

   cdfid = ncopn(da_file, NCWRITE, io_status )

   ! for WRF hybrid coordinate

   call da_get_gl_att_int_cdf(da_file, 'BOTTOM-TOP_PATCH_END_STAG',   nlevf, debug, io_status)
   call da_get_gl_att_int_cdf(da_file, 'BOTTOM-TOP_PATCH_END_UNSTAG', nlevh, debug, io_status)
   if ( io_status /= NF_NOERR .or. nlevf <= 0 .or. nlevh <= 0 ) then
      write(unit=stdout,fmt=*) 'Error finding the number of vertical levels'
      stop
   else
      allocate ( c1f(nlevf) )
      allocate ( c2f(nlevf) )
      allocate ( c1h(nlevh) )
      allocate ( c2h(nlevh) )
   end if

   ! initialize as use_theta_m = 0
   use_theta_m = 0
   call da_get_gl_att_int_cdf(da_file, 'USE_THETA_M', use_theta_m, debug, io_status)
   write(stdout,*) 'use_theta_m = ', use_theta_m

   ! initialize as hybrid_opt = 0
   hybrid_opt = 0
   c1f(:) = 1.0
   c2f(:) = 0.0
   c1h(:) = 1.0
   c2h(:) = 0.0

   call da_get_gl_att_int_cdf(da_file, 'HYBRID_OPT', hybrid_opt, debug, io_status)
   if ( io_status == NF_NOERR .and. hybrid_opt > 0 ) then
      ! when HYBRID_OPT is available from the file (V3.9 and later)
      ! use C1F,C2F,C1H,C2H from the file
      call da_get_var_1d_real_cdf( da_file, 'C1F', c1f, nlevf, 1, debug)
      call da_get_var_1d_real_cdf( da_file, 'C2F', c2f, nlevf, 1, debug)
      call da_get_var_1d_real_cdf( da_file, 'C1H', c1h, nlevh, 1, debug)
      call da_get_var_1d_real_cdf( da_file, 'C2H', c2h, nlevh, 1, debug)
   end if

   write(stdout,*) 'hybrid_opt = ', hybrid_opt
   if ( debug ) then
      write(stdout,*) 'c1f = ', c1f
      write(stdout,*) 'c2f = ', c2f
      write(stdout,*) 'c1h = ', c1h
      write(stdout,*) 'c2h = ', c2f
   end if

   ! For 2D variables
   ! Get mu, mub, msfu, and msfv

   do n=1,num2d

      io_status = nf_inq_varid(cdfid, trim(varsf(n)), varid)
      if (io_status /= 0 ) then
         print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                   n, io_status, trim(varsf(n)), " does not exist"
         cycle
      endif

      call da_get_dims_cdf( da_file, trim(varsf(n)), dims, &
         ndims, debug)

      select case(trim(varsf(n)))
      case ('MU') ;
         if ( .not. update_lateral_bdy ) cycle

         allocate(mu(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( da_file, &
            trim(varsf(n)), mu, dims(1), dims(2), 1, debug)

         east_end=dims(1)+1
         north_end=dims(2)+1

         if ( var4d_lbc ) then
            allocate(mu2(dims(1), dims(2)))

            call da_get_var_2d_real_cdf( da_file_02, &
               trim(varsf(n)), mu2, dims(1), dims(2), 1, debug)
         end if
      case ('MUB') ;
         if ( .not. update_lateral_bdy ) cycle

         allocate(mub(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( da_file, trim(varsf(n)), mub, &
                                   dims(1), dims(2), 1, debug)
      case ('MAPFAC_U') ;
         if ( .not. update_lateral_bdy ) cycle

         allocate(msfu(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( da_file, trim(varsf(n)), msfu, &
                                   dims(1), dims(2), 1, debug)
      case ('MAPFAC_V') ;
         if ( .not. update_lateral_bdy ) cycle

         allocate(msfv(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( da_file, trim(varsf(n)), msfv, &
                                   dims(1), dims(2), 1, debug)
      case ('MAPFAC_M') ;
         if ( .not. update_lateral_bdy ) cycle

         allocate(msfm(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( da_file, trim(varsf(n)), msfm, &
                                   dims(1), dims(2), 1, debug)
      case ('TSK') ;
         if ( .not. update_low_bdy ) cycle

         allocate(tsk(dims(1), dims(2)))
         allocate(tsk_wrfvar(dims(1), dims(2)))
         allocate(ivgtyp(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( wrf_input, trim(varsf(n)), tsk, &
                                   dims(1), dims(2), 1, debug)

         if ( keep_tsk_wrf ) then
            call da_get_var_2d_real_cdf( da_file, trim(varsf(n)), tsk_wrfvar, &
                                      dims(1), dims(2), 1, debug)
            !hcl call da_get_var_2d_int_cdf( da_file, 'IVGTYP', ivgtyp, &
            call da_get_var_2d_int_cdf( wrf_input, 'IVGTYP', ivgtyp, &
                                      dims(1), dims(2), 1, debug)
            ! update TSK.
            do j=1,dims(2)
               do i=1,dims(1)
                  if (ivgtyp(i,j) /= iswater)  tsk(i,j)=tsk_wrfvar(i,j)
               end do
            end do
         end if

            call da_put_var_2d_real_cdf( da_file, trim(varsf(n)), tsk, &
                                      dims(1), dims(2), 1, debug)
            deallocate(tsk)
            deallocate(ivgtyp)
            deallocate(tsk_wrfvar)

         !hcl case ('TMN', 'SST', 'VEGFRA', 'ALBBCK', 'SEAICE') ;
         case ('TMN', 'SST', 'VEGFRA', 'ALBBCK', 'SEAICE', 'LANDMASK', 'XLAND') ;
            if ( .not. update_low_bdy ) cycle

            allocate(full2d(dims(1), dims(2)))

            call da_get_var_2d_real_cdf( wrf_input, trim(varsf(n)), full2d, &
                                      dims(1), dims(2), 1, debug)

            call da_put_var_2d_real_cdf( da_file, trim(varsf(n)), full2d, &
                                      dims(1), dims(2), 1, debug)
            deallocate(full2d)

         case ('IVGTYP', 'ISLTYP') ;  !hcl add
            if ( .not. update_low_bdy ) cycle

            allocate(full2dint(dims(1), dims(2)))

            call da_get_var_2d_int_cdf( wrf_input, trim(varsf(n)), full2dint, &
                                      dims(1), dims(2), 1, debug)

            call da_put_var_2d_int_cdf( da_file, trim(varsf(n)), full2dint, &
                                      dims(1), dims(2), 1, debug)
            deallocate(full2dint)

         case ('SNOW', 'RHOSN', 'SNOWH', 'SNOWC') ;
            if ( (.not. update_lsm) .and. (.not. update_low_bdy) ) cycle
            if ( keep_snow_wrf ) cycle
               allocate(full2d(dims(1), dims(2)))

               call da_get_var_2d_real_cdf( wrf_input, trim(varsf(n)), full2d, &
                                      dims(1), dims(2), 1, debug )

               call da_put_var_2d_real_cdf( da_file, trim(varsf(n)), full2d, &
                                      dims(1), dims(2), 1, debug )
               deallocate(full2d)

         case ('CANWAT') ;
            if ( .not. update_lsm ) cycle
               allocate(full2d(dims(1), dims(2)))

               call da_get_var_2d_real_cdf( wrf_input, trim(varsf(n)), full2d, &
                                      dims(1), dims(2), 1, debug )
!               print *,"sum(full2d^2)=", sum(full2d*full2d)

               call da_put_var_2d_real_cdf( da_file, trim(varsf(n)), full2d, &
                                      dims(1), dims(2), 1, debug )
               deallocate(full2d)

         case ('TSLB', 'SMOIS', 'SH2O') ;
            if( .not. update_lsm ) cycle
               allocate(full3d(dims(1), dims(2), dims(3)))

               call da_get_var_3d_real_cdf( wrf_input, trim(varsf(n)), full3d, &
                                      dims(1), dims(2), dims(3), 1, debug )
!               print *,"sum(full3d^2)=", sum(full3d*full3d)

               call da_put_var_3d_real_cdf( da_file, trim(varsf(n)), full3d, &
                                      dims(1), dims(2), dims(3), 1, debug )
               deallocate(full3d)

         case default ;
            write(unit=stdout,fmt=*) 'It is impossible here. varsf(n)=', trim(varsf(n))
      end select
   end do

   ! check for snow over water
   iostatus(1) = nf_inq_varid(cdfid, 'IVGTYP', varid)
   iostatus(2) = nf_inq_varid(cdfid, 'SNOW',   varid)
   iostatus(3) = nf_inq_varid(cdfid, 'SNOWC',  varid)
   iostatus(4) = nf_inq_varid(cdfid, 'SNOWH',  varid)
   if ( iostatus(1) == 0 ) then
      allocate(snow(dims(1), dims(2)))
      allocate(snowc(dims(1), dims(2)))
      allocate(snowh(dims(1), dims(2)))
      allocate(ivgtyp(dims(1), dims(2)))
      if ( iostatus(1) == 0 ) then
         call da_get_var_2d_int_cdf( da_file, 'IVGTYP', ivgtyp,    &
                               dims(1), dims(2), 1, debug)
      end if
      if ( iostatus(2) == 0 ) then
         call da_get_var_2d_real_cdf( da_file, 'SNOW',    snow,     &
                                dims(1), dims(2), 1, debug)
      end if
      if ( iostatus(3) == 0 ) then
         call da_get_var_2d_real_cdf( da_file, 'SNOWC',  snowc,     &
                                dims(1), dims(2), 1, debug)
      end if
      if ( iostatus(4) == 0 ) then
         call da_get_var_2d_real_cdf( da_file, 'SNOWH',  snowh,     &
                                dims(1), dims(2), 1, debug)
      end if
      if ( iostatus(2) == 0 ) then
         do j = 1, dims(2)
            do i = 1, dims(1)
               if (ivgtyp(i,j) == iswater)  then
                  if ( snow(i,j) > 0.0 ) then
                     write(unit=stdout,fmt=*) 'Remove snow over water at i, j = ', i, j
                     if ( iostatus(2) == 0 ) snow(i,j)  = 0.0
                     if ( iostatus(3) == 0 ) snowc(i,j) = 0.0
                     if ( iostatus(4) == 0 ) snowh(i,j) = 0.0
                  end if
               end if
            end do
         end do
      end if
      if ( iostatus(2) == 0 ) then
         call da_put_var_2d_real_cdf( da_file, 'SNOW',   snow, &
                                dims(1), dims(2), 1, debug)
      end if
      if ( iostatus(3) == 0 ) then
         call da_put_var_2d_real_cdf( da_file, 'SNOWC',  snowc, &
                                dims(1), dims(2), 1, debug)
      end if
      if ( iostatus(4) == 0 ) then
         call da_put_var_2d_real_cdf( da_file, 'SNOWH',  snowh, &
                                dims(1), dims(2), 1, debug)
      end if
      deallocate(snow)
      deallocate(snowc)
      deallocate(snowh)
      deallocate(ivgtyp)
   end if
   
 if ( update_lateral_bdy ) then

   if (east_end < 1 .or. north_end < 1) then
      write(unit=stdout, fmt='(a)') 'Wrong data for Boundary.'
      stop
   end if

   write(unit=stdout,fmt='(/a/)') 'Processing the lateral boundary condition:'

   ! boundary variables
   bdyname(1)='_BXS'
   bdyname(2)='_BXE'
   bdyname(3)='_BYS'
   bdyname(4)='_BYE'

   ! boundary tendancy variables
   tenname(1)='_BTXS'
   tenname(2)='_BTXE'
   tenname(3)='_BTYS'
   tenname(4)='_BTYE'

   do m=1,4
      var_name='MU' // trim(bdyname(m))
      vbt_name='MU' // trim(tenname(m))

      call da_get_dims_cdf( wrf_bdy_file, trim(var_name), dims, ndims, debug)

      allocate(frst2d(dims(1), dims(2)))
      allocate(scnd2d(dims(1), dims(2)))
      allocate(tend2d(dims(1), dims(2)))

      ! Get variable at second time level
      if ( .not. var4d_lbc ) then
         if (time_level > 1) then
            call da_get_var_2d_real_cdf( wrf_bdy_file, trim(var_name), scnd2d, &
                                      dims(1), dims(2), 2, debug)
         else
            call da_get_var_2d_real_cdf( wrf_bdy_file, trim(var_name), frst2d, &
                                      dims(1), dims(2), 1, debug)
            call da_get_var_2d_real_cdf( wrf_bdy_file, trim(vbt_name), tend2d, &
                                      dims(1), dims(2), 1, debug)
         end if
      end if

      if (debug) then
         write(unit=ori_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
              'No.', m, 'Variable: ', trim(vbt_name), &
              'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

         call da_get_var_2d_real_cdf( wrf_bdy_file, trim(vbt_name), tend2d, &
                                   dims(1), dims(2), 1, debug)

         write(unit=ori_unit, fmt='(a, 10i12)') &
              ' old ', (i, i=1,dims(2))
         do j=1,dims(1)
            write(unit=ori_unit, fmt='(i4, 1x, 10e20.7)') &
                  j, (tend2d(j,i), i=1,dims(2))
         end do
      end if

      ! calculate variable at first time level
      select case(m)
      case (1) ;             ! West boundary
         do l=1,dims(2)
            do j=1,dims(1)
               if (time_level < 2 .and. .not. var4d_lbc) &
                  scnd2d(j,l)=frst2d(j,l)+tend2d(j,l)*bdyfrq
               if (var4d_lbc) scnd2d(j,l)=mu2(l,j)
               frst2d(j,l)=mu(l,j)
            end do
         end do
      case (2) ;             ! East boundary
         do l=1,dims(2)
            do j=1,dims(1)
               if (time_level < 2 .and. .not. var4d_lbc) &
                  scnd2d(j,l)=frst2d(j,l)+tend2d(j,l)*bdyfrq
               if (var4d_lbc) scnd2d(j,l)=mu2(east_end-l,j)
               frst2d(j,l)=mu(east_end-l,j)
            end do
         end do
      case (3) ;             ! South boundary
         do l=1,dims(2)
            do i=1,dims(1)
               if (time_level < 2 .and. .not. var4d_lbc) &
                  scnd2d(i,l)=frst2d(i,l)+tend2d(i,l)*bdyfrq
               if (var4d_lbc) scnd2d(i,l)=mu2(i,l)
               frst2d(i,l)=mu(i,l)
            end do
         end do
      case (4) ;             ! North boundary
         do l=1,dims(2)
            do i=1,dims(1)
               if (time_level < 2 .and. .not. var4d_lbc) &
                  scnd2d(i,l)=frst2d(i,l)+tend2d(i,l)*bdyfrq
               if (var4d_lbc) scnd2d(i,l)=mu2(i,north_end-l)
               frst2d(i,l)=mu(i,north_end-l)
            end do
         end do
      case default ;
         write(unit=stdout,fmt=*) 'It is impossible here. mu, m=', m
      end select

      ! calculate new tendancy 
      do l=1,dims(2)
         do i=1,dims(1)
            tend2d(i,l)=(scnd2d(i,l)-frst2d(i,l))/bdyfrq
         end do
      end do

      if (debug) then
         write(unit=new_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
              'No.', m, 'Variable: ', trim(vbt_name), &
              'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

         write(unit=new_unit, fmt='(a, 10i12)') &
              ' new ', (i, i=1,dims(2))

         do j=1,dims(1)
            write(unit=new_unit, fmt='(i4, 1x, 10e20.7)') &
                  j, (tend2d(j,i), i=1,dims(2))
         end do
      end if

      ! output new variable at first time level
      call da_put_var_2d_real_cdf( wrf_bdy_file, trim(var_name), frst2d, &
                                dims(1), dims(2), 1, debug)
      ! output new tendancy 
      call da_put_var_2d_real_cdf( wrf_bdy_file, trim(vbt_name), tend2d, &
                                dims(1), dims(2), 1, debug)

      deallocate(frst2d)
      deallocate(scnd2d)
      deallocate(tend2d)
   end do

   !---------------------------------------------------------------------
   ! For 3D variables

   ! Get U
   call da_get_dims_cdf( da_file, 'U', dims, ndims, debug)

   ! call da_get_att_cdf( da_file, 'U', debug)

   allocate(u(dims(1), dims(2), dims(3)))

   ids=1
   ide=dims(1)-1
   jds=1
   jde=dims(2)
   kds=1
   kde=dims(3)

   call da_get_var_3d_real_cdf( da_file, 'U', u, &
                             dims(1), dims(2), dims(3), 1, debug)
   if ( var4d_lbc ) then
      allocate(u2(dims(1), dims(2), dims(3)))
      call da_get_var_3d_real_cdf( da_file_02, 'U', u2, &
                                dims(1), dims(2), dims(3), 1, debug)
   end if

   ! do j=1,dims(2)
   !    write(unit=stdout, fmt='(2(a,i5), a, f12.8)') &
   !       'u(', dims(1), ',', j, ',1)=', u(dims(1),j,1)
   ! end do

   ! Get V
   call da_get_dims_cdf( da_file, 'V', dims, ndims, debug)

   ! call da_get_att_cdf( da_file, 'V', debug)

   allocate(v(dims(1), dims(2), dims(3)))

   call da_get_var_3d_real_cdf( da_file, 'V', v, &
                             dims(1), dims(2), dims(3), 1, debug)
   if ( var4d_lbc ) then
      allocate(v2(dims(1), dims(2), dims(3)))
      call da_get_var_3d_real_cdf( da_file_02, 'V', v2, &
                                dims(1), dims(2), dims(3), 1, debug)
   end if

   ! do i=1,dims(1)
   !    write(unit=stdout, fmt='(2(a,i5), a, f12.8)') &
   !       'v(', i, ',', dims(2), ',1)=', v(i,dims(2),1)
   ! end do

   if (debug) then
      write(unit=stdout, fmt='(a,e20.12,4x)') &
           'Before couple Sample u=', u(dims(1)/2,dims(2)/2,dims(3)/2), &
           'Before couple Sample v=', v(dims(1)/2,dims(2)/2,dims(3)/2)
   end if

   !---------------------------------------------------------------------
   ! Couple u, v.
   call da_couple_uv ( u, v, mu, mub, msfu, msfv, c1h, c2h, ids, ide, jds, jde, kds, kde)
   if ( var4d_lbc ) then
      call da_couple_uv ( u2, v2, mu2, mub, msfu, msfv, c1h, c2h, ids, ide, jds, jde, kds, kde)
   end if

   if (debug) then
      write(unit=stdout, fmt='(a,e20.12,4x)') &
           'After  couple Sample u=', u(dims(1)/2,dims(2)/2,dims(3)/2), &
           'After  couple Sample v=', v(dims(1)/2,dims(2)/2,dims(3)/2)
   end if

   !---------------------------------------------------------------------
   !For 3D variables

   do n=1,num3d
      write(unit=stdout, fmt='(a, i3, 2a)') 'Processing: var3d(', n, ')=', trim(var3d(n))

      call da_get_dims_cdf( da_file, trim(var3d(n)), dims, ndims, debug)

      allocate(full3d(dims(1), dims(2), dims(3)))
      if ( var4d_lbc ) allocate(full3d2(dims(1), dims(2), dims(3)))

      east_end=dims(1)+1
      north_end=dims(2)+1

      select case(trim(var3d(n)))
      case ('U') ;           ! U
         ! var_pref='R' // trim(var3d(n))
         var_pref=trim(var3d(n))
         full3d(:,:,:)=u(:,:,:)
         if ( var4d_lbc ) full3d2(:,:,:)=u2(:,:,:)
      case ('V') ;           ! V 
         ! var_pref='R' // trim(var3d(n))
         var_pref=trim(var3d(n))
         full3d(:,:,:)=v(:,:,:)
         if ( var4d_lbc ) full3d2(:,:,:)=v2(:,:,:)
      case ('W') ;
         ! var_pref = 'R' // trim(var3d(n))
         var_pref = trim(var3d(n))

         call da_get_var_3d_real_cdf( da_file, trim(var3d(n)), &
            full3d, dims(1), dims(2), dims(3), 1, debug)
         if ( var4d_lbc ) &
            call da_get_var_3d_real_cdf( da_file_02, trim(var3d(n)), &
               full3d2, dims(1), dims(2), dims(3), 1, debug)

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'Before couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if

         do k=1,dims(3)
            do j=1,dims(2)
               do i=1,dims(1)
                  full3d(i,j,k)=full3d(i,j,k)*(c1f(k)*(mu(i,j)+mub(i,j))+c2f(k))/msfm(i,j)
                  if ( var4d_lbc ) full3d2(i,j,k)=full3d2(i,j,k)*(c1f(k)*(mu2(i,j)+mub(i,j))+c2f(k))/msfm(i,j)
               end do
            end do
         end do

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'After  couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if
      case ('T', 'PH') ;
         var_pref=trim(var3d(n))
 
         if ( use_theta_m > 0 .and. trim(var3d(n)) == 'T' ) then
           call da_get_var_3d_real_cdf( da_file, 'THM', &
              full3d, dims(1), dims(2), dims(3), 1, debug)
           if ( var4d_lbc ) &
              call da_get_var_3d_real_cdf( da_file_02, 'THM', &
                 full3d2, dims(1), dims(2), dims(3), 1, debug)
         else
           call da_get_var_3d_real_cdf( da_file, trim(var3d(n)), &
              full3d, dims(1), dims(2), dims(3), 1, debug)
           if ( var4d_lbc ) &
              call da_get_var_3d_real_cdf( da_file_02, trim(var3d(n)), &
                 full3d2, dims(1), dims(2), dims(3), 1, debug)
         end if

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'Before couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if

         if ( dims(3) == nlevf ) then
            ! applying c1f and c2f for mu
            do k=1,dims(3)
               do j=1,dims(2)
                  do i=1,dims(1)
                     full3d(i,j,k)=full3d(i,j,k)*(c1f(k)*(mu(i,j)+mub(i,j))+c2f(k))
                     if ( var4d_lbc ) full3d2(i,j,k)=full3d2(i,j,k)*(c1f(k)*(mu2(i,j)+mub(i,j))+c2f(k))
                  end do
               end do
            end do
         else if ( dims(3) == nlevh ) then
            ! applying c1h and c2h for mu
            do k=1,dims(3)
               do j=1,dims(2)
                  do i=1,dims(1)
                     full3d(i,j,k)=full3d(i,j,k)*(c1h(k)*(mu(i,j)+mub(i,j))+c2h(k))
                     if ( var4d_lbc ) full3d2(i,j,k)=full3d2(i,j,k)*(c1h(k)*(mu2(i,j)+mub(i,j))+c2h(k))
                  end do
               end do
            end do
         else
            write(unit=stdout,fmt=*) 'Error finding matched dimensions.'
            stop
         end if

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'After  couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if
      case ('QVAPOR', 'QCLOUD', 'QRAIN', 'QICE', 'QSNOW', 'QGRAUP') ;
         ! var_pref='R' // var3d(n)(1:2)
         ! var_pref=var3d(n)(1:2)
         var_pref=var3d(n)
 
         call da_get_var_3d_real_cdf( da_file, trim(var3d(n)), &
            full3d, dims(1), dims(2), dims(3), 1, debug)
         if ( var4d_lbc ) &
            call da_get_var_3d_real_cdf( da_file_02, trim(var3d(n)), &
               full3d2, dims(1), dims(2), dims(3), 1, debug)

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'Before couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if

         do k=1,dims(3)
            do j=1,dims(2)
               do i=1,dims(1)
                  full3d(i,j,k)=full3d(i,j,k)*(c1h(k)*(mu(i,j)+mub(i,j))+c2h(k))
                  if ( var4d_lbc ) full3d2(i,j,k)=full3d2(i,j,k)*(c1h(k)*(mu2(i,j)+mub(i,j))+c2h(k))
               end do
            end do
         end do

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'After  couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if
      case default ;
         write(unit=stdout,fmt=*) 'It is impossible here. var3d(', n, ')=', trim(var3d(n))
      end select

      do m=1,4
         var_name=trim(var_pref) // trim(bdyname(m))
         vbt_name=trim(var_pref) // trim(tenname(m))

         write(unit=stdout, fmt='(a, i3, 2a)') &
            'Processing: bdyname(', m, ')=', trim(var_name)

         call da_get_dims_cdf( wrf_bdy_file, trim(var_name), dims, ndims, debug)

         allocate(frst3d(dims(1), dims(2), dims(3)))
         allocate(scnd3d(dims(1), dims(2), dims(3)))
         allocate(tend3d(dims(1), dims(2), dims(3)))

         ! Get variable at second time level
         if ( .not. var4d_lbc ) then
            if (time_level > 1) then
               call da_get_var_3d_real_cdf( wrf_bdy_file, trim(var_name), scnd3d, &
                                         dims(1), dims(2), dims(3), 2, debug)
            else
               call da_get_var_3d_real_cdf( wrf_bdy_file, trim(var_name), frst3d, &
                                         dims(1), dims(2), dims(3), 1, debug)
               call da_get_var_3d_real_cdf( wrf_bdy_file, trim(vbt_name), tend3d, &
                                         dims(1), dims(2), dims(3), 1, debug)
            end if
         end if

         if (debug) then
            write(unit=ori_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
                 'No.', m, 'Variable: ', trim(vbt_name), &
                 'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

            call da_get_var_3d_real_cdf( wrf_bdy_file, trim(vbt_name), tend3d, &
                                      dims(1), dims(2), dims(3), 1, debug)

            write(unit=ori_unit, fmt='(a, 10i12)') &
                 ' old ', (i, i=1,dims(3))
            do j=1,dims(1)
               write(unit=ori_unit, fmt='(i4, 1x, 10e20.7)') &
                     j, (tend3d(j,dims(2)/2,i), i=1,dims(3))
            end do
         end if
   
         select case(trim(bdyname(m)))
         case ('_BXS') ;             ! West boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do j=1,dims(1)
               if (time_level < 2 .and. .not. var4d_lbc) &
               scnd3d(j,k,l)=frst3d(j,k,l)+tend3d(j,k,l)*bdyfrq
               if ( var4d_lbc ) scnd3d(j,k,l)=full3d2(l,j,k)
               frst3d(j,k,l)=full3d(l,j,k)
            end do
            end do
            end do
         case ('_BXE') ;             ! East boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do j=1,dims(1)
               if (time_level < 2 .and. .not. var4d_lbc) &
               scnd3d(j,k,l)=frst3d(j,k,l)+tend3d(j,k,l)*bdyfrq
               if ( var4d_lbc ) scnd3d(j,k,l)=full3d2(east_end-l,j,k)
               frst3d(j,k,l)=full3d(east_end-l,j,k)
            end do
            end do
            end do
         case ('_BYS') ;             ! South boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do i=1,dims(1)
               if (time_level < 2 .and. .not. var4d_lbc) &
               scnd3d(i,k,l)=frst3d(i,k,l)+tend3d(i,k,l)*bdyfrq
               if ( var4d_lbc )scnd3d(i,k,l)=full3d2(i,l,k)
               frst3d(i,k,l)=full3d(i,l,k)
            end do
            end do
            end do
         case ('_BYE') ;             ! North boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do i=1,dims(1)
               if (time_level < 2 .and. .not. var4d_lbc) &
               scnd3d(i,k,l)=frst3d(i,k,l)+tend3d(i,k,l)*bdyfrq
               if ( var4d_lbc ) scnd3d(i,k,l)=full3d2(i,north_end-l,k)
               frst3d(i,k,l)=full3d(i,north_end-l,k)
            end do
            end do
            end do
         case default ;
            write(unit=stdout,fmt=*) 'It is impossible here.'
            write(unit=stdout,fmt=*) 'bdyname(', m, ')=', trim(bdyname(m))
            stop
         end select

         write(unit=stdout, fmt='(a, i3, 2a)') &
            'cal. tend: bdyname(', m, ')=', trim(vbt_name)

         ! calculate new tendancy 
         do l=1,dims(3)
            do k=1,dims(2)
               do i=1,dims(1)
                  tend3d(i,k,l)=(scnd3d(i,k,l)-frst3d(i,k,l))/bdyfrq
               end do
            end do
         end do

         if (debug) then
            write(unit=new_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
                 'No.', m, 'Variable: ', trim(vbt_name), &
                 'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

            write(unit=new_unit, fmt='(a, 10i12)') &
                 ' new ', (i, i=1,dims(3))

            do j=1,dims(1)
               write(unit=new_unit, fmt='(i4, 1x, 10e20.7)') &
                     j, (tend3d(j,dims(2)/2,i), i=1,dims(3))
            end do
         end if

         ! output new variable at first time level
         call da_put_var_3d_real_cdf( wrf_bdy_file, trim(var_name), frst3d, &
                                dims(1), dims(2), dims(3), 1, debug)
         call da_put_var_3d_real_cdf( wrf_bdy_file, trim(vbt_name), tend3d, &
                                   dims(1), dims(2), dims(3), 1, debug)

         deallocate(frst3d)
         deallocate(scnd3d)
         deallocate(tend3d)
      end do
      
      deallocate(full3d)
      if ( var4d_lbc ) deallocate(full3d2)
   end do

   deallocate(mu)
   if ( var4d_lbc ) deallocate(mu2)
   deallocate(u)
   if ( var4d_lbc ) deallocate(u2)
   deallocate(v)
   if ( var4d_lbc ) deallocate(v2)

!--------------------- second time level-----------------------------------------
!- for var4d_lbc, we need to update the second time level LBC
   if ( var4d_lbc .and. update_lateral_bdy .and. time_level > 1 ) then

   east_end=0
   north_end=0

   cdfid = ncopn(da_file_02, NCNOWRIT, io_status )

   ! For 2D variables
   ! Get mu, mub, msfu, and msfv


   io_status = nf_inq_varid(cdfid, trim(varsf(n)), varid)
   if (io_status /= 0 ) then
      print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                n, io_status, trim(varsf(n)), " does not exist"
      stop
   endif

   call da_get_dims_cdf( da_file_02, 'MU', dims, &
      ndims, debug)

   allocate(mu(dims(1), dims(2)))

   call da_get_var_2d_real_cdf( da_file_02, &
      'MU', mu, dims(1), dims(2), 1, debug)

   east_end=dims(1)+1
   north_end=dims(2)+1

   if (east_end < 1 .or. north_end < 1) then
      write(unit=stdout, fmt='(a)') 'Wrong data for Boundary.'
      stop
   end if

   write(unit=stdout,fmt='(/a/)') 'Processing the lateral boundary condition:'

   ! boundary variables
   bdyname(1)='_BXS'
   bdyname(2)='_BXE'
   bdyname(3)='_BYS'
   bdyname(4)='_BYE'

   ! boundary tendancy variables
   tenname(1)='_BTXS'
   tenname(2)='_BTXE'
   tenname(3)='_BTYS'
   tenname(4)='_BTYE'

   do m=1,4
      var_name='MU' // trim(bdyname(m))
      vbt_name='MU' // trim(tenname(m))

      call da_get_dims_cdf( wrf_bdy_file, trim(var_name), dims, ndims, debug)

      allocate(frst2d(dims(1), dims(2)))
      allocate(scnd2d(dims(1), dims(2)))
      allocate(tend2d(dims(1), dims(2)))

      ! Get variable at third time level
      if (time_level > 2) then
         call da_get_var_2d_real_cdf( wrf_bdy_file, trim(var_name), scnd2d, &
                                   dims(1), dims(2), 3, debug)
      else
         call da_get_var_2d_real_cdf( wrf_bdy_file, trim(var_name), frst2d, &
                                   dims(1), dims(2), 2, debug)
         call da_get_var_2d_real_cdf( wrf_bdy_file, trim(vbt_name), tend2d, &
                                   dims(1), dims(2), 2, debug)
      end if

      if (debug) then
         write(unit=ori_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
              'No.', m, 'Variable: ', trim(vbt_name), &
              'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

         call da_get_var_2d_real_cdf( wrf_bdy_file, trim(vbt_name), tend2d, &
                                   dims(1), dims(2), 2, debug)

         write(unit=ori_unit, fmt='(a, 10i12)') &
              ' old ', (i, i=1,dims(2))
         do j=1,dims(1)
            write(unit=ori_unit, fmt='(i4, 1x, 10e20.7)') &
                  j, (tend2d(j,i), i=1,dims(2))
         end do
      end if

      ! calculate variable at second time level
      select case(m)
      case (1) ;             ! West boundary
         do l=1,dims(2)
            do j=1,dims(1)
               if (time_level < 3) &
                  scnd2d(j,l)=frst2d(j,l)+tend2d(j,l)*bdyfrq
               frst2d(j,l)=mu(l,j)
            end do
         end do
      case (2) ;             ! East boundary
         do l=1,dims(2)
            do j=1,dims(1)
               if (time_level < 3) &
                  scnd2d(j,l)=frst2d(j,l)+tend2d(j,l)*bdyfrq
               frst2d(j,l)=mu(east_end-l,j)
            end do
         end do
      case (3) ;             ! South boundary
         do l=1,dims(2)
            do i=1,dims(1)
               if (time_level < 3) &
                  scnd2d(i,l)=frst2d(i,l)+tend2d(i,l)*bdyfrq
               frst2d(i,l)=mu(i,l)
            end do
         end do
      case (4) ;             ! North boundary
         do l=1,dims(2)
            do i=1,dims(1)
               if (time_level < 3) &
                  scnd2d(i,l)=frst2d(i,l)+tend2d(i,l)*bdyfrq
               frst2d(i,l)=mu(i,north_end-l)
            end do
         end do
      case default ;
         write(unit=stdout,fmt=*) 'It is impossible here. mu, m=', m
      end select

      ! calculate new tendancy 
      do l=1,dims(2)
         do i=1,dims(1)
            tend2d(i,l)=(scnd2d(i,l)-frst2d(i,l))/bdyfrq
         end do
      end do

      if (debug) then
         write(unit=new_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
              'No.', m, 'Variable: ', trim(vbt_name), &
              'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

         write(unit=new_unit, fmt='(a, 10i12)') &
              ' new ', (i, i=1,dims(2))

         do j=1,dims(1)
            write(unit=new_unit, fmt='(i4, 1x, 10e20.7)') &
                  j, (tend2d(j,i), i=1,dims(2))
         end do
      end if

      ! output new variable at first time level
      call da_put_var_2d_real_cdf( wrf_bdy_file, trim(var_name), frst2d, &
                                dims(1), dims(2), 2, debug)
      ! output new tendancy 
      call da_put_var_2d_real_cdf( wrf_bdy_file, trim(vbt_name), tend2d, &
                                dims(1), dims(2), 2, debug)

      deallocate(frst2d)
      deallocate(scnd2d)
      deallocate(tend2d)
   end do

   !---------------------------------------------------------------------
   ! For 3D variables

   ! Get U
   call da_get_dims_cdf( da_file_02, 'U', dims, ndims, debug)

   ! call da_get_att_cdf( da_file_02, 'U', debug)

   allocate(u(dims(1), dims(2), dims(3)))

   ids=1
   ide=dims(1)-1
   jds=1
   jde=dims(2)
   kds=1
   kde=dims(3)

   call da_get_var_3d_real_cdf( da_file_02, 'U', u, &
                             dims(1), dims(2), dims(3), 1, debug)

   ! Get V
   call da_get_dims_cdf( da_file_02, 'V', dims, ndims, debug)

   allocate(v(dims(1), dims(2), dims(3)))

   call da_get_var_3d_real_cdf( da_file_02, 'V', v, &
                             dims(1), dims(2), dims(3), 1, debug)

   if (debug) then
      write(unit=stdout, fmt='(a,e20.12,4x)') &
           'Before couple Sample u=', u(dims(1)/2,dims(2)/2,dims(3)/2), &
           'Before couple Sample v=', v(dims(1)/2,dims(2)/2,dims(3)/2)
   end if

   !---------------------------------------------------------------------
   ! Couple u, v.
   call da_couple_uv ( u, v, mu, mub, msfu, msfv, c1h, c2h, ids, ide, jds, jde, kds, kde)

   if (debug) then
      write(unit=stdout, fmt='(a,e20.12,4x)') &
           'After  couple Sample u=', u(dims(1)/2,dims(2)/2,dims(3)/2), &
           'After  couple Sample v=', v(dims(1)/2,dims(2)/2,dims(3)/2)
   end if

   !---------------------------------------------------------------------
   !For 3D variables

   do n=1,num3d
      write(unit=stdout, fmt='(a, i3, 2a)') 'Processing: var3d(', n, ')=', trim(var3d(n))

      call da_get_dims_cdf( da_file_02, trim(var3d(n)), dims, ndims, debug)

      allocate(full3d(dims(1), dims(2), dims(3)))

      east_end=dims(1)+1
      north_end=dims(2)+1

      select case(trim(var3d(n)))
      case ('U') ;           ! U
         ! var_pref='R' // trim(var3d(n))
         var_pref=trim(var3d(n))
         full3d(:,:,:)=u(:,:,:)
      case ('V') ;           ! V 
         ! var_pref='R' // trim(var3d(n))
         var_pref=trim(var3d(n))
         full3d(:,:,:)=v(:,:,:)
      case ('W') ;
         ! var_pref = 'R' // trim(var3d(n))
         var_pref = trim(var3d(n))

         call da_get_var_3d_real_cdf( da_file_02, trim(var3d(n)), &
            full3d, dims(1), dims(2), dims(3), 1, debug)

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'Before couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if

         do k=1,dims(3)
            do j=1,dims(2)
               do i=1,dims(1)
                  full3d(i,j,k)=full3d(i,j,k)*(c1f(k)*(mu(i,j)+mub(i,j))+c2f(k))/msfm(i,j)
               end do
            end do
         end do

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'After  couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if
      case ('T', 'PH') ;
         var_pref=trim(var3d(n))
 
         if ( use_theta_m > 0 .and. trim(var3d(n)) == 'T' ) then
           call da_get_var_3d_real_cdf( da_file_02, 'THM', &
              full3d, dims(1), dims(2), dims(3), 1, debug)
         else
           call da_get_var_3d_real_cdf( da_file_02, trim(var3d(n)), &
              full3d, dims(1), dims(2), dims(3), 1, debug)
         end if

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'Before couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if

         if ( dims(3) == nlevf ) then
            ! applying c1f and c2f for mu
            do k=1,dims(3)
               do j=1,dims(2)
                  do i=1,dims(1)
                     full3d(i,j,k)=full3d(i,j,k)*(c1f(k)*(mu(i,j)+mub(i,j))+c2f(k))
                  end do
               end do
            end do
         else if ( dims(3) == nlevh ) then
            ! applying c1h and c2h for mu
            do k=1,dims(3)
               do j=1,dims(2)
                  do i=1,dims(1)
                     full3d(i,j,k)=full3d(i,j,k)*(c1h(k)*(mu(i,j)+mub(i,j))+c2h(k))
                  end do
               end do
            end do
         else
            write(unit=stdout,fmt=*) 'Error finding matched dimensions.'
            stop
         end if

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'After  couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if
      case ('QVAPOR', 'QCLOUD', 'QRAIN', 'QICE', 'QSNOW', 'QGRAUP') ;
         ! var_pref='R' // var3d(n)(1:2)
         ! var_pref=var3d(n)(1:2)
         var_pref=var3d(n)
 
         call da_get_var_3d_real_cdf( da_file_02, trim(var3d(n)), &
            full3d, dims(1), dims(2), dims(3), 1, debug)

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'Before couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if

         do k=1,dims(3)
            do j=1,dims(2)
               do i=1,dims(1)
                  full3d(i,j,k)=full3d(i,j,k)*(c1h(k)*(mu(i,j)+mub(i,j))+c2h(k))
               end do
            end do
         end do

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'After  couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if
      case default ;
         write(unit=stdout,fmt=*) 'It is impossible here. var3d(', n, ')=', trim(var3d(n))
      end select

      do m=1,4
         var_name=trim(var_pref) // trim(bdyname(m))
         vbt_name=trim(var_pref) // trim(tenname(m))

         write(unit=stdout, fmt='(a, i3, 2a)') &
            'Processing: bdyname(', m, ')=', trim(var_name)

         call da_get_dims_cdf( wrf_bdy_file, trim(var_name), dims, ndims, debug)

         allocate(frst3d(dims(1), dims(2), dims(3)))
         allocate(scnd3d(dims(1), dims(2), dims(3)))
         allocate(tend3d(dims(1), dims(2), dims(3)))

         ! Get variable at second time level
         if (time_level > 2) then
            call da_get_var_3d_real_cdf( wrf_bdy_file, trim(var_name), scnd3d, &
                                      dims(1), dims(2), dims(3), 3, debug)
         else
            call da_get_var_3d_real_cdf( wrf_bdy_file, trim(var_name), frst3d, &
                                      dims(1), dims(2), dims(3), 2, debug)
            call da_get_var_3d_real_cdf( wrf_bdy_file, trim(vbt_name), tend3d, &
                                      dims(1), dims(2), dims(3), 2, debug)
         end if

         if (debug) then
            write(unit=ori_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
                 'No.', m, 'Variable: ', trim(vbt_name), &
                 'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

            call da_get_var_3d_real_cdf( wrf_bdy_file, trim(vbt_name), tend3d, &
                                      dims(1), dims(2), dims(3), 2, debug)

            write(unit=ori_unit, fmt='(a, 10i12)') &
                 ' old ', (i, i=1,dims(3))
            do j=1,dims(1)
               write(unit=ori_unit, fmt='(i4, 1x, 10e20.7)') &
                     j, (tend3d(j,dims(2)/2,i), i=1,dims(3))
            end do
         end if
   
         select case(trim(bdyname(m)))
         case ('_BXS') ;             ! West boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do j=1,dims(1)
               if (time_level < 3) &
               scnd3d(j,k,l)=frst3d(j,k,l)+tend3d(j,k,l)*bdyfrq
               frst3d(j,k,l)=full3d(l,j,k)
            end do
            end do
            end do
         case ('_BXE') ;             ! East boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do j=1,dims(1)
               if (time_level < 3) &
               scnd3d(j,k,l)=frst3d(j,k,l)+tend3d(j,k,l)*bdyfrq
               frst3d(j,k,l)=full3d(east_end-l,j,k)
            end do
            end do
            end do
         case ('_BYS') ;             ! South boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do i=1,dims(1)
               if (time_level < 3) &
               scnd3d(i,k,l)=frst3d(i,k,l)+tend3d(i,k,l)*bdyfrq
               frst3d(i,k,l)=full3d(i,l,k)
            end do
            end do
            end do
         case ('_BYE') ;             ! North boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do i=1,dims(1)
               if (time_level < 3) &
               scnd3d(i,k,l)=frst3d(i,k,l)+tend3d(i,k,l)*bdyfrq
               frst3d(i,k,l)=full3d(i,north_end-l,k)
            end do
            end do
            end do
         case default ;
            write(unit=stdout,fmt=*) 'It is impossible here.'
            write(unit=stdout,fmt=*) 'bdyname(', m, ')=', trim(bdyname(m))
            stop
         end select

         write(unit=stdout, fmt='(a, i3, 2a)') &
            'cal. tend: bdyname(', m, ')=', trim(vbt_name)

         ! calculate new tendancy 
         do l=1,dims(3)
            do k=1,dims(2)
               do i=1,dims(1)
                  tend3d(i,k,l)=(scnd3d(i,k,l)-frst3d(i,k,l))/bdyfrq
               end do
            end do
         end do

         if (debug) then
            write(unit=new_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
                 'No.', m, 'Variable: ', trim(vbt_name), &
                 'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

            write(unit=new_unit, fmt='(a, 10i12)') &
                 ' new ', (i, i=1,dims(3))

            do j=1,dims(1)
               write(unit=new_unit, fmt='(i4, 1x, 10e20.7)') &
                     j, (tend3d(j,dims(2)/2,i), i=1,dims(3))
            end do
         end if

         ! output new variable at first time level
         call da_put_var_3d_real_cdf( wrf_bdy_file, trim(var_name), frst3d, &
                                dims(1), dims(2), dims(3), 2, debug)
         call da_put_var_3d_real_cdf( wrf_bdy_file, trim(vbt_name), tend3d, &
                                   dims(1), dims(2), dims(3), 2, debug)

         deallocate(frst3d)
         deallocate(scnd3d)
         deallocate(tend3d)
      end do
      
      deallocate(full3d)
   end do

   deallocate(mu)
   deallocate(u)
   deallocate(v)

   end if ! end of update second time level LBC for var4d_lbc

   deallocate(mub)
   deallocate(msfu)
   deallocate(msfv)
   deallocate(times)
   deallocate(thisbdytime)
   deallocate(nextbdytime)

 end if ! end if update_lateral_bdy

 deallocate ( c1f )
 deallocate ( c2f )
 deallocate ( c1h )
 deallocate ( c2h )

 write(unit=stdout,fmt=*) &
    '=================================================================='
 if ( update_lateral_bdy ) then
    write(unit=stdout,fmt=*) 'Lateral boundary tendency updated.'
 end if
 if ( update_low_bdy ) then
    write(unit=stdout,fmt=*) 'Low boundary updated with wrf_input fields.'
 end if
 if ( update_lsm ) then
    write(unit=stdout,fmt=*) 'LSM variables updated with wrf_input fields.'
 end if

   if (io_status == 0) &
      write (unit=stdout,fmt=*) "*** Update_bc completed successfully ***"

end program da_update_bc

