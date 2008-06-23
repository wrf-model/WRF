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
      da_get_times_cdf, da_get_bdyfrq, stderr, stdout

   use da_module_couple_uv, only : da_couple_uv

   implicit none

   include 'netcdf.inc'

   integer, parameter :: max_3d_variables = 20, &
                         max_2d_variables = 20
 
   character(len=512) :: wrfvar_output_file, &
                         wrf_bdy_file, &
                         wrf_input
 
   character(len=20) :: var_pref, var_name, vbt_name

   character(len=20) :: var3d(max_3d_variables), &
                        varsf(max_2d_variables)

   character(len=10), dimension(4) :: bdyname, tenname

   integer           :: ids, ide, jds, jde, kds, kde
   integer           :: num3d, num2d, ndims
   integer           :: time_level
   integer           :: i,j,k,l,m,n

   integer, dimension(4) :: dims
 
   real, allocatable, dimension(:,:,:) :: tend3d, scnd3d, frst3d, full3d

   real, allocatable, dimension(:,:,:) :: u, v

   real, allocatable, dimension(:,  :) :: mu, mub, msfu, msfv, msfm, &
                                          tend2d, scnd2d, frst2d, full2d

   real, allocatable, dimension(:,  :) :: tsk, tsk_wrfvar

   integer, allocatable, dimension(:,:) :: ivgtyp

   character(len=80), allocatable, dimension(:) :: times, &
                                                   thisbdytime, nextbdytime
 
   integer :: east_end, north_end, io_status, cdfid, varid

   logical :: cycling, debug, low_bdy_only, update_lsm

   real :: bdyfrq

   integer, parameter :: namelist_unit = 7, &
                         ori_unit = 11, &
                         new_unit = 12

   namelist /control_param/ wrfvar_output_file, &
                            wrf_bdy_file, &
                            wrf_input, &
                            cycling, debug, low_bdy_only, update_lsm

   wrfvar_output_file = 'wrfvar_output'
   wrf_bdy_file       = 'wrfbdy_d01'
   wrf_input          = 'wrfinput_d01'

   cycling = .false.
   debug   = .false. 
   low_bdy_only = .false.
   update_lsm = .false.
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

      WRITE(unit=stdout, fmt='(2a)') &
           'wrfvar_output_file = ', trim(wrfvar_output_file), &
           'wrf_bdy_file          = ', trim(wrf_bdy_file), &
           'wrf_input     = ', trim(wrf_input)

      WRITE(unit=stdout, fmt='(a, L10)') &
           'cycling = ', cycling

      close(unit=namelist_unit)
   end if

   ! 3D need update
   num3d=6
   var3d(1)='U'
   var3d(2)='V'
   var3d(3)='W'
   var3d(4)='T'
   var3d(5)='PH'
   var3d(6)='QVAPOR'

   ! 2D need update
   num2d=18
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

   east_end=0
   north_end=0

   cdfid = ncopn(wrfvar_output_file, NCNOWRIT, io_status )

   ! For 2D variables
   ! Get mu, mub, msfu, and msfv

   do n=1,num2d

      io_status = nf_inq_varid(cdfid, trim(varsf(n)), varid)
      if (io_status /= 0 ) then
         print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                   n, io_status, trim(varsf(n)), " not existed"
         cycle
      endif

      call da_get_dims_cdf( wrfvar_output_file, trim(varsf(n)), dims, &
         ndims, debug)

      select case(trim(varsf(n)))
      case ('MU') ;
         if (low_bdy_only) cycle

         allocate(mu(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( wrfvar_output_file, &
            trim(varsf(n)), mu, dims(1), dims(2), 1, debug)

         east_end=dims(1)+1
         north_end=dims(2)+1
      case ('MUB') ;
         if (low_bdy_only) cycle

         allocate(mub(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( wrfvar_output_file, trim(varsf(n)), mub, &
                                   dims(1), dims(2), 1, debug)
      case ('MAPFAC_U') ;
         if (low_bdy_only) cycle

         allocate(msfu(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( wrfvar_output_file, trim(varsf(n)), msfu, &
                                   dims(1), dims(2), 1, debug)
      case ('MAPFAC_V') ;
         if (low_bdy_only) cycle

         allocate(msfv(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( wrfvar_output_file, trim(varsf(n)), msfv, &
                                   dims(1), dims(2), 1, debug)
      case ('MAPFAC_M') ;
         if (low_bdy_only) cycle

         allocate(msfm(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( wrfvar_output_file, trim(varsf(n)), msfm, &
                                   dims(1), dims(2), 1, debug)
      case ('TSK') ;
         if (.not. cycling .and. .not.low_bdy_only) cycle

         allocate(tsk(dims(1), dims(2)))
         allocate(tsk_wrfvar(dims(1), dims(2)))
         allocate(ivgtyp(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( wrf_input, trim(varsf(n)), tsk, &
                                   dims(1), dims(2), 1, debug)
         call da_get_var_2d_real_cdf( wrfvar_output_file, trim(varsf(n)), tsk_wrfvar, &
                                   dims(1), dims(2), 1, debug)
         call da_get_var_2d_int_cdf( wrfvar_output_file, 'IVGTYP', ivgtyp, &
                                   dims(1), dims(2), 1, debug)

         ! update TSK.
         do j=1,dims(2)
         do i=1,dims(1)
               if (ivgtyp(i,j) /= 16) &
                  tsk(i,j)=tsk_wrfvar(i,j)
            end do
            end do

            call da_put_var_2d_real_cdf( wrfvar_output_file, trim(varsf(n)), tsk, &
                                      dims(1), dims(2), 1, debug)
            deallocate(tsk)
            deallocate(ivgtyp)
            deallocate(tsk_wrfvar)

         case ('TMN', 'SST', 'VEGFRA', 'ALBBCK', 'SEAICE') ;
            if (.not. cycling .and. .not.low_bdy_only) cycle

            allocate(full2d(dims(1), dims(2)))

            call da_get_var_2d_real_cdf( wrf_input, trim(varsf(n)), full2d, &
                                      dims(1), dims(2), 1, debug)

            call da_put_var_2d_real_cdf( wrfvar_output_file, trim(varsf(n)), full2d, &
                                      dims(1), dims(2), 1, debug)
            deallocate(full2d)

         case ('SNOW', 'CANWAT', 'RHOSN', 'SNOWH') ;
            if(.not. cycling .and. .not.low_bdy_only) cycle
            if (update_lsm) then
               allocate(full2d(dims(1), dims(2)))

               call da_get_var_2d_real_cdf( wrf_input, trim(varsf(n)), full2d, &
                                      dims(1), dims(2), 1, debug )
!               print *,"sum(full2d^2)=", sum(full2d*full2d)

               call da_put_var_2d_real_cdf( wrfvar_output_file, trim(varsf(n)), full2d, &
                                      dims(1), dims(2), 1, debug )
               deallocate(full2d)
            endif

         case ('TSLB', 'SMOIS', 'SH2O') ;
            if(.not. cycling .and. .not.low_bdy_only) cycle
            if (update_lsm) then
               allocate(full3d(dims(1), dims(2), dims(3)))

               call da_get_var_3d_real_cdf( wrf_input, trim(varsf(n)), full3d, &
                                      dims(1), dims(2), dims(3), 1, debug )
!               print *,"sum(full3d^2)=", sum(full3d*full3d)

               call da_put_var_3d_real_cdf( wrfvar_output_file, trim(varsf(n)), full3d, &
                                      dims(1), dims(2), dims(3), 1, debug )
               deallocate(full3d)

            endif

         case default ;
            write(unit=stdout,fmt=*) 'It is impossible here. varsf(n)=', trim(varsf(n))
      end select
   end do
  
   if (low_bdy_only) goto 9999

   if (east_end < 1 .or. north_end < 1) then
      write(unit=stdout, fmt='(a)') 'Wrong data for Boundary.'
      stop
   end if

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
      if (time_level > 1) then
         call da_get_var_2d_real_cdf( wrf_bdy_file, trim(var_name), scnd2d, &
                                   dims(1), dims(2), 2, debug)
      else
         call da_get_var_2d_real_cdf( wrf_bdy_file, trim(var_name), frst2d, &
                                   dims(1), dims(2), 1, debug)
         call da_get_var_2d_real_cdf( wrf_bdy_file, trim(vbt_name), tend2d, &
                                   dims(1), dims(2), 1, debug)
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
            write(unit=ori_unit, fmt='(i4, 1x, 5e20.7)') &
                  j, (tend2d(j,i), i=1,dims(2))
         end do
      end if

      ! calculate variable at first time level
      select case(m)
      case (1) ;             ! West boundary
         do l=1,dims(2)
            do j=1,dims(1)
               if (time_level < 2) &
               scnd2d(j,l)=frst2d(j,l)+tend2d(j,l)*bdyfrq
               frst2d(j,l)=mu(l,j)
            end do
         end do
      case (2) ;             ! East boundary
         do l=1,dims(2)
            do j=1,dims(1)
               if (time_level < 2) &
                  scnd2d(j,l)=frst2d(j,l)+tend2d(j,l)*bdyfrq
               frst2d(j,l)=mu(east_end-l,j)
            end do
         end do
      case (3) ;             ! South boundary
         do l=1,dims(2)
            do i=1,dims(1)
               if (time_level < 2) &
                  scnd2d(i,l)=frst2d(i,l)+tend2d(i,l)*bdyfrq
               frst2d(i,l)=mu(i,l)
            end do
         end do
      case (4) ;             ! North boundary
         do l=1,dims(2)
            do i=1,dims(1)
               if (time_level < 2) &
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
            write(unit=new_unit, fmt='(i4, 1x, 5e20.7)') &
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
   call da_get_dims_cdf( wrfvar_output_file, 'U', dims, ndims, debug)

   ! call da_get_att_cdf( wrfvar_output_file, 'U', debug)

   allocate(u(dims(1), dims(2), dims(3)))

   ids=1
   ide=dims(1)-1
   jds=1
   jde=dims(2)
   kds=1
   kde=dims(3)

   call da_get_var_3d_real_cdf( wrfvar_output_file, 'U', u, &
                             dims(1), dims(2), dims(3), 1, debug)

   ! do j=1,dims(2)
   !    write(unit=stdout, fmt='(2(a,i5), a, f12.8)') &
   !       'u(', dims(1), ',', j, ',1)=', u(dims(1),j,1)
   ! end do

   ! Get V
   call da_get_dims_cdf( wrfvar_output_file, 'V', dims, ndims, debug)

   ! call da_get_att_cdf( wrfvar_output_file, 'V', debug)

   allocate(v(dims(1), dims(2), dims(3)))

   call da_get_var_3d_real_cdf( wrfvar_output_file, 'V', v, &
                             dims(1), dims(2), dims(3), 1, debug)

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
   call da_couple_uv ( u, v, mu, mub, msfu, msfv, ids, ide, jds, jde, kds, kde)

   if (debug) then
      write(unit=stdout, fmt='(a,e20.12,4x)') &
           'After  couple Sample u=', u(dims(1)/2,dims(2)/2,dims(3)/2), &
           'After  couple Sample v=', v(dims(1)/2,dims(2)/2,dims(3)/2)
   end if

   !---------------------------------------------------------------------
   !For 3D variables

   do n=1,num3d
      write(unit=stdout, fmt='(a, i3, 2a)') 'Processing: var3d(', n, ')=', trim(var3d(n))

      call da_get_dims_cdf( wrfvar_output_file, trim(var3d(n)), dims, ndims, debug)

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

         call da_get_var_3d_real_cdf( wrfvar_output_file, trim(var3d(n)), &
            full3d, dims(1), dims(2), dims(3), 1, debug)

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'Before couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if

         do k=1,dims(3)
            do j=1,dims(2)
               do i=1,dims(1)
                  full3d(i,j,k)=full3d(i,j,k)*(mu(i,j)+mub(i,j))/msfm(i,j)
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
 
         call da_get_var_3d_real_cdf( wrfvar_output_file, trim(var3d(n)), &
            full3d, dims(1), dims(2), dims(3), 1, debug)

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'Before couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if

         do k=1,dims(3)
            do j=1,dims(2)
               do i=1,dims(1)
                  full3d(i,j,k)=full3d(i,j,k)*(mu(i,j)+mub(i,j))
               end do
            end do
         end do

            if (debug) then
               write(unit=stdout, fmt='(3a,e20.12,4x)') &
                    'After  couple Sample ', trim(var3d(n)), &
                    '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
            end if
      case ('QVAPOR', 'QCLOUD', 'QRAin', 'QICE', 'QSNOW', 'QGRAUP') ;
         ! var_pref='R' // var3d(n)(1:2)
         ! var_pref=var3d(n)(1:2)
         var_pref=var3d(n)
 
         call da_get_var_3d_real_cdf( wrfvar_output_file, trim(var3d(n)), &
            full3d, dims(1), dims(2), dims(3), 1, debug)

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'Before couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if

         do k=1,dims(3)
            do j=1,dims(2)
               do i=1,dims(1)
                  full3d(i,j,k)=full3d(i,j,k)*(mu(i,j)+mub(i,j))
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
         if (time_level > 1) then
            call da_get_var_3d_real_cdf( wrf_bdy_file, trim(var_name), scnd3d, &
                                      dims(1), dims(2), dims(3), 2, debug)
         else
            call da_get_var_3d_real_cdf( wrf_bdy_file, trim(var_name), frst3d, &
                                      dims(1), dims(2), dims(3), 1, debug)
            call da_get_var_3d_real_cdf( wrf_bdy_file, trim(vbt_name), tend3d, &
                                      dims(1), dims(2), dims(3), 1, debug)
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
               write(unit=ori_unit, fmt='(i4, 1x, 5e20.7)') &
                     j, (tend3d(j,dims(2)/2,i), i=1,dims(3))
            end do
         end if
   
         select case(trim(bdyname(m)))
         case ('_BXS') ;             ! West boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do j=1,dims(1)
               if (time_level < 2) &
               scnd3d(j,k,l)=frst3d(j,k,l)+tend3d(j,k,l)*bdyfrq
               frst3d(j,k,l)=full3d(l,j,k)
            end do
            end do
            end do
         case ('_BXE') ;             ! East boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do j=1,dims(1)
               if (time_level < 2) &
               scnd3d(j,k,l)=frst3d(j,k,l)+tend3d(j,k,l)*bdyfrq
               frst3d(j,k,l)=full3d(east_end-l,j,k)
            end do
            end do
            end do
         case ('_BYS') ;             ! South boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do i=1,dims(1)
               if (time_level < 2) &
               scnd3d(i,k,l)=frst3d(i,k,l)+tend3d(i,k,l)*bdyfrq
               frst3d(i,k,l)=full3d(i,l,k)
            end do
            end do
            end do
         case ('_BYE') ;             ! North boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do i=1,dims(1)
               if (time_level < 2) &
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
               write(unit=new_unit, fmt='(i4, 1x, 5e20.7)') &
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
   end do

   deallocate(mu)
   deallocate(mub)
   deallocate(msfu)
   deallocate(msfv)
   deallocate(u)
   deallocate(v)

 9999  continue

   print *,' '
   print *,'=================================================================='
   if(cycling) then
     print *, &
    'Both low boudary and lateral boundary updated.for cycling run with WRFvar'
     if (update_lsm) print *,'  LSM variables updated from wrf_input file.' 
   else
     if(low_bdy_only) then
       print *, 'Only low boudary updated for long-term forecast.'
       if (update_lsm) print *,'  LSM variables updated from wrf_input file.' 
     else
       print *, 'Only lateral boundary updated for Cold-start run with WRFVar.'
     endif
   endif
   
   if (io_status == 0) &
      write (unit=stdout,fmt=*) "*** Update_bc completed successfully ***"

end program da_update_bc

