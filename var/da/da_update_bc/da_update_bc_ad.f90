program da_update_bc_ad

   !-----------------------------------------------------------------------
   ! Purpose: adjoint version of update BC file from wrfvar output.
   ! current version reads only wrf-netcdf file format
   !
   ! Xin Zhang, 12/12/2011:
   !            for 3DVAR use only with FSO 
   !
   !-----------------------------------------------------------------------

   use da_netcdf_interface, only : da_get_var_3d_real_cdf, &
      da_put_var_3d_real_cdf, da_get_dims_cdf, da_put_var_2d_real_cdf, &
      da_get_var_2d_real_cdf, da_get_var_2d_int_cdf, da_get_bdytimestr_cdf, &
      da_get_times_cdf, da_get_bdyfrq, stderr, stdout, da_put_var_2d_int_cdf

   use da_module_couple_uv_ad, only : da_couple_uv_b

   implicit none

   include 'netcdf.inc'

   integer, parameter :: max_3d_variables = 5, &
                         max_2d_variables = 4
 
   character(len=512) :: wrfinput,      &
                         init_sens,     &
                         wrf_bdy_file,  &
                         bdy_sens
 
   character(len=20) :: var_pref, var_name, vbt_name

   character(len=20) :: var3d(max_3d_variables), &
                        varsf(max_2d_variables)

   character(len=10), dimension(4) :: bdyname, tenname

   integer           :: ids, ide, jds, jde, kds, kde
   integer           :: num3d, num2d, ndims
   integer           :: time_level
   integer           :: i,j,k,l,m,n

   integer, dimension(4) :: dims
 
   real, allocatable, dimension(:,:,:) :: tend3d, scnd3d, frst3d, full3d, full3d9

   real, allocatable, dimension(:,:,:) :: u, v
   real, allocatable, dimension(:,:,:) :: a_u, a_v

   real, allocatable, dimension(:,  :) :: mu, a_mu, mub, msfu, msfv, &
                                          tend2d, frst2d

   character(len=80), allocatable, dimension(:) :: times, &
                                                   thisbdytime, nextbdytime
 
   integer :: east_end, north_end, io_status, cdfid, varid
   integer :: iostatus(4)

   logical :: debug, update_lateral_bdy

   real :: bdyfrq

   integer, parameter :: namelist_unit = 7

   namelist /control_param/ wrfinput,      &
                            init_sens,    &
                            wrf_bdy_file, &
                            bdy_sens, &
                            debug, update_lateral_bdy
                           

   wrfinput           = 'wrfinput_d01'
   init_sens          = 'init_sens_d01'
   wrf_bdy_file       = 'wrfbdy_d01'
   bdy_sens           = 'gradient_wrfbdy_d01'

   debug              = .false. 
   update_lateral_bdy = .true.

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
           'wrfinput       = ', trim(wrfinput), &
           'init_sens     = ', trim(init_sens), &
           'wrf_bdy_file  = ', trim(wrf_bdy_file), &
           'bdy_sens  = ', trim(bdy_sens)

      WRITE(unit=stdout, fmt='(2(a, L10))')             &
           'update_lateral_bdy = ', update_lateral_bdy

      close(unit=namelist_unit)
   end if

   ! 3D need update
   num3d=5
   var3d(1)='A_U'
   var3d(2)='A_V'
   var3d(3)='A_T'
   var3d(4)='A_PH'
   var3d(5)='A_QVAPOR'

   ! 2D need update
   num2d=4
   varsf(1)='MUB'
   varsf(2)='MU'
   varsf(3)='MAPFAC_U'
   varsf(4)='MAPFAC_V'

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

   cdfid = ncopn(wrfinput, NCNOWRIT, io_status )

!---------------------------------------------------------------
   ! For 2D variables
   ! Get mu, mub, msfu, and msfv

   do n=1,num2d

      io_status = nf_inq_varid(cdfid, trim(varsf(n)), varid)
      if (io_status /= 0 ) then
         print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                   n, io_status, trim(varsf(n)), " does not exist"
         cycle
      endif

      call da_get_dims_cdf( wrfinput, trim(varsf(n)), dims, &
         ndims, debug)

      select case(trim(varsf(n)))
      case ('MU') ;
         if ( .not. update_lateral_bdy ) cycle

         allocate(mu(dims(1), dims(2)))
         allocate(a_mu(dims(1), dims(2)))
         a_mu=0.0

         call da_get_var_2d_real_cdf( wrfinput, &
            trim(varsf(n)), mu, dims(1), dims(2), 1, debug)

      case ('MUB') ;
         if ( .not. update_lateral_bdy ) cycle

         allocate(mub(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( wrfinput, trim(varsf(n)), mub, &
                                   dims(1), dims(2), 1, debug)
      case ('MAPFAC_U') ;
         if ( .not. update_lateral_bdy ) cycle

         allocate(msfu(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( wrfinput, trim(varsf(n)), msfu, &
                                   dims(1), dims(2), 1, debug)
      case ('MAPFAC_V') ;
         if ( .not. update_lateral_bdy ) cycle

         allocate(msfv(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( wrfinput, trim(varsf(n)), msfv, &
                                   dims(1), dims(2), 1, debug)

      case default ;
            write(unit=stdout,fmt=*) 'It is impossible here. varsf(n)=', trim(varsf(n))
      end select
   end do


   ! Get U
   call da_get_dims_cdf( wrfinput, 'U', dims, ndims, debug)

   allocate(u(dims(1), dims(2), dims(3)))
   allocate(a_u(dims(1), dims(2), dims(3)))
   a_u=0.0

   ids=1
   ide=dims(1)-1
   jds=1
   jde=dims(2)
   kds=1
   kde=dims(3)

   call da_get_var_3d_real_cdf( wrfinput, 'U', u, &
                             dims(1), dims(2), dims(3), 1, debug)

   ! Get V
   call da_get_dims_cdf( wrfinput, 'V', dims, ndims, debug)

   allocate(v(dims(1), dims(2), dims(3)))
   allocate(a_v(dims(1), dims(2), dims(3)))
   a_v=0.0

   call da_get_var_3d_real_cdf( wrfinput, 'V', v, &
                             dims(1), dims(2), dims(3), 1, debug)

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

   !For 3D variables

   do n=1,num3d
      write(unit=stdout, fmt='(a, i3, 2a)') 'Processing: var3d(', n, ')=', trim(var3d(n))

      call da_get_dims_cdf( init_sens, trim(var3d(n)), dims, ndims, debug)

      allocate(full3d(dims(1), dims(2), dims(3)))
      full3d=0.0

      allocate(full3d9(dims(1), dims(2), dims(3)))

      east_end=dims(1)+1
      north_end=dims(2)+1

      var_pref=trim(var3d(n))

      do m=1,4
         var_name=trim(var_pref) // trim(bdyname(m))
         vbt_name=trim(var_pref) // trim(tenname(m))

         write(unit=stdout, fmt='(a, i3, 2a)') &
            'Processing: bdyname(', m, ')=', trim(var_name)

         call da_get_dims_cdf( bdy_sens, trim(var_name), dims, ndims, debug)

         allocate(frst3d(dims(1), dims(2), dims(3)))
         allocate(tend3d(dims(1), dims(2), dims(3)))

         write(unit=stdout, fmt='(a, i3, 2a)') &
            'adjoint cal. tend: bdyname(', m, ')=', trim(vbt_name)

         ! output new variable at first time level
         call da_get_var_3d_real_cdf( bdy_sens, trim(var_name), frst3d, &
                                   dims(1), dims(2), dims(3), 1, debug)
         call da_get_var_3d_real_cdf( bdy_sens, trim(vbt_name), tend3d, &
                                   dims(1), dims(2), dims(3), 1, debug)

         ! calculate new tendancy 
         do l=1,dims(3)
            do k=1,dims(2)
               do i=1,dims(1)
                  frst3d(i,k,l)=frst3d(i,k,l)-tend3d(i,k,l)/bdyfrq
                  tend3d(i,k,l)=0.0
               end do
            end do
         end do

         select case(trim(bdyname(m)))
         case ('_BXS') ;             ! West boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do j=1,dims(1)
               full3d(l,j,k)=full3d(l,j,k)+frst3d(j,k,l)
               frst3d(j,k,l)=0.0
            end do
            end do
            end do
         case ('_BXE') ;             ! East boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do j=1,dims(1)
               full3d(east_end-l,j,k)=full3d(east_end-l,j,k)+frst3d(j,k,l)
               frst3d(j,k,l)=0.0
            end do
            end do
            end do
         case ('_BYS') ;             ! South boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do i=1,dims(1)
               full3d(i,l,k)=full3d(i,l,k)+frst3d(i,k,l)
               frst3d(i,k,l)=0.0
            end do
            end do
            end do
         case ('_BYE') ;             ! North boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do i=1,dims(1)
               full3d(i,north_end-l,k)=full3d(i,north_end-l,k)+frst3d(i,k,l)
               frst3d(i,k,l)=0.0
            end do
            end do
            end do
         case default ;
            write(unit=stdout,fmt=*) 'It is impossible here.'
            write(unit=stdout,fmt=*) 'bdyname(', m, ')=', trim(bdyname(m))
            stop
         end select

         deallocate(frst3d)
         deallocate(tend3d)
      end do
      
      select case(trim(var3d(n)))
      case ('A_U') ;           ! U
         var_pref=trim(var3d(n))
         a_u(:,:,:)=a_u(:,:,:)+full3d(:,:,:)
         full3d(:,:,:)=0.0
      case ('A_V') ;           ! V 
         var_pref=trim(var3d(n))
         a_v(:,:,:)=a_v(:,:,:)+full3d(:,:,:)
         full3d(:,:,:)=0.0
      case ('A_T') ;
         var_pref=trim(var3d(n))
 
         call da_get_dims_cdf( init_sens, trim(var3d(n)), dims, ndims, debug)
         call da_get_var_3d_real_cdf( wrfinput, 'T', &
            full3d9, dims(1), dims(2), dims(3), 1, debug)

         do k=1,dims(3)
            do j=1,dims(2)
               do i=1,dims(1)
                  a_mu(i,j)=a_mu(i,j)+full3d(i,j,k)*full3d9(i,j,k)
                  full3d(i,j,k)=full3d(i,j,k)*(mu(i,j)+mub(i,j))
               end do
            end do
         end do

         call da_get_var_3d_real_cdf( init_sens, 'A_T', &
            full3d9, dims(1), dims(2), dims(3), 1, debug)

         full3d=full3d+full3d9
         full3d9=0.0

         call da_put_var_3d_real_cdf( init_sens, trim(var3d(n)), &
            full3d, dims(1), dims(2), dims(3), 1, debug)

         full3d=0.0

      case ('A_PH') ;
         var_pref=trim(var3d(n))
 
         call da_get_dims_cdf( init_sens, trim(var3d(n)), dims, ndims, debug)
         call da_get_var_3d_real_cdf( wrfinput, 'PH', &
            full3d9, dims(1), dims(2), dims(3), 1, debug)

         do k=1,dims(3)
            do j=1,dims(2)
               do i=1,dims(1)
                  a_mu(i,j)=a_mu(i,j)+full3d(i,j,k)*full3d9(i,j,k)
                  full3d(i,j,k)=full3d(i,j,k)*(mu(i,j)+mub(i,j))
               end do
            end do
         end do

         call da_get_var_3d_real_cdf( init_sens, 'A_PH', &
            full3d9, dims(1), dims(2), dims(3), 1, debug)

         full3d=full3d+full3d9
         full3d9=0.0

         call da_put_var_3d_real_cdf( init_sens, trim(var3d(n)), &
            full3d, dims(1), dims(2), dims(3), 1, debug)

         full3d=0.0

      case ('A_QVAPOR') ;
         var_pref=var3d(n)
 
         call da_get_dims_cdf( init_sens, trim(var3d(n)), dims, ndims, debug)
         call da_get_var_3d_real_cdf( wrfinput, 'QVAPOR', &
            full3d9, dims(1), dims(2), dims(3), 1, debug)

         do k=1,dims(3)
            do j=1,dims(2)
               do i=1,dims(1)
                  a_mu(i,j)=a_mu(i,j)+full3d(i,j,k)*full3d9(i,j,k)
                  full3d(i,j,k)=full3d(i,j,k)*(mu(i,j)+mub(i,j))
               end do
            end do
         end do

         call da_get_var_3d_real_cdf( init_sens, 'A_QVAPOR', &
            full3d9, dims(1), dims(2), dims(3), 1, debug)

         full3d=full3d+full3d9
         full3d9=0.0

         call da_put_var_3d_real_cdf( init_sens, trim(var3d(n)), &
            full3d, dims(1), dims(2), dims(3), 1, debug)

         full3d=0.0

      case default ;
         write(unit=stdout,fmt=*) 'It is impossible here. var3d(', n, ')=', trim(var3d(n))
      end select

      deallocate(full3d)
      deallocate(full3d9)

   end do

   !---------------------------------------------------------------------
   ! Couple u, v.
   call da_couple_uv_b ( u, a_u, v, a_v, mu, a_mu, mub, msfu, msfv, ids, ide, jds, jde, kds, kde)

   call da_get_dims_cdf( init_sens, 'A_V', dims, ndims, debug)
   call da_get_var_3d_real_cdf( init_sens, 'A_V', v, &
                             dims(1), dims(2), dims(3), 1, debug)
   a_v=a_v+v
   v=0.0
   call da_put_var_3d_real_cdf( init_sens, 'A_V', a_v, &
                             dims(1), dims(2), dims(3), 1, debug)
   a_v=0.0

   call da_get_dims_cdf( init_sens, 'A_U', dims, ndims, debug)
   call da_get_var_3d_real_cdf( init_sens, 'A_U', u, &
                             dims(1), dims(2), dims(3), 1, debug)
   a_u=a_u+u
   u=0.0
   call da_put_var_3d_real_cdf( init_sens, 'A_U', a_u, &
                             dims(1), dims(2), dims(3), 1, debug)
   a_u=0.0

   call da_get_dims_cdf( wrfinput, 'MU', dims, &
      ndims, debug)

   east_end=dims(1)+1
   north_end=dims(2)+1

   do m=1,4
      var_name='A_MU' // trim(bdyname(m))
      vbt_name='A_MU' // trim(tenname(m))

      call da_get_dims_cdf( bdy_sens, trim(var_name), dims, ndims, debug)

      allocate(frst2d(dims(1), dims(2)))
      allocate(tend2d(dims(1), dims(2)))

      ! get new variable at first time level
      call da_get_var_2d_real_cdf( bdy_sens, trim(var_name), frst2d, &
                                dims(1), dims(2), 1, debug)
      ! output new tendancy 
      call da_get_var_2d_real_cdf( bdy_sens, trim(vbt_name), tend2d, &
                                dims(1), dims(2), 1, debug)

      ! calculate new tendancy 
      do l=1,dims(2)
         do i=1,dims(1)
            frst2d(i,l)=frst2d(i,l)-tend2d(i,l)/bdyfrq
            tend2d(i,l)=0.0
         end do
      end do

      ! calculate variable at first time level
      select case(m)
      case (1) ;             ! West boundary
         do l=1,dims(2)
            do j=1,dims(1)
               a_mu(l,j)=a_mu(l,j)+frst2d(j,l)
               frst2d(j,l)=0.0
            end do
         end do
      case (2) ;             ! East boundary
         do l=1,dims(2)
            do j=1,dims(1)
               a_mu(east_end-l,j)=a_mu(east_end-l,j)+frst2d(j,l)
               frst2d(j,l)=0.0
            end do
         end do
      case (3) ;             ! South boundary
         do l=1,dims(2)
            do i=1,dims(1)
               a_mu(i,l)=a_mu(i,l)+frst2d(i,l)
               frst2d(i,l)=0.0
            end do
         end do
      case (4) ;             ! North boundary
         do l=1,dims(2)
            do i=1,dims(1)
               a_mu(i,north_end-l)=a_mu(i,north_end-l)+frst2d(i,l)
               frst2d(i,l)=0.0
            end do
         end do
      case default ;
         write(unit=stdout,fmt=*) 'It is impossible here. mu, m=', m
      end select

      deallocate(frst2d)
      deallocate(tend2d)
   end do

   call da_get_dims_cdf( init_sens, 'A_MU', dims, &
      ndims, debug)
   call da_get_var_2d_real_cdf( init_sens, &
      'A_MU', mu, dims(1), dims(2), 1, debug)
   a_mu=a_mu+mu
   mu=0.0
   call da_put_var_2d_real_cdf( init_sens, &
      'A_MU', a_mu, dims(1), dims(2), 1, debug)
   a_mu=0.0

   !---------------------------------------------------------------------


   deallocate(mu)
   deallocate(u)
   deallocate(v)
   deallocate(a_mu)
   deallocate(a_u)
   deallocate(a_v)
   deallocate(mub)
   deallocate(msfu)
   deallocate(msfv)

   deallocate(times)
   deallocate(thisbdytime)
   deallocate(nextbdytime)

   write (unit=stdout,fmt=*) "*** Adjoint of update_bc completed successfully ***"

end program da_update_bc_ad

