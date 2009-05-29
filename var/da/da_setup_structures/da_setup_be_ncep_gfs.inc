subroutine da_setup_be_ncep_gfs( grid, be )
!------------------------------------------------------------------------------
!  PURPOSE: Define and allocate components of background errors for cv_option 3.
!
!  METHOD:  Allocate components in turn.
!
!  HISTORY: 08/02/2002 - Creation of F90 version.           Wan-Shu Wu
!
!  PARENT_MODULE: DA_Setup_Structures
!
!  Modified by Yong-Run Guo,  03/07/2009, for WRFVar 3.1
!
!------------------------------------------------------------------------------

   IMPLICIT NONE

   type (domain), intent(in)   :: grid
   TYPE (be_type), INTENT(OUT) :: be                    ! Back. errors structure.

   INTEGER                     :: ij,ijk                ! Scalar.
   INTEGER                     :: i, j, k, ic, jc       ! Loop counters.
   INTEGER                     :: ier, be_unit          ! error index


! added for AVN
   integer                     :: nlath
   integer                     :: nsig
   integer                     :: m,n,m1,n1,n4            ! loop counter
   integer                     :: msig,mlath,nmdszh,kcap  ! dummy variables
   REAL, ALLOCATABLE           :: corz_kz(:,:)
   REAL, ALLOCATABLE           :: cord_kz(:,:)
   REAL, ALLOCATABLE           :: corh_kz(:,:)
   REAL, ALLOCATABLE           :: corq_kz(:,:)
   REAL, ALLOCATABLE           :: corz_avn(:,:)
   REAL, ALLOCATABLE           :: cord_avn(:,:)
   REAL, ALLOCATABLE           :: corh_avn(:,:)
   REAL, ALLOCATABLE           :: corq_avn(:,:)
   REAL, ALLOCATABLE           :: corp_avn(:)
   REAL, ALLOCATABLE           :: clat_avn(:),sigma_avn(:)
   REAL, ALLOCATABLE           :: hwll_avn(:,:,:),hwllp_avn(:),hwll_kz(:,:,:)
   REAL, ALLOCATABLE           :: vztdq_avn(:,:,:),vztdq_kz(:,:,:)

   REAL, ALLOCATABLE           :: agv_avn(:,:,:),agv_kz(:,:,:)
   REAL, ALLOCATABLE           :: bv_avn(:,:),wgv_avn(:,:),bv_kz(:,:),wgv_kz(:,:)
   REAL, ALLOCATABLE           :: dsh(:),turn(:,:)

   REAL, DIMENSION( kts: kte,  kts: kte) :: vv
   REAL, DIMENSION( ids: ide,  jds: jde,  kds: kde, 1:4) :: hwll
   REAL, DIMENSION( ids: ide,  jds: jde)                 :: hwllp, &
                                                            coef1, coef2, &
                                                            global_lat, global_fac
   INTEGER, DIMENSION( ids: ide,  jds: jde)              :: mlat

   INTEGER :: nta,ndeg,ku,kz
   real    :: samp,s2u,tin,as(5),slim
   character(len=256) :: mesg

   write (6,'(A)') ' ----------------------------------------------------------'
   write (6,'(A,I3)') ' [3.0] Set up background errors (be) for cv_option:', cv_options
   write (6,'(A)') ' ----------------------------------------------------------'
   write (6,*)

   if (cv_options /= 3) then
      write(unit=*, fmt='(a, i2)') &
           'cv_options:', cv_options

      call wrf_shutdown

      stop "Stopped for Wrong CV_OPTION."
   endif

!!!!!!!!! cv_options=3
   be % v1 % name = 'psi  '           ! Streamfunction
   be % v2 % name = 'chi_u'           ! Uncorrelated velocity potential.
   be % v3 % name = 't_u'             ! Unbalanced temperature.
   be % v4 % name = 'q/qsg'
   be % v5 % name = 'psfc'            ! surface pressure
   write(6,'(3x,A)')' DA_Setup_Background_Errors: 3DVAR dry control variables are:'
   write(6,'(4x,7A)')TRIM(be % v1 % name), ', ', TRIM(be % v2 % name), ', ', &
                  TRIM(be % v3 % name), ' and ', TRIM(be % v5 % name)

   write(6,'(3x,A,A)')' DA_Setup_Background_Errors: 3DVAR humidity control variable is ',&
                     TRIM(be % v4 % name)

   write(6,*)

   be % mix =  ide -  ids + 1
   be % mjy =  jde -  jds + 1

   ij = ( ite- its+1)*( jte- jts+1)
   ijk = ij * ( kte- kts+1)

   be % v1 % mz =  kde- kds+1
   be % v2 % mz =  kde- kds+1
   be % v3 % mz =  kde- kds+1
   be % v4 % mz =  kde- kds+1
   be % v5 % mz = 1           

   be % cv % size1  = ijk
   be % cv % size2  = ijk
   be % cv % size3  = ijk
   be % cv % size4  = ijk
   be % cv % size5  = ij

   be % cv % size = be % cv % size1 + be % cv % size2 + be % cv % size3 + &
                    be % cv % size4 + be % cv % size5

   cv_size = be % cv % size

   call da_get_unit(be_unit)
   open (unit=be_unit,file="be.dat",status="old",form="unformatted")

   rewind(be_unit)

   read(be_unit, iostat= ier) nsig,nlath

   if (ier /= 0) then
      write(unit=*, fmt='(a, i2, a, i3)') &
           'cv_options:', cv_options,' Reading error in unit=',be_unit
      call wrf_shutdown
      stop "Stopped, Please check if CV3 BE file provided correctly."
   endif

   print*,'number of vertical level for stats =', nsig
   print*,'number of latitude           nlath =', nlath

   kz =  kde- kds+1

   if(nsig.ne.kz)then
     print*,'number of vertical level for WRFVar=', kz
   end if

! 1, Allocate the arrays 

!   1.1 for BK STATS at WRF eta-levels:

  ! Variances for psi(corz), chi_u(cord), t_u(corh), and p-rh(corq)  
   ALLOCATE ( corz_kz(1:2*nlath+1,1:kz),cord_kz(1:2*nlath+1,1:kz) )
   ALLOCATE ( corh_kz(1:2*nlath+1,1:kz),corq_kz(1:2*nlath+1,1:kz) )
  ! Scale lengths: horizontal (hwll), vertical (vztdq)
   ALLOCATE ( hwll_kz(0:nlath*2+1,1:kz,1:4)                      )
   ALLOCATE ( vztdq_kz(1:kz,0:nlath*2+1,1:4)                     )
  ! Regression coefficients: t(agv), chi(bv), psfc(wgv)
   ALLOCATE ( agv_kz(0:nlath*2+1,1:kz,1:kz)                      )
   ALLOCATE ( bv_kz(0:nlath*2+1,1:kz),wgv_kz(0:nlath*2+1,1:kz)   )

!   1.2 for BK STATS inputed from NCEP GFS sigma levels:

  ! Variances for psi(corz), chi_u(cord), t_u(corh), and p-rh(corq)
   ALLOCATE ( corz_avn(1:2*nlath+1,1:nsig),cord_avn(1:2*nlath+1,1:nsig) )
   ALLOCATE ( corh_avn(1:2*nlath+1,1:nsig),corq_avn(1:2*nlath+1,1:nsig) )
  ! Variance for Psfc:
   ALLOCATE ( corp_avn(1:2*nlath+1),clat_avn(1:2*nlath),sigma_avn(1:nsig) )
  ! Scale lengths: horizontal (hwll), vertical (vztdq)
   ALLOCATE ( hwll_avn(0:nlath*2+1,1:nsig,1:4),hwllp_avn(0:nlath*2+1) )
   ALLOCATE ( vztdq_avn(1:nsig,0:nlath*2+1,1:4)                     )
  ! Regression coefficients: t(agv), chi(bv), psfc(wgv)
   ALLOCATE ( agv_avn(0:nlath*2+1,1:nsig,1:nsig)                    )
   ALLOCATE ( bv_avn(0:nlath*2+1,1:nsig),wgv_avn(0:nlath*2+1,1:nsig) )

!   1.3 for BK STATS at the WRF model grids:

   ALLOCATE ( be % corz(its:ite,jts:jte,kts:kte,1:4) )
   ALLOCATE ( be % corp(its:ite,jts:jte) )

   ALLOCATE ( be % vz(kts:kte,its:ite,jts:jte,1:4) )

   ALLOCATE ( be % agvz(its:ite,jts:jte,kts:kte,kts:kte) )
   ALLOCATE ( be % bvz(its:ite,jts:jte,kts:kte) )
   ALLOCATE ( be % wgvz(its:ite,jts:jte,kts:kte) )
!

! 2, load the WFR model latitude and map factor:

#ifdef DM_PARALLEL
   ij = ( ide- ids+1)*( jde- jds+1)
!  Collect xb component of lat into global buffer.
   call da_patch_to_global( grid, grid%xb%lat, global_lat )
   call wrf_dm_bcast_real( global_lat, ij )
!  Collect xb component of fac into global buffer.
   call da_patch_to_global( grid, grid%xb%map_factor, global_fac )
   call wrf_dm_bcast_real( global_fac, ij )
#else
   do i = ids, ide
      do j = jds, jde
         global_lat(i,j) = grid%xb%lat(i,j)
         global_fac(i,j) = grid%xb%map_factor(i,j)
      enddo
   enddo
#endif

! 3, Read in the NCEP GFS BK STATS:

!   3.1 Latitude and sigma values:

   read(be_unit, iostat=ier) clat_avn,(sigma_avn(k),k=1,nsig)
   if (ier /= 0) then
      write(unit=*, fmt='(a, i2, a, i3)') &
           'cv_options:', cv_options,' Reading error in unit=',be_unit
      call wrf_shutdown
      stop "Stopped, Please check if CV3 BE file provided correctly."
   endif

!   3.2 Variances:

   m=2*nlath+1
   read (be_unit) &
                   ((corz_avn(i,k),i=1,m),k=1,nsig),   &
                   ((cord_avn(i,k),i=1,m),k=1,nsig),   &
                   ((corh_avn(i,k),i=1,m),k=1,nsig),   &
                   ((corq_avn(i,k),i=1,m),k=1,nsig),corp_avn

!   3.3 Scale lengths

   ! horizontal
   read(be_unit) (((hwll_avn(i,k,m),i=0,nlath*2+1),k=1,nsig),m=1,4),   &
                     hwllp_avn
   ! vertical:
   read(be_unit) (((vztdq_avn(k,i,m),k=1,nsig),i=0,nlath*2+1),m=1,4)

!   3.4 Regression coefficients:
  
   read(be_unit) (((agv_avn(i,k,m),i=0,nlath*2+1),k=1,nsig),m=1,nsig), &
                         ((bv_avn(i,k),i=0,nlath*2+1),k=1,nsig), &
                         ((wgv_avn(i,k),i=0,nlath*2+1),k=1,nsig)

! 4, incorporate the tuning factors:

!   4.1 Horizontal scales:
 
   as(1)=as1(2)
   as(2)=as2(2)
   as(3)=as3(2)
   as(4)=as4(2)
   do m=1,4
      do k=1,nsig
         do i=0,nlath*2+1
            hwll_avn(i,k,m)=hwll_avn(i,k,m)*as(m)
         enddo
      enddo
   enddo
   do i=0,nlath*2+1
      hwllp_avn(i)=hwllp_avn(i)*as5(2)
   enddo

!   4.2 Vertical scales:
   as(1)=as1(3)
   as(2)=as2(3)
   as(3)=as3(3)
   as(4)=as4(3)
   do m=1,4
      do i=0,nlath*2+1
         do k=1,nsig
            vztdq_avn(k,i,m)=vztdq_avn(k,i,m)*as(m)
         enddo
       enddo
   enddo

! 5, determine the level ku, which locates above and nearest sigma=0.15:

   ku= kde+1
   k_loop: do k= kds+1, kde
      if (grid%xb%sigmah(k-1)>0.15 .and. grid%xb%sigmah(k)<=0.15) then
         ku=k
         exit k_loop
      endif
   end do k_loop
   write(mesg,'(a,i5,a)') "==> level ku =", ku ," locates above and nearest sigma=0.15"
   call wrf_debug ( 1 , mesg )

! 6, Vertical interpolation of BK STATS: 
!    to convert the NCEP GFS sigma levels to WRF model eta levels

   call da_chgvres(nlath,nsig,kz,grid%xb%sigmah,sigma_avn,&
                   corz_avn,cord_avn,corh_avn,corq_avn,hwll_avn,vztdq_avn,agv_avn,bv_avn,wgv_avn,&
                   corz_kz, cord_kz, corh_kz, corq_kz, hwll_kz, vztdq_kz, agv_kz, bv_kz, wgv_kz)

! 7, Horizontal interpolation

!   7.1 Calculate the interpolation coefficients:

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( j, i, m, m1 )
   do j= jds, jde
      do i= ids,  ide

         if (global_lat(i,j).ge.clat_avn(2*nlath)) then
            ! 7.1.1 Model lat >= max AVN lat: 
            mlat(i,j)=nlath*2-1
            coef1(i,j)=0.
            coef2(i,j)=1.
         else
            ! 7.1.2 Model lat < max AVN lat: 
            do m=1,2*nlath-1
               m1=m+1
               if ((global_lat(i,j).ge.clat_avn(m)).and.  &
                  (global_lat(i,j).lt.clat_avn(m1))) then
                  mlat(i,j)=m
                  exit
               end if
            end do

            coef2(i,j)=(global_lat(i,j)-clat_avn(m))/(clat_avn(m1)-clat_avn(m))
            coef1(i,j)=1.-coef2(i,j)
         endif

      end do
   end do
   !$OMP END PARALLEL DO

!   7.2 interpolation of the covariance

   ! Psfc:
   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, j, i, m, m1 )
   do ij = 1, grid%num_tiles

   do j=grid%j_start(ij), grid%j_end(ij)
      do i=its,ite
         m=mlat(i,j)
         m1=m+1
         be%corp(i,j)=corp_avn(m)*coef1(i,j)+corp_avn(m1)*coef2(i,j)
      enddo
   enddo

   enddo
   !$OMP END PARALLEL DO

   ! psi, chi, t, and rh:
   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, j, i, k, m, m1 )
   do ij = 1, grid%num_tiles

   do k=kts,kte
      do j=grid%j_start(ij), grid%j_end(ij)
         do i=its,ite
            m=mlat(i,j)
            m1=m+1
            be%corz(i,j,k,1)=corz_kz(m,k)*coef1(i,j)+corz_kz(m1,k)*coef2(i,j)
            be%corz(i,j,k,2)=cord_kz(m,k)*coef1(i,j)+cord_kz(m1,k)*coef2(i,j)
            be%corz(i,j,k,3)=corh_kz(m,k)*coef1(i,j)+corh_kz(m1,k)*coef2(i,j)
            be%corz(i,j,k,4)=corq_kz(m,k)*coef1(i,j)+corq_kz(m1,k)*coef2(i,j)
         end do
      end do
   end do

   end do
   !$OMP END PARALLEL DO
  
!   7.3 interpolation of the horizontal scale lengths

   ic = 1 + ( ide- ids)/2
   jc = 1 + ( jde- jds)/2

   ! Psfc:
   do j= jds,  jde
      do i= ids,  ide
         m=mlat(i,j)
         m1=m+1
         hwllp(i,j)=hwllp_avn(m)*coef1(i,j)+hwllp_avn(m1)*coef2(i,j)
      end do
   end do
   write(mesg,'(a,2i5,a,f20.10)') 'Horizontal scale length (m) for Psfc at (',ic/2,jc/2,') hwllp=',hwllp(ic/2,jc/2)
   call wrf_debug ( 1 , mesg )

   ! psi, chi, t, and rh:
   do n4=1,4
      do k= kds,  kde
         do j= jds,  jde
            do i= ids,  ide
               m=mlat(i,j)
               m1=m+1
               hwll(i,j,k,n4)=hwll_kz(m,k,n4)*coef1(i,j)+hwll_kz(m1,k,n4)*coef2(i,j)
            end do
         end do
      end do
   end do

!   7.4 interpolation of the vertical scale lengths

   do n4=1,4
      do j=jts,jte
         do i=its,ite
            m=mlat(i,j)
            m1=m+1
            do k=kts,kte
               be%vz(k,i,j,n4)=vztdq_kz(k,m,n4)*coef1(i,j)+vztdq_kz(k,m1,n4)*coef2(i,j)
            end do
         end do
      end do
   end do
   write(mesg,'(a,3i5,a,4f20.16)') 'Vertical scale length for Psi, Chi, t, and rh at (k,i,j)=', &
                                  10,its+2,jts+5,'  vz=',(be%vz(10,its+2,jts+5,n4),n4=1,4)
   call wrf_debug ( 1 , mesg )

!   7.5 interpolation of the regression coefficients

   ! Temperature:
   do k=kts,kte
      do n=kts,kte
         do j=jts,jte
            do i=its,ite
               m=mlat(i,j)
               m1=m+1
               be%agvz(i,j,n,k)=agv_kz(m,n,k)*coef1(i,j)+agv_kz(m1,n,k)*coef2(i,j)
            end do
         end do
      end do
   end do

   ! potentail velocity:
   do k=kts,kte
      do j=jts,jte
         do i=its,ite
            m=mlat(i,j)
            m1=m+1
            be%bvz(i,j,k)=bv_kz(m,k)*coef1(i,j)+bv_kz(m1,k)*coef2(i,j)
         end do
      end do
   end do

   ! Surface pressure:
   do k=kts,kte
      do j=jts,jte
         do i=its,ite
            m=mlat(i,j)
            m1=m+1
            be%wgvz(i,j,k)=wgv_kz(m,k)*coef1(i,j)+wgv_kz(m1,k)*coef2(i,j)
         end do
      end do
   end do

!   7.6 Deallocate the arrays:

   ! For NCEP GFS BK STATS:
   DEALLOCATE ( corz_avn,cord_avn )
   DEALLOCATE ( corh_avn,corq_avn )
   DEALLOCATE ( corp_avn,clat_avn,sigma_avn )
   DEALLOCATE ( hwll_avn,hwllp_avn )
   DEALLOCATE ( vztdq_avn )
   DEALLOCATE ( agv_avn )
   DEALLOCATE ( bv_avn,wgv_avn )

   ! For WRF model levels:
   DEALLOCATE ( corz_kz,cord_kz )
   DEALLOCATE ( corh_kz,corq_kz )
   DEALLOCATE ( hwll_kz )
   DEALLOCATE ( vztdq_kz )
   DEALLOCATE ( agv_kz )
   DEALLOCATE ( bv_kz,wgv_kz )

!
! 8, Create the parameters for recursive filter

!  call da_prerf(grid%xb,be)
!
!   8.1 Set the constants and generate be%be, samp, be%table:

   ! Constant-1: ndeg:
   ndeg=4

   be%ndeg=ndeg
 
   ALLOCATE ( turn (1:ndeg,1:ndeg) )
   ALLOCATE ( be % be (1:ndeg) )
   ALLOCATE ( be % rate (1:ndeg) )

   CALL RFDPAR1(be%BE,be%RATE,ndeg)
   CALL RFDPAR2(be%BE,be%RATE,TURn,SAMP,ndeg)

    ! Constant-2: nta:
   nta=5600

   be%nta=nta
   allocate (dsh(1:nta)        )
   ALLOCATE ( be % table (1:nta,1:ndeg) )

    ! Constant-3: be%swidth:
   be%swidth=10.

   tin=be%swidth/float(nta)
   do i=1,nta
      dsh(i)=float(i-1)*tin
   enddo

   call  RFDPARV(DSH,be%RATE,be%table,nta,ndeg )

!   8.2 Deallocate the working arrays:

   deallocate (dsh )
   deallocate (turn )

! 9, Incorporate the tuning factors for covariance

! sli in scale  unit (map_factor come with ds )
!           variance* amp for 3d/2d RF

   as(1)=sqrt(as1(1))
   as(2)=sqrt(as2(1))
   as(3)=sqrt(as3(1))
   as(4)=sqrt(as4(1))
   as(5)=sqrt(as5(1))

!   9.1 Scale the horizontal scale in unit of grid-point:

   s2u= 1./grid%xb%ds
   hwll=hwll*s2u
   hwllp=hwllp*s2u
 
!   9.2 Re-scale the covariance for psi, chi, t, and rh:

   !$OMP PARALLEL DO &
   !$OMP PRIVATE (ij, n, j, i, vv, k)
   do ij = 1, grid%num_tiles
   do n=1,4
      do j=grid%j_start(ij), grid%j_end(ij)
         do i=its,ite

            vv=0.
            do k=kts,kte
               vv(k,k)=1.
            enddo

     ! Recursive filter routie applied in vertical with 
     ! the vertical scale length be%vz: 
            call da_rfz0(vv,kz,kz,be%ndeg,&
                         be%vz(kts:kte,i,j,n),be%be,be%table,be%nta,be%swidth)

     ! Re-scale the covariance for psi, chi, t, and rh:
            do k=kts,kte
               be % corz(i,j,k,n)=be % corz(i,j,k,n)*as(n) &
                                  *samp/hwll(i,j,k,n)/vv(k,k)/global_fac(i,j)
            enddo

         enddo
      enddo
   enddo
   enddo
   !$OMP END PARALLEL DO

!   9.3 Re-scale the covariance for Psfc:

    be % corp(its:ite,jts:jte)=be % corp(its:ite,jts:jte)*as(5) &
         *samp/hwllp(its:ite,jts:jte)/global_fac(its:ite,jts:jte)
    write(mesg,*) 'Re-scaled covariance for Psfc: sum(be%corp*be%corp)=', &
                  sum(be%corp*be%corp)
    call wrf_debug ( 1 , mesg )

!
! 10, Assign the inverse of scale length fields for recursive filter:
!
!   10.1 allocate the arrays for be scales: y-direction and x-direction:

    ALLOCATE ( be % sljpy ( grid%xp%ipsy: grid%xp%ipey, grid%xp%jpsy: grid%xp%jpey) )
    ALLOCATE ( be % sljy ( grid%xp%ipsy: grid%xp%ipey, grid%xp%jpsy: grid%xp%jpey, grid%xp%kpsy: grid%xp%kpey,1:4) )
    ALLOCATE ( be % slipx ( grid%xp%ipsx: grid%xp%ipex, grid%xp%jpsx: grid%xp%jpex) )
    ALLOCATE ( be % slix ( grid%xp%ipsx: grid%xp%ipex, grid%xp%jpsx: grid%xp%jpex, grid%xp%kpsx: grid%xp%kpex,1:4) )

!   10.2 Y-direction:

    ! 3-D fields: psi, chi, t, and rh:
    do n=1,4
       do k= grid%xp%kpsy, grid%xp%kpey
          do j= grid%xp%jpsy, grid%xp%jpey
             do i= grid%xp%ipsy, grid%xp%ipey
                be%sljy(i,j,k,n)=1./global_fac(i,j)/hwll(i,j,k,n)
             enddo
          enddo
       enddo
    enddo

    ! Above level ku,the sljy fields are set to a constant 
    ! for psi and chi, i.e. homogenous:
    do n=1,2
       do k=max(ku, grid%xp%kpsy), grid%xp%kpey
          slim=1./global_fac(ic,jc)/hwll(ic,jc,k,n)
          do j= grid%xp%jpsy, grid%xp%jpey
             do i= grid%xp%ipsy, grid%xp%ipey
                be%sljy(i,j,k,n)=slim
             enddo
          enddo
       enddo
    enddo

    ! 2-D field: Psfc:
    do j= grid%xp%jpsy, grid%xp%jpey
       do i= grid%xp%ipsy, grid%xp%ipey
          be%sljpy(i,j)=1./global_fac(i,j)/hwllp(i,j)
       enddo
    enddo

!   10.3 X-direction:

   ! 3-D fields: psi, chi, t, and rh:
   do n=1,4
      do k= grid%xp%kpsx, grid%xp%kpex
         do j= grid%xp%jpsx, grid%xp%jpex
            do i= grid%xp%ipsx, grid%xp%ipex
               be%slix(i,j,k,n)=1./global_fac(i,j)/hwll(i,j,k,n)
            enddo
         enddo
      enddo
   enddo

   ! Above level ku,the sljy fields are set to a constant 
   ! for psi and chi, i.e. homogenous:
   do n=1,2
      do k=max(ku, grid%xp%kpsx), grid%xp%kpex
         slim=1./global_fac(ic,jc)/hwll(ic,jc,k,n)
         do j= grid%xp%jpsx, grid%xp%jpex
            do i= grid%xp%ipsx, grid%xp%ipex
               be%slix(i,j,k,n)=slim
            enddo
         enddo
      enddo
   enddo

   ! 2-D field: Psfc:
   do j= grid%xp%jpsx, grid%xp%jpex
      do i= grid%xp%ipsx, grid%xp%ipex
         be%slipx(i,j)=1./global_fac(i,j)/hwllp(i,j)
      enddo
   enddo

   close(be_unit)

end subroutine da_setup_be_ncep_gfs

