program average_be

   use be_type
   use readwrf_module

   implicit none

   integer :: i, j, k, jj, kk, n, b, istatus
   integer :: mapproj, west_east_dim, south_north_dim, idim, jdim, kdim, idim_stag, jdim_stag
   integer :: nmax
   integer, dimension(1) :: nmax_a
   real :: d
   real :: avg_psfc
   real :: delta_lat
   real, allocatable, dimension(:) :: wt
   real, allocatable, dimension(:) :: temp_stats_eval_loc, temp_xb_eval_loc
   real, allocatable, dimension(:,:) :: temp_stats_evec_loc, temp_xb_evec_loc
   real, allocatable, dimension(:) :: stats_regcoeff1, stats_regcoeff2
   real, allocatable, dimension(:) :: xb_regcoeff1, xb_regcoeff2
   real, allocatable, dimension(:,:) :: stats_regcoeff3, xb_regcoeff3
   real*4 :: dx, dy, cen_lat, cen_lon, stand_lon, true1, true2, ratio, miycors, mjxcors
   real, allocatable, dimension(:) :: xb_pres, stats_pres
   character (len=256) :: be_fname
   type (be_dat) :: new_h_be, new_be
   type (be_dat), allocatable, dimension(:) :: be

   real  :: lat1,coslat1

   ! Namelist variables
   integer :: nbins
   character (len=256) :: fg_file_name

   namelist /nl_average_be/ nbins, fg_file_name


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read namelist
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   open(10,file='namelist.average_be',status='old')
   read(10,nl_average_be)
   close(10)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read in BE stats for each latitude bin
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   allocate(be(nbins))
   do i=1,nbins
      write(6,*) '***** Reading in BE stats for bin ',i
      write(be_fname,'(a,i1)') 'be.dat.',i 
      call rd_be_cv_5(be_fname, be(i))
   end do 


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read in a first guess file to get information about the model domain
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   write(6,*) '***** Reading in first guess file ', trim(fg_file_name)
   istatus = readwrf(fg_file_name, west_east_dim, south_north_dim, dx, dy, cen_lat, cen_lon, &
                 stand_lon, true1, true2, mapproj, idim, jdim, kdim, idim_stag, jdim_stag, &
                 ratio, miycors, mjxcors)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Set up BE structure for interpolated statistics
!
! new_h_be will hold the horizontally interpolated BE statistics
! new_be   will hold the vertically and horizontally interpolated BE statistics
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   write(6,*) '***** Allocating BE structures'
   new_be%ni = idim
   new_be%nj = jdim
   new_be%nk = kdim
   new_be%nk_2d = 1

   new_be%bin_type = 5
   new_be%num_bins = kdim
   new_be%num_bins2d = 1

   new_be%lat_min = minval(wrf_xlat)   ! Minimum latitude in wrfinput file
   new_be%lat_max =  maxval(wrf_xlat)  ! Maximum latitude in wrfinput file
   new_be%binwidth_lat = (new_be%lat_max - new_be%lat_min) / real(jdim-1)

   new_be%hgt_min = 0.0
   new_be%hgt_max =  0.0
   new_be%binwidth_hgt = 0.0

   new_be%variable(:) = be(1)%variable(:)

   allocate( new_be%bin (idim,jdim,kdim) )
   allocate( new_be%bin2d (idim,jdim) )

   allocate( new_be%regcoeff1 (new_be%num_bins) )
   allocate( new_be%regcoeff2 (kdim,new_be%num_bins2d) )
   allocate( new_be%regcoeff3 (kdim,kdim,new_be%num_bins2d) )

   allocate ( new_be%be1_eval_loc (1:jdim,1:kdim) )
   allocate ( new_be%be2_eval_loc (1:jdim,1:kdim) )
   allocate ( new_be%be3_eval_loc (1:jdim,1:kdim) )
   allocate ( new_be%be4_eval_loc (1:jdim,1:kdim) )
   allocate ( new_be%be5_eval_loc (1:jdim,1:1 ) )

   allocate ( new_be%be1_eval_glo (1:kdim) )
   allocate ( new_be%be2_eval_glo (1:kdim) )
   allocate ( new_be%be3_eval_glo (1:kdim) )
   allocate ( new_be%be4_eval_glo (1:kdim) )
   allocate ( new_be%be5_eval_glo (1:1) )

   allocate ( new_be%be1_evec_loc (1:jdim,1:kdim,1:kdim))
   allocate ( new_be%be2_evec_loc (1:jdim,1:kdim,1:kdim))
   allocate ( new_be%be3_evec_loc (1:jdim,1:kdim,1:kdim))
   allocate ( new_be%be4_evec_loc (1:jdim,1:kdim,1:kdim))
   allocate ( new_be%be5_evec_loc (1:jdim,1: 1,1: 1))

   allocate ( new_be%be1_evec_glo (1:kdim,1:kdim) )
   allocate ( new_be%be2_evec_glo (1:kdim,1:kdim) )
   allocate ( new_be%be3_evec_glo (1:kdim,1:kdim) )
   allocate ( new_be%be4_evec_glo (1:kdim,1:kdim) )
   allocate ( new_be%be5_evec_glo (1:1,1:1) )

   allocate ( new_be%be1_rf_lengthscale (1:kdim) )
   allocate ( new_be%be2_rf_lengthscale (1:kdim) )
   allocate ( new_be%be3_rf_lengthscale (1:kdim) )
   allocate ( new_be%be4_rf_lengthscale (1:kdim) )
   allocate ( new_be%be5_rf_lengthscale (1:kdim) )



   allocate( new_h_be%bin (idim,jdim,be(1)%nk) )
   allocate( new_h_be%bin2d (idim,jdim) )

   ! NB: The regression coefficient arrays of new_h_be are never actually used
   allocate( new_h_be%regcoeff1 (new_be%num_bins) )
   allocate( new_h_be%regcoeff2 (be(1)%nk,new_be%num_bins2d) )
   allocate( new_h_be%regcoeff3 (be(1)%nk,be(1)%nk,new_be%num_bins2d) )

   allocate ( new_h_be%be1_eval_loc (1:jdim,1:be(1)%nk) )
   allocate ( new_h_be%be2_eval_loc (1:jdim,1:be(1)%nk) )
   allocate ( new_h_be%be3_eval_loc (1:jdim,1:be(1)%nk) )
   allocate ( new_h_be%be4_eval_loc (1:jdim,1:be(1)%nk) )
   allocate ( new_h_be%be5_eval_loc (1:jdim,1:1 ) )

   allocate ( new_h_be%be1_eval_glo (1:be(1)%nk) )
   allocate ( new_h_be%be2_eval_glo (1:be(1)%nk) )
   allocate ( new_h_be%be3_eval_glo (1:be(1)%nk) )
   allocate ( new_h_be%be4_eval_glo (1:be(1)%nk) )
   allocate ( new_h_be%be5_eval_glo (1:1) )

   allocate ( new_h_be%be1_evec_loc (1:jdim,1:be(1)%nk,1:be(1)%nk))
   allocate ( new_h_be%be2_evec_loc (1:jdim,1:be(1)%nk,1:be(1)%nk))
   allocate ( new_h_be%be3_evec_loc (1:jdim,1:be(1)%nk,1:be(1)%nk))
   allocate ( new_h_be%be4_evec_loc (1:jdim,1:be(1)%nk,1:be(1)%nk))
   allocate ( new_h_be%be5_evec_loc (1:jdim,1: 1,1: 1))

   allocate ( new_h_be%be1_evec_glo (1:be(1)%nk,1:be(1)%nk) )
   allocate ( new_h_be%be2_evec_glo (1:be(1)%nk,1:be(1)%nk) )
   allocate ( new_h_be%be3_evec_glo (1:be(1)%nk,1:be(1)%nk) )
   allocate ( new_h_be%be4_evec_glo (1:be(1)%nk,1:be(1)%nk) )
   allocate ( new_h_be%be5_evec_glo (1:1,1:1) )

   allocate ( new_h_be%be1_rf_lengthscale (1:be(1)%nk) )
   allocate ( new_h_be%be2_rf_lengthscale (1:be(1)%nk) )
   allocate ( new_h_be%be3_rf_lengthscale (1:be(1)%nk) )
   allocate ( new_h_be%be4_rf_lengthscale (1:be(1)%nk) )
   allocate ( new_h_be%be5_rf_lengthscale (1:be(1)%nk) )


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Horizontally interpolate BE statistics
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   write(6,*) '***** Horizontally interpolating statistics'

   !
   ! Determine what percent of the regional domain falls within each global bin
   !
   allocate(wt(nbins))
   wt(:) = 0.0
   do i=1,idim
      do j=1,jdim
         do n=1,nbins
            if (wrf_xlat(i,j) >= (real(n-1)*be(n)%binwidth_lat + be(n)%lat_min) .and. &
                wrf_xlat(i,j) <= (real(n)*be(n)%binwidth_lat + be(n)%lat_min)) then
               wt(n) = wt(n) + 1.0
            end if
         end do
      end do
   end do
   do n=1,nbins
      wt(n) = wt(n) / real(idim*jdim)
      write(0,*) wt(n),' percent of regional domain is within band ', n 
   end do

   ! Set nmax to the global bin that contains largest part of regional domain
   nmax_a = maxloc(wt) 
   nmax = nmax_a(1)


   ! Loop over original dimension of evals and evecs
   do k=1,be(1)%nk

      !
      ! Interpolate global eigenvectors
      !

      ! Loop over eigenvector component
      do kk=1,be(1)%nk
         new_h_be%be1_evec_glo(k,kk) = be(nmax)%be1_evec_glo(k,kk)
         new_h_be%be2_evec_glo(k,kk) = be(nmax)%be2_evec_glo(k,kk)
         new_h_be%be3_evec_glo(k,kk) = be(nmax)%be3_evec_glo(k,kk)
         new_h_be%be4_evec_glo(k,kk) = be(nmax)%be4_evec_glo(k,kk)

         if (k == 1 .and. kk == 1) then
            new_h_be%be5_evec_glo(1,1) = be(nmax)%be5_evec_glo(1,1)
         end if
      end do

      !
      ! Interpolate global eigenvalues
      !
      
      new_h_be%be1_eval_glo(k) = be(nmax)%be1_eval_glo(k)
      new_h_be%be2_eval_glo(k) = be(nmax)%be2_eval_glo(k)
      new_h_be%be3_eval_glo(k) = be(nmax)%be3_eval_glo(k)
      new_h_be%be4_eval_glo(k) = be(nmax)%be4_eval_glo(k)
      if (k == 1) new_h_be%be5_eval_glo(k) = be(nmax)%be5_eval_glo(k)

      !
      ! Interpolate lengthscales
      !
      new_h_be%be1_rf_lengthscale(k) = 0.0
      new_h_be%be2_rf_lengthscale(k) = 0.0
      new_h_be%be3_rf_lengthscale(k) = 0.0
      new_h_be%be4_rf_lengthscale(k) = 0.0

      do n=1,nbins
         lat1 = (real(n)-0.5)*be(n)%binwidth_lat + be(n)%lat_min
         coslat1 = cos(lat1*deg_to_rad)

         new_h_be%be1_rf_lengthscale(k) = new_h_be%be1_rf_lengthscale(k) + coslat1*be(n)%be1_rf_lengthscale(k) * wt(n)
         new_h_be%be2_rf_lengthscale(k) = new_h_be%be2_rf_lengthscale(k) + coslat1*be(n)%be2_rf_lengthscale(k) * wt(n)
         new_h_be%be3_rf_lengthscale(k) = new_h_be%be3_rf_lengthscale(k) + coslat1*be(n)%be3_rf_lengthscale(k) * wt(n)
         new_h_be%be4_rf_lengthscale(k) = new_h_be%be4_rf_lengthscale(k) + coslat1*be(n)%be4_rf_lengthscale(k) * wt(n)
         if (k == 1) &
         new_h_be%be5_rf_lengthscale(k) = new_h_be%be5_rf_lengthscale(k) + coslat1*be(n)%be5_rf_lengthscale(k) * wt(n)
      end do
   end do

   !
   ! Interpolate local eigenvectors
   !
   do j=1,new_be%nj   ! new j latitudes

      ! Set d to the row in the global domain that is closest in latitude to current xb j index
      d = (wrf_xlat(1,j)-be(1)%lat_min)/(be(1)%lat_max-be(1)%lat_min)*real(be(1)%nj)

      do k=1,be(1)%nk       ! original dimension of evals and evecs
      do kk=1,be(1)%nk   ! kk is eigenvector component index

         new_h_be%be1_evec_loc(j,k,kk) = be(1)%be1_evec_loc(nint(d),k,kk)
         new_h_be%be2_evec_loc(j,k,kk) = be(1)%be2_evec_loc(nint(d),k,kk)
         new_h_be%be3_evec_loc(j,k,kk) = be(1)%be3_evec_loc(nint(d),k,kk)
         new_h_be%be4_evec_loc(j,k,kk) = be(1)%be4_evec_loc(nint(d),k,kk)
         if (k == 1 .and. kk == 1) new_h_be%be5_evec_loc(j,k,kk) = be(1)%be5_evec_loc(nint(d),k,kk)

      end do
      end do
   end do

   !
   ! Interpolate local eigenvalues
   !
   do j=1,new_be%nj   ! new j latitudes
      d = (wrf_xlat(1,j)-be(1)%lat_min)/(be(1)%lat_max-be(1)%lat_min)*real(be(1)%nj)

      do k=1,be(1)%nk       ! original dimension of evals and evecs

         new_h_be%be1_eval_loc(j,k) = be(1)%be1_eval_loc(nint(d),k)
         new_h_be%be2_eval_loc(j,k) = be(1)%be2_eval_loc(nint(d),k)
         new_h_be%be3_eval_loc(j,k) = be(1)%be3_eval_loc(nint(d),k)
         new_h_be%be4_eval_loc(j,k) = be(1)%be4_eval_loc(nint(d),k)
         if (k == 1) new_h_be%be5_eval_loc(j,k) = be(1)%be5_eval_loc(nint(d),k)

      end do
   end do

   !
   ! Set up bins
   !
    do k=1,kdim
       new_be%bin(:,:,k) = k
    end do
    new_be%bin2d(:,:) = 1


   write(6,*) '** Interpolating stats for domain dimensioned ',idim,jdim,kdim
   write(6,*) 'Domain lower-left at (lat,lon)=',wrf_xlat(1,1),wrf_xlong(1,1)
   write(6,*) 'Domain upper-right at (lat,lon)=',wrf_xlat(idim,jdim),wrf_xlong(idim,jdim)
   write(6,*) 'znu from model = ', znu
   write(6,*) 'model ptop = ', ptop


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Vertically interpolate BE statistics
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   write(6,*) '***** Vertically interpolating statistics'

   !
   ! Fill in array with mean pressure on "xb" levels
   !
   allocate(xb_pres(kdim))
   avg_psfc = sum(wrf_psfc(:,:))/real(idim*jdim)      
   do k=1,kdim
      xb_pres(k) = (avg_psfc - ptop)*znu(k) + ptop
      write(6,*) ' xb_pres(',k,')= ',xb_pres(k)
   end do

   !
   ! Fill in array with mean pressure on stats levels
   !
   allocate(stats_pres(be(1)%nk))
   do k=1,be(1)%nk
      stats_pres(k) = (be(1)%psfc - be(1)%ptop)*be(1)%znu(k) + be(1)%ptop
      write(6,*) ' stats_pres(',k,')= ',stats_pres(k)
   end do


   !
   ! Interpolate global eigenvectors, global eigenvalues, and lengthscales
   !
   call da_interpolate_stats( be(1)%nk,                    &   ! Number of levels in stats.
                              kdim,                        &   ! Number of levels in xb.
                              stats_pres,                  &   ! Mean pressure on stats levs.
                              xb_pres,                     &   ! Mean pressure on xb levs.
                              new_h_be%be1_evec_glo,       &   ! Eigenvectors of vert B.
                              new_h_be%be1_eval_glo,       &   ! Eigenvalues of vert B.
                              new_h_be%be1_rf_lengthscale, &   ! Correlation scale.
                              new_be%be1_evec_glo,         &   ! New eigenvectors.
                              new_be%be1_eval_glo,         &   ! New eigenvalues.
                              new_be%be1_rf_lengthscale,   &   ! New lengthscales.
                              dx                           )   ! grid distance in m     

   call da_interpolate_stats( be(1)%nk,                    &   ! Number of levels in stats.
                              kdim,                        &   ! Number of levels in xb.
                              stats_pres,                  &   ! Mean pressure on stats levs.
                              xb_pres,                     &   ! Mean pressure on xb levs.
                              new_h_be%be2_evec_glo,       &   ! Eigenvectors of vert B.
                              new_h_be%be2_eval_glo,       &   ! Eigenvalues of vert B.
                              new_h_be%be2_rf_lengthscale, &   ! Correlation scale.
                              new_be%be2_evec_glo,         &   ! New eigenvectors.
                              new_be%be2_eval_glo,         &   ! New eigenvalues.
                              new_be%be2_rf_lengthscale,   &   ! New lengthscales.
                              dx                           )   ! grid distance in m     

   call da_interpolate_stats( be(1)%nk,                    &   ! Number of levels in stats.
                              kdim,                        &   ! Number of levels in xb.
                              stats_pres,                  &   ! Mean pressure on stats levs.
                              xb_pres,                     &   ! Mean pressure on xb levs.
                              new_h_be%be3_evec_glo,       &   ! Eigenvectors of vert B.
                              new_h_be%be3_eval_glo,       &   ! Eigenvalues of vert B.
                              new_h_be%be3_rf_lengthscale, &   ! Correlation scale.
                              new_be%be3_evec_glo,         &   ! New eigenvectors.
                              new_be%be3_eval_glo,         &   ! New eigenvalues.
                              new_be%be3_rf_lengthscale,   &   ! New lengthscales.
                              dx                           )   ! grid distance in m     

   call da_interpolate_stats( be(1)%nk,                    &   ! Number of levels in stats.
                              kdim,                        &   ! Number of levels in xb.
                              stats_pres,                  &   ! Mean pressure on stats levs.
                              xb_pres,                     &   ! Mean pressure on xb levs.
                              new_h_be%be4_evec_glo,       &   ! Eigenvectors of vert B.
                              new_h_be%be4_eval_glo,       &   ! Eigenvalues of vert B.
                              new_h_be%be4_rf_lengthscale, &   ! Correlation scale.
                              new_be%be4_evec_glo,         &   ! New eigenvectors.
                              new_be%be4_eval_glo,         &   ! New eigenvalues.
                              new_be%be4_rf_lengthscale,   &   ! New lengthscales.
                              dx                           )   ! grid distance in m     

   new_be%be5_rf_lengthscale(1) = new_h_be%be5_rf_lengthscale(1)
   do k=2,kdim
      new_be%be5_rf_lengthscale(k) = 0.0
   end do
   new_be%be5_evec_glo(:,:) = new_h_be%be5_evec_glo(:,:)
   new_be%be5_eval_glo(:)   = new_h_be%be5_eval_glo(:)


   !
   ! Interpolate local eigenvectors, and local eigenvalues
   !
   allocate(temp_stats_evec_loc(be(1)%nk,be(1)%nk))
   allocate(temp_stats_eval_loc(be(1)%nk))
   allocate(temp_xb_evec_loc(kdim,kdim))
   allocate(temp_xb_eval_loc(kdim))

   do j=1,jdim
      temp_stats_evec_loc(:,:) = new_h_be%be1_evec_loc(j,:,:)
      temp_stats_eval_loc(:)   = new_h_be%be1_eval_loc(j,:)
      call da_interpolate_stats( be(1)%nk,                     &   ! Number of levels in stats.
                                 kdim,                         &   ! Number of levels in xb.
                                 stats_pres,                   &   ! Mean pressure on stats levs.
                                 xb_pres,                      &   ! Mean pressure on xb levs.
                                 temp_stats_evec_loc,          &   ! Eigenvectors of vert B.
                                 temp_stats_eval_loc,          &   ! Eigenvalues of vert B.
                                 new_h_be%be1_rf_lengthscale,  &   ! Correlation scale.
                                 temp_xb_evec_loc,             &   ! New eigenvectors.
                                 temp_xb_eval_loc,             &   ! New eigenvalues.
                                 new_be%be1_rf_lengthscale,    &   ! New lengthscales.
                                 dx                            )   ! grid distance in m     
      new_be%be1_evec_loc(j,:,:) = temp_xb_evec_loc(:,:)
      new_be%be1_eval_loc(j,:)   = temp_xb_eval_loc(:)
   
      temp_stats_evec_loc(:,:) = new_h_be%be2_evec_loc(j,:,:)
      temp_stats_eval_loc(:)   = new_h_be%be2_eval_loc(j,:)
      call da_interpolate_stats( be(1)%nk,                     &   ! Number of levels in stats.
                                 kdim,                         &   ! Number of levels in xb.
                                 stats_pres,                   &   ! Mean pressure on stats levs.
                                 xb_pres,                      &   ! Mean pressure on xb levs.
                                 temp_stats_evec_loc,          &   ! Eigenvectors of vert B.
                                 temp_stats_eval_loc,          &   ! Eigenvalues of vert B.
                                 new_h_be%be2_rf_lengthscale,  &   ! Correlation scale.
                                 temp_xb_evec_loc,             &   ! New eigenvectors.
                                 temp_xb_eval_loc,             &   ! New eigenvalues.
                                 new_be%be2_rf_lengthscale,    &   ! New lengthscales.
                                 dx                            )   ! grid distance in m     
      new_be%be2_evec_loc(j,:,:) = temp_xb_evec_loc(:,:)
      new_be%be2_eval_loc(j,:)   = temp_xb_eval_loc(:)
   
      temp_stats_evec_loc(:,:) = new_h_be%be3_evec_loc(j,:,:)
      temp_stats_eval_loc(:)   = new_h_be%be3_eval_loc(j,:)
      call da_interpolate_stats( be(1)%nk,                     &   ! Number of levels in stats.
                                 kdim,                         &   ! Number of levels in xb.
                                 stats_pres,                   &   ! Mean pressure on stats levs.
                                 xb_pres,                      &   ! Mean pressure on xb levs.
                                 temp_stats_evec_loc,          &   ! Eigenvectors of vert B.
                                 temp_stats_eval_loc,          &   ! Eigenvalues of vert B.
                                 new_h_be%be3_rf_lengthscale,  &   ! Correlation scale.
                                 temp_xb_evec_loc,             &   ! New eigenvectors.
                                 temp_xb_eval_loc,             &   ! New eigenvalues.
                                 new_be%be3_rf_lengthscale,    &   ! New lengthscales.
                                 dx                            )   ! grid distance in m     
      new_be%be3_evec_loc(j,:,:) = temp_xb_evec_loc(:,:)
      new_be%be3_eval_loc(j,:)   = temp_xb_eval_loc(:)
   
      temp_stats_evec_loc(:,:) = new_h_be%be4_evec_loc(j,:,:)
      temp_stats_eval_loc(:)   = new_h_be%be4_eval_loc(j,:)
      call da_interpolate_stats( be(1)%nk,                     &   ! Number of levels in stats.
                                 kdim,                         &   ! Number of levels in xb.
                                 stats_pres,                   &   ! Mean pressure on stats levs.
                                 xb_pres,                      &   ! Mean pressure on xb levs.
                                 temp_stats_evec_loc,          &   ! Eigenvectors of vert B.
                                 temp_stats_eval_loc,          &   ! Eigenvalues of vert B.
                                 new_h_be%be4_rf_lengthscale,  &   ! Correlation scale.
                                 temp_xb_evec_loc,             &   ! New eigenvectors.
                                 temp_xb_eval_loc,             &   ! New eigenvalues.
                                 new_be%be4_rf_lengthscale,    &   ! New lengthscales.
                                 dx                            )   ! grid distance in m     
      new_be%be4_evec_loc(j,:,:) = temp_xb_evec_loc(:,:)
      new_be%be4_eval_loc(j,:)   = temp_xb_eval_loc(:)
   
   end do

   new_be%be5_evec_loc(:,:,:) = new_h_be%be5_evec_loc(:,:,:)
   new_be%be5_eval_loc(:,:)   = new_h_be%be5_eval_loc(:,:)

   allocate( stats_regcoeff1(1:be(1)%nk) )
   allocate( stats_regcoeff2(1:be(1)%nk) )
   allocate( stats_regcoeff3(1:be(1)%nk, 1:be(1)%nk) )
   
   stats_regcoeff1 = 0.
   stats_regcoeff2 = 0.
   stats_regcoeff3 = 0.
   do k=1,be(1)%nk 
      do n=1,nbins
         kk = n + (k-1) * nbins    
         stats_regcoeff1(k)= stats_regcoeff1(k) + be(1)%regcoeff1(kk)*wt(n)
         stats_regcoeff2(k)= stats_regcoeff2(k) + be(1)%regcoeff2(k,n)*wt(n)
         do i=1,be(1)%nk 
            stats_regcoeff3(i,k)= stats_regcoeff3(i,k) + be(1)%regcoeff3(i,k,n)*wt(n)
         end do
      end do
   end do

   allocate( xb_regcoeff1(1:new_be%nk) )
   allocate( xb_regcoeff2(1:new_be%nk) )
   allocate( xb_regcoeff3(1:new_be%nk, 1:new_be%nk) )

   call da_interpolate_regcoeff( be(1)%nk, new_be%nk, stats_pres, xb_pres,    &
                                 stats_regcoeff1, stats_regcoeff2, stats_regcoeff3, &
                                 xb_regcoeff1, xb_regcoeff2, xb_regcoeff3 )     

! Now transfer regression coefficients to new_be array

   do k=1,new_be%nk
      new_be%regcoeff1 (k)  = xb_regcoeff1(k) 
      new_be%regcoeff2 (k,:)= xb_regcoeff2(k) 
      do kk=1,new_be%nk
         new_be%regcoeff3(k,kk,:)= xb_regcoeff3(k,kk) 
      end do
   end do
 
   deallocate(temp_stats_evec_loc)
   deallocate(temp_stats_eval_loc)
   deallocate(temp_xb_evec_loc)
   deallocate(temp_xb_eval_loc)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Write out interpolated be statistics to new file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   be_fname = 'be.dat'
   write(6,*) '***** Writing interpolated BE stats to ', trim(be_fname)
   call wr_be_cv_5(be_fname, new_be)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Clean up
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   do i=1,nbins
      call free_be_dat(be(i))
   end do
   deallocate(be)
   call free_be_dat(new_be)
   call free_be_dat(new_h_be)
   deallocate(wt)
   deallocate(xb_pres)
   deallocate(stats_pres)

   stop

end program average_be
