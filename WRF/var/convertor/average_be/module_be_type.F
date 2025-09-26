module be_type


   type be_dat
      integer                     :: nrec, max_wavenumber           ! Scalar.
      real, allocatable           :: height(:,:,:)                  ! Height field.
      integer, allocatable        :: bin(:,:,:)                     ! Bin assigned to each 3D point
      integer, allocatable        :: bin2d(:,:)                     ! Bin assigned to each 2D point
      integer                     :: bin_type                       ! Type of bin to average over.
      integer                     :: num_bins                       ! Number of bins (3D fields).
      integer                     :: num_bins2d                     ! Number of bins (3D fields).
      real                        :: lat_min, lat_max, binwidth_lat ! Used if bin_type = 2 (degrees)..
      real                        :: hgt_min, hgt_max, binwidth_hgt ! Used if bin_type = 2 (m). .
      integer                     :: num_bins_lat, num_bins_hgt     ! Used if bin_type = 2.
   
      logical                     :: use_global_eofs                ! True if projected data uses global EOFs

      real, allocatable           :: be1_eval_loc(:,:)     ! Local Eigenvalues.
      real, allocatable           :: be2_eval_loc(:,:)     ! Local Eigenvalues.
      real, allocatable           :: be3_eval_loc(:,:)     ! Local Eigenvalues.
      real, allocatable           :: be4_eval_loc(:,:)     ! Local Eigenvalues.
      real, allocatable           :: be5_eval_loc(:,:)     ! Local Eigenvalues.
   
      real, allocatable           :: be1_eval_glo(:)       ! Global Eigenvalues.
      real, allocatable           :: be2_eval_glo(:)       ! Global Eigenvalues.
      real, allocatable           :: be3_eval_glo(:)       ! Global Eigenvalues.
      real, allocatable           :: be4_eval_glo(:)       ! Global Eigenvalues.
      real, allocatable           :: be5_eval_glo(:)       ! Global Eigenvalues.
   
      real, allocatable           :: be1_evec_loc(:,:,:)   ! Local Eigenvectors.
      real, allocatable           :: be2_evec_loc(:,:,:)   ! Local Eigenvectors.
      real, allocatable           :: be3_evec_loc(:,:,:)   ! Local Eigenvectors.
      real, allocatable           :: be4_evec_loc(:,:,:)   ! Local Eigenvectors.
      real, allocatable           :: be5_evec_loc(:,:,:)   ! Local Eigenvectors.
   
      real, allocatable           :: be1_evec_glo(:,:)     ! Global Eigenvectors.
      real, allocatable           :: be2_evec_glo(:,:)     ! Global Eigenvectors.
      real, allocatable           :: be3_evec_glo(:,:)     ! Global Eigenvectors.
      real, allocatable           :: be4_evec_glo(:,:)     ! Global Eigenvectors.
      real, allocatable           :: be5_evec_glo(:,:)     ! Global Eigenvectors.
   
      real, allocatable           :: be1_rf_lengthscale(:) ! RF lengthscale.
      real, allocatable           :: be2_rf_lengthscale(:) ! RF lengthscale.
      real, allocatable           :: be3_rf_lengthscale(:) ! RF lengthscale.
      real, allocatable           :: be4_rf_lengthscale(:) ! RF lengthscale.
      real, allocatable           :: be5_rf_lengthscale(:)
      real, allocatable           :: alpha_rf_lengthscale(:)
      real, allocatable           :: evec_loc(:,:,:)        ! Latitudinally varying eigenvectors.
      real, allocatable           :: eval_loc(:,:)          ! Latitudinally varying eigenvalues.

      character (len=10), dimension(5) :: variable
      integer                     :: ni, nj, nk, nk_2d, nlev
      real, allocatable           :: regcoeff1(:)
      real, allocatable           :: regcoeff2(:,:)
      real, allocatable           :: regcoeff3(:,:,:)

      real, allocatable           :: znu(:)
      real                        :: ptop
      real                        :: psfc
   end type be_dat

   contains

   subroutine free_be_dat(be)

      implicit none
 
      type (be_dat) :: be

      deallocate( be%bin )
      deallocate( be%bin2d )
   
      deallocate( be%regcoeff1 )
      deallocate( be%regcoeff2 )
      deallocate( be%regcoeff3 )
   
      deallocate ( be%be1_eval_loc )
      deallocate ( be%be2_eval_loc )
      deallocate ( be%be3_eval_loc )
      deallocate ( be%be4_eval_loc )
      deallocate ( be%be5_eval_loc )
   
      deallocate ( be%be1_eval_glo )
      deallocate ( be%be2_eval_glo )
      deallocate ( be%be3_eval_glo )
      deallocate ( be%be4_eval_glo )
      deallocate ( be%be5_eval_glo )
   
      deallocate ( be%be1_evec_loc )
      deallocate ( be%be2_evec_loc )
      deallocate ( be%be3_evec_loc )
      deallocate ( be%be4_evec_loc )
      deallocate ( be%be5_evec_loc )
   
      deallocate ( be%be1_evec_glo )
      deallocate ( be%be2_evec_glo )
      deallocate ( be%be3_evec_glo )
      deallocate ( be%be4_evec_glo )
      deallocate ( be%be5_evec_glo )
   
      deallocate ( be%be1_rf_lengthscale )
      deallocate ( be%be2_rf_lengthscale )
      deallocate ( be%be3_rf_lengthscale )
      deallocate ( be%be4_rf_lengthscale )
      deallocate ( be%be5_rf_lengthscale )

   end subroutine free_be_dat
   
end module be_type
