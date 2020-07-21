program be_readwrite

! gfortran -o combine_be_cv5.exe -fconvert=big-endian combine_be_cv5.f90

! input:  be_[PSI|CHI_U|T_U|RH|PSFC_U].dat
!         regcoeff[1|2|3].dat
! output: be.dat (in real(8) and big_endian)

   implicit none

   integer               :: ni, nj, nk           ! dimensions read in
   integer               :: bin_type             ! type of bin to average over
   integer               :: num_bins             ! number of 3D bins
   integer               :: num_bins2d           ! number of 2D bins
   integer, allocatable  :: bin(:,:,:)           ! bin assigned to each 3D point
   integer, allocatable  :: bin2d(:,:)           ! bin assigned to each 2D point

!for be_[PSI|CHI_U|T_U|RH|PSFC_U].dat in real(4) when gen_be_v3 is run with write_be_dat_r8=.false.
   !real,    allocatable  :: e_vec(:,:)           ! domain-averaged eigenvectors
   !real,    allocatable  :: e_val(:)             ! domain-averaged eigenvalues
   !real,    allocatable  :: e_vec_loc(:,:,:)     ! latitudinally varying eigenvectors
   !real,    allocatable  :: e_val_loc(:,:)       ! latitudinally varying eigenvalues
   !real,    allocatable  :: scale_length(:)      ! scale length for regional application
   !real,    allocatable  :: regcoeff1(:)         ! psi/chi regcoeff
   !real,    allocatable  :: regcoeff2(:,:)       ! psi/ps regcoeff
   !real,    allocatable  :: regcoeff3(:,:,:)     ! psi/t regcoeff
!for be_[PSI|CHI_U|T_U|RH|PSFC_U].dat in real(8) when gen_be_v3 is run with write_be_dat_r8=.true.(default)
   real*8,  allocatable  :: e_vec(:,:)           ! domain-averaged eigenvectors
   real*8,  allocatable  :: e_val(:)             ! domain-averaged eigenvalues
   real*8,  allocatable  :: e_vec_loc(:,:,:)     ! latitudinally varying eigenvectors
   real*8,  allocatable  :: e_val_loc(:,:)       ! latitudinally varying eigenvalues
   real*8,  allocatable  :: scale_length(:)      ! scale length for regional application
   real*8,  allocatable  :: regcoeff1(:)         ! psi/chi regcoeff
   real*8,  allocatable  :: regcoeff2(:,:)       ! psi/ps regcoeff
   real*8,  allocatable  :: regcoeff3(:,:,:)     ! psi/t regcoeff

   real*8,  allocatable  :: e_vec_r8(:,:)        ! domain-averaged eigenvectors
   real*8,  allocatable  :: e_val_r8(:)          ! domain-averaged eigenvalues
   real*8,  allocatable  :: e_vec_loc_r8(:,:,:)  ! latitudinally varying eigenvectors
   real*8,  allocatable  :: e_val_loc_r8(:,:)    ! latitudinally varying eigenvalues
   real*8,  allocatable  :: scale_length_r8(:)   ! scale length for regional application

   real*8,  allocatable  :: regcoeff1_r8(:)      ! psi/chi regcoeff
   real*8,  allocatable  :: regcoeff2_r8(:,:)    ! psi/ps regcoeff
   real*8,  allocatable  :: regcoeff3_r8(:,:,:)  ! psi/t regcoeff

   character(len=256)    :: infile               ! input filename
   character(len=256)    :: outfile              ! output filename
   logical               :: isfile
   integer               :: iunit, ounit

   integer, parameter :: nvar = 5
   character(len=10)  :: varnames(nvar) = (/ "PSI       ", &
                                             "CHI_U     ", &
                                             "T_U       ", &
                                             "RH        ", &
                                             "PSFC_U    " /)
   character(len=10)  :: varout(nvar)   = (/ "psi       ", &
                                             "chi_u     ", &
                                             "t_u       ", &
                                             "rh        ", &
                                             "ps_u      " /)
   character(len=10)  :: this_var
   integer            :: var_dim
   integer            :: i
   integer            :: nk_2d = 1

   real*8  :: lat_min, lat_max, binwidth_lat
   real*8  :: hgt_min, hgt_max, binwidth_hgt

   lat_min = 0.0
   lat_max = 0.0
   binwidth_lat = 0.0
   hgt_min = 0.0
   hgt_max = 0.0
   binwidth_hgt = 0.0

   iunit = 81
   ounit = 82

   outfile = 'be.dat'
   open (ounit, file=trim(outfile), form='unformatted', status='replace')

   ! read and write dimension and bin info

   infile = 'be_'//trim(varnames(1))//'.dat'
   inquire(file=trim(infile), exist=isfile)
   if ( .not. isfile ) then
      write(*,*) 'STOP: ', trim(infile), ' does not exist.'
      stop
   end if
   open (iunit, file=trim(infile), form='unformatted', status='old')
   write(*,*) 'Reading from ', trim(infile), ' for dimension and bin info'

   read(iunit) ni, nj, nk

   allocate( bin(1:ni,1:nj,1:nk) )
   allocate( bin2d(1:ni,1:nj) )

   read(iunit) bin_type
   read(iunit) num_bins, num_bins2d
   read(iunit) bin
   read(iunit) bin2d
   close(iunit)

   write(ounit) ni, nj, nk
   write(ounit) bin_type
   write(ounit) lat_min, lat_max, binwidth_lat
   write(ounit) hgt_min, hgt_max, binwidth_hgt
   write(ounit) num_bins, num_bins2d
   write(ounit) bin
   write(ounit) bin2d

   ! read and write regcoeff info

   allocate( regcoeff1(1:num_bins) )
   allocate( regcoeff2(1:nk,1:num_bins2d) )
   allocate( regcoeff3(1:nk,1:nk,1:num_bins2d) )
   allocate( regcoeff1_r8(1:num_bins) )
   allocate( regcoeff2_r8(1:nk,1:num_bins2d) )
   allocate( regcoeff3_r8(1:nk,1:nk,1:num_bins2d) )

   infile = 'regcoeff1.dat'
   inquire(file=trim(infile), exist=isfile)
   if ( .not. isfile ) then
      write(*,*) 'STOP: ', trim(infile), ' does not exist.'
      stop
   end if
   open (iunit, file=trim(infile), form='unformatted', status='old')
   write(*,*) 'Reading from ', trim(infile)
   read(iunit) ni, nj, nk
   read(iunit) num_bins, num_bins2d
   read(iunit) regcoeff1
   regcoeff1_r8(:) = regcoeff1(:)
   write(ounit) regcoeff1_r8
   close(iunit)

   infile = 'regcoeff2.dat'
   inquire(file=trim(infile), exist=isfile)
   if ( .not. isfile ) then
      write(*,*) 'STOP: ', trim(infile), ' does not exist.'
      stop
   end if
   open (iunit, file=trim(infile), form='unformatted', status='old')
   write(*,*) 'Reading from ', trim(infile)
   read(iunit) ni, nj, nk
   read(iunit) num_bins, num_bins2d
   read(iunit) regcoeff2
   regcoeff2_r8(:,:) = regcoeff2(:,:)
   write(ounit) regcoeff2_r8
   close(iunit)

   infile = 'regcoeff3.dat'
   inquire(file=trim(infile), exist=isfile)
   if ( .not. isfile ) then
      write(*,*) 'STOP: ', trim(infile), ' does not exist.'
      stop
   end if
   open (iunit, file=trim(infile), form='unformatted', status='old')
   write(*,*) 'Reading from ', trim(infile)
   read(iunit) ni, nj, nk
   read(iunit) num_bins, num_bins2d
   read(iunit) regcoeff3
   regcoeff3_r8(:,:,:) = regcoeff3(:,:,:)
   write(ounit) regcoeff3_r8
   close(iunit)

   ! done writing dimension, bin and regcoeff info

   deallocate( regcoeff1 )
   deallocate( regcoeff2 )
   deallocate( regcoeff3 )
   deallocate( regcoeff1_r8 )
   deallocate( regcoeff2_r8 )
   deallocate( regcoeff3_r8 )

   allocate( e_vec(1:nk,1:nk) )
   allocate( e_val(1:nk) )
   allocate( e_vec_loc(1:nk,1:nk,1:num_bins2d) )
   allocate( e_val_loc(1:nk,1:num_bins2d) )
   allocate( e_vec_r8(1:nk,1:nk) )
   allocate( e_val_r8(1:nk) )
   allocate( e_vec_loc_r8(1:nk,1:nk,1:num_bins2d) )
   allocate( e_val_loc_r8(1:nk,1:num_bins2d) )

   ! loop over individual files to gather and write out e_vec/e_val info

   do i = 1, nvar

      infile = 'be_'//trim(varnames(i))//'.dat'
      inquire(file=trim(infile), exist=isfile)
      if ( .not. isfile ) then
         write(*,*) 'STOP: ', trim(infile), ' does not exist.'
         stop
      end if
      open (iunit, file=trim(infile), form='unformatted', status='old')
      write(*,*) 'Reading from ', trim(infile)

      read(iunit) ni, nj, nk

      read(iunit) bin_type
      read(iunit) num_bins, num_bins2d
      read(iunit) bin
      read(iunit) bin2d

      read(iunit) this_var
      write(ounit) varout(i)

      read(iunit) var_dim
      if ( var_dim == 3 ) then
         read(iunit) e_vec
         read(iunit) e_val
         read(iunit) e_vec_loc
         read(iunit) e_val_loc
         e_vec_r8 = e_vec
         e_val_r8 = e_val
         e_vec_loc_r8 = e_vec_loc
         e_val_loc_r8 = e_val_loc
         write(ounit) nk, num_bins2d
         write(ounit) e_vec_r8
         write(ounit) e_val_r8
         write(ounit) e_vec_loc_r8
         write(ounit) e_val_loc_r8
      else if ( var_dim == 2 ) then
         read(iunit) e_vec(1:1,1:1)
         read(iunit) e_val(1:1)
         read(iunit) e_vec_loc(1:1,1:1,:)
         read(iunit) e_val_loc(1:1,:)
         e_vec_r8 = e_vec
         e_val_r8 = e_val
         e_vec_loc_r8 = e_vec_loc
         e_val_loc_r8 = e_val_loc
         write(ounit) nk_2d, num_bins2d
         write(ounit) e_vec_r8(1:1,1:1)
         write(ounit) e_val_r8(1:1)
         write(ounit) e_vec_loc_r8(1:1,1:1,:)
         write(ounit) e_val_loc_r8(1:1,:)
      end if
      close(iunit)
   end do ! var loop for e_val/e_vec

   ! read individual files again to gather and write out lengthscales

   allocate( scale_length(1:nk) )
   allocate( scale_length_r8(1:nk) )

   do i = 1, nvar
      infile = 'be_'//trim(varnames(i))//'.dat'
      open (iunit, file=trim(infile), form='unformatted', status='old')
      !write(*,*) 'Reading from ', trim(infile)
      read(iunit) ni, nj, nk
      read(iunit) bin_type
      read(iunit) num_bins, num_bins2d
      read(iunit) bin
      read(iunit) bin2d

      read(iunit) this_var
      write(ounit) varout(i)

      read(iunit) var_dim
      if ( var_dim == 3 ) then
         read(iunit) e_vec
         read(iunit) e_val
         read(iunit) e_vec_loc
         read(iunit) e_val_loc
         read(iunit) scale_length
         scale_length_r8 = scale_length
         write(ounit) scale_length_r8
      else if ( var_dim == 2 ) then
         read(iunit) e_vec(1:1,1:1)
         read(iunit) e_val(1:1)
         read(iunit) e_vec_loc(1:1,1:1,:)
         read(iunit) e_val_loc(1:1,:)
         read(iunit) scale_length(1:1)
         scale_length_r8 = scale_length
         write(ounit) scale_length_r8(1:1)
      end if
      close(iunit)
   end do
   close(ounit)

   deallocate( e_vec )
   deallocate( e_val )
   deallocate( e_vec_loc )
   deallocate( e_val_loc )
   deallocate( e_vec_r8 )
   deallocate( e_val_r8 )
   deallocate( e_vec_loc_r8 )
   deallocate( e_val_loc_r8 )

   deallocate (scale_length)
   deallocate (scale_length_r8)

   write(*,*) 'Done writing be.dat for cv_options=5'

end program be_readwrite
