program gen_be_addmean
!
!---------------------------------------------------------------------- 
!  Purpose : add updated ensemble mean and perturbations to get analysis
!
!  Arthur P. Mizzi (NCAR/MMM) April 2011
!
!----------------------------------------------------------------------
! xlf90 -C -I${NETCDF}/include -L${NETCDF}/lib -lnetcdf -lnetcdff -o da_run_add_mean.exe da_run_add_mean.f90
!
   implicit none 

#include "netcdf.inc"

   integer, parameter    :: max_num_vars = 50              ! Maximum number of variables.
   integer, parameter    :: max_num_dims = 20              ! Maximum number of dimensions.
   integer, parameter    :: unit = 100                     ! Unit number.

   integer               :: num_members                    ! Ensemble size.
   integer               :: nv                             ! Number of variables.
   integer               :: length                         ! Filename length.
   integer               :: rcode                          ! NETCDF return code.
   integer               :: cdfid_mean, cdfid_pert         ! NETCDF file IDs.
   integer               :: nijkv, nijkv_mean, nijkv_pert  ! Array counter.
   integer               :: index                          ! Array index.
   integer               :: member, v, o, i, j, k, ijkv    ! Loop counters.
   integer               :: ivtype                         ! 4=integer, 5=real, 6=d.p.
   integer               :: natts                          ! Number of field attributes.
   integer               :: istart_mean(1:max_num_vars)    ! Start index.
   integer               :: istart_pert(1:max_num_vars)    ! Start index.
   integer               :: id_var_mean(1:max_num_vars)    ! NETCDF variable ID.
   integer               :: id_var_pert(1:max_num_vars)    ! NETCDF variable ID.
   integer               :: ndims_mean(1:max_num_vars)     ! Number of field dimensions.
   integer               :: ndims_pert(1:max_num_vars)     ! Number of field dimensions.
   integer               :: one(1:max_num_dims)            ! Array of dimension starts.
   integer               :: dimids_mean(1:max_num_dims)    ! Array of dimension IDs.
   integer               :: dimids_pert(1:max_num_dims)    ! Array of dimension IDs.
   integer               :: dims_mean(1:max_num_vars,1:max_num_dims)      ! Array of dimensions.
   integer               :: dims_pert(1:max_num_vars,1:max_num_dims)      ! Array of dimensions.

   real (kind=4), allocatable     :: data_r_3d(:,:,:)               ! 3-D Data array.
   real (kind=4), allocatable     :: data_r_4d(:,:,:,:)             ! 4-D Data array.
   real, allocatable              :: xf_mean(:)                     ! Ensemble mean.
   real, allocatable              :: xf_pert(:)                     ! Ensemble perturbations.

   character (len=3)     :: ce                        ! Member index -> character.
   character (len=10)    :: cv(1:max_num_vars)        ! Default array of variable names.
   character (len=160)   :: path                      ! Input file path. 
   character (len=160)   :: file_mean                 ! Input file_mean name. 
   character (len=160)   :: file_pert                 ! Input file_pert name. 
   character (len=160)   :: input_file_mean           ! Input file. 
   character (len=160)   :: input_file_pert           ! Input file. 

   namelist / add_mean_nl / num_members, nv, cv, path, file_mean, file_pert
!
! OPEN AND READ NAMELIST DATA   
   open(unit=unit, file='add_mean_nl.nl', &
        form='formatted', status='old', action='read')
   read(unit, add_mean_nl)
   close(unit)
!
   print *, 'NUM_MEMBERS ', num_members
   print *, 'NV ', nv
   print *, 'CV ', (trim(cv(i)),i=1,nv)
   print *, 'PATH ', trim(path)
   print *, 'FILE_MEAN ', trim(file_mean)
   print *, 'FILE_PERT ', trim(file_pert)
!
! OPEN THE ENSEMBLE MEAN FILE
   input_file_mean = trim(path)//'/'//trim(file_mean)
!   print *, 'INPUT_FILE_MEAN', trim(input_file_mean)
   length = len_trim(input_file_mean)
   rcode = nf_open( input_file_mean(1:length), NF_WRITE, cdfid_mean )
!
! OPEN THE ENSEMBLE PERT FILE
   input_file_pert = trim(path)//'/'//trim(file_pert)
!   print *, 'INPUT_FILE_PERT', trim(input_file_pert)
   length = len_trim(input_file_pert)
   rcode = nf_open( input_file_pert(1:length), NF_WRITE, cdfid_pert )
!
! LOOP OVER VARIABLES (v) TO GET DIMENSIONS
   do v = 1, nv
!
! GET VARIABLE IDENTIFIER        
      rcode = nf_inq_varid ( cdfid_mean, cv(v), id_var_mean(v) )
      if ( rcode /= 0 ) then
         write(UNIT=6,FMT='(A,A)') &
         cv(v), ' variable is not in input file mean'
         stop
      end if 
      rcode = nf_inq_varid ( cdfid_pert, cv(v), id_var_pert(v) )
      if ( rcode /= 0 ) then
         write(UNIT=6,FMT='(A,A)') &
         cv(v), ' variable is not in input file pert'
         stop
      end if 
      if (id_var_mean(v) .ne. id_var_pert(v)) then
        print *, 'mean var id not equal pert var id ',v,id_var_mean(v),id_var_pert(v)
        stop
      end if
!
! CHECK IF VARIABLE IS TYPE REAL
      dimids_mean = 0
      rcode = nf_inq_var( cdfid_mean, id_var_mean(v), cv(v), ivtype, ndims_mean(v), dimids_mean, natts )
      if ( ivtype /= 5 ) then
         write(UNIT=6,FMT='(A,A)') cv(v), ' variable is not real type in file_mean'
         stop
      end if
      dimids_pert = 0
      rcode = nf_inq_var( cdfid_pert, id_var_pert(v), cv(v), ivtype, ndims_pert(v), dimids_pert, natts )
      if ( ivtype /= 5 ) then
         write(UNIT=6,FMT='(A,A)') cv(v), ' variable is not real type in file_pert'
         stop
      end if
!
! GET DIMENSIONS FOR THIS VARIABLE
      dims_mean(v,:) = 0
      do i = 1, ndims_mean(v)
         rcode = nf_inq_dimlen( cdfid_mean, dimids_mean(i), dims_mean(v,i) )
      end do
      dims_pert(v,:) = 0
      do i = 1, ndims_pert(v)
         rcode = nf_inq_dimlen( cdfid_pert, dimids_pert(i), dims_pert(v,i) )
      end do
   end do
!
! SET COLUMN VECTOR INDEXING ARRAY
   one = 1
   istart_mean(1) = 1
   do v = 2, nv
      istart_mean(v) = istart_mean(v-1) + product(dims_mean(v-1,1:ndims_mean(v-1)-1))
   end do
   one = 1
   istart_pert(1) = 1
   do v = 2, nv
      istart_pert(v) = istart_pert(v-1) + product(dims_pert(v-1,1:ndims_pert(v-1)-1))
   end do
!
! CALCULATE SIZE OF COLUMN VECTOR AND ALLOCATE ARRAY
   nijkv_mean = istart_mean(nv) + product(dims_mean(nv,1:ndims_mean(nv)-1)) - 1
   nijkv_pert = istart_pert(nv) + product(dims_pert(nv,1:ndims_pert(nv)-1)) - 1
   nijkv=nijkv_mean
   if(nijkv_mean .ne. nijkv_pert) then
      print *, 'nijkv_mean not equal nijkv_pert ',nijkv_mean, nijkv_pert
      stop
   end if 

   allocate(xf_mean(1:nijkv))
   allocate(xf_pert(1:nijkv))
!
! LOOP OVER VARIABLES TO BE UPDATED
   do v = 1, nv
      print *, 'process variable ',v
!
! GET MEAN FIELD VARIABLE IDENTIFIER        
      rcode = nf_inq_varid ( cdfid_mean, cv(v), id_var_mean(v) )
      if ( rcode /= 0 ) then
         write(UNIT=6,FMT='(A,A)') &
         cv(v), ' variable is not in input file mean'
         stop
      end if 
!      print *, 'save mean field'
      index = istart_mean(v)
      if(ndims_mean(v) == 3) then
         allocate( data_r_3d(dims_mean(v,1),dims_mean(v,2),dims_mean(v,3)))
         rcode = nf_get_vara_real( cdfid_mean, id_var_mean(v), one, dims_mean(v,:), data_r_3d)
         if(rcode.ne.0) then
            print *, 'Error read ',trim(input_file_mean),' variable ',cv(v)
            print *, 'rcode ',rcode
            stop
         end if
         do k = 1, dims_mean(v,3)
            do j = 1, dims_mean(v,2)
               do i = 1, dims_mean(v,1)
                  xf_mean(index) = data_r_3d(i,j,k)
                  index = index + 1
               end do
            end do
         end do
         deallocate( data_r_3d )
      end if
      if(ndims_mean(v) == 4) then
         allocate( data_r_4d(dims_mean(v,1),dims_mean(v,2),dims_mean(v,3),dims_mean(v,4)))
         rcode = nf_get_vara_real( cdfid_mean, id_var_mean(v), one, dims_mean(v,:), data_r_4d)
         if(rcode.ne.0) then
            print *, 'Error read ',trim(input_file_mean),' variable ',cv(v)
            print *, 'rcode ',rcode
            stop
         end if
         do k = 1, dims_mean(v,3)
            do j = 1, dims_mean(v,2)
               do i = 1, dims_mean(v,1)
                  xf_mean(index) = data_r_4d(i,j,k,1)
                  index = index + 1
               end do
            end do
         end do
         deallocate( data_r_4d )
      end if
!
! GET PERT FIELD VARIABLE IDENTIFIER        
      rcode = nf_inq_varid ( cdfid_pert, cv(v), id_var_pert(v) )
      if ( rcode /= 0 ) then
         write(UNIT=6,FMT='(A,A)') &
         cv(v), ' variable is not in input file pert'
         stop
      end if 
!      print *, 'save pert field'
      index = istart_pert(v)
      if(ndims_pert(v) == 3) then
         allocate( data_r_3d(dims_pert(v,1),dims_pert(v,2),dims_pert(v,3)))
         rcode = nf_get_vara_real( cdfid_pert, id_var_pert(v), one, dims_pert(v,:), data_r_3d)
         if(rcode.ne.0) then
            print *, 'Error read ',trim(input_file_pert),' variable ',cv(v)
            print *, 'rcode ',rcode
            stop
         endif
         do k = 1, dims_pert(v,3)
            do j = 1, dims_pert(v,2)
               do i = 1, dims_pert(v,1)
                  xf_pert(index) = data_r_3d(i,j,k)
                  index = index + 1
               end do
            end do
         end do
         deallocate( data_r_3d )
      end if
      if(ndims_pert(v) == 4) then
         allocate( data_r_4d(dims_pert(v,1),dims_pert(v,2),dims_pert(v,3),dims_pert(v,4)))
         rcode = nf_get_vara_real( cdfid_pert, id_var_pert(v), one, dims_pert(v,:), data_r_4d)
         if(rcode.ne.0) then
            print *, 'Error read ',trim(input_file_pert),' variable ',cv(v)
            print *, 'rcode ',rcode
            stop
         end if
         do k = 1, dims_pert(v,3)
            do j = 1, dims_pert(v,2)
               do i = 1, dims_pert(v,1)
                  xf_pert(index) = data_r_4d(i,j,k,1)
                  index = index + 1
               end do
            end do
         end do
         deallocate( data_r_4d )
      end if
!
! ADD THE MEAN AND PERT FIELDS
!      print *, 'add mean and pert'
      do ijkv = 1, nijkv
         xf_pert(ijkv) = xf_mean(ijkv) + xf_pert(ijkv)
      end do
!
! WRITE UPDATED FIELD TO FILE_PERT
!      print *, 'write total field'
      index = istart_pert(v)
      if(ndims_pert(v) == 3) then
         allocate( data_r_3d(dims_pert(v,1),dims_pert(v,2),dims_pert(v,3)))
         do k = 1, dims_pert(v,3)
            do j = 1, dims_pert(v,2)
               do i = 1, dims_pert(v,1)
                  data_r_3d(i,j,k) = xf_pert(index)
                  index = index + 1
               end do
            end do
         end do
         call ncvpt( cdfid_pert, id_var_pert(v), one, dims_pert(v,:), data_r_3d, rcode)
         deallocate( data_r_3d )
      end if
      if(ndims_pert(v) == 4) then
         allocate( data_r_4d(dims_pert(v,1),dims_pert(v,2),dims_pert(v,3),dims_pert(v,4)))
         do k = 1, dims_pert(v,3)
            do j = 1, dims_pert(v,2)
               do i = 1, dims_pert(v,1)
                  data_r_4d(i,j,k,1) = xf_pert(index)
                  index = index + 1
               end do
            end do
         end do
         call ncvpt( cdfid_pert, id_var_pert(v), one, dims_pert(v,:), data_r_4d, rcode)
         deallocate( data_r_4d )
      endif
   enddo
   rcode = nf_close( cdfid_mean )
   rcode = nf_close( cdfid_pert )
   deallocate(xf_mean)
   deallocate(xf_pert)
end program gen_be_addmean
