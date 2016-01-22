program gen_be_etkf

!---------------------------------------------------------------------- 
!  Purpose : Perform an Ensemble Transform Kalman Filter (ETKF) rescaling
!  of WRF ensemble forecast data.
!
!  Dale Barker (NCAR/MMM)     January 2006    WRF/ETKF interface. 
!  Xuguang Wang (NOAA)        January 2006    ETKF algorithm.
!  Arthur P. Mizzi (NCAR/MMM) February 2011:  Modified for ETKF obs filtering, ETKF obs error filtering, 
!                                             and for LETKF data.  Also, the ETKF interface uses the 
!                                             extended ob.etkf files (including additional metadata for
!                                             filtering and LETKF formulation).  Also, for hybrid cycling
!                                             algorithm, this procedure modified to output ensemble 
!                                             perturbations.      
!
!----------------------------------------------------------------------

#ifdef crayx1
#define iargc ipxfargc
#endif

   use da_control, only : trace_use, stdout,filename_len
!   use da_gen_be
   use da_etkf, only : da_solve_etkf, da_solve_letkf, rand_filter
   use da_reporting, only : da_error, message

   implicit none

#include "netcdf.inc"

   integer, parameter    :: max_num_vars = 50         ! Maximum number of variables.
   integer, parameter    :: max_num_dims = 20         ! Maximum number of dimensions.
   integer, parameter    :: unit = 100                ! Unit number.

   character (len=filename_len)   :: input_file                ! Input file. 
   character (len=filename_len)   :: output_file               ! Output file. 
   character (len=3)     :: ce                        ! Member index -> character.

   integer               :: num_members               ! Ensemble size.
   integer               :: nv                        ! Number of variables.
   integer               :: num_obs                   ! Number of observations.
   integer               :: naccumt1                  ! Number of previous cycles.
   integer               :: naccumt2                  ! Number of previous cycles.
   integer               :: nstartaccum1              ! Cycle from which naccumt1 cycle starts.
   integer               :: nstartaccum2              ! Cycle from which naccumt2 cycle starts.
   integer               :: nout                      ! Output record for inn. vec./ob. error var.
   integer               :: length                    ! Filename length.
   integer               :: rcode                     ! NETCDF return code.
   integer               :: cdfid                     ! NETCDF file IDs.
   integer               :: member, v, o, i, j, k, ijkv ! Loop counters.
   integer               :: ivtype                    ! 4=integer, 5=real, 6=d.p.
   integer               :: natts                     ! Number of field attributes.

   integer               :: index                     ! Array index.
   integer               :: nijkv                     ! Array counter.
   integer               :: iend                      ! End of array 
   real                  :: num_members_inv           ! 1 / ensemble size.
   real                  :: tainflatinput             ! Pre-specified inflation, if not using adaptive inflation.
   real                  :: rhoinput                  ! Pre-specified inflation, if not using adaptive rho factor.
   real                  :: ds                        ! Grid resolution (m).

   character(len=10)     :: cv(1:max_num_vars)        ! Default array of variable names.
   integer               :: id_var(1:max_num_vars)    ! NETCDF variable ID.
   integer               :: ndims(1:max_num_vars)     ! Number of field dimensions.
   integer               :: istart(1:max_num_vars)    ! Start index.
   integer               :: dimids(1:max_num_dims)    ! Array of dimension IDs.
   integer               :: one(1:max_num_dims)       ! Array of dimension starts.
   integer               :: dims(1:max_num_vars,1:max_num_dims)      ! Array of dimensions.
   integer               :: dim_prod(1:max_num_dims)  ! Product of array dimensions.
   character (len=NF_MAX_NAME) :: var_name            ! Returned variable name from NF_INQ_VAR

   real (kind=4), allocatable     :: data_r_3d(:,:,:)               ! 3-D Data array.
   real (kind=4), allocatable     :: data_r_4d(:,:,:,:)             ! 4-D Data array.

   real, pointer         :: xf(:,:)                   ! Ensemble perturbations.
   real, pointer         :: xf_mean(:)                ! Ensemble perturbation mean.
   real, pointer         :: xf_vari(:)                ! Ensemble perturbation variance.
   real, pointer         :: y(:,:)                    ! H(xf).
   real, pointer         :: sigma_o2(:)               ! Ob error variance.
   real, pointer         :: yo(:)                     ! Observation.
   real, pointer         :: ens_mean(:)               ! Variable ensemble mean.
   real, pointer         :: ens_stdv_pert_prior(:)    ! Variable prior perturbation std. dev.
   real, pointer         :: ens_stdv_pert_poste(:)    ! Variable posterior perturbation std. dev.

   character(len=150)    :: infl_let_file             ! Inflation factor data filename and path.
   character(len=150)    :: infl_fac_file             ! Inflation factor data filename and path.
   character(len=150)    :: eigen_val_file            ! Eigen value filename and path.
   character(len=150)    :: inno2_val_file            ! Innovation value filename and path.
   character(len=150)    :: proj2_val_file            ! Projection value filename and path.
   real                  :: num_members_m1_inv        ! 1 / (ensemble size-1).
   
   real, pointer                 :: apm_yo(:,:)
   real, pointer                 :: apm_y(:,:)
   real, pointer                 :: apm_sigma_o2(:,:)
   character(len=10), pointer    :: apm_obs_type(:,:)
   real, pointer                 :: apm_type(:,:)
   real, pointer                 :: apm_subtype(:,:)
   real, pointer                 :: apm_obslat(:,:)
   real, pointer                 :: apm_obslon(:,:)
   real, pointer                 :: apm_obsprs(:,:)
   real, pointer                 :: apm_obstim(:,:)
   integer, pointer              :: apm_irec(:,:)
   character(len=10), pointer    :: yo_obs_typ(:)
   real, pointer                 :: yo_typ(:)
   real, pointer                 :: yo_subtyp(:)
   real, pointer                 :: yo_lat(:)
   real, pointer                 :: yo_lon(:)
   real, pointer                 :: yo_prs(:)
   real, pointer                 :: yo_tim(:)
   real, pointer                 :: xf_lon(:)
   real, pointer                 :: xf_lat(:)
   real (kind=4), allocatable    :: ens_lon_T(:,:),ens_lon_u(:,:),ens_lon_v(:,:)
   real (kind=4), allocatable    :: ens_lat_T(:,:),ens_lat_u(:,:),ens_lat_v(:,:)
   real (kind=4), allocatable    :: ens_prs_T(:,:,:),ens_pbs_T(:,:,:)
   integer                       :: ityp,idim
   integer                       :: id_lat_T,id_lat_u,id_lat_v
   integer                       :: id_lon_T,id_lon_u,id_lon_v
   integer                       :: id_prs_T,id_pbs_T
   integer                       :: ndim_lat_T,ndim_lat_u,ndim_lat_v
   integer                       :: ndim_lon_T,ndim_lon_u,ndim_lon_v
   integer                       :: ndim_prs_T,ndim_pbs_T
   integer, pointer              :: dim_lat_T(:),dim_lat_u(:),dim_lat_v(:)
   integer, pointer              :: dim_lon_T(:),dim_lon_u(:),dim_lon_v(:)
   integer, pointer              :: dim_prs_T(:),dim_pbs_T(:)

   integer                       :: apm_num_obs
   integer                       :: apm_icnt
   integer                       :: apm_reject
   integer                       :: apm_imem
   logical                       :: infl_fac_TRNK,infl_fac_WG03,infl_fac_WG07,letkf_flg
   logical                       :: infl_fac_BOWL, rand_filt 
   integer, allocatable          :: ijk_idx(:,:,:,:)
   integer, allocatable          :: apl_idx(:,:)
   integer, allocatable          :: apl_ndim(:)
   integer                       :: napl1, napl2, nxs, nys, nzs, ixy, idx, apm_unit
   character(len=150),dimension(:),allocatable    :: filt_ob_etkf              ! Filtered ob.etkf file.
   integer                       :: rnd_seed, rnd_nobs, nobs_flt, nseed

   logical                       :: etkf_erro_flg, etkf_inno_flg, etkf_wrfda  
   real                          :: etkf_erro_max, etkf_erro_min
   real                          :: etkf_inno_max, etkf_inno_min

   namelist / gen_be_etkf_nl / num_members, nv, cv, &
                               naccumt1, naccumt2, nstartaccum1, nstartaccum2, &
                               nout, tainflatinput, rhoinput, infl_fac_file, &
                               eigen_val_file, inno2_val_file, proj2_val_file, &
                               infl_fac_TRNK, infl_fac_WG03, infl_fac_WG07, &
                               infl_fac_BOWL, letkf_flg, rand_filt, rnd_seed, &
                               rnd_nobs, infl_let_file, etkf_erro_max, etkf_erro_min, &
                               etkf_inno_max, etkf_inno_min, etkf_erro_flg, etkf_inno_flg, &
                               etkf_wrfda
   
!---------------------------------------------------------------------------------------------
   write(stdout,'(/a)')' [1] Initialize information.'
!-----------------------------------------------------------------------------------------

   num_members = 56
   nv = 1
   cv = "U"
   naccumt1 = 0
   naccumt2 = 0
   nstartaccum1 = 0
   nstartaccum2 = 0
   nout = 1 
   tainflatinput = 0.0
   rhoinput = 0.0
   infl_fac_file  = "inflation_factor.dat"
   infl_let_file  = "inflation_letkf.dat"
   eigen_val_file = "eigen_value.dat"
   inno2_val_file = "innovation_value.dat"
   proj2_val_file = "projection_value.dat"

   open(unit=unit, file='gen_be_etkf_nl.nl', &
        form='formatted', status='old', action='read')
   read(unit, gen_be_etkf_nl)
   close(unit)

   write(stdout,'(a,i4)')'   Number of ensemble members = ', num_members
   write(stdout,'(a,i4)')'   Number of prognostic variables = ', nv
   write(stdout,'(50a)')'    List of prognostic variables = ', cv(1:nv)
   write(stdout,'(a,i4)')'   naccumt1 = ', naccumt1
   write(stdout,'(a,i4)')'   naccumt2 = ', naccumt2
   write(stdout,'(a,i4)')'   nstartaccum1 = ', nstartaccum1
   write(stdout,'(a,i4)')'   nstartaccum2 = ', nstartaccum2
   write(stdout,'(a,i4)')'   nout = ', nout
   write(stdout,'(a,f15.5)')'   tainflatinput = ', tainflatinput
   write(stdout,'(a,f15.5)')'   rhoinput = ', rhoinput
   write(stdout,'(150a)')'   infl_fac_file = ',infl_fac_file
   write(stdout,'(150a)')'   infl_let_file = ',infl_let_file
   write(stdout,'(150a)')'   eigen_val_file = ',eigen_val_file
   write(stdout,'(150a)')'   inno2_val_file = ',inno2_val_file
   write(stdout,'(150a)')'   proj2_val_file = ',proj2_val_file
   write(stdout,'(a,l10)')'   infl_fac_TRNK = ',infl_fac_TRNK
   write(stdout,'(a,l10)')'   infl_fac_WG03 = ',infl_fac_WG03
   write(stdout,'(a,l10)')'   infl_fac_WG07 = ',infl_fac_WG07
   write(stdout,'(a,l10)')'   infl_fac_BOWL = ',infl_fac_BOWL
   write(stdout,'(a,l10)')'   letkf_flag = ',letkf_flg
   write(stdout,'(a,i20)')'   rnd_seed = ',rnd_seed
   write(stdout,'(a,i20)')'   rnd_nobs = ',rnd_nobs
   write(stdout,'(a,f15.5)')' etkf_erro_max = ',etkf_erro_max
   write(stdout,'(a,f15.5)')' etkf_erro_min = ',etkf_erro_min
   write(stdout,'(a,f15.5)')' etkf_inno_max = ',etkf_inno_max
   write(stdout,'(a,f15.5)')' etkf_inno_min = ',etkf_inno_min
   write(stdout,'(a,l10)')'   etkf_erro_flg = ',etkf_erro_flg
   write(stdout,'(a,l10)')'   etkf_inno_flg = ',etkf_inno_flg
   write(stdout,'(a,l10)')'   etkf_wrfda = ',etkf_wrfda

   num_members_inv = 1.0 / real(num_members)
   num_members_m1_inv = 1.0 / real(num_members)

   allocate( ens_mean(1:nv) )
   allocate( ens_stdv_pert_prior(1:nv) )
   allocate( ens_stdv_pert_poste(1:nv) )

!-----------------------------------------------------------------------------------------
   write(stdout,'(/a)')' [2] Read observation information.'
!-----------------------------------------------------------------------------------------
!
! LOOP OVER ENSEMBLE MEMBERS AND READ OB.ETKF.exxx DATA
   do member = 1, num_members
      write(unit=ce,FMT='(i3.3)') member
      input_file = 'ob.etkf.e'//ce  
      open(unit, file = input_file, status='old')
      read(unit,*) apm_num_obs
      print *, ' Number of unfiltered observations = ',apm_num_obs,member
      if ( member == 1 ) then
         allocate( apm_yo(1:apm_num_obs,1:num_members) )
         allocate( apm_y(1:apm_num_obs,1:num_members) )
         allocate( apm_sigma_o2(1:apm_num_obs,1:num_members) )
         allocate( apm_obs_type(1:apm_num_obs,1:num_members) )
         allocate( apm_type(1:apm_num_obs,1:num_members) )
         allocate( apm_subtype(1:apm_num_obs,1:num_members) )
         allocate( apm_obslat(1:apm_num_obs,1:num_members) )
         allocate( apm_obslon(1:apm_num_obs,1:num_members) )
         allocate( apm_obsprs(1:apm_num_obs,1:num_members) )
         allocate( apm_obstim(1:apm_num_obs,1:num_members) )
         allocate( apm_irec(1:apm_num_obs,1:num_members) )
      end if
!
! READ OB.ETKF DATA
      if(etkf_wrfda) then
         do o = 1, apm_num_obs
            read(unit,'(3f17.7)') apm_yo(o,member),apm_y(o,member),apm_sigma_o2(o,member)
         end do
         close(unit)
      else
         do o = 1, apm_num_obs
            read(unit,'(3f17.7,2x,a10,2x,2(f6.0,2x),4(f8.2,2x),i10)') &
            apm_yo(o,member),apm_y(o,member),apm_sigma_o2(o,member),apm_obs_type(o,member), &
            apm_type(o,member),apm_subtype(o,member),apm_obslat(o,member), &
            apm_obslon(o,member),apm_obsprs(o,member),apm_obstim(o,member),apm_irec(o,member)
         end do
      end if
   end do    
!
   apm_icnt=0
!
! FILTER OBS BASED ON TYPE 
! (SELECT ONLY SOUNDING OBS)
   if(etkf_wrfda) then
      write(stdout,'(a)') 'etkf_wrfda is true: NO ETKF OBS FILTERING '
      apm_icnt = apm_num_obs
   else
      do o = 1, apm_num_obs
         if((apm_type(o,1).ne.220.) .and. (apm_type(o,1).ne.120.)) then
            apm_reject=1
            do member=1,num_members
               apm_irec(o,member)=-999        
            end do 
         else
            apm_reject=0
            do member=1, num_members
!
! OBS ERROR TEST
               if((etkf_erro_flg .and. &
                  (apm_sigma_o2(o,member)/apm_yo(o,member).le.etkf_erro_min) .or. &
                  (apm_sigma_o2(o,member)/apm_yo(o,member).ge.etkf_erro_max)) .or. &
!
! INNOVATION TEST
                  (etkf_inno_flg .and. & 
                  (abs(apm_y(o,member))/apm_yo(o,member).le.etkf_inno_min) .or. &
                  (abs(apm_y(o,member))/apm_yo(o,member).ge.etkf_inno_max))) then  
                  apm_reject=1
               endif            
            end do
            if(apm_reject.eq.1) then 
               do member=1,num_members
                  apm_irec(o,member)=-999        
               end do 
            else         
               apm_icnt=apm_icnt+1
            end if
         end if
      end do
   end if     
!
! ASSIGN FILTERED OBS TO ORIGINAL ARRAYS 
   write(stdout,'(a,i10)')'   Number of filtered observations = ', apm_icnt
   nobs_flt = apm_icnt
!
! CONDUCT RANDOM FILTERING
   if(etkf_wrfda) then
      num_obs=nobs_flt
      allocate( y(1:num_obs,1:num_members) )
      allocate( sigma_o2(1:num_obs) )
      allocate( yo(1:num_obs) )
      apm_icnt=0
      do o=1,num_obs
         yo(o)=apm_yo(o,1)
         sigma_o2(o)=apm_sigma_o2(o,1)
         do member=1,num_members
            y(o,member)=apm_y(o,member)
         end do
      end do 
   else
      if(rand_filt) then
         nseed=rnd_seed
         num_obs=rnd_nobs
         allocate( y(1:num_obs,1:num_members) )
         allocate( sigma_o2(1:num_obs) )
         allocate( yo(1:num_obs) )
         allocate( yo_obs_typ(1:num_obs) )
         allocate( yo_typ(1:num_obs) )
         allocate( yo_subtyp(1:num_obs) )
         allocate( yo_lat(1:num_obs) )
         allocate( yo_lon(1:num_obs) )
         allocate( yo_prs(1:num_obs) )
         allocate( yo_tim(1:num_obs) )
         call rand_filter(apm_y,apm_sigma_o2,apm_yo,apm_obs_type,apm_type,apm_subtype, &
                         apm_obslat,apm_obslon,apm_obsprs,apm_obstim,y,sigma_o2,yo,yo_obs_typ, &
                         yo_typ,yo_subtyp,yo_lat,yo_lon,yo_prs,yo_tim,apm_irec,apm_num_obs, &
                         num_obs,nobs_flt,num_members,nseed)
      else   
         num_obs=nobs_flt
         allocate( y(1:num_obs,1:num_members) )
         allocate( sigma_o2(1:num_obs) )
         allocate( yo(1:num_obs) )
         allocate( yo_obs_typ(1:num_obs) )
         allocate( yo_typ(1:num_obs) )
         allocate( yo_subtyp(1:num_obs) )
         allocate( yo_lat(1:num_obs) )
         allocate( yo_lon(1:num_obs) )
         allocate( yo_prs(1:num_obs) )
         allocate( yo_tim(1:num_obs) )
         apm_icnt=0
         do o=1,apm_num_obs
            if(apm_irec(o,1).ne.-999) then
               apm_icnt=apm_icnt+1
               yo(apm_icnt)=apm_yo(o,1)
               yo_obs_typ(apm_icnt)=apm_obs_type(o,1)
               yo_typ(apm_icnt)=apm_type(o,1)
               yo_subtyp(apm_icnt)=apm_subtype(o,1)
               yo_lat(apm_icnt)=apm_obslat(o,1)
               yo_lon(apm_icnt)=apm_obslon(o,1)
               yo_prs(apm_icnt)=apm_obsprs(o,1)
               yo_tim(apm_icnt)=apm_obstim(o,1)
               sigma_o2(apm_icnt)=apm_sigma_o2(o,1)
               do member=1,num_members
                  y(apm_icnt,member)=apm_y(o,member)
               end do
!           print *, apm_icnt,yo(apm_icnt),y(apm_icnt,1),sigma_o2(apm_icnt), &
!           y(apm_icnt,2),y(apm_icnt,3),yo_lat(apm_icnt),yo_lon(apm_icnt), &
!           yo_prs(apm_icnt),yo_tim(apm_icnt) 
            endif
         enddo
      endif
!
! OPEN FILTERED OB.ETKF FILES 
      print *, "begin setup of ob.etkf.filt "
      allocate(filt_ob_etkf(num_members))
      do member=1,num_members
         write(unit=ce,FMT='(i3.3)') member
         filt_ob_etkf(member) = 'ob.etkf.filt.e'//ce 
         apm_unit=200+member 
         open(apm_unit, file = filt_ob_etkf(member), status='unknown')
         write(apm_unit,'(i17)') num_obs
      end do
      print *, "complete setup of ob.etkf.filt "
!
! SAVE FILTERED OB.ETKF FILES 
      do member=1,num_members
         apm_icnt=0
         do o=1, num_obs
               apm_icnt=apm_icnt+1
               apm_unit=200+member
               write(apm_unit,'(3f17.7,2x,a10,2x,2(f6.0,2x),4(f8.2,2x),i10)') &
               yo(o),y(o,member),sigma_o2(o),yo_obs_typ(o),yo_typ(o),yo_subtyp(o), &
               yo_lat(o),yo_lon(o),yo_prs(o),yo_tim(o),apm_icnt
         end do
      end do
      do member=1,num_members
         apm_unit=200+member
         close(apm_unit)
      enddo
      deallocate(filt_ob_etkf)
      deallocate(apm_yo,apm_y,apm_sigma_o2,apm_obs_type,apm_type,apm_subtype, &
      apm_obslat,apm_obslon,apm_obsprs,apm_obstim,apm_irec)  
   end if      
!
! MANIPULATE FILTERED OB.ETKF DATA FOR USE IN REMAINDER OF CODE
   do o = 1, num_obs
!
! CONVERT OBS ERROR STANDARD DEVIATION TO VARIANCE
      sigma_o2(o) = sigma_o2(o) * sigma_o2(o)
!
! CONVERT yo-H(xb) TO H(xb):
      do member = 1, num_members
         y(o,member) = yo(o) - y(o,member)
      end do
   end do

!-----------------------------------------------------------------------------------------
   write(stdout,'(/a)')' [3] Set up arrays using input ensemble mean forecast'
!-----------------------------------------------------------------------------------------
!
! OPEN THE ENSEMBLE MEAN DATA FILE
      input_file = 'etkf_input'
      length = len_trim(input_file)
      rcode = nf_open( input_file(1:length), NF_NOWRITE, cdfid )
!
! LOOP OVER VARIABLES (v) TO GET DIMENSIONS
      do v = 1, nv
!
! GET VARIABLE IDENTIFIER        
          rcode = nf_inq_varid ( cdfid, cv(v), id_var(v) )
         if ( rcode /= 0 ) then
            write(UNIT=message(1),FMT='(A,A)') &
            cv(v), ' variable is not in input file'
            call da_error(__FILE__,__LINE__,message(1:1)) 
         end if 
!
! CHECK IF VARIABLE IS TYPE REAL
         dimids = 0
         rcode = nf_inq_var( cdfid, id_var(v), cv(v), ivtype, ndims(v), dimids, natts )
         if ( ivtype /= 5 ) then
            write(UNIT=message(1),FMT='(A,A)') cv(v), ' variable is not real type'
            call da_error(__FILE__,__LINE__,message(1:1))
         end if
!
! GET DIMENSIONS FOR THIS VARIABLE
         dims(v,:) = 0
         do i = 1, ndims(v)
            rcode = nf_inq_dimlen( cdfid, dimids(i), dims(v,i) )
         end do
      end do
!
! SET COLUMN VECTOR INDEXING ARRAY
      one = 1
      istart(1) = 1
      do v = 2, nv
         istart(v) = istart(v-1) + product(dims(v-1,1:ndims(v-1)-1))
      end do
!
! CALCULATE SIZE OF COLUMN VECTOR AND ALLOCATE ARRAY
      nijkv = istart(nv) + product(dims(nv,1:ndims(nv)-1)) - 1
      allocate( xf_mean(1:nijkv) )
!   
! GET THE ENSEMBLE MEAN DATA
      do v = 1, nv
         index = istart(v)
         if(ndims(v) == 3) then
            allocate( data_r_3d(dims(v,1),dims(v,2),dims(v,3)))
            rcode = nf_get_vara_real( cdfid, id_var(v), one, dims(v,:), data_r_3d)
            do k = 1, dims(v,3)
               do j = 1, dims(v,2)
                  do i = 1, dims(v,1)
                     xf_mean(index) = data_r_3d(i,j,k)
                     index = index + 1
                  end do
               end do
            end do
            deallocate( data_r_3d )
         endif
         if(ndims(v) == 4) then
            allocate( data_r_4d(dims(v,1),dims(v,2),dims(v,3),dims(v,4)))
            rcode = nf_get_vara_real( cdfid, id_var(v), one, dims(v,:), data_r_4d)
            do k = 1, dims(v,3)
               do j = 1, dims(v,2)
                  do i = 1, dims(v,1)
                     xf_mean(index) = data_r_4d(i,j,k,1)
                     index = index + 1
                  end do
               end do
            end do
            deallocate( data_r_4d )
         endif
      enddo
!
! GET VAR IDs
      rcode=nf_inq_varid(cdfid,'XLONG',id_lon_T)
      rcode=nf_inq_varid(cdfid,'XLONG_U',id_lon_u)
      rcode=nf_inq_varid(cdfid,'XLONG_V',id_lon_v)
      rcode=nf_inq_varid(cdfid,'XLAT',id_lat_T)
      rcode=nf_inq_varid(cdfid,'XLAT_U',id_lat_u)
      rcode=nf_inq_varid(cdfid,'XLAT_V',id_lat_v)
      rcode=nf_inq_varid(cdfid,'P',id_prs_T)
      rcode=nf_inq_varid(cdfid,'PB',id_pbs_T)
!      print *,id_lon_T
!      print *,id_lon_u
!      print *,id_lon_v
!      print *,id_lat_T
!      print *,id_lat_u
!      print *,id_lat_v
!      print *,id_prs_T
!      print *,id_pbs_T
!
! GET DIMENSIONS
      dimids(:)=0
      var_name='XLONG'
      rcode=nf_inq_var(cdfid,id_lon_T,var_name,ityp,ndim_lon_T,dimids,natts)
      allocate(dim_lon_T(ndim_lon_T))
      do idim=1,ndim_lon_T
         rcode=nf_inq_dimlen(cdfid,dimids(idim),dim_lon_T(idim))
      enddo
!      print *,"dim_lon_T ",dim_lon_T
      dimids(:)=0
      var_name='XLONG_U'
      rcode=nf_inq_var(cdfid,id_lon_u,var_name,ityp,ndim_lon_u,dimids,natts)
      allocate(dim_lon_u(ndim_lon_u))
      do idim=1,ndim_lon_u
         rcode=nf_inq_dimlen(cdfid,dimids(idim),dim_lon_u(idim))
      enddo
!      print *,"dim_lon_u ",dim_lon_u
      dimids(:)=0
      var_name='XLONG_V'
      rcode=nf_inq_var(cdfid,id_lon_v,var_name,ityp,ndim_lon_v,dimids,natts)
      allocate(dim_lon_v(ndim_lon_v))
      do idim=1,ndim_lon_v
         rcode=nf_inq_dimlen(cdfid,dimids(idim),dim_lon_v(idim))
      enddo
!      print *,"dim_lon_v ",dim_lon_v
      dimids(:)=0
      var_name='XLAT'
      rcode=nf_inq_var(cdfid,id_lat_T,var_name,ityp,ndim_lat_T,dimids,natts)
      allocate(dim_lat_T(ndim_lat_T))
      do idim=1,ndim_lat_T
         rcode=nf_inq_dimlen(cdfid,dimids(idim),dim_lat_T(idim))
      enddo
!      print *,"dim_lat_T ",dim_lat_T
      dimids(:)=0
      var_name='XLAT_U'
      rcode=nf_inq_var(cdfid,id_lat_u,var_name,ityp,ndim_lat_u,dimids,natts)
      allocate(dim_lat_u(ndim_lat_u))
      do idim=1,ndim_lat_u
         rcode=nf_inq_dimlen(cdfid,dimids(idim),dim_lat_u(idim))
      enddo
!      print *,"dim_lat_u ",dim_lat_u
      dimids(:)=0
      var_name='XLAT_V'
      rcode=nf_inq_var(cdfid,id_lat_v,var_name,ityp,ndim_lat_v,dimids,natts)
      allocate(dim_lat_v(ndim_lat_v))
      do idim=1,ndim_lat_v
         rcode=nf_inq_dimlen(cdfid,dimids(idim),dim_lat_v(idim))
      enddo
!      print *,"dim_lat_v ",dim_lat_v
      dimids(:)=0
      var_name='P'
      rcode=nf_inq_var(cdfid,id_prs_T,var_name,ityp,ndim_prs_T,dimids,natts)
      allocate(dim_prs_T(ndim_prs_T))
      do idim=1,ndim_prs_T
         rcode=nf_inq_dimlen(cdfid,dimids(idim),dim_prs_T(idim))
      enddo
!      print *,"dim_prs_T ",dim_prs_T
      dimids(:)=0
      var_name='PB'
      rcode=nf_inq_var(cdfid,id_pbs_T,var_name,ityp,ndim_pbs_T,dimids,natts)
      allocate(dim_pbs_T(ndim_pbs_T))
      do idim=1,ndim_pbs_T
         rcode=nf_inq_dimlen(cdfid,dimids(idim),dim_pbs_T(idim))
      enddo
!      print *,"dim_pbs_T ",dim_pbs_T
!
! ALLOCATE TEMPORARY ARRAYS
      allocate(ens_lon_T(dim_lon_T(1),dim_lon_T(2)))
      allocate(ens_lon_u(dim_lon_u(1),dim_lon_u(2)))
      allocate(ens_lon_v(dim_lon_v(1),dim_lon_v(2)))
      allocate(ens_lat_T(dim_lat_T(1),dim_lat_T(2)))
      allocate(ens_lat_u(dim_lat_u(1),dim_lat_u(2)))
      allocate(ens_lat_v(dim_lat_v(1),dim_lat_v(2)))
      allocate(ens_prs_T(dim_prs_T(1),dim_prs_T(2),dim_prs_T(3)))
      allocate(ens_pbs_T(dim_pbs_T(1),dim_pbs_T(2),dim_pbs_T(3)))
!
! READ WRF GRID DATA
      one(:)=1
      rcode=nf_get_vara_real(cdfid,id_lon_T,one,dim_lon_T,ens_lon_T)
!      print *,"Complete read id_lon_T",ens_lon_T(1,1),ens_lon_T(22,22),ens_lon_T(44,44)
      rcode=nf_get_vara_real(cdfid,id_lon_u,one,dim_lon_u,ens_lon_u)
!      print *,"Complete read id_lon_u",ens_lon_u(1,1),ens_lon_u(22,22),ens_lon_u(45,44)
      rcode=nf_get_vara_real(cdfid,id_lon_v,one,dim_lon_v,ens_lon_v)
!      print *,"Complete read id_lon_v",ens_lon_v(1,1),ens_lon_v(44,44)
      rcode=nf_get_vara_real(cdfid,id_lat_T,one,dim_lat_T,ens_lat_T)
!      print *,"Complete read id_lat_T",ens_lat_T(1,1),ens_lat_T(44,44)
      rcode=nf_get_vara_real(cdfid,id_lat_u,one,dim_lat_u,ens_lat_u)
!      print *,"Complete read id_lat_u",ens_lat_u(1,1),ens_lat_u(44,44)
      rcode=nf_get_vara_real(cdfid,id_lat_v,one,dim_lat_v,ens_lat_v)
!      print *,"Complete read id_lat_v",ens_lat_v(1,1),ens_lat_v(44,44)
      rcode=nf_get_vara_real(cdfid,id_prs_T,one,dim_prs_T,ens_prs_T)
!      print *,"Complete read id_prs_T",ens_prs_T(1,1,1),ens_prs_T(44,44,27)
      rcode=nf_get_vara_real(cdfid,id_pbs_T,one,dim_pbs_T,ens_pbs_T)
!      print *,"Complete read id_pbs_T",ens_pbs_T(1,1,1),ens_pbs_T(44,44,27)
      rcode = nf_close( cdfid )
!
!-----------------------------------------------------------------------------------------
   write(stdout,'(/a)')' [4] Extract necessary fields from WRF ensemble forecasts.'
!-----------------------------------------------------------------------------------------
!
! ALLOCATE ENSEMBLE MEMBER DATA ARRAYS
      allocate( xf(1:nijkv,1:num_members) )
      allocate( xf_lon(1:nijkv),xf_lat(1:nijkv))
      allocate( xf_vari(1:nijkv) )
!
! LOOP THROUGH ENSEMBLE MEMBERS TO GET DATA 
      do member = 1, num_members
!         print *, "Open ob.etkf files ", member
         write(UNIT=ce,FMT='(i3.3)')member
         input_file = 'etkf_input.e'//ce
         length = len_trim(input_file)
         rcode = nf_open( input_file(1:length), NF_NOWRITE, cdfid )
         do v = 1, nv
            index = istart(v)
            if(ndims(v) == 3) then
               allocate( data_r_3d(dims(v,1),dims(v,2),dims(v,3)))
               rcode = nf_get_vara_real( cdfid, id_var(v), one, dims(v,:), data_r_3d)
               do k = 1, dims(v,3)
                  do j = 1, dims(v,2)
                     do i = 1, dims(v,1)
                        xf(index,member) = data_r_3d(i,j,k)
                        index = index + 1
                     end do
                  end do
               end do
               deallocate( data_r_3d )
            endif
            if(ndims(v) == 4) then
               allocate( data_r_4d(dims(v,1),dims(v,2),dims(v,3),dims(v,4)))
               rcode = nf_get_vara_real( cdfid, id_var(v), one, dims(v,:), data_r_4d)
               do k = 1, dims(v,3)
                  do j = 1, dims(v,2)
                     do i = 1, dims(v,1)
                        xf(index,member) = data_r_4d(i,j,k,1)
                        index = index + 1
                     end do
                  end do
               end do
               deallocate( data_r_4d )
            endif
         end do
         rcode = nf_close( cdfid )
      end do
!
! ASSIGN WRF GRID DATA (THE GRID ASSIGNMENT IS FIXED TO CERTAIN DATA)
! (THE VERTICAL COORDINATE IS NOT INCORPORATED YET)
!
! ALSO CREATE LETKF EFFICIENCY ARRAY
      nxs=dims(1,1)
      nys=dims(2,2)
      nzs=dims(3,3)

      napl1=(nxs-1)*(nys-1)
      napl2=2*nv*nzs

      allocate (ijk_idx(1:nv,1:nxs,1:nys,1:nzs))
      allocate (apl_idx(1:napl1,1:napl2))
      allocate (apl_ndim(1:napl1))
      
      ijk_idx=0
      apl_idx=0
      apl_ndim=0

      do v = 1, nv
         index = istart(v)
         if(cv(v) .eq. 'U') then
            do k = 1, dims(v,3)
               do j = 1, dims(v,2)
                  do i = 1, dims(v,1)
                     xf_lat(index) = ens_lat_u(i,j)
                     xf_lon(index) = ens_lon_u(i,j)
                     ijk_idx(v,i,j,k)=index
                     index = index + 1
                  end do
               end do
            end do
         end if
         if(cv(v) .eq. 'V') then
            do k = 1, dims(v,3)
               do j = 1, dims(v,2)
                  do i = 1, dims(v,1)
                     xf_lat(index) = ens_lat_v(i,j)
                     xf_lon(index) = ens_lon_v(i,j)
                     ijk_idx(v,i,j,k)=index
                     index = index + 1
                  end do
               end do
            end do
         end if
         if(cv(v).eq.'W' .or. cv(v).eq.'T' .or. cv(v).eq.'W' .or. &
         cv(v).eq.'PH' .or. cv(v).eq.'QVAPOR') then
            do k = 1, dims(v,3)
               do j = 1, dims(v,2)
                  do i = 1, dims(v,1)
                     xf_lat(index) = ens_lat_T(i,j)
                     xf_lon(index) = ens_lon_T(i,j)
                     ijk_idx(v,i,j,k)=index
                     index = index + 1
                  end do
               end do
            end do
         end if
         if(cv(v) .eq. 'MU') then
            do j = 1, dims(v,2)
               do i = 1, dims(v,1)
                  xf_lat(index) = ens_lat_v(i,j)
                  xf_lon(index) = ens_lon_v(i,j)
                  ijk_idx(v,i,j,1)=index
                  index = index + 1
               end do
            end do
         end if
      end do
!
! SETUP APPLICATION INDEX ARRAY
      ixy=0
      do j=1,nys-1
         do i=1,nxs-1
            ixy=ixy+1
! U   
            idx=0
            do k=1,dims(1,3)
               idx=idx+1
               apl_idx(ixy,idx)=ijk_idx(1,i,j,k)
               if(i.eq.nxs-1) then
                 idx=idx+1
                 apl_idx(ixy,idx)=ijk_idx(1,nxs,j,k)
               endif
            enddo
! V          
            do k=1,dims(2,3)
               idx=idx+1
               apl_idx(ixy,idx)=ijk_idx(2,i,j,k)
               if(j.eq.nys-1) then
                 idx=idx+1
                 apl_idx(ixy,idx)=ijk_idx(2,i,nys,k)
               endif
            enddo
! W          
            do k=1,dims(3,3)
               idx=idx+1
               apl_idx(ixy,idx)=ijk_idx(3,i,j,k)
            enddo
! PH       
            do k=1,dims(4,3)
               idx=idx+1
               apl_idx(ixy,idx)=ijk_idx(4,i,j,k)
            enddo
! T          
            do k=1,dims(5,3)
               idx=idx+1
               apl_idx(ixy,idx)=ijk_idx(5,i,j,k)
            enddo
! Q          
            do k=1,dims(6,3)
               idx=idx+1
               apl_idx(ixy,idx)=ijk_idx(6,i,j,k)
            enddo
! MU       
            idx=idx+1
            apl_idx(ixy,idx)=ijk_idx(7,i,j,1)
            apl_ndim(ixy)=idx
         enddo  
      enddo
!
! CONVERT ENSEMBLE FORECAST DATA TO PERTURBATIONS
      do ijkv = 1, nijkv
         xf(ijkv,1:num_members) = xf(ijkv,1:num_members) - xf_mean(ijkv)
         xf_vari(ijkv) = sum(xf(ijkv,1:num_members)**2) * num_members_m1_inv
      end do
!
! PRINT A PRIORI SPATIALLY AVERAGED ENSEMBLE MEAN AND SQUARE ROOT OF 
! SPATIALLY AVERAGED ENSEMBLE VARIANCE
     do v = 1, nv
        iend = istart(v) + product(dims(v,1:ndims(v)-1)) - 1
        ens_mean(v) = sum(xf_mean(istart(v):iend)) / real(iend - istart(v) + 1)
        ens_stdv_pert_prior(v) =sqrt( sum(xf_vari(istart(v):iend)) / &
                                    real(iend - istart(v) + 1) )
     end do

!-----------------------------------------------------------------------------------------
   write(stdout,'(/a)')' [5] Call ETKF:'
!-----------------------------------------------------------------------------------------
!
! CALL ETKF SOLVER
     if (letkf_flg) then
        call da_solve_letkf( nijkv, num_members, num_obs, xf, y, sigma_o2, yo, nout, &
                       naccumt1, naccumt2, nstartaccum1, nstartaccum2, tainflatinput, &
                       rhoinput, infl_fac_file, eigen_val_file, inno2_val_file, &
                       proj2_val_file, infl_fac_TRNK, infl_fac_WG03, infl_fac_WG07, &
                       infl_fac_BOWL, yo_lat, yo_lon, yo_prs, xf_lat, xf_lon, ijk_idx, &
                       apl_idx, apl_ndim, napl1, napl2, nxs, nys, nzs, nv, infl_let_file)
     else
        call da_solve_etkf( nijkv, num_members, num_obs, xf, y, sigma_o2, yo, nout, &
                       naccumt1, naccumt2, nstartaccum1, nstartaccum2, tainflatinput, &
                       rhoinput, infl_fac_file, eigen_val_file, inno2_val_file, &
                       proj2_val_file, infl_fac_TRNK, infl_fac_WG03, infl_fac_WG07, &
                       infl_fac_BOWL)
     endif
!
! CALCULATE THE POSTERIORI STANDARD DEVIATION
     do ijkv = 1, nijkv
        xf_vari(ijkv) = sum(xf(ijkv,1:num_members)**2) * num_members_m1_inv
     end do
     write(stdout,'(5a)')'   v', ' Variable  ', '    Ensemble Mean', &
                       '  Prior Pert StDv', ' Post. Pert. StDv'
     do v = 1, nv
        iend = istart(v) + product(dims(v,1:ndims(v)-1)) - 1
        ens_stdv_pert_poste(v) =sqrt( sum(xf_vari(istart(v):iend)) / &
                                    real(iend - istart(v) + 1) )
        write(stdout,'(i4,1x,a10,3f17.7)')v, cv(v), &
        ens_mean(v), ens_stdv_pert_prior(v), ens_stdv_pert_poste(v)
     end do

!-----------------------------------------------------------------------------------------
   write(stdout,'(/a)')' [6] Output ETKF analysis ensemble:'
!----------------------------------------------------------------------------------------- 
     do member = 1, num_members
        write(UNIT=ce,FMT='(i3.3)')member
        input_file = 'etkf_output.e'//ce
        rcode = nf_open( trim(input_file), NF_WRITE, cdfid )
        if ( rcode /= 0 ) then
           print *, 'MISSING FILE ',trim(input_file)
           print *, 'ERROR OPENING OUTPUT FILE: ', nf_strerror(rcode)
           stop
        end if
        do v = 1, nv
           index = istart(v)
           if(ndims(v) == 3) then
              allocate( data_r_3d(dims(v,1),dims(v,2),dims(v,3)))
              rcode = nf_get_vara_real( cdfid, id_var(v), one, dims(v,:), data_r_3d)
              do k = 1, dims(v,3)
                 do j = 1, dims(v,2)
                    do i = 1, dims(v,1)
!
! APM: modification to output perturbation only
!                       data_r_3d(i,j,k) = xf_mean(index) + xf(index,member)
                       data_r_3d(i,j,k) = xf(index,member)
                       index = index + 1
                    end do
                 end do
              end do
              call ncvpt( cdfid, id_var(v), one, dims(v,:), data_r_3d, rcode)
              deallocate( data_r_3d )
           endif
           if(ndims(v) == 4) then
              allocate( data_r_4d(dims(v,1),dims(v,2),dims(v,3),dims(v,4)))
              rcode = nf_get_vara_real( cdfid, id_var(v), one, dims(v,:), data_r_4d)
              do k = 1, dims(v,3)
                 do j = 1, dims(v,2)
                    do i = 1, dims(v,1)
!
! APM: modification to output perturbation only
!                       data_r_4d(i,j,k,1) = xf_mean(index) + xf(index,member)
                       data_r_4d(i,j,k,1) = xf(index,member)
                       index = index + 1
                    end do
                 end do
              end do
              call ncvpt( cdfid, id_var(v), one, dims(v,:), data_r_4d, rcode)
              deallocate( data_r_4d )
           endif
        enddo
        rcode = nf_close( cdfid )
     end do
     deallocate( ens_mean )
     deallocate( ens_stdv_pert_prior )
     deallocate( ens_stdv_pert_poste )

end program gen_be_etkf
