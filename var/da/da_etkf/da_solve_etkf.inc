subroutine da_solve_etkf( ndim,nanals,nobs,ens,ens_ob,oberrvar,obs,nout,&
   naccumt1,naccumt2,nstartaccum1,nstartaccum2,tainflatinput,rhoinput)

   !-----------------------------------------------------------------------
   ! Purpose: ETKF perturbation updates 
   ! Xuguang Wang, Jan. 2006
   ! Dale Barker. January 2007. Implicit none, Convert to f90, and enclose within gen_be_etkf.f90 driver within WRF.
   ! Dale Barker. Also modify inflation factor method (nstartaccum1,nstartaccum2 redundant).
   !
   ! references:
   ! Bishop et al 2001 MWR, 
   ! Wang and Bishop 2003 MWR, 
   ! Wang et al. 2004, MWR
   ! Wang et al. 2006, MWR
   !
   !1) nanals, ensemble size
   !2) ndim, dimension of the perturbation vector that needs to be updated
   !3) nobs, number of observations assmiilated
   !4) ens, array of perturbations before (Xf') and after (Xa') ETKF update
   !5) ens_ob, array of HXf
   !6) oberrvar, observation error variance, listed in the same sequence as HXf
   !7) obs, observations assmiilated
   !8) naccumt1, number of previous cycles immediately before the current cycle, which is needed for calculating adaptive inflation factor. naccumt1 < 0 for pre-specified inflation
   !9) naccumt2, number of previous cycles immediately before the current cycle, which is needed for calculating the rho factor in the latest version of ETKF. naccumt2 < 0 for using the older version of the ETKF. naccumt2 = 0 for using pre-specified rho factor
   !10) nstartaccum1, the cycle from which the accumulation of previous naccumt1 cycles in 8) starts
   !11) nstartaccum2, the cycle from which the accumulation of previous naccumt2 in 9) starts
   !12) tainflatinput, pre-specified inflation, if not using adaptive inflation
   !13) rhoinput, pre-specified rho factor, if not using adaptively determined rho factor
   !14) nout, record number for output squared innovations and the forecast error variance 
   !-----------------------------------------------------------------------projected onto ensemble subspace, which is related to 8) and 9) 

   implicit none

   integer, intent(in) :: nanals,ndim,nobs
   real, intent(inout), dimension(ndim,nanals) :: ens
   real, intent(inout), dimension(nobs,nanals) :: ens_ob
   real, intent(in), dimension(nobs) :: oberrvar
   real, intent(in), dimension(nobs) :: obs
   real, intent(in) :: tainflatinput,rhoinput
   integer, intent(in) :: nout,naccumt1,naccumt2,nstartaccum1,nstartaccum2

   integer                :: n                          ! Loop counters.
   integer                :: nmin                       ! Minimum nout value to use.
   real                   :: nanals_inv                 ! 1 / nanals.
   real                   :: ainflat_mean               ! Rolling mean inflation factor.

 real, dimension(nobs) :: ensmean_ob
 real, dimension(nobs) :: obsinc
 integer :: ij, nanal, i, j               ! Loop counters.
 integer :: info, lwork
 real*8, allocatable, dimension(:) :: work
 real*8, dimension(1) :: workt
 real*8, dimension(nanals) :: eignv1
 real, dimension(nanals) :: eignv
 real*8, dimension(nanals, nanals) :: hzthz
 real, dimension(nanals, nanals) :: C, TR
 real, dimension(nanals-1, nanals) :: CT
 real, dimension(nanals, nanals-1) :: T
 ! real, dimension(nanals, nanals-1) :: cgamma
 ! real, dimension(nobs,nanals-1) :: E
 real, dimension(nobs,nanals) :: enspert_ob_tmp
! file for output eigen values filename_eignv
! file for output factors filename_factor
 character (len=150) :: filename_factor
 ! character (len=150) :: filename_eignv
! file for squared innovation vectors
! file for squared projection of fcst err variance onto ensemble subspace
 ! character (len=150) :: filename_inno2
 ! character (len=150) :: filename_proj2
 ! real, dimension(nanals-1) :: proj
 real :: tracehpfht
 ! real :: ta, tb
 ! real :: sum_eignv
 ! real :: aftersum_eignv
 real :: ainflat
 ! real :: tainflat, rho
 ! real :: proj2, proj2sum
 ! real :: proj2sum1
 ! real :: squareinnosum, squareinnosum1
 real :: squareinno

   if (trace_use) call da_trace_entry("da_solve_etkf")

   filename_factor = "inflation_factor.dat"
   open(109,form="unformatted",file=filename_factor,access="direct",recl=8)
 
!------------------------------------------------------------------------------
!  [1] Compute mean(H(xf)) and H(xf)'.
!------------------------------------------------------------------------------

   nanals_inv = 1.0 / real(nanals)
   do ij = 1, nobs
      ensmean_ob(ij) = sum(ens_ob(ij,:)) * nanals_inv
   end do

   do nanal = 1, nanals
      ens_ob(:,nanal) = ens_ob(:,nanal) - ensmean_ob(:)
   end do

!------------------------------------------------------------------------------
!  [2] Calculate HZTHZ in Bishop et al. 2001
!------------------------------------------------------------------------------

   do i = 1, nobs
      enspert_ob_tmp(i,:) = ens_ob(i,:)/sqrt(oberrvar(i))
   end do

   call da_innerprod(enspert_ob_tmp,hzthz,nobs,nanals) 
   hzthz = hzthz/float(nanals-1)

!------------------------------------------------------------------------------
!  [3] Calculate C and Gamma in Bishop et al. 2001
!------------------------------------------------------------------------------
!  in outputs, hzthz contains C, eignv contains gamma  
   call dsyev('V', 'L', nanals , hzthz, nanals, eignv1, workt, -1, info)
   lwork = int(workt(1))
   allocate (work(lwork))
   call dsyev('V', 'L', nanals , hzthz, nanals, eignv1, work, lwork, info)
   deallocate(work)

!  note eignv1 output from SSYEV is in ascending order !!!
!  re-order both eigenvalues and eigenvectors first

   do i = 1, nanals
      eignv(i) = eignv1(nanals-i+1)
   end do

!  note eigenvectors output from SSYEV are stored in columns !!!
   do i = 1, nanals
      C(:,i) = hzthz(:,nanals-i+1)
   end do

!------------------------------------------------------------------------------
!  [4] Calculate inflation factor:
!------------------------------------------------------------------------------

   if ( naccumt1 > 0 ) then

      tracehpfht = 0.0
      do i = 1, nanals-1
         tracehpfht = tracehpfht + eignv(i)
      end do

      obsinc(:) = ensmean_ob(:) - obs(:)
      squareinno = sum( obsinc(:) * obsinc(:) / oberrvar(:) )
      ainflat = ( squareinno - real(nobs) ) / tracehpfht
      write(109,rec=nout) ainflat

!     Calculate running mean inflation factor:
      nmin = max( 1, nout - naccumt1 + 1 )
      ainflat_mean = 0.0
      do n = nmin, nout
         read(109,rec=n) ainflat
         ainflat_mean = ainflat_mean + ainflat
      end do
      ainflat_mean = ainflat_mean / real( nout - nmin + 1 )
      write (unit=stdout,fmt='(/a,f15.5)')  " Current Inflation factor = ", ainflat
      write (unit=stdout,fmt='(a,f15.5)')  " Rolling mean inflation factor = ", ainflat_mean
    else
!      This is for pre-specified inflation factor
       ainflat = tainflatinput
    end if

!------------------------------------------------------------------------------
!  [5] Calculate the grand transformation matrix:
!------------------------------------------------------------------------------

   do i = 1, nanals
      do j = 1, nanals-1
         T(i,j) = C(i,j)*sqrt(1.0/(eignv(j)+1.0)) 
      end do
   end do
 
   do i = 1, nanals-1
      do j = 1, nanals 
         CT(i,j) = C(j,i) 
      end do
   end do

   call da_matmulti(T,CT,TR,nanals,nanals,nanals-1) 

!  Apply inflation:
   tr = sqrt(ainflat_mean) * tr
  
!------------------------------------------------------------------------------
!  [4] Calculate the rescaled ETKF perturbations
!------------------------------------------------------------------------------

   call da_matmultiover(ens, tr, ndim, nanals)

   if (trace_use) call da_trace_exit("da_solve_etkf")

end subroutine da_solve_etkf

