subroutine da_solve_etkf( ndim,nanals,nobs,ens,ens_ob,oberrvar,obs,nout,&
   naccumt1,naccumt2,tainflatinput,rhoinput)

   !-----------------------------------------------------------------------
   ! Purpose: ETKF perturbation updates 
   ! Xuguang Wang, Jan. 2006
   ! Dale Barker. January 2007. Implicit none, Convert to f90, and enclose within gen_be_etkf.f90 driver within WRF.
   ! Dale Barker. Also modify inflation factor method (nstartaccum1,nstartaccum2 redundant).
   ! Luke Peffers. August 2010. Include Wang et al. (2007) adaptive "rho" factor
   ! Luke Peffers. Fix bug (ainflat changed to ainflat_mean) ... combine .dat files into single DA text file
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
   !8) naccumt1, number of previous cycles immediately before the current cycle for inflation factor averaging 
       ! for calculating adaptive inflation factor. naccumt1 < 0 for pre-specified inflation
       ! naccumt1 .gt. 0 for adaptive inflation factor
       ! naccumt1 .eq. 0 for pre-specified (tainflatinput) inflation factor
   !9) naccumt2, number of previous cycles immediately before the current cycle for innovation averaging
       ! which is needed for calculating the rho factor in the latest version of ETKF
       ! naccumt2 .gt. 0 for adaptive rho calculation
       ! naccumt2 .eq. 0 for using pre-specified (rhoinput) rho factor
   !10) tainflatinput, pre-specified inflation, if not using adaptive inflation
   !11) rhoinput, pre-specified rho factor, if not using adaptively determined rho factor
   !12) nout, record number for output squared innovations and the forecast error variance 
        !projected onto ensemble subspace, which is related to 8) and 9) 

   implicit none

   integer, intent(in) :: nanals,ndim,nobs
   real, intent(inout), dimension(ndim,nanals) :: ens
   real, intent(inout), dimension(nobs,nanals) :: ens_ob
   real, intent(in), dimension(nobs) :: oberrvar
   real, intent(in), dimension(nobs) :: obs
   real, intent(in) :: tainflatinput,rhoinput
   integer, intent(in) :: nout,naccumt1,naccumt2

   integer                :: n                          ! Loop counters.
   integer                :: nmin,nmin1,nmin2           ! Minimum nout value to use.
   real                   :: nanals_inv                 ! 1 / nanals.
   real                   :: ainflat_mean               ! Rolling mean inflation factor.

   real, dimension(nobs) :: ensmean_ob
   real, dimension(nobs) :: obsinc
   integer :: ij, nanal, i, j, k, nt              ! Loop counters.
   integer :: info, lwork, fileis
   real*8, allocatable, dimension(:) :: work
   real*8, dimension(1) :: workt
   real*8, dimension(nanals) :: eignv1
   real, dimension(nanals) :: eignv
   real*8, dimension(nanals, nanals) :: hzthz
   real, dimension(nanals, nanals) :: C, TR
   real, dimension(nanals-1, nanals) :: CT
   real, dimension(nanals, nanals-1) :: T
   real, dimension(nanals, nanals-1) :: cgamma
   real, dimension(nobs,nanals-1) :: E
   real, dimension(nobs,nanals) :: enspert_ob_tmp
   character (len=150) :: filename_etkf_data
   real, dimension(nanals-1) :: proj
   real :: tracehpfht
   real :: ainflat, rho
   real :: proj2, proj2sum
   real :: proj2sum1
   real :: squareinnosum, squareinnosum1
   real :: squareinno

   if (trace_use) call da_trace_entry("da_solve_etkf")

   filename_etkf_data = "etkf_data.txt"
   if ( naccumt1 .gt. 0 .or. naccumt2 .gt.0 ) then   
      if (nout .gt. 1) then
         open (unit=109,file=filename_etkf_data,form='formatted',status='old',access='direct',recl=20*3+1)
         end if
      if (nout .eq. 1) then
         write(unit=stdout,fmt=*)""
         write(unit=stdout,fmt=*)"This is cycle 1"
         write(unit=stdout,fmt=*)"A new etkf_data.txt file will be created"
         write(unit=stdout,fmt=*)""
         open (unit=109,file=filename_etkf_data,form='formatted',status='new',access='direct',recl=20*3+1)
      end if
   end if

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
      write (unit=stdout,fmt=*) "Eigenvalues",i,eignv(i)
   end do

!  note eigenvectors output from SSYEV are stored in columns !!!
   do i = 1, nanals
      C(:,i) = hzthz(:,nanals-i+1)
   end do
 
!------------------------------------------------------------------------------
!  [4] Calculate innovations, tracehpfht, and ainflat
!------------------------------------------------------------------------------
    if ( naccumt1 .gt. 0 .or. naccumt2 .gt.0 ) then
       tracehpfht = 0.0
      do i = 1, nanals-1
         tracehpfht = tracehpfht + eignv(i)
      end do

      obsinc(:) = ensmean_ob(:) - obs(:)
      squareinno = sum( obsinc(:) * obsinc(:) / oberrvar(:) )
      ainflat = ( squareinno - real(nobs) ) / tracehpfht

    end if

!------------------------------------------------------------------------------
!  [5] Calculate variance projection for rho
!------------------------------------------------------------------------------
   if (naccumt2 > 0) then
      ! calculate E, the ensemble subspace eign vectors in Wang and Bishop (2003)
      ! E=R(-1/2)HZCgamma^(-1/2)

      do i = 1, nanals 
         do j = 1, nanals-1
            cgamma(i,j) = C(i,j)*sqrt(1.0/eignv(j)) 
         end do
      end do

      ! R(-1/2)HZ
      do i = 1, nobs
         enspert_ob_tmp(i,:) = ens_ob(i,:)/sqrt(oberrvar(i))/sqrt(float(nanals-1))
      end do

      call da_matmulti(enspert_ob_tmp,cgamma,E,nobs,nanals-1,nanals) 

      ! project normalized (divided by oberrstdev) innovation vector onto E
      proj = 0.0
      do i = 1, nanals-1 
         do k = 1, nobs 
            proj(i) = proj(i) + obsinc(k)/sqrt(oberrvar(k))*E(k,i) 
         end do
      end do

      ! get rho = (sum(proj*proj)-dim. of proj))/(normalized innovation^2-nobs)  
      ! since nanals is relatively small, need follow wang et al (2005) for accumulation
      ! relative error = sqrt(2/(dim of proj)) = e.g., sqrt(2/207)=10%
      ! e.g., 50mem 2wks = sqrt(2/(14*49)) = 5% = e.g. 3wks sqrt(2/(21*49))=4%

      write(unit=stdout,fmt=*)"Relative error (% for 12-h cycle) =",sqrt(4/(float(nanals)*float(naccumt2)))
      proj2 = sum(proj*proj)
      proj2sum = proj2
    else
     proj2 = 0.0
    endif
     

!------------------------------------------------------------------------------
!  [6] Write ainflat, squareinno, and proj2 to file
!      Calculate rolling mean values 
!------------------------------------------------------------------------------
   if ( naccumt1 .gt. 0 .or. naccumt2 .gt.0 ) then 
      write(109,'(3f20.7,a)',rec=nout) ainflat, squareinno, proj2, char(10)
     
     nmin1 = max( 1, nout - naccumt1 + 1 )
     nmin2 = max( 1, nout - naccumt2 + 1 )
     nmin  = min( nmin1, nmin2)
     ainflat_mean = 0.0
     proj2sum = 0.0
     squareinnosum = 0.0
     do n = nmin, nout
        write(unit=stdout,fmt=*)"Reading from etkf_data.txt, rec = ",n
        read(109,'(3f20.7)',rec=n) ainflat, squareinnosum1, proj2sum1
        if (n .ge. ( nout - naccumt1 +1) ) then
           ainflat_mean = ainflat_mean + ainflat
        end if
        if (n .ge. ( nout - naccumt2 +1) ) then
           proj2sum = proj2sum + proj2sum1
           squareinnosum = squareinnosum + squareinnosum1
        end if 
    end do
   end if

!------------------------------------------------------------------------------
!  [7] If averaging, calculate rolling means: ainflat_mean and rho
!------------------------------------------------------------------------------

   if (naccumt1 .gt. 0 ) then
      ainflat_mean = ainflat_mean / real( nout - nmin1 + 1 )
      write (unit=stdout,fmt='(/a,f15.5)')  " Current inflation factor = ", ainflat
      write (unit=stdout,fmt='(a,f15.5)')   " Final inflation factor = ", ainflat_mean
      write (unit=stdout,fmt='(a,f15.5)')   " Trace hpfht = ", tracehpfht
   else
      ainflat_mean = tainflatinput
      write (unit=stdout,fmt='(/a,f15.5)')  " Inflation factor = tainflatinput = ", ainflat_mean
   end if
     
   if (naccumt2 .gt. 0 ) then
     proj2sum = proj2sum/float( nout - nmin2 + 1 )
     squareinnosum = squareinnosum/float( nout - nmin2 + 1 )
     rho = (proj2sum-(float(nanals-1)))/(squareinnosum-float(nobs))
     if (rho .le. 0) then
        write (unit=stdout,fmt='(/a,f15.5)') " Rho as calculated is < 1.0 ...Setting rho=1.0"
        rho = 1.0
     end if
   write (unit=stdout,fmt=*) "Rho = ",rho
   else
      ! this if for pre-specified rho factor
       rho = rhoinput
       write (unit=stdout,fmt=*) "Rho = rhoinput = ",rho
       write (unit=stdout,fmt=*)""
   end if

!------------------------------------------------------------------------------
!  [8] Calculate the grand transformation matrix:
!------------------------------------------------------------------------------
   do i = 1, nanals
      do j = 1, nanals-1
         T(i,j) = C(i,j)*sqrt(1.0/(rho*eignv(j)+1.0))
      end do
   end do
 
   do i = 1, nanals-1
      do j = 1, nanals 
         CT(i,j) = C(j,i) 
      end do
   end do

   call da_matmulti(T,CT,TR,nanals,nanals,nanals-1) 

!  Apply inflation:
   TR = sqrt(ainflat_mean) * TR
  
!------------------------------------------------------------------------------
!  [9] Calculate the rescaled ETKF perturbations
!------------------------------------------------------------------------------

   call da_matmultiover(ens, TR, ndim, nanals)

   if (trace_use) call da_trace_exit("da_solve_etkf")

end subroutine da_solve_etkf

