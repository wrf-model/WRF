subroutine da_etkf(ndim,nanals,nobs,ens,ens_ob,oberrvar,obs,nout,naccumt1, &
   naccumt2,nstartaccum1,nstartaccum2,tainflatinput,rhoinput)

   !-----------------------------------------------------------------------
   ! Purpose: subroutine for ETKF perturbation updates 
   ! Xuguang Wang, Jan. 2006
   !
   ! references:
   ! Bishop et al. 2001 MWR
   ! Wang and Bishop 2003 JAS 
   ! Wang et al. 2004 MWR
   ! Wang et al. 2005 MWR
   ! 1) nanals, ensemble size
   ! 2) ndim, dimension of the perturbation vector that needs to be updated
   ! 3) nobs, number of observations assmiilated
   ! 4) ens, array of perturbations before (Xf') and after (Xa') ETKF update
   ! 5) ens_ob, array of HXf
   ! 6) oberrvar, observation error variance, listed in the same sequence as HXf
   ! 7) obs, observations assmiilated
   ! 8) naccumt1, number of previous cycles immediately before the current cycle,
   ! which is needed for calculating adaptive inflation factor. naccumt1 < 0 for
   ! pre-specified inflation
   ! 9) naccumt2, number of previous cycles immediately before the current cycle,
   ! which is needed for calculating the rho factor in the latest version of 
   ! ETKF. naccumt2 < 0 for using the older version of the ETKF. naccumt2 = 0 
   ! for using pre-specified rho factor
   ! 10) nstartaccum1, the cycle from which the accumulation of previous 
   ! naccumt1 cycles in 8) starts
   ! 11) nstartaccum2, the cycle from which the accumulation of previous naccumt2
   ! in 9) starts
   ! 12) tainflatinput, pre-specified inflation, if not using adaptive inflation
   ! 13) rhoinput, pre-specified rho factor, if not using adaptively determined 
   ! rho factor
   ! 14) nout, record number for output squared innovations and the forecast 
   ! error variance projected onto ensemble subspace, which is related to 8) and 9) 
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in) :: nanals,ndim,nobs
   real, intent(inout), dimension(ndim,nanals) :: ens
   real, intent(inout), dimension(nobs,nanals) :: ens_ob
   real, intent(in), dimension(nobs) :: oberrvar
   real, intent(in), dimension(nobs) :: obs
   real, intent(in) :: tainflatinput,rhoinput
   integer, intent(in) :: nout,naccumt1,naccumt2,nstartaccum1,nstartaccum2

   real, dimension(ndim) :: ensmean
   real, dimension(nobs) :: ensmean_ob
   real, dimension(nobs) :: obsinc
   integer :: ij, nanal, i, j, k, nt
   integer :: info, lwork
   real, allocatable, dimension(:) :: work
   real, dimension(1) :: workt
   real, dimension(nanals) :: eignv, eignv1
   real, dimension(nanals, nanals) :: hzthz, C, TR
   real, dimension(nanals-1, nanals) :: CT
   real, dimension(nanals, nanals-1) :: cgamma, T
   real, dimension(nobs,nanals-1) :: E
   real, dimension(nobs,nanals) :: enspert_ob_tmp
   ! file for output eigen values filename_eignv
   ! file for output factors filename_factor
   character (len=150) :: filename_factor,filename_eignv
   ! file for squared innovation vectors
   ! file for squared projection of fcst err variance onto ensemble subspace
   character (len=150) :: filename_inno2,filename_proj2
   real, dimension(nanals-1) :: proj
   real :: ainflat, tainflat, rho
   real :: proj2, proj2sum, proj2sum1
   real :: squareinnosum, squareinno, squareinnosum1
   real :: ta, tb, tracehpfht, aftersum_eignv, sum_eignv
   filename_factor = "factor.dat"
   filename_eignv = "eignv.dat"
   filename_inno2 = "inno2.dat"
   filename_proj2 = "proj2.dat"

   ! open file for output factors ainflat, tainflat, rho 
   open(109,form="unformatted",file=filename_factor,access="direct",recl=4)

   ! open file for output eigenvalues  
   open(119,form="unformatted",file=filename_eignv,access="direct",recl=4*nanals)

   ! open file for    
   open(99,form="unformatted",file=filename_inno2,access="direct",recl=4)

   ! open file for    
   open(89,form="unformatted",file=filename_proj2,access="direct",recl=4)

   call cpu_time(ta)

   ! compute ensemble mean.
   do ij=1,ndim
      ensmean(ij) = sum(ens(ij,:))/float(nanals)
   end do

   do ij=1,nobs
      ensmean_ob(ij) = sum(ens_ob(ij,:))/float(nanals)
   end do

   ! remove ens mean to get ensemble perturbations

   do nanal=1,nanals
      ens(:,nanal) = ens(:,nanal)-ensmean(:)
      ens_ob(:,nanal) = ens_ob(:,nanal)-ensmean_ob(:)
   end do

   ! calculate HZTHZ  Wang and Bishop 2003
   do i = 1, nobs
      enspert_ob_tmp(i,:) = ens_ob(i,:)/sqrt(oberrvar(i))
   end do

   call innerprod(enspert_ob_tmp,hzthz,nobs,nanals) 
   hzthz = hzthz/float(nanals-1)

   ! calculate C and Gamma in  Wang and Bishop 2003
   ! in outputs, hzthz contains C, eignv contains gamma  
   call SSYEV('V', 'L', nanals , hzthz, nanals, eignv1, workt, -1, info)
   lwork = int(workt(1))
   allocate (work(lwork))
   call SSYEV('V', 'L', nanals , hzthz, nanals, eignv1, work, lwork, info)
   deallocate(work)

   ! note eignv1 output from SSYEV is in ascending order !!!
   ! re-order both eigenvalues and eigenvectors first

   do i = 1, nanals
      eignv(i) = eignv1(nanals-i+1)
   end do

   write(119,rec=nout) eignv 

   write (unit=stdout,fmt=*) "eignv=",eignv

   ! note eigenvectors output from SSYEV are stored in columns !!!
   do i = 1, nanals
      C(:,i) = hzthz(:,nanals-i+1)
   end do


   ! calculate the inflation factor used to inflate the ETKF initial perts 
   ! by ainflat = (obsinc*obsinc/oberrvar-nobs)/trace(pfht_ob/oberrvar), 
   ! tainflate = ainflate1*ainflat2*.... in Wang and Bishop 2003

   tracehpfht = 0.0
   do i = 1, nanals-1
      tracehpfht = tracehpfht + eignv(i)
   end do

   obsinc = ensmean_ob - obs

   squareinno = 0.0
   do i = 1, nobs
      squareinno = squareinno + obsinc(i)*obsinc(i)/oberrvar(i)   
   end do

   write(99,rec=nout) squareinno 

   write (unit=stdout,fmt=*) "squareinno=", squareinno
   write (unit=stdout,fmt=*) "nout=", nout
   write (unit=stdout,fmt=*) "nstartaccum1=", nstartaccum1
   write (unit=stdout,fmt=*) "tracehpfht=",tracehpfht
   write (unit=stdout,fmt=*) "nobs=",nobs

   if (naccumt1 .gt. 0) then
      squareinnosum = squareinno

      if (nout.ge.nstartaccum1) then
         write (unit=stdout,fmt=*) "OK"
         squareinnosum = 0.0
         do nt = nout-naccumt1+1, nout 
            read(99,rec=nt) squareinnosum1
            write (unit=stdout,fmt=*) "squareinnosum1=", squareinnosum1
            squareinnosum = squareinnosum + squareinnosum1/float(naccumt1)
         end do
      end if

      write (unit=stdout,fmt=*) "squareinnosum=",squareinnosum

      ainflat = (squareinnosum-float(nobs))/tracehpfht
      if (ainflat .le. 0) then
         ainflat = 1.0
      end if

      if (nout .le. nstartaccum1) then
         tainflat = ainflat
      end if

      if (nout .gt. nstartaccum1) then
         read(109, rec=(nout-2)*3+2) tainflat
         tainflat = tainflat*ainflat
      end if

      write(109, rec=(nout-1)*3+1) ainflat
      write(109, rec=(nout-1)*3+2) tainflat

      write (unit=stdout,fmt=*) "tainflat=", tainflat
      write (unit=stdout,fmt=*) "ainflat=", ainflat

   else
      ! this is for pre-specified inflation factor
      tainflat = tainflatinput
      write (unit=stdout,fmt=*) "tainflat=", tainflat
      write(109, rec=(nout-1)*3+2) tainflat
   end if


   ! calculate the rho factor following Wang et al 2005
   if (naccumt2 .gt. 0) then
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

      call matmulti(enspert_ob_tmp,cgamma,E,nobs,nanals-1,nanals) 

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

      proj2 = sum(proj*proj)

      write(89,rec=nout) proj2

      proj2sum = proj2

      if (nout .ge. nstartaccum2) then 
         proj2sum = 0.0
         do nt = nout-naccumt2+1,nout
            read(89,rec=nt) proj2sum1
            proj2sum = proj2sum + proj2sum1/float(naccumt2)
         end do
      end if

      squareinnosum = squareinno

     if (nout.ge.nstartaccum2) then
        write (unit=stdout,fmt=*) "OK"
        squareinnosum = 0.0
        do nt = nout-naccumt2+1, nout 
           read(99,rec=nt) squareinnosum1
           write (unit=stdout,fmt=*) "squareinnosum1=", squareinnosum1
           squareinnosum = squareinnosum + squareinnosum1/float(naccumt2)
        end do
     end if

     write (unit=stdout,fmt=*) "squareinnosum=",squareinnosum

     rho = (proj2sum-(float(nanals-1)))/(squareinnosum-float(nobs))
     if (rho .le. 0) then
        rho = 1.0
     end if

     write(109, rec=(nout-1)*3+3) rho

   else if (naccumt2 .lt. 0.0) then
      ! this is for the old formulation of ETKF in Wang and Bishop 2003
      rho = 1.0
   else
      ! this if for pre-specified rho factor
      rho = rhoinput
   end if

   write (unit=stdout,fmt=*) "rho=",rho
   write (unit=stdout,fmt=*) "proj2sum=",proj2sum

   ! calculate the grand transformation matrix following Wang et al 2005
   ! TR = C*(rho*gamma+I)^(-1/2)*C^T
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

   call matmulti(T,CT,TR,nanals,nanals,nanals-1) 
   ! apply ainflat
   TR = sqrt(tainflat)*TR  

   write (unit=stdout,fmt=*) "ens"
   write (unit=stdout,fmt=*) maxval(ens)
   write (unit=stdout,fmt=*) minval(ens)
   write (unit=stdout,fmt=*) "eigen/(eigen*rho+1)"
   write (unit=stdout,fmt=*) eignv/(eignv*rho+1.0)
   sum_eignv = 0.0
   aftersum_eignv = 0.0
   do i = 1, nanals
      sum_eignv = sum_eignv + eignv(i)
      aftersum_eignv = aftersum_eignv + eignv(i)/(eignv(i)*rho+1.0)
   end do
   write (unit=stdout,fmt=*) "sum_eignv=", sum_eignv
   write (unit=stdout,fmt=*) "sum_aftereignv=", aftersum_eignv


   write (unit=stdout,fmt=*) "avg_ens=",sum(ens)/float(ndim*nanals)
   write (unit=stdout,fmt=*) "std_ens=",sum(ens*ens)/float(ndim*nanals)

   ! calculate the ETKF initial perturbations
   call matmultiover(ens,TR,ndim,nanals)
   write (unit=stdout,fmt=*) "ens"
   write (unit=stdout,fmt=*) maxval(ens)
   write (unit=stdout,fmt=*) minval(ens)
   write (unit=stdout,fmt=*) "afteravg_ens=",sum(ens)/float(ndim*nanals)
   write (unit=stdout,fmt=*) "afterstd_ens=",sum(ens*ens)/float(ndim*nanals)

   ! calculate the ETKF initial perturbations

   call cpu_time(tb)
   write (unit=stdout,fmt=*) "total cpu time =", tb-ta
   write (unit=stdout,fmt=*) "total cpu time tb =", tb

end subroutine da_etkf


