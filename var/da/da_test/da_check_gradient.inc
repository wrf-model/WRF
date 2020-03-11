subroutine da_check_gradient(grid, config_flags, cv_size, xhat, cv, pdx, itertest, xbx, be, iv, y, re, j_cost)

   !-----------------------------------------------------------------------
   ! Purpose: Gradient test
   ! Adopted from grtest.f90 of GSI by Xin Zhang , September, 2011
   !-----------------------------------------------------------------------

   implicit none

   type(domain),               intent(inout) :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   real,    intent(in)    ::   pdx
   integer, intent(in   ) :: itertest
   integer, intent(in)           :: cv_size ! Size of cv array.
   real,    intent(inout)        :: xhat(1:cv_size)  ! control variable (local).
   real,    intent(inout)           :: cv(1:cv_size)    ! control variable (local).
   type (xbx_type),   intent(inout) :: xbx   ! Header & non-gridded vars.
   type (be_type),    intent(in) :: be    ! background error structure.
   type (iv_type),    intent(inout) :: iv    ! ob. increment vector.
   type (y_type),     intent(inout) :: y             ! y = h (xa)
   type (y_type), intent(inout)  :: re    ! residual (o-a) structure.
   type (j_type), intent(inout)  :: j_cost                 ! cost function

   real    :: xdir(1:cv_size) , grad(1:cv_size), yhat(1:cv_size)
   real    :: zfy,zf0,zdf0,za,zfa,zdfa
   real    :: zabuf(itertest),zfabuf(itertest),ztf2buf(itertest)
   real    :: ZT1,ZB,ZFB,ztco,ZTC1,ZT2,ZTC1A,ZTC2,ZTF2
   real    :: ZAL,ZFAL,ZBL,ZFBL,ZTF2L
   real    :: ZTC00,ZTC02,ZTC10,ZTC12
   real    :: ZERMIN,ZT1TST,ZREF
   integer                           :: jp_start, jp_end       ! Start/end indices of Jp.
   integer :: ibest,idig
   integer :: ii, jj

   call da_trace_entry("da_check_gradient")

   jp_start   = be % cv % size_jb + be % cv % size_je + 1
   jp_end     = be % cv % size_jb + be % cv % size_je + be % cv % size_jp

   call da_message((/' gradient test starting'/))

   if (pdx<=EPSILON(pdx)) then
      if (rootproc) write(6,*)'grtest, pdx=',pdx
      write(6,*)'grtest: pdx too small',pdx
      call da_trace_exit("da_check_gradient")
      return
   endif

   !----------------------------------------------------------------------------
   ! [1] Initial point
   !----------------------------------------------------------------------------

   call da_initialize_cv (cv_size, cv)
   call da_initialize_cv (cv_size, xhat)

   ! Initialize cv values with random data:
   call random_number(xhat(:))
   xhat(:) = xhat(:) - 0.5
   if (rootproc) write(6,*)'grtest: use random_number(xhat)'

   yhat=xhat

   call da_calculate_j(1, 1, cv_size, be%cv%size_jb, be%cv%size_je, be%cv%size_jp, &
                       be%cv%size_jl, be%cv%size_jt, xbx, be, iv, yhat, cv, re, y, j_cost, grid, config_flags)

   call da_calculate_gradj(1,1,cv_size,be%cv%size_jb,be%cv%size_je,be%cv%size_jp, &
                           be%cv%size_jl, be%cv%size_jt, xbx, be, iv, yhat+cv, y, grad, grid, config_flags, re)

   zfy = j_cost%total

   !----------------------------------------------------------------------------
   ! [1.1] Define perturbation direction ZH
   !----------------------------------------------------------------------------

   call da_message((/' The test direction is the opposite of the gradient '/))

   xdir  = -1.0  * grad
   !----------------------------------------------------------------------------
   ! [1.2] Set function f value and derivative at origin
   !----------------------------------------------------------------------------

   zf0=zfy
   zdf0=da_dot_cv(cv_size,grad,xdir,grid,be%cv_mz,be%ncv_mz,jp_start,jp_end)
   if (rootproc) write(6,*)'grtest: F(0)=',zf0,' DF(0)=',zdf0

   IF (ZDF0>0.0) write(6,*) 'GRTEST Warning, DF should be negative'
   IF (ABS(ZDF0) < SQRT(EPSILON(ZDF0))) THEN
      if (rootproc) write(6,*) 'GRTEST WARNING, DERIVATIVE IS TOO SMALL'
   ENDIF

   !----------------------------------------------------------------------------
   ! [2] Loop on test point
   !----------------------------------------------------------------------------

   ztf2buf(1)=0.0

   DO jj=1,itertest

      za=pdx*(10.0**(jj-1))

      if (rootproc) write(6,*)'grtest iter=',jj,' alpha=',za
   
      !----------------------------------------------------------------------------
      ! [2.1] Compute f and df at new point y=x+a.h
      !----------------------------------------------------------------------------

      do ii=1,cv_size
         yhat(ii) = xhat(ii) + za * xdir(ii)
      end do

      call da_calculate_j(1, 1, cv_size, be%cv%size_jb, be%cv%size_je, be%cv%size_jp, &
                          be%cv%size_jl, be%cv%size_jt, xbx, be, iv, yhat, cv, re, y, j_cost, grid, config_flags)

      call da_calculate_gradj(1,1,cv_size,be%cv%size_jb,be%cv%size_je,be%cv%size_jp, &
                              be%cv%size_jl, be%cv%size_jt, xbx, be, iv, yhat+cv, y, grad, grid, config_flags, re)

      zfy = j_cost%total

      zfa=zfy
      zdfa=da_dot_cv(cv_size,grad,xdir,grid,be%cv_mz,be%ncv_mz,jp_start,jp_end)

      if (rootproc) write(6,*)'grtest: alpha=',za,' F(a)=',zfa,' DF(a)=',zdfa
   
      zabuf(jj)=za
      zfabuf(jj)=zfa

      !----------------------------------------------------------------------------
      ! [2.2] Quantity TC0=f(a)/f(0)-1 
      !----------------------------------------------------------------------------

!         if f is continuous then TC0->1 at origin,
!         at least linearly with a.

      IF (ABS(zf0)<=TINY(zf0)) THEN
!           do not compute T1 in this unlikely case
         if (rootproc) write(6,*) 'grtest: Warning: zf0 is suspiciously small.'
         if (rootproc) write(6,*) 'grtest: F(a)-F(0)=',zfa-zf0
      ELSE
         ztco=zfa/zf0-1.0
         if (rootproc) write(6,*)'grtest: continuity TC0=',ztco
      ENDIF

      !----------------------------------------------------------------------------
      !                     f(a)-f(0)
      ! [2.3] Quantity T1=-----------
      !                      a.df(0)
      !----------------------------------------------------------------------------

!         if df is the gradient then T1->1 at origin,
!         linearly with a. T1 is undefined if df(0)=0.

      IF (ABS(za*zdf0)<=SQRT(TINY(zf0))) THEN
         if (rootproc) write(6,*)'grtest: Warning: could not compute ',&
          & 'gradient test T1, a.df(0)=',za*zdf0
      ELSE
         zt1=(zfa-zf0)/(za*zdf0)
         if (rootproc) write(6,*)'grtest: gradient T1=',zt1
      ENDIF

      !----------------------------------------------------------------------------
      ! [2.4] Quantity TC1=( f(a)-f(0)-a.df(0) )/a
      !----------------------------------------------------------------------------

!         if df is the gradient and df is continuous,
!         then TC1->0 linearly with a.
      ZTC1=(ZFA-ZF0-ZA*ZDF0)/ZA
      if (rootproc) write(6,*)'grtest: grad continuity TC1=',ZTC1

      !----------------------------------------------------------------------------
      ! [2.5] Quantity T2=( f(a)-f(0)-a.df(0) )*2/a**2
      !----------------------------------------------------------------------------

!         if d2f exists then T2 -> d2f(0) linearly with a.
      ZT2=(ZFA-ZF0-ZA*ZDF0)*2.0/(ZA**2)
      if (rootproc) write(6,*)'grtest: second derivative T2=',ZT2

      !----------------------------------------------------------------------------
      ! [2.6] Quantity TC1A=df(a)-df(0)
      !----------------------------------------------------------------------------

!         if df is the gradient in a and df is continuous,
!         then TC1A->0 linearly with a.
      ZTC1A=ZDFA-ZDF0
      if (rootproc) write(6,*)'grtest: a-grad continuity TC1A=',ZTC1A

      !----------------------------------------------------------------------------
      ! [2.7] Quantity TC2=( 2(f(0)-f(a))+ a(df(0)+df(a))/a**2
      !----------------------------------------------------------------------------

!         if f is exactly quadratic, then TC2=0, always: numerically
!         it has to -> 0 when a is BIG. Otherwise TC2->0 linearly for
!         small a is trivially implied by TC1A and T2.
      ZTC2=(2.0*(ZF0-ZFA)+ZA*(ZDF0+ZDFA))/(ZA**2)
      if (rootproc) write(6,*)'grtest: quadraticity TC2=',ZTC2

      !----------------------------------------------------------------------------
      !                     2   f(0)-f(b)   f(a)-f(b)
      ! [2.8] Quantity TF2=---( --------- + --------- )
      !                     a       b          a-b
      !----------------------------------------------------------------------------

!         if 0, a and b are distinct and f is quadratic then
!         TF2=d2f, always. The estimate is most stable when a,b are big.
!         This test works only after two loops, but it is immune against
!         gradient bugs. 

      IF (jj>=2) THEN
         ZB =ZABUF (jj-1)
         ZFB=ZFABUF(jj-1)
         ZTF2=2.0/ZA*((ZF0-ZFB)/ZB+(ZFA-ZFB)/(ZA-ZB))
         if (rootproc) write(6,*)'grtest: convexity ZTF2=',ZTF2
         ztf2buf(jj)=ztf2
      ENDIF

! End loop
   ENDDO

   !----------------------------------------------------------------------------
   ! [3] Comment on the results
   !----------------------------------------------------------------------------

!       TC0(0)/TC0(2)<.011 -> df looks continuous
!       item with (T1<1 and 1-T1 is min) = best grad test item
!       reldif(TF2(last),TF2(last-1)) = precision on quadraticity
   
   !----------------------------------------------------------------------------
   !       3.1 Fundamental checks
   !----------------------------------------------------------------------------

   if (rootproc) then
      write(6,*) 'GRTEST: TENTATIVE CONCLUSIONS :'

      ZTC00=ABS(zfabuf(1)-zf0)
      ZTC02=ABS(zfabuf(3)-zf0)
      IF( ZTC00/zabuf(1)  <=  1.5*(ZTC02/zabuf(3)) )THEN
         write(6,*) 'GRTEST: function f looks continous.'
      ELSE
         write(6,*) 'GRTEST: WARNING f does not look continuous',&
          & ' (perhaps truncation problem)'
      ENDIF
   
   !----------------------------------------------------------------------------
   !       3.2 Gradient quality
   !----------------------------------------------------------------------------

      IF (ABS(zdf0)<=SQRT(TINY(zf0))) THEN
         write(6,*) 'GRTEST: The gradient is 0, which is unusual !'
         ZTC10=ABS(zfabuf(1)-zf0)
         ZTC12=ABS(zfabuf(3)-zf0)
         IF( ZTC10/zabuf(1)**2  <=  1.1*ZTC12/zabuf(3)**2)THEN
            write(6,*)'GRTEST: The gradient looks good anyway.'
         ENDIF
      ELSE
!        Find best gradient test index
         ZERMIN=HUGE(0.0)
         ibest=-1
         DO jj=1,itertest
            ZT1TST=(zfabuf(jj)-zf0)/(zabuf(jj)*zdf0)
            ZT1TST=ABS(ZT1TST-1.0)
            IF (ZT1TST<ZERMIN) THEN
               ibest=jj
               ZERMIN=ZT1TST
            ENDIF
         ENDDO
         IF(ibest == -1)THEN
            write(6,*)'GRTEST: gradient test problem : bad ',&
             & 'gradient, non-convex cost, or truncation errors ?'
         ELSE
            idig=INT(-LOG(ZERMIN+TINY(ZERMIN))/LOG(10.0))
            write(6,*)'GRTEST: the best gradient test found has ',&
             & idig,' satisfactory digits.'
            IF(idig <= 1)THEN
               write(6,*)'GRTEST: SAYS: THE GRADIENT IS VERY BAD.'
            ELSEIF(idig <= 3)THEN
               write(6,*)'GRTEST: SAYS: THE GRADIENT IS SUSPICIOUS.'
            ELSEIF(idig <= 5)THEN
               write(6,*)'GRTEST: SAYS: THE GRADIENT IS ACCEPTABLE.'
            ELSE
               write(6,*)'GRTEST: SAYS: THE GRADIENT IS EXCELLENT.'
            ENDIF

            IF (ibest<=itertest-2) THEN
               ZTC10=ABS(zfabuf(ibest         )-zf0-zabuf(ibest         )*zdf0)/zabuf(ibest         )
               ZTC12=ABS(zfabuf(ibest+2)-zf0-zabuf(ibest+2)*zdf0)/zabuf(ibest+2)
               IF(ZTC10/zabuf(ibest) <=  1.1*ZTC12/zabuf(ibest+2) )THEN
                  write(6,*)'GRTEST: Gradient convergence looks good.'
               ELSE
                  write(6,*)'GRTEST: Gradient convergence is suspicious.'
               ENDIF
            ELSE
               write(6,*)'GRTEST: could not check grad convergence.'
            ENDIF
         ENDIF
      ENDIF

   !----------------------------------------------------------------------------
   !         3.3 Quadraticity
   !      finite difference quadraticity test (gradient-free)
   !----------------------------------------------------------------------------

      ZTF2=ztf2buf(itertest)
      ZTF2L=ztf2buf(itertest-1)
      write(6,*) 'GRTEST: finite diff. d2f estimate no1:',ZTF2
      write(6,*) 'GRTEST: finite diff. d2f estimate no2:',ZTF2L
      ZREF=(ABS(ZTF2L)+ABS(ZTF2))/2.0
      IF (ZREF<=TINY(ZREF)) THEN
         write(6,*) 'GRTEST: they are too small to decide whether ',&
          & 'they agree or not.'
      ELSE
         idig=INT(-LOG(ABS(ZTF2L-ZTF2)/ZREF+TINY(ZTF2))/LOG(10.0))
         write(6,*) 'GRTEST: the fin.dif. estimates of d2f ',&
          & 'have ',idig,' satisfactory digits.'
         IF(idig <= 1)THEN
            write(6,*) 'GRTEST: THE FD-QUADRATICITY IS BAD.'
         ELSEIF(idig <= 3)THEN
            write(6,*) 'GRTEST:: THE FD-QUADRATICITY IS SUSPICIOUS.'
         ELSEIF(idig <= 5)THEN
            write(6,*) 'GRTEST: THE FD-QUADRATICITY IS ACCEPTABLE.'
         ELSE
            write(6,*) 'GRTEST: THE FD-QUADRATICITY IS EXCELLENT.'
         ENDIF
      ENDIF

      write(6,*) 'grtest: Goodbye.'
   endif

   call da_trace_exit("da_check_gradient")

end subroutine da_check_gradient


