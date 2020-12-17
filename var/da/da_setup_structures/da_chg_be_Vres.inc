SUBROUTINE da_chg_be_Vres(kz, nk, eta_h, eta_be,&
                        reg_chi_be,reg_ps_be,reg_t_be, &
                        reg_chi   ,reg_ps   ,reg_t   , &
                        covm1_be, covm2_be, covm3_be, covm4_be, &
                        covm1   , covm2   , covm3   , covm4   , &
                        rfls1_be, rfls2_be, rfls3_be, rfls4_be, &
                        rfls1, rfls2, rfls3, rfls4)

!------------------------------------------------------------------------------
!  PURPOSE: Change vertical resolution of background stats for cv_options=3
!------------------------------------------------------------------------------
   integer, intent(in) :: kz, nk  ! model V-resolution and BE V-resolution
   real   , intent(in) :: eta_h(kz), eta_be(nk) ! Eta level definition
!
!  Regression coefficient's arrays:
   real, dimension(1:nk),      intent(in) :: reg_chi_be, reg_ps_be 
   real, dimension(1:nk,1:nk), intent(in) :: reg_t_be
   real, dimension(1:kz),      intent(out):: reg_chi   , reg_ps 
   real, dimension(1:kz,1:kz), intent(out):: reg_t

! Vertical Convarince matrix arrays:
   real, dimension(1:nk,1:nk), intent(in) :: covm1_be, covm2_be, &
                                             covm3_be, covm4_be
   real, dimension(1:kz,1:kz), intent(out):: covm1   , covm2   , &
                                             covm3   , covm4 

! Recursive filter scale-length arrays:
   real, dimension(nk),        intent(in) :: rfls1_be, rfls2_be, &
                                             rfls3_be, rfls4_be
   real, dimension(kz),        intent(out):: rfls1   , rfls2   , &
                                             rfls3   , rfls4  


   integer             :: i,j,k,m,l,l1,m1,n
   integer             :: lsig(kz)
   real                :: rsig(kz), rsigo(nk)
   real                :: coef1(kz),coef2(kz)
   logical             :: NO_INTERP
! ---------------------------------------------------------------------

   NO_INTERP = .FALSE.

! Check if the # of levels and the values of eta are same: 
!
   NO_INTERP = (kz == nk)
   if (NO_INTERP) then
      do k = 1, nk
        if (abs(eta_h(k)-eta_be(k)) > 1.0e-6) then
          NO_INTERP = .FALSE.
          exit
        endif
       enddo
   endif

   if(NO_INTERP )then
! Regression coefficients:
     reg_chi = reg_chi_be
     reg_ps  = reg_ps_be
     reg_t   = reg_t_be

! Vertical covarince matrix:
     covm1 = covm1_be
     covm2 = covm2_be
     covm3 = covm3_be
     covm4 = covm4_be

! Recursive filter scale-length:
     rfls1 = rfls1_be
     rfls2 = rfls2_be
     rfls3 = rfls3_be
     rfls4 = rfls4_be

     return
   endif

!   if (.not.NO_INTERP) then
!     write(6,'(/10X,a/2X,"Model eta levels:")') &
!             "Vertical resolution conversion needed for CV5 BE:::"
!     write(6,'(2X,I3,2X,f10.5)') ( k,eta_h(k),k=1,kz)
!   endif

! Convert Eta to log(eta):
    do k=1,kz
      rsig(k)=log(eta_h(k))
    enddo
    do k=1,nk
      rsigo(k)=log(eta_be(k))
    enddo

! Find the coef1 and coef2 for the vertical interpolation:
!  
  do k=1,kz

! Model levels below the lowest BE level:
  if(rsig(k).ge.rsigo(1))then
     m=1
     m1=2
     lsig(k)=1
     coef1(k)=1.
     coef2(k)=0.

! Model levels above the highest BE level:
  else if(rsig(k).lt.rsigo(nk))then
     m=nk-1
     m1=nk
     lsig(k)=nk-1
     coef1(k)=0.
     coef2(k)=1

! Model levels located within the BE levels:
  else
     do m=1,nk
       m1=m+1
       if((rsig(k).le.rsigo(m))   .and.  &
          (rsig(k).gt.rsigo(m1))     )then
         lsig(k)=m
        go to 2345
       end if
     end do

2345    continue
    coef1(k)=(rsigo(m1)-rsig(k))/(rsigo(m1)-rsigo(m))
    coef2(k)=1.-coef1(k)
     if(lsig(k)==nk)then
     lsig(k)=nk-1
     coef2(k)=1.
     coef1(k)=0.
     endif
 endif

   end do

  do k=1,kz
    m=lsig(k)
    m1=m+1
! Interpolation for Regression coefficients:
      reg_chi(k)=reg_chi_be(m)*coef1(k)+reg_chi_be(m1)*coef2(k)
      reg_ps(k) =reg_ps_be (m)*coef1(k)+reg_ps_be(m1) *coef2(k)

! Recursive filter scale-lengths:
      rfls1(k) =rfls1_be (m)*coef1(k)+rfls1_be(m1) *coef2(k)
      rfls2(k) =rfls2_be (m)*coef1(k)+rfls2_be(m1) *coef2(k)
      rfls3(k) =rfls3_be (m)*coef1(k)+rfls3_be(m1) *coef2(k)
      rfls4(k) =rfls4_be (m)*coef1(k)+rfls4_be(m1) *coef2(k)

    do j=1,kz
      l=lsig(j)
      l1=l+1
! Interpolation for Regression coefficients:
        reg_t(j,k)=(reg_t_be(l,m)*coef1(j)+reg_t_be(l1,m)*coef2(j))*coef1(k) &
                  +(reg_t_be(l,m1)*coef1(j)+reg_t_be(l1,m1)*coef2(j))*coef2(k)
! Vertical covariance matrix:
        covm1(j,k)=(covm1_be(l,m)*coef1(j)+covm1_be(l1,m)*coef2(j))*coef1(k) &
                  +(covm1_be(l,m1)*coef1(j)+covm1_be(l1,m1)*coef2(j))*coef2(k)
        covm2(j,k)=(covm2_be(l,m)*coef1(j)+covm2_be(l1,m)*coef2(j))*coef1(k) &
                  +(covm2_be(l,m1)*coef1(j)+covm2_be(l1,m1)*coef2(j))*coef2(k)
        covm3(j,k)=(covm3_be(l,m)*coef1(j)+covm3_be(l1,m)*coef2(j))*coef1(k) &
                  +(covm3_be(l,m1)*coef1(j)+covm3_be(l1,m1)*coef2(j))*coef2(k)
        covm4(j,k)=(covm4_be(l,m)*coef1(j)+covm4_be(l1,m)*coef2(j))*coef1(k) &
                  +(covm4_be(l,m1)*coef1(j)+covm4_be(l1,m1)*coef2(j))*coef2(k)
    enddo
  enddo

!--------------------------------------------------------------------
end subroutine da_chg_be_Vres
