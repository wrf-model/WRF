! FCOPY, ICOPY, etc.: C pre-processor macros for C->N and N->C
! horizontal interpolation.  These exist solely to reduce code
! complexity.

! Copy from C array to N array:
#define FCOPY(N,C,i,j,k) \
  N(k,i-nits+1)=W1(i,j)*C(II(i,j)  ,JJ(i,j)  ,k)  \
  +W2(i,j)*C(II(i,j)+1,JJ(i,j)  ,k)  \
  +W3(i,j)*C(II(i,j)+a,JJ(i,j)-1,k)  \
  +W4(i,j)*C(II(i,j)+a,JJ(i,j)+1,k)

! Copy from C array to (k,used) indexed N array:
#define uFCOPY(N,C,i,j,k) \
  N(k,used)=W1(i,j)*C(II(i,j)  ,JJ(i,j)  ,k)  \
  +W2(i,j)*C(II(i,j)+1,JJ(i,j)  ,k)  \
  +W3(i,j)*C(II(i,j)+a,JJ(i,j)-1,k)  \
  +W4(i,j)*C(II(i,j)+a,JJ(i,j)+1,k)

! ICOPY: Grab from C array, but do not assign:
#define ICOPY(C,i,j,k) \
   (W1(i,j)*C(II(i,j)  ,JJ(i,j)  ,k)  \
  + W2(i,j)*C(II(i,j)+1,JJ(i,j)  ,k)  \
  + W3(i,j)*C(II(i,j)+a,JJ(i,j)-1,k)  \
  + W4(i,j)*C(II(i,j)+a,JJ(i,j)+1,k))

! IKJCOPY: grab from IKJ C array, do not assign:
#define IKJCOPY(C,i,k,j) \
   (W1(i,j)*C(II(i,j)  ,k,JJ(i,j)  )  \
  + W2(i,j)*C(II(i,j)+1,k,JJ(i,j)  )  \
  + W3(i,j)*C(II(i,j)+a,k,JJ(i,j)-1)  \
  + W4(i,j)*C(II(i,j)+a,k,JJ(i,j)+1))

! ICOPY2D: grab from IJ C array, do not assign:
#define ICOPY2D(C,i,j) \
   (W1(i,j)*C(II(i,j)  ,JJ(i,j)  )  \
  + W2(i,j)*C(II(i,j)+1,JJ(i,j)  )  \
  + W3(i,j)*C(II(i,j)+a,JJ(i,j)-1)  \
  + W4(i,j)*C(II(i,j)+a,JJ(i,j)+1))

! Copying from N array to C array:
#define UPCOPY(C,N,i,j,k,ni,nj) \
  C(k,i-istart+1)=(N(ni,nj+2,k) \
 + N(ni-a  ,nj+1,k)+ N(ni+1-a,nj+1,k) \
 + N(ni-1,nj  ,k)+ N(ni,nj  ,k)  + N(ni+1,nj  ,k) \
 + N(ni-a  ,nj-1,k)+ N(ni+1-a,nj-1,k) \
 +  N(ni,nj-2,k) \
  ) / 9

! Average to C points from N points without assignment:
#define NGRAB(N,ni,nj,nk) \
(N(ni,nj+2,nk) \
 + N(ni-a  ,nj+1,nk)+ N(ni+1-a,nj+1,nk)\
 + N(ni-1,nj  ,nk)  + N(ni,nj  ,nk)  + N(ni+1,nj  ,nk)\
 + N(ni-a  ,nj-1,nk)+ N(ni+1-a,nj-1,nk)\
 +  N(ni,nj-2,nk) \
  ) / 9

! Average to C points from N points without assignment on an IKJ grid:
#define NGRABIKJ(N,ni,nk,nj) \
(N(ni,nk,nj+2) \
 + N(ni-a  ,nk,nj+1)+ N(ni+1-a,nk,nj+1)   \
 + N(ni-1,nk,nj  )  + N(ni,nk,nj  )  + N(ni+1,nk,nj  )\
 + N(ni-a  ,nk,nj-1)+ N(ni+1-a,nk,nj-1)\
 +  N(ni,nk,nj-2) \
  ) / 9

! Average to C points from N points without assignment, no vertical levels:
#define NGRAB2D(N,ni,nj) \
(N(ni,nj+2) \
 + N(ni-a  ,nj+1)+ N(ni+1-a,nj+1)\
 + N(ni-1,nj  )  + N(ni,nj  )  + N(ni+1,nj  )\
 + N(ni-a  ,nj-1)+ N(ni+1-a,nj-1)\
 +  N(ni,nj-2) \
  ) / 9

! Copying from N array to I array:
#define N2ICOPY(C,N,i,j,k,ni,nj) \
  C(i,j,k)=( N(ni,nj+2,k) \
 + N(ni-a  ,nj+1,k)+ N(ni+1-a,nj+1,k) \
 + N(ni-1,nj  ,k)+ N(ni,nj  ,k)  + N(ni+1,nj  ,k) \
 + N(ni-a  ,nj-1,k)+ N(ni+1-a,nj-1,k) \
 +  N(ni,nj-2,k) \
  ) / 9

! Maximum value from N array to C array:
#define N2I_SET_MAX(C,N,i,j,k,ni,nj) \
  C(i,j,k)=max(C(i,j,k),max(N(ni,nj+2,k),   \
  max(N(ni-1,nj  ,k),max(N(ni,nj  ,k),  max(N(ni+1,nj  ,k),\
  max(N(ni-a  ,nj-1,k),max(N(ni+1-a,nj-1,k),\
 N(ni,nj-2,k) )))))))

! Maximum value from N array to C array:
#define N2I_SET_MAX_IJ(C,N,i,j,ni,nj) \
  C(i,j)=max(C(i,j), max(N(ni,nj+2),\
 max(N(ni-1,nj  ),max(N(ni,nj  ),  max(N(ni+1,nj  ),  \
   max(N(ni-a  ,nj-1),max(N(ni+1-a,nj-1), \
    N(ni,nj-2) )))))))
       

module module_interp_nmm
  use module_model_constants, only: g, R_D, p608
  implicit none

  private
  public :: find_kpres

  public :: nmm_interp_pd, nmm_keep_pd, nmm_method_linear

  public :: c2b_fulldom, c2n_fulldom, n2c_fulldom
  public :: n2c_max2d, n2c_max3d
  public :: c2b_fulldom_new, n2c_fulldom_new
  public :: c2b_mass, c2n_mass, n2c_mass
  public :: c2n_massikj, n2c_massikj
  public :: c2b_copy3d, c2n_copy3d, n2c_copy3d

  public :: c2b_copy2d, c2n_copy2d, n2c_copy2d, c2n_copy2d_nomask
  public :: c2b_near2d, c2n_near2d, n2c_near2d
  public :: c2b_inear2d, c2n_inear2d, n2c_inear2d

  public :: c2n_near3dikj, c2n_sst, c2n_near3d

! unimplemented:  public :: nmm_method_quadratic, nmm_method_spline_log, nmm_method_copy, nmm_method_spline

!  integer, parameter :: nmm_method_copy = 5
!  integer, parameter :: nmm_method_spline_log = 4
!  integer, parameter :: nmm_method_spline = 3
!  integer, parameter :: nmm_method_quadratic = 2

  integer, parameter :: nmm_method_linear = 1

  integer, parameter :: nmm_interp_pd = 1
  integer, parameter :: nmm_keep_pd = 0

  ! Constants from the original base_state_parent:
  REAL, PARAMETER :: LAPSR=6.5E-3, GI=1./G,D608=0.608
  REAL, PARAMETER :: COEF3=287.05*GI*LAPSR, COEF2=-1./COEF3
  REAL, PARAMETER :: TRG=2.0*R_D*GI,LAPSI=1.0/LAPSR
  REAL, PARAMETER :: P_REF=103000.

  integer, parameter :: EConst=0, ECopy=1, EExtrap=2 ! MUST match module_dm

contains
  ! ********************************************************************
  ! subs *_NEAR2D -- horizontally interpolate via nearest neighbor
  ! method, but do not vertically interpolate.  Only handles 2D H grid
  ! arrays.
  ! ********************************************************************

  subroutine c2b_near2d (inear,jnear,cfield,         &
       fbxs,fbxe,fbys,fbye,                  &
       cims, cime, cjms, cjme,   &
       nids, nide, njds, njde,   &
       nims, nime, njms, njme,   &
       nits, nite, njts, njte)
    implicit none
    integer, parameter :: bdyw = 1

    integer, intent(in):: &
         cims, cime, cjms, cjme,   &
         nids, nide, njds, njde,   &
         nims, nime, njms, njme,   &
         nits, nite, njts, njte

    integer, intent(in), dimension(nims:nime,njms:njme) :: inear,jnear
    real, intent(in) :: cfield(cims:cime,cjms:cjme)

    ! Output field boundary info:
    real,dimension(nims:nime,bdyw) :: fbys,fbye
    real,dimension(njms:njme,bdyw) :: fbxs,fbxe

    integer :: j,i,a,nx,nz,ck,k,j1,j2,add
    real :: weight

    nx=min(nide-1,nite)-max(nids,nits)+1

    j1=max(njts-1,njds)
    if(mod(j1,2)/=1)   j1=j1+1

    j2=min(njte+1,njde-1)
    if(mod(j2,2)/=1)   j2=j2-1

    if(nits==1) then
       i=1
       do j=j1,j2,2
          fbxs(j,1)=cfield(inear(i,j),jnear(i,j))
       enddo
    endif
    if(nite>=nide-1) then
       i=nide-1
       do j=j1,j2,2
          fbxe(j,1)=cfield(inear(i,j),jnear(i,j))
       enddo
    endif
    if(njts==1) then
       j=1
       do i=max(nits-1,nids),min(nite+1,nide-1)
          fbys(i,1)=cfield(inear(i,j),jnear(i,j))
       enddo
    endif
    if(njte>=njde-1) then
       j=njde-1
       do i=max(nits-1,nids),min(nite+1,nide-1)
          fbye(i,1)=cfield(inear(i,j),jnear(i,j))
       enddo
    endif
  end subroutine c2b_near2d

  subroutine n2c_near2d (&
       cfield,nfield,ipos,jpos,  &
       cids, cide, cjds, cjde,   &
       cims, cime, cjms, cjme,   &
       cits, cite, cjts, cjte,   &
       nids, nide, njds, njde,   &
       nims, nime, njms, njme,   &
       nits, nite, njts, njte)
    implicit none
    integer, intent(in) :: &
         cids, cide, cjds, cjde,   &
         cims, cime, cjms, cjme,   &
         cits, cite, cjts, cjte,   &
         nids, nide, njds, njde,   &
         nims, nime, njms, njme,   &
         nits, nite, njts, njte,   &
         ipos,jpos
    real, intent(out) :: cfield(cims:cime,cjms:cjme)
    real, intent(in) :: nfield(nims:nime,njms:njme)

    integer, parameter :: nri=3, nrj=3 ! parent:nest ratio, must be 3 in NMM
    integer :: nz,jstart,jend,istart,iend,ni,nj,a,i,j

    jstart=MAX(jpos+1,cjts)
    jend=MIN(jpos+(njde-njds)/nrj-1,cjte)

    bigj: do j=jstart,jend
       a=mod(j,2)
       istart=MAX(ipos+a,cits)
       iend=MIN(ipos+(nide-nids)/nri-1,cite)
       nj = (j-jpos)*nrj + 1

       iloop: do i=istart,iend
          ni = (i-ipos)*nri + 2 - a
          cfield(i,j)=nfield(ni,nj)
       enddo iloop
    enddo bigj
  end subroutine n2c_near2d

    subroutine c2n_near2d (inear,jnear,    &
       cfield,nfield,imask,     &
       cims, cime, cjms, cjme,   &
       nids, nide, njds, njde,   &
       nims, nime, njms, njme,   &
       nits, nite, njts, njte)
    implicit none

    integer, intent(in), dimension(nims:nime,njms:njme) :: inear,jnear

    integer, intent(in):: cims, cime, cjms, cjme,   &
         nids, nide, njds, njde,   &
         nims, nime, njms, njme,   &
         nits, nite, njts, njte
    integer, intent(in), dimension(nims:nime,njms:njme) :: imask
    real, intent(in) :: cfield(cims:cime,cjms:cjme)
    real, intent(out) :: nfield(nims:nime,njms:njme)

    integer :: j,i,a,nx

    nx=min(nide-1,nite)-max(nids,nits)+1

    bigj: do j=max(njds,njts),min(njde-1,njte)
       interploop: do i=max(nids,nits),min(nide-1,nite)
          if(imask(i,j)/=0) cycle interploop
          nfield(i,j)=cfield(inear(i,j),jnear(i,j))
       enddo interploop
    end do bigj
  end subroutine c2n_near2d

    subroutine c2n_near3d (inear,jnear,    &
       cfield,nfield,imask,     &
       cims, cime, cjms, cjme, ckms, ckme,   &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte)
    implicit none

    integer, intent(in), dimension(nims:nime,njms:njme) :: inear,jnear

    integer, intent(in):: cims, cime, cjms, cjme, ckms, ckme,   &
         nids, nide, njds, njde, nkds, nkde,   &
         nims, nime, njms, njme, nkms, nkme,   &
         nits, nite, njts, njte, nkts, nkte
    integer, intent(in), dimension(nims:nime,njms:njme) :: imask
    real, intent(in) :: cfield(cims:cime,cjms:cjme,ckms:ckme)
    real, intent(out) :: nfield(nims:nime,njms:njme,nkms:nkme)

    integer :: j,i,a,nx,k

    nx=min(nide-1,nite)-max(nids,nits)+1

    bigk: do k=nkds,nkde
       bigj: do j=max(njds,njts),min(njde-1,njte)
          interploop: do i=max(nids,nits),min(nide-1,nite)
             if(imask(i,j)/=0) cycle interploop
             nfield(i,j,k)=cfield(inear(i,j),jnear(i,j),k)
          enddo interploop
       end do bigj
    enddo bigk
  end subroutine c2n_near3d

    subroutine c2n_near3dikj (inear,jnear,    &
       cfield,nfield,imask,     &
       cims, cime, cjms, cjme, ckms, ckme,   &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte)
    implicit none

    integer, intent(in), dimension(nims:nime,njms:njme) :: inear,jnear

    integer, intent(in):: cims, cime, cjms, cjme, ckms, ckme,   &
         nids, nide, njds, njde, nkds, nkde,   &
         nims, nime, njms, njme, nkms, nkme,   &
         nits, nite, njts, njte, nkts, nkte
    integer, intent(in), dimension(nims:nime,njms:njme) :: imask
    real, intent(in) :: cfield(cims:cime,ckms:ckme,cjms:cjme)
    real, intent(out) :: nfield(nims:nime,nkms:nkme,njms:njme)

    integer :: j,i,a,nx,k

    nx=min(nide-1,nite)-max(nids,nits)+1

    bigj: do j=max(njds,njts),min(njde-1,njte)
       bigk: do k=nkds,nkde
          interploop: do i=max(nids,nits),min(nide-1,nite)
             if(imask(i,j)/=0) cycle interploop
             nfield(i,k,j)=cfield(inear(i,j),k,jnear(i,j))
          enddo interploop
       enddo bigk
    end do bigj
  end subroutine c2n_near3dikj

    subroutine c2n_sst (inear,jnear,    &
       cfield,nfield,     &
       cims, cime, cjms, cjme,   &
       nids, nide, njds, njde,   &
       nims, nime, njms, njme,   &
       nits, nite, njts, njte)
    implicit none

    integer, intent(in), dimension(nims:nime,njms:njme) :: inear,jnear

    integer, intent(in):: cims, cime, cjms, cjme,   &
         nids, nide, njds, njde,   &
         nims, nime, njms, njme,   &
         nits, nite, njts, njte
    real, intent(in) :: cfield(cims:cime,cjms:cjme)
    real, intent(out) :: nfield(nims:nime,njms:njme)

    integer :: j,i,a,nx

    nx=min(nide-1,nite)-max(nids,nits)+1

    bigj: do j=max(njds,njts),min(njde-1,njte)
       interploop: do i=max(nids,nits),min(nide-1,nite)
          nfield(i,j)=cfield(inear(i,j),jnear(i,j))
       enddo interploop
    end do bigj
  end subroutine c2n_sst

  ! ********************************************************************
  ! subs *_INEAR2D -- horizontally interpolate integers via nearest
  ! neighbor method, but do not vertically interpolate.  Only handles
  ! 2D H grid integer arrays.
  ! ********************************************************************

  subroutine c2b_inear2d (inear,jnear,cfield,         &
       fbxs,fbxe,fbys,fbye,                  &
       cims, cime, cjms, cjme,   &
       nids, nide, njds, njde,   &
       nims, nime, njms, njme,   &
       nits, nite, njts, njte)
    implicit none
    integer, parameter :: bdyw = 1

    integer, intent(in):: &
         cims, cime, cjms, cjme,   &
         nids, nide, njds, njde,   &
         nims, nime, njms, njme,   &
         nits, nite, njts, njte

    integer, intent(in), dimension(nims:nime,njms:njme) :: inear,jnear
    integer, intent(in) :: cfield(cims:cime,cjms:cjme)

    ! Output field boundary info:
    integer,dimension(nims:nime,bdyw) :: fbys,fbye
    integer,dimension(njms:njme,bdyw) :: fbxs,fbxe

    integer :: j,i,a,nx,nz,ck,k,j1,j2,add

    nx=min(nide-1,nite)-max(nids,nits)+1

    j1=max(njts-1,njds)
    if(mod(j1,2)/=1)   j1=j1+1

    j2=min(njte+1,njde-1)
    if(mod(j2,2)/=1)   j2=j2-1

    if(nits==1) then
       i=1
       do j=j1,j2,2
          fbxs(j,1)=cfield(inear(i,j),jnear(i,j))
       enddo
    endif
    if(nite>=nide-1) then
       i=nide-1
       do j=j1,j2,2
          fbxe(j,1)=cfield(inear(i,j),jnear(i,j))
       enddo
    endif
    if(njts==1) then
       j=1
       do i=max(nits-1,nids),min(nite+1,nide-1)
          fbys(i,1)=cfield(inear(i,j),jnear(i,j))
       enddo
    endif
    if(njte>=njde-1) then
       j=njde-1
       do i=max(nits-1,nids),min(nite+1,nide-1)
          fbye(i,1)=cfield(inear(i,j),jnear(i,j))
       enddo
    endif
  end subroutine c2b_inear2d

  subroutine n2c_inear2d (&
       cfield,nfield,ipos,jpos,  &
       cids, cide, cjds, cjde,   &
       cims, cime, cjms, cjme,   &
       cits, cite, cjts, cjte,   &
       nids, nide, njds, njde,   &
       nims, nime, njms, njme,   &
       nits, nite, njts, njte)
    implicit none
    integer, intent(in) :: &
         cids, cide, cjds, cjde,   &
         cims, cime, cjms, cjme,   &
         cits, cite, cjts, cjte,   &
         nids, nide, njds, njde,   &
         nims, nime, njms, njme,   &
         nits, nite, njts, njte,   &
         ipos,jpos
    integer, intent(out) :: cfield(cims:cime,cjms:cjme)
    integer, intent(in) :: nfield(nims:nime,njms:njme)

    integer, parameter :: nri=3, nrj=3 ! parent:nest ratio, must be 3 in NMM
    integer :: nz,jstart,jend,istart,iend,ni,nj,a,i,j

    jstart=MAX(jpos+1,cjts)
    jend=MIN(jpos+(njde-njds)/nrj-1,cjte)

    bigj: do j=jstart,jend
       a=mod(j,2)
       istart=MAX(ipos+a,cits)
       iend=MIN(ipos+(nide-nids)/nri-1,cite)
       nj = (j-jpos)*nrj + 1

       iloop: do i=istart,iend
          ni = (i-ipos)*nri + 2 - a
          cfield(i,j)=nfield(ni,nj)
       enddo iloop
    enddo bigj
  end subroutine n2c_inear2d

    subroutine c2n_inear2d (inear,jnear,    &
       cfield,nfield,imask,     &
       cims, cime, cjms, cjme,   &
       nids, nide, njds, njde,   &
       nims, nime, njms, njme,   &
       nits, nite, njts, njte)
    implicit none

    integer, intent(in), dimension(nims:nime,njms:njme) :: inear,jnear

    integer, intent(in):: cims, cime, cjms, cjme,   &
         nids, nide, njds, njde,   &
         nims, nime, njms, njme,   &
         nits, nite, njts, njte
    integer, intent(in), dimension(nims:nime,njms:njme) :: imask
    integer, intent(in) :: cfield(cims:cime,cjms:cjme)
    integer, intent(out) :: nfield(nims:nime,njms:njme)

    integer :: j,i,a,nx

    nx=min(nide-1,nite)-max(nids,nits)+1

    bigj: do j=max(njds,njts),min(njde-1,njte)
       interploop: do i=max(nids,nits),min(nide-1,nite)
          if(imask(i,j)/=0) cycle interploop
          nfield(i,j)=cfield(inear(i,j),jnear(i,j))
       enddo interploop
    end do bigj
  end subroutine c2n_inear2d

  ! ********************************************************************
  ! subs *_COPY2D -- horizontally interpolate but do not vertically
  ! interpolate.  Only handles 2D arrays (H or V grid).
  ! ********************************************************************

  subroutine c2b_copy2d (II,JJ,W1,W2,W3,W4,cfield,         &
       fbxs,fbxe,fbys,fbye,                  &
       cims, cime, cjms, cjme,   &
       nids, nide, njds, njde,   &
       nims, nime, njms, njme,   &
       nits, nite, njts, njte, hgrid)
    implicit none
    integer, parameter :: bdyw = 1

    integer, intent(in):: &
         cims, cime, cjms, cjme,   &
         nids, nide, njds, njde,   &
         nims, nime, njms, njme,   &
         nits, nite, njts, njte
    logical, intent(in) :: hgrid
    real, intent(in), dimension(nims:nime,njms:njme) :: &
         W1,W2,W3,W4
    integer, intent(in), dimension(nims:nime,njms:njme) :: II,JJ
    real, intent(in) :: cfield(cims:cime,cjms:cjme)

    ! Output field boundary info:
    real,dimension(nims:nime,bdyw) :: fbys,fbye
    real,dimension(njms:njme,bdyw) :: fbxs,fbxe

    integer :: j,i,a,nx,nz,ck,k,j1,j2,add
    real :: weight

    nx=min(nide-1,nite)-max(nids,nits)+1

    if(hgrid) then
       a=1
    else
       a=0
    endif

    j1=max(njts-1,njds)
    if(mod(j1,2)/=a)   j1=j1+1

    j2=min(njte+1,njde-1)
    if(mod(j2,2)/=a)   j2=j2-1

    if(nits==1) then
       i=1
       do j=j1,j2,2
          if(hgrid) then
             a=1-mod(JJ(i,j),2)
          else
             a=mod(JJ(i,j),2)
          endif
          fbxs(j,1)=ICOPY2D(cfield,i,j)
       enddo
    endif
    if(nite>=nide-1) then
       i=nide-1
       do j=j1,j2,2
          if(hgrid) then
             a=1-mod(JJ(i,j),2)
          else
             a=mod(JJ(i,j),2)
          endif
          fbxe(j,1)=ICOPY2D(cfield,i,j)
       enddo
    endif
    if(njts==1) then
       j=1
       do i=max(nits-1,nids),min(nite+1,nide-1)
          if(hgrid) then
             a=1-mod(JJ(i,j),2)
          else
             a=mod(JJ(i,j),2)
          endif
          fbys(i,1)=ICOPY2D(cfield,i,j)
       enddo
    endif
    if(njte>=njde-1) then
       j=njde-1
       do i=max(nits-1,nids),min(nite+1,nide-1)
          if(hgrid) then
             a=1-mod(JJ(i,j),2)
          else
             a=mod(JJ(i,j),2)
          endif
          fbye(i,1)=ICOPY2D(cfield,i,j)
       enddo
    endif
  end subroutine c2b_copy2d
  subroutine n2c_copy2d (&
       cfield,nfield,ipos,jpos,  &
       cids, cide, cjds, cjde,   &
       cims, cime, cjms, cjme,   &
       cits, cite, cjts, cjte,   &
       nids, nide, njds, njde,   &
       nims, nime, njms, njme,   &
       nits, nite, njts, njte, &
       hgrid)
    implicit none
    integer, intent(in) :: &
         cids, cide, cjds, cjde,   &
         cims, cime, cjms, cjme,   &
         cits, cite, cjts, cjte,   &
         nids, nide, njds, njde,   &
         nims, nime, njms, njme,   &
         nits, nite, njts, njte,   &
         ipos,jpos
    real, intent(out) :: cfield(cims:cime,cjms:cjme)
    real, intent(in) :: nfield(nims:nime,njms:njme)
    logical, intent(in) :: hgrid

    integer, parameter :: nri=3, nrj=3 ! parent:nest ratio, must be 3 in NMM
    integer :: nz,jstart,jend,istart,iend,ni,nj,a,i,j

    jstart=MAX(jpos+1,cjts)
    jend=MIN(jpos+(njde-njds)/nrj-1,cjte)

    bigj: do j=jstart,jend
          a=mod(j,2)
          if(.not.hgrid) then
             a=1-a
          endif
       istart=MAX(ipos+a,cits)
       iend=MIN(ipos+(nide-nids)/nri-1,cite)
       nj = (j-jpos)*nrj + 1

       iloop: do i=istart,iend
          ni = (i-ipos)*nri + 2 - a
          cfield(i,j)=NGRAB2D(nfield,ni,nj)
       enddo iloop
    enddo bigj 
 end subroutine n2c_copy2d

  subroutine n2c_max2d (&
       cfield,nfield,ipos,jpos,  &
       cids, cide, cjds, cjde,   &
       cims, cime, cjms, cjme,   &
       cits, cite, cjts, cjte,   &
       nids, nide, njds, njde,   &
       nims, nime, njms, njme,   &
       nits, nite, njts, njte, &
       hgrid)
    implicit none
    integer, intent(in) :: &
         cids, cide, cjds, cjde,   &
         cims, cime, cjms, cjme,   &
         cits, cite, cjts, cjte,   &
         nids, nide, njds, njde,   &
         nims, nime, njms, njme,   &
         nits, nite, njts, njte,   &
         ipos,jpos
    real, intent(out) :: cfield(cims:cime,cjms:cjme)
    real, intent(in) :: nfield(nims:nime,njms:njme)
    logical, intent(in) :: hgrid

    integer, parameter :: nri=3, nrj=3 ! parent:nest ratio, must be 3 in NMM
    integer :: nz,jstart,jend,istart,iend,ni,nj,a,i,j

    jstart=MAX(jpos+1,cjts)
    jend=MIN(jpos+(njde-njds)/nrj-1,cjte)

    bigj: do j=jstart,jend
          a=mod(j,2)
          if(.not.hgrid) then
             a=1-a
          endif
       istart=MAX(ipos+a,cits)
       iend=MIN(ipos+(nide-nids)/nri-1,cite)
       nj = (j-jpos)*nrj + 1

       iloop: do i=istart,iend
          ni = (i-ipos)*nri + 2 - a
          N2I_SET_MAX_IJ(cfield,nfield,i,j,ni,nj)
       enddo iloop
    enddo bigj 
  end subroutine n2c_max2d

    subroutine c2n_copy2d (II,JJ,W1,W2,W3,W4,    &
       cfield,nfield,imask,     &
       cims, cime, cjms, cjme,   &
       nids, nide, njds, njde,   &
       nims, nime, njms, njme,   &
       nits, nite, njts, njte, hgrid)
    implicit none
    logical, intent(in) :: hgrid
    real, intent(in), dimension(nims:nime,njms:njme) :: &
         W1,W2,W3,W4
    integer, intent(in), dimension(nims:nime,njms:njme) :: II,JJ

    integer, intent(in):: cims, cime, cjms, cjme,   &
         nids, nide, njds, njde,   &
         nims, nime, njms, njme,   &
         nits, nite, njts, njte
    integer, intent(in), dimension(nims:nime,njms:njme) :: imask
    real, intent(in) :: cfield(cims:cime,cjms:cjme)
    real, intent(out) :: nfield(nims:nime,njms:njme)

    integer :: j,i,a,nx

    nx=min(nide-1,nite)-max(nids,nits)+1

    bigj: do j=max(njds,njts),min(njde-1,njte)
       interploop: do i=max(nids,nits),min(nide-1,nite)
          if(imask(i,j)/=0) cycle interploop
          if(hgrid) then
             a=1-mod(JJ(i,j),2)
          else
             a=mod(JJ(i,j),2)
          endif
          nfield(i,j)=ICOPY2D(cfield,i,j)
       enddo interploop
    end do bigj
  end subroutine c2n_copy2d

    subroutine c2n_copy2d_nomask (II,JJ,W1,W2,W3,W4,    &
       cfield,nfield,     &
       cims, cime, cjms, cjme,   &
       nids, nide, njds, njde,   &
       nims, nime, njms, njme,   &
       nits, nite, njts, njte, hgrid)
    implicit none
    logical, intent(in) :: hgrid
    real, intent(in), dimension(nims:nime,njms:njme) :: &
         W1,W2,W3,W4
    integer, intent(in), dimension(nims:nime,njms:njme) :: II,JJ

    integer, intent(in):: cims, cime, cjms, cjme,   &
         nids, nide, njds, njde,   &
         nims, nime, njms, njme,   &
         nits, nite, njts, njte
    real, intent(in) :: cfield(cims:cime,cjms:cjme)
    real, intent(out) :: nfield(nims:nime,njms:njme)

    integer :: j,i,a,nx

    nx=min(nide-1,nite)-max(nids,nits)+1

    bigj: do j=max(njds,njts),min(njde-1,njte)
       interploop: do i=max(nids,nits),min(nide-1,nite)
          if(hgrid) then
             a=1-mod(JJ(i,j),2)
          else
             a=mod(JJ(i,j),2)
          endif
          nfield(i,j)=ICOPY2D(cfield,i,j)
       enddo interploop
    end do bigj
  end subroutine c2n_copy2d_nomask


  ! ********************************************************************
  ! subs *_COPY3D -- horizontally interpolate but do not vertically
  ! interpolate
  ! ********************************************************************

  subroutine c2b_copy3d  (II,JJ,W1,W2,W3,W4,cfield,         &
       fbxs,fbxe,fbys,fbye,                  &
       cims, cime, cjms, cjme, ckms, ckme,   &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte, hgrid)
    implicit none
    integer, parameter :: bdyw = 1
    logical, intent(in) :: hgrid
    integer, intent(in):: &
         cims, cime, cjms, cjme, ckms, ckme,   &
         nids, nide, njds, njde, nkds, nkde,   &
         nims, nime, njms, njme, nkms, nkme,   &
         nits, nite, njts, njte, nkts, nkte

    real, intent(in), dimension(nims:nime,njms:njme) :: &
         W1,W2,W3,W4
    integer, intent(in), dimension(nims:nime,njms:njme) :: II,JJ
    real, intent(in) :: cfield(cims:cime,cjms:cjme,ckms:ckme)

    ! Output field boundary info:
    real,dimension(nims:nime,nkms:nkme,bdyw) :: fbys,fbye
    real,dimension(njms:njme,nkms:nkme,bdyw) :: fbxs,fbxe

    integer :: j,i,a,nx,nz,ck,k,j1,j2
    real :: weight

    nx=min(nide-1,nite)-max(nids,nits)+1
    nz=nkde-nkds+1

    j1=max(njts-1,njds)
    if(mod(j1,2)/=0)   j1=j1+1

    j2=min(njte+1,njde-1)
    if(mod(j2,2)/=0)   j2=j2-1

    if(nits==1) then
       i=1
       do j=j1,j2,2
          if(hgrid) then
             a=1-mod(JJ(i,j),2)
          else
             a=mod(JJ(i,j),2)
          endif
          kloop1: do k=nkds,min(nkde,nkte)
             fbxs(j,k,1)=ICOPY(cfield,i,j,k)
          enddo kloop1
       enddo
    endif
    if(nite>=nide-1) then
       i=nide-1
       do j=j1,j2,2
          if(hgrid) then
             a=1-mod(JJ(i,j),2)
          else
             a=mod(JJ(i,j),2)
          endif
          kloop2: do k=nkds,min(nkde,nkte)
             fbxe(j,k,1)=ICOPY(cfield,i,j,k)
          enddo kloop2
       enddo
    endif
    if(njts==1) then
       j=1
       do i=max(nits-1,nids),min(nite+1,nide-1)
          if(hgrid) then
             a=1-mod(JJ(i,j),2)
          else
             a=mod(JJ(i,j),2)
          endif
          kloop3: do k=nkts,min(nkde,nkte)
             fbys(i,k,1)=ICOPY(cfield,i,j,k)
          enddo kloop3
       enddo
    endif
    if(njte>=njde-1) then
       j=njde-1
       do i=max(nits-1,nids),min(nite+1,nide-1)
          if(hgrid) then
             a=1-mod(JJ(i,j),2)
          else
             a=mod(JJ(i,j),2)
          endif
          kloop4: do k=nkts,min(nkde,nkte)
             fbye(i,k,1)=ICOPY(cfield,i,j,k)
          enddo kloop4
       enddo
    endif
  end subroutine c2b_copy3d
  
  subroutine n2c_copy3d (&
       cfield,nfield,ipos,jpos,  &
       cids, cide, cjds, cjde, ckds, ckde,   &
       cims, cime, cjms, cjme, ckms, ckme,   &
       cits, cite, cjts, cjte, ckts, ckte,   &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte,   &
       hgrid)
    implicit none
    logical, intent(in) :: hgrid
    integer, intent(in) :: &
         cids, cide, cjds, cjde, ckds, ckde,   &
         cims, cime, cjms, cjme, ckms, ckme,   &
         cits, cite, cjts, cjte, ckts, ckte,   &
         nids, nide, njds, njde, nkds, nkde,   &
         nims, nime, njms, njme, nkms, nkme,   &
         nits, nite, njts, njte, nkts, nkte,   &
         ipos,jpos
    real, intent(out) :: cfield(cims:cime,cjms:cjme,ckms:ckme)
    real, intent(in) :: nfield(nims:nime,njms:njme,nkms:nkme)

    integer, parameter :: nri=3, nrj=3 ! parent:nest ratio, must be 3 in NMM
    integer :: nx,nz,jstart,jend,istart,iend,ni,nj,a,i,j,k,nk
    real :: weight

    nx=min(nide-2,nite)-max(nids+1,nits)+1
    nz=nkde-nkds+1

    jstart=MAX(jpos+1,cjts)
    jend=MIN(jpos+(njde-njds)/nrj-1,cjte)

    bigj: do j=jstart,jend
       if(hgrid) then
          a=mod(j,2)
       else
          a=1-mod(j,2)
       endif
       istart=MAX(ipos+a,cits)
       iend=MIN(ipos+(nide-nids)/nri-1,cite)
       nj = (j-jpos)*nrj + 1

       iloop: do i=istart,iend
          do k=nkds,nkde
             ni = (i-ipos)*nri + 2 - a
             cfield(i,j,k)=NGRAB(nfield,ni,nj,k)
          enddo
       enddo iloop
    enddo bigj
  end subroutine n2c_copy3d

  subroutine n2c_max3d (&
       cfield,nfield,ipos,jpos,  &
       cids, cide, cjds, cjde, ckds, ckde,   &
       cims, cime, cjms, cjme, ckms, ckme,   &
       cits, cite, cjts, cjte, ckts, ckte,   &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte,   &
       hgrid)
    implicit none
    logical, intent(in) :: hgrid
    integer, intent(in) :: &
         cids, cide, cjds, cjde, ckds, ckde,   &
         cims, cime, cjms, cjme, ckms, ckme,   &
         cits, cite, cjts, cjte, ckts, ckte,   &
         nids, nide, njds, njde, nkds, nkde,   &
         nims, nime, njms, njme, nkms, nkme,   &
         nits, nite, njts, njte, nkts, nkte,   &
         ipos,jpos
    real, intent(out) :: cfield(cims:cime,cjms:cjme,ckms:ckme)
    real, intent(in) :: nfield(nims:nime,njms:njme,nkms:nkme)

    integer, parameter :: nri=3, nrj=3 ! parent:nest ratio, must be 3 in NMM
    integer :: nx,nz,jstart,jend,istart,iend,ni,nj,a,i,j,k,nk
    real :: weight

    nx=min(nide-2,nite)-max(nids+1,nits)+1
    nz=nkde-nkds+1

    jstart=MAX(jpos+1,cjts)
    jend=MIN(jpos+(njde-njds)/nrj-1,cjte)

    bigj: do j=jstart,jend
       if(hgrid) then
          a=mod(j,2)
       else
          a=1-mod(j,2)
       endif
       istart=MAX(ipos+a,cits)
       iend=MIN(ipos+(nide-nids)/nri-1,cite)
       nj = (j-jpos)*nrj + 1

       iloop: do i=istart,iend
          do k=nkds,nkde
             ni = (i-ipos)*nri + 2 - a
             N2I_SET_MAX(cfield,nfield,i,j,k,ni,nj)
          enddo
       enddo iloop
    enddo bigj
  end subroutine n2c_max3d

  subroutine c2n_copy3d (II,JJ,W1,W2,W3,W4,    &
       cfield,nfield,imask,      &
       cims, cime, cjms, cjme, ckms, ckme,   &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte,   &
       hgrid)
    implicit none
    real, intent(in), dimension(nims:nime,njms:njme) :: &
         W1,W2,W3,W4
    integer, intent(in), dimension(nims:nime,njms:njme) :: II,JJ
    logical, intent(in) :: hgrid
    integer, intent(in):: cims, cime, cjms, cjme, ckms, ckme,   &
         nids, nide, njds, njde, nkds, nkde,   &
         nims, nime, njms, njme, nkms, nkme,   &
         nits, nite, njts, njte, nkts, nkte
    integer, intent(in), dimension(nims:nime,njms:njme) :: imask
    real, intent(in) :: cfield(cims:cime,cjms:cjme,ckms:ckme)
    real, intent(out) :: nfield(nims:nime,njms:njme,nkms:nkme)

    integer :: j,i,a,nx,nz,k

    nx=min(nide-1,nite)-max(nids,nits)+1
    nz=nkde-nkds+1

    bigj: do j=max(njds,njts),min(njde-1,njte)
       interploop: do i=max(nids,nits),min(nide-1,nite)
          if(imask(i,j)/=0) cycle interploop
          kinterploop: do k=nkds,nkde
             if(hgrid) then
                a=1-mod(JJ(i,j),2)
             else
                a=mod(JJ(i,j),2)
             endif
             nfield(i,j,k) = ICOPY(cfield,i,j,k)
          enddo kinterploop
       enddo interploop
    end do bigj
  end subroutine c2n_copy3d

  ! ********************************************************************
  ! subs *_MASS -- horizontally and vertically interpolate.  Vertical
  ! interpolation uses the iinfo and winfo arrays (and equivalent
  ! boundary arrays) generated in the *_fulldom subroutines.
  ! ********************************************************************

  subroutine c2b_mass (II,JJ,W1,W2,W3,W4,cfield,         &
       ibxs,ibxe,ibys,ibye,                  &
       wbxs,wbxe,wbys,wbye,                  &
       fbxs,fbxe,fbys,fbye,                  &
       emethod,evalue,                       &
       cims, cime, cjms, cjme, ckms, ckme,   &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte)
    use module_interp_store, only: kpres
    implicit none
    integer, parameter :: bdyw = 1

    integer, intent(in):: &
         cims, cime, cjms, cjme, ckms, ckme,   &
         nids, nide, njds, njde, nkds, nkde,   &
         nims, nime, njms, njme, nkms, nkme,   &
         nits, nite, njts, njte, nkts, nkte

    integer, intent(in) :: emethod ! extrapolation method
    real, intent(in) :: evalue
    real, intent(in), dimension(nims:nime,njms:njme) :: &
         W1,W2,W3,W4
    integer, intent(in), dimension(nims:nime,njms:njme) :: II,JJ
    real, intent(in) :: cfield(cims:cime,cjms:cjme,ckms:ckme)

    ! Output field boundary info:
    real,dimension(nims:nime,nkms:nkme,bdyw) :: fbys,fbye
    real,dimension(njms:njme,nkms:nkme,bdyw) :: fbxs,fbxe

    ! Weights and indices:
    real,dimension(nims:nime,nkms:nkme,bdyw) :: wbys,wbye
    real,dimension(njms:njme,nkms:nkme,bdyw) :: wbxs,wbxe
    integer,dimension(nims:nime,nkms:nkme,bdyw) :: ibys,ibye
    integer,dimension(njms:njme,nkms:nkme,bdyw) :: ibxs,ibxe

    integer :: j,i,a,nx,nz,ck,k,j1,j2
    real :: weight

    nx=min(nide-1,nite)-max(nids,nits)+1
    nz=nkde-nkds+1

    j1=max(njts-1,njds)
    if(mod(j1,2)/=1)   j1=j1+1

    j2=min(njte+1,njde-1)
    if(mod(j2,2)/=1)   j2=j2-1

    if(nits==1) then
       i=1
       do j=j1,j2,2
          a=1-mod(JJ(i,j),2)
          kcopy1: do k=min(nkde,nkte),kpres+1,-1
             fbxs(j,k,1) = ICOPY(cfield,i,j,k)
          enddo kcopy1
          kloop1: do k=kpres,nkds,-1
             weight=wbxs(j,k,1)
             ck=ibxs(j,k,1)
             
             if(ck>1) then
                fbxs(j,k,1) = &
                     ICOPY(cfield,i,j,ck)   * weight + &
                     ICOPY(cfield,i,j,ck-1) * (1.0-weight)
             else
                ! Extrapolate
                if(emethod==EConst) then
                   fbxs(j,k,1)=evalue
                else if(emethod==ECopy) then
                   fbxs(j,k,1)=ICOPY(cfield,i,j,1)
                else ! assume 2: linear extrap
                   fbxs(j,k,1)=evalue * weight + &
                               ICOPY(cfield,i,j,1) * (1.0-weight)
                endif
             endif
          enddo kloop1
       enddo
    endif
    if(nite>=nide-1) then
       i=nide-1
       do j=j1,j2,2
          a=1-mod(JJ(i,j),2)
          kcopy2: do k=min(nkde,nkte),kpres+1,-1
             fbxe(j,k,1)=ICOPY(cfield,i,j,k)
          enddo kcopy2
          kloop2: do k=kpres,nkds,-1
             weight=wbxe(j,k,1)
             ck=ibxe(j,k,1)

             if(ck>1) then
                fbxe(j,k,1) = &
                     ICOPY(cfield,i,j,ck)   * weight + &
                     ICOPY(cfield,i,j,ck-1) * (1.0-weight)
             else
                ! Extrapolate
                if(emethod==EConst) then
                   fbxe(j,k,1)=evalue
                else if(emethod==ECopy) then
                   fbxe(j,k,1)=ICOPY(cfield,i,j,1)
                else ! assume 2: linear extrap
                   fbxe(j,k,1)=evalue * weight + &
                        ICOPY(cfield,i,j,1) * (1.0-weight)
                endif
             endif
          enddo kloop2
       enddo
    endif
    if(njts==1) then
       j=1
       do i=max(nits-1,nids),min(nite+1,nide-1)
          a=1-mod(JJ(i,j),2)
          kcopy3: do k=min(nkde,nkte),kpres+1,-1
             fbys(i,k,1) = ICOPY(cfield,i,j,k)
          enddo kcopy3
          kloop3: do k=kpres,nkds,-1
             weight=wbys(i,k,1)
             ck=ibys(i,k,1)

             if(ck>1) then
                fbys(i,k,1) = &
                     ICOPY(cfield,i,j,ck)   * weight + &
                     ICOPY(cfield,i,j,ck-1) * (1.0-weight)
             else
                ! Extrapolate
                if(emethod==EConst) then
                   fbys(i,k,1)=evalue
                else if(emethod==ECopy) then
                   fbys(i,k,1)=ICOPY(cfield,i,j,1)
                else ! assume 2: linear extrap
                   fbys(i,k,1)=evalue*weight + &
                        ICOPY(cfield,i,j,1)*(1.0-weight)
                endif
             endif
          enddo kloop3
       enddo
    endif
    if(njte>=njde-1) then
       j=njde-1
       do i=max(nits-1,nids),min(nite+1,nide-1)
          a=1-mod(JJ(i,j),2)
          kcopy4: do k=min(nkde,nkte),kpres+1,-1
             fbye(i,k,1) = ICOPY(cfield,i,j,k)
          enddo kcopy4
          kloop4: do k=kpres,nkds,-1
             weight=wbye(i,k,1)
             ck=ibye(i,k,1)

             if(ck>1) then
                fbye(i,k,1) = &
                     ICOPY(cfield,i,j,ck)   * weight + &
                     ICOPY(cfield,i,j,ck-1) * (1.0-weight)
             else
                if(emethod==EConst) then
                   fbye(i,k,1)=evalue
                else if(emethod==ECopy) then
                   fbye(i,k,1)=ICOPY(cfield,i,j,1)
                else ! assume 2: linear extrap
                   fbye(i,k,1)=evalue*weight + &
                        ICOPY(cfield,i,j,1)*(1.0-weight)
                endif
             endif
          enddo kloop4
       enddo
    endif

  end subroutine c2b_mass

  subroutine n2c_mass (&
       cfield,nfield,iinfo,winfo,ipos,jpos,  &
       emethod, evalue,                      &
       cids, cide, cjds, cjde, ckds, ckde,   &
       cims, cime, cjms, cjme, ckms, ckme,   &
       cits, cite, cjts, cjte, ckts, ckte,   &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte)
    use module_interp_store, only: kpres
    implicit none
    integer, intent(in) :: &
         cids, cide, cjds, cjde, ckds, ckde,   &
         cims, cime, cjms, cjme, ckms, ckme,   &
         cits, cite, cjts, cjte, ckts, ckte,   &
         nids, nide, njds, njde, nkds, nkde,   &
         nims, nime, njms, njme, nkms, nkme,   &
         nits, nite, njts, njte, nkts, nkte,   &
         ipos,jpos, emethod
    real, intent(in) :: evalue
    real, intent(in), dimension(cims:cime,cjms:cjme,ckms:ckme) :: winfo
    integer, intent(in), dimension(cims:cime,cjms:cjme,ckms:ckme) :: iinfo
    real, intent(out) :: cfield(cims:cime,cjms:cjme,ckms:ckme)
    real, intent(in) :: nfield(nims:nime,njms:njme,nkms:nkme)

    integer, parameter :: nri=3, nrj=3 ! parent:nest ratio, must be 3 in NMM
    integer :: nz,jstart,jend,istart,iend,ni,nj,a,i,j,k,nk
    real :: weight

    nz=nkde-nkds+1

    jstart=MAX(jpos+1,cjts)
    jend=MIN(jpos+(njde-njds)/nrj-1,cjte)

    kcopy: do k=ckde,kpres+1,-1
       jcopy: do j=jstart,jend
          a=mod(j,2)
          istart=MAX(ipos+a,cits)
          iend=MIN(ipos+(nide-nids)/nri-1,cite)
          nj = (j-jpos)*nrj + 1
          icopy: do i=istart,iend
             ni = (i-ipos)*nri + 2 - a
             cfield(i,j,k) = NGRAB(nfield,ni,nj,k)
          enddo icopy
       enddo jcopy
    enddo kcopy

    bigj: do j=jstart,jend
          a=mod(j,2)
       istart=MAX(ipos+a,cits)
       iend=MIN(ipos+(nide-nids)/nri-1,cite)
       nj = (j-jpos)*nrj + 1

       iloop: do i=istart,iend
          ni = (i-ipos)*nri + 2 - a
          kinterploop: do k=kpres,ckds,-1
             weight=winfo(i,j,k)
             nk=iinfo(i,j,k)

             if(nk>1) then
                cfield(i,j,k) = &
                     NGRAB(nfield,ni,nj,nk)   * weight + &
! pjj/cray - source line limit in Cray compiler
             NGRAB(nfield,ni,nj,nk-1) &
             * (1.0-weight)
             else
                ! Extrapolate
                if(emethod==EConst) then
                   cfield(i,j,k)=evalue
                elseif(emethod==ECopy) then
                   cfield(i,j,k)=NGRAB(nfield,ni,nj,1)
                else ! Assume 2: linear extrap
                   cfield(i,j,k)=evalue*weight + &
                        NGRAB(nfield,ni,nj,1)*(1.0-weight)
                endif
             endif
          end do kinterploop
       enddo iloop
    enddo bigj
  end subroutine n2c_mass

  subroutine n2c_massikj (&
       cfield,nfield,iinfo,winfo,ipos,jpos,  &
       emethod, evalue,                      &
       cids, cide, cjds, cjde, ckds, ckde,   &
       cims, cime, cjms, cjme, ckms, ckme,   &
       cits, cite, cjts, cjte, ckts, ckte,   &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte)
    use module_interp_store, only: kpres
    implicit none
    integer, intent(in) :: &
         cids, cide, cjds, cjde, ckds, ckde,   &
         cims, cime, cjms, cjme, ckms, ckme,   &
         cits, cite, cjts, cjte, ckts, ckte,   &
         nids, nide, njds, njde, nkds, nkde,   &
         nims, nime, njms, njme, nkms, nkme,   &
         nits, nite, njts, njte, nkts, nkte,   &
         ipos,jpos, emethod
    real, intent(in) :: evalue
    real, intent(in), dimension(cims:cime,cjms:cjme,ckms:ckme) :: winfo
    integer, intent(in), dimension(cims:cime,cjms:cjme,ckms:ckme) :: iinfo
    real, intent(out) :: cfield(cims:cime,ckms:ckme,cjms:cjme)
    real, intent(in) :: nfield(nims:nime,nkms:nkme,njms:njme)

    integer, parameter :: nri=3, nrj=3 ! parent:nest ratio, must be 3 in NMM
    integer :: nz,jstart,jend,istart,iend,ni,nj,a,i,j,k,nk
    real :: weight

    nz=nkde-nkds+1

    jstart=MAX(jpos+1,cjts)
    jend=MIN(jpos+(njde-njds)/nrj-1,cjte)

    bigj: do j=jstart,jend
          a=mod(j,2)
       istart=MAX(ipos+a,cits)
       iend=MIN(ipos+(nide-nids)/nri-1,cite)
       nj = (j-jpos)*nrj + 1

       kcopyloop: do k=nkde,kpres+1,-1
          icopyloop: do i=istart,iend
             ni = (i-ipos)*nri + 2 - a
             cfield(i,k,j) = NGRABIKJ(nfield,ni,k,nj)
          enddo icopyloop
       enddo kcopyloop

       kinterploop: do k=kpres+1,nkds,-1
          iloop: do i=istart,iend
             ni = (i-ipos)*nri + 2 - a
             weight=winfo(i,j,k)
             nk=iinfo(i,j,k)

             if(nk>1) then
                cfield(i,k,j) = &
                     NGRABIKJ(nfield,ni,nk,nj) * weight + &
! pjj/cray - source line limit in Cray compiler
             NGRABIKJ(nfield,ni,nk-1,nj) &
             * (1.0-weight)
             else
                ! Extrapolate
                if(emethod==EConst) then
                   cfield(i,k,j)=evalue
                elseif(emethod==ECopy) then
                   cfield(i,k,j)=NGRABIKJ(nfield,ni,1,nj)
                else ! Assume 2: linear extrap
                   cfield(i,k,j)=evalue*weight + &
                        NGRABIKJ(nfield,ni,1,nj)*(1.0-weight)
                endif
             endif
          enddo iloop
       end do kinterploop
    enddo bigj
  end subroutine n2c_massikj

  subroutine c2n_mass (II,JJ,W1,W2,W3,W4,    &
       cfield,nfield,iinfo,winfo,imask,      &
       emethod, evalue,                      &
       cims, cime, cjms, cjme, ckms, ckme,   &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte)
    use module_interp_store, only: kpres
    implicit none
    real, intent(in), dimension(nims:nime,njms:njme) :: &
         W1,W2,W3,W4
    integer, intent(in), dimension(nims:nime,njms:njme) :: II,JJ

    integer, intent(in):: cims, cime, cjms, cjme, ckms, ckme,   &
         nids, nide, njds, njde, nkds, nkde,   &
         nims, nime, njms, njme, nkms, nkme,   &
         nits, nite, njts, njte, nkts, nkte, emethod
    real, intent(in) :: evalue
    real, intent(in), dimension(nims:nime,njms:njme,nkms:nkme) :: winfo
    integer, intent(in), dimension(nims:nime,njms:njme,nkms:nkme) :: iinfo
    integer, intent(in), dimension(nims:nime,njms:njme) :: imask
    real, intent(in) :: cfield(cims:cime,cjms:cjme,ckms:ckme)
    real, intent(out) :: nfield(nims:nime,njms:njme,nkms:nkme)
    character*255 :: message
    integer :: j,i,a,nx,nz,ck,k
    real :: weight

    nx=min(nide-1,nite)-max(nids,nits)+1
    nz=nkde-nkds+1
    if(kpres<=nkds .or. kpres>=nkde) then
       call wrf_error_fatal('invalid kpres: outside domain bounds')
    end if
    bigj: do j=max(njds,njts),min(njde-1,njte)
       interploop: do i=max(nids,nits),min(nide-1,nite)
          if(imask(i,j)/=0) cycle interploop
          a=1-mod(JJ(i,j),2)
          kcopyloop: do k=nkde,kpres+1,-1
             nfield(i,j,k)=ICOPY(cfield,i,j,k)
             !weight=winfo(i,j,k)
             ck=iinfo(i,j,k)
!              if(1.-weight>1e-5) then
! 2000            format(I0,", ",I0,", ",I0,": invalid weight at constant pressure level: w=",F0.5," i=",I0)
!                 write(message,2000) i,j,k, weight,ck
!                 call wrf_error_fatal(message)
!              endif
!              if(ck/=k) then
! 2001            format(I0,", ",I0,", ",I0,": invalid iinfo at constant pressure level: w=",F0.5," i=",I0)
!                 write(message,2000) i,j,k, weight,ck
!                 call wrf_error_fatal(message)
!              endif
          enddo kcopyloop
          kinterploop: do k=kpres,nkds,-1
             weight=winfo(i,j,k)
             ck=iinfo(i,j,k)

             if(ck>1) then
                nfield(i,j,k) = &
                     ICOPY(cfield,i,j,ck)   * weight + &
                     ICOPY(cfield,i,j,ck-1) * (1.0-weight)
             else                
                ! Extrapolate
                if(emethod==EConst) then
                   nfield(i,j,k)=evalue
                elseif(emethod==ECopy) then
                   nfield(i,j,k)=ICOPY(cfield,i,j,1)
                else ! Assume 2: linear extrap
                   nfield(i,j,k)=evalue*weight + &
                        ICOPY(cfield,i,j,1)*(1.0-weight)
                endif
             endif
          enddo kinterploop
       enddo interploop
    end do bigj

  end subroutine c2n_mass

  subroutine c2n_massikj (II,JJ,W1,W2,W3,W4, &
       cfield,nfield,iinfo,winfo,imask,      &
       emethod, evalue,                      &
       cims, cime, cjms, cjme, ckms, ckme,   &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte)
    use module_interp_store, only: kpres
    implicit none
    real, intent(in), dimension(nims:nime,njms:njme) :: &
         W1,W2,W3,W4
    integer, intent(in), dimension(nims:nime,njms:njme) :: II,JJ

    integer, intent(in):: cims, cime, cjms, cjme, ckms, ckme,   &
         nids, nide, njds, njde, nkds, nkde,   &
         nims, nime, njms, njme, nkms, nkme,   &
         nits, nite, njts, njte, nkts, nkte, emethod
    real, intent(in), dimension(nims:nime,njms:njme,nkms:nkme) :: winfo
    integer, intent(in), dimension(nims:nime,njms:njme,nkms:nkme) :: iinfo
    integer, intent(in), dimension(nims:nime,njms:njme) :: imask
    real, intent(in) :: cfield(cims:cime,ckms:ckme,cjms:cjme)
    real, intent(out) :: nfield(nims:nime,nkms:nkme,njms:njme)
    real, intent(In) :: evalue

    integer :: j,i,a,nx,nz,ck,k
    real :: weight

    nx=min(nide-1,nite)-max(nids,nits)+1
    nz=nkde-nkds+1

    bigj: do j=max(njds,njts),min(njde-1,njte)
       interploop: do i=max(nids,nits),min(nide-1,nite)
          if(imask(i,j)/=0) cycle interploop
          a=1-mod(JJ(i,j),2)
          kcopyloop: do k=nkde,kpres+1,-1
             nfield(i,k,j)=IKJCOPY(cfield,i,k,j)
          end do kcopyloop
          kinterploop: do k=kpres,nkds,-1
             weight=winfo(i,j,k)
             ck=iinfo(i,j,k)

             if(ck>1) then
                nfield(i,k,j) = &
                     IKJCOPY(cfield,i,ck,j)   * weight + &
                     IKJCOPY(cfield,i,ck-1,j) * (1.0-weight)
             else
                ! Extrapolate
                if(emethod==EConst) then
                   nfield(i,k,j)=evalue
                elseif(emethod==ECopy) then
                   nfield(i,k,j)=IKJCOPY(cfield,i,1,j)
                else ! Assume 2: linear extrapolation
                   nfield(i,k,j)=evalue * weight + &
                               IKJCOPY(cfield,i,1,j) * (1.0-weight)
                endif
             endif
          enddo kinterploop
       enddo interploop
    end do bigj

  end subroutine c2n_massikj

  subroutine find_kpres(kpres, eta2, kds,kde, kms,kme)
    ! Find the level at which the pressure/sigma transition occurs.
    ! All levels above this are pure pressure levels, whereas kpres
    ! and below are sigma.
    integer, intent(out) :: kpres
    real, intent(in) :: eta2(kms:kme)
    integer, intent(in) :: kds,kde, kms,kme

    integer :: k

    k=kde-1
    do while(eta2(k) < 1e-5 .and. eta2(k-1) < 1e-5)
       k=k-1
       if(k<=kds) then
          call wrf_error_fatal('New NMM interpolation routines do not work in a constant pressure space.')
       endif
    enddo
    kpres=k
  end subroutine find_kpres

  ! ********************************************************************
  ! subs *_FULLDOM -- recalculates PD and PINT based on FIS if
  ! requested.  Also, generates vertical interpolation arrays for use
  ! in later interpolations and interpolates T and Q while doing so.
  ! ********************************************************************

  subroutine n2c_fulldom  (                  &
       deta1,deta2, eta1,eta2, ptop,pdtop,   &
       cfis,cpint,ct,cpd,cq,                 &
       cids, cide, cjds, cjde, ckds, ckde,   &
       cims, cime, cjms, cjme, ckms, ckme,   &
       cits, cite, cjts, cjte, ckts, ckte,   &
       nfis,npint,nt,npd,nq,                 &
       ipos,jpos,                            &
!       nri,nrj                               &
       out_iinfo,out_winfo,                  &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte, kpres)

    implicit none

    integer, intent(in) :: ipos,jpos
!    integer, intent(in) :: nri,nrj
    integer, parameter :: nri=3, nrj=3 ! parent:nest ratio, must be 3 in NMM

    integer, intent(in):: &
         nids, nide, njds, njde, nkds, nkde,   &
         nims, nime, njms, njme, nkms, nkme,   &
         nits, nite, njts, njte, nkts, nkte,   kpres
    integer, intent(in):: &
         cids, cide, cjds, cjde, ckds, ckde,   &
         cims, cime, cjms, cjme, ckms, ckme,   &
         cits, cite, cjts, cjte, ckts, ckte
    real, intent(out), dimension(cims:cime,cjms:cjme,ckms:ckme) :: cT,cQ,cPINT
    real, intent(inout), dimension(cims:cime,cjms:cjme,1) :: cPD,cFIS

    real, intent(out), dimension(cims:cime,cjms:cjme,ckms:ckme) :: out_winfo
    integer, intent(out), dimension(cims:cime,cjms:cjme,ckms:ckme) :: out_iinfo

    real, intent(in), dimension(nims:nime,njms:njme,nkms:nkme) :: nT,nQ
    real, intent(in), dimension(nims:nime,njms:njme,nkms:nkme) :: nPINT
    real, intent(in), dimension(nims:nime,njms:njme,1) :: nFIS
    real, intent(in), dimension(nims:nime,njms:njme,1) :: nPD

    real, intent(in), dimension(nkms:nkme) :: eta1,eta2,deta1,deta2
    real, intent(in) :: ptop,pdtop

    real, dimension(1,nite-nits+1) :: inFIS,inPD,icFIS,icPD
    real, dimension(nkde-1,nite-nits+1) :: inT0,inQ,icT,icQ, qinfo,winfo
    real, dimension(nkde,nite-nits+1) :: inPINT,icPINT
    integer, dimension(nkde-1,nite-nits+1) :: iinfo
    integer :: nx,nz,k,i,a,j, istart,iend,jstart,jend, ni,nj,jprint,itest,jtest
    character*255 :: message
    logical bad, warned

    warned=.false. ! allow one P>PSTD message

    nx=min(cide-2,cite)-max(cids+1,cits)+1
    nz=ckde-ckds+1

    jstart=MAX(jpos+1,cjts)
    jend=MIN(jpos+(njde-njds)/nrj-1,cjte)

    bigj: do j=jstart,jend
       nj = (j-jpos)*nrj + 1

       a=mod(j,2)
       istart=MAX(ipos+a,cits)
       iend=MIN(ipos+(nide-nids)/nri-1,cite)
       nx=iend-istart+1

       ! STEP 1: Copy coarse and fine nest data into 
       ! temporary arrays, reordering dimensions:
       qtloop: do k=ckts,ckte-1
          do i=istart,iend
             ni = (i-ipos)*nri + 2 - a

             UPCOPY(inT0,nT,i,j,k,ni,nj)
             UPCOPY(inQ,nQ,i,j,k,ni,nj)
             UPCOPY(inPINT,nPINT,i,j,k,ni,nj)
          enddo
       enddo qtloop

       k=nkte
       loop2d: do i=istart,iend
          ni = (i-ipos)*nri + 2 - a

          UPCOPY(inPINT,nPINT,i,j,k,ni,nj)
          UPCOPY(inPD,nPD,i,j,1,ni,nj)
          UPCOPY(inFIS,nFIS,i,j,1,ni,nj)

          icPD(1,i-istart+1)=cPD(i,j,1)
!          icPD(1,i-istart+1)=use_this_pd(i,j,1)
          icFIS(1,i-istart+1)=cFIS(i,j,1)
       enddo loop2d

       ! Step 2: Interpolate coarse grid to fine grid in reordered
       ! arrays:
       call interp_T_PD_Q(nmm_method_linear, nmm_keep_pd, nx,nz, &
            deta1,deta2,eta1,eta2,ptop,pdtop, kpres, &
            inFIS,icFIS, inPINT,icPINT, inT0, icT, inPD,icPD, inQ,icQ, &
            iinfo, winfo, warned)

       ! Step 3: Copy back from reordered arrays to final nest arrays:

       qtloop2: do k=ckts,max(ckte-1,ckde-1)
          do i=istart,iend
             cT(i,j,k)=icT(k,i-istart+1)
             cQ(i,j,k)=icQ(k,i-istart+1)
          enddo
       enddo qtloop2

       izloop: do k=ckts,max(ckte-1,ckde-1)
          ixloop: do i=istart,iend
             out_iinfo(i,j,k)=iinfo(k,i-istart+1)
             out_winfo(i,j,k)=winfo(k,i-istart+1)
          enddo ixloop
       enddo izloop

       iendloop: do i=istart,iend
          out_iinfo(i,j,nkde)=nkde
          out_winfo(i,j,nkde)=1.0
       end do iendloop

       k=nkte+1
       loop2d2: do i=istart,iend
          cPD(i,j,1)=icPD(1,i-istart+1)
       enddo loop2d2
    end do bigj
  end subroutine n2c_fulldom

  subroutine n2c_fulldom_new (               &
       deta1,deta2, eta1,eta2, ptop,pdtop,   &
       cfis,cpint,ct,cpd,cq,                 &
       cids, cide, cjds, cjde, ckds, ckde,   &
       cims, cime, cjms, cjme, ckms, ckme,   &
       cits, cite, cjts, cjte, ckts, ckte,   &
       nfis,npint,nt,npd,nq,                 &
       ipos,jpos,                            &
!       nri,nrj                               &
       out_iinfo,out_winfo,                  &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte, kpres)

    implicit none

    integer, intent(in) :: ipos,jpos
!    integer, intent(in) :: nri,nrj
    integer, parameter :: nri=3, nrj=3 ! parent:nest ratio, must be 3 in NMM

    integer, intent(in):: &
         nids, nide, njds, njde, nkds, nkde,   &
         nims, nime, njms, njme, nkms, nkme,   &
         nits, nite, njts, njte, nkts, nkte,   kpres
    integer, intent(in):: &
         cids, cide, cjds, cjde, ckds, ckde,   &
         cims, cime, cjms, cjme, ckms, ckme,   &
         cits, cite, cjts, cjte, ckts, ckte
    real, intent(out), dimension(cims:cime,cjms:cjme,ckms:ckme) :: cT,cQ,cPINT
    real, intent(inout), dimension(cims:cime,cjms:cjme,1) :: cPD,cFIS

    real, intent(out), dimension(cims:cime,cjms:cjme,ckms:ckme) :: out_winfo
    integer, intent(out), dimension(cims:cime,cjms:cjme,ckms:ckme) :: out_iinfo

    real, intent(in), dimension(nims:nime,njms:njme,nkms:nkme) :: nT,nQ
    real, intent(in), dimension(nims:nime,njms:njme,nkms:nkme) :: nPINT
    real, intent(in), dimension(nims:nime,njms:njme,1) :: nFIS
    real, intent(in), dimension(nims:nime,njms:njme,1) :: nPD

    real, intent(in), dimension(nkms:nkme) :: eta1,eta2,deta1,deta2
    real, intent(in) :: ptop,pdtop

    real, dimension(1,nite-nits+1) :: inFIS,inPD,icFIS,icPD
    real, dimension(kpres+1,nite-nits+1) :: inT0,inQ,icT,icQ, qinfo,winfo
    real, dimension(kpres+2,nite-nits+1) :: inPINT,icPINT
    integer, dimension(kpres+1,nite-nits+1) :: iinfo
    integer :: nx,nz,k,i,a,j, istart,iend,jstart,jend, ni,nj,jprint,itest,jtest
    character*255 :: message
    logical bad, warned

    warned=.false. ! Allow one P>PSTD message

    nx=min(cide-2,cite)-max(cids+1,cits)+1
    nz=ckde-ckds+1

    jstart=MAX(jpos+1,cjts)
    jend=MIN(jpos+(njde-njds)/nrj-1,cjte)

    bigj: do j=jstart,jend
       nj = (j-jpos)*nrj + 1

       a=mod(j,2)
       istart=MAX(ipos+a,cits)
       iend=MIN(ipos+(nide-nids)/nri-1,cite)
       nx=iend-istart+1

       ! STEP 1: Copy coarse and fine nest data into 
       ! temporary arrays, reordering dimensions:
       qtloop: do k=ckts,kpres+1
          do i=istart,iend
             ni = (i-ipos)*nri + 2 - a

             UPCOPY(inT0,nT,i,j,k,ni,nj)
             UPCOPY(inQ,nQ,i,j,k,ni,nj)
             UPCOPY(inPINT,nPINT,i,j,k,ni,nj)
          enddo
       enddo qtloop

       k=kpres+1
       loop2d: do i=istart,iend
          ni = (i-ipos)*nri + 2 - a

          UPCOPY(inPINT,nPINT,i,j,k,ni,nj)
          UPCOPY(inPD,nPD,i,j,1,ni,nj)
          UPCOPY(inFIS,nFIS,i,j,1,ni,nj)

          icPD(1,i-istart+1)=cPD(i,j,1)
!          icPD(1,i-istart+1)=use_this_pd(i,j,1)
          icFIS(1,i-istart+1)=cFIS(i,j,1)
       enddo loop2d

       ! Step 2: Interpolate coarse grid to fine grid in reordered
       ! arrays:
       call interp_T_PD_Q_kpres(nmm_method_linear, nmm_keep_pd, nx,nz, &
            deta1,deta2,eta1,eta2,ptop,pdtop, kpres, kpres+2, &
            inFIS,icFIS, inPINT,icPINT, inT0, icT, inPD,icPD, inQ,icQ, &
            iinfo, winfo, warned)

       ! Step 3: Copy back from reordered arrays to final nest arrays:

       qtloop2: do k=ckts,kpres+1
          do i=istart,iend
             cT(i,j,k)=icT(k,i-istart+1)
             cQ(i,j,k)=icQ(k,i-istart+1)
          enddo
       enddo qtloop2

       izloop: do k=ckts,kpres+1
          ixloop: do i=istart,iend
             out_iinfo(i,j,k)=iinfo(k,i-istart+1)
             out_winfo(i,j,k)=winfo(k,i-istart+1)
          enddo ixloop
       enddo izloop

       k=nkte+1
       loop2d2: do i=istart,iend
          cPD(i,j,1)=icPD(1,i-istart+1)
       enddo loop2d2
    end do bigj

    kcopy: do k=kpres+1,ckde-1
       jcopy: do j=jstart,jend
          nj = (j-jpos)*nrj + 1
          
          a=mod(j,2)
          istart=MAX(ipos+a,cits)
          iend=MIN(ipos+(nide-nids)/nri-1,cite)
          icopy: do i=istart,iend
             ni = (i-ipos)*nri + 2 - a
             cT(i,j,k)=NGRAB(nT,ni,nj,k)
             cQ(i,j,k)=NGRAB(nQ,ni,nj,k)
             out_iinfo(i,j,k)=k
             out_winfo(i,j,k)=1.0
          enddo icopy
          if(k==ckde-1) then
             icopy2: do i=istart,iend
                out_iinfo(i,j,nkde)=nkde
                out_winfo(i,j,nkde)=1.0
             enddo icopy2
          endif
       end do jcopy
    end do kcopy
  end subroutine n2c_fulldom_new

  subroutine c2b_fulldom  (II,JJ,W1,W2,W3,W4,&
       deta1,deta2, eta1,eta2, ptop,pdtop,   &
       cfis,cpint,ct,cpd,cq,    nfis,        &
       cims, cime, cjms, cjme, ckms, ckme,   &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte,   &
       kpres,                                &
       ibxs,    ibxe,    ibys,    ibye,      &
       wbxs,    wbxe,    wbys,    wbye,      &
       pdbxs,   pdbxe,   pdbys,   pdbye,     &
       tbxs,    tbxe,    tbys,    tbye,      &
       qbxs,    qbxe,    qbys,    qbye)
    implicit none
    integer, intent(in):: &
         cims, cime, cjms, cjme, ckms, ckme,   &
         nids, nide, njds, njde, nkds, nkde,   &
         nims, nime, njms, njme, nkms, nkme,   &
         nits, nite, njts, njte, nkts, nkte

    integer, parameter :: bdyw=1

    ! Domain info:
    real, intent(in), dimension(nims:nime,njms:njme) :: W1,W2,W3,W4
    integer, intent(in), dimension(nims:nime,njms:njme) :: II,JJ
    real, intent(in), dimension(nkms:nkme) :: eta1,eta2,deta1,deta2
    real, intent(in) :: ptop,pdtop
    integer, intent(in) :: kpres

    ! Parent fields:
    real, intent(in), dimension(cims:cime,cjms:cjme,ckms:ckme) :: cT,cQ,cPINT
    real, intent(in), dimension(cims:cime,cjms:cjme,1) :: cPD,cFIS

    ! Nest terrain info:
    real, intent(in), dimension(nims:nime,njms:njme,1) :: nFIS

    ! T, Q, PINT, PD boundary info:
    real,dimension(nims:nime,nkms:nkme,bdyw) :: tbys,tbye,qbys,qbye
    real,dimension(njms:njme,nkms:nkme,bdyw) :: tbxs,tbxe,qbxs,qbxe
    real,dimension(nims:nime,1,bdyw) :: pdbys,pdbye
    real,dimension(njms:njme,1,bdyw) :: pdbxs,pdbxe

    ! Weights and indices:
    real,dimension(nims:nime,nkms:nkme,bdyw) :: wbys,wbye
    real,dimension(njms:njme,nkms:nkme,bdyw) :: wbxs,wbxe
    integer,dimension(nims:nime,nkms:nkme,bdyw) :: ibys,ibye
    integer,dimension(njms:njme,nkms:nkme,bdyw) :: ibxs,ibxe

    integer :: i,j,k,a,b,nx,nz,used,j1,j2,used1

    real, dimension(1,2*(nite-nits+5)+2*(njte-njts+5)) :: inFIS,inPD,icFIS,icPD
    real, dimension(nkde-1,2*(nite-nits+5)+2*(njte-njts+5)) :: inT,inQ,icT,icQ,winfo
    integer, dimension(nkde-1,2*(nite-nits+5)+2*(njte-njts+5)) :: iinfo
    real, dimension(nkde,2*(nite-nits+5)+2*(njte-njts+5)) :: inPINT,icPINT
    logical :: warned

    warned=.false. ! Allow one P>PSTD message

    nx=min(nide-1,nite)-max(nids,nits)+1
    nz=nkde-nkds+1

    j1=max(njts-1,njds)
    if(mod(j1,2)/=1)   j1=j1+1

    j2=min(njte+1,njde-1)
    if(mod(j2,2)/=1)   j2=j2-1

    used=0
    bdyloop: do b=1,4
       if_xbdy: if(b==1 .or. b==2) then
          if(b==1) then
             if(nits/=1) cycle bdyloop
             i=1
          endif
          if(b==2) then
             if(nite<nide-1) cycle bdyloop
             i=nide-1
          endif
          do j=j1,j2,2
             a=1-mod(JJ(i,j),2)
             used=used+1
             do k=nkts,nkte-1
                uFCOPY(icT,cT,i,j,k)
                uFCOPY(icQ,cQ,i,j,k)
                uFCOPY(icPINT,cPINT,i,j,k)
             enddo
                
             k=nkte
             a=1-mod(JJ(i,j),2)

             uFCOPY(icPINT,cPINT,i,j,k)
             uFCOPY(icPD,cPD,i,j,1)
             uFCOPY(icFIS,cFIS,i,j,1)

             inFIS(1,used)=nFIS(i,j,1)

          enddo
       endif if_xbdy
       if_ybdy: if(b==3 .or. b==4) then
          if(b==3) then
             if(njts/=1) cycle bdyloop
             j=1
          endif
          if(b==4) then
             if(njte<njde-1) cycle bdyloop
             j=njde-1
          endif
          do i=max(nits-1,nids),min(nite+1,nide-1)
             used=used+1
             do k=nkts,nkte-1
                a=1-mod(JJ(i,j),2)
                uFCOPY(icT,cT,i,j,k)
                uFCOPY(icQ,cQ,i,j,k)
                uFCOPY(icPINT,cPINT,i,j,k)
             enddo
                
             k=nkte
             a=1-mod(JJ(i,j),2)

             uFCOPY(icPINT,cPINT,i,j,k)
             uFCOPY(icPD,cPD,i,j,1)
             uFCOPY(icFIS,cFIS,i,j,1)

             inFIS(1,used)=nFIS(i,j,1)
          enddo
       endif if_ybdy
    enddo bdyloop

    if(used==0) then
       ! No boundary points on this tile so we are done
       return
    endif

    call interp_T_PD_Q_kpres(nmm_method_linear, nmm_interp_pd, used,nz, &
         deta1,deta2,eta1,eta2,ptop,pdtop, kpres, nz, &
         icFIS,inFIS, icPINT,inPINT, icT, inT, icPD,inPD, icQ,inQ, &
         iinfo, winfo, warned)

    used1=used
    used=0

    if_bxs: if(nits==1) then
       i=1
       do j=j1,j2,2
          used=used+1
          do k=nkts,nkte-1
             tbxs(j,k,1)=inT(k,used)
             qbxs(j,k,1)=inQ(k,used)
             ibxs(j,k,1)=iinfo(k,used)
             wbxs(j,k,1)=winfo(k,used)
          enddo
          ibxs(j,nkde,1)=nkde
          wbxs(j,nkde,1)=1.0
          pdbxs(j,1,1)=inPD(1,used)
       enddo
    endif if_bxs

    if_bxe: if(nite>=nide-1) then
       i=nide-1
       do j=j1,j2,2
          used=used+1
          do k=nkts,nkte-1
             tbxe(j,k,1)=inT(k,used)
             qbxe(j,k,1)=inQ(k,used)
             ibxe(j,k,1)=iinfo(k,used)
             wbxe(j,k,1)=winfo(k,used)
          enddo
          ibxe(j,nkde,1)=nkde
          wbxe(j,nkde,1)=1.0
          pdbxe(j,1,1)=inPD(1,used)
       enddo
    endif if_bxe

    if_bys: if(njts==1) then
       j=1
       do i=max(nits-1,nids),min(nite+1,nide-1)
          used=used+1
          do k=nkts,nkte-1
             tbys(i,k,1)=inT(k,used)
             qbys(i,k,1)=inQ(k,used)
             ibys(i,k,1)=iinfo(k,used)
             wbys(i,k,1)=winfo(k,used)
          enddo
          ibys(i,nkde,1)=nkde
          wbys(i,nkde,1)=1.0
          pdbys(i,1,1)=inPD(1,used)
       enddo
    endif if_bys

    if_bye: if(njte>=njde-1) then
       j=njde-1
       do i=max(nits-1,nids),min(nite+1,nide-1)
          used=used+1
          do k=nkts,nkte-1
             tbye(i,k,1)=inT(k,used)
             qbye(i,k,1)=inQ(k,used)
             ibye(i,k,1)=iinfo(k,used)
             wbye(i,k,1)=winfo(k,used)
          enddo
          ibye(i,nkde,1)=nkde
          wbye(i,nkde,1)=1.0
          pdbye(i,1,1)=inPD(1,used)
       enddo
    endif if_bye

    if(used/=used1) then
       call wrf_error_fatal('Number of input and output points does not match.')
    endif

  end subroutine c2b_fulldom

  ! ####################################################################

  subroutine c2b_fulldom_new  (II,JJ,W1,W2,W3,W4,&
       deta1,deta2, eta1,eta2, ptop,pdtop,   &
       cfis,cpint,ct,cpd,cq,    nfis,        &
       cims, cime, cjms, cjme, ckms, ckme,   &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte,   &
       kpres,                                &
       ibxs,    ibxe,    ibys,    ibye,      &
       wbxs,    wbxe,    wbys,    wbye,      &
       pdbxs,   pdbxe,   pdbys,   pdbye,     &
       tbxs,    tbxe,    tbys,    tbye,      &
       qbxs,    qbxe,    qbys,    qbye)
    implicit none
    integer, intent(in):: &
         cims, cime, cjms, cjme, ckms, ckme,   &
         nids, nide, njds, njde, nkds, nkde,   &
         nims, nime, njms, njme, nkms, nkme,   &
         nits, nite, njts, njte, nkts, nkte

    integer, parameter :: bdyw=1

    ! Domain info:
    real, intent(in), dimension(nims:nime,njms:njme) :: W1,W2,W3,W4
    integer, intent(in), dimension(nims:nime,njms:njme) :: II,JJ
    real, intent(in), dimension(nkms:nkme) :: eta1,eta2,deta1,deta2
    real, intent(in) :: ptop,pdtop
    integer, intent(in) :: kpres

    ! Parent fields:
    real, intent(in), dimension(cims:cime,cjms:cjme,ckms:ckme) :: cT,cQ,cPINT
    real, intent(in), dimension(cims:cime,cjms:cjme,1) :: cPD,cFIS

    ! Nest terrain info:
    real, intent(in), dimension(nims:nime,njms:njme,1) :: nFIS

    ! T, Q, PINT, PD boundary info:
    real,dimension(nims:nime,nkms:nkme,bdyw) :: tbys,tbye,qbys,qbye
    real,dimension(njms:njme,nkms:nkme,bdyw) :: tbxs,tbxe,qbxs,qbxe
    real,dimension(nims:nime,1,bdyw) :: pdbys,pdbye
    real,dimension(njms:njme,1,bdyw) :: pdbxs,pdbxe

    ! Weights and indices:
    real,dimension(nims:nime,nkms:nkme,bdyw) :: wbys,wbye
    real,dimension(njms:njme,nkms:nkme,bdyw) :: wbxs,wbxe
    integer,dimension(nims:nime,nkms:nkme,bdyw) :: ibys,ibye
    integer,dimension(njms:njme,nkms:nkme,bdyw) :: ibxs,ibxe

    integer :: i,j,k,a,b,nx,nz,used,j1,j2,used1

    real, dimension(1,2*(nite-nits+5)+2*(njte-njts+5)) :: inFIS,inPD,icFIS,icPD
    real, dimension(kpres+1,2*(nite-nits+5)+2*(njte-njts+5)) :: inT,inQ,icT,icQ,winfo
    integer, dimension(kpres+1,2*(nite-nits+5)+2*(njte-njts+5)) :: iinfo
    real, dimension(kpres+2,2*(nite-nits+5)+2*(njte-njts+5)) :: inPINT,icPINT
    logical :: warned

    warned=.false. ! Allow one P>PSTD message

    nx=min(nide-1,nite)-max(nids,nits)+1
    nz=nkde-nkds+1

    j1=max(njts-1,njds)
    if(mod(j1,2)/=1)   j1=j1+1

    j2=min(njte+1,njde-1)
    if(mod(j2,2)/=1)   j2=j2-1

    used=0
    bdyloop: do b=1,4
       if_xbdy: if(b==1 .or. b==2) then
          if(b==1) then
             if(nits/=1) cycle bdyloop
             i=1
          endif
          if(b==2) then
             if(nite<nide-1) cycle bdyloop
             i=nide-1
          endif
          do j=j1,j2,2
             used=used+1
             do k=nkts,kpres+1
                a=1-mod(JJ(i,j),2)
                uFCOPY(icT,cT,i,j,k)
                uFCOPY(icQ,cQ,i,j,k)
                uFCOPY(icPINT,cPINT,i,j,k)
             enddo
                
             k=kpres+2
             a=1-mod(JJ(i,j),2)

             uFCOPY(icPINT,cPINT,i,j,k)
             uFCOPY(icPD,cPD,i,j,1)
             uFCOPY(icFIS,cFIS,i,j,1)

             inFIS(1,used)=nFIS(i,j,1)

          enddo
       endif if_xbdy
       if_ybdy: if(b==3 .or. b==4) then
          if(b==3) then
             if(njts/=1) cycle bdyloop
             j=1
          endif
          if(b==4) then
             if(njte<njde-1) cycle bdyloop
             j=njde-1
          endif
          do i=max(nits-1,nids),min(nite+1,nide-1)
             used=used+1
             do k=nkts,kpres+1
                a=1-mod(JJ(i,j),2)
                uFCOPY(icT,cT,i,j,k)
                uFCOPY(icQ,cQ,i,j,k)
                uFCOPY(icPINT,cPINT,i,j,k)
             enddo
                
             k=kpres+2
             a=1-mod(JJ(i,j),2)

             uFCOPY(icPINT,cPINT,i,j,k)
             uFCOPY(icPD,cPD,i,j,1)
             uFCOPY(icFIS,cFIS,i,j,1)

             inFIS(1,used)=nFIS(i,j,1)
          enddo
       endif if_ybdy
    enddo bdyloop

    if(used==0) then
       ! No boundary points on this tile so we are done
       return
    endif

    call interp_T_PD_Q_kpres(nmm_method_linear, nmm_interp_pd, used,nz, &
         deta1,deta2,eta1,eta2,ptop,pdtop, kpres, kpres+2, &
         icFIS,inFIS, icPINT,inPINT, icT, inT, icPD,inPD, icQ,inQ, &
         iinfo, winfo, warned)

    used1=used
    used=0

    if_bxs: if(nits==1) then
       i=1
       do j=j1,j2,2
          used=used+1
          do k=nkts,kpres
             tbxs(j,k,1)=inT(k,used)
             qbxs(j,k,1)=inQ(k,used)
             ibxs(j,k,1)=iinfo(k,used)
             wbxs(j,k,1)=winfo(k,used)
          enddo
          ibxs(j,nkde,1)=nkde
          wbxs(j,nkde,1)=1.0
          pdbxs(j,1,1)=inPD(1,used)
       enddo
       do k=kpres+1,nkde-1
          do j=j1,j2,2
             a=1-mod(JJ(i,j),2)
             tbxs(j,k,1)=ICOPY(cT,i,j,k)
             qbxs(j,k,1)=ICOPY(cQ,i,j,k)
             ibxs(j,k,1)=k
             wbxs(j,k,1)=1.0
          enddo
       enddo
    endif if_bxs

    if_bxe: if(nite>=nide-1) then
       i=nide-1
       do j=j1,j2,2
          used=used+1
          do k=nkts,kpres
             tbxe(j,k,1)=inT(k,used)
             qbxe(j,k,1)=inQ(k,used)
             ibxe(j,k,1)=iinfo(k,used)
             wbxe(j,k,1)=winfo(k,used)
          enddo
          ibxe(j,nkde,1)=nkde
          wbxe(j,nkde,1)=1.0
          pdbxe(j,1,1)=inPD(1,used)
       enddo
       do k=kpres+1,nkde-1
          do j=j1,j2,2
             a=1-mod(JJ(i,j),2)
             tbxe(j,k,1)=ICOPY(cT,i,j,k)
             qbxe(j,k,1)=ICOPY(cQ,i,j,k)
             ibxe(j,k,1)=k
             wbxe(j,k,1)=1.0
          enddo
       enddo
    endif if_bxe

    if_bys: if(njts==1) then
       j=1
       do i=max(nits-1,nids),min(nite+1,nide-1)
          used=used+1
          do k=nkts,kpres
             tbys(i,k,1)=inT(k,used)
             qbys(i,k,1)=inQ(k,used)
             ibys(i,k,1)=iinfo(k,used)
             wbys(i,k,1)=winfo(k,used)
          enddo
          ibys(i,nkde,1)=nkde
          wbys(i,nkde,1)=1.0
          pdbys(i,1,1)=inPD(1,used)
       enddo
       do k=kpres+1,nkde-1
          do i=max(nits-1,nids),min(nite+1,nide-1)
             a=1-mod(JJ(i,j),2)
             tbys(i,k,1)=ICOPY(cT,i,j,k)
             qbys(i,k,1)=ICOPY(cQ,i,j,k)
             ibys(i,k,1)=k
             wbys(i,k,1)=1.0
          enddo
       enddo
    endif if_bys

    if_bye: if(njte>=njde-1) then
       j=njde-1
       do i=max(nits-1,nids),min(nite+1,nide-1)
          used=used+1
          do k=nkts,kpres
             tbye(i,k,1)=inT(k,used)
             qbye(i,k,1)=inQ(k,used)
             ibye(i,k,1)=iinfo(k,used)
             wbye(i,k,1)=winfo(k,used)
          enddo
          ibye(i,nkde,1)=nkde
          wbye(i,nkde,1)=1.0
          pdbye(i,1,1)=inPD(1,used)
       enddo
       do k=kpres+1,nkde-1
          do i=max(nits-1,nids),min(nite+1,nide-1)
             a=1-mod(JJ(i,j),2)
             tbye(i,k,1)=ICOPY(cT,i,j,k)
             qbye(i,k,1)=ICOPY(cQ,i,j,k)
             ibye(i,k,1)=k
             wbye(i,k,1)=1.0
          enddo
       enddo
    endif if_bye

    if(used/=used1) then
       call wrf_error_fatal('Number of input and output points does not match.')
    endif

  end subroutine c2b_fulldom_new

  ! ####################################################################

  subroutine c2n_fulldom  (II,JJ,W1,W2,W3,W4,&
       deta1,deta2, eta1,eta2, ptop,pdtop,   &
       cfis,cpint,ct,cpd,cq,                 &
       cims, cime, cjms, cjme, ckms, ckme,   &
       nfis,npint,nt,npd,nq,                 &
       out_iinfo,out_winfo,imask,&
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte, kpres)
    implicit none
    integer, intent(in):: cims, cime, cjms, cjme, ckms, ckme,   &
         nids, nide, njds, njde, nkds, nkde,   &
         nims, nime, njms, njme, nkms, nkme,   &
         nits, nite, njts, njte, nkts, nkte, kpres
    real, intent(in), dimension(cims:cime,cjms:cjme,ckms:ckme) :: cT,cQ,cPINT
    real, intent(in), dimension(cims:cime,cjms:cjme,1) :: cPD,cFIS

    real, intent(out), dimension(nims:nime,njms:njme,nkms:nkme) :: out_winfo
    integer, intent(out), dimension(nims:nime,njms:njme,nkms:nkme) :: out_iinfo

    real, intent(out), dimension(nims:nime,njms:njme,nkms:nkme) :: nT,nQ
    real, intent(in), dimension(nims:nime,njms:njme,nkms:nkme) :: nPINT
    real, intent(in), dimension(nims:nime,njms:njme,1) :: nFIS
    real, intent(inout), dimension(nims:nime,njms:njme,1) :: nPD
    integer, intent(in), dimension(nims:nime,njms:njme) :: imask

    real, intent(in), dimension(nims:nime,njms:njme) :: &
         W1,W2,W3,W4
    integer, intent(in), dimension(nims:nime,njms:njme) :: II,JJ

    real, intent(in), dimension(nkms:nkme) :: eta1,eta2,deta1,deta2
    real, intent(in) :: ptop,pdtop

    real, dimension(1,nite-nits+1) :: inFIS,inPD,icFIS,icPD
    real, dimension(nkde-1,nite-nits+1) :: inT,inQ,icT,icQ,winfo
    integer, dimension(nkde-1,nite-nits+1) :: iinfo
    real, dimension(nkde,nite-nits+1) :: inPINT,icPINT

    real :: pdcheck
    integer :: i,j,k,a, nx,nz,used
    logical :: badbad, warned

    warned=.false. ! Allow one P>PSTD message

    nx=min(nide-1,nite)-max(nids,nits)+1
    nz=nkde-nkds+1

    bigj: do j=max(njds,njts),min(njde-1,njte)
       ! STEP 1: Copy coarse and fine nest data into 
       ! temporary arrays, reordering dimensions:
       used=0
       iloop1: do i=max(nids,nits),min(nide-1,nite)
          if(imask(i,j)/=0) cycle iloop1
          used=used+1

          qtloop: do k=nkts,nkte-1
             a=1-mod(JJ(i,j),2)
             uFCOPY(icT,cT,i,j,k)
             uFCOPY(icQ,cQ,i,j,k)
             uFCOPY(icPINT,cPINT,i,j,k)
          enddo qtloop

          k=nkte
          a=1-mod(JJ(i,j),2)

          uFCOPY(icPINT,cPINT,i,j,k)
          uFCOPY(icPD,cPD,i,j,1)
          uFCOPY(icFIS,cFIS,i,j,1)

!           nPD(i,j,1)=use_this_pd(i,j,1) ! FIXME: remove this line
 !           if(nPD(i,j,1)> 1.3e5 .or. nPD(i,j,1)<2e4) then
 ! 3131         format('Invalid input nest PD in C2N interpolation: PD(',I0,',',I0,') = ',F0.7)
 !              write(0,3131) i,j,nPD(i,j,1)
 !              call wrf_error_fatal('Invalid input nest PD')
 !           endif
          inPD(1,used)=nPD(i,j,1)
          inFIS(1,used)=nFIS(i,j,1)
       enddo iloop1

       ! Step 2: Interpolate coarse grid to fine grid in reordered
       ! arrays:
       
       if(used==0) then
          ! No points in this row require interpolation, so we're done
          ! with this row:
          cycle bigj
       else
          call interp_T_PD_Q(nmm_method_linear, nmm_interp_pd, used,nz, &
               deta1,deta2,eta1,eta2,ptop,pdtop,kpres, &
               icFIS,inFIS, icPINT,inPINT, icT, inT, icPD,inPD, icQ,inQ, &
               iinfo, winfo, warned)
       endif

       ! Step 3: Copy back from reordered arrays to final nest arrays:
       used=0
       iloop2: do i=max(nids,nits),min(nide-1,nite)
          if(imask(i,j)/=0) cycle iloop2
          used=used+1

          qtloop2: do k=nkts,nkte-1
             nT(i,j,k)=inT(k,used)
             nQ(i,j,k)=inQ(k,used)
             nQ(i,j,k)=inQ(k,used)
          enddo qtloop2

          izloop: do k=nkts,nkte-1
             out_iinfo(i,j,k)=iinfo(k,used)
             out_winfo(i,j,k)=winfo(k,used)
          enddo izloop
          out_iinfo(i,j,nkde)=nkde
          out_winfo(i,j,nkde)=1.0

          k=nkte+1
          a=1-mod(JJ(i,j),2)
          nPD(i,j,1)=inPD(1,used)
          pdcheck=npd(i,j,1)+ptop+pdtop
 !          if(pdcheck<50000.0 .or. pdcheck>105000.0) then
 ! 3131         format('Invalid output nest PD in C2N interpolation: PD(',I0,',',I0,') = ',F0.7,' which is ',F0.7,' Pa')
 !              write(0,3131) i,j,nPD(i,j,1),pdcheck
 !              call wrf_error_fatal('Invalid output nest PD')
 !          endif
       enddo iloop2
    enddo bigj

  end subroutine c2n_fulldom

  ! ********************************************************************
  ! INTERP_T_PD_Q -- this is the actual vertical interpolation
  ! subroutine, the implementation of the *_fulldom subroutines.  It
  ! takes arrays of atmospheric columns, with no knowledge of the
  ! horizontal layout of those columns.  This routine will recalculate
  ! PD if requested, then generate interpolation information and
  ! interpolate T and Q, within each atmospheric column.  
  !
  ! Other variables are handled by the various other c2n, n2c and c2b
  ! routines in this module.  PD, T and Q are handled specially since
  ! they are critical to the stability of the hydrostatic solver, and
  ! hence have more expensive extrapolation and mass rebalancing
  ! methods.
  !
  ! Since this routine has no knowledge of the horizontal layout of
  ! the atmospheric columns it is given, the columns do not have to
  ! lie on an I- or J-row.  The boundary interpolation routines take
  ! advantage of that and provide all boundary information to this
  ! routine in a single call.  Of course, the caller must handle all
  ! horizontal interpolation.
  ! ********************************************************************

  subroutine interp_T_PD_Q(method, pd_interp, nx, nz, &
       deta1,deta2, eta1,eta2, ptop,pdtop, kpres,     &
       fisA,fisB, pintA,pintB, tA,tB, pdA,pdB, qA,qB, &
       iinfo, winfo, warned)
    implicit none

    integer, intent(in) :: pd_interp,method, kpres
    !      real, intent(in) :: dtA,dtB

    ! Coordinate system definitions must be the same for all domains:
    real, intent(in) :: deta1(nz),deta2(nz),eta1(nz),eta2(nz),ptop,pdtop
    integer, intent(in) :: nx,nz

    ! Surface height and mass field information for source (A):
    real, intent(in) :: fisA(nx),tA(nz-1,nx),pdA(nx),qA(nz-1,nx),pintA(nz,nx)

    ! Surface height and mass field information for target (B):
    real, intent(inout) :: fisB(nx),tB(nz-1,nx),pdB(nx),qB(nz-1,nx),pintB(nz,nx)

    ! Interpolation or extrapolation information for use in other
    ! calls later on:
    real, intent(out) :: winfo(nz-1,nx)
    integer, intent(out) :: iinfo(nz-1,nx)
    logical, intent(inout) :: warned

    ! ==================== Local variables ====================

    character*255 :: message
    integer :: ix,iz,izA,izB,xpr
    real :: zA,zB,znext,apelp,rtopp,dz,weight,A,B,pstd1,z,pstd2,pstd12
    real :: tsfc(nx), slp(nx), zmslp(nx), z0mid(nx), zbelow(nx), &
            tbelow(nx), RHbelow(nx)
    real :: pb,pb1,pb2,pa,pa1,pa2,pnext,pa3, wnum,wdenom, QC, P0

    ! Constants from UPP params.f for RH calculation (all mks):
    real, parameter :: PQ0=379.90516
    real, parameter :: A2=17.2693882
    real, parameter :: A3=273.16
    real, parameter :: A4=35.86
    real, parameter :: RHmin=1.0E-6     ! minimal RH bound

    if(method/=nmm_method_linear) then
       call wrf_error_fatal('only linear interpolation is supported')
    endif

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Step 1: calculate near-surface values !!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    pstd1=p_ref ! pstd(1) from base_state_parent
    ! pstd(2) from base_state_parent:
    pstd2=eta1(2)*pdtop + eta2(2)*(p_ref-pdtop-ptop) + ptop
    pstd12=exp((alog(pstd1)+alog(pstd2))*0.5)

    do ix=1,nx
       ! These calculations are from base_state_parent:
       APELP    = (pintA(2,ix)+pintA(1,ix))
       RTOPP    = TRG*tA(1,ix)*(1.0+qA(1,ix)*P608)/APELP
       DZ       = RTOPP*(DETA1(1)*PDTOP+DETA2(1)*pdA(ix))

       Z0MID(ix) = fisA(ix)/g + dz/2

       zA=fisA(ix)/g

       TSFC(ix) = TA(1,ix)*(1.+D608*QA(1,ix)) &
            + LAPSR*(zA + zA+dz)*0.5

       A         = LAPSR*zA/TSFC(ix)
       SLP(ix)  = PINTA(1,ix)*(1-A)**COEF2    ! sea level pressure 
       B         = (pstd1/SLP(ix))**COEF3
       ZMSLP(ix)= TSFC(ix)*LAPSI*(1.0 - B)   ! Height at 1030. mb level

       TBELOW(ix) = TA(1,ix) + LAPSR*(Z0MID(ix)-ZMSLP(ix))
       
       ! Calculate lowest level RH.  This calculation is from UPP
       ! CALRH.f:
       P0=pdA(ix)+ptop+pdtop ! Use hydrostatic lowest model level P
       QC=PQ0/P0*EXP(A2*(TA(1,ix)-A3)/(TA(1,ix)-A4))
       RHbelow(ix)=max(RHmin,min(1.0,QA(1,ix)/QC))
    enddo



    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Step 2: figure out the new surface pressures !!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



    if_pd_interp: if(pd_interp==nmm_keep_pd) then
       ! PD interpolation is turned off, so we use the original PD.
    elseif(pd_interp==nmm_interp_pd) then
       ! PD interpolation is requested.  As with the old base_state_parent,
       ! determine PD by interpolating or extrapolating the non-hydrostatic
       ! pressure field (PINT) in source grid to the surface height of the
       ! target grid.  
       xloop: do ix=1,nx
          if(pintA(1,ix)>p_ref) then
             ! Cannot extrapolate PD below ground because pressure is
             ! unrealistically high.  Follow base_state_parent method:
             ! when this happens, assign input pressure to output 
             ! pressure.
             if(.not.warned) then
                call wrf_message2('WARNING: NESTED DOMAIN PRESSURE AT LOWEST LEVEL HIGHER THAN PSTD')
                write(message,*) 'PINT(1),PD(1),PSTD(1)',pintA(1,ix),pdA(ix),p_ref
                call wrf_message2(message)
                warned=.true.
             endif
             pdB(ix)=pdA(ix)
             cycle xloop
          endif

          zA=fisA(ix)/g
          zB=fisB(ix)/g

          if(zB<zA) then
             ! Need to extrapolate PD below ground.
             ! Follow base_state_parent method: 
             !   For pressures between pint(1) and 1030 mbar, interpolate
             iz=1
             weight=abs((zB-zmslp(ix))/(zA-zmslp(ix)))
             pdB(ix) = exp(weight*log(pdA(ix)+pdtop+ptop) + &
                        (1.0-weight)*log(P_REF)) &
                        - pdtop - ptop
!             write(0,*) 'WARNING: extrapolating below ground at ix=',ix
!1302         format('Extrap at ix=',I0,' zA=',F0.3,' zB=',F0.3,' zmslp=',F0.3,' weight=',F0.3,' pdB+pdtop+ptop=',F0.3,' pintA=',F0.3,' pdA(ix)+pdtop+ptop=',F0.3,' P_REF=',F0.3)
!             write(0,1302) ix,zA,zB,zmslp(ix),weight,pdB(ix)+pdtop+ptop,pintA(1,ix),pdA(ix)+pdtop+ptop,P_REF
             cycle xloop
          endif

          ! PD in grid B lies above ground in grid A.  Find out where by
          ! walking up from the ground until we find the two levels the
          ! pressure lies between.
          znext=-9e9
          z=zA
          do iz=1,nz-1
             ! Calculations from base_state_parent:
             APELP    = (pintA(iz+1,ix)+pintA(iz,ix))
             RTOPP    = TRG*tA(iz,ix)*(1.0+qA(iz,ix)*P608)/APELP
             DZ       = RTOPP*(DETA1(iz)*PDTOP+DETA2(iz)*pdA(ix))
             znext=z+dz

             if(znext>zB) then
                ! Interpolate between these two levels
                weight=(zB-z)/dz
                pdB(ix) = exp(weight*log(pintA(iz+1,ix)) + &
                              (1.0-weight)*log(pintA(iz,ix))) &
                      - pdtop - ptop
!                if(iz>1) then
!                   write(0,*) 'WARNING: interpolate above ground at ix=',ix
!1303               format('Interp at ix=',I0,' with iz=',I0,' zA=',F0.3,' zB=',F0.3,' zmslp=',F0.3,' weight=',F0.3,' pdB+pdtop+ptop=',F0.3,' pintA=',F0.3,' pdA(ix)+pdtop+ptop=',F0.3,' P_REF=',F0.3)
!                   write(0,1303) ix,iz,zA,zB,zmslp(ix),weight,pdB(ix)+pdtop+ptop,pintA(1,ix),pdA(ix)+pdtop+ptop,P_REF
!                endif
                cycle xloop
             else
                z=znext
             endif

          enddo

201       format('interp_T_PD_Q: Target domain surface height ',F0.4,'m is higher than the model top ',F0.4,'m of the source domain.  ABORTING')
          write(message,201) zB,znext
          call wrf_error_fatal(message)
       enddo xloop
    else
202    format('Invalid value ',I0,' for pd_interp in interp_T_PD_Q')
       write(message,202) pd_interp
       call wrf_error_fatal(message)
    endif if_pd_interp



    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Step 3: interpolate T and Q in pressure space !!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



    outer: do ix=1,nx
       ! For constant pressure levels, do a straight level-by-level copy:
       iz=nz-1
       copyloop: do while(iz>kpres)
          tB(iz,ix)=tA(iz,ix)
          qB(iz,ix)=qA(iz,ix)

          ! Save interpolation information
          iinfo(iz,ix)=iz
          winfo(iz,ix)=1.0

          iz=iz-1
       enddo copyloop

       ! For sigma levels where target domain lies within source,
       ! interpolate from top down, stopping when we go below ground
       ! in the source domain.
       izA=iz
       izB=iz

       ! FIXME: REMOVE THIS CHECK
       if(iz>nz) then
          ! Make sure the entire vertical extent isn't sigma levels:
          call wrf_error_fatal('ERROR: WRF-NMM does not support pure sigma levels (only sigma-pressure hybrid)')
       endif

       pB2=log(eta1(izB)*pdtop + eta2(izB)*pdB(ix) + ptop)
       pB1=log(eta1(izB+1)*pdtop + eta2(izB+1)*pdB(ix) + ptop)
       pB=(pB2+pB1)*0.5

       pA2=log(eta1(izA)*pdtop + eta2(izA)*pdA(ix) + ptop)
       pA1=log(eta1(izA+1)*pdtop + eta2(izA+1)*pdA(ix) + ptop)
       pA=(pA2+pA1)*0.5

       ! Find the lowest mass level izA that is above pB
       interpinit: do while(izA>1)
          pA3=log(eta1(izA-1)*pdtop + eta2(izA-1)*pdA(ix) + ptop)
          pnext=(pA2+pA3)*0.5
          wdenom=pnext-pA
          wnum=pnext-pb
          if(pA<=pB .and. wnum>1e-5) then
             exit interpinit
          else
             pA1=pA2
             pA2=pa3
             izA=izA-1
             pA=pnext
          endif
       enddo interpinit

       ! Loop over all remaining B points, interpolating to them.
       interploop: do while(izB>0 .and. izA>1)
          ! Find the source domain levels that this target domain level lies between:
          zinterp: do while(izA>1)
             if(pnext>=pB) then
                ! We found the two levels, so interpolate and move on to
                ! the next target level:
                weight=max(0.,wnum/wdenom)
                tB(izB,ix)=weight*tA(izA,ix) + (1.0-weight)*tA(izA-1,ix)
                qB(izB,ix)=weight*qA(izA,ix) + (1.0-weight)*qA(izA-1,ix)

                ! Save interpolation info
                iinfo(izB,ix)=izA    ! upper level
                winfo(izB,ix)=weight ! linear interpolation weight

                ! We interpolated to a B point, so go to the next one:
                pB1=pB2
                izB=izB-1
                if(izB>0) then
                   pB2=log(eta1(izB)*pdtop + eta2(izB)*pdB(ix) + ptop)
                   pB=(pb1+pb2)*0.5
                else
                   exit interploop
                endif
                wnum=pnext-pb
                
                exit zinterp
             else
                izA=izA-1
                pA1=pA2
                pA2=pa3
                pA=pnext
                if(izA>1) then
                   pA3=log(eta1(izA-1)*pdtop + eta2(izA-1)*pdA(ix) + ptop)
                   pnext=(pA2+pA3)*0.5
                   wdenom=pnext-pa
                   wnum=pnext-pb
                else
                   exit interploop
                endif
             endif
          enddo zinterp
       enddo interploop

       ! Follow what base_state_parent did for temperature:
       ! interpolate between the second to last level and P_REF using
       ! a lapse rate atmosphere.  For Q, we use constant RH below
       ! ground, as suggested by Greg Thompson.
       extraploop: do while(izB>=1)
          ! Decide extrapolation weight:
          weight=(pB-pA)/(pstd12-pA)

          ! Extrapolate Q by copying lowest sigma layer value:
          !qB(izB,ix)=qA(1,ix)

          ! Extrapolate Q by linearly interpolating between 0 and surface value:
          !qB(izB,ix)=(1.0-weight)*qA(1,ix)

          ! Extrapolate T using a lapse rate atmosphere
          tB(izB,ix)=weight*tbelow(ix) + (1.0-weight)*tA(1,ix)

          ! Extrapolate Q using constant RH below ground, as suggested
          ! by Greg Thompson.  This is the inversion of the RH
          ! calculation used earlier in this function:
          P0=eta1(izB)*pdtop + eta2(izB)*pdB(ix) + ptop
          QC=PQ0/P0*EXP(A2*(TB(izB,ix)-A3)/(TB(izB,ix)-A4))
          QB(izB,ix)=QC*RHbelow(ix)

          ! Save extrapolation information
          iinfo(izB,ix)=0
          winfo(izB,ix)=weight

          ! Move to the next B level
          izB=izB-1
          if(izB>0) then
             pB1=pB2
             pB2=log(eta1(izB)*pdtop + eta2(izB)*pdB(ix) + ptop)
             pB=(pb1+pb2)*0.5
          endif
       enddo extraploop
    enddo outer
  end subroutine interp_T_PD_Q

  subroutine interp_T_PD_Q_kpres(method, pd_interp, nx, nz, &
       deta1,deta2, eta1,eta2, ptop,pdtop, kpres, nz2,     &
       fisA,fisB, pintA,pintB, tA,tB, pdA,pdB, qA,qB, &
       iinfo, winfo, warned)
    implicit none

    integer, intent(in) :: pd_interp,method, kpres, nz2
    !      real, intent(in) :: dtA,dtB

    ! Coordinate system definitions must be the same for all domains:
    real, intent(in) :: deta1(nz),deta2(nz),eta1(nz),eta2(nz),ptop,pdtop
    integer, intent(in) :: nx,nz

    ! Surface height and mass field information for source (A):
    real, intent(in) :: fisA(nx),tA(nz2-1,nx),pdA(nx),qA(nz2-1,nx),pintA(nz2,nx)

    ! Surface height and mass field information for target (B):
    real, intent(inout) :: fisB(nx),tB(nz2-1,nx),pdB(nx),qB(nz2-1,nx),pintB(nz2,nx)

    ! Interpolation or extrapolation information for use in other
    ! calls later on:
    real, intent(out) :: winfo(nz2-1,nx)
    integer, intent(out) :: iinfo(nz2-1,nx)

    logical, intent(inout) :: warned

    ! ==================== Local variables ====================

    character*255 :: message
    integer :: ix,iz,izA,izB,xpr
    real :: zA,zB,znext,apelp,rtopp,dz,weight,A,B,pstd1,z,pstd2,pstd12
    real :: tsfc(nx), slp(nx), zmslp(nx), z0mid(nx), zbelow(nx), &
            tbelow(nx), RHbelow(nx)
    real :: pb,pb1,pb2,pa,pa1,pa2,pnext,pa3, wnum,wdenom, QC, P0

    ! Constants from UPP params.f for RH calculation (all mks):
    real, parameter :: PQ0=379.90516
    real, parameter :: A2=17.2693882
    real, parameter :: A3=273.16
    real, parameter :: A4=35.86
    real, parameter :: RHmin=1.0E-6     ! minimal RH bound

    if(method/=nmm_method_linear) then
       call wrf_error_fatal('only linear interpolation is supported')
    endif

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Step 1: calculate near-surface values !!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



    pstd1=p_ref ! pstd(1) from base_state_parent
    ! pstd(2) from base_state_parent:
    pstd2=eta1(2)*pdtop + eta2(2)*(p_ref-pdtop-ptop) + ptop
    pstd12=exp((alog(pstd1)+alog(pstd2))*0.5)

    do ix=1,nx
       ! These calculations are from base_state_parent:
       APELP    = (pintA(2,ix)+pintA(1,ix))
       RTOPP    = TRG*tA(1,ix)*(1.0+qA(1,ix)*P608)/APELP
       DZ       = RTOPP*(DETA1(1)*PDTOP+DETA2(1)*pdA(ix))

       Z0MID(ix) = fisA(ix)/g + dz/2

       zA=fisA(ix)/g

       TSFC(ix) = TA(1,ix)*(1.+D608*QA(1,ix)) &
            + LAPSR*(zA + zA+dz)*0.5

       A         = LAPSR*zA/TSFC(ix)
       SLP(ix)  = PINTA(1,ix)*(1-A)**COEF2    ! sea level pressure 
       B         = (pstd1/SLP(ix))**COEF3
       ZMSLP(ix)= TSFC(ix)*LAPSI*(1.0 - B)   ! Height at 1030. mb level

       TBELOW(ix) = TA(1,ix) + LAPSR*(Z0MID(ix)-ZMSLP(ix))
       
       ! Calculate lowest level RH.  This calculation is from UPP
       ! CALRH.f:
       P0=pdA(ix)+ptop+pdtop ! Use hydrostatic lowest model level P
       QC=PQ0/P0*EXP(A2*(TA(1,ix)-A3)/(TA(1,ix)-A4))
       RHbelow(ix)=max(RHmin,min(1.0,QA(1,ix)/QC))
    enddo



    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Step 2: figure out the new surface pressures !!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



    if_pd_interp: if(pd_interp==nmm_keep_pd) then
       ! PD interpolation is turned off, so we use the original PD.
    elseif(pd_interp==nmm_interp_pd) then
       ! PD interpolation is requested.  As with the old base_state_parent,
       ! determine PD by interpolating or extrapolating the non-hydrostatic
       ! pressure field (PINT) in source grid to the surface height of the
       ! target grid.  
       xloop: do ix=1,nx
          if(pintA(1,ix)>p_ref) then
             ! Cannot extrapolate PD below ground because pressure is
             ! unrealistically high.  Follow base_state_parent method:
             ! when this happens, assign input pressure to output 
             ! pressure.
             if(.not.warned) then
                call wrf_message2('WARNING: NESTED DOMAIN PRESSURE AT LOWEST LEVEL HIGHER THAN PSTD')
                WRITE(message,*) 'PINT(1),PD(1),PSTD(1)',pintA(1,ix),pdA(ix),p_ref
                call wrf_message2(message)
                warned=.true.
             endif
             pdB(ix)=pdA(ix)
             cycle xloop
          endif

          zA=fisA(ix)/g
          zB=fisB(ix)/g

          if(zB<zA) then
             ! Need to extrapolate PD below ground.
             ! Follow base_state_parent method: 
             !   For pressures between pint(1) and 1030 mbar, interpolate
             iz=1
             weight=abs((zB-zmslp(ix))/(zA-zmslp(ix)))
             pdB(ix) = exp(weight*log(pdA(ix)+pdtop+ptop) + &
                        (1.0-weight)*log(P_REF)) &
                        - pdtop - ptop
!             write(0,*) 'WARNING: extrapolating below ground at ix=',ix
!1302         format('Extrap at ix=',I0,' zA=',F0.3,' zB=',F0.3,' zmslp=',F0.3,' weight=',F0.3,' pdB+pdtop+ptop=',F0.3,' pintA=',F0.3,' pdA(ix)+pdtop+ptop=',F0.3,' P_REF=',F0.3)
!             write(0,1302) ix,zA,zB,zmslp(ix),weight,pdB(ix)+pdtop+ptop,pintA(1,ix),pdA(ix)+pdtop+ptop,P_REF
             cycle xloop
          endif

          ! PD in grid B lies above ground in grid A.  Find out where by
          ! walking up from the ground until we find the two levels the
          ! pressure lies between.
          znext=-9e9
          z=zA
          do iz=1,nz2-1
             ! Calculations from base_state_parent:
             APELP    = (pintA(iz+1,ix)+pintA(iz,ix))
             RTOPP    = TRG*tA(iz,ix)*(1.0+qA(iz,ix)*P608)/APELP
             DZ       = RTOPP*(DETA1(iz)*PDTOP+DETA2(iz)*pdA(ix))
             znext=z+dz

             if(znext>zB) then
                ! Interpolate between these two levels
                weight=(zB-z)/dz
                pdB(ix) = exp(weight*log(pintA(iz+1,ix)) + &
                              (1.0-weight)*log(pintA(iz,ix))) &
                      - pdtop - ptop
!                if(iz>1) then
!                   write(0,*) 'WARNING: interpolate above ground at ix=',ix
!1303               format('Interp at ix=',I0,' with iz=',I0,' zA=',F0.3,' zB=',F0.3,' zmslp=',F0.3,' weight=',F0.3,' pdB+pdtop+ptop=',F0.3,' pintA=',F0.3,' pdA(ix)+pdtop+ptop=',F0.3,' P_REF=',F0.3)
!                   write(0,1303) ix,iz,zA,zB,zmslp(ix),weight,pdB(ix)+pdtop+ptop,pintA(1,ix),pdA(ix)+pdtop+ptop,P_REF
!                endif
                cycle xloop
             else
                z=znext
             endif

          enddo

201       format('interp_T_PD_Q_kpres: Target domain surface height ',F0.4,'m is higher than the model top ',F0.4,'m of the source domain.  ABORTING')
          write(message,201) zB,znext
          call wrf_error_fatal(message)
       enddo xloop
    else
202    format('Invalid value ',I0,' for pd_interp in interp_T_PD_Q')
       write(message,202) pd_interp
       call wrf_error_fatal(message)
    endif if_pd_interp



    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Step 3: interpolate T and Q in pressure space !!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



    outer: do ix=1,nx
       ! For constant pressure levels, do a straight level-by-level copy:
       iz=nz2-1
       copyloop: do while(iz>kpres)
          tB(iz,ix)=tA(iz,ix)
          qB(iz,ix)=qA(iz,ix)

          ! Save interpolation information
          iinfo(iz,ix)=iz
          winfo(iz,ix)=1.0

          iz=iz-1
       enddo copyloop

       ! For sigma levels where target domain lies within source,
       ! interpolate from top down, stopping when we go below ground
       ! in the source domain.
       izA=iz
       izB=iz

       ! FIXME: REMOVE THIS CHECK
       if(iz>nz2) then
          ! Make sure the entire vertical extent isn't sigma levels:
          call wrf_error_fatal('ERROR: WRF-NMM does not support pure sigma levels (only sigma-pressure hybrid)')
       endif

       pB2=log(eta1(izB)*pdtop + eta2(izB)*pdB(ix) + ptop)
       pB1=log(eta1(izB+1)*pdtop + eta2(izB+1)*pdB(ix) + ptop)
       pB=(pB2+pB1)*0.5

       pA2=log(eta1(izA)*pdtop + eta2(izA)*pdA(ix) + ptop)
       pA1=log(eta1(izA+1)*pdtop + eta2(izA+1)*pdA(ix) + ptop)
       pA=(pA2+pA1)*0.5

       ! Find the lowest mass level izA that is above pB
       interpinit: do while(izA>1)
          pA3=log(eta1(izA-1)*pdtop + eta2(izA-1)*pdA(ix) + ptop)
          pnext=(pA2+pA3)*0.5
          wdenom=pnext-pA
          wnum=pnext-pb
          if(pA<=pB .and. wnum>1e-5) then
             exit interpinit
          else
             pA1=pA2
             pA2=pa3
             izA=izA-1
             pA=pnext
          endif
       enddo interpinit

       ! Loop over all remaining B points, interpolating to them.
       interploop: do while(izB>0 .and. izA>1)
          ! Find the source domain levels that this target domain level lies between:
          zinterp: do while(izA>1)
             if(pnext>=pB) then
                ! We found the two levels, so interpolate and move on to
                ! the next target level:
                weight=max(0.,wnum/wdenom)
                tB(izB,ix)=weight*tA(izA,ix) + (1.0-weight)*tA(izA-1,ix)
                qB(izB,ix)=weight*qA(izA,ix) + (1.0-weight)*qA(izA-1,ix)

                ! Save interpolation info
                iinfo(izB,ix)=izA    ! upper level
                winfo(izB,ix)=weight ! linear interpolation weight

                ! We interpolated to a B point, so go to the next one:
                pB1=pB2
                izB=izB-1
                if(izB>0) then
                   pB2=log(eta1(izB)*pdtop + eta2(izB)*pdB(ix) + ptop)
                   pB=(pb1+pb2)*0.5
                else
                   exit interploop
                endif
                wnum=pnext-pb
                
                exit zinterp
             else
                izA=izA-1
                pA1=pA2
                pA2=pa3
                pA=pnext
                if(izA>1) then
                   pA3=log(eta1(izA-1)*pdtop + eta2(izA-1)*pdA(ix) + ptop)
                   pnext=(pA2+pA3)*0.5
                   wdenom=pnext-pa
                   wnum=pnext-pb
                else
                   exit interploop
                endif
             endif
          enddo zinterp
       enddo interploop

       ! Follow what base_state_parent did for temperature:
       ! interpolate between the second to last level and P_REF using
       ! a lapse rate atmosphere.  For Q, we use constant RH below
       ! ground, as suggested by Greg Thompson.
       extraploop: do while(izB>=1)
          ! Decide extrapolation weight:
          weight=(pB-pA)/(pstd12-pA)

          ! Extrapolate Q by copying lowest sigma layer value:
          !qB(izB,ix)=qA(1,ix)

          ! Extrapolate Q by linearly interpolating between 0 and surface value:
          !qB(izB,ix)=(1.0-weight)*qA(1,ix)

          ! Extrapolate T using a lapse rate atmosphere
          tB(izB,ix)=weight*tbelow(ix) + (1.0-weight)*tA(1,ix)

          ! Extrapolate Q using constant RH below ground, as suggested
          ! by Greg Thompson.  This is the inversion of the RH
          ! calculation used earlier in this function:
          P0=eta1(izB)*pdtop + eta2(izB)*pdB(ix) + ptop
          QC=PQ0/P0*EXP(A2*(TB(izB,ix)-A3)/(TB(izB,ix)-A4))
          QB(izB,ix)=QC*RHbelow(ix)

          ! Save extrapolation information
          iinfo(izB,ix)=0
          winfo(izB,ix)=weight

          ! Move to the next B level
          izB=izB-1
          if(izB>0) then
             pB1=pB2
             pB2=log(eta1(izB)*pdtop + eta2(izB)*pdB(ix) + ptop)
             pB=(pb1+pb2)*0.5
          endif
       enddo extraploop
    enddo outer
  end subroutine interp_T_PD_Q_kpres
end module module_interp_nmm

! ********************************************************************
! subs EXT_*_FULLDOM -- simple wrappers around module_interp_nmm's
! *_FULLDOM routines.  These exist solely to allow module_dm to
! interface with this module -- this is needed because the WRF build
! scripts compile module_dm before module_interp_nmm.
! ********************************************************************

subroutine ext_c2n_fulldom  (II,JJ,W1,W2,W3,W4,&
       deta1,deta2, eta1,eta2, ptop,pdtop,   &
       cpint,ct,cpd,cq,                 &
       cims, cime, cjms, cjme, ckms, ckme,   &
       npint,nt,npd,nq,                 &
       out_iinfo,out_winfo,imask,&
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte)
  ! This subroutine is an alias for c2n_fulldom.  It exists to allow
  ! the routine to be called from module_dm
  use module_interp_store, only: parent_fis, nest_fis, kpres
  use module_interp_nmm, only: c2n_fulldom, find_kpres
  implicit none
  integer, intent(in):: cims, cime, cjms, cjme, ckms, ckme,   &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte
  real, intent(in), dimension(cims:cime,cjms:cjme,ckms:ckme) :: cT,cQ,cPINT
  real, intent(in), dimension(cims:cime,cjms:cjme,1) :: cPD

  real, intent(out), dimension(nims:nime,njms:njme,nkms:nkme) :: out_winfo
  integer, intent(out), dimension(nims:nime,njms:njme,nkms:nkme) :: out_iinfo

  real, intent(out), dimension(nims:nime,njms:njme,nkms:nkme) :: nT,nQ
  real, intent(in), dimension(nims:nime,njms:njme,nkms:nkme) :: nPINT
  real, intent(inout), dimension(nims:nime,njms:njme,1) :: nPD
  integer, intent(in), dimension(nims:nime,njms:njme) :: imask

  real, intent(in), dimension(nims:nime,njms:njme) :: &
       W1,W2,W3,W4
  integer, intent(in), dimension(nims:nime,njms:njme) :: II,JJ

  real, intent(in), dimension(nkms:nkme) :: eta1,eta2,deta1,deta2
  real, intent(in) :: ptop,pdtop
  integer :: i,j,k
  logical :: badbad

  call find_kpres(kpres,eta2,nkds,nkde,nkms,nkme)
  call c2n_fulldom(II,JJ,W1,W2,W3,W4,&
       deta1,deta2, eta1,eta2, ptop,pdtop,   &
       parent_fis,cpint,ct,cpd,cq,           &
       cims, cime, cjms, cjme, ckms, ckme,   &
       nest_fis,npint,nt,npd,nq,             &
       out_iinfo,out_winfo,imask,            &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte,   &
       kpres)

end subroutine ext_c2n_fulldom

subroutine ext_n2c_fulldom  ( &
       deta1,deta2, eta1,eta2, ptop,pdtop,   &
       cpint,ct,cpd,cq,                 &
       cids, cide, cjds, cjde, ckds, ckde,   &
       cims, cime, cjms, cjme, ckms, ckme,   &
       cits, cite, cjts, cjte, ckts, ckte,   &
       npint,nt,npd,nq,                 &
       ipos,jpos,                            &
       out_iinfo,out_winfo,      &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte)
  ! This subroutine is an alias for n2c_fulldom.  It exists to allow
  ! the routine to be called from module_dm
  use module_interp_store, only: parent_fis, nest_fis, kpres
  use module_interp_nmm, only: n2c_fulldom, find_kpres, n2c_fulldom_new
  implicit none
  integer, intent(in):: cims, cime, cjms, cjme, ckms, ckme,   &
       cids, cide, cjds, cjde, ckds, ckde,   &
       cits, cite, cjts, cjte, ckts, ckte,   &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte,   &
       ipos, jpos ! nest start loctions within parent
  real, intent(out), dimension(cims:cime,cjms:cjme,ckms:ckme) :: cT,cQ,cPINT
  real, intent(inout), dimension(cims:cime,cjms:cjme,1) :: cPD

  real, intent(out), dimension(cims:cime,cjms:cjme,ckms:ckme) :: out_winfo
  integer, intent(out), dimension(cims:cime,cjms:cjme,ckms:ckme) :: out_iinfo

  real, intent(in), dimension(nims:nime,njms:njme,nkms:nkme) :: nT,nQ
  real, intent(in), dimension(nims:nime,njms:njme,nkms:nkme) :: nPINT
  real, intent(in), dimension(nims:nime,njms:njme,1) :: nPD

  real, intent(in), dimension(nkms:nkme) :: eta1,eta2,deta1,deta2
  real, intent(in) :: ptop,pdtop

  call find_kpres(kpres,eta2,nkds,nkde,nkms,nkme)
  call n2c_fulldom_new( &
       deta1,deta2, eta1,eta2, ptop,pdtop,   &
       parent_fis,cpint,ct,cpd,cq,           &
       cids, cide, cjds, cjde, ckds, ckde,   &
       cims, cime, cjms, cjme, ckms, ckme,   &
       cits, cite, cjts, cjte, ckts, ckte,   &
       nest_fis,npint,nt,npd,nq,             &
       ipos,jpos,                            &
       out_iinfo,out_winfo,                  &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte, kpres)
end subroutine ext_n2c_fulldom

subroutine ext_c2b_fulldom  (II,JJ,W1,W2,W3,W4,&
     deta1,deta2, eta1,eta2, ptop,pdtop,   &
     cpint,ct,cpd,cq,                      &
     cims, cime, cjms, cjme, ckms, ckme,   &
     nids, nide, njds, njde, nkds, nkde,   &
     nims, nime, njms, njme, nkms, nkme,   &
     nits, nite, njts, njte, nkts, nkte,   &
     ibxs,    ibxe,    ibys,    ibye,      &
     wbxs,    wbxe,    wbys,    wbye,      &
     pdbxs,   pdbxe,   pdbys,   pdbye,     &
     tbxs,    tbxe,    tbys,    tbye,      &
     qbxs,    qbxe,    qbys,    qbye)
  use module_interp_nmm, only: c2b_fulldom, find_kpres, c2b_fulldom_new
  use module_interp_store, only: parent_fis, nest_fis, kpres
  implicit none
  integer, intent(in):: &
       cims, cime, cjms, cjme, ckms, ckme,   &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte
  integer, parameter :: bdyw = 1
  ! Domain info:
  real, intent(in), dimension(nims:nime,njms:njme) :: W1,W2,W3,W4
  integer, intent(in), dimension(nims:nime,njms:njme) :: II,JJ
  real, intent(in), dimension(nkms:nkme) :: eta1,eta2,deta1,deta2
  real, intent(in) :: ptop,pdtop

  ! Parent fields:
  real, intent(in), dimension(cims:cime,cjms:cjme,ckms:ckme) :: cT,cQ,cPINT
  real, intent(in), dimension(cims:cime,cjms:cjme,1) :: cPD

  ! T, Q, PINT, PD boundary info:
  real,dimension(nims:nime,nkms:nkme,bdyw) :: tbys,tbye,qbys,qbye
  real,dimension(njms:njme,nkms:nkme,bdyw) :: tbxs,tbxe,qbxs,qbxe
  real,dimension(nims:nime,1,bdyw) :: pdbys,pdbye
  real,dimension(njms:njme,1,bdyw) :: pdbxs,pdbxe
  
  ! Weights and indices:
  real,dimension(nims:nime,nkms:nkme,bdyw) :: wbys,wbye
  real,dimension(njms:njme,nkms:nkme,bdyw) :: wbxs,wbxe
  integer,dimension(nims:nime,nkms:nkme,bdyw) :: ibys,ibye
  integer,dimension(njms:njme,nkms:nkme,bdyw) :: ibxs,ibxe

  call find_kpres(kpres,eta2,nkds,nkde,nkms,nkme)
  call c2b_fulldom_new    (II,JJ,W1,W2,W3,W4,&
       deta1,deta2, eta1,eta2, ptop,pdtop,   &
       parent_fis,cpint,ct,cpd,cq,nest_fis,  &
       cims, cime, cjms, cjme, ckms, ckme,   &
       nids, nide, njds, njde, nkds, nkde,   &
       nims, nime, njms, njme, nkms, nkme,   &
       nits, nite, njts, njte, nkts, nkte,   &
       kpres,                                &
       ibxs,    ibxe,    ibys,    ibye,      &
       wbxs,    wbxe,    wbys,    wbye,      &
       pdbxs,   pdbxe,   pdbys,   pdbye,     &
       tbxs,    tbxe,    tbys,    tbye,      &
       qbxs,    qbxe,    qbys,    qbye)
end subroutine ext_c2b_fulldom
