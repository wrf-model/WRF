module module_SMOOTH_TERRAIN
#if (NMM_NEST == 1)
contains
  subroutine smooth_terrain(grid,lines,nsmud, &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       IPS,IPE,JPS,JPE,KPS,KPE)

    ! Parallelized smoothing routine for NMM domain terrain heights.
    ! Also supports serial setups.
    !
    ! Author: Sam Trahan, September 2011
    
    ! This is a replacement for, and based on, SMDHLD, which can be
    ! found lower down in this module.  This smooths boundaries of the
    ! grid%HRES_AVC.

    ! Two grid%variables are used: HRES_LND (land mask) and HRES_AVC.
    ! Those are initialized in NEST_TERRAIN and module_TERRAIN's
    ! terrain_for.  This routine is not sensitive to the units of
    ! HRES_AVC, so it could potentially be called on HRES_FIS instead.

    USE MODULE_DOMAIN, ONLY : DOMAIN, GET_IJK_FROM_GRID
#ifdef DM_PARALLEL
    USE MODULE_COMM_DM, ONLY: HALO_NMM_TERRAIN_SMOOTH_sub
    USE MODULE_DM, ONLY: ntasks_x, ntasks_y, mytask, ntasks, local_communicator
#endif

    implicit none

    INTEGER                            :: IDS,IDE,JDS,JDE,KDS,KDE
    INTEGER                            :: IMS,IME,JMS,JME,KMS,KME
    INTEGER                            :: IPS,IPE,JPS,JPE,KPS,KPE
    integer, intent(in) :: lines,nsmud
    character(len=256) :: message
    type(domain) :: grid
    integer :: i,j,k,jmelin,ibas,buf

    integer :: im,jm
    integer :: ihl,ihh,ks,m2l,imid,jmid,itgt,jtgt
    real :: hbms(ips:ipe,jps:jpe)

    integer :: ihw((jps-2):(jpe+2)),ihe((jps-2):(jpe+2))
    real :: hse((ips-1):(ipe+1),(jps-1):(jpe+1))
    real :: hne((ips-1):(ipe+1),(jps-1):(jpe+1))
    !-----------------------------------------------------------------------

    im=ide-1
    jm=jde-1
    imid=(ips+ipe)/2
    jmid=(jps+jpe)/2

    itgt=1
    jtgt=143
    buf=1

    !-----------------------------------------------------------------------
    do j=max(1,jps-2),min(jm,jpe+2)
       ihw(j)=-mod(j,2)
       ihe(j)=ihw(j)+1
    enddo
    !-----------------------------------------------------------------------

    do j=jps,jpe
       do i=ips,ipe
          hbms(i,j)=grid%hres_lnd(i,j)
       enddo
    enddo
    !
    jmelin=jm-lines+1
    ibas=lines/2
    m2l=mod(lines,2)
    !
    do j=max(jps,lines),min(jpe,jmelin)
       ihl=ibas+mod(j,2)+m2l*mod(j+1,2)
       ihh=im-ibas-m2l*mod(j+1,2)

       !
       do i=max(ihl,ips),min(ihh,ipe)
          hbms(i,j)=0.
       enddo
    enddo

    !-----------------------------------------------------------------------
    smooth_loop: do ks=1,nsmud

#ifdef DM_PARALLEL
#    include "HALO_NMM_TERRAIN_SMOOTH.inc"
#endif
       do j=max(jps-1,1),min(jpe+1,jm-1)
          do i=max(ips-1,1),min(ipe+1,im-1)
             hne(i,j)=grid%hres_avc(i+ihe(j),j+1)-grid%hres_avc(i,j)
          enddo
       enddo
       do j=max(jps-1,2),min(jpe+1,jm)
          do i=max(ips-1,1),min(ipe+1,im-1)
             hse(i,j)=grid%hres_avc(i+ihe(j),j-1)-grid%hres_avc(i,j)
          enddo
       enddo
       !
       do j=max(jps,2),min(jpe,jm-1)
          do i=max(ips,1+mod(j,2)),min(ipe,im-1)
             grid%hres_avc(i,j)=(hne(i,j)-hne(i+ihw(j),j-1) &
                  &       +hse(i,j)-hse(i+ihw(j),j+1))*hbms(i,j)*0.125+grid%hres_avc(i,j)
          enddo
       enddo

       !--------------------------------------------------------------------

       !       smooth around boundary somehow?
       !       special treatment for four corners

       wbound: if(1>=ips .and. 1<=ipe) then
          if(1>=jps .and. 1<=jpe) then
             if (hbms(1,1) .eq. 1) then
                grid%hres_avc(1,1)=0.75*grid%hres_avc(1,1)+0.125*grid%hres_avc(1+ihe(1),2)+ &
                     0.0625*(grid%hres_avc(2,1)+grid%hres_avc(1,3))
             endif
          endif
          if(jm>=jps .and. jm<=jpe) then
             if (hbms(1,jm) .eq. 1) then
                grid%hres_avc(1,jm)=0.75*grid%hres_avc(1,jm)+0.125*grid%hres_avc(1+ihe(jm),jm-1)+ &
                     0.0625*(grid%hres_avc(2,jm)+grid%hres_avc(1,jm-2))
             endif
          endif
       endif wbound
       ebound: if(im>=ips .and. im<=ipe) then
          if(1>=jps .and. 1<=jpe) then
             if (hbms(im,1) .eq. 1) then
                grid%hres_avc(im,1)=0.75*grid%hres_avc(im,1)+0.125*grid%hres_avc(im+ihw(1),2)+ &
                     0.0625*(grid%hres_avc(im-1,1)+grid%hres_avc(im,3))
             endif
          endif
          if(jm>=jps .and. jm<=jpe) then
             if (hbms(im,jm) .eq. 1) then
                grid%hres_avc(im,jm)=0.75*grid%hres_avc(im,jm)+0.125*grid%hres_avc(im+ihw(jm),jm-1)+ &
                     0.0625*(grid%hres_avc(im-1,jm)+grid%hres_avc(im,jm-2))
             endif
          endif
       endif ebound

#ifdef DM_PARALLEL
#    include "HALO_NMM_TERRAIN_SMOOTH.inc"
#endif

       !       S bound
       if(1>=jps .and. 1<=jpe) then
          J=1
          do I=max(ips,2),min(ipe,im-1)
             if (hbms(I,J) .eq. 1) then
                hne(i,j)=0.125*(grid%hres_avc(I+ihw(J),J+1)+grid%hres_avc(I+ihe(J),J+1))
             endif
          enddo
          do I=max(ips,2),min(ipe,im-1)
             if (hbms(I,J) .eq. 1) then
                grid%hres_avc(I,J)=0.75*grid%hres_avc(I,J)+hne(i,j)
             endif
          enddo
       endif

       !       N bound
       if(jm>=jps .and. jm<=jpe) then
          J=JM
          do I=max(ips,2),min(ipe,im-1)
             if (hbms(I,J) .eq. 1) then
                grid%hres_avc(I,J)=0.75*grid%hres_avc(I,J)+0.125*(grid%hres_avc(I+ihw(J),J-1)+grid%hres_avc(I+ihe(J),J-1))
             endif
          enddo
          do I=max(ips,2),min(ipe,im-1)
             if (hbms(I,J) .eq. 1) then
                hne(i,j)=0.125*(grid%hres_avc(I+ihw(J),J-1)+grid%hres_avc(I+ihe(J),J-1))
             endif
          enddo
       endif

       !       W bound
       if(1>=ips .and. 1<=ipe) then
          I=1
          do J=max(jps,3),min(jpe,jm-2)
             if (hbms(I,J) .eq. 1) then
                hne(i,j)=0.125*(grid%hres_avc(I+ihe(J),J+1)+grid%hres_avc(I+ihe(J),J-1))
             endif
          enddo
          do J=max(jps,3),min(jpe,jm-2)
             if (hbms(I,J) .eq. 1) then
                grid%hres_avc(I,J)=0.75*grid%hres_avc(I,J)+hne(i,j)
             endif
          enddo
       endif

       !       E bound
       if(im>=ips .and. im<=ipe) then
          I=IM
          do J=max(jps,3),min(jpe,jm-2)
             if (hbms(I,J) .eq. 1) then
                hne(i,j)=0.125*(grid%hres_avc(I+ihw(J),J+1)+grid%hres_avc(I+ihw(J),J-1))
             endif
          enddo
          do J=max(jps,3),min(jpe,jm-2)
             if (hbms(I,J) .eq. 1) then
                grid%hres_avc(I,J)=0.75*grid%hres_avc(I,J)+hne(i,j)
             endif
          enddo
       endif

    enddo smooth_loop

#ifdef DM_PARALLEL
#    include "HALO_NMM_TERRAIN_SMOOTH.inc"
#endif

    !-------------4-point averaging of mountains along inner boundary-------

    if(2>=jps .and. 2<=jpe) then
       do i=max(ips,1),min(ipe,im-1)
          grid%hres_avc(i,2)=0.25*(grid%hres_avc(i,1)+grid%hres_avc(i+1,1)+ &
               &                    grid%hres_avc(i,3)+grid%hres_avc(i+1,3))
       enddo
    endif

    if(jm-1>=jps .and. jm-1<=jpe) then
       do i=max(ips,1),min(ipe,im-1)
          grid%hres_avc(i,jm-1)=0.25*(grid%hres_avc(i,jm-2)+grid%hres_avc(i+1,jm-2)+ &
               &                       grid%hres_avc(i,jm)+grid%hres_avc(i+1,jm))
       enddo
    endif

#ifdef DM_PARALLEL
#    include "HALO_NMM_TERRAIN_SMOOTH.inc"
#endif
    if(2>=ips .and. 2<=ipe) then
       do j=4,jm-3,2
          if(j>=jps .and. j<=jpe) then
             grid%hres_avc(1,j)=0.25*(grid%hres_avc(1,j-1)+ &
                  grid%hres_avc(2,j-1)+grid%hres_avc(1,j+1)+ &
                  grid%hres_avc(2,j+1))
          endif
       enddo
    endif

    if(im-1>=ips .and. im-1<=ipe) then
       do j=4,jm-3,2
          if(j>=jps .and. j<=jpe) then
             grid%hres_avc(im-1,j)=0.25*(grid%hres_avc(im-1,j-1)+ &
                  grid%hres_avc(im,j-1)+grid%hres_avc(im-1,j+1)+ &
                  grid%hres_avc(im,j+1))
          endif
       enddo
    endif
  end subroutine smooth_terrain

  ! ---------------------------------------------------------------------


  ! ---------------------------------------------------------------------


  subroutine smdhld(ids,ide,jds,jde,h,s1,lines,nsmud)
    ! This is the old serial smoothing routine from NMM_NEST_UTILS1.F
    character(len=255) :: message
    dimension ihw(jde-1),ihe(jde-1)
    dimension h(ids:ide,jds:jde),s1(ids:ide,jds:jde) &
         &     ,hbms(ide-1,jde-1),hne(ide-1,jde-1),hse(ide-1,jde-1)

    jm=jde-1
    im=ide-1
    !-----------------------------------------------------------------------
    do j=1,jm
       ihw(j)=-mod(j,2)
       ihe(j)=ihw(j)+1
    enddo
    !-----------------------------------------------------------------------

    do j=1,jm
       do i=1,im
          hbms(i,j)=s1(i,j)
       enddo
    enddo
    !     
    jmelin=jm-lines+1
    ibas=lines/2
    m2l=mod(lines,2)
    !     
    do j=lines,jmelin
       ihl=ibas+mod(j,2)+m2l*mod(j+1,2)
       ihh=im-ibas-m2l*mod(j+1,2)

       !     
       do i=ihl,ihh
          hbms(i,j)=0.
       enddo
    enddo

    !-----------------------------------------------------------------------
    ks_loop: do ks=1,nsmud

       !-----------------------------------------------------------------------
       do j=1,jm-1
          do i=1,im-1
             hne(i,j)=h(i+ihe(j),j+1)-h(i,j)
          enddo
       enddo
       do j=2,jm
          do i=1,im-1
             hse(i,j)=h(i+ihe(j),j-1)-h(i,j)
          enddo
       enddo
       !     
       do j=2,jm-1
          do i=1+mod(j,2),im-1
             h(i,j)=(hne(i,j)-hne(i+ihw(j),j-1) &
                  &              +hse(i,j)-hse(i+ihw(j),j+1))*hbms(i,j)*0.125+h(i,j)
          enddo
       enddo

       !-----------------------------------------------------------------------

       !     smooth around boundary somehow?
       !     special treatment for four corners

       if (hbms(1,1) .eq. 1) then
          h(1,1)=0.75*h(1,1)+0.125*h(1+ihe(1),2)+ &
               &           0.0625*(h(2,1)+h(1,3))
       endif

       if (hbms(im,1) .eq. 1) then
          h(im,1)=0.75*h(im,1)+0.125*h(im+ihw(1),2)+ &
               &           0.0625*(h(im-1,1)+h(im,3))
       endif

       if (hbms(1,jm) .eq. 1) then
          h(1,jm)=0.75*h(1,jm)+0.125*h(1+ihe(jm),jm-1)+ &
               &           0.0625*(h(2,jm)+h(1,jm-2))
       endif

       if (hbms(im,jm) .eq. 1) then
          h(im,jm)=0.75*h(im,jm)+0.125*h(im+ihw(jm),jm-1)+ &
               &           0.0625*(h(im-1,jm)+h(im,jm-2))
       endif

       !     S bound

       J=1
       do I=2,im-1
          if (hbms(I,J) .eq. 1) then
             h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J+1)+h(I+ihe(J),J+1))
          endif
       enddo

       !     N bound

       J=JM
       do I=2,im-1
          if (hbms(I,J) .eq. 1) then
             h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J-1)+h(I+ihe(J),J-1))
          endif
       enddo

       !     W bound

       I=1
       do J=3,jm-2
          if (hbms(I,J) .eq. 1) then
             h(I,J)=0.75*h(I,J)+0.125*(h(I+ihe(J),J+1)+h(I+ihe(J),J-1))
          endif
       enddo

       !     E bound

       I=IM
       do J=3,jm-2
          if (hbms(I,J) .eq. 1) then
             h(I,J)=0.75*h(I,J)+0.125*(h(I+ihw(J),J+1)+h(I+ihw(J),J-1))
          endif
       enddo

    enddo ks_loop

    !-------------4-point averaging of mountains along inner boundary-------

        do i=1,im-1
            h(i,2)=0.25*(h(i,1)+h(i+1,1)+h(i,3)+h(i+1,3))
        enddo

        do i=1,im-1
            h(i,jm-1)=0.25*(h(i,jm-2)+h(i+1,jm-2)+h(i,jm)+h(i+1,jm))
        enddo

        do j=4,jm-3,2
            h(1,j)=0.25*(h(1,j-1)+h(2,j-1)+h(1,j+1)+h(2,j+1))
        enddo

        do j=4,jm-3,2
            h(im-1,j)=0.25*(h(im-1,j-1)+h(im,j-1)+h(im-1,j+1)+h(im,j+1))
        enddo

    !-----------------------------------------------------------------------
    return
  end subroutine smdhld
#endif
end module module_SMOOTH_TERRAIN
