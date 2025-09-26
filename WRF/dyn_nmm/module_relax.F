module module_relax
  implicit none
contains
  subroutine relax4e(grid,relax_coeff,nrelax,expand,  &
       IDS,IDE,JDS,JDE,KDS,KDE, &
       IMS,IME,JMS,JME,KMS,KME, &
       IPS,IPE,JPS,JPE,KPS,KPE)
    USE MODULE_DOMAIN, ONLY : domain,get_ijk_from_grid
#ifdef DM_PARALLEL
    USE MODULE_COMM_DM, ONLY : HALO_NMM_MEMBRANE_RELAX_sub, HALO_NMM_MEMBRANE_MASK_sub
    USE MODULE_DM, ONLY: ntasks_x, ntasks_y, mytask, ntasks, local_communicator
#endif
    implicit none
    type(domain), intent(inout) :: grid
    real, intent(in) :: relax_coeff
    integer, intent(in) :: nrelax
    integer, intent(in) :: expand

    integer :: IDS,IDE,JDS,JDE,KDS,KDE
    integer :: IMS,IME,JMS,JME,KMS,KME
    integer :: IPS,IPE,JPS,JPE,KPS,KPE
    real :: nextvalue(ims:ime,jms:jme)
    real :: r,r1
    integer :: i,j,irelax,a,iter

    ! Aliases to simplify the expressions below, we'll use "r" instead
    ! of relax_coeff, and r1 instead of 1-r:
    r=relax_coeff
    r1=1.0-r

    ! Relax all points within "expand" gridpoints of a point that
    ! wants to be relaxed:
    expand_relax: if(expand>0) then
       do j=jps,min(jpe,jde-1)
          do i=ips,min(ipe,ide-1)
             if(grid%relaxmask(i,j)) then
                grid%relaximask(i,j)=1
             else
                grid%relaximask(i,j)=0
             endif
          enddo
       enddo
       if(.false.) then
          do iter=1,expand
#ifdef DM_PARALLEL
#      include "HALO_NMM_MEMBRANE_MASK.inc"
#endif
             do j=max(jps,jds+1),min(jpe,jde-2)
                a=mod(j,2)
                do i=max(ips,ids+1),min(ipe,ide-2)
                   grid%relaximask(i,j) = grid%relaximask(i,j) + &
                        grid%relaximask(i-a, j-1) + &
                        grid%relaximask(i-a, j+1) + &
                        grid%relaximask(i+1-a, j+1) + &
                        grid%relaximask(i+1-a, j-1)
                enddo
             enddo
          enddo
       endif
       do j=jps,min(jpe,jde-1)
          do i=ips,min(ipe,ide-1)
             if(grid%relaximask(i,j)>0) then
                grid%relaxmask(i,j)=.true.
             endif
          enddo
       enddo
    endif expand_relax
    relaxloop: do irelax=1,nrelax
#ifdef DM_PARALLEL
#      include "HALO_NMM_MEMBRANE_RELAX.inc"
#endif

       !$omp parallel do      &
       !$omp private(i,j,a)
       bigj: do j=max(jps,jds+1),min(jpe,jde-2)
          a=mod(j,2)
          bigi: do i=max(ips,ids+1),min(ipe,ide-2)
             if(grid%relaxmask(i,j)) then
                nextvalue(i,j) = &
                     r1 * grid%relaxwork(i,j) + &
                     r * ( &
                     grid%relaxwork(i-a,  j-1) + &
                     grid%relaxwork(i-a,  j+1) + &
                     grid%relaxwork(i+1-a,j+1) + &
                     grid%relaxwork(i+1-a,j-1) )/4.
             else
                nextvalue(i,j) = grid%relaxwork(i,j)
             endif
          enddo bigi
       enddo bigj
       ! Handle boundary points next.
       ! SOUTH:
       if(jps<=jds) then
          j=1
          a=mod(j,2)
          !$omp parallel do      &
          !$omp private(i,j,a)
          do i=max(ips,ids+1),min(ipe,ide-2)
             if(grid%relaxmask(i,j)) then
                nextvalue(i,j) = &
                     r1 * grid%relaxwork(i,j) + r * &
                     (grid%relaxwork(i-a,  j+1) + grid%relaxwork(i+1-a,j+1) )/2.
             else
                nextvalue(i,j)=grid%relaxwork(i,j)
             endif
          enddo
       endif
       ! NORTH:
       if(jpe>=jde-1) then
          j=jde-1
          a=mod(j,2)
          !$omp parallel do      &
          !$omp private(i,j,a)
          do i=max(ips,ids+1),min(ipe,ide-2)
             if(grid%relaxmask(i,j)) then
                nextvalue(i,j) = &
                     r1 * grid%relaxwork(i,j) + r * &
                     (grid%relaxwork(i-a,  j-1) + grid%relaxwork(i+1-a,j-1) )/2.
             else
                nextvalue(i,j)=grid%relaxwork(i,j)
             endif
          enddo
       endif
       ! WEST:
       if(ips<=ids) then
          i=1
          !$omp parallel do      &
          !$omp private(i,j,a)
          do j=max(jps,jds+1),min(jpe,jde-2)
             a=mod(j,2)
             if(grid%relaxmask(i,j)) then
                nextvalue(i,j) = &
                     r1 * grid%relaxwork(i,j) + r * &
                     (grid%relaxwork(i+1-a,j+1) + grid%relaxwork(i+1-a,j-1) )/2.
             else
                nextvalue(i,j)=grid%relaxwork(i,j)
             endif
          enddo
       endif
       ! EAST:
       if(ipe>=ide-1) then
          i=ide-1
          !$omp parallel do      &
          !$omp private(i,j,a)
          do j=max(jps,jds+1),min(jpe,jde-2)
             a=mod(j,2)
             if(grid%relaxmask(i,j)) then
                nextvalue(i,j) = &
                     r1 * grid%relaxwork(i,j) + r * &
                     (grid%relaxwork(i-a,j+1) + grid%relaxwork(i-a,j-1) )/2.
             else
                nextvalue(i,j)=grid%relaxwork(i,j)
             endif
          enddo
       endif

       ! Finally, handle corner points:
       ! SOUTHWEST:
       if(ips<=ids .and. jps<=jds) then
          if(grid%relaxmask(ids,jds)) then
             nextvalue(ids,jds) = &
                  r1 * grid%relaxwork(ids,jds) + r * &
                  grid%relaxwork(ids,  jds+1)
          else
             nextvalue(ids,jds)=grid%relaxwork(ids,jds)
          end if
       endif
       ! SOUTHEAST:
       if(ipe>=ide-1 .and. jps<=jds) then
          if(grid%relaxmask(ide-1,jds)) then
             nextvalue(ide-1,jds) = &
                  r1 * grid%relaxwork(ide-1,jds) + r * &
                  (grid%relaxwork(ide-1,jds+1) + grid%relaxwork(ide-2,jds))/2.
          else
             nextvalue(ide-1,jds)=grid%relaxwork(ide-1,jds)
          endif
       endif
       ! NORTHWEST:
       if(ips<=ids .and. jpe>=jde-1) then
          if(grid%relaxmask(ids,jde-1)) then
             a=mod(jde-1,2)
             if(a==1) then
                nextvalue(ids,jde-1) = &
                     r1 * grid%relaxwork(ids,jde-1) + r * &
                     grid%relaxwork(ids,jde-2)
             else
                nextvalue(ids,jde-1) = &
                     r1 * grid%relaxwork(ids,jde-1) + r * &
                     (grid%relaxwork(ids,jde-2) + grid%relaxwork(ids+1,jde-2))/2.
             endif
          else
             nextvalue(ids,jde-1)=grid%relaxwork(ids,jde-1)
          endif
       endif
       ! NORTHEAST:
       if(ipe>=ide-1 .and. jpe>=jde-1) then
          if(grid%relaxmask(ide-1,jde-1)) then
             a=mod(jde-1,2)
             if(a==0) then
                nextvalue(ide-1,jde-1) = &
                     r1 * grid%relaxwork(ide-1,jde-1) + r * &
                     grid%relaxwork(ide-1,jde-2)
             else
                nextvalue(ide-1,jde-1) = &
                     r1 * grid%relaxwork(ide-1,jde-1) + r * &
                     (grid%relaxwork(ide-1,jde-2) + grid%relaxwork(ide-2,jde-2))/2.
             endif
          else
             nextvalue(ide-1,jde-1)=grid%relaxwork(ide-1,jde-1)
          endif
       endif

       do j=max(jps,jds),min(jpe,jde-1)
          a=mod(j,2)
          do i=max(ips,ids),min(ipe,ide-1)
             grid%relaxwork(i,j)=nextvalue(i,j)
          enddo
       enddo
    enddo relaxloop
  end subroutine relax4e
end module module_relax
