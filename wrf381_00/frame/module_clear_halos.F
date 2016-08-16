module module_clear_halos
  implicit none
contains
  ! --------------------------------------------------------------------
  subroutine clear_ij_full_domain(grid,how)
    ! Convenience function - wrapper around clear_ij_halos.  Clears
    ! full domain with badval.  See clear_ij_halos for details.
    use module_domain, only: domain,get_ijk_from_grid,fieldlist
    type(domain), intent(inout) :: grid
    integer, intent(in) :: how
    !
    call clear_ij_halos(grid,how,full_domain=.true.)
  end subroutine clear_ij_full_domain
  ! --------------------------------------------------------------------
  subroutine clear_ij_halos(grid,how,full_domain)
    ! Clears halo regions OR full domain with badval.  Select full
    ! domain with full_domain=.true.  Select badval type with "how"
    ! parameter:

    ! how=1 -- badval=0
    ! how=2 -- badval=quiet NaN or -maxint
    ! how=3 -- badval=signaling NaN or -maxint

    ! Fills outside domain with 0 UNLESS fill_domain=.true.  If
    ! fill_domain=true., entire array is filled with badval.

    use module_domain, only: domain,get_ijk_from_grid,fieldlist
    use module_configure, only: PARAM_FIRST_SCALAR
#ifndef NO_IEEE_MODULE
    use,intrinsic :: ieee_arithmetic
#endif
    implicit none

    logical, intent(in), optional :: full_domain
    integer, intent(in) :: how
    type(domain), intent(inout) :: grid

    type( fieldlist ), pointer :: p
    integer :: itrace, i,j, &
         ids, ide, jds, jde, kds, kde,    &
         ims, ime, jms, jme, kms, kme,    &
         ips, ipe, jps, jpe, kps, kpe
    logical :: fulldom
    real :: badR, badR_N,badR_NE,badR_NW,badR_S,badR_SW,badR_SE,badR_E,badR_W
#if (RWORDSIZE==4)
    double precision :: badD, badD_N,badD_NE,badD_NW,badD_S,badD_SW,badD_SE,badD_E,badD_W
#else
    real             :: badD, badD_N,badD_NE,badD_NW,badD_S,badD_SW,badD_SE,badD_E,badD_W
#endif
    integer :: badI, badI_N,badI_NE,badI_NW,badI_S,badI_SW,badI_SE,badI_E,badI_W

    select case(how)
    case(0)
       return
    case(1)
       call wrf_message('Fill I and J halos with 0.')
       badR = 0
       badD = 0
       badI = 0
    case(2)
       call wrf_message('Fill I and J halos with -maxint or quiet NaN.')
#ifndef NO_IEEE_MODULE
       badR = ieee_value(badR,ieee_quiet_nan)
       badD = ieee_value(badD,ieee_quiet_nan)
       badI = -huge(badI)
#else
       badR = -huge(badR) 
       badD = -huge(badD)
       badI = -huge(badI)
#endif
    case(3)
       call wrf_message('Fill I and J halos with -maxint or signalling NaN.')
#ifndef NO_IEEE_MODULE
       badR = ieee_value(badR,ieee_signaling_nan)
       badD = ieee_value(badD,ieee_signaling_nan)
       badI = -huge(badI)
#else
       badR = -huge(badR) 
       badD = -huge(badD)
       badI = -huge(badI)
#endif
    case default
       if(fulldom) then
          call wrf_message('Invalid value for clear_ij_full_domain/clear_ij_halos "how" parameter.  Will not clear domain.')
       else
          call wrf_message('Invalid value for clear_ij_halos "how" parameter.  Will not clear halos.')
       endif
       return
    end select

    fulldom=.false.
    if(present(full_domain)) fulldom=full_domain
    if(fulldom) then
       call wrf_message('Filling entire memory area, not just halos.')
    endif

    badR_N =badR ; badD_N =badD ; badI_N =badI
    badR_NE=badR ; badD_NE=badD ; badI_NE=badI
    badR_NW=badR ; badD_NW=badD ; badI_NW=badI
    badR_S =badR ; badD_S =badD ; badI_S =badI
    badR_SE=badR ; badD_SE=badD ; badI_SE=badI
    badR_SW=badR ; badD_SW=badD ; badI_SW=badI
    badR_E =badR ; badD_E =badD ; badI_E =badI
    badR_W =badR ; badD_W =badD ; badI_W =badI

    CALL get_ijk_from_grid (  grid ,      &
         ids, ide, jds, jde, kds, kde,    &
         ims, ime, jms, jme, kms, kme,    &
         ips, ipe, jps, jpe, kps, kpe     )

    if(ips==ids) then
       badR_S =0 ; badD_S =0 ; badI_S =0
       badR_SE=0 ; badD_SE=0 ; badI_SE=0
       badR_SW=0 ; badD_SW=0 ; badI_SW=0
    endif
    if(ipe==ide) then
       badR_N =0 ; badD_N =0 ; badI_N =0
       badR_NE=0 ; badD_NE=0 ; badI_NE=0
       badR_NW=0 ; badD_NW=0 ; badI_NW=0
    endif
    if(jps==jds) then
       badR_NW=0 ; badD_NW=0 ; badI_NW=0
       badR_SW=0 ; badD_SW=0 ; badI_SW=0
       badR_W =0 ; badD_W =0 ; badI_W =0
    endif
    if(jpe==jde) then
       badR_NE=0 ; badD_NE=0 ; badI_NE=0
       badR_SE=0 ; badD_SE=0 ; badI_SE=0
       badR_E =0 ; badD_E =0 ; badI_E =0
    endif

    if(.not.associated(grid%head_statevars)) then
       call wrf_message('grid%head_statevars is not associated')
       return
    elseif(.not.associated(grid%head_statevars%next)) then
       call wrf_message('grid%head_statevars%next is not associated')
       return
    endif
    p => grid%head_statevars%next
    DO WHILE ( ASSOCIATED( p ) ) 
       IF ( p%ProcOrient .NE. 'X' .AND. p%ProcOrient .NE. 'Y' ) THEN
          IF ( p%Ndim .EQ. 2 ) THEN
             IF (      p%MemoryOrder(1:1) .EQ. 'X' .AND.  p%MemoryOrder(2:2) .EQ.  'Y' ) THEN
                IF      ( p%Type .EQ. 'r' ) THEN
                   IF ( SIZE(p%rfield_2d,1)*SIZE(p%rfield_2d,2) .GT. 1 ) THEN
                      if(fulldom) then
                         p%rfield_2d=badR
                      else
                         p%rfield_2d(ims:ips-1,jps:jpe) = badR_S
                         p%rfield_2d(ims:ips-1,jms:jps-1) = badR_SW
                         p%rfield_2d(ims:ips-1,jpe+1:jme) = badR_SE
                         p%rfield_2d(ipe+1:ime,jps:jpe) = badR_N
                         p%rfield_2d(ipe+1:ime,jms:jps-1) = badR_NW
                         p%rfield_2d(ipe+1:ime,jpe+1:jme) = badR_NE
                         p%rfield_2d(ips:ipe,jms:jps-1) = badR_W
                         p%rfield_2d(ips:ipe,jpe+1:jme) = badR_E
                      endif
                   ENDIF
                ELSE IF ( p%Type .EQ. 'd' ) THEN
                   IF ( SIZE(p%dfield_2d,1)*SIZE(p%dfield_2d,2) .GT. 1 ) THEN
                      if(fulldom) then
                         p%dfield_2d=badD
                      else
                         p%dfield_2d(ims:ips-1,jps:jpe) = badD_S
                         p%dfield_2d(ims:ips-1,jms:jps-1) = badD_SW
                         p%dfield_2d(ims:ips-1,jpe+1:jme) = badD_SE
                         p%dfield_2d(ipe+1:ime,jps:jpe) = badD_N
                         p%dfield_2d(ipe+1:ime,jms:jps-1) = badD_NW
                         p%dfield_2d(ipe+1:ime,jpe+1:jme) = badD_NE
                         p%dfield_2d(ips:ipe,jms:jps-1) = badD_W
                         p%dfield_2d(ips:ipe,jpe+1:jme) = badD_E
                      endif
                   ENDIF
                ELSE IF ( p%Type .EQ. 'i' ) THEN
                   IF ( SIZE(p%ifield_2d,1)*SIZE(p%ifield_2d,2) .GT. 1 ) THEN
                      if(fulldom) then
                         p%ifield_2d=badI
                      else
                         p%ifield_2d(ims:ips-1,jps:jpe) = badI_S
                         p%ifield_2d(ims:ips-1,jms:jps-1) = badI_SW
                         p%ifield_2d(ims:ips-1,jpe+1:jme) = badI_SE
                         p%ifield_2d(ipe+1:ime,jps:jpe) = badI_N
                         p%ifield_2d(ipe+1:ime,jms:jps-1) = badI_NW
                         p%ifield_2d(ipe+1:ime,jpe+1:jme) = badI_NE
                         p%ifield_2d(ips:ipe,jms:jps-1) = badI_W
                         p%ifield_2d(ips:ipe,jpe+1:jme) = badI_E
                      endif
                   ENDIF
                ENDIF
             ENDIF
          ELSE IF ( p%Ndim .EQ. 3 ) THEN
             IF (      p%MemoryOrder(1:1) .EQ. 'X' .AND.  p%MemoryOrder(3:3) .EQ.  'Y' ) THEN
                IF      ( p%Type .EQ. 'r' ) THEN
                   IF ( SIZE(p%rfield_3d,1)*SIZE(p%rfield_3d,3) .GT. 1 ) THEN
                      if(fulldom) then
                         p%rfield_3d=badR
                      else
                         p%rfield_3d(ims:ips-1,:,jps:jpe) = badR_S
                         p%rfield_3d(ims:ips-1,:,jms:jps-1) = badR_SW
                         p%rfield_3d(ims:ips-1,:,jpe+1:jme) = badR_SE
                         p%rfield_3d(ipe+1:ime,:,jps:jpe) = badR_N
                         p%rfield_3d(ipe+1:ime,:,jms:jps-1) = badR_NW
                         p%rfield_3d(ipe+1:ime,:,jpe+1:jme) = badR_NE
                         p%rfield_3d(ips:ipe,:,jms:jps-1) = badR_W
                         p%rfield_3d(ips:ipe,:,jpe+1:jme) = badR_E
                      endif
                   ENDIF
                ELSE IF ( p%Type .EQ. 'd' ) THEN
                   IF ( SIZE(p%dfield_3d,1)*SIZE(p%dfield_3d,3) .GT. 1 ) THEN
                      if(fulldom) then
                         p%dfield_3d=badD
                      else
                         p%dfield_3d(ims:ips-1,:,jps:jpe) = badD_S
                         p%dfield_3d(ims:ips-1,:,jms:jps-1) = badD_SW
                         p%dfield_3d(ims:ips-1,:,jpe+1:jme) = badD_SE
                         p%dfield_3d(ipe+1:ime,:,jps:jpe) = badD_N
                         p%dfield_3d(ipe+1:ime,:,jms:jps-1) = badD_NW
                         p%dfield_3d(ipe+1:ime,:,jpe+1:jme) = badD_NE
                         p%dfield_3d(ips:ipe,:,jms:jps-1) = badD_W
                         p%dfield_3d(ips:ipe,:,jpe+1:jme) = badD_E
                      endif
                   ENDIF
                ELSE IF ( p%Type .EQ. 'i' ) THEN
                   IF ( SIZE(p%ifield_3d,1)*SIZE(p%ifield_3d,3) .GT. 1 ) THEN
                      if(fulldom) then
                         p%ifield_3d=badI
                      else
                         p%ifield_3d(ims:ips-1,:,jps:jpe) = badI_S
                         p%ifield_3d(ims:ips-1,:,jms:jps-1) = badI_SW
                         p%ifield_3d(ims:ips-1,:,jpe+1:jme) = badI_SE
                         p%ifield_3d(ipe+1:ime,:,jps:jpe) = badI_N
                         p%ifield_3d(ipe+1:ime,:,jms:jps-1) = badI_NW
                         p%ifield_3d(ipe+1:ime,:,jpe+1:jme) = badI_NE
                         p%ifield_3d(ips:ipe,:,jms:jps-1) = badI_W
                         p%ifield_3d(ips:ipe,:,jpe+1:jme) = badI_E
                      endif
                   ENDIF
                ENDIF
             ELSE IF (  p%MemoryOrder(1:2) .EQ. 'XY' ) THEN
                IF      ( p%Type .EQ. 'r' ) THEN
                   IF ( SIZE(p%rfield_3d,1)*SIZE(p%rfield_3d,2) .GT. 1 ) THEN
                      if(fulldom) then
                         p%rfield_3d=badR
                      else
                         p%rfield_3d(ims:ips-1,jps:jpe,:) = badR_S
                         p%rfield_3d(ims:ips-1,jms:jps-1,:) = badR_SW
                         p%rfield_3d(ims:ips-1,jpe+1:jme,:) = badR_SE
                         p%rfield_3d(ipe+1:ime,jps:jpe,:) = badR_N
                         p%rfield_3d(ipe+1:ime,jms:jps-1,:) = badR_NW
                         p%rfield_3d(ipe+1:ime,jpe+1:jme,:) = badR_NE
                         p%rfield_3d(ips:ipe,jms:jps-1,:) = badR_W
                         p%rfield_3d(ips:ipe,jpe+1:jme,:) = badR_E
                      endif
                   ENDIF
                ELSE IF ( p%Type .EQ. 'd' ) THEN
                   IF ( SIZE(p%dfield_3d,1)*SIZE(p%dfield_3d,2) .GT. 1 ) THEN
                      if(fulldom) then
                         p%dfield_3d=badD
                      else
                         p%dfield_3d(ims:ips-1,jps:jpe,:) = badD_S
                         p%dfield_3d(ims:ips-1,jms:jps-1,:) = badD_SW
                         p%dfield_3d(ims:ips-1,jpe+1:jme,:) = badD_SE
                         p%dfield_3d(ipe+1:ime,jps:jpe,:) = badD_N
                         p%dfield_3d(ipe+1:ime,jms:jps-1,:) = badD_NW
                         p%dfield_3d(ipe+1:ime,jpe+1:jme,:) = badD_NE
                         p%dfield_3d(ips:ipe,jms:jps-1,:) = badD_W
                         p%dfield_3d(ips:ipe,jpe+1:jme,:) = badD_E
                      endif
                   ENDIF
                ELSE IF ( p%Type .EQ. 'i' ) THEN
                   IF ( SIZE(p%ifield_3d,1)*SIZE(p%ifield_3d,2) .GT. 1 ) THEN
                      if(fulldom) then
                         p%ifield_3d=badI
                      else
                         p%ifield_3d(ims:ips-1,jps:jpe,:) = badI_S
                         p%ifield_3d(ims:ips-1,jms:jps-1,:) = badI_SW
                         p%ifield_3d(ims:ips-1,jpe+1:jme,:) = badI_SE
                         p%ifield_3d(ipe+1:ime,jps:jpe,:) = badI_N
                         p%ifield_3d(ipe+1:ime,jms:jps-1,:) = badI_NW
                         p%ifield_3d(ipe+1:ime,jpe+1:jme,:) = badI_NE
                         p%ifield_3d(ips:ipe,jms:jps-1,:) = badI_W
                         p%ifield_3d(ips:ipe,jpe+1:jme,:) = badI_E
                      endif
                   ENDIF
                ENDIF
             ENDIF
          ELSE IF ( p%Ndim .EQ. 4 ) THEN
             IF (      p%MemoryOrder(1:1) .EQ. 'X' .AND.  p%MemoryOrder(3:3) .EQ.  'Y' ) THEN
                IF      ( p%Type .EQ. 'r' ) THEN
                   IF ( SIZE(p%rfield_4d,1)*SIZE(p%rfield_4d,3) .GT. 1 ) THEN
                      DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                         if(fulldom) then
                            p%rfield_4d(:,:,:,itrace)=badR
                         else
                            p%rfield_4d(ims:ips-1,:,jps:jpe,itrace) = badR_S
                            p%rfield_4d(ims:ips-1,:,jms:jps-1,itrace) = badR_SW
                            p%rfield_4d(ims:ips-1,:,jpe+1:jme,itrace) = badR_SE
                            p%rfield_4d(ipe+1:ime,:,jps:jpe,itrace) = badR_N
                            p%rfield_4d(ipe+1:ime,:,jms:jps-1,itrace) = badR_NW
                            p%rfield_4d(ipe+1:ime,:,jpe+1:jme,itrace) = badR_NE
                            p%rfield_4d(ips:ipe,:,jms:jps-1,itrace) = badR_W
                            p%rfield_4d(ips:ipe,:,jpe+1:jme,itrace) = badR_E
                         endif
                      ENDDO
                   ENDIF
                ELSE IF ( p%Type .EQ. 'd' ) THEN
                   IF ( SIZE(p%dfield_4d,1)*SIZE(p%dfield_4d,3) .GT. 1 ) THEN
                      DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                         if(fulldom) then
                            p%dfield_4d(:,:,:,itrace)=badD
                         else
                            p%dfield_4d(ims:ips-1,:,jps:jpe,itrace) = badD_S
                            p%dfield_4d(ims:ips-1,:,jms:jps-1,itrace) = badD_SW
                            p%dfield_4d(ims:ips-1,:,jpe+1:jme,itrace) = badD_SE
                            p%dfield_4d(ipe+1:ime,:,jps:jpe,itrace) = badD_N
                            p%dfield_4d(ipe+1:ime,:,jms:jps-1,itrace) = badD_NW
                            p%dfield_4d(ipe+1:ime,:,jpe+1:jme,itrace) = badD_NE
                            p%dfield_4d(ips:ipe,:,jms:jps-1,itrace) = badD_W
                            p%dfield_4d(ips:ipe,:,jpe+1:jme,itrace) = badD_E
                         endif
                      ENDDO
                   ENDIF
                ELSE IF ( p%Type .EQ. 'i' ) THEN
                   IF ( SIZE(p%ifield_4d,1)*SIZE(p%ifield_4d,3) .GT. 1 ) THEN
                      DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                         if(fulldom) then
                            p%ifield_4d(:,:,:,itrace)=badI
                         else
                            p%ifield_4d(ims:ips-1,:,jps:jpe,itrace) = badI_S
                            p%ifield_4d(ims:ips-1,:,jms:jps-1,itrace) = badI_SW
                            p%ifield_4d(ims:ips-1,:,jpe+1:jme,itrace) = badI_SE
                            p%ifield_4d(ipe+1:ime,:,jps:jpe,itrace) = badI_N
                            p%ifield_4d(ipe+1:ime,:,jms:jps-1,itrace) = badI_NW
                            p%ifield_4d(ipe+1:ime,:,jpe+1:jme,itrace) = badI_NE
                            p%ifield_4d(ips:ipe,:,jms:jps-1,itrace) = badI_W
                            p%ifield_4d(ips:ipe,:,jpe+1:jme,itrace) = badI_E
                         endif
                      ENDDO
                   ENDIF
                ENDIF
             ELSE IF (  p%MemoryOrder(1:2) .EQ. 'XY' ) THEN
                IF      ( p%Type .EQ. 'r' ) THEN
                   IF ( SIZE(p%rfield_4d,1)*SIZE(p%rfield_4d,2) .GT. 1 ) THEN
                      DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                         if(fulldom) then
                            p%rfield_4d(:,:,:,itrace)=badR
                         else
                            p%rfield_4d(ims:ips-1,jps:jpe,:,itrace) = badR_S
                            p%rfield_4d(ims:ips-1,jms:jps-1,:,itrace) = badR_SW
                            p%rfield_4d(ims:ips-1,jpe+1:jme,:,itrace) = badR_SE
                            p%rfield_4d(ipe+1:ime,jps:jpe,:,itrace) = badR_N
                            p%rfield_4d(ipe+1:ime,jms:jps-1,:,itrace) = badR_NW
                            p%rfield_4d(ipe+1:ime,jpe+1:jme,:,itrace) = badR_NE
                            p%rfield_4d(ips:ipe,jms:jps-1,:,itrace) = badR_W
                            p%rfield_4d(ips:ipe,jpe+1:jme,:,itrace) = badR_E
                         endif
                      ENDDO
                   ENDIF
                ELSE IF ( p%Type .EQ. 'd' ) THEN
                   IF ( SIZE(p%dfield_4d,1)*SIZE(p%dfield_4d,2) .GT. 1 ) THEN
                      DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                         if(fulldom) then
                            p%dfield_4d(:,:,:,itrace)=badD
                         else
                            p%dfield_4d(ims:ips-1,jps:jpe,:,itrace) = badD_S
                            p%dfield_4d(ims:ips-1,jms:jps-1,:,itrace) = badD_SW
                            p%dfield_4d(ims:ips-1,jpe+1:jme,:,itrace) = badD_SE
                            p%dfield_4d(ipe+1:ime,jps:jpe,:,itrace) = badD_N
                            p%dfield_4d(ipe+1:ime,jms:jps-1,:,itrace) = badD_NW
                            p%dfield_4d(ipe+1:ime,jpe+1:jme,:,itrace) = badD_NE
                            p%dfield_4d(ips:ipe,jms:jps-1,:,itrace) = badD_W
                            p%dfield_4d(ips:ipe,jpe+1:jme,:,itrace) = badD_E
                         endif
                      ENDDO
                   ENDIF
                ELSE IF ( p%Type .EQ. 'i' ) THEN
                   IF ( SIZE(p%ifield_4d,1)*SIZE(p%ifield_4d,2) .GT. 1 ) THEN
                      DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                         if(fulldom) then
                            p%ifield_4d(:,:,:,itrace)=badI
                         else
                            p%ifield_4d(ims:ips-1,jps:jpe,:,itrace) = badI_S
                            p%ifield_4d(ims:ips-1,jms:jps-1,:,itrace) = badI_SW
                            p%ifield_4d(ims:ips-1,jpe+1:jme,:,itrace) = badI_SE
                            p%ifield_4d(ipe+1:ime,jps:jpe,:,itrace) = badI_N
                            p%ifield_4d(ipe+1:ime,jms:jps-1,:,itrace) = badI_NW
                            p%ifield_4d(ipe+1:ime,jpe+1:jme,:,itrace) = badI_NE
                            p%ifield_4d(ips:ipe,jms:jps-1,:,itrace) = badI_W
                            p%ifield_4d(ips:ipe,jpe+1:jme,:,itrace) = badI_E
                         endif
                      ENDDO
                   ENDIF
                ENDIF
             ENDIF
          ENDIF
       ENDIF
       p => p%next
    ENDDO
  end subroutine clear_ij_halos
end module module_clear_halos
