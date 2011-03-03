      SUBROUTINE ASSEMBLE(FG,FL,knd)

      USE ATM_cc, ONLY: &
     &                  ids,idf,jds,jdf,kds,kde, &
     &                  ims,ime,jms,jme,kms,kme, &
     &                  its,ite,jts,jte,kts,kte

      USE MODULE_PATCH_QUILT

      implicit none

      real,dimension(ids:idf,jds:jdf),intent(out):: FG
      real,dimension(ims:ime,jms:jme),intent(in) :: FL
      integer,                        intent(in) :: knd

      integer kl,kg
!

      kl=kind(FL)
      kg=kind(FG)
      if (knd.ne.kl .or. knd.ne.kg) then
        print*,'knd must = loc. and glob. kinds in ASSEMBLE. '// &
     &  'To generalize, call of QUILT_2 must be generalized ',knd,kl,kg
        call GLOB_ABORT(1,'whong kinds in ASSEMBLE',1)
      end if

      call QUILT_2(FL,FG,ids,idf,jds,jdf,kds,kde, &
     &ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

      return
      END
!
!***********************************************************************
!
      SUBROUTINE DISASSEMBLE(FG,FL,knd)

      USE ATM_cc, ONLY: ids,idf,jds,jdf,kds,kde, &
     &                  ims,ime,jms,jme,kms,kme, &
     &                  its,ite,jts,jte,kts,kte

      USE MODULE_PATCH_QUILT

      implicit none

      real,dimension(ids:idf,jds:jdf),intent(in) :: FG
      real,dimension(ims:ime,jms:jme),intent(out):: FL
      integer,                        intent(in) :: knd

      integer kl,kg
!

      kl=kind(FL)
      kg=kind(FG)
      if (knd.ne.kl .or. knd.ne.kg) then
        print*,'knd must = loc. and glob. kinds in DISASSEMBLE. '// &
     &  'To generalize, call of PATCH must be generalized ',knd,kl,kg
        call GLOB_ABORT(1,'whong kinds in DISASSEMBLE',1)
      end if

      call PATCH(FG,FL,ids,idf,jds,jdf,kds,kde, &
     &ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)

      return
      END
