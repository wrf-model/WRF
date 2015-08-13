!WRF:MEDIATION_LAYER:INTERPOLATIONFUNCTION
!

   SUBROUTINE interp_fcn ( cfld,                                 &  ! CD field
                           cids, cide, ckds, ckde, cjds, cjde,   &
                           cims, cime, ckms, ckme, cjms, cjme,   &
                           cits, cite, ckts, ckte, cjts, cjte,   &
                           nfld,                                 &  ! ND field
                           nids, nide, nkds, nkde, njds, njde,   &
                           nims, nime, nkms, nkme, njms, njme,   &
                           nits, nite, nkts, nkte, njts, njte,   &
                           nri, nrj                             )   ! nest ratios
     IMPLICIT NONE


     INTEGER, INTENT(IN) :: cids, cide, ckds, ckde, cjds, cjde,   &
                            cims, cime, ckms, ckme, cjms, cjme,   &
                            cits, cite, ckts, ckte, cjts, cjte,   &
                            nids, nide, nkds, nkde, njds, njde,   &
                            nims, nime, nkms, nkme, njms, njme,   &
                            nits, nite, nkts, nkte, njts, njte,   &
                            nri, nrj

     REAL, DIMENSION ( cims:cime, ckms:ckme, cjms:cjme ) :: cfld
     REAL, DIMENSION ( nims:nime, nkms:nkme, njms:njme ) :: nfld

     ! Local

     INTEGER ci, cj, ck, ni, nj, nk, ip, jp

     ! Iterate over the ND tile and comtute the values
     ! from the CD tile. 

     DO nj = njts, njte
        cj = cjts + nj / nrj          ! coord of CD point
        jp = cjts + mod ( nj , nrj )  ! coord of ND w/i CD point
        DO nk = nkts, nkte
           ck = nk
           DO ni = nits, nite
              ci = cits + ni / nri          ! coord of CD point
              ip = cits + mod ( ni , nri )  ! coord of ND w/i CD point

              ! This is a trivial implementation of the interp_fcn; just copies
              ! the values from the CD into the ND

              nfld( ni, nk, nj ) = cfld( ci , ck , cj )

           ENDDO
        ENDDO
     ENDDO

     RETURN

   END SUBROUTINE interp_fcn


