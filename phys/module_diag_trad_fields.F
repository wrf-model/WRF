#if (NMM_CORE == 1)
MODULE module_trad_fields
CONTAINS
   SUBROUTINE trad_fields
   END SUBROUTINE trad_fields
END MODULE module_trad_fields
#else
!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE module_trad_fields
CONTAINS

   SUBROUTINE trad_fields ( u,v,w,t,qv,zp,zb,pp,pb,p,pw,            &
                    msfux,msfuy,msfvx,msfvy,msftx,msfty,            &
                    f,e,sina,cosa,                                  &
                    ht,                                             &
                    use_theta_m,                                    &
                    sealevelp,                                      &
                    temperature,pressure,geoheight,                 &
                    umet,vmet,speed,dir,                            &
                    ids,ide, jds,jde, kds,kde,                      &
                    ims,ime, jms,jme, kms,kme,                      &
                    ips,ipe, jps,jpe, kps,kpe,                      &
                    its,ite, jts,jte, kts,kte                       )
   
      USE diag_functions
      USE module_model_constants
   
      IMPLICIT NONE
   
   
      !  Input variables
   
      INTEGER, INTENT(IN   )                                          :: ids,ide, jds,jde, kds,kde, &
                                                                         ims,ime, jms,jme, kms,kme, &
                                                                         ips,ipe, jps,jpe, kps,kpe, &
                                                                         its,ite, jts,jte, kts,kte
      REAL   , INTENT(IN   ) , DIMENSION(ims:ime , jms:jme)           :: msfux,msfuy,msfvx,msfvy,msftx,msfty, &
                                                                         f,e,sina,cosa,ht
      INTEGER, INTENT(IN   )                                          :: use_theta_m
      REAL   , INTENT(IN   ) , DIMENSION(ims:ime , kms:kme , jms:jme) :: u,v,w,t,qv,zp,zb,pp,pb,p,pw
   
      !  Output variables
   
      REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , jms:jme)           :: sealevelp
      REAL   , INTENT(  OUT) ,  DIMENSION(ims:ime , kms:kme , jms:jme) :: temperature , &
                                                                          pressure    , &
                                                                          geoheight   , &
                                                                          umet        , &
                                                                          vmet        , &
                                                                          speed       , &
                                                                          dir
   
      !  Local variables
   
      REAL,  DIMENSION(its:ite , jts:jte) :: pscf

      REAL, PARAMETER :: eps = 0.622, t_kelvin = svpt0 , s1 = 243.5, s2 = svp2 , s3 = svp1*10., s4 = 611.0, s5 = 5418.12
      REAL, PARAMETER :: zshul=75., tvshul=290.66
   
      INTEGER :: i, j, k
      REAL    :: es, qs
      REAL    :: gammas
   
      ! Half levels

      j_loop_h : DO j = jts , MIN(jte,jde-1)
         k_loop_h : DO k = kts , MIN(kte,kde-1)
            i_loop_h : DO i = its , MIN(ite,ide-1)
   
               !  Temperature

               IF ( use_theta_m .EQ. 1 ) THEN
                  temperature(i,k,j) = ( ( t(i,k,j) + t0 ) * ( (pb(i,k,j)+pp(i,k,j)) / p1000mb ) ** rcp ) / ( 1. + (R_v/R_d) * qv(i,k,j) )
               ELSE
                  temperature(i,k,j) = ( ( t(i,k,j) + t0 ) * ( (pb(i,k,j)+pp(i,k,j)) / p1000mb ) ** rcp )
               END IF
 
               !  Hydrostatic ressure

               pressure(i,k,j) = p(i,k,j)
 
               !  Height

               geoheight(i,k,j) = ( zb(i,k,j)+zp(i,k,j)+zb(i,k+1,j)+zp(i,k+1,j) ) / (2.0 * g )
   
               !  Earth relative winds

               umet(i,k,j) = (u(i,k,j)+u(i+1,k,j))*cosa(i,j) - (v(i,k,j)+v(i,k,j+1))*sina(i,j)
               vmet(i,k,j) = (u(i,k,j)+u(i+1,k,j))*sina(i,j) + (v(i,k,j)+v(i,k,j+1))*cosa(i,j)
   
               !  Horizontal wind speed

               speed(i,k,j) = SQRT ( umet(i,k,j)**2 + vmet(i,k,j)**2 )
   
               !  Direction

               IF      ( ( umet(i,k,j) .EQ. 0. ) .AND. ( vmet(i,k,j) .EQ. 0. ) ) THEN
                  dir(i,k,j) = 0.
               ELSE IF ( ( umet(i,k,j) .EQ. 0. ) .AND. ( vmet(i,k,j) .GT. 0. ) ) THEN
                  dir(i,k,j) = 180.
               ELSE IF ( ( umet(i,k,j) .EQ. 0. ) .AND. ( vmet(i,k,j) .LT. 0. ) ) THEN
                  dir(i,k,j) = 0.
               ELSE
                  dir(i,k,j) = 270. - atan2(vmet(i,k,j),umet(i,k,j)) * 180./3.14159265358979
                  IF ( dir(i,k,j) .GE. 360. ) THEN
                     dir(i,k,j) = dir(i,k,j) - 360.
                  END IF
                  IF ( dir(i,k,j) .LE.   0. ) THEN
                     dir(i,k,j) = dir(i,k,j) + 360.
                  END IF
               END IF

            END DO i_loop_h
         END DO k_loop_h
      END DO j_loop_h
   
      ! Full levels

      j_loop_f : DO j = jts , MIN(jte,jde-1)
         k_loop_f : DO k = kts , kte
            i_loop_f : DO i = its , MIN(ite,ide-1)

 ! so far nothing
 
            END DO i_loop_f
         END DO k_loop_f
      END DO j_loop_f
   
      ! 2d

      j_loop_2 : DO j = jts , MIN(jte,jde-1)
         i_loop_2 : DO i = its , MIN(ite,ide-1)

               ! Mean sea level pressure

               sealevelp(i,j) =   MSLP ( ht(i,j), pressure(i,kms,j), geoheight(i,kms,j) , &
                                         qv(i,kms,j), temperature(i,kms,j) )
 
         END DO i_loop_2
      END DO j_loop_2

   END SUBROUTINE trad_fields

END MODULE module_trad_fields
#endif
