!WRF
!
MODULE module_tsout

USE module_model_constants

CONTAINS
  
SUBROUTINE cell_centered_calc( uts, vts, wts, tts, pts, tkets, zts,       &
                               t11ts, t22ts, t33ts, t12ts, t13ts, t23ts,  &
                               h3ts, usts, z0ts,                          &
                               u, v, w, t, p, pb, tke, ph, phb,           &
                               t11, t22, t33, t12, t13, t23,              &
                               h3, ust, z0,                               &
                               ids, ide, jds, jde, kds, kde,              &
                               ims, ime, jms, jme, kms, kme,              &
                               its, ite, jts, jte, kts, kte                )

!=======================================================================
!
!  This subroutine calculates cell centered parameters for reduced output
!
!======================================================================= 

   IMPLICIT NONE

   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT( OUT ) :: uts, vts, wts, &
                                                       tts, pts, tkets, zts, &
                                                       t11ts, t22ts, t33ts,  &
                                                       t12ts, t13ts, t23ts,  &
                                                       h3ts 

   REAL, DIMENSION(ims:ime,jms:jme), INTENT( OUT ) :: usts, z0ts

   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT( IN ) :: u, v, w,           &
                                                             t, p, pb,          &
                                                             tke, ph, phb, t11, &
                                                             t22, t33, t12,     & 
                                                             t13, t23, h3 
                                                         
   REAL, DIMENSION(ims:ime,jms:jme), INTENT( IN ) :: ust, z0 
   
   INTEGER , INTENT( IN  ) :: ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte

! local variables
   INTEGER :: i, j, k

!-----------------------------------------------------------------------
! executable starts here
!

   DO i = its, ite
      DO j = jts, jte
         
         DO k=kts,MIN(kte,kde-1)
            uts(i,k,j)  = 0.5 * ( u(i,k,j) + u(i+1,k,j) )
            vts(i,k,j)  = 0.5 * ( v(i,k,j) + v(i,k,j+1) )
            wts(i,k,j)  = 0.5 * ( w(i,k,j) + w(i,k+1,j) )
            tts(i,k,j)  = t(i,k,j)
            pts(i,k,j)  = p(i,k,j) + pb(i,k,j)
            tkets(i,k,j) = tke(i,k,j)
            t11ts(i,k,j) = t11(i,k,j)
            t22ts(i,k,j) = t22(i,k,j)
            t33ts(i,k,j) = t33(i,k,j)
            t12ts(i,k,j) = 0.25 * ( t12(i,k,j)   + t12(i+1,k,j) + t12(i,k,j+1) + t12(i+1,k,j+1) )
            t13ts(i,k,j) = 0.5 * ( t13(i,k,j) + t13(i+1,k,j) ) 
            t23ts(i,k,j) = 0.5 * ( t23(i,k,j) + t23(i,k,j+1) ) 
            h3ts(i,k,j)  = h3(i,k,j)
            zts(i,k,j)   = ( phb(i,k,j) + ph(i,k,j) )/g
         ENDDO
         
         t13ts(i,kde,j) = 0.5 * ( t13(i,kde,j) + t13(i+1,kde,j) ) 
         t23ts(i,kde,j) = 0.5 * ( t23(i,kde,j) + t23(i,kde,j+1) ) 
         h3ts(i,kde,j)  = h3(i,kde,j) 
         zts(i,kde,j)   = ( phb(i,kde,j) + ph(i,kde,j) )/g
                   
         usts(i,j) = ust(i,j)  
         z0ts(i,j) = z0(i,j)  
         
      END DO
   END DO
   
   RETURN

END SUBROUTINE cell_centered_calc

END MODULE module_tsout
