MODULE module_sf_fogdes

  USE module_model_constants

  USE module_bl_mynn, only: qcgmin, gno, gpw


  IMPLICIT NONE

   REAL, PARAMETER :: myu = 1.8e-5  

CONTAINS

  SUBROUTINE sf_fogdes(&
               vdfg,fgdp,dfgdp,ivgtyp,lai,wspd,qc_curr,            &
               dtbl,rho,dz8w,grav_settling,nlcat,                  &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               its,ite, jts,jte, kts,kte                           &
                                                                   )

















  IMPLICIT NONE








































   INTEGER, INTENT(IN)                       :: ims,ime,jms,jme,kms,kme &
                                               ,its,ite,jts,jte,kts,kte &
                                               ,ids,ide,jds,jde,kds,kde

   INTEGER, INTENT(IN)                       :: grav_settling,nlcat

   INTEGER,DIMENSION( ims:ime , jms:jme ),INTENT(INOUT)       :: ivgtyp

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                        &
                                       INTENT(IN),OPTIONAL    :: qc_curr
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                        &
                                       INTENT(IN)             :: rho
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                        &
                                       INTENT(IN   )          :: dz8w

   REAL, DIMENSION( ims:ime, jms:jme ),INTENT(INOUT),OPTIONAL :: vdfg
   REAL, DIMENSION( ims:ime, jms:jme ),INTENT(INOUT),OPTIONAL :: fgdp
   REAL, DIMENSION( ims:ime, jms:jme ),INTENT(INOUT),OPTIONAL :: dfgdp
   REAL, DIMENSION( ims:ime, jms:jme ),INTENT(INOUT),OPTIONAL :: lai
   REAL, DIMENSION( ims:ime, jms:jme ),INTENT(INOUT)          :: wspd

   REAL, INTENT(INOUT),OPTIONAL                               :: dtbl



   REAL,parameter :: gpw2=0.66666666666667



   INTEGER :: i,j
   REAL    :: lad, spcfct, vegh, ftmp1, ftmp2, dp_fog, lwc
   CHARACTER (LEN=25) :: land_use_type, lu_fogdes



   IF     ((nlcat .eq. 20).or.(nlcat .eq. 21)) THEN 
     land_use_type = 'MODIS'
   ELSEIF ((nlcat .eq. 24).or.(nlcat .eq. 28)) THEN 
     land_use_type = 'USGS'
   ELSE
     PRINT *, 'Unknown landuse category (sf_fogdes.F): num_land_cat=',nlcat
     STOP
   END IF

   DO j=jts,jte
    DO i=its,ite
       lwc = rho(i,kts,j)*qc_curr(i,kts,j)

       IF ( grav_settling .eq. 2 ) THEN

        IF (land_use_type .eq. 'USGS') THEN
         IF(  (ivgtyp(i,j) .ge.  2 .and. ivgtyp(i,j) .le. 15)           &
         .or. (ivgtyp(i,j) .ge. 17 .and. ivgtyp(i,j) .le. 18)           &
         .or. (ivgtyp(i,j) .ge. 20 .and. ivgtyp(i,j) .le. 22) ) THEN
          IF    ((ivgtyp(i,j).ge. 2 .and. ivgtyp(i,j).le. 5)            &
            .or. (ivgtyp(i,j).eq. 7)                                    &
            .or. (ivgtyp(i,j).eq. 17)                                   &
            .or. (ivgtyp(i,j).eq. 20)                         ) THEN
            lu_fogdes= 'CROP_GRASS'
          ELSEIF((ivgtyp(i,j).eq. 6) .or. (ivgtyp(i,j).eq. 9) ) THEN
            lu_fogdes= 'MIXED_CROP_GRASS_WOOD'
          ELSEIF( ivgtyp(i,j).eq. 8                           ) THEN
            lu_fogdes= 'SHRUB'
          ELSEIF((ivgtyp(i,j).eq.11) .or. (ivgtyp(i,j).eq.13) ) THEN
            lu_fogdes= 'BROAD_FOREST'
          ELSEIF((ivgtyp(i,j).eq.15) .or. (ivgtyp(i,j).eq.22) ) THEN
            lu_fogdes= 'MIXED_FOREST'
          ELSE
            lu_fogdes= 'CONIFER_FOREST_ETC'
          ENDIF
         ELSE
            lu_fogdes= 'OTHERS'
         ENDIF
        ELSE

         IF(  (ivgtyp(i,j) .ge.  1 .and. ivgtyp(i,j) .le. 10)           &
         .or. (ivgtyp(i,j) .eq. 12)                                     &
         .or. (ivgtyp(i,j) .eq. 14)                                     &
         .or. (ivgtyp(i,j) .ge. 18 .and. ivgtyp(i,j) .le. 19) ) THEN
          IF    ((ivgtyp(i,j).eq.10) .or. (ivgtyp(i,j).eq.12) ) THEN
            lu_fogdes= 'CROP_GRASS'
          ELSEIF( ivgtyp(i,j).eq.14                           ) THEN
            lu_fogdes= 'MIXED_CROP_GRASS_WOOD'
          ELSEIF((ivgtyp(i,j).eq. 6) .or. (ivgtyp(i,j).eq. 7) ) THEN
            lu_fogdes= 'SHRUB'
          ELSEIF((ivgtyp(i,j).eq. 2) .or. (ivgtyp(i,j).eq. 4) ) THEN
            lu_fogdes= 'BROAD_FOREST'
          ELSEIF((ivgtyp(i,j).eq. 5) .or. (ivgtyp(i,j).eq.19) ) THEN
            lu_fogdes= 'MIXED_FOREST'
          ELSE
            lu_fogdes= 'CONIFER_FOREST_ETC'
          ENDIF
         ELSE
            lu_fogdes= 'OTHERS'
         ENDIF
        ENDIF







        IF    ( lu_fogdes .eq. 'OTHERS'                ) THEN
         dp_fog= (17.03*lwc*1.e3 + 9.72)*1.e-6 
         vdfg(i,j)= (rhowater-rho(i,kts,j))*dp_fog**2.0*g/(18.0*myu)
        ELSE
         lu_select: SELECT CASE(lu_fogdes)
         CASE ('CROP_GRASS')
           spcfct= 0.2170
           vegh  = 3.0               
         CASE ('MIXED_CROP_GRASS_WOOD')
           spcfct= ( 1.0 + 0.2170 )/2.0
           vegh  = (20.0 + 3.0    )/2.0
         CASE ('SHRUB')
           spcfct= 1.0
           vegh  = 4.0
         CASE ('BROAD_FOREST')
           spcfct= 0.8255
           vegh  = 20.0
         CASE ('MIXED_FOREST')
           spcfct= ( 1.0 + 0.8255 )/2.0
           vegh  = 20.0
         CASE ('CONIFER_FOREST_ETC')
           spcfct= 1.0
           vegh  = 20.0
         END SELECT lu_select
 



         lad  = lai(i,j)/vegh
         ftmp1= 0.0164*lad**(-0.5000 )             
         ftmp2= 0.0095*lai(i,j)**3.0 - 0.05*lai(i,j)**2.0             &
              + 0.0916*lai(i,j) + 0.0082               
         vdfg(i,j)= spcfct*MIN( ftmp1, ftmp2 )*wspd(i,j)
        ENDIF



       ELSE IF (grav_settling .eq. 0 ) THEN
          
          vdfg(i,j) = 0.0
       ELSE IF (grav_settling .eq. 1 ) THEN
          
          
          IF ((qc_curr(i,kts,j)/(1.+qc_curr(i,kts,j))) > qcgmin) THEN
             vdfg(i,j)=gno*(qc_curr(i,kts,j)/(1.+qc_curr(i,kts,j)))**gpw2
          ELSE
             vdfg(i,j)=0.
          ENDIF
       ENDIF



       vdfg(i,j)=MIN( 0.5*dz8w(i,kts,j)/dtbl, vdfg(i,j) )

       IF ( PRESENT( fgdp ) ) THEN
         dfgdp(i,j)= vdfg(i,j)*lwc*dtbl
         fgdp(i,j) = fgdp(i,j)+dfgdp(i,j)
       ELSE
         CALL wrf_error_fatal3("<stdin>",240,&
'Missing arguments for FGDP in sf_fogdes')
       ENDIF

       dfgdp(i,j)= MAX (dfgdp(i,j), 0.0)
       fgdp(i,j) = MAX (fgdp(i,j),  0.0)

     ENDDO
   ENDDO

  END SUBROUTINE sf_fogdes



END MODULE module_sf_fogdes
