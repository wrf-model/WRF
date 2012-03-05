MODULE module_init_utilities

CONTAINS

 real function interp_0( v_in,  &
                         z_in, z_out, nz_in  )
 implicit none
 integer nz_in, nz_out
 real    v_in(nz_in), z_in(nz_in)
 real    z_out

 integer kp, k, im, ip
 logical interp, increasing_z 
 real    height, w1, w2
 logical debug
 parameter ( debug = .false. )

! does vertical coordinate increase or decrease with increasing k?
! set offset appropriately

 height = z_out

 if(debug) write(6,*) ' height in interp_0 ',height

 if (z_in(nz_in) .gt. z_in(1)) then

    if(debug) write(6,*) ' monotonic increase in z in interp_0 '
    IF (height > z_in(nz_in)) then
      if(debug) write(6,*) ' point 1 in interp_0 '
      w2 = (z_in(nz_in)-height)/(z_in(nz_in)-z_in(nz_in-1))
      w1 = 1.-w2
      interp_0 = w1*v_in(nz_in) + w2*v_in(nz_in-1)
    ELSE IF (height < z_in(1)) then
      if(debug) write(6,*) ' point 2 in interp_0 '
      w2 = (z_in(2)-height)/(z_in(2)-z_in(1))
      w1 = 1.-w2
      interp_0 = w1*v_in(2) + w2*v_in(1)
    ELSE
      if(debug) write(6,*) ' point 3 in interp_0 '
      interp = .false.
      kp = nz_in
      DO WHILE ( (interp .eqv. .false.) .and. (kp .ge. 2) )
        IF(   ((z_in(kp)   .ge. height) .and.     &
               (z_in(kp-1) .le. height))        )   THEN
          w2 = (height-z_in(kp))/(z_in(kp-1)-z_in(kp))
          w1 = 1.-w2
          interp_0 = w1*v_in(kp) + w2*v_in(kp-1)
          if(debug) write(6,*) ' interp data, kp, w1, w2 ',kp, w1, w2
          if(debug) write(6,*) ' interp data, v_in(kp), v_in(kp-1), interp_0 ', &
                     v_in(kp), v_in(kp-1), interp_0
          interp = .true.
        END IF
        kp = kp-1
      ENDDO
    ENDIF

 else

    if(debug) write(6,*) ' monotonic decrease in z in interp_0 '

    IF (height < z_in(nz_in)) then
      if(debug) write(6,*) ' point 1 in interp_0 '
      w2 = (z_in(nz_in)-height)/(z_in(nz_in)-z_in(nz_in-1))
      w1 = 1.-w2
      interp_0 = w1*v_in(nz_in) + w2*v_in(nz_in-1)
    ELSE IF (height > z_in(1)) then
      if(debug) write(6,*) ' point 2 in interp_0 '
      w2 = (z_in(2)-height)/(z_in(2)-z_in(1))
      w1 = 1.-w2
      interp_0 = w1*v_in(2) + w2*v_in(1)
    ELSE
      if(debug) write(6,*) ' point 3 in interp_0 '
      interp = .false.
      kp = nz_in
      height = z_out
      DO WHILE ( (interp .eqv. .false.) .and. (kp .ge. 2) )
        IF(   ((z_in(kp)   .le. height) .and.     &
               (z_in(kp-1) .ge. height))             )   THEN
          w2 = (height-z_in(kp))/(z_in(kp-1)-z_in(kp))
          w1 = 1.-w2
          interp_0 = w1*v_in(kp) + w2*v_in(kp-1)
          interp = .true.
        END IF
        kp = kp-1
      ENDDO
    ENDIF

 end if

 return
 END FUNCTION interp_0

END MODULE module_init_utilities


