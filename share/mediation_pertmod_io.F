!
!WRF:MEDIATION_LAYER:IO
!
MODULE mediation_pertmod_io

USE module_domain, ONLY : domain, head_grid, get_ijk_from_grid
USE module_state_description, ONLY : PARAM_FIRST_SCALAR, num_moist, &
    P_A_QV, P_A_QC, P_A_QR, P_A_QI, P_A_QS, P_A_QH, P_A_QG

TYPE pertmod_io
    CHARACTER*256 :: time                       ! time stamp
    REAL, ALLOCATABLE, DIMENSION(:)  :: data    ! data
    TYPE (pertmod_io), POINTER :: next          ! pointer to the next node
    TYPE (pertmod_io), POINTER :: prev          ! pointer to the previous node
END TYPE pertmod_io

TYPE (pertmod_io), POINTER :: xtraj_head, xtraj_tail
TYPE (pertmod_io), POINTER :: xtraj_pointer

TYPE ad_forcing_list
    CHARACTER*256 :: time                       ! time stamp
    REAL, ALLOCATABLE, DIMENSION(:)  :: data    ! data
    TYPE (ad_forcing_list), POINTER :: next     ! pointer to the next node
    TYPE (ad_forcing_list), POINTER :: prev     ! pointer to the previous node
END TYPE ad_forcing_list

TYPE (ad_forcing_list), POINTER :: ad_forcing_head, ad_forcing_tail, tl_pert_head

INTEGER :: bytes_ad_forcing , bytes_xtraj, n3d, n2d, n1d, nsd, nbd

INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                   ims , ime , jms , jme , kms , kme , &
                                   ips , ipe , jps , jpe , kps , kpe

CONTAINS

SUBROUTINE adtl_initialize

   IMPLICIT NONE

   TYPE (ad_forcing_list), POINTER :: adtl_current
   INTEGER :: i

   ! 3D variables: a_u, a_v, a_w, a_t, a_ph
   bytes_ad_forcing = n3d*5
   ! 2D variables: a_mu
   bytes_ad_forcing = bytes_ad_forcing + n2d
   ! Moist variables
   DO i = PARAM_FIRST_SCALAR, num_moist
      IF ( i .eq. P_A_QV ) &
         bytes_ad_forcing = bytes_ad_forcing + n3d
      IF ( i .eq. P_A_QC ) &
         bytes_ad_forcing = bytes_ad_forcing + n3d
      IF ( i .eq. P_A_QR ) &
         bytes_ad_forcing = bytes_ad_forcing + n3d
      IF ( i .eq. P_A_QI ) &
         bytes_ad_forcing = bytes_ad_forcing + n3d
      IF ( i .eq. P_A_QS ) &
         bytes_ad_forcing = bytes_ad_forcing + n3d
      IF ( i .eq. P_A_QG ) &
         bytes_ad_forcing = bytes_ad_forcing + n3d
      IF ( i .eq. P_A_QH ) &
         bytes_ad_forcing = bytes_ad_forcing + n3d
   ENDDO

   adtl_current => ad_forcing_head

   DO WHILE ( ASSOCIATED (adtl_current) )
       ad_forcing_head => adtl_current%next
       DEALLOCATE ( adtl_current%data )
       DEALLOCATE ( adtl_current )
       adtl_current => ad_forcing_head
   ENDDO

   NULLIFY (ad_forcing_head)

   CALL wrf_debug ( 1 , 'ad_forcing linked list is initialized' )

   adtl_current => tl_pert_head

   DO WHILE ( ASSOCIATED (adtl_current) )
       tl_pert_head => adtl_current%next
       DEALLOCATE ( adtl_current%data )
       DEALLOCATE ( adtl_current )
       adtl_current => tl_pert_head
   ENDDO

   NULLIFY (tl_pert_head)

   CALL wrf_debug ( 1 , 'tl_pert linked list is initialized' )

END SUBROUTINE adtl_initialize

SUBROUTINE save_tl_pert ( time )

   IMPLICIT NONE

   CHARACTER*256, INTENT(IN) :: time

   TYPE (ad_forcing_list), POINTER :: tl_pert_current
   CHARACTER*256 mess

   IF ( .NOT. ASSOCIATED(tl_pert_head) ) THEN
       NULLIFY (tl_pert_head)

       ALLOCATE (tl_pert_head)
       ALLOCATE (tl_pert_head%data(bytes_ad_forcing))
       NULLIFY (tl_pert_head%next)

       tl_pert_head%time = TRIM(time)
       call packup_ad_forcing (tl_pert_head%data)

   ELSE
       ALLOCATE (tl_pert_current)
       ALLOCATE (tl_pert_current%data(bytes_ad_forcing))
       NULLIFY (tl_pert_current%next)
       tl_pert_current%time = TRIM(time)
       call packup_ad_forcing ( tl_pert_current%data)
       tl_pert_current%next => tl_pert_head
       tl_pert_head => tl_pert_current
   ENDIF

   WRITE(mess, FMT='(A,A)') 'Push tl. perturbation time_stamp:', TRIM(tl_pert_head%time)
   CALL wrf_debug ( 0 , mess )

END SUBROUTINE save_tl_pert

SUBROUTINE save_ad_forcing ( time )

   IMPLICIT NONE

   CHARACTER*256, INTENT(IN) :: time

   TYPE (ad_forcing_list), POINTER :: ad_forcing_current
   CHARACTER*256 mess

   IF ( .NOT. ASSOCIATED(ad_forcing_head) ) THEN
       NULLIFY (ad_forcing_head)

       ALLOCATE (ad_forcing_head)
       ALLOCATE (ad_forcing_head%data(bytes_ad_forcing))
       NULLIFY (ad_forcing_head%next)

       ad_forcing_head%time = TRIM(time)
       call packup_ad_forcing (ad_forcing_head%data)

       ad_forcing_tail => ad_forcing_head

   ELSE
       ALLOCATE (ad_forcing_current)
       ALLOCATE (ad_forcing_current%data(bytes_ad_forcing))
       NULLIFY (ad_forcing_current%next)
       NULLIFY (ad_forcing_current%prev)
       ad_forcing_current%time = TRIM(time)
       call packup_ad_forcing ( ad_forcing_current%data)
       ad_forcing_current%next => ad_forcing_head
       ad_forcing_head%prev => ad_forcing_current
       ad_forcing_head => ad_forcing_current
   ENDIF

   WRITE(mess, FMT='(A,A)') 'Push ad. forcing time_stamp:', TRIM(ad_forcing_head%time)
   CALL wrf_debug ( 0 , mess )

END SUBROUTINE save_ad_forcing

SUBROUTINE swap_ad_forcing (numberOfRun)

   IMPLICIT NONE

   INTEGER, INTENT(IN) :: numberOfRun

   CHARACTER*256 time, mess
   REAL, ALLOCATABLE, DIMENSION(:)  :: data
   TYPE (ad_forcing_list), POINTER :: firstNode, secondNode
   INTEGER :: n

   ALLOCATE(data(bytes_ad_forcing))
   
   firstNode => ad_forcing_head
   secondNode => ad_forcing_tail

   DO n = 1, numberOfRun
      WRITE(mess, FMT='(6a)') "Swap time: <", TRIM(firstNode%time), ">", "and: <", TRIM(secondNode%time), ">"
      CALL wrf_message ( mess )
      time = firstNode%time
      data = firstNode%data
      firstNode%time = secondNode%time 
      firstNode%data = secondNode%data 
      secondNode%time = time
      secondNode%data = data

      IF ( n .LT. numberOfRun ) THEN
         firstNode => firstNode%next
         secondNode => secondNode%prev
      ENDIF
   ENDDO

   DEALLOCATE(data)
   NULLIFY(firstNode)
   NULLIFY(secondNode)

END SUBROUTINE swap_ad_forcing

SUBROUTINE read_tl_pert ( time )

   IMPLICIT NONE

   CHARACTER*256, INTENT(IN) :: time

   TYPE (ad_forcing_list), POINTER :: tl_pert_current
   CHARACTER*256 ::  mess

   tl_pert_current => tl_pert_head

   IF (TRIM(tl_pert_current%time) .NE. TRIM(time)) THEN
      WRITE(mess, FMT='(6a)') "Want time: <", TRIM(time), ">", "But time in list is: <", TRIM(tl_pert_current%time), ">"
      CALL wrf_message ( mess )
      RETURN
   endif

   WRITE(mess, FMT='(A,A)') &
         'read tl. perturbation time stamp:', TRIM(tl_pert_current%time)
   CALL wrf_debug ( 0 , mess )

   call restore_ad_forcing (tl_pert_current%data)

   tl_pert_current => tl_pert_current%next

   NULLIFY(tl_pert_head%next)
   DEALLOCATE(tl_pert_head%data)
   DEALLOCATE(tl_pert_head)
   tl_pert_head => tl_pert_current

END SUBROUTINE read_tl_pert

SUBROUTINE read_ad_forcing ( time )

   IMPLICIT NONE

   CHARACTER*256, INTENT(IN) :: time

   TYPE (ad_forcing_list), POINTER :: ad_forcing_current
   CHARACTER*256 ::  mess

   ad_forcing_current => ad_forcing_head

   IF (TRIM(ad_forcing_current%time) .NE. TRIM(time)) THEN
      WRITE(mess, FMT='(6a)') "Want time: <", TRIM(time), ">", "But time in list is: <", TRIM(ad_forcing_current%time), ">"
      CALL wrf_message ( mess )
      RETURN
   endif

   WRITE(mess, FMT='(A,A)') &
         'read ad. forcing time stamp:', TRIM(ad_forcing_current%time)
   CALL wrf_debug ( 0 , mess )

   call restore_ad_forcing (ad_forcing_current%data)

   ad_forcing_current => ad_forcing_current%next

   NULLIFY(ad_forcing_head%next)
   DEALLOCATE(ad_forcing_head%data)
   DEALLOCATE(ad_forcing_head)
   ad_forcing_head => ad_forcing_current

END SUBROUTINE read_ad_forcing

SUBROUTINE xtraj_io_initialize

   IMPLICIT NONE

   TYPE (pertmod_io), POINTER :: current

   INTEGER :: i

   CALL get_ijk_from_grid (  head_grid ,                   &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe )

   ! Calculate how many bytes are needed to store the ad_forcing

   n3d = (ime-ims+1)*(jme-jms+1)*(kme-kms+1)
   n2d = (ime-ims+1)*(jme-jms+1)
   n1d = (kme-kms+1)
   nsd = head_grid%num_soil_layers
   nbd = head_grid%spec_bdy_width

   ! 3D variables: u_2, v_2, w_2, t_2, ph_2, p, al, alt, alb, phb, pb
   bytes_xtraj = n3d*11

   ! 2D variables: mu_2, lu_index, q2, t2, th2, u10, v10, landmask, xice, ivgtyp, isltyp,
   !               vegfra, snow, snowh, canwt, sst, msft, msfu, msfv, f, e, sina, cosa,
   !               ht, tsk, xlat, xlong, albbck, tmn, xland, znt, mub, psfc, snowc
   bytes_xtraj = bytes_xtraj + n2d*34

   ! 1D K variables: znu, znw, fnm, fnp, rdnw, rdn, dnw, dn
   bytes_xtraj = bytes_xtraj + n1d*8

   ! 1D L variables: zs, dzs
   bytes_xtraj = bytes_xtraj + nsd*2

   ! 3D L variables: tslb, smois, sh2o, smcrel
   bytes_xtraj = bytes_xtraj + n2d*nsd*4

   ! scalar : cfn, cfn1, rdx, rdy, dts, dtseps, resm, zetatop, cf1, cf2, cf3, dtbc
   bytes_xtraj = bytes_xtraj + 12

   ! bdy variables: fcx, gcx
   bytes_xtraj = bytes_xtraj + nbd*2
   
   ! Moist variables
   DO i = PARAM_FIRST_SCALAR, num_moist
      IF ( i .eq. P_A_QV ) &
         bytes_xtraj = bytes_xtraj + n3d
      IF ( i .eq. P_A_QC ) &
         bytes_xtraj = bytes_xtraj + n3d
      IF ( i .eq. P_A_QR ) &
         bytes_xtraj = bytes_xtraj + n3d
      IF ( i .eq. P_A_QI ) &
         bytes_xtraj = bytes_xtraj + n3d
      IF ( i .eq. P_A_QS ) &
         bytes_xtraj = bytes_xtraj + n3d
      IF ( i .eq. P_A_QG ) &
         bytes_xtraj = bytes_xtraj + n3d
      IF ( i .eq. P_A_QH ) &
         bytes_xtraj = bytes_xtraj + n3d
   ENDDO

   current => xtraj_head

   DO WHILE ( ASSOCIATED (current) )
       xtraj_head => current%next
       DEALLOCATE ( current%data )
       DEALLOCATE ( current )
       current => xtraj_head
       xtraj_pointer => xtraj_head
   ENDDO

   NULLIFY (xtraj_head)

   CALL wrf_debug ( 1 , 'xtraj linked list is initialized' )

END SUBROUTINE xtraj_io_initialize

SUBROUTINE save_xtraj ( time )

   IMPLICIT NONE

   CHARACTER*256, INTENT(IN) :: time

   TYPE (pertmod_io), POINTER :: current
   CHARACTER*256 mess

   IF ( .NOT. ASSOCIATED(xtraj_head) ) THEN
       NULLIFY (xtraj_head)

       ALLOCATE (xtraj_head)
       ALLOCATE (xtraj_head%data(bytes_xtraj))
       NULLIFY (xtraj_head%next)

       xtraj_head%time = TRIM(time)
       call packup_xtraj ( xtraj_head%data )

       xtraj_tail => xtraj_head
       xtraj_pointer => xtraj_head
   ELSE
       ALLOCATE (current)
       ALLOCATE (current%data(bytes_xtraj))
       NULLIFY (current%next)
       NULLIFY (current%prev)
       current%time = TRIM(time)
       call packup_xtraj ( current%data )
       current%next => xtraj_head
       xtraj_head%prev => current 
       xtraj_head => current
       xtraj_pointer => current
   ENDIF

   WRITE(mess, FMT='(A,A)') 'Push xtraj time_stamp:', TRIM(xtraj_head%time)
   CALL wrf_debug ( 0 , mess )

END SUBROUTINE save_xtraj

SUBROUTINE read_xtraj ( time )

   IMPLICIT NONE

   CHARACTER*256, INTENT(IN) :: time

   CHARACTER*256 ::  mess

   IF (TRIM(xtraj_pointer%time) .NE. TRIM(time)) THEN
      WRITE(mess, FMT='(6a)') "Want time: <", TRIM(time), ">", "But time in list is: <", TRIM(xtraj_pointer%time), ">"
      CALL wrf_message ( mess )
      RETURN
   ENDIF

   WRITE(mess, FMT='(A,A)') &
         'read xtraj time stamp:', TRIM(xtraj_pointer%time)
   CALL wrf_debug ( 0 , mess )

   CALL restore_xtraj (xtraj_pointer%data)

   IF ( ASSOCIATED(xtraj_pointer%next) ) xtraj_pointer => xtraj_pointer%next

END SUBROUTINE read_xtraj

SUBROUTINE read_xtraj_reverse ( time )

   IMPLICIT NONE

   CHARACTER*256, INTENT(IN) :: time

   CHARACTER*256 ::  mess

   IF (TRIM(xtraj_pointer%time) .NE. TRIM(time)) THEN
      WRITE(mess, FMT='(A,A,A,A,A,A)') "Want time: <", TRIM(time), ">", "But time in list is: <", TRIM(xtraj_pointer%time), ">"
      CALL wrf_message ( mess )
      RETURN
   endif

   WRITE(mess, FMT='(A,A)') &
         'read time stamp:', TRIM(xtraj_pointer%time)
   CALL wrf_debug ( 0 , mess )

   CALL restore_xtraj (xtraj_pointer%data)

   IF ( ASSOCIATED(xtraj_pointer%prev) ) xtraj_pointer => xtraj_pointer%prev

END SUBROUTINE read_xtraj_reverse

SUBROUTINE read_nl_xtraj ( time )

   IMPLICIT NONE

   CHARACTER*256, INTENT(IN) :: time

   CHARACTER*256 ::  mess

   xtraj_pointer => xtraj_head

   DO WHILE ( ASSOCIATED(xtraj_pointer) )

      IF (TRIM(xtraj_pointer%time) .NE. TRIM(time)) THEN
         xtraj_pointer => xtraj_pointer%next
         CYCLE
      ENDIF

      WRITE(mess, FMT='(A,A)') &
         'read nonlinear xtraj time stamp:', TRIM(xtraj_pointer%time)
      CALL wrf_debug ( 0 , mess )

      CALL restore_xtraj (xtraj_pointer%data)

      RETURN

   ENDDO

   WRITE(mess, FMT='(A,A)') &
       'Can not find nonlinear xtraj time stamp:', TRIM(xtraj_pointer%time)
   CALL wrf_error_fatal ( mess )

END SUBROUTINE read_nl_xtraj

SUBROUTINE packup_ad_forcing (data)

   IMPLICIT NONE

   REAL, DIMENSION(:), INTENT(OUT) :: data

   INTEGER :: ns, ne, i
   
   ns =       1 ; ne =    n3d ; data(ns:ne) = RESHAPE( head_grid%g_u_2, (/n3d/))
   ns =    ne+1 ; ne = ne+n3d ; data(ns:ne) = RESHAPE( head_grid%g_v_2, (/n3d/))
   ns =    ne+1 ; ne = ne+n3d ; data(ns:ne) = RESHAPE( head_grid%g_w_2, (/n3d/))
   ns =    ne+1 ; ne = ne+n3d ; data(ns:ne) = RESHAPE( head_grid%g_t_2, (/n3d/))
   ns =    ne+1 ; ne = ne+n3d ; data(ns:ne) = RESHAPE( head_grid%g_ph_2, (/n3d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%g_mu_2, (/n2d/))

   DO i = PARAM_FIRST_SCALAR, num_moist
      ns =    ne+1 ; ne = ne+n3d ; data(ns:ne) = RESHAPE( head_grid%g_moist(:,:,:,i), (/n3d/))
   ENDDO
 
   IF ( ne .NE. bytes_ad_forcing ) &
      CALL wrf_error_fatal ( 'packup_ad_forcing:  ne is not equal to bytes_ad_forcing' )

END SUBROUTINE packup_ad_forcing

SUBROUTINE restore_ad_forcing (data)

   !
   !   Use perturbation variables to store the adjoint forcing temporarily.
   !
   IMPLICIT NONE

   REAL, DIMENSION(:), INTENT(OUT) :: data

   INTEGER :: ns, ne, i

   ns =       1 ; ne =    n3d ; head_grid%g_u_2 = RESHAPE( data(ns:ne), (/ime-ims+1,kme-kms+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n3d ; head_grid%g_v_2 = RESHAPE( data(ns:ne), (/ime-ims+1,kme-kms+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n3d ; head_grid%g_w_2 = RESHAPE( data(ns:ne), (/ime-ims+1,kme-kms+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n3d ; head_grid%g_t_2 = RESHAPE( data(ns:ne), (/ime-ims+1,kme-kms+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n3d ; head_grid%g_ph_2 = RESHAPE( data(ns:ne), (/ime-ims+1,kme-kms+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%g_mu_2 = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))

   DO i = PARAM_FIRST_SCALAR, num_moist
      ns =    ne+1 ; ne = ne+n3d ; head_grid%g_moist(:,:,:,i) = RESHAPE( data(ns:ne), (/ime-ims+1,kme-kms+1,jme-jms+1/))
   ENDDO

   IF ( ne .NE. bytes_ad_forcing ) &
      CALL wrf_error_fatal ( 'restore_ad_forcing:  ne is not equal to bytes_ad_forcing' )

END SUBROUTINE restore_ad_forcing

SUBROUTINE packup_xtraj (data)

   IMPLICIT NONE

   REAL, DIMENSION(:), INTENT (OUT) :: data
 
   INTEGER :: ns, ne, n

   ! 3D variables: u_2, v_2, w_2, t_2, ph_2, p, al, alt, alb, phb, pb
   ns =       1 ; ne =    n3d ; data(ns:ne) = RESHAPE( head_grid%u_2, (/n3d/))
   ns =    ne+1 ; ne = ne+n3d ; data(ns:ne) = RESHAPE( head_grid%v_2, (/n3d/))
   ns =    ne+1 ; ne = ne+n3d ; data(ns:ne) = RESHAPE( head_grid%w_2, (/n3d/))
   ns =    ne+1 ; ne = ne+n3d ; data(ns:ne) = RESHAPE( head_grid%t_2, (/n3d/))
   ns =    ne+1 ; ne = ne+n3d ; data(ns:ne) = RESHAPE( head_grid%ph_2, (/n3d/))
   ns =    ne+1 ; ne = ne+n3d ; data(ns:ne) = RESHAPE( head_grid%p, (/n3d/))
   ns =    ne+1 ; ne = ne+n3d ; data(ns:ne) = RESHAPE( head_grid%al, (/n3d/))
   ns =    ne+1 ; ne = ne+n3d ; data(ns:ne) = RESHAPE( head_grid%alt, (/n3d/))
   ns =    ne+1 ; ne = ne+n3d ; data(ns:ne) = RESHAPE( head_grid%alb, (/n3d/))
   ns =    ne+1 ; ne = ne+n3d ; data(ns:ne) = RESHAPE( head_grid%phb, (/n3d/))
   ns =    ne+1 ; ne = ne+n3d ; data(ns:ne) = RESHAPE( head_grid%pb, (/n3d/))

   ! 2D variables: mu_2, lu_index, q2, t2, th2, u10, v10, landmask, xice, ivgtyp, isltyp,
   !               vegfra, snow, snowh, canwat, sst, msft, msfu, msfv, f, e, sina, cosa,
   !               ht, tsk, xlat, xlong, albbck, tmn, xland, znt, mub, psfc, snowc
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%mu_2, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%lu_index, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%q2, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%t2, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%th2, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%u10, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%v10, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%landmask, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%xice, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%ivgtyp, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%isltyp, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%vegfra, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%snow, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%snowh, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%canwat, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%sst, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%msft, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%msfu, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%msfv, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%f, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%e, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%sina, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%cosa, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%ht, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%tsk, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%xlat, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%xlong, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%albbck, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%tmn, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%xland, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%znt, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%mub, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%psfc, (/n2d/))
   ns =    ne+1 ; ne = ne+n2d ; data(ns:ne) = RESHAPE( head_grid%snowc, (/n2d/))

   ! 1D K variables: znu, znw, fnm, fnp, rdnw, rdn, dnw, dn
   ns =    ne+1 ; ne = ne+n1d ; data(ns:ne) = head_grid%znu
   ns =    ne+1 ; ne = ne+n1d ; data(ns:ne) = head_grid%znw
   ns =    ne+1 ; ne = ne+n1d ; data(ns:ne) = head_grid%fnm
   ns =    ne+1 ; ne = ne+n1d ; data(ns:ne) = head_grid%fnp
   ns =    ne+1 ; ne = ne+n1d ; data(ns:ne) = head_grid%rdnw
   ns =    ne+1 ; ne = ne+n1d ; data(ns:ne) = head_grid%rdn
   ns =    ne+1 ; ne = ne+n1d ; data(ns:ne) = head_grid%dnw
   ns =    ne+1 ; ne = ne+n1d ; data(ns:ne) = head_grid%dn

   ! 1D L variables: zs, dzs
   ns =    ne+1 ; ne = ne+nsd ; data(ns:ne) = head_grid%zs
   ns =    ne+1 ; ne = ne+nsd ; data(ns:ne) = head_grid%dzs

   ! 3D L variables: tslb, smois, sh2o, smcrel
   ns =    ne+1 ; ne = ne+n2d*nsd ; data(ns:ne) = RESHAPE( head_grid%tslb, (/n2d*nsd/))
   ns =    ne+1 ; ne = ne+n2d*nsd ; data(ns:ne) = RESHAPE( head_grid%smois, (/n2d*nsd/))
   ns =    ne+1 ; ne = ne+n2d*nsd ; data(ns:ne) = RESHAPE( head_grid%sh2o, (/n2d*nsd/))
   ns =    ne+1 ; ne = ne+n2d*nsd ; data(ns:ne) = RESHAPE( head_grid%smcrel, (/n2d*nsd/))

   ! scalar : cfn, cfn1, rdx, rdy, dts, dtseps, resm, zetatop, cf1, cf2, cf3, dtbc
   ns =    ne+1 ; ne = ne+1 ; data(ns:ne) = head_grid%cfn
   ns =    ne+1 ; ne = ne+1 ; data(ns:ne) = head_grid%cfn1
   ns =    ne+1 ; ne = ne+1 ; data(ns:ne) = head_grid%rdx
   ns =    ne+1 ; ne = ne+1 ; data(ns:ne) = head_grid%rdy
   ns =    ne+1 ; ne = ne+1 ; data(ns:ne) = head_grid%dts
   ns =    ne+1 ; ne = ne+1 ; data(ns:ne) = head_grid%dtseps
   ns =    ne+1 ; ne = ne+1 ; data(ns:ne) = head_grid%resm
   ns =    ne+1 ; ne = ne+1 ; data(ns:ne) = head_grid%zetatop
   ns =    ne+1 ; ne = ne+1 ; data(ns:ne) = head_grid%cf1
   ns =    ne+1 ; ne = ne+1 ; data(ns:ne) = head_grid%cf2
   ns =    ne+1 ; ne = ne+1 ; data(ns:ne) = head_grid%cf3
   ns =    ne+1 ; ne = ne+1 ; data(ns:ne) = head_grid%dtbc

   ! bdy variables: fcx, gcx
   ns =    ne+1 ; ne = ne+nbd ; data(ns:ne) = head_grid%fcx
   ns =    ne+1 ; ne = ne+nbd ; data(ns:ne) = head_grid%gcx

   DO n = PARAM_FIRST_SCALAR, num_moist
      ns =    ne+1 ; ne = ne+n3d ; data(ns:ne) = RESHAPE( head_grid%moist(:,:,:,n), (/n3d/))
   ENDDO

   IF ( ne .NE. bytes_xtraj ) &
      CALL wrf_error_fatal ( 'packup_xtraj:  ne is not equal to bytes_xtraj' )

END SUBROUTINE packup_xtraj

SUBROUTINE restore_xtraj (data)

   IMPLICIT NONE

   REAL, DIMENSION(:), INTENT (IN) :: data

   INTEGER :: ns, ne, n

   ! 3D variables: u_2, v_2, w_2, t_2, ph_2, p, al, alt, alb, phb, pb
   ns =       1 ; ne =    n3d ; head_grid%u_2 = RESHAPE( data(ns:ne), (/ime-ims+1,kme-kms+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n3d ; head_grid%v_2 = RESHAPE( data(ns:ne), (/ime-ims+1,kme-kms+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n3d ; head_grid%w_2 = RESHAPE( data(ns:ne), (/ime-ims+1,kme-kms+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n3d ; head_grid%t_2 = RESHAPE( data(ns:ne), (/ime-ims+1,kme-kms+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n3d ; head_grid%ph_2 = RESHAPE( data(ns:ne), (/ime-ims+1,kme-kms+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n3d ; head_grid%p = RESHAPE( data(ns:ne), (/ime-ims+1,kme-kms+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n3d ; head_grid%al = RESHAPE( data(ns:ne), (/ime-ims+1,kme-kms+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n3d ; head_grid%alt = RESHAPE( data(ns:ne), (/ime-ims+1,kme-kms+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n3d ; head_grid%alb = RESHAPE( data(ns:ne), (/ime-ims+1,kme-kms+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n3d ; head_grid%phb = RESHAPE( data(ns:ne), (/ime-ims+1,kme-kms+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n3d ; head_grid%pb = RESHAPE( data(ns:ne), (/ime-ims+1,kme-kms+1,jme-jms+1/))

   ! 2D variables: mu_2, lu_index, q2, t2, th2, u10, v10, landmask, xice, ivgtyp, isltyp,
   !               vegfra, snow, snowh, canwat, sst, msft, msfu, msfv, f, e, sina, cosa,
   !               ht, tsk, xlat, xlong, albbck, tmn, xland, znt, mub, psfc, snowc
   ns =    ne+1 ; ne = ne+n2d ; head_grid%mu_2 = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%lu_index = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%q2 = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%t2 = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%th2 = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%u10 = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%v10 = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%landmask = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%xice = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%ivgtyp = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%isltyp = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%vegfra = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%snow = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%snowh = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%canwat = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%sst = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%msft = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%msfu = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%msfv = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%f = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%e = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%sina = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%cosa = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%ht = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%tsk = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%xlat = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%xlong = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%albbck = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%tmn = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%xland = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%znt = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%mub = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%psfc = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d ; head_grid%snowc = RESHAPE( data(ns:ne), (/ime-ims+1,jme-jms+1/))

   ! 1D K variables: znu, znw, fnm, fnp, rdnw, rdn, dnw, dn
   ns =    ne+1 ; ne = ne+n1d ; head_grid%znu = data(ns:ne)
   ns =    ne+1 ; ne = ne+n1d ; head_grid%znw = data(ns:ne)
   ns =    ne+1 ; ne = ne+n1d ; head_grid%fnm = data(ns:ne)
   ns =    ne+1 ; ne = ne+n1d ; head_grid%fnp = data(ns:ne)
   ns =    ne+1 ; ne = ne+n1d ; head_grid%rdnw = data(ns:ne)
   ns =    ne+1 ; ne = ne+n1d ; head_grid%rdn = data(ns:ne)
   ns =    ne+1 ; ne = ne+n1d ; head_grid%dnw = data(ns:ne)
   ns =    ne+1 ; ne = ne+n1d ; head_grid%dn = data(ns:ne)

   ! 1D L variables: zs, dzs
   ns =    ne+1 ; ne = ne+nsd ; head_grid%zs = data(ns:ne)
   ns =    ne+1 ; ne = ne+nsd ; head_grid%dzs = data(ns:ne)

   ! 3D L variables: tslb, smois, sh2o, smcrel
   ns =    ne+1 ; ne = ne+n2d*nsd ; head_grid%tslb = RESHAPE( data(ns:ne), (/ime-ims+1,nsd,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d*nsd ; head_grid%smois = RESHAPE( data(ns:ne), (/ime-ims+1,nsd,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d*nsd ; head_grid%sh2o = RESHAPE( data(ns:ne), (/ime-ims+1,nsd,jme-jms+1/))
   ns =    ne+1 ; ne = ne+n2d*nsd ; head_grid%smcrel = RESHAPE( data(ns:ne), (/ime-ims+1,nsd,jme-jms+1/))

   ! scalar : cfn, cfn1, rdx, rdy, dts, dtseps, resm, zetatop, cf1, cf2, cf3, dtbc
   ns =    ne+1 ; ne = ne+1 ; head_grid%cfn = data(ns)
   ns =    ne+1 ; ne = ne+1 ; head_grid%cfn1 = data(ns)
   ns =    ne+1 ; ne = ne+1 ; head_grid%rdx = data(ns)
   ns =    ne+1 ; ne = ne+1 ; head_grid%rdy = data(ns)
   ns =    ne+1 ; ne = ne+1 ; head_grid%dts = data(ns)
   ns =    ne+1 ; ne = ne+1 ; head_grid%dtseps = data(ns)
   ns =    ne+1 ; ne = ne+1 ; head_grid%resm = data(ns)
   ns =    ne+1 ; ne = ne+1 ; head_grid%zetatop = data(ns)
   ns =    ne+1 ; ne = ne+1 ; head_grid%cf1 = data(ns)
   ns =    ne+1 ; ne = ne+1 ; head_grid%cf2 = data(ns)
   ns =    ne+1 ; ne = ne+1 ; head_grid%cf3 = data(ns)
   ns =    ne+1 ; ne = ne+1 ; head_grid%dtbc = data(ns)

   ! bdy variables: fcx, gcx
   ns =    ne+1 ; ne = ne+nbd ; head_grid%fcx = data(ns:ne)
   ns =    ne+1 ; ne = ne+nbd ; head_grid%gcx = data(ns:ne)

   DO n = PARAM_FIRST_SCALAR, num_moist
      ns =    ne+1 ; ne = ne+n3d ; head_grid%moist(:,:,:,n) = RESHAPE( data(ns:ne), (/ime-ims+1,kme-kms+1,jme-jms+1/))
   ENDDO

   IF ( ne .NE. bytes_xtraj ) &
      CALL wrf_error_fatal ( 'restore_xtraj:  ne is not equal to bytes_xtraj' )

END SUBROUTINE restore_xtraj

END MODULE mediation_pertmod_io

SUBROUTINE add_forcing_to_ad ( grid )
   USE module_domain, ONLY : domain

   IMPLICIT NONE

   TYPE(domain), INTENT(INOUT)  :: grid

   INTEGER :: n

   grid%a_u_2  = grid%a_u_2  + grid%g_u_2
   grid%a_v_2  = grid%a_v_2  + grid%g_v_2
   grid%a_w_2  = grid%a_w_2  + grid%g_w_2
   grid%a_ph_2 = grid%a_ph_2 + grid%g_ph_2
   grid%a_t_2  = grid%a_t_2  + grid%g_t_2
   grid%a_mu_2 = grid%a_mu_2 + grid%g_mu_2
   grid%a_moist = grid%a_moist + grid%g_moist

END SUBROUTINE add_forcing_to_ad

