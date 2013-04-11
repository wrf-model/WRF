MODULE module_positive_definite

  USE module_wrf_error      ! frame

CONTAINS

SUBROUTINE positive_definite_slab( f,                            &
                                   ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte)

  IMPLICIT NONE

  ! Arguments
  INTEGER, INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                            ims, ime, jms, jme, kms, kme, &
                            its, ite, jts, jte, kts, kte
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: f

  ! Local variables
  REAL, DIMENSION(:), ALLOCATABLE :: line
  INTEGER :: j, k, i_end, j_end, k_end
  REAL :: fmin, ftotal_pre, rftotal_post

  ! Initialize variables
  i_end = ide-1
  j_end = MIN(jte, jde-1)
  k_end = kte-1
  ! Only do anything if we have to...
  IF (ANY(f(ids:i_end,kts:k_end,jts:j_end) < 0.)) THEN
     ! number of points in the X direction, not including U-stagger
     ALLOCATE(line(ide-ids))
     DO j = jts, j_end
     DO k = kts, kte-1
        !while_lt_0_loop: DO WHILE (ANY(f(ids:i_end,k,j) < 0.))
        f_lt_0: IF (ANY(f(ids:i_end,k,j) < 0.)) THEN
           line(:) = f(ids:i_end,k,j)
           ! This is actually an integration over x assuming dx is constant
           ftotal_pre = SUM(line)
           ! If the total is negative, set everything to 0. and exit
           IF (ftotal_pre < 0.) THEN
              line(:) = 0.
           ELSE
              ! Value to add to array to make sure every element is > 0.
              fmin = MINVAL(line)
              line(:) = line(:) - fmin ! fmin is negative...
              rftotal_post = 1./SUM(line)
              line = line*ftotal_pre*rftotal_post
              ! The following error can naturally occur on 32-bit machines:
              !IF (SUM(line) /= ftotal_pre) THEN
              !   write(wrf_err_message,*) 'ERROR: module_positive_definite, ',&
              !                            'mismatching sums ',j,k,ftotal_pre,&
              !                            SUM(line),fmin,1./rftotal_post
              !   write(*,*) line
              !   CALL wrf_error_fatal( wrf_err_message )
              !END IF
           END IF
           f(ids:i_end,k,j) = line(:)
        END IF f_lt_0
        !END DO while_lt_0_loop
     END DO
     END DO
     DEALLOCATE(line)
  END IF

END SUBROUTINE positive_definite_slab

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE positive_definite_sheet( f, f_total, nx, ny )

  IMPLICIT NONE

  ! Arguments
  INTEGER, INTENT(IN   ) :: nx, ny
  REAL, DIMENSION( nx, ny ), INTENT(INOUT) :: f
  REAL, DIMENSION( ny ), INTENT(IN) :: f_total

  ! Local variables
  REAL, DIMENSION(:), ALLOCATABLE :: line
  INTEGER :: iy
  REAL :: fmin, rftotal_post, sum_line
  REAL, PARAMETER :: eps = 1.0e-15

  ! Only do anything if we have to...
  IF (ANY(f < 0.)) THEN
     ALLOCATE(line(nx))
     DO iy = 1, ny
        !while_lt_0_loop: DO WHILE (ANY(f(:,iy) < 0.))
        f_lt_0: IF (ANY(f(:,iy) < 0.)) THEN
           line(:) = f(:,iy)
           ! If the total is negative, set everything to 0. and exit
           IF (f_total(iy) < 0.) THEN
              line(:) = 0.
           ELSE
              ! Value to add to array to make sure every element is > 0.
              fmin = MINVAL(line)
              line(:) = line(:) - fmin ! fmin is negative...
              sum_line = SUM(line)
              IF(sum_line > eps) THEN
                rftotal_post = 1./sum_line
                line = line*f_total(iy)*rftotal_post
              ELSE
                line(:) = 0.
              END IF
              ! The following error can naturally occur on 32-bit machines:
              !IF (SUM(line) /= f_total(iy)) THEN
              !   write(wrf_err_message,*) 'ERROR: module_positive_definite, ',&
              !                            'mismatching sums ',iy,f_total(iy),  &
              !                            SUM(line),fmin,1./rftotal_post
              !   write(*,*) line
              !   CALL wrf_error_fatal( wrf_err_message )
              !END IF
           END IF
           f(:,iy) = line(:)
        END IF f_lt_0
        !END DO while_lt_0_loop
     END DO
     DEALLOCATE(line)
  END IF

END SUBROUTINE positive_definite_sheet

END MODULE module_positive_definite
