module module_sf_ctsm

  implicit none
  private

  public :: create_gindex

contains

  subroutine create_gindex(ide, jde, its, ite, jts, jte, gindex)
    ! Create a gindex array on each task. This gives the list of global indices owned by
    ! each processor, on the mass point grid.
    integer, intent(in) :: ide  ! domain end index, i
    integer, intent(in) :: jde  ! domain end index, j
    integer, intent(in) :: its  ! task start index, i
    integer, intent(in) :: ite  ! task end index, i
    integer, intent(in) :: jts  ! task start index, j
    integer, intent(in) :: jte  ! task end index, j
    integer, allocatable, intent(out) :: gindex(:)

    integer :: ite_limited  ! task end index on the mass point grid, i
    integer :: jte_limited  ! task end index on the mass point grid, j
    integer :: num_points
    integer :: i, j, n

    ! The very last index in both row & column space is just used on the momentum grid.
    ! Here we are just working with the mass point grid, so we need to ignore that last
    ! index.
    ite_limited = min(ite, ide-1)
    jte_limited = min(jte, jde-1)

    num_points = ((ite_limited - its + 1) * (jte_limited - jts + 1))

    print *, "ide :", ide
    print *, "jde :", jde
    print *, "its :", its
    print *, "ite :", ite
    print *, "jts :", jts
    print *, "jte :", jte
    print *, "ite_limited :", ite_limited
    print *, "jte_limited :", jte_limited
    print *, "num_points :, " , num_points

    allocate(gindex(num_points))

    print *, "here 3"
    n = 0
    do j = jts, jte_limited
       do i = its, ite_limited
          n = n + 1
          print *, "about to set ", n , "of", num_points
          ! In the following, note that we use ide-1 rather than ide for the same reason
          ! that we need ite_limited: ide gives the domain end index on the momentum grid,
          ! but here we're just dealing with the mass point grid, which has one less point
          ! in each direction.
          gindex(n) = (j-1)*(ide-1) + i

          ! This should be added in later:
          !INTEGER , DIMENSION(total_computational_MPI_ranks) :: count
          ! INTEGER, INTENT(IN) :: total_computational_MPI_ranks
          !DO m = 1, total_computational_MPI_ranks
          ! IF (m==1) THEN
          !WRITE(UNIT=10,FMT='("MPI RANK = ",I6," OF ",I6," Processes")') m-1,total_computational_MPI_ranks
          !DO c = 1, count(m)
          !    WRITE(UNIT=10,FMT='(I8i)' gindex
          ! END DO
          !ENDIF
          !END DO

       end do
    end do
  end subroutine create_gindex

end module module_sf_ctsm
