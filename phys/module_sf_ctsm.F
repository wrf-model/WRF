module module_sf_ctsm

  implicit none
  private

  public :: create_gindex
  public :: lilac_2d_to_1d

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

    allocate(gindex(num_points))

    n = 0
    do j = jts, jte_limited
       do i = its, ite_limited
          n = n + 1
          ! In the following, note that we use ide-1 rather than ide for the same reason
          ! that we need ite_limited: ide gives the domain end index on the momentum grid,
          ! but here we're just dealing with the mass point grid, which has one less point
          ! in each direction.
          gindex(n) = (j-1)*(ide-1) + i
       end do
    end do
  end subroutine create_gindex

  subroutine lilac_2d_to_1d (ide, jde, ims, ime, jms, jme, its, ite, jts, jte, var_2d, var_1d)
    ! Convert a 2-d native WRF array to a 1-d array appropriate for LILAC

    use module_wrf_error

    ! input/output variables
    integer , intent(in)  :: ide ! domain end index, i
    integer , intent(in)  :: jde ! domain end index, j
    integer , intent(in)  :: ims ! memory start index (includes halo cells), i
    integer , intent(in)  :: ime ! memory end index (includes halo cells), i
    integer , intent(in)  :: jms ! memory start index (includes halo cells), j
    integer , intent(in)  :: jme ! memory end index (includes halo cells), j
    integer , intent(in)  :: its ! task start index, i
    integer , intent(in)  :: ite ! task end index, i
    integer , intent(in)  :: jts ! task start index, j
    integer , intent(in)  :: jte ! task end index, j
    real    , intent(in)  :: var_2d (ims: , jms: )
    real    , allocatable, intent(out) :: var_1d(:)

    ! local variables
    integer :: ite_limited  ! task end index on the mass point grid, i
    integer :: jte_limited  ! task end index on the mass point grid, j
    integer :: num_points
    integer :: i, j, n

    if (.not. all(ubound(var_2d) == [ime, jme])) then
       call wrf_error_fatal('lilac_2d_to_1d: incorrect bounds for var_2d')
    end if

    ite_limited = min(ite, ide-1)
    jte_limited = min(jte, jde-1)

    num_points  = ((ite_limited - its + 1) * (jte_limited - jts + 1))

    allocate (var_1d (num_points))

    n = 0

    do j = jts, jte_limited
       do i = its, ite_limited
          n = n + 1
          var_1d(n) = var_2d(i,j)
       end do
    end do

  end subroutine lilac_2d_to_1d

end module module_sf_ctsm
