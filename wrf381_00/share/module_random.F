module module_random
  ! This module implements an array of pseudo-random number 
  ! generators (PRNGs).  The internal state of the PRNGs is stored 
  ! in state1, state2, state3, and state4 arrays.  The rand_grid
  ! routines will produce grids of random numbers from these 
  ! generators.  The sequence of random numbers will not vary with
  ! processor decomposition, operating system, computer, compiler or
  ! compiler optimizations, and will be the same every time the
  ! model is run (if the seed is unchanged).  Each gridpoint will 
  ! produce its own independent sequence of random numbers.
  
  ! The srand_grid routine seeds the random number generators, given
  ! an optional "seed" argument.  Each random number generator in
  ! the grid is given a different seed, but those seeds are based on
  ! the seed you provide.  If you do not provide one, the same value
  ! (0) will be used every time the model is run.  That is chosen
  ! intentionally to ensure reproducability of results.  

  ! The rand_grid routines will produce random numbers using the
  ! arrays of random number generators.  The floating-point
  ! versions of rand_grid produce numbers between 0, inclusive, and 1, 
  ! exclusive.  The integer versions produce numbers that span the
  ! entire range of values representable by the datatype.  The full
  ! precision of the floating-point values are generated.

  ! Also, this module is not dependent on dimension ordering.
  ! Arrays are defined as i,j,k, but the code still works if
  ! the dimensions have a different ordering

  ! The implementation of the PRNG is in bobrand.c

  ! Author: Sam Trahan, October 2011

  interface rand_grid
     module procedure rand_grid_i4
     module procedure rand_grid_r4
     module procedure rand_grid_i8
     module procedure rand_grid_r8
  end interface

contains
  subroutine srand_grid(state1,state2,state3,state4, &
                           IDS,IDE,JDS,JDE,KDS,KDE, &
                           IMS,IME,JMS,JME,KMS,KME, &
                           ITS,ITE,JTS,JTE,KTS,KTE,seed)

    ! This routine initializes a grid of random number generators,
    ! using the optional seed argument.  Every random number 
    ! generator will have its own seed, but the seed you provide
    ! will be used to modify those seeds.  If you provide the same
    ! seed, the same sequence of random numbers should be produced,
    ! regardless of computer, compiler, optimization, or operating
    ! system.

    ! If you do not provide a seed, the value 0 will be used,
    ! ensuring that each simulation will produce identical output.

    implicit none
    integer(kind=4), intent(inout) :: state1(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state2(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state3(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state4(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(in), optional :: seed
    integer(kind=4) :: iseed
    integer :: i,j,k
    INTEGER, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE, &
                           IMS,IME,JMS,JME,KMS,KME, &
                           ITS,ITE,JTS,JTE,KTS,KTE
    integer :: seeds(its:ite),n

    if(present(seed)) then
       iseed=seed
    else
       iseed=0
    endif

    n=ite-its+1

    ! Seed all random number generators.
    do k=kts,kte
       do j=jts,jte
          do i=its,ite
             seeds(i)=(kde-kds+1)*((jde-jds+1)*i+j)+k
             ! We can use the same seed here every time
             ! because bobraninit will use both the 
             ! "seeds" array and the "seed" integer to
             ! create the actual seed for each generator.
          enddo
          call bobraninit(state1(its,j,k),state2(its,j,k), &
                          state3(its,j,k),state4(its,j,k), &
                          seeds,seed,n)
       enddo
    enddo
  end subroutine srand_grid

  subroutine rand_grid_r4(state1,state2,state3,state4,randdat, &
                          IDS,IDE,JDS,JDE,KDS,KDE, &
                          IMS,IME,JMS,JME,KMS,KME, &
                          ITS,ITE,JTS,JTE,KTS,KTE)

    ! This routine fills randdat(ITS:ITE,JTS:JTE,KTS:KTE) with an
    ! array of random 32-bit floating-point numbers uniformly
    ! distributed from 0 (inclusive) to 1 (exclusive).
    !
    ! Make sure you call srand_grid before calling this routine.

    implicit none
    integer(kind=4), intent(inout) :: state1(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state2(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state3(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state4(ims:ime,jms:jme,kms:kme)
    real(kind=4),    intent(inout) :: randdat(ims:ime,jms:jme,kms:kme)
    integer :: i,j,k,n
    INTEGER, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE, &
                           IMS,IME,JMS,JME,KMS,KME, &
                           ITS,ITE,JTS,JTE,KTS,KTE

    n=ite-its+1

    do k=kts,kte
       do j=jts,jte
          call bobranval_r4(state1(its,j,k),state2(its,j,k), &
                            state3(its,j,k),state4(its,j,k), &
                            randdat(its,j,k),n)
       enddo
    enddo

  end subroutine rand_grid_r4

  subroutine rand_grid_i4(state1,state2,state3,state4,randdat, &
                          IDS,IDE,JDS,JDE,KDS,KDE, &
                          IMS,IME,JMS,JME,KMS,KME, &
                          ITS,ITE,JTS,JTE,KTS,KTE)

    ! This routine fills randdat(ITS:ITE,JTS:JTE,KTS:KTE) with an
    ! array of random 32-bit signed integers.  The integers will
    ! be uniformly distributed across the entire range of
    ! representation of their datatype: -2**31..2**31-1.
    !
    ! Make sure you call srand_grid before calling this routine.
    !
    ! If you want integers that fall in a specified range, you 
    ! can produce them like this:
    !
    ! do (for each gridpoint)
    !    ! random numbers uniformly distributed from 0..9:
    !    randdat(i,j,k)=mod(abs(randdat(i,j,k),10))
    ! enddo

    implicit none
    integer(kind=4), intent(inout) :: state1(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state2(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state3(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state4(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: randdat(ims:ime,jms:jme,kms:kme)
    integer :: i,j,k,n
    INTEGER, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE, &
                           IMS,IME,JMS,JME,KMS,KME, &
                           ITS,ITE,JTS,JTE,KTS,KTE

    n=ite-its+1

    do k=kts,kte
       do j=jts,jte
          call bobranval_i4(state1(its,j,k),state2(its,j,k), &
                            state3(its,j,k),state4(its,j,k), &
                            randdat(its,j,k),n)
       enddo
    enddo

  end subroutine rand_grid_i4

  subroutine rand_grid_r8(state1,state2,state3,state4,randdat, &
                          IDS,IDE,JDS,JDE,KDS,KDE, &
                          IMS,IME,JMS,JME,KMS,KME, &
                          ITS,ITE,JTS,JTE,KTS,KTE)

    ! This routine fills randdat(ITS:ITE,JTS:JTE,KTS:KTE) with an
    ! array of random 64-bit floating-point numbers uniformly
    ! distributed from 0 (inclusive) to 1 (exclusive).
    !
    ! Make sure you call srand_grid before calling this routine.

    implicit none
    integer(kind=4), intent(inout) :: state1(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state2(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state3(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state4(ims:ime,jms:jme,kms:kme)
    real(kind=8),    intent(inout) :: randdat(ims:ime,jms:jme,kms:kme)
    integer :: i,j,k,n
    INTEGER, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE, &
                           IMS,IME,JMS,JME,KMS,KME, &
                           ITS,ITE,JTS,JTE,KTS,KTE

    n=ite-its+1

    do k=kts,kte
       do j=jts,jte
          call bobranval_r8(state1(its,j,k),state2(its,j,k), &
                            state3(its,j,k),state4(its,j,k), &
                            randdat(its,j,k),n)
       enddo
    enddo

  end subroutine rand_grid_r8

  subroutine rand_grid_i8(state1,state2,state3,state4,randdat, &
                          IDS,IDE,JDS,JDE,KDS,KDE, &
                          IMS,IME,JMS,JME,KMS,KME, &
                          ITS,ITE,JTS,JTE,KTS,KTE)

    ! This routine fills randdat(ITS:ITE,JTS:JTE,KTS:KTE) with an
    ! array of random 64-bit signed integers.  The integers will
    ! be uniformly distributed across the entire range of
    ! representation of their datatype: -2**63..2**63-1.
    !
    ! Make sure you call srand_grid before calling this routine.
    !
    ! If you want integers that fall in a specified range, you 
    ! can produce them like this:
    !
    ! do (for each gridpoint)
    !    ! random numbers uniformly distributed from 0..9:
    !    randdat(i,j,k)=mod(abs(randdat(i,j,k),10))
    ! enddo

    implicit none
    integer(kind=4), intent(inout) :: state1(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state2(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state3(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state4(ims:ime,jms:jme,kms:kme)
    integer(kind=8), intent(inout) :: randdat(ims:ime,jms:jme,kms:kme)
    integer :: i,j,k,n
    INTEGER, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE, &
                           IMS,IME,JMS,JME,KMS,KME, &
                           ITS,ITE,JTS,JTE,KTS,KTE

    n=ite-its+1

    do k=kts,kte
       do j=jts,jte
          call bobranval_i8(state1(its,j,k),state2(its,j,k), &
                            state3(its,j,k),state4(its,j,k), &
                            randdat(its,j,k),n)
       enddo
    enddo

  end subroutine rand_grid_i8

end module module_random
