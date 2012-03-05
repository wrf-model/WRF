subroutine da_tprh_to_q_adj (grid)

   !---------------------------------------------------------------------------
   !  Purpose: Adjoint of da_tprh_to_q_adj.
   !---------------------------------------------------------------------------

   implicit none

   type (domain), intent(inout)  :: grid

   integer :: is, ie   ! 1st dim. end points.
   integer :: js, je   ! 2nd dim. end points.
   integer :: ks, ke   ! 3rd dim. end points.
   integer :: i, j, k  ! Loop counter.
   real    :: qs_prime_over_qs(its:ite,jts:jte,kts:kte) ! Temp.

   if (trace_use) call da_trace_entry("da_tprh_to_q_adj")

   !---------------------------------------------------------------------------
   ! [1.0] initialise:
   !---------------------------------------------------------------------------

   is = its; ie = ite
   js = jts; je = jte
   ks = kts; ke = kte   

   if (test_transforms) then
      is = its-1
      js = jts-1

      ie = ite+1
      je = jte+1

      if ( is < ids ) is = ids
      if ( js < jds ) js = jds

      if ( ie > ide ) ie = ide
      if ( je > jde ) je = jde
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Calculate relative humidity increment:
   !---------------------------------------------------------------------------

   do k = ks, ke
      do j = js, je
         do i = is, ie
            qs_prime_over_qs(i,j,k) = grid%xb % q(i,j,k) * grid%xa % q(i,j,k)

            grid%xa % rh(i,j,k) = grid%xa % rh(i,j,k) + qs_prime_over_qs(i,j,k) / &
                             grid%xb % rh(i,j,k)
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [2.0] Calculate saturation specific humidity ratio qs~/qs:
   !---------------------------------------------------------------------------

   call da_tp_to_qs_adj (grid, qs_prime_over_qs )

   if (trace_use) call da_trace_exit("da_tprh_to_q_adj")

end subroutine da_tprh_to_q_adj

   !subroutine da_tprh_to_q_adj( t, p, es, q, rh, &
   !                             t_prime, p_prime, rh_prime, q_prime, n )

   !---------------------------------------------------------------------------
   !  Purpose: Adjoint of da_tprh_to_q_adj.
   !---------------------------------------------------------------------------

   !   implicit none

   !   integer        i, n
   !   real           t, es, p, q, rh,t_prime, p_prime, rh_prime, q_prime  
   !   dimension      t       (n) ! Temperature.
   !   dimension      es      (n) ! Saturation vapour pressure.
   !   dimension      p       (n) ! Pressure.
   !   dimension      q       (n) ! Specific humidity.
   !   dimension      rh      (n) ! Relative Humidity.
   !   dimension      t_prime (n) ! Temperature increment.
   !   dimension      p_prime (n) ! Pressure increment.
   !   dimension      rh_prime(n) ! Pressure increment.
   !   dimension      q_prime (n) ! Pressure increment.

   !   real        temp, qs_prime_over_qs  ! Temporary storage.
   !   dimension   qs_prime_over_qs(n)     ! qs~/qs.

   !   do i = 1,n
   !   temp = q(i) * q_prime(i)

   !---------------------------------------------------------------------------
   !  [2.0] Calculate relative humidity increment:
   !---------------------------------------------------------------------------

   !   rh_prime(i) = rh_prime(i) + temp / rh(i)
   !   qs_prime_over_qs(i) = temp
   !   end do

   !---------------------------------------------------------------------------
   !  [1.0] Calculate saturation specific humidity ratio qs~/qs:
   !---------------------------------------------------------------------------

   !   call da_tp_to_qs_adj( t, p, es, t_prime, p_prime, qs_prime_over_qs, n )

   !end subroutine da_tprh_to_q_adj


