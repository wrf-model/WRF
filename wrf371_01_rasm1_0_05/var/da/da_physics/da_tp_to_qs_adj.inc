subroutine da_tp_to_qs_adj (grid, qs_prime_over_qs )

   !---------------------------------------------------------------------------
   ! Purpose: Adjoint of da_tp_to_qs_lin
   !---------------------------------------------------------------------------

   implicit none

   type (domain), intent(inout)  :: grid
   real,          intent(in)     :: qs_prime_over_qs(its:ite,jts:jte,kts:kte)

   integer :: is, ie       ! 1st dim. end points.
   integer :: js, je       ! 2nd dim. end points.
   integer :: ks, ke       ! 3rd dim. end points.
   integer :: i, j, k      ! Loop counters.
   real    :: temp         ! Temporary array.
   real    :: es_prime_over_es ! Sat Vap pressure ratio.

   if (trace_use) call da_trace_entry("da_tp_to_qs_adj")

   !---------------------------------------------------------------------------
   ! [1.0] initialise:
   !---------------------------------------------------------------------------

   is = its; ie = ite
   js = jts; je = jte
   ks = kts; ke = kte      

   if ( test_transforms ) then
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
   ! [3.0] Calculate saturation specific humidity increment:
   !---------------------------------------------------------------------------

   do k = ks, ke
      do j = js, je
         do i = is, ie

            temp = qs_prime_over_qs(i,j,k) / &
                   ( grid%xb % p(i,j,k) - rd_over_rv1 * grid%xb % es(i,j,k) )
   
            es_prime_over_es = temp * grid%xb % p(i,j,k)

            grid%xa % p(i,j,k) = grid%xa % p(i,j,k) - temp
   
   !---------------------------------------------------------------------------
   ! [2.0] Calculate saturation vapour pressure increment:
   !---------------------------------------------------------------------------

            temp = grid%xb % t(i,j,k) + es_gammakelvin

            grid%xa % t(i,j,k) = grid%xa % t(i,j,k) + es_gammabeta * es_prime_over_es / &
                            ( temp * temp )
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_tp_to_qs_adj")

end subroutine da_tp_to_qs_adj

   !subroutine da_tp_to_qs_adj( t, p, es, t_prime, p_prime, &
   !                            qs_prime_over_qs, n )

   !---------------------------------------------------------------------------
   ! Purpose: Adjoint of da_tp_to_qs_lin
   !---------------------------------------------------------------------------

   ! implicit none

   ! integer      i, n
   ! real         t, p, es, t_prime, p_prime, qs_prime_over_qs
   ! dimension    t               (n) ! Temperature.
   ! dimension    p               (n) ! Pressure.
   ! dimension    es              (n) ! Sat. vapour pressure.
   ! dimension    t_prime         (n) ! Temperature increment.
   ! dimension    p_prime         (n) ! Pressure increment.
   ! dimension    qs_prime_over_qs(n) ! qs~/qs.

   ! real         temp             ! Temporary storage.
   ! real         es_prime_over_es ! es~/es
   !    
   ! do i = 1,n
      !------------------------------------------------------------------------
      !    [3.0] Calculate saturation specific humidity increment:
      !------------------------------------------------------------------------

      ! temp = qs_prime_over_qs(i) / ( p(i) - rd_over_rv1 * es(i) )

      ! es_prime_over_es = temp * p(i)

      ! p_prime(i) = p_prime(i) - temp

      !------------------------------------------------------------------------
      ! [2.0] Calculate saturation vapour pressure increment:
      !------------------------------------------------------------------------

      ! temp = t(i) + es_gammakelvin

      ! t_prime(i) = t_prime(i) + es_gammabeta * es_prime_over_es / ( temp * temp )
   ! end do

   ! end subroutine da_tp_to_qs_adj


