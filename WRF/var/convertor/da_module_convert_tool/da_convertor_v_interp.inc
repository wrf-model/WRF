subroutine da_convertor_v_interp( v_in, z_in, n_in, &
                                  v_out, z_out, n_out )

   implicit none

   integer,                intent(in)  :: n_in, n_out
   real, dimension(n_in),  intent(in)  :: v_in, z_in
   real, dimension(n_out), intent(in)  :: z_out
   real, dimension(n_out), intent(out) :: v_out

   integer :: k, kabv, kblw, i
   real    :: w1, w2

   logical :: increase_in_vertical

   ! does vertical coordinate increase or decrease with increasing k?
   ! set offset appropriately

   if (z_in(n_in) > z_in(1)) then
      increase_in_vertical = .true.
   else
      increase_in_vertical = .false.
   end if

   if (increase_in_vertical) then
      do k=1, n_out
         if (z_out(k) <= z_in(1)) then
            kabv = 2
         else if (z_out(k) >= z_in(n_in)) then
            kabv = n_in
         else
            i_loop: do i=2, n_in
               if (z_out(k) <= z_in(i)) then
                  kabv = i
                  exit i_loop
               end if
            end do i_loop
         end if

         kblw = kabv - 1
         w2 = (z_in(kabv)-z_out(k))/(z_in(kabv)-z_in(kblw))
         w1 = 1.0-w2
         v_out(k) = w1*v_in(kabv) + w2*v_in(kblw)
      end do
   else
      do k=1, n_out
         if (z_out(k) >= z_in(1)) then
            kabv = 2
         else if (z_out(k) <= z_in(n_in)) then
            kabv = n_in
         else
            d_loop: do i=2, n_in
               if (z_out(k) >= z_in(i)) then
                  kabv = i
                  exit d_loop
               end if
            end do d_loop
         end if

         kblw = kabv - 1
         w2 = (z_in(kabv)-z_out(k))/(z_in(kabv)-z_in(kblw))
         w1 = 1.0-w2
         v_out(k) = w1*v_in(kabv) + w2*v_in(kblw)
      end do
   end if

end subroutine da_convertor_v_interp

