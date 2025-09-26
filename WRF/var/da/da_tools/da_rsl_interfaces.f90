module da_rsl_interfaces

   interface
      subroutine rsl_register_f90_base_and_size(base,size)
         real,    intent(in) :: base(:)
         integer, intent(in) :: size
      end subroutine rsl_register_f90_base_and_size
   end interface

end module da_rsl_interfaces
