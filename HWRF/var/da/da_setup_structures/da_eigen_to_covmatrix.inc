Subroutine da_Eigen_to_covmatrix(kzs, be_evec_glo, be_eval_glo, b_native)
!---------------------------------------------------------------------------
!   Recalculate native global vertical background error cov matrix:
!---------------------------------------------------------------------------
   implicit none
   
   integer, intent(in) :: kzs
   real   , dimension(1:kzs,1:kzs), intent(in) :: be_evec_glo
   real   , dimension(1:kzs)                   :: be_eval_glo
   real   , dimension(1:kzs,1:kzs), intent(out):: b_native

   integer    :: k1, k2

     do k1 = 1, kzs
        do k2 = k1, kzs
          b_native(k1,k2) = SUM( be_evec_glo(k1,:) * be_eval_glo(:) * &
                                be_evec_glo(k2,:) )
        end do
     end do
   
     do k1 = 2, kzs
        do k2 = 1, k1-1
          b_native(k1,k2) = b_native(k2,k1)
        end do
     end do

end  Subroutine da_Eigen_to_covmatrix
