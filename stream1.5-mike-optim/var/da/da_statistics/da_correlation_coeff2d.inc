subroutine da_correlation_coeff2d(field1, field2, corr_coeff, rel_acc)
   
   !--------------------------------------------------------------------------
   ! Purpose: Calculate correlation coefficient between two fields.
   !--------------------------------------------------------------------------

   implicit none
   
   real, intent(in)  :: field1(:,:)     ! input field 1.
   real, intent(in)  :: field2(:,:)     ! input field 2.
   real, intent(out) :: corr_coeff      ! correlation coefficient
   real, intent(out) :: rel_acc      ! relative error.
   
   integer           :: iy              ! size of field dim1.
   integer           :: jx              ! size of field dim2.
   real              :: ij_inv          ! 1/(ij)
   real              :: coeff0          ! coefficient.
   real              :: coeff1          ! coefficient.
   real              :: coeff2          ! coefficient.
   real              :: coeff3          ! coefficient.
   real              :: field_mean      ! mean of field.
   real, allocatable :: data1(:,:)
   real, allocatable :: data2(:,:)

   if (trace_use) call da_trace_entry("da_correlation_coeff2d")
   
   ! [1.0] Set up scalars:
   
   iy = size(field1(:,:),dim=1)
   jx = size(field1(:,:),dim=2)
   
   ij_inv = 1.0 / real(iy*jx)
   
   ! [2.0] Calculate mean and remove from field:
   
   field_mean = sum(field1(:,:)) * ij_inv
   allocate(data1(1:iy,1:jx))
   data1(:,:) = field1(:,:) - field_mean
   
   field_mean = sum(field2(:,:)) * ij_inv
   allocate(data2(1:iy,1:jx))
   data2(:,:) = field2(:,:) - field_mean
   
   ! [3.0] Calculate correlation coefficient:
   
   coeff0 = sum(data1(:,:) * data2(:,:))
   coeff1 = sum(data1(:,:) * data1(:,:))
   coeff2 = sum(data2(:,:) * data2(:,:))
   
   if (coeff1 /= 0.0 .and. coeff2 /= 0.0) then
      corr_coeff = coeff0 /  sqrt(coeff1 * coeff2)
   else
      corr_coeff = 0.0
   end if
   
   ! [4.0] Calculate relative error:
   
   coeff3 = sum((data2(:,:) - data1(:,:))**2)
   if (coeff2 /= 0.0) then
      rel_acc = min(coeff3/coeff2,1.0)
   else
      rel_acc = 0.0
   end if
   
   deallocate(data1)
   deallocate(data2)

   if (trace_use) call da_trace_exit("da_correlation_coeff2d")
   
end subroutine da_correlation_coeff2d


