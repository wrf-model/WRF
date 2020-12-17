 subroutine da_transform_rescale(mz, scaling_factor, field)

 !----------------------------------------------------------------------
 ! Purpose: Control-variable transform through rescaling
 ! Author: Yann Michel 2009/9/29, Aime' Fournier 2011/1/2
 !----------------------------------------------------------------------

 implicit none
 integer,          intent(in)    :: mz
 real,             intent(in)    :: scaling_factor(:,:,:)        ! Scaling factor
 real,             intent(inout) :: field(:,:,:)                 ! Field to be transformed. 
 integer :: kt                                                   ! Loop counters.

 if (trace_use_dull) call da_trace_entry("da_transform_rescale")

 do kt = max(1,kts), min(mz,kte)
   field(its:ite,jts:jte,kt)=field(its:ite,jts:jte,kt)*scaling_factor(its:ite,jts:jte,kt)
 enddo
  
 if (trace_use_dull) call da_trace_exit("da_transform_rescale")

endsubroutine da_transform_rescale
