  subroutine da_varbc_direct (iv)

   !---------------------------------------------------------------------------
   !  PURPOSE: Apply bias correction to radiance innovations.
   !
   !  METHOD:  d   = y - H(x) - bc
   !          bc   = SUM( beta_i Pred_i )
   !                  i
   !          beta = VarBC parameters
   !          Pred = Bias predictors
   !
   !  Called from da_get_innov_vector_radiance
   !
   !  HISTORY: 10/26/2007 - Creation                     Tom Auligne
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(inout)  :: iv        ! O-B structure.

   integer              :: inst, k, n, npred, npredmax
    
   if (trace_use) call da_trace_entry("da_varbc_direct")

      do inst = 1, iv%num_inst                 ! loop for sensor
         npredmax = iv%instid(inst)%varbc_info%npredmax
         write(unit=stdout,fmt='(A,A)') 'VARBC: Applying bias correction for ', &
            trim(iv%instid(inst)%rttovid_string)
	          
         do k=1,iv%instid(inst)%nchan          ! loop for channel
            npred          = iv%instid(inst)%varbc(k)%npred
	    if (npred <= 0) cycle	               ! VarBC channel only

            do n= iv%instid(inst)%info%n1,iv%instid(inst)%info%n2  ! loop for pixel      	     
              ! apply bias correction through linear regression
              !-------------------------------------------------
              if ( iv%instid(inst)%tb_inv(k,n) > missing_r ) then
                 iv%instid(inst)%tb_inv(k,n) = iv%instid(inst)%tb_inv(k,n) - &
                       SUM( iv%instid(inst)%varbc(k)%param(1:npred) * &
                            iv%instid(inst)%varbc_info%pred( &
                            iv%instid(inst)%varbc(k)%ipred(1:npred),n) )
              end if
	    end do
         end do
      end do                                   ! end loop for sensor

   if (trace_use) call da_trace_exit("da_varbc_direct")

 end subroutine da_varbc_direct
