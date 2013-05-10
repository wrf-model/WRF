subroutine wr_be_cv_5(be_fname, be)

   use be_type

   IMPLICIT NONE

   integer :: ni, nj, nk, nk_2d, j, b
   character (len=256) :: be_fname
   type (be_dat) :: be
   

   open(12,file=trim(be_fname),form='unformatted')
   write(12) be%ni, be%nj, be%nk

   ni = be%ni
   nj = be%nj
   nk = be%nk

   write(12) be%bin_type
   write(12) be%lat_min, be%lat_max, be%binwidth_lat
   write(12) be%hgt_min, be%hgt_max, be%binwidth_hgt
   write(12) be%num_bins, be%num_bins2d
   write(12) be%bin(1:ni,1:nj,1:nk)
   write(12) be%bin2d(1:ni,1:nj)

!  1.1 Read in regression coefficients

   write(12) be%regcoeff1  
   write(12) be%regcoeff2 
   write(12) be%regcoeff3  

!  2.0 Load the eigenvector and eigenvalue

! 2.2.1 Control variable 1 (psi)

   write(12) be%variable(1)
   write(12) be%nk, be%num_bins2d
   write(12) be%be1_evec_glo
   write(12) be%be1_eval_glo
   allocate( be%evec_loc(1:nk,1:nk,1:be%num_bins2d) )
   allocate( be%eval_loc(1:nk,     1:be%num_bins2d) )
   do j=1,nj
      b = be%bin2d(1,j)
      be%evec_loc(1:nk,1:nk,b) = be%be1_evec_loc(j,1:nk,1:nk)
      be%eval_loc(1:nk,b)      = be%be1_eval_loc(j,1:nk     )
   enddo
   write(12) be%evec_loc
   write(12) be%eval_loc

! 2.2.2 Control variable 2 (chi_u)
   write(12) be%variable(2)
   write(12) be%nk, be%num_bins2d
   write(12) be%be2_evec_glo
   write(12) be%be2_eval_glo
   do j=1,nj
      b = be%bin2d(1,j)
      be%evec_loc(1:nk,1:nk,b) = be%be2_evec_loc(j,1:nk,1:nk)
      be%eval_loc(1:nk,b)      = be%be2_eval_loc(j,1:nk     )
   enddo
   write(12) be%evec_loc
   write(12) be%eval_loc

! 2.2.3 Control variable 3 (t_u)
   write(12) be%variable(3)
   write(12) be%nk, be%num_bins2d
   write(12) be%be3_evec_glo
   write(12) be%be3_eval_glo
   do j=1,nj
      b = be%bin2d(1,j)
      be%evec_loc(1:nk,1:nk,b) = be%be3_evec_loc(j,1:nk,1:nk)
      be%eval_loc(1:nk,b)      = be%be3_eval_loc(j,1:nk     )
   enddo
   write(12) be%evec_loc
   write(12) be%eval_loc

! 2.2.4 Control variable 4 (q/qsg)
   write(12) be%variable(4)
   write(12) be%nk, be%num_bins2d
   write(12) be%be4_evec_glo
   write(12) be%be4_eval_glo
   do j=1,nj
      b = be%bin2d(1,j)
      be%evec_loc(1:nk,1:nk,b) = be%be4_evec_loc(j,1:nk,1:nk)
      be%eval_loc(1:nk,b)      = be%be4_eval_loc(j,1:nk     )
   enddo
   write(12) be%evec_loc
   write(12) be%eval_loc
   deallocate ( be%evec_loc )
   deallocate ( be%eval_loc )
 
! 2.2.5 Control variable ps_u
   nk_2d = be%nk_2d
   allocate( be%evec_loc(1:nk_2d,1:nk_2d,1:be%num_bins2d) )
   allocate( be%eval_loc(1:nk_2d,        1:be%num_bins2d) )
   write(12) be%variable(5)
   write(12) be%nk_2d, be%num_bins2d
   write(12) be%be5_evec_glo
   write(12) be%be5_eval_glo
   do j=1,nj
      b = be%bin2d(1,j)
      be%evec_loc(1:nk_2d,1:nk_2d,b) = be%be5_evec_loc(j,1:nk_2d,1:nk_2d)
      be%eval_loc(1:nk_2d,b)         = be%be5_eval_loc(j,1:nk_2d     )
   enddo
   write(12) be%evec_loc
   write(12) be%eval_loc

   deallocate ( be%evec_loc )
   deallocate ( be%eval_loc )


! 5.0 Load the scale lengths
! ~~~~~~~~~~~~~~~~~~~~~~~~~~


! 5.2 read in the scale lengths

   write(12) be%variable(1)
   write(12) be%be1_rf_lengthscale

   write(12) be%variable(2)
   write(12) be%be2_rf_lengthscale 

   write(12) be%variable(3)
   write(12) be%be3_rf_lengthscale 

   write(12) be%variable(4)
   write(12) be%be4_rf_lengthscale 

   write(12) be%variable(5)
   write(12) be%be5_rf_lengthscale(1:1)

   close(12)

end subroutine wr_be_cv_5
