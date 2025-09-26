subroutine rd_be_cv_5(be_fname, be)

   use be_type

   IMPLICIT NONE

   character (len=256) :: be_fname 
   type (be_dat) :: be 

   integer :: ni, nj, nk, nk_2d, j, b
   integer :: be_unit, istatus


   be_unit = 10
   open (be_unit, file=trim(be_fname), status='old', form='unformatted',iostat=istatus)

   read(be_unit) be%ni, be%nj, be%nk
   ni = be%ni
   nj = be%nj
   nk = be%nk

   allocate( be%bin(1:ni,1:nj,1:nk) )
   allocate( be%bin2d(1:ni,1:nj) )

   read(be_unit) be%bin_type
   read(be_unit) be%lat_min, be%lat_max, be%binwidth_lat
   read(be_unit) be%hgt_min, be%hgt_max, be%binwidth_hgt
   read(be_unit) be%num_bins, be%num_bins2d
   read(be_unit) be%bin(1:ni,1:nj,1:nk)
   read(be_unit) be%bin2d(1:ni,1:nj)

!  1.1 Read in regression coefficients

   allocate (be%regcoeff1(1:be%num_bins))
   allocate (be%regcoeff2(1:nk,1:be%num_bins2d))
   allocate (be%regcoeff3(1:nk,1:nk,1:be%num_bins2d))
   read(be_unit) be%regcoeff1  
   read(be_unit) be%regcoeff2 
   read(be_unit) be%regcoeff3  

!  2.0 Load the eigenvector and eigenvalue
 
   allocate ( be%be1_eval_loc (1:nj,1:nk) )
   allocate ( be%be2_eval_loc (1:nj,1:nk) )
   allocate ( be%be3_eval_loc (1:nj,1:nk) )
   allocate ( be%be4_eval_loc (1:nj,1:nk) )
   allocate ( be%be5_eval_loc (1:nj,1:1 ) )

   allocate ( be%be1_eval_glo(1:nk) )
   allocate ( be%be2_eval_glo(1:nk) )
   allocate ( be%be3_eval_glo(1:nk) )
   allocate ( be%be4_eval_glo(1:nk) )
   allocate ( be%be5_eval_glo(1:1) )

   allocate ( be%be1_evec_loc(1:nj,1:nk,1:nk))
   allocate ( be%be2_evec_loc(1:nj,1:nk,1:nk))
   allocate ( be%be3_evec_loc(1:nj,1:nk,1:nk))
   allocate ( be%be4_evec_loc(1:nj,1:nk,1:nk))
   allocate ( be%be5_evec_loc(1:nj,1: 1,1: 1))

   allocate ( be%be1_evec_glo(1:nk,1:nk) )
   allocate ( be%be2_evec_glo(1:nk,1:nk) )
   allocate ( be%be3_evec_glo(1:nk,1:nk) )
   allocate ( be%be4_evec_glo(1:nk,1:nk) )
   allocate ( be%be5_evec_glo(1:1,1:1) )

! 2.2.1 Control variable 1 (psi)

   read(be_unit) be%variable(1)
   read(be_unit) be%nk, be%num_bins2d
   nk = be%nk
   read(be_unit)  be%be1_evec_glo
   read(be_unit)  be%be1_eval_glo
   allocate( be%evec_loc(1:nk,1:nk,1:be%num_bins2d) )
   allocate( be%eval_loc(1:nk,     1:be%num_bins2d) )
   read(be_unit)  be%evec_loc
   read(be_unit)  be%eval_loc
   if( be%variable(1)(1:3) /= 'psi') then
      print*,' Variable mismatch while transfering eigen values from BE file '
      print*,' Expected psi but got ',be%variable
      stop
   endif
   do j=1,nj
      b = be%bin2d(1,j)
      be%be1_evec_loc(j,1:nk,1:nk) = be%evec_loc(1:nk,1:nk,b)
      be%be1_eval_loc(j,1:nk     ) = be%eval_loc(1:nk,b)
   enddo

! 2.2.2 Control variable 2 (chi_u)
   read(be_unit) be%variable(2)
   read(be_unit) be%nk, be%num_bins2d
   nk = be%nk
   read(be_unit)  be%be2_evec_glo
   read(be_unit)  be%be2_eval_glo
   read(be_unit)  be%evec_loc
   read(be_unit)  be%eval_loc
   if( be%variable(2)(1:5) /= 'chi_u') then
      print*,' Variable mismatch while transfering eigen values from BE file '
      print*,' Expected chi_u but got ',be%variable
      stop
   endif
   do j=1,nj
      b = be%bin2d(1,j)
      be%be2_evec_loc(j,1:nk,1:nk) = be%evec_loc(1:nk,1:nk,b)
      be%be2_eval_loc(j,1:nk     ) = be%eval_loc(1:nk,b)
   enddo

! 2.2.3 Control variable 3 (t_u)
   read(be_unit) be%variable(3)
   read(be_unit) be%nk, be%num_bins2d
   nk = be%nk
   read(be_unit)  be%be3_evec_glo
   read(be_unit)  be%be3_eval_glo
   read(be_unit)  be%evec_loc
   read(be_unit)  be%eval_loc
   if( be%variable(3)(1:3) /= 't_u') then
      print*,' Variable mismatch while transfering eigen values from BE file '
      print*,' Expected t_u but got ',be%variable
      stop
   endif
   do j=1,nj
      b = be%bin2d(1,j)
      be%be3_evec_loc(j,1:nk,1:nk) = be%evec_loc(1:nk,1:nk,b)
      be%be3_eval_loc(j,1:nk     ) = be%eval_loc(1:nk,b)
   enddo

! 2.2.4 Control variable 4 (q/qsg)
   read(be_unit) be%variable(4)
   read(be_unit) be%nk, be%num_bins2d
   nk = be%nk
   read(be_unit)  be%be4_evec_glo
   read(be_unit)  be%be4_eval_glo
   read(be_unit)  be%evec_loc
   read(be_unit)  be%eval_loc
   if( be%variable(4)(1:3) /= 'rh') then
      print*,' Variable mismatch while transfering eigen values from BE file '
      print*,' Expected rh but got ',be%variable
      stop
   endif
   do j=1,nj
      b = be%bin2d(1,j)
      be%be4_evec_loc(j,1:nk,1:nk) = be%evec_loc(1:nk,1:nk,b)
      be%be4_eval_loc(j,1:nk     ) = be%eval_loc(1:nk,b)
   enddo

   deallocate ( be%evec_loc )
   deallocate ( be%eval_loc )

! 2.2.5 Control variable ps_u
   read(be_unit) be%variable(5)
   read(be_unit) be%nk_2d, be%num_bins2d
   nk_2d = be%nk_2d
   allocate( be%evec_loc(1:nk_2d,1:nk_2d,1:be%num_bins2d) )
   allocate( be%eval_loc(1:nk_2d,        1:be%num_bins2d) )
   read(be_unit)  be%be5_evec_glo
   read(be_unit)  be%be5_eval_glo
   read(be_unit)  be%evec_loc
   read(be_unit)  be%eval_loc
   if( be%variable(5)(1:4) /= 'ps_u') then
      print*,' Variable mismatch while transfering eigen values from BE file '
      print*,' Expected ps_u but got ',be%variable
      stop
   endif
   do j=1,nj
      b = be%bin2d(1,j)
      be%be5_evec_loc(j,1:nk_2d,1:nk_2d) = be%evec_loc(1:nk_2d,1:nk_2d,b)
      be%be5_eval_loc(j,1:nk_2d    )     = be%eval_loc(1:nk_2d,b)
   enddo

   deallocate ( be%evec_loc )
   deallocate ( be%eval_loc )


! 5.0 Load the scale lengths
! ~~~~~~~~~~~~~~~~~~~~~~~~~~

! 5.1 Allocate the array for scale lengths

   allocate ( be%be1_rf_lengthscale(1:nk) )
   allocate ( be%be2_rf_lengthscale(1:nk) )
   allocate ( be%be3_rf_lengthscale(1:nk) )
   allocate ( be%be4_rf_lengthscale(1:nk) )
   allocate ( be%be5_rf_lengthscale(1:nk) )

! 5.2 read in the scale lengths

   read(be_unit) be%variable(1)
   read(be_unit) be%be1_rf_lengthscale

   read(be_unit) be%variable(2)
   read(be_unit) be%be2_rf_lengthscale

   read(be_unit) be%variable(3)
   read(be_unit) be%be3_rf_lengthscale

   read(be_unit) be%variable(4)
   read(be_unit) be%be4_rf_lengthscale

   read(be_unit) be%variable(5)
   read(be_unit) be%be5_rf_lengthscale(1:1)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! FOR TESTING PURPOSES !!!!!!!!!!!!!!!!!!!!!!!
   allocate(be%znu(39))
   be%znu = (/ 0.9965, 0.988, 0.9765, 0.962, 0.944, 0.9215, 0.8945, 0.8613333, 0.824, &
    0.7866668, 0.7493334, 0.6973798, 0.6332842, 0.5739912, 0.5192013,                &
    0.4686304, 0.4220099, 0.3790851, 0.3396154, 0.3033731, 0.2701429,                &
    0.2397217, 0.2119173, 0.1865487, 0.1634448, 0.1424445, 0.123396,                 &
    0.106156, 0.09059004, 0.07657114, 0.06397995, 0.05270405, 0.04263748,            &
    0.03368013, 0.02573696, 0.0187168, 0.01253016, 0.007084141, 0.002267124 /)
   be%ptop = 1000.
   be%psfc = 101300.

   close(be_unit)

end subroutine rd_be_cv_5
