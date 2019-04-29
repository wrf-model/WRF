subroutine da_scale_background_errors ( be, it )
!
   TYPE (be_type), INTENT(INOUT) :: be     ! Back. errors structure
   INTEGER,        INTENT(IN)    :: it     ! outer-loop index
!
   real, allocatable, dimension(:,:) :: v1_val , v2_val , v3_val , &
                                        v4_val , v5_val,           &
                                        v6_val , v7_val , v8_val , &
                                        v9_val , v10_val, v11_val
   real*8, allocatable, dimension(:) :: rf_len1, rf_len2, rf_len3, &
                                        rf_len4, rf_len5,          &
                                        rf_len6, rf_len7, rf_len8, &
                                        rf_len9, rf_len10,rf_len11
!
   integer                     :: be_rf_unit, be_print_unit
   integer  :: i, ix, jy, kz, v1_mz, v2_mz, v3_mz, v4_mz, v5_mz,  &
                              v6_mz, v7_mz, v8_mz, v9_mz, v10_mz,v11_mz
   real     :: ds

   if ( jb_factor <= 0.0 ) return
!
! Rewind the unit:
    be_rf_unit    = unit_end + 1
    be_print_unit = unit_end + 2
    rewind (be_rf_unit)
!
! Read the dimensions and allocate the arrays:
    if(cloud_cv_options >= 2.and. cloud_cv_options.ne.3) then
      read(be_rf_unit) kz, jy, ix, v1_mz, v2_mz, v3_mz, v4_mz, v5_mz, &
                       v6_mz, v7_mz, v8_mz, v9_mz, v10_mz,v11_mz,      &
                       ds   
    else
      read(be_rf_unit) kz, jy, ix, v1_mz, v2_mz, v3_mz, v4_mz, v5_mz, ds
    endif
!
    allocate ( v1_val (1:jy,1:v1_mz) )
    allocate ( rf_len1(1:kz) )
    allocate ( v2_val (1:jy,1:v2_mz) )
    allocate ( rf_len2(1:kz) )
    allocate ( v3_val (1:jy,1:v3_mz) )
    allocate ( rf_len3(1:kz) )
    allocate ( v4_val (1:jy,1:v4_mz) )
    allocate ( rf_len4(1:kz) )
    allocate ( v5_val (1:jy,1:v5_mz) )
    allocate ( rf_len5(1:1) )
    if(cloud_cv_options >= 2 .and. cloud_cv_options .ne. 3) then
      allocate ( v6_val (1:jy,1:v6_mz) )
      allocate ( rf_len6(1:kz) )
      allocate ( v7_val (1:jy,1:v7_mz) )
      allocate ( rf_len7(1:kz) )
      allocate ( v8_val (1:jy,1:v8_mz) )
      allocate ( rf_len8(1:kz) )
      allocate ( v9_val (1:jy,1:v9_mz) )
      allocate ( rf_len9(1:kz) )
      allocate ( v10_val (1:jy,1:v10_mz) )
      allocate ( rf_len10(1:kz) )
      allocate ( v11_val (1:jy,1:v11_mz) )
      allocate ( rf_len11(1:kz) )
    endif

!
! Read the variances and scale-lengths and restore them to be:
   if(cloud_cv_options >= 2.and.cloud_cv_options.ne.3) then
      read(be_rf_unit) v1_val , v2_val , v3_val , v4_val , v5_val , &
                       v6_val , v7_val , v8_val , v9_val , v10_val , v11_val, &
                       rf_len1, rf_len2, rf_len3, rf_len4, rf_len5, &
                       rf_len6, rf_len7, rf_len8, rf_len9, rf_len10,rf_len11

   else
      read(be_rf_unit) v1_val , v2_val , v3_val , v4_val , v5_val , &
                       rf_len1, rf_len2, rf_len3, rf_len4, rf_len5
   endif
!
    be % v1 % val = v1_val
    be % v2 % val = v2_val
    be % v3 % val = v3_val
    be % v4 % val = v4_val
    be % v5 % val = v5_val
    if(cloud_cv_options >= 2.and.cloud_cv_options.ne.3) then
      be % v6 % val = v6_val
      be % v7 % val = v7_val
      be % v8 % val = v8_val
      be % v9 % val = v9_val
      be % v10 % val = v10_val    
      be % v11 % val = v11_val
    endif
!
! Rescale the scale-lengths and variances:
   CALL da_rescale_background_errors( var_scaling1(it), len_scaling1(it), &
                                      ds, rf_len1, be % v1 )
!  .........................................................    
   CALL da_rescale_background_errors( var_scaling2(it), len_scaling2(it), &
                                      ds, rf_len2, be % v2 )
! ..........................................................
   CALL da_rescale_background_errors( var_scaling3(it), len_scaling3(it), &
                                      ds, rf_len3, be % v3 )
! ...............................................................
   CALL da_rescale_background_errors( var_scaling4(it), len_scaling4(it), &
                                      ds, rf_len4, be % v4 )
! ..............................................................
   CALL da_rescale_background_errors( var_scaling5(it), len_scaling5(it), &
                                      ds, rf_len5, be % v5 )
   if(cloud_cv_options >= 2 .and. cloud_cv_options .ne. 3 ) then

     CALL da_rescale_background_errors( var_scaling6(it), len_scaling6(it), &
                                        ds, rf_len6, be % v6 )
!  .........................................................
     CALL da_rescale_background_errors( var_scaling7(it), len_scaling7(it), &
                                        ds, rf_len7, be % v7 )
! ..........................................................
     CALL da_rescale_background_errors( var_scaling8(it), len_scaling8(it), &
                                        ds, rf_len8, be % v8 )
! ...............................................................
     CALL da_rescale_background_errors( var_scaling9(it), len_scaling9(it), &
                                        ds, rf_len9, be % v9 )
! ..............................................................
     CALL da_rescale_background_errors( var_scaling10(it), len_scaling10(it), &
                                        ds, rf_len10, be % v10 )
! ..............................................................
     CALL da_rescale_background_errors( var_scaling11(it), len_scaling11(it), &
                                        ds, rf_len11, be % v11 )
   endif
!
! Print the variances and RF (Recursive Filter) factors rf_alpha:
    write(unit=stdout,fmt='(/5x,"Complete the Rescale BES in outer-loop:" i2)') it
    
    if ( print_detail_be ) then
       write(be_print_unit,'(/"============================================================")')
       write(be_print_unit,'("For outer loop ",i2)') it
       write(be_print_unit,'("it=",i2,2x,"kz=",i3,2x,"jy=",i4,2x,"ix=",i4,2x,"ds=",e12.5)') &
                                                      it, kz, jy, ix, ds
       write(be_print_unit,'("Namelist options specified for this iteration:")')
       write(be_print_unit,'("var_scaling1(it) = ",e12.5,2x,"len_scaling1(it) = "e12.5)')var_scaling1(it),len_scaling1(it)
       write(be_print_unit,'("var_scaling2(it) = ",e12.5,2x,"len_scaling2(it) = "e12.5)')var_scaling2(it),len_scaling2(it)
       write(be_print_unit,'("var_scaling3(it) = ",e12.5,2x,"len_scaling3(it) = "e12.5)')var_scaling3(it),len_scaling3(it)
       write(be_print_unit,'("var_scaling4(it) = ",e12.5,2x,"len_scaling4(it) = "e12.5)')var_scaling4(it),len_scaling4(it)
       write(be_print_unit,'("var_scaling5(it) = ",e12.5,2x,"len_scaling5(it) = "e12.5)')var_scaling5(it),len_scaling5(it)
       write(be_print_unit,'("Background error statistics for this iteration:")')
       write(be_print_unit,'("mz=",i3,2x,"be%v1%val:"/(10e12.5))') be%v1%mz, be%v1%val(1,:)
       write(be_print_unit,'("mz=",i3,2x,"be%v2%val:"/(10e12.5))') be%v2%mz, be%v2%val(1,:)
       write(be_print_unit,'("mz=",i3,2x,"be%v3%val:"/(10e12.5))') be%v3%mz, be%v3%val(1,:)
       write(be_print_unit,'("mz=",i3,2x,"be%v4%val:"/(10e12.5))') be%v4%mz, be%v4%val(1,:)
       write(be_print_unit,'("mz=",i3,2x,"be%v5%val:"/(10e12.5))') be%v5%mz, be%v5%val(1,:)
       write(be_print_unit,'("be%v1%rf_alpha:"/(10e12.5))') be % v1 % rf_alpha(:)
       write(be_print_unit,'("be%v2%rf_alpha:"/(10e12.5))') be % v2 % rf_alpha(:)
       write(be_print_unit,'("be%v3%rf_alpha:"/(10e12.5))') be % v3 % rf_alpha(:)
       write(be_print_unit,'("be%v4%rf_alpha:"/(10e12.5))') be % v4 % rf_alpha(:)
       write(be_print_unit,'("be%v5%rf_alpha:"/(10e12.5))') be % v5 % rf_alpha(:)
       write(be_print_unit,'(/"scale-length: kz=",i3)') kz
       do i = 1,kz 
          if (i == 1) then
             write(be_print_unit,'(i3,2x,5e15.5)') i, rf_len1(i), rf_len2(i), rf_len3(i), rf_len4(i), rf_len5(i)
          else
             write(be_print_unit,'(i3,2x,4e15.5)') i, rf_len1(i), rf_len2(i), rf_len3(i), rf_len4(i)
          endif
       enddo
    endif
!
! Deallocate the arrays:
    deallocate ( v1_val )
    deallocate ( rf_len1 )
    deallocate ( v2_val )
    deallocate ( rf_len2 )
    deallocate ( v3_val )
    deallocate ( rf_len3 )
    deallocate ( v4_val )
    deallocate ( rf_len4 )
    deallocate ( v5_val )
    deallocate ( rf_len5 )
    if ( cloud_cv_options >= 2 .and.cloud_cv_options .ne. 3 ) then
      print*,"deallocate cloud_cv_options >=2 arrays"
      if(allocated(v6_val))deallocate(v6_val)
      if(allocated(v7_val))deallocate(v7_val)
      if(allocated(v8_val))deallocate(v8_val)
      if(allocated(v9_val))deallocate(v9_val)
      if(allocated(v10_val))deallocate(v10_val)
      if(allocated(v11_val))deallocate(v11_val)
      if(allocated(rf_len6))deallocate(rf_len6)
      if(allocated(rf_len7))deallocate(rf_len7)
      if(allocated(rf_len8))deallocate(rf_len8)
      if(allocated(rf_len9))deallocate(rf_len9)
      if(allocated(rf_len10))deallocate(rf_len10)
      if(allocated(rf_len11))deallocate(rf_len11)
    endif
!
end subroutine da_scale_background_errors

