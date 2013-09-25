subroutine da_setup_div_obs(iv, ob)

	!-------------------------------------------------------------------------
	! Purpose: Sets up pseudo ob part of observation structure.
	!-------------------------------------------------------------------------

	implicit none

	type(iv_type),    intent(inout) :: iv   ! Obs and header structure.
	type(y_type),     intent(inout) :: ob   ! (Smaller) observation structure.

	integer                       :: i,j,k,n,ndiv    ! Loop counters.

	real :: div_val=0.0, div_err=0.0001
	real :: div_x=160.0, div_y=140.0, div_z=30.0
	if (trace_use_dull) call da_trace_entry("da_setup_div_obs")

	ndiv = ite*jte*kte
	!ndiv  = 1
	iv%info(idxdiv)%nlocal          = ndiv
	ob%nlocal(idxdiv)               = ndiv
	iv%info(idxdiv)%ntotal          = ndiv
	iv%info(idxdiv)%max_lev         = 1
    
	iv%info(idxdiv)%plocal(iv%time) = iv%info(idxdiv)%nlocal
    iv%info(idxdiv)%ptotal(iv%time) = iv%info(idxdiv)%ntotal
	
 	allocate (iv%div(1:iv%info(idxdiv)%nlocal))
	
	allocate (iv%info(idxdiv)%name(iv%info(idxdiv)%nlocal))     
	allocate (iv%info(idxdiv)%platform(iv%info(idxdiv)%nlocal)) 
	allocate (iv%info(idxdiv)%id(iv%info(idxdiv)%nlocal))       
	allocate (iv%info(idxdiv)%date_char(iv%info(idxdiv)%nlocal))
	allocate (iv%info(idxdiv)%levels(iv%info(idxdiv)%nlocal))   
	allocate (iv%info(idxdiv)%lat(iv%info(idxdiv)%max_lev,iv%info(idxdiv)%nlocal))    
	allocate (iv%info(idxdiv)%lon(iv%info(idxdiv)%max_lev,iv%info(idxdiv)%nlocal))    
	allocate (iv%info(idxdiv)%elv(iv%info(idxdiv)%nlocal))      
	allocate (iv%info(idxdiv)%pstar(iv%info(idxdiv)%nlocal))    

	allocate (iv%info(idxdiv)%slp(iv%info(idxdiv)%nlocal))   
	allocate (iv%info(idxdiv)%pw(iv%info(idxdiv)%nlocal))    

	allocate (iv%info(idxdiv)%x  (kms:kme,iv%info(idxdiv)%nlocal))   
	allocate (iv%info(idxdiv)%y  (kms:kme,iv%info(idxdiv)%nlocal))   
	allocate (iv%info(idxdiv)%i  (kms:kme,iv%info(idxdiv)%nlocal))   
	allocate (iv%info(idxdiv)%j  (kms:kme,iv%info(idxdiv)%nlocal))      
	allocate (iv%info(idxdiv)%dx (kms:kme,iv%info(idxdiv)%nlocal))  
	allocate (iv%info(idxdiv)%dxm(kms:kme,iv%info(idxdiv)%nlocal)) 
	allocate (iv%info(idxdiv)%dy (kms:kme,iv%info(idxdiv)%nlocal))  
	allocate (iv%info(idxdiv)%dym(kms:kme,iv%info(idxdiv)%nlocal)) 
	allocate (iv%info(idxdiv)%k  (iv%info(idxdiv)%max_lev,iv%info(idxdiv)%nlocal))
	allocate (iv%info(idxdiv)%dz (iv%info(idxdiv)%max_lev,iv%info(idxdiv)%nlocal))  
	allocate (iv%info(idxdiv)%dzm(iv%info(idxdiv)%max_lev,iv%info(idxdiv)%nlocal)) 
	allocate (iv%info(idxdiv)%zk (iv%info(idxdiv)%max_lev,iv%info(idxdiv)%nlocal)) 
	allocate (iv%info(idxdiv)%proc_domain(iv%info(idxdiv)%max_lev,iv%info(idxdiv)%nlocal)) 
	allocate (iv%info(idxdiv)%thinned(iv%info(idxdiv)%max_lev,iv%info(idxdiv)%nlocal)) 
	allocate (iv%info(idxdiv)%obs_global_index(iv%info(idxdiv)%nlocal)) 

	iv%info(idxdiv)%proc_domain(:,:)  = .false.
	iv%info(idxdiv)%thinned(:,:)      = .false.
	iv%info(idxdiv)%zk(:,:)           = missing_r
	iv%info(idxdiv)%n1 = 1
	iv%info(idxdiv)%n2 = iv%info(idxdiv)%nlocal

!   init memory for ob
	ob % nlocal(idxdiv) = iv%info(idxdiv)%nlocal
	ob % ntotal(idxdiv) = iv%info(idxdiv)%ntotal
	allocate (ob % div(1:ob%nlocal(idxdiv)))
	ob%div(1:ob%nlocal(idxdiv)) = 0.0


! 	do n=1, iv%info(idxdiv)%nlocal
! 		iv% div(n) % inv = div_val
! 		iv% div(n) % error = div_err
! 		iv% div(n) % qc = 0
! 		ob% div(n) = missing_r
! 
! 		iv%info(idxdiv)%x(:,n)  = div_x
! 		iv%info(idxdiv)%y(:,n)  = div_y
! 		iv%info(idxdiv)%zk(:,n) = div_z
! 
! 		iv%info(idxdiv)%i(:,n) = int(div_x)
! 		iv%info(idxdiv)%j(:,n) = int(div_y)
! 		iv%info(idxdiv)%k(:,n) = int(div_z)
! 
! 		iv%info(idxdiv)%dx(:,n) = div_x-real(iv%info(idxdiv)%i(:,n))
! 		iv%info(idxdiv)%dy(:,n) = div_y-real(iv%info(idxdiv)%j(:,n))
! 		iv%info(idxdiv)%dxm(:,n)=1.0-iv%info(idxdiv)%dx(:,n)
! 		iv%info(idxdiv)%dym(:,n)=1.0-iv%info(idxdiv)%dy(:,n)
! 		iv%info(idxdiv)%levels(:) = 1
! 
! 	end do
	iv%div(:)%inv = div_val
	iv%div(:)%error = div_error
	iv%div(:)%qc = 0 
	iv%info(idxdiv)%levels(:) = 1
 
	n=0
	do k=kts,kte
		do j=jts,jte
			do i=its,ite
				n=n+1
				iv%info(idxdiv)%x(:,n)  = i
				iv%info(idxdiv)%y(:,n)  = j
				iv%info(idxdiv)%zk(:,n) = k
			end do
		end do
	end do

	iv%info(idxdiv)%i(:,:) = iv%info(idxdiv)%x(:,:)
	iv%info(idxdiv)%j(:,:) = iv%info(idxdiv)%y(:,:)
	iv%info(idxdiv)%k(:,:) = iv%info(idxdiv)%zk(:,:)

	iv%info(idxdiv)%dx(:,:) = 0. !i-real(iv%info(idxdiv)%i(:,n))
	iv%info(idxdiv)%dy(:,:) = 0. !j-real(iv%info(idxdiv)%j(:,n))
	iv%info(idxdiv)%dxm(:,:)=1.0 !-iv%info(idxdiv)%dx(:,n)
	iv%info(idxdiv)%dym(:,:)=1.0 !-iv%info(idxdiv)%dy(:,n) 
	call da_convert_zk (iv%info(idxdiv))
	
	print *, 'finist obs div setup'
! 	if (trace_use_dull) call da_trace_exit("da_setup_div_obs")

end subroutine da_setup_div_obs


