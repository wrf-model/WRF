   subroutine da_varbc_tamdar_init (iv)

   !-----------------------------------------------------!
   !  Initialize Varbc for TAMDAR from VARBC_TAMDAR.tbl  !
   !-----------------------------------------------------!

   implicit none

   type (iv_type), intent(inout)   :: iv

   integer, parameter              :: nmaxpred = 5  ! 1,w,(optional: Mach,dT/dt,T)
   integer, parameter              :: nphase = 3    ! descent/ascent/cruise

   integer                         :: npred,nair
   integer                         :: iunit,i,id,iflt,iobs,iphase

   logical                         :: ifexist_table
   character(len=120)              :: filename
   character(len=5), allocatable   :: tail_id(:)


   if (trace_use) call da_trace_entry("da_varbc_tamdar_init")
   
   if (rootproc) then
       write(unit=varbc_tamdar_unit,fmt='(//5X,A/)')      'VARBC_TAMDAR namelist options'
       write(unit=varbc_tamdar_unit,fmt='(10X,A,7X,L)')   'use_varbc_tamdar :         ', use_varbc_tamdar
       write(unit=varbc_tamdar_unit,fmt='(10X,A,7X,I1)')  'varbc_tamdar_bm :          ', varbc_tamdar_bm       ! bias models
       write(unit=varbc_tamdar_unit,fmt='(10X,A,F8.3)')   'varbc_tamdar_pred0 :       ', varbc_tamdar_pred0    ! predictor(1) 
       write(unit=varbc_tamdar_unit,fmt='(10X,A,I8)')     'varbc_tamdar_nbgerr :      ', varbc_tamdar_nbgerr
       write(unit=varbc_tamdar_unit,fmt='(10X,A,I8)')     'varbc_tamdar_nobsmin :     ', varbc_tamdar_nobsmin
   end if

   filename = 'VARBC_TAMDAR.tbl'
   inquire(file=trim(adjustl(filename)), exist=ifexist_table)

   if (ifexist_table) then

       if (rootproc) write(unit=varbc_tamdar_unit,fmt='(//5X,A/)') &
          'Reading in VARBC_TAMDAR.tbl and initializating'

       iv%varbc_tamdar%nmaxpred = nmaxpred
       iv%varbc_tamdar%nphase   = nphase

       call da_get_unit(iunit)
       open(unit=iunit,file=filename, status='old')
       do i = 1, 11; read(iunit,*); end do
       read(iunit,*) nair
       read(iunit,*)

       iv%varbc_tamdar%nair = nair

       npred = 2 
       if (varbc_tamdar_bm == 2) npred = 5

       iv%varbc_tamdar%npred = npred

       allocate ( iv%varbc_tamdar%tail_id  (nair) )
       allocate ( iv%varbc_tamdar%ifuse    (nphase,nair) )
       allocate ( iv%varbc_tamdar%nobs     (nphase+1,nair) )
       allocate ( iv%varbc_tamdar%nobs_sum (nphase+1,nair) )
       allocate ( iv%varbc_tamdar%param    (nmaxpred,nphase,nair) )
       allocate ( iv%varbc_tamdar%pred     (nmaxpred,nphase,nair) )
       allocate ( iv%varbc_tamdar%bgerr    (npred,nphase,nair) )
       allocate ( iv%varbc_tamdar%index    (npred,nphase,nair) )
       allocate ( iv%varbc_tamdar%vtox     (npred,npred,nphase,nair) )

       iv%varbc_tamdar%tail_id(:)    = 0
       iv%varbc_tamdar%ifuse(:,:)    = 0
       iv%varbc_tamdar%nobs(:,:)     = 0
       iv%varbc_tamdar%nobs_sum(:,:) = 0
       iv%varbc_tamdar%param(:,:,:)  = 0.
       iv%varbc_tamdar%pred(:,:,:)   = 0.
       iv%varbc_tamdar%pred(1,:,:)   = varbc_tamdar_pred0
       iv%varbc_tamdar%bgerr(:,:,:)  = 0.
       iv%varbc_tamdar%index(:,:,:)  = 0
       iv%varbc_tamdar%vtox(:,:,:,:) = 0.


       write(iv%varbc_tamdar%fmt_param,*) '(1X,A5,1X,3(1X,I1),15F7.3))'

       allocate( tail_id (nair) )
       tail_id = ''
       do iflt = 1, nair
          read(iunit, fmt=trim(adjustl(iv%varbc_tamdar%fmt_param))) &
               tail_id(iflt), iv%varbc_tamdar%ifuse(1:nphase,iflt),     &
              (iv%varbc_tamdar%param(1:nmaxpred,iphase,iflt), iphase=1,nphase) 

          read(tail_id(iflt),'(I5)') iv%varbc_tamdar%tail_id(iflt)

          do iobs = 1, iv%info(tamdar)%nlocal
             read(iv%info(tamdar)%id(iobs),'(I5)') id
             if (iv%varbc_tamdar%tail_id(iflt) .eq. id) &
                 iv%varbc_tamdar%nobs(nphase+1,iflt) = iv%varbc_tamdar%nobs(nphase+1,iflt) + 1
          end do

          iv%varbc_tamdar%nobs_sum(nphase+1,iflt) = wrf_dm_sum_integer(iv%varbc_tamdar%nobs(nphase+1,iflt))
       end do

       iv%varbc_tamdar%nmaxobs = MAXVAL( iv%varbc_tamdar%nobs_sum(nphase+1,:) )

       allocate ( iv%varbc_tamdar%obs_sn(iv%varbc_tamdar%nmaxobs,nphase+1,nair) )
       iv%varbc_tamdar%obs_sn(:,:,:) = 0

       do iflt = 1, nair
          i = 0
          do iobs = 1, iv%info(tamdar)%nlocal
             read(iv%info(tamdar)%id(iobs),'(I5)') id
             if (iv%varbc_tamdar%tail_id(iflt) .eq. id) then
                 i = i + 1
                 iv%varbc_tamdar%obs_sn(i,nphase+1,iflt) = iobs
             end if
          end do
       end do

       if (rootproc) then
           write(unit=varbc_tamdar_unit,fmt='(10X,A,I4)') &
                    'Number of aircrafts in table : ', nair
           write(unit=varbc_tamdar_unit,fmt='(10X,A,I4)') &
                    'Max. Obs. number in aircraft : ', iv%varbc_tamdar%nmaxobs
       end if

       close(iunit)
       call da_free_unit(iunit)

       deallocate (tail_id)

   else

       use_varbc_tamdar = .false.
       if (rootproc) write(unit=varbc_tamdar_unit,fmt='(/5X,A/)') &
                    'Could not find VARBC_TAMDAR.tbl file. VARBC_TAMDAR is switched off.'

   end if

   if (trace_use) call da_trace_exit("da_varbc_tamdar_init")

   end subroutine da_varbc_tamdar_init 
