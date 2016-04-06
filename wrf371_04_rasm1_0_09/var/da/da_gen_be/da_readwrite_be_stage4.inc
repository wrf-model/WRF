subroutine da_readwrite_be_stage4(outunit, nk, uh_method, n_smth_sl, variable)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, parameter          :: spike_tolerance = 1.5      ! Threshold for detecting spikes in data. 

   integer, intent(in)      :: outunit                    ! Output unit number.
   integer, intent(in)      :: nk                         ! Number of vertical levels/modes.
   integer    , intent(in)  :: n_smth_sl                  ! Number of smoothing. 0: no smoothig
   character*7, intent(in)  :: uh_method                  ! Uh method (power, scale, wavelet)
   character*10, intent(in) :: variable                   ! Variable name.
   character, save          :: namw1                      ! Test namw.
   character(len=filename_len) :: filename                ! Input filename.
   character*10, save       :: cvar                       ! Dummy variable name.
   character*2              :: ck                         ! Loop index -> character.
   integer                  :: iunit
   integer                  :: k                          ! Loop counter.
   integer                  :: kdum                       ! Dummy vertical index.
   integer, save            :: lf1,nb1,nij1(0:1)          ! Test lf, nb & nij.
   integer                  :: max_wavenumber             ! Smallest scale required (ni/2 - 1).
   integer                  :: stride                     ! Only for filename.
   logical                  :: ex                         ! Test if input file exists.
   logical, save            :: do_normalize1              ! Test do_normalize.
   real                     :: ml                         ! Gradient (read from file but not used).
   real                     :: mean_scale                 ! Average scale between points.
   logical, save            :: first_time=.true.          ! True if first time called.
   real                     :: scale_length(nk)
   real                     :: sl_smth(nk)
   real, allocatable        :: sd(:,:)                    ! Horizontal standard deviations.
   real, allocatable        :: total_power(:)             ! Total Power spectrum.

   call da_get_unit(iunit)

   if (trim(uh_method) .eq. 'power') then
      do k = 1, nk
         write(ck,'(i2)')k
         if (k < 10) ck = '0'//ck(2:2)

         filename = trim(variable)//'/'//trim(variable) &
                                  //'.'//ck//'.spectrum'

         open (iunit, file = filename, form='unformatted')
         read(iunit)cvar
         if (trim(cvar) /=  trim(variable)) then
           call da_error(__FILE__,__LINE__, &
             (/"Variable name inconsistency"/))
         end if

         read(iunit)max_wavenumber, kdum
         if (kdum /= k) then
           call da_error(__FILE__,__LINE__, &
             (/"Inconsistent vertical label"/))
         end if

         allocate(total_power(0:max_wavenumber))

         read(iunit)total_power
         close(iunit)

         write(outunit)variable
         write(outunit)max_wavenumber, k
         write(outunit) .false. ! preserve file format
         write(outunit)total_power

         deallocate(total_power)

      end do

   else if (trim(uh_method) == 'scale') then

      if( .not.use_rf ) then
         call da_error(__FILE__,__LINE__,(/"{uh_method,use_rf}={''scale'',.false.} error"/))
      end if

      filename = trim(variable)//'/'//'sl_print.'//trim(variable)
      open (iunit, file = filename)

      do k=1, nk
         read(iunit,'(a,2e20.8)') ck, ml, scale_length(k)
         ! If missing value is encountered, use the value from the last
         ! mode or level (YRG, 06/30/2005):
         if (ml == 0.0 .and. scale_length(k) == 0.0) &
             scale_length(k) = scale_length(k-1)
      end do

      ! Remove spikes in lengthscales (extrapolate if spike detected):
      do k = 2, nk-1
         mean_scale = 0.5 * ( scale_length(k-1) + scale_length(k+1) )
         if ( scale_length(k) > spike_tolerance * mean_scale ) then
            scale_length(k) = mean_scale
         end if
      end do

      ! Smoothing the scale_length
      sl_smth =  scale_length
      do kdum = 1, n_smth_sl
         do k = 2, nk-1
            sl_smth(k) = scale_length(k) &
               + 0.25*(scale_length(k-1)+scale_length(k+1)-2.0*scale_length(k))
         end do
         scale_length = sl_smth 
      end do
     
      write(outunit) variable
      write(outunit) scale_length
      close(iunit)

      if( do_normalize )then
         allocate(nij(0:0,0:1,0:0))
         do k=1, nk
            write(ck,'(i0)')k
            ex=.false.
            stride=1
            do while( .not.ex )		! loop thus only because gen_be_stage4_regional uses stride:
               write(filename,'("gen_be_stage4_regional.",i0)')stride
               inquire(exist=ex,file=trim(filename))
               if( .not.ex ) then
                  write(unit=message(1),fmt='(a,a)') trim(filename),' does not exist.'
                  call da_warning(__FILE__,__LINE__,message(1:1))
               end if
               stride=stride+1
            enddo
            filename = trim(filename)//"/dir."//trim(variable)//trim(ck)//"/mom"
            open(iunit,action="read",file=trim(filename),form="unformatted",status="old")
            read(iunit)do_normalize1
            if( do_normalize.neqv.do_normalize1 ) &
               call da_error(__FILE__,__LINE__,(/"do_normalize.neqv.do_normalize1 for uh_method==''scale''"/))
            read(iunit)nij
            if( k==1 )then
               write(outunit)do_normalize
               write(outunit)nij
               allocate(sd(nij(0,1,0),nij(0,0,0)))
            endif
            read(iunit)sd
            write(outunit)sd
            close(iunit)
         enddo
         deallocate(nij,sd)
      endif

   elseif( trim(uh_method) == 'wavelet' )then
      if( use_rf )call da_error(__FILE__,__LINE__,(/"{uh_method,use_rf}={''wavelet'',.true.} error"/))
      if( first_time )then
         write(outunit) len_trim('wavelet')
         write(outunit) 'wavelet'
         cvar=trim(variable)
      endif
      write(outunit) len_trim(variable),nk
      write(outunit) trim(variable)
      do k=1, nk
         write(ck,'(i0)')k
         filename = "gen_be_stage4_regional/dir."//trim(variable)//trim(ck)//"/momw"
         open(iunit,action="read",file=trim(filename),form="unformatted",status="old")
         read(iunit)do_normalize1,namw,lf,nb
         if( first_time )then		! Use same basis forall {variable,k}:
            if( do_normalize.neqv.do_normalize1 ) &
               call da_error(__FILE__,__LINE__,(/"do_normalize.neqv.do_normalize1 in "//filename/))
            namw1=namw
            lf1=lf
            nb1=nb
            write(outunit)do_normalize,namw,lf,nb
            allocate(nij(0:nb,0:1,0:2))
         elseif( do_normalize.neqv.do_normalize1 .or. namw/=namw1 .or. lf/=lf1 .or. nb/=nb1 )then
            write(message(1), &
               '("{nb namw lf do_normalize}[",a,",k=",i0,"]/{nb namw lf do_normalize}[",a,",1]={",i0,a,i0,l2,"}/{",i0,a,i0,l2,"}.")') &
               trim(variable),k,trim(cvar),nb,namw,lf,do_normalize,nb1,namw1,lf1,do_normalize1
            call da_error(__FILE__,__LINE__,message(1:1))
         endif
         read(iunit)nij
         if( first_time )then
            nij1=nij(0,:,0)
            write(outunit)nij
            allocate(wsd(nij(0,1,2),nij(0,0,2)))
            if( do_normalize )allocate(sd(nij(0,1,0),nij(0,0,0)))
            first_time=.false.
         elseif( any(nij(0,:,0)/=nij1) )then
            write(message(1), &
               '("{ni nj}[",a,",k=",i0,"]/{ni nj}[",a,",1]={",i0,1x,i0,"}/{",i0,1x,i0,"}.")') &
               trim(variable),k,trim(cvar),nij(0,:,0),nij1
            call da_error(__FILE__,__LINE__,message(1:1))
         endif
         read(iunit)wsd
         write(outunit)wsd
         if( do_normalize )then
            read(iunit)sd
            write(outunit)sd
         endif
         close(iunit)
      end do				! do k=1,nk
   end if

   call da_free_unit(iunit)

end subroutine da_readwrite_be_stage4
