module module_ozmixm
  implicit none
  real, pointer, dimension(:,:,:,:) :: ozmixm=>NULL()
  real, pointer, dimension(:,:,:,:,:) :: aerodm=>NULL()
  real, pointer, dimension(:) :: pin=>NULL(), pina=>NULL()
  integer :: n_ozmixm=-1,levsiz=-1,no_src_types=-1,alevsiz=-1,num_ozmixm=-1

contains

  subroutine check_module_ozmixm(why)
    implicit none
    character*(*),intent(in) :: why
    character*255 ::message
38  format(A,': module_ozmixm sizes: n_ozmixm=',I0,' levsiz=',I0,&
         ' no_src_types=',I0,' alevsiz=',I0,' num_ozmixm='I0)
    write(message,38) why,n_ozmixm,levsiz,no_src_types,alevsiz,num_ozmixm
    call wrf_debug(1,message)
77  format(A,': pin=',F0.7,'...',F0.7,' ozmixm=',F0.7,'...',F0.7)
    write(message,77) why,minval(pin),maxval(pin),minval(ozmixm),maxval(ozmixm)
    call wrf_debug(1,message)
  end subroutine check_module_ozmixm

  !--------------------------------

  subroutine init_module_ozmixm(ims,ime,set_levsiz,jms,jme,set_n_ozmixm,&
       set_alevsiz,set_no_src_types)
    implicit none
    integer, intent(in) :: ims,ime,set_levsiz,jms,jme,set_n_ozmixm,&
         set_no_src_types,set_alevsiz
    if(associated(ozmixm)) return
    !$OMP MASTER
    call wrf_debug(0,"Allocated OZMIXM and friends in module_ozmixm.")
    levsiz=set_levsiz
    n_ozmixm=set_n_ozmixm
    num_ozmixm=set_n_ozmixm
    no_src_types=set_no_src_types
    allocate(ozmixm(ims:ime,levsiz,jms:jme,n_ozmixm),pin(levsiz), &
         aerodm(ims:ime,alevsiz,jms:jme,n_ozmixm-1,no_src_types), &
         pina(alevsiz))
    ozmixm=0
    pin=0
    aerodm=0
    pina=0
    !$OMP END MASTER
  end subroutine init_module_ozmixm
end module module_ozmixm
