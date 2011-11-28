MODULE module_netcdf2kma_interface

   use module_wave2grid_kma
!  implicit none

CONTAINS

SUBROUTINE netcdf2kma_interface ( grid, config_flags ) 

   USE module_domain
   USE module_timing
   USE module_driver_constants
   USE module_configure

!  IMPLICIT NONE
   real,allocatable    :: DPSE(:,:),DUE(:,:,:),DVE(:,:,:),DTE(:,:,:),DQE(:,:,:)
   real,allocatable    :: PSB (:,:), UB(:,:,:), VB(:,:,:), TB(:,:,:), QB(:,:,:)
   real,allocatable    :: PSG (:,:), UG(:,:,:), VG(:,:,:), TG(:,:,:), QG(:,:,:)
   integer :: i,j,k      !shcimsi
   real,allocatable    :: dum(:,:,:)  !shcimsi

!--Input data.

   TYPE(domain) , INTENT(INOUT)  :: grid
   TYPE (grid_config_rec_type)   :: config_flags
   integer                       :: USE_INCREMENT      !shc
   integer     :: incre,back,ID(5),KT,IM,JM,KM         !shc
   integer     :: IMAXE,JMAXE,IMAX,JMAX,KMAX,IDIM,JDIM,MEND1,ISST,JSST,ISNW,JSNW,MAXJZ,IVAR
   integer :: JMAXHF, MNWAV, IMX

! we have to convert in equal lat/lon data 
!           to Gaussian latitude
!
!   First the Equal lat/lon data
! set Field as per KMA order (North top South and 0 to 360 east)

   NAMELIST /netcdf2kma_parm/ IMAXE,JMAXE,IMAX,JMAX,KMAX,IDIM,JDIM,MEND1,ISST,JSST,ISNW,JSNW,MAXJZ,IVAR
!
      READ  (111, NML = netcdf2kma_parm, ERR = 8000)
      close (111)
      print*,' netcdf2kma_parm namelist data read are as follows:'
      print*,' IMAXE= ',IMAXE
      print*,' JMAXE= ',JMAXE
      print*,' MEND1= ',MEND1
      print*,' ISST = ',ISST
      print*,' JSST = ',JSST
      print*,' MAXJZ= ',MAXJZ
      print*,' IVAR = ',IVAR

      JMAXHF=JMAX/2
      MNWAV=MEND1*(MEND1+1)/2
      IMX=IMAX+2

   allocate(DPSE(imaxe,jmaxe))
   allocate(DUE(imaxe,jmaxe,kmax),DVE(imaxe,jmaxe,kmax))
   allocate(DTE(imaxe,jmaxe,kmax),DQE(imaxe,jmaxe,kmax))
   allocate(PSB(imax,jmax))
   allocate(UB(imax,jmax,kmax),VB(imax,jmax,kmax))
   allocate(TB(imax,jmax,kmax),QB(imax,jmax,kmax))
   allocate(PSG(imax,jmax))
   allocate(UG(imax,jmax,kmax),VG(imax,jmax,kmax))
   allocate(TG(imax,jmax,kmax),QG(imax,jmax,kmax))
   allocate(dum(imax,jmax,kmax))  !shcimsi

!shc-wei start
!  back = 102                    !shc start
   back = 48                     !shc start
!shc-wei end
   read(back) ID,KT,IM,JM,KM
   read(back)       !topo
   read(back) PSB
   read(back)       !psea
   read(back) TB 
   read(back) UB 
   read(back) VB 
   read(back) QB 
   read(back)       !rh
   read(back)       !z           !shc end
   USE_INCREMENT=1     !shc start
   if (USE_INCREMENT.eq.1) then
!shc-wei start
!  incre = 101                   
   incre = 47                   
!shc-wei end
   read(incre) DPSE
   read(incre) DUE
   read(incre) DVE
   read(incre) DTE
   read(incre) DQE     !shc end
!  DPSE=20.0; DUE=3.0; DVE=3.0; DTE=5.0;  DQE=0.001    !shcimsi
!  imaxe=grid%ed31-grid%sd31                    !shc start
!  jmaxe=grid%ed32-grid%sd32
!  kmaxe=grid%ed33-grid%sd33
!  imaxg=imaxe;  jmaxg=jmaxe-1; kmaxg=kmaxe    
   call reorder_for_kma(DPSE,imaxe,jmaxe,1)        
   call reorder_for_kma(DUE,imaxe,jmaxe,kmax)
   call reorder_for_kma(DVE,imaxe,jmaxe,kmax)
   call reorder_for_kma(DTE,imaxe,jmaxe,kmax)
   call reorder_for_kma(DQE,imaxe,jmaxe,kmax)  !shc end
   DPSE=DPSE*0.01                              !shchPa 
   call Einc_to_Ganl(DPSE,DUE,DVE,DTE,DQE,&    !shc start
                      PSB, UB, VB, TB, QB,&
                      PSG, UG, VG, TG, QG,&
                      IMAX,JMAX,IMAXE,JMAXE,KMAX,MAXJZ)             
9001 format(10e15.7)     !shcimsi start
!modified by shc nk start
!modified by shc nk end

   call PREGSM1(PSG,TG,UG,VG,QG,PSB,TB,UB,VB,QB,IMAXE,JMAXE,ISST,JSST,MAXJZ,IVAR, &
                IMAX,JMAX,KMAX,IDIM,JDIM,MEND1,MEND1,MEND1,ISNW,JSNW,JMAXHF,MNWAV,IMX ) !shc end

   else          !shc

   call reorder_for_kma(grid%ht(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1),&
                         grid%ed31-grid%sd31  ,grid%ed32-grid%sd32,1)
   call reorder_for_kma(grid%psfc(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1),&
                           grid%ed31-grid%sd31  ,grid%ed32-grid%sd32,1)
   call reorder_for_kma(grid%u_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,&
                             grid%sd33:grid%ed33-1),&
                             grid%ed31-grid%sd31  ,grid%ed32-grid%sd32   ,&
                             grid%ed33-grid%sd33)
   call reorder_for_kma(grid%v_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,&
                             grid%sd33:grid%ed33-1),&
                             grid%ed31-grid%sd31  ,grid%ed32-grid%sd32   ,&
                             grid%ed33-grid%sd33)
   call reorder_for_kma(grid%t_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,&
                             grid%sd33:grid%ed33-1),&
                             grid%ed31-grid%sd31  ,grid%ed32-grid%sd32   ,&
                             grid%ed33-grid%sd33)
   call reorder_for_kma(grid%moist(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,&
                             grid%sd33:grid%ed33-1,P_qv:P_qv),&
                             grid%ed31-grid%sd31  ,grid%ed32-grid%sd32   ,&
                             grid%ed33-grid%sd33)
!
! convert xb-psfc pressure in hPa
  grid%psfc(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1) = 0.01 *  &
  grid%psfc(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1)
  write(*,*) 'shcimsi num of gird',grid%ed31,grid%ed32,grid%ed33
  write(*,*) 'shcimsi grid',grid%ed31-grid%sd31,grid%ed32-grid%sd32,&
             grid%ed33-grid%sd33

  CALL PREGSM(grid%psfc(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1),&
        grid%t_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1),&
        grid%u_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1),&
        grid%v_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1),&
    grid%moist(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1,P_qv),&                   !shc
    PSB,TB,UB,VB,QB,IMAXE,JMAXE,ISST,JSST,MAXJZ,IVAR, &
                IMAX,JMAX,KMAX,IDIM,JDIM,MEND1,MEND1,MEND1,ISNW,JSNW,JMAXHF,MNWAV,IMX)         !shc

    endif         !shc

   deallocate(DPSE,DUE,DVE,DTE,DQE)
   deallocate(PSB , UB, VB, TB, QB)
   deallocate(PSG , UG, VG, TG, QG, dum)

8000  print*,' read error on namelist unit 111'
      stop
    
END SUBROUTINE netcdf2kma_interface


SUBROUTINE reorder_for_kma(wrf,n1,n2,n3)

!IMPLICIT none                
 integer, intent(in) :: n1,n2,n3
 real, intent(inout) :: wrf(n1,n2,n3)

 real, dimension(n1,n2,n3)   :: kma
 integer                     :: i,j,k, n1half
!
    n1half = n1/2 + 0.5
    do k=1,n3
      do j= 1,n2
        do i=1,n1
         if( i <= n1half)then
         kma(n1half+i,n2-j+1,k) = wrf(i,j,k)
         else   
         kma(i-n1half,n2-j+1,k) = wrf(i,j,k)
         end if
        end do
      end do
    end do
      wrf = kma
END SUBROUTINE reorder_for_kma

END MODULE module_netcdf2kma_interface

