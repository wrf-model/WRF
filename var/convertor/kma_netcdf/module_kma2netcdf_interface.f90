MODULE module_kma2netcdf_interface

   use module_kma_wave2grid

   USE module_domain
   USE module_timing
   USE module_driver_constants
   USE module_configure
   use module_kma_wave2grid
   USE module_tiles
!  implicit none    !shc-wei

CONTAINS

SUBROUTINE kma2netcdf_interface ( grid, config_flags)

!  IMPLICIT NONE    !shc-wei

!--Input data.
   TYPE (grid_config_rec_type)   :: config_flags
   TYPE(domain), TARGET          :: grid
#ifdef DEREF_KLUDGE
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33

   sm31             = grid%sm31
   em31             = grid%em31
   sm32             = grid%sm32
   em32             = grid%em32
   sm33             = grid%sm33
   em33             = grid%em33
#endif

   call kma2netcdf_solver( grid, config_flags  &

#include "actual_args.inc"

               )

end SUBROUTINE kma2netcdf_interface


SUBROUTINE kma2netcdf_solver( grid, config_flags &
#include "dummy_args.inc"
                 )

!  IMPLICIT NONE   !shc-wei

!--Input data.
   TYPE (grid_config_rec_type)   :: config_flags
   TYPE(domain), TARGET               :: grid
!  Definitions of dummy arguments to solve

#include "dummy_decl.inc"
!    INCLUDE 'mpif.h'
    real, allocatable      :: q(:,:,:) 
    Integer                :: my_proc_id, ierr 
    Integer                :: ii, jj,landmask_T213(428,215)  
    real                   :: sfc_T213(428,215)
!---------------------------------------------------------------------------
   INTEGER  :: ids,ide, jds,jde, kds,kde   ! domain dims.
   INTEGER  :: ims,ime, jms,jme, kms,kme   ! memory dims.
   INTEGER  :: ips,ipe, jps,jpe, kps,kpe   ! patch  dims.
   INTEGER  :: its,ite, jts,jte, kts,kte   ! Tile   dims.
   INTEGER  :: i
   INTEGER  :: IGRD, JGRD, KGRD, JCAP, KMAX, INTVL                           
!rizvi add ------------------------------------------------------------------
   NAMELIST /kma2netcdf_parm/ IGRD, JGRD, KGRD, JCAP, KMAX, INTVL                           
!
      READ  (111, NML = kma2netcdf_parm, ERR = 8000)
      close (111)
      write(unit=*, fmt='(A,5(1x,/,5x,A,i6))')'kma2netcdf_parm namelist read: ',&
      'IGRD= ',IGRD,'JGRD= ',JGRD,'KGRD= ',KGRD,'JCAP = ',JCAP,'KMAX= ',KMAX,'INTVL= ',INTVL
      DPHI=180./(JGRD-1)
      LMAX=KGRD-1
      KLMAX=KMAX
      MEND1 = JCAP + 1
      NEND1 = JCAP + 1
      JEND1 = JCAP + 1
      IMAXG = IGRD 
      JMAXG = JGRD - 1 

      IMAX  =360./DPHI+0.5
      IOUT  =IMAX/INTVL  
      JMAX  =180./DPHI+1.5
      JOUT  =(JMAX-1)/INTVL+1
      IMX   =IMAX+2
      JOUTHF= (JOUT+1)/2
      JMXGHF= (1+JMAXG)/2
      KMX2  =KMAX*2  
      LMX2 =LMAX*2
      MNWAV =MEND1*(MEND1+1)/2


!rizvi add ------------------------------------------------------------------
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

  call copy_dims( grid, xp, &
                   ids, ide, jds, jde, kds, kde, &
                   ims, ime, jms, jme, kms, kme, &
                   ips, ipe, jps, jpe, kps, kpe  )
!--Compute these starting and stopping locations for each tile and number of tiles.

   CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )

   call copy_tile_dims( grid, xp, its, ite, jts, jte, kts, kte )

   allocate (q(ims:ime,jms:jme,kms:kme))
    
!   go to   100
#ifndef DEREF_KLUDGE
     call W2GCONV(IGRD, JGRD, KGRD, JCAP, KMAX, INTVL , &
             DPHI, LMAX, KLMAX, MEND1, NEND1, JEND1, IMAXG, JMAXG, &
             IMAX, IOUT, JMAX, JOUT, IMX, JOUTHF, JMXGHF, KMX2, LMX2, MNWAV, &                            
             ht, psfc, t_2, u_2, v_2, q, KMA_A, KMA_B, &
             xp%ims, xp%jms, xp%kms, xp%ime, xp%jme, xp%kme,&
             xp%ids, xp%jds, xp%kds, xp%ide, xp%jde, xp%kde,&
             xp%its, xp%jts, xp%kts, xp%ite, xp%jte, xp%kte )
#else
     call W2GCONV(IGRD, JGRD, KGRD, JCAP, KMAX, INTVL , &
             DPHI, LMAX, KLMAX, MEND1, NEND1, JEND1, IMAXG, JMAXG, &
             IMAX, IOUT, JMAX, JOUT, IMX, JOUTHF, JMXGHF, KMX2, LMX2, MNWAV, &                            
     ht(ims,jms),psfc(ims,jms),t_2(ims,jms,kms),& 
     u_2(ims,jms,kms), v_2(ims,jms,kms), q(ims,jms,kms),&
     KMA_A(kms),KMA_B(kms), ims, jms, kms, ime, jme, kme, &
                            ids, jds, kds, ide, jde, kde, &
                            its, jts, kts, ite, jte, kte  )
#endif

!  convert KMA pressure which is in hPa into Psacal in grid-array
    psfc(its:ite,jts:jte) = 100. * psfc(its:ite,jts:jte)
100  continue
 my_proc_id = 0
#ifdef DM_PARALLEL
   call MPI_COMM_RANK( MPI_COMM_WORLD, my_proc_id, ierr )
#else
   my_proc_id = 0
#endif

   moist(ims:ime,jms:jme,kms:kme,P_qv) = q(ims:ime,jms:jme,kms:kme)
   deallocate (q)                 
! Load landmask from KMA-original land mask for T213
    if( JCAP == 213 ) then
     open( UNIT = 151, file= 'KMA_landmask_428_215', status= 'old')
       do jj=jds,jde
        read(151,'(428i1)', err=9000) (landmask_T213(ii,jj),ii=1,428)
       enddo
       do jj=jts,jte
        landmask(its:ite,jj) = landmask_T213(its:ite,jde-jj+1)
       enddo
       write(unit=*, fmt='(A)')'read successfully landmask'
       close (151) 
! Load U10 at T213 (428x215) resolution                          
     open( UNIT = 151, file= 'nwpgr_UUUU.2007020100', status= 'old')
        read(151,'(10e20.10)', err=9000) sfc_T213                     
       do jj=jts,jte
        u10(its:ite,jj) = sfc_T213(its:ite,jde-jj+1)
       enddo
       write(unit=*, fmt='(A)')'read successfully U10'
       close (151) 
! Load V10 at T213 (428x215) resolution                          
     open( UNIT = 151, file= 'nwpgr_VVVV.2007020100', status= 'old')
        read(151,'(10e20.10)', err=9000) sfc_T213                     
       do jj=jts,jte
        v10(its:ite,jj) = sfc_T213(its:ite,jde-jj+1)
       enddo
       write(unit=*, fmt='(A)')'read successfully V10'
       close (151) 
! Load T2  at T213 (428x215) resolution                          
     open( UNIT = 151, file= 'nwpgr_TTTT.2007020100', status= 'old')
        read(151,'(10e20.10)', err=9000) sfc_T213                     
       do jj=jts,jte
         t2(its:ite,jj) = sfc_T213(its:ite,jde-jj+1)
       enddo
       write(unit=*, fmt='(A)')'read successfully T2'
       close (151) 
! Load Q2  at T213 (428x215) resolution                          
     open( UNIT = 151, file= 'nwpgr_QQQQ.2007020100', status= 'old')
        read(151,'(10e20.10)', err=9000) sfc_T213                     
       do jj=jts,jte
         q2(its:ite,jj) = sfc_T213(its:ite,jde-jj+1)
       enddo
       write(unit=*, fmt='(A)')'read successfully Q2'
       close (151) 
! Load SST at T213 (428x215) resolution                          
     open( UNIT = 151, file= 'nwpgr_SSTT.2007020100', status= 'old')
        read(151,'(10e20.10)', err=9000) sfc_T213                     
       do jj=jts,jte
         sst(its:ite,jj) = sfc_T213(its:ite,jde-jj+1)
       enddo
       write(unit=*, fmt='(A)')'read successfully SST'
       close (151) 

    else
    write(unit=*, fmt='(A,i3)')'Surface data is not available for T',JCAP
    endif

    write(unit=*, fmt='(A)')'Job done for kma2netcdf_solver' 
    return
8000 write(unit=*, fmt='(A)')'read error on namelist unit 111'
     stop
9000 write(unit=*, fmt='(A)')'read error on unit 151'
     stop
END SUBROUTINE kma2netcdf_solver

SUBROUTINE copy_dims(grid, xp, &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     ips, ipe, jps, jpe, kps, kpe )
!------------------------------------------------------------------------------
!  PURPOSE: Copy dimensioning information from grid structure.
!
!------------------------------------------------------------------------------

   USE module_domain
   TYPE(domain), INTENT(IN)         :: grid
   TYPE (xpose_type),INTENT(INOUT)  :: xp      ! Transpose variables.

   INTEGER,      INTENT(OUT)        :: ids,ide, jds,jde, kds,kde   ! domain dims.
   INTEGER,      INTENT(OUT)        :: ims,ime, jms,jme, kms,kme   ! memory dims.
   INTEGER,      INTENT(OUT)        :: ips,ipe, jps,jpe, kps,kpe   ! patch  dims.

!--De-reference dimension information stored in the grid data structure.

   ids = grid%sd31 
   ide = grid%ed31 - 1
   jds = grid%sd32 
   jde = grid%ed32 - 1
   kds = grid%sd33 
   kde = grid%ed33 - 1

   ims = grid%sm31 
   ime = grid%em31 
   jms = grid%sm32 
   jme = grid%em32 
   kms = grid%sm33 
   kme = grid%em33 

   ips = grid%sp31 
   ipe = grid%ep31 
   jps = grid%sp32 
   jpe = grid%ep32 
   kps = grid%sp33 
   kpe = grid%ep33 

!--Indices for yz decomposition

   xp%idsx = grid%sd31
   xp%idex = grid%ed31 - 1
   xp%jdsx = grid%sd32
   xp%jdex = grid%ed32 - 1
   xp%kdsx = grid%sd33
   xp%kdex = grid%ed33 - 1

   xp%imsx = grid%sm31x
   xp%imex = grid%em31x
   xp%jmsx = grid%sm32x
   xp%jmex = grid%em32x
   xp%kmsx = grid%sm33x
   xp%kmex = grid%em33x

   xp%itsx = grid%sp31x
   xp%itex = grid%ep31x
   xp%jtsx = grid%sp32x
   xp%jtex = grid%ep32x
   xp%ktsx = grid%sp33x
   xp%ktex = grid%ep33x

   xp%ipsx = grid%sp31x
   xp%ipex = grid%ep31x
   xp%jpsx = grid%sp32x
   xp%jpex = grid%ep32x
   xp%kpsx = grid%sp33x
   xp%kpex = grid%ep33x

!--Indices for xz decomposition

   xp%idsy = grid%sd31
   xp%idey = grid%ed31 - 1
   xp%jdsy = grid%sd32
   xp%jdey = grid%ed32 - 1
   xp%kdsy = grid%sd33
   xp%kdey = grid%ed33 - 1

   xp%imsy = grid%sm31y
   xp%imey = grid%em31y
   xp%jmsy = grid%sm32y
   xp%jmey = grid%em32y
   xp%kmsy = grid%sm33y
   xp%kmey = grid%em33y

   xp%itsy = grid%sp31y
   xp%itey = grid%ep31y
   xp%jtsy = grid%sp32y
   xp%jtey = grid%ep32y
   xp%ktsy = grid%sp33y
   xp%ktey = grid%ep33y

   xp%ipsy = grid%sp31y
   xp%ipey = grid%ep31y
   xp%jpsy = grid%sp32y
   xp%jpey = grid%ep32y
   xp%kpsy = grid%sp33y
   xp%kpey = grid%ep33y

   if(ipe > ide) ipe = ide
   if(jpe > jde) jpe = jde
   if(kpe > kde) kpe = kde

   ! Indices for yz decomposition

   if(xp%itex > ide) xp%itex = ide
   if(xp%jtex > jde) xp%jtex = jde
   if(xp%ktex > kde) xp%ktex = kde

   if(xp%ipex > ide) xp%ipex = ide
   if(xp%jpex > jde) xp%jpex = jde
   if(xp%kpex > kde) xp%kpex = kde

   ! Indices for xz decomposition

   if(xp%itey > ide) xp%itey = ide
   if(xp%jtey > jde) xp%jtey = jde
   if(xp%ktey > kde) xp%ktey = kde

   if(xp%ipey > ide) xp%ipey = ide
   if(xp%jpey > jde) xp%jpey = jde
   if(xp%kpey > kde) xp%kpey = kde

!  Copy xpose dimensions from grid structure to xp structure.

!--Indices for xy decomposition

   xp%ids = ids
   xp%ide = ide
   xp%jds = jds
   xp%jde = jde
   xp%kds = kds
   xp%kde = kde

   xp%ims = ims
   xp%ime = ime
   xp%jms = jms
   xp%jme = jme
   xp%kms = kms
   xp%kme = kme

   xp%ips = ips
   xp%ipe = ipe
   xp%jps = jps
   xp%jpe = jpe
   xp%kps = kps
   xp%kpe = kpe

END SUBROUTINE copy_dims

SUBROUTINE copy_tile_dims( grid, xp, its, ite, jts, jte, kts, kte )

!------------------------------------------------------------------------------
!  PURPOSE: Copy tile dimensions from grid structure.
!
!------------------------------------------------------------------------------

!   USE module_domain
   TYPE(domain), INTENT(IN)         :: grid
   TYPE (xpose_type),INTENT(INOUT)  :: xp      ! Transpose variables.
   INTEGER,      INTENT(OUT)        :: its,ite, jts,jte, kts,kte ! tile dims.

   INTEGER                  :: ij   ! Loop counter

!  De-reference tile indices stored in the grid data structure.

   DO ij = 1 , grid%num_tiles
     its = grid%i_start(ij)
     ite = grid%i_end(ij)
     jts = grid%j_start(ij)
     jte = grid%j_end(ij)
     kts = xp%kds
     kte = xp%kde

     xp%its = its
     xp%ite = ite
     xp%jts = jts
     xp%jte = jte
     xp%kts = kts
     xp%kte = kte

     if(xp%ite > xp%ide) xp%ite = xp%ide
     if(xp%jte > xp%jde) xp%jte = xp%jde
     if(xp%kte > xp%kde) xp%kte = xp%kde

     if(ite > xp%ide) ite = xp%ide
     if(jte > xp%jde) jte = xp%jde
     if(kte > xp%kde) kte = xp%kde

        write(unit=*, fmt='(/)')
        write(unit=*, fmt='(A,i3,A,5x,3(i3,A,i3,5x))') 'Tile ',ij, &
                ' size:', its,':',ite, jts,':',jte, kts,':',kte
   END DO
END SUBROUTINE copy_tile_dims

END MODULE module_kma2netcdf_interface
