SUBROUTINE shift_domain_em ( grid , disp_x, disp_y, &
!
# include <em_dummy_args.inc>
!
                           )
   USE module_domain
   USE module_timing
   USE module_configure
   USE module_dm
   USE module_timing
   IMPLICIT NONE
  ! Arguments
   INTEGER disp_x, disp_y       ! number of parent domain points to move
   TYPE(domain) , POINTER                     :: grid
  ! Local 
   INTEGER i, j, ii
   INTEGER px, py       ! number and direction of nd points to move
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   TYPE (grid_config_rec_type)  :: config_flags
#ifdef DEREF_KLUDGE
! see <a href="www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm">here</a>
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

   !  Definitions of dummy arguments to solve
#include <em_dummy_decl.inc>

#ifdef MOVE_NESTS

#define COPY_IN
#include <em_scalar_derefs.inc>
#include "deref_kludge.h"
#ifdef DM_PARALLEL
#      include <em_data_calls.inc>
#endif

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   CALL get_ijk_from_grid (  grid ,                   &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )

   px = isign(parent_grid_ratio,disp_x)
   py = isign(parent_grid_ratio,disp_y)

   imask_nostag = 1
   imask_xstag = 1
   imask_ystag = 1
   imask_xystag = 1

   imask_nostag(ips:min(ide-1,ipe),jps:min(jde-1,jpe)) = 0
   imask_xstag(ips:ipe,jps:min(jde-1,jpe)) = 0
   imask_ystag(ips:min(ide-1,ipe),jps:jpe) = 0
   imask_xystag(ips:ipe,jps:jpe) = 0

#if 1

! shift the nest domain in x
   do ii = 1,abs(disp_x)
#include <em_shift_halo_x.inc>
   enddo

! shift the nest domain in y
   do ii = 1,abs(disp_y)
#include <em_shift_halo_y.inc>
   enddo

#endif

#define COPY_OUT
#include <em_scalar_derefs.inc>

#endif

END SUBROUTINE shift_domain_em

