MODULE da_wavelet
!
! Purpose: wavelet routines by Aime' Fournier
!
 USE da_control,    ONLY: ite,its,jte,jts,kme,kms,trace_use_dull
 USE module_domain, ONLY: domain, vp_type
 IMPLICIT NONE
 CHARACTER            :: namw		! Wavelet name 'B', 'C', 'D' or 'V'.
 INTEGER              :: lf		! Wavelet filter length.
 INTEGER              :: nb		! Nu. wavelet bands.
 INTEGER, ALLOCATABLE :: nij(:,:,:)	! Wavelet indexes.
 LOGICAL              :: do_normalize	! Normalize before wavelet-transform.
 REAL, ALLOCATABLE    :: sd(:,:)	! Field standard deviations.
 REAL, ALLOCATABLE    :: ws(:)		! Wavelet scratch work space.
 REAL, ALLOCATABLE    :: wsd(:,:)	! Wavelet-coefficient std. devs.
CONTAINS
#include "da_transform_through_wavelet.inc"
#include "da_transform_through_wavelet_adj.inc"
END MODULE da_wavelet
