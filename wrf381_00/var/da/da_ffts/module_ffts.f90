module module_ffts

   !---------------------------------------------------------------------------
   ! Purpose: Routines to perform Fourier transforms.
   !---------------------------------------------------------------------------

   implicit none

   contains

#include "fft551.inc"
#include "fft661.inc"
#include "qpassm.inc"

end module module_ffts
