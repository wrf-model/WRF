/*

   This WSM5 microphysics accelerated for the NVIDIA GPU.  It is experimental and
   is not supported as part of WRF.  There is additional information available
   at http://www.mmm.ucar.edu/people/michalakes.  Requests for assistance will be
   considered only on a case by case basis, favoring active collaborators.

   Required: a Linux x86 or x86_64 system with a CUDA-enabled NVIDIA GPU installed
   as a co-processor as well as the CUDA libraries on a directory in your system,
   for example:

      /usr/local/cuda/lib/libcublas.so

   included in the CUDA SDK 1.1 from NVIDIA (see nvidia.com).

   To use with WRF:

   1)  Compile this file and companion file as:

         gcc -c wsm5.cu.c
         gcc -c wsm5_gpu.cu.c

       producing wsm5.cu.o and wsm5_gpu.cu.o

   2)  configure WRF, generating a configure.wrf file for your system
       Note that serial and dmpar work with the GPU, but smpar
       and dm+sm may not.

   3)  Modify configure.wrf:

      a) add -DTEST_ON_GPU_RK -DRUN_ON_GPU to ARCH_LOCAL
      b) add ../phys/wsm5.cu.o and ../phys/wsm5_gpu.cu.o to LIB_LOCAL
         (define LIB_LOCAL it does not already exist)
      c) add -L/usr/local/cuda/lib -lcuda -lcudart to LIB_LOCAL
         (or wherever the cuda lib is on your system)

   3)  Compile wrf as usual.

   Note: The GPU code is compiled for a maximum number of 41 vertical levels
   If you need a larger number, contact below.

   20080721, JM  (michalak@ucar.edu)

*/

# 1 "/tmp/tmpxft_00001ecc_00000000-0.c"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "/tmp/tmpxft_00001ecc_00000000-0.c"
# 1 "y.cu"
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIcLi1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIaLi1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIhLi1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char1Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar1Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char2Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar2Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char3Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar3Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char4Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar4Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIsLi1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureItLi1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short1Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort1Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short2Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort2Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short3Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort3Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short4Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort4Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIiLi1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIjLi1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int1Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint1Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int2Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint2Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int3Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint3Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int4Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint4Li1EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIcLi1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIaLi1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIhLi1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char1Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar1Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char2Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar2Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char3Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar3Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char4Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar4Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIsLi1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureItLi1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short1Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort1Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short2Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort2Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short3Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort3Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short4Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort4Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIcLi2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIaLi2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIhLi2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char1Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar1Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char2Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar2Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char3Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar3Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char4Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar4Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIsLi2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureItLi2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short1Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort1Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short2Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort2Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short3Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort3Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short4Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort4Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIiLi2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIjLi2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int1Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint1Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int2Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint2Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int3Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint3Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int4Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint4Li2EL19cudaTextureReadMode0EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIiLi1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIjLi1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int1Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint1Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int2Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint2Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int3Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint3Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int4Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint4Li1EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIcLi2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIaLi2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIhLi2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char1Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar1Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char2Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar2Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char3Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar3Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char4Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar4Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIsLi2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureItLi2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short1Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort1Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short2Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort2Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short3Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort3Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short4Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort4Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIiLi2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIjLi2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int1Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint1Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int2Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint2Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int3Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint3Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int4Li2EL19cudaTextureReadMode1EE;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint4Li2EL19cudaTextureReadMode1EE;
# 268 "/usr/include/libio.h" 3
struct _IO_FILE;
# 214 "/usr/lib/gcc/x86_64-redhat-linux/4.1.2/include/stddef.h" 3
typedef unsigned long size_t;
# 1 "/usr/local/cuda/bin/../include/crt/host_runtime.h" 1 3
# 56 "/usr/local/cuda/bin/../include/crt/host_runtime.h" 3
typedef char bool;



# 1 "/usr/local/cuda/bin/../include/cuda_runtime_api.h" 1 3
# 48 "/usr/local/cuda/bin/../include/cuda_runtime_api.h" 3
# 1 "/usr/local/cuda/bin/../include/host_defines.h" 1 3
# 49 "/usr/local/cuda/bin/../include/cuda_runtime_api.h" 2 3
# 1 "/usr/local/cuda/bin/../include/builtin_types.h" 1 3
# 42 "/usr/local/cuda/bin/../include/builtin_types.h" 3
# 1 "/usr/local/cuda/bin/../include/device_types.h" 1 3
# 46 "/usr/local/cuda/bin/../include/device_types.h" 3
enum cudaRoundMode
{
  cudaRoundNearest,
  cudaRoundZero,
  cudaRoundPosInf,
  cudaRoundMinInf
};
# 43 "/usr/local/cuda/bin/../include/builtin_types.h" 2 3
# 1 "/usr/local/cuda/bin/../include/driver_types.h" 1 3
# 60 "/usr/local/cuda/bin/../include/driver_types.h" 3
enum cudaError
{
  cudaSuccess = 0,
  cudaErrorMissingConfiguration,
  cudaErrorMemoryAllocation,
  cudaErrorInitializationError,
  cudaErrorLaunchFailure,
  cudaErrorPriorLaunchFailure,
  cudaErrorLaunchTimeout,
  cudaErrorLaunchOutOfResources,
  cudaErrorInvalidDeviceFunction,
  cudaErrorInvalidConfiguration,
  cudaErrorInvalidDevice,
  cudaErrorInvalidValue,
  cudaErrorInvalidPitchValue,
  cudaErrorInvalidSymbol,
  cudaErrorMapBufferObjectFailed,
  cudaErrorUnmapBufferObjectFailed,
  cudaErrorInvalidHostPointer,
  cudaErrorInvalidDevicePointer,
  cudaErrorInvalidTexture,
  cudaErrorInvalidTextureBinding,
  cudaErrorInvalidChannelDescriptor,
  cudaErrorInvalidMemcpyDirection,
  cudaErrorAddressOfConstant,
  cudaErrorTextureFetchFailed,
  cudaErrorTextureNotBound,
  cudaErrorSynchronizationError,
  cudaErrorInvalidFilterSetting,
  cudaErrorInvalidNormSetting,
  cudaErrorMixedDeviceExecution,
  cudaErrorCudartUnloading,
  cudaErrorUnknown,
  cudaErrorNotYetImplemented,
  cudaErrorMemoryValueTooLarge,
  cudaErrorInvalidResourceHandle,
  cudaErrorNotReady,
  cudaErrorStartupFailure = 0x7f,
  cudaErrorApiFailureBase = 10000
};


enum cudaMemcpyKind
{
  cudaMemcpyHostToHost = 0,
  cudaMemcpyHostToDevice,
  cudaMemcpyDeviceToHost,
  cudaMemcpyDeviceToDevice
};


struct cudaDeviceProp
{
  char name[256];
  size_t totalGlobalMem;
  size_t sharedMemPerBlock;
  int regsPerBlock;
  int warpSize;
  size_t memPitch;
  int maxThreadsPerBlock;
  int maxThreadsDim[3];
  int maxGridSize[3];
  size_t totalConstMem;
  int major;
  int minor;
  int clockRate;
  size_t textureAlignment;
};
# 154 "/usr/local/cuda/bin/../include/driver_types.h" 3
typedef enum cudaError cudaError_t;


typedef int cudaStream_t;


typedef int cudaEvent_t;
# 44 "/usr/local/cuda/bin/../include/builtin_types.h" 2 3
# 1 "/usr/local/cuda/bin/../include/texture_types.h" 1 3
# 46 "/usr/local/cuda/bin/../include/texture_types.h" 3
struct cudaArray;


enum cudaChannelFormatKind
{
  cudaChannelFormatKindSigned,
  cudaChannelFormatKindUnsigned,
  cudaChannelFormatKindFloat
};


struct cudaChannelFormatDesc
{
  int x;
  int y;
  int z;
  int w;
  enum cudaChannelFormatKind f;
};


enum cudaTextureAddressMode
{
  cudaAddressModeWrap,
  cudaAddressModeClamp
};


enum cudaTextureFilterMode
{
  cudaFilterModePoint,
  cudaFilterModeLinear
};


enum cudaTextureReadMode
{
  cudaReadModeElementType,
  cudaReadModeNormalizedFloat
};


struct textureReference
{
  int normalized;
  enum cudaTextureFilterMode filterMode;
  enum cudaTextureAddressMode addressMode[2];
  struct cudaChannelFormatDesc channelDesc;
};
# 45 "/usr/local/cuda/bin/../include/builtin_types.h" 2 3
# 1 "/usr/local/cuda/bin/../include/vector_types.h" 1 3
# 54 "/usr/local/cuda/bin/../include/vector_types.h" 3
struct char1
{
  signed char x;
};


struct uchar1
{
  unsigned char x;
};


struct char2
{
  signed char x, y;
};


struct uchar2
{
  unsigned char x, y;
};


struct char3
{
  signed char x, y, z;
};


struct uchar3
{
  unsigned char x, y, z;
};


struct char4
{
  signed char x, y, z, w;
};


struct uchar4
{
  unsigned char x, y, z, w;
};


struct short1
{
  short x;
};


struct ushort1
{
  unsigned short x;
};


struct short2
{
  short x, y;
};


struct ushort2
{
  unsigned short x, y;
};


struct short3
{
  short x, y, z;
};


struct ushort3
{
  unsigned short x, y, z;
};


struct short4
{
  short x, y, z, w;
};


struct ushort4
{
  unsigned short x, y, z, w;
};


struct int1
{
  int x;
};


struct uint1
{
  unsigned int x;
};


struct int2
{
  int x, y;
};


struct uint2
{
  unsigned int x, y;
};


struct int3
{
  int x, y, z;
};


struct uint3
{
  unsigned int x, y, z;
};


struct int4
{
  int x, y, z, w;
};


struct uint4
{
  unsigned int x, y, z, w;
};


struct long1
{
  long x;
};


struct ulong1
{
  unsigned long x;
};


struct long2
{
  long x, y;
};


struct ulong2
{
  unsigned long x, y;
};


struct long3
{
  long x, y, z;
};


struct ulong3
{
  unsigned long x, y, z;
};


struct long4
{
  long x, y, z, w;
};


struct ulong4
{
  unsigned long x, y, z, w;
};


struct float1
{
  float x;
};


struct float2
{
  float x, y;
};


struct float3
{
  float x, y, z;
};


struct float4
{
  float x, y, z, w;
};


struct double2
{
  double x, y;
};
# 282 "/usr/local/cuda/bin/../include/vector_types.h" 3
typedef struct char1 char1;

typedef struct uchar1 uchar1;

typedef struct char2 char2;

typedef struct uchar2 uchar2;

typedef struct char3 char3;

typedef struct uchar3 uchar3;

typedef struct char4 char4;

typedef struct uchar4 uchar4;

typedef struct short1 short1;

typedef struct ushort1 ushort1;

typedef struct short2 short2;

typedef struct ushort2 ushort2;

typedef struct short3 short3;

typedef struct ushort3 ushort3;

typedef struct short4 short4;

typedef struct ushort4 ushort4;

typedef struct int1 int1;

typedef struct uint1 uint1;

typedef struct int2 int2;

typedef struct uint2 uint2;

typedef struct int3 int3;

typedef struct uint3 uint3;

typedef struct int4 int4;

typedef struct uint4 uint4;

typedef struct long1 long1;

typedef struct ulong1 ulong1;

typedef struct long2 long2;

typedef struct ulong2 ulong2;

typedef struct long3 long3;

typedef struct ulong3 ulong3;

typedef struct long4 long4;

typedef struct ulong4 ulong4;

typedef struct float1 float1;

typedef struct float2 float2;

typedef struct float3 float3;

typedef struct float4 float4;

typedef struct double2 double2;
# 363 "/usr/local/cuda/bin/../include/vector_types.h" 3
typedef struct dim3 dim3;


struct dim3
{
    unsigned int x, y, z;





};
# 45 "/usr/local/cuda/bin/../include/builtin_types.h" 2 3
# 50 "/usr/local/cuda/bin/../include/cuda_runtime_api.h" 2 3
# 82 "/usr/local/cuda/bin/../include/cuda_runtime_api.h" 3
extern cudaError_t cudaMalloc(void **devPtr, size_t size);
extern cudaError_t cudaMallocHost(void **ptr, size_t size);
extern cudaError_t cudaMallocPitch(void **devPtr, size_t *pitch, size_t width, size_t height);
extern cudaError_t cudaMallocArray(struct cudaArray **array, const struct cudaChannelFormatDesc *desc, size_t width, size_t height );
extern cudaError_t cudaFree(void *devPtr);
extern cudaError_t cudaFreeHost(void *ptr);
extern cudaError_t cudaFreeArray(struct cudaArray *array);
# 97 "/usr/local/cuda/bin/../include/cuda_runtime_api.h" 3
extern cudaError_t cudaMemcpy(void *dst, const void *src, size_t count, enum cudaMemcpyKind kind);
extern cudaError_t cudaMemcpyToArray(struct cudaArray *dst, size_t wOffset, size_t hOffset, const void *src, size_t count, enum cudaMemcpyKind kind);
extern cudaError_t cudaMemcpyFromArray(void *dst, const struct cudaArray *src, size_t wOffset, size_t hOffset, size_t count, enum cudaMemcpyKind kind);
extern cudaError_t cudaMemcpyArrayToArray(struct cudaArray *dst, size_t wOffsetDst, size_t hOffsetDst, const struct cudaArray *src, size_t wOffsetSrc, size_t hOffsetSrc, size_t count, enum cudaMemcpyKind kind );
extern cudaError_t cudaMemcpy2D(void *dst, size_t dpitch, const void *src, size_t spitch, size_t width, size_t height, enum cudaMemcpyKind kind);
extern cudaError_t cudaMemcpy2DToArray(struct cudaArray *dst, size_t wOffset, size_t hOffset, const void *src, size_t spitch, size_t width, size_t height, enum cudaMemcpyKind kind);
extern cudaError_t cudaMemcpy2DFromArray(void *dst, size_t dpitch, const struct cudaArray *src, size_t wOffset, size_t hOffset, size_t width, size_t height, enum cudaMemcpyKind kind);
extern cudaError_t cudaMemcpy2DArrayToArray(struct cudaArray *dst, size_t wOffsetDst, size_t hOffsetDst, const struct cudaArray *src, size_t wOffsetSrc, size_t hOffsetSrc, size_t width, size_t height, enum cudaMemcpyKind kind );
extern cudaError_t cudaMemcpyToSymbol(const char *symbol, const void *src, size_t count, size_t offset , enum cudaMemcpyKind kind );
extern cudaError_t cudaMemcpyFromSymbol(void *dst, const char *symbol, size_t count, size_t offset , enum cudaMemcpyKind kind );







extern cudaError_t cudaMemcpyAsync(void *dst, const void *src, size_t count, enum cudaMemcpyKind kind, cudaStream_t stream);
extern cudaError_t cudaMemcpyToArrayAsync(struct cudaArray *dst, size_t wOffset, size_t hOffset, const void *src, size_t count, enum cudaMemcpyKind kind, cudaStream_t stream);
extern cudaError_t cudaMemcpyFromArrayAsync(void *dst, const struct cudaArray *src, size_t wOffset, size_t hOffset, size_t count, enum cudaMemcpyKind kind, cudaStream_t stream);
extern cudaError_t cudaMemcpy2DAsync(void *dst, size_t dpitch, const void *src, size_t spitch, size_t width, size_t height, enum cudaMemcpyKind kind, cudaStream_t stream);
extern cudaError_t cudaMemcpy2DToArrayAsync(struct cudaArray *dst, size_t wOffset, size_t hOffset, const void *src, size_t spitch, size_t width, size_t height, enum cudaMemcpyKind kind, cudaStream_t stream);
extern cudaError_t cudaMemcpy2DFromArrayAsync(void *dst, size_t dpitch, const struct cudaArray *src, size_t wOffset, size_t hOffset, size_t width, size_t height, enum cudaMemcpyKind kind, cudaStream_t stream);







extern cudaError_t cudaMemset(void *mem, int c, size_t count);
extern cudaError_t cudaMemset2D(void *mem, size_t pitch, int c, size_t width, size_t height);







extern cudaError_t cudaGetSymbolAddress(void **devPtr, const char *symbol);
extern cudaError_t cudaGetSymbolSize(size_t *size, const char *symbol);







extern cudaError_t cudaGetDeviceCount(int *count);
extern cudaError_t cudaGetDeviceProperties(struct cudaDeviceProp *prop, int device);
extern cudaError_t cudaChooseDevice(int *device, const struct cudaDeviceProp *prop);
extern cudaError_t cudaSetDevice(int device);
extern cudaError_t cudaGetDevice(int *device);







extern cudaError_t cudaBindTexture(size_t *offset, const struct textureReference *texref, const void *devPtr, const struct cudaChannelFormatDesc *desc, size_t size );
extern cudaError_t cudaBindTextureToArray(const struct textureReference *texref, const struct cudaArray *array, const struct cudaChannelFormatDesc *desc);
extern cudaError_t cudaUnbindTexture(const struct textureReference *texref);
extern cudaError_t cudaGetTextureAlignmentOffset(size_t *offset, const struct textureReference *texref);
extern cudaError_t cudaGetTextureReference(const struct textureReference **texref, const char *symbol);







extern cudaError_t cudaGetChannelDesc(struct cudaChannelFormatDesc *desc, const struct cudaArray *array);
extern struct cudaChannelFormatDesc cudaCreateChannelDesc(int x, int y, int z, int w, enum cudaChannelFormatKind f);







extern cudaError_t cudaGetLastError(void);
extern const char* cudaGetErrorString(cudaError_t error);







extern cudaError_t cudaConfigureCall(dim3 gridDim, dim3 blockDim, size_t sharedMem , cudaStream_t stream );
extern cudaError_t cudaSetupArgument(const void *arg, size_t size, size_t offset);
extern cudaError_t cudaLaunch(const char *symbol);







extern cudaError_t cudaStreamCreate(cudaStream_t *stream);
extern cudaError_t cudaStreamDestroy(cudaStream_t stream);
extern cudaError_t cudaStreamSynchronize(cudaStream_t stream);
extern cudaError_t cudaStreamQuery(cudaStream_t stream);







extern cudaError_t cudaEventCreate(cudaEvent_t *event);
extern cudaError_t cudaEventRecord(cudaEvent_t event, cudaStream_t stream);
extern cudaError_t cudaEventQuery(cudaEvent_t event);
extern cudaError_t cudaEventSynchronize(cudaEvent_t event);
extern cudaError_t cudaEventDestroy(cudaEvent_t event);
extern cudaError_t cudaEventElapsedTime(float *ms, cudaEvent_t start, cudaEvent_t end);







extern cudaError_t cudaThreadExit(void);
extern cudaError_t cudaThreadSynchronize(void);
# 61 "/usr/local/cuda/bin/../include/crt/host_runtime.h" 2 3
# 1 "/usr/local/cuda/bin/../include/crt/storage_class.h" 1 3
# 62 "/usr/local/cuda/bin/../include/crt/host_runtime.h" 2 3
# 216 "/usr/lib/gcc/x86_64-redhat-linux/4.1.2/include/stddef.h" 2 3
# 148 "/usr/include/bits/types.h" 3
typedef long __clock_t;
# 61 "/usr/include/time.h" 3
typedef __clock_t clock_t;
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIcLi1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIaLi1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIhLi1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char1Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar1Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char2Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar2Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char3Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar3Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char4Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar4Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIsLi1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureItLi1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short1Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort1Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short2Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort2Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short3Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort3Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short4Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort4Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIiLi1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIjLi1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int1Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint1Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int2Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint2Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int3Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint3Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int4Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint4Li1EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIcLi1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIaLi1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIhLi1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char1Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar1Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char2Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar2Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char3Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar3Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char4Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar4Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIsLi1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureItLi1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short1Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort1Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short2Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort2Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short3Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort3Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short4Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort4Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIcLi2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIaLi2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIhLi2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char1Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar1Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char2Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar2Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char3Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar3Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char4Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar4Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIsLi2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureItLi2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short1Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort1Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short2Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort2Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short3Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort3Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short4Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort4Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIiLi2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIjLi2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int1Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint1Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int2Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint2Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int3Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint3Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int4Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint4Li2EL19cudaTextureReadMode0EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIiLi1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIjLi1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int1Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint1Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int2Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint2Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int3Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint3Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int4Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint4Li1EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIcLi2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIaLi2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIhLi2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char1Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar1Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char2Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar2Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char3Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar3Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5char4Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6uchar4Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIsLi2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureItLi2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short1Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort1Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short2Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort2Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short3Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort3Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI6short4Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI7ushort4Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIiLi2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureIjLi2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int1Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint1Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int2Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint2Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int3Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint3Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI4int4Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 115 "/usr/local/cuda/bin/../include/texture_types.h"
struct _Z7textureI5uint4Li2EL19cudaTextureReadMode1EE { struct textureReference __b_16textureReference;};
# 46 "/usr/include/stdio.h" 3
typedef struct _IO_FILE FILE;
void *memcpy(void*, const void*, size_t); void *memset(void*, int, size_t);
# 82 "/usr/local/cuda/bin/../include/cuda_runtime_api.h"
extern cudaError_t cudaMalloc(void **, size_t);



extern cudaError_t cudaFree(void *);
# 97 "/usr/local/cuda/bin/../include/cuda_runtime_api.h"
extern cudaError_t cudaMemcpy(void *, const void *, size_t, enum cudaMemcpyKind);
# 145 "/usr/local/cuda/bin/../include/cuda_runtime_api.h"
extern cudaError_t cudaGetDeviceCount(int *);
extern cudaError_t cudaGetDeviceProperties(struct cudaDeviceProp *, int);

extern cudaError_t cudaSetDevice(int);
# 187 "/usr/local/cuda/bin/../include/cuda_runtime_api.h"
extern cudaError_t cudaConfigureCall(dim3, dim3, size_t, cudaStream_t);
# 222 "/usr/local/cuda/bin/../include/cuda_runtime_api.h"
extern cudaError_t cudaThreadSynchronize(void);
# 184 "/usr/include/time.h" 3
extern __attribute__((__weak__)) clock_t clock(void);
# 58 "/usr/local/cuda/bin/../include/common_functions.h"
extern __attribute__((__weak__)) void *memset(void *, int, size_t);
# 65 "/usr/local/cuda/bin/../include/math_functions.h"
extern __attribute__((__weak__)) int abs(int) __attribute__((__const__));

extern __attribute__((__weak__)) long labs(long) __attribute__((__const__));

extern __attribute__((__weak__)) long long llabs(long long) __attribute__((__const__));

extern __attribute__((__weak__)) double fabs(double) __attribute__((__const__));

extern __attribute__((__weak__)) float fabsf(float) __attribute__((__const__));


extern __attribute__((__weak__)) int min(int, int);

extern __attribute__((__weak__)) unsigned umin(unsigned, unsigned);

extern __attribute__((__weak__)) float fminf(float, float);

extern __attribute__((__weak__)) double fmin(double, double);


extern __attribute__((__weak__)) int max(int, int);

extern __attribute__((__weak__)) unsigned umax(unsigned, unsigned);

extern __attribute__((__weak__)) float fmaxf(float, float);

extern __attribute__((__weak__)) double fmax(double, double);


extern __attribute__((__weak__)) double sin(double);

extern __attribute__((__weak__)) float sinf(float);


extern __attribute__((__weak__)) double cos(double);

extern __attribute__((__weak__)) float cosf(float);


extern __attribute__((__weak__)) void sincos(double, double *, double *);

extern __attribute__((__weak__)) void sincosf(float, float *, float *);


extern __attribute__((__weak__)) double tan(double);

extern __attribute__((__weak__)) float tanf(float);


extern __attribute__((__weak__)) double sqrt(double);

extern __attribute__((__weak__)) float sqrtf(float);


extern __attribute__((__weak__)) double rsqrt(double);

extern __attribute__((__weak__)) float rsqrtf(float);


extern __attribute__((__weak__)) double exp2(double);

extern __attribute__((__weak__)) float exp2f(float);


extern __attribute__((__weak__)) double exp10(double);

extern __attribute__((__weak__)) float exp10f(float);


extern __attribute__((__weak__)) double expm1(double);

extern __attribute__((__weak__)) float expm1f(float);


extern __attribute__((__weak__)) double log2(double);

extern __attribute__((__weak__)) float log2f(float);


extern __attribute__((__weak__)) double log10(double);

extern __attribute__((__weak__)) float log10f(float);


extern __attribute__((__weak__)) double log(double);

extern __attribute__((__weak__)) float logf(float);


extern __attribute__((__weak__)) double log1p(double);

extern __attribute__((__weak__)) float log1pf(float);


extern __attribute__((__weak__)) double floor(double) __attribute__((__const__));

extern __attribute__((__weak__)) float floorf(float) __attribute__((__const__));


extern __attribute__((__weak__)) double exp(double);

extern __attribute__((__weak__)) float expf(float);


extern __attribute__((__weak__)) double cosh(double);

extern __attribute__((__weak__)) float coshf(float);


extern __attribute__((__weak__)) double sinh(double);

extern __attribute__((__weak__)) float sinhf(float);


extern __attribute__((__weak__)) double tanh(double);

extern __attribute__((__weak__)) float tanhf(float);


extern __attribute__((__weak__)) double acosh(double);

extern __attribute__((__weak__)) float acoshf(float);


extern __attribute__((__weak__)) double asinh(double);

extern __attribute__((__weak__)) float asinhf(float);


extern __attribute__((__weak__)) double atanh(double);

extern __attribute__((__weak__)) float atanhf(float);


extern __attribute__((__weak__)) double ldexp(double, int);

extern __attribute__((__weak__)) float ldexpf(float, int);


extern __attribute__((__weak__)) double logb(double);

extern __attribute__((__weak__)) float logbf(float);


extern __attribute__((__weak__)) int ilogb(double);

extern __attribute__((__weak__)) int ilogbf(float);


extern __attribute__((__weak__)) double scalbn(double, int);

extern __attribute__((__weak__)) float scalbnf(float, int);


extern __attribute__((__weak__)) double scalbln(double, long);

extern __attribute__((__weak__)) float scalblnf(float, long);


extern __attribute__((__weak__)) double frexp(double, int *);

extern __attribute__((__weak__)) float frexpf(float, int *);


extern __attribute__((__weak__)) double round(double) __attribute__((__const__));

extern __attribute__((__weak__)) float roundf(float) __attribute__((__const__));


extern __attribute__((__weak__)) long lround(double);

extern __attribute__((__weak__)) long lroundf(float);


extern __attribute__((__weak__)) long long llround(double);

extern __attribute__((__weak__)) long long llroundf(float);


extern __attribute__((__weak__)) double rint(double);

extern __attribute__((__weak__)) float rintf(float);


extern __attribute__((__weak__)) long lrint(double);

extern __attribute__((__weak__)) long lrintf(float);


extern __attribute__((__weak__)) long long llrint(double);

extern __attribute__((__weak__)) long long llrintf(float);


extern __attribute__((__weak__)) double nearbyint(double);

extern __attribute__((__weak__)) float nearbyintf(float);


extern __attribute__((__weak__)) double ceil(double) __attribute__((__const__));

extern __attribute__((__weak__)) float ceilf(float) __attribute__((__const__));


extern __attribute__((__weak__)) double trunc(double) __attribute__((__const__));

extern __attribute__((__weak__)) float truncf(float) __attribute__((__const__));


extern __attribute__((__weak__)) double fdim(double, double);

extern __attribute__((__weak__)) float fdimf(float, float);


extern __attribute__((__weak__)) double atan2(double, double);

extern __attribute__((__weak__)) float atan2f(float, float);


extern __attribute__((__weak__)) double atan(double);

extern __attribute__((__weak__)) float atanf(float);


extern __attribute__((__weak__)) double asin(double);

extern __attribute__((__weak__)) float asinf(float);


extern __attribute__((__weak__)) double acos(double);

extern __attribute__((__weak__)) float acosf(float);


extern __attribute__((__weak__)) double hypot(double, double);

extern __attribute__((__weak__)) float hypotf(float, float);


extern __attribute__((__weak__)) double cbrt(double);

extern __attribute__((__weak__)) float cbrtf(float);


extern __attribute__((__weak__)) double pow(double, double);

extern __attribute__((__weak__)) float powf(float, float);


extern __attribute__((__weak__)) double modf(double, double *);

extern __attribute__((__weak__)) float modff(float, float *);


extern __attribute__((__weak__)) double fmod(double, double);

extern __attribute__((__weak__)) float fmodf(float, float);


extern __attribute__((__weak__)) double remainder(double, double);

extern __attribute__((__weak__)) float remainderf(float, float);


extern __attribute__((__weak__)) double remquo(double, double, int *);

extern __attribute__((__weak__)) float remquof(float, float, int *);


extern __attribute__((__weak__)) double erf(double);

extern __attribute__((__weak__)) float erff(float);


extern __attribute__((__weak__)) double erfc(double);

extern __attribute__((__weak__)) float erfcf(float);


extern __attribute__((__weak__)) double lgamma(double);

extern __attribute__((__weak__)) float lgammaf(float);


extern __attribute__((__weak__)) double tgamma(double);

extern __attribute__((__weak__)) float tgammaf(float);


extern __attribute__((__weak__)) double copysign(double, double) __attribute__((__const__));

extern __attribute__((__weak__)) float copysignf(float, float) __attribute__((__const__));


extern __attribute__((__weak__)) double nextafter(double, double) __attribute__((__const__));

extern __attribute__((__weak__)) float nextafterf(float, float) __attribute__((__const__));


extern __attribute__((__weak__)) double nan(const char *) __attribute__((__const__));

extern __attribute__((__weak__)) float nanf(const char *) __attribute__((__const__));


extern __attribute__((__weak__)) int __signbit(double) __attribute__((__const__));

extern __attribute__((__weak__)) int __signbitf(float) __attribute__((__const__));


extern __attribute__((__weak__)) int __isinf(double) __attribute__((__const__));

extern __attribute__((__weak__)) int __isinff(float) __attribute__((__const__));


extern __attribute__((__weak__)) int __isnan(double) __attribute__((__const__));

extern __attribute__((__weak__)) int __isnanf(float) __attribute__((__const__));


extern __attribute__((__weak__)) int __finite(double) __attribute__((__const__));

extern __attribute__((__weak__)) int __finitef(float) __attribute__((__const__));


extern __attribute__((__weak__)) double fma(double, double, double);

extern __attribute__((__weak__)) float fmaf(float, float, float);
# 193 "/usr/include/bits/mathcalls.h" 3
extern __attribute__((__weak__)) int __isinfl(long double) __attribute__((__const__));


extern __attribute__((__weak__)) int __finitel(long double) __attribute__((__const__));
# 231 "/usr/include/bits/mathcalls.h" 3
extern __attribute__((__weak__)) int __isnanl(long double) __attribute__((__const__));
# 350 "/usr/include/bits/mathcalls.h" 3
extern __attribute__((__weak__)) int __signbitl(long double) __attribute__((__const__));
# 589 "/usr/include/stdlib.h" 3
extern void *malloc(size_t) __attribute__((__malloc__));
# 327 "/usr/include/stdio.h" 3
extern int fprintf(FILE *, const char *, ...);
# 113 "y.cu"
extern int rsl_internal_microclock_(void);
# 135 "y.cu"
extern int gethostname(char *, size_t);
# 142 "y.cu"
extern int wsm5_gpu_init_(int *, int *, int *);
# 199 "y.cu"
extern int wsm5_host_(float *, float *, float *, float *, float *, float *, float *, float *, float *, float *, float *, float *, float *, float *, float *, float *, int *, int *, int *, int *, int *, int *, int *, int *, int *, int *, int *, int *, int *, int *, int *, int *, int *, int *);
# 470 "y.cu"
extern int get_wsm5_gpu_levels_(int *);
extern void __sti___29_tmpxft_00001ecc_00000000_2_ii_91788a12(void) __attribute__((__constructor__));
# 144 "/usr/include/stdio.h" 3
extern struct _IO_FILE *stderr;
# 1 "/tmp/tmpxft_00001ecc_00000000-0.stub.h" 1 3




extern void __device_stub__Z8wsm5_gpuPfS_S_S_S_S_S_S_S_S_S_S_S_S_S_fS_iiiiiiiiiiiiiiiiii(float *, float *, float *, float *, float *, float *, float *, float *, float *, float *, float *, float *, float *, float *, float *, float, float *, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int);
# 146 "/usr/include/stdio.h" 2 3
# 142 "y.cu"
int wsm5_gpu_init_( int *myproc, int *nproc, int *mydevice)
{
auto float x;
# 144 "y.cu"
auto float *x_d;
auto int s;
# 145 "y.cu"
auto int e;
auto int i;
# 146 "y.cu"
auto int dc;
auto cudaError_t cerr;
auto char hostname[64];
auto struct cudaDeviceProp dp;

cudaGetDeviceCount((&dc));
if (dc > 4)
{ fprintf(stderr, "warning: more than %d devices on node (%d)\n", 4, dc); dc = 4; }
fprintf(stderr, "Number of devices on this node: %d\n", dc);



i = ((*mydevice));
if (dc > 0)
{
if ((int)(cerr = (cudaSetDevice(i)))) {
fprintf(stderr, "    non-zero cerr %d\n", ((int)cerr));
}
}
gethostname(((char *)hostname), 64UL);
fprintf(stderr, "Setting device %02d for task %03d on host %s\n", i, ((*myproc)), ((char *)hostname));

if ((int)(cerr = (cudaGetDeviceProperties((&dp), i)))) {
fprintf(stderr, "Device %02d: cerr = %d\n", ((int)cerr));
} else {
fprintf(stderr, "Device %02d: name %s\n", i, ((char *)(&dp.name)));
fprintf(stderr, "Device %02d: mem       %d\n", i, ((dp.totalGlobalMem)));
fprintf(stderr, "Device %02d: smem      %d\n", i, ((dp.sharedMemPerBlock)));
fprintf(stderr, "Device %02d: nreg      %d\n", i, ((dp.regsPerBlock)));
fprintf(stderr, "Device %02d: warp      %d\n", i, ((dp.warpSize)));
fprintf(stderr, "Device %02d: pitch     %d\n", i, ((dp.memPitch)));
fprintf(stderr, "Device %02d: maxthrds  %d\n", i, ((dp.maxThreadsPerBlock)));
fprintf(stderr, "Device %02d: maxtdim   %d %d %d\n", i, (((int *)(&dp.maxThreadsDim))[0]), (((int *)(&dp.maxThreadsDim))[1]), (((int *)(&dp.maxThreadsDim))[2]));


fprintf(stderr, "Device %02d: maxgdim   %d %d %d\n", i, (((int *)(&dp.maxGridSize))[0]), (((int *)(&dp.maxGridSize))[1]), (((int *)(&dp.maxGridSize))[2]));


fprintf(stderr, "Device %02d: clock     %d\n", i, ((dp.clockRate)));
fprintf(stderr, "Device %02d: talign    %d\n", i, ((dp.textureAlignment)));
}


s = (rsl_internal_microclock_());
cudaMalloc(((void **)(&x_d)), 4UL);
cudaMemcpy(((void *)x_d), ((const void *)(&x)), 4UL, cudaMemcpyHostToDevice);
cudaFree(((void *)x_d));
e = (rsl_internal_microclock_());
fprintf(stderr, "wsm5_init: %d\n", (e - s));
return 0;
}


int wsm5_host_(
float *th, float *pii,
float *q,
float *qc, float *qi, float *qr, float *qs,
float *den, float *p, float *delz,



float *delt,
float *rain, float *rainncv,
float *sr,
float *snow, float *snowncv,
int *ids, int *ide, int *jds, int *jde, int *kds, int *kde,
int *ims, int *ime, int *jms, int *jme, int *kms, int *kme,
int *ips, int *ipe, int *jps, int *jpe, int *kps, int *kpe)

{ auto unsigned __T20;
auto unsigned __T21;
auto float *bigbuf;
auto int s;
# 218 "y.cu"
auto int e;
# 218 "y.cu"
auto int s2;
# 218 "y.cu"
auto int e2;
auto int d3;
auto int d2;
# 229 "y.cu"
auto int dips;
# 229 "y.cu"
auto int dipe;

auto int djps;
# 231 "y.cu"
auto int djpe;
auto int dkps;
# 232 "y.cu"
auto int dkpe;
# 242 "y.cu"
auto float *th_d;
auto float *pii_d;
auto float *q_d;
auto float *qc_d;
auto float *qi_d;
auto float *qr_d;
auto float *qs_d;
auto float *den_d;
auto float *p_d;
auto float *delz_d;



auto float *rain_d;
auto float *rainncv_d;
auto float *sr_d;
auto float *snow_d;
auto float *snowncv_d;
auto float retvals[100];



auto float *retvals_d;

auto int remx;
# 266 "y.cu"
auto int remy;




auto dim3 dimBlock;

auto dim3 dimGrid;
# 219 "y.cu"
d3 = ((((((*ime)) - ((*ims))) + 1) * ((((*jme)) - ((*jms))) + 1)) * ((((*kme)) - ((*kms))) + 1));
d2 = (((((*ime)) - ((*ims))) + 1) * ((((*jme)) - ((*jms))) + 1));
# 229 "y.cu"
dips = 0; dipe = ((((*ipe)) - ((*ips))) + 1);

djps = 0; djpe = ((((*jpe)) - ((*jps))) + 1);
dkps = 0; dkpe = ((((*kpe)) - ((*kps))) + 1);

bigbuf = ((float *)(malloc((((unsigned long)((dipe * djpe) * dkpe)) * 4UL))));
# 241 "y.cu"
s = (rsl_internal_microclock_());
cudaMalloc(((void **)(&th_d)), (((unsigned long)d3) * 4UL)); cudaMemcpy(((void *)th_d), ((const void *)th), (((unsigned long)d3) * 4UL), cudaMemcpyHostToDevice);
cudaMalloc(((void **)(&pii_d)), (((unsigned long)d3) * 4UL)); cudaMemcpy(((void *)pii_d), ((const void *)pii), (((unsigned long)d3) * 4UL), cudaMemcpyHostToDevice);
cudaMalloc(((void **)(&q_d)), (((unsigned long)d3) * 4UL)); cudaMemcpy(((void *)q_d), ((const void *)q), (((unsigned long)d3) * 4UL), cudaMemcpyHostToDevice);
cudaMalloc(((void **)(&qc_d)), (((unsigned long)d3) * 4UL)); cudaMemcpy(((void *)qc_d), ((const void *)qc), (((unsigned long)d3) * 4UL), cudaMemcpyHostToDevice);
cudaMalloc(((void **)(&qi_d)), (((unsigned long)d3) * 4UL)); cudaMemcpy(((void *)qi_d), ((const void *)qi), (((unsigned long)d3) * 4UL), cudaMemcpyHostToDevice);
cudaMalloc(((void **)(&qr_d)), (((unsigned long)d3) * 4UL)); cudaMemcpy(((void *)qr_d), ((const void *)qr), (((unsigned long)d3) * 4UL), cudaMemcpyHostToDevice);
cudaMalloc(((void **)(&qs_d)), (((unsigned long)d3) * 4UL)); cudaMemcpy(((void *)qs_d), ((const void *)qs), (((unsigned long)d3) * 4UL), cudaMemcpyHostToDevice);
cudaMalloc(((void **)(&den_d)), (((unsigned long)d3) * 4UL)); cudaMemcpy(((void *)den_d), ((const void *)den), (((unsigned long)d3) * 4UL), cudaMemcpyHostToDevice);
cudaMalloc(((void **)(&p_d)), (((unsigned long)d3) * 4UL)); cudaMemcpy(((void *)p_d), ((const void *)p), (((unsigned long)d3) * 4UL), cudaMemcpyHostToDevice);
cudaMalloc(((void **)(&delz_d)), (((unsigned long)d3) * 4UL)); cudaMemcpy(((void *)delz_d), ((const void *)delz), (((unsigned long)d3) * 4UL), cudaMemcpyHostToDevice);



cudaMalloc(((void **)(&rain_d)), (((unsigned long)d2) * 4UL)); cudaMemcpy(((void *)rain_d), ((const void *)rain), (((unsigned long)d2) * 4UL), cudaMemcpyHostToDevice);
cudaMalloc(((void **)(&rainncv_d)), (((unsigned long)d2) * 4UL)); cudaMemcpy(((void *)rainncv_d), ((const void *)rainncv), (((unsigned long)d2) * 4UL), cudaMemcpyHostToDevice);
cudaMalloc(((void **)(&sr_d)), (((unsigned long)d2) * 4UL)); cudaMemcpy(((void *)sr_d), ((const void *)sr), (((unsigned long)d2) * 4UL), cudaMemcpyHostToDevice);
cudaMalloc(((void **)(&snow_d)), (((unsigned long)d2) * 4UL)); cudaMemcpy(((void *)snow_d), ((const void *)snow), (((unsigned long)d2) * 4UL), cudaMemcpyHostToDevice);
cudaMalloc(((void **)(&snowncv_d)), (((unsigned long)d2) * 4UL)); cudaMemcpy(((void *)snowncv_d), ((const void *)snowncv), (((unsigned long)d2) * 4UL), cudaMemcpyHostToDevice);

{ auto int k;
for (k = 0; (k < ((((*kme)) - ((*kms))) + 1)); k++) { (((float *)retvals)[k]) = (0.0F); }
}
cudaMalloc(((void **)(&retvals_d)), (((unsigned long)((((*kme)) - ((*kms))) + 1)) * 4UL)); cudaMemcpy(((void *)retvals_d), ((const void *)((float *)retvals)), (((unsigned long)((((*kme)) - ((*kms))) + 1)) * 4UL), cudaMemcpyHostToDevice);



remx = ((((((*ipe)) - ((*ips))) + 1) % 16) ? 1 : 0);
remy = ((((((*jpe)) - ((*jps))) + 1) % 8) ? 1 : 0);

{ (dimBlock.x) = 16U; (dimBlock.y) = 8U; (dimBlock.z) = 1U; }

{ __T20 = ((unsigned)((((((*ipe)) - ((*ips))) + 1) / 16) + remx)); __T21 = ((unsigned)((((((*jpe)) - ((*jps))) + 1) / 8) + remy)); { (dimGrid.x) = __T20; (dimGrid.y) = __T21; (dimGrid.z) = 1U; } }

fprintf(stderr, "Call to wsm5_gpu: block dims %d %d\n", ((dimBlock.x)), ((dimBlock.y)));
fprintf(stderr, "Call to wsm5_gpu: grid  dims %d %d\n", ((dimGrid.x)), ((dimGrid.y)));
# 284 "y.cu"
s2 = (rsl_internal_microclock_());
((int)(cudaConfigureCall(dimGrid, dimBlock, 0UL, 0))) ? ((void)0) : (__device_stub__Z8wsm5_gpuPfS_S_S_S_S_S_S_S_S_S_S_S_S_S_fS_iiiiiiiiiiiiiiiiii(th_d, pii_d, q_d, qc_d, qi_d, qr_d, qs_d, den_d, p_d, delz_d, rain_d, rainncv_d, sr_d, snow_d, snowncv_d, ((*delt)), retvals_d, (dips + 1), ((((*ipe)) - ((*ips))) + 1), (djps + 1), ((((*jpe)) - ((*jps))) + 1), (dkps + 1), ((((*kpe)) - ((*kps))) + 1), (dips + 1), dipe, (djps + 1), djpe, (dkps + 1), dkpe, (dips + 1), dipe, (djps + 1), djpe, (dkps + 1), dkpe));
# 299 "y.cu"
cudaThreadSynchronize();
e2 = (rsl_internal_microclock_());
fprintf(stderr, "Call to wsm5_gpu (not including data xfer): %d microseconds\n", (e2 - s2));


cudaMemcpy(((void *)th), ((const void *)th_d), (((unsigned long)d3) * 4UL), cudaMemcpyDeviceToHost);
cudaMemcpy(((void *)pii), ((const void *)pii_d), (((unsigned long)d3) * 4UL), cudaMemcpyDeviceToHost);
cudaMemcpy(((void *)q), ((const void *)q_d), (((unsigned long)d3) * 4UL), cudaMemcpyDeviceToHost);
cudaMemcpy(((void *)qc), ((const void *)qc_d), (((unsigned long)d3) * 4UL), cudaMemcpyDeviceToHost);
cudaMemcpy(((void *)qi), ((const void *)qi_d), (((unsigned long)d3) * 4UL), cudaMemcpyDeviceToHost);
cudaMemcpy(((void *)qr), ((const void *)qr_d), (((unsigned long)d3) * 4UL), cudaMemcpyDeviceToHost);
cudaMemcpy(((void *)qs), ((const void *)qs_d), (((unsigned long)d3) * 4UL), cudaMemcpyDeviceToHost);



cudaMemcpy(((void *)rain), ((const void *)rain_d), (((unsigned long)d2) * 4UL), cudaMemcpyDeviceToHost);
cudaMemcpy(((void *)rainncv), ((const void *)rainncv_d), (((unsigned long)d2) * 4UL), cudaMemcpyDeviceToHost);
cudaMemcpy(((void *)sr), ((const void *)sr_d), (((unsigned long)d2) * 4UL), cudaMemcpyDeviceToHost);
cudaMemcpy(((void *)snow), ((const void *)snow_d), (((unsigned long)d2) * 4UL), cudaMemcpyDeviceToHost);
cudaMemcpy(((void *)snowncv), ((const void *)snowncv_d), (((unsigned long)d2) * 4UL), cudaMemcpyDeviceToHost);
e = (rsl_internal_microclock_());

cudaMemcpy(((void *)((float *)retvals)), ((const void *)retvals_d), (((unsigned long)((((*kme)) - ((*kms))) + 1)) * 4UL), cudaMemcpyDeviceToHost);
fprintf(stderr, "Call to wsm5_gpu (including data xfer): %d microseconds\n", (e - s));

{


}

cudaFree(((void *)th_d));
cudaFree(((void *)pii_d));
cudaFree(((void *)q_d));
cudaFree(((void *)qc_d));
cudaFree(((void *)qi_d));
cudaFree(((void *)qr_d));
cudaFree(((void *)qs_d));
cudaFree(((void *)den_d));
cudaFree(((void *)p_d));
cudaFree(((void *)delz_d));



cudaFree(((void *)rain_d));
cudaFree(((void *)rainncv_d));
cudaFree(((void *)sr_d));
cudaFree(((void *)snow_d));
cudaFree(((void *)snowncv_d));
cudaFree(((void *)retvals_d));

return 0;
}
# 470 "y.cu"
int get_wsm5_gpu_levels_( int *retval)
{
(*retval) = 41;
}
void __sti___29_tmpxft_00001ecc_00000000_2_ii_91788a12(void) { }
# 1 "/tmp/tmpxft_00001ecc_00000000-0.stub.c" 1



# 1 "/tmp/tmpxft_00001ecc_00000000-1.c" 1
# 1 "/usr/local/cuda/bin/../include/__cudaFatFormat.h" 1
# 97 "/usr/local/cuda/bin/../include/__cudaFatFormat.h"
typedef struct {
    char* gpuProfileName;
    char* cubin;
} __cudaFatCubinEntry;
# 113 "/usr/local/cuda/bin/../include/__cudaFatFormat.h"
typedef struct {
    char* gpuProfileName;
    char* ptx;
} __cudaFatPtxEntry;
# 125 "/usr/local/cuda/bin/../include/__cudaFatFormat.h"
typedef struct {
    char* gpuProfileName;
    char* debug;
} __cudaFatDebugEntry;


typedef enum {
      __cudaFatDontSearchFlag = (1 << 0),
      __cudaFatDontCacheFlag = (1 << 1)
} __cudaFatCudaBinaryFlag;
# 145 "/usr/local/cuda/bin/../include/__cudaFatFormat.h"
typedef struct {
    unsigned long magic;
    unsigned long version;
    unsigned long gpuInfoVersion;
    char* key;
    char* ident;
    char* usageMode;
    __cudaFatPtxEntry *ptx;
    __cudaFatCubinEntry *cubin;
    __cudaFatDebugEntry *debug;
    void* debugInfo;
    unsigned int flags;
} __cudaFatCudaBinary;
# 189 "/usr/local/cuda/bin/../include/__cudaFatFormat.h"
void fatGetCubinForGpu( __cudaFatCudaBinary *binary, char* gpuName, char* *cubin, char* *dbgInfoFile );
# 2 "/tmp/tmpxft_00001ecc_00000000-1.c" 2







static const unsigned char __deviceText[] = {
0x61,0x72,0x63,0x68,0x69,0x74,0x65,0x63,0x74,0x75,0x72,0x65,0x20,0x7b,0x73,0x6d,
0x5f,0x31,0x30,0x7d,0x0a,0x61,0x62,0x69,0x76,0x65,0x72,0x73,0x69,0x6f,0x6e,0x20,
0x7b,0x30,0x7d,0x0a,0x63,0x6f,0x64,0x65,0x20,0x20,0x7b,0x0a,0x09,0x6e,0x61,0x6d,
0x65,0x20,0x3d,0x20,0x5f,0x5f,0x64,0x75,0x6d,0x6d,0x79,0x5f,0x65,0x6e,0x74,0x72,
0x79,0x5f,0x5f,0x0a,0x09,0x6c,0x6d,0x65,0x6d,0x20,0x3d,0x20,0x30,0x0a,0x09,0x73,
0x6d,0x65,0x6d,0x20,0x3d,0x20,0x30,0x0a,0x09,0x72,0x65,0x67,0x20,0x3d,0x20,0x30,
0x0a,0x09,0x62,0x61,0x72,0x20,0x3d,0x20,0x30,0x0a,0x09,0x62,0x69,0x6e,0x63,0x6f,
0x64,0x65,0x20,0x20,0x7b,0x0a,0x09,0x09,0x30,0x78,0x66,0x30,0x30,0x30,0x30,0x30,
0x30,0x31,0x20,0x30,0x78,0x65,0x30,0x30,0x30,0x30,0x30,0x30,0x31,0x20,0x0a,0x09,
0x7d,0x0a,0x7d,0x0a,0x00
};





static __cudaFatPtxEntry __ptxEntries [] = {{0,0}};
static __cudaFatCubinEntry __cubinEntries[] = {{"sm_10",(char*)__deviceText},{0,0}};
static __cudaFatDebugEntry __debugEntries[] = {{0,0}};

static __cudaFatCudaBinary __fatDeviceText __attribute__ ((section (".nvFatBinSegment")))= {0x1ee55a01,0x00000002,0x840b5bca,"81bb892378501d16","y.cu"," ",__ptxEntries,__cubinEntries,__debugEntries,0,0};
# 5 "/tmp/tmpxft_00001ecc_00000000-0.stub.c" 2
# 1 "/usr/local/cuda/bin/../include/crt/host_runtime.h" 1
# 65 "/usr/local/cuda/bin/../include/crt/host_runtime.h"
# 1 "/usr/local/cuda/bin/../include/host_defines.h" 1
# 66 "/usr/local/cuda/bin/../include/crt/host_runtime.h" 2
# 88 "/usr/local/cuda/bin/../include/crt/host_runtime.h"
extern void** __cudaRegisterFatBinary(
  void *fatCubin
);

extern void __cudaUnregisterFatBinary(
  void **fatCubinHandle
);

extern void __cudaRegisterVar(
        void **fatCubinHandle,
        char *hostVar,
        char *deviceAddress,
  const char *deviceName,
        int ext,
        int size,
        int constant,
        int global
);

extern void __cudaRegisterTexture(
        void **fatCubinHandle,
  const struct textureReference *hostVar,
  const void **deviceAddress,
  const char *deviceName,
        int dim,
        int norm,
        int ext
);

extern void __cudaRegisterShared(
  void **fatCubinHandle,
  void **devicePtr
);

extern void __cudaRegisterFunction(
        void **fatCubinHandle,
  const char *hostFun,
        char *deviceFun,
  const char *deviceName,
        int thread_limit,
        uint3 *tid,
        uint3 *bid,
        dim3 *bDim,
        dim3 *gDim
);





static void **__cudaFatCubinHandle;

static void __cudaUnregisterBinaryUtil(void)
{
  __cudaUnregisterFatBinary(__cudaFatCubinHandle);
}



__attribute__((destructor)) static void __cudaUnregisterBinary(void)
{
  __cudaUnregisterBinaryUtil();
}
# 195 "/usr/local/cuda/bin/../include/crt/host_runtime.h"
# 1 "/usr/local/cuda/bin/../include/common_functions.h" 1
# 64 "/usr/local/cuda/bin/../include/common_functions.h"
# 1 "/usr/local/cuda/bin/../include/crt/func_macro.h" 1 3
# 65 "/usr/local/cuda/bin/../include/common_functions.h" 2

extern __attribute__((weak)) long __cuda_clock(void); long __cuda_clock(void)
{
  return clock();
}

extern __attribute__((weak)) void *__cuda_memset(void *s, int c, size_t n); void *__cuda_memset(void *s, int c, size_t n)
{
  char *p = (char*)s;

  while (n--) *p++ = (char)c;

  return s;
}
# 88 "/usr/local/cuda/bin/../include/common_functions.h"
# 1 "/usr/local/cuda/bin/../include/math_functions.h" 1 3
# 794 "/usr/local/cuda/bin/../include/math_functions.h" 3
extern __attribute__((weak)) int __cuda___signbitl(long double a); int __cuda___signbitl(long double a);
extern __attribute__((weak)) int __cuda___isinfl(long double a); int __cuda___isinfl(long double a);
extern __attribute__((weak)) int __cuda___isnanl(long double a); int __cuda___isnanl(long double a);
extern __attribute__((weak)) int __cuda___finitel(long double a); int __cuda___finitel(long double a);
# 834 "/usr/local/cuda/bin/../include/math_functions.h" 3
extern __attribute__((weak)) int __cuda_abs(int a); int __cuda_abs(int a)
{
  return abs(a);
}

extern __attribute__((weak)) float __cuda_fabsf(float a); float __cuda_fabsf(float a)
{
  return fabsf(a);
}

extern __attribute__((weak)) long long int __cuda_llabs(long long int a); long long int __cuda_llabs(long long int a)
{



  return llabs(a);

}

extern __attribute__((weak)) float __cuda_exp2f(float a); float __cuda_exp2f(float a)
{
  return exp2f(a);
}

# 1 "/usr/local/cuda/bin/../include/device_functions.h" 1 3
# 322 "/usr/local/cuda/bin/../include/device_functions.h" 3
# 1 "/usr/local/cuda/bin/../include/math_constants.h" 1 3
# 323 "/usr/local/cuda/bin/../include/device_functions.h" 2 3



       extern __attribute__((weak)) int __cuda___isnan(double a); int __cuda___isnan(double a);
       extern __attribute__((weak)) int __cuda___isnanf(float a); int __cuda___isnanf(float a);
static int __double2int_rz(double);
static unsigned int __double2uint_rz(double);
static long long int __double2ll_rz(double);
static unsigned long long int __double2ull_rz(double);
# 345 "/usr/local/cuda/bin/../include/device_functions.h" 3
static int __mulhi(int a, int b)
{
  long long int c = (long long int)a * (long long int)b;

  return (int)(c >> 32);
}

static unsigned int __umulhi(unsigned int a, unsigned int b)
{
  unsigned long long int c = (unsigned long long int)a * (unsigned long long int)b;

  return (unsigned int)(c >> 32);
}

static unsigned long long int __umul64hi(unsigned long long int a, unsigned long long int b)
{
  unsigned int a_lo = (unsigned int)a;
  unsigned long long int a_hi = a >> 32;
  unsigned int b_lo = (unsigned int)b;
  unsigned long long int b_hi = b >> 32;
  unsigned long long int m1 = a_lo * b_hi;
  unsigned long long int m2 = a_hi * b_lo;
  unsigned int carry;

  carry = (0ULL + __umulhi(a_lo, b_lo) + (unsigned int)m1 + (unsigned int)m2) >> 32;

  return a_hi * b_hi + (m1 >> 32) + (m2 >> 32) + carry;
}

static long long int __mul64hi(long long int a, long long int b)
{
  return __umul64hi(a, b) - (a < 0LL ? b : 0LL) - (b < 0LL ? a : 0LL);
}

static float __saturatef(float a)
{
  return a >= 1.0f ? 1.0f : a <= 0.0f ? 0.0f : a;
}

static unsigned int __sad(int a, int b, unsigned int c)
{
  long long int diff = (long long int)a - (long long int)b;

  return (unsigned int)(__cuda_llabs(diff) + (long long int)c);
}

static unsigned int __usad(unsigned int a, unsigned int b, unsigned int c)
{
  long long int diff = (long long int)a - (long long int)b;

  return (unsigned int)(__cuda_llabs(diff) + (long long int)c);
}

static int __mul24(int a, int b)
{
  a &= 0xffffff;
  a = (a & 0x800000) != 0 ? a | ~0xffffff : a;
  b &= 0xffffff;
  b = (b & 0x800000) != 0 ? b | ~0xffffff : b;

  return a * b;
}

static unsigned int __umul24(unsigned int a, unsigned int b)
{
  a &= 0xffffff;
  b &= 0xffffff;

  return a * b;
}

static float __int_as_float(int a)
{
  union {int a; float b;} u;

  u.a = a;

  return u.b;
}

static int __float_as_int(float a)
{
  union {float a; int b;} u;

  u.a = a;

  return u.b;
}

static long long int __internal_float2ll_kernel(float a, long long int max, long long int min, long long int nan, enum cudaRoundMode rndMode)
{
  unsigned long long int res, t = 0ULL;
  int shift;
  unsigned int ia;

  if (sizeof(a) == sizeof(double) && __cuda___isnan((double)a)) return nan; if (sizeof(a) == sizeof(float) && __cuda___isnanf((float)a)) return nan; if (a >= max) return max; if (a <= min) return min;
  ia = __float_as_int(a);
  shift = 189 - ((ia >> 23) & 0xff);
  res = (unsigned long long int)(((ia << 8) | 0x80000000) >> 1) << 32;
  if (shift >= 64) {
    t = res;
    res = 0;
  } else if (shift) {
    t = res << (64 - shift);
    res = res >> shift;
  }
  if (rndMode == cudaRoundNearest && (long long int)t < 0LL) {
    res += t == 0x8000000000000000ULL ? res & 1ULL : 1ULL;
  }
  else if (rndMode == cudaRoundMinInf && t != 0ULL && ia > 0x80000000) {
    res++;
  }
  else if (rndMode == cudaRoundPosInf && t != 0ULL && (int)ia > 0) {
    res++;
  }
  if ((int)ia < 0) res = (unsigned long long int)-(long long int)res;
  return (long long int)res;
}

static int __internal_float2int(float a, enum cudaRoundMode rndMode)
{
  return (int)__internal_float2ll_kernel(a, 2147483647LL, -2147483648LL, 0LL, rndMode);
}

static int __float2int_rz(float a)
{
  return __internal_float2int(a, cudaRoundZero);
}

static int __float2int_ru(float a)
{
  return __internal_float2int(a, cudaRoundPosInf);
}

static int __float2int_rd(float a)
{
  return __internal_float2int(a, cudaRoundMinInf);
}

static int __float2int_rn(float a)
{
  return __internal_float2int(a, cudaRoundNearest);
}

static long long int __internal_float2ll(float a, enum cudaRoundMode rndMode)
{
  return __internal_float2ll_kernel(a, 9223372036854775807LL, -9223372036854775807LL -1LL, -9223372036854775807LL -1LL, rndMode);
}

static long long int __float2ll_rz(float a)
{
  return __internal_float2ll(a, cudaRoundZero);
}

static long long int __float2ll_ru(float a)
{
  return __internal_float2ll(a, cudaRoundPosInf);
}

static long long int __float2ll_rd(float a)
{
  return __internal_float2ll(a, cudaRoundMinInf);
}

static long long int __float2ll_rn(float a)
{
  return __internal_float2ll(a, cudaRoundNearest);
}

static unsigned long long int __internal_float2ull_kernel(float a, unsigned long long int max, unsigned long long int nan, enum cudaRoundMode rndMode)
{
  unsigned long long int res, t = 0ULL;
  int shift;
  unsigned int ia;

  if (sizeof(a) == sizeof(double) && __cuda___isnan((double)a)) return nan; if (sizeof(a) == sizeof(float) && __cuda___isnanf((float)a)) return nan; if (a >= max) return max; if (a <= 0LL) return 0LL;
  ia = __float_as_int(a);
  shift = 190 - ((ia >> 23) & 0xff);
  res = (unsigned long long int)((ia << 8) | 0x80000000) << 32;
  if (shift >= 64) {
    t = res >> (int)(shift > 64);
    res = 0;
  } else if (shift) {
    t = res << (64 - shift);
    res = res >> shift;
  }
  if (rndMode == cudaRoundNearest && (long long int)t < 0LL) {
    res += t == 0x8000000000000000ULL ? res & 1ULL : 1ULL;
  }
  else if (rndMode == cudaRoundPosInf && t != 0ULL) {
    res++;
  }
  return res;
}

static unsigned int __internal_float2uint(float a, enum cudaRoundMode rndMode)
{
  return (unsigned int)__internal_float2ull_kernel(a, 4294967295U, 0U, rndMode);
}

static unsigned int __float2uint_rz(float a)
{
  return __internal_float2uint(a, cudaRoundZero);
}

static unsigned int __float2uint_ru(float a)
{
  return __internal_float2uint(a, cudaRoundPosInf);
}

static unsigned int __float2uint_rd(float a)
{
  return __internal_float2uint(a, cudaRoundMinInf);
}

static unsigned int __float2uint_rn(float a)
{
  return __internal_float2uint(a, cudaRoundNearest);
}

static unsigned long long int __internal_float2ull(float a, enum cudaRoundMode rndMode)
{
  return __internal_float2ull_kernel(a, 18446744073709551615ULL, 9223372036854775808ULL, rndMode);
}

static unsigned long long int __float2ull_rz(float a)
{
  return __internal_float2ull(a, cudaRoundZero);
}

static unsigned long long int __float2ull_ru(float a)
{
  return __internal_float2ull(a, cudaRoundPosInf);
}

static unsigned long long int __float2ull_rd(float a)
{
  return __internal_float2ull(a, cudaRoundMinInf);
}

static unsigned long long int __float2ull_rn(float a)
{
  return __internal_float2ull(a, cudaRoundNearest);
}

static int __internal_normalize64(unsigned long long int *a)
{
  int lz = 0;

  if ((*a & 0xffffffff00000000ULL) == 0ULL) {
    *a <<= 32;
    lz += 32;
  }
  if ((*a & 0xffff000000000000ULL) == 0ULL) {
    *a <<= 16;
    lz += 16;
  }
  if ((*a & 0xff00000000000000ULL) == 0ULL) {
    *a <<= 8;
    lz += 8;
  }
  if ((*a & 0xf000000000000000ULL) == 0ULL) {
    *a <<= 4;
    lz += 4;
  }
  if ((*a & 0xC000000000000000ULL) == 0ULL) {
    *a <<= 2;
    lz += 2;
  }
  if ((*a & 0x8000000000000000ULL) == 0ULL) {
    *a <<= 1;
    lz += 1;
  }
  return lz;
}

static int __internal_normalize(unsigned int *a)
{
  unsigned long long int t = (unsigned long long int)*a;
  int lz = __internal_normalize64(&t);

  *a = (unsigned int)(t >> 32);

  return lz - 32;
}

static float __internal_int2float_kernel(int a, enum cudaRoundMode rndMode)
{
  volatile union {
    float f;
    unsigned int i;
  } res;
  int shift;
  unsigned int t;
  res.i = a;
  if (a == 0) return res.f;
  if (a < 0) res.i = (unsigned int)-a;
  shift = __internal_normalize((unsigned int*)&res.i);
  t = res.i << 24;
  res.i = (res.i >> 8);
  res.i += (127 + 30 - shift) << 23;
  if (a < 0) res.i |= 0x80000000;
  if ((rndMode == cudaRoundNearest) && (t >= 0x80000000)) {
    res.i += (t == 0x80000000) ? (res.i & 1) : (t >> 31);
  }
  else if ((rndMode == cudaRoundMinInf) && t && (a < 0)) {
    res.i++;
  }
  else if ((rndMode == cudaRoundPosInf) && t && (a > 0)) {
    res.i++;
  }
  return res.f;
}

static float __int2float_rz(int a)
{
  return __internal_int2float_kernel(a, cudaRoundZero);
}

static float __int2float_ru(int a)
{
  return __internal_int2float_kernel(a, cudaRoundPosInf);
}

static float __int2float_rd(int a)
{
  return __internal_int2float_kernel(a, cudaRoundMinInf);
}

static float __int2float_rn(int a)
{
  return __internal_int2float_kernel(a, cudaRoundNearest);
}

static float __internal_uint2float_kernel(unsigned int a, enum cudaRoundMode rndMode)
{
  volatile union {
    float f;
    unsigned int i;
  } res;
  int shift;
  unsigned int t;
  res.i = a;
  if (a == 0) return res.f;
  shift = __internal_normalize((unsigned int*)&res.i);
  t = res.i << 24;
  res.i = (res.i >> 8);
  res.i += (127 + 30 - shift) << 23;
  if ((rndMode == cudaRoundNearest) && (t >= 0x80000000)) {
    res.i += (t == 0x80000000) ? (res.i & 1) : (t >> 31);
  }
  else if ((rndMode == cudaRoundPosInf) && t) {
    res.i++;
  }
  return res.f;
}

static float __uint2float_rz(unsigned int a)
{
  return __internal_uint2float_kernel(a, cudaRoundZero);
}

static float __uint2float_ru(unsigned int a)
{
  return __internal_uint2float_kernel(a, cudaRoundPosInf);
}

static float __uint2float_rd(unsigned int a)
{
  return __internal_uint2float_kernel(a, cudaRoundMinInf);
}

static float __uint2float_rn(unsigned int a)
{
  return __internal_uint2float_kernel(a, cudaRoundNearest);
}

static float __ll2float_rn(long long int a)
{
  return (float)a;
}

static float __ull2float_rn(unsigned long long int a)
{
  unsigned long long int temp;
  unsigned int res, t;
  int shift;
  if (a == 0ULL) return 0.0f;
  temp = a;
  shift = __internal_normalize64(&temp);
  temp = (temp >> 8) | ((temp & 0xffULL) ? 1ULL : 0ULL);
  res = (unsigned int)(temp >> 32);
  t = (unsigned int)temp;
  res += (127 + 62 - shift) << 23;
  res += t == 0x80000000 ? res & 1 : t >> 31;
  return __int_as_float(res);
}

static float __internal_fmul_kernel(float a, float b, int rndNearest)
{
  unsigned long long product;
  volatile union {
    float f;
    unsigned int i;
  } xx, yy;
  unsigned expo_x, expo_y;

  xx.f = a;
  yy.f = b;

  expo_y = 0xFF;
  expo_x = expo_y & (xx.i >> 23);
  expo_x = expo_x - 1;
  expo_y = expo_y & (yy.i >> 23);
  expo_y = expo_y - 1;

  if ((expo_x <= 0xFD) &&
      (expo_y <= 0xFD)) {
multiply:
    expo_x = expo_x + expo_y;
    expo_y = xx.i ^ yy.i;
    xx.i = xx.i & 0x00ffffff;
    yy.i = yy.i << 8;
    xx.i = xx.i | 0x00800000;
    yy.i = yy.i | 0x80000000;

    product = ((unsigned long long)xx.i) * yy.i;
    expo_x = expo_x - 127 + 2;
    expo_y = expo_y & 0x80000000;
    xx.i = (unsigned int)(product >> 32);
    yy.i = (unsigned int)(product & 0xffffffff);

    if (xx.i < 0x00800000) {
      xx.i = (xx.i << 1) | (yy.i >> 31);
      yy.i = (yy.i << 1);
      expo_x--;
    }
    if (expo_x <= 0xFD) {
      xx.i = xx.i | expo_y;
      xx.i = xx.i + (expo_x << 23);

      if (yy.i < 0x80000000) return xx.f;
      xx.i += (((yy.i == 0x80000000) ? (xx.i & 1) : (yy.i >> 31))
               && rndNearest);
      return xx.f;
    } else if ((int)expo_x >= 254) {

      xx.i = (expo_y | 0x7F800000) - (!rndNearest);
      return xx.f;
    } else {

      expo_x = ((unsigned int)-((int)expo_x));
      if (expo_x > 25) {

        xx.i = expo_y;
        return xx.f;
      } else {
        yy.i = (xx.i << (32 - expo_x)) | ((yy.i) ? 1 : 0);
        xx.i = expo_y + (xx.i >> expo_x);
        xx.i += (((yy.i == 0x80000000) ? (xx.i & 1) : (yy.i >> 31))
                 && rndNearest);
        return xx.f;
      }
    }
  } else {
    product = xx.i ^ yy.i;
    product = product & 0x80000000;
    if (!(xx.i & 0x7fffffff)) {
      if (expo_y != 254) {
        xx.i = (unsigned int)product;
        return xx.f;
      }
      expo_y = yy.i << 1;
      if (expo_y == 0xFF000000) {
        xx.i = expo_y | 0x00C00000;
      } else {
        xx.i = yy.i | 0x00400000;
      }
      return xx.f;
    }
    if (!(yy.i & 0x7fffffff)) {
      if (expo_x != 254) {
        xx.i = (unsigned int)product;
        return xx.f;
      }
      expo_x = xx.i << 1;
      if (expo_x == 0xFF000000) {
        xx.i = expo_x | 0x00C00000;
      } else {
        xx.i = xx.i | 0x00400000;
      }
      return xx.f;
    }
    if ((expo_y != 254) && (expo_x != 254)) {
      expo_y++;
      expo_x++;
      if (expo_x == 0) {
        expo_y |= xx.i & 0x80000000;




        xx.i = xx.i << 8;
        while (!(xx.i & 0x80000000)) {
          xx.i <<= 1;
          expo_x--;
        }
        xx.i = (xx.i >> 8) | (expo_y & 0x80000000);
        expo_y &= ~0x80000000;
        expo_y--;
        goto multiply;
      }
      if (expo_y == 0) {
        expo_x |= yy.i & 0x80000000;
        yy.i = yy.i << 8;
        while (!(yy.i & 0x80000000)) {
          yy.i <<= 1;
          expo_y--;
        }
        yy.i = (yy.i >> 8) | (expo_x & 0x80000000);
        expo_x &= ~0x80000000;
        expo_x--;
        goto multiply;
      }
    }
    expo_x = xx.i << 1;
    expo_y = yy.i << 1;

    if (expo_x > 0xFF000000) {

      xx.i = xx.i | 0x00400000;
      return xx.f;
    }

    if (expo_y > 0xFF000000) {

      xx.i = yy.i | 0x00400000;
      return xx.f;
    }
    xx.i = (unsigned int)product | 0x7f800000;
    return xx.f;
  }
}

static float __internal_fadd_kernel(float a, float b, int rndNearest)
{
  volatile union {
    float f;
    unsigned int i;
  } xx, yy;
  unsigned int expo_x;
  unsigned int expo_y;
  unsigned int temp;

  xx.f = a;
  yy.f = b;


  expo_y = yy.i << 1;
  if (expo_y > (xx.i << 1)) {
    expo_y = xx.i;
    xx.i = yy.i;
    yy.i = expo_y;
  }

  temp = 0xff;
  expo_x = temp & (xx.i >> 23);
  expo_x = expo_x - 1;
  expo_y = temp & (yy.i >> 23);
  expo_y = expo_y - 1;

  if ((expo_x <= 0xFD) &&
      (expo_y <= 0xFD)) {

add:
    expo_y = expo_x - expo_y;
    if (expo_y > 25) {
      expo_y = 31;
    }
    temp = xx.i ^ yy.i;
    xx.i = xx.i & ~0x7f000000;
    xx.i = xx.i | 0x00800000;
    yy.i = yy.i & ~0xff000000;
    yy.i = yy.i | 0x00800000;

    if ((int)temp < 0) {

      temp = 32 - expo_y;
      temp = (expo_y) ? (yy.i << temp) : 0;
      temp = (unsigned int)(-((int)temp));
      xx.i = xx.i - (yy.i >> expo_y) - (temp ? 1 : 0);
      if (xx.i & 0x00800000) {
        if (expo_x <= 0xFD) {
          xx.i = xx.i & ~0x00800000;
          xx.i = (xx.i + (expo_x << 23)) + 0x00800000;
          if (temp < 0x80000000) return xx.f;
          xx.i += (((temp == 0x80000000) ? (xx.i & 1) : (temp >> 31))
                   && rndNearest);
          return xx.f;
        }
      } else {
        if ((temp | (xx.i << 1)) == 0) {

          xx.i = 0;
          return xx.f;
        }

        yy.i = xx.i & 0x80000000;
        do {
          xx.i = (xx.i << 1) | (temp >> 31);
          temp <<= 1;
          expo_x--;
        } while (!(xx.i & 0x00800000));
        xx.i = xx.i | yy.i;
      }
    } else {

      temp = 32 - expo_y;
      temp = (expo_y) ? (yy.i << temp) : 0;
      xx.i = xx.i + (yy.i >> expo_y);
      if (!(xx.i & 0x01000000)) {
        if (expo_x <= 0xFD) {
          expo_y = xx.i & 1;
          xx.i = xx.i + (expo_x << 23);
          if (temp < 0x80000000) return xx.f;
          xx.i += (((temp == 0x80000000) ? expo_y : (temp >> 31))
                   && rndNearest);
          return xx.f;
        }
      } else {

        temp = (xx.i << 31) | (temp >> 1);

        xx.i = ((xx.i & 0x80000000) | (xx.i >> 1)) & ~0x40000000;
        expo_x++;
      }
    }
    if (expo_x <= 0xFD) {
      expo_y = xx.i & 1;
      xx.i += (((temp == 0x80000000) ? expo_y : (temp >> 31))
               && rndNearest);
      xx.i = xx.i + (expo_x << 23);
      return xx.f;
    }
    if ((int)expo_x >= 254) {

        xx.i = ((xx.i & 0x80000000) | 0x7f800000) - (!rndNearest);
        return xx.f;
    }

    expo_y = expo_x + 32;
    yy.i = xx.i & 0x80000000;
    xx.i = xx.i & ~0xff000000;

    expo_x = (unsigned int)(-((int)expo_x));
    temp = xx.i << expo_y | ((temp) ? 1 : 0);
    xx.i = yy.i | (xx.i >> expo_x);
    xx.i += (((temp == 0x80000000) ? (xx.i & 1) : (temp >> 31))
             && rndNearest);
    return xx.f;
  } else {

    if (!(yy.i << 1)) {
      if (xx.i == 0x80000000) {
        xx.i = yy.i;
      }
      return xx.f;
    }
    if ((expo_y != 254) && (expo_x != 254)) {

      if (expo_x == (unsigned int) -1) {
        temp = xx.i & 0x80000000;
        xx.i = xx.i << 8;
        while (!(xx.i & 0x80000000)) {
          xx.i <<= 1;
          expo_x--;
        }
        expo_x++;
        xx.i = (xx.i >> 8) | temp;
      }
      if (expo_y == (unsigned int) -1) {
        temp = yy.i & 0x80000000;
        yy.i = yy.i << 8;
        while (!(yy.i & 0x80000000)) {
          yy.i <<= 1;
          expo_y--;
        }
        expo_y++;
        yy.i = (yy.i >> 8) | temp;
      }
      goto add;
    }
    expo_x = xx.i << 1;
    expo_y = yy.i << 1;

    if (expo_x > 0xff000000) {

      xx.i = xx.i | 0x00400000;
      return xx.f;
    }

    if (expo_y > 0xff000000) {

      xx.i = yy.i | 0x00400000;
      return xx.f;
    }
    if ((expo_x == 0xff000000) && (expo_y == 0xff000000)) {




      expo_x = xx.i ^ yy.i;
      xx.i = xx.i | ((expo_x) ? 0xffc00000 : 0);
      return xx.f;
    }

    if (expo_y == 0xff000000) {
      xx.i = yy.i;
    }
    return xx.f;
  }
}

static float __fadd_rz(float a, float b)
{
  return __internal_fadd_kernel(a, b, 0);
}

static float __fmul_rz(float a, float b)
{
  return __internal_fmul_kernel(a, b, 0);
}

static float __fdividef(float a, float b)
{

  if (__cuda_fabsf(b) > 8.507059173e37f) {
    if (__cuda_fabsf(a) <= 3.402823466e38f) {
      return ((a / b) / 3.402823466e38f) / 3.402823466e38f;
    } else {
      return __int_as_float(0x7fffffff);
    }
  } else {
    return a / b;
  }
}

static void __brkpt(int c)
{

}

extern int __cudaSynchronizeThreads(void**, void*);



static inline __attribute__((always_inline)) void __syncthreads(void)
{
  volatile int _ = 0;
  L: if (__cudaSynchronizeThreads((void**)&&L, (void*)&_)) goto L;
}

static void __trap(void)
{
  __builtin_trap();
}
# 1139 "/usr/local/cuda/bin/../include/device_functions.h" 3
static float __sinf(float a)
{
  return sinf(a);
}

static float __cosf(float a)
{
  return cosf(a);
}

static float __log2f(float a)
{
  return log2f(a);
}







static float __internal_accurate_fdividef(float a, float b)
{
  if (__cuda_fabsf(b) > 8.507059173e37f) {
    a *= .25f;
    b *= .25f;
  }
  return __fdividef(a, b);
}

static float __tanf(float a)
{
  return __sinf(a) / __cosf(a);
}

static void __sincosf(float a, float *sptr, float *cptr)
{
  *sptr = __sinf(a);
  *cptr = __cosf(a);
}

static float __expf(float a)
{
  return __cuda_exp2f(a * 1.442695041f);
}

static float __exp10f(float a)
{
  return __cuda_exp2f(a * 3.321928094f);
}

static float __log10f(float a)
{
  return 0.301029996f * __log2f(a);
}

static float __logf(float a)
{
  return 0.693147181f * __log2f(a);
}

static float __powf(float a, float b)
{
  return __cuda_exp2f(b * __log2f(a));
}

static float fdividef(float a, float b)
{



  return __internal_accurate_fdividef(a, b);

}

static int __clz(int a)
{
  return (a)?(158-(__float_as_int(__uint2float_rz((unsigned int)a))>>23)):32;
}

static int __ffs(int a)
{
  return 32 - __clz (a & -a);
}

static int __clzll(long long int a)
{
  int ahi = ((int)(a >> 32));
  int alo = ((int)(a & 0xffffffffULL));
  int res;
  if (ahi) {
      res = 0;
  } else {
      res = 32;
      ahi = alo;
  }
  res = res + __clz(ahi);
  return res;
}

static int __ffsll(long long int a)
{
  return 64 - __clzll (a & -a);
}
# 1252 "/usr/local/cuda/bin/../include/device_functions.h" 3
static double fdivide(double a, double b)
{
  return (double)fdividef((float)a, (float)b);
}



static int __double2int_rz(double a)
{
  return __float2int_rz((float)a);
}

static unsigned int __double2uint_rz(double a)
{
  return __float2uint_rz((float)a);
}

static long long int __double2ll_rz(double a)
{
  return __float2ll_rz((float)a);
}

static unsigned long long int __double2ull_rz(double a)
{
  return __float2ull_rz((float)a);
}
# 1291 "/usr/local/cuda/bin/../include/device_functions.h" 3
# 1 "/usr/local/cuda/bin/../include/sm_11_atomic_functions.h" 1 3
# 214 "/usr/local/cuda/bin/../include/sm_11_atomic_functions.h" 3
static int __iAtomicAdd(int *address, int val)
{
  int old = *address;

  *address = old + val;

  return old;
}

static unsigned int __uAtomicAdd(unsigned int *address, unsigned int val)
{
  unsigned int old = *address;

  *address = old + val;

  return old;
}

static int __iAtomicExch(int *address, int val)
{
  int old = *address;

  *address = val;

  return old;
}

static unsigned int __uAtomicExch(unsigned int *address, unsigned int val)
{
  unsigned int old = *address;

  *address = val;

  return old;
}

static float __fAtomicExch(float *address, float val)
{
  float old = *address;

  *address = val;

  return old;
}

static int __iAtomicMin(int *address, int val)
{
  int old = *address;

  *address = old < val ? old : val;

  return old;
}

static unsigned int __uAtomicMin(unsigned int *address, unsigned int val)
{
  unsigned int old = *address;

  *address = old < val ? old : val;

  return old;
}

static int __iAtomicMax(int *address, int val)
{
  int old = *address;

  *address = old > val ? old : val;

  return old;
}

static unsigned int __uAtomicMax(unsigned int *address, unsigned int val)
{
  unsigned int old = *address;

  *address = old > val ? old : val;

  return old;
}

static unsigned int __uAtomicInc(unsigned int *address, unsigned int val)
{
  unsigned int old = *address;

  *address = (old >= val) ? 0 : old + 1;

  return old;
}

static unsigned int __uAtomicDec(unsigned int *address, unsigned int val)
{
  unsigned int old = *address;

  *address = ((old == 0) | (old > val)) ? val : (old - 1);

  return old;
}

static int __iAtomicAnd(int *address, int val)
{
  int old = *address;

  *address = old & val;

  return old;
}

static unsigned int __uAtomicAnd(unsigned int *address, unsigned int val)
{
  unsigned int old = *address;

  *address = old & val;

  return old;
}

static int __iAtomicOr(int *address, int val)
{
  int old = *address;

  *address = old | val;

  return old;
}

static unsigned int __uAtomicOr(unsigned int *address, unsigned int val)
{
  unsigned int old = *address;

  *address = old | val;

  return old;
}

static int __iAtomicXor(int *address, int val)
{
  int old = *address;

  *address = old ^ val;

  return old;
}

static unsigned int __uAtomicXor(unsigned int *address, unsigned int val)
{
  unsigned int old = *address;

  *address = old ^ val;

  return old;
}

static int __iAtomicCAS(int *address, int compare, int val)
{
  int old = *address;

  *address = old == compare ? val : old;

  return old;
}

static unsigned int __uAtomicCAS(unsigned int *address, unsigned int compare, unsigned int val)
{
  unsigned int old = *address;

  *address = old == compare ? val : old;

  return old;
}
# 1292 "/usr/local/cuda/bin/../include/device_functions.h" 2 3
# 1 "/usr/local/cuda/bin/../include/texture_fetch_functions.h" 1 3
# 2007 "/usr/local/cuda/bin/../include/texture_fetch_functions.h" 3
extern void __cudaTextureFetch(const void *tex, void *index, int integer, void *val);

static int4 __itexfetchi(const void *tex, int4 index)
{
  int4 val;

  __cudaTextureFetch(tex, &index, 1, &val);

  return val;
}

static uint4 __utexfetchi(const void *tex, int4 index)
{
  uint4 val;

  __cudaTextureFetch(tex, &index, 1, &val);

  return val;
}

static float4 __ftexfetchi(const void *tex, int4 index)
{
  float4 val;

  __cudaTextureFetch(tex, &index, 1, &val);

  return val;
}

static int4 __itexfetch(const void *tex, float4 index, int dim)
{
  int4 val;

  __cudaTextureFetch(tex, &index, 0, &val);

  return val;
}

static uint4 __utexfetch(const void *tex, float4 index, int dim)
{
  uint4 val;

  __cudaTextureFetch(tex, &index, 0, &val);

  return val;
}

static float4 __ftexfetch(const void *tex, float4 index, int dim)
{
  float4 val;

  __cudaTextureFetch(tex, &index, 0, &val);

  return val;
}
# 1293 "/usr/local/cuda/bin/../include/device_functions.h" 2 3
# 859 "/usr/local/cuda/bin/../include/math_functions.h" 2 3


extern __attribute__((weak)) int __cuda___signbitf(float a); int __cuda___signbitf(float a)
{
  return (int)((unsigned int)__float_as_int(a) >> 31);
}




extern __attribute__((weak)) float __cuda_copysignf(float a, float b); float __cuda_copysignf(float a, float b)
{
  return __int_as_float((__float_as_int(b) & 0x80000000) |
                        (__float_as_int(a) & ~0x80000000));
}
# 883 "/usr/local/cuda/bin/../include/math_functions.h" 3
extern __attribute__((weak)) int min(int a, int b); int min(int a, int b)
{
  return a < b ? a : b;
}

extern __attribute__((weak)) unsigned int umin(unsigned int a, unsigned int b); unsigned int umin(unsigned int a, unsigned int b)
{
  return a < b ? a : b;
}

extern __attribute__((weak)) int max(int a, int b); int max(int a, int b)
{
  return a > b ? a : b;
}

extern __attribute__((weak)) unsigned int umax(unsigned int a, unsigned int b); unsigned int umax(unsigned int a, unsigned int b)
{
  return a > b ? a : b;
}
# 967 "/usr/local/cuda/bin/../include/math_functions.h" 3
extern __attribute__((weak)) float __internal_nearbyintf(float a); float __internal_nearbyintf(float a)
{
  float fa = fabsf(a);

  if (fa >= 8388608.0f) {
    return a;
  } else {
    volatile float u = 8388608.0f + fa;

    u = u - 8388608.0f;
    return copysignf(u, a);
  }
}

extern __attribute__((weak)) float __internal_fminf(float a, float b); float __internal_fminf(float a, float b)
{
  volatile union {
    float f;
    unsigned int i;
  } cvta, cvtb;

  cvta.f = a;
  cvtb.f = b;
  if ((cvta.i << 1) > 0xff000000) return b;
  if ((cvtb.i << 1) > 0xff000000) return a;
  if ((cvta.i | cvtb.i) == 0x80000000) {
    return __int_as_float(0x80000000);
  }
  return a < b ? a : b;
}

extern __attribute__((weak)) float __internal_fmaxf(float a, float b); float __internal_fmaxf(float a, float b)
{
  volatile union {
    float f;
    unsigned int i;
  } cvta, cvtb;

  cvta.f = a;
  cvtb.f = b;
  if ((cvta.i << 1) > 0xff000000) return b;
  if ((cvtb.i << 1) > 0xff000000) return a;
  if ((cvta.f == 0.0f) && (cvtb.f == 0.0f)) {
    cvta.i &= cvtb.i;
    return cvta.f;
  }
  return a > b ? a : b;
}
# 1055 "/usr/local/cuda/bin/../include/math_functions.h" 3
extern __attribute__((weak)) long int __cuda_labs(long int a); long int __cuda_labs(long int a)
{
  return labs(a);
}

extern __attribute__((weak)) float __cuda_ceilf(float a); float __cuda_ceilf(float a)
{
  return ceilf(a);
}

extern __attribute__((weak)) float __cuda_floorf(float a); float __cuda_floorf(float a)
{
  return floorf(a);
}

extern __attribute__((weak)) float __cuda_sqrtf(float a); float __cuda_sqrtf(float a)
{
   return sqrtf(a);
}

extern __attribute__((weak)) float __cuda_rsqrtf(float a); float __cuda_rsqrtf(float a)
{
   return 1.0f / sqrtf(a);
}

extern __attribute__((weak)) float __cuda_truncf(float a); float __cuda_truncf(float a)
{
  return truncf(a);
}

extern __attribute__((weak)) int __cuda_max(int a, int b); int __cuda_max(int a, int b)
{
  return max(a, b);
}

extern __attribute__((weak)) int __cuda_min(int a, int b); int __cuda_min(int a, int b)
{
  return min(a, b);
}

extern __attribute__((weak)) unsigned int __cuda_umax(unsigned int a, unsigned int b); unsigned int __cuda_umax(unsigned int a, unsigned int b)
{
  return umax(a, b);
}

extern __attribute__((weak)) unsigned int __cuda_umin(unsigned int a, unsigned int b); unsigned int __cuda_umin(unsigned int a, unsigned int b)
{
  return umin(a, b);
}

extern __attribute__((weak)) long long int __cuda_llrintf(float a); long long int __cuda_llrintf(float a)
{
  return __float2ll_rn(a);
}

extern __attribute__((weak)) long int __cuda_lrintf(float a); long int __cuda_lrintf(float a)
{

  return (long int)__cuda_llrintf(a);



}

extern __attribute__((weak)) float __cuda_nearbyintf(float a); float __cuda_nearbyintf(float a)
{



  return __internal_nearbyintf(a);

}

extern __attribute__((weak)) float __cuda_fmaxf(float a, float b); float __cuda_fmaxf(float a, float b)
{



  return __internal_fmaxf(a, b);

}

extern __attribute__((weak)) float __cuda_fminf(float a, float b); float __cuda_fminf(float a, float b)
{



  return __internal_fminf(a, b);

}
# 1162 "/usr/local/cuda/bin/../include/math_functions.h" 3
extern __attribute__((weak)) int __cuda___finitef(float a); int __cuda___finitef(float a)
{
  return __cuda_fabsf(a) < __int_as_float(0x7f800000);
}

extern __attribute__((weak)) int __cuda___isinff(float a); int __cuda___isinff(float a)
{
  return __cuda_fabsf(a) == __int_as_float(0x7f800000);
}

extern __attribute__((weak)) int __cuda___isnanf(float a); int __cuda___isnanf(float a)
{
  return !(__cuda_fabsf(a) <= __int_as_float(0x7f800000));
}

extern __attribute__((weak)) float __cuda_nextafterf(float a, float b); float __cuda_nextafterf(float a, float b)
{
  unsigned int ia;
  unsigned int ib;
  ia = __float_as_int(a);
  ib = __float_as_int(b);




  if (__cuda___isnanf(a) || __cuda___isnanf(b)) return a + b;
  if (__int_as_float (ia | ib) == 0.0f) return b;





  if (__int_as_float(ia) == 0.0f) {
    return __cuda_copysignf(__int_as_float(0x00000001), b);
  }

  if ((a < b) && (a < 0.0f)) ia--;
  if ((a < b) && (a > 0.0f)) ia++;
  if ((a > b) && (a < 0.0f)) ia++;
  if ((a > b) && (a > 0.0f)) ia--;
  a = __int_as_float(ia);





  return a;
}

extern __attribute__((weak)) float __cuda_nanf(const char *tagp); float __cuda_nanf(const char *tagp)
{

  return __int_as_float(0x7fffffff);
}


extern __attribute__((weak)) float __internal_atanhf_kernel(float a_1, float a_2); float __internal_atanhf_kernel(float a_1, float a_2)
{
  float a, a2, t;

  a = a_1 + a_2;
  a2 = a * a;
  t = 1.566305595598990E-001f/64.0f;
  t = t * a2 + 1.995081856004762E-001f/16.0f;
  t = t * a2 + 3.333382699617026E-001f/4.0f;
  t = t * a2;
  t = t * a + a_2;
  t = t + a_1;
  return t;
}




extern __attribute__((weak)) float __internal_atanf_kernel(float a); float __internal_atanf_kernel(float a)
{
  float t4, t0, t1;

  t4 = a * a;
  t0 = - 5.674867153f;
  t0 = t4 * - 0.823362947f + t0;
  t0 = t0 * t4 - 6.565555096f;
  t0 = t0 * t4;
  t0 = t0 * a;
  t1 = t4 + 11.33538818f;
  t1 = t1 * t4 + 28.84246826f;
  t1 = t1 * t4 + 19.69667053f;
  t1 = 1.0f / t1;
  a = t0 * t1 + a;
  return a;
}


extern __attribute__((weak)) float __internal_tan_kernel(float a); float __internal_tan_kernel(float a)
{
  float a2, s, t;

  a2 = a * a;
  t = 4.114678393115178E-003f * a2 - 8.231194034909670E-001f;
  s = a2 - 2.469348886157666E+000f;
  s = 1.0f / s;
  t = t * s;
  t = t * a2;
  t = t * a + a;
  return t;
}

extern __attribute__((weak)) float __internal_accurate_logf(float a); float __internal_accurate_logf(float a)
{
  float t;
  float z;
  float m;
  int ia, e;
  ia = __float_as_int(a);

  if ((ia < 0x00800000) || (ia > 0x7f7fffff)) {
    return __logf(a);
  }

  m = __int_as_float((ia & 0x807fffff) | 0x3f800000);
  e = ((unsigned)ia >> 23) - 127;
  if (m > 1.414213562f) {
    m = m * 0.5f;
    e = e + 1;
  }
  t = m - 1.0f;
  z = m + 1.0f;
  z = t / z;
  z = -t * z;
  z = __internal_atanhf_kernel(t, z);
  z = (float)e * 0.693147181f + z;
  return z;
}

extern __attribute__((weak)) float __internal_accurate_log2f(float a); float __internal_accurate_log2f(float a)
{
  return 1.442695041f * __internal_accurate_logf(a);
}


static unsigned int __cudart_i2opi_f [] = {
  0x3c439041,
  0xdb629599,
  0xf534ddc0,
  0xfc2757d1,
  0x4e441529,
  0xa2f9836e,
};


extern __attribute__((weak)) float __internal_trig_reduction_kernel(float a, int *quadrant); float __internal_trig_reduction_kernel(float a, int *quadrant)
{
  float j;
  int q;
  if (__cuda_fabsf(a) > 48039.0f) {

    unsigned int ia = __float_as_int(a);
    unsigned int s = ia & 0x80000000;
    unsigned int result[7];
    unsigned int phi, plo;
    unsigned int hi, lo;
    unsigned int e;
    int idx;
    e = ((ia >> 23) & 0xff) - 128;
    ia = (ia << 8) | 0x80000000;

    idx = 4 - (e >> 5);
    hi = 0;



    for (q = 0; q < 6; q++) {
      plo = __cudart_i2opi_f[q] * ia;
      phi = __umulhi (__cudart_i2opi_f[q], ia);
      lo = hi + plo;
      hi = phi + (lo < plo);
      result[q] = lo;
    }
    result[q] = hi;
    e = e & 31;



    hi = result[idx+2];
    lo = result[idx+1];
    if (e) {
      q = 32 - e;
      hi = (hi << e) | (lo >> q);
      lo = (lo << e) | (result[idx] >> q);
    }
    q = hi >> 30;

    hi = (hi << 2) | (lo >> 30);
    lo = (lo << 2);
    e = (hi + (lo > 0)) > 0x80000000;
    q += e;
    if (s) q = -q;
    if (e) {
      unsigned int t;
      hi = ~hi;
      lo = -(int)lo;
      t = (lo == 0);
      hi += t;
      s = s ^ 0x80000000;
    }
    *quadrant = q;

    e = 0;
    while ((int)hi > 0) {
      hi = (hi << 1) | (lo >> 31);
      lo = (lo << 1);
      e--;
    }
    lo = hi * 0xc90fdaa2;
    hi = __umulhi(hi, 0xc90fdaa2);
    if ((int)hi > 0) {
      hi = (hi << 1) | (lo >> 31);
      lo = (lo << 1);
      e--;
    }
    hi = hi + (lo > 0);
    ia = s | (((e + 126) << 23) + (hi >> 8) + ((hi << 24) >= 0x80000000));
    return __int_as_float(ia);
  }
  q = __float2int_rn(a * 0.636619772f);
  j = (float)q;
  a = a - j * 1.5703125000000000e+000f;
  a = a - j * 4.8351287841796875e-004f;
  a = a - j * 3.1385570764541626e-007f;
  a = a - j * 6.0771005065061922e-011f;
  *quadrant = q;
  return a;
}
# 1405 "/usr/local/cuda/bin/../include/math_functions.h" 3
extern __attribute__((weak)) float __internal_expf_kernel(float a, float scale); float __internal_expf_kernel(float a, float scale)
{
  float j, z;

  j = __cuda_truncf(a * 1.442695041f);
  z = a - j * 0.6931457519f;
  z = z - j * 1.4286067653e-6f;
  z = z * 1.442695041f;
  z = __cuda_exp2f(z) * __cuda_exp2f(j + scale);
  return z;
}

extern __attribute__((weak)) float __internal_accurate_expf(float a); float __internal_accurate_expf(float a)
{
  float z;
  z = __internal_expf_kernel(a, 0.0f);
  if (a < -105.0f) z = 0.0f;
  if (a > 105.0f) z = __int_as_float(0x7f800000);
  return z;
}

extern __attribute__((weak)) float __internal_accurate_exp10f(float a); float __internal_accurate_exp10f(float a)
{
  float j, z;
  j = __cuda_truncf(a * 3.321928094f);
  z = a - j * 3.0102920532226563e-001f;
  z = z - j * 7.9034171557301747e-007f;
  z = z * 3.321928094f;
  z = __cuda_exp2f(z) * __cuda_exp2f(j);
  if (a < -46.0f) z = 0.0f;
  if (a > 46.0f) z = __int_as_float(0x7f800000);
  return z;
}

extern __attribute__((weak)) float __internal_lgammaf_pos(float a); float __internal_lgammaf_pos(float a)
{
  float sum;
  float s, t;

  if (__cuda___isinff(a)) {
    return a;
  }
  if (a >= 3.0f) {
    if (a >= 7.8f) {



      s = 1.0f / a;
      t = s * s;
      sum = 0.77783067e-3f;
      sum = sum * t - 0.2777655457e-2f;
      sum = sum * t + 0.83333273853e-1f;
      sum = sum * s + 0.918938533204672f;
      s = 0.5f * __internal_accurate_logf(a);
      t = a - 0.5f;
      s = s * t;
      t = s - a;
      s = s + sum;
      t = t + s;
      return t;
    } else {
      a = a - 3.0f;
      s = - 7.488903254816711E+002f;
      s = s * a - 1.234974215949363E+004f;
      s = s * a - 4.106137688064877E+004f;
      s = s * a - 4.831066242492429E+004f;
      s = s * a - 1.430333998207429E+005f;
      t = a - 2.592509840117874E+002f;
      t = t * a - 1.077717972228532E+004f;
      t = t * a - 9.268505031444956E+004f;
      t = t * a - 2.063535768623558E+005f;
      t = s / t;
      t = t + a;
      return t;
    }
  } else if (a >= 1.5f) {
    a = a - 2.0f;
    t = + 4.959849168282574E-005f;
    t = t * a - 2.208948403848352E-004f;
    t = t * a + 5.413142447864599E-004f;
    t = t * a - 1.204516976842832E-003f;
    t = t * a + 2.884251838546602E-003f;
    t = t * a - 7.382757963931180E-003f;
    t = t * a + 2.058131963026755E-002f;
    t = t * a - 6.735248600734503E-002f;
    t = t * a + 3.224670187176319E-001f;
    t = t * a + 4.227843368636472E-001f;
    t = t * a;
    return t;
  } else if (a >= 0.7f) {
    a = 1.0f - a;
    t = + 4.588266515364258E-002f;
    t = t * a + 1.037396712740616E-001f;
    t = t * a + 1.228036339653591E-001f;
    t = t * a + 1.275242157462838E-001f;
    t = t * a + 1.432166835245778E-001f;
    t = t * a + 1.693435824224152E-001f;
    t = t * a + 2.074079329483975E-001f;
    t = t * a + 2.705875136435339E-001f;
    t = t * a + 4.006854436743395E-001f;
    t = t * a + 8.224669796332661E-001f;
    t = t * a + 5.772156651487230E-001f;
    t = t * a;
    return t;
  } else {
    t = + 3.587515669447039E-003f;
    t = t * a - 5.471285428060787E-003f;
    t = t * a - 4.462712795343244E-002f;
    t = t * a + 1.673177015593242E-001f;
    t = t * a - 4.213597883575600E-002f;
    t = t * a - 6.558672843439567E-001f;
    t = t * a + 5.772153712885004E-001f;
    t = t * a;
    t = t * a + a;
    return -__internal_accurate_logf(t);
  }
}


extern __attribute__((weak)) float __internal_sin_kernel(float x); float __internal_sin_kernel(float x)
{
  float x2, z;

  x2 = x * x;
  z = - 1.95152959e-4f;
  z = z * x2 + 8.33216087e-3f;
  z = z * x2 - 1.66666546e-1f;
  z = z * x2;
  z = z * x + x;

  return z;
}


extern __attribute__((weak)) float __internal_cos_kernel(float x); float __internal_cos_kernel(float x)
{
  float x2, z;

  x2 = x * x;
  z = 2.44331571e-5f;
  z = z * x2 - 1.38873163e-3f;
  z = z * x2 + 4.16666457e-2f;
  z = z * x2 - 5.00000000e-1f;
  z = z * x2 + 1.00000000e+0f;
  return z;
}

extern __attribute__((weak)) float __internal_accurate_sinf(float a); float __internal_accurate_sinf(float a)
{
  float z;
  int i;

  if (__cuda___isinff(a)) {
    return __int_as_float(0x7fffffff);
  }
  if (a == 0.0f) {
    return a;
  }
  z = __internal_trig_reduction_kernel(a, &i);

  if (i & 1) {
    z = __internal_cos_kernel(z);
  } else {
    z = __internal_sin_kernel(z);
  }
  if (i & 2) {
    z = -z;
  }
  return z;
}







extern __attribute__((weak)) float __cuda_rintf(float a); float __cuda_rintf(float a)
{
  return __cuda_nearbyintf(a);
}

extern __attribute__((weak)) float __cuda_sinf(float a); float __cuda_sinf(float a)
{



  return __internal_accurate_sinf(a);

}

extern __attribute__((weak)) float __cuda_cosf(float a); float __cuda_cosf(float a)
{



  float z;
  int i;

  if (__cuda___isinff(a)) {
    return __int_as_float(0x7fffffff);
  }
  z = __internal_trig_reduction_kernel(a, &i);

  i++;
  if (i & 1) {
    z = __internal_cos_kernel(z);
  } else {
    z = __internal_sin_kernel(z);
  }
  if (i & 2) {
    z = -z;
  }
  return z;

}

extern __attribute__((weak)) float __cuda_tanf(float a); float __cuda_tanf(float a)
{



  float z;
  int i;

  if (__cuda___isinff(a)) {
    return __int_as_float(0x7fffffff);
  }
  z = __internal_trig_reduction_kernel(a, &i);

  z = __internal_tan_kernel(z);
  if (i & 1) {
    z = -1.0f / z;
  }
  return z;

}

extern __attribute__((weak)) float __cuda_log2f(float a); float __cuda_log2f(float a)
{



  return __internal_accurate_log2f(a);

}

extern __attribute__((weak)) float __cuda_expf(float a); float __cuda_expf(float a)
{



  return __internal_accurate_expf(a);

}

extern __attribute__((weak)) float __cuda_exp10f(float a); float __cuda_exp10f(float a)
{



  return __internal_accurate_exp10f(a);

}

extern __attribute__((weak)) float __cuda_coshf(float a); float __cuda_coshf(float a)
{
  float z;

  a = __cuda_fabsf(a);
  z = __internal_expf_kernel(a, -2.0f);
  z = 2.0f * z + 0.125f / z;
  if (a >= 90.0f) {
    z = __int_as_float(0x7f800000);
  }
  return z;
}

extern __attribute__((weak)) float __cuda_sinhf(float a); float __cuda_sinhf(float a)
{
  float s, z;

  s = a;
  a = __cuda_fabsf(a);
  if (a < 1.0f) {
    float a2 = a * a;

    z = 2.816951222e-6f;
    z = z * a2 + 1.983615978e-4f;
    z = z * a2 + 8.333350058e-3f;
    z = z * a2 + 1.666666650e-1f;
    z = z * a2;
    z = z * a + a;
  } else {
    z = __internal_expf_kernel(a, -2.0f);
    z = 2.0f * z - 0.125f / z;
    if (a >= 90.0f) {
      z = __int_as_float(0x7f800000);
    }
  }
  return __cuda_copysignf(z, s);
}

extern __attribute__((weak)) float __cuda_tanhf(float a); float __cuda_tanhf(float a)
{
  float t;

  t = __cuda_fabsf(a);
  if (t < 0.55f) {
    float z, z2;
    z = t;
    z2 = z * z;
    t = 1.643758066599993e-2f;
    t = t * z2 - 5.267181327760551e-2f;
    t = t * z2 + 1.332072505223051e-1f;
    t = t * z2 - 3.333294663641083e-1f;
    t = t * z2;
    t = t * z + z;
  }
  else if (t < 88.0f) {
    t = 1.0f - 2.0f / (__internal_expf_kernel(2.0f * t, 0.0f) + 1.0f);
  }
  else if (t >= 88.0f) {
    t = 1.0f;
  }
  return __cuda_copysignf(t, a);
}

extern __attribute__((weak)) float __cuda_atan2f(float a, float b); float __cuda_atan2f(float a, float b)
{
  float t0, t1, t3;



  t3 = __cuda_fabsf(b);
  t1 = __cuda_fabsf(a);

  if (t3 == 0.0f && t1 == 0.0f) {
    t3 = __cuda___signbitf(b) ? 3.141592654f : 0;
  } else if (__cuda___isinff(t3) && __cuda___isinff(t1)) {
    t3 = __cuda___signbitf(b) ? 2.356194490f : 0.785398163f;
  } else {

    if (t3 < t1) {
      t0 = t1;
      t1 = t3;
    } else {
      t0 = t3;
      t1 = t1;
    }
    t3 = __internal_accurate_fdividef(t1, t0);
    t3 = __internal_atanf_kernel(t3);

    if (__cuda_fabsf(a) > __cuda_fabsf(b)) t3 = 1.570796327f - t3;
    if (b < 0.0f) t3 = 3.141592654f - t3;
  }
  t3 = __cuda_copysignf(t3, a);

  return t3;
}

extern __attribute__((weak)) float __cuda_atanf(float a); float __cuda_atanf(float a)
{
  float t0, t1;


  t0 = __cuda_fabsf(a);
  t1 = t0;
  if (t0 > 1.0f) {
    t1 = 1.0f / t1;
  }

  t1 = __internal_atanf_kernel(t1);

  if (t0 > 1.0f) {
    t1 = 1.570796327f - t1;
  }
  return __cuda_copysignf(t1, a);
}


extern __attribute__((weak)) float __internal_asinf_kernel(float a); float __internal_asinf_kernel(float a)
{
  float t2, t3, t4;

  t2 = a * a;
  t3 = - 0.501162291f;
  t3 = t3 * t2 + 0.915201485f;
  t3 = t3 * t2;
  t3 = t3 * a;
  t4 = t2 - 5.478654385f;
  t4 = t4 * t2 + 5.491230488f;
  t4 = 1.0f / t4;
  a = t3 * t4 + a;
  return a;
}

extern __attribute__((weak)) float __cuda_asinf(float a); float __cuda_asinf(float a)
{
  float t0, t1, t2;

  t0 = __cuda_fabsf(a);
  t2 = 1.0f - t0;
  t2 = 0.5f * t2;
  t2 = __cuda_sqrtf(t2);
  t1 = t0 > 0.575f ? t2 : t0;
  t1 = __internal_asinf_kernel(t1);
  t2 = -2.0f * t1 + 1.570796327f;
  if (t0 > 0.575f) {
    t1 = t2;
  }
  return __cuda_copysignf(t1, a);
}

extern __attribute__((weak)) float __cuda_acosf(float a); float __cuda_acosf(float a)
{
  float t0, t1, t2;

  t0 = __cuda_fabsf(a);
  t2 = 1.0f - t0;
  t2 = 0.5f * t2;
  t2 = __cuda_sqrtf(t2);
  t1 = t0 > 0.575f ? t2 : t0;
  t1 = __internal_asinf_kernel(t1);
  t1 = t0 > 0.575f ? 2.0f * t1 : 1.570796327f - t1;
  if (__cuda___signbitf(a)) {
    t1 = 3.141592654f - t1;
  }
  return t1;
}

extern __attribute__((weak)) float __cuda_logf(float a); float __cuda_logf(float a)
{



  return __internal_accurate_logf(a);

}

extern __attribute__((weak)) float __cuda_log10f(float a); float __cuda_log10f(float a)
{



  return 0.434294482f * __internal_accurate_logf(a);

}

extern __attribute__((weak)) float __cuda_log1pf(float a); float __cuda_log1pf(float a)
{
  float t;




  if (a >= -0.394f && a <= 0.65f) {

    t = a + 2.0f;
    t = a / t;
    t = -a * t;
    t = __internal_atanhf_kernel (a, t);
  } else {
    t = __internal_accurate_logf (1.0f + a);
  }
  return t;
}

extern __attribute__((weak)) float __cuda_acoshf(float a); float __cuda_acoshf(float a)
{
  float s, t;

  t = a - 1.0f;
  if (__cuda_fabsf(t) > 8388608.0f) {

    return 0.693147181f + __internal_accurate_logf(a);
  } else {
    s = a + 1.0f;
    t = t + __cuda_sqrtf(s * t);
    return __cuda_log1pf(t);
  }
}

extern __attribute__((weak)) float __cuda_asinhf(float a); float __cuda_asinhf(float a)
{
  float fa, oofa, t;

  fa = __cuda_fabsf(a);
  if (fa > 8.507059173e37f) {
    t = 0.693147181f + __logf(fa);
  } else {
    oofa = 1.0f / fa;
    t = fa + fa / (oofa + __cuda_sqrtf(1.0f + oofa * oofa));
    t = __cuda_log1pf(t);
  }
  return __cuda_copysignf(t, a);
}

extern __attribute__((weak)) float __cuda_atanhf(float a); float __cuda_atanhf(float a)
{
  float fa, t;

  fa = __cuda_fabsf(a);
  t = (2.0f * fa) / (1.0f - fa);
  t = 0.5f * __cuda_log1pf(t);
  return __cuda_copysignf(t, a);
}

extern __attribute__((weak)) float __cuda_expm1f(float a); float __cuda_expm1f(float a)
{
  float t, z, j, u;

  t = __cuda_rintf (a * 1.442695041f);
  z = a - t * 0.6931457519f;
  z = z - t * 1.4286067653e-6f;

  if (__cuda_fabsf(a) < 0.41f) {
    z = a;
    t = 0.0f;
  }

  j = t;
  if (t == 128.0f) j = j - 1.0f;

  u = 1.38795078474044430E-003f;
  u = u * z + 8.38241261853264930E-003f;
  u = u * z + 4.16678317762833940E-002f;
  u = u * z + 1.66663978874356580E-001f;
  u = u * z + 4.99999940395997040E-001f;
  u = u * z;
  u = u * z + z;
  if (a == 0.0f) u = a;

  z = __cuda_exp2f (j);
  a = z - 1.0f;
  if (a != 0.0f) u = u * z + a;
  if (t == 128.0f) u = u + u;

  if (j > 128.0f) u = __int_as_float(0x7f800000);
  if (j < -25.0f) u = -1.0f;
  return u;
}

extern __attribute__((weak)) float __cuda_hypotf(float a, float b); float __cuda_hypotf(float a, float b)
{
  float v, w, t;

  a = __cuda_fabsf(a);
  b = __cuda_fabsf(b);

  if (a > b) {
    v = a;
    w = b;
  } else {
    v = b;
    w = a;
  }
  t = __internal_accurate_fdividef(w, v);
  t = 1.0f + t * t;
  t = v * __cuda_sqrtf(t);
  if (v == 0.0f) {
    t = v + w;
  }
  if ((v == __int_as_float(0x7f800000)) || (w == __int_as_float(0x7f800000))) {
    t = __int_as_float(0x7f800000);
  }
  return t;
}

extern __attribute__((weak)) float __cuda_cbrtf(float a); float __cuda_cbrtf(float a)
{
  float s, t;
  if (a == 0.0f || __cuda___isinff(a)) {
    return a;
  }
  s = __cuda_fabsf(a);
  t = __cuda_exp2f(0.333333333f * __log2f(s));
  t = t - (t - (s / (t * t))) * 0.333333333f;
  if (__cuda___signbitf(a)) {
     t = -t;
  }
  return t;
}

extern __attribute__((weak)) float __cuda_erff(float a); float __cuda_erff(float a)
{
  float t, r, q;

  t = __cuda_fabsf(a);
  if (t < 1.0f) {
    t = t * t;
    r = -5.58510127926029810E-004f;
    r = r * t + 4.90688891415893070E-003f;
    r = r * t - 2.67027980930150640E-002f;
    r = r * t + 1.12799056505903940E-001f;
    r = r * t - 3.76122956138427440E-001f;
    r = r * t + 1.12837911712623450E+000f;
    a = a * r;
  } else if (t <= __int_as_float(0x7f800000)) {



    q = 0.3275911f * t + 1.0f;
    q = 1.0f / q;
    r = 1.061405429f;
    r = r * q - 1.453152027f;
    r = r * q + 1.421413741f;
    r = r * q - 0.284496736f;
    r = r * q + 0.254829592f;
    r = r * q;
    q = __internal_expf_kernel(-a * a, 0.0f);
    r = 1.0f - q * r;
    if (t >= 5.5f) {
      r = 1.0f;
    }
    a = __int_as_float (__float_as_int(r) | (__float_as_int(a) & 0x80000000));
  }
  return a;
}

extern __attribute__((weak)) float __cuda_erfcf(float a); float __cuda_erfcf(float a)
{
  if (a <= 0.55f) {
    return 1.0f - __cuda_erff(a);
  } else if (a > 10.0f) {
    return 0.0f;
  } else {
    float p;
    float q;
    float h;
    float l;




    p = + 4.014893410762552E-006f;
    p = p * a + 5.640401259462436E-001f;
    p = p * a + 2.626649872281140E+000f;
    p = p * a + 5.486372652389673E+000f;
    p = p * a + 5.250714831459401E+000f;
    q = a + 4.651376250488319E+000f;
    q = q * a + 1.026302828878470E+001f;
    q = q * a + 1.140762166021288E+001f;
    q = q * a + 5.251211619089947E+000f;

    h = 1.0f / q;
    q = 2.0f * h - q * h * h;
    p = p * q;

    h = __int_as_float(__float_as_int(a) & 0xfffff000);
    l = a - h;
    q = -h * h;
    q = __internal_expf_kernel(q, 0.0f);
    if (l != 0.0f) {
      a = a + h;
      l = l * a;
      h = __internal_expf_kernel(-l, 0.0f);
      q = q * h;
    }
    p = p * q;
    return p;
  }
}

extern __attribute__((weak)) float __cuda_lgammaf(float a); float __cuda_lgammaf(float a)
{
  float t;
  float i;
  int quot;
  t = __internal_lgammaf_pos(__cuda_fabsf(a));
  if (a >= 0.0f) return t;
  a = __cuda_fabsf(a);
  i = __cuda_floorf(a);
  if (a == i) return __int_as_float(0x7f800000);
  if (a < 1e-19f) return -__internal_accurate_logf(a);
  i = __cuda_rintf (2.0f * a);
  quot = (int)i;
  i = a - 0.5f * i;
  i = i * 3.141592654f;
  if (quot & 1) {
    i = __internal_cos_kernel(i);
  } else {
    i = __internal_sin_kernel(i);
  }
  i = __cuda_fabsf(i);
  t = 1.144729886f - __internal_accurate_logf(i * a) - t;
  return t;
}

extern __attribute__((weak)) float __cuda_ldexpf(float a, int b); float __cuda_ldexpf(float a, int b)
{
  float fa = __cuda_fabsf(a);

  if (fa == 0.0f || __cuda___isinff(fa) || b == 0) {
    return a;
  }
  else if (__cuda_abs(b) < 126) {
    return a * __cuda_exp2f((float)b);
  }
  else if (__cuda_abs(b) < 252) {
    int bhalf = b / 2;
    return a * __cuda_exp2f((float)bhalf) * __cuda_exp2f((float)(b - bhalf));
  }
  else {
    int bquarter = b / 4;
    float t = __cuda_exp2f((float)bquarter);
    return a * t * t * t * __cuda_exp2f((float)(b - 3 * bquarter));
  }
}

extern __attribute__((weak)) float __cuda_scalbnf(float a, int b); float __cuda_scalbnf(float a, int b)
{

  return __cuda_ldexpf(a, b);
}

extern __attribute__((weak)) float __cuda_scalblnf(float a, long int b); float __cuda_scalblnf(float a, long int b)
{
  int t;
  if (b > 2147483647L) {
    t = 2147483647;
  } else if (b < (-2147483647 - 1)) {
    t = (-2147483647 - 1);
  } else {
    t = (int)b;
  }
  return __cuda_scalbnf(a, t);
}

extern __attribute__((weak)) float __cuda_frexpf(float a, int *b); float __cuda_frexpf(float a, int *b)
{
  float fa = __cuda_fabsf(a);
  unsigned int expo;
  unsigned int denorm;

  if (fa < 1.175494351e-38f) {
    a *= 16777216.0f;
    denorm = 24;
  } else {
    denorm = 0;
  }
  expo = ((__float_as_int(a) >> 23) & 0xff);
  if ((fa == 0.0f) || (expo == 0xff)) {
    expo = 0;
    a = a + a;
  } else {
    expo = expo - denorm - 126;
    a = __int_as_float(((__float_as_int(a) & 0x807fffff) | 0x3f000000));
  }
  *b = expo;
  return a;
}

extern __attribute__((weak)) float __cuda_modff(float a, float *b); float __cuda_modff(float a, float *b)
{
  float t;
  if (__cuda___finitef(a)) {
    t = __cuda_truncf(a);
    *b = t;
    t = a - t;
    return __cuda_copysignf(t, a);
  } else if (__cuda___isinff(a)) {
    t = 0.0f;
    *b = a;
    return __cuda_copysignf(t, a);
  } else {
    *b = a;
    return a;
  }
}

extern __attribute__((weak)) float __cuda_fmodf(float a, float b); float __cuda_fmodf(float a, float b)
{
  float orig_a;

  if (__cuda___isnanf(a) || __cuda___isnanf(b)) {
    return a + b;
  }
  orig_a = a;
  a = __cuda_fabsf(a);
  b = __cuda_fabsf(b);
  if (__cuda___isinff(a) || b == 0.0f) {
    return __int_as_float(0x7fffffff);
  } else if (a >= b) {


    int expoa = (a < 1.175494351e-38f) ?
        ((int)__log2f(a)) : (((__float_as_int(a) >> 23) & 0xff) - 127);
    int expob = (b < 1.175494351e-38f) ?
        ((int)__log2f(b)) : (((__float_as_int(b) >> 23) & 0xff) - 127);
    int scale = expoa - expob;
    float scaled_b = __cuda_ldexpf(b, scale);
    if (scaled_b <= 0.5f * a) {
      scaled_b *= 2.0f;
    }







    while (scaled_b >= b) {
      if (a >= scaled_b) {
        a -= scaled_b;
      }
      scaled_b *= 0.5f;
    }
    return __cuda_copysignf(a, orig_a);
  } else {
    return orig_a;
  }
}

extern __attribute__((weak)) float __cuda_remainderf(float a, float b); float __cuda_remainderf(float a, float b)
{
  float orig_a;
  float twoa = 0.0f;
  unsigned int quot0 = 0;

  if (__cuda___isnanf(a) || __cuda___isnanf(b)) {
    return a + b;
  }
  orig_a = a;
  a = __cuda_fabsf(a);
  b = __cuda_fabsf(b);
  if (__cuda___isinff(a) || (b == 0.0f)) {
    return __int_as_float(0x7fffffff);
  } else if (a >= b) {

    int expoa = (a < 1.175494351e-38f) ?
        ((int)__log2f(a)) : (((__float_as_int(a) >> 23) & 0xff) - 127);
    int expob = (b < 1.175494351e-38f) ?
        ((int)__log2f(b)) : (((__float_as_int(b) >> 23) & 0xff) - 127);
    int scale = expoa - expob;
    float scaled_b = __cuda_ldexpf(b, scale);
    if (scaled_b <= 0.5f * a) {
      scaled_b *= 2.0f;
    }
# 2255 "/usr/local/cuda/bin/../include/math_functions.h" 3
    while (scaled_b >= b) {
      quot0 = 0;
      if (a >= scaled_b) {
        twoa = (2.0f * a - scaled_b) - scaled_b;
        a -= scaled_b;
        quot0 = 1;
      }
      scaled_b *= 0.5f;
    }
  }


  twoa = 2.0f * a;
  if ((twoa > b) || ((twoa == b) && quot0)) {
    a -= b;
    a = __cuda_copysignf (a, -1.0f);
  }
# 2287 "/usr/local/cuda/bin/../include/math_functions.h" 3
  a = __int_as_float((__float_as_int(orig_a) & 0x80000000)^
                     __float_as_int(a));
  return a;
}

extern __attribute__((weak)) float __cuda_remquof(float a, float b, int* quo); float __cuda_remquof(float a, float b, int* quo)
{
  float orig_a;
  float twoa = 0.0f;
  unsigned int quot = 0;
  unsigned int sign;

  if (__cuda___isnanf(a) || __cuda___isnanf(b)) {
    *quo = quot;
    return a + b;
  }
  orig_a = a;

  sign = 0 - (__cuda___signbitf(a) != __cuda___signbitf(b));
  a = __cuda_fabsf(a);
  b = __cuda_fabsf(b);
  if (__cuda___isinff(a) || (b == 0.0f)) {
    *quo = quot;
    return __int_as_float(0x7fffffff);
  } else if (a >= b) {


    int expoa = (a < 1.175494351e-38f) ?
        ((int)__log2f(a)) : (((__float_as_int(a) >> 23) & 0xff) - 127);
    int expob = (b < 1.175494351e-38f) ?
        ((int)__log2f(b)) : (((__float_as_int(b) >> 23) & 0xff) - 127);
    int scale = expoa - expob;
    float scaled_b = __cuda_ldexpf(b, scale);
    if (scaled_b <= 0.5f * a) {
      scaled_b *= 2.0f;
    }
# 2340 "/usr/local/cuda/bin/../include/math_functions.h" 3
    while (scaled_b >= b) {
      quot <<= 1;
      if (a >= scaled_b) {
        twoa = (2.0f * a - scaled_b) - scaled_b;
        a -= scaled_b;
        quot += 1;
      }
      scaled_b *= 0.5f;
    }
  }


  twoa = 2.0f * a;
  if ((twoa > b) || ((twoa == b) && (quot & 1))) {
    quot++;
    a -= b;
    a = __cuda_copysignf (a, -1.0f);
  }
# 2375 "/usr/local/cuda/bin/../include/math_functions.h" 3
  a = __int_as_float((__float_as_int(orig_a) & 0x80000000)^
                     __float_as_int(a));
  quot = quot & (~((~0)<<3));
  quot = quot ^ sign;
  quot = quot - sign;
  *quo = quot;
  return a;
}

extern __attribute__((weak)) float __cuda_fmaf(float a, float b, float c); float __cuda_fmaf(float a, float b, float c)
{
  unsigned int xx, yy, zz, ww;
  unsigned int temp, s, u;
  unsigned int expo_x, expo_y, expo_z;

  xx = __float_as_int(a);
  yy = __float_as_int(b);
  zz = __float_as_int(c);
# 2401 "/usr/local/cuda/bin/../include/math_functions.h" 3
  temp = 0xff;
  expo_x = temp & (xx >> 23);
  expo_x = expo_x - 1;
  expo_y = temp & (yy >> 23);
  expo_y = expo_y - 1;
  expo_z = temp & (zz >> 23);
  expo_z = expo_z - 1;

  if (!((expo_x <= 0xFD) &&
        (expo_y <= 0xFD) &&
        (expo_z <= 0xFD))) {




    if ((yy << 1) > 0xff000000) {
      return __int_as_float(0x7fffffff);
    }
    if ((zz << 1) > 0xff000000) {
      return __int_as_float(0x7fffffff);
    }
    if ((xx << 1) > 0xff000000) {
      return __int_as_float(0x7fffffff);
    }
# 2436 "/usr/local/cuda/bin/../include/math_functions.h" 3
    if ((((xx << 1) == 0) && ((yy << 1) == 0xff000000)) ||
        (((yy << 1) == 0) && ((xx << 1) == 0xff000000))) {
      return __int_as_float(0x7fffffff);
    }
    if ((zz << 1) == 0xff000000) {
      if (((yy << 1) == 0xff000000) || ((xx << 1) == 0xff000000)) {
        if ((int)(xx ^ yy ^ zz) < 0) {
          return __int_as_float(0x7fffffff);
        }
      }
    }




    if ((xx << 1) == 0xff000000) {
      xx = xx ^ (yy & 0x80000000);
      return __int_as_float(xx);
    }
    if ((yy << 1) == 0xff000000) {
      yy = yy ^ (xx & 0x80000000);
      return __int_as_float(yy);
    }
    if ((zz << 1) == 0xff000000) {
      return __int_as_float(zz);
    }





    if (zz == 0x80000000) {
      if (((xx << 1) == 0) || ((yy << 1) == 0)) {
        if ((int)(xx ^ yy) < 0) {
          return __int_as_float(zz);
        }
      }
    }



    if (((zz << 1) == 0) &&
        (((xx << 1) == 0) || ((yy << 1) == 0))) {
      zz &= 0x7fffffff;
      return __int_as_float(zz);
    }



    if (((xx << 1) == 0) || ((yy << 1) == 0)) {
      return __int_as_float(zz);
    }

    if (expo_x == (unsigned int)-1) {
      temp = xx & 0x80000000;
      xx = xx << 8;
      while (!(xx & 0x80000000)) {
        xx <<= 1;
        expo_x--;
      }
      expo_x++;
      xx = (xx >> 8) | temp;
    }

    if (expo_y == (unsigned int)-1) {
      temp = yy & 0x80000000;
      yy = yy << 8;
      while (!(yy & 0x80000000)) {
        yy <<= 1;
        expo_y--;
      }
      expo_y++;
      yy = (yy >> 8) | temp;
    }

    if ((expo_z == (unsigned int)-1) && ((zz << 1) != 0)) {
      temp = zz & 0x80000000;
      zz = zz << 8;
      while (!(zz & 0x80000000)) {
        zz <<= 1;
        expo_z--;
      }
      expo_z++;
      zz = (zz >> 8) | temp;
    }
  }

  expo_x = expo_x + expo_y;
  expo_y = xx ^ yy;
  xx = xx & 0x00ffffff;
  yy = yy << 8;
  xx = xx | 0x00800000;
  yy = yy | 0x80000000;

  s = __umulhi(xx, yy);
  yy = xx * yy;
  xx = s;
  expo_x = expo_x - 127 + 2;
  expo_y = expo_y & 0x80000000;


  if (xx < 0x00800000) {
      xx = (xx << 1) | (yy >> 31);
      yy = (yy << 1);
      expo_x--;
  }
  temp = 0;
  if ((zz << 1) != 0) {
    s = zz & 0x80000000;
    zz &= 0x00ffffff;
    zz |= 0x00800000;
    ww = 0;

    if ((int)expo_z > (int)expo_x) {
      temp = expo_z;
      expo_z = expo_x;
      expo_x = temp;
      temp = zz;
      zz = xx;
      xx = temp;
      temp = ww;
      ww = yy;
      yy = temp;
      temp = expo_y;
      expo_y = s;
      s = temp;
    }


    expo_z = expo_x - expo_z;
    u = expo_y ^ s;
    if (expo_z <= 49) {

      temp = 0;
      while (expo_z >= 32) {
        temp = ww | (temp != 0);
        ww = zz;
        zz = 0;
        expo_z -= 32;
      }
      if (expo_z) {
        temp = ((temp >> expo_z) | (ww << (32 - expo_z)) |
                ((temp << (32 - expo_z)) != 0));
        ww = (ww >> expo_z) | (zz << (32 - expo_z));
        zz = (zz >> expo_z);
      }
    } else {
      temp = 1;
      ww = 0;
      zz = 0;
    }
    if ((int)u < 0) {

      temp = (unsigned)(-(int)temp);
      s = (temp != 0);
      u = yy - s;
      s = u > yy;
      yy = u - ww;
      s += yy > u;
      xx = (xx - zz) - s;
      if (!(xx | yy | temp)) {

        return __int_as_float(xx);
      }
      if ((int)xx < 0) {



        temp = ~temp;
        yy = ~yy;
        xx = ~xx;
        if (++temp == 0) {
          if (++yy == 0) {
            ++xx;
          }
        }
        expo_y ^= 0x80000000;
      }

      while (!(xx & 0x00800000)) {
        xx = (xx << 1) | (yy >> 31);
        yy = (yy << 1);
        expo_x--;
      }
    } else {

      yy = yy + ww;
      s = yy < ww;
      xx = xx + zz + s;
      if (xx & 0x01000000) {
        temp = temp | (yy << 31);
        yy = (yy >> 1) | (xx << 31);
        xx = ((xx & 0x80000000) | (xx >> 1)) & ~0x40000000;
        expo_x++;
      }
    }
  }
  temp = yy | (temp != 0);
  if (expo_x <= 0xFD) {

    xx |= expo_y;
    s = xx & 1;
    xx += (temp == 0x80000000) ? s : (temp >> 31);
    xx = xx + (expo_x << 23);
    return __int_as_float(xx);
  } else if ((int)expo_x >= 126) {

    xx = expo_y | 0x7f800000;
    return __int_as_float(xx);
  }

  expo_x = (unsigned int)(-(int)expo_x);
  if (expo_x > 25) {

    return __int_as_float(expo_y);
  }
  yy = (xx << (32 - expo_x)) | ((yy) ? 1 : 0);
  xx = expo_y + (xx >> expo_x);
  xx = xx + ((yy==0x80000000) ? (xx & 1) : (yy >> 31));
  xx |= expo_y;




  return __int_as_float(xx);
}

static float __cudart_A1[32] =
{
  1.0000000000e+000f,
  1.0218971968e+000f,
  1.0442737341e+000f,
  1.0671404600e+000f,
  1.0905077457e+000f,
  1.1143867970e+000f,
  1.1387885809e+000f,
  1.1637248993e+000f,
  1.1892070770e+000f,
  1.2152473927e+000f,
  1.2418577671e+000f,
  1.2690509558e+000f,
  1.2968395948e+000f,
  1.3252366781e+000f,
  1.3542555571e+000f,
  1.3839099407e+000f,
  1.4142135382e+000f,
  1.4451807737e+000f,
  1.4768261909e+000f,
  1.5091644526e+000f,
  1.5422108173e+000f,
  1.5759809017e+000f,
  1.6104903221e+000f,
  1.6457555294e+000f,
  1.6817928553e+000f,
  1.7186193466e+000f,
  1.7562521696e+000f,
  1.7947090864e+000f,
  1.8340080976e+000f,
  1.8741676807e+000f,
  1.9152065516e+000f,
  1.9571441412e+000f
};

static float __cudart_A2[32] =
{
  0.0000000000e+000f,
 -4.8115598617e-008f,
  4.8334701575e-008f,
 -5.9337519787e-008f,
 -1.3077539940e-008f,
 -5.4355400181e-008f,
  5.3862223126e-008f,
 -4.0514414934e-008f,
  3.7976352729e-008f,
 -3.2673948880e-008f,
  4.4968381019e-008f,
  1.4193333175e-009f,
 -4.0189995332e-008f,
 -3.4963733242e-008f,
 -1.0123349270e-008f,
 -5.8755773580e-008f,
  2.4203234972e-008f,
  3.3241999375e-008f,
 -4.5008988536e-008f,
 -2.4959373235e-008f,
  8.0709048333e-009f,
 -5.6610254262e-008f,
  9.8362171741e-009f,
 -5.1249720912e-008f,
 -2.4755326677e-008f,
 -4.8496175964e-008f,
 -9.2357703707e-009f,
 -1.1415044909e-008f,
 -1.1239277953e-008f,
 -4.6630056261e-008f,
  9.8453281083e-009f,
 -1.7021804410e-008f
};

static float __cudart_Ainv[32] =
{
  1.0000000000e+000f,
  9.7857207060e-001f,
  9.5760327578e-001f,
  9.3708384037e-001f,
  9.1700404882e-001f,
  8.9735454321e-001f,
  8.7812608480e-001f,
  8.5930967331e-001f,
  8.4089642763e-001f,
  8.2287776470e-001f,
  8.0524516106e-001f,
  7.8799045086e-001f,
  7.7110540867e-001f,
  7.5458222628e-001f,
  7.3841309547e-001f,
  7.2259038687e-001f,
  7.0710676908e-001f,
  6.9195497036e-001f,
  6.7712777853e-001f,
  6.6261833906e-001f,
  6.4841979742e-001f,
  6.3452547789e-001f,
  6.2092888355e-001f,
  6.0762369633e-001f,
  5.9460353851e-001f,
  5.8186244965e-001f,
  5.6939429045e-001f,
  5.5719339848e-001f,
  5.4525387287e-001f,
  5.3357023001e-001f,
  5.2213686705e-001f,
  5.1094859838e-001f
};

extern __attribute__((weak)) float __internal_accurate_powf(float a, float b); float __internal_accurate_powf(float a, float b)
{
  int i;
  float t;
  int expo;
  float log_hi, log_lo;
  float b_hi, b_lo;
  float prod_hi, prod_lo;

  if ((a > 0.707106781f) && (a < 1.414213562f)) {
    float f, g, u, v, q;






    f = a - 1.0f;
    g = a + 1.0f;
    g = 1.0f / g;
    u = 2.0f * f * g;
    v = u * u;
    q = 1.49356810919559350E-001f/64.0f;
    q = q * v + 1.99887797540072460E-001f/16.0f;
    q = q * v + 3.33333880955515580E-001f/4.0f;
    q = q * v;
    q = q * u;
    log_hi = __int_as_float(__float_as_int(u) & 0xfffff000);
    v = __int_as_float(__float_as_int(f) & 0xfffff000);
    u = 2.0f * (f - log_hi);
    f = f - v;
    u = u - log_hi * v;
    u = u - log_hi * f;
    u = g * u;
    log_lo = q + u;


    b_hi = __int_as_float(__float_as_int(b) & 0xfffff000);
    b_lo = b - b_hi;
    prod_lo = b_lo * log_lo;
    prod_lo += b_lo * log_hi;
    prod_lo += b_hi * log_lo;
    prod_hi = b_hi * log_hi;


    return __cuda_expf(prod_hi) * __cuda_expf(prod_lo);
  }


  if (a >= 1.175494351e-38f) {
    i = __float_as_int(a);
    expo = ((i >> 23) & 0xff) - 127;
  } else {
    a *= 16777216.0f;
    i = __float_as_int(a);
    expo = ((i >> 23) & 0xff) - 127 - 24;
  }
  i = (i & 0x007fffff) | (0x3f800000);
  t = __int_as_float(i);

  i = 0;
  if (t >= __cudart_A1[i+16]) i += 16;
  if (t >= __cudart_A1[i+8]) i += 8;
  if (t >= __cudart_A1[i+4]) i += 4;
  if (t >= __cudart_A1[i+2]) i += 2;
  if (t >= __cudart_A1[i+1]) i += 1;

  t = t - __cudart_A1[i];
  t = t - __cudart_A2[i];

  t = t * __cudart_Ainv[i];


  log_hi = (float)expo + (float)i * 0.03125f;

  log_lo = - 3.42338934684934650E-001f;
  log_lo = log_lo * t + 4.80524913518140690E-001f;
  log_lo = log_lo * t - 7.21345070621603800E-001f;
  log_lo = log_lo * t + 1.44269503837073180E+000f;
  log_lo = log_lo * t;


  b_hi = __int_as_float(__float_as_int(b) & 0xfffff000);
  b_lo = b - b_hi;
  prod_lo = b_lo * log_lo;
  prod_lo = prod_lo + b_lo * log_hi;
  prod_lo = prod_lo + b_hi * log_lo;
  prod_hi = b_hi * log_hi;


  if (prod_hi >= 256.0f) {
    return __int_as_float(0x7f800000);
  }
  if (prod_hi <= -256.0f) {
    return 0.0f;
  }


  b = __cuda_exp2f (0.5f * prod_hi);
  t = __cuda_exp2f (prod_lo);
  t = t * b;
  t = t * b;
  return t;
}

extern __attribute__((weak)) float __cuda_powif(float a, int b); float __cuda_powif(float a, int b)
{
  unsigned int e = __cuda_abs(b);
  float r = 1.0f;

  while (1) {
    if ((e & 1) != 0) {
      r = r * a;
    }
    e = e >> 1;
    if (e == 0) {
      return b < 0 ? 1.0f/r : r;
    }
    a = a * a;
  }
}

extern __attribute__((weak)) double __cuda_powi(double a, int b); double __cuda_powi(double a, int b)
{
  unsigned int e = __cuda_abs(b);
  double r = 1.0;

  while (1) {
    if ((e & 1) != 0) {
      r = r * a;
    }
    e = e >> 1;
    if (e == 0) {
      return b < 0 ? 1.0/r : r;
    }
    a = a * a;
  }
}

extern __attribute__((weak)) float __cuda_powf(float a, float b); float __cuda_powf(float a, float b)
{



  int bIsOddInteger;
  float t;
  if (a == 1.0f || b == 0.0f) {
    return 1.0f;
  }
  if (__cuda___isnanf(a) || __cuda___isnanf(b)) {
    return a + b;
  }
  if (a == __int_as_float(0x7f800000)) {
    return __cuda___signbitf(b) ? 0.0f : __int_as_float(0x7f800000);
  }
  if (__cuda___isinff(b)) {
    if (a == -1.0f) {
      return 1.0f;
    }
    t = (__cuda_fabsf(a) > 1.0f) ? __int_as_float(0x7f800000) : 0.0f;
    if (b < 0.0f) {
      t = 1.0f / t;
    }
    return t;
  }
  bIsOddInteger = (b - (2.0f * floorf(0.5f * b))) == 1.0f;
  if (a == 0.0f) {
    t = bIsOddInteger ? a : 0.0f;
    if (b < 0.0f) {
      t = 1.0f / t;
    }
    return t;
  }
  if (a == -__int_as_float(0x7f800000)) {
    t = (b < 0.0f) ? -1.0f/a : -a;
    if (bIsOddInteger) {
      t = __int_as_float(__float_as_int(t) ^ 0x80000000);
    }
    return t;
  }
  if ((a < 0.0f) && (b != __cuda_truncf(b))) {
    return __int_as_float(0x7fffffff);
  }
  t = __cuda_fabsf(a);
  t = __internal_accurate_powf(t, b);
  if ((a < 0.0f) && bIsOddInteger) {
    t = __int_as_float(__float_as_int(t) ^ 0x80000000);
  }
  return t;

}


extern __attribute__((weak)) float __internal_tgammaf_kernel(float a); float __internal_tgammaf_kernel(float a)
{
  float t;
  t = - 1.05767296987211380E-003f;
  t = t * a + 7.09279059435508670E-003f;
  t = t * a - 9.65347121958557050E-003f;
  t = t * a - 4.21736613253687960E-002f;
  t = t * a + 1.66542401247154280E-001f;
  t = t * a - 4.20043267827838460E-002f;
  t = t * a - 6.55878234051332940E-001f;
  t = t * a + 5.77215696929794240E-001f;
  t = t * a + 1.00000000000000000E+000f;
  return t;
}





extern __attribute__((weak)) float __cuda_tgammaf(float a); float __cuda_tgammaf(float a)
{
  float s, xx, x=a;
  if (x >= 0.0f) {
    if (x > 36.0f) x = 36.0f;
    s = 1.0f;
    xx = x;
    if (x > 34.03f) {
      xx -= 1.0f;
    }
    while (xx > 1.5f) {
      xx = xx - 1.0f;
      s = s * xx;
    }
    if (x >= 0.5f) {
      xx = xx - 1.0f;
    }
    xx = __internal_tgammaf_kernel(xx);
    if (x < 0.5f) {
      xx = xx * x;
    }
    s = s / xx;
    if (x > 34.03f) {

      xx = x - 1.0f;
      s = s * xx;
    }
    return s;
  } else {
    if (x == __cuda_floorf(x)) {
      x = __int_as_float(0x7fffffff);

      return x;

    }
    if (x < -41.1f) x = -41.1f;
    xx = x;
    if (x < -34.03f) {
      xx += 6.0f;
    }
    s = xx;
    while (xx < -0.5f) {
      xx = xx + 1.0f;
      s = s * xx;
    }
    xx = __internal_tgammaf_kernel(xx);
    s = s * xx;
    s = 1.0f / s;
    if (x < -34.03f) {
      xx = x;
      xx *= (x + 1.0f);
      xx *= (x + 2.0f);
      xx *= (x + 3.0f);
      xx *= (x + 4.0f);
      xx *= (x + 5.0f);
      xx = 1.0f / xx;
      s = s * xx;
      if ((a < -42.0f) && !(((int)a)&1)) {
        s = __int_as_float(0x80000000);
      }
    }
    return s;
  }
}

extern __attribute__((weak)) float __cuda_roundf(float a); float __cuda_roundf(float a)
{
  float fa = __cuda_fabsf(a);
  if (fa > 8388608.0f) {
    return a;
  } else {
    float u = __cuda_floorf(fa + 0.5f);
    if (fa < 0.5f) u = 0.0f;
    return __cuda_copysignf(u, a);
  }
}

extern __attribute__((weak)) long long int __internal_llroundf_kernel(float a); long long int __internal_llroundf_kernel(float a)
{
  unsigned long long int res, t = 0LL;
  int shift;
  unsigned int ia = __float_as_int(a);
  if ((ia << 1) > 0xff000000) return 0LL;
  if ((int)ia >= 0x5f000000) return 0x7fffffffffffffffLL;
  if (ia >= 0xdf000000) return 0x8000000000000000LL;
  shift = 189 - ((ia >> 23) & 0xff);
  res = ((long long int)(((ia << 8) | 0x80000000) >> 1)) << 32;
  if (shift >= 64) {
    t = res;
    res = 0;
  } else if (shift) {
    t = res << (64 - shift);
    res = res >> shift;
  }
  if (t >= 0x8000000000000000LL) {
      res++;
  }
  if ((int)ia < 0) res = (unsigned long long int)(-(long long int)res);
  return (long long int)res;
}

extern __attribute__((weak)) long long int __cuda_llroundf(float a); long long int __cuda_llroundf(float a)
{
  return __internal_llroundf_kernel(a);
}

extern __attribute__((weak)) long int __cuda_lroundf(float a); long int __cuda_lroundf(float a)
{

  return (long int)__cuda_llroundf(a);
# 3101 "/usr/local/cuda/bin/../include/math_functions.h" 3
}

extern __attribute__((weak)) float __cuda_fdimf(float a, float b); float __cuda_fdimf(float a, float b)
{
  float t;
  t = a - b;
  if (a <= b) {
    t = 0.0f;
  }
  return t;
}

extern __attribute__((weak)) int __cuda_ilogbf(float a); int __cuda_ilogbf(float a)
{
  unsigned int i;
  int expo;
  a = __cuda_fabsf(a);
  if (a <= 1.175494351e-38f) {

    if (a == 0.0f) {
      expo = -((int)((unsigned int)-1 >> 1))-1;
    } else {
      expo = -126;
      i = __float_as_int(a);
      i = i << 8;
      while ((int)i >= 0) {
        expo--;
        i = i + i;
      }
    }
  } else {
    i = __float_as_int(a);
    expo = ((int)((i >> 23) & 0xff)) - 127;
    if ((i == 0x7f800000)) {
      expo = ((int)((unsigned int)-1 >> 1));
    }
    if ((i > 0x7f800000)) {
      expo = -((int)((unsigned int)-1 >> 1))-1;
    }
  }
  return expo;
}

extern __attribute__((weak)) float __cuda_logbf(float a); float __cuda_logbf(float a)
{
  unsigned int i;
  int expo;
  float res;

  if (__cuda___isnanf(a)) return a + a;

  a = __cuda_fabsf(a);
  if (a <= 1.175494351e-38f) {

    if (a == 0.0f) {
      res = -__int_as_float(0x7f800000);
    } else {
      expo = -126;
      i = __float_as_int(a);
      i = i << 8;
      while ((int)i >= 0) {
        expo--;
        i = i + i;
      }
      res = (float)expo;
    }
  } else {
    i = __float_as_int(a);
    expo = ((int)((i >> 23) & 0xff)) - 127;
    res = (float)expo;
    if ((i >= 0x7f800000)) {

      res = a + a;
    }
  }
  return res;
}

extern __attribute__((weak)) void __cuda_sincosf(float a, float *sptr, float *cptr); void __cuda_sincosf(float a, float *sptr, float *cptr)
{



  float t, u, s, c;
  int quadrant;
  if (__cuda___isinff(a)) {
    *sptr = __int_as_float(0x7fffffff);
    *cptr = __int_as_float(0x7fffffff);
    return;
  }
  if (a == 0.0f) {
    *sptr = a;
    *cptr = 1.0f;
    return;
  }
  t = __internal_trig_reduction_kernel(a, &quadrant);
  u = __internal_cos_kernel(t);
  t = __internal_sin_kernel(t);
  if (quadrant & 1) {
    s = u;
    c = t;
  } else {
    s = t;
    c = u;
  }
  if (quadrant & 2) {
    s = -s;
  }
  quadrant++;
  if (quadrant & 2) {
    c = -c;
  }
  *sptr = s;
  *cptr = c;

}
# 3226 "/usr/local/cuda/bin/../include/math_functions.h" 3
extern __attribute__((weak)) double rsqrt(double a); double rsqrt(double a)
{
  return 1.0 / sqrt(a);
}

extern __attribute__((weak)) float rsqrtf(float a); float rsqrtf(float a)
{
  return (float)rsqrt((double)a);
}
# 4167 "/usr/local/cuda/bin/../include/math_functions.h" 3
# 1 "/usr/local/cuda/bin/../include/math_functions_dbl_ptx1.h" 1 3
# 45 "/usr/local/cuda/bin/../include/math_functions_dbl_ptx1.h" 3
extern __attribute__((weak)) double __cuda_fabs(double a); double __cuda_fabs(double a)
{
  return (float)__cuda_fabsf((float)a);
}

extern __attribute__((weak)) double __cuda_fmax(double a, double b); double __cuda_fmax(double a, double b)
{
  return (float)__cuda_fmaxf((float)a, (float)b);
}

extern __attribute__((weak)) double __cuda_fmin(double a, double b); double __cuda_fmin(double a, double b)
{
  return (float)__cuda_fminf((float)a, (float)b);
}

extern __attribute__((weak)) int __cuda___finite(double a); int __cuda___finite(double a)
{
  return __cuda___finitef((float)a);
}

extern __attribute__((weak)) int __cuda___isinf(double a); int __cuda___isinf(double a)
{
  return __cuda___isinff((float)a);
}

extern __attribute__((weak)) int __cuda___isnan(double a); int __cuda___isnan(double a)
{
  return __cuda___isnanf((float)a);
}

extern __attribute__((weak)) int __cuda___signbit(double a); int __cuda___signbit(double a)
{
  return __cuda___signbitf((float)a);
}

extern __attribute__((weak)) double __cuda_sqrt(double a); double __cuda_sqrt(double a)
{
  return (double)__cuda_sqrtf((float)a);
}

extern __attribute__((weak)) double __cuda_rsqrt(double a); double __cuda_rsqrt(double a)
{
  return (double)__cuda_rsqrtf((float)a);
}

extern __attribute__((weak)) double __cuda_ceil(double a); double __cuda_ceil(double a)
{
  return (double)__cuda_ceilf((float)a);
}

extern __attribute__((weak)) double __cuda_trunc(double a); double __cuda_trunc(double a)
{
  return (double)__cuda_truncf((float)a);
}

extern __attribute__((weak)) double __cuda_floor(double a); double __cuda_floor(double a)
{
  return (double)__cuda_floorf((float)a);
}

extern __attribute__((weak)) double __cuda_copysign(double a, double b); double __cuda_copysign(double a, double b)
{
  return (double)__cuda_copysignf((float)a, (float)b);
}

extern __attribute__((weak)) double __cuda_sin(double a); double __cuda_sin(double a)
{
  return (double)__cuda_sinf((float)a);
}

extern __attribute__((weak)) double __cuda_cos(double a); double __cuda_cos(double a)
{
  return (double)__cuda_cosf((float)a);
}

extern __attribute__((weak)) void __cuda_sincos(double a, double *sptr, double *cptr); void __cuda_sincos(double a, double *sptr, double *cptr)
{
  float fs, fc;

  __cuda_sincosf((float)a, &fs, &fc);

  *sptr = (double)fs;
  *cptr = (double)fc;
}

extern __attribute__((weak)) double __cuda_tan(double a); double __cuda_tan(double a)
{
  return (double)__cuda_tanf((float)a);
}

extern __attribute__((weak)) double __cuda_exp(double a); double __cuda_exp(double a)
{
  return (double)__cuda_expf((float)a);
}

extern __attribute__((weak)) double __cuda_exp2(double a); double __cuda_exp2(double a)
{
  return (double)__cuda_exp2f((float)a);
}

extern __attribute__((weak)) double __cuda_exp10(double a); double __cuda_exp10(double a)
{
  return (double)__cuda_exp10f((float)a);
}

extern __attribute__((weak)) double __cuda_expm1(double a); double __cuda_expm1(double a)
{
  return (double)__cuda_expm1f((float)a);
}

extern __attribute__((weak)) double __cuda_cosh(double a); double __cuda_cosh(double a)
{
  return (double)__cuda_coshf((float)a);
}

extern __attribute__((weak)) double __cuda_sinh(double a); double __cuda_sinh(double a)
{
  return (double)__cuda_sinhf((float)a);
}

extern __attribute__((weak)) double __cuda_tanh(double a); double __cuda_tanh(double a)
{
  return (double)__cuda_tanhf((float)a);
}

extern __attribute__((weak)) double __cuda_asin(double a); double __cuda_asin(double a)
{
  return (double)__cuda_asinf((float)a);
}

extern __attribute__((weak)) double __cuda_acos(double a); double __cuda_acos(double a)
{
  return (double)__cuda_acosf((float)a);
}

extern __attribute__((weak)) double __cuda_atan(double a); double __cuda_atan(double a)
{
  return (double)__cuda_atanf((float)a);
}

extern __attribute__((weak)) double __cuda_atan2(double a, double b); double __cuda_atan2(double a, double b)
{
  return (double)__cuda_atan2f((float)a, (float)b);
}

extern __attribute__((weak)) double __cuda_log(double a); double __cuda_log(double a)
{
  return (double)__cuda_logf((float)a);
}

extern __attribute__((weak)) double __cuda_log2(double a); double __cuda_log2(double a)
{
  return (double)__cuda_log2f((float)a);
}

extern __attribute__((weak)) double __cuda_log10(double a); double __cuda_log10(double a)
{
  return (double)__cuda_log10f((float)a);
}

extern __attribute__((weak)) double __cuda_log1p(double a); double __cuda_log1p(double a)
{
  return (double)__cuda_log1pf((float)a);
}

extern __attribute__((weak)) double __cuda_acosh(double a); double __cuda_acosh(double a)
{
  return (double)__cuda_acoshf((float)a);
}

extern __attribute__((weak)) double __cuda_asinh(double a); double __cuda_asinh(double a)
{
  return (double)__cuda_asinhf((float)a);
}

extern __attribute__((weak)) double __cuda_atanh(double a); double __cuda_atanh(double a)
{
  return (double)__cuda_atanhf((float)a);
}

extern __attribute__((weak)) double __cuda_hypot(double a, double b); double __cuda_hypot(double a, double b)
{
  return (double)__cuda_hypotf((float)a, (float)b);
}

extern __attribute__((weak)) double __cuda_cbrt(double a); double __cuda_cbrt(double a)
{
  return (double)__cuda_cbrtf((float)a);
}

extern __attribute__((weak)) double __cuda_erf(double a); double __cuda_erf(double a)
{
  return (double)__cuda_erff((float)a);
}

extern __attribute__((weak)) double __cuda_erfc(double a); double __cuda_erfc(double a)
{
  return (double)__cuda_erfcf((float)a);
}

extern __attribute__((weak)) double __cuda_lgamma(double a); double __cuda_lgamma(double a)
{
  return (double)__cuda_lgammaf((float)a);
}

extern __attribute__((weak)) double __cuda_tgamma(double a); double __cuda_tgamma(double a)
{
  return (double)__cuda_tgammaf((float)a);
}

extern __attribute__((weak)) double __cuda_ldexp(double a, int b); double __cuda_ldexp(double a, int b)
{
  return (double)__cuda_ldexpf((float)a, b);
}

extern __attribute__((weak)) double __cuda_scalbn(double a, int b); double __cuda_scalbn(double a, int b)
{
  return (double)__cuda_scalbnf((float)a, b);
}

extern __attribute__((weak)) double __cuda_scalbln(double a, long b); double __cuda_scalbln(double a, long b)
{
  return (double)__cuda_scalblnf((float)a, b);
}

extern __attribute__((weak)) double __cuda_frexp(double a, int *b); double __cuda_frexp(double a, int *b)
{
  return (double)__cuda_frexpf((float)a, b);
}

extern __attribute__((weak)) double __cuda_modf(double a, double *b); double __cuda_modf(double a, double *b)
{
  float fb;
  float fa = __cuda_modff((float)a, &fb);

  *b = (double)fb;

  return (double)fa;
}

extern __attribute__((weak)) double __cuda_fmod(double a, double b); double __cuda_fmod(double a, double b)
{
  return (double)__cuda_fmodf((float)a, (float)b);
}

extern __attribute__((weak)) double __cuda_remainder(double a, double b); double __cuda_remainder(double a, double b)
{
  return (double)__cuda_remainderf((float)a, (float)b);
}

extern __attribute__((weak)) double __cuda_remquo(double a, double b, int *c); double __cuda_remquo(double a, double b, int *c)
{
  return (double)__cuda_remquof((float)a, (float)b, c);
}

extern __attribute__((weak)) double __cuda_nextafter(double a, double b); double __cuda_nextafter(double a, double b)
{
  return (double)__cuda_nextafterf((float)a, (float)b);
}

extern __attribute__((weak)) double __cuda_nan(const char *tagp); double __cuda_nan(const char *tagp)
{
  return (double)__cuda_nanf(tagp);
}

extern __attribute__((weak)) double __cuda_pow(double a, double b); double __cuda_pow(double a, double b)
{
  return (double)__cuda_powf((float)a, (float)b);
}

extern __attribute__((weak)) double __cuda_round(double a); double __cuda_round(double a)
{
  return (double)__cuda_roundf((float)a);
}

extern __attribute__((weak)) long __cuda_lround(double a); long __cuda_lround(double a)
{
  return __cuda_lroundf((float)a);
}

extern __attribute__((weak)) long long __cuda_llround(double a); long long __cuda_llround(double a)
{
  return __cuda_llroundf((float)a);
}

extern __attribute__((weak)) double __cuda_rint(double a); double __cuda_rint(double a)
{
  return (double)__cuda_rintf((float)a);
}

extern __attribute__((weak)) long __cuda_lrint(double a); long __cuda_lrint(double a)
{
  return __cuda_lrintf((float)a);
}

extern __attribute__((weak)) long long __cuda_llrint(double a); long long __cuda_llrint(double a)
{
  return __cuda_llrintf((float)a);
}

extern __attribute__((weak)) double __cuda_nearbyint(double a); double __cuda_nearbyint(double a)
{
  return (double)__cuda_nearbyintf((float)a);
}

extern __attribute__((weak)) double __cuda_fdim(double a, double b); double __cuda_fdim(double a, double b)
{
  return (double)__cuda_fdimf((float)a, (float)b);
}

extern __attribute__((weak)) int __cuda_ilogb(double a); int __cuda_ilogb(double a)
{
  return __cuda_ilogbf((float)a);
}

extern __attribute__((weak)) double __cuda_logb(double a); double __cuda_logb(double a)
{
  return (double)__cuda_logbf((float)a);
}

extern __attribute__((weak)) double __cuda_fma(double a, double b, double c); double __cuda_fma(double a, double b, double c)
{
  return (double)__cuda_fmaf((float)a, (float)b, (float)c);
}
# 4168 "/usr/local/cuda/bin/../include/math_functions.h" 2 3
# 89 "/usr/local/cuda/bin/../include/common_functions.h" 2
# 196 "/usr/local/cuda/bin/../include/crt/host_runtime.h" 2
# 6 "/tmp/tmpxft_00001ecc_00000000-0.stub.c" 2
extern void __sti____cudaRegisterAll_29_tmpxft_00001ecc_00000000_2_ii_91788a12(void) __attribute__((__constructor__));
void __sti____cudaRegisterAll_29_tmpxft_00001ecc_00000000_2_ii_91788a12(void){__cudaFatCubinHandle = __cudaRegisterFatBinary((void*)(&__fatDeviceText));}
# 475 "y.cu" 2
