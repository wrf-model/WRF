subroutine da_cloud_detect_airs(isensor,nchannels,ndim,kts,kte,n,iv)

!** *CLOUD_DETECT_AIRS* - CLOUD FLAGGING FOR AIRS AND IASI

!    AUTHOR: THOMAS AULIGNE      DATE : 01/08/2005
!
!    PURPOSE.
!    -------
!    FLAG THE PRESENCE OF CLOUD CONTAMINATION IN AIRS AND IASI CHANNELS
!
!**  INTERFACE.
!    ---------
!    WHERE nchannels    : Number of channels
!          kts          : model level corresponding to 100hPa (top of initial cloud search)
!          kte          : model level corresponding to surface (lower extent of cloud)            
!          rad_obs      : Potentially cloudy observations
!          rad_clr      : Clear radiance from Model
!          rad_ovc      : Model overcast radiance estimates
!          cloud_flag   : Cloud flag by channel; 1=clear, -1=cloudy
!
!**  EXTERNALS
!    ---------
!    N2QN1  - Minimization algorithm (double-precision constrained version of M1QN3)
!
!    MODIFICATIONS
!    -------------
!    NONE
!**  -----------------------------------------

IMPLICIT NONE

!* 0.1 Global arrays
INTEGER,INTENT(IN)    :: isensor             ! sensor index. 
INTEGER,INTENT(IN)    :: nchannels           ! number of channels 
INTEGER,INTENT(IN)    :: ndim                ! model levels between surface (lower extent of cloud) and 100hPa (top of cloud search)     
INTEGER,INTENT(IN)    :: kts                 ! model level corresponding to 100hPa (top of initial cloud search)
INTEGER,INTENT(IN)    :: kte                 ! model level corresponding to surface (lower extent of cloud)
INTEGER,INTENT(IN)    :: n                   ! pixel index 
type (iv_type), intent(inout)  :: iv         ! O-B structure.

INTEGER,PARAMETER     :: NITER             = 100
INTEGER,PARAMETER     :: NBAND             = 1
LOGICAL,PARAMETER     :: LPRECON           = .false.
INTEGER,PARAMETER     :: NEIGNVEC          = 4
INTEGER,PARAMETER     :: AIRS_Max_Channels = 2378

!! local declarations 
INTEGER               :: ichan(nchannels)    ! AIRS and IASI channel IDs
REAL                  :: rad_obs(nchannels)  ! Observed radiance
REAL                  :: rad_clr(nchannels)  ! Model clear radiance estimates
REAL                  :: rad_ovc(nchannels,ndim-1) ! RT overcast radiance estimates
double precision      :: px(ndim) !neignvec)        ! Cloud fractions
REAL                  :: rad_cld(nchannels)
INTEGER               :: ich,ilev,jlev,i,j,JBAND
double precision      :: ZF, ZF_CLR
double precision      :: ZG(ndim)
double precision      :: binf(ndim), bsup(ndim)
REAL                  :: AMAT(nchannels,ndim)
INTEGER               :: NCHAN
LOGICAL               :: LMATCH
INTEGER               :: Band_Size(5)
INTEGER               :: Bands(AIRS_Max_Channels,5) 
integer               :: cldtoplevel

! Hessian evaluation
 REAL                 :: hessian(ndim,ndim), eignvec(ndim,ndim), eignval(ndim)

!! local declarations for N2QN1 !!
INTEGER               :: NRZ, impres, io, IMODE, NSIM, nit, izs(2)
double precision      :: ZDF1, ZDXMIN, ZEPSG
double precision ,ALLOCATABLE :: ZRZ(:)
real, allocatable     :: RZS(:)
INTEGER, ALLOCATABLE  :: IZ(:)
DOUBLE PRECISION, ALLOCATABLE :: DZS(:)

REAL :: ZHOOK_HANDLE

! Initializations
      Band_Size(:)   = 0
      Bands(:,:)     = 0 
      Band_Size(1:5) = (/86, 0, 0, 16, 0 /)
 
      Bands(1:Band_Size(1),1) = &
&    (/                                                 &              !&      1,   6,   7,  10,  11,  15,  16,  17,  20,  21, &
&                                                       &              !&     22,  24,  27,  28,  30,  36,  39,  40,  42,  51, &
&                                                       &              !&     52,  54,  55,  56,  59,  62,  63,  68,  69,  71, &
&                                                       &              !&     72,  73,  74,  75,  76,  77,  78,  79,  80,  82, &
&                     92,  93,  98,  99, 101, 104, 105, &              !&     83,  84,  86,  92,  93,  98,  99, 101, 104, 105, &
&     108, 110, 111, 113, 116, 117, 123, 124, 128, 129, &
&     138, 139, 144, 145, 150, 151, 156, 157, 159, 162, &
&     165, 168, 169, 170, 172, 173, 174, 175, 177, 179, &
&     180, 182, 185, 186, 190, 192,      198, 201, 204, &              !&     180, 182, 185, 186, 190, 192, 193, 198, 201, 204, &
&     207, 210,      215, 216,      221,      226, 227, &              !&     207, 210, 213, 215, 216, 218, 221, 224, 226, 227, &
&     232,                     252, 253, 256, 257, 261, &              !&     232, 239, 248, 250, 251, 252, 253, 256, 257, 261, &
&     262, 267, 272, 295, 299,      305,           310, &              !&     262, 267, 272, 295, 299, 300, 305, 308, 309, 310, &
&          321, 325, 333, 338, 355, 362, 375, 453, 475, &              !&     318, 321, 325, 333, 338, 355, 362, 375, 453, 475, &
&     484, 497, 528, 587, 672, 787, 791, 843, 870, 914, &
&     950 /)

!      Bands(1:Band_Size(2),2) = &
!&    (/ 1003, 1012, 1019, 1024, 1030, 1038, 1048, 1069, 1079, 1082,  &
!&       1083, 1088, 1090, 1092, 1095, 1104, 1111, 1115, 1116, 1119,  &
!&       1120, 1123, 1130, 1138, 1142, 1178, 1199, 1206, 1221, 1237,  &
!&       1252, 1260, 1263, 1266, 1278, 1285 /)

!      Bands(1:Band_Size(3),3) = &
!&    (/       1301, 1304, 1329, 1371, 1382, 1415, 1424, 1449, 1455, &  !&    1290, 1301, 1304, 1329, 1371, 1382, 1415, 1424, 1449, 1455, &  
!&       1466,       1477,             1500, 1519,       1538, 1545, &  !&    1466, 1471, 1477, 1479, 1488, 1500, 1519, 1520, 1538, 1545, &  
!&       1565, 1574, 1583, 1593,       1627, 1636,       1652, 1669, &  !&    1565, 1574, 1583, 1593, 1614, 1627, 1636, 1644, 1652, 1669, & 
!&                   1694, 1708,       1723, 1740, 1748,       1756, &  !&    1674, 1681, 1694, 1708, 1717, 1723, 1740, 1748, 1751, 1756, &
!&             1766, 1771, 1777,       1783, 1794, 1800,       1806, &  !&    1763, 1766, 1771, 1777, 1780, 1783, 1794, 1800, 1803, 1806, &
!&             1826, 1843  /)                                           !&    1812, 1826, 1843  /)

      Bands(1:Band_Size(4),4) = &
&    (/ 1852, 1865, 1866,       1868, 1869, 1872, 1873,       1876, &  !&    1852, 1865, 1866, 1867, 1868, 1869, 1872, 1873, 1875, 1876, 
&             1881, 1882, 1883,                   1911, 1917, 1918, &  !&    1877, 1881, 1882, 1883, 1884, 1897, 1901, 1911, 1917, 1918, &
&                   1924, 1928        /)                               !&    1921, 1923, 1924, 1928, 1937  /)   

!      Bands(1:Band_Size(5),5) = &
!&    (/ 1938, 1939, 1941, 1946, 1947, 1948, 1958, 1971, 1973, 1988, &
!&       1995, 2084, 2085, 2097, 2098, 2099, 2100, 2101, 2103, 2104, &
!&       2106, 2107, 2108, 2109, 2110, 2111, 2112, 2113, 2114, 2115, &
!&       2116, 2117, 2118, 2119, 2120, 2121, 2122, 2123, 2128, 2134, &
!&       2141, 2145, 2149, 2153, 2164, 2189, 2197, 2209, 2226, 2234, &
!&       2280, 2318, 2321, 2325, 2328, 2333, 2339, 2348, 2353, 2355, &
!&       2363, 2370, 2371, 2377  /)  

    ichan        = iv%instid(isensor)%ichan(1:nchannels)
    rad_clr      = iv%instid(isensor)%rad_xb(1:nchannels,n)              !iv%instid(isensor)%tb_xb(1:nchan,n)
    rad_obs      = iv%instid(isensor)%rad_obs(1:nchannels,n)             !iv%instid(isensor)%tb_inv(1:nchan,n) + rad_clr
    rad_ovc      = iv%instid(isensor)%rad_ovc(1:nchannels,kts+1:kte,n)

    nchan        = 0
    AMAT(:,:)    = 0.0
    px(1:ndim-1) = 0.0
    px(ndim)     = 1.0
    ZF_CLR       = 0.0
    nit          = niter

!do ich=1,nchannels  
!   CALL CRTM_Planck_Radiance(11,ichan(ich),tb_obs(ich),rad_obs(ich))
!   CALL CRTM_Planck_Radiance(11,ichan(ich),tb_clr(ich),rad_clr(ich))
!end do                

    !--------------------!
    !   Loop over band   ! 
    !--------------------!
    BAND_LOOP: DO JBAND = 1, NBAND
      DO i = 1, Band_Size(JBAND)
        LMATCH = .FALSE.
        DO ich=1,nchannels        
          IF (ichan(ich)/= Bands(i,JBAND)) CYCLE 
          IF ((rad_obs(ich)<=0.0).OR.(rad_obs(ich)>1000.0)) CYCLE
          IF ((rad_clr(ich)<=0.0).OR.(rad_clr(ich)>1000.0)) CYCLE
          IF (ANY(rad_ovc(ich,1:NDIM-1)<=0.0)) CYCLE
          IF (ANY(rad_ovc(ich,1:NDIM-1)>1000.0)) CYCLE

          LMATCH               = .TRUE.    !! Found match for channel
          nchan                = nchan +1
          AMAT(nchan,1:ndim-1) = rad_ovc(ich,1:NDIM-1) / rad_obs(ich)
	  AMAT(nchan,ndim)     = rad_clr(ich) / rad_obs(ich)
          ZF_CLR               = ZF_CLR + 0.5*(AMAT(nchan,ndim)-1.0)**2   
        ENDDO
        IF (.NOT. LMATCH) WRITE(*,*) &
           'CLOUD_DETECT_AIRS: No matching for channel:',i,Bands(i,JBAND) 
      ENDDO	
    ENDDO BAND_LOOP                      ! Loop over band
    
    !--------------------!
    ! Hessian evaluation !
    !--------------------!
    IF (LPRECON) THEN
!      write(76,*) '**** HESSIAN ****'
      hessian(:,:)= 0.0      
      DO ilev=1, NDIM
        DO jlev=ilev, NDIM
          DO J=1,NCHAN
              hessian(ilev,jlev) = hessian(ilev,jlev)  + &
                                  (AMAT(J,ilev)-AMAT(J,NDIM)) * &
				  (AMAT(J,jlev)-AMAT(J,NDIM))
          ENDDO
          hessian(jlev,ilev) = hessian(ilev,jlev)   
        ENDDO
!	write(76,*) hessian(ilev,1:NDIM)
      ENDDO  
!!!      call da_eof_decomposition(ndim, hessian, eignvec, eignval)
    ENDIF
       
     !-----------------!
     ! n2qn1 minimizer !
     !-----------------!
      impres = 2
      io     = 66
      NSIM   = NITER+5
      ZDXMIN = 1.e-6
      ZEPSG  = 1.e-3 !e-9
      IMODE  = 1
      NRZ    = NDIM*(NDIM+9)/2 ! N2QN1
      ALLOCATE(IZ(2*NDIM +1))
      ALLOCATE(ZRZ(NRZ))
      ALLOCATE(DZS(NCHAN*NDIM))
      allocate(rzs(ndim*neignvec))
      binf   = -1000.0
      bsup   = 1000.0
      izs(1) = nchan
      izs(2) = neignvec
      rzs    = 0.0
      ZRZ    = 0.0
      dzs(1:NCHAN*NDIM)=RESHAPE(AMAT(1:NCHAN,1:NDIM),(/NCHAN*NDIM/))
	  
      IF (LPRECON) THEN
        IMODE = 2
        i     = 0
        DO ilev=1, NDIM
          DO jlev=ilev, NDIM
            i = i + 1
            ZRZ(i) = hessian(jlev,ilev)
          ENDDO
        ENDDO    
      ENDIF
!      rzs(1:ndim*neignvec)                  = RESHAPE(eignvec(1:ndim,1:neignvec),(/ndim*neignvec/))
!      rzs(ndim*neignvec+1:(ndim+1)*neignvec)= eignval(1:neignvec)

      call da_cloud_sim(0,NDIM,px,ZF,ZG,izs,RZS,DZS)
      ZDF1      = 1.e-1*ZF 


      call da_error(__FILE__,__LINE__, &
             (/"inria_n2qn1 is not implemented here, please contact the author of this subroutine."/))
!     call inria_n2qn1(da_cloud_sim,NDIM,px,ZF,ZG,(/(ZDXMIN,jlev=1,NDIM)/),ZDF1, &
!                ZEPSG,impres,io,IMODE,nit,NSIM,binf,bsup,IZ,ZRZ,izs,RZS,DZS)
  
      IF (ALLOCATED(IZ))  DEALLOCATE(IZ)       
      IF (ALLOCATED(ZRZ)) DEALLOCATE(ZRZ)       
      IF (ALLOCATED(DZS)) DEALLOCATE(DZS)       
      if (allocated(rzs)) deallocate(rzs)                 
         
      !-----------------!
      ! Cloudy radiance !
      !-----------------!
      DO ich=1,nchannels
        rad_cld(ich) = SUM(px(1:ndim-1) * rad_ovc(ich,1:ndim-1)) + px(ndim) * rad_clr(ich) 
	
	if (ABS(rad_cld(ich)-rad_clr(ich)) < 0.01*rad_clr(ich)) then
	   iv%instid(isensor)%cloud_flag(ich,n) = qc_good
	else
   	   iv%instid(isensor)%cloud_flag(ich,n) = qc_bad
	end if   
      ENDDO    
      
    ! Dump cloud top pressure
    do ilev = kte, kts+2, -1
      if (px(ilev-kts+1) > 0.01) cldtoplevel = ilev
    end do   
    
    if (rtm_option == rtm_option_rttov) then
#ifdef RTTOV
       iv%instid(isensor)%clwp(n) = coefs(isensor)%coef%ref_prfl_p(cldtoplevel)
#endif
    elseif (rtm_option == rtm_option_crtm) then
#ifdef CRTM
       iv%instid(isensor)%clwp(n) = iv%instid(isensor)%pm(cldtoplevel,n)
#endif
    end if  	    
    
 
end subroutine da_cloud_detect_airs
