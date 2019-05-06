subroutine da_cloud_detect(isensor,nchannels,ndim,kts,kte,n,iv)

!** *CLOUD_DETECT* - CLOUD FLAGGING FOR IR Channels 

!    AUTHOR: THOMAS AULIGNE      DATE : 01/08/2005
!
!    PURPOSE.
!    -------
!    FLAG THE PRESENCE OF CLOUD CONTAMINATION IN IR CHANNELS
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
!**  METHODS
!    CLOUD DETECTION SCHEME MMR FROM AULIGNÉ T. MWR (2014).OR. PF FROM XU ET AL., GMD (2016)
!    MODIFICATIONS
!    -------------
!    BY DONGMEI XU 201904
!    PURPOSE, ADD CLOUD DETECTION METHOD BASED ON PARTICLE FILTER
!    METHOD, Xu et al., 2016: A method for retrieving clouds with satellite infrared radiances using the particle filter. Geosci. Model Dev., 9, 3919–3932
!
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
INTEGER,PARAMETER     :: IASI_Max_Channels = 8079
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
INTEGER               :: NCHAN,k
LOGICAL               :: LMATCH
INTEGER               :: Band_Size(5)
INTEGER               :: Bands(IASI_Max_Channels,5)
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
INTEGER               :: gn
REAL :: ZHOOK_HANDLE


logical   :: iasi,airs, modis,imager,sounder,cris,giirs,ahi
double precision, allocatable            :: ppx(:,:),wx(:),jo(:) 
integer                   :: p1,ii,jj
double precision      :: tmp
! Initializations


iasi  = trim(rttov_inst_name(rtminit_sensor(isensor))) == 'iasi'
airs  = trim(rttov_inst_name(rtminit_sensor(isensor))) == 'airs'	  	 
imager  = trim(rttov_inst_name(rtminit_sensor(isensor))) == 'imager'
ahi  = trim(rttov_inst_name(rtminit_sensor(isensor))) == 'ahi'		 
Band_Size(:)   = 0
Bands(:,:)     = 0 
	  
if ( iasi ) then	  
      Band_Size(1:5) = (/ 193, 15, 116, 4, 15 /)
 
      Bands(1:Band_Size(1),1) = &
&      (/    16,   38,   49,   51,   55,   57,   59,   61,   63,   66, &
&            70,   72,   74,   79,   81,   83,   85,   87,   89,   92, &
&            95,   97,   99,  101,  104,  106,  109,  111,  113,  116, &
&           119,  122,  125,  128,  131,  133,  135,  138,  141,  144, &
&           146,  148,  151,  154,  157,  159,  161,  163,  165,  167, &
&           170,  173,  176,  178,  179,  180,  183,  185,  187,  189, &
&           191,  193,  195,  197,  199,  201,  203,  205,  207,  210, &
&           212,  214,  217,  219,  222,  224,  226,  228,  230,  232, &
&           234,  236,  239,  241,  242,  243,  246,  249,  252,  254, &
&           256,  258,  260,  262,  265,  267,  269,  271,  272,  273, &
&           275,  278,  280,  282,  284,  286,  288,  290,  292,  294, &
&           296,  299,  301,  303,  306,  308,  310,  312,  314,  316, &
&           318,  320,  323,  325,  327,  329,  331,  333,  335,  337, &
&           339,  341,  343,  345,  347,  350,  352,  354,  356,  358, &
&           360,  362,  364,  366,  369,  371,  373,  375,  377,  379, &
&           381,  383,  386,  389,  398,  401,  404,  407,  410,  414, &
&           416,  426,  428,  432,  434,  439,  445,  457,  515,  546, &
&           552,  559,  566,  571,  573,  646,  662,  668,  756,  867, &
&           921, 1027, 1090, 1133, 1191, 1194, 1271, 1805, 1884, 1946, &
&          1991, 2094, 2239 /)


      Bands(1:Band_Size(2),2) = &
&      (/ 1479, 1509, 1513, 1521, 1536, 1574, 1579, 1585, 1587, 1626, &
&         1639, 1643, 1652, 1658, 1671  /)

      Bands(1:Band_Size(3),3) = &
&      (/ 2119, 2213, 2271, 2321, 2398, 2701, 2741, 2819, 2889, 2907, 2910, &
&         2919, 2939, 2944, 2948, 2951, 2958, 2977, 2985, 2988, 2991, &
&         2993, 3002, 3008, 3014, 3027, 3029, 3036, 3047, 3049, 3053, &
&         3058, 3064, 3069, 3087, 3093, 3098, 3105, 3107, 3110, 3127, &
&         3136, 3151, 3160, 3165, 3168, 3175, 3178, 3207, 3228, 3244, &
&         3248, 3252, 3256, 3263, 3281, 3303, 3309, 3312, 3322, 3375, &
&         3378, 3411, 3438, 3440, 3442, 3444, 3446, 3448, 3450, 3452, &
&         3454, 3458, 3467, 3476, 3484, 3491, 3497, 3499, 3504, 3506, &
&         3509, 3518, 3527, 3555, 3575, 3577, 3580, 3582, 3586, 3589, &
&         3599, 3653, 3658, 3661, 4032, 5368, 5371, 5379, 5381, 5383, &
&         5397, 5399, 5401, 5403, 5405, 5455, 5480, 5483, 5485, 5492, &
&         5502, 5507, 5509, 5517, 5558  /)                                           !&    1812, 1826, 1843  /)

      Bands(1:Band_Size(4),4) = &
&      (/   5988, 5992, 5994, 6003  /)                              !&    1921, 1923, 1924, 1928, 1937  /)   

      Bands(1:Band_Size(5),5) = &
&      (/  6982, 6985, 6987, 6989, 6991, 6993, 6995, 6997, 7267, 7269, &
&          7424, 7426, 7428, 7885, 8007 /)   

else if (airs) then
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

      Bands(1:Band_Size(4),4) = &
&    (/ 1852, 1865, 1866,       1868, 1869, 1872, 1873,       1876, &  !&    1852, 1865, 1866, 1867, 1868, 1869, 1872, 1873, 1875, 1876, 
&             1881, 1882, 1883,                   1911, 1917, 1918, &  !&    1877, 1881, 1882, 1883, 1884, 1897, 1901, 1911, 1917, 1918, &
&                   1924, 1928        /)                               !&    1921, 1923, 1924, 1928, 1937  /)   
 
else if (imager) then
      Band_Size(1) = 2
      Bands(1:Band_Size(1),1) = &
&   (/4,5/)	
else if (ahi) then
      Band_Size(1) = 4
      Bands(1:Band_Size(1),1) = &
&   (/7,8,9,10/)	 
end if

    allocate(ppx(ndim*11,ndim))  
    allocate(wx(ndim*11))    
    allocate(jo(ndim*11))
    wx=1.0
    jo=0.0
    px(1:ndim-1) = 1.0/ndim
    px(ndim)     = 1.0 - SUM(px(1:ndim-1))
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
          
        if (use_clddet==2)  then
          p1=0		  
          do ii=0,10
            do jj=1,ndim-1
              p1=p1+1
              ppx(p1,1:ndim)=0 !initialization
              ppx(p1,jj)=real(ii)/10
              ppx(p1,ndim)=1-ppx(p1,jj) !initialization
              jo(p1)=iv%instid(isensor)%tb_error(ich,n)*(1-SUM(ppx(p1,1:ndim)*amat(nchan,1:ndim)))**2
           end do
          end do		  
          ! step 2 calculate the weight
          do ii=1,p1
             ! jo(ii)=jo(ii)-mv
              tmp=exp(-jo(ii))/sum(exp(-jo(1:p1))) ! normalize the weight
              wx(ii)=tmp*wx(ii) !1/jo  !exp(-jo)
          end do
          wx(:)=wx(:)/sum(wx(1:p1))		
          px=0
          do k = 1,ndim
            px(k)= SUM(wx(1:p1)*ppx(1:p1,k))
          end do
          px(1:ndim) = px(1:ndim) / SUM(px(1:ndim))   ! Re-normalization
        end if		  
        ENDDO
        IF (.NOT. LMATCH) then
           if (print_detail_rad) then
              write(unit=message(1),fmt='(A,2I8)') 'CLOUD_DETECT: No match for channel:',i,Bands(i,JBAND)
              call da_message(message(1:1))
           endif
        ENDIF
      ENDDO
    ENDDO BAND_LOOP                      ! Loop over band

if (use_clddet==1) then    
    !--------------------!
    ! Hessian evaluation !
    !--------------------!
    IF (LPRECON) THEN
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
      ENDDO  
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


!      call da_error(__FILE__,__LINE__, &
!             (/"inria_n2qn1 is not implemented here, please contact the author of this subroutine."/))
!    call inria_n2qn1(da_cloud_sim,NDIM,px,ZF,ZG,(/(ZDXMIN,jlev=1,NDIM)/),ZDF1, &
!                ZEPSG,impres,io,IMODE,nit,NSIM,binf,bsup,IZ,ZRZ,izs,RZS,DZS)
  
      if (allocated(iz))  deallocate(iz)       
      if (allocated(zrz)) deallocate(zrz)       
      if (allocated(dzs)) deallocate(dzs)       
      if (allocated(rzs)) deallocate(rzs)                 
end if !mmr
      deallocate(ppx)
      deallocate(wx)
      deallocate(jo)         
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
    
end subroutine da_cloud_detect
