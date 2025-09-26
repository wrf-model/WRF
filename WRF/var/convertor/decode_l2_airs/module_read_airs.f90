!
! Based on the routine airs_ret_rdr supplied through the AIRS web site
!
module read_airs

   integer, parameter :: AIRS_RET_GEOXTRACK      =  30
   integer, parameter :: AIRS_RET_GEOTRACK       =  45
   integer, parameter :: AIRS_RET_STDPRESSURELEV =  28
   integer, parameter :: AIRS_RET_STDPRESSURELAY =  28
   integer, parameter :: AIRS_RET_H2OPRESSURELEV =  15  ! new in V5
   integer, parameter :: AIRS_RET_H2OPRESSURELAY =  14  ! new in V5
   integer, parameter :: AIRS_RET_AIRSXTRACK     =   3
   integer, parameter :: AIRS_RET_AIRSTRACK      =   3
   integer, parameter :: AIRS_RET_CLOUD          =   2
   integer, parameter :: AIRS_RET_CHANAMSUA      =  15
   integer, parameter :: AIRS_RET_CHANHSB        =   5
   integer, parameter :: AIRS_RET_MWHINGESURF    =   7
   integer, parameter :: AIRS_RET_HINGESURF      = 100

   type airs_ret_gran_t
      double precision :: start_Latitude
      double precision :: start_Longitude
      double precision :: start_Time
      double precision :: end_Latitude
      double precision :: end_Longitude
      double precision :: end_Time
      integer :: start_year
      integer :: start_month
      integer :: start_day
      integer :: start_hour
      integer :: start_minute
      real :: start_sec

      double precision, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: Latitude
      double precision, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: Longitude
      double precision, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: Time

      integer*2, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: nStd_mid_top_bndry
      integer*2, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: nStd_bot_mid_bndry
      integer*2, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: RetQAFlag
      integer*2, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: Qual_Temp_Profile_Top
      integer*2, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: Qual_Temp_Profile_Mid
      integer*2, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: Qual_Temp_Profile_Bot
      integer*2, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: Qual_Cloud_OLR
      integer*2, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: Qual_H2O
      integer*2, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: Qual_Surf
      integer*2, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: nBestStd ! new in V5
      integer*2, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: nGoodStd ! new in V5
      real,    dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: PBest      ! new in V5
      real,    dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: PGood      ! new in V5

      integer, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: nSurfStd
      real, dimension(AIRS_RET_STDPRESSURELEV)                 :: pressStd
      real, dimension(AIRS_RET_H2OPRESSURELEV)                 :: pressH2O   ! new in V5
      real, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK)    :: TSurfStd
      real, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK)    :: TSurfAir
      real, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK)    :: PSurfStd
      real, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK)    :: Press_bot_mid_bndry
      real, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK)    :: Press_mid_top_bndry
      real, dimension(AIRS_RET_STDPRESSURELAY,AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: H2OMMRStd
      real, dimension(AIRS_RET_STDPRESSURELAY,AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: H2OMMRStdErr
      real, dimension(AIRS_RET_STDPRESSURELEV,AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: TAirStd
      real, dimension(AIRS_RET_STDPRESSURELEV,AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK) :: GP_Height

      character (len=256) :: processing_level
      character (len=256) :: instrument
      character (len=256) :: DayNightFlag
      character (len=256) :: AutomaticQAFlag
      character (len=256) :: node_type
      character (len=256) :: granules_present

      integer :: NumTotalData
      integer :: NumProcessData
      integer :: NumSpecialData
      integer :: NumBadData
      integer :: NumMissingData
      integer :: NumLandSurface
      integer :: NumOceanSurface

   end type airs_ret_gran_t


   contains


   subroutine airs_ret_rdr(file_name, airs_ret_gran, version)

      implicit none

      ! Arguments
      character (len=*),      intent(in)    :: file_name
      type (airs_ret_gran_t), intent(inout) :: airs_ret_gran
      character (len=2),      intent(out)   :: version       ! V4 or V5
   
      ! Local variables
      integer :: statn           ! HDF-EOS status. 0 for success
      integer :: fid             ! HDF-EOS file ID
      integer :: swid            ! HDF-EOS swath ID
      integer :: nchar           ! Number of characters
      integer :: nswath          ! Number of swaths
      character (len=256) :: swathname ! Name of swath
      character (len=256) :: dimnames  ! Name of dimensions
      integer, dimension(10) :: start  ! start of each dimensions for Swath I/O
                                       ! 0 => start with first element
      integer, dimension(10) :: stride ! stride of each dimensions for Swath I/O
                                       ! 1 => use every element
      integer, dimension(10) :: edge   ! size of each dimension for swath I/O
                                       ! will be set for each individual read
      integer :: swopen, swinqswath, swattach, swinqdims
      integer :: swrdfld, swrdattr
      integer :: swdetach, swclose

      start = 0
      stride = 1
   
      !
      ! Open
      !
      fid = swopen(file_name, 1)
      if (fid == -1) then
         write(6,*) 'Error ', fid, ' opening file ', file_name
         stop
      end if
   
      !
      ! Get name of swath(s)
      !
      nswath = swinqswath(file_name, swathname, nchar)
      if (nswath /= 1) then
         write(6,*) 'swinqswath found ', nswath, ' swaths for file ', file_name, ' Need exactly 1'
         stop
      end if
   
      !
      ! There's exactly one swath.  Make sure it is the right one.
      !
      if (swathname /= 'L2_Standard_atmospheric&surface_product') then
         write(6,*) 'Error: bad swath name ', swathname, ' in file ', file_name
         write(6,*) 'Expected L2_Standard_atmospheric&surface_product'
         stop
      end if
   
      !
      ! Attach to (open) the one swath.
      !
      swid = swattach(fid, swathname)
      if (swid == -1) then
        write(6,*) 'Failed to attach to swath ', swathname,' in file ', file_name
        stop
      end if
   
      !
      ! Read dimension names
      !
      statn = swinqdims(swid, dimnames, nchar) 
      if ( index(dimnames,'H2OPressureLev') > 0 ) then
         version = 'V5'
      else
         version = 'V4'
      end if
      write(6,*) 'Processing version ', version, ' file'

      !
      ! Read attributes
      !
      statn = swrdattr(swid, 'processing_level', airs_ret_gran%processing_level)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute processing_level'
   
      statn = swrdattr(swid, 'instrument', airs_ret_gran%instrument)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute instrument'
   
      statn = swrdattr(swid, 'DayNightFlag', airs_ret_gran%DayNightFlag)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute DayNightFlag'
      airs_ret_gran%DayNightFlag = airs_ret_gran%DayNightFlag(1:index(airs_ret_gran%DayNightFlag,char(0))-1)
   
      statn = swrdattr(swid, 'AutomaticQAFlag', airs_ret_gran%AutomaticQAFlag)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute AutomaticQAFlag'
   
      statn = swrdattr(swid, 'NumTotalData', airs_ret_gran%NumTotalData)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute NumTotalData'
   
      statn = swrdattr(swid, 'NumProcessData', airs_ret_gran%NumProcessData)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute NumProcessData'
   
      statn = swrdattr(swid, 'NumSpecialData', airs_ret_gran%NumSpecialData)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute NumSpecialData'
   
      statn = swrdattr(swid, 'NumBadData', airs_ret_gran%NumBadData)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute NumBadData'
   
      statn = swrdattr(swid, 'NumMissingData', airs_ret_gran%NumMissingData)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute NumMissingData'
   
      statn = swrdattr(swid, 'NumLandSurface', airs_ret_gran%NumLandSurface)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute NumLandSurface'
   
      statn = swrdattr(swid, 'NumOceanSurface', airs_ret_gran%NumOceanSurface)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute NumOceanSurface'
   
      statn = swrdattr(swid, 'node_type', airs_ret_gran%node_type)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute node_type'
   
      statn = swrdattr(swid, 'start_year', airs_ret_gran%start_year)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute start_year'
   
      statn = swrdattr(swid, 'start_month', airs_ret_gran%start_month)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute start_month'
   
      statn = swrdattr(swid, 'start_day', airs_ret_gran%start_day)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute start_day'
   
      statn = swrdattr(swid, 'start_hour', airs_ret_gran%start_hour)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute start_hour'
   
      statn = swrdattr(swid, 'start_minute', airs_ret_gran%start_minute)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute start_minute'
   
      statn = swrdattr(swid, 'start_sec', airs_ret_gran%start_sec)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute start_sec'
   
      statn = swrdattr(swid, 'start_Latitude', airs_ret_gran%start_Latitude)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute start_Latitude'
   
      statn = swrdattr(swid, 'start_Longitude', airs_ret_gran%start_Longitude)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute start_Longitude'
   
      statn = swrdattr(swid, 'start_Time', airs_ret_gran%start_Time)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute start_Time'
   
      statn = swrdattr(swid, 'end_Latitude', airs_ret_gran%end_Latitude)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute end_Latitude'
   
      statn = swrdattr(swid, 'end_Longitude', airs_ret_gran%end_Longitude)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute end_Longitude'
   
      statn = swrdattr(swid, 'end_Time', airs_ret_gran%end_Time)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading attribute end_Time'
   
      start = 0
      stride = 1
   
      !
      ! Read geolocation fields
      !
      edge(1) = AIRS_RET_GEOXTRACK
      edge(2) = AIRS_RET_GEOTRACK
      statn = swrdfld(swid, 'Latitude', start, stride, edge, airs_ret_gran%Latitude)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field Latitude'
   
      statn = swrdfld(swid, 'Longitude', start, stride, edge, airs_ret_gran%Longitude)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field Longitude'
   
      statn = swrdfld(swid, 'Time', start, stride, edge, airs_ret_gran%Time)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field Time'
   
      start = 0
      stride = 1
   
      !
      ! Read data Fields
      !
      edge(2) = 45
      edge(1) = 30
      statn = swrdfld(swid, 'RetQAFlag', start, stride, edge, airs_ret_gran%RetQAFlag)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field RetQAFlag'

      edge(2) = 45
      edge(1) = 30
      statn = swrdfld(swid, 'Press_mid_top_bndry', start, stride, edge, airs_ret_gran%Press_mid_top_bndry)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field Press_mid_top_bndry'

      edge(2) = 45
      edge(1) = 30
      statn = swrdfld(swid, 'Press_bot_mid_bndry', start, stride, edge, airs_ret_gran%Press_bot_mid_bndry)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field Press_bot_mid_bndry'

      if ( version == 'V5' ) then
         edge(2) = 45
         edge(1) = 30
         statn = swrdfld(swid, 'PBest', start, stride, edge, airs_ret_gran%PBest)
         if (statn /= 0) write(6,*) 'Error ', statn, ' reading field PBest'

         edge(2) = 45
         edge(1) = 30
         statn = swrdfld(swid, 'PGood', start, stride, edge, airs_ret_gran%PGood)
         if (statn /= 0) write(6,*) 'Error ', statn, ' reading field PGood'

         edge(2) = 45
         edge(1) = 30
         statn = swrdfld(swid, 'nBestStd', start, stride, edge, airs_ret_gran%nBestStd)
         if (statn /= 0) write(6,*) 'Error ', statn, ' reading field nBestStd'

         edge(2) = 45
         edge(1) = 30
         statn = swrdfld(swid, 'nGoodStd', start, stride, edge, airs_ret_gran%nGoodStd)
         if (statn /= 0) write(6,*) 'Error ', statn, ' reading field nGoodStd'

         edge(1) = AIRS_RET_H2OPRESSURELEV
         statn = swrdfld(swid, 'pressH2O', start, stride, edge, airs_ret_gran%pressH2O)
         if (statn /= 0) write(6,*) 'Error ', statn, ' reading field pressH2O'
   
      end if

      edge(2) = 45
      edge(1) = 30
      statn = swrdfld(swid, 'Qual_Cloud_OLR', start, stride, edge, airs_ret_gran%Qual_Cloud_OLR)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field Qual_Cloud_OLR'
   
      edge(2) = 45
      edge(1) = 30
      statn = swrdfld(swid, 'Qual_H2O', start, stride, edge, airs_ret_gran%Qual_H2O)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field Qual_H2O'
   
      edge(2) = 45
      edge(1) = 30
      statn = swrdfld(swid, 'Qual_Temp_Profile_Top', start, stride, edge, airs_ret_gran%Qual_Temp_Profile_Top)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field Qual_Temp_Profile_Top'
   
      edge(2) = 45
      edge(1) = 30
      statn = swrdfld(swid, 'Qual_Temp_Profile_Mid', start, stride, edge, airs_ret_gran%Qual_Temp_Profile_Mid)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field Qual_Temp_Profile_Mid'
   
      edge(2) = 45
      edge(1) = 30
      statn = swrdfld(swid, 'Qual_Temp_Profile_Bot', start, stride, edge, airs_ret_gran%Qual_Temp_Profile_Bot)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field Qual_Temp_Profile_Bot'
   
      edge(2) = 45
      edge(1) = 30
      statn = swrdfld(swid, 'Qual_Surf', start, stride, edge, airs_ret_gran%Qual_Surf)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field Qual_Surf'
   
      edge(2) = 45
      edge(1) = 30
      statn = swrdfld(swid, 'PSurfStd', start, stride, edge, airs_ret_gran%PSurfStd)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field PSurfStd'
   
      edge(2) = 45
      edge(1) = 30
      statn = swrdfld(swid, 'nSurfStd', start, stride, edge, airs_ret_gran%nSurfStd)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field nSurfStd'

      edge(1) = 28
      statn = swrdfld(swid, 'pressStd', start, stride, edge, airs_ret_gran%pressStd)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field pressStd'
   
      edge(2) = 45
      edge(1) = 30
      statn = swrdfld(swid, 'nStd_mid_top_bndry', start, stride, edge, airs_ret_gran%nStd_mid_top_bndry)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field nStd_mid_top_bndry'
   
      edge(2) = 45
      edge(1) = 30
      statn = swrdfld(swid, 'nStd_bot_mid_bndry', start, stride, edge, airs_ret_gran%nStd_bot_mid_bndry)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field nStd_bot_mid_bndry'
   
      edge(2) = 45
      edge(1) = 30
      statn = swrdfld(swid, 'TSurfStd', start, stride, edge, airs_ret_gran%TSurfStd)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field TSurfStd'
   
      edge(2) = 45
      edge(1) = 30
      statn = swrdfld(swid, 'TSurfAir', start, stride, edge, airs_ret_gran%TSurfAir)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field TSurfAir'
   
      edge(3) = 45
      edge(2) = 30
      edge(1) = 28
      statn = swrdfld(swid, 'TAirStd', start, stride, edge, airs_ret_gran%TAirStd)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field TAirStd'

      edge(3) = 45
      edge(2) = 30
      edge(1) = 28
      statn = swrdfld(swid, 'GP_Height', start, stride, edge, airs_ret_gran%GP_Height)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field GP_Height'
   
      edge(3) = 45
      edge(2) = 30
      if ( version == 'V5' ) then
         edge(1) = AIRS_RET_H2OPRESSURELAY
      else
         edge(1) = AIRS_RET_STDPRESSURELAY
      end if
      airs_ret_gran%H2OMMRStd = -9999.0   ! initialize
      statn = swrdfld(swid, 'H2OMMRStd', start, stride, edge, airs_ret_gran%H2OMMRStd)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field H2OMMRStd'
   
      edge(3) = 45
      edge(2) = 30
      if ( version == 'V5' ) then
         edge(1) = AIRS_RET_H2OPRESSURELAY
      else
         edge(1) = AIRS_RET_STDPRESSURELAY
      end if
      airs_ret_gran%H2OMMRStdErr = -9999.0   ! initialize
      statn = swrdfld(swid, 'H2OMMRStdErr', start, stride, edge, airs_ret_gran%H2OMMRStdErr)
      if (statn /= 0) write(6,*) 'Error ', statn, ' reading field H2OMMRStdErr'
      !
      ! Final clean-up
      !
      statn = swdetach(swid)
      ! if (statn /= 0) write(6,*) 'Error detaching from input file ', file_name

      statn = swclose(fid)
      ! if (statn /= 0) write(6,*) 'Error closing input file ', file_name
   
   end subroutine airs_ret_rdr
   
end module read_airs
