! Module for handling all associated scale_factor, add_offset, and
! attributes for individual files across various possible
! National Water Model output files. In the future, this will move to a
! table that the user will be able to switch on/off variables for
! outputting. For now, attributes, etc will be stored here.

! Logan Karsten
! National Center for Atmospheric Research
! Research Applications Laboratory
! karsten at ucar dot edu

module module_NWM_io_dict

  use module_version, only: get_model_version
  use config_base, only: nlst
  use module_hydro_stop, only: HYDRO_stop

implicit none

! Declare parameter values for module.
integer, parameter :: numChVars = 11
integer, parameter :: numLdasVars = 116
! Note: if more ldas variables are added the logic will need to be changed in
!       module_NWM_io.F:output_NoahMP_NWM for when to close the restart file
integer, parameter :: numLdasVars_crocus_off = 98

integer, parameter :: numRtDomainVars = 5
integer, parameter :: numLakeVars = 2
integer, parameter :: numChGrdVars = 1
integer, parameter :: numLsmVars = 14
integer, parameter :: numChObsVars = 1
integer, parameter :: numGwVars = 4
integer :: i
!integer :: nsoil = nlst_rt(1)%nsoil
integer :: nsoil

! Declare public types that will hold metadata
public :: chrtMeta ! Public CHRTOUT metadata for NWM output.
public :: ldasMeta ! Public LDASOUT metadata for NWM output.
public :: rtDomainMeta ! Public RT_DOMAIN metadata for NWM output.
public :: lakeMeta ! Public lake metadata for NWM output
public :: chrtGrdMeta ! Public CHRTOUT_GRID metadata for NWM output.
public :: lsmMeta ! Public metadata for LSMOUT output.
public :: chObsMeta ! Public CHANOBS metadata for NWM output.
public :: gwMeta ! Public groundwater metadata.

real*8 :: one_dbl = 1.0d0

! Establish types for each output type
type chrtMeta
   ! Variable names
   character (len=64), dimension(numChVars) :: varNames
   integer :: numVars = numChVars
   character (len=512) :: modelOutputType = "channel_rt"
   character (len=64) :: modelConfigType
   ! Output variable attributes
   real, dimension(numChVars) :: scaleFactor ! scale_factor values used for each
                                                  ! variable to converte from real to
                                                  ! integer.
   real, dimension(numChVars)             :: addOffset   ! add_offset values for each variable.
   character (len=64), dimension(numChVars) :: longName  ! Long names for each variable.
   character (len=64), dimension(numChVars) :: units ! Units for each variable.
   character (len=64), dimension(numChVars) :: coordNames ! Coordinate names for each variable.
   integer(kind=4), dimension(numChVars) :: validMinComp ! Valid min (after conversion to integer)
   integer(kind=4), dimension(numChVars) :: validMaxComp ! Valid max (after conversion to integer)
   real*8, dimension(numChVars) :: validMinDbl ! Valid minimum (before conversion to integer)
   real*8, dimension(numChVars) :: validMaxDbl ! Valid maximum (before converstion to integer)
   integer(kind=4), dimension(numChVars) :: missingComp ! Missing value attribute (after conversion to integer)
   real, dimension(numChVars) :: missingReal ! Missing value attribute (before conversion to integer)
   real, dimension(numChVars) :: fillReal ! Fill value (before conversion to integer)
   integer(kind=4), dimension(numChVars) :: fillComp ! Fill value (after conversion to integer)
   integer, dimension(numChVars) :: outFlag ! 0/1 flag to turn outputting off/on
   integer, dimension(numChVars) :: timeZeroFlag ! 0/1 flag to either set variable to all NDV values or output
                                                 ! the actual data. This was done because time 0
                                                 ! output does not all contain valid data.
   real :: modelNdv ! NDV value represented within the model code
   ! Time variable attribues
   character (len=64) :: timeLName ! long_name - usually valid output time
   character (len=64) :: timeUnits ! Usually seconds since 1/1/1970
   character (len=64) :: timeStName ! standard_name - usually time
   integer :: timeValidMin ! the minimum time each configuration can have, time of the first output file
   integer :: timeValidMax ! the maximum time each configuration can have, time of the last output file
   ! Reference time attributes
   character (len=64) :: rTimeLName ! long_name - usually model initialization time
   character (len=64) :: rTimeStName ! standard_name - usually forecast_reference_time
   character (len=64) :: rTimeUnits ! Usually seconds since 1/1/1970
   ! feature_id attributes
   character (len=256) :: featureIdLName ! long_name - usually Reach ID
   character (len=256) :: featureIdComment ! Comment attribute
   character (len=64) :: cfRole ! cf_role attribute
   ! latitude variable attributes
   character (len=64) :: latLName ! long_name
   character (len=64) :: latUnits ! units
   character (len=64) :: latStName ! Standard Name
   ! longitude variable attributes
   character (len=64) :: lonLName ! long_name
   character (len=64) :: lonUnits ! units
   character (len=64) :: lonStName ! Standard Name
   ! Elevation variable attributes
   character (len=64) :: elevLName ! long_name
   character (len=64) :: elevUnits ! units
   character (len=64) :: elevStName ! Standard Name

   ! Order variable attributes
   character (len=64) :: orderLName ! long_name
   character (len=64) :: orderStName ! Standard Name
   ! Global attributes
   character (len=128) :: title ! file title
   character (len=128) :: fType ! featureType attribute
   character (len=128) :: proj4 ! proj4 attribute
   character (len=128) :: initTime ! model_initialization_time attribute
   character (len=128) :: validTime ! model_output_valid_time attribute
   character (len=128) :: stDim ! station_dimension attribute
   integer :: stOrder ! stream_order_output attribute
   character (len=128) :: cdm ! cdm_datatype attribute
   character (len=1024) :: esri ! esri_pe_string attribute
   character (len=128) :: conventions ! Conventions string
   integer             :: totalValidTime ! # number of valid time (#of output files)


end type chrtMeta

type ldasMeta
   ! Variable names
   character (len=64), dimension(numLdasVars) :: varNames
   integer :: numVars = numLdasVars
   integer :: numSnowLayers = 3
   integer :: numSoilLayers                       ! Fill from Namelist
   integer :: act_lev
   integer :: numSpectrumBands = 2
   character (len=512) :: modelOutputType = "land"
   character (len=64) :: modelConfigType

   ! Output variable attributes
   real, dimension(numLdasVars) :: scaleFactor ! scale_factor values used for each
                                                  ! variable to converte from
                                                  ! real to
                                                  ! integer
   real, dimension(numLdasVars)             :: addOffset   ! add_offset values for each variable.
   character (len=128), dimension(numLdasVars) :: longName  ! Long names for each variable.
   character (len=64), dimension(numLdasVars) :: units ! Units for each variable.
   integer, dimension(numLdasVars) :: numLev ! Number of levels for each variable.
   integer(kind=4), dimension(numLdasVars) :: validMinComp ! Valid min (after conversion to integer)
   integer(kind=4), dimension(numLdasVars) :: validMaxComp ! Valid max (after conversion to integer)
   real*8, dimension(numLdasVars) :: validMinDbl ! Valid minimum (before conversion to integer)
   real*8, dimension(numLdasVars) :: validMaxDbl ! Valid maximum (before converstion to integer)
   integer(kind=4), dimension(numLdasVars) :: missingComp ! Missing value attribute (after conversion to integer)
   real, dimension(numLdasVars) :: missingReal ! Missing value attribute (before conversion to integer)
   real, dimension(numLdasVars) :: fillReal ! Fill value (before conversion to integer)
   integer(kind=4), dimension(numLdasVars) :: fillComp ! Fill value (after conversion to integer)
   integer, dimension(numLdasVars) :: outFlag ! 0/1 flag to turn outputting off/on
   integer, dimension(numLdasVars) :: timeZeroFlag ! 0/1 flag to either set variable to all NDV values or output
                                                   ! the actual data. This was done because time 0
                                                   ! output does not all contain valid data.
   real :: modelNdv ! NDV value represented within the model code
   real :: modelNdv2 ! Alternative NDV value in NoahMP
   real :: modelNdv3 ! Alternative NDV value in NoahMP
   integer :: modelNdvInt ! NDV value represented in model as integer
   ! Time variable attribues
   character (len=64) :: timeLName ! long_name - usually valid output time
   character (len=64) :: timeUnits ! Usually seconds since 1/1/1970
   character (len=64) :: timeStName ! standard_name - usually time
   integer :: timeValidMin ! the minimum time each configuration can have, time of the first output file
   integer :: timeValidMax ! the maximum time each configuration can have, time of the last output file

   ! Reference time attributes
   character (len=64) :: rTimeLName ! long_name - usually model initialization time
   character (len=64) :: rTimeStName ! standard_name - usually forecast_reference_time
   character (len=64) :: rTimeUnits ! Usually seconds since 1/1/1970
   ! Establish a proj4 string
   character (len=1024) :: proj4
   ! Establish crs-related variables.
   character(len=1024), dimension(20) :: crsCharAttNames,crsFloatAttNames
   character(len=1024), dimension(20) :: crsCharAttVals
   real, dimension(20,20) :: crsRealAttVals
   integer, dimension(20) :: crsRealAttLen
   integer :: nCrsRealAtts, nCrsCharAtts
   ! Establish x/y related variables.
   character(len=1024), dimension(20) :: xCharAttNames,xFloatAttNames
   character(len=1024), dimension(20) :: xCharAttVals
   real, dimension(20,20) :: xRealAttVals
   integer, dimension(20) :: xRealAttLen
   integer :: nxRealAtts, nxCharAtts
   character(len=1024), dimension(20) :: yCharAttNames,yFloatAttNames
   character(len=1024), dimension(20) :: yCharAttVals
   real, dimension(20,20) :: yRealAttVals
   integer, dimension(20) :: yRealAttLen
   integer :: nyRealAtts, nyCharAtts

   ! Global attributes
   character (len=128) :: title ! file title
   character (len=128) :: initTime ! model_initialization_time attribute
   character (len=128) :: validTime ! model_output_valid_time attribute
   character (len=128) :: conventions ! Conventions string
   integer             :: totalValidTime ! # number of valid time (#of output files)

end type ldasMeta

type rtDomainMeta
   ! Variable names
   character (len=64), dimension(numRtDomainVars) :: varNames
   integer :: numVars = numRtDomainVars
   integer :: numSoilLayers                       ! Fill from Namelist
   character (len=512) :: modelOutputType = "terrain_rt"
   character (len=64) :: modelConfigType

   ! Output variable attributes
   real, dimension(numRtDomainVars) :: scaleFactor ! scale_factor values used for each
                                                  ! variable to converte from
                                                  ! real to
                                                  ! integer.
   real, dimension(numRtDomainVars)             :: addOffset   ! add_offset values for each variable.
   character (len=64), dimension(numRtDomainVars) :: longName  ! Long names for each variable.
   character (len=64), dimension(numRtDomainVars) :: units ! Units for each variable.
   character (len=64), dimension(numRtDomainVars) :: coordNames ! Coordinate names for each variable.
   integer(kind=4), dimension(numRtDomainVars) :: validMinComp ! Valid min (after conversion to integer)
   integer(kind=4), dimension(numRtDomainVars) :: validMaxComp ! Valid max (after conversion to integer)
   real*8, dimension(numRtDomainVars) :: validMinDbl ! Valid minimum (before conversion to integer)
   real*8, dimension(numRtDomainVars) :: validMaxDbl ! Valid maximum (before converstion to integer)
   integer(kind=4), dimension(numRtDomainVars) :: missingComp ! Missing value attribute (after conversion to integer)
   real, dimension(numRtDomainVars) :: missingReal ! Missing value attribute (before conversion to integer)
   real, dimension(numRtDomainVars) :: fillReal ! Fill value (before conversion to integer)
   integer(kind=4), dimension(numRtDomainVars) :: fillComp ! Fill value (after conversion to integer)
   integer, dimension(numRtDomainVars) :: outFlag ! 0/1 flag to turn outputting off/on
   integer, dimension(numRtDomainVars) :: timeZeroFlag ! 0/1 flag to either set variable to all NDV values or output
                                                       ! the actual data. This was done because time 0
                                                       ! output does not all contain valid data.
   real :: modelNdv ! NDV value represented within the model code
   ! Time variable attribues
   character (len=64) :: timeLName ! long_name - usually valid output time
   character (len=64) :: timeUnits ! Usually seconds since 1/1/1970
   character (len=64) :: timeStName ! standard_name - usually time
   integer :: timeValidMin ! the minimum time each configuration can have, time of the first output file
   integer :: timeValidMax ! the maximum time each configuration can have, time of the last output file
   ! Reference time attributes
   character (len=64) :: rTimeLName ! long_name - usually model initialization time
   character (len=64) :: rTimeStName ! standard_name - usually forecast_reference_time
   character (len=64) :: rTimeUnits ! Usually seconds since 1/1/1970
   real :: yRes, xRes ! Resoltution in meters

   ! Establish a proj4 string
   character (len=1024) :: proj4
   ! Establish crs-related variables.
   character(len=1024), dimension(20) :: crsCharAttNames,crsFloatAttNames
   character(len=1024), dimension(20) :: crsCharAttVals
   real, dimension(20,20) :: crsRealAttVals
   integer, dimension(20) :: crsRealAttLen
   integer :: nCrsRealAtts, nCrsCharAtts
   ! Establish x/y related variables.
   character(len=1024), dimension(20) :: xCharAttNames,xFloatAttNames
   character(len=1024), dimension(20) :: xCharAttVals
   real, dimension(20,20) :: xRealAttVals
   integer, dimension(20) :: xRealAttLen
   integer :: nxRealAtts, nxCharAtts
   character(len=1024), dimension(20) :: yCharAttNames,yFloatAttNames
   character(len=1024), dimension(20) :: yCharAttVals
   real, dimension(20,20) :: yRealAttVals
   integer, dimension(20) :: yRealAttLen
   integer :: nyRealAtts, nyCharAtts

   ! Global attributes
   integer :: decimation ! Decimation factor
   character (len=128) :: title ! file title
   character (len=128) :: initTime ! model_initialization_time attribute
   character (len=128) :: validTime ! model_output_valid_time attribute
   character (len=128) :: conventions ! Conventions string
   integer             :: totalValidTime ! # number of valid time (#of output files)

end type rtDomainMeta

type lakeMeta
   ! Variable names
   character (len=64), dimension(numLakeVars) :: varNames
   integer :: numVars = numLakeVars
   character (len=512) :: modelOutputType = "reservoir"
   character (len=64) :: modelConfigType

   ! Output variable attributes
   real, dimension(numLakeVars) :: scaleFactor ! scale_factor values used for each
                                               ! variable to converte from
                                               ! real to
                                               ! integer.
   real, dimension(numLakeVars)             :: addOffset   ! add_offset values for each variable.
   character (len=64), dimension(numLakeVars) :: longName  ! Long names for each variable.
   character (len=64), dimension(numLakeVars) :: units ! Units for each variable.
   character (len=64), dimension(numLakeVars) :: coordNames ! Coordinate names for each variable.
   integer(kind=4), dimension(numLakeVars) :: validMinComp ! Valid min (after conversion to integer)
   integer(kind=4), dimension(numLakeVars) :: validMaxComp ! Valid max (after conversion to integer)
   real*8, dimension(numLakeVars) :: validMinDbl ! Valid minimum (before conversion to integer)
   real*8, dimension(numLakeVars) :: validMaxDbl ! Valid maximum (before converstion to integer)
   integer(kind=4), dimension(numLakeVars) :: missingComp ! Missing value attribute (after conversion to integer)
   real, dimension(numLakeVars) :: missingReal ! Missing value attribute (before conversion to integer)
   real, dimension(numLakeVars) :: fillReal ! Fill value (before conversion to integer)
   integer(kind=4), dimension(numLakeVars) :: fillComp ! Fill value (after conversion to integer)
   integer, dimension(numLakeVars) :: outFlag ! 0/1 flag to turn outputting off/on
   integer, dimension(numLakeVars) :: timeZeroFlag ! 0/1 flag to either set variable to all NDV values or output
                                                   ! the actual data. This was done because time 0
                                                   ! output does not all contain valid data.
   real :: modelNdv ! NDV value represented within the model code
   ! Time variable attribues
   character (len=64) :: timeLName ! long_name - usually valid output time
   character (len=64) :: timeUnits ! Usually seconds since 1/1/1970
   character (len=64) :: timeStName ! standard_name - usually time
   integer :: timeValidMin ! the minimum time each configuration can have, time of the first output file
   integer :: timeValidMax ! the maximum time each configuration can have, time of the last output file
   ! Reference time attributes
   character (len=64) :: rTimeLName ! long_name - usually model initialization time
   character (len=64) :: rTimeStName ! standard_name - usually forecast_reference_time
   character (len=64) :: rTimeUnits ! Usually seconds since 1/1/1970
   ! lake_id attributes
   character (len=64) :: lakeIdLName ! long_name - usually Lake COMMON ID
   character (len=256) :: LakeIdComment ! Comment attribute
   ! feature_id attributes
   character (len=256) :: featureIdLName ! long_name - usually lake COMMON ID
   character (len=256) :: featureIdComment ! Comment attribute
   character (len=64) :: cfRole ! cf_role attribute
   ! reservoir_type attributes
   character (len=64) :: reservoirTypeLName ! long_name - usually "reservoir_type"
   integer, dimension(4) :: reservoirTypeFlagValues ! valid flags attribute
   character (len=64) :: reservoirTypeFlagMeanings ! flag meanings attribute
   ! reservoir_assimilated_value attributes
   character (len=64) :: reservoirAssimilatedValueLName ! long_name - usually "reservoir_assimilated_value, m3 s-1"
   character (len=64) :: reservoirAssimilatedValueUnits ! units
   ! reservoir_assimilated_source_file attributes
   character (len=64) :: reservoirAssimilatedSourceFileLName ! long_name - usually "reservoir_assimilated_source_file"
   ! latitude variable attributes
   character (len=64) :: latLName ! long_name
   character (len=64) :: latUnits ! units
   character (len=64) :: latStName ! Standard Name
   ! longitude variable attributes
   character (len=64) :: lonLName ! long_name
   character (len=64) :: lonUnits ! units
   character (len=64) :: lonStName ! Standard Name
   ! elevation variable attributes
   character (len=64) :: elevLName ! long_name
   character (len=64) :: elevUnits ! units
   character (len=64) :: elevStName ! Standard Name
   character (len=256) :: elevComment ! Comment
   ! Global attributes
   character (len=128) :: title ! file title
   character (len=128) :: fType ! featureType attribute
   character (len=128) :: proj4 ! proj4 attribute
   character (len=128) :: initTime ! model_initialization_time attribute
   character (len=128) :: validTime ! model_output_valid_time attribute
   character (len=128) :: lakeDim ! lake_dimension attribute
   character (len=128) :: cdm ! cdm_datatype attribute
   character (len=1024) :: esri ! esri_pe_string attribute
   character (len=128) :: conventions ! Conventions string
   integer             :: totalValidTime ! # number of valid time (#of output files)

end type lakeMeta

type chrtGrdMeta
   ! Variable names
   character (len=64), dimension(numChGrdVars) :: varNames
   integer :: numVars = numChGrdVars
   character (len=512) :: modelOutputType = "channel_rt"
   character (len=64) :: modelConfigType

   ! Output variable attributes
   real, dimension(numChGrdVars) :: scaleFactor ! scale_factor values for each
                                                ! variable to convert from
                                                ! real to integer
   real, dimension(numChGrdVars) :: addOffset ! add_offset values for each variable.
   character (len=64), dimension(numChGrdVars) :: longName ! longname for each variable
   character (len=64), dimension(numChGrdVars) :: units ! Units for each variable.
   character (len=64), dimension(numChGrdVars) :: coordNames ! Coordinate names for each variable.
   integer(kind=4), dimension(numChGrdVars) :: validMinComp ! Valid min (after conversion to integer)
   integer(kind=4), dimension(numChGrdVars) :: validMaxComp ! Valid max (after conversion to integer)
   real*8, dimension(numChGrdVars) :: validMinDbl ! Valid minimum (before conversion to integer)
   real*8, dimension(numChGrdVars) :: validMaxDbl ! Valid maximum (before converstion to integer)
   integer(kind=4), dimension(numChGrdVars) :: missingComp ! Missing value attribute (after conversion to integer)
   real, dimension(numChGrdVars) :: missingReal ! Missing value attribute (before conversion to integer)
   real, dimension(numChGrdVars) :: fillReal ! Fill value (before conversion to integer)
   integer(kind=4), dimension(numChGrdVars) :: fillComp ! Fill value (after conversion to integer)
   integer, dimension(numChGrdVars) :: outFlag ! 0/1 flag to turn outputting off/on
   integer, dimension(numChGrdVars) :: timeZeroFlag ! 0/1 flag to either set variable to all NDV values or output
                                                       ! the actual data. This
                                                       ! was done because time 0
                                                       ! output does not all
                                                       ! contain valid data.
   real :: modelNdv ! NDV value represented within the model code
   ! Time variable attribues
   character (len=64) :: timeLName ! long_name - usually valid output time
   character (len=64) :: timeUnits ! Usually seconds since 1/1/1970
   character (len=64) :: timeStName ! standard_name - usually time
   integer :: timeValidMin ! the minimum time each configuration can have, time of the first output file
   integer :: timeValidMax ! the maximum time each configuration can have, time of the last output file
   ! Reference time attributes
   character (len=64) :: rTimeLName ! long_name - usually model initialization time
   character (len=64) :: rTimeStName ! standard_name - usually forecast_reference_time
   character (len=64) :: rTimeUnits ! Usually seconds since 1/1/1970
   real :: yRes, xRes ! Resoltution in meters

   ! Establish a proj4 string
   character (len=1024) :: proj4
   ! Establish crs-related variables.
   character(len=1024), dimension(20) :: crsCharAttNames,crsFloatAttNames
   character(len=1024), dimension(20) :: crsCharAttVals
   real, dimension(20,20) :: crsRealAttVals
   integer, dimension(20) :: crsRealAttLen
   integer :: nCrsRealAtts, nCrsCharAtts
   ! Establish x/y related variables.
   character(len=1024), dimension(20) :: xCharAttNames,xFloatAttNames
   character(len=1024), dimension(20) :: xCharAttVals
   real, dimension(20,20) :: xRealAttVals
   integer, dimension(20) :: xRealAttLen
   integer :: nxRealAtts, nxCharAtts
   character(len=1024), dimension(20) :: yCharAttNames,yFloatAttNames
   character(len=1024), dimension(20) :: yCharAttVals
   real, dimension(20,20) :: yRealAttVals
   integer, dimension(20) :: yRealAttLen
   integer :: nyRealAtts, nyCharAtts

   ! Global attributes
   integer :: decimation ! Decimation factor
   character (len=128) :: title ! file title
   character (len=128) :: initTime ! model_initialization_time attribute
   character (len=128) :: validTime ! model_output_valid_time attribute
   character (len=128) :: conventions ! Conventions string
   integer             :: totalValidTime ! # number of valid time (#of output files)

end type chrtGrdMeta

type lsmMeta
   ! Variable names
   character (len=64), dimension(numLsmVars) :: varNames
   integer :: numVars = numLsmVars
   integer :: numSnowLayers = 3
   integer :: numSoilLayers                       ! Fill from Namelist
   integer :: act_lev
   character (len=512) :: modelOutputType = "land"
   character (len=64) :: modelConfigType

   ! Output variable attributes
   real, dimension(numLsmVars) :: scaleFactor ! scale_factor values used for each
                                                  ! variable to converte from
                                                  ! real to
                                                  ! integer
   real, dimension(numLsmVars)             :: addOffset   ! add_offset values for each variable.
   character (len=64), dimension(numLsmVars) :: longName  ! Long names for each variable.
   character (len=64), dimension(numLsmVars) :: units ! Units for each variable.
   integer, dimension(numLsmVars) :: numLev ! Number of levels for each variable.
   integer(kind=4), dimension(numLsmVars) :: validMinComp ! Valid min (after conversion to integer)
   integer(kind=4), dimension(numLsmVars) :: validMaxComp ! Valid max (after conversion to integer)
   real*8, dimension(numLsmVars) :: validMinDbl ! Valid minimum (before conversion to integer)
   real*8, dimension(numLsmVars) :: validMaxDbl ! Valid maximum (before converstion to integer)
   integer(kind=4), dimension(numLsmVars) :: missingComp ! Missing value attribute (after conversion to integer)
   real, dimension(numLsmVars) :: missingReal ! Missing value attribute (before conversion to integer)
   real, dimension(numLsmVars) :: fillReal ! Fill value (before conversion to integer)
   integer(kind=4), dimension(numLsmVars) :: fillComp ! Fill value (after conversion to integer)
   integer, dimension(numLsmVars) :: outFlag ! 0/1 flag to turn outputting off/on
   integer, dimension(numLsmVars) :: timeZeroFlag ! 0/1 flag to either set variable to all NDV values or output
                                                   ! the actual data. This was
                                                   ! done because time 0
                                                   ! output does not all contain
                                                   ! valid data.
   real :: modelNdv ! NDV value represented within the model code
   integer :: modelNdvInt ! NDV value represented in model as integer
   ! Time variable attribues
   character (len=64) :: timeLName ! long_name - usually valid output time
   character (len=64) :: timeUnits ! Usually seconds since 1/1/1970
   character (len=64) :: timeStName ! standard_name - usually time
   integer :: timeValidMin ! the minimum time each configuration can have, time of the first output file
   integer :: timeValidMax ! the maximum time each configuration can have, time of the last output file
   ! Reference time attributes
   character (len=64) :: rTimeLName ! long_name - usually model initialization time
   character (len=64) :: rTimeStName ! standard_name - usually forecast_reference_time
   character (len=64) :: rTimeUnits ! Usually seconds since 1/1/1970

   ! Establish a proj4 string
   character (len=1024) :: proj4
   ! Establish crs-related variables.
   character(len=1024), dimension(20) :: crsCharAttNames,crsFloatAttNames
   character(len=1024), dimension(20) :: crsCharAttVals
   real, dimension(20,20) :: crsRealAttVals
   integer, dimension(20) :: crsRealAttLen
   integer :: nCrsRealAtts, nCrsCharAtts
   ! Establish x/y related variables.
   character(len=1024), dimension(20) :: xCharAttNames,xFloatAttNames
   character(len=1024), dimension(20) :: xCharAttVals
   real, dimension(20,20) :: xRealAttVals
   integer, dimension(20) :: xRealAttLen
   integer :: nxRealAtts, nxCharAtts
   character(len=1024), dimension(20) :: yCharAttNames,yFloatAttNames
   character(len=1024), dimension(20) :: yCharAttVals
   real, dimension(20,20) :: yRealAttVals
   integer, dimension(20) :: yRealAttLen
   integer :: nyRealAtts, nyCharAtts

   ! Global attributes
   character (len=128) :: title ! file title
   character (len=128) :: initTime ! model_initialization_time attribute
   character (len=128) :: validTime ! model_output_valid_time attribute
   character (len=128) :: conventions ! Conventions string
   integer             :: totalValidTime ! # number of valid time (#of output files)

end type lsmMeta

type chObsMeta
   ! Variable names
   character (len=64), dimension(numChObsVars) :: varNames
   integer :: numVars = numChObsVars
   character (len=512) :: modelOutputType = "channel_rt"
   character (len=64) :: modelConfigType

   ! Output variable attributes
   real, dimension(numChObsVars) :: scaleFactor ! scale_factor values used for
                                                 ! each variable to convert
                                                 ! from real to integer
   real, dimension(numChObsVars)             :: addOffset   ! add_offset values for each variable.
   character (len=64), dimension(numChObsVars) :: longName  ! Long names for each variable.
   character (len=64), dimension(numChObsVars) :: units ! Units for each variable.
   character (len=64), dimension(numChObsVars) :: coordNames ! Coordinate names for each variable.
   integer(kind=4), dimension(numChObsVars) :: validMinComp ! Valid min (after conversion to integer)
   integer(kind=4), dimension(numChObsVars) :: validMaxComp ! Valid max (after conversion to integer)
   real*8, dimension(numChObsVars) :: validMinDbl ! Valid minimum (before conversion to integer)
   real*8, dimension(numChObsVars) :: validMaxDbl ! Valid maximum (before converstion to integer)
   integer(kind=4), dimension(numChObsVars) :: missingComp ! Missing value attribute (after conversion to integer)
   real, dimension(numChObsVars) :: missingReal ! Missing value attribute (before conversion to integer)
   real, dimension(numChObsVars) :: fillReal ! Fill value (before conversion to integer)
   integer(kind=4), dimension(numChObsVars) :: fillComp ! Fill value (after conversion to integer)
   integer, dimension(numChObsVars) :: outFlag ! 0/1 flag to turn outputting off/on
   integer, dimension(numChObsVars) :: timeZeroFlag ! 0/1 flag to either set variable to all NDV values or output
                                                 ! the actual data. This was
                                                 ! done because time 0
                                                 ! output does not all contain valid data.
   real :: modelNdv ! NDV value represented within the model code
   ! Time variable attribues
   character (len=64) :: timeLName ! long_name - usually valid output time
   character (len=64) :: timeUnits ! Usually seconds since 1/1/1970
   character (len=64) :: timeStName ! standard_name - usually time
   integer :: timeValidMin ! the minimum time each configuration can have, time of the first output file
   integer :: timeValidMax ! the maximum time each configuration can have, time of the last output file
   ! Reference time attributes
   character (len=64) :: rTimeLName ! long_name - usually model initialization time
   character (len=64) :: rTimeStName ! standard_name - usually forecast_reference_time
   character (len=64) :: rTimeUnits ! Usually seconds since 1/1/1970
   ! feature_id attributes
   character (len=256) :: featureIdLName ! long_name - usually Reach ID
   character (len=256) :: featureIdComment ! Comment attribute
   character (len=64) :: cfRole ! cf_role attribute
   ! latitude variable attributes
   character (len=64) :: latLName ! long_name
   character (len=64) :: latUnits ! units
   character (len=64) :: latStName ! Standard Name
   ! longitude variable attributes
   character (len=64) :: lonLName ! long_name
   character (len=64) :: lonUnits ! units
   character (len=64) :: lonStName ! Standard Name
   ! Elevation variable attributes
   character (len=64) :: elevLName ! long_name
   character (len=64) :: elevUnits ! units
   character (len=64) :: elevStName ! Standard Name
   ! Order variable attributes
   character (len=64) :: orderLName ! long_name
   character (len=64) :: orderStName ! Standard Name
   ! Global attributes
   character (len=128) :: title ! file title
   character (len=128) :: fType ! featureType attribute
   character (len=128) :: proj4 ! proj4 attribute
   character (len=128) :: initTime ! model_initialization_time attribute
   character (len=128) :: validTime ! model_output_valid_time attribute
   character (len=128) :: stDim ! station_dimension attribute
   integer :: stOrder ! stream_order_output attribute
   character (len=128) :: cdm ! cdm_datatype attribute
   character (len=1024) :: esri ! esri_pe_string attribute
   character (len=128) :: conventions ! Conventions string
   integer             :: totalValidTime ! # number of valid time (#of output files)

end type chObsMeta

type gwMeta
   ! Variable names
   character (len=64), dimension(numGwVars) :: varNames
   integer :: numVars = numGwVars
   character (len=512) :: modelOutputType = "groundwater_rt"
   character (len=64) :: modelConfigType

   ! Output variable attributes
   real, dimension(numGwVars) :: scaleFactor ! scale_factor values used for each
                                               ! variable to converte from
                                               ! real to
                                               ! integer.
   real, dimension(numGwVars)             :: addOffset   ! add_offset values for each variable.
   character (len=64), dimension(numGwVars) :: longName  ! Long names for each variable.
   character (len=64), dimension(numGwVars) :: units ! Units for each variable.
   !character (len=64), dimension(numGwVars) :: coordNames ! Coordinate names for each variable.
   integer(kind=4), dimension(numGwVars) :: validMinComp ! Valid min (after conversion to integer)
   integer(kind=4), dimension(numGwVars) :: validMaxComp ! Valid max (after conversion to integer)
   real*8, dimension(numGwVars) :: validMinDbl ! Valid minimum (before conversion to integer)
   real*8, dimension(numGwVars) :: validMaxDbl ! Valid maximum (before converstion to integer)
   integer(kind=4), dimension(numGwVars) :: missingComp ! Missing value attribute (after conversion to integer)
   real, dimension(numGwVars) :: missingReal ! Missing value attribute (before conversion to integer)
   real, dimension(numGwVars) :: fillReal ! Fill value (before conversion to integer)
   integer(kind=4), dimension(numGwVars) :: fillComp ! Fill value (after conversion to integer)
   integer, dimension(numGwVars) :: outFlag ! 0/1 flag to turn outputting off/on
   integer, dimension(numGwVars) :: timeZeroFlag ! 0/1 flag to either set variable to all NDV values or output
                                                   ! the actual data. This was
                                                   ! done because time 0
                                                   ! output does not all contain
                                                   ! valid data.
   real :: modelNdv ! NDV value represented within the model code
   ! Time variable attribues
   character (len=64) :: timeLName ! long_name - usually valid output time
   character (len=64) :: timeUnits ! Usually seconds since 1/1/1970
   character (len=64) :: timeStName ! standard_name - usually time
   integer :: timeValidMin ! the minimum time each configuration can have, time of the first output file
   integer :: timeValidMax ! the maximum time each configuration can have, time of the last output file
   ! Reference time attributes
   character (len=64) :: rTimeLName ! long_name - usually model initialization time
   character (len=64) :: rTimeStName ! standard_name - usually forecast_reference_time
   character (len=64) :: rTimeUnits ! Usually seconds since 1/1/1970
   ! gw_id attributes
   character (len=64) :: gwIdLName ! long_name - usually gw bucket ID
   character (len=256) :: gwIdComment ! Comment attribute
   ! feature_id attributes
   character (len=256) :: featureIdLName ! long_name - usually gw bucket ID
   character (len=256) :: featureIdComment ! Comment attribute
   character (len=64) :: cfRole ! cf_role attribute
   ! latitude variable attributes
   !character (len=64) :: latLName ! long_name
   !character (len=64) :: latUnits ! units
   !character (len=64) :: latStName ! Standard Name
   ! longitude variable attributes
   !character (len=64) :: lonLName ! long_name
   !character (len=64) :: lonUnits ! units
   !character (len=64) :: lonStName ! Standard Name
   ! elevation variable attributes
   !character (len=64) :: elevLName ! long_name
   !character (len=64) :: elevUnits ! units
   !character (len=64) :: elevStName ! Standard Name
   ! Global attributes
   character (len=128) :: title ! file title
   character (len=128) :: fType ! featureType attribute
   !character (len=128) :: proj4 ! proj4 attribute
   character (len=128) :: initTime ! model_initialization_time attribute
   character (len=128) :: validTime ! model_output_valid_time attribute
   character (len=128) :: gwDim ! gw_dimension attribute
   !character (len=128) :: cdm ! cdm_datatype attribute
   !character (len=1024) :: esri ! esri_pe_string attribute
   character (len=128) :: conventions ! Conventions string
   integer             :: totalValidTime ! # number of valid time (#of output files)

end type gwMeta

contains

subroutine initChrtDict(chrtOutDict,diagFlag,procId)
  use config_base, only: nlst
  use netcdf
  implicit none

   type(chrtMeta), intent(inout) :: chrtOutDict
   integer, intent(inout) :: diagFlag
   integer, intent(inout) :: procId

   ! Local variables
   integer :: ftnMeta,iret
   integer :: projVarId

   chrtOutDict%modelNdv = -9.E15
   ! CHRTOUT FILES

   ! Pull spatial metadata information about the modeling domain from the land
   ! spatial metadata file.
   if(procId .eq. 0) then
      iret = nf90_open(trim(nlst(1)%land_spatial_meta_flnm),NF90_NOWRITE,ncid=ftnMeta)
      if(iret .ne. 0) then
         ! Spatial metadata file not found for land grid.
         call postDiagMsg(diagFlag,'WARNING: Unable to open LAND spatial metadata file.')
         chrtOutDict%proj4 = ''
         chrtOutDict%esri = ''
      else
         ! First pull metadata on coordinate system.
         iret = nf90_inq_varid(ftnMeta,'crs',projVarId)
         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: Unable to find crs in LAND spatial metadata file')
            chrtOutDict%proj4 = ''
            chrtOutDict%esri = ''
         else
            iret = nf90_get_att(ftnMeta,projVarId,'esri_pe_string',chrtOutDict%esri)
            if(iret .ne. 0) then
               call postDiagMsg(diagFlag,'WARNING: Unable to find esri_pe_string in LAND spatial metadata file.')
               chrtOutDict%esri = ''
            endif
            iret = nf90_get_att(ftnMeta,NF90_GLOBAL,'proj4',chrtOutDict%proj4)
            ! We are going to put a relaxed constraint on the proj4 string.
            if(iret .ne. 0) then
               call postDiagMsg(diagFlag,'WARNING: proj4 string not found. Defaulting to blank string.')
               chrtOutDict%proj4 = ''
            endif
         endif
         ! Close the file
         iret = nf90_close(ftnMeta)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close LAND spatial metadata file.')
      endif
   endif

   ! NOTE !!!!! If you see PLC, this means OWP has no desire to output these,
   !            which means meta-data standards have yet to be determined
   !            for these variables. Fill in if it's desired to output....
   ! First establish global attributes for the channel output files
   chrtOutDict%title = "OUTPUT FROM " // trim(get_model_version())
   chrtOutDict%fType = 'timeSeries'
   chrtOutDict%initTime = '1970-01-01_00:00:00' ! This will be calculated in I/O code
   chrtOutDict%validTime = '1970-01-01_00:00:00' ! This will be calculated in I/O code
   chrtOutDict%stDim = 'feature_id'
   chrtOutDict%stOrder = 1
   chrtOutDict%cdm = 'Station'
   chrtOutDict%conventions = 'CF-1.6'

   ! Next establish time attribues
   chrtOutDict%timeLName = 'valid output time'
   chrtOutDict%timeUnits = 'minutes since 1970-01-01 00:00:00 UTC'
   chrtOutDict%timeStName = 'time'
   chrtOutDict%rTimeLName = 'model initialization time'
   chrtOutDict%rTimeStName = 'forecast_reference_time'
   chrtOutDict%rTimeUnits = 'minutes since 1970-01-01 00:00:00 UTC'

   ! Esatablish lat/lon attributes
   chrtOutDict%latLName = "Feature latitude"
   chrtOutDict%latUnits = "degrees_north"
   chrtOutDict%latStName = "latitude"
   chrtOutDict%lonLName = "Feature longitude"
   chrtOutDict%lonUnits = "degrees_east"
   chrtOutDict%lonStName = "longitude"

   ! Establish streamflw order attributes
   chrtOutDict%orderLName = "Streamflow Order"
   chrtOutDict%orderStName = "order"

   ! Establish point elevation attributes
   chrtOutDict%elevLName = "Feature Elevation"
   chrtOutDict%elevUnits = "meters"
   chrtOutDict%elevStName = "Elevation"

   ! Next establish feature_id attributes. Given this is merged community, we
   chrtOutDict%featureIdLName = 'Reach ID'
   chrtOutDict%featureIdComment = 'NHDPlusv2 ComIDs within CONUS, arbitrary Reach IDs outside of CONUS'
   chrtOutDict%cfRole = 'timeseries_id'

   ! Now establish attributes for output variables.
   !chrtOutDict%varNames(:) = (/"streamflow","nudge","q_lateral","velocity",&
   !                            "Head","qSfcLatRunoff","qBucket",&
   !                            "qBtmVertRunoff","AccSfcLatRunoff","accBucket"/)
   chrtOutDict%varNames(:) = [character(len=64) :: "streamflow","nudge","q_lateral","velocity",&
                              "Head","qSfcLatRunoff","qBucket",&
                              "qBtmVertRunoff","AccSfcLatRunoff","accBucket","qloss"]
   chrtOutDict%longName(:) = [character(len=64) :: "River Flow","Amount of stream flow alteration",&
                              "Runoff into channel reach","River Velocity",&
                              "River Stage","Runoff from terrain routing",&
                              "Flux from gw bucket",&
                              "Runoff from bottom of soil to bucket",&
                              "Accumulated runoff from terrain routing",&
                              "Accumulated runoff from gw bucket",&
                              "Channel Infiltration"]
   chrtOutDict%units(:) = [character(len=64) :: "m3 s-1","m3 s-1","m3 s-1","m s-1",&
                                                "meter","m3 s-1","m3 s-1",&
                                                "m3","m3","m3","m3 s-1"]
   chrtOutDict%coordNames(:) = [character(len=64) :: "latitude longitude","latitude longitude",&
                                 "latitude longitude","latitude longitude",&
                                 "latitude longitude","latitude longitude",&
                                 "latitude longitude","latitude longitude",&
                                 "latitude longitude","latitude longitude",&
                                 "latitude longitude"]
   chrtOutDict%scaleFactor(:) = [0.01,0.01,0.1,0.01,0.01,0.00001,0.00001,0.001,&
                                 0.01,0.01,0.1]
   chrtOutDict%addOffset(:) = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                               0.0,0.0]
   ! Initialize all output flags to 0. Modify (if absolutely necessary) in the
   ! output subroutine.
   chrtOutDict%outFlag(:) = [0,0,0,0,0,0,0,0,0,0,0]
   chrtOutDict%timeZeroFlag(:) = [1,1,1,1,1,1,1,1,1,1,1]
   chrtOutDict%fillReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0]
   chrtOutDict%missingReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                                 -9999.0]
   chrtOutDict%validMinDbl(:) = [0.0d0, -50000.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
                                  0.0d0, 0.0d0, 0.0d0]
   chrtOutDict%validMaxDbl(:) = [50000.0d0, 50000.0d0, 50000.0d0, 50000.0d0, 50000.0d0, &
                                  20000.0d0, 20000.0d0, 20000.0d0, 50000.0d0, &
                                  50000.0d0, 50000.0d0]
   ! Loop through and calculate missing/fill/min/max values that will be placed
   ! into the NetCDF attributes after scale_factor/add_offset are applied.
   do i=1,numChVars
      chrtOutDict%fillComp(i) = &
           nint((chrtOutDict%fillReal(i)     + chrtOutDict%addOffset(i)), 8) * &
           nint(one_dbl / chrtOutDict%scaleFactor(i), 8)
      chrtOutDict%missingComp(i) = &
           nint((chrtOutDict%missingReal(i)  + chrtOutDict%addOffset(i)), 8) * &
           nint(one_dbl / chrtOutDict%scaleFactor(i), 8)
      chrtOutDict%validMinComp(i) = &
           nint((chrtOutDict%validMinDbl(i) + chrtOutDict%addOffset(i)) * &
                nint(one_dbl / chrtOutDict%scaleFactor(i), 8), 8)
      chrtOutDict%validMaxComp(i) = &
           nint((chrtOutDict%validMaxDbl(i) + chrtOutDict%addOffset(i)) * &
                nint(one_dbl / chrtOutDict%scaleFactor(i), 8), 8)
   end do
end subroutine initChrtDict

subroutine initLdasDict(ldasOutDict,procId,diagFlag)
  use config_base, only: nlst
  use netcdf
  implicit none

   type(ldasMeta), intent(inout) :: ldasOutDict
   integer, intent(inout) :: procId
   integer, intent(inout) :: diagFlag
   integer :: ftnMeta,projVarId,xVarId,yVarId
   integer :: iret
   integer :: crsRealAttCnt,xRealAttCnt,yRealAttCnt
   integer :: crsCharAttCnt,xCharAttCnt,yCharAttCnt
   integer :: i, nCrsAtts,nxAtts,nyAtts
   integer :: charFlag
   integer :: floatFlag
   character(len=512) :: tmpAttName
   integer :: xtypeTmp
   integer :: tmpLen

   ! LDASOUT FILES

   ldasOutDict%numSoilLayers = nlst(1)%nsoil
   ldasOutDict%act_lev = nlst(1)%act_lev


   ldasOutDict%modelNdv = 9.9692099683868690E36
   ldasOutDict%modelNdv2 = -1.E33
   ldasOutDict%modelNdv3 = -1.E36
   ldasOutDict%modelNdvInt = -2147483647

   ! First establish global attributes.
   ldasOutDict%title = "OUTPUT FROM " // trim(get_model_version())
   ldasOutDict%initTime = "1970-01-01_00:00:00" ! This will be calculated in I/O code.
   ldasOutDict%validTime = "1970-01-01_00:00:00" ! This will be calculated in I/O code.
   ldasOutDict%conventions = "CF-1.6"

   ! Next establish time attributes
   ldasOutDict%timeLName = "valid output time"
   ldasOutDict%timeUnits = "minutes since 1970-01-01 00:00:00 UTC"
   ldasOutDict%timeStName = "time"
   ldasOutDict%rTimeLName = "model initialization time"
   ldasOutDict%rTimeUnits = "minutes since 1970-01-01 00:00:00 UTC"
   ldasOutDict%rTimeStName = "forecast_reference_time"

   crsRealAttCnt = 0
   crsCharAttCnt = 0
   xRealAttCnt = 0
   xCharAttCnt = 0
   yRealAttCnt = 0
   yCharAttCnt = 0

   ! Pull spatial metadata information about the modeling domain from the land
   ! spatial metadata file.
   if(procId .eq. 0) then
      iret = nf90_open(trim(nlst(1)%land_spatial_meta_flnm),NF90_NOWRITE,ncid=ftnMeta)
      if(iret .ne. 0) then
         ! Spatial metadata file not found for land grid.
         call postDiagMsg(diagFlag,'WARNING: Unable to open LAND spatial metadata file. No crs variable or attributes will be created.')
         ldasOutDict%nCrsCharAtts = 0
         ldasOutDict%nCrsRealAtts = 0
         ldasOutDict%nxCharAtts = 0
         ldasOutDict%nxRealAtts = 0
         ldasOutDict%nyCharAtts = 0
         ldasOutDict%nyRealAtts = 0
         ldasOutDict%proj4 = ''
      else
         iret = nf90_get_att(ftnMeta,NF90_GLOBAL,'proj4',ldasOutDict%proj4)
         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: Unable to find proj4 global attribute. Defaulting to blank string.')
            ldasOutDict%proj4 = ''
         endif
         charFlag = 0
         floatFlag = 0
         ! Find the crs variable and pull out the attributes, their names, and
         ! their values. This will be translated to output files.
         iret = nf90_inq_varid(ftnMeta,'crs',projVarId)
         ! For now we are going to allow the code to move forward without
         ! finding this variable. In the future, we will probably restrict the
         ! code to ensure things are more seamless.
         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: Unable to locate the crs variable. No crs variable or attributes will be created.')
            ldasOutDict%nCrsCharAtts = 0
            ldasOutDict%nCrsRealAtts = 0
         else
            iret = nf90_inquire_variable(ftnMeta,projVarId,nAtts=nCrsAtts)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find crs number of attributes')
            do i =1,nCrsAtts
               iret = nf90_inq_attname(ftnMeta,projVarId,i,name=tmpAttName)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to extract attribute names from crs variable.')
               iret = nf90_inquire_attribute(ftnMeta,projVarId,trim(tmpAttName),xtype=xtypeTmp,len=tmpLen)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to find attribute types from crs variable.')
               select case (xtypeTmp)
                  case (NF90_FLOAT)
                     floatFlag = 1
                  case (NF90_CHAR)
                     charFlag = 1
                  case (NF90_SHORT)
                     floatFlag = 1
                  case (NF90_USHORT)
                     floatFlag = 1
                  case (NF90_INT)
                     floatFlag = 1
                  case (NF90_UINT)
                     floatFlag = 1
                  case (NF90_INT64)
                     floatFlag = 1
                  case (NF90_UINT64)
                     floatFlag = 1
                  case (NF90_DOUBLE)
                     floatFlag = 1
                  case (NF90_STRING)
                     charFlag = 1
               end select
               if(charFlag .eq. 1) then
                  ldasOutDict%crsCharAttNames(crsCharAttCnt+1) = trim(tmpAttName)
                  iret = nf90_get_att(ftnMeta,projVarId,trim(tmpAttName),ldasOutDict%crsCharAttVals(crsCharAttCnt+1))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull crs attributes')
                  crsCharAttCnt = crsCharAttCnt + 1
               else
                  ldasOutDict%crsFloatAttNames(crsRealAttCnt+1) = trim(tmpAttName)
                  ldasOutDict%crsRealAttLen(crsRealAttCnt+1) = tmpLen
                  iret = nf90_get_att(ftnMeta,projVarId,trim(tmpAttName),ldasOutDict%crsRealAttVals(crsRealAttCnt+1,1:tmpLen))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull crs attributes')
                  crsRealAttCnt = crsRealAttCnt + 1
               endif
               charFlag = 0
               floatFlag = 0
            end do
            ldasOutDict%nCrsRealAtts = crsRealAttCnt
            ldasOutDict%nCrsCharAtts = crsCharAttCnt

         endif

         ! Next pull the attributes from the x/y dimensions
         charFlag = 0
         floatFlag = 0
         iret = nf90_inq_varid(ftnMeta,'x',xVarId)
         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: Unable to locate the x variable. No x variable or attributes will be created.')
            ldasOutDict%nxCharAtts = 0
            ldasOutDict%nxRealAtts = 0
         else
            iret = nf90_inquire_variable(ftnMeta,xVarId,nAtts=nxAtts)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find x number of attributes')
            do i =1,nxAtts
               iret = nf90_inq_attname(ftnMeta,xVarId,i,name=tmpAttName)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to extract attribute names from x variable.')
               iret = nf90_inquire_attribute(ftnMeta,xVarId,trim(tmpAttName),xtype=xtypeTmp,len=tmpLen)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to find attribute types from x variable.')
               select case (xtypeTmp)
                  case (NF90_FLOAT)
                     floatFlag = 1
                  case (NF90_CHAR)
                     charFlag = 1
                  case (NF90_SHORT)
                     floatFlag = 1
                  case (NF90_USHORT)
                     floatFlag = 1
                  case (NF90_INT)
                     floatFlag = 1
                  case (NF90_UINT)
                     floatFlag = 1
                  case (NF90_INT64)
                     floatFlag = 1
                  case (NF90_UINT64)
                     floatFlag = 1
                  case (NF90_DOUBLE)
                     floatFlag = 1
                  case (NF90_STRING)
                     charFlag = 1
               end select
               if(charFlag .eq. 1) then
                  ldasOutDict%xCharAttNames(xCharAttCnt+1) = trim(tmpAttName)
                  iret = nf90_get_att(ftnMeta,xVarId,trim(tmpAttName),ldasOutDict%xCharAttVals(xCharAttCnt+1))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull x attributes')
                  xCharAttCnt = xCharAttCnt + 1
               else
                  ldasOutDict%xFloatAttNames(xRealAttCnt+1) = trim(tmpAttName)
                  ldasOutDict%xRealAttLen(xRealAttCnt+1) = tmpLen
                  iret = nf90_get_att(ftnMeta,xVarId,trim(tmpAttName),ldasOutDict%xRealAttVals(xRealAttCnt+1,1:tmpLen))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull x attributes')
                  xRealAttCnt = xRealAttCnt + 1
               endif
               charFlag = 0
               floatFlag = 0
            end do
            ldasOutDict%nxRealAtts = xRealAttCnt
            ldasOutDict%nxCharAtts = xCharAttCnt

         endif

         charFlag = 0
         floatFlag = 0
         iret = nf90_inq_varid(ftnMeta,'y',yVarId)
         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: Unable to locate the y variable. No y variable or attributes will be created.')
            ldasOutDict%nyCharAtts = 0
            ldasOutDict%nyRealAtts = 0
         else
            iret = nf90_inquire_variable(ftnMeta,yVarId,nAtts=nyAtts)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find y number of attributes')
            do i =1,nyAtts
               iret = nf90_inq_attname(ftnMeta,yVarId,i,name=tmpAttName)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to extract attribute names from y variable.')
               iret = nf90_inquire_attribute(ftnMeta,yVarId,trim(tmpAttName),xtype=xtypeTmp,len=tmpLen)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to find attribute types from y variable.')
               select case (xtypeTmp)
                  case (NF90_FLOAT)
                     floatFlag = 1
                  case (NF90_CHAR)
                     charFlag = 1
                  case (NF90_SHORT)
                     floatFlag = 1
                  case (NF90_USHORT)
                     floatFlag = 1
                  case (NF90_INT)
                     floatFlag = 1
                  case (NF90_UINT)
                     floatFlag = 1
                  case (NF90_INT64)
                     floatFlag = 1
                  case (NF90_UINT64)
                     floatFlag = 1
                  case (NF90_DOUBLE)
                     floatFlag = 1
                  case (NF90_STRING)
                     charFlag = 1
               end select
               if(charFlag .eq. 1) then
                  ldasOutDict%yCharAttNames(yCharAttCnt+1) = trim(tmpAttName)
                  iret = nf90_get_att(ftnMeta,yVarId,trim(tmpAttName),ldasOutDict%yCharAttVals(yCharAttCnt+1))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull y attributes')
                  yCharAttCnt = yCharAttCnt + 1
               else
                  ldasOutDict%yFloatAttNames(yRealAttCnt+1) = trim(tmpAttName)
                  ldasOutDict%yRealAttLen(yRealAttCnt+1) = tmpLen
                  iret = nf90_get_att(ftnMeta,yVarId,trim(tmpAttName),ldasOutDict%yRealAttVals(yRealAttCnt+1,1:tmpLen))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull y attributes')
                  yRealAttCnt = yRealAttCnt + 1
               endif
               charFlag = 0
               floatFlag = 0
            end do
            ldasOutDict%nyRealAtts = yRealAttCnt
            ldasOutDict%nyCharAtts = yCharAttCnt

         endif
         ! Close the file
         iret = nf90_close(ftnMeta)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close LAND spatial metadata file.')
      endif
   endif

   ! Now establish metadata attributes for variables. ESRI string is defined
   ! above and applies to all gridded variables for LDASOUT.
   ldasOutDict%varNames(:) = [character(len=64) :: &
                               "IVGTYP","ISLTYP","FVEG","LAI","SAI", &                   !1-5
                               "SWFORC","COSZ","LWFORC","RAINRATE","EMISS", &            !6-10
                               "FSA","FIRA","GRDFLX","HFX","LH", &                       !11-15
                               "ECAN","EDIR","ALBEDO","ETRAN","UGDRNOFF", &              !16-20
                               "SFCRNOFF","CANLIQ","CANICE","ZWT","WA", &                !21-25
                               "WT","ACCPRCP","ACCECAN","ACCEDIR","ACCETRAN", &          !26-30
                               "SAV","TR","EVC","IRC","SHC", &                           !31-35
                               "IRG","SHG","EVG","GHV","SAG", &                          !36-40
                               "IRB","SHB","EVB","GHB","TRAD", &                         !41-45
                               "TG","TV","TAH","TGV","TGB", &                            !46-50
                               "T2MV","T2MB","Q2MV","Q2MB","EAH", &                      !51-55
                               "FWET","ZSNSO_SN","SNICE","SNLIQ","SOIL_T", &             !56-60
                               "SOIL_W","SNOW_T","SOIL_M","SNOWH","SNEQV", &             !60-65
                               "QSNOW","ISNOW","FSNO","ACSNOW","ACSNOM", &               !66-70
                               "CM","CH","CHV","CHB","CHLEAF", &                         !71-75
                               "CHUC","CHV2","CHB2","LFMASS","RTMASS", &                 !76-80
                               "STMASS","WOOD","STBLCP","FASTCP","NEE", &                !81-85
                               "GPP","NPP","PSN","APAR","ACCET", &                       !86-90
                               "CANWAT","SOILICE","SOILSAT_TOP","SOILSAT","SNOWT_AVG", & !91-95
                               "ALBSND","ALBSNI","QRAIN",&                               !96-98
                               "glacier", "glacier_thickness" ,"PSNOWALB",&              !99-101
                               "PSNOWTHRUFAL" ,"PSNOWHEIGHT","PSNOWTOTSWE" ,&            !102-104
                               "PSNOWGRAN1","PSNOWGRAN2","PSNOWAGE",&                    !105-107
                               "PSNOWTEMP","PSNOWDZ","PSNOWHIST",&                       !108-110
                               "PSNOWLIQ","PSNOWHEAT","PSNOWRHO",&                       !111-113
                               "PSNOWSWE", "FLOW_ICE", "FLOW_SNOW"]                      !114-116

  ldasOutDict%longName(:) = [character(len=128) :: &
                              "Dominant vegetation category",&          !1
                              "Dominant soil category",&                !2
                              "Green Vegetation Fraction",&             !3
                              "Leaf area index",&                       !4
                              "Stem area index",&                       !5
                              "Shortwave forcing",&                     !6
                              "Cosine of zenith angle",&                !7
                              "Longwave forcing",&                      !8
                              "Precipitation rate",&                    !9
                              "Grid emissivity",&                       !10
                              "Total absorbed SW radiation",&           !11
                              "Total net LW radiation to atmosphere",&  !12
                              "Heat flux into the soil",&               !13
                              "Total sensible heat to the atmosphere",& !14
                              "Total latent heat to the atmosphere",&   !15
                              "Canopy water evaporation rate",&         !16
                              "Direct from soil evaporation rate",&     !17
                              "Surface albedo",&                        !18
                              "Transpiration rate",&                    !19
                              "Accumulated underground runoff",&        !20
                              "Accumulated surface runoff",&            !21
                              "Canopy liquid water content",&           !22
                              "Canopy ice water content",&              !23
                              "Depth to water table",&                  !24
                              "Water in aquifer",&                      !25
                              "Water in aquifer and saturated soil",&   !26
                              "Accumulated precip",&                    !27
                              "Accumulated canopy evap",&               !28
                              "Accumulated direct soil evap",&          !29
                              "Accumulated transpiration",&             !30
                              "Solar radiative heat flux absorbed by vegetation",& !31
                              "Transpiration heat",&                    !32
                              "Canopy evap heat",&                      !33
                              "Canopy net LW rad",&                     !34
                              "Canopy sensible heat",&                  !35
                              "Ground net LW rad",&                     !36
                              "Ground sensible heat",&                  !37
                              "Ground evap heat",&                      !38
                              "Ground heat flux + to soil vegetated",&  !39
                              "Solar radiative heat flux absorbed by ground",& !40
                              "Net LW rad to atm bare",&                !41
                              "Sensible heat atm bare",&                !42
                              "Evaporation heat to atm bare",&          !43
                              "Ground heat flux + to soil bare",&       !44
                              "Surface radiative temperature",&         !45
                              "Ground temperature",&                    !46
                              "Vegetation temperature",&                !47
                              "Canopy air temperature",&                !48
                              "Ground surface Temp vegetated",&         !49
                              "Ground surface Temp bare",&              !50
                              "2m Air Temp vegetated",&                 !51
                              "2m Air Temp bare",&                      !52
                              "2m mixing ratio vegetated",&             !53
                              "2m mixing ratio bare",&                  !54
                              "Canopy air vapor pressure",&             !55
                              "Wetted or snowed fraction of canopy",&   !56
                              "Snow layer depths from snow surface",&   !57
                              "Snow layer ice",&                        !58
                              "Snow layer liquid water",&               !59
                              "soil temperature",&                      !60
                              "liquid volumetric soil moisture",&       !61
                              "snow temperature",&                      !62
                              "volumetric soil moisture, the dimensionless ratio of water volume (m3) to soil volume (m3)",& !63
                              "Snow depth",&                            !64
                              "Snow water equivalent",&                 !65
                              "Snowfall rate on the ground",&           !66
                              "Number of snow layers",&                 !67
                              "Snow-cover fraction on the ground",&     !68
                              "accumulated snow fall",&                 !69
                              "accumulated melting water out of snow bottom",& !70
                              "Momentum drag coefficient",&             !71
                              "Sensible heat exchange coefficient",&    !72
                              "Exchange coefficient vegetated",&        !73
                              "Exchange coefficient bare",&             !74
                              "Exchange coefficient leaf",&             !75
                              "Exchange coefficient bare",&             !76
                              "Exchange coefficient 2-meter vegetated",&!77
                              "Exchange coefficient 2-meter bare",&     !78
                              "Leaf mass",&                             !79
                              "Mass of fine roots",&                    !80
                              "Stem mass",&                             !81
                              "Mass of wood and woody roots",&          !82
                              "Stable carbon in deep soil",&            !83
                              "Short-lived carbon in shallow soil",&    !84
                              "Net ecosystem exchange",&                !85
                              "Net instantaneous assimilation",&        !86
                              "Net primary productivity",&              !87
                              "Total photosynthesis",&                  !88
                              "Photosynthesis active energy by canopy",&!89
                              "Accumulated total ET",&                  !90
                              "Total canopy water (liquid + ice)",&     !91
                              "fraction of soil moisture that is ice",& !92
                              "fraction of soil saturation, top 2 layers",& !93
                              "fraction of soil saturation, column integrated",& !94
                              "average snow temperature (by layer mass)",& !95
                              "snowpack albedo, direct",&               !96
                              "snowpack albedo, diffuse",&              !97
                              "Rainfall rate on the ground",&           !98
                              "Glacier grid point",&                    !99
                              "Glacier height",&                        !100
                              "Snow albedo",&                           !101
                              "Runoff from glacier",&                   !102
                              "Total snow height",&                     !103
                              "Total snow swe", &                       !104
                               "Snow gran 1, optiacal diameter",&       !105
                               "Snow gran 2, sphericity",&              !106
                               "Snow age",&                             !107
                               "Snow temperature",&                     !108
                               "Snow thickness",&                       !109
                               "Snow history",&                         !110
                               "Liquid in snow",&                       !111
                               "Snow heat",&                            !112
                               "Snow density",&                         !113
                               "Snow water equivalent", &               !114
                               "Accumulated glacier melt from ice", &   !115
                               "Accumulated glacier melt from snow"]    !116

   ldasOutDict%units(:) = [character(len=64) :: &
                            "category","category","-","-","-", &               !1-5
                            "W m-2","-","W m-2","kg m-2 s-1","-", &            !6-10
                            "W m-2","W m-2","W m-2","W m-2","W m-2", &         !11-15
                            "kg m-2 s-1","kg m-2 s-1","-","kg m-2 s-1","mm", & !16-20
                            "mm","mm","mm","m","kg m-2", &                     !21-25
                            "kg m-2","mm","mm","mm","mm", &                    !26-30
                            "W m-2","W m-2","W m-2","W m-2","W m-2", &         !31-35
                            "W m-2","W m-2","W m-2","W m-2","W m-2", &         !36-40
                            "W m-2","W m-2","W m-2","W m-2","K", &             !41-45
                            "K","K","K","K","K", &                             !46-50
                            "K","K","kg/kg","kg/kg","Pa", &                    !51-55
                            "fraction","m","mm","mm","K", &                    !56-60
                            "m3 m-3","K","m3 m-3","m","kg m-2", &              !61-65
                            "mm s-1","count","1","mm","mm", &                  !66-70
                            "-","-","m s-1","m s-1","m s-1", &                 !71-75
                            "m s-1","m s-1","m s-1","g m-2","g m-2", &         !76-80
                            "g m-2","g m-2","g m-2","g m-2","g m-2s-1 CO2", &  !81-85
                            "g m-2s-1 C","g m-2s-1 C","umol CO2 m-2 s-1","W m-2","mm", & !86-90
                            "mm","1","1","1","K", &                            !91-95
                            "-","-","mm s-1",&                                 !96-98
                            "-","m","-", &                                     !99-101
                            "kg/(m2 s)","m","kg m-2",&                         !102-104
                            "m","-","days since snowfall",&                    !105-107
                            "K","m","-",&                                      !108-110
                            "kg/m3","J/m2","m",&                               !111-113
                            "kg m-2","kg/m2","kg/m2"]                          !114-116

   ldasOutDict%scaleFactor(:) = [1.0, 1.0, 0.01, 0.1, 0.1, &                   !1-5
                                 0.1, 0.01, 0.1, 0.00001, 0.01, &              !6-10
                                 0.1, 0.1, 0.1, 0.1, 0.1, &                    !11-15
                                 0.00001, 0.00001, 0.01, 0.00001, 0.01, &      !16-20
                                 0.001, 0.01, 0.01, 0.00001, 0.01, &           !21-25
                                 0.01, 0.01, 0.01, 0.01, 0.01, &               !26-30
                                 0.1, 0.1, 0.1, 0.1, 0.1, &                    !31-35
                                 0.1, 0.1, 0.1, 0.1, 0.1, &                    !36-40
                                 0.1, 0.1, 0.1, 0.1, 0.1, &                    !41-45
                                 0.1, 0.1, 0.1, 0.1, 0.1, &                    !46-50
                                 0.1, 0.1, 0.0001, 0.0001, 0.1, &              !51-55
                                 0.01, 0.00001, 0.01, 0.01, 0.1, &             !56-60
                                 0.01, 0.1, 0.01, 0.0001, 0.1, &               !61-65
                                 0.00001, 1.0, 0.001, 0.01, 0.01, &            !66-70
                                 0.00001, 0.00001, 0.00001, 0.00001, 0.00001, &!71-75
                                 0.00001, 0.00001, 0.00001, 0.01, 0.01, &      !76-80
                                 0.01, 0.01, 0.01, 0.01, 0.01, &               !81-85
                                 0.01, 0.01, 0.01, 0.01, 0.01, &               !86-90
                                 0.01, 0.01, 0.001, 0.001, 0.1, &              !91-95
                                 0.01, 0.01, 0.00001, &                        !96-98
                                 1.0, 0.1, 0.01, &                             !99-101
                                 0.0001, 0.0001, 0.1, &                        !102-104
                                 0.01, 0.01, 0.01, &                           !105-107
                                 0.1, 0.0001, 0.01, &                          !108-110
                                 0.001, 1000.0, 0.1, &                         !111-113
                                 0.1, 0.001, 0.001 ]                           !114-116

   ldasOutDict%addOffset(:) = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0, & !1-10
                               0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0, & !11-20
                               0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0, & !21-30
                               0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0, & !31-40
                               0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0, & !41-50
                               0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0, & !51-60
                               0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0, & !61-70
                               0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0, & !71-80
                               0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0, & !81-90
                               0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0, &         !91-98
                               0.0,0.0,0.0, &                             !99-101
                               0.0,0.0,0.0, &                             !102-104
                               0.0,0.0,0.0, &                             !105-107
                               0.0,0.0,0.0, &                             !108-110
                               0.0,0.0,0.0, &                             !111-113
                               0.0,0.0,0.0]                               !114-116

   ! Note that output flags will be set in the the output routine, and will vary
   ! by the IOC flag specified in hydro.namelist.
   ldasOutDict%outFlag(:) = [0,0,0,0,0,0,0,0,0,0, & !1-10
                             1,0,0,0,0,0,0,0,0,0, & !11-20
                             0,0,0,0,0,0,0,0,0,0, & !21-30
                             0,0,0,0,0,0,0,0,0,0, & !31-40
                             0,0,0,0,0,0,0,0,0,0, & !41-50
                             0,0,0,0,0,0,0,0,0,0, & !51-60
                             0,0,0,0,0,0,0,0,0,0, & !61-70
                             0,0,0,0,0,0,0,0,0,0, & !71-80
                             0,0,0,0,0,0,0,0,0,0, & !81-90
                             0,0,0,0,0,0,0,0, &     !91-98
                             0,0,0, &               !99-101
                             0,0,0, &               !102-104
                             0,0,0, &               !105-107
                             0,0,0, &               !108-110
                             0,0,0, &               !111-103
                             0,0,0]                 !114-116

   ldasOutDict%timeZeroFlag(:) = [1,1,1,1,1,1,1,1,1,1, & !1-10
                                  1,1,1,1,1,1,1,1,1,1, & !11-20
                                  1,1,1,1,1,1,1,1,1,1, & !21-30
                                  1,1,1,1,1,1,1,1,1,1, & !31-40
                                  1,1,1,1,1,1,1,1,1,1, & !41-50
                                  1,1,1,1,1,1,1,1,1,1, & !51-60
                                  1,1,1,1,1,1,1,1,1,1, & !61-70
                                  1,1,1,1,1,1,1,1,1,1, & !71-80
                                  1,1,1,1,1,1,1,1,1,1, & !81-90
                                  1,1,1,1,1,1,1,1, &     !91-98
                                  1,1,1, &               !99-101
                                  1,1,1, &               !102-104
                                  1,1,1, &               !105-107
                                  1,1,1, &               !108-110
                                  1,1,1, &               !111-113
                                  1,1,1]                 !114-116

   ldasOutDict%numLev(:) = [1,1,1,1,1,1,1,1,1,1, &  !1-10
                            1,1,1,1,1,1,1,1,1,1, &  !11-20
                            1,1,1,1,1,1,1,1,1,1, &  !21-30
                            1,1,1,1,1,1,1,1,1,1, &  !31-40
                            1,1,1,1,1,1,1,1,1,1, &  !41-50
                            1,1,1,1,1,1,3,3,3,4, &  !51-60
                            4,3,4,1,1,1,1,1,1,1, &  !61-70
                            1,1,1,1,1,1,1,1,1,1, &  !71-80
                            1,1,1,1,1,1,1,1,1,1, &  !81-90
                            1,1,1,1,1,2,2,1, &      !91-98
                            1,1,1, &                !99-101
                            1,1,1, &                !102-104
                            40,40,40, &             !105-107
                            40,40,40, &             !108-110
                            40,40,40, &             !111-113
                            40,1,1]                 !114-116
   ldasOutDict%numLev(105:114) = ldasOutDict%act_lev ! Set crocus levels to number from namelist

   ldasOutDict%missingReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !1-5
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !6-10
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !11-15
                                 -9999.0, -999.0,-9999.0,-9999.0,-9999.0, & !16-20
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !21-25
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !26-30
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !31-35
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !36-40
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !41-45
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !46-50
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !51-55
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !56-60
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !61-65
                                  -999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !66-70
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !71-75
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !76-80
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !81-85
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !86-90
                                 -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !91-95
                                 -9999.0,-9999.0,-999.0,                  & !96-98
                                 -9999.0,-9999.0,-9999.0,                 & !99-101
                                 -9999.0,-9999.0,-9999.0,                 & !102-104
                                 -9999.0,-9999.0,-9999.0,                 & !105-107
                                 -9999.0,-9999.0,-9999.0,                 & !108-110
                                 -9999.0, 9999000.0,-9999.0,                 & !111-113
                                 -9999.0,-9999.0,-9999.0 ]                  !114-116

   ldasOutDict%fillReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !1-5
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !6-10
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !11-15
                              -9999.0, -999.0,-9999.0,-9999.0,-9999.0, & !16-20
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !21-25
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !26-30
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !31-35
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !36-40
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !41-45
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !46-50
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !51-55
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !56-60
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !61-65
                               -999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !66-70
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !71-75
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !76-80
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !81-85
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !86-90
                              -9999.0,-9999.0,-9999.0,-9999.0,-9999.0, & !91-95
                              -9999.0,-9999.0,-999.0,                  & !96-98
                              -9999.0,-9999.0,-9999.0,                 & !99-101
                              -9999.0,-9999.0,-9999.0,                 & !102-104
                              -9999.0,-9999.0,-9999.0,                 & !105-107
                              -9999.0,-9999.0,-9999.0,                 & !108-110
                              -9999.0, 9999000.0,-9999.0,                 & !111-113
                              -9999.0,-9999.0,-9999.0 ]                  !114-116

   ldasOutDict%validMinDbl(:) = [0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &                     !1-5
                                 -1000.0d0, -1.0d0, -1500.0d0, 0.0d0, 0.0d0, &            !6-10
                                 -1500.0d0, -1500.0d0, -1500.0d0, -1500.0d0, -1500.0d0, & !11-15
                                 -100.0d0, -100.0d0, 0.0d0, -100.0d0, -100.0d0, &         !16-20
                                 0.0d0, -5.0d0, -5.0d0, 0.0d0, 0.0d0, &                   !21-25
                                 0.0d0, 0.0d0, -100.0d0, -100.0d0, -100.0d0, &            !26-30
                                 -1500.0d0, -1500.0d0, -1500.0d0, -1500.0d0, -1500.0d0, & !31-35
                                 -1500.0d0, -1500.0d0, -1500.0d0, -1500.0d0, -1500.0d0, & !36-40
                                 -1500.0d0, -1500.0d0, -1500.0d0, -1500.0d0, 0.0d0, &     !41-45
                                 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &                     !46-50
                                 0.0d0, 0.0d0, 0.0d0, 0.0d0, -1000.0d0, &                 !51-55
                                 0.0d0, -100.0d0, 0.0d0, 0.0d0, 0.0d0, &                  !56-60
                                 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &                     !61-65
                                 0.0d0, -10.d0, 0.0d0, 0.0d0, 0.0d0, &                    !66-70
                                 -5.0d0, -5.0d0, -5.0d0, -5.0d0, -5.0d0, &                !71-75
                                 -5.0d0, -5.0d0, -5.0d0, 0.0d0, 0.0d0, &                  !76-80
                                 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &                     !81-85
                                 0.0d0, 0.0d0, 0.0d0, 0.0d0, -1000.0d0, &                 !86-90
                                 -5.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &                    !91-95
                                 0.0d0, 0.0d0, 0.0d0,&                                    !96-98
   !! NBNB Check these values
                                 0.0d0, 0.0d0, 0.0d0, &                                   !99-101
                                 0.0d0, 0.0d0, 0.0d0, &                                   !102-104
                                 -1.0d2, 0.0d0, 0.0d0, &                                  !105-107
                                 0.0d0, 0.0d0, 0.0d0, &                                   !108-110
                                 0.0d0, -2.0d12, 0.0d0, &                                  !111-113
                                 0.0d0, 0.0d0, 0.0d0 ]                                    !114-116

   ldasOutDict%validMaxDbl(:) = [100.0d0, 100.0d0, 1.0d0, 20.0d0, 20.0d0, &             !1-5
                                 3000.0d0, 1.0d0, 1500.0d0, 100.0d0, 1.0d0, &           !6-10
                                 1500.0d0, 1500.0d0, 1500.0d0, 1500.0d0, 1500.0d0, &    !11-15
                                 100.0d0, 100.0d0, 1.0d0, 100.0d0, 100000.0d0, &        !16-20
                                 100000.0d0, 30000.0d0, 30000.0d0, 10.0d0, 10000.0d0, & !21-25
                                 10000.0d0, 1.0D+6, 1.0D+6, 1.0D+6, 1.0D+6, &           !26-30
                                 1500.0d0, 1500.0d0, 1500.0d0, 1500.0d0, 1500.0d0, &    !31-35
                                 1500.0d0, 1500.0d0, 1500.0d0, 1500.0d0, 1500.0d0, &    !36-40
                                 1500.0d0, 1500.0d0, 1500.0d0, 1500.0d0, 400.0d0, &     !41-45
                                 400.0d0, 400.0d0, 400.0d0, 400.0d0, 400.0d0, &         !46-50
                                 400.0d0, 400.0d0, 1.0d0, 1.0d0, 100000.0d0, &          !51-55
                                 1.0d0, 100.0d0, 100000.0d0, 100000.0d0, 400.0d0, &     !56-60
                                 1.0d0, 400.0d0, 1.0d0, 10000.0d0, 100000000.0d0, &     !61-65
                                 100.0d0, 10.0d0, 1.0d0, 1.0D+6, 100000.0d0, &          !66-70
                                 5.0d0, 5.0d0, 5.0d0, 5.0d0, 5.0d0, &                   !71-76
                                 5.0d0, 5.0d0, 5.0d0, 1000.0d0, 1000.0d0, &             !76-80
                                 1000.0d0, 1000.0d0, 5000.0d0, 5000.0d0, 1000.0d0, &    !81-85
                                 1000.0d0, 1000.0d0, 1000.0d0, 1000.0d0, 1.0D+6, &      !86-90
                                 30000.0d0, 1.0d0, 1.0d0, 1.0d0, 400.0d0, &             !91-95
                                 1.0d0, 1.0d0, 100.0d0,&                                !96-98
   ! NBNB Check these values
                                 1.0d0, 1.0d4, 1.0d0, &                                 !99-101
                                 1.0d5, 1.0d4, 1.0d8, &                                 !102-104
                                 1.0D2, 1.0D2, 1.0d5, &                                 !105-107
                                 300.0d0, 1.0d4, 1.0d2, &                               !108-110
                                 1.0d5, 0.0d0, 1.0d3, &                                 !111-113
                                 1.0d6, 1.d5, 1.d5]                                     !114-116

   ! Loop through and calculate missing/fill/min/max values that will be placed
   ! into the NetCDF attributes after scale_factor/add_offset are applied.
   do i=1,numLdasVars
      if (ldasOutDict%scaleFactor(i) .le. 1. .and. ldasOutDict%scaleFactor(i) .gt. 0.) then
        ldasOutDict%fillComp(i) = &
           nint((ldasOutDict%fillReal(i)     + ldasOutDict%addOffset(i)), 4) * &
           nint(one_dbl / ldasOutDict%scaleFactor(i), 4)
        ldasOutDict%missingComp(i) = &
           nint((ldasOutDict%missingReal(i)  + ldasOutDict%addOffset(i)), 4) * &
           nint(one_dbl / ldasOutDict%scaleFactor(i), 4)
        ldasOutDict%validMinComp(i) = &
           nint((ldasOutDict%validMinDbl(i) + ldasOutDict%addOffset(i)) * &
                nint(one_dbl / ldasOutDict%scaleFactor(i), 4), 4)
        ldasOutDict%validMaxComp(i) = &
           nint((ldasOutDict%validMaxDbl(i) + ldasOutDict%addOffset(i)) * &
                nint(one_dbl / ldasOutDict%scaleFactor(i), 4), 4)
      else if (ldasOutDict%scaleFactor(i) .gt. 1.) then
        ldasOutDict%fillComp(i) = &
           nint( (ldasOutDict%fillReal(i) + ldasOutDict%addOffset(i)) * &
                 (one_dbl / ldasOutDict%scaleFactor(i)), 4 )
        ldasOutDict%missingComp(i) = &
           nint( (ldasOutDict%missingReal(i) + ldasOutDict%addOffset(i)) * &
                 (one_dbl / ldasOutDict%scaleFactor(i)), 4 )
        ldasOutDict%validMinComp(i) = &
           nint( (ldasOutDict%validMinDbl(i) + ldasOutDict%addOffset(i)) * &
                 (one_dbl / ldasOutDict%scaleFactor(i)), 4 )
        ldasOutDict%validMaxComp(i) = &
           nint( (ldasOutDict%validMaxDbl(i) + ldasOutDict%addOffset(i)) * &
                 (one_dbl / ldasOutDict%scaleFactor(i)), 4 )
      end if
   end do

end subroutine initLdasDict

subroutine initRtDomainDict(rtDomainDict,procId,diagFlag)
  use config_base, only: nlst
  use netcdf
  implicit none

   type(rtDomainMeta), intent(inout) :: rtDomainDict
   integer, intent(inout) :: procId,diagFlag
   integer :: ftnMeta,projVarId,xVarId,yVarId,ftnGeo
   integer :: xDimId,numColLand,numColHydro
   real :: resLand,resHydro,aggFactor
   integer :: iret
   integer :: crsRealAttCnt,xRealAttCnt,yRealAttCnt
   integer :: crsCharAttCnt,xCharAttCnt,yCharAttCnt
   integer :: i, nCrsAtts,nxAtts,nyAtts
   integer :: charFlag
   integer :: floatFlag
   character(len=512) :: tmpAttName
   integer :: xtypeTmp
   integer :: tmpLen
   ! RT_DOMAIN files

   rtDomainDict%numSoilLayers = nlst(1)%nsoil

   rtDomainDict%modelNdv = -9.E15

   ! First establish global attributes.
   rtDomainDict%title = "OUTPUT FROM " // trim(get_model_version())
   rtDomainDict%initTime = "1970-01-01_00:00:00" ! This will be calculated in I/O code.
   rtDomainDict%validTime = "1970-01-01_00:00:00" ! This will be calculated in I/O code.
   rtDomainDict%decimation = 1
   rtDomainDict%conventions = "CF-1.6"

   ! Next establish time attributes
   rtDomainDict%timeLName = "valid output time"
   rtDomainDict%timeUnits = "minutes since 1970-01-01 00:00:00 UTC"
   rtDomainDict%timeStName = "time"
   rtDomainDict%rTimeLName = "model initialization time"
   rtDomainDict%rTimeUnits = "minutes since 1970-01-01 00:00:00 UTC"
   rtDomainDict%rTimeStName = "forecast_reference_time"

   crsRealAttCnt = 0
   crsCharAttCnt = 0
   xRealAttCnt = 0
   xCharAttCnt = 0
   yRealAttCnt = 0
   yCharAttCnt = 0

   ! Pull spatial metadata information about the modeling domain from the
   ! Fulldom file.
   if(procId .eq. 0) then
      iret = nf90_open(trim(nlst(1)%geo_finegrid_flnm),NF90_NOWRITE,ncid=ftnMeta)
      if(iret .ne. 0) then
         ! Spatial metadata file not found for hydro grid.
         call postDiagMsg(diagFlag,'WARNING: Unable to open Fulldom metadata file. No crs variable or attributes will be created.')
         rtDomainDict%nCrsCharAtts = 0
         rtDomainDict%nCrsRealAtts = 0
         rtDomainDict%nxCharAtts = 0
         rtDomainDict%nxRealAtts = 0
         rtDomainDict%nyCharAtts = 0
         rtDomainDict%nyRealAtts = 0
         rtDomainDict%proj4 = ''
      else
         iret = nf90_get_att(ftnMeta,NF90_GLOBAL,'proj4',rtDomainDict%proj4)
         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: Unable to find proj4 global attribute. Defaulting to blank string.')
            rtDomainDict%proj4 = ''
         endif
         charFlag = 0
         floatFlag = 0
         ! Find the crs variable and pull out the attributes, their names, and
         ! their values. This will be translated to output files.
         iret = nf90_inq_varid(ftnMeta,'crs',projVarId)
         ! For now we are going to allow the code to move forward without
         ! finding this variable. In the future, we will probably restrict the
         ! code to ensure things are more seamless.

         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: Unable to locate the crs variable. No crs variable or attributes will be created.')
            rtDomainDict%nCrsCharAtts = 0
            rtDomainDict%nCrsRealAtts = 0
         else
            iret = nf90_inquire_variable(ftnMeta,projVarId,nAtts=nCrsAtts)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find crs number of attributes')
            do i =1,nCrsAtts
               iret = nf90_inq_attname(ftnMeta,projVarId,i,name=tmpAttName)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to extract attribute names from crs variable.')
               iret = nf90_inquire_attribute(ftnMeta,projVarId,trim(tmpAttName),xtype=xtypeTmp,len=tmpLen)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to find attribute types from crs variable.')
               select case (xtypeTmp)
                  case (NF90_FLOAT)
                     floatFlag = 1
                  case (NF90_CHAR)
                     charFlag = 1
                  case (NF90_SHORT)
                     floatFlag = 1
                  case (NF90_USHORT)
                     floatFlag = 1
                  case (NF90_INT)
                     floatFlag = 1
                  case (NF90_UINT)
                     floatFlag = 1
                  case (NF90_INT64)
                     floatFlag = 1
                  case (NF90_UINT64)
                     floatFlag = 1
                  case (NF90_DOUBLE)
                     floatFlag = 1
                  case (NF90_STRING)
                     charFlag = 1
               end select
               if(charFlag .eq. 1) then
                  rtDomainDict%crsCharAttNames(crsCharAttCnt+1) = trim(tmpAttName)
                  iret = nf90_get_att(ftnMeta,projVarId,trim(tmpAttName),rtDomainDict%crsCharAttVals(crsCharAttCnt+1))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull crs attributes')
                  crsCharAttCnt = crsCharAttCnt + 1
               else
                  rtDomainDict%crsFloatAttNames(crsRealAttCnt+1) = trim(tmpAttName)
                  rtDomainDict%crsRealAttLen(crsRealAttCnt+1) = tmpLen
                  iret = nf90_get_att(ftnMeta,projVarId,trim(tmpAttName),rtDomainDict%crsRealAttVals(crsRealAttCnt+1,1:tmpLen))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull crs attributes')
                  crsRealAttCnt = crsRealAttCnt + 1
               endif
               charFlag = 0
               floatFlag = 0
            end do
            rtDomainDict%nCrsRealAtts = crsRealAttCnt
            rtDomainDict%nCrsCharAtts = crsCharAttCnt

         endif

         ! Next pull the attributes from the x/y dimensions
         charFlag = 0
         floatFlag = 0
         iret = nf90_inq_varid(ftnMeta,'x',xVarId)
         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: Unable to locate the x variable. No x variable or attributes will be created.')
            rtDomainDict%nxCharAtts = 0
            rtDomainDict%nxRealAtts = 0
         else
            iret = nf90_inquire_variable(ftnMeta,xVarId,nAtts=nxAtts)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find x number of attributes')
            do i =1,nxAtts
               iret = nf90_inq_attname(ftnMeta,xVarId,i,name=tmpAttName)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to extract attribute names from x variable.')
               iret = nf90_inquire_attribute(ftnMeta,xVarId,trim(tmpAttName),xtype=xtypeTmp,len=tmpLen)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to find attribute types from x variable.')
               select case (xtypeTmp)
                  case (NF90_FLOAT)
                     floatFlag = 1
                  case (NF90_CHAR)
                     charFlag = 1
                  case (NF90_SHORT)
                     floatFlag = 1
                  case (NF90_USHORT)
                     floatFlag = 1
                  case (NF90_INT)
                     floatFlag = 1
                  case (NF90_UINT)
                     floatFlag = 1
                  case (NF90_INT64)
                     floatFlag = 1
                  case (NF90_UINT64)
                     floatFlag = 1
                  case (NF90_DOUBLE)
                     floatFlag = 1
                  case (NF90_STRING)
                     charFlag = 1
               end select
               if(charFlag .eq. 1) then
                  rtDomainDict%xCharAttNames(xCharAttCnt+1) = trim(tmpAttName)
                  iret = nf90_get_att(ftnMeta,xVarId,trim(tmpAttName),rtDomainDict%xCharAttVals(xCharAttCnt+1))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull x attributes')
                  xCharAttCnt = xCharAttCnt + 1
               else
                  rtDomainDict%xFloatAttNames(xRealAttCnt+1) = trim(tmpAttName)
                  rtDomainDict%xRealAttLen(xRealAttCnt+1) = tmpLen
                  iret = nf90_get_att(ftnMeta,xVarId,trim(tmpAttName),rtDomainDict%xRealAttVals(xRealAttCnt+1,1:tmpLen))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull x attributes')
                  xRealAttCnt = xRealAttCnt + 1
               endif
               charFlag = 0
               floatFlag = 0
            end do
            rtDomainDict%nxRealAtts = xRealAttCnt
            rtDomainDict%nxCharAtts = xCharAttCnt

         endif

         charFlag = 0
         floatFlag = 0
         iret = nf90_inq_varid(ftnMeta,'y',yVarId)
         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: Unable to locate the y variable. No y variable or attributes will be created.')
            rtDomainDict%nyCharAtts = 0
            rtDomainDict%nyRealAtts = 0
         else
            iret = nf90_inquire_variable(ftnMeta,yVarId,nAtts=nyAtts)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find y number of attributes')
            do i =1,nyAtts
               iret = nf90_inq_attname(ftnMeta,yVarId,i,name=tmpAttName)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to extract attribute names from y variable.')
               iret = nf90_inquire_attribute(ftnMeta,yVarId,trim(tmpAttName),xtype=xtypeTmp,len=tmpLen)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to find attribute types from y variable.')
               select case (xtypeTmp)
                  case (NF90_FLOAT)
                     floatFlag = 1
                  case (NF90_CHAR)
                     charFlag = 1
                  case (NF90_SHORT)
                     floatFlag = 1
                  case (NF90_USHORT)
                     floatFlag = 1
                  case (NF90_INT)
                     floatFlag = 1
                  case (NF90_UINT)
                     floatFlag = 1
                  case (NF90_INT64)
                     floatFlag = 1
                  case (NF90_UINT64)
                     floatFlag = 1
                  case (NF90_DOUBLE)
                     floatFlag = 1
                  case (NF90_STRING)
                     charFlag = 1
               end select
               if(charFlag .eq. 1) then
                  rtDomainDict%yCharAttNames(yCharAttCnt+1) = trim(tmpAttName)
                  iret = nf90_get_att(ftnMeta,yVarId,trim(tmpAttName),rtDomainDict%yCharAttVals(yCharAttCnt+1))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull y attributes')
                  yCharAttCnt = yCharAttCnt + 1
               else
                  rtDomainDict%yFloatAttNames(yRealAttCnt+1) = trim(tmpAttName)
                  rtDomainDict%yRealAttLen(yRealAttCnt+1) = tmpLen
                  iret = nf90_get_att(ftnMeta,yVarId,trim(tmpAttName),rtDomainDict%yRealAttVals(yRealAttCnt+1,1:tmpLen))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull y attributes')
                  yRealAttCnt = yRealAttCnt + 1
               endif
               charFlag = 0
               floatFlag = 0
            end do
            rtDomainDict%nyRealAtts = yRealAttCnt
            rtDomainDict%nyCharAtts = yCharAttCnt
         endif
         ! Close the file
         iret = nf90_close(ftnMeta)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close Fulldom file.')
      endif

      ! Next get the number of columns on the land grid. This will be used to
      ! calculate the resolution of the routing grid in meters and aggfactor.
      iret = nf90_open(trim(nlst(1)%land_spatial_meta_flnm),NF90_NOWRITE,ncid=ftnGeo)
      if(iret .ne. 0) then
         call postDiagMsg(diagFlag,'WARNING: Unable to open LAND Spatial Metadata file. Defaulting to missing values.')
         numColLand = -9999
         resLand = -9999
      else
         iret = nf90_inq_dimid(ftnGeo,'x',xDimId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find x dimension in LAND spatial Metadata file')
         iret = nf90_inquire_dimension(ftnGeo,xDimId,len=numColLand)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to retrieve number of columns in the LAND spatial metadata file')
         iret = nf90_inq_varid(ftnGeo,'x',xVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find x variable in LAND spatial metadata file')
         iret = nf90_get_att(ftnGeo,xVarId,'resolution',resLand)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x resolution in LAND spatial metadata file')
         iret = nf90_close(ftnGeo)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close the LAND spatial metadata file')
      endif

      ! Next get the number of columns on the high-resolution routing grid.
      ! This will be used to calculate the resolution of the routing grid in
      ! meters.
      iret = nf90_open(trim(nlst(1)%geo_finegrid_flnm),NF90_NOWRITE,ncid=ftnGeo)
      if(iret .ne. 0) then
         call nwmCheck(diagFlag,iret,'ERROR: Unable to open Fulldom file')
      else
         iret = nf90_inq_dimid(ftnGeo,'x',xDimId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find x dimension in Fulldom file')
         iret = nf90_inquire_dimension(ftnGeo,xDimId,len=numColHydro)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to retrieve number of columns in the Fulldom file')
         iret = nf90_close(ftnGeo)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close the Fulldom file')
      endif

      ! Calculate the aggregation factor and resolution of the hydro routing
      ! grid.
      if(numColLand .ne. -9999) then
         aggFactor = float(numColHydro/numColLand)
         resHydro = resLand/aggFactor
      else
         resHydro = -9999
      endif

      rtDomainDict%xRes = resHydro
      rtDomainDict%yRes = resHydro

   endif

   rtDomainDict%varNames(:) = [character(len=64) :: "zwattablrt","sfcheadsubrt","QSTRMVOLRT",&
                               "QBDRYRT","SOIL_M"]
   rtDomainDict%longName(:) = [character(len=64) :: "depth to saturation, rounded to highest saturated layer",&
                               "surface head","channel inflow",&
                               "accumulated value of the boundary flux, + into domain - out of domain",&
                               "volumetric soil moisture"]
   rtDomainDict%units(:) = [character(len=64) :: "m","mm","mm","mm","m3 m-3"]
   rtDomainDict%scaleFactor(:) = [0.1,1.0,1.0,1.0,0.01]
   rtDomainDict%addOffset(:) = [0.0,0.0,0.0,0.0,0.0]
   rtDomainDict%outFlag(:) = [0,0,0,0,0]
   rtDomainDict%timeZeroFlag(:) = [1,1,1,1,1]
   rtDomainDict%missingReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0,-9999.0]
   rtDomainDict%fillReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0,-9999.0]
   rtDomainDict%validMinDbl(:) = [0.0d0, 0.0d0, 0.0d0, -1000000.0d0, 0.0d0]
   rtDomainDict%validMaxDbl(:) = [100.0d0, 1000000.0d0, 1000000.0d0, 1000000.0d0, 100.0d0]

   ! Loop through and calculate missing/fill/min/max values that will be placed
   ! into the NetCDF attributes after scale_factor/add_offset are applied.
   do i=1,numRtDomainVars
      rtDomainDict%fillComp(i) = &
           nint((rtDomainDict%fillReal(i)     + rtDomainDict%addOffset(i)), 8) * &
           nint(one_dbl / rtDomainDict%scaleFactor(i), 8)
      rtDomainDict%missingComp(i) = &
           nint((rtDomainDict%missingReal(i)  + rtDomainDict%addOffset(i)), 8) * &
           nint(one_dbl / rtDomainDict%scaleFactor(i), 8)
      rtDomainDict%validMinComp(i) = &
           nint((rtDomainDict%validMinDbl(i) + rtDomainDict%addOffset(i)) * &
                nint(one_dbl / rtDomainDict%scaleFactor(i), 8), 8)
      rtDomainDict%validMaxComp(i) = &
           nint((rtDomainDict%validMaxDbl(i) + rtDomainDict%addOffset(i)) * &
                nint(one_dbl / rtDomainDict%scaleFactor(i), 8), 8)
   end do

end subroutine initRtDomainDict

subroutine initLakeDict(lakeOutDict,procId,diagFlag)
  use config_base, only: nlst
  use netcdf
  implicit none

   type(lakeMeta), intent(inout) :: lakeOutDict
   integer, intent(inout) :: diagFlag
   integer, intent(inout) :: procId

   ! Local variables
   integer :: ftnMeta,iret
   integer :: projVarId

   lakeOutDict%modelNdv = -9.E15
   ! LAKE FILES
   ! NOTE !!!!! If you see PLC, this means OWP has no desire to output these,
   !            which means meta-data standards have yet to be determined
   !            for these variables. Fill in if it's desired to output....
   ! First establish global attributes for the channel output files
   lakeOutDict%title = "OUTPUT FROM " // trim(get_model_version())
   lakeOutDict%fType = 'timeSeries'
   lakeOutDict%initTime = '1970-01-01_00:00:00' ! This will be calculated in I/O code
   lakeOutDict%validTime = '1970-01-01_00:00:00' ! This will be calculated in I/O code
   lakeOutDict%lakeDim = 'lake_id'
   lakeOutDict%cdm = 'PLACEHOLDER'
   lakeOutDict%conventions = 'CF-1.6'

   ! Pull spatial metadata information about the modeling domain from the land
   ! spatial metadata file.
   if(procId .eq. 0) then
      iret = nf90_open(trim(nlst(1)%land_spatial_meta_flnm),NF90_NOWRITE,ncid=ftnMeta)
      if(iret .ne. 0) then
         ! Spatial metadata file not found for land grid.
         call postDiagMsg(diagFlag,'WARNING: Unable to open LAND spatial metadata file.')
         lakeOutDict%proj4 = ''
         lakeOutDict%esri = ''
      else
         ! First pull metadata on coordinate system.
         iret = nf90_inq_varid(ftnMeta,'crs',projVarId)
         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: Unable to find crs in LAND spatial metadata file')
            lakeOutDict%proj4 = ''
            lakeOutDict%esri = ''
         else
            iret = nf90_get_att(ftnMeta,projVarId,'esri_pe_string',lakeOutDict%esri)
            if(iret .ne. 0) then
               call postDiagMsg(diagFlag,'WARNING: Unable to find esri_pe_string in LAND spatial metadata file.')
               lakeOutDict%esri = ''
            endif
            iret = nf90_get_att(ftnMeta,NF90_GLOBAL,'proj4',lakeOutDict%proj4)
            ! We are going to put a relaxed constraint on the proj4 string.
            if(iret .ne. 0) then
               call postDiagMsg(diagFlag,'WARNING: proj4 string not found. Defaulting to blank string.')
               lakeOutDict%proj4 = ''
            endif
         endif
         ! Close the file
         iret = nf90_close(ftnMeta)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close LAND spatial metadata file.')
      endif
   endif

   ! Next establish time attribues
   lakeOutDict%timeLName = 'valid output time'
   lakeOutDict%timeUnits = 'minutes since 1970-01-01 00:00:00 UTC'
   lakeOutDict%timeStName = 'time'
   lakeOutDict%rTimeLName = 'model initialization time'
   lakeOutDict%rTimeStName = 'forecast_reference_time'
   lakeOutDict%rTimeUnits = 'minutes since 1970-01-01 00:00:00 UTC'

   ! Establish elevation variable attributes
   lakeOutDict%elevLName = "Water Surface Elevation"
   lakeOutDict%elevUnits = "m" ! meters
   lakeOutDict%elevComment = "If reservoir_type = 4, " &
       // "water_sfc_elev is invalid because this value corresponds only to level pool"

   ! Establish feature_id attributes
   lakeOutDict%featureIdLName = "Lake ComID"
   lakeOutDict%featureIdComment = "ComID from NHDPlusV2 waterbody layer"
   lakeOutDict%cfRole = 'timeseries_id'

   ! Establish reservoir_type attributes
   lakeOutDict%reservoirTypeLName = "reservoir_type"
   lakeOutDict%reservoirTypeFlagValues = [1,2,3,4]
   lakeOutDict%reservoirTypeFlagMeanings = "Level_pool USGS-persistence USACE-persistence RFC-forecasts"

   ! Establish reservoir_assimilated_value attributes
   lakeOutDict%reservoirAssimilatedValueLName = "reservoir_assimilated_value"
   lakeOutDict%reservoirAssimilatedValueUnits = "m3 s-1"

   ! Establish reservoir_assimilated_source_file attributes
   lakeOutDict%reservoirAssimilatedSourceFileLName= "reservoir_assimilated_source_file"

  ! Esatablish lat/lon attributes
   lakeOutDict%latLName = "Lake latitude"
   lakeOutDict%latUnits = "degrees_north"
   lakeOutDict%latStName = "latitude"
   lakeOutDict%lonLName = "Lake longitude"
   lakeOutDict%lonUnits = "degrees_east"
   lakeOutDict%lonStName = "longitude"

   lakeOutDict%varNames(:) = [character(len=64) :: 'inflow','outflow']
   lakeOutDict%longName(:) = [character(len=64) :: 'Lake Inflow','Lake Outflow']
   lakeOutDict%units(:) = [character(len=64) :: 'm3 s-1','m3 s-1']
   lakeOutDict%coordNames(:) = [character(len=64) :: 'latitude longitude','latitude longitude']
   lakeOutDict%scaleFactor(:) = [0.01,0.01]
   lakeOutDict%addOffset(:) = [0.0,0.0]
   lakeOutDict%outFlag(:) = [0,0]
   lakeOutDict%timeZeroFlag(:) = [1,1]
   lakeOutDict%fillReal(:) = [-9999.0,-9999.0]
   lakeOutDict%missingReal(:) = [-9999.0,-9999.0]
   lakeOutDict%validMinDbl(:) = [-10000.0d0, -10000.0d0]
   lakeOutDict%validMaxDbl(:) = [10000.0d0, 10000.0d0]

   ! Loop through and calculate missing/fill/min/max values that will be placed
   ! into the NetCDF attributes after scale_factor/add_offset are applied.
   do i=1,numLakeVars
      lakeOutDict%fillComp(i) = &
           nint((lakeOutDict%fillReal(i)     + lakeOutDict%addOffset(i)), 8) * &
           nint(one_dbl / lakeOutDict%scaleFactor(i), 8)
      lakeOutDict%missingComp(i) = &
           nint((lakeOutDict%missingReal(i)  + lakeOutDict%addOffset(i)), 8) * &
           nint(one_dbl / lakeOutDict%scaleFactor(i), 8)
      lakeOutDict%validMinComp(i) = &
           nint((lakeOutDict%validMinDbl(i) + lakeOutDict%addOffset(i)) * &
                nint(one_dbl / lakeOutDict%scaleFactor(i), 8), 8)
      lakeOutDict%validMaxComp(i) = &
           nint((lakeOutDict%validMaxDbl(i) + lakeOutDict%addOffset(i)) * &
                nint(one_dbl / lakeOutDict%scaleFactor(i), 8), 8)
   end do

end subroutine initLakeDict

subroutine initChrtGrdDict(chrtGrdDict,procId,diagFlag)
  use config_base, only: nlst
  use netcdf
  implicit none

   type(chrtGrdMeta), intent(inout) :: chrtGrdDict
   integer, intent(inout) :: procId,diagFlag
   integer :: ftnMeta,projVarId,xVarId,yVarId,ftnGeo
   integer :: xDimId,numColLand,numColHydro
   real :: resLand,resHydro,aggFactor
   integer :: iret
   integer :: crsRealAttCnt,xRealAttCnt,yRealAttCnt
   integer :: crsCharAttCnt,xCharAttCnt,yCharAttCnt
   integer :: i, nCrsAtts,nxAtts,nyAtts
   integer :: charFlag
   integer :: floatFlag
   character(len=512) :: tmpAttName
   integer :: xtypeTmp
   integer :: tmpLen
   !CHRTOUT_GRID files

   chrtGrdDict%modelNdv = -9.E15

   ! First establish global attributes.
   chrtGrdDict%title = "OUTPUT FROM " // trim(get_model_version())
   chrtGrdDict%initTime = "1970-01-01_00:00:00" ! This will be calculated in I/O code.
   chrtGrdDict%validTime = "1970-01-01_00:00:00" ! This will be calculated in I/O code.
   chrtGrdDict%decimation = 1
   chrtGrdDict%conventions = "CF-1.6"

   ! Next establish time attributes
   chrtGrdDict%timeLName = "valid output time"
   chrtGrdDict%timeUnits = "minutes since 1970-01-01 00:00:00 UTC"
   chrtGrdDict%timeStName = "time"
   chrtGrdDict%rTimeLName = "model initialization time"
   chrtGrdDict%rTimeUnits = "minutes since 1970-01-01 00:00:00 UTC"
   chrtGrdDict%rTimeStName = "forecast_reference_time"

   crsRealAttCnt = 0
   crsCharAttCnt = 0
   xRealAttCnt = 0
   xCharAttCnt = 0
   yRealAttCnt = 0
   yCharAttCnt = 0

   ! Pull spatial metadata information about the modeling domain from the
   ! Fulldom file.
   if(procId .eq. 0) then
      iret = nf90_open(trim(nlst(1)%geo_finegrid_flnm),NF90_NOWRITE,ncid=ftnMeta)
      if(iret .ne. 0) then
         ! Spatial metadata file not found for hydro grid.
         call postDiagMsg(diagFlag,'WARNING: Unable to open Fulldom file. No crs variable or attributes will be created.')
         chrtGrdDict%nCrsCharAtts = 0
         chrtGrdDict%nCrsRealAtts = 0
         chrtGrdDict%nxCharAtts = 0
         chrtGrdDict%nxRealAtts = 0
         chrtGrdDict%nyCharAtts = 0
         chrtGrdDict%nyRealAtts = 0
         chrtGrdDict%proj4 = ''
      else
         iret = nf90_get_att(ftnMeta,NF90_GLOBAL,'proj4',chrtGrdDict%proj4)
         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: Unable to find proj4 global attribute. Defaulting to blank string.')
            chrtGrdDict%proj4 = ''
         endif
         charFlag = 0
         floatFlag = 0
         ! Find the crs variable and pull out the attributes, their names, and
         ! their values. This will be translated to output files.
         iret = nf90_inq_varid(ftnMeta,'crs',projVarId)
         ! For now we are going to allow the code to move forward without
         ! finding this variable. In the future, we will probably restrict the
         ! code to ensure things are more seamless.

         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: Unable to locate the crs variable. No crs variable or attributes will be created.')
            chrtGrdDict%nCrsCharAtts = 0
            chrtGrdDict%nCrsRealAtts = 0
         else
            iret = nf90_inquire_variable(ftnMeta,projVarId,nAtts=nCrsAtts)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find crs number of attributes')
            do i =1,nCrsAtts
               iret = nf90_inq_attname(ftnMeta,projVarId,i,name=tmpAttName)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to extract attribute names from crs variable.')
               iret = nf90_inquire_attribute(ftnMeta,projVarId,trim(tmpAttName),xtype=xtypeTmp,len=tmpLen)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to find attribute types from crs variable.')
               select case (xtypeTmp)
                  case (NF90_FLOAT)
                     floatFlag = 1
                  case (NF90_CHAR)
                     charFlag = 1
                  case (NF90_SHORT)
                     floatFlag = 1
                  case (NF90_USHORT)
                     floatFlag = 1
                  case (NF90_INT)
                     floatFlag = 1
                  case (NF90_UINT)
                     floatFlag = 1
                  case (NF90_INT64)
                     floatFlag = 1
                  case (NF90_UINT64)
                     floatFlag = 1
                  case (NF90_DOUBLE)
                     floatFlag = 1
                  case (NF90_STRING)
                     charFlag = 1
               end select
               if(charFlag .eq. 1) then
                  chrtGrdDict%crsCharAttNames(crsCharAttCnt+1) = trim(tmpAttName)
                  iret = nf90_get_att(ftnMeta,projVarId,trim(tmpAttName),chrtGrdDict%crsCharAttVals(crsCharAttCnt+1))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull crs attributes')
                  crsCharAttCnt = crsCharAttCnt + 1
               else
                  chrtGrdDict%crsFloatAttNames(crsRealAttCnt+1) = trim(tmpAttName)
                  chrtGrdDict%crsRealAttLen(crsRealAttCnt+1) = tmpLen
                  iret = nf90_get_att(ftnMeta,projVarId,trim(tmpAttName),chrtGrdDict%crsRealAttVals(crsRealAttCnt+1,1:tmpLen))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull crs attributes')
                  crsRealAttCnt = crsRealAttCnt + 1
               endif
               charFlag = 0
               floatFlag = 0
            end do
            chrtGrdDict%nCrsRealAtts = crsRealAttCnt
            chrtGrdDict%nCrsCharAtts = crsCharAttCnt
         endif

         ! Next pull the attributes from the x/y dimensions
         charFlag = 0
         floatFlag = 0
         iret = nf90_inq_varid(ftnMeta,'x',xVarId)
         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: Unable to locate the x variable. No x variable or attributes will be created.')
            chrtGrdDict%nxCharAtts = 0
            chrtGrdDict%nxRealAtts = 0
         else
            iret = nf90_inquire_variable(ftnMeta,xVarId,nAtts=nxAtts)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find x number of attributes')
            do i =1,nxAtts
               iret = nf90_inq_attname(ftnMeta,xVarId,i,name=tmpAttName)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to extract attribute names from x variable.')
               iret = nf90_inquire_attribute(ftnMeta,xVarId,trim(tmpAttName),xtype=xtypeTmp,len=tmpLen)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to find attribute types from x variable.')
               select case (xtypeTmp)
                  case (NF90_FLOAT)
                     floatFlag = 1
                  case (NF90_CHAR)
                     charFlag = 1
                  case (NF90_SHORT)
                     floatFlag = 1
                  case (NF90_USHORT)
                     floatFlag = 1
                  case (NF90_INT)
                     floatFlag = 1
                  case (NF90_UINT)
                     floatFlag = 1
                  case (NF90_INT64)
                     floatFlag = 1
                  case (NF90_UINT64)
                     floatFlag = 1
                  case (NF90_DOUBLE)
                     floatFlag = 1
                  case (NF90_STRING)
                     charFlag = 1
               end select
               if(charFlag .eq. 1) then
                  chrtGrdDict%xCharAttNames(xCharAttCnt+1) = trim(tmpAttName)
                  iret = nf90_get_att(ftnMeta,xVarId,trim(tmpAttName),chrtGrdDict%xCharAttVals(xCharAttCnt+1))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull x attributes')
                  xCharAttCnt = xCharAttCnt + 1
               else
                  chrtGrdDict%xFloatAttNames(xRealAttCnt+1) = trim(tmpAttName)
                  chrtGrdDict%xRealAttLen(xRealAttCnt+1) = tmpLen
                  iret = nf90_get_att(ftnMeta,xVarId,trim(tmpAttName),chrtGrdDict%xRealAttVals(xRealAttCnt+1,1:tmpLen))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull x attributes')
                  xRealAttCnt = xRealAttCnt + 1
               endif
               charFlag = 0
               floatFlag = 0
            end do
            chrtGrdDict%nxRealAtts = xRealAttCnt
            chrtGrdDict%nxCharAtts = xCharAttCnt
         endif

         charFlag = 0
         floatFlag = 0
         iret = nf90_inq_varid(ftnMeta,'y',yVarId)
         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: Unable to locate the y variable. No y variable or attributes will be created.')
            chrtGrdDict%nyCharAtts = 0
            chrtGrdDict%nyRealAtts = 0
         else
            iret = nf90_inquire_variable(ftnMeta,yVarId,nAtts=nyAtts)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find y number of attributes')
            do i =1,nyAtts
               iret = nf90_inq_attname(ftnMeta,yVarId,i,name=tmpAttName)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to extract attribute names from y variable.')
               iret = nf90_inquire_attribute(ftnMeta,yVarId,trim(tmpAttName),xtype=xtypeTmp,len=tmpLen)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to find attribute types from y variable.')
               select case (xtypeTmp)
                  case (NF90_FLOAT)
                     floatFlag = 1
                  case (NF90_CHAR)
                     charFlag = 1
                  case (NF90_SHORT)
                     floatFlag = 1
                  case (NF90_USHORT)
                     floatFlag = 1
                  case (NF90_INT)
                     floatFlag = 1
                  case (NF90_UINT)
                     floatFlag = 1
                  case (NF90_INT64)
                     floatFlag = 1
                  case (NF90_UINT64)
                     floatFlag = 1
                  case (NF90_DOUBLE)
                     floatFlag = 1
                  case (NF90_STRING)
                     charFlag = 1
               end select
               if(charFlag .eq. 1) then
                  chrtGrdDict%yCharAttNames(yCharAttCnt+1) = trim(tmpAttName)
                  iret = nf90_get_att(ftnMeta,yVarId,trim(tmpAttName),chrtGrdDict%yCharAttVals(yCharAttCnt+1))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull y attributes')
                  yCharAttCnt = yCharAttCnt + 1
               else
                  chrtGrdDict%yFloatAttNames(yRealAttCnt+1) = trim(tmpAttName)
                  chrtGrdDict%yRealAttLen(yRealAttCnt+1) = tmpLen
                  iret = nf90_get_att(ftnMeta,yVarId,trim(tmpAttName),chrtGrdDict%yRealAttVals(yRealAttCnt+1,1:tmpLen))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull y attributes')
                  yRealAttCnt = yRealAttCnt + 1
               endif
               charFlag = 0
               floatFlag = 0
            end do
            chrtGrdDict%nyRealAtts = yRealAttCnt
            chrtGrdDict%nyCharAtts = yCharAttCnt
         endif
         ! Close the file
         iret = nf90_close(ftnMeta)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close LAND spatial metadata file.')

      endif

      ! Next get the number of columns on the land grid. This will be used to
      ! calculate the resolution of the routing grid in meters and aggfactor.
      iret = nf90_open(trim(nlst(1)%land_spatial_meta_flnm),NF90_NOWRITE,ncid=ftnGeo)
      if(iret .ne. 0) then
         call postDiagMsg(diagFlag,'WARNING: Unable to open LAND Spatial Metadata file. Defaulting to missing values.')
         numColLand = -9999
         resLand = -9999
      else
         iret = nf90_inq_dimid(ftnGeo,'x',xDimId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find x dimension in LAND spatial Metadata file')
         iret = nf90_inquire_dimension(ftnGeo,xDimId,len=numColLand)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to retrieve number of columns in the LAND spatial metadata file')
         iret = nf90_inq_varid(ftnGeo,'x',xVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find x variable in LAND spatial metadata file')
         iret = nf90_get_att(ftnGeo,xVarId,'resolution',resLand)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to get x resolution in LAND spatial metadata file')
         iret = nf90_close(ftnGeo)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close the LAND spatial metadata file')
      endif

      ! Next get the number of columns on the high-resolution routing grid.
      ! This will be used to calculate the resolution of the routing grid in
      ! meters.
      iret = nf90_open(trim(nlst(1)%geo_finegrid_flnm),NF90_NOWRITE,ncid=ftnGeo)
      if(iret .ne. 0) then
         call nwmCheck(diagFlag,iret,'ERROR: Unable to open Fulldom file')
      else
         iret = nf90_inq_dimid(ftnGeo,'x',xDimId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find x dimension in Fulldom file')
         iret = nf90_inquire_dimension(ftnGeo,xDimId,len=numColHydro)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to retrieve number of columns in the Fulldom file')
         iret = nf90_close(ftnGeo)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close the Fulldom file')
      endif

      ! Calculate the aggregation factor and resolution of the hydro routing
      ! grid.
      if(numColLand .ne. -9999) then
         aggFactor = float(numColHydro/numColLand)
         resHydro = resLand/aggFactor
      else
         resHydro = -9999
      endif

      chrtGrdDict%xRes = resHydro
      chrtGrdDict%yRes = resHydro

   endif

   chrtGrdDict%varNames(:) = [character(len=64) :: "streamflow"]
   chrtGrdDict%longName(:) = [character(len=64) :: "River Flow"]
   chrtGrdDict%units(:) = [character(len=64) :: "m3 s-1"]
   chrtGrdDict%scaleFactor(:) = [0.1]
   chrtGrdDict%addOffset(:) = [0.0]
   chrtGrdDict%outFlag(:) = [0]
   chrtGrdDict%timeZeroFlag(:) = [1]
   chrtGrdDict%missingReal(:) = [-9999.0]
   chrtGrdDict%fillReal(:) = [-9999.0]
   chrtGrdDict%validMinDbl(:) = [0.0d0]
   chrtGrdDict%validMaxDbl(:) = [50000.0d0]

   ! Loop through and calculate missing/fill/min/max values that will be placed
   ! into the NetCDF attributes after scale_factor/add_offset are applied.
   do i=1,numChGrdVars
      chrtGrdDict%fillComp(i) = &
           nint((chrtGrdDict%fillReal(i)     + chrtGrdDict%addOffset(i)), 8) * &
           nint(one_dbl / chrtGrdDict%scaleFactor(i), 8)
      chrtGrdDict%missingComp(i) = &
           nint((chrtGrdDict%missingReal(i)  + chrtGrdDict%addOffset(i)), 8) * &
           nint(one_dbl / chrtGrdDict%scaleFactor(i), 8)
      chrtGrdDict%validMinComp(i) = &
           nint((chrtGrdDict%validMinDbl(i) + chrtGrdDict%addOffset(i)) * &
                nint(one_dbl / chrtGrdDict%scaleFactor(i), 8), 8)
      chrtGrdDict%validMaxComp(i) = &
           nint((chrtGrdDict%validMaxDbl(i) + chrtGrdDict%addOffset(i)) * &
                nint(one_dbl / chrtGrdDict%scaleFactor(i), 8), 8)
   end do

end subroutine initChrtGrdDict

subroutine initLsmOutDict(lsmOutDict,procId,diagFlag)
  use config_base, only: nlst
  use netcdf
  implicit none

   type(lsmMeta), intent(inout) :: lsmOutDict
   integer, intent(inout) :: procId
   integer, intent(inout) :: diagFlag
   integer :: ftnMeta,projVarId,xVarId,yVarId
   integer :: iret
   integer :: crsRealAttCnt,xRealAttCnt,yRealAttCnt
   integer :: crsCharAttCnt,xCharAttCnt,yCharAttCnt
   integer :: i, nCrsAtts,nxAtts,nyAtts
   integer :: charFlag
   integer :: floatFlag
   character(len=512) :: tmpAttName
   integer :: xtypeTmp
   integer :: tmpLen
   !LSMOUT files

   lsmOutDict%numSoilLayers = nlst(1)%nsoil
   lsmOutDict%act_lev = nlst(1)%act_lev

   lsmOutDict%modelNdv = 9.9692099683868690E36
   lsmOutDict%modelNdvInt = -2147483647

   ! First establish global attributes.
   lsmOutDict%title = "OUTPUT FROM " // trim(get_model_version())
   lsmOutDict%initTime = "1970-01-01_00:00:00" ! This will be calculated in I/O code.
   lsmOutDict%validTime = "1970-01-01_00:00:00" ! This will be calculated in I/O code.
   lsmOutDict%conventions = "CF-1.6"

   ! Next establish time attributes
   lsmOutDict%timeLName = "valid output time"
   lsmOutDict%timeUnits = "minutes since 1970-01-01 00:00:00 UTC"
   lsmOutDict%timeStName = "time"
   lsmOutDict%rTimeLName = "model initialization time"
   lsmOutDict%rTimeUnits = "minutes since 1970-01-01 00:00:00 UTC"
   lsmOutDict%rTimeStName = "forecast_reference_time"

   crsRealAttCnt = 0
   crsCharAttCnt = 0
   xRealAttCnt = 0
   xCharAttCnt = 0
   yRealAttCnt = 0
   yCharAttCnt = 0

   ! Pull spatial metadata information about the modeling domain from the land
   ! spatial metadata file.
   if(procId .eq. 0) then
      iret = nf90_open(trim(nlst(1)%land_spatial_meta_flnm),NF90_NOWRITE,ncid=ftnMeta)
      if(iret .ne. 0) then
         ! Spatial metadata file not found for land grid.
         call postDiagMsg(diagFlag,'WARNING: Unable to open LAND spatial metadata file. No crs variable or attributes will be created.')
         lsmOutDict%nCrsCharAtts = 0
         lsmOutDict%nCrsRealAtts = 0
         lsmOutDict%nxCharAtts = 0
         lsmOutDict%nxRealAtts = 0
         lsmOutDict%nyCharAtts = 0
         lsmOutDict%nyRealAtts = 0
         lsmOutDict%proj4 = ''
      else
         iret = nf90_get_att(ftnMeta,NF90_GLOBAL,'proj4',lsmOutDict%proj4)
         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: Unable to find proj4 global attribute. Defaulting to blank string.')
            lsmOutDict%proj4 = ''
         endif
         charFlag = 0
         floatFlag = 0
         ! Find the crs variable and pull out the attributes, their names, and
         ! their values. This will be translated to output files.
         iret = nf90_inq_varid(ftnMeta,'crs',projVarId)
         ! For now we are going to allow the code to move forward without
         ! finding this variable. In the future, we will probably restrict the
         ! code to ensure things are more seamless.

         ! code to ensure things are more seamless.
         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: Unable to locate the crs variable. No crs variable or attributes will be created.')
            lsmOutDict%nCrsCharAtts = 0
            lsmOutDict%nCrsRealAtts = 0
         else
            iret = nf90_inquire_variable(ftnMeta,projVarId,nAtts=nCrsAtts)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find crs number of attributes')
            do i =1,nCrsAtts
               iret = nf90_inq_attname(ftnMeta,projVarId,i,name=tmpAttName)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to extract attribute names from crs variable.')
               iret = nf90_inquire_attribute(ftnMeta,projVarId,trim(tmpAttName),xtype=xtypeTmp,len=tmpLen)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to find attribute types from crs variable.')
               select case (xtypeTmp)
                  case (NF90_FLOAT)
                     floatFlag = 1
                  case (NF90_CHAR)
                     charFlag = 1
                  case (NF90_SHORT)
                     floatFlag = 1
                  case (NF90_USHORT)
                     floatFlag = 1
                  case (NF90_INT)
                     floatFlag = 1
                  case (NF90_UINT)
                     floatFlag = 1
                  case (NF90_INT64)
                     floatFlag = 1
                  case (NF90_UINT64)
                     floatFlag = 1
                  case (NF90_DOUBLE)
                     floatFlag = 1
                  case (NF90_STRING)
                     charFlag = 1
               end select
               if(charFlag .eq. 1) then
                  lsmOutDict%crsCharAttNames(crsCharAttCnt+1) = trim(tmpAttName)
                  iret = nf90_get_att(ftnMeta,projVarId,trim(tmpAttName),lsmOutDict%crsCharAttVals(crsCharAttCnt+1))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull crs attributes')
                  crsCharAttCnt = crsCharAttCnt + 1
               else
                  lsmOutDict%crsFloatAttNames(crsRealAttCnt+1) = trim(tmpAttName)
                  lsmOutDict%crsRealAttLen(crsRealAttCnt+1) = tmpLen
                  iret = nf90_get_att(ftnMeta,projVarId,trim(tmpAttName),lsmOutDict%crsRealAttVals(crsRealAttCnt+1,1:tmpLen))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull crs attributes')
                  crsRealAttCnt = crsRealAttCnt + 1
               endif
               charFlag = 0
               floatFlag = 0
            end do
            lsmOutDict%nCrsRealAtts = crsRealAttCnt
            lsmOutDict%nCrsCharAtts = crsCharAttCnt

         endif

         ! Next pull the attributes from the x/y dimensions
         charFlag = 0
         floatFlag = 0
         iret = nf90_inq_varid(ftnMeta,'x',xVarId)
         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: Unable to locate the x variable. No x variable or attributes will be created.')
            lsmOutDict%nxCharAtts = 0
            lsmOutDict%nxRealAtts = 0
         else
            iret = nf90_inquire_variable(ftnMeta,xVarId,nAtts=nxAtts)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find x number of attributes')
            do i =1,nxAtts
               iret = nf90_inq_attname(ftnMeta,xVarId,i,name=tmpAttName)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to extract attribute names from x variable.')
               iret = nf90_inquire_attribute(ftnMeta,xVarId,trim(tmpAttName),xtype=xtypeTmp,len=tmpLen)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to find attribute types from x variable.')
               select case (xtypeTmp)
                  case (NF90_FLOAT)
                     floatFlag = 1
                  case (NF90_CHAR)
                     charFlag = 1
                  case (NF90_SHORT)
                     floatFlag = 1
                  case (NF90_USHORT)
                     floatFlag = 1
                  case (NF90_INT)
                     floatFlag = 1
                  case (NF90_UINT)
                     floatFlag = 1
                  case (NF90_INT64)
                     floatFlag = 1
                  case (NF90_UINT64)
                     floatFlag = 1
                  case (NF90_DOUBLE)
                     floatFlag = 1
                  case (NF90_STRING)
                     charFlag = 1
               end select
               if(charFlag .eq. 1) then
                  lsmOutDict%xCharAttNames(xCharAttCnt+1) = trim(tmpAttName)
                  iret = nf90_get_att(ftnMeta,xVarId,trim(tmpAttName),lsmOutDict%xCharAttVals(xCharAttCnt+1))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull x attributes')
                  xCharAttCnt = xCharAttCnt + 1
               else
                  lsmOutDict%xFloatAttNames(xRealAttCnt+1) = trim(tmpAttName)
                  lsmOutDict%xRealAttLen(xRealAttCnt+1) = tmpLen
                  iret = nf90_get_att(ftnMeta,xVarId,trim(tmpAttName),lsmOutDict%xRealAttVals(xRealAttCnt+1,1:tmpLen))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull x attributes')
                  xRealAttCnt = xRealAttCnt + 1
               endif
               charFlag = 0
               floatFlag = 0
            end do
            lsmOutDict%nxRealAtts = xRealAttCnt
            lsmOutDict%nxCharAtts = xCharAttCnt

         endif

         charFlag = 0
         floatFlag = 0
         iret = nf90_inq_varid(ftnMeta,'y',yVarId)
         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: Unable to locate the y variable. No y variable or attributes will be created.')
            lsmOutDict%nyCharAtts = 0
            lsmOutDict%nyRealAtts = 0
         else
            iret = nf90_inquire_variable(ftnMeta,yVarId,nAtts=nyAtts)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find y number of attributes')
            do i =1,nyAtts
               iret = nf90_inq_attname(ftnMeta,yVarId,i,name=tmpAttName)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to extract attribute names from y variable.')
               iret = nf90_inquire_attribute(ftnMeta,yVarId,trim(tmpAttName),xtype=xtypeTmp,len=tmpLen)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to find attribute types from y variable.')
               select case (xtypeTmp)
                  case (NF90_FLOAT)
                     floatFlag = 1
                  case (NF90_CHAR)
                     charFlag = 1
                  case (NF90_SHORT)
                     floatFlag = 1
                  case (NF90_USHORT)
                     floatFlag = 1
                  case (NF90_INT)
                     floatFlag = 1
                  case (NF90_UINT)
                     floatFlag = 1
                  case (NF90_INT64)
                     floatFlag = 1
                  case (NF90_UINT64)
                     floatFlag = 1
                  case (NF90_DOUBLE)
                     floatFlag = 1
                  case (NF90_STRING)
                     charFlag = 1
               end select
               if(charFlag .eq. 1) then
                  lsmOutDict%yCharAttNames(yCharAttCnt+1) = trim(tmpAttName)
                  iret = nf90_get_att(ftnMeta,yVarId,trim(tmpAttName),lsmOutDict%yCharAttVals(yCharAttCnt+1))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull y attributes')
                  yCharAttCnt = yCharAttCnt + 1
               else
                  lsmOutDict%yFloatAttNames(yRealAttCnt+1) = trim(tmpAttName)
                  lsmOutDict%yRealAttLen(yRealAttCnt+1) = tmpLen
                  iret = nf90_get_att(ftnMeta,yVarId,trim(tmpAttName),lsmOutDict%yRealAttVals(yRealAttCnt+1,1:tmpLen))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to pull y attributes')
                  yRealAttCnt = yRealAttCnt + 1
               endif
               charFlag = 0
               floatFlag = 0
            end do
            lsmOutDict%nyRealAtts = yRealAttCnt
            lsmOutDict%nyCharAtts = yCharAttCnt

         endif
         ! Close the file
         iret = nf90_close(ftnMeta)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close LAND spatial metadata file.')
      endif

   endif

   lsmOutDict%varNames(:) = [character(len=64) :: "stc1","smc1","sh2ox1","stc2",&
                             "smc2","sh2ox2","stc3","smc3","sh2ox3","stc4",&
                             "smc4","sh2ox4","infxsrt","sfcheadrt"]
   lsmOutDict%longName(:) = [character(len=64) :: "Soil temperature in the top layer",&
                             "Soil moisture in the top layer",&
                             "Volumetric soil moisture in the top layer",&
                             "Soil temperature in the second layer",&
                             "Soil moisture in the second layer",&
                             "Volumetric soil moisture in the second layer",&
                             "Soil temperature in the third layer",&
                             "Soil moisture in the third layer",&
                             "Volumetric soil moisture in the third layer",&
                             "Soil temperature in the fourth layer",&
                             "Soil moisture in the fourth layer",&
                             "Volumetric soil moisture in the fourth layer",&
                             "Infiltration excess","Surface head"]
   lsmOutDict%units(:) = [character(len=64) :: "K","fraction","fraction",&
                          "K","fraction","fraction","K","fraction",&
                          "fraction","K","fraction","fraction",&
                          "mm","mm"]
   lsmOutDict%scaleFactor(:) = [0.1,0.01,0.01,0.1,0.01,0.01,0.1,0.01,0.01,&
                                0.1,0.01,0.01,1.0,1.0]
   lsmOutDict%addOffset(:) = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                              0.0,0.0,0.0,0.0]
   lsmOutDict%timeZeroFlag(:) = [1,1,1,1,1,1,1,1,1,1,1,1,1,1]
   lsmOutDict%numLev(:) = [1,1,1,1,1,1,1,1,1,1,1,1,1,1]
   lsmOutDict%missingReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                                -9999.0,-9999.0,-9999.0,-9999.0]
   lsmOutDict%fillReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,-9999.0,&
                             -9999.0,-9999.0,-9999.0,-9999.0]
   lsmOutDict%validMinDbl(:) = [150.0d0, 0.0d0, 0.0d0, 150.0d0, 0.0d0, 0.0d0, 150.0d0, 0.0d0, 0.0d0, &
                                 150.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0]
   lsmOutDict%validMaxDbl(:) = [400.0d0, 1.0d0, 1.0d0, 400.0d0, 1.0d0, 1.0d0, 400.0d0, 1.0d0, 1.0d0, &
                                 400.0d0, 1.0d0, 1.0d0, 100000.0d0, 100000.0d0]
   ! Loop through and calculate missing/fill/min/max values that will be placed
   ! into the NetCDF attributes after scale_factor/add_offset are applied.
   do i=1,numLsmVars
      lsmOutDict%fillComp(i) = &
           nint((lsmOutDict%fillReal(i)     + lsmOutDict%addOffset(i)), 8) * &
           nint(one_dbl / lsmOutDict%scaleFactor(i), 8)
      lsmOutDict%missingComp(i) = &
           nint((lsmOutDict%missingReal(i)  + lsmOutDict%addOffset(i)), 8) * &
           nint(one_dbl / lsmOutDict%scaleFactor(i), 8)
      lsmOutDict%validMinComp(i) = &
           nint((lsmOutDict%validMinDbl(i) + lsmOutDict%addOffset(i)) * &
                nint(one_dbl / lsmOutDict%scaleFactor(i), 8), 8)
      lsmOutDict%validMaxComp(i) = &
           nint((lsmOutDict%validMaxDbl(i) + lsmOutDict%addOffset(i)) * &
                nint(one_dbl / lsmOutDict%scaleFactor(i), 8), 8)
   end do

end subroutine initLsmOutDict

subroutine initChanObsDict(chObsDict,diagFlag,procId)
  use config_base, only: nlst
  use netcdf
  implicit none

   type(chObsMeta), intent(inout) :: chObsDict
   integer, intent(inout) :: diagFlag
   integer, intent(inout) :: procId

   ! Local variables
   integer :: ftnMeta, iret
   integer :: projVarId

   chObsDict%modelNdv = -9.E15
   !CHANOBS FILES

   ! Pull spatial metadata information about the modeling domain from the land
   ! spatial metadata file.
   if(procId .eq. 0) then
      iret = nf90_open(trim(nlst(1)%land_spatial_meta_flnm),NF90_NOWRITE,ncid=ftnMeta)
      if(iret .ne. 0) then
         ! Spatial metadata file not found for land grid.
         call postDiagMsg(diagFlag,'WARNING: Unable to open LAND spatial metadata file.')
         chObsDict%proj4 = ''
         chObsDict%esri = ''
      else
         ! First pull metadata on coordinate system.
         iret = nf90_inq_varid(ftnMeta,'crs',projVarId)
         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: Unable to find crs in LAND spatial metadata file')
            chObsDict%proj4 = ''
            chObsDict%esri = ''
         else
            iret = nf90_get_att(ftnMeta,projVarId,'esri_pe_string',chObsDict%esri)
            if(iret .ne. 0) then
               call postDiagMsg(diagFlag,'WARNING: Unable to find esri_pe_string in LAND spatial metadata file.')
               chObsDict%esri = ''
            endif
            iret = nf90_get_att(ftnMeta,NF90_GLOBAL,'proj4',chObsDict%proj4)
            ! We are going to put a relaxed constraint on the proj4 string.
            if(iret .ne. 0) then
               call postDiagMsg(diagFlag,'WARNING: proj4 string not found. Defaulting to blank string.')
               chObsDict%proj4 = ''
            endif
         endif
         ! Close the file
         iret = nf90_close(ftnMeta)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close LAND spatial metadata file.')
      endif
   endif

   chObsDict%title = "OUTPUT FROM " // trim(get_model_version())
   chObsDict%fType = 'timeSeries'
   chObsDict%initTime = '1970-01-01_00:00:00' ! This will be calculated in I/O code
   chObsDict%validTime = '1970-01-01_00:00:00' ! This will be calculated in I/O code
   chObsDict%stDim = 'feature_id'
   chObsDict%stOrder = 1
   chObsDict%cdm = 'Station'
   chObsDict%conventions = 'CF-1.6'

   ! Next establish time attribues
   chObsDict%timeLName = 'valid output time'
   chObsDict%timeUnits = 'minutes since 1970-01-01 00:00:00 UTC'
   chObsDict%timeStName = 'time'
   chObsDict%rTimeLName = 'model initialization time'
   chObsDict%rTimeStName = 'forecast_reference_time'
   chObsDict%rTimeUnits = 'minutes since 1970-01-01 00:00:00 UTC'

   ! Esatablish lat/lon attributes
   chObsDict%latLName = "Feature latitude"
   chObsDict%latUnits = "degrees_north"
   chObsDict%latStName = "latitude"
   chObsDict%lonLName = "Feature longitude"
   chObsDict%lonUnits = "degrees_east"
   chObsDict%lonStName = "longitude"

   ! Establish streamflw order attributes
   chObsDict%orderLName = "Streamflow Order"
   chObsDict%orderStName = "order"

   ! Establish point elevation attributes
   chObsDict%elevLName = "Feature Elevation"
   chObsDict%elevUnits = "meters"
   chObsDict%elevStName = "Elevation"

   ! Next establish feature_id attributes
   chObsDict%featureIdLName = 'Reach ID'
   chObsDict%featureIdComment = 'NHDPlusv2 ComIDs within CONUS, arbitrary Reach IDs outside of CONUS'
   chObsDict%cfRole = 'timeseries_id'

   chObsDict%varNames(:) = [character(len=64) :: "streamflow"]
   chObsDict%longName(:) = [character(len=64) :: "River Flow"]
   chObsDict%units(:) = [character(len=64) :: "m3 s-1"]
   chObsDict%coordNames(:) = [character(len=64) :: "latitude longitude"]
   chObsDict%scaleFactor(:) = [0.01]
   chObsDict%addOffset(:) = [0.0]
   ! Initialize all output flags to 0. Modify (if absolutely necessary) in the
   ! output subroutine.
   chObsDict%outFlag(:) = [0]
   chObsDict%timeZeroFlag(:) = [1]
   chObsDict%fillReal(:) = [-9999.0]
   chObsDict%missingReal(:) = [-9999.0]
   chObsDict%validMinDbl(:) = [0.0d0]
   chObsDict%validMaxDbl(:) = [50000.0d0]
   ! Loop through and calculate missing/fill/min/max values that will be placed
   ! into the NetCDF attributes after scale_factor/add_offset are applied.
   do i=1,numChObsVars
      chObsDict%fillComp(i) = &
           nint((chObsDict%fillReal(i)     + chObsDict%addOffset(i)), 8) * &
           nint(one_dbl / chObsDict%scaleFactor(i), 8)
      chObsDict%missingComp(i) = &
           nint((chObsDict%missingReal(i)  + chObsDict%addOffset(i)), 8) * &
           nint(one_dbl / chObsDict%scaleFactor(i), 8)
      chObsDict%validMinComp(i) = &
           nint((chObsDict%validMinDbl(i) + chObsDict%addOffset(i)) * &
                nint(one_dbl / chObsDict%scaleFactor(i), 8), 8)
      chObsDict%validMaxComp(i) = &
           nint((chObsDict%validMaxDbl(i) + chObsDict%addOffset(i)) * &
                nint(one_dbl / chObsDict%scaleFactor(i), 8), 8)
   end do

end subroutine initChanObsDict

subroutine initGwDict(gwOutDict)
   implicit none

   type(gwMeta), intent(inout) :: gwOutDict


   gwOutDict%modelNdv = -9.E15

   gwOutDict%title = "OUTPUT FROM " // trim(get_model_version())
   gwOutDict%fType = 'timeSeries'
   !gwOutDict%proj4 = '+proj=longlat +datum=NAD83 +no_defs'
   gwOutDict%initTime = '1970-01-01_00:00:00' ! This will be calculated in I/O code
   gwOutDict%validTime = '1970-01-01_00:00:00' ! This will be calculated in I/O code
   gwOutDict%gwDim = 'gw_id'
   !gwOutDict%cdm = 'PLACEHOLDER'
   !gwOutDict%esri = 'GEOGCS[GCS_North_American_1983,DATUM[D_North_American_1983,&
   !                   &SPHEROID[GRS_1980,6378137.0,298.257222101]],&
   !                   &PRIMEM[Greenwich,0.0],UNIT[Degree,0.017453292519943295]]'
   gwOutDict%conventions = 'CF-1.6'

   ! Next establish time attribues
   gwOutDict%timeLName = 'valid output time'
   gwOutDict%timeUnits = 'minutes since 1970-01-01 00:00:00 UTC'
   gwOutDict%timeStName = 'time'
   gwOutDict%rTimeLName = 'model initialization time'
   gwOutDict%rTimeStName = 'forecast_reference_time'
   gwOutDict%rTimeUnits = 'minutes since 1970-01-01 00:00:00 UTC'

   ! Establish elevation variable attributes
   !gwOutDict%elevLName = "Water Surface Elevation"
   !gwOutDict%elevUnits = "meters"

   ! Establish feature_id attributes
   gwOutDict%featureIdLName = "Groundwater Bucket ID"
   gwOutDict%featureIdComment = "Groundwater Bucket ID"
   gwOutDict%cfRole = 'timeseries_id'

   ! Esatablish lat/lon attributes
   !gwOutDict%latLName = "Groundwater Bucket latitude"
   !gwOutDict%latUnits = "degrees_north"
   !gwOutDict%latStName = "latitude"
   !gwOutDict%lonLName = "Groundwater Bucket longitude"
   !gwOutDict%lonUnits = "degrees_east"
   !gwOutDict%lonStName = "longitude"

   gwOutDict%varNames(:) = [character(len=64) :: 'inflow','outflow','loss','depth']
   gwOutDict%longName(:) = [character(len=64) :: 'Bucket Inflow','Bucket Outflow','Bucket Loss','Bucket Depth']
   gwOutDict%units(:) = [character(len=64) :: 'm3 s-1','m3 s-1','m3 s-1','mm']
   !gwOutDict%coordNames(:) = [character(len=64) :: 'latitude longitude','latitude longitude','latitude longitude','latitude longitude']
   gwOutDict%scaleFactor(:) = [0.001,0.001,0.001,0.1]
   gwOutDict%addOffset(:) = [0.0,0.0,0.0,0.0]
   gwOutDict%outFlag(:) = [0,0,0,0]
   gwOutDict%timeZeroFlag(:) = [0,0,0,1]
   gwOutDict%fillReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0]
   gwOutDict%missingReal(:) = [-9999.0,-9999.0,-9999.0,-9999.0]
   gwOutDict%validMinDbl(:) = [0.0d0, 0.0d0, 0.0d0, 0.0d0]
   gwOutDict%validMaxDbl(:) = [50000.0d0, 50000.0d0, 50000.0d0, 10000.0d0]

   ! Loop through and calculate missing/fill/min/max values that will be placed
   ! into the NetCDF attributes after scale_factor/add_offset are applied.
   do i=1,numGwVars
      gwOutDict%fillComp(i) = &
           nint((gwOutDict%fillReal(i)     + gwOutDict%addOffset(i)), 8) * &
           nint(one_dbl / gwOutDict%scaleFactor(i), 8)
      gwOutDict%missingComp(i) = &
           nint((gwOutDict%missingReal(i)  + gwOutDict%addOffset(i)), 8) * &
           nint(one_dbl / gwOutDict%scaleFactor(i), 8)
      gwOutDict%validMinComp(i) = &
           nint((gwOutDict%validMinDbl(i) + gwOutDict%addOffset(i)) * &
                nint(one_dbl / gwOutDict%scaleFactor(i), 8), 8)
      gwOutDict%validMaxComp(i) = &
           nint((gwOutDict%validMaxDbl(i) + gwOutDict%addOffset(i)) * &
                nint(one_dbl / gwOutDict%scaleFactor(i), 8), 8)
   end do

end subroutine initGwDict

subroutine postDiagMsg(diagFlag,diagMsg)
   implicit none

   ! Subroutine arguments.
   integer, intent(in) :: diagFlag
   character(len=*), intent(in) :: diagMsg

   ! Only write out message if the diagnostic WRF_HYDRO_D flag was
   ! set to 1
   if (diagFlag .eq. 1) then
      print*, trim(diagMsg)
   end if

end subroutine postDiagMsg

subroutine nwmCheck(diagFlag,iret,msg)
   implicit none

   ! Subroutine arguments.
   integer, intent(in) :: diagFlag,iret
   character(len=*), intent(in) :: msg

   ! Check status. If status of command is not 0, then post the error message
   ! if WRF_HYDRO_D was set to be 1.
   if (iret .ne. 0) then
      call hydro_stop(trim(msg))
   end if

end subroutine nwmCheck

end module module_NWM_io_dict
