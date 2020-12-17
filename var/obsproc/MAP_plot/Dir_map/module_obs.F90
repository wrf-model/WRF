module module_obs

USE DA_Constants
USE DA_Define_Structures

#     include    <nestdmn.incl>
#     include    <maps.incl>

CONTAINS

      INCLUDE 'DA_Setup_Obs_Structures.inc'
      INCLUDE 'DA_Read_Obs_Info.inc'
      INCLUDE 'DA_Read_Obs.inc'

END MODULE module_obs
