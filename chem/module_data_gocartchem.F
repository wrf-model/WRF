MODULE module_data_gocartchem

  IMPLICIT NONE

  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------  
  ! tracer info
  !-----------------------------------------------------------------------
  ! Tracer index:
  ! default initialization for all sulfur and carbon species is 0 (undefined)
  !     1. DMS       = Dimethyl sulfide            = CH3SCH3  
  !     2. SO2       = Sulfur dioxide              = SO2               
  !     3. SO4       = Sulfate                     = SO4            
  !     4. MSA       = Methane sulfonic acid       = CH3SO3H             
  INTEGER               :: NDMS=1, NSO2=2, NSO4=3, NMSA=4 
  REAL,    PARAMETER :: airmw    = 28.97
  REAL,    PARAMETER :: smw      = 32.00 

END MODULE module_data_gocartchem
