!WRF:DRIVER_LAYER:CONFIGURATION
!
MODULE module_configure

   USE module_driver_constants
   USE module_state_description
   USE module_wrf_error

   TYPE model_config_rec_type
      SEQUENCE
! Statements that declare namelist variables are in this file
! Note that the namelist is SEQUENCE and generated such that the first item is an
! integer, first_item_in_struct and the last is an integer last_item_in_struct
! this provides a way of converting this to a buffer for passing to and from
! the driver.
#include <namelist_defines.inc>
   END TYPE model_config_rec_type

   TYPE grid_config_rec_type
#include <namelist_defines2.inc>
   END TYPE grid_config_rec_type

   TYPE(model_config_rec_type) :: model_config_rec

#include <scalar_tables.inc>

! special entries (put here but not enshrined in Registry for one reason or other)

   CHARACTER (LEN=4) :: mminlu = '    '         ! character string for landuse table

   PRIVATE read_namelist_data

CONTAINS


! Model layer, even though it does I/O -- special case of namelist I/O.

   SUBROUTINE initial_config
!<DESCRIPTION>
! This routine reads in the namelist.input file and sets
! module_config_rec, a structure of TYPE(model_config_rec_type), which is is seen via USE association by any
! subprogram that uses module_configure.  The module_config_rec structure
! contains all namelist settings for all domains.  Variables that apply
! to the entire run and have only one value regardless of domain are
! scalars.  Variables that allow different settings for each domain are
! defined as arrays of dimension max_domains (defined in
! frame/module_driver_constants.F, from a setting passed in from
! configure.wrf). There is another type in WRF, TYPE(grid_config_rec_type), in which
! all fields pertain only to a single domain (and are all scalars). The subroutine
! model_to_grid_config_rec(), also in frame/module_configure.F, is used to retrieve
! the settings for a given domain from a TYPE(module_config_rec_type) and put them into
! a TYPE(grid_config_rec_type), variables of which type are often called <em>config_flags</em>
! in the WRF code.
! 
! Most of the code in this routine is generated from the Registry file
! rconfig entries and included from the following files (found in the inc directory):
! 
! <pre>
! namelist_defines.inc	declarations of namelist variables (local to this routine)
! namelist_statements.inc	NAMELIST statements for each variable
! namelist_defaults.inc	assignment to default values if specified in Registry
! config_reads.inc		read statements for each namelist record
! config_assigns.inc	assign each variable to field in module_config_rec
! </pre>
!
!NOTE: generated subroutines from Registry file rconfig entries are renamed nl_
! instead of rconfig_ due to length limits for subroutine names.
!
! Note for version WRF 2.0: there is code here to force all domains to
! have the same mp_physics setting. This is because different mp_physics
! packages have different numbers of tracers but the nest forcing and
! feedback code relies on the parent and nest having the same number and
! kind of tracers. This means that the microphysics option
! specified on the highest numbered domain is the microphysics
! option for <em>all</em> domains in the run. This will be revisited.
! 
!</DESCRIPTION>
      IMPLICIT NONE

      INTEGER              :: io_status, nml_unit
      INTEGER              :: i

! define as temporaries
#include <namelist_defines.inc>

! Statements that specify the namelists
#include <namelist_statements.inc>

      OPEN ( UNIT   = 10               ,      &
             FILE   = "namelist.input" ,      &
             FORM   = "FORMATTED"      ,      &
             STATUS = "OLD"            ,      &
             IOSTAT = io_status         )

      IF ( io_status .NE. 0 ) THEN
        CALL WRF_ERROR_FATAL ( 'ERROR OPENING namelist.input' )
      ENDIF

      nml_unit = 10

! Statements that set the namelist vars to default vals
#  include <namelist_defaults.inc>

! Statements that read the namelist are in this file
#  define NAMELIST_READ_ERROR_LABEL 9200
#  include <config_reads.inc>

! 2004/04/28  JM (with consensus by the group of developers)
! This is needed to ensure that nesting will work, since
! different mp_physics packages have different numbers of 
! tracers. Basically, this says that the microphysics option
! specified on the highest numbered domain *is* the microphysics
! option for the run. Not the best solution but okay for 2.0.
!

      DO i = 1, max_dom
         mp_physics(i) = mp_physics(max_dom)
      ENDDO

! Statements that assign the variables to the cfg record are in this file
! except the namelist_derived variables where are assigned below
#undef SOURCE_RECORD
#undef DEST_RECORD
#undef SOURCE_REC_DEX
#define SOURCE_RECORD 
#define DEST_RECORD model_config_rec %
#define SOURCE_REC_DEX
#include <config_assigns.inc>

      CLOSE ( UNIT = 10 , IOSTAT = io_status )

      IF ( io_status .NE. 0 ) THEN
        CALL WRF_ERROR_FATAL ( 'ERROR CLOSING namelist.input' )
      ENDIF

      RETURN
9200  CONTINUE
      CALL wrf_error_fatal( 'module_configure: initial_config: error reading namelist' )

   END SUBROUTINE initial_config

#if 1
   SUBROUTINE get_config_as_buffer( buffer, buflen, ncopied )
! note that model_config_rec_type must be defined as a sequence derived type
      INTEGER,   INTENT(INOUT) ::  buffer(*)
      INTEGER,   INTENT(IN)    ::  buflen
      INTEGER,   INTENT(OUT)   ::  ncopied
!      TYPE(model_config_rec_type) :: model_config_rec
      INTEGER :: nbytes
      CALL wrf_num_bytes_between ( model_config_rec%last_item_in_struct ,   &
                                   model_config_rec%first_item_in_struct ,  &
                                   nbytes )
!      nbytes = loc(model_config_rec%last_item_in_struct) - &
!               loc(model_config_rec%first_item_in_struct)
      IF ( nbytes .gt. buflen ) THEN
        CALL wrf_error_fatal( &
        "get_config_rec_as_buffer: buffer size to small for config_rec" )
      ENDIF
      CALL wrf_mem_copy( model_config_rec, buffer, nbytes )
      ncopied = nbytes
      RETURN
   END SUBROUTINE get_config_as_buffer

   SUBROUTINE set_config_as_buffer( buffer, buflen )
! note that model_config_rec_type must be defined as a sequence derived type
      INTEGER,   INTENT(INOUT) ::  buffer(*)
      INTEGER,   INTENT(IN)    ::  buflen
!      TYPE(model_config_rec_type) :: model_config_rec
      INTEGER :: nbytes
      CALL wrf_num_bytes_between ( model_config_rec%last_item_in_struct ,  &
                                   model_config_rec%first_item_in_struct , &
                                   nbytes )
!      nbytes = loc(model_config_rec%last_item_in_struct) - &
!               loc(model_config_rec%first_item_in_struct)
      IF ( nbytes .gt. buflen ) THEN
        CALL wrf_error_fatal( &
        "set_config_rec_as_buffer: buffer length too small to fill model config record" )
      ENDIF
      CALL wrf_mem_copy( buffer, model_config_rec, nbytes )
      RETURN
   END SUBROUTINE set_config_as_buffer
#else
   SUBROUTINE get_config_as_buffer( buffer, buflen, ncopied )
! note that model_config_rec_type must be defined as a sequence derived type
      INTEGER*1, INTENT(INOUT) ::  buffer(*)
      INTEGER,   INTENT(IN)    ::  buflen
      INTEGER,   INTENT(OUT)   ::  ncopied
!      TYPE(model_config_rec_type) :: model_config_rec
      INTEGER :: nbytes
      nbytes = loc(model_config_rec%last_item_in_struct) - &
               loc(model_config_rec%first_item_in_struct)
      IF ( nbytes .gt. buflen ) THEN
        CALL wrf_error_fatal( &
        "get_config_rec_as_buffer: buffer size to small for config_rec" )
      ENDIF
      CALL wrf_mem_copy( model_config_rec, buffer, nbytes )
      ncopied = nbytes
      RETURN
   END SUBROUTINE get_config_as_buffer

   SUBROUTINE set_config_as_buffer( buffer, buflen )
! note that model_config_rec_type must be defined as a sequence derived type
      INTEGER*1, INTENT(INOUT) ::  buffer(*)
      INTEGER,   INTENT(IN)    ::  buflen
!      TYPE(model_config_rec_type) :: model_config_rec
      INTEGER :: nbytes
      nbytes = loc(model_config_rec%last_item_in_struct) - &
               loc(model_config_rec%first_item_in_struct)
      IF ( nbytes .gt. buflen ) THEN
        CALL wrf_error_fatal( &
        "set_config_rec_as_buffer: buffer length too small to fill model config record" )
      ENDIF
      CALL wrf_mem_copy( buffer, model_config_rec, nbytes )
      RETURN
   END SUBROUTINE set_config_as_buffer
#endif

   SUBROUTINE model_to_grid_config_rec ( id_id , model_config_rec , grid_config_rec )
      INTEGER , INTENT(IN)                         ::  id_id
      TYPE ( model_config_rec_type ) , INTENT(IN)  ::  model_config_rec
      TYPE ( grid_config_rec_type  ) , INTENT(OUT) ::  grid_config_rec
! <DESCRIPTION>
! This routine is called to populate a domain specific configuration
! record of TYPE(grid_config_rec_type) with the configuration information
! for that domain that is stored in TYPE(model_config_rec). Both types
! are defined in frame/module_configure.F.  The input argument is the
! record of type model_config_rec_type contains the model-wide
! configuration information (that is, settings that apply to the model in
! general) and configuration information for each individual domain.  The
! output argument is the record of type grid_config_rec_type which
! contains the model-wide configuration information and the
! domain-specific information for this domain only.  In the
! model_config_rec, the domain specific information is arrays, indexed by
! the grid id's.  In the grid_config_rec the domain-specific information
! is scalar and for the specific domain.  The first argument to this
! routine is the grid id (top-most domain is always 1) as specified in
! the domain-specific namelist variable grid_id.
! 
! The actual assignments form the model_config_rec_type to the
! grid_config_rec_type are generate from the rconfig entries in the
! Registry file and included by this routine from the file
! inc/config_assigns.inc.
!
!NOTE: generated subroutines from Registry file rconfig entries are renamed nl_
! instead of rconfig_ due to length limits for subroutine names.
!
! 
! </DESCRIPTION>
#undef SOURCE_RECORD
#undef SOURCE_REC_DEX
#undef DEST_RECORD
#define SOURCE_RECORD model_config_rec %
#define SOURCE_REC_DEX (id_id)
#define DEST_RECORD   grid_config_rec %
#include <config_assigns.inc>
   END SUBROUTINE model_to_grid_config_rec

! Include the definitions of all the routines that return a namelist values
! back to the driver. These are generated by the registry

   SUBROUTINE init_module_configure
     IMPLICIT NONE
     ! Local vars

     INTEGER i , j

     DO j = 1, max_domains
#include <scalar_tables_init.inc>
     END DO
   END SUBROUTINE init_module_configure

! When the compiler has Intel Inside (TM) (that is, ifort), the large
! number of nl_get and nl_set routines inside the module causes the
! compiler to never finish with this routine. For ifort, move the
! routines outside the module. Note, the registry generates a 
! USE module_configure for all the nl_get and nl_set routines
! if IFORT_KLUDGE is in effect.
#ifdef IFORT_KLUDGE

END MODULE module_configure

# include <get_nl_config.inc>

#else

# include <get_nl_config.inc>

END MODULE module_configure

#endif

! Special (outside registry)
SUBROUTINE nl_get_mminlu ( idum , retval )
  USE module_configure
  CHARACTER(LEN=4)  :: retval
  INTEGER idum
  retval(1:4) = mminlu(1:4)   ! mminlu is defined in module_configure
  RETURN
END SUBROUTINE nl_get_mminlu
SUBROUTINE nl_set_mminlu ( idum, inval )
  USE module_configure
  CHARACTER(LEN=4) :: inval
  INTEGER idum
  mminlu(1:4) = inval(1:4)    ! mminlu is defined in module_configure
  RETURN
END SUBROUTINE nl_set_mminlu


SUBROUTINE set_scalar_indices_from_config ( idomain , dummy2, dummy1 )
  USE module_driver_constants
  USE module_state_description
  USE module_wrf_error
  USE module_configure
  IMPLICIT NONE
  INTEGER , INTENT(IN)  :: idomain
  INTEGER               :: dummy1
  INTEGER               :: dummy2

!<DESCRIPTION>
!This routine is called to adjust the integer variables that are defined
!in frame/module_state_description.F (Registry-generated) and that serve
!as indices into 4D tracer arrays for moisture, chemistry, etc.
!Different domains (different grid data structures) are allowed to have
!different sets of tracers so these indices can vary from domain to
!domain. However, since the indices are defined globally in
!module_state_description (a shortcoming in the current software), it is
!necessary that these indices be reset each time a different grid is to
!be computed on.
!
!The scalar idices are set according to the particular physics
!packages -- more specifically in the case of the moisture tracers, microphysics
!packages -- that are stored for each domain in model_config_rec and
!indexed by the grid id, passed in as an argument to this routine.  (The
!initial_config() routine in module_configure is what reads the
!namelist.input file and sets model_config_rec.)
!
!The actual code for calculating the scalar indices on a particular
!domain is generated from the Registry state array definitions for the
!4d tracers and from the package definitions that indicate which physics
!packages use which tracers.
!
!</DESCRIPTION>

#include <scalar_indices.inc>
#include <scalar_indices_init.inc>
  RETURN
END SUBROUTINE set_scalar_indices_from_config
